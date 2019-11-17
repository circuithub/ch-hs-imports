{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}
{-# language NamedFieldPuns #-}
{-# language TupleSections #-}
{-# language ApplicativeDo #-}

-- base
import Control.Applicative hiding (some, many)
import qualified Control.Applicative as Applicative
import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor (first, second, bimap)
import qualified Data.Char as Char
import Data.Foldable
import Data.Function
import Data.Functor
import qualified Data.List as List
import Data.Maybe
import Data.Tuple (swap)
import Data.Void
import Prelude hiding (takeWhile)
import System.IO (stderr, hPutStrLn)

import Control.Concurrent.Async (mapConcurrently, forConcurrently_)
import Control.Monad.IO.Unlift



-- containers
import qualified Data.Map as Map
import Data.Map (Map)

-- directory
import System.Directory

-- dlist
import qualified Data.DList as DList

-- filepath
import System.FilePath.Posix

-- megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

-- optparse-applicative
import qualified Options.Applicative as Options

-- streaming
import qualified Streaming as S
import qualified Streaming.Prelude as S

-- streaming-bytestring
import qualified Data.ByteString.Streaming as SB
import qualified Data.ByteString.Streaming.Char8 as SB

-- streaming-process
import Streaming.Process (withStreamingCommand)

-- text
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.Lazy.Builder as TextBuilder

-- transformers
import Control.Monad.Trans.Maybe


data Options = Options
  { overwrite :: Bool
  , localModulesFromCurrentDir :: Bool
  , modulesFromAllGhcPkgs :: Bool
  , reportProgress :: Bool
  , filePaths :: [String]
  }

options :: Options.Parser Options
options = do
  overwrite <-
    [ Options.long "overwrite"
    , Options.help "Overwrite the input file"
    ]
    & mconcat
    & Options.switch

  localModulesFromCurrentDir <-
    [ Options.long "local-modules-from-current-dir"
    , Options.help "Get the local modules from the current directory once and not each time relative to each file."
    ]
    & mconcat
    & Options.switch

  modulesFromAllGhcPkgs <-
    [ Options.long "modules-from-all-ghc-pkgs"
    , Options.help "Get the module set once at the start from all the packages registered with ghc. A lot faster."
    ]
    & mconcat
    & Options.switch


  reportProgress <-
    [ Options.long "report-progress"
    , Options.help "When doing multiple files report progress along the way"
    ]
    & mconcat
    & Options.switch

  filePaths <-
    [ Options.metavar "FILES..."
    , Options.help "The files whose imports will be grouped"
    ]
    & mconcat
    & Options.argument Options.str
    & Applicative.some

  pure Options {..}

main :: IO ()
main = do
  porgramOptions <-
     Options.execParser
          ( [ Options.fullDesc
            , Options.progDesc "Group imports by package"
            , Options.header "Group imports by package"
            ]
            & mconcat
            & Options.info (options <**> Options.helper)
          )

  groupFileImports porgramOptions


groupFileImports
  :: ( MonadUnliftIO m
     )
  => Options -> m ()
groupFileImports
  Options
    { overwrite
    , localModulesFromCurrentDir
    , modulesFromAllGhcPkgs
    , reportProgress
    , filePaths
    } = do

  when
    reportProgress
    (liftIO $ putStrLn  $ "processing " ++ show (length filePaths) ++ " number of files.")

  globalModulesFromAllGhcPackages <-
    if modulesFromAllGhcPkgs
      then
        getModuleMapFromGhc
        <* when
            reportProgress
            (liftIO $ putStrLn "got all exposed modules from GHC package set")
      else pure Map.empty

  localModulesCWD <-
    if localModulesFromCurrentDir
      then
          ( projectModules "./"
              <&> concatMap (\(package, modules) -> (,package) <$> modules)
              <&> Map.fromList
          )
          <* when
              reportProgress
              (liftIO $ putStrLn "got all local modules from current working directory")
      else
        pure Map.empty

  runInIO <- askRunInIO
  liftIO $ forConcurrently_  filePaths $ \filePath -> runInIO $ do
    fileText <- liftIO $ Text.readFile filePath
    moduleMap <-
      ( if localModulesFromCurrentDir
         then
           pure localModulesCWD

         else
           ( ( (++)
                 <$> (maybeToList <$>  localModules filePath)
                 <*> projectModules filePath
             )
             <&> reverse
             <&> concatMap (\(package, modules) -> (,package) <$> modules)
             <&> Map.fromList
           )
          <* when
              reportProgress
              (liftIO $ putStrLn $ "got local modules relative to file path: " ++ filePath)

      ) <&> (`Map.union` globalModulesFromAllGhcPackages)

    let outPutResult =
          if overwrite
            then \newFileText ->
                    when
                      (newFileText /= fileText)
                      (Text.writeFile filePath newFileText)
            else Text.putStr

    case second (extractImports . contentSplitTrailingBlankLines) $ parse parseConent filePath fileText of
      Left e ->
        liftIO $ hPutStrLn stderr (unlines ["Failed parsing:", errorBundlePretty e, "for file: "  ++ filePath])

      Right ExtractImports{beforeImports, imports, afterImports} ->
        do groupedImports <-
              liftIO$ groupImportsByPackage <$> traverse (getModuleToPackageMap moduleMap) imports

           [beforeImports, groupedImports, afterImports] & mconcat & contentToText & outPutResult & liftIO

    when
      reportProgress
      (liftIO $ putStrLn $ "finished: " ++ filePath)


contentSplitTrailingBlankLines :: [Content] -> [Content]
contentSplitTrailingBlankLines =
  List.concatMap
    (\case
        AImport i ->
          importSplitTrailingBlankLines i
            & bimap AImport (map ABlankLine)
            & uncurry (:)
        x -> [x]
    )

importSplitTrailingBlankLines :: Import -> (Import, [BlankLine])
importSplitTrailingBlankLines i@Import{content} =
  let (contentNew, blanks) = splitTrailingBlankLines content
  in (i{content = contentNew}, map BlankLine blanks)


splitTrailingBlankLines :: Text -> (Text, [Text])
splitTrailingBlankLines =
  Text.splitOn "\n"
    >>> List.reverse
    >>> (\xs ->
          xs
            & List.findIndex (not . or . sequence [Text.null, Text.all Char.isSpace])
            & fromMaybe 0
            & (`List.splitAt` xs)
        )
    >>> second (List.reverse >>> Text.intercalate "\n")
    >>> first List.reverse
    >>> swap

data ExtractImports = ExtractImports
  { beforeImports :: [Content]
  , imports :: [Import]
  , afterImports :: [Content]
  } deriving (Show, Eq, Ord)

extractImports :: [Content] -> ExtractImports
extractImports xs0 =
  let matchImport =
         \case
            AImport _ -> True
            _ -> False

      (before, xs1) =
        xs0
          & List.findIndex matchImport
          & fromMaybe 0
          & (`List.splitAt` xs0)

      (imports, afterImports) =
        xs1
          & reverse
          & List.findIndex matchImport
          & maybe 0 (\i -> length xs1 - i)
          & (`List.splitAt` xs1)
          & first
              (mapMaybe
                (\case
                    AImport x -> Just x
                    _ -> Nothing
                )
              )
      beforeImports =
            case (reverse before, imports) of
                (_, []) -> before
                ([ASingleLineComment _], _) -> []
                (ASingleLineComment _ : reversedBefore, _) -> reverse (ABlankLine (BlankLine "") : reversedBefore)
                _ -> before

  in ExtractImports{..}

contentToText :: [Content] -> Text
contentToText =
  (`Text.snoc` '\n')
    . Text.toStrict
    . TextBuilder.toLazyText
    . mconcat
    . List.intersperse (TextBuilder.singleton '\n')
    . map (TextBuilder.fromText . toText)
  where
    toText = \case
      ASingleLineComment (SingleLineComment t) -> t
      ABlankLine (BlankLine t) -> t
      AImport Import{content = t} -> t
      AOtherLine (OtherLine t) -> t

data Import = Import
  { content :: Text
  , packageName :: Maybe Text
  , moduleName :: ModuleName
  }
  deriving (Show, Eq, Ord)

newtype SingleLineComment = SingleLineComment Text deriving (Show, Eq, Ord)

newtype BlankLine = BlankLine Text deriving (Show, Eq, Ord)

newtype OtherLine = OtherLine Text deriving (Show, Eq, Ord)

data Content
  = ASingleLineComment SingleLineComment
  | ABlankLine BlankLine
  | AImport Import
  | AOtherLine OtherLine
  deriving (Show, Eq, Ord)

type Parser = Parsec Void Text

parseConent :: Parser [Content]
parseConent =
  manyTill
    ( choice $ map try
        [ ASingleLineComment <$> parseSingleLineComment
        , ABlankLine <$> parseBlankLine
        , AImport <$> parseImport
        , AOtherLine <$> parseOtherLine
        ]
    )
    eof

parseSingleLineComment :: Parser SingleLineComment
parseSingleLineComment = (<?> "parseSingleLineComment") $ do
  bol

  spacesBefore <-
    (<?> "spacesBefore") $
    Text.pack <$> many spaceChar

  commentStart <-
    (<?> "commentStart") $
    string "--"

  restOfLine <-
    parseRestOfLine

  pure (SingleLineComment (mconcat [spacesBefore, commentStart, restOfLine]))

parseBlankLine :: Parser BlankLine
parseBlankLine = (<?> "parseSingleLineComment") $ do
  bol
  BlankLine . Text.pack <$> manyTill spaceChar eol

parseImport :: Parser Import
parseImport = (<?> "parseImport") $ do
  bol

  textImport <-
    (<?> "textImport") $
    string "import"

  spacesFollowingImport <-
    (<?> "spacesFollowingImport") $
    Text.pack <$> some spaceChar


  maybeQualified <-
    (<?> "maybeQualified") $
    try (Just <$> string "qualified") <|> pure Nothing

  spacesFollowingQualified <-
    (<?> "spacesFollowingQualified") $
    Text.pack <$> many spaceChar

  maybePackageName <-
    (<?> "maybePackageName") $
    try
      (Just <$> do
          packageName <- char '"' >> (Text.pack <$> someTill printChar (char '"'))
          pure (Text.cons '"' packageName `Text.snoc` '"')
      ) <|> pure Nothing

  spacesFollowingPackage <-
    (<?> "spacesFollowingPackage") $
    Text.pack <$> many spaceChar

  moduleName@(ModuleName moduleNameText)  <-
    parseModuleName

  restOfLine <-
    parseRestOfLine

  indentedLines <-
    (<?> "indentedLines") $
    many $ try $
      (do
        spacesAtStartOfLine <- Text.pack <$> some (notFollowedBy  eol >> spaceChar)
        restOfIndentedLine <- parseRestOfLine
        pure (spacesAtStartOfLine <> restOfIndentedLine)
      ) <|> ("" <$ eol)

  pure
    Import
      { content =
          Text.intercalate
            "\n"
            (mconcat
              [ textImport
              , spacesFollowingImport
              , fromMaybe "" maybeQualified
              , spacesFollowingQualified
              , fromMaybe "" maybePackageName
              , spacesFollowingPackage
              , moduleNameText
              , restOfLine
              ]
              :  indentedLines
            )
      , packageName = maybePackageName
      , moduleName = moduleName
      }

parseModuleName :: Parser ModuleName
parseModuleName =
    (<?> "moduleName") $
    ModuleName . Text.intercalate "." <$>
    sepBy1
      (do firstCharacter <- upperChar
          rest <- many (choice [alphaNumChar, char '_'])
          pure (Text.pack (firstCharacter : rest))
      )
      (char '.')

parseOtherLine :: Parser OtherLine
parseOtherLine = do
  bol

  notFollowedBy
    (choice $ map try
      [ void parseImport
      , void parseSingleLineComment
      , void parseBlankLine
      ]
    )

  OtherLine <$> parseRestOfLine


bol :: Parser ()
bol = (<?> "bol") $ do
  pos <- sourceColumn <$> getSourcePos
  guard (pos == pos1)


newtype PackageName = PackageName {getPackageName :: Text} deriving (Eq, Ord, Show)

dropPackageVersion :: Text -> Text
dropPackageVersion t =
  let idx =
        t
          & Text.reverse
          & Text.findIndex ('-' ==)
          & fromMaybe 1
          & (\i -> Text.length t - i - 1)

  in t & Text.splitAt idx & fst

getModuleToPackageMap :: Map ModuleName PackageName -> Import -> IO (Import, PackageName)
getModuleToPackageMap _ i@Import{packageName = Just packageName } = pure (i, PackageName packageName)
getModuleToPackageMap localModules_ i@Import{moduleName}
  | Just (PackageName packageName)
      <- Map.lookup moduleName localModules_
      = pure (i, PackageName packageName)

  | otherwise =
      maybe (i, PackageName "") (i,)
      . listToMaybe
      . S.fst'
      <$> withStreamingCommand
            ("ghc-pkg find-module " <> Text.unpack (unModuleName moduleName) <> " --simple-output")
            SB.empty
            ( SB.words
                >>> S.mapped (SB.foldlChunks (<>) "")
                >>> S.map (PackageName . dropPackageVersion . Text.decodeUtf8)
                >>> S.hoist SB.effects
                >>> S.toList
            )

groupImportsByPackage :: [(Import, PackageName)] -> [Content]
groupImportsByPackage =
  List.sortOn snd
    >>> List.groupBy ((==) `on` snd)
    >>> map (List.sortOn (moduleName . fst))
    >>> map
      (\((h, PackageName packageName):t) ->
        ASingleLineComment (SingleLineComment ("-- " <> packageName))
        : AImport h
        : map (AImport . fst) t
      )
    >>> List.intersperse [ABlankLine (BlankLine "")]
    >>> List.concat

findEnclosingFile
  :: ( MonadIO m
     )
  => (FilePath -> Bool) -> FilePath -> m (Maybe FilePath)
findEnclosingFile fileMatcher =  liftIO . canonicalizePath >=> runMaybeT . go
  where
    go filePath = do
      liftIO (doesPathExist filePath) >>= guard

      let tryPath = do
            liftIO (doesDirectoryExist filePath) >>= guard
            liftIO (listDirectory filePath)
              <&> List.filter fileMatcher
              <&> listToMaybe
              <&> fmap (filePath</>)
              >>= maybe mzero pure

          ascendToParent = do
            let parentDir = takeDirectory filePath
            guard (parentDir /= filePath)
            go parentDir

      tryPath <|> ascendToParent

byExtension :: String -> FilePath -> Bool
byExtension extension = and . sequence [isExtensionOf extension, not . List.null . takeBaseName]

newtype ModuleName = ModuleName {unModuleName :: Text} deriving (Show, Eq, Ord)

projectModules
  :: ( MonadUnliftIO m
     )
  => FilePath -> m [(PackageName, [ModuleName])]
projectModules filePath = do
  mcabalProjectFile <-
    findEnclosingFile (("cabal.project"==) . takeFileName) filePath

  case mcabalProjectFile of
    Just cabalProjectFile -> do
      cabalFiles <-
        gatherFiles ".cabal" (takeDirectory cabalProjectFile)

      traverse localModules cabalFiles <&> catMaybes

    Nothing ->
      pure []


localModules
  :: ( MonadUnliftIO m
     )
  => FilePath -> m (Maybe (PackageName, [ModuleName]))
localModules filePath = do
  mcabalFile <-
    findEnclosingFile (byExtension ".cabal") filePath

  case mcabalFile of
    Just cabalFile -> do
      modules <-
        gatherFiles ".hs" (takeDirectory cabalFile)
        >>= liftIO . mapConcurrently (fmap (either (const Nothing) Just . parse parseModuleAndModuleName "") . Text.readFile)
        <&> catMaybes

      pure $ Just
        ( PackageName
            . Text.pack
            . takeBaseName
            $ cabalFile
        , modules
        )

    Nothing ->
      pure Nothing

gatherFiles
  :: ( MonadUnliftIO m
     )
  => String -> FilePath -> m [FilePath]
gatherFiles extension =
  liftIO . canonicalizePath >=> fmap DList.toList  . go
  where
    go filePath = do
      runInIO <- askRunInIO
      fmap (fromMaybe DList.empty)
        . runMaybeT
        $ do
          liftIO (doesPathExist filePath) >>= guard

          filePath
            & takeFileName
            & not . List.isPrefixOf "."
            & guard

          let descendDirectory = do
                liftIO (doesDirectoryExist filePath) >>= guard
                liftIO (listDirectory filePath)
                  <&> map (filePath </>)
                  >>= liftIO . mapConcurrently  (runInIO . go)
                  <&> mconcat

              checkFile = do
                filePath
                  & byExtension extension
                  & guard
                pure (DList.singleton filePath)

          descendDirectory <|> checkFile

parseModuleAndModuleName :: Parser ModuleName
parseModuleAndModuleName = moduleName <|> (parseRestOfLine >> parseModuleAndModuleName)
  where
    moduleName =
        string "module"
         >> some spaceChar
         >> parseModuleName


getModuleMapFromGhc
  :: ( MonadUnliftIO m
     )
  => m (Map ModuleName PackageName)
getModuleMapFromGhc =
  liftIO getPackagesFromGhc
    >>= liftIO . mapConcurrently (\packageName -> map (, packageName) <$> getPackageModulesFromGhc packageName)
    <&> Map.fromList . concat


getPackagesFromGhc :: IO [PackageName]
getPackagesFromGhc =
  S.fst'
    <$> withStreamingCommand
          "ghc-pkg list --simple-output"
          SB.empty
          ( SB.words
              >>> S.mapped (SB.foldlChunks (<>) "")
              >>> S.map (PackageName . dropPackageVersion . Text.decodeUtf8)
              >>> S.hoist SB.effects
              >>> S.toList
            )

getPackageModulesFromGhc :: PackageName -> IO [ModuleName]
getPackageModulesFromGhc (PackageName packageName)=
  S.fst'
    <$> withStreamingCommand
          ("ghc-pkg field " <> Text.unpack packageName <> " exposed-modules --simple-output")
          SB.empty
          ( SB.words
              >>> S.mapped (SB.foldlChunks (<>) "")
              >>> S.map (ModuleName . Text.decodeUtf8)
              >>> S.hoist SB.effects
              >>> S.toList
            )

parseRestOfLine :: Parser Text
parseRestOfLine =
      (<?> "restOfLine") $ Text.pack <$> manyTill (printChar <|> char '\t') (void eol)
