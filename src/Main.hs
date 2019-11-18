{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}
{-# language NamedFieldPuns #-}
{-# language TupleSections #-}
{-# language ApplicativeDo #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

-- async
import Control.Concurrent.Async (mapConcurrently, forConcurrently_)

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
import Data.List.NonEmpty (NonEmpty, NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Tuple (swap)
import Data.Void
import Prelude hiding (takeWhile)
import System.IO (stderr, hPutStrLn)

-- directory
import System.Directory

-- dlist
import qualified Data.DList as DList

-- filepath
import System.FilePath.Posix

-- megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

-- monoidal-containers
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MonoidalMap

-- nonempty-containers
import qualified Data.Set.NonEmpty as NESet
import Data.Set.NonEmpty (NESet)

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

-- unliftio-core
import Control.Monad.IO.Unlift

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

  (globalModulesFromAllGhcPackages, determineModulePackage_) <-
    if modulesFromAllGhcPkgs
      then
        (, \ a b c -> pure $ determineModulePackage' a b c ) <$> getModuleMapFromGhc
        <* when
            reportProgress
            (liftIO $ putStrLn "got all exposed modules from GHC package set")
      else pure (MonoidalMap.empty, determineModulePackage)

  localModulesCWD <-
    if localModulesFromCurrentDir
      then
          projectModules "./"
          <* when
              reportProgress
              (liftIO $ putStrLn "got all local modules from current working directory")
      else
        pure MonoidalMap.empty

  runInIO <- askRunInIO
  liftIO $ forConcurrently_  filePaths $ \filePath -> runInIO $ do
    absoluteFilePath_ <- absoluteFilePath filePath
    fileText <- liftIO $ Text.readFile filePath
    moduleMap <-
      ( if localModulesFromCurrentDir
         then
           pure localModulesCWD

         else
           ( (<>)
               <$> localModules filePath
               <*> projectModules filePath
           )
          <* when
              reportProgress
              (liftIO $ putStrLn $ "got local modules relative to file path: " ++ filePath)

      ) <&> (<> globalModulesFromAllGhcPackages)

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
              liftIO $ groupImportsByPackage <$> traverse (determineModulePackage_ absoluteFilePath_ moduleMap) imports

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

data LocalPackage = LocalPackage
  { packageName :: PackageName
  , pathToCabalFile :: AbsoluteFilePath
  , cabalPackageDependencies :: [PackageName]
  } deriving (Show, Eq, Ord)

data PackageSource
  = ALocalPackage LocalPackage
  | AGlobalPackage PackageName
  deriving (Show, Eq, Ord)

newtype PackageName = PackageName {getPackageName :: Text} deriving (Eq, Ord, Show)

packageNameFromSource :: PackageSource -> PackageName
packageNameFromSource = \case
  AGlobalPackage packageName ->  packageName
  ALocalPackage LocalPackage{packageName} ->  packageName

dropPackageVersion :: Text -> Text
dropPackageVersion t =
  let idx =
        t
          & Text.reverse
          & Text.findIndex ('-' ==)
          & fromMaybe 1
          & (\i -> Text.length t - i - 1)

  in t & Text.splitAt idx & fst

determineModulePackage'
  :: AbsoluteFilePath
  -> MonoidalMap ModuleName (NESet PackageSource)
  -> Import
  -> (Import, PackageName)
determineModulePackage' absoluteFilePath_ localModules_ i@Import{moduleName}
  | Import{packageName = Just packageName } <- i
      = (i, PackageName packageName)

  | Just inPackageMap
      <- MonoidalMap.lookup moduleName localModules_
      =  (i, pickPackage absoluteFilePath_ inPackageMap)

  | otherwise = (i, PackageName "")

determineModulePackage
  :: AbsoluteFilePath
  -> MonoidalMap ModuleName (NESet PackageSource)
  -> Import
  -> IO (Import, PackageName)
determineModulePackage absoluteFilePath_ localModules_ import_@Import{moduleName} =  do
  fromFindModule <-
   withStreamingCommand
     ("ghc-pkg find-module " <> Text.unpack (unModuleName moduleName) <> " --simple-output")
     SB.empty
     ( SB.words
         >>> S.mapped (SB.foldlChunks (<>) "")
         >>> S.map
              ((moduleName,)
                . NESet.singleton
                . AGlobalPackage
                . PackageName
                . dropPackageVersion
                . Text.decodeUtf8
              )
         >>> S.hoist SB.effects
         >>> S.toList
     )
     <&> MonoidalMap.fromList . S.fst'

  pure $ determineModulePackage' absoluteFilePath_ (localModules_ <> fromFindModule) import_

pickPackage :: AbsoluteFilePath -> NESet PackageSource -> PackageName
pickPackage (AbsoluteFilePath filePath)= NESet.toList >>> \case
  (matchOnePackage :| []) ->
    packageNameFromSource matchOnePackage

  -- when there are multiple matches
  --  * first attempt to filter the matching packages according to the cabal file dependencies
  --  * otherwise if the file path matches a single local package pick that
  --  * otherwise be deterministic by picking first one sorted set passed in
  manyMatches ->
    let closestParentCabalFile =
          manyMatches
            & NonEmpty.toList
            & mapMaybe
                (\case
                    AGlobalPackage _ ->
                      Nothing

                    ALocalPackage
                      localPackage@LocalPackage
                        {pathToCabalFile = AbsoluteFilePath pathToCabalFile } -> do

                      guard
                        (takeDirectory pathToCabalFile `List.isPrefixOf` filePath)

                      pure localPackage
                )

        oneBestLocalMatch =
          closestParentCabalFile
            & \case
                [LocalPackage {packageName}] -> Just packageName
                _ -> Nothing

        cabalDependencyMatch = do
          LocalPackage {cabalPackageDependencies} <- closestParentCabalFile & listToMaybe
          manyMatches
            & NonEmpty.filter
                ( (`elem` cabalPackageDependencies)
                    . packageNameFromSource
                )
            & \case
                [x] -> Just (packageNameFromSource x)
                _ -> Nothing

        fallBackMatch =
          manyMatches
            & NonEmpty.head
            & packageNameFromSource

    in fromMaybe fallBackMatch  (cabalDependencyMatch <|> oneBestLocalMatch)

groupImportsByPackage :: [(Import, PackageName)] -> [Content]
groupImportsByPackage = maybe [] go . NonEmpty.nonEmpty
  where
    go =
      NonEmpty.sortBy (compare `on` snd)
        >>> NonEmpty.groupBy ((==) `on` snd)
        >>> fmap (NonEmpty.sortBy (compare `on` moduleName . fst))
        >>> map
          (\((h, PackageName packageName) :| t) ->
            ASingleLineComment (SingleLineComment ("-- " <> packageName))
            : AImport h
            : map (AImport . fst) t
          )
        >>> List.intersperse [ABlankLine (BlankLine "")]
        >>> List.concat

findEnclosingFile
  :: ( MonadIO m
     )
  => (FilePath -> Bool) -> FilePath -> m (Maybe AbsoluteFilePath)
findEnclosingFile fileMatcher =  absoluteFilePath >=> runMaybeT . go
  where
    go (AbsoluteFilePath filePath) = do
      liftIO (doesPathExist filePath) >>= guard

      let tryPath = do
            liftIO (doesDirectoryExist filePath) >>= guard
            liftIO (listDirectory filePath)
              <&> List.filter fileMatcher
              <&> listToMaybe
              <&> fmap (AbsoluteFilePath . (filePath</>))
              >>= maybe mzero pure

          ascendToParent = do
            let parentDir = takeDirectory filePath
            guard (parentDir /= filePath)
            go (AbsoluteFilePath parentDir)

      tryPath <|> ascendToParent

byExtension :: String -> FilePath -> Bool
byExtension extension = and . sequence [isExtensionOf extension, not . List.null . takeBaseName]

newtype ModuleName = ModuleName {unModuleName :: Text} deriving (Show, Eq, Ord)

projectModules
  :: ( MonadUnliftIO m
     )
  => FilePath -> m (MonoidalMap ModuleName (NESet PackageSource))
projectModules filePath = do
  mcabalProjectFile <-
    findEnclosingFile (("cabal.project"==) . takeFileName) filePath

  case mcabalProjectFile of
    Just (AbsoluteFilePath cabalProjectFile) -> do
      cabalFiles <-
        gatherFiles ".cabal" (takeDirectory cabalProjectFile)

      traverse (localModules . unAbsoluteFilePath) cabalFiles <&> mconcat

    Nothing ->
      pure MonoidalMap.empty


localModules
  :: ( MonadUnliftIO m
     )
  => FilePath -> m (MonoidalMap ModuleName (NESet PackageSource))
localModules filePath = do
  mpathToCabalFile <-
    findEnclosingFile (byExtension ".cabal") filePath

  case mpathToCabalFile of
    Just (AbsoluteFilePath cabalPath) -> do

      cabalPackageDependencies <-
        Text.readFile cabalPath
          <&> parse parseCabalDependencies  cabalPath
          >>= \case
                Left e ->
                  hPutStrLn
                        stderr
                        ( unlines
                            [ "Failed parsing dependencies from cabal file :"
                            , errorBundlePretty e
                            , "for file: "  ++ cabalPath
                            ]
                        )
                  $> []

                Right x -> pure x
          & liftIO

      gatherFiles ".hs" (takeDirectory cabalPath)
        >>= liftIO . mapConcurrently (fmap (either (const Nothing) Just . parse parseModuleAndModuleName "") . Text.readFile . unAbsoluteFilePath)
        <&> mapMaybe
              (fmap $ \moduleName ->
                  MonoidalMap.singleton
                    moduleName
                    ( NESet.singleton
                        ( ALocalPackage
                            LocalPackage
                              { packageName =
                                  PackageName
                                    . Text.pack
                                    . takeBaseName
                                    $ cabalPath
                              , pathToCabalFile = AbsoluteFilePath cabalPath
                              , ..
                              }
                        )
                    )
              )
        <&> mconcat


    Nothing ->
      pure MonoidalMap.empty

gatherFiles
  :: ( MonadUnliftIO m
     )
  => String -> FilePath -> m [AbsoluteFilePath]
gatherFiles extension =
  absoluteFilePath >=> fmap DList.toList  . go
  where
    go (AbsoluteFilePath filePath) = do
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
                  <&> map (AbsoluteFilePath . (filePath </>))
                  >>= liftIO . mapConcurrently  (runInIO . go)
                  <&> mconcat

              checkFile = do
                filePath
                  & byExtension extension
                  & guard
                pure (DList.singleton (AbsoluteFilePath filePath))

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
  => m (MonoidalMap ModuleName (NESet PackageSource))
getModuleMapFromGhc =
  liftIO getPackagesFromGhc
    >>= liftIO
          . mapConcurrently
              ( \packageName ->
                  map (, NESet.singleton (AGlobalPackage packageName))
                    <$> getPackageModulesFromGhc packageName
              )
    <&> MonoidalMap.fromListWith (<>) . concat


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

newtype AbsoluteFilePath = AbsoluteFilePath {unAbsoluteFilePath :: FilePath} deriving (Show, Eq, Ord)

absoluteFilePath :: (MonadIO m) => FilePath -> m AbsoluteFilePath
absoluteFilePath = fmap AbsoluteFilePath . liftIO . makeAbsolute


parseCabalDependencies :: Parser [PackageName]
parseCabalDependencies = do
  colStartPos <-
    skipManyTill
      (try otherLine)
      (try parseBuildDependsStart)

  some (parsePackageName colStartPos)

  where
    otherLine =
      manyTill printChar (void eol <|> eof)

    parseBuildDependsStart =
      space1
        >> (mkPos . (+1) . unPos . sourceColumn <$> getSourcePos)
        <* (string "build-depends" >> space >> void (char ':') >> space)
        <?> "parseBuildDependsStart"

    parsePackageName :: Pos -> Parser PackageName
    parsePackageName leftBorder = do
      try (space >> char ',' >> space)
        <|> pure ()

      pos <- sourceColumn <$> getSourcePos <?> "start column"

      guard (leftBorder <= pos)
        <?> "check start column"

      PackageName . Text.pack
        <$> ( (:)
                <$> letterChar
                <*> many
                      ( choice
                          [ alphaNumChar
                          , char '-'
                          , char '_'
                          ]
                      )
                <* skipManyTill
                     anySingle
                     ( try (void eol)
                         <|> (char ',' >> space)
                     )
            ) <?> "package name"
