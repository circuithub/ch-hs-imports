{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}
{-# language NamedFieldPuns #-}
{-# language TupleSections #-}
{-# language ApplicativeDo #-}

import Control.Monad.IO.Class
import System.Directory
import Control.Monad.Trans.Maybe
import System.FilePath.Posix
import Data.Functor
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.Map as Map
import Data.Map (Map)

-- base
import Control.Applicative hiding (some, many)
import Control.Arrow ((>>>))
import Control.Monad
import Data.Bifunctor (first, second, bimap)
import qualified Data.Char as Char
import Data.Function
import qualified Data.List as List
import Data.Maybe
import Data.Tuple (swap)
import Data.Void
import Prelude hiding (takeWhile)

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


data Options = Options
  { overwrite :: Bool
  , filePath :: String
  }

options :: Options.Parser Options
options = do
  overwrite <-
    [ Options.long "overwrite"
    , Options.help "Overwrite the input file"
    ]
    & mconcat
    & Options.switch

  filePath <-
    [ Options.metavar "FILE"
    , Options.help "The file whose imports will be grouped"
    ]
    & mconcat
    & Options.argument Options.str

  pure Options {..}

main :: IO ()
main =
  groupFileImports
    =<< Options.execParser
          ( [ Options.fullDesc
            , Options.progDesc "Group imports by package"
            , Options.header "Group imports by package"
            ]
            & mconcat
            & Options.info (options <**> Options.helper)
          )

groupFileImports :: Options -> IO ()
groupFileImports Options{overwrite, filePath} = do
  fileText <- Text.readFile filePath

  moduleMap <-
    ( (++)
        <$> (maybeToList <$>  runMaybeT (localModules filePath))
        <*> (fromMaybe [] <$> runMaybeT ( projectModules filePath))
    )
    <&> reverse
    <&> concatMap (\(package, modules) -> (,package) <$> modules)
    <&> Map.fromList


  let outPutResult =
        if overwrite
          then Text.writeFile filePath
          else Text.putStr

  case second (extractImports . contentSplitTrailingBlankLines) $ parse parseConent filePath fileText of
    Left e ->
      fail $ "Failed parsing: \n" ++ show e

    Right ExtractImports{beforeImports, imports, afterImports} ->
      do groupedImports <-
            groupImportsByPackage <$> traverse (getModuleToPackageMap moduleMap) imports
         [beforeImports, groupedImports, afterImports] & mconcat & contentToText & outPutResult

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
                (ASingleLineComment _ : reversedBefore, _) -> reverse reversedBefore
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
    (<?> "restOfLine") $
    Text.pack <$> someTill printChar (void eol)

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

  maybeQualified <-
    (<?> "maybeQualified") $
    try (Just <$> string "qualified") <|> pure Nothing

  spacesFollowingQualified <-
    (<?> "spacesFollowingQualified") $
    Text.pack <$> many spaceChar

  moduleName@(ModuleName moduleNameText)  <-
    parseModuleName

  restOfLine <-
    (<?> "restOfLine") $
    Text.pack <$> manyTill printChar (void eol)

  indentedLines <-
    (<?> "indentedLines") $
    many $ try $
      (do
        spacesAtStartOfLine <- Text.pack <$> some (notFollowedBy  eol >> spaceChar)
        restOfIndentedLine <- Text.pack <$> manyTill printChar (void eol)
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
              , fromMaybe "" maybePackageName
              , spacesFollowingPackage
              , fromMaybe "" maybeQualified
              , spacesFollowingQualified
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

  restOfLine <-
    (<?> "restOfLine") $
    Text.pack <$> someTill printChar (void eol)

  pure (OtherLine restOfLine)

bol :: Parser ()
bol = (<?> "bol") $ do
  pos <- sourceColumn <$> getSourcePos
  guard (pos == pos1)


newtype PackageName = PackageName {getPackageName :: Text} deriving (Eq, Ord, Show)

parsePackageName :: Text -> Maybe PackageName
parsePackageName = parseMaybe (PackageName . dropVersion <$> parser)
  where
    dropVersion :: Text -> Text
    dropVersion t =
      let idx =
            t
              & Text.reverse
              & Text.findIndex ('-' ==)
              & fromMaybe 1
              & (\i -> Text.length t - i - 1)

      in t & Text.splitAt idx & fst

    parser :: Parser Text
    parser =
      Text.cons
        <$> (space >> satisfy Char.isAlpha <?> "Must start with alpha character")
        <*>  nameP
        <* (space >> eof <?> "may not have any characters following it")

    nameP =
        takeWhile1P
          Nothing
          ( or
             . sequence
               [ Char.isAlphaNum
               , (=='_')
               , (=='-')
               , (=='.')
               ]
          )

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
            ("ghc-pkg find-module " <> Text.unpack (unModuleName moduleName))
            SB.empty
            ( SB.lines
                >>> S.mapped (SB.foldlChunks (<>) "")
                >>> S.mapMaybe (parsePackageName . Text.decodeUtf8)
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

findEnclosingFile :: (MonadIO m, MonadPlus m) => (FilePath -> Bool) -> FilePath -> m FilePath
findEnclosingFile fileMatcher =  liftIO . canonicalizePath >=> go
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

projectModules :: (MonadIO m, MonadPlus m) => FilePath -> m [(PackageName, [ModuleName])]
projectModules filePath = do
  cabalProjectFile <-
    findEnclosingFile (("cabal.project"==) . takeFileName) filePath

  cabalFiles <-
    gatherFiles ".cabal" (takeDirectory cabalProjectFile)

  traverse localModules cabalFiles


localModules :: (MonadIO m, MonadPlus m) => FilePath -> m (PackageName, [ModuleName])
localModules filePath = do
  cabalFile <-
    findEnclosingFile (byExtension ".cabal") filePath

  modules <-
    gatherFiles ".hs" (takeDirectory cabalFile)
    >>= traverse (liftIO . Text.readFile)
    <&> mapMaybe (either (const Nothing) Just . parse parseModuleAndModuleName "")

  pure
    ( PackageName
        . Text.pack
        . takeBaseName
        $ cabalFile
    , modules
    )

gatherFiles :: (MonadIO m) => String -> FilePath -> m [FilePath]
gatherFiles extension =
  liftIO . canonicalizePath >=> fmap DList.toList  . go
  where
    go :: (MonadIO n) => FilePath -> n (DList FilePath)
    go filePath =
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
                  >>= traverse go
                  <&> mconcat

              checkFile = do
                filePath
                  & byExtension extension
                  & guard
                pure (DList.singleton filePath)

          descendDirectory <|> checkFile

parseModuleAndModuleName :: Parser ModuleName
parseModuleAndModuleName = moduleName <|> (restOfLine >> parseModuleAndModuleName)
  where
    moduleName =
        string "module"
         >> some spaceChar
         >> parseModuleName

    restOfLine =
      (<?> "restOfLine") $ manyTill printChar (void eol)
