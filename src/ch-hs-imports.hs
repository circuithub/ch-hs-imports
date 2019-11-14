{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}
{-# language NamedFieldPuns #-}
{-# language TupleSections #-}
{-# language ApplicativeDo #-}

import Prelude hiding (takeWhile)

import Data.Maybe
import Control.Monad
import Control.Applicative hiding (some, many)
import qualified Data.List as List
import Data.Function
import Data.Either
import Streaming.Process (withStreamingCommand)
import qualified Streaming.Prelude as S
import qualified Streaming as S
import Control.Arrow ((>>>), left)
import Data.Bifunctor (first, second, bimap)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Char as Char
import qualified Data.ByteString.Streaming as SB
import qualified Data.ByteString.Streaming.Char8 as SB

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Data.Text.Lazy as Text (toStrict)
import Data.Tuple (swap)

import qualified Options.Applicative as Options
import Data.Semigroup

data Options = Options
  { overwrite :: Bool
  , filePath :: String
  }

options :: Options.Parser Options
options = do
  overwrite <-
    [ Options.long "overwite"
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

  let outPutResult =
        if overwrite
          then Text.writeFile filePath
          else Text.putStr

  case second (extractImports . contentSplitTrailingBlankLines) $ parse parseConent filePath fileText of
    Left e ->
      fail $ "Failed parsing: \n" ++ show e

    Right ExtractImports{beforeImports, imports, afterImports} ->
      do groupedImports <-
            groupImportsByPackage <$> traverse getModuleToPackageMap imports
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

      (beforeImports, xs1) =
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
  , moduleName :: Text
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

  moduleName <-
    (<?> "moduleName") $
    Text.intercalate "." <$>
    sepBy1
      (do firstCharacter <- upperChar
          rest <- many (choice [alphaNumChar, char '_'])
          pure (Text.pack (firstCharacter : rest))
      )
      (char '.')


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
              , moduleName
              , restOfLine
              ]
              :  indentedLines
            )
      , packageName = maybePackageName
      , moduleName = moduleName
      }

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

getModuleToPackageMap :: Import -> IO (Import, PackageName)
getModuleToPackageMap i@Import{moduleName, packageName = Just packageName } = pure (i, PackageName packageName)
getModuleToPackageMap i@Import{moduleName}=
  maybe (i, PackageName "") (i,)
  . listToMaybe
  . S.fst'
  <$> withStreamingCommand
        ("ghc-pkg find-module " <> Text.unpack moduleName)
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
