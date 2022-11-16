{-# language BlockArguments #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}
{-# language NamedFieldPuns #-}
{-# language TupleSections #-}
{-# language ApplicativeDo #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}

-- abstract-par
import Control.Monad.Par.Class (ParFuture)

-- base
import Control.Applicative hiding (some, many)
import qualified Control.Applicative as Applicative
import Control.Arrow ((>>>))
import Control.Monad
import Data.Bifunctor (first, second, bimap)
import qualified Data.Char as Char
import Data.Either ( partitionEithers )
import Data.Foldable
import Data.Function
import Data.Functor
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Ord (Down(Down))
import Data.Tuple (swap)
import Data.Void
import GHC.Generics (Generic)
import Prelude hiding (takeWhile)
import System.IO (stderr, hPutStrLn)

-- containers
import Data.Set (Set)
import qualified Data.Set as Set

-- deepseq
import Control.DeepSeq

-- directory
import System.Directory

-- dlist
import qualified Data.DList as DList

-- filepath
import System.FilePath.Posix

-- megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

-- monad-par
import Control.Monad.Par.IO (runParIO)

-- monad-par-extras
import Control.Monad.Par.Combinator (parMapM)

-- monoidal-containers
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MonoidalMap

-- mtl
import Control.Monad.Trans

-- nonempty-containers
import qualified Data.Set.NonEmpty as NESet
import Data.Set.NonEmpty (NESet)

-- optparse-applicative
import qualified Options.Applicative as Options

-- process
import System.Process (runInteractiveCommand)

-- text
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.Lazy.Builder as TextBuilder

-- transformers
import Control.Monad.Trans.Maybe


data Options = Options
  { overwrite :: Bool
  , localModulesFromCurrentDir :: Bool
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

  runParIO (groupFileImports porgramOptions)


groupFileImports
  :: ( MonadIO m
     , ParFuture iv m
     )
  => Options -> m ()
groupFileImports
  Options
    { overwrite
    , localModulesFromCurrentDir
    , reportProgress
    , filePaths
    } = do

  when
    reportProgress
    (liftIO $ putStrLn  $ "processing " ++ show (length filePaths) ++ " number of files.")

  globalModulesFromAllGhcPackages <-
    liftIO getPackageModulesFromGhcDump
      <* when
          reportProgress
          (liftIO $ putStrLn "got all exposed modules from GHC package set")

  localModulesCWD <-
    if localModulesFromCurrentDir
      then
          projectModules "./"
          <* when
              reportProgress
              (liftIO $ putStrLn "got all local modules from current working directory")
      else
        pure (Set.empty, MonoidalMap.empty)

  void
    $ filePaths
    & parMapM
    (\filePath -> do
        absoluteFilePath_ <- absoluteFilePath filePath
        fileText <- liftIO $ Text.readFile filePath
        (localPackages_, moduleMap) <-
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

          ) <&> second (<> globalModulesFromAllGhcPackages)

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
             let groupedImports =
                    imports
                      & map (determineModulePackage absoluteFilePath_ localPackages_ moduleMap)
                      & groupImportsByPackage

             in [beforeImports, groupedImports, afterImports]
                  & mconcat
                  & contentToText
                  & outPutResult
                  & liftIO

        when
          reportProgress
          (liftIO $ putStrLn $ "finished: " ++ filePath)
    )


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
  { beforeImports :: ![Content]
  , imports :: ![Import]
  , afterImports :: ![Content]
  } deriving (Show, Eq, Ord, Generic, NFData)

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
  { content :: !Text
  , packageName :: !(Maybe Text)
  , moduleName :: !ModuleName
  }
  deriving (Show, Eq, Ord, Generic, NFData)

newtype SingleLineComment = SingleLineComment Text deriving (Show, Eq, Ord, Generic, NFData)

newtype BlankLine = BlankLine Text deriving (Show, Eq, Ord, Generic, NFData)

newtype OtherLine = OtherLine Text deriving (Show, Eq, Ord, Generic, NFData)

data Content
  = ASingleLineComment !SingleLineComment
  | ABlankLine !BlankLine
  | AImport !Import
  | AOtherLine !OtherLine
  deriving (Show, Eq, Ord, Generic, NFData)

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

  maybeSource <-
    (<?> "maybeSource") $
    try (Just <$> string "{-# SOURCE #-}") <|> 
    try (Just <$> string "{-# source #-}") <|> 
    pure Nothing

  spacesFollowingSource <-
    (<?> "spacesFollowingSource") $
    Text.pack <$> many spaceChar
 
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
          char '"' >> (Text.pack <$> someTill printChar (char '"'))
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
              , fromMaybe "" maybeSource
              , spacesFollowingSource
              , fromMaybe "" maybeQualified
              , spacesFollowingQualified
              , maybe "" (\packageName -> "\"" <> packageName <> "\"") maybePackageName
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
  { packageName :: !PackageName
  , pathToCabalFile :: !AbsoluteFilePath
  , cabalPackageDependencies :: ![PackageName]
  } deriving (Show, Eq, Ord, Generic, NFData)

data PackageSource
  = ALocalPackage !LocalPackage
  | AGlobalPackage !PackageName
  deriving (Show, Eq, Ord, Generic, NFData)

newtype PackageName = PackageName {getPackageName :: Text} deriving (Eq, Ord, Show, Generic, NFData)

packageNameFromSource :: PackageSource -> PackageName
packageNameFromSource = \case
  AGlobalPackage packageName ->  packageName
  ALocalPackage LocalPackage{packageName} ->  packageName

determineModulePackage
  :: AbsoluteFilePath
  -> Set LocalPackage
  -> MonoidalMap ModuleName (NESet PackageSource)
  -> Import
  -> (Import, PackageName)
determineModulePackage absoluteFilePath_ localPackages_ localModules_ i@Import{moduleName}
  | Import{packageName = Just packageName } <- i
      = (i, PackageName packageName)

  | Just inPackageMap
      <- MonoidalMap.lookup moduleName localModules_
      =  (i, pickPackage absoluteFilePath_ localPackages_ inPackageMap)

  | otherwise = (i, PackageName "")

pickPackage :: AbsoluteFilePath -> Set LocalPackage -> NESet PackageSource -> PackageName
pickPackage (AbsoluteFilePath filePath) localPackages_ = NESet.toList >>> \case
  (matchOnePackage :| []) ->
    packageNameFromSource matchOnePackage

  -- when there are multiple matches
  --  * first attempt to filter the matching packages according to the cabal file dependencies
  --  * otherwise if the file path matches a single local package pick that
  --  * otherwise be deterministic by picking first one sorted set passed in
  manyMatches ->
    let oneBestLocalMatch =
          manyMatches
            & NonEmpty.filter
                (\case
                    AGlobalPackage _ ->
                      False

                    ALocalPackage
                      LocalPackage
                        {pathToCabalFile = AbsoluteFilePath pathToCabalFile } ->
                        takeDirectory pathToCabalFile `List.isPrefixOf` filePath
                )
            & \case
                [x] -> Just (packageNameFromSource x)
                _ -> Nothing

        cabalDependencyMatch = do
          LocalPackage {cabalPackageDependencies} <-
            localPackages_
              & Set.toList
              & List.filter
                  (\case
                      LocalPackage
                        {pathToCabalFile = AbsoluteFilePath pathToCabalFile } ->
                          takeDirectory pathToCabalFile `List.isPrefixOf` filePath

                  )
              -- When we have a cabal project with anohter cabal project in a subdirectory
              -- we need to take the longest matching path. That is match to the cabal project
              -- closest to file.
              & List.sortOn (Down . pathToCabalFile)
              & listToMaybe

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

    in fromMaybe fallBackMatch  (oneBestLocalMatch <|> cabalDependencyMatch)

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

newtype ModuleName = ModuleName {unModuleName :: Text} deriving (Show, Eq, Ord, Generic, NFData)

projectModules
  :: ( MonadIO m
     , ParFuture iv m
     )
  => FilePath -> m (Set LocalPackage, MonoidalMap ModuleName (NESet PackageSource))
projectModules filePath = do
  mcabalProjectFile <-
    findEnclosingFile (("cabal.project"==) . takeFileName) filePath

  case mcabalProjectFile of
    Just (AbsoluteFilePath cabalProjectFile) -> do
      cabalFiles <-
        gatherFiles ".cabal" (takeDirectory cabalProjectFile)

      (localPackages, modules) <-
        traverse (localModules . unAbsoluteFilePath) cabalFiles <&> mconcat

      pure (localPackages, removeLocalPackagePrefixes <$> modules)

    Nothing ->
      pure (Set.empty, MonoidalMap.empty)


localModules
  :: ( MonadIO m
     , ParFuture iv m
     )
  => FilePath -> m (Set LocalPackage, MonoidalMap ModuleName (NESet PackageSource))
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

      let localPackage =
            LocalPackage
              { packageName =
                  PackageName
                    . Text.pack
                    . takeBaseName
                    $ cabalPath
              , pathToCabalFile = AbsoluteFilePath cabalPath
              , ..
              }

      moduleMap <- gatherFiles ".hs" (takeDirectory cabalPath)
        >>= parMapM (fmap (either (const Nothing) Just . parse parseModuleAndModuleName "") . (liftIO . Text.readFile) . unAbsoluteFilePath)
        <&> mapMaybe
              (fmap $ \moduleName ->
                 MonoidalMap.singleton
                  moduleName
                  ( NESet.singleton (ALocalPackage localPackage)
                  )
              )
        <&> mconcat

      pure (Set.singleton localPackage, moduleMap)


    Nothing ->
      pure (Set.empty, MonoidalMap.empty)

gatherFiles
  :: ( MonadIO m
     , ParFuture iv m
     )
  => String -> FilePath -> m [AbsoluteFilePath]
gatherFiles extension =
  absoluteFilePath >=> fmap DList.toList  . go
  where
    go (AbsoluteFilePath filePath) =
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
                  >>= lift . parMapM  go
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

parseRestOfLine :: Parser Text
parseRestOfLine =
      (<?> "restOfLine") $ Text.pack <$> manyTill (printChar <|> char '\t') (void eol)

newtype AbsoluteFilePath = AbsoluteFilePath {unAbsoluteFilePath :: FilePath} deriving (Show, Eq, Ord, Generic, NFData)

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

getPackageModulesFromGhcDump :: IO (MonoidalMap ModuleName (NESet PackageSource))
getPackageModulesFromGhcDump = do
  (_, hOut, _, _) <-
    runInteractiveCommand "ghc-pkg dump"
  Text.hGetContents hOut
    <&> parse parsePackageDump ""
    >>= \case
          Left e ->
            putStrLn
              ("error parsing ghc package dump: " ++ errorBundlePretty e)
              >> pure MonoidalMap.empty

          Right x ->
            pure x

parsePackageDump :: Parser (MonoidalMap ModuleName (NESet PackageSource))
parsePackageDump =
  some parsePackageDumpPackage <&> mconcat


parsePackageDumpPackage :: Parser (MonoidalMap ModuleName (NESet PackageSource))
parsePackageDumpPackage = do
  package_ <-
    NESet.singleton
      . AGlobalPackage
      . PackageName
      . Text.pack
      <$> parseName

  (moduleNames, skipTillEndOfPackage) <-
    skipSomeTill
      (try skipLine)
      ( asum
          [ (,True) <$> parseExposedModules
          , ([], False) <$ try packageEnd
          ]
      )

  when
    skipTillEndOfPackage
    (void $ skipManyTill (try skipLine) packageEnd)

  pure
    ( moduleNames
        & map (, package_)
        & MonoidalMap.fromListWith (<>)
    )

  where
    skipLine = manyTill (printChar <|> char '\t') eol

    parseName =
      string "name:" >> skipSome nonEolSpaceChar >> someTill printChar eol

    parseExposedModules =
      string "exposed-modules:"
        >> (void eol <|> pure ())
        >> some
             ( skipSome nonEolSpaceChar
                >> sepBy1 (parseModuleName <* optional from <* optional ",") nonEolSpaceChar
                <* eol
             )
             <&> concat

    packageEnd = try (string "---" >> (void eol <|> eof)) <|> eof

    from = do
      _ <- " from " 
      some $ choice [ alphaNumChar, char '-', char ':', char '.' ]

nonEolSpaceChar :: Parser ()
nonEolSpaceChar =
  void $ notFollowedBy eol >> (spaceChar <|> char '\t')


-- The way gatherFiles works is that it locates .hs files in the
-- subdirectories of a directory with a .cabal file in it, but those
-- subdirectories in turn have a .cabal file in it, then .hs files beneath
-- that directory should not be treated as being part of the parent package.
removeLocalPackagePrefixes :: NESet PackageSource -> NESet PackageSource
removeLocalPackagePrefixes set = NESet.fromList packages'
  where
    packages = NESet.toList set
    (locals, globals) = partitionPackages (toList packages)
    locals' = removePrefixes cabalDir locals
      where
        cabalDir = takeDirectory . unAbsoluteFilePath . pathToCabalFile
    packages' = fromMaybe packages $ NonEmpty.nonEmpty $
      ALocalPackage <$> locals' <|> AGlobalPackage <$> globals


partitionPackages :: [PackageSource] -> ([LocalPackage], [PackageName])
partitionPackages = partitionEithers . map \case
  ALocalPackage localPackage -> Left localPackage
  AGlobalPackage globalPackage -> Right globalPackage


removePrefixes :: Eq b => (a -> [b]) -> [a] -> [a]
removePrefixes f input =
  filter (\a -> none (f a `prefix`) (f <$> input)) input
  where
    prefix a b = case a `List.stripPrefix` b of
      Just (_ : _) -> True
      _ -> False
    none = (not .) . any
