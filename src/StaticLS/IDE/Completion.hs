{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}

module StaticLS.IDE.Completion
  ( getCompletion,
    Context (..),
    TriggerKind (..),
    Completion (..),
    CompletionKind (..),
    CompletionMessage (..),
    resolveCompletionEdit,
  )
where

import AST qualified
import AST.Haskell qualified as H
import AST.Haskell qualified as Haskell
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson qualified as Aeson
import Data.Char qualified as Char
import Data.Coerce (coerce)
import Data.Containers.ListUtils (nubOrd)
import Data.Edit (Edit)
import Data.Edit qualified as Edit
import Data.Function ((&))
import Data.Functor.Identity qualified as Identity
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Pos (LineCol (..), Pos (..))
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import Data.TextUtils qualified as TextUtils
import Data.Traversable (for)
import Database.SQLite.Simple qualified as SQL
import GHC.Generics (Generic)
import HieDb (HieDb)
import HieDb qualified
import StaticLS.HIE.Queries (allGlobalSymbols)
import StaticLS.Hir qualified as Hir
import StaticLS.IDE.CodeActions.AutoImport qualified as IDE.CodeActions.AutoImport
import StaticLS.IDE.Monad
import StaticLS.Logger (logInfo)
import StaticLS.Monad
import StaticLS.Semantic.Position qualified as Semantic.Position
import StaticLS.StaticEnv
import StaticLS.Tree qualified as Tree
import StaticLS.Utils (isRightOrThrowT)
import System.FilePath

makeRelativeMaybe :: FilePath -> FilePath -> Maybe FilePath
makeRelativeMaybe base path = do
  let rel = makeRelative base path
  guard $ path /= rel
  pure rel

pathToModule :: AbsPath -> StaticLsM (Maybe Text)
pathToModule absPath = do
  let fp = Path.toFilePath absPath
  staticEnv <- getStaticEnv
  let srcDirs = staticEnv.srcDirs
  let wsRoot = staticEnv.wsRoot
  pure $ do
    modPath <- asum ((\srcDir -> makeRelativeMaybe (Path.toFilePath srcDir) fp) <$> srcDirs)
    let (modPathWithoutExt, ext) = splitExtension modPath
    guard $ ext == ".hs"
    let modText = T.replace (T.pack [pathSeparator]) "." (T.pack modPathWithoutExt)
    pure modText

stripNameSpacePrefix :: Text -> Text
stripNameSpacePrefix t = snd $ T.breakOnEnd ":" t

getExportsForMod :: HieDb -> Text -> IO [Text]
getExportsForMod (HieDb.getConn -> conn) mod = do
  res <-
    SQL.query @_ @(SQL.Only Text)
      conn
      "SELECT DISTINCT exports.occ \
      \FROM exports \
      \JOIN mods using (hieFile) \
      \WHERE mods.mod = ?"
      (SQL.Only mod)
  pure $ fmap stripNameSpacePrefix $ coerce res

getModules :: StaticLsM [Text]
getModules = do
  mods <- runMaybeT $ runHieDbMaybeT \(HieDb.getConn -> conn) -> do
    res <-
      SQL.query_
        @(SQL.Only Text)
        conn
        "SELECT DISTINCT mod FROM mods"
    pure $ coerce res
  pure $ Maybe.fromMaybe [] mods

getCompletionsForMod :: Text -> StaticLsM [Completion]
getCompletionsForMod mod = do
  res <- runMaybeT $ runHieDbMaybeT \hiedb -> do
    getExportsForMod hiedb mod
  res <- pure $ Maybe.fromMaybe [] res
  pure $ fmap textCompletion res

getCompletionsForMods :: [Text] -> StaticLsM [Completion]
getCompletionsForMods mods = do
  importCompletions <- for mods \mod -> do
    getCompletionsForMod mod
  pure $ concat importCompletions

getFileCompletions :: Context -> StaticLsM [Completion]
getFileCompletions cx = do
  let path = cx.path
  fileCompletions <-
    runMaybeT $ do
      hieFile <- getHieFile path
      let symbols = allGlobalSymbols hieFile
      let symbolsNubbed = nubOrd symbols
      -- logInfo $ "symbols: " <> T.pack (show symbols)
      let completions = fmap textCompletion symbolsNubbed
      pure completions
  fileCompletions <- pure $ Maybe.fromMaybe [] fileCompletions
  pure fileCompletions

getUnqualifiedImportCompletions :: Context -> StaticLsM [Completion]
getUnqualifiedImportCompletions cx = do
  let path = cx.path
  haskell <- getHaskell path
  let prog = Hir.parseHaskell haskell
  (_errs, prog) <- isRightOrThrowT prog
  let imports = prog.imports
  let unqualifiedImports = filter (\imp -> not imp.qualified) imports
  getCompletionsForMods $ (.mod.text) <$> unqualifiedImports

data CompletionMode
  = ImportMode !(Maybe Text)
  | HeaderMode !Text
  | QualifiedMode !Text !Text
  | UnqualifiedMode
  deriving (Show, Eq)

getModulePrefix :: Context -> Rope -> Maybe (Text, Text)
getModulePrefix cx sourceRope = do
  let lineCol = cx.lineCol
  let line = Rope.toText $ Maybe.fromMaybe "" $ Rope.getLine sourceRope (Pos lineCol.line)
  let (beforeCol, _afterCol) = T.splitAt lineCol.col line
  let (_, prefix) = Identity.runIdentity $ T.spanEndM (pure . not . Char.isSpace) beforeCol
  let firstChar = fst <$> T.uncons prefix
  let firstIsUpper = case firstChar of
        Just c -> Char.isUpper c
        Nothing -> False
  if
    | firstIsUpper, Just (mod, match) <- TextUtils.splitOnceEnd "." prefix -> Just (mod, match)
    | otherwise -> Nothing

getImportPrefix :: Context -> Rope -> H.Haskell -> Maybe (Maybe Text)
getImportPrefix cx sourceRope hs = do
  let lineCol = cx.lineCol
  let line = Rope.toText $ Maybe.fromMaybe "" $ Rope.getLine sourceRope (Pos lineCol.line)
  let astPoint = Semantic.Position.lineColToAstPoint cx.lineCol
  let imports = AST.getDeepestContaining @Haskell.Imports astPoint (AST.getDynNode hs)
  case "import" `T.stripPrefix` line of
    Just rest | Maybe.isJust imports -> do
      let mod = T.dropWhile Char.isSpace rest
      let modPrefix = TextUtils.splitOnceEnd "." mod
      Just $ fst <$> modPrefix
    _ -> Nothing

getCompletionMode :: Context -> StaticLsM CompletionMode
getCompletionMode cx = do
  let path = cx.path
  haskell <- getHaskell path
  header <- Tree.getHeader haskell & isRightOrThrowT
  sourceRope <- getSourceRope path
  mod <- pathToModule path
  if
    | (Nothing, Just mod) <- (header, mod) -> pure $ HeaderMode mod
    | Just modPrefix <- getImportPrefix cx sourceRope haskell -> do
        pure $ ImportMode modPrefix
    | Just (mod, match) <- getModulePrefix cx sourceRope -> do
        pure $ QualifiedMode mod match
    | otherwise -> do
        pure UnqualifiedMode

defaultAlias :: Text -> Maybe Text
defaultAlias = \case
  "T" -> Just "Data.Text"
  "TE" -> Just "Data.Text.Encoding"
  "B" -> Just "Data.ByteString"
  "BL" -> Just "Data.ByteString.Lazy"
  "TL" -> Just "Data.Text.Lazy"
  "TIO" -> Just "Data.Text.IO"
  _ -> Nothing

bootModules :: [Text]
bootModules =
  [ "Data.Text",
    "Data.ByteString",
    "Data.Map",
    "Data.Set",
    "Data.IntMap",
    "Data.IntSet",
    "Data.Sequence"
  ]

isModSubseqOf :: Text -> Text -> Bool
isModSubseqOf sub mod = List.isSubsequenceOf sub' mod' || sub == mod
  where
    sub' = T.splitOn "." sub
    mod' = T.splitOn "." mod

isBootModule :: Text -> Bool
isBootModule mod = any (\bootMod -> mod `isModSubseqOf` bootMod || bootMod `isModSubseqOf` mod) bootModules

formatQualifiedAs :: Text -> Text -> Text
formatQualifiedAs mod alias = "import qualified " <> mod <> " as " <> alias

getFlyImports :: Context -> Text -> Text -> StaticLsM [Completion]
getFlyImports cx prefix match = do
  expandedPrefix <- pure $ Maybe.fromMaybe prefix (defaultAlias prefix)
  let bootCompletions = if isBootModule expandedPrefix then [mkBootCompletion expandedPrefix prefix match cx.path] else []
  mods <- getModules
  mods <- pure $ filter (expandedPrefix `isModSubseqOf`) mods
  completions <- for mods \mod -> do
    modCompletions <- getCompletionsForMod mod
    -- do some filtering
    modCompletions <- pure $ filter (\completion -> match `T.isPrefixOf` completion.label) modCompletions
    pure $
      fmap
        ( \completion ->
            completion
              { description = Just $ formatQualifiedAs mod prefix,
                msg = Just $ CompletionMessage {path = cx.path, kind = FlyImportCompletionKind mod prefix}
              }
        )
        modCompletions
  completions <- pure $ concat completions
  pure $ bootCompletions ++ completions

getCompletion :: Context -> StaticLsM (Bool, [Completion])
getCompletion cx = do
  logInfo $ "triggerKind: " <> T.pack (show cx.triggerKind)
  mode <- getCompletionMode cx
  logInfo $ "mode: " <> T.pack (show mode)
  case mode of
    ImportMode modPrefix -> do
      mods <- getModules
      let modsWithoutPrefix = case modPrefix of
            Just prefix -> Maybe.mapMaybe (T.stripPrefix (prefix <> ".")) mods
            Nothing -> mods
      pure $ (False, textCompletion <$> modsWithoutPrefix)
    HeaderMode mod -> do
      let label = "module " <> mod <> " where"
      pure (False, [mkCompletion label (label <> "\n$0")])
    UnqualifiedMode -> do
      fileCompletions <- getFileCompletions cx
      importCompletions <- getUnqualifiedImportCompletions cx
      pure $ (False, nubOrd $ importCompletions ++ fileCompletions)
    QualifiedMode mod match -> do
      let path = cx.path
      haskell <- getHaskell path
      let prog = Hir.parseHaskell haskell
      (_errs, prog) <- isRightOrThrowT prog
      let imports = prog.imports
      let importsWithAlias = filter (\imp -> fmap (.text) imp.alias == Just mod) imports
      -- TODO: append both flyimports and normal ones
      qualifiedCompletions <- nubOrd <$> (getCompletionsForMods $ (.mod.text) <$> importsWithAlias)
      flyImports <- case match of
        "" -> pure []
        _ -> getFlyImports cx mod match
      pure $ (match == "", qualifiedCompletions ++ flyImports)

resolveCompletionEdit :: CompletionMessage -> StaticLsM Edit
resolveCompletionEdit msg = do
  let path = msg.path
  case msg.kind of
    FlyImportCompletionKind mod alias -> do
      sourceRope <- getSourceRope path
      haskell <- getHaskell path
      change <-
        IDE.CodeActions.AutoImport.insertImportChange haskell sourceRope (formatQualifiedAs mod alias)
          & isRightOrThrowT
      pure $ Edit.singleton change

data CompletionMessage = CompletionMessage
  { path :: AbsPath,
    kind :: CompletionKind
  }
  deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON CompletionMessage

instance Aeson.FromJSON CompletionMessage

data CompletionKind
  = FlyImportCompletionKind
      -- | The module to import
      !Text
      -- | The alias to use for the import
      !Text
  deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON CompletionKind

instance Aeson.FromJSON CompletionKind

data Completion = Completion
  { label :: !Text,
    insertText :: !Text,
    labelDetail :: Maybe Text,
    description :: Maybe Text,
    detail :: Maybe Text,
    edit :: !Edit,
    msg :: Maybe CompletionMessage
  }
  deriving (Show, Eq, Ord)

mkBootCompletion :: Text -> Text -> Text -> AbsPath -> Completion
mkBootCompletion mod alias match path =
  (mkCompletion match "")
    { description = Just $ formatQualifiedAs mod alias,
      msg =
        Just $
          CompletionMessage
            { path,
              kind = FlyImportCompletionKind mod alias
            }
    }

textCompletion :: Text -> Completion
textCompletion text = mkCompletion text text

mkCompletion :: Text -> Text -> Completion
mkCompletion label insertText =
  Completion
    { label,
      detail = Nothing,
      labelDetail = Nothing,
      description = Nothing,
      insertText,
      edit = Edit.empty,
      msg = Nothing
    }

data TriggerKind = TriggerCharacter | TriggerUnknown
  deriving (Show, Eq)

data Context = Context
  { path :: AbsPath,
    pos :: !Pos,
    lineCol :: !LineCol,
    triggerKind :: !TriggerKind
  }
  deriving (Show, Eq)
