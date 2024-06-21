module StaticLS.Hir where

import AST qualified as AST
import AST.Haskell qualified as H
import Control.Applicative (asum, (<|>))
import Data.Either qualified as Either
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
import Data.Text (Text)

data Module = Module
  { ids :: NonEmpty Text,
    text :: Text
  }
  deriving (Show, Eq)

data ImportName = ImportName
  { name :: Text
  }
  deriving (Show, Eq)

data Import = Import
  { mod :: Module,
    alias :: Maybe Module,
    qualified :: !Bool,
    hiding :: !Bool,
    importList :: [ImportName]
  }
  deriving (Show, Eq)

findNode :: (AST.DynNode -> Maybe b) -> AST.DynNode -> Maybe b
findNode f n = go n
  where
    go n = f n <|> asum (go <$> (AST.nodeChildren n))

parseImportName :: H.ImportName -> AST.Err ImportName
parseImportName name = do
  let text = AST.nodeToText name
  pure $ ImportName {name = text}

parseImportList :: H.ImportList -> AST.Err [ImportName]
parseImportList i = do
  names <- AST.collapseErr i.name
  names <- traverse parseImportName names
  pure names

parseModule :: H.Module -> AST.Err Module
parseModule m = do
  ids <- AST.collapseErr m.children
  pure $
    Module
      { text = AST.nodeToText m,
        ids = fmap AST.nodeToText ids
      }

parseImport :: H.Import -> AST.Err Import
parseImport i = do
  mod <- i.module'
  mod <- parseModule mod
  alias <- AST.collapseErr i.alias
  alias <- traverse parseModule alias
  importList <- AST.collapseErr i.names
  importList <- traverse parseImportList importList
  importList <- pure $ Maybe.fromMaybe [] importList
  let qualified = Maybe.isJust $ findNode (AST.cast @(AST.Token "qualified")) (AST.getDynNode i)
  let hiding = Maybe.isJust $ findNode (AST.cast @(AST.Token "hiding")) (AST.getDynNode i)
  pure
    Import
      { mod,
        alias,
        qualified,
        hiding,
        importList
      }

parseImports :: H.Imports -> AST.Err ([Text], [Import])
parseImports i = do
  import' <- i.import'
  let (es, imports) = Either.partitionEithers (NE.toList import')
  imports <- pure $ parseImport <$> imports
  let (es', imports') = Either.partitionEithers imports
  pure (es ++ es', imports')

data Program = Program
  { imports :: [Import]
  }
  deriving (Show, Eq)

parseHaskell :: H.Haskell -> AST.Err ([Text], Program)
parseHaskell h = do
  imports <- AST.collapseErr h.imports
  (es, imports) <- case imports of
    Nothing -> pure ([], [])
    Just imports -> parseImports imports
  pure (es, Program {imports})
