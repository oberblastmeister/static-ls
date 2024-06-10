{-# LANGUAGE DuplicateRecordFields #-}

module TestImport.TestData where

import Data.LineColRange (LineColRange (..))
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Pos (LineCol (..))
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.IDE.FileWith (FileLcRange, FileWith (..))
import System.Directory qualified as Dir

myFunDefTdiAndPosition :: (LSP.TextDocumentIdentifier, LSP.Position)
myFunDefTdiAndPosition =
  let pos =
        LSP.Position
          { LSP._line = unsafeIntToUInt 4
          , LSP._character = unsafeIntToUInt 0
          }
      tdi =
        LSP.TextDocumentIdentifier $ LSP.filePathToUri "test/TestData/Mod2.hs"
   in (tdi, pos)

myFunDefLocation :: IO FileLcRange
myFunDefLocation = do
  absPath <- Path.filePathToAbs "test/TestData/Mod2.hs"
  pure $
    FileWith
      absPath
      LineColRange
        { start = LineCol 10 0
        , end = LineCol 10 5
        }

myFunRef1TdiAndPosition :: IO (AbsPath, LineCol)
myFunRef1TdiAndPosition = do
  path <- Path.filePathToAbs "test/TestData/Mod1.hs"
  let pos = LineCol 10 18
  pure (path, pos)

unsafeIntToUInt :: Int -> LSP.UInt
unsafeIntToUInt = fromIntegral
