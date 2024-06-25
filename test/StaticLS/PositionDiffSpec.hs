{-# LANGUAGE BlockArguments #-}

module StaticLS.PositionDiffSpec where

import Data.Diff qualified as Diff
import Data.Pos
import Data.Text qualified as T
import NeatInterpolation
import StaticLS.PositionDiff
import StaticLS.PositionDiff qualified as PositionDiff
import Test.Hspec

spec :: Spec
spec = do
  let check diff name pos pos' = it name do
        let dm = getDiffMapFromDiff diff
        putStrLn $ "dm: " ++ show dm
        updatePositionUsingDiff diff pos `shouldBe` pos'

  describe "simple diff" do
    let diff =
          [ Diff.Keep (mkToken "module"),
            Diff.Delete (mkToken "ca"),
            Diff.Keep (mkToken "da")
          ]

    let check' = check diff

    check' "" (Pos 0) (Pos 0)
    check' "" (Pos 1) (Pos 1)
    check' "clipped 1" (Pos 6) (Pos 6)
    check' "clipped 2" (Pos 7) (Pos 6)

  describe "last diff is delete" do
    let diff =
          [ Diff.Keep (mkToken "module"),
            Diff.Delete (mkToken "ca")
          ]
    let check' = check diff
    check' "" (Pos 6) (Pos 5)
    check' "" (Pos 7) (Pos 5)
    check' "out of bounds" (Pos 8) (Pos 8)
    pure ()

  describe "more complex diff" do
    let diff =
          [ Diff.Keep (mkToken "module"),
            Diff.Delete (mkToken "ca"),
            Diff.Keep (mkToken "da"),
            Diff.Insert (mkToken "hello"),
            Diff.Delete (mkToken "hela")
          ]
    let check' = check diff
    check' "" (Pos 8) (Pos 6)
    -- check' "" (Pos 12) (Pos 12)
    pure ()

  describe "last delta" do
    let diff =
          [ Diff.Keep (mkToken "first"),
            Diff.Delete (mkToken "hello")
          ]
    let check' = check diff
    check' "" (Pos 6) (Pos 4)

  describe "last delta only delete" do
    let diff = [
            Diff.Delete (mkToken "hello")
          ]
    let check' = check diff
    check' "" (Pos 2) (Pos 0)
    check' "" (Pos 2) (Pos 0)
    
  xit "smoke" do
    let x = "first second third fourth"
    let y = "third second third fourth"
    let diff = PositionDiff.diffText x y
    diff `shouldBe` []

  xdescribe "lexing" do
    let check name s ex = it name do
          let ts = fmap mkToken ex
          PositionDiff.lex s `shouldBe` ts

    let checkCooked name s ex = it name do
          let ts = fmap mkToken ex
          PositionDiff.lexCooked s `shouldBe` ts

    check "" "hello 'Message adfadsf adfpaoidfu  adpofiuasdfpoi aspdfoiuasfpo adsf poiasduf ' 'adf'asdf a" []

    checkCooked "" "hello 'Message adfadsf adfpaoidfu adpofiuasdfpoi apodsifu asf asdf'a sdfaspodfiu adfopi" []

    pure ()

  pure ()
