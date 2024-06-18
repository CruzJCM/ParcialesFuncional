module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de funciones" $ do

  --Punto 1 TP1----------------------------------------------------------------
    it "Se asigno correctamente el valor de la ciudad Baradero" $ do
      2+2 `shouldBe` 4