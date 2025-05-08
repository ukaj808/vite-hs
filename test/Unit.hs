module Main (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Test.HUnit
import Vite

testViteHtmlCorrectManifest1 :: Test
testViteHtmlCorrectManifest1 =
  TestCase
    ( do
        testManifestFile <- LBS.readFile "test/resources/test-manifest-1.json"
        expectedFooOutput <- UTF8.toString <$> LBS.readFile "test/resources/test-manifest-1-foo.html"
        expectedBarOutput <- UTF8.toString <$> LBS.readFile "test/resources/test-manifest-1-bar.html"
        viteManifest <- case (decode testManifestFile :: Maybe ViteManifest) of
          Nothing -> assertFailure "Unable to decode test-manifest-1.json"
          Just vm -> return vm
        assertEqual
          "For test-manifest-1.json, foo entry file"
          (Right expectedFooOutput)
          (viteHeadHtml "foo" viteManifest)
        assertEqual
          "For test-manifest-1.json, bar entry file"
          (Right expectedBarOutput)
          (viteHeadHtml "bar" viteManifest)
    )

testViteHtmlCorrectManifest2 :: Test
testViteHtmlCorrectManifest2 =
  TestCase
    ( do
        testManifestFile <- LBS.readFile "test/resources/test-manifest-2.json"
        expectedUserOutput <- UTF8.toString <$> LBS.readFile "test/resources/test-manifest-2-user.html"
        expectedPubOutput <- UTF8.toString <$> LBS.readFile "test/resources/test-manifest-2-pub.html"
        expectedLibOutput <- UTF8.toString <$> LBS.readFile "test/resources/test-manifest-2-lib.html"
        expectedHomeOutput <- UTF8.toString <$> LBS.readFile "test/resources/test-manifest-2-home.html"
        expectedAboutOutput <- UTF8.toString <$> LBS.readFile "test/resources/test-manifest-2-about.html"
        viteManifest <- case (decode testManifestFile :: Maybe ViteManifest) of
          Nothing -> assertFailure "Unable to decode test-manifest-2.json"
          Just vm -> return vm
        assertEqual
          "For test-manifest-2.json, user entry file"
          (Right expectedUserOutput)
          (viteHeadHtml "user" viteManifest)
        assertEqual
          "For test-manifest-2.json, pub entry file"
          (Right expectedPubOutput)
          (viteHeadHtml "pub" viteManifest)
        assertEqual
          "For test-manifest-2.json, pub entry file"
          (Right expectedLibOutput)
          (viteHeadHtml "lib" viteManifest)
        assertEqual
          "For test-manifest-2.json, home entry file"
          (Right expectedHomeOutput)
          (viteHeadHtml "home" viteManifest)
        assertEqual
          "For test-manifest-2.json, about entry file"
          (Right expectedAboutOutput)
          (viteHeadHtml "about" viteManifest)
    )

realManifestTests :: Test
realManifestTests =
  TestList
    [ TestLabel "test_vite_html_correct_manifest_1" testViteHtmlCorrectManifest1
    , TestLabel "test_vite_html_correct_manifest_2" testViteHtmlCorrectManifest2
    ]

main :: IO ()
main = runTestTTAndExit realManifestTests
