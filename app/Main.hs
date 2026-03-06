{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char (toLower)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)
import System.Directory (getDirectoryContents)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Prelude

data Palabra = Palabra { palabra :: String,
                         image :: String}
               deriving (Show, Generic)

-- Constructor for Palabra that makes it lowercase and replaces spaces with hyphens
mkPalabra :: String -> String -> Palabra
mkPalabra word img = Palabra { palabra = word, image = "images/" <> img }

dummyPalabra :: Palabra
dummyPalabra = mkPalabra "Hola" "nice"

dummyPalabras = [dummyPalabra, dummyPalabra, dummyPalabra]

instance FromJSON Palabra
instance ToJSON Palabra

-- Classes as in the example
classes :: BL.ByteString
classes = "{\
  \  dropZoneContainer: \"drop-zones\",\
  \  dropItem: \"item\",\
  \  dropZone: \"drop-zone\",\
  \  image: \"image\",\
  \  word: \"word\"\
  \}"

-- Generate the JS object for a list of Palabras
genJSObject :: [Palabra] -> BL.ByteString
genJSObject palabras =
  let wordsData = map (\p -> "{image: \"" <> BL.pack (image p) <> "\", word: \"" <> BL.pack (palabra p) <> "\"}" ) palabras
      wordsDataStr = BL.intercalate ",\n        " wordsData
  in "{\n      wordsData: [\n        " <> wordsDataStr <> "\n      ],\n      classes: " <> classes <> "\n    }"

palabras :: [String] -> [Palabra]
palabras = mapMaybe (\line ->
    case splitOn "\t" line of
        [w, i] -> Just (mkPalabra w i)
        _      -> Nothing
    )           

main :: IO ()
main = do
  d <- getDirectoryContents "data"
  let files = filter (isSuffixOf ".txt") d
  mapM_ (\file -> do
    content <- readFile ("data/" <> file)
    let lines' = lines content
        -- Assuming each line is "word image"
        palab = palabras lines'
        jsObject = genJSObject palab
        outFile = "output/" <> takeWhile (/= '.') file <> ".js"
    BL.writeFile outFile ("const exerciseConfig = " <> jsObject <> ";")
    putStrLn $ "Generated " <> outFile
    ) files
