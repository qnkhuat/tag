-- https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.Text
import Data.Aeson 
import Data.Map (Map)
import qualified Data.ByteString.Lazy as BS


data Tag = Tag {
    path :: Text, 
    time :: Int } 
          deriving (Generic, Show)

data Tags = Tags {
    name :: Text, 
    tags :: [Tag] } 
         deriving (Generic, Show)

instance FromJSON Tag
instance FromJSON Tags
instance ToJSON Tag
instance ToJSON Tags


main :: IO ()
main = do
    a <- BS.readFile "template.json"
    -- putStrLn $ "Decode: " ++ (show (decode a :: Maybe (Map Text Tags)))
    BS.writeFile "newtemplate.json" (encode (decode a :: Maybe (Map Text Tags)))
    
