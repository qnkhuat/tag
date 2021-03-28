{-# LANGUAGE OverloadedStrings #-}
module Main where
-- https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html
--{-# LANGUAGE DeriveGeneric #-}
--
--module Main where
--
--import GHC.Generics
--import Data.Text
--import Data.Aeson 
--import Data.Map (Map)
--import qualified Data.ByteString.Lazy as BS
--
--
--data Tag = Tag {
--    path :: Text, 
--    time :: Int } 
--          deriving (Generic, Show)
--
--data Tags = Tags {
--    name :: Text, 
--    tags :: [Tag] } 
--         deriving (Generic, Show)
--
--instance FromJSON Tag
--instance FromJSON Tags
--instance ToJSON Tag
--instance ToJSON Tags
--
--
--main :: IO ()
--main = do
--    a <- BS.readFile "template.json"
--    -- putStrLn $ "Decode: " ++ (show (decode a :: Maybe (Map Text Tags)))
--    BS.writeFile "newtemplate.json" (encode (decode a :: Maybe (Map Text Tags)))
    
-- import System.Process
-- 
-- main = callCommand "touch abc.txt"
--
import System.Environment (getArgs)
--import Prelude (putStrLn, list)
--import Prelude (IO, Show, FilePath, String, Integer, Enum)
import Prelude


{-
`tag [color|name]` => list all tags with color
`tag color|name path(s)`-> add
`tag cd color|name index` -> cd
`tag cp color|name index` -> copy to clipboard
`tag set color name` -> alias name to color
`tag rm color|name path(s)` -> remove tag
`tag help` -> Print help
-}
data Command = CommandList {
    color :: String
} | CommandCd {
    color :: String,
    index :: Integer
} | CommandCp {
    color :: String,
    path :: FilePath
} | CommandSet {
    color :: String,
    name :: String 
} | CommandRm {
    color :: String,
    index :: Integer
} | CommandHelp | CommandUnknown deriving (Show)

data Colors = Red | Purple | Blue | Green | Yellow | Gray deriving (Enum, Show, Eq)


parseCommand :: [String] -> Command
-- parseCommand Nil = "List without color"
-- parseCommand (x)= "List without color"
-- `tag cp color|name index` -> copy to clipboard
--parseCommand ("cp" : color : path : [])  = "cp color: " ++ color ++ ", path: " ++ path
parseCommand ("help":[])  = CommandHelp{}
parseCommand ("cp" : color : path : [])  = CommandCp{color=color, path=path}
parseCommand ("cd" : color : index: []) = CommandCd{color=color, index = (read index :: Integer)}
parseCommand ("set" : color : name : []) = CommandSet{color=color, name=name}
parseCommand ("rm" : color : index: []) = CommandRm{color=color, index = (read index :: Integer)}
parseCommand (color:[]) = CommandList{color=color}
parseCommand ([]) = CommandList{}
parseCommand _ = CommandUnknown{}

main :: IO ()
main = do
    args <- getArgs
    print(parseCommand args)
    --putStrLn Red == "Red"































