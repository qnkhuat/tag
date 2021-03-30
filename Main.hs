{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wmissing-fields #-}
-- https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html
module Main where
-------------------------------------------------
-- `tag [color|name]` => list all tags with color
-- `tag color|name path(s)`-> add
-- `tag cd color|name index` -> cd
-- `tag cp color|name index` -> copy to clipboard
-- `tag set color name` -> alias name to color
-- `tag rm color|name path(s)` -> remove tag
-- `tag help` -> Print help
-------------------------------------------------

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import GHC.Generics (Generic)

import Text.Read (readMaybe)
import Data.Aeson (ToJSON, FromJSON, encode, decode, eitherDecode')
import Data.Map (Map, fromList)
import Data.Maybe (fromJust, maybe)
import Data.Ord (Ord)
import qualified Data.Text  as T
import qualified Data.Char as C
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import qualified System.Console.ANSI as ANSI

-- ***** Data Types *****

-- | Concept : Algebraic data types and Record Syntax
data Command = CommandAdd {
    name :: String,
    path :: FilePath
} | CommandLs {
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

data Tag = Tag {
    tagPath :: String, 
    time :: Int } 
          deriving (Generic, Show)

data Tags = Tags {
    tagName :: String, 
    tags :: [Tag] } 
         deriving (Generic, Show)

instance FromJSON Tag
instance FromJSON Tags
instance ToJSON Tag
instance ToJSON Tags

-- | Concept : Type synonyms
type TagsData = Map String Tags 

data Color = Red | Purple | Blue | Green | Yellow | White deriving (Show, Read, Ord, Eq)


-- ***** Constants *****

colorsList :: [Color]
colorsList = [Red, Purple, Blue, Green, Yellow, White] 
colorsNoWhite :: [Color]
colorsNoWhite = filter (\c -> c /= White) colorsList

-- | Concept : function application `$`
tagTemplateData :: TagsData 
tagTemplateData = Map.fromList(map makePair colorsNoWhite)
    where makePair color = (lowerString $ show $ color, Tags{tagName=lowerString $ show $ color, tags=[]})


colorANSIMap :: Map Color ANSI.Color
colorANSIMap = Map.fromList [
    (Red, ANSI.Red),
    (Purple, ANSI.Magenta),
    (Blue, ANSI.Blue),
    (Green, ANSI.Green),
    (Yellow, ANSI.Yellow),
    (White, ANSI.White)]

colorToANSI :: Color -> ANSI.Color
colorToANSI c = case c of 
                     Red -> ANSI.Red
                     Purple -> ANSI.Magenta
                     Blue -> ANSI.Blue
                     Green -> ANSI.Green
                     Yellow -> ANSI.Yellow
                     White -> ANSI.White


-- ***** Utilities *****
lowerString :: String -> String
lowerString s = map C.toLower s

capitalizeString :: String -> String
capitalizeString (s:xs) = (C.toUpper s):(lowerString xs)
        
printTags :: Color -> Tags -> IO ()
printTags c ts = do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull (colorToANSI c)]
    putStrLn $ tagName ts
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White]    
    let tagList = tags ts
    case length tagList of 0 -> putStrLn "There are no tags. Type `tag [color] [path] to add one!`"
                           _ -> mapM_ printTag (zip [0..] tagList)


printTag :: (Integer, Tag) -> IO () 
printTag (i, tag) = do 
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
    putStr $ show i
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]    
    putStr "\t"
    putStr (tagPath tag)
    putStr "\n"

-- ***** I/O *****
writeJson :: TagsData -> FilePath -> IO ()
writeJson d p = BS.writeFile p $ encode d

decodeJson :: FilePath -> IO TagsData
decodeJson p = do
    b <- BS.readFile p
    let jsonResult = fromJust $ decode b :: TagsData
    return jsonResult

readJson :: FilePath -> IO TagsData
readJson p = do 
    exists <- doesFileExist p
    if exists then
        decodeJson p
    else
       do
           writeJson tagTemplateData p
           return tagTemplateData

-- ***** Parser *****
-- | Concept : Pattern Matching
parseCommand :: [String] -> Command
parseCommand ("help": [])                = CommandHelp{}
parseCommand ("cp"  : color : path : []) = CommandCp{color = color, path = path}
parseCommand ("cd"  : color : index: []) = CommandCd{color = color, index = (read index :: Integer)}
parseCommand ("set" : color : name : []) = CommandSet{color = color, name = name}
parseCommand ("rm"  : color : index: []) = CommandRm{color=color, index = (read index :: Integer)}
parseCommand (name  : path  : [])        = CommandAdd{name = name, path = path}
parseCommand (color : [])                = CommandLs{color = color}
parseCommand ([])                        = CommandLs{color = ""}
parseCommand _                           = CommandUnknown{}


applyRunLs :: TagsData -> Color -> IO ()
applyRunLs tagsData co = do
    let c = lowerString $ show co
    let tagList = Map.lookup c tagsData
    maybe (putStrLn "Tag not found") (printTags co) $ tagList


-- ***** Runner *****
run :: Command -> IO ()
run command@(CommandHelp{}) = print command


run command@(CommandLs{}) = do
    let c = lowerString $ color command    
    tagsData <- readJson "tag.json"
    if length c /= 0
       then do 
            let co = readMaybe (capitalizeString c) :: Maybe Color
            maybe (putStrLn "Invalid tag") (applyRunLs tagsData) co
        else mapM_ (applyRunLs tagsData) colorsNoWhite
    
run command@(CommandAdd{}) = print command
    
run command@(CommandCd{}) = print command
run command@(CommandCp{}) = print command
run command@(CommandSet{}) = print command
run command@(CommandRm{}) = print command
run command@(CommandUnknown{}) = print command

main :: IO ()
main = do
    args <- getArgs
    run $ parseCommand args

    --d <- readJson "newtemplate.json"
    --putStrLn $ "Decode: " ++ show d








