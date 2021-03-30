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
import System.Directory (doesFileExist, doesPathExist, makeAbsolute)
import System.Exit (exitWith)
import System.Process (callCommand)
import GHC.Generics (Generic) 

import Control.Monad.Except (throwError, catchError)
import Text.Read (readMaybe)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.Map (Map, fromList, insertWithKey, insert)
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
    color :: String,
    path :: FilePath
} | CommandLs {
    color :: String
} | CommandCd {
    color :: String,
    index :: Int
} | CommandCp {
    color :: String,
    index :: Int
} | CommandSet {
    color :: String,
    name :: String 
} | CommandRm {
    color :: String,
    index :: Int
} | CommandHelp | CommandUnknown deriving (Show)

data Tags = Tags {
    tagName :: String, 
    paths :: [FilePath] } 
         deriving (Generic, Show)

data TagError = NotFound String 
              | Invalid String
              deriving (Show)

instance FromJSON Tags
instance ToJSON Tags

-- | Concept : Type synonyms
type TagsData = Map String Tags 
type ThrowsError = Either TagError

data Color = Red | Purple | Blue | Green | Yellow | White deriving (Show, Read, Ord, Eq)

-- ***** Constants *****

tagFilePath :: FilePath
tagFilePath = "tag.json"

colorsList :: [Color]
colorsList = [Red, Purple, Blue, Green, Yellow, White] 

colorsNoWhite :: [Color]
colorsNoWhite = filter (\c -> c /= White) colorsList

-- | Concept : function application `$`
tagTemplateData :: TagsData 
tagTemplateData = Map.fromList(map makePair colorsNoWhite)
    where makePair color = (lowerString $ show $ color, Tags{tagName=lowerString $ show $ color, paths=[]})

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

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where (lft, (_:rgt)) = splitAt idx xs
        
printTags :: Color -> Tags -> IO ()
printTags c ts = do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull (colorToANSI c)]
    putStrLn $ tagName ts
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White]    
    let tagList = paths ts
    case length tagList of 0 -> putStrLn "There are no tags. Type `tag [color] [path] to add one!`"
                           _ -> mapM_ printTag (zip [0..] tagList)


printTag :: (Int, FilePath) -> IO () 
printTag (i, tag) = do 
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
    putStr $ show i
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]    
    putStr "\t"
    putStr tag
    putStr "\n"

-- ***** I/O *****
writeJson :: TagsData -> FilePath -> IO ()
writeJson d p = BS.writeFile p $ encode d

decodeJson :: FilePath -> IO TagsData
decodeJson p = do
    b <- BS.readFile p
    let jsonResult = fromJust $ decode b :: TagsData -- TODO :handle case failed to decode
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

insertTag :: FilePath -> Tags -> Tags
insertTag filepath t = do
    if filepath `elem` (paths t) 
       then t
       else Tags{tagName = (tagName t), paths = (paths t) ++ [filepath]}

insertTags :: TagsData -> String -> FilePath -> TagsData
insertTags tagsData key path = do
    let currentTags = Map.lookup key tagsData
    maybe tagsData (\x -> insert key (insertTag path x) tagsData) currentTags


removeTag :: Int -> Tags -> Tags
removeTag i t = do
    if length (paths t) <= i
       then t
       else Tags{tagName = (tagName t), paths = deleteAt i (paths t)}

removeTags :: TagsData -> String -> Int -> TagsData
removeTags tagsData key i = do
    let currentTags = Map.lookup key tagsData
    maybe tagsData (\x -> insert key (removeTag i x) tagsData) currentTags


-- ***** Error Handling *****
trapError action = catchError action (return . show)
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- ***** Parser *****
-- | Concept : Pattern Matching
parseCommand :: [String] -> Command
parseCommand ("help": [])                = CommandHelp{}
parseCommand (color : index : "cp"  : []) = CommandCp{color = color, index = (read index :: Int)}
parseCommand (color : index : "cd"  : []) = CommandCd{color = color, index = (read index :: Int)}
parseCommand (color : name  : "set" : []) = CommandSet{color = color, name = name}
parseCommand (color : index : "rm"  : []) = CommandRm{color = color, index = (read index :: Int)}
parseCommand (color : path  : [])        = CommandAdd{color = color, path = path}
parseCommand (color : [])                = CommandLs{color = color}
parseCommand ([])                        = CommandLs{color = ""}
parseCommand _                           = CommandUnknown{}

-- ***** Runner *****
applyRunLs :: TagsData -> Color -> IO ()
applyRunLs tagsData co = do
    let c = lowerString $ show co
    let tagList = Map.lookup c tagsData
    maybe (putStrLn $ "Invalid tag: " ++ c) (printTags co) $ tagList

run :: Command -> IO ()
run command@(CommandHelp{}) = print command

run command@(CommandLs{}) = do
    let c = lowerString $ color command    
    tagsData <- readJson tagFilePath
    if length c /= 0
       then do 
            let co = readMaybe (capitalizeString c) :: Maybe Color
            maybe (putStrLn $ "Invalid tag: " ++  c) (applyRunLs tagsData) co
        else mapM_ (applyRunLs tagsData) colorsNoWhite

run command@(CommandAdd{}) = do -- print command
    let c = lowerString $ color command
    tagsData <- readJson tagFilePath
    let tagList = Map.lookup c tagsData
    case tagList of
         Nothing -> putStrLn $ "Invalid tag: " ++ c
         _ -> do
             filepath <-  makeAbsolute $ path command
             exist <- doesPathExist filepath
             if exist
                then do 
                    writeJson (insertTags tagsData c filepath ) tagFilePath
                    putStrLn $ "Added " ++ filepath ++ " to tag " ++ c
                else putStrLn "Invalid path"

run command@(CommandCd{}) = do
    let c = lowerString $ color command
    tagsData <- readJson tagFilePath
    let tagList = Map.lookup c tagsData
    case tagList of
         Nothing -> putStrLn $ "Invalid tag: " ++ c
         _ -> do
             let pathList = (paths $ fromJust tagList)
             let pathIndex = index command
             if length pathList >= pathIndex
                then callCommand $ "cd " ++ (pathList !! pathIndex)
                else putStrLn $ "Index out of range for tag: " ++ c
             
run command@(CommandCp{}) = do
    let c = lowerString $ color command
    tagsData <- readJson tagFilePath
    let tagList = Map.lookup c tagsData
    case tagList of
         Nothing -> putStrLn $ "Invalid tag: " ++ c
         _ -> do
             let pathList = (paths $ fromJust tagList)
             let pathIndex = index command
             if length pathList >= pathIndex
                then do 
                    callCommand $ "echo \"" ++ pathList !! pathIndex ++"\" | pbcopy"-- TODO : Only work on OSX
                    putStrLn "Copied to your clipboard!"
                else putStrLn $ "Index out of range for tag: " ++ c


run command@(CommandRm{}) = do
    let c = lowerString $ color command
    tagsData <- readJson tagFilePath
    let tagList = Map.lookup c tagsData
    case tagList of
         Nothing -> putStrLn $ "Invalid tag: " ++ c
         _ -> do
             writeJson (removeTags tagsData c (index command)) tagFilePath -- TODO: warning when index out of range
             putStrLn $ "Removed!"

run command@(CommandSet{}) = print command
run command@(CommandRm{}) = print command
run command@(CommandUnknown{}) = putStrLn "Unknown command!"

main :: IO ()
main = do
    args <- getArgs
    run $ parseCommand args






