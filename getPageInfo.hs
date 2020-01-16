module Main where

import Network.HTTP
import GHC.IO.Encoding
import Data.List
import Data.Set


data Actor = Director | ABRAHAM | Apothecary | BALTHASAR | BENVOLIO | CAPULET | Chorus | FRIAR_JOHN | FRIAR_LAURENCE |
             First_Citizen | First_Musician | First_Servant | First_Watchman | GREGORY | JULIET | LADY_CAPULET | 
             LADY_MONTAGUE | MERCUTIO | MONTAGUE | Musician | Nurse | PAGE | PARIS | PETER | PRINCE | ROMEO | 
             SAMPSON | Second_Capulet | Second_Musician | Second_Servant | Second_Watchman | Servant | TYBALT |
             Third_Musician | Third_Watchman
    deriving (Show)



-- <A NAME=speech32><b>JULIET</b></a>, tags of these forms we target

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)




dropUntilSpeechChars :: [String] -> [String]
dropUntilSpeechChars [] = []
dropUntilSpeechChars (x:xs) = if Data.List.take 4 x == "<H3>" || Data.List.take 4 x == "<h3>" 
                              then [Data.List.drop 4 (Data.List.take ((length x) - 5) x)] ++ dropUntilSpeechChars xs
                              else [Data.List.drop 8 x] ++ dropUntilSpeechChars xs

booleanIfSpeech :: String -> Bool
booleanIfSpeech str = if (Data.List.take 6 str) == "speech"
                      then True
                      else False

removeEndTags :: String -> String
removeEndTags str = Data.List.take (length str - 8) str


removeFrontTagsForSpeech :: String -> String
removeFrontTagsForSpeech str = Data.List.drop 2 (dropWhile ('b'/=) str) 

removeFrontTagsForDialogue :: String -> String
removeFrontTagsForDialogue str = Data.List.drop 1 (dropWhile ('>'/=) str) 

getActor :: String -> Actor
getActor str = case removeFrontTagsForSpeech (removeEndTags str) of [] -> Director
                                                                    "ABRAHAM" -> ABRAHAM
                                                                    "Apothecary" -> Apothecary
                                                                    "BALTHASAR" -> BALTHASAR
                                                                    "BENVOLIO" -> BENVOLIO
                                                                    "CAPULET" -> CAPULET
                                                                    "Chorus" -> Chorus
                                                                    "FRIAR JOHN" -> FRIAR_JOHN
                                                                    "FRIAR LAURENCE" -> FRIAR_LAURENCE
                                                                    "First Citizen" -> First_Citizen
                                                                    "First Musician" -> First_Musician
                                                                    "First Servant" -> First_Servant
                                                                    "First Watchman" -> First_Watchman
                                                                    "GREGORY" -> GREGORY
                                                                    "JULIET" -> JULIET
                                                                    "LADY  CAPULET" -> LADY_CAPULET
                                                                    "LADY CAPULET" -> LADY_CAPULET
                                                                    "LADY MONTAGUE" -> LADY_MONTAGUE
                                                                    "MERCUTIO" -> MERCUTIO
                                                                    "MONTAGUE" -> MONTAGUE
                                                                    "Musician" -> Musician
                                                                    "NURSE" -> Nurse
                                                                    "Nurse" -> Nurse
                                                                    "PAGE" -> PAGE
                                                                    "PARIS" -> PARIS
                                                                    "PETER" -> PETER
                                                                    "PRINCE" -> PRINCE
                                                                    "ROMEO" -> ROMEO
                                                                    "SAMPSON" -> SAMPSON
                                                                    "Second Capulet" -> Second_Capulet
                                                                    "Second Musician" -> Second_Musician
                                                                    "Second Servant" -> Second_Servant
                                                                    "Second Watchman" -> Second_Watchman
                                                                    "Servant" -> Servant
                                                                    "TYBALT" -> TYBALT
                                                                    "Third Musician" -> Third_Musician
                                                                    "Third Watchman" -> Third_Watchman
                                                                    otherwise -> Director



actorAssignment :: [String] -> Actor -> [(Actor, String)] 
actorAssignment [] _ = []
actorAssignment (x:xs) actor = if booleanIfSpeech x
                               then actorAssignment xs (getActor x)
                               else (if Data.List.take 1 x == "1" || Data.List.take 1 x == "2" || Data.List.take 1 x == "3" ||
                                     Data.List.take 1 x == "4" || Data.List.take 1 x == "5"
                                     then [(actor, removeFrontTagsForDialogue (removeEndTags x))] ++ actorAssignment xs actor
                                     else (if Data.List.take 3 x == "ACT" || Data.List.take 3 x == "PRO" || Data.List.take 3 x == "SCE" 
                                           then [(Director, x)] ++ actorAssignment xs actor
                                           else actorAssignment xs actor))

recursiveShow :: [(Actor, String)] -> String
recursiveShow [] = []
recursiveShow (x:xs) = show x ++ ",\n  " ++ recursiveShow xs





main :: IO ()
main = do
    setLocaleEncoding utf8
    src <- openURL "http://shakespeare.mit.edu/romeo_juliet/full.html"
    --putStr (head (lines src))        
    writeFile "src12.htm" (recursiveShow (actorAssignment (dropUntilSpeechChars (lines src)) Director))
       
    