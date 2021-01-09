-- stack --resolver lts-16.27 script --optimize --package aeson,vector,unordered-containers

{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import qualified Data.HashMap.Strict           as M
import           Data.Maybe                     ( fromJust )
import           Data.Vector                    ( toList )

main :: IO ()
main = do
  jsonMaybe <- decodeFileStrict "input.txt" :: IO (Maybe Value)
  let jsonData = fromJust jsonMaybe
  putStrLn $ "Part 1: " ++ (show $ (round $ addJson False jsonData :: Int))
  putStrLn $ "Part 2: " ++ (show $ (round $ addJson True jsonData :: Int))
 where
  addJson _   (Number x) = x
  addJson red (Array  a) = sum $ map (addJson red) $ toList a
  addJson red (Object o) | red && String "red" `elem` M.elems o = 0
                         | otherwise = sum $ map (addJson red) $ M.elems o
  addJson _ _ = 0
