{-# LANGUAGE LambdaCase #-}

module Lib
    ( someFunc
    ) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Report (rowToReport, Report(..))
import Data.Foldable (foldMap')
import qualified Data.Vector as V
import System.Environment (getArgs)

someFunc :: IO ()
someFunc =
  getArgs >>= \case
    [path] -> do
      csvData <- BL.readFile path 
      case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, vs) ->
          print $ holdings $ foldMap' rowToReport $ V.reverse vs

    _ -> putStrLn "Usage: stack run -- path/to/schwab/export.csv"
