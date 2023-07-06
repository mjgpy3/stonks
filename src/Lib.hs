{-# LANGUAGE LambdaCase #-}

module Lib
    ( someFunc
    ) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Money
import Data.Report (rowToReport, Report(..), summarizeSymbols)
import Data.Foldable (foldMap')
import qualified Data.Vector as V
import System.Environment (getArgs)
import Data.MarketValue (fetchSymbolMarketValue)
import qualified Data.Text as T

someFunc :: IO ()
someFunc =
  getArgs >>= \case
    [path] -> do
      csvData <- BL.readFile path 
      case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, vs) ->
          print $ summarizeSymbols $ foldMap' rowToReport $ V.reverse vs

    ["check", symbol] ->
      fetchSymbolMarketValue (T.pack symbol) >>= print . fmap (Money.denseToDecimal Money.defaultDecimalConf Money.Round)

    _ ->
      putStrLn "Usage:\n  stack run -- path/to/schwab/export.csv\n(OR)\n  stack run -- check SYMBOL"
