{-# LANGUAGE LambdaCase #-}

module Lib
    ( someFunc
    ) where

import Prelude hiding (writeFile)

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Money
import Data.Report (rowToReport, summarizeSymbols, Report(symbolsInvolved), fillMarketValues)
import Data.Foldable (foldMap', for_)
import qualified Data.Vector as V
import System.Environment (getArgs)
import Data.MarketValue (fetchSymbolMarketValue, fetchSymbolMarketValues)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Writers.Org (writeOrg)
import Text.Pandoc.Writers.Markdown (writeMarkdown)
import Data.Default (def)
import Data.Report.Pandoc (reportToPandoc)
import Data.Text.IO (writeFile)
import Text.Pandoc.Class hiding (getCurrentTime)
import Control.Monad.IO.Class (liftIO)

someFunc :: IO ()
someFunc =
  getArgs >>= \case
    ["report", path] -> do
      csvData <- BL.readFile path
      case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, vs) -> do
          now <- getCurrentTime
          let reportWithoutMarket = summarizeSymbols $ foldMap' rowToReport $ V.reverse vs
          market <- fetchSymbolMarketValues $ symbolsInvolved reportWithoutMarket
          let reportWithMarket = fillMarketValues market reportWithoutMarket
          let pandocReport = reportToPandoc now reportWithMarket
          runIOorExplode $ for_ [(writeHtml5String, "html"), (writeOrg, "org"), (writeMarkdown, "md")] $ \(write, extension) -> do
            let fileName = "stonks_and_vesties_" <> formatTime defaultTimeLocale "%s" now <> "." <> extension
            contents <- write def pandocReport
            liftIO $ writeFile fileName contents

    "check":symbols ->
      for_ symbols $ \symbol -> do
        print symbol
        fetchSymbolMarketValue (T.pack symbol) >>= print . fmap (Money.denseToDecimal Money.defaultDecimalConf Money.Round)

    _ ->
      putStrLn "Usage:\n  stack run -- report path/to/schwab/export.csv\n(OR)\n  stack run -- check SYMBOL1 [...SYMBOLN]"
