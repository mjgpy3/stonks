{-# LANGUAGE OverloadedStrings #-}

module Data.MarketValue (fetchSymbolMarketValue) where

import qualified Data.Text as T
import Data.SchwabExport (Symbol, Usd)
import Network.Wreq
import Control.Lens
import qualified Money
import Data.Aeson.Lens (key, nth, _Number)
import Data.Aeson (fromJSON, Result(..))
import Money.Aeson ()

fetchSymbolMarketValue :: Symbol -> IO (Maybe Usd)
fetchSymbolMarketValue symbol = do
  r <- get $ "https://query1.finance.yahoo.com/v8/finance/chart/" <> T.unpack symbol
  let v = r ^? responseBody . key "chart" . key "result" . nth 0 . key "meta" . key "regularMarketPrice" . _Number

  pure $ Money.denseFromDecimal Money.defaultDecimalConf =<< fmap (T.pack . show) v
