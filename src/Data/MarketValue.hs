{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.MarketValue (fetchSymbolMarketValue, fetchSymbolMarketValues) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.SchwabExport (Symbol, Usd)
import Data.Traversable (for)
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Network.Wreq
import Control.Lens
import qualified Money
import Data.Aeson.Lens (key, nth, _Number)

fetchSymbolMarketValue :: Symbol -> IO (Maybe Usd)
fetchSymbolMarketValue symbol = do
  r <- get $ "https://query1.finance.yahoo.com/v8/finance/chart/" <> T.unpack symbol
  let v = r ^? responseBody . key "chart" . key "result" . nth 0 . key "meta" . key "regularMarketPrice" . _Number

  pure $ Money.denseFromDecimal Money.defaultDecimalConf =<< fmap (T.pack . show) v

fetchSymbolMarketValues :: Foldable t => t Symbol -> IO (M.Map Symbol Usd)
fetchSymbolMarketValues symbols =
  fmap (M.fromList . mapMaybe (\(sym, mUsd) -> (sym,) <$> mUsd)) $ for (toList symbols) $ \symbol ->
    (symbol, ) <$> fetchSymbolMarketValue symbol
