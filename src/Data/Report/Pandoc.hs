{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Report.Pandoc (reportToPandoc) where

import qualified Data.Text as T
import Data.Report
import Text.Pandoc.Builder
import qualified Data.HashMap.Monoidal as HMM
import Data.Time.Clock
import Data.String (fromString)
import Text.Pandoc.Shared (tshow)
import qualified Money
import Data.Monoid (Sum(..), First(..))
import Data.Semigroup (Min(..), Max(..))

reportToPandoc :: UTCTime -> Report SymbolDetails -> Pandoc
reportToPandoc now Report{..} = setTitle "Stonks and Vesties" $ setDate (text $ tshow now) $ doc $
  header 1 "Holdings" <> symbolLevelReport holdings <>
  header 1 "Gifts" <> symbolLevelReport gifts <>
  header 1 "All Symbols" <> symbolLevelReport allSymbols <>
  header 1 "Fees" <> feesReport fees <>
  header 1 "Bank" <> bankReport bank <>
  header 1 "Dividends" <> dividendsReport dividends

  where
    symbolLevelReport SymbolLevelReport{..} =
      header 2 "Totals" <>
      simpleTable
        [para $ strong "Total Spent", para $ strong "Total Value", para $ strong "Total Gain"]
        [[plain $ usd $ getSum totalSpent, orNa (plain . usd) $ getFirst totalValue, orNa (plain . usd) $ getFirst totalGain]] <>
      header 2 "By Symbol" <>
      simpleTable
        (para . strong <$> ["Symbol", "#", "# Sold", "$ Sale Total", "$ Spent", "Min $ per share at split", "Max $ PSAS", "# Splits", "$/Share", "Total Value", "Total Gain"])
        (toSymbolRow <$> HMM.toList symbols)

    toSymbolRow (symbol, SymbolDetails {..}) =
      [
        plain $ text symbol
      , para $ usd symbolCurrentlyHeld
      , para $ usd symbolTotalSold
      , para $ usd symbolTotalGained
      , para $ usd symbolTotalSpent
      , paraSplits $ HMM.map getMin symbolMinSpentPerShareAtSplit
      , paraSplits $ HMM.map getMax symbolMaxSpentPerShareAtSplit
      , para $ text $ tshow symbolSplitTimes
      , orNa (plain . usd) symbolCurrentShareValue
      , orNa (plain . usd) symbolCurrentTotalValue
      , orNa (plain . usd) symbolGain
      ]

    paraSplits vs =
      para $ text $ T.intercalate ", " $ (\(s, v) -> usdt v <> "@" <> tshow s) <$> HMM.toList vs

    feesReport Fees{..} =
      para $ strong "Total Fees" <> " " <> usd (getSum totalFees)

    bankReport BankStats{..} =
      simpleTable
        [para $ strong "Tx In", para $ strong "Times Tx", para $ strong "Interest Gained"]
        [[plain $ usd $ getSum transferredToSchwab, plain $ fromString $ show $ getSum timesTransferredToSchwab, plain $ usd $ getSum interestGained]]

    dividendsReport DividendStats{..} =
      simpleTable
        [para $ strong "Total Div", para $ strong "Times Div Rec"]
        [[plain $ usd $ getSum totalDividends, plain $ fromString $ show $ getSum timeDividendsWereReceived]]

    usdt = Money.denseToDecimal Money.defaultDecimalConf Money.Round
    usd = fromString . T.unpack . usdt

    orNa just = \case
      Just v -> just v
      Nothing -> plain "N/A"

--  para "This is the first paragraph" <>
--  para ("And " <> emph "another" <> ".") <>
--  bulletList [ para "item one" <> para "continuation"
--             , plain ("item two and a " <>
--                 link "/url" "go to url" "link")
--             ]
