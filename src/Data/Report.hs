{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Report (Report(..), rowToReport) where

import Data.SchwabExport (SchwabExportRow(..), ExportAction(..), Symbol, Usd)
import Data.Text (Text)
import Data.Monoid (Sum(..), First(..))
import Data.HashMap.Monoidal
import Numeric.Natural
import Data.Monoid.Generic (genericMempty, genericMappend)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import qualified Data.Set as S

rowToReport :: SchwabExportRow -> Report
rowToReport SchwabExportRow {..} =
  includeSymbol $ case action of
    ADRMgmtFee -> mempty { fees = amountAsFee }
    BankInterest -> mempty { bank = interest }
    Buy -> mempty { holdings = maybe mempty buy symbol }
    CashDividend -> mempty { dividends = dividend }
    ForeignTaxPaid -> mempty { fees = amountAsFee }
    JournaledShares -> mempty { holdings = maybe mempty gift symbol }
    MoneyLinkTransfer -> mempty { bank = moneyLinkTransfer }
    PrYrCashDiv -> mempty { dividends = dividend }
    QualifiedDividend -> mempty { dividends = dividend }
    SpecialQualDiv -> mempty { dividends = dividend }
    StockSplit -> mempty { holdings = maybe mempty split symbol, gifts = maybe mempty split symbol  }

  where
    sumAmount = Sum $ fromMaybe 0 amount

    dividend = DividendStats {
      totalDividends = sumAmount
      , timeDividendsWereReceived = 1
      , dividendsBySymbol = singleton symbol sumAmount
      }

    split sym = SymbolLevelReport {
      symbols = singleton sym SymbolLevelDetails {
                                symbolEvents = [Split]
                                , symbolShareValue = First Nothing
                                , symbolTotalValue = First Nothing
                                , symbolGain = First Nothing
                              }
      , totalSpent = sumAmount
      , totalValue = First Nothing
      , totalGain = First Nothing
      }

    buy sym = SymbolLevelReport {
      symbols = singleton sym SymbolLevelDetails {
                                symbolEvents = [Bought (fromMaybe 0 quantity) $ fromMaybe 0 price]
                                , symbolShareValue = First Nothing
                                , symbolTotalValue = First Nothing
                                , symbolGain = First Nothing
                              }
      , totalSpent = sumAmount
      , totalValue = First Nothing
      , totalGain = First Nothing
      }

    gift sym = SymbolLevelReport {
      symbols = singleton sym SymbolLevelDetails {
                                symbolEvents = [WasGifted $ fromMaybe 0 quantity]
                                , symbolShareValue = First Nothing
                                , symbolTotalValue = First Nothing
                                , symbolGain = First Nothing
                              }
      , totalSpent = 0
      , totalValue = First Nothing
      , totalGain = First Nothing
      }

    interest = BankStats {
      transferredToSchwab = 0
      , timesTransferredToSchwab = 0
      , interestGained = sumAmount
    }

    moneyLinkTransfer = BankStats {
      transferredToSchwab = sumAmount
      , timesTransferredToSchwab = 1
      , interestGained = 0
    }

    amountAsFee = Fees {
      totalFees = sumAmount
      , feeEvents = [feeEvent]
      , feesBySymbol = singleton symbol sumAmount
      }

    feeEvent = FeeEvent {
      feeAction = action
      , feeSymbol = symbol
      , feeDate = date
      }

    includeSymbol report =
      case symbol of
        Just s -> report { symbolsInvolved = S.insert s (symbolsInvolved report) }
        Nothing -> report

data Report = Report {
  holdings :: SymbolLevelReport
  , gifts :: SymbolLevelReport
  , allSymbols :: SymbolLevelReport
  , fees :: Fees
  , bank :: BankStats
  , dividends :: DividendStats
  , symbolsInvolved :: S.Set Symbol
  }
  deriving stock (Show, Eq, Generic)

instance Monoid Report where
  mempty = genericMempty

instance Semigroup Report where
  (<>) = genericMappend

data SymbolLevelReport = SymbolLevelReport {
  symbols :: MonoidalHashMap Symbol SymbolLevelDetails
  , totalSpent :: Sum Usd
  , totalValue :: First Usd
  , totalGain :: First Usd
  }
  deriving stock (Show, Eq, Generic)

instance Monoid SymbolLevelReport where
  mempty = genericMempty

instance Semigroup SymbolLevelReport where
  (<>) = genericMappend

data BankStats
  = BankStats {
     transferredToSchwab :: Sum Usd
     , timesTransferredToSchwab :: Sum Natural
     , interestGained :: Sum Usd
   }
  deriving stock (Show, Eq, Generic)

instance Monoid BankStats where
  mempty = genericMempty

instance Semigroup BankStats where
  (<>) = genericMappend

data SymbolEvent
  = Bought Double Usd
  | Sold Double Usd
  | WasGifted Double
  | Split
  deriving (Show, Eq)

data SymbolLevelDetails =
  SymbolLevelDetails {
    symbolEvents :: [SymbolEvent]
    , symbolShareValue :: First Usd
    , symbolTotalValue :: First Usd
    , symbolGain :: First Usd
  }
  deriving stock (Show, Eq, Generic)

instance Semigroup SymbolLevelDetails  where
  (<>) = genericMappend

data Fees = Fees {
  totalFees :: Sum Usd
  , feeEvents :: [FeeEvent]
  , feesBySymbol :: MonoidalHashMap (Maybe Symbol) (Sum Usd)
  }
  deriving stock (Show, Eq, Generic)

instance Monoid Fees where
  mempty = genericMempty

instance Semigroup Fees  where
  (<>) = genericMappend

data FeeEvent = FeeEvent {
  feeAction :: ExportAction
  , feeSymbol :: Maybe Symbol
  , feeDate :: Text
  }
  deriving stock (Show, Eq)

data DividendStats = DividendStats {
  totalDividends :: Sum Usd
  , timeDividendsWereReceived :: Sum Natural
  , dividendsBySymbol :: MonoidalHashMap (Maybe Symbol) (Sum Usd)
  }
  deriving stock (Show, Eq, Generic)

instance Monoid DividendStats where
  mempty = genericMempty

instance Semigroup DividendStats where
  (<>) = genericMappend
