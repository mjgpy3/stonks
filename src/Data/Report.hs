{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Report (Report(..), rowToReport, summarizeSymbols) where

import Data.SchwabExport (SchwabExportRow(..), ExportAction(..), Symbol, Usd)
import Data.Text (Text)
import Data.Monoid (Sum(..), First(..))
import Data.HashMap.Monoidal
import qualified Data.HashMap.Monoidal as HMM
import Numeric.Natural
import Data.Monoid.Generic (genericMempty, genericMappend)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Min(..), Max(..))
import Data.Foldable (foldl')
import GHC.Generics (Generic)
import qualified Data.Set as S

summarizeSymbols :: Report [SymbolEvent] -> Report SymbolDetails
summarizeSymbols report@Report{..} =
  report {
    holdings = summarizeDetails holdings
    , gifts = summarizeDetails gifts
    , allSymbols = summarizeDetails allSymbols
         }

  where
    summarizeDetails details@SymbolLevelReport {..} =
      details {
        symbols = HMM.map (foldl' summarize emptySymbolDetails) symbols
       }

rowToReport :: SchwabExportRow -> Report [SymbolEvent]
rowToReport SchwabExportRow {..} =
  includeSymbol $ case action of
    ADRMgmtFee -> mempty { fees = amountAsFee }
    BankInterest -> mempty { bank = interest }
    Buy -> mempty { holdings = maybe mempty buy symbol, allSymbols = maybe mempty buy symbol }
    CashDividend -> mempty { dividends = dividend }
    ForeignTaxPaid -> mempty { fees = amountAsFee }
    JournaledShares -> mempty { holdings = maybe mempty gift symbol, allSymbols = maybe mempty gift symbol }
    MoneyLinkTransfer -> mempty { bank = moneyLinkTransfer }
    PrYrCashDiv -> mempty { dividends = dividend }
    QualifiedDividend -> mempty { dividends = dividend }
    SpecialQualDiv -> mempty { dividends = dividend }
    StockSplit ->
      let
        r = mempty :: Report [SymbolEvent]
      in
        r { holdings = maybe mempty split symbol, gifts = maybe mempty split symbol, allSymbols = maybe mempty split symbol }

  where
    sumAmount = Sum $ fromMaybe 0 amount

    dividend = DividendStats {
      totalDividends = sumAmount
      , timeDividendsWereReceived = 1
      , dividendsBySymbol = singleton symbol sumAmount
      }

    split sym = SymbolLevelReport {
      symbols = singleton sym [Split]
      , totalSpent = sumAmount
      , totalValue = First Nothing
      , totalGain = First Nothing
      }

    buy sym = SymbolLevelReport {
      symbols = singleton sym [Bought (fromMaybe 0 quantity) $ fromMaybe 0 price]
      , totalSpent = sumAmount
      , totalValue = First Nothing
      , totalGain = First Nothing
      }

    gift sym = SymbolLevelReport {
      symbols = singleton sym [WasGifted $ fromMaybe 0 quantity]
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

data Report sym = Report {
  holdings :: SymbolLevelReport sym
  , gifts :: SymbolLevelReport sym
  , allSymbols :: SymbolLevelReport sym
  , fees :: Fees
  , bank :: BankStats
  , dividends :: DividendStats
  , symbolsInvolved :: S.Set Symbol
  }
  deriving stock (Show, Eq, Generic)

instance Monoid sym => Monoid (Report sym) where
  mempty = genericMempty

instance Semigroup sym => Semigroup (Report sym) where
  (<>) = genericMappend

data SymbolLevelReport sym = SymbolLevelReport {
  symbols :: MonoidalHashMap Symbol sym
  , totalSpent :: Sum Usd
  , totalValue :: First Usd
  , totalGain :: First Usd
  }
  deriving stock (Show, Eq, Generic)

instance Monoid sym => Monoid (SymbolLevelReport sym) where
  mempty = genericMempty

instance Semigroup sym => Semigroup (SymbolLevelReport sym) where
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
  = Bought Usd Usd
  | Sold Usd Usd
  | WasGifted Usd
  | Split
  deriving (Show, Eq)

summarize :: SymbolDetails -> SymbolEvent -> SymbolDetails
summarize res@SymbolDetails {..} = \case
  Bought qty price -> res {
    symbolCurrentlyHeld = symbolCurrentlyHeld + qty
    , symbolTotalSpent = symbolTotalSpent + (qty*price)
    , symbolMinSpentPerShareAtSplit = singleton symbolSplitTimes $ Min price
    , symbolMaxSpentPerShareAtSplit = singleton symbolSplitTimes $ Max price
    }
  WasGifted qty -> res { symbolCurrentlyHeld = symbolCurrentlyHeld + qty }
  Sold qty price -> res { symbolCurrentlyHeld = symbolCurrentlyHeld - qty, symbolTotalGained = symbolTotalGained + (qty*price), symbolTotalSold = symbolTotalSold + qty }
  Split -> res { symbolCurrentlyHeld = 2*symbolCurrentlyHeld, symbolSplitTimes = symbolSplitTimes + 1 }

data SymbolDetails
  = SymbolDetails {
    symbolCurrentlyHeld :: Usd
    -- ^ How many are currently held
    , symbolTotalSold :: Usd
    -- ^ How many total were sold
    , symbolTotalGained :: Usd
    -- ^ How much did sales result in
    , symbolTotalSpent :: Usd
    -- ^ How much has been spent at this point
    , symbolMinSpentPerShareAtSplit :: MonoidalHashMap Natural (Min Usd)
    -- ^ Min spent per share, at split point
    , symbolMaxSpentPerShareAtSplit :: MonoidalHashMap Natural (Max Usd)
    -- ^ Max spent per share, at split point
    , symbolSplitTimes :: Natural
    -- ^ How many times has this split
    }
  deriving (Show, Eq)

emptySymbolDetails :: SymbolDetails
emptySymbolDetails = SymbolDetails {
  symbolCurrentlyHeld = 0
  , symbolTotalSold = 0
  , symbolTotalGained = 0
  , symbolTotalSpent = 0
  , symbolMinSpentPerShareAtSplit = mempty
  , symbolMaxSpentPerShareAtSplit = mempty
  , symbolSplitTimes = 0
  }

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
