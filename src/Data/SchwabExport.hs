{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.SchwabExport (SchwabExportRow(..), ExportAction(..), Symbol, Usd) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Money
import Data.Csv
import Control.Monad (mzero)
import GHC.Generics (Generic)

data ExportAction
  = ADRMgmtFee
  | BankInterest
  | Buy
  | CashDividend
  | ForeignTaxPaid
  | JournaledShares
  | MoneyLinkTransfer
  | PrYrCashDiv
  | QualifiedDividend
  | SpecialQualDiv
  | StockSplit
  deriving stock (Show, Eq, Generic)

type Usd = Money.Dense "USD"

newtype SchwabBucks = SchwabBucks { unSchwabBucks :: Usd }

type Symbol = Text

data SchwabExportRow =
  SchwabExportRow {
    date :: Text
    , action :: ExportAction
    , symbol :: Maybe Symbol
    , description :: Text
    , quantity :: Maybe Double
    , price :: Maybe Usd
    , feesAndComm :: Maybe Usd
    , amount :: Maybe Usd
    }

instance FromNamedRecord SchwabExportRow where
  parseNamedRecord v =
    SchwabExportRow
      <$> v .: "Date"
      <*> v .: "Action"
      <*> v .: "Symbol"
      <*> v .: "Description"
      <*> v .: "Quantity"
      <*> (fmap unSchwabBucks <$> v .: "Price")
      <*> (fmap unSchwabBucks <$> v .: "Fees & Comm")
      <*> (fmap unSchwabBucks <$> v .: "Amount")

instance FromField ExportAction where
  parseField = \case
    "ADR Mgmt Fee" -> pure ADRMgmtFee
    "Bank Interest" -> pure BankInterest
    "Buy" -> pure Buy
    "Cash Dividend" -> pure CashDividend
    "Foreign Tax Paid" -> pure ForeignTaxPaid
    "Journaled Shares" -> pure JournaledShares
    "MoneyLink Transfer" -> pure MoneyLinkTransfer
    "Pr Yr Cash Div" -> pure PrYrCashDiv
    "Qualified Dividend" -> pure QualifiedDividend
    "Special Qual Div" -> pure SpecialQualDiv
    "Stock Split" -> pure StockSplit
    _ -> mzero

instance FromField SchwabBucks where
  parseField v = do
    withoutDollar <- T.replace "$" "" <$> parseField v
    maybe
      mzero (pure . SchwabBucks)
      (Money.denseFromDecimal Money.defaultDecimalConf withoutDollar)

