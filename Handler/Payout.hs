module Handler.Payout where

import Import
import Handler.Common
import qualified Data.Text as T

data Payment = Payment
  { paymentAmount :: Int
  , paymentDesc :: T.Text
  }

getPayoutR :: Handler Html
getPayoutR = do
  (payoutWidget, enctype) <- generateFormPost payoutForm
  defaultLayout $ do
    $(widgetFile "payout")

postPayoutR :: Handler Html
postPayoutR = do
  ((res, _), _) <- runFormPost payoutForm
  case res of
    FormSuccess payment -> do
      updateCashier (- (paymentAmount payment)) ("Auszahlung: " `T.append` paymentDesc payment)
      setMessage "Betrag ausgezahlt"
      redirect $ HomeR
    _ -> do
      setMessage "Auszahlung nicht möglich"
      redirect $ JournalR

payoutForm :: Form Payment
payoutForm = renderDivs $ Payment
  <$> areq currencyField "Betrag" Nothing
  <*> areq textField "Beschreibung" Nothing
