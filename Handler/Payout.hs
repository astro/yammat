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
      msg <- renderMessage' $ MsgPayout $ paymentDesc payment
      updateCashier (- (paymentAmount payment)) msg
      setMessageI MsgPaidOut
      redirect $ HomeR
    _ -> do
      setMessageI MsgNotPaidOut
      redirect $ JournalR

payoutForm :: Form Payment
payoutForm = renderDivs $ Payment
  <$> areq currencyField (fieldSettingsLabel MsgValue) Nothing
  <*> areq textField (fieldSettingsLabel MsgDescription) Nothing
