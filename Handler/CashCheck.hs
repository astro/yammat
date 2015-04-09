module Handler.CashCheck where

import Import
import Handler.Common

getCashCheckR :: Handler Html
getCashCheckR = do
  (cashCheckWidget, enctype) <- generateFormPost createCashierForm
  defaultLayout $ do
    $(widgetFile "cashCheck")

postCashCheckR :: Handler Html
postCashCheckR = do
  ((res, _), _) <- runFormPost createCashierForm
  case res of
    FormSuccess c -> do
      runDB $ insert_ c
      setMessageI MsgCashChecked
      redirect $ HomeR
    _ -> do
      setMessageI MsgCashCheckError
      redirect $ CashCheckR

createCashierForm :: Form Cashier
createCashierForm = renderDivs $ Cashier
  <$> areq currencyField (fieldSettingsLabel MsgCountedValue) Nothing
  <*> lift (liftIO getCurrentTime)
