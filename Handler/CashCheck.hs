module Handler.CashCheck where

import Import
import Handler.Common

getCashCheckR :: Handler Html
getCashCheckR = do
  (cashCheckWidget, enctype) <- generateFormPost createCashCheckForm
  defaultLayout $ do
    $(widgetFile "cashCheck")

postCashCheckR :: Handler Html
postCashCheckR = do
  ((res, _), _) <- runFormPost createCashCheckForm
  case res of
    FormSuccess c -> do
      currentTime <- liftIO getCurrentTime
      runDB $ insert_ c
      runDB $ insert_ $ Cashier (cashCheckBalance c) currentTime
      setMessageI MsgCashChecked
      redirect $ HomeR
    _ -> do
      setMessageI MsgCashCheckError
      redirect $ CashCheckR

createCashCheckForm :: Form CashCheck
createCashCheckForm = renderDivs $ CashCheck
  <$> areq currencyField (fieldSettingsLabel MsgCountedValue) Nothing
  <*> lift (liftIO getCurrentTime)
