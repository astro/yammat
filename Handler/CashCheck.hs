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
      runDB $ insert_ c
      runDB $ insert_ $ Cashier (cashCheckBalance c)
      setMessage "Kassensturz durchgeführt. Kasse aktualisiert"
      redirect $ HomeR
    _ -> do
      setMessage "Fehler im Kassensturz"
      redirect $ CashCheckR

createCashCheckForm :: Form CashCheck
createCashCheckForm = renderDivs $ CashCheck
  <$> areq currencyField "Gezählter Betrag" Nothing
  <*> lift (liftIO getCurrentTime)
