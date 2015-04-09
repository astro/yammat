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
      setMessage "Kassensturz durchgeführt. Kasse aktualisiert"
      redirect $ HomeR
    _ -> do
      setMessage "Fehler im Kassensturz"
      redirect $ CashCheckR

createCashierForm :: Form Cashier
createCashierForm = renderDivs $ Cashier
  <$> areq currencyField "Gezählter Betrag" Nothing
  <*> lift (liftIO getCurrentTime)
