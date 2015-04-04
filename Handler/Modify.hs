module Handler.Modify where

import Import
import Handler.Common

getModifyR :: BeverageId -> Handler Html
getModifyR bId = do
  mBev <- runDB $ get bId
  case mBev of
    Just bev -> do
      (modifyWidget, enctype) <- generateFormPost $ modifyForm bev
      defaultLayout $ do
        $(widgetFile "modify")
    Nothing -> do
      setMessage "Artikel unbekannt"
      redirect $ SummaryR

postModifyR :: BeverageId -> Handler Html
postModifyR bId = do
  mBev <- runDB $ get bId
  case mBev of
    Just bev -> do
      ((res, _), _) <- runFormPost $ modifyForm bev
      case res of
        FormSuccess nBev -> do
          runDB $ update bId
            [ BeverageIdent =. beverageIdent nBev
            , BeveragePrice =. beveragePrice nBev
            , BeverageAmount =. beverageAmount nBev
            , BeverageAlertAmount =. beverageAlertAmount nBev
            ]
          setMessage "Bearbeitung erfolgreich"
          redirect $ SummaryR
        _ -> do
          setMessage "Bearbeitung nicht möglich"
          redirect $ SummaryR
    Nothing -> do
      setMessage "Artikel unbekannt"
      redirect $ SummaryR

modifyForm :: Beverage -> Form Beverage
modifyForm bev = renderDivs $ Beverage
  <$> areq textField "Name" (Just $ beverageIdent bev)
  <*> areq currencyField "Preis" (Just $ beveragePrice bev)
  <*> areq amountField "aktueller Bestand" (Just $ beverageAmount bev)
  <*> areq amountField "Meldebestand" (Just $ beverageAlertAmount bev)

getDeleteBeverageR :: BeverageId -> Handler Html
getDeleteBeverageR bId = do
  mBev <- runDB $ get bId
  case mBev of
    Just bev -> do
      runDB $ delete bId
      setMessage "Artikel gelöscht"
      redirect $ HomeR
    Nothing -> do
      setMessage "Artikel unbekannt"
      redirect $ HomeR
