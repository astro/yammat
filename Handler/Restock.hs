module Handler.Restock where

import Import
import Handler.Common

getRestockR :: Handler Html
getRestockR = do
  beverages <- runDB $ selectList [] [Desc BeverageIdent]
  defaultLayout $ do
    $(widgetFile "restock")

getUpstockR :: BeverageId -> Handler Html
getUpstockR bId = do
  mBev <- runDB $ get bId
  case mBev of
    Just bev -> do
      (upstockWidget, enctype) <- generateFormPost upstockForm
      defaultLayout $ do
        $(widgetFile "upstock")
    Nothing -> do
      setMessage "Artikel unbekannt"
      redirect $ HomeR

postUpstockR :: BeverageId -> Handler Html
postUpstockR bId = do
  mBev <- runDB $ get bId
  case mBev of
    Just bev -> do
      ((res, _), _) <- runFormPost upstockForm
      case res of
        FormSuccess c -> do
          case c > 0 of
            True -> do
              runDB $ update bId [BeverageAmount +=. c]
              setMessage "Bestand aufgefüllt"
              redirect $ HomeR
            False -> do
              setMessage "Bestand kann nicht negativ aufgefüllt werden"
              redirect $ UpstockR bId
        _ -> do
          setMessage "Fehler beim Auffüllen"
          redirect $ UpstockR bId
    Nothing -> do
      setMessage "Artikel unbekannt"
      redirect $ HomeR

upstockForm :: Form Int
upstockForm = renderDivs
  $ areq amountField "Anzahl hinzugefügt" (Just 0)

getNewArticleR :: Handler Html
getNewArticleR = do
  (newArticleWidget, enctype) <- generateFormPost newArticleForm
  defaultLayout $ do
    $(widgetFile "newArticle")

postNewArticleR :: Handler Html
postNewArticleR = do
  ((result, _), _) <- runFormPost newArticleForm
  case result of
    FormSuccess bev -> do
      runDB $ insert_ bev
      setMessage "Neuer Artikel hinzugefügt"
      redirect $ HomeR
    _ -> do
      setMessage "Fehler beim Hinzufügen"
      redirect $ HomeR

newArticleForm :: Form Beverage
newArticleForm = renderDivs $ Beverage
  <$> areq textField "Name" Nothing
  <*> areq currencyField "Preis" (Just 100)
  <*> areq amountField "Anzahl Elemente" (Just 0)
  <*> areq amountField "Warnung bei Anzahl" (Just 0)
