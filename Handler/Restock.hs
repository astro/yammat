module Handler.Restock where

import Import
import Handler.Common

getRestockR :: Handler Html
getRestockR = do
  beverages <- runDB $ selectList [] [Asc BeverageIdent]
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
      setMessageI MsgItemUnknown
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
              setMessageI MsgStockedUp
              redirect $ HomeR
            False -> do
              setMessageI MsgNotStockedUp
              redirect $ UpstockR bId
        _ -> do
          setMessageI MsgStockupError
          redirect $ UpstockR bId
    Nothing -> do
      setMessageI MsgItemUnknown
      redirect $ HomeR

upstockForm :: Form Int
upstockForm = renderDivs
  $ areq amountField (fieldSettingsLabel MsgAmountAdded) (Just 1)

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
      setMessageI MsgItemAdded
      redirect $ HomeR
    _ -> do
      setMessageI MsgItemNotAdded
      redirect $ HomeR

newArticleForm :: Form Beverage
newArticleForm = renderDivs $ Beverage
  <$> areq textField (fieldSettingsLabel MsgName) Nothing
  <*> areq currencyField (fieldSettingsLabel MsgPrice) (Just 100)
  <*> areq amountField (fieldSettingsLabel MsgAmount) (Just 0)
  <*> areq amountField (fieldSettingsLabel MsgAmountWarning) (Just 0)
  <*> aopt (selectField albums) (fieldSettingsLabel MsgSelectAvatar) Nothing
  where
    albums = do
      ents <- runDB $ selectList [] [Asc AvatarIdent]
      optionsPairs $ map (\ent -> ((avatarIdent $ entityVal ent), entityKey ent)) ents
