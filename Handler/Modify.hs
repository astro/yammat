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
      setMessageI MsgItemUnknown
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
            , BeverageAvatar =. beverageAvatar nBev
            ]
          setMessageI MsgEditSuccess
          redirect $ SummaryR
        _ -> do
          setMessageI MsgEditFail
          redirect $ SummaryR
    Nothing -> do
      setMessageI MsgItemUnknown
      redirect $ SummaryR

modifyForm :: Beverage -> Form Beverage
modifyForm bev = renderDivs $ Beverage
  <$> areq textField (fieldSettingsLabel MsgName) (Just $ beverageIdent bev)
  <*> areq currencyField (fieldSettingsLabel MsgPrice) (Just $ beveragePrice bev)
  <*> areq amountField (fieldSettingsLabel MsgCurrentStock) (Just $ beverageAmount bev)
  <*> areq amountField (fieldSettingsLabel MsgAnnouncedStock) (Just $ beverageAlertAmount bev)
  <*> aopt (selectField avatars) (fieldSettingsLabel MsgSelectAvatar) (Just $ beverageAvatar bev)
  where
    avatars = do
      ents <- runDB $ selectList [] [Asc AvatarIdent]
      optionsPairs $ map (\ent -> ((avatarIdent $ entityVal ent), entityKey ent)) ents

getDeleteBeverageR :: BeverageId -> Handler Html
getDeleteBeverageR bId = do
  mBev <- runDB $ get bId
  case mBev of
    Just bev -> do
      runDB $ delete bId
      setMessageI MsgItemDeleted
      redirect $ HomeR
    Nothing -> do
      setMessageI MsgItemUnknown
      redirect $ HomeR
