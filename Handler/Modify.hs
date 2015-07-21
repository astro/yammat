module Handler.Modify where

import Import
import Handler.Common

getModifyR :: BeverageId -> Handler Html
getModifyR bId = do
  mBev <- runDB $ get bId
  case mBev of
    Just bev -> do
      p <- lookupGetParam "barcode"
      _ <- handleGetParam p (Right bId)
      rawbs <- runDB $ selectList [BarcodeBev ==. Just bId] []
      bs <- return $ map (barcodeCode . entityVal) rawbs
      (modifyWidget, enctype) <- generateFormPost $ modifyForm bev bs
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
      rawbs <- runDB $ selectList [BarcodeBev ==. Just bId] []
      bs <- return $ map (barcodeCode . entityVal) rawbs
      ((res, _), _) <- runFormPost $ modifyForm bev bs
      case res of
        FormSuccess nBev -> do
          runDB $ update bId
            [ BeverageIdent =. modBevIdent nBev
            , BeveragePrice =. modBevPrice nBev
            , BeverageAmount =. modBevAmount nBev
            , BeverageAlertAmount =. modBevAlertAmount nBev
            , BeverageCorrectedAmount +=. ((modBevAmount nBev) - (beverageAmount bev))
            , BeverageMl =. modBevMl nBev
            , BeverageAvatar =. modBevAvatar nBev
            ]
          setMessageI MsgEditSuccess
          redirect $ SummaryR
        _ -> do
          setMessageI MsgEditFail
          redirect $ SummaryR
    Nothing -> do
      setMessageI MsgItemUnknown
      redirect $ SummaryR

data ModBev = ModBev
  { modBevIdent :: Text
  , modBevPrice :: Int
  , modBevAmount :: Int
  , modBevAlertAmount :: Int
  , modBevMl :: Int
  , modBevAvatar :: Maybe AvatarId
  , modBevBarcodes :: Maybe [Text]
  }

modifyForm :: Beverage -> [Text] -> Form ModBev
modifyForm bev bs = renderDivs $ ModBev
  <$> areq textField (fieldSettingsLabel MsgName) (Just $ beverageIdent bev)
  <*> areq currencyField (fieldSettingsLabel MsgPrice) (Just $ beveragePrice bev)
  <*> areq amountField (fieldSettingsLabel MsgCurrentStock) (Just $ beverageAmount bev)
  <*> areq amountField (fieldSettingsLabel MsgAnnouncedStock) (Just $ beverageAlertAmount bev)
  <*> areq volumeField (fieldSettingsLabel MsgVolume) (Just $ beverageMl bev)
  <*> aopt (selectField avatars) (fieldSettingsLabel MsgSelectAvatar) (Just $ beverageAvatar bev)
  <*> aopt barcodeField (fieldSettingsLabel MsgBarcodeField) (Just $ Just bs)
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
