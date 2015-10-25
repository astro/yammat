--  yammat - Yet Another MateMAT
--  Copyright (C) 2015  Amedeo Molnár
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Affero General Public License as published
--  by the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Affero General Public License for more details.
--
--  You should have received a copy of the GNU Affero General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
      let bs = map (barcodeCode . entityVal) rawbs
      (modifyWidget, enctype) <- generateFormPost
        $ renderBootstrap3 BootstrapBasicForm
        $ modifyForm bev bs
      defaultLayout $
        $(widgetFile "modify")
    Nothing -> do
      setMessageI MsgItemUnknown
      redirect SummaryR

postModifyR :: BeverageId -> Handler Html
postModifyR bId = do
  mBev <- runDB $ get bId
  case mBev of
    Just bev -> do
      rawbs <- runDB $ selectList [BarcodeBev ==. Just bId] []
      let bs = map (barcodeCode . entityVal) rawbs
      ((res, _), _) <- runFormPost
        $ renderBootstrap3 BootstrapBasicForm
        $ modifyForm bev bs
      case res of
        FormSuccess nBev -> do
          runDB $ update bId
            [ BeverageIdent =. modBevIdent nBev
            , BeveragePrice =. modBevPrice nBev
            , BeverageAmount =. modBevAmount nBev
            , BeverageAlertAmount =. modBevAlertAmount nBev
            , BeverageCorrectedAmount +=. (modBevAmount nBev - beverageAmount bev)
            , BeverageMl =. modBevMl nBev
            , BeverageAvatar =. modBevAvatar nBev
            , BeverageSupplier =. modBevSupp nBev
            , BeverageMaxAmount =. modBevMaxAmount nBev
            , BeveragePerCrate =. modBevPC nBev
            , BeverageArtNr =. modBevArtNr nBev
            , BeveragePricePerCrate =. modBevPricePC nBev
            ]
          handleBarcodes (Right bId) (fromMaybe [] $ modBevBarcodes nBev)
          setMessageI MsgEditSuccess
          redirect SummaryR
        _ -> do
          setMessageI MsgEditFail
          redirect SummaryR
    Nothing -> do
      setMessageI MsgItemUnknown
      redirect SummaryR

data ModBev = ModBev
  { modBevIdent :: Text
  , modBevPrice :: Int
  , modBevAmount :: Int
  , modBevAlertAmount :: Int
  , modBevMaxAmount :: Int
  , modBevMl :: Int
  , modBevPC :: Maybe Int
  , modBevPricePC :: Maybe Int
  , modBevAvatar :: Maybe AvatarId
  , modBevBarcodes :: Maybe [Text]
  , modBevSupp :: Maybe SupplierId
  , modBevArtNr :: Maybe Text
  }

modifyForm :: Beverage -> [Text] -> AForm Handler ModBev
modifyForm bev bs = ModBev
  <$> areq textField (bfs MsgName) (Just $ beverageIdent bev)
  <*> areq currencyField (bfs MsgPrice) (Just $ beveragePrice bev)
  <*> areq amountField (bfs MsgCurrentStock) (Just $ beverageAmount bev)
  <*> areq amountField (bfs MsgAnnouncedStock) (Just $ beverageAlertAmount bev)
  <*> areq amountField (bfs MsgMaxAmount) (Just $ beverageMaxAmount bev)
  <*> areq volumeField (bfs MsgVolume) (Just $ beverageMl bev)
  <*> aopt amountField (bfs MsgAmountPerCrate) (Just $ beveragePerCrate bev)
  <*> aopt currencyField (bfs MsgPricePerCrate) (Just $ beveragePricePerCrate bev)
  <*> aopt (selectField avatars) (bfs MsgSelectAvatar) (Just $ beverageAvatar bev)
  <*> aopt barcodeField (bfs MsgBarcodeField) (Just $ Just bs)
  <*> aopt (selectField sups) (bfs MsgSelectSupplier) (Just $ beverageSupplier bev)
  <*> aopt textField (bfs MsgArtNr) (Just $ beverageArtNr bev)
  <*  bootstrapSubmit (msgToBSSubmit MsgSubmit)
  where
    avatars = optionsPersistKey [] [Asc AvatarIdent] avatarIdent
    sups = optionsPersistKey [] [Asc SupplierIdent] supplierIdent

getDeleteBeverageR :: BeverageId -> Handler Html
getDeleteBeverageR bId = do
  mBev <- runDB $ get bId
  case mBev of
    Just bev -> do
      runDB $ delete bId
      setMessageI MsgItemDeleted
      redirect HomeR
    Nothing -> do
      setMessageI MsgItemUnknown
      redirect HomeR
