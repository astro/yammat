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
module Handler.Restock where

import Import
import Handler.Common
import Data.Maybe (fromJust)

getRestockR :: Handler Html
getRestockR = do
  beverages <- runDB $ selectList [] [Asc BeverageIdent]
  defaultLayout $
    $(widgetFile "restock")

getUpstockR :: BeverageId -> Handler Html
getUpstockR bId = do
  mBev <- runDB $ get bId
  case mBev of
    Just bev -> do
      (upstockWidget, enctype) <- generateFormPost upstockForm
      defaultLayout $
        $(widgetFile "upstock")
    Nothing -> do
      setMessageI MsgItemUnknown
      redirect HomeR

postUpstockR :: BeverageId -> Handler Html
postUpstockR bId = do
  mBev <- runDB $ get bId
  case mBev of
    Just _ -> do
      ((res, _), _) <- runFormPost upstockForm
      case res of
        FormSuccess c ->
          if c > 0
            then do
              runDB $ update bId [BeverageAmount +=. c]
              setMessageI MsgStockedUp
              redirect HomeR
            else do
              setMessageI MsgNotStockedUp
              redirect $ UpstockR bId
        _ -> do
          setMessageI MsgStockupError
          redirect $ UpstockR bId
    Nothing -> do
      setMessageI MsgItemUnknown
      redirect HomeR

upstockForm :: Form Int
upstockForm = renderDivs
  $ areq amountField (fieldSettingsLabel MsgAmountAdded) (Just 1)

getNewArticleR :: Handler Html
getNewArticleR = do
  (newArticleWidget, enctype) <- generateFormPost newArticleForm
  defaultLayout $
    $(widgetFile "newArticle")

postNewArticleR :: Handler Html
postNewArticleR = do
  ((result, _), _) <- runFormPost newArticleForm
  case result of
    FormSuccess bev -> do
      runDB $ insert_ bev
      setMessageI MsgItemAdded
      redirect HomeR
    _ -> do
      setMessageI MsgItemNotAdded
      redirect HomeR

newArticleForm :: Form Beverage
newArticleForm = renderDivs $ (\a b c d e f g h i j k-> Beverage a b c d g h i j e f k)
  <$> areq textField (fieldSettingsLabel MsgName) Nothing
  <*> areq currencyField (fieldSettingsLabel MsgPrice) (Just 100)
  <*> areq amountField (fieldSettingsLabel MsgAmount) (Just 0)
  <*> areq amountField (fieldSettingsLabel MsgAmountWarning) (Just 0)
  <*> areq amountField (fieldSettingsLabel MsgMaxAmount) (Just 200)
  <*> aopt amountField (fieldSettingsLabel MsgAmountPerCrate) (Just $ Just 20)
  <*> pure 0
  <*> areq volumeField (fieldSettingsLabel MsgVolume) (Just 500)
  <*> aopt (selectField avatars) (fieldSettingsLabel MsgSelectAvatar) Nothing
  <*> aopt (selectField sups) (fieldSettingsLabel MsgSelectSupplier) Nothing
  <*> aopt textField (fieldSettingsLabel MsgArtNr) Nothing
  <*> aopt currencyField (fieldSettingsLabel MsgPricePerCrate) Nothing
  where
    avatars = optionsPersistKey [] [Asc AvatarIdent] avatarIdent
    sups = optionsPersistKey [] [Asc SupplierIdent] supplierIdent
