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
      (upstockWidget, enctype) <- generateFormPost
        $ renderBootstrap3 BootstrapBasicForm upstockForm
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
      ((res, _), _) <- runFormPost
        $ renderBootstrap3 BootstrapBasicForm upstockForm
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

upstockForm :: AForm Handler Int
upstockForm = areq amountField (bfs MsgAmountAdded) (Just 1)
  <* bootstrapSubmit (msgToBSSubmit MsgFillup)

getNewArticleR :: Handler Html
getNewArticleR = do
  (newArticleWidget, enctype) <- generateFormPost
    $ renderBootstrap3 BootstrapBasicForm newArticleForm
  defaultLayout $
    $(widgetFile "newArticle")

postNewArticleR :: Handler Html
postNewArticleR = do
  ((result, _), _) <- runFormPost
    $ renderBootstrap3 BootstrapBasicForm newArticleForm
  case result of
    FormSuccess bev -> do
      runDB $ insert_ bev
      setMessageI MsgItemAdded
      redirect HomeR
    _ -> do
      setMessageI MsgItemNotAdded
      redirect HomeR

newArticleForm :: AForm Handler Beverage
newArticleForm = (\a b c d e f g h i j k l -> Beverage a b c d e i j k f g l h)
  <$> areq textField (bfs MsgName) Nothing
  <*> areq currencyField (bfs MsgPrice) (Just 100)
  <*> areq amountField (bfs MsgAmount) (Just 0)
  <*> areq amountField (bfs MsgAmountWarning) (Just 0)
  <*> pure 0
  <*> areq amountField (bfs MsgMaxAmount) (Just 200)
  <*> aopt amountField (bfs MsgAmountPerCrate) (Just $ Just 20)
  <*> aopt currencyField (bfs MsgPricePerCrate) Nothing
  <*> areq volumeField (bfs MsgVolume) (Just 500)
  <*> aopt (selectField avatars) (bfs MsgSelectAvatar) Nothing
  <*> aopt (selectField sups) (bfs MsgSelectSupplier) Nothing
  <*> aopt textField (bfs MsgArtNr) Nothing
  <*  bootstrapSubmit (msgToBSSubmit MsgSubmit)
  where
    avatars = optionsPersistKey [] [Asc AvatarIdent] avatarIdent
    sups = optionsPersistKey [] [Asc SupplierIdent] supplierIdent
