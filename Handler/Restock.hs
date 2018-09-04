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
import Handler.Modify
import Data.Maybe (fromJust)

getRestockR :: Handler Html
getRestockR = do
  beverages <- runDB $ selectList [] [Asc BeverageIdent]
  defaultLayout $
    $(widgetFile "restock")

getUpstockR :: BeverageId -> Handler Html
getUpstockR bId =
  isBeverage bId RestockR >>= (\bev -> do
    (upstockWidget, enctype) <- generateFormPost
      $ renderBootstrap3 BootstrapBasicForm upstockForm
    defaultLayout $
      $(widgetFile "upstock")
  )

postUpstockR :: BeverageId -> Handler Html
postUpstockR bId =
  isBeverage bId RestockR >>= (\bev -> do
    ((res, _), _) <- runFormPost
      $ renderBootstrap3 BootstrapBasicForm upstockForm
    case res of
      FormSuccess c ->
        if upstockSingles c >= 0 && upstockCrates c >= 0
          then do
            let total = upstockSingles c + (upstockCrates c * (fromMaybe 0 $ beveragePerCrate bev))
            runDB $ update bId [BeverageAmount +=. total]
            setMessageI MsgStockedUp
            redirect RestockR
          else do
            setMessageI MsgNotStockedUp
            redirect $ UpstockR bId
      
      _ -> do
        setMessageI MsgStockupError
        redirect $ UpstockR bId
  )

data UpstockAmount = UpstockAmount
  { upstockSingles :: Int
  , upstockCrates  :: Int
  }

upstockForm :: AForm Handler UpstockAmount
upstockForm = UpstockAmount
  <$> areq amountField (bfs MsgAmountAdded) (Just 0)
  <*> areq amountField (bfs MsgCrateAmountAdded) (Just 0)
  <*  bootstrapSubmit (msgToBSSubmit MsgFillup)

getNewArticleR :: Handler Html
getNewArticleR = do
  (newArticleWidget, enctype) <- generateFormPost
    $ renderBootstrap3 BootstrapBasicForm $ modifyForm Nothing []
  defaultLayout $
    $(widgetFile "newArticle")

postNewArticleR :: Handler Html
postNewArticleR = do
  ((result, _), _) <- runFormPost
    $ renderBootstrap3 BootstrapBasicForm $ modifyForm Nothing []
  case result of
    FormSuccess nBev -> do
      bId <- runDB $ insert $ Beverage
        (modBevIdent nBev)
        (modBevPrice nBev)
        (modBevAmount nBev)
        (modBevAlertAmount nBev)
        0
        (modBevMl nBev)
        (modBevAvatar nBev)
        (modBevSupp nBev)
        (modBevMaxAmount nBev)
        0
        (modBevPC nBev)
        (modBevArtNr nBev)
        (modBevPricePC nBev)
      handleBarcodes (Right bId) (fromMaybe [] $ modBevBarcodes nBev)
      setMessageI MsgItemAdded
      redirect RestockR
    _ -> do
      setMessageI MsgItemNotAdded
      redirect RestockR
