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
module Handler.Select where

import Import
import Handler.Common
import qualified Text.Read as R
import qualified Data.Text as T
import Data.Maybe

getSelectR :: UserId -> Handler Html
getSelectR uId = do
  mUser <- runDB $ get uId
  case mUser of
    Just user -> do
      master <- getYesod
      beverages <- runDB $ selectList [BeverageAmount >. 0] [Asc BeverageIdent]
      defaultLayout $ do
        $(widgetFile "select")
    Nothing -> do
      setMessageI MsgUserUnknown
      redirect $ HomeR

getSelectCashR :: Handler Html
getSelectCashR = do
  beverages <- runDB $ selectList [BeverageAmount >. 0] [Asc BeverageIdent]
  defaultLayout $ do
    $(widgetFile "selectCash")

getRechargeR :: UserId -> Handler Html
getRechargeR uId = do
  mUser <- runDB $ get uId
  case mUser of
    Just user -> do
      (rechargeWidget, enctype) <- generateFormPost rechargeForm
      currency <- appCurrency <$> appSettings <$> getYesod
      defaultLayout $ do
        $(widgetFile "recharge")
    Nothing -> do
      setMessageI MsgUserUnknown
      redirect $ HomeR

postRechargeR :: UserId -> Handler Html
postRechargeR uId = do
  mUser <- runDB $ get uId
  case mUser of
    Just user -> do
      ((res, _), _) <- runFormPost rechargeForm
      case res of
        FormSuccess amount -> do
          case amount < 0 of
            False -> do
              updateCashier amount ("Guthaben: " `T.append` (userIdent user))
              time <- liftIO getCurrentTime
              secs <- return $ R.read $ formatTime defaultTimeLocale "%s" time
              runDB $ update uId [UserBalance +=. amount, UserTimestamp =. secs]
              setMessageI MsgRecharged
              redirect $ HomeR
            True -> do
              setMessageI MsgNegativeRecharge
              redirect $ RechargeR uId
        _ -> do
          setMessageI MsgRechargeError
          redirect $ HomeR
    Nothing -> do
      setMessageI MsgUserUnknown
      redirect $ HomeR

rechargeForm :: Form Int
rechargeForm = renderDivs
  $ areq currencyField (fieldSettingsLabel MsgValue) (Just 0)
