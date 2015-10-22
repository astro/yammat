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
      defaultLayout $
        $(widgetFile "select")
    Nothing -> do
      setMessageI MsgUserUnknown
      redirect HomeR

getSelectCashR :: Handler Html
getSelectCashR = do
  beverages <- runDB $ selectList [BeverageAmount >. 0] [Asc BeverageIdent]
  defaultLayout $
    $(widgetFile "selectCash")

getRechargeR :: UserId -> Handler Html
getRechargeR uId = do
  mUser <- runDB $ get uId
  case mUser of
    Just user -> do
      (rechargeWidget, enctype) <- generateFormPost
        $ renderBootstrap3 BootstrapBasicForm rechargeForm
      currency <- appCurrency <$> appSettings <$> getYesod
      defaultLayout $
        $(widgetFile "recharge")
    Nothing -> do
      setMessageI MsgUserUnknown
      redirect HomeR

postRechargeR :: UserId -> Handler Html
postRechargeR uId = do
  mUser <- runDB $ get uId
  case mUser of
    Just user -> do
      ((res, _), _) <- runFormPost
        $ renderBootstrap3 BootstrapBasicForm rechargeForm
      case res of
        FormSuccess amount ->
          if amount < 0
            then do
              setMessageI MsgNegativeRecharge
              redirect $ RechargeR uId
            else do
              updateCashier amount ("Guthaben: " `T.append` userIdent user)
              time <- liftIO getCurrentTime
              let secs = R.read $ formatTime defaultTimeLocale "%s" time
              runDB $ update uId [UserBalance +=. amount, UserTimestamp =. secs]
              setMessageI MsgRecharged
              redirect HomeR
        _ -> do
          setMessageI MsgRechargeError
          redirect HomeR
    Nothing -> do
      setMessageI MsgUserUnknown
      redirect HomeR

rechargeForm :: AForm Handler Int
rechargeForm = areq currencyField (bfs MsgValue) (Just 0)
  <* bootstrapSubmit (msgToBSSubmit MsgRecharge)
