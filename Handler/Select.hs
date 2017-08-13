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
import qualified Data.Text as T
import Data.Maybe

getSelectR :: UserId -> Handler Html
getSelectR uId =
  isUser uId HomeR >>= (\user -> do
    master <- getYesod
    beverages <- runDB $ selectList [BeverageAmount >. 0] [Asc BeverageIdent]
    defaultLayout $ do
      addScript $ StaticR js_barcode_js
      $(widgetFile "select")
  )

getSelectCashR :: Handler Html
getSelectCashR = do
  beverages <- runDB $ selectList [BeverageAmount >. 0] [Asc BeverageIdent]
  defaultLayout $ do
    addScript $ StaticR js_barcode_js
    $(widgetFile "selectCash")

getRechargeR :: UserId -> Handler Html
getRechargeR uId =
  isUser uId HomeR >>= (\user -> do
    (rechargeWidget, enctype) <- generateFormPost
      $ renderBootstrap3 BootstrapBasicForm rechargeForm
    currency <- appCurrency <$> appSettings <$> getYesod
    defaultLayout $
      $(widgetFile "recharge")
  )

postRechargeR :: UserId -> Handler Html
postRechargeR uId =
  isUser uId HomeR >>= (\user -> do
    ((res, _), _) <- runFormPost
      $ renderBootstrap3 BootstrapBasicForm rechargeForm
    case res of
      FormSuccess amount ->
        if amount <= 0
          then do
            setMessageI MsgNegativeRecharge
            redirect $ RechargeR uId
          else do
            updateCashier amount ("Guthaben: " `T.append` userIdent user)
            today <- liftIO $ return . utctDay =<< getCurrentTime
            runDB $ update uId [UserBalance +=. amount, UserTimestamp =. today]
            setMessageI MsgRecharged
            redirect HomeR
      _ -> do
        setMessageI MsgRechargeError
        redirect HomeR
  )

rechargeForm :: AForm Handler Int
rechargeForm = areq currencyField (bfs MsgValue) (Just 0)
  <* bootstrapSubmit (msgToBSSubmit MsgRecharge)
