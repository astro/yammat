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
module Handler.CashCheck where

import Import
import Handler.Common

getCashCheckR :: Handler Html
getCashCheckR = do
  (cashCheckWidget, enctype) <- generateFormPost
    $ renderBootstrap3 BootstrapBasicForm createCashCheckForm
  defaultLayout $
    $(widgetFile "cashCheck")

postCashCheckR :: Handler Html
postCashCheckR = do
  ((res, _), _) <- runFormPost
    $ renderBootstrap3 BootstrapBasicForm createCashCheckForm
  case res of
    FormSuccess c -> do
      currentTime <- liftIO getCurrentTime
      runDB $ insert_ c
      runDB $ insert_ $ Cashier (cashCheckBalance c) currentTime
      setMessageI MsgCashChecked
      redirect HomeR
    _ -> do
      setMessageI MsgCashCheckError
      redirect CashCheckR

createCashCheckForm :: AForm Handler CashCheck
createCashCheckForm = CashCheck
  <$> areq currencyField (bfs MsgCountedValue) Nothing
  <*> lift (liftIO getCurrentTime)
  <*  bootstrapSubmit (msgToBSSubmit MsgSubmit)
