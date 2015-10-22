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
module Handler.Payout where

import Import
import Handler.Common
import qualified Data.Text as T

data Payment = Payment
  { paymentAmount :: Int
  , paymentDesc :: T.Text
  }

getPayoutR :: Handler Html
getPayoutR = do
  (payoutWidget, enctype) <- generateFormPost
    $ renderBootstrap3 BootstrapBasicForm payoutForm
  defaultLayout $
    $(widgetFile "payout")

postPayoutR :: Handler Html
postPayoutR = do
  ((res, _), _) <- runFormPost
    $ renderBootstrap3 BootstrapBasicForm payoutForm
  case res of
    FormSuccess payment -> do
      msg <- renderMessage' $ MsgPayout $ paymentDesc payment
      updateCashier (- (paymentAmount payment)) msg
      setMessageI MsgPaidOut
      redirect HomeR
    _ -> do
      setMessageI MsgNotPaidOut
      redirect JournalR

payoutForm :: AForm Handler Payment
payoutForm = Payment
  <$> areq currencyField (bfs MsgValue) Nothing
  <*> areq textField (bfs MsgDescription) Nothing
  <*  bootstrapSubmit (msgToBSSubmit MsgDoPayout)
