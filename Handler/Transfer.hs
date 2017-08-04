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
module Handler.Transfer where

import Import
import Handler.Common
import Data.Maybe
import Text.Shakespeare.Text (stext)

getTransferSelectR :: UserId -> Handler Html
getTransferSelectR from =
  isUser from HomeR >>= (\_ -> do
    users <- runDB $ selectList [UserId !=. from] [Asc UserIdent]
    defaultLayout $
      $(widgetFile "transferSelect")
  )

getTransferR :: UserId -> UserId -> Handler Html
getTransferR from to =
  isUser from HomeR >>= (\sender ->
    isUser to (TransferSelectR from) >>= (\recpt -> do
      (transferWidget, enctype) <- generateFormPost
        $ renderBootstrap3 BootstrapBasicForm transferForm
      currency <- appCurrency <$> appSettings <$> getYesod
      defaultLayout $ do
        $(widgetFile "transfer")
      )
  )

postTransferR :: UserId -> UserId -> Handler Html
postTransferR from to =
  isUser from HomeR >>= (\sender ->
    isUser to (TransferSelectR from) >>= (\recpt -> do
      ((res, _), _) <- runFormPost
        $ renderBootstrap3 BootstrapBasicForm transferForm
      case res of
        FormSuccess amount
          | amount < 0 -> do
              setMessageI MsgNegativeTransfer
              redirect $ TransferR from to
          | userBalance sender < amount -> do
              setMessageI MsgNotEnoughFunds
              redirect HomeR
          | otherwise -> do
              runDB $ update from [UserBalance -=. amount]
              runDB $ update to [UserBalance +=. amount]
              master <- getYesod
              liftIO $ notify sender recpt amount master
              setMessageI MsgTransferComplete
              redirect HomeR
        _ -> do
          setMessageI MsgTransferError
          redirect HomeR
      )
  )

transferForm :: AForm Handler Int
transferForm = areq currencyField (bfs MsgValue) (Just 0)
  <* bootstrapSubmit (msgToBSSubmit MsgTransfer)

notify :: User -> User -> Int -> App -> IO ()
notify sender rcpt amount master = do
  when (isJust $ userEmail sender) $
    liftIO $ sendMail (fromJust $ userEmail sender) "Guthabentransfer beim Matematen"
      [stext|
Hallo #{userIdent sender}

Du hast gerade #{formatIntCurrency amount}#{appCurrency $ appSettings master} an #{userIdent rcpt} transferiert.

Viele Grüße,

Dein Matemat
      |]
  when (isJust $ userEmail rcpt) $
    liftIO $ sendMail (fromJust $ userEmail rcpt) "Guthabentransfer eingetroffen"
      [stext|
Hallo #{userIdent rcpt}

Du hast gerade #{formatIntCurrency amount}#{appCurrency $ appSettings master} von #{userIdent sender} erhalten.

Viele Grüße,

Dein Matemat
      |]
