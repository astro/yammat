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
module Handler.Buy where

import Import
import Handler.Common
import Text.Shakespeare.Text

getBuyR :: UserId -> BeverageId -> Handler Html
getBuyR uId bId = do
  (_, bev) <- checkData uId bId
  master <- getYesod
  (buyWidget, enctype) <- generateFormPost
    $ renderBootstrap3 BootstrapBasicForm
    $ buyForm
  defaultLayout $
    $(widgetFile "buy")

postBuyR :: UserId -> BeverageId -> Handler Html
postBuyR uId bId = do
  (user, bev) <- checkData uId bId
  ((res, _), _) <- runFormPost
    $ renderBootstrap3 BootstrapBasicForm
    $ buyForm
  case res of
    FormSuccess quant -> do
      if quant > beverageAmount bev
        then do
          setMessageI MsgNotEnoughItems
          redirect $ BuyR uId bId
        else do
          let price = quant * (beveragePrice bev)
          let sw = price > (userBalance user)
          today <- liftIO $ return . utctDay =<< getCurrentTime
          runDB $ do
            update uId [UserTimestamp =. today]
            update uId [UserBalance -=. price]
            update bId [BeverageAmount -=. quant]
          checkAlert bId
          master <- getYesod
          liftIO $ notifyUser user bev quant price master
          case sw of
            False -> do
              setMessageI MsgPurchaseSuccess
              redirect HomeR
            True -> do
              let level = case userBalance user - price of
                    balance
                      | balance <= -5000 -> 3
                      | balance <= -1000 -> 2
                      | otherwise -> 1
              redirect $ DemandR level
    _ -> do
      setMessageI MsgErrorOccured
      redirect HomeR

notifyUser :: User -> Beverage -> Int -> Int -> App -> IO ()
notifyUser user bev quant price master = do
  case userEmail user of
    Just email -> do
      addendum <- if (userBalance user - price) < 0
        then
          return $
            "\n\nDein Guthaben Beträgt im Moment " ++
            formatIntCurrency (userBalance user - price) ++
            appCurrency (appSettings master) ++
            ".\n" ++
            "LADE DEIN GUTHABEN AUF!\n" ++
            "VERDAMMT NOCHMAL!!!"
        else
          return ""
      liftIO $ sendMail email "Einkauf beim Matematen"
        [lt|
Hallo #{userIdent user},

Du hast gerade beim Matematen für #{formatIntCurrency price}#{appCurrency $ appSettings master} #{quant} Stück #{beverageIdent bev} eingekauft.#{addendum}

Viele Grüsse,

Dein Matemat
        |]
    Nothing ->
      return ()

getBuyCashR :: BeverageId -> Handler Html
getBuyCashR bId = do
  mBev <- runDB $ get bId
  case mBev of
    Just bev -> do
      master <- getYesod
      (buyCashWidget, enctype) <- generateFormPost
        $ renderBootstrap3 BootstrapBasicForm
        $ buyForm
      defaultLayout $
        $(widgetFile "buyCash")
    Nothing -> do
      setMessageI MsgItemUnknown
      redirect HomeR

postBuyCashR :: BeverageId -> Handler Html
postBuyCashR bId =
  isBeverage bId HomeR >>= (\bev -> do
    ((res, _), _) <- runFormPost
      $ renderBootstrap3 BootstrapBasicForm
      $ buyForm
    case res of
      FormSuccess quant -> do
        if quant > beverageAmount bev
          then do
            setMessageI MsgNotEnoughItems
            redirect $ BuyCashR bId
          else do
            master <- getYesod
            let price = quant * (beveragePrice bev + appCashCharge (appSettings master))
            runDB $ update bId [BeverageAmount -=. quant]
            updateCashier price "Barzahlung"
            checkAlert bId
            let currency = appCurrency $ appSettings master
            setMessageI $ MsgPurchaseSuccessCash price currency
            redirect HomeR
      _ -> do
        setMessageI MsgItemDisappeared
        redirect HomeR
  )

checkData :: UserId -> BeverageId -> Handler (User, Beverage)
checkData uId bId =
  isUser uId HomeR >>= (\user -> do
    isBeverage bId HomeR >>= (\bev ->
      return (user, bev)
      )
  )

buyForm :: AForm Handler Int
buyForm = areq amountField (bfs MsgAmount) (Just 1)
  <* bootstrapSubmit (msgToBSSubmit MsgPurchase)
