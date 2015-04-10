module Handler.Buy where

import Import
import Handler.Common
import qualified Data.Text as T
import Text.Blaze.Internal
import Text.Shakespeare.Text

getBuyR :: UserId -> BeverageId -> Handler Html
getBuyR uId bId = do
  mTup <- checkData uId bId
  case mTup of
    Just (user, bev) -> do
      master <- getYesod
      (buyWidget, enctype) <- generateFormPost buyForm
      defaultLayout $ do
        $(widgetFile "buy")
    Nothing -> do
      setMessageI MsgUserOrArticleUnknown
      redirect $ HomeR

postBuyR :: UserId -> BeverageId -> Handler Html
postBuyR uId bId = do
  mTup <- checkData uId bId
  case mTup of
    Just (user, bev) -> do
      ((res, _), _) <- runFormPost buyForm
      case res of
        FormSuccess quant -> do
          case quant > beverageAmount bev of
            False -> do
              price <- return $ quant * (beveragePrice bev)
              sw <- return $ price > (userBalance user)
              runDB $ update uId [UserBalance -=. price]
              runDB $ update bId [BeverageAmount -=. quant]
              checkAlert bId
              master <- getYesod
              liftIO $ notifyUser user bev price master
              case sw of
                False -> do
                  setMessageI MsgPurchaseSuccess
                  redirect $ HomeR
                True -> do
                  setMessageI MsgPurchaseDebtful
                  redirect $ HomeR
            True -> do
              setMessageI MsgNotEnoughItems
              redirect $ BuyR uId bId
        _ -> do
          setMessageI MsgErrorOccured
          redirect $ HomeR
    Nothing -> do
      setMessageI MsgUserUnknown
      redirect $ HomeR

notifyUser :: User -> Beverage -> Int -> App -> IO ()
notifyUser user bev price master = do
  case userEmail user of
    Just email ->
      liftIO $ sendMail email "Einkauf beim Matematen"
        [stext|
Hallo #{userIdent user},

Du hast gerade beim Matematen für #{formatIntCurrency price}#{appCurrency $ appSettings master} #{beverageIdent bev} eingekauft.

Viele Grüsse,

Der Matemat
        |]
    Nothing ->
      return ()

getBuyCashR :: BeverageId -> Handler Html
getBuyCashR bId = do
  mBev <- runDB $ get bId
  case mBev of
    Just bev -> do
      master <- getYesod
      (buyCashWidget, enctype) <- generateFormPost buyForm
      defaultLayout $ do
        $(widgetFile "buyCash")
    Nothing -> do
      setMessageI MsgItemUnknown
      redirect $ HomeR

postBuyCashR :: BeverageId -> Handler Html
postBuyCashR bId = do
  mBev <- runDB $ get bId
  case mBev of
    Just bev -> do
      ((res, _), _) <- runFormPost buyForm
      case res of
        FormSuccess quant -> do
          case quant > beverageAmount bev of
            False -> do
              master <- getYesod
              price <- return $ quant * (beveragePrice bev + 50)
              runDB $ update bId [BeverageAmount -=. quant]
              updateCashier price "Barzahlung"
              checkAlert bId
              let currency = appCurrency $ appSettings master
              setMessageI $ MsgPurchaseSuccessCash price currency
              redirect HomeR
            True -> do
              setMessageI MsgNotEnoughItems
              redirect $ BuyCashR bId
        _ -> do
          setMessageI MsgItemDisappeared
          redirect $ HomeR
    Nothing -> do
      setMessageI MsgItemUnknown
      redirect $ HomeR

checkData :: UserId -> BeverageId -> Handler (Maybe (User, Beverage))
checkData uId bId = do
  mUser <- runDB $ get uId
  mBev <- runDB $ get bId
  case mUser of
    Just user -> do
      case mBev of
        Just bev -> return $ Just (user, bev)
        Nothing -> return Nothing
    Nothing -> return Nothing

buyForm :: Form Int
buyForm = renderDivs
  $ areq amountField (fieldSettingsLabel MsgAmount) (Just 1)
