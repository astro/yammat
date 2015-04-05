module Handler.Buy where

import Import
import Handler.Common
import qualified Data.Text as T
import Text.Blaze.Internal

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
      setMessage "Benutzer oder Artikel unbekannt"
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
              case sw of
                False -> do
                  setMessage "Viel Vergnügen"
                  redirect $ HomeR
                True -> do
                  setMessage "Achtung: Guthaben im negativen Bereich"
                  redirect $ HomeR
            True -> do
              setMessage "So viele Artikel sind nicht vorhanden"
              redirect $ BuyR uId bId
        _ -> do
          setMessage "Etwas ist schief gelaufen"
          redirect $ HomeR
    Nothing -> do
      setMessage "Benutzer oder Artikel unbekannt"
      redirect $ HomeR

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
      setMessage "Getrank unbekannt"
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
              setMessage $ Content $ Text $ "Viel Vergnügen. Bitte Zahle "
                `T.append` (T.pack $ show ((fromIntegral price) / 100))
                `T.append` " "
                `T.append` (appCurrency $ appSettings master)
                `T.append` " in die Kasse ein"
              redirect $ HomeR
            True -> do
              setMessage "So viele Artikel sind nicht vorhanden"
              redirect $ BuyCashR bId
        _ -> do
          setMessage "Etwas ist schief gelaufen"
          redirect $ HomeR
    Nothing -> do
      setMessage "Artikel unbekannt"
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
  $ areq amountField "Anzahl" (Just 1)
