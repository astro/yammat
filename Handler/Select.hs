module Handler.Select where

import Import
import Handler.Common
import qualified Text.Read as R
import qualified Data.Text as T

getSelectR :: UserId -> Handler Html
getSelectR uId = do
  mUser <- runDB $ get uId
  case mUser of
    Just user -> do
      master <- getYesod
      beverages <- runDB $ selectList [BeverageAmount >=. 0] [Desc BeverageIdent]
      defaultLayout $ do
        $(widgetFile "select")
    Nothing -> do
      setMessage "Benutzer unbekannt"
      redirect $ HomeR

getSelectCashR :: Handler Html
getSelectCashR = do
  beverages <- runDB $ selectList [BeverageAmount >=. 0] [Desc BeverageIdent]
  defaultLayout $ do
    $(widgetFile "selectCash")

getRechargeR :: UserId -> Handler Html
getRechargeR uId = do
  mUser <- runDB $ get uId
  case mUser of
    Just user -> do
      (rechargeWidget, enctype) <- generateFormPost rechargeForm
      defaultLayout $ do
        $(widgetFile "recharge")
    Nothing -> do
      setMessage "Benutzer unbekannt"
      redirect $ HomeR

postRechargeR :: UserId -> Handler Html
postRechargeR uId = do
  mUser <- runDB $ get uId
  case mUser of
    Just user -> do
      ((res, _), _) <- runFormPost rechargeForm
      case res of
        FormSuccess amount -> do
          updateCashier amount ("Guthaben: " `T.append` (userIdent user))
          time <- liftIO getCurrentTime
          secs <- return $ R.read $ formatTime defaultTimeLocale "%s" time
          runDB $ update uId [UserBalance +=. amount, UserTimestamp =. secs]
          setMessage "Guthaben erfolgreich aufgeladen"
          redirect $ HomeR
        _ -> do
          setMessage "Fehler beim Aufladen"
          redirect $ HomeR
    Nothing -> do
      setMessage "Benutzer unbekannt"
      redirect $ HomeR

rechargeForm :: Form Int
rechargeForm = renderDivs
  $ areq currencyField "Betrag" (Just 0)
