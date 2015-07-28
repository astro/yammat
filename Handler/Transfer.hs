module Handler.Transfer where

import Import
import Handler.Common
import Data.Maybe
import Text.Shakespeare.Text (stext)

getTransferSelectR :: UserId -> Handler Html
getTransferSelectR from = do
  mUser <- runDB $ get from
  case mUser of
    Just user -> do
      users <- runDB $ selectList [UserId !=. from] [Asc UserIdent]
      defaultLayout $ do
        $(widgetFile "transferSelect")
    Nothing -> do
      setMessageI MsgUserUnknown
      redirect HomeR

getTransferR :: UserId -> UserId -> Handler Html
getTransferR from to = do
  mSender <- runDB $ get from
  case mSender of
    Just sender -> do
      mRecpt <- runDB $ get to
      case mRecpt of
        Just recpt -> do
          (transferWidget, enctype) <- generateFormPost transferForm
          currency <- appCurrency <$> appSettings <$> getYesod
          defaultLayout $ do
            $(widgetFile "transfer")
        Nothing -> do
          setMessageI MsgUserUnknown
          redirect $ TransferSelectR from
    Nothing -> do
      setMessageI MsgUserUnknown
      redirect HomeR

postTransferR :: UserId -> UserId -> Handler Html
postTransferR from to = do
  mSender <- runDB $ get from
  case mSender of
    Just sender -> do
      mRecpt <- runDB $ get to
      case mRecpt of
        Just recpt -> do
          ((res, _), _) <- runFormPost transferForm
          case res of
            FormSuccess amount -> do
              case amount < 0 of
                False -> do
                  runDB $ update from [UserBalance -=. amount]
                  runDB $ update to [UserBalance +=. amount]
                  master <- getYesod
                  liftIO $ notify sender recpt amount master
                  setMessageI MsgTransferComplete
                  redirect HomeR
                True -> do
                  setMessageI MsgNegativeTransfer
                  redirect $ TransferR from to
            _ -> do
              setMessageI MsgTransferError
              redirect HomeR
        Nothing -> do
          setMessageI MsgUserUnknown
          redirect $ TransferSelectR from
    Nothing -> do
      setMessageI MsgUserUnknown
      redirect HomeR

transferForm :: Form Int
transferForm = renderDivs
  $ areq currencyField (fieldSettingsLabel MsgValue) (Just 0)

notify :: User -> User -> Int -> App -> IO ()
notify sender recpt amount master = do
  case userEmail sender of
    Just email ->
      liftIO $ sendMail email "Guthabentransfer beim Matematen"
        [stext|
Hallo #{userIdent sender}

Du hast gerade #{formatIntCurrency amount}#{appCurrency $ appSettings master} an #{userIdent recpt} transferiert.

Viele Grüßem

Der Matemat
        |]
    Nothing ->
      return ()
