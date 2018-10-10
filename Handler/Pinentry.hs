module Handler.Pinentry where

import Import

import Handler.Common

import qualified Data.Text as T (pack)

getPinentryR :: UserId -> Handler Html
getPinentryR uId =
  isUser uId HomeR >>= (\user -> do
    case userPin user of
      Just pin -> do
        (pinWidget, enctype) <- generateFormPost
          $ renderBootstrap3 BootstrapBasicForm
          $ pinentryForm
        defaultLayout $ do
          [whamlet|
            <h3>_{MsgEnterPin}
            <form method="post" enctype=#{enctype}>
              ^{pinWidget}
            |]
      Nothing -> do
        setSession "pinentry" (T.pack $ show uId)
        redirect $ SelectR uId
    )

postPinentryR :: UserId -> Handler Html
postPinentryR uId = do
  isUser uId HomeR >>= (\user -> do
    case userPin user of
      Just pin -> do
        ((res, _), _) <- runFormPost
          $ renderBootstrap3 BootstrapBasicForm
          $ pinentryForm
        case res of
          FormSuccess ppin ->
            if ppin == pin
            then do
              setSession "pinentry" (T.pack $ show uId)
              redirect $ SelectR uId
            else do
              deleteSession "pinentry"
              setMessageI MsgWrongPin
              redirect HomeR
          _ -> do
            deleteSession "pinentry"
            setMessageI MsgPinFailure
            redirect HomeR
      Nothing -> do
        setSession "pinentry" (T.pack $ show uId)
        redirect $ SelectR uId
    )

pinentryForm :: AForm Handler Text
pinentryForm = areq passwordField (withAtofocus $ bfs MsgPIN) Nothing
 <* bootstrapSubmit (msgToBSSubmit MsgSubmit)
