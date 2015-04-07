module Handler.NewUser where

import Import as I
import Text.Read

getNewUserR :: Handler Html
getNewUserR = do
  time <- liftIO getCurrentTime
  secs <- return $ read $ formatTime defaultTimeLocale "%s" time
  (newUserWidget, enctype) <- generateFormPost $ newUserForm secs
  defaultLayout $ do
    $(widgetFile "newUser")

postNewUserR :: Handler Html
postNewUserR = do
  time <- liftIO getCurrentTime
  secs <- return $ read $ formatTime defaultTimeLocale "%s" time
  ((res, _), _) <- runFormPost $ newUserForm secs
  case res of
    FormSuccess user -> do
      _ <- runDB $ insert user
      setMessage "Benutzer angelegt"
      redirect $ HomeR
    _ -> do
      setMessage "Benutzer konnte nicht angelegt werden"
      redirect $ NewUserR

newUserForm :: Int -> Form User
newUserForm secs = renderDivs $ User
  <$> areq textField "Nickname" Nothing
  <*> pure 0
  <*> pure secs
  <*> aopt emailField "E-mail" Nothing
  <*> areq boolField "Benachrichtigung bei Kauf" (Just False)

data UserConf = UserConf
  { userConfEmail :: Maybe Text
  , userConfNotify :: Bool
  }

getModifyUserR :: UserId -> Handler Html
getModifyUserR uId = do
  mUser <- runDB $ I.get uId
  case mUser of
    Just user -> do
      (modifyUserWidget, enctype) <- generateFormPost $ modifyUserForm user
      defaultLayout $ do
      $(widgetFile "modifyUser")
    Nothing -> do
      setMessage "Benutzer unbekannt"
      redirect $ HomeR

postModifyUserR :: UserId -> Handler Html
postModifyUserR uId = do
  mUser <- runDB $ I.get uId
  case mUser of
    Just user -> do
      ((res, _), _) <- runFormPost $ modifyUserForm user
      case res of
        FormSuccess conf -> do
          runDB $ update uId
            [ UserEmail =. userConfEmail conf
            , UserNotify =. userConfNotify conf
            ]
          setMessage "Nutzerdaten aktualisiert"
          redirect $ SelectR uId
        _ -> do
          setMessage "Nutzerdatenaktualisierung nicht erfolgreich"
          redirect $ SelectR uId
    Nothing -> do
      setMessage "Nutzer unbekannt"
      redirect $ HomeR

modifyUserForm :: User -> Form UserConf
modifyUserForm user = renderDivs $ UserConf
  <$> aopt emailField "E-Mail" (Just $ userEmail user)
  <*> areq boolField "Benachrichtigung bei Kauf" (Just $ userNotify user)
