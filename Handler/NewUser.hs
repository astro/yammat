module Handler.NewUser where

import Import as I
import Handler.Common
import Text.Read
import Text.Shakespeare.Text

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
      setMessageI MsgUserCreated
      redirect $ HomeR
    _ -> do
      setMessageI MsgUserNotCreated
      redirect $ NewUserR

newUserForm :: Int -> Form User
newUserForm secs = renderDivs $ User
  <$> areq textField (fieldSettingsLabel MsgName) Nothing
  <*> pure 0
  <*> pure secs
  <*> aopt emailField (fieldSettingsLabel MsgEmailNotify) Nothing
  <*> aopt (selectField avatars) (fieldSettingsLabel MsgSelectAvatar) Nothing
  where
    avatars = do
      ents <- runDB $ selectList [] [Asc AvatarIdent]
      optionsPairs $ map (\ent -> ((avatarIdent $ entityVal ent), entityKey ent)) ents

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
      setMessageI MsgUserUnknown
      redirect $ HomeR

postModifyUserR :: UserId -> Handler Html
postModifyUserR uId = do
  mUser <- runDB $ I.get uId
  case mUser of
    Just user -> do
      ((res, _), _) <- runFormPost $ modifyUserForm user
      case res of
        FormSuccess email -> do
          runDB $ update uId
            [ UserEmail =. email
            ]
          liftIO $ notify user email
          setMessageI MsgUserEdited
          redirect $ SelectR uId
        _ -> do
          setMessageI MsgUserNotEdited
          redirect $ SelectR uId
    Nothing -> do
      setMessageI MsgUserUnknown
      redirect $ HomeR

modifyUserForm :: User -> Form (Maybe Text)
modifyUserForm user = renderDivs $
  aopt emailField (fieldSettingsLabel MsgEmailNotify) (Just $ userEmail user)

notify :: User -> Maybe Text -> IO ()
notify user email
  | (userEmail user) == email = return ()
  | otherwise = case userEmail user of
    Just address -> sendMail address "Profiländerung"
      [stext|
Hallo #{userIdent user},

deine Profileinstellungen wurden geändert.
Nur damit du Bescheid weißt.

Grüße,

der Matemat
      |]
    Nothing -> return ()
