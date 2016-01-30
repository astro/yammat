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
module Handler.NewUser where

import Import as I
import Handler.Common
import Text.Read
import Text.Shakespeare.Text

getNewUserR :: Handler Html
getNewUserR = do
  time <- liftIO getCurrentTime
  let secs = read $ formatTime defaultTimeLocale "%s" time
  (newUserWidget, enctype) <- generateFormPost
    $ renderBootstrap3 BootstrapBasicForm
    $ newUserForm secs
  defaultLayout $
    $(widgetFile "newUser")

postNewUserR :: Handler Html
postNewUserR = do
  time <- liftIO getCurrentTime
  let secs = read $ formatTime defaultTimeLocale "%s" time
  ((res, _), _) <- runFormPost
    $ renderBootstrap3 BootstrapBasicForm
    $ newUserForm secs
  case res of
    FormSuccess user -> do
      namesakes <- runDB $ selectList [UserIdent ==. userIdent user] []
      if null namesakes
        then do
          runDB $ insert_ user
          setMessageI MsgUserCreated
          redirect HomeR
        else do
          setMessageI MsgUserIdentNotUnique
          redirect NewUserR
    _ -> do
      setMessageI MsgUserNotCreated
      redirect NewUserR

newUserForm :: Int -> AForm Handler User
newUserForm secs = User
  <$> areq textField (bfs MsgName) Nothing
  <*> pure 0
  <*> pure secs
  <*> aopt emailField (bfs MsgEmailNotify) Nothing
  <*> aopt (selectField avatars) (bfs MsgSelectAvatar) Nothing
  <*  bootstrapSubmit (msgToBSSubmit MsgSubmit)
  where
    avatars = do
      ents <- runDB $ selectList [] [Asc AvatarIdent]
      optionsPairs $ map (\ent -> ((avatarIdent $ entityVal ent), entityKey ent)) ents

data UserConf = UserConf
  { userConfIdent :: Text
  , userConfEmail :: Maybe Text
  , userConfAvatar :: Maybe AvatarId
  , userConfBarcode :: Maybe [Text]
  }

getModifyUserR :: UserId -> Handler Html
getModifyUserR uId = do
  mUser <- runDB $ I.get uId
  case mUser of
    Just user -> do
      p <- lookupGetParam "barcode"
      _ <- handleGetParam p (Left uId)
      rawbs <- runDB $ selectList [BarcodeUser ==. Just uId] []
      let bs = map (barcodeCode . entityVal) rawbs
      (modifyUserWidget, enctype) <- generateFormPost
        $ renderBootstrap3 BootstrapBasicForm
        $ modifyUserForm user bs
      defaultLayout $
        $(widgetFile "modifyUser")
    Nothing -> do
      setMessageI MsgUserUnknown
      redirect HomeR

postModifyUserR :: UserId -> Handler Html
postModifyUserR uId = do
  mUser <- runDB $ I.get uId
  case mUser of
    Just user -> do
      rawbs <- runDB $ selectList [BarcodeUser ==. Just uId] []
      let bs = map (barcodeCode . entityVal) rawbs
      ((res, _), _) <- runFormPost
        $ renderBootstrap3 BootstrapBasicForm
        $ modifyUserForm user bs
      case res of
        FormSuccess uc -> do
          namesakes <- runDB $ selectList [UserIdent ==. userConfIdent uc, UserId !=. uId] []
          if null namesakes
            then do
              runDB $ update uId
                [ UserIdent =. userConfIdent uc
                , UserEmail =. userConfEmail uc
                , UserAvatar =. userConfAvatar uc
                ]
              liftIO $ notify user (userConfEmail uc)
              handleBarcodes (Left uId) (fromMaybe [] $ userConfBarcode uc)
              setMessageI MsgUserEdited
              redirect $ SelectR uId
            else do
              setMessageI MsgUserIdentNotUnique
              redirect $ ModifyUserR uId
        _ -> do
          setMessageI MsgUserNotEdited
          redirect $ SelectR uId
    Nothing -> do
      setMessageI MsgUserUnknown
      redirect HomeR

modifyUserForm :: User -> [Text] -> AForm Handler UserConf
modifyUserForm user bs = UserConf
  <$> areq textField (bfs MsgName) (Just $ userIdent user)
  <*> aopt emailField (bfs MsgEmailNotify) (Just $ userEmail user)
  <*> aopt (selectField avatars) (bfs MsgSelectAvatar) (Just $ userAvatar user)
  <*> aopt barcodeField (bfs MsgBarcodeField) (Just $ Just bs)
  <*  bootstrapSubmit (msgToBSSubmit MsgSubmit)
  where
    avatars = do
      ents <- runDB $ selectList [] [Asc AvatarIdent]
      optionsPairs $ map (\ent -> ((avatarIdent $ entityVal ent), entityKey ent)) ents

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
