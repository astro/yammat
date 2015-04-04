module Handler.NewUser where

import Import
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
