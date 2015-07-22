module Handler.Home where

import Import
import qualified Text.Read as R
import Data.Maybe

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  beverages <- runDB $ selectList [BeverageAmount !=. 0] [Desc BeverageIdent]
  time <- liftIO getCurrentTime
  secs <- return $ (R.read $ formatTime defaultTimeLocale "%s" time) - 2592000
  users <- runDB $ selectList [UserTimestamp >=. secs] [Asc UserIdent]
  defaultLayout $ do
    $(widgetFile "home")

postHomeR :: Handler Html
postHomeR = do
  error "Not yet implemented"

getReactivateR :: Handler Html
getReactivateR = do
  time <- liftIO getCurrentTime
  secs <- return $ (R.read $ formatTime defaultTimeLocale "%s" time) - 2592000
  users <- runDB $ selectList [UserTimestamp <. secs] [Asc UserIdent]
  defaultLayout $ do
    $(widgetFile "reactivate")

getUserReactivateR :: UserId -> Handler Html
getUserReactivateR uId = do
  mUser <- runDB $ get uId
  case mUser of
    Just user -> do
      time <- liftIO getCurrentTime
      secs <- return $ R.read $ formatTime defaultTimeLocale "%s" time
      runDB $ update uId [UserTimestamp =. secs]
      setMessageI MsgUserReactivated
      redirect $ HomeR
    Nothing -> do
      setMessageI MsgUserUnknown
      redirect $ HomeR
