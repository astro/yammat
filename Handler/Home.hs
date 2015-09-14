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
  let secs = R.read (formatTime defaultTimeLocale "%s" time) - 2592000
  users <- runDB $ selectList [UserTimestamp >=. secs] [Asc UserIdent]
  defaultLayout $
    $(widgetFile "home")

getReactivateR :: Handler Html
getReactivateR = do
  time <- liftIO getCurrentTime
  let secs = R.read (formatTime defaultTimeLocale "%s" time) - 2592000
  users <- runDB $ selectList [UserTimestamp <. secs] [Asc UserIdent]
  defaultLayout $
    $(widgetFile "reactivate")

getUserReactivateR :: UserId -> Handler Html
getUserReactivateR uId = do
  mUser <- runDB $ get uId
  case mUser of
    Just user -> do
      time <- liftIO getCurrentTime
      let secs = R.read $ formatTime defaultTimeLocale "%s" time
      runDB $ update uId [UserTimestamp =. secs]
      setMessageI MsgUserReactivated
      redirect HomeR
    Nothing -> do
      setMessageI MsgUserUnknown
      redirect HomeR
