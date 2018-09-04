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
import Data.Maybe
import Data.Time.Calendar (addDays)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  settings <- getsYesod appSettings
  beverages <- runDB $ selectList [BeverageAmount !=. 0] [Desc BeverageIdent]
  today <- liftIO $ utctDay <$> getCurrentTime
  users <- runDB $ selectList [UserTimestamp >=. addDays (-30) today] [Asc UserIdent]
  ousers <- runDB $ selectList [UserTimestamp <. addDays (-30) today] [Asc UserIdent]
  defaultLayout $ do
    addScript $ StaticR js_barcode_js
    setTitleI MsgMainPage
    $(widgetFile "home")

getReactivateR :: Handler Html
getReactivateR = do
  today <- liftIO $ return . utctDay =<< getCurrentTime
  users <- runDB $ selectList [UserTimestamp <. addDays (-30) today] [Asc UserIdent]
  defaultLayout $ do
    addScript $ StaticR js_barcode_js
    setTitleI MsgUserReactivate
    $(widgetFile "reactivate")

getUserReactivateR :: UserId -> Handler Html
getUserReactivateR uId = do
  mUser <- runDB $ get uId
  case mUser of
    Just _ -> do
      today <- liftIO $ return . utctDay =<< getCurrentTime
      runDB $ update uId [UserTimestamp =. today]
      setMessageI MsgUserReactivated
      redirect HomeR
    Nothing -> do
      setMessageI MsgUserUnknown
      redirect HomeR
