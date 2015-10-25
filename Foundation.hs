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
module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.BrowserId (authBrowserId)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import Settings.StaticFiles
--snip
import qualified Data.Text as T
import Network.Wai as Wai
import Data.List (tail)
import Data.List.Split
import Text.Printf



prependZero :: Text -> Text
prependZero t0
 | T.null t1              = t1
 | T.head t1 == '.'       = '0' `T.cons` t1
 | "-." `T.isPrefixOf` t1 = "-0." `T.append` T.drop 2 t1
 | otherwise              = t1
 where t1 = T.dropWhile (' ' ==) t0

formatFloat :: Double -> Text
formatFloat d = T.pack (pre ++ t ++ c)
  where
    t = reverse (intercalate "." $ chunksOf 3 $ reverse $ fst sp)
    c = "," ++ tail (snd sp)
    sp = break (== '.') (printf "%.2f" (abs d))
    pre = if d < 0
      then "-"
      else ""
    -- T.pack . (splitEvery 3) . (printf "%,2f")

formatIntCurrency :: Int -> Text
formatIntCurrency x = formatFloat $ fromIntegral x / 100

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

mkMessage "App" "messages" "de"

renderMessage' e = do
  m <- getYesod
  l <- languages
  return $ renderMessage m l e

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

approotRequest :: App -> Wai.Request -> T.Text
approotRequest master req =
    case requestHeaderHost req of
        Just a  -> prefix `T.append` decodeUtf8 a
        Nothing -> appRoot $ appSettings master
    where
        prefix =
            if isSecure req
                then "https://"
                else "http://"

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    --approot = ApprootMaster $ appRoot . appSettings
    approot = ApprootRequest approotRequest

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
        copyrightWidget <- widgetToPageContent $
            $(widgetFile "copyright")
        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_bootstrap_min_css
                , css_main_css
                ])
            $(combineScripts 'StaticR
                [ js_crementing_js
                , js_barcode_js
                ])
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")


    -- The page to be redirected to when authentication is required.
    -- authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    -- isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

-- instance YesodAuth App where
--     type AuthId App = UserId

--     -- Where to send a user after successful login
--     loginDest _ = HomeR
--     -- Where to send a user after logout
--     logoutDest _ = HomeR
--     -- Override the above two destinations when a Referer: header is present
--     redirectToReferer _ = True

--     getAuthId creds = runDB $ do
--         x <- getBy $ UniqueUser $ credsIdent creds
--         case x of
--             Just (Entity uid _) -> return $ Just uid
--             Nothing -> do
--                 fmap Just $ insert User
--                     { userIdent = credsIdent creds
--                     , userPassword = Nothing
--                     }

--     -- You can add other plugins like BrowserID, email or OAuth here
--     authPlugins _ = [authBrowserId def]

--     authHttpManager = getHttpManager

-- instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

