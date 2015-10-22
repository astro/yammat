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
-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Yesod.Form.Bootstrap3
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Read as R
import Data.Maybe
import qualified Data.Char as C
import Yesod.Form.Functions
import Text.Shakespeare.Text
import Network.Mail.Mime
import Import

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
  $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
  $ toContent $(embedFile "config/robots.txt")

-- msgToBSSubmit :: T.Text -> BootstrapSubmit T.Text
msgToBSSubmit t = BootstrapSubmit
  { bsValue = t
  , bsClasses = "btn-default"
  , bsAttrs = []
  }

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys)
  | x == y    = removeItem x ys
  | otherwise = y : (removeItem x ys)

updateCashier :: Int -> Text -> Handler ()
updateCashier amount desc = do
  mCashier <- runDB $ selectFirst [] [Desc CashierId]
  trans <- liftIO $ (return . Transaction desc amount) =<< getCurrentTime
  case mCashier of
    Just entCash -> do
      runDB $ update (entityKey entCash) [CashierBalance +=. amount]
      runDB $ insert_ trans
    Nothing -> do
      currentTime <- liftIO getCurrentTime
      runDB $ insert_ $ Cashier amount currentTime
      runDB $ insert_ trans

getCashierBalance :: Handler Int
getCashierBalance = do
  mCashier <- runDB $ selectFirst [] [Desc CashierId]
  case mCashier of
    Just cashier ->
      return $ cashierBalance $ entityVal cashier
    Nothing ->
      return 0

currencyField :: (RenderMessage (HandlerSite m) FormMessage, Show a, Monad m, Integral a) => Field m a
currencyField = Field
  { fieldParse = parseHelper $ \rawVals ->
      case R.double (prependZero rawVals) of
        Right (a, "") -> Right $ floor $ (100 * a) + 0.5
        _             -> Left $ MsgInvalidNumber rawVals
  , fieldView = \theId name attr val req -> toWidget [hamlet|$newline never
      <input id=#{theId} name=#{name} *{attr} type="number" step=0.01 min=0 :req:required="required" value=#{showVal val}>
      |]
  , fieldEnctype = UrlEncoded
  }
  where
    showVal = either id (pack . showA)
    showA x = show ((fromIntegral x :: Double) / 100)

volumeField :: (RenderMessage (HandlerSite m) FormMessage, Show a, Monad m, Integral a) => Field m a
volumeField = Field
  { fieldParse = parseHelper $ \rawVals ->
      case R.double (prependZero rawVals) of
        Right (a, "") -> Right $ floor $ 1000 * a
        _             -> Left $ MsgInvalidNumber rawVals
  , fieldView = \theId name attr val req -> toWidget [hamlet|$newline never
      <input id=#{theId} name=#{name} *{attr} type="number" step=0.01 min=0 :req:required="required" value=#{showVal val}>
      |]
  , fieldEnctype = UrlEncoded
  }
  where
    showVal = either id (pack . showA)
    showA x = show ((fromIntegral x :: Double) / 1000)

barcodeField = Field
  { fieldParse = parseHelper $ Right . removeItem "" . L.nub . T.splitOn ", "
  , fieldView = \theId name attrs val isReq -> toWidget [hamlet|$newline never
      <input type="text" id="#{theId}" name="#{name}" :isReq:required="" *{attrs} value="#{either id (T.intercalate ", ") val}">
      |]
  , fieldEnctype = UrlEncoded
  }

handleBarcodes :: Either UserId BeverageId -> [Text] -> Handler ()
handleBarcodes (Left uId) nbs = do
  raws <- runDB $ selectList [BarcodeUser ==. Just uId] []
  let obs = map (barcodeCode . entityVal) raws
  let toDel = obs L.\\ nbs
  let toAdd = nbs L.\\ obs
  mapM_ (\b -> runDB $ insert_ $ Barcode
    b
    True
    (Just uId)
    Nothing
    ) toAdd
  ents <- mapM (runDB . getBy . UniqueBarcode) toDel
  mapM_ (runDB . delete . entityKey . fromJust) ents
handleBarcodes (Right bId) nbs = do
  raws <- runDB $ selectList [BarcodeBev ==. Just bId] []
  let obs = map (barcodeCode . entityVal) raws
  let toDel = obs L.\\ nbs
  let toAdd = nbs L.\\ obs
  mapM_ (\b -> runDB $ insert_ $ Barcode
    b
    False
    Nothing
    (Just bId)
    ) toAdd
  ents <- mapM (runDB . getBy . UniqueBarcode) toDel
  mapM_ (runDB . delete . entityKey . fromJust) ents

handleGetParam :: Maybe Text -> Either UserId BeverageId -> Handler ()
handleGetParam Nothing _ =
  return ()
handleGetParam (Just b) eub = do
  f <- return $ T.filter C.isAlphaNum b
  if T.length f > 0 && b /= ", "
    then do
      e <- runDB $ getBy $ UniqueBarcode f
      if isNothing e
        then do
          _ <- case eub of
            Left uId -> do
              -- should usernames containing, among other, spaces cause problems, replace b for f here
              runDB $ insert_ $ Barcode b True (Just uId) Nothing
            Right bId -> do
              -- and here
              runDB $ insert_ $ Barcode b False Nothing (Just bId)
          setMessageI MsgBarcodeAdded
        else
          setMessageI MsgBarcodeDuplicate
    else
      setMessageI MsgProvideBarcode

amountField :: (RenderMessage (HandlerSite m) FormMessage, Show a, Monad m, Integral a) => Field m a
amountField = Field
  { fieldParse = parseHelper $ \s ->
    case R.decimal s of
      Right (a, "") -> Right a
      _ -> Left $ MsgInvalidInteger s
  , fieldView = \theId name attr val req -> toWidget [hamlet|$newline never
    <input #crement id=#{theId} name=#{name} *{attr} type="number" step=1 min=0 :req:required="required" value="#{showVal val}">
    |]
  , fieldEnctype = UrlEncoded
  }
  where
    showVal = either id (pack . showI)
    showI x = show (fromIntegral x :: Integer)

checkAlert :: BeverageId -> Handler ()
checkAlert bId = do
  bev <- runDB $ getJust bId
  if beverageAmount bev < beverageAlertAmount bev
    then do
      master <- getYesod
      let to = appEmail $ appSettings master 
      liftIO $ sendMail to "Niedriger Bestand"
        [stext|
Hallo,

Der Bestand an #{beverageIdent bev} ist unterhalb der Warnschwelle von #{beverageAlertAmount bev}.
Der momentane Bestand ist bei #{beverageAmount bev} Artikeln.

Viele Grüße,

der Matemat
        |]
    else
      return () -- do nothing

--sendMail :: MonadIO m => Text -> Text -> Text -> m ()
sendMail to subject body =
  renderSendMail
    Mail
      { mailFrom = Address Nothing "noreply"
      , mailTo = [Address Nothing to]
      , mailCc = []
      , mailBcc = []
      , mailHeaders = [("Subject", subject),
         ("List-Id", "\"Matemat\" <matemat@matemat.hq.c3d2.de>")]
      , mailParts =[[Part
        { partType = "text/plain; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partHeaders = []
        , partContent = E.encodeUtf8 body
        }]]
      }

formatIntVolume :: Int -> Text
formatIntVolume x = formatFloat (fromIntegral x / 1000)
