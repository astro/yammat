-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
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

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys)
  | x == y = removeItem x ys
  | otherwise = y : (removeItem x ys)

updateCashier :: Int -> Text -> Handler ()
updateCashier amount desc = do
  mCashier <- runDB $ selectFirst [] [Desc CashierId]
  trans <- liftIO $ (\time -> return $ Transaction desc amount time) =<< getCurrentTime
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
    Just cashier -> do
      return $ cashierBalance $ entityVal cashier
    Nothing -> do
      return 0

currencyField :: (RenderMessage (HandlerSite m) FormMessage, Show a, Monad m, Integral a) => Field m a
currencyField = Field
  { fieldParse = parseHelper $ \rawVals ->
      case R.double (prependZero rawVals) of
        Right (a, "") -> Right $ floor $ 100 * a
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
  obs <- return $ map (barcodeCode . entityVal) raws
  toDel <- return $ obs L.\\ nbs
  toAdd <- return $ nbs L.\\ obs
  _ <- mapM (\b -> runDB $ insert_ $ Barcode
    b
    True
    (Just uId)
    Nothing
    ) toAdd
  ents <- mapM (runDB . getBy . UniqueBarcode) toDel
  mapM_ (runDB . delete . entityKey . fromJust) ents
handleBarcodes (Right bId) nbs = do
  raws <- runDB $ selectList [BarcodeBev ==. Just bId] []
  obs <- return $ map (barcodeCode . entityVal) raws
  toDel <- return $ obs L.\\ nbs
  toAdd <- return $ nbs L.\\ obs
  _ <- mapM (\b -> runDB $ insert $ Barcode
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
  case (T.length f) > 0 of
    True -> do
      e <- runDB $ getBy $ UniqueBarcode f
      case e of
        Nothing -> do
          _ <- case eub of
            Left uId -> do
              -- should usernames containing, among other, spaces cause problems, replace b for f here
              runDB $ insert_ $ Barcode b True (Just uId) Nothing
            Right bId -> do
              -- and here
              runDB $ insert_ $ Barcode b False Nothing (Just bId)
          setMessageI MsgBarcodeAdded
        Just _ ->
          setMessageI MsgBarcodeDuplicate
    False -> do
      setMessageI MsgProvideBarcode

amountField :: (RenderMessage (HandlerSite m) FormMessage, Show a, Monad m, Integral a) => Field m a
amountField = Field
  { fieldParse = parseHelper $ \s ->
    case R.decimal s of
      Right (a, "") -> Right a
      _ -> Left $ MsgInvalidInteger s
  , fieldView = \theId name attr val req -> toWidget [hamlet|$newline never
    <input id="crement" id=#{theId} name=#{name} *{attr} type="number" step=1 min=0 :req:required="required" value="#{showVal val}">
    |]
  , fieldEnctype = UrlEncoded
  }
  where
    showVal = either id (pack . showI)
    showI x = show (fromIntegral x :: Integer)

checkAlert :: BeverageId -> Handler ()
checkAlert bId = do
  bev <- runDB $ getJust bId
  case beverageAmount bev < beverageAlertAmount bev of
    True -> do
      master <- getYesod
      to <- return $ appEmail $ appSettings master 
      liftIO $ sendMail to "Niedriger Bestand"
        [stext|
Hallo,

Der Bestand an #{beverageIdent bev} ist unterhalb der Warnschwelle von #{beverageAlertAmount bev}.
Der momentane Bestand ist bei #{beverageAmount bev} Artikeln.

Viele Grüße,

der Matemat
        |]
    False -> return () -- do nothing

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
formatIntVolume x = formatFloat $ ((fromIntegral x) / 1000)
