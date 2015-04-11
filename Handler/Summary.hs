module Handler.Summary where

import Import
import qualified Data.List as L
import Data.Aeson
import Data.Conduit.Binary

getSummaryR :: Handler Html
getSummaryR = do
  master <- getYesod
  bevs <- runDB $ selectList [] [Asc BeverageIdent]
  defaultLayout $ do
    $(widgetFile "summary")

getSummaryJsonR :: Handler RepJson
getSummaryJsonR = do
  master <- getYesod
  bevs <- runDB $ selectList [] [Asc BeverageIdent]
  return $
    repJson $ array $
    map (\(Entity _ bev) ->
          object [ "name" .= beverageIdent bev
                 , "value" .= beverageAmount bev
                 , "price" .= ((fromIntegral (beveragePrice bev)) / 100 :: Double)
                 , "currency" .= appCurrency (appSettings master)
                 ]
        ) bevs

instance ToJSON Beverage where
  toJSON (Beverage ident price amount alertAmount) =
    object
      [ "name" .= ident
      , "price" .= price
      , "amount" .= amount
      , "alertAt" .= alertAmount
      ]

instance FromJSON Beverage where
  parseJSON (Object o) = Beverage
    <$> o .: "name"
    <*> o .: "price"
    <*> o .: "amount"
    <*> o .: "alertAt"
  -- For errors
  parseJSON _ = mzero

getInventoryJsonR :: Handler RepJson
getInventoryJsonR = do
  bevs <- runDB $ selectList [] [Asc BeverageIdent]
  return $
    repJson $ array $
      map (\(Entity _ bev) -> toJSON bev) bevs

getUploadInventoryJsonR :: Handler Html
getUploadInventoryJsonR = do
  (uploadJsonWidget, enctype) <- generateFormPost uploadJsonForm
  defaultLayout $ do
    $(widgetFile "uploadJson")

postUploadInventoryJsonR :: Handler Html
postUploadInventoryJsonR = do
  ((res, _), _) <- runFormPost uploadJsonForm
  case res of
    FormSuccess file -> do
      case fileContentType file == "application/json" of
        True -> do
          source <- runResourceT $ fileSource file $$ sinkLbs
          bevs <- return $ fromMaybe [] $ (decode source :: Maybe [Beverage])
          _ <- return $ map insOrUpd bevs
          setMessageI MsgRestoreSuccess
          redirect $ HomeR
        False -> do
          setMessageI MsgNotJson
          redirect $ UploadInventoryJsonR
    _ -> do
      setMessageI MsgErrorOccured
      redirect $ UploadInventoryJsonR

uploadJsonForm :: Form FileInfo
uploadJsonForm = renderDivs
  $ areq fileField (fieldSettingsLabel MsgSelectFile) Nothing

insOrUpd :: Beverage -> Handler ()
insOrUpd bev = do
  meb <- runDB $ getBy $ UniqueBeverage $ beverageIdent bev
  case meb of
    Just eb -> do
      runDB $ update (entityKey eb)
        [ BeveragePrice =. beveragePrice bev
        , BeverageAmount =. beverageAmount bev
        , BeverageAlertAmount =. beverageAlertAmount bev
        ]
    Nothing -> do
      runDB $ insert_ bev
