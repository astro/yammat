module Handler.Summary where

import Import as I
import qualified Data.List as L
import Data.Aeson
import Data.Conduit.Binary
import Handler.Common

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
                 , "volume" .= ((fromIntegral (beverageMl bev)) / 1000 :: Double)
                 , "price" .= ((fromIntegral (beveragePrice bev)) / 100 :: Double)
                 , "currency" .= appCurrency (appSettings master)
                 ]
        ) bevs

data BevStore = BevStore
  { bevStoreIdent :: Text
  , bevStorePrice :: Int
  , bevStoreAmount :: Int
  , bevStoreAlertAmount :: Int
  , bevStoreMl :: Int
  }

instance ToJSON BevStore where
  toJSON (BevStore ident price amount alertAmount ml) =
    object
      [ "name" .= ident
      , "price" .= price
      , "amount" .= amount
      , "alertAt" .= alertAmount
      , "ml" .= ml
      ]

instance FromJSON BevStore where
  parseJSON (Object o) = BevStore
    <$> o .: "name"
    <*> o .: "price"
    <*> o .: "amount"
    <*> o .: "alertAt"
    <*> o .: "ml"
  -- For errors
  parseJSON _ = mzero

getInventoryJsonR :: Handler RepJson
getInventoryJsonR = do
  bevs <- runDB $ selectList [] [Asc BeverageIdent]
  return $
    repJson $ array $
      map (\(Entity _ bev) -> toJSON $ BevStore
        (beverageIdent bev)
        (beveragePrice bev)
        (beverageAmount bev)
        (beverageAlertAmount bev)
        (beverageMl bev)
        ) bevs

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
          bevs <- return $ fromMaybe [] $ (decode source :: Maybe [BevStore])
          I.mapM_ insOrUpd bevs
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

insOrUpd :: BevStore -> Handler ()
insOrUpd bev = do
  meb <- runDB $ getBy $ UniqueBeverage $ bevStoreIdent bev
  case meb of
    Just eb -> do
      runDB $ update (entityKey eb)
        [ BeveragePrice =. bevStorePrice bev
        , BeverageAmount =. bevStoreAmount bev
        , BeverageAlertAmount =. bevStoreAlertAmount bev
        , BeverageMl =. bevStoreMl bev
        ]
    Nothing -> do
      runDB $ insert_ $ Beverage
        (bevStoreIdent bev)
        (bevStorePrice bev)
        (bevStoreAmount bev)
        (bevStoreAlertAmount bev)
        0
        (bevStoreMl bev)
        Nothing
