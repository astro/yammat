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
  defaultLayout $
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
                 , "volume" .= (fromIntegral (beverageMl bev) / 1000 :: Double)
                 , "price" .= (fromIntegral (beveragePrice bev) / 100 :: Double)
                 , "currency" .= appCurrency (appSettings master)
                 ]
        ) bevs

data BevStore = BevStore
  { bevStoreIdent :: Text
  , bevStorePrice :: Int
  , bevStoreAmount :: Int
  , bevStoreMaxAmount :: Int
  , bevStorePerCrate :: Maybe Int
  , bevStoreAlertAmount :: Int
  , bevStoreMl :: Int
  , bevStoreArtNr :: Maybe Text
  , bevStorePricePerCrate :: Maybe Int
  }

instance ToJSON BevStore where
  toJSON (BevStore ident price amount maxAmount perCrate alertAmount ml artNr ppc) =
    object
      [ "name" .= ident
      , "price" .= price
      , "amount" .= amount
      , "alertAt" .= alertAmount
      , "max" .= maxAmount
      , "perCrate" .= perCrate
      , "ml" .= ml
      , "artNr" .= artNr
      , "pricePerCrate" .= ppc
      ]

instance FromJSON BevStore where
  parseJSON (Object o) = BevStore
    <$> o .: "name"
    <*> o .: "price"
    <*> o .: "amount"
    <*> o .: "max"
    <*> o .:? "perCrate"
    <*> o .: "alertAt"
    <*> o .: "ml"
    <*> o .:? "artNr"
    <*> o .:? "pricePerCrate"
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
        (beverageMaxAmount bev)
        (beveragePerCrate bev)
        (beverageAlertAmount bev)
        (beverageMl bev)
        (beverageArtNr bev)
        (beveragePricePerCrate bev)
        ) bevs

getUploadInventoryJsonR :: Handler Html
getUploadInventoryJsonR = do
  (uploadJsonWidget, enctype) <- generateFormPost
    $ renderBootstrap3 BootstrapBasicForm uploadJsonForm
  defaultLayout $
    $(widgetFile "uploadJson")

postUploadInventoryJsonR :: Handler Html
postUploadInventoryJsonR = do
  ((res, _), _) <- runFormPost
    $ renderBootstrap3 BootstrapBasicForm uploadJsonForm
  case res of
    FormSuccess file ->
      if fileContentType file == "application/json"
        then do
          source <- runResourceT $ fileSource file $$ sinkLbs
          let bevs = fromMaybe [] (decode source :: Maybe [BevStore])
          _ <- I.mapM insOrUpd bevs
          setMessageI MsgRestoreSuccess
          redirect HomeR
        else do
          setMessageI MsgNotJson
          redirect UploadInventoryJsonR
    _ -> do
      setMessageI MsgErrorOccured
      redirect UploadInventoryJsonR

uploadJsonForm :: AForm Handler FileInfo
uploadJsonForm = areq fileField (bfs MsgSelectFile) Nothing
  <* bootstrapSubmit (msgToBSSubmit MsgSubmit)

insOrUpd :: BevStore -> Handler (Entity Beverage)
insOrUpd bev = do
  nbev <- return $ Beverage
    (bevStoreIdent bev)
    (bevStorePrice bev)
    (bevStoreAmount bev)
    (bevStoreAlertAmount bev)
    0
    (bevStoreMl bev)
    Nothing
    Nothing
    (bevStoreMaxAmount bev)
    (bevStorePerCrate bev)
    (bevStoreArtNr bev)
    (bevStorePricePerCrate bev)
  runDB $ upsert nbev
    [ BeverageIdent =. bevStoreIdent bev
    , BeveragePrice =. bevStorePrice bev
    , BeverageAmount =. bevStoreAmount bev
    , BeverageAlertAmount =. bevStoreAlertAmount bev
    , BeverageMl =. bevStoreMl bev
    , BeverageMaxAmount =. bevStoreMaxAmount bev
    , BeveragePerCrate =. bevStorePerCrate bev
    , BeverageArtNr =. bevStoreArtNr bev
    , BeveragePricePerCrate =. bevStorePricePerCrate bev
    ]
