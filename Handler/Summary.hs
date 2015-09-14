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
  defaultLayout $
    $(widgetFile "uploadJson")

postUploadInventoryJsonR :: Handler Html
postUploadInventoryJsonR = do
  ((res, _), _) <- runFormPost uploadJsonForm
  case res of
    FormSuccess file ->
      if fileContentType file == "application/json"
        then do
          source <- runResourceT $ fileSource file $$ sinkLbs
          let bevs = fromMaybe [] (decode source :: Maybe [BevStore])
          I.mapM_ insOrUpd bevs
          setMessageI MsgRestoreSuccess
          redirect HomeR
        else do
          setMessageI MsgNotJson
          redirect UploadInventoryJsonR
    _ -> do
      setMessageI MsgErrorOccured
      redirect UploadInventoryJsonR

uploadJsonForm :: Form FileInfo
uploadJsonForm = renderDivs
  $ areq fileField (fieldSettingsLabel MsgSelectFile) Nothing

insOrUpd :: BevStore -> Handler ()
insOrUpd bev = do
  meb <- runDB $ getBy $ UniqueBeverage $ bevStoreIdent bev
  case meb of
    Just eb ->
      runDB $ update (entityKey eb)
        [ BeveragePrice =. bevStorePrice bev
        , BeverageAmount =. bevStoreAmount bev
        , BeverageAlertAmount =. bevStoreAlertAmount bev
        , BeverageMl =. bevStoreMl bev
        ]
    Nothing ->
      runDB $ insert_ $ Beverage
        (bevStoreIdent bev)
        (bevStorePrice bev)
        (bevStoreAmount bev)
        (bevStoreAlertAmount bev)
        0
        (bevStoreMl bev)
        Nothing
