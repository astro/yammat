module Handler.Summary where

import Import
import qualified Data.List as L
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
                 , "price" .= beveragePrice bev
                 , "currency" .= appCurrency (appSettings master)
                 ]
        ) bevs
