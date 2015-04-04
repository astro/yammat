module Handler.Summary where

import Import
import Data.List as L

getSummaryR :: Handler Html
getSummaryR = do
  master <- getYesod
  bevs <- runDB $ selectList [] [Asc BeverageIdent]
  defaultLayout $ do
    $(widgetFile "summary")
