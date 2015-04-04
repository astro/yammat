module Handler.Summary where

import Import
import Data.List as L

getSummaryR :: Handler Html
getSummaryR = do
  bevs <- runDB $ selectList [] [Asc BeverageIdent]
  defaultLayout $ do
    $(widgetFile "summary")
