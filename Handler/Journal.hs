module Handler.Journal where

import Import as I
import Data.List as L
import Handler.Common

getJournalR :: Handler Html
getJournalR = do
  entries <- runDB $ selectList [] [Asc TransactionId]
  total <- return $ L.sum $ I.map (transactionAmount . entityVal) entries
  cashBalance <- getCashierBalance
  defaultLayout $ do
    $(widgetFile "journal")
