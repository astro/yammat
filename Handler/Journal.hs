module Handler.Journal where

import Import as I
import Data.List as L
import Handler.Common

getJournalR :: Handler Html
getJournalR = do
  master <- getYesod
  rawEntries <- runDB $ selectList [] [Desc TransactionId, LimitTo 30]
  entries <- return $ L.reverse rawEntries
  total <- return $ L.sum $ I.map (transactionAmount . entityVal) entries
  timeLimit <- return $ transactionTime $ entityVal $ L.last $ entries
  cashiers <- runDB $ selectList [CashierCreated <=. timeLimit] [Asc CashierId]
  list <- return $ merge entries cashiers
  cashBalance <- getCashierBalance
  defaultLayout $ do
    $(widgetFile "journal")

merge :: [Entity Transaction] -> [Entity Cashier] -> [Either Transaction Cashier]
merge [] [] = []
merge (t:ts) [] = (Left $ entityVal t) : merge ts []
merge (t:ts) (c:cs)
  | transactionTime (entityVal t) < cashierCreated (entityVal c) = (Left $ entityVal t)  : merge ts (c:cs)
  | transactionTime (entityVal t) > cashierCreated (entityVal c) = (Right $ entityVal c) : merge (t:ts) cs
