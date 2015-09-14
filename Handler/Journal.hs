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
module Handler.Journal where

import Import as I
import Data.List as L
import Handler.Common

getJournalR :: Handler Html
getJournalR = do
  master <- getYesod
  rawEntries <- runDB $ selectList [] [Desc TransactionId]
  next <- runDB $ selectList [] [Desc TransactionId, OffsetBy 30]
  let entries = L.reverse $ L.take 30 rawEntries
  let total = L.sum $ I.map (transactionAmount . entityVal) rawEntries
  timeLimit <- if L.null entries
    then liftIO getCurrentTime
    else return $ transactionTime $ entityVal $ L.head entries
  cashChecks <- runDB $ selectList [CashCheckTime >=. timeLimit] [Asc CashCheckId]
  let list = merge entries cashChecks
  cashBalance <- getCashierBalance
  defaultLayout $
    $(widgetFile "journal")

merge :: [Entity Transaction] -> [Entity CashCheck] -> [Either Transaction CashCheck]
merge [] [] = []
merge [] (c:cs) = (Right $ entityVal c) : merge [] cs
merge (t:ts) [] = (Left $ entityVal t) : merge ts []
merge (t:ts) (c:cs)
  | transactionTime (entityVal t) < cashCheckTime (entityVal c) = (Left $ entityVal t)  : merge ts (c:cs)
  | transactionTime (entityVal t) > cashCheckTime (entityVal c) = (Right $ entityVal c) : merge (t:ts) cs

getJournalPageR :: Int -> Handler Html
getJournalPageR p = do
  master <- getYesod
  rawEntries <- runDB $ selectList [] [Desc TransactionId, OffsetBy (p * 30)]
  next <- runDB $ selectList [] [Desc TransactionId, OffsetBy ((p + 1) * 30)]
  let entries = L.reverse $ L.take 30 rawEntries
  lTimeLimit <- if L.null entries
    then liftIO getCurrentTime
    else return $ transactionTime $ entityVal $ L.head entries
  uTimeLimit <- if L.null entries
    then liftIO getCurrentTime
    else return $ transactionTime $ entityVal $ L.last entries
  cashChecks <- runDB $ selectList [CashCheckTime >=. lTimeLimit, CashCheckTime <. uTimeLimit] [Asc CashCheckId]
  let list = merge entries cashChecks
  defaultLayout $
    $(widgetFile "journalPage")
