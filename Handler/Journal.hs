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
  entries <- return $ L.reverse $ L.take 30 rawEntries
  total <- return $ L.sum $ I.map (transactionAmount . entityVal) rawEntries
  timeLimit <- case L.null entries of
    False -> return $ transactionTime $ entityVal $ L.head $ entries
    True -> liftIO getCurrentTime
  cashChecks <- runDB $ selectList [CashCheckTime >=. timeLimit] [Asc CashCheckId]
  list <- return $ merge entries cashChecks
  cashBalance <- getCashierBalance
  defaultLayout $ do
    $(widgetFile "journal")

merge :: [Entity Transaction] -> [Entity CashCheck] -> [Either Transaction CashCheck]
merge [] [] = []
merge [] (c:cs) = (Right $ entityVal c) : merge [] cs
merge (t:ts) [] = (Left $ entityVal t) : merge ts []
merge (t:ts) (c:cs)
  | transactionTime (entityVal t) < cashCheckTime (entityVal c) = (Left $ entityVal t)  : merge ts (c:cs)
  | transactionTime (entityVal t) > cashCheckTime (entityVal c) = (Right $ entityVal c) : merge (t:ts) cs
