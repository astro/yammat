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
module Handler.Statistics where

import Import
import Data.List hiding (length)
import Data.Maybe (fromJust)

getStatisticsR :: Handler RepJson
getStatisticsR = do
  users <- runDB $ selectList [] [Asc UserId]
  positiveBalance <- return $ foldl (\acc (Entity _ u) -> if userBalance u > 0
    then acc + (fromIntegral $ userBalance u) / 100
    else acc
    ) 0 users
  negativeBalance <- return $ foldl (\acc (Entity _ u) -> if userBalance u < 0
    then acc + (fromIntegral $ userBalance u) / 100
    else acc
    ) 0 users
  goodUsers <- runDB $ selectList [UserBalance >=. 0] []
  noobAngels <- runDB $ selectList [UserBalance >=. 0, UserBalance <=. 1000] []
  noobDevils <- runDB $ selectList [UserBalance <=. 0, UserBalance >=. -1000] []
  archangels <- runDB $ selectList [UserBalance >. 5000] []
  archdevils <- runDB $ selectList [UserBalance <. -5000] []
  bevs <- runDB $ selectList [] [Asc BeverageId]
  totalLossPrime <- return $ foldl (\acc (Entity _ bev) ->
    let primePrice = if not (isNothing (beveragePricePerCrate bev) && isNothing (beveragePerCrate bev)) then (fromIntegral $ fromJust (beveragePricePerCrate bev)) / (fromIntegral $ fromJust (beveragePerCrate bev)) else 0
    in acc + (((fromIntegral $ beverageCorrectedAmount bev) * primePrice) / 100)
    ) 0 bevs
  totalLossRetail <- return $ foldl (\acc (Entity _ bev) ->
    acc + ((fromIntegral $ beverageCorrectedAmount bev) * (fromIntegral $ beveragePrice bev) / 100)
    ) 0 bevs
  return $ repJson $ toJSON $ Statistics
    (length users)
    positiveBalance
    negativeBalance
    (length goodUsers)
    (length users - length goodUsers)
    (length noobAngels)
    (length noobDevils)
    (length archangels)
    (length archdevils)
    totalLossPrime
    totalLossRetail

data Statistics = Statistics
  { totalUsers :: Int
  , positiveBalance :: Double
  , negativeBalance :: Double
  , goodUsers :: Int
  , evilUsers :: Int
  , noobAngels :: Int
  , noobDevils :: Int
  , archangels :: Int
  , archdevils :: Int
  , totalLossPrime :: Double
  , totalLossRetail :: Double
  }

instance ToJSON Statistics where
  toJSON (Statistics tu pb nb gu eu na nd aa ad tlp tlr) =
    object
      [ "total_users" .= tu
      , "positive_balance" .= pb
      , "negative_balance" .= nb
      , "good_users" .= gu
      , "evil_users" .= eu
      , "noob_angels" .= na
      , "noob_devils" .= nd
      , "archangels" .= aa
      , "archdevils" .= ad
      , "total_loss_prime_price" .= tlp
      , "total_loss_retail_price" .= tlr
      ]
