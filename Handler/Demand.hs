--  yammat - Yet Another MateMAT
--  Copyright (C) 2015  Amedeo Molnár, Astro
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
module Handler.Demand where

import Import

getDemandR :: Int -> Handler Html
getDemandR level = defaultLayout $ do
  let audio = lookup level $ [1..] `zip` map StaticR
              [audio_air_alarm_mp3,
               audio_nuclear_alarm_mp3,
               audio_alarm_loud_pulsating_mp3]
  $(widgetFile "demand")
