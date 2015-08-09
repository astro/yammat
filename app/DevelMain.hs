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
-- | Development version to be run inside GHCi.
--
-- start this up with:
--
-- cabal repl --ghc-options="-O0 -fobject-code"
--
-- run with:
--
-- :l DevelMain
-- DevelMain.update
--
-- You will need to add these packages to your .cabal file
-- * foreign-store >= 0.1 (very light-weight)
-- * warp (you already depend on this, it just isn't in your .cabal file)
--
-- If you don't use cabal repl, you will need
-- to add settings to your .ghci file.
--
-- :set -DDEVELOPMENT
--
-- There is more information about using ghci
-- on the wiki: https://github.com/yesodweb/yesod/wiki/ghci

module DevelMain where

import Application (getApplicationDev)

import Control.Exception (finally)
import Control.Concurrent
import Data.IORef
import Foreign.Store
import Network.Wai.Handler.Warp

-- | Start or restart the server.
-- A Store holds onto some data across ghci reloads
update :: IO ()
update = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
      -- no server running
      Nothing -> do
          done <- storeAction doneStore newEmptyMVar
          tid <- start done
          _ <- storeAction (Store tidStoreNum) (newIORef tid)
          return ()
      -- server is already running
      Just tidStore ->
          -- shut the server down with killThread and wait for the done signal
          modifyStoredIORef tidStore $ \tid -> do
              killThread tid
              withStore doneStore takeMVar >> readStore doneStore >>= start
  where
    doneStore = Store 0
    tidStoreNum = 1

    modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
    modifyStoredIORef store f = withStore store $ \ref -> do
        v <- readIORef ref
        f v >>= writeIORef ref

-- | Start the server in a separate thread.
start :: MVar () -- ^ Written to when the thread is killed.
      -> IO ThreadId
start done = do
    (settings,app) <- getApplicationDev
    forkIO (finally (runSettings settings app)
                    (putMVar done ()))
