import Database.HDBC
import Database.HDBC.PostgreSQL
import System.IO
import Control.Exception
import Data.ByteString.Base64
import qualified Crypto.Hash.SHA3 as SHA3

main :: IO ()
main = do
  putStrLn "Enter database host"
  dbHost <- getLine
  putStrLn "Enter database port"
  dbPort <- getLine
  putStrLn "Enter database user"
  dbUser <- getLine
  putStrLn "Enter database name"
  dbName <- getLine
  putStrLn "Enter database password"
  dbPasswd <- getPasswd
  let dbString = "host=" ++ dbHost ++ " port=" ++ dbPort ++ " user=" ++ dbUser ++ " dbname=" ++ dbName ++ " password=" ++ dbPasswd
  conn <- connectPostgreSQL dbString
  stmt1 <- prepare conn "select * from avatar"
  _ <- execute stmt1 []
  rows <- fetchAllRowsAL stmt1
  tups <- return $ map (\entry ->
    case entry of
      [("id", theId), ("ident", _), ("data", SqlByteString theData), ("hash", _)] ->
        [SqlByteString $ encode $ SHA3.hash 32 theData, theId]
      _ ->
        error "malformed entry"
    ) rows
  stmt2 <- prepare conn "update avatar set hash = ? where id = ?"
  executeMany stmt2 tups
  commit conn
  disconnect conn
  putStrLn "Migration successfull!!"

getPasswd :: IO String
getPasswd = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
