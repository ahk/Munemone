module Munemone.hs where
import System.FilePath
import System.FilePath.Glob
import Database.Redis
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.UTF8 as B
import Control.Concurrent
import Control.Concurrent.STM
import System.Environment

{-DONE: read a dir for mp3s-}
{-TODO: take read filenames put into DB-}
{- hedis docs: http://hackage.haskell.org/packages/archive/hedis/0.4.1/doc/html/Database-Redis.html-}

testMp3 = joinPath ["a_fake_dir", "#1hit.mp3"]
redisDB = 0
maxRedisConn = 50

musicDir = takeDirectory testMp3

mp3s = fmap (head.fst) globber
    where globber = globDir [compile "*.mp3"] musicDir

writeTracks = do
    set (b "hello") (b "drewbins")
    set (b "world") (b "world")
    hello <- get (b "hello")
    world <- get (b "world")
    liftIO $ print (hello,world)
        where b = B.fromString

{-stupid thread reading code-}
atomRead = atomically . readTVar
dispVar x = atomRead x >>= print
appV fn x = atomically $ readTVar x >>= writeTVar x . fn

main = do
    argv <- getArgs
    putStrLn . show $ argv
    files <- mp3s
    putStrLn . show $ files

    shared <- atomically $ newTVar 0
    before <- atomRead shared
    putStrLn $ "Before: " ++ show before

    conn <- connect defaultConnectInfo { connectMaxConnections = maxRedisConn }
    runRedis conn $ select redisDB
    forkIO $ runRedis conn $ 50 `timesDo` writeTracks
    forkIO $ runRedis conn $ 50 `timesDo` writeTracks
    forkIO $ runRedis conn $ 50 `timesDo` writeTracks
    {-forkIO $ 50 `timesDo` (appV ((+) 2) shared >> milliSleep 50)-}
    {-forkIO $ 50 `timesDo` (appV pred shared >> milliSleep 25)-}

    after <- atomRead shared
    putStrLn $ "After: " ++ show after
    where timesDo = replicateM_
          milliSleep = threadDelay . (*) 1000
