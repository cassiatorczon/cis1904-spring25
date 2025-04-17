module Concurrent where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (replicateM_)
import Data.IORef

progA :: IO ()
progA = do
  print 1
  print 2

progB :: IO ()
progB = do
  print 3
  print 4

example :: IO ()
example = do
  forkIO progB
  progA

-- bank transfers:

type Account = IORef Int

withdraw :: Account -> Int -> IO ()
withdraw acc amount = do
  balance <- readIORef acc
  writeIORef acc (balance - amount)

deposit :: Account -> Int -> IO ()
deposit acc amount = withdraw acc (-amount)

transfer :: Account -> Account -> Int -> IO ()
transfer from to amount = do
  withdraw from amount
  deposit to amount

view :: Account -> IO ()
view acc = readIORef acc >>= print

bank :: IO ()
bank = do
  acc1 <- newIORef 100
  acc2 <- newIORef 100
  forkIO (view acc1 >> view acc2)
  replicateM_ 10 (transfer acc1 acc2 10)

-- bank transfers, now with STM:

type Account' = TVar Int

withdraw' :: Account' -> Int -> STM ()
withdraw' acc amount = do
  balance <- readTVar acc
  writeTVar acc (balance - amount)

deposit' :: Account' -> Int -> STM ()
deposit' acc amount = withdraw' acc (-amount)

transfer' :: Account' -> Account' -> Int -> STM ()
transfer' from to amount = do
  withdraw' from amount
  deposit' to amount

view' :: Account' -> IO ()
view' acc = readTVarIO acc >>= print

bank' :: IO ()
bank' = do
  acc1 <- newTVarIO 100
  acc2 <- newTVarIO 100
  forkIO (view' acc1 >> view' acc2)
  replicateM_ 10 (atomically (transfer' acc1 acc2 10))

bank'' :: IO ()
bank'' = do
  acc1 <- newTVarIO 100
  acc2 <- newTVarIO 100
  forkIO (view' acc1 >> view' acc2)
  atomically (replicateM_ 10 (transfer' acc1 acc2 10))