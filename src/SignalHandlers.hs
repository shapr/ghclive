{-# LANGUAGE CPP #-}

module SignalHandlers where

#if !defined(mingw32_HOST_OS)

import System.Posix.Signals
import Control.Concurrent
import Control.Monad

restoreHandlers :: IO ()
restoreHandlers = do
    mapM_ restore [sigQUIT, sigINT, sigHUP, sigTERM]
    putStrLn "" -- for some reason, restoring handlers often fails without this
  where
    restore s  = installHandler s Default Nothing

#else

restoreHandlers :: IO ()
restoreHandlers = return ()

#endif
