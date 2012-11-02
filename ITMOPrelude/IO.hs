{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.IO where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Categories

data RealWorld = RealWorld
    { stdIn :: List Nat
    , stdOut :: List Nat
    , exitCode :: Nat }

type IO a = State RealWorld a

getNat :: IO Nat
getNat = State $ \r -> (RealWorld (end (stdIn r)) (stdOut r) (exitCode r), start (stdIn r))

putNat :: Nat -> IO ()
putNat = State $ \r -> (RealWorld (stdIn r) (Cons a (stdOut r)) (exitCode r), ())

setExitCode :: Nat -> IO ()
setExitCode c = State $ \r -> (RealWorld (stdIn r) (stdOut r) c, ())
