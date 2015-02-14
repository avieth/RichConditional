{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.RichConditional (

    TotalIf
  , decide

  , PartialIf
  , indicate

  , ifElse
  , inCase
  , ensure
  , ensurePositive
  , ensureNegative

  ) where

import Data.Maybe
import Data.Either
import Control.Monad

type Indication = Maybe

type Decision = Either

-- | Instances of this class provide a test on some type a. If it passes, it
--   gives a value of type b; otherwise, it gives Nothing.
class PartialIf a b where
  indicate :: a -> Indication b

-- | Instances of this class provide a test on some type a with an
--   alternative case for when it fails. If it passes, it gives a value of
--   type b; otherwise, it gives a value of type c.
class TotalIf a b c where
  decide :: a -> Decision b c

-- | Replace an if via an PartialIf instance. This is just like the
--   function 'maybe' which decomposes a Maybe, except that the Maybe
--   is produced through an PartialIf instance determined by the type of
--   the positive case.
inCase :: PartialIf a b => a -> (b -> c) -> c -> c
inCase x ifYes ifNo = maybe ifNo ifYes (indicate x)

-- | Replace an if/else via a TotalIf instance. This is just like the
--   function 'either' which decomposes an Either, except that the Either
--   is produced through a TotalIf instance determined by the types of
--   the cases.
ifElse :: TotalIf a b c => a -> (b -> d) -> (c -> d) -> d
ifElse x ifYes ifNo = either ifYes ifNo (decide x)

-- | Like 'guard' but with a bonus: if the condition passes, you actually get
--   a hold of some new information.
ensure :: (Monad m, MonadPlus m, PartialIf a b) => a -> m b
ensure x = inCase x return mzero

-- | Like 'ensure' but for the positive case (Left) of a TotalIf.
--   Requires a proxy on the negative type in order to disambiguate.
ensurePositive
  :: forall m a b c u .
     ( Monad m
     , MonadPlus m
     , TotalIf a b c
     )
   => a
   -> u c
   -> m b
ensurePositive x _ =
    let typedConstMzero = const mzero :: c -> m b
    in  ifElse x return typedConstMzero

-- | Like 'ensure' but for the negative case (Right) of a TotalIf.
--   Requires a proxy on the positive type in order to disambiguate.
ensureNegative
  :: forall m a b c u .
     ( Monad m
     , MonadPlus m
     , TotalIf a b c
     )
  => a
  -> u b
  -> m c
ensureNegative x _ =
    let typedConstMzero = const mzero :: b -> m c
    in  ifElse x typedConstMzero return
