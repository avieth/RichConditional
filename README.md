Control.RichConditional
=======================

Typeclasses for describing "rich conditionals" which accomplish exactly what
a classic if/else would but without the use of `Bool`.

The inspiration is found in dependently typed languages, in which a `Bool` is
seen to be a comparatively pathetic datatype: it carries very little
information. A useful test for some condition always introduces new
information, but with a classical if/else, this information not known to the
compiler because it's lossfully compressed into a `Bool`.

There is no new technology here, only typeclasses and functions which might
make it more convenient to avoid using if/else.

#A motivating example

To demonstrate the point, suppose we defined the following module, in which
the constructors of `Visitor` are not exposed:

```Haskell
module Visitor (
    Visitor
  , isLoggedIn
  , isGuest
  ) where

data Visitor = LoggedIn User | Guest

isLoggedIn :: Visitor -> Bool
isLoggedIn v = case v of
  LoggedIn _ -> True
  Guest -> False

isGuest :: Visitor -> Bool
isGuest v = case v of
  LoggedIn _ -> False
  Guest -> True
```

We have indicator functions on `Visitor`, but they don't allow for a 
useful interface. In the example below, we can't build a user's landing
page because we can't get a hold of a `User` value.

```Haskell
userLandingPage :: User -> LandingPage

guestLandingPage :: LandingPage

blindLandingPage :: Visitor -> LandingPage
blindLandingPage v =
  if isLoggedIn v
  -- We know there's a user, but GHC does not!
  then userLandingPage ?
  else guestLandingPage
```

Evidently the `Visitor` library must provide some way to get a hold of
a `User` from a `Visitor`, but if it does provide this, then why even bother
giving the indicators `isLoggedIn` and `isGuest`?

Contrast the above definitions with a `Bool`-free approach:

```Haskell
module Visitor (
    Visitor
  , ifVisitor
  ) where

data Visitor = LoggedIn User | Guest

ifVisitor :: Visitor -> (User -> a) -> a -> a
ifVisitor v ifUser ifGuest = case v of
  LoggedIn user -> ifUser user
  Guest -> ifGuest
```

`Visitor` is just `Maybe User` with a new name, and `ifVisitor` is just the
function `maybe` with its parameter order shuffled. It provides users of
`Visitor` a way to get a hold of a `User` for `LoggedIn` cases without
pattern matching on `Visitor` directly. It can be used to implement a well
factored version of the landing page example:

```Haskell
userLandingPage :: User -> LandingPage

guestLandingPage :: LandingPage

landingPage :: Visitor -> LandingPage
landingPage v = ifVisitor v userLandingPage guestLandingPage
```

#Use of RichConditional

This modification of the above examples shows how RichConditional could be
used:

```Haskell
{-# LANGUAGE MultiParamTypeClasses #-}

data User = User

data Guest = Human | Robot

data Visitor = LoggedIn User | NotLoggedIn Guest

instance PartialIf Visitor User where
  indicate v = case v of
    LoggedIn user -> Just user
    NotLoggedIn _ -> Nothing

instance PartialIf Visitor Guest where
  indicate v = case v of
    LoggedIn _ -> Nothing
    NotLoggedIn guest -> Just guest

instance TotalIf Visitor User Guest where
  decide v = case v of
    LoggedIn user -> Left user
    NotLoggedIn guest -> Right guest

allGuests :: [Visitor] -> [Guest]
allGuests vs = do
  v <- vs
  -- This is like guard, except that useful data comes
  -- out of it, so that it fulfills the type signature
  -- of allGuests.
  ensure v
```
