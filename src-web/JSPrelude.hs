{-# OPTIONS_GHC -Wno-orphans #-}

module JSPrelude
  ( module JSPrelude
  , module Asterius
  ) where

import Asterius.Types as Asterius

instance Semigroup JSString where (<>) = js_str_concat

foreign import javascript "wrapper"
  makeHaskellCallback :: IO JSVal -> IO JSFunction
foreign import javascript "wrapper"
  makeHaskellCallback1 :: (JSVal -> IO JSVal) -> IO JSFunction
foreign import javascript "wrapper"
  makeHaskellCallback2 :: (JSVal -> JSVal -> IO JSVal) -> IO JSFunction

--

foreign import javascript "$1 + $2" js_str_concat :: JSString -> JSString -> JSString
