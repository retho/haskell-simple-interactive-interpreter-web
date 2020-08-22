module JSLibs
  ( importJSModulesAndThen
  , markedjs
  ) where

import JSPrelude

import Data.Coerce (coerce)
import Control.Monad (void)

foreign import javascript "globalThis.marked($1)" markedjs :: JSString -> JSString -- https://github.com/markedjs/marked

importJSModulesAndThen :: IO () -> IO ()
importJSModulesAndThen io = do
  p <- import_markedjs
  void $ p `promiseThen` const (io >> promise_resolve js_null)


promiseThen :: Promise -> (JSVal -> IO Promise) -> IO Promise
promiseThen p f = makeHaskellCallback1 (coerce f) >>= \cb -> p `promise_then` cb


newtype Promise = Promise JSVal
foreign import javascript "null" js_null :: JSVal
foreign import javascript "Promise.resolve($1)" promise_resolve :: JSVal -> IO Promise
foreign import javascript "$1.then($2)" promise_then :: Promise -> JSFunction -> IO Promise
foreign import javascript "import('https://cdn.jsdelivr.net/npm/marked/marked.min.js')" import_markedjs :: IO Promise -- https://github.com/markedjs/marked
