module Main where

import Asterius.Types (JSString(..))

main :: IO ()
main = document_body_append "Hello from Asterius"

foreign import javascript "document.body.append($1)" document_body_append :: JSString -> IO ()
