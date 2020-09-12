module Main where

import JSPrelude
import JSLibs (importJSModulesAndThen, markedjs)
import Asterius.ByteString (byteStringToJSUint8Array)
import Data.ByteString (ByteString)
import Data.FileEmbed


description :: JSString
description = bytestring2jsstring $(embedFile "DESCRIPTION.md")

main :: IO ()
main = do
  importJSModulesAndThen $ do
    let el_desc = createElement "div" "description" $ markedjs description
    document_body_append_child el_desc
  document_body_append_child $ createElement "div" "hello" "Hello from Asterius"


bytestring2jsstring :: ByteString -> JSString
bytestring2jsstring = js_utf8_decode . byteStringToJSUint8Array

foreign import javascript "document.body.appendChild($1)" document_body_append_child :: JSHtmlElement -> IO ()
foreign import javascript "new TextDecoder('utf-8').decode($1)" js_utf8_decode :: JSUint8Array -> JSString


newtype JSHtmlElement = JSHtmlElement JSVal
createElement :: JSString -> JSString -> JSString -> JSHtmlElement
createElement = js_create_element

foreign import javascript "(() => {         \
\   const el = document.createElement($1);  \
\   el.setAttribute('id', $2);              \
\   el.innerHTML = $3;                      \
\   return el;                              \
\ })()"
  js_create_element :: JSString -> JSString -> JSString -> JSHtmlElement





foreign import javascript "$1   \
\   .replace(/&/g, '&amp;')     \
\   .replace(/</g, '&lt;')      \
\   .replace(/>/g, '&gt;')      \
\   .replace(/\"/g, '&quot;')   \
\   .replace(/'/g, '&#039;')    "
  js_escape_html :: JSString -> JSString

type TagName = JSString;
type Attribute = (JSString, JSString);
data HtmlElement = HtmlElement TagName [Attribute] [HtmlElement] | HtmlText JSString

text :: JSString -> HtmlElement
text = HtmlText . js_escape_html

div :: [] Attribute -> [] HtmlElement -> HtmlElement
div = HtmlElement "div"

stringifyHtml :: HtmlElement -> JSString
stringifyHtml (HtmlText x) = undefined
