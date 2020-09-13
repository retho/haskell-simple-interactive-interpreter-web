module Main where

import Prelude hiding (div)
import JSPrelude
import JSLibs (importJSModulesAndThen, markedjs)
import Asterius.ByteString (byteStringToJSUint8Array)
import Data.ByteString (ByteString)
import Data.FileEmbed
import Data.Coerce (coerce)
import Data.Maybe (fromJust)
import Control.Monad (when)

description :: JSString
description = bytestring2jsstring $(embedFile "DESCRIPTION.md")

css :: JSString
css = bytestring2jsstring $(embedFile "src-web/styles.css")

main :: IO ()
main = do
  importJSModulesAndThen $ do
    let el_desc = createElement "div" "description" $ markedjs description
    document_body_append_child el_desc
  document_head_append_child $ createElement "style" "css" css
  document_body_append_child $ createElement "div" "root" $ stringifyHtml layout
  wrp <- getElementById "console-input-wrapper" >>= pure . fromJust
  inp <- getElementById "console-input" >>= pure . fromJust
  let
    handlekeypress :: JSKeyEvent -> IO JSVal
    handlekeypress ev
      | isEnterPressed ev = do
        val <- getInputValue inp
        when (jsstr_length (js_trim val) > 0) $ do
          insert_before_node wrp $ createElement "div" "" $ stringifyHtml $ renderConsoleInput val True Nothing
        setInputValue inp ""
        pure js_null
      | otherwise = pure js_null
  callback <- makeHaskellCallback1 $ handlekeypress . coerce
  addEventListener inp "keyup" callback

layout :: HtmlElement
layout = div []
  [ div' [("class", "app-header")] $ text "Simple Interactive Interpreter"
  , div [("class", "app-console")]
    [ div [("class", "app-console__container")]
      [ div' [("class", "app-console-tip")] $ text "Type expressions in here."
      , div' [("id", "console-input-wrapper")] $ renderConsoleInput "" False (pure "console-input")
      ]
    , div [("class", "app-console__container")]
      [ div' [("class", "app-console-tip")] $ text "Some tips" ]
    ]
  ]

getElementById :: JSString -> IO (Maybe JSHtmlElement)
getElementById attr_id = get_element_by_id attr_id >>= \val ->
    if is_null_or_undefined val
      then pure $ Nothing
      else pure . pure $ coerce val

newtype JSKeyEvent = JSKeyEvent JSVal

foreign import javascript "null" js_null :: JSVal
foreign import javascript "$1 == null" is_null_or_undefined :: JSVal -> Bool
foreign import javascript "document.getElementById($1)" get_element_by_id :: JSString -> IO JSVal

foreign import javascript "$1.keyCode === 13" isEnterPressed :: JSKeyEvent -> Bool
foreign import javascript "$1.value" getInputValue :: JSHtmlElement -> IO JSString
foreign import javascript "console.log($1)" console_log :: JSString -> IO ()
foreign import javascript "$1.addEventListener($2, $3)" addEventListener :: JSHtmlElement -> JSString -> JSFunction -> IO ()
foreign import javascript "$1.value = $2" setInputValue :: JSHtmlElement -> JSString -> IO ()

renderConsoleInput :: JSString -> Bool -> Maybe JSString -> HtmlElement
renderConsoleInput value disabled attr_id = div [("class", "app-console-input")]
  [ div' [("class", "app-console-input__label")] $ text "λ\xa0"
  , input
    $ [("class", "app-console-input__input"), ("value", value)]
    <> if disabled then pure ("disabled", "true") else []
    <> maybe [] (pure . ("id",)) attr_id
  ]

bytestring2jsstring :: ByteString -> JSString
bytestring2jsstring = js_utf8_decode . byteStringToJSUint8Array

foreign import javascript "document.body.appendChild($1)" document_body_append_child :: JSHtmlElement -> IO ()
foreign import javascript "document.head.appendChild($1)" document_head_append_child :: JSHtmlElement -> IO ()
foreign import javascript "($1.parentNode).insertBefore($2, $1)" insert_before_node :: JSHtmlElement -> JSHtmlElement -> IO ()
foreign import javascript "new TextDecoder('utf-8').decode($1)" js_utf8_decode :: JSUint8Array -> JSString


newtype JSHtmlElement = JSHtmlElement JSVal
createElement :: JSString -> JSString -> JSString -> JSHtmlElement
createElement = js_create_element

foreign import javascript "(() => {         \
\   const el = document.createElement($1);  \
\   if ($2) el.setAttribute('id', $2);      \
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
text = HtmlText

div :: [] Attribute -> [] HtmlElement -> HtmlElement
div = HtmlElement "div"

div' :: [] Attribute -> HtmlElement -> HtmlElement
div' attrs = HtmlElement "div" attrs . pure

input :: [] Attribute -> HtmlElement
input attrs = HtmlElement "input" attrs []

stringifyHtml :: HtmlElement -> JSString
stringifyHtml (HtmlText x) = js_escape_html x
stringifyHtml (HtmlElement tag attrs children)
  = "<" <> tag <> " " <> joinStr (flip map attrs $ \(k, v) -> k <> "='" <> v <> "'") " " <> ">"
  <> joinStr (map stringifyHtml children) ""
  <> "</" <> tag <> ">"

joinStr :: [] JSString -> JSString -> JSString
joinStr [] _ = ""
joinStr (x:[]) _ = x
joinStr (x:xs) sep = x <> sep <> joinStr xs sep

foreign import javascript "$1.trim()" js_trim :: JSString -> JSString
foreign import javascript "$1.length" jsstr_length :: JSString -> Int
