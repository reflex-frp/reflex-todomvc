{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Reflex.IOS where
import Language.Javascript.JSaddle.WKWebView (jsaddleMain, WKWebView(..))
import Reflex.TodoMVC (main)

foreign export ccall webViewMain :: WKWebView -> IO ()

webViewMain :: WKWebView -> IO ()
webViewMain = jsaddleMain main
