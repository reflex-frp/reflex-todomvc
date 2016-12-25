{-# LANGUAGE TemplateHaskell, OverloadedStrings, RankNTypes, ScopedTypeVariables, RecursiveDo, TypeFamilies, TypeOperators #-}

import qualified Reflex.TodoMVC as Todo

import Reflex.Dom
import Reflex.Dom.Internal.Foreign
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate
import Reflex.Dom.PerformEvent.Class
import Reflex.Dom.PerformEvent.Base
import Reflex.Dom.PostBuild.Class
import Reflex.Host.Class

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Node as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.HTMLElement as DOM
import qualified GHCJS.DOM.HTMLDocument as DOM

import Control.Monad 
import Control.Monad.IO.Class
import Control.Monad.Ref
import Control.Monad.Fix
import Control.Monad.Identity
import Data.Monoid
import qualified Data.Text as T
import Data.IORef
import Data.Functor.Compose
import Data.Maybe
import Data.Text.Encoding
import Text.Read
import System.Environment

import Data.Dependent.Sum

import Control.Concurrent

import Data.FileEmbed
import Data.Map (Map)
import qualified Data.Map as Map

import Criterion
import Criterion.Main

import System.Exit


type TaskMap = Map Int Todo.Task

initialTasks :: Int -> TaskMap
initialTasks n = Map.fromList $ map (\i -> (i,  Todo.Task ("task " <> T.pack (show i)) (even i))) [1..n]


todoWithHooks :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => TaskMap -> Event t TaskMap ->  Event t TaskMap -> Dynamic t Todo.Filter -> m ()
todoWithHooks initial addTask removeTask activeFilter = do
  
    rec 
      {- tasks <- foldDyn ($) initial $ mergeWith (.)
                    [ mappend <$> addTask 
                    , flip Map.difference <$> removeTask
                    , listModifyTasks
                    ] -}
                    
      (tasks, _) <- Todo.taskList activeFilter initial $ fmap (fmap Just) addTask
    return ()
    
    
main :: IO ()
main = do
  [n] <- getArgs
  Just numIterations <- return $ readMaybe n
  runWebGUI $ \webView -> withWebViewSingleton webView $ \webViewSing -> do
    Just doc <- liftM (fmap DOM.castToHTMLDocument) $ DOM.webViewGetDomDocument webView
    Just rootElement <- DOM.getBody doc
    Just headElement <- liftM (fmap DOM.castToHTMLElement) $ DOM.getHead doc
    DOM.setInnerHTML headElement . Just $ "<style>" <> T.unpack (decodeUtf8 $(embedFile "style.css")) <> "</style>" --TODO: Fix this
    (addTriggerRef, FireCommand fire) <- attachWidget' rootElement undefined $ do
      (add, addTriggerRef) <- newEventWithTriggerRef
      todoWithHooks mempty add never $ constDyn Todo.All
      return addTriggerRef
    runSpiderHost $ forM_ [1..numIterations] $ \i -> do
      Just t <- liftIO $ readIORef addTriggerRef
      fire [t :=> Identity (Map.singleton i (Todo.Task ("task " <> T.pack (show i)) (even i)))] $ return ()
    quitWebView webView
    return ()

{-
benchmarks :: forall t m m'. (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, TriggerEvent t m, MonadIO m, PerformEvent t m' m, MonadIO m') => m ()
benchmarks = do
  (testEvent, testEventTriggerRef) <- newEventWithTriggerRef

  setupComplete <- switchPromptlyDyn <$> widgetHold (return never) testEvent
  doneVar <- liftIO $ newEmptyMVar
  
  performEvent_ $ liftIO . putMVar doneVar <$> setupComplete
                   
  let 
    runTest :: m (Event t ()) -> IO ()
    runTest test = do
      fireTest test
      void $ takeMVar doneVar

    benchN n = bgroup ("n = " <> show n) $
      [ bench "create items" $ nfIO $ runTest (testCreate n)      
      , bench "add items (incremental)" $ nfIO $ runTest (testAdd n)
      , bench "modify items (incremental)" $ nfIO $ runTest (testModify n)
      , bench "toggle filter (x10)" $ nfIO $ runTest (testFilter n 10)
      ]

  void $ liftIO $ forkIO $ do 
    defaultMain 
      [ bench "creation" $ nfIO $ runTest (Todo.todoMVC >> getPostBuild)
      , benchN 25
      , benchN 50
      ]
      
    DOM.postGUISync $ exitWith ExitSuccess
  
-}
