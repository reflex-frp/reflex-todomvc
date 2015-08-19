{-# LANGUAGE TemplateHaskell, RankNTypes, ScopedTypeVariables #-}

import qualified Reflex.TodoMVC as Todo

import Reflex.Dom
import Reflex.Host.Class

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Ref

import Data.Dependent.Sum

import Control.Concurrent

import Data.Map (Map)
import qualified Data.Map as Map

import Data.FileEmbed
import Criterion
import Criterion.Main

import Reflex
import Reflex.Dom

import System.Exit


import qualified GHCJS.DOM as Dom

main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") benchmarks


benchmarks :: forall t m. MonadWidget t m => m ()
benchmarks = do
  (testEvent, triggerRef) <- newEventWithTriggerRef

  done <- switchPromptlyDyn <$> widgetHold (return never) testEvent
  doneVar <- liftIO $ newEmptyMVar
  
  performEvent_ $ liftIO . putMVar doneVar <$> done
      
  postGui <- askPostGui
  runWithActions <- askRunWithActions      
             
  let 
    runTest :: m (Event t ()) -> IO ()
    runTest test = do
      postGui $ mapM_ (\t -> runWithActions [t :=> test]) =<< readRef triggerRef
      void $ takeMVar doneVar


  void $ liftIO $ forkIO $ do 
    defaultMain 
      [ bench "creation" $ nfIO $ runTest (Todo.todoMVC >> getPostBuild)
      ]
    
    Dom.postGUIAsync $ exitWith ExitSuccess

  