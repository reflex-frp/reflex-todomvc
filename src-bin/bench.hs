{-# LANGUAGE TemplateHaskell, RankNTypes, ScopedTypeVariables, RecursiveDo #-}

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



todoWithHooks :: MonadWidget t m => Dynamic t Todo.Filter -> m (Map Int Todo.Task -> IO (), Map Int Todo.Task -> IO ())
todoWithHooks activeFilter = do
  
    (removeTask, fireRemove) <- newEventWithFire
    (addTask, fireAdd) <- newEventWithFire

    rec 
      tasks <- foldDyn ($) mempty $ mergeWith (.)
                    [ mappend <$> addTask 
                    , flip Map.difference <$> removeTask
                    , listModifyTasks
                    ]
      listModifyTasks <- Todo.taskList activeFilter tasks
        
    return (fireAdd, fireRemove)

newEventWithFire :: MonadWidget t m =>  m (Event t a, a -> IO ())
newEventWithFire =  do
  
  postGui <- askPostGui
  runWithActions <- askRunWithActions  
  
  (e, triggerRef) <- newEventWithTriggerRef
  return (e, \a -> postGui $ mapM_ (\t -> runWithActions [t :=> a]) =<< readRef triggerRef)



benchmarks :: forall t m. MonadWidget t m => m ()
benchmarks = do
  (testEvent, fireTest) <- newEventWithFire

  setupComplete <- switchPromptlyDyn <$> widgetHold (return never) testEvent
  doneVar <- liftIO $ newEmptyMVar
  
  performEvent_ $ liftIO . putMVar doneVar <$> setupComplete
                   
  let 
    runTest :: m (Event t ()) -> IO ()
    runTest test = do
      fireTest test
      void $ takeMVar doneVar


  void $ liftIO $ forkIO $ do 
    defaultMain 
      [ bench "creation" $ nfIO $ runTest (Todo.todoMVC >> getPostBuild)
      ]
    
    Dom.postGUIAsync $ exitWith ExitSuccess

  