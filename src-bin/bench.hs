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





todoWithHooks :: MonadWidget t m => Event t (Map Int Todo.Task) ->  Event t (Map Int Todo.Task) -> Dynamic t Todo.Filter -> m (Event t Int)
todoWithHooks addTask removeTask activeFilter = do
  
    rec 
      tasks <- foldDyn ($) mempty $ mergeWith (.)
                    [ mappend <$> addTask 
                    , flip Map.difference <$> removeTask
                    ]
                    
      (listModifyTasks, itemsAdded) <- Todo.taskList activeFilter tasks
    return itemsAdded    
    
    
testAdd :: forall t m. MonadWidget t m => Int -> m (Event t ())
testAdd n = do
  rec
    tick <- todoWithHooks addTask never (constDyn Todo.All)
    
    postBuild <- getPostBuild
    counter <- updated <$> count (leftmost [void tick, postBuild])
        
    let addTask = newTask <$> (ffilter (< n)) counter
  return $ void $ ffilter (>= n) counter    
  
  where      
    newTask i = Map.singleton i (Todo.Task ("task " ++ show i) (odd i))
    

testFilter :: forall t m. MonadWidget t m => Int -> Int -> m (Event t ())
testFilter n repeats = do
  rec
    postBuild <- getPostBuild
    tick <- todoWithHooks (taskList <$ postBuild) never filter
        
    counter <- updated <$> count (void tick)
    filter <- holdDyn Todo.Active (filterNum <$> counter)
        
  return $ void $ ffilter (>= repeats) counter    
  
  where      
    taskList = Map.fromList $ map (\i -> (i,  Todo.Task ("task " ++ show i) True)) [1..n]
    filterNum i = if odd i then Todo.Active else Todo.Completed
    
    
testModify :: forall t m. MonadWidget t m => Int -> m (Event t ())
testModify n = do
  rec
    postBuild <- getPostBuild
    tick <- todoWithHooks addEvent never (constDyn Todo.All)
        
    counter <- updated <$> count (void tick)
    let addEvent = leftmost 
          [ taskList <$ postBuild
          , modifyTask <$> counter ]
        
  return $ void $ ffilter (>= n) counter    
  
  where      
    taskList = Map.fromList $ map (\i -> (i,  Todo.Task ("task " ++ show i) True)) [1..n]
    filterNum i = if odd i then Todo.Active else Todo.Completed    
    
    modifyTask i = Map.singleton i (Todo.Task ("task " ++ show i) False)


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
      , bench "add100" $ nfIO $ runTest (testAdd 100)
      , bench "modify100" $ nfIO $ runTest (testModify 100)

      , bench "filter100 x 50" $ nfIO $ runTest (testFilter 100 50)
      ]

    Dom.postGUIAsync $ exitWith ExitSuccess

  