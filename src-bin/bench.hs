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

import System.Exit


import qualified GHCJS.DOM as Dom

main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") benchmarks


type TaskMap = Map Int Todo.Task

initialTasks :: Int -> TaskMap
initialTasks n = Map.fromList $ map (\i -> (i,  Todo.Task ("task " ++ show i) True)) [1..n]


todoWithHooks :: MonadWidget t m => TaskMap -> Event t TaskMap ->  Event t TaskMap -> Dynamic t Todo.Filter -> m (Event t Int)
todoWithHooks initial addTask removeTask activeFilter = do
  
    rec 
      tasks <- foldDyn ($) initial $ mergeWith (.)
                    [ mappend <$> addTask 
                    , flip Map.difference <$> removeTask
                    , listModifyTasks
                    ]
                    
      (listModifyTasks, itemsAdded) <- Todo.taskList activeFilter tasks
      counter <- updated <$> count (void itemsAdded)
    return counter    
    
    
testAdd :: forall t m. MonadWidget t m => Int -> m (Event t ())
testAdd n = do
  rec
    counter <- todoWithHooks mempty addTask never (constDyn Todo.All)
        
    let addTask = newTask <$> (ffilter (< n)) counter
  return $ void $ ffilter (>= n) counter    
  
  where      
    newTask i = Map.singleton i (Todo.Task ("task " ++ show i) (odd i))
    

testFilter :: forall t m. MonadWidget t m => Int -> Int -> m (Event t ())
testFilter n repeats = do
  rec
    counter <- todoWithHooks (initialTasks n) never never filter
    filter <- holdDyn Todo.Active (filterNum <$> counter)
  return $ void $ ffilter (>= repeats) counter    
  
  where      
    filterNum i = if odd i then Todo.Active else Todo.Completed

testCreate :: forall t m. MonadWidget t m => Int -> m (Event t ())
testCreate n = void <$> todoWithHooks (initialTasks n) never never (constDyn Todo.All)
    
    
testModify :: forall t m. MonadWidget t m => Int -> m (Event t ())
testModify n = do
  rec
    counter <- todoWithHooks (initialTasks n) (modifyTask <$> counter) never (constDyn Todo.All)
  return $ void $ ffilter (>= n) counter    
  where      
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

    benchN n = bgroup ("n = " ++ show n) $
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
      
    Dom.postGUIAsync $ exitWith ExitSuccess
  