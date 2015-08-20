{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
module Reflex.TodoMVC where

import Prelude hiding (mapM, mapM_, all, sequence)

import GHCJS.DOM.Element
import Control.Monad hiding (mapM, mapM_, forM, forM_, sequence)
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable
import Data.Monoid ((<>))
import Data.List (intercalate)
import Data.FileEmbed
import Control.Concurrent
import qualified Data.Text as T

import Reflex
import Reflex.Dom

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

data Task
   = Task { taskDescription :: String
          , taskCompleted :: Bool
          }
   deriving (Show, Eq)

-- | Add a new value to a map; automatically choose an unused key
insertNew_ :: (Enum k, Ord k) => v -> Map k v -> Map k v
insertNew_ v m = case Map.maxViewWithKey m of
  Nothing -> Map.singleton (toEnum 0) v
  Just ((k, _), _) -> Map.insert (succ k) v m

initialTasks :: Map Int Task
initialTasks = Map.empty

--------------------------------------------------------------------------------
-- Filters
--------------------------------------------------------------------------------

-- | Subsets of the task list that can be selected by the user
data Filter
   = All -- ^ All tasks
   | Active -- ^ Uncompleted tasks
   | Completed -- ^ Completed tasks
   deriving (Show, Eq)

-- | Determine whether this Task should be shown when this Filter is in effect
satisfiesFilter :: Filter -> Task -> Bool
satisfiesFilter f = case f of
  All -> const True
  Active -> not . taskCompleted
  Completed -> taskCompleted

--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") todoMVC

todoMVC :: MonadWidget t m => m ()
todoMVC = do
  el "div" $ do
    elAttr "section" ("class" =: "todoapp") $ do
      mainHeader
      rec tasks <- foldDyn ($) initialTasks $ mergeWith (.)
                     [ fmap insertNew_ newTask
                     , listModifyTasks
                     , fmap (const $ Map.filter $ not . taskCompleted) clearCompleted -- Call out the type and purpose of these things
                     ]
          newTask <- taskEntry
          (listModifyTasks, _) <- taskList activeFilter tasks
          (activeFilter, clearCompleted) <- controls tasks
      return ()
    infoFooter

-- | Display the main header
mainHeader :: MonadWidget t m => m ()
mainHeader = el "h1" $ text "todos"

-- | Display an input field; produce new Tasks when the user creates them
taskEntry :: MonadWidget t m => m (Event t Task)
taskEntry = do
  el "header" $ do
    -- Create the textbox; it will be cleared whenever the user presses enter
    rec let newValueEntered = ffilter (==keycodeEnter) (_textInput_keypress descriptionBox)
        descriptionBox <- textInput $ def & setValue .~ fmap (const "") newValueEntered
                                          & attributes .~ constDyn (mconcat [ "class" =: "new-todo"
                                                                            , "placeholder" =: "What needs to be done?"
                                                                            , "name" =: "newTodo"
                                                                            ])
    -- Request focus on this element when the widget is done being built
    schedulePostBuild $ liftIO $ elementFocus $ _textInput_element descriptionBox
    let -- | Get the current value of the textbox whenever the user hits enter
        newValue = tag (current $ _textInput_value descriptionBox) newValueEntered
        -- | Strip leading and trailing whitespace from the user's entry, and discard it if nothing remains
        stripDescription d = case T.unpack $ T.strip $ T.pack d of
          "" -> Nothing
          trimmed -> Just $ Task trimmed False
    -- Set focus when the user enters a new Task
    performEvent_ $ fmap (const $ liftIO $ elementFocus $ _textInput_element descriptionBox) newValueEntered
    return $ fmapMaybe stripDescription newValue

-- | Display the user's Tasks, subject to a Filter; return requested modifications to the Task list
taskList :: (MonadWidget t m, Ord k, Show k)
         => Dynamic t Filter
         -> Dynamic t (Map k Task)
         -> m (Event t (Map k Task -> Map k Task), Event t Int)
taskList activeFilter tasks = elAttr "section" ("class" =: "main") $ do
  -- Create "toggle all" button
  toggleAllState <- mapDyn (all taskCompleted . Map.elems) tasks
  toggleAllAttrs <- mapDyn (\t -> "class" =: "toggle-all" <> "name" =: "toggle" <> if Map.null t then "style" =: "visibility:hidden" else mempty) tasks
  toggleAll <- checkboxView toggleAllAttrs toggleAllState
  elAttr "label" ("for" =: "toggle-all") $ text "Mark all as complete"
  -- Filter the item list
  visibleTasks <- combineDyn (Map.filter . satisfiesFilter) activeFilter tasks
  -- Hide the item list itself if there are no items
  itemListAttrs <- forDyn visibleTasks $ \t -> mconcat
    [ "class" =: "todo-list"
    , if Map.null t then "style" =: "visibility:hidden" else mempty
    ]
  -- Display the items
  items <- elDynAttr "ul" itemListAttrs $ do
    list visibleTasks todoItem
  -- Aggregate the changes produced by the elements
  let combineItemChanges = fmap (foldl' (.) id) . mergeList . map (\(k, v) -> fmap (flip Map.update k) v) . Map.toList
  itemChangeEvent <- mapDyn combineItemChanges items
  let itemChanges = switch $ current itemChangeEvent
      -- Change all items' completed state when the toggleAll button is clicked
      toggleAllChanges = fmap (\oldAllCompletedState -> fmap (\t -> t { taskCompleted = not oldAllCompletedState })) $ tag (current toggleAllState) toggleAll
  return ( mergeWith (.) [ itemChanges
                         , toggleAllChanges
                         ] 
         , Map.size <$> updated items)

-- | Display an individual todo item
todoItem :: MonadWidget t m
         => Dynamic t Task
         -> m (Event t (Task -> Maybe Task))
todoItem todo = do
  description <- liftM nubDyn $ mapDyn taskDescription todo
  rec -- Construct the attributes for our element; use 
      attrs <- combineDyn (\t e -> "class" =: intercalate " " ((if taskCompleted t then ["completed"] else []) <> (if e then ["editing"] else []))) todo editing'
      (editing', changeTodo) <- elDynAttr "li" attrs $ do
        (setCompleted, destroy, startEditing) <- elAttr "div" ("class" =: "view") $ do
          -- Display the todo item's completed status, and allow it to be set
          completed <- liftM nubDyn $ mapDyn taskCompleted todo
          completedCheckbox <- checkboxView (constDyn $ "class" =: "toggle") completed
          let setCompleted = fmap not $ tag (current completed) completedCheckbox
          -- Display the todo item's name for viewing purposes
          (descriptionLabel, _) <- el' "label" $ dynText description
          -- Display the button for deleting the todo item
          (destroyButton, _) <- elAttr' "button" ("class" =: "destroy") $ return ()
          return (setCompleted, domEvent Click destroyButton, domEvent Dblclick descriptionLabel)
        -- Set the current value of the editBox whenever we start editing (it's not visible in non-editing mode)
        let setEditValue = tagDyn description $ ffilter id $ updated editing'
        editBox <- textInput $ def & setValue .~ setEditValue & attributes .~ constDyn ("class" =: "edit" <> "name" =: "title")
        let -- Set the todo item's description when the user leaves the textbox or presses enter in it
            setDescription = tag (current $ _textInput_value editBox) $ leftmost
              [ fmap (const ()) $ ffilter (==keycodeEnter) $ _textInput_keypress editBox
              , fmap (const ()) $ ffilter not $ updated $ _textInput_hasFocus editBox
              ]
            -- Cancel editing (without changing the item's description) when the user presses escape in the textbox
            cancelEdit = fmap (const ()) $ ffilter (==keycodeEscape) $ _textInput_keydown editBox
            -- Put together all the ways the todo item can change itself
            changeSelf = mergeWith (>=>) [ fmap (\c t -> Just $ t { taskCompleted = c }) setCompleted
                                         , fmap (const $ const Nothing) destroy
                                         , fmap (\d t -> case T.unpack $ T.strip $ T.pack d of { "" -> Nothing ; trimmed -> Just $ t { taskDescription = trimmed } }) setDescription
                                         ]
        -- Set focus on the edit box when we enter edit mode
        postGui <- askPostGui
        performEvent_ $ fmap (const $ liftIO $ void $ forkIO $ threadDelay 1000 >> postGui (liftIO $ elementFocus $ _textInput_element editBox)) startEditing -- Without the delay, the focus doesn't take effect because the element hasn't become unhidden yet; we need to use postGui to ensure that this is threadsafe when built with GTK
        -- Determine the current editing state; initially false, but can be modified by various events
        editing <- holdDyn False $ leftmost [ fmap (const True) startEditing
                                            , fmap (const False) setDescription
                                            , fmap (const False) cancelEdit
                                            ]
        return (editing, changeSelf)
  -- Return an event that fires whenever we change ourselves
  return changeTodo

-- | Display the control footer; return the user's currently-selected filter and an event that fires when the user chooses to clear all completed events
controls :: MonadWidget t m => Dynamic t (Map k Task) -> m (Dynamic t Filter, Event t ())
controls tasks = do
  -- Determine the attributes for the footer; it is invisible when there are no todo items
  controlsAttrs <- mapDyn (\t -> "class" =: "footer" <> if Map.null t then "style" =: "visibility:hidden" else mempty) tasks
  elDynAttr "footer" controlsAttrs $ do
    -- Compute the number of completed and uncompleted tasks
    (tasksCompleted, tasksLeft) <- splitDyn <=< forDyn tasks $ \m ->
      let completed = Map.size $ Map.filter taskCompleted m
      in (completed, Map.size m - completed)
    elAttr "span" ("class" =: "todo-count") $ do
      el "strong" $ dynText =<< mapDyn show tasksLeft
      dynText =<< mapDyn (\n -> (if n == 1 then " item" else " items") <> " left") tasksLeft
    activeFilter <- elAttr "ul" ("class" =: "filters") $ do
      rec activeFilter <- holdDyn All setFilter
          let filterButton f = el "li" $ do
                buttonAttrs <- mapDyn (\af -> "class" =: (if f == af then "selected" else "")) activeFilter
                (e, _) <- elDynAttr' "a" buttonAttrs $ text $ show f
                return $ fmap (const f) (domEvent Click e)
          allButton <- filterButton All
          text " "
          activeButton <- filterButton Active
          text " "
          completedButton <- filterButton Completed
          let setFilter = leftmost [allButton, activeButton, completedButton]
      return activeFilter
    clearCompletedAttrs <- flip mapDyn tasksCompleted $ \n -> mconcat
      [ "class" =: "clear-completed"
      , if n > 0 then mempty else "hidden" =: ""
      ]
    (clearCompletedAttrsButton, _) <- elDynAttr' "button" clearCompletedAttrs $ dynText =<< mapDyn (\n -> "Clear completed (" <> show n <> ")") tasksCompleted
    return (activeFilter, domEvent Click clearCompletedAttrsButton)

-- | Display static information about the application
infoFooter :: MonadWidget t m => m ()
infoFooter = do
  elAttr "footer" ("class" =: "info") $ do
    el "p" $ text "Click to edit a todo"
    el "p" $ do
      text "Written by "
      elAttr "a" ("href" =: "https://github.com/ryantrinkle") $ text "Ryan Trinkle"
    el "p" $ do
      text "Part of "
      elAttr "a" ("href" =: "http://todomvc.com") $ text "TodoMVC"
