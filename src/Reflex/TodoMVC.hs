{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds #-}
module Reflex.TodoMVC where

import Prelude hiding (mapM, mapM_, all, sequence)

import Control.Monad hiding (mapM, mapM_, forM, forM_, sequence)
import Control.Monad.Fix
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import GHCJS.DOM.Types (JSM)

import Reflex
import Reflex.Dom.Core
import Data.Text.Encoding (encodeUtf8)

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

data Task
   = Task { taskDescription :: Text
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

main :: JSM ()
main = mainWidgetWithCss (encodeUtf8 css) todoMVC

todoMVC :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
        => m ()
todoMVC = el "div" $ do
  elAttr "section" ("class" =: "todoapp") $ do
    mainHeader
    rec tasks <- foldDyn ($) initialTasks $ mergeWith (.)
                   [ fmap insertNew_ newTask
                   , listModifyTasks
                   , fmap (const $ Map.filter $ not . taskCompleted) clearCompleted -- Call out the type and purpose of these things
                   ]
        newTask <- taskEntry
        listModifyTasks <- taskList activeFilter tasks
        (activeFilter, clearCompleted) <- controls tasks
    return ()
  infoFooter

-- | Display the main header
mainHeader :: DomBuilder t m => m ()
mainHeader = el "h1" $ text "todos"

-- | Strip leading and trailing whitespace from the user's entry, and discard it if nothing remains
stripDescription :: Text -> Maybe Text
stripDescription d =
  let trimmed = T.strip d
  in if T.null trimmed
     then Nothing
     else Just trimmed

keyCodeIs :: Key -> KeyCode -> Bool
keyCodeIs k c = keyCodeLookup c == k

-- | Display an input field; produce new Tasks when the user creates them
taskEntry :: (DomBuilder t m, MonadFix m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace) => m (Event t Task)
taskEntry = el "header" $ do
    -- Create the textbox; it will be cleared whenever the user presses enter
    rec let newValueEntered = ffilter (keyCodeIs Enter . fromIntegral) (_textInput_keypress descriptionBox)
        descriptionBox <- textInput $ def
          & textInputConfig_setValue .~ fmap (const "") newValueEntered
          & textInputConfig_attributes .~ constDyn (mconcat [ "class" =: "new-todo"
                                                            , "placeholder" =: "What needs to be done?"
                                                            , "name" =: "newTodo"
                                                            ])
    -- Request focus on this element when the widget is done being built
    -- schedulePostBuild $ liftIO $ focus $ _textInput_element descriptionBox
    let -- | Get the current value of the textbox whenever the user hits enter
        newValue = tag (current $ _textInput_value descriptionBox) newValueEntered
    -- Set focus when the user enters a new Task
    -- performEvent_ $ fmap (const $ liftIO $ focus $ _textInput_element descriptionBox) newValueEntered
    return $ fmap (\d -> Task d False) $ fmapMaybe stripDescription newValue

-- | Display the user's Tasks, subject to a Filter; return requested modifications to the Task list
taskList :: ( DomBuilder t m
            , DomBuilderSpace m ~ GhcjsDomSpace
            , PostBuild t m
            , MonadHold t m
            , MonadFix m
            , Ord k
            )
         => Dynamic t Filter
         -> Dynamic t (Map k Task)
         -> m (Event t (Map k Task -> Map k Task))
taskList activeFilter tasks = elAttr "section" ("class" =: "main") $ do
  -- Create "toggle all" button
  let toggleAllState = all taskCompleted . Map.elems <$> tasks
      toggleAllAttrs = ffor tasks $ \t -> "class" =: "toggle-all" <> "name" =: "toggle" <> if Map.null t then "style" =: "visibility:hidden" else mempty
  toggleAll <- checkboxView toggleAllAttrs toggleAllState
  elAttr "label" ("for" =: "toggle-all") $ text "Mark all as complete"
  -- Filter the item list
  let visibleTasks = zipDynWith (Map.filter . satisfiesFilter) activeFilter tasks
  -- Hide the item list itself if there are no items
  let itemListAttrs = ffor visibleTasks $ \t -> mconcat
        [ "class" =: "todo-list"
        , if Map.null t then "style" =: "visibility:hidden" else mempty
        ]
  -- Display the items
  items <- elDynAttr "ul" itemListAttrs (list visibleTasks todoItem)
  -- Aggregate the changes produced by the elements
  let combineItemChanges = fmap (foldl' (.) id) . mergeList . map (\(k, v) -> fmap (flip Map.update k) v) . Map.toList
      itemChangeEvent = fmap combineItemChanges items
      itemChanges = switch $ current itemChangeEvent
      -- Change all items' completed state when the toggleAll button is clicked
      toggleAllChanges = fmap (\oldAllCompletedState -> fmap (\t -> t { taskCompleted = not oldAllCompletedState })) $ tag (current toggleAllState) toggleAll
  return $ mergeWith (.) [ itemChanges
                         , toggleAllChanges
                         ]

buildCompletedCheckbox
  :: ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t Task
  -> Dynamic t Text
  -> m (Event t Bool, Event t (), Event t ())
buildCompletedCheckbox todo description = elAttr "div" ("class" =: "view") $ do
  -- Display the todo item's completed status, and allow it to be set
  completed <- holdUniqDyn $ fmap taskCompleted todo
  completedCheckbox <- checkboxView (constDyn $ "class" =: "toggle") completed
  let setCompleted = fmap not $ tag (current completed) completedCheckbox
  -- Display the todo item's name for viewing purposes
  (descriptionLabel, _) <- el' "label" $ dynText description
  -- Display the button for deleting the todo item
  (destroyButton, _) <- elAttr' "button" ("class" =: "destroy") $ return ()
  return ( setCompleted
         , domEvent Click destroyButton
         , void $ domEvent Dblclick descriptionLabel
         )

-- | Display an individual todo item
todoItem :: ( DomBuilder t m
            , DomBuilderSpace m ~ GhcjsDomSpace
            , MonadFix m
            , MonadHold t m
            , PostBuild t m
            )
         => Dynamic t Task
         -> m (Event t (Task -> Maybe Task))
todoItem todo = do
  description <- holdUniqDyn $ fmap taskDescription todo
  rec -- Construct the attributes for our element
      let attrs = ffor2 todo editing' $ \t e -> "class" =: T.unwords
            [ cls
            | (cls, use) <- [("completed", taskCompleted t), ("editing", e)]
            , use
            ]
      (editing', changeTodo) <- elDynAttr "li" attrs $ do
        (setCompleted, destroy, startEditing) <- buildCompletedCheckbox todo description
        -- Set the current value of the editBox whenever we start editing (it's not visible in non-editing mode)
        let setEditValue = tag (current description) $ ffilter id $ updated editing'
        editBox <- textInput $ def
          & textInputConfig_setValue .~ setEditValue
          & textInputConfig_attributes .~ constDyn ("class" =: "edit" <> "name" =: "title")
        let -- Set the todo item's description when the user leaves the textbox or presses enter in it
            setDescription = tag (current $ _textInput_value editBox) $ leftmost
              [ void $ ffilter (keyCodeIs Enter . fromIntegral) $ _textInput_keypress editBox
              , void $ ffilter not $ updated $ _textInput_hasFocus editBox
              ]
            -- Cancel editing (without changing the item's description) when the user presses escape in the textbox
            cancelEdit = void $ ffilter (keyCodeIs Escape . fromIntegral) $ _textInput_keydown editBox
            -- Put together all the ways the todo item can change itself
            changeSelf = mergeWith (>=>) [ fmap (\c t -> Just $ t { taskCompleted = c }) setCompleted
                                         , fmap (const $ const Nothing) destroy
                                         , fmap (\d t -> fmap (\trimmed -> t { taskDescription = trimmed }) $ stripDescription d) setDescription
                                         ]
        -- Set focus on the edit box when we enter edit mode
--        postGui <- askPostGui
--        performEvent_ $ fmap (const $ liftIO $ void $ forkIO $ threadDelay 1000 >> postGui (liftIO $ focus $ _textInput_element editBox)) startEditing -- Without the delay, the focus doesn't take effect because the element hasn't become unhidden yet; we need to use postGui to ensure that this is threadsafe when built with GTK
        -- Determine the current editing state; initially false, but can be modified by various events
        editing <- holdDyn False $ leftmost [ fmap (const True) startEditing
                                            , fmap (const False) setDescription
                                            , fmap (const False) cancelEdit
                                            ]
        return (editing, changeSelf)
  -- Return an event that fires whenever we change ourselves
  return changeTodo

buildActiveFilter :: MonadWidget t m => m (Dynamic t Filter)
buildActiveFilter = elAttr "ul" ("class" =: "filters") $ do
  rec activeFilter <- holdDyn All setFilter
      let filterButton f = el "li" $ do
            let buttonAttrs = ffor activeFilter $ \af -> "class" =: if f == af then "selected" else ""
            (e, _) <- elDynAttr' "a" buttonAttrs $ text $ T.pack $ show f
            return $ fmap (const f) (domEvent Click e)
      allButton <- filterButton All
      text " "
      activeButton <- filterButton Active
      text " "
      completedButton <- filterButton Completed
      let setFilter = leftmost [allButton, activeButton, completedButton]
  return activeFilter

-- | Display the control footer; return the user's currently-selected filter and an event that fires when the user chooses to clear all completed events
controls :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Dynamic t (Map k Task) -> m (Dynamic t Filter, Event t ())
controls tasks = do
  -- Determine the attributes for the footer; it is invisible when there are no todo items
  let controlsAttrs = ffor tasks $ \t -> "class" =: "footer" <> if Map.null t then "style" =: "visibility:hidden" else mempty
  elDynAttr "footer" controlsAttrs $ do
    -- Compute the number of completed and uncompleted tasks
    let (tasksCompleted, tasksLeft) = splitDynPure $ ffor tasks $ \m ->
          let completed = Map.size $ Map.filter taskCompleted m
          in (completed, Map.size m - completed)
    elAttr "span" ("class" =: "todo-count") $ do
      el "strong" $ dynText $ fmap (T.pack . show) tasksLeft
      dynText $ fmap (\n -> (if n == 1 then " item" else " items") <> " left") tasksLeft
    activeFilter <- buildActiveFilter
    let clearCompletedAttrs = ffor tasksCompleted $ \n -> mconcat
          [ "class" =: "clear-completed"
          , if n > 0 then mempty else "hidden" =: ""
          ]
    (clearCompletedAttrsButton, _) <- elDynAttr' "button" clearCompletedAttrs $ dynText $ ffor tasksCompleted $ \n -> "Clear completed (" <> T.pack (show n) <> ")"
    return (activeFilter, domEvent Click clearCompletedAttrsButton)

-- | Display static information about the application
infoFooter :: DomBuilder t m => m ()
infoFooter = elAttr "footer" ("class" =: "info") $ do
  el "p" $ text "Click to edit a todo"
  el "p" $ do
    text "Written by "
    elAttr "a" ("href" =: "https://github.com/ryantrinkle") $ text "Ryan Trinkle"
  el "p" $ do
    text "Part of "
    elAttr "a" ("href" =: "http://todomvc.com") $ text "TodoMVC"

css :: Text
css = "\
    \html,\
    \body {\
    \\tmargin: 0;\
    \\tpadding: 0;\
    \}\
    \\
    \button {\
    \\tmargin: 0;\
    \\tpadding: 0;\
    \\tborder: 0;\
    \\tbackground: none;\
    \\tfont-size: 100%;\
    \\tvertical-align: baseline;\
    \\tfont-family: inherit;\
    \\tfont-weight: inherit;\
    \\tcolor: inherit;\
    \\t-webkit-appearance: none;\
    \\tappearance: none;\
    \\t-webkit-font-smoothing: antialiased;\
    \\t-moz-osx-font-smoothing: grayscale;\
    \}\
    \\
    \body {\
    \\tfont: 14px 'Helvetica Neue', Helvetica, Arial, sans-serif;\
    \\tline-height: 1.4em;\
    \\tbackground: #f5f5f5;\
    \\tcolor: #111111;\
    \\tmin-width: 230px;\
    \\tmax-width: 550px;\
    \\tmargin: 0 auto;\
    \\t-webkit-font-smoothing: antialiased;\
    \\t-moz-osx-font-smoothing: grayscale;\
    \\tfont-weight: 300;\
    \}\
    \\
    \:focus {\
    \\toutline: 0;\
    \}\
    \\
    \.hidden {\
    \\tdisplay: none;\
    \}\
    \\
    \.todoapp {\
    \\tbackground: #fff;\
    \\tmargin: 130px 0 40px 0;\
    \\tposition: relative;\
    \\tbox-shadow: 0 2px 4px 0 rgba(0, 0, 0, 0.2),\
    \\t            0 25px 50px 0 rgba(0, 0, 0, 0.1);\
    \}\
    \\
    \.todoapp input::-webkit-input-placeholder {\
    \\tfont-style: italic;\
    \\tfont-weight: 300;\
    \\tcolor: rgba(0, 0, 0, 0.4);\
    \}\
    \\
    \.todoapp input::-moz-placeholder {\
    \\tfont-style: italic;\
    \\tfont-weight: 300;\
    \\tcolor: rgba(0, 0, 0, 0.4);\
    \}\
    \\
    \.todoapp input::input-placeholder {\
    \\tfont-style: italic;\
    \\tfont-weight: 300;\
    \\tcolor: rgba(0, 0, 0, 0.4);\
    \}\
    \\
    \.todoapp h1 {\
    \\tposition: absolute;\
    \\ttop: -140px;\
    \\twidth: 100%;\
    \\tfont-size: 80px;\
    \\tfont-weight: 200;\
    \\ttext-align: center;\
    \\tcolor: #b83f45;\
    \\t-webkit-text-rendering: optimizeLegibility;\
    \\t-moz-text-rendering: optimizeLegibility;\
    \\ttext-rendering: optimizeLegibility;\
    \}\
    \\
    \.new-todo,\
    \.edit {\
    \\tposition: relative;\
    \\tmargin: 0;\
    \\twidth: 100%;\
    \\tfont-size: 24px;\
    \\tfont-family: inherit;\
    \\tfont-weight: inherit;\
    \\tline-height: 1.4em;\
    \\tcolor: inherit;\
    \\tpadding: 6px;\
    \\tborder: 1px solid #999;\
    \\tbox-shadow: inset 0 -1px 5px 0 rgba(0, 0, 0, 0.2);\
    \\tbox-sizing: border-box;\
    \\t-webkit-font-smoothing: antialiased;\
    \\t-moz-osx-font-smoothing: grayscale;\
    \}\
    \\
    \.new-todo {\
    \\tpadding: 16px 16px 16px 60px;\
    \\tborder: none;\
    \\tbackground: rgba(0, 0, 0, 0.003);\
    \\tbox-shadow: inset 0 -2px 1px rgba(0,0,0,0.03);\
    \}\
    \\
    \.main {\
    \\tposition: relative;\
    \\tz-index: 2;\
    \\tborder-top: 1px solid #e6e6e6;\
    \}\
    \\
    \.toggle-all {\
    \\twidth: 1px;\
    \\theight: 1px;\
    \\tborder: none; /* Mobile Safari */\
    \\topacity: 0;\
    \\tposition: absolute;\
    \\tright: 100%;\
    \\tbottom: 100%;\
    \}\
    \\
    \.toggle-all + label {\
    \\twidth: 60px;\
    \\theight: 34px;\
    \\tfont-size: 0;\
    \\tposition: absolute;\
    \\ttop: -52px;\
    \\tleft: -13px;\
    \\t-webkit-transform: rotate(90deg);\
    \\ttransform: rotate(90deg);\
    \}\
    \\
    \.toggle-all + label:before {\
    \\tcontent: '❯';\
    \\tfont-size: 22px;\
    \\tcolor: #e6e6e6;\
    \\tpadding: 10px 27px 10px 27px;\
    \}\
    \\
    \.toggle-all:checked + label:before {\
    \\tcolor: #737373;\
    \}\
    \\
    \.todo-list {\
    \\tmargin: 0;\
    \\tpadding: 0;\
    \\tlist-style: none;\
    \}\
    \\
    \.todo-list li {\
    \\tposition: relative;\
    \\tfont-size: 24px;\
    \\tborder-bottom: 1px solid #ededed;\
    \}\
    \\
    \.todo-list li:last-child {\
    \\tborder-bottom: none;\
    \}\
    \\
    \.todo-list li.editing {\
    \\tborder-bottom: none;\
    \\tpadding: 0;\
    \}\
    \\
    \.todo-list li.editing .edit {\
    \\tdisplay: block;\
    \\twidth: calc(100% - 43px);\
    \\tpadding: 12px 16px;\
    \\tmargin: 0 0 0 43px;\
    \}\
    \\
    \.todo-list li.editing .view {\
    \\tdisplay: none;\
    \}\
    \\
    \.todo-list li .toggle {\
    \\ttext-align: center;\
    \\twidth: 40px;\
    \\t/* auto, since non-WebKit browsers doesn't support input styling */\
    \\theight: auto;\
    \\tposition: absolute;\
    \\ttop: 0;\
    \\tbottom: 0;\
    \\tmargin: auto 0;\
    \\tborder: none; /* Mobile Safari */\
    \\t-webkit-appearance: none;\
    \\tappearance: none;\
    \}\
    \\
    \.todo-list li .toggle {\
    \\topacity: 0;\
    \}\
    \\
    \.todo-list li .toggle + label {\
    \\t/*\
    \\t\tFirefox requires `#` to be escaped - https://bugzilla.mozilla.org/show_bug.cgi?id=922433\
    \\t\tIE and Edge requires *everything* to be escaped to render, so we do that instead of just the `#` - https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/7157459/\
    \\t*/\
    \\tbackground-image: url('data:image/svg+xml;utf8,%3Csvg%20xmlns%3D%22http%3A//www.w3.org/2000/svg%22%20width%3D%2240%22%20height%3D%2240%22%20viewBox%3D%22-10%20-18%20100%20135%22%3E%3Ccircle%20cx%3D%2250%22%20cy%3D%2250%22%20r%3D%2250%22%20fill%3D%22none%22%20stroke%3D%22%23ededed%22%20stroke-width%3D%223%22/%3E%3C/svg%3E');\
    \\tbackground-repeat: no-repeat;\
    \\tbackground-position: center left;\
    \}\
    \\
    \.todo-list li .toggle:checked + label {\
    \\tbackground-image: url('data:image/svg+xml;utf8,%3Csvg%20xmlns%3D%22http%3A//www.w3.org/2000/svg%22%20width%3D%2240%22%20height%3D%2240%22%20viewBox%3D%22-10%20-18%20100%20135%22%3E%3Ccircle%20cx%3D%2250%22%20cy%3D%2250%22%20r%3D%2250%22%20fill%3D%22none%22%20stroke%3D%22%23bddad5%22%20stroke-width%3D%223%22/%3E%3Cpath%20fill%3D%22%235dc2af%22%20d%3D%22M72%2025L42%2071%2027%2056l-4%204%2020%2020%2034-52z%22/%3E%3C/svg%3E');\
    \}\
    \\
    \.todo-list li label {\
    \\tword-break: break-all;\
    \\tpadding: 15px 15px 15px 60px;\
    \\tdisplay: block;\
    \\tline-height: 1.2;\
    \\ttransition: color 0.4s;\
    \\tfont-weight: 400;\
    \\tcolor: #4d4d4d;\
    \}\
    \\
    \.todo-list li.completed label {\
    \\tcolor: #cdcdcd;\
    \\ttext-decoration: line-through;\
    \}\
    \\
    \.todo-list li .destroy {\
    \\tdisplay: none;\
    \\tposition: absolute;\
    \\ttop: 0;\
    \\tright: 10px;\
    \\tbottom: 0;\
    \\twidth: 40px;\
    \\theight: 40px;\
    \\tmargin: auto 0;\
    \\tfont-size: 30px;\
    \\tcolor: #cc9a9a;\
    \\tmargin-bottom: 11px;\
    \\ttransition: color 0.2s ease-out;\
    \}\
    \\
    \.todo-list li .destroy:hover {\
    \\tcolor: #af5b5e;\
    \}\
    \\
    \.todo-list li .destroy:after {\
    \\tcontent: '×';\
    \}\
    \\
    \.todo-list li:hover .destroy {\
    \\tdisplay: block;\
    \}\
    \\
    \.todo-list li .edit {\
    \\tdisplay: none;\
    \}\
    \\
    \.todo-list li.editing:last-child {\
    \\tmargin-bottom: -1px;\
    \}\
    \\
    \.footer {\
    \\tpadding: 10px 15px;\
    \\theight: 20px;\
    \\ttext-align: center;\
    \\tfont-size: 15px;\
    \\tborder-top: 1px solid #e6e6e6;\
    \}\
    \\
    \.footer:before {\
    \\tcontent: '';\
    \\tposition: absolute;\
    \\tright: 0;\
    \\tbottom: 0;\
    \\tleft: 0;\
    \\theight: 50px;\
    \\toverflow: hidden;\
    \\tbox-shadow: 0 1px 1px rgba(0, 0, 0, 0.2),\
    \\t            0 8px 0 -3px #f6f6f6,\
    \\t            0 9px 1px -3px rgba(0, 0, 0, 0.2),\
    \\t            0 16px 0 -6px #f6f6f6,\
    \\t            0 17px 2px -6px rgba(0, 0, 0, 0.2);\
    \}\
    \\
    \.todo-count {\
    \\tfloat: left;\
    \\ttext-align: left;\
    \}\
    \\
    \.todo-count strong {\
    \\tfont-weight: 300;\
    \}\
    \\
    \.filters {\
    \\tmargin: 0;\
    \\tpadding: 0;\
    \\tlist-style: none;\
    \\tposition: absolute;\
    \\tright: 0;\
    \\tleft: 0;\
    \}\
    \\
    \.filters li {\
    \\tdisplay: inline;\
    \}\
    \\
    \.filters li a {\
    \\tcolor: inherit;\
    \\tmargin: 3px;\
    \\tpadding: 3px 7px;\
    \\ttext-decoration: none;\
    \\tborder: 1px solid transparent;\
    \\tborder-radius: 3px;\
    \}\
    \\
    \.filters li a:hover {\
    \\tborder-color: rgba(175, 47, 47, 0.1);\
    \}\
    \\
    \.filters li a.selected {\
    \\tborder-color: rgba(175, 47, 47, 0.2);\
    \}\
    \\
    \.clear-completed,\
    \html .clear-completed:active {\
    \\tfloat: right;\
    \\tposition: relative;\
    \\tline-height: 20px;\
    \\ttext-decoration: none;\
    \\tcursor: pointer;\
    \}\
    \\
    \.clear-completed:hover {\
    \\ttext-decoration: underline;\
    \}\
    \\
    \.info {\
    \\tmargin: 65px auto 0;\
    \\tcolor: #4d4d4d;\
    \\tfont-size: 11px;\
    \\ttext-shadow: 0 1px 0 rgba(255, 255, 255, 0.5);\
    \\ttext-align: center;\
    \}\
    \\
    \.info p {\
    \\tline-height: 1;\
    \}\
    \\
    \.info a {\
    \\tcolor: inherit;\
    \\ttext-decoration: none;\
    \\tfont-weight: 400;\
    \}\
    \\
    \.info a:hover {\
    \\ttext-decoration: underline;\
    \}\
    \\
    \/*\
    \\tHack to remove background from Mobile Safari.\
    \\tCan't use it globally since it destroys checkboxes in Firefox\
    \*/\
    \@media screen and (-webkit-min-device-pixel-ratio:0) {\
    \\t.toggle-all,\
    \\t.todo-list li .toggle {\
    \\t\tbackground: none;\
    \\t}\
    \\
    \\t.todo-list li .toggle {\
    \\t\theight: 40px;\
    \\t}\
    \}\
    \\
    \@media (max-width: 430px) {\
    \\t.footer {\
    \\t\theight: 50px;\
    \\t}\
    \\
    \\t.filters {\
    \\t\tbottom: 10px;\
    \\t}\
    \}"
