module Kevernote where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)
import List exposing (reverse, head, filter, map)
import Signal exposing((<~), Address, merge, foldp, mailbox)
import String exposing(left)
import Time exposing (Time, every, second, minute, hour)
import Debug

--------
-- Model
--------

lipsum : String
lipsum =
    "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."

type alias Note = { id: Int, title : String, body : String, createdAt : Float, distanceInTime : String }

type alias Model = { uid : Int, notes : List Note, selectedNote : Int, currentTime: Float }

initialModel : Model
initialModel =
    { uid = 2,
      notes = reverse [Note 1 "Note #1" lipsum 1438554156953 "...", Note 2 "Note #2" lipsum 1438554156953 "..."],
      selectedNote = 2,
      currentTime = 0 }

----------------
-- Model helpers
----------------

getSelectedNote : Model -> Note
getSelectedNote model =
    first (filter (\n -> n.id == model.selectedNote) model.notes)


first : List Note -> Note
first notes =
  case head notes of Just note -> note

---------
-- Update
---------

type Action =
    NoOp
    | Select Note
    | New
    | UpdateTitle Note String
    | UpdateBody Note String
    | Delete Note
    | Tick Float


update : Action -> Model -> Model
update event model =

    let newModel = case event of
        Tick time ->
            {model |
                currentTime <- time,
                notes <- updateNotesTimeDistance model.notes time }

        New ->
            let id = model.uid + 1
                newNote = Note id ("Note #" ++ toString id) "Type your stuff here" model.currentTime "Now"
            in {model |
                uid <- newNote.id,
                notes <- [newNote] ++ model.notes,
                selectedNote <- newNote.id}

        Select note ->
            {model | selectedNote <- note.id }

        UpdateTitle note newTitle ->
            let updatedNote = {note | title <- newTitle}
            in updateNote model note updatedNote

        UpdateBody note newBody ->
            let updatedNote = {note | body <- newBody}
            in updateNote model note updatedNote

        Delete note ->
            {model |
                selectedNote <- (first model.notes).id,
                notes <- filter (\n -> note /= n) model.notes}
    in Debug.watch "model" newModel

-----------------
-- Update helpers
-----------------

updateNote : Model -> Note -> Note -> Model
updateNote model note updatedNote =
  let replaceNote n = if note.id == n.id then updatedNote else n
  in {model |
        selectedNote <- updatedNote.id,
        notes <- map replaceNote model.notes}

updateNotesTimeDistance : List Note -> Float -> List Note
updateNotesTimeDistance notes time =
    map (updateNoteTimeDistance time) notes


updateNoteTimeDistance : Float -> Note -> Note
updateNoteTimeDistance time note =
    {note |
        distanceInTime <- distanceInTime note.createdAt time }


distanceInTime : Float -> Float -> String
distanceInTime event now =
    let distance = now - event
    in let x = if | distance < minute -> (distance / second, "seconds")
                  | distance < hour -> (distance / minute, "minutes")
                  | otherwise -> (distance / hour, "hours")
    in case x of (amount, unit) -> (toString <| floor <| amount) ++ " " ++ unit ++ " ago"

-------
-- View
-------

view : Address Action -> Model -> Html
view address model =
    main' [class "app"] [
        css "/style.css",
        actionBar address model,
        noteList address model,
        noteView address model
    ]

----------------
-- View partials
----------------

actionBar : Address Action -> Model -> Html
actionBar address model =
    nav [class "action-bar"] [
        div [class "action-bar__logo"] [],
        button [class "action-bar__new", onClick address New] [text "+"]
    ]


noteList : Address Action -> Model -> Html
noteList address model =
    aside [class "note-list"] [
        h2 [class "note-list__title"] [text "Notes"],
        div [class "note-list__summary"] [text (noteCount model.notes)],
        ul [class "note-list__container"] (map (notePreview address model.selectedNote) model.notes)
    ]


notePreview : Address Action -> Int -> Note -> Html
notePreview address selectedNote note =
    li [class ("note-preview" ++ if note.id == selectedNote then " is-selected" else "")] [
        a [href "#", class "note-preview__link", onClick address (Select note)] [
            span [class "note-preview__time"] [text note.distanceInTime],
            h2 [class "note-preview__title"] [text note.title],
            p [class "note-preview__body"] [text (limit note.body) ]
        ]
    ]


noteView : Address Action -> Model -> Html
noteView address model =
    let note = (getSelectedNote model)
    in
        article [class "note-view"] [
            nav [class "note-view__actions"] [
                button [class "note-view__actions__trash", onClick address (Delete note)] [],
                span [class "note-view__actions__status"] [text "Saved"]
            ],
            input [class "note-view__title", onInput address (UpdateTitle note), value note.title] [],
            textarea [class "note-view__body", onInput address (UpdateBody note), value note.body] []
        ]

---------------
-- View helpers
---------------

css : String -> Html
css url =
    node "link" [rel "stylesheet", type' "text/css", href url] []


onInput : Address a -> (String -> a) -> Attribute
onInput address action =
    on "input" targetValue (Signal.message address << action)


limit : String -> String
limit string =
    if String.length string > 100
        then (left 97 string) ++ "..."
        else string


noteCount : List Note -> String
noteCount notes =
    let len = List.length notes
    in if len == 1
        then "1 note"
        else (toString len) ++ " notes"

-------------------
-- Elm architecture
-------------------

tick : Signal Action
tick =
    Tick <~ every second


signals : Signal Action
signals =
    merge actions.signal tick


model : Signal Model
model =
    foldp update initialModel signals


actions : Signal.Mailbox Action
actions =
    mailbox NoOp


main : Signal Html
main =
    (view actions.address) <~ model
