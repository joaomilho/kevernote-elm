module Kevernote where

import Date exposing (fromString, toTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing((<~), (~), Address)
import String
import Time exposing (every, second, minute, hour)

--------
-- Model
--------

type alias Note = { id: Int, title : String, body : String, created_at : String, distance_in_time : String }

type alias Model = { uid : Int, notes : List Note, selectedNote : Int, windowHeight: Int }

initialModel : Model
initialModel =
    { uid = 2,
      notes = [Note 1 "New" "New" "2015-8-2 15:31:32" "...", Note 2 "Title2" "Desc2" "2015-8-1 15:10:32" "..."],
      selectedNote = 1,
      windowHeight = 700 }

----------------
-- Model helpers
----------------

getSelectedNote : Model -> Note
getSelectedNote model =
    first (List.filter (\n -> n.id == model.selectedNote) model.notes)

first : List Note -> Note
first notes =
  case List.head notes of Just note -> note

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
    case event of
        Tick time ->
            {model |
                notes <- updateNotesTimeDistance model.notes time }

        New ->
            let newNote = Note (model.uid + 1) "New" "New" "2015-8-3 20:10:32" "Now"
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
                notes <- List.filter (\n -> note /= n) model.notes}

-----------------
-- Update helpers
-----------------

updateNote : Model -> Note -> Note -> Model
updateNote model note updatedNote =
  let replaceNote n = if note.id == n.id then updatedNote else n
  in {model |
        selectedNote <- updatedNote.id,
        notes <- List.map replaceNote model.notes}

updateNotesTimeDistance : List Note -> Float -> List Note
updateNotesTimeDistance notes time =
    List.map (updateNoteTimeDistance time) notes


distanceInTime : Float -> Float -> String
distanceInTime event now =
    let distance = now - event
    in let x = if | distance < minute -> (distance / second, "seconds")
                  | distance < hour -> (distance / minute, "minutes")
                  | otherwise -> (distance / hour, "hours")
    in case x of (amount, unit) -> (toString <| floor <| amount) ++ " " ++ unit ++ " ago"


updateNoteTimeDistance : Float -> Note -> Note
updateNoteTimeDistance time note =
    let date = fromString note.created_at
    in
        case date of
          Ok date ->
            {note | distance_in_time <- distanceInTime (toTime date) time }

          Err _ ->
            {note | distance_in_time <- "ERR" }

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
        div [class "note-list__summary"] [text (length model.notes)],
        ul [class "note-list__container"] (List.map (notePreview address model.selectedNote) model.notes)
    ]


notePreview : Address Action -> Int -> Note -> Html
notePreview address selectedNote note =
    li [class "note-preview"] [
        a [class ("note-preview__link" ++ if note.id == selectedNote then " is-selected" else ""), onClick address (Select note)] [
            span [class "note-preview__time"] [text note.distance_in_time],
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
    if String.length string > 200
        then (String.left 197 string) ++ "..."
        else string


length : List Note -> String
length notes =
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
    Signal.merge actions.signal tick


model : Signal Model
model =
    Signal.foldp update initialModel signals


actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp


main : Signal Html
main =
    (view actions.address) <~ model
