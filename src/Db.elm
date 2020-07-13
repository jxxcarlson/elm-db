module Db exposing (..)

import BiDict exposing (BiDict)
import Dict exposing (Dict)
import Maybe.Extra
import MultiBiDict exposing (MultiBiDict)
import Set


type alias Uuid =
    String


type alias Author =
    String


type alias Tag =
    String


type alias Title =
    String


type alias Row a =
    { a | id : Uuid, title : String, author : String, tags : List String }


type alias Db a =
    { data : Dict Uuid (Row a)
    , authorDict : BiDict Author Uuid
    , titleDict : BiDict Title Uuid
    , tagDict : MultiBiDict Tag Uuid
    }


empty =
    { data = Dict.empty
    , authorDict = BiDict.empty
    , titleDict = BiDict.empty
    , tagDict = MultiBiDict.empty
    }


{-|

> db = insert row1 empty

-}
insert : Row a -> Db a -> Db a
insert item db =
    { db
        | data = Dict.insert item.id item db.data
        , authorDict = BiDict.insert item.author item.id db.authorDict
        , titleDict = BiDict.insert (normalize item.title) item.id db.titleDict
    }


tag : Tag -> Uuid -> Db a -> Db a
tag tag_ id db =
    { db | tagDict = MultiBiDict.insert tag_ id db.tagDict }



-- GETTERS


get : Db a -> Uuid -> Maybe (Row a)
get db uuid =
    Dict.get uuid db.data


{-|

> getByTitle db "Intro to QM"
> Just { author = "jxxcarlson", content = "Yada yada", id = "123", tags = [], title = "Intro to QM" }

    : Maybe (Row { content : String })

-}
getByTitle : Db a -> Title -> Maybe (Row a)
getByTitle db title =
    BiDict.get (normalize title) db.titleDict
        |> Maybe.andThen (get db)


getByTag : Tag -> Db a -> List (Row a)
getByTag tag_ db =
    MultiBiDict.get (normalize tag_) db.tagDict
        |> Set.toList
        |> List.map (get db)
        |> Maybe.Extra.values


{-|

    > getByTags ["qm", "atom"] mdb
    [{ author = "jxxcarlson", content = "Yada yada", id = "234", tags = ["qm","atom"], title = "The Atom" }]

-}
getByTags : List Tag -> Db a -> List (Row a)
getByTags tagList db =
    case List.head tagList of
        Nothing ->
            []

        Just firstTag ->
            let
                remainingItems : List (Row a)
                remainingItems =
                    getByTag firstTag db

                remainingTags : List Tag
                remainingTags =
                    List.drop 1 tagList

                filter_ : Tag -> List (Row a) -> List (Row a)
                filter_ tag_ items_ =
                    List.filter (\item_ -> List.member tag_ item_.tags) items_
            in
            List.foldl filter_ remainingItems remainingTags



-- |> List.map (Maybe.andThen (get db))
-- HELPERS


normalize : String -> String
normalize str =
    str
        |> String.toLower
        |> String.replace " " "_"



-- TESTS


type alias Document =
    Row { content : String }


row1 : Document
row1 =
    { id = "123", author = "jxxcarlson", tags = [ "qm", "intro" ], title = "Intro to QM", content = "Yada yada" }


row2 : Document
row2 =
    { id = "234", author = "jxxcarlson", tags = [ "qm", "atom" ], title = "The Atom", content = "Yada yada" }


mdb =
    insert row1 empty
        |> insert row2
        |> tag "qm" row1.id
        |> tag "qm" row2.id
        |> tag "into" row1.id
        |> tag "atom" row2.id
