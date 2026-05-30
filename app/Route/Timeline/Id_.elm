module Route.Timeline.Id_ exposing (ActionData, CharacterSummary, Data, Model, MouseState, Msg, PostData, RouteParams, route)

import Annotation exposing (Annotation(..), MessageId)
import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.File as File
import BoundingBox2d exposing (BoundingBox2d)
import Codec exposing (Codec)
import Color
import Color.Oklch as Oklch exposing (Oklch)
import Dict
import Dict.Extra
import Effect exposing (Effect)
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import Glowfic.Utils
import GlowficApi.Extra
import GlowficApi.Types exposing (PostDetails, PostSummary, Reply, Status(..))
import GlowficRoute
import Head
import Head.Seo as Seo
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Html.Lazy
import Html.Parser
import Id exposing (BoardId, CharacterId, IconId, Id, PostId)
import Json.Decode
import Length exposing (Length, Meters)
import List.Extra
import Maybe.Extra
import Monad exposing (Monad)
import Monad.Do as Do
import OpenApi.Common
import Pages.Url
import PagesMsg
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Round
import Route
import RouteBuilder exposing (App, StatefulRoute)
import SeqDict exposing (SeqDict)
import Server.Response as Response exposing (Response)
import Shared
import String.Extra
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InMeters
import TypedSvg.Core
import TypedSvg.Types exposing (AnchorAlignment(..), DominantBaseline(..), Paint(..))
import Url exposing (Url)
import UrlPath exposing (UrlPath)
import Vector2d exposing (Vector2d)
import View exposing (View)


type alias ActionData =
    Never


type alias Data =
    { boardId : Id BoardId
    , name : String
    , initialPosts : SeqDict (Id PostId) PostData
    , characters : SeqDict (Id CharacterId) CharacterSummary
    }


type alias PostData =
    { post : PostDetails
    , replies : List Reply
    , annotations : List ( MessageId, Annotation )
    , boundingBox : BoundingBox2d Meters {}
    }


type alias CharacterSummary =
    { name : String
    , color : Oklch
    , icon : Maybe { id : Id IconId, url : Url }
    }


type alias Model =
    { posts : SeqDict (Id PostId) PostData
    , mouseState : MouseState
    , selectedCharacter : Maybe (Id CharacterId)
    }


type MouseState
    = MouseNotDragging
    | MouseDragging (Id PostId) (Point2d Meters {}) (Point2d Meters {})


type Msg
    = MouseDown PointerEvent
    | MouseMove PointerEvent
    | MouseUp PointerEvent
    | MouseEnterCharacter (Id CharacterId)
    | MouseLeaveCharacter (Id CharacterId)
    | DownloadPositions


type alias PointerEvent =
    { offsetPosition : Point2d Pixels {}
    , button : Mouse.Button
    , elementSize : { width : Quantity Float Pixels, height : Quantity Float Pixels }
    }


type alias RouteParams =
    { id : String }


route : StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.preRenderWithFallback
        { head = head
        , data = data
        , pages =
            BackendTask.succeed
                [--     { id = "4968" }
                 -- , { id = "4902" }
                ]
        }
        |> RouteBuilder.buildWithLocalState
            { init = init
            , update = update
            , subscriptions = subscriptions
            , view = \app _ model -> view app model |> View.map PagesMsg.fromMsg
            }


init : App Data ActionData RouteParams -> Shared.Model -> ( Model, Effect msg )
init app _ =
    ( { posts = app.data.initialPosts
      , mouseState = MouseNotDragging
      , selectedCharacter = Nothing
      }
    , Effect.none
    )


update : App Data ActionData RouteParams -> Shared.Model -> Msg -> Model -> ( Model, Effect msg )
update app _ msg model =
    case msg of
        MouseDown event ->
            if event.button /= Mouse.MainButton then
                ( model, Effect.none )

            else
                let
                    initialPosition : Point2d Meters {}
                    initialPosition =
                        event.offsetPosition
                            |> Point2d.at (pixelsToMeters event)
                in
                case
                    model.posts
                        |> SeqDict.values
                        |> List.Extra.findMap
                            (\{ post, boundingBox } ->
                                if BoundingBox2d.contains initialPosition boundingBox then
                                    Just post.id

                                else
                                    Nothing
                            )
                of
                    Nothing ->
                        ( model, Effect.none )

                    Just postId ->
                        ( { model | mouseState = MouseDragging postId initialPosition initialPosition }, Effect.none )

        MouseMove event ->
            ( case model.mouseState of
                MouseNotDragging ->
                    model

                MouseDragging postId initialPosition _ ->
                    let
                        position : Point2d Meters {}
                        position =
                            event.offsetPosition
                                |> Point2d.at (pixelsToMeters event)
                    in
                    { model | mouseState = MouseDragging postId initialPosition position }
            , Effect.none
            )

        MouseUp event ->
            if event.button /= Mouse.MainButton then
                ( model, Effect.none )

            else
                ( case model.mouseState of
                    MouseNotDragging ->
                        model

                    MouseDragging postId initialPosition draggedPosition ->
                        let
                            vector : Vector2d Meters {}
                            vector =
                                Vector2d.from initialPosition draggedPosition
                        in
                        { model
                            | mouseState = MouseNotDragging
                            , posts =
                                SeqDict.updateIfExists
                                    postId
                                    (\post ->
                                        { post | boundingBox = BoundingBox2d.translateBy vector post.boundingBox }
                                    )
                                    model.posts
                        }
                , Effect.none
                )

        DownloadPositions ->
            ( model
            , Effect.saveFile
                { filename = Glowfic.Utils.boardAnnotationsFilename app.data.boardId
                , mime = "application/json"
                , content =
                    model.posts
                        |> SeqDict.map (\_ { boundingBox } -> boundingBox)
                        |> Codec.encodeToString 0 positionsCodec
                }
            )

        MouseEnterCharacter id ->
            ( { model | selectedCharacter = Just id }, Effect.none )

        MouseLeaveCharacter id ->
            if model.selectedCharacter == Just id then
                ( { model | selectedCharacter = Nothing }, Effect.none )

            else
                ( model, Effect.none )


pixelsToMeters : PointerEvent -> Quantity Float (Quantity.Rate Meters Pixels)
pixelsToMeters event =
    Quantity.min
        (svgViewBoxSize.width |> Quantity.per event.elementSize.width)
        (svgViewBoxSize.height |> Quantity.per event.elementSize.height)


subscriptions : RouteParams -> UrlPath -> Shared.Model -> Model -> Sub msg
subscriptions _ _ _ _ =
    Sub.none


head : App Data ActionData RouteParams -> List Head.Tag
head _ =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = [ "images", "icon-png.png" ] |> UrlPath.join |> Pages.Url.fromPath
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "Welcome to elm-pages!"
        , locale = Nothing
        , title = "elm-pages is running"
        }
        |> Seo.website


data : RouteParams -> BackendTask FatalError (Response Data ErrorPage)
data params =
    Monad.run (monad params)


monad : RouteParams -> Monad (Response Data ErrorPage)
monad params =
    Do.do
        (case String.toInt params.id of
            Nothing ->
                ("Invalid id: " ++ params.id)
                    |> Monad.failString

            Just i ->
                let
                    boardId : Id BoardId
                    boardId =
                        Id.unsafe i
                in
                Monad.succeed boardId
        )
    <| \boardId ->
    Do.do (GlowficApi.Extra.getBoard boardId) <| \board ->
    Do.do (GlowficApi.Extra.getAllBoardsIdPosts boardId) <| \results ->
    -- Reverse posts so that 429s hurt us less
    Do.eachCount (results |> List.reverse |> List.Extra.removeWhen nonCanonical)
        (\post ->
            Monad.map2 Tuple.pair
                (GlowficApi.Extra.getPost post.id)
                (Glowfic.Utils.readAnnotationsFromFile post.id
                    |> Monad.lift
                    |> Monad.map (Maybe.withDefault [])
                )
        )
    <| \posts ->
    Do.do (Monad.lift (positionsData board.id)) <| \positions ->
    let
        frequency : SeqDict (Id CharacterId) Int
        frequency =
            posts
                |> List.map (\( post, _ ) -> Glowfic.Utils.allCharactersIds post |> SeqDict.map (\_ _ -> 1))
                |> List.foldl
                    (\e a ->
                        SeqDict.merge
                            SeqDict.insert
                            (\k v1 v2 d -> SeqDict.insert k (v1 + v2) d)
                            SeqDict.insert
                            e
                            a
                            SeqDict.empty
                    )
                    SeqDict.empty

        charactersIds : List (Id CharacterId)
        charactersIds =
            frequency
                |> SeqDict.keys
                |> List.sortBy
                    (\id ->
                        SeqDict.get id frequency
                            |> Maybe.withDefault 0
                            |> negate
                    )
    in
    Do.log (Ansi.Color.fontColor Ansi.Color.cyan ("🧑 Got " ++ String.fromInt (List.length charactersIds) ++ " characters, fetching icons")) <| \() ->
    Do.eachCount (assignColors charactersIds) (\( id, color ) -> getCharacter id color) <| \characters ->
    let
        result : Data
        result =
            { boardId = boardId
            , characters =
                characters
                    |> Maybe.Extra.values
                    |> SeqDict.fromList
            , initialPosts =
                posts
                    |> List.sortBy
                        (\( ( p, _ ), _ ) ->
                            ( case p.section of
                                OpenApi.Common.Present { id } ->
                                    Id.toInt id

                                OpenApi.Common.Null ->
                                    0
                            , Id.toInt p.id
                            )
                        )
                    |> List.indexedMap
                        (\i ( ( p, r ), ann ) ->
                            ( Id.for p
                            , { post = p
                              , replies = r
                              , annotations = ann
                              , boundingBox = SeqDict.get p.id positions |> Maybe.withDefault (defaultBoundingBox i)
                              }
                            )
                        )
                    |> SeqDict.fromList
            , name = board.name
            }
    in
    result
        |> Response.render
        |> Monad.succeed


positionsData : Id BoardId -> BackendTask FatalError (SeqDict (Id PostId) (BoundingBox2d Meters {}))
positionsData boardId =
    File.jsonFile (Codec.decoder positionsCodec) (Glowfic.Utils.boardAnnotationsFilepath boardId)
        |> BackendTask.onError
            (\e ->
                case e.recoverable of
                    File.FileDoesntExist ->
                        BackendTask.succeed SeqDict.empty

                    File.FileReadError _ ->
                        BackendTask.fail e.fatal

                    File.DecodingError _ ->
                        BackendTask.fail e.fatal
            )


positionsCodec : Codec (SeqDict (Id PostId) (BoundingBox2d Meters {}))
positionsCodec =
    Codec.tuple Id.codec positionCodec
        |> Codec.list
        |> Codec.map SeqDict.fromList SeqDict.toList


positionCodec : Codec (BoundingBox2d Meters {})
positionCodec =
    boundingBox2dCodec


boundingBox2dCodec : Codec (BoundingBox2d unit coordinates)
boundingBox2dCodec =
    Codec.map
        BoundingBox2d.fromExtrema
        boundingBox2dToExtrema
        (Codec.object
            (\minX maxX minY maxY ->
                { minX = minX
                , maxX = maxX
                , minY = minY
                , maxY = maxY
                }
            )
            |> Codec.field "minX" .minX (quantityCodec Codec.float)
            |> Codec.field "maxX" .maxX (quantityCodec Codec.float)
            |> Codec.field "minY" .minY (quantityCodec Codec.float)
            |> Codec.field "maxY" .maxY (quantityCodec Codec.float)
            |> Codec.buildObject
        )


boundingBox2dToExtrema :
    BoundingBox2d units coordinates
    ->
        { minX : Quantity Float units
        , maxX : Quantity Float units
        , minY : Quantity Float units
        , maxY : Quantity Float units
        }
boundingBox2dToExtrema boundingBox =
    { minX = BoundingBox2d.minX boundingBox
    , maxX = BoundingBox2d.maxX boundingBox
    , minY = BoundingBox2d.minY boundingBox
    , maxY = BoundingBox2d.maxY boundingBox
    }


quantityCodec : Codec number -> Codec (Quantity number unit)
quantityCodec codec =
    Codec.map Quantity.unsafe Quantity.unwrap codec


nonCanonical : PostSummary -> Bool
nonCanonical post =
    case post.section of
        OpenApi.Common.Present section ->
            section.name == "Non-Canon"

        OpenApi.Common.Null ->
            False


assignColors : List id -> List ( id, Oklch )
assignColors list =
    List.indexedMap (\i e -> ( e, Oklch.oklch 0.7 0.2 (toFloat i / 9) )) list


childrenToString : List Html.Parser.Node -> String
childrenToString nodes =
    String.concat (List.map nodeToString nodes)


nodeToString : Html.Parser.Node -> String
nodeToString node =
    case node of
        Html.Parser.Element _ _ children ->
            childrenToString children

        Html.Parser.Text t ->
            t

        Html.Parser.Comment _ ->
            ""


getCharacter :
    Id CharacterId
    -> Oklch
    -> Monad (Maybe ( Id CharacterId, CharacterSummary ))
getCharacter id color =
    GlowficApi.Extra.getCharacter id
        |> Monad.map
            (\character ->
                if character.npc then
                    Nothing

                else
                    Just
                        ( id
                        , { name = character.name
                          , color = color
                          , icon =
                                case character.default_icon of
                                    OpenApi.Common.Null ->
                                        Nothing

                                    OpenApi.Common.Present icon ->
                                        Just { id = icon.id, url = icon.url }
                          }
                        )
            )


view : App Data ActionData RouteParams -> Model -> View Msg
view app model =
    { title = app.data.name
    , body =
        [ Html.div
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "grid-template-columns" "auto 1fr auto"
            , Html.Attributes.style "grid-template-rows" "1fr auto"
            , Html.Attributes.style "color" "#f3f3f3"
            , Html.Attributes.style "background" "#211e2f"
            , Html.Attributes.style "gap" "8px"
            , Html.Attributes.style "align-items" "start"
            , Html.Attributes.style "height" "100dvh"
            ]
            [ Html.Lazy.lazy viewPostsList app.data.initialPosts
            , let
                postsList : List PostData
                postsList =
                    model.posts
                        |> SeqDict.values
                        |> List.reverse
              in
              [ TypedSvg.g
                    [ TypedSvg.Attributes.id "wordlines"
                    ]
                    (viewWordlines app.data.characters model.selectedCharacter postsList)
              , TypedSvg.g
                    [ TypedSvg.Attributes.id "posts"
                    ]
                    (List.map (viewPost model.mouseState) postsList)
              ]
                |> TypedSvg.svg
                    [ TypedSvg.Attributes.InMeters.viewBox
                        Quantity.zero
                        Quantity.zero
                        svgViewBoxSize.width
                        svgViewBoxSize.height
                    , mouseEventWithSize "pointerdown" MouseDown
                    , mouseEventWithSize "pointermove" MouseMove
                    , mouseEventWithSize "pointerup" MouseUp
                    , Html.Attributes.style "background" "#228"
                    , Html.Attributes.style "overflow" "scroll"
                    ]
                |> List.singleton
                |> Html.div
                    [ Html.Attributes.style "max-height" "calc(100dvh - 80px)"
                    , Html.Attributes.style "display" "flex"
                    ]
            , Html.Lazy.lazy2 viewCharactersList model.selectedCharacter app.data.characters
            , Html.div [ Html.Attributes.style "padding" "8px 0" ]
                [ Html.button
                    [ Html.Events.onClick DownloadPositions
                    ]
                    [ Html.text "Download positions" ]
                ]
            ]
        ]
    }


viewWordlines : SeqDict (Id CharacterId) CharacterSummary -> Maybe (Id CharacterId) -> List PostData -> List (Html Msg)
viewWordlines characters selected posts =
    posts
        |> List.concatMap
            (\{ boundingBox, annotations } ->
                List.filterMap (annotationToPoint boundingBox) annotations
            )
        |> Dict.Extra.groupBy (\( characterId, _ ) -> Id.toInt characterId)
        |> Dict.values
        |> List.filterMap
            (\t ->
                case t of
                    [] ->
                        Nothing

                    ( id, _ ) :: _ ->
                        Just ( id, List.map Tuple.second t )
            )
        |> List.map
            (\( id, t ) ->
                viewWordline selected id (SeqDict.get id characters) t
            )


viewWordline : Maybe (Id CharacterId) -> Id CharacterId -> Maybe CharacterSummary -> List (Point2d Meters {}) -> Html Msg
viewWordline selected id maybeCharacter points =
    let
        path : String
        path =
            points
                |> List.sortBy
                    (\p ->
                        let
                            { x, y } =
                                Point2d.toMeters p
                        in
                        ( y, x )
                    )
                |> List.indexedMap
                    (\i p ->
                        let
                            { x, y } =
                                Point2d.toMeters p
                        in
                        if i == 0 then
                            "M " ++ Round.round 0 (x * 100) ++ " " ++ Round.round 0 (y * 100)

                        else
                            "L " ++ Round.round 0 (x * 100) ++ " " ++ Round.round 0 (y * 100)
                    )
                |> String.join " "

        ( characterAttrs, stroke ) =
            case maybeCharacter of
                Nothing ->
                    ( [], Oklch.oklch 1 0 0 )

                Just character ->
                    ( [ TypedSvg.Attributes.title character.name
                      ]
                    , character.color
                    )
    in
    TypedSvg.path
        ([ TypedSvg.Attributes.d path
         , TypedSvg.Attributes.fill PaintNone
         , TypedSvg.Attributes.InMeters.strokeWidth (Length.centimeters 1.5)
         , Html.Events.onMouseEnter (MouseEnterCharacter id)
         , Html.Events.onMouseLeave (MouseLeaveCharacter id)
         , if selected == Nothing || selected == Just id then
            TypedSvg.Core.attribute "stroke" (Oklch.toCssString stroke)

           else
            TypedSvg.Core.attribute "stroke" "#ffffff20"
         , Html.Attributes.style "animate" "stroke 0.5s"
         ]
            ++ characterAttrs
        )
        []


annotationToPoint :
    BoundingBox2d units coordinates
    -> ( MessageId, Annotation )
    -> Maybe ( Id CharacterId, Point2d units coordinates )
annotationToPoint boundingBox ( _, annotation ) =
    case annotation of
        Enter characterId ->
            Just ( characterId, BoundingBox2d.centerPoint boundingBox )

        Exit _ ->
            Nothing

        HappensBefore _ ->
            Nothing

        HappensAfter _ ->
            Nothing


svgViewBoxSize :
    { width : Length
    , height : Length
    }
svgViewBoxSize =
    { width = Length.meters 6
    , height = Length.meters 6
    }


mouseEventWithSize : String -> (PointerEvent -> msg) -> Html.Attribute msg
mouseEventWithSize name tag =
    eventDecoder
        |> Json.Decode.map
            (\ev ->
                { message = tag ev
                , stopPropagation = False
                , preventDefault = True
                }
            )
        |> Html.Events.custom name


eventDecoder : Json.Decode.Decoder PointerEvent
eventDecoder =
    let
        pixels : Json.Decode.Decoder (Quantity Float Pixels)
        pixels =
            Json.Decode.map Pixels.pixels Json.Decode.float
    in
    Json.Decode.map5
        (\svgX svgY svgWidth svgHeight event ->
            let
                ( clientX, clientY ) =
                    event.clientPos
            in
            { offsetPosition =
                Point2d.xy
                    (Pixels.pixels clientX |> Quantity.minus svgX)
                    (Pixels.pixels clientY |> Quantity.minus svgY)
            , elementSize =
                { width = svgWidth
                , height = svgHeight
                }
            , button = event.button
            }
        )
        (Json.Decode.at [ "currentTarget", "__boundingBox", "x" ] pixels)
        (Json.Decode.at [ "currentTarget", "__boundingBox", "y" ] pixels)
        (Json.Decode.at [ "currentTarget", "__boundingBox", "width" ] pixels)
        (Json.Decode.at [ "currentTarget", "__boundingBox", "height" ] pixels)
        Mouse.eventDecoder


defaultBoundingBox : Int -> BoundingBox2d Meters {}
defaultBoundingBox i =
    let
        columns : Int
        columns =
            Quantity.ratio
                (svgViewBoxSize.width
                    |> Quantity.minus (Length.centimeters gap)
                )
                (Length.centimeters (defaultWidth + gap))
                |> floor
                |> max 1

        row : Int
        row =
            i // columns

        column : Int
        column =
            modBy columns i

        defaultWidth : number
        defaultWidth =
            12

        defaultHeight : number
        defaultHeight =
            14

        gap : number
        gap =
            10
    in
    BoundingBox2d.fromExtrema
        { minX = Length.centimeters (gap + (defaultWidth + gap) * toFloat column)
        , minY = Length.centimeters (gap + (defaultHeight + gap) * toFloat row)
        , maxX = Length.centimeters (gap + (defaultWidth + gap) * toFloat column + defaultWidth)
        , maxY = Length.centimeters (gap + (defaultHeight + gap) * toFloat row + defaultHeight)
        }


viewPost : MouseState -> PostData -> Html Msg
viewPost mouseState postData =
    let
        vector : Vector2d Meters {}
        vector =
            case mouseState of
                MouseNotDragging ->
                    Vector2d.zero

                MouseDragging postId initialPosition draggedPosition ->
                    if postId == postData.post.id then
                        Vector2d.from initialPosition draggedPosition

                    else
                        Vector2d.zero
    in
    innerViewPost postData.post vector postData.boundingBox


innerViewPost : PostDetails -> Vector2d Meters {} -> BoundingBox2d Meters {} -> Html msg
innerViewPost =
    Html.Lazy.lazy3
        (\post vector boundingBox ->
            let
                moved : BoundingBox2d Meters {}
                moved =
                    boundingBox |> BoundingBox2d.translateBy vector

                ( x, y ) =
                    ( BoundingBox2d.minX moved, BoundingBox2d.minY moved )

                ( w, h ) =
                    BoundingBox2d.dimensions boundingBox

                cx : Quantity Float Meters
                cx =
                    Quantity.plus x (Quantity.half w)

                cy : Quantity Float Meters
                cy =
                    Quantity.plus y (Quantity.half h)

                lines : List String
                lines =
                    post.subject
                        |> String.Extra.ellipsis 40
                        |> String.Extra.softWrap 10
                        |> String.lines

                linesCount : Int
                linesCount =
                    List.length lines

                fontSize : Length
                fontSize =
                    Length.centimeters 2
            in
            [ TypedSvg.rect
                [ TypedSvg.Attributes.InMeters.x x
                , TypedSvg.Attributes.InMeters.y y
                , TypedSvg.Attributes.InMeters.width w
                , TypedSvg.Attributes.InMeters.height h
                ]
                []
            , lines
                |> List.indexedMap
                    (\i line ->
                        TypedSvg.tspan
                            [ TypedSvg.Attributes.InMeters.x cx
                            , TypedSvg.Attributes.InMeters.y
                                (cy
                                    |> Quantity.plus
                                        (fontSize |> Quantity.multiplyBy (toFloat i - toFloat (linesCount - 1) / 2))
                                )
                            ]
                            [ TypedSvg.Core.text line ]
                    )
                |> TypedSvg.text_
                    [ TypedSvg.Attributes.fill (Paint Color.white)
                    , TypedSvg.Attributes.InMeters.fontSize fontSize

                    -- , TypedSvg.Core.attribute "shape-inside" ("url(#" ++ id ++ ")")
                    -- , TypedSvg.Core.attribute "shape-padding" "8px"
                    , TypedSvg.Attributes.InMeters.x cx
                    , TypedSvg.Attributes.InMeters.y cy
                    , TypedSvg.Attributes.dominantBaseline DominantBaselineMiddle
                    , TypedSvg.Attributes.textAnchor AnchorMiddle

                    -- , TypedSvg.Attributes.InMeters.textLength w
                    -- , TypedSvg.Attributes.lengthAdjust LengthAdjustSpacingAndGlyphs
                    ]
            ]
                |> TypedSvg.g [ TypedSvg.Attributes.class [ "svg-post" ] ]
        )


viewPostsList : SeqDict (Id PostId) PostData -> Html msg
viewPostsList posts =
    posts
        |> SeqDict.values
        |> List.map
            (\postData ->
                case postData.post.section of
                    OpenApi.Common.Null ->
                        ( -1, "", postData )

                    OpenApi.Common.Present section ->
                        ( section.order, section.name, postData )
            )
        |> Dict.Extra.groupBy (\( ord, name, _ ) -> ( ord, name ))
        |> Dict.toList
        |> List.concatMap viewPostSectionForList
        |> Html.div
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "grid-template-columns" "40px auto"
            , Html.Attributes.style "gap" "8px"
            , Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "width" "200px"
            , Html.Attributes.style "overflow" "scroll"
            , Html.Attributes.style "height" "calc(100dvh - 16px)"
            , Html.Attributes.style "grid-row" "1 / span 2"
            ]


viewPostSectionForList : ( ( int, String ), List ( int, string, PostData ) ) -> List (Html msg)
viewPostSectionForList ( ( _, sectionName ), posts ) =
    let
        titleViews : List (Html msg)
        titleViews =
            if String.isEmpty sectionName then
                []

            else
                [ Html.div
                    [ Html.Attributes.style "font-weight" "bold"
                    , Html.Attributes.style "grid-column" "1 / span 2"
                    ]
                    [ Html.text sectionName ]
                ]

        postsViews : List (Html msg)
        postsViews =
            posts
                |> List.sortBy (\( _, _, { post } ) -> Id.toInt post.id)
                |> List.concatMap (\( _, _, postData ) -> viewPostForList postData)
    in
    titleViews ++ postsViews


viewPostForList : PostData -> List (Html msg)
viewPostForList { post, annotations } =
    let
        cutTitle : String
        cutTitle =
            String.Extra.ellipsis 40 post.subject
    in
    [ Html.div []
        [ [ case post.status of
                Status__Complete ->
                    "✅"

                Status__Active ->
                    "✍️"

                Status__Abandoned ->
                    "💀"
          , if List.isEmpty annotations then
                "⚠️"

            else
                ""
          ]
            |> List.Extra.removeWhen String.isEmpty
            |> String.join "\u{00A0}"
            |> Html.text
        ]
    , Route.link
        [ Html.Attributes.style "display" "block"
        ]
        [ Html.text cutTitle ]
        (Route.Timeline__Post__Id_ { id = Id.toString post.id })
    ]


viewCharactersList : Maybe (Id CharacterId) -> SeqDict (Id CharacterId) CharacterSummary -> Html Msg
viewCharactersList selected characters =
    characters
        |> SeqDict.toList
        |> List.concatMap (viewCharacterForList selected)
        |> Html.div
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "grid-template-columns" "40px auto"
            , Html.Attributes.style "gap" "8px"
            , Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "width" "200px"
            , Html.Attributes.style "overflow" "scroll"
            , Html.Attributes.style "height" "calc(100dvh - 16px)"
            , Html.Attributes.style "grid-row" "1 / span 2"
            , Html.Attributes.style "grid-column" "3"
            ]


viewCharacterForList : Maybe (Id CharacterId) -> ( Id CharacterId, CharacterSummary ) -> List (Html Msg)
viewCharacterForList selected ( characterId, { name, icon, color } ) =
    let
        common : List (Attribute Msg)
        common =
            [ if selected == Just characterId || selected == Nothing then
                Html.Attributes.style "opacity" "1"

              else
                Html.Attributes.style "opacity" "0.7"
            , Html.Attributes.style "transition" "opacity 0.5s"
            , Html.Attributes.style "display" "block"
            , Html.Events.onMouseEnter (MouseEnterCharacter characterId)
            , Html.Events.onMouseLeave (MouseLeaveCharacter characterId)
            ]
    in
    [ case icon of
        Just { id, url } ->
            Html.a
                ([ Html.Attributes.class "icon"
                 , Html.Attributes.href (GlowficRoute.icon id)
                 , Html.Attributes.title name
                 ]
                    ++ common
                )
                [ Html.img
                    [ Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "height" "auto"
                    , Html.Attributes.src (Url.toString url)
                    ]
                    []
                ]

        Nothing ->
            Html.div [] []
    , Html.a
        ([ Html.Attributes.style "background" (Oklch.toCssString color)
         , if color.lightness > 0.5 then
            Html.Attributes.style "color" "black"

           else
            Html.Attributes.style "color" "white"
         , Html.Attributes.href (GlowficRoute.character characterId)
         ]
            ++ common
        )
        [ Html.text name ]
    ]



-- viewLinksAsGraph : Data -> Html msg
-- viewLinksAsGraph appData =
--     case appData.links of
--         Err ( content, deadEnds ) ->
--             errorToHtml content deadEnds
--         Ok links ->
--             let
--                 toPostId : MessageId -> Id PostId
--                 toPostId messageId =
--                     case messageId of
--                         MessageIdPost pid ->
--                             pid
--                         MessageIdReply pid _ ->
--                             pid
--                 posts : SeqSet (Id PostId)
--                 posts =
--                     links
--                         |> List.concatMap
--                             (\link ->
--                                 [ toPostId link.from
--                                 , toPostId link.to
--                                 ]
--                             )
--                         |> SeqSet.fromList
--                 nodes : Result (Html msg) String
--                 nodes =
--                     posts
--                         |> SeqSet.toList
--                         |> Result.Extra.combineMap
--                             (\pid ->
--                                 let
--                                     pidString =
--                                         Id.toString pid
--                                 in
--                                 case SeqDict.get pid appData.posts of
--                                     Nothing ->
--                                         "Could not find post {pid}"
--                                             |> String.replace "{pid}" pidString
--                                             |> Err
--                                     Just ( post, _ ) ->
--                                         (pidString ++ " " ++ post.subject)
--                                             |> Ok
--                             )
--                         |> Result.map (String.join "\n")
--                         |> Result.mapError Html.text
--                 edges : Result (Html msg) String
--                 edges =
--                     case appData.links of
--                         Err ( s, e ) ->
--                             Err (errorToHtml s e)
--                         Ok ls ->
--                             ls
--                                 |> List.map
--                                     (\{ from, to, label } ->
--                                         [ Id.toString (toPostId from)
--                                         , Id.toString (toPostId to)
--                                         , label
--                                         ]
--                                             |> String.join " "
--                                     )
--                                 |> String.join "\n"
--                                 |> Ok
--             in
--             Html.pre []
--                 [ case Result.map2 Tuple.pair nodes edges of
--                     Ok ( n, e ) ->
--                         Html.text (n ++ "\n#\n" ++ e)
--                     Err e ->
--                         e
--                 ]
-- viewLinkEndpoint : MessageId -> Html msg
-- viewLinkEndpoint id =
--     case id of
--         MessageIdPost pid ->
--             Html.a
--                 [ Html.Attributes.href (GlowficRoute.post pid)
--                 ]
--                 [ "Post {pid}"
--                     |> String.replace "{pid}" (Id.toString pid)
--                     |> Html.text
--                 ]
--         MessageIdReply pid rid ->
--             Html.a
--                 [ Html.Attributes.href (GlowficRoute.reply rid)
--                 ]
--                 [ "Reply {rid} from post {pid}"
--                     |> String.replace "{rid}" (Id.toString rid)
--                     |> String.replace "{pid}" (Id.toString pid)
--                     |> Html.text
--                 ]
