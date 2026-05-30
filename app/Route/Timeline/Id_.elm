module Route.Timeline.Id_ exposing (ActionData, CharacterSummary, Data, Model, MouseState, Msg, Position, RouteParams, route)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.File as File
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox2d.Extra
import Codec exposing (Codec)
import Color
import Color.Oklch as Oklch exposing (Oklch)
import Effect exposing (Effect)
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import Frame2d exposing (Frame2d)
import Glowfic.Utils
import GlowficApi.Extra
import GlowficApi.Types exposing (PostDetails, PostSummary, Reply, Status(..))
import GlowficRoute
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Pointer as Pointer
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
import Route
import RouteBuilder exposing (App, StatefulRoute)
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Server.Response as Response exposing (Response)
import Shared
import String.Extra
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InMeters
import TypedSvg.Core
import TypedSvg.Types exposing (AnchorAlignment(..), DominantBaseline(..), LengthAdjust(..), Paint(..))
import Url exposing (Url)
import UrlPath exposing (UrlPath)
import Vector2d exposing (Vector2d)
import View exposing (View)


type alias ActionData =
    Never


type alias Data =
    { name : String
    , posts : SeqDict (Id PostId) { post : ( PostDetails, List Reply ), hasAnnotations : Bool }
    , characters : SeqDict (Id CharacterId) CharacterSummary
    , initialPositions : SeqDict (Id PostId) Position
    }


type alias Position =
    BoundingBox2d Meters {}


type alias CharacterSummary =
    { name : String
    , color : Oklch
    , icon : Maybe { id : Id IconId, url : Url }
    }


type alias Model =
    { positions : SeqDict (Id PostId) Position
    , mouseState : MouseState
    }


type MouseState
    = MouseNotDragging
    | MouseDragging (Id PostId) (Point2d Meters {}) (Point2d Meters {})


type Msg
    = MouseDown PointerEvent
    | MouseMove PointerEvent
    | MouseUp PointerEvent


type alias PointerEvent =
    { offsetPosition : Point2d Pixels {}
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
            , view = \app shared model -> view app shared model |> View.map PagesMsg.fromMsg
            }


init : App Data ActionData RouteParams -> Shared.Model -> ( Model, Effect msg )
init app _ =
    ( { positions = app.data.initialPositions
      , mouseState = MouseNotDragging
      }
    , Effect.none
    )


update : App Data ActionData RouteParams -> Shared.Model -> Msg -> Model -> ( Model, Effect msg )
update app _ msg model =
    case msg of
        MouseDown event ->
            let
                initialPosition : Point2d Meters {}
                initialPosition =
                    event.offsetPosition
                        |> Point2d.at (pixelsToMeters event)
            in
            case
                postsAndPositions app model
                    |> List.Extra.findMap
                        (\( { post }, boundingBox ) ->
                            if BoundingBox2d.contains initialPosition boundingBox then
                                Just (Tuple.first post).id

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
                        , positions =
                            SeqDict.update
                                postId
                                (\bb ->
                                    bb
                                        |> Maybe.withDefault (defaultBoundingBox 0)
                                        |> BoundingBox2d.translateBy vector
                                        |> Just
                                )
                                model.positions
                    }
            , Effect.none
            )


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
    <| \continuityId ->
    Do.do (GlowficApi.Extra.getBoard continuityId) <| \board ->
    Do.do (GlowficApi.Extra.getAllBoardsIdPosts continuityId) <| \results ->
    -- Reverse posts so that 429s hurt us less
    Do.eachCount (results |> List.reverse |> List.Extra.removeWhen nonCanonical)
        (\post ->
            Monad.map2 Tuple.pair
                (GlowficApi.Extra.getPost post.id)
                (Monad.lift (File.exists (Glowfic.Utils.postAnnotationsFilepath post.id)))
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
            { characters =
                characters
                    |> Maybe.Extra.values
                    |> SeqDict.fromList
            , posts =
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
                    |> List.map
                        (\( ( p, r ), ann ) ->
                            ( Id.for p, { post = ( p, r ), hasAnnotations = ann } )
                        )
                    |> SeqDict.fromList
            , name = board.name
            , initialPositions = positions
            }
    in
    result
        |> Response.render
        |> Monad.succeed


positionsData : Id BoardId -> BackendTask FatalError (SeqDict (Id PostId) Position)
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


positionsCodec : Codec (SeqDict (Id PostId) Position)
positionsCodec =
    Codec.tuple Id.codec positionCodec
        |> Codec.list
        |> Codec.map SeqDict.fromList SeqDict.toList


positionCodec : Codec Position
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
    let
        len : Float
        len =
            toFloat (List.length list)
    in
    List.indexedMap (\i e -> ( e, Oklch.oklch 0.7 0.2 (toFloat i / len) )) list


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


view : App Data ActionData RouteParams -> Shared.Model -> Model -> View Msg
view app _ model =
    { title = app.data.name
    , body =
        [ Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "color" "#f3f3f3"
            , Html.Attributes.style "background" "#211e2f"
            , Html.Attributes.style "gap" "8px"
            , Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "align-items" "start"
            ]
            [ postsAndPositions app model
                |> List.reverse
                |> List.map (viewPost model.mouseState)
                |> TypedSvg.svg
                    [ Html.Attributes.style "overflow" "scroll"
                    , Html.Attributes.style "max-width" "100vw"
                    , TypedSvg.Attributes.InMeters.viewBox
                        Quantity.zero
                        Quantity.zero
                        svgViewBoxSize.width
                        svgViewBoxSize.height
                    , mouseEventWithSize "pointerdown" MouseDown
                    , mouseEventWithSize "pointermove" MouseMove
                    , mouseEventWithSize "pointerup" MouseUp
                    ]
            , Html.Lazy.lazy viewCharacters app.data.characters
            ]
        ]
    }


svgViewBoxSize :
    { width : Length
    , height : Length
    }
svgViewBoxSize =
    { width = Length.meters 2
    , height = Length.meters 4
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
    Json.Decode.map6
        (\clientX clientY svgX svgY svgWidth svgHeight ->
            { offsetPosition =
                Point2d.xy
                    (clientX |> Quantity.minus svgX)
                    (clientY |> Quantity.minus svgY)
            , elementSize =
                { width = svgWidth
                , height = svgHeight
                }
            }
        )
        (Json.Decode.field "clientX" pixels)
        (Json.Decode.field "clientY" pixels)
        (Json.Decode.at [ "currentTarget", "__boundingBox", "x" ] pixels)
        (Json.Decode.at [ "currentTarget", "__boundingBox", "y" ] pixels)
        (Json.Decode.at [ "currentTarget", "__boundingBox", "width" ] pixels)
        (Json.Decode.at [ "currentTarget", "__boundingBox", "height" ] pixels)


postsAndPositions :
    App Data ActionData RouteParams
    -> Model
    ->
        List
            ( { post : ( PostDetails, List Reply ), hasAnnotations : Bool }
            , BoundingBox2d Meters {}
            )
postsAndPositions app model =
    SeqDict.merge
        (\_ post ( i, acc ) -> ( i + 1, ( post, defaultBoundingBox i ) :: acc ))
        (\_ post position ( i, acc ) -> ( i, ( post, position ) :: acc ))
        (\_ _ acc -> acc)
        app.data.posts
        model.positions
        ( 0, [] )
        |> Tuple.second


defaultBoundingBox : Int -> BoundingBox2d Meters {}
defaultBoundingBox i =
    let
        columns : Int
        columns =
            Quantity.ratio
                (svgViewBoxSize.width
                    |> Quantity.plus (Length.centimeters gap)
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
        { minX = Length.centimeters ((defaultWidth + gap) * toFloat column)
        , minY = Length.centimeters ((defaultHeight + gap) * toFloat row)
        , maxX = Length.centimeters ((defaultWidth + gap) * toFloat column + defaultWidth)
        , maxY = Length.centimeters ((defaultHeight + gap) * toFloat row + defaultHeight)
        }


viewPost : MouseState -> ( { post : ( PostDetails, List Reply ), hasAnnotations : Bool }, Position ) -> Html Msg
viewPost mouseState ( d, boundingBox ) =
    let
        ( post, _ ) =
            d.post

        vector : Vector2d Meters {}
        vector =
            case mouseState of
                MouseNotDragging ->
                    Vector2d.zero

                MouseDragging postId initialPosition draggedPosition ->
                    if postId == post.id then
                        Vector2d.from initialPosition draggedPosition

                    else
                        Vector2d.zero
    in
    innerViewPost post vector boundingBox


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
                |> TypedSvg.g []
        )


viewCharacters : SeqDict (Id CharacterId) CharacterSummary -> Html msg
viewCharacters characters =
    characters
        |> SeqDict.toList
        |> List.concatMap viewCharacter
        |> Html.div
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "grid-template-columns" "40px auto"
            , Html.Attributes.style "gap" "8px"
            , Html.Attributes.style "width" "200px"
            ]


viewCharacter : ( Id CharacterId, CharacterSummary ) -> List (Html msg)
viewCharacter ( characterId, { name, icon, color } ) =
    [ case icon of
        Just { url } ->
            Html.a
                [ Html.Attributes.class "icon"
                , Html.Attributes.href (GlowficRoute.character characterId)
                , Html.Attributes.title name
                , Html.Attributes.style "display" "block"
                ]
                [ Html.img
                    [ Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "height" "auto"
                    , Html.Attributes.src (Url.toString url)
                    ]
                    []
                ]

        Nothing ->
            Html.div [] []
    , Html.div
        [ Html.Attributes.style "background" (Oklch.toCssString color)
        , if color.lightness > 0.5 then
            Html.Attributes.style "color" "black"

          else
            Html.Attributes.style "color" "white"
        ]
        [ Html.text name ]
    ]


viewCharacterNames : Data -> List (Html msg)
viewCharacterNames appData =
    appData.characters
        |> SeqDict.toList
        |> List.map
            (\( id, { name, color } ) ->
                Html.a
                    [ Html.Attributes.href (GlowficRoute.character id)
                    , Html.Attributes.style "display" "block"
                    , Html.Attributes.style "grid-column-start" "character-name-start"
                    , Html.Attributes.style "grid-row-start" ("c" ++ Id.toString id ++ "-start")
                    , Html.Attributes.style "background" (Oklch.toCssString color)
                    , if color.lightness > 0.5 then
                        Html.Attributes.style "color" "black"

                      else
                        Html.Attributes.style "color" "white"
                    ]
                    [ Html.text name ]
            )


viewPostTitles : Data -> List (Html msg)
viewPostTitles appData =
    appData.posts
        |> SeqDict.values
        |> List.map
            (\postData ->
                let
                    ( post, _ ) =
                        postData.post

                    cutTitle : String
                    cutTitle =
                        String.Extra.ellipsis 40 post.subject
                in
                Route.link
                    [ Html.Attributes.style "display" "block"
                    , Html.Attributes.style "grid-row-start" "post-name-start"
                    , Html.Attributes.style "grid-column-start" ("p" ++ Id.toString post.id ++ "-start")
                    , Html.Attributes.style "writing-mode" "vertical-rl"
                    , Html.Attributes.style "text-orientation" "mixed"
                    , Html.Attributes.style "align-self" "start"
                    ]
                    [ [ case post.status of
                            Status__Complete ->
                                "✅"

                            Status__Active ->
                                "✍️"

                            Status__Abandoned ->
                                "💀"
                      , if postData.hasAnnotations then
                            ""

                        else
                            "⚠️"
                      , cutTitle
                      ]
                        |> List.Extra.removeWhen String.isEmpty
                        |> String.join " "
                        |> Html.text
                    ]
                    (Route.Timeline__Post__Id_ { id = Id.toString post.id })
            )



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


viewPostCharacters : Data -> ( PostDetails, List Reply ) -> List (Html msg)
viewPostCharacters appData ( post, replies ) =
    let
        charactersIds : SeqDict (Id CharacterId) (SeqSet String)
        charactersIds =
            Glowfic.Utils.allCharactersIds ( post, replies )
    in
    charactersIds
        |> SeqDict.keys
        |> List.Extra.stableSortWith
            (\l r ->
                case
                    ( SeqDict.get l appData.characters |> Maybe.andThen .icon
                    , SeqDict.get r appData.characters |> Maybe.andThen .icon
                    )
                of
                    ( Nothing, Just _ ) ->
                        GT

                    ( Just _, Nothing ) ->
                        LT

                    ( Nothing, Nothing ) ->
                        EQ

                    ( Just _, Just _ ) ->
                        EQ
            )
        |> List.map
            (\characterId ->
                Html.div
                    [ -- Html.Attributes.style "height" "60px"
                      -- , Html.Attributes.style "width" "10px"
                      -- ,
                      Html.Attributes.style "border" "1px solid white"
                    , Html.Attributes.style "padding" "4px"
                    , Html.Attributes.style "grid-row-start" ("c" ++ Id.toString characterId ++ "-start")
                    , Html.Attributes.style "grid-column-start" ("p" ++ Id.toString post.id ++ "-start")
                    ]
                    [ Html.text "X" ]
            )
