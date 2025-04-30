-- link: https://bellroy.com/products/venture-sling-6l?color=ranger_green&material=baida_nylon&size=6l
-- image url: https://bellroy-product-images.imgix.net/bellroy_dot_com_range_page_image/USD/BMVA-NSK-218/0?auto=format&fit=max&w=160

module Main exposing (main)

import Browser
import Html exposing (Html, div, h2, img, li, p, text, ul)
import Html.Attributes exposing (src)
import Http
import Json.Decode exposing (Decoder, field, int, list, maybe, string)


-- MODEL
type alias ProductVariation =
    { name : String
    , color : String
    , sku : String
    }
type alias ProductCard =
    { id : Maybe String
    , slides : List ProductVariation
    , productName : Maybe String
    , description : Maybe String
    , price : Maybe Int
    , error : Maybe String
    }
type alias Model =
    { cards : List ProductCard
    , fetchError : Maybe String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { cards = [], fetchError = Nothing }
    , fetchProducts
    )


-- UPDATE
type Msg
    = GotProducts (Result Http.Error (List ProductCard))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotProducts (Ok cards) ->
            ( { model | cards = cards }, Cmd.none )

        GotProducts (Err err) ->
            ( { model | fetchError = Just (httpErrorToString err) }, Cmd.none )

httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out."

        Http.NetworkError ->
            "Network error."

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad body: " ++ body


-- VIEW
view : Model -> Html Msg
view model =
    case model.fetchError of
        Just err ->
            div [] [ text ("Error fetching data: " ++ err) ]

        Nothing ->
            div []
                (List.map viewCard model.cards)

viewCard : ProductCard -> Html Msg
viewCard card =
    div [ Html.Attributes.style "border" "1px solid #ccc", Html.Attributes.style "margin" "1rem", Html.Attributes.style "padding" "1rem" ]
        [ h2 [] [ text (Maybe.withDefault "Unnamed Product" card.productName) ]
        , p [] [ text ("Description: " ++ Maybe.withDefault "No description" card.description) ]
        , p [] [ text ("Price: $" ++ Maybe.withDefault "0" (Maybe.map String.fromInt card.price)) ]
        , ul [] (List.map viewSlide card.slides)
        ]

viewSlide : ProductVariation -> Html Msg
viewSlide slide =
    li []
        [ text (slide.name ++ " — " ++ slide.color ++ " (SKU: " ++ slide.sku ++ ")") ]

-- HTTP

fetchProducts : Cmd Msg
fetchProducts =
    Http.get
        { url = "/public/data.json"
        , expect = Http.expectJson GotProducts (Json.Decode.list productCardDecoder)
        }

-- DECODERS

productVariationDecoder : Decoder ProductVariation
productVariationDecoder =
    Json.Decode.map3 ProductVariation
        (field "name" string)
        (field "color" string)
        (field "sku" string)

productCardDecoder : Decoder ProductCard
productCardDecoder =
    Json.Decode.map6 ProductCard
        (field "id" (maybe string))
        (field "slides" (list productVariationDecoder))
        (field "productName" (maybe string))
        (field "description" (maybe string))
        (field "price" (maybe int))
        (Json.Decode.succeed Nothing)

-- MAIN
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


