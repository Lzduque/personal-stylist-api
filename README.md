## PERSONAL STYLIST API

The Personal Stylist website was created to help women that want a personalized capsule wardrobe and don't know where to start!

The objective of this project was to practice Haskell by building an API as well as practicing React, Hooks, and Typescript in the Frontend. Another personal goal achieved with his project is working with UI styling and functional CSS, in this case, Tachyons.

## Contributors

[Leticia Duque](https://github.com/Lzduque) 

## Getting Started

If you want to clone both repos locally and test them, this is the Backend repo, and the Frontend repo is [here](https://github.com/Lzduque/personal-stylist).

1. Install Backend dependencies using the `stack setup` command. To run the tests in the back end use the `stack test` command.
2. Start Backend using the `stack build && stack exec personal-stylist-exe\`. The API will be served at <http://localhost:3000/>.

If you want to use the deployed version of the app, go to <https://lzduque.github.io/personal-stylist/>. 
The instructions to use the Fronten are in [here](https://github.com/Lzduque/personal-stylist).

## How to Use this API

### Request

If you want to use this API, make a request to <https://personal-stylist-API.herokuapp.com/capsule/{input}>.

The input should be a JSON object encoded in [base64URL](https://simplycalc.com/base64url-encode.php), as the example:

```
https://personal-stylist-api.herokuapp.com/capsule/eyJzZWFzb24iOiJBdXR1bW5XaW50ZXIiLCJzdHlsZSI6IkNhc3VhbCIsIm51bWJlck9mT3V0Zml0cyI6IkZyb20xODF0bzE5MCIsImNvbG9ycyI6WyJXaGl0ZSIsIk9mZldoaXRlIiwiR3JheSIsIkJsYWNrIiwiQmVpZ2UiLCJDYW1lbCJdLCJwcmVmZXJlbmNlcyI6WyJTa2lydHMiLCJEcmVzc2VzIiwiUGFudHMiLCJIaWdoSGVlbHMiLCJMZWdnaW5nc1BhbnRzIl19
```

The JSON object with the inputs that are necessary to generate the Capsule Wardrobe. It should have this structure:

```
{
    "season":"AutumnWinter",
    "style":"Casual",
    "numberOfOutfits":"From181to190",
    "colors":["White","OffWhite","Gray","Black","Beige","Camel"],
    "preferences":["Skirts","Dresses","Pants","HighHeels","LeggingsPants"]
}
```

Possible values:

- Season: AutumnWinter | SpringSummer
- Style: Casual | Office
- NumberOfOutfits: From10to20 | From21to30 | From31to40 | From41to50 | From51to60 | From61to70 | From71to80 | From81to90 | From91to100 | From101to110 | From111to120 | From121to130 | From131to140 | From141to150 | From151to160 | From161to170 | From171to180 | From181to190 | From191to200
- Colors: White | OffWhite | Beige | Camel | Brown | Gray | Black | Navy | Blue | LightBlue | DarkGreen | Green | LightGreen | DarkYellow | Yellow | LightYellow | DarkPink | Pink | LightPink | DarkRed | Red | Coral | DarkOrange | Orange | LightOrange | DarkPurple | Purple | LightPurple
- Preferences: Skirts | Dresses | Pants | HighHeels | LeggingsPants

Colors and Preferences are always lists, even if they contain only one value.
You must send at least one color and one preference.
One of the selected references must be Skirts, Dresses or Pants.

If any of these constraints are not obeyed, the server will return an error, like this one:

```
{ 
    error = True, 
    message = "No capsule can be generated within this range, for these parameters. Please, change the number of outfits." 
}
```

### Response

This API will respond with an Capsule Wardrobe for display purposes as a JSON. All pieces will be grouped into types, their quantities will be displayed, as well as their suggested colors:

```
[
    ["Shirt",9,["White","OffWhite","Gray","Black","Beige","Camel"]],
    ["Jeans",1,["White"]],
    ["Leggings",2,["White","OffWhite"]],
    ["DaySkirt",2,["White","OffWhite"]],
    ["DayDress",2,["Beige","Camel"]],
    ["Sweater",1,["White"]],
    ["Cardigan",1,["White"]],
    ["Jacket",1,["White"]],
    ["TrenchCoat",1,["White"]],
    ["Flats",2,["White","OffWhite"]],
    ["AnkleBoots",1,["White"]],
    ["Boots",2,["White","OffWhite"]],
    ["Sneakers",2,["White","OffWhite"]],
    ["RelaxedBag",3,["Black","Beige","Camel"]]
]
```

Every response will be an array of pieces, represented like this:

```
[(String,Int,[Colors])]
```

## Dependencies

- aeson >= 1.4.6.0
- hspec
- scotty
- for a complete list check the "package.yaml" file.

## Extensions:

- OverloadedStrings
- DeriveGeneric
- DeriveAnyClass
- RecordWildCards
- NamedFieldPuns
- MultiWayIf

## Final Product Screenshots

### Home page
!["Home page"](https://github.com/Lzduque/personal-stylist/blob/master/public/APP_HOME_PAGE.png?raw=true)

### Options Selection
!["Recipe Book Page"](https://github.com/Lzduque/personal-stylist/blob/master/public/APP_SELECTION.png?raw=true)

### Wardrobe Generator
!["Wardrobe Generator"](https://github.com/Lzduque/personal-stylist/blob/master/public/APP_WARDROBE.png?raw=true)


## Additional Features to come

- UI style theme changes when clicking a button.
- Show images that represent a possible combination of clothes.
- Show color palette when colors are selected.

