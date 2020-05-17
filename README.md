## PERSONAL STYLIST API

The Personal Stylist website was created to help women that want a personalized capsule wardrobe and don't know where to start!

The objective of this project was to practice Haskell by building an API as well as practicing React, Hooks, and Typescript in the Frontend. Another personal goal achieved with his project is working with UI styling and functional CSS, in this case, Tachyons.

## Contributors

[Leticia Duque](https://github.com/Lzduque) 

## Getting Started

If you want to clone both repos locally and test them, this is the Backend repo, and the Frontend repo is [here](https://github.com/Lzduque/personal-stylist).

1. Install Backend dependencies using the `stack setup` command. To run the tests in the back end use the `stack test` command.
2. Start Backend using the `stack build && stack exec personal-stylist-exe\`. The API will be served at <http://localhost:3000/>.

The instructions to use the Frontend are in [here](https://github.com/Lzduque/personal-stylist).

## How to Use this API

### Request

If you want to use this API, make a request to <https://personal-stylist-API.herokuapp.com/capsule/{input}>.

The input should be a JSON object encoded in [base64URL](https://simplycalc.com/base64url-encode.php), as the example:

```
https://personal-stylist-api.herokuapp.com/capsule/eyJzZWFzb24iOiJBdXR1bW5XaW50ZXIiLCJzdHlsZSI6IkNhc3VhbCIsIm51bWJlck9mT3V0Zml0cyI6IkZyb201MXRvNjAiLCJjb2xvcnMiOnsibWFpbnMiOlsiTmF2eSIsIldoaXRlIiwiTGlnaHRCbHVlIl0sIm5ldXRyYWxzIjpbIk9mZldoaXRlIiwiQmVpZ2UiXSwiYWNjZW50cyI6WyJMaWdodEdyZWVuIiwiTGlnaHRQaW5rIiwiUmVkIiwiQ29yYWwiXX0sInByZWZlcmVuY2VzIjpbIlNraXJ0cyIsIkRyZXNzZXMiLCJQYW50cyJdfQ==
```

The JSON object with the inputs that are necessary to generate the Capsule Wardrobe. It should have this structure:

```
{
    "season":"AutumnWinter",
    "style":"Casual",
    "numberOfOutfits":"From51to60",
    "colors":{
        "mains":["Navy","OffWhite","LightBlue"],
        "neutrals":["OffWhite","Beige"],
        "accents":["LightGreen","LightPink","Red","Coral"]
        },
    "preferences":["Skirts","Dresses","Pants"]
}
```

Possible values:

- Season: AutumnWinter | SpringSummer
- Style: Casual | Office
- NumberOfOutfits: From10to20 | From21to30 | From31to40 | From41to50 | From51to60 | From61to70 | From71to80 | From81to90 | From91to100 | From101to110 | From111to120 | From121to130 | From131to140 | From141to150 | From151to160 | From161to170 | From171to180 | From181to190 | From191to200
- Colors: White | OffWhite | Beige | Camel | Brown | Gray | Black | Navy | Blue | LightBlue | DarkGreen | Green | LightGreen | DarkYellow | Yellow | LightYellow | DarkPink | Pink | LightPink | DarkRed | Red | Coral | DarkOrange | Orange | LightOrange | DarkPurple | Purple | LightPurple
- Preferences: Skirts | Dresses | Pants | LeggingsPants | ShortsPants

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
    ["ShirtTop",3,[Navy","White","LightBlue"]],
    ["TShirtTankTop",3,[Navy","White","LightBlue"]],
    ["JeansPants",2,["Navy","White"]],
    ["DaySkirt",1,["Navy"]],
    ["DayDress",1,["LightGreen"]],
    ["Sweater",2,["Navy","White"]],
    ["Jacket",1,["Navy"]],
    ["Shoes",2,["OffWhite","Beige"]],
    ["RelaxedBag",1,["OffWhite"]]
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

## Additional Features to come

- UI style theme changes when clicking a button.
- Show images that represent a possible combination of clothes.
- Show color palette when colors are selected.

