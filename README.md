# shoppinglist-frontend-elm
Shoppinglist in elm

You need a http-rest service to get/put/patch/ json messages to.

**With json-server**

$ `json-server --watch server/shoppinglist1.json -p 5019 --cors --delay 1000`

file server/shoppinglist1.json :
`{
  "items": [
    {
      "id": 1,
      "name": "melk",
      "description": "Halfvolle melk",
      "unit": "liter"
    },
    {
      "id": 2,
      "name": "kefir",
      "description": "Biologsche kefir",
      "unit": "liter"
    }
  ]
}` 

The application can be started with elm-live :

$ `elm-live src/Main.elm --pushstate`

Look in localhost:8000/items
