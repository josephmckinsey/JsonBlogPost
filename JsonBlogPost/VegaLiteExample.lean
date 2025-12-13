import JsonSchema.Validation

def vegaliteSchemaStr : String := include_str "v6.4.1.json"

def vegaliteSchema : JsonSchema.Schema :=
  ((Lean.Json.parse vegaliteSchemaStr) >>= JsonSchema.schemaFromJson
  ).toOption.get!

def exampleJson : Lean.Json :=
  Option.get! <| Except.toOption <| Lean.Json.parse r##"{
  "$schema": "https://vega.github.io/schema/vega-lite/v6.json",
  "data": {"url": "data/seattle-weather.csv"},
  "mark": "bar",
  "encoding": {
    "x": {
      "timeUnit": "month",
      "field": "date",
      "type": "ordinal",
      "title": "Month of the year"
    },
    "y": {
      "aggregate": "count",
      "type": "quantitative"
    },
    "color": {
      "field": "weather",
      "type": "nominal",
      "scale": {
        "domain": ["sun", "fog", "drizzle", "rain", "snow"],
        "range": ["#e7ba52", "#c7c7c7", "#aec7e8", "#1f77b4", "#9467bd"]
      },
      "title": "Weather type"
    }
  }
}
"##

def testRun : IO Unit := do
  match JsonSchema.validate vegaliteSchema exampleJson with
  | .ok _ => IO.println "Success"
  | .error e => for error in e do
    IO.println error

--#eval testRun
