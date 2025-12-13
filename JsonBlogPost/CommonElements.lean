import VersoBlog

open Verso Genre Blog
open Verso.Doc
section
open Verso Doc Elab ArgParse
open Lean
open Verso Output Html
open Template

def Verso.Output.Html.addAttrs (attrList : Array (String × String)) : Verso.Output.Html → Verso.Output.Html
| .tag (name : String) (attrs : Array (String × String)) contents =>
  .tag name (attrList.append attrs) contents
| x => x

/--
Creates an HTML element that renders a Vega-Lite visualization.
Takes a Lean.Json value containing the Vega-Lite specification and
generates the necessary HTML with embedded scripts.

The visualization will be rendered in a div element with a unique ID.
This function includes references to Vega, Vega-Lite, and Vega-Embed libraries from CDN.
-/
def vegaLiteElement (spec : Lean.Json) (id : String := "vis") : Html :=
  let specStr := spec.compress
  let scriptContent := s!"vegaEmbed('#{id}', {specStr});"
  {{
    <div style="display: block; margin-left: auto; margin-right: auto; width: fit-content;">
      <div id={{id}}></div>
      <script src="https://cdn.jsdelivr.net/npm/vega@6.2.0"></script>
      <script src="https://cdn.jsdelivr.net/npm/vega-lite@6.4.1"></script>
      <script src="https://cdn.jsdelivr.net/npm/vega-embed@7.0.2"></script>
      <script>
        {{Html.text false scriptContent}}
      </script>
    </div>
  }}
