import ProofWidgets.Data.Html
import Verso.Output.Html

namespace ProofWidgets

open Verso Output Html

def escapeString2 : Lean.Json → String
  | .null => "null"
  | .bool (b : Bool) => toString b
  | .num n => toString n
  | .str (s : String) => s
  | .arr _ => "array"
  | .obj _ => "obj"


/--
Converts `ProofWidgets.Html` to `Verso.Output.Html`.

This is a partial implementation. It handles `element` and `text` nodes.
For `component` nodes, it currently just renders the children, as Verso
doesn't have a corresponding concept for interactive components.
-/
partial def pwHtmlToVersoHtml (h : ProofWidgets.Html) : Verso.Output.Html :=
  match h with
  | .text s => .text true s
  | .element t attrs children =>
    let attrs' := attrs.map fun (k, v) => (k, ProofWidgets.escapeString2 v)
    let children' := children.map pwHtmlToVersoHtml
    .tag t attrs' (.seq children')
  | .component _hash _export _props children =>
    -- Verso doesn't have a notion of interactive components from ProofWidgets,
    -- so we just render the children as a fallback.
    .seq (children.map pwHtmlToVersoHtml)

end ProofWidgets

--#check pwHtmlToVersoHtml (compare_hilbert_curves 2 3).toHtml
