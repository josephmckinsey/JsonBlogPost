import VersoBlog
import JsonBlogPost.CommonElements
import LeanUri
import JsonSchema.Validation

open Verso Genre Blog
open Verso.Doc

section
open Verso Doc Elab ArgParse
open Lean
open Verso Output Html
open Template

def blueprintImage : Html :=
  {{<img src="blueprint_section.png" alt="TypeScript lets you combine types to make array types, tuple types, union types, and intersection types" style="width: 600px; display: block; margin-left: auto; margin-right: auto"/>}}

-- Example VegaLite specification
def exampleVegaLiteSpec : Lean.Json :=
  Option.get! <| Except.toOption <| Lean.Json.parse r##"{
  "$schema": "https://vega.github.io/schema/vega-lite/v6.json",
  "data": {"url": "https://vega.github.io/vega-lite/data/seattle-weather.csv"},
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

-- Create the VegaLite visualization element
def exampleVegaLiteVis : Html :=
  vegaLiteElement exampleVegaLiteSpec "weather-vis"

end

set_option pp.rawOnError true

-- I removed some definitions, and now this is necessary...
set_option maxHeartbeats 800000

#doc (Page) "Type-safe JSON in Lean" =>

It's easier than ever to create extremely interactive plots using JavaScript, but it's quite
difficult to make quick plots in Lean. Since there's line, scatter, area, labels, legends, etc., I was thinking
of making a wrapper around [VegaLite](https://vega.github.io/vega-lite/). The plan was to generate Lean code from [VegaLite's
JSON Schema](https://vega.github.io/schema/vega-lite/v6.4.1.json), and then convert those structures to JSON for
VegaLite's consumption. Things did not go according to plan.

I ended up making a fork of [`json-schema-lean`](https://github.com/josephmckinsey/json-schema-lean/), a URI library
[`lean-uri`](https://github.com/josephmckinsey/lean-uri), and then a subtyping framework [`json-subtyping`](https://github.com/josephmckinsey/json-subtyping).

# Step 1: JSON Schema in Lean

There was already a [json-schema-lean](https://github.com/CAIMEOX/json-schema-lean/) library, which was a GSoC 2024 project.
The main branch is currently incomplete, but there's good [testing infrastructure](https://github.com/bowtie-json-schema/bowtie).
I figured it wouldn't be _too_ difficult to complete.

## JSON Schema

JSON Schema lets you use a schema, written in JSON, to check JSON documents. The schema language lets you
constrain JSON and reference definitions across files. Tools like OpenAPI and `openapi-generator` let you
describe REST endpoints and generate stubs in different languages. In my personal opinion, the generated
code is like merely ok, but it's a real business technology.

I have primarily used JSON Schema Draft 7, which has a [pretty good spec](https://json-schema.org/draft-07/draft-handrews-json-schema-01).
With the spec in hand, I implemented it as fast as I could, and used a lot of Claude Code to implement the features,
especially once it got really repetitive. I discovered that there were a lot of features which I would never use, which
really complicate the schema:

- You can pattern match regexes in strings.
- You can have loops in your references, which you have to guide against.
- There are properties, pattern properties, additional properties, etc. These all interact in ways I find confusing.

The schema turned into a [big mutual inductive type](https://github.com/josephmckinsey/json-schema-lean/blob/27846b7410a8fd0955ce09b8c36b3904eb895a0b/JsonSchema/Schema.lean#L50),
since `true` is in fact a JSON schema which always passes.

Luckily the available tests are really comprehensive! Lean has a few regex libraries to choose from like [Regex](https://github.com/bergmannjg/regex).
And writing loop detection is not that hard to implement. Sadly, there was still a missing element.

## Step 1.1: URIs

[*Uniform Resource Identifiers*](https://en.wikipedia.org/wiki/Uniform_Resource_Identifier) are special strings that serve
as IDs with extra information. [URLs](https://en.wikipedia.org/wiki/URL) are subtype of URIs. A URL is a URI plus the protocol
to retrieve the URL. The [URI spec (RFC 3986)](https://www.rfc-editor.org/rfc/rfc3986) breaks URI into `URI = scheme ":" ["//" authority] path ["?" query] ["#" fragment]`,
and it also defines the percent-encoding and decoding (like why we replace spaces with `%20`). Different schemes can then
have special rules like for ordinary URLs. There's also *relative URIs* which let you look at different parts of
the path.

So why does this have to do with JSON Schema? Well, `$id`s and `$ref` follow the rules of URIs. You can use different
protocols, you can use relative paths, and the fragments point to definitions within files. Since Lean didn't have
a URI library, I implemented one. Since testing is confusing, I mainly have some examples from the RFC and looked at the
Haskell equivalents. Implementation was mainly just feeding the spec into AI again and then rewriting it when it got ugly.
Since I prefer parser combinators in all languages, I started with some hand-written [`Std.Internal.Parsec.String`](https://leanprover-community.github.io/mathlib4_docs/Std/Internal/Parsec/String.html),
and tried to work within that monad.

## Step 1.1.1: Testing

Since URIs didn't already have testing, I really wanted some good testing here. Lean has a way to hook up an executable
to [`lake test`](https://github.com/leanprover-community/mathlib4/wiki/Setting-up-linting-and-testing-for-your-Lean-project),
so I wanted something for that. I didn't really like [LSpec](https://github.com/argumentcomputer/LSpec), which appears
to be the only testing library in Lean... So I decided to write my own instead. I decided to put it in a Test monad based
on IO, where you can log tests, group tests, etc.

```leanInit testing
```

```lean testing
inductive TestTree where
  | test (name : String) (passed : Bool) (skipped : Bool := false) (message : Option String := none)
  | group (name : String) (children : Array TestTree) (skipped : Bool := false)
deriving Inhabited, Repr

structure TestState where
  stack : List (String × Array TestTree) := [ ("", #[]) ] -- (group name, children)
  filter : String → Bool := fun _ => true
deriving Inhabited

abbrev TestM := StateT TestState IO
```

This worked pretty fine for what I needed, and I ended up using the same helpers for my fork of `json-schema-lean`.
Some tiny features like filtering or whatever are easy to reproduce. I doubt I'll use this long-term, since having IO in all your tests is a blessing and a curse.

Once I had the basic URI interface, it was mainly a matter of repeatedly testing the `$ref` and `$id` cases until I got it
to mostly work.

### Json Pointer Fragments

On a side note, [RFC 6901](https://www.rfc-editor.org/rfc/rfc6901) describes how JSON pointer fragments can be used to point
to specific parts of a JSON object.

## What doesn't work?

Not all the JSON Schema tests pass, because several of them require you to read referenced JSON schemas on the fly. Lean
does not have a good URL library, so I just decided to give up.

# Step 2: Json Schema Code Gen

Now that I parse and understand all the features of JSON schema, I decided to try generating Lean structs from JSON schema.
My strategy plodded along trying to parse into `Except String Std.Format`, where first I would try the simpler
abbreviations and only later try to generate `structure`s. [`Std.Format`](https://lean-lang.org/doc/api/Init/Data/Format/Basic.html#Std.Format)
is Lean's way to handle all the identation problems with just templating strings; this is so much nicer than I thought it would be.
People who like macros may be asking "Why not use macros?" When I figure out how to make comments and print them out
nicely, then I might switch to macros instead of essentially string manipulation.

Eventually, I needed too much context and recursion and custom toJson instances, so it ended up being
`SchemaGen TypeDefinition` where `SchemaGen` has the context and error handling and `TypeDefinition` has
all the stuff that might need to be propagated up. Using a custom monad name is really useful, and I
can understand why the monad transformer life is so real.

```lean testing
/-- A complete type definition including the type declaration and optional JSON instances -/
structure TypeDefinition where
  /-- The main type declaration (structure, inductive, or abbrev) -/
  typeDecl : Std.Format
  /-- Optional FromJson instance implementation -/
  fromJsonImpl : Option Std.Format := none
  /-- Optional ToJson instance implementation -/
  toJsonImpl : Option Std.Format := none
  /-- Nested type definitions that should be prepended before this definition -/
  dependencies : List TypeDefinition := []
  /-- Extra doc comment for use in structures and inductives -/
  extraDocComment : Option Std.Format := none
deriving Inhabited

/-- Configuration for code generation -/
structure Config where
  /-- How to sanitize names to valid Lean identifiers -/
  sanitizeName : String → String
  /-- Indentation string -/
  indent : String := "  "
  /-- Whether to generate FromJson instances -/
  generateFromJson : Bool := false
  /-- Whether to generate ToJson instances -/
  generateToJson : Bool := false
  /-- Whether to include the base URI filename in generated type names.
      When true: user.json with definition "Address" → "UserAddress"
      When false: user.json with definition "Address" → "Address" -/
  includeBaseNamePrefix : Bool := false

/-- Identifies a schema by its canonical URI and path -/
structure SchemaID where
  baseURI : LeanUri.URI
  path : List String
deriving BEq, Hashable, Inhabited

/-- Extended code generation context with reference support -/
structure CodeGenContext where
  /-- Resolver for looking up schemas -/
  resolver : Resolver
  /-- Mapping from SchemaID to generated type name -/
  nameMap : Std.HashMap SchemaID String
  /-- Configuration -/
  config : Config
  /-- Base URI which gets updated as we traverse -/
  baseURI : LeanUri.URI
  /-- Current type name being generated (for error reporting) -/
  currentTypeName : Option String := none
  /-- Current schema ID being processed (for error reporting) -/
  currentSchemaID : Option SchemaID := none

abbrev SchemaGen := ReaderT CodeGenContext (Except String ·)

def SchemaGen.run (gen : SchemaGen α) (ctx : CodeGenContext)
    : Except String α := ReaderT.run gen ctx
```

To generate mutually recusive types, I wanted to use
[Tarjan's strongly connected components](https://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm),
to detect the definition ordering and "real" mutual dependencies.
The implementation can essentially automated as there exist examples of it in [Mathlib](https://github.com/leanprover-community/mathlib4/blob/560872a203ef726bf76117856ece2872f8cff918/Mathlib/Tactic/Order/Graph/Tarjan.lean#L17-L29)
and [Lean.SCC](https://github.com/leanprover/lean4/blob/9d4ad1273f6cea397c3066c2c83062a4410d16bf/src/Lean/Util/SCC.lean#L25-L28).
Although it would have been fun to write and debug it myself, throwing those sources at Claude Code worked just as well ☹️.

Aside from that, implementation was really annyoing, since there are
so many different special cases, inductive, abbreviations, inductives, etc. A small
change to a schema may no longer have as nice a representation in Lean.

# Step 3: Code Gen with VegaLite

How did code gen work with the VegaLite?

* Mutual induction is still broken, oops?
* It would take several minutes to compile, and it's >15,000 lines.
* All the code is too ugly to use.

Once I saw `RectConfig`, I realized this was going not going to be fun.

```
structure RectConfig where
  /-- The horizontal alignment of the text or ranged marks (area, bar, image, rect, rule). One of `"left"`, `"right"`, `"center"`.

    __Note:__ Expression reference is *not* supported for range marks. -/
  align : Option (Align ⊕ ExprRef) := none
  /-- The rotation angle of the text, in degrees. -/
  angle : Option (Float ⊕ ExprRef) := none
  /-- A boolean flag indicating if [ARIA attributes](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA) should be included (SVG output only). If `false`, the "aria-hidden" attribute will be set on the output SVG element, removing the mark item from the ARIA accessibility tree. -/
  aria : Option (Bool ⊕ ExprRef) := none
  /-- Sets the type of user interface element of the mark item for [ARIA accessibility](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA) (SVG output only). If specified, this property determines the "role" attribute. Warning: this property is experimental and may be changed in the future. -/
  ariaRole : Option (String ⊕ ExprRef) := none
  /-- A human-readable, author-localized description for the role of the mark item for [ARIA accessibility](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA) (SVG output only). If specified, this property determines the "aria-roledescription" attribute. Warning: this property is experimental and may be changed in the future. -/
  ...
```

Remembering the ordering, using `.inl`, and trying to remember what
`Align` is really difficult. It is probably easier just to try "left"
and run it than figure out it's `.some (.inl .left)`.

# Back to the drawing board

You know what would be really convenient? If you could just determine if the JSON schema worked during compile time.
If the entire JSON is known at compile time, this is completely trivial, since you can use `by native_decide` to check
whether `validate schema json` passes. What if a theorem about the struture of your JSON data let you know if `validate schema json` would pass? That sounds great,
but honestly just checking if `validate` _terminates_ is already pretty difficult. If we use a more restrictive schema language,
it should be easier to prove theorems about validation _ahead_ of time.

I put this idea to the side for a while, but I couldn't let it go. After using [TypeScript](https://www.typescriptlang.org/) a bit professionally, I figured
that it actually might not be too hard to crib TypeScript's homework and use a fragment of it to check JSON. One day
I carved out a few hours to write up a [blueprint](https://github.com/josephmckinsey/json-subtyping/blob/main/blueprint/plan.pdf)
in [Typst](https://typst.app/) (a LaTeX alternative). This was also a more fun idea than improving JSON Schema validation.

## TypeScript

JavaScript has only a few main types: `null, string, number, boolean, object, undefined`. Many "types" in JavaScript are
really just special objects like "functions". TypeScript lets you gradually define types that assign properties of
variables at specific locations, and then it will check properties "x has property 'id'" at compile-time by inspecting
the control flow graph. TypeScript does not try _particularly_ hard to be sound, though.

## Blueprint

I've never taken any course on type theory, so I probably did not use proof tree rules correctly, but
I wrote a bunch of them out anyways. I found it helpful especially for getting the object subtyping rules correctly.

::::blob blueprintImage
::::

For objects, JSON objects are always allowed to have extra fields, which becomes a lot more annoying when you are
checking whether fields exist in big type unions. More on that later.

## Strange challenges

Lean has a built-in JSON type at [`Lean.Json`](https://leanprover-community.github.io/mathlib4_docs/Lean/Data/Json/Basic.html#Lean.Json),
which is used for the language server among other applications. Since the JSON type is recursive, and the recursion is contained
in types like Array and maps, Lean identifies it as a ["nested inductive type"](https://lean-lang.org/doc/reference/latest/The-Type-System/Inductive-Types/#nested-inductive-types).

Lean handles nested inductives with more care and tries its best to make it appear normal. They are not normal. Their
default induction rules are a real pain to use, since it includes the rules for Arrays and maps. Worse, you cannot include
some data in inductive data types.

### `Std.TreeMap.Raw`

Since we are using nested inductives, Lean can't use [`Std.TreeMap`](https://leanprover-community.github.io/mathlib4_docs/Std/Data/TreeMap/Basic.html#Std.TreeMap),
so instead we have to use [`Std.TreeMap.Raw`](https://leanprover-community.github.io/mathlib4_docs/Std/Data/TreeMap/Raw/Basic.html#Std.TreeMap.Raw),
which has no proof the map is "well-formed". Sadly, this means that we lose a lot of the theorems for `Std.TreeMap`.
I carefully tried to work around this by asserting properties about the [`.get?`](https://leanprover-community.github.io/mathlib4_docs/Std/Data/TreeMap/Raw/Basic.html#Std.TreeMap.Raw.get?)
call or by constructing maps using [`ofList`](https://leanprover-community.github.io/mathlib4_docs/Std/Data/TreeMap/Raw/Basic.html#Std.TreeMap.Raw.ofList),
which is known to be well formed.

### Proving anything about JSON

Given that JSON is so annoying, it is somewhat surprising it isn't _too_ hard to prove things about JSON.
The primary challenge for any type with weird structure is proving termination. Once you have termination,
you can define custom induction routines by proving induction terminates.

I spent a day reading [Lean's reference manual on well-founded recursion](https://lean-lang.org/doc/reference/4.26.0-rc2/Definitions/Recursive-Definitions/#well-founded-recursion)
and tried simplifying termination goals on a `Json.beq` implementation. When you iterate `.obj map : Lean.Json`,
you want to know that when `(k, v) ∈ map`, then the size of `x` is smaller than map, which is smaller than `.obj map`.
Lean provides a `sizeOf` definition automatically for most types where this property is pretty much always true.
During automatic termination checking, Lean attaches the membership proofs with [`List.attach`](https://leanprover-community.github.io/mathlib4_docs/Init/Data/List/Attach.html#List.attach),
and apply [`List.sizeOf_lt_of_mem`](https://leanprover-community.github.io/mathlib4_docs/Init/Data/List/BasicAux.html#List.sizeOf_lt_of_mem).

Although this "attach" framework works well for lists and arrays, the types don't work as nicely for `Std.TreeMap`.
After asking a bunch of questions to Claude Code, I learned about [`Std.DTreeMap.Internal.Impl.toListModel`](https://leanprover-community.github.io/mathlib4_docs/Std/Data/DTreeMap/Internal/Def.html#Std.DTreeMap.Internal.Impl.toListModel).
This function is a really inefficient way to flatten a list, but it's a really easy to reason about. It's also proved equivalent
to `.toList`, which is more efficient. Claude Code was essentially able to one-shot the `sizeOf` lemmas:

```lean testing
-- sizeOf lemmas for TreeMap internals
open Std.DTreeMap.Internal in
theorem Impl.sizeOf_lt_of_mem {α : Type u} {β : α → Type v} [SizeOf α] [(a : α) → SizeOf (β a)]
    {t : Impl α β} {k : α} {v : β k} (h : ⟨k, v⟩ ∈ t.toListModel) :
    sizeOf v < sizeOf t := by
  induction t with
  | leaf => simp [Impl.toListModel] at h
  | inner sz k' v' l r ihl ihr =>
    simp only [Impl.toListModel_inner, List.mem_append, List.mem_cons] at h
    rw [Impl.inner.sizeOf_spec]
    rcases h with hl | heq | hr
    · have := ihl hl; omega
    · cases heq; omega
    · have := ihr hr; omega

theorem TreeMap.Raw.sizeOf_lt_of_mem {α β : Type} {cmp : α → α → Ordering}
    [SizeOf α] [SizeOf β] {t : Std.TreeMap.Raw α β cmp} {k : α} {v : β}
    (h : ⟨k, v⟩ ∈ t.inner.inner.toListModel) :
    sizeOf v < sizeOf t := by
  have h1 : sizeOf t = 1 + sizeOf t.inner := Std.TreeMap.Raw.mk.sizeOf_spec t.inner
  have h2 : sizeOf t.inner = 1 + sizeOf t.inner.inner := Std.DTreeMap.Raw.mk.sizeOf_spec t.inner.inner
  have h3 := Impl.sizeOf_lt_of_mem h
  omega
```

So my strategy was to use `.toList.attach` which would attach the membership lemmas, which I could then use with `.sizeOf_lt_of_mem`
to prove that the size of recursive arguments decreases. In retrospect, this was pretty simple.
Now we can implement better JSON induction methods:

```lean testing
set_option linter.unusedVariables false in
open Lean (Json) in
def Json.recOn {motive : Json → Sort u}
    (x : Json)
    (null : motive .null)
    (bool : ∀ b, motive (.bool b))
    (num : ∀ n, motive (.num n))
    (str : ∀ s, motive (.str s))
    (arr : ∀ (a : Array Json), (∀ j, j ∈ a → motive j) → motive (.arr a))
    (obj : ∀ (o : Std.TreeMap.Raw String Json compare),
           (∀ field value, ⟨field, value⟩ ∈ o.inner.inner.toList → motive value) →
           motive (.obj o)) :
    motive x :=
  match x with
  | .null => null
  | .bool b => bool b
  | .num n => num n
  | .str s => str s
  | .arr a => arr a (fun j jMem => Json.recOn j null bool num str arr obj)
  | .obj o => obj o (fun k v vMem => Json.recOn v null bool num str arr obj)
termination_by x
decreasing_by
  · suffices sizeOf j < sizeOf a by simp +arith; omega
    exact Array.sizeOf_lt_of_mem jMem
  suffices sizeOf v < sizeOf o by simp +arith; omega
  apply TreeMap.Raw.sizeOf_lt_of_mem
  rw [<-Std.DTreeMap.Internal.Impl.toList_eq_toListModel]
  exact vMem
```

Custom recursors like this lets you split up recursive definitions really easily as long as you are able
to take and apply hypotheses like `j ∈ a`. I appreciate not making every definition partial.

### List equality was a pain

Lawful equality already exists for `List α`, but it expects an existing instance of equality for `α`.
When I'm trying to define a more lawful `BEq Json`, I wanted to use the existing infrastructure for lists.
I believe there is a way to do it using similar `.attach` membership properties, but at the time, I ran out
of patience and cleverness, so I reimplemented it instead. I also now believe that writing your definitions
in the typeclass instance directly is bad if you ever want to unfold those definitions.

## Subtyping

After handling all those JSON difficulties, most of my other problems were actually about the mathematical objects
I was constructing instead--a welcome change. From the beginning, I wanted to be able to bundle the type `t : JsonType`,
the check, and the JSON into an object `TypedJson t`. Then when you wanted to check if it also satisfied `t'`,
you could run a decision procedure `t.subtype t'` to prove `t.check x → t'.check x`. This would allow you to use
JSON in more general contexts without having to care about the exact types involved.

I applied a bit of cleverness by bundling the proof into the return type of the subtype checker:

```
inductive DecideSubtype (t1 t2 : JsonType) where
  | none : DecideSubtype t1 t2
  | isSubtype : (∀j, t1.check j = true → t2.check j = true) → DecideSubtype t1 t2
```

This allowed me to do a lot of the proof plumbing within the convoluted logic of the subtype checking. Proving that subtyping
worked separately involved so much casework, it was so much easier to piggy-back off the existing casework. Truly, dependent
types are wonderful!

There was a lot of rewriting and sublemmas for all the different cases, especially for object subtyping, but I was mainly
correcting the plan for object subtyping, ensuring that required and optional types behaved correctly, and trying a bunch
of subcases. I don't find the details interesting, and I wish LLMs could have done even more here.

## Object Construction

Given `TypedJson`, `x1 x2 : TypedJson t`, I would like to be able to construct typed objects during compile time. This
might actually be the ideal use case for heterogeneous lists, where each element has a different type.

I carefully put the key-values as type parameters, so I can access them at compile time more easily, then the actual typed
JSON is included as a field.

```
/-- Heterogeneous list of object fields indexed by schema.
    Each field carries a TypedJson value with proof that it checks against its type. -/
inductive ObjectFields : List (String × JsonType) → Type where
  | nil : ObjectFields []
  | cons {ty : JsonType} {rest : List (String × JsonType)} (name : String) :
      TypedJson ty → ObjectFields rest → ObjectFields ((name, ty) :: rest)
```

Then I could guarantee objects would pass validation (also you have to prove the keys have no duplicates):

```
/-- Construct an object from ObjectFields -/
def mkObj {req : List (String × JsonType)} (fields : ObjectFields req)
    (noDups : req.Pairwise (fun a b => ¬compare a.1 b.1 = .eq) := by native_decide) :
    TypedJson (.object req []) :=tring × JsonType)} (fields : ObjectFields req)
    (noDups : req.Pairwise (fun a b => ¬compare a.1 b.1 = .eq) := by native_decide) :
    TypedJson (.object req []) := sorry
```

I had Claude Code make a macro that would construct ObjectFields using `obj{}` notation, since I could just point
it at the existing macro for list syntax `[a, b, c]`.

## Narrowing

When you are within if statements, `TypeScript` is capable of narrowing unions to specific elements:

```
type Person =
    | { age: number, id: number }
    | { dob: Date }

const person = getPerson();

if ("age" in person) {
    // person has type {age : number, id: number}
    console.log("person.age =", person.age);
    console.log("person.id =", person.id);
}
```

This is really helpful, but it is also more fraught with difficulty than it appears. Since objects may have
_extra_ fields in JSON, it is possible to have `person = {dob: ..., age: 10}`. Now TypeScript mistakenly
believes that `person.id` is defined. When I learned about this, I decided that I should stop looking at the TypeScript
source code, and also I should probably stop at checking field properties.

I created checkers for whether properties can match or not:

```
def JsonType.canMatchPropertyStr (t : JsonType) (key : String) (str : String) : Bool :=
  t.canBeObject &&
  match t.getKey? key with
  | .some kt => kt.check str
  | .none => true -- we have no information about key
```

If `x` has property `key` with value `str`, then I prove that `t.check x = true` implies `t.canMatchPropertyStr key str = true`.
Similarly, if `x` does not have property `key` with value `str`, then I can prove `t.canMismatchPropertyStr key str = true`.

If `t = t1 ||| t2 ||| t3 ||| t4`, then I can turn that into a list of `[t1, t2, t3, t4]` for which any of them must check,
now if `t.check x = true`, then I know that say `t1.check x = true`, so `t1.canMatchPropertyStr key str`. The upshot
is that I can filter `[t1, t2, t3, t4]` for `canMatchPropertyStr`, and one of them will match `x` still.

```
theorem JsonType.filterUnion_correctness
    {t : JsonType} {f : JsonType → Bool} {x : Json}
    (h : t.check x = true)
    (h' : ∀ t', t'.check x = true → f t' = true) :
    (t.filterUnion f).check x = true := by sorry
```

## It's a cool idea!

Given that my day job involves more Julia than anything else, I know how helpful subtyping can be. For many applications,
such as plotting, using gradual subtyping is a natural fit as people add more features, extensions, etc. to existing
code. I'm especially optimistic on the potential for domain-specific typing for dataframes. Using restrictive
dataframe types is incredibly annoying, and being able to gradually type existing messy dataframe code would be delightful.
Currently, no existing dataframe libraries have such a subtyping system. Something to look into.

# Well, what about plotting?

I still want to plot something, so how can I do that? I'm not so gung-ho about translating JSON Schema into my JsonType,
so for now, I'll just do runtime checking with the JSON Schema.

In this case, the approach is relatively simple, we load in the Vegalite schema with `include_str`, then we can parse
it and use it to validate.

```lean testing
def exampleVegaLite : Lean.Json :=
  Option.get! <| Except.toOption <| Lean.Json.parse r##"{
  "$schema": "https://vega.github.io/schema/vega-lite/v6.json",
  "data": {"url": "https://vega.github.io/vega-lite/data/seattle-weather.csv"},
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
def vegaliteSchemaStr : String := include_str "v6.4.1.json"

def vegaliteSchema : JsonSchema.Schema :=
  ((Lean.Json.parse vegaliteSchemaStr) >>= JsonSchema.schemaFromJson
  ).toOption.get!

#eval JsonSchema.validate vegaliteSchema exampleVegaLite
```

We can then embed the JSON into an HTML tag and display it as needed:

::::blob exampleVegaLiteVis
::::

It should then be relatively simple to make this a [Lean widget](https://lean-lang.org/examples/1900-1-1-widgets/).
However, the syntax for constructing JSON in Lean is very unergonomic, since we can't combine string formatting
with JSON syntax easily. Creating a custom syntax for JavaScript strings in general might be an even easier
solution to using lots of JavaScript libraries in widgets.
