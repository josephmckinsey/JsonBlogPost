import VersoBlog
import JsonBlogPost.CommonElements
import LeanUri
import JsonSchema.Validation

open Verso Genre Blog
open Verso.Doc

#exit
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

To have type-safe JSON, you can create specialized data types, or you can validate during runtime.
I wanted to explore using [Vega-Lite](https://vega.github.io/vega-lite/) for plotting in Lean, and
I wanted to have some sort of check. So in one giant PR, I mostly finished the [json-schema-lean](https://github.com/CAIMEOX/json-schema-lean/)
library, since Vega-Lite has a [schema](https://vega.github.io/schema/vega-lite/v6.4.1.json). Then I decided to
try generating code from JSON Schema. Along the way, I had to make a URI library
[`lean-uri`](https://github.com/josephmckinsey/lean-uri). Skip to {ref plot}[the end] to see usage.

I became dissatisfied with how JSON Schema code generation produces unergonomic types in almost every language,
no matter what I try. Meanwhile, most languages don't track runtime validation. If you know that a JSON passed validation, then
the compiler can't guarantee any properties of your data. But in Lean, we can use the validation as an assumption
in a theorem. Then we can prove that fields are populated, or go backwards and prove that validation would check if we
ran it. This idea wouldn't leave me, so I also tried to implement that:
[`json-subtyping`](https://github.com/josephmckinsey/json-subtyping).

Despite dependent typing being really great since you can prove theorems, it's not ideal for
things like plotting libraries. When every union is implicitly tagged with a discrimintaor,
as in Lean, you have to be more aware during union construction. Subtyping helps you avoid that.
Additionally, you don't need to type every object if you have gradual typing.

I have particular hope for using dependent typing to provide guarantees for dataframe libraries, SQL results, and
tensor manipulation. Those are places where traditional type systems have dropped the ball, but we could track
the properties with a custom schema. Using dependent types can invite "dependent type hell", but I had luck
with just attaching the validation check as a `Prop`, although I may pay for my sins in the future.

# Step 1: JSON Schema in Lean

Luckily there was already a [`json-schema-lean`](https://github.com/CAIMEOX/json-schema-lean/) library--a GSoC 2024 project.
The main branch was incomplete, but there was nice [testing infrastructure](https://github.com/bowtie-json-schema/bowtie).
I figured it wouldn't be _too_ difficult to complete.

## JSON Schema

JSON Schema is a schema spec; specs are written in JSON, and schema implementations let you validate JSON.
The schema language lets you constrain JSON values, take unions of schemas, reference external definitions, etc.
Tools like OpenAPI and `openapi-generator` generate stubs in different languages. Personally, the generated
code is substandard but more or less workable depending on the language and application.

I have primarily used JSON Schema Draft 7, which has a [relatively short spec](https://json-schema.org/draft-07/draft-handrews-json-schema-01).
With it in hand, I implemented most of the features within a week or two, using a lot of Claude Code for most features,
especially once it got really repetitive. I discovered that there were a lot of features I never use nor want to:

- You can pattern match regexes in strings.
- You can have loops in your schema references, which you have to guard against.
- There are properties, pattern properties, additional properties, etc. These all interact with each other.

The schema turned into a [big mutual inductive type](https://github.com/josephmckinsey/json-schema-lean/blob/27846b7410a8fd0955ce09b8c36b3904eb895a0b/JsonSchema/Schema.lean#L50),
since schemas have unusual options like just `true` or nested `if-then-else` schemas.

Bowtie's comprehensive tests saved me a bunch. There were quite a few parts around references where the spec confused me.
Lean has a few regex libraries to choose from like [`Regex`](https://github.com/bergmannjg/regex). That also saved me a
lot of trouble since regex is not an isolated feature; it can be used to select schemas for properties. Sadly there was
another missing element leading to yet another side-quest.

## Step 1.1: URIs

[*Uniform Resource Identifiers*](https://en.wikipedia.org/wiki/Uniform_Resource_Identifier) are special strings that serve
as semantic IDs. They don't quite provide a unique value, but they often tell you something about some digital resource.
[URLs](https://en.wikipedia.org/wiki/URL) are a subtype of URIs. A URL is a URI plus the protocol to retrieve the URL.
The [URI spec (RFC 3986)](https://www.rfc-editor.org/rfc/rfc3986) breaks URI into `URI = scheme ":" ["//" authority] path ["?" query] ["#" fragment]`,
and it defines the percent-encoding and decoding (like why Google replaces spaces with `%20` in the results URL).
Different schemes can then have special rules on top like URL domains or base64-encoding. There's also *relative URIs*
to access "nearby" resources.

So what does this have to do with JSON Schema? Well, `$id`s and `$ref`s follow the rules of URIs. You can use different
protocols, you can use relative paths, and fragments point to definitions within files. Since Lean didn't have
a URI library, I implemented one. Since testing is not as well centralized, I mainly picked examples from the RFC and looked at
Haskell equivalents. For implementation, I started with hoping Claude could read and execute the spec, then rewriting it when it got ugly.
Since I hold parser combinators close to heart, I started with some hand-written [`Std.Internal.Parsec.String`](https://leanprover-community.github.io/mathlib4_docs/Std/Internal/Parsec/String.html),
and tried to work within that monad, making the code more uniform.

## Step 1.1.1: Testing

Since URIs (unlike URLs) didn't seem to have a nice testing framework already, I really wanted to count on my own testing.
Lean has a way to hook up an executable to [`lake test`](https://github.com/leanprover-community/mathlib4/wiki/Setting-up-linting-and-testing-for-your-Lean-project),
and that attracted me too. I didn't really like [LSpec](https://github.com/argumentcomputer/LSpec), which appears
to be the only testing library in Lean. My conclusion: I decided to write a little testing library. I decided to put it in a
`Test` monad based on IO, where you can log tests to monad state, group tests, etc.

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

This worked fine for what I needed, and I reused the helpers for testing code gen on a branch of `json-schema-lean`.
Some tiny features like filtering or whatever are simple. I doubt I'll use this long-term, since having IO in all your
tests is a blessing and a curse. There are too many features I want in a proper testing library to use my hacky version
forever.

Once I had the basic URI interface, it was mainly a matter of repeatedly testing the `$ref` and `$id` cases until I got tests
passing (except for a few).

### A slight detour: Json Pointer Fragments

For refering to definitions within files, say `#/definitions/A/`, any JSON "pointer fragment" can be used.
[RFC 6901](https://www.rfc-editor.org/rfc/rfc6901) describes the format for JSON pointer fragments trailing on a URI
anchors.

## What doesn't work?

Several Bowtie tests require you to read referenced JSON schemas on the fly from a URL. Lean does not have a good URL library,
so I just decided to give up on that part.

# Step 2: Json Schema Code Gen

Now that I parse and understand all the features of JSON schema, I decided to try generating Lean structs from JSON schema.
My strategy plodded along trying to parse in the `Except String Std.Format` monad: first simple
abbreviations, then `structure`s, etc. I considered macros, but inserting comments eluded me. I settled on
[`Std.Format`s](https://lean-lang.org/doc/api/Init/Data/Format/Basic.html#Std.Format), Lean's
way to handle indentation, precedence, etc. The API was far nicer than I deserved, even though it is string
manipulation.

Eventually, I needed more code generation context, recursion, and custom toJson/fromJson instances. I settled
on writing all the parsers into a `SchemaGen TypeDefinition` monad where `SchemaGen` had context and error handling
while `TypeDefinition` held all the parsed definitions. Using a custom monad name here is beyond useful, and I
understand now why so many libraries walk the life of a custom named monad transformer.

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

Lean requires mutually recursive types to be put in `mutual` blocks, so
[Tarjan's strongly connected components](https://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm)
lets me detect the cyclic dependencies in the schema reference graph.
The implementation can essentially be automated as there exist examples of it in [Mathlib](https://github.com/leanprover-community/mathlib4/blob/560872a203ef726bf76117856ece2872f8cff918/Mathlib/Tactic/Order/Graph/Tarjan.lean#L17-L29)
and [Lean.SCC](https://github.com/leanprover/lean4/blob/9d4ad1273f6cea397c3066c2c83062a4410d16bf/src/Lean/Util/SCC.lean#L25-L28).
So Claude could do it, taking away some of my fun ☹️.

Aside from that, there are so many different special cases, abbreviations, inductives, etc. The tension
between inductives and abbreviations was quite annoying.

# Step 3: Code Gen with Vega-Lite

How did code gen work with Vega-Lite?

* Mutual induction is still broken, oops?
* It would take several minutes to compile, and it's >15,000 lines.
* All the code is too ugly to use.

Once I saw how I'd have to construct a `RectConfig`, I realized the library would
not be fun to use as a whole.

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
  ... -- It goes on like this for all the alphabetically sorted fields.
```

Now instead of `{align: "left", ...}`, we have to use `.some (.inl .left)`. Specifying `.inl` or `.inr` and finding the
right enum name can be really annoying. The long list of fields makes it difficult to remember which one if any is required.
There are definitely ways to simplify this, but doing it automatically might be a bit much.

# Back to the drawing board

You know what would be really convenient? If you could determine if the JSON schema validation passed during compile time.
If the entire JSON is known at compile time, we can prove it using `by native_decide` to
run the computation `validate schema json` and verify it equals `true`. What if a theorem about your JSON's structure proved `validate schema json` would pass? That
sounds great, but proving the most useful properties requires `validate` _terminates_--not an easy proof at all.
If we use a more restrictive language, we could more easily prove validation _ahead_ of time.

I put this idea away for a week or so. After using [TypeScript](https://www.typescriptlang.org/) a bit professionally, I figured
that we could crib TypeScript's homework and use a fragment of it to check JSON. One day
I spent a few hours writing up a [blueprint](https://github.com/josephmckinsey/json-subtyping/blob/main/blueprint/plan.pdf)
in [Typst](https://typst.app/) (an up-and-coming LaTeX alternative). This was also more fun than improving JSON Schema validation.

## TypeScript

JavaScript has 6 types: `null, string, number, boolean, object, undefined`. Many "types" in JavaScript are
really just special objects like "functions". TypeScript lets you gradually define types to assign properties of
variables at locations, and then it will check properties "x has property 'id'" at compile-time by inspecting
the control flow graph connecting locations. TypeScript does not try much to be sound.

## Blueprint

I've never taken any course on type theory, so I probably did not use proof tree rules correctly, but
I wrote a bunch of them in the blueprint anyways. I found it helpful especially for subtyping objects
correctly.

::::blob blueprintImage
::::

For objects, JSON objects are always allowed to have extra fields, which becomes a lot more annoying when you are
checking whether fields exist in big type unions. More on that later.

## Strange challenges

Lean has a built-in JSON type at [`Lean.Json`](https://leanprover-community.github.io/mathlib4_docs/Lean/Data/Json/Basic.html#Lean.Json),
which is used for the language server among other applications. Since the JSON type is recursive, and the recursion is contained
in types like Array and maps, Lean identifies it as a ["nested inductive type"](https://lean-lang.org/doc/reference/latest/The-Type-System/Inductive-Types/#nested-inductive-types).

Nested inductives are massaged much more to make them appear normal. They are reorganized
as an ordinary inductive, and then new definitions are uesd to create the appearance of
the original nested inductive. The default induction rules are a real pain to use,
since they include the induction rules for Arrays and maps. Worse, you cannot include
dependent terms in inductive data types. The kernel will error with `nested inductive datatypes parameters
cannot contain local variables`, which remains mildly mysterious to me.

### `Std.TreeMap.Raw`

The local variables restriction prevents the use of [`Std.TreeMap`](https://leanprover-community.github.io/mathlib4_docs/Std/Data/TreeMap/Basic.html#Std.TreeMap),
a balanced binary search tree, in nested structures like JSON. `Std.TreeMap` contains
a proof that the tree is properly constructed. Without that "well-formed" predicate,
operations like insertion and lookup have fewer guarantees, but its inclusion does
not play well with the nested inductive. So instead we have to use [`Std.TreeMap.Raw`](https://leanprover-community.github.io/mathlib4_docs/Std/Data/TreeMap/Raw/Basic.html#Std.TreeMap.Raw),
which has no proof the data is "well-formed". The usual solution is to add the well-formedness _again_
in the inductive, but Json lacks this.

I carefully tried to work around potentially ill-formed trees by asserting properties about
[`.get?`](https://leanprover-community.github.io/mathlib4_docs/Std/Data/TreeMap/Raw/Basic.html#Std.TreeMap.Raw.get?)
calls or by constructing maps using [`ofList`](https://leanprover-community.github.io/mathlib4_docs/Std/Data/TreeMap/Raw/Basic.html#Std.TreeMap.Raw.ofList),
whose output is always well-formed.

### Proving anything about JSON

Given that `Json` sheds so many invariants, it is somewhat surprising how elegant it is to prove JSON properties.
As long as you can prove termination of functions on JSON, you can write your own custom induction rules
by proving induction terminates.

I spent a day reading [Lean's reference manual on "well-founded recursion"](https://lean-lang.org/doc/reference/4.26.0-rc2/Definitions/Recursive-Definitions/#well-founded-recursion)
and tried simplifying termination goals on a `Json.beq` implementation. As opposed
to trying to use induction, we write a recursive function where we only call
on "smaller" values somehow. You can use any ["well-founded" relation](https://en.wikipedia.org/wiki/Well-founded_relation) you want, which
guarantees that eventually an execution will reduce the data to nothing and no more
recursive calls are possible.

for instance, when we iterate `.obj map : Lean.Json` and recursively call on the values,
we want to know that `(k, v) ∈ map` implies the size of the size of `v` is smaller than map, which is smaller than `.obj map`.
Lean provides a `sizeOf` definition automatically for most types where this property is practically guaranteed.
During automatic termination checking, Lean attaches the membership proofs to elements of lists with
[`List.attach (l : List α) : List { x // x ∈ l }`](https://leanprover-community.github.io/mathlib4_docs/Init/Data/List/Attach.html#List.attach).

We can then apply [`List.sizeOf_lt_of_mem {a : α} {as : List α} (h : a ∈ as) : sizeOf a < sizeOf as`](https://leanprover-community.github.io/mathlib4_docs/Init/Data/List/BasicAux.html#List.sizeOf_lt_of_mem).

Although this "attach" framework works well (and automatically) for lists and arrays, the types don't work as nicely for `Std.TreeMap`.
After combing through `Std.TreeMap`'s source code with Claude Code, I learned about [`Std.DTreeMap.Internal.Impl.toListModel`](https://leanprover-community.github.io/mathlib4_docs/Std/Data/DTreeMap/Internal/Def.html#Std.DTreeMap.Internal.Impl.toListModel).
This function inefficiently flattens into a list, but the reasoning is far simpler. It's also proved equivalent
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
to prove that the size of recursive arguments decreases. Compared to the verbosity of
ordinary induction here, this is much briefer. Now we can implement better JSON induction methods:

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

Custom recursors like this let you split up recursive definitions really easily as long as you are able
to take and apply hypotheses like `j ∈ a`. As long as you carry around membership information, you can
split up recursive definitions while still proving termination.

### List equality was a pain

Lawful equality already exists for `List α`, but it expects an existing instance of equality for `α`.
For a particular subtyping proof, I needed a `ReflBEq Json` (`∀x : Json, x == x`), I hoped to use `ReflBEq (List α)`.
At the time, I ran out of patience and cleverness and could not figure out the recursion for `ReflBEq (List α)`,
so I reimplemented the lemma instead. Relatedly, writing inline definitions
in a typeclass instance makes it strangely difficult to unfold those definitions.

## Subtyping

After handling all those JSON difficulties, most of my other problems arose from logical errors in my blueprint.
From the beginning, I wanted to bundle the "schema" `t : JsonType`,
the propositional check, and the JSON into an object `TypedJson t`. Then when you wanted to check if a `json` also satisfied `t'`,
you could run a decision procedure `t.subtype t'` to prove `∀x, t.check x → t'.check x`. This would allow you to use
JSON in more general contexts without having to care about the exact types involved.

I applied a bit of cleverness by bundling the proof into the return type of the subtype checker:

```
inductive DecideSubtype (t1 t2 : JsonType) where
  | none : DecideSubtype t1 t2
  | isSubtype : (∀j, t1.check j = true → t2.check j = true) → DecideSubtype t1 t2
```

This allowed me to reuse the convoluted logic of the subtype checking for the proof plumbing. Proving that subtyping
worked separately involved so much more casework; it was so much easier to piggyback off the existing casework. Truly, dependent
types are wonderful!

There was a lot of rewriting and sublemmas for all the different `JsonType` constructors, especially for object subtyping,
but I was mainly correcting the plan, ensuring that required and optional types behaved correctly, and trying a bunch
of examples. I won't go any further into the proofs for the strategies for object subtyping,
since it granted me no more insight.

## Object Construction

Given some `TypedJson`, say `x1 x2 : TypedJson t`, I would like to be able to construct typed objects during compile time. This
might actually be the ideal use case for heterogeneous lists, a dependently typed list where each
element can have its own type.

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

Then I could guarantee objects would pass validation once you ensure no duplicates.

```
/-- Construct an object from ObjectFields -/
def mkObj {req : List (String × JsonType)} (fields : ObjectFields req)
    (noDups : req.Pairwise (fun a b => ¬compare a.1 b.1 = .eq) := by native_decide) :
    TypedJson (.object req []) := sorry
```

I had Claude Code make a macro that would construct ObjectFields using `obj{}` notation, since I could just point
it at the existing macro for list syntax `[a, b, c]`.

## Narrowing

When you are within if statements, `TypeScript` is capable of ["narrowing"](https://www.typescriptlang.org/docs/handbook/2/narrowing.html#equality-narrowing)
unions to a variant:

```
type Person =
    | { age: number, id: number }
    | { dob: Date }

const person = getPerson();

if ("age" in person) {
    // person's type is "narrowed" to {age : number, id: number}
    console.log("person.age =", person.age);
    console.log("person.id =", person.id);
}
```

This is really helpful, but it is also more fraught with difficulty than it appears. Since objects may have
_extra_ fields in JSON, it is possible to have `person = {dob: ..., age: 10}`. Now TypeScript mistakenly
believes that `person.id` is defined. When I learned about this, I decided that I should stop looking at the TypeScript
source code, and also I am going to check field properties as my proof of concept.

The blog post
["Flow Nodes: How Type Inference Is Implemented"](https://effectivetypescript.com/2024/03/24/flownodes/)
really helped me here too. For Lean, we have to start with filters for whether properties can match or not:

```
def JsonType.canMatchPropertyStr (t : JsonType) (key : String) (str : String) : Bool :=
  t.canBeObject &&
  match t.getKey? key with
  | .some kt => kt.check str
  | .none => true -- we have no information about key
```

If `x` has property `key` with value `str`, then I prove that `t.check x = true` implies `t.canMatchPropertyStr key str = true`.
Similarly, if `x` does *not* have property `key` with value `str`, then I can prove `t.canMismatchPropertyStr key str = true`.

If `t = t1 ||| t2 ||| t3 ||| t4`, then I can turn that into a list `[t1, t2, t3, t4]` for which at least one must check,
now if `t.check x = true`, then I know some `ti.check x = true`, so `ti.canMatchPropertyStr key str`. The upshot
is that I can filter `[t1, t2, t3, t4]` for `canMatchPropertyStr`, and one of them will still match `x`.

```
theorem JsonType.filterUnion_correctness
    {t : JsonType} {f : JsonType → Bool} {x : Json}
    (h : t.check x = true)
    (h' : ∀ t', t'.check x = true → f t' = true) :
    (t.filterUnion f).check x = true := by sorry
```

## A small aside: subtyping is a cool idea!

Subtyping is far more common out in the "real-world". In the programming language Julia, subtyping is used for multiple dispatch,
for compiler monomorphization, correctness, units, etc. For plotting and array manipulation, it works well.
Even object inheritance (a form of subtyping in my opinion) has its place. Using gradual subtyping is a natural fit for extending code while maintaining compatibility.
I'm especially optimistic on the potential for domain-specific typing for dataframes. Something to look into.

# {label plot}[Back to plotting]

I still want to plot something, so how will I do that? I'm not so gung-ho about translating JSON Schema into my JsonType,
so for now, I'll just do runtime checking with the JSON Schema. I don't _really_ need to have it working ahead of time.

In this case, the approach is relatively simple: we load in the Vega-Lite schema with `include_str`, then we can parse
it and use it to validate.

```
import JsonSchema.Validation
```

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
with JSON syntax easily. Creating custom string syntax for JavaScript in general might be easier
for using JavaScript libraries in widgets.

## Other potential improvements

String formatting can't be combined with raw strings like `r##""##`, and the default formatting uses `{}`, a common string in
JavaScript and JSON. Mustache templating with multi-line strings would make it fairly
elegant to include [`d3.js`](https://d3js.org/what-is-d3) code inline in Lean.
We might be able to call out to TypeScript and even have nice [VS Code support for the embedded language](https://code.visualstudio.com/api/language-extensions/embedded-languages).
