import JsonBlogPost

import VersoBlog

open Verso Genre Blog Site Syntax

open Output Html Template Theme in
def theme : Theme := { Theme.default with
  primaryTemplate := do
    return {{
      <html>
        <head>
        <meta name="tags" content="math, lean, json"/> -- not really a theme...
        <meta name="authors" content="Joseph McKinsey"/>
        <meta name="category" content="article"/>
        <meta charset="UTF-8"/>
        <title>{{ (← param (α := String) "title") }}</title>
        </head>
        <body>
          {{← builtinHeader }}  -- My blog generator needs the JS deep in here
          <div class="main" role="main">
            <div class="wrap">
              {{ (← param "content") }}
            </div>
          </div>
        </body>
      </html>
    }}
  }


def jsonBlogSite : Site := Site.page `JsonPost (%doc JsonBlogPost.Post) #[]

def main := blogMain theme jsonBlogSite
