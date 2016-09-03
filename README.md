hakyll-shakespeare
====

[hackage](https://hackage.haskell.org/package/hakyll-shakespeare)

This library is for using Shakespeare template engine with Hakyll.

```haskell
import Hakyll
import Hakyll.Web.Hamlet

main :: IO ()
main = hakyll $ do
    match "templates/*.hamlet" $ compile hamlTemplateCompiler
```
