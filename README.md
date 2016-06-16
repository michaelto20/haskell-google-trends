GOOGLE TRENDS
-------------

Very simple library for accessing Google Trends

INSTALL
-------

`cabal install haskell-google-trends`

HOW TO USE
----------

Currently library exports only one function.

### queryTrendsWithLogin :: String -> String -> String -> IO (Maybe [(Integer, String, Integer)])
`Returns maybe a list of tuples of form (Year, Month, Value)`

**Example:**
```haskell
import Google.Trends

main = do
    Just results <- queryTrendsWithLogin "your-login@gmail.com" "password" "pizza"
    print [(month, value) | (year, month, value) <- results, year == 2010]
```
