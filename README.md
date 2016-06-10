GOOGLE TRENDS
-------------

Very simple library for accessing Google Trends

INSTALL
-------

`cabal install haskell-google-trends`

HOW TO USE
----------

Currently library exports two functions.
Both return maybe a list of tuples of form (Year, Month, Value).

```haskell 
queryTrendsWithLogin :: String -> String -> Maybe (String, Int) -> String -> IO (Maybe [(Integer, String, Integer)])
queryTrendsWithLogin email password maybeProxy keywords

queryTrendsNoLogin :: Maybe (String, Int) -> String -> IO (Maybe [(Integer, String, Integer)])
queryTrendsNoLogin maybeProxy keywords
```

**Example (No proxy):**
```haskell
import Google.Trends

main = do
    Just results <- queryTrendsWithLogin "your-login@gmail.com" "password" Nothing "pizza"
    print [(month, value) | (year, month, value) <- results, year == 2010]
```

**Example (With proxy):**
```haskell
import Google.Trends

main = do
    Just results <- queryTrendsNoLogin (Just ("54.153.7.21", 8083)) "pizza"
    print [(month, value) | (year, month, value) <- results, year == 2010]
```
