GOOGLE TRENDS
-------------

Very simple library for accessing Google Trends

HOW TO USE
----------

Currently library exports only one function.

## queryTrendsWithLogin :: String -> String -> String -> IO (Maybe [(Integer, String, Integer)])
Returns maybe a list of tuples of form (Year, Month, Value)

_Example:_

main = do
    Just results <- queryTrendsWithLogin "your-login@gmail.com" "password" "pizza"
    print [(month, value) | (year, month, value) <- results, year == 2010]
