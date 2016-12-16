# SimpleJSONParser
Small Haskell parser for JavaScript Object Notation.
Run in ghci with:

\> run "<Json String>"

<Json String> should be regular JSON notation. Parser handles all objects, arrays, Ints nulls, bools and strings. For string inputs \" must be used.

Example input:

\> run "{\"names\":[\"Bill\", \"Ben\", {\"Number\": 12345}]}"

Output:

\> ObjectJson [("names",ArrayJson [StrJson "Bill",StrJson "Ben",ObjectJson [("Number",IntJson 12345)]])]
