# Notes while learning racket

## Transparrend vs. prefab structs: [link](https://stackoverflow.com/q/32098301).

## string vs. byte string vs. string port
+ A [string](https://docs.racket-lang.org/reference/strings.html#(part._strings))
  + is a fixed-length array of characters
  + can be mutable or immutable
+ A [byte string](https://docs.racket-lang.org/reference/bytestrings.html)
  + is a fixed-length array of bytes (an exact integer between 0 and 255)
  + can be mutable or immutable
+ A [string port](https://docs.racket-lang.org/reference/stringport.html#(part._stringport))
  + input string port: can be created from a byte string or from a string
    + when it is created from a string, the string is UTF-8 encoded
	+ we can think of an input port like a file that we want to open for reading -- it is our "input"
	+
  + output string port: collects output into a byte string
  + string ports do not need to be explicitly closed
