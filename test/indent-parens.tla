Foo = (\A c \in Clients:
       FooVals[c] = TRUE)

Bar = { \A c \in Clients:
        BarVals[c] = TRUE }

Quux = [ a |-> 1,
         b |-> 2,
         c |-> 3 ]
\* Add [], possibly nesting, ensure lines up with first non-paren character
