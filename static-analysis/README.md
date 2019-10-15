# Static analysis

A static analyzer written in functional Typescript using [fp-ts](https://github.com/gcanti/fp-ts). It's composed by:

1. Monadic parser
2. Denotational semantics interpreter
3. Static analyzer of numeric domain via Abstract Interpretation

## Denotational semantics

- [What is a Y combinator](https://stackoverflow.com/questions/93526/what-is-a-y-combinator)
- [Fixed point combinators in JS](https://blog.benestudio.co/fixed-point-combinators-in-javascript-c214c15ff2f6)

## TODO

- Nested loops
- While ambiguity

- Range operator
- if (geZero <= leZero) => zero
- if (greaterZero <= leZero) => bottom
- Bottom if while loop doesn't terminate
- Division by zero (from bottom value to bottom state)
