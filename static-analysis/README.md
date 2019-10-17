# Static analysis

A static analyzer written in functional Typescript using [fp-ts](https://github.com/gcanti/fp-ts). It's composed by:

1. Monadic parser
2. [Denotational semantics interpreter](https://en.wikipedia.org/wiki/Denotational_semantics)
3. Static analyzer of numeric domain via Abstract Interpretation, based on [Tutorial on Static Inference of Numeric Invariants by Antoine Miné](https://hal.sorbonne-universite.fr/hal-01657536/document)

## TODO

- Nested loops
- Test syntactic sugar in abstract semantics
- Check <= and > in Sign domain tests
