# Masala

Masala is a work in progress C preprocessor lexing/parsing/transformation library built in Haskell.

## Feature set (WIP)

Simple interface for
- scanning preprocessor tokens, which also tokenizes C++ in a decent way
- evaluating preprocessor directives
- modifying preprocessor directives programmatically

## Unsupported edge-cases

For now, the lexer does not support most edge-cases involving "\", such as:

- within comments outside of directives:

```
//\
#define A //A is not defined, in Masala it is
```

- splitting a token in two:

```
#define MY_DEFINE 1
#define A MY_\
DEFINE //A should be MY_DEFINE and hence 1, but will be tokenised as [MY_, DEFINE]

#define B 1.0\
5f //B should be 1.05f, but will be tokenised as [1.0, 5f]
```

- and probably a few more I haven't thought about!


