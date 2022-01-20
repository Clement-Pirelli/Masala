# Masala

Masala is a work in progress C preprocessor lexing/parsing/transformation library built in Haskell.

## Feature set (WIP)

Simple interface for
- scanning preprocessor tokens, which also tokenizes C++ in a decent way
- parsing and evaluating preprocessor directives
- modifying preprocessor directives programmatically

Currently, only the first one is implemented, and doesn't handle floating point/character literals properly, nor does it handle most escape sequences properly. Baby steps!
