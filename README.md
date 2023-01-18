# Masala

Masala is a work in progress C preprocessor lexing/parsing/transformation library built in Haskell.

## Feature set (WIP)

Simple interface for
- scanning and parsing preprocessor directives
- evaluating preprocessor expressions
- modifying preprocessor directives programmatically
- writing back the modified directives with style-preservation

Currently, only the first bullet point is implemented, but scanning doesn't handle floating point/character literals properly, nor does it handle most escape sequences properly. Baby steps!
