# Web Script
Programming language to quickly bring up websites while still including shell-like capabilities.
Code is mostly stolen from https://github.com/sdiehl/kaleidoscope/tree/master/src/chapter7.

## Core
- Haskell to implement compiler/parser
- Underlying http lib should use haskell

## Implementation
- What are the core ideas for a compiler
- Units
    - Token
    - Expression
    - Statement
- Steps
    - Lexer : Text -> Token
    - Parser : Token -> AST
    - Interpreter : AST -> Eval

### MVP
- Create endpoints
- Handle endpoits using shell-like abilities
- Variables should be available in scope
    - Headers
    - Body
- Can run scripting commands to modify response
- Primarily functional
- Dynamically typed
