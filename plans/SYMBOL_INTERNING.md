  1. Parser - when tokenizing symbol literals from source code
  2. string->symbol - when user code creates symbols at runtime

  These are the only two paths where strings become symbols. Everything else (macro expansion, internal operations) should work with already-interned symbols.

  External Text
       │
       ▼
  ┌─────────────┐     ┌──────────────────┐
  │   Parser    │     │  string->symbol  │
  └──────┬──────┘     └────────┬─────────┘
         │                     │
         └──────────┬──────────┘
                    ▼
           TopLevelEnvironment.InternSymbol()
                    │
                    ▼
           Interned *Symbol (used everywhere else)

  Both gates would call TopLevelEnvironment.InternSymbol(), which owns the intern table for that instance.

  Internal code that needs symbols would use:
  - topLevel.InternSymbolByName("foo") - for known names
  - Already-interned symbols passed through the system

  This way:
  - Interning is centralized in TopLevelEnvironment
  - The gates are explicit and minimal
  - Performance analysis later could swap interning for string comparison at a single point
  - Each Scheme instance remains isolated

