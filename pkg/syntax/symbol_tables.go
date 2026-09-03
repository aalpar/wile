package syntax

// LiteralSymbols maps each pattern literal of a syntax-rules or syntax-case
// form, by name, to the symbol that named it in the literals list. The value
// carries the literal's definition-site scopes: the pattern compiler compares
// them to decide whether a same-named pattern identifier is the literal or a
// pattern variable (bound-identifier=?), and the matcher compares them against
// the input identifier's scopes (free-identifier=?, R7RS §4.3.2).
//
// This is a name table, not a binding table. Two same-named identifiers are
// told apart by the scopes on the value, never by the key.
type LiteralSymbols map[string]*SyntaxSymbol

// PatternVarSymbols maps each pattern variable of a clause, by name, to the
// symbol that bound it in the pattern. The value carries the variable's scopes:
// template expansion substitutes a template symbol only when the pattern
// variable's scopes are a subset of the template symbol's, which is what keeps a
// nested macro's same-named pattern variable from capturing an outer macro's
// introduced identifier.
//
// Like LiteralSymbols, a name table with the scopes on the value.
type PatternVarSymbols map[string]*SyntaxSymbol
