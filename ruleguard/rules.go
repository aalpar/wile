// Package gorules defines custom lint rules for the Wile project.
// These rules are loaded by gocritic's ruleguard checker at lint time.
//
// See: https://github.com/quasilyte/go-ruleguard
package gorules

import "github.com/quasilyte/go-ruleguard/dsl"

// noCompoundIf flags if-statements that use an init clause.
// Project convention: separate the init and the condition into
// distinct statements for readability.
//
//	// Wrong:
//	if err := f(); err != nil { ... }
//
//	// Right:
//	err := f()
//	if err != nil { ... }
func noCompoundIf(m dsl.Matcher) { //nolint:unused // loaded by gocritic ruleguard checker at lint time
	m.Match(`if $init; $cond { $*_ }`).
		Report(`compound if-init statement: separate "$init" from the condition`)
}
