// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

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

// noErrorsNew flags uses of errors.New in production code.
// Project convention: use werr.WrapForeignErrorf(sentinel, msg) or
// werr.NewForeignErrorf(msg) instead, so callers can match with errors.Is.
// Skips test files — tests legitimately create ad-hoc errors.
func noErrorsNew(m dsl.Matcher) { //nolint:unused // loaded by gocritic ruleguard checker at lint time
	m.Match(`errors.New($msg)`).
		Where(!m.File().Name.Matches(`_test\.go$`)).
		Report(`use werr.WrapForeignErrorf(sentinel, msg) instead of errors.New`)
}

// noFmtErrorf flags uses of fmt.Errorf in production code.
// Project convention: use werr.WrapForeignErrorf(sentinel, msg, args...)
// or werr.NewForeignErrorf(msg, args...) instead.
// Skips test files — tests legitimately create ad-hoc errors.
func noFmtErrorf(m dsl.Matcher) { //nolint:unused // loaded by gocritic ruleguard checker at lint time
	m.Match(`fmt.Errorf($*args)`).
		Where(!m.File().Name.Matches(`_test\.go$`)).
		Report(`use werr.WrapForeignErrorf(sentinel, msg, args...) instead of fmt.Errorf`)
}

// noBareSentinelPanic flags panic calls with bare werr sentinel errors.
// Project convention: always wrap with WrapForeignErrorf for site context.
//
//	// Wrong:
//	panic(werr.ErrNotAList)
//
//	// Right:
//	panic(werr.WrapForeignErrorf(werr.ErrNotAList, "site: what failed"))
func noBareSentinelPanic(m dsl.Matcher) { //nolint:unused // loaded by gocritic ruleguard checker at lint time
	m.Match(`panic(werr.$err)`).
		Where(m["err"].Text.Matches(`^Err[A-Z]`)).
		Report(`panic with bare sentinel: wrap with werr.WrapForeignErrorf(werr.$err, "site: context")`)
}
