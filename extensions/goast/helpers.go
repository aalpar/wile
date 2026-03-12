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

package goast

import (
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// Extension-local error sentinels.
var (
	errGoParseError       = werr.NewStaticError("go parse error")
	errMalformedGoAST     = werr.NewStaticError("malformed go ast")
	errGoPackageLoadError = werr.NewStaticError("go package load error")
)

// Tag returns a symbol value for a node tag name.
func Tag(name string) values.Value {
	return values.NewSymbol(name)
}

// Field returns a pair (key . val) for an alist entry.
func Field(key string, val values.Value) values.Value {
	return values.NewCons(values.NewSymbol(key), val)
}

// Node builds a tagged alist: (tag (field . val) ...).
// The car is the tag symbol, the cdr is the alist of fields.
func Node(tagName string, fields ...values.Value) values.Value {
	return values.NewCons(Tag(tagName), values.List(fields...))
}

// Str returns an immutable Scheme string.
func Str(s string) values.Value {
	return values.NewString(s)
}

// Sym returns a Scheme symbol.
func Sym(s string) values.Value {
	return values.NewSymbol(s)
}

// ValueList builds a proper Scheme list from a slice of values.
func ValueList(vs []values.Value) values.Value {
	return values.List(vs...)
}

// GetField looks up a key in an alist (list of pairs).
// Returns the cdr of the matching pair and true, or FalseValue and false.
func GetField(fields values.Value, key string) (values.Value, bool) {
	tuple, ok := fields.(values.Tuple)
	if !ok {
		return values.FalseValue, false
	}

	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			return values.FalseValue, false
		}
		entry, ok := pair.Car().(*values.Pair)
		if !ok {
			cdr, ok := pair.Cdr().(values.Tuple)
			if !ok {
				return values.FalseValue, false
			}
			tuple = cdr
			continue
		}
		sym, ok := entry.Car().(*values.Symbol)
		if ok && sym.Key == key {
			return entry.Cdr(), true
		}
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			return values.FalseValue, false
		}
		tuple = cdr
	}
	return values.FalseValue, false
}

// RequireField looks up a key in an alist, returning an error if missing.
func RequireField(fields values.Value, nodeType, key string) (values.Value, error) {
	val, ok := GetField(fields, key)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: %s missing required field '%s'", nodeType, key)
	}
	return val, nil
}

// RequireString extracts a Go string from a Scheme string value.
func RequireString(v values.Value, nodeType, fieldName string) (string, error) {
	s, ok := v.(*values.String)
	if !ok {
		return "", werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: %s field '%s' expected string, got %T", nodeType, fieldName, v)
	}
	return s.Value, nil
}

// RequireSymbol extracts a Go string from a Scheme symbol value.
func RequireSymbol(v values.Value, nodeType, fieldName string) (string, error) {
	s, ok := v.(*values.Symbol)
	if !ok {
		return "", werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: %s field '%s' expected symbol, got %T", nodeType, fieldName, v)
	}
	return s.Key, nil
}

// IsFalse returns true if v is #f (used for optional/nil fields).
func IsFalse(v values.Value) bool {
	b, ok := v.(*values.Boolean)
	return ok && !b.Value
}
