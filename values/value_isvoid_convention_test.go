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

package values

import (
	"go/ast"
	"go/parser"
	"go/token"
	"path/filepath"
	"reflect"
	"sort"
	"strings"
	"testing"
)

// allValueExemplars is the canonical roster of every concrete Value
// implementer in this package. The entries are:
//
//   - Typed nil pointers for pointer-receiver types — the documented
//     "nil receiver → IsVoid() returns true" convention is the
//     interesting behavior to test.
//   - Zero-value instances for the four value-receiver singletons.
//
// Maintenance: when adding a new Value implementer, add one entry
// here. TestAllValueExemplarsCompleteness walks values/ source via
// go/parser and fails if any IsVoid method is not represented.
var allValueExemplars = []Value{
	// Pointer-receiver types — default convention: nil receiver → true.
	(*AtomicBox)(nil),
	(*AtomicInt64)(nil),
	(*BigComplex)(nil),
	(*BigFloat)(nil),
	(*BigInteger)(nil),
	(*Boolean)(nil),
	(*Box)(nil),
	(*Byte)(nil),
	(*ByteVector)(nil),
	(*Channel)(nil),
	(*CharSet)(nil),
	(*Character)(nil),
	(*CompileTimeValue)(nil),
	(*Complex)(nil),
	(*ConditionVariable)(nil),
	(*Float)(nil),
	(*Hashtable)(nil),
	(*Integer)(nil),
	(*Mutex)(nil),
	(*NativeError)(nil),
	(*Once)(nil),
	(*OpaqueValue)(nil),
	(*Pair)(nil),
	(*PortObject)(nil),
	(*Process)(nil),
	(*Promise)(nil),
	(*RWMutex)(nil),
	(*Rational)(nil),
	(*Record)(nil),
	(*RecordType)(nil),
	(*SourceContext)(nil),
	(*String)(nil),
	(*Symbol)(nil),
	(*SyntaxVector)(nil),
	(*Thread)(nil),
	(*Time)(nil),
	(*Vector)(nil),
	(*WaitGroup)(nil),
	// Value-receiver singletons — documented exceptions.
	voidType{},
	eofType{},
	emptyListType{},
	SourceIndexes{},
}

// isVoidExceptions documents the Value types whose IsVoid() returns
// something other than the default "nil receiver → true" convention.
// The map value is the expected IsVoid() result for the exemplar.
var isVoidExceptions = map[string]bool{
	"voidType":      true,  // singleton: always void
	"eofType":       false, // singleton: never void
	"emptyListType": false, // singleton: never void
	"SourceIndexes": false, // value receiver: nil-pointer case N/A
}

// TestIsVoidConvention verifies that every Value implementer in the
// allValueExemplars roster returns the documented IsVoid() value when
// invoked on a typed nil pointer (or zero value, for the four
// singletons). The convention is documented at values/utils.go (IsVoid
// free function) and on the Value interface in values.go.
func TestIsVoidConvention(t *testing.T) {
	for _, exemplar := range allValueExemplars {
		typeName := exemplarTypeName(exemplar)
		want, isException := isVoidExceptions[typeName]
		if !isException {
			want = true
		}
		got := exemplar.IsVoid()
		if got != want {
			t.Errorf("%s.IsVoid() = %v, want %v (exception=%v)",
				typeName, got, want, isException)
		}
	}
}

// TestAllValueExemplarsCompleteness scans values/ source via go/parser,
// finds every type that defines an IsVoid() bool method, and verifies
// each is represented in allValueExemplars. This catches the case
// where a new Value implementer is added but the roster is not
// updated.
func TestAllValueExemplarsCompleteness(t *testing.T) {
	inRoster := make(map[string]bool, len(allValueExemplars))
	for _, exemplar := range allValueExemplars {
		inRoster[exemplarTypeName(exemplar)] = true
	}

	fset := token.NewFileSet()
	matches, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatalf("glob *.go: %v", err)
	}
	typesWithIsVoid := make(map[string]bool)
	for _, file := range matches {
		if strings.HasSuffix(file, "_test.go") {
			continue
		}
		astFile, err := parser.ParseFile(fset, file, nil, parser.SkipObjectResolution)
		if err != nil {
			t.Fatalf("parse %s: %v", file, err)
		}
		for _, decl := range astFile.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Recv == nil || fn.Name.Name != "IsVoid" {
				continue
			}
			recv := fn.Recv.List[0].Type
			if star, ok := recv.(*ast.StarExpr); ok {
				recv = star.X
			}
			ident, ok := recv.(*ast.Ident)
			if !ok {
				continue
			}
			typesWithIsVoid[ident.Name] = true
		}
	}

	var missing []string
	for name := range typesWithIsVoid {
		if !inRoster[name] {
			missing = append(missing, name)
		}
	}
	if len(missing) > 0 {
		sort.Strings(missing)
		t.Errorf("types implement IsVoid() but missing from allValueExemplars: %v\n"+
			"add an entry to value_isvoid_convention_test.go", missing)
	}

	var extra []string
	for name := range inRoster {
		if !typesWithIsVoid[name] {
			extra = append(extra, name)
		}
	}
	if len(extra) > 0 {
		sort.Strings(extra)
		t.Errorf("allValueExemplars has entries for types without IsVoid(): %v\n"+
			"remove stale entries or implement IsVoid()", extra)
	}
}

// exemplarTypeName extracts the Go type name from a Value exemplar,
// stripping the pointer prefix for pointer-receiver types so the name
// matches the AST-level type declaration.
func exemplarTypeName(v Value) string {
	rt := reflect.TypeOf(v)
	if rt.Kind() == reflect.Ptr {
		return rt.Elem().Name()
	}
	return rt.Name()
}
