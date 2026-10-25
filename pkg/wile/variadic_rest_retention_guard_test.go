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

// Ratchet for the rest-list retention audit (crosscheck item A15, task X39).
//
// buildRestArg (pkg/machine/machine_context_apply.go) hands every variadic
// ForeignClosure call the same per-MachineContext PairBlock, so a primitive that
// keeps its rest list past the call, or across a nested foreign call on the same
// context, sees the cells re-linked by the next variadic call
// (TestBuildRestArgAliasesTheSharedBlockPin). Only the cells alias; an element read
// with Car is the caller's own value and is safe.
//
// Every Impl listed below was audited on 2026-10-24 with an interprocedural SSA
// taint pass (golang.org/x/tools/go/ssa, closures included). The pass seeds
// Arg(ParamCount-1), follows Cdr, conversions, phis, and stores into locals and
// aggregates, and stops at Car. It reports a reach into SetValue/SetValues/
// ApplyCallable operands, returns, non-local stores, map stores, sends and closure
// captures. It found no retention. Three planted retentions were caught: a
// SetValue of the rest Tuple in PrimList, a Cdr returned through a helper inside a
// factory-built closure, and a package-level store in PrimAdd. The procedure-invoking
// Impls were also read by hand for the nested-dispatch case. All but two copy what
// they need (Car reads, CollectList, ForEachList into a slice) before they dispatch.
// environment and make-namespace call ImportSpecInto while still walking their rest
// list, and Pair.ForEach reads the next cdr after the callback returns, so a relink
// there would corrupt the walk. It cannot happen today: ImportSpecInto receives only
// mc.Context() (whose values carry the load chain, not a MachineContext) and runs library
// code on fresh MachineContexts (vmMacroEvaluator.EvalTemplate), never on mc.
//
// What this test pins is the audited SET, not the property. A new variadic
// primitive, or a spec whose Impl moves to another function, fails here until it is
// read for retention and added. Not seen: an edit inside a listed Impl; an edit to a
// callee that a listed Impl walks its rest list across (ImportSpecInto, LoadLibrary:
// were either to run library code on the caller's MachineContext, environment and
// make-namespace would corrupt their walk); a variadic spec registered only outside
// the KitchenSink engine (the audit was module-wide, so none exists today); a second
// variadic NewForeignClosure inside a function or var initializer already in
// reviewedVariadicClosureSites.
//
// Why a Go test and not a .goast-beliefs belief. A sound belief is not expressible
// with wile-goast's primitives today:
//
//   - go-ssa-build emits no anonymous functions (pkg/registry/core: 216 functions,
//     none of them a closure), and 31 of the 129 variadic specs run a closure built
//     by one of 7 factories (makeCharSetFold, makeWriteVariant, the compare and
//     hashtable constructors). The belief checker never receives those, so a
//     retention planted in one is green by construction.
//   - value-flow-reached is intraprocedural and marks every call result whose
//     operand is reached. A Car read then reaches SetValue (a false positive in
//     nearly every Impl), and a rest list handed to a helper can be cleared only by
//     a name-keyed callee allow-list.
//   - Nothing in the Makefile or the workflows runs .goast-beliefs, so a belief
//     would be an on-demand check. This test runs in make ci.
//
// The variadic ForeignClosures built outside a PrimitiveSpec go through
// buildRestArg too. TestVariadicForeignClosureSitesReviewed pins them by AST.
// RegisterFunc's variadic wrapper (ffiSpec.makeWrapper) was audited with them: it
// walks the rest list by element and converts each value.

package wile

import (
	"context"
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"maps"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

// reviewedVariadicImpls maps each variadic primitive's name to the top-level Go
// function that holds its Impl (closure suffix stripped, module prefix trimmed).
var reviewedVariadicImpls = map[string]string{
	"%char-set":                             "extensions/charsets.primCharSetCtor",
	"%char-set-intersection":                "extensions/charsets.makeCharSetFold",
	"%char-set-union":                       "extensions/charsets.makeCharSetFold",
	"%char-set-xor":                         "extensions/charsets.makeCharSetFold",
	"*":                                     "pkg/registry/core.PrimMul",
	"+":                                     "pkg/registry/core.PrimAdd",
	"-":                                     "pkg/registry/core.PrimSub",
	"/":                                     "pkg/registry/core.PrimDiv",
	"<":                                     "pkg/registry/core.PrimNumLt",
	"<=":                                    "pkg/registry/core.PrimNumLe",
	"=":                                     "pkg/registry/core.PrimNumEq",
	">":                                     "pkg/registry/core.PrimNumGt",
	">=":                                    "pkg/registry/core.PrimNumGe",
	"abort-current-continuation":            "pkg/registry/core.PrimAbortCurrentContinuation",
	"append":                                "pkg/registry/core.PrimAppend",
	"apply":                                 "pkg/registry/core.PrimApply",
	"atan":                                  "extensions/math.PrimAtan",
	"bytevector":                            "pkg/registry/core.PrimBytevector",
	"bytevector-append":                     "pkg/registry/core.PrimBytevectorAppend",
	"bytevector-copy!":                      "pkg/registry/core.PrimBytevectorCopyBang",
	"bytevector-copy":                       "pkg/registry/core.PrimBytevectorCopy",
	"call-with-immediate-continuation-mark": "pkg/registry/core.PrimCallWithImmediateContMark",
	"char-ci<=?":                            "pkg/internal/extensions/all.makeCharCiComparePrimitive",
	"char-ci<?":                             "pkg/internal/extensions/all.makeCharCiComparePrimitive",
	"char-ci=?":                             "pkg/internal/extensions/all.makeCharCiComparePrimitive",
	"char-ci>=?":                            "pkg/internal/extensions/all.makeCharCiComparePrimitive",
	"char-ci>?":                             "pkg/internal/extensions/all.makeCharCiComparePrimitive",
	"char-ready?":                           "pkg/extensions/io.PrimCharReadyQ",
	"char-set-difference":                   "extensions/charsets.makeCharSetFold",
	"char-set<=":                            "extensions/charsets.primCharSetSubset",
	"char-set=":                             "extensions/charsets.primCharSetEqual",
	"char<=?":                               "pkg/registry/core.makeCharComparePrimitive",
	"char<?":                                "pkg/registry/core.makeCharComparePrimitive",
	"char=?":                                "pkg/registry/core.makeCharComparePrimitive",
	"char>=?":                               "pkg/registry/core.makeCharComparePrimitive",
	"char>?":                                "pkg/registry/core.makeCharComparePrimitive",
	"continuation-mark-set->list":           "pkg/registry/core.PrimContinuationMarkSetToList",
	"continuation-mark-set->list*":          "pkg/registry/core.PrimContinuationMarkSetToListStar",
	"continuation-mark-set-first":           "pkg/registry/core.PrimContinuationMarkSetFirst",
	"continuation-marks":                    "pkg/registry/core.PrimContinuationMarks",
	"current-continuation-marks":            "pkg/registry/core.PrimCurrentContinuationMarks",
	"current-stack-trace":                   "pkg/registry/core.PrimCurrentStackTrace",
	"display":                               "pkg/extensions/io.makeWriteVariant",
	"emergency-exit":                        "extensions/system.PrimEmergencyExit",
	"environment":                           "extensions/eval.PrimEnvironment",
	"error":                                 "pkg/registry/core.PrimError",
	"eval":                                  "extensions/eval.PrimEval",
	"exit":                                  "extensions/system.PrimExit",
	"flush-output-port":                     "pkg/extensions/io.PrimFlushOutputPort",
	"gcd":                                   "pkg/registry/core.PrimGcd",
	"hashtable-clear!":                      "pkg/registry/core.PrimHashtableClear",
	"hashtable-copy":                        "pkg/registry/core.PrimHashtableCopy",
	"lcm":                                   "pkg/registry/core.PrimLcm",
	"list":                                  "pkg/registry/core.PrimList",
	"list->char-set":                        "extensions/charsets.primListToCharSet",
	"log":                                   "extensions/math.PrimLog",
	"make-bytevector":                       "pkg/registry/core.PrimMakeBytevector",
	"make-condition-variable":               "extensions/threads.PrimMakeConditionVariable",
	"make-continuation-prompt-tag":          "pkg/registry/core.PrimMakeContinuationPromptTag",
	"make-eq-hashtable":                     "pkg/registry/core.makeHashtableConstructor",
	"make-equal-hashtable":                  "pkg/registry/core.makeHashtableConstructor",
	"make-eqv-hashtable":                    "pkg/registry/core.makeHashtableConstructor",
	"make-hashtable":                        "pkg/registry/core.PrimMakeHashtable",
	"make-list":                             "pkg/registry/core.PrimMakeList",
	"make-mutex":                            "extensions/threads.PrimMakeMutex",
	"make-namespace":                        "pkg/internal/extensions/namespace.PrimMakeNamespace",
	"make-parameter":                        "pkg/registry/core.PrimMakeParameter",
	"make-string":                           "pkg/registry/core.PrimMakeString",
	"make-thread":                           "extensions/threads.PrimMakeThread",
	"make-vector":                           "pkg/registry/core.PrimMakeVector",
	"max":                                   "pkg/registry/core.PrimMax",
	"min":                                   "pkg/registry/core.PrimMin",
	"mutex-lock!":                           "extensions/threads.PrimMutexLock",
	"mutex-unlock!":                         "extensions/threads.PrimMutexUnlock",
	"namespace-ref":                         "pkg/internal/extensions/namespace.PrimNamespaceRef",
	"newline":                               "pkg/extensions/io.PrimNewline",
	"number->string":                        "extensions/math.PrimNumberToString",
	"peek-char":                             "pkg/extensions/io.PrimPeekChar",
	"peek-u8":                               "pkg/extensions/io.PrimPeekU8",
	"process-spawn":                         "extensions/process.PrimProcessSpawn",
	"read":                                  "pkg/extensions/io.PrimRead",
	"read-bytevector!":                      "pkg/extensions/io.PrimReadBytevectorBang",
	"read-bytevector":                       "pkg/extensions/io.PrimReadBytevector",
	"read-char":                             "pkg/extensions/io.PrimReadChar",
	"read-line":                             "pkg/extensions/io.PrimReadLine",
	"read-string":                           "pkg/extensions/io.PrimReadString",
	"read-syntax":                           "pkg/extensions/io.PrimReadSyntax",
	"read-u8":                               "pkg/extensions/io.PrimReadU8",
	"string":                                "pkg/registry/core.PrimString",
	"string->char-set":                      "extensions/charsets.primStringToCharSet",
	"string->list":                          "pkg/registry/core.PrimStringToList",
	"string->number":                        "extensions/math.PrimStringToNumber",
	"string->utf8":                          "pkg/registry/core.PrimStringToUtf8",
	"string->vector":                        "pkg/registry/core.PrimStringToVector",
	"string-append":                         "pkg/registry/core.PrimStringAppend",
	"string-ci<=?":                          "pkg/internal/extensions/all.makeStringCiComparePrimitive",
	"string-ci<?":                           "pkg/internal/extensions/all.makeStringCiComparePrimitive",
	"string-ci=?":                           "pkg/internal/extensions/all.makeStringCiComparePrimitive",
	"string-ci>=?":                          "pkg/internal/extensions/all.makeStringCiComparePrimitive",
	"string-ci>?":                           "pkg/internal/extensions/all.makeStringCiComparePrimitive",
	"string-copy!":                          "pkg/internal/extensions/all.PrimStringCopyTo",
	"string-copy":                           "pkg/registry/core.PrimStringCopy",
	"string-fill!":                          "pkg/internal/extensions/all.PrimStringFill",
	"string<=?":                             "pkg/registry/core.makeStringComparePrimitive",
	"string<?":                              "pkg/registry/core.makeStringComparePrimitive",
	"string=?":                              "pkg/registry/core.makeStringComparePrimitive",
	"string>=?":                             "pkg/registry/core.makeStringComparePrimitive",
	"string>?":                              "pkg/registry/core.makeStringComparePrimitive",
	"syntax-local-value":                    "pkg/registry/core.PrimSyntaxLocalValue",
	"syntax-local-value/immediate":          "extensions/eval.PrimSyntaxLocalValueImmediate",
	"thread-join!":                          "extensions/threads.PrimThreadJoin",
	"u8-ready?":                             "pkg/extensions/io.PrimU8ReadyQ",
	"ucs-range->char-set":                   "extensions/charsets.primUcsRangeToCharSet",
	"utf8->string":                          "pkg/registry/core.PrimUtf8ToString",
	"values":                                "pkg/registry/core.PrimValues",
	"vector":                                "pkg/registry/core.PrimVector",
	"vector->list":                          "pkg/registry/core.PrimVectorToList",
	"vector->string":                        "pkg/registry/core.PrimVectorToString",
	"vector-append":                         "pkg/registry/core.PrimVectorAppend",
	"vector-copy!":                          "pkg/registry/core.PrimVectorCopyTo",
	"vector-copy":                           "pkg/registry/core.PrimVectorCopy",
	"vector-fill!":                          "pkg/registry/core.PrimVectorFill",
	"write":                                 "pkg/extensions/io.makeWriteVariant",
	"write-bytevector":                      "pkg/extensions/io.PrimWriteBytevector",
	"write-char":                            "pkg/extensions/io.PrimWriteChar",
	"write-shared":                          "pkg/extensions/io.makeWriteVariant",
	"write-simple":                          "pkg/extensions/io.makeWriteVariant",
	"write-string":                          "pkg/extensions/io.PrimWriteString",
	"write-u8":                              "pkg/extensions/io.PrimWriteU8",
}

// reviewedVariadicClosureSites are the non-test NewForeignClosure calls whose
// variadic argument is not the literal false, as "<file>:<enclosing func or var>".
// The first three are values-forwarding finalizers whose rest list is copied by
// CollectList. registerPhasePrimitive passes a registry spec's IsVariadic through, so
// the registry half covers what it builds. Engine.RegisterPrimitive binds through
// DefineOwnGlobal, not the registry, so the registry half does not see it; its one
// variadic producer in this module is RegisterFunc, whose wrapper
// (ffiSpec.makeWrapper) was audited and walks the rest list by element.
var reviewedVariadicClosureSites = map[string]bool{
	"pkg/machine/run_body_under_timer.go:RunBodyUnderTimer": true,
	"pkg/machine/exception_raise.go:raiseToHandlers":        true,
	"pkg/registry/core/prim_exit.go:PrimCallWithExit":       true,
	"pkg/registry/apply.go:registerPhasePrimitive":          true,
	"pkg/wile/engine.go:RegisterPrimitive":                  true,
}

func TestVariadicRestRetentionReviewed(t *testing.T) {
	t.Parallel()

	eng, err := NewEngine(context.Background(), WithProfile(KitchenSink), WithLibraryPaths())
	if err != nil {
		t.Fatalf("new engine (profile=KitchenSink, no library paths): %v", err)
	}
	seen := map[string]bool{}
	for _, pr := range eng.Registry().Primitives() {
		if !pr.Spec.IsVariadic {
			continue
		}
		goFn, _, _, rErr := resolveImpl(pr.Spec.Impl)
		if rErr != nil {
			t.Errorf("resolveImpl(%q): %v", pr.Spec.Name, rErr)
			continue
		}
		pkg, fn := splitQualified(goFn)
		impl := strings.TrimPrefix(pkg, modulePath+"/") + "." + closureSuffix.ReplaceAllString(fn, "")
		seen[pr.Spec.Name] = true
		want, ok := reviewedVariadicImpls[pr.Spec.Name]
		if !ok {
			t.Errorf("variadic primitive %q (Impl %s) is not in reviewedVariadicImpls. Its rest "+
				"list (Arg(%d)) is backed by the shared restArgBuf: check that no rest cell "+
				"reaches SetValue/SetValues, a return, a store, a capture or a send, and is "+
				"not read after a nested dispatch on the same context; copy the spine "+
				"(values.List, as PrimList does) if one does. Then add the row.",
				pr.Spec.Name, impl, pr.Spec.ParamCount-1)
			continue
		}
		if want != impl {
			t.Errorf("variadic primitive %q now has Impl %s, reviewed as %s: re-read the new Impl for "+
				"rest-list retention, then update the row.", pr.Spec.Name, impl, want)
		}
	}
	for _, name := range slices.Sorted(maps.Keys(reviewedVariadicImpls)) {
		if !seen[name] {
			t.Errorf("reviewedVariadicImpls lists %q, which is no longer a registered variadic "+
				"primitive: remove the row.", name)
		}
	}
}

func TestVariadicForeignClosureSitesReviewed(t *testing.T) {
	t.Parallel()

	root := repoRoot(t)
	fset := token.NewFileSet()
	found := map[string]bool{}
	walkErr := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			name := d.Name()
			if path != root && (strings.HasPrefix(name, ".") || name == "testdata" || name == "vendor") {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		f, perr := parser.ParseFile(fset, path, nil, 0)
		if perr != nil {
			return perr
		}
		rel, rerr := filepath.Rel(root, path)
		if rerr != nil {
			return rerr
		}
		for _, site := range variadicClosureSites(f) {
			found[filepath.ToSlash(rel)+":"+site] = true
		}
		return nil
	})
	if walkErr != nil {
		t.Fatalf("walk %s: %v", root, walkErr)
	}
	for _, site := range slices.Sorted(maps.Keys(found)) {
		if !reviewedVariadicClosureSites[site] {
			t.Errorf("%s builds a ForeignClosure that may be variadic. Its rest list is backed by the "+
				"shared restArgBuf: audit the Impl for rest-list retention (see this file's header), "+
				"then add the site to reviewedVariadicClosureSites.", site)
		}
	}
	for _, site := range slices.Sorted(maps.Keys(reviewedVariadicClosureSites)) {
		if !found[site] {
			t.Errorf("reviewedVariadicClosureSites lists %s, which no longer builds a variadic "+
				"ForeignClosure: remove the row.", site)
		}
	}
}

// variadicClosureSites returns the enclosing top-level declaration (a function, or
// the first name of a var spec) of every NewForeignClosure call in f whose third
// argument is not the identifier false.
func variadicClosureSites(f *ast.File) []string {
	var q []string
	for _, d := range f.Decls {
		switch decl := d.(type) {
		case *ast.FuncDecl:
			if decl.Body != nil {
				q = append(q, variadicClosureCalls(decl.Body, decl.Name.Name)...)
			}
		case *ast.GenDecl:
			for _, sp := range decl.Specs {
				vs, ok := sp.(*ast.ValueSpec)
				if !ok || len(vs.Names) == 0 {
					continue
				}
				for _, v := range vs.Values {
					q = append(q, variadicClosureCalls(v, vs.Names[0].Name)...)
				}
			}
		}
	}
	return q
}

// variadicClosureCalls returns site once per NewForeignClosure call under n whose
// third argument is not the identifier false.
func variadicClosureCalls(n ast.Node, site string) []string {
	var q []string
	ast.Inspect(n, func(m ast.Node) bool {
		call, ok := m.(*ast.CallExpr)
		if !ok || len(call.Args) < 3 || calleeName(call.Fun) != "NewForeignClosure" {
			return true
		}
		id, ok := call.Args[2].(*ast.Ident)
		if !ok || id.Name != "false" {
			q = append(q, site)
		}
		return true
	})
	return q
}

// calleeName returns the bare name of a call's function expression: foo for foo()
// and x.foo().
func calleeName(fun ast.Expr) string {
	switch fn := fun.(type) {
	case *ast.Ident:
		return fn.Name
	case *ast.SelectorExpr:
		return fn.Sel.Name
	}
	return ""
}
