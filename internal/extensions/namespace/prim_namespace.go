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

package namespace

import (
	"context"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/machine/compilation"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimNamespaceQ implements the (namespace?) predicate.
var PrimNamespaceQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*environment.Namespace)
	return ok
})

// PrimNamespaceName implements (namespace-name ns).
func PrimNamespaceName(mc machine.CallContext) error {
	ns, err := helpers.RequireType[*environment.Namespace](mc.Arg(0), werr.ErrNotANamespace, "namespace-name")
	if err != nil {
		return err
	}
	if ns.Name == "" {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(values.NewString(ns.Name))
	return nil
}

// PrimMakeNamespace implements (make-namespace . import-specs).
// With no arguments, returns an empty namespace (kernel only).
// With import specs, pre-loads the specified libraries.
func PrimMakeNamespace(mc machine.CallContext) error {
	argsVal := mc.Arg(0)

	callerTopLevel := mc.EnvironmentFrame().Namespace()
	// Import source = the mutable runtime (reaches the sealed base via its parent walk);
	// TopLevel() now returns the sealed base alone.
	callerEnv := callerTopLevel.Runtime()
	newNS := callerTopLevel.NewChildNamespace()
	newNS.Name = "namespace"

	// Propagate registry and authorizer from caller
	newNS.SetRegistry(callerTopLevel.Registry())
	newNS.SetAuthorizer(callerTopLevel.Authorizer())
	newEnv := newNS.Runtime()

	if values.IsEmptyList(argsVal) {
		mc.SetValue(newNS)
		return nil
	}

	args, ok := argsVal.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "make-namespace: expected list of import specs, got %T", argsVal)
	}

	err := helpers.ForEachList(mc.Context(), args, "make-namespace", func(_ context.Context, _ int, _ bool, specVal values.Value) error {
		return compilation.ImportSpecInto(mc.Context(), specVal, callerEnv, newEnv, machine.NewVMMacroEvaluator(), "make-namespace")
	})
	if err != nil {
		return err
	}

	mc.SetValue(newNS)
	return nil
}

// PrimNamespaceDerive implements (namespace-derive ns).
func PrimNamespaceDerive(mc machine.CallContext) error {
	ns, err := helpers.RequireType[*environment.Namespace](mc.Arg(0), werr.ErrNotANamespace, "namespace-derive")
	if err != nil {
		return err
	}
	mc.SetValue(ns.NewChildNamespace())
	return nil
}

// PrimNamespaceDefine implements (namespace-define! ns sym val).
func PrimNamespaceDefine(mc machine.CallContext) error {
	ns, err := helpers.RequireType[*environment.Namespace](mc.Arg(0), werr.ErrNotANamespace, "namespace-define!")
	if err != nil {
		return err
	}
	sym, err := helpers.RequireType[*values.Symbol](mc.Arg(1), werr.ErrNotASymbol, "namespace-define!")
	if err != nil {
		return err
	}
	val := mc.Arg(2)

	env := ns.Runtime()
	env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
	setErr := env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), val)
	if setErr != nil {
		return setErr
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimNamespaceRef implements (namespace-ref ns sym [default]).
// With ParamCount: 2, IsVariadic: true:
//
//	Arg(0) = ns, Arg(1) = rest list (sym [default])
func PrimNamespaceRef(mc machine.CallContext) error {
	ns, err := helpers.RequireType[*environment.Namespace](mc.Arg(0), werr.ErrNotANamespace, "namespace-ref")
	if err != nil {
		return err
	}

	// Destructure rest list: (sym [default])
	rest, ok := mc.Arg(1).(values.Tuple)
	if !ok || rest.IsEmptyList() {
		return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments, "namespace-ref: expected symbol argument")
	}
	sym, err := helpers.RequireType[*values.Symbol](rest.Car(), werr.ErrNotASymbol, "namespace-ref")
	if err != nil {
		return err
	}

	// Check for optional default and reject excess arguments
	restCdr, _ := rest.Cdr().(values.Tuple)
	var hasDefault bool
	var defaultVal values.Value
	if restCdr != nil && !restCdr.IsEmptyList() {
		hasDefault = true
		defaultVal = restCdr.Car()
		// Reject extra arguments beyond default
		extraCdr, _ := restCdr.Cdr().(values.Tuple)
		if extraCdr != nil && !extraCdr.IsEmptyList() {
			return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments, "namespace-ref: expected 2 or 3 arguments")
		}
	}

	env := ns.Runtime()
	binding := env.GetBinding(sym, nil)
	if binding == nil {
		if hasDefault {
			mc.SetValue(defaultVal)
			return nil
		}
		return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "namespace-ref: unbound symbol %s", sym.Key)
	}

	mc.SetValue(binding.Value())
	return nil
}

// PrimNamespaceBound implements (namespace-bound? ns sym).
func PrimNamespaceBound(mc machine.CallContext) error {
	ns, err := helpers.RequireType[*environment.Namespace](mc.Arg(0), werr.ErrNotANamespace, "namespace-bound?")
	if err != nil {
		return err
	}
	sym, err := helpers.RequireType[*values.Symbol](mc.Arg(1), werr.ErrNotASymbol, "namespace-bound?")
	if err != nil {
		return err
	}

	env := ns.Runtime()
	binding := env.GetBinding(sym, nil)
	mc.SetValue(values.BoolToBoolean(binding != nil))
	return nil
}

// PrimNamespaceUndefine implements (namespace-undefine! ns sym).
func PrimNamespaceUndefine(mc machine.CallContext) error {
	ns, err := helpers.RequireType[*environment.Namespace](mc.Arg(0), werr.ErrNotANamespace, "namespace-undefine!")
	if err != nil {
		return err
	}
	sym, err := helpers.RequireType[*values.Symbol](mc.Arg(1), werr.ErrNotASymbol, "namespace-undefine!")
	if err != nil {
		return err
	}

	ns.Runtime().GlobalEnvironment().DeleteBinding(sym)
	mc.SetValue(values.Void)
	return nil
}

// PrimNamespaceBoundNames implements (namespace-bound-names ns).
func PrimNamespaceBoundNames(mc machine.CallContext) error {
	ns, err := helpers.RequireType[*environment.Namespace](mc.Arg(0), werr.ErrNotANamespace, "namespace-bound-names")
	if err != nil {
		return err
	}

	mc.SetValue(ns.BoundSymbolNames())
	return nil
}

// PrimNamespaceRequire implements (namespace-require ns lib-spec).
func PrimNamespaceRequire(mc machine.CallContext) error {
	ns, err := helpers.RequireType[*environment.Namespace](mc.Arg(0), werr.ErrNotANamespace, "namespace-require")
	if err != nil {
		return err
	}
	specVal := mc.Arg(1)

	// Import source = the mutable runtime (reaches the sealed base via its parent walk);
	// TopLevel() now returns the sealed base alone.
	callerEnv := mc.EnvironmentFrame().Namespace().Runtime()
	targetEnv := ns.Runtime()

	err = compilation.ImportSpecInto(mc.Context(), specVal, callerEnv, targetEnv, machine.NewVMMacroEvaluator(), "namespace-require")
	if err != nil {
		return err
	}

	mc.SetValue(values.Void)
	return nil
}
