package repl

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"
)

func TestRegistryDocProvider_Found(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()
	reg.AddPrimitive(registry.PrimitiveSpec{
		Name:       "test-prim",
		ParamCount: 2,
		Doc:        "A test primitive.",
		ParamNames: []string{"a", "b"},
		Category:   "test",
	}, registry.PhaseRuntime)

	provider := NewRegistryDocProvider(reg)
	info, found := provider.LookupDoc("test-prim")
	c.Assert(found, qt.IsTrue)
	c.Assert(info.Doc, qt.Equals, "A test primitive.")
	c.Assert(info.ParamNames, qt.DeepEquals, []string{"a", "b"})
	c.Assert(info.Category, qt.Equals, "test")
}

func TestRegistryDocProvider_ContractFields(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()
	reg.AddPrimitive(registry.PrimitiveSpec{
		Name:       "test-contracted",
		ParamCount: 2,
		Impl: func(_ *machine.MachineContext) error {
			return nil
		},
		Doc:        "A test.",
		ParamNames: []string{"s", "k"},
		Category:   "test",
		ParamTypes: []values.ValueType{values.TypeString, values.TypeInteger},
		ReturnType: values.TypeCharacter,
	}, registry.PhaseRuntime)
	prov := NewRegistryDocProvider(reg)
	info, found := prov.LookupDoc("test-contracted")
	c.Assert(found, qt.IsTrue)
	c.Assert(info.ParamTypes, qt.HasLen, 2)
	c.Assert(info.ParamTypes[0], qt.Equals, values.TypeString)
	c.Assert(info.ParamTypes[1], qt.Equals, values.TypeInteger)
	c.Assert(info.ReturnType, qt.Equals, values.TypeCharacter)
}

func TestRegistryDocProvider_NotFound(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()
	provider := NewRegistryDocProvider(reg)
	_, found := provider.LookupDoc("nonexistent")
	c.Assert(found, qt.IsFalse)
}
