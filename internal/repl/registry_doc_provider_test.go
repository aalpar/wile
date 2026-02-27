package repl

import (
	"testing"

	"github.com/aalpar/wile/registry"
	qt "github.com/frankban/quicktest"
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

func TestRegistryDocProvider_NotFound(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()
	provider := NewRegistryDocProvider(reg)
	_, found := provider.LookupDoc("nonexistent")
	c.Assert(found, qt.IsFalse)
}
