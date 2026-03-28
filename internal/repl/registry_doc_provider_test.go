package repl

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"
)

func buildTestRegistry() *registry.Registry {
	reg := registry.NewRegistry()
	reg.AddPrimitives([]registry.PrimitiveSpec{
		{
			Name:       "string-append",
			ParamCount: 1,
			IsVariadic: true,
			Doc:        "Concatenate strings.",
			Category:   "strings",
		},
		{
			Name:       "+",
			ParamCount: 1,
			IsVariadic: true,
			Doc:        "Returns the sum of its arguments.",
			Category:   "arithmetic",
		},
	}, registry.PhaseRuntime)
	return reg
}

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

func TestRegistryDocProvider_Search(t *testing.T) {
	tcs := []struct {
		name     string
		pattern  string
		expected []string
	}{
		{
			name:     "match by name substring",
			pattern:  "string-app",
			expected: []string{"string-append"},
		},
		{
			name:     "match by doc substring",
			pattern:  "concatenate",
			expected: []string{"string-append"},
		},
		{
			name:     "match by category",
			pattern:  "arithmetic",
			expected: []string{"+"},
		},
		{
			name:     "case insensitive",
			pattern:  "STRING-APP",
			expected: []string{"string-append"},
		},
		{
			name:     "no match",
			pattern:  "zzzzzzz",
			expected: []string{},
		},
	}
	provider := NewRegistryDocProvider(buildTestRegistry())
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			results := provider.Search(tc.pattern)
			names := make([]string, len(results))
			for i, r := range results {
				names[i] = r.Name
			}
			c.Assert(names, qt.DeepEquals, tc.expected)
		})
	}
}

func TestRegistryDocProvider_Categories(t *testing.T) {
	c := qt.New(t)
	provider := NewRegistryDocProvider(buildTestRegistry())
	cats := provider.Categories()
	c.Assert(cats, qt.DeepEquals, []string{"arithmetic", "strings"})
}

func TestRegistryDocProvider_Categories_ExcludesEmpty(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()
	reg.AddPrimitive(registry.PrimitiveSpec{
		Name:       "no-category",
		ParamCount: 0,
		Doc:        "Has no category.",
	}, registry.PhaseRuntime)
	provider := NewRegistryDocProvider(reg)
	cats := provider.Categories()
	c.Assert(cats, qt.HasLen, 0)
}

func TestRegistryDocProvider_ByCategory(t *testing.T) {
	tcs := []struct {
		name     string
		category string
		expected []string
	}{
		{
			name:     "existing category",
			category: "strings",
			expected: []string{"string-append"},
		},
		{
			name:     "nonexistent category",
			category: "nonexistent",
			expected: []string{},
		},
	}
	provider := NewRegistryDocProvider(buildTestRegistry())
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			results := provider.ByCategory(tc.category)
			names := make([]string, len(results))
			for i, r := range results {
				names[i] = r.Name
			}
			c.Assert(names, qt.DeepEquals, tc.expected)
		})
	}
}
