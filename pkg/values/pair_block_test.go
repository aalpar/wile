package values_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

func TestPairBlock_LinkWith(t *testing.T) {
	tests := []struct {
		name string
		vals []values.Value
		want values.Value
	}{
		{
			name: "empty",
			vals: nil,
			want: values.EmptyList,
		},
		{
			name: "single element",
			vals: []values.Value{values.NewInteger(1)},
			want: values.List(values.NewInteger(1)),
		},
		{
			name: "three elements",
			vals: []values.Value{values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)},
			want: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name: "mixed types",
			vals: []values.Value{values.NewInteger(1), values.NewString("hello"), values.TrueValue},
			want: values.List(values.NewInteger(1), values.NewString("hello"), values.TrueValue),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var block values.PairBlock
			if len(tt.vals) > 0 {
				block = make(values.PairBlock, len(tt.vals))
			}
			got := block.LinkWith(tt.vals)
			qt.Assert(t, got, valuestest.SchemeEquals, tt.want)
		})
	}
}

func TestPairBlock_LinkWith_MutationSemantics(t *testing.T) {
	// Block-allocated pairs via LinkWith must support set-car!/set-cdr!.
	c := qt.New(t)

	block := make(values.PairBlock, 3)
	lst := block.LinkWith([]values.Value{
		values.NewInteger(1), values.NewInteger(2), values.NewInteger(3),
	})

	p := lst.(*values.Pair)
	p.SetCar(values.NewInteger(99))
	c.Assert(p.Car(), valuestest.SchemeEquals, values.NewInteger(99))

	// Cdr chain intact
	second := p.Cdr().(*values.Pair)
	c.Assert(second.Car(), valuestest.SchemeEquals, values.NewInteger(2))
}

func TestPairBlock_LinkWith_BufferReuse(t *testing.T) {
	// Simulate the restArgBuf pattern: reuse a pre-allocated buffer.
	buf := make(values.PairBlock, 10)

	lst1 := buf[:3].LinkWith([]values.Value{
		values.NewInteger(1), values.NewInteger(2), values.NewInteger(3),
	})
	qt.Assert(t, lst1.SchemeString(), qt.Equals, "(1 2 3)")

	// Reuse same buffer for a different list
	lst2 := buf[:2].LinkWith([]values.Value{
		values.NewInteger(10), values.NewInteger(20),
	})
	qt.Assert(t, lst2.SchemeString(), qt.Equals, "(10 20)")
}
