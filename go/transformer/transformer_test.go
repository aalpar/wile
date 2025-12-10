package transformer

import (
	"wile/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestTransformer_compile(t *testing.T) {
	c := qt.New(t)
	v0 := values.List(values.NewSymbol("one"), values.NewInteger(1))
	keywords := map[string]ReservedWord{}
	bindings := map[string]*values.KeyValue{}
	stack := []*values.Pair{}
	err := compile(keywords, bindings, stack, nil, v0)
	c.Assert(err, qt.IsNil)
}
