package transformer

import (
	"skeme/environment"
	"skeme/values"
)

type Transformer struct {
	Cenv     *environment.EnvironmentFrame
	Keywords map[string]struct{}
	Binding  map[string]*values.KeyValue
	template []atom
}

type atom struct {
	atoms []atom
	kind  int
}

func NewTransformer(cenv *environment.EnvironmentFrame) *Transformer {
	q := &Transformer{
		Cenv:     cenv,
		Keywords: map[string]struct{}{},
		Binding:  map[string]*values.KeyValue{},
		template: []atom{},
	}
	return q
}

type VariableMatch struct {
	First values.Value
}

type EllipsisMatch struct {
	First values.Value
}

type StructureMatch struct {
	First values.Value
}

// compilePair
func compilePair(keywords map[string]ReservedWord, bindings map[string]*values.KeyValue, stack0 []*values.Pair, pri0 values.Value, val0 *values.Pair) error {
	if val0 == values.EmptyList {
		return nil
	}
	var car values.Value
	var cdr values.Value
	var err error
	for !values.IsEmptyList(val0) {
		pri := car
		car = val0[0]
		err = compile(keywords, bindings, stack0, pri, car)
		if err != nil {
			return err
		}
		cdr = val0[1]
		val0, _ = cdr.(*values.Pair)
	}
	return nil
}

// compile
func compile(keywords map[string]ReservedWord, bindings map[string]*values.KeyValue, stack0 []*values.Pair, pri0, val0 values.Value) error {
	switch v0 := val0.(type) {
	case *values.Pair:
		stack := append(stack0, v0)
		err := compilePair(keywords, bindings, stack, nil, v0)
		if err != nil {
			return err
		}
	case *values.Symbol:
		err := compileSymbol(keywords, v0)
		if err != nil {
			return err
		}
	}
	return nil
}

type ReservedWord int

const (
	ReservedWordKeyword ReservedWord = iota
	ReservedWordVariable
)

func compileSymbol(keywords map[string]ReservedWord, val0 *values.Symbol) error {
	if val0.Key == "..." {
		// ellipsis
	}
	ty, ok := keywords[val0.Key]
	if !ok {
		return nil
	}
	if ty == ReservedWordKeyword {
		// keyword
	}
	if ty == ReservedWordVariable {
		// variable
	}
	return nil
}
