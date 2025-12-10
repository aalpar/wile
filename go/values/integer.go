package values

import (
	"math/big"
	"strconv"
)

var (
	_ Value  = (*Integer)(nil)
	_ Number = (*Integer)(nil)
	//_ Comparable = (*Integer)(nil)
)

type Integer struct {
	Value int64
}

func NewInteger(v int64) *Integer {
	q := &Integer{Value: v}
	return q
}

func (p *Integer) Datum() int64 {
	return p.Value
}

func (p *Integer) Add(o Number) Number {
	if o.IsZero() {
		return p
	}
	if p.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *Integer:
		return NewInteger(p.Value + v.Value)
	case *Float:
		return NewFloat(float64(p.Value) + float64(v.Value))
	case *Rational:
		self := big.NewRat(p.Value, 1)
		result := new(big.Rat).Add(self, v.Rat())
		return &Rational{value: result}
	case *Complex:
		return NewComplex(complex(float64(p.Value), 0) + v.Value)
	}
	panic(ErrNotANumber)
}

func (p *Integer) Subtract(o Number) Number {
	if o.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *Integer:
		return NewInteger(p.Value - v.Value)
	case *Float:
		return NewFloat(float64(p.Value) - v.Value)
	case *Rational:
		self := big.NewRat(p.Value, 1)
		result := new(big.Rat).Sub(self, v.Rat())
		return &Rational{value: result}
	case *Complex:
		return NewComplex(complex(float64(p.Value), 0) - v.Value)
	}
	panic(ErrNotANumber)
}

func (p *Integer) Multiply(o Number) Number {
	if o.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *Integer:
		return NewInteger(p.Value * v.Value)
	case *Float:
		return NewFloat(float64(p.Value) * v.Value)
	case *Rational:
		self := big.NewRat(p.Value, 1)
		result := new(big.Rat).Mul(self, v.Rat())
		return &Rational{value: result}
	case *Complex:
		return NewComplex(complex(float64(p.Value), 0) * v.Value)
	}
	panic(ErrNotANumber)
}

func (p *Integer) Divide(o Number) Number {
	if o.IsZero() {
		panic(ErrDivisionByZero)
	}
	switch v := o.(type) {
	case *Integer:
		result := NewRational(p.Value, v.Value)
		if result.IsInteger() {
			return NewInteger(result.NumInt64())
		}
		return result
	case *Float:
		return NewFloat(float64(p.Value) / v.Value)
	case *Rational:
		self := big.NewRat(p.Value, 1)
		result := new(big.Rat).Quo(self, v.Rat())
		return &Rational{value: result}
	case *Complex:
		return NewComplex(complex(float64(p.Value), 0) / v.Value)
	}
	panic(ErrNotANumber)
}

func (p *Integer) IsZero() bool {
	return p.Value == 0
}

func (p *Integer) LessThan(o Number) bool {
	switch v := o.(type) {
	case *Integer:
		return p.Value < v.Value
	case *Float:
		return float64(p.Value) < v.Value
	case *Rational:
		self := big.NewRat(p.Value, 1)
		return self.Cmp(v.Rat()) < 0
	case *Complex:
		return float64(p.Value) < real(v.Value)
	}
	panic(ErrNotANumber)
}

func (p *Integer) IsVoid() bool {
	return p == nil
}

func (p *Integer) EqualTo(v Value) bool {
	other, ok := v.(*Integer)
	if ok {
		return p.Value == other.Value
	}
	return false
}

func (p *Integer) SchemeString() string {
	return strconv.FormatInt(p.Value, 10)
}
