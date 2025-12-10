package values

import (
	"strconv"
)

var (
	_ Value  = (*Float)(nil)
	_ Number = (*Float)(nil)
	//_ Comparable = (*Float)(nil)
)

type Float struct {
	Value float64
}

func NewFloat(v float64) *Float {
	q := &Float{Value: v}
	return q
}

func (p *Float) Datum() float64 {
	return p.Value
}

func (p *Float) Add(o Number) Number {
	if o.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *Integer:
		return NewFloat(p.Value + float64(v.Value))
	case *Float:
		return NewFloat(p.Value + v.Value)
	case *Rational:
		return NewFloat(p.Value + v.Float64())
	case *Complex:
		return NewComplex(complex(p.Value, 0) + v.Value)
	}
	panic(ErrNotANumber)
}

func (p *Float) Subtract(o Number) Number {
	if o.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *Integer:
		return NewFloat(p.Value - float64(v.Value))
	case *Float:
		return NewFloat(p.Value - v.Value)
	case *Rational:
		return NewFloat(p.Value - v.Float64())
	case *Complex:
		return NewComplex(complex(p.Value, 0) - v.Value)
	}
	panic(ErrNotANumber)
}

func (p *Float) Multiply(o Number) Number {
	if o.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *Integer:
		return NewFloat(p.Value * float64(v.Value))
	case *Float:
		return NewFloat(p.Value * v.Value)
	case *Rational:
		return NewFloat(p.Value * v.Float64())
	case *Complex:
		return NewComplex(complex(p.Value, 0) * v.Value)
	}
	panic(ErrNotANumber)
}

func (p *Float) Divide(o Number) Number {
	if o.IsZero() {
		panic(ErrDivisionByZero)
	}
	switch v := o.(type) {
	case *Integer:
		return NewFloat(p.Value / float64(v.Value))
	case *Float:
		return NewFloat(p.Value / v.Value)
	case *Rational:
		return NewFloat(p.Value / v.Float64())
	case *Complex:
		return NewComplex(complex(p.Value, 0) / v.Value)
	}
	panic(ErrNotANumber)
}

func (p *Float) IsZero() bool {
	return p.Value == 0.0
}

func (p *Float) LessThan(o Number) bool {
	switch v := o.(type) {
	case *Integer:
		return p.Value < float64(v.Value)
	case *Float:
		return p.Value < v.Value
	case *Rational:
		return p.Value < v.Float64()
	case *Complex:
		return p.Value < real(v.Value)
	}
	panic(ErrNotANumber)
}

func (p *Float) IsVoid() bool {
	return p == nil
}

func (p *Float) EqualTo(v Value) bool {
	if other, ok := v.(*Float); ok {
		return p.Value == other.Value
	}
	return false
}

func (p *Float) SchemeString() string {
	return strconv.FormatFloat(p.Value, 'f', -1, 64)
}

func (p *Float) String() string {
	return strconv.FormatFloat(p.Value, 'f', -1, 64)
}
