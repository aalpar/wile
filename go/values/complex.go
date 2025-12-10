package values

import (
	"fmt"
	"math/cmplx"
)

var (
	_ Value  = (*Complex)(nil)
	_ Number = (*Complex)(nil)
)

type Complex struct {
	Value complex128
}

func NewComplex(v complex128) *Complex {
	return &Complex{Value: v}
}

func NewComplexFromParts(realPart, imagPart float64) *Complex {
	return &Complex{Value: complex(realPart, imagPart)}
}

func (p *Complex) Datum() complex128 {
	return p.Value
}

func (p *Complex) Real() float64 {
	return real(p.Value)
}

func (p *Complex) Imag() float64 {
	return imag(p.Value)
}

func (p *Complex) Add(o Number) Number {
	if o.IsZero() {
		return p
	}
	if p.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *Complex:
		return NewComplex(p.Value + v.Value)
	case *Float:
		return NewComplex(p.Value + complex(v.Value, 0))
	case *Integer:
		return NewComplex(p.Value + complex(float64(v.Value), 0))
	case *Rational:
		return NewComplex(p.Value + complex(v.Float64(), 0))
	}
	panic(ErrNotANumber)
}

func (p *Complex) Subtract(o Number) Number {
	if o.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *Complex:
		return NewComplex(p.Value - v.Value)
	case *Float:
		return NewComplex(p.Value - complex(v.Value, 0))
	case *Integer:
		return NewComplex(p.Value - complex(float64(v.Value), 0))
	case *Rational:
		return NewComplex(p.Value - complex(v.Float64(), 0))
	}
	panic(ErrNotANumber)
}

func (p *Complex) Multiply(o Number) Number {
	if o.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *Complex:
		return NewComplex(p.Value * v.Value)
	case *Float:
		return NewComplex(p.Value * complex(v.Value, 0))
	case *Integer:
		return NewComplex(p.Value * complex(float64(v.Value), 0))
	case *Rational:
		return NewComplex(p.Value * complex(v.Float64(), 0))
	}
	panic(ErrNotANumber)
}

func (p *Complex) Divide(o Number) Number {
	if o.IsZero() {
		panic(ErrDivisionByZero)
	}
	switch v := o.(type) {
	case *Complex:
		return NewComplex(p.Value / v.Value)
	case *Float:
		return NewComplex(p.Value / complex(v.Value, 0))
	case *Integer:
		return NewComplex(p.Value / complex(float64(v.Value), 0))
	case *Rational:
		return NewComplex(p.Value / complex(v.Float64(), 0))
	}
	panic(ErrNotANumber)
}

func (p *Complex) IsZero() bool {
	return p.Value == 0
}

func (p *Complex) LessThan(o Number) bool {
	switch v := o.(type) {
	case *Complex:
		return real(p.Value) < real(v.Value)
	case *Float:
		return real(p.Value) < v.Value
	case *Integer:
		return real(p.Value) < float64(v.Value)
	case *Rational:
		return real(p.Value) < v.Float64()
	}
	panic(ErrNotANumber)
}

func (p *Complex) IsReal() bool {
	return imag(p.Value) == 0
}

func (p *Complex) Magnitude() float64 {
	return cmplx.Abs(p.Value)
}

func (p *Complex) Phase() float64 {
	return cmplx.Phase(p.Value)
}

func (p *Complex) IsVoid() bool {
	return p == nil
}

func (p *Complex) EqualTo(v Value) bool {
	other, ok := v.(*Complex)
	if ok {
		return p.Value == other.Value
	}
	return false
}

func (p *Complex) SchemeString() string {
	r := real(p.Value)
	i := imag(p.Value)
	if i >= 0 {
		return fmt.Sprintf("%g+%gi", r, i)
	}
	return fmt.Sprintf("%g%gi", r, i)
}