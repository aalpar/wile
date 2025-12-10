package values

import (
	"math/big"
)

var (
	_ Value  = (*Rational)(nil)
	_ Number = (*Rational)(nil)
)

type Rational struct {
	value *big.Rat
}

// NewRational creates a new Rational from numerator and denominator.
// The fraction is automatically normalized (reduced to lowest terms).
func NewRational(num, denom int64) *Rational {
	return &Rational{
		value: big.NewRat(num, denom),
	}
}

// NewRationalFromBigInt creates a new Rational from big.Int numerator and denominator.
func NewRationalFromBigInt(num, denom *big.Int) *Rational {
	r := new(big.Rat)
	r.SetFrac(num, denom)
	return &Rational{value: r}
}

// NewRationalFromRat creates a Rational from an existing big.Rat.
func NewRationalFromRat(r *big.Rat) *Rational {
	return &Rational{value: new(big.Rat).Set(r)}
}

// Rat returns the underlying big.Rat value.
func (p *Rational) Rat() *big.Rat {
	return p.value
}

// Num returns the numerator as a big.Int.
func (p *Rational) Num() *big.Int {
	return p.value.Num()
}

// Denom returns the denominator as a big.Int.
func (p *Rational) Denom() *big.Int {
	return p.value.Denom()
}

// NumInt64 returns the numerator as int64 (may overflow for large values).
func (p *Rational) NumInt64() int64 {
	return p.value.Num().Int64()
}

// DenomInt64 returns the denominator as int64 (may overflow for large values).
func (p *Rational) DenomInt64() int64 {
	return p.value.Denom().Int64()
}

// Float64 returns the rational as a float64 approximation.
func (p *Rational) Float64() float64 {
	f, _ := p.value.Float64()
	return f
}

// IsInteger returns true if the rational represents an integer (denominator is 1).
func (p *Rational) IsInteger() bool {
	return p.value.IsInt()
}

func (p *Rational) Add(o Number) Number {
	if o.IsZero() {
		return p
	}
	if p.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *Rational:
		result := new(big.Rat).Add(p.value, v.value)
		return &Rational{value: result}
	case *Integer:
		other := big.NewRat(v.Value, 1)
		result := new(big.Rat).Add(p.value, other)
		return &Rational{value: result}
	case *Float:
		return NewFloat(p.Float64() + v.Value)
	case *Complex:
		return NewComplex(complex(p.Float64(), 0) + v.Value)
	}
	panic(ErrNotANumber)
}

func (p *Rational) Subtract(o Number) Number {
	if o.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *Rational:
		result := new(big.Rat).Sub(p.value, v.value)
		return &Rational{value: result}
	case *Integer:
		other := big.NewRat(v.Value, 1)
		result := new(big.Rat).Sub(p.value, other)
		return &Rational{value: result}
	case *Float:
		return NewFloat(p.Float64() - v.Value)
	case *Complex:
		return NewComplex(complex(p.Float64(), 0) - v.Value)
	}
	panic(ErrNotANumber)
}

func (p *Rational) Multiply(o Number) Number {
	if o.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *Rational:
		result := new(big.Rat).Mul(p.value, v.value)
		return &Rational{value: result}
	case *Integer:
		other := big.NewRat(v.Value, 1)
		result := new(big.Rat).Mul(p.value, other)
		return &Rational{value: result}
	case *Float:
		return NewFloat(p.Float64() * v.Value)
	case *Complex:
		return NewComplex(complex(p.Float64(), 0) * v.Value)
	}
	panic(ErrNotANumber)
}

func (p *Rational) Divide(o Number) Number {
	if o.IsZero() {
		panic(ErrDivisionByZero)
	}
	switch v := o.(type) {
	case *Rational:
		result := new(big.Rat).Quo(p.value, v.value)
		return &Rational{value: result}
	case *Integer:
		other := big.NewRat(v.Value, 1)
		result := new(big.Rat).Quo(p.value, other)
		return &Rational{value: result}
	case *Float:
		return NewFloat(p.Float64() / v.Value)
	case *Complex:
		return NewComplex(complex(p.Float64(), 0) / v.Value)
	}
	panic(ErrNotANumber)
}

func (p *Rational) IsZero() bool {
	return p.value.Sign() == 0
}

func (p *Rational) LessThan(o Number) bool {
	switch v := o.(type) {
	case *Rational:
		return p.value.Cmp(v.value) < 0
	case *Integer:
		other := big.NewRat(v.Value, 1)
		return p.value.Cmp(other) < 0
	case *Float:
		return p.Float64() < v.Value
	case *Complex:
		return p.Float64() < real(v.Value)
	}
	panic(ErrNotANumber)
}

func (p *Rational) IsVoid() bool {
	return p == nil
}

func (p *Rational) EqualTo(v Value) bool {
	if other, ok := v.(*Rational); ok {
		return p.value.Cmp(other.value) == 0
	}
	return false
}

func (p *Rational) SchemeString() string {
	return p.value.RatString()
}