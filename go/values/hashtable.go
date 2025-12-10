package values

import (
	"fmt"
)

var (
	_ Value = (*Hashtable)(nil)
)

type Hashtable struct {
	Value map[string]Value
}

func NewHashtable(v map[string]Value) *Hashtable {
	q := &Hashtable{
		Value: v,
	}
	return q
}

func (p *Hashtable) Datum() map[string]Value {
	return p.Value
}

func (p *Hashtable) IsVoid() bool {
	return p == nil
}

func (p *Hashtable) EqualTo(o Value) bool {
	v, ok := o.(*Hashtable)
	if !ok {
		return false
	}
	if len(p.Value) != len(v.Value) {
		return false
	}
	for k := range p.Value {
		_, ok = v.Value[k]
		if !ok {
			return false
		}
	}
	for k := range v.Value {
		_, ok = p.Value[k]
		if !ok {
			return false
		}
	}
	for k := range p.Value {
		if !p.Value[k].EqualTo(v.Value[k]) {
			return false
		}
	}
	return true
}

func (p *Hashtable) SchemeString() string {
	return fmt.Sprintf("%v", p.Value)
}
