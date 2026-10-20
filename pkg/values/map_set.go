// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package values

import "maps"

type MapSet[T comparable] map[T]struct{}

// NewMapSet creates a new MapSet with the given initial size.
func NewMapSet[T comparable](sz int) MapSet[T] {
	return make(MapSet[T], sz)
}

// Set adds the given value to the set.
func (p MapSet[T]) Set(s T) {
	p[s] = struct{}{}
}

// SetAll adds all the given values to the set.
func (p MapSet[T]) SetAll(vs ...T) {
	for _, v := range vs {
		p.Set(v)
	}
}

// Unset removes the given value from the set.
func (p MapSet[T]) Unset(s T) {
	delete(p, s)
}

// UnsetAll removes all the given values from the set.
func (p MapSet[T]) UnsetAll(vs ...T) {
	for _, v := range vs {
		p.Unset(v)
	}
}

// ContainsOne returns true if the given value is present in the set.
func (p MapSet[T]) ContainsOne(s T) bool {
	_, q := p[s]
	return q
}

// ContainsAll returns true if all of the given values are present in the set.
func (p MapSet[T]) ContainsAll(vs ...T) bool {
	for _, v := range vs {
		if !p.ContainsOne(v) {
			return false
		}
	}
	return true
}

// Len returns the number of elements in the set.
func (p MapSet[T]) Len() int {
	return len(p)
}

// Intersection returns a new set containing elements in `set` that are also in all of the `others`.
func Intersection[T comparable](set MapSet[T], others ...MapSet[T]) MapSet[T] {
	result := NewMapSet[T](set.Len())
	for k := range set {
		keep := true
		for _, other := range others {
			if !other.ContainsOne(k) {
				keep = false
				break
			}
		}
		if keep {
			result.Set(k)
		}
	}
	return result
}

// Union returns a new set containing elements in `set` and all of the `others`.
//
// The three set operations treat a nil operand as empty and always return a
// fresh allocated set (maps.Clone would return nil for a nil set, and a nil
// result cannot be written to).
func Union[T comparable](set MapSet[T], others ...MapSet[T]) MapSet[T] {
	result := NewMapSet[T](set.Len())
	maps.Copy(result, set)
	for _, other := range others {
		maps.Copy(result, other)
	}
	return result
}

// Difference returns a new set containing elements in `set` that are not in any of the `others`.
func Difference[T comparable](set MapSet[T], others ...MapSet[T]) MapSet[T] {
	result := NewMapSet[T](set.Len())
	maps.Copy(result, set)
	for _, other := range others {
		for k := range other {
			result.Unset(k)
		}
	}
	return result
}

// StringSet and IntSet are the two instantiations common enough to have earned
// a name. They are ALIASES, not defined types, which is what makes them free:
// a defined type does not inherit MapSet's method set, so each of the four would
// have to be restated here and kept from drifting. An alias has one method set
// because it is one type.
//
// The cost, stated: aliases are interchangeable with MapSet[string] /
// MapSet[int] everywhere, so a signature naming StringSet documents intent
// without enforcing it, and a type switch cannot have arms for both. Neither is
// load-bearing anywhere in the tree — nothing switches on these, and no
// signature relies on a string set being refused where a MapSet[string] is
// expected. If that changes, the way back is `type StringSet MapSet[string]`
// plus five delegating methods, and the compiler will name every site.

type StringSet = MapSet[string]

func NewStringSet(sz int) StringSet {
	return make(StringSet, sz)
}

type IntSet = MapSet[int]

func NewIntSet(sz int) IntSet {
	return make(IntSet, sz)
}
