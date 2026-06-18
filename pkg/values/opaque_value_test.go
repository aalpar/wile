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

package values_test

import (
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

func TestOpaqueValue_SchemeString(t *testing.T) {
	tcs := []struct {
		name string
		tag  string
		val  any
		want string // prefix only — ID is unpredictable
	}{
		{name: "db-conn tag", tag: "db-conn", val: "fake-db", want: "#<db-conn:"},
		{name: "session tag", tag: "session", val: 42, want: "#<session:"},
		{name: "nil value", tag: "empty", val: nil, want: "#<empty:"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			v := values.NewOpaqueValue(tc.tag, tc.val)
			s := v.SchemeString()
			qt.Assert(t, strings.HasPrefix(s, tc.want), qt.IsTrue, qt.Commentf("got %q", s))
			qt.Assert(t, strings.HasSuffix(s, ">"), qt.IsTrue)
		})
	}
}

func TestOpaqueValue_SchemeString_NilReceiver(t *testing.T) {
	var v *values.OpaqueValue
	qt.Assert(t, v.SchemeString(), qt.Equals, "#<opaque:void>")
}

func TestOpaqueValue_EqualTo(t *testing.T) {
	a := values.NewOpaqueValue("tag", "inner")
	b := values.NewOpaqueValue("tag", "inner")

	tcs := []struct {
		name string
		lhs  values.Value
		rhs  values.Value
		want bool
	}{
		{name: "same object", lhs: a, rhs: a, want: true},
		{name: "different objects same contents", lhs: a, rhs: b, want: false},
		{name: "different type", lhs: a, rhs: values.TrueValue, want: false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.lhs.EqualTo(tc.rhs), qt.Equals, tc.want)
		})
	}
}

func TestOpaqueValue_IsVoid(t *testing.T) {
	v := values.NewOpaqueValue("tag", "val")
	qt.Assert(t, v.IsVoid(), qt.IsFalse)

	var nilOpaque *values.OpaqueValue
	qt.Assert(t, nilOpaque.IsVoid(), qt.IsTrue)
}

func TestOpaqueValue_OpaqueTag(t *testing.T) {
	v := values.NewOpaqueValue("my-tag", nil)
	qt.Assert(t, v.OpaqueTag(), qt.Equals, "my-tag")
}

func TestOpaqueValue_Unwrap(t *testing.T) {
	type myDB struct{ name string }
	db := &myDB{name: "test"}
	v := values.NewOpaqueValue("db", db)

	got, ok := v.Unwrap().(*myDB)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, got.name, qt.Equals, "test")
}

func TestOpaqueValue_UniqueIDs(t *testing.T) {
	a := values.NewOpaqueValue("tag", nil)
	b := values.NewOpaqueValue("tag", nil)
	qt.Assert(t, a.SchemeString() != b.SchemeString(), qt.IsTrue,
		qt.Commentf("expected different IDs: %s vs %s", a.SchemeString(), b.SchemeString()))
}

func TestOpaqueValue_OpaqueTag_NilReceiver(t *testing.T) {
	var v *values.OpaqueValue
	qt.Assert(t, v.OpaqueTag(), qt.Equals, "")
}
