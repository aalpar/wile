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

package parser

import (
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
)

func BenchmarkParseInteger(b *testing.B) {
	env := environment.NewNamespace().Runtime()
	input := "42"
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		p := NewParser(env, true, strings.NewReader(input))
		_, _ = p.ReadSyntax(context.TODO())
		_ = p.Close()
	}
}

func BenchmarkParseList(b *testing.B) {
	env := environment.NewNamespace().Runtime()
	input := "(+ 1 2 3 4 5)"
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		p := NewParser(env, true, strings.NewReader(input))
		_, _ = p.ReadSyntax(context.TODO())
		_ = p.Close()
	}
}

func BenchmarkParseQuoted(b *testing.B) {
	env := environment.NewNamespace().Runtime()
	input := "'(a b c)"
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		p := NewParser(env, true, strings.NewReader(input))
		_, _ = p.ReadSyntax(context.TODO())
		_ = p.Close()
	}
}

func BenchmarkParseNestedList(b *testing.B) {
	env := environment.NewNamespace().Runtime()
	input := "((lambda (x) (+ x 1)) 42)"
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		p := NewParser(env, true, strings.NewReader(input))
		_, _ = p.ReadSyntax(context.TODO())
		_ = p.Close()
	}
}

func BenchmarkParseString(b *testing.B) {
	env := environment.NewNamespace().Runtime()
	input := `"hello world"`
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		p := NewParser(env, true, strings.NewReader(input))
		_, _ = p.ReadSyntax(context.TODO())
		_ = p.Close()
	}
}

func BenchmarkParseFloat(b *testing.B) {
	env := environment.NewNamespace().Runtime()
	input := "3.14159"
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		p := NewParser(env, true, strings.NewReader(input))
		_, _ = p.ReadSyntax(context.TODO())
		_ = p.Close()
	}
}
