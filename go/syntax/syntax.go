// Copyright 2025 Aaron Alpar
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


package syntax

import "wile/values"

// syntaxVoidType is a sentinel value representing the absence of a syntax value.
type syntaxVoidType struct{}

func (syntaxVoidType) SchemeString() string        { return values.SpecialVoid }
func (syntaxVoidType) IsVoid() bool                { return true }
func (syntaxVoidType) EqualTo(v values.Value) bool { return v != nil && v.IsVoid() }
func (syntaxVoidType) SourceContext() *SourceContext { return nil }
func (syntaxVoidType) Unwrap() values.Value        { return values.Void }
func (syntaxVoidType) UnwrapAll() values.Value     { return values.Void }

// SyntaxVoid is the singleton syntax void value.
var SyntaxVoid SyntaxValue = syntaxVoidType{}

type SyntaxValue interface {
	values.Value
	SourceContext() *SourceContext
	Unwrap() values.Value
	UnwrapAll() values.Value
}
