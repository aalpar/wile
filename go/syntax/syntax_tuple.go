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

import (
	"context"
	"wile/values"
)

type SyntaxForEachFunc func(ctx context.Context, i int, hasNext bool, v SyntaxValue) error

type SyntaxTuple interface {
	values.Tuple
	SyntaxValue
	SyntaxCar() SyntaxValue
	SyntaxCdr() SyntaxValue
	SetSyntaxCar(SyntaxValue)
	SetSyntaxCdr(SyntaxValue)
	AsSyntaxVector() *SyntaxVector
	SyntaxAppend(value SyntaxValue) SyntaxValue
	SyntaxForEach(ctx context.Context, fn SyntaxForEachFunc) (SyntaxValue, error)
}
