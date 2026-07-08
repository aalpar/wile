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

// External representations of the IEEE-754 special inexact reals, per R7RS
// §6.2.5. These are the exact lexemes the reader accepts and the writer emits
// for the infinities and NaN. Centralized here — the lowest package that both
// the reader (parser) and writer (Float/BigFloat SchemeString, number->string)
// depend on — so those sites share one source of truth rather than repeating
// the literals.
//
// The writer only ever emits PositiveInfinityString / NegativeInfinityString /
// NaNString. NegativeNaNString exists solely because the reader accepts "-nan.0"
// as an input alias for NaN (there is no signed NaN in the external syntax);
// keep it paired with NaNString in reader case lists.
const (
	PositiveInfinityString = "+inf.0"
	NegativeInfinityString = "-inf.0"
	NaNString              = "+nan.0"
	NegativeNaNString      = "-nan.0"
)
