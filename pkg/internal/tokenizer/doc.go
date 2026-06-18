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

// Package tokenizer implements R7RS Scheme lexical analysis.
//
// The tokenizer converts a Unicode rune stream into tokens with source positions:
//
// # Token Categories
//
//   - Delimiters: (, ), (), .
//   - Quotation: ', `, ,, ,@ and syntax variants
//   - Numbers: integers, decimals, rationals, scientific, complex, polar
//   - Special: +inf.0, -inf.0, +nan.0, imaginary variants
//   - Big numbers: #z prefix for BigInteger, #m for BigFloat
//   - Literals: symbols, strings, characters
//   - Booleans: #t, #f, #true, #false
//   - Comments: line (;), block (#|...|#), datum (#;)
//   - Vectors: #(, #u8(
//   - Labels: #n=, #n#
//
// # Usage
//
//	tok := tokenizer.NewTokenizer(reader, caseInsensitive)
//	for {
//	    token, err := tok.Next()
//	    if err == io.EOF {
//	        break
//	    }
//	    // process token
//	}
//
// Each [Token] provides source position via Start() and End(), raw text
// via String(), and processed value (escapes resolved) via Value().
package tokenizer
