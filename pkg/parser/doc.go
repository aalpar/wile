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

// Package parser implements R7RS Scheme syntax parsing.
//
// The parser converts a token stream into syntax values with source location:
//
// # Features
//
//   - All Scheme datums: literals, symbols, lists, vectors, bytevectors
//   - Full numeric tower: integers, floats, rationals, complex, big numbers
//   - Quote forms: ', `, ,, ,@ and syntax variants #', #`, #,, #,@
//   - Datum labels: #n= and #n# for shared/circular structures (R7RS 2.4)
//   - Case folding: #!fold-case and #!no-fold-case directives
//   - Symbol interning at parse time
//
// # Usage
//
//	p := parser.NewParserWithFile(env, true, reader, "example.scm")
//	for {
//	    stx, err := p.ReadSyntax(ctx)
//	    if err == io.EOF {
//	        break
//	    }
//	    // process stx
//	}
//
// # Error Handling
//
// Parse errors are wrapped in [ParserError] with source location from the
// offending token. The parser accumulates no state across expressions.
package parser
