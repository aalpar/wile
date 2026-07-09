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

package tokenizer

func (p *Tokenizer) continueBlockComment() Token {
	p.state = TokenizerStateBlockCommentBody
	// Read content until we find |# at depth 0 or EOF
	for p.err == nil {
		switch {
		case isMarker(p.curr()):
			p.next()
			if p.err != nil {
				break
			}
			if isVerticalLine(p.curr()) {
				// Nested #|
				p.blockDepth++
				p.next()
			}

		case isVerticalLine(p.curr()):
			p.next()
			if p.err != nil {
				break
			}
			if isMarker(p.curr()) { // Found |#
				p.next()
				if p.err != nil {
					break
				}
				if p.blockDepth == 0 {
					// Found closing |# - remove the | from scratch and stop
					if len(p.value) > 0 {
						p.value = p.value[:len(p.value)-1]
					}
					p.term()
					return NewSimpleToken(p.state, p.text, "", &p.tokenStart, &p.tokenEnd, false, 0)
				}
				p.blockDepth--
			}

		default:
			p.next()
		}
	}
	// EOF before closing - emit Body token, no End will follow
	p.term()
	return NewSimpleToken(p.state, p.text, "", &p.tokenStart, &p.tokenEnd, false, 0)
}

func (p *Tokenizer) readLineCommentOrPragma() {
	for p.err == nil && p.curr() == ';' {
		p.next()
	}
	for p.err == nil && !isLineEnding(p.curr()) {
		p.next()
	}
}

// readVectorOrExactnessOrRadixOrModifierOrMnemonicOrBooleanOrComment handles # prefixed tokens.
// Called after '#' is consumed. Dispatches based on the next character to parse:
//   - #' #` #, #,@ : syntax quotation
