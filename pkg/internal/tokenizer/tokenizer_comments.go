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

import (
	"errors"
	"io"

	"github.com/aalpar/wile/pkg/werr"
)

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
				if p.blockDepth == 0 {
					// Found closing |#; consume it and stop. (p.value is unused for
					// block comments: the token carries only raw text.)
					// Consuming the '#' may hit EOF; the comment is closed all the
					// same, so no error check may intervene before this return.
					p.next()
					if len(p.value) > 0 {
						p.value = p.value[:len(p.value)-1]
					}
					p.term()
					return NewSimpleToken(p.state, string(p.text), "", &p.tokenStart, &p.tokenEnd, false, 0)
				}
				p.next()
				if p.err != nil {
					break
				}
				p.blockDepth--
			}

		default:
			p.next()
		}
	}
	// R7RS §2.2: a block comment runs to its matching |#. Reaching EOF first is
	// malformed input, not an acceptable end of it — the comment was silently
	// accepted before, so an unclosed #| swallowed the rest of the file.
	if errors.Is(p.err, io.EOF) {
		p.failWrap(werr.ErrIncompleteInput, MessageUnterminatedBlockComment)
	}
	p.term()
	return NewSimpleToken(p.state, string(p.text), "", &p.tokenStart, &p.tokenEnd, false, 0)
}

func (p *Tokenizer) readLineCommentOrPragma() {
	for p.err == nil && p.curr() == ';' {
		p.next()
	}
	for p.err == nil && !isLineEnding(p.curr()) {
		p.next()
	}
}
