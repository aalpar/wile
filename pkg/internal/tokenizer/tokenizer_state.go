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

import "strconv"

// tokenizerStateNames is the single source of truth for state names, indexed by
// TokenizerState and index-parallel to the const block in tokenizer.go.
//
// Names are kebab-case rather than the Go identifier because they are read by
// Scheme users in reader diagnostics, not only by Go developers. The mapping
// back to the constant is mechanical: unsigned-integer is
// TokenizerStateUnsignedInteger.
var tokenizerStateNames = [tokenizerStateCount]string{
	TokenizerStateFailed: "failed",

	TokenizerStateSyntax:           "syntax",
	TokenizerStateUnsyntax:         "unsyntax",
	TokenizerStateUnsyntaxSplicing: "unsyntax-splicing",
	TokenizerStateQuasisyntax:      "quasisyntax",

	TokenizerStateQuote:           "quote",
	TokenizerStateUnquote:         "unquote",
	TokenizerStateUnquoteSplicing: "unquote-splicing",
	TokenizerStateQuasiquote:      "quasiquote",

	TokenizerStateSignedInf:            "signed-inf",
	TokenizerStateSignedNan:            "signed-nan",
	TokenizerStateSignedImaginaryInf:   "signed-imaginary-inf",
	TokenizerStateSignedImaginaryNan:   "signed-imaginary-nan",
	TokenizerStateSignedImaginary:      "signed-imaginary",
	TokenizerStateSignedComplex:        "signed-complex",
	TokenizerStateSignedComplexPolar:   "signed-complex-polar",
	TokenizerStateUnsignedImaginaryInf: "unsigned-imaginary-inf",
	TokenizerStateUnsignedImaginaryNan: "unsigned-imaginary-nan",
	TokenizerStateUnsignedImaginary:    "unsigned-imaginary",
	TokenizerStateUnsignedComplex:      "unsigned-complex",
	TokenizerStateUnsignedComplexPolar: "unsigned-complex-polar",

	TokenizerStateMarker:              "marker",
	TokenizerStateMarkerBooleanFalse:  "marker-boolean-false",
	TokenizerStateMarkerBooleanTrue:   "marker-boolean-true",
	TokenizerStateMarkerNumberInexact: "marker-number-inexact",
	TokenizerStateMarkerNumberExact:   "marker-number-exact",

	TokenizerStateSignedInteger:   "signed-integer",
	TokenizerStateUnsignedInteger: "unsigned-integer",

	TokenizerStateBigFloat:   "big-float",
	TokenizerStateBigInteger: "big-integer",

	TokenizerStateMarkerBase2:  "marker-base-2",
	TokenizerStateMarkerBase8:  "marker-base-8",
	TokenizerStateMarkerBase10: "marker-base-10",
	TokenizerStateMarkerBase16: "marker-base-16",

	TokenizerStateSignedDecimalFraction:    "signed-decimal-fraction",
	TokenizerStateSignedRationalFraction:   "signed-rational-fraction",
	TokenizerStateUnsignedRationalFraction: "unsigned-rational-fraction",
	TokenizerStateUnsignedDecimalFraction:  "unsigned-decimal-fraction",

	TokenizerStateSignedScientificNotation:   "signed-scientific-notation",
	TokenizerStateUnsignedScientificNotation: "unsigned-scientific-notation",

	TokenizerStateEmptyList:    "empty-list",
	TokenizerStateOpenParen:    "open-paren",
	TokenizerStateCloseParen:   "close-paren",
	TokenizerStateOpenBracket:  "open-bracket",
	TokenizerStateCloseBracket: "close-bracket",
	TokenizerStateCons:         "cons",

	TokenizerStateString: "string",

	TokenizerStateCharMnemonicOrHexEscape: "char-mnemonic-or-hex-escape",
	TokenizerStateCharMnemonic:            "char-mnemonic",
	TokenizerStateCharHexEscape:           "char-hex-escape",
	TokenizerStateCharGraphic:             "char-graphic",

	TokenizerStateLineCommentBody:   "line-comment-body",
	TokenizerStateBlockCommentBody:  "block-comment-body",
	TokenizerStateDatumCommentBegin: "datum-comment-begin",

	TokenizerStateSymbol: "symbol",

	TokenizerStateOpenVector:                   "open-vector",
	TokenizerStateOpenVectorUnsignedByteMarker: "open-vector-unsigned-byte-marker",

	TokenizerStateDirective:       "directive",
	TokenizerStateLabelReference:  "label-reference",
	TokenizerStateLabelAssignment: "label-assignment",

	TokenizerStateBoxBegin: "box-begin",
}

// String returns the state's kebab-case name, for reader diagnostics.
//
// An out-of-range value renders as its number rather than a fixed "unknown", so
// a state added to the const block without a tokenizerStateNames entry is still
// identifiable from a user's bug report.
func (p TokenizerState) String() string {
	if p < 0 || p >= tokenizerStateCount {
		return "tokenizer-state-" + strconv.Itoa(int(p))
	}
	return tokenizerStateNames[p]
}
