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

package syntax

import "github.com/aalpar/wile/pkg/values"

// SourceContext is the source-location-and-hygiene metadata type. It is
// defined in package values so that values.emptyListType can implement
// values.SyntaxValue directly (the empty-list duality merge — see
// values/syntax_value.go and the comment in values/source_context.go).
type SourceContext = values.SourceContext

// OriginInfo tracks macro expansion chains. Defined in package values
// alongside SourceContext.
type OriginInfo = values.OriginInfo

// NewSourceContext constructs a SourceContext.
func NewSourceContext(text, file string, start, end SourceIndexes) *SourceContext {
	return values.NewSourceContext(text, file, start, end)
}

// NewZeroValueSourceContext constructs an empty SourceContext.
func NewZeroValueSourceContext() *SourceContext {
	return values.NewZeroValueSourceContext()
}

// FormatOriginChain renders a macro expansion chain as a string.
// maxDepth limits how many expansions to show (0 = unlimited).
func FormatOriginChain(origin *OriginInfo, maxDepth int) string {
	return values.FormatOriginChain(origin, maxDepth)
}

// DefaultMaxOriginDepth is how many macro expansions a diagnostic names before
// it summarizes the rest.
const DefaultMaxOriginDepth = 10

// OriginChainText renders the expansion chain src carries, at
// DefaultMaxOriginDepth, or "" when src is nil or was written by hand. It is the
// one rendering both diagnostic surfaces use: the Go error text
// (compilation.SourcedError) and the Scheme condition's message
// (machine.ConditionFromError), so the two cannot disagree on depth or format.
func OriginChainText(src *SourceContext) string {
	if src == nil {
		return ""
	}
	return FormatOriginChain(src.Origin, DefaultMaxOriginDepth)
}
