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

package compilation

import (
	"github.com/aalpar/wile/pkg/syntax"
)

// pushOrigin returns the OriginInfo for one macro expansion: the macro's name
// and where it was used, chained onto the origin the use site already carried.
// The parent is read off the use site's own context, which is what links a
// macro that expands another: the inner use is written by the outer's template,
// so it arrives wearing the outer expansion's origin.
//
// The origin chain is stamped at the three places a transformer's output leaves
// the kernel, not inside any one transformer: expandMacroInvocation and
// ExpandOnce for closure transformers (syntax-rules, syntax-case and anything a
// lambda defines) and invokeERTransformer for explicit-renaming ones. Exactly
// one of the three fires for an invocation: expandMacroInvocation returns into
// the ER path before its closure arm and ExpandOnce does the same, so a form is
// never stamped twice.
func pushOrigin(macroName string, useSiteCtx *syntax.SourceContext) *syntax.OriginInfo {
	q := &syntax.OriginInfo{
		Identifier: macroName,
		Location:   useSiteCtx,
	}
	if useSiteCtx != nil {
		q.Parent = useSiteCtx.Origin
	}
	return q
}
