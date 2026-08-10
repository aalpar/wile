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

package core_test

import (
	"context"

	"github.com/aalpar/wile/pkg/schemeutil"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// mustDatumToSyntax converts test data that is known acyclic. The error arm
// exists because DatumToSyntaxValue refuses a circular datum, which no test in
// this package constructs.
func mustDatumToSyntax(sctx *syntax.SourceContext, v values.Value) syntax.SyntaxValue {
	q, err := schemeutil.DatumToSyntaxValue(context.Background(), sctx, v)
	if err != nil {
		panic(werr.WrapForeignErrorf(err, "mustDatumToSyntax: test datum is circular"))
	}
	return q
}
