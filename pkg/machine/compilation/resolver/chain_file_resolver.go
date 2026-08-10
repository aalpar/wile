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

package resolver

import (
	"context"
	"errors"
	"io/fs"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/werr"
)

// ChainFileResolver tries multiple resolvers in order, falling through to the
// next on absence, and on a denial only when the next resolver will re-ask
// under a different source (see continuesPast). Anything else — a permission
// failure, any other I/O error — propagates immediately, so a higher-priority
// file that exists but cannot be read is never silently replaced by a
// lower-priority one.
type ChainFileResolver struct {
	resolvers []environment.FileResolver
}

// NewChainFileResolver creates a resolver that tries each resolver in order.
// Panics if resolvers is empty.
func NewChainFileResolver(resolvers []environment.FileResolver) *ChainFileResolver {
	if len(resolvers) == 0 {
		panic(werr.WrapForeignErrorf(werr.ErrEngineInit, "NewChainFileResolver: resolvers must not be empty"))
	}
	return &ChainFileResolver{
		resolvers: resolvers,
	}
}

// ResolveAndOpen tries each resolver in order, returning the first successful
// result. The reported failure is the last resolver's, which is the only one
// that searched to the end of the chain.
func (p *ChainFileResolver) ResolveAndOpen(ctx context.Context, path string) (fs.File, string, error) {
	var lastErr error
	for i, r := range p.resolvers {
		f, resolved, err := r.ResolveAndOpen(ctx, path)
		if err == nil {
			return f, resolved, nil
		}
		if !p.continuesPast(err, i) {
			return nil, "", err
		}
		lastErr = err
	}
	return nil, "", lastErr
}

// continuesPast reports whether the chain may look in resolvers[i+1:] after
// resolvers[i] failed with err. Absence always continues. A denial continues
// only when the next resolver authorizes under a DIFFERENT source.
//
// Both halves of that condition are load-bearing.
//
// It must continue sometimes, or the documented WithSourceFS + WithSourceOS
// pairing breaks under every path-confining authorizer: since a candidate is
// now authorized before it is stat'd (which is what closes the existence
// oracle), a name absent from the fs.FS is refused rather than reported
// missing, and refusing the virtual namespace says nothing about the host file
// the OS resolver holds.
//
// It must NOT continue otherwise. A resolver asking the same question gets the
// same answer, so continuing is pointless; and one asking NO question —
// EmbedFileResolver, which authorizes nothing and therefore implements no
// SourceGate — would hand out its copy of the very file the policy just
// refused.
func (p *ChainFileResolver) continuesPast(err error, i int) bool {
	if IsNotFound(err) {
		return true
	}
	if !errors.Is(err, security.ErrAccessDenied) {
		return false
	}
	if i+1 >= len(p.resolvers) {
		return false
	}
	refused, ok := p.resolvers[i].(SourceGate)
	if !ok {
		return false
	}
	next, ok := p.resolvers[i+1].(SourceGate)
	if !ok {
		return false
	}
	return next.AuthorizedSource() != refused.AuthorizedSource()
}

// EnumerateFiles unions file enumerations from all child resolvers that
// implement FileEnumerator. Results are concatenated in resolver order
// with no deduplication; ordering implies priority.
// Best-effort: walk errors are accumulated and returned alongside
// partial results, matching OSFileResolver and FSFileResolver semantics.
func (p *ChainFileResolver) EnumerateFiles() ([]string, error) {
	var result []string
	var chainErrs []error

	for _, r := range p.resolvers {
		enumerator, ok := r.(FileEnumerator)
		if !ok {
			continue
		}
		files, err := enumerator.EnumerateFiles()
		result = append(result, files...)
		if err != nil {
			chainErrs = append(chainErrs, err)
		}
	}

	return result, errors.Join(chainErrs...)
}
