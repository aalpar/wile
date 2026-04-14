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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/werr"
)

// ChainFileResolver tries multiple resolvers in order, falling through
// to the next on ErrFileNotFound. Non-file-not-found errors (security
// denials, I/O errors) propagate immediately.
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
// result. Falls through on ErrFileNotFound; other errors propagate immediately.
func (p *ChainFileResolver) ResolveAndOpen(ctx context.Context, path string) (fs.File, string, error) {
	var lastErr error
	for _, r := range p.resolvers {
		f, resolved, err := r.ResolveAndOpen(ctx, path)
		if err == nil {
			return f, resolved, nil
		}
		if !errors.Is(err, werr.ErrFileNotFound) {
			return nil, "", err
		}
		lastErr = err
	}
	return nil, "", lastErr
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
