package sourceload

import (
	"errors"
	"io/fs"
	"path"

	"github.com/aalpar/wile/pkg/werr"
)

// Finder locates files within an fs.FS by searching an ordered list of
// directories. It supports an optional LoadStack for current-directory-relative
// resolution and an optional canonicalize function applied to resolved paths.
type Finder struct {
	fsys         fs.FS
	searchDirs   []string
	stack        *LoadStack
	canonicalize func(string) string
}

// FinderOption is a functional option for Finder.
type FinderOption func(*Finder)

// WithStack attaches a LoadStack to the Finder. When set, the directory of
// the currently-loading file is searched first (before explicit searchDirs).
func WithStack(s *LoadStack) FinderOption {
	return func(p *Finder) {
		p.stack = s
	}
}

// WithCanonicalize sets a path transformation applied to every resolved path
// before it is returned from Open. It does not affect which file is opened —
// only the returned path string.
func WithCanonicalize(fn func(string) string) FinderOption {
	return func(p *Finder) {
		p.canonicalize = fn
	}
}

// NewFinder constructs a Finder that searches fsys in the given directories.
// It panics if fsys is nil.
func NewFinder(fsys fs.FS, searchDirs []string, opts ...FinderOption) *Finder {
	if fsys == nil {
		panic(werr.WrapForeignErrorf(werr.ErrEngineInit, "NewFinder: fsys must not be nil"))
	}
	p := &Finder{
		fsys:       fsys,
		searchDirs: searchDirs,
	}
	for _, opt := range opts {
		opt(p)
	}
	return p
}

// Open searches for the named path and returns an open file, the resolved
// path, and nil on success. It returns ErrNotFound when no match is found.
//
// Search order:
//  1. stack.CurrentDir() — if stack is non-nil, CurrentDir is non-empty, and
//     is not "."
//  2. Each searchDir provided to NewFinder, in order
//  3. FS root "."
//
// The returned path goes through canonicalize (if set) before being returned.
// Returns ErrNotFound immediately for an empty path argument.
func (p *Finder) Open(name string) (fs.File, string, error) {
	if name == "" {
		return nil, "", ErrNotFound
	}
	for _, dir := range p.buildSearchDirs() {
		candidate := path.Join(dir, name)
		if !fs.ValidPath(candidate) {
			continue
		}
		_, err := fs.Stat(p.fsys, candidate)
		if err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				continue
			}
			return nil, "", err
		}
		f, err := p.fsys.Open(candidate)
		if err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				continue
			}
			return nil, "", err
		}
		resolved := candidate
		if p.canonicalize != nil {
			resolved = p.canonicalize(candidate)
		}
		return f, resolved, nil
	}
	return nil, "", ErrNotFound
}

// buildSearchDirs returns the ordered list of directories to try. The stack's
// current directory (if meaningful) comes first, followed by the configured
// search directories, then the FS root ".".
func (p *Finder) buildSearchDirs() []string {
	q := make([]string, 0, len(p.searchDirs)+2)
	if p.stack != nil {
		cur := p.stack.CurrentDir()
		if cur != "" && cur != "." {
			q = append(q, cur)
		}
	}
	q = append(q, p.searchDirs...)
	q = append(q, ".")
	return q
}
