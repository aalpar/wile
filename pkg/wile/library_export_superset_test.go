package wile_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// newStdlibEngine builds a KitchenSink engine with the embedded stdlib resolvable,
// the construction the other engine_stdlib tests use.
func newStdlibEngine(ctx context.Context, t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
	)
	qt.Assert(t, err, qt.IsNil)
	return eng
}

// TestLibraryExport_OnlyEnforcesMembership is the methodology guard for
// TestLibraryExportSupersets: (only <lib> id) must reject an id the library does
// not export. Without this, every positive assertion in the superset table below
// would be vacuous — an (only ...) that silently admitted unknown names would
// "prove" nothing. applyToExports raises here at expand time.
func TestLibraryExport_OnlyEnforcesMembership(t *testing.T) {
	ctx := context.Background()
	eng := newStdlibEngine(ctx, t)
	defer func() {
		_ = eng.Close()
	}()

	_, err := eng.EvalMultiple(ctx,
		`(import (only (scheme cxr) definitely-not-exported-xyz)) 42`)
	qt.Assert(t, err, qt.IsNotNil,
		qt.Commentf("(only ...) must reject a non-exported identifier, else the "+
			"superset pins below are vacuous"))
}

// TestLibraryExportSupersets pins the deliberate non-R7RS export supersets
// documented in docs/reference/r7rs-differences.md → "Standard-Library Export
// Supersets". Each binding below is homed by R7RS in a DIFFERENT library; Wile
// additionally re-exports it from the library named here as an embedding
// convenience, so a program can reach it without importing its formal home.
//
// (only <lib> id) resolves strictly through <lib>'s own export set — the
// preceding methodology guard proves applyToExports rejects a non-member — so an
// (only ...) import that compiles is proof the superset export exists. These are
// deviations, not R7RS: a strict implementation exports each id only from its
// home library, so code importing it from the convenience library here does not
// port. If a future change narrows Wile to the strict R7RS surface, this test
// fails and forces a deliberate docs-and-deviation update rather than a silent
// public-API break.
func TestLibraryExportSupersets(t *testing.T) {
	ctx := context.Background()

	tcs := []struct {
		name string
		src  string // an (only ...) import that compiles iff the superset export exists
	}{
		// (scheme base) — R7RS homes: case-lambda → (scheme case-lambda),
		// read → (scheme read), write / display → (scheme write). The finite? /
		// infinite? / nan? trio (home (scheme inexact)) is a separately documented
		// dual-home and is not re-pinned here.
		{"base/case-lambda", `(import (only (scheme base) case-lambda)) 42`},
		{"base/read", `(import (only (scheme base) read)) 42`},
		{"base/write", `(import (only (scheme base) write)) 42`},
		{"base/display", `(import (only (scheme base) display)) 42`},

		// (scheme eval) — R7RS home: (scheme r5rs).
		{"eval/scheme-report-environment",
			`(import (only (scheme eval) scheme-report-environment)) 42`},
		{"eval/null-environment", `(import (only (scheme eval) null-environment)) 42`},

		// (scheme cxr) — R7RS home for the two-deep accessors: (scheme base).
		{"cxr/caar", `(import (only (scheme cxr) caar)) 42`},
		{"cxr/cadr", `(import (only (scheme cxr) cadr)) 42`},
		{"cxr/cdar", `(import (only (scheme cxr) cdar)) 42`},
		{"cxr/cddr", `(import (only (scheme cxr) cddr)) 42`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng := newStdlibEngine(ctx, t)
			defer func() {
				_ = eng.Close()
			}()

			q, err := eng.EvalMultiple(ctx, tc.src)
			qt.Assert(t, err, qt.IsNil,
				qt.Commentf("the superset export is a documented deviation "+
					"(r7rs-differences.md → Standard-Library Export Supersets)"))
			qt.Assert(t, q.SchemeString(), qt.Equals, "42")
		})
	}
}
