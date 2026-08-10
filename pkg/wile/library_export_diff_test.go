package wile_test

import (
	"context"
	"io/fs"
	"os"
	"path"
	"sort"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// This file is the generated export-set conformance diff: for every bundled
// .sld that has an external authority, it diffs the library's (export ...)
// form against that authority in BOTH directions — names the authority has and
// the .sld lacks (missing), and names the .sld has and the authority lacks
// (extra) — and requires each delta to be explicitly sanctioned.
//
// Sanctioned extras are cross-checked against docs/reference/r7rs-differences.md
// → "Standard-Library Export Supersets", so a new extra cannot be sanctioned
// here without a doc row. That is the 2026-07-14 export-superset policy
// (TODO.md, commit cc3c48bb) made executable: a non-conformant export is
// documented as a deliberate deviation rather than deleted, because removing an
// export is a user-visible API break for out-of-repo consumers.
//
// GATES THIS FILE DOES NOT COVER, and where each is covered instead:
//
//   - Arity and semantics. The diff compares NAMES. A library can export the
//     right name with the wrong signature and pass here: SRFI-13's
//     substring/shared (2-arg optional end) and string-upcase/string-downcase
//     (optional [start end]) are exactly that failure, and need behavioural arms
//     in test/scheme/. TestLibraryExportDiff_SanctionedExtrasResolve below
//     proves only that a sanctioned extra RESOLVES, not that it behaves.
//   - Libraries with no ratified authority. (chibi ...), (rnrs ...) and
//     (wile ...) are excluded by noAuthorityPrefixes; (srfi 14) and (srfi 132)
//     are excluded by name. The partition guard below makes every exclusion
//     explicit, so a newly bundled .sld fails until it is classified.
//   - Whether an authority transcription is faithful to its source document.
//     wantAuthorityCount pins each list's length so an accidental edit or a
//     duplicated name fails, but only a human re-reading the spec can catch a
//     name transcribed wrongly in both places.
//   - Import-time conflicts between libraries (R7RS §5.6). Covered by
//     import_conflict_test.go.

// r7rsBase is R7RS Appendix A's (scheme base) export list, transcribed verbatim
// from https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-10.html.
const r7rsBase = `
* + - ... / < <= = => > >= _ abs and append apply assoc assq assv begin
binary-port? boolean=? boolean? bytevector bytevector-append bytevector-copy
bytevector-copy! bytevector-length bytevector-u8-ref bytevector-u8-set!
bytevector? caar cadr call-with-current-continuation call-with-port
call-with-values call/cc car case cdar cddr cdr ceiling char->integer
char-ready? char<=? char<? char=? char>=? char>? char? close-input-port
close-output-port close-port complex? cond cond-expand cons current-error-port
current-input-port current-output-port define define-record-type define-syntax
define-values denominator do dynamic-wind else eof-object eof-object? eq?
equal? eqv? error error-object-irritants error-object-message error-object?
even? exact exact-integer-sqrt exact-integer? exact? expt features file-error?
floor floor-quotient floor-remainder floor/ flush-output-port for-each gcd
get-output-bytevector get-output-string guard if include include-ci inexact
inexact? input-port-open? input-port? integer->char integer? lambda lcm length
let let* let*-values let-syntax let-values letrec letrec* letrec-syntax list
list->string list->vector list-copy list-ref list-set! list-tail list?
make-bytevector make-list make-parameter make-string make-vector map max member
memq memv min modulo negative? newline not null? number->string number?
numerator odd? open-input-bytevector open-input-string open-output-bytevector
open-output-string or output-port-open? output-port? pair? parameterize
peek-char peek-u8 port? positive? procedure? quasiquote quote quotient raise
raise-continuable rational? rationalize read-bytevector read-bytevector!
read-char read-error? read-line read-string read-u8 real? remainder reverse
round set! set-car! set-cdr! square string string->list string->number
string->symbol string->utf8 string->vector string-append string-copy
string-copy! string-fill! string-for-each string-length string-map string-ref
string-set! string<=? string<? string=? string>=? string>? string? substring
symbol->string symbol=? symbol? syntax-error syntax-rules textual-port?
truncate truncate-quotient truncate-remainder truncate/ u8-ready? unless
unquote unquote-splicing utf8->string values vector vector->list vector->string
vector-append vector-copy vector-copy! vector-fill! vector-for-each
vector-length vector-map vector-ref vector-set! vector? when
with-exception-handler write-bytevector write-char write-string write-u8 zero?
`

// r7rsR5RS is R7RS Appendix A's (scheme r5rs) export list. Appendix A defines it
// as "the identifiers defined by R5RS", which is why call/cc — an R7RS-added
// synonym, per R7RS "Language changes" — is absent from it.
const r7rsR5RS = `
* + - ... / < <= = => > >= _ abs acos and angle append apply asin assoc assq
assv atan begin boolean? caaaar caaadr caaar caadar caaddr caadr caar cadaar
cadadr cadar caddar cadddr caddr cadr call-with-current-continuation
call-with-input-file call-with-output-file call-with-values car case cdaaar
cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr
cddr cdr ceiling char->integer char-alphabetic? char-ci<=? char-ci<? char-ci=?
char-ci>=? char-ci>? char-downcase char-lower-case? char-numeric? char-ready?
char-upcase char-upper-case? char-whitespace? char<=? char<? char=? char>=?
char>? char? close-input-port close-output-port complex? cond cons cos
current-input-port current-output-port define define-syntax delay denominator
display do dynamic-wind else eof-object? eq? equal? eqv? eval even?
exact->inexact exact? exp expt floor for-each force gcd if imag-part
inexact->exact inexact? input-port? integer->char integer?
interaction-environment lambda lcm length let let* let-syntax letrec
letrec-syntax list list->string list->vector list-ref list-tail list? load log
magnitude make-polar make-rectangular make-string make-vector map max member
memq memv min modulo negative? newline not null-environment null?
number->string number? numerator odd? open-input-file open-output-file or
output-port? pair? peek-char positive? procedure? quasiquote quote quotient
rational? rationalize read read-char real-part real? remainder reverse round
scheme-report-environment set! set-car! set-cdr! sin sqrt string string->list
string->number string->symbol string-append string-ci<=? string-ci<?
string-ci=? string-ci>=? string-ci>? string-copy string-fill! string-length
string-ref string-set! string<=? string<? string=? string>=? string>? string?
substring symbol->string symbol? syntax-rules tan truncate values vector
vector->list vector-fill! vector-length vector-ref vector-set! vector?
with-input-from-file with-output-to-file write write-char zero?
`

// srfi1Authority is SRFI-1's procedure index (https://srfi.schemers.org/srfi-1/).
// It includes the R5RS list procedures SRFI-1 re-exports unchanged (cons, car,
// map, assoc, …) and the 28 c…r compositions, because a conforming (srfi 1)
// must supply them all — importing (srfi 1) alone has to give a working list
// library.
const srfi1Authority = `
cons list xcons cons* make-list list-tabulate list-copy circular-list iota
pair? null? proper-list? circular-list? dotted-list? not-pair? null-list?
list= car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar
cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr
cdadar cdaddr cddaar cddadr cdddar cddddr list-ref first second third fourth
fifth sixth seventh eighth ninth tenth car+cdr take drop take-right drop-right
take! drop-right! split-at split-at! last last-pair length length+ append
concatenate reverse append! concatenate! reverse! append-reverse
append-reverse! zip unzip1 unzip2 unzip3 unzip4 unzip5 count fold fold-right
pair-fold pair-fold-right reduce reduce-right unfold unfold-right map for-each
append-map append-map! map! map-in-order pair-for-each filter-map filter
partition remove filter! partition! remove! find find-tail take-while
drop-while take-while! span break span! break! any every list-index member
memq memv delete delete-duplicates delete! delete-duplicates! assoc assq assv
alist-cons alist-copy alist-delete alist-delete! lset<= lset= lset-adjoin
lset-union lset-intersection lset-difference lset-xor lset-diff+intersection
lset-union! lset-intersection! lset-difference! lset-xor!
lset-diff+intersection! set-car! set-cdr!
`

// srfi13Authority is SRFI-13's procedure index (https://srfi.schemers.org/srfi-13/).
const srfi13Authority = `
string? string-null? string-every string-any make-string string string-tabulate
string->list list->string reverse-list->string string-join string-length
string-ref string-copy substring/shared string-copy! string-take
string-take-right string-drop string-drop-right string-pad string-pad-right
string-trim string-trim-right string-trim-both string-set! string-fill!
string-compare string-compare-ci string<> string= string< string> string<=
string>= string-ci<> string-ci= string-ci< string-ci> string-ci<= string-ci>=
string-hash string-hash-ci string-prefix-length string-suffix-length
string-prefix-length-ci string-suffix-length-ci string-prefix? string-suffix?
string-prefix-ci? string-suffix-ci? string-index string-index-right
string-skip string-skip-right string-count string-contains string-contains-ci
string-titlecase string-titlecase! string-upcase string-upcase!
string-downcase string-downcase! string-reverse string-reverse! string-append
string-concatenate string-concatenate/shared string-append/shared
string-concatenate-reverse string-concatenate-reverse/shared string-map
string-map! string-fold string-fold-right string-unfold string-unfold-right
string-for-each string-for-each-index xsubstring string-xcopy! string-replace
string-tokenize string-filter string-delete string-parse-start+end
string-parse-final-start+end let-string-start+end check-substring-spec
substring-spec-ok? make-kmp-restart-vector kmp-step string-kmp-partial-search
`

// exportAuthorityRow is one (library, authority list, sanctioned extras) entry.
// sanctionedOmissions is the other direction: authority names the library
// deliberately does not supply.
type exportAuthorityRow struct {
	// lib is the library name as an import spec, e.g. "(scheme base)".
	lib string
	// sldPath is the path inside stdlib.FS, e.g. "scheme/base.sld".
	sldPath string
	// authority is the whitespace-separated authority export list.
	authority string
	// wantAuthorityCount pins the transcription length of authority.
	wantAuthorityCount int
	// sanctionedExtras are exports the .sld has and the authority does not.
	// Each must also appear in r7rs-differences.md, either as a supersets-table
	// row or in docProseExtras below.
	sanctionedExtras []string
	// sanctionedOmissions are authority names the .sld deliberately omits.
	sanctionedOmissions []string
	// why explains the omissions; empty when there are none.
	why string
}

// exportAuthorityRows is the diff table. Every bundled .sld with an external
// authority appears here; every one without appears in a no-authority rule
// below, and TestLibraryExportDiff_PartitionsEveryBundledLibrary proves the two
// together account for the whole embedded stdlib.
var exportAuthorityRows = []exportAuthorityRow{
	{
		lib:                "(scheme base)",
		sldPath:            "scheme/base.sld",
		authority:          r7rsBase,
		wantAuthorityCount: 238,
		sanctionedExtras: []string{
			"case-lambda", "delay", "delay-force", "display", "finite?",
			"force", "infinite?", "nan?", "read", "write",
		},
	},
	{
		lib:                "(scheme case-lambda)",
		sldPath:            "scheme/case-lambda.sld",
		authority:          `case-lambda`,
		wantAuthorityCount: 1,
	},
	{
		lib:     "(scheme char)",
		sldPath: "scheme/char.sld",
		authority: `
char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>?
char-downcase char-foldcase char-lower-case? char-numeric? char-upcase
char-upper-case? char-whitespace? digit-value string-ci<=? string-ci<?
string-ci=? string-ci>=? string-ci>? string-downcase string-foldcase
string-upcase
`,
		wantAuthorityCount: 22,
	},
	{
		lib:                "(scheme complex)",
		sldPath:            "scheme/complex.sld",
		authority:          `angle imag-part magnitude make-polar make-rectangular real-part`,
		wantAuthorityCount: 6,
	},
	{
		lib:     "(scheme cxr)",
		sldPath: "scheme/cxr.sld",
		authority: `
caaaar caaadr caaar caadar caaddr caadr cadaar cadadr cadar caddar cadddr
caddr cdaaar cdaadr cdaar cdadar cdaddr cdadr cddaar cddadr cddar cdddar
cddddr cdddr
`,
		wantAuthorityCount: 24,
		sanctionedExtras:   []string{"caar", "cadr", "cdar", "cddr"},
	},
	{
		lib:                "(scheme eval)",
		sldPath:            "scheme/eval.sld",
		authority:          `environment eval`,
		wantAuthorityCount: 2,
		sanctionedExtras:   []string{"null-environment", "scheme-report-environment"},
	},
	{
		lib:     "(scheme file)",
		sldPath: "scheme/file.sld",
		authority: `
call-with-input-file call-with-output-file delete-file file-exists?
open-binary-input-file open-binary-output-file open-input-file open-output-file
with-input-from-file with-output-to-file
`,
		wantAuthorityCount: 10,
	},
	{
		lib:                "(scheme inexact)",
		sldPath:            "scheme/inexact.sld",
		authority:          `acos asin atan cos exp finite? infinite? log nan? sin sqrt tan`,
		wantAuthorityCount: 12,
	},
	{
		lib:                "(scheme lazy)",
		sldPath:            "scheme/lazy.sld",
		authority:          `delay delay-force force make-promise promise?`,
		wantAuthorityCount: 5,
	},
	{
		lib:                "(scheme load)",
		sldPath:            "scheme/load.sld",
		authority:          `load`,
		wantAuthorityCount: 1,
	},
	{
		lib:     "(scheme process-context)",
		sldPath: "scheme/process-context.sld",
		authority: `
command-line emergency-exit exit get-environment-variable
get-environment-variables
`,
		wantAuthorityCount: 5,
	},
	{
		lib:                "(scheme r5rs)",
		sldPath:            "scheme/r5rs.sld",
		authority:          r7rsR5RS,
		wantAuthorityCount: 222,
		// Kept, not deleted, under the 2026-07-14 export-superset policy. call/cc
		// is the sharpest case for that policy: it is excluded by Appendix A's
		// enumeration AND by the library's defining sentence, and until this file
		// existed it was pinned by nothing — so dropping it would have failed no
		// test, which is exactly the silent public-API break the policy prevents.
		sanctionedExtras: []string{"call/cc", "unquote", "unquote-splicing"},
	},
	{
		lib:                "(scheme read)",
		sldPath:            "scheme/read.sld",
		authority:          `read`,
		wantAuthorityCount: 1,
	},
	{
		lib:                "(scheme repl)",
		sldPath:            "scheme/repl.sld",
		authority:          `interaction-environment`,
		wantAuthorityCount: 1,
	},
	{
		lib:                "(scheme time)",
		sldPath:            "scheme/time.sld",
		authority:          `current-jiffy current-second jiffies-per-second`,
		wantAuthorityCount: 3,
	},
	{
		lib:                "(scheme write)",
		sldPath:            "scheme/write.sld",
		authority:          `display write write-shared write-simple`,
		wantAuthorityCount: 4,
	},
	{
		lib:                "(srfi 1)",
		sldPath:            "srfi/1.sld",
		authority:          srfi1Authority,
		wantAuthorityCount: 149,
	},
	{
		lib:                "(srfi 13)",
		sldPath:            "srfi/13.sld",
		authority:          srfi13Authority,
		wantAuthorityCount: 94,
		// string-trim-left is not in SRFI-13; Wile supplies it as an alias for
		// string-trim, which SRFI-13 defines as the left-trimming form.
		sanctionedExtras: []string{"string-trim-left"},
		sanctionedOmissions: []string{
			// SRFI-13 re-exports the R5RS string procedures; Wile leaves them to
			// their R7RS homes, (scheme base) and (scheme char), rather than
			// duplicating them into a second export set.
			"list->string", "make-string", "string", "string->list",
			"string-append", "string-copy", "string-copy!", "string-fill!",
			"string-for-each", "string-length", "string-ref", "string-set!",
			"string?",
			// Deliberate v1 scope cuts, recorded in
			// memory/2026-05-03-string-primitives-design.local.md §11.
			"string-append/shared", "string-concatenate-reverse",
			"string-concatenate-reverse/shared", "string-concatenate/shared",
			"string-hash-ci", "string-titlecase!", "string-unfold",
			"string-unfold-right", "string-xcopy!",
			// The reference implementation's internal parsing and KMP helpers.
			// SRFI-13 indexes them, but they exist to implement the library, not
			// to be called; Wile's equivalents are the %-prefixed helpers in
			// 13/util.scm and are deliberately not exported.
			"check-substring-spec", "kmp-step", "let-string-start+end",
			"make-kmp-restart-vector", "string-kmp-partial-search",
			"string-parse-final-start+end", "string-parse-start+end",
			"substring-spec-ok?",
		},
		why: "R5RS re-exports stay at their R7RS homes; the /shared and unfold " +
			"forms are recorded v1 scope cuts; the parse and KMP names are " +
			"reference-implementation internals",
	},
}

// docProseExtras are sanctioned extras documented in r7rs-differences.md as
// prose under the named heading rather than as a supersets-table row. Keyed
// "<lib> <id>". The heading's existence is asserted, so a rename of the section
// fails here instead of silently exempting the ids.
var docProseExtras = map[string]string{
	"(scheme base) finite?":   "Conflicting Imports Signalled (R7RS §5.6)",
	"(scheme base) infinite?": "Conflicting Imports Signalled (R7RS §5.6)",
	"(scheme base) nan?":      "Conflicting Imports Signalled (R7RS §5.6)",
}

// noAuthorityPrefixes are bundled library trees with no external authority to
// diff against. The reason is the point: a tree listed here is not "untested",
// it is "not specified elsewhere".
var noAuthorityPrefixes = map[string]string{
	"chibi/": "chibi-scheme extension libraries; no ratified specification to diff against",
	"rnrs/":  "R6RS-shaped subset supplied on purpose; the R6RS library list is not the target",
	"wile/":  "Wile-native libraries; Wile is the authority",
}

// noAuthorityFiles are individually excluded .sld files.
var noAuthorityFiles = map[string]string{
	"srfi/14.sld":  "SRFI-14 authority not yet transcribed; charset surface is tracked by test/scheme/srfi-14-test.scm",
	"srfi/132.sld": "SRFI-132 authority not yet transcribed; sort surface is tracked by test/scheme/srfi-132-test.scm",
}

// newStdlibEngine builds a KitchenSink engine with the embedded stdlib
// resolvable, the construction the other engine_stdlib tests use.
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

// TestLibraryExport_OnlyEnforcesMembership is the membership non-vacuity guard
// for everything below: (only <lib> id) must reject an id the library does not
// export. Without it, TestLibraryExportDiff_SanctionedExtrasResolve would be
// vacuous — an (only ...) that silently admitted unknown names would "prove"
// every sanctioned extra exists whether or not any of them did.
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
			"sanctioned-extra resolution assertions are vacuous"))
}

// TestLibraryExportDiff_PartitionsEveryBundledLibrary is the coverage
// non-vacuity guard. An empty diff produced because the generator found nothing
// is indistinguishable from a passing diff, so the library count is DERIVED from
// the embedded FS and every bundled .sld must fall into exactly one of three
// buckets: a diff-table row, a no-authority prefix, or a no-authority file.
// A newly bundled library therefore fails here until it is classified.
func TestLibraryExportDiff_PartitionsEveryBundledLibrary(t *testing.T) {
	discovered := bundledLibraryPaths(t)
	qt.Assert(t, len(discovered) > 0, qt.IsTrue,
		qt.Commentf("no .sld files found in the embedded stdlib; the walk is broken"))
	t.Logf("derived bundled .sld count: %d", len(discovered))

	tabled := make(map[string]bool, len(exportAuthorityRows))
	for _, row := range exportAuthorityRows {
		qt.Assert(t, tabled[row.sldPath], qt.IsFalse,
			qt.Commentf("duplicate diff-table row for %s", row.sldPath))
		tabled[row.sldPath] = true
	}

	prefixHits := make(map[string]int, len(noAuthorityPrefixes))
	fileHits := make(map[string]int, len(noAuthorityFiles))
	tableHits := 0

	for _, p := range discovered {
		matched := 0
		if tabled[p] {
			matched++
			tableHits++
		}
		_, isFile := noAuthorityFiles[p]
		if isFile {
			matched++
			fileHits[p]++
		}
		for prefix := range noAuthorityPrefixes {
			if strings.HasPrefix(p, prefix) {
				matched++
				prefixHits[prefix]++
			}
		}
		qt.Assert(t, matched, qt.Equals, 1,
			qt.Commentf("%s must match exactly one classification rule "+
				"(diff-table row, no-authority prefix, or no-authority file); "+
				"matched %d", p, matched))
	}

	qt.Assert(t, tableHits, qt.Equals, len(exportAuthorityRows),
		qt.Commentf("every diff-table row must name a bundled .sld that exists"))
	for prefix := range noAuthorityPrefixes {
		qt.Assert(t, prefixHits[prefix] > 0, qt.IsTrue,
			qt.Commentf("no-authority prefix %q matched nothing; it is stale", prefix))
	}
	for p := range noAuthorityFiles {
		qt.Assert(t, fileHits[p], qt.Equals, 1,
			qt.Commentf("no-authority file %q matched %d bundled libraries; it is stale",
				p, fileHits[p]))
	}
}

// TestLibraryExportDiff diffs each bundled library's (export ...) form against
// its authority in both directions. Both deltas must equal the sanctioned sets
// exactly: an unsanctioned extra is an undocumented public-API superset, an
// unsanctioned omission is a conformance hole, and a sanctioned entry that no
// longer appears in the diff is a stale sanction.
func TestLibraryExportDiff(t *testing.T) {
	docExtras := parseSupersetDocTable(t)

	for _, row := range exportAuthorityRows {
		t.Run(row.sldPath, func(t *testing.T) {
			authority := strings.Fields(row.authority)
			qt.Assert(t, len(authority), qt.Equals, row.wantAuthorityCount,
				qt.Commentf("authority transcription for %s changed length", row.lib))
			qt.Assert(t, len(uniqueSet(authority)), qt.Equals, row.wantAuthorityCount,
				qt.Commentf("authority transcription for %s repeats a name", row.lib))

			exported := sldExportNames(t, row.sldPath)
			qt.Assert(t, len(exported) > 0, qt.IsTrue,
				qt.Commentf("parsed no exports from %s; the .sld reader is broken",
					row.sldPath))

			missing := setDifference(authority, exported)
			extra := setDifference(exported, authority)

			qt.Check(t, extra, qt.DeepEquals, sortedCopy(row.sanctionedExtras),
				qt.Commentf("%s exports names its authority does not; each must be "+
					"documented in r7rs-differences.md and sanctioned here "+
					"(exports never get deleted — 2026-07-14 policy)", row.lib))
			qt.Check(t, missing, qt.DeepEquals, sortedCopy(row.sanctionedOmissions),
				qt.Commentf("%s omits names its authority requires; add them or "+
					"sanction them with a reason. %s", row.lib, row.why))

			for _, id := range row.sanctionedExtras {
				key := row.lib + " " + id
				_, prose := docProseExtras[key]
				qt.Check(t, docExtras[key] || prose, qt.IsTrue,
					qt.Commentf("sanctioned extra %s is not documented in "+
						"docs/reference/r7rs-differences.md; the superset policy "+
						"requires a doc row before a sanction", key))
			}
		})
	}
}

// TestLibraryExportDiff_SanctionedExtrasResolve proves each sanctioned extra is
// EFFECTIVE, not merely written into the .sld text: (only <lib> id) resolves
// strictly through <lib>'s own export set, so an import that compiles is proof
// the superset export exists. The preceding membership guard proves the
// rejection direction. This is the generated form of the hand-written allowlist
// this file replaces.
func TestLibraryExportDiff_SanctionedExtrasResolve(t *testing.T) {
	ctx := context.Background()

	for _, row := range exportAuthorityRows {
		for _, id := range row.sanctionedExtras {
			t.Run(row.sldPath+"/"+id, func(t *testing.T) {
				eng := newStdlibEngine(ctx, t)
				defer func() {
					_ = eng.Close()
				}()

				q, err := eng.EvalMultiple(ctx,
					"(import (only "+row.lib+" "+id+")) 42")
				qt.Assert(t, err, qt.IsNil,
					qt.Commentf("the superset export is a documented deviation "+
						"(r7rs-differences.md → Standard-Library Export Supersets)"))
				qt.Assert(t, q.SchemeString(), qt.Equals, "42")
			})
		}
	}
}

// TestLibraryExportDiff_DocTableHasNoStaleRows runs the doc cross-check in the
// other direction: a supersets-table row naming a library or identifier no
// sanction claims is a doc row outliving its deviation.
func TestLibraryExportDiff_DocTableHasNoStaleRows(t *testing.T) {
	docExtras := parseSupersetDocTable(t)
	qt.Assert(t, len(docExtras) > 0, qt.IsTrue,
		qt.Commentf("parsed no rows from the Standard-Library Export Supersets "+
			"table; the markdown reader is broken"))

	sanctioned := make(map[string]bool)
	for _, row := range exportAuthorityRows {
		for _, id := range row.sanctionedExtras {
			sanctioned[row.lib+" "+id] = true
		}
	}

	for key := range docExtras {
		qt.Check(t, sanctioned[key], qt.IsTrue,
			qt.Commentf("r7rs-differences.md documents %q as a superset export, "+
				"but no diff-table row sanctions it", key))
	}
}

// bundledLibraryPaths returns every .sld path in the embedded stdlib, sorted.
func bundledLibraryPaths(t *testing.T) []string {
	t.Helper()
	var q []string
	err := fs.WalkDir(stdlib.FS, ".",
		func(p string, d fs.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if d.IsDir() {
				return nil
			}
			if path.Ext(p) == ".sld" {
				q = append(q, p)
			}
			return nil
		})
	qt.Assert(t, err, qt.IsNil)
	sort.Strings(q)
	return q
}

// parseSupersetDocTable reads docs/reference/r7rs-differences.md and returns the
// "<lib> <id>" keys its Standard-Library Export Supersets table documents.
func parseSupersetDocTable(t *testing.T) map[string]bool {
	t.Helper()
	const docPath = "../../docs/reference/r7rs-differences.md"
	raw, err := os.ReadFile(docPath)
	qt.Assert(t, err, qt.IsNil)
	text := string(raw)

	for _, heading := range docProseExtras {
		qt.Assert(t, strings.Contains(text, "## "+heading), qt.IsTrue,
			qt.Commentf("r7rs-differences.md has no %q heading; a prose exemption "+
				"points at a section that no longer exists", heading))
	}

	const sectionHeading = "### Standard-Library Export Supersets"
	start := strings.Index(text, sectionHeading)
	qt.Assert(t, start >= 0, qt.IsTrue,
		qt.Commentf("r7rs-differences.md has no %q section", sectionHeading))

	q := make(map[string]bool)
	inTable := false
	for line := range strings.SplitSeq(text[start:], "\n") {
		trimmed := strings.TrimSpace(line)
		if !strings.HasPrefix(trimmed, "|") {
			if inTable {
				break
			}
			continue
		}
		inTable = true
		cells := strings.Split(strings.Trim(trimmed, "|"), "|")
		if len(cells) < 2 {
			continue
		}
		lib := strings.TrimSpace(cells[0])
		if !strings.HasPrefix(lib, "`") {
			continue // header or separator row
		}
		lib = strings.Trim(lib, "`")
		for id := range strings.SplitSeq(cells[1], ",") {
			id = strings.Trim(strings.TrimSpace(id), "`")
			if id == "" {
				continue
			}
			q[lib+" "+id] = true
		}
	}
	return q
}

// sldExportNames returns the identifiers in sldPath's (export ...) form, read
// from the embedded stdlib FS.
func sldExportNames(t *testing.T, sldPath string) []string {
	t.Helper()
	raw, err := fs.ReadFile(stdlib.FS, sldPath)
	qt.Assert(t, err, qt.IsNil)

	toks := scanUpToExportForm(t, sldPath, string(raw))
	open := -1
	for i := 0; i+1 < len(toks); i++ {
		if toks[i] == "(" && toks[i+1] == "export" {
			open = i
			break
		}
	}
	qt.Assert(t, open >= 0, qt.IsTrue,
		qt.Commentf("%s has no (export ...) form", sldPath))

	var q []string
	for _, tok := range toks[open+2:] {
		if tok == ")" {
			return q
		}
		qt.Assert(t, tok != "(", qt.IsTrue,
			qt.Commentf("%s nests a form inside (export ...); this reader only "+
				"understands bare identifiers and would mis-parse it", sldPath))
		q = append(q, tok)
	}
	t.Fatalf("%s: unterminated (export ...) form", sldPath)
	return nil
}

// scanUpToExportForm tokenizes src into parens and atoms, stopping once the
// (export ...) form has closed. It understands ; line comments, "..." strings
// with backslash escapes, #|...|# block comments and #\<char> literals, which
// is everything that appears before or inside a bundled library's export form.
func scanUpToExportForm(t *testing.T, sldPath, src string) []string {
	t.Helper()
	var q []string
	depth := 0
	exportDepth := -1
	runes := []rune(src)

	for i := 0; i < len(runes); i++ {
		c := runes[i]
		switch {
		case c == ';':
			for i < len(runes) && runes[i] != '\n' {
				i++
			}
		case c == '"':
			i++
			for i < len(runes) && runes[i] != '"' {
				if runes[i] == '\\' {
					i++
				}
				i++
			}
		case c == '#' && i+1 < len(runes) && runes[i+1] == '|':
			nesting := 1
			i += 2
			for i+1 < len(runes) && nesting > 0 {
				if runes[i] == '#' && runes[i+1] == '|' {
					nesting++
					i += 2
					continue
				}
				if runes[i] == '|' && runes[i+1] == '#' {
					nesting--
					i += 2
					continue
				}
				i++
			}
		case c == '#' && i+1 < len(runes) && runes[i+1] == '\\':
			i += 2
			for i+1 < len(runes) && !isSchemeDelimiter(runes[i+1]) {
				i++
			}
		case c == '(':
			q = append(q, "(")
			depth++
		case c == ')':
			q = append(q, ")")
			depth--
			if exportDepth >= 0 && depth == exportDepth {
				return q
			}
		case isSchemeDelimiter(c):
			// whitespace between atoms
		default:
			start := i
			for i < len(runes) && !isSchemeDelimiter(runes[i]) &&
				runes[i] != '(' && runes[i] != ')' && runes[i] != ';' {
				i++
			}
			atom := string(runes[start:i])
			i--
			q = append(q, atom)
			if atom == "export" && len(q) >= 2 && q[len(q)-2] == "(" {
				exportDepth = depth - 1
			}
		}
	}
	t.Fatalf("%s: reached end of file without closing the (export ...) form", sldPath)
	return nil
}

// isSchemeDelimiter reports whether c ends an atom.
func isSchemeDelimiter(c rune) bool {
	return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f'
}

// setDifference returns the sorted members of a that are absent from b.
func setDifference(a, b []string) []string {
	inB := uniqueSet(b)
	q := []string{}
	for name := range uniqueSet(a) {
		if !inB[name] {
			q = append(q, name)
		}
	}
	sort.Strings(q)
	return q
}

// uniqueSet returns the set of names in xs.
func uniqueSet(xs []string) map[string]bool {
	q := make(map[string]bool, len(xs))
	for _, x := range xs {
		q[x] = true
	}
	return q
}

// sortedCopy returns xs sorted, never nil, so DeepEquals against an empty diff
// does not trip on nil-vs-empty.
func sortedCopy(xs []string) []string {
	q := make([]string, len(xs))
	copy(q, xs)
	sort.Strings(q)
	return q
}
