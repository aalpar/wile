module github.com/aalpar/wile

go 1.25.0

// Build with go1.27.0+. This clears the go1.26.3 stdlib vulnerabilities that
// first motivated a pinned toolchain — GO-2026-5039 (net/textproto) and
// GO-2026-5037 (crypto/x509) — and it is also what the tooling needs:
// golang.org/x/tools processes source with the language version of the
// toolchain that BUILT the tool, so a tool built on 1.26 silently mis-reads a
// tree whose `go list` is 1.27 (wile-goast's go-ssa-build returned zero
// functions and no error).
//
// The language directive is kept as LOW as the dependencies allow, so embedders
// are not forced onto a newer language version than they need; the build
// toolchain above is pinned independently and does not propagate to them.
// It sits at 1.25.0 rather than 1.24.0 only because golang.org/x/text v0.39.0
// declares go 1.25.0, and v0.39.0 is the first release fixing GO-2026-5970 (an
// infinite loop on malformed input, reachable from string-upcase /
// string-downcase / string-foldcase). No 1.24-compatible release carries that
// fix, so the floor moved by one minor to take it. Do not raise it further
// without the same kind of reason.
toolchain go1.27.0

require (
	github.com/ergochat/readline v0.1.3 // cli
	github.com/frankban/quicktest v1.14.6 // test
	github.com/google/go-cmp v0.6.0 // test
	github.com/jessevdk/go-flags v1.6.1 // cli
	github.com/quasilyte/go-ruleguard/dsl v0.3.23 // tooling
	golang.org/x/text v0.39.0 // internal
)

require (
	github.com/kr/pretty v0.3.1 // indirect; test
	github.com/kr/text v0.2.0 // indirect; test
	github.com/rogpeppe/go-internal v1.9.0 // indirect; test
	golang.org/x/sys v0.41.0 // indirect; cli
)

require (
	github.com/bahlo/generic-list-go v0.2.0 // indirect
	github.com/buger/jsonparser v1.1.2 // indirect
	github.com/google/uuid v1.6.0 // indirect
	github.com/invopop/jsonschema v0.13.0 // indirect
	github.com/mailru/easyjson v0.7.7 // indirect
	github.com/mark3labs/mcp-go v0.45.0
	github.com/spf13/cast v1.7.1 // indirect
	github.com/wk8/go-ordered-map/v2 v2.1.8 // indirect
	github.com/yosida95/uritemplate/v3 v3.0.2 // indirect
	gopkg.in/yaml.v3 v3.0.1 // indirect
)
