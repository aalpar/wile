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

package core

import (
	"embed"
	"io/fs"

	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/werr"
)

// BootstrapFS embeds the two bootstrap sources. bootstrap.scm is retained on disk as
// documentation of load order (it (include)s both files) but is no longer registered
// as a source — the two files are registered directly through separate channels so
// they target different frames. The embedded FS is still passed to an EmbedFileResolver
// so any (include ...) directives resolve.
//
//go:embed bootstrap.scm bootstrap_macros.scm bootstrap_procedures.scm
var BootstrapFS embed.FS

// MutableVectorStringMapSource and ImmutableVectorStringMapSource are the two
// interchangeable bootstrap fragments defining vector-map and string-map — the one
// bootstrap procedure that depends on a mutation primitive. The default (Mutable)
// fills a fresh result in place with vector-set! / string-set! (fastest); the
// Immutable one builds it functionally (list->vector / list->string over map) and
// depends on no mutator. addBootstrapSources registers the Mutable one, so every
// engine is unchanged by default; a no-mutation dialect swaps it for the Immutable
// one (matching by value in the registry's procedure sources), which lets the
// mutation primitives be removed entirely. This is the sole seam where a dialect
// substitutes a mutation-dependent bootstrap procedure.
//
//go:embed bootstrap_maps_mutable.scm
var MutableVectorStringMapSource string

//go:embed bootstrap_maps_immutable.scm
var ImmutableVectorStringMapSource string

// LateBootstrapMacroSource holds the bootstrap macros whose templates reference
// bootstrap PROCEDURES (unless -> not, guard -> with-exception-handler). It loads
// AFTER bootstrap_procedures.scm so those free identifiers pin to the sealed base
// at macro-definition time (like case -> memv, a Go primitive) instead of degrading
// to a nil pin a use-site redefinition can capture (R7RS 4.3.2).
//
//go:embed bootstrap_macros_late.scm
var LateBootstrapMacroSource string

// addBootstrapSources registers bootstrap_macros.scm (define-syntax forms) as a macro
// source and bootstrap_procedures.scm (define forms) as a procedure source. The split
// is the phase boundary: macros load into the mutable expand frame, procedures into the
// immutable sealed base. Macros MUST load before procedures (procedures use let/and from
// the macros file) — the loader enforces order, this only registers the sources.
func addBootstrapSources(r *registry.Registry) error {
	macros, err := fs.ReadFile(BootstrapFS, "bootstrap_macros.scm")
	if err != nil {
		return werr.WrapForeignErrorWithCause(
			werr.ErrFileNotFound, err, "bootstrap: reading bootstrap_macros.scm",
		)
	}
	procs, err := fs.ReadFile(BootstrapFS, "bootstrap_procedures.scm")
	if err != nil {
		return werr.WrapForeignErrorWithCause(
			werr.ErrFileNotFound, err, "bootstrap: reading bootstrap_procedures.scm",
		)
	}
	r.AddMacroSource(string(macros))
	r.AddProcedureSource(string(procs))
	// The vector-map/string-map fragment loads as its own procedure source, AFTER
	// the main procedures (it uses map, defined there) and as a distinct slice entry
	// so a dialect can swap it by value. Default = the mutating fragment.
	r.AddProcedureSource(MutableVectorStringMapSource)
	return nil
}
