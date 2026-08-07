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

package bootstrap

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/werr"
)

// ErrProfileWidensEngine is returned when (environment '(wile <name>)) asks for
// a profile carrying primitives the calling engine never registered, and the
// engine's authorizer does not permit the widening.
var ErrProfileWidensEngine = werr.NewStaticError("profile widens the engine's capability surface")

// checkProfileWidening decides whether callerNS may construct a namespace for
// the named profile.
//
// The hazard this closes: NewProfileEnvironment builds the child through
// callerNS.NewChildNamespace(), which copies the authorizer, so every *gated*
// resource stays under the engine's policy. But an extension that defines no
// gate sites at all — threads, gointerop, namespace — is not gated by anything,
// so a Small or ConsoleWithLoad engine could reach them simply by asking for
// '(wile kitchen-sink). An authorizer cannot refuse what it is never asked
// about.
//
// The rule, in the order the three cases are decided:
//
//  1. No authorizer installed — allow. Widening via (environment '(wile ...))
//     is the documented way to get a richer namespace from Scheme, and an
//     embedder that installed no policy has not asked to be protected from it.
//     Tightening here would break every un-sandboxed embedder for no gain.
//  2. Authorizer installed, profile contained in the engine's own surface —
//     allow, without consulting the authorizer. A Console engine asking for
//     '(wile console) or '(wile tiny) acquires nothing it did not already have,
//     so there is no capability question to put to a policy. This is what keeps
//     the change compatible: today's sandboxed embedders are not newly denied.
//  3. Authorizer installed, profile NOT contained — ask, as namespace:create
//     with the profile name as target. Built-in authorizers deny unknown
//     resources by default, so Console/ConsoleWithLoad refuse; a custom
//     authorizer can opt in deliberately.
//
// Containment is over PRIMITIVE NAMES, not extension identities, and that is
// load-bearing rather than incidental. Console carries all.SafeExtension while
// kitchen-sink's allExtensions carries all.Extension and not the Safe one, so
// identity-inclusion reports Console ⊄ KitchenSink and the order is broken
// before it starts. Names are what a program can actually call, and the Safe
// variant's names really are a subset of the full one's.
func checkProfileWidening(callerNS *environment.Namespace, profileName string, exts []registry.Extension) error {
	auth := callerNS.Authorizer()
	if auth == nil {
		return nil
	}

	requested, err := extensionPrimitiveNames(exts)
	if err != nil {
		return werr.WrapForeignErrorf(err,
			"checkProfileWidening: cannot enumerate profile %q", profileName)
	}

	extra := namesNotIn(requested, enginePrimitiveNames(callerNS))
	if extra == "" {
		return nil
	}

	err = security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceNamespace,
		Action:   security.ActionCreate,
		Target:   profileName,
	})
	if err != nil {
		return werr.WrapForeignErrorWithCause(ErrProfileWidensEngine, err,
			"environment: profile %q adds primitives this engine does not have (e.g. %q)",
			profileName, extra)
	}
	return nil
}

// extensionPrimitiveNames returns the set of primitive names a set of
// extensions registers, by applying them to a scratch registry. Nothing is
// bound into an environment — Apply is never called — so this costs the
// registration walk only.
func extensionPrimitiveNames(exts []registry.Extension) (map[string]struct{}, error) {
	reg := registry.NewRegistry()
	for _, ext := range exts {
		err := ext.AddToRegistry(reg)
		if err != nil {
			return nil, werr.WrapForeignErrorf(err,
				"extensionPrimitiveNames: extension %q failed to register", ext.Name())
		}
	}
	return primitiveNameSet(reg), nil
}

// enginePrimitiveNames returns the primitive names the calling engine has
// registered, or nil when that cannot be determined.
//
// This reads the FULL registry rather than EffectiveRegistry: the effective one
// is the narrowed surface the visible top level was bound from, and an engine
// can import its way back to everything the full registry holds. The capability
// boundary is the full set; the narrowed set is only about what is pre-bound.
//
// A nil result means "unknown", and callers must treat that as not-contained —
// an engine whose surface cannot be established is one whose containment cannot
// be proven.
func enginePrimitiveNames(callerNS *environment.Namespace) map[string]struct{} {
	reg, ok := callerNS.Registry().(*registry.PrimitiveRegistry)
	if !ok || reg == nil {
		return nil
	}
	return primitiveNameSet(reg)
}

// primitiveNameSet projects a registry down to the set of names it registers.
func primitiveNameSet(reg *registry.PrimitiveRegistry) map[string]struct{} {
	q := make(map[string]struct{}, reg.PrimitiveCount())
	for _, p := range reg.Primitives() {
		q[p.Spec.Name] = struct{}{}
	}
	return q
}

// namesNotIn returns one name present in requested but absent from have, for
// the denial message, or "" when requested ⊆ have. A nil have is the unknown
// case and is never a superset of a non-empty request.
//
// One example rather than the whole difference: the set can run to hundreds of
// names, and the first witness is what makes the refusal legible. Iteration
// order over a Go map is randomized, so which witness appears varies between
// runs — the message names an instance, never an exhaustive list, and its
// wording ("e.g.") says so.
func namesNotIn(requested, have map[string]struct{}) string {
	for name := range requested {
		_, ok := have[name]
		if !ok {
			return name
		}
	}
	return ""
}
