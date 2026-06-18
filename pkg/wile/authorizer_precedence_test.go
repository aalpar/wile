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

package wile

import (
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// denyTarget returns an authorizer that denies exactly one request Target and
// allows everything else, so a probe set can fingerprint which authorizers are
// present in a resolved chain.
func denyTarget(tag string) security.Authorizer {
	return security.AuthorizerFunc(func(req security.AccessRequest) error {
		if req.Target == tag {
			return werr.WrapForeignErrorf(security.ErrAccessDenied, "denyTarget: %s", tag)
		}
		return nil
	})
}

// deniedTags probes auth with one file:read request per tag and returns the
// tags that were denied. Only an ErrAccessDenied counts as a denial; any other
// error fails the test so an unexpected failure cannot masquerade as a denial.
// A nil authorizer denies nothing.
func deniedTags(c *qt.C, auth security.Authorizer, tags ...string) []string {
	if auth == nil {
		return nil
	}
	var denied []string
	for _, tag := range tags {
		err := auth.Authorize(security.AccessRequest{
			Resource: security.ResourceFile,
			Action:   security.ActionRead,
			Target:   tag,
		})
		if err == nil {
			continue
		}
		c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue,
			qt.Commentf("tag %q: unexpected non-deny error: %v", tag, err))
		denied = append(denied, tag)
	}
	return denied
}

// TestResolveAuthorizer pins the precedence rule for the three authorizer
// config fields: an explicit WithAuthorizer (even nil) overrides a profile's
// built-in authorizer, and a WithSandbox layer is always intersected on top.
// The resolution is a pure function of the field set, so it is inherently
// order-independent at the option level (each option writes a distinct field).
func TestResolveAuthorizer(t *testing.T) {
	c := qt.New(t)

	profile := denyTarget("P")
	explicit := denyTarget("E")
	sandbox := denyTarget("S")
	probes := []string{"P", "E", "S"}

	cases := []struct {
		name           string
		mutate         func(*engineConfig)
		wantNil        bool
		wantDeniedTags []string
	}{
		{"empty", func(*engineConfig) {}, true, nil},
		{"profile only", func(cfg *engineConfig) {
			cfg.profileAuthorizer = profile
		}, false, []string{"P"}},
		{"explicit only", func(cfg *engineConfig) {
			cfg.explicitAuthorizer = explicit
			cfg.explicitAuthorizerSet = true
		}, false, []string{"E"}},
		{"explicit overrides profile", func(cfg *engineConfig) {
			cfg.profileAuthorizer = profile
			cfg.explicitAuthorizer = explicit
			cfg.explicitAuthorizerSet = true
		}, false, []string{"E"}},
		{"explicit nil opens, overriding profile", func(cfg *engineConfig) {
			cfg.profileAuthorizer = profile
			cfg.explicitAuthorizer = nil
			cfg.explicitAuthorizerSet = true
		}, true, nil},
		{"sandbox composes with profile", func(cfg *engineConfig) {
			cfg.profileAuthorizer = profile
			cfg.sandboxAuthorizer = sandbox
		}, false, []string{"P", "S"}},
		{"sandbox composes with explicit (profile suppressed)", func(cfg *engineConfig) {
			cfg.profileAuthorizer = profile
			cfg.explicitAuthorizer = explicit
			cfg.explicitAuthorizerSet = true
			cfg.sandboxAuthorizer = sandbox
		}, false, []string{"E", "S"}},
		{"sandbox only", func(cfg *engineConfig) {
			cfg.sandboxAuthorizer = sandbox
		}, false, []string{"S"}},
	}

	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			cfg := newEngineConfig()
			tc.mutate(cfg)
			got := cfg.resolveAuthorizer()
			if tc.wantNil {
				c.Assert(got, qt.IsNil)
				return
			}
			c.Assert(got, qt.IsNotNil)
			c.Assert(deniedTags(c, got, probes...), qt.DeepEquals, tc.wantDeniedTags)
		})
	}
}

// TestAuthorizerOptions_ExplicitBeatsProfile_OrderIndependent is the regression
// guard for the order-dependent clobber bug: before resolveAuthorizer, calling
// WithAuthorizer before WithProfile let the profile's built-in authorizer
// silently overwrite the explicit one. Both orders must now yield an engine
// where the explicit authorizer wins.
func TestAuthorizerOptions_ExplicitBeatsProfile_OrderIndependent(t *testing.T) {
	c := qt.New(t)

	codeEval := security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionEval,
		Target:   "(noop)",
	}

	build := func(opts ...EngineOption) security.Authorizer {
		cfg := newEngineConfig()
		for _, opt := range opts {
			opt(cfg)
		}
		return cfg.resolveAuthorizer()
	}

	// Baseline: the Console profile's authorizer denies code:eval. This is the
	// behavior an explicit allow-all authorizer must be able to override.
	consoleOnly := build(WithProfile(Console))
	c.Assert(consoleOnly, qt.IsNotNil)
	c.Assert(errors.Is(consoleOnly.Authorize(codeEval), security.ErrAccessDenied), qt.IsTrue,
		qt.Commentf("Console authorizer is expected to deny code:eval"))

	allowAll := security.AuthorizerFunc(func(security.AccessRequest) error {
		return nil
	})

	orders := []struct {
		name string
		opts []EngineOption
	}{
		{"profile-then-authorizer", []EngineOption{WithProfile(Console), WithAuthorizer(allowAll)}},
		{"authorizer-then-profile", []EngineOption{WithAuthorizer(allowAll), WithProfile(Console)}},
	}
	for _, o := range orders {
		got := build(o.opts...)
		c.Assert(got, qt.IsNotNil, qt.Commentf(o.name))
		c.Assert(got.Authorize(codeEval), qt.IsNil,
			qt.Commentf("%s: explicit authorizer must override the profile regardless of order", o.name))
	}
}

// TestAuthorizerOptions_NilAndMultiProfile pins two option-level contracts the
// field-level matrix cannot reach: WithAuthorizer(nil) must open the engine even
// over a profile that defines an authorizer (the option still sets
// explicitAuthorizerSet), and a later profile with no authorizer must not clear
// an earlier profile's authorizer.
func TestAuthorizerOptions_NilAndMultiProfile(t *testing.T) {
	c := qt.New(t)

	codeEval := security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionEval,
		Target:   "(noop)",
	}

	build := func(opts ...EngineOption) security.Authorizer {
		cfg := newEngineConfig()
		for _, opt := range opts {
			opt(cfg)
		}
		return cfg.resolveAuthorizer()
	}

	// WithAuthorizer(nil) opens the engine, overriding the profile, in either
	// order — the nil is meaningful (symmetric with WithEnvMap(nil)).
	c.Assert(build(WithProfile(Console), WithAuthorizer(nil)), qt.IsNil)
	c.Assert(build(WithAuthorizer(nil), WithProfile(Console)), qt.IsNil)

	// A later authorizer-less profile (Tiny) must not clear Console's
	// authorizer — the prior restriction is retained (fails safe).
	got := build(WithProfile(Console), WithProfile(Tiny))
	c.Assert(got, qt.IsNotNil)
	c.Assert(errors.Is(got.Authorize(codeEval), security.ErrAccessDenied), qt.IsTrue,
		qt.Commentf("Console authorizer must survive a later authorizer-less profile"))
}

// TestSandboxOption_MultipleCallsAccumulate guards against the regression where
// a second WithSandbox would overwrite the first instead of composing with it,
// silently widening access. Two sandboxes with disjoint env prefixes must
// intersect: a var permitted by one layer is still denied by the other.
func TestSandboxOption_MultipleCallsAccumulate(t *testing.T) {
	c := qt.New(t)

	build := func(opts ...EngineOption) security.Authorizer {
		cfg := newEngineConfig()
		for _, opt := range opts {
			opt(cfg)
		}
		return cfg.resolveAuthorizer()
	}
	envRead := func(target string) security.AccessRequest {
		return security.AccessRequest{Resource: security.ResourceEnv, Action: security.ActionRead, Target: target}
	}

	got := build(WithSandbox(SandboxEnvPrefix("A_")), WithSandbox(SandboxEnvPrefix("B_")))
	c.Assert(got, qt.IsNotNil)
	// Last-wins (the pre-fix regression) would leave only the B_ restriction,
	// allowing B_X. Accumulation denies it via the surviving A_ layer.
	c.Assert(errors.Is(got.Authorize(envRead("B_X")), security.ErrAccessDenied), qt.IsTrue,
		qt.Commentf("second sandbox must not widen the first: B_X must stay denied by the A_ layer"))
	c.Assert(errors.Is(got.Authorize(envRead("A_X")), security.ErrAccessDenied), qt.IsTrue,
		qt.Commentf("A_X must be denied by the B_ layer"))
}

// TestSandboxOption_OrderIndependent verifies WithSandbox intersects its
// restriction onto a profile no matter whether it appears before or after
// WithProfile — the ordering caveat the old prose comment warned about.
func TestSandboxOption_OrderIndependent(t *testing.T) {
	c := qt.New(t)

	// A profile authorizer denies code:eval; the sandbox additionally denies
	// file writes. Both restrictions must hold in either option order.
	codeEval := security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionEval, Target: "(noop)"}
	fileWrite := security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionWrite, Target: "/tmp/x"}

	build := func(opts ...EngineOption) security.Authorizer {
		cfg := newEngineConfig()
		for _, opt := range opts {
			opt(cfg)
		}
		return cfg.resolveAuthorizer()
	}

	orders := []struct {
		name string
		opts []EngineOption
	}{
		{"profile-then-sandbox", []EngineOption{WithProfile(Console), WithSandbox()}},
		{"sandbox-then-profile", []EngineOption{WithSandbox(), WithProfile(Console)}},
	}
	for _, o := range orders {
		got := build(o.opts...)
		c.Assert(got, qt.IsNotNil, qt.Commentf(o.name))
		c.Assert(errors.Is(got.Authorize(codeEval), security.ErrAccessDenied), qt.IsTrue,
			qt.Commentf("%s: profile restriction (code:eval) must survive sandbox composition", o.name))
		c.Assert(errors.Is(got.Authorize(fileWrite), security.ErrAccessDenied), qt.IsTrue,
			qt.Commentf("%s: sandbox restriction (file:write) must apply regardless of order", o.name))
	}
}
