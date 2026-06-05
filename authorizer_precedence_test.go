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

	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/werr"

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
// tags that were denied. A nil authorizer denies nothing.
func deniedTags(auth security.Authorizer, tags ...string) []string {
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
		if err != nil {
			denied = append(denied, tag)
		}
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
			cfg := &engineConfig{}
			tc.mutate(cfg)
			got := cfg.resolveAuthorizer()
			if tc.wantNil {
				c.Assert(got, qt.IsNil)
				return
			}
			c.Assert(got, qt.IsNotNil)
			c.Assert(deniedTags(got, probes...), qt.DeepEquals, tc.wantDeniedTags)
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
		cfg := &engineConfig{}
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
		cfg := &engineConfig{}
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
