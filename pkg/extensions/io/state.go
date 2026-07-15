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

package io

import (
	"context"
	"os"
	"sync"

	"github.com/aalpar/wile/pkg/internal/tokenizer"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// State is the per-engine I/O port + cache state. One per Namespace, reached by
// primitives via stateFrom(cc). Replaces the former package globals so two
// Engines in one process do not share port parameters or caches (staff-sweep #8).
//
// The tokenizers/parsers caches are keyed by port value; keeping them in the
// same per-engine struct as the port parameters is what makes their isolation
// hold — two engines reading their default current-input-port now key on
// distinct default port objects, so their read positions no longer collide.
type State struct {
	inPort, outPort, errPort *machine.Parameter

	// mu protects the tokenizers/parsers cache maps — get-or-create and evict
	// only. It is never held across a read: a read runs under the per-port
	// entry lock (parserEntry.mu / tokenizerEntry.mu), so a blocking read on one
	// port neither serialises other ports nor freezes the engine's I/O.
	mu sync.RWMutex
	// tokenizers caches tokenizers per input port; parsers caches parsers per
	// input port. Entries are evicted via evictPortCache() on port close or EOF.
	// Ports abandoned without close or EOF retain their entries until the engine
	// is collected.
	tokenizers map[values.Value]*tokenizerEntry
	parsers    map[values.Value]*parserEntry
}

// parserEntry pairs a port's cached parser with a mutex that serialises reads
// on that port. A port's read position is a causal chain — two concurrent reads
// cannot both yield coherent data — so reads on one port serialise on entry.mu,
// held across the whole ReadSyntax call. Reads on DIFFERENT ports take different
// entry locks and never contend. The parser is constructed lazily under entry.mu
// (parser.NewParser reads the first rune eagerly, so construction is itself I/O
// that must stay off State.mu). Fixes the reviews/2026-07-13 §4 finding where the
// shared parser was driven outside any per-port lock.
type parserEntry struct {
	mu     sync.Mutex
	parser *parser.Parser
	mk     func() *parser.Parser // constructor; nil once parser is built
}

// tokenizerEntry is the read-token analogue of parserEntry.
type tokenizerEntry struct {
	mu        sync.Mutex
	tokenizer *tokenizer.Tokenizer
	mk        func() *tokenizer.Tokenizer // constructor; nil once built
}

// NewState builds a fresh per-engine State: three port parameters defaulting to
// the process std streams, and empty caches. Fully constructed here — there is
// no lazy initialization, unlike the former package-global InitState.
func NewState() *State {
	return &State{
		inPort:     machine.NewParameter(values.NewCharacterInputPortFromReader(os.Stdin), nil),
		outPort:    machine.NewParameter(values.NewCharacterOutputPortFromWriter(os.Stdout), nil),
		errPort:    machine.NewParameter(values.NewCharacterOutputPortFromWriter(os.Stderr), nil),
		tokenizers: map[values.Value]*tokenizerEntry{},
		parsers:    map[values.Value]*parserEntry{},
	}
}

// SetInputPort sets this engine's base input port. Used by tests and by
// embedders configuring an engine's ports before running. Per R7RS parameter
// semantics the value is not validated here; a non-textual port surfaces as a
// Scheme error when a default-port read resolves it (resolveCurrentInputPort).
func (p *State) SetInputPort(port *values.PortObject) {
	p.inPort.SetValue(port)
}

// SetOutputPort sets this engine's base output port. Not validated here (see
// SetInputPort); a non-textual port errors at resolve time.
func (p *State) SetOutputPort(port *values.PortObject) {
	p.outPort.SetValue(port)
}

// GetInputPort returns this engine's base input port, or a wrapped sentinel if
// the parameter does not hold a textual input port. For non-VM callers (tests,
// embedders) that read the base value without the panic-on-error contract of
// resolveCurrentInputPort (whose panic is caught by the VM's recover).
func (p *State) GetInputPort() (*values.PortObject, error) {
	return currentTextualInputPort("current-input-port", p.inPort.Value())
}

// GetOutputPort returns this engine's base output port, or a wrapped sentinel if
// the parameter does not hold a textual output port.
func (p *State) GetOutputPort() (*values.PortObject, error) {
	return currentTextualOutputPort("current-output-port", p.outPort.Value())
}

// stateFrom pulls the per-engine State off the CallContext's namespace, or
// returns a wrapped sentinel when the context has no namespace/State (e.g. a bare
// sub-context). The State lives on the engine's Namespace (root-delegated, like
// ImmutableLiterals); this reaches it via the already-typed CallContext surface
// rather than a dedicated interface method, keeping the opaque `any` slot off the
// public contract.
func stateFrom(cc machine.CallContext) (*State, error) {
	env := cc.EnvironmentFrame()
	if env == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrNoIOState,
			"io: context has no environment frame")
	}
	ns := env.Namespace()
	if ns == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrNoIOState,
			"io: context has no namespace")
	}
	st, ok := ns.IOState().(*State)
	if !ok || st == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrNoIOState,
			"io: no per-engine I/O state on this context")
	}
	return st, nil
}

// currentTextualInputPort asserts v is a *PortObject with rune-read
// capability. Wraps the result with a sentinel-bearing error
// otherwise, so callers can match via errors.Is.
func currentTextualInputPort(name string, v values.Value) (*values.PortObject, error) {
	p, ok := v.(*values.PortObject)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAnInputPort,
			"%s: value is %T, not a port", name, v)
	}
	_, hasRR := p.AsRuneReader()
	if !hasRR {
		return nil, werr.WrapForeignErrorf(werr.ErrNotATextualPort,
			"%s: port is not textual", name)
	}
	return p, nil
}

// currentTextualOutputPort asserts v is a *PortObject with rune-write
// capability.
func currentTextualOutputPort(name string, v values.Value) (*values.PortObject, error) {
	p, ok := v.(*values.PortObject)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAnOutputPort,
			"%s: value is %T, not a port", name, v)
	}
	_, hasRW := p.AsRuneWriter()
	if !hasRW {
		return nil, werr.WrapForeignErrorf(werr.ErrNotATextualPort,
			"%s: port is not textual", name)
	}
	return p, nil
}

// resolveCurrentOutputPort returns the effective current output port,
// checking continuation marks (from parameterize) before falling back
// to the base value. Panics with a wrapped error if the resolved value
// is not a textual output port — the panic is caught by
// OperationForeignFunctionCall's recover and converted to a Scheme
// exception.
func resolveCurrentOutputPort(cc machine.CallContext) *values.PortObject {
	mc, err := machine.RequireMachineContext(cc, "current-output-port")
	if err != nil {
		panic(err)
	}
	st, err := stateFrom(cc)
	if err != nil {
		panic(err)
	}
	v := mc.ResolveParameterValue(st.outPort)
	p, err := currentTextualOutputPort("current-output-port", v)
	if err != nil {
		panic(err)
	}
	return p
}

// resolveCurrentInputPort returns the effective current input port,
// checking continuation marks (from parameterize) before falling back
// to the base value. Panics with a wrapped error if the resolved value
// is not a textual input port — the panic is caught by
// OperationForeignFunctionCall's recover and converted to a Scheme
// exception.
func resolveCurrentInputPort(cc machine.CallContext) *values.PortObject {
	mc, err := machine.RequireMachineContext(cc, "current-input-port")
	if err != nil {
		panic(err)
	}
	st, err := stateFrom(cc)
	if err != nil {
		panic(err)
	}
	v := mc.ResolveParameterValue(st.inPort)
	p, err := currentTextualInputPort("current-input-port", v)
	if err != nil {
		panic(err)
	}
	return p
}

// readSyntaxCached returns the next datum from the port's cached parser. It
// get-or-creates the port's entry under State.mu (no I/O), then holds that
// entry's lock across the whole ReadSyntax call so two threads reading one port
// serialise on a single parser instead of racing its fields and datum-label map.
// The parser is built lazily under the entry lock (parser.NewParser reads a rune
// eagerly), so State.mu is never held across a read. First-creator's mk wins,
// matching the pre-fix "first read constructs the cached parser" behaviour.
func (p *State) readSyntaxCached(ctx context.Context, port values.Value, mk func() *parser.Parser) (syntax.SyntaxValue, error) {
	p.mu.Lock()
	entry, ok := p.parsers[port]
	if !ok || entry == nil {
		entry = &parserEntry{mk: mk}
		p.parsers[port] = entry
	}
	p.mu.Unlock()

	entry.mu.Lock()
	defer entry.mu.Unlock()
	if entry.parser == nil {
		entry.parser = entry.mk()
		entry.mk = nil
	}
	return entry.parser.ReadSyntax(ctx)
}

// nextTokenCached returns the next token from the port's cached tokenizer,
// serialising reads on one port under the entry lock. Same discipline as
// readSyntaxCached; the tokenizer is likewise built lazily under the entry lock.
func (p *State) nextTokenCached(port values.Value, mk func() *tokenizer.Tokenizer) (tokenizer.Token, error) {
	p.mu.Lock()
	entry, ok := p.tokenizers[port]
	if !ok || entry == nil {
		entry = &tokenizerEntry{mk: mk}
		p.tokenizers[port] = entry
	}
	p.mu.Unlock()

	entry.mu.Lock()
	defer entry.mu.Unlock()
	if entry.tokenizer == nil {
		entry.tokenizer = entry.mk()
		entry.mk = nil
	}
	return entry.tokenizer.Next()
}
