package repl

import (
	"sort"
	"strings"

	"github.com/aalpar/wile/environment"
)

// SchemeCompleter implements readline.AutoCompleter for the Wile REPL.
type SchemeCompleter struct {
	env          *environment.EnvironmentFrame
	metaCommands []string
}

// NewSchemeCompleter creates a completer that completes Scheme bindings
// from all phase environments and meta-command names.
func NewSchemeCompleter(
	env *environment.EnvironmentFrame,
	metaCommands []string,
) *SchemeCompleter {
	return &SchemeCompleter{
		env:          env,
		metaCommands: metaCommands,
	}
}

// Do implements readline.AutoCompleter.
func (p *SchemeCompleter) Do(line []rune, pos int) ([][]rune, int) {
	lineStr := string(line[:pos])

	// Context 1: after "," — complete meta-command names
	if strings.HasPrefix(lineStr, ",") {
		prefix := lineStr[1:]
		return p.completeFromList(prefix, p.metaCommands)
	}

	// Context 2: complete Scheme bindings
	prefix := p.extractSymbolPrefix(lineStr)
	if prefix == "" {
		return nil, 0
	}

	names := p.collectBindingNames()
	return p.completeFromList(prefix, names)
}

// extractSymbolPrefix finds the Scheme symbol being typed at the cursor.
// Walks backward from the end of the line until hitting a delimiter.
func (p *SchemeCompleter) extractSymbolPrefix(line string) string {
	delimiters := " \t\n\r()[]{}\"';,`"
	i := len(line) - 1
	for i >= 0 && !strings.ContainsRune(delimiters, rune(line[i])) {
		i--
	}
	return line[i+1:]
}

// collectBindingNames walks all phase environments and returns unique binding names.
func (p *SchemeCompleter) collectBindingNames() []string {
	if p.env == nil {
		return nil
	}

	topLevel := p.env.TopLevelEnv()
	if topLevel == nil {
		return nil
	}

	seen := make(map[string]bool)
	var names []string

	phases := topLevel.Phases()
	phaseIndices := phases.Phases()
	sort.Ints(phaseIndices)

	for _, phase := range phaseIndices {
		phaseEnv := phases.Get(phase)
		if phaseEnv == nil {
			continue
		}
		global := phaseEnv.GlobalEnvironment()
		if global == nil {
			continue
		}
		for sym := range global.Keys() {
			name := sym.Key
			if !seen[name] {
				seen[name] = true
				names = append(names, name)
			}
		}
	}

	sort.Strings(names)
	return names
}

// completeFromList returns completions matching the given prefix.
func (p *SchemeCompleter) completeFromList(prefix string, candidates []string) ([][]rune, int) {
	var matches [][]rune
	for _, name := range candidates {
		if strings.HasPrefix(name, prefix) {
			suffix := name[len(prefix):]
			matches = append(matches, []rune(suffix))
		}
	}
	return matches, len(prefix)
}

// BindingNames returns all binding names visible in the environment.
// Exposed for use by the REPL to provide hints.
func (p *SchemeCompleter) BindingNames() []string {
	return p.collectBindingNames()
}
