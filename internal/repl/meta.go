//nolint:errcheck // Meta-command output doesn't need error handling
package repl

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/exec"
	"slices"
	"sort"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/machine/compilation"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// MetaCommandHandler dispatches comma-prefixed meta-commands.
// Session commands (help, doc, edit) are handled directly;
// debug commands are delegated to DebugContext.
type MetaCommandHandler struct {
	env      *environment.EnvironmentFrame
	debugCtx *DebugContext
	docProv  DocProvider
	pager    string
}

// NewMetaCommandHandler creates a new meta-command handler.
func NewMetaCommandHandler(
	env *environment.EnvironmentFrame,
	debugCtx *DebugContext,
	docProv DocProvider,
) *MetaCommandHandler {
	return &MetaCommandHandler{
		env:      env,
		debugCtx: debugCtx,
		docProv:  docProv,
		pager:    os.Getenv("PAGER"),
	}
}

// SetPager overrides the pager command used for long output.
// Pass "" to disable paging (e.g., for non-TTY contexts like MCP).
func (p *MetaCommandHandler) SetPager(pager string) {
	p.pager = pager
}

// Handle processes a line starting with ",". Returns true if the line was
// a meta-command (even if unrecognized), false if it's not a meta-command.
func (p *MetaCommandHandler) Handle(line string, out io.Writer) bool {
	line = strings.TrimSpace(line)
	if !strings.HasPrefix(line, ",") {
		return false
	}

	parts := strings.Fields(strings.TrimPrefix(line, ","))
	if len(parts) == 0 {
		return true
	}

	cmd := parts[0]
	args := parts[1:]

	switch cmd {
	// Session commands
	case "help", "h", "?":
		p.cmdHelp(args, out)
	case "doc":
		p.cmdDoc(args, out)
	case "edit":
		p.cmdEdit(args, out)
	case "apropos", "a":
		p.cmdApropos(args, out)
	case "topics":
		p.cmdTopics(out)
	case "topic":
		p.cmdTopic(args, out)
	case "libraries", "libs":
		p.cmdLibraries(out)
	case "disassemble", "dis":
		p.cmdDisassemble(args, out)
	default:
		// Delegate to debug context
		if p.debugCtx != nil && p.debugCtx.HandleDebugCommand(line, out) {
			return true
		}
		fmt.Fprintf(out, "Unknown command: %s (type ,help for commands)\n", cmd)
	}

	return true
}

// Commands returns all meta-command names and aliases (session + debug) for autocomplete.
func (p *MetaCommandHandler) Commands() []string {
	var names []string
	for _, cmd := range metaCommands {
		if cmd.category == "session" {
			names = append(names, cmd.name)
			names = append(names, cmd.aliases...)
		}
	}
	if p.debugCtx != nil {
		for _, dc := range p.debugCtx.DebugCommands() {
			names = append(names, dc.Name)
			names = append(names, dc.Aliases...)
		}
	}
	return names
}

type commandInfo struct {
	name     string
	aliases  []string
	summary  string
	detail   string
	category string // "session" or "debug"
}

// metaCommands defines metadata for all commands (session + debug).
// Session commands are declared here; debug commands are appended in init()
// from debugCommandMetadata, the single source of truth.
var metaCommands = []commandInfo{
	{"help", []string{"h", "?"}, "Show this help or help for a specific command",
		"Usage: ,help [command]\n\nWith no arguments, lists all commands.\nWith a command name, shows detailed help for that command.",
		"session"},
	{"doc", nil, "Show documentation for a Scheme binding or library",
		"Usage: ,doc [-x] <name> or ,doc (<library-name>)\n\n" +
			"Looks up the named binding across all phase environments\n" +
			"(runtime, expand, compile) and displays documentation.\n" +
			"For primitives, shows signature, description, and category.\n" +
			"For user bindings, shows type and current value.\n" +
			"For libraries, shows description, source, and export list.\n\n" +
			"Options:\n  -x    Include usage examples in the output",
		"session"},
	{"edit", nil, "Open file in $EDITOR",
		"Usage: ,edit <file>\n\nOpens the given file in the editor specified by the $EDITOR\nenvironment variable. The REPL blocks until the editor exits.",
		"session"},
	{"apropos", []string{"a"}, "Search bindings by name, doc, or category",
		"Usage: ,apropos <pattern>\n\nSearches all bindings for case-insensitive substring matches\nagainst names, documentation, and categories.\nResults show name, category, and one-line description.",
		"session"},
	{"topics", nil, "List documentation categories",
		"Usage: ,topics\n\nShows all available documentation categories with entry counts.",
		"session"},
	{"topic", nil, "List bindings in a documentation category",
		"Usage: ,topic <category>\n\nLists all bindings in the named category.\nUse ,topics to see available categories.",
		"session"},
	{"libraries", []string{"libs"}, "List loaded Scheme libraries",
		"Usage: ,libraries\n\nLists all Scheme libraries currently loaded in the environment,\nsorted alphabetically, with their descriptions.",
		"session"},
	{"disassemble", []string{"dis"}, "Show bytecode disassembly of a procedure",
		"Usage: ,disassemble <name> or ,dis <name>\n\n" +
			"Looks up the named binding and displays its bytecode disassembly.\n" +
			"For native closures, shows the instruction listing with annotations.\n" +
			"For case-lambda, shows each clause separately.\n" +
			"For foreign closures, shows name, arity, and documentation.\n\n" +
			"For ad-hoc expressions, use (disassemble expr) at the REPL instead.",
		"session"},
}

func init() {
	// Derive debug command entries from the canonical metadata table.
	for _, dc := range debugCommandMetadata {
		metaCommands = append(metaCommands, commandInfo{
			name:     dc.Name,
			aliases:  dc.Aliases,
			summary:  dc.Summary,
			detail:   dc.Detail,
			category: "debug",
		})
	}
}

func (p *MetaCommandHandler) cmdHelp(args []string, out io.Writer) {
	if len(args) > 0 {
		p.cmdHelpSpecific(args[0], out)
		return
	}

	var content strings.Builder
	// Group by category
	for _, category := range []string{"session", "debug"} {
		switch category {
		case "session":
			fmt.Fprintln(&content, "Session commands:")
		case "debug":
			fmt.Fprintln(&content, "\nDebug commands:")
		}
		for _, cmd := range metaCommands {
			if cmd.category != category {
				continue
			}
			aliases := ""
			if len(cmd.aliases) > 0 {
				aliases = " (," + strings.Join(cmd.aliases, ", ,") + ")"
			}
			fmt.Fprintf(&content, "  ,%-12s %s%s\n", cmd.name, cmd.summary, aliases)
		}
	}

	writeWithPager(out, content.String(), p.pager)
}

func (p *MetaCommandHandler) cmdHelpSpecific(name string, out io.Writer) {
	for _, cmd := range metaCommands {
		if cmd.name == name || containsString(cmd.aliases, name) {
			var content strings.Builder
			fmt.Fprintf(&content, ",%s — %s\n\n%s\n", cmd.name, cmd.summary, cmd.detail)
			writeWithPager(out, content.String(), p.pager)
			return
		}
	}
	fmt.Fprintf(out, "Unknown command: %s (type ,help for commands)\n", name)
}

func containsString(ss []string, s string) bool {
	return slices.Contains(ss, s)
}

func (p *MetaCommandHandler) cmdDoc(args []string, out io.Writer) {
	if len(args) == 0 {
		fmt.Fprintln(out, "Usage: ,doc [-x] <name> or ,doc (<library-name>)")
		return
	}

	showExamples := false
	if args[0] == "-x" {
		showExamples = true
		args = args[1:]
		if len(args) == 0 {
			fmt.Fprintln(out, "Usage: ,doc [-x] <name> or ,doc (<library-name>)")
			return
		}
	}

	// Check if arguments form a library name like (scheme base)
	if strings.HasPrefix(args[0], "(") {
		joined := strings.Join(args, " ")
		if strings.HasSuffix(joined, ")") {
			p.cmdDocLibrary(joined, out)
			return
		}
	}

	name := args[0]
	var content strings.Builder

	// Walk phase environments first — the binding's value is the source of truth
	if p.env != nil {
		topLevel := p.env.Namespace()
		if topLevel != nil {
			phases := topLevel.Phases()
			phaseIndices := phases.Phases()
			sort.Ints(phaseIndices)

			sym := values.NewSymbol(name)
			for _, phase := range phaseIndices {
				phaseEnv := phases.Get(phase)
				if phaseEnv == nil {
					continue
				}
				bnd := phaseEnv.GetBinding(sym, nil)
				if bnd != nil {
					// For foreign closures, prefer DocProvider's rich format
					// (signature, types, category)
					if bnd.BindingType() == environment.BindingTypeVariable {
						_, isForeign := bnd.Value().(*machine.ForeignClosure)
						if isForeign && p.docProv != nil {
							info, found := p.docProv.LookupDoc(name)
							if found {
								formatPrimitiveDoc(&content, name, info, showExamples)
								writeWithPager(out, content.String(), p.pager)
								return
							}
						}
					}
					formatBindingDoc(&content, name, bnd, phase, showExamples)
					writeWithPager(out, content.String(), p.pager)
					return
				}
			}
		}
	}

	// Fallback: DocProvider for names not found in any phase environment
	if p.docProv != nil {
		info, found := p.docProv.LookupDoc(name)
		if found {
			formatPrimitiveDoc(&content, name, info, showExamples)
			writeWithPager(out, content.String(), p.pager)
			return
		}
	}

	fmt.Fprintf(out, "Unbound identifier: %s\n", name)
}

func (p *MetaCommandHandler) cmdDocLibrary(nameStr string, out io.Writer) {
	// Strip parens: "(scheme base)" -> "scheme base"
	inner := strings.TrimPrefix(strings.TrimSuffix(nameStr, ")"), "(")
	parts := strings.Fields(inner)
	if len(parts) == 0 {
		fmt.Fprintln(out, "Usage: ,doc (library-name)")
		return
	}

	libName := compilation.NewLibraryName(parts...)

	if p.env == nil {
		fmt.Fprintf(out, "Library %s: no environment available\n", libName.SchemeString())
		return
	}

	regAny := p.env.LibraryRegistry()
	if regAny == nil {
		fmt.Fprintf(out, "Library %s: no library registry configured\n", libName.SchemeString())
		return
	}
	reg, ok := regAny.(*compilation.LibraryRegistry)
	if !ok {
		fmt.Fprintf(out, "Library %s: library registry unavailable\n", libName.SchemeString())
		return
	}

	lib := reg.Lookup(libName)
	if lib == nil {
		fmt.Fprintf(out, "Library %s: not loaded\n", libName.SchemeString())
		return
	}

	var content strings.Builder
	formatLibraryDoc(&content, lib)
	writeWithPager(out, content.String(), p.pager)
}

func formatLibraryDoc(w *strings.Builder, lib *compilation.CompiledLibrary) {
	fmt.Fprintf(w, "Library: %s\n", lib.Name.SchemeString())
	if lib.Description != "" {
		fmt.Fprintf(w, "\n  %s\n", lib.Description)
	}
	if lib.SourceFile != "" {
		fmt.Fprintf(w, "\nSource: %s\n", lib.SourceFile)
	}

	exports := make([]string, 0, len(lib.Exports))
	for name := range lib.Exports {
		exports = append(exports, name)
	}
	sort.Strings(exports)

	fmt.Fprintf(w, "\nExports (%d):\n", len(exports))
	for _, name := range exports {
		fmt.Fprintf(w, "  %s\n", name)
	}
}

func formatPrimitiveDoc(w *strings.Builder, name string, info DocInfo, showExamples bool) {
	hasTypes := len(info.ParamTypes) > 0

	// Signature line
	fmt.Fprintf(w, "(%s", name)
	for _, pn := range info.ParamNames {
		fmt.Fprintf(w, " %s", pn)
	}
	if info.IsVariadic {
		fmt.Fprint(w, " ...")
	}
	fmt.Fprint(w, ")")
	if hasTypes && info.ReturnType != values.TypeAny {
		fmt.Fprintf(w, " → %s", info.ReturnType.String())
	}
	fmt.Fprintln(w)

	// Description
	doc := info.Doc
	if !showExamples {
		doc = StripExamples(doc)
	}
	if doc != "" {
		fmt.Fprintf(w, "  %s\n", doc)
	}

	// Parameter types
	if hasTypes && len(info.ParamNames) > 0 {
		fmt.Fprintln(w, "  Parameters:")
		for i, pn := range info.ParamNames {
			vt := paramTypeForDoc(info.ParamTypes, i)
			fmt.Fprintf(w, "    %s : %s\n", pn, vt.String())
		}
	}

	// Return type
	if hasTypes && info.ReturnType != values.TypeAny {
		fmt.Fprintf(w, "  Returns: %s\n", info.ReturnType.String())
	}

	// Category
	if info.Category != "" {
		fmt.Fprintf(w, "  Category: %s\n", info.Category)
	}
}

// paramTypeForDoc returns the ValueType for parameter position i.
// For variadic primitives, positions beyond len(types)-1 use the last entry.
func paramTypeForDoc(types []values.ValueType, i int) values.ValueType {
	if i < len(types) {
		return types[i]
	}
	if len(types) > 0 {
		return types[len(types)-1]
	}
	return values.TypeAny
}

func formatBindingDoc(w *strings.Builder, name string, bnd *environment.Binding, phase int, showExamples bool) {
	phaseName := phaseLabel(phase)

	switch bnd.BindingType() {
	case environment.BindingTypePrimitive:
		fmt.Fprintf(w, "%s: special form (%s)\n", name, phaseName)
	case environment.BindingTypeSyntax:
		fmt.Fprintf(w, "%s: syntax transformer (%s)\n", name, phaseName)
	case environment.BindingTypeVariable:
		val := bnd.Value()
		fmt.Fprintf(w, "%s: %s (%s)\n", name, val.SchemeString(), phaseName)
	default:
		fmt.Fprintf(w, "%s: bound in %s\n", name, phaseName)
	}

	// Try closure docstring first (same logic as procedure-documentation),
	// then fall back to binding-level doc (special forms, macros).
	doc := ""
	if bnd.BindingType() == environment.BindingTypeVariable {
		doc = callableDoc(bnd.Value())
	}
	if doc == "" {
		doc = bnd.Doc()
	}
	if doc != "" {
		if !showExamples {
			doc = StripExamples(doc)
		}
		indented := strings.ReplaceAll(doc, "\n", "\n  ")
		fmt.Fprintf(w, "\n  %s\n", indented)
	}
}

// callableDoc extracts the docstring from a callable value.
// Uses the same logic as (procedure-documentation proc).
func callableDoc(v values.Value) string {
	switch c := v.(type) {
	case *machine.MachineClosure:
		return c.Template().Doc()
	case *machine.ForeignClosure:
		return c.Doc()
	case *machine.CaseLambdaClosure:
		clauses := c.Clauses()
		if len(clauses) > 0 {
			return clauses[0].Template().Doc()
		}
	}
	return ""
}

func phaseLabel(phase int) string {
	switch phase {
	case 0:
		return "runtime"
	case 1:
		return "expand"
	case 2:
		return "compile"
	default:
		return fmt.Sprintf("phase %d", phase)
	}
}

func (p *MetaCommandHandler) cmdEdit(args []string, out io.Writer) {
	if len(args) == 0 {
		fmt.Fprintln(out, "Usage: ,edit <file>")
		return
	}

	editor := strings.TrimSpace(os.Getenv("EDITOR"))
	if editor == "" {
		fmt.Fprintln(out, "Error: $EDITOR is not set")
		return
	}

	parts := strings.Fields(editor)
	cmdArgs := append(parts[1:], args[0])
	cmd := exec.CommandContext(context.Background(), parts[0], cmdArgs...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	err := cmd.Run()
	if err != nil {
		fmt.Fprintf(out, "Editor exited with error: %v\n", err)
	}
}

func (p *MetaCommandHandler) cmdApropos(args []string, out io.Writer) {
	if len(args) == 0 {
		fmt.Fprintln(out, "Usage: ,apropos <pattern>")
		return
	}

	pattern := strings.Join(args, " ")
	searchProv, ok := p.docProv.(DocSearchProvider)
	if !ok {
		fmt.Fprintln(out, "Search not available")
		return
	}

	results := searchProv.Search(pattern)

	// Also search phase environment bindings and loaded libraries
	if p.env != nil {
		envResults := p.searchBindings(pattern)
		results = mergeSearchResults(results, envResults)
		libResults := p.searchLibraries(pattern)
		results = mergeSearchResults(results, libResults)
	}

	if len(results) == 0 {
		fmt.Fprintf(out, "No matches for %q\n", pattern)
		return
	}

	var content strings.Builder
	maxName := 0
	for _, r := range results {
		if len(r.Name) > maxName {
			maxName = len(r.Name)
		}
	}
	for _, r := range results {
		cat := ""
		if r.Category != "" {
			cat = fmt.Sprintf("[%s]", r.Category)
		}
		doc := firstLine(r.Doc)
		fmt.Fprintf(&content, "  %-*s  %-14s %s\n", maxName, r.Name, cat, doc)
	}
	writeWithPager(out, content.String(), p.pager)
}

func (p *MetaCommandHandler) cmdTopics(out io.Writer) {
	searchProv, ok := p.docProv.(DocSearchProvider)
	if !ok {
		fmt.Fprintln(out, "Topics not available")
		return
	}

	cats := searchProv.Categories()
	if len(cats) == 0 {
		fmt.Fprintln(out, "No categories found")
		return
	}

	var content strings.Builder
	fmt.Fprintln(&content, "Categories:")
	for _, cat := range cats {
		count := len(searchProv.ByCategory(cat))
		fmt.Fprintf(&content, "  %-18s (%d)\n", cat, count)
	}
	writeWithPager(out, content.String(), p.pager)
}

func (p *MetaCommandHandler) cmdTopic(args []string, out io.Writer) {
	if len(args) == 0 {
		fmt.Fprintln(out, "Usage: ,topic <category>")
		return
	}

	category := args[0]
	searchProv, ok := p.docProv.(DocSearchProvider)
	if !ok {
		fmt.Fprintln(out, "Topics not available")
		return
	}

	results := searchProv.ByCategory(category)
	if len(results) == 0 {
		fmt.Fprintf(out, "No category %q (use ,topics to list categories)\n", category)
		return
	}

	var content strings.Builder
	fmt.Fprintf(&content, "%s (%d procedures):\n", category, len(results))
	maxName := 0
	for _, r := range results {
		if len(r.Name) > maxName {
			maxName = len(r.Name)
		}
	}
	for _, r := range results {
		doc := firstLine(r.Doc)
		fmt.Fprintf(&content, "  %-*s  %s\n", maxName, r.Name, doc)
	}
	writeWithPager(out, content.String(), p.pager)
}

func (p *MetaCommandHandler) cmdLibraries(out io.Writer) {
	if p.env == nil {
		fmt.Fprintln(out, "No environment available")
		return
	}
	regAny := p.env.LibraryRegistry()
	if regAny == nil {
		fmt.Fprintln(out, "No library registry configured")
		return
	}
	reg, ok := regAny.(*compilation.LibraryRegistry)
	if !ok {
		fmt.Fprintln(out, "Library registry unavailable")
		return
	}

	libs := reg.All()
	if len(libs) == 0 {
		fmt.Fprintln(out, "No libraries loaded")
		return
	}

	var content strings.Builder
	fmt.Fprintf(&content, "Loaded libraries (%d):\n", len(libs))
	maxName := 0
	for _, lib := range libs {
		n := len(lib.Name.SchemeString())
		if n > maxName {
			maxName = n
		}
	}
	for _, lib := range libs {
		name := lib.Name.SchemeString()
		desc := firstLine(lib.Description)
		fmt.Fprintf(&content, "  %-*s  %s\n", maxName, name, desc)
	}
	writeWithPager(out, content.String(), p.pager)
}

func (p *MetaCommandHandler) cmdDisassemble(args []string, out io.Writer) {
	if len(args) == 0 {
		fmt.Fprintln(out, "Usage: ,disassemble <name>")
		return
	}

	content, err := p.DisassembleBinding(args[0])
	if err != nil {
		fmt.Fprintln(out, err.Error())
		return
	}
	writeWithPager(out, content, p.pager)
}

// DisassembleBinding looks up a named binding and returns its formatted
// disassembly. Returns an error if the name is unbound, nil, or not a
// procedure. Exported for use by the MCP handler.
func (p *MetaCommandHandler) DisassembleBinding(name string) (string, error) {
	sym := values.NewSymbol(name)

	var val values.Value
	if p.env != nil {
		topLevel := p.env.Namespace()
		if topLevel != nil {
			phases := topLevel.Phases()
			phaseIndices := phases.Phases()
			sort.Ints(phaseIndices)
			for _, phase := range phaseIndices {
				phaseEnv := phases.Get(phase)
				if phaseEnv == nil {
					continue
				}
				bnd := phaseEnv.GetBinding(sym, nil)
				if bnd != nil {
					val = bnd.Value()
					break
				}
			}
		}
	}

	if val == nil {
		return "", werr.NewForeignErrorf("Unbound identifier: %s", name)
	}

	switch c := val.(type) {
	case *machine.MachineClosure:
		if c == nil {
			return "", werr.NewForeignErrorf("%s is bound to a nil closure", name)
		}
		return machine.DisassembleString(c.Template()), nil
	case *machine.CaseLambdaClosure:
		if c == nil {
			return "", werr.NewForeignErrorf("%s is bound to a nil closure", name)
		}
		var sb strings.Builder
		for i, clause := range c.Clauses() {
			if i > 0 {
				sb.WriteString("\n")
			}
			fmt.Fprintf(&sb, "--- clause %d ---\n", i)
			sb.WriteString(machine.DisassembleString(clause.Template()))
		}
		return sb.String(), nil
	case *machine.ForeignClosure:
		if c == nil {
			return "", werr.NewForeignErrorf("%s is bound to a nil closure", name)
		}
		var sb strings.Builder
		fmt.Fprintf(&sb, "%s  (foreign, params: %d, variadic: %v)\n",
			c.Name(), c.ParameterCount(), c.IsVariadic())
		if c.Doc() != "" {
			fmt.Fprintf(&sb, "doc: %s\n", c.Doc())
		}
		return sb.String(), nil
	default:
		return "", werr.NewForeignErrorf("%s is not a procedure (type: %T)", name, val)
	}
}

// searchBindings searches phase environment bindings for the pattern.
func (p *MetaCommandHandler) searchBindings(pattern string) []DocSearchResult {
	if p.env == nil {
		return nil
	}
	lowerPattern := strings.ToLower(pattern)
	topLevel := p.env.Namespace()
	if topLevel == nil {
		return nil
	}
	phases := topLevel.Phases()
	phaseIndices := phases.Phases()

	seen := make(map[string]bool)
	var results []DocSearchResult
	for _, phase := range phaseIndices {
		phaseEnv := phases.Get(phase)
		if phaseEnv == nil {
			continue
		}
		global := phaseEnv.GlobalEnvironment()
		if global == nil {
			continue
		}
		// Keys() and Bindings() are separate locked snapshots. A concurrent
		// define could add a key whose index exceeds the bindings snapshot
		// length. The idx < len(bindings) guard below prevents a panic;
		// the skipped entry is acceptable for a best-effort REPL search.
		keys := global.Keys()
		bindings := global.Bindings()
		for sym, idx := range keys {
			name := sym.Key
			if seen[name] {
				continue
			}
			seen[name] = true

			doc := ""
			if idx < len(bindings) {
				bnd := bindings[idx]
				if bnd == nil {
					continue
				}
				doc = bnd.Doc()
				if doc == "" && bnd.BindingType() == environment.BindingTypeVariable {
					doc = callableDoc(bnd.Value())
				}
			}

			if strings.Contains(strings.ToLower(name), lowerPattern) ||
				strings.Contains(strings.ToLower(doc), lowerPattern) {
				results = append(results, DocSearchResult{
					Name: name,
					Doc:  doc,
				})
			}
		}
	}
	sort.Slice(results, func(i, j int) bool {
		return results[i].Name < results[j].Name
	})
	return results
}

// mergeSearchResults merges registry and environment results, deduplicating by name.
// Registry results take precedence (richer metadata).
func mergeSearchResults(registryResults, envResults []DocSearchResult) []DocSearchResult {
	seen := make(map[string]bool, len(registryResults))
	for _, r := range registryResults {
		seen[r.Name] = true
	}
	for _, r := range envResults {
		if !seen[r.Name] {
			registryResults = append(registryResults, r)
		}
	}
	sort.Slice(registryResults, func(i, j int) bool {
		return registryResults[i].Name < registryResults[j].Name
	})
	return registryResults
}

// searchLibraries searches loaded libraries for the pattern, matching against
// the library name (e.g. "(wile algebra)") and its description.
func (p *MetaCommandHandler) searchLibraries(pattern string) []DocSearchResult {
	if p.env == nil {
		return nil
	}
	regAny := p.env.LibraryRegistry()
	if regAny == nil {
		return nil
	}
	reg, ok := regAny.(*compilation.LibraryRegistry)
	if !ok {
		return nil
	}
	lowerPattern := strings.ToLower(pattern)
	var results []DocSearchResult
	for _, lib := range reg.All() {
		name := lib.Name.SchemeString()
		if strings.Contains(strings.ToLower(name), lowerPattern) ||
			strings.Contains(strings.ToLower(lib.Description), lowerPattern) {
			results = append(results, DocSearchResult{
				Name:     name,
				Doc:      lib.Description,
				Category: "library",
			})
		}
	}
	return results
}

// firstLine returns the first line of s, or s itself if single-line.
func firstLine(s string) string {
	line, _, found := strings.Cut(s, "\n")
	if found {
		return line
	}
	return s
}
