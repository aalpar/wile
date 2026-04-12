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

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/docparse"
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// MetaCommandHandler dispatches comma-prefixed meta-commands.
// Session commands (help, doc, edit) are handled directly;
// debug commands are delegated to DebugContext.
type MetaCommandHandler struct {
	eng      *wile.Engine
	debugCtx *DebugContext
	docProv  DocProvider
	pager    string
}

// MetaOption configures a MetaCommandHandler.
type MetaOption func(*MetaCommandHandler)

// WithMetaDocProvider sets the documentation provider for the meta handler.
func WithMetaDocProvider(dp DocProvider) MetaOption {
	return func(h *MetaCommandHandler) {
		h.docProv = dp
	}
}

// NewMetaCommandHandler creates a new meta-command handler.
func NewMetaCommandHandler(eng *wile.Engine, opts ...MetaOption) *MetaCommandHandler {
	q := &MetaCommandHandler{
		eng:   eng,
		pager: os.Getenv("PAGER"),
	}
	for _, opt := range opts {
		opt(q)
	}
	return q
}

// SetDebugContext attaches a debug context for debug command delegation.
func (p *MetaCommandHandler) SetDebugContext(dc *DebugContext) {
	p.debugCtx = dc
}

// SetPager overrides the pager command used for long output.
// Pass "" to disable paging (e.g., for non-TTY contexts like MCP).
func (p *MetaCommandHandler) SetPager(pager string) {
	p.pager = pager
}

// Handle processes a line starting with ",". Returns true if the line was
// a meta-command (even if unrecognized), false if it's not a meta-command.
func (p *MetaCommandHandler) Handle(ctx context.Context, line string, out io.Writer) bool {
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
		p.cmdApropos(ctx, args, out)
	case "topics":
		p.cmdTopics(ctx, out)
	case "topic":
		p.cmdTopic(ctx, args, out)
	case "libraries", "libs":
		p.cmdLibraries(ctx, out)
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

// env returns the engine's environment, or nil if the engine is nil.
func (p *MetaCommandHandler) env() *environment.EnvironmentFrame {
	if p.eng == nil {
		return nil
	}
	return p.eng.Environment()
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
	env := p.env()
	if env != nil {
		topLevel := env.Namespace()
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
						isForeign := p.eng != nil &&
							p.eng.FormLabel(wile.WrapValue(bnd.Value())) == "primitive"
						if isForeign && p.docProv != nil {
							info, found := p.docProv.LookupDoc(name)
							if found {
								formatPrimitiveDoc(&content, name, info, showExamples)
								writeWithPager(out, content.String(), p.pager)
								return
							}
						}
					}
					formatBindingDoc(&content, name, bnd, phase, p.eng, showExamples)
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

	if p.eng == nil {
		fmt.Fprintf(out, "Library (%s): no environment available\n", strings.Join(parts, " "))
		return
	}

	lib, lookupErr := p.eng.LookupLibrary(parts...)
	if lookupErr != nil {
		fmt.Fprintf(out, "Library (%s): %v\n", strings.Join(parts, " "), lookupErr)
		return
	}
	if lib == nil {
		fmt.Fprintf(out, "Library (%s): not loaded\n", strings.Join(parts, " "))
		return
	}

	var content strings.Builder
	formatLibraryDoc(&content, lib)
	writeWithPager(out, content.String(), p.pager)
}

func formatLibraryDoc(w *strings.Builder, lib *wile.LibraryInfo) {
	fmt.Fprintf(w, "Library: %s\n", lib.Name)
	if lib.Description != "" {
		fmt.Fprintf(w, "\n  %s\n", lib.Description)
	}
	if lib.SourceFile != "" {
		fmt.Fprintf(w, "\nSource: %s\n", lib.SourceFile)
	}

	fmt.Fprintf(w, "\nExports (%d):\n", len(lib.Exports))
	for _, name := range lib.Exports {
		fmt.Fprintf(w, "  %s\n", name)
	}
}

func formatPrimitiveDoc(w *strings.Builder, name string, info DocInfo, showExamples bool) {
	hasTypes := len(info.ParamTypes) > 0

	// Line 1: syntax + return type.
	// Builder approach: prefer structured ParamNames, fall back to Syntax.
	switch {
	case len(info.ParamNames) > 0:
		fmt.Fprintf(w, "(%s", name)
		for _, pn := range info.ParamNames {
			fmt.Fprintf(w, " %s", strings.ToUpper(pn))
		}
		if info.IsVariadic {
			fmt.Fprint(w, " ...")
		}
		fmt.Fprint(w, ")")
	case info.Syntax != "":
		fmt.Fprint(w, info.Syntax)
	default:
		fmt.Fprintf(w, "(%s)", name)
	}
	if info.ReturnType != nil {
		fmt.Fprintf(w, " → %s", info.ReturnType.Name())
	}
	fmt.Fprintln(w)

	// Form type
	if info.TypeLabel != "" {
		fmt.Fprintf(w, "  Form: %s\n", info.TypeLabel)
	}

	// Description
	doc := info.Doc
	if !showExamples {
		doc = StripExamples(doc)
	}
	if doc != "" {
		indented := strings.ReplaceAll(doc, "\n", "\n  ")
		fmt.Fprintf(w, "  %s\n", indented)
	}

	// Parameter types
	if hasTypes && len(info.ParamNames) > 0 {
		fmt.Fprintln(w, "  Parameters:")
		for i, pn := range info.ParamNames {
			vt := paramTypeForDoc(info.ParamTypes, i)
			if vt != nil {
				fmt.Fprintf(w, "    %s : %s\n", strings.ToUpper(pn), vt.Name())
			} else {
				fmt.Fprintf(w, "    %s\n", strings.ToUpper(pn))
			}
		}
	}

	// Return type
	if info.ReturnType != nil {
		fmt.Fprintf(w, "  Returns: %s\n", info.ReturnType.Name())
	}

	// Category
	if info.Category != "" {
		fmt.Fprintf(w, "  Category: %s\n", info.Category)
	}

	// Keywords
	if len(info.Keywords) > 0 {
		fmt.Fprintf(w, "  Keywords: %s\n", strings.Join(info.Keywords, ", "))
	}
}

// paramTypeForDoc returns the TypeConstraint for parameter position i.
// For variadic primitives, positions beyond len(types)-1 use the last entry.
// Returns nil when no type information is available.
func paramTypeForDoc(types []values.TypeConstraint, i int) values.TypeConstraint {
	if i < len(types) {
		return types[i]
	}
	if len(types) > 0 {
		return types[len(types)-1]
	}
	return nil
}

// tryStructuredBindingDoc attempts to parse a docstring and render it via
// formatPrimitiveDoc when structured metadata is found. Returns true if
// the structured path was taken (caller should return early).
func tryStructuredBindingDoc(w *strings.Builder, name, doc, typeLabel string, showExamples bool) bool {
	if doc == "" {
		return false
	}
	parsed := docparse.ParseDocstring(doc)
	if !parsed.HasStructuredMetadata() {
		return false
	}
	formatPrimitiveDoc(w, name, DocInfo{
		Doc:        parsed.Doc,
		Syntax:     parsed.Syntax,
		TypeLabel:  typeLabel,
		ParamNames: parsed.ParamNames,
		ParamTypes: parsed.ParamTypes,
		ReturnType: parsed.ReturnType,
		Category:   parsed.Category,
		Keywords:   parsed.Keywords,
	}, showExamples)
	return true
}

func formatBindingDoc(w *strings.Builder, name string, bnd *environment.Binding, phase int, eng *wile.Engine, showExamples bool) {
	phaseName := phaseLabel(phase)

	switch bnd.BindingType() {
	case environment.BindingTypePrimitive:
		if tryStructuredBindingDoc(w, name, bnd.Doc(), "special form", showExamples) {
			return
		}
		fmt.Fprintf(w, "%s: special form (%s)\n", name, phaseName)

	case environment.BindingTypeSyntax:
		if tryStructuredBindingDoc(w, name, bnd.Doc(), "syntax", showExamples) {
			return
		}
		fmt.Fprintf(w, "%s: syntax transformer (%s)\n", name, phaseName)

	case environment.BindingTypeVariable:
		val := bnd.Value()

		// Try structured docstring for closures.
		raw := callableDoc(val)
		if raw != "" {
			parsed := docparse.ParseDocstring(raw)
			if parsed.HasStructuredMetadata() {
				typeLabel := ""
				if eng != nil {
					typeLabel = eng.FormLabel(wile.WrapValue(val))
				}
				formatPrimitiveDoc(w, name, DocInfo{
					Doc:        parsed.Doc,
					Syntax:     parsed.Syntax,
					TypeLabel:  typeLabel,
					ParamNames: parsed.ParamNames,
					ParamTypes: parsed.ParamTypes,
					ReturnType: parsed.ReturnType,
					Category:   parsed.Category,
					Keywords:   parsed.Keywords,
				}, showExamples)
				return
			}
		}

		fmt.Fprintf(w, "%s: %s (%s)\n", name, val.SchemeString(), phaseName)
	default:
		fmt.Fprintf(w, "%s: bound in %s\n", name, phaseName)
	}

	// Fallback: raw doc display for bindings without structured metadata.
	// All structured paths above return early, so this only runs for
	// unstructured docstrings.
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
	dc, ok := v.(interface{ Doc() string })
	if ok {
		return dc.Doc()
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

func (p *MetaCommandHandler) cmdApropos(ctx context.Context, args []string, out io.Writer) {
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

	results := searchProv.Search(ctx, pattern)
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
		tag := ""
		if r.Category == "library" {
			// Libraries use "library" as a pseudo-category to identify their
			// result type, not as an actual topic. Render distinctly so users
			// don't try ",topic library".
			tag = "library"
		} else if r.Category != "" {
			tag = fmt.Sprintf("[%s]", r.Category)
		}
		doc := firstLine(r.Doc)
		fmt.Fprintf(&content, "  %-*s  %-14s %s\n", maxName, r.Name, tag, doc)
	}
	writeWithPager(out, content.String(), p.pager)
}

func (p *MetaCommandHandler) cmdTopics(_ context.Context, out io.Writer) {
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

func (p *MetaCommandHandler) cmdTopic(_ context.Context, args []string, out io.Writer) {
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

func (p *MetaCommandHandler) cmdLibraries(ctx context.Context, out io.Writer) {
	if p.eng == nil {
		fmt.Fprintln(out, "No environment available")
		return
	}

	loaded, loadErr := p.eng.LoadedLibraries()
	if loadErr != nil {
		fmt.Fprintf(out, "Error loading libraries: %v\n", loadErr)
		return
	}
	unloaded := p.eng.UnloadedLibraries(ctx)

	if len(loaded) == 0 && len(unloaded) == 0 {
		fmt.Fprintln(out, "No libraries loaded")
		return
	}

	// Compute max name width across both sections for consistent alignment.
	maxName := 0
	for _, lib := range loaded {
		if len(lib.Name) > maxName {
			maxName = len(lib.Name)
		}
	}
	for _, lib := range unloaded {
		if len(lib.Name) > maxName {
			maxName = len(lib.Name)
		}
	}

	var content strings.Builder
	if len(loaded) > 0 {
		fmt.Fprintf(&content, "Loaded libraries (%d):\n", len(loaded))
		for _, lib := range loaded {
			desc := firstLine(lib.Description)
			fmt.Fprintf(&content, "  %-*s  %s\n", maxName, lib.Name, desc)
		}
	}

	if len(unloaded) > 0 {
		if len(loaded) > 0 {
			content.WriteString("\n")
		}
		fmt.Fprintf(&content, "Available libraries (%d):\n", len(unloaded))
		for _, lib := range unloaded {
			desc := firstLine(lib.Description)
			fmt.Fprintf(&content, "  %-*s  %s\n", maxName, lib.Name, desc)
		}
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
	env := p.env()
	if env != nil {
		topLevel := env.Namespace()
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
		return "", werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"DisassembleBinding: unbound identifier: %s", name)
	}

	if p.eng == nil {
		return "", werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"DisassembleBinding: engine not available for disassembly")
	}

	return p.eng.DisassembleValue(wile.WrapValue(val))
}

// firstLine returns the first line of s, or s itself if single-line.
func firstLine(s string) string {
	line, _, found := strings.Cut(s, "\n")
	if found {
		return line
	}
	return s
}
