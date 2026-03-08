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
	"github.com/aalpar/wile/values"
)

// MetaCommandHandler dispatches comma-prefixed meta-commands.
// Session commands (help, doc, edit) are handled directly;
// debug commands are delegated to DebugContext.
type MetaCommandHandler struct {
	env      *environment.EnvironmentFrame
	debugCtx *DebugContext
	docProv  DocProvider
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
	}
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
	default:
		// Delegate to debug context
		if p.debugCtx != nil && p.debugCtx.HandleDebugCommand(line, out) {
			return true
		}
		fmt.Fprintf(out, "Unknown command: %s (type ,help for commands)\n", cmd)
	}

	return true
}

// Commands returns all meta-command names (session + debug) for autocomplete.
func (p *MetaCommandHandler) Commands() []string {
	return []string{
		"help", "doc", "edit",
		"break", "delete", "list", "enable", "disable",
		"step", "next", "finish", "continue",
		"backtrace", "where",
	}
}

type commandInfo struct {
	name     string
	aliases  []string
	summary  string
	detail   string
	category string // "session" or "debug"
}

// metaCommands defines metadata for all commands (session + debug).
// Debug command metadata is duplicated from DebugContext to provide unified
// help output. If debug commands are added or changed, update both here
// and in DebugContext.HandleDebugCommand.
var metaCommands = []commandInfo{
	{"help", []string{"h", "?"}, "Show this help or help for a specific command",
		"Usage: ,help [command]\n\nWith no arguments, lists all commands.\nWith a command name, shows detailed help for that command.",
		"session"},
	{"doc", nil, "Show documentation for a Scheme binding",
		"Usage: ,doc <name>\n\nLooks up the named binding across all phase environments\n(runtime, expand, compile) and displays documentation.\nFor primitives, shows signature, description, and category.\nFor user bindings, shows type and current value.",
		"session"},
	{"edit", nil, "Open file in $EDITOR",
		"Usage: ,edit <file>\n\nOpens the given file in the editor specified by the $EDITOR\nenvironment variable. The REPL blocks until the editor exits.",
		"session"},
	{"break", []string{"b"}, "Set breakpoint at FILE:LINE[:COLUMN]",
		"Usage: ,break FILE:LINE[:COLUMN]", "debug"},
	{"delete", []string{"d"}, "Delete a breakpoint",
		"Usage: ,delete ID", "debug"},
	{"list", []string{"l"}, "List breakpoints",
		"Usage: ,list", "debug"},
	{"enable", nil, "Enable a breakpoint",
		"Usage: ,enable ID", "debug"},
	{"disable", nil, "Disable a breakpoint",
		"Usage: ,disable ID", "debug"},
	{"step", []string{"s"}, "Step into",
		"Usage: ,step", "debug"},
	{"next", []string{"n"}, "Step over",
		"Usage: ,next", "debug"},
	{"finish", []string{"f"}, "Step out",
		"Usage: ,finish", "debug"},
	{"continue", []string{"c"}, "Continue execution",
		"Usage: ,continue", "debug"},
	{"backtrace", []string{"bt"}, "Show stack trace",
		"Usage: ,backtrace", "debug"},
	{"where", nil, "Show current location",
		"Usage: ,where", "debug"},
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

	writeWithPager(out, content.String(), os.Getenv("PAGER"))
}

func (p *MetaCommandHandler) cmdHelpSpecific(name string, out io.Writer) {
	for _, cmd := range metaCommands {
		if cmd.name == name || containsString(cmd.aliases, name) {
			var content strings.Builder
			fmt.Fprintf(&content, ",%s — %s\n\n%s\n", cmd.name, cmd.summary, cmd.detail)
			writeWithPager(out, content.String(), os.Getenv("PAGER"))
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
		fmt.Fprintln(out, "Usage: ,doc <name>")
		return
	}

	name := args[0]
	var content strings.Builder

	// Try DocProvider first (primitive registry docs)
	if p.docProv != nil {
		info, found := p.docProv.LookupDoc(name)
		if found {
			formatPrimitiveDoc(&content, name, info)
			writeWithPager(out, content.String(), os.Getenv("PAGER"))
			return
		}
	}

	// Walk phase environments for binding info
	if p.env != nil {
		topLevel := p.env.TopLevelEnv()
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
				bnd := phaseEnv.GetBinding(sym)
				if bnd != nil {
					formatBindingDoc(&content, name, bnd, phase)
					writeWithPager(out, content.String(), os.Getenv("PAGER"))
					return
				}
			}
		}
	}

	fmt.Fprintf(out, "Unbound identifier: %s\n", name)
}

func formatPrimitiveDoc(w *strings.Builder, name string, info DocInfo) {
	// Signature line
	fmt.Fprintf(w, "(%s", name)
	for _, pn := range info.ParamNames {
		fmt.Fprintf(w, " %s", pn)
	}
	if info.IsVariadic {
		fmt.Fprint(w, " ...")
	}
	fmt.Fprintln(w, ")")

	if info.Doc != "" {
		fmt.Fprintf(w, "  %s\n", info.Doc)
	}
	if info.Category != "" {
		fmt.Fprintf(w, "  Category: %s\n", info.Category)
	}
}

func formatBindingDoc(w *strings.Builder, name string, bnd *environment.Binding, phase int) {
	phaseName := phaseLabel(phase)

	switch bnd.BindingType() {
	case environment.BindingTypePrimitive:
		fmt.Fprintf(w, "%s: primitive (%s)\n", name, phaseName)
	case environment.BindingTypeSyntax:
		fmt.Fprintf(w, "%s: syntax transformer (%s)\n", name, phaseName)
	case environment.BindingTypeVariable:
		val := bnd.Value()
		fmt.Fprintf(w, "%s: %s (%s)\n", name, val.SchemeString(), phaseName)
	default:
		fmt.Fprintf(w, "%s: bound in %s\n", name, phaseName)
	}
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
