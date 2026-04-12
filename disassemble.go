package wile

import (
	"fmt"
	"strings"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/werr"
)

// FormLabel returns a human-readable type label for a value:
// "primitive" for foreign (Go-implemented) closures,
// "procedure" for compiled Scheme closures,
// "" for non-callable values.
func (p *Engine) FormLabel(v Value) string {
	inner := unwrapValue(v)
	switch inner.(type) {
	case *machine.ForeignClosure:
		return "primitive"
	case *machine.MachineClosure, *machine.CaseLambdaClosure:
		return "procedure"
	default:
		return ""
	}
}

// DisassembleValue returns the formatted disassembly of a callable value.
// For compiled closures, shows bytecode instructions. For case-lambda,
// shows each clause separately. For foreign closures, shows name, arity,
// and documentation. Returns an error for non-procedure values.
func (p *Engine) DisassembleValue(v Value) (string, error) {
	inner := unwrapValue(v)
	switch c := inner.(type) {
	case *machine.MachineClosure:
		if c == nil {
			return "", werr.WrapForeignErrorf(
				werr.ErrInvalidArgument,
				"DisassembleValue: nil closure")
		}
		return machine.DisassembleString(c.Template()), nil

	case *machine.CaseLambdaClosure:
		if c == nil {
			return "", werr.WrapForeignErrorf(
				werr.ErrInvalidArgument,
				"DisassembleValue: nil case-lambda closure")
		}
		var q strings.Builder
		for i, clause := range c.Clauses() {
			if i > 0 {
				q.WriteString("\n")
			}
			fmt.Fprintf(&q, "--- clause %d ---\n", i)
			q.WriteString(machine.DisassembleString(clause.Template()))
		}
		return q.String(), nil

	case *machine.ForeignClosure:
		if c == nil {
			return "", werr.WrapForeignErrorf(
				werr.ErrInvalidArgument,
				"DisassembleValue: nil foreign closure")
		}
		var q strings.Builder
		fmt.Fprintf(&q, "%s  (foreign, params: %d, variadic: %v)\n",
			c.Name(), c.ParameterCount(), c.IsVariadic())
		if c.Doc() != "" {
			fmt.Fprintf(&q, "doc: %s\n", c.Doc())
		}
		return q.String(), nil

	default:
		return "", werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"DisassembleValue: not a procedure (type: %T)", inner)
	}
}
