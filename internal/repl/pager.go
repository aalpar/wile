package repl

import (
	"fmt"
	"io"
	"os/exec"
	"strings"
)

// writeWithPager writes content to out, piping through the given pager command
// if non-empty. If pager is empty, writes directly to out.
// The pager command string is split on spaces to support arguments (e.g. "less -R").
func writeWithPager(out io.Writer, content string, pager string) {
	if pager == "" || content == "" {
		fmt.Fprint(out, content)
		return
	}

	parts := strings.Fields(pager)
	cmd := exec.Command(parts[0], parts[1:]...)
	cmd.Stdin = strings.NewReader(content)
	cmd.Stdout = out
	cmd.Stderr = out

	err := cmd.Run()
	if err != nil {
		// Pager failed — fall back to direct write
		fmt.Fprint(out, content)
	}
}
