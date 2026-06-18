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

//nolint:errcheck // Pager output doesn't need error handling
package repl

import (
	"context"
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
	if len(parts) == 0 {
		fmt.Fprint(out, content)
		return
	}
	cmd := exec.CommandContext(context.Background(), parts[0], parts[1:]...)
	cmd.Stdin = strings.NewReader(content)
	cmd.Stdout = out
	cmd.Stderr = out

	err := cmd.Run()
	if err != nil {
		// Pager failed — fall back to direct write
		fmt.Fprint(out, content)
	}
}
