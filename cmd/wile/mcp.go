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

package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"io/fs"
	"log"
	"strings"
	"sync"
	"time"

	"github.com/mark3labs/mcp-go/mcp"
	"github.com/mark3labs/mcp-go/server"

	"github.com/aalpar/wile"
	ioext "github.com/aalpar/wile/internal/extensions/io"
	"github.com/aalpar/wile/internal/repl"
	"github.com/aalpar/wile/stdlib"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

type mcpServer struct {
	mu             sync.Mutex
	engine         *wile.Engine
	meta           *repl.MetaCommandHandler
	defaultTimeout time.Duration
}

// doMCP starts a Model Context Protocol server on stdio, exposing the Wile
// documentation, evaluation, and session management tools.
func doMCP(ctx context.Context, timeoutSec float64) error {
	ms := &mcpServer{}
	if timeoutSec > 0 {
		ms.defaultTimeout = time.Duration(timeoutSec * float64(time.Second))
	}

	v := BuildVersion
	if v == "" {
		v = "dev"
	}

	s := server.NewMCPServer(
		"wile",
		v,
		server.WithToolCapabilities(true),
		server.WithPromptCapabilities(true),
	)

	s.AddTool(
		mcp.NewTool("eval",
			mcp.WithDescription(
				"Evaluate one or more Scheme expressions in a persistent session. "+
					"Definitions, imports, and state carry forward across calls. "+
					"All R7RS features and wile extensions are available. "+
					"Multiple top-level definitions in a single call can reference each other. "+
					"Returns JSON: {\"output\":\"...\", \"value\":\"...\"} where output is captured "+
					"stdout (display/write) and value is the result of the last expression. "+
					"Fields are omitted when empty. "+
					"Default timeout is 30s; pass timeout parameter to override."),
			mcp.WithString("code",
				mcp.Required(),
				mcp.Description("Scheme expression(s) to evaluate"),
			),
			mcp.WithNumber("timeout",
				mcp.Description(
					"Eval timeout in seconds. Overrides the session default (30s). "+
						"Use for long-running computations. 0 means no timeout."),
			),
		),
		ms.handleEval,
	)

	s.AddTool(
		mcp.NewTool("doc",
			mcp.WithDescription(
				"Show documentation for a Scheme binding or library. "+
					"For bindings, pass the name (e.g. \"car\", \"map\", \"define\"). "+
					"For libraries, pass the name in parentheses (e.g. \"(scheme base)\", \"(wile algebra)\"). "+
					"Returns signature, description, parameter types, category, and source."),
			mcp.WithString("name",
				mcp.Required(),
				mcp.Description("Binding name (e.g. \"car\") or library name (e.g. \"(scheme base)\")"),
			),
		),
		ms.handleDoc,
	)

	s.AddTool(
		mcp.NewTool("apropos",
			mcp.WithDescription(
				"Search Scheme bindings by name, documentation text, or category. "+
					"Case-insensitive substring match. "+
					"Returns matching names with category and one-line description."),
			mcp.WithString("pattern",
				mcp.Required(),
				mcp.Description("Search pattern (substring, case-insensitive)"),
			),
		),
		ms.handleApropos,
	)

	s.AddTool(
		mcp.NewTool("topics",
			mcp.WithDescription(
				"List available documentation categories with entry counts. "+
					"Use with the topic tool to browse by category."),
		),
		ms.handleTopics,
	)

	s.AddTool(
		mcp.NewTool("topic",
			mcp.WithDescription(
				"List all bindings in a documentation category with one-line descriptions. "+
					"Use topics tool first to see available category names."),
			mcp.WithString("category",
				mcp.Required(),
				mcp.Description("Category name (use topics tool to see available categories)"),
			),
		),
		ms.handleTopic,
	)

	s.AddTool(
		mcp.NewTool("libraries",
			mcp.WithDescription(
				"List all Scheme libraries currently loaded in the session, "+
					"sorted alphabetically with their descriptions. "+
					"Use doc with a library name (e.g. \"(scheme base)\") to see its exports."),
		),
		ms.handleLibraries,
	)

	s.AddTool(
		mcp.NewTool("reset",
			mcp.WithDescription(
				"Reset the Scheme session, discarding all definitions and imported libraries. "+
					"The next tool call reinitializes the engine from scratch. "+
					"Use this to start fresh without restarting the MCP server."),
		),
		ms.handleReset,
	)

	s.AddTool(
		mcp.NewTool("set-timeout",
			mcp.WithDescription(
				"Set the default eval timeout for this session in seconds. "+
					"Affects all subsequent eval calls that don't specify their own timeout. "+
					"Use 0 to disable the timeout. Initial default is 30s."),
			mcp.WithNumber("seconds",
				mcp.Required(),
				mcp.Description("Timeout in seconds (0 = no timeout)"),
			),
		),
		ms.handleSetTimeout,
	)

	err := ms.registerPrompts(s)
	if err != nil {
		return err
	}

	return server.ServeStdio(s)
}

// initLocked lazily initializes the engine and meta-command handler.
// The caller must hold ms.mu. Redirects current-output-port away from
// os.Stdout to prevent Scheme output from corrupting the MCP transport.
func (ms *mcpServer) initLocked(ctx context.Context) error {
	if ms.meta != nil {
		return nil
	}
	engine, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(buildLibraryPaths()...),
	)
	if err != nil {
		return err
	}
	ms.engine = engine

	// Redirect output away from stdout (the MCP JSON-RPC transport).
	// Each handleEval call captures into its own buffer; between evals,
	// output goes to discard.
	ioext.SetCurrentOutputPort(values.NewCharacterOutputPortFromWriter(io.Discard))

	docProv := repl.NewRegistryDocProvider(engine.Registry())
	ms.meta = repl.NewMetaCommandHandler(engine.Environment(), nil, docProv)
	return nil
}

// evalResult is the structured JSON response from the eval tool.
type evalResult struct {
	Output string `json:"output,omitempty"`
	Value  string `json:"value,omitempty"`
}

func (ms *mcpServer) handleEval(ctx context.Context, req mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	code := req.GetString("code", "")
	if code == "" {
		return mcp.NewToolResultError("code parameter is required"), nil
	}

	ms.mu.Lock()
	defer ms.mu.Unlock()

	err := ms.initLocked(ctx)
	if err != nil {
		return mcp.NewToolResultError(fmt.Sprintf("engine init failed: %v", err)), nil
	}

	// Apply timeout: per-call parameter overrides session default.
	timeout := req.GetFloat("timeout", -1)
	evalTimeout := ms.defaultTimeout
	if timeout > 0 {
		evalTimeout = time.Duration(timeout * float64(time.Second))
	} else if timeout == 0 {
		evalTimeout = 0
	}
	if evalTimeout > 0 {
		var cancel context.CancelFunc
		ctx, cancel = context.WithTimeout(ctx, evalTimeout)
		defer cancel()
	}

	// Capture stdout: redirect current-output-port to a buffer.
	var buf bytes.Buffer
	ioext.SetCurrentOutputPort(values.NewCharacterOutputPortFromWriter(&buf))
	defer ioext.SetCurrentOutputPort(values.NewCharacterOutputPortFromWriter(io.Discard))

	// Wrap in (begin ...) so all defines have mutual visibility,
	// matching the file execution pattern in runFile.
	wrapped := "(begin " + code + "\n)"

	val, evalErr := ms.engine.EvalMultiple(ctx, wrapped)

	var result evalResult
	output := buf.String()
	if output != "" {
		result.Output = output
	}
	if evalErr != nil {
		// Include any captured output alongside the error.
		errMsg := evalErr.Error()
		if output != "" {
			errMsg = fmt.Sprintf("%s\n\n[stderr]\n%s", output, evalErr.Error())
		}
		return mcp.NewToolResultError(errMsg), nil
	}
	if val != nil && !val.IsVoid() {
		result.Value = val.SchemeString()
	}

	encoded, jsonErr := json.Marshal(result)
	if jsonErr != nil {
		return mcp.NewToolResultError(fmt.Sprintf("encoding result: %v", jsonErr)), nil
	}
	return mcp.NewToolResultText(string(encoded)), nil
}

// runMeta routes a comma-command through MetaCommandHandler, capturing output
// in a strings.Builder and returning it as a tool result.
func (ms *mcpServer) runMeta(ctx context.Context, line string) (*mcp.CallToolResult, error) {
	ms.mu.Lock()
	defer ms.mu.Unlock()
	err := ms.initLocked(ctx)
	if err != nil {
		return mcp.NewToolResultError(fmt.Sprintf("engine init failed: %v", err)), nil
	}
	var sb strings.Builder
	ms.meta.Handle(line, &sb)
	return mcp.NewToolResultText(sb.String()), nil
}

func (ms *mcpServer) handleDoc(ctx context.Context, req mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	name := req.GetString("name", "")
	if name == "" {
		return mcp.NewToolResultError("name parameter is required"), nil
	}
	return ms.runMeta(ctx, ",doc "+name)
}

func (ms *mcpServer) handleApropos(ctx context.Context, req mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	pattern := req.GetString("pattern", "")
	if pattern == "" {
		return mcp.NewToolResultError("pattern parameter is required"), nil
	}
	return ms.runMeta(ctx, ",apropos "+pattern)
}

func (ms *mcpServer) handleTopics(ctx context.Context, _ mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	return ms.runMeta(ctx, ",topics")
}

func (ms *mcpServer) handleTopic(ctx context.Context, req mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	category := req.GetString("category", "")
	if category == "" {
		return mcp.NewToolResultError("category parameter is required"), nil
	}
	return ms.runMeta(ctx, ",topic "+category)
}

func (ms *mcpServer) handleLibraries(ctx context.Context, _ mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	return ms.runMeta(ctx, ",libraries")
}

func (ms *mcpServer) handleReset(_ context.Context, _ mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	ms.mu.Lock()
	defer ms.mu.Unlock()
	if ms.engine != nil {
		err := ms.engine.Close()
		if err != nil {
			log.Printf("reset: engine close: %v", err)
		}
		ms.engine = nil
		ms.meta = nil
	}
	ioext.ResetState()
	return mcp.NewToolResultText("Session reset. The next call will reinitialize the engine."), nil
}

func (ms *mcpServer) handleSetTimeout(_ context.Context, req mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	seconds := req.GetFloat("seconds", -1)
	if seconds < 0 {
		return mcp.NewToolResultError("seconds parameter is required"), nil
	}
	ms.mu.Lock()
	defer ms.mu.Unlock()
	if seconds == 0 {
		ms.defaultTimeout = 0
		return mcp.NewToolResultText("Eval timeout disabled."), nil
	}
	ms.defaultTimeout = time.Duration(seconds * float64(time.Second))
	return mcp.NewToolResultText(fmt.Sprintf("Eval timeout set to %s.", ms.defaultTimeout)), nil
}

func (ms *mcpServer) registerPrompts(s *server.MCPServer) error {
	type promptDef struct {
		name        string
		description string
		file        string
		args        []mcp.PromptOption
		argNames    []string
	}

	prompts := []promptDef{
		{
			name:        "wile-scheme",
			description: "Write and evaluate Scheme code with wile — session model, imports, available libraries, and common patterns",
			file:        "prompts/wile-scheme.md",
			argNames:    []string{"task"},
			args: []mcp.PromptOption{
				mcp.WithArgument("task",
					mcp.RequiredArgument(),
					mcp.ArgumentDescription("The Scheme task or question to address"),
				),
			},
		},
	}

	for _, p := range prompts {
		content, readErr := fs.ReadFile(embeddedPrompts, p.file)
		if readErr != nil {
			return werr.WrapForeignErrorWithCause(werr.ErrFileNotFound, readErr, "reading prompt %s", p.file)
		}
		text := string(content)
		promptOpts := append([]mcp.PromptOption{mcp.WithPromptDescription(p.description)}, p.args...)

		s.AddPrompt(
			mcp.NewPrompt(p.name, promptOpts...),
			ms.makePromptHandler(text, p.argNames),
		)
	}
	return nil
}

func (ms *mcpServer) makePromptHandler(template string, argNames []string) server.PromptHandlerFunc {
	allowed := make(map[string]struct{}, len(argNames))
	for _, n := range argNames {
		allowed[n] = struct{}{}
	}
	return func(_ context.Context, req mcp.GetPromptRequest) (*mcp.GetPromptResult, error) {
		text := template
		for k, v := range req.Params.Arguments {
			_, ok := allowed[k]
			if !ok {
				continue
			}
			text = strings.ReplaceAll(text, "{{"+k+"}}", v)
		}
		for _, n := range argNames {
			text = strings.ReplaceAll(text, "{{"+n+"}}", "(not specified)")
		}
		return &mcp.GetPromptResult{
			Messages: []mcp.PromptMessage{
				mcp.NewPromptMessage(mcp.RoleUser, mcp.NewTextContent(text)),
			},
		}, nil
	}
}
