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
	"context"
	"fmt"
	"strings"
	"sync"

	"github.com/mark3labs/mcp-go/mcp"
	"github.com/mark3labs/mcp-go/server"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/internal/repl"
	"github.com/aalpar/wile/stdlib"
)

type mcpServer struct {
	engine     *wile.Engine
	meta       *repl.MetaCommandHandler
	engineOnce sync.Once
	engineErr  error
}

// doMCP starts a Model Context Protocol server on stdio, exposing the Wile
// documentation and evaluation tools: eval, doc, apropos, topics, topic.
func doMCP(ctx context.Context) error {
	ms := &mcpServer{}

	v := BuildVersion
	if v == "" {
		v = "dev"
	}

	s := server.NewMCPServer(
		"wile",
		v,
		server.WithToolCapabilities(true),
	)

	s.AddTool(
		mcp.NewTool("eval",
			mcp.WithDescription(
				"Evaluate one or more Scheme expressions. "+
					"All R7RS features and wile extensions are available. "+
					"Returns the value of the last expression, or empty string for void results."),
			mcp.WithString("code",
				mcp.Required(),
				mcp.Description("Scheme expression(s) to evaluate"),
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

	return server.ServeStdio(s)
}

// getHandler lazily initializes the engine and meta-command handler.
// All five tool handlers go through this.
func (ms *mcpServer) getHandler(ctx context.Context) (*repl.MetaCommandHandler, error) {
	ms.engineOnce.Do(func() {
		ms.engine, ms.engineErr = wile.NewEngine(ctx,
			wile.WithAllExtensions(),
			wile.WithSourceFS(stdlib.FS),
			wile.WithSourceOS(),
			wile.WithLibraryPaths(buildLibraryPaths()...),
		)
		if ms.engineErr != nil {
			return
		}
		docProv := repl.NewRegistryDocProvider(ms.engine.Registry())
		ms.meta = repl.NewMetaCommandHandler(ms.engine.Environment(), nil, docProv)
	})
	return ms.meta, ms.engineErr
}

func (ms *mcpServer) handleEval(ctx context.Context, req mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	code := req.GetString("code", "")
	if code == "" {
		return mcp.NewToolResultError("code parameter is required"), nil
	}
	_, err := ms.getHandler(ctx)
	if err != nil {
		return mcp.NewToolResultError(fmt.Sprintf("engine init failed: %v", err)), nil
	}
	val, err := ms.engine.EvalMultiple(ctx, code)
	if err != nil {
		return mcp.NewToolResultError(err.Error()), nil
	}
	if val == nil || val.IsVoid() {
		return mcp.NewToolResultText(""), nil
	}
	return mcp.NewToolResultText(val.SchemeString()), nil
}

// runMeta routes a comma-command through MetaCommandHandler, capturing output
// in a strings.Builder and returning it as a tool result.
func (ms *mcpServer) runMeta(ctx context.Context, line string) (*mcp.CallToolResult, error) {
	handler, err := ms.getHandler(ctx)
	if err != nil {
		return mcp.NewToolResultError(fmt.Sprintf("engine init failed: %v", err)), nil
	}
	var sb strings.Builder
	handler.Handle(line, &sb)
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
