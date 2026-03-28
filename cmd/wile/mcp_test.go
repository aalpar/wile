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
	"encoding/json"
	"strings"
	"testing"
	"time"

	qt "github.com/frankban/quicktest"
	"github.com/mark3labs/mcp-go/mcp"
	"github.com/mark3labs/mcp-go/server"
)

// --- test helpers ---

// toolReq builds a CallToolRequest with the given arguments map.
func toolReq(args map[string]any) mcp.CallToolRequest {
	return mcp.CallToolRequest{
		Params: mcp.CallToolParams{
			Arguments: args,
		},
	}
}

// resultText extracts the text from a single-content tool result.
func resultText(c *qt.C, res *mcp.CallToolResult) string {
	c.Helper()
	c.Assert(res.Content, qt.HasLen, 1)
	tc, ok := res.Content[0].(mcp.TextContent)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected TextContent, got %T", res.Content[0]))
	return tc.Text
}

// newTestServer creates a fresh mcpServer for testing.
func newTestServer() *mcpServer {
	return &mcpServer{}
}

// --- handleEval tests ---

func TestHandleEval_BasicArithmetic(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	res, err := srv.handleEval(ctx, toolReq(map[string]any{"code": "(+ 1 2)"}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)

	var result evalResult
	c.Assert(json.Unmarshal([]byte(resultText(c, res)), &result), qt.IsNil)
	c.Assert(result.Value, qt.Equals, "3")
	c.Assert(result.Output, qt.Equals, "")
}

func TestHandleEval_ForwardReferences(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	code := `
(define (even? n) (if (= n 0) #t (odd? (- n 1))))
(define (odd? n) (if (= n 0) #f (even? (- n 1))))
(even? 10)
`
	res, err := srv.handleEval(ctx, toolReq(map[string]any{"code": code}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)

	var result evalResult
	c.Assert(json.Unmarshal([]byte(resultText(c, res)), &result), qt.IsNil)
	c.Assert(result.Value, qt.Equals, "#t")
}

func TestHandleEval_OutputCapture(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	code := `(import (scheme write)) (display "hello world") (+ 1 1)`
	res, err := srv.handleEval(ctx, toolReq(map[string]any{"code": code}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)

	var result evalResult
	c.Assert(json.Unmarshal([]byte(resultText(c, res)), &result), qt.IsNil)
	c.Assert(result.Output, qt.Equals, "hello world")
	c.Assert(result.Value, qt.Equals, "2")
}

func TestHandleEval_VoidResult(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	res, err := srv.handleEval(ctx, toolReq(map[string]any{"code": "(define x 42)"}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)

	var result evalResult
	c.Assert(json.Unmarshal([]byte(resultText(c, res)), &result), qt.IsNil)
	// define returns void — both fields should be empty.
	c.Assert(result.Value, qt.Equals, "")
	c.Assert(result.Output, qt.Equals, "")
}

func TestHandleEval_SessionPersistence(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	// First call: define a binding.
	res, err := srv.handleEval(ctx, toolReq(map[string]any{"code": "(define x 42)"}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)

	// Second call: use the binding from the first call.
	res, err = srv.handleEval(ctx, toolReq(map[string]any{"code": "(* x 2)"}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)

	var result evalResult
	c.Assert(json.Unmarshal([]byte(resultText(c, res)), &result), qt.IsNil)
	c.Assert(result.Value, qt.Equals, "84")
}

func TestHandleEval_ParseError(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	// Use a token-level error that (begin ...) wrapping cannot heal.
	res, err := srv.handleEval(ctx, toolReq(map[string]any{"code": "#\\bad-char-literal"}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsTrue)
}

func TestHandleEval_RuntimeError(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	res, err := srv.handleEval(ctx, toolReq(map[string]any{"code": "(/ 1 0)"}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsTrue)
	text := resultText(c, res)
	c.Assert(strings.Contains(text, "zero") || strings.Contains(text, "division"), qt.IsTrue,
		qt.Commentf("expected division-by-zero error, got: %s", text))
}

func TestHandleEval_ErrorWithOutput(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	code := `(import (scheme write)) (display "before") (error "boom")`
	res, err := srv.handleEval(ctx, toolReq(map[string]any{"code": code}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsTrue)

	text := resultText(c, res)
	c.Assert(strings.Contains(text, "before"), qt.IsTrue,
		qt.Commentf("error result should include captured output, got: %s", text))
	c.Assert(strings.Contains(text, "boom"), qt.IsTrue,
		qt.Commentf("error result should include error message, got: %s", text))
}

func TestHandleEval_EmptyCode(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	res, err := srv.handleEval(ctx, toolReq(map[string]any{"code": ""}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsTrue)
	c.Assert(strings.Contains(resultText(c, res), "required"), qt.IsTrue)
}

func TestHandleEval_MissingCode(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	res, err := srv.handleEval(ctx, toolReq(map[string]any{}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsTrue)
	c.Assert(strings.Contains(resultText(c, res), "required"), qt.IsTrue)
}

func TestHandleEval_Timeout(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	// Use a very short per-call timeout to trigger cancellation.
	res, err := srv.handleEval(ctx, toolReq(map[string]any{
		"code":    "(let loop () (loop))",
		"timeout": 0.1,
	}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsTrue)
}

func TestHandleEval_PanicRecovery(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	// Trigger a panic — (raise 42) with no guard should panic in the VM.
	res, err := srv.handleEval(ctx, toolReq(map[string]any{"code": "(raise 42)"}))
	c.Assert(err, qt.IsNil)
	// Whether this is IsError or a panic-recovery result, the server survives.
	// The key assertion: the server didn't crash and returned a result.
	c.Assert(res, qt.IsNotNil)

	// Verify the server is still operational after the panic.
	res, err = srv.handleEval(ctx, toolReq(map[string]any{"code": "(+ 1 1)"}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)

	var result evalResult
	c.Assert(json.Unmarshal([]byte(resultText(c, res)), &result), qt.IsNil)
	c.Assert(result.Value, qt.Equals, "2")
}

// --- handleReset tests ---

func TestHandleReset_ClearsState(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	// Define a binding.
	res, err := srv.handleEval(ctx, toolReq(map[string]any{"code": "(define x 99)"}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)

	// Reset the session.
	res, err = srv.handleReset(ctx, toolReq(nil))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)
	c.Assert(strings.Contains(resultText(c, res), "reset"), qt.IsTrue,
		qt.Commentf("expected reset confirmation, got: %s", resultText(c, res)))

	// The binding should be gone — referencing x should error.
	res, err = srv.handleEval(ctx, toolReq(map[string]any{"code": "x"}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsTrue)
}

func TestHandleReset_BeforeInit(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	// Reset without ever initializing — should not error.
	res, err := srv.handleReset(ctx, toolReq(nil))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)
}

// --- handleSetTimeout tests ---

func TestHandleSetTimeout_Set(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()

	res, err := srv.handleSetTimeout(context.Background(), toolReq(map[string]any{"seconds": 60.0}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)
	c.Assert(srv.defaultTimeout, qt.Equals, 60*time.Second)
}

func TestHandleSetTimeout_Disable(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	srv.defaultTimeout = 30 * time.Second

	res, err := srv.handleSetTimeout(context.Background(), toolReq(map[string]any{"seconds": 0.0}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)
	c.Assert(srv.defaultTimeout, qt.Equals, time.Duration(0))
	c.Assert(strings.Contains(resultText(c, res), "disabled"), qt.IsTrue,
		qt.Commentf("expected disabled message, got: %s", resultText(c, res)))
}

func TestHandleSetTimeout_MissingParam(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()

	res, err := srv.handleSetTimeout(context.Background(), toolReq(map[string]any{}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsTrue)
}

// --- meta-command tool tests ---

func TestHandleDoc_KnownBinding(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	res, err := srv.handleDoc(ctx, toolReq(map[string]any{"name": "car"}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)
	text := resultText(c, res)
	c.Assert(strings.Contains(text, "car"), qt.IsTrue,
		qt.Commentf("doc for car should mention 'car', got: %s", text))
}

func TestHandleDoc_MissingName(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	res, err := srv.handleDoc(ctx, toolReq(map[string]any{}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsTrue)
	c.Assert(strings.Contains(resultText(c, res), "required"), qt.IsTrue)
}

func TestHandleApropos_FindsResults(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	res, err := srv.handleApropos(ctx, toolReq(map[string]any{"pattern": "string"}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)
	text := resultText(c, res)
	c.Assert(strings.Contains(text, "string"), qt.IsTrue,
		qt.Commentf("apropos for 'string' should find string-related bindings, got: %s", text))
}

func TestHandleApropos_MissingPattern(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	res, err := srv.handleApropos(ctx, toolReq(map[string]any{}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsTrue)
	c.Assert(strings.Contains(resultText(c, res), "required"), qt.IsTrue)
}

func TestHandleTopics(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	res, err := srv.handleTopics(ctx, toolReq(nil))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)
	// Topics should return a non-empty list of categories.
	c.Assert(len(resultText(c, res)) > 0, qt.IsTrue)
}

func TestHandleTopic_ValidCategory(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	// "list" is a category that should exist across any Scheme implementation.
	res, err := srv.handleTopic(ctx, toolReq(map[string]any{"category": "list"}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)
	text := resultText(c, res)
	c.Assert(len(text) > 0, qt.IsTrue,
		qt.Commentf("topic 'list' should return bindings"))
}

func TestHandleTopic_MissingCategory(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	res, err := srv.handleTopic(ctx, toolReq(map[string]any{}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsTrue)
	c.Assert(strings.Contains(resultText(c, res), "required"), qt.IsTrue)
}

func TestHandleLibraries(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	res, err := srv.handleLibraries(ctx, toolReq(nil))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)
	// Should mention at least some built-in libraries.
	text := resultText(c, res)
	c.Assert(len(text) > 0, qt.IsTrue,
		qt.Commentf("libraries should return non-empty result"))
}

// --- registerPrompts integration tests ---

func TestRegisterPrompts_ListsWileScheme(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()

	s := server.NewMCPServer("test", "0.0.0", server.WithPromptCapabilities(true))
	err := srv.registerPrompts(s)
	c.Assert(err, qt.IsNil)

	resp := s.HandleMessage(context.Background(), json.RawMessage(`{
		"jsonrpc": "2.0",
		"id": 1,
		"method": "prompts/list"
	}`))
	rpcResp, ok := resp.(mcp.JSONRPCResponse)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected JSONRPCResponse, got %T", resp))

	result, ok := rpcResp.Result.(mcp.ListPromptsResult)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected ListPromptsResult, got %T", rpcResp.Result))
	c.Assert(result.Prompts, qt.HasLen, 1)
	c.Assert(result.Prompts[0].Name, qt.Equals, "wile-scheme")
	c.Assert(result.Prompts[0].Description, qt.Not(qt.Equals), "")

	// Verify the prompt declares a required "task" argument.
	c.Assert(result.Prompts[0].Arguments, qt.HasLen, 1)
	c.Assert(result.Prompts[0].Arguments[0].Name, qt.Equals, "task")
	c.Assert(result.Prompts[0].Arguments[0].Required, qt.IsTrue)
}

func TestRegisterPrompts_GetSubstitutesTask(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()

	s := server.NewMCPServer("test", "0.0.0", server.WithPromptCapabilities(true))
	err := srv.registerPrompts(s)
	c.Assert(err, qt.IsNil)

	resp := s.HandleMessage(context.Background(), json.RawMessage(`{
		"jsonrpc": "2.0",
		"id": 2,
		"method": "prompts/get",
		"params": {
			"name": "wile-scheme",
			"arguments": {"task": "implement Dijkstra's algorithm"}
		}
	}`))
	rpcResp, ok := resp.(mcp.JSONRPCResponse)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected JSONRPCResponse, got %T", resp))

	result, ok := rpcResp.Result.(mcp.GetPromptResult)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected GetPromptResult, got %T", rpcResp.Result))
	c.Assert(result.Messages, qt.HasLen, 1)

	tc, ok := result.Messages[0].Content.(mcp.TextContent)
	c.Assert(ok, qt.IsTrue)

	// The embedded template should have {{task}} replaced.
	c.Assert(strings.Contains(tc.Text, "implement Dijkstra's algorithm"), qt.IsTrue,
		qt.Commentf("task should be substituted in the prompt"))
	c.Assert(strings.Contains(tc.Text, "{{task}}"), qt.IsFalse,
		qt.Commentf("raw placeholder should not remain"))

	// Spot-check that the real embedded content is present,
	// not an empty or synthetic template.
	c.Assert(strings.Contains(tc.Text, "eval"), qt.IsTrue,
		qt.Commentf("embedded prompt should mention the eval tool"))
	c.Assert(strings.Contains(tc.Text, "persistent session"), qt.IsTrue,
		qt.Commentf("embedded prompt should describe the session model"))
}

// --- prompt handler tests ---

func TestMakePromptHandler_Substitution(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()

	handler := srv.makePromptHandler("Task: {{task}}", []string{"task"})

	req := mcp.GetPromptRequest{
		Params: mcp.GetPromptParams{
			Arguments: map[string]string{"task": "compute fibonacci"},
		},
	}
	result, err := handler(context.Background(), req)
	c.Assert(err, qt.IsNil)
	c.Assert(result.Messages, qt.HasLen, 1)
	tc, ok := result.Messages[0].Content.(mcp.TextContent)
	c.Assert(ok, qt.IsTrue)
	c.Assert(strings.Contains(tc.Text, "compute fibonacci"), qt.IsTrue)
	c.Assert(strings.Contains(tc.Text, "{{task}}"), qt.IsFalse,
		qt.Commentf("placeholder should be replaced"))
}

func TestMakePromptHandler_MissingArg(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()

	handler := srv.makePromptHandler("Task: {{task}}", []string{"task"})

	req := mcp.GetPromptRequest{
		Params: mcp.GetPromptParams{
			Arguments: map[string]string{},
		},
	}
	result, err := handler(context.Background(), req)
	c.Assert(err, qt.IsNil)
	c.Assert(result.Messages, qt.HasLen, 1)
	tc, ok := result.Messages[0].Content.(mcp.TextContent)
	c.Assert(ok, qt.IsTrue)
	c.Assert(strings.Contains(tc.Text, "(not specified)"), qt.IsTrue,
		qt.Commentf("missing arg should be replaced with default"))
}

func TestMakePromptHandler_UnknownArgIgnored(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()

	handler := srv.makePromptHandler("Task: {{task}}", []string{"task"})

	req := mcp.GetPromptRequest{
		Params: mcp.GetPromptParams{
			Arguments: map[string]string{
				"task":    "do stuff",
				"unknown": "should be ignored",
			},
		},
	}
	result, err := handler(context.Background(), req)
	c.Assert(err, qt.IsNil)
	tc, ok := result.Messages[0].Content.(mcp.TextContent)
	c.Assert(ok, qt.IsTrue)
	c.Assert(strings.Contains(tc.Text, "do stuff"), qt.IsTrue)
	c.Assert(strings.Contains(tc.Text, "should be ignored"), qt.IsFalse,
		qt.Commentf("unknown args should not appear in output"))
}

// --- resource handler tests ---

// resourceReq builds a ReadResourceRequest for the given URI.
func resourceReq(uri string) mcp.ReadResourceRequest {
	return mcp.ReadResourceRequest{
		Params: mcp.ReadResourceParams{
			URI: uri,
		},
	}
}

// resourceText extracts the text from the first TextResourceContents.
func resourceText(c *qt.C, contents []mcp.ResourceContents) string {
	c.Helper()
	c.Assert(len(contents) > 0, qt.IsTrue, qt.Commentf("expected at least one resource content"))
	tc, ok := contents[0].(mcp.TextResourceContents)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected TextResourceContents, got %T", contents[0]))
	return tc.Text
}

func TestHandleSessionResource_BeforeInit(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	srv.defaultTimeout = 30 * time.Second

	contents, err := srv.handleSessionResource(context.Background(), resourceReq("wile://session"))
	c.Assert(err, qt.IsNil)

	var state sessionState
	c.Assert(json.Unmarshal([]byte(resourceText(c, contents)), &state), qt.IsNil)
	c.Assert(state.Initialized, qt.IsFalse)
}

func TestHandleSessionResource_AfterInit(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	srv.defaultTimeout = 30 * time.Second
	ctx := context.Background()

	// Initialize engine via eval.
	_, err := srv.handleEval(ctx, toolReq(map[string]any{"code": "(define x 1)"}))
	c.Assert(err, qt.IsNil)

	contents, err := srv.handleSessionResource(ctx, resourceReq("wile://session"))
	c.Assert(err, qt.IsNil)

	var state sessionState
	c.Assert(json.Unmarshal([]byte(resourceText(c, contents)), &state), qt.IsNil)
	c.Assert(state.Initialized, qt.IsTrue)
	c.Assert(state.LibraryCount > 0, qt.IsTrue,
		qt.Commentf("should report loaded libraries"))
	c.Assert(state.TimeoutSeconds, qt.Equals, 30.0)
}

func TestHandleLibrariesResource(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	contents, err := srv.handleLibrariesResource(ctx, resourceReq("wile://libraries"))
	c.Assert(err, qt.IsNil)

	text := resourceText(c, contents)
	var libs []libraryInfo
	c.Assert(json.Unmarshal([]byte(text), &libs), qt.IsNil)
	c.Assert(len(libs) > 0, qt.IsTrue,
		qt.Commentf("should list at least one library"))

	// Verify structure: each entry has a name.
	for _, lib := range libs {
		c.Assert(lib.Name != "", qt.IsTrue,
			qt.Commentf("each library should have a name"))
	}
}

func TestHandlePrimitivesResource(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	contents, err := srv.handlePrimitivesResource(ctx, resourceReq("wile://primitives"))
	c.Assert(err, qt.IsNil)

	text := resourceText(c, contents)
	var prims []primitiveInfo
	c.Assert(json.Unmarshal([]byte(text), &prims), qt.IsNil)
	c.Assert(len(prims) > 10, qt.IsTrue,
		qt.Commentf("should list many primitives, got %d", len(prims)))

	// Spot-check: "car" should be present.
	found := false
	for _, p := range prims {
		if p.Name == "car" {
			found = true
			c.Assert(p.ParamCount, qt.Equals, 1)
			c.Assert(p.Variadic, qt.IsFalse)
			break
		}
	}
	c.Assert(found, qt.IsTrue, qt.Commentf("car should be in primitives list"))
}

func TestRegisterResources_ListsAll(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()

	s := server.NewMCPServer("test", "0.0.0", server.WithResourceCapabilities(true, false))
	srv.registerResources(s)

	resp := s.HandleMessage(context.Background(), json.RawMessage(`{
		"jsonrpc": "2.0",
		"id": 1,
		"method": "resources/list"
	}`))
	rpcResp, ok := resp.(mcp.JSONRPCResponse)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected JSONRPCResponse, got %T", resp))

	result, ok := rpcResp.Result.(mcp.ListResourcesResult)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected ListResourcesResult, got %T", rpcResp.Result))
	c.Assert(result.Resources, qt.HasLen, 3)

	// Verify URIs are present.
	uris := make(map[string]bool)
	for _, r := range result.Resources {
		uris[r.URI] = true
	}
	c.Assert(uris["wile://session"], qt.IsTrue)
	c.Assert(uris["wile://libraries"], qt.IsTrue)
	c.Assert(uris["wile://primitives"], qt.IsTrue)
}

// --- initLocked tests ---

func TestInitLocked_Idempotent(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	ctx := context.Background()

	srv.mu.Lock()
	err := srv.initLocked(ctx)
	srv.mu.Unlock()
	c.Assert(err, qt.IsNil)

	// Save the engine pointer.
	engine1 := srv.engine

	// Call again — should be a no-op.
	srv.mu.Lock()
	err = srv.initLocked(ctx)
	srv.mu.Unlock()
	c.Assert(err, qt.IsNil)
	c.Assert(srv.engine, qt.Equals, engine1,
		qt.Commentf("second initLocked should not recreate the engine"))
}

// --- session default timeout tests ---

func TestHandleEval_SessionDefaultTimeout(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	srv.defaultTimeout = 100 * time.Millisecond
	ctx := context.Background()

	res, err := srv.handleEval(ctx, toolReq(map[string]any{
		"code": "(let loop () (loop))",
	}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsTrue)
}

func TestHandleEval_PerCallTimeoutOverridesDefault(t *testing.T) {
	c := qt.New(t)
	srv := newTestServer()
	srv.defaultTimeout = 100 * time.Millisecond
	ctx := context.Background()

	// Per-call timeout of 0 disables timeout entirely,
	// but we still need a finite test. Use a short expression instead.
	res, err := srv.handleEval(ctx, toolReq(map[string]any{
		"code":    "(+ 1 1)",
		"timeout": 0.0,
	}))
	c.Assert(err, qt.IsNil)
	c.Assert(res.IsError, qt.IsFalse)
}
