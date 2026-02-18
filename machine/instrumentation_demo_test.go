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

package machine

import (
	"context"
	"fmt"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
)

// TestParameterCopyInstrumentation demonstrates the parameter copy timing instrumentation.
// This test creates a recursive fibonacci function and measures how much time is spent
// copying parameters to bindings during execution.
func TestParameterCopyInstrumentation(t *testing.T) {
	// Skip in short mode - this is a demonstration test
	if testing.Short() {
		t.Skip("skipping instrumentation demo in short mode")
	}

	// Create a simple recursive function with varying parameter counts
	testCases := []struct {
		name       string
		paramCount int
		iterations int
	}{
		{"2-params-100-calls", 2, 100},
		{"5-params-100-calls", 5, 100},
		{"10-params-100-calls", 10, 100},
		{"2-params-1000-calls", 2, 1000},
		{"10-params-1000-calls", 10, 1000},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			// Create a simple function that just returns its first argument
			topLevel := environment.NewTopLevelEnvironment()
			env := topLevel.Runtime()

			// Create template with N parameters
			localEnv := environment.NewLocalEnvironment(tc.paramCount)
			tpl := &NativeTemplate{
				operations:     Operations{NewOperationLoadVoid()}, // Just return void
				parameterCount: tc.paramCount,
				isVariadic:     false,
			}

			// Create closure
			frameEnv := environment.NewEnvironmentFrameWithParent(localEnv, env)
			cls := NewClosureWithTemplate(tpl, frameEnv)

			// Create context
			cont := NewMachineContinuation(nil, tpl, frameEnv)
			mc := NewMachineContext(context.Background(), cont)

			// Prepare arguments
			args := make([]values.Value, tc.paramCount)
			for i := range args {
				args[i] = values.NewInteger(int64(i))
			}

			// Call the function multiple times
			for i := 0; i < tc.iterations; i++ {
				_, err := mc.Apply(cls, args...)
				if err != nil {
					t.Fatalf("Apply failed: %v", err)
				}
			}

			// Print instrumentation results
			counters := mc.counters
			avgNsPerCall := float64(counters.ParamCopyTimeNanos) / float64(tc.iterations)
			avgNsPerParam := avgNsPerCall / float64(tc.paramCount)
			totalMs := float64(counters.ParamCopyTimeNanos) / 1_000_000.0
			percentOfTotal := 0.0
			if counters.OpsExecuted > 0 {
				percentOfTotal = (float64(counters.ParamCopyTimeNanos) / 1_000_000.0) / float64(counters.OpsExecuted) * 100
			}

			fmt.Printf("\n%s:\n", tc.name)
			fmt.Printf("  Calls:                  %d\n", tc.iterations)
			fmt.Printf("  Params per call:        %d\n", tc.paramCount)
			fmt.Printf("  Total params copied:    %d\n", counters.BindingsCopied)
			fmt.Printf("  Total copy time:        %.3f ms\n", totalMs)
			fmt.Printf("  Avg time per call:      %.0f ns\n", avgNsPerCall)
			fmt.Printf("  Avg time per param:     %.0f ns\n", avgNsPerParam)
			fmt.Printf("  Percent of total time:  %.2f%%\n", percentOfTotal)
		})
	}
}

// BenchmarkParameterCopy measures the overhead of parameter copying at different arity levels.
func BenchmarkParameterCopy(b *testing.B) {
	paramCounts := []int{1, 2, 5, 10, 20}

	for _, paramCount := range paramCounts {
		b.Run(fmt.Sprintf("params=%d", paramCount), func(b *testing.B) {
			// Setup
			topLevel := environment.NewTopLevelEnvironment()
			env := topLevel.Runtime()
			localEnv := environment.NewLocalEnvironment(paramCount)
			tpl := &NativeTemplate{
				operations:     Operations{NewOperationLoadVoid()},
				parameterCount: paramCount,
				isVariadic:     false,
			}
			frameEnv := environment.NewEnvironmentFrameWithParent(localEnv, env)
			cls := NewClosureWithTemplate(tpl, frameEnv)
			cont := NewMachineContinuation(nil, tpl, frameEnv)
			mc := NewMachineContext(context.Background(), cont)

			args := make([]values.Value, paramCount)
			for i := range args {
				args[i] = values.NewInteger(int64(i))
			}

			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				_, _ = mc.Apply(cls, args...)
			}
			b.StopTimer()

			// Report instrumentation data
			avgNsPerCall := float64(mc.counters.ParamCopyTimeNanos) / float64(b.N)
			avgNsPerParam := avgNsPerCall / float64(paramCount)
			b.ReportMetric(avgNsPerCall, "ns/call")
			b.ReportMetric(avgNsPerParam, "ns/param")
		})
	}
}
