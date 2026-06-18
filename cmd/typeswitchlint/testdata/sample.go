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

// Package testdata holds fixtures for the typeswitchlint test. The marker
// placement and switch line numbers are asserted by main_test.go, so do not
// reflow this file.
package testdata

import "github.com/aalpar/wile/pkg/values"

func markedAbove(x values.Value) int {
	//exhaustive
	switch x.(type) {
	case *values.Integer:
		return 1
	case *values.Float:
		return 2
	}
	return 0
}

func markedTrailing(x values.Value) int {
	switch x.(type) { //exhaustive
	case *values.Boolean:
		return 1
	}
	return 0
}

func unmarked(x values.Value) int {
	switch x.(type) {
	case *values.Symbol:
		return 1
	}
	return 0
}

func markedNoValues(x any) int {
	//exhaustive
	switch x.(type) {
	case int:
		return 1
	case string:
		return 2
	}
	return 0
}
