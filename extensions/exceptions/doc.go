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

// Package exceptions provides R7RS exception handling primitives.
//
// # Exception Handling (R7RS 6.11)
//
//   - with-exception-handler: install exception handler
//   - raise: raise non-continuable exception
//   - raise-continuable: raise continuable exception
//
// # Error Construction
//
//   - error: create error with message and irritants
//   - error-object?: test if value is error object
//   - error-object-message: get error message
//   - error-object-irritants: get error irritants
//
// # Error Predicates
//
//   - read-error?: test for read errors
//   - file-error?: test for file errors
//
// Use [Extension] or [AddToRegistry] to register all primitives.
package exceptions
