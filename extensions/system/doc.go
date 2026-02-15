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

// Package system provides system interface primitives.
//
// # System Interface (R7RS 6.14)
//
//   - command-line: return command-line arguments as list
//   - exit: terminate with exit status
//   - emergency-exit: terminate immediately without cleanup
//   - get-environment-variable: get environment variable value
//   - get-environment-variables: get all environment variables
//
// # Features
//
//   - features: return list of implementation features
//
// # Time
//
//   - current-second: current time as inexact seconds since epoch
//   - current-jiffy: high-resolution monotonic time
//   - jiffies-per-second: resolution of current-jiffy
//
// Use [Extension] or [AddToRegistry] to register all primitives.
package system
