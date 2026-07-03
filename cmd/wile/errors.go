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

// Exit codes from BSD sysexits.h. Only the codes actually used by the CLI are
// defined here; the full sysexits table (EX_USAGE, EX_DATAERR, ...) was unused
// and removed. Add a code back from sysexits.h when a caller needs it. Note
// that not every exit path uses a sysexits code — Failf and several argument-
// parsing errors exit with a plain 1.
const (
	EX_OK    = 0  // successful termination
	EX_IOERR = 74 // input/output error
)
