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

package values

// SourceTableRefs is a template's per-instruction source table: one index into the
// template's sourceTable per instruction, parallel to its code.
//
// It is VM plumbing and deliberately NOT a Value. Nothing stores a SourceTableRefs
// in a Value slot, and implementing Value would enrol it in the exhaustiveness
// set tools/cmd/typeswitchlint derives from this package (knownValueTypes), which
// exists to enumerate the types a Scheme datum can be.
type SourceTableRefs []uint32
