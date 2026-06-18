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

package security

// ReadOnly returns an Authorizer that allows read and stat operations but
// denies everything else — including write, delete, exit, and code load.
//
// ReadOnly does NOT permit ActionLoad: loading a file compiles and runs its
// contents, which is not a read-only operation. Callers that genuinely need to
// load code under an otherwise read-only policy should use ReadOnlyWithLoad.
//
// ReadOnly applies NO path confinement — it allows reads of any path the host
// process can reach. Compose it with FilesystemRoot via All(...) to bound which
// paths may be read.
func ReadOnly() Authorizer {
	return AuthorizerFunc(func(req AccessRequest) error {
		switch req.Action {
		case ActionRead, ActionStat:
			return nil
		default:
			return ErrAccessDenied
		}
	})
}

// ReadOnlyWithLoad returns an Authorizer identical to ReadOnly but also
// permitting ActionLoad (loading and running code from a resolved file path).
//
// Like ReadOnly it applies NO path confinement; compose it with FilesystemRoot
// via All(...) to bound which paths may be read or loaded.
func ReadOnlyWithLoad() Authorizer {
	return AuthorizerFunc(func(req AccessRequest) error {
		switch req.Action {
		case ActionRead, ActionStat, ActionLoad:
			return nil
		default:
			return ErrAccessDenied
		}
	})
}
