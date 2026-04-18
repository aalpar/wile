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

import (
	"path/filepath"
	"strings"
)

// ConsoleAuthorizer returns an Authorizer for the Console profile.
// File operations are restricted to /tmp. Environment variable reads
// are allowed (the envvars primitive handles virtual-vs-OS routing).
// Code loading and process execution are denied.
func ConsoleAuthorizer() Authorizer {
	return AuthorizerFunc(func(req AccessRequest) error {
		switch req.Resource {
		case ResourceFile:
			cleaned := filepath.Clean(req.Target)
			if !strings.HasPrefix(cleaned, "/tmp/") && cleaned != "/tmp" {
				return ErrAccessDenied
			}
			return nil
		case ResourceEnv:
			return nil
		default:
			return ErrAccessDenied
		}
	})
}
