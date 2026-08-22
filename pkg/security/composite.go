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

import "slices"

// All returns an Authorizer that requires every authorizer in the list
// to allow the operation. The first denial short-circuits and its error
// is returned. An empty list allows everything.
func All(authorizers ...Authorizer) Authorizer {
	if len(authorizers) == 1 {
		return authorizers[0]
	}
	cpy := slices.Clone(authorizers)
	return &compositeAuthorizer{authorizers: cpy}
}

type compositeAuthorizer struct {
	authorizers []Authorizer
}

func (p *compositeAuthorizer) Authorize(req AccessRequest) error {
	for _, auth := range p.authorizers {
		err := auth.Authorize(req)
		if err != nil {
			return err
		}
	}
	return nil
}
