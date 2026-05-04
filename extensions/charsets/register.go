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

package charsets

import (
	"github.com/aalpar/wile/registry"
)

// Extension is the SRFI-14 Character-Set Library extension.
var Extension = registry.NewDescribedExtension("charsets",
	"SRFI-14 Character-Set Library: char-set value type, set algebra, named char-sets.",
	AddToRegistry)

// Builder aggregates all charsets registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all charset primitives.
var AddToRegistry = Builder.AddToRegistry
