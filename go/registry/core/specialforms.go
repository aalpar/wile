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

package core

import (
	"wile/registry"
)

// compileTimeBindings are names that exist only at compile time.
// These are handled specially by the compiler and should NOT have
// their arguments expanded by the macro expander.
var compileTimeBindings = []string{
	"if",
	"lambda",
	"case-lambda",
	"quote",
	"define",
	"define-syntax",
	"set!",
	"begin",
	"include",
	"include-ci",
	"quasiquote",
	"unquote",
	"unquote-splicing",
	"cond-expand",
	"define-for-syntax",
	"begin-for-syntax",
	"eval-when",
}

func addSpecialForms(r *registry.Registry) error {
	r.AddBindings(compileTimeBindings)
	return nil
}
