// Copyright 2025 Aaron Alpar
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

package environment

import (
	"github.com/aalpar/wile/values"
)

// ExportSpec is the interface for export directives in library definitions.
type ExportSpec interface {
	Next() ExportSpec
}

// ImportSpec is the interface for import directives in library definitions.
type ImportSpec interface {
	Next() ImportSpec
}

// OnlyExportDirective indicates that only the given identifier should be exported.
type OnlyExportDirective struct {
	next ExportSpec
	// only identifier to export
	only values.Symbol
}

// Next returns the next export spec in the chain.
func (p *OnlyExportDirective) Next() ExportSpec {
	return p.next
}

// RenameExportDirective indicates that an identifier should be exported under a different name.
type RenameExportDirective struct {
	next ExportSpec
	from values.Symbol
	to   values.Symbol
}

// Next returns the next export spec in the chain.
func (p *RenameExportDirective) Next() ExportSpec {
	return p.next
}

// LibraryImportDirective indicates that all identifiers from the given library should be imported.
type LibraryImportDirective struct {
	next ImportSpec
	// library to import from
	library values.Symbol
}

// Next returns the next import spec in the chain.
func (p *LibraryImportDirective) Next() ImportSpec {
	return p.next
}

// OnlyImportDirective indicates that only the given identifier should be imported.
type OnlyImportDirective struct {
	next ImportSpec
	// only identifier to import
	only values.Symbol
}

// Next returns the next import spec in the chain.
func (p *OnlyImportDirective) Next() ImportSpec {
	return p.next
}

// ExceptImportDirective indicates that the given identifier should not be imported.
type ExceptImportDirective struct {
	next ImportSpec
	// exclude identifier from import
	except values.Symbol
}

// Next returns the next import spec in the chain.
func (p *ExceptImportDirective) Next() ImportSpec {
	return p.next
}

// PrefixImportDirective indicates that any identifier with the given prefix should be imported.
type PrefixImportDirective struct {
	next ImportSpec
	// any identifier with this prefix to import
	prefix values.Symbol
}

// Next returns the next import spec in the chain.
func (p *PrefixImportDirective) Next() ImportSpec {
	return p.next
}

// RenameImportDirective indicates that an identifier should be imported under a different name.
type RenameImportDirective struct {
	next ImportSpec
	from values.Symbol
	to   values.Symbol
}

// Next returns the next import spec in the chain.
func (p *RenameImportDirective) Next() ImportSpec {
	return p.next
}

// ExportSet holds the resolved export mappings for a library.
type ExportSet struct{}

// ImportSet holds the resolved import mappings for a library.
type ImportSet struct{}

// NewExportSet creates an ExportSet from a chain of export specs.
func NewExportSet(spec ExportSpec) (*ExportSet, error) {
	q := &ExportSet{}
	for spec != nil {
		switch spec.(type) {
		case *RenameExportDirective:
		case *OnlyExportDirective:
		default:
			return nil, values.WrapForeignErrorf(values.ErrInvalidArgument, "unsupported spec type: %T", spec)
		}
		spec = spec.Next()
	}
	return q, nil
}

// NewImportSet creates an ImportSet from a chain of import specs.
func NewImportSet(spec ImportSpec) (*ImportSet, error) {
	q := &ImportSet{}
	for spec != nil {
		switch spec.(type) {
		case *RenameImportDirective:
		case *OnlyImportDirective:
		case *ExceptImportDirective:
		case *PrefixImportDirective:
		case *LibraryImportDirective:
		default:
			return nil, values.WrapForeignErrorf(values.ErrInvalidArgument, "unsupported spec type: %T", spec)
		}
		spec = spec.Next()
	}
	return q, nil
}
