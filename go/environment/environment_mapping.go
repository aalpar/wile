package environment

import (
	"fmt"
	"skeme/values"
)

type ExportSpec interface {
	Next() ExportSpec
}

type ImportSpec interface {
	Next() ImportSpec
}

// OnlyExportDirective indicates that only the given identifier should be exported.
type OnlyExportDirective struct {
	next ExportSpec
	// only identifier to export
	only values.Symbol
}

func (p *OnlyExportDirective) Next() ExportSpec {
	return p.next
}

type RenameExportDirective struct {
	next ExportSpec
	from values.Symbol
	to   values.Symbol
}

func (p *RenameExportDirective) Next() ExportSpec {
	return p.next
}

// LibraryImportDirective indicates that all identifiers from the given library should be imported.
type LibraryImportDirective struct {
	next ImportSpec
	// library to import from
	library values.Symbol
}

func (p *LibraryImportDirective) Next() ImportSpec {
	return p.next
}

// OnlyImportDirective indicates that only the given identifier should be imported.
type OnlyImportDirective struct {
	next ImportSpec
	// only identifier to import
	only values.Symbol
}

func (p *OnlyImportDirective) Next() ImportSpec {
	return p.next
}

// ExceptImportDirective indicates that the given identifier should not be imported.
type ExceptImportDirective struct {
	next ImportSpec
	// exclude identifier from import
	except values.Symbol
}

func (p *ExceptImportDirective) Next() ImportSpec {
	return p.next
}

// PrefixImportDirective indicates that any identifier with the given prefix should be imported.
type PrefixImportDirective struct {
	next ImportSpec
	// any identifier with this prefix to import
	prefix values.Symbol
}

func (p *PrefixImportDirective) Next() ImportSpec {
	return p.next
}

type RenameImportDirective struct {
	next ImportSpec
	from values.Symbol
	to   values.Symbol
}

func (p *RenameImportDirective) Next() ImportSpec {
	return p.next
}

type ExportSet struct {
	exports map[values.Symbol]values.Symbol
}

type ImportSet struct {
	imports map[values.Symbol]values.Symbol
}

func NewExportSet(spec ExportSpec) (*ExportSet, error) {
	q := &ExportSet{}
	for spec != nil {
		switch spec.(type) {
		case *RenameExportDirective:
		case *OnlyExportDirective:
		default:
			return nil, fmt.Errorf("unsupported spec type: %T", spec)
		}
		spec = spec.Next()
	}
	return q, nil
}

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
			return nil, fmt.Errorf("unsupported spec type: %T", spec)
		}
		spec = spec.Next()
	}
	return q, nil
}
