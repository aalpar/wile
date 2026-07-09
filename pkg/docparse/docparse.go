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

// Package docparse parses structured metadata from Guile-style docstrings.
//
// Docstrings may contain metadata sections (Syntax:, Parameters:, Returns:,
// Category:) that are extracted into structured fields, and prose sections
// (Examples:, See also:) that remain in the Doc text.
package docparse

import (
	"strings"

	"github.com/aalpar/wile/pkg/values"
)

// typeNameToValueType maps Scheme-style type names to ValueType constants.
// Built from values.ValueType.String() at init time.
var typeNameToValueType map[string]values.ValueType

func init() {
	typeNameToValueType = make(map[string]values.ValueType)
	for vt := range values.TypeCount {
		name := vt.String()
		if name != "" && name != "unknown" {
			typeNameToValueType[name] = vt
		}
	}
}

// ParseValueType converts a Scheme-style type name to a TypeConstraint.
// Known names return the corresponding ValueType constant.
// Unknown names return a NamedTypeConstraint preserving the original name.
// Empty names return nil (unspecified).
func ParseValueType(name string) values.TypeConstraint {
	if name == "" {
		return nil
	}
	vt, ok := typeNameToValueType[name]
	if ok {
		return vt
	}
	return values.NewNamedTypeConstraint(name)
}

// DocInfo holds structured metadata extracted from a docstring.
type DocInfo struct {
	Doc        string
	Syntax     string // extracted from "Syntax: ..." line
	ParamNames []string
	ParamTypes []values.TypeConstraint
	ReturnType values.TypeConstraint
	Category   string
	Keywords   []string
}

// HasStructuredMetadata reports whether any structured metadata was extracted.
func (p DocInfo) HasStructuredMetadata() bool {
	return p.Syntax != "" || len(p.ParamNames) > 0 || p.ReturnType != nil || p.Category != "" || len(p.Keywords) > 0
}

// parametersPrefix is the one metadata section whose entries span multiple
// indented continuation lines; every other metadata header carries its value
// inline on the header line. Named so the sectionHeaders row and the
// continuation-line dispatch in ParseDocstring reference the same string.
const parametersPrefix = "Parameters:"

// sectionKind distinguishes docstring sections whose content is extracted into
// structured DocInfo fields (metadata) from those left in the prose Doc text.
type sectionKind int

const (
	kindMetadata sectionKind = iota
	kindProse
)

// sectionHeader describes one recognized docstring section header: its line
// prefix, its kind, and — for a metadata header carrying an inline value — how
// to apply that value to the DocInfo.
type sectionHeader struct {
	prefix string
	kind   sectionKind
	// apply extracts the header line's inline value into info. nil when the
	// header carries no inline value: prose headers, and Parameters, whose
	// entries follow on subsequent indented lines.
	apply func(info *DocInfo, value string)
}

// sectionHeaders is the single source of truth for the docstring section
// vocabulary. The header predicate (matchSectionHeader) and the ParseDocstring
// dispatch both derive from it, so adding a section is one row — not a new
// prefix string copied across a predicate and a parse switch.
var sectionHeaders = []sectionHeader{
	{prefix: "Syntax:", kind: kindMetadata, apply: func(info *DocInfo, value string) {
		info.Syntax = value
	}},
	{prefix: parametersPrefix, kind: kindMetadata},
	{prefix: "Returns:", kind: kindMetadata, apply: func(info *DocInfo, value string) {
		info.ReturnType = ParseValueType(value)
	}},
	{prefix: "Category:", kind: kindMetadata, apply: func(info *DocInfo, value string) {
		info.Category = value
	}},
	{prefix: "Keywords:", kind: kindMetadata, apply: func(info *DocInfo, value string) {
		info.Keywords = parseKeywords(value)
	}},
	{prefix: "Examples:", kind: kindProse},
	{prefix: "See also:", kind: kindProse},
}

// matchSectionHeader finds the section whose prefix begins line, returning the
// header, the trimmed inline value following the prefix, and true. Search
// follows sectionHeaders order.
func matchSectionHeader(line string) (sectionHeader, string, bool) {
	for _, h := range sectionHeaders {
		after, found := strings.CutPrefix(line, h.prefix)
		if found {
			return h, strings.TrimSpace(after), true
		}
	}
	return sectionHeader{}, "", false
}

// parseKeywords splits a comma-separated Keywords: value into trimmed,
// non-empty keyword strings.
func parseKeywords(raw string) []string {
	parts := strings.Split(raw, ",")
	keywords := make([]string, 0, len(parts))
	for _, part := range parts {
		trimmed := strings.TrimSpace(part)
		if trimmed != "" {
			keywords = append(keywords, trimmed)
		}
	}
	return keywords
}

// ParseDocstring parses a raw docstring into structured metadata.
// Metadata sections (Syntax:, Parameters:, Returns:, Category:) are extracted
// into typed fields. Prose sections (Examples:, See also:) remain in the Doc text.
func ParseDocstring(raw string) DocInfo {
	if raw == "" {
		return DocInfo{}
	}

	lines := strings.Split(raw, "\n")

	var info DocInfo
	var docLines []string
	var currentSection string

	for _, line := range lines {
		// Blank lines end metadata sections and return to prose.
		// This preserves blank-line separators before prose sections
		// like "Examples:" so that StripExamples can find "\n\nExamples:\n".
		if strings.TrimSpace(line) == "" && currentSection != "" && currentSection != "prose" {
			docLines = append(docLines, line)
			currentSection = ""
			continue
		}

		hdr, value, ok := matchSectionHeader(line)
		if ok {
			if hdr.kind == kindProse {
				// Prose section header — include in doc.
				docLines = append(docLines, line)
				currentSection = "prose"
			} else {
				if hdr.apply != nil {
					hdr.apply(&info, value)
				}
				currentSection = hdr.prefix
			}
			continue
		}

		switch currentSection {
		case parametersPrefix:
			// Parameter lines: "  name : type"
			parts := strings.SplitN(strings.TrimSpace(line), ":", 2)
			if len(parts) == 2 {
				name := strings.TrimSpace(parts[0])
				typeName := strings.TrimSpace(parts[1])
				info.ParamNames = append(info.ParamNames, name)
				info.ParamTypes = append(info.ParamTypes, ParseValueType(typeName))
			}

		case "", "prose":
			// Before any section or inside a prose section: keep the line.
			docLines = append(docLines, line)

		default:
			// A single-line metadata section (Syntax/Returns/Category/Keywords):
			// its value was consumed from the header line; ignore continuations.
			continue
		}
	}

	info.Doc = strings.TrimRight(strings.Join(docLines, "\n"), "\n")
	return info
}
