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

	"github.com/aalpar/wile/values"
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

// ParseValueType converts a Scheme-style type name to a ValueType constant.
// Unknown names return TypeAny.
func ParseValueType(name string) values.ValueType {
	vt, ok := typeNameToValueType[name]
	if ok {
		return vt
	}
	return values.TypeAny
}

// DocInfo holds structured metadata extracted from a docstring.
type DocInfo struct {
	Doc        string
	Syntax     string // extracted from "Syntax: ..." line
	ParamNames []string
	ParamTypes []values.ValueType
	ReturnType values.ValueType
	Category   string
}

// HasStructuredMetadata reports whether any structured metadata was extracted.
func (p DocInfo) HasStructuredMetadata() bool {
	return p.Syntax != "" || len(p.ParamNames) > 0 || p.ReturnType != values.TypeAny || p.Category != ""
}

// isMetadataHeader reports whether a line starts a metadata section
// (content extracted from prose).
func isMetadataHeader(line string) bool {
	return strings.HasPrefix(line, "Syntax:") ||
		strings.HasPrefix(line, "Parameters:") ||
		strings.HasPrefix(line, "Returns:") ||
		strings.HasPrefix(line, "Category:")
}

// isProseHeader reports whether a line starts a prose section
// (content stays in Doc text).
func isProseHeader(line string) bool {
	return strings.HasPrefix(line, "Examples:") ||
		strings.HasPrefix(line, "See also:")
}

// isSectionHeader reports whether a line starts any recognized section.
func isSectionHeader(line string) bool {
	return isMetadataHeader(line) || isProseHeader(line)
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

		if isSectionHeader(line) {
			switch {
			case strings.HasPrefix(line, "Syntax:"):
				info.Syntax = strings.TrimSpace(strings.TrimPrefix(line, "Syntax:"))
				currentSection = "Syntax:"

			case strings.HasPrefix(line, "Parameters:"):
				// Parameter lines follow on subsequent indented lines.
				currentSection = "Parameters:"

			case strings.HasPrefix(line, "Returns:"):
				val := strings.TrimSpace(strings.TrimPrefix(line, "Returns:"))
				info.ReturnType = ParseValueType(val)
				currentSection = "Returns:"

			case strings.HasPrefix(line, "Category:"):
				info.Category = strings.TrimSpace(strings.TrimPrefix(line, "Category:"))
				currentSection = "Category:"

			default:
				// Prose section header — include in doc.
				docLines = append(docLines, line)
				currentSection = "prose"
			}
			continue
		}

		switch currentSection {
		case "Parameters:":
			// Parameter lines: "  name : type"
			parts := strings.SplitN(strings.TrimSpace(line), ":", 2)
			if len(parts) == 2 {
				name := strings.TrimSpace(parts[0])
				typeName := strings.TrimSpace(parts[1])
				info.ParamNames = append(info.ParamNames, name)
				info.ParamTypes = append(info.ParamTypes, ParseValueType(typeName))
			}

		case "Syntax:", "Returns:", "Category:":
			// These are single-line sections; ignore continuation lines.
			continue

		default:
			// Before any section or inside a prose section.
			docLines = append(docLines, line)
		}
	}

	info.Doc = strings.TrimRight(strings.Join(docLines, "\n"), "\n")
	return info
}
