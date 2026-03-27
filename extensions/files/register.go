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

// Package files provides file I/O primitives.
package files

import (
	_ "embed"

	"github.com/aalpar/wile/registry"
)

// withFileMacroSource contains with-input-from-file and with-output-to-file macros.
// These are implemented as macros using parameterize to ensure proper integration
// with the continuation system (fixes T3 from architectural review).
//
// Source: go/extensions/files/with_file_macros.scm (embedded at compile-time)
//
//go:embed with_file_macros.scm
var withFileMacroSource string

// Extension is the file I/O extension.
var Extension = registry.NewExtension("files", AddToRegistry)

// Builder aggregates all file registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives, addMacros)

// AddToRegistry registers all file primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "open-input-file", ParamCount: 1, Impl: PrimOpenInputFile,
			Doc: "Opens filename for textual input and returns a character input port.", ParamNames: []string{"filename"}, Category: "files"},
		{Name: "open-output-file", ParamCount: 1, Impl: PrimOpenOutputFile,
			Doc: "Opens filename for textual output and returns a character output port. Truncates existing files.", ParamNames: []string{"filename"}, Category: "files"},
		{Name: "open-binary-input-file", ParamCount: 1, Impl: PrimOpenBinaryInputFile,
			Doc: "Opens filename for binary input and returns a binary input port.", ParamNames: []string{"filename"}, Category: "files"},
		{Name: "open-binary-output-file", ParamCount: 1, Impl: PrimOpenBinaryOutputFile,
			Doc: "Opens filename for binary output and returns a binary output port.", ParamNames: []string{"filename"}, Category: "files"},
		{Name: "file-exists?", ParamCount: 1, Impl: PrimFileExistsQ,
			Doc: "Returns #t if a file or directory exists at the given path.", ParamNames: []string{"filename"}, Category: "files"},
		{Name: "delete-file", ParamCount: 1, Impl: PrimDeleteFile,
			Doc: "Deletes the named file. Raises a file error if the file does not exist.", ParamNames: []string{"filename"}, Category: "files"},
		{Name: "call-with-input-file", ParamCount: 2, Impl: PrimCallWithInputFile,
			Doc: "Opens filename for input, calls proc with the port, then closes it. The port is closed even if proc raises an error.", ParamNames: []string{"filename", "proc"}, Category: "files"},
		{Name: "call-with-output-file", ParamCount: 2, Impl: PrimCallWithOutputFile,
			Doc: "Opens filename for output, calls proc with the port, then closes it. The port is closed even if proc raises an error.", ParamNames: []string{"filename", "proc"}, Category: "files"},
		{Name: "create-directory", ParamCount: 1, Impl: PrimCreateDirectory,
			Doc: "Creates a new directory at path. Raises a file error if the directory already exists.", ParamNames: []string{"path"}, Category: "files"},
		{Name: "delete-directory", ParamCount: 1, Impl: PrimDeleteDirectory,
			Doc: "Removes the directory at path. The directory must be empty.", ParamNames: []string{"path"}, Category: "files"},
		{Name: "directory-files", ParamCount: 1, Impl: PrimDirectoryFiles,
			Doc: "Returns a list of filenames (strings) in the directory at path.", ParamNames: []string{"path"}, Category: "files"},
		{Name: "current-directory", Impl: PrimCurrentDirectory,
			Doc: "Returns the current working directory as a string.", Category: "files"},
		{Name: "set-current-directory!", ParamCount: 1, Impl: PrimSetCurrentDirectory,
			Doc: "Changes the current working directory to path.", ParamNames: []string{"path"}, Category: "files"},
		// with-input-from-file and with-output-to-file are now macros (see addMacros)
	}, registry.PhaseRuntime)
	return nil
}

func addMacros(r *registry.Registry) error {
	r.AddMacroSource(withFileMacroSource)
	return nil
}
