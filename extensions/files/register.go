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

	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
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
var Extension = registry.NewDescribedExtension("files",
	"Filesystem operations: file I/O, directory traversal, temporary files.",
	AddToRegistry)

// Builder aggregates all file registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives, addMacros)

// AddToRegistry registers all file primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "open-input-file", ParamCount: 1, Impl: PrimOpenInputFile,
			Doc: "Opens FILENAME for textual input and returns a character input port.", ParamNames: []string{"filename"}, Category: "files",
			ParamTypes: []values.TypeConstraint{values.TypeString},
			ReturnType: values.TypeTextualInputPort},
		{Name: "open-output-file", ParamCount: 1, Impl: PrimOpenOutputFile,
			Doc: "Opens FILENAME for textual output and returns a character output port. Truncates existing files.", ParamNames: []string{"filename"}, Category: "files",
			ParamTypes: []values.TypeConstraint{values.TypeString},
			ReturnType: values.TypeTextualOutputPort},
		{Name: "open-binary-input-file", ParamCount: 1, Impl: PrimOpenBinaryInputFile,
			Doc: "Opens FILENAME for binary input and returns a binary input port.", ParamNames: []string{"filename"}, Category: "files",
			ParamTypes: []values.TypeConstraint{values.TypeString},
			ReturnType: values.TypeBinaryInputPort},
		{Name: "open-binary-output-file", ParamCount: 1, Impl: PrimOpenBinaryOutputFile,
			Doc: "Opens FILENAME for binary output and returns a binary output port.", ParamNames: []string{"filename"}, Category: "files",
			ParamTypes: []values.TypeConstraint{values.TypeString},
			ReturnType: values.TypeBinaryOutputPort},
		{Name: "file-exists?", ParamCount: 1, Impl: PrimFileExistsQ,
			Doc: "Returns #t if a file or directory exists at FILENAME.", ParamNames: []string{"filename"}, Category: "files",
			ParamTypes: []values.TypeConstraint{values.TypeString},
			ReturnType: values.TypeBoolean},
		{Name: "delete-file", ParamCount: 1, Impl: PrimDeleteFile,
			Doc: "Deletes FILENAME. Raises a file error if the file does not exist.", ParamNames: []string{"filename"}, Category: "files",
			Keywords:   []string{"remove", "unlink", "erase"},
			ParamTypes: []values.TypeConstraint{values.TypeString},
			ReturnType: values.TypeVoid},
		{Name: "call-with-input-file", ParamCount: 2, Impl: PrimCallWithInputFile,
			Doc: "Opens FILENAME for input, calls PROC with the port, then closes it. The port is closed even if PROC raises an error.", ParamNames: []string{"filename", "proc"}, Category: "files",
			ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeProcedure},
			ReturnType: values.TypeAny},
		{Name: "call-with-output-file", ParamCount: 2, Impl: PrimCallWithOutputFile,
			Doc: "Opens FILENAME for output, calls PROC with the port, then closes it. The port is closed even if PROC raises an error.", ParamNames: []string{"filename", "proc"}, Category: "files",
			ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeProcedure},
			ReturnType: values.TypeAny},
		{Name: "create-directory", ParamCount: 1, Impl: PrimCreateDirectory,
			Doc: "Creates a new directory at PATH. Raises a file error if the directory already exists.", ParamNames: []string{"path"}, Category: "files",
			Keywords:   []string{"mkdir", "make directory", "create folder"},
			ParamTypes: []values.TypeConstraint{values.TypeString},
			ReturnType: values.TypeVoid},
		{Name: "delete-directory", ParamCount: 1, Impl: PrimDeleteDirectory,
			Doc: "Removes the directory at PATH. The directory must be empty.", ParamNames: []string{"path"}, Category: "files",
			Keywords:   []string{"rmdir", "remove directory"},
			ParamTypes: []values.TypeConstraint{values.TypeString},
			ReturnType: values.TypeVoid},
		{Name: "directory-files", ParamCount: 1, Impl: PrimDirectoryFiles,
			Doc: "Returns a list of filenames (strings) in the directory at PATH.", ParamNames: []string{"path"}, Category: "files",
			Keywords:   []string{"ls", "list directory", "readdir"},
			ParamTypes: []values.TypeConstraint{values.TypeString},
			ReturnType: values.TypeList},
		{Name: "current-directory", Impl: PrimCurrentDirectory,
			Doc: "Returns the current working directory as a string.", Category: "files",
			Keywords:   []string{"pwd", "getcwd", "working directory"},
			ReturnType: values.TypeString},
		{Name: "set-current-directory!", ParamCount: 1, Impl: PrimSetCurrentDirectory,
			Doc: "Changes the current working directory to PATH.", ParamNames: []string{"path"}, Category: "files",
			Keywords:   []string{"cd", "chdir", "change directory"},
			ParamTypes: []values.TypeConstraint{values.TypeString},
			ReturnType: values.TypeVoid},
		// with-input-from-file and with-output-to-file are now macros (see addMacros)
	}, registry.PhaseSetRuntime)
	return nil
}

func addMacros(r *registry.Registry) error {
	r.AddMacroSource(withFileMacroSource)
	r.AddDocumentation("with-input-from-file",
		"Open FILENAME for input and call THUNK with current-input-port\nbound to the opened port. Closes the port when THUNK returns.\n\nCategory: files")
	r.AddDocumentation("with-output-to-file",
		"Open FILENAME for output and call THUNK with current-output-port\nbound to the opened port. Closes the port when THUNK returns.\n\nCategory: files")
	return nil
}
