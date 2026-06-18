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

package files

import (
	"os"
	"slices"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// PrimCreateDirectory implements the (create-directory) primitive.
// Creates a single directory level. Errors if it already exists or
// the parent is missing (no recursive mkdir -p behavior).
func PrimCreateDirectory(mc machine.CallContext) error {
	path, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "create-directory")
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionWrite,
		Target:   path.Value,
	})
	if err != nil {
		return err
	}
	err = confinedMkdir(mc.Authorizer(), path.Value, 0o755)
	if err != nil {
		return werr.WrapForeignFileError(err, "create-directory", path.Value)
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimDeleteDirectory implements the (delete-directory) primitive.
// Removes an empty directory. Errors if not empty, nonexistent, or not a directory.
func PrimDeleteDirectory(mc machine.CallContext) error {
	path, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "delete-directory")
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionDelete,
		Target:   path.Value,
	})
	if err != nil {
		return err
	}
	info, err := confinedStat(mc.Authorizer(), path.Value)
	if err != nil {
		return werr.WrapForeignFileError(err, "delete-directory", path.Value)
	}
	if !info.IsDir() {
		return werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"delete-directory: not a directory: %s", path.Value,
		)
	}
	err = confinedRemove(mc.Authorizer(), path.Value)
	if err != nil {
		return werr.WrapForeignFileError(err, "delete-directory", path.Value)
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimDirectoryFiles implements the (directory-files) primitive.
// Returns a list of filename strings in the directory, excluding "." and "..".
// Names are filenames only (not full paths).
func PrimDirectoryFiles(mc machine.CallContext) error {
	path, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "directory-files")
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionRead,
		Target:   path.Value,
	})
	if err != nil {
		return err
	}
	entries, err := confinedReadDir(mc.Authorizer(), path.Value)
	if err != nil {
		return werr.WrapForeignFileError(err, "directory-files", path.Value)
	}
	list := values.EmptyList
	for i := range slices.Backward(entries) {
		name := entries[i].Name()
		if name == "." || name == ".." {
			continue
		}
		list = values.NewCons(values.NewString(name), list)
	}
	mc.SetValue(list)
	return nil
}

// PrimCurrentDirectory implements the (current-directory) primitive.
// Returns the current working directory as a string.
func PrimCurrentDirectory(mc machine.CallContext) error {
	wd, err := os.Getwd()
	if err != nil {
		return werr.WrapForeignFileError(err, "current-directory", ".")
	}
	// Gate the working directory as a file stat so a confining authorizer can
	// withhold it: DenyAll denies it (honouring "deny everything"), and
	// FilesystemRoot denies it when the cwd lies outside the root (preventing a
	// host-path leak out of the sandbox). The check runs after Getwd so the path
	// is available to evaluate but before SetValue so a denial leaks nothing.
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionStat,
		Target:   wd,
	})
	if err != nil {
		return err
	}
	mc.SetValue(values.NewString(wd))
	return nil
}

// PrimSetCurrentDirectory implements the (set-current-directory!) primitive.
//
// WARNING: os.Chdir is process-global. Multiple engines in the same Go
// process share a single working directory. Concurrent calls from different
// goroutines race on the same OS state. This is inherent to POSIX — there
// is no per-thread working directory.
func PrimSetCurrentDirectory(mc machine.CallContext) error {
	path, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "set-current-directory!")
	if err != nil {
		return err
	}
	// Gate as a file write of the destination path (not an opaque
	// {process,write,"cwd"} request, which path-confining authorizers like
	// FilesystemRoot never inspect — letting chdir escape the root). With
	// ResourceFile+Target=path, containment authorizers evaluate the
	// destination, confining the new working directory to the sandbox root.
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionWrite,
		Target:   path.Value,
	})
	if err != nil {
		return err
	}
	err = os.Chdir(path.Value)
	if err != nil {
		return werr.WrapForeignFileError(err, "set-current-directory!", path.Value)
	}
	mc.SetValue(values.Void)
	return nil
}
