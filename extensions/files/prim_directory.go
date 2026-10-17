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
	"io/fs"
	"os"
	"path/filepath"
	"syscall"

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
	abs, err := enginePath(mc, "create-directory", path)
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionWrite,
		Target:   abs,
	})
	if err != nil {
		return err
	}
	err = confinedMkdir(mc.Authorizer(), abs, 0o755)
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
	abs, err := enginePath(mc, "delete-directory", path)
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionDelete,
		Target:   abs,
	})
	if err != nil {
		return err
	}
	info, err := confinedStat(mc.Authorizer(), abs)
	if err != nil {
		return werr.WrapForeignFileError(err, "delete-directory", path.Value)
	}
	if !info.IsDir() {
		return werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"delete-directory: not a directory: %s", path.Value,
		)
	}
	err = confinedRemove(mc.Authorizer(), abs)
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
	abs, err := enginePath(mc, "directory-files", path)
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionRead,
		Target:   abs,
	})
	if err != nil {
		return err
	}
	entries, err := confinedReadDir(mc.Authorizer(), abs)
	if err != nil {
		return werr.WrapForeignFileError(err, "directory-files", path.Value)
	}
	names := make([]values.Value, 0, len(entries))
	for _, entry := range entries {
		name := entry.Name()
		if name == "." || name == ".." {
			continue
		}
		names = append(names, values.NewString(name))
	}
	mc.SetValue(values.List(names...))
	return nil
}

// PrimCurrentDirectory implements the (current-directory) primitive.
// Returns the engine's current directory as a string: the one
// set-current-directory! last entered, else the host process's working
// directory (environment.Namespace.CurrentDirectory).
func PrimCurrentDirectory(mc machine.CallContext) error {
	wd, err := mc.EnvironmentFrame().Namespace().CurrentDirectory()
	if err != nil {
		return werr.WrapForeignFileError(err, "current-directory", ".")
	}
	// Gate the working directory as a file stat so a confining authorizer can
	// withhold it: DenyAll denies it (honouring "deny everything"), and
	// FilesystemRoot denies it when the directory lies outside the root
	// (preventing a host-path leak out of the sandbox). The check runs after the
	// read so the path is available to evaluate but before SetValue so a denial
	// leaks nothing.
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
// The current directory is ENGINE state (environment.Namespace.
// SetCurrentDirectory), not the process's: entering a directory moves this
// engine and every namespace it owns, never the host or another engine. Every
// relative path the engine later resolves (the file primitives, load/include,
// a spawned process's start directory) is resolved against it.
func PrimSetCurrentDirectory(mc machine.CallContext) error {
	path, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "set-current-directory!")
	if err != nil {
		return err
	}
	abs, err := enginePath(mc, "set-current-directory!", path)
	if err != nil {
		return err
	}
	// Gate as a file write of the destination path (not an opaque
	// {process,write,"cwd"} request, which path-confining authorizers like
	// FilesystemRoot never inspect — letting the directory escape the root).
	// With ResourceFile+Target=path, containment authorizers evaluate the
	// destination, confining the new current directory to the sandbox root.
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionWrite,
		Target:   abs,
	})
	if err != nil {
		return err
	}
	// Record the symlink-resolved path, re-gated when it differs from the
	// spelling, so the directory entered is the one judged and ".." after a
	// symlink is applied after the link, as chdir would.
	target, err := unconfinedTarget(mc.Authorizer(), abs, security.ActionWrite)
	if err != nil {
		return err
	}
	// The stat stands in for the refusals chdir made: a missing path, one that
	// is not a directory, and one without search permission. Statting target
	// itself needs x only on its parents; resolving target/. needs it on target
	// too, which is chdir's own requirement. Neither confinedStat nor
	// filepath.Join may build the path, since both clean the "/." away. The bare
	// stat only decides whether to record target, which is already gated, and
	// every later access below it goes through the confined* helpers.
	info, err := os.Stat(target + string(filepath.Separator) + ".")
	if err != nil {
		return werr.WrapForeignFileError(err, "set-current-directory!", path.Value)
	}
	if !info.IsDir() {
		return werr.WrapForeignFileError(
			&fs.PathError{Op: "stat", Path: target, Err: syscall.ENOTDIR},
			"set-current-directory!", path.Value)
	}
	mc.EnvironmentFrame().Namespace().SetCurrentDirectory(target)
	mc.SetValue(values.Void)
	return nil
}
