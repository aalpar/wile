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
	"errors"
	"io/fs"
	"os"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// openFilePort implements the shared logic for open-{input,output}-file and
// open-binary-{input,output}-file. It extracts the filename, checks security,
// opens the file (race-free via os.Root when the authorizer confines file
// access; see confined.go), and wraps it in a port via makePort.
func openFilePort(
	mc machine.CallContext, name string, action string,
	makePort func(*os.File) values.Value,
) error {
	filename, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, name)
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   action,
		Target:   filename.Value,
	})
	if err != nil {
		return err
	}
	file, err := confinedOpenForAction(mc.Authorizer(), filename.Value, action)
	if err != nil {
		return werr.WrapForeignFileError(err, name, filename.Value)
	}
	mc.SetValue(makePort(file))
	return nil
}

// PrimOpenInputFile implements the open-input-file primitive.
// Opens a file for reading and returns an input port.
func PrimOpenInputFile(mc machine.CallContext) error {
	return openFilePort(mc, "open-input-file", security.ActionRead, func(f *os.File) values.Value {
		return values.NewCharacterInputPortFromReader(f)
	})
}

// PrimOpenOutputFile implements the open-output-file primitive.
// Opens a file for writing and returns an output port.
func PrimOpenOutputFile(mc machine.CallContext) error {
	return openFilePort(mc, "open-output-file", security.ActionWrite, func(f *os.File) values.Value {
		return values.NewCharacterOutputPortFromWriter(f)
	})
}

// PrimOpenBinaryInputFile implements the open-binary-input-file primitive (R7RS).
// Opens a file for binary reading and returns a binary input port.
func PrimOpenBinaryInputFile(mc machine.CallContext) error {
	return openFilePort(mc, "open-binary-input-file", security.ActionRead, func(f *os.File) values.Value {
		return values.NewBinaryInputPortFromReader(f)
	})
}

// PrimOpenBinaryOutputFile implements the open-binary-output-file primitive (R7RS).
// Opens a file for binary writing and returns a binary output port.
func PrimOpenBinaryOutputFile(mc machine.CallContext) error {
	return openFilePort(mc, "open-binary-output-file", security.ActionWrite, func(f *os.File) values.Value {
		return values.NewBinaryOutputPortFromWriter(f)
	})
}

// statAnswersFalse reports whether a stat failure is one file-exists? answers #f
// for rather than raising.
//
// Three sources, one answer, and the classifier must accept all three or the
// split is wrong in a way tests do not see. ErrNotExist is the ordinary absent
// file. ErrPermission is the OS refusing the lookup — EACCES on an unreadable
// parent, EPERM. ErrAccessDenied is this engine's own authorizer refusing it,
// which reaches here (rather than through the gate above) when confinedStat
// re-gates a resolved symlink target. Keying on the sentinel alone would let an
// EACCES raise; keying on the errno alone would let the re-gate raise.
//
// Everything else — EIO, ELOOP, ENAMETOOLONG, a timeout — says nothing about
// whether the file is there, so it is not a #f and must not be reported as one.
func statAnswersFalse(err error) bool {
	return errors.Is(err, fs.ErrNotExist) ||
		errors.Is(err, fs.ErrPermission) ||
		errors.Is(err, security.ErrAccessDenied)
}

// PrimFileExistsQ implements the (file-exists?) primitive.
// Returns #t if the file exists and this engine is permitted to see it.
//
// DENIAL AND ABSENCE ARE DELIBERATELY INDISTINGUISHABLE. A path the authorizer
// refuses, and a path the OS refuses to stat, both answer #f — the same answer
// a path that is not there gets. That is not a rounding error to be fixed back:
// R7RS §6.14 gives this procedure a boolean and no error clause, an authorizer
// violation does not present itself as an error (it stays in the authorizer's
// domain and escapes to the host that installed it, not to the sandboxed
// program), and a predicate that distinguished the two would hand a sandboxed
// program an oracle for the filesystem outside its sandbox. A caller that needs
// to know WHY is not served by this procedure; a lower-level call that reports
// the reason is anticipated and is not built here.
//
// The refusals are not the whole error space. A system error that carries no
// information about existence — EIO, ELOOP, ENAMETOOLONG, a timeout — raises,
// because reporting it as #f would claim the file is absent on evidence that
// says nothing of the kind. See statAnswersFalse for where the line falls.
func PrimFileExistsQ(mc machine.CallContext) error {
	filename, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "file-exists?")
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionStat,
		Target:   filename.Value,
	})
	if err != nil {
		// Deliberately dropped, and this is the whole point of the item: the
		// denial IS the answer, rendered as #f. Returning it would report an
		// authorizer decision to the sandboxed program, which is not the party
		// it is addressed to. Unconditional, not statAnswersFalse-gated — an
		// authorizer that denies with an unexpected cause is still a denial,
		// and must not become the one refusal that raises.
		mc.SetValue(values.FalseValue)
		return nil //nolint:nilerr // a denial is reported as #f, never as an error
	}
	_, err = confinedStat(mc.Authorizer(), filename.Value)
	if err != nil && !statAnswersFalse(err) {
		return werr.WrapForeignFileError(err, "file-exists?", filename.Value)
	}
	mc.SetValue(values.BoolToBoolean(err == nil))
	return nil
}

// PrimDeleteFile implements the (delete-file) primitive.
// Deletes a file from the filesystem.
func PrimDeleteFile(mc machine.CallContext) error {
	filename, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "delete-file")
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionDelete,
		Target:   filename.Value,
	})
	if err != nil {
		return err
	}
	err = confinedRemove(mc.Authorizer(), filename.Value)
	if err != nil {
		return werr.WrapForeignFileError(err, "delete-file", filename.Value)
	}
	mc.SetValue(values.Void)
	return nil
}

// callWithFile is a helper for call-with-input-file and call-with-output-file.
// Takes filename at index 0, proc at index 1. Opens file, creates port,
// calls proc, closes file, returns all values from proc.
//
//nolint:unparam
func callWithFile(
	mc *machine.MachineContext,
	name string,
	action string,
	portCreator func(*os.File) values.Value,
) error {
	filename, err := helpers.RequireType[*values.String](mc.Arg(0), werr.ErrNotAString, name)
	if err != nil {
		return err
	}

	proc, err := helpers.RequireCallable(mc.Arg(1), name)
	if err != nil {
		return err
	}

	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   action,
		Target:   filename.Value,
	})
	if err != nil {
		return err
	}

	file, err := confinedOpenForAction(mc.Authorizer(), filename.Value, action)
	if err != nil {
		return werr.WrapForeignFileError(err, name, filename.Value)
	}
	defer file.Close() //nolint:errcheck

	port := portCreator(file)

	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	_, err = sub.ApplyCallable(proc, port)
	if err != nil {
		return err
	}
	err = sub.RunWithinBoundary()
	if err != nil {
		return err
	}

	mc.SetValues(sub.GetValues()...)
	return nil
}

// PrimCallWithInputFile implements the call-with-input-file primitive.
func PrimCallWithInputFile(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "call-with-input-file")
	if err != nil {
		return err
	}
	return callWithFile(mc, "call-with-input-file", security.ActionRead,
		func(f *os.File) values.Value {
			return values.NewCharacterInputPortFromReader(f)
		})
}

// PrimCallWithOutputFile implements the call-with-output-file primitive.
func PrimCallWithOutputFile(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "call-with-output-file")
	if err != nil {
		return err
	}
	return callWithFile(mc, "call-with-output-file", security.ActionWrite,
		func(f *os.File) values.Value {
			return values.NewCharacterOutputPortFromWriter(f)
		})
}

// PrimWithInputFromFile and PrimWithOutputToFile have been moved to
// with_file_macros.scm as macros using parameterize. This ensures proper
// integration with the continuation system (fixes T3 from architectural review).
