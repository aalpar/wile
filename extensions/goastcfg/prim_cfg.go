package goastcfg

import (
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimGoCFG implements (go-cfg pattern func-name . options).
// Stub: validates args, returns empty list. Filled in by Task 2.
func PrimGoCFG(mc *machine.MachineContext) error {
	_, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "go-cfg")
	if err != nil {
		return err
	}
	_, err = helpers.RequireArg[*values.String](mc, 1, werr.ErrNotAString, "go-cfg")
	if err != nil {
		return err
	}
	mc.SetValue(values.EmptyList)
	return nil
}
