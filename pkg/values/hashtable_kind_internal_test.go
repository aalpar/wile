package values

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestHashtableKindCodesHaveNames ties HashtableKindCount to the codes String
// names: exactly HashtableKindCount of the 256 codes render as a kind, so a
// kind added with a String row but no count bump, or a count bump with no row,
// fails here. (A kind added with neither still passes; the count ratchet in
// TestHashtableKindsAreExhaustive is the prompt for that one.)
func TestHashtableKindCodesHaveNames(t *testing.T) {
	named := 0
	for code := range 256 {
		if (HashtableKind{code: uint8(code)}).String() != "unknown" {
			named++
		}
	}
	qt.Assert(t, named, qt.Equals, HashtableKindCount)
}
