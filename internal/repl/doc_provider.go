package repl

import "github.com/aalpar/wile/values"

// DocInfo holds documentation for a primitive binding.
type DocInfo struct {
	Doc        string
	ParamNames []string
	Category   string
	ParamCount int
	IsVariadic bool
	ParamTypes []values.ValueType
	ReturnType values.ValueType
}

// DocProvider looks up documentation for named bindings.
type DocProvider interface {
	// LookupDoc returns documentation for the named primitive.
	// Returns found=false if no documentation exists.
	LookupDoc(name string) (info DocInfo, found bool)
}
