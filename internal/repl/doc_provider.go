package repl

// DocInfo holds documentation for a primitive binding.
type DocInfo struct {
	Doc        string
	ParamNames []string
	Category   string
	ParamCount int
	IsVariadic bool
}

// DocProvider looks up documentation for named bindings.
type DocProvider interface {
	// LookupDoc returns documentation for the named primitive.
	// Returns found=false if no documentation exists.
	LookupDoc(name string) (info DocInfo, found bool)
}
