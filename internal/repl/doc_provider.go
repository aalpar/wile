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

// DocSearchResult holds one search hit.
type DocSearchResult struct {
	Name     string
	Doc      string
	Category string
}

// DocSearchProvider extends DocProvider with search and category browsing.
type DocSearchProvider interface {
	DocProvider
	// Search returns entries whose name, doc, or category contains pattern
	// (case-insensitive substring match). Results are sorted by name.
	Search(pattern string) []DocSearchResult
	// Categories returns sorted category names.
	Categories() []string
	// ByCategory returns entries in the named category, sorted by name.
	ByCategory(category string) []DocSearchResult
}
