package values

import "fmt"

var (
	_ Value = (*SchemeEnvironment)(nil)
)

// SchemeEnvironment represents a first-class environment for use with eval.
// It wraps an environment frame for use as a Scheme value.
type SchemeEnvironment struct {
	// Name is an optional descriptive name (e.g., "interaction-environment")
	Name string
	// Frame is the actual environment frame - stored as interface{} to avoid
	// circular dependency with environment package
	Frame interface{}
}

func NewSchemeEnvironment(name string, frame interface{}) *SchemeEnvironment {
	return &SchemeEnvironment{
		Name:  name,
		Frame: frame,
	}
}

func (e *SchemeEnvironment) IsVoid() bool {
	return e == nil
}

func (e *SchemeEnvironment) EqualTo(v Value) bool {
	other, ok := v.(*SchemeEnvironment)
	if !ok {
		return false
	}
	return e == other // Environments are compared by identity
}

func (e *SchemeEnvironment) SchemeString() string {
	if e.Name != "" {
		return fmt.Sprintf("#<environment %s>", e.Name)
	}
	return "#<environment>"
}
