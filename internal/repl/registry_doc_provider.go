package repl

import (
	"github.com/aalpar/wile/registry"
)

// RegistryDocProvider adapts a registry.Registry to the DocProvider interface.
type RegistryDocProvider struct {
	reg *registry.Registry
}

// NewRegistryDocProvider creates a DocProvider backed by the given registry.
func NewRegistryDocProvider(reg *registry.Registry) *RegistryDocProvider {
	return &RegistryDocProvider{
		reg: reg,
	}
}

// LookupDoc returns documentation for the named primitive from the registry.
func (p *RegistryDocProvider) LookupDoc(name string) (DocInfo, bool) {
	pr, found := p.reg.FindPrimitive(name, 0)
	if !found {
		return DocInfo{}, false
	}
	return DocInfo{
		Doc:        pr.Spec.Doc,
		ParamNames: pr.Spec.ParamNames,
		Category:   pr.Spec.Category,
		ParamCount: pr.Spec.ParamCount,
		IsVariadic: pr.Spec.IsVariadic,
		ParamTypes: pr.Spec.ParamTypes,
		ReturnType: pr.Spec.ReturnType,
	}, true
}
