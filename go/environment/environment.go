package environment

type Environment interface {
	Parent() Environment
	Values() []*Binding
	SetValues(v []*Binding)
	Keys() map[string]int
}

type EnvironmentNavigation interface {
	Meta()
	Parent() Environment
	LocalEnvironment() Environment
	GlobalEnvironment() Environment
}
