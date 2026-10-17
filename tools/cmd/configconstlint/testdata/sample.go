package sample

func use(a, b int) {
}

func bad() {
	use(DefaultInlineThreshold, 1)
	use(1, compilation.DefaultMaxExpandDepth)
}

func fine() {
	x := DefaultMaxExpandDepth
	use(x, NamespaceMaxExpandDepth(nil))
}
