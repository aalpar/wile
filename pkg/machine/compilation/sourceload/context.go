package sourceload

import "context"

// loadStackKey is the context key for a per-load-chain LoadStack. A zero-value
// of an unexported type cannot collide with any other package's context key (the
// standard context-key idiom, matching loadChainKey in the compilation package).
type loadStackKey struct{}

// WithLoadStack returns a context carrying stack as the active per-load-chain load
// stack. Library loading installs a private stack here so that concurrent SRFI-18
// thread loads resolve their (include …) directives against their own directory
// instead of a single mutable LoadStack shared on the root namespace. The stack
// threads down the synchronous load → compile → include call chain via context,
// but context does NOT cross the SRFI-18 goroutine boundary (each thread
// sub-context starts from its own context), so its Push/Pop stay confined to one
// goroutine and need no cross-thread synchronization — exactly the property that
// makes the shared-namespace stack unsafe under concurrency.
func WithLoadStack(ctx context.Context, stack *LoadStack) context.Context {
	return context.WithValue(ctx, loadStackKey{}, stack)
}

// LoadStackFromContext returns the per-load-chain LoadStack carried on ctx, or nil
// when none has been installed (the top-level, non-library-load case, where the
// shared per-namespace stack remains the source of the current load directory).
func LoadStackFromContext(ctx context.Context) *LoadStack {
	s, _ := ctx.Value(loadStackKey{}).(*LoadStack)
	return s
}
