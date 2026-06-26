package values

// PairBlock is a contiguous slice of Pairs that can be linked into a proper
// list. Block allocation amortizes N heap allocations to 1 for list
// construction.
type PairBlock []Pair

// LinkSpine links the block's cdrs into a proper-list spine and terminates
// the final cdr with EmptyList, leaving every car unset for the caller to
// fill. Use this when the cars must be assigned in an order LinkWith cannot
// express — e.g. reverse fills them back-to-front during a streaming walk.
// This is the single home for the spine-linking invariant; LinkWith builds
// on it. A nil or empty block is a no-op. Returns the block for chaining.
func (b PairBlock) LinkSpine() PairBlock {
	n := len(b)
	if n == 0 {
		return b
	}
	for i := 0; i < n-1; i++ {
		b[i][1] = &b[i+1]
	}
	b[n-1][1] = EmptyList
	return b
}

// LinkWith fills cars from vs and links cdrs into a proper list, returning
// the head as a Tuple. The block must have the same length as vs.
// A nil or empty block returns EmptyList.
func (b PairBlock) LinkWith(vs []Value) Tuple {
	n := len(b)
	if n == 0 {
		return EmptyList
	}
	b.LinkSpine()
	for i := range n {
		b[i][0] = vs[i]
	}
	return &b[0]
}
