package values

// PairBlock is a contiguous slice of Pairs that can be linked into a proper
// list. Block allocation amortizes N heap allocations to 1 for list
// construction.
type PairBlock []Pair

// LinkWith fills cars from vs and links cdrs into a proper list, returning
// the head as a Tuple. The block must have the same length as vs.
// A nil or empty block returns EmptyList.
func (b PairBlock) LinkWith(vs []Value) Tuple {
	n := len(b)
	if n == 0 {
		return EmptyList
	}
	for i := 0; i < n-1; i++ {
		b[i][0] = vs[i]
		b[i][1] = &b[i+1]
	}
	b[n-1][0] = vs[n-1]
	b[n-1][1] = EmptyList
	return &b[0]
}
