package utils

import "skeme/values"

// PairIterator provides sequential traversal over Scheme pairs (linked lists).
// It tracks the original head, the current pair being visited, and the tail
// (cdr) of the last visited pair. The iterator handles both proper lists
// (terminated by EmptyList) and improper lists (terminated by a non-pair value).
//
// Usage:
//
//	iter := &PairIterator{}
//	iter.head = list.(*values.Pair)
//	iter.Reset()
//	for !iter.Done() {
//	    car, done := iter.Next()
//	    // process car
//	}
//	// iter.Tail() contains the final cdr (EmptyList for proper lists)
type PairIterator struct {
	head *values.Pair  // the original pair to iterate over
	cur  *values.Pair  // current position in the list
	tail values.Value  // cdr of the most recently visited pair
	done bool          // true when iteration is complete
}

// NewPairIterator creates a new iterator for the given pair.
// The iterator is automatically reset to the beginning.
func NewPairIterator(head *values.Pair) *PairIterator {
	p := &PairIterator{head: head}
	p.Reset()
	return p
}

// Reset restores the iterator to its initial state, pointing at the head.
// Call Reset before beginning iteration or to restart from the beginning.
// Sets done to true if head is void or empty list.
func (p *PairIterator) Reset() {
	p.cur = p.head
	p.tail = p.head
	p.done = p.cur.IsVoid() || p.cur.IsEmptyList()
}

// Head returns the original pair that was set for iteration.
func (p *PairIterator) Head() values.Value {
	return p.head
}

// Tail returns the cdr of the most recently visited pair.
// After full iteration of a proper list, this will be EmptyList.
// For improper lists, this will be the terminating non-pair value.
func (p *PairIterator) Tail() values.Value {
	return p.tail
}

// Current returns the pair at the current iteration position.
// Returns nil after iteration completes.
func (p *PairIterator) Current() values.Value {
	return p.cur
}

// Done returns true when iteration is complete.
func (p *PairIterator) Done() bool {
	return p.done
}

// Next advances the iterator and returns the car of the current pair.
// The second return value indicates whether this was the final element
// (i.e., the cdr is not another pair). After Next returns with done=true,
// further calls to Next should not be made. For proper lists, the final
// call returns the last car with done=true. For improper lists, the final
// non-pair cdr is available via Tail().
func (p *PairIterator) Next() (values.Value, bool) {
	car := p.cur[0]
	p.tail = p.cur[1]
	p.cur, _ = p.tail.(*values.Pair)
	p.done = p.cur.IsVoid() || p.cur.IsEmptyList()
	return car, p.done
}
