package primitives

func FloorDivide(n0, n1 int64) (q, r int64) {
	q = n0 / n1
	r = n0 % n1
	// Floor division: quotient rounds toward negative infinity
	// If remainder is non-zero and signs differ, adjust
	if r != 0 && (n0 < 0) != (n1 < 0) {
		q--
		r += n1
	}
	return q, r
}

func GcdInt(a, b int64) int64 {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}
