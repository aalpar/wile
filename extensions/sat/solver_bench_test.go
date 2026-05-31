package sat

import (
	"context"
	"testing"
)

func BenchmarkPHP_5(b *testing.B) {
	cs, n := makePHP(6, 5)
	for i := 0; i < b.N; i++ {
		s := newSolver(context.Background(), copyClauses(cs), n, -1)
		_ = s.solve()
	}
}

func BenchmarkPHP_6(b *testing.B) {
	cs, n := makePHP(7, 6)
	for i := 0; i < b.N; i++ {
		s := newSolver(context.Background(), copyClauses(cs), n, -1)
		_ = s.solve()
	}
}

func BenchmarkPHP_7(b *testing.B) {
	cs, n := makePHP(8, 7)
	for i := 0; i < b.N; i++ {
		s := newSolver(context.Background(), copyClauses(cs), n, -1)
		_ = s.solve()
	}
}

func BenchmarkRandom3SAT_100(b *testing.B) {
	rng := newDeterministicRNG(42)
	cs, n := randomCNF(rng, 100, 426, 3)
	for i := 0; i < b.N; i++ {
		s := newSolver(context.Background(), copyClauses(cs), n, -1)
		_ = s.solve()
	}
}

func makePHP(p, h int) ([]clause, int32) {
	v := func(i, j int) int32 {
		return int32((i-1)*h + j)
	}
	var cs []clause
	for i := 1; i <= p; i++ {
		lits := make([]literal, 0, h)
		for j := 1; j <= h; j++ {
			lits = append(lits, literal(2*v(i, j)))
		}
		cs = append(cs, clause{lits: lits})
	}
	for j := 1; j <= h; j++ {
		for i1 := 1; i1 <= p; i1++ {
			for i2 := i1 + 1; i2 <= p; i2++ {
				cs = append(cs, clause{
					lits: []literal{literal(2*v(i1, j) + 1), literal(2*v(i2, j) + 1)},
				})
			}
		}
	}
	return cs, int32(p * h)
}

func copyClauses(cs []clause) []clause {
	out := make([]clause, len(cs))
	for i, c := range cs {
		out[i] = clause{
			learnt:   c.learnt,
			activity: c.activity,
			lits:     append([]literal(nil), c.lits...),
		}
	}
	return out
}
