## Problem
Current `Hashtable` re-implements hash map on top of `map[uint64][]hashtableEntry`. Go's map already does bucket chaining, resizing, and amortized O(1) lookup.

## Proposed Solution
1. Typed maps for common cases (`map[int64]Value`, `map[string]Value`) with fallback
2. Accept current design (~50 lines, handles arbitrary `Hashable` keys correctly)

## Next Steps
- [ ] Profile actual workloads before committing to redesign
- [ ] Measure memory overhead of current approach
- [ ] Benchmark typed-map performance gain

## Location
`values/hashtable.go`

