package utils

import "wile/values" // adjust import path as needed

// AsList converts a slice of values into a proper Scheme-style list.
// The list is a linked list of *Pair nodes where each pair's Car holds
// a value and Cdr points to the next pair, terminated by EmptyList.
func AsList(items []values.Value) *values.Pair {
	if len(items) == 0 {
		return nil // or return EmptyList depending on your API
	}

	// Build the list from the end backwards
	var result values.Value = values.EmptyList

	for i := len(items) - 1; i >= 0; i-- {
		result = &values.Pair{items[i], result}
	}

	return result.(*values.Pair)
}
