package utils

import "skeme/values"

func BoolToBoolean(b bool) *values.Boolean {
	if b {
		return values.TrueValue
	}
	return values.FalseValue
}
