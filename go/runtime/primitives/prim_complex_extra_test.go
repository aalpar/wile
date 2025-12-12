// Copyright 2025 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package primitives_test

import (
	"math"
	"testing"

	qt "github.com/frankban/quicktest"

	"wile/values"
)

// ----------------------------------------------------------------------------
// angle primitive tests
// ----------------------------------------------------------------------------

func TestAngle(t *testing.T) {
	tcs := []struct {
		name      string
		code      string
		expected  float64
		tolerance float64
	}{
		{
			name:     "angle of positive integer",
			code:     "(angle 1)",
			expected: 0.0,
		},
		{
			name:     "angle of negative integer",
			code:     "(angle -1)",
			expected: math.Pi,
		},
		{
			name:     "angle of positive float",
			code:     "(angle 1.0)",
			expected: 0.0,
		},
		{
			name:     "angle of negative float",
			code:     "(angle -1.0)",
			expected: math.Pi,
		},
		{
			name:     "angle of positive rational",
			code:     "(angle 1/2)",
			expected: 0.0,
		},
		{
			name:     "angle of negative rational",
			code:     "(angle -1/2)",
			expected: math.Pi,
		},
		{
			name:      "angle of complex number 1+1i",
			code:      "(angle 1+1i)",
			expected:  math.Pi / 4,
			tolerance: 0.0001,
		},
		{
			name:      "angle of complex number 0+1i",
			code:      "(angle 0+1i)",
			expected:  math.Pi / 2,
			tolerance: 0.0001,
		},
		{
			name:      "angle of complex number -1+0i",
			code:      "(angle -1+0i)",
			expected:  math.Pi,
			tolerance: 0.0001,
		},
		{
			name:      "angle of complex number 0-1i",
			code:      "(angle 0-1i)",
			expected:  -math.Pi / 2,
			tolerance: 0.0001,
		},
		{
			name:      "angle of complex number 1-1i",
			code:      "(angle 1-1i)",
			expected:  -math.Pi / 4,
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)

			resultFloat, ok := result.(*values.Float)
			qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected *values.Float, got %T", result))

			if tc.tolerance > 0 {
				diff := math.Abs(resultFloat.Value - tc.expected)
				qt.Assert(t, diff < tc.tolerance, qt.IsTrue,
					qt.Commentf("expected %v (±%v), got %v", tc.expected, tc.tolerance, resultFloat.Value))
			} else {
				qt.Assert(t, resultFloat.Value, qt.Equals, tc.expected)
			}
		})
	}
}

// ----------------------------------------------------------------------------
// make-polar primitive tests
// ----------------------------------------------------------------------------

func TestMakePolar(t *testing.T) {
	tcs := []struct {
		name      string
		code      string
		checkReal float64
		checkImag float64
		tolerance float64
	}{
		{
			name:      "make-polar with magnitude 1, angle 0",
			code:      "(make-polar 1 0)",
			checkReal: 1.0,
			checkImag: 0.0,
			tolerance: 0.0001,
		},
		{
			name:      "make-polar with magnitude 2, angle π/2",
			code:      "(make-polar 2 1.5707963267948966)",
			checkReal: 0.0,
			checkImag: 2.0,
			tolerance: 0.0001,
		},
		{
			name:      "make-polar with float args",
			code:      "(make-polar 1.0 0.0)",
			checkReal: 1.0,
			checkImag: 0.0,
			tolerance: 0.0001,
		},
		{
			name:      "make-polar with magnitude 1, angle π/4",
			code:      "(make-polar 1 0.7853981633974483)",
			checkReal: math.Sqrt(2) / 2,
			checkImag: math.Sqrt(2) / 2,
			tolerance: 0.0001,
		},
		{
			name:      "make-polar with magnitude 3, angle π",
			code:      "(make-polar 3 3.141592653589793)",
			checkReal: -3.0,
			checkImag: 0.0,
			tolerance: 0.0001,
		},
		{
			name:      "make-polar with magnitude 1, angle -π/2",
			code:      "(make-polar 1 -1.5707963267948966)",
			checkReal: 0.0,
			checkImag: -1.0,
			tolerance: 0.0001,
		},
		{
			name:      "make-polar with rational magnitude",
			code:      "(make-polar 1/2 0)",
			checkReal: 0.5,
			checkImag: 0.0,
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)

			resultComplex, ok := result.(*values.Complex)
			qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected *values.Complex, got %T", result))

			realDiff := math.Abs(real(resultComplex.Value) - tc.checkReal)
			imagDiff := math.Abs(imag(resultComplex.Value) - tc.checkImag)

			qt.Assert(t, realDiff < tc.tolerance, qt.IsTrue,
				qt.Commentf("real part: expected %v (±%v), got %v", tc.checkReal, tc.tolerance, real(resultComplex.Value)))
			qt.Assert(t, imagDiff < tc.tolerance, qt.IsTrue,
				qt.Commentf("imag part: expected %v (±%v), got %v", tc.checkImag, tc.tolerance, imag(resultComplex.Value)))
		})
	}
}

// ----------------------------------------------------------------------------
// Round-trip tests: make-polar and angle should be inverses
// ----------------------------------------------------------------------------

func TestMakePolarAngleRoundTrip(t *testing.T) {
	tcs := []struct {
		name      string
		magnitude float64
		angle     float64
		tolerance float64
	}{
		{
			name:      "round-trip with magnitude 1, angle 0",
			magnitude: 1.0,
			angle:     0.0,
			tolerance: 0.0001,
		},
		{
			name:      "round-trip with magnitude 2, angle π/4",
			magnitude: 2.0,
			angle:     math.Pi / 4,
			tolerance: 0.0001,
		},
		{
			name:      "round-trip with magnitude 3, angle π/2",
			magnitude: 3.0,
			angle:     math.Pi / 2,
			tolerance: 0.0001,
		},
		{
			name:      "round-trip with magnitude 1.5, angle -π/3",
			magnitude: 1.5,
			angle:     -math.Pi / 3,
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// Create a complex number from polar coordinates
			makePolarCode := "(make-polar " + floatToString(tc.magnitude) + " " + floatToString(tc.angle) + ")"
			resultMakePolar, err := runSchemeCode(t, makePolarCode)
			qt.Assert(t, err, qt.IsNil)

			resultComplex, ok := resultMakePolar.(*values.Complex)
			qt.Assert(t, ok, qt.IsTrue)

			// Get the angle back
			angleCode := "(angle " + resultComplex.SchemeString() + ")"
			resultAngle, err := runSchemeCode(t, angleCode)
			qt.Assert(t, err, qt.IsNil)

			resultAngleFloat, ok := resultAngle.(*values.Float)
			qt.Assert(t, ok, qt.IsTrue)

			// Check that the angle matches (within tolerance)
			angleDiff := math.Abs(resultAngleFloat.Value - tc.angle)
			qt.Assert(t, angleDiff < tc.tolerance, qt.IsTrue,
				qt.Commentf("expected angle %v (±%v), got %v", tc.angle, tc.tolerance, resultAngleFloat.Value))

			// Check that the magnitude matches (within tolerance)
			magnitudeCode := "(magnitude " + resultComplex.SchemeString() + ")"
			resultMagnitude, err := runSchemeCode(t, magnitudeCode)
			qt.Assert(t, err, qt.IsNil)

			resultMagnitudeFloat, ok := resultMagnitude.(*values.Float)
			qt.Assert(t, ok, qt.IsTrue)

			magnitudeDiff := math.Abs(resultMagnitudeFloat.Value - tc.magnitude)
			qt.Assert(t, magnitudeDiff < tc.tolerance, qt.IsTrue,
				qt.Commentf("expected magnitude %v (±%v), got %v", tc.magnitude, tc.tolerance, resultMagnitudeFloat.Value))
		})
	}
}

// Helper function to convert float64 to string for Scheme code
func floatToString(f float64) string {
	return values.NewFloat(f).SchemeString()
}
