// Copyright 2026 Aaron Alpar
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

package core_test

import (
	"testing"
	"time"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// ----------------------------------------------------------------------------
// Go Channel Primitives Tests
// ----------------------------------------------------------------------------

func TestChannelQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "channel? on channel object",
			code: "(channel? (make-channel))",
			out:  values.TrueValue,
		},
		{
			name: "channel? on buffered channel",
			code: "(channel? (make-channel 10))",
			out:  values.TrueValue,
		},
		{
			name: "channel? on integer",
			code: "(channel? 42)",
			out:  values.FalseValue,
		},
		{
			name: "channel? on string",
			code: `(channel? "channel")`,
			out:  values.FalseValue,
		},
		{
			name: "channel? on mutex",
			code: "(channel? (make-mutex))",
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestMakeChannel(t *testing.T) {
	// make-channel should return a channel
	result, err := runSchemeCode(t, "(channel? (make-channel))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestMakeChannelWithBuffer(t *testing.T) {
	// make-channel with buffer size
	code := "(channel-capacity (make-channel 5))"
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(5))
}

func TestChannelCapacity(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "unbuffered channel capacity",
			code: "(channel-capacity (make-channel))",
			out:  values.NewInteger(0),
		},
		{
			name: "buffered channel capacity",
			code: "(channel-capacity (make-channel 10))",
			out:  values.NewInteger(10),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestChannelCapacityError(t *testing.T) {
	_, err := runSchemeCode(t, "(channel-capacity 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestChannelLength(t *testing.T) {
	// Empty channel should have length 0
	code := "(channel-length (make-channel 5))"
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(0))
}

func TestChannelLengthError(t *testing.T) {
	_, err := runSchemeCode(t, "(channel-length 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestChannelSendAndReceive(t *testing.T) {
	// Test send and receive on buffered channel
	code := `
		(let ((ch (make-channel 1)))
			(channel-send! ch 42)
			(channel-receive ch))
	`
	result, err := runSchemeCodeWithTimeout(t, code, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(42))
}

func TestChannelSendError(t *testing.T) {
	_, err := runSchemeCode(t, "(channel-send! 42 'value)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestChannelReceiveError(t *testing.T) {
	_, err := runSchemeCode(t, "(channel-receive 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestChannelTrySend(t *testing.T) {
	// Try send on buffered channel with space
	code := `
		(let ((ch (make-channel 1)))
			(channel-try-send! ch 42))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestChannelTrySendFull(t *testing.T) {
	// Try send on full channel should return #f
	code := `
		(let ((ch (make-channel 1)))
			(channel-try-send! ch 1)
			(channel-try-send! ch 2))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.FalseValue)
}

func TestChannelTrySendError(t *testing.T) {
	_, err := runSchemeCode(t, "(channel-try-send! 42 'value)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestChannelTryReceive(t *testing.T) {
	// Try receive returns multiple values
	code := `
		(let ((ch (make-channel 1)))
			(channel-try-send! ch 42)
			(call-with-values
				(lambda () (channel-try-receive ch))
				(lambda (val received open)
					(list val received open))))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	// Should return (42 #t #t)
	pair, ok := result.(*values.Pair)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, pair.Car(), valuestest.SchemeEquals, values.NewInteger(42))
}

func TestChannelTryReceiveEmpty(t *testing.T) {
	// Try receive on empty channel
	code := `
		(let ((ch (make-channel 1)))
			(call-with-values
				(lambda () (channel-try-receive ch))
				(lambda (val received open)
					received)))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.FalseValue)
}

func TestChannelTryReceiveError(t *testing.T) {
	_, err := runSchemeCode(t, "(channel-try-receive 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestChannelClose(t *testing.T) {
	code := `
		(let ((ch (make-channel)))
			(channel-close! ch)
			(channel-closed? ch))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestChannelCloseError(t *testing.T) {
	_, err := runSchemeCode(t, "(channel-close! 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestChannelClosedQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "open channel",
			code: "(channel-closed? (make-channel))",
			out:  values.FalseValue,
		},
		{
			name: "closed channel",
			code: `
				(let ((ch (make-channel)))
					(channel-close! ch)
					(channel-closed? ch))
			`,
			out: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestChannelClosedQError(t *testing.T) {
	_, err := runSchemeCode(t, "(channel-closed? 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestChannelWithThread(t *testing.T) {
	// Test channel communication between threads
	code := `
		(let ((ch (make-channel 1)))
			(let ((producer (make-thread (lambda () (channel-send! ch 100)))))
				(thread-start! producer)
				(thread-join! producer)
				(channel-receive ch)))
	`
	result, err := runSchemeCodeWithTimeout(t, code, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(100))
}

func TestChannelMultipleValues(t *testing.T) {
	// Test sending and receiving multiple values
	code := `
		(let ((ch (make-channel 3)))
			(channel-send! ch 1)
			(channel-send! ch 2)
			(channel-send! ch 3)
			(+ (channel-receive ch)
			   (channel-receive ch)
			   (channel-receive ch)))
	`
	result, err := runSchemeCodeWithTimeout(t, code, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(6))
}
