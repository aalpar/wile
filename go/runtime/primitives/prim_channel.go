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

// Go Channel Primitives for Scheme
// Exposes Go channels to Scheme for CSP-style concurrency

package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimMakeChannel creates a new channel
// (make-channel [buffer-size]) -> channel
func PrimMakeChannel(_ context.Context, mc *machine.MachineContext) error {
	restVal := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

	bufferSize := 0
	// Parse optional buffer-size from rest list
	if !values.IsEmptyList(restVal) {
		if restList, ok := restVal.(*values.Pair); ok {
			if n, ok := restList.Car().(*values.Integer); ok {
				bufferSize = int(n.Value)
				if bufferSize < 0 {
					bufferSize = 0
				}
			}
		}
	}

	ch := values.NewChannel(bufferSize)
	mc.SetValue(ch)
	return nil
}

// PrimChannelQ tests if an object is a channel
// (channel? obj) -> boolean
func PrimChannelQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.Channel)
	if ok {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimChannelSend sends a value on the channel (blocking)
// (channel-send! ch value) -> void
func PrimChannelSend(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	val := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	ch, ok := o.(*values.Channel)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAChannel, "channel-send!: expected channel, got %T", o)
	}

	err := ch.Send(val)
	if err != nil {
		return values.WrapForeignErrorf(err, "channel-send!")
	}

	mc.SetValue(values.Void)
	return nil
}

// PrimChannelReceive receives a value from the channel (blocking)
// (channel-receive ch) -> value
func PrimChannelReceive(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Channel)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAChannel, "channel-receive: expected channel, got %T", o)
	}

	v, ok := ch.Receive()
	if !ok {
		// Channel is closed
		mc.SetValue(values.Void)
	} else if v == nil {
		mc.SetValue(values.Void)
	} else {
		mc.SetValue(v)
	}
	return nil
}

// PrimChannelTrySend attempts to send without blocking
// (channel-try-send! ch value) -> boolean
func PrimChannelTrySend(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	val := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	ch, ok := o.(*values.Channel)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAChannel, "channel-try-send!: expected channel, got %T", o)
	}

	sent, err := ch.TrySend(val)
	if err != nil {
		return values.WrapForeignErrorf(err, "channel-try-send!")
	}

	if sent {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimChannelTryReceive attempts to receive without blocking
// (channel-try-receive ch) -> (values value received? open?)
func PrimChannelTryReceive(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Channel)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAChannel, "channel-try-receive: expected channel, got %T", o)
	}

	v, received, open := ch.TryReceive()

	// Return multiple values
	var val values.Value
	if v == nil {
		val = values.FalseValue
	} else {
		val = v
	}

	var receivedVal values.Value
	if received {
		receivedVal = values.TrueValue
	} else {
		receivedVal = values.FalseValue
	}

	var openVal values.Value
	if open {
		openVal = values.TrueValue
	} else {
		openVal = values.FalseValue
	}

	mc.SetValues(val, receivedVal, openVal)
	return nil
}

// PrimChannelClose closes the channel
// (channel-close! ch) -> void
func PrimChannelClose(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Channel)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAChannel, "channel-close!: expected channel, got %T", o)
	}

	err := ch.Close()
	if err != nil {
		return values.WrapForeignErrorf(err, "channel-close!")
	}

	mc.SetValue(values.Void)
	return nil
}

// PrimChannelClosedQ tests if a channel is closed
// (channel-closed? ch) -> boolean
func PrimChannelClosedQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Channel)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAChannel, "channel-closed?: expected channel, got %T", o)
	}

	if ch.IsClosed() {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimChannelLength returns the number of elements in the channel buffer
// (channel-length ch) -> integer
func PrimChannelLength(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Channel)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAChannel, "channel-length: expected channel, got %T", o)
	}

	mc.SetValue(values.NewInteger(int64(ch.Len())))
	return nil
}

// PrimChannelCapacity returns the channel's buffer capacity
// (channel-capacity ch) -> integer
func PrimChannelCapacity(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Channel)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAChannel, "channel-capacity: expected channel, got %T", o)
	}

	mc.SetValue(values.NewInteger(int64(ch.Cap())))
	return nil
}
