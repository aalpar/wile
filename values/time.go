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

package values

import (
	"fmt"
	"time"
)

var _ Value = (*Time)(nil)

// Time represents a point in time (SRFI-18)
type Time struct {
	t time.Time
}

// NewTime creates a new Time from a Go time.Time
func NewTime(t time.Time) *Time {
	return &Time{t: t}
}

// CurrentTime returns the current time
func CurrentTime() *Time {
	return &Time{t: time.Now()}
}

// NewTimeFromSeconds creates a Time from seconds since epoch
func NewTimeFromSeconds(seconds float64) *Time {
	sec := int64(seconds)
	nsec := int64((seconds - float64(sec)) * 1e9)
	return &Time{t: time.Unix(sec, nsec)}
}

// GoTime returns the underlying Go time.Time
func (p *Time) GoTime() time.Time {
	return p.t
}

// Seconds returns the time as seconds since the epoch
func (p *Time) Seconds() float64 {
	return float64(p.t.UnixNano()) / 1e9
}

// Add returns a new Time that is the given duration after this time
func (p *Time) Add(d time.Duration) *Time {
	return &Time{t: p.t.Add(d)}
}

// Sub returns the duration between this time and another
func (p *Time) Sub(other *Time) time.Duration {
	return p.t.Sub(other.t)
}

// Before returns true if this time is before another
func (p *Time) Before(other *Time) bool {
	return p.t.Before(other.t)
}

// After returns true if this time is after another
func (p *Time) After(other *Time) bool {
	return p.t.After(other.t)
}

// DurationFromNow returns the duration from now until this time.
func (p *Time) DurationFromNow() time.Duration {
	return time.Until(p.t)
}

// buf interface implementation

// IsVoid returns true if the time is nil.
func (p *Time) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both times represent the same instant.
func (p *Time) EqualTo(v Value) bool {
	other, ok := v.(*Time)
	if !ok {
		return false
	}
	if p == nil && other == nil {
		return true
	}
	if p == nil || other == nil {
		return false
	}
	return p.t.Equal(other.t)
}

// SchemeString returns the Scheme representation of the time.
func (p *Time) SchemeString() string {
	if p == nil {
		return "#<time:void>"
	}
	return fmt.Sprintf("#<time %s>", p.t.Format(time.RFC3339Nano))
}
