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

package values

import (
	"os"
	"sync"
	"testing"
)

// TestChannelSendCloseTOCTOU reproduces the reviews/2026-07-13 §4 CONCURRENCY
// finding at pkg/values/channel.go:67. Channel.Send checks p.closed under the
// read lock, RELEASES the lock, then performs `ch <- v`. A concurrent Close
// landing between the unlock and the send turns the send into Go's "send on
// closed channel" panic — a classic check-then-act (TOCTOU) hole. TrySend
// (channel.go:82) shares the same shape.
//
// This is a LOGICAL race, not a -race data race: the channel operation itself
// is runtime-synchronised, so the evidence is the panic, which this test
// provokes across many trials and recovers. Gated OUT of CI:
//
//	WILE_RACE_REPRO=1 go test -run TestChannelSendCloseTOCTOU ./pkg/values/
//
// Expected: at least one recovered "send on closed channel" panic. Once the
// finding is fixed (send under the lock, or a recover in Send that maps the
// panic to werr.ErrChannelClosed), this test should report zero panics and can
// be inverted into a guard.
func TestChannelSendCloseTOCTOU(t *testing.T) {
	if os.Getenv("WILE_RACE_REPRO") == "" {
		t.Skip("deliberate TOCTOU repro; set WILE_RACE_REPRO=1 to run")
	}

	panics := 0
	const trials = 20000
	for range trials {
		ch := NewChannel(1)
		var wg sync.WaitGroup
		wg.Add(1)
		go func() {
			defer wg.Done()
			defer func() {
				r := recover()
				if r != nil {
					// A send raced past the closed-check and hit the closed
					// channel: the TOCTOU manifested.
					panics++
				}
			}()
			// Fill the buffer then keep sending, so a send is blocked in flight
			// when Close lands and the runtime panics it.
			for range 4 {
				serr := ch.Send(TrueValue)
				if serr != nil {
					return
				}
			}
		}()
		_ = ch.Close()
		wg.Wait()
	}

	if panics == 0 {
		t.Fatalf("TOCTOU did not reproduce in %d trials; expected at least one "+
			"send-on-closed-channel panic (finding may be fixed)", trials)
	}
	t.Logf("reproduced send-on-closed-channel panic in %d/%d trials", panics, trials)
}
