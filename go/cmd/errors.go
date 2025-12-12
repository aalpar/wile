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


package main

const (
	EX_OK    = 0  /* successful termination */
	EX__BASE = 64 /* base value for error messages */
	EX__MAX  = 78 /* maximum listed value */
)

const (
	EX_USAGE       = EX__BASE + iota /* command line usage error */
	EX_DATAERR                       /* data format error */
	EX_NOINPUT                       /* cannot open input */
	EX_NOUSER                        /* addressee unknown */
	EX_NOHOST                        /* host name unknown */
	EX_UNAVAILABLE                   /* service unavailable */
	EX_SOFTWARE                      /* internal software error */
	EX_OSERR                         /* system error (e.g., can't fork) */
	EX_OSFILE                        /* critical OS file missing */
	EX_CANTCREAT                     /* can't create (user) output file */
	EX_IOERR                         /* input/output error */
	EX_TEMPFAIL                      /* temp failure; user is invited to retry */
	EX_PROTOCOL                      /* remote error in protocol */
	EX_NOPERM                        /* permission denied */
	EX_CONFIG                        /* configuration error */
)
