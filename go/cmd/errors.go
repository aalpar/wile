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
