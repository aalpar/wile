TODO
----

Code Maintenance
----------------
- Dead code removal (see Dead Code Removal section below)
- Project rename from "skeme" to "wile" (module path, binary name, env vars, docs)
- check all functions that accept pairs or lists that they handle partial lists correctly.  
  functions should either accept partial lists (lists without the last element being a EmptyList)
  or should produce a ErrNotAPair or ErrNotAList errors.
- check all scheme primitives that they work properly with partial lists (lists without the last element being a EmptyList)
  each function should return an Error object or return the resule defined in the R7RS specification.  All functions must be
  R7RS or R6RS compliant

Code Cleanup
------------
- Tokenization error handling is obscure - figure something better out
- [ ] Inf Nan handling in tokenizer is inconsistent.  Some places use math.Inf(1)/math.Inf(-1) and some use predefined constants.  Standardize on one approach.
- [x] Refactor `if err := ...; err != nil` patterns to separate assignment from comparison (see CODING_STYLE.md)
- consoloidate error handling for unimplemented features (e.g., in compiler)
- syntax->datum and datum->syntax have duplicate code.  Consolidate.
- Refactor compiler primitive handling to use PrimitiveCompiler registry for less code redundancy
- Refactor compiler literal handling to use LiteralCompiler registry for less code redundancy
- Refactor compiler special form handling to use SpecialFormCompiler registry for less code redundancy
- Refactor compiler expansion handling to use Expander registry for less code redundancy
- Refactor compiler optimization handling to use Optimizer registry for less code redundancy
- Refactor compiler evaluation handling to use Evaluator registry for less code redundancy
- compile_validate.go needs to be reduced in size.  Separate methods for types instead of using interfaces.
- evalWhenCompileForRuntime uses odd loop.  Analyze and simplify.
- "Must" wrapper for ForEach so that proper list check can be removed
- compile_eval_when.go: list of hardcoded phases.  can this be moved to a table-driven approach?
- Turn tests into table-driven tests where applicable
- Unwrap and Datums: simplify code by removing unnecessary wrapping and unwrapping of Datum types. Its laborious and duplicative in a few places.  Try to keep Datum wrapping/unwrapping at the edges of functions. Keep as much code as part of the methods on the objects.
- Get rid of vardec functions (too confusing)
- Fix tokenizer warnings: unreachable code (lines 1641, 1664) and unhandled errors
- Refactor tokenizer duplicate code (see Tokenizer Refactoring Notes below)
- Refactor ExpandPrimitiveForm into PrimitiveExpander registry (like PrimitiveCompiler)
- Refactor "formName" processing for compiler-form. Form name should come from car of original form, but instead is being set on an object. Allow dynamic setting of the form-name. Note that the form sometimes is abstract, such as a parameter list or a literal - find a solution for this.
- Refactor environment functions for less code redundancy
- Rename token types for comments.
- Number parsing in tokenizer is messy.  Refactor to reduce code redundancy and improve clarity.  Evaluate removing "signed" token types.
- Consolidate tests into single table tests.  Many tests have the same code for running the tests but are in different files and test functions.  Combine into single table-driven tests where possible.
- Scheme header "Program running, send SIGQUIT (Ctrl+\\) to dump stacks." should be output to stderr and add option for "--quiet"
- fixup "( environment )" creation.
- eval should take 1 or 2 arguments (second argument is optional environment)
- parseComplex is messy - refactor to reduce code redundancy and improve clarity.

Primitive Unit Tests
--------------------

**Status:** 219 of 224 primitives missing dedicated test files.

Each primitive file (`prim_*.go`) should have a matching `prim_*_test.go` file that:
1. Executes Scheme code to test the primitive
2. Tests all accepted types (Integer, BigInteger, Float, Rational, Complex, etc.)
3. Tests edge cases (empty, single element, boundary conditions)
4. Tests error conditions (wrong types, out of bounds, etc.)

**Test Pattern:** Use `runSchemeCode(t, code)` helper with table-driven tests:
```go
func TestPrimName(t *testing.T) {
    tcs := []struct {
        name string
        code string
        out  values.Value
    }{
        {"happy path", `(prim-name arg)`, values.NewInteger(42)},
        {"edge case", `(prim-name)`, values.NewInteger(0)},
    }
    for _, tc := range tcs {
        t.Run(tc.name, func(t *testing.T) {
            result, err := runSchemeCode(t, tc.code)
            qt.Assert(t, err, qt.IsNil)
            qt.Assert(t, result, values.SchemeEquals, tc.out)
        })
    }
}
```

### Arithmetic (17 primitives)
- [ ] `prim_add_test.go` - +, test: integers, floats, rationals, complex, big integers, mixed types, zero args, one arg
- [ ] `prim_sub_test.go` - -, test: unary negation, binary subtraction, variadic, all numeric types
- [ ] `prim_mul_test.go` - *, test: zero args (returns 1), all numeric types, overflow to big integer
- [ ] `prim_div_test.go` - /, test: unary reciprocal, division, rational results, division by zero error
- [ ] `prim_quotient_test.go` - quotient, test: positive/negative, truncation toward zero
- [ ] `prim_remainder_test.go` - remainder, test: sign follows dividend
- [ ] `prim_modulo_test.go` - modulo, test: sign follows divisor
- [ ] `prim_gcd_test.go` - gcd, test: zero args, one arg, multiple args, negative numbers
- [ ] `prim_lcm_test.go` - lcm, test: zero args, one arg, multiple args, zero in args
- [ ] `prim_expt_test.go` - expt, test: integer powers, fractional powers, negative bases
- [ ] `prim_sqrt_test.go` - sqrt, test: perfect squares, non-perfect, negative (complex result)
- [ ] `prim_square_test.go` - square, test: all numeric types
- [ ] `prim_max_test.go` - max, test: mixed types, single arg, NaN handling
- [ ] `prim_min_test.go` - min, test: mixed types, single arg, NaN handling
- [ ] `prim_exact_integer_sqrt_test.go` - exact-integer-sqrt, test: perfect squares, non-perfect (returns two values)
- [ ] `prim_rationalize_test.go` - rationalize, test: tolerance parameter

### Transcendental Functions (12 primitives)
- [ ] `prim_exp_test.go` - exp, test: e^0=1, e^1=e, negative exponents
- [ ] `prim_log_test.go` - log, test: log(1)=0, log(e)=1, two-arg form (base), negative (complex)
- [ ] `prim_sin_test.go` - sin, test: sin(0)=0, sin(pi/2)=1, periodicity
- [ ] `prim_cos_test.go` - cos, test: cos(0)=1, cos(pi)=-1
- [ ] `prim_tan_test.go` - tan, test: tan(0)=0, tan(pi/4)=1
- [ ] `prim_asin_test.go` - asin, test: asin(0)=0, asin(1)=pi/2, out of range (complex)
- [ ] `prim_acos_test.go` - acos, test: acos(1)=0, acos(0)=pi/2
- [ ] `prim_atan_test.go` - atan, test: one-arg form, two-arg form (atan2)

### Complex Numbers (6 primitives)
- [ ] `prim_make_rectangular_test.go` - make-rectangular, test: real+imag parts
- [ ] `prim_make_polar_test.go` - make-polar, test: magnitude+angle
- [ ] `prim_real_part_test.go` - real-part, test: complex, real numbers (return self)
- [ ] `prim_imag_part_test.go` - imag-part, test: complex, real numbers (return 0)
- [ ] `prim_magnitude_test.go` - magnitude, test: complex, real (abs)
- [ ] `prim_angle_test.go` - angle, test: complex, positive/negative real

### Numeric Predicates (15 primitives)
- [ ] `prim_zero_q_test.go` - zero?, test: all numeric types
- [ ] `prim_positive_q_test.go` - positive?, test: integers, floats, rationals
- [ ] `prim_negative_q_test.go` - negative?, test: integers, floats, rationals
- [ ] `prim_odd_q_test.go` - odd?, test: integers only, error on non-integer
- [ ] `prim_even_q_test.go` - even?, test: integers only
- [ ] `prim_exact_q_test.go` - exact?, test: integers (true), floats (false)
- [ ] `prim_inexact_q_test.go` - inexact?, test: floats (true), integers (false)
- [ ] `prim_exact_integer_q_test.go` - exact-integer?, test: integers (true), rationals (false)
- [ ] `prim_finite_q_test.go` - finite?, test: normal numbers (true), inf/nan (false)
- [ ] `prim_infinite_q_test.go` - infinite?, test: +inf.0/-inf.0 (true)
- [ ] `prim_nan_q_test.go` - nan?, test: +nan.0 (true)
- [ ] `prim_integer_q_test.go` - integer?, test: all numeric types
- [ ] `prim_rational_q_test.go` - rational?, test: integers, rationals (true), inf/nan (false)
- [ ] `prim_real_q_test.go` - real?, test: real numbers (true), complex with imag (false)

### Numeric Comparisons (5 primitives)
- [ ] `prim_num_eq_test.go` - =, test: mixed types, transitivity, NaN behavior
- [ ] `prim_num_lt_test.go` - <, test: chain comparison, mixed types
- [ ] `prim_num_gt_test.go` - >, test: chain comparison
- [ ] `prim_num_le_test.go` - <=, test: equality at boundaries
- [ ] `prim_num_ge_test.go` - >=, test: equality at boundaries

### Numeric Conversion (6 primitives)
- [ ] `prim_exact_test.go` - exact, test: float->rational conversion
- [ ] `prim_inexact_test.go` - inexact, test: integer->float, rational->float
- [ ] `prim_numerator_test.go` - numerator, test: rationals, integers (return self)
- [ ] `prim_denominator_test.go` - denominator, test: rationals, integers (return 1)
- [ ] `prim_number_to_string_test.go` - number->string, test: radix parameter (2,8,10,16)
- [ ] `prim_string_to_number_test.go` - string->number, test: radix, invalid strings (#f)

### Division Operations (9 primitives)
- [ ] `prim_floor_div_test.go` - floor/, test: returns quotient and remainder
- [ ] `prim_floor_quotient_test.go` - floor-quotient
- [ ] `prim_floor_remainder_test.go` - floor-remainder
- [ ] `prim_truncate_div_test.go` - truncate/, test: returns quotient and remainder
- [ ] `prim_truncate_quotient_test.go` - truncate-quotient
- [ ] `prim_truncate_remainder_test.go` - truncate-remainder

### List Operations (20 primitives)
- [ ] `prim_car_test.go` - car, test: pairs, lists, error on non-pair
- [ ] `prim_cdr_test.go` - cdr, test: pairs, lists, improper lists
- [ ] `prim_cons_test.go` - cons, test: building lists, improper pairs
- [ ] `prim_set_car_test.go` - set-car!, test: mutation, error on non-pair
- [ ] `prim_set_cdr_test.go` - set-cdr!, test: mutation
- [ ] `prim_null_q_test.go` - null?, test: empty list (true), non-empty (false)
- [ ] `prim_pair_q_test.go` - pair?, test: pairs (true), empty list (false)
- [ ] `prim_list_q_test.go` - list?, test: proper lists, improper lists (false), circular (false)
- [ ] `prim_length_test.go` - length, test: empty list, proper lists
- [ ] `prim_append_test.go` - append, test: zero args, multiple lists, improper final arg
- [ ] `prim_reverse_test.go` - reverse, test: empty list, proper lists
- [ ] `prim_list_ref_test.go` - list-ref, test: first, middle, last, out of bounds error
- [ ] `prim_list_set_test.go` - list-set!, test: mutation at various indices
- [ ] `prim_list_tail_test.go` - list-tail, test: k=0, k=length
- [ ] `prim_make_list_test.go` - make-list, test: with/without fill value
- [ ] `prim_memq_test.go` - memq, test: found, not found, uses eq?
- [ ] `prim_memv_test.go` - memv, test: found, not found, uses eqv?
- [ ] `prim_member_test.go` - member, test: found, not found, uses equal?
- [ ] `prim_assq_test.go` - assq, test: alist lookup with eq?
- [ ] `prim_assv_test.go` - assv, test: alist lookup with eqv?
- [ ] `prim_assoc_test.go` - assoc, test: alist lookup with equal?

### Equality Predicates (3 primitives)
- [ ] `prim_eq_q_test.go` - eq?, test: identical objects, symbols, small integers
- [ ] `prim_eqv_q_test.go` - eqv?, test: numbers, characters, booleans
- [ ] `prim_equal_q_test.go` - equal?, test: deep comparison, lists, vectors, strings

### String Operations (20 primitives)
- [ ] `prim_string_length_test.go` - string-length, test: empty, ASCII, Unicode
- [ ] `prim_string_ref_test.go` - string-ref, test: first, last, out of bounds error
- [ ] `prim_substring_test.go` - substring, test: start/end indices, empty result
- [ ] `prim_string_append_test.go` - string-append, test: zero args, multiple strings
- [ ] `prim_string_to_list_test.go` - string->list, test: with/without start/end indices
- [ ] `prim_list_to_string_test.go` - list->string, test: empty list, character list
- [ ] `prim_string_upcase_test.go` - string-upcase
- [ ] `prim_string_downcase_test.go` - string-downcase
- [ ] `prim_string_to_symbol_test.go` - string->symbol
- [ ] `prim_symbol_to_string_test.go` - symbol->string
- [ ] `prim_string_to_number_test.go` (see Numeric Conversion)
- [ ] `prim_number_to_string_test.go` (see Numeric Conversion)

### String Comparisons (10 primitives)
- [ ] `prim_string_eq_test.go` - string=?, test: equal, not equal, empty strings
- [ ] `prim_string_lt_test.go` - string<?, test: lexicographic ordering
- [ ] `prim_string_gt_test.go` - string>?
- [ ] `prim_string_le_test.go` - string<=?
- [ ] `prim_string_ge_test.go` - string>=?
- [ ] `prim_string_ci_eq_test.go` - string-ci=?, test: case-insensitive
- [ ] `prim_string_ci_lt_test.go` - string-ci<?
- [ ] `prim_string_ci_gt_test.go` - string-ci>?
- [ ] `prim_string_ci_le_test.go` - string-ci<=?
- [ ] `prim_string_ci_ge_test.go` - string-ci>=?

### Character Operations (12 primitives)
- [ ] `prim_char_to_integer_test.go` - char->integer
- [ ] `prim_integer_to_char_test.go` - integer->char, test: valid/invalid code points
- [ ] `prim_char_upcase_test.go` - char-upcase
- [ ] `prim_char_downcase_test.go` - char-downcase
- [ ] `prim_char_foldcase_test.go` - char-foldcase
- [ ] `prim_digit_value_test.go` - digit-value, test: digits return value, non-digits return #f
- [ ] `prim_char_eq_test.go` - char=?
- [ ] `prim_char_lt_test.go` - char<?
- [ ] `prim_char_gt_test.go` - char>?
- [ ] `prim_char_le_test.go` - char<=?
- [ ] `prim_char_ge_test.go` - char>=?

### Vector Operations (6 primitives) - partially tested
- [ ] `prim_make_vector_test.go` - expand existing tests for BigInteger indices
- [ ] `prim_vector_length_test.go` - expand existing tests
- [ ] `prim_vector_ref_test.go` - expand existing tests, out of bounds error
- [ ] `prim_vector_set_test.go` - vector-set!, test: mutation
- [ ] `prim_vector_to_list_test.go` - expand existing tests
- [ ] `prim_list_to_vector_test.go` - expand existing tests

### Bytevector Operations (8 primitives) - partially tested
- [ ] `prim_make_bytevector_test.go` - expand for edge cases
- [ ] `prim_bytevector_length_test.go`
- [ ] `prim_bytevector_u8_ref_test.go`
- [ ] `prim_bytevector_u8_set_test.go`
- [ ] `prim_bytevector_copy_test.go`
- [ ] `prim_bytevector_copy_bang_test.go`
- [ ] `prim_bytevector_append_test.go`
- [ ] `prim_utf8_to_string_test.go` / `prim_string_to_utf8_test.go` (partially done)

### I/O Ports (25 primitives)
- [ ] `prim_open_input_file_test.go`
- [ ] `prim_open_output_file_test.go`
- [ ] `prim_open_binary_input_file_test.go`
- [ ] `prim_open_binary_output_file_test.go`
- [ ] `prim_open_input_string_test.go`
- [ ] `prim_open_output_string_test.go`
- [ ] `prim_open_input_bytevector_test.go`
- [ ] `prim_open_output_bytevector_test.go`
- [ ] `prim_get_output_string_test.go`
- [ ] `prim_get_output_bytevector_test.go`
- [ ] `prim_close_port_test.go`
- [ ] `prim_input_port_q_test.go`
- [ ] `prim_output_port_q_test.go`
- [ ] `prim_port_q_test.go`
- [ ] `prim_input_port_open_q_test.go`
- [ ] `prim_output_port_open_q_test.go`
- [ ] `prim_current_input_port_test.go`
- [ ] `prim_current_output_port_test.go`
- [ ] `prim_call_with_input_file_test.go`
- [ ] `prim_call_with_output_file_test.go`
- [ ] `prim_with_input_from_file_test.go`
- [ ] `prim_with_output_to_file_test.go`
- [ ] `prim_eof_object_test.go`
- [ ] `prim_eof_object_q_test.go`

### Read/Write (8 primitives)
- [ ] `prim_read_test.go`
- [ ] `prim_read_syntax_test.go`
- [ ] `prim_read_token_test.go`
- [ ] `prim_write_test.go`
- [ ] `prim_write_simple_test.go`
- [ ] `prim_write_shared_test.go`
- [ ] `prim_display_test.go`
- [ ] `prim_write_char_test.go`
- [ ] `prim_newline_test.go`

### Control Flow (8 primitives)
- [ ] `prim_apply_test.go` - apply, test: with list, with multiple args + list
- [ ] `prim_map_test.go` - map, test: single list, multiple lists, empty lists
- [ ] `prim_for_each_test.go` - for-each, test: side effects, return value
- [ ] `prim_call_cc_test.go` - expand existing tests for edge cases
- [ ] `prim_call_with_values_test.go` - test: producer/consumer pattern
- [ ] `prim_values_test.go` - values, test: zero, one, multiple values
- [ ] `prim_dynamic_wind_test.go` - test: before/after thunks, with continuations
- [ ] `prim_not_test.go` - not, test: #f->#t, everything else->#f

### Exception Handling (6 primitives)
- [ ] `prim_with_exception_handler_test.go`
- [ ] `prim_raise_test.go`
- [ ] `prim_raise_continuable_test.go`
- [ ] `prim_error_test.go`
- [ ] `prim_error_object_q_test.go`
- [ ] `prim_error_object_message_test.go`
- [ ] `prim_error_object_irritants_test.go`

### Promises (4 primitives)
- [ ] `prim_make_promise_test.go`
- [ ] `prim_make_lazy_promise_test.go`
- [ ] `prim_force_test.go` - test: memoization, multiple force calls

### Parameters (2 primitives)
- [ ] `prim_make_parameter_test.go` - test: with/without converter

### Environment/Eval (6 primitives)
- [ ] `prim_eval_test.go`
- [ ] `prim_environment_test.go`
- [ ] `prim_interaction_environment_test.go`
- [ ] `prim_scheme_report_environment_test.go`
- [ ] `prim_null_environment_test.go`
- [ ] `prim_load_test.go`

### Syntax Operations (8 primitives)
- [ ] `prim_datum_to_syntax_test.go`
- [ ] `prim_syntax_to_datum_test.go`
- [ ] `prim_identifier_q_test.go`
- [ ] `prim_bound_identifier_equal_q_test.go`
- [ ] `prim_free_identifier_equal_q_test.go`
- [ ] `prim_syntax_local_value_test.go`
- [ ] `prim_syntax_local_introduce_test.go`
- [ ] `prim_syntax_local_identifier_as_binding_test.go`
- [ ] `prim_make_compile_time_value_test.go`

### Expansion (3 primitives)
- [ ] `prim_expand_test.go`
- [ ] `prim_expand_once_test.go`
- [ ] `prim_compile_test.go`

### File System (2 primitives)
- [ ] `prim_file_exists_q_test.go`
- [ ] `prim_delete_file_test.go`

### Process/Environment (5 primitives)
- [ ] `prim_command_line_test.go`
- [ ] `prim_exit_test.go`
- [ ] `prim_emergency_exit_test.go`
- [ ] `prim_get_environment_variable_test.go`
- [ ] `prim_get_environment_variables_test.go`
- [ ] `prim_features_test.go`

### Time (4 primitives)
- [ ] `prim_current_second_test.go`
- [ ] `prim_current_jiffy_test.go`
- [ ] `prim_jiffies_per_second_test.go`
- [ ] `prim_time_test.go`

### Threading - SRFI-18 (3 primitives)
- [ ] `prim_thread_test.go`
- [ ] `prim_mutex_test.go`
- [ ] `prim_condvar_test.go`

### Go Concurrency (2 primitives)
- [ ] `prim_channel_test.go`
- [ ] `prim_sync_test.go` - WaitGroup, RWMutex, Once, Atomic

### Miscellaneous
- [ ] `prim_utils_test.go`

Code Refactoring (see REFACTORING_PROPOSAL.md)
----------------------------------------------
- [ ] Add `go/runtime/primitives/args.go` - helper functions for argument extraction (~600 lines saved)
- [ ] Add `go/runtime/primitives/fold.go` - variadic numeric fold helpers (~500 lines saved)
- [ ] Add `go/machine/operation_helpers.go` - EqualTo helper functions (~300 lines saved)
- [ ] Migrate ~150 primitive files to use new argument helpers
- [ ] Migrate ~27 operation files to use EqualTo helpers

R7RS Missing Features
---------------------

### Tokenizer (R7RS 7.1.1 Lexical Structure)

**Completely Missing:**
- Extended symbols (`|...|`): Tokenizer returns `ExtendedSymbolStart` for `|` but never reads contents or closing `|`. Parser doesn't handle this token. R7RS requires `|<symbol element>*|` where `<symbol element>` is any character except `\` or `|`, or escape sequences.
- Escapes inside extended symbols: Within `|...|`, R7RS requires `\a`, `\b`, `\t`, `\n`, `\r`, `\|`, `\\`, and `\x<hex>;`
- `\|` escape in strings: R7RS requires `\|` to produce vertical bar. Not in `readIntraStringEscape` (tokenizer.go:687-724)
- String line continuation: R7RS requires `\<intraline-whitespace>*<line-ending><intraline-whitespace>*` to escape nothing (continuation). Current code just adds whitespace chars (line 714-715)
- Hex escape semicolon terminator in strings: R7RS requires `\x<hex>;` format. Current code reads hex digits without expecting terminating semicolon (line 693)

**Partially Implemented:**
- Exponent markers: Only `e`/`E` supported. R7RS also allows `s`, `f`, `d`, `l` for short/single/double/long precision
- Inexact digit placeholder `#`: R7RS allows `#` in inexact numbers (e.g., `1.2###`). Not implemented

### Primitives

- Box primitives: `box`, `box?`, `unbox`, `set-box!` (Box type exists in values/box.go but no Scheme primitives registered)
- Hashtable primitives: `make-hashtable`, `hashtable?`, `hashtable-ref`, `hashtable-set!`, `hashtable-delete!`, `hashtable-keys`, `hashtable-values`, `hashtable-size`, `hashtable-copy`, `hashtable-clear!` (Hashtable type exists in values/hashtable.go but no Scheme primitives; also current implementation only supports string keys, not arbitrary Scheme values)
- BigInteger: No automatic promotion from Integer or `#bigint` reader syntax (BigInteger type exists in values/big_integer.go)
- BigFloat: No automatic promotion from Float or `#bigfloat` reader syntax (BigFloat type exists in values/big_float.go)

Library Status
--------------

| Library               | Status |
|-----------------------|--------|
| scheme/base           | ~98%   |
| scheme/char           | 100%   |
| scheme/file           | 100%   |
| scheme/write          | 100%   |
| scheme/r5rs           | 0%     |
| (others)              | 100%   |

---

Dead Code Removal
-----------------

### Phase 5 (Optional): Test-Only Functions
- [ ] Consider deleting `AddScopeToSet()` and `RemoveScopeFromSet()` in `go/syntax/scope_utils.go`

### Phase 6 (Optional): Fix Method Signature
- [ ] Fix `Error.Unwrap()` to return `error` instead of `*String` for Go compatibility

---

Future Extensions
-----------------

### Standard Libraries

**Network Libraries (Racket-compatible)**
- TCP/UDP sockets (tcp-connect, tcp-listen, tcp-accept, tcp-close)
- HTTP client/server primitives
- SSL/TLS support
- DNS resolution

**OS Libraries (Racket-compatible)**
- Process execution (subprocess, system, system*)
- Process control (kill, wait)
- Fork/exec primitives
- Environment variables (getenv, putenv)
- File system operations beyond R7RS (permissions, symlinks, stat)
- Signal handling

**Unit Testing Library**
- Test case definition (test, test-case, test-suite)
- Assertions (check-equal?, check-true, check-false, check-exn)
- Test runners with reporting
- Setup/teardown fixtures

**Logging Library**
- Log levels (debug, info, warn, error, fatal)
- Structured logging with key-value pairs
- Multiple outputs (console, file, custom handlers)
- Log formatting and filtering

### Multithreading

See **DESIGN_MULTITHREADING.md** for full implementation plan.

**Approach:** SRFI-18 standard API + Go-native extensions (channels, sync primitives)

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | Thread infrastructure, thread-safe globals | Not started |
| 2 | Basic thread primitives (make-thread, thread-start!, thread-join!) | Not started |
| 3 | Mutex primitives (make-mutex, mutex-lock!, mutex-unlock!) | Not started |
| 4 | Condition variables | Not started |
| 5 | Go channels (make-channel, channel-send!, channel-receive, channel-select) | Not started |
| 6 | Go sync extensions (WaitGroup, RWMutex, Once, atomics) | Not started |
| 7 | Exception handling integration | Not started |
| 8 | call/cc and dynamic-wind integration | Not started |

**Key challenges:**
- MachineContext thread safety (each thread gets own context)
- Global environment synchronization (RWMutex on bindings)
- call/cc scope limited to single thread
- dynamic-wind cleanup on thread termination

### Programmatic Tokenization and Parsing

See **DESIGN_PROGRAMMATIC_READER.md** for full implementation plan.

Expose tokenizer and parser to Scheme code for building custom readers, REPLs, and tooling.

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | Token introspection (token?, token-type, token-value, etc.) | Not started |
| 2 | Syntax introspection (syntax?, syntax-line, syntax-column, etc.) | Not started |
| 3 | EOF handling improvements | Not started |
| 4 | Advanced reader control (optional) | Not started |

### POSIX API (SRFI-170)

See **DESIGN_POSIX.md** for full implementation plan.

Comprehensive POSIX API implementing SRFI-170 with Go-native implementation.

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | File information (stat, file-info) | Not started |
| 2 | Permissions and ownership | Not started |
| 3 | Links and directories | Not started |
| 4 | Temp files and misc operations | Not started |
| 5 | Environment variables | Not started |
| 6 | Process execution (subprocess, system) | Not started |
| 7 | Signal handling | Not started |
| 8 | User/group database | Not started |
| 9 | Terminal control | Not started |
| 10 | Error handling (SRFI-198) | Not started |

**Key features:**
- File system operations (stat, permissions, links, directories)
- Process control (subprocess, system, signals)
- User and group database access
- Terminal control
- Platform-aware (Unix vs Windows)

### Racket-style Scribble Syntax (At-Expressions)

Support for Racket's `@`-reader syntax for inline documentation and text processing.

**Syntax forms:**
- `@id{text}` — Function call with text argument: `(id "text")`
- `@id[arg ...]{text}` — Function call with args and text: `(id arg ... "text")`
- `@{text}` — Literal text string
- `|{text}|` — Verbatim text (no escaping)

**Implementation phases:**
1. Tokenizer: Recognize `@` as reader dispatch character
2. Parser: Handle `@`-expression forms and text blocks
3. Integration: Enable/disable via reader flag or `#lang at-exp`

**References:**
- https://docs.racket-lang.org/scribble/reader.html
- https://docs.racket-lang.org/at-exp/index.html

### Arbitrary Precision Numbers
Tagged literals: `#bigint`, `#bigfloat` using Go's `big.Int` and `big.Float`.

### Go FFI
Registry-based (Phase 1) → Reflection-based (Phase 2) → Plugin support (Phase 3).
See detailed design notes in DESIGN_GO_FFI.md.

### Runtime Source Location Tracking

Track variable definition sites and enable source-level debugging at runtime.

See **DESIGN_SOURCE_TRACKING.md** for full implementation plan.

**Implementation status:** ✅ Infrastructure complete

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | Binding source locations | ✅ Complete |
| 2 | Source map in templates | ✅ Complete |
| 3 | Stack traces | ✅ Complete |
| 4 | Debugger support (breakpoints, stepping) | ✅ Complete |

**What's implemented:**
- `Binding.source` field for variable definition sites
- `SourceMap` type for PC → source location mapping
- `NativeTemplate.sourceMap` and `NativeTemplate.name` fields
- `StackFrame` and `StackTrace` types
- `SchemeError` type with source location and stack trace
- `Debugger` type with breakpoints and stepping modes
- `MachineContext.CaptureStackTrace()` and `CurrentSource()` methods
- Debugger integration in VM `Run()` loop

**Remaining work to fully utilize:**
- Wire up compilation to record source locations in source map
- Wire up error handling to use `SchemeError` with `CaptureStackTrace()`
- Create debugger REPL or IDE integration (e.g., Debug Adapter Protocol)

---

Tokenizer Refactoring Notes
---------------------------
Potential helper functions to reduce ~200-300 lines:
1. `readRadixPrefix` — consolidate #b/#o/#d/#x handling
2. `readBooleanLiteral` — consolidate #t/#true and #f/#false
3. `scanKeyword` — unify scan(), scanCaseInsensitive(), readToken()
4. `readDecimalFractionWithExponent` — extract decimal+exponent pattern
5. `readImaginarySuffix` — consolidate imaginary number suffixes
6. `readExplicitSignNumber` — consolidate +/- number handling
7. `advanceOrError` — combine next() + error check
8. `checkDelimiter` — replace inline delimiter checking
9. `readInfNan` — consolidate inf.0/nan.0 parsing

Reflection 
----------
procedures for reflection into the environment - lists of bound symbol names.  Parameters for procedures.  Types and predicates for types.

Event Callbacks
---------------
variables to hold event callback methods for expansion, compiling and some low level runtime functions (setting values - for debugging).

