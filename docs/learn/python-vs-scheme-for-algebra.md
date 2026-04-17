# Python vs Scheme for Algebra

Imagine you want to write a program that takes the expression *x² + 2xy + y²* and
simplifies it to *(x + y)²*. Not computes it for specific numbers — *rewrites* it.
The program needs to look inside the expression, recognize patterns, and produce a
new expression.

Here's the question that splits these two languages apart: **what is an expression
made of?**


## Two Answers to the Same Question

In Scheme, you'd write that expression as:

```scheme
(+ (* x x) (* 2 x y) (* y y))
```

That's a list. The first element is `+`, the rest are its operands. Each operand is
itself a list. You can take it apart with `car` and `cdr`, traverse it recursively,
pattern-match on it. It's data. It's also valid code — if `x` and `y` are bound to
numbers, you can evaluate it. Same structure, two roles.

In Python (using SymPy), you'd write:

```python
x, y = symbols('x y')
expr = x**2 + 2*x*y + y**2
```

What is `expr`? It's an `Add` object containing `Pow(x, 2)`, `Mul(2, x, y)`, and
`Pow(y, 2)` as children. Each of those is an instance of a class that inherits from
`Expr`, which inherits from `Basic`. To inspect the tree, you use `.args` (the
children) and `.func` (the node type). It's an object graph with methods.

Both languages built an expression tree. But Scheme's tree is the language's native
data structure — the list. Python had to construct a parallel universe of classes to
represent what Scheme gets for free.


## Why That Difference Matters: Term Rewriting

Symbolic algebra is, at its core, **term rewriting**. You have rules like:

- *a + a* → *2a*
- *a · 0* → *0*
- *d/dx (f · g)* → *f' · g + f · g'*

Each rule says: "if the expression matches this pattern, replace it with that." This
is the beating heart of every computer algebra system — Mathematica, Maxima, Maple,
SymPy. They all do term rewriting. They differ in how natural the language makes it.

In Scheme, a simplification rule looks like this:

```scheme
(define (simplify expr)
  (match expr
    [(list '+ a a)           (list '* 2 a)]
    [(list '* a 0)           0]
    [(list '* 0 a)           0]
    [(list '+ a 0)           a]
    [(list 'deriv (list '* f g) x)
     (list '+ (list '* (list 'deriv f x) g)
              (list '* f (list 'deriv g x)))]
    [_ expr]))
```

Each `match` clause is a rewrite rule. The pattern is on the left. The replacement is
on the right. Both are lists — the same data structure as the expressions themselves.
Writing a rewrite rule and writing an expression are the same activity.

In SymPy, the equivalent uses `.replace()` with `Wild` patterns, or a custom
function that walks `.args`:

```python
from sympy import Wild, symbols, Mul, Add

a, b = Wild('a'), Wild('b')
expr = expr.replace(Mul(a, 0), lambda a: Integer(0))
expr = expr.replace(Add(a, a), lambda a: 2*a)
```

It works. But notice: you're operating in a meta-language. You construct `Wild`
objects that represent pattern variables. You pass lambdas for the replacement. The
pattern and the expression look nothing alike — one is a `Wild`-decorated class
construction, the other is an operator-overloaded Python expression. They're the same
thing conceptually but they don't share a syntax.

This is the homoiconicity advantage, concretely. Not a theoretical nicety about code
and data being the same — a practical consequence. **In Scheme, the pattern language
for rewriting expressions IS the expression language.** In Python, you need a DSL
on top.


## The Numeric Tower: Where Exactness Matters

There's a subtler structural advantage. Scheme (R7RS) has an exact numeric tower
built in: integers of arbitrary size, exact rationals (3/7 stays 3/7, not
0.42857142857...), and a clear distinction between exact and inexact numbers.

Why does this matter for algebra? Consider simplifying:

```
(1/3)x + (2/3)x → x
```

With exact rationals, `1/3 + 2/3 = 1` and you're done. With floating-point,
`0.333... + 0.666...` might give you `0.999...`, and now your simplifier has to
decide whether that's close enough to `1`. Floating-point contaminates symbolic
reasoning.

SymPy solves this by building its own numeric types — `Integer`, `Rational`, `Float`
— that sit alongside Python's built-in `int` and `float`. It works, but it means
every boundary between SymPy and regular Python is a potential contamination point.
Write `x + 0.5` instead of `x + Rational(1, 2)` and you've introduced an inexact
number into your symbolic expression.

Scheme's numeric tower isn't bolted on. It's the language's arithmetic. Every
operation preserves exactness unless you explicitly request otherwise. This removes an
entire category of bugs from symbolic computation.


## Where Python Wins Decisively: Analytic Computation

Now flip the question. Suppose you don't want to simplify *x² + 2xy + y²*
symbolically. You want to evaluate it for a million values of *x* and *y*, find
the minimum, plot the surface, compute a numerical integral over some region.

This is **analytic** (numerical) algebra, and Python's ecosystem is overwhelming:

- **NumPy**: array-oriented arithmetic that dispatches to optimized C/Fortran BLAS
  routines. A million evaluations happen in one vectorized call, not a million
  interpreter iterations.
- **SciPy**: numerical integration, optimization, linear algebra, sparse matrices,
  signal processing, statistics — all calling into LAPACK, FFTW, and friends.
- **matplotlib/Jupyter**: visualization and interactive exploration that make
  numerical results tangible.

Scheme has nothing comparable. The standard library gives you exact arithmetic on
individual numbers, but no array-oriented computation, no BLAS bindings, no plotting.
You can build these — SCMUTILS (Sussman and Wisdom's system at MIT) includes numerical
integrators and differential equation solvers — but you're building infrastructure that
Python inherits from decades of scientific computing investment.

This isn't a language limitation. It's an ecosystem gap. Scheme's numeric operations
are fine one-at-a-time. But scientific computing needs bulk operations on arrays, and
that requires either FFI to Fortran/C or a JIT compiler. Python solved this by
admitting that the inner loop shouldn't be Python — it's C underneath. Scheme hasn't
made that trade (with rare exceptions like Chez Scheme's FFI).


## SCMUTILS: The Proof That Scheme Can Do It

The strongest evidence that Scheme is genuinely good for algebra — not just
theoretically, but in practice — is SCMUTILS, the system Gerald Jay Sussman and Jack
Wisdom built at MIT for their classical mechanics course.

SCMUTILS does both symbolic and numeric computation in MIT/GNU Scheme. It can
differentiate functions symbolically, simplify the results, and then numerically
integrate the differential equations that fall out. It handles generic arithmetic
(the same `+` works on numbers, symbolic expressions, vectors, matrices, functions)
through Scheme's dispatch mechanisms.

Sussman and Wisdom chose Scheme for a specific reason: **mathematical notation lies
to you about its own structure.** When a physicist writes *L(q, q̇, t)*, the
Lagrangian looks like a function of three independent variables. But *q̇* is the time
derivative of *q* — they're not independent. Traditional notation hides this. Scheme's
functional abstraction makes it explicit:

```scheme
(define ((L-free-particle mass) local)
  (let ((v (velocity local)))
    (* 1/2 mass (dot-product v v))))
```

The Lagrangian is a function that returns a function. The structure of the
computation is visible in the code. You can differentiate it, compose it, pass it to
a numerical integrator. The same expression serves as a symbolic formula and a
computable procedure.

This dual use — expression as formula AND as computation — is what homoiconicity
buys you in practice. Not "code is data" in the abstract, but "I can differentiate
my program."

## The Real Comparison

The question "Python or Scheme for algebra?" is actually two questions wearing a
trench coat.

**For symbolic manipulation** — rewriting expressions, applying algebraic identities,
symbolic differentiation, proving equivalences — Scheme has structural advantages that
are real and consequential. The expression IS the data structure. Pattern matching IS
term rewriting. The numeric tower preserves exactness. You're working with the grain
of the language.

Python can do all of this through SymPy, but SymPy is essentially a Lisp-style
symbolic system implemented in Python. It builds its own expression trees, its own
pattern matcher, its own numeric types. It's good — battle-tested and well-documented
— but it's a layer of abstraction that Scheme doesn't need.

**For numerical/analytic computation** — evaluating expressions over arrays, numerical
integration, optimization, linear algebra — Python has an ecosystem that Scheme
can't touch. NumPy and SciPy aren't just libraries; they're bridges to decades of
Fortran and C numerical code. And the Jupyter notebook environment makes exploration
interactive in a way that no Scheme REPL matches.

**For the hybrid** — symbolic derivation followed by numerical evaluation — both
languages have a path. SymPy's `lambdify()` turns a symbolic expression into a NumPy
function. SCMUTILS passes symbolic results to numerical integrators internally. But
Python's bridge is more polished because the numerical side is more mature.

## What Would Break

Remove homoiconicity from Scheme and you lose the ability to write rewrite rules in
the same syntax as expressions. You'd need to build what SymPy built — a class
hierarchy, a pattern DSL, a visitor protocol. This is exactly what happened when
computer algebra moved from Lisp (Macsyma, 1968) to C++ and Java in the 1990s.
Systems got faster but harder to extend. The rule that a mathematician could read and
modify became an `AbstractExpressionVisitor` that only a software engineer could
navigate.

Remove Python's scientific ecosystem and you lose the numerical side entirely. The
language itself — without NumPy — has no story for bulk numerical computation.
`for x in range(1000000): result += f(x)` is hundreds of times slower than
`np.sum(f(xs))`.

The constraint each language chose tells you what it optimized for. Scheme optimized
for **structural transparency** — making the bones of computation visible and
manipulable. Python optimized for **ecosystem breadth** — making it easy to connect
to existing numerical infrastructure. Both are legitimate. They're just different
bets.


## Sources

- [Computer algebra system (Wikipedia)](https://en.wikipedia.org/wiki/Computer_algebra_system)
- [Fateman — Building Algebra Systems by Overloading Lisp](https://people.eecs.berkeley.edu/~fateman/generic/overload-small.pdf)
- [SymPy Architecture (Brown University)](https://www.cfm.brown.edu/people/dobrush/am33/SymPy/architecture.html)
- [SymPy: symbolic computing in Python (PeerJ)](https://peerj.com/articles/cs-103/)
- [SymPy Expression Manipulation (docs)](https://docs.sympy.org/latest/tutorials/intro-tutorial/manipulation.html)
- [SCMUTILS Reference Manual (MIT)](https://groups.csail.mit.edu/mac/users/gjs/6946/refman.txt)
- [Emmy (SICM reimplementation in Clojure)](https://github.com/mentat-collective/emmy)
- [MIT OCW: Classical Mechanics Tools](https://ocw.mit.edu/courses/earth-atmospheric-and-planetary-sciences/12-620j-classical-mechanics-a-computational-approach-fall-2008/tools/)
- [SymPy vs. Mathematica (wiki)](https://github.com/sympy/sympy/wiki/SymPy-vs.-Mathematica)
