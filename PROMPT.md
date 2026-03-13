This file is for teaching AI how to use Que Script correctly and avoid common mistakes.

Que Script is a **functional, expression-only, Lisp-style language** using **S-expressions**.

---

### Core

- Everything is an **expression**
- Expressions **always return a value**
- Lists are function calls: `(f a b)`
- The **last expression** is the result
- No statements

---

### Bindings & Scope

- `(let name value)` — immutable, returns **Unit (`0`)**
- Rebinding in the same scope is illegal
- `do` sequences expressions and returns the last one
- `do` does **not** create a new scope
- Lambda boundaries create scope; if you need a fresh scope, use a lambda

```lisp
(do
  (let x 1)
  (let y 2)
  (+ x y))
```

---

### Functions

- `(lambda arg1 arg2 body)`
- Last form is always the body
- Called as `(f x y)`
- Use `do` for multi-expression function bodies

---

### Destructuring Patterns

Lambda parameters can destructure tuples/vectors directly.

- Tuple destructuring: `(lambda { a b } body)`
- Vector destructuring: `(lambda [a b c] body)`
- `.` is a skip marker:
  - In vectors, `[a b .]` means bind first items and ignore the rest.
  - In tuples, `{ a . }` or `{ . b }` means skip one tuple slot.
- For vectors, the rest marker position is always last.
- Destructuring can be combined across multiple parameters and nested patterns.

```lisp
; tuple destructuring
(let pick-right (lambda { . b } b))
(pick-right { 10 99 }) ; 99

; vector destructuring with rest ignored
(let first-two-sum (lambda [a b .] (+ a b)))
(first-two-sum [4 5 6 7]) ; 9

; mixed destructuring across multiple args
(let merge (lambda { x y } { . b } { (+ x y) b }))
(merge { 1 2 } { true 10 }) ; { 3 10 }
```

---

### When to use `do`

Top level already behaves like a `do` block.
Inside lambdas, use `do` when there is more than one expression.

---

### Recursion

- `let*` defines recursive functions
- TCO is available in wasm builds
- VM forbids returning closures that capture outer scope

---

### Control Flow

- `(if cond then else)`
- `(cond c1 e1 c2 e2 ... default)`
- `(unless cond expr nil)`
- `and` / `or` short-circuit
- Side-effect `if` can be one-armed: `(if cond effect)`; Unit is implied (no explicit `nil` needed)

**Important:** branches must unify to one type.

```lisp
; good: both Int
(if flag 1 0)

; bad: Int vs String
; (if flag 1 "nope")
```

---

### Loops

- `(while condition body)` — canonical looping form (body is a normal expression, usually `do`)
- `loop` forms may introduce lambda scope; avoid them for `mut`-based counters/state
- No `break`; use mutable flags
- Returns **Unit (`0`)**

---

### Pipe

- `(|> x f g h)` means `(h (g (f x)))`
- Left to right, value flows through

---

### Variadic Forms

- Arithmetic operators are variadic:
  - Int: `+`, `-`, `*`, `/`
  - Float: `+.`, `-.`, `*.`, `/.`
- `cons` is also variadic and is useful for building strings/chars without deep nesting.

```lisp
(*. a b c d)
(-. a b c)
(cons "tick " (Integer->String t) " shots " (Integer->String s))
```

---

### Types

- Primitives: `Int Float Bool Char`
- Composite:
  - `[T]` — homogeneous vector
  - `{A * B}` — tuple
  - `A -> B` — function
- String is `[Char]`

**Important:** vectors are homogeneous.

```lisp
; good
[1 2 3]

; bad
; ["x" 1]
```

---

### Mutation (Explicit)

Immutable by default. Mutable state is boxed:

```lisp
(integer i 0)
(boolean b false)
(variable x 42)
(get i) (++ i) (set i 10)
```

Vector mutation: `push!`, `set!`, `pull!`, `pop!`

Que also supports lightweight primitive scalar mutation:

```lisp
(mut i 0)
(alter! i (+ i 1))
i
```

- `mut` is only for primitive scalars: `Int`, `Float`, `Bool`, `Char`
- `alter!` updates the binding in place
- Unlike boxed scalars, `mut` values are read directly (`i`), no `(get i)`
- `mut` is local to its current lambda scope. Do not use it across any lambda boundary.
- If state must be shared across lambda scopes, use boxed mutation (`integer`, `floating`, `boolean`, or `variable`).
- Hybrid pattern: read boxed value once with `get`, copy to local `mut`, do local `alter!` updates, then write back once with `set`.

### Impure Function Naming

Functions that cause side effects must end with `!`.

- Mutating outside/local state (`set!`, `push!`, `alter!`, boxed `set`, etc.) => function name ends in `!`
- I/O side effects (`print!`, `read!`, `sleep!`, `clear!`, command/file ops) => function name ends in `!`
- Pure functions (no mutation, no I/O) should not use `!`
- If a function mutates one of its args, the mutated value must be the **first arg**
- If a function needs to mutate multiple values, pass a tuple as the first arg and mutate fields inside that tuple (for example `{ frame depth }`)

```lisp
; pure
(let sum2 (lambda a b (+ a b)))

; impure (I/O)
(let log-line! (lambda s (print! s)))

; impure (mutates external vector)
(let append-one! (lambda xs (set! xs (length xs) 1)))

; impure (mutates multiple values through first tuple arg)
(let clear-buffers! (lambda { output zbuffer } (do
  (set! output 0 sp)
  (set! zbuffer 0 127))))
```

---

### Unit

- Effect-only expressions return **Unit**
- Runtime Unit value is `0`
- `nil` is Unit

---

### Type System

- Hindley–Milner inference
- No type annotations required

---

### Std/aliases

This project commonly uses aliases from `std/fp.lisp` and `std/ds.lisp`.
Prefer existing helpers (for example `map`, `filter`, `sum`, `odd?`, `square`, `String->Integer`, `String->Vector`, `Set/new`) instead of re-implementing them.
Std helpers are auto-imported in scripts; use built-ins directly when available (for example `sin`, `cos`).

---

### Common Gotchas (Important for AI)

1. `ARGV` values are strings (`[Char]`), not numbers.

```lisp
(let part (get ARGV 1))
(match? part 1)
(String->Integer (get ARGV 2))
```

2. Script args are passed **before** `--allow`.
   Use quotes only when an arg contains spaces.

```bash
que myfile.que 1 2 3000 42 --allow read write
que myfile.que "Hello world" 42 --allow read
```

3. `read!` path resolution is relative to the **script file location**.
   If script is `scripts/aoc/day1.que`, then input path should usually be like `inputs/day1.txt`.

4. Branch result types must match (in `if` / `cond`).
   If needed, return sentinel `-1` for invalid args instead of mixing with strings.

5. `zip` takes a tuple of vectors, not two separate args.
   Use tuple input form or use explicit loops when unsure.

6. `mut` is only for primitive scalars (`Int`, `Float`, `Bool`, `Char`).
   For vectors/tuples/functions, use boxed mutation (`variable`, `set`, `set!`, `push!`, etc.).
7. `mut` does not cross lambda scopes (including loop lambdas).
   For `mut`-driven loops, use `while` with explicit local counters (`mut i 0`, `alter! i (+ i 1)`).
8. If a value must be mutated across lambda scopes, use boxed mutation (`integer`, `floating`, `boolean`, or `variable`) instead of `mut`.
9. Prefer hybrid style for readability with shared refs: `get` boxed -> local `mut` + `alter!` -> final `set` to boxed.
10. `do` does not create scope. Re-declaring the same `let` name in different `if`/`cond` branches inside one lambda still conflicts.
11. For terminal animation, avoid relying on raw ANSI escape/control chars. Use `clear!` or newline-scroll fallback when ANSI handling is unavailable.
12. Prefer one buffered write per frame (`Vector->String` + single `print!`) over per-cell printing.
13. Char comparisons use char operators (`=#`, `<#`, `>#`, etc.), not numeric/int equality operators.
14. In this runtime, `=#` is the valid char equality op (not `#=`).
15. Prefer std trig directly (`sin`, `cos`) when available instead of re-implementing series approximations.
16. For large nested S-expression refactors, edit one function at a time and run `--debug` after each step to avoid cascading parenthesis errors.

---

### CLI Flags

Que supports a help command and richer debugging output.

- `--help` shows CLI usage and exits.
- `--no-result` runs the script without printing the final evaluated result.
- `--debug` (no mode) is the first step and is usually enough.
- Default run shows compiler/runtime error location in terminal.
- `--debug code` prints desugared code.
- `--debug types` prints inferred type trace.
- `--debug all` prints both desugared code and types.

```bash
que --help
que myfile.que --no-result
que myfile.que
que myfile.que --debug
que myfile.que --debug code
que myfile.que --debug types
que myfile.que --debug all
```

Token-efficient debugging flow:

1. Run `que myfile.que --debug` first (fewest extra tokens, often enough).
2. If error is still ambiguous, choose one targeted mode:
   - `--debug code` for desugaring/syntax-sugar issues.
   - `--debug types` for type mismatch/inference issues.
3. Use `--debug all` only when both views are needed.
4. If parser reports unresolved unclosed forms, immediately switch to small-scope edits and re-run after each function-level change.

---

### Useful Functions

Libraries are tree-shaken: use what you need.

```lisp
map        (T -> K) -> [T] -> [K]
filter     (T -> Bool) -> [T] -> [T]
reduce     (A -> T -> A) -> A -> [T] -> A
range      Int -> Int -> [Int]
sum        [Int] -> Int
product    [Int] -> Int
every?/some? (T -> Bool) -> [T] -> Bool
zip        {[T] * [K]} -> [{T * K}]
unzip      [{T * K}] -> {[T] * [K]}
```

---

### Example: AoC-Style runner with ARGV

```lisp
(let parse (lambda input
  (|> input
      (String->Vector std/char/new-line)
      (filter not-empty?)
      (map String->Integer))))

(let part1 (lambda xs (sum xs)))
(let part2 (lambda xs (if (empty? xs) 0 (maximum xs))))

(let run (lambda argv
  (if (< (length argv) 2)
      -1
      (do
        (let input (read! (get argv 0)))
        (let part (get argv 1))
        (let parsed (parse input))
        (cond
          (match? part "1") (part1 parsed)
          (match? part "2") (part2 parsed)
          -1)))))

(run ARGV)
```
