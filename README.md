wp-local-tactic
===============

A Frama-C/WP plug-in that lets you attach a **Rocq (Coq) proof script to an ACSL
contract**, next to the specification, and have Frama-C run Rocq and report the
result like any other prover.


A motivating example
--------------------

Suppose you want WP to establish that the matrix

```
        [  2  -1 ]
    M = [ -1   2 ]
```

is positive semidefinite. `M` is PSD iff its quadratic form is nonnegative for
every vector, i.e.

```
    x·M·x = 2·x² - 2·x·y + 2·y² ≥ 0    for all real x, y.
```

That polynomial is a sum of squares, `(x - y)² + x² + y²`. No SMT prover
discharges it reliably, but Rocq's `nra` (`Psatz`) finds the certificate in one
line — and so does the dedicated `validsdp` tactic (Cholesky / SDP certificate).

Today, using Rocq from WP means: run `frama-c -wp -wp-prover coq
-wp-interactive=…`, let WP write a `.v` stub for the goal, open it in an editor,
figure out how WP phrased the goal, write the tactic, add the `Require` lines,
save, and replay. Every goal, by hand, out of band from the C file.

With this plug-in you write the proof where the property is:

```c
/* examples/psd_matrix.c */

/*@ ensures psd:
      \forall real x, y; 2.0 * x * x - 2.0 * x * y + 2.0 * y * y >= 0.0;
  @ assigns \nothing;
  @ rocq_script "intros x y. nra." ;
*/
void m_is_psd(void) { return; }
```

and run

```console
$ frama-c -wp -local-tactic -local-tactic-rocq-import Reals,Psatz \
    examples/psd_matrix.c -then -report
...
  Coq 9.1.1:       1 (200ms)
...
[  Valid  ] Post-condition 'psd'
```

The plug-in asked WP to generate the goal's `.v`, inserted `Require Import
Reals.` / `Require Import Psatz.` and `Proof. intros x y. nra. Qed.`, compiled it
with `coqc`, and committed the `Valid` verdict back into WP so it shows up in
`-wp-status`, `-report` and the GUI. The generated `.v` stays on disk
(`.frama-c/wp/interactive/<goal>.v`) so you can inspect or debug it.

`examples/psd_matrix.c` also carries a second contract proved with
`rocq_script "intros x y. ltac2:(validsdp)."` — the dedicated SDP/Cholesky
tactic. That one needs `coq-validsdp`; see the Appendix.


Installation
------------

### Requirements

* **Frama-C 32.x** (Germanium) with the WP plug-in and OCaml ≥ 4.14.
* **Rocq/Coq** registered in the Why3 configuration. Check with:

  ```console
  $ why3 config list-provers
  Coq 9.1.1
  ...
  ```

  If Rocq is missing, install it and run `why3 config detect`.

### Build and install the plug-in

```console
$ dune build
$ dune install            # into the current opam switch; Frama-C then auto-loads it
```

Without installing, load it explicitly on the command line:

```console
$ frama-c -load-module ./_build/default/LocalTactics.cmxs -wp -local-tactic file.c
```

Any Rocq tactic from the switch's standard library (`nra`, `lra`, `field`, …)
works with the default setup. For the `validsdp` tactic, or to run a `coqc` that
lives in a **different opam switch** than Frama-C, follow
[Appendix — using `validsdp` (or another opam switch's Rocq)](#appendix--using-validsdp-or-another-opam-switchs-rocq)
at the end of this document.

### Smoke test

```console
$ frama-c -wp -local-tactic test.c -then -report
...
  Coq 9.1.1:       3 (190ms)
...
[  Valid  ] Lemma 'sq_nonneg_lemma'
[  Valid  ] Post-condition 'sq_nonneg'
[  Valid  ] Post-condition 'sq_nonneg2'
```

`examples/readable_proofs.c` walks through three syntax extensions covered below
(multi-line scripts, `rocq_alias`, auto-`intros` naming) in one file:

```console
$ frama-c -wp -local-tactic -local-tactic-rocq-import ZArith,Lia \
    examples/readable_proofs.c -then -report
```

`examples/loop_invariant.c` covers a fourth, `rocq_loop_script`:

```console
$ frama-c -wp -local-tactic -local-tactic-rocq-import Lia \
    examples/loop_invariant.c -then -report
```

`examples/assert.c` covers a fifth, `rocq_assert_script`, for a plain `assert`:

```console
$ frama-c -wp -local-tactic -local-tactic-rocq-import Lia \
    examples/assert.c -then -report
```


Writing annotations
-------------------

### `rocq_script` — inline script

A `rocq_script` clause in a function or statement contract carries a literal Rocq
proof script:

```c
/*@ ensures sq_nonneg: \forall integer k; k * k >= 0;
  @ assigns \nothing;
  @ rocq_script "intros k. apply ZArith.BinInt.Z.square_nonneg." ;
*/
void f(void) { return; }
```

Note there is **no `:` after the keyword** (`rocq_script "…"`, not
`rocq_script: "…"`): the ACSL grammar for extension clauses expects the keyword
followed directly by its argument.

### `rocq_strategy` + `\by(…)` — a named, reusable script

Declare a script once as a global annotation and reference it by name:

```c
/*@ rocq_strategy SquareNonNeg:
      "intros k. apply ZArith.BinInt.Z.square_nonneg." ;
*/

/*@ ensures sq_nonneg2: \forall integer k; k * k >= 0;
  @ assigns \nothing;
  @ rocq_script \by(SquareNonNeg) ;
*/
void g(void) { return; }
```

### Targeting one post-condition

When a contract has several post-conditions, prefix the script with the
post-condition label to apply it to that one only:

```c
/*@ ensures a: P;
  @ ensures b: Q;
  @ rocq_script b: "..." ;   // proves `b`; `a` is left to the other provers
*/
```

Without a label, the script is used for every `ensures` of the contract.

### `rocq_proof` — attach a script to a lemma

A `lemma` (or `axiom`) is not a contract, so it has no clause to hang a script
on. Instead, a **global** `rocq_proof` annotation names the lemma:

```c
/*@ lemma sq_nonneg: \forall integer k; k * k >= 0; */
/*@ rocq_proof sq_nonneg: "intros k. apply ZArith.BinInt.Z.square_nonneg." ; */

/*@ axiomatic Reals {
  @   lemma sq_nonneg_r: \forall real x; x * x >= 0.0;
  @ }
*/
/*@ rocq_proof sq_nonneg_r: \by(SquareNonNeg) ; */
```

`rocq_proof` takes a string literal or `\by(Name)`, exactly like `rocq_script`.
Lemmas anywhere in the file are matched by name, including inside an `axiomatic`
or `module`; an unknown name is warned and skipped. (The keyword is `rocq_proof`
rather than `rocq_script` because ACSL will not let one extension keyword be used
at two grammar levels — contract clause *and* global annotation.)

Lemmas are the cleanest target: WP emits exactly one proof obligation per lemma,
stated as the ACSL predicate itself, with none of the machine-integer or
`\at`-label hypotheses a contract obligation carries.

### `rocq_loop_script` — attach a script to a loop invariant

A loop invariant is not a behavior clause either (it isn't part of any
`requires`/`ensures`/`assigns` block), so `rocq_script` doesn't reach it. Use
`rocq_loop_script`, written with the `loop` keyword right next to `loop
invariant` / `loop assigns` / `loop variant`:

```c
/*@ loop invariant inv: 2 * s == i * (i - 1);
  @ loop rocq_loop_script inv: "nia." ;
  @ loop assigns i, s;
  @ loop variant n - i;
*/
while (i < n) { s += i; i++; }
```

Like `rocq_script`, an optional `label:` selects one named invariant among
several; without one, the script applies to every `loop invariant` on that
loop (a `for behavior:`-restricted one included). WP splits an invariant
into up to two proof obligations — "established" (true on entry) and
"preserved" (true again after one iteration) — and `rocq_loop_script` is
simply handed to whichever of the two actually needs an interactive prover;
the other is often closed by WP's own simplifier before Coq is ever
invoked, with nothing left to splice. The bare `invariant P;` *code*
annotation (no `loop` prefix — an assertion-style invariant, unrelated to
any specific loop) is a different ACSL construct and is not targeted.

### `rocq_assert_script` — attach a script to a plain `assert`

A bare `assert` is a *code annotation*, not a behavior clause, so
`rocq_script` doesn't reach it either. Use `rocq_assert_script`, written in
its own `/*@ ... */` comment **immediately after** the `assert` it targets:

```c
/*@ assert sq: n * n >= 0; */
/*@ rocq_assert_script sq: "nia." ; */
```

Frama-C allows only one code annotation per comment, so the `assert` and its
script cannot share a single `/*@ ... */` block the way an `ensures` and
`rocq_script` can share a behavior, or a `loop invariant` and
`rocq_loop_script` can share a loop's annotation list — each becomes its own
statement, and `rocq_assert_script` locates its target as the `assert`
immediately preceding it in the control-flow graph. This means it must come
right after the `assert` it targets, with nothing else — not even another
plain statement — in between; if two `assert`s are stacked back to back, only
the one directly above the script is reachable, and the (still optional)
`label:` is then just a sanity check that catches pointing at the wrong one:

```c
/*@ assert trivial: x == x; */
/*@ assert nonlinear: x * x >= 0; */
/*@ rocq_assert_script nonlinear: "nia." ; */   // targets 'nonlinear', not 'trivial'
```

Without a label, the script applies to whatever single `assert` immediately
precedes it.

### Multi-line scripts

A `rocq_script` / `rocq_strategy` / `rocq_proof` body can be **several string
literals** instead of one long one; they are joined with a newline. A
standalone `"(* ... *)"` string is just a Rocq comment, so this is a way to
narrate a proof step by step instead of cramming it onto one line:

```c
/*@ rocq_proof sq_nonneg:
      "(* squares of integers are nonnegative *)",
      "intros k.",
      "apply ZArith.BinInt.Z.square_nonneg." ;
*/
```

`\by(Name)` can appear as one of the segments too, mixed with plain strings.
`rocq_strategy` accepts the same comma-separated form (string segments only,
no `\by` inside a recipe definition).

### `rocq_alias` — a friendly name for a lemma's Coq axiom

WP compiles an ACSL `lemma foo` to a Rocq axiom named `Q_foo` wherever another
goal needs it as a hypothesis (`Wp.Lang.lemma_id`) — a name you would
otherwise only discover by opening the generated `.v`. `rocq_alias` gives it a
name of your choosing:

```c
/*@ lemma sq_nonneg: \forall integer k; k * k >= 0; */
/*@ rocq_alias sq_nonneg: "SqNonNeg"; */

/*@ ensures uses_lemma: \forall integer k; k * k >= 0;
  @ assigns \nothing;
  @ rocq_script "apply SqNonNeg." ;
*/
void g(void) { return; }
```

Wherever the plugin splices a generated `.v` that actually declares
`Q_sq_nonneg` as an axiom, it inserts `Notation SqNonNeg := Q_sq_nonneg.`
right there, so any script — in that file or another — can write `apply
SqNonNeg.` instead. An alias whose lemma axiom never shows up in any file
generated by the run is reported with a warning (likely a typo in the lemma
name); two lemmas cannot claim the same alias.

### Readable hypothesis names

Why3's own proof skeleton introduces every hypothesis generically (`intros i
h1 h2 h3.`), independently of the `requires` clauses' ACSL names. When a
script does **not** start with its own `intros`, the plugin prepends one that
reuses the `requires` names instead:

```c
/*@ requires pos: x > 0;
  @ requires bound: x < 100;
  @ ensures range: x * x < 10000;
  @ assigns \nothing;
  @ rocq_script "nia." ;
*/
void h(int x) { return; }
```

produces a spliced proof of the form:

```coq
Proof.
(* local-tactic intros: h1 -> pos, h2 -> bound *)
intros i pos bound h3.
nia.
Qed.
```

so the script itself can refer to `pos` / `bound` directly. This is
best-effort (see *Modeling choices* below for when it does and doesn't fire)
and never silent: the substitution actually made is always echoed as a Rocq
comment right above the `intros` line, and a script that already starts with
its own `intros` is left completely untouched (so existing scripts are
unaffected).

### Options

| Option | Meaning |
| --- | --- |
| `-local-tactic` | enable the pipeline (off by default) |
| `-local-tactic-rocq-import lib.mod,...` | libraries to load in **every** generated `.v`. `x.y` → `From x Require Import y.`; a bare `x` → `Require Import x.` |
| `-local-tactic-prover name` | WP prover that checks the generated `.v` (default `Coq`). Set it to a prover registered via `-wp-why3-extra-config` to use a `coqc` from another opam switch — see the Appendix. |

`-wp` on the command line is optional: `-local-tactic` invokes WP's obligation
generator itself. Passing `-wp` just means WP's own proving runs first (the
plug-in copes with goals it already closed — see *How it executes*). To skip that
redundant SMT attempt on goals you intend to prove with Rocq, add
`-wp-prover none`.


Modeling choices
----------------

**A plug-in-owned clause, not `\wp::strategy`.** Recent WP has its own
`\wp::strategy` / `\wp::proof` ACSL annotations for naming proof recipes and
attaching them to properties. They cannot carry a Rocq script or request extra
libraries, and their set of "alternatives" (`\prover`, `\tactic`, `\auto`, …) is
closed — a plug-in cannot add a `\rocq` case. So `wp-local-tactic` registers its
own extensions (`rocq_script`, `rocq_strategy`) through `Acsl_extension`. You may
write them bare, or fully qualified as `\local-tactic::rocq_script …` (which is
how Frama-C prints them back with `-print`).

**The script is inline text, spliced verbatim.** The string you write is dropped,
unchanged, between `Proof.` and `Qed.` of the theorem WP generates. It must
therefore close the goal *as WP phrases it* — with WP's variable names, its
machine-integer hypotheses (`is_sint32 …`), its real-number encoding, and so on.
The practical workflow is: run once, open
`.frama-c/wp/interactive/<goal>.v`, read the actual `Theorem wp_goal : …`
statement, then write the script against it. The file is kept precisely so you
can iterate this way.

**Libraries are a global command-line option, not per-clause.** `Require`
directives rarely differ from one goal to the next in a given development, so
`-local-tactic-rocq-import` applies one list to every generated file. Entries are
comma-separated; `mathcomp.all_ssreflect` becomes `From mathcomp Require Import
all_ssreflect.`, `Lia` becomes `Require Import Lia.`. The lines are inserted just
before the `(* Why3 goal *)` marker, i.e. after Why3's own preamble and before
the goal, where `From … Require Import …` is always legal.

**The verdict goes through WP.** The plug-in does not emit its own property
status; it drives WP's normal proving path with the Rocq prover, so a proved goal
is reported as `Valid (Coq)` and consolidated exactly like an Alt-Ergo or Z3
result — visible to `-wp-status`, `-report`, `-wp-report-json`, the GUI, and any
downstream consolidation.

**Scope: function and statement contracts, loop invariants, and asserts.**
`rocq_script` is a *behavior* extension, so it lives among `requires` /
`ensures` / `assigns`. It is read from both function contracts and statement
contracts. Statement contracts additionally trigger a pre-existing WP message
(`Statement specifications not yet supported (skipped)`) and are handled at
whole-function granularity; function contracts are the cleaner target.
`rocq_loop_script` is a separate *loop* annotation for the same reason
`rocq_proof` is a separate *global* one: ACSL won't let a single extension
keyword be used at two grammar levels, and a loop invariant is neither a
behavior clause nor a global. `rocq_assert_script` is a plain *code*
annotation for the same reason again — an `assert` is neither — but unlike
the other three, each `/*@ ... */` comment becomes its own CIL statement (only
one code annotation is allowed per comment), so it cannot share its target's
annotation list the way a `loop rocq_loop_script` shares the loop's; instead
it locates its target as the `assert` immediately preceding it in the CFG.

**Auto-`intros` naming is reconstructed, not observed, and says so when it
can't be trusted.** WP exposes a goal's hypotheses as `Conditions.sequent`
(`Wpo.compute`), a list of *steps* each carrying the ACSL property it came
from — but that list is not a literal preimage of the printed `forall`/`->`
chain: memory-model bookkeeping (`State` steps) has no printed counterpart,
and machine-integer range facts (`Type` steps, `is_sintN`/`is_uintN`) print
*after* the real hypotheses rather than in their internal order. Both are
corrected for. When a goal shares a computed value across hypotheses via a
printed `let` (typically a contract whose `ensures` mentions `\result` more
than once, or in a compound expression), Why3 can introduce further range
hypotheses that never appear in `Conditions.sequent` at all — invisible from
the plug-in's side. As a hard safety net, the reconstructed hypothesis count
is always cross-checked against an independent count of top-level `->` in the
printed statement; any mismatch means no renaming happens at all, rather than
a wrong one. In practice this means the feature reliably fires for lemmas and
for `ensures` clauses that don't route a shared value through a `let`, and
quietly does nothing otherwise — the script still compiles, just with Why3's
own generic hypothesis names.


How it executes
---------------

`-local-tactic` registers a Frama-C main action. If `-wp` is also on the command
line it runs first; either way the plug-in then drives WP's obligation generator
and prover interface directly. It collects its targets — the `ensures` properties
of every contract carrying a `rocq_script` (filtered by label if present), every
loop invariant named by a `rocq_loop_script`, every `assert` immediately
preceding a `rocq_assert_script`, and every lemma named by a `rocq_proof`,
with `\by(Name)` resolved against the declared `rocq_strategy` recipes — then:

**0. Save and override WP options.** It records and later restores
`-wp-prover`, `-wp-interactive` and `-wp-status-valid`. It sets the prover to
`-local-tactic-prover` (default `Coq`) and `-wp-status-valid` to *true* — the
latter because WP's goal generator skips any property already marked `Valid`, and
a preceding `-wp` run will typically have closed these with Alt-Ergo. Each target
is also `remove`d from WP's goal table so it is rebuilt from scratch.

**Pass 1 — generate the `.v`.** With `-wp-interactive=update`, it asks WP to
(re)generate the proof obligation. WP's Why3 → Rocq driver writes
`.frama-c/wp/interactive/<goal>.v` containing its preamble, the
`Theorem wp_goal : …` statement, and a placeholder `Proof. … Qed.`. WP then runs
`coqc` on that placeholder; it fails (the proof is a stub), and that failure is
expected and ignored — pass 1 only exists to produce the file. Any stale file for
a target is deleted first so generation always starts clean.

**Splice.** The plug-in rewrites each generated file: it inserts the
`-local-tactic-rocq-import` lines just before `(* Why3 goal *)`, and replaces
everything between `Proof.` and the closing `Qed.` / `Admitted.` with
`Proof.`, your script, `Qed.`.

**Pass 2 — check.** With `-wp-interactive=batch`, it asks WP to prove the same
goals again. In batch mode WP does **not** regenerate the `.v` — it compiles the
file exactly as it is on disk (so the spliced imports and script survive) and
turns the `coqc` result into the property's verdict, committed through WP's
standard mechanism.

Afterwards the options from step 0 are restored and the `.v` files are left in
place.

Because this is a Frama-C main action, `-then` stages re-run it. That is
idempotent (same goals, same result) but repeats the two passes; if you only want
a report, put `-then -report` after a single `-local-tactic` and expect the
pipeline to have run once per stage.


Limitations
-----------

* The script must match WP's exact phrasing of the goal; there is no
  normalization, and `requires`-name `intros` inference is best-effort (see
  *Modeling choices*) — it quietly does nothing on a goal it can't safely
  reconstruct, rather than guessing.
* Statement-contract post-conditions work but at whole-function granularity, with
  the WP `Statement specifications not yet supported` message.
* Pass 1 spends one (failing) `coqc` invocation per goal on the stub.
* One `rocq_script` per behavior; use several behaviors, or several contracts, for
  per-clause scripts beyond the single `label:` selector.
* `rocq_proof` targets `lemma` / `axiom` by name (contract post-conditions use
  `rocq_script`); there is currently no name-based binding for a post-condition
  declared in another part of the file.
* `rocq_alias` only helps once a lemma is already proved and used as a
  hypothesis elsewhere; it has nothing to alias inside the lemma's own proof.
* `rocq_loop_script` targets *normal* loop invariants (`loop invariant ...;`,
  `for behavior: loop invariant ...;`) only, not the unrelated bare
  `invariant P;` code annotation. Its script is not split between the
  "established" and "preserved" sub-goals; the same one is used for
  whichever of the two isn't already closed by WP's own simplifier.


Background: what changed from the 2013 version
----------------------------------------------

The original plug-in (Frama-C *Fluorine*) used an `axiomatic` enum of tactic
names and a `PROOF_TACTIC (use_strategy (X))` clause that mapped each constant to
a `-wp-tactic <name>` setting, then ran WP's native Rocq output on the annotated
properties.

None of that exists anymore: WP removed its native Rocq backend and the
per-property `-wp-tactic` mechanism; Rocq is now reachable only as a Why3
*interactive* prover producing a `.v` you complete yourself. The plug-in was
rewritten against the Frama-C 32 API (`Boot.Main.extend`, `Acsl_extension`, the
`frama-c-wp.core` library API, dune build) and re-centered on the capability WP
still lacks: the proof script and its libraries living in the C file, with Rocq
run automatically and the verdict flowing back through WP.


Appendix — using `validsdp` (or another opam switch's Rocq)
----------------------------------------------------------

**Skip this section** unless you want the `validsdp` tactic, or you need to prove
with a `coqc` that lives in a different opam switch than Frama-C. The default
setup (Frama-C's own switch, `-local-tactic-prover Coq`) already gives you every
tactic in that switch's Rocq standard library — `nra`, `lra`, `field`, `psatz`,
`ring`, …

### Why two switches

`validsdp` drags in a large stack (`mathcomp`, `coqeal`, `coq-interval`,
`coq-flocq`, `osdp`, an SDP solver). Putting all of that in the Frama-C switch is
often undesirable, and `coq-validsdp 1.1.1` additionally needs
`coq-mathcomp-field` in the **2.3 – 2.4** range (it does not build against
`mathcomp` 2.5). So keep it in its own switch.

You cannot simply point Why3 at the other switch's `coqc`. Every `.v` Why3
generates begins with `Require Import BuiltIn.` &c. — Why3's own Coq *realisation*
library — and a Rocq `.vo` loads only under the exact Rocq build that produced it
(a foreign one fails with `inconsistent assumptions over Corelib.Init.Prelude`).
Those realisations must be **recompiled with the other switch's `coqc`**, and a
Why3 prover pointed at that copy. `tools/setup-alt-rocq.sh` does exactly this.

### Step 1 — the Frama-C switch

```console
$ opam switch create framac ocaml-base-compiler.5.4.1
$ eval $(opam env --switch framac)
$ opam install frama-c            # pulls why3, why3-coq, alt-ergo …
$ opam install rocq-prover        # the default Rocq (any 9.x)
$ why3 config detect              # register Alt-Ergo, Coq, … with Why3
$ why3 config list-provers        # expect a "Coq <version>" line
```

Then build this plug-in in that switch:

```console
$ git clone <this repo> && cd wp-local-tactic
$ dune build && dune install      # Frama-C now auto-loads it
```

### Step 2 — a second switch with `validsdp`

Use the **same OCaml/Rocq major versions** as switch 1 (here OCaml 5.4.1,
Rocq 9.1). An SDP solver must be on `PATH` — CSDP is the simplest
(`brew install csdp`, or `apt install coinor-csdp`); SDPA also works.

```console
$ opam switch create validsdp ocaml-base-compiler.5.4.1
$ eval $(opam env --switch validsdp)
$ opam repo add rocq-released https://rocq-prover.org/opam/released

$ opam install conf-csdp                                   # SDP solver binary
$ opam pin add -n osdp https://github.com/Embedded-SW-VnV/osdp.git

# coq-validsdp 1.1.1 requires mathcomp < 2.5 — pin the field library first
$ opam install coq-mathcomp-field.2.4.0 coq-validsdp.1.1.1
```

Quick check that the tactic itself works in this switch:

```console
$ cat > /tmp/t.v <<'EOF'
Require Import Reals. From ValidSDP Require Import validsdp.
Local Open Scope R_scope.
Goal forall x y : R, 0 <= 2*x*x - 2*x*y + 2*y*y.
Proof. intros x y. ltac2:(validsdp). Qed.
EOF
$ coqc /tmp/t.v && echo OK
```

### Step 3 — back in the Frama-C switch, bridge the two

```console
$ eval $(opam env --switch framac)
$ cd wp-local-tactic
$ tools/setup-alt-rocq.sh validsdp CoqValidSDP
```

This

* recompiles Why3's Coq realisations with the `validsdp` switch's `coqc` into
  `~/.local/share/why3-validsdp/`, and
* writes `~/.local/share/why3-validsdp.conf`, declaring a Why3 prover
  **`CoqValidSDP`** whose command is that switch's
  `coqtop -R ~/.local/share/why3-validsdp Why3 -l %f`.

Re-run it whenever the `validsdp` switch's Rocq toolchain changes. It needs the
Why3 source tree that opam keeps under
`~/.opam/framac/.opam-switch/sources/why3.<version>/` — present after a normal
`opam install why3`.

### Step 4 — prove with `validsdp`

`validsdp` is an **Ltac2** notation, so call it as `ltac2:(validsdp)`:

```c
/*@ ensures psd:
      \forall real x, y; 2.0*x*x - 2.0*x*y + 2.0*y*y >= 0.0;
  @ assigns \nothing;
  @ rocq_script "intros x y. ltac2:(validsdp)." ;
*/
void m_is_psd(void) { return; }
```

```console
$ frama-c -wp -local-tactic \
    -local-tactic-prover CoqValidSDP \
    -wp-why3-extra-config ~/.local/share/why3-validsdp.conf \
    -local-tactic-rocq-import Reals,ValidSDP.validsdp \
    examples/psd_matrix.c -then -report
...
  CoqValidSDP 9.1.1:    1 (2.9s)
[  Valid  ] Post-condition 'psd_validsdp'
```

Frama-C/WP runs in the `framac` switch; it drives the `validsdp` switch's
`coqtop`; `validsdp` runs its SDP solver plus the formally verified Cholesky
check; the verdict comes back as `Valid (CoqValidSDP)`.

### Reusing this for any switch / tactic

`tools/setup-alt-rocq.sh <switch> <ProverName>` works for any second switch —
e.g. one with a newer Rocq, or a custom tactic library. Then pass
`-local-tactic-prover <ProverName> -wp-why3-extra-config
~/.local/share/why3-<switch>.conf` and the relevant `-local-tactic-rocq-import`.
`-local-tactic-prover` must name a **distinct** Why3 prover: WP ignores Why3
prover *alternatives*, so the script registers `<ProverName>` as its own prover,
not as `Coq (…)`.

### Troubleshooting

| Symptom | Cause / fix |
| --- | --- |
| `inconsistent assumptions over Corelib.Init.Prelude` | the realisation copy is stale or was built with the wrong `coqc` — re-run `tools/setup-alt-rocq.sh`. |
| `prover '<name>' not found in the Why3 configuration` | wrong `-local-tactic-prover`, or `-wp-why3-extra-config` not passed / wrong path. `why3 config --extra-config=<conf> list-provers` should list it. |
| `Error: The reference validsdp was not found` | script used bare `validsdp` instead of `ltac2:(validsdp)`, or `ValidSDP.validsdp` not in `-local-tactic-rocq-import`. |
| `setup-alt-rocq.sh`: *realisation sources not found* | `opam install why3` again in the Frama-C switch so opam re-fetches its source tree. |


License
-------

LGPL-2.1-only. Copyright © 2026 Pierre-Loïc Garoche, ENAC.
Contact <ploc@garoche.net>.

This is a complete reimplementation for Frama-C 32 (dune build, the integrated
Why3→Rocq pipeline, the `rocq_script` / `rocq_strategy` extensions,
`tools/setup-alt-rocq.sh`); it shares no code with the original 2013 plug-in.
Implemented with the help of Claude (Anthropic).
