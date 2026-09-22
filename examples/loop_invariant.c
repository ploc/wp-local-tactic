/* Attaching a Rocq proof script to a loop invariant.
 *
 *   frama-c -wp -local-tactic -local-tactic-rocq-import Lia \
 *     examples/loop_invariant.c -then -report
 *
 * A loop invariant is not a behavior clause (it isn't part of any
 * `requires`/`ensures`/`assigns` block), so `rocq_script` -- which lives
 * among those -- doesn't apply to it. Instead, `rocq_loop_script` is a
 * *loop* annotation, written with the `loop` keyword next to `loop
 * invariant` / `loop assigns` / `loop variant`, and targets the invariant
 * by name exactly like `rocq_script` targets an `ensures`.
 *
 * WP splits a loop invariant into up to two proof obligations --
 * "established" (true on entry) and "preserved" (true again after one
 * iteration, assuming it held before) -- each becoming its own goal.
 * `rocq_loop_script` is not aware of that split; it is simply given to
 * whichever of the two actually needs an interactive prover (here, only
 * "preserved": maintaining 2*s == i*(i-1) across `s += i; i++;` is a
 * genuinely nonlinear step Alt-Ergo won't close on its own, while
 * "established" -- 2*0 == 0*(0-1) && 0 <= n -- is closed by WP's own
 * simplifier before Coq is even invoked). The `i <= n` conjunct carries no
 * proof difficulty of its own; it is there so the invariant is strong
 * enough to pin `i = n` at loop exit, which the postcondition needs.
 */

/*@ requires n >= 0;
  @ ensures \result == n * (n - 1);
  @ assigns \nothing;
  @ rocq_script "nia." ;
*/
int sum2(int n)
{
  int i = 0, s = 0;
  /*@ loop invariant inv: 2 * s == i * (i - 1) && i <= n;
    @ loop rocq_loop_script inv: "nia." ;
    @ loop assigns i, s;
    @ loop variant n - i;
  */
  while (i < n) {
    s += i;
    i++;
  }
  return 2 * s;
}

/* Expected:
 *   [  Valid  ] Post-condition (via rocq_script, same "nia" step)
 *   [  Valid  ] Invariant 'inv'
 */
