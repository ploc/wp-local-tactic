/* Attaching a Rocq proof script to a plain 'assert'.
 *
 *   frama-c -wp -local-tactic -local-tactic-rocq-import Lia \
 *     examples/assert.c -then -report
 *
 * A bare 'assert' is a code annotation, not a behavior clause, so
 * 'rocq_script' -- which lives among 'requires'/'ensures'/'assigns' -- does
 * not reach it. 'rocq_assert_script' is written right next to the 'assert'
 * it targets, in the same annotation block, exactly the way WP's own
 * '\wp::probe' sits next to an assert: both end up as code annotations
 * attached to the same statement, and the plugin matches them up there.
 *
 * Unlike a loop invariant (a *loop* annotation, attached to the statement
 * that follows it), an assert and its script sit at the same point, so
 * 'rocq_assert_script' is registered as an ordinary code annotation, not a
 * "next loop" one.
 */

/*@ requires n >= 0; */
void square_nonneg(int n)
{
  /*@ assert sq: n * n >= 0; */
  /*@ rocq_assert_script sq: "nia." ; */
  return;
}

/* Two asserts at the same point: the label picks out one, leaving the other
 * to WP's other provers (here 'trivial' is closed by Alt-Ergo on its own). */
void two_asserts(int x)
{
  /*@ assert trivial: x == x; */
  /*@ assert nonlinear: x * x >= 0; */
  /*@ rocq_assert_script nonlinear: "nia." ; */
  return;
}

/* Without a label, the script applies directly (there's only one assert
 * here, so no label is needed to disambiguate). */
void single_assert(int x)
{
  /*@ assert x * x >= 0; */
  /*@ rocq_assert_script "nia." ; */
  return;
}

/* Expected:
 *   [  Valid  ] Assertion 'sq'
 *   [  Valid  ] Assertion 'trivial'
 *   [  Valid  ] Assertion 'nonlinear'
 *   [  Valid  ] Assertion
 */
