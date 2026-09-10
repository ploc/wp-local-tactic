/* Attaching Rocq proof scripts to ACSL lemmas.
 *
 *   frama-c -wp -local-tactic \
 *     -local-tactic-rocq-import Reals \
 *     examples/lemmas.c -then -report
 *
 * A lemma is not a contract, so there is no clause to carry the script.
 * A global `rocq_proof <lemma>: ...` names the lemma instead. It accepts a
 * string literal or `\by(Name)`, just like `rocq_script`.
 */

/* --- inline script, top-level lemma ------------------------------------- */

/*@ lemma sq_nonneg: \forall integer k; k * k >= 0; */

/*@ rocq_proof sq_nonneg: "intros k. apply ZArith.BinInt.Z.square_nonneg." ; */

/* --- named recipe, lemma inside an axiomatic -------------------------- */

/*@ rocq_strategy SqNonNegR: "intros x. apply Rle_0_sqr." ; */

/*@ axiomatic RealFacts {
  @   lemma sq_nonneg_r: \forall real x; x * x >= 0.0;
  @ }
*/

/*@ rocq_proof sq_nonneg_r: \by(SqNonNegR) ; */

/* Expected:
 *   [  Valid  ] Lemma 'sq_nonneg'
 *   [  Valid  ] Lemma 'sq_nonneg_r'
 */
