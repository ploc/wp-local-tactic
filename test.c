/* Demonstration of the LocalTactics plugin.
 *
 *   frama-c -wp -local-tactic test.c
 *
 * Each 'rocq_script' clause carries a Rocq/Coq proof script that the plugin
 * splices into the proof obligation's generated .v file (under
 * .frama-c/wp/interactive/), then discharges with coqc through WP.
 */

/*@ ensures sq_nonneg: \forall integer k; k * k >= 0;
  @ assigns \nothing;
  @ rocq_script "intros k. apply ZArith.BinInt.Z.square_nonneg." ;
*/
void inline_example(void)
{
  return;
}

/* A named recipe, declared once and referenced by \by(...). */
/*@ rocq_strategy SquareNonNeg:
      "intros k. apply ZArith.BinInt.Z.square_nonneg." ;
*/

/*@ ensures sq_nonneg2: \forall integer k; k * k >= 0;
  @ assigns \nothing;
  @ rocq_script \by(SquareNonNeg) ;
*/
void recipe_example(void)
{
  return;
}

/* A lemma has no contract clause, so it is targeted by a global 'rocq_proof'
 * that names it (see also examples/lemmas.c). */
/*@ lemma sq_nonneg_lemma: \forall integer k; k * k >= 0; */
/*@ rocq_proof sq_nonneg_lemma: \by(SquareNonNeg) ; */
