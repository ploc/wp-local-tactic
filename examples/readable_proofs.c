/* Three syntax extensions that make proof scripts easier to read and write.
 *
 *   frama-c -wp -local-tactic -local-tactic-rocq-import ZArith,Lia \
 *     examples/readable_proofs.c -then -report
 */

/* --- 1. a multi-line script -------------------------------------------
 *
 * A [rocq_script]/[rocq_strategy]/[rocq_proof] body can be several string
 * literals instead of one long line; they are joined with a newline. A
 * standalone "(* ... *)" string is just a Rocq comment, so this is a way
 * to narrate a proof step by step, right next to the ACSL it proves. */

/*@ lemma sq_nonneg: \forall integer k; k * k >= 0; */

/*@ rocq_proof sq_nonneg:
      "(* squares of integers are nonnegative *)",
      "intros k.",
      "apply ZArith.BinInt.Z.square_nonneg." ;
*/

/* --- 2. rocq_alias -- a friendly name for a lemma's Coq axiom ---------
 *
 * WP compiles an ACSL lemma 'foo' to a Rocq axiom named 'Q_foo' wherever
 * another goal needs it as a hypothesis (Wp.Lang.lemma_id) -- a name you
 * would otherwise have to discover by opening the generated .v. rocq_alias
 * gives it a name of your choosing, usable from any later rocq_script. */

/*@ rocq_alias sq_nonneg: "SqNonNeg"; */

/*@ ensures uses_lemma: \forall integer k; k * k >= 0;
  @ assigns \nothing;
  @ rocq_script "apply SqNonNeg." ;
*/
void uses_lemma_alias(void)
{
  return;
}

/* --- 3. readable hypothesis names --------------------------------------
 *
 * Why3's own proof skeleton introduces every hypothesis with a generic
 * name ("intros i h1 h2 h3."), independently of the requires clauses'
 * ACSL names. When a script does not start with its own 'intros', the
 * plugin prepends one that reuses the 'requires' names instead -- shown
 * here as a Rocq comment so the substitution is never silent. This is
 * best-effort: it only fires when the mapping between WP's sequent and
 * the printed goal can be verified (see README, "Modeling choices"); a
 * script that already starts with its own 'intros' is left untouched. */

/*@ requires pos: x > 0;
  @ requires bound: x < 100;
  @ ensures range: x * x < 10000;
  @ assigns \nothing;
  @ rocq_script "nia." ;
*/
void auto_intros(int x)
{
  return;
}

/* Expected:
 *   [  Valid  ] Lemma 'sq_nonneg'
 *   [  Valid  ] Post-condition 'uses_lemma'
 *   [  Valid  ] Post-condition 'range'
 */
