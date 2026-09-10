/* Positive-semidefiniteness of a concrete matrix, proved through Rocq.
 *
 *          [  2  -1 ]
 *      M = [ -1   2 ]        (eigenvalues 1 and 3)
 *
 * M is positive semidefinite iff its quadratic form is nonnegative for every
 * vector, i.e.  x^T M x = 2*x^2 - 2*x*y + 2*y^2 >= 0  for all real x, y.
 * That polynomial is a sum of squares:  (x - y)^2 + x^2 + y^2.
 *
 * ---------------------------------------------------------------------------
 * Variant 1 -- works out of the box (Rocq standard library only).
 *
 *   frama-c -wp -local-tactic -local-tactic-rocq-import Reals,Psatz \
 *     examples/psd_matrix.c
 *
 * `nra` (Psatz) finds the sum-of-squares certificate itself.
 *
 * To prove both post-conditions in one run, import both and select the
 * `validsdp` toolchain (see Variant 2):
 *
 *   frama-c -wp -local-tactic -local-tactic-prover CoqValidSDP \
 *     -wp-why3-extra-config ~/.local/share/why3-<switch>.conf \
 *     -local-tactic-rocq-import Reals,Psatz,ValidSDP.validsdp \
 *     examples/psd_matrix.c -then -report
 *   ...
 *   [  Valid  ] Post-condition 'psd_nra'
 *   [  Valid  ] Post-condition 'psd_validsdp'
 */

/*@ ensures psd_nra:
      \forall real x, y; 2.0 * x * x - 2.0 * x * y + 2.0 * y * y >= 0.0;
  @ assigns \nothing;
  @ rocq_script psd_nra: "intros x y. nra." ;
*/
void m_is_psd_nra(void)
{
  return;
}

/* ---------------------------------------------------------------------------
 * Variant 2 -- the `validsdp` tactic (Cholesky / SDP certificate).
 *
 * `validsdp` needs `coq-validsdp` (and `mathcomp`, `coqeal`, `osdp`, an SDP
 * solver). If that stack lives in a *different* opam switch than Frama-C, bridge
 * it once with  tools/setup-alt-rocq.sh <switch> CoqValidSDP  and run:
 *
 *   frama-c -wp -local-tactic -local-tactic-prover CoqValidSDP \
 *     -wp-why3-extra-config ~/.local/share/why3-<switch>.conf \
 *     -local-tactic-rocq-import Reals,ValidSDP.validsdp \
 *     examples/psd_matrix.c
 *
 * `validsdp` is an Ltac2 notation, so it is called as `ltac2:(validsdp)` from
 * the (Ltac1) proof script that the plug-in splices in.
 */

/*@ ensures psd_validsdp:
      \forall real x, y; 2.0 * x * x - 2.0 * x * y + 2.0 * y * y >= 0.0;
  @ assigns \nothing;
  @ rocq_script psd_validsdp: "intros x y. ltac2:(validsdp)." ;
*/
void m_is_psd_validsdp(void)
{
  return;
}
