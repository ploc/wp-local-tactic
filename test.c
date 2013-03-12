/*@ axiomatic ellipsoids_proof_tactics {
  @   type ellipsoids_tactics = Intuition | Tactic2;
  @   predicate use_strategy (ellipsoids_tactics t);
  @ }
*/

/*@ requires x < 0;
  @ ensures \result <= 0; 
*/
int plus_one (int x) {
int y,z;
/*@ ensures y <= 0; 
  @ PROOF_TACTIC (use_strategy (Intuition));
*/
{
y = x + 1;
}
/*@ ensures z <= 0; 
  @ PROOF_TACTIC (use_strategy (Tactic2));
*/
{
z = 2 * y;
}
return z;
}

