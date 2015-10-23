Ideas for a crazy type theory

Syntax:
 * Forth-style stack syntax
 * Two- or N-dimensional syntax a la Brainfuck: operator arities are
   directions in an n-dimensional space. E.g. the identity type takes
   a type argument upwards, an element to the left, and an element
   below, then the syntax might involve rotations of subterms. We can
   have a max operator arity of 2^n in an n-dimensional program.
   * Generalizations: interfaces between different dimensionalities of
     program
   * Wormhole operator: connect two things across the space
   
Semantics:
 * The Postulate that Shall Not Be Named: have a postulate in the
   theory that is needed to prove something or other. Hide clues to
   its name. This postulate is only available in our implementation.
 * Negative / reciprocal types: what op makes op A + A uninhabited?
