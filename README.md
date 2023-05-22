# An interpreter by fixpoint computation

This is an assignment for a course in Software Verification. 

# The REC language

The REC language syntax and semantics is specified in <i>Winskel, G. (Glynn) - The formal semantics of programming languages : an introduction</i>. 

# The interpreter

The assignment's aim is the implementation of an interpreter that computes the fixpoint of an environment of functions. 
The algorithm computes the Kleene-Knaster-Tarski sequence, until an environment that is enough defined to provide a result is computed.

Note that:
- when the function diverges, the algorithm diverges as well;
- furthermore, the optimized "stack" computation does not incur into stack overflow when this happens.

