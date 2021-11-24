(*








    - See todo in [rule_match] for how to handle sequence and for loops in that function
    - define [trm_fun args body] as short for [trm_let_fun "" args body]

    - the script for replacing the loop, at the "basic" level, will be as follows:
        -- put  the [bag_iter it =] instruction with the [for] loop into a sequence
        -- call the new [Function.uninline] operation, which is the opposite of inlining,
           on bag_ho_iter_basic, to replace the sequence with a term of the form
             trm_app "bag_ho_iter_basic" [trm_var "b"; trm_fun ["p",_] body)].
            // beware that the term obtained is not valid C code, but it will be valid in C23.
        -- replace bag_ho_iter_basic with bag_ho_iter_chunk
        -- inline bag_ho_iter_chunk
        -- beta-reduce the call [trm_app (trm_fun ["p",_] body) [trm_var "p"]
           to simply [body]   (here, "p" is replaced with "p"); this is a new transformation.


    - Details on Function.uninline  // for now, we only support unit type functions
        - target a term [t]   (usually a sequence)
        - consider a function definition   [void f(x) {body}]
        - the idea is to replace [t] with [f(a)] for the right argument a
        - this is done by comparing [t] and [body], to find the instantiation of [x]
          that corresponds to [a]; this isi mplemented using [rule_match].

*)