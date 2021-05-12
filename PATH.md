A path is a list of constraints on how to explore a term in order to find some
subterm(s).

There are two kinds of constraints: the ones that give instructions on which
subterm to explore while resolving the rest of the path and the ones describing
particular nodes to go through before continuing.

All constraints take an optional boolean argument `strict` that, when set to
true, forbids recursive exploration for path resolution.

First the constraints giving the next subterm to explore:
- `cNth n`: go to the `n`-th element of a seq, an array or a struct term.
- `cCond ()`: go to the condition of an if statement, a for loop or a switch
  statement.
- `cThen ()`: go to the then branch of an if statement.
- `cElse ()`: go to the else branch of an if statement.
- `cBody ()`: go to the body of a for/while loop, a definition or a case in a
  switch, to the result in a return statement or to the term under a label.
- `cInit ()`: go to the initialisation instruction of a for loop.
- `cStep ()`: go to the step statement of a for loop.
- `cAppFun ()`: in a function application `f args`, go to `f`.
- `cArg n`: go to the `n`-th argument of a function in a application or a
  declaration.
- `cName ()`: go to the name of a declared variable/function or to the label of
  a labelled term.
- `cDircase n cd`: follow `cd` in the `n`-th case group of a switch
  statement. `cd` is either `cCaseName m` (go to the `m`-th case in the group)
  or `cCaseBody` (go to the body of the case group).
- `cEnumConst n ecd`: follow `ecd` in the `n`-th constant of an enum
  declaration. `ecd` is either `cEnumConstName` (go to the name of the constant)
  or `cEnumConstVal` (go to the value of the constant).
- `cList_int p next`: match the path `p` against all elements of the current seq
  contruct/of the arguments list of the current function. `next` is a function
  which takes the result as a list of booleans and that outputs the list of
  indices of the subterms to explore next in path resolution.
- `cList p next`: same as `cList_int` but `next` outputs a list of booleans
  (`true` for "explore", `false` for "do not explore") instead of a list of
  indices.
- `cFirst p`: basTyp_struct ( [
            ( g, Typ_int );
            ( t, Typ_array ( Typ_var T, Trm_val ( Val_lit ( Lit_int 2 ) ) ) )
          ],
           ) ) );ed on `cList`, go to the first element that matches `p`.
- `p1 >> p2`: based on `cList`, resolve `p2` in all elements after the first one
  that matches `p1`.
- `p1 >>! p2`: based on `cList`, resolve `p2` in the element just after the
  first one that matches `p1`.
- `p1 !>> p2` and `p1 !>>! p2`: same as `p1 >> p2` and `p1 >>! p2`, but with the
  `strict` argument of `cList` set to true.
- `p1 << p2`, `p1 <<! p2`, `p1 !<< p2` and `p1 !<<! p2`: same as `p1 >> p2`,
  `p1 >>! p2`, `p1 !>> p2` and `p1 !>>! p2`, but replacing "after" with
  "before".
- `cInclude s`: go to the included file whose name is given by `s`. Note that
  this is a mandatory constraint to provide a path pointing at the content of
  such a file.

Then, the constraints matching nodes to go through:
- `cStr ~regexp s`: match the string/regexp `s` against the current node. The
  boolean `regexp`, `false` by default, must be set to `true` if `s` describes a
  regexp. This is an exact match.
- `cInstrSubstr ~exact ~regexp s`: match the string/regexp `s` against the
  current node. The boolean `exact`, `false` by default, must be set to `true`
  for an exact match.
- `cFor ~init ~cond ~step ~body ()`: match a for loop. The `init`, `cond`,
  `step` and `body` paths may be used to give constraints on the different parts
  of the loop.
- `cWhile ~cond ~body ()`: match a while loop. The `cond` and `body` paths may
  be used to give constraints on the different parts of the loop.
- `cIf ~cond ~then_ ~else_ ()`: match an if statement. The `cond`, `then_` and
  `else_` paths may be used to give constraints on the different parts of the
  statement.
- `cVarDef ~name ~exact ~body ()`: match a variable declaration. The string
  `name` may be used to match, exactly by default, the name. The path `body` may
  be used to give constraints on the initialisation.
- `cFunDef ~name ~exact ~args ~validate ~body ()`: match a function
  declaration. The `args` path may be used to match arguments. The `validate`
  function takes the result as a list of booleans and returns a boolean that
  indicates if such a result is valid.
- `cTopFun ~name ~exact ~args ~validate ~body ()`: same as `cFunDef` but the
  declaration must be toplevel (hence, no `strict` argument here).
- `cTypDef name ~exact ()`: match a type declaration by its name.
- `cEnum ~name ~exact ~constants ()`: match an enum declaration. `name` and
  `exact` are used to match the name of the enum. `constants` is a list of pairs
  of string and path to respectively match the constant name and value.
- `cSeq ~args ~validate ()`: match a seq statement using `args` and `validate`.
- `cVar ~name ~exact ()`: match a variable.
- `cBool b`: match a boolean literal against `b`.
- `cInt n`: match an integer literal against `n`.
- `cDouble f`: match a double literal against the float `f`.
- `cString s`: match a string literal against `s`.
- `cPrim p`: match a primitive against `p`.
- `cApp ~name ~fun ~args ~validate ()`: match a function application. The
  function may be matched either using its name (if it is a variable) or using
  the path `fun_`.
- `cLabel ~label ~exact ~body ()`: match a labelled term using `label` and
  `exact` for the label and `body` for the term.
- `cGoto ~label ~exact ()`: match a goto statement using `label` and `exact` for
  the label.
- `cReturn ~res ()`: match a return statement. The path `res` may be used to
  give constraints on the result.
- `cAbort ~kind ()`: match an abort node depending on its kind (cAbrtAny,
  cAbrtRet, cAbrtBrk or cAbrtCtn).
- `cAccesses ~base ~accesses ()`: match a succession of array/struct
  accesses. The path `base` may be used to give constraints on the base of the
  accesses. The list `accesses` may be used to give constraint on the
  accesses. It must give constraints on all accesses using the following
  constructs:
  + `cAccess ()`: allow any access.
  + `cIndex ~index ()`: match an array access using the path `index` to match
  the index.
  + `cField ~field ~exact ()`: match a struct access using the string `field` to
  match, exactly by default, the field.
- `cSwitch ~cond ~cases ()`: match a switch statement. The path `cond` may be
  used to give constraints on the condition. The list `cases` may be used to
  give constraints on the cases. It must give constraints on all case groups
  using the following constructs:
  + `(cCase ~value (), p)`: match the path `p` against the body of the case. If
  a path is provided for `value` match the value of the case. Otherwise, any
  case is valid.
  + `(cDefault (), p)`: match a default case. The path `p` may be used to give
  constraints on the body.
- `cSet ~lhs ~rhs ()`: match an assignment when the path `lhs` (resp. `rhs`)
  gives constraints on the left-hand (resp. right-hand) side.