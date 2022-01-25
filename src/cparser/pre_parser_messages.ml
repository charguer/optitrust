
(* This file was auto-generated based on "handcrafted.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 29 | 244 | 40 | 442 | 202 | 190 ->
        "Internal error when printing a syntax error message. Please report.\n"
    | 170 ->
        "Ill-formed _Static_assert.\nAt this point, a semicolon ';' is expected.\n"
    | 169 ->
        "Ill-formed _Static_assert.\nAt this point, a closing parenthesis ')' is expected.\n"
    | 168 ->
        "Ill-formed _Static_assert.\nAt this point, a string literal is expected.\n"
    | 167 ->
        "Ill-formed _Static_assert.\nAt this point, a comma ',' is expected.\n"
    | 77 ->
        "Ill-formed _Static_assert.\nAt this point, a constant expression is expected.\n"
    | 76 ->
        "Ill-formed _Static_assert.\nAt this point, an opening parenthesis '(' is expected.\n"
    | 337 ->
        "Ill-formed __builtin_offsetof.\nAt this point, a member-designator is expected.\n# ------------------------------------------------------------------------------\n"
    | 328 ->
        "Ill-formed __builtin_offsetof.\nAt this point, a member-designator is expected.\n"
    | 327 ->
        "Ill-formed __builtin_offsetof.\nAt this point, a member-designator is expected.\n"
    | 326 ->
        "Ill-formed __builtin_offsetof.\nAt this point, a colon ',' is expected\n"
    | 53 ->
        "Ill-formed __builtin_offsetof.\nAt this point, a struct or union name is expected.\n"
    | 52 ->
        "Ill-formed __builtin_offsetof.\nAt this point, an opening parenthesis '(' is expected.\n"
    | 606 ->
        "Ill-formed K&R function definition.\nAt this point, one of the following is expected:\n  a declaration; or\n  an opening brace '{' (for the function body).\n"
    | 16 ->
        "Ill-formed declaration.\nThe following identifier is used as a type, but has not been defined as such:\n  $0\n"
    | 237 ->
        "Up to this point, a list of parameter declarations has been recognized:\n  $0\nIf this list is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 238 ->
        "At this point, one of the following is expected:\n  a parameter declaration; or\n  an ellipsis '...'.\n"
    | 610 ->
        "Ill-formed declaration or function definition.\nUp to this point, a list of attribute specifiers has been recognized.\nIf this is a declaration,\n  then at this point, a semicolon ';' is expected.\nIf this is a function definition,\n  then at this point, an opening brace '{' is expected (for the function body).\nIf this is the parameter declaration of a K&R function definition,\n  then at this point, one of the following is expected:\n    a storage class specifier; or\n    a type qualifier; or\n    a type specifier.\n"
    | 597 ->
        "Ill-formed K&R parameter declaration.\nAt this point, one of the following is expected:\n  a storage class specifier; or\n  a type qualifier; or\n  a type specifier.\n"
    | 596 | 601 | 594 | 599 ->
        "Ill-formed K&R parameter declaration.\nAt this point, one of the following is expected:\n  a storage class specifier; or\n  a type qualifier; or\n  a list of declarators.\n"
    | 291 ->
        "Ill-formed K&R function definition.\nThe following type name is used as a K&R parameter name:\n  $0\n"
    | 290 ->
        "Ill-formed K&R function definition.\nAt this point, an identifier is expected.\n"
    | 251 ->
        "Up to this point, an expression has been recognized:\n  $0\nIf this expression is complete,\nthen at this point, a closing bracket ']' is expected.\n"
    | 561 ->
        "Ill-formed init declarator.\nAt this point, an initializer is expected.\n"
    | 375 ->
        "Ill-formed initializer.\nAt this point, an optional designation,\nfollowed with an initializer, is expected.\n"
    | 376 ->
        "Ill-formed initializer.\nUp to this point, a list of initializers has been recognized:\n  $0\nIf this list is complete,\nthen at this point, a closing brace '}' is expected.\n"
    | 377 ->
        "Ill-formed initializer list.\nAt this point, one of the following is expected:\n  an optional designation, followed with an initializer; or\n  a closing brace '}'.\n"
    | 331 ->
        "Ill-formed designator.\nAt this point, a constant expression is expected.\n"
    | 332 ->
        "Ill-formed designator.\nUp to this point, an opening bracket and an expression have been recognized:\n  $1 $0\nIf this expression is complete,\nthen at this point, a closing bracket ']' is expected.\n"
    | 334 ->
        "Ill-formed designator.\nAt this point, the name of a struct or union member is expected.\n"
    | 381 ->
        "Ill-formed designation.\nUp to this point, a list of designators has been recognized:\n  $0\nIf this list is complete,\nthen at this point, an equals sign '=' is expected.\n"
    | 374 | 378 ->
        "Ill-formed initializer list.\nAt this point, an initializer is expected.\n"
    | 556 ->
        "Ill-formed declaration.\nAt this point, an init declarator is expected.\n"
    | 555 ->
        "Up to this point, a list of declarators has been recognized:\n  $0\nIf this list is complete,\nthen at this point, a semicolon ';' is expected.\n"
    | 144 ->
        "Ill-formed use of the sequencing operator ','.\nAt this point, an expression is expected.\n"
    | 578 ->
        "A type identifier has been recognized.\nAssuming this is the beginning of a declaration,\nat this point, one of the following is expected:\n  a storage class specifier; or\n  a type qualifier; or\n  an init declarator, followed with a semicolon ';'.\n"
    | 440 ->
        "Up to this point, an expression has been recognized:\n  $0\nIf this expression is complete,\nthen at this point, a semicolon ';' is expected.\n"
    | 437 ->
        "Ill-formed 'return' statement.\nAt this point, one of the following is expected:\n  an expression; or\n  a semicolon ';'.\n"
    | 436 ->
        "At this point, one of the following is expected:\n  a declaration; or\n  a statement; or\n  a pragma; or\n  a closing brace '}'.\n"
    | 492 ->
        "Ill-formed 'while' statement.\nAt this point, an opening parenthesis '(' is expected.\n"
    | 493 ->
        "Ill-formed 'while' statement.\nAt this point, an expression is expected.\n"
    | 494 ->
        "Ill-formed 'while' statement.\nUp to this point, an expression has been recognized:\n  $0\nIf this expression is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 495 ->
        "Ill-formed 'while' statement.\nAt this point, a statement (the loop body) is expected.\n"
    | 508 ->
        "Ill-formed 'switch' statement.\nAt this point, an opening parenthesis '(' is expected.\n"
    | 509 ->
        "Ill-formed 'switch' statement.\nAt this point, an expression is expected.\n"
    | 510 ->
        "Ill-formed 'switch' statement.\nUp to this point, an expression has been recognized:\n  $0\nIf this expression is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 511 ->
        "Ill-formed 'switch' statement.\nAt this point, a statement is expected.\nIt usually takes the form of a series of labeled statements,\nenclosed within braces '{' and '}'.\n"
    | 513 ->
        "Ill-formed 'if' statement.\nAt this point, an opening parenthesis '(' is expected.\n"
    | 514 ->
        "Ill-formed 'if' statement.\nAt this point, an expression is expected.\n"
    | 515 ->
        "Ill-formed 'if' statement.\nUp to this point, an expression has been recognized:\n  $0\nIf this expression is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 517 ->
        "Ill-formed 'if' statement.\nAt this point, a statement is expected.\n"
    | 569 ->
        "Ill-formed 'if' ... 'else' statement.\nAt this point, a statement is expected.\n"
    | 445 ->
        "Ill-formed 'goto' statement.\nAt this point, an identifier (a 'goto' label) is expected.\n"
    | 520 ->
        "Ill-formed 'for' statement.\nAt this point, an opening parenthesis '(' is expected.\n"
    | 521 ->
        "Ill-formed 'for' statement.\nAt this point, one of the following is expected:\n  an optional expression\n    (evaluated once at the beginning),\n  followed with a semicolon ';'; or\n  a declaration.\n"
    | 541 ->
        "Ill-formed 'for' statement.\nUp to this point, an expression has been recognized:\n  $0\nIf this expression is complete,\nthen at this point, a semicolon ';' is expected.\n"
    | 534 ->
        "Ill-formed 'for' statement.\nAt this point, an optional expression\n  (evaluated before each execution of the loop body),\nfollowed with a semicolon ';', is expected.\n"
    | 535 ->
        "Ill-formed 'for' statement.\nAt this point, an optional expression\n  (evaluated after each execution of the loop body),\nfollowed with a closing parenthesis ')', is expected.\n"
    | 539 ->
        "Ill-formed 'for' statement.\nUp to this point, an expression has been recognized:\n  $0\nIf this expression is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 537 ->
        "Ill-formed 'for' statement.\nAt this point, a statement (the loop body) is expected.\n"
    | 567 ->
        "Ill-formed 'do' ... 'while' statement.\nAt this point, a statement (the loop body) is expected.\n"
    | 571 ->
        "Ill-formed 'do' ... 'while' statement.\nAt this point, a 'while' keyword is expected.\n"
    | 572 ->
        "Ill-formed 'do' ... 'while' statement.\nAt this point, an opening parenthesis '(' is expected.\n"
    | 573 ->
        "Ill-formed 'do' ... 'while' statement.\nAt this point, an expression is expected.\n"
    | 574 ->
        "Ill-formed 'do' ... 'while' statement.\nUp to this point, an expression has been recognized:\n  $0\nIf this expression is complete,\nthen at this point, a closing parenthesis ')' and a semicolon ';' are expected.\n"
    | 448 | 502 ->
        "Ill-formed labeled statement.\nAt this point, a colon ':' is expected.\n"
    | 452 ->
        "Ill-formed labeled statement.\nAt this point, a constant expression is expected.\n"
    | 453 ->
        "Ill-formed labeled statement.\nUp to this point, an expression has been recognized:\n  $0\nIf this expression is complete,\nthen at this point, a colon ':' is expected.\n"
    | 454 | 449 | 503 ->
        "Ill-formed labeled statement.\nAt this point, a statement is expected.\n"
    | 455 | 450 | 575 | 446 | 487 ->
        "Ill-formed statement.\nAt this point, a semicolon ';' is expected.\n"
    | 463 ->
        "Ill-formed assembly statement.\nAt this point, a string literal, representing an instruction, is expected.\n"
    | 464 ->
        "Ill-formed assembly statement.\nAt this point, one of the following is expected:\n  a string literal, representing one more instruction; or\n  a colon ':', followed with a list of outputs; or\n  a closing parenthesis ')'.\n"
    | 473 ->
        "Ill-formed assembly operand.\nAt this point, an opening parenthesis '(',\nfollowed with an expression and a closing parenthesis ')', is expected.\n"
    | 474 ->
        "Ill-formed assembly operand.\nAt this point, an expression is expected.\n"
    | 475 ->
        "Ill-formed assembly operand.\nUp to this point, an expression has been recognized:\n  $0\nIf this expression is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 470 ->
        "Ill-formed assembly statement.\nAt this point, an assembly operand is expected.\n"
    | 466 ->
        "Ill-formed assembly operand.\nAt this point, an identifier is expected.\n"
    | 467 ->
        "Ill-formed assembly operand.\nAt this point, a closing bracket ']' is expected.\n"
    | 472 ->
        "Ill-formed assembly operand.\nAt this point, a string literal, representing a constraint, is expected.\n"
    | 479 ->
        "Ill-formed assembly statement.\nUp to this point, a list of outputs and a list of inputs have been recognized:\n  $2\n  $0\nIf the latter list is complete,\nthen at this point, one of the following is expected:\n  a colon ':', followed with a list of clobbered resources; or\n  a closing parenthesis ')'.\n"
    | 477 ->
        "Ill-formed assembly statement.\nUp to this point, a list of outputs has been recognized:\n  $0\nIf this list is complete,\nthen at this point, one of the following is expected:\n  a colon ':', followed with a list of inputs; or\n  a closing parenthesis ')'.\n"
    | 482 ->
        "Ill-formed assembly statement.\nUp to this point, a list of clobbered resources has been recognized:\n  $0\nIf this list is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 483 | 480 ->
        "Ill-formed assembly statement.\nAt this point, a clobbered resource is expected.\nExamples of clobbered resources:\n  \"memory\"\n  \"eax\"\n"
    | 459 | 458 | 457 ->
        "Ill-formed assembly statement.\nAt this point, one of the following is expected:\n  an assembly attribute, such as 'volatile'; or\n  an opening parenthesis '('.\n"
    | 257 ->
        "At this point, a list of parameter declarations,\nfollowed with a closing parenthesis ')', is expected.\n"
    | 545 ->
        "At this point, a declarator is expected.\n"
    | 544 ->
        "Up to this point, a list of declarators has been recognized:\n  $0\nIf this list is complete,\nthen at this point, a semicolon ';' is expected.\n"
    | 198 ->
        "Ill-formed declarator.\nAt this point, one of the following is expected:\n  a type qualifier; or\n  a star '*', possibly followed with type qualifiers; or\n  a direct declarator.\n"
    | 201 ->
        "Ill-formed function definition.\nAt this point, a list of parameter declarations,\nfollowed with a closing parenthesis ')', is expected.\n"
    | 289 ->
        "Ill-formed K&R function definition.\nUp to this point, a list of identifiers has been recognized:\n  $0\nIf this list is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 195 ->
        "Ill-formed direct declarator.\nAt this point, a declarator is expected.\n"
    | 272 ->
        "Up to this point, a declarator has been recognized:\n  $0\nIf this declarator is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 72 ->
        "Ill-formed struct or union specifier.\nAt this point, one of the following is expected:\n  an attribute specifier; or\n  an identifier; or\n  an opening brace '{', followed with a list of members.\n"
    | 75 ->
        "At this point, one of the following is expected:\n  a struct declaration; or\n  a closing brace '}'.\n"
    | 182 ->
        "Ill-formed struct declaration.\nAt this point, one of the following is expected:\n  a type qualifier; or\n  a type specifier.\n"
    | 301 ->
        "Ill-formed struct declaration.\nUp to this point, a declarator has been recognized:\n  $0\nIf this declarator is complete,\nthen at this point, one of the following is expected:\n  a colon ':', followed with a constant expression; or\n  a comma ',', followed with a struct declarator; or\n  a semicolon ';'.\n"
    | 296 ->
        "Ill-formed struct declaration.\nAt this point, a struct declarator is expected.\n"
    | 299 ->
        "Ill-formed struct declarator.\nAt this point, a constant expression is expected.\n"
    | 295 ->
        "Ill-formed struct declaration.\nUp to this point, a list of struct declarators has been recognized:\n  $0\nIf this list is complete,\nthen at this point, a semicolon ';' is expected.\n"
    | 189 ->
        "Ill-formed struct declaration.\nUp to this point,\na list of type qualifiers and type specifiers has been recognized:\n  $0\nIf this list is complete, then \nat this point, one of the following is expected:\n  a struct declarator; or\n  a semicolon ';'.\n"
    | 524 | 530 | 526 | 532 ->
        "Ill-formed declaration.\nAt this point, one of the following is expected:\n  a storage class specifier; or\n  a type qualifier; or\n  an init declarator.\n"
    | 528 ->
        "Ill-formed declaration.\nAt this point, one of the following is expected:\n  a storage class specifier; or\n  a type qualifier; or\n  a type specifier.\n"
    | 418 ->
        "Ill-formed declaration or function definition.\nAt this point, one of the following is expected:\n  a storage class specifier; or\n  a type qualifier; or\n  a type specifier.\n"
    | 209 | 232 | 215 | 234 ->
        "Ill-formed parameter declaration.\nAt this point, one of the following is expected:\n  a storage class specifier; or\n  a type qualifier; or\n  a declarator; or\n  an abstract declarator; or\n  a comma ',', followed with a parameter declaration; or\n  a closing parenthesis ')'.\n"
    | 409 | 426 | 413 | 430 ->
        "Ill-formed declaration or function definition.\nAt this point, one of the following is expected:\n  a storage class specifier; or\n  a type qualifier; or\n  an init declarator,\n    if this is a declaration; or\n  a declarator,\n    followed with a function body,\n    if this is a function definition.\n"
    | 230 ->
        "Ill-formed parameter declaration.\nAt this point, one of the following is expected:\n  a storage class specifier; or\n  a type qualifier; or\n  a type specifier.\n"
    | 9 | 420 ->
        "Ill-formed type definition.\nAt this point, one of the following is expected:\n  a storage class specifier; or\n  a type qualifier; or\n  a type specifier.\n"
    | 402 | 411 | 422 | 428 | 404 | 415 | 424 | 432 ->
        "Ill-formed type definition.\nAt this point, one of the following is expected:\n  a storage class specifier; or\n  a type qualifier; or\n  a list of declarators, followed with a semicolon ';'.\n"
    | 2 ->
        "At this point, one of the following is expected:\n  a function definition; or\n  a declaration; or\n  a pragma; or\n  the end of the file.\n"
    | 18 ->
        "Ill-formed $0 attribute.\nAt this point, an opening parenthesis '(',\nfollowed with a possibly empty list of expressions,\nis expected.\n"
    | 24 ->
        "Ill-formed expression.\nThe following identifier is used as a variable, but has been defined as a type:\n  $0\n"
    | 19 ->
        "Ill-formed $1 attribute.\nAt this point, a list of expressions is expected.\n"
    | 399 ->
        "Ill-formed $2 attribute.\nUp to this point, a list of expressions has been recognized:\n  $0\nIf this list is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 143 ->
        "Ill-formed conditional expression.\nUp to this point, an expression, '?', and an expression have been recognized:\n  $2\n  $1\n  $0\nIf the last expression is complete,\nthen at this point, a colon ':' is expected.\n"
    | 147 | 125 ->
        "Ill-formed conditional expression.\nAt this point, an expression is expected.\n"
    | 86 ->
        "Ill-formed expression.\nAt this point, a list of expressions,\nfollowed with a closing parenthesis ')', is expected.\n"
    | 156 ->
        "Up to this point, a list of expressions has been recognized:\n  $0\nIf this list is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 159 ->
        "Ill-formed expression.\nAt this point, an expression is expected.\n"
    | 160 ->
        "Ill-formed expression.\nUp to this point, an expression has been recognized:\n  $0\nIf this expression is complete,\nthen at this point, a closing bracket ']' is expected.\n"
    | 163 | 84 ->
        "Ill-formed use of the dereferencing operator $0.\nAt this point, the name of a struct or union member is expected.\n"
    | 157 ->
        "Ill-formed list of expressions.\nAt this point, an expression is expected.\n"
    | 99 ->
        "Ill-formed use of the assignment operator $0.\nAt this point, an expression is expected.\n"
    | 138 | 127 | 129 | 150 | 131 | 121 | 135 | 114 | 103 | 108 ->
        "Ill-formed use of the binary operator $0.\nAt this point, an expression is expected.\n"
    | 78 ->
        "Ill-formed use of the unary operator $0.\nAt this point, an expression is expected.\n"
    | 30 ->
        "Ill-formed expression.\nAn opening parenthesis '(' has just been recognized.\nAt this point, one of the following is expected:\n  a type name,   if this is a type cast or a compound literal; or\n  an expression, if this is a parenthesized expression.\n"
    | 394 ->
        "Ill-formed expression.\nUp to this point, a type name in parentheses has been recognized:\n  $2 $1 $0\nAt this point, one of the following is expected:\n  an expression,        if this is a type cast; or\n  an opening brace '{', if this is a compound literal.\n"
    | 373 ->
        "Ill-formed compound literal.\nAt this point, an initializer is expected.\n"
    | 387 ->
        "Ill-formed compound literal.\nUp to this point, a list of initializers has been recognized:\n  $0\nIf this list is complete,\nthen at this point, a closing brace '}' is expected.\n"
    | 390 ->
        "Ill-formed expression.\nUp to this point, an expression has been recognized:\n  $0\nIf this expression is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 34 ->
        "Ill-formed expression.\nAt this point, one of the following is expected:\n  a type name (if this is the beginning of a compound literal); or\n  an expression.\n"
    | 372 ->
        "Ill-formed expression.\nUp to this point, a type name in parentheses has been recognized:\n  $2 $1 $0\nIf this is the beginning of a compound literal,\n  then at this point, an opening brace '{' is expected.\nIf this is intended to be the beginning of a cast expression,\n  then perhaps an opening parenthesis '(' was forgotten earlier.\n"
    | 48 | 33 ->
        "Ill-formed expression.\nAt this point, an expression is expected.\n"
    | 50 ->
        "Ill-formed use of $0.\nAt this point, an opening parenthesis '(' is expected.\n"
    | 51 ->
        "Ill-formed use of $1.\nAt this point, an expression is expected.\n"
    | 340 ->
        "Ill-formed use of $3.\nAt this point, a type name is expected.\n"
    | 339 ->
        "Ill-formed use of $2.\nUp to this point, an expression has been recognized:\n  $0\nIf this expression is complete,\nthen at this point, a comma ',' is expected.\n"
    | 23 ->
        "Ill-formed use of $0.\nAt this point, an opening parenthesis '(' is expected,\nfollowed with an expression or a type name.\n"
    | 64 ->
        "Ill-formed use of $0.\nAt this point, an opening parenthesis '(' is expected,\nfollowed with a type name.\n"
    | 65 | 28 ->
        "Ill-formed use of $1.\nAt this point, an expression is expected.\n"
    | 356 ->
        "Ill-formed enumeration specifier.\nAt this point, one of the following is expected:\n  an attribute specifier; or\n  an identifier; or\n  an opening brace '{'.\n"
    | 358 ->
        "Ill-formed enumeration specifier.\nAt this point, an enumerator is expected.\n"
    | 363 ->
        "Ill-formed enumeration specifier.\nAt this point, one of the following is expected:\n  an equals sign '=', followed with a constant expression; or\n  a comma ',', followed with an enumerator; or\n  a closing brace '}'.\n"
    | 364 ->
        "Ill-formed enumeration specifier.\nAt this point, a constant expression is expected.\n"
    | 360 ->
        "Ill-formed enumeration specifier.\nUp to this point, a list of enumerators has been recognized:\n  $0\nIf this list is complete,\nthen at this point, a closing brace '}' is expected.\n"
    | 361 ->
        "Ill-formed enumeration specifier.\nAt this point, an enumerator is expected.\n"
    | 37 ->
        "Ill-formed gcc attribute specifier.\nAt this point, two opening parentheses '((' are expected.\n"
    | 38 ->
        "Ill-formed gcc attribute specifier.\nAt this point, a second opening parenthesis '(' is expected.\n"
    | 39 ->
        "Ill-formed gcc attribute specifier.\nAt this point, a gcc attribute is expected.\n"
    | 46 ->
        "Ill-formed gcc attribute specifier.\nUp to this point, an attribute has been recognized:\n  $0\nAt this point, one of the following is expected:\n  an opening parenthesis '(',\n    followed with a list of parameters for this attribute; or\n  a comma ',',\n    followed with another attribute; or\n  a closing parenthesis ')'.\n"
    | 47 ->
        "Ill-formed gcc attribute.\nAt this point, a list of expressions is expected.\n"
    | 344 ->
        "Ill-formed gcc attribute.\nAt this point, a comma ',' is expected.\n"
    | 345 ->
        "Ill-formed gcc attribute.\nAt this point, an expression is expected.\n"
    | 346 ->
        "Ill-formed gcc attribute.\nUp to this point, a list of expressions has been recognized:\n  $0\nIf this list is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 350 ->
        "Ill-formed attribute specifier.\nAt this point, one of the following is expected:\n  a comma ',', followed with a gcc attribute; or\n  two closing parentheses '))'.\n"
    | 351 ->
        "Ill-formed attribute specifier.\nAt this point, a second closing parenthesis ')' is expected.\n"
    | 353 ->
        "Ill-formed attribute specifier.\nAt this point, a gcc attribute is expected.\n"
    | 60 ->
        "Ill-formed _Alignas qualifier.\nAt this point, an opening parenthesis '(' is expected.\n"
    | 61 ->
        "Ill-formed _Alignas qualifier.\nAt this point, one of the following is expected:\n  an expression; or\n  a type name.\n"
    | 307 ->
        "Ill-formed type name.\nAt this point, one of the following is expected:\n  a type qualifier; or\n  a type specifier.\n"
    | 242 ->
        "An opening parenthesis '(' has been recognized.\nAt this point, one of the following is expected:\n  an abstract declarator or a declarator,\n    if this parenthesis is a delimiter; or\n  a list of parameter declarations,\n    if this parenthesis is the beginning of a function declarator.\n"
    | 315 ->
        "An opening parenthesis '(' has been recognized.\nAt this point, one of the following is expected:\n  an abstract declarator,\n    if this parenthesis is a delimiter; or\n  a list of parameter declarations,\n    if this parenthesis is the beginning of a function declarator.\n"
    | 274 ->
        "Up to this point, an abstract declarator has been recognized:\n  $0\nAt this point, a closing parenthesis ')' is expected.\n"
    | 276 | 259 | 293 ->
        "At this point, a closing parenthesis ')' is expected.\n"
    | 248 | 265 ->
        "Ill-formed array declarator.\nAt this point, one of the following is expected:\n  an expression, followed with a closing bracket ']'; or\n  a closing bracket ']'.\n"
    | 324 ->
        "Ill-formed _Alignas qualifier.\nUp to this point, an expression has been recognized:\n  $0\nIf this expression is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 393 ->
        "Up to this point, a type name has been recognized:\n  $0\nIf this type name is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 371 ->
        "Ill-formed compound literal.\nUp to this point, a type name has been recognized:\n  $0\nIf this type name is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 341 ->
        "Ill-formed use of __builtin_va_arg.\nUp to this point, a type name has been recognized:\n  $0\nIf this type name is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 312 | 396 ->
        "Ill-formed use of $2.\nUp to this point, a type name has been recognized:\n  $0\nIf this type name is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | 322 ->
        "Ill-formed _Alignas qualifier.\nUp to this point, a type name has been recognized:\n  $0\nIf this type name is complete,\nthen at this point, a closing parenthesis ')' is expected.\n"
    | _ ->
        raise Not_found
