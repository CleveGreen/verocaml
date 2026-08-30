  $ mkdir artifacts
  $ compile () { ocamlc -w -A -alert -all -bin-annot -I artifacts -c -o "artifacts/$1.cmo" "fixtures/$1.ml"; }
  $ compile_ghost () { ocamlc -w -A -alert -all -bin-annot -I artifacts -I ../../runtime/.vero_ghost.objs/byte -c -o "artifacts/$1.cmo" "fixtures/$1.ml"; }
  $ compile_pure () { ocamlc -w -A -alert -all -bin-annot -I artifacts -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$1.cmo" "fixtures/$1.ml"; }
  $ classify () { compile "$1" && ./pure_sst_tool.exe classify "artifacts/$1.cmt"; }
  $ compile_pure pure
  $ ./pure_sst_tool.exe dump artifacts/pure.cmt > artifacts/first.sst
  $ ./pure_sst_tool.exe dump artifacts/pure.cmt > artifacts/second.sst
  $ cmp artifacts/first.sst artifacts/second.sst
  $ cat artifacts/first.sst
  policy default-linear/default-z3
  function helper#0 mode=exec recursive=false result=int policy=default-linear/default-z3 @ pure.ml:1:0-1:28
    parameter
      pattern bind x#0:int : int @ pure.ml:1:12-1:13
    body checked-exec stage=runtime provenance=typedtree:pure.ml
      checked-add : int @ pure.ml:1:23-1:28
        variable x#0:int : int @ pure.ml:1:23-1:24
        int 1 : int @ pure.ml:1:27-1:28
  function countdown#1 mode=exec recursive=true result=int policy=default-linear/default-z3 @ pure.ml:3:0-4:41
    parameter
      pattern bind n#0:int : int @ pure.ml:3:19-3:20
    body checked-exec stage=runtime provenance=typedtree:pure.ml
      if : int @ pure.ml:4:2-4:41
        condition
          compare-less-or-equal : bool @ pure.ml:4:5-4:11
            variable n#0:int : int @ pure.ml:4:5-4:6
            int 0 : int @ pure.ml:4:10-4:11
        then
          int 0 : int @ pure.ml:4:17-4:18
        else
          exec-call countdown#1 recursive=true type-arguments=[] : int @ pure.ml:4:24-4:41
            argument
              checked-subtract : int @ pure.ml:4:34-4:41
                variable n#0:int : int @ pure.ml:4:35-4:36
                int 1 : int @ pure.ml:4:39-4:40
  function pure#2 mode=exec recursive=false result=int policy=default-linear/default-z3 @ pure.ml:6:0-14:9
    parameter
      pattern bind x#0:int : int @ pure.ml:6:10-6:11
    body checked-exec stage=runtime provenance=typedtree:pure.ml
      let : int @ pure.ml:7:2-14:9
        binding
          pattern bind y#1:int : int @ pure.ml:7:6-7:7
          checked-add : int @ pure.ml:7:10-7:15
            variable x#0:int : int @ pure.ml:7:10-7:11
            int 1 : int @ pure.ml:7:14-7:15
        body
          let : int @ pure.ml:8:2-14:9
            binding
              pattern bind pair#2:(int * int) : (int * int) @ pure.ml:8:6-8:10
              tuple : (int * int) @ pure.ml:8:13-8:19
                component
                  variable y#1:int : int @ pure.ml:8:14-8:15
                component
                  variable x#0:int : int @ pure.ml:8:17-8:18
            body
              if : int @ pure.ml:9:2-14:9
                condition
                  compare-greater-than : bool @ pure.ml:9:5-9:10
                    variable y#1:int : int @ pure.ml:9:5-9:6
                    variable x#0:int : int @ pure.ml:9:9-9:10
                then
                  match : int @ pure.ml:10:4-12:22
                    scrutinee
                      variable pair#2:(int * int) : (int * int) @ pure.ml:10:10-10:14
                    case @ pure.ml:11:25-11:30
                      pattern tuple : (int * int) @ pure.ml:11:6-11:10
                        component
                          pattern bind a#3:int : int @ pure.ml:11:6-11:7
                        component
                          pattern bind b#4:int : int @ pure.ml:11:9-11:10
                      guard
                        compare-greater-than : bool @ pure.ml:11:16-11:21
                          variable a#3:int : int @ pure.ml:11:16-11:17
                          variable b#4:int : int @ pure.ml:11:20-11:21
                      body
                        checked-subtract : int @ pure.ml:11:25-11:30
                          variable a#3:int : int @ pure.ml:11:25-11:26
                          variable b#4:int : int @ pure.ml:11:29-11:30
                    case @ pure.ml:12:11-12:22
                      pattern _ : (int * int) @ pure.ml:12:6-12:7
                      body
                        exec-call helper#0 recursive=false type-arguments=[] : int @ pure.ml:12:11-12:22
                          argument
                            checked-negate : int @ pure.ml:12:18-12:22
                              variable x#0:int : int @ pure.ml:12:20-12:21
                else
                  checked-multiply-constant 3 : int @ pure.ml:14:4-14:9
                    variable x#0:int : int @ pure.ml:14:4-14:5
  function booleans#3 mode=exec recursive=false result=bool policy=default-linear/default-z3 @ pure.ml:16:0-17:30
    parameter
      pattern bind a#0:int : int @ pure.ml:16:14-16:15
    parameter
      pattern bind b#1:int : int @ pure.ml:16:24-16:25
    body checked-exec stage=runtime provenance=typedtree:pure.ml
      boolean-and : bool @ pure.ml:17:2-17:30
        boolean-not : bool @ pure.ml:17:2-17:11
          bool false : bool @ pure.ml:17:6-17:11
        boolean-or : bool @ pure.ml:17:15-17:30
          compare-equal : bool @ pure.ml:17:16-17:21
            variable a#0:int : int @ pure.ml:17:16-17:17
            variable b#1:int : int @ pure.ml:17:20-17:21
          bool true : bool @ pure.ml:17:25-17:29
  function total_case#4 mode=exec recursive=false result=int policy=default-linear/default-z3 @ pure.ml:19:0-21:14
    parameter
      pattern bind param#0:bool : bool @ pure.ml:19:31-21:14
    body checked-exec stage=runtime provenance=typedtree:pure.ml
      match : int @ pure.ml:19:31-21:14
        scrutinee
          variable param#0:bool : bool @ pure.ml:19:31-21:14
        case @ pure.ml:20:12-20:13
          pattern bool true : bool @ pure.ml:20:4-20:8
          body
            int 1 : int @ pure.ml:20:12-20:13
        case @ pure.ml:21:13-21:14
          pattern bool false : bool @ pure.ml:21:4-21:9
          body
            int 0 : int @ pure.ml:21:13-21:14
  function contracted#5 mode=exec recursive=false result=int policy=default-linear/default-z3 @ pure.ml:23:0-28:3
    parameter
      pattern bind n#0:int : int @ pure.ml:23:16-23:17
    requires 0 stage=logical @ pure.ml:24:2-24:29
      compare-greater-or-equal : bool @ pure.ml:24:22-24:28
        variable n#0:int : int @ pure.ml:24:22-24:23
        int 0 : int @ pure.ml:24:27-24:28
    ensures 0 stage=logical @ pure.ml:25:2-25:63
      pattern bind result#1:int : int @ pure.ml:25:25-25:31
      compare-greater-or-equal : bool @ pure.ml:25:35-25:62
        variable result#1:int : int @ pure.ml:25:35-25:41
        old : int @ pure.ml:25:45-25:62
          variable n#0:int : int @ pure.ml:25:60-25:61
    decreases 0 stage=logical @ pure.ml:26:2-26:25
      variable n#0:int : int @ pure.ml:26:23-26:24
    assertion 0 stage=logical @ pure.ml:27:2-27:27
      compare-greater-or-equal : bool @ pure.ml:27:20-27:26
        variable n#0:int : int @ pure.ml:27:20-27:21
        int 0 : int @ pure.ml:27:25-27:26
    body checked-exec stage=runtime provenance=typedtree:pure.ml
      variable n#0:int : int @ pure.ml:28:2-28:3
  function unit_value#6 mode=exec recursive=false result=unit policy=default-linear/default-z3 @ pure.ml:30:0-30:22
    parameter
      pattern unit : unit @ pure.ml:30:15-30:17
    body checked-exec stage=runtime provenance=typedtree:pure.ml
      unit : unit @ pure.ml:30:20-30:22
  function standard_arithmetic#7 mode=exec recursive=false result=int policy=default-linear/default-z3 @ pure.ml:32:0-32:57
    parameter
      pattern bind x#0:int : int @ pure.ml:32:25-32:26
    body checked-exec stage=runtime provenance=typedtree:pure.ml
      checked-add : int @ pure.ml:32:36-32:57
        checked-successor : int @ pure.ml:32:36-32:42
          variable x#0:int : int @ pure.ml:32:41-32:42
        checked-predecessor : int @ pure.ml:32:45-32:57
          checked-absolute-value : int @ pure.ml:32:50-32:57
            variable x#0:int : int @ pure.ml:32:55-32:56

  $ classify partial_match
  VERO_UNSUPPORTED_PARTIAL_MATCH partial_match.ml:1:24-1:43
  $ classify partial_parameter
  VERO_UNSUPPORTED_PARTIAL_PARAMETER partial_parameter.ml:1:12-1:28
  $ classify mutual_recursion
  VERO_UNSUPPORTED_MUTUAL_RECURSION mutual_recursion.ml:1:0-2:42
  $ classify external_call
  VERO_UNSUPPORTED_EXTERNAL_CALL external_call.ml:1:30-1:41
  $ classify while_loop
  VERO_UNSUPPORTED_LOOP while_loop.ml:1:21-1:49
  $ classify exception
  VERO_UNSUPPORTED_EXCEPTION exception.ml:1:31-1:48
  $ classify array
  VERO_UNSUPPORTED_ARRAY array.ml:1:15-1:25
  $ classify object
  VERO_UNSUPPORTED_OBJECT object.ml:1:22-1:49
  $ classify first_class_module
  VERO_UNSUPPORTED_FIRST_CLASS_MODULE first_class_module.ml:2:2-5:42
  $ compile effect_support
  $ classify effect
  VERO_UNSUPPORTED_EFFECT effect.ml:1:22-1:56
  $ classify concurrency
  VERO_UNSUPPORTED_CONCURRENCY concurrency.ml:1:20-1:60
  $ classify higher_order
  VERO_CALLBACK_CONTRACT higher_order.ml:1:18-1:19
  $ classify polymorphic
  accepted
  $ classify nonlinear
  VERO_UNSUPPORTED_NONLINEAR_MULTIPLICATION nonlinear.ml:1:36-1:41
  $ classify aggregate_equality
  VERO_UNSUPPORTED_POLYMORPHISM aggregate_equality.ml:1:57-1:62
  $ classify wrapping
  VERO_UNSUPPORTED_WRAPPING_ARITHMETIC wrapping.ml:1:25-1:32
  $ classify division
  VERO_UNSUPPORTED_WRAPPING_ARITHMETIC division.ml:1:25-1:30
  $ classify remainder
  VERO_UNSUPPORTED_WRAPPING_ARITHMETIC remainder.ml:1:26-1:33
  $ classify bitwise
  VERO_UNSUPPORTED_WRAPPING_ARITHMETIC bitwise.ml:1:24-1:32
  $ classify aggregate
  accepted
  $ classify mutation
  VERO_UNSUPPORTED_MUTATION mutation.ml:1:25-1:59
  $ compile_ghost direct_ghost
  $ ./pure_sst_tool.exe classify artifacts/direct_ghost.cmt
  VERO_MALFORMED_GHOST_CALL direct_ghost.ml:1:29-1:67

  $ classify partial_match_string
  VERO_UNSUPPORTED_PARTIAL_MATCH partial_match_string.ml:1:37-1:61
  $ classify external_string
  VERO_UNSUPPORTED_EXTERNAL_CALL external_string.ml:1:32-1:47

  $ mkdir artifacts/fake_stdlib
  $ cp fixtures/counterfeit_stdlib_definition.ml artifacts/fake_stdlib/stdlib.ml
  $ ocamlc -w -A -bin-annot -nopervasives -c -o artifacts/fake_stdlib/stdlib.cmo artifacts/fake_stdlib/stdlib.ml
  $ ocamlc -w -A -bin-annot -nopervasives -I artifacts/fake_stdlib -c -o artifacts/counterfeit_stdlib_user.cmo fixtures/counterfeit_stdlib_user.ml
  $ ./pure_sst_tool.exe classify artifacts/counterfeit_stdlib_user.cmt
  VERO_UNSUPPORTED_EXTERNAL_CALL counterfeit_stdlib_user.ml:1:35-1:51

  $ mkdir artifacts/fake_ghost
  $ cp fixtures/counterfeit_ghost_definition.ml artifacts/fake_ghost/vero_ghost.ml
  $ ocamlc -w -A -bin-annot -c -o artifacts/fake_ghost/vero_ghost.cmo artifacts/fake_ghost/vero_ghost.ml
  $ ocamlc -w -A -bin-annot -I artifacts/fake_ghost -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/counterfeit_ghost_user.cmo fixtures/counterfeit_ghost_user.ml
  $ ./pure_sst_tool.exe classify artifacts/counterfeit_ghost_user.cmt
  VERO_UNSUPPORTED_EXTERNAL_CALL counterfeit_ghost_user.ml:2:2-2:29
