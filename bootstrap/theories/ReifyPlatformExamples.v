From Stdlib Require Import List String.
From NM Require Import Audit Sx Parse Elab ReifyData Reify ReifyProgram ReifyProviderExamples.
Import ListNotations Audit.Input.
Local Open Scope string_scope.
Definition scalar_platform_candidate : option xprogram :=
  match parse_moss "type I32;assume I32{val i32_one:I32;fn i32_add(x:I32,y:I32):I32;}context Wasm=I32,i32_one,i32_add;",
    parse_moss "import ""./wasm.moss"" use Wasm,i32_one,i32_add;",
    parse_moss "assume Wasm{fn main(){i32_add(i32_one,i32_one);}}"with
  | Good wasm,Good prelude,Good main=>
      match elaborate_runtime_candidate(Bundle "main.moss" "lib/prelude.moss"
        [AModule "lib/wasm.moss" false [] wasm;AModule "lib/prelude.moss" false ["lib/wasm.moss"] prelude;
         AModule "main.moss" true [] main])with
      | Good result=>Some(rr_program result)|Bad _=>None end
  | _,_,_=>None end.
Example initial_scalar_platform_runs :
  option_map example_program_outcome scalar_platform_candidate=Some(XOk XVUnit).
Proof.
 let compiled:=eval vm_compute in scalar_platform_candidate in
 change(option_map example_program_outcome compiled=Some(XOk XVUnit));reflexivity.
Qed.
Definition heap_platform_candidate : option xprogram :=
  match parse_moss "type StructRef;type Boxed;assume Boxed{tag Ref[Boxed](StructRef);fn .boxed():Ref;fn Ref.deref():Boxed;}functor Heap[Boxed]:StructRef,Boxed->Boxed.boxed,Ref.deref;",
    parse_moss "import ""./wasm.moss"" use StructRef,Heap,Boxed,Ref,.boxed;",
    parse_moss "tag A;assume StructRef,Heap{fn main(){bind Boxed=A;bind Heap;A.boxed().deref();}}"with
  | Good wasm,Good prelude,Good main=>
      match elaborate_runtime_candidate(Bundle "main.moss" "lib/prelude.moss"
        [AModule "lib/wasm.moss" false [] wasm;
         AModule "lib/prelude.moss" false ["lib/wasm.moss"] prelude;
         AModule "main.moss" true [] main])with
      | Good result=>Some(rr_program result)|Bad _=>None end
  | _,_,_=>None end.
Example initial_heap_platform_runs :
  option_map example_program_outcome heap_platform_candidate=Some(XOk XVUnit).
Proof.
 let compiled:=eval vm_compute in heap_platform_candidate in
 change(option_map example_program_outcome compiled=Some(XOk XVUnit));reflexivity.
Qed.
