(* ======================================================================== *)
(* FrontEndExamples.v — evidence that Audit.Syntax.Derives says what is     *)
(* meant: it relates real files of the tree to their ASTs, and it rejects  *)
(* syntax errors of tests/errors.  The sources are the files' text,         *)
(* verbatim (each double quote doubled, as Rocq writes it); regenerate    *)
(* them if the files change.                                                 *)
(* ======================================================================== *)

From Stdlib Require Import String List.
From NM Require Import Audit Parse FrontEnd FrontEndComplete.
Import ListNotations Audit.Input Audit.Syntax.
Local Open Scope string_scope.

(* lib/int.moss *)
Definition lib_int : string :=
"import ""./num.moss"" use .add, .sub, .mul, .div, .rem, .neg, .and, .or, .xor, .shl, .shr, .eq, .ne, .lt, .gt, .le, .ge;

type Int;

assume Int {
  val zero: Int;
  val one: Int;

  context IntOps =
    Int.add,
    Int.sub,
    Int.mul,
    Int.div,
    Int.rem,
    Int.neg,
    Int.and,
    Int.or,
    Int.xor,
    Int.shl,
    Int.shr,
    Int.eq,
    Int.ne,
    Int.lt,
    Int.gt,
    Int.le,
    Int.ge,
  ;
}
".

(* lib/cell.moss *)
Definition lib_cell : string :=
"import ""./access.moss"" use .read;
import ""./int.moss"" use Int;

type CellInt;

# `.read` is the shared accessor of access.moss, provided here at
# `CellInt` with `Elem=Int` (D61); `.write` is this module's own.

assume Int, CellInt {
  fn cell_int(): CellInt;

  fn .write(value: Int);
}
".

(* examples/exit.moss *)
Definition examples_exit : string :=
"import ""wasi.moss"" use WasiStd;
import ""wasistd.moss"" use Branch;
import ""wasm.moss"" use Wasm;
import ""wasip1.moss"" use Wasi;

fn main() {}
".

(* tests/errors/unclosed_string.moss *)
Definition err_unclosed_string : string :=
"import ""./nope.moss;
".

(* tests/errors/multiline_string.moss *)
Definition err_multiline_string : string :=
"import ""./nope
.moss"";
".

(* tests/errors/exponent_suffix.moss *)
Definition err_exponent_suffix : string :=
"# D68: `1ex` is no exponent, since no digit follows the `e`, so it is a
# literal running straight into a name.
import ""wasi.moss"" use WasiStd;
import ""wasistd.moss"" use Branch;
import ""wasm.moss"" use Wasm;
import ""wasip1.moss"" use Wasi;

assume Std {
  fn run() {
    let x = 1ex;
  }
}

# The platform runs `main` with the primitive context, and `run` under
# the `Std` it installs (D52).
assume Wasm, Wasi, Branch {
  fn main() {
    bind WasiStd;
    run();
  }
}
".

(* tests/errors/stray_colon.moss *)
Definition err_stray_colon : string :=
"fn foo(): ;
".

(* tests/errors/in_is_keyword.moss *)
Definition err_in_is_keyword : string :=
"# D85: `in` is a keyword, the one of `for x in xs`, so it names nothing.
fn f(in: Int);
".

(* tests/errors/unit_tag_brackets.moss *)
Definition err_unit_tag_brackets : string :=
"# D73: only a tag with a payload lists parameters; a unit tag is written
# `tag Nothing;`, without brackets, not even empty ones.
tag Nothing[];
".


(* Real files derive their ASTs (by soundness, since the parser accepts them). *)
Example lib_int_derives : exists f, Derives (list_byte_of_string lib_int) f.
Proof. eexists. apply parse_moss_sound. vm_compute. reflexivity. Qed.

Example lib_cell_derives : exists f, Derives (list_byte_of_string lib_cell) f.
Proof. eexists. apply parse_moss_sound. vm_compute. reflexivity. Qed.

Example examples_exit_derives : exists f, Derives (list_byte_of_string examples_exit) f.
Proof. eexists. apply parse_moss_sound. vm_compute. reflexivity. Qed.

(* Lexical errors have no derivation: a derivation's tokens are what the
   lexer computes (lex_complete), and here it reports an error. *)
Lemma lex_rejects s m o : lex (source_bytes (list_byte_of_string s)) = LErr m o ->
  forall f, ~ Derives (list_byte_of_string s) f.
Proof.
  intros H f (ts & Hl & _). apply lex_complete in Hl. congruence.
Qed.

Example unclosed_string_rejected : forall f, ~ Derives (list_byte_of_string err_unclosed_string) f.
Proof. eapply lex_rejects. vm_compute. reflexivity. Qed.

Example multiline_string_rejected : forall f, ~ Derives (list_byte_of_string err_multiline_string) f.
Proof. eapply lex_rejects. vm_compute. reflexivity. Qed.

(* D68: `1ex` is no exponent and no suffix. *)
Example exponent_suffix_rejected : forall f, ~ Derives (list_byte_of_string err_exponent_suffix) f.
Proof. eapply lex_rejects. vm_compute. reflexivity. Qed.

Ltac tok := repeat match goal with
    | H : Tk _ _ _ |- _ => let Hk := fresh in destruct H as [Hk ?]; vm_compute in Hk; try discriminate; subst
    | H : TkName _ _ _ |- _ => let Hk := fresh in destruct H as (Hk & ? & ?); vm_compute in Hk; try discriminate; subst
    | H : TkText _ _ _ _ |- _ => let Hk := fresh in destruct H as (Hk & ? & ?); vm_compute in Hk; try discriminate; subst
    | H : kind _ <> _ |- _ => vm_compute in H; try congruence; clear H
    | H : kind _ = _ |- _ => vm_compute in H; try discriminate; clear H
    end.

(* A grammar error: `fn foo(): ;` has no type after the `:`. *)
Example stray_colon_rejected : forall f, ~ Derives (list_byte_of_string err_stray_colon) f.
Proof.
  intros f (ts & Hl & ts1 & ts2 & Hi & Hd). apply lex_complete in Hl. vm_compute in Hl.
  injection Hl as <-.
  inversion Hi; subst; tok.
  2: { match goal with H : Import_ _ _ _ |- _ => inversion H; subst; tok end. }
  inversion Hd; subst; tok.
  match goal with H : Decl _ _ _ |- _ => inversion H; subst; tok end.
  all: try match goal with H : Function_ _ _ _ |- _ => inversion H; subst; tok end.
  all: try match goal with H : CommaList _ _ _ _ _ |- _ => inversion H; subst; tok end.
  all: try match goal with H : ~ In _ _ |- _ => vm_compute in H; solve [exfalso; apply H; auto] end.
  all: try match goal with H : OptType _ _ _ |- _ => inversion H; subst; tok end.
  all: try match goal with H : Type_ _ _ _ |- _ => apply type_first in H; vm_compute in H; intuition discriminate end.
  all: match goal with H : _ = _ |- _ => vm_compute in H; discriminate
                     | H : _ <> _ |- _ => vm_compute in H; congruence end.
Qed.

Ltac refute := do 40 try progress (tok; try match goal with
  | H : Function_ _ _ _ |- _ => inversion H; clear H; subst
  | H : Param _ _ _ |- _ => inversion H; clear H; subst
  | H : OptType _ _ _ |- _ => inversion H; clear H; subst
  | H : TagParams _ _ _ |- _ => destruct H as [(? & ? & ?)|(? & ? & ? & ? & ?)]
  | H : NameList _ _ _ |- _ => destruct H as (? & ? & ? & ? & ?)
  | H : ~ In _ _ |- _ => vm_compute in H; solve [exfalso; apply H; auto]
  | H : In _ _ |- _ => vm_compute in H; solve [intuition discriminate]
  | H : Type_ _ _ _ |- _ => apply type_first in H; vm_compute in H; solve [intuition discriminate]
  | H : _ = _ |- _ => vm_compute in H; solve [discriminate]
  | H : _ <> _ |- _ => vm_compute in H; solve [congruence]
  | H : CommaList _ _ _ _ _ |- _ => inversion H; clear H; subst
  | H : RecordType _ _ _ |- _ => inversion H; clear H; subst
  end).

Ltac refute_file Hl Hi Hd :=
  apply lex_complete in Hl; vm_compute in Hl; injection Hl as <-;
  inversion Hi; subst; tok;
  try match goal with H : Import_ _ _ _ |- _ => inversion H; subst; tok end;
  inversion Hd; subst; tok;
  try match goal with H : Decl _ _ _ |- _ => inversion H; subst; tok end;
  refute.

(* D85: `in` is a keyword, so `fn f(in: Int);` has no parameter name. *)
Example in_is_keyword_rejected : forall f, ~ Derives (list_byte_of_string err_in_is_keyword) f.
Proof. intros f (ts & Hl & ts1 & ts2 & Hi & Hd). refute_file Hl Hi Hd. Qed.

(* D73: a unit tag has no brackets: `tag Nothing[];` *)
Example unit_tag_brackets_rejected : forall f, ~ Derives (list_byte_of_string err_unit_tag_brackets) f.
Proof. intros f (ts & Hl & ts1 & ts2 & Hi & Hd). refute_file Hl Hi Hd. Qed.

(* `_` is a wildcard token, never a declaration, parameter or import name.
   A longer name may still start or end with an underscore. *)
Example underscore_parameter_rejected : forall f,
  ~ Derives (list_byte_of_string "fn f(_: Int);") f.
Proof.
  intros f H. apply FrontEndComplete.parse_moss_complete in H.
  vm_compute in H. discriminate.
Qed.

Example underscore_receiver_rejected : forall f,
  ~ Derives (list_byte_of_string "fn _.m();") f.
Proof.
  intros f H. apply FrontEndComplete.parse_moss_complete in H.
  vm_compute in H. discriminate.
Qed.

Example underscore_alias_rejected : forall f,
  ~ Derives (list_byte_of_string "import ""x"" as _;") f.
Proof.
  intros f H. apply FrontEndComplete.parse_moss_complete in H.
  vm_compute in H. discriminate.
Qed.

Example underscore_names_and_wildcard :
  exists f, Derives (list_byte_of_string
    "fn _f(x_: Int): Int { match x_ { _ => x_ } }") f.
Proof.
  eexists. apply parse_moss_sound. vm_compute. reflexivity.
Qed.

(* D88: the raw spelling has exactly the ordinary string expression's AST. *)
Example raw_string_matches_quoted :
  parse_moss "fn main() { let text =
  > hello
  > world
  >
;
}" = parse_moss "fn main() { let text = ""hello\nworld\n\n""; }".
Proof. vm_compute. reflexivity. Qed.

Example raw_empty_at_eof :
  source_bytes (list_byte_of_string ">") =
  list_byte_of_string "(""\n"")".
Proof. vm_compute. reflexivity. Qed.
