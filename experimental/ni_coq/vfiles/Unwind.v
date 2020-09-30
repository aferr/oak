Require Import Coq.Lists.List.
Import ListNotations.
From OakIFC Require Import
    Lattice
    Parameters
    GenericMap
    RuntimeModel
    EvAugSemantics
    Events
    LowEquivalences
    NIUtilTheorems.
From RecordUpdate Require Import RecordSet.
Import RecordSetNotations.
Local Open Scope map_scope.
(*============================================================================
 Unwinding Theorems
============================================================================*)
(*
This file contains theorems that prove "the unwinding condition" for 
various single pure functions. An unwinding condition is a single step of
noninterference.
*)

(*
It might be better to write them using something like this in the future:

Definition state_unwind (f: state -> state):
    forall ell s1 s2, 
        state_low_eq ell s1 s2 ->
        state_low_eq ell (f s1) (fs 2)

but we have to fix the way the definitions are curried.
*)

Theorem chan_append_unwind:
    forall ell ch1 ch2 msg,
    chan_low_eq ell ch1 ch2 ->
    chan_low_eq ell (chan_append ch1 msg) (chan_append ch2 msg).
Proof.
Admitted. (* WIP // TODO *)

Theorem state_upd_unwind_from_leqn:
    forall ell id n1 n2 s1 s2,
    node_low_eq ell n1 n2 ->
    state_low_eq ell s1 s2 ->
    state_low_eq ell (state_upd_node id n1 s1) (state_upd_node id n2 s2).
Proof.
Admitted. (* WIP *)

Theorem set_call_unwind: forall ell id c s1 s2,
    state_low_eq ell s1 s2 ->
    state_low_eq ell (s_set_call s1 id c) (s_set_call s2 id c).
Proof.
    intros. inversion H; subst.
    unfold s_set_call.
    destruct (s1.(nodes).[? id]) eqn:E1; destruct (s2.(nodes).[? id]) eqn:E2.
    - (* some, some *)
        assert (E: node_low_eq ell 
            (n <| ncall ::= (fun x=> c) |>)
            (n0 <| ncall ::= (fun x=> c) |>)).
        {
            admit.
            (*
            specialize (H0 id). rewrite E1 in H0.
            rewrite E2 in H0.
            inversion H0; subst. 
            constructor; reflexivity.
            constructor 2; assumption.
            *)
        }
        eapply state_upd_unwind_from_leqn; assumption.
    - (* some, none *)
        admit.
        (*
        exfalso. specialize (H0 id).
        rewrite E1, E2 in H0.
        assumption.
        *)
    - (* none, some *)
        admit.
        (*
        exfalso. specialize (H0 id).
        rewrite E1, E2 in H0.
        assumption.
        *)
    - admit. (* split; assumption *)
Admitted.
