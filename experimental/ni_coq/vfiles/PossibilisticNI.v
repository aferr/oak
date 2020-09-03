Require Import Coq.Lists.List.
Import ListNotations.
From OakIFC Require Import
    Parameters
    RuntimeModel
    EvAugSemantics
    Events
    LowEquivalences
    TraceTheorems.
From mathcomp Require Import all_ssreflect finmap.
From RecordUpdate Require Import RecordSet.
Import RecordSetNotations.


(*
This is the top-level candidate security condition. This is a
"possibilistic security condition". A possibilistic security condition
says that two executions look the same from the perspective of an observer
if all _possible behaviors_ look the same if they begin from initial states
that look the same to the observer.
In other words there is some way to reach an execution trace
that looks the same beginning from the other state.

Trapeze uses a possibilistic definition of security:
https://pdfs.semanticscholar.org/809b/f2702a765b9e7dba4624a1dbc53af11579db.pdf
See also:
https://www.cs.cornell.edu/andru/papers/csfw03.pdf

*)

(* An alternative way of specifying security for
   concurrent systems is observational determinism, which says
   that for any two executions that begin from low-equivalent
   initial states, all actual observed behaviors 
   (by contrast to the possibly observed behaviors)
   _always_ look the same.

    This looks like:
    forall s1 s2 t1 t2,
        (s1 =L s2) /\
        (step_multi s1) => t1 /\
        (step_multi s2) => t2 ->
            t1 =L t2.

    An advantage of observational determinism over possibilistic
    noninterference is that O.D. is preserved by refinement. This does
    not matter in this context since we are not doing a refinement proof.
    
    O.D. also has requirements about data race freedom that are not
    needed to prove a possibilistic security definition. (It may be
    worth looking into whether or not the runtime actually satisfies
    these data race freedom requirements later, though it does not
    seem high priority).

    ***
    These two definitions also crucially require different
    definitions of trace low-equivalence as discussed in Events.v
    ***
*)

Definition is_init(t: trace) := length t = 1.

Definition conjecture_possibilistic_ni := forall ell t1_init t2_init t1n,
    (trace_low_eq ell t1_init t2_init) /\
    (is_init t1_init) /\
    (is_init t2_init) /\
    (step_system_ev_multi t1_init t1n) ->
    (exists t2n,
        (step_system_ev_multi t2_init t2n) /\
        (trace_low_eq ell t1n t2n)).

Theorem possibilistic_ni_1step_node: forall ell id t1 t2 s1 s2 n1 n2 t1',
    (head_st t1 = Some s1) ->
    (head_st t2 = Some s2) ->
    (s1.(nodes) .[?id] = Some n1) ->
    (s2.(nodes) .[?id] = Some n2) ->
    (trace_low_eq ell t1 t2) ->
    (step_node_ev id (ncall n1) t1 t1') ->
    (exists t2',
        (step_node_ev id (ncall n2) t2 t2') /\
        (trace_low_eq ell t1' t2')).
Proof.
Admitted.

Theorem call_havoc_unwind: forall ell id c t1 t2 s1 s2 n1 n2 t1' t2',
    (trace_low_eq ell t1 t2) ->
    (head_st t1 = Some s1) ->
    (head_st t2 = Some s2) ->
    (* may only need one n *)
    (s1.(nodes) .[?id] = Some n1) ->
    (s2.(nodes) .[?id] = Some n2) ->
    (t1' = havoc_call t1 id c n1 s1) ->
    (t2' = havoc_call t2 id c n2 s2) ->
    (trace_low_eq ell t1' t2').
Proof.
Admitted.
 
Theorem possibilistic_ni_1step: forall ell t1 t2 t1',
    (trace_low_eq ell t1 t2) ->
    (step_system_ev t1 t1') ->
    (exists t2',
        (step_system_ev t2 t2') /\
        (trace_low_eq ell t1' t2')).
Proof.
    intros.
    inversion H0; subst.
    rename t' into t1_sn. rename s into s1. rename s' into s1'. rename n into n1.
    assert (Ht2call: exists s2 n2, 
        head_st t2 = Some s2 /\ 
        (nodes s2).[? id] = Some n2). {
            admit. 
            (* by low-equivalence of t1 t2 at some ell *)
            (* no_steps_from_empty *)
            (* inspection of step_node_ev *)
        }
    destruct Ht2call as [s2 [n2 [Ht2s2 Hs2n2]]].
    assert (Hnode_1step: (exists t2_sn, (step_node_ev id (ncall n2) t2 t2_sn) /\
        (trace_low_eq ell t1_sn t2_sn)))
        by (apply (possibilistic_ni_1step_node ell id
            t1 t2 s1 s2 n1 n2 t1_sn  H1 Ht2s2 H3 Hs2n2 H H5)).
    destruct Hnode_1step as [t2_sn [Hnstep2 Hnstep_leq]].
    assert (Hs2': exists s2', head_st t2_sn = Some s2') by admit.
        (* no steps to empty *)
    destruct Hs2' as [s2' Hs2']. 
    remember (havoc_call t2_sn id c' n2 s2') as t2'.
    exists t2'. split.
    + subst. (* can step from t2_sn to t2' *)
        apply (ValidStep id n2 (ncall n2) c' s2 t2 s2' t2_sn);
            (try assumption; try reflexivity).
    + (* loq-equiv t1' t2' *)
        remember (havoc_call t1_sn id c' n1 s1') as t1' eqn:Ht1'.
        apply (call_havoc_unwind ell id c' t1_sn t2_sn s1' s2' n1 n2
            t1' t2'); (try assumption; try reflexivity).
            - admit. (* not true: node is changed, need to get new node *)
            - admit. (* not true; node is changed, need to get new node *) 
Admitted. (* NOTE: work in progress *)


Theorem possibilistic_ni: conjecture_possibilistic_ni.
Proof.
unfold conjecture_possibilistic_ni.
intros ell t1_init t2_init t1n [Hinit_tleq [Ht1_init [Ht2_init Ht1_mstep_t1n]]].
remember ([]: trace) as emp eqn:R.
induction t1n.
    - (* t1 is empty trace, which is not possible *)
        exfalso. apply (no_steps_to_empty t1_init Ht1_mstep_t1n).
    - (* inductive case *)
    inversion Ht1_mstep_t1n ; subst.
        + assert (E: exists t2n,
            (step_system_ev t2_init t2n) /\
            (trace_low_eq ell (a::t1n) t2n)) by
            apply (possibilistic_ni_1step ell t1_init t2_init
                (a::t1n) Hinit_tleq H).
            destruct E as [t2n [E1 E2]].
            exists t2n. split. constructor. assumption. assumption.
        + rename Ht1_mstep_t1n into Ht1_mstep_at1n.
        assert (E: step_system_ev_multi t2 t1n)
            by apply (step_system_multi_backwards t2 t1n a H0).
        assert (H': step_system_ev_multi t1_init t2) by
            (constructor; assumption).
        assert (Ht1_mstep_t1n: step_system_ev_multi t1_init t1n)
            by apply (step_system_transitive t1_init t2 t1n H' E).
        apply IHt1n in Ht1_mstep_t1n as [t2n [Hm_t2_init_t2n Hleq_t1n_t2n]].
        assert (E2: step_system_ev t1n (a::t1n))
            by apply (step_system_multi_extends t2 t1n a H0).
            (*need to get non-multi from E2.  *)
        assert (E3: exists t2n',
            (step_system_ev t2n t2n') /\
            (trace_low_eq ell (a::t1n) t2n')) by
            apply (possibilistic_ni_1step ell t1n t2n (a::t1n) Hleq_t1n_t2n E2).
            destruct E3 as [t2n' [Hs_t2n_t2n' Hleq_at1n_t2n']].
        exists t2n'.
        split.
        + apply (step_system_transitive t2_init t2n t2n'). assumption.
        constructor. assumption. assumption.
Qed.
