Require Import Coq.Lists.List.
Import ListNotations.
From OakIFC Require Import
    Lattice
    Parameters
    GenericMap
    RuntimeModel
    ModelSemUtils
    EvAugSemanticsDwn
    State
    Events
    LowEquivalences
    TraceLowEq
    TraceTheoremsDwn
    NIUtilTheorems
    Unwind
    EscapeHatchCondition_def 
    Tactics.
From RecordUpdate Require Import RecordSet.
Import RecordSetNotations.
Local Open Scope map_scope.
Local Open Scope ev_notation.


Hint Resolve multi_system_ev_refl multi_system_ev_tran : multi.

(* hints for eauto in event part of the unobservable step proof *)
Hint Extern 4 (low_proj _ _  = _ ) => erewrite nflows_labeled_proj : unobs_ev.
Hint Extern 4 (_  = low_proj _ _ ) => erewrite nflows_labeled_proj : unobs_ev.
Hint Extern 4 (event_low_eq _ _ _) => unfold event_low_eq : unobs_ev.
Hint Extern 4 (low_eq _ _ _) => unfold low_eq : unobs_ev.

Definition empty_event (ell: level) := Labeled event None ell.

Theorem escape_hatch_1step: forall ell s1 s2 s1' d1 d2 e1,
    (state_low_eq ell s1 s2) ->
    (down_low_eq ell d1 d2) ->
    (step_system_ev s1 d1 s1' e1) ->
    (exists s2' e2,
        (step_system_ev s2 d2 s2' e2) /\
        (state_low_eq ell s1' s2') /\
        (event_low_eq ell e1 e2)).
Proof.
Admitted.

Theorem escape_hatch_unwind_t: forall ell t1 t2 dt1 dt2 t1',
(eh_trace_low_eq ell t1 t2) ->
(down_list_low_eq ell dt1 dt2) ->
(step_system_ev_t t1 t1') ->
(dt1 = (extract_downgrade_trace t1')) ->
(exists t2',
    (step_system_ev_t t2 t2') /\
    (dt2 = (extract_downgrade_trace t2')) /\
    (eh_trace_low_eq ell t1' t2')).
Proof.
    intros. 
    inversion H; crush.
    -  (* starts from empty trace *)
        exfalso. eapply no_steps_from_empty; eauto.
    - (* non-empty trace *)
        inversion H1; inversion H2; crush.
        inversion H0. crush.
        pose proof escape_hatch_1step.
        pose proof (escape_hatch_1step _ s s2 s' d _ _
            ltac:(eauto) ltac:(eauto) ltac:(eauto)).
        pose proof (escape_hatch_1step _ s s2 s' _ _ _
            ltac:(eauto) ltac:(eauto) ltac:(eauto)) as [s2' [e2' [H2step [Hsleq Heleq]]]].
        eexists ((s2', d3, e2') :: (s2, d2, e2) :: t3); repeat split; crush.
        econstructor; crush; eauto.
        unfold extract_downgrade_trace in *.
        admit. (* extract trace is = *)
        econstructor; crush; eauto.
Admitted.


Theorem escape_hatch: (conjecture_escape_hatch step_system_ev_multi).
Proof.
  unfold conjecture_escape_hatch. crush.
  let H := match goal with H : step_system_ev_multi _ _ |- _ => H end in
  induction H; crush.
  all:pose proof (escape_hatch_unwind_t
                    _ _ _ _
                    ltac:(eassumption) ltac:(eassumption)).
  all:crush.
  all:eexists; split; crush; [ | eassumption ]; eauto with multi.
Qed.
