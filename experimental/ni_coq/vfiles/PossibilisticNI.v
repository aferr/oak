Require Import Coq.Lists.List.
Import ListNotations.
From OakIFC Require Import
    Lattice
    Parameters
    GenericMap
    RuntimeModel
    ModelSemUtils
    EvAugSemantics
    State
    Events
    LowEquivalences
    TraceTheorems
    NIUtilTheorems
    Unwind
    PossibilisticNI_def
    Tactics.
From RecordUpdate Require Import RecordSet.
Import RecordSetNotations.
Local Open Scope map_scope.
Local Open Scope ev_notation.

(*
This is a proof that step_system_ev enforces Possibilistic Noninterference
*)

Lemma invert_chans_state_low_proj_flowsto ell lvl s han :
  lvl <<L ell ->
  lvl <<L (chans (state_low_proj ell s)).[? han].(lbl) ->
  lvl <<L (chans s).[? han].(lbl).
Proof.
  destruct s.
  repeat match goal with
         | _ => progress cbn [state_low_proj
                               State.chans State.lbl ]
         | _ => progress cbv [low_proj chan_state_low_proj fnd]
         | _ => destruct_match
         | _ => tauto
         end.
    (*
    Note: I think this is not true with the low-projection def
    where labels are partially secret
    *)
Qed.

Hint Resolve multi_system_ev_refl multi_system_ev_tran : multi.

(* hints for eauto in event part of the unobservable step proof *)
Hint Extern 4 (low_proj _ _  = _ ) => erewrite nflows_labeled_proj : unobs_ev.
Hint Extern 4 (_  = low_proj _ _ ) => erewrite nflows_labeled_proj : unobs_ev.
Hint Extern 4 (event_low_eq _ _ _) => unfold event_low_eq : unobs_ev.
Hint Extern 4 (low_eq _ _ _) => unfold low_eq : unobs_ev.

Definition empty_event (ell: level) := Labeled event None ell.

Theorem trace_leq_imples_head_st_leq: forall ell t1 t2 s1 s2,
    (head_st t1 = Some s1) ->
    (head_st t2 = Some s2) ->
    (trace_low_eq ell t1 t2) ->
    (state_low_eq ell s1 s2).
Proof.
    inversion 3. 
    - 
        exfalso. rewrite <- H3 in H. inversion H.
    - 
        assert (xs = s1). {
            assert (head_st ((xs, xe) :: t0 ) = Some xs) by reflexivity.
            congruence.
        }

        assert (ys = s2). {
            assert (head_st ((ys, ye) :: t3 ) = Some ys) by reflexivity.
            congruence.
        }
    congruence.
Qed.

Theorem unobservable_node_step: forall ell s s' e id nl n,
    s.(nodes).[? id] = nl ->
    nl.(obj) = Some n ->
    step_node_ev id n.(ncall) s s' e ->
    ~(lbl nl <<L ell) ->
    (state_low_eq ell s s') /\ (event_low_eq ell e (empty_event e.(lbl))).
Proof.
    intros. inversion H1; (split; [ inversion H5 | ] ); subst_lets; crush; 
        eauto with unobs_ev. (* handles all event-low-eq cases: *) 
        all: separate_hyp node; separate_hyp channel.
    - (* WriteChannel *)
        assert (~ nlbl0 <<L ell) by congruence.
        assert ( ~ (lbl (chans s).[? han] <<L ell) ) by eauto using ord_trans.
        eapply state_low_eq_trans.
        eapply state_upd_node_unobs; eauto.
        eapply state_chan_append_labeled_unobs; eauto.
    - (* ReadChannel; *)
        assert (~ nlbl0 <<L ell) by congruence.
        assert ( ~ (clbl <<L ell) ) by eauto using ord_trans.
        eapply state_low_eq_trans.
        eapply (state_upd_node_unobs _ _ _  
            (node_get_hans n0 msg0) ltac:(eauto)).
        eapply (state_upd_chan_unobs _ _ _ (chan_pop ch)
            ltac:(eauto)).
        Unshelve. subst. eauto.
    - (* CreateChannel *)
        (* there are no unobservable channel creations, these only happen
        from public *)
        pose proof (bot_is_bot ell).
        rewrite H4 in H2.
        contradiction.
    - (* CreateNode *)
        (* there are no unobservable node creations, these only happen
        from public*)
        pose proof (bot_is_bot ell).
        rewrite H4 in H2.
        contradiction.
Qed.

Theorem low_eq_to_unobs {A: Type}: forall ell (x1 x2: @labeled A),
    low_eq ell x1 x2 ->
    ~(x1.(lbl) <<L ell) ->
    ~(x2.(lbl) <<L ell).
Proof.
    destruct x1, x2. cbv [low_eq low_proj State.lbl].
    intros. destruct (lbl <<? ell); destruct (lbl0 <<? ell);
        try eauto; try contradiction. 
    inversion H. unfold not. intros. congruence.
Qed.



Theorem step_implies_lowproj_steps_leq: forall ell s1 s1' e1,
    (step_system_ev s1 s1' e1) ->
    (exists s2' e2,
        (step_system_ev (state_low_proj ell s1) s2' e2) /\
        (state_low_eq ell s1' s2') /\
        (event_low_eq ell e1 e2)).
Proof.
    intros. inversion H; subst. 
    - (* SystemSkip *)
        exists (state_low_proj ell s1'), (empty_event ell0); repeat try split.
        constructor. all: symmetry; eapply state_low_proj_loweq.
    - (* NodeSetp *)
        rename s' into s1'. rename s'' into s1''. rename H2 into H_step_s1_s1'.
        remember ((nodes s1).[?id]) as nl.
        destruct (nl.(lbl) <<? ell).
        * (* flowsto case *)
            inversion H_step_s1_s1'; crush; remember ((nodes s1).[?id]) as nl;
            pose proof (state_nidx_to_proj_state_idx ell _ _ nl
                ltac:(eauto)) as Hn_idx_s1proj;
            (erewrite flows_labeled_proj in Hn_idx_s1proj; eauto);
            inversion H3; crush; subst_lets.
            all: try replace n0 with n in * by
            (pose proof (can_split_node_index _ _ _ _ ltac:(eauto));
                logical_simplify; congruence). 
            (*
                I can't quite get this to work. Coq complains
                about s1 not being found in environment
                all: try rename s1' into s1. 
                    attempt to try SInternal from erroring
                    in following tactic:
                all: try lazymatch goal with
                    | H: (nodes s1).[? id] = _ |- _ =>
                        rewrite H in Hn_idx_s1proj;
                        pose proof (can_split_node_index _ _ _ _ Hn_idx_s1proj);
                        logical_simplify
                end.
            *)
            + (* WriteChannel *)
                do 2 eexists; split_ands; [ | | reflexivity ].
                (* step *)
                rewrite H5 in Hn_idx_s1proj;
                pose proof (can_split_node_index _ _ _ _ Hn_idx_s1proj);
                logical_simplify.
                apply_all_constructors; eauto. congruence.
                erewrite state_hidx_to_proj_state_hidx'.
                eapply proj_labels_increase.
                eauto.
                (* low-equiv *)
                subst_lets. eauto 7 with unwind.
            + (* ReadChannel *)
                do 2 eexists; split_ands; [ | | reflexivity ].
                (* step *)
                rewrite H5 in Hn_idx_s1proj;
                pose proof (can_split_node_index _ _ _ _ Hn_idx_s1proj);
                logical_simplify.
                apply_all_constructors; eauto. congruence.
                pose proof (state_hidx_to_proj_state_hidx ell _ _ _ ltac:(eauto)).
                assert (nlbl <<L ell) by congruence.  (* the last eauto needs this *)
                rewrite flows_labeled_proj in H12; eauto.
                simpl. eauto using ord_trans.
                (* state low_eq *)
                subst_lets. eauto 6 with unwind.
            + (* CreateChannel *)
                do 2 eexists; split_ands; [ | | reflexivity ]. 
                (* step *)
                rewrite H4 in Hn_idx_s1proj;
                pose proof (can_split_node_index _ _ _ _ Hn_idx_s1proj);
                logical_simplify.
                apply_all_constructors; eauto. congruence.
                eapply proj_preserves_fresh_han; eauto.
                (* state loweq *)
                subst_lets. eauto 7 with unwind.
            + (* CreateNode *)
                do 2 eexists; split_ands; [ | | reflexivity ].
                rewrite H5 in Hn_idx_s1proj;
                pose proof (can_split_node_index _ _ _ _ Hn_idx_s1proj);
                logical_simplify.
                apply_all_constructors; eauto. congruence.
                eapply proj_preserves_fresh_nid; eauto.
                eauto with unwind.
            +  (* Internal *)
                do 2 eexists; split_ands; [ | | reflexivity ].
                eapply SystemEvStepNode; eauto.
                rewrite Hn_idx_s1proj. eauto.
                rewrite <- H1.
                eapply SInternalEv.
                congruence.
                econstructor.
                eauto with unwind.
        * (* not flowsTo case *)
            rename n0 into Hflows.
            pose proof (unobservable_node_step _ _ _ _ _ nl _ ltac:(eauto)
                ltac:(eauto) H_step_s1_s1' ltac:(eauto))
                as [Hustep Huev].
            exists (state_low_proj ell s1), (empty_event (lbl e1)).
            split. crush. split.
            (* states leq *)
            pose proof (state_loweq_to_deref_node _ _ _ _ (nodes s1).[?id]
                ltac:(eauto) ltac:(eauto)) 
                as [ns1' [Hns1'idx Hns1'leq]].
            unfold s1''. transitivity s1'.
            symmetry. 
            eapply set_call_unobs; eauto.
            rewrite <- Hns1'idx in Hns1'leq.
            eapply low_eq_to_unobs; eauto. 
            congruence.
            pose proof (state_low_proj_loweq ell s1). 
            symmetry. transitivity s1; congruence.
            (* events leq *)
            congruence.
Qed.

Theorem flows_uncons_chan_state_proj: forall ell s han ch,
    lbl (chans (state_low_proj ell s)).[? han] <<L ell ->
    obj (chans (state_low_proj ell s)).[? han] = Some ch ->
    obj (chans s).[? han] = Some ch.
Proof.
    autounfold with loweq. autounfold with structs. simpl.
    intros. destruct_match. destruct (lbl <<? ell);
    (eauto || contradiction).
Qed.

Theorem state_proj_preserves_chan_lbl: forall ell s han,
    lbl (chans (state_low_proj ell s)).[? han] =
    lbl (chans s).[? han].
Proof.
    destruct s. autounfold with loweq. 
    simpl. autounfold with structs.
    intros. destruct_match. 
    destruct (lbl <<? ell); reflexivity.
Qed.

Theorem low_proj_steps_implies_leq_step: forall ell s s1' e1,
    (step_system_ev (state_low_proj ell s) s1' e1) ->
    (exists s2' e2,
        (step_system_ev s s2' e2) /\
        (state_low_eq ell s1' s2') /\
        (event_low_eq ell e1 e2)).
Proof.
    intros. inversion H; subst.
    - (* SystemSkip *)
        exists s, (empty_event ell0). split. constructor.
        split. eapply state_low_proj_loweq. reflexivity.
    - (* NodeStep *)
        rename s'' into s1''. rename s' into s1'. rename H2 into H_step_projs_s1'.
        remember ((nodes (state_low_proj ell s)).[? id]) as nl.
        pose proof (uncons_proj_node_s _ _ _ nl ltac:(eauto))
            as [n' [Hidx_n' Hproj_n']].
        destruct (nl.(lbl) <<? ell).
        * (* flowsto case*)
            inversion H_step_projs_s1'; inversion H3.
            all: rewrite @flows_labeled_proj in *; eauto; crush.
            all: erewrite uncons_proj_node_s' in *.
            all: lazymatch goal with
                | H: lbl (low_proj _ _ ) <<L _ |- _ <<L _ =>
                    rewrite <- low_proj_preserves_obs in H; eauto
                | _ => idtac
            end.
            all: separate_hyp node; separate_hyp channel.
            + (* WriteChannel *)
                remember (s_set_call (state_chan_append_labeled han msg
                                        (state_upd_node id n'0 s)) id c') as s2''.
                eexists s2'', (lbl _ ---> msg); split; [ | split].
                { crush; try congruence. 
                    separate_goal; eauto; congruence.
                  rewrite <-Hproj_n' in *.
                  eauto using invert_chans_state_low_proj_flowsto. }
                { crush; subst_lets; eauto with unwind. }
                { crush. congruence. }
            + (* ReadChannle *)
                remember (s_set_call (state_upd_chan han (chan_pop ch)
                   (state_upd_node id (node_get_hans n0 msg0) s)) id c') as s2''.
                eexists s2'', (lbl _ <--- msg).
                split; [ | split].
                subst_lets.
                {
                    crush; try separate_goal; try congruence.
                    all: rewrite <- Hproj_n' in *.
                    all: pose proof (ord_anti _ _ ltac:(eauto) ltac:(eauto)).
                    auto. eapply flows_uncons_chan_state_proj; eauto. congruence.
                    erewrite state_proj_preserves_chan_lbl in *.
                    congruence.
                }
                { crush; subst_lets; eauto with unwind. }
                { crush. congruence. }
            + (* CreateChannel *) admit.
            + (* CreateNode *) admit. 
            + (* Internal *) admit.
          (*
            + (* ReadChannel *)
                pose proof (uncons_proj_chan_s _ _ _ _ ltac:(eauto))
                    as [ch2 [H_ch'_idx H_ch2_proj]].
                remember (s_set_call (state_upd_chan han ch'
                   (state_upd_node id (node_get_hans n msg0)
                    s)) id c') as s2'.
                exists s2'; eexists; repeat split; crush.
                rewrite chan_projection_preserves_lbl in *.
                rewrite flows_chan_proj in *; eauto using ord_trans.
                unfold n'0, s1'' in *.
                eauto with unwind.
            + (* CreateChannel *)
                remember (s_set_call (state_upd_node id (node_add_whan h n)
                    (state_upd_node id (node_add_rhan h n)
                     (state_upd_chan h (new_chan lbl) 
                     s ))) id c') as s2'. 
                exists s2'; eexists; repeat split; crush.
                erewrite <- proj_pres_handle_fresh; eauto.
                unfold s1'', s'0, s3, s2.
                eauto with unwind.
            + (* CreateNode *)
                remember (s_set_call
                   (state_upd_node id (node_add_rhan h n)
                   (state_upd_node new_id
                      {|
                        nlbl := lbl;
                        read_handles := Ensembles.Singleton h;
                        write_handles := Ensembles.Empty_set;
                        ncall := Internal
                      |} s)) id c' ) as s2'.
                exists s2'; eexists; repeat split; crush.
                erewrite <- proj_pres_nid_fresh; eauto.
                unfold s1'', s'0, s4 in *.
                eauto with unwind.
            + (* Internal *)
                unfold s1''.
                exists (s_set_call s id c'); eexists; repeat split; crush.
                eauto with unwind.
        *)
        (* by cases on the command by n in s *)
        * (* not flowsTo case *)
            pose proof (unobservable_node_step _ _ _ _ _ nl _ 
                ltac:(eauto) ltac:(eauto) ltac:(eauto) ltac:(eauto))
                as [Hustep Huev].
            exists s, (empty_event (lbl e1)); split; [ | split].
            constructor.
            + (* s1'' ={ell} s *)
                apply state_low_eq_sym.
                eapply (state_low_eq_trans _ s s1' s1'').
                eapply (state_low_eq_trans _ s (state_low_proj ell s) s1').
                apply state_low_eq_sym.
                eapply state_low_proj_loweq.
                unfold s1''. 
                subst_lets. congruence.
                assert (H_leq_s_s1': state_low_eq ell s s1'). {
                    eapply (state_low_eq_trans _ s
                        (state_low_proj ell s) s1').
                    apply state_low_eq_sym.
                    eapply state_low_proj_loweq.
                    assumption.
                }
                specialize (state_loweq_to_deref_node ell s s1' id n'
                    Hidx_n' H_leq_s_s1') as [ns1' [Hidx_s1' H_leq_n'_n2]].
                eapply set_call_unobs.

                admit.
                (*
                eassumption.
                replace (lbl ns1') with (lbl n')
                    by (eapply node_low_eq_to_lbl_eq; eassumption).
                replace (lbl n') with (lbl nl). eauto.
                rewrite <- Hproj_n'.
                eapply low_projection_preserves_lbl.
                *)
            + (* e1 ={ell} *) assumption.
Admitted.

Lemma step_node_projection ell s1 s2 s3 id c :
  state_low_eq ell s1 s2 ->
  step_node id c (state_low_proj ell s2) s3 ->
  (exists s4, state_low_eq ell s3 s4
         /\ step_node id c (state_low_proj ell s1) s4).
Proof.
  intros Hleq Hstep. pose proof (state_low_eq_sym _ _ _ Hleq).
  invert_clean Hstep; intros.
  all:repeat erewrite (state_low_eq_implies_node_lookup_eq ell s1 s2) in * by eauto.
  all:repeat erewrite (state_low_eq_implies_chan_lookup_eq ell s1 s2) in * by eauto.
  all:eexists; split.
  all:try lazymatch goal with
      | |- step_node _ _ _ _ =>
        econstructor; solve [eauto]
      | _ => subst_lets
      end.
  all:eauto using state_low_eq_projection with unwind.
Admitted.

Lemma step_node_ev_projection ell s1 s2 s3 id c e :
  state_low_eq ell s1 s2 ->
  step_node_ev id c (state_low_proj ell s2) s3 e ->
  (exists s4, state_low_eq ell s3 s4
         /\ step_node_ev id c (state_low_proj ell s1) s4 e).
Proof.
  intros Hleq Hstep. invert_clean Hstep; intros.
  all:lazymatch goal with
      | H : step_node _ _ _ _ |- _ =>
        eapply step_node_projection in H; [ | solve [eauto] ]
      end.
  all:logical_simplify.
  all:eexists; split; [ solve [eauto] | econstructor ].
  all:repeat erewrite (state_low_eq_implies_node_lookup_eq ell s1 s2) by eauto.
  all:repeat erewrite (state_low_eq_implies_chan_lookup_eq ell s1 s2) by eauto.
  all:eauto.
Qed.

Lemma step_system_ev_projection ell s1 s2 s3 e :
  state_low_eq ell s1 s2 ->
  step_system_ev (state_low_proj ell s2) s3 e ->
  (exists s4, state_low_eq ell s3 s4
         /\ step_system_ev (state_low_proj ell s1) s4 e).
Proof.
  intros Hleq Hstep. pose proof (state_low_eq_sym _ _ _ Hleq).
  invert_clean Hstep; intros.
  { eexists; split.
    { apply state_low_eq_projection.
      apply state_low_eq_sym; eauto. }
    { econstructor; eauto. } }
  { lazymatch goal with
    | H : step_node_ev _ _ _ _ _ |- _ =>
      eapply step_node_ev_projection in H; [ | solve [eauto] ]
    end.
    logical_simplify. subst_lets.
    eexists; split; eauto; [ | econstructor; eauto ];
      [ solve [eauto with unwind] | ].
    erewrite state_low_eq_implies_node_lookup_eq by eauto.
    eauto. }
Qed.

Theorem possibilistic_ni_1step: forall ell s1 s2 s1' e1,
    (state_low_eq ell s1 s2) ->
    (step_system_ev s1 s1' e1) ->
    (exists s2' e2,
        (step_system_ev s2 s2' e2) /\
        (state_low_eq ell s1' s2') /\
        (event_low_eq ell e1 e2)).
Proof.
    intros ell s1 s2 s1' e1 H_leq_s1_s2 Hstep_s1_s1'.
    specialize (step_implies_lowproj_steps_leq ell s1 s1' e1 Hstep_s1_s1')
      as [s3' [ e3 [Hstep_s1_s3 [H_leq_s1'_s3' H_leq_e1_e3]]]].
    eapply step_system_ev_projection in Hstep_s1_s3;
      [ | symmetry; eassumption ].
    logical_simplify.
    match goal with
      | H : step_system_ev (state_low_proj ell s2) _ _ |- _ =>
        apply low_proj_steps_implies_leq_step in H;
          destruct H
          as [s2' [ e2 [Hstep_s2_s2' [H_leq_s3'_s2' H_leq_e3_e2]]]]
    end.
    exists s2', e2. split. assumption. split.
    - eapply state_low_eq_trans; eauto;
        eapply state_low_eq_trans; eauto.
    - eapply event_low_eq_trans; eauto.
Qed.

Theorem possibilistic_ni_unwind_t: forall ell t1 t2 t1',
(trace_low_eq_pni ell t1 t2) ->
(step_system_ev_t t1 t1') ->
(exists t2',
    (step_system_ev_t t2 t2') /\
    (trace_low_eq_pni ell t1' t2')).
Proof.
    intros. 
    inversion H; crush.
    -  (* starts from empty trace *)
        exfalso. eapply no_steps_from_empty; eauto.
    - (* non-empty trace *)
        inversion H0; inversion H4; crush.
        pose proof (possibilistic_ni_1step _ s ys s' _
            ltac:(eauto) ltac:(eauto)) as [s2' [e2 [H2step [Hsleq Heleq]]]].
        eexists ((s2', e2) :: (ys, ye) :: t3); repeat split; crush.
        econstructor; crush; eauto. 
        econstructor; crush; eauto.
Qed.

Theorem possibilistic_ni: (conjecture_possibilistic_ni step_system_ev_multi).
Proof.
  unfold conjecture_possibilistic_ni. crush.
  let H := match goal with H : step_system_ev_multi _ _ |- _ => H end in
  induction H; crush.
  all:pose proof (possibilistic_ni_unwind_t
                    _ _ _ _
                    ltac:(eassumption) ltac:(eassumption)).
  all:crush.
  all:eexists; split; crush; [ | eassumption ]; eauto with multi.
Qed.
