Require Import Coq.Lists.List.
Require Import Coq.Sets.Ensembles.
Import ListNotations.
From OakIFC Require Import
    Lattice
    Parameters
    GenericMap
    Lattice
    State
    Events
    ModelTypes
    LowEquivalences.

From RecordUpdate Require Import RecordSet.
Import RecordSetNotations.

(* Ensembles don't have implicit type params and this line fixes that *)
Arguments Ensembles.Empty_set{U}.

(*============================================================================
* Trace Low Equivalences
*===========================================================================*)

(* We might need two different definitions of trace low-equivalence
* depending on the top-level security condition *)

(* This is a straightforward definition of trace low-equivalence
    Roughly, it says that 
    t1 =L t2 <-> forall i, t1[i] =L t2[i].

    This definition would be useful for a 
    "possibilistic security condition". A possibilistic security condition
    says that two executions look the same from the perspective of an observer
    if all _possible behaviors_ look the same if they begin from initial states
    that look the same to the observer.

    Possibilistic security conditions say that
    forall s1 s2 t1, 
        (s1 =L s2 -> <c, s1> => t1),
        exists t2, <c, s2> => t2 /\ t1 =L t2.

    In other words there is some way to reach an execution trace
    that looks the same beginning from the other state.
    
    Trapeze uses a possibilistic definition of security:
    https://pdfs.semanticscholar.org/809b/f2702a765b9e7dba4624a1dbc53af11579db.pdf
    See also:
    https://www.cs.cornell.edu/andru/papers/csfw03.pdf

    and discussion in PossibilisticNI.v
*)

Inductive trace_low_eq {s_leq: @low_eq_t state}{e_leq: @low_eq_t event_l}:
        @trace_low_eqT (state * (@labeled event)) :=
    | NilEQ ell: trace_low_eq ell [] []
    | AddBoth ell xs xe ys ye t1 t2:
        trace_low_eq ell t1 t2 ->
        e_leq ell xe ye ->
        s_leq ell xs ys ->
        trace_low_eq ell ((xs, xe)::t1) ((ys, ye)::t2).


(* An alternative way of specifying security for
   concurrent systems is observational determinism, which says
   that for any two executions that begin from low-equivalent
   initial states, the actual observed behaviors 
   (by contrast to possibly observed behaviors)
   _always_ look the same.

    This looks like:
    forall s1 s2 t1 t2,
        (s1 =L s2) /\
        (step_multi s1) => t1 /\
        (step_multi s2) => t2 ->
            t1 =L t2.

    If we write this top-level theorem using the straightforward
    definition of trace low-equivalence from above, the security condition
    would rule out *some* timing channels that we know our system does not
    prevent (so the security condition would not work). 

    The straightforward security condition would rule out the case where:
    - The observer is L
    - There is label L' s.t. not (L' flowsTo L)
    - A node called Other with Label L' takes more state transitions in one execution to
    perform some computation than in the other.

    While the "Other" node is executing, it can only affect parts of the system
    labeled L' (or higher), so for this part of a single execution, it will
    look like a sequence where ... si =L si+1 =L si+2 ... . In other words, the
    sub-sequence is low-equivalent. If the observer really can't measure time,
    two sequences that differ just in the number of transitions by "Other"
    really do look the same.

    This definition of trace low-equivalence rules this out by collapsing
    adjacent low-equivalent states (called "high stutter") in the traces.
*)

Inductive stut_trace_low_eq {s_leq: @low_eq_t state}{e_leq: @low_eq_t event_l}:
        @trace_low_eqT (state * (@labeled event)) :=
    | SNilEQ ell: stut_trace_low_eq ell [] []
    | SAddBoth ell xs xe ys ye t1 t2:
        stut_trace_low_eq ell t1 t2 ->
        event_low_eq ell xe ye ->
        state_low_eq ell xs ys ->
        stut_trace_low_eq ell ((xs, xe)::t1) ((ys, ye)::t2)
    | SAddEqR ell xs xe ys ye t1 t2:
        stut_trace_low_eq ell t1 ((ys, ye)::t2) ->
        event_low_eq ell xe ye ->
        state_low_eq ell xs ys ->
        stut_trace_low_eq ell t1 ((xs, xe)::(ys, ye)::t2)
    | SAddEqL ell xs xe ys ye t1 t2:
        stut_trace_low_eq ell ((ys, ye)::t1) t2 ->
        event_low_eq ell xe ye ->
        state_low_eq ell xs ys ->
        stut_trace_low_eq ell ((xs, xe)::t1) ((ys, ye)::t2).

(*============================================================================
* Low Equivalences for escape hatch condition
* ==========================================================================*)
(* These are used for downgrading conditions involving *)
Definition project_downgrades (e_l: event_l) :=
    match e_l.(obj) with
        | Some e => 
            match e with
                | DownEv _ _ _ => e_l
                | _ => Labeled _ None e_l.(lbl)
            end
        | None => e_l
    end.

Fixpoint extract_downgrade_trace (t: @trace (state * list event_l)):
        @trace (list event_l) :=
    match t with
        | [] => []
        | (state_x, evl_x) :: ts => 
            (map project_downgrades evl_x) :: (extract_downgrade_trace ts)
    end.

Inductive ev_list_low_eq: level -> list event_l -> list event_l -> Prop :=
    | EvlNilEQ ell: ev_list_low_eq ell [] []
    | EvlBothEq ell e1 e2 l1 l2:
        ev_list_low_eq ell l1 l2 ->
        event_low_eq ell e1 e2 ->
        ev_list_low_eq ell (e1::l1) (e2::l2).

Inductive dwn_t_low_eq: @trace_low_eqT (list event_l) :=
    | ETNilEq ell: dwn_t_low_eq ell [] []
    | ETAddBoth ell e1 e2 t1 t2:
            dwn_t_low_eq ell t1 t2 ->
            ev_list_low_eq ell e1 e2 ->
            dwn_t_low_eq ell (e1 :: t1) (e2 :: t2).

Inductive trace_low_eq_down: @trace_low_eqT (state * list event_l) :=
    | TDNilEQ ell: trace_low_eq_down ell [] []
    | TDAddBoth ell s1 s2 el1 el2 t1 t2:
        trace_low_eq_down ell t1 t2 ->
        ev_list_low_eq ell el1 el2->
        state_low_eq ell s1 s2->
        trace_low_eq_down ell ((s1, el1)::t1) ((s2, el2)::t2).


