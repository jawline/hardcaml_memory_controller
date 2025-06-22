open Core
open Hardcaml
open Hardcaml_custom_handshake
open Signal

module Make
    (S : Handshake_intf.S)
    (M : sig
       val num_channels : int
     end) =
struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; ch_to_controller : 'a S.Source.t list [@length M.num_channels]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { which_ch : 'a [@bits num_bits_to_represent (M.num_channels - 1)]
      ; selected_ch : 'a S.Source.t
      ; acks : 'a S.Dest.t list [@length M.num_channels]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let rotate n xs = List.(concat [ drop xs n; take xs n ])

  let round_robin_priority_select ~clock ~ch_to_controller scope =
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let%hw round_robin =
      reg_fb
        ~width:(Signal.num_bits_to_represent (M.num_channels - 1))
        ~f:(mod_counter ~max:(M.num_channels - 1))
        reg_spec_no_clear
    in
    let channels =
      List.mapi
        ~f:(fun (ch : int) (t : Signal.t S.Source.t) : Signal.t With_valid.t ->
          { With_valid.valid = t.valid
          ; value = of_int_trunc ~width:(num_bits_to_represent (M.num_channels - 1)) ch
          })
        ch_to_controller
    in
    mux_init
      ~f:(fun (ch : int) -> (rotate ch channels |> priority_select).value)
      round_robin
      M.num_channels
  ;;

  let priority_order ~clock:_ ~ch_to_controller _scope =
    let channels =
      List.mapi
        ~f:(fun (ch : int) (t : Signal.t S.Source.t) : Signal.t With_valid.t ->
          { With_valid.valid = t.valid
          ; value = of_int_trunc ~width:(num_bits_to_represent (M.num_channels - 1)) ch
          })
        ch_to_controller
    in
    (priority_select channels).value
  ;;

  let create ~priority_mode scope ({ clock; clear = _; ch_to_controller } : _ I.t) =
    let%hw which_ch =
      if M.num_channels = 1
      then gnd
      else (
        match priority_mode with
        | Priority_mode.Round_robin ->
          round_robin_priority_select ~clock ~ch_to_controller scope
        | Priority_order -> priority_order ~clock ~ch_to_controller scope)
    in
    let selected_ch =
      if M.num_channels = 1
      then List.hd_exn ch_to_controller
      else S.Source.Of_signal.mux which_ch ch_to_controller
    in
    { O.which_ch
    ; selected_ch
    ; acks =
        List.mapi
          ~f:(fun i t -> { S.Dest.ready = which_ch ==:. i &: t.valid })
          ch_to_controller
    }
  ;;

  let hierarchical ?instance ~priority_mode (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ?instance
      ~scope
      ~name:"memory_channel_arbitrator"
      (create ~priority_mode)
      input
  ;;
end
