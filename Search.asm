; =============================================================================
; RUSTY CHESS - AI Search (Negamax with Alpha-Beta)
; =============================================================================

SEARCH_DEPTH    EQU 2
SCORE_INF       EQU 30000
SCORE_MATE      EQU 20000

UNDO_ENTRY_SIZE EQU 15
UNDO_MAX        EQU 4

UNDO_FROM       EQU 0
UNDO_TO         EQU 1
UNDO_MOVING     EQU 2
UNDO_CAPTURED   EQU 3
UNDO_EP         EQU 4
UNDO_CASTLE     EQU 5
UNDO_WK         EQU 6
UNDO_BK         EQU 7
UNDO_EP_SQ      EQU 8
UNDO_EP_PIECE   EQU 9
UNDO_DID_CASTLE EQU 10
UNDO_ROOK_FROM  EQU 11
UNDO_ROOK_TO    EQU 12
UNDO_ROOK_PIECE EQU 13
UNDO_DID_PROMO  EQU 14

; =============================================================================
; AI MAKE MOVE
; =============================================================================
ai_move:
    CALL generate_moves

    LD A, (move_list_count)
    OR A
    RET Z

    ; Save root move list
    LD HL, move_list
    LD DE, ai_root_moves
    LD A, (move_list_count)
    LD (ai_root_count), A
    ADD A, A
    LD C, A
    LD B, 0
    LDIR

    ; Init
    LD HL, -SCORE_INF
    LD (ai_best_score), HL
    LD (ai_alpha), HL
    XOR A
    LD (ai_best_idx), A
    LD (ai_cur_idx), A
    LD (undo_ptr), A

.root_loop:
    LD A, (ai_cur_idx)
    LD B, A
    LD A, (ai_root_count)
    CP B
    JP Z, .root_done
    JP C, .root_done

    ; Get move
    LD A, (ai_cur_idx)
    LD L, A
    LD H, 0
    ADD HL, HL
    LD DE, ai_root_moves
    ADD HL, DE
    LD A, (HL)
    LD (move_from), A
    INC HL
    LD A, (HL)
    LD (move_to), A

    CALL make_search_move

    LD A, (side_to_move)
    XOR COLOR_MASK
    LD (side_to_move), A

    ; Child search
    LD HL, (ai_alpha)
    LD A, H
    CPL
    LD H, A
    LD A, L
    CPL
    LD L, A
    INC HL
    LD (search_beta), HL

    LD HL, -SCORE_INF
    LD (search_alpha), HL

    LD A, SEARCH_DEPTH - 1
    LD (search_depth), A

    CALL negamax

    ; Negate
    LD A, H
    CPL
    LD H, A
    LD A, L
    CPL
    LD L, A
    INC HL
    LD (ai_cur_score), HL

    LD A, (side_to_move)
    XOR COLOR_MASK
    LD (side_to_move), A

    CALL unmake_search_move

    ; Better?
    LD HL, (ai_cur_score)
    LD DE, (ai_best_score)
    CALL signed_compare_hl_de
    JP C, .not_better
    JP Z, .not_better

    LD HL, (ai_cur_score)
    LD (ai_best_score), HL
    LD A, (ai_cur_idx)
    LD (ai_best_idx), A
    LD HL, (ai_cur_score)
    LD (ai_alpha), HL

.not_better:
    LD A, (ai_cur_idx)
    INC A
    LD (ai_cur_idx), A
    JP .root_loop

.root_done:
    ; --- Play the best move using game logic ---
    ; Get best move from/to
    LD A, (ai_best_idx)
    LD L, A
    LD H, 0
    ADD HL, HL
    LD DE, ai_root_moves
    ADD HL, DE
    LD A, (HL)
    LD (move_from), A
    INC HL
    LD A, (HL)
    LD (move_to), A

    ; Set up moving_piece (needed by execute_move)
    LD A, (move_from)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (moving_piece), A

    ; Set up dest_piece
    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (dest_piece), A

    ; --- Do what execute_move does, but skip the legality check ---
    ; Save state for undo (needed by record_move and captures)
    LD A, (move_from)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (save_from_piece), A

    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (save_to_piece), A

    LD A, (white_king_sq)
    LD (save_wking), A
    LD A, (black_king_sq)
    LD (save_bking), A
    LD A, (castling_rights)
    LD (save_castling), A
    LD A, (ep_square)
    LD (save_ep), A

    XOR A
    LD (did_castle), A
    LD (did_ep_capture), A
    LD (did_promote), A
    LD (save_ep_captured), A

    ; --- Make the move on the board ---
    LD A, (move_from)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD (HL), EMPTY

    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (moving_piece)
    LD (HL), A

    ; --- King move ---
    LD A, (moving_piece)
    AND PIECE_MASK
    CP 6
    JP NZ, .ai_not_king

    LD A, (moving_piece)
    AND COLOR_MASK
    JP NZ, .ai_bking

    LD A, (move_to)
    LD (white_king_sq), A
    LD A, (castling_rights)
    AND ~CASTLE_W_ALL & $FF
    LD (castling_rights), A

    LD A, (move_from)
    LD B, A
    LD A, (move_to)
    SUB B
    CP 2
    JP Z, .ai_wk_castle
    CP $FE
    JP Z, .ai_wq_castle
    JP .ai_not_king

.ai_wk_castle:
    LD A, 1
    LD (did_castle), A
    LD A, (board + SQ_H1)
    LD (save_castle_rook), A
    LD (board + SQ_F1), A
    LD A, EMPTY
    LD (board + SQ_H1), A
    JP .ai_not_king

.ai_wq_castle:
    LD A, 1
    LD (did_castle), A
    LD A, (board + SQ_A1)
    LD (save_castle_rook), A
    LD (board + SQ_D1), A
    LD A, EMPTY
    LD (board + SQ_A1), A
    JP .ai_not_king

.ai_bking:
    LD A, (move_to)
    LD (black_king_sq), A
    LD A, (castling_rights)
    AND ~CASTLE_B_ALL & $FF
    LD (castling_rights), A

    LD A, (move_from)
    LD B, A
    LD A, (move_to)
    SUB B
    CP 2
    JP Z, .ai_bk_castle
    CP $FE
    JP Z, .ai_bq_castle
    JP .ai_not_king

.ai_bk_castle:
    LD A, 1
    LD (did_castle), A
    LD A, (board + SQ_H8)
    LD (save_castle_rook), A
    LD (board + SQ_F8), A
    LD A, EMPTY
    LD (board + SQ_H8), A
    JP .ai_not_king

.ai_bq_castle:
    LD A, 1
    LD (did_castle), A
    LD A, (board + SQ_A8)
    LD (save_castle_rook), A
    LD (board + SQ_D8), A
    LD A, EMPTY
    LD (board + SQ_A8), A

.ai_not_king:
    ; --- Rook move castling rights ---
    LD A, (moving_piece)
    AND PIECE_MASK
    CP 4
    JP NZ, .ai_not_rook
    LD A, (move_from)
    CP SQ_A1
    JP NZ, .ai_nr1
    LD A, (castling_rights)
    AND ~CASTLE_WQ & $FF
    LD (castling_rights), A
.ai_nr1:
    LD A, (move_from)
    CP SQ_H1
    JP NZ, .ai_nr2
    LD A, (castling_rights)
    AND ~CASTLE_WK & $FF
    LD (castling_rights), A
.ai_nr2:
    LD A, (move_from)
    CP SQ_A8
    JP NZ, .ai_nr3
    LD A, (castling_rights)
    AND ~CASTLE_BQ & $FF
    LD (castling_rights), A
.ai_nr3:
    LD A, (move_from)
    CP SQ_H8
    JP NZ, .ai_not_rook
    LD A, (castling_rights)
    AND ~CASTLE_BK & $FF
    LD (castling_rights), A
.ai_not_rook:

    ; --- Rook capture castling rights ---
    LD A, (move_to)
    CP SQ_A1
    JP NZ, .ai_nc1
    LD A, (castling_rights)
    AND ~CASTLE_WQ & $FF
    LD (castling_rights), A
.ai_nc1:
    LD A, (move_to)
    CP SQ_H1
    JP NZ, .ai_nc2
    LD A, (castling_rights)
    AND ~CASTLE_WK & $FF
    LD (castling_rights), A
.ai_nc2:
    LD A, (move_to)
    CP SQ_A8
    JP NZ, .ai_nc3
    LD A, (castling_rights)
    AND ~CASTLE_BQ & $FF
    LD (castling_rights), A
.ai_nc3:
    LD A, (move_to)
    CP SQ_H8
    JP NZ, .ai_nc4
    LD A, (castling_rights)
    AND ~CASTLE_BK & $FF
    LD (castling_rights), A
.ai_nc4:

    ; --- En passant capture ---
    LD A, (moving_piece)
    AND PIECE_MASK
    CP 1
    JP NZ, .ai_not_ep
    LD A, (ep_square)
    CP EP_NONE
    JP Z, .ai_not_ep
    LD B, A
    LD A, (move_to)
    CP B
    JP NZ, .ai_not_ep

    LD A, 1
    LD (did_ep_capture), A
    LD A, (moving_piece)
    AND COLOR_MASK
    JP NZ, .ai_ep_black
    LD A, (move_to)
    SUB 16
    JP .ai_ep_rem
.ai_ep_black:
    LD A, (move_to)
    ADD A, 16
.ai_ep_rem:
    LD (ep_capture_sq), A
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (save_ep_captured), A
    LD (HL), EMPTY

.ai_not_ep:
    ; --- Update EP square ---
    LD A, EP_NONE
    LD (ep_square), A
    LD A, (moving_piece)
    AND PIECE_MASK
    CP 1
    JP NZ, .ai_no_double
    LD A, (move_from)
    LD B, A
    LD A, (move_to)
    SUB B
    CP 32
    JP Z, .ai_ep_w
    CP $E0
    JP Z, .ai_ep_b
    JP .ai_no_double
.ai_ep_w:
    LD A, (move_from)
    ADD A, 16
    LD (ep_square), A
    JP .ai_no_double
.ai_ep_b:
    LD A, (move_from)
    SUB 16
    LD (ep_square), A
.ai_no_double:

    ; --- Promotion ---
    LD A, (moving_piece)
    AND PIECE_MASK
    CP 1
    JP NZ, .ai_no_promo
    LD A, (moving_piece)
    AND COLOR_MASK
    JP NZ, .ai_bpromo
    LD A, (move_to)
    AND $F0
    CP $70
    JP NZ, .ai_no_promo
    LD A, 1
    LD (did_promote), A
    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD (HL), W_QUEEN
    JP .ai_no_promo
.ai_bpromo:
    LD A, (move_to)
    AND $F0
    CP $00
    JP NZ, .ai_no_promo
    LD A, 1
    LD (did_promote), A
    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD (HL), B_QUEEN
.ai_no_promo:

    ; --- Record capture ---
    LD A, (save_to_piece)
    CALL record_capture
    LD A, (did_ep_capture)
    OR A
    JP Z, .ai_no_ep_rec
    LD A, (save_ep_captured)
    CALL record_capture
.ai_no_ep_rec:

    ; Record move in history
    CALL record_move

    ; Switch side to white
    LD A, (side_to_move)
    XOR COLOR_MASK
    LD (side_to_move), A

    ; Check for game end
    CALL check_game_end

    RET

ai_root_moves:     DEFS MAX_MOVES * 2, 0
ai_root_count:     DEFB 0
ai_best_score:     DEFW 0
ai_best_idx:       DEFB 0
ai_alpha:          DEFW 0
ai_cur_idx:        DEFB 0
ai_cur_score:      DEFW 0

; =============================================================================
; NEGAMAX SEARCH
; =============================================================================
negamax:
    LD A, (search_depth)
    OR A
    JP Z, evaluate

    CALL generate_moves

    LD A, (move_list_count)
    OR A
    JP Z, .no_moves

    ; Copy to depth buffer
    LD A, (search_depth)
    CP 2
    JP Z, .copy_d2
    LD HL, move_list
    LD DE, srch_moves_2
    LD A, (move_list_count)
    LD (srch_count_2), A
    ADD A, A
    LD C, A
    LD B, 0
    LDIR
    JP .search_init
.copy_d2:
    LD HL, move_list
    LD DE, srch_moves_1
    LD A, (move_list_count)
    LD (srch_count_1), A
    ADD A, A
    LD C, A
    LD B, 0
    LDIR

.search_init:
    LD HL, -SCORE_INF
    LD (node_best), HL
    XOR A
    LD (node_cur_idx), A

.move_loop:
    LD A, (search_depth)
    CP 2
    JP Z, .cnt_d2
    LD A, (srch_count_2)
    JP .got_cnt
.cnt_d2:
    LD A, (srch_count_1)
.got_cnt:
    LD B, A
    LD A, (node_cur_idx)
    CP B
    JP NC, .moves_done

    ; Get move
    LD A, (search_depth)
    CP 2
    JP Z, .mov_d2
    LD A, (node_cur_idx)
    LD L, A
    LD H, 0
    ADD HL, HL
    LD DE, srch_moves_2
    ADD HL, DE
    JP .got_mov
.mov_d2:
    LD A, (node_cur_idx)
    LD L, A
    LD H, 0
    ADD HL, HL
    LD DE, srch_moves_1
    ADD HL, DE
.got_mov:
    LD A, (HL)
    LD (move_from), A
    INC HL
    LD A, (HL)
    LD (move_to), A

    CALL make_search_move

    LD A, (side_to_move)
    XOR COLOR_MASK
    LD (side_to_move), A

    ; Save parent state on Z80 stack
    LD HL, (search_alpha)
    PUSH HL
    LD HL, (search_beta)
    PUSH HL
    LD HL, (node_best)
    PUSH HL
    LD A, (node_cur_idx)
    PUSH AF
    LD A, (search_depth)
    PUSH AF

    ; Child alpha = -parent beta, child beta = -parent alpha
    LD HL, (search_beta)
    LD A, H
    CPL
    LD H, A
    LD A, L
    CPL
    LD L, A
    INC HL
    PUSH HL                         ; temp save child alpha

    LD HL, (search_alpha)
    LD A, H
    CPL
    LD H, A
    LD A, L
    CPL
    LD L, A
    INC HL
    LD (search_beta), HL            ; child beta = -parent alpha

    POP HL
    LD (search_alpha), HL           ; child alpha = -parent beta

    LD A, (search_depth)
    DEC A
    LD (search_depth), A

    CALL negamax

    ; Negate child score
    LD A, H
    CPL
    LD H, A
    LD A, L
    CPL
    LD L, A
    INC HL
    LD (node_child_score), HL

    ; Restore parent state
    POP AF
    LD (search_depth), A
    POP AF
    LD (node_cur_idx), A
    POP HL
    LD (node_best), HL
    POP HL
    LD (search_beta), HL
    POP HL
    LD (search_alpha), HL

    LD A, (side_to_move)
    XOR COLOR_MASK
    LD (side_to_move), A

    CALL unmake_search_move

    ; Score > best?
    LD HL, (node_child_score)
    LD DE, (node_best)
    CALL signed_compare_hl_de
    JP C, .no_improve
    JP Z, .no_improve

    LD HL, (node_child_score)
    LD (node_best), HL

    LD DE, (search_alpha)
    CALL signed_compare_hl_de
    JP C, .check_cutoff
    JP Z, .check_cutoff
    LD (search_alpha), HL

.check_cutoff:
    LD HL, (search_alpha)
    LD DE, (search_beta)
    CALL signed_compare_hl_de
    JP C, .no_improve
    LD HL, (node_best)
    RET

.no_improve:
    LD A, (node_cur_idx)
    INC A
    LD (node_cur_idx), A
    JP .move_loop

.moves_done:
    LD HL, (node_best)
    RET

.no_moves:
    LD A, (side_to_move)
    OR A
    JP NZ, .nm_black
    LD A, (white_king_sq)
    LD B, BLACK
    JP .nm_check
.nm_black:
    LD A, (black_king_sq)
    LD B, WHITE
.nm_check:
    CALL is_square_attacked
    JP Z, .nm_stalemate
    LD HL, -SCORE_MATE
    RET
.nm_stalemate:
    LD HL, 0
    RET

search_depth:       DEFB 0
search_alpha:       DEFW 0
search_beta:        DEFW 0
node_best:          DEFW 0
node_cur_idx:       DEFB 0
node_child_score:   DEFW 0

; =============================================================================
; SIGNED 16-BIT COMPARE
; =============================================================================
signed_compare_hl_de:
    LD A, H
    XOR D
    JP M, .diff_signs
    OR A
    SBC HL, DE
    ADD HL, DE
    RET
.diff_signs:
    BIT 7, H
    RET Z
    SCF
    RET

; =============================================================================
; MAKE SEARCH MOVE
; =============================================================================
make_search_move:
    LD A, (undo_ptr)
    CALL get_undo_ix

    LD A, (ep_square)
    LD (IX+UNDO_EP), A
    LD A, (castling_rights)
    LD (IX+UNDO_CASTLE), A
    LD A, (white_king_sq)
    LD (IX+UNDO_WK), A
    LD A, (black_king_sq)
    LD (IX+UNDO_BK), A

    XOR A
    LD (IX+UNDO_DID_CASTLE), A
    LD (IX+UNDO_EP_SQ), A
    LD (IX+UNDO_DID_PROMO), A

    LD A, (move_from)
    LD (IX+UNDO_FROM), A
    LD A, (move_to)
    LD (IX+UNDO_TO), A

    LD A, (move_from)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (IX+UNDO_MOVING), A

    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (IX+UNDO_CAPTURED), A

    LD A, (move_from)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD (HL), EMPTY

    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (IX+UNDO_MOVING)
    LD (HL), A

    ; King
    LD A, (IX+UNDO_MOVING)
    AND PIECE_MASK
    CP 6
    JP NZ, .s_nk
    LD A, (IX+UNDO_MOVING)
    AND COLOR_MASK
    JP NZ, .s_bk
    LD A, (move_to)
    LD (white_king_sq), A
    JP .s_nk
.s_bk:
    LD A, (move_to)
    LD (black_king_sq), A
.s_nk:

    ; EP capture
    LD A, (IX+UNDO_MOVING)
    AND PIECE_MASK
    CP 1
    JP NZ, .s_nep
    LD A, (ep_square)
    CP EP_NONE
    JP Z, .s_nep
    LD B, A
    LD A, (move_to)
    CP B
    JP NZ, .s_nep
    LD A, (IX+UNDO_MOVING)
    AND COLOR_MASK
    JP NZ, .s_epb
    LD A, (move_to)
    SUB 16
    JP .s_epd
.s_epb:
    LD A, (move_to)
    ADD A, 16
.s_epd:
    LD (IX+UNDO_EP_SQ), A
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (IX+UNDO_EP_PIECE), A
    LD (HL), EMPTY
.s_nep:

    ; Castling rook
    LD A, (IX+UNDO_MOVING)
    AND PIECE_MASK
    CP 6
    JP NZ, .s_nc
    LD A, (move_from)
    LD B, A
    LD A, (move_to)
    SUB B
    CP 2
    JP Z, .s_ksc
    CP $FE
    JP Z, .s_qsc
    JP .s_nc
.s_ksc:
    LD A, 1
    LD (IX+UNDO_DID_CASTLE), A
    LD A, (move_from)
    ADD A, 3
    LD (IX+UNDO_ROOK_FROM), A
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (IX+UNDO_ROOK_PIECE), A
    LD (HL), EMPTY
    LD A, (move_from)
    ADD A, 1
    LD (IX+UNDO_ROOK_TO), A
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (IX+UNDO_ROOK_PIECE)
    LD (HL), A
    JP .s_nc
.s_qsc:
    LD A, 1
    LD (IX+UNDO_DID_CASTLE), A
    LD A, (move_from)
    SUB 4
    LD (IX+UNDO_ROOK_FROM), A
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (IX+UNDO_ROOK_PIECE), A
    LD (HL), EMPTY
    LD A, (move_from)
    SUB 1
    LD (IX+UNDO_ROOK_TO), A
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (IX+UNDO_ROOK_PIECE)
    LD (HL), A
.s_nc:

    ; Castling rights
    LD A, (IX+UNDO_MOVING)
    AND PIECE_MASK
    CP 6
    JP NZ, .s_cr
    LD A, (IX+UNDO_MOVING)
    AND COLOR_MASK
    JP NZ, .s_bcl
    LD A, (castling_rights)
    AND ~CASTLE_W_ALL & $FF
    LD (castling_rights), A
    JP .s_cr
.s_bcl:
    LD A, (castling_rights)
    AND ~CASTLE_B_ALL & $FF
    LD (castling_rights), A
.s_cr:
    LD A, (move_from)
    CP SQ_A1
    JP NZ, .s_r1
    LD A, (castling_rights)
    AND ~CASTLE_WQ & $FF
    LD (castling_rights), A
.s_r1:
    LD A, (move_from)
    CP SQ_H1
    JP NZ, .s_r2
    LD A, (castling_rights)
    AND ~CASTLE_WK & $FF
    LD (castling_rights), A
.s_r2:
    LD A, (move_from)
    CP SQ_A8
    JP NZ, .s_r3
    LD A, (castling_rights)
    AND ~CASTLE_BQ & $FF
    LD (castling_rights), A
.s_r3:
    LD A, (move_from)
    CP SQ_H8
    JP NZ, .s_r4
    LD A, (castling_rights)
    AND ~CASTLE_BK & $FF
    LD (castling_rights), A
.s_r4:

    ; EP square
    LD A, EP_NONE
    LD (ep_square), A
    LD A, (IX+UNDO_MOVING)
    AND PIECE_MASK
    CP 1
    JP NZ, .s_cp
    LD A, (move_from)
    LD B, A
    LD A, (move_to)
    SUB B
    CP 32
    JP Z, .s_ew
    CP $E0
    JP Z, .s_eb
    JP .s_cp
.s_ew:
    LD A, (move_from)
    ADD A, 16
    LD (ep_square), A
    JP .s_cp
.s_eb:
    LD A, (move_from)
    SUB 16
    LD (ep_square), A
.s_cp:

    ; Promotion
    LD A, (IX+UNDO_MOVING)
    AND PIECE_MASK
    CP 1
    JP NZ, .s_done
    LD A, (move_to)
    AND $70
    CP $70
    JP Z, .s_pw
    OR A
    JP NZ, .s_done
    LD A, 1
    LD (IX+UNDO_DID_PROMO), A
    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD (HL), B_QUEEN
    JP .s_done
.s_pw:
    LD A, 1
    LD (IX+UNDO_DID_PROMO), A
    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD (HL), W_QUEEN
.s_done:
    LD A, (undo_ptr)
    INC A
    LD (undo_ptr), A
    RET

; =============================================================================
; UNMAKE SEARCH MOVE
; =============================================================================
unmake_search_move:
    LD A, (undo_ptr)
    DEC A
    LD (undo_ptr), A
    CALL get_undo_ix

    LD A, (IX+UNDO_DID_PROMO)
    OR A
    JP Z, .u_np
    LD A, (IX+UNDO_TO)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (IX+UNDO_MOVING)
    LD (HL), A
.u_np:

    LD A, (IX+UNDO_DID_CASTLE)
    OR A
    JP Z, .u_nc
    LD A, (IX+UNDO_ROOK_TO)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD (HL), EMPTY
    LD A, (IX+UNDO_ROOK_FROM)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (IX+UNDO_ROOK_PIECE)
    LD (HL), A
.u_nc:

    LD A, (IX+UNDO_EP_SQ)
    OR A
    JP Z, .u_ne
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (IX+UNDO_EP_PIECE)
    LD (HL), A
.u_ne:

    LD A, (IX+UNDO_FROM)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (IX+UNDO_MOVING)
    LD (HL), A

    LD A, (IX+UNDO_TO)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (IX+UNDO_CAPTURED)
    LD (HL), A

    LD A, (IX+UNDO_EP)
    LD (ep_square), A
    LD A, (IX+UNDO_CASTLE)
    LD (castling_rights), A
    LD A, (IX+UNDO_WK)
    LD (white_king_sq), A
    LD A, (IX+UNDO_BK)
    LD (black_king_sq), A
    RET

; =============================================================================
; GET UNDO ENTRY IX POINTER
; =============================================================================
get_undo_ix:
    LD C, A
    LD B, 0
    LD L, A
    LD H, 0
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    OR A
    SBC HL, BC
    LD DE, undo_stack
    ADD HL, DE
    PUSH HL
    POP IX
    RET

; =============================================================================
; DATA
; =============================================================================
undo_ptr:       DEFB 0
undo_stack:     DEFS UNDO_ENTRY_SIZE * UNDO_MAX, 0
srch_moves_1:   DEFS MAX_MOVES * 2, 0
srch_count_1:   DEFB 0
srch_moves_2:   DEFS MAX_MOVES * 2, 0
srch_count_2:   DEFB 0