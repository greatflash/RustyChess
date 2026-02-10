; =============================================================================
; RUSTY CHESS - AI Search (Negamax with Alpha-Beta)
; =============================================================================

SEARCH_DEPTH    EQU 2
SCORE_INF       EQU 30000
SCORE_MATE      EQU 20000

; Undo stack entry: 15 bytes
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

    ; Child: alpha = -INF, beta = -parent_alpha
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
    ; Execute best move through game logic
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

    ; Set up for execute_move
    LD A, (move_from)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (moving_piece), A

    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (dest_piece), A

    CALL execute_move
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
; Input: search_depth, search_alpha, search_beta
; Output: HL = score from current side's perspective
; =============================================================================
negamax:
    ; Leaf node?
    LD A, (search_depth)
    OR A
    JP Z, evaluate

    ; Generate moves
    CALL generate_moves

    LD A, (move_list_count)
    OR A
    JP Z, .no_moves

    ; Copy move list to depth-specific buffer
    ; At depth 2 we use srch_moves_1, at depth 1 we use srch_moves_2
    ; (depth counts down, so higher depth = earlier in tree)
    LD A, (search_depth)
    CP 2
    JP Z, .copy_d2
    ; Depth 1
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
    ; Best = -infinity
    LD HL, -SCORE_INF
    LD (node_best), HL
    XOR A
    LD (node_cur_idx), A

.move_loop:
    ; Get move count for current depth
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

    ; Get move from correct buffer
    LD A, (search_depth)
    CP 2
    JP Z, .mov_d2
    ; Depth 1 buffer
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

    ; Save parent state to Z80 stack
    ; This is safe because recursion is only 1 level deep (depth 2->1->eval)
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

    ; Set up child: alpha = -beta, beta = -alpha, depth = depth-1
    LD HL, (search_beta)
    LD A, H
    CPL
    LD H, A
    LD A, L
    CPL
    LD L, A
    INC HL
    PUSH HL                         ; Save child alpha

    LD HL, (search_alpha)
    LD A, H
    CPL
    LD H, A
    LD A, L
    CPL
    LD L, A
    INC HL
    LD (search_beta), HL            ; Child beta = -parent alpha

    POP HL
    LD (search_alpha), HL           ; Child alpha = -parent beta

    LD A, (search_depth)
    DEC A
    LD (search_depth), A

    ; Recurse
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

    ; Restore parent state from Z80 stack
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

    ; Switch side back
    LD A, (side_to_move)
    XOR COLOR_MASK
    LD (side_to_move), A

    ; Unmake
    CALL unmake_search_move

    ; Score > best?
    LD HL, (node_child_score)
    LD DE, (node_best)
    CALL signed_compare_hl_de
    JP C, .no_improve
    JP Z, .no_improve

    ; New best
    LD HL, (node_child_score)
    LD (node_best), HL

    ; Update alpha?
    LD DE, (search_alpha)
    CALL signed_compare_hl_de
    JP C, .check_cutoff
    JP Z, .check_cutoff
    LD (search_alpha), HL

.check_cutoff:
    ; Alpha >= beta?
    LD HL, (search_alpha)
    LD DE, (search_beta)
    CALL signed_compare_hl_de
    JP C, .no_improve

    ; Cutoff
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
    ; Checkmate or stalemate?
    LD A, (side_to_move)
    OR A
    JP NZ, .no_moves_black
    LD A, (white_king_sq)
    LD B, BLACK
    JP .no_moves_check
.no_moves_black:
    LD A, (black_king_sq)
    LD B, WHITE
.no_moves_check:
    CALL is_square_attacked
    JP Z, .is_stalemate
    LD HL, -SCORE_MATE
    RET
.is_stalemate:
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
; HL vs DE (signed)
; Carry set if HL < DE, Zero if HL == DE, neither if HL > DE
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
    RET Z                           ; HL positive, DE negative: HL > DE
    SCF                             ; HL negative, DE positive: HL < DE
    RET

; =============================================================================
; MAKE SEARCH MOVE (incremental with undo stack)
; =============================================================================
make_search_move:
    LD A, (undo_ptr)
    CALL get_undo_ix

    ; Save state
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

    ; Get moving piece
    LD A, (move_from)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (IX+UNDO_MOVING), A

    ; Get captured piece
    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (IX+UNDO_CAPTURED), A

    ; Clear source
    LD A, (move_from)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD (HL), EMPTY

    ; Place on destination
    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (IX+UNDO_MOVING)
    LD (HL), A

    ; Update king position
    LD A, (IX+UNDO_MOVING)
    AND PIECE_MASK
    CP 6
    JP NZ, .not_king
    LD A, (IX+UNDO_MOVING)
    AND COLOR_MASK
    JP NZ, .bk_moved
    LD A, (move_to)
    LD (white_king_sq), A
    JP .not_king
.bk_moved:
    LD A, (move_to)
    LD (black_king_sq), A

.not_king:
    ; --- EP capture ---
    LD A, (IX+UNDO_MOVING)
    AND PIECE_MASK
    CP 1
    JP NZ, .not_ep
    LD A, (ep_square)
    CP EP_NONE
    JP Z, .not_ep
    LD B, A
    LD A, (move_to)
    CP B
    JP NZ, .not_ep
    LD A, (IX+UNDO_MOVING)
    AND COLOR_MASK
    JP NZ, .ep_black
    LD A, (move_to)
    SUB 16
    JP .ep_do
.ep_black:
    LD A, (move_to)
    ADD A, 16
.ep_do:
    LD (IX+UNDO_EP_SQ), A
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (IX+UNDO_EP_PIECE), A
    LD (HL), EMPTY

.not_ep:
    ; --- Castling rook ---
    LD A, (IX+UNDO_MOVING)
    AND PIECE_MASK
    CP 6
    JP NZ, .not_castle
    LD A, (move_from)
    LD B, A
    LD A, (move_to)
    SUB B
    CP 2
    JP Z, .ks_castle
    CP $FE
    JP Z, .qs_castle
    JP .not_castle

.ks_castle:
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
    JP .not_castle

.qs_castle:
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

.not_castle:
    ; --- Update castling rights ---
    LD A, (IX+UNDO_MOVING)
    AND PIECE_MASK
    CP 6
    JP NZ, .chk_rook
    LD A, (IX+UNDO_MOVING)
    AND COLOR_MASK
    JP NZ, .bk_c_loss
    LD A, (castling_rights)
    AND ~CASTLE_W_ALL & $FF
    LD (castling_rights), A
    JP .chk_rook
.bk_c_loss:
    LD A, (castling_rights)
    AND ~CASTLE_B_ALL & $FF
    LD (castling_rights), A

.chk_rook:
    LD A, (move_from)
    CP SQ_A1
    JP NZ, .nA1
    LD A, (castling_rights)
    AND ~CASTLE_WQ & $FF
    LD (castling_rights), A
.nA1:
    LD A, (move_from)
    CP SQ_H1
    JP NZ, .nH1
    LD A, (castling_rights)
    AND ~CASTLE_WK & $FF
    LD (castling_rights), A
.nH1:
    LD A, (move_from)
    CP SQ_A8
    JP NZ, .nA8
    LD A, (castling_rights)
    AND ~CASTLE_BQ & $FF
    LD (castling_rights), A
.nA8:
    LD A, (move_from)
    CP SQ_H8
    JP NZ, .nH8
    LD A, (castling_rights)
    AND ~CASTLE_BK & $FF
    LD (castling_rights), A
.nH8:

    ; --- Update EP square ---
    LD A, EP_NONE
    LD (ep_square), A
    LD A, (IX+UNDO_MOVING)
    AND PIECE_MASK
    CP 1
    JP NZ, .chk_promo
    LD A, (move_from)
    LD B, A
    LD A, (move_to)
    SUB B
    CP 32
    JP Z, .sep_w
    CP $E0
    JP Z, .sep_b
    JP .chk_promo
.sep_w:
    LD A, (move_from)
    ADD A, 16
    LD (ep_square), A
    JP .chk_promo
.sep_b:
    LD A, (move_from)
    SUB 16
    LD (ep_square), A

.chk_promo:
    ; --- Promotion (auto-queen) ---
    LD A, (IX+UNDO_MOVING)
    AND PIECE_MASK
    CP 1
    JP NZ, .mk_done
    LD A, (move_to)
    AND $70
    CP $70
    JP Z, .promo_w
    OR A
    JP NZ, .mk_done
    ; Black pawn rank 1
    LD A, 1
    LD (IX+UNDO_DID_PROMO), A
    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD (HL), B_QUEEN
    JP .mk_done
.promo_w:
    LD A, 1
    LD (IX+UNDO_DID_PROMO), A
    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD (HL), W_QUEEN

.mk_done:
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

    ; Undo promotion
    LD A, (IX+UNDO_DID_PROMO)
    OR A
    JP Z, .no_up
    LD A, (IX+UNDO_TO)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (IX+UNDO_MOVING)
    LD (HL), A
.no_up:

    ; Undo castling rook
    LD A, (IX+UNDO_DID_CASTLE)
    OR A
    JP Z, .no_uc
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
.no_uc:

    ; Undo EP capture
    LD A, (IX+UNDO_EP_SQ)
    OR A
    JP Z, .no_ue
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (IX+UNDO_EP_PIECE)
    LD (HL), A
.no_ue:

    ; Restore pieces
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

    ; Restore state
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
; Input: A = index
; Output: IX = undo_stack + A * 15
; =============================================================================
get_undo_ix:
    LD C, A
    LD B, 0
    LD L, A
    LD H, 0
    ADD HL, HL                      ; *2
    ADD HL, HL                      ; *4
    ADD HL, HL                      ; *8
    ADD HL, HL                      ; *16
    OR A
    SBC HL, BC                      ; *15
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