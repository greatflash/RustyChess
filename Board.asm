; =============================================================================
; RUSTY CHESS - Board State and Move Execution
; =============================================================================

; =============================================================================
; BOARD INITIALIZATION
; =============================================================================
init_board:
    LD HL, board
    LD DE, board + 1
    LD BC, BOARD_SIZE - 1
    LD (HL), EMPTY
    LDIR

    LD HL, board + $00
    LD (HL), W_ROOK
    INC HL
    LD (HL), W_KNIGHT
    INC HL
    LD (HL), W_BISHOP
    INC HL
    LD (HL), W_QUEEN
    INC HL
    LD (HL), W_KING
    INC HL
    LD (HL), W_BISHOP
    INC HL
    LD (HL), W_KNIGHT
    INC HL
    LD (HL), W_ROOK

    LD HL, board + $10
    LD B, 8
.wp:
    LD (HL), W_PAWN
    INC HL
    DJNZ .wp

    LD HL, board + $60
    LD B, 8
.bp:
    LD (HL), B_PAWN
    INC HL
    DJNZ .bp

    LD HL, board + $70
    LD (HL), B_ROOK
    INC HL
    LD (HL), B_KNIGHT
    INC HL
    LD (HL), B_BISHOP
    INC HL
    LD (HL), B_QUEEN
    INC HL
    LD (HL), B_KING
    INC HL
    LD (HL), B_BISHOP
    INC HL
    LD (HL), B_KNIGHT
    INC HL
    LD (HL), B_ROOK

    LD A, SQ_E1
    LD (white_king_sq), A
    LD A, SQ_E8
    LD (black_king_sq), A
    RET

; =============================================================================
; EXECUTE MOVE (WITH CHECK VALIDATION)
; =============================================================================
execute_move:
    ; --- Save state for undo ---
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

    ; Clear side effect flags
    XOR A
    LD (did_castle), A
    LD (did_ep_capture), A
    LD (did_promote), A
    LD (save_ep_captured), A

    ; --- Make the move ---
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

    ; --- Handle king move ---
    LD A, (moving_piece)
    AND PIECE_MASK
    CP 6
    JP NZ, .not_king

    LD A, (moving_piece)
    AND COLOR_MASK
    JP NZ, .moved_bking

    ; White king moved
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
    JP Z, .castle_wk
    CP $FE
    JP Z, .castle_wq
    JP .not_king

.castle_wk:
    LD A, 1
    LD (did_castle), A
    LD A, (board + SQ_H1)
    LD (save_castle_rook), A
    LD (board + SQ_F1), A
    LD A, EMPTY
    LD (board + SQ_H1), A
    JP .not_king

.castle_wq:
    LD A, 1
    LD (did_castle), A
    LD A, (board + SQ_A1)
    LD (save_castle_rook), A
    LD (board + SQ_D1), A
    LD A, EMPTY
    LD (board + SQ_A1), A
    JP .not_king

.moved_bking:
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
    JP Z, .castle_bk
    CP $FE
    JP Z, .castle_bq
    JP .not_king

.castle_bk:
    LD A, 1
    LD (did_castle), A
    LD A, (board + SQ_H8)
    LD (save_castle_rook), A
    LD (board + SQ_F8), A
    LD A, EMPTY
    LD (board + SQ_H8), A
    JP .not_king

.castle_bq:
    LD A, 1
    LD (did_castle), A
    LD A, (board + SQ_A8)
    LD (save_castle_rook), A
    LD (board + SQ_D8), A
    LD A, EMPTY
    LD (board + SQ_A8), A

.not_king:
    ; --- Handle rook move (remove castling rights) ---
    LD A, (moving_piece)
    AND PIECE_MASK
    CP 4
    JP NZ, .not_rook_move

    LD A, (move_from)
    CP SQ_A1
    JP NZ, .not_a1
    LD A, (castling_rights)
    AND ~CASTLE_WQ & $FF
    LD (castling_rights), A
.not_a1:
    LD A, (move_from)
    CP SQ_H1
    JP NZ, .not_h1
    LD A, (castling_rights)
    AND ~CASTLE_WK & $FF
    LD (castling_rights), A
.not_h1:
    LD A, (move_from)
    CP SQ_A8
    JP NZ, .not_a8
    LD A, (castling_rights)
    AND ~CASTLE_BQ & $FF
    LD (castling_rights), A
.not_a8:
    LD A, (move_from)
    CP SQ_H8
    JP NZ, .not_rook_move
    LD A, (castling_rights)
    AND ~CASTLE_BK & $FF
    LD (castling_rights), A

.not_rook_move:
    ; --- Handle rook capture (remove opponent castling rights) ---
    LD A, (move_to)
    CP SQ_A1
    JP NZ, .not_cap_a1
    LD A, (castling_rights)
    AND ~CASTLE_WQ & $FF
    LD (castling_rights), A
.not_cap_a1:
    LD A, (move_to)
    CP SQ_H1
    JP NZ, .not_cap_h1
    LD A, (castling_rights)
    AND ~CASTLE_WK & $FF
    LD (castling_rights), A
.not_cap_h1:
    LD A, (move_to)
    CP SQ_A8
    JP NZ, .not_cap_a8
    LD A, (castling_rights)
    AND ~CASTLE_BQ & $FF
    LD (castling_rights), A
.not_cap_a8:
    LD A, (move_to)
    CP SQ_H8
    JP NZ, .not_cap_h8
    LD A, (castling_rights)
    AND ~CASTLE_BK & $FF
    LD (castling_rights), A
.not_cap_h8:

    ; --- Handle en passant capture ---
    LD A, (moving_piece)
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

    LD A, 1
    LD (did_ep_capture), A

    LD A, (moving_piece)
    AND COLOR_MASK
    JP NZ, .ep_black

    LD A, (move_to)
    SUB 16
    JP .ep_remove

.ep_black:
    LD A, (move_to)
    ADD A, 16

.ep_remove:
    LD (ep_capture_sq), A
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (save_ep_captured), A
    LD (HL), EMPTY

.not_ep:
    ; --- Update en passant square ---
    LD A, EP_NONE
    LD (ep_square), A

    LD A, (moving_piece)
    AND PIECE_MASK
    CP 1
    JP NZ, .not_pawn_double

    LD A, (move_from)
    LD B, A
    LD A, (move_to)
    SUB B
    CP 32
    JP Z, .set_ep_white
    CP $E0
    JP Z, .set_ep_black
    JP .not_pawn_double

.set_ep_white:
    LD A, (move_from)
    ADD A, 16
    LD (ep_square), A
    JP .not_pawn_double

.set_ep_black:
    LD A, (move_from)
    SUB 16
    LD (ep_square), A

.not_pawn_double:

    ; --- Handle pawn promotion ---
    LD A, (moving_piece)
    AND PIECE_MASK
    CP 1
    JP NZ, .not_promotion

    LD A, (moving_piece)
    AND COLOR_MASK
    JP NZ, .check_black_promo

    LD A, (move_to)
    AND $F0
    CP $70
    JP NZ, .not_promotion

    LD A, 1
    LD (did_promote), A
    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, W_QUEEN
    LD (HL), A
    JP .not_promotion

.check_black_promo:
    LD A, (move_to)
    AND $F0
    CP $00
    JP NZ, .not_promotion

    LD A, 1
    LD (did_promote), A
    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, B_QUEEN
    LD (HL), A

.not_promotion:

    ; --- Check if our king is now in check ---
    LD A, (side_to_move)
    OR A
    JP NZ, .check_bk

    LD A, (white_king_sq)
    LD B, BLACK
    JP .do_check

.check_bk:
    LD A, (black_king_sq)
    LD B, WHITE

.do_check:
    CALL is_square_attacked

    JP Z, .move_ok

    ; --- ILLEGAL: undo everything ---
    CALL undo_move
    JP move_illegal

    ; --- Legal move ---
.move_ok:
    ; Record capture (if any)
    LD A, (save_to_piece)
    CALL record_capture

    ; Record EP capture too
    LD A, (did_ep_capture)
    OR A
    JR Z, .no_ep_record
    LD A, (save_ep_captured)
    CALL record_capture
.no_ep_record:

    ; Record move in history
    CALL record_move

    ; Switch side
    LD A, (side_to_move)
    XOR COLOR_MASK
    LD (side_to_move), A

    CALL check_game_end

    RET

; =============================================================================
; UNDO MOVE
; =============================================================================
undo_move:
    LD A, (move_from)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (save_from_piece)
    LD (HL), A

    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (save_to_piece)
    LD (HL), A

    LD A, (save_wking)
    LD (white_king_sq), A
    LD A, (save_bking)
    LD (black_king_sq), A

    LD A, (save_castling)
    LD (castling_rights), A

    LD A, (save_ep)
    LD (ep_square), A

    LD A, (did_castle)
    OR A
    JP Z, .no_castle_undo

    LD A, (move_to)
    CP SQ_G1
    JP Z, .undo_wk_castle
    CP SQ_C1
    JP Z, .undo_wq_castle
    CP SQ_G8
    JP Z, .undo_bk_castle
    CP SQ_C8
    JP Z, .undo_bq_castle
    JP .no_castle_undo

.undo_wk_castle:
    LD A, (save_castle_rook)
    LD (board + SQ_H1), A
    LD A, EMPTY
    LD (board + SQ_F1), A
    JP .no_castle_undo

.undo_wq_castle:
    LD A, (save_castle_rook)
    LD (board + SQ_A1), A
    LD A, EMPTY
    LD (board + SQ_D1), A
    JP .no_castle_undo

.undo_bk_castle:
    LD A, (save_castle_rook)
    LD (board + SQ_H8), A
    LD A, EMPTY
    LD (board + SQ_F8), A
    JP .no_castle_undo

.undo_bq_castle:
    LD A, (save_castle_rook)
    LD (board + SQ_A8), A
    LD A, EMPTY
    LD (board + SQ_D8), A

.no_castle_undo:
    LD A, (did_ep_capture)
    OR A
    JP Z, .no_ep_undo

    LD A, (ep_capture_sq)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (save_ep_captured)
    LD (HL), A

.no_ep_undo:
    RET

; Signal illegal move
move_illegal:
    LD A, 1
    LD (illegal_flash), A
    RET

; =============================================================================
; CHECK FOR CHECKMATE / STALEMATE
; =============================================================================
check_game_end:
    CALL has_any_legal_move
    RET NZ

    LD A, (side_to_move)
    OR A
    JP NZ, .check_bk

    LD A, (white_king_sq)
    LD B, BLACK
    JP .test

.check_bk:
    LD A, (black_king_sq)
    LD B, WHITE

.test:
    CALL is_square_attacked

    JP Z, .stalemate

    LD A, 1
    LD (game_over), A
    RET

.stalemate:
    LD A, 2
    LD (game_over), A
    RET