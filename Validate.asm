; =============================================================================
; RUSTY CHESS - Move Validation
; =============================================================================

; =============================================================================
; MAIN VALIDATION ENTRY POINT
; =============================================================================
try_make_move:
    ; Get moving piece
    LD A, (move_from)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (moving_piece), A

    OR A
    RET Z

    ; Get destination contents
    LD A, (move_to)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (dest_piece), A

    ; Can't capture own piece - but skip check for king (castling)
    OR A
    JP Z, .check_piece

    LD A, (moving_piece)
    AND PIECE_MASK
    CP 6
    JP Z, .check_piece              ; King handles own-piece in validator

    LD A, (dest_piece)
    AND COLOR_MASK
    LD B, A
    LD A, (moving_piece)
    AND COLOR_MASK
    CP B
    JP NZ, .check_piece

    LD A, 1
    LD (illegal_flash), A
    RET

.check_piece:
    ; Can't move to same square
    LD A, (move_from)
    LD B, A
    LD A, (move_to)
    CP B
    RET Z

    ; Dispatch to validator
    LD A, (moving_piece)
    AND PIECE_MASK

    CP 1
    JP Z, validate_pawn
    CP 2
    JP Z, validate_knight
    CP 3
    JP Z, validate_bishop
    CP 4
    JP Z, validate_rook
    CP 5
    JP Z, validate_queen
    CP 6
    JP Z, validate_king
    RET

; =============================================================================
; PAWN VALIDATION
; =============================================================================
validate_pawn:
    LD A, (moving_piece)
    AND COLOR_MASK
    JP NZ, .black

    ; --- White pawn ---
    LD A, (move_from)
    LD B, A
    LD A, (move_to)
    LD C, A
    LD A, C
    SUB B
    LD D, A

    CP 16
    JP NZ, .w_not_single
    LD A, (dest_piece)
    OR A
    JP NZ, move_illegal
    JP execute_move

.w_not_single:
    CP 32
    JP NZ, .w_not_double
    LD A, B
    AND $F0
    CP $10
    JP NZ, move_illegal
    LD A, (dest_piece)
    OR A
    JP NZ, move_illegal
    LD A, B
    ADD A, 16
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    OR A
    JP NZ, move_illegal
    JP execute_move

.w_not_double:
    LD A, D
    CP 15
    JP Z, .w_capture
    CP 17
    JP Z, .w_capture
    JP move_illegal

.w_capture:
    LD A, (dest_piece)
    OR A
    JP NZ, execute_move
    LD A, (ep_square)
    CP EP_NONE
    JP Z, move_illegal
    LD B, A
    LD A, (move_to)
    CP B
    JP NZ, move_illegal
    JP execute_move

    ; --- Black pawn ---
.black:
    LD A, (move_from)
    LD B, A
    LD A, (move_to)
    LD C, A
    LD A, B
    SUB C
    LD D, A

    CP 16
    JP NZ, .b_not_single
    LD A, (dest_piece)
    OR A
    JP NZ, move_illegal
    JP execute_move

.b_not_single:
    CP 32
    JP NZ, .b_not_double
    LD A, B
    AND $F0
    CP $60
    JP NZ, move_illegal
    LD A, (dest_piece)
    OR A
    JP NZ, move_illegal
    LD A, B
    SUB 16
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    OR A
    JP NZ, move_illegal
    JP execute_move

.b_not_double:
    LD A, D
    CP 15
    JP Z, .b_capture
    CP 17
    JP Z, .b_capture
    JP move_illegal

.b_capture:
    LD A, (dest_piece)
    OR A
    JP NZ, execute_move
    LD A, (ep_square)
    CP EP_NONE
    JP Z, move_illegal
    LD B, A
    LD A, (move_to)
    CP B
    JP NZ, move_illegal
    JP execute_move

; =============================================================================
; KNIGHT VALIDATION
; =============================================================================
validate_knight:
    LD A, (move_from)
    LD B, A
    LD A, (move_to)
    SUB B
    LD D, A

    LD HL, knight_diffs
    LD B, 8
.loop:
    LD A, (HL)
    CP D
    JP Z, execute_move
    INC HL
    DJNZ .loop
    JP move_illegal

knight_diffs:
    DEFB 14, 18, 31, 33
    DEFB $F2, $EE, $E1, $DF

; =============================================================================
; BISHOP VALIDATION
; =============================================================================
validate_bishop:
    CALL calc_direction
    CP 15
    JP Z, .valid
    CP 17
    JP Z, .valid
    CP $F1
    JP Z, .valid
    CP $EF
    JP Z, .valid
    JP move_illegal
.valid:
    CALL check_path_clear
    JP C, move_illegal
    JP execute_move

; =============================================================================
; ROOK VALIDATION
; =============================================================================
validate_rook:
    CALL calc_direction
    CP 1
    JP Z, .valid
    CP 16
    JP Z, .valid
    CP $FF
    JP Z, .valid
    CP $F0
    JP Z, .valid
    JP move_illegal
.valid:
    CALL check_path_clear
    JP C, move_illegal
    JP execute_move

; =============================================================================
; QUEEN VALIDATION
; =============================================================================
validate_queen:
    CALL calc_direction
    CP 1
    JP Z, .valid
    CP 15
    JP Z, .valid
    CP 16
    JP Z, .valid
    CP 17
    JP Z, .valid
    CP $FF
    JP Z, .valid
    CP $F1
    JP Z, .valid
    CP $F0
    JP Z, .valid
    CP $EF
    JP Z, .valid
    JP move_illegal
.valid:
    CALL check_path_clear
    JP C, move_illegal
    JP execute_move

; =============================================================================
; KING VALIDATION (WITH CASTLING)
; =============================================================================
validate_king:
    ; --- Check for castling first by explicit square pairs ---

    ; White kingside: e1 -> g1
    LD A, (move_from)
    CP SQ_E1
    JP NZ, .not_wk_castle
    LD A, (move_to)
    CP SQ_G1
    JP Z, .do_white_kingside
.not_wk_castle:

    ; White queenside: e1 -> c1
    LD A, (move_from)
    CP SQ_E1
    JP NZ, .not_wq_castle
    LD A, (move_to)
    CP SQ_C1
    JP Z, .do_white_queenside
.not_wq_castle:

    ; Black kingside: e8 -> g8
    LD A, (move_from)
    CP SQ_E8
    JP NZ, .not_bk_castle
    LD A, (move_to)
    CP SQ_G8
    JP Z, .do_black_kingside
.not_bk_castle:

    ; Black queenside: e8 -> c8
    LD A, (move_from)
    CP SQ_E8
    JP NZ, .not_bq_castle
    LD A, (move_to)
    CP SQ_C8
    JP Z, .do_black_queenside
.not_bq_castle:

    ; --- Normal king move (one square any direction) ---
    LD A, (move_from)
    LD B, A
    LD A, (move_to)
    SUB B
    LD D, A

    LD HL, king_diffs
    LD B, 8
.loop:
    LD A, (HL)
    CP D
    JP Z, .normal_move
    INC HL
    DJNZ .loop

    JP move_illegal

.normal_move:
    LD A, (dest_piece)
    OR A
    JP Z, execute_move
    AND COLOR_MASK
    LD B, A
    LD A, (moving_piece)
    AND COLOR_MASK
    CP B
    JP Z, move_illegal
    JP execute_move

; --- White kingside castling ---
.do_white_kingside:
    LD A, (moving_piece)
    CP W_KING
    JP NZ, move_illegal

    LD A, (castling_rights)
    AND CASTLE_WK
    JP Z, move_illegal

    LD A, (board + SQ_F1)
    OR A
    JP NZ, move_illegal
    LD A, (board + SQ_G1)
    OR A
    JP NZ, move_illegal

    LD A, SQ_E1
    LD B, BLACK
    CALL is_square_attacked
    JP NZ, move_illegal

    LD A, SQ_F1
    LD B, BLACK
    CALL is_square_attacked
    JP NZ, move_illegal

    JP execute_move

; --- White queenside castling ---
.do_white_queenside:
    LD A, (moving_piece)
    CP W_KING
    JP NZ, move_illegal

    LD A, (castling_rights)
    AND CASTLE_WQ
    JP Z, move_illegal

    LD A, (board + SQ_B1)
    OR A
    JP NZ, move_illegal
    LD A, (board + SQ_C1)
    OR A
    JP NZ, move_illegal
    LD A, (board + SQ_D1)
    OR A
    JP NZ, move_illegal

    LD A, SQ_E1
    LD B, BLACK
    CALL is_square_attacked
    JP NZ, move_illegal

    LD A, SQ_D1
    LD B, BLACK
    CALL is_square_attacked
    JP NZ, move_illegal

    JP execute_move

; --- Black kingside castling ---
.do_black_kingside:
    LD A, (moving_piece)
    CP B_KING
    JP NZ, move_illegal

    LD A, (castling_rights)
    AND CASTLE_BK
    JP Z, move_illegal

    LD A, (board + SQ_F8)
    OR A
    JP NZ, move_illegal
    LD A, (board + SQ_G8)
    OR A
    JP NZ, move_illegal

    LD A, SQ_E8
    LD B, WHITE
    CALL is_square_attacked
    JP NZ, move_illegal

    LD A, SQ_F8
    LD B, WHITE
    CALL is_square_attacked
    JP NZ, move_illegal

    JP execute_move

; --- Black queenside castling ---
.do_black_queenside:
    LD A, (moving_piece)
    CP B_KING
    JP NZ, move_illegal

    LD A, (castling_rights)
    AND CASTLE_BQ
    JP Z, move_illegal

    LD A, (board + SQ_B8)
    OR A
    JP NZ, move_illegal
    LD A, (board + SQ_C8)
    OR A
    JP NZ, move_illegal
    LD A, (board + SQ_D8)
    OR A
    JP NZ, move_illegal

    LD A, SQ_E8
    LD B, WHITE
    CALL is_square_attacked
    JP NZ, move_illegal

    LD A, SQ_D8
    LD B, WHITE
    CALL is_square_attacked
    JP NZ, move_illegal

    JP execute_move

king_diffs:
    DEFB 1, 15, 16, 17
    DEFB $FF, $F1, $F0, $EF

; =============================================================================
; DIRECTION CALCULATOR
; =============================================================================
calc_direction:
    LD A, (move_from)
    LD B, A
    LD A, (move_to)
    LD C, A

    LD A, B
    AND $07
    LD D, A
    LD A, B
    RRCA
    RRCA
    RRCA
    RRCA
    AND $07
    LD E, A

    LD A, C
    AND $07
    LD H, A
    LD A, C
    RRCA
    RRCA
    RRCA
    RRCA
    AND $07
    LD L, A

    LD A, H
    SUB D
    LD (file_diff), A
    LD A, L
    SUB E
    LD (rank_diff), A

    LD A, (file_diff)
    OR A
    JP NZ, .not_vert
    LD A, (rank_diff)
    BIT 7, A
    JP NZ, .vert_down
    LD A, 16
    RET
.vert_down:
    LD A, $F0
    RET

.not_vert:
    LD A, (rank_diff)
    OR A
    JP NZ, .not_horiz
    LD A, (file_diff)
    BIT 7, A
    JP NZ, .horiz_left
    LD A, 1
    RET
.horiz_left:
    LD A, $FF
    RET

.not_horiz:
    LD A, (file_diff)
    BIT 7, A
    JR Z, .fd_pos
    NEG
.fd_pos:
    LD B, A
    LD A, (rank_diff)
    BIT 7, A
    JR Z, .rd_pos
    NEG
.rd_pos:
    CP B
    JP NZ, .not_diag

    LD A, (rank_diff)
    BIT 7, A
    JP NZ, .diag_down
    LD A, (file_diff)
    BIT 7, A
    JP NZ, .up_left
    LD A, 17
    RET
.up_left:
    LD A, 15
    RET
.diag_down:
    LD A, (file_diff)
    BIT 7, A
    JP NZ, .down_left
    LD A, $F1
    RET
.down_left:
    LD A, $EF
    RET

.not_diag:
    XOR A
    RET

file_diff:  DEFB 0
rank_diff:  DEFB 0

; =============================================================================
; PATH CLEAR CHECK
; =============================================================================
check_path_clear:
    LD (path_dir), A

    LD A, (move_from)
    LD B, A
    LD A, (move_to)
    LD C, A

.step:
    LD A, (path_dir)
    LD E, A
    LD A, B
    ADD A, E

    CP C
    JR Z, .clear

    LD D, A
    AND OFF_BOARD
    JR NZ, .blocked

    LD A, D
    LD B, A
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    OR A
    JR NZ, .blocked

    JR .step

.clear:
    OR A
    RET
.blocked:
    SCF
    RET

path_dir:   DEFB 0