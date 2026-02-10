; =============================================================================
; RUSTY CHESS - Move Generation
; =============================================================================
; Generates all legal moves for the side to move.
; Used by AI search and for checkmate/stalemate detection.
; =============================================================================

; =============================================================================
; MOVE LIST BUFFER
; =============================================================================
; Each move: 2 bytes (from_sq, to_sq)
MAX_MOVES       EQU 128
move_list:      DEFS MAX_MOVES * 2, 0
move_list_count: DEFB 0

; =============================================================================
; GENERATE ALL LEGAL MOVES
; =============================================================================
; Output: move_list filled, move_list_count set
;         Returns count in A
; =============================================================================
generate_moves:
    XOR A
    LD (move_list_count), A

    ; Scan all 64 squares for our pieces
    LD A, 0
    LD (gen_from_sq), A

.scan_loop:
    LD A, (gen_from_sq)

    ; Check off-board (0x88 test)
    LD B, A
    AND OFF_BOARD
    JP NZ, .next_sq

    ; Get piece on this square
    LD A, B
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)

    ; Empty?
    OR A
    JP Z, .next_sq

    ; Our piece?
    LD C, A                         ; C = piece
    AND COLOR_MASK
    LD B, A
    LD A, (side_to_move)
    CP B
    JP NZ, .next_sq

    ; It's our piece - generate moves for it
    LD A, C                         ; A = piece
    AND PIECE_MASK
    CP 1
    JP Z, gen_pawn_moves
    CP 2
    JP Z, gen_knight_moves
    CP 3
    JP Z, gen_bishop_moves
    CP 4
    JP Z, gen_rook_moves
    CP 5
    JP Z, gen_queen_moves
    CP 6
    JP Z, gen_king_moves

.next_sq:
    LD A, (gen_from_sq)
    INC A
    LD (gen_from_sq), A
    CP $78                          ; Past last valid square
    JP C, .scan_loop

    LD A, (move_list_count)
    RET

gen_from_sq:    DEFB 0

; =============================================================================
; ADD MOVE TO LIST (with legality check via make/unmake)
; =============================================================================
; Input: gen_from_sq = from, A = to square
; Tests legality by making the move, checking for check, then unmaking.
; =============================================================================
try_add_move:
    LD (gen_to_sq), A

    ; --- Get pieces involved ---
    LD A, (gen_from_sq)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (gen_moving_piece), A

    LD A, (gen_to_sq)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (gen_captured_piece), A

    ; Can't capture own piece
    OR A
    JP Z, .do_make

    AND COLOR_MASK
    LD B, A
    LD A, (gen_moving_piece)
    AND COLOR_MASK
    CP B
    JP Z, .skip_move                ; Same color, skip

.do_make:
    ; --- Save state for unmake ---
    LD A, (white_king_sq)
    LD (gen_save_wk), A
    LD A, (black_king_sq)
    LD (gen_save_bk), A

    ; --- Make the move ---
    ; Clear source square
    LD A, (gen_from_sq)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD (HL), EMPTY

    ; Place piece on destination
    LD A, (gen_to_sq)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (gen_moving_piece)
    LD (HL), A

    ; Update king position if king moved
    LD A, (gen_moving_piece)
    AND PIECE_MASK
    CP 6
    JP NZ, .not_king_move

    LD A, (gen_moving_piece)
    AND COLOR_MASK
    JP NZ, .bk_move
    LD A, (gen_to_sq)
    LD (white_king_sq), A
    JP .not_king_move
.bk_move:
    LD A, (gen_to_sq)
    LD (black_king_sq), A

.not_king_move:
    ; Handle en passant capture removal
    XOR A
    LD (gen_ep_removed_sq), A       ; 0 = no EP removal

    LD A, (gen_moving_piece)
    AND PIECE_MASK
    CP 1
    JP NZ, .check_legal

    LD A, (ep_square)
    CP EP_NONE
    JP Z, .check_legal

    LD B, A
    LD A, (gen_to_sq)
    CP B
    JP NZ, .check_legal

    ; EP capture - find and remove captured pawn
    LD A, (gen_moving_piece)
    AND COLOR_MASK
    JP NZ, .ep_black_gen
    LD A, (gen_to_sq)
    SUB 16
    JP .ep_remove_gen
.ep_black_gen:
    LD A, (gen_to_sq)
    ADD A, 16
.ep_remove_gen:
    LD (gen_ep_removed_sq), A
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (gen_ep_removed_piece), A
    LD (HL), EMPTY

.check_legal:
    ; Is our king in check after this move?
    LD A, (side_to_move)
    OR A
    JP NZ, .check_bk_gen

    LD A, (white_king_sq)
    LD B, BLACK
    JP .do_check_gen

.check_bk_gen:
    LD A, (black_king_sq)
    LD B, WHITE

.do_check_gen:
    CALL is_square_attacked

    ; Save result
    PUSH AF

    ; --- Unmake the move ---
    ; Restore source square
    LD A, (gen_from_sq)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (gen_moving_piece)
    LD (HL), A

    ; Restore destination square
    LD A, (gen_to_sq)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (gen_captured_piece)
    LD (HL), A

    ; Restore EP captured pawn if any
    LD A, (gen_ep_removed_sq)
    OR A
    JP Z, .no_ep_restore
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (gen_ep_removed_piece)
    LD (HL), A
.no_ep_restore:

    ; Restore king positions
    LD A, (gen_save_wk)
    LD (white_king_sq), A
    LD A, (gen_save_bk)
    LD (black_king_sq), A

    ; Check result
    POP AF
    JP NZ, .skip_move               ; King was in check = illegal

    ; --- Legal move - add to list ---
    LD A, (move_list_count)
    CP MAX_MOVES
    JP NC, .skip_move               ; Buffer full safety check

    LD L, A
    LD H, 0
    ADD HL, HL
    LD DE, move_list
    ADD HL, DE

    LD A, (gen_from_sq)
    LD (HL), A
    INC HL
    LD A, (gen_to_sq)
    LD (HL), A

    LD A, (move_list_count)
    INC A
    LD (move_list_count), A

.skip_move:
    RET

gen_to_sq:              DEFB 0
gen_moving_piece:       DEFB 0
gen_captured_piece:     DEFB 0
gen_ep_removed_sq:      DEFB 0
gen_ep_removed_piece:   DEFB 0
gen_save_wk:            DEFB 0
gen_save_bk:            DEFB 0

; =============================================================================
; PAWN MOVE GENERATION
; =============================================================================
gen_pawn_moves:
    LD A, (side_to_move)
    OR A
    JP NZ, .gen_black_pawns

    ; --- White pawn moves ---
    ; Single push
    LD A, (gen_from_sq)
    ADD A, 16
    LD B, A
    AND OFF_BOARD
    JP NZ, generate_moves.next_sq
    LD A, B
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    OR A
    JP NZ, .w_no_single
    LD A, B
    CALL try_add_move

    ; Double push from rank 2
    LD A, (gen_from_sq)
    AND $F0
    CP $10
    JP NZ, .w_no_double
    LD A, (gen_from_sq)
    ADD A, 32
    LD B, A
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    OR A
    JP NZ, .w_no_double
    LD A, B
    CALL try_add_move
.w_no_double:
.w_no_single:

    ; Captures (left: +15, right: +17)
    LD A, (gen_from_sq)
    ADD A, 15
    CALL .try_w_capture
    LD A, (gen_from_sq)
    ADD A, 17
    CALL .try_w_capture

    JP generate_moves.next_sq

.try_w_capture:
    LD B, A
    AND OFF_BOARD
    RET NZ
    ; Check for enemy piece or EP square
    LD A, B
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    OR A
    JP NZ, .w_has_target
    ; Check EP
    LD A, (ep_square)
    CP B
    RET NZ
.w_has_target:
    ; Verify it's enemy or EP
    LD A, (HL)
    OR A
    JP Z, .w_ep_ok                   ; Empty = must be EP (already checked)
    AND COLOR_MASK
    CP BLACK
    RET NZ                          ; Not enemy
.w_ep_ok:
    LD A, B
    CALL try_add_move
    RET

    ; --- Black pawn moves ---
.gen_black_pawns:
    ; Single push
    LD A, (gen_from_sq)
    SUB 16
    LD B, A
    AND OFF_BOARD
    JP NZ, generate_moves.next_sq
    LD A, B
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    OR A
    JP NZ, .b_no_single
    LD A, B
    CALL try_add_move

    ; Double push from rank 7
    LD A, (gen_from_sq)
    AND $F0
    CP $60
    JP NZ, .b_no_double
    LD A, (gen_from_sq)
    SUB 32
    LD B, A
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    OR A
    JP NZ, .b_no_double
    LD A, B
    CALL try_add_move
.b_no_double:
.b_no_single:

    ; Captures (left: -15, right: -17)
    LD A, (gen_from_sq)
    SUB 15
    CALL .try_b_capture
    LD A, (gen_from_sq)
    SUB 17
    CALL .try_b_capture

    JP generate_moves.next_sq

.try_b_capture:
    LD B, A
    AND OFF_BOARD
    RET NZ
    LD A, B
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    OR A
    JP NZ, .b_has_target
    LD A, (ep_square)
    CP B
    RET NZ
.b_has_target:
    LD A, (HL)
    OR A
    JP Z, .b_ep_ok
    AND COLOR_MASK
    CP WHITE
    RET NZ
.b_ep_ok:
    LD A, B
    CALL try_add_move
    RET

; =============================================================================
; KNIGHT MOVE GENERATION
; =============================================================================
gen_knight_moves:
    LD HL, knight_offsets
    LD B, 8
.loop:
    PUSH BC
    PUSH HL
    LD A, (gen_from_sq)
    ADD A, (HL)
    LD B, A
    AND OFF_BOARD
    JP NZ, .skip
    LD A, B
    CALL try_add_move
.skip:
    POP HL
    POP BC
    INC HL
    DJNZ .loop
    JP generate_moves.next_sq

knight_offsets:
    DEFB 14, 18, 31, 33
    DEFB $F2, $EE, $E1, $DF

; =============================================================================
; SLIDING PIECE GENERATION (Bishop, Rook, Queen)
; =============================================================================
gen_bishop_moves:
    LD HL, diag_offsets
    LD B, 4
    JP gen_sliding

gen_rook_moves:
    LD HL, straight_offsets
    LD B, 4
    JP gen_sliding

gen_queen_moves:
    LD HL, all_offsets
    LD B, 8

gen_sliding:
    LD (gen_slide_ptr), HL

.dir_loop:
    PUSH BC
    LD HL, (gen_slide_ptr)
    LD A, (HL)
    LD (gen_slide_dir), A
    INC HL
    LD (gen_slide_ptr), HL

    LD A, (gen_from_sq)

.step:
    LD B, A
    LD A, (gen_slide_dir)
    ADD A, B
    LD B, A
    AND OFF_BOARD
    JP NZ, .next_dir

    ; Check if square occupied
    LD A, B
    LD (gen_slide_to), A
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    OR A
    JP Z, .empty_sq

    ; Occupied - can capture if enemy, then stop
    AND COLOR_MASK
    LD C, A
    LD A, (side_to_move)
    CP C
    JP Z, .next_dir                  ; Own piece, blocked

    LD A, (gen_slide_to)
    CALL try_add_move
    JP .next_dir

.empty_sq:
    LD A, (gen_slide_to)
    CALL try_add_move
    LD A, (gen_slide_to)
    JP .step

.next_dir:
    POP BC
    DJNZ .dir_loop
    JP generate_moves.next_sq

gen_slide_ptr:  DEFW 0
gen_slide_dir:  DEFB 0
gen_slide_to:   DEFB 0

diag_offsets:
    DEFB 15, 17, $F1, $EF

straight_offsets:
    DEFB 1, 16, $FF, $F0

all_offsets:
    DEFB 1, 15, 16, 17, $FF, $F1, $F0, $EF

; =============================================================================
; KING MOVE GENERATION (including castling)
; =============================================================================
gen_king_moves:
    ; Normal king moves (8 directions)
    LD HL, all_offsets
    LD B, 8
.loop:
    PUSH BC
    PUSH HL
    LD A, (gen_from_sq)
    ADD A, (HL)
    LD B, A
    AND OFF_BOARD
    JP NZ, .skip
    LD A, B
    CALL try_add_move
.skip:
    POP HL
    POP BC
    INC HL
    DJNZ .loop

    ; --- Castling ---
    LD A, (side_to_move)
    OR A
    JP NZ, .black_castle

    ; White kingside
    LD A, (castling_rights)
    AND CASTLE_WK
    JP Z, .no_wk
    LD A, (board + SQ_F1)
    OR A
    JP NZ, .no_wk
    LD A, (board + SQ_G1)
    OR A
    JP NZ, .no_wk
    LD A, SQ_E1
    LD B, BLACK
    CALL is_square_attacked
    JP NZ, .no_wk
    LD A, SQ_F1
    LD B, BLACK
    CALL is_square_attacked
    JP NZ, .no_wk
    LD A, SQ_G1
    CALL try_add_move
.no_wk:

    ; White queenside
    LD A, (castling_rights)
    AND CASTLE_WQ
    JP Z, .no_wq
    LD A, (board + SQ_B1)
    OR A
    JP NZ, .no_wq
    LD A, (board + SQ_C1)
    OR A
    JP NZ, .no_wq
    LD A, (board + SQ_D1)
    OR A
    JP NZ, .no_wq
    LD A, SQ_E1
    LD B, BLACK
    CALL is_square_attacked
    JP NZ, .no_wq
    LD A, SQ_D1
    LD B, BLACK
    CALL is_square_attacked
    JP NZ, .no_wq
    LD A, SQ_C1
    CALL try_add_move
.no_wq:
    JP generate_moves.next_sq

.black_castle:
    ; Black kingside
    LD A, (castling_rights)
    AND CASTLE_BK
    JP Z, .no_bk
    LD A, (board + SQ_F8)
    OR A
    JP NZ, .no_bk
    LD A, (board + SQ_G8)
    OR A
    JP NZ, .no_bk
    LD A, SQ_E8
    LD B, WHITE
    CALL is_square_attacked
    JP NZ, .no_bk
    LD A, SQ_F8
    LD B, WHITE
    CALL is_square_attacked
    JP NZ, .no_bk
    LD A, SQ_G8
    CALL try_add_move
.no_bk:

    ; Black queenside
    LD A, (castling_rights)
    AND CASTLE_BQ
    JP Z, .no_bq
    LD A, (board + SQ_B8)
    OR A
    JP NZ, .no_bq
    LD A, (board + SQ_C8)
    OR A
    JP NZ, .no_bq
    LD A, (board + SQ_D8)
    OR A
    JP NZ, .no_bq
    LD A, SQ_E8
    LD B, WHITE
    CALL is_square_attacked
    JP NZ, .no_bq
    LD A, SQ_D8
    LD B, WHITE
    CALL is_square_attacked
    JP NZ, .no_bq
    LD A, SQ_C8
    CALL try_add_move
.no_bq:
    JP generate_moves.next_sq

; =============================================================================
; HAS ANY LEGAL MOVE (for checkmate/stalemate detection)
; =============================================================================
has_any_legal_move:
    CALL generate_moves
    LD A, (move_list_count)
    OR A
    RET