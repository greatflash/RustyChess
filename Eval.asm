; =============================================================================
; RUSTY CHESS - Position Evaluation
; =============================================================================
; Returns score in HL (16-bit signed, from side_to_move perspective)
; Positive = good for side to move
; =============================================================================

; --- Piece values (centipawns) ---
VAL_PAWN        EQU 100
VAL_KNIGHT      EQU 320
VAL_BISHOP      EQU 330
VAL_ROOK        EQU 500
VAL_QUEEN       EQU 900
VAL_KING        EQU 10000

; =============================================================================
; EVALUATE POSITION
; =============================================================================
; Output: HL = score from side_to_move's perspective
; =============================================================================
evaluate:
    ; Clear score accumulators
    LD HL, 0
    LD (eval_score), HL

    ; Scan all 64 squares
    LD A, 0
    LD (eval_sq), A

.scan_loop:
    LD A, (eval_sq)
    LD B, A
    AND OFF_BOARD
    JP NZ, .next_sq

    ; Get piece
    LD A, B
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    OR A
    JP Z, .next_sq

    ; Save piece and square
    LD (eval_piece), A
    LD A, (eval_sq)
    LD (eval_cur_sq), A

    ; Get piece type
    LD A, (eval_piece)
    AND PIECE_MASK
    LD (eval_type), A

    ; Get base material value
    CALL get_piece_value            ; Returns HL = value

    ; Add positional bonus
    PUSH HL
    CALL get_positional_bonus       ; Returns HL = bonus
    POP DE
    ADD HL, DE                      ; HL = value + bonus

    ; Check piece colour
    LD A, (eval_piece)
    AND COLOR_MASK
    JP NZ, .black_piece

    ; White piece: add to score
    LD DE, (eval_score)
    ADD HL, DE
    LD (eval_score), HL
    JP .next_sq

.black_piece:
    ; Black piece: subtract from score
    LD DE, (eval_score)
    EX DE, HL
    OR A                            ; Clear carry
    SBC HL, DE
    LD (eval_score), HL

.next_sq:
    LD A, (eval_sq)
    INC A
    LD (eval_sq), A
    CP $78
    JP C, .scan_loop

    ; --- Second pass: check for hanging pieces ---
    LD A, 0
    LD (eval_sq), A

.hanging_loop:
    LD A, (eval_sq)
    LD B, A
    AND OFF_BOARD
    JP NZ, .next_hanging

    ; Get piece
    LD A, B
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    OR A
    JP Z, .next_hanging

    ; Save piece info
    LD (eval_piece), A
    LD A, B
    LD (eval_cur_sq), A

    ; Get piece type
    LD A, (eval_piece)
    AND PIECE_MASK
    LD (eval_type), A

    ; Skip kings (type 6)
    CP 6
    JP Z, .next_hanging

    ; Get piece color
    LD A, (eval_piece)
    AND COLOR_MASK
    LD (eval_color), A

    ; Get enemy color
    XOR COLOR_MASK
    LD (eval_enemy_color), A

    ; Check if attacked by enemy
    LD A, (eval_cur_sq)
    LD B, A                         ; Save square for defender check
    LD (eval_save_sq), A
    LD A, (eval_enemy_color)
    LD C, A                         ; Save enemy color
    LD (eval_save_enemy), A
    
    LD A, (eval_save_sq)
    LD B, C                         ; B = attacking side color
    CALL is_square_attacked
    JP Z, .next_hanging             ; Not attacked, skip

    ; Piece is attacked - check if defended
    LD A, (eval_save_sq)
    LD B, A
    LD A, (eval_color)
    LD C, A
    LD A, B
    LD B, C                         ; B = defending side color
    CALL is_square_attacked
    JP NZ, .next_hanging            ; Is defended, skip

    ; Piece is hanging - apply penalty
    CALL get_piece_value            ; Returns HL = value
    
    ; Halve the value (right shift)
    SRL H
    RR L

    ; Apply penalty based on piece color
    LD A, (eval_color)
    OR A
    JP NZ, .hanging_black

    ; White piece hanging: subtract from score
    LD DE, (eval_score)
    EX DE, HL
    OR A
    SBC HL, DE
    LD (eval_score), HL
    JP .next_hanging

.hanging_black:
    ; Black piece hanging: add to score (makes position better for white)
    LD DE, (eval_score)
    ADD HL, DE
    LD (eval_score), HL

.next_hanging:
    LD A, (eval_sq)
    INC A
    LD (eval_sq), A
    CP $78
    JP C, .hanging_loop

    ; Score is from white's perspective
    ; If black to move, negate
    LD HL, (eval_score)
    LD A, (side_to_move)
    OR A
    RET Z                           ; White to move, return as-is

    ; Negate HL for black's perspective
    LD A, H
    CPL
    LD H, A
    LD A, L
    CPL
    LD L, A
    INC HL
    RET

eval_sq:        DEFB 0
eval_piece:     DEFB 0
eval_cur_sq:    DEFB 0
eval_type:      DEFB 0
eval_score:     DEFW 0
eval_color:     DEFB 0
eval_enemy_color: DEFB 0
eval_save_sq:   DEFB 0
eval_save_enemy: DEFB 0

; =============================================================================
; GET PIECE VALUE
; =============================================================================
; Input: eval_type = piece type (1-6)
; Output: HL = material value
; =============================================================================
get_piece_value:
    LD A, (eval_type)
    ADD A, A                        ; *2 for word table
    LD E, A
    LD D, 0
    LD HL, piece_value_table
    ADD HL, DE
    LD A, (HL)
    INC HL
    LD H, (HL)
    LD L, A
    RET

piece_value_table:
    DEFW 0                          ; 0 = empty
    DEFW VAL_PAWN                   ; 1 = pawn
    DEFW VAL_KNIGHT                 ; 2 = knight
    DEFW VAL_BISHOP                 ; 3 = bishop
    DEFW VAL_ROOK                   ; 4 = rook
    DEFW VAL_QUEEN                  ; 5 = queen
    DEFW VAL_KING                   ; 6 = king

; =============================================================================
; GET POSITIONAL BONUS
; =============================================================================
; Input: eval_type, eval_cur_sq
; Output: HL = positional bonus
; =============================================================================
get_positional_bonus:
    LD A, (eval_type)
    CP 1
    JP Z, .pawn_bonus
    CP 2
    JP Z, .knight_bonus
    CP 3
    JP Z, .bishop_bonus
    CP 6
    JP Z, .king_bonus

    ; Rook/Queen: no positional bonus for now
    LD HL, 0
    RET

; --- Pawn bonuses ---
.pawn_bonus:
    LD HL, 0

    ; Centre bonus: d4,e4,d5,e5 get +20
    LD A, (eval_cur_sq)
    CP $33                          ; d4
    JP Z, .pawn_centre
    CP $34                          ; e4
    JP Z, .pawn_centre
    CP $43                          ; d5
    JP Z, .pawn_centre
    CP $44                          ; e5
    JP Z, .pawn_centre

    ; Extended centre: c3-f3, c4-f4, c5-f5, c6-f6 get +10
    LD A, (eval_cur_sq)
    AND $07                         ; File
    CP 2
    JP C, .pawn_advance             ; a,b files
    CP 6
    JP NC, .pawn_advance            ; g,h files
    ; File c-f, check rank 3-6
    LD A, (eval_cur_sq)
    AND $70                         ; Rank
    CP $20                          ; Rank 3
    JP C, .pawn_advance
    CP $70                          ; Past rank 6
    JP NC, .pawn_advance
    LD HL, 10
    JP .pawn_advance

.pawn_centre:
    LD HL, 20

.pawn_advance:
    ; Advancement bonus: rank * 2 (white) or (7-rank) * 2 (black)
    PUSH HL
    LD A, (eval_piece)
    AND COLOR_MASK
    JP NZ, .pawn_black_advance

    ; White pawn: bonus for advancing
    LD A, (eval_cur_sq)
    AND $70
    RRCA
    RRCA
    RRCA
    RRCA                            ; rank 0-7
    ADD A, A                        ; *2
    JP .pawn_add_advance

.pawn_black_advance:
    LD A, (eval_cur_sq)
    AND $70
    RRCA
    RRCA
    RRCA
    RRCA
    LD B, A
    LD A, 7
    SUB B
    ADD A, A                        ; *2

.pawn_add_advance:
    LD E, A
    LD D, 0
    POP HL
    ADD HL, DE
    RET

; --- Knight bonuses ---
.knight_bonus:
    ; Knights are better in the centre
    ; Penalty for edge squares
    LD HL, 0

    ; Check file
    LD A, (eval_cur_sq)
    AND $07
    CP 0
    JP Z, .knight_edge
    CP 7
    JP Z, .knight_edge

    ; Check rank
    LD A, (eval_cur_sq)
    AND $70
    CP $00
    JP Z, .knight_edge
    CP $70
    JP Z, .knight_edge

    ; Inner centre bonus
    LD A, (eval_cur_sq)
    AND $07
    CP 2
    JP C, .knight_no_centre
    CP 6
    JP NC, .knight_no_centre
    LD A, (eval_cur_sq)
    AND $70
    CP $20
    JP C, .knight_no_centre
    CP $60
    JP NC, .knight_no_centre
    LD HL, 15                       ; Centre knight bonus
    RET

.knight_no_centre:
    LD HL, 0
    RET

.knight_edge:
    LD HL, -10                      ; Edge knight penalty
    RET

; --- Bishop bonuses ---
.bishop_bonus:
    ; Small bonus for not being on back rank
    LD A, (eval_piece)
    AND COLOR_MASK
    JP NZ, .bishop_black

    ; White bishop: bonus if off rank 1
    LD A, (eval_cur_sq)
    AND $70
    JP Z, .bishop_undeveloped
    LD HL, 10
    RET

.bishop_black:
    ; Black bishop: bonus if off rank 8
    LD A, (eval_cur_sq)
    AND $70
    CP $70
    JP Z, .bishop_undeveloped
    LD HL, 10
    RET

.bishop_undeveloped:
    LD HL, 0
    RET

; --- King bonuses ---
.king_bonus:
    ; King safety: bonus for being castled (on g1/c1 or g8/c8)
    LD A, (eval_cur_sq)
    CP SQ_G1
    JP Z, .king_castled
    CP SQ_C1
    JP Z, .king_castled
    CP SQ_G8
    JP Z, .king_castled
    CP SQ_C8
    JP Z, .king_castled

    ; Penalty for king in centre during opening
    LD A, (eval_cur_sq)
    AND $07
    CP 2
    JP C, .king_ok
    CP 6
    JP NC, .king_ok
    LD HL, -20                      ; Centre king penalty
    RET

.king_castled:
    LD HL, 30
    RET

.king_ok:
    LD HL, 0
    RET