; =============================================================================
; RUSTY CHESS - Attack Detection
; =============================================================================

; =============================================================================
; IS SQUARE ATTACKED
; =============================================================================
; Input:  A = square to check, B = attacking side color
; Output: Z = not attacked, NZ = attacked
; =============================================================================
is_square_attacked:
    LD (isa_square), A
    LD A, B
    LD (isa_attacker), A

    CALL check_knight_attack
    RET NZ
    CALL check_pawn_attack
    RET NZ
    CALL check_king_attack
    RET NZ
    CALL check_rook_queen_attack
    RET NZ
    CALL check_bishop_queen_attack
    RET

isa_square:     DEFB 0
isa_attacker:   DEFB 0

; =============================================================================
; KNIGHT ATTACK CHECK
; =============================================================================
check_knight_attack:
    LD HL, isa_knight_offsets
    LD B, 8

.loop:
    PUSH BC
    PUSH HL

    LD A, (isa_square)
    LD E, (HL)
    ADD A, E

    LD D, A
    AND OFF_BOARD
    JR NZ, .next

    LD A, D
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)

    OR A
    JR Z, .next

    LD C, A
    AND PIECE_MASK
    CP 2                            ; Knight?
    JR NZ, .next

    LD A, C
    AND COLOR_MASK
    LD E, A
    LD A, (isa_attacker)
    CP E
    JR NZ, .next

    POP HL
    POP BC
    OR 1                            ; NZ = attacked
    RET

.next:
    POP HL
    POP BC
    INC HL
    DJNZ .loop

    XOR A                           ; Z = not attacked
    RET

isa_knight_offsets:
    DEFB 14, 18, 31, 33
    DEFB -14 & $FF, -18 & $FF, -31 & $FF, -33 & $FF

; =============================================================================
; PAWN ATTACK CHECK
; =============================================================================
check_pawn_attack:
    LD A, (isa_attacker)
    OR A
    JR NZ, .black_pawns

    ; White pawns attack upward, so look below target
    LD A, (isa_square)
    SUB 17
    CALL .check_pawn_at
    RET NZ
    LD A, (isa_square)
    SUB 15
    CALL .check_pawn_at
    RET

.black_pawns:
    ; Black pawns attack downward, so look above target
    LD A, (isa_square)
    ADD A, 17
    CALL .check_pawn_at
    RET NZ
    LD A, (isa_square)
    ADD A, 15
    CALL .check_pawn_at
    RET

.check_pawn_at:
    LD D, A
    AND OFF_BOARD
    JR NZ, .no_pawn

    LD A, D
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)

    OR A
    JR Z, .no_pawn

    LD C, A
    AND PIECE_MASK
    CP 1                            ; Pawn?
    JR NZ, .no_pawn

    LD A, C
    AND COLOR_MASK
    LD E, A
    LD A, (isa_attacker)
    CP E
    JR NZ, .no_pawn

    OR 1                            ; NZ
    RET

.no_pawn:
    XOR A                           ; Z
    RET

; =============================================================================
; KING ATTACK CHECK
; =============================================================================
check_king_attack:
    LD HL, isa_king_offsets
    LD B, 8

.loop:
    PUSH BC
    PUSH HL

    LD A, (isa_square)
    LD E, (HL)
    ADD A, E

    LD D, A
    AND OFF_BOARD
    JR NZ, .next

    LD A, D
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)

    OR A
    JR Z, .next

    LD C, A
    AND PIECE_MASK
    CP 6                            ; King?
    JR NZ, .next

    LD A, C
    AND COLOR_MASK
    LD E, A
    LD A, (isa_attacker)
    CP E
    JR NZ, .next

    POP HL
    POP BC
    OR 1
    RET

.next:
    POP HL
    POP BC
    INC HL
    DJNZ .loop

    XOR A
    RET

isa_king_offsets:
    DEFB 1, 15, 16, 17
    DEFB -1 & $FF, -15 & $FF, -16 & $FF, -17 & $FF

; =============================================================================
; ROOK/QUEEN ATTACK CHECK (ORTHOGONAL SLIDING)
; =============================================================================
check_rook_queen_attack:
    LD HL, isa_ortho_dirs
    LD B, 4

.dir_loop:
    PUSH BC
    PUSH HL

    LD A, (HL)
    LD (isa_slide_dir), A
    LD A, (isa_square)

.slide:
    LD E, A
    LD A, (isa_slide_dir)
    ADD A, E
    LD D, A

    AND OFF_BOARD
    JR NZ, .next_dir

    LD A, D
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)

    OR A
    JR Z, .continue

    ; Found piece
    LD C, A
    AND COLOR_MASK
    LD E, A
    LD A, (isa_attacker)
    CP E
    JR NZ, .next_dir                ; Friendly blocks

    LD A, C
    AND PIECE_MASK
    CP 4                            ; Rook?
    JR Z, .found
    CP 5                            ; Queen?
    JR Z, .found
    JR .next_dir                    ; Enemy but wrong type

.continue:
    ; Get square index back from board pointer
    PUSH HL
    LD DE, board
    OR A
    SBC HL, DE
    LD A, L
    POP HL
    JR .slide

.found:
    POP HL
    POP BC
    OR 1
    RET

.next_dir:
    POP HL
    POP BC
    INC HL
    DJNZ .dir_loop

    XOR A
    RET

isa_ortho_dirs:
    DEFB 1, -1 & $FF, 16, -16 & $FF

isa_slide_dir:  DEFB 0

; =============================================================================
; BISHOP/QUEEN ATTACK CHECK (DIAGONAL SLIDING)
; =============================================================================
check_bishop_queen_attack:
    LD HL, isa_diag_dirs
    LD B, 4

.dir_loop:
    PUSH BC
    PUSH HL

    LD A, (HL)
    LD (isa_slide_dir2), A
    LD A, (isa_square)

.slide:
    LD E, A
    LD A, (isa_slide_dir2)
    ADD A, E
    LD D, A

    AND OFF_BOARD
    JR NZ, .next_dir

    LD A, D
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)

    OR A
    JR Z, .continue

    LD C, A
    AND COLOR_MASK
    LD E, A
    LD A, (isa_attacker)
    CP E
    JR NZ, .next_dir

    LD A, C
    AND PIECE_MASK
    CP 3                            ; Bishop?
    JR Z, .found
    CP 5                            ; Queen?
    JR Z, .found
    JR .next_dir

.continue:
    PUSH HL
    LD DE, board
    OR A
    SBC HL, DE
    LD A, L
    POP HL
    JR .slide

.found:
    POP HL
    POP BC
    OR 1
    RET

.next_dir:
    POP HL
    POP BC
    INC HL
    DJNZ .dir_loop

    XOR A
    RET

isa_diag_dirs:
    DEFB 15, 17, -15 & $FF, -17 & $FF

isa_slide_dir2: DEFB 0