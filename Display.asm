; =============================================================================
; RUSTY CHESS - Display Routines
; =============================================================================

; =============================================================================
; CLEAR SCREEN
; =============================================================================
cls_screen:
    LD HL, SCREEN_PIXELS
    LD DE, SCREEN_PIXELS + 1
    LD BC, 6143
    LD (HL), 0
    LDIR

    LD HL, SCREEN_ATTR
    LD DE, SCREEN_ATTR + 1
    LD BC, 767
    LD (HL), %00111000              ; White paper, black ink
    LDIR
    RET

; =============================================================================
; DRAW BOARD
; =============================================================================
draw_board:
    LD C, 0                         ; Rank counter (0-7)
.rank_loop:
    LD B, 0                         ; File counter (0-7)
.file_loop:
    ; Calculate 0x88 square index: rank*16 + file
    LD A, C
    RLCA
    RLCA
    RLCA
    RLCA
    OR B
    LD (draw_sq_idx), A

    ; Get piece on this square
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)
    LD (draw_piece_val), A          ; Save full piece value

    ; Screen position: row = BOARD_Y + (7 - rank), col = BOARD_X + file
    PUSH BC
    LD A, 7
    SUB C
    ADD A, BOARD_Y
    LD D, A
    LD A, B
    ADD A, BOARD_X
    LD E, A
    LD (draw_scr_row), A            ; Save col in low, row in high
    LD A, D
    LD (draw_scr_row), A
    LD A, E
    LD (draw_scr_col), A

    ; Get piece bitmap using FULL piece value (0-14)
    LD A, (draw_piece_val)
    LD L, A
    LD H, 0
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL                      ; *8 bytes per bitmap
    LD BC, piece_bitmaps
    ADD HL, BC
    PUSH HL
    POP IX

    CALL draw_char
    POP BC                          ; B=file, C=rank

    ; --- Calculate and set attribute ---
    ; Paper colour: light if (file+rank) is odd, dark if even
    LD A, B
    ADD A, C
    AND 1
    JP Z, .dark_sq
    LD A, LIGHT_PAPER
    JP .got_paper
.dark_sq:
    LD A, DARK_PAPER
.got_paper:
    LD (draw_attr_val), A

    ; Ink colour based on piece colour
    LD A, (draw_piece_val)
    OR A
    JP Z, .set_final_attr           ; Empty - no ink to add

    AND COLOR_MASK
    JP NZ, .is_black_piece

    ; White piece: blue ink
    LD A, (draw_attr_val)
    OR INK_WHITE
    LD (draw_attr_val), A
    JP .set_final_attr

.is_black_piece:
    LD A, (draw_attr_val)
    OR INK_BLACK
    LD (draw_attr_val), A

.set_final_attr:
    ; Restore screen position
    LD A, (draw_scr_row)
    LD D, A
    LD A, (draw_scr_col)
    LD E, A
    LD A, (draw_attr_val)
    CALL set_attr

    ; Next file
    INC B
    LD A, B
    CP 8
    JP NZ, .file_loop

    INC C
    LD A, C
    CP 8
    JP NZ, .rank_loop
    RET

draw_sq_idx:    DEFB 0
draw_piece_val: DEFB 0
draw_scr_row:   DEFB 0
draw_scr_col:   DEFB 0
draw_attr_val:  DEFB 0

; =============================================================================
; DRAW CURSOR HIGHLIGHT
; =============================================================================
draw_cursor:
    LD A, (cursor_sq)
    CALL sq_to_screen
    LD A, CURSOR_ATTR
    CALL set_attr

    ; Draw selection if active
    LD A, (selected_sq)
    CP $FF
    RET Z
    CALL sq_to_screen
    LD A, SELECT_ATTR
    CALL set_attr
    RET

; =============================================================================
; DRAW CHECK HIGHLIGHT
; =============================================================================
draw_check_highlight:
    LD A, (side_to_move)
    OR A
    JP NZ, .check_black

    LD A, (white_king_sq)
    LD B, BLACK
    JP .do_check

.check_black:
    LD A, (black_king_sq)
    LD B, WHITE

.do_check:
    PUSH AF
    CALL is_square_attacked
    POP BC

    JP Z, .no_check

    LD A, 1
    LD (in_check), A
    LD A, B
    CALL sq_to_screen
    LD A, CHECK_ATTR
    CALL set_attr
    RET

.no_check:
    XOR A
    LD (in_check), A
    RET

; =============================================================================
; DRAW STATUS LINE
; =============================================================================
draw_status:
    ; Clear status area (row 17, cols 0-31)
    LD D, 17
    LD E, 0
    LD B, 32
.clear_status:
    PUSH BC
    PUSH DE
    LD A, ' '
    CALL draw_font_char
    POP DE
    POP BC
    INC E
    DJNZ .clear_status

    ; Game over messages
    LD A, (game_over)
    CP 1
    JP Z, .show_checkmate
    CP 2
    JP Z, .show_stalemate

    ; Normal play - show side to move
    LD A, (side_to_move)
    OR A
    JP NZ, .show_black

    LD HL, str_white
    JP .show_side

.show_black:
    LD HL, str_black

.show_side:
    LD D, 17
    LD E, 0
    CALL draw_string

    ; Show check indicator
    LD A, (in_check)
    OR A
    RET Z
    LD HL, str_check
    LD D, 17
    LD E, 16
    CALL draw_string
    RET

.show_checkmate:
    LD HL, str_checkmate
    LD D, 17
    LD E, 0
    CALL draw_string

    LD A, (side_to_move)
    OR A
    JP NZ, .white_wins

    LD HL, str_black_wins
    JP .show_winner

.white_wins:
    LD HL, str_white_wins

.show_winner:
    LD D, 17
    LD E, 14
    CALL draw_string

    LD HL, str_restart
    LD D, 18
    LD E, 0
    CALL draw_string
    RET

.show_stalemate:
    LD HL, str_stalemate
    LD D, 17
    LD E, 0
    CALL draw_string

    LD HL, str_draw
    LD D, 17
    LD E, 14
    CALL draw_string

    LD HL, str_restart
    LD D, 18
    LD E, 0
    CALL draw_string
    RET

; =============================================================================
; DRAW LABELS
; =============================================================================
draw_labels:
    LD B, 8
    LD C, 0
.file_labels:
    LD A, C
    ADD A, BOARD_X
    LD E, A
    LD D, BOARD_Y + 8
    LD A, C
    ADD A, 'a'
    PUSH BC
    PUSH DE
    CALL draw_font_char
    POP DE
    POP BC
    INC C
    DJNZ .file_labels

    LD B, 8
    LD C, 0
.rank_labels:
    LD E, BOARD_X - 1
    LD A, 7
    SUB C
    ADD A, BOARD_Y
    LD D, A
    LD A, C
    ADD A, '1'
    PUSH BC
    PUSH DE
    CALL draw_font_char
    POP DE
    POP BC
    INC C
    DJNZ .rank_labels
    RET

; =============================================================================
; DRAW HELP TEXT
; =============================================================================
draw_help:
    LD HL, str_help1
    LD D, 18
    LD E, 0
    CALL draw_string

    LD HL, str_help2
    LD D, 19
    LD E, 0
    CALL draw_string
    RET

; =============================================================================
; SHOW ILLEGAL MOVE FLASH
; =============================================================================
show_illegal:
    LD A, (cursor_sq)
    CALL sq_to_screen
    LD A, ILLEGAL_ATTR
    CALL set_attr
    RET

; =============================================================================
; CONVERT 0x88 SQUARE TO SCREEN POSITION
; =============================================================================
sq_to_screen:
    LD B, A
    AND $07
    ADD A, BOARD_X
    LD E, A
    LD A, B
    RRCA
    RRCA
    RRCA
    RRCA
    AND $07
    LD B, A
    LD A, 7
    SUB B
    ADD A, BOARD_Y
    LD D, A
    RET

; =============================================================================
; SET ATTRIBUTE AT SCREEN POSITION
; =============================================================================
set_attr:
    PUSH AF
    LD A, D
    RRCA
    RRCA
    RRCA
    AND %11100000
    OR E
    LD L, A
    LD A, D
    RRCA
    RRCA
    RRCA
    AND %00000011
    OR %01011000
    LD H, A
    POP AF
    LD (HL), A
    RET

; =============================================================================
; DRAW 8x8 CHARACTER AT SCREEN POSITION
; =============================================================================
draw_char:
    LD A, D
    AND %00011000
    OR %01000000
    LD H, A
    LD A, D
    AND %00000111
    RRCA
    RRCA
    RRCA
    OR E
    LD L, A

    LD B, 8
.draw_loop:
    LD A, (IX)
    LD (HL), A
    INC IX
    INC H
    DJNZ .draw_loop
    RET

; =============================================================================
; DRAW FONT CHARACTER AT SCREEN POSITION
; =============================================================================
draw_font_char:
    SUB 32
    LD L, A
    LD H, 0
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    LD BC, font_data
    ADD HL, BC
    PUSH HL
    POP IX

    CALL draw_char
    RET

; =============================================================================
; DRAW STRING AT SCREEN POSITION
; =============================================================================
draw_string:
    LD A, (HL)
    OR A
    RET Z

    PUSH HL
    PUSH DE
    CALL draw_font_char
    POP DE
    POP HL

    INC HL
    INC E
    JP draw_string