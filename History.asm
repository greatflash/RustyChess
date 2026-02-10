; =============================================================================
; RUSTY CHESS - Move History and Captured Pieces
; =============================================================================

; =============================================================================
; RECORD MOVE IN HISTORY
; =============================================================================
record_move:
    LD A, (move_count)
    INC A
    LD (move_count), A

    LD A, (hist_entries)
    CP HIST_MAX
    JP C, .not_full

    LD HL, hist_buffer + 3
    LD DE, hist_buffer
    LD BC, (HIST_MAX - 1) * 3
    LDIR

    LD A, HIST_MAX - 1
    JP .write_entry

.not_full:
    LD A, (hist_entries)
    INC A
    LD (hist_entries), A
    DEC A

.write_entry:
    LD L, A
    LD H, 0
    LD D, H
    LD E, L
    ADD HL, HL
    ADD HL, DE
    LD DE, hist_buffer
    ADD HL, DE

    LD A, (move_from)
    LD (HL), A
    INC HL
    LD A, (move_to)
    LD (HL), A
    INC HL

    LD A, (save_to_piece)
    OR A
    JP NZ, .was_capture
    LD A, (did_ep_capture)
    OR A
    JP NZ, .was_capture
    LD (HL), 0
    RET
.was_capture:
    LD (HL), 1
    RET

; =============================================================================
; RECORD CAPTURED PIECE
; =============================================================================
record_capture:
    OR A
    RET Z

    LD C, A

    AND COLOR_MASK
    JP NZ, .black_captured

    LD A, (capt_by_black_count)
    CP CAPT_MAX
    RET NC
    LD L, A
    LD H, 0
    LD DE, capt_by_black
    ADD HL, DE
    LD (HL), C
    LD A, (capt_by_black_count)
    INC A
    LD (capt_by_black_count), A
    RET

.black_captured:
    LD A, (capt_by_white_count)
    CP CAPT_MAX
    RET NC
    LD L, A
    LD H, 0
    LD DE, capt_by_white
    ADD HL, DE
    LD (HL), C
    LD A, (capt_by_white_count)
    INC A
    LD (capt_by_white_count), A
    RET

; =============================================================================
; DRAW MOVE HISTORY
; =============================================================================
draw_history:
    LD HL, str_moves
    LD D, HIST_Y - 1
    LD E, HIST_X
    CALL draw_string

    LD C, 0
.clear_loop:
    LD A, C
    CP HIST_MAX
    JP NC, .clear_done
    PUSH BC
    LD A, C
    ADD A, HIST_Y
    LD D, A
    LD E, HIST_X
    LD B, 9
.clear_inner:
    PUSH BC
    PUSH DE
    LD A, ' '
    CALL draw_font_char
    POP DE
    POP BC
    INC E
    DJNZ .clear_inner
    POP BC
    INC C
    JP .clear_loop
.clear_done:

    LD A, (hist_entries)
    OR A
    RET Z

    LD (hist_draw_total), A
    XOR A
    LD (hist_draw_idx), A

.draw_loop:
    LD A, (hist_draw_idx)
    LD B, A
    LD A, (hist_draw_total)
    CP B
    RET Z

    LD A, (hist_draw_idx)
    ADD A, HIST_Y
    LD (hist_draw_row), A

    LD A, (move_count)
    LD B, A
    LD A, (hist_draw_total)
    LD C, A
    LD A, B
    SUB C
    LD B, A
    LD A, (hist_draw_idx)
    ADD A, B
    INC A
    LD (hist_draw_ply), A

    DEC A
    SRA A
    INC A

    LD (hist_draw_num), A
    LD A, (hist_draw_row)
    LD D, A
    LD E, HIST_X

    LD A, (hist_draw_num)
    CP 10
    JP C, .one_digit

    LD C, 0
.div10:
    SUB 10
    JP C, .div10_done
    INC C
    JP .div10
.div10_done:
    ADD A, 10
    LD B, A
    LD A, C
    ADD A, '0'
    PUSH BC
    PUSH DE
    CALL draw_font_char
    POP DE
    POP BC
    INC E
    LD A, B
    ADD A, '0'
    PUSH DE
    CALL draw_font_char
    POP DE
    INC E
    JP .after_num

.one_digit:
    ADD A, '0'
    PUSH DE
    CALL draw_font_char
    POP DE
    INC E

.after_num:
    LD A, '.'
    PUSH DE
    CALL draw_font_char
    POP DE
    INC E

    LD A, (hist_draw_idx)
    LD L, A
    LD H, 0
    LD D, H
    LD E, L
    ADD HL, HL
    ADD HL, DE
    LD DE, hist_buffer
    ADD HL, DE

    LD A, (HL)
    LD (hist_temp_from), A
    INC HL
    LD A, (HL)
    LD (hist_temp_to), A
    INC HL
    LD A, (HL)
    LD (hist_temp_capt), A

    LD A, (hist_draw_row)
    LD D, A
    LD A, (hist_draw_num)
    CP 10
    JP C, .e_one_digit
    LD E, HIST_X + 3
    JP .e_done
.e_one_digit:
    LD E, HIST_X + 2
.e_done:

    LD A, (hist_temp_from)
    LD B, A
    AND $07
    ADD A, 'a'
    PUSH BC
    PUSH DE
    CALL draw_font_char
    POP DE
    POP BC
    INC E

    LD A, B
    RRCA
    RRCA
    RRCA
    RRCA
    AND $07
    ADD A, '1'
    PUSH DE
    CALL draw_font_char
    POP DE
    INC E

    LD A, (hist_temp_capt)
    OR A
    JP NZ, .draw_capture_sep
    LD A, '-'
    JP .draw_sep
.draw_capture_sep:
    LD A, 'x'
.draw_sep:
    PUSH DE
    CALL draw_font_char
    POP DE
    INC E

    LD A, (hist_temp_to)
    LD B, A
    AND $07
    ADD A, 'a'
    PUSH BC
    PUSH DE
    CALL draw_font_char
    POP DE
    POP BC
    INC E

    LD A, B
    RRCA
    RRCA
    RRCA
    RRCA
    AND $07
    ADD A, '1'
    PUSH DE
    CALL draw_font_char
    POP DE

    LD A, (hist_draw_idx)
    INC A
    LD (hist_draw_idx), A
    JP .draw_loop

hist_draw_idx:      DEFB 0
hist_draw_total:    DEFB 0
hist_draw_row:      DEFB 0
hist_draw_num:      DEFB 0
hist_draw_ply:      DEFB 0
hist_temp_from:     DEFB 0
hist_temp_to:       DEFB 0
hist_temp_capt:     DEFB 0

; =============================================================================
; DRAW CAPTURED PIECES (as piece graphics)
; =============================================================================
draw_captures:
    LD HL, str_capt_w
    LD D, CAPT_W_Y - 1
    LD E, CAPT_W_X
    CALL draw_string

    LD HL, str_capt_b
    LD D, CAPT_B_Y - 1
    LD E, CAPT_B_X
    CALL draw_string

    CALL clear_area_w
    CALL clear_area_b

    ; --- White's captures (black pieces taken) ---
    LD A, (capt_by_white_count)
    OR A
    JP Z, .do_black_capt

    LD B, A
    LD C, 0
    LD HL, capt_by_white
.w_loop:
    PUSH BC
    PUSH HL

    ; Get full piece value for bitmap
    LD A, (HL)
    LD L, A
    LD H, 0
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    LD DE, piece_bitmaps
    ADD HL, DE
    PUSH HL
    POP IX

    ; Calculate screen position
    LD A, C
    AND 7
    ADD A, CAPT_W_X
    LD E, A
    LD A, C
    RRCA
    RRCA
    RRCA
    AND 3
    ADD A, CAPT_W_Y
    LD D, A

    PUSH DE
    CALL draw_char
    POP DE

    ; Black pieces shown with black ink on white paper
    LD A, %00111000
    CALL set_attr

    POP HL
    POP BC
    INC HL
    INC C
    DJNZ .w_loop

.do_black_capt:
    ; --- Black's captures (white pieces taken) ---
    LD A, (capt_by_black_count)
    OR A
    RET Z

    LD B, A
    LD C, 0
    LD HL, capt_by_black
.b_loop:
    PUSH BC
    PUSH HL

    ; Get full piece value for bitmap
    LD A, (HL)
    LD L, A
    LD H, 0
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    LD DE, piece_bitmaps
    ADD HL, DE
    PUSH HL
    POP IX

    ; Calculate screen position
    LD A, C
    AND 7
    ADD A, CAPT_B_X
    LD E, A
    LD A, C
    RRCA
    RRCA
    RRCA
    AND 3
    ADD A, CAPT_B_Y
    LD D, A

    PUSH DE
    CALL draw_char
    POP DE

    ; White pieces shown with blue ink on white paper
    LD A, %00111001
    CALL set_attr

    POP HL
    POP BC
    INC HL
    INC C
    DJNZ .b_loop
    RET

; =============================================================================
; CLEAR CAPTURE DISPLAY AREAS
; =============================================================================
clear_area_w:
    LD D, CAPT_W_Y
    LD E, CAPT_W_X
    LD B, 8
.row1:
    PUSH BC
    PUSH DE
    LD A, ' '
    CALL draw_font_char
    POP DE
    POP BC
    INC E
    DJNZ .row1
    LD D, CAPT_W_Y + 1
    LD E, CAPT_W_X
    LD B, 8
.row2:
    PUSH BC
    PUSH DE
    LD A, ' '
    CALL draw_font_char
    POP DE
    POP BC
    INC E
    DJNZ .row2
    RET

clear_area_b:
    LD D, CAPT_B_Y
    LD E, CAPT_B_X
    LD B, 8
.row1:
    PUSH BC
    PUSH DE
    LD A, ' '
    CALL draw_font_char
    POP DE
    POP BC
    INC E
    DJNZ .row1
    LD D, CAPT_B_Y + 1
    LD E, CAPT_B_X
    LD B, 8
.row2:
    PUSH BC
    PUSH DE
    LD A, ' '
    CALL draw_font_char
    POP DE
    POP BC
    INC E
    DJNZ .row2
    RET

; =============================================================================
; CLEAR HISTORY AND CAPTURES
; =============================================================================
clear_history:
    XOR A
    LD (move_count), A
    LD (hist_entries), A
    LD (capt_by_white_count), A
    LD (capt_by_black_count), A

    LD HL, hist_buffer
    LD B, HIST_MAX * 3
.ch1:
    LD (HL), 0
    INC HL
    DJNZ .ch1

    LD HL, capt_by_white
    LD B, CAPT_MAX
.ch2:
    LD (HL), 0
    INC HL
    DJNZ .ch2

    LD HL, capt_by_black
    LD B, CAPT_MAX
.ch3:
    LD (HL), 0
    INC HL
    DJNZ .ch3
    RET