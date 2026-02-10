; =============================================================================
; RUSTY CHESS - Input Handling
; =============================================================================

read_keys:
    ; --- Movement keys ---
    ; Q = Up
    LD BC, PORT_QWERT
    IN A, (C)
    BIT 0, A
    JP Z, .move_up

    ; A = Down
    LD BC, PORT_ASDFG
    IN A, (C)
    BIT 0, A
    JP Z, .move_down

    ; O = Left
    LD BC, PORT_POIUY
    IN A, (C)
    BIT 1, A
    JP Z, .move_left

    ; P = Right
    LD BC, PORT_POIUY
    IN A, (C)
    BIT 0, A
    JP Z, .move_right

    ; SPACE = Select/Move
    LD BC, PORT_SPACE
    IN A, (C)
    BIT 0, A
    JP Z, .select

    ; X = Quit
    LD BC, PORT_SHIFT
    IN A, (C)
    BIT 2, A
    JP Z, .quit

    ; R = Restart
    LD BC, PORT_QWERT
    IN A, (C)
    BIT 3, A
    JP Z, .restart

    ; T = Load test position
    LD BC, PORT_QWERT
    IN A, (C)
    BIT 4, A
    JP Z, .test_pos

    ; G = Generate moves test
    LD BC, PORT_ASDFG
    IN A, (C)
    BIT 4, A
    JP Z, .gen_test

    ; No key pressed - reset repeat
    LD A, $FF
    LD (last_key), A
    XOR A
    LD (key_held), A
    RET

; --- Move Up ---
.move_up:
    LD A, 1
    CALL check_repeat
    RET Z

    LD A, (cursor_sq)
    ADD A, 16
    LD B, A
    AND OFF_BOARD
    RET NZ
    LD A, B
    LD (cursor_sq), A
    RET

; --- Move Down ---
.move_down:
    LD A, 2
    CALL check_repeat
    RET Z

    LD A, (cursor_sq)
    SUB 16
    LD B, A
    AND OFF_BOARD
    RET NZ
    LD A, B
    LD (cursor_sq), A
    RET

; --- Move Left ---
.move_left:
    LD A, 3
    CALL check_repeat
    RET Z

    LD A, (cursor_sq)
    DEC A
    LD B, A
    AND OFF_BOARD
    RET NZ
    LD A, B
    LD (cursor_sq), A
    RET

; --- Move Right ---
.move_right:
    LD A, 4
    CALL check_repeat
    RET Z

    LD A, (cursor_sq)
    INC A
    LD B, A
    AND OFF_BOARD
    RET NZ
    LD A, B
    LD (cursor_sq), A
    RET

; --- Select ---
.select:
    LD A, 5
    CALL check_repeat
    RET Z

    ; If no piece selected, select one
    LD A, (selected_sq)
    CP $FF
    JP NZ, .try_move

    ; Select piece at cursor
    LD A, (cursor_sq)
    LD HL, board
    LD E, A
    LD D, 0
    ADD HL, DE
    LD A, (HL)

    ; Empty square?
    OR A
    RET Z

    ; Must be our piece
    AND COLOR_MASK
    LD B, A
    LD A, (side_to_move)
    CP B
    RET NZ

    ; Select it
    LD A, (cursor_sq)
    LD (selected_sq), A
    RET

.try_move:
    ; If cursor is on selected square, deselect
    LD A, (cursor_sq)
    LD B, A
    LD A, (selected_sq)
    CP B
    JP NZ, .do_move

    LD A, $FF
    LD (selected_sq), A
    RET

.do_move:
    ; Set up move
    LD A, (selected_sq)
    LD (move_from), A
    LD A, (cursor_sq)
    LD (move_to), A

    ; Deselect
    LD A, $FF
    LD (selected_sq), A

    ; Try the move
    CALL try_make_move
    RET

; --- Quit ---
.quit:
    LD A, 1
    LD (quit_flag), A
    RET

; --- Restart ---
.restart:
    LD A, 8
    CALL check_repeat
    RET Z

    CALL reset_game
    CALL cls_screen
    CALL draw_labels
    CALL draw_help
    RET

; --- Load test position ---
.test_pos:
    LD A, 9
    CALL check_repeat
    RET Z

    CALL load_test_position
    CALL cls_screen
    CALL draw_labels
    CALL draw_help
    RET

; --- Generate moves test ---
.gen_test:
    LD A, 10
    CALL check_repeat
    RET Z

    CALL generate_moves

    ; Clear rows 1-6 (display area)
    LD C, 0
.clear_all:
    LD A, C
    CP 6
    JP NC, .clear_all_done
    PUSH BC
    LD A, C
    ADD A, 1
    LD D, A
    LD E, 0
    LD B, 32                        ; Full width
.clear_all_row:
    PUSH BC
    PUSH DE
    LD A, ' '
    CALL draw_font_char
    POP DE
    POP BC
    INC E
    DJNZ .clear_all_row
    POP BC
    INC C
    JP .clear_all
.clear_all_done:

    ; Row 1: "GEN:nn CK:xx"
    LD HL, str_gen
    LD D, 1
    LD E, 1
    CALL draw_string

    ; Convert move_list_count to decimal and display
    LD A, (move_list_count)

    ; Hundreds
    LD C, 0
.hundreds:
    CP 100
    JP C, .no_hundreds
    SUB 100
    INC C
    JP .hundreds
.no_hundreds:
    LD B, A
    LD A, C
    OR A
    JP Z, .skip_hundreds
    ADD A, '0'
    PUSH BC
    LD D, 1
    LD E, 5
    PUSH DE
    CALL draw_font_char
    POP DE
    POP BC
    INC E
    JP .do_tens
.skip_hundreds:
    LD E, 5

.do_tens:
    LD A, B
    LD C, 0
.tens:
    CP 10
    JP C, .no_tens
    SUB 10
    INC C
    JP .tens
.no_tens:
    LD B, A
    LD A, C
    ADD A, '0'
    PUSH BC
    LD D, 1
    PUSH DE
    CALL draw_font_char
    POP DE
    POP BC
    INC E

    ; Ones
    LD A, B
    ADD A, '0'
    LD D, 1
    PUSH DE
    CALL draw_font_char
    POP DE
    INC E
    INC E                           ; gap

    ; Board checksum
    PUSH DE
    LD HL, board
    LD B, 0
    LD C, 128
.cksum:
    LD A, (HL)
    ADD A, B
    LD B, A
    INC HL
    DEC C
    JP NZ, .cksum
    LD A, B
    LD (cksum_val), A
    POP DE

    LD HL, str_ck
    LD D, 1
    PUSH DE
    CALL draw_string
    POP DE
    LD A, E
    ADD A, 3
    LD E, A

    LD A, (cksum_val)
    PUSH AF
    RRCA
    RRCA
    RRCA
    RRCA
    AND $0F
    CALL .hex_digit
    LD D, 1
    PUSH DE
    CALL draw_font_char
    POP DE
    INC E

    POP AF
    AND $0F
    CALL .hex_digit
    LD D, 1
    PUSH DE
    CALL draw_font_char
    POP DE

    ; --- Display move list ---
    ; Calculate start index for this page (20 per page)
    LD A, (gen_page)
    LD L, A
    LD H, 0
    ; *20: *4 + *16
    ADD HL, HL                      ; *2
    ADD HL, HL                      ; *4
    LD D, H
    LD E, L
    ADD HL, HL                      ; *8
    ADD HL, HL                      ; *16
    ADD HL, DE                      ; *20
    LD A, L
    LD (gen_display_idx), A

    ; Check if start index is past end of moves
    LD B, A
    LD A, (move_list_count)
    CP B
    JP Z, .reset_page
    JP C, .reset_page
    JP .show_moves

.reset_page:
    XOR A
    LD (gen_page), A
    LD (gen_display_idx), A

.show_moves:
    ; Display page header "Pn:" on row 2
    LD D, 2
    LD E, 1
    LD A, 'P'
    PUSH DE
    CALL draw_font_char
    POP DE
    INC E
    LD A, (gen_page)
    ADD A, '1'
    PUSH DE
    CALL draw_font_char
    POP DE
    INC E
    LD A, ':'
    PUSH DE
    CALL draw_font_char
    POP DE

    ; Display up to 20 moves in 5 rows x 4 cols
    LD A, 0
    LD (gen_display_count), A

.show_loop:
    ; Check if done 20 on this page
    LD A, (gen_display_count)
    CP 20
    JP NC, .show_done

    ; Check if past end of move list
    LD A, (gen_display_idx)
    LD B, A
    LD A, (move_list_count)
    CP B
    JP Z, .show_done
    JP C, .show_done

    ; Calculate screen position
    ; Row = 2 + (gen_display_count / 4)
    ; Col = 5 + (gen_display_count AND 3) * 7
    LD A, (gen_display_count)
    RRCA
    RRCA
    AND $3F                         ; Proper divide by 4
    ADD A, 2
    LD D, A

    LD A, (gen_display_count)
    AND 3
    ; Multiply by 7
    LD B, A
    ADD A, A                        ; *2
    ADD A, A                        ; *4
    ADD A, A                        ; *8
    SUB B                           ; *7
    ADD A, 5
    LD E, A

    ; Get move from list
    LD A, (gen_display_idx)
    LD L, A
    LD H, 0
    ADD HL, HL                      ; *2 bytes per move
    PUSH DE
    LD DE, move_list
    ADD HL, DE
    POP DE

    ; Read from and to squares
    LD A, (HL)
    LD (gen_temp_from), A
    INC HL
    LD A, (HL)
    LD (gen_temp_to), A

    ; Draw from file
    LD A, (gen_temp_from)
    AND $07
    ADD A, 'a'
    PUSH DE
    CALL draw_font_char
    POP DE
    INC E

    ; Draw from rank
    LD A, (gen_temp_from)
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

    ; Draw separator - check if capture
    LD A, (gen_temp_to)
    LD HL, board
    PUSH DE
    LD E, A
    LD D, 0
    ADD HL, DE
    POP DE
    LD A, (HL)
    OR A
    JP NZ, .gen_is_capture

    ; Check EP capture
    LD A, (gen_temp_to)
    LD B, A
    LD A, (ep_square)
    CP B
    JP Z, .gen_check_ep_pawn
    JP .gen_not_capture

.gen_check_ep_pawn:
    LD A, (gen_temp_from)
    LD HL, board
    PUSH DE
    LD E, A
    LD D, 0
    ADD HL, DE
    POP DE
    LD A, (HL)
    AND PIECE_MASK
    CP 1
    JP Z, .gen_is_capture

.gen_not_capture:
    LD A, '-'
    JP .gen_draw_sep
.gen_is_capture:
    LD A, 'x'
.gen_draw_sep:
    PUSH DE
    CALL draw_font_char
    POP DE
    INC E

    ; Draw to file
    LD A, (gen_temp_to)
    AND $07
    ADD A, 'a'
    PUSH DE
    CALL draw_font_char
    POP DE
    INC E

    ; Draw to rank
    LD A, (gen_temp_to)
    RRCA
    RRCA
    RRCA
    RRCA
    AND $07
    ADD A, '1'
    PUSH DE
    CALL draw_font_char
    POP DE

    ; Next move
    LD A, (gen_display_idx)
    INC A
    LD (gen_display_idx), A
    LD A, (gen_display_count)
    INC A
    LD (gen_display_count), A
    JP .show_loop

.show_done:
    ; Advance page for next G press
    LD A, (gen_page)
    INC A
    LD (gen_page), A
    RET

.hex_digit:
    CP 10
    JP C, .dec_digit
    ADD A, 'A' - 10
    RET
.dec_digit:
    ADD A, '0'
    RET

cksum_val:          DEFB 0
gen_page:           DEFB 0
gen_display_idx:    DEFB 0
gen_display_count:  DEFB 0
gen_temp_from:      DEFB 0
gen_temp_to:        DEFB 0

; =============================================================================
; KEY REPEAT HANDLER
; =============================================================================
; Input: A = key ID
; Output: Z = block this keypress, NZ = allow this keypress
; =============================================================================
check_repeat:
    LD B, A
    LD A, (last_key)
    CP B
    JP NZ, .new_key

    ; Same key still held
    LD A, (key_timer)
    OR A
    JP Z, .timer_expired

    ; Timer not yet expired - decrement and block
    DEC A
    LD (key_timer), A
    XOR A                           ; Set Z flag = block
    RET

.timer_expired:
    ; Allow repeat and reset timer
    LD A, KEY_REPEAT_SPEED
    LD (key_timer), A
    LD A, 1
    OR A                            ; Set NZ = allow
    RET

.new_key:
    ; First press of new key - allow and set initial delay
    LD A, B
    LD (last_key), A
    LD A, KEY_REPEAT_INITIAL
    LD (key_timer), A
    LD A, 1
    LD (key_held), A
    OR A                            ; Set NZ = allow
    RET