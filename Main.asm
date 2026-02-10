; =============================================================================
; RUSTY CHESS - Main Entry Point and Game Loop
; =============================================================================

start:
    DI
    LD SP, $FF40
    nextreg $07,%11
    CALL reset_game

    LD A, 1
    OUT ($FE), A

    CALL cls_screen
    CALL draw_labels
    CALL draw_help

    EI

; =============================================================================
; MAIN LOOP
; =============================================================================
main_loop:
    CALL draw_board
    CALL draw_cursor
    CALL draw_check_highlight
    CALL draw_status
    CALL draw_history
    CALL draw_captures

    ; Handle illegal move flash
    LD A, (illegal_flash)
    OR A
    JP Z, .no_flash
    CALL show_illegal
    XOR A
    LD (illegal_flash), A
.no_flash:

    display game_over
    ; Game over?
    LD A, (game_over)
    OR A
    JP NZ, game_over_loop

    ; Is it AI's turn? (black = 8)
    LD A, (side_to_move)
    CP BLACK
    JP Z, .ai_turn

    HALT
    CALL read_keys

    LD A, (quit_flag)
    OR A
    JP Z, main_loop

    XOR A
    OUT ($FE), A
    RET

.ai_turn:
    ; Show "Thinking..." status
    LD D, 17
    LD E, 0
    LD B, 32
.clear_think:
    PUSH BC
    PUSH DE
    LD A, ' '
    CALL draw_font_char
    POP DE
    POP BC
    INC E
    DJNZ .clear_think

    LD HL, str_thinking
    LD D, 17
    LD E, 0
    CALL draw_string

    ; Disable interrupts for AI search
    DI

    CALL ai_move

    ; Re-enable interrupts
    EI

    JP main_loop

; =============================================================================
; GAME OVER LOOP
; =============================================================================
game_over_loop:
    CALL draw_status

    HALT

    ; X = quit
    LD BC, PORT_SHIFT
    IN A, (C)
    BIT 2, A
    JP NZ, .not_quit
    XOR A
    OUT ($FE), A
    RET

.not_quit:
    ; R = restart
    LD BC, PORT_QWERT
    IN A, (C)
    BIT 3, A
    JP NZ, game_over_loop

.wait_release:
    HALT
    LD BC, PORT_QWERT
    IN A, (C)
    BIT 3, A
    JP Z, .wait_release

    CALL reset_game
    CALL cls_screen
    CALL draw_labels
    CALL draw_help

    JP main_loop

; =============================================================================
; RESET GAME STATE
; =============================================================================
reset_game:
    XOR A
    LD (quit_flag), A
    LD (side_to_move), A
    LD (illegal_flash), A
    LD (key_held), A
    LD (key_timer), A
    LD (in_check), A
    LD (game_over), A
    LD (promoting), A

    LD A, $FF
    LD (selected_sq), A
    LD (last_key), A

    LD A, EP_NONE
    LD (ep_square), A

    LD A, CASTLE_ALL
    LD (castling_rights), A

    LD A, $14                       ; e2
    LD (cursor_sq), A

    CALL init_board
    CALL clear_history
    RET

; =============================================================================
; LOAD TEST POSITION
; =============================================================================
load_test_position:
    ; Clear board
    LD HL, board
    LD DE, board + 1
    LD BC, BOARD_SIZE - 1
    LD (HL), EMPTY
    LDIR

    ; White pieces
    LD A, W_ROOK
    LD (board + SQ_A1), A
    LD A, W_KING
    LD (board + SQ_E1), A
    LD A, W_ROOK
    LD (board + SQ_H1), A
    LD A, W_PAWN
    LD (board + $11), A             ; b2
    LD A, W_PAWN
    LD (board + $17), A             ; h2
    LD A, W_PAWN
    LD (board + $45), A             ; f5

    ; White pawn on a7 for promotion test
    LD A, W_PAWN
    LD (board + $60), A             ; a7

    ; Black pieces
    LD A, B_ROOK
    LD (board + SQ_A8), A
    LD A, B_KING
    LD (board + SQ_E8), A
    LD A, B_ROOK
    LD (board + SQ_H8), A
    LD A, B_PAWN
    LD (board + $61), A             ; b7
    LD A, B_PAWN
    LD (board + $67), A             ; h7
    LD A, B_PAWN
    LD (board + $44), A             ; e5

    ; Black pawn on a2 for promotion test
    LD A, B_PAWN
    LD (board + $10), A             ; a2

    ; King positions
    LD A, SQ_E1
    LD (white_king_sq), A
    LD A, SQ_E8
    LD (black_king_sq), A

    LD A, CASTLE_ALL
    LD (castling_rights), A

    LD A, $54                       ; e6
    LD (ep_square), A

    XOR A
    LD (side_to_move), A
    LD (game_over), A
    LD (in_check), A
    LD (promoting), A

    LD A, $FF
    LD (selected_sq), A

    LD A, $14
    LD (cursor_sq), A

    CALL clear_history
    RET