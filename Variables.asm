; =============================================================================
; RUSTY CHESS - Runtime Variables
; =============================================================================

board:          DEFS BOARD_SIZE, 0
cursor_sq:      DEFB 0
selected_sq:    DEFB $FF
side_to_move:   DEFB 0
white_king_sq:  DEFB SQ_E1
black_king_sq:  DEFB SQ_E8
move_from:      DEFB 0
move_to:        DEFB 0
moving_piece:   DEFB 0
dest_piece:     DEFB 0
quit_flag:      DEFB 0
illegal_flash:  DEFB 0
in_check:       DEFB 0
game_over:      DEFB 0
promoting:      DEFB 0
temp_char:      DEFB 0

; Castling
castling_rights:    DEFB CASTLE_ALL

; En passant
ep_square:      DEFB EP_NONE

; Saved state for check validation undo
save_from_piece:    DEFB 0
save_to_piece:      DEFB 0
save_wking:         DEFB 0
save_bking:         DEFB 0
save_castling:      DEFB 0
save_ep:            DEFB 0

; Castling undo state
did_castle:         DEFB 0
save_castle_rook:   DEFB 0

; En passant undo state
did_ep_capture:     DEFB 0
ep_capture_sq:      DEFB 0
save_ep_captured:   DEFB 0

; Promotion undo state
did_promote:        DEFB 0

; Key repeat state
last_key:       DEFB $FF
key_held:       DEFB 0
key_timer:      DEFB 0

; =============================================================================
; MOVE HISTORY
; =============================================================================
; Each entry: 3 bytes (from_sq, to_sq, capture_flag)
move_count:     DEFB 0
hist_entries:   DEFB 0

; History buffer: HIST_MAX entries x 3 bytes each
hist_buffer:    DEFS HIST_MAX * 3, 0

; =============================================================================
; CAPTURED PIECES
; =============================================================================
capt_by_white_count:    DEFB 0
capt_by_white:          DEFS CAPT_MAX, 0

capt_by_black_count:    DEFB 0
capt_by_black:          DEFS CAPT_MAX, 0