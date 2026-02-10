; =============================================================================
; RUSTY CHESS - Constants and Definitions
; =============================================================================

; --- Piece Definitions ---
EMPTY           EQU 0
W_PAWN          EQU 1
W_KNIGHT        EQU 2
W_BISHOP        EQU 3
W_ROOK          EQU 4
W_QUEEN         EQU 5
W_KING          EQU 6
B_PAWN          EQU 9
B_KNIGHT        EQU 10
B_BISHOP        EQU 11
B_ROOK          EQU 12
B_QUEEN         EQU 13
B_KING          EQU 14

COLOR_MASK      EQU $08
PIECE_MASK      EQU $07
WHITE           EQU 0
BLACK           EQU 8

; --- 0x88 Board ---
BOARD_SIZE      EQU 128
RANK_OFFSET     EQU 16
OFF_BOARD       EQU $88

; --- Key Squares ---
SQ_A1           EQU $00
SQ_B1           EQU $01
SQ_C1           EQU $02
SQ_D1           EQU $03
SQ_E1           EQU $04
SQ_F1           EQU $05
SQ_G1           EQU $06
SQ_H1           EQU $07
SQ_A8           EQU $70
SQ_B8           EQU $71
SQ_C8           EQU $72
SQ_D8           EQU $73
SQ_E8           EQU $74
SQ_F8           EQU $75
SQ_G8           EQU $76
SQ_H8           EQU $77

; --- Castling Flags (bit mask) ---
CASTLE_WK       EQU %00000001
CASTLE_WQ       EQU %00000010
CASTLE_BK       EQU %00000100
CASTLE_BQ       EQU %00001000
CASTLE_W_ALL    EQU %00000011
CASTLE_B_ALL    EQU %00001100
CASTLE_ALL      EQU %00001111

; --- Screen ---
SCREEN_PIXELS   EQU $4000
SCREEN_ATTR     EQU $5800
BOARD_X         EQU 12
BOARD_Y         EQU 8

; --- History / Captures display ---
HIST_X          EQU 22             ; Column for move history
HIST_Y          EQU 8              ; Starting row for move history
HIST_MAX        EQU 8              ; Max moves shown

CAPT_W_X        EQU 1              ; Column for white's captured pieces
CAPT_W_Y        EQU 8              ; Row for white's captures (pieces white took)
CAPT_B_X        EQU 1              ; Column for black's captured pieces
CAPT_B_Y        EQU 12             ; Row for black's captures (pieces black took)
CAPT_MAX        EQU 16             ; Max captured pieces per side

; --- Attributes ---
LIGHT_PAPER     EQU %01110000
DARK_PAPER      EQU %00100000

INK_WHITE       EQU 1              ; Blue ink for white pieces
INK_BLACK       EQU 0              ; Black ink for black pieces

CURSOR_ATTR     EQU %11110000
SELECT_ATTR     EQU %01100000
ILLEGAL_ATTR    EQU %01010000
CHECK_ATTR      EQU %01010010

; --- Keyboard Ports ---
PORT_QWERT      EQU $FBFE
PORT_ASDFG      EQU $FDFE
PORT_POIUY      EQU $DFFE
PORT_SPACE      EQU $7FFE
PORT_ENTER      EQU $BFFE
PORT_SHIFT      EQU $FEFE

; --- Input Timing ---
KEY_REPEAT_INITIAL EQU 8
KEY_REPEAT_SPEED   EQU 3

; --- En Passant ---
EP_NONE         EQU $FF