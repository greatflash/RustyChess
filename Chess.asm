; =============================================================================
; RUSTY CHESS - ZX Spectrum Next Chess Engine
; =============================================================================
;
; Assembler: sjasmplus
; Build:     sjasmplus chess.asm
; Output:    chess.nex
;
; Controls:  Q=Up  A=Down  O=Left  P=Right  SPACE=Select  X=Quit
;
; =============================================================================

    DEVICE ZXSPECTRUMNEXT
    ORG $5C00

    INCLUDE "constants.asm"
    INCLUDE "main.asm"
    INCLUDE "board.asm"
    INCLUDE "display.asm"
    INCLUDE "input.asm"
    INCLUDE "validate.asm"
    INCLUDE "attack.asm"
    INCLUDE "movegen.asm"
    INCLUDE "history.asm"
    INCLUDE "font.asm"
    INCLUDE "strings.asm"
    INCLUDE "variables.asm"
    INCLUDE "eval.asm" 
    INCLUDE "search.asm"
; =============================================================================
; SIZE REPORT
; =============================================================================
code_end:

CODE_SIZE   EQU code_end - $5C00
MAX_SIZE    EQU $BFFF - $5C00 + 1
FREE_SPACE  EQU MAX_SIZE - CODE_SIZE

    DISPLAY "-------------------------------"
    DISPLAY "RUSTY CHESS - Build Report"
    DISPLAY "-------------------------------"
    DISPLAY "Start:      $", /H, $5C00
    DISPLAY "End:        $", /H, code_end
    DISPLAY "Code size:  ", /D, CODE_SIZE, " bytes"
    DISPLAY "Max size:   ", /D, MAX_SIZE, " bytes"
    DISPLAY "Free space: ", /D, FREE_SPACE, " bytes"
    DISPLAY "Used:       ", /D, (CODE_SIZE * 100) / MAX_SIZE, "%"
    DISPLAY "-------------------------------"

    ASSERT code_end <= $C000, "ERROR: Code exceeds available space!"

    SAVENEX OPEN "chess.nex", start, $FF40
    SAVENEX CORE 3, 1, 5
    SAVENEX CFG 1, 0, 0, 0
    SAVENEX AUTO
    SAVENEX CLOSE
    CSPECTMAP "Game.map"
    END start