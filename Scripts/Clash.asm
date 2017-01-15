.include "Scripts/header.inc"
.include "Scripts/snes_init.asm"

.macro ConvertX
; Data in: our coord in A
; Data out: SNES scroll data in C (the 16 bit A)
.rept 5
asl a		; multiply A by 32
.endr
rep #%00100000	; 16 bit A
eor #$FFFF	; this will do A=1-A
inc a		; A=A+1
sep #%00100000	; 8 bit A
.endm

.macro ConvertY
; Data in: our coord in A
; Data out: SNES scroll data in C (the 16 bit A)
.rept 5
asl a		; multiply A by 32
.endr
rep #%00100000	; 16 bit A
eor #$FFFF	; this will do A=1-A
sep #%00100000	; 8 bit A
.endm

.bank 0 slot 0
.org 0
.section "Vblank"
;--------------------------------------
;;;;;;;;
;VBLANK;
;;;;;;;;

VBlank:
lda $4212		; get joypad status
and #%00000001	; if joypad is not ready
bne VBlank		; wait
lda $4219		; read joypad input (BYSTudlr)
sta $0201		; store input
cmp $0200		; compare input with the previous input
bne +			; did input change from last check?
rti				; button didn't change, so return

;;;;;;;;;;;;;;;;;;;;;;
;JOYPAD BUTTON CHECKS;
;;;;;;;;;;;;;;;;;;;;;;
+
sta $0200		; store as previous joystick input
cmp #%10000000  ; B pressed?
bne +			; if not B, jump
; B has been pressed
; do stuff


;;;;;;;;;;;;;;;;;;;;;;;;
;JOYPAD MOVEMENT CHECKS;
;;;;;;;;;;;;;;;;;;;;;;;;
+
lda $0201		; get joypad input
and #$00001111	; AND input with control stick inputs
sta $0201		; store the AND result

; NOTE: (0,0) is top-left of map

cmp #%00001000  ; up?
bne +			; if not up, jump
lda $0101		; get player y-value
cmp #$00		; player at top of map?
beq +			; if at top, jump
dec $0101		; move player 1 up

+
lda $0201 		; get stored joypad input
cmp #%00000100  ; down?
bne +			; if not down, jump
lda $0101		; get player y-value
cmp MAP_HEIGHT	; player at bottom of map?
beq +			; if at bottom, jump
inc $0101		; move player 1 down

+
lda $0201 		; get stored joypad input
cmp #%00000010  ; left?
bne +			; if not left, jump
lda $0100		; get player x-value
cmp #$00		; player at left of map?
beq +			; if at far left, jump
dec $0101		; move player 1 left

+
lda $0201 		; get stored joypad input
cmp #%00000001  ; right?
bne +			; if not right, jump
lda $0100		; get player x-value
cmp #$MAP_WIDTH	; player at left of map?
beq +			; if at far right, jump
dec $0101		; move player 1 right

+
rti 			; done joypad input checks
.ends			; done VBlank