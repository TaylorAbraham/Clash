;TODO: Why doesn't push/pop projectile motion work?
;TODO: asm syntax highlighting?
;TODO: Normalize capitalization

.include "Scripts/header.inc"
.include "Scripts/snes_init.asm"

;TODO: Does EQU exist in 65c816?
;MAP_WIDTH EQU $8
;MAP_HEIGHT EQU $4

;-------------------;
; CONVERSION MACROS ;
;-------------------;

.macro ConvertX
; Data in: our coord in A
; Data out: SNES scroll data in C (the 16 bit A)
.rept 5
asl a			; multiply A by 32
.endr
rep #%00100000	; 16 bit A
eor #$FFFF		; this will do A=1-A
inc a			; A=A+1
sep #%00100000	; 8 bit A
.endm

.macro ConvertY
; Data in: our coord in A
; Data out: SNES scroll data in C (the 16 bit A)
.rept 5
asl a			; multiply A by 32
.endr
rep #%00100000	; 16 bit A
eor #$FFFF		; this will do A=1-A
sep #%00100000	; 8 bit A
.endm

;-------------------------;
; Random Number Generator ;
;-------------------------;

.macro RNG
PSHA		; Preserve A
LDA $102	; Load last random number
; TODO: Implement XORShift to generate new random number
STA $102	; Store new random number
PULA		; Restore A
.endm

;--------------------------------------
.bank 0 slot 0
.org 0
.section "Vblank"

;------------------;
;      VBLANK      ;
;------------------;

VBlank:
LDA $4212		; Get joypad status
AND #%00000001	; If joypad is not ready
BNE VBlank		; Wait



;-------------------;
; PROJECTILE MOTION ;
;-------------------;

LDA $104		; Load frequency counter into A
; Increment A and store it back
INA
STA $104
CMP #$A			; Run every 10 times
BNE endProjectile

; Reset counter
LDA #$0
STA $104

; Now start the projectile motion calculation
LDY #$4	; Clear Y, used as a counter for Y
--
DEY
LDX #$8	; Clear X, used as a counter for X
-
DEX
TYA			; A now holds simulated Y value
STA $0202	; Store Y value into $0202 to be used for multiplication
CLC
; Simulate a multiply by 8
.rept 7
ADC $0202	; A*8 = A+A+A+...
.endr
; A now holds Y*8
STX $0202	; Store X (X counter) into $0202
ADC $0202	; Add X value to A
TAX
; X now holds offset of potential X
LDA $0,x
CMP #$8
BNE +++
STY $204	; Preserve Y before loading blank tile
LDY #$0		; Load blank tile into Y
STY $0,x	; Put blank tile into current X
LDY $202	; Y now holds X counter for comparison
CPY #$7		; If end of line, don't move X to next row
BEQ ++
LDY $1,x 	; Y now holds tile next to X
CPY #$6		; If an enemy is 1 over, X is about to hit it
BNE +
; TODO: RNG call causes duplicate label error on pass 1
; RNG		; 1 space is required before macros
; TODO: RNG logic to determine if enemy should drop loot
+
STA $1,x	; Move X over by 1
++
LDY $204	; Restore Y
+++
LDX $0202
CPX #$0	; Loop 8 times
BNE -
CPY #$0	; Loop 4 times
BNE --

endProjectile:

;----------------;
; ENEMY MOVEMENT ;
;----------------;
;TODO enemy movement part of projectile motion?


;--------------;
;ENEMY SPAWNING;
;--------------;

LDA $105		; Load enemy frequency counter into A
; Increment A and store it back
INA
STA $105
CMP #$FF			; Run every 255 times
BNE +

; Reset counter
LDA #$0
STA $105

; Fetch random number (column to place enemy)
 RNG		; 1 space is required before macros
LDA $102

CLC
.REPT 7
ADC $102	; A = A*8, for column/Y offset
.ENDR
ADC #$7		; Add X offset of 7 to place enemy at right edge
TAX
LDA #$6		; Load an 'O' tile into A
STA $0,x

+

;--------------------;
;JOYPAD BUTTON CHECKS;
;--------------------;

lda $4219		; read joypad input (BYSTudlr)
sta $0201		; store input
cmp $0200		; compare input with the previous input
bne +			; did input change from last check?
rti				; button didn't change, so return
+
sta $0200		; store as previous joystick input

cmp #%10000000  ; B pressed?
bne +			; if not B, jump
; B has been pressed
; do stuff
; write an X at player position, this will be a projectile
lda $0101	; Get player Y
sta $0202	; Store it to a temporary variable
; A*3
clc
.rept 7
adc $0202
.endr
; A has now been multiplied by 3
adc $0100	; Add player X
; A contains address to write to
ldx #$0000	; Clear register X
tax			; Transfer A to X
lda #$08	; Load an 'X' into register A
sta $0000,x; Store 'X' into the address held in X

+

;----------------------;
;JOYPAD MOVEMENT CHECKS;
;----------------------;

lda $0201		; get joypad input
and #%00001111	; AND input with control stick inputs
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
cmp #$06		; player at bottom of map?
beq +			; if at bottom, jump
inc $0101		; move player 1 down

+
lda $0201 		; get stored joypad input
cmp #%00000010  ; left?
bne +			; if not left, jump
lda $0100		; get player x-value
cmp #$00		; player at left of map?
beq +			; if at far left, jump
dec $0100		; move player 1 left

+
lda $0201 		; get stored joypad input
cmp #%00000001  ; right?
bne +			; if not right, jump
lda $0100		; get player x-value
cmp #$07		; player at left of map?
beq +			; if at far right, jump
inc $0100		; move player 1 right

+
rti 			; done joypad input checks
.ends			; done VBlank
;--------------------------------------




.bank 0 slot 0
.org 0
.section "Main"
;--------------------------------------
;--------------;
;INITIALIZATION;
;--------------;

Start:
 Snes_Init
rep #%00010000	;16 bit xy
sep #%00100000	;8 bit ab

; Make sure global and state variables start cleared
ldx #$0000
stx $102
ldx #$0000
stx $104
ldx #$0000
stx $105

- lda UntitledPalette.l,x
sta $2122
inx
cpx #8
bne -

;We'll have two palettes, only one color is needed for the second:
lda #33		;The color we need is the 33rd
sta $2121
lda.l Palette2
sta $2122
lda.l Palette2+1
sta $2122
ldx #UntitledData	; Address
lda #:UntitledData	; of UntitledData
ldy #(16*16*2)	; length of data
stx $4302	; write
sta $4304	; address
sty $4305	; and length
lda #%00000001	; set this mode (transferring words)
sta $4300
lda #$18	; $211[89]: VRAM data write
sta $4301	; set destination

ldy #$0000	; Write to VRAM from $0000
sty $2116

lda #%00000001	; start DMA, channel 0
sta $420B
lda #%10000000	; VRAM writing mode
sta $2115
ldx #$4000	; write to vram
stx $2116	; from $4000


ldx #$6000	; BG2 will start here
stx $2116
ldx #$000C	; And will contain 1 tile (cursor)
stx $2118
;set up the screen
lda #%00110000	; 16x16 tiles, mode 0
sta $2105	; screen mode register
lda #%01000000	; data starts from $4000
sta $2107	; for BG1
lda #%01100000	; and $6000
sta $2108	; for BG2

stz $210B	; BG1 and BG2 use the $0000 tiles

lda #%00000011	; enable bg1&2
sta $212C

;The PPU doesn't process the top line, so we scroll down 1 line.
rep #$20	; 16bit a
lda #$07FF	; this is -1 for BG1
sep #$20	; 8bit a
sta $210F	; BG1 vert scroll
xba
sta $210F

rep #$20	; 16bit a
lda #$FFFF	; this is -1 for BG2
sep #$20	; 8bit a
sta $2110	; BG2 vert scroll
xba
sta $2110

lda #%00001111	; enable screen, set brightness to 15
sta $2100

lda #%10000001	; enable NMI and joypads
sta $4200

forever:
wai
rep #%00100000	; get 16 bit A
lda #$0000		; empty it
sep #%00100000	; 8 bit A
lda $0100		; get our X coord
 ConvertX		; WLA needs a space before a macro name
sta $210F		; BG2 horz scroll
xba
sta $210F		; write 16 bits

;now repeat it, but change $0100 to $0101, and $210F to $2110
rep #%00100000	; get 16 bit A
lda #$0000		; empty it
sep #%00100000	; 8 bit A
lda $0101		; get our Y coord
 ConvertY		; WLA needs a space before a macro name
sta $2110		; BG2 vert scroll
xba
sta $2110		; write 16 bits

;--------------------------------------
ldx #$0000		; reset our counter
-
rep #%00100000	; 16 bit A
lda #$0000		; empty it
sep #%00100000	; 8 bit a
lda VRAMtable.l,x	; this is a long indexed address, nice :)
rep #%00100000
clc
adc #$4000		; add $4000 to the value
sta $2116		; write to VRAM from here
lda #$0000		; reset A while it's still 16 bit
sep #%00100000	; 8 bit A
lda $0000,x		; get the corresponding tile from RAM
; VRAM data write mode is still %10000000
sta $2118		; write
stz $2119		; this is the hi-byte
inx
cpx #$28		; finished?
bne -			; no, go back
jmp forever

;--------------------------------------
.ends

.bank 1 slot 0		; We'll use bank 1
.org 0
.section "Tiledata"
.include "Scripts/tiles.inc"	; If you are using your own tiles, replace this
.ends

.bank 2 slot 0
.org 0
.section "Conversiontable"
VRAMtable:
.db $00,$02,$04,$06,$08,$0A,$0C,$0E
.db $40,$42,$44,$46,$48,$4A,$4C,$4E
.db $80,$82,$84,$86,$88,$8A,$8C,$8E
.db $C0,$C2,$C4,$C6,$C8,$CA,$CC,$CE
.ends
