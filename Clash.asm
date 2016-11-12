.include "Header.inc"
.include "Snes_Init.asm"
VBlank:
	RTI

Start:
Snes_Init
sep		#$20
lda		#%10000000 
sta		$2100
lda		#%11100000
sta		$2122
lda		#%00000000
sta		$2122
lda		#%00001111
sta		$2100

Forever:
	jmp Forever