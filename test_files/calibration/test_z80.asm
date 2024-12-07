//===========================================================================
//
//  TEST_Z80.ASM
//
//  Test the Z80 instruction set in full and all of the XA80 commands and
//  functions.
//
//  Duncan Munro  04/06/2023
//
//===========================================================================

;
; Some defines for testing
;


;----------------------------------------------------------------------------
;
; Taken from the Opcode map
;
;----------------------------------------------------------------------------

U8		EQU		5				// 8 bit unsigned value
U16		EQU		5555			// 16 bit unsigned value
DISPL	EQU		57				// Displacement value

		INCLUDE	"test_z80.inc"
				


;----------------------------------------------------------------------------
;
; Test macro definition, expansion and nesting
;
;----------------------------------------------------------------------------

; Set up a simple macro

NOP3		MACRO
			NOP
			NOP
			NOP
			ENDM
			
; Set up a nested macro with parameters
			
DELAY		MACRO 	CYCLES
			LD		A,{CYCLES}
			OR		A,A
			JR		Z,DELAYX{#}
 			LD		B,A
DELAY{#}:	NOP3
			DJNZ	DELAY{#}
DELAYX{#}:
			ENDM
			
; Now invoke the macro

			DELAY	123
			DELAY	19
			
			
;----------------------------------------------------------------------------
;
; Z80 / Z180 specific stuff
;
;----------------------------------------------------------------------------

; Test indirection

		LD 		A,(200+100)  	// $3A $2C $01 Indirection
		LD 		A,(1+2)*(3+4)  	// $3E $15     No indirection 

			
			
;----------------------------------------------------------------------------
;
; Test expressions
;
;----------------------------------------------------------------------------

		INCLUDE	"test_shared.inc"
		
		END