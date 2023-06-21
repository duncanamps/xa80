			IF DEFINED(__DMFPBASE_INC)
				LISTOFF
			ELSE
				LISTON
				__DMFPBASE_INC = 1

;
; DMFPBASE.INC - Duncan's base floating point routines for Z80
;
; Uses IEEE 754 technical standard
;
; Floating point format (4 bytes, IEEE 754 single precision)
;
;	FPACC+0  FPACC+1  FPACC+2  FPACC+3
;	SEEEEEEE EMMMMMMM MMMMMMMM MMMMMMMM
;
; S = Sign bit
;	0 = positive
;	1 = negative
;
; E = Exponent (8 bits with bias of 0x7f)
;	0x7f = 2 ^ 0
;	0x80 = 2 ^ 1 etc.
;	0x00 = special case for zero (MMMM is zero too)
;	0xff = infinity (MMMM is zero too)
;	  
; MMMM = mantissa as 24 bits with an implied 1 at the start
; for normalised mantissas (this being overwritten by the lowest
; bit of the exponent).
;
; Examples
;
;	00111111 10000000 00000000 00000000 = 1.000000
;	01000000 00101101 11111000 01010100 = e
;
; The mantissa will always be normalised so that it is in
; the range 1 <= mmmm < 2 (except zero).



			INCLUDE "dmfpmacro.inc"

			
;----------------------------------------------------------------------
;
; FPADD
;
; Add FPACC1 to FPACC2 and store the result in FPACC1.
;
; Calling:
;	FPACC1	holding the first value
;	FPACC2	holding the second value
;
; Results:
;	FPACC1	Result of addition
;
; Destroyed:
;	FPACC2
;	A	
;
; Note, there are 4 quadrants handled here:
;	A: +FPACC1, +FPACC2		Perform add, normal answer
;	B: +FPACC1, -FPACC2		Perform subtract, normal answer
;	C: -FPACC1, +FPACC2		Swap and perform subtract
;	D: -FPACC1, -FPACC2		Perform add and negate answer
;
; This routine is also called by FPSUB (subtract) which simply
; negates FPACC2 and performs an add.
;
;----------------------------------------------------------------------

FPADD
			LD		A,(FPACC1)		; Check if FPACC1 is zero
			OR		A,A				; Set flags
			JP		Z,FPSWAP		; Swap FPACC1 with FPACC2 and exit
			LD		A,(FPACC2)		; Check if FPACC2 is zero
			OR		A,A				; Set flags
			JR		Z,FPADD_BX		; Just exit if so
			; Find out which quadrant A-D we are in
			LD		A,(FPACC1)		; Get sign/exponent
			RL		A				; Rotate sign into carry
			JR		C,FPADD_CD		; Check for C and D
			; Must be in A or B, find out which
			LD		A,(FPACC2)		; Get sign/exponent
			RL		A				; Rotate sign into carry
			JR		C,FPADD_B		; Must be B if neg

			; Quadrant A: perform straight add of FPACC1 and FPACC2		
FPADD_A
			CALL	FPDEBMSG
			DB		"FPADD b.denorm $"
			CALL	FPDEBUG
			CALL	FPDENORMALISEA	; Denormalise FPACC1/2 and align them
			CALL	FPDEBMSG
			DB		"FPADD a.denorm $"
			CALL	FPDEBUG
			CALL	FPADD12DN		; Add the denormalised values together
			CALL	FPDEBMSG
			DB		"FPADD a.add12n $"
			CALL	FPDEBUG
			CALL	FPDEBREG
			JR		NC,FPADD_A2		; Skip if no carry
			CALL	FPSHIFT1R		; Overflow situation, deal with it
FPADD_A2	JP		FPNORMALISE1	; Normalise FPACC1 and exit

			; Quadrant B: Negate FPACC2 then subtract from FPACC1
FPADD_B
			CALL	FPNEG2			; Negate FPACC2 to make it positive
			CALL	FPDENORMALISEA	; Denormalise FPACC1/2 and align them
			CALL	FPSUB12DN		; Subtract mantissa of FPACC2 from FPACC1
			JR		NC,FPADD_B2		; We didn't overflow into negative @@@@@
			CALL	FPNOT1DN		; Take the logical not of the 24 bit value
			SCF						; Set carry to indicate negative result
FPADD_B2
			PUSH	AF				; Save flags
			CALL	FPNORMALISE1	; Normalise FPACC1
			POP		AF				; Restore flags, carry = negative
			JR		NC,FPADD_BX		; Skip if positive result
			CALL	FPNEG1			; Negate FPACC1 if not
FPADD_BX	RET						; Finally, exit

			; Quadrant C/D, find out which
FPADD_CD
			LD		A,(FPACC2)		; Get sign/exponent
			RL		A				; Rotate sign into carry
			JR		C,FPADD_D		; Must be D if neg
	
			; Quadrant C, swap then subtract FPACC1 from FPACC2
FPADD_C
			CALL	FPSWAP			; Swap FPACC1 and FPACC2
			JR		FPADD_B			; and handle as quadrant B
	
			; Quadrant D, negate both, add together and then negate result	
FPADD_D
			CALL	FPNEG1			; Negate FPACC1
			CALL	FPNEG2			; Negate FPACC2
			CALL	FPADD_A			; Add them together
			JP		FPNEG1			; Negate FPACC1 and exit
	

;----------------------------------------------------------------------
;
; FPCOMP
;
; Compare FPACC1 and FPACC2.
;
; Calling:
;	FPACC1	holding first value to check
;	FPACC2	holding second value to check
;
; Results:
;	Z flag	FPACC1 == FPACC2
;	C flag	FPACC1 < FPACC2
;
; Destroyed:
;	FPACC1
;	FPACC2
;	A	
;
;----------------------------------------------------------------------

FPCOMP
			CALL	FPSUB			; Subtract FPACC2 from FPACC1
			SUB		A,A				; Clear carry for now
			LD		A,(FPACC1)		; Check if result is zero
			JR		Z,FPCOMPX		; Exit if so with Z=1, C=0
			; Non-equivalent, check if remainder in FPACC1 is positive
			RLCA					; Z=0 C=negative
FPCOMPX		RET						; and return	


	

;----------------------------------------------------------------------
;
; FPDIV
;
; Divide FPACC1 by FPACC2 and store result in FPACC1.
;
; Calling:
;	FPACC1	holding the value
;	FPACC2	holding the divisor
;
; Results:
;	FPACC1	Result of division
;
; Destroyed:
;	FPACC2
;	A	
;
; There are four quadrants handled here
;	A: +FPACC1, +FPACC2		Positive result
;	B: +FPACC1, -FPACC2		Negative result
;	C: -FPACC1, +FPACC2		Negative result
;	D: -FPACC1, -FPACC2		Positive result
;
; The algorithm works by repeatedly subtracting and shifting.
; FPACC1 contains a denormalised value (24 bit) which is continually
; subtracted from by the denormalised 24 bit value in the mantissa
; of FPACC2.
;
; The value in FPACC2 is shifted to the right after each subtract.
;
; This gives us a 24 bit subtraction on each iteration, the
; results being rolled into FPACCTMP.
;
;----------------------------------------------------------------------
	
FPDIV
			; Check sign of FPACC1/2 and handle accordingly
			; Find out if neg result
			LD		A,(FPACC1)		; Get sign/exponent of FPACC1
			PUSH	AF				; Save for now
			RL		A				; Bit 7 into C
			JP		NC,FPMULT1		; Skip if FPACC1 is positive
			CALL	FPNEG1			; Negate FPACC1 if negative
FPDIVS1		LD		A,(FPACC2)		; Get sign/exponent of FPACC2
			PUSH	AF				; Save this too
			RL		A				; Bit 7 into C
			JP		NC,FPMULTS2		; Skip if FPACC2 is positive
			CALL	FPNEG2			; Make FPACC2 positive
FPDIVS2		CALL	FPACC1Z			; Check if FPACC1 is zero
			JR		NZ,FPDIVZ2		; No, go to next bit
FPDIVZ1		POP		AF				; Pop the two AF's
			POP		AF				; we pushed earlier
			JP		FPLOAD_ZERO		; Get zero for FPACC1 as 0/anything=0
FPDIVZ2		CALL	FPACC2Z			; Check if FPACC2 is zero
			JR		Z,FPDIVZ1		; give 0 result, should be infinity
									; or an error message, so some work
									; to be done here

			; We now have two positive non-zero numbers to divide in
			; FPACC1 and FPACC2.  The two 24 bit mantissas are divided
			; the result left in FPACCTMP as a 24 bit value.

			CALL	FPDENORMALISE1	; Denormalise FPACC1	
			CALL	FPDENORMALISE2	; Denormalise FPACC2
			CALL	FPCLRTMP		; Clear the results area
			
			; Main divide loop.  Subtract 24 bits stored in FPACC2
			; from 24 bits stored in the mantissa of FPACC1.
			; The result is rolled into FPACCTMP.

			LD		B,24			; 24 iterations
FPDIVLOOP
			PUSH	BC				; Save loop counter
			CALL	FPSUB12DN		; Subtract mantissa of FPACC2 from FPACC1
			CCF						; Turn C=0 into a 1 bit to roll in
			CALL	FPTMPRL			; Rotate temp area to the left (C from FPSUB12DN)
			JR		C,FPDIVLOOP2	; We managed to subtract OK
			; We were unable to subtract, add the stuff back
			CALL	FPADD12DN		; Add FPACC2 back to FPACC1
FPDIVLOOP2
			SUB		A,A				; Clear carry for rotates
			M_RRF24	FPACC2			; Spin the divisor
			POP		BC				; Restore loop counter
			DJNZ	FPDIVLOOP		; and loop back for more
			; All done, tidy up by putting FPACCTMP results into FPACC1
			; normalising, and fixing exponents
FPDIVLOOPX
			LD		A,(FPACCTMP+0)
			LD		(FPACC1+1),A
			LD		A,(FPACCTMP+1)
			LD		(FPACC1+2),A
			LD		A,(FPACCTMP+2)
			LD		(FPACC1+3),A
			LD		A,(FPEXP1)		; Get exponent 1
			LD		C,A				; Move to C for now
			LD		A,(FPEXP2)		; Get exponent 2
			ADD		A,C				; add them
			ADD		A,0x82			; Add this to subtract 0x7e
			LD		(FPEXP1),A		; Put back for now
			CALL	FPNORMALISE1	; and normalise FPACC1
			; Check if we need to negate it
			POP		BC				; Was originally a PUSH AF
			POP		AF				; Get A back from start of routine
			XOR		A,B				; XOR the negative flags
			RL		A				; Bit z into carry
			JR		NC,FPDIVX		; Exit if no negation required
			JP		FPNEG1			; Negate FPACC1 if needed and exit
FPDIVX		RET



;----------------------------------------------------------------------
;
; FPMULT
;
; Multiply FPACC1 by FPACC2 and store result in FPACC1.
;
; Calling:
;	FPACC1	holding the first value
;	FPACC2	holding the second value
;
; Results:
;	FPACC1	Result of multiplication
;
; Destroyed:
;	FPACC2
;	A	
;
; There are four quadrants handled here
;	A: +FPACC1, +FPACC2		Positive result
;	B: +FPACC1, -FPACC2		Negative result
;	C: -FPACC1, +FPACC2		Negative result
;	D: -FPACC1, -FPACC2		Positive result
;
;----------------------------------------------------------------------

FPMULT
			; Check sign of FPACC1/2 and handle accordingly
			; Find out if neg result
			LD		A,(FPACC1)		; Get sign/exponent of FPACC1
			PUSH	AF				; Save for now
			RL		A				; Bit 7 into C
			JR		NC,FPMULT1		; Skip if FPACC1 is positive
			CALL	FPNEG1			; Negate FPACC1 if negative
FPMULT1		LD		A,(FPACC2)		; Get sign/exponent of FPACC2
			PUSH	AF				; Save this too
			RL		A				; Bit 7 into C
			JR		NC,FPMULTS2		; Skip if FPACC2 is positive
			CALL	FPNEG2			; Make FPACC2 positive
FPMULTS2	CALL	FPACC1Z			; Check if FPACC1 is zero
			JR		NZ,FPMULTZ2		; No, go to next bit
FPMULTZ1	POP		AF				; Get rid of the two AF's
			POP		AF				; we pushed
			JP		FPLOAD_ZERO		; Get zero for FPACC1
FPMULTZ2	CALL	FPACC2Z			; Check if FPACC2 is zero
			JR		Z,FPMULTZ1		; Yes, result will be zero

			; We now have two positive non-zero numbers to multiply in
			; FPACC1 and FPACC2.  The two 24 bit mantissas are multiplied
			; the result left in FPACCTMP as a 48 bit value.

			CALL	FPDENORMALISE1	; Denormalise FPACC1	
			CALL	FPDENORMALISE2	; Denormalise FPACC2
			CALL	FPCLRTMP		; Clear the temporary area

			; Main multiply loop.  Rotate the answer round and add in
			; FPACC2 mantissa if the high bit of FPACC1 mantissa is set

			LD		B,24			; 24 iterations
FPMULTLOOP
			PUSH	BC				; Save loop counter
			SUB		A,A				; Make sure carry is clear
			CALL	FPTMPRL			; Rotate temporary area to the left
			LD		HL,FPACC1+3		; Point to end of mantissa
			SLA		(HL)			; Shift mantissa
			DEC		HL
			RL		(HL)
			DEC		HL
			RL		(HL)
			JR		NC,FPMULTL2		; Skip next bit if clear
			M_ADC24	FPACCTMP+3,FPACC2+1	; Add FPACC2 mantissa to FPACCTMP
			JR		NC,FPMULTL2		; Skip if there was no carry
			LD		HL,FPACCTMP+2	; Point to high 3 bytes
			INC		(HL)			; Bump if carry required
			JR		NZ,FPMULTL2		; Didn't roll over so skip following
			DEC		HL				; Rollover from FPACCTMP(2) to FPACCTMP(1)
			INC		(HL)			; Bump if carry required
			JR		NZ,FPMULTL2		; Didn't roll over so skip following
			DEC		HL				; Rollover from FPACCTMP(1) to FPACCTMP(0)
			INC		(HL)			; Bump if carry required
FPMULTL2
			POP		BC				; Restore loop counter
			DJNZ	FPMULTLOOP		; GO back for more
			; All done, tidy up by putting FPACCTMP results into FPACC1
			; normalising, and fixing exponents
			LD		A,(FPACCTMP+0)
			LD		(FPACC1+1),A
			LD		A,(FPACCTMP+1)
			LD		(FPACC1+2),A
			LD		A,(FPACCTMP+2)
			LD		(FPACC1+3),A
			LD		A,(FPEXP1)		; Get exponent 1
			LD		C,A				; Move to C for now
			LD		A,(FPEXP2)		; Get exponent 2
			ADD		A,C				; add them
			ADD		A,0x82			; Add this to subtract 0x7e
			LD		(FPEXP1),A		; Put back for now
			CALL	FPNORMALISE1	; and normalise FPACC1
			; Check if we need to negate it
			POP		BC				; Was originally a PUSH AF
			POP		AF				; Get A back from start of routine
			XOR		A,B				; XOR the negative flags
			RL		A				; Bit z into carry
			JR		NC,FPMULTX		; Exit if no negation required
			JP		FPNEG1			; Negate FPACC1 if needed and exit
FPMULTX		RET

;----------------------------------------------------------------------
;
; FPSUB
;
; Subtract FPACC2 from FPACC1 and store the result in FPACC1.
;
; Calling:
;	FPACC1	holding the first value
;	FPACC2	holding the second value
;
; Results:
;	FPACC1	Result of subtraction
;
; Destroyed:
;	FPACC2
;	A	
;
; This routine is extremely simple, it negates FPACC2 and then performs
; and add between FPACC1 and FPACC2.
;
;----------------------------------------------------------------------

FPSUB
			CALL	FPNEG2			; Make FPACC2 negative
			JP		FPADD			; and add to FPACC1


;----------------------------------------------------------------------
;
; Utility routines
;
;----------------------------------------------------------------------

; Check if FPACC1 is zero

FPACC1Z
			LD		A,(FPACC1+1)	; Get high bit of mantissa
			LD		C,A				; Save to C for now
			LD		A,(FPACC1)		; Get exponent byte
			OR		A,C				; OR to check if zero
			RET
	
	
; Check if FPACC2 is zero

FPACC2Z
			LD		A,(FPACC2+1)	; Get high bit of mantissa
			LD		C,A				; Save to C for now
			LD		A,(FPACC2)		; Get exponent byte
			OR		A,C				; OR to check if zero
			RET
			
; Add the mantissa of FPACC2 from FPACC1 and put in FPACC1
; where they are both denormalised and respecting carry out

FPADD12DN
			M_ADC24	FPACC1,FPACC2	; Do the addition
			RET						; return with carry result
	
; Align two denormalised values

FPALIGNDN
			LD		HL,FPEXP1		; Address of FPEXP1
			LD		DE,FPEXP2		; Address of FPEXP2
			LD		A,(DE)			; Get exponent of FPACC2
			SUB		A,(HL)			; and subtract exponent of FPACC1
			JR		Z,FPALIGNDNX	; Exit if identical
			JR		C,FPALIGNDN3	; If (exp)FPACC1 > (exp)FPACC2
FPALIGNDN2
			LD		A,(DE)			; Get FPACC2 exponent
			CP		A,(HL)			; Compare with FPACC1 exponent
			JR		Z,FPALIGNDNX	; If zero, we can leave
			CALL	FPSHIFT1R		; Shift FPACC1 to the right and increase exponent
			JR		FPALIGNDN2		; Keep going until they line up
FPALIGNDN3
			LD		A,(DE)			; Get FPACC2 exponent
			CP		A,(HL)			; Compare with FPACC1 exponent
			JR		Z,FPALIGNDNX	; If zero, we can leave
			CALL	FPSHIFT2R		; Shift FPACC2 to the right and increase exponent
			JR		FPALIGNDN3		; Keep going until they line up
FPALIGNDNX	RET


; Clear temporary area

FPCLRTMP
			LD		HL,0			; Zero HL
			LD		(FPACCTMP+0),HL	; Wipe the buffer
			LD		(FPACCTMP+2),HL	
			LD		(FPACCTMP+4),HL	
			RET
	
	
; Denormalise FPACC1 into a 24 bit value

FPDENORMALISE1
			LD		HL,FPACC1+1	; Point to high of mantissa / low of exp
			LD		DE,FPEXP1	; and the exponent 1 byte
FPDENORMALISESHR			
			LD		A,(HL)		; Get this byte emmmmmmm
			RL		A			; Rotate lowest bit of exp into C
			DEC		HL			; Move to exp byte
			LD		A,(HL)		; and get the partial exp seeeeeee
			RL		A			; Make it eeeeeeee
			LD		(DE),A		; Save the byte
			INC		HL			; Back to the mantissa
			LD		A,(HL)		; Get high mantissa byte again
			OR		A,%10000000	; Set high bit
			LD		(HL),A		; and save back
			RET


; Denormalise FPACC2 into a 24 bit value

FPDENORMALISE2
			LD		HL,FPACC2+1	; Point to high of mantissa / low of exp
			LD		DE,FPEXP2	; and the exponent 1 byte
			JR		FPDENORMALISESHR
			
	
; Denormalise FPACC1/2 and align them for add or subtract

FPDENORMALISEA
			CALL	FPDENORMALISE1	; Denormalise FPACC1
			CALL	FPDENORMALISE2	; and FPACC2
			JR		FPALIGNDN		; Align the values
	
	
; Calculate 10 ^ A where A >= 0

FPEXP10
			PUSH	AF				; Save for now
			CALL	FPLOAD_ONE		; Get 1.0 in FPACC1
			POP		AF				; Get counter back
			OR		A,A				; Check A
			JR		Z,FPEXP10X		; Exit if zero
			LD		B,A				; Get loop count into B
FPEXP10L	PUSH	BC				; Save BC for now
			CALL	FPLOAD2_TEN		; Get 10.0 in FPACC2
			CALL	FPMULT			; Multiply by FPACC1
			POP		BC				; Get BC back
			DJNZ	FPEXP10L		; Loop back if more to do
FPEXP10X	RET						; Return			


;
; Load memory into FPACC1 or FPACC2. HL points to address to load
;

FPLOAD_ZERO	LD		HL,FPCONSTZERO	; Get 0.0
			JR		FPLOAD1			; into FPACC1
FPLOAD_ONE	LD		HL,FPCONSTONE	; Get 1.0
FPLOAD1		LD		DE,FPACC1		; into FPACC1
FPLOADX		LD		BC,4			; 4 bytes to move
			LDIR					; Move it
			RET						; and return
FPLOAD2_TEN	LD		HL,FPCONSTTEN	; Get 10.0			
FPLOAD2		LD		DE,FPACC2		; Address should be FPACC2
			JR		FPLOADX			; and perform load


;
; Negate FPACC1/FPACC2
;

FPNEG1		LD		HL,FPACC1		; Address of exponent byte
FPNEG		LD		A,(HL)			; Get exp byte
			XOR		A,0x80			; Flip top bit
			LD		(HL),A			; save it
			RET						; and return
FPNEG2		LD		HL,FPACC2		; Point to FPACC2
			JR		FPNEG			; and negate it

	
; Normalise FPACC1

FPNORMALISE1
			PUSH	IX				; Save IX for now
			LD		IX,FPACC1		; Get address of FPACC
			LD		HL,FPEXP1		; and exponent
FPNORMALISESHR
			LD		A,(IX+3)		; Get mantissa bytes
			OR		A,(IX+2)		; to check
			OR		A,(IX+1)		; if zero
			JR		NZ,FPNORMALISE1Z
			LD		(IX),A			; Set the exponent byte if zero
			JR		Z,FPNORMALISE1X	; and exit
FPNORMALISE1Z
			BIT		7,(IX+1)		; See if already normalised
			JR		NZ,FPNORMALISE1A	; Yes it is
			SLA		(IX+3)			; No, rotate round
			RL		(IX+2)
			RL		(IX+1)
			DEC		(HL)			; and reduce exponent
			JR		FPNORMALISE1Z	; Loop back round
FPNORMALISE1A
			RES		7,(IX+1)		; Reset bit 23 of mantissa for now
			LD		A,(IX)			; Get existing neg bit
			RL		A				; into C
			SRL		(HL)			; Roll exponent to the right and roll neg bit in
			JR		NC,FPNORMALISE1B
			SET		7,(IX+1)		; Set high bit of mantissa if needed
FPNORMALISE1B
			LD		A,(HL)			; Get new FPEXP1
			LD		(IX),A			; and store in FPACC1
FPNORMALISE1X
			POP		IX				; Get IX back
			RET
			
FPNORMALISE2			
			PUSH	IX				; Save IX for now
			LD		IX,FPACC2		; Get address of FPACC
			LD		HL,FPEXP2		; and exponent
			
	
; Denormalised mantissa in FPACC1 is changed to twos complement

FPNOT1DN
			LD		HL,FPACC1+3		; Get address of LSB of mantissa
			LD		B,3				; 3 bytes to do
			XOR		A,A				; Clear carry
FPNOT1DN2
			LD		A,0				; Zero value
			SBC		A,(HL)			; Subtract mantissa
			LD		(HL),A			; and store back in mantissa
			DEC		HL				; Bump pointer
			DJNZ	FPNOT1DN2		; and loop back
			RET

	
;
; Save memory from FPACC1 or FPACC2. DE points to address to save to
;

FPSAVE1		LD		HL,FPACC1		; Source is FPACC1
FPSAVEX		LD		BC,4			; 4 bytes to move
			LDIR					; Move it
			RET						; and return
FPSAVE2		LD		HL,FPACC2		; Source is FPACC2
			JR		FPSAVEX			; and perform save

FPACC1SAVE	LD		DE,FPSAVED		; Point to save area
			JR		FPSAVE1			; Save the FPACC
FPACC1REST	LD		HL,FPSAVED		; Point to temp area to load from
			JP		FPLOAD1			; Load the FPACC again
	
; Shift FPACC1 mantissa to the right and increase exponent
; FPACC1 must be denormalised first

FPSHIFT1R
			PUSH	IX
			LD		HL,FPEXP1		; Get address of exponent 1
			LD		IX,FPACC1		; and FPACC1
FPSHIFTSHR
			SRL		(IX+1)			; Shift mantissa
			RR		(IX+2)
			RR		(IX+3)
			INC		(HL)
FPSHIFT1RX	
			POP		IX
			RET	
	
; Shift FPACC2 mantissa to the right and increase exponent
; FPACC2 must be denormalised first

FPSHIFT2R
			PUSH	IX
			LD		HL,FPEXP2		; Get address of exponent 2
			LD		IX,FPACC2		; and FPACC2
			JR		FPSHIFTSHR
	
; Subtract the mantissa of FPACC2 from FPACC1
; where they are both denormalised and respecting carry out

FPSUB12DN
			M_SBC24	FPACC1,FPACC2 	; Do the subtraction
			RET						; return with carry result
	
	
; 
; Swap FPACC1 and FPACC2
;

FPSWAP		LD		B,4				; 4 bytes to move
			LD		HL,FPACC1		; Set addresses
			LD		DE,FPACC2	
FPSWAPL		LD		C,(HL)			; Swap bytes
			LD		A,(DE)
			LD		(HL),A
			LD		A,C				; Get C into A
			LD		(DE),A			; for save
			INC		DE
			INC		HL
			DJNZ	FPSWAPL			; Loop back until done
			RET

; Rotate the temporary area to the left (multiply by 2)
; If carry set on entry, a 1 bit will be rotated in

FPTMPRL
			PUSH	AF				; Save original carry for later
			LD		B,6				; 6 bytes to move
			LD		HL,FPACCTMP+5
FPTMPRL2			
			RL		(HL)			; Rotate to multiply answer by 2
			DEC		HL
			DJNZ	FPTMPRL2
			POP		AF
			RET
	
	
;
; Storage area
;

FPACC1		DS		4			; Floating point accumulator #1
FPACC2		DS		4			; Floating point accumulator #2
FPACCTMP	DS		6			; Utility 48 bit area
FPEXP1		DS		1			; Saved exponent 1
FPEXP2		DS		1			; Saved exponent 2
FPSAVED		DS		4			; Saved FPACC

;
; Constants
;

FPCONSTZERO	DB		$00,$00,$00,$00		; Floating point constant 0.0
FPCONSTONE	DB		$3F,$80,$00,$00
FPCONSTPI	DB		$40,$49,$0F,$DB
FPCONSTTEN	DB		$41,$20,$00,$00
			
			
			ENDIF // IF DEFINED(__DMFPBASE_INC)
			LISTON

			