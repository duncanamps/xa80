			IF DEFINED(__DMFPPRINT_INC)
				LISTOFF
			ELSE
				__DMFPPRINT_INC = 1
			
				INCLUDE "dmfpbase.inc"

;
; DMFPPRINT.INC - Duncan's floating point print routine
;
; Sends print output to the console
;
; Routines are:
;
; FP2STR - Convert floating point to string value
;			A  - Format flags, high nyb = before DP, low = after
;			     $FF = don't use fixed point, just minimum
;		    DE - Destination address for the string. Must
;		         have enough space to take B+C characters
;				 + sign character + DP + 00 terminator
;			HL - Address of the floating point value
;
; FPOUT	-  Output the floating point number to the console
;			Same parameters as FP2STR however DE is not
;			used
;



; FP Flags

FPF_NEG		EQU		0
			
			
;-----------------------------------------------------------------------------
;
; FPASCII - Convert number in FPACC1 to string in FPSTRBUF			
;
; Calling:
;	FPACC1	holding IEEE 754 32 bit floating point value
;	Output	FPSTRBUF is a string buffer which will be filled
;			with <sign> predecimal <period> postdecimal <nul> in
;			a fixed format, e.g. "   -3.14159<nul>".  Up to 19
;			locations may be required.
;	A		holding value bbbbaaaa where bbbb is the number of
;			places before the decimal, and aaaa is the number
;			of places after the decimal.  Please note the the numbers
;			can only be represented to around 8 decimal places, so
;			no point having 8.8 digit result, as the last 8 will be
;			meaningless.  This applies even if using small numbers
;			like 1.2456, as anything after the decimal point will be
;			garbage.  Choose the number of digits carefully...
;
;-----------------------------------------------------------------------------

FPASCII
			CALL	FPDEBMSG		; Show a debug message
			DB 		"Entering FPASCII",13,10,"$"
			CALL	FPDEBUG			; Show FPACC1 and FPACC2 as hex
			PUSH	AF				; Save A for now
			AND		A,0x0f			; Mask for low bits only
			LD		(FPFMTPOST),A	; Save in number of post digits
			POP		AF				; Get A back
			SRL		A				; Move into low nibble
			SRL		A
			SRL		A
			SRL		A
			LD		(FPFMTPRE),A	; And save
			XOR		A,A				; Zero A
			LD		(FPFLAGS),A		; Save FP flags
			; IX will point to FPACC1 from now on
			; IY points to output buffer
			LD		IX,FPACC1		; Get addr of FPACC1
			LD		IY,FPSTRBUF		; and addr of output buf
			BIT		7,(IX+0)		; Get neg flag of FPACC
			JR		Z,FPASCII2		; Skip if positive
			SET		FPF_NEG,(IY+FPFLAGS-FPSTRBUF) ; Set negative flag
			CALL	FPNEG1			; Negate FPACC1 to make it positive	
FPASCII2
			CALL	FPDEBMSG		; Show a debug message
			DB 		"FPASCII2",13,10,"$"
			; Set up 'subtractor' which will be 10 ^ FPFMTPRE
			CALL	FPACC1SAVE		; Save FPACC1 for now
			LD		A,(FPFMTPRE)	; Get pre-DP digits
			CALL	FPEXP10			; Calculate 10 ^ A
			M_COPY	FPACC1,FPSUBTRACTR ; Copy to subtractor
			CALL	FPSWAP			; Move into FPACC2
			CALL	FPACC1REST		; Restore FPACC1
			CALL	FPDEBMSG		; Show a debug message
			DB 		"FPASCII2 - before FPCOMP",13,10,"$"
			CALL	FPCOMP			; Compare value and 10 ^ A
			JP		Z,FPASCII_OVERLOAD   ; Equal, so no good
			JP		NC,FPASCII_OVERLOAD  ; Value > 10^A so also no good
			; No overload situation, start filling out
			CALL	FPACC1REST		; Restore FPACC1 again
			PUSH	IY				; Copy IY
			POP		HL				; into HL
			LD		(HL),' '		; Space at start
			INC		HL				; and bump pointer
			LD		A,(FPFMTPRE)	; Get number of pre-digits
			OR		A,A				; Check it
			JR		Z,FPASCIIDP		; If none, do decimal point
			LD		B,A				; Save in counter
			; FPACC1 contains the floating point value to print
			; FPSUBTRACTR contains the current subtractor
FPASCII3
			CALL	FPDEBMSG		; Show a debug message
			DB 		"FPASCII3",13,10,"$"
			CALL	FPDEBUG			; Show FPACC1 and FPACC2 as hex
			CALL	FPCOUNTSUB		; Count number of times sub will go in
			ADD		A,'0'			; Convert to ASCII
			LD		(HL),A			; Save in the string
			INC		HL
			DJNZ	FPASCII3		; Loop back if more to do
FPASCIIDP
			CALL	FPDEBMSG		; Show a debug message
			DB 		"FPASCIIDP",13,10,"$"
			LD		A,(FPFMTPOST)	; Get number of post digits
			LD		B,A				; Copy into counter
			OR		A,A				; Check if zero
			JR		Z,FPASCIIX		; exit if none
			LD		(HL),'.'		; Decimal point
			INC		HL				; and bump pointer
FPASCII4
			CALL	FPDEBMSG		; Show a debug message
			DB 		"FPASCII4",13,10,"$"
			CALL	FPCOUNTSUB		; Count number of times sub will go in
			ADD		A,'0'			; Convert to ASCII
			LD		(HL),A			; Save in string
			INC		HL				; Bump pointer
			DJNZ	FPASCII4		; Loop back if more to do
FPASCIIX
			CALL	FPDEBMSG		; Show a debug message
			DB 		"FPASCIIX",13,10,"$"
			LD		(HL),0			; Terminator, store it
	
			; Now clear leading zeroes
	
			LD		HL,FPSTRBUF+1	; Get address of buffer
FPASCIIZ
			CALL	FPDEBMSG		; Show a debug message
			DB 		"FPASCIIZ",13,10,"$"
			LD		A,(HL)			; Get a byte from the buffer
			XOR		A,'0'			; XOR with digit 0
			JR		NZ,FPASCIIZ2	; Bomb out if it wasn't '0'
			LD		(HL),' '		; Replace with space
			INC		HL				; Bump the pointer
			JR		FPASCIIZ		; and loop round
	
			; Check if leading zero is required
			
FPASCIIZ2
			CALL	FPDEBMSG		; Show a debug message
			DB 		"FPASCIIZ2",13,10,"$"
			LD		A,(FPFMTPRE)	; Get pre-digits
			OR		A,A				; Any?
			JR		Z,FPASCIIM		; No, move on
			LD		A,(HL)			; Check if we
			OR		A,A				; hit the terminator
			JR		Z,FPASCIIZZ		; Yes, we need to add a zero
			CP		A,'.'			; Was it the DP we hit?
			JR		NZ,FPASCIIM		; No, don't add a leading zero
	
			; Add the leading zero
			
FPASCIIZZ
			CALL	FPDEBMSG		; Show a debug message
			DB 		"FPASCIIZZ",13,10,"$"
			DEC		HL				; Move the pointer back
			LD		(HL),'0'		; and put the leading zero in
	
	; Deal with leading minus if appropriate
	
FPASCIIM
			CALL	FPDEBMSG		; Show a debug message
			DB 		"FPASCIIM",13,10,"$"
			BIT		FPF_NEG,(IY+FPFLAGS-FPSTRBUF) ; Check negative flag
			JR		Z,FPASCIIRET	; Return if not set
			DEC		HL				; Go back one step
			LD		(HL),'-'		; Negative indicator
FPASCIIRET
			CALL	FPDEBMSG		; Show a debug message
			DB 		"FPASCIIRET",13,10,"$"
			RET						; Return to caller
	
	; Number is too big to be formatted, display as [-]****.****
	
FPASCII_OVERLOAD
			PUSH	IY				; Copy IY
			POP		HL				; into HL again
			LD		A,' '			; Get space character
			BIT		FPF_NEG,(IY+FPFLAGS-FPSTRBUF) ; Check negative flag
			JR		Z,FPASCII_OVERLOAD1 ; Skip if positive
			LD		A,'-'			; else get minus character
FPASCII_OVERLOAD1
			LD		(HL),A			; Save the character
			INC		HL				; and bump pointer
			LD		A,(FPFMTPRE)	; Check count of pre dp characters
			OR		A,A
			JR		Z,FPASCII_OVERLOADDP ; None, just do DP
			LD		B,A				; Copy into loop counter
			LD		A,'*'			; Get * character
FPASCII_OVERLOAD2
			LD		(HL),A			; Save in buf
			INC		HL				; Bump pointer
			DJNZ 	FPASCII_OVERLOAD2 ; and loop if required
FPASCII_OVERLOADDP
			LD		A,(FPFMTPOST)	; Get count of post dp characters
			LD		B,A
			OR		A,A
			JR		Z,FPASCII_OVERLOADNUL ; None do the nul at the end
			LD		(HL),'.'		; Place DP character
			INC		HL				; and bump pointer
			LD		A,'*'			; Get * character
FPASCII_OVERLOAD3
			LD		(HL),A			; Save in string
			INC		HL				; and bump pointer
			DJNZ	FPASCII_OVERLOAD3 ; Loop back until done
FPASCII_OVERLOADNUL
			LD		(HL),0			; Store NUL in string


; Count the number of times a subtractor will go into FPACC1
; The subtractor is divided by 10 on entry, and the result will be
; in W

FPCOUNTSUB
			PUSH	BC				; Save BC
			PUSH	HL				; and HL
			CALL	FPACC1SAVE		; Save FPACC1 for now
			M_COPY	FPSUBTRACTR,FPACC1	; Get subtractor value
			CALL	FPLOAD2_TEN		; Get 10.000 in FPACC2
			CALL	FPDIV			; Divide by 10
			M_COPY	FPACC1,FPSUBTRACTR	; Save subtractor value
			CALL	FPACC1REST		; Restore FPACC1
			XOR		A,A				; Clear the digit counter
FPCOUNTSUB2
			PUSH	AF				; Save digit counter
			M_COPY	FPSUBTRACTR,FPACC2	; Get subtractor in FPACC2
			CALL	FPSUB				; and take it away
			BIT		FPF_NEG,(IY+FPFLAGS-FPSTRBUF) ; Check negative flag
			POP		AF				; Restore digit counter
			JR		NZ,FPCOUNTSUB3	; Negative, break out of loop
			INC		A				; Increment the digit counter
			JR		FPCOUNTSUB2		; and loop back for more
FPCOUNTSUB3
			PUSH	AF				; Save digit counter
			M_COPY	FPSUBTRACTR,FPACC2	; Get subtractor in FPACC2
			CALL	FPADD			; Add back to FPACC1
			POP		AF				; Get digit counter back
			POP		HL				; Restore HL
			POP		BC				; and BC
			RET						; end exit	


;
; FPOUT - Print a floating point string to the console, FP value in FPACC1
;         A holds formatting information
;
			
FPOUT		CALL	FPDEBMSG		; Show a debug message
			DB 		"Entering FPOUT",13,10,"$"
			CALL	FPDEBUG
			CALL	FPDEBREG
			CALL	FPASCII			; Convert to a string
			XOR		A,A				; Zero A
			LD		HL,FPSTRBUF		; Get address of buffer
			PUSH	HL				; Save back on the stack again
			LD		BC,19			; Max 19 characters
			CPIR					; Find the NULL terminator
			DEC		HL				; Move back to the 00 terminator
			LD		(HL),'$'		; Put the terminator in
			POP		DE				; Get buffer address into DE
			LD      C,9    			; Write string function
			JP      5        		; Go to BDOS to write the string	


;
; Saved string buffer and other variables
; Don't change the order of these as the IY register
; +offset is used to access the flags etc.

FPSTRBUF	DS		19				; 19 bytes will be enough			
FPFLAGS		DS		1				; Various FP flags. See EQUs at top for values
FPFMTPOST	DS		1				; Number of digits after DP
FPFMTPRE	DS		1				; Number of digits before DP
FPSUBTRACTR DS		4				; Subtractor

; DEBUF STUFF

; Print the debug registers

FPDEBREG	PUSH	AF				; Save all registers
			PUSH	BC
			PUSH	DE
			PUSH	HL
			PUSH	IX
			PUSH	IY
			LD		(FPDREGIY),IY	; Save to memory
			LD		(FPDREGIX),IX
			LD		(FPDREGHL),HL
			LD		(FPDREGDE),DE
			LD		(FPDREGBC),BC
			PUSH	AF				; THere's no LD (nnnn),AF
			POP		HL
			LD		(FPDREGAF),HL
			; Encode the details into the strings
			LD		HL,FPDREGAF
			LD		DE,FPDREGSAF
			CALL	FPDEBWORD
			LD		HL,FPDREGBC
			LD		DE,FPDREGSBC
			CALL	FPDEBWORD
			LD		HL,FPDREGDE
			LD		DE,FPDREGSDE
			CALL	FPDEBWORD
			LD		HL,FPDREGHL
			LD		DE,FPDREGSHL
			CALL	FPDEBWORD
			LD		HL,FPDREGIX
			LD		DE,FPDREGSIX
			CALL	FPDEBWORD
			LD		HL,FPDREGIY
			LD		DE,FPDREGSIY
			CALL	FPDEBWORD
			; Now print the message itself
			LD		DE,FPDREGSTR	; Debug string with all registers in
			LD      C,9    			; Write string function
			CALL    5        		; Call BDOS to write the string	
			POP		IY				; Restore all registers
			POP		IX
			POP		HL
			POP		DE
			POP		BC
			POP		AF
			RET						; and exit

; Print the debug message which is at the return address

FPDEBMSG	PUSH	AF				; Save all registers
			PUSH	BC
			PUSH	DE
			PUSH	HL
			PUSH	IX
			PUSH	IY
			LD		HL,12			; 12 bytes to cover pushes above
			ADD		HL,SP			; add to SP
			LD		SP,HL			; back to SP
			POP		HL				; Get return address into HL
			LD		(FPDEBRET),HL	; and save it
			LD		A,'$'			; $ sign to search for
			LD		BC,512			; Maximum message length
			CPIR					; Search for dollar
			PUSH	HL				; Save as new return address
			LD		HL,-12			; Load -12 to add to SP
			ADD		HL,SP			; Add to current SP
			LD		SP,HL			; Restore SP to correct value
			; Stack fixes and return address all sorted out
			; Now print the message itself
			LD		DE,(FPDEBRET)	; Return (message) address
			LD      C,9    			; Write string function
			CALL    5        		; Call BDOS to write the string	
			POP		IY				; Restore all registers
			POP		IX
			POP		HL
			POP		DE
			POP		BC
			POP		AF
			RET						; and exit

; Print hex version of FPACC1 and FPACC2

FPDEBUG		PUSH	AF				; Save all registers
			PUSH	BC
			PUSH	DE
			PUSH	HL
			PUSH	IX
			PUSH	IY
			LD		HL,FPACC1		; Point to FPACC1
			LD		DE,FPDEBSTR1	; and space in bu;ffer
			CALL	FPDEBFILL		; Fill the buffer with FPACC1
			LD		HL,FPACC2		; Point to FPACC2
			LD		DE,FPDEBSTR2	; and space in buffer
			CALL	FPDEBFILL		; Fill the buffer with FPACC2
			LD		HL,FPACCTMP		; Point to FPTMP
			LD		DE,FPDEBSTRT	; and space in buffer
			CALL	FPDEBFILL6		; Fill the buffer with FPACCTMP
			; Output the string
			LD		DE,FPDEBSTR		; Get address of string to display
			LD      C,9    			; Write string function
			CALL    5        		; Call BDOS to write the string	
			POP		IY				; Restore all registers
			POP		IX
			POP		HL
			POP		DE
			POP		BC
			POP		AF
			RET						; and exit

FPDEBWORD	INC		HL
			CALL	FPDEBFILLB		; Do MSB first
			DEC		HL
			DEC		HL				; Go back to MSB
			JR		FPDEBFILLB		; and do it
	
FPDEBFILL6	LD		B,6				; 6 bytes to do
			JR		FPDEBFILL2		; Share the following routine
FPDEBFILLB	LD		B,1				; 1 byte to do
			JR		FPDEBFILL2		; Share the following routine
FPDEBFILL	LD		B,4				; 4 bytes to do
FPDEBFILL2	LD		A,(HL)			; Get source byte
			RR		A				; Move to low nybble
			RR		A
			RR		A
			RR		A
			CALL	FPDEBDIGIT		; and output digit
			LD		A,(HL)			; Get source byte again
			CALL	FPDEBDIGIT		; Low nibble output this time
			INC		HL				; Bump pointer
			DJNZ	FPDEBFILL2		; Loop back for more
			RET						; and exit
			
FPDEBDIGIT	AND		A,%00001111		; Mask to leave low nibble
			CP		A,0x0a			; Above digit 9 ?
			JR		C,FPDEBDIGIT2	; No, we skip
			ADD		A,'A'-'9'-1		; Add the offset
FPDEBDIGIT2	ADD		A,'0'			; Turn into ASCII
			LD		(DE),A			; and save
			INC		DE				; Bump buffer pointer
			RET						; and back to caller
			
FPDEBSTR	DB		"FPACC1="
FPDEBSTR1	DB		"00000000 FPACC2="
FPDEBSTR2	DB		"00000000 FPACCTMP="
FPDEBSTRT	DB		"000000000000",13,10,"$"
FPDREGSTR	DB		"AF="
FPDREGSAF	DB		".... BC="
FPDREGSBC	DB		".... DE="
FPDREGSDE	DB		".... HL="
FPDREGSHL	DB		".... IX="
FPDREGSIX	DB		".... IY="
FPDREGSIY	DB		"....",13,10,"$"
FPDREGAF	DS		2
FPDREGBC	DS		2
FPDREGDE	DS		2
FPDREGHL	DS		2
FPDREGIX	DS		2
FPDREGIY	DS		2

FPDEBRET	DS		2				; Return address for message routine
FPDEBSTK	DS		2				; Stack address for message routine return

			ENDIF // IF DEFINED(__DMFPPRINT_INC)
			LISTON