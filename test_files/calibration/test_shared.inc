//===========================================================================
//
//  TEST_SHARED.INC
//
//  Shared file to test common functions, should be included for all
//  processors and saves repeating identical stuff over and over again.
//
//  Duncan Munro  05/06/2023
//
//===========================================================================

;
; Simple operands
;

		DB		123				// Simple 8 bit number
		DB		'0'				// A character
		DB		0x7b			// Hex values
		DB		0X7B
		DB		7BH
		DB		7bh
		DB		$7B
		DB		$7b
		DB		0b01111011		// Binary values
		DB		0B01111011
		DB		01111011b
		DB		01111011B
		DB		%01111011
		
;
; Mathematical operators
;

		DB		1+2				// 03 Addition
		DB		5-2				// 03 Subtraction
		DB		5*2				// 0A Multiplication
		DB		99/3			// 21 Division
		DB		9+'0'			// 39 Addition using character
		DB		1+2*3			// 07 Precedence
;
; Unary operators
;

		DW		~123			// FF84 Unary NOT
		DW		-123			// FF85 Unary MINUS
		DW		+123			// 007B Unary PLUS
		DW		~'0'			// FFEF Unoary NOT with character

;
; Binary operators
;

		DB		123 & $3C		// 38 Binary AND
		DB		123 | $3C		// 7F Binary OR
		DB		123 ^ $3C		// 47 Binary XOR
		DB		1 << 3			// 08 Left shift
		DB		1 shl 3
		DB		0x80 >> 4		// 08 Right shift
		DB		0x80 shr 4
		DB		123 % 23		// 08 Modulo
		DB		123 mod 23
		DB		'9' & 0x0f		// 09 Binary AND with character
		
;
; Comparison operators
;

LOWNUM	EQU		2
MEDNUM	EQU		7
MEDNUM2	EQU		7
HIGHNUM	EQU		19

		DB		HIGHNUM >= MEDNUM		// 01
		DB		MEDNUM2 >= MEDNUM		// 01
		DB		LOWNUM  >= MEDNUM		// 00
		DB		HIGHNUM >  MEDNUM		// 01
		DB		MEDNUM2 >  MEDNUM		// 00
		DB		LOWNUM  >  MEDNUM		// 00
		DB		HIGHNUM == MEDNUM		// 00
		DB		MEDNUM2 == MEDNUM		// 01
		DB		LOWNUM  == MEDNUM		// 00
		DB		HIGHNUM != MEDNUM		// 01
		DB		MEDNUM2 != MEDNUM		// 00
		DB		LOWNUM  != MEDNUM		// 01
		DB		HIGHNUM <  MEDNUM		// 00
		DB		MEDNUM2 <  MEDNUM		// 00
		DB		LOWNUM  <  MEDNUM		// 01
		DB		HIGHNUM <= MEDNUM		// 00
		DB		MEDNUM2 <= MEDNUM		// 01
		DB		LOWNUM  <= MEDNUM		// 01

;
; Numeric function testing		
;

		DB		ASC("7")				// 37 ASCII value of character
		DB		ASC("765")				// 37 Only first character
		DB		ASC("")					// 00 NULL string gives character 0
		DB		DEFINED(HIGHNUM)		// 01 as HIGHNUM is already defined
		DB		DEFINED(NONUM)			// 00 as NONUM is not already defined
		DB		HIGH(0x1234)			// 12 High byte of word
		DB		IIF(1>2,0x12,0x99)		// 99 Immediate IF
		DB		IIF(1<2,0x12,0x99)		// 12
		DW		LENGTH("Duncan Munro")	// 0C Length of a string
		DB		LOW(0x1234)				// 34 Low byte of word
		DW		$						// ?? ?? The origin
		DW		ORG()					// ?? ?? The origin
		DB		POS("N","DUNCAN")		// 03 Pos of substring in string
		DB		POS("Z","DUNCAN")		// 00
		DB		VALUE("123")			// 7B Convert string to number

;
; String function testing
;

		DB		CHR(55)					// 37 Character value of ASCII code
		DB		HEX(0x1234)				// 31 32 33 34 Hex string of number
		DB		IIF(1>2,"Y","N")		// 4E Immediate IF for string
		DB		IIF(1<2,"Y","N")		// 59
		DB		LEFT("DUNCAN",2)		// 44 55 Leftmost characters in string
		DB		LOWER("AbC0")			// 61 62 63 30 Lower case of string
		DB		MID("DUNCAN",3,2)		// 4E 43 Middle characters in string
		DB		RIGHT("DUNCAN",2)		// 41 4E Rightmost characters in string
		DB		STRING(12)				// 31 32 Convert number to string
		DB		UPPER("AbC0")			// 41 42 43 30 Upper case of string

;
; Functions commented out as the values will vary and frustrate the
; automated testing
;

;		DB		BUILD()					// ?? Current build number as a string
;		DB		DATE()					// ?? Current date as a string
;		DB		TIME()					// ?? Current time as a string
		
;
; More complex numeric expressions
;

		DW		19*2+41*3				// 00A1 * precedes +
		DW		0x04 | 0x03 << 9		// 0604 << precedes |
		
;
; String expressions
;

		DB		"A" + "B" + "C"			// 41 42 43 concatenation
FIRST	EQU		"AC"
LAST	EQU		"DC"
FULL	=		FIRST + "/" + LAST
		DB		FULL					// 41 43 2F 44 43...
		DB		RIGHT(FULL,2) + LEFT(FULL,2)	// 44 43 41 43 

;
; Strings with bit 7 set in last character
;

KEYWDS	DC		'SGN', 'TAN', 'COS'
TABLK	DEFC	'END', 'FOR', 'NEXT', 0, 'PLOT', "VLIN"
		DC		LEFT("ENDING",3), LEFT("FORTRAN",3), RIGHT("INNEXT",4)

;
; IF / ELSE / ENDIF
;

		; Check IF for a true value
		
		IF 1
			MSGINFO "Good"
		ELSE
			MSGERROR "BAD!"
		ENDIF
		
		; Check IF for a false value
		
		IF 0
			MSGERROR "BAD!"
		ELSE
			MSGINFO "Good"
		ENDIF

		; Check nesting
		
		IF 1
		  MSGINFO "Good"
		  IF 0
		    MSGERROR "Bad"
		  ELSE
		    MSGINFO "Good"
		  ENDIF
		ELSE
		  MSGERROR "Bad"
		  IF 1
		    MSGERROR "Also bad"
		  ELSE
		    MSGERROR "Also bad"
		  ENDIF
		ENDIF

;
; WHILE / ENDW
;

		; Simple while
		
		J = 0
		WHILE J < 3
		  J = J + 1
		  DB J
		ENDW

		; Nested while
		
		J = 0
		WHILE J < 2
		  J = J + 1
		  K = 0
		  WHILE K < 2
		    K = K + 1
			DW (J << 8) + K
		  ENDW
		ENDW
		
;
; REPEAT / ENDR
;

		; Simple repeat
		
		loop_count = 0
		
		REPEAT 3
			NOP
			loop_count = loop_count + 1
		ENDR
			
		REPEAT loop_count
			DB loop_count
			loop_count = loop_count - 1
		ENDR
			
;
; Nested repeat
;

		J = 0
		REPEAT 3
			J = J + 1
			K = 0
			REPEAT 3
				K = K + 1
				LL = J << K
				DB LL
			ENDR
		ENDR		
		
;
; Composites of IF / WHILE / REPEAT etc.
;

		IF 1
		  K = 0
		  WHILE K < 4
		    IF (K MOD 2) == 0
			  DB "Even"
			  REPEAT 2
			    DB K
			  ENDR
			ELSE
			  DB "Odd"
			  REPEAT 3
				DB K
			  ENDR
			ENDIF
		    K = K + 1
		  ENDW
		ELSE
		  J = 0
		  WHILE J < 2
		    J = J + 1
			MSGERROR "Bad"
		  ENDW
		ENDIF
		
