//===========================================================================
//
//  TEST_8080.ASM
//
//  Test the 8080 instruction set in full and all of the XA80 commands and
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

;
; 8080 - Move Load and Store
;

		MOV		B,B				// | %01000000
		MOV		B,C				// | %01000001
		MOV		B,D				// | %01000010
		MOV		B,E				// | %01000011
		MOV		B,H				// | %01000100
		MOV		B,L				// | %01000101
		MOV		B,M				// | %01000110
		MOV		B,A				// | %01000111

		MOV		C,B				// | %01001000
		MOV		C,C				// | %01001001
		MOV		C,D				// | %01001010
		MOV 	C,E				// | %01001011
		MOV 	C,H				// | %01001100
		MOV 	C,L				// | %01001101
		MOV 	C,M				// | %01001110
		MOV 	C,A				// | %01001111

		MOV 	D,B				// | %01010000
		MOV 	D,C				// | %01010001
		MOV 	D,D				// | %01010010
		MOV 	D,E				// | %01010011
		MOV 	D,H				// | %01010100
		MOV 	D,L				// | %01010101
		MOV 	D,M				// | %01010110
		MOV 	D,A				// | %01010111

		MOV 	E,B				// | %01011000
		MOV 	E,C				// | %01011001
		MOV 	E,D				// | %01011010
		MOV 	E,E				// | %01011011
		MOV 	E,H				// | %01011100
		MOV 	E,L				// | %01011101
		MOV 	E,M				// | %01011110
		MOV 	E,A				// | %01011111

		MOV 	H,B				// | %01100000
		MOV 	H,C				// | %01100001
		MOV 	H,D				// | %01100010
		MOV 	H,E				// | %01100011
		MOV 	H,H				// | %01100100
		MOV 	H,L				// | %01100101
		MOV 	H,M				// | %01100110
		MOV 	H,A				// | %01100111

		MOV 	L,B				// | %01101000
		MOV 	L,C				// | %01101001
		MOV 	L,D				// | %01101010
		MOV 	L,E				// | %01101011
		MOV 	L,H				// | %01101100
		MOV 	L,L				// | %01101101
		MOV 	L,M				// | %01101110
		MOV 	L,A				// | %01101111

		MOV 	M,B				// | %01110000
		MOV 	M,C				// | %01110001
		MOV 	M,D				// | %01110010
		MOV 	M,E				// | %01110011
		MOV 	M,H				// | %01110100
		MOV 	M,L				// | %01110101
		; MOV M M doesn't exist, it's the HLT instruction :)
		MOV 	M,A				// | %01110111

		MOV 	A,B				// | %01111000
		MOV 	A,C				// | %01111001
		MOV 	A,D				// | %01111010
		MOV 	A,E				// | %01111011
		MOV 	A,H				// | %01111100
		MOV 	A,L				// | %01111101
		MOV 	A,M				// | %01111110
		MOV 	A,A				// | %01111111

		MVI 	B,U8			// | %00000110 [2:U8]
		MVI 	C,U8			// | %00001110 [2:U8]
		MVI 	D,U8			// | %00010110 [2:U8]
		MVI 	E,U8			// | %00011110 [2:U8]
		MVI 	H,U8			// | %00100110 [2:U8]
		MVI 	L,U8			// | %00101110 [2:U8]
		MVI 	M,U8			// | %00110110 [2:U8]
		MVI 	A,U8			// | %00111110 [2:U8]

		LXI 	B,U16			// | %00000001 [2:U16]
		LXI 	D,U16			// | %00010001 [2:U16]
		LXI 	H,U16			// | %00100001 [2:U16]
		STAX 	B				// | %00000010
		STAX 	D				// | %00010010
		LDAX 	B				// | %00001010
		LDAX 	D				// | %00011010
		STA 	U16				// | %00110010 [1:U16]
		LDA 	U16				// | %00111010 [1:U16]
		SHLD 	U16				// | %00100010 [1:U16]
		LHLD 	U16				// | %00101010 [1:U16]
		XCHG					// | %11101011

;
; 8080 - Stack operations
;

		PUSH 	B				// | %11000101
		PUSH 	D				// | %11010101
		PUSH 	H				// | %11100101
		PUSH 	PSW				// | %11110101
		POP 	B				// | %11000001
		POP 	D				// | %11010001
		POP 	H				// | %11100001
		POP 	PSW				// | %11110001
		XTHL	  				// | %11100011
		SPHL					// | %11111001
		LXI 	SP,U16			// | %00110001 [2:U16]
		INX 	SP				// | %00110011
		DCX 	SP				// | %00111011

;
; 8080 - Jump
;

		JMP 	U16				// | %11000011 [1:U16]
		JC 		U16				// | %11011010 [1:U16]
		JNC 	U16				// | %11010010 [1:U16]
		JZ 		U16				// | %11001010 [1:U16]
		JNZ 	U16				// | %11000010 [1:U16]
		JP 		U16				// | %11110010 [1:U16]
		JM 		U16				// | %11111010 [1:U16]
		JPE 	U16				// | %11101010 [1:U16]
		JPO 	U16				// | %11100010 [1:U16]
		PCHL					// | %11101001

;
; 8080 - Call
;

		CALL 	U16				// | %11001101 [1:U16]
		CC 		U16				// | %11011100 [1:U16]
		CNC 	U16				// | %11010100 [1:U16]
		CZ 		U16				// | %11001100 [1:U16]
		CNZ 	U16				// | %11000100 [1:U16]
		CP 		U16				// | %11110100 [1:U16]
		CM 		U16				// | %11111100 [1:U16]
		CPE 	U16				// | %11101100 [1:U16]
		CPO 	U16				// | %11100100 [1:U16]

;
; 8080 - Return
;

		RET						// | %11001001
		RC						// | %11011000
		RNC						// | %11010000
		RZ						// | %11001000
		RNZ						// | %11000000
		RP						// | %11110000
		RM						// | %11111000
		RPE						// | %11101000  ; Incorrectly shows %11100000 in Intel manual
		RPO						// | %11100000

;
; 8080 - Restart
;

		RST 	U8				// | %11[1:RST]111
		RST		0
		RST		1
		RST		2
		RST		3
		RST		4
		RST		5
		RST		6
		RST		7
		RST		8				; And another set 8/16/.../48/56
		RST		16
		RST		24
		RST		32
		RST		40
		RST		48
		RST		56
;
; 8080 - Increment and Decrement
;

		INR 	B				// | %00000100
		INR 	C				// | %00001100
		INR 	D				// | %00010100
		INR 	E				// | %00011100
		INR 	H				// | %00100100
		INR 	L				// | %00101100
		INR 	M				// | %00110100
		INR 	A				// | %00111100

		DCR 	B				// | %00000101
		DCR 	C				// | %00001101
		DCR 	D				// | %00010101
		DCR 	E				// | %00011101
		DCR 	H				// | %00100101
		DCR 	L				// | %00101101
		DCR 	M				// | %00110101
		DCR 	A				// | %00111101

		INX 	B				// | %00000011
		INX 	D				// | %00010011
		INX 	H				// | %00100011

		DCX 	B				// | %00001011
		DCX 	D				// | %00011011
		DCX 	H				// | %00101011

;
; 8080 - Add
;

		ADD 	B				// | %10000000
		ADD 	C				// | %10000001
		ADD 	D				// | %10000010
		ADD 	E				// | %10000011
		ADD 	H				// | %10000100
		ADD 	L				// | %10000101
		ADD 	M				// | %10000110
		ADD 	A				// | %10000111

		ADC 	B				// | %10001000
		ADC 	C				// | %10001001
		ADC 	D				// | %10001010
		ADC 	E				// | %10001011
		ADC 	H				// | %10001100
		ADC 	L				// | %10001101
		ADC 	M				// | %10001110
		ADC 	A				// | %10001111

		ADI 	U8				// | %11000110 [1:U8]
		ACI 	U8				// | %11001110 [1:U8]

		DAD 	B				// | %00001001
		DAD 	D				// | %00011001
		DAD 	H				// | %00101001
		DAD 	SP				// | %00111001

;
; 8080 - Subtract
;

		SUB 	B				// | %10010000
		SUB 	C				// | %10010001
		SUB 	D				// | %10010010
		SUB 	E				// | %10010011
		SUB 	H				// | %10010100
		SUB 	L				// | %10010101
		SUB 	M				// | %10010110
		SUB 	A				// | %10010111

		SBB 	B				// | %10011000
		SBB 	C				// | %10011001
		SBB 	D				// | %10011010
		SBB 	E				// | %10011011
		SBB 	H				// | %10011100
		SBB 	L				// | %10011101
		SBB 	M				// | %10011110
		SBB 	A				// | %10011111

		SUI 	U8				// | %11010110 [1:U8]
		SBI 	U8				// | %11011110 [1:U8]

;
; 8080 - Logical
;

		ANA 	B				// | %10100000
		ANA 	C				// | %10100001
		ANA 	D				// | %10100010
		ANA 	E				// | %10100011
		ANA 	H				// | %10100100
		ANA 	L				// | %10100101
		ANA 	M				// | %10100110
		ANA 	A				// | %10100111

		XRA 	B				// | %10101000
		XRA 	C				// | %10101001
		XRA 	D				// | %10101010
		XRA 	E				// | %10101011
		XRA 	H				// | %10101100
		XRA 	L				// | %10101101
		XRA	 	M				// | %10101110
		XRA 	A				// | %10101111

		ORA 	B				// | %10110000
		ORA 	C				// | %10110001
		ORA 	D				// | %10110010
		ORA 	E				// | %10110011
		ORA 	H				// | %10110100
		ORA 	L				// | %10110101
		ORA 	M				// | %10110110
		ORA 	A				// | %10110111

		CMP 	B				// | %10111000
		CMP 	C				// | %10111001
		CMP 	D				// | %10111010
		CMP 	E				// | %10111011
		CMP 	H				// | %10111100
		CMP 	L				// | %10111101
		CMP 	M				// | %10111110
		CMP 	A				// | %10111111

		ANI 	U8				// | %11100110 [1:U8]
		XRI 	U8				// | %11101110 [1:U8]
		ORI 	U8				// | %11110110 [1:U8]
		CPI 	U8				// | %11111110 [1:U8]

;
; 8080 - Rotate
;

		RLC						// | %00000111
		RRC						// | %00001111
		RAL						// | %00010111
		RAR						// | %00011111

;
; 8080 - Specials
;

		CMA						// | %00101111
		STC						// | %00110111
		CMC						// | %00111111
		DAA						// | %00100111

;
; 8080 - Input / Output
;

		IN 		U8				// | %11011011 [1:U8]
		OUT 	U8				// | %11010011 [1:U8]

;
; 8080 - Control
;

		EI						// | %11111011
		DI						// | %11110011
		NOP						// | %00000000
		HLT						// | %01110110



;----------------------------------------------------------------------------
;
; Test expressions
;
;----------------------------------------------------------------------------

;
; Simple operands
;

		MVI		A,123			// Simple 8 bit number
		MVI		A,'0'			// A character
		MVI		A,0x7b			// Hex values
		MVI		A,0X7B
		MVI		A,7BH
		MVI		A,7bh
		MVI		A,$7B
		MVI		A,$7b
		MVI		A,0b01111011	// Binary values
		MVI		A,0B01111011
		MVI		A,01111011b
		MVI		A,01111011B
		MVI		A,%01111011
		
;
; Mathematical operators
;

		MVI		A,1+2			// 03 Addition
		MVI		A,5-2			// 03 Subtraction
		MVI		A,5*2			// 0A Multiplication
		MVI		A,99/3			// 21 Division
		MVI		A,9+'0'			// 39 Addition using character
		MVI		A,1+2*3			// 07 Precedence
;
; Unary operators
;

		LXI		H,~123			// FF84 Unary NOT
		LXI		H,-123			// FF85 Unary MINUS
		LXI		H,+123			// 007B Unary PLUS
		LXI		H,~'0'			// FFEF Unoary NOT with character

;
; Binary operators
;

		MVI		A,123 & $3C		// 38 Binary AND
		MVI		A,123 | $3C		// 7F Binary OR
		MVI		A,123 ^ $3C		// 47 Binary XOR
		MVI		A,1 << 3		// 08 Left shift
		MVI		A,1 shl 3
		MVI		A,0x80 >> 4		// 08 Right shift
		MVI		A,0x80 shr 4
		MVI		A,123 % 23		// 08 Modulo
		MVI		A,123 mod 23
		MVI		A,'9' & 0x0f	// 09 Binary AND with character
		
;
; Comparison operators
;

LOWNUM	EQU		2
MEDNUM	EQU		7
MEDNUM2	EQU		7
HIGHNUM	EQU		19

		MVI		A,HIGHNUM >= MEDNUM		// 01
		MVI		A,MEDNUM2 >= MEDNUM		// 01
		MVI		A,LOWNUM  >= MEDNUM		// 00
		MVI		A,HIGHNUM >  MEDNUM		// 01
		MVI		A,MEDNUM2 >  MEDNUM		// 00
		MVI		A,LOWNUM  >  MEDNUM		// 00
		MVI		A,HIGHNUM == MEDNUM		// 00
		MVI		A,MEDNUM2 == MEDNUM		// 01
		MVI		A,LOWNUM  == MEDNUM		// 00
		MVI		A,HIGHNUM != MEDNUM		// 01
		MVI		A,MEDNUM2 != MEDNUM		// 00
		MVI		A,LOWNUM  != MEDNUM		// 01
		MVI		A,HIGHNUM <  MEDNUM		// 00
		MVI		A,MEDNUM2 <  MEDNUM		// 00
		MVI		A,LOWNUM  <  MEDNUM		// 01
		MVI		A,HIGHNUM <= MEDNUM		// 00
		MVI		A,MEDNUM2 <= MEDNUM		// 01
		MVI		A,LOWNUM  <= MEDNUM		// 01

;
; Numeric function testing		
;

		DB		ASC("7")				// 37 ASCII value of character
		DB		ASC("765")				// 37 Only first character
		DB		ASC("")					// 00 NULL string gives character 0
		DB		HIGH(0x1234)			// 12 High byte of word
		DB		IIF(1>2,0x12,0x99)		// 99 Immediate IF
		DB		IIF(1<2,0x12,0x99)		// 12
		DB		LOW(0x1234)				// 34 Low byte of word
		DW		ORG						// ?? ?? The origin
		DW		$						// ?? ?? The origin
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
		DB		VERSION()				// 30 2E 32 2E 30 Version as string

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