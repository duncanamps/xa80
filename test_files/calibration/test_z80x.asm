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


;
; Z80X - Arithmetic instructions
;

		ADC     A             // $8F ; Alternative version of ADC A,A
		ADC     A,A           // $8F
		ADC     A,B           // $88
		ADC     A,C           // $89
		ADC     A,D           // $8A
		ADC     A,E           // $8B
		ADC     A,H           // $8C
		ADC     A,(HL)        // $8E
		ADC     A,IXH         // $DD $8C ; *UNDOCUMENTED*
		ADC     A,IXL         // $DD $8D ; *UNDOCUMENTED*
		ADC     A,(IX+DISPL)  // $DD $8E [2:S8]
		ADC     A,(IX)        // $DD $8E $00 ; (IX) --> (IX+0)
		ADC     A,IYH         // $FD $8C ; *UNDOCUMENTED*
		ADC     A,IYL         // $FD $8D ; *UNDOCUMENTED*
		ADC     A,(IY+DISPL)  // $FD $8E [2:S8]
		ADC     A,(IY)        // $FD $8E $00 ; (IY) --> (IY+0)
		ADC     A,L           // $8D
		ADC     A,U8          // $CE [2:U8]
		ADC     B             // $88 ; Alternative version of ADC A,B
		ADC     C             // $89 ; Alternative version of ADC A,C
		ADC     D             // $8A ; Alternative version of ADC A,D
		ADC     E             // $8B ; Alternative version of ADC A,E
		ADC     H             // $8C ; Alternative version of ADC A,H
		ADC     HL,BC         // $ED $4A
		ADC     HL,DE         // $ED $5A
		ADC     HL,HL         // $ED $6A
		ADC     HL,SP         // $ED $7A
		ADC     (HL)          // $8E ; Alternative version of ADC A,(HL)
		ADC     IXH           // $DD $8C ; *UNDOCUMENTED*
		ADC     IXL           // $DD $8D ; *UNDOCUMENTED*
		ADC     (IX+DISPL)    // $DD $8E [1:S8] ; Alternative version of ADC A,(IX+DISPL)
		ADC     (IX)          // $DD $8E $00 ; (IX) --> (IX+0), alternative form of ADC A,(IX+0)
		ADC     IYH           // $FD $8C ; *UNDOCUMENTED*
		ADC     IYL           // $FD $8D ; *UNDOCUMENTED*
		ADC     (IY+DISPL)    // $FD $8E [1:S8] ; Alternative version of ADC A,(IY+DISPL)
		ADC     (IY)          // $FD $8E $00 ; (IY) --> (IY+0), alternative form of ADC A,(IY+0)
		ADC     L             // $8D ; Alternative version of ADC A,L
		ADC     U8            // $CE [1:U8] ; Alternative version of ADC A,U8
		ADD     A             // $87 ; Alternative version of ADD A,A
		ADD     A,A           // $87
		ADD     A,B           // $80
		ADD     A,C           // $81
		ADD     A,D           // $82
		ADD     A,E           // $83
		ADD     A,H           // $84
		ADD     A,(HL)        // $86
		ADD     A,IXH         // $DD $84 ; *UNDOCUMENTED*
		ADD     A,IXL         // $DD $85 ; *UNDOCUMENTED*
		ADD     A,(IX+DISPL)  // $DD $86 [2:S8]
		ADD     A,(IX)        // $DD $86 $00 ; (IX) --> (IX+0)
		ADD     A,IYH         // $FD $84 ; *UNDOCUMENTED*
		ADD     A,IYL         // $FD $85 ; *UNDOCUMENTED*
		ADD     A,(IY+DISPL)  // $FD $86 [2:S8]
		ADD     A,(IY)        // $FD $86 $00 ; (IY) --> (IY+0)
		ADD     A,L           // $85
		ADD     A,U8          // $C6 [2:U8]
		ADD     B             // $80 ; Alternative version of ADD A,B
		ADD     C             // $81 ; Alternative version of ADD A,C
		ADD     D             // $82 ; Alternative version of ADD A,D
		ADD     E             // $83 ; Alternative version of ADD A,E
		ADD     H             // $84 ; Alternative version of ADD A,H
		ADD     HL,BC         // $09
		ADD     HL,DE         // $19
		ADD     HL,HL         // $29
		ADD     HL,SP         // $39
		ADD     (HL)          // $86 ; Alternative version of ADD A,(HL)
		ADD     IX,BC         // $DD $09
		ADD     IX,DE         // $DD $19
		ADD     IX,IX         // $DD $29
		ADD     IX,SP         // $DD $39
		ADD     IXH           // $DD $84 ; *UNDOCUMENTED*
		ADD     IXL           // $DD $85 ; *UNDOCUMENTED*
		ADD     (IX+DISPL)    // $DD $86 [1:S8] ; Alternative version of ADD A,(IX+DISPL)
		ADD     (IX)          // $DD $86 $00 ; (IX) --> (IX+0), alternative form of ADD A,(IX+0)
		ADD     IY,BC         // $FD $09
		ADD     IY,DE         // $FD $19
		ADD     IY,IY         // $FD $29
		ADD     IY,SP         // $FD $39
		ADD     IYH           // $FD $84 ; *UNDOCUMENTED*
		ADD     IYL           // $FD $85 ; *UNDOCUMENTED*
		ADD     (IY+DISPL)    // $FD $86 [1:S8] ; Alternative version of ADD A,(IY+DISPL)
		ADD     (IY)          // $FD $86 $00 ; (IY) --> (IY+0), alternative form of ADD A,(IY+0)
		ADD     L             // $85 ; Alternative version of ADD A,L
		ADD     U8            // $C6 [1:U8] ; Alternative version of ADD A,U8
		CPL                   // $2F
		DAA                   // $27
		NEG                   // $ED $44
		SBC     A             // $9F ; Alternative version of SBC A,A
		SBC     A,A           // $9F
		SBC     A,B           // $98
		SBC     A,C           // $99
		SBC     A,D           // $9A
		SBC     A,E           // $9B
		SBC     A,H           // $9C
		SBC     A,(HL)        // $9E
		SBC     A,IXH         // $DD $9C ; *UNDOCUMENTED*
		SBC     A,IXL         // $DD $9D ; *UNDOCUMENTED*
		SBC     A,(IX+DISPL)  // $DD $9E [2:S8]
		SBC     A,(IX)        // $DD $9E $00 ; (IX) --> (IX+0)
		SBC     A,IYH         // $FD $9C ; *UNDOCUMENTED*
		SBC     A,IYL         // $FD $9D ; *UNDOCUMENTED*
		SBC     A,(IY+DISPL)  // $FD $9E [2:S8]
		SBC     A,(IY)        // $FD $9E $00 ; (IY) --> (IY+0)
		SBC     A,L           // $9D
		SBC     A,U8          // $DE [2:U8]
		SBC     B             // $98 ; Alternative version of SBC A,B
		SBC     C             // $99 ; Alternative version of SBC A,C
		SBC     D             // $9A ; Alternative version of SBC A,D
		SBC     E             // $9B ; Alternative version of SBC A,E
		SBC     H             // $9C ; Alternative version of SBC A,H
		SBC     HL,BC         // $ED $42
		SBC     HL,DE         // $ED $52
		SBC     HL,HL         // $ED $62
		SBC     HL,SP         // $ED $72
		SBC     (HL)          // $9E ; Alternative version of SBC A,(HL)
		SBC     IXH           // $DD $9C ; *UNDOCUMENTED*
		SBC     IXL           // $DD $9D ; *UNDOCUMENTED*
		SBC     (IX+DISPL)    // $DD $9E [1:S8] ; Alternative version of SBC A,(IX+DISPL)
		SBC     (IX)          // $DD $9E $00 ; (IX) --> (IX+0), alternative form of SBC A,(IX+0)
		SBC     IYH           // $FD $9C ; *UNDOCUMENTED*
		SBC     IYL           // $FD $9D ; *UNDOCUMENTED*
		SBC     (IY+DISPL)    // $FD $9E [1:S8] ; Alternative version of SBC A,(IY+DISPL)
		SBC     (IY)          // $FD $9E $00 ; (IY) --> (IY+0), alternative form of SBC A,(IY+0)
		SBC     L             // $9D ; Alternative version of SBC A,L
		SBC     U8            // $DE [1:U8] ; Alternative version of SBC A,U8
		SUB     A             // $97 ; Alternative version of SUB A,A
		SUB     A,A           // $97
		SUB     A,B           // $90
		SUB     A,C           // $91
		SUB     A,D           // $92
		SUB     A,E           // $93
		SUB     A,H           // $94
		SUB     A,(HL)        // $96
		SUB     A,IXH         // $DD $94 ; *UNDOCUMENTED*
		SUB     A,IXL         // $DD $95 ; *UNDOCUMENTED*
		SUB     A,(IX+DISPL)  // $DD $96 [2:S8]
		SUB     A,(IX)        // $DD $96 $00 ; (IX) --> (IX+0)
		SUB     A,IYH         // $FD $94 ; *UNDOCUMENTED*
		SUB     A,IYL         // $FD $95 ; *UNDOCUMENTED*
		SUB     A,(IY+DISPL)  // $FD $96 [2:S8]
		SUB     A,(IY)        // $FD $96 $00 ; (IY) --> (IY+0)
		SUB     A,L           // $95
		SUB     A,U8          // $D6 [2:U8]
		SUB     B             // $90 ; Alternative version of SUB A,B
		SUB     C             // $91 ; Alternative version of SUB A,C
		SUB     D             // $92 ; Alternative version of SUB A,D
		SUB     E             // $93 ; Alternative version of SUB A,E
		SUB     H             // $94 ; Alternative version of SUB A,H
		SUB     (HL)          // $96 ; Alternative version of SUB A,(HL)
		SUB     IXH           // $DD $94 ; *UNDOCUMENTED*
		SUB     IXL           // $DD $95 ; *UNDOCUMENTED*
		SUB     (IX+DISPL)    // $DD $96 [1:S8] ; Alternative version of SUB A,(IX+DISPL)
		SUB     (IX)          // $DD $96 $00 ; (IX) --> (IX+0), alternative form of SUB A,(IX+0)
		SUB     IYH           // $FD $94 ; *UNDOCUMENTED*
		SUB     IYL           // $FD $95 ; *UNDOCUMENTED*
		SUB     (IY+DISPL)    // $FD $96 [1:S8] ; Alternative version of SUB A,(IY+DISPL)
		SUB     (IY)          // $FD $96 $00 ; (IY) --> (IY+0), alternative form of SUB A,(IY+0)
		SUB     L             // $95 ; Alternative version of SUB A,L
		SUB     U8            // $D6 [1:U8] ; Alternative version of SUB A,U8


;
; Z80X - Bit instructions
;

		BIT     U8,A          // $CB %01[1:B3]111
		BIT     U8,B          // $CB %01[1:B3]000
		BIT     U8,C          // $CB %01[1:B3]001
		BIT     U8,D          // $CB %01[1:B3]010
		BIT     U8,E          // $CB %01[1:B3]011
		BIT     U8,H          // $CB %01[1:B3]100
		BIT     U8,(HL)       // $CB %01[1:B3]110
		BIT     U8,(IX+DISPL) // $DD $CB [2:S8] %01[1:B3]110
		BIT     U8,(IX)       // $DD $CB $00 %01[1:B3]110 ; (IX) --> (IX+0)
		BIT     U8,(IY+DISPL) // $FD $CB [2:S8] %01[1:B3]110
		BIT     U8,(IY)       // $FD $CB $00 %01[1:B3]110 ; (IY) --> (IY+0)
		BIT     U8,L          // $CB %01[1:B3]101
		RES     U8,A          // $CB %10[1:B3]111
		RES     U8,B          // $CB %10[1:B3]000
		RES     U8,C          // $CB %10[1:B3]001
		RES     U8,D          // $CB %10[1:B3]010
		RES     U8,E          // $CB %10[1:B3]011
		RES     U8,H          // $CB %10[1:B3]100
		RES     U8,(HL)       // $CB %10[1:B3]110
		RES     U8,(IX+DISPL) // $DD $CB [2:S8] %10[1:B3]110
		RES     U8,(IX)       // $DD $CB $00 %10[1:B3]110 ; (IX) --> (IX+0)
		RES     U8,(IY+DISPL) // $FD $CB [2:S8] %10[1:B3]110
		RES     U8,(IY)       // $FD $CB $00 %10[1:B3]110 ; (IY) --> (IY+0)
		RES     U8,L          // $CB %10[1:B3]101
		SET     U8,A          // $CB %11[1:B3]111
		SET     U8,B          // $CB %11[1:B3]000
		SET     U8,C          // $CB %11[1:B3]001
		SET     U8,D          // $CB %11[1:B3]010
		SET     U8,E          // $CB %11[1:B3]011
		SET     U8,H          // $CB %11[1:B3]100
		SET     U8,(HL)       // $CB %11[1:B3]110
		SET     U8,(IX+DISPL) // $DD $CB [2:S8] %11[1:B3]110
		SET     U8,(IX)       // $DD $CB $00 %11[1:B3]110 ; (IX) --> (IX+0)
		SET     U8,(IY+DISPL) // $FD $CB [2:S8] %11[1:B3]110
		SET     U8,(IY)       // $FD $CB $00 %11[1:B3]110 ; (IY) --> (IY+0)
		SET     U8,L          // $CB %11[1:B3]101


;
; Z80X - Compare instructions
;

		CP      A             // $BF
		CP      A,A           // $BF
		CP      A,B           // $B8
		CP      A,C           // $B9
		CP      A,D           // $BA
		CP      A,E           // $BB
		CP      A,H           // $BC
		CP      A,(HL)        // $BE
		CP      A,IXH         // $DD $BC ; *UNDOCUMENTED*
		CP      A,IXL         // $DD $BD ; *UNDOCUMENTED*
		CP      A,(IX+DISPL)  // $DD $BE [2:S8]
		CP      A,(IX)        // $DD $BE $00 ; (IX) --> (IX+0)
		CP      A,IYH         // $FD $BC ; *UNDOCUMENTED*
		CP      A,IYL         // $FD $BD ; *UNDOCUMENTED*
		CP      A,(IY+DISPL)  // $FD $BE [2:S8]
		CP      A,(IY)        // $FD $BE $00 ; (IY) --> (IY+0)
		CP      A,L           // $BD
		CP      A,U8          // $FE [2:U8]
		CP      B             // $B8
		CP      C             // $B9
		CP      D             // $BA
		CP      E             // $BB
		CP      H             // $BC
		CP      (HL)          // $BE
		CP      IXH           // $DD $BC ; *UNDOCUMENTED*
		CP      IXL           // $DD $BD ; *UNDOCUMENTED*
		CP      (IX+DISPL)    // $DD $BE [1:S8]
		CP      (IX)          // $DD $BE $00 ; (IX) --> (IX+0), alternative form of CP A,(IX+0)
		CP      IYH           // $FD $BC ; *UNDOCUMENTED*
		CP      IYL           // $FD $BD ; *UNDOCUMENTED*
		CP      (IY+DISPL)    // $FD $BE [1:S8]
		CP      (IY)          // $FD $BE $00 ; (IY) --> (IY+0), alternative form of CP A,(IY+0)
		CP      L             // $BD
		CP      U8            // $FE [1:U8]
		CPD                   // $ED $A9
		CPDR                  // $ED $B9
		CPI                   // $ED $A1
		CPIR                  // $ED $B1


;
; Z80X - Data move instructions
;

		EX      AF,AF'        // $08
		EX      DE,HL         // $EB
		EX      (SP),HL       // $E3
		EX      (SP),IX       // $DD $E3
		EX      (SP),IY       // $FD $E3
		EXX                   // $D9
		LD      A,A           // $7F
		LD      A,B           // $78
		LD      A,(BC)        // $0A
		LD      A,C           // $79
		LD      A,D           // $7A
		LD      A,(DE)        // $1A
		LD      A,E           // $7B
		LD      A,H           // $7C
		LD      A,(HL)        // $7E
		LD      A,I           // $ED $57
		LD      A,IXH         // $DD $7C ; *UNDOCUMENTED*
		LD      A,IXL         // $DD $7D ; *UNDOCUMENTED*
		LD      A,(IX+DISPL)  // $DD $7E [2:S8]
		LD      A,(IX)        // $DD $7E $00 ; (IX) --> (IX+0)
		LD      A,IYH         // $FD $7C ; *UNDOCUMENTED*
		LD      A,IYL         // $FD $7D ; *UNDOCUMENTED*
		LD      A,(IY+DISPL)  // $FD $7E [2:S8]
		LD      A,(IY)        // $FD $7E $00 ; (IY) --> (IY+0)
		LD      A,L           // $7D
		LD      A,R           // $ED $5F
		LD      A,(U16)       // $3A [2:U16]
		LD      A,U8          // $3E [2:U8]
		LD      B,A           // $47
		LD      B,B           // $40
		LD      B,C           // $41
		LD      B,D           // $42
		LD      B,E           // $43
		LD      B,H           // $44
		LD      B,(HL)        // $46
		LD      B,IXH         // $DD $44 ; *UNDOCUMENTED*
		LD      B,IXL         // $DD $45 ; *UNDOCUMENTED*
		LD      B,(IX+DISPL)  // $DD $46 [2:S8]
		LD      B,(IX)        // $DD $46 $00 ; (IX) --> (IX+0)
		LD      B,IYH         // $FD $44 ; *UNDOCUMENTED*
		LD      B,IYL         // $FD $45 ; *UNDOCUMENTED*
		LD      B,(IY+DISPL)  // $FD $46 [2:S8]
		LD      B,(IY)        // $FD $46 $00 ; (IY) --> (IY+0)
		LD      B,L           // $45
		LD      B,U8          // $06 [2:U8]
		LD      BC,U16        // $01 [2:U16]
		LD      BC,(U16)      // $ED $4B [2:U16]
		LD      (BC),A        // $02
		LD      C,A           // $4F
		LD      C,B           // $48
		LD      C,C           // $49
		LD      C,D           // $4A
		LD      C,E           // $4B
		LD      C,H           // $4C
		LD      C,(HL)        // $4E
		LD      C,IXH         // $DD $4C ; *UNDOCUMENTED*
		LD      C,IXL         // $DD $4D ; *UNDOCUMENTED*
		LD      C,(IX+DISPL)  // $DD $4E [2:S8]
		LD      C,(IX)        // $DD $4E $00 ; (IX) --> (IX+0)
		LD      C,IYH         // $FD $4C ; *UNDOCUMENTED*
		LD      C,IYL         // $FD $4D ; *UNDOCUMENTED*
		LD      C,(IY+DISPL)  // $FD $4E [2:S8]
		LD      C,(IY)        // $FD $4E $00 ; (IY) --> (IY+0)
		LD      C,L           // $4D
		LD      C,U8          // $0E [2:U8]
		LD      D,A           // $57
		LD      D,B           // $50
		LD      D,C           // $51
		LD      D,D           // $52
		LD      D,E           // $53
		LD      D,H           // $54
		LD      D,(HL)        // $56
		LD      D,IXH         // $DD $54 ; *UNDOCUMENTED*
		LD      D,IXL         // $DD $55 ; *UNDOCUMENTED*
		LD      D,(IX+DISPL)  // $DD $56 [2:S8]
		LD      D,(IX)        // $DD $56 $00 ; (IX) --> (IX+0)
		LD      D,IYH         // $FD $54 ; *UNDOCUMENTED*
		LD      D,IYL         // $FD $55 ; *UNDOCUMENTED*
		LD      D,(IY+DISPL)  // $FD $56 [2:S8]
		LD      D,(IY)        // $FD $56 $00 ; (IY) --> (IY+0)
		LD      D,L           // $55
		LD      D,U8          // $16 [2:U8]
		LD      DE,U16        // $11 [2:U16]
		LD      DE,(U16)      // $ED $5B [2:U16]
		LD      (DE),A        // $12
		LD      E,A           // $5F
		LD      E,B           // $58
		LD      E,C           // $59
		LD      E,D           // $5A
		LD      E,E           // $5B
		LD      E,H           // $5C
		LD      E,(HL)        // $5E
		LD      E,IXH         // $DD $5C ; *UNDOCUMENTED*
		LD      E,IXL         // $DD $5D ; *UNDOCUMENTED*
		LD      E,(IX+DISPL)  // $DD $5E [2:S8]
		LD      E,(IX)        // $DD $5E $00 ; (IX) --> (IX+0)
		LD      E,IYH         // $FD $5C ; *UNDOCUMENTED*
		LD      E,IYL         // $FD $5D ; *UNDOCUMENTED*
		LD      E,(IY+DISPL)  // $FD $5E [2:S8]
		LD      E,(IY)        // $FD $5E $00 ; (IY) --> (IY+0)
		LD      E,L           // $5D
		LD      E,U8          // $1E [2:U8]
		LD      H,A           // $67
		LD      H,B           // $60
		LD      H,C           // $61
		LD      H,D           // $62
		LD      H,E           // $63
		LD      H,H           // $64
		LD      H,(HL)        // $66
		LD      H,(IX+DISPL)  // $DD $66 [2:S8]
		LD      H,(IX)        // $DD $66 $00 ; (IX) --> (IX+0)
		LD      H,(IY+DISPL)  // $FD $66 [2:S8]
		LD      H,(IY)        // $FD $66 $00 ; (IY) --> (IY+0)
		LD      H,L           // $65
		LD      H,U8          // $26 [2:U8]
		LD      HL,U16        // $21 [2:U16]
		LD      HL,(U16)      // $2A [2:U16]
		LD      (HL),A        // $77
		LD      (HL),B        // $70
		LD      (HL),C        // $71
		LD      (HL),D        // $72
		LD      (HL),E        // $73
		LD      (HL),H        // $74
		LD      (HL),L        // $75
		LD      (HL),U8       // $36 [2:U8]
		LD      I,A           // $ED $47
		LD      IX,U16        // $DD $21 [2:U16]
		LD      IX,(U16)      // $DD $2A [2:U16]
		LD      IXH,A         // $DD $67 ; *UNDOCUMENTED*
		LD      IXH,B         // $DD $60 ; *UNDOCUMENTED*
		LD      IXH,C         // $DD $61 ; *UNDOCUMENTED*
		LD      IXH,D         // $DD $62 ; *UNDOCUMENTED*
		LD      IXH,E         // $DD $63 ; *UNDOCUMENTED*
		LD      IXH,IXH       // $DD $64 ; *UNDOCUMENTED*
		LD      IXH,IXL       // $DD $65 ; *UNDOCUMENTED*
		LD      IXH,U8        // $DD $26 [2:U8] ; *UNDOCUMENTED*
		LD      IXL,A         // $DD $6F ; *UNDOCUMENTED*
		LD      IXL,B         // $DD $68 ; *UNDOCUMENTED*
		LD      IXL,C         // $DD $69 ; *UNDOCUMENTED*
		LD      IXL,D         // $DD $6A ; *UNDOCUMENTED*
		LD      IXL,E         // $DD $6B ; *UNDOCUMENTED*
		LD      IXL,IXH       // $DD $6C ; *UNDOCUMENTED*
		LD      IXL,IXL       // $DD $6D ; *UNDOCUMENTED*
		LD      IXL,U8        // $DD $2E [2:U8] ; *UNDOCUMENTED*
		LD      (IX+DISPL),A  // $DD $77 [1:S8]
		LD      (IX+DISPL),B  // $DD $70 [1:S8]
		LD      (IX+DISPL),C  // $DD $71 [1:S8]
		LD      (IX+DISPL),D  // $DD $72 [1:S8]
		LD      (IX+DISPL),E  // $DD $73 [1:S8]
		LD      (IX+DISPL),H  // $DD $74 [1:S8]
		LD      (IX+DISPL),L  // $DD $75 [1:S8]
		LD      (IX+DISPL),U8 // $DD $36 [1:S8] [2:U8]
		LD      (IX),A        // $DD $77 $00 ; (IX) --> (IX+0)
		LD      (IX),B        // $DD $70 $00 ; (IX) --> (IX+0)
		LD      (IX),C        // $DD $71 $00 ; (IX) --> (IX+0)
		LD      (IX),D        // $DD $72 $00 ; (IX) --> (IX+0)
		LD      (IX),E        // $DD $73 $00 ; (IX) --> (IX+0)
		LD      (IX),H        // $DD $74 $00 ; (IX) --> (IX+0)
		LD      (IX),L        // $DD $75 $00 ; (IX) --> (IX+0)
		LD      (IX),U8       // $DD $36 $00 [2:U8] ; (IX) --> (IX+0)
		LD      IY,U16        // $FD $21 [2:U16]
		LD      IY,(U16)      // $FD $2A [2:U16]
		LD      IYH,A         // $FD $67 ; *UNDOCUMENTED*
		LD      IYH,B         // $FD $60 ; *UNDOCUMENTED*
		LD      IYH,C         // $FD $61 ; *UNDOCUMENTED*
		LD      IYH,D         // $FD $62 ; *UNDOCUMENTED*
		LD      IYH,E         // $FD $63 ; *UNDOCUMENTED*
		LD      IYH,IYH       // $FD $64 ; *UNDOCUMENTED*
		LD      IYH,IYL       // $FD $65 ; *UNDOCUMENTED*
		LD      IYH,U8        // $FD $26 [2:U8] ; *UNDOCUMENTED*
		LD      IYL,A         // $FD $6F ; *UNDOCUMENTED*
		LD      IYL,B         // $FD $68 ; *UNDOCUMENTED*
		LD      IYL,C         // $FD $69 ; *UNDOCUMENTED*
		LD      IYL,D         // $FD $6A ; *UNDOCUMENTED*
		LD      IYL,E         // $FD $6B ; *UNDOCUMENTED*
		LD      IYL,IYH       // $FD $6C ; *UNDOCUMENTED*
		LD      IYL,IYL       // $FD $6D ; *UNDOCUMENTED*
		LD      IYL,U8        // $FD $2E [2:U8] ; *UNDOCUMENTED*
		LD      (IY+DISPL),A  // $FD $77 [1:S8]
		LD      (IY+DISPL),B  // $FD $70 [1:S8]
		LD      (IY+DISPL),C  // $FD $71 [1:S8]
		LD      (IY+DISPL),D  // $FD $72 [1:S8]
		LD      (IY+DISPL),E  // $FD $73 [1:S8]
		LD      (IY+DISPL),H  // $FD $74 [1:S8]
		LD      (IY+DISPL),L  // $FD $75 [1:S8]
		LD      (IY+DISPL),U8 // $FD $36 [1:S8] [2:U8]
		LD      (IY),A        // $FD $77 $00 ; (IY) --> (IY+0)
		LD      (IY),B        // $FD $70 $00 ; (IY) --> (IY+0)
		LD      (IY),C        // $FD $71 $00 ; (IY) --> (IY+0)
		LD      (IY),D        // $FD $72 $00 ; (IY) --> (IY+0)
		LD      (IY),E        // $FD $73 $00 ; (IY) --> (IY+0)
		LD      (IY),H        // $FD $74 $00 ; (IY) --> (IY+0)
		LD      (IY),L        // $FD $75 $00 ; (IY) --> (IY+0)
		LD      (IY),U8       // $FD $36 $00 [2:U8] ; (IY) --> (IY+0)
		LD      L,A           // $6F
		LD      L,B           // $68
		LD      L,C           // $69
		LD      L,D           // $6A
		LD      L,E           // $6B
		LD      L,H           // $6C
		LD      L,(HL)        // $6E
		LD      L,(IX+DISPL)  // $DD $6E [2:S8]
		LD      L,(IX)        // $DD $6E $00 ; (IX) --> (IX+0)
		LD      L,(IY+DISPL)  // $FD $6E [2:S8]
		LD      L,(IY)        // $FD $6E $00 ; (IY) --> (IY+0)
		LD      L,L           // $6D
		LD      L,U8          // $2E [2:U8]
		LD      R,A           // $ED $4F
		LD      SP,HL         // $F9
		LD      SP,IX         // $DD $F9
		LD      SP,IY         // $FD $F9
		LD      SP,U16        // $31 [2:U16]
		LD      SP,(U16)      // $ED $7B [2:U16]
		LD      (U16),A       // $32 [1:U16]
		LD      (U16),BC      // $ED $43 [1:U16]
		LD      (U16),DE      // $ED $53 [1:U16]
		LD      (U16),HL      // $22 [1:U16]
		LD      (U16),IX      // $DD $22 [1:U16]
		LD      (U16),IY      // $FD $22 [1:U16]
		LD      (U16),SP      // $ED $73 [1:U16]
		LDD                   // $ED $A8
		LDDR                  // $ED $B8
		LDI                   // $ED $A0
		LDIR                  // $ED $B0


;
; Z80X - Flag instructions
;

		CCF                   // $3F
		DI                    // $F3
		EI                    // $FB
		SCF                   // $37


;
; Z80X - Increment / decrement instructions
;

		DEC     A             // $3D
		DEC     B             // $05
		DEC     BC            // $0B
		DEC     C             // $0D
		DEC     D             // $15
		DEC     DE            // $1B
		DEC     E             // $1D
		DEC     H             // $25
		DEC     HL            // $2B
		DEC     (HL)          // $35
		DEC     IX            // $DD $2B
		DEC     IXH           // $DD $25 ; *UNDOCUMENTED*
		DEC     IXL           // $DD $2D ; *UNDOCUMENTED*
		DEC     (IX+DISPL)    // $DD $35 [1:S8]
		DEC     (IX)          // $DD $35 $00 ; (IX) --> (IX+0)
		DEC     IY            // $FD $2B
		DEC     IYH           // $FD $25 ; *UNDOCUMENTED*
		DEC     IYL           // $FD $2D ; *UNDOCUMENTED*
		DEC     (IY+DISPL)    // $FD $35 [1:S8]
		DEC     (IY)          // $FD $35 $00 ; (IY) --> (IY+0)
		DEC     L             // $2D
		DEC     SP            // $3B
		INC     A             // $3C
		INC     B             // $04
		INC     BC            // $03
		INC     C             // $0C
		INC     D             // $14
		INC     DE            // $13
		INC     E             // $1C
		INC     H             // $24
		INC     HL            // $23
		INC     (HL)          // $34
		INC     IX            // $DD $23
		INC     IXH           // $DD $24 ; *UNDOCUMENTED*
		INC     IXL           // $DD $2C ; *UNDOCUMENTED*
		INC     (IX+DISPL)    // $DD $34 [1:S8]
		INC     (IX)          // $DD $34 $00 ; (IX) --> (IX+0)
		INC     IY            // $FD $23
		INC     IYH           // $FD $24 ; *UNDOCUMENTED*
		INC     IYL           // $FD $2C ; *UNDOCUMENTED*
		INC     (IY+DISPL)    // $FD $34 [1:S8]
		INC     (IY)          // $FD $34 $00 ; (IY) --> (IY+0)
		INC     L             // $2C
		INC     SP            // $33


;
; Z80X - Logical instructions
;

		AND     A             // $A7 ; Alternative version of AND A,A
		AND     A,A           // $A7
		AND     A,B           // $A0
		AND     A,C           // $A1
		AND     A,D           // $A2
		AND     A,E           // $A3
		AND     A,H           // $A4
		AND     A,(HL)        // $A6
		AND     A,IXH         // $DD $A4 ; *UNDOCUMENTED*
		AND     A,IXL         // $DD $A5 ; *UNDOCUMENTED*
		AND     A,(IX+DISPL)  // $DD $A6 [2:S8]
		AND     A,(IX)        // $DD $A6 $00 ; (IX) --> (IX+0)
		AND     A,IYH         // $FD $A4 ; *UNDOCUMENTED*
		AND     A,IYL         // $FD $A5 ; *UNDOCUMENTED*
		AND     A,(IY+DISPL)  // $FD $A6 [2:S8]
		AND     A,(IY)        // $FD $A6 $00 ; (IY) --> (IY+0)
		AND     A,L           // $A5
		AND     A,U8          // $E6 [2:U8]
		AND     B             // $A0 ; Alternative version of AND A,B
		AND     C             // $A1 ; Alternative version of AND A,C
		AND     D             // $A2 ; Alternative version of AND A,D
		AND     E             // $A3 ; Alternative version of AND A,E
		AND     H             // $A4 ; Alternative version of AND A,H
		AND     (HL)          // $A6 ; Alternative version of AND A,(HL)
		AND     IXH           // $DD $A4 ; *UNDOCUMENTED*
		AND     IXL           // $DD $A5 ; *UNDOCUMENTED*
		AND     (IX+DISPL)    // $DD $A6 [1:S8] ; Alternative version of AND A,(IX+DISPL)
		AND     (IX)          // $DD $A6 $00 ; (IX) --> (IX+0), alternative form of AND A,(IX+0)
		AND     IYH           // $FD $A4 ; *UNDOCUMENTED*
		AND     IYL           // $FD $A5 ; *UNDOCUMENTED*
		AND     (IY+DISPL)    // $FD $A6 [1:S8] ; Alternative version of AND A,(IY+DISPL)
		AND     (IY)          // $FD $A6 $00 ; (IY) --> (IY+0), alternative form of AND A,(IY+0)
		AND     L             // $A5 ; Alternative version of AND A,L
		AND     U8            // $E6 [1:U8] ; Alternative version of AND A,U8
		OR      A             // $B7 ; Alternative version of OR A,A
		OR      A,A           // $B7
		OR      A,B           // $B0
		OR      A,C           // $B1
		OR      A,D           // $B2
		OR      A,E           // $B3
		OR      A,H           // $B4
		OR      A,(HL)        // $B6
		OR      A,IXH         // $DD $B4 ; *UNDOCUMENTED*
		OR      A,IXL         // $DD $B5 ; *UNDOCUMENTED*
		OR      A,(IX+DISPL)  // $DD $B6 [2:S8]
		OR      A,(IX)        // $DD $B6 $00 ; (IX) --> (IX+0)
		OR      A,IYH         // $FD $B4 ; *UNDOCUMENTED*
		OR      A,IYL         // $FD $B5 ; *UNDOCUMENTED*
		OR      A,(IY+DISPL)  // $FD $B6 [2:S8]
		OR      A,(IY)        // $FD $B6 $00 ; (IY) --> (IY+0)
		OR      A,L           // $B5
		OR      A,U8          // $F6 [2:U8]
		OR      B             // $B0 ; Alternative version of OR A,B
		OR      C             // $B1 ; Alternative version of OR A,C
		OR      D             // $B2 ; Alternative version of OR A,D
		OR      E             // $B3 ; Alternative version of OR A,E
		OR      H             // $B4 ; Alternative version of OR A,H
		OR      (HL)          // $B6 ; Alternative version of OR A,(HL)
		OR      IXH           // $DD $B4 ; *UNDOCUMENTED*
		OR      IXL           // $DD $B5 ; *UNDOCUMENTED*
		OR      (IX+DISPL)    // $DD $B6 [1:S8] ; Alternative version of OR A,(IX+DISPL)
		OR      (IX)          // $DD $B6 $00 ; (IX) --> (IX+0), alternative form of OR A,(IX+0)
		OR      IYH           // $FD $B4 ; *UNDOCUMENTED*
		OR      IYL           // $FD $B5 ; *UNDOCUMENTED*
		OR      (IY+DISPL)    // $FD $B6 [1:S8] ; Alternative version of OR A,(IY+DISPL)
		OR      (IY)          // $FD $B6 $00 ; (IY) --> (IY+0), alternative form of OR A,(IY+0)
		OR      L             // $B5 ; Alternative version of OR A,L
		OR      U8            // $F6 [1:U8] ; Alternative version of OR A,U8
		XOR     A             // $AF ; Alternative version of XOR A,A
		XOR     A,A           // $AF
		XOR     A,B           // $A8
		XOR     A,C           // $A9
		XOR     A,D           // $AA
		XOR     A,E           // $AB
		XOR     A,H           // $AC
		XOR     A,(HL)        // $AE
		XOR     A,IXH         // $DD $AC ; *UNDOCUMENTED*
		XOR     A,IXL         // $DD $AD ; *UNDOCUMENTED*
		XOR     A,(IX+DISPL)  // $DD $AE [2:S8]
		XOR     A,(IX)        // $DD $AE $00 ; (IX) --> (IX+0)
		XOR     A,IYH         // $FD $AC ; *UNDOCUMENTED*
		XOR     A,IYL         // $FD $AD ; *UNDOCUMENTED*
		XOR     A,(IY+DISPL)  // $FD $AE [2:S8]
		XOR     A,(IY)        // $FD $AE $00 ; (IY) --> (IY+0)
		XOR     A,L           // $AD
		XOR     A,U8          // $EE [2:U8]
		XOR     B             // $A8 ; Alternative version of XOR A,B
		XOR     C             // $A9 ; Alternative version of XOR A,C
		XOR     D             // $AA ; Alternative version of XOR A,D
		XOR     E             // $AB ; Alternative version of XOR A,E
		XOR     H             // $AC ; Alternative version of XOR A,H
		XOR     (HL)          // $AE ; Alternative version of XOR A,(HL)
		XOR     IXH           // $DD $AC ; *UNDOCUMENTED*
		XOR     IXL           // $DD $AD ; *UNDOCUMENTED*
		XOR     (IX+DISPL)    // $DD $AE [1:S8] ; Alternative version of XOR A,(IX+DISPL)
		XOR     (IX)          // $DD $AE $00 ; (IX) --> (IX+0), alternative form of XOR A,(IX+0)
		XOR     IYH           // $FD $AC ; *UNDOCUMENTED*
		XOR     IYL           // $FD $AD ; *UNDOCUMENTED*
		XOR     (IY+DISPL)    // $FD $AE [1:S8] ; Alternative version of XOR A,(IY+DISPL)
		XOR     (IY)          // $FD $AE $00 ; (IY) --> (IY+0), alternative form of XOR A,(IY+0)
		XOR     L             // $AD ; Alternative version of XOR A,L
		XOR     U8            // $EE [1:U8] ; Alternative version of XOR A,U8


;
; Z80X - Port instructions
;

		IN      A,(C)         // $ED $78
		IN      A,(U8)        // $DB [2:U8]
		IN      B,(C)         // $ED $40
		IN      C,(C)         // $ED $48
		IN      (C)           // $ED $70 ; *UNDOCUMENTED*
		IN      D,(C)         // $ED $50
		IN      E,(C)         // $ED $58
		IN      H,(C)         // $ED $60
		IN      L,(C)         // $ED $68
		IND                   // $ED $AA
		INDR                  // $ED $BA
		INI                   // $ED $A2
		INIR                  // $ED $B2
		OTDR                  // $ED $BB
		OTIR                  // $ED $B3
		OUT     (C)      	  // $ED $71 ; *UNDOCUMENTED* Same as OUT (C),0
		OUT     (C),A         // $ED $79
		OUT     (C),B         // $ED $41
		OUT     (C),C         // $ED $49
		OUT     (C),D         // $ED $51
		OUT     (C),E         // $ED $59
		OUT     (C),H         // $ED $61
		OUT     (C),L         // $ED $69
		OUT     (C),U8        // $ED $71 ; *UNDOCUMENTED* (U8 value will be treated as 0 in all cases)
		OUT     (U8),A        // $D3 [1:U8]
		OUTD                  // $ED $AB
		OUTI                  // $ED $A3


;
; Z80X - Program flow instructions
;

		CALL    C,U16         // $DC [2:U16]
		CALL    M,U16         // $FC [2:U16]
		CALL    NC,U16        // $D4 [2:U16]
		CALL    NZ,U16        // $C4 [2:U16]
		CALL    P,U16         // $F4 [2:U16]
		CALL    PE,U16        // $EC [2:U16]
		CALL    PO,U16        // $E4 [2:U16]
		CALL    U16           // $CD [1:U16]
		CALL    Z,U16         // $CC [2:U16]
		DJNZ    $+125         // $10 [1:R8]
		HALT                  // $76
		JP      C,U16         // $DA [2:U16]
		JP      (HL)          // $E9
		JP      (IX)          // $DD $E9
		JP      (IY)          // $FD $E9
		JP      M,U16         // $FA [2:U16]
		JP      NC,U16        // $D2 [2:U16]
		JP      NZ,U16        // $C2 [2:U16]
		JP      P,U16         // $F2 [2:U16]
		JP      PE,U16        // $EA [2:U16]
		JP      PO,U16        // $E2 [2:U16]
		JP      U16           // $C3 [1:U16]
		JP      Z,U16         // $CA [2:U16]
		JR      C,$+125       // $38 [2:R8]
		JR      NC,$+125      // $30 [2:R8]
		JR      NZ,$+125      // $20 [2:R8]
		JR      $+125         // $18 [1:R8]
		JR      Z,$+125       // $28 [2:R8]
		RET                   // $C9
		RET     C             // $D8
		RET     M             // $F8
		RET     NC            // $D0
		RET     NZ            // $C0
		RET     P             // $F0
		RET     PE            // $E8
		RET     PO            // $E0
		RET     Z             // $C8
		RETI                  // $ED $4D
		RETN                  // $ED $45
		RST 	0          	  // %11[1:RST]111             
		RST 	1          	  // %11[1:RST]111             
		RST 	2          	  // %11[1:RST]111             
		RST 	3          	  // %11[1:RST]111             
		RST 	4          	  // %11[1:RST]111             
		RST 	5          	  // %11[1:RST]111             
		RST 	6          	  // %11[1:RST]111             
		RST 	7          	  // %11[1:RST]111             
		RST 	08H        	  // %11[1:RST]111             
		RST 	10H        	  // %11[1:RST]111             
		RST 	18H        	  // %11[1:RST]111             
		RST 	20H        	  // %11[1:RST]111             
		RST 	28H        	  // %11[1:RST]111             
		RST 	30H        	  // %11[1:RST]111             
		RST 	38H        	  // %11[1:RST]111             


;
; Z80X - Shift / rotate instructions
;

		RL      A             // $CB $17
		RL      B             // $CB $10
		RL      C             // $CB $11
		RL      D             // $CB $12
		RL      E             // $CB $13
		RL      H             // $CB $14
		RL      (HL)          // $CB $16
		RL      (IX+DISPL)    // $DD $CB [1:S8] $16
		RL      (IX)          // $DD $CB $00 $16 ; (IX) --> (IX+0)
		RL      (IY+DISPL)    // $FD $CB [1:S8] $16
		RL      (IY)          // $FD $CB $00 $16 ; (IY) --> (IY+0)
		RL      L             // $CB $15
		RLA                   // $17
		RLC     A             // $CB $07
		RLC     B             // $CB $00
		RLC     C             // $CB $01
		RLC     D             // $CB $02
		RLC     E             // $CB $03
		RLC     H             // $CB $04
		RLC     (HL)          // $CB $06
		RLC     (IX+DISPL)    // $DD $CB [1:S8] $06
		RLC     (IX)          // $DD $CB $00 $06 ; (IX) --> (IX+0)
		RLC     (IY+DISPL)    // $FD $CB [1:S8] $06
		RLC     (IY)          // $FD $CB $00 $06 ; (IY) --> (IY+0)
		RLC     L             // $CB $05
		RLCA                  // $07
		RLD                   // $ED $6F
		RR      A             // $CB $1F
		RR      B             // $CB $18
		RR      C             // $CB $19
		RR      D             // $CB $1A
		RR      E             // $CB $1B
		RR      H             // $CB $1C
		RR      (HL)          // $CB $1E
		RR      (IX+DISPL)    // $DD $CB [1:S8] $1E
		RR      (IX)          // $DD $CB $00 $1E ; (IX) --> (IX+0)
		RR      (IY+DISPL)    // $FD $CB [1:S8] $1E
		RR      (IY)          // $FD $CB $00 $1E ; (IY) --> (IY+0)
		RR      L             // $CB $1D
		RRA                   // $1F
		RRC     A             // $CB $0F
		RRC     B             // $CB $08
		RRC     C             // $CB $09
		RRC     D             // $CB $0A
		RRC     E             // $CB $0B
		RRC     H             // $CB $0C
		RRC     (HL)          // $CB $0E
		RRC     (IX+DISPL)    // $DD $CB [1:S8] $0E
		RRC     (IX)          // $DD $CB $00 $0E ; (IX) --> (IX+0)
		RRC     (IY+DISPL)    // $FD $CB [1:S8] $0E
		RRC     (IY)          // $FD $CB $00 $0E ; (IY) --> (IY+0)
		RRC     L             // $CB $0D
		RRCA                  // $0F
		RRD                   // $ED $67
		SLA     A             // $CB $27
		SLA     B             // $CB $20
		SLA     C             // $CB $21
		SLA     D             // $CB $22
		SLA     E             // $CB $23
		SLA     H             // $CB $24
		SLA     (HL)          // $CB $26
		SLA     (IX+DISPL)    // $DD $CB [1:S8] $26
		SLA     (IX)          // $DD $CB $00 $26 ; (IX) --> (IX+0)
		SLA     (IY+DISPL)    // $FD $CB [1:S8] $26
		SLA     (IY)          // $FD $CB $00 $26 ; (IY) --> (IY+0)
		SLA     L             // $CB $25
		SRA     A             // $CB $2F
		SRA     B             // $CB $28
		SRA     C             // $CB $29
		SRA     D             // $CB $2A
		SRA     E             // $CB $2B
		SRA     H             // $CB $2C
		SRA     (HL)          // $CB $2E
		SRA     (IX+DISPL)    // $DD $CB [1:S8] $2E
		SRA     (IX)          // $DD $CB $00 $2E ; (IX) --> (IX+0)
		SRA     (IY+DISPL)    // $FD $CB [1:S8] $2E
		SRA     (IY)          // $FD $CB $00 $2E ; (IY) --> (IY+0)
		SRA     L             // $CB $2D
		SRL     A             // $CB $3F
		SRL     B             // $CB $38
		SRL     C             // $CB $39
		SRL     D             // $CB $3A
		SRL     E             // $CB $3B
		SRL     H             // $CB $3C
		SRL     (HL)          // $CB $3E
		SRL     (IX+DISPL)    // $DD $CB [1:S8] $3E
		SRL     (IX)          // $DD $CB $00 $3E ; (IX) --> (IX+0)
		SRL     (IY+DISPL)    // $FD $CB [1:S8] $3E
		SRL     (IY)          // $FD $CB $00 $3E ; (IY) --> (IY+0)
		SRL     L             // $CB $3D


;
; Z80X - Special instructions
;

		IM      0             // $ED $46
		IM      1             // $ED $56
		IM      2             // $ED $5E
		NOP                   // $00


;
; Z80X - Stack instructions
;

		POP     AF            // $F1
		POP     BC            // $C1
		POP     DE            // $D1
		POP     HL            // $E1
		POP     IX            // $DD $E1
		POP     IY            // $FD $E1
		PUSH    AF            // $F5
		PUSH    BC            // $C5
		PUSH    DE            // $D5
		PUSH    HL            // $E5
		PUSH    IX            // $DD $E5
		PUSH    IY            // $FD $E5


;
; Z80X - UNDOCUMENTED Bit instructions
;

		RES0LDA (IX+DISPL)    // $DD $CB [1:S8] $87 ; *UNDOCUMENTED* RES 0,(IX+N) then LD A,(IX+N)
		RES0LDA (IY+DISPL)    // $FD $CB [1:S8] $87 ; *UNDOCUMENTED* RES 0,(IY+N) then LD A,(IY+N)
		RES0LDB (IX+DISPL)    // $DD $CB [1:S8] $80 ; *UNDOCUMENTED* RES 0,(IX+N) then LD B,(IX+N)
		RES0LDB (IY+DISPL)    // $FD $CB [1:S8] $80 ; *UNDOCUMENTED* RES 0,(IY+N) then LD B,(IY+N)
		RES0LDC (IX+DISPL)    // $DD $CB [1:S8] $81 ; *UNDOCUMENTED* RES 0,(IX+N) then LD C,(IX+N)
		RES0LDC (IY+DISPL)    // $FD $CB [1:S8] $81 ; *UNDOCUMENTED* RES 0,(IY+N) then LD C,(IY+N)
		RES0LDD (IX+DISPL)    // $DD $CB [1:S8] $82 ; *UNDOCUMENTED* RES 0,(IX+N) then LD D,(IX+N)
		RES0LDD (IY+DISPL)    // $FD $CB [1:S8] $82 ; *UNDOCUMENTED* RES 0,(IY+N) then LD D,(IY+N)
		RES0LDE (IX+DISPL)    // $DD $CB [1:S8] $83 ; *UNDOCUMENTED* RES 0,(IX+N) then LD E,(IX+N)
		RES0LDE (IY+DISPL)    // $FD $CB [1:S8] $83 ; *UNDOCUMENTED* RES 0,(IY+N) then LD E,(IY+N)
		RES0LDF (IX+DISPL)    // $DD $CB [1:S8] $86 ; *UNDOCUMENTED* RES 0,(IX+N) then LD F,(IX+N)
		RES0LDF (IY+DISPL)    // $FD $CB [1:S8] $86 ; *UNDOCUMENTED* RES 0,(IY+N) then LD F,(IY+N)
		RES0LDH (IX+DISPL)    // $DD $CB [1:S8] $84 ; *UNDOCUMENTED* RES 0,(IX+N) then LD H,(IX+N)
		RES0LDH (IY+DISPL)    // $FD $CB [1:S8] $84 ; *UNDOCUMENTED* RES 0,(IY+N) then LD H,(IY+N)
		RES0LDL (IX+DISPL)    // $DD $CB [1:S8] $85 ; *UNDOCUMENTED* RES 0,(IX+N) then LD L,(IX+N)
		RES0LDL (IY+DISPL)    // $FD $CB [1:S8] $85 ; *UNDOCUMENTED* RES 0,(IY+N) then LD L,(IY+N)
		RES1LDA (IX+DISPL)    // $DD $CB [1:S8] $8F ; *UNDOCUMENTED* RES 1,(IX+N) then LD A,(IX+N)
		RES1LDA (IY+DISPL)    // $FD $CB [1:S8] $8F ; *UNDOCUMENTED* RES 1,(IY+N) then LD A,(IY+N)
		RES1LDB (IX+DISPL)    // $DD $CB [1:S8] $88 ; *UNDOCUMENTED* RES 1,(IX+N) then LD B,(IX+N)
		RES1LDB (IY+DISPL)    // $FD $CB [1:S8] $88 ; *UNDOCUMENTED* RES 1,(IY+N) then LD B,(IY+N)
		RES1LDC (IX+DISPL)    // $DD $CB [1:S8] $89 ; *UNDOCUMENTED* RES 1,(IX+N) then LD C,(IX+N)
		RES1LDC (IY+DISPL)    // $FD $CB [1:S8] $89 ; *UNDOCUMENTED* RES 1,(IY+N) then LD C,(IY+N)
		RES1LDD (IX+DISPL)    // $DD $CB [1:S8] $8A ; *UNDOCUMENTED* RES 1,(IX+N) then LD D,(IX+N)
		RES1LDD (IY+DISPL)    // $FD $CB [1:S8] $8A ; *UNDOCUMENTED* RES 1,(IY+N) then LD D,(IY+N)
		RES1LDE (IX+DISPL)    // $DD $CB [1:S8] $8B ; *UNDOCUMENTED* RES 1,(IX+N) then LD E,(IX+N)
		RES1LDE (IY+DISPL)    // $FD $CB [1:S8] $8B ; *UNDOCUMENTED* RES 1,(IY+N) then LD E,(IY+N)
		RES1LDF (IX+DISPL)    // $DD $CB [1:S8] $8E ; *UNDOCUMENTED* RES 1,(IX+N) then LD F,(IX+N)
		RES1LDF (IY+DISPL)    // $FD $CB [1:S8] $8E ; *UNDOCUMENTED* RES 1,(IY+N) then LD F,(IY+N)
		RES1LDH (IX+DISPL)    // $DD $CB [1:S8] $8C ; *UNDOCUMENTED* RES 1,(IX+N) then LD H,(IX+N)
		RES1LDH (IY+DISPL)    // $FD $CB [1:S8] $8C ; *UNDOCUMENTED* RES 1,(IY+N) then LD H,(IY+N)
		RES1LDL (IX+DISPL)    // $DD $CB [1:S8] $8D ; *UNDOCUMENTED* RES 1,(IX+N) then LD L,(IX+N)
		RES1LDL (IY+DISPL)    // $FD $CB [1:S8] $8D ; *UNDOCUMENTED* RES 1,(IY+N) then LD L,(IY+N)
		RES2LDA (IX+DISPL)    // $DD $CB [1:S8] $97 ; *UNDOCUMENTED* RES 2,(IX+N) then LD A,(IX+N)
		RES2LDA (IY+DISPL)    // $FD $CB [1:S8] $97 ; *UNDOCUMENTED* RES 2,(IY+N) then LD A,(IY+N)
		RES2LDB (IX+DISPL)    // $DD $CB [1:S8] $90 ; *UNDOCUMENTED* RES 2,(IX+N) then LD B,(IX+N)
		RES2LDB (IY+DISPL)    // $FD $CB [1:S8] $90 ; *UNDOCUMENTED* RES 2,(IY+N) then LD B,(IY+N)
		RES2LDC (IX+DISPL)    // $DD $CB [1:S8] $91 ; *UNDOCUMENTED* RES 2,(IX+N) then LD C,(IX+N)
		RES2LDC (IY+DISPL)    // $FD $CB [1:S8] $91 ; *UNDOCUMENTED* RES 2,(IY+N) then LD C,(IY+N)
		RES2LDD (IX+DISPL)    // $DD $CB [1:S8] $92 ; *UNDOCUMENTED* RES 2,(IX+N) then LD D,(IX+N)
		RES2LDD (IY+DISPL)    // $FD $CB [1:S8] $92 ; *UNDOCUMENTED* RES 2,(IY+N) then LD D,(IY+N)
		RES2LDE (IX+DISPL)    // $DD $CB [1:S8] $93 ; *UNDOCUMENTED* RES 2,(IX+N) then LD E,(IX+N)
		RES2LDE (IY+DISPL)    // $FD $CB [1:S8] $93 ; *UNDOCUMENTED* RES 2,(IY+N) then LD E,(IY+N)
		RES2LDF (IX+DISPL)    // $DD $CB [1:S8] $96 ; *UNDOCUMENTED* RES 2,(IX+N) then LD F,(IX+N)
		RES2LDF (IY+DISPL)    // $FD $CB [1:S8] $96 ; *UNDOCUMENTED* RES 2,(IY+N) then LD F,(IY+N)
		RES2LDH (IX+DISPL)    // $DD $CB [1:S8] $94 ; *UNDOCUMENTED* RES 2,(IX+N) then LD H,(IX+N)
		RES2LDH (IY+DISPL)    // $FD $CB [1:S8] $94 ; *UNDOCUMENTED* RES 2,(IY+N) then LD H,(IY+N)
		RES2LDL (IX+DISPL)    // $DD $CB [1:S8] $95 ; *UNDOCUMENTED* RES 2,(IX+N) then LD L,(IX+N)
		RES2LDL (IY+DISPL)    // $FD $CB [1:S8] $95 ; *UNDOCUMENTED* RES 2,(IY+N) then LD L,(IY+N)
		RES3LDA (IX+DISPL)    // $DD $CB [1:S8] $9F ; *UNDOCUMENTED* RES 3,(IX+N) then LD A,(IX+N)
		RES3LDA (IY+DISPL)    // $FD $CB [1:S8] $9F ; *UNDOCUMENTED* RES 3,(IY+N) then LD A,(IY+N)
		RES3LDB (IX+DISPL)    // $DD $CB [1:S8] $98 ; *UNDOCUMENTED* RES 3,(IX+N) then LD B,(IX+N)
		RES3LDB (IY+DISPL)    // $FD $CB [1:S8] $98 ; *UNDOCUMENTED* RES 3,(IY+N) then LD B,(IY+N)
		RES3LDC (IX+DISPL)    // $DD $CB [1:S8] $99 ; *UNDOCUMENTED* RES 3,(IX+N) then LD C,(IX+N)
		RES3LDC (IY+DISPL)    // $FD $CB [1:S8] $99 ; *UNDOCUMENTED* RES 3,(IY+N) then LD C,(IY+N)
		RES3LDD (IX+DISPL)    // $DD $CB [1:S8] $9A ; *UNDOCUMENTED* RES 3,(IX+N) then LD D,(IX+N)
		RES3LDD (IY+DISPL)    // $FD $CB [1:S8] $9A ; *UNDOCUMENTED* RES 3,(IY+N) then LD D,(IY+N)
		RES3LDE (IX+DISPL)    // $DD $CB [1:S8] $9B ; *UNDOCUMENTED* RES 3,(IX+N) then LD E,(IX+N)
		RES3LDE (IY+DISPL)    // $FD $CB [1:S8] $9B ; *UNDOCUMENTED* RES 3,(IY+N) then LD E,(IY+N)
		RES3LDF (IX+DISPL)    // $DD $CB [1:S8] $9E ; *UNDOCUMENTED* RES 3,(IX+N) then LD F,(IX+N)
		RES3LDF (IY+DISPL)    // $FD $CB [1:S8] $9E ; *UNDOCUMENTED* RES 3,(IY+N) then LD F,(IY+N)
		RES3LDH (IX+DISPL)    // $DD $CB [1:S8] $9C ; *UNDOCUMENTED* RES 3,(IX+N) then LD H,(IX+N)
		RES3LDH (IY+DISPL)    // $FD $CB [1:S8] $9C ; *UNDOCUMENTED* RES 3,(IY+N) then LD H,(IY+N)
		RES3LDL (IX+DISPL)    // $DD $CB [1:S8] $9D ; *UNDOCUMENTED* RES 3,(IX+N) then LD L,(IX+N)
		RES3LDL (IY+DISPL)    // $FD $CB [1:S8] $9D ; *UNDOCUMENTED* RES 3,(IY+N) then LD L,(IY+N)
		RES4LDA (IX+DISPL)    // $DD $CB [1:S8] $A7 ; *UNDOCUMENTED* RES 4,(IX+N) then LD A,(IX+N)
		RES4LDA (IY+DISPL)    // $FD $CB [1:S8] $A7 ; *UNDOCUMENTED* RES 4,(IY+N) then LD A,(IY+N)
		RES4LDB (IX+DISPL)    // $DD $CB [1:S8] $A0 ; *UNDOCUMENTED* RES 4,(IX+N) then LD B,(IX+N)
		RES4LDB (IY+DISPL)    // $FD $CB [1:S8] $A0 ; *UNDOCUMENTED* RES 4,(IY+N) then LD B,(IY+N)
		RES4LDC (IX+DISPL)    // $DD $CB [1:S8] $A1 ; *UNDOCUMENTED* RES 4,(IX+N) then LD C,(IX+N)
		RES4LDC (IY+DISPL)    // $FD $CB [1:S8] $A1 ; *UNDOCUMENTED* RES 4,(IY+N) then LD C,(IY+N)
		RES4LDD (IX+DISPL)    // $DD $CB [1:S8] $A2 ; *UNDOCUMENTED* RES 4,(IX+N) then LD D,(IX+N)
		RES4LDD (IY+DISPL)    // $FD $CB [1:S8] $A2 ; *UNDOCUMENTED* RES 4,(IY+N) then LD D,(IY+N)
		RES4LDE (IX+DISPL)    // $DD $CB [1:S8] $A3 ; *UNDOCUMENTED* RES 4,(IX+N) then LD E,(IX+N)
		RES4LDE (IY+DISPL)    // $FD $CB [1:S8] $A3 ; *UNDOCUMENTED* RES 4,(IY+N) then LD E,(IY+N)
		RES4LDF (IX+DISPL)    // $DD $CB [1:S8] $A6 ; *UNDOCUMENTED* RES 4,(IX+N) then LD F,(IX+N)
		RES4LDF (IY+DISPL)    // $FD $CB [1:S8] $A6 ; *UNDOCUMENTED* RES 4,(IY+N) then LD F,(IY+N)
		RES4LDH (IX+DISPL)    // $DD $CB [1:S8] $A4 ; *UNDOCUMENTED* RES 4,(IX+N) then LD H,(IX+N)
		RES4LDH (IY+DISPL)    // $FD $CB [1:S8] $A4 ; *UNDOCUMENTED* RES 4,(IY+N) then LD H,(IY+N)
		RES4LDL (IX+DISPL)    // $DD $CB [1:S8] $A5 ; *UNDOCUMENTED* RES 4,(IX+N) then LD L,(IX+N)
		RES4LDL (IY+DISPL)    // $FD $CB [1:S8] $A5 ; *UNDOCUMENTED* RES 4,(IY+N) then LD L,(IY+N)
		RES5LDA (IX+DISPL)    // $DD $CB [1:S8] $AF ; *UNDOCUMENTED* RES 5,(IX+N) then LD A,(IX+N)
		RES5LDA (IY+DISPL)    // $FD $CB [1:S8] $AF ; *UNDOCUMENTED* RES 5,(IY+N) then LD A,(IY+N)
		RES5LDB (IX+DISPL)    // $DD $CB [1:S8] $A8 ; *UNDOCUMENTED* RES 5,(IX+N) then LD B,(IX+N)
		RES5LDB (IY+DISPL)    // $FD $CB [1:S8] $A8 ; *UNDOCUMENTED* RES 5,(IY+N) then LD B,(IY+N)
		RES5LDC (IX+DISPL)    // $DD $CB [1:S8] $A9 ; *UNDOCUMENTED* RES 5,(IX+N) then LD C,(IX+N)
		RES5LDC (IY+DISPL)    // $FD $CB [1:S8] $A9 ; *UNDOCUMENTED* RES 5,(IY+N) then LD C,(IY+N)
		RES5LDD (IX+DISPL)    // $DD $CB [1:S8] $AA ; *UNDOCUMENTED* RES 5,(IX+N) then LD D,(IX+N)
		RES5LDD (IY+DISPL)    // $FD $CB [1:S8] $AA ; *UNDOCUMENTED* RES 5,(IY+N) then LD D,(IY+N)
		RES5LDE (IX+DISPL)    // $DD $CB [1:S8] $AB ; *UNDOCUMENTED* RES 5,(IX+N) then LD E,(IX+N)
		RES5LDE (IY+DISPL)    // $FD $CB [1:S8] $AB ; *UNDOCUMENTED* RES 5,(IY+N) then LD E,(IY+N)
		RES5LDF (IX+DISPL)    // $DD $CB [1:S8] $AE ; *UNDOCUMENTED* RES 5,(IX+N) then LD F,(IX+N)
		RES5LDF (IY+DISPL)    // $FD $CB [1:S8] $AE ; *UNDOCUMENTED* RES 5,(IY+N) then LD F,(IY+N)
		RES5LDH (IX+DISPL)    // $DD $CB [1:S8] $AC ; *UNDOCUMENTED* RES 5,(IX+N) then LD H,(IX+N)
		RES5LDH (IY+DISPL)    // $FD $CB [1:S8] $AC ; *UNDOCUMENTED* RES 5,(IY+N) then LD H,(IY+N)
		RES5LDL (IX+DISPL)    // $DD $CB [1:S8] $AD ; *UNDOCUMENTED* RES 5,(IX+N) then LD L,(IX+N)
		RES5LDL (IY+DISPL)    // $FD $CB [1:S8] $AD ; *UNDOCUMENTED* RES 5,(IY+N) then LD L,(IY+N)
		RES6LDA (IX+DISPL)    // $DD $CB [1:S8] $B7 ; *UNDOCUMENTED* RES 6,(IX+N) then LD A,(IX+N)
		RES6LDA (IY+DISPL)    // $FD $CB [1:S8] $B7 ; *UNDOCUMENTED* RES 6,(IY+N) then LD A,(IY+N)
		RES6LDB (IX+DISPL)    // $DD $CB [1:S8] $B0 ; *UNDOCUMENTED* RES 6,(IX+N) then LD B,(IX+N)
		RES6LDB (IY+DISPL)    // $FD $CB [1:S8] $B0 ; *UNDOCUMENTED* RES 6,(IY+N) then LD B,(IY+N)
		RES6LDC (IX+DISPL)    // $DD $CB [1:S8] $B1 ; *UNDOCUMENTED* RES 6,(IX+N) then LD C,(IX+N)
		RES6LDC (IY+DISPL)    // $FD $CB [1:S8] $B1 ; *UNDOCUMENTED* RES 6,(IY+N) then LD C,(IY+N)
		RES6LDD (IX+DISPL)    // $DD $CB [1:S8] $B2 ; *UNDOCUMENTED* RES 6,(IX+N) then LD D,(IX+N)
		RES6LDD (IY+DISPL)    // $FD $CB [1:S8] $B2 ; *UNDOCUMENTED* RES 6,(IY+N) then LD D,(IY+N)
		RES6LDE (IX+DISPL)    // $DD $CB [1:S8] $B3 ; *UNDOCUMENTED* RES 6,(IX+N) then LD E,(IX+N)
		RES6LDE (IY+DISPL)    // $FD $CB [1:S8] $B3 ; *UNDOCUMENTED* RES 6,(IY+N) then LD E,(IY+N)
		RES6LDF (IX+DISPL)    // $DD $CB [1:S8] $B6 ; *UNDOCUMENTED* RES 6,(IX+N) then LD F,(IX+N)
		RES6LDF (IY+DISPL)    // $FD $CB [1:S8] $B6 ; *UNDOCUMENTED* RES 6,(IY+N) then LD F,(IY+N)
		RES6LDH (IX+DISPL)    // $DD $CB [1:S8] $B4 ; *UNDOCUMENTED* RES 6,(IX+N) then LD H,(IX+N)
		RES6LDH (IY+DISPL)    // $FD $CB [1:S8] $B4 ; *UNDOCUMENTED* RES 6,(IY+N) then LD H,(IY+N)
		RES6LDL (IX+DISPL)    // $DD $CB [1:S8] $B5 ; *UNDOCUMENTED* RES 6,(IX+N) then LD L,(IX+N)
		RES6LDL (IY+DISPL)    // $FD $CB [1:S8] $B5 ; *UNDOCUMENTED* RES 6,(IY+N) then LD L,(IY+N)
		RES7LDA (IX+DISPL)    // $DD $CB [1:S8] $BF ; *UNDOCUMENTED* RES 7,(IX+N) then LD A,(IX+N)
		RES7LDA (IY+DISPL)    // $FD $CB [1:S8] $BF ; *UNDOCUMENTED* RES 7,(IY+N) then LD A,(IY+N)
		RES7LDB (IX+DISPL)    // $DD $CB [1:S8] $B8 ; *UNDOCUMENTED* RES 7,(IX+N) then LD B,(IX+N)
		RES7LDB (IY+DISPL)    // $FD $CB [1:S8] $B8 ; *UNDOCUMENTED* RES 7,(IY+N) then LD B,(IY+N)
		RES7LDC (IX+DISPL)    // $DD $CB [1:S8] $B9 ; *UNDOCUMENTED* RES 7,(IX+N) then LD C,(IX+N)
		RES7LDC (IY+DISPL)    // $FD $CB [1:S8] $B9 ; *UNDOCUMENTED* RES 7,(IY+N) then LD C,(IY+N)
		RES7LDD (IX+DISPL)    // $DD $CB [1:S8] $BA ; *UNDOCUMENTED* RES 7,(IX+N) then LD D,(IX+N)
		RES7LDD (IY+DISPL)    // $FD $CB [1:S8] $BA ; *UNDOCUMENTED* RES 7,(IY+N) then LD D,(IY+N)
		RES7LDE (IX+DISPL)    // $DD $CB [1:S8] $BB ; *UNDOCUMENTED* RES 7,(IX+N) then LD E,(IX+N)
		RES7LDE (IY+DISPL)    // $FD $CB [1:S8] $BB ; *UNDOCUMENTED* RES 7,(IY+N) then LD E,(IY+N)
		RES7LDF (IX+DISPL)    // $DD $CB [1:S8] $BE ; *UNDOCUMENTED* RES 7,(IX+N) then LD F,(IX+N)
		RES7LDF (IY+DISPL)    // $FD $CB [1:S8] $BE ; *UNDOCUMENTED* RES 7,(IY+N) then LD F,(IY+N)
		RES7LDH (IX+DISPL)    // $DD $CB [1:S8] $BC ; *UNDOCUMENTED* RES 7,(IX+N) then LD H,(IX+N)
		RES7LDH (IY+DISPL)    // $FD $CB [1:S8] $BC ; *UNDOCUMENTED* RES 7,(IY+N) then LD H,(IY+N)
		RES7LDL (IX+DISPL)    // $DD $CB [1:S8] $BD ; *UNDOCUMENTED* RES 7,(IX+N) then LD L,(IX+N)
		RES7LDL (IY+DISPL)    // $FD $CB [1:S8] $BD ; *UNDOCUMENTED* RES 7,(IY+N) then LD L,(IY+N)
		SET0LDA (IX+DISPL)    // $DD $CB [1:S8] $C7 ; *UNDOCUMENTED* SET 0,(IX+N) then LD A,(IX+N)
		SET0LDA (IY+DISPL)    // $FD $CB [1:S8] $C7 ; *UNDOCUMENTED* SET 0,(IY+N) then LD A,(IY+N)
		SET0LDB (IX+DISPL)    // $DD $CB [1:S8] $C0 ; *UNDOCUMENTED* SET 0,(IX+N) then LD B,(IX+N)
		SET0LDB (IY+DISPL)    // $FD $CB [1:S8] $C0 ; *UNDOCUMENTED* SET 0,(IY+N) then LD B,(IY+N)
		SET0LDC (IX+DISPL)    // $DD $CB [1:S8] $C1 ; *UNDOCUMENTED* SET 0,(IX+N) then LD C,(IX+N)
		SET0LDC (IY+DISPL)    // $FD $CB [1:S8] $C1 ; *UNDOCUMENTED* SET 0,(IY+N) then LD C,(IY+N)
		SET0LDD (IX+DISPL)    // $DD $CB [1:S8] $C2 ; *UNDOCUMENTED* SET 0,(IX+N) then LD D,(IX+N)
		SET0LDD (IY+DISPL)    // $FD $CB [1:S8] $C2 ; *UNDOCUMENTED* SET 0,(IY+N) then LD D,(IY+N)
		SET0LDE (IX+DISPL)    // $DD $CB [1:S8] $C3 ; *UNDOCUMENTED* SET 0,(IX+N) then LD E,(IX+N)
		SET0LDE (IY+DISPL)    // $FD $CB [1:S8] $C3 ; *UNDOCUMENTED* SET 0,(IY+N) then LD E,(IY+N)
		SET0LDF (IX+DISPL)    // $DD $CB [1:S8] $C6 ; *UNDOCUMENTED* SET 0,(IX+N) then LD F,(IX+N)
		SET0LDF (IY+DISPL)    // $FD $CB [1:S8] $C6 ; *UNDOCUMENTED* SET 0,(IY+N) then LD F,(IY+N)
		SET0LDH (IX+DISPL)    // $DD $CB [1:S8] $C4 ; *UNDOCUMENTED* SET 0,(IX+N) then LD H,(IX+N)
		SET0LDH (IY+DISPL)    // $FD $CB [1:S8] $C4 ; *UNDOCUMENTED* SET 0,(IY+N) then LD H,(IY+N)
		SET0LDL (IX+DISPL)    // $DD $CB [1:S8] $C5 ; *UNDOCUMENTED* SET 0,(IX+N) then LD L,(IX+N)
		SET0LDL (IY+DISPL)    // $FD $CB [1:S8] $C5 ; *UNDOCUMENTED* SET 0,(IY+N) then LD L,(IY+N)
		SET1LDA (IX+DISPL)    // $DD $CB [1:S8] $CF ; *UNDOCUMENTED* SET 1,(IX+N) then LD A,(IX+N)
		SET1LDA (IY+DISPL)    // $FD $CB [1:S8] $CF ; *UNDOCUMENTED* SET 1,(IY+N) then LD A,(IY+N)
		SET1LDB (IX+DISPL)    // $DD $CB [1:S8] $C8 ; *UNDOCUMENTED* SET 1,(IX+N) then LD B,(IX+N)
		SET1LDB (IY+DISPL)    // $FD $CB [1:S8] $C8 ; *UNDOCUMENTED* SET 1,(IY+N) then LD B,(IY+N)
		SET1LDC (IX+DISPL)    // $DD $CB [1:S8] $C9 ; *UNDOCUMENTED* SET 1,(IX+N) then LD C,(IX+N)
		SET1LDC (IY+DISPL)    // $FD $CB [1:S8] $C9 ; *UNDOCUMENTED* SET 1,(IY+N) then LD C,(IY+N)
		SET1LDD (IX+DISPL)    // $DD $CB [1:S8] $CA ; *UNDOCUMENTED* SET 1,(IX+N) then LD D,(IX+N)
		SET1LDD (IY+DISPL)    // $FD $CB [1:S8] $CA ; *UNDOCUMENTED* SET 1,(IY+N) then LD D,(IY+N)
		SET1LDE (IX+DISPL)    // $DD $CB [1:S8] $CB ; *UNDOCUMENTED* SET 1,(IX+N) then LD E,(IX+N)
		SET1LDE (IY+DISPL)    // $FD $CB [1:S8] $CB ; *UNDOCUMENTED* SET 1,(IY+N) then LD E,(IY+N)
		SET1LDF (IX+DISPL)    // $DD $CB [1:S8] $CE ; *UNDOCUMENTED* SET 1,(IX+N) then LD F,(IX+N)
		SET1LDF (IY+DISPL)    // $FD $CB [1:S8] $CE ; *UNDOCUMENTED* SET 1,(IY+N) then LD F,(IY+N)
		SET1LDH (IX+DISPL)    // $DD $CB [1:S8] $CC ; *UNDOCUMENTED* SET 1,(IX+N) then LD H,(IX+N)
		SET1LDH (IY+DISPL)    // $FD $CB [1:S8] $CC ; *UNDOCUMENTED* SET 1,(IY+N) then LD H,(IY+N)
		SET1LDL (IX+DISPL)    // $DD $CB [1:S8] $CD ; *UNDOCUMENTED* SET 1,(IX+N) then LD L,(IX+N)
		SET1LDL (IY+DISPL)    // $FD $CB [1:S8] $CD ; *UNDOCUMENTED* SET 1,(IY+N) then LD L,(IY+N)
		SET2LDA (IX+DISPL)    // $DD $CB [1:S8] $D7 ; *UNDOCUMENTED* SET 2,(IX+N) then LD A,(IX+N)
		SET2LDA (IY+DISPL)    // $FD $CB [1:S8] $D7 ; *UNDOCUMENTED* SET 2,(IY+N) then LD A,(IY+N)
		SET2LDB (IX+DISPL)    // $DD $CB [1:S8] $D0 ; *UNDOCUMENTED* SET 2,(IX+N) then LD B,(IX+N)
		SET2LDB (IY+DISPL)    // $FD $CB [1:S8] $D0 ; *UNDOCUMENTED* SET 2,(IY+N) then LD B,(IY+N)
		SET2LDC (IX+DISPL)    // $DD $CB [1:S8] $D1 ; *UNDOCUMENTED* SET 2,(IX+N) then LD C,(IX+N)
		SET2LDC (IY+DISPL)    // $FD $CB [1:S8] $D1 ; *UNDOCUMENTED* SET 2,(IY+N) then LD C,(IY+N)
		SET2LDD (IX+DISPL)    // $DD $CB [1:S8] $D2 ; *UNDOCUMENTED* SET 2,(IX+N) then LD D,(IX+N)
		SET2LDD (IY+DISPL)    // $FD $CB [1:S8] $D2 ; *UNDOCUMENTED* SET 2,(IY+N) then LD D,(IY+N)
		SET2LDE (IX+DISPL)    // $DD $CB [1:S8] $D3 ; *UNDOCUMENTED* SET 2,(IX+N) then LD E,(IX+N)
		SET2LDE (IY+DISPL)    // $FD $CB [1:S8] $D3 ; *UNDOCUMENTED* SET 2,(IY+N) then LD E,(IY+N)
		SET2LDF (IX+DISPL)    // $DD $CB [1:S8] $D6 ; *UNDOCUMENTED* SET 2,(IX+N) then LD F,(IX+N)
		SET2LDF (IY+DISPL)    // $FD $CB [1:S8] $D6 ; *UNDOCUMENTED* SET 2,(IY+N) then LD F,(IY+N)
		SET2LDH (IX+DISPL)    // $DD $CB [1:S8] $D4 ; *UNDOCUMENTED* SET 2,(IX+N) then LD H,(IX+N)
		SET2LDH (IY+DISPL)    // $FD $CB [1:S8] $D4 ; *UNDOCUMENTED* SET 2,(IY+N) then LD H,(IY+N)
		SET2LDL (IX+DISPL)    // $DD $CB [1:S8] $D5 ; *UNDOCUMENTED* SET 2,(IX+N) then LD L,(IX+N)
		SET2LDL (IY+DISPL)    // $FD $CB [1:S8] $D5 ; *UNDOCUMENTED* SET 2,(IY+N) then LD L,(IY+N)
		SET3LDA (IX+DISPL)    // $DD $CB [1:S8] $DF ; *UNDOCUMENTED* SET 3,(IX+N) then LD A,(IX+N)
		SET3LDA (IY+DISPL)    // $FD $CB [1:S8] $DF ; *UNDOCUMENTED* SET 3,(IY+N) then LD A,(IY+N)
		SET3LDB (IX+DISPL)    // $DD $CB [1:S8] $D8 ; *UNDOCUMENTED* SET 3,(IX+N) then LD B,(IX+N)
		SET3LDB (IY+DISPL)    // $FD $CB [1:S8] $D8 ; *UNDOCUMENTED* SET 3,(IY+N) then LD B,(IY+N)
		SET3LDC (IX+DISPL)    // $DD $CB [1:S8] $D9 ; *UNDOCUMENTED* SET 3,(IX+N) then LD C,(IX+N)
		SET3LDC (IY+DISPL)    // $FD $CB [1:S8] $D9 ; *UNDOCUMENTED* SET 3,(IY+N) then LD C,(IY+N)
		SET3LDD (IX+DISPL)    // $DD $CB [1:S8] $DA ; *UNDOCUMENTED* SET 3,(IX+N) then LD D,(IX+N)
		SET3LDD (IY+DISPL)    // $FD $CB [1:S8] $DA ; *UNDOCUMENTED* SET 3,(IY+N) then LD D,(IY+N)
		SET3LDE (IX+DISPL)    // $DD $CB [1:S8] $DB ; *UNDOCUMENTED* SET 3,(IX+N) then LD E,(IX+N)
		SET3LDE (IY+DISPL)    // $FD $CB [1:S8] $DB ; *UNDOCUMENTED* SET 3,(IY+N) then LD E,(IY+N)
		SET3LDF (IX+DISPL)    // $DD $CB [1:S8] $DE ; *UNDOCUMENTED* SET 3,(IX+N) then LD F,(IX+N)
		SET3LDF (IY+DISPL)    // $FD $CB [1:S8] $DE ; *UNDOCUMENTED* SET 3,(IY+N) then LD F,(IY+N)
		SET3LDH (IX+DISPL)    // $DD $CB [1:S8] $DC ; *UNDOCUMENTED* SET 3,(IX+N) then LD H,(IX+N)
		SET3LDH (IY+DISPL)    // $FD $CB [1:S8] $DC ; *UNDOCUMENTED* SET 3,(IY+N) then LD H,(IY+N)
		SET3LDL (IX+DISPL)    // $DD $CB [1:S8] $DD ; *UNDOCUMENTED* SET 3,(IX+N) then LD L,(IX+N)
		SET3LDL (IY+DISPL)    // $FD $CB [1:S8] $DD ; *UNDOCUMENTED* SET 3,(IY+N) then LD L,(IY+N)
		SET4LDA (IX+DISPL)    // $DD $CB [1:S8] $E7 ; *UNDOCUMENTED* SET 4,(IX+N) then LD A,(IX+N)
		SET4LDA (IY+DISPL)    // $FD $CB [1:S8] $E7 ; *UNDOCUMENTED* SET 4,(IY+N) then LD A,(IY+N)
		SET4LDB (IX+DISPL)    // $DD $CB [1:S8] $E0 ; *UNDOCUMENTED* SET 4,(IX+N) then LD B,(IX+N)
		SET4LDB (IY+DISPL)    // $FD $CB [1:S8] $E0 ; *UNDOCUMENTED* SET 4,(IY+N) then LD B,(IY+N)
		SET4LDC (IX+DISPL)    // $DD $CB [1:S8] $E1 ; *UNDOCUMENTED* SET 4,(IX+N) then LD C,(IX+N)
		SET4LDC (IY+DISPL)    // $FD $CB [1:S8] $E1 ; *UNDOCUMENTED* SET 4,(IY+N) then LD C,(IY+N)
		SET4LDD (IX+DISPL)    // $DD $CB [1:S8] $E2 ; *UNDOCUMENTED* SET 4,(IX+N) then LD D,(IX+N)
		SET4LDD (IY+DISPL)    // $FD $CB [1:S8] $E2 ; *UNDOCUMENTED* SET 4,(IY+N) then LD D,(IY+N)
		SET4LDE (IX+DISPL)    // $DD $CB [1:S8] $E3 ; *UNDOCUMENTED* SET 4,(IX+N) then LD E,(IX+N)
		SET4LDE (IY+DISPL)    // $FD $CB [1:S8] $E3 ; *UNDOCUMENTED* SET 4,(IY+N) then LD E,(IY+N)
		SET4LDF (IX+DISPL)    // $DD $CB [1:S8] $E6 ; *UNDOCUMENTED* SET 4,(IX+N) then LD F,(IX+N)
		SET4LDF (IY+DISPL)    // $FD $CB [1:S8] $E6 ; *UNDOCUMENTED* SET 4,(IY+N) then LD F,(IY+N)
		SET4LDH (IX+DISPL)    // $DD $CB [1:S8] $E4 ; *UNDOCUMENTED* SET 4,(IX+N) then LD H,(IX+N)
		SET4LDH (IY+DISPL)    // $FD $CB [1:S8] $E4 ; *UNDOCUMENTED* SET 4,(IY+N) then LD H,(IY+N)
		SET4LDL (IX+DISPL)    // $DD $CB [1:S8] $E5 ; *UNDOCUMENTED* SET 4,(IX+N) then LD L,(IX+N)
		SET4LDL (IY+DISPL)    // $FD $CB [1:S8] $E5 ; *UNDOCUMENTED* SET 4,(IY+N) then LD L,(IY+N)
		SET5LDA (IX+DISPL)    // $DD $CB [1:S8] $EF ; *UNDOCUMENTED* SET 5,(IX+N) then LD A,(IX+N)
		SET5LDA (IY+DISPL)    // $FD $CB [1:S8] $EF ; *UNDOCUMENTED* SET 5,(IY+N) then LD A,(IY+N)
		SET5LDB (IX+DISPL)    // $DD $CB [1:S8] $E8 ; *UNDOCUMENTED* SET 5,(IX+N) then LD B,(IX+N)
		SET5LDB (IY+DISPL)    // $FD $CB [1:S8] $E8 ; *UNDOCUMENTED* SET 5,(IY+N) then LD B,(IY+N)
		SET5LDC (IX+DISPL)    // $DD $CB [1:S8] $E9 ; *UNDOCUMENTED* SET 5,(IX+N) then LD C,(IX+N)
		SET5LDC (IY+DISPL)    // $FD $CB [1:S8] $E9 ; *UNDOCUMENTED* SET 5,(IY+N) then LD C,(IY+N)
		SET5LDD (IX+DISPL)    // $DD $CB [1:S8] $EA ; *UNDOCUMENTED* SET 5,(IX+N) then LD D,(IX+N)
		SET5LDD (IY+DISPL)    // $FD $CB [1:S8] $EA ; *UNDOCUMENTED* SET 5,(IY+N) then LD D,(IY+N)
		SET5LDE (IX+DISPL)    // $DD $CB [1:S8] $EB ; *UNDOCUMENTED* SET 5,(IX+N) then LD E,(IX+N)
		SET5LDE (IY+DISPL)    // $FD $CB [1:S8] $EB ; *UNDOCUMENTED* SET 5,(IY+N) then LD E,(IY+N)
		SET5LDF (IX+DISPL)    // $DD $CB [1:S8] $EE ; *UNDOCUMENTED* SET 5,(IX+N) then LD F,(IX+N)
		SET5LDF (IY+DISPL)    // $FD $CB [1:S8] $EE ; *UNDOCUMENTED* SET 5,(IY+N) then LD F,(IY+N)
		SET5LDH (IX+DISPL)    // $DD $CB [1:S8] $EC ; *UNDOCUMENTED* SET 5,(IX+N) then LD H,(IX+N)
		SET5LDH (IY+DISPL)    // $FD $CB [1:S8] $EC ; *UNDOCUMENTED* SET 5,(IY+N) then LD H,(IY+N)
		SET5LDL (IX+DISPL)    // $DD $CB [1:S8] $ED ; *UNDOCUMENTED* SET 5,(IX+N) then LD L,(IX+N)
		SET5LDL (IY+DISPL)    // $FD $CB [1:S8] $ED ; *UNDOCUMENTED* SET 5,(IY+N) then LD L,(IY+N)
		SET6LDA (IX+DISPL)    // $DD $CB [1:S8] $F7 ; *UNDOCUMENTED* SET 6,(IX+N) then LD A,(IX+N)
		SET6LDA (IY+DISPL)    // $FD $CB [1:S8] $F7 ; *UNDOCUMENTED* SET 6,(IY+N) then LD A,(IY+N)
		SET6LDB (IX+DISPL)    // $DD $CB [1:S8] $F0 ; *UNDOCUMENTED* SET 6,(IX+N) then LD B,(IX+N)
		SET6LDB (IY+DISPL)    // $FD $CB [1:S8] $F0 ; *UNDOCUMENTED* SET 6,(IY+N) then LD B,(IY+N)
		SET6LDC (IX+DISPL)    // $DD $CB [1:S8] $F1 ; *UNDOCUMENTED* SET 6,(IX+N) then LD C,(IX+N)
		SET6LDC (IY+DISPL)    // $FD $CB [1:S8] $F1 ; *UNDOCUMENTED* SET 6,(IY+N) then LD C,(IY+N)
		SET6LDD (IX+DISPL)    // $DD $CB [1:S8] $F2 ; *UNDOCUMENTED* SET 6,(IX+N) then LD D,(IX+N)
		SET6LDD (IY+DISPL)    // $FD $CB [1:S8] $F2 ; *UNDOCUMENTED* SET 6,(IY+N) then LD D,(IY+N)
		SET6LDE (IX+DISPL)    // $DD $CB [1:S8] $F3 ; *UNDOCUMENTED* SET 6,(IX+N) then LD E,(IX+N)
		SET6LDE (IY+DISPL)    // $FD $CB [1:S8] $F3 ; *UNDOCUMENTED* SET 6,(IY+N) then LD E,(IY+N)
		SET6LDF (IX+DISPL)    // $DD $CB [1:S8] $F6 ; *UNDOCUMENTED* SET 6,(IX+N) then LD F,(IX+N)
		SET6LDF (IY+DISPL)    // $FD $CB [1:S8] $F6 ; *UNDOCUMENTED* SET 6,(IY+N) then LD F,(IY+N)
		SET6LDH (IX+DISPL)    // $DD $CB [1:S8] $F4 ; *UNDOCUMENTED* SET 6,(IX+N) then LD H,(IX+N)
		SET6LDH (IY+DISPL)    // $FD $CB [1:S8] $F4 ; *UNDOCUMENTED* SET 6,(IY+N) then LD H,(IY+N)
		SET6LDL (IX+DISPL)    // $DD $CB [1:S8] $F5 ; *UNDOCUMENTED* SET 6,(IX+N) then LD L,(IX+N)
		SET6LDL (IY+DISPL)    // $FD $CB [1:S8] $F5 ; *UNDOCUMENTED* SET 6,(IY+N) then LD L,(IY+N)
		SET7LDA (IX+DISPL)    // $DD $CB [1:S8] $FF ; *UNDOCUMENTED* SET 7,(IX+N) then LD A,(IX+N)
		SET7LDA (IY+DISPL)    // $FD $CB [1:S8] $FF ; *UNDOCUMENTED* SET 7,(IY+N) then LD A,(IY+N)
		SET7LDB (IX+DISPL)    // $DD $CB [1:S8] $F8 ; *UNDOCUMENTED* SET 7,(IX+N) then LD B,(IX+N)
		SET7LDB (IY+DISPL)    // $FD $CB [1:S8] $F8 ; *UNDOCUMENTED* SET 7,(IY+N) then LD B,(IY+N)
		SET7LDC (IX+DISPL)    // $DD $CB [1:S8] $F9 ; *UNDOCUMENTED* SET 7,(IX+N) then LD C,(IX+N)
		SET7LDC (IY+DISPL)    // $FD $CB [1:S8] $F9 ; *UNDOCUMENTED* SET 7,(IY+N) then LD C,(IY+N)
		SET7LDD (IX+DISPL)    // $DD $CB [1:S8] $FA ; *UNDOCUMENTED* SET 7,(IX+N) then LD D,(IX+N)
		SET7LDD (IY+DISPL)    // $FD $CB [1:S8] $FA ; *UNDOCUMENTED* SET 7,(IY+N) then LD D,(IY+N)
		SET7LDE (IX+DISPL)    // $DD $CB [1:S8] $FB ; *UNDOCUMENTED* SET 7,(IX+N) then LD E,(IX+N)
		SET7LDE (IY+DISPL)    // $FD $CB [1:S8] $FB ; *UNDOCUMENTED* SET 7,(IY+N) then LD E,(IY+N)
		SET7LDF (IX+DISPL)    // $DD $CB [1:S8] $FE ; *UNDOCUMENTED* SET 7,(IX+N) then LD F,(IX+N)
		SET7LDF (IY+DISPL)    // $FD $CB [1:S8] $FE ; *UNDOCUMENTED* SET 7,(IY+N) then LD F,(IY+N)
		SET7LDH (IX+DISPL)    // $DD $CB [1:S8] $FC ; *UNDOCUMENTED* SET 7,(IX+N) then LD H,(IX+N)
		SET7LDH (IY+DISPL)    // $FD $CB [1:S8] $FC ; *UNDOCUMENTED* SET 7,(IY+N) then LD H,(IY+N)
		SET7LDL (IX+DISPL)    // $DD $CB [1:S8] $FD ; *UNDOCUMENTED* SET 7,(IX+N) then LD L,(IX+N)
		SET7LDL (IY+DISPL)    // $FD $CB [1:S8] $FD ; *UNDOCUMENTED* SET 7,(IY+N) then LD L,(IY+N)


;
; Z80X - UNDOCUMENTED Shift / rotate instructions
;

		RLC     (IX+DISPL),A  // $DD $CB [1:S8] $07 ; *UNDOCUMENTED* RLC (IX+N) then LD A,(IX+N)
		RLC     (IY+DISPL),A  // $FD $CB [1:S8] $07 ; *UNDOCUMENTED* RLC (IY+N) then LD A,(IY+N)
		RLC     (IX+DISPL),B  // $DD $CB [1:S8] $00 ; *UNDOCUMENTED* RLC (IX+N) then LD B,(IX+N)
		RLC     (IY+DISPL),B  // $FD $CB [1:S8] $00 ; *UNDOCUMENTED* RLC (IY+N) then LD B,(IY+N)
		RLC     (IX+DISPL),C  // $DD $CB [1:S8] $01 ; *UNDOCUMENTED* RLC (IX+N) then LD C,(IX+N)
		RLC     (IY+DISPL),C  // $FD $CB [1:S8] $01 ; *UNDOCUMENTED* RLC (IY+N) then LD C,(IY+N)
		RLC     (IX+DISPL),D  // $DD $CB [1:S8] $02 ; *UNDOCUMENTED* RLC (IX+N) then LD D,(IX+N)
		RLC     (IY+DISPL),D  // $FD $CB [1:S8] $02 ; *UNDOCUMENTED* RLC (IY+N) then LD D,(IY+N)
		RLC     (IX+DISPL),E  // $DD $CB [1:S8] $03 ; *UNDOCUMENTED* RLC (IX+N) then LD E,(IX+N)
		RLC     (IY+DISPL),E  // $FD $CB [1:S8] $03 ; *UNDOCUMENTED* RLC (IY+N) then LD E,(IY+N)
		RLC     (IX+DISPL),H  // $DD $CB [1:S8] $04 ; *UNDOCUMENTED* RLC (IX+N) then LD H,(IX+N)
		RLC     (IY+DISPL),H  // $FD $CB [1:S8] $04 ; *UNDOCUMENTED* RLC (IY+N) then LD H,(IY+N)
		RLC     (IX+DISPL),L  // $DD $CB [1:S8] $05 ; *UNDOCUMENTED* RLC (IX+N) then LD L,(IX+N)
		RLC     (IY+DISPL),L  // $FD $CB [1:S8] $05 ; *UNDOCUMENTED* RLC (IY+N) then LD L,(IY+N)
		RL      (IY+DISPL),A  // $FD $CB [1:S8] $17 ; *UNDOCUMENTED* RL (IY+N) then LD A,(IY+N)
		RL      (IX+DISPL),B  // $DD $CB [1:S8] $10 ; *UNDOCUMENTED* RL (IX+N) then LD B,(IX+N)
		RL      (IY+DISPL),B  // $FD $CB [1:S8] $10 ; *UNDOCUMENTED* RL (IY+N) then LD B,(IY+N)
		RL      (IX+DISPL),C  // $DD $CB [1:S8] $11 ; *UNDOCUMENTED* RL (IX+N) then LD C,(IX+N)
		RL      (IY+DISPL),C  // $FD $CB [1:S8] $11 ; *UNDOCUMENTED* RL (IY+N) then LD C,(IY+N)
		RL      (IX+DISPL),D  // $DD $CB [1:S8] $12 ; *UNDOCUMENTED* RL (IX+N) then LD D,(IX+N)
		RL      (IY+DISPL),D  // $FD $CB [1:S8] $12 ; *UNDOCUMENTED* RL (IY+N) then LD D,(IY+N)
		RL      (IX+DISPL),E  // $DD $CB [1:S8] $13 ; *UNDOCUMENTED* RL (IX+N) then LD E,(IX+N)
		RL      (IY+DISPL),E  // $FD $CB [1:S8] $13 ; *UNDOCUMENTED* RL (IY+N) then LD E,(IY+N)
		RL      (IX+DISPL),H  // $DD $CB [1:S8] $14 ; *UNDOCUMENTED* RL (IX+N) then LD H,(IX+N)
		RL      (IY+DISPL),H  // $FD $CB [1:S8] $14 ; *UNDOCUMENTED* RL (IY+N) then LD H,(IY+N)
		RL      (IX+DISPL),L  // $DD $CB [1:S8] $15 ; *UNDOCUMENTED* RL (IX+N) then LD L,(IX+N)
		RL      (IY+DISPL),L  // $FD $CB [1:S8] $15 ; *UNDOCUMENTED* RL (IY+N) then LD L,(IY+N)
		RRC     (IX+DISPL),A  // $DD $CB [1:S8] $0F ; *UNDOCUMENTED* RRC (IX+N) then LD A,(IX+N)
		RRC     (IY+DISPL),A  // $FD $CB [1:S8] $0F ; *UNDOCUMENTED* RRC (IY+N) then LD A,(IY+N)
		RRC     (IX+DISPL),B  // $DD $CB [1:S8] $08 ; *UNDOCUMENTED* RRC (IX+N) then LD B,(IX+N)
		RRC     (IY+DISPL),B  // $FD $CB [1:S8] $08 ; *UNDOCUMENTED* RRC (IY+N) then LD B,(IY+N)
		RRC     (IX+DISPL),C  // $DD $CB [1:S8] $09 ; *UNDOCUMENTED* RRC (IX+N) then LD C,(IX+N)
		RRC     (IY+DISPL),C  // $FD $CB [1:S8] $09 ; *UNDOCUMENTED* RRC (IY+N) then LD C,(IY+N)
		RRC     (IX+DISPL),D  // $DD $CB [1:S8] $0A ; *UNDOCUMENTED* RRC (IX+N) then LD D,(IX+N)
		RRC     (IY+DISPL),D  // $FD $CB [1:S8] $0A ; *UNDOCUMENTED* RRC (IY+N) then LD D,(IY+N)
		RRC     (IX+DISPL),E  // $DD $CB [1:S8] $0B ; *UNDOCUMENTED* RRC (IX+N) then LD E,(IX+N)
		RRC     (IY+DISPL),E  // $FD $CB [1:S8] $0B ; *UNDOCUMENTED* RRC (IY+N) then LD E,(IY+N)
		RRC     (IX+DISPL),H  // $DD $CB [1:S8] $0C ; *UNDOCUMENTED* RRC (IX+N) then LD H,(IX+N)
		RRC     (IY+DISPL),H  // $FD $CB [1:S8] $0C ; *UNDOCUMENTED* RRC (IY+N) then LD H,(IY+N)
		RRC     (IX+DISPL),L  // $DD $CB [1:S8] $0D ; *UNDOCUMENTED* RRC (IX+N) then LD L,(IX+N)
		RRC     (IY+DISPL),L  // $FD $CB [1:S8] $0D ; *UNDOCUMENTED* RRC (IY+N) then LD L,(IY+N)
		RR      (IX+DISPL),A  // $DD $CB [1:S8] $1F ; *UNDOCUMENTED* RR (IX+N) then LD A,(IX+N)
		RR      (IY+DISPL),A  // $FD $CB [1:S8] $1F ; *UNDOCUMENTED* RR (IY+N) then LD A,(IY+N)
		RR      (IX+DISPL),B  // $DD $CB [1:S8] $18 ; *UNDOCUMENTED* RR (IX+N) then LD B,(IX+N)
		RR      (IY+DISPL),B  // $FD $CB [1:S8] $18 ; *UNDOCUMENTED* RR (IY+N) then LD B,(IY+N)
		RR      (IX+DISPL),C  // $DD $CB [1:S8] $19 ; *UNDOCUMENTED* RR (IX+N) then LD C,(IX+N)
		RR      (IY+DISPL),C  // $FD $CB [1:S8] $19 ; *UNDOCUMENTED* RR (IY+N) then LD C,(IY+N)
		RR      (IX+DISPL),D  // $DD $CB [1:S8] $1A ; *UNDOCUMENTED* RR (IX+N) then LD D,(IX+N)
		RR      (IY+DISPL),D  // $FD $CB [1:S8] $1A ; *UNDOCUMENTED* RR (IY+N) then LD D,(IY+N)
		RR      (IX+DISPL),E  // $DD $CB [1:S8] $1B ; *UNDOCUMENTED* RR (IX+N) then LD E,(IX+N)
		RR      (IY+DISPL),E  // $FD $CB [1:S8] $1B ; *UNDOCUMENTED* RR (IY+N) then LD E,(IY+N)
		RR      (IX+DISPL),H  // $DD $CB [1:S8] $1C ; *UNDOCUMENTED* RR (IX+N) then LD H,(IX+N)
		RR      (IY+DISPL),H  // $FD $CB [1:S8] $1C ; *UNDOCUMENTED* RR (IY+N) then LD H,(IY+N)
		RR      (IX+DISPL),L  // $DD $CB [1:S8] $1D ; *UNDOCUMENTED* RR (IX+N) then LD L,(IX+N)
		RR      (IY+DISPL),L  // $FD $CB [1:S8] $1D ; *UNDOCUMENTED* RR (IY+N) then LD L,(IY+N)
		SLA     (IX+DISPL),A  // $DD $CB [1:S8] $27 ; *UNDOCUMENTED* SLA (IX+N) then LD A,(IX+N)
		SLA     (IY+DISPL),A  // $FD $CB [1:S8] $27 ; *UNDOCUMENTED* SLA (IY+N) then LD A,(IY+N)
		SLA     (IX+DISPL),B  // $DD $CB [1:S8] $20 ; *UNDOCUMENTED* SLA (IX+N) then LD B,(IX+N)
		SLA     (IY+DISPL),B  // $FD $CB [1:S8] $20 ; *UNDOCUMENTED* SLA (IY+N) then LD B,(IY+N)
		SLA     (IX+DISPL),C  // $DD $CB [1:S8] $21 ; *UNDOCUMENTED* SLA (IX+N) then LD C,(IX+N)
		SLA     (IY+DISPL),C  // $FD $CB [1:S8] $21 ; *UNDOCUMENTED* SLA (IY+N) then LD C,(IY+N)
		SLA     (IX+DISPL),D  // $DD $CB [1:S8] $22 ; *UNDOCUMENTED* SLA (IX+N) then LD D,(IX+N)
		SLA     (IY+DISPL),D  // $FD $CB [1:S8] $22 ; *UNDOCUMENTED* SLA (IY+N) then LD D,(IY+N)
		SLA     (IX+DISPL),E  // $DD $CB [1:S8] $23 ; *UNDOCUMENTED* SLA (IX+N) then LD E,(IX+N)
		SLA     (IY+DISPL),E  // $FD $CB [1:S8] $23 ; *UNDOCUMENTED* SLA (IY+N) then LD E,(IY+N)
		SLA     (IX+DISPL),H  // $DD $CB [1:S8] $24 ; *UNDOCUMENTED* SLA (IX+N) then LD H,(IX+N)
		SLA     (IY+DISPL),H  // $FD $CB [1:S8] $24 ; *UNDOCUMENTED* SLA (IY+N) then LD H,(IY+N)
		SLA     (IX+DISPL),L  // $DD $CB [1:S8] $25 ; *UNDOCUMENTED* SLA (IX+N) then LD L,(IX+N)
		SLA     (IY+DISPL),L  // $FD $CB [1:S8] $25 ; *UNDOCUMENTED* SLA (IY+N) then LD L,(IY+N)
		SLL     A             // $CB $37 ; *UNDOCUMENTED*
		SLL     B             // $CB $30 ; *UNDOCUMENTED*
		SLL     C             // $CB $31 ; *UNDOCUMENTED*
		SLL     D             // $CB $32 ; *UNDOCUMENTED*
		SLL     E             // $CB $33 ; *UNDOCUMENTED*
		SLL     H             // $CB $34 ; *UNDOCUMENTED*
		SLL     (HL)          // $CB $36 ; *UNDOCUMENTED*
		SLL     L             // $CB $35 ; *UNDOCUMENTED*
		SLL     (IX+DISPL),A  // $DD $CB [1:S8] $37 ; *UNDOCUMENTED* SLL (IX+N) then LD A,(IX+N)
		SLL     (IY+DISPL),A  // $FD $CB [1:S8] $37 ; *UNDOCUMENTED* SLL (IY+N) then LD A,(IY+N)
		SLL     (IX+DISPL),B  // $DD $CB [1:S8] $30 ; *UNDOCUMENTED* SLL (IX+N) then LD B,(IX+N)
		SLL     (IY+DISPL),B  // $FD $CB [1:S8] $30 ; *UNDOCUMENTED* SLL (IY+N) then LD B,(IY+N)
		SLL     (IX+DISPL),C  // $DD $CB [1:S8] $31 ; *UNDOCUMENTED* SLL (IX+N) then LD C,(IX+N)
		SLL     (IY+DISPL),C  // $FD $CB [1:S8] $31 ; *UNDOCUMENTED* SLL (IY+N) then LD C,(IY+N)
		SLL     (IX+DISPL),D  // $DD $CB [1:S8] $32 ; *UNDOCUMENTED* SLL (IX+N) then LD D,(IX+N)
		SLL     (IY+DISPL),D  // $FD $CB [1:S8] $32 ; *UNDOCUMENTED* SLL (IY+N) then LD D,(IY+N)
		SLL     (IX+DISPL),E  // $DD $CB [1:S8] $33 ; *UNDOCUMENTED* SLL (IX+N) then LD E,(IX+N)
		SLL     (IY+DISPL),E  // $FD $CB [1:S8] $33 ; *UNDOCUMENTED* SLL (IY+N) then LD E,(IY+N)
		SLL     (IX+DISPL)    // $DD $CB [1:S8] $36 ; *UNDOCUMENTED* SLL (IX+N)
		SLL     (IY+DISPL)    // $FD $CB [1:S8] $36 ; *UNDOCUMENTED* SLL (IY+N)
		SLL     (IX+DISPL),H  // $DD $CB [1:S8] $34 ; *UNDOCUMENTED* SLL (IX+N) then LD H,(IX+N)
		SLL     (IY+DISPL),H  // $FD $CB [1:S8] $34 ; *UNDOCUMENTED* SLL (IY+N) then LD H,(IY+N)
		SLL     (IX+DISPL),L  // $DD $CB [1:S8] $35 ; *UNDOCUMENTED* SLL (IX+N) then LD L,(IX+N)
		SLL     (IY+DISPL),L  // $FD $CB [1:S8] $35 ; *UNDOCUMENTED* SLL (IY+N) then LD L,(IY+N)
		SRA     (IX+DISPL),A  // $DD $CB [1:S8] $2F ; *UNDOCUMENTED* SRA (IX+N) then LD A,(IX+N)
		SRA     (IY+DISPL),A  // $FD $CB [1:S8] $2F ; *UNDOCUMENTED* SRA (IY+N) then LD A,(IY+N)
		SRA     (IX+DISPL),B  // $DD $CB [1:S8] $28 ; *UNDOCUMENTED* SRA (IX+N) then LD B,(IX+N)
		SRA     (IY+DISPL),B  // $FD $CB [1:S8] $28 ; *UNDOCUMENTED* SRA (IY+N) then LD B,(IY+N)
		SRA     (IX+DISPL),C  // $DD $CB [1:S8] $29 ; *UNDOCUMENTED* SRA (IX+N) then LD C,(IX+N)
		SRA     (IY+DISPL),C  // $FD $CB [1:S8] $29 ; *UNDOCUMENTED* SRA (IY+N) then LD C,(IY+N)
		SRA     (IX+DISPL),D  // $DD $CB [1:S8] $2A ; *UNDOCUMENTED* SRA (IX+N) then LD D,(IX+N)
		SRA     (IY+DISPL),D  // $FD $CB [1:S8] $2A ; *UNDOCUMENTED* SRA (IY+N) then LD D,(IY+N)
		SRA     (IX+DISPL),E  // $DD $CB [1:S8] $2B ; *UNDOCUMENTED* SRA (IX+N) then LD E,(IX+N)
		SRA     (IY+DISPL),E  // $FD $CB [1:S8] $2B ; *UNDOCUMENTED* SRA (IY+N) then LD E,(IY+N)
		SRA     (IX+DISPL),H  // $DD $CB [1:S8] $2C ; *UNDOCUMENTED* SRA (IX+N) then LD H,(IX+N)
		SRA     (IY+DISPL),H  // $FD $CB [1:S8] $2C ; *UNDOCUMENTED* SRA (IY+N) then LD H,(IY+N)
		SRA     (IX+DISPL),L  // $DD $CB [1:S8] $2D ; *UNDOCUMENTED* SRA (IX+N) then LD L,(IX+N)
		SRA     (IY+DISPL),L  // $FD $CB [1:S8] $2D ; *UNDOCUMENTED* SRA (IY+N) then LD L,(IY+N)
		SRL     (IX+DISPL),A  // $DD $CB [1:S8] $3F ; *UNDOCUMENTED* SRL (IX+N) then LD A,(IX+N)
		SRL     (IY+DISPL),A  // $FD $CB [1:S8] $3F ; *UNDOCUMENTED* SRL (IY+N) then LD A,(IY+N)
		SRL     (IX+DISPL),B  // $DD $CB [1:S8] $38 ; *UNDOCUMENTED* SRL (IX+N) then LD B,(IX+N)
		SRL     (IY+DISPL),B  // $FD $CB [1:S8] $38 ; *UNDOCUMENTED* SRL (IY+N) then LD B,(IY+N)
		SRL     (IX+DISPL),C  // $DD $CB [1:S8] $39 ; *UNDOCUMENTED* SRL (IX+N) then LD C,(IX+N)
		SRL     (IY+DISPL),C  // $FD $CB [1:S8] $39 ; *UNDOCUMENTED* SRL (IY+N) then LD C,(IY+N)
		SRL     (IX+DISPL),D  // $DD $CB [1:S8] $3A ; *UNDOCUMENTED* SRL (IX+N) then LD D,(IX+N)
		SRL     (IY+DISPL),D  // $FD $CB [1:S8] $3A ; *UNDOCUMENTED* SRL (IY+N) then LD D,(IY+N)
		SRL     (IX+DISPL),E  // $DD $CB [1:S8] $3B ; *UNDOCUMENTED* SRL (IX+N) then LD E,(IX+N)
		SRL     (IY+DISPL),E  // $FD $CB [1:S8] $3B ; *UNDOCUMENTED* SRL (IY+N) then LD E,(IY+N)
		SRL     (IX+DISPL),H  // $DD $CB [1:S8] $3C ; *UNDOCUMENTED* SRL (IX+N) then LD H,(IX+N)
		SRL     (IY+DISPL),H  // $FD $CB [1:S8] $3C ; *UNDOCUMENTED* SRL (IY+N) then LD H,(IY+N)
		SRL     (IX+DISPL),L  // $DD $CB [1:S8] $3D ; *UNDOCUMENTED* SRL (IX+N) then LD L,(IX+N)
		SRL     (IY+DISPL),L  // $FD $CB [1:S8] $3D ; *UNDOCUMENTED* SRL (IY+N) then LD L,(IY+N)

				


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