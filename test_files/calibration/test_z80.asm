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
; Z80 - 8 bit load instructions
;

		LD 		A,I          	// $ED $57                    
		LD 		A,R          	// $ED $5F                    
		LD 		A,A          	// $7F                        
		LD 		A,B          	// $78                        
		LD 		A,C          	// $79                        
		LD 		A,D          	// $7A                        
		LD 		A,E          	// $7B                        
		LD 		A,H          	// $7C                        
		LD 		A,L          	// $7D                        
		LD 		A,(HL)     		// $7E                        
		LD 		A,(BC)     		// $0A                        
		LD 		A,(DE)     		// $1A                        
		LD 		A,(IX+DISPL)   	// $DD $7E [2:S8]             
		LD 		A,(IY+DISPL)   	// $FD $7E [2:S8]             
		LD 		A,(U16)    		// $3A [2:U16]                
		LD 		A,U8         	// $3E [2:U8]                 
		LD 		B,A          	// $47                        
		LD 		B,B          	// $40                        
		LD 		B,C          	// $41                        
		LD 		B,D          	// $42                        
		LD 		B,E          	// $43                        
		LD 		B,H          	// $44                        
		LD 		B,L          	// $45                        
		LD 		B,(HL)     		// $46                        
		LD 		B,(IX+DISPL)   	// $DD $46 [2:S8]             
		LD 		B,(IY+DISPL)   	// $FD $46 [2:S8]             
		LD 		B,U8         	// $06 [2:U8]                 
		LD 		C,A          	// $4F                        
		LD 		C,B          	// $48                        
		LD 		C,C          	// $49                        
		LD 		C,D          	// $4A                        
		LD 		C,E          	// $4B                        
		LD 		C,H          	// $4C                        
		LD 		C,L          	// $4D                        
		LD 		C,(HL)     		// $4E                        
		LD 		C,(IX+DISPL)   	// $DD $4E [2:S8]             
		LD 		C,(IY+DISPL)   	// $FD $4E [2:S8]             
		LD 		C,U8         	// $0E [2:U8]                 
		LD 		D,A          	// $57                        
		LD 		D,B          	// $50                        
		LD 		D,C          	// $51                        
		LD 		D,D          	// $52                        
		LD 		D,E          	// $53                        
		LD 		D,H          	// $54                        
		LD 		D,L          	// $55                        
		LD 		D,(HL)     		// $56                        
		LD 		D,(IX+DISPL)   	// $DD $56 [2:S8]             
		LD 		D,(IY+DISPL)   	// $FD $56 [2:S8]             
		LD 		D,U8         	// $16 [2:U8]                 
		LD 		E,A          	// $5F                        
		LD 		E,B          	// $58                        
		LD 		E,C          	// $59                        
		LD 		E,D          	// $5A                        
		LD 		E,E          	// $5B                        
		LD 		E,H          	// $5C                        
		LD 		E,L          	// $5D                        
		LD 		E,(HL)     		// $5E                        
		LD 		E,(IX+DISPL)   	// $DD $5E [2:S8]             
		LD 		E,(IY+DISPL)   	// $FD $5E [2:S8]             
		LD 		E,U8         	// $1E [2:U8]                 
		LD 		H,A          	// $67                        
		LD 		H,B          	// $60                        
		LD 		H,C          	// $61                        
		LD 		H,D          	// $62                        
		LD 		H,E          	// $63                        
		LD 		H,H          	// $64                        
		LD 		H,L          	// $65                        
		LD 		H,(HL)     		// $66                        
		LD 		H,(IX+DISPL)   	// $DD $66 [2:S8]             
		LD 		H,(IY+DISPL)   	// $FD $66 [2:S8]             
		LD 		H,U8         	// $26 [2:U8]                 
		LD 		L,A          	// $6F                        
		LD 		L,B          	// $68                        
		LD 		L,C          	// $69                        
		LD 		L,D          	// $6A                        
		LD 		L,E          	// $6B                        
		LD 		L,H          	// $6C                        
		LD 		L,L          	// $6D                        
		LD 		L,(HL)     		// $6E                        
		LD 		L,(IX+DISPL)   	// $DD $6E [2:S8]             
		LD 		L,(IY+DISPL)   	// $FD $6E [2:S8]             
		LD 		L,U8         	// $2E [2:U8]                 
		LD 		(HL),A     		// $77                        
		LD 		(HL),B     		// $70                        
		LD 		(HL),C     		// $71                        
		LD 		(HL),D     		// $72                        
		LD 		(HL),E     		// $73                        
		LD 		(HL),H     		// $74                        
		LD 		(HL),L     		// $75                        
		LD 		(HL),U8    		// $36 [2:U8]                        
		LD 		(BC),A     		// $02                        
		LD 		(DE),A     		// $12                        
		LD 		(IX+DISPL),A   	// $DD $77 [1:S8]             
		LD 		(IX+DISPL),B   	// $DD $70 [1:S8]             
		LD 		(IX+DISPL),C   	// $DD $71 [1:S8]             
		LD 		(IX+DISPL),D   	// $DD $72 [1:S8]             
		LD 		(IX+DISPL),E   	// $DD $73 [1:S8]             
		LD 		(IX+DISPL),H   	// $DD $74 [1:S8]             
		LD 		(IX+DISPL),L   	// $DD $75 [1:S8]             
		LD 		(IX+DISPL),U8  	// $DD $36 [1:S8] [2:U8]      
		LD 		(IY+DISPL),A   	// $FD $77 [1:S8]             
		LD 		(IY+DISPL),B   	// $FD $70 [1:S8]             
		LD 		(IY+DISPL),C   	// $FD $71 [1:S8]             
		LD 		(IY+DISPL),D   	// $FD $72 [1:S8]             
		LD 		(IY+DISPL),E   	// $FD $73 [1:S8]             
		LD 		(IY+DISPL),H   	// $FD $74 [1:S8]             
		LD 		(IY+DISPL),L   	// $FD $75 [1:S8]             
		LD 		(IY+DISPL),U8  	// $FD $36 [1:S8] [2:U8]      
		LD 		(U16),A    		// $32 [1:U16]                
		LD 		I,A          	// $ED $47                    
		LD 		R,A          	// $ED $4F                    

;
; Z80 - 16 bit load instructions
;

		POP 	AF          	// $F1                        
		POP 	BC          	// $C1                        
		POP 	DE          	// $D1                        
		POP 	HL          	// $E1                        
		POP 	IX          	// $DD $E1                    
		POP 	IY          	// $FD $E1                    
		PUSH 	AF         		// $F5                        
		PUSH 	BC         		// $C5                        
		PUSH 	DE         		// $D5                        
		PUSH 	HL         		// $E5                        
		PUSH 	IX         		// $DD $E5                    
		PUSH 	IY         		// $FD $E5  
		LD 		SP,HL			// $F9
		LD 		SP,IX			// $DD $F9
		LD 		SP,IY        	// $FD $F9                  
		LD 		BC,U16       	// $01 [2:U16]                
		LD 		DE,U16       	// $11 [2:U16]                
		LD 		HL,U16       	// $21 [2:U16]                
		LD 		SP,U16       	// $31 [2:U16]                
		LD 		IX,U16       	// $DD $21 [2:U16]            
		LD 		IY,U16       	// $FD $21 [2:U16]            
		LD 		BC,(U16)   		// $ED $4B [2:U16]            
		LD 		DE,(U16)   		// $ED $5B [2:U16]            
		LD 		HL,(U16)   		// $2A [2:U16]                
		LD 		SP,(U16)   		// $ED $7B [2:U16]            
		LD 		IX,(U16)   		// $DD $2A [2:U16]            
		LD 		IY,(U16)   		// $FD $2A [2:U16]            
		LD 		(U16),BC   		// $ED $43 [1:U16]            
		LD 		(U16),DE   		// $ED $53 [1:U16]            
		LD 		(U16),HL   		// $22 [1:U16]                
		LD 		(U16),SP   		// $ED $73 [1:U16]            
		LD 		(U16),IX   		// $DD $22 [1:U16]            
		LD 		(U16),IY   		// $FD $22 [1:U16]            

;
; Z80 - Exchanges
;

		EX 		DE,HL        	// $EB                        
		EX 		AF,AF'       	// $08                        
		EXX             		// $D9                        
		EX 		(SP),HL    		// $E3                        
		EX 		(SP),IX    		// $DD $E3                    
		EX 		(SP),IY    		// $FD $E3                    

; Z80 - Block transfer and search groups

		LDI             		// $ED $A0                    
		LDIR            		// $ED $B0                    
		LDD             		// $ED $A8                    
		LDDR            		// $ED $B8                    
		CPI             		// $ED $A1                    
		CPIR            		// $ED $B1                    
		CPD             		// $ED $A9                    
		CPDR            		// $ED $B9                    

;
; Z80 - 8 bit arithmetic and logic
;
; These are in twice for the most part as correct instructions
; would be XOR A,H or AND A,30H however XOR H and AND 30H are
; also permitted
;

		ADC 	(HL)      		// $8E
		ADC 	(IX+DISPL)    	// $DD $8E [1:S8]
		ADC 	(IY+DISPL)    	// $FD $8E [1:S8]
		ADC 	A           	// $8F
		ADC 	B           	// $88
		ADC 	C           	// $89
		ADC 	D           	// $8A
		ADC 	E           	// $8B
		ADC 	H           	// $8C
		ADC 	L				// $8D
		ADC 	U8          	// $CE [1:U8]
		ADD 	(HL)      		// $86
		ADD 	(IX+DISPL)    	// $DD $86 [1:S8]
		ADD 	(IY+DISPL)    	// $FD $86 [1:S8]
		ADD 	A           	// $87
		ADD 	B           	// $80
		ADD 	C           	// $81
		ADD 	D           	// $82
		ADD 	E           	// $83
		ADD 	H           	// $84
		ADD 	L           	// $85
		ADD 	U8          	// $C6 [1:U8]
		AND 	(HL)      		// $A6
		AND 	(IX+DISPL)    	// $DD $A6 [1:S8]
		AND 	(IY+DISPL)    	// $FD $A6 [1:S8]
		AND 	A           	// $A7
		AND 	B           	// $A0
		AND 	C           	// $A1
		AND 	D           	// $A2
		AND 	E           	// $A3
		AND 	H           	// $A4
		AND 	L           	// $A5
		AND 	U8          	// $E6 [1:U8]
		CP 		(HL)       		// $BE
		CP 		(IX+DISPL)     	// $DD $BE [1:S8]
		CP 		(IY+DISPL)     	// $FD $BE [1:S8]
		CP 		A            	// $BF
		CP 		B            	// $B8
		CP 		C            	// $B9
		CP 		D            	// $BA
		CP 		E            	// $BB
		CP 		H            	// $BC
		CP 		L            	// $BD
		CP 		U8           	// $FE [1:U8]
		OR 		(HL)       		// $B6
		OR 		(IX+DISPL)     	// $DD $B6 [1:S8]
		OR 		(IY+DISPL)     	// $FD $B6 [1:S8]
		OR 		A            	// $B7
		OR 		B            	// $B0
		OR 		C            	// $B1
		OR 		D            	// $B2
		OR 		E            	// $B3
		OR 		H            	// $B4
		OR 		L            	// $B5
		OR 		U8           	// $F6 [1:U8]
		SBC 	(HL)      		// $9E
		SBC 	(IX+DISPL)    	// $DD $9E [1:S8]
		SBC 	(IY+DISPL)    	// $FD $9E [1:S8]
		SBC 	A           	// $9F
		SBC 	B           	// $98
		SBC 	C           	// $99
		SBC 	D           	// $9A
		SBC 	E           	// $9B
		SBC 	H           	// $9C
		SBC 	L           	// $9D
		SBC 	U8          	// $DE [1:U8]
		SUB 	(HL)      		// $96
		SUB 	(IX+DISPL)    	// $DD $96 [1:S8]
		SUB 	(IY+DISPL)    	// $FD $96 [1:S8]
		SUB 	A           	// $97
		SUB 	B           	// $90
		SUB 	C           	// $91
		SUB 	D           	// $92
		SUB		E           	// $93
		SUB 	H           	// $94
		SUB 	L           	// $95
		SUB 	U8          	// $D6 [1:U8]
		XOR 	(HL)      		// $AE
		XOR 	(IX+DISPL)    	// $DD $AE [1:S8]
		XOR 	(IY+DISPL)    	// $FD $AE [1:S8]
		XOR 	A           	// $AF
		XOR 	B           	// $A8
		XOR 	C           	// $A9
		XOR 	D           	// $AA
		XOR 	E           	// $AB
		XOR 	H           	// $AC
		XOR 	L           	// $AD
		XOR 	U8          	// $EE [1:U8]
		ADC 	A,(HL)    		// $8E                        
		ADC 	A,(IX+DISPL)  	// $DD $8E [2:S8]             
		ADC 	A,(IY+DISPL)  	// $FD $8E [2:S8]             
		ADC 	A,A         	// $8F                        
		ADC 	A,B         	// $88                        
		ADC 	A,C         	// $89                        
		ADC 	A,D         	// $8A                        
		ADC 	A,E         	// $8B                        
		ADC 	A,H         	// $8C                        
		ADC 	A,L         	// $8D
		ADC 	A,U8        	// $CE [2:U8]                        
		ADD 	A,(HL)    		// $86                        
		ADD 	A,(IX+DISPL)  	// $DD $86 [2:S8]             
		ADD 	A,(IY+DISPL)  	// $FD $86 [2:S8]             
		ADD 	A,A         	// $87                        
		ADD 	A,B         	// $80                        
		ADD 	A,C         	// $81                        
		ADD 	A,D         	// $82                        
		ADD 	A,E         	// $83                        
		ADD 	A,H         	// $84                        
		ADD 	A,L         	// $85                        
		ADD 	A,U8        	// $C6 [2:U8]                        
		AND 	A,(HL)    		// $A6                        
		AND 	A,(IX+DISPL)  	// $DD $A6 [2:S8]             
		AND 	A,(IY+DISPL)  	// $FD $A6 [2:S8]             
		AND 	A,A         	// $A7                        
		AND 	A,B         	// $A0                        
		AND 	A,C         	// $A1                        
		AND 	A,D         	// $A2                        
		AND 	A,E         	// $A3                        
		AND 	A,H         	// $A4                        
		AND 	A,L         	// $A5                        
		AND 	A,U8        	// $E6 [2:U8]                        
		CP 		A,(HL)     		// $BE                        
		CP 		A,(IX+DISPL)   	// $DD $BE [2:S8]             
		CP 		A,(IY+DISPL)   	// $FD $BE [2:S8]             
		CP 		A,A          	// $BF                        
		CP 		A,B          	// $B8                        
		CP 		A,C          	// $B9                        
		CP 		A,D          	// $BA                        
		CP 		A,E          	// $BB                        
		CP 		A,H          	// $BC                        
		CP 		A,L          	// $BD                        
		CP 		A,U8         	// $FE [2:U8]                        
		DEC 	(HL)      		// $35                        
		DEC 	(IX+DISPL)    	// $DD $35 [1:S8]             
		DEC 	(IY+DISPL)    	// $FD $35 [1:S8]             
		DEC 	A           	// $3D                        
		DEC 	B           	// $05                        
		DEC 	C           	// $0D                        
		DEC 	D           	// $15                        
		DEC 	E           	// $1D                        
		DEC 	H           	// $25                        
		DEC 	L           	// $2D                        
		INC 	(HL)      		// $34                        
		INC 	(IX+DISPL)    	// $DD $34 [1:S8]             
		INC 	(IY+DISPL)    	// $FD $34 [1:S8]             
		INC 	A           	// $3C                        
		INC 	B           	// $04                        
		INC 	C           	// $0C                        
		INC 	D           	// $14                        
		INC 	E           	// $1C                        
		INC 	H           	// $24                        
		INC 	L           	// $2C                        
		OR 		A,(HL)     		// $B6                        
		OR 		A,(IX+DISPL)   	// $DD $B6 [2:S8]             
		OR 		A,(IY+DISPL)   	// $FD $B6 [2:S8]             
		OR 		A,A          	// $B7                        
		OR 		A,B          	// $B0                        
		OR 		A,C          	// $B1                        
		OR 		A,D          	// $B2                        
		OR 		A,E          	// $B3                        
		OR 		A,H          	// $B4                        
		OR 		A,L          	// $B5                        
		OR 		A,U8         	// $F6 [2:U8]                        
		SBC 	A,(HL)    		// $9E                        
		SBC 	A,(IX+DISPL)  	// $DD $9E [2:S8]             
		SBC 	A,(IY+DISPL)  	// $FD $9E [2:S8]             
		SBC 	A,A         	// $9F                        
		SBC 	A,B         	// $98                        
		SBC 	A,C         	// $99                        
		SBC 	A,D         	// $9A                        
		SBC 	A,E         	// $9B                        
		SBC 	A,H         	// $9C                        
		SBC 	A,L         	// $9D                        
		SBC 	A,U8        	// $DE [2:U8]                        
		SUB 	A,(HL)    		// $96                        
		SUB 	A,(IX+DISPL)  	// $DD $96 [2:S8]             
		SUB 	A,(IY+DISPL)  	// $FD $96 [2:S8]             
		SUB 	A,A         	// $97                        
		SUB 	A,B         	// $90                        
		SUB 	A,C         	// $91                        
		SUB 	A,D         	// $92                        
		SUB 	A,E         	// $93                        
		SUB 	A,H         	// $94                        
		SUB 	A,L         	// $95                        
		SUB 	A,U8        	// $D6 [2:U8]                        
		XOR 	A,(HL)    		// $AE                        
		XOR 	A,(IX+DISPL)  	// $DD $AE [2:S8]             
		XOR 	A,(IY+DISPL)  	// $FD $AE [2:S8]             
		XOR 	A,A         	// $AF                        
		XOR 	A,B         	// $A8                        
		XOR 	A,C         	// $A9                        
		XOR 	A,D         	// $AA                        
		XOR 	A,E         	// $AB                        
		XOR 	A,H         	// $AC                        
		XOR 	A,L         	// $AD                        
		XOR 	A,U8        	// $EE [2:U8]                        

;
; Z80 - General purpose arithmetic and CPU control groups
;

		DAA             		// $27                        
		CPL             		// $2F                        
		NEG             		// $ED $44                    
		CCF             		// $3F                        
		SCF             		// $37                        
		NOP             		// $00                        
		HALT            		// $76                        
		DI              		// $F3                        
		EI              		// $FB                        
		IM 		0           	// $ED [1:IM]                 
		IM 		1           	// $ED [1:IM]                 
		IM 		2           	// $ED [1:IM]                 

;
; Z80 - 16 bit arithmetic group
;

		ADD 	HL,BC       	// $09                        
		ADD 	HL,DE       	// $19                        
		ADD 	HL,HL       	// $29                        
		ADD 	HL,SP       	// $39                        
		ADD 	IX,BC       	// $DD $09                    
		ADD 	IX,DE       	// $DD $19                    
		ADD 	IX,SP       	// $DD $39                    
		ADD 	IX,IX       	// $DD $29                    
		ADD 	IY,BC       	// $FD $09                    
		ADD 	IY,DE       	// $FD $19                    
		ADD 	IY,SP       	// $FD $39                    
		ADD 	IY,IY       	// $FD $29                    
		ADC 	HL,BC       	// $ED $4A                    
		ADC 	HL,DE       	// $ED $5A                    
		ADC 	HL,HL       	// $ED $6A                    
		ADC 	HL,SP       	// $ED $7A                    
		SBC 	HL,BC       	// $ED $42                    
		SBC 	HL,DE       	// $ED $52                    
		SBC 	HL,HL       	// $ED $62                    
		SBC 	HL,SP       	// $ED $72                    
		INC 	BC          	// $03                        
		INC 	DE          	// $13                        
		INC 	HL          	// $23                        
		INC 	SP          	// $33                        
		INC 	IX          	// $DD $23                    
		INC 	IY          	// $FD $23                    
		DEC 	BC          	// $0B                        
		DEC 	DE          	// $1B                        
		DEC 	HL          	// $2B                        
		DEC 	SP          	// $3B                        
		DEC 	IX          	// $DD $2B                    
		DEC 	IY          	// $FD $2B                    

;
; Z80 - Rotate and shift group
;

		RLCA            		// $07                        
		RLA             		// $17                        
		RRCA            		// $0F                        
		RRA             		// $1F                        
		RLC 	B           	// $CB $00                    
		RLC 	C           	// $CB $01                    
		RLC 	D           	// $CB $02                    
		RLC 	E           	// $CB $03                    
		RLC 	H           	// $CB $04                    
		RLC 	L           	// $CB $05                    
		RLC 	A           	// $CB $07                    
		RLC 	(HL)      		// $CB $06                    
		RLC 	(IX+DISPL)    	// $DD $CB [1:S8] $06         
		RLC 	(IY+DISPL)    	// $FD $CB [1:S8] $06         
		RL 		B            	// $CB $10                    
		RL 		C            	// $CB $11                    
		RL 		D            	// $CB $12                    
		RL 		E            	// $CB $13                    
		RL 		H            	// $CB $14                    
		RL 		L            	// $CB $15                    
		RL 		A            	// $CB $17                    
		RL 		(HL)       		// $CB $16                    
		RL 		(IX+DISPL)     	// $DD $CB [1:S8] $16         
		RL 		(IY+DISPL)     	// $FD $CB [1:S8] $16         
		RRC 	B           	// $CB $08                    
		RRC 	C           	// $CB $09                    
		RRC 	D           	// $CB $0A                    
		RRC 	E           	// $CB $0B                    
		RRC 	H           	// $CB $0C                    
		RRC 	L           	// $CB $0D                    
		RRC 	A           	// $CB $0F                    
		RRC 	(HL)      		// $CB $0E                    
		RRC 	(IX+DISPL)    	// $DD $CB [1:S8] $0E         
		RRC 	(IY+DISPL)    	// $FD $CB [1:S8] $0E         
		RR 		B            	// $CB $18                    
		RR 		C            	// $CB $19                    
		RR 		D            	// $CB $1A                    
		RR 		E            	// $CB $1B                    
		RR 		H            	// $CB $1C                    
		RR		L            	// $CB $1D                    
		RR 		A            	// $CB $1F                    
		RR 		(HL)       		// $CB $1E                    
		RR 		(IX+DISPL)     	// $DD $CB [1:S8] $1E         
		RR 		(IY+DISPL)     	// $FD $CB [1:S8] $1E         
		SLA 	B           	// $CB $20                    
		SLA 	C           	// $CB $21                    
		SLA 	D           	// $CB $22                    
		SLA 	E           	// $CB $23                    
		SLA 	H           	// $CB $24                    
		SLA 	L           	// $CB $25                    
		SLA 	A           	// $CB $27                    
		SLA 	(HL)      		// $CB $26                    
		SLA 	(IX+DISPL)    	// $DD $CB [1:S8] $26         
		SLA 	(IY+DISPL)    	// $FD $CB [1:S8] $26         
		SRA 	B           	// $CB $28                    
		SRA 	C           	// $CB $29                    
		SRA 	D           	// $CB $2A                    
		SRA 	E           	// $CB $2B                    
		SRA 	H           	// $CB $2C                    
		SRA 	L           	// $CB $2D                    
		SRA 	A           	// $CB $2F                    
		SRA 	(HL)      		// $CB $2E                    
		SRA 	(IX+DISPL)    	// $DD $CB [1:S8] $2E         
		SRA 	(IY+DISPL)    	// $FD $CB [1:S8] $2E         
		SRL 	B           	// $CB $38                    
		SRL 	C           	// $CB $39                    
		SRL 	D           	// $CB $3A                    
		SRL 	E           	// $CB $3B                    
		SRL 	H           	// $CB $3C                    
		SRL 	L           	// $CB $3D                    
		SRL 	A           	// $CB $3F                    
		SRL 	(HL)      		// $CB $3E                    
		SRL 	(IX+DISPL)    	// $DD $CB [1:S8] $3E         
		SRL 	(IY+DISPL)    	// $FD $CB [1:S8] $3E         
		RLD             		// $ED $6F                    
		RRD             		// $ED $67                    

;
; Z80 - Bit set reset and test group
;

		BIT		0,A        		// $CB %01[1:B3]111           
		BIT		0,B        		// $CB %01[1:B3]000           
		BIT		0,C        		// $CB %01[1:B3]001           
		BIT		0,D        		// $CB %01[1:B3]010           
		BIT		0,E        		// $CB %01[1:B3]011           
		BIT		0,H        		// $CB %01[1:B3]100           
		BIT		0,L        		// $CB %01[1:B3]101           
		BIT		0,(HL)   		// $CB %01[1:B3]110           
		BIT		0,(IX+DISPL) 	// $DD $CB [2:S8] %01[1:B3]110
		BIT		0,(IY+DISPL) 	// $FD $CB [2:S8] %01[1:B3]110
		SET		0,A        		// $CB %11[1:B3]111           
		SET		0,B        		// $CB %11[1:B3]000           
		SET		0,C        		// $CB %11[1:B3]001           
		SET		0,D       	 	// $CB %11[1:B3]010           
		SET		0,E        		// $CB %11[1:B3]011           
		SET		0,H        		// $CB %11[1:B3]100           
		SET		0,L        		// $CB %11[1:B3]101           
		SET		0,(HL)   		// $CB %11[1:B3]110           
		SET		0,(IX+DISPL) 	// $DD $CB [2:S8] %11[1:B3]110
		SET		0,(IY+DISPL) 	// $FD $CB [2:S8] %11[1:B3]110
		RES		0,A        		// $CB %10[1:B3]111           
		RES		0,B        		// $CB %10[1:B3]000           
		RES		0,C        		// $CB %10[1:B3]001           
		RES		0,D        		// $CB %10[1:B3]010           
		RES		0,E        		// $CB %10[1:B3]011           
		RES		0,H        		// $CB %10[1:B3]100           
		RES		0,L        		// $CB %10[1:B3]101           
		RES		0,(HL)   		// $CB %10[1:B3]110           
		RES		0,(IX+DISPL) 	// $DD $CB [2:S8] %10[1:B3]110
		RES		0,(IY+DISPL) 	// $FD $CB [2:S8] %10[1:B3]110

		BIT		1,A        		// $CB %01[1:B3]111           
		BIT		1,B        		// $CB %01[1:B3]000           
		BIT		1,C        		// $CB %01[1:B3]001           
		BIT		1,D        		// $CB %01[1:B3]010           
		BIT		1,E        		// $CB %01[1:B3]011           
		BIT		1,H        		// $CB %01[1:B3]100           
		BIT		1,L        		// $CB %01[1:B3]101           
		BIT		1,(HL)   		// $CB %01[1:B3]110           
		BIT		1,(IX+DISPL) 	// $DD $CB [2:S8] %01[1:B3]110
		BIT		1,(IY+DISPL) 	// $FD $CB [2:S8] %01[1:B3]110
		SET		1,A        		// $CB %11[1:B3]111           
		SET		1,B        		// $CB %11[1:B3]000           
		SET		1,C        		// $CB %11[1:B3]001           
		SET		1,D       	 	// $CB %11[1:B3]010           
		SET		1,E        		// $CB %11[1:B3]011           
		SET		1,H        		// $CB %11[1:B3]100           
		SET		1,L        		// $CB %11[1:B3]101           
		SET		1,(HL)   		// $CB %11[1:B3]110           
		SET		1,(IX+DISPL) 	// $DD $CB [2:S8] %11[1:B3]110
		SET		1,(IY+DISPL) 	// $FD $CB [2:S8] %11[1:B3]110
		RES		1,A        		// $CB %10[1:B3]111           
		RES		1,B        		// $CB %10[1:B3]000           
		RES		1,C        		// $CB %10[1:B3]001           
		RES		1,D        		// $CB %10[1:B3]010           
		RES		1,E        		// $CB %10[1:B3]011           
		RES		1,H        		// $CB %10[1:B3]100           
		RES		1,L        		// $CB %10[1:B3]101           
		RES		1,(HL)   		// $CB %10[1:B3]110           
		RES		1,(IX+DISPL) 	// $DD $CB [2:S8] %10[1:B3]110
		RES		1,(IY+DISPL) 	// $FD $CB [2:S8] %10[1:B3]110

		BIT		2,A        		// $CB %01[1:B3]111           
		BIT		2,B        		// $CB %01[1:B3]000           
		BIT		2,C        		// $CB %01[1:B3]001           
		BIT		2,D        		// $CB %01[1:B3]010           
		BIT		2,E        		// $CB %01[1:B3]011           
		BIT		2,H        		// $CB %01[1:B3]100           
		BIT		2,L        		// $CB %01[1:B3]101           
		BIT		2,(HL)   		// $CB %01[1:B3]110           
		BIT		2,(IX+DISPL) 	// $DD $CB [2:S8] %01[1:B3]110
		BIT		2,(IY+DISPL) 	// $FD $CB [2:S8] %01[1:B3]110
		SET		2,A        		// $CB %11[1:B3]111           
		SET		2,B        		// $CB %11[1:B3]000           
		SET		2,C        		// $CB %11[1:B3]001           
		SET		2,D       	 	// $CB %11[1:B3]010           
		SET		2,E        		// $CB %11[1:B3]011           
		SET		2,H        		// $CB %11[1:B3]100           
		SET		2,L        		// $CB %11[1:B3]101           
		SET		2,(HL)   		// $CB %11[1:B3]110           
		SET		2,(IX+DISPL) 	// $DD $CB [2:S8] %11[1:B3]110
		SET		2,(IY+DISPL) 	// $FD $CB [2:S8] %11[1:B3]110
		RES		2,A        		// $CB %10[1:B3]111           
		RES		2,B        		// $CB %10[1:B3]000           
		RES		2,C        		// $CB %10[1:B3]001           
		RES		2,D        		// $CB %10[1:B3]010           
		RES		2,E        		// $CB %10[1:B3]011           
		RES		2,H        		// $CB %10[1:B3]100           
		RES		2,L        		// $CB %10[1:B3]101           
		RES		2,(HL)   		// $CB %10[1:B3]110           
		RES		2,(IX+DISPL) 	// $DD $CB [2:S8] %10[1:B3]110
		RES		2,(IY+DISPL) 	// $FD $CB [2:S8] %10[1:B3]110		

		BIT		3,A        		// $CB %01[1:B3]111           
		BIT		3,B        		// $CB %01[1:B3]000           
		BIT		3,C        		// $CB %01[1:B3]001           
		BIT		3,D        		// $CB %01[1:B3]010           
		BIT		3,E        		// $CB %01[1:B3]011           
		BIT		3,H        		// $CB %01[1:B3]100           
		BIT		3,L        		// $CB %01[1:B3]101           
		BIT		3,(HL)   		// $CB %01[1:B3]110           
		BIT		3,(IX+DISPL) 	// $DD $CB [2:S8] %01[1:B3]110
		BIT		3,(IY+DISPL) 	// $FD $CB [2:S8] %01[1:B3]110
		SET		3,A        		// $CB %11[1:B3]111           
		SET		3,B        		// $CB %11[1:B3]000           
		SET		3,C        		// $CB %11[1:B3]001           
		SET		3,D       	 	// $CB %11[1:B3]010           
		SET		3,E        		// $CB %11[1:B3]011           
		SET		3,H        		// $CB %11[1:B3]100           
		SET		3,L        		// $CB %11[1:B3]101           
		SET		3,(HL)   		// $CB %11[1:B3]110           
		SET		3,(IX+DISPL) 	// $DD $CB [2:S8] %11[1:B3]110
		SET		3,(IY+DISPL) 	// $FD $CB [2:S8] %11[1:B3]110
		RES		3,A        		// $CB %10[1:B3]111           
		RES		3,B        		// $CB %10[1:B3]000           
		RES		3,C        		// $CB %10[1:B3]001           
		RES		3,D        		// $CB %10[1:B3]010           
		RES		3,E        		// $CB %10[1:B3]011           
		RES		3,H        		// $CB %10[1:B3]100           
		RES		3,L        		// $CB %10[1:B3]101           
		RES		3,(HL)   		// $CB %10[1:B3]110           
		RES		3,(IX+DISPL) 	// $DD $CB [2:S8] %10[1:B3]110
		RES		3,(IY+DISPL) 	// $FD $CB [2:S8] %10[1:B3]110

		BIT		4,A        		// $CB %01[1:B3]111           
		BIT		4,B        		// $CB %01[1:B3]000           
		BIT		4,C        		// $CB %01[1:B3]001           
		BIT		4,D        		// $CB %01[1:B3]010           
		BIT		4,E        		// $CB %01[1:B3]011           
		BIT		4,H        		// $CB %01[1:B3]100           
		BIT		4,L        		// $CB %01[1:B3]101           
		BIT		4,(HL)   		// $CB %01[1:B3]110           
		BIT		4,(IX+DISPL) 	// $DD $CB [2:S8] %01[1:B3]110
		BIT		4,(IY+DISPL) 	// $FD $CB [2:S8] %01[1:B3]110
		SET		4,A        		// $CB %11[1:B3]111           
		SET		4,B        		// $CB %11[1:B3]000           
		SET		4,C        		// $CB %11[1:B3]001           
		SET		4,D       	 	// $CB %11[1:B3]010           
		SET		4,E        		// $CB %11[1:B3]011           
		SET		4,H        		// $CB %11[1:B3]100           
		SET		4,L        		// $CB %11[1:B3]101           
		SET		4,(HL)   		// $CB %11[1:B3]110           
		SET		4,(IX+DISPL) 	// $DD $CB [2:S8] %11[1:B3]110
		SET		4,(IY+DISPL) 	// $FD $CB [2:S8] %11[1:B3]110
		RES		4,A        		// $CB %10[1:B3]111           
		RES		4,B        		// $CB %10[1:B3]000           
		RES		4,C        		// $CB %10[1:B3]001           
		RES		4,D        		// $CB %10[1:B3]010           
		RES		4,E        		// $CB %10[1:B3]011           
		RES		4,H        		// $CB %10[1:B3]100           
		RES		4,L        		// $CB %10[1:B3]101           
		RES		4,(HL)   		// $CB %10[1:B3]110           
		RES		4,(IX+DISPL) 	// $DD $CB [2:S8] %10[1:B3]110
		RES		4,(IY+DISPL) 	// $FD $CB [2:S8] %10[1:B3]110

		BIT		5,A        		// $CB %01[1:B3]111           
		BIT		5,B        		// $CB %01[1:B3]000           
		BIT		5,C        		// $CB %01[1:B3]001           
		BIT		5,D        		// $CB %01[1:B3]010           
		BIT		5,E        		// $CB %01[1:B3]011           
		BIT		5,H        		// $CB %01[1:B3]100           
		BIT		5,L        		// $CB %01[1:B3]101           
		BIT		5,(HL)   		// $CB %01[1:B3]110           
		BIT		5,(IX+DISPL) 	// $DD $CB [2:S8] %01[1:B3]110
		BIT		5,(IY+DISPL) 	// $FD $CB [2:S8] %01[1:B3]110
		SET		5,A        		// $CB %11[1:B3]111           
		SET		5,B        		// $CB %11[1:B3]000           
		SET		5,C        		// $CB %11[1:B3]001           
		SET		5,D       	 	// $CB %11[1:B3]010           
		SET		5,E        		// $CB %11[1:B3]011           
		SET		5,H        		// $CB %11[1:B3]100           
		SET		5,L        		// $CB %11[1:B3]101           
		SET		5,(HL)   		// $CB %11[1:B3]110           
		SET		5,(IX+DISPL) 	// $DD $CB [2:S8] %11[1:B3]110
		SET		5,(IY+DISPL) 	// $FD $CB [2:S8] %11[1:B3]110
		RES		5,A        		// $CB %10[1:B3]111           
		RES		5,B        		// $CB %10[1:B3]000           
		RES		5,C        		// $CB %10[1:B3]001           
		RES		5,D        		// $CB %10[1:B3]010           
		RES		5,E        		// $CB %10[1:B3]011           
		RES		5,H        		// $CB %10[1:B3]100           
		RES		5,L        		// $CB %10[1:B3]101           
		RES		5,(HL)   		// $CB %10[1:B3]110           
		RES		5,(IX+DISPL) 	// $DD $CB [2:S8] %10[1:B3]110
		RES		5,(IY+DISPL) 	// $FD $CB [2:S8] %10[1:B3]110

		BIT		6,A        		// $CB %01[1:B3]111           
		BIT		6,B        		// $CB %01[1:B3]000           
		BIT		6,C        		// $CB %01[1:B3]001           
		BIT		6,D        		// $CB %01[1:B3]010           
		BIT		6,E        		// $CB %01[1:B3]011           
		BIT		6,H        		// $CB %01[1:B3]100           
		BIT		6,L        		// $CB %01[1:B3]101           
		BIT		6,(HL)   		// $CB %01[1:B3]110           
		BIT		6,(IX+DISPL) 	// $DD $CB [2:S8] %01[1:B3]110
		BIT		6,(IY+DISPL) 	// $FD $CB [2:S8] %01[1:B3]110
		SET		6,A        		// $CB %11[1:B3]111           
		SET		6,B        		// $CB %11[1:B3]000           
		SET		6,C        		// $CB %11[1:B3]001           
		SET		6,D       	 	// $CB %11[1:B3]010           
		SET		6,E        		// $CB %11[1:B3]011           
		SET		6,H        		// $CB %11[1:B3]100           
		SET		6,L        		// $CB %11[1:B3]101           
		SET		6,(HL)   		// $CB %11[1:B3]110           
		SET		6,(IX+DISPL) 	// $DD $CB [2:S8] %11[1:B3]110
		SET		6,(IY+DISPL) 	// $FD $CB [2:S8] %11[1:B3]110
		RES		6,A        		// $CB %10[1:B3]111           
		RES		6,B        		// $CB %10[1:B3]000           
		RES		6,C        		// $CB %10[1:B3]001           
		RES		6,D        		// $CB %10[1:B3]010           
		RES		6,E        		// $CB %10[1:B3]011           
		RES		6,H        		// $CB %10[1:B3]100           
		RES		6,L        		// $CB %10[1:B3]101           
		RES		6,(HL)   		// $CB %10[1:B3]110           
		RES		6,(IX+DISPL) 	// $DD $CB [2:S8] %10[1:B3]110
		RES		6,(IY+DISPL) 	// $FD $CB [2:S8] %10[1:B3]110

		BIT		7,A        		// $CB %01[1:B3]111           
		BIT		7,B        		// $CB %01[1:B3]000           
		BIT		7,C        		// $CB %01[1:B3]001           
		BIT		7,D        		// $CB %01[1:B3]010           
		BIT		7,E        		// $CB %01[1:B3]011           
		BIT		7,H        		// $CB %01[1:B3]100           
		BIT		7,L        		// $CB %01[1:B3]101           
		BIT		7,(HL)   		// $CB %01[1:B3]110           
		BIT		7,(IX+DISPL) 	// $DD $CB [2:S8] %01[1:B3]110
		BIT		7,(IY+DISPL) 	// $FD $CB [2:S8] %01[1:B3]110
		SET		7,A        		// $CB %11[1:B3]111           
		SET		7,B        		// $CB %11[1:B3]000           
		SET		7,C        		// $CB %11[1:B3]001           
		SET		7,D       	 	// $CB %11[1:B3]010           
		SET		7,E        		// $CB %11[1:B3]011           
		SET		7,H        		// $CB %11[1:B3]100           
		SET		7,L        		// $CB %11[1:B3]101           
		SET		7,(HL)   		// $CB %11[1:B3]110           
		SET		7,(IX+DISPL) 	// $DD $CB [2:S8] %11[1:B3]110
		SET		7,(IY+DISPL) 	// $FD $CB [2:S8] %11[1:B3]110
		RES		7,A        		// $CB %10[1:B3]111           
		RES		7,B        		// $CB %10[1:B3]000           
		RES		7,C        		// $CB %10[1:B3]001           
		RES		7,D        		// $CB %10[1:B3]010           
		RES		7,E        		// $CB %10[1:B3]011           
		RES		7,H        		// $CB %10[1:B3]100           
		RES		7,L        		// $CB %10[1:B3]101           
		RES		7,(HL)   		// $CB %10[1:B3]110           
		RES		7,(IX+DISPL) 	// $DD $CB [2:S8] %10[1:B3]110
		RES		7,(IY+DISPL) 	// $FD $CB [2:S8] %10[1:B3]110
;
; Z80 - Jump group
;

		JP 		U16          	// $C3 [1:U16]                
		JP 		NZ,U16       	// $C2 [2:U16]                
		JP 		Z,U16        	// $CA [2:U16]                
		JP 		NC,U16       	// $D2 [2:U16]                
		JP 		C,U16        	// $DA [2:U16]                
		JP 		PO,U16       	// $E2 [2:U16]                
		JP 		PE,U16       	// $EA [2:U16]                
		JP 		P,U16        	// $F2 [2:U16]                
		JP 		M,U16        	// $FA [2:U16]                
		JR 		$+125          	// $18 [1:R8]                 
		JR 		C,$+125        	// $38 [2:R8]                 
		JR 		NC,$+125       	// $30 [2:R8]                 
		JR 		Z,$+125        	// $28 [2:R8]                 
		JR 		NZ,$+125       	// $20 [2:R8]                 
		JP 		(HL)       		// $E9                        
		JP 		(IX) 	      	// $DD $E9                    
		JP 		(IY)	       	// $FD $E9                    
		DJNZ 	$+125       	// $10 [1:R8]                 

;
; Z80 - Call and return group
;

		CALL 	U16        		// $CD [1:U16]                
		CALL 	NZ,U16     		// $C4 [2:U16]                
		CALL 	Z,U16      		// $CC [2:U16]                
		CALL 	NC,U16     		// $D4 [2:U16]                
		CALL 	C,U16      		// $DC [2:U16]                
		CALL 	PO,U16     		// $E4 [2:U16]                
		CALL 	PE,U16     		// $EC [2:U16]                
		CALL 	P,U16      		// $F4 [2:U16]                
		CALL 	M,U16      		// $FC [2:U16]                
		RET             		// $C9                        
		RET 	NZ          	// $C0                        
		RET 	Z           	// $C8                        
		RET 	NC          	// $D0                        
		RET 	C           	// $D8                        
		RET 	PO          	// $E0                        
		RET 	PE          	// $E8                        
		RET 	P           	// $F0                        
		RET 	M           	// $F8                        
		RETI            		// $ED $4D                    
		RETN            		// $ED $45                    
		RST 	0          		// %11[1:RST]111             
		RST 	1          		// %11[1:RST]111             
		RST 	2          		// %11[1:RST]111             
		RST 	3          		// %11[1:RST]111             
		RST 	4          		// %11[1:RST]111             
		RST 	5          		// %11[1:RST]111             
		RST 	6          		// %11[1:RST]111             
		RST 	7          		// %11[1:RST]111             
		RST 	08H        		// %11[1:RST]111             
		RST 	10H        		// %11[1:RST]111             
		RST 	18H        		// %11[1:RST]111             
		RST 	20H        		// %11[1:RST]111             
		RST 	28H        		// %11[1:RST]111             
		RST 	30H        		// %11[1:RST]111             
		RST 	38H        		// %11[1:RST]111             

;
; Z80 - Input and output group
;

		IN 		A,(U8)     		// $DB [2:U8]                 
		IN 		B,(C)      		// $ED $40                    
		IN 		C,(C)      		// $ED $48                    
		IN 		D,(C)      		// $ED $50                    
		IN 		E,(C)      		// $ED $58                    
		IN 		H,(C)      		// $ED $60                    
		IN 		L,(C)      		// $ED $68                    
		IN 		A,(C)      		// $ED $78                    
		INI             		// $ED $A2                    
		INIR            		// $ED $B2                    
		IND             		// $ED $AA                    
		INDR            		// $ED $BA                    
		OUT 	(U8),A    		// $D3 [1:U8]                 
		OUT 	(C),B     		// $ED $41                    
		OUT 	(C),C     		// $ED $49                    
		OUT 	(C),D     		// $ED $51                    
		OUT 	(C),E     		// $ED $59                    
		OUT 	(C),H     		// $ED $61                    
		OUT 	(C),L     		// $ED $69                    
		OUT 	(C),A     		// $ED $79                    
		OUTI            		// $ED $A3                    
		OTIR            		// $ED $B3                    
		OUTD            		// $ED $AB                    
		OTDR            		// $ED $BB             

;
; Z80 - Test the pseudo instructions where XXX (IX) --> XXX (IX+0) as there
;       is no official (IX) or (IY) option other than JP (IX) or JP (IY)
;

		ADC		A,(IX)			// $DD $8E $00	
		ADC		A,(IY)			// $FD $8E $00	
		ADC		(IX) 			// $DD $8E $00	
		ADC		(IY)			// $FD $8E $00	
		ADD		A,(IX)			// $DD $86 $00	
		ADD		A,(IY)			// $FD $86 $00	
		ADD		(IX)			// $DD $86 $00	
		ADD		(IY)			// $FD $86 $00	
		AND		A,(IX)			// $DD $A6 $00	
		AND		A,(IY)			// $FD $A6 $00	
		AND		(IX)			// $DD $A6 $00	
		AND		(IY)			// $FD $A6 $00	
		BIT		U8,(IX)			// $DD $CB $00 %01[1:B3]110
		BIT		U8,(IY)			// $FD $CB $00 %01[1:B3]110
		CP		A,(IX)			// $DD $BE $00	
		CP		A,(IY)			// $FD $BE $00	
		CP		(IX)			// $DD $BE $00	
		CP		(IY)			// $FD $BE $00	
		DEC		(IX)			// $DD $35 $00	
		DEC		(IY)			// $FD $35 $00	
		INC		(IX)			// $DD $34 $00	
		INC		(IY)			// $FD $34 $00	
		LD		A,(IX)			// $DD $7E $00	
		LD		A,(IY)			// $FD $7E $00	
		LD		B,(IX)			// $DD $46 $00	
		LD		B,(IY)			// $FD $46 $00	
		LD		C,(IX)			// $DD $4E $00	
		LD		C,(IY)			// $FD $4E $00	
		LD		D,(IX)			// $DD $56 $00	
		LD		D,(IY)			// $FD $56 $00	
		LD		E,(IX)			// $DD $5E $00	
		LD		E,(IY)			// $FD $5E $00	
		LD		H,(IX)			// $DD $66 $00	
		LD		H,(IY)			// $FD $66 $00	
		LD		(IX),A			// $DD $77 $00	
		LD		(IX),B			// $DD $70 $00	
		LD		(IX),C			// $DD $71 $00	
		LD		(IX),D			// $DD $72 $00	
		LD		(IX),E			// $DD $73 $00	
		LD		(IX),H			// $DD $74 $00	
		LD		(IX),L			// $DD $75 $00	
		LD		(IX),U8			// $DD $36 $00 [2:U8]
		LD		(IY),A			// $FD $77 $00	
		LD		(IY),B			// $FD $70 $00	
		LD		(IY),C			// $FD $71 $00	
		LD		(IY),D			// $FD $72 $00	
		LD		(IY),E			// $FD $73 $00	
		LD		(IY),H			// $FD $74 $00	
		LD		(IY),L			// $FD $75 $00	
		LD		(IY),U8			// $FD $36 $00 [2:U8]
		LD		L,(IX)			// $DD $6E $00	
		LD		L,(IY)			// $FD $6E $00	
		OR		A,(IX)			// $DD $B6 $00	
		OR		A,(IY)			// $FD $B6 $00	
		OR		(IX)			// $DD $B6 $00	
		OR		(IY)			// $FD $B6 $00	
		RES		U8,(IX)			// $DD $CB $00 %10[1:B3]110
		RES		U8,(IY)			// $FD $CB $00 %10[1:B3]110
		RL		(IX)			// $DD $CB $00 $16
		RL		(IY)			// $FD $CB $00 $16
		RLC		(IX)			// $DD $CB $00 $06
		RLC		(IY)			// $FD $CB $00 $06
		RR		(IX)			// $DD $CB $00 $1E
		RR		(IY)			// $FD $CB $00 $1E
		RRC		(IX)			// $DD $CB $00 $0E
		RRC		(IY)			// $FD $CB $00 $0E
		SBC		A,(IX)			// $DD $9E $00	
		SBC		A,(IY)			// $FD $9E $00	
		SBC		(IX)			// $DD $9E $00	
		SBC		(IY)			// $FD $9E $00	
		SET		U8,(IX)			// $DD $CB $00 %11[1:B3]110
		SET		U8,(IY)			// $FD $CB $00 %11[1:B3]110
		SLA		(IX)			// $DD $CB $00 $26
		SLA		(IY)			// $FD $CB $00 $26
		SRA		(IX)			// $DD $CB $00 $2E
		SRA		(IY)			// $FD $CB $00 $2E
		SRL		(IX)			// $DD $CB $00 $3E
		SRL		(IY)			// $FD $CB $00 $3E
		SUB		A,(IX)			// $DD $96 $00	
		SUB		A,(IY)			// $FD $96 $00	
		SUB		(IX)			// $DD $96 $00	
		SUB		(IY)			// $FD $96 $00	
		XOR		A,(IX)			// $DD $AE $00	
		XOR		A,(IY)			// $FD $AE $00	
		XOR		(IX)			// $DD $AE $00	
		XOR		(IY)			// $FD $AE $00
				


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