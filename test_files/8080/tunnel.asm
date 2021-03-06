;  ***************************
; * orion-128.2 "TUNNEL" 253B *
; *         VERSION 1         *
; *    BY RIDDLE (C) 2019     *
;  ***************************
ORG 0
START:
MVI A,6
OUT 0F8H ; 16-COLOR MODE
JMP CLS
NOP
SETCLR: ; RST 1 -> HL - ADDRESS, C - COLOR
EI
MVI A,1
CALL 0F839H
DI
RET
HLINE: ; RST 2 -> DE, L, A, B, C
RST 4
DCR A ; A=-1
DCR D
DCR B
JP HLINE
RET
ALIGNR: ; RST 3 -> A - MASK
ORA A
RZ
AL0:
RRC
JNC AL0
RLC
RET
CPIX: ; RST 4 -> DE, L, A, C
MOV H,D
MOV M,A
STAX D
RST 1
XCHG
RST 1
XCHG
RET
HALL3D: ; RST 5 -> B - Y, C - COLOR
PUSH D
PUSH B
LXI D,0D780H ; MIDDLE OF THE SCREEN
MOV A,E
SUB B
MOV L,A ; L - TOP 
MOV A,E
ADD B
MOV E,A ; E - BOTTOM
MVI A,13 ; SOUND WAVE COLOR
CMP C
JZ Y2X2
MOV A,B ; Y -> X
ORA A
RAR
ORA A
RAR
ADD B ; X = Y*TAN(0.896)
H3D3:
MOV B,A
ANI 7
MOV H,A
MOV A,B
RRC
RRC
ANI 3EH
MOV B,A
RAR
ADD D
MOV D,A
INR D
PUSH D ; BOTTOM RIGHT
PUSH H ; TAIL LEN
XRA A
H3D0:
STC
RAR
DCR H
JP H3D0
RAL
PUSH PSW ; TAIL BITMAP
RST 2 ; HLINE
POP PSW  ; TAIL BITMAP
RST 3 ; TAIL -> HEAD BITMAP
RST 4 ; CPIX
POP H ; TAIL LEN
LXI B,8001H
H3D1: ; VERTICAL BITS
MOV A,B
RLC
MOV B,A
MOV A,C
RRC
MOV C,A
DCR H
JP H3D1
POP H   ; HL - BOTTOM RIGHT
MOV E,L ; DE - BOTTOM LEFT
XTHL
MVI A,10 ; VERTICAL ONLY IF LIGHT GREEN
CMP L
MOV A,H
XTHL
JNZ H3DX
RLC
SUI 4
DCR E
DCR L
H3D2:
EI
DCR E
DCR L
MOV M,C
XCHG
MOV M,B
XCHG
DCR A
DI
JNZ H3D2
H3DX:
POP B
POP D
MVI A,13 ; BREAK RECURSION FOR SOUND WAVE
CMP C
RZ
PUSH B
MOV C,A
MOV A,E
RLC
SUI 2
MOV B,A
RST 5
POP B
RET
Y2X2: ; SOUND WAVE SHAPE
MVI A,16
CMD:
DCR A
ANI 1FH
STA Y2X2+1
JNZ H3D3
LDA CMD
XRI 1 ; DCR <-> INR
STA CMD
JMP CMD
CLS:
LXI H,0C000H
CLR:
MVI C,6
MOV A,H
CPI 0DFH
JNC CLR0
CPI 0D1H
JC CLR0
MVI C,14
CLR0:
RST 1
MOV M,A
INX H
MOV A,H
CPI 0F0H
JNZ CLR
LOOP:
MVI E,0CH
MAIN:
MVI B,123
MVI D,0BH
HLN0:
MVI C,10
RST 5
DCR B
MVI C,2
MOV A,E
CMP D
JNZ HLN1
MVI C,11
CPI 4
JNC HLN1
MVI C,15
HLN1:
RST 5
MOV A,B
SUB D
SUI 2
MOV B,A
DCR D
JNZ HLN0
DCR E
JNZ MAIN
LXI H,1800H
HLN2:
EI
MOV A,H
ORA L
DCX H
DI
JNZ HLN2
JMP LOOP
END