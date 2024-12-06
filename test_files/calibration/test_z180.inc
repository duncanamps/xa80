//----------------------------------------------------------------------
//
// Automatically generated include file for CPU = z180
//   Filename:    test_z180.inc
//   Created:     Fri 06-Dec-2024 23:54:36
//
// This file contains all the instructions that the assembler will
// recognise for the chosen CPU.
//
//----------------------------------------------------------------------

; Z180 CPU Arithmetic instructions

   ADC     A
   ADC     A,A
   ADC     A,B
   ADC     A,C
   ADC     A,D
   ADC     A,E
   ADC     A,H
   ADC     A,(HL)
   ADC     A,(IX+DISPL)
   ADC     A,(IX)
   ADC     A,(IY+DISPL)
   ADC     A,(IY)
   ADC     A,L
   ADC     A,U8
   ADC     B
   ADC     C
   ADC     D
   ADC     E
   ADC     H
   ADC     HL,BC
   ADC     HL,DE
   ADC     HL,HL
   ADC     HL,SP
   ADC     (HL)
   ADC     (IX+DISPL)
   ADC     (IX)
   ADC     (IY+DISPL)
   ADC     (IY)
   ADC     L
   ADC     U8
   ADD     A
   ADD     A,A
   ADD     A,B
   ADD     A,C
   ADD     A,D
   ADD     A,E
   ADD     A,H
   ADD     A,(HL)
   ADD     A,(IX+DISPL)
   ADD     A,(IX)
   ADD     A,(IY+DISPL)
   ADD     A,(IY)
   ADD     A,L
   ADD     A,U8
   ADD     B
   ADD     C
   ADD     D
   ADD     E
   ADD     H
   ADD     HL,BC
   ADD     HL,DE
   ADD     HL,HL
   ADD     HL,SP
   ADD     (HL)
   ADD     IX,BC
   ADD     IX,DE
   ADD     IX,IX
   ADD     IX,SP
   ADD     (IX+DISPL)
   ADD     (IX)
   ADD     IY,BC
   ADD     IY,DE
   ADD     IY,IY
   ADD     IY,SP
   ADD     (IY+DISPL)
   ADD     (IY)
   ADD     L
   ADD     U8
   CPL     
   DAA     
   MLT     BC
   MLT     DE
   MLT     HL
   MLT     SP
   NEG     
   SBC     A
   SBC     A,A
   SBC     A,B
   SBC     A,C
   SBC     A,D
   SBC     A,E
   SBC     A,H
   SBC     A,(HL)
   SBC     A,(IX+DISPL)
   SBC     A,(IX)
   SBC     A,(IY+DISPL)
   SBC     A,(IY)
   SBC     A,L
   SBC     A,U8
   SBC     B
   SBC     C
   SBC     D
   SBC     E
   SBC     H
   SBC     HL,BC
   SBC     HL,DE
   SBC     HL,HL
   SBC     HL,SP
   SBC     (HL)
   SBC     (IX+DISPL)
   SBC     (IX)
   SBC     (IY+DISPL)
   SBC     (IY)
   SBC     L
   SBC     U8
   SUB     A
   SUB     A,A
   SUB     A,B
   SUB     A,C
   SUB     A,D
   SUB     A,E
   SUB     A,H
   SUB     A,(HL)
   SUB     A,(IX+DISPL)
   SUB     A,(IX)
   SUB     A,(IY+DISPL)
   SUB     A,(IY)
   SUB     A,L
   SUB     A,U8
   SUB     B
   SUB     C
   SUB     D
   SUB     E
   SUB     H
   SUB     (HL)
   SUB     (IX+DISPL)
   SUB     (IX)
   SUB     (IY+DISPL)
   SUB     (IY)
   SUB     L
   SUB     U8

; Z180 CPU Bit instructions

   BIT     U8,A
   BIT     U8,B
   BIT     U8,C
   BIT     U8,D
   BIT     U8,E
   BIT     U8,H
   BIT     U8,(HL)
   BIT     U8,(IX+DISPL)
   BIT     U8,(IX)
   BIT     U8,(IY+DISPL)
   BIT     U8,(IY)
   BIT     U8,L
   RES     U8,A
   RES     U8,B
   RES     U8,C
   RES     U8,D
   RES     U8,E
   RES     U8,H
   RES     U8,(HL)
   RES     U8,(IX+DISPL)
   RES     U8,(IX)
   RES     U8,(IY+DISPL)
   RES     U8,(IY)
   RES     U8,L
   SET     U8,A
   SET     U8,B
   SET     U8,C
   SET     U8,D
   SET     U8,E
   SET     U8,H
   SET     U8,(HL)
   SET     U8,(IX+DISPL)
   SET     U8,(IX)
   SET     U8,(IY+DISPL)
   SET     U8,(IY)
   SET     U8,L

; Z180 CPU Compare instructions

   CP      A
   CP      A,A
   CP      A,B
   CP      A,C
   CP      A,D
   CP      A,E
   CP      A,H
   CP      A,(HL)
   CP      A,(IX+DISPL)
   CP      A,(IX)
   CP      A,(IY+DISPL)
   CP      A,(IY)
   CP      A,L
   CP      A,U8
   CP      B
   CP      C
   CP      D
   CP      E
   CP      H
   CP      (HL)
   CP      (IX+DISPL)
   CP      (IX)
   CP      (IY+DISPL)
   CP      (IY)
   CP      L
   CP      U8
   CPD     
   CPDR    
   CPI     
   CPIR    

; Z180 CPU Data move instructions

   EX      AF,AF'
   EX      DE,HL
   EX      (SP),HL
   EX      (SP),IX
   EX      (SP),IY
   EXX     
   LD      A,A
   LD      A,B
   LD      A,(BC)
   LD      A,C
   LD      A,D
   LD      A,(DE)
   LD      A,E
   LD      A,H
   LD      A,(HL)
   LD      A,I
   LD      A,(IX+DISPL)
   LD      A,(IX)
   LD      A,(IY+DISPL)
   LD      A,(IY)
   LD      A,L
   LD      A,R
   LD      A,(U16)
   LD      A,U8
   LD      B,A
   LD      B,B
   LD      B,C
   LD      B,D
   LD      B,E
   LD      B,H
   LD      B,(HL)
   LD      B,(IX+DISPL)
   LD      B,(IX)
   LD      B,(IY+DISPL)
   LD      B,(IY)
   LD      B,L
   LD      B,U8
   LD      BC,U16
   LD      BC,(U16)
   LD      (BC),A
   LD      C,A
   LD      C,B
   LD      C,C
   LD      C,D
   LD      C,E
   LD      C,H
   LD      C,(HL)
   LD      C,(IX+DISPL)
   LD      C,(IX)
   LD      C,(IY+DISPL)
   LD      C,(IY)
   LD      C,L
   LD      C,U8
   LD      D,A
   LD      D,B
   LD      D,C
   LD      D,D
   LD      D,E
   LD      D,H
   LD      D,(HL)
   LD      D,(IX+DISPL)
   LD      D,(IX)
   LD      D,(IY+DISPL)
   LD      D,(IY)
   LD      D,L
   LD      D,U8
   LD      DE,U16
   LD      DE,(U16)
   LD      (DE),A
   LD      E,A
   LD      E,B
   LD      E,C
   LD      E,D
   LD      E,E
   LD      E,H
   LD      E,(HL)
   LD      E,(IX+DISPL)
   LD      E,(IX)
   LD      E,(IY+DISPL)
   LD      E,(IY)
   LD      E,L
   LD      E,U8
   LD      H,A
   LD      H,B
   LD      H,C
   LD      H,D
   LD      H,E
   LD      H,H
   LD      H,(HL)
   LD      H,(IX+DISPL)
   LD      H,(IX)
   LD      H,(IY+DISPL)
   LD      H,(IY)
   LD      H,L
   LD      H,U8
   LD      HL,U16
   LD      HL,(U16)
   LD      (HL),A
   LD      (HL),B
   LD      (HL),C
   LD      (HL),D
   LD      (HL),E
   LD      (HL),H
   LD      (HL),L
   LD      (HL),U8
   LD      I,A
   LD      IX,U16
   LD      IX,(U16)
   LD      (IX+DISPL),A
   LD      (IX+DISPL),B
   LD      (IX+DISPL),C
   LD      (IX+DISPL),D
   LD      (IX+DISPL),E
   LD      (IX+DISPL),H
   LD      (IX+DISPL),L
   LD      (IX+DISPL),U8
   LD      (IX),A
   LD      (IX),B
   LD      (IX),C
   LD      (IX),D
   LD      (IX),E
   LD      (IX),H
   LD      (IX),L
   LD      (IX),U8
   LD      IY,U16
   LD      IY,(U16)
   LD      (IY+DISPL),A
   LD      (IY+DISPL),B
   LD      (IY+DISPL),C
   LD      (IY+DISPL),D
   LD      (IY+DISPL),E
   LD      (IY+DISPL),H
   LD      (IY+DISPL),L
   LD      (IY+DISPL),U8
   LD      (IY),A
   LD      (IY),B
   LD      (IY),C
   LD      (IY),D
   LD      (IY),E
   LD      (IY),H
   LD      (IY),L
   LD      (IY),U8
   LD      L,A
   LD      L,B
   LD      L,C
   LD      L,D
   LD      L,E
   LD      L,H
   LD      L,(HL)
   LD      L,(IX+DISPL)
   LD      L,(IX)
   LD      L,(IY+DISPL)
   LD      L,(IY)
   LD      L,L
   LD      L,U8
   LD      R,A
   LD      SP,HL
   LD      SP,IX
   LD      SP,IY
   LD      SP,U16
   LD      SP,(U16)
   LD      (U16),A
   LD      (U16),BC
   LD      (U16),DE
   LD      (U16),HL
   LD      (U16),IX
   LD      (U16),IY
   LD      (U16),SP
   LDD     
   LDDR    
   LDI     
   LDIR    

; Z180 CPU Flag instructions

   CCF     
   DI      
   EI      
   SCF     

; Z180 CPU Increment / decrement instructions

   DEC     A
   DEC     B
   DEC     BC
   DEC     C
   DEC     D
   DEC     DE
   DEC     E
   DEC     H
   DEC     HL
   DEC     (HL)
   DEC     IX
   DEC     (IX+DISPL)
   DEC     (IX)
   DEC     IY
   DEC     (IY+DISPL)
   DEC     (IY)
   DEC     L
   DEC     SP
   INC     A
   INC     B
   INC     BC
   INC     C
   INC     D
   INC     DE
   INC     E
   INC     H
   INC     HL
   INC     (HL)
   INC     IX
   INC     (IX+DISPL)
   INC     (IX)
   INC     IY
   INC     (IY+DISPL)
   INC     (IY)
   INC     L
   INC     SP

; Z180 CPU Logical instructions

   AND     A
   AND     A,A
   AND     A,B
   AND     A,C
   AND     A,D
   AND     A,E
   AND     A,H
   AND     A,(HL)
   AND     A,(IX+DISPL)
   AND     A,(IX)
   AND     A,(IY+DISPL)
   AND     A,(IY)
   AND     A,L
   AND     A,U8
   AND     B
   AND     C
   AND     D
   AND     E
   AND     H
   AND     (HL)
   AND     (IX+DISPL)
   AND     (IX)
   AND     (IY+DISPL)
   AND     (IY)
   AND     L
   AND     U8
   OR      A
   OR      A,A
   OR      A,B
   OR      A,C
   OR      A,D
   OR      A,E
   OR      A,H
   OR      A,(HL)
   OR      A,(IX+DISPL)
   OR      A,(IX)
   OR      A,(IY+DISPL)
   OR      A,(IY)
   OR      A,L
   OR      A,U8
   OR      B
   OR      C
   OR      D
   OR      E
   OR      H
   OR      (HL)
   OR      (IX+DISPL)
   OR      (IX)
   OR      (IY+DISPL)
   OR      (IY)
   OR      L
   OR      U8
   TST     A
   TST     B
   TST     C
   TST     D
   TST     E
   TST     H
   TST     (HL)
   TST     L
   TST     U8
   TSTIO   U8
   XOR     A
   XOR     A,A
   XOR     A,B
   XOR     A,C
   XOR     A,D
   XOR     A,E
   XOR     A,H
   XOR     A,(HL)
   XOR     A,(IX+DISPL)
   XOR     A,(IX)
   XOR     A,(IY+DISPL)
   XOR     A,(IY)
   XOR     A,L
   XOR     A,U8
   XOR     B
   XOR     C
   XOR     D
   XOR     E
   XOR     H
   XOR     (HL)
   XOR     (IX+DISPL)
   XOR     (IX)
   XOR     (IY+DISPL)
   XOR     (IY)
   XOR     L
   XOR     U8

; Z180 CPU Port instructions

   IN      A,(C)
   IN      A,(U8)
   IN      B,(C)
   IN      C,(C)
   IN      D,(C)
   IN      E,(C)
   IN      H,(C)
   IN      L,(C)
   IN0     A,(U8)
   IN0     B,(U8)
   IN0     C,(U8)
   IN0     D,(U8)
   IN0     E,(U8)
   IN0     H,(U8)
   IN0     (HL),(U8)
   IN0     L,(U8)
   IND     
   INDR    
   INI     
   INIR    
   OTDM    
   OTDMR   
   OTDR    
   OTIM    
   OTIMR   
   OTIR    
   OUT     (C),A
   OUT     (C),B
   OUT     (C),C
   OUT     (C),D
   OUT     (C),E
   OUT     (C),H
   OUT     (C),L
   OUT     (U8),A
   OUT0    (U8),A
   OUT0    (U8),B
   OUT0    (U8),C
   OUT0    (U8),D
   OUT0    (U8),E
   OUT0    (U8),H
   OUT0    (U8),(HL)
   OUT0    (U8),L
   OUTD    
   OUTI    

; Z180 CPU Program flow instructions

   CALL    C,U16
   CALL    M,U16
   CALL    NC,U16
   CALL    NZ,U16
   CALL    P,U16
   CALL    PE,U16
   CALL    PO,U16
   CALL    U16
   CALL    Z,U16
   DJNZ    $+123
   HALT    
   JP      C,U16
   JP      (HL)
   JP      (IX)
   JP      (IY)
   JP      M,U16
   JP      NC,U16
   JP      NZ,U16
   JP      P,U16
   JP      PE,U16
   JP      PO,U16
   JP      U16
   JP      Z,U16
   JR      C,$+123
   JR      NC,$+123
   JR      NZ,$+123
   JR      $+123
   JR      Z,$+123
   RET     
   RET     C
   RET     M
   RET     NC
   RET     NZ
   RET     P
   RET     PE
   RET     PO
   RET     Z
   RETI    
   RETN    
   RST     U8
   SLP     

; Z180 CPU Shift / rotate instructions

   RL      A
   RL      B
   RL      C
   RL      D
   RL      E
   RL      H
   RL      (HL)
   RL      (IX+DISPL)
   RL      (IX)
   RL      (IY+DISPL)
   RL      (IY)
   RL      L
   RLA     
   RLC     A
   RLC     B
   RLC     C
   RLC     D
   RLC     E
   RLC     H
   RLC     (HL)
   RLC     (IX+DISPL)
   RLC     (IX)
   RLC     (IY+DISPL)
   RLC     (IY)
   RLC     L
   RLCA    
   RLD     
   RR      A
   RR      B
   RR      C
   RR      D
   RR      E
   RR      H
   RR      (HL)
   RR      (IX+DISPL)
   RR      (IX)
   RR      (IY+DISPL)
   RR      (IY)
   RR      L
   RRA     
   RRC     A
   RRC     B
   RRC     C
   RRC     D
   RRC     E
   RRC     H
   RRC     (HL)
   RRC     (IX+DISPL)
   RRC     (IX)
   RRC     (IY+DISPL)
   RRC     (IY)
   RRC     L
   RRCA    
   RRD     
   SLA     A
   SLA     B
   SLA     C
   SLA     D
   SLA     E
   SLA     H
   SLA     (HL)
   SLA     (IX+DISPL)
   SLA     (IX)
   SLA     (IY+DISPL)
   SLA     (IY)
   SLA     L
   SRA     A
   SRA     B
   SRA     C
   SRA     D
   SRA     E
   SRA     H
   SRA     (HL)
   SRA     (IX+DISPL)
   SRA     (IX)
   SRA     (IY+DISPL)
   SRA     (IY)
   SRA     L
   SRL     A
   SRL     B
   SRL     C
   SRL     D
   SRL     E
   SRL     H
   SRL     (HL)
   SRL     (IX+DISPL)
   SRL     (IX)
   SRL     (IY+DISPL)
   SRL     (IY)
   SRL     L

; Z180 CPU Special instructions

   IM      U8
   NOP     

; Z180 CPU Stack instructions

   POP     AF
   POP     BC
   POP     DE
   POP     HL
   POP     IX
   POP     IY
   PUSH    AF
   PUSH    BC
   PUSH    DE
   PUSH    HL
   PUSH    IX
   PUSH    IY
