//----------------------------------------------------------------------
//
// Automatically generated include file for CPU = 8080
//   Filename:    test_8080.inc
//   Created:     Fri 06-Dec-2024 23:54:35
//
// This file contains all the instructions that the assembler will
// recognise for the chosen CPU.
//
//----------------------------------------------------------------------

; 8080 CPU Arithmetic instructions

   ACI     U8
   ADC     A
   ADC     B
   ADC     C
   ADC     D
   ADC     E
   ADC     H
   ADC     L
   ADC     M
   ADD     A
   ADD     B
   ADD     C
   ADD     D
   ADD     E
   ADD     H
   ADD     L
   ADD     M
   ADI     U8
   CMA     
   DAA     
   DAD     B
   DAD     D
   DAD     H
   DAD     SP
   SBB     A
   SBB     B
   SBB     C
   SBB     D
   SBB     E
   SBB     H
   SBB     L
   SBB     M
   SBI     U8
   SUB     A
   SUB     B
   SUB     C
   SUB     D
   SUB     E
   SUB     H
   SUB     L
   SUB     M
   SUI     U8

; 8080 CPU Compare instructions

   CMP     A
   CMP     B
   CMP     C
   CMP     D
   CMP     E
   CMP     H
   CMP     L
   CMP     M
   CP      U16
   CPI     U8

; 8080 CPU Data move instructions

   LDA     U16
   LDAX    B
   LDAX    D
   LHLD    U16
   LXI     B,U16
   LXI     D,U16
   LXI     H,U16
   LXI     SP,U16
   MOV     A,A
   MOV     A,B
   MOV     A,C
   MOV     A,D
   MOV     A,E
   MOV     A,H
   MOV     A,L
   MOV     A,M
   MOV     B,A
   MOV     B,B
   MOV     B,C
   MOV     B,D
   MOV     B,E
   MOV     B,H
   MOV     B,L
   MOV     B,M
   MOV     C,A
   MOV     C,B
   MOV     C,C
   MOV     C,D
   MOV     C,E
   MOV     C,H
   MOV     C,L
   MOV     C,M
   MOV     D,A
   MOV     D,B
   MOV     D,C
   MOV     D,D
   MOV     D,E
   MOV     D,H
   MOV     D,L
   MOV     D,M
   MOV     E,A
   MOV     E,B
   MOV     E,C
   MOV     E,D
   MOV     E,E
   MOV     E,H
   MOV     E,L
   MOV     E,M
   MOV     H,A
   MOV     H,B
   MOV     H,C
   MOV     H,D
   MOV     H,E
   MOV     H,H
   MOV     H,L
   MOV     H,M
   MOV     L,A
   MOV     L,B
   MOV     L,C
   MOV     L,D
   MOV     L,E
   MOV     L,H
   MOV     L,L
   MOV     L,M
   MOV     M,A
   MOV     M,B
   MOV     M,C
   MOV     M,D
   MOV     M,E
   MOV     M,H
   MOV     M,L
   MVI     A,U8
   MVI     B,U8
   MVI     C,U8
   MVI     D,U8
   MVI     E,U8
   MVI     H,U8
   MVI     L,U8
   MVI     M,U8
   SHLD    U16
   SPHL    
   STA     U16
   STAX    B
   STAX    D
   XCHG    
   XTHL    

; 8080 CPU Flag instructions

   CMC     
   DI      
   EI      

; 8080 CPU Increment / decrement instructions

   DCR     A
   DCR     B
   DCR     C
   DCR     D
   DCR     E
   DCR     H
   DCR     L
   DCR     M
   DCX     B
   DCX     D
   DCX     H
   DCX     SP
   INR     A
   INR     B
   INR     C
   INR     D
   INR     E
   INR     H
   INR     L
   INR     M
   INX     B
   INX     D
   INX     H
   INX     SP

; 8080 CPU Logical instructions

   ANA     A
   ANA     B
   ANA     C
   ANA     D
   ANA     E
   ANA     H
   ANA     L
   ANA     M
   ANI     U8
   ORA     A
   ORA     B
   ORA     C
   ORA     D
   ORA     E
   ORA     H
   ORA     L
   ORA     M
   ORI     U8
   XRA     A
   XRA     B
   XRA     C
   XRA     D
   XRA     E
   XRA     H
   XRA     L
   XRA     M
   XRI     U8

; 8080 CPU Port instructions

   IN      U8
   OUT     U8

; 8080 CPU Program flow instructions

   CALL    U16
   CC      U16
   CM      U16
   CNC     U16
   CNZ     U16
   CPE     U16
   CPO     U16
   CZ      U16
   HLT     
   JC      U16
   JM      U16
   JMP     U16
   JNC     U16
   JNZ     U16
   JP      U16
   JPE     U16
   JPO     U16
   JZ      U16
   PCHL    
   RC      
   RET     
   RM      
   RNC     
   RNZ     
   RP      
   RPE     
   RPO     
   RST     U8
   RZ      

; 8080 CPU Shift / rotate instructions

   RAL     
   RAR     
   RLC     
   RRC     

; 8080 CPU Special instructions

   NOP     
   STC     

; 8080 CPU Stack instructions

   POP     B
   POP     D
   POP     H
   POP     PSW
   PUSH    B
   PUSH    D
   PUSH    H
   PUSH    PSW
