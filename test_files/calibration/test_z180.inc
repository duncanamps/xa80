//----------------------------------------------------------------------
//
// Automatically generated include file for CPU = z180
//   Filename:    test_z180.inc
//   Created:     Sat 07-Dec-2024 12:12:03
//
// This file contains all the instructions that the assembler will
// recognise for the chosen CPU.
//
//----------------------------------------------------------------------

; Z180 CPU Arithmetic instructions

   ADC     A                  // $8F                          ; Alternative version of ADC A,A
   ADC     A,A                // $8F
   ADC     A,B                // $88
   ADC     A,C                // $89
   ADC     A,D                // $8A
   ADC     A,E                // $8B
   ADC     A,H                // $8C
   ADC     A,(HL)             // $8E
   ADC     A,(IX+DISPL)       // $DD $8E [2:S8]
   ADC     A,(IX)             // $DD $8E $00                  ; (IX) --> (IX+0)
   ADC     A,(IY+DISPL)       // $FD $8E [2:S8]
   ADC     A,(IY)             // $FD $8E $00                  ; (IY) --> (IY+0)
   ADC     A,L                // $8D
   ADC     A,U8               // $CE [2:U8]
   ADC     B                  // $88                          ; Alternative version of ADC A,B
   ADC     C                  // $89                          ; Alternative version of ADC A,C
   ADC     D                  // $8A                          ; Alternative version of ADC A,D
   ADC     E                  // $8B                          ; Alternative version of ADC A,E
   ADC     H                  // $8C                          ; Alternative version of ADC A,H
   ADC     HL,BC              // $ED $4A
   ADC     HL,DE              // $ED $5A
   ADC     HL,HL              // $ED $6A
   ADC     HL,SP              // $ED $7A
   ADC     (HL)               // $8E                          ; Alternative version of ADC A,HL_IND
   ADC     (IX+DISPL)         // $DD $8E [1:S8]               ; Alternative version of ADC A,IXPD_IND
   ADC     (IX)               // $DD $8E $00                  ; (IX) --> (IX+0), alternative form of ADC A,(IX+0)
   ADC     (IY+DISPL)         // $FD $8E [1:S8]               ; Alternative version of ADC A,IYPD_IND
   ADC     (IY)               // $FD $8E $00                  ; (IY) --> (IY+0), alternative form of ADC A,(IY+0)
   ADC     L                  // $8D                          ; Alternative version of ADC A,L
   ADC     U8                 // $CE [1:U8]                   ; Alternative version of ADC A,U8
   ADD     A                  // $87                          ; Alternative version of ADD A,A
   ADD     A,A                // $87
   ADD     A,B                // $80
   ADD     A,C                // $81
   ADD     A,D                // $82
   ADD     A,E                // $83
   ADD     A,H                // $84
   ADD     A,(HL)             // $86
   ADD     A,(IX+DISPL)       // $DD $86 [2:S8]
   ADD     A,(IX)             // $DD $86 $00                  ; (IX) --> (IX+0)
   ADD     A,(IY+DISPL)       // $FD $86 [2:S8]
   ADD     A,(IY)             // $FD $86 $00                  ; (IY) --> (IY+0)
   ADD     A,L                // $85
   ADD     A,U8               // $C6 [2:U8]
   ADD     B                  // $80                          ; Alternative version of ADD A,B
   ADD     C                  // $81                          ; Alternative version of ADD A,C
   ADD     D                  // $82                          ; Alternative version of ADD A,D
   ADD     E                  // $83                          ; Alternative version of ADD A,E
   ADD     H                  // $84                          ; Alternative version of ADD A,H
   ADD     HL,BC              // $09
   ADD     HL,DE              // $19
   ADD     HL,HL              // $29
   ADD     HL,SP              // $39
   ADD     (HL)               // $86                          ; Alternative version of ADD A,HL_IND
   ADD     IX,BC              // $DD $09
   ADD     IX,DE              // $DD $19
   ADD     IX,IX              // $DD $29
   ADD     IX,SP              // $DD $39
   ADD     (IX+DISPL)         // $DD $86 [1:S8]               ; Alternative version of ADD A,IXPD_IND
   ADD     (IX)               // $DD $86 $00                  ; (IX) --> (IX+0), alternative form of ADD A,(IX+0)
   ADD     IY,BC              // $FD $09
   ADD     IY,DE              // $FD $19
   ADD     IY,IY              // $FD $29
   ADD     IY,SP              // $FD $39
   ADD     (IY+DISPL)         // $FD $86 [1:S8]               ; Alternative version of ADD A,IYPD_IND
   ADD     (IY)               // $FD $86 $00                  ; (IY) --> (IY+0), alternative form of ADD A,(IY+0)
   ADD     L                  // $85                          ; Alternative version of ADD A,L
   ADD     U8                 // $C6 [1:U8]                   ; Alternative version of ADD A,U8
   CPL                        // $2F
   DAA                        // $27
   MLT     BC                 // $ED $4C
   MLT     DE                 // $ED $5C
   MLT     HL                 // $ED $6C
   MLT     SP                 // $ED $7C
   NEG                        // $ED $44
   SBC     A                  // $9F                          ; Alternative version of SBC A,A
   SBC     A,A                // $9F
   SBC     A,B                // $98
   SBC     A,C                // $99
   SBC     A,D                // $9A
   SBC     A,E                // $9B
   SBC     A,H                // $9C
   SBC     A,(HL)             // $9E
   SBC     A,(IX+DISPL)       // $DD $9E [2:S8]
   SBC     A,(IX)             // $DD $9E $00                  ; (IX) --> (IX+0)
   SBC     A,(IY+DISPL)       // $FD $9E [2:S8]
   SBC     A,(IY)             // $FD $9E $00                  ; (IY) --> (IY+0)
   SBC     A,L                // $9D
   SBC     A,U8               // $DE [2:U8]
   SBC     B                  // $98                          ; Alternative version of SBC A,B
   SBC     C                  // $99                          ; Alternative version of SBC A,C
   SBC     D                  // $9A                          ; Alternative version of SBC A,D
   SBC     E                  // $9B                          ; Alternative version of SBC A,E
   SBC     H                  // $9C                          ; Alternative version of SBC A,H
   SBC     HL,BC              // $ED $42
   SBC     HL,DE              // $ED $52
   SBC     HL,HL              // $ED $62
   SBC     HL,SP              // $ED $72
   SBC     (HL)               // $9E                          ; Alternative version of SBC A,HL_IND
   SBC     (IX+DISPL)         // $DD $9E [1:S8]               ; Alternative version of SBC A,IXPD_IND
   SBC     (IX)               // $DD $9E $00                  ; (IX) --> (IX+0), alternative form of SBC A,(IX+0)
   SBC     (IY+DISPL)         // $FD $9E [1:S8]               ; Alternative version of SBC A,IYPD_IND
   SBC     (IY)               // $FD $9E $00                  ; (IY) --> (IY+0), alternative form of SBC A,(IY+0)
   SBC     L                  // $9D                          ; Alternative version of SBC A,L
   SBC     U8                 // $DE [1:U8]                   ; Alternative version of SBC A,U8
   SUB     A                  // $97                          ; Alternative version of SUB A,A
   SUB     A,A                // $97
   SUB     A,B                // $90
   SUB     A,C                // $91
   SUB     A,D                // $92
   SUB     A,E                // $93
   SUB     A,H                // $94
   SUB     A,(HL)             // $96
   SUB     A,(IX+DISPL)       // $DD $96 [2:S8]
   SUB     A,(IX)             // $DD $96 $00                  ; (IX) --> (IX+0)
   SUB     A,(IY+DISPL)       // $FD $96 [2:S8]
   SUB     A,(IY)             // $FD $96 $00                  ; (IY) --> (IY+0)
   SUB     A,L                // $95
   SUB     A,U8               // $D6 [2:U8]
   SUB     B                  // $90                          ; Alternative version of SUB A,B
   SUB     C                  // $91                          ; Alternative version of SUB A,C
   SUB     D                  // $92                          ; Alternative version of SUB A,D
   SUB     E                  // $93                          ; Alternative version of SUB A,E
   SUB     H                  // $94                          ; Alternative version of SUB A,H
   SUB     (HL)               // $96                          ; Alternative version of SUB A,HL_IND
   SUB     (IX+DISPL)         // $DD $96 [1:S8]               ; Alternative version of SUB A,IXPD_IND
   SUB     (IX)               // $DD $96 $00                  ; (IX) --> (IX+0), alternative form of SUB A,(IX+0)
   SUB     (IY+DISPL)         // $FD $96 [1:S8]               ; Alternative version of SUB A,IYPD_IND
   SUB     (IY)               // $FD $96 $00                  ; (IY) --> (IY+0), alternative form of SUB A,(IY+0)
   SUB     L                  // $95                          ; Alternative version of SUB A,L
   SUB     U8                 // $D6 [1:U8]                   ; Alternative version of SUB A,U8

; Z180 CPU Bit instructions

   BIT     0,A                // $CB %01000111($47)
   BIT     1,A                // $CB %01001111($4F)
   BIT     2,A                // $CB %01010111($57)
   BIT     3,A                // $CB %01011111($5F)
   BIT     4,A                // $CB %01100111($67)
   BIT     5,A                // $CB %01101111($6F)
   BIT     6,A                // $CB %01110111($77)
   BIT     7,A                // $CB %01111111($7F)
   BIT     0,B                // $CB %01000000($40)
   BIT     1,B                // $CB %01001000($48)
   BIT     2,B                // $CB %01010000($50)
   BIT     3,B                // $CB %01011000($58)
   BIT     4,B                // $CB %01100000($60)
   BIT     5,B                // $CB %01101000($68)
   BIT     6,B                // $CB %01110000($70)
   BIT     7,B                // $CB %01111000($78)
   BIT     0,C                // $CB %01000001($41)
   BIT     1,C                // $CB %01001001($49)
   BIT     2,C                // $CB %01010001($51)
   BIT     3,C                // $CB %01011001($59)
   BIT     4,C                // $CB %01100001($61)
   BIT     5,C                // $CB %01101001($69)
   BIT     6,C                // $CB %01110001($71)
   BIT     7,C                // $CB %01111001($79)
   BIT     0,D                // $CB %01000010($42)
   BIT     1,D                // $CB %01001010($4A)
   BIT     2,D                // $CB %01010010($52)
   BIT     3,D                // $CB %01011010($5A)
   BIT     4,D                // $CB %01100010($62)
   BIT     5,D                // $CB %01101010($6A)
   BIT     6,D                // $CB %01110010($72)
   BIT     7,D                // $CB %01111010($7A)
   BIT     0,E                // $CB %01000011($43)
   BIT     1,E                // $CB %01001011($4B)
   BIT     2,E                // $CB %01010011($53)
   BIT     3,E                // $CB %01011011($5B)
   BIT     4,E                // $CB %01100011($63)
   BIT     5,E                // $CB %01101011($6B)
   BIT     6,E                // $CB %01110011($73)
   BIT     7,E                // $CB %01111011($7B)
   BIT     0,H                // $CB %01000100($44)
   BIT     1,H                // $CB %01001100($4C)
   BIT     2,H                // $CB %01010100($54)
   BIT     3,H                // $CB %01011100($5C)
   BIT     4,H                // $CB %01100100($64)
   BIT     5,H                // $CB %01101100($6C)
   BIT     6,H                // $CB %01110100($74)
   BIT     7,H                // $CB %01111100($7C)
   BIT     0,(HL)             // $CB %01000110($46)
   BIT     1,(HL)             // $CB %01001110($4E)
   BIT     2,(HL)             // $CB %01010110($56)
   BIT     3,(HL)             // $CB %01011110($5E)
   BIT     4,(HL)             // $CB %01100110($66)
   BIT     5,(HL)             // $CB %01101110($6E)
   BIT     6,(HL)             // $CB %01110110($76)
   BIT     7,(HL)             // $CB %01111110($7E)
   BIT     0,(IX+DISPL)       // $DD $CB [2:S8] %01000110($46)
   BIT     1,(IX+DISPL)       // $DD $CB [2:S8] %01001110($4E)
   BIT     2,(IX+DISPL)       // $DD $CB [2:S8] %01010110($56)
   BIT     3,(IX+DISPL)       // $DD $CB [2:S8] %01011110($5E)
   BIT     4,(IX+DISPL)       // $DD $CB [2:S8] %01100110($66)
   BIT     5,(IX+DISPL)       // $DD $CB [2:S8] %01101110($6E)
   BIT     6,(IX+DISPL)       // $DD $CB [2:S8] %01110110($76)
   BIT     7,(IX+DISPL)       // $DD $CB [2:S8] %01111110($7E)
   BIT     0,(IX)             // $DD $CB $00 %01000110($46)   ; (IX) --> (IX+0)
   BIT     1,(IX)             // $DD $CB $00 %01001110($4E)   ; (IX) --> (IX+0)
   BIT     2,(IX)             // $DD $CB $00 %01010110($56)   ; (IX) --> (IX+0)
   BIT     3,(IX)             // $DD $CB $00 %01011110($5E)   ; (IX) --> (IX+0)
   BIT     4,(IX)             // $DD $CB $00 %01100110($66)   ; (IX) --> (IX+0)
   BIT     5,(IX)             // $DD $CB $00 %01101110($6E)   ; (IX) --> (IX+0)
   BIT     6,(IX)             // $DD $CB $00 %01110110($76)   ; (IX) --> (IX+0)
   BIT     7,(IX)             // $DD $CB $00 %01111110($7E)   ; (IX) --> (IX+0)
   BIT     0,(IY+DISPL)       // $FD $CB [2:S8] %01000110($46)
   BIT     1,(IY+DISPL)       // $FD $CB [2:S8] %01001110($4E)
   BIT     2,(IY+DISPL)       // $FD $CB [2:S8] %01010110($56)
   BIT     3,(IY+DISPL)       // $FD $CB [2:S8] %01011110($5E)
   BIT     4,(IY+DISPL)       // $FD $CB [2:S8] %01100110($66)
   BIT     5,(IY+DISPL)       // $FD $CB [2:S8] %01101110($6E)
   BIT     6,(IY+DISPL)       // $FD $CB [2:S8] %01110110($76)
   BIT     7,(IY+DISPL)       // $FD $CB [2:S8] %01111110($7E)
   BIT     0,(IY)             // $FD $CB $00 %01000110($46)   ; (IY) --> (IY+0)
   BIT     1,(IY)             // $FD $CB $00 %01001110($4E)   ; (IY) --> (IY+0)
   BIT     2,(IY)             // $FD $CB $00 %01010110($56)   ; (IY) --> (IY+0)
   BIT     3,(IY)             // $FD $CB $00 %01011110($5E)   ; (IY) --> (IY+0)
   BIT     4,(IY)             // $FD $CB $00 %01100110($66)   ; (IY) --> (IY+0)
   BIT     5,(IY)             // $FD $CB $00 %01101110($6E)   ; (IY) --> (IY+0)
   BIT     6,(IY)             // $FD $CB $00 %01110110($76)   ; (IY) --> (IY+0)
   BIT     7,(IY)             // $FD $CB $00 %01111110($7E)   ; (IY) --> (IY+0)
   BIT     0,L                // $CB %01000101($45)
   BIT     1,L                // $CB %01001101($4D)
   BIT     2,L                // $CB %01010101($55)
   BIT     3,L                // $CB %01011101($5D)
   BIT     4,L                // $CB %01100101($65)
   BIT     5,L                // $CB %01101101($6D)
   BIT     6,L                // $CB %01110101($75)
   BIT     7,L                // $CB %01111101($7D)
   RES     0,A                // $CB %10000111($87)
   RES     1,A                // $CB %10001111($8F)
   RES     2,A                // $CB %10010111($97)
   RES     3,A                // $CB %10011111($9F)
   RES     4,A                // $CB %10100111($A7)
   RES     5,A                // $CB %10101111($AF)
   RES     6,A                // $CB %10110111($B7)
   RES     7,A                // $CB %10111111($BF)
   RES     0,B                // $CB %10000000($80)
   RES     1,B                // $CB %10001000($88)
   RES     2,B                // $CB %10010000($90)
   RES     3,B                // $CB %10011000($98)
   RES     4,B                // $CB %10100000($A0)
   RES     5,B                // $CB %10101000($A8)
   RES     6,B                // $CB %10110000($B0)
   RES     7,B                // $CB %10111000($B8)
   RES     0,C                // $CB %10000001($81)
   RES     1,C                // $CB %10001001($89)
   RES     2,C                // $CB %10010001($91)
   RES     3,C                // $CB %10011001($99)
   RES     4,C                // $CB %10100001($A1)
   RES     5,C                // $CB %10101001($A9)
   RES     6,C                // $CB %10110001($B1)
   RES     7,C                // $CB %10111001($B9)
   RES     0,D                // $CB %10000010($82)
   RES     1,D                // $CB %10001010($8A)
   RES     2,D                // $CB %10010010($92)
   RES     3,D                // $CB %10011010($9A)
   RES     4,D                // $CB %10100010($A2)
   RES     5,D                // $CB %10101010($AA)
   RES     6,D                // $CB %10110010($B2)
   RES     7,D                // $CB %10111010($BA)
   RES     0,E                // $CB %10000011($83)
   RES     1,E                // $CB %10001011($8B)
   RES     2,E                // $CB %10010011($93)
   RES     3,E                // $CB %10011011($9B)
   RES     4,E                // $CB %10100011($A3)
   RES     5,E                // $CB %10101011($AB)
   RES     6,E                // $CB %10110011($B3)
   RES     7,E                // $CB %10111011($BB)
   RES     0,H                // $CB %10000100($84)
   RES     1,H                // $CB %10001100($8C)
   RES     2,H                // $CB %10010100($94)
   RES     3,H                // $CB %10011100($9C)
   RES     4,H                // $CB %10100100($A4)
   RES     5,H                // $CB %10101100($AC)
   RES     6,H                // $CB %10110100($B4)
   RES     7,H                // $CB %10111100($BC)
   RES     0,(HL)             // $CB %10000110($86)
   RES     1,(HL)             // $CB %10001110($8E)
   RES     2,(HL)             // $CB %10010110($96)
   RES     3,(HL)             // $CB %10011110($9E)
   RES     4,(HL)             // $CB %10100110($A6)
   RES     5,(HL)             // $CB %10101110($AE)
   RES     6,(HL)             // $CB %10110110($B6)
   RES     7,(HL)             // $CB %10111110($BE)
   RES     0,(IX+DISPL)       // $DD $CB [2:S8] %10000110($86)
   RES     1,(IX+DISPL)       // $DD $CB [2:S8] %10001110($8E)
   RES     2,(IX+DISPL)       // $DD $CB [2:S8] %10010110($96)
   RES     3,(IX+DISPL)       // $DD $CB [2:S8] %10011110($9E)
   RES     4,(IX+DISPL)       // $DD $CB [2:S8] %10100110($A6)
   RES     5,(IX+DISPL)       // $DD $CB [2:S8] %10101110($AE)
   RES     6,(IX+DISPL)       // $DD $CB [2:S8] %10110110($B6)
   RES     7,(IX+DISPL)       // $DD $CB [2:S8] %10111110($BE)
   RES     0,(IX)             // $DD $CB $00 %10000110($86)   ; (IX) --> (IX+0)
   RES     1,(IX)             // $DD $CB $00 %10001110($8E)   ; (IX) --> (IX+0)
   RES     2,(IX)             // $DD $CB $00 %10010110($96)   ; (IX) --> (IX+0)
   RES     3,(IX)             // $DD $CB $00 %10011110($9E)   ; (IX) --> (IX+0)
   RES     4,(IX)             // $DD $CB $00 %10100110($A6)   ; (IX) --> (IX+0)
   RES     5,(IX)             // $DD $CB $00 %10101110($AE)   ; (IX) --> (IX+0)
   RES     6,(IX)             // $DD $CB $00 %10110110($B6)   ; (IX) --> (IX+0)
   RES     7,(IX)             // $DD $CB $00 %10111110($BE)   ; (IX) --> (IX+0)
   RES     0,(IY+DISPL)       // $FD $CB [2:S8] %10000110($86)
   RES     1,(IY+DISPL)       // $FD $CB [2:S8] %10001110($8E)
   RES     2,(IY+DISPL)       // $FD $CB [2:S8] %10010110($96)
   RES     3,(IY+DISPL)       // $FD $CB [2:S8] %10011110($9E)
   RES     4,(IY+DISPL)       // $FD $CB [2:S8] %10100110($A6)
   RES     5,(IY+DISPL)       // $FD $CB [2:S8] %10101110($AE)
   RES     6,(IY+DISPL)       // $FD $CB [2:S8] %10110110($B6)
   RES     7,(IY+DISPL)       // $FD $CB [2:S8] %10111110($BE)
   RES     0,(IY)             // $FD $CB $00 %10000110($86)   ; (IY) --> (IY+0)
   RES     1,(IY)             // $FD $CB $00 %10001110($8E)   ; (IY) --> (IY+0)
   RES     2,(IY)             // $FD $CB $00 %10010110($96)   ; (IY) --> (IY+0)
   RES     3,(IY)             // $FD $CB $00 %10011110($9E)   ; (IY) --> (IY+0)
   RES     4,(IY)             // $FD $CB $00 %10100110($A6)   ; (IY) --> (IY+0)
   RES     5,(IY)             // $FD $CB $00 %10101110($AE)   ; (IY) --> (IY+0)
   RES     6,(IY)             // $FD $CB $00 %10110110($B6)   ; (IY) --> (IY+0)
   RES     7,(IY)             // $FD $CB $00 %10111110($BE)   ; (IY) --> (IY+0)
   RES     0,L                // $CB %10000101($85)
   RES     1,L                // $CB %10001101($8D)
   RES     2,L                // $CB %10010101($95)
   RES     3,L                // $CB %10011101($9D)
   RES     4,L                // $CB %10100101($A5)
   RES     5,L                // $CB %10101101($AD)
   RES     6,L                // $CB %10110101($B5)
   RES     7,L                // $CB %10111101($BD)
   SET     0,A                // $CB %11000111($C7)
   SET     1,A                // $CB %11001111($CF)
   SET     2,A                // $CB %11010111($D7)
   SET     3,A                // $CB %11011111($DF)
   SET     4,A                // $CB %11100111($E7)
   SET     5,A                // $CB %11101111($EF)
   SET     6,A                // $CB %11110111($F7)
   SET     7,A                // $CB %11111111($FF)
   SET     0,B                // $CB %11000000($C0)
   SET     1,B                // $CB %11001000($C8)
   SET     2,B                // $CB %11010000($D0)
   SET     3,B                // $CB %11011000($D8)
   SET     4,B                // $CB %11100000($E0)
   SET     5,B                // $CB %11101000($E8)
   SET     6,B                // $CB %11110000($F0)
   SET     7,B                // $CB %11111000($F8)
   SET     0,C                // $CB %11000001($C1)
   SET     1,C                // $CB %11001001($C9)
   SET     2,C                // $CB %11010001($D1)
   SET     3,C                // $CB %11011001($D9)
   SET     4,C                // $CB %11100001($E1)
   SET     5,C                // $CB %11101001($E9)
   SET     6,C                // $CB %11110001($F1)
   SET     7,C                // $CB %11111001($F9)
   SET     0,D                // $CB %11000010($C2)
   SET     1,D                // $CB %11001010($CA)
   SET     2,D                // $CB %11010010($D2)
   SET     3,D                // $CB %11011010($DA)
   SET     4,D                // $CB %11100010($E2)
   SET     5,D                // $CB %11101010($EA)
   SET     6,D                // $CB %11110010($F2)
   SET     7,D                // $CB %11111010($FA)
   SET     0,E                // $CB %11000011($C3)
   SET     1,E                // $CB %11001011($CB)
   SET     2,E                // $CB %11010011($D3)
   SET     3,E                // $CB %11011011($DB)
   SET     4,E                // $CB %11100011($E3)
   SET     5,E                // $CB %11101011($EB)
   SET     6,E                // $CB %11110011($F3)
   SET     7,E                // $CB %11111011($FB)
   SET     0,H                // $CB %11000100($C4)
   SET     1,H                // $CB %11001100($CC)
   SET     2,H                // $CB %11010100($D4)
   SET     3,H                // $CB %11011100($DC)
   SET     4,H                // $CB %11100100($E4)
   SET     5,H                // $CB %11101100($EC)
   SET     6,H                // $CB %11110100($F4)
   SET     7,H                // $CB %11111100($FC)
   SET     0,(HL)             // $CB %11000110($C6)
   SET     1,(HL)             // $CB %11001110($CE)
   SET     2,(HL)             // $CB %11010110($D6)
   SET     3,(HL)             // $CB %11011110($DE)
   SET     4,(HL)             // $CB %11100110($E6)
   SET     5,(HL)             // $CB %11101110($EE)
   SET     6,(HL)             // $CB %11110110($F6)
   SET     7,(HL)             // $CB %11111110($FE)
   SET     0,(IX+DISPL)       // $DD $CB [2:S8] %11000110($C6)
   SET     1,(IX+DISPL)       // $DD $CB [2:S8] %11001110($CE)
   SET     2,(IX+DISPL)       // $DD $CB [2:S8] %11010110($D6)
   SET     3,(IX+DISPL)       // $DD $CB [2:S8] %11011110($DE)
   SET     4,(IX+DISPL)       // $DD $CB [2:S8] %11100110($E6)
   SET     5,(IX+DISPL)       // $DD $CB [2:S8] %11101110($EE)
   SET     6,(IX+DISPL)       // $DD $CB [2:S8] %11110110($F6)
   SET     7,(IX+DISPL)       // $DD $CB [2:S8] %11111110($FE)
   SET     0,(IX)             // $DD $CB $00 %11000110($C6)   ; (IX) --> (IX+0)
   SET     1,(IX)             // $DD $CB $00 %11001110($CE)   ; (IX) --> (IX+0)
   SET     2,(IX)             // $DD $CB $00 %11010110($D6)   ; (IX) --> (IX+0)
   SET     3,(IX)             // $DD $CB $00 %11011110($DE)   ; (IX) --> (IX+0)
   SET     4,(IX)             // $DD $CB $00 %11100110($E6)   ; (IX) --> (IX+0)
   SET     5,(IX)             // $DD $CB $00 %11101110($EE)   ; (IX) --> (IX+0)
   SET     6,(IX)             // $DD $CB $00 %11110110($F6)   ; (IX) --> (IX+0)
   SET     7,(IX)             // $DD $CB $00 %11111110($FE)   ; (IX) --> (IX+0)
   SET     0,(IY+DISPL)       // $FD $CB [2:S8] %11000110($C6)
   SET     1,(IY+DISPL)       // $FD $CB [2:S8] %11001110($CE)
   SET     2,(IY+DISPL)       // $FD $CB [2:S8] %11010110($D6)
   SET     3,(IY+DISPL)       // $FD $CB [2:S8] %11011110($DE)
   SET     4,(IY+DISPL)       // $FD $CB [2:S8] %11100110($E6)
   SET     5,(IY+DISPL)       // $FD $CB [2:S8] %11101110($EE)
   SET     6,(IY+DISPL)       // $FD $CB [2:S8] %11110110($F6)
   SET     7,(IY+DISPL)       // $FD $CB [2:S8] %11111110($FE)
   SET     0,(IY)             // $FD $CB $00 %11000110($C6)   ; (IY) --> (IY+0)
   SET     1,(IY)             // $FD $CB $00 %11001110($CE)   ; (IY) --> (IY+0)
   SET     2,(IY)             // $FD $CB $00 %11010110($D6)   ; (IY) --> (IY+0)
   SET     3,(IY)             // $FD $CB $00 %11011110($DE)   ; (IY) --> (IY+0)
   SET     4,(IY)             // $FD $CB $00 %11100110($E6)   ; (IY) --> (IY+0)
   SET     5,(IY)             // $FD $CB $00 %11101110($EE)   ; (IY) --> (IY+0)
   SET     6,(IY)             // $FD $CB $00 %11110110($F6)   ; (IY) --> (IY+0)
   SET     7,(IY)             // $FD $CB $00 %11111110($FE)   ; (IY) --> (IY+0)
   SET     0,L                // $CB %11000101($C5)
   SET     1,L                // $CB %11001101($CD)
   SET     2,L                // $CB %11010101($D5)
   SET     3,L                // $CB %11011101($DD)
   SET     4,L                // $CB %11100101($E5)
   SET     5,L                // $CB %11101101($ED)
   SET     6,L                // $CB %11110101($F5)
   SET     7,L                // $CB %11111101($FD)

; Z180 CPU Compare instructions

   CP      A                  // $BF
   CP      A,A                // $BF
   CP      A,B                // $B8
   CP      A,C                // $B9
   CP      A,D                // $BA
   CP      A,E                // $BB
   CP      A,H                // $BC
   CP      A,(HL)             // $BE
   CP      A,(IX+DISPL)       // $DD $BE [2:S8]
   CP      A,(IX)             // $DD $BE $00                  ; (IX) --> (IX+0)
   CP      A,(IY+DISPL)       // $FD $BE [2:S8]
   CP      A,(IY)             // $FD $BE $00                  ; (IY) --> (IY+0)
   CP      A,L                // $BD
   CP      A,U8               // $FE [2:U8]
   CP      B                  // $B8
   CP      C                  // $B9
   CP      D                  // $BA
   CP      E                  // $BB
   CP      H                  // $BC
   CP      (HL)               // $BE
   CP      (IX+DISPL)         // $DD $BE [1:S8]
   CP      (IX)               // $DD $BE $00                  ; (IX) --> (IX+0), alternative form of CP A,(IX+0)
   CP      (IY+DISPL)         // $FD $BE [1:S8]
   CP      (IY)               // $FD $BE $00                  ; (IY) --> (IY+0), alternative form of CP A,(IY+0)
   CP      L                  // $BD
   CP      U8                 // $FE [1:U8]
   CPD                        // $ED $A9
   CPDR                       // $ED $B9
   CPI                        // $ED $A1
   CPIR                       // $ED $B1

; Z180 CPU Data move instructions

   EX      AF,AF'             // $08
   EX      DE,HL              // $EB
   EX      (SP),HL            // $E3
   EX      (SP),IX            // $DD $E3
   EX      (SP),IY            // $FD $E3
   EXX                        // $D9
   LD      A,A                // $7F
   LD      A,B                // $78
   LD      A,(BC)             // $0A
   LD      A,C                // $79
   LD      A,D                // $7A
   LD      A,(DE)             // $1A
   LD      A,E                // $7B
   LD      A,H                // $7C
   LD      A,(HL)             // $7E
   LD      A,I                // $ED $57
   LD      A,(IX+DISPL)       // $DD $7E [2:S8]
   LD      A,(IX)             // $DD $7E $00                  ; (IX) --> (IX+0)
   LD      A,(IY+DISPL)       // $FD $7E [2:S8]
   LD      A,(IY)             // $FD $7E $00                  ; (IY) --> (IY+0)
   LD      A,L                // $7D
   LD      A,R                // $ED $5F
   LD      A,(U16)            // $3A [2:U16]
   LD      A,U8               // $3E [2:U8]
   LD      B,A                // $47
   LD      B,B                // $40
   LD      B,C                // $41
   LD      B,D                // $42
   LD      B,E                // $43
   LD      B,H                // $44
   LD      B,(HL)             // $46
   LD      B,(IX+DISPL)       // $DD $46 [2:S8]
   LD      B,(IX)             // $DD $46 $00                  ; (IX) --> (IX+0)
   LD      B,(IY+DISPL)       // $FD $46 [2:S8]
   LD      B,(IY)             // $FD $46 $00                  ; (IY) --> (IY+0)
   LD      B,L                // $45
   LD      B,U8               // $06 [2:U8]
   LD      BC,U16             // $01 [2:U16]
   LD      BC,(U16)           // $ED $4B [2:U16]
   LD      (BC),A             // $02
   LD      C,A                // $4F
   LD      C,B                // $48
   LD      C,C                // $49
   LD      C,D                // $4A
   LD      C,E                // $4B
   LD      C,H                // $4C
   LD      C,(HL)             // $4E
   LD      C,(IX+DISPL)       // $DD $4E [2:S8]
   LD      C,(IX)             // $DD $4E $00                  ; (IX) --> (IX+0)
   LD      C,(IY+DISPL)       // $FD $4E [2:S8]
   LD      C,(IY)             // $FD $4E $00                  ; (IY) --> (IY+0)
   LD      C,L                // $4D
   LD      C,U8               // $0E [2:U8]
   LD      D,A                // $57
   LD      D,B                // $50
   LD      D,C                // $51
   LD      D,D                // $52
   LD      D,E                // $53
   LD      D,H                // $54
   LD      D,(HL)             // $56
   LD      D,(IX+DISPL)       // $DD $56 [2:S8]
   LD      D,(IX)             // $DD $56 $00                  ; (IX) --> (IX+0)
   LD      D,(IY+DISPL)       // $FD $56 [2:S8]
   LD      D,(IY)             // $FD $56 $00                  ; (IY) --> (IY+0)
   LD      D,L                // $55
   LD      D,U8               // $16 [2:U8]
   LD      DE,U16             // $11 [2:U16]
   LD      DE,(U16)           // $ED $5B [2:U16]
   LD      (DE),A             // $12
   LD      E,A                // $5F
   LD      E,B                // $58
   LD      E,C                // $59
   LD      E,D                // $5A
   LD      E,E                // $5B
   LD      E,H                // $5C
   LD      E,(HL)             // $5E
   LD      E,(IX+DISPL)       // $DD $5E [2:S8]
   LD      E,(IX)             // $DD $5E $00                  ; (IX) --> (IX+0)
   LD      E,(IY+DISPL)       // $FD $5E [2:S8]
   LD      E,(IY)             // $FD $5E $00                  ; (IY) --> (IY+0)
   LD      E,L                // $5D
   LD      E,U8               // $1E [2:U8]
   LD      H,A                // $67
   LD      H,B                // $60
   LD      H,C                // $61
   LD      H,D                // $62
   LD      H,E                // $63
   LD      H,H                // $64
   LD      H,(HL)             // $66
   LD      H,(IX+DISPL)       // $DD $66 [2:S8]
   LD      H,(IX)             // $DD $66 $00                  ; (IX) --> (IX+0)
   LD      H,(IY+DISPL)       // $FD $66 [2:S8]
   LD      H,(IY)             // $FD $66 $00                  ; (IY) --> (IY+0)
   LD      H,L                // $65
   LD      H,U8               // $26 [2:U8]
   LD      HL,U16             // $21 [2:U16]
   LD      HL,(U16)           // $2A [2:U16]
   LD      (HL),A             // $77
   LD      (HL),B             // $70
   LD      (HL),C             // $71
   LD      (HL),D             // $72
   LD      (HL),E             // $73
   LD      (HL),H             // $74
   LD      (HL),L             // $75
   LD      (HL),U8            // $36 [2:U8]
   LD      I,A                // $ED $47
   LD      IX,U16             // $DD $21 [2:U16]
   LD      IX,(U16)           // $DD $2A [2:U16]
   LD      (IX+DISPL),A       // $DD $77 [1:S8]
   LD      (IX+DISPL),B       // $DD $70 [1:S8]
   LD      (IX+DISPL),C       // $DD $71 [1:S8]
   LD      (IX+DISPL),D       // $DD $72 [1:S8]
   LD      (IX+DISPL),E       // $DD $73 [1:S8]
   LD      (IX+DISPL),H       // $DD $74 [1:S8]
   LD      (IX+DISPL),L       // $DD $75 [1:S8]
   LD      (IX+DISPL),U8      // $DD $36 [1:S8] [2:U8]
   LD      (IX),A             // $DD $77 $00                  ; (IX) --> (IX+0)
   LD      (IX),B             // $DD $70 $00                  ; (IX) --> (IX+0)
   LD      (IX),C             // $DD $71 $00                  ; (IX) --> (IX+0)
   LD      (IX),D             // $DD $72 $00                  ; (IX) --> (IX+0)
   LD      (IX),E             // $DD $73 $00                  ; (IX) --> (IX+0)
   LD      (IX),H             // $DD $74 $00                  ; (IX) --> (IX+0)
   LD      (IX),L             // $DD $75 $00                  ; (IX) --> (IX+0)
   LD      (IX),U8            // $DD $36 $00 [2:U8]           ; (IX) --> (IX+0)
   LD      IY,U16             // $FD $21 [2:U16]
   LD      IY,(U16)           // $FD $2A [2:U16]
   LD      (IY+DISPL),A       // $FD $77 [1:S8]
   LD      (IY+DISPL),B       // $FD $70 [1:S8]
   LD      (IY+DISPL),C       // $FD $71 [1:S8]
   LD      (IY+DISPL),D       // $FD $72 [1:S8]
   LD      (IY+DISPL),E       // $FD $73 [1:S8]
   LD      (IY+DISPL),H       // $FD $74 [1:S8]
   LD      (IY+DISPL),L       // $FD $75 [1:S8]
   LD      (IY+DISPL),U8      // $FD $36 [1:S8] [2:U8]
   LD      (IY),A             // $FD $77 $00                  ; (IY) --> (IY+0)
   LD      (IY),B             // $FD $70 $00                  ; (IY) --> (IY+0)
   LD      (IY),C             // $FD $71 $00                  ; (IY) --> (IY+0)
   LD      (IY),D             // $FD $72 $00                  ; (IY) --> (IY+0)
   LD      (IY),E             // $FD $73 $00                  ; (IY) --> (IY+0)
   LD      (IY),H             // $FD $74 $00                  ; (IY) --> (IY+0)
   LD      (IY),L             // $FD $75 $00                  ; (IY) --> (IY+0)
   LD      (IY),U8            // $FD $36 $00 [2:U8]           ; (IY) --> (IY+0)
   LD      L,A                // $6F
   LD      L,B                // $68
   LD      L,C                // $69
   LD      L,D                // $6A
   LD      L,E                // $6B
   LD      L,H                // $6C
   LD      L,(HL)             // $6E
   LD      L,(IX+DISPL)       // $DD $6E [2:S8]
   LD      L,(IX)             // $DD $6E $00                  ; (IX) --> (IX+0)
   LD      L,(IY+DISPL)       // $FD $6E [2:S8]
   LD      L,(IY)             // $FD $6E $00                  ; (IY) --> (IY+0)
   LD      L,L                // $6D
   LD      L,U8               // $2E [2:U8]
   LD      R,A                // $ED $4F
   LD      SP,HL              // $F9
   LD      SP,IX              // $DD $F9
   LD      SP,IY              // $FD $F9
   LD      SP,U16             // $31 [2:U16]
   LD      SP,(U16)           // $ED $7B [2:U16]
   LD      (U16),A            // $32 [1:U16]
   LD      (U16),BC           // $ED $43 [1:U16]
   LD      (U16),DE           // $ED $53 [1:U16]
   LD      (U16),HL           // $22 [1:U16]
   LD      (U16),IX           // $DD $22 [1:U16]
   LD      (U16),IY           // $FD $22 [1:U16]
   LD      (U16),SP           // $ED $73 [1:U16]
   LDD                        // $ED $A8
   LDDR                       // $ED $B8
   LDI                        // $ED $A0
   LDIR                       // $ED $B0

; Z180 CPU Flag instructions

   CCF                        // $3F
   DI                         // $F3
   EI                         // $FB
   SCF                        // $37

; Z180 CPU Increment / decrement instructions

   DEC     A                  // $3D
   DEC     B                  // $05
   DEC     BC                 // $0B
   DEC     C                  // $0D
   DEC     D                  // $15
   DEC     DE                 // $1B
   DEC     E                  // $1D
   DEC     H                  // $25
   DEC     HL                 // $2B
   DEC     (HL)               // $35
   DEC     IX                 // $DD $2B
   DEC     (IX+DISPL)         // $DD $35 [1:S8]
   DEC     (IX)               // $DD $35 $00                  ; (IX) --> (IX+0)
   DEC     IY                 // $FD $2B
   DEC     (IY+DISPL)         // $FD $35 [1:S8]
   DEC     (IY)               // $FD $35 $00                  ; (IY) --> (IY+0)
   DEC     L                  // $2D
   DEC     SP                 // $3B
   INC     A                  // $3C
   INC     B                  // $04
   INC     BC                 // $03
   INC     C                  // $0C
   INC     D                  // $14
   INC     DE                 // $13
   INC     E                  // $1C
   INC     H                  // $24
   INC     HL                 // $23
   INC     (HL)               // $34
   INC     IX                 // $DD $23
   INC     (IX+DISPL)         // $DD $34 [1:S8]
   INC     (IX)               // $DD $34 $00                  ; (IX) --> (IX+0)
   INC     IY                 // $FD $23
   INC     (IY+DISPL)         // $FD $34 [1:S8]
   INC     (IY)               // $FD $34 $00                  ; (IY) --> (IY+0)
   INC     L                  // $2C
   INC     SP                 // $33

; Z180 CPU Logical instructions

   AND     A                  // $A7                          ; Alternative version of AND A,A
   AND     A,A                // $A7
   AND     A,B                // $A0
   AND     A,C                // $A1
   AND     A,D                // $A2
   AND     A,E                // $A3
   AND     A,H                // $A4
   AND     A,(HL)             // $A6
   AND     A,(IX+DISPL)       // $DD $A6 [2:S8]
   AND     A,(IX)             // $DD $A6 $00                  ; (IX) --> (IX+0)
   AND     A,(IY+DISPL)       // $FD $A6 [2:S8]
   AND     A,(IY)             // $FD $A6 $00                  ; (IY) --> (IY+0)
   AND     A,L                // $A5
   AND     A,U8               // $E6 [2:U8]
   AND     B                  // $A0                          ; Alternative version of AND A,B
   AND     C                  // $A1                          ; Alternative version of AND A,C
   AND     D                  // $A2                          ; Alternative version of AND A,D
   AND     E                  // $A3                          ; Alternative version of AND A,E
   AND     H                  // $A4                          ; Alternative version of AND A,H
   AND     (HL)               // $A6                          ; Alternative version of AND A,HL_IND
   AND     (IX+DISPL)         // $DD $A6 [1:S8]               ; Alternative version of AND A,IXPD_IND
   AND     (IX)               // $DD $A6 $00                  ; (IX) --> (IX+0), alternative form of AND A,(IX+0)
   AND     (IY+DISPL)         // $FD $A6 [1:S8]               ; Alternative version of AND A,IYPD_IND
   AND     (IY)               // $FD $A6 $00                  ; (IY) --> (IY+0), alternative form of AND A,(IY+0)
   AND     L                  // $A5                          ; Alternative version of AND A,L
   AND     U8                 // $E6 [1:U8]                   ; Alternative version of AND A,U8
   OR      A                  // $B7                          ; Alternative version of OR A,A
   OR      A,A                // $B7
   OR      A,B                // $B0
   OR      A,C                // $B1
   OR      A,D                // $B2
   OR      A,E                // $B3
   OR      A,H                // $B4
   OR      A,(HL)             // $B6
   OR      A,(IX+DISPL)       // $DD $B6 [2:S8]
   OR      A,(IX)             // $DD $B6 $00                  ; (IX) --> (IX+0)
   OR      A,(IY+DISPL)       // $FD $B6 [2:S8]
   OR      A,(IY)             // $FD $B6 $00                  ; (IY) --> (IY+0)
   OR      A,L                // $B5
   OR      A,U8               // $F6 [2:U8]
   OR      B                  // $B0                          ; Alternative version of OR A,B
   OR      C                  // $B1                          ; Alternative version of OR A,C
   OR      D                  // $B2                          ; Alternative version of OR A,D
   OR      E                  // $B3                          ; Alternative version of OR A,E
   OR      H                  // $B4                          ; Alternative version of OR A,H
   OR      (HL)               // $B6                          ; Alternative version of OR A,HL_IND
   OR      (IX+DISPL)         // $DD $B6 [1:S8]               ; Alternative version of OR A,IXPD_IND
   OR      (IX)               // $DD $B6 $00                  ; (IX) --> (IX+0), alternative form of OR A,(IX+0)
   OR      (IY+DISPL)         // $FD $B6 [1:S8]               ; Alternative version of OR A,IYPD_IND
   OR      (IY)               // $FD $B6 $00                  ; (IY) --> (IY+0), alternative form of OR A,(IY+0)
   OR      L                  // $B5                          ; Alternative version of OR A,L
   OR      U8                 // $F6 [1:U8]                   ; Alternative version of OR A,U8
   TST     A                  // $ED $3C
   TST     B                  // $ED $04
   TST     C                  // $ED $0C
   TST     D                  // $ED $14
   TST     E                  // $ED $1C
   TST     H                  // $ED $24
   TST     (HL)               // $ED $34
   TST     L                  // $ED $2C
   TST     U8                 // $ED $64 [1:U8]
   TSTIO   U8                 // $ED $74 [1:U8]
   XOR     A                  // $AF                          ; Alternative version of XOR A,A
   XOR     A,A                // $AF
   XOR     A,B                // $A8
   XOR     A,C                // $A9
   XOR     A,D                // $AA
   XOR     A,E                // $AB
   XOR     A,H                // $AC
   XOR     A,(HL)             // $AE
   XOR     A,(IX+DISPL)       // $DD $AE [2:S8]
   XOR     A,(IX)             // $DD $AE $00                  ; (IX) --> (IX+0)
   XOR     A,(IY+DISPL)       // $FD $AE [2:S8]
   XOR     A,(IY)             // $FD $AE $00                  ; (IY) --> (IY+0)
   XOR     A,L                // $AD
   XOR     A,U8               // $EE [2:U8]
   XOR     B                  // $A8                          ; Alternative version of XOR A,B
   XOR     C                  // $A9                          ; Alternative version of XOR A,C
   XOR     D                  // $AA                          ; Alternative version of XOR A,D
   XOR     E                  // $AB                          ; Alternative version of XOR A,E
   XOR     H                  // $AC                          ; Alternative version of XOR A,H
   XOR     (HL)               // $AE                          ; Alternative version of XOR A,HL_IND
   XOR     (IX+DISPL)         // $DD $AE [1:S8]               ; Alternative version of XOR A,IXPD_IND
   XOR     (IX)               // $DD $AE $00                  ; (IX) --> (IX+0), alternative form of XOR A,(IX+0)
   XOR     (IY+DISPL)         // $FD $AE [1:S8]               ; Alternative version of XOR A,IYPD_IND
   XOR     (IY)               // $FD $AE $00                  ; (IY) --> (IY+0), alternative form of XOR A,(IY+0)
   XOR     L                  // $AD                          ; Alternative version of XOR A,L
   XOR     U8                 // $EE [1:U8]                   ; Alternative version of XOR A,U8

; Z180 CPU Port instructions

   IN      A,(C)              // $ED $78
   IN      A,(U8)             // $DB [2:U8]
   IN      B,(C)              // $ED $40
   IN      C,(C)              // $ED $48
   IN      D,(C)              // $ED $50
   IN      E,(C)              // $ED $58
   IN      H,(C)              // $ED $60
   IN      L,(C)              // $ED $68
   IN0     A,(U8)             // $ED $38 [2:U8]
   IN0     B,(U8)             // $ED $00 [2:U8]
   IN0     C,(U8)             // $ED $08 [2:U8]
   IN0     D,(U8)             // $ED $10 [2:U8]
   IN0     E,(U8)             // $ED $18 [2:U8]
   IN0     H,(U8)             // $ED $20 [2:U8]
   IN0     (HL),(U8)          // $ED $30 [2:U8]
   IN0     L,(U8)             // $ED $28 [2:U8]
   IND                        // $ED $AA
   INDR                       // $ED $BA
   INI                        // $ED $A2
   INIR                       // $ED $B2
   OTDM                       // $ED $8B
   OTDMR                      // $ED $9B
   OTDR                       // $ED $BB
   OTIM                       // $ED $83
   OTIMR                      // $ED $93
   OTIR                       // $ED $B3
   OUT     (C),A              // $ED $79
   OUT     (C),B              // $ED $41
   OUT     (C),C              // $ED $49
   OUT     (C),D              // $ED $51
   OUT     (C),E              // $ED $59
   OUT     (C),H              // $ED $61
   OUT     (C),L              // $ED $69
   OUT     (U8),A             // $D3 [1:U8]
   OUT0    (U8),A             // $ED $39 [1:U8]
   OUT0    (U8),B             // $ED $01 [1:U8]
   OUT0    (U8),C             // $ED $09 [1:U8]
   OUT0    (U8),D             // $ED $11 [1:U8]
   OUT0    (U8),E             // $ED $19 [1:U8]
   OUT0    (U8),H             // $ED $21 [1:U8]
   OUT0    (U8),(HL)          // $ED $31 [1:U8]
   OUT0    (U8),L             // $ED $29 [1:U8]
   OUTD                       // $ED $AB
   OUTI                       // $ED $A3

; Z180 CPU Program flow instructions

   CALL    C,U16              // $DC [2:U16]
   CALL    M,U16              // $FC [2:U16]
   CALL    NC,U16             // $D4 [2:U16]
   CALL    NZ,U16             // $C4 [2:U16]
   CALL    P,U16              // $F4 [2:U16]
   CALL    PE,U16             // $EC [2:U16]
   CALL    PO,U16             // $E4 [2:U16]
   CALL    U16                // $CD [1:U16]
   CALL    Z,U16              // $CC [2:U16]
   DJNZ    $+123              // $10 [1:R8]($79)
   HALT                       // $76
   JP      C,U16              // $DA [2:U16]
   JP      (HL)               // $E9
   JP      (IX)               // $DD $E9
   JP      (IY)               // $FD $E9
   JP      M,U16              // $FA [2:U16]
   JP      NC,U16             // $D2 [2:U16]
   JP      NZ,U16             // $C2 [2:U16]
   JP      P,U16              // $F2 [2:U16]
   JP      PE,U16             // $EA [2:U16]
   JP      PO,U16             // $E2 [2:U16]
   JP      U16                // $C3 [1:U16]
   JP      Z,U16              // $CA [2:U16]
   JR      C,$+123            // $38 [2:R8]($79)
   JR      NC,$+123           // $30 [2:R8]($79)
   JR      NZ,$+123           // $20 [2:R8]($79)
   JR      $+123              // $18 [1:R8]($79)
   JR      Z,$+123            // $28 [2:R8]($79)
   RET                        // $C9
   RET     C                  // $D8
   RET     M                  // $F8
   RET     NC                 // $D0
   RET     NZ                 // $C0
   RET     P                  // $F0
   RET     PE                 // $E8
   RET     PO                 // $E0
   RET     Z                  // $C8
   RETI                       // $ED $4D
   RETN                       // $ED $45
   RST     U8                 // %11[1:RST]111
   SLP                        // $ED $76

; Z180 CPU Shift / rotate instructions

   RL      A                  // $CB $17
   RL      B                  // $CB $10
   RL      C                  // $CB $11
   RL      D                  // $CB $12
   RL      E                  // $CB $13
   RL      H                  // $CB $14
   RL      (HL)               // $CB $16
   RL      (IX+DISPL)         // $DD $CB [1:S8] $16
   RL      (IX)               // $DD $CB $00 $16              ; (IX) --> (IX+0)
   RL      (IY+DISPL)         // $FD $CB [1:S8] $16
   RL      (IY)               // $FD $CB $00 $16              ; (IY) --> (IY+0)
   RL      L                  // $CB $15
   RLA                        // $17
   RLC     A                  // $CB $07
   RLC     B                  // $CB $00
   RLC     C                  // $CB $01
   RLC     D                  // $CB $02
   RLC     E                  // $CB $03
   RLC     H                  // $CB $04
   RLC     (HL)               // $CB $06
   RLC     (IX+DISPL)         // $DD $CB [1:S8] $06
   RLC     (IX)               // $DD $CB $00 $06              ; (IX) --> (IX+0)
   RLC     (IY+DISPL)         // $FD $CB [1:S8] $06
   RLC     (IY)               // $FD $CB $00 $06              ; (IY) --> (IY+0)
   RLC     L                  // $CB $05
   RLCA                       // $07
   RLD                        // $ED $6F
   RR      A                  // $CB $1F
   RR      B                  // $CB $18
   RR      C                  // $CB $19
   RR      D                  // $CB $1A
   RR      E                  // $CB $1B
   RR      H                  // $CB $1C
   RR      (HL)               // $CB $1E
   RR      (IX+DISPL)         // $DD $CB [1:S8] $1E
   RR      (IX)               // $DD $CB $00 $1E              ; (IX) --> (IX+0)
   RR      (IY+DISPL)         // $FD $CB [1:S8] $1E
   RR      (IY)               // $FD $CB $00 $1E              ; (IY) --> (IY+0)
   RR      L                  // $CB $1D
   RRA                        // $1F
   RRC     A                  // $CB $0F
   RRC     B                  // $CB $08
   RRC     C                  // $CB $09
   RRC     D                  // $CB $0A
   RRC     E                  // $CB $0B
   RRC     H                  // $CB $0C
   RRC     (HL)               // $CB $0E
   RRC     (IX+DISPL)         // $DD $CB [1:S8] $0E
   RRC     (IX)               // $DD $CB $00 $0E              ; (IX) --> (IX+0)
   RRC     (IY+DISPL)         // $FD $CB [1:S8] $0E
   RRC     (IY)               // $FD $CB $00 $0E              ; (IY) --> (IY+0)
   RRC     L                  // $CB $0D
   RRCA                       // $0F
   RRD                        // $ED $67
   SLA     A                  // $CB $27
   SLA     B                  // $CB $20
   SLA     C                  // $CB $21
   SLA     D                  // $CB $22
   SLA     E                  // $CB $23
   SLA     H                  // $CB $24
   SLA     (HL)               // $CB $26
   SLA     (IX+DISPL)         // $DD $CB [1:S8] $26
   SLA     (IX)               // $DD $CB $00 $26              ; (IX) --> (IX+0)
   SLA     (IY+DISPL)         // $FD $CB [1:S8] $26
   SLA     (IY)               // $FD $CB $00 $26              ; (IY) --> (IY+0)
   SLA     L                  // $CB $25
   SRA     A                  // $CB $2F
   SRA     B                  // $CB $28
   SRA     C                  // $CB $29
   SRA     D                  // $CB $2A
   SRA     E                  // $CB $2B
   SRA     H                  // $CB $2C
   SRA     (HL)               // $CB $2E
   SRA     (IX+DISPL)         // $DD $CB [1:S8] $2E
   SRA     (IX)               // $DD $CB $00 $2E              ; (IX) --> (IX+0)
   SRA     (IY+DISPL)         // $FD $CB [1:S8] $2E
   SRA     (IY)               // $FD $CB $00 $2E              ; (IY) --> (IY+0)
   SRA     L                  // $CB $2D
   SRL     A                  // $CB $3F
   SRL     B                  // $CB $38
   SRL     C                  // $CB $39
   SRL     D                  // $CB $3A
   SRL     E                  // $CB $3B
   SRL     H                  // $CB $3C
   SRL     (HL)               // $CB $3E
   SRL     (IX+DISPL)         // $DD $CB [1:S8] $3E
   SRL     (IX)               // $DD $CB $00 $3E              ; (IX) --> (IX+0)
   SRL     (IY+DISPL)         // $FD $CB [1:S8] $3E
   SRL     (IY)               // $FD $CB $00 $3E              ; (IY) --> (IY+0)
   SRL     L                  // $CB $3D

; Z180 CPU Special instructions

   IM      0                  // $ED $46
   IM      1                  // $ED $56
   IM      2                  // $ED $5E
   NOP                        // $00

; Z180 CPU Stack instructions

   POP     AF                 // $F1
   POP     BC                 // $C1
   POP     DE                 // $D1
   POP     HL                 // $E1
   POP     IX                 // $DD $E1
   POP     IY                 // $FD $E1
   PUSH    AF                 // $F5
   PUSH    BC                 // $C5
   PUSH    DE                 // $D5
   PUSH    HL                 // $E5
   PUSH    IX                 // $DD $E5
   PUSH    IY                 // $FD $E5
