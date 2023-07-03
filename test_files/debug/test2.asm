		DB		1 shl 3
			
		

CTEST		MACRO		inst, source, result
			LD			BC,{source}
			LD			DE,{result}
			CALL		TEST_{inst}
			ENDM

MAKETEST	MACRO		inst
TEST_{inst}	PUSH		BC			// Copy BC register
			POP			AF			// into AF
			{inst}					// Perform shift / rotate
			PUSH		AF			// Save flags straight away
			POP			HL			// Get flags back into HL
			CP			A,D			// Check if A the same
			JP			NZ, FAIL	// No, mismatch, flag an error
			LD			A,$13		// Mask for Half carry, subtract, and carry flags
			AND			A,L			// Leave just the ones we want
			CP			A,E			// Expected?
			JP			NZ, FAIL	// No, flag as an error
			RET						// Return; all was good
			ENDM
			
TESTXRLA	MACRO		source
			expect = (({source} & $7F00) << 1) | (({source} & $0001) << 8) | (({source} & $8000) >> 15)
			CTEST		RLA, {source}, expect
			ENDM
			
TESTXRLCA	MACRO		source
			expect = (({source} & $7F00) << 1) | (({source} & $8000) >> 7) | (({source} & $8000) >> 15)
			CTEST 		RLCA, {source}, expect
			ENDM
			
TESTXRRA	MACRO		source
			expect = (({source} & $FE00) >> 1) | (({source} & $0001) << 15) | (({source} & $0100) >> 8)
			CTEST 		RRA, {source}, expect
			ENDM
			
TESTXRRCA	MACRO		source
			expect = (({source} & $FE00) >> 1) | (({source} & $0100) << 7) | (({source} & $0100) >> 8)
			CTEST 		RRCA, {source}, expect
			ENDM
			
			ORG	$2000	// Put our code at the basic cold start address

// What follows is our test code

			LD			HL,WELCOME
			CALL		PRINT
			
//----------------------------------------------------------------------------			
//	$07		Test RLCA instruction
//----------------------------------------------------------------------------			
			LD			HL,MSGRLCA
			CALL		PRINT
			TESTXRLCA	$0000
			TESTXRLCA	$0001
			TESTXRLCA	$0F00
			TESTXRLCA	$0F01
			TESTXRLCA	$5500
			TESTXRLCA	$5501
			TESTXRLCA	$AA00
			TESTXRLCA	$AA01
			TESTXRLCA	$F000
			TESTXRLCA	$F001
			TESTXRLCA	$FF00
			TESTXRLCA	$FF01
			CALL		PRINTOK

// The actual test procedures themselves

			MAKETEST	RLA
			MAKETEST	RLCA
			MAKETEST	RRA
			MAKETEST	RRCA
			
			