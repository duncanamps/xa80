//
// Test E2064_FAILED_MACRO_EXPANSION
// Duncan Munro - 08/07/2023
//

MYMACRO		MACRO	param1, param2
			DB		{parma1}	// Note the typo
			DB		{param2}
			ENDM
			
			MYMACRO	1,2
			
			END
			