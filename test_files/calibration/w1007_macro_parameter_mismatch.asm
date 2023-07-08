//
// Test W1007_MACRO_PARAMETER_MISMATCH
// Duncan Munro - 08/07/2023
//

MYMACRO		MACRO	param1, param2
			DB		{param1}
			DB		{param2}
			ENDM
			
			MYMACRO	1,2,3	// Have a sneaky third parameter
			
;			MYMACRO	1		// Insufficient
		
			END
			