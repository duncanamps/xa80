//
// Test W1006_COMMAND_AS_SYMBOL
// Duncan Munro - 08/07/2023
//

MYMACRO		MACRO	param1, param2
			DB		{param1}
			DB		{param2}
			ENDM
			
			MYMACRO	1,2,3	// Have a sneaky third parameter
		
			END
			