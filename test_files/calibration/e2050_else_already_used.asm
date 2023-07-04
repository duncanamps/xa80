//
// Test E2050_ELSE_ALREADY_USED
// Duncan Munro - 04/07/2023
//

			IF  1 == 1
				LD	A, stuff
			ELSE
				LD	A, more_stuff
			ELSE
				LD	A, even_more_Stuff
			ENDIF
			
            END

