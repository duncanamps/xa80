//
// Test E2068_CANNOT_MAKE_SEGMENT_FIXED
// Duncan Munro - 26/01/2024
//

			SEGMENT	CSEG,RELOCATABLE
			
			// Start outputting code from 0
			
			DB		123
			DB		1
			DB		2
			DB		3
			
			// Now try and set ORG to make the segment fixed
			
			ORG		$0100
			
			END
			