		ORG		$200
		
MM		MACRO	source, dest, count
		LD		HL, source
		LD		DE, dest
		LD		BC, count
		LDIR
		ENDM
		
LABEL	MM		$4000,$5000,$1000


