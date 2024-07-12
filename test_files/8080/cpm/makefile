objects = bdos22.bin \
		  ccp22.bin

cpm : $(objects)

%.bin : %.asm
	xa80dev $< --com=$@ --errorlog --listing --map --processor=8080 --tab=8 --verbose=5

clean :
	CLEAN.BAT