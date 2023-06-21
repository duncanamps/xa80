			IF DEFINED(__DMFPMACRO_INC)
				LISTOFF
			ELSE
				__DMFPMACRO_INC = 1
			
			
;-----------------------------------------------------------------------------
;
; MACROS
;
;-----------------------------------------------------------------------------			

; Add 24 bit value with carry out
; dst <-- dst + src
; src is preserved

M_ADC24		MACRO	dst,src
			LD		HL,{dst}+2			; Get LSB address
			LD		A,({src}+2)			; Get LSB
			ADD		A,(HL)				; add to dest
			LD		(HL),A				; Save it
			DEC		HL					; next byte
			LD		A,({src}+1)			; Get central byte
			ADC		A,(HL)				; add to dest
			LD		(HL),A				; Save it
			DEC		HL					; next byte
			LD		A,({src}+0)			; Get MSB
			ADC		A,(HL)				; add to dest
			LD		(HL),A				; Save it
			ENDM
			
; Copy floating point 4 bytes from source to dest

M_COPY		MACRO	source, dest
			IF "{source}" == "FPACC1"
			  LD	DE,{dest}
			  CALL	FPSAVE1
			ELSE
			  IF "{source}" == "FPACC2"
			    LD 	 DE,{dest}
				CALL FPSAVE2
			  ELSE
			    IF "{dest}" == "FPACC1"
				  LD HL,{source}
				  CALL FPLOAD1
				ELSE
				  IF "{dest}" == "FPACC2"
				    LD HL,{source}
					CALL FPLOAD2
				  ELSE
				    LD HL,{source}
					LD DE,{dest}
					CALL FPLOADX
				  ENDIF
				ENDIF
			  ENDIF
			ENDIF
			ENDM

; Rotate right 24 bits with carry in if present

M_RRF24		MACRO	acc1
			LD		HL,{acc1}+1
			RR		(HL)
			DEC		HL
			RR		(HL)
			DEC		HL
			RR		(HL)
			ENDM
	
; Subtract 24 bit value with carry out
; dst <-- dst - src
; src is preserved

M_SBC24		MACRO	dst,src
			LD		HL,{src}+2			; Get LSB address
			LD		A,({dst}+2)			; Get dest LSB
			SUB		A,(HL)				; and subtract source
			LD		({dst}+2),A			; save it
			DEC		HL					; Bump pointer
			LD		A,({dst}+1)			; Get dest middle byte
			SBC		A,(HL)				; and subtract source
			LD		({dst}+1),A			; save it
			DEC		HL					; Bump pointer
			LD		A,({dst})			; Get dest MSB
			SUB		A,(HL)				; and subtract source
			LD		({dst}),A			; save it
			ENDM


			ENDIF // IF DEFINED(__DMFPMACRO_INC)
			LISTON
			
