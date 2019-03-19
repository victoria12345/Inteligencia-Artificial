;*************************************************************************
; Autores: Victoria Pelayo e Ignacio Rabunnal
; grupo: 2301
; Practica 2 apartado A
;*************************************************************************

;**************************************************************************
; DEFINICION DEL SEGMENTO DE DATOS
DATOS SEGMENT
	MATRIZ db 2,2,3,4,5,6,7,8,9
	POSITIVAS dw 3 dup(?)
	NEGATIVAS dw 4 dup(?)
	CLR_PANT 	DB 	1BH,"[2","J$"
	PREGUNTA 	DB 	1BH,"[2;1fIntroducir datos (1) o calcular valor por defecto (2)?$"
	TEXTO_AUX   DB  1BH,"[4;1fLa opcion elegida es: $"
	TEXTO_1   DB  1BH,"[1;1fLa opcion elegida es: INTRODUCIR DATOS. Introduzca los nueve numeros(entre 15 y -15) separados por espacios$"
	TEXTO_2   DB  1BH,"[1;1fLa opcion elegida es: VALORES POR DEFECTO$"
	TEXTO_ERROR   DB  1BH,"[1;1fOPCION INTRODUCIDA NO VALIDA$"
	ELECCION DB 3 dup(?)
	NUMEROS DB 80 dup(?)
DATOS ENDS
;**************************************************************************
; DEFINICION DEL SEGMENTO DE PILA
PILA SEGMENT STACK "STACK"
DB 40h DUP (0) ;ejemplo de inicialización, 64 bytes inicializados a 0
PILA ENDS
;**************************************************************************
; DEFINICION DEL SEGMENTO EXTRA
EXTRA SEGMENT
RESULT DW 0,0 ;ejemplo de inicialización. 2 PALABRAS (4 BYTES)
EXTRA ENDS
;**************************************************************************
; DEFINICION DEL SEGMENTO DE CODIGO
CODE SEGMENT
ASSUME CS: CODE, DS: DATOS, ES: EXTRA, SS: PILA
; COMIENZO DEL PROCEDIMIENTO PRINCIPAL
INICIO PROC
; INICIALIZA LOS REGISTROS DE SEGMENTO CON SU VALOR
MOV AX, DATOS
MOV DS, AX
MOV AX, PILA
MOV SS, AX
MOV AX, EXTRA
MOV ES, AX
MOV SP, 64 ; CARGA EL PUNTERO DE PILA CON EL VALOR MAS ALTO
; FIN DE LAS INICIALIZACIONES
; COMIENZO DEL PROGRAMA


MOV AH,9	; BORRA LA PANTALLA
MOV DX, OFFSET CLR_PANT
INT 21H

MOV DX,OFFSET PREGUNTA		;MUESTRA "ESCRIBE ...
INT 21H

MOV AH,0AH			;ALMACENA LA ELECCION TECLEADA
MOV DX,OFFSET ELECCION

MOV ELECCION[0], 3
INT 21H

cmp ELECCION[2], 31h
je JMP_ETIQUETA1

cmp ELECCION[2], 32h
je JMP_ETIQUETA2

MOV AH,9	; BORRA LA PANTALLA
MOV DX, OFFSET CLR_PANT
INT 21H

MOV DX,OFFSET TEXTO_ERROR		
INT 21H

MOV AX, 4C00h
INT 21h

;; Utiliza los numeros por defecto
JMP_ETIQUETA2:
	MOV AH,9	; BORRA LA PANTALLA
	MOV DX, OFFSET CLR_PANT
	INT 21H

	MOV DX,OFFSET TEXTO_2		
	INT 21H
	
	CALL DET

	; FIN DEL PROGRAMA
	MOV AX, 4C00h
	INT 21h

;;
;; Etiqueta para introducir datos
JMP_ETIQUETA1:
	MOV AH,9	; BORRA LA PANTALLA
	MOV DX, OFFSET CLR_PANT
	INT 21H

	MOV DX,OFFSET TEXTO_1		
	INT 21H

	MOV AH,0AH			;ALMACENA LA ELECCION TECLEADA
	MOV DX,OFFSET NUMEROS

	MOV NUMEROS[0], 60
	INT 21H
	
	MOV BX,2
	MOV SI,0
	ALMACENAR:
		;Si es un espacio salta a aux
		CMP NUMEROS[BX], 20h
		JE AUX
		
		MOV AX, 1
		CMP NUMEROS[BX], 2Dh
		JNE POSITIVO
		
		MOV CL, -1
		IMUL CL		
		INC BX
		
		POSITIVO:
		CMP NUMEROS[BX], 31h
		JE MULTIPLO_10
		
		GUARDAR:
		MOV CL, 30h
		SUB NUMEROS[BX], CL
		
		IMUL NUMEROS[BX]
		MOV MATRIZ[SI], AL
		
		INC SI
		INC BX
		CMP SI, 9
		
		JNE ALMACENAR
		JE FIN
		
		MULTIPLO_10:
		CMP NUMEROS[BX + 1], 20h
		je GUARDAR
		INC BX
		
		MOV CL, 10
		
		MOV CH, 30h
		SUB NUMEROS[BX], CH
		
		ADD CL, NUMEROS[BX]
		IMUL CL
		MOV MATRIZ[SI], AL
		INC SI
		
		;en aux incrementamos BX y saltamos si corresponde a Almacenar
		AUX:
		CMP SI, 9
		INC BX
		JNE ALMACENAR
		JE FIN
	
	FIN:
	
	CALL DET
	
	MOV AX, 4C00h
	INT 21h
INICIO ENDP

DET PROC NEAR
	;;EMPIEZA EL CALUCLO DEL DETERMINANTE

	;PRIMERA DIAGONAL +
	MOV AX, 000h
	
	MOV AL, MATRIZ[0]
	IMUL MATRIZ[4]
	IMUL WORD PTR MATRIZ[8]
	MOV POSITIVAS[0], AX
	
	MOV AX, 000h
	MOV BX, 000h
	;2 DIAGONAL +
	MOV AL, MATRIZ[2]
	IMUL MATRIZ[3]
	MOV Bl, MATRIZ[7]
	IMUL BX
	MOV POSITIVAS[2], AX
	

	;3 DIAGONAL +
	MOV AX, 000h
	MOV BX, 000h
	MOV AL, MATRIZ[1]
	IMUL MATRIZ[5]
	MOV Bl, MATRIZ[6]
	IMUL BX
	MOV POSITIVAS[4], AX
	

;;NEGATIVAS

	;1 DIAGONAL -
	MOV AX, 000h
	MOV BX, 000h
	MOV AL, MATRIZ[2]
	IMUL MATRIZ[4]
	MOV BL, MATRIZ[6]
	IMUL BX
	MOV NEGATIVAS[0], AX
	

	;2 DIAGONAL -
	MOV AX, 000h
	MOV BX, 000h
	MOV AL, MATRIZ[0]
	IMUL MATRIZ[5]
	MOV BL, MATRIZ[7]
	IMUL BX
	MOV NEGATIVAS[2], AX
	

	;3 DIAGONAL -
	MOV AX, 000h
	MOV AL, MATRIZ[1]
	IMUL MATRIZ[3]
	IMUL WORD PTR MATRIZ[8]
	MOV NEGATIVAS[4], AX
	
	MOV AX, 000h
	MOV AX, POSITIVAS[0]
	MOV BX, POSITIVAS[2]
	
	ADD POSITIVAS[4], AX
	ADD POSITIVAS[4], BX
	
	MOV AX, NEGATIVAS[0]
	MOV BX, NEGATIVAS[2]
	
	ADD NEGATIVAS[4], AX
	ADD NEGATIVAS[4], BX
	
	;Guardamos en positivas el resultado del determinante
	MOV AX, NEGATIVAS[4]
	SUB POSITIVAS[4], AX
	MOV AX, POSITIVAS[4]
	
	RET
	; FIN DEL SEGMENTO DE CODIGO
DET ENDP 
CODE ENDS
; FIN DEL PROGRAMA INDICANDO DONDE COMIENZA LA EJECUCION
END INICIO