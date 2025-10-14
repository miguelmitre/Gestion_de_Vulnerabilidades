database safre_af
 main
   call despliega_archivos()
 end main

##############################################################################
FUNCTION despliega_archivos()
##############################################################################

	DEFINE aux_pausa			CHAR(1)
	DEFINE HOY		DATE
        DEFINE SW_1             SMALLINT

        OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

        WHENEVER ERROR STOP
   
   	--DEFER INTERRUPT

	LET HOY = TODAY
	OPEN WINDOW window_1 AT 3,3 WITH FORM "CONTM081" ATTRIBUTE( BORDER)
	DISPLAY "  CONTM008               CONSULTA TRANSACCIONES CONTABLES                                " AT 3,1 
        ATTRIBUTE(REVERSE) 
        DISPLAY HOY USING "dd-mm-yyyy" AT 3,61 
        ATTRIBUTE(REVERSE)
	MENU "CONSULTA TRANSACCIONES "
                       COMMAND "Consulta" "Consultar Transacciones Contables"
	                        CALL Consulta()
                       COMMAND "Salir" "Salir del Programa"
                                EXIT MENU
	END MENU
        CLOSE WINDOW window_1
 END FUNCTION
################################################################################
FUNCTION Consulta()
         
	 DEFINE ga_record   ARRAY[3000] OF RECORD
                transa_cod        LIKE          tab_transaccion.transa_cod,
                transa_desc        LIKE         tab_transaccion.transa_desc,
		debito_credito     LIKE         tab_transaccion. debito_credito
	 END RECORD
         
	 DEFINE pos                SMALLINT
	 DISPLAY "                                                                               " AT 1,1 
	 DISPLAY "  CTRL-C cancela                                                                " AT 2,1
         DISPLAY " CONSULTA " AT 2,62
	
	 DECLARE transa_contab CURSOR FOR
	 SELECT transa_cod, transa_desc,
	       debito_credito 
	      FROM tab_transaccion
		ORDER BY 1
	 LET pos = 1
	 FOREACH transa_contab INTO ga_record[pos].*
	         LET pos = pos + 1
         END FOREACH
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
   	
	    DISPLAY ARRAY ga_record TO scr_1.*
		    
		    ON KEY (INTERRUPT)
                       CLEAR FORM
		       EXIT DISPLAY
	               RETURN
	    END DISPLAY
       END IF
   	END FUNCTION 

