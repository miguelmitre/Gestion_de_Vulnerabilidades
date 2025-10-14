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
	OPEN WINDOW window_1 AT 3,3 WITH FORM "CONTM021" ATTRIBUTE( BORDER)
	DISPLAY "  CONTM002               CONSULTA FOLIOS PROCESADOS                                " AT 3,1 
        ATTRIBUTE(REVERSE) 
        DISPLAY HOY USING "dd-mm-yyyy" AT 3,61 
        ATTRIBUTE(REVERSE)
	MENU "CONSULTA FOLIOS "
                       COMMAND "Consulta" "Consultar Folios Contabilidad"
	                        CALL Consulta()
                       COMMAND "Salir" "Salir del Programa"
                                EXIT MENU
	END MENU
        CLOSE WINDOW window_1
 END FUNCTION
################################################################################
FUNCTION Consulta()
         
	 DEFINE ga_record   ARRAY[3000] OF RECORD
                transaccion        LIKE         con_his_archivo.transaccion,
                transa_desc        LIKE         tab_transaccion.transa_desc,
                monto              LIKE         con_his_archivo.monto,
                folio              LIKE         con_his_archivo.folio
	 END RECORD
         
	 DEFINE pos                SMALLINT
	 DEFINE vfolio             CHAR(4)
	 DISPLAY "                                                                               " AT 1,1 
	 DISPLAY "  CTRL-C cancela                                                                " AT 2,1
         DISPLAY " CONSULTA " AT 2,62
	
         PROMPT "Proporcione el Numero de Folio a Consultar : "
	 ATTRIBUTE (REVERSE)
	 FOR vfolio
	 ATTRIBUTE (REVERSE)

	 DECLARE archivo_contab CURSOR FOR
	 SELECT con_his_archivo.transaccion,  tab_transaccion.transa_desc,
	        con_his_archivo.monto, con_his_archivo.folio
	      FROM con_his_archivo, tab_transaccion
	        WHERE con_his_archivo.transaccion[1,4] = tab_transaccion.transa_cod
	          AND folio = vfolio 
		ORDER BY 1
	 LET pos = 1
	 FOREACH archivo_contab INTO ga_record[pos].*
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
	 ELSE
	    ERROR "FOLIO INEXISTENTE .. VERIFIQUE"
	    ATTRIBUTE (REVERSE)
	    SLEEP 2
	    ERROR " "
	 END IF
END FUNCTION
