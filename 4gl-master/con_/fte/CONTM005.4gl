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
        DEFINE vfolio           CHAR(3)
        DEFINE vtransaccion     CHAR(6)
        DEFINE vfecha_conversion DATE
        DEFINE vrespuesta       CHAR(1)

        OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

        WHENEVER ERROR STOP
   
   	--DEFER INTERRUPT

	LET HOY = TODAY
	OPEN WINDOW window_1 AT 3,3 WITH FORM "CONTM003" ATTRIBUTE( BORDER)
	DISPLAY "           CONTM005  ACTUALIZACION HISTORICO DE ARCHIVOS            " AT 3,1 
        ATTRIBUTE(REVERSE) 
        DISPLAY HOY USING "dd-mm-yyyy" AT 3,61 
        ATTRIBUTE (REVERSE)
        
        PROMPT "DESEA BORRAR EL ARCHIVO?  S/N"
        ATTRIBUTE(REVERSE)
        FOR vrespuesta
        ATTRIBUTE (REVERSE)
        IF vrespuesta MATCHES"[sS]" THEN

        INPUT BY NAME vfolio, vtransaccion, vfecha_conversion
              
               BEFORE FIELD vfolio           
                   ERROR "PROPORCIONE EL NUMERO DE FOLIO"
                   ATTRIBUTE (REVERSE)
                   SLEEP 2
                   ERROR " "
               BEFORE FIELD vtransaccion           
                   ERROR "PROPORCIONE LA TRANSACCION P o L" 
                   ATTRIBUTE (REVERSE)
                   SLEEP 2
                   ERROR " "
 
               AFTER FIELD vtransaccion
                  IF vtransaccion = "P" THEN
                       LET vtransaccion = "11"
                   ELSE 
                       LET vtransaccion = "12"
                  END IF

               SELECT unique(fecha_conversion) INTO vfecha_conversion 
               FROM con_his_archivo
               WHERE folio = vfolio
               AND transaccion[1,2] = vtransaccion
               IF status = NOTFOUND THEN
               ERROR "NO EXISTEN REGISTROS"
               ATTRIBUTE (REVERSE)
               SLEEP 3
               ERROR "  "
               CLEAR FORM
               RETURN
               END IF

               DISPLAY BY NAME vfecha_conversion


                DELETE FROM con_his_archivo
                WHERE folio = vfolio
                AND fecha_conversion = vfecha_conversion 
                AND transaccion[1,2] = vtransaccion
                ERROR "HISTORICO DE CONTABILIDAD BORRADO"
                ATTRIBUTE (REVERSE) 
                SLEEP 3
                ERROR "  "
                CLEAR FORM
                RETURN
 
             END INPUT
           ELSE
                ERROR "PROCESO CANCELADO"
                ATTRIBUTE(REVERSE)
                SLEEP 2
                ERROR " "
        END IF	

END FUNCTION
