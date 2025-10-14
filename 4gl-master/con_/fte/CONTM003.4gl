################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => EFP
#Programa CONTM003 => ACTUALIZACION HISTORICO CONTABILIDAD              
#Fecha             => 8 Junio 1998				               #
#By                => JOSE MANUEL VIZCAINO CULEBRA. 			       #
#Sistema           => CONT. 					               #
################################################################################
DATABASE safre_af
GLOBALS
	DEFINE HOY	     	                DATE
        DEFINE vfolio                LIKE       con_his_archivo.folio,
               vfecha_conversion                DATE

        DEFINE
            vtransa             CHAR(2),
            vresp               CHAR(1)

END GLOBALS
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

        WHENEVER ERROR STOP
   
   	DEFER INTERRUPT

	LET HOY = TODAY
	#OPEN WINDOW ventana_1 AT 3,3 WITH FORM "CONTM041" ATTRIBUTE( BORDER)
	#OPEN WINDOW ventana_1 AT 3,3 WITH FORM "CONTM031" ATTRIBUTE( BORDER)
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "CONTM003" ATTRIBUTE( BORDER)
	DISPLAY " CONTM003                REVERSION FOLIOS CONTABILIDAD                             " AT 3,1 
        ATTRIBUTE(REVERSE) 
        DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 
        ATTRIBUTE(REVERSE)
	MENU "ACTUALIZACION FOLIOS"
                           COMMAND "Icefas" "Actualiza Historico Icefas"
		                    LET vtransa = "3" 
                                    CALL actualiza_historico(vtransa)
                           COMMAND "Recaudacion" "Recaudacion Normal"
                                    LET vtransa = "1"
                                    CALL actualiza_historico(vtransa)
                           COMMAND "Salir" "Salir del Programa"
		                     EXIT MENU
	END MENU
        CLOSE WINDOW ventana_1
END MAIN

################################################################################
   FUNCTION  actualiza_historico(vvtransa)
################################################################################
        
        DEFINE ga_record   ARRAY[300] OF RECORD
               folio              LIKE   con_his_archivo.folio,
               fecha_conversion          DATE,
               codigo_analisis1          CHAR(30) 
        END RECORD
        
        DEFINE gr_record  RECORD
               folio              LIKE   con_his_archivo.folio,
               fecha_conversion          DATE,
               codigo_analisis1          CHAR(30) 
        END RECORD
        
        DEFINE pos                SMALLINT,
               vvtransa           CHAR(1)   

        #OPEN WINDOW ventana_2 AT 10,8 WITH FORM "pantalla"
        OPEN WINDOW ventana_2 AT 6,8 WITH FORM "CONTM032"
        ATTRIBUTE (BORDER)

        DECLARE cursor_2 CURSOR FOR
	SELECT con_his_archivo.folio, con_his_archivo.fecha_conversion,
               tab_transaccion.codigo_analisis1
          FROM con_his_archivo, tab_transaccion
	   WHERE tab_transaccion.transa_cod[1,1] = vvtransa
            AND tab_transaccion.transa_cod[1,2] = con_his_archivo.transaccion[1,2] 
                GROUP BY 1,2,3      
                ORDER BY 1
        
        LET pos = 1
	FOREACH cursor_2 INTO ga_record[pos].*
	        LET pos = pos + 1
        END FOREACH
	
        IF (pos-1) >= 1 THEN
	   CALL  SET_COUNT(pos-1)
	   
           DISPLAY ARRAY ga_record TO scr_1.* 
                   ON KEY (CONTROL-M)
                      LET pos = ARR_CURR()
              LET gr_record.folio = ga_record[pos].folio
              LET gr_record.fecha_conversion = ga_record[pos].fecha_conversion
              LET vfolio = gr_record.folio    
              LET vfecha_conversion = gr_record.fecha_conversion

                  EXIT DISPLAY
                   
                  ON KEY (INTERRUPT)
                   ERROR "PROCESO CANCELADO .."
                   ATTRIBUTE (REVERSE)
                   SLEEP 2
                   EXIT PROGRAM
           
           END DISPLAY
	   CLOSE WINDOW ventana_2
           DISPLAY "FOLIO                : ", vfolio   AT 7,13 
           DISPLAY "FECHA CONVERSION     : ",  vfecha_conversion  AT  10,13
    
           PROMPT "Desea Borrar Este Archivo ? S/N "
           ATTRIBUTE (REVERSE)
           FOR vresp
           ATTRIBUTE (REVERSE)

           IF vresp MATCHES "[Ss]" THEN
              DELETE FROM con_his_archivo
                 WHERE folio = vfolio
                   AND fecha_conversion = vfecha_conversion
                   ERROR "HISTORICO ACTUALIZADO SATISFACTORIAMENTE"
                   ATTRIBUTE (REVERSE)
                   SLEEP 1
                   ERROR " "	
                   DISPLAY "                                 "   AT 7,13 
                   DISPLAY "                                 "   AT  10,13
               ELSE
                ERROR "PROCESO CANCELADO"
                ATTRIBUTE (REVERSE)
                SLEEP 2 
                ERROR " "
                CLEAR FORM
                DISPLAY "                                 "   AT 7,13 
                DISPLAY "                                 "   AT  10,13
           END IF
    ELSE
	   ERROR "FOLIO INEXISTENTE EN HISTORICO CONTABILIDAD"
    END IF

END FUNCTION

