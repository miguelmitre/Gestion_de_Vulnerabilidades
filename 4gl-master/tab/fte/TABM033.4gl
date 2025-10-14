################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#Owner             => EFP  					       #
#Programa TABM025  => MANTENEDOR ARCHIVO COMISIONES.
#Fecha             => 20 JUNIO 1997 					       #
#By                => JOSE MANUEL VIZCAINO
#Sistema           => TAB. 					               #
################################################################################
DATABASE safre_af
GLOBALS
	DEFINE aux_pausa			CHAR(1)
          DEFINE g_reg			RECORD 
               tipo_comision    LIKE    dis_val_comision.tipo_comision,
               subcuenta        LIKE    dis_val_comision.subcuenta,
               val_porcentaje   LIKE    dis_val_comision.val_porcentaje,
               val_pesos        LIKE    dis_val_comision.val_pesos
        END RECORD

        DEFINE RESP       CHAR(1)
 	DEFINE HOY		DATE
        DEFINE vquery     CHAR(700)

        DEFINE vcomision  CHAR(20)
        DEFINE vpais_cod  CHAR(5)
        DEFINE vflag      CHAR(1)

        DEFINE l_record   ARRAY[300] OF RECORD
               tipo_comision    LIKE    dis_val_comision.tipo_comision,
               descripcion      LIKE    tab_movimiento.descripcion,
               subcuenta        LIKE    dis_val_comision.subcuenta,
               subct_corta      LIKE    tab_subcuenta.subct_corta,
               val_porcentaje   LIKE    dis_val_comision.val_porcentaje,
               val_pesos        LIKE    dis_val_comision.val_pesos
       END RECORD
        DEFINE pos                SMALLINT   

END GLOBALS
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT
        WHENEVER ERROR STOP

	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,2 WITH FORM "TABM0331" ATTRIBUTE( BORDER)
	DISPLAY " TABM033                MANTENIMIENTO DE COMISIONES                               " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
	
	MENU "MANTENEDOR COMISIONES"
		COMMAND "Agrega" "Agrega Comisiones" 
                                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Comisiones"
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Comisiones"
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Comisiones"
		                     CALL Elimina()
                           COMMAND "Salir" "Salir del Programa"
		                     EXIT MENU
	END MENU
        CLOSE WINDOW ventana_1
END MAIN
################################################################################
FUNCTION Inicializa()
	INITIALIZE g_reg.* TO NULL
	DISPLAY BY NAME g_reg.*
END FUNCTION
################################################################################
FUNCTION Agrega()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ( Esc ) Agrega        (Ctrl-c) Salir                                          " AT 2,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
        LET g_reg.tipo_comision = NULL
	INPUT BY NAME  g_reg.*
	      AFTER FIELD tipo_comision
		    IF g_reg.tipo_comision IS NULL THEN
		       ERROR "Tipo de Comision  NO puede ser nulo" 
                       NEXT FIELD tipo_comision
		    END IF
		    SELECT "X" FROM dis_val_comision
		    WHERE tipo_comision = g_reg.tipo_comision
		    IF STATUS <> NOTFOUND THEN
		       ERROR "TIPO DE COMISION NO PUEDE SER NULO"
		       NEXT FIELD tipo_comision
		    END IF
              AFTER FIELD subcuenta
		     IF g_reg.subcuenta IS NULL THEN
		        ERROR "Codigo de Subcuenta NO puede ser nula"
		        NEXT FIELD subcuenta
		     END IF 
              AFTER FIELD val_porcentaje
		     IF g_reg.val_porcentaje IS NULL THEN
		        ERROR "Porcentaje NO puede ser nulo"
		        NEXT FIELD val_porcentaje
		     END IF 
              AFTER FIELD val_pesos
		     IF g_reg.val_pesos IS NULL THEN
		        ERROR "Valor en pesos NO pueder ser nulo"
		        NEXT FIELD val_pesos
		     END IF 
              NEXT FIELD val_pesos

               ON KEY ( ESC )
		     IF g_reg.tipo_comision IS NULL THEN
		        ERROR "Tipo de Comision NO puede ser NULO"
		        NEXT FIELD tipo_comision
		     END IF
		     IF g_reg.subcuenta IS NULL THEN
		        ERROR "Codigo de Subcuenta NO puede ser NULO"
                        NEXT FIELD subcuenta
		     END IF
		     SELECT "X" FROM dis_val_comision
		     WHERE tipo_comision = g_reg.tipo_comision
		     IF STATUS <> NOTFOUND THEN
		        ERROR "Comision ya Ingresada"
		        NEXT FIELD dis_val_comision
		     END IF
                     INSERT INTO dis_val_comision VALUES ( g_reg.* ) 
		     ERROR "REGISTRO INGRESADO" SLEEP 1
		     ERROR ""
                     CALL Inicializa()
		     NEXT FIELD tipo_comision
                    ON KEY (INTERRUPT)
                       CALL Inicializa()
                       EXIT INPUT
	END INPUT
END FUNCTION

################################################################################
FUNCTION Consulta()
	
         DISPLAY "" AT 1,1
	 DISPLAY "" AT 2,1
	 DISPLAY " ( Esc ) Consulta        (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
	 DISPLAY " CONSULTA " AT 1,65 

         LET pos = 2
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 5,3 WITH FORM "TABM0332" ATTRIBUTE( BORDER)
	      DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
	      DISPLAY " (Ctrl-M) Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	      DISPLAY "                             C O M I S I O N E S                                              " AT 2,1 ATTRIBUTE(REVERSE,BOLD) 
   	
	    INPUT BY NAME vcomision
               ATTRIBUTE (REVERSE)

                  AFTER field vcomision
                      IF vcomision IS NULL THEN
                         ERROR "Codigo de Comision NO puede ser nulo"
                         ATTRIBUTE (REVERSE)
                         SLEEP 2
                         ERROR " " 
                         NEXT FIELD vcomision
                        ELSE
                         EXIT INPUT
                      END IF
            END INPUT 
      LET vquery = " SELECT dis_val_comision.tipo_comision, ",
         " tab_movimiento.descripcion, dis_val_comision.subcuenta, ",
         " tab_subcuenta.subct_corta, dis_val_comision.val_porcentaje, ",
         " dis_val_comision.val_pesos ",
         " FROM dis_val_comision, tab_movimiento, tab_subcuenta ",
         " WHERE dis_val_comision.tipo_comision = tab_movimiento.codigo ",
         " AND dis_val_comision.subcuenta = tab_subcuenta.subct_cod",
         " AND tab_movimiento.descripcion MATCHES ", '"', vcomision CLIPPED, '"',
                   " ORDER BY 1, 2, 3 " CLIPPED
              PREPARE cur20y FROM vquery
              DECLARE prog_1 cursor FOR cur20y
              LET pos = 1
              FOREACH prog_1 INTO l_record[pos].*
                    LET pos = pos + 1
              END FOREACH
            IF (pos-1) < 1 THEN
                ERROR "NO SE ENCONTRARON REGISTROS CON LAS CONDICIONES"
                ATTRIBUTE (REVERSE)
                SLEEP 2
                ERROR " "
                INITIALIZE vquery TO NULL
                CLOSE WINDOW ventana_2
                RETURN
            END IF 
                CALL SET_COUNT(pos-1)
                DISPLAY ARRAY l_record TO scr_1.*
                    ON KEY ( INTERRUPT)
                       LET pos = 0 
                       EXIT DISPLAY
                    ON KEY ( INTERRUPT )
                       LET pos = ARR_CURR()
                       EXIT DISPLAY
                       END DISPLAY
              CLOSE WINDOW ventana_2 
           END IF
END FUNCTION

################################################################################
FUNCTION  Modifica()
################################################################################
         
         DISPLAY "" AT 1,1
	 DISPLAY "" AT 1,1
	 DISPLAY " ( Esc ) Modifica        (Ctrl-c) Salir                                          " AT 2,1 ATTRIBUTE(BOLD)
	 DISPLAY " MODIFICA " AT 1,65 

         LET pos = 2
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 5,3 WITH FORM "TABM0332" ATTRIBUTE( BORDER)
	      DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
	      DISPLAY " (Ctrl-M) Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	      DISPLAY "                           C O M I S I O N E S                                               " AT 2,1 ATTRIBUTE(REVERSE,BOLD) 
   	
	    INPUT BY NAME vcomision
               ATTRIBUTE (REVERSE)

                  AFTER field vcomision
                      IF vcomision IS NULL THEN
                         ERROR "Codigo de comision NO puede ser nulo"
                         ATTRIBUTE (REVERSE)
                         SLEEP 2
                         ERROR " " 
                         NEXT FIELD vcomision
                        ELSE
                         EXIT INPUT
                      END IF
            END INPUT 
            LET vflag = 1
            WHILE vflag = 1 
      
      LET vquery = " SELECT dis_val_comision.tipo_comision, ",
         " tab_movimiento.descripcion, dis_val_comision.subcuenta, ",
         " tab_subcuenta.subct_corta, dis_val_comision.val_porcentaje, ",
         " dis_val_comision.val_pesos ",
         " FROM dis_val_comision, tab_movimiento, tab_subcuenta ",
         " WHERE dis_val_comision.tipo_comision = tab_movimiento.codigo ",
         " AND dis_val_comision.subcuenta = tab_subcuenta.subct_cod",
         " AND tab_movimiento.descripcion MATCHES ", '"', vcomision CLIPPED, '"',
                   " ORDER BY 1, 2, 3 " CLIPPED
              PREPARE cur30y FROM vquery
              DECLARE prog_2 cursor FOR cur30y
              LET pos = 1
              FOREACH prog_2 INTO l_record[pos].*
                    LET pos = pos + 1
              END FOREACH
            IF (pos-1) < 1 THEN
                ERROR "NO SE ENCONTRARON REGISTROS CON LAS CONDICIONES"
                ATTRIBUTE (REVERSE)
                SLEEP 2
                ERROR " "
                INITIALIZE vquery TO NULL
                CLOSE WINDOW ventana_2
                RETURN
            END IF 
                CALL SET_COUNT(pos-1)
                DISPLAY ARRAY l_record TO scr_1.*
                    ON KEY ( INTERRUPT)
                       LET pos = 0 
                       EXIT DISPLAY
                    ON KEY ( CONTROL-M )
                       LET pos = ARR_CURR()
                       EXIT DISPLAY
                       END DISPLAY
                       IF pos <> 0 THEN
                    EXIT WHILE
                  END IF
             END WHILE
               LET g_reg.tipo_comision    = l_record[pos].tipo_comision
               LET g_reg.subcuenta       = l_record[pos].subcuenta
               LET g_reg.val_porcentaje  = l_record[pos].val_porcentaje
               LET g_reg.val_pesos       = l_record[pos].val_pesos

               CLOSE WINDOW ventana_2 
               DISPLAY BY NAME g_reg.tipo_comision, g_reg.subcuenta,
                               g_reg.val_porcentaje, g_reg.val_pesos
         
               INPUT BY NAME g_reg.tipo_comision, g_reg.val_porcentaje,
                             g_reg.val_pesos WITHOUT DEFAULTS

                     AFTER FIELD tipo_comision
                        IF g_reg.tipo_comision IS NULL THEN
                           ERROR "TIPO COMISION NO PUEDE SER NULO"
                           ATTRIBUTE (REVERSE)
                           SLEEP 2
                           ERROR " "
                           NEXT FIELD tipo_comision
                        END IF
                     
                     AFTER FIELD val_porcentaje
                        IF g_reg.val_porcentaje IS NULL THEN
                           ERROR "PORCENTAJE NO PUEDE SER NULO"
                           ATTRIBUTE (REVERSE)
                           SLEEP 2
                           ERROR " "
                           NEXT FIELD val_porcentaje
                        END IF

                     AFTER FIELD val_pesos
                        IF g_reg.val_pesos IS NULL THEN
                           ERROR "VALOR EN PESOS NO PUEDE SER NULO"
                           ATTRIBUTE (REVERSE)
                           SLEEP 2
                           ERROR " "
                           NEXT FIELD val_pesos
                        END IF

                     NEXT FIELD val_pesos
 
                       ON KEY (ESC)

                        IF g_reg.tipo_comision IS NULL THEN
                           ERROR "TIPO COMISION NO PUEDE SER NULO"
                           ATTRIBUTE (REVERSE)
                           SLEEP 2
                           ERROR " "
                           NEXT FIELD tipo_comision
                        END IF

                        IF g_reg.val_porcentaje IS NULL THEN
                           ERROR "PORCENTAJE NO PUEDE SER NULO"
                           ATTRIBUTE (REVERSE)
                           SLEEP 2
                           ERROR " "
                           NEXT FIELD val_porcentaje
                        END IF

                        IF g_reg.val_pesos IS NULL THEN
                           ERROR "VALOR EN PESOS NO PUEDE SER NULO"
                           ATTRIBUTE (REVERSE)
                           SLEEP 2
                           ERROR " "
                           NEXT FIELD val_pesos
                        END IF

                        UPDATE dis_val_comision
                            SET (tipo_comision, val_porcentaje,
                                 val_pesos)
                             =  (g_reg.tipo_comision, g_reg.val_porcentaje,
                                 g_reg.val_pesos)
                            WHERE subcuenta  = g_reg.subcuenta
                            ERROR "REGISTRO ACTUALIZADO SATISFACTORIAMENTE"    
                            ATTRIBUTE (REVERSE)
                            SLEEP 2
                            ERROR " "
                            INITIALIZE g_reg.* TO NULL
                            CLEAR FORM
                            RETURN 
      END INPUT
    END IF    
END FUNCTION

################################################################################
FUNCTION  Elimina()
################################################################################
         
	 DISPLAY "" AT 1,1
	 DISPLAY "" AT 1,1
	 DISPLAY " [ Esc ] Elimina         (Ctrl-c) Salir                                          " AT 2,1 ATTRIBUTE(BOLD)
	 DISPLAY " ELIMINA " AT 1,65 

         WHENEVER ERROR STOP
	 
	 LET pos = 2
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 5,3 WITH FORM "TABM0332" ATTRIBUTE( BORDER)
	      DISPLAY " ELIMINA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
	      DISPLAY " [Ctrl-M] Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	      DISPLAY "                           C O M I S I O N E S                                               " AT 2,1 ATTRIBUTE(REVERSE,BOLD) 
   	
	    INPUT BY NAME vcomision
               ATTRIBUTE (REVERSE)

                  AFTER field vcomision
                      IF vcomision IS NULL THEN
                         ERROR "Codigo de comision NO puede ser nulo"
                         ATTRIBUTE (REVERSE)
                         SLEEP 2
                         ERROR " " 
                         NEXT FIELD vcomision
                        ELSE
                         EXIT INPUT
                      END IF
            END INPUT 
            LET vflag = 1
            WHILE vflag = 1 
      
      LET vquery = " SELECT dis_val_comision.tipo_comision, ",
                   " tab_movimiento.descripcion, dis_val_comision.subcuenta, ",
                   " tab_subcuenta.subct_corta, dis_val_comision.val_porcentaje, ",
                   " dis_val_comision.val_pesos ",
                   " FROM dis_val_comision, tab_movimiento, tab_subcuenta ",
                 " WHERE dis_val_comision.tipo_comision = tab_movimiento.codigo ",
          " AND dis_val_comision.subcuenta = tab_subcuenta.subct_cod",
          " AND tab_movimiento.descripcion MATCHES ", '"', vcomision CLIPPED, '"',
                   " ORDER BY 1, 2, 3 " CLIPPED
              PREPARE cur90y FROM vquery
              DECLARE prog_4 cursor FOR cur90y
              LET pos = 1
              FOREACH prog_4 INTO l_record[pos].*
                    LET pos = pos + 1
              END FOREACH
            IF (pos-1) < 1 THEN
                ERROR "NO SE ENCONTRARON REGISTROS CON LAS CONDICIONES"
                ATTRIBUTE (REVERSE)
                SLEEP 2
                ERROR " "
                INITIALIZE vquery TO NULL
                CLOSE WINDOW ventana_2
                RETURN
            END IF 
                CALL SET_COUNT(pos-1)
                DISPLAY ARRAY l_record TO scr_1.*
                    ON KEY ( INTERRUPT)
                       LET pos = 0 
                       EXIT DISPLAY
                    ON KEY ( CONTROL-M )
                       LET pos = ARR_CURR()
                       EXIT DISPLAY
                       END DISPLAY
                       IF pos <> 0 THEN
                    EXIT WHILE
                  END IF
             END WHILE
               LET g_reg.tipo_comision   = l_record[pos].tipo_comision
               LET g_reg.subcuenta       = l_record[pos].subcuenta
               LET g_reg.val_porcentaje  = l_record[pos].val_porcentaje
               LET g_reg.val_pesos       = l_record[pos].val_pesos

               CLOSE WINDOW ventana_2 
               DISPLAY BY NAME g_reg.tipo_comision, g_reg.subcuenta,
                               g_reg.val_porcentaje, g_reg.val_pesos
         
               PROMPT "DESEA ELIMINAR ESTE REGISTRO S/N ? "
               ATTRIBUTE (REVERSE)
               FOR RESP
               ATTRIBUTE (REVERSE)              

               IF RESP MATCHES "[sS]" THEN
                  DELETE FROM dis_val_comision
                       WHERE subcuenta = g_reg.subcuenta
                            
                       ERROR "REGISTRO ELIMINADO SATISFACTORIAMENTE"    
                       ATTRIBUTE (REVERSE)
                       SLEEP 2
                       ERROR " "
                       INITIALIZE g_reg.* TO NULL
                       CLEAR FORM
                       RETURN 
                   ELSE
                       ERROR "OPERACION CANCELADA .."
                       ATTRIBUTE (REVERSE)
                       SLEEP 2
                       ERROR " "
                       INITIALIZE g_reg.* TO NULL
                       CLEAR FORM
                       WHENEVER ERROR STOP
               END IF
    END IF    
END FUNCTION

