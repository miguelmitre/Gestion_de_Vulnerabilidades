##############################################################################
#Project             => SAFRE (Mexico)                                       #
#Owner               => E.F.P.                                               #
#Programa AFIL026    => CONSULTA DE INFORMACION DE NOTIFICACION.             #
#Fecha actualiz.     => 07 ENERO 2003                                        #
#Actualizacion       => FERNANDO HERRERA HERNANDEZ                           #
#Fecha actualiz.     =>                                                      #
#Sistema             => AFI                                                  #
##############################################################################
DATABASE safre_af
GLOBALS
    DEFINE
           HOY                     DATE,
           pos1			   INTEGER,
	   sel_where		   CHAR(1000),
           cla_where		   CHAR(1000),
	   z_reg2		   ARRAY[9] OF SMALLINT,
           z_reg		   RECORD  
				   orden_1     SMALLINT,           
				   orden_2     SMALLINT,           
				   orden_3     SMALLINT,           
				   orden_4     SMALLINT,           
				   orden_5     SMALLINT,
				   orden_6     SMALLINT,
				   orden_7     SMALLINT
    				   END RECORD,
           reg			   RECORD
                                   n_seguro		CHAR(11),
				   paterno_proc		CHAR(40),
				   materno_proc		CHAR(40),
				   nombres_proc		CHAR(40),
				   factualiza		DATE,
				   f_transf_lote	DATE,
 				   status_cta		SMALLINT
       				   END RECORD,
           reg1	 		   ARRAY[5000] OF RECORD
                                   n_seguro		CHAR(11),
				   nom_com_proc		CHAR(120),
				   factualiza		DATE,
				   f_transf_lote	DATE,
				   desc_cta		CHAR(40)
       				   END RECORD,
          vn_seguro		   CHAR(11),
          enter                    CHAR(01),
          usuario                  CHAR(08),
          salir                    SMALLINT,
          g_paramgrales RECORD     LIKE glo_parametro.*

END GLOBALS
############################################################################
MAIN
    DEFER INTERRUPT
    OPTIONS
    PROMPT LINE LAST,
           INPUT WRAP

    CALL STARTLOG('AFIL026.log')

       WHENEVER ERROR CONTINUE

    LET HOY = TODAY
    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIL026c" ATTRIBUTE(BORDER)
    DISPLAY " AFIL026                     A F I L I A C I O N                               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "                   CONSULTA   DE   NOTIFICACION   DE   NSS                     " AT 6,1 ATTRIBUTE(REVERSE)

    MENU "NOMBRE"
         COMMAND "Consultar" "Consulta "           #c
                  CALL consulta()
         COMMAND "Salir" "Salir del Programa"
              EXIT PROGRAM
    END MENU
END MAIN

############################################################################
FUNCTION init()

    LET salir       = 0

    SELECT *, USER
      INTO g_paramgrales.* , usuario        
      FROM glo_parametro

END FUNCTION

############################################################################

FUNCTION consulta()
#c-----------------

    OPTIONS
          ACCEPT KEY ESC,
          INPUT WRAP

    CALL init()
    OPEN WINDOW ventana_2 AT 3,5 WITH FORM "AFIL026a" ATTRIBUTE( BORDER)
    DISPLAY "                         OPCIONES DE CONSULTA                                  " AT 2,1 ATTRIBUTE(REVERSE) 
    DISPLAY "[ ESC ] Grabar   [ Ctrl-C ] Salir" AT 1,1 ATTRIBUTES(BOLD)
    LET pos1 = 2
    IF  (pos1 - 1) >= 1 THEN     
        CALL SET_COUNT(pos1 - 1) 
        LET int_flag = FALSE
        CONSTRUCT BY NAME cla_where ON n_seguro, paterno_proc, materno_proc, 
                                       nombres_proc, factualiza, f_transf_lote, 
                                       status_cta

        IF  int_flag = TRUE THEN          
            LET int_flag = FALSE          
            ERROR "BUSQUEDA CANCELADA..." 
            SLEEP 2                       
            ERROR ""                      
            CLOSE WINDOW ventana_2        
	    RETURN 
        END IF
        CALL ordena() #o
	CLEAR FORM

        LET z_reg2[1] = z_reg.orden_1
        LET z_reg2[2] = z_reg.orden_2
        LET z_reg2[3] = z_reg.orden_3
        LET z_reg2[4] = z_reg.orden_4
        LET z_reg2[5] = z_reg.orden_5
        LET z_reg2[6] = z_reg.orden_6
	LET z_reg2[7] = z_reg.orden_7
        
        LET sel_where = " SELECT n_seguro, paterno_proc, materno_proc,", 
			" nombres_proc, factualiza, ", 
                        " f_transf_lote, status_cta",
                        " FROM afi_det_notifica",
			" WHERE ", cla_where CLIPPED ,
                        " ORDER BY ", z_reg.orden_1,",",z_reg.orden_2,",",
                                      z_reg.orden_3,",",z_reg.orden_4,",",
                                      z_reg.orden_5,",",z_reg.orden_6,",",
				      z_reg.orden_7

        LET sel_where = sel_where CLIPPED

        PREPARE qry_modif FROM sel_where 
        DECLARE cursor_m CURSOR FOR qry_modif
        LET pos1 = 1
        FOREACH cursor_m INTO reg.n_seguro,
			      reg.paterno_proc, 
			      reg.materno_proc,
			      reg.nombres_proc,
			      reg.factualiza,
			      reg.f_transf_lote,
			      reg.status_cta

            LET reg1[pos1].n_seguro      = reg.n_seguro
            LET reg1[pos1].nom_com_proc  = reg.nombres_proc CLIPPED, " ",
                                           reg.paterno_proc CLIPPED, " ",
                                           reg.materno_proc CLIPPED
            LET reg1[pos1].factualiza    = reg.factualiza
            LET reg1[pos1].f_transf_lote = reg.f_transf_lote

            CASE reg.status_cta
               WHEN 0
                 LET reg1[pos1].desc_cta      = "MARCA"      CLIPPED
               WHEN 1
                 LET reg1[pos1].desc_cta      = "MARCA"      CLIPPED
               WHEN 2
                 LET reg1[pos1].desc_cta      = "DESMARCA"   CLIPPED
               WHEN 9
		 LET reg1[pos1].desc_cta      = "NO MARCADO" CLIPPED
               OTHERWISE
                 LET reg1[pos1].desc_cta      = "MARCA"      CLIPPED

            END CASE
            LET vn_seguro = reg.n_seguro
            LET pos1      = pos1 + 1

            IF pos1 > 4999 THEN
               ERROR "Capacidad de presentacion de informacion insuficiente."
               SLEEP 2
               CLOSE WINDOW ventana_2        
               RETURN
            END IF    
        END FOREACH                          
        CLOSE WINDOW ventana_2              
        INITIALIZE reg1[pos1].* TO NULL

        IF  (pos1 - 1) >= 1 THEN                 
            CALL  SET_COUNT(pos1 - 1)            

            ERROR ""                     
            OPEN WINDOW ventana_10 AT 3,2 WITH FORM "AFIL026c"
               ATTRIBUTE( BORDER)
               DISPLAY "" AT 1,1
               DISPLAY "" AT 2,1
               DISPLAY "[ Ctrl-C ] Salir                        " AT 2,1 
               ATTRIBUTE(BOLD)
               DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
               DISPLAY ARRAY reg1 TO scr_1.*
               ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")
                  ON KEY ( INTERRUPT )
                     EXIT DISPLAY

                  ON KEY ( CONTROL - M )
                     LET pos1 = ARR_CURR()

               END DISPLAY 

          CLOSE WINDOW ventana_10

          IF vn_seguro IS NULL  OR
             vn_seguro = 0      THEN
             ERROR "  Informacion inexistente" ATTRIBUTE(NORMAL)
             LET salir = 3 
          ELSE
             LET salir = 1
          END IF

          CASE salir 
             WHEN 1
             WHEN 3
                PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR" 
                FOR CHAR enter
          END CASE
        ELSE                                   
            ERROR "NO EXISTE INFORMACION. . ."
            SLEEP 2                        
            ERROR ""                            
	    RETURN
        END IF                                 
    END IF

END FUNCTION

############################################################################
FUNCTION ordena()
#o--------------
    OPEN WINDOW ventana_3 AT 3,5 WITH FORM "AFIL026b" ATTRIBUTE(BORDER)
    DISPLAY " [ Esc ] Grabar      [ Ctrl-C ] Salir"
             AT 1,1 ATTRIBUTE(BOLD) 
    DISPLAY "                      OPCIONES DE ORDENAMIENTO                                        " AT 2,1 ATTRIBUTE(REVERSE,BOLD)
    LET z_reg.orden_1 = 1
    LET z_reg.orden_2 = 2
    LET z_reg.orden_3 = 3
    LET z_reg.orden_4 = 4
    LET z_reg.orden_5 = 5
    LET z_reg.orden_6 = 6
    LET z_reg.orden_7 = 7
    DISPLAY BY NAME z_reg.*
    INPUT BY NAME z_reg.* WITHOUT DEFAULTS
        AFTER FIELD orden_1
            IF  z_reg.orden_1 IS NULL THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD orden_1
            ELSE
                IF  z_reg.orden_1 < 1 OR z_reg.orden_1 > 7 THEN
                    ERROR "La opcion de orden digitada no existe...Reingrese"
                    NEXT FIELD orden_1
                END IF
            END IF
        AFTER FIELD orden_2
            IF  z_reg.orden_2 IS NULL THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD orden_2
            ELSE
               IF  z_reg.orden_2 < 1 OR z_reg.orden_2 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_2
               END IF
               IF  z_reg.orden_2 = z_reg.orden_1 THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_2
               END IF
           END IF
       AFTER FIELD orden_3
           IF  z_reg.orden_3 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_3
           ELSE
               IF  z_reg.orden_3 < 1 OR z_reg.orden_3 > 7 THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_3
               END IF
               IF  (z_reg.orden_3 = z_reg.orden_1)
                   OR (z_reg.orden_3 = z_reg.orden_2) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_3
               END IF
           END IF
       AFTER FIELD orden_4
           IF  z_reg.orden_4 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_4
           ELSE
               IF  z_reg.orden_4 < 1 OR z_reg.orden_4 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_4
               END IF
               IF  (z_reg.orden_4 = z_reg.orden_1)
                   OR (z_reg.orden_4 = z_reg.orden_2)
                   OR (z_reg.orden_4 = z_reg.orden_3) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_4
               END IF
           END IF
       AFTER FIELD orden_5
           IF  z_reg.orden_5 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_5
           ELSE
               IF  z_reg.orden_5 < 1 OR z_reg.orden_5 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_5
               END IF
               IF  (z_reg.orden_5 = z_reg.orden_1)
                   OR (z_reg.orden_5 = z_reg.orden_2)
                   OR (z_reg.orden_5 = z_reg.orden_3) 
                   OR (z_reg.orden_5 = z_reg.orden_4) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_5
               END IF
           END IF

       AFTER FIELD orden_6
           IF  z_reg.orden_6 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_6
           ELSE
               IF  z_reg.orden_6 < 1 OR z_reg.orden_6 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_6
               END IF
               IF  (z_reg.orden_6 = z_reg.orden_1)
                   OR (z_reg.orden_6 = z_reg.orden_2)
                   OR (z_reg.orden_6 = z_reg.orden_3) 
                   OR (z_reg.orden_6 = z_reg.orden_4) 
                   OR (z_reg.orden_6 = z_reg.orden_5) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_6
               END IF
           END IF

       AFTER FIELD orden_7
           IF  z_reg.orden_7 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_7
           ELSE
               IF  z_reg.orden_7 < 1 OR z_reg.orden_7 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_7
               END IF
               IF  (z_reg.orden_7 = z_reg.orden_1)
                   OR (z_reg.orden_7 = z_reg.orden_2)
                   OR (z_reg.orden_7 = z_reg.orden_3) 
                   OR (z_reg.orden_7 = z_reg.orden_4) 
                   OR (z_reg.orden_7 = z_reg.orden_5) 
                   OR (z_reg.orden_7 = z_reg.orden_6) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_7
               END IF
           END IF

       ON KEY ( ESC )
           IF  z_reg.orden_1 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_1
           ELSE
               IF  z_reg.orden_1 < 1 OR z_reg.orden_1 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_1
               END IF
           END IF
           IF  z_reg.orden_2 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_2
           ELSE
               IF  z_reg.orden_2 < 1 OR z_reg.orden_2 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_2
               END IF
               IF  z_reg.orden_2 = z_reg.orden_1 THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_2
               END IF
           END IF
           IF  z_reg.orden_3 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_3
           ELSE
               IF  z_reg.orden_3 < 1 OR z_reg.orden_3 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_3
               END IF
               IF  (z_reg.orden_3 = z_reg.orden_1)
                   OR (z_reg.orden_3 = z_reg.orden_2) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_3
               END IF
           END IF
           IF  z_reg.orden_4 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_4
           ELSE
               IF  z_reg.orden_4 < 1 OR z_reg.orden_4 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_4
               END IF
               IF  (z_reg.orden_4 = z_reg.orden_1)
                   OR (z_reg.orden_4 = z_reg.orden_2)
                   OR (z_reg.orden_4 = z_reg.orden_3) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_4
               END IF
           END IF
           IF  z_reg.orden_5 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_5
           ELSE
               IF  z_reg.orden_5 < 1 OR z_reg.orden_5 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_5
               END IF
               IF  (z_reg.orden_5 = z_reg.orden_1)
                   OR (z_reg.orden_5 = z_reg.orden_2)
                   OR (z_reg.orden_5 = z_reg.orden_3) 
                   OR (z_reg.orden_5 = z_reg.orden_4) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_5
               END IF
           END IF

           IF  z_reg.orden_6 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_6
           ELSE
               IF  z_reg.orden_6 < 1 OR z_reg.orden_6 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_6
               END IF
               IF  (z_reg.orden_6 = z_reg.orden_1)
                   OR (z_reg.orden_6 = z_reg.orden_2)
                   OR (z_reg.orden_6 = z_reg.orden_3) 
                   OR (z_reg.orden_6 = z_reg.orden_4) 
                   OR (z_reg.orden_6 = z_reg.orden_5) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_6
               END IF
           END IF

           IF  z_reg.orden_7 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_7
           ELSE
               IF  z_reg.orden_7 < 1 OR z_reg.orden_7 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_7
               END IF
               IF  (z_reg.orden_7 = z_reg.orden_1)
                   OR (z_reg.orden_7 = z_reg.orden_2)
                   OR (z_reg.orden_7 = z_reg.orden_3) 
                   OR (z_reg.orden_7 = z_reg.orden_4) 
                   OR (z_reg.orden_7 = z_reg.orden_5) 
                   OR (z_reg.orden_7 = z_reg.orden_6) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_7
               END IF
           END IF

       ON KEY ( INTERRUPT )
           EXIT INPUT
    END INPUT
    CLOSE WINDOW ventana_3
    RETURN
END FUNCTION
############################################################################
