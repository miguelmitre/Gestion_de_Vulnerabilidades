#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa TABM0001 => CATALOGO ARCHIVO DE CIUDADES.                         #
#Fecha             => 7 Mayo 1997.        				    #
#Por               => GERARDO ALFONSO VEGA PAREDES.			    #
#Fecha Modificacion=> 13 DE OCTUBRE 1999.                                   #
#Modificado        => MIGUEL ANGEL HERNANDEZ MARTINEZ.                      #
#Sistema           => TAB. 					            #
#############################################################################
DATABASE safre_af
GLOBALS
        DEFINE g_param_dis   RECORD LIKE seg_modulo.*

        DEFINE g_reg RECORD 
                   estad_cod       INTEGER,
                   ciudad_cod        INTEGER,
                   ciudad_desc       CHAR(40)
	END RECORD

        DEFINE l_record   ARRAY[1000] OF RECORD
                   cod            INTEGER,
                   codigo         INTEGER,
                   descripcion    CHAR(40)
        END RECORD

        DEFINE     aux_estad_desc  CHAR(40),
                   aux_pausa       CHAR(01),
                   x_x             CHAR(150),
                   x_buscar        CHAR(30),
                   hoy             DATE,
                   sw_1            SMALLINT,
                   seg_usuario         CHAR(08),
                   siono           CHAR(01),
                   pos             INTEGER,
                   cla_where       CHAR(300),
                   sel_where       CHAR(300),
                   g_lista         CHAR(300),
                   g_impre         CHAR(300),
                   hora            CHAR(08)


END GLOBALS
#####################################################################
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT
       
        CALL inicio()
        CALL proceso()
END MAIN
#####################################################################
FUNCTION inicio()
        SELECT USER,*
        INTO   seg_usuario
        FROM glo_parametro

        SELECT ruta_listados
        INTO g_param_dis.ruta_listados
        FROM seg_modulo
        WHERE modulo_cod = 'tab'
END FUNCTION
#####################################################################
FUNCTION proceso()
	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0011" ATTRIBUTE( BORDER)
	DISPLAY " TABM001           CATALOGO DE LOCALIDADES / CIUDADES                          " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "CATALOGO CIUDAD"
                          COMMAND "Agrega" "Agrega Ciudades"
		                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Ciudades"
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Ciudades"
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Ciudades"
		                     CALL Elimina()
                           COMMAND "Salir" "Salir del Programa"
		                     EXIT MENU
	END MENU

        CLOSE WINDOW ventana_1
END FUNCTION
#####################################################################
FUNCTION Inicializa()
        LET sw_1 = 0
	INITIALIZE g_reg.* TO NULL
	DISPLAY BY NAME g_reg.*
        LET aux_estad_desc = null
        DISPLAY BY NAME aux_estad_desc
END FUNCTION
#####################################################################
FUNCTION Agrega()

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ( Esc ) Agrega                   (Ctrl-c) Salir                               " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

        LET g_reg.ciudad_desc = NULL
        LET sw_1 = 0

	INPUT BY NAME  g_reg.*
             AFTER  FIELD estad_cod
                IF g_reg.estad_cod IS NULL THEN
                   CALL Despliega_estado() 
                        RETURNING g_reg.estad_cod,aux_estad_desc
                ELSE
                   SELECT estad_cod,estad_desc
                   INTO   g_reg.estad_cod,aux_estad_desc
                   FROM   tab_estado 
                   WHERE  estad_cod = g_reg.estad_cod

                   IF STATUS = NOTFOUND THEN
                       ERROR "No existe esta entidad"
                       NEXT FIELD estad_cod
		   END IF
	        END IF
                DISPLAY BY NAME aux_estad_desc
	      BEFORE FIELD ciudad_cod
                 IF sw_1 = 0 THEN
                     LET sw_1 = 1
	             SELECT MAX(ciudad_cod) 
                     INTO g_reg.ciudad_cod 
                     FROM tab_ciudad
                     IF g_reg.ciudad_cod = 0 OR g_reg.ciudad_cod IS NULL THEN
	                LET g_reg.ciudad_cod = 1
	             ELSE
			LET g_reg.ciudad_cod = g_reg.ciudad_cod + 1
		     END IF
                     DISPLAY BY NAME g_reg.*
                 END IF
                 DISPLAY BY NAME g_reg.*
	      AFTER FIELD ciudad_cod
		    IF g_reg.ciudad_cod IS NULL THEN
		       ERROR "Codigo de Ciudad NO puede ser nulo"
		       NEXT FIELD  ciudad_cod
		    END IF
		    
                    SELECT "X" 
                    FROM tab_ciudad
		    WHERE ciudad_cod = g_reg.ciudad_cod

		    IF STATUS <> NOTFOUND THEN
		       ERROR "Ciudad ya Ingresada"
		       NEXT FIELD ciudad_cod
		    END IF

	      BEFORE FIELD ciudad_desc
		     IF g_reg.ciudad_cod IS NULL OR  g_reg.ciudad_cod = 0 THEN
		        ERROR "Codigo de Ciudad NO puede ser nulo"
			NEXT FIELD  ciudad_cod
		     END IF 

              AFTER FIELD ciudad_desc
		     IF g_reg.ciudad_desc IS NULL THEN
		        ERROR "Descripcion de Ciudad NO puede ser nula"
		        NEXT FIELD  ciudad_desc
		     END IF 

		     SELECT "X" 
                     FROM tab_ciudad
		     WHERE ciudad_desc = g_reg.ciudad_desc and
                           ciudad_cod  = g_reg.ciudad_cod
		     IF STATUS <> NOTFOUND THEN
		        ERROR "Ciudad ya Ingresada"
		        NEXT FIELD ciudad_cod
		     END IF

	      ON KEY ( ESC )
		     IF g_reg.ciudad_cod IS NULL THEN
		        ERROR "Codigo de Ciudad NO puede ser NULO"
		        NEXT FIELD ciudad_cod
		     END IF
		     IF g_reg.ciudad_desc IS NULL THEN
		        ERROR "Descripcion de Ciudad NO puede ser NULO"
                        NEXT FIELD ciudad_desc
		     END IF

		     SELECT "X" 
                     FROM   tab_ciudad
		     WHERE ciudad_desc = g_reg.ciudad_desc and
                           ciudad_cod  = g_reg.ciudad_cod

		     IF STATUS <> NOTFOUND THEN
		        ERROR "Ciudad ya Ingresada"
		        NEXT FIELD ciudad_cod
		     END IF

                     INSERT INTO tab_ciudad VALUES ( g_reg.* ) 
		     ERROR "REGISTRO INGRESADO" SLEEP 1
		     ERROR ""
                     CALL Inicializa()
		     NEXT FIELD estad_cod
                    ON KEY (INTERRUPT)
                       CALL Inicializa()
                       EXIT INPUT
               END INPUT
               CLEAR SCREEN
END FUNCTION
############################################################
FUNCTION Consulta()
    LET pos = 2
    IF (pos-1) >= 1 THEN
       CALL SET_COUNT(pos-1)
       OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0012" ATTRIBUTE(BORDER) 
       DISPLAY " (Enter) Consulta              (Ctrl-C) Salir             (Ctrl-P) Imprimir   " AT 1,1 ATTRIBUTE(REVERSE)

       DISPLAY "                           LOCALIDADES / CIUDADES                              " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

       LET int_flag = FALSE

       CONSTRUCT cla_where ON estad_cod,ciudad_cod FROM estad_cod,ciudad_cod
          ON KEY (control-m)
             LET int_flag = FALSE
             EXIT CONSTRUCT     
          ON KEY (control-c)
             LET int_flag = TRUE
             EXIT CONSTRUCT
       END CONSTRUCT

       IF int_flag = TRUE THEN
          LET int_flag = FALSE
          ERROR "BUSQUEDA CANCELADA..."
          SLEEP 2
          ERROR ""
          CLEAR SCREEN
          CLOSE WINDOW ventana_2
          RETURN
       END IF

       LET sel_where = "SELECT * FROM tab_ciudad WHERE ",cla_where CLIPPED,
                       "ORDER BY 1,2 "

       PREPARE query FROM sel_where
    
       DECLARE cursor_1 CURSOR FOR query     

       LET pos = 1

       FOREACH cursor_1 INTO l_record[pos].*
          LET pos = pos + 1
       END FOREACH
   
       INITIALIZE l_record[pos].* TO NULL

       IF (pos-1) >= 1 THEN
          CALL SET_COUNT(pos-1)
          DISPLAY ARRAY l_record TO scr_1.*
             ON KEY (control-p)
                ERROR "PROCESANDO IMPRESION..."
                CALL Impresion(pos)
             ON KEY (INTERRUPT)
                EXIT DISPLAY
          END DISPLAY
          CLOSE WINDOW ventana_2
       ELSE
          ERROR "REGISTROS DE CIUDAD .... NO EXISTE"
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
       END IF
    END IF
    CLEAR SCREEN
END FUNCTION
#####################################################################
FUNCTION  Modifica()

   LET pos = 2
    
   IF (POS-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
        OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0012" ATTRIBUTE(BORDER) 
        DISPLAY " (Enter) Consulta                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
        DISPLAY "                 Escoja con < ENTER > el registro a modificar                  " AT 2,1           
        DISPLAY "                           LOCALIDADES / CIUDADES                              " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

       LET int_flag = FALSE

       CONSTRUCT cla_where ON estad_cod,ciudad_cod FROM estad_cod,ciudad_cod
          ON KEY (control-m)
             LET int_flag = FALSE
             EXIT CONSTRUCT     
          ON KEY (control-c)
             LET int_flag = TRUE
             EXIT CONSTRUCT
       END CONSTRUCT

       IF int_flag = TRUE THEN
          LET int_flag = FALSE
          ERROR "BUSQUEDA CANCELADA..."
          SLEEP 2
          ERROR ""
          CLEAR SCREEN
          CLOSE WINDOW ventana_2
          RETURN
       END IF

       LET sel_where = "SELECT * FROM tab_ciudad WHERE ",cla_where CLIPPED,
                       "ORDER BY 1,2 "

       PREPARE query1 FROM sel_where
    
       DECLARE cursor_2 CURSOR FOR query1     

       LET pos = 1

       FOREACH cursor_2 INTO l_record[pos].*
          LET pos = pos + 1
       END FOREACH
   
       INITIALIZE l_record[pos].* TO NULL

       IF (pos-1) >= 1 THEN
	  CALL  SET_COUNT(pos-1)
	  DISPLAY ARRAY l_record TO scr_1.* 
             ON KEY (CONTROL-M)
                LET pos = ARR_CURR()
                LET g_reg.estad_cod = l_record[pos].cod
	        LET g_reg.ciudad_cod  = l_record[pos].codigo
                LET g_reg.ciudad_desc = l_record[pos].descripcion

                SELECT estad_desc 
                INTO aux_estad_desc
                FROM tab_estado
                WHERE estad_cod = g_reg.estad_cod

                EXIT DISPLAY
             ON KEY (INTERRUPT)
	        ERROR "Usted debe escojer un registro"
                      LET pos = ARR_CURR()
	   END DISPLAY

	   CLOSE WINDOW ventana_2
       ELSE
          ERROR "REGISTRO DE CIUDAD .... NO EXISTE"
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
          RETURN
       END IF 

       DISPLAY "" AT 1,1
       DISPLAY "" AT 2,1
       DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
       DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

       INPUT BY NAME  g_reg.* WITHOUT DEFAULTS 
          BEFORE FIELD estad_cod
             DISPLAY BY NAME aux_estad_desc
             NEXT FIELD ciudad_desc

          AFTER FIELD ciudad_desc
             IF g_reg.ciudad_desc IS NULL THEN
                ERROR "Descripcion de Ciudad NO puede ser nula"
                NEXT FIELD  ciudad_desc
             END IF 
      
           CALL pregunta()
      
           IF aux_pausa MATCHES "[Ss]" THEN
              UPDATE tab_ciudad SET
                     ciudad_desc = g_reg.ciudad_desc
              WHERE ciudad_cod = g_reg.ciudad_cod

	      ERROR "REGISTRO MODIFICADO" SLEEP 1
	      ERROR ""
              CALL Inicializa()
           ELSE
              ERROR "PROCESO DE MODIFICACION,CANCELADO..."
              SLEEP 2
           END IF

           ERROR ""
           EXIT INPUT
        ON KEY (INTERRUPT)
           CALL inicializa()
           EXIT INPUT
        END INPUT
    ELSE
       ERROR "PROCESO DE MODIFICACION,CANCELADO..."
    END IF
    CLEAR SCREEN
END FUNCTION
######################################################################
FUNCTION Elimina()

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0012" ATTRIBUTE(BORDER) 
      DISPLAY " (Enter) Consulta                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                 Escoja con < ENTER > el registro a eleminar                  " AT 2,1           
      DISPLAY "                           LOCALIDADES / CIUDADES                              " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON estad_cod,ciudad_cod FROM estad_cod,ciudad_cod
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT     
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT * FROM tab_ciudad WHERE ",cla_where CLIPPED,
                       "ORDER BY 1,2 "

      PREPARE query2 FROM sel_where
    
      DECLARE cursor_3 CURSOR FOR query2     

      LET pos = 1

      FOREACH cursor_3 INTO l_record[pos].*
          LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL
   
      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.* 
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.estad_cod = l_record[pos].cod
               LET g_reg.ciudad_cod = l_record[pos].codigo
               LET g_reg.ciudad_desc = l_record[pos].descripcion

               SELECT estad_desc 
               INTO aux_estad_desc
               FROM tab_estado
               WHERE estad_cod = g_reg.estad_cod

               EXIT DISPLAY
            ON KEY (INTERRUPT)
	       ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
          END DISPLAY
          CLOSE WINDOW ventana_2
       ELSE
          ERROR "REGISTROS DE CIUDADES .... NO EXISTE"
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
          RETURN
       END IF

       DISPLAY "" AT 1,1
       DISPLAY "" AT 2,1
       DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
       DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

       DISPLAY BY NAME  g_reg.*
       DISPLAY BY NAME aux_estad_desc
          CALL Pregunta()

          IF aux_pausa MATCHES "[Ss]" THEN
             DELETE FROM tab_ciudad
             WHERE ciudad_cod = g_reg.ciudad_cod

             ERROR "REGISTRO ELIMINADO" 
             SLEEP 2
          ELSE
             ERROR "ELIMINAR CANCELADO" 
             SLEEP 2
          END IF

          ERROR ""
           CALL Inicializa()
      ELSE
	   ERROR "ARCHIVO DE CIUDADES.... VACIO"
           SLEEP 2
      END IF
      CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION Despliega_estado()
      DEFINE aux_val          SMALLINT
      DEFINE x_x CHAR(150)
      DEFINE x_buscar CHAR(30)
      DEFINE l_reg ARRAY[500] OF RECORD
               codigo           INTEGER,
               descripcion      CHAR(50)
      END RECORD
      DEFINE pos              SMALLINT
      OPEN WINDOW ventana_3 AT 6,3 WITH FORM "TABM0013" ATTRIBUTE(BORDER) 
      DISPLAY "                 Escoja con < ENTER > el registro a ingresar                  " AT 1,1           
      DISPLAY "                             E N T I D A D E S                                 " AT 2,1 ATTRIBUTE(REVERSE)

       LET int_flag = FALSE

       CONSTRUCT x_buscar ON estad_cod,estad_desc FROM estad_cod,estad_desc
          ON KEY (control-m)
             LET int_flag = FALSE
             EXIT CONSTRUCT     
          ON KEY (control-c)
             LET int_flag = TRUE
             EXIT CONSTRUCT
       END CONSTRUCT

       IF int_flag = TRUE THEN
          LET int_flag = FALSE
          ERROR "BUSQUEDA CANCELADA..."
          SLEEP 2
          ERROR ""
          CLEAR SCREEN
          CLOSE WINDOW ventana_3
          RETURN
       END IF

        WHILE TRUE
              LET x_x = " SELECT estad_cod,estad_desc FROM tab_estado ",
                        " WHERE ",x_buscar CLIPPED,
                        " ORDER BY 1 " CLIPPED
              PREPARE cur20y FROM x_x
              DECLARE cur_2y CURSOR FOR cur20y
              LET pos = 1
              FOREACH cur_2y INTO l_reg[pos].*
                      LET pos = pos + 1
              END FOREACH
              IF (pos-1) < 1 THEN
                 ERROR "ARCHIVO ENTIDADES..... VACIO"
              END IF
              CALL SET_COUNT(pos-1)
              DISPLAY ARRAY l_reg TO scr_1.*
                      ON KEY ( INTERRUPT )
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
        CLOSE WINDOW ventana_3
        RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
#####################################################################
FUNCTION Impresion(pos)
    DEFINE i,pos INTEGER
 
    LET hora = TIME

    LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",seg_usuario CLIPPED,
                  ".IMPCIUDAD",hoy USING "dd-mm-yyyy","_",hora CLIPPED

    START REPORT rpt_tabcity TO g_impre

    FOR i=1 TO (pos+1)
        LET g_reg.estad_cod         = l_record[i].cod
        LET g_reg.ciudad_cod          = l_record[i].codigo
        LET g_reg.ciudad_desc         = l_record[i].descripcion
  
        IF g_reg.estad_cod IS NULL THEN
           EXIT FOR
        END IF
  
        OUTPUT TO REPORT rpt_tabcity(g_reg.*)
    END FOR

    FINISH REPORT rpt_tabcity

    ERROR "LISTADO GENERADO..."
    SLEEP 2
    ERROR ""
  
    LET g_lista = "lp ",g_impre
    RUN g_lista
END FUNCTION

REPORT rpt_tabcity(g_reg)
    DEFINE g_reg RECORD 
           estad_cod       INTEGER,
           ciudad_cod        INTEGER,
           ciudad_desc       CHAR(40)
    END RECORD

    OUTPUT
       TOP MARGIN 1
       BOTTOM MARGIN 0
       LEFT MARGIN 0 
       RIGHT MARGIN 0
       PAGE LENGTH 60
    FORMAT
    PAGE HEADER
       PRINT COLUMN 02," TABM001 ",
             COLUMN 24," LISTADO DE CATALOGOS DE CIUADADES ",
             COLUMN 67, TODAY USING "dd-mm-yyyy"
       SKIP 2 LINE 

       PRINT COLUMN 01,"CODIGO DE ESTADO",
             COLUMN 18,"CODIGO DE CIUDAD",
             COLUMN 36,"DESCRIPCION DE CIUDAD"
       SKIP 1 LINE

     ON EVERY ROW
       PRINT COLUMN 05,g_reg.estad_cod USING "######",
             COLUMN 24,g_reg.ciudad_cod  USING "######",
             COLUMN 37,g_reg.ciudad_desc
    PAGE TRAILER
       SKIP 2 LINE
       PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
    ON LAST ROW
       SKIP 2 LINE
       PRINT COLUMN 02,"Total de registros : ",COUNT(*) USING "<<<<<"
END REPORT

