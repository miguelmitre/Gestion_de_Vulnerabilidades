########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )		       #
#Owner             => E.F.P.                                           #
#Programa TABM002  => CATALOGO ARCHIVO DELEGACIONES                    #
#Fecha             =>  7 Mayo 1997.      			       #
#By                => GERARDO ALFONSO VEGA PAREDES.		       #
#Fecha Modifica.   => 22 Octubre 1999.                                 #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ.                 #
#Sistema           => TAB. 					       #
########################################################################
DATABASE safre_af
GLOBALS
        DEFINE g_param_dis          RECORD LIKE seg_modulo.*

        DEFINE g_reg  RECORD 
           estad_cod		INTEGER,
           estad_desc           CHAR(40),
  	   deleg_cod		INTEGER,
	   deleg_desc	  	CHAR(40)
	END RECORD

        DEFINE l_reg ARRAY[500] OF RECORD
               estad_cod           INTEGER,
               estad_desc      CHAR(50)
        END RECORD

        DEFINE l_record   ARRAY[5000] OF RECORD
               cod            INTEGER,
               codigo         INTEGER,
               descripcion  CHAR(50)
        END RECORD

	DEFINE HOY		DATE,
               aux_pausa        CHAR(01),
               sw_1             INTEGER,
               cla_where        CHAR(200),
               sel_where        CHAR(200),
               seg_usuario          CHAR(08),
               pos              INTEGER,
               g_lista          CHAR(500),
               g_impre          CHAR(500) 

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
        INTO   g_param_dis.ruta_listados
        FROM   seg_modulo
        WHERE modulo_cod = 'tab'
END FUNCTION
#####################################################################
FUNCTION proceso()
	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0021" ATTRIBUTE( BORDER)
	DISPLAY " TABM002          CATALOGO DE DELEGACIONES / MUNICIPIOS                        " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "CATALOGO DELEG/MUNIC"
		COMMAND "Agrega" "Agrega Delegaciones"
		                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Delegaciones"
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Delegaciones"
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Delegaciones"
		                     CALL Elimina()
                           COMMAND "Salir" "Salir del Programa"
		                     EXIT MENU
	END MENU
        CLOSE WINDOW ventana_1
END FUNCTION
################################################################################
FUNCTION Inicializa()
        LET sw_1 = 0
	INITIALIZE g_reg.* TO NULL
	DISPLAY BY NAME g_reg.*
        LET g_reg.estad_desc = NULL
        DISPLAY BY NAME g_reg.estad_desc
END FUNCTION
################################################################################
FUNCTION Agrega()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ( Esc ) Agrega                   (Ctrl-c) Salir                               " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
        LET g_reg.deleg_desc = NULL
  
        LET sw_1 = 0
        INPUT BY NAME  g_reg.estad_cod, g_reg.deleg_cod, g_reg.deleg_desc
     
        AFTER  FIELD estad_cod
           IF g_reg.estad_cod IS NULL THEN
              CALL Despliega_estado() 
              RETURNING g_reg.estad_cod,g_reg.estad_desc
           ELSE
              SELECT estad_cod,estad_desc 
              INTO g_reg.estad_cod,g_reg.estad_desc
              FROM tab_estado 
              WHERE estad_cod = g_reg.estad_cod
              IF STATUS = NOTFOUND THEN
                 ERROR "No existe esta entidad"
                 NEXT FIELD estad_cod
	      END IF
	   END IF

           DISPLAY BY NAME g_reg.estad_desc
	BEFORE FIELD deleg_cod
           IF sw_1 = 0 THEN
              LET sw_1 = 1
	      SELECT MAX(deleg_cod) 
              INTO g_reg.deleg_cod 
              FROM tab_delegacion
              IF g_reg.deleg_cod = 0 OR g_reg.deleg_cod IS NULL THEN
	         LET g_reg.deleg_cod = 1
	      ELSE
	         LET g_reg.deleg_cod = g_reg.deleg_cod + 1
	      END IF
           END IF
           DISPLAY BY NAME g_reg.*
	AFTER FIELD deleg_cod
	   IF g_reg.deleg_cod IS NULL THEN
	      ERROR "Codigo de Delegacion NO puede ser nulo"
	      NEXT FIELD  deleg_cod
	   END IF

	   SELECT "X" 
           FROM tab_delegacion
	   WHERE deleg_cod = g_reg.deleg_cod

	   IF STATUS <> NOTFOUND THEN
	      ERROR "Delegacion ya Ingresada"
	      NEXT FIELD deleg_cod
	   END IF

	BEFORE FIELD deleg_desc
	    IF g_reg.deleg_cod IS NULL OR  g_reg.deleg_cod = 0 THEN
	       ERROR "Codigo de Delegacion NO puede ser nulo"
	       NEXT FIELD  deleg_cod
	    END IF 
        AFTER FIELD deleg_desc
	    IF g_reg.deleg_desc IS NULL THEN
	       ERROR "Descripcion de Delegacion NO puede ser nula"
	       NEXT FIELD  deleg_desc
	    END IF 

	    SELECT "X" 
            FROM tab_delegacion
	    WHERE deleg_desc = g_reg.deleg_desc

            IF STATUS <> NOTFOUND THEN
	       ERROR "Delegacion ya Ingresada"
	       NEXT FIELD deleg_cod
	    END IF
	ON KEY ( ESC )
	   IF g_reg.deleg_cod IS NULL THEN
              ERROR "Codigo de Delegacion NO puede ser NULO"
              NEXT FIELD deleg_cod
           END IF

           IF g_reg.deleg_desc IS NULL THEN
              ERROR "Descripcion de Delegacion NO puede ser NULO"
              NEXT FIELD deleg_desc
           END IF

           SELECT "X" 
           FROM tab_delegacion
           WHERE deleg_desc = g_reg.deleg_desc

           IF STATUS <> NOTFOUND THEN
              ERROR "Delegacion ya Ingresada"
              NEXT FIELD deleg_cod
           END IF

           INSERT INTO tab_delegacion VALUES ( g_reg.estad_cod,g_reg.deleg_cod,
                                         g_reg.deleg_desc) 

           ERROR "REGISTRO INGRESADO" 
           SLEEP 2
	   ERROR ""
           CALL Inicializa()

           NEXT FIELD estad_cod
        ON KEY (INTERRUPT)
           CALL Inicializa()
           EXIT INPUT

           ERROR ""
           CLEAR SCREEN
	END INPUT
        ERROR ""
        CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Consulta()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      OPEN WINDOW vent_1 AT 6,3 WITH FORM "TABM0022" ATTRIBUTE(BORDER)
      DISPLAY " (Enter) Consulta              (Ctrl-c) Salir            (Ctrl-p) Impresion    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                         DELEGACIONES / MUNICIPIOS                             " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON estad_cod,deleg_cod FROM estad_cod,deleg_cod
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
         CLOSE WINDOW vent_1
         RETURN
      END IF

      LET sel_where = " SELECT * FROM tab_delegacion WHERE ",cla_where CLIPPED,
                      " ORDER BY 2 " CLIPPED

      PREPARE curdel1 FROM sel_where

      DECLARE cursor_1 CURSOR FOR curdel1

      LET pos = 1
      FOREACH cursor_1 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (control-p)
               ERROR "PROCESANDO IMPRESION..."
               CALL impresion(pos)
            ON KEY (INTERRUPT)
               EXIT DISPLAY
	 END DISPLAY
         CLOSE WINDOW vent_1
      ELSE
	 ERROR "ARCHIVO DE DELEGACION NO ENCONTRADO"
         SLEEP 2
         ERROR ""  
         CLOSE WINDOW vent_1
      END IF
    END IF
    CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION  Modifica()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      OPEN WINDOW vent_1 AT 6,3 WITH FORM "TABM0022" ATTRIBUTE(BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                Escoja con < ENTER > la ciudad a modificar                     " AT 2,1 
      DISPLAY "                         DELEGACIONES / MUNICIPIOS                             " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON estad_cod,deleg_cod FROM estad_cod,deleg_cod
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
         CLOSE WINDOW vent_1
         RETURN
      END IF

      LET sel_where = " SELECT * FROM tab_delegacion WHERE ",cla_where CLIPPED,
                      " ORDER BY 2 " CLIPPED

      PREPARE curdel2 FROM sel_where

      DECLARE cursor_2 CURSOR FOR curdel2

      LET pos = 1
      FOREACH cursor_2 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.* 
            ON KEY (control-m)
               LET pos = ARR_CURR()
               LET g_reg.estad_cod = l_record[pos].cod
	       LET g_reg.deleg_cod = l_record[pos].codigo
               LET g_reg.deleg_desc = l_record[pos].descripcion

               SELECT estad_desc 
               INTO g_reg.estad_desc
               FROM tab_estado
               WHERE estad_cod = g_reg.estad_cod

               EXIT DISPLAY
            ON KEY (INTERRUPT)
	       ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
	 CLOSE WINDOW vent_1
      ELSE
         ERROR "REGISTROS DE DELEGACION ... NO EXITE"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW vent_1
         RETURN
      END IF 

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

      INPUT BY NAME  g_reg.* WITHOUT DEFAULTS 
         BEFORE FIELD estad_cod
            DISPLAY BY NAME g_reg.estad_desc
            NEXT FIELD deleg_desc
         AFTER FIELD deleg_desc
            IF g_reg.deleg_desc IS NULL THEN
               ERROR "Descripcion de Delegacion NO puede ser nula"
               NEXT FIELD  deleg_desc
             END IF 
      
            IF g_reg.deleg_desc IS NULL THEN
	       ERROR "Descripcion de Delegacion NO puede ser NULO"
               NEXT FIELD deleg_desc
            END IF
            CALL Pregunta()
    
            IF aux_pausa MATCHES "[Ss]" THEN
               UPDATE tab_delegacion SET
                      deleg_desc = g_reg.deleg_desc
                WHERE deleg_cod = g_reg.deleg_cod

	       ERROR "REGISTRO MODIFICADO"
               SLEEP 2
	       ERROR ""

               CALL Inicializa()
            ELSE
               ERROR "PROCESO DE MODIFICACION,CANCELADO..."
               SLEEP 2
               ERROR ""
            END IF
            EXIT INPUT
	 ON KEY ( INTERRUPT )
            CALL Inicializa()
            EXIT INPUT
      END INPUT
   ELSE
      ERROR "ARCHIVO DE DELEGACIONES VACIO"
      SLEEP 2
      CLOSE WINDOW vent_1
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      OPEN WINDOW vent_1 AT 6,3 WITH FORM "TABM0022" ATTRIBUTE(BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                  Escoja con < ENTER > la ciudad a eliminar                    " AT 2,1 
      DISPLAY "                         DELEGACIONES / MUNICIPIOS                             " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON estad_cod,deleg_cod FROM estad_cod,deleg_cod
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
         CLOSE WINDOW vent_1
         RETURN
      END IF

      LET sel_where = " SELECT * FROM tab_delegacion WHERE ",cla_where CLIPPED,
                      " ORDER BY 2 " CLIPPED

      PREPARE curdel3 FROM sel_where

      DECLARE cursor_3 CURSOR FOR curdel3

      LET pos = 1
      FOREACH cursor_3 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.* 
            ON KEY (control-m)
               LET pos = ARR_CURR()
               LET g_reg.estad_cod = l_record[pos].cod
               LET g_reg.deleg_cod = l_record[pos].codigo
               LET g_reg.deleg_desc = l_record[pos].descripcion

               SELECT estad_desc 
               INTO g_reg.estad_desc
               FROM tab_estado
               WHERE estad_cod = g_reg.estad_cod

               EXIT DISPLAY
            ON KEY (INTERRUPT)
	       ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
	 CLOSE WINDOW vent_1
      ELSE
         ERROR "REGISTROS DE CIUDADES ... NO EXISTE"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW vent_1
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY BY NAME  g_reg.*
      DISPLAY BY NAME g_reg.estad_desc

      CALL Pregunta()

      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE FROM tab_delegacion
         WHERE deleg_cod = g_reg.deleg_cod

         ERROR "REGISTRO ELIMINADO" 
         SLEEP 1
      ELSE
         ERROR "ELIMINAR CANCELADO" 
         SLEEP 1
      END IF

      ERROR ""
      CALL Inicializa()
   ELSE
      ERROR "ARCHIVO DE DELEGACIONES VACIO"
      SLEEP 2
      CLOSE WINDOW vent_1
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION Despliega_estado()
     LET pos = 2
     IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)
        OPEN WINDOW vent_1 AT 6,3 WITH FORM "TABM0023" ATTRIBUTE(BORDER)
        DISPLAY "                        DELEGACIONES / MUNICIPIOS                              " AT 2,1 ATTRIBUTE(REVERSE)
        
        LET int_flag = FALSE

        CONSTRUCT cla_where ON estad_cod FROM estad_cod
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
           CLOSE WINDOW vent_1
           RETURN
        END IF

        WHILE TRUE
           LET sel_where = " SELECT estad_cod,estad_desc FROM tab_estado ",
                           " WHERE ",cla_where CLIPPED,
                           " ORDER BY 1 " CLIPPED
    
           PREPARE cur20y FROM sel_where

           DECLARE cur_2y CURSOR FOR cur20y

           LET pos = 1
           FOREACH cur_2y INTO l_reg[pos].*
                   LET pos = pos + 1
           END FOREACH
  
           INITIALIZE l_reg[pos].* TO NULL

           IF (pos-1) >= 1 THEN
              CALL SET_COUNT(pos-1)
              DISPLAY ARRAY l_reg TO scr_1.*
                 ON KEY ( INTERRUPT )
                    LET pos = 0
                    EXIT DISPLAY
                 ON KEY ( CONTROL-M )
                    LET pos = ARR_CURR()
                    EXIT DISPLAY
              END DISPLAY
            END IF
            IF pos <> 0 THEN
               EXIT WHILE
            END IF
        END WHILE
        CLOSE WINDOW vent_1
        RETURN l_reg[pos].estad_cod,l_reg[pos].estad_desc
        END IF
END FUNCTION
#####################################################################
FUNCTION impresion(pos)
   DEFINE i,pos    SMALLINT

   LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",seg_usuario CLIPPED,
                 ".IMPDELEG",HOY USING "dd-mm-yyyy"

   START REPORT rpt_tabdeleg TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.estad_cod         = l_record[i].cod
       LET g_reg.deleg_cod         = l_record[i].codigo
       LET g_reg.deleg_desc        = l_record[i].descripcion

       IF g_reg.estad_cod IS NULL THEN
          EXIT FOR
       END IF
   
       OUTPUT TO REPORT rpt_tabdeleg(g_reg.estad_cod,g_reg.deleg_cod,
                                     g_reg.deleg_desc)
   END FOR

   FINISH REPORT rpt_tabdeleg

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabdeleg(g_reg)   

   DEFINE g_reg  RECORD 
          estad_cod	INTEGER,
          deleg_cod	INTEGER,
          deleg_desc  	CHAR(40)
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM002 ",
               COLUMN 15," LISTADO DE CATALOGOS DE DELEGACIONES / MUNICIPIOS ",
               COLUMN 70,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE 
 
         PRINT COLUMN 01,"CODIGO ESTADO",
               COLUMN 15,"CODIGO DELEGACION",
               COLUMN 35,"DESCRIPCION DELEGACION"
         SKIP 1 LINE

      ON EVERY ROW
         PRINT COLUMN 04,g_reg.estad_cod USING "#####&",
               COLUMN 20,g_reg.deleg_cod USING "#####&",
               COLUMN 37,g_reg.deleg_desc 
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<<<"
END REPORT
       
