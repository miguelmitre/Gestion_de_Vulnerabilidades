################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Propietario       => E.F.P.                                                   #
#Programa TABM127  => CATALOGO TIPO DE COMPROBANTE DE DOMICILIO                #
#Fecha             => 20 SEPTIEMBRE 2006 		                       #
#Autor             => VERONICA LOPEZ SANCHEZ          			       #
#Fecha modifica    =>                                                          #
#Autor             =>                                                          #
#Sistema           => tab  					               #
################################################################################
DATABASE safre_af
GLOBALS
         DEFINE g_param_dis	RECORD LIKE dis_parametro.*

	 DEFINE aux_pausa	CHAR(1),
		sw_1		SMALLINT,
		usuario		CHAR(8),
		hoy		DATE,
		pos		INTEGER,
		sel_where	CHAR(30000),
		cla_where	CHAR(30000),
		g_impre		CHAR(300),
		g_lista		CHAR(300)

         DEFINE g_reg		RECORD 
		tipo            SMALLINT,
		descripcion     CHAR(25)
	 END RECORD
 
         DEFINE l_record	ARRAY[30000] OF RECORD
		tipo   		SMALLINT,
		descripcion	CHAR(25)
	 END RECORD

END GLOBALS


MAIN
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
	ACCEPT KEY control-o

	DEFER INTERRUPT
        CALL inicio()
	CALL proceso()

END MAIN


FUNCTION inicio()

    SELECT USER,*
    INTO   usuario
    FROM   glo_parametro

    SELECT ruta_spool
    INTO   g_param_dis.ruta_spool
    FROM   dis_parametro

END FUNCTION


FUNCTION proceso()

    LET HOY = TODAY
    OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM1271" ATTRIBUTE( BORDER)
    DISPLAY " TABM127     CATALOGO  TIPO  COMPROBANTE  DE  DOMICILIOS                       " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
    MENU " MENU"
        COMMAND "Agrega" "Agrega Tipo Comprobante"
                CALL Agrega()
                CLEAR SCREEN
        COMMAND "Consulta" "Consulta Tipo Comprobante"
		CALL Consulta()
                CLEAR SCREEN
        COMMAND "Modifica" "Modifica Tipo Comprobante"
		CALL Modifica()
                CLEAR SCREEN
        COMMAND "Elimina" "Elimina Tipo Comprobante"
		CALL Elimina()
                CLEAR SCREEN
        COMMAND "Salir" "Salir del Programa"
		EXIT MENU
    END MENU
              CLOSE WINDOW ventana_1
END FUNCTION


FUNCTION Inicializa()

    LET sw_1 = 0
    INITIALIZE g_reg.* TO NULL
    DISPLAY BY NAME g_reg.*

END FUNCTION


FUNCTION Agrega()
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " ( Esc ) Agrega                 (Ctrl-c) Salir                                 " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

    LET g_reg.descripcion = NULL

    LET sw_1 = 0

    INPUT BY NAME  g_reg.*

    BEFORE FIELD tipo

    IF sw_1 = 0 THEN
        LET sw_1 = 1
	SELECT MAX(tipo) 
        INTO   g_reg.tipo 
        FROM   tab_tipo_comp_dom

        IF g_reg.tipo = 0 OR 
            g_reg.tipo IS NULL THEN
	    LET g_reg.tipo = 1
	ELSE
	    LET g_reg.tipo = g_reg.tipo + 1
        END IF

        DISPLAY BY NAME g_reg.*

    END IF

    AFTER FIELD tipo
        IF g_reg.tipo IS NULL THEN
	    ERROR "Tipo Comprobante de Domicilios NO puede ser NULO"
            NEXT FIELD tipo
        END IF

        SELECT "X" 
        FROM   tab_tipo_comp_dom
        WHERE  tipo  = g_reg.tipo

        IF STATUS <> NOTFOUND THEN
	    ERROR "Tipo ya Ingresado"
	    NEXT FIELD tipo
	END IF

    BEFORE FIELD descripcion
        IF g_reg.tipo IS NULL OR  g_reg.tipo = 0 THEN
	    ERROR "Tipo Comprobante de Domicilios  NO puede ser NULO"
	    NEXT FIELD tipo
	END IF 

    AFTER FIELD descripcion
        IF g_reg.descripcion IS NULL THEN
	    ERROR "La Descripcion NO puede ser NULA"
	    NEXT FIELD descripcion
	END IF 

        SELECT "X" 
        FROM   tab_tipo_comp_dom
	WHERE  descripcion = g_reg.descripcion

	IF STATUS <> NOTFOUND THEN
	    ERROR "Tipo Comprobante de Domicilio ya Ingresado"
	    NEXT FIELD tipo
	END IF

	ON KEY ( ESC )
            IF g_reg.tipo IS NULL THEN
	        ERROR "Tipo Comprobante de Domicilios NO puede ser NULO"
                NEXT FIELD tipo
            END IF

            IF g_reg.descripcion IS NULL THEN
	        ERROR "La Descripcion NO debe ser NULA"
                NEXT FIELD descripcion
            END IF

            SELECT "X" 
            FROM   tab_tipo_comp_dom
	    WHERE  descripcion = g_reg.descripcion

            IF STATUS <> NOTFOUND THEN
	        ERROR "Tipo Comprobante de Domicilio ya Ingresado"
                NEXT FIELD tipo
            END IF

            INSERT INTO tab_tipo_comp_dom VALUES ( g_reg.*, HOY, usuario) 
	    ERROR "REGISTRO INGRESADO" SLEEP 2
	    ERROR ""
            CALL Inicializa()
	    NEXT FIELD tipo

        ON KEY (INTERRUPT)
            CALL Inicializa()

    EXIT INPUT
    END INPUT

END FUNCTION


FUNCTION Consulta()

    LET pos = 2

    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)

        OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1272" ATTRIBUTE( BORDER)
	DISPLAY "        (Enter) Consulta                              (Ctrl-C) Salir           " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	DISPLAY "                  TIPO  COMPROBANTE  DE  DOMICILIOS                            " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	LET int_flag = FALSE

	CONSTRUCT cla_where ON   tipo ,
                                 descripcion
                            FROM tipo,
                                 descripcion

            ON KEY (control-m)
                ERROR "PROCESANDO INFORMACION..."
                SLEEP 2
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

	LET sel_where = "SELECT * ",
                        "FROM  tab_tipo_comp_dom ",
                        "WHERE ", cla_where CLIPPED,
			"ORDER BY 1 "

	PREPARE query FROM sel_where

	DECLARE cursor_1 CURSOR FOR query

   	LET pos = 1

	FOREACH cursor_1 INTO l_record[pos].*
	    LET pos = pos + 1
	END FOREACH

	INITIALIZE l_record[pos].* TO NULL

	IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
            ERROR ""	
	    DISPLAY ARRAY l_record TO scr_1.*

	    ---ON KEY (control-p)
                ---ERROR "PROCESANDO INFORMACION..."
                ---CALL impresion(pos)

            ON KEY (INTERRUPT)
                EXIT DISPLAY
	        END DISPLAY
	        CLOSE WINDOW ventana_2
	ELSE
	    ERROR "ARCHIVO DE TIPO COMPROBANTE DE DOMICILIOS... VACIO"
            SLEEP 2
	    ERROR ""
	    CLOSE WINDOW ventana_2
	END IF
    END IF

    CLEAR SCREEN

END FUNCTION


FUNCTION  Modifica()

    LET pos = 2

    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)
	OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1272" ATTRIBUTE( BORDER)
	DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
        DISPLAY "           Escoja con <ENTER> el Tipo de Domicilio a modificar                 " AT 2,1
	DISPLAY "                  TIPO  COMPROBANTE  DE  DOMICILIOS                            " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

        LET int_flag = FALSE

	CONSTRUCT cla_where ON   tipo,
                                 descripcion 
                            FROM tipo,
                                 descripcion

	    ON KEY (control-m)
                ERROR "PROCESANDO INFORMACION..."
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

	LET sel_where = "SELECT * ",
                        "FROM tab_tipo_comp_dom ",
                        "WHERE ", cla_where CLIPPED,
			"ORDER BY 1 "

	PREPARE query1 FROM sel_where

	DECLARE cursor_2 CURSOR FOR query1

   	LET pos = 1

	FOREACH cursor_2 INTO l_record[pos].*
	    LET pos = pos + 1
	END FOREACH

	INITIALIZE l_record[pos].* TO NULL

	IF (pos-1) >= 1 THEN
            CALL  SET_COUNT(pos-1)
            ERROR ""	
	    DISPLAY ARRAY l_record TO scr_1.*

            ON KEY (control-m)
                LET pos = ARR_CURR()
                LET g_reg.tipo = l_record[pos].tipo
                LET g_reg.descripcion = l_record[pos].descripcion
                EXIT DISPLAY

            ON KEY (INTERRUPT)
                ERROR "Debe elegir un registro."
		LET pos = ARR_CURR()
	        END DISPLAY
	        CLOSE WINDOW ventana_2
	ELSE
	    ERROR "ARCHIVO DE TIPO COMPROBANTE DE DOMICILIOS... VACIO"
            SLEEP 2
	    ERROR ""
	    CLOSE WINDOW ventana_2
            RETURN
        END IF

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

	INPUT BY NAME g_reg.* WITHOUT DEFAULTS

	BEFORE FIELD tipo
        NEXT FIELD descripcion

	AFTER FIELD descripcion
        IF g_reg.descripcion IS NULL THEN
            ERROR "La Descipcion NO debe ser NULA"
            NEXT FIELD descripcion
        END IF

        CALL Pregunta()

        IF aux_pausa MATCHES "[Ss]" THEN
            UPDATE tab_tipo_comp_dom
            SET descripcion = g_reg.descripcion
            WHERE tipo = g_reg.tipo
	
            ERROR "REGISTRO MODIFICADO"
            SLEEP 2
            ERROR ""

            CALL Inicializa()
        ELSE
            ERROR "PROCESO DE MODIFICAR CANCELADO"
            SLEEP 2
            ERROR ""
        END IF

        EXIT INPUT
        ON KEY (INTERRUPT)
            CALL Inicializa()
            EXIT INPUT
            END INPUT
    ELSE
        ERROR "ARCHIVO DE TIPO COMPROBANTE DE DOMICILIOS... VACIO."
    END IF

    CLEAR SCREEN

END FUNCTION


FUNCTION Elimina()

    LET pos = 2

    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)
        OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1272" ATTRIBUTE( BORDER)
        DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
        DISPLAY "            Escoja con <ENTER> el Tipo de Domicilio a Eliminar                 " AT 2,1
        DISPLAY "                  TIPO  COMPROBANTE  DE  DOMICILIOS                            " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

        LET int_flag = FALSE

        CONSTRUCT cla_where ON  tipo,
                                descripcion
                          FROM  tipo,
                                descripcion
            ON KEY (control-m)
                ERROR "PROCESANDO INFORMACION..."
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

        LET sel_where = "SELECT * ",
                        "FROM tab_tipo_comp_dom ",
                        "WHERE ", cla_where CLIPPED,
			"ORDER BY 1 "

        PREPARE query2 FROM sel_where

        DECLARE cursor_3 CURSOR FOR query2

        LET pos = 1

        FOREACH cursor_3 INTO l_record[pos].*
            LET pos = pos + 1
        END FOREACH

        INITIALIZE l_record[pos].* TO NULL

        IF (pos-1) >= 1 THEN
            CALL  SET_COUNT(pos-1)
            ERROR ""	
            DISPLAY ARRAY l_record TO scr_1.*

            ON KEY (control-m)
                LET pos = ARR_CURR()
                LET g_reg.tipo = l_record[pos].tipo
                LET g_reg.descripcion = l_record[pos].descripcion
                EXIT DISPLAY

            ON KEY (INTERRUPT)
                ERROR "Debe elegir un Registro"
                LET pos = ARR_CURR()
                END DISPLAY
                CLOSE WINDOW ventana_2
        ELSE
            ERROR "ARCHIVO DE TIPO COMPROBANTE DE DOMICILIOS... VACIO"
            SLEEP 2
            ERROR ""
            CLOSE WINDOW ventana_2
            RETURN
	END IF

        DISPLAY "" AT 1,1
        DISPLAY "" AT 2,1
        DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
        DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

        DISPLAY BY NAME g_reg.*
        CALL Pregunta()

        IF aux_pausa MATCHES "[Ss]" THEN
            DELETE FROM tab_tipo_comp_dom
	    WHERE tipo = g_reg.tipo

            ERROR "REGISTRO ELIMINADO"
            SLEEP 2
            ERROR ""
        ELSE
            ERROR "PROCESO CANCELADO"
            SLEEP 2
            ERROR ""
        END IF

        CALL Inicializa()

    ELSE
        ERROR "ARCHIVO DE TIPO COMPROBANTE DE DOMICILIOS... VACIO."
    END IF

    CLEAR SCREEN

END FUNCTION


FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION

{ 

FUNCTION impresion(pos)
	DEFINE i, pos SMALLINT

	LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",usuario CLIPPED,".IMPTRECHA",hoy USING "dd-mm-yyyy" CLIPPED

	START REPORT rpt_tabrechcurp To g_impre

	FOR i = 1 TO (pos+1)
	   LET g_reg.curp_cod_rech	 = l_record[i].codigo
	   LET g_reg.des_rechazo = l_record[i].descripcion

	   IF g_reg.curp_cod_rech	 IS NULL THEN
	      EXIT FOR
	   END IF

	   OUTPUT TO REPORT rpt_tabrechcurp(g_reg.*)
	END FOR

	FINISH REPORT rpt_tabrechcurp

	ERROR "LISTADO GENERADO..."
	SLEEP 2
	ERROR ""

	LET g_lista = "lp ",g_impre
	RUN g_lista
END FUNCTION
################################################################################
REPORT rpt_tabrechcurp(g_reg)
         DEFINE g_reg		RECORD 
		curp_cod_rech	SMALLINT,
		des_rechazo  	CHAR(40)
	 END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM028 ",
               COLUMN 24," LISTADO DE CATALOGO DE RECHAZOS ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"CODIGO",
               COLUMN 20,"DESCRIPCION RECHAZO"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 10,g_reg.curp_cod_rech ,
               COLUMN 25,g_reg.des_rechazo
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
	 PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT }
