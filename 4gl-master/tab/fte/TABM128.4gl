################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Propietario       => E.F.P.        					       #
#Programa TABM128  => CATALOGO GRADO DE CONTROL DE LA AFORE                    #
#Fecha             => 21 SEPTIEMBRE 2006	                               #
#Autor             => VERONICA LOPEZ SANCHEZ        		               #
#Fecha Modifica    =>                               		               #
#Autor             =>                               		               #
#Sistema           => tab  					               #
################################################################################
DATABASE safre_af
GLOBALS

    DEFINE g_detalle ARRAY[32767] OF RECORD 
           cv_afore        SMALLINT,
           afore_desc      CHAR(25),
           fecha_ini       DATE,
           fecha_fin       DATE,
           grado_ctr_afo   DECIMAL(10,6),
           descripcion     CHAR(25)
    END RECORD

    DEFINE r_detalle  RECORD 
           cv_afore        SMALLINT,
           afore_desc      CHAR(25),
           fecha_ini       DATE,
           fecha_fin       DATE,
           grado_ctr_afo   DECIMAL(10,6),
           descripcion     CHAR(25)
    END RECORD

    DEFINE g_com_parametro RECORD LIKE com_parametro.*

    DEFINE HOY	             DATE
    DEFINE fecha_actualiza   DATE
    DEFINE l_max_fecha       DATE
    DEFINE arr_c,scr_l       SMALLINT
    DEFINE g_usuario         CHAR(8)
    DEFINE opc               CHAR(01)
    DEFINE aux_pausa         CHAR(1)
    DEFINE cla_sel           CHAR(300)
    DEFINE i 	             SMALLINT
    DEFINE l_count           SMALLINT
    DEFINE desc_afore        CHAR(25)
    DEFINE cla_whe             CHAR(1200)
    DEFINE cod_afore         SMALLINT
    DEFINE sel_where         CHAR(2000)
    DEFINE sel_where1        CHAR(2000)
    DEFINE sel_where2        CHAR(2000)
    DEFINE cla_where         CHAR(2000)

END GLOBALS


MAIN
    OPTIONS PROMPT LINE LAST,
            INPUT WRAP,
            ACCEPT KEY control-i

    DEFER INTERRUPT

    CALL STARTLOG("TABM128.log")

    LET HOY = DATE

    SELECT *,USER 
    INTO g_com_parametro.*,g_usuario 
    FROM com_parametro

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "TABM1281" ATTRIBUTE( BORDER)
    DISPLAY " TABM128              G R A D O    D E    C O N T R O L                         " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
	
    MENU "MENU"
        COMMAND "Agrega" "Agrega Grado de Control de la AFORE"
            CALL inicio()
            CALL Agrega()
        COMMAND "Consulta" "Consulta Grado de Control de la AFORE"
            CALL inicio()
            CALL Consulta()
        COMMAND "Modifica" "Modifica Grado de Control de la AFORE"
            CALL inicio()
            CALL Modifica()
        COMMAND "Elimina" "Elimina Grado de Control de la AFORE"
            CALL inicio()
            CALL Elimina()
        COMMAND "Salir" "Salir del Programa"
            EXIT MENU
    END MENU

    #CLOSE WINDOW ventana_1

END MAIN

FUNCTION inicio()

    SELECT a.codigo_afore,b.afore_desc
    INTO   cod_afore,desc_afore
    FROM   tab_afore_local a,tab_afore b
    WHERE  a.codigo_afore = b.afore_cod

    CLEAR FORM

END FUNCTION


FUNCTION Agrega()

    DEFINE i            SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
    DISPLAY " [ Ctrl-c ] Salir sin Agregar" AT 2,1 ATTRIBUTE(BOLD)
    DISPLAY "          " AT 4,21 
    DISPLAY " Para la descripcion digite Enter para ver Catalogo  " AT 20,1 ATTRIBUTE(BOLD) 

    INPUT ARRAY g_detalle FROM scr_1.*

    BEFORE FIELD cv_afore
        LET arr_c = ARR_CURR()
        LET scr_l = SCR_LINE()

        LET g_detalle[arr_c].cv_afore = cod_afore
        DISPLAY g_detalle[arr_c].cv_afore TO scr_1[scr_l].cv_afore

        LET g_detalle[arr_c].afore_desc = desc_afore
        DISPLAY g_detalle[arr_c].afore_desc TO scr_1[scr_l].afore_desc
        NEXT FIELD fecha_ini

    BEFORE FIELD fecha_ini
        LET arr_c = ARR_CURR()
        LET scr_l = SCR_LINE()
        LET g_detalle[arr_c].fecha_ini = TODAY
        DISPLAY g_detalle[arr_c].fecha_ini TO scr_1[scr_l].fecha_ini

    AFTER FIELD fecha_ini
        IF g_detalle[arr_c].fecha_ini IS NULL THEN
            ERROR "Fecha Inicial NO puede ser NULA"
            NEXT FIELD fecha_ini
        END IF
    NEXT FIELD grado_ctr_afo

    BEFORE FIELD grado_ctr_afo
    LET arr_c = ARR_CURR()
    LET scr_l = SCR_LINE()

    SELECT MAX(fecha_ini)
    INTO l_max_fecha 
    FROM tab_grado_ctr_afo
    WHERE cv_afore = g_detalle[arr_c].cv_afore

    IF g_detalle[arr_c].fecha_ini <= l_max_fecha THEN
        ERROR "Fecha NO puede ser menor que ", l_max_fecha USING "dd-mm-yyyy"
        NEXT FIELD fecha_ini
    END IF

    AFTER FIELD grado_ctr_afo

    IF g_detalle[arr_c].grado_ctr_afo IS NULL THEN
        ERROR "Grado de Control de la Afore NO puede ser NULO"
        NEXT FIELD grado_ctr_afo
    END IF

    AFTER FIELD descripcion
    
    IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
        NEXT FIELD PREVIOUS
    END IF

    IF g_detalle[arr_c].descripcion IS NOT NULL  THEN
    --Checa si es una descripcion valida
        SELECT COUNT(*)
        INTO   l_count
        FROM   tab_grado_ctr
        WHERE  descripcion  =  g_detalle[arr_c].descripcion

        IF l_count = 0  THEN
            ERROR "   NO EXISTE DESCRIPCION DEL GRADO DE CONTROL  "
            NEXT FIELD descripcion
        END IF
    ELSE
        CALL trae_descripcion(arr_c)
        DISPLAY  g_detalle[arr_c].descripcion TO scr_1[scr_l].descripcion
    END IF

    IF g_detalle[arr_c].descripcion IS NULL THEN
        {CALL trae_descripcion(arr_c)
        DISPLAY  g_detalle[arr_c].descripcion TO scr_1[scr_l].descripcion}
       ERROR " CAMPO NO PUEDE SER NULO"
       NEXT FIELD descripcion
    END IF

    PROMPT "Desea Grabar [S/N]... " FOR CHAR aux_pausa

    IF aux_pausa MATCHES "[Ss]" THEN
        
        UPDATE tab_grado_ctr_afo
        SET fecha_fin        = (g_detalle[arr_c].fecha_ini-1)
        WHERE  fecha_fin    IS NULL

        ERROR "REGISTRO INGRESADO" SLEEP 2 ERROR ""

        LET fecha_actualiza = TODAY

        INSERT INTO tab_grado_ctr_afo
        VALUES (g_detalle[arr_c].cv_afore,
		g_detalle[arr_c].grado_ctr_afo,
 	        g_detalle[arr_c].descripcion,
 	        g_detalle[arr_c].fecha_ini,
 	        g_detalle[arr_c].fecha_fin,
 	        fecha_actualiza,
                g_usuario)
    ELSE
        ERROR " INGRESO CANCELADO " 
        SLEEP 2 
        EXIT INPUT 
    END IF

    ON KEY ( INTERRUPT )
        CALL inicio()
        EXIT INPUT
    END INPUT

    ERROR ""

END FUNCTION


FUNCTION trae_descripcion(arr_c)

    DEFINE  arr_c       SMALLINT  --Donde cargara el registro seleccionado
    DEFINE  pa_elem     SMALLINT
    DEFINE  i_grado     SMALLINT

    DEFINE  ga_grado  ARRAY[20] OF RECORD
        grado_ctr_afo  SMALLINT,
        descripcion    CHAR(20)
    END RECORD 
           
 
    OPEN WINDOW win_st  AT 10,30  WITH FORM "TABM1282" ATTRIBUTE(BORDER)

    DISPLAY  "  CATALOGO GRADO DE CONTROL AFORE  "  AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY  "<ENTER> SELECCIONAR    <ESC> SALIR "  AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY  "  USE <FLECHAS> PARA DESPLAZARSE   "  AT 3,1 ATTRIBUTE(BOLD)
 
    ---SELECCION DE LOS GRADOS DE CONTROL PARA LA AYUDA

    LET i_grado  = 1

    DECLARE c_grado  CURSOR FOR
    SELECT grado_ctr_afo,descripcion
    FROM   tab_grado_ctr
    ORDER  BY  1

    FOREACH c_grado INTO ga_grado[i_grado].*
       LET i_grado = i_grado + 1
    END FOREACH

    CALL SET_COUNT(i_grado - 1)

    DISPLAY ARRAY ga_grado TO sa_grado.*
        ON KEY (ESC)
           LET pa_elem = arr_curr()
           EXIT DISPLAY
        ON KEY (CONTROL-M)
           LET pa_elem = arr_curr()
           LET g_detalle[arr_c].descripcion = ga_grado[pa_elem].descripcion
           EXIT DISPLAY
     END DISPLAY

     CLOSE WINDOW win_st

END FUNCTION


FUNCTION Consulta()

    DEFINE i  SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
    DISPLAY " [ Ctrl-c ] Salir" AT 2,1 ATTRIBUTE(BOLD)
    DISPLAY  cod_afore   AT 8,2 ATTRIBUTE(BOLD)
    DISPLAY  desc_afore  AT 8,7 ATTRIBUTE(BOLD)

    INITIALIZE g_detalle TO NULL
    INITIALIZE r_detalle TO NULL

    FOR i = 1 TO 10
        LET g_detalle[i].cv_afore       = " "
        LET g_detalle[i].afore_desc     = " "
        LET g_detalle[i].fecha_ini      = " "
        LET g_detalle[i].fecha_fin      = " "
        LET g_detalle[i].grado_ctr_afo  = " "
        LET g_detalle[i].descripcion    = " "
    END FOR

    LET i = 0

    CONSTRUCT cla_where ON  a.fecha_ini ,
                            a.fecha_fin ,
                            a.descripcion
                       FROM fecha_ini ,
                            fecha_fin , 
                            descripcion

       ON KEY (ESC)
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

       ON KEY (INTERRUPT)
            LET INT_FLAG = TRUE
            EXIT CONSTRUCT

    END CONSTRUCT 

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR FORM 
        RETURN
    END IF 

    LET sel_where = " SELECT  a.cv_afore,",
                            " b.afore_desc,",
                            " a.fecha_ini,",
                            " a.fecha_fin,",
                            " a.grado_ctr_afo,",
                            " a.descripcion",
                    " FROM   tab_grado_ctr_afo a, tab_afore b",
                    " WHERE ", cla_where CLIPPED,
                    " AND   a.cv_afore = b.afore_cod",
                    " ORDER BY 3 ASC" CLIPPED

    LET sel_where = sel_where CLIPPED

    PREPARE qry_consul FROM sel_where

    DECLARE cursor_c CURSOR FOR qry_consul

    LET i = 1

    FOREACH cursor_c INTO g_detalle[i].cv_afore  THRU g_detalle[i].descripcion
        LET i = i + 1
    END FOREACH

    IF (i-1) >= 1 THEN
        CALL SET_COUNT(i-1)
        DISPLAY ARRAY g_detalle TO scr_1.*

        ON KEY ( INTERRUPT )
        CALL inicio()
        EXIT DISPLAY
        END DISPLAY
    ELSE
         ERROR "ARCHIVO GRADO DE CONTROL... VACIO"
         SLEEP 2
         ERROR ""
         RETURN
    END IF

END FUNCTION


FUNCTION Modifica()

    LET INT_FLAG  = FALSE

    DISPLAY  cod_afore   AT 8,2 ATTRIBUTE(BOLD)
    DISPLAY  desc_afore  AT 8,7 ATTRIBUTE(BOLD)

    INITIALIZE g_detalle TO NULL
    INITIALIZE r_detalle TO NULL

    FOR i = 1 TO 10
        LET g_detalle[i].cv_afore       = " "
        LET g_detalle[i].afore_desc     = " "
        LET g_detalle[i].fecha_ini      = " "
        LET g_detalle[i].fecha_fin      = " "
        LET g_detalle[i].grado_ctr_afo  = " "
        LET g_detalle[i].descripcion    = " "
    END FOR

    CONSTRUCT BY NAME cla_whe ON fecha_ini,
                                 fecha_fin,
                                 descripcion

    ON KEY (INTERRUPT)
       LET INT_FLAG = TRUE
       EXIT CONSTRUCT

    ON KEY (ESC)
       LET INT_FLAG = FALSE
       EXIT CONSTRUCT
    END CONSTRUCT
 
    IF INT_FLAG = TRUE THEN
       LET INT_FLAG = FALSE
       CLEAR FORM
       CLEAR SCREEN
       RETURN
    END IF

    LET sel_where1 = " SELECT  a.cv_afore,",
                            " b.afore_desc,",
                            " a.fecha_ini,",
                            " a.fecha_fin,",
                            " a.grado_ctr_afo,",
                            " a.descripcion",
                    " FROM   tab_grado_ctr_afo a, tab_afore b",
                    " WHERE ", cla_whe CLIPPED,
                    " AND   a.cv_afore = b.afore_cod",
                    " AND   a.fecha_fin IS NULL",
                    " ORDER BY a.fecha_ini" CLIPPED

    PREPARE claexe2 FROM sel_where1
    DECLARE cur_2 CURSOR FOR claexe2

    LET i = 1

    ERROR "PROCESANDO INFORMACION"

    FOREACH cur_2 INTO g_detalle[i].*
        LET i = i + 1
    END FOREACH

    INITIALIZE g_detalle[i].* TO NULL

    IF (i-1) >= 1 THEN
        CALL SET_COUNT(i-1)
        ERROR ""
        DISPLAY ARRAY g_detalle TO scr_1.*

        ON KEY (control-m)
            LET i = ARR_CURR()
            LET r_detalle.cv_afore      = g_detalle[i].cv_afore
            LET r_detalle.afore_desc    = g_detalle[i].afore_desc
            LET r_detalle.fecha_ini     = g_detalle[i].fecha_ini
            LET r_detalle.fecha_fin     = g_detalle[i].fecha_fin
            LET r_detalle.grado_ctr_afo = g_detalle[i].grado_ctr_afo
            LET r_detalle.descripcion   = g_detalle[i].descripcion
        EXIT DISPLAY

        ON KEY(INTERRUPT)
            ERROR "Debe elegir un registro."
            LET i = ARR_CURR()
            END DISPLAY
    ELSE
         ERROR "ARCHIVO GRADO DE CONTROL... VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_1
         RETURN
    END IF

    IF INT_FLAG = TRUE THEN
       LET INT_FLAG = FALSE
       CLEAR FORM
       CLEAR SCREEN
       RETURN
    END IF

    CALL construccion(g_detalle[i].*)

END FUNCTION


FUNCTION construccion(g_detalle)

    DEFINE zona_cod_or              DECIMAL(10,6)
    DEFINE fecha_desde_sm_or        DATE
    DEFINE fecha_act                DATE
    DEFINE monto_sm_or              CHAR(20)

    DEFINE g_detalle RECORD
           cv_afore        SMALLINT,
           afore_desc      CHAR(25),
           fecha_ini       DATE,
           fecha_fin       DATE,
           grado_ctr_afo   DECIMAL(10,6),
           descripcion     CHAR(25)
    END RECORD

    LET i = 0

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
    DISPLAY " [ Ctrl-c ] Salir" AT 2,1 ATTRIBUTE(BOLD)

    INPUT BY NAME g_detalle.* WITHOUT DEFAULTS

    BEFORE FIELD cv_afore
        LET arr_c = ARR_CURR()
        LET scr_l = SCR_LINE()

        LET g_detalle.cv_afore = cod_afore
        DISPLAY g_detalle.cv_afore TO scr_1[scr_l].cv_afore
            
        LET g_detalle.afore_desc = desc_afore
        DISPLAY g_detalle.afore_desc TO scr_1[scr_l].afore_desc
        NEXT FIELD fecha_ini
         
    BEFORE FIELD fecha_ini
	LET arr_c = ARR_CURR()
	LET scr_l = SCR_LINE()
        LET zona_cod_or       = g_detalle.grado_ctr_afo
        LET fecha_desde_sm_or = g_detalle.fecha_ini
 	LET monto_sm_or       = g_detalle.descripcion

        NEXT FIELD grado_ctr_afo

    BEFORE FIELD grado_ctr_afo
        LET arr_c = ARR_CURR()
	LET scr_l = SCR_LINE()

        LET zona_cod_or       = g_detalle.grado_ctr_afo
        LET fecha_desde_sm_or = g_detalle.fecha_ini
 	LET monto_sm_or       = g_detalle.descripcion

    AFTER FIELD grado_ctr_afo
        IF g_detalle.grado_ctr_afo IS NULL THEN
	    ERROR "Grado Control de Afore NO puede ser NULO"
	    NEXT FIELD grado_ctr_afo
	END IF

	LET fecha_act = TODAY

    AFTER FIELD descripcion

        IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
            NEXT FIELD PREVIOUS
        END IF

        IF g_detalle.descripcion IS NOT NULL  THEN
        --Checa si es una descripcion valida
            SELECT COUNT(*)
            INTO   l_count
            FROM   tab_grado_ctr
            WHERE  descripcion  =  g_detalle.descripcion
    
            IF l_count = 0  THEN
                ERROR "   NO EXISTE DESCRIPCION DEL GRADO DE CONTROL  "
                NEXT FIELD descripcion
            END IF
        END IF

        IF g_detalle.descripcion IS NULL THEN
           ERROR " CAMPO NO PUEDE SER NULO"
           NEXT FIELD descripcion
        END IF

        PROMPT "Desea Grabar [S/N]... " FOR CHAR aux_pausa

        IF aux_pausa MATCHES "[Ss]" THEN
            UPDATE tab_grado_ctr_afo 
            SET grado_ctr_afo   = g_detalle.grado_ctr_afo,
                descripcion     = g_detalle.descripcion,
 	        fecha_actualiza = fecha_act,
	        usuario         = g_usuario 
	    WHERE fecha_fin IS NULL
            AND   grado_ctr_afo = zona_cod_or 
            AND   fecha_ini     = fecha_desde_sm_or 
 	    #AND   descripcion   = monto_sm_or

            ERROR "REGISTRO MODIFICADO"
            SLEEP 2
            ERROR ""
     
            CALL inicio()
            CLEAR FORM
            RETURN
        ELSE
            ERROR "INGRESO CANCELADO" SLEEP 2 ERROR ""
        END IF

    ON KEY ( INTERRUPT )
        CALL inicio()
	EXIT INPUT
    END INPUT

    ERROR ""

END FUNCTION


FUNCTION Elimina()

    DEFINE i			SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " ELIMINA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
    DISPLAY " [ Enter ] Elimina                        [ Ctrl-c ] Salir" 
    AT 2,1 ATTRIBUTE(BOLD)
    DISPLAY  cod_afore   AT 8,2 ATTRIBUTE(BOLD)
    DISPLAY  desc_afore  AT 8,7 ATTRIBUTE(BOLD)

    LET INT_FLAG  = FALSE

    INITIALIZE g_detalle TO NULL
    INITIALIZE r_detalle TO NULL

    FOR i = 1 TO 10
        LET g_detalle[i].cv_afore       = " "
        LET g_detalle[i].afore_desc     = " "
        LET g_detalle[i].fecha_ini      = " "
        LET g_detalle[i].fecha_fin      = " "
        LET g_detalle[i].grado_ctr_afo  = " "
        LET g_detalle[i].descripcion    = " "
    END FOR

    CONSTRUCT BY NAME cla_whe ON fecha_ini,
                                 fecha_fin,
                                 descripcion

    ON KEY (INTERRUPT)
       LET INT_FLAG = TRUE
       EXIT CONSTRUCT

    ON KEY (ESC)
       LET INT_FLAG = FALSE
       EXIT CONSTRUCT
    END CONSTRUCT

    IF INT_FLAG = TRUE THEN
       LET INT_FLAG = FALSE
       CLEAR FORM
       CLEAR SCREEN
       RETURN
    END IF

    LET sel_where2 = " SELECT  a.cv_afore,",
                            " b.afore_desc,",
                            " a.fecha_ini,",
                            " a.fecha_fin,",
                            " a.grado_ctr_afo,",
                            " a.descripcion",
                    " FROM   tab_grado_ctr_afo a, tab_afore b",
                    " WHERE ", cla_whe CLIPPED,
                    " AND   a.cv_afore = b.afore_cod",
                    " AND   a.fecha_fin IS NULL",
                    " ORDER BY a.fecha_ini" CLIPPED

    PREPARE claexe3 FROM sel_where2
    DECLARE cur_3 CURSOR FOR claexe3

    LET i = 1

    ERROR "PROCESANDO INFORMACION"

    FOREACH cur_3 INTO g_detalle[i].*
        LET i = i + 1
    END FOREACH

    INITIALIZE g_detalle[i].* TO NULL

    IF (i-1) >= 1 THEN
        CALL SET_COUNT(i-1)
        ERROR ""
        DISPLAY ARRAY g_detalle TO scr_1.*

        ON KEY (control-m)
            LET i = ARR_CURR()
            LET r_detalle.cv_afore      = g_detalle[i].cv_afore
            LET r_detalle.afore_desc    = g_detalle[i].afore_desc
            LET r_detalle.fecha_ini     = g_detalle[i].fecha_ini
            LET r_detalle.fecha_fin     = g_detalle[i].fecha_fin
            LET r_detalle.grado_ctr_afo = g_detalle[i].grado_ctr_afo
            LET r_detalle.descripcion   = g_detalle[i].descripcion
        EXIT DISPLAY

        ON KEY(INTERRUPT)
            ERROR "Debe elegir un registro."
            LET i = ARR_CURR()
            END DISPLAY
    ELSE
         ERROR "ARCHIVO GRADO DE CONTROL... VACIO"
         SLEEP 2
         ERROR ""
         ---CLOSE WINDOW ventana_1
         RETURN
    END IF
 
    CALL construccion1(g_detalle[i].*)

END FUNCTION 


FUNCTION construccion1(g_detalle)

    DEFINE zona_cod_or        DECIMAL(10,6)
    DEFINE fecha_desde_sm_or  DATE
    DEFINE fecha_act          DATE
    DEFINE monto_sm_or        CHAR(20)

    DEFINE g_detalle RECORD
           cv_afore        SMALLINT,
           afore_desc      CHAR(25),
           fecha_ini       DATE,
           fecha_fin       DATE,
           grado_ctr_afo   DECIMAL(10,6),
           descripcion     CHAR(25)
    END RECORD

    LET i = 0

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " ELIMINA  " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
    DISPLAY " [ Ctrl-c ] Salir" AT 2,1 ATTRIBUTE(BOLD)
    DISPLAY  cod_afore   AT 8,2 ATTRIBUTE(BOLD)
    DISPLAY  desc_afore  AT 8,7 ATTRIBUTE(BOLD)


    DISPLAY BY NAME g_detalle.* 
 
    PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa

    IF aux_pausa MATCHES "[Ss]" THEN
        DELETE FROM tab_grado_ctr_afo 
	WHERE fecha_fin IS NULL
        AND   grado_ctr_afo = g_detalle.grado_ctr_afo
        AND   fecha_ini     = g_detalle.fecha_ini
 	#AND   descripcion   = g_detalle.descripcion

        ERROR "REGISTRO ELIMINADO"
        SLEEP 2
        ERROR ""
    ELSE
        ERROR "PROCESO CANCELADO"
        SLEEP 2
        ERROR ""
    END IF

    ERROR ""

    CALL inicio()

END FUNCTION
