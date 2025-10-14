######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Owner             => E.F.P.                                         #
#Programa TABM135  => CATALOGO DE RECHAZOS.                          #
#Fecha             => 17 de JULIO DE 2008                            #
#By                => JAVIER GONZALEZ JERONIMO                       #
#Fecha modifica    =>                                                #
#Modificado por    =>                                                #
#Sistema           => TAB.                                           #
######################################################################
DATABASE safre_af

GLOBALS

    DEFINE g_reg  RECORD
        cod_rechazo_ent     LIKE ret_rechazo_grl.cod_rechazo_ent,
        entidad             LIKE ret_rechazo_grl.entidad,
        tipo_retiro         LIKE ret_rechazo_grl.tipo_retiro,
        des_corta           LIKE ret_rechazo_grl.des_corta,
        des_larga           LIKE ret_rechazo_grl.des_larga
    END RECORD

    DEFINE gd_fecha_actualiza LIKE ret_rechazo_grl.fecha_actualiza
    DEFINE gc_descripcion     LIKE tab_entidad.descripcion

    DEFINE gr_seg_modulo  RECORD LIKE  seg_modulo.*

    DEFINE l_record ARRAY[3000] OF RECORD
        cod_rechazo_ent     LIKE ret_rechazo_grl.cod_rechazo_ent,
        entidad             LIKE ret_rechazo_grl.entidad,
        tipo_retiro         LIKE ret_rechazo_grl.tipo_retiro,
        des_corta           LIKE ret_rechazo_grl.des_corta,
        des_larga           LIKE ret_rechazo_grl.des_larga
    END RECORD


    DEFINE sw_1                SMALLINT,
           aux_pausa           CHAR(01),
           HOY                 DATE,
           aux_estad_desc      CHAR(40),
           seg_usuario         CHAR(08),
           pos                 SMALLINT,
           cla_where           CHAR(300),
           sel_where           CHAR(300),
           g_lista             CHAR(300),
           g_impre             CHAR(300)

END GLOBALS

##########################################################################

MAIN
    OPTIONS
        PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY control-o

    DEFER INTERRUPT

    CALL inicio()

    CALL proceso()

END MAIN

##########################################################################

FUNCTION inicio()

    SELECT USER,
           *
    INTO   seg_usuario,
           gr_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

END FUNCTION

##########################################################################

FUNCTION proceso()

    LET HOY = TODAY

    OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM1351" ATTRIBUTE(BORDER)
    DISPLAY " TABM135               CATALOGO GENERAL DE RECHAZOS                            " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    MENU "CATALOGO RECHAZOS"
        COMMAND "Agrega" "Agrega Rechazos"
            CALL Agrega()
        COMMAND "Consulta" "Consulta Rechazos"
            CALL Consulta()
        COMMAND "Modifica" "Modifica Rechazos"
            CALL Modifica()
        COMMAND "Elimina" "Elimina Rechazos"
            CALL Elimina()
        COMMAND "Salir" "Salir del Programa"
            EXIT MENU
    END MENU

    CLOSE WINDOW ventana_1

END FUNCTION

#############################################################################

FUNCTION Inicializa()

    LET sw_1 = 0
    INITIALIZE gc_descripcion TO NULL
    DISPLAY gc_descripcion TO desc_entidad

    INITIALIZE g_reg.* TO NULL
    DISPLAY BY NAME g_reg.*

END FUNCTION

################################################################################

FUNCTION Agrega()

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " ( Esc ) Agrega                  (Ctrl-c) Salir " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY " AGREGA " AT 1,70 ATTRIBUTE(REVERSE,BOLD)

    LET gd_fecha_actualiza = HOY
    INITIALIZE g_reg.entidad TO NULL

    INPUT BY NAME g_reg.* WITHOUT DEFAULTS

        BEFORE INPUT
            
            SELECT MAX(cod_rechazo_ent) + 1
            INTO   g_reg.cod_rechazo_ent
            FROM   ret_rechazo_grl
            WHERE  cod_rechazo_ent <> 99

        #--
        AFTER FIELD cod_rechazo_ent
            IF g_reg.cod_rechazo_ent IS NULL THEN
                ERROR "El campo no puede ser nulo"
                NEXT FIELD cod_rechazo_ent
            ELSE
                SELECT "X"
                FROM   ret_rechazo_grl
                WHERE  cod_rechazo_ent = g_reg.cod_rechazo_ent
                GROUP BY 1

                IF STATUS <> NOTFOUND THEN
                    ERROR "Ya existe el rechazo"
                    NEXT FIELD cod_rechazo_ent
                END IF
            END IF

        #--
        AFTER FIELD entidad
            IF g_reg.entidad IS NULL OR g_reg.entidad = 0 THEN
                CALL despliega_entidad()
                DISPLAY g_reg.entidad  TO entidad
                DISPLAY gc_descripcion TO desc_entidad
            ELSE
                SELECT descripcion
                INTO   gc_descripcion
                FROM   tab_entidad
                WHERE  entidad = g_reg.entidad

                IF STATUS = NOTFOUND THEN
                    ERROR "No existe la entidad"
                    NEXT FIELD entidad
                ELSE
                    DISPLAY gc_descripcion TO desc_entidad
                END IF
            END IF

        #--
        AFTER FIELD tipo_retiro
            IF g_reg.tipo_retiro IS NULL THEN
                ERROR "El campo no puede ser nulo"
                NEXT FIELD tipo_retiro
            END IF

        #--
        AFTER FIELD des_corta
            IF g_reg.des_corta IS NULL THEN
                ERROR "El campo no puede ser nulo"
                NEXT FIELD des_corta
            END IF

        #--
        AFTER FIELD des_larga
            IF g_reg.des_larga IS NULL THEN
                ERROR "El campo no puede ser nulo"
                NEXT FIELD des_larga
            END IF

        ON KEY ( ESC )

            IF g_reg.cod_rechazo_ent IS NULL THEN
                ERROR "El campo no puede ser nulo"
                NEXT FIELD cod_rechazo_ent
            END IF

            IF g_reg.entidad IS NULL THEN
                ERROR "El campo no puede ser nulo"
                NEXT FIELD entidad
            END IF

            IF g_reg.tipo_retiro IS NULL THEN
                ERROR "El campo no puede ser nulo"
                NEXT FIELD tipo_retiro
            END IF

            IF g_reg.des_corta IS NULL THEN
                ERROR "El campo no puede ser nulo"
                NEXT FIELD des_corta
            END IF

            IF g_reg.des_larga IS NULL THEN
                ERROR "El campo no puede ser nulo"
                NEXT FIELD des_larga
            END IF

            INSERT INTO ret_rechazo_grl
            VALUES (g_reg.*,
                    gd_fecha_actualiza,
                    seg_usuario )

            ERROR "REGISTRO INGRESADO"
            SLEEP 1
            ERROR ""

            CALL Inicializa()

            NEXT FIELD cod_rechazo_ent

        ON KEY (INTERRUPT)
            CALL Inicializa()
            EXIT INPUT

    END INPUT

    CALL Inicializa()

    CLEAR SCREEN

END FUNCTION

################################################################################
FUNCTION Consulta()

    LET pos = 2

    IF (pos-1) >= 1 THEN
        CALL  SET_COUNT(pos-1)

        OPEN WINDOW ventana_2 AT 4,3 WITH FORM "TABM1352" ATTRIBUTE(BORDER)
        DISPLAY " (ENTER) Consulta           (Ctrl-p) Impresion               (Ctrl-C) Salir   " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
        DISPLAY "             Escoja con < ENTER > el rechazo para ver completo                " AT 2,1
        DISPLAY "                            CODIGOS DE RECHAZO                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

        LET int_flag = FALSE

        CONSTRUCT cla_where ON   cod_rechazo_ent,
                                 entidad,
                                 tipo_retiro
                            FROM cod_rechazo_ent,
                                 entidad,
                                 tipo_retiro

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

        LET sel_where = "SELECT * FROM ret_rechazo_grl WHERE ",
                         cla_where CLIPPED,
                        " ORDER BY 1,2,3 "

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
                ON KEY (CONTROL-M)
                    LET pos = ARR_CURR()
                    LET g_reg.cod_rechazo_ent   = l_record[pos].cod_rechazo_ent
                    LET g_reg.entidad           = l_record[pos].entidad
                    LET g_reg.tipo_retiro       = l_record[pos].tipo_retiro
                    LET g_reg.des_corta         = l_record[pos].des_corta
                    LET g_reg.des_larga         = l_record[pos].des_larga
                    EXIT DISPLAY

                ON KEY (control-p)
                    ERROR "PROCESANDO IMPRESION..."
                    CALL impresion(pos)

                ON KEY (INTERRUPT)
                    EXIT DISPLAY
            END DISPLAY

            CLOSE WINDOW ventana_2
        ELSE
            ERROR "ARCHIVO DE CODIGO DE RECHAZOS.... VACIO"
            SLEEP 2
            ERROR ""
            CLOSE WINDOW ventana_2
            RETURN
        END IF

        DISPLAY "" AT 1,1
        DISPLAY "" AT 2,1
        DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
        DISPLAY " CONSULTA" AT 1,67 ATTRIBUTE(REVERSE,BOLD)

        DISPLAY BY NAME  g_reg.*

        SELECT descripcion
        INTO   gc_descripcion
        FROM   tab_entidad
        WHERE  entidad = g_reg.entidad

        DISPLAY gc_descripcion TO desc_entidad

    END IF

    CLEAR SCREEN

END FUNCTION

################################################################################

FUNCTION  Modifica()

    LET pos = 2

    IF (pos-1) >= 1 THEN

        CALL  SET_COUNT(pos-1)

        OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1352" ATTRIBUTE( BORDER)
        DISPLAY " (ENTER) Consulta                                            (Ctrl-c) Salir   " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
        DISPLAY "            Escoja con < ENTER > el rechazo de afiliado a modificar           " AT 2,1
        DISPLAY "                            CODIGOS DE RECHAZO                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

        LET int_flag = FALSE

        CONSTRUCT cla_where ON   cod_rechazo_ent,
                                 entidad,
                                 tipo_retiro
                            FROM cod_rechazo_ent,
                                 entidad,
                                 tipo_retiro
            ON KEY (control-m)
                LET int_flag = FALSE
                EXIT CONSTRUCT

            ON KEY (control-c)
                LET int_flag = TRUE
                EXIT CONSTRUCT

        END CONSTRUCT

        IF INT_FLAG = TRUE THEN
            LET INT_FLAG = FALSE
            ERROR "BUSQUEDA CANCELADA..."
            SLEEP 2
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW ventana_2
            RETURN
        END IF

        LET sel_where = "SELECT * FROM ret_rechazo_grl WHERE ",
                         cla_where CLIPPED,
                        " ORDER BY 1,2,3 "

        PREPARE query1 FROM sel_where

        DECLARE cursor_2 CURSOR FOR query1

        LET pos = 1

        FOREACH cursor_2 INTO l_record[pos].*
            LET pos = pos +1
        END FOREACH

        INITIALIZE l_record[pos].* TO NULL

        IF (pos-1) >= 1 THEN
            CALL SET_COUNT(pos-1)

            DISPLAY ARRAY l_record TO scr_1.*
                ON KEY (CONTROL-M)
                    LET pos = ARR_CURR()

                    LET g_reg.cod_rechazo_ent   = l_record[pos].cod_rechazo_ent
                    LET g_reg.entidad           = l_record[pos].entidad
                    LET g_reg.tipo_retiro       = l_record[pos].tipo_retiro
                    LET g_reg.des_corta         = l_record[pos].des_corta
                    LET g_reg.des_larga         = l_record[pos].des_larga
                    EXIT DISPLAY

                ON KEY (INTERRUPT)
                    ERROR "Usted debe escojer un registro"
                    LET pos = ARR_CURR()
            END DISPLAY

            CLOSE WINDOW ventana_2
        ELSE
            ERROR "ARCHIVO DE CODIGO DE RECHAZOS.... VACIO"
            SLEEP 2
            ERROR ""
            CLOSE WINDOW ventana_2
            RETURN
        END IF

        DISPLAY "" AT 1,1
        DISPLAY "" AT 2,1
        DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
        DISPLAY " MODIFICA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

        INPUT BY NAME g_reg.* WITHOUT DEFAULTS

            BEFORE INPUT

                SELECT descripcion
                INTO   gc_descripcion
                FROM   tab_entidad
                WHERE  entidad = g_reg.entidad

                DISPLAY gc_descripcion TO desc_entidad

            BEFORE FIELD cod_rechazo_ent
                NEXT FIELD entidad

            #--

            AFTER FIELD entidad
                IF g_reg.entidad IS NULL OR g_reg.entidad = 0 THEN
                    CALL despliega_entidad()
                    DISPLAY g_reg.entidad  TO entidad
                    DISPLAY gc_descripcion TO desc_entidad
                ELSE
                    SELECT descripcion
                    INTO   gc_descripcion
                    FROM   tab_entidad
                    WHERE  entidad = g_reg.entidad

                    IF STATUS = NOTFOUND THEN
                        ERROR "No existe la entidad"
                        NEXT FIELD entidad
                    ELSE
                        DISPLAY gc_descripcion TO desc_entidad
                    END IF
                END IF

            #--
            AFTER FIELD tipo_retiro
                IF g_reg.tipo_retiro IS NULL THEN
                    ERROR "El campo no puede ser nulo"
                    NEXT FIELD tipo_retiro
                END IF

            #--
            AFTER FIELD des_corta
                IF g_reg.des_corta IS NULL THEN
                    ERROR "El campo no puede ser nulo"
                    NEXT FIELD des_corta
                END IF

            #--
            AFTER FIELD des_larga
                IF g_reg.des_larga IS NULL THEN
                    ERROR "El campo no puede ser nulo"
                    NEXT FIELD des_larga
                END IF

                CALL Pregunta()

                IF aux_pausa MATCHES "[Ss]" THEN

                    LET gd_fecha_actualiza = HOY

                    UPDATE ret_rechazo_grl
                    SET    entidad         = g_reg.entidad     ,
                           tipo_retiro     = g_reg.tipo_retiro ,
                           des_corta       = g_reg.des_corta   ,
                           des_larga       = g_reg.des_larga   ,
                           usuario         = seg_usuario       ,
                           fecha_actualiza = gd_fecha_actualiza
                    WHERE  cod_rechazo_ent = g_reg.cod_rechazo_ent

                    ERROR "REGISTRO MODIFICADO"
                    SLEEP 2

                    CALL Inicializa()
                ELSE
                    ERROR "PROCESO DE MODIFICACION,CANCELADO"
                    SLEEP 2
                END IF

                ERROR ""
                EXIT INPUT

            ON KEY ( INTERRUPT )
                CALL Inicializa()
                EXIT INPUT
        END INPUT
    ELSE
        ERROR "ARCHIVO DE CODIGO DE RECHAZOS.... VACIO"
    END IF

    CALL Inicializa()

    CLEAR SCREEN

END FUNCTION

################################################################################
FUNCTION Elimina()

    LET pos = 2

    IF (pos-1) >= 1 THEN

        CALL  SET_COUNT(pos-1)

        OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1352" ATTRIBUTE(BORDER)
        DISPLAY " (ENTER) Consulta                                            (Ctrl-c) Salir   " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
        DISPLAY "              Escoja con < ENTER > el rechazo de afiliado a eliminar          " AT 2,1
        DISPLAY "                            CODIGOS DE RECHAZO                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

        LET int_flag = FALSE

        CONSTRUCT cla_where ON   cod_rechazo_ent,
                                 entidad,
                                 tipo_retiro
                            FROM cod_rechazo_ent,
                                 entidad,
                                 tipo_retiro
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

        LET sel_where = "SELECT * FROM ret_rechazo_grl WHERE ",
                         cla_where CLIPPED,
                        " ORDER BY 1,2,3 "

        PREPARE query2 FROM sel_where

        DECLARE cursor_3 CURSOR FOR query2

        LET pos = 1

        FOREACH cursor_3 INTO l_record[pos].*
            LET pos = pos +1
        END FOREACH

        INITIALIZE l_record[pos].* TO NULL

        IF (pos-1) >= 1 THEN

            CALL SET_COUNT(pos-1)

            DISPLAY ARRAY l_record TO scr_1.*

                ON KEY (CONTROL-M)
                    LET pos = ARR_CURR()
                    LET g_reg.cod_rechazo_ent   = l_record[pos].cod_rechazo_ent
                    LET g_reg.entidad           = l_record[pos].entidad
                    LET g_reg.tipo_retiro       = l_record[pos].tipo_retiro
                    LET g_reg.des_corta         = l_record[pos].des_corta
                    LET g_reg.des_larga         = l_record[pos].des_larga

                    EXIT DISPLAY

                ON KEY (INTERRUPT)
                    ERROR "Usted debe escojer un registro"
                    LET pos = ARR_CURR()
            END DISPLAY

            CLOSE WINDOW ventana_2
        ELSE
            ERROR "ARCHIVO DE CODIGO DE RECHAZOS.... VACIO"
            SLEEP 2
            ERROR ""
            CLOSE WINDOW ventana_2
            RETURN
        END IF

        DISPLAY "" AT 1,1
        DISPLAY "" AT 2,1
        DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
        DISPLAY " ELIMINA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

        DISPLAY BY NAME  g_reg.*

        SELECT descripcion
        INTO   gc_descripcion
        FROM   tab_entidad
        WHERE  entidad = g_reg.entidad

        DISPLAY gc_descripcion TO desc_entidad

        CALL Pregunta()

        IF aux_pausa MATCHES "[Ss]" THEN

            DELETE
            FROM   ret_rechazo_grl
            WHERE  cod_rechazo_ent = g_reg.cod_rechazo_ent
            AND    entidad         = g_reg.entidad
            AND    tipo_retiro     = g_reg.tipo_retiro

            ERROR "REGISTRO ELIMINADO"
            SLEEP 2
        ELSE
            ERROR "ELIMINAR CANCELADO"
            SLEEP 2
        END IF

        ERROR ""

        CALL Inicializa()
    ELSE
        ERROR "ARCHIVO DE CODIGO DE RECHAZOS.... VACIO"
    END IF

    CALL Inicializa()

    CLEAR SCREEN

END FUNCTION


################################################################################

FUNCTION Pregunta()

    PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa

END FUNCTION

################################################################################


FUNCTION impresion(pos)
   
    DEFINE 
        i       ,
        pos     SMALLINT

    
    LET g_impre = gr_seg_modulo.ruta_listados CLIPPED,"/",seg_usuario CLIPPED,
                  ".IMP_RET_RECHAZO_GRL", HOY USING "dd-mm-yyyy" CLIPPED

    START REPORT rpt_ret_rechazo_grl TO g_impre

    FOR i = 1 TO (pos+1)
        LET g_reg.cod_rechazo_ent   = l_record[i].cod_rechazo_ent
        LET g_reg.entidad           = l_record[i].entidad
        LET g_reg.tipo_retiro       = l_record[i].tipo_retiro
        LET g_reg.des_corta         = l_record[i].des_corta
        LET g_reg.des_larga         = l_record[i].des_larga
        
        IF g_reg.cod_rechazo_ent IS NULL THEN
            EXIT FOR
        END IF

        OUTPUT TO REPORT rpt_ret_rechazo_grl(g_reg.*)
    END FOR

    FINISH REPORT rpt_ret_rechazo_grl

    ERROR "LISTADO GENERADO..."
    SLEEP 2
    ERROR ""

    LET g_lista = "lp ", g_impre
--    LET g_lista = "vi ",g_impre
    RUN g_lista

END FUNCTION

#####################################################################
REPORT rpt_ret_rechazo_grl(lr_reg)

    DEFINE lr_reg  RECORD
        cod_rechazo_ent     LIKE ret_rechazo_grl.cod_rechazo_ent,
        entidad             LIKE ret_rechazo_grl.entidad,
        tipo_retiro         LIKE ret_rechazo_grl.tipo_retiro,
        des_corta           LIKE ret_rechazo_grl.des_corta,
        des_larga           LIKE ret_rechazo_grl.des_larga
    END RECORD

    OUTPUT
        TOP MARGIN    0
        BOTTOM MARGIN 0
        LEFT MARGIN   0
        RIGHT MARGIN  0
        PAGE LENGTH  45
   
    FORMAT 
        
        PAGE HEADER
            PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
                  '\033e\033(s23H'
                  
            PRINT COLUMN 001,"TABM135",
                  COLUMN 050,"LISTADO DE CATALOGO DE RECHAZOS",
                  COLUMN 130,TODAY USING "dd-mm-yyyy",
                            '\033015'
            
            SKIP 3 LINES

            PRINT COLUMN 002,"CODIGO",
                  COLUMN 010,"ENTIDAD",
                  COLUMN 019,"T. RETIRO",
                  COLUMN 038,"DESCRIPCION CORTA",
                  COLUMN 090,"DESCRIPCION LARGA",
                            '\033015'
            SKIP 1 LINE
        
        ON EVERY ROW
            PRINT COLUMN 001, lr_reg.cod_rechazo_ent,
                  COLUMN 008, lr_reg.entidad        ,
                  COLUMN 022, lr_reg.tipo_retiro    ,
                  COLUMN 032, lr_reg.des_corta      ,
                  COLUMN 070, lr_reg.des_larga      ,
                            '\033015'
             
        PAGE TRAILER
            SKIP 2 LINES
            
            PRINT COLUMN 060," Pagina : ",PAGENO USING "<<<<<",
                            '\033015'
      
        ON LAST ROW
            SKIP 4 LINES
            
            PRINT COLUMN 001," Total de registros : ", COUNT(*) USING "<<<<",
                            '\033015'

END REPORT


FUNCTION despliega_entidad()

    DEFINE ra_reg ARRAY[100] OF RECORD
        codigo            CHAR(02),
        descripcion       CHAR(50)
    END RECORD

    DEFINE
        prepare_1    CHAR(200),
        x_buscar     CHAR(030)

    OPEN WINDOW tabm135_01 AT 05,12 WITH FORM "TABM1353" ATTRIBUTE(BORDER)
    DISPLAY "                      TIPO ENTIDAD                      " AT 2,1  ATTRIBUTE(REVERSE)
    INPUT BY NAME x_buscar

        BEFORE FIELD x_buscar
            LET x_buscar = "*"

        AFTER FIELD x_buscar
            IF x_buscar IS NULL THEN
                ERROR "    DESCRIPCION A BUSCAR NO PUEDE SER NULA"
                ATTRIBUTE(NORMAL)
                NEXT FIELD x_buscar
            ELSE
                EXIT INPUT
            END IF

    END INPUT

    WHILE TRUE
        LET prepare_1 = " SELECT * FROM tab_entidad ",
                        " WHERE descripcion MATCHES ",'"',x_buscar CLIPPED,'"',
                        " ORDER BY 1 " CLIPPED

        PREPARE pre_5 FROM prepare_1
        DECLARE cur_5 CURSOR FOR pre_5

        LET pos = 1

        FOREACH cur_5 INTO ra_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
                ERROR "    Fue Sobrepasada la capacidad maxima del arreglo"
                ATTRIBUTE(NORMAL)
                EXIT FOREACH
            END IF
        END FOREACH

        IF (pos-1) < 1 THEN
            ERROR "    ARCHIVO TIPO DE RETIRO VACIO"
            ATTRIBUTE(NORMAL)
        END IF

        CALL SET_COUNT(pos-1)
        DISPLAY ARRAY ra_reg TO scr_1.*
            ON KEY ( CONTROL-C )
                LET pos = 0
                EXIT DISPLAY

            ON KEY ( INTERRUPT )
                LET pos = 0
                EXIT DISPLAY

            ON KEY ( CONTROL-M )
                LET pos = ARR_CURR()
                LET g_reg.entidad  = ra_reg[pos].codigo
                LET gc_descripcion = ra_reg[pos].descripcion
                EXIT DISPLAY
        END DISPLAY

        IF pos <> 0 THEN
            EXIT WHILE
        END IF
    END WHILE

    CLOSE WINDOW tabm135_01

END FUNCTION

