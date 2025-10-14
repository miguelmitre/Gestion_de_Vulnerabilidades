############################################################################
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       => E.F.P.                                               #
#Programa TAAB016  => CONSULTA NSS DEVUELTOS DE AFORE RECEPTORA            #
#Fecha             => 10 DE OCTUBRE DE 2001                                #
#Autor             => MAURO MUNIZ CABALLERO                                #
#Sistema           => TAA                                                  #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg ARRAY[30000] OF RECORD
        n_seguro           CHAR(11),
        fecha_presentacion DATE    ,
        f_actualiza        DATE    ,
        tipo_traspaso      SMALLINT,
        motivo_rechazo     SMALLINT,
        desc_estado        CHAR(30),
        grado_riesgo       CHAR(02),
        vector_riesgo      CHAR(08)
    END RECORD

    DEFINE reg1            RECORD
        n_seguro           CHAR(11),
        fecha_presentacion DATE    ,
        f_actualiza        DATE    ,
        tipo_traspaso      SMALLINT,
        motivo_rechazo     SMALLINT,
        desc_estado        CHAR(30),
        grado_riesgo       CHAR(02),
        vector_riesgo      CHAR(08)
    END RECORD

    DEFINE 
        n_seguro          CHAR(11),
        fecha_presentacion DATE    ,
        f_actualiza        DATE  

    DEFINE
        g_afore       RECORD LIKE tab_afore_local.*,
        g_paramgrales RECORD LIKE seg_modulo.*

    DEFINE
        HOY       DATE

    DEFINE
        enter     CHAR(1),
        HORA      CHAR(8),
        g_usuario CHAR(8) 

    DEFINE
        i         SMALLINT  ,
        pos       INTEGER   ,
        sel_where CHAR(1000),
        cla_where CHAR(1000)

    DEFINE 
        vdiag_proceso CHAR(03)

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP

    CALL inicio()
    CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY   = TODAY
    LET HORA  = TIME

    SELECT *, USER
    INTO   g_paramgrales.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'taa'

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "TAAB0161" ATTRIBUTE( BORDER)

    DISPLAY " TAAB016         CONSULTA TRASPASOS VALIDACION IMAGENES                        " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    MENU " TRASPASOS RECIBIDOS "
        COMMAND "Consulta" " Consulta Validacion Imagenes "
            CALL Consulta()
        COMMAND "Salir " " Salir de Programa"
            EXIT MENU
    END MENU

END FUNCTION

FUNCTION Inicializa()
#iz------------------

    DEFINE j SMALLINT

    INITIALIZE reg TO NULL

    FOR j = 1 TO 12
        DISPLAY reg[i].* TO scr_1[i].* ATTRIBUTE (NORMAL)
    END FOR

    CLEAR FORM

END FUNCTION

FUNCTION Consulta()
#C-----------------

    DISPLAY "      NSS     F PRES FRECEP  TTAA      MOTIVO RECHAZO           G y V RIESGO   " AT 5,1 ATTRIBUTE(REVERSE)

    LET int_flag = FALSE

    CONSTRUCT cla_where ON n_seguro,
                           fecha_presentacion,
                           f_actualiza,
                           tipo_traspaso,
                           grado_riesgo
                      FROM n_seguro,
                           fecha_presentacion,
                           f_actualiza,
                           tipo_traspaso,
                           grado_riesgo

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        RETURN
    END IF

    LET sel_where = " SELECT a.n_seguro,",
                           " a.fecha_presentacion,",
                           " a.f_actualiza,",
                           " a.tipo_traspaso, ",
                           " a.motivo_rechazo, ",
                           " ' ', ",
                           " a.grado_riesgo, ",
                           " a.vector_riesgo ",
                    " FROM   taa_det_devol a ",
                    " WHERE ", cla_where CLIPPED

    LET sel_where = sel_where CLIPPED

    #LET sel_where = "echo ",sel_where clipped, " > x.sql"  
    #RUN sel_where

    PREPARE qry_consul FROM sel_where 

    DECLARE cursor_c CURSOR FOR qry_consul

    LET pos = 1

    #FOREACH cursor_c INTO reg[pos].n_seguro THRU reg[pos].vector_riesgo 
    FOREACH cursor_c INTO reg1.*

        SELECT desc_rech
          INTO reg1.desc_estado
          FROM tab_dev_taa
         #WHERE cod_rech[1,2]  = reg1.motivo_rechazo
         WHERE cod_rech  = reg1.motivo_rechazo
           AND tipo_diag = 2
        IF STATUS = NOTFOUND THEN

           SELECT a.diag_proceso
             INTO reg1.motivo_rechazo
             FROM taa_det_devol a
            WHERE a.n_seguro           = reg1.n_seguro
              AND a.fecha_presentacion = reg1.fecha_presentacion
              AND a.f_actualiza        = reg1.f_actualiza
              AND a.diag_proceso      IS NOT NULL 

           SELECT a.desc_rech
             INTO reg1.desc_estado
             FROM tab_dev_taa a
            WHERE a.cod_rech  = reg1.motivo_rechazo 
              AND a.tipo_diag = 1

           IF reg1.motivo_rechazo IS NULL THEN
              SELECT a.diag_imagen[1,2], b.desc_rech
                INTO reg1.motivo_rechazo, reg1.desc_estado
                FROM taa_det_devol a, tab_dev_taa b
               WHERE a.n_seguro           = reg1.n_seguro
                 AND a.fecha_presentacion = reg1.fecha_presentacion
                 AND a.f_actualiza        = reg1.f_actualiza
                 AND a.diag_imagen[1,2]   = cod_rech
                 AND b.tipo_diag          = 2
                 AND a.diag_imagen       IS NOT NULL
           END IF
        END IF

        LET reg[pos].* = reg1.*

        LET pos = pos + 1
    END FOREACH

    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)
        DISPLAY ARRAY reg TO scr_1.*

        IF int_flag = TRUE THEN
            LET int_flag = FALSE
            ERROR "BUSQUEDA TERMINADA..."
            SLEEP 2
            ERROR ""
            CLEAR FORM
            RETURN
        END IF
    ELSE
        ERROR "REGISTROS CON ESAS CONDICIONES NO EXISTEN"
        SLEEP 2
        ERROR ""
    END IF

END FUNCTION

