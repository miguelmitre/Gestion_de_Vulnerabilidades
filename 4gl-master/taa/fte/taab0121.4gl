############################################################################
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       => E.F.P.                                               #
#Programa TAAB012  => CONSULTA NSS TRASPASO DE AFORE RECEPTORA             #
#Fecha             => 10 DE OCTUBRE DE 2001                                #
#Autor             => MAURO MUNIZ CABALLERO                                #
#Sistema           => TAA                                                  #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg ARRAY[30000] OF RECORD
        n_seguro           CHAR(11),
        fecha_presentacion DATE    ,
        fecha_trasp        DATE    ,
        folio              INTEGER ,
        estado             SMALLINT,
        desc_estado        CHAR(10),
        fecha_vol_pat      DATE,
        fecha_vol_ven      DATE
    END RECORD

    DEFINE 
        n_seguro          CHAR(11),
        fecha_presentacion DATE    ,
        fecha_trasp        DATE    ,
        folio              INTEGER 

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

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "taab01211" ATTRIBUTE( BORDER)

    DISPLAY " TAAB012         CONSULTA TRASPASOS DE AFORE RECEPTORA                         " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    MENU " TRASPASOS RECIBIDOS "
        COMMAND "Consulta" " Consulta Traspasos"
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

    DISPLAY "   NSS      F PRESENTA F LIQUIDAC  FOLIO TIPO TRASPASO F VOL PAT F VOL VEN     " AT 5,1 ATTRIBUTE(REVERSE)

    LET int_flag = FALSE

    CONSTRUCT cla_where ON nss,
                           fecha_presentacion,
                           fecha_mov_banxico,
                           folio
                      FROM n_seguro,
                           fecha_presentacion,
                           fecha_trasp,
                           folio

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        RETURN
    END IF

    LET sel_where = " SELECT a.nss,",
                           " a.fecha_presentacion,",
                           " a.fecha_mov_banxico,",
                           " a.folio,",
                           " a.tipo_traspaso, ",
                           " a.fecha_vol_pat, ",
                           " a.fecha_vol_ven  ",
                    " FROM   taa_rcv_recepcion a ",
                    " WHERE ", cla_where CLIPPED

    LET sel_where = sel_where CLIPPED

    #LET sel_where = "echo ",sel_where clipped, " > x.sql"  
    #RUN sel_where

    PREPARE qry_consul FROM sel_where 

    DECLARE cursor_c CURSOR FOR qry_consul

    LET pos = 1

    FOREACH cursor_c INTO reg[pos].n_seguro THRU reg[pos].estado
        CASE reg[pos].estado
            WHEN  1 LET reg[pos].desc_estado = 'NORMAL'
            WHEN 12 LET reg[pos].desc_estado = 'UNIFAC'
            WHEN 20 LET reg[pos].desc_estado = 'ASI-UNI'
            WHEN 51 LET reg[pos].desc_estado = 'ASIGNAC'
        END CASE

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

