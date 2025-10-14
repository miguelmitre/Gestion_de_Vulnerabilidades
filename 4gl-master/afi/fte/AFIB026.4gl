############################################################################
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       => E.F.P.                                               #
#Programa AFIB026  => CONSULTA LIBERACION DE PENDIENTES NO AFILIADOS       #
#Fecha             => 08 DE ABRIL DE 2006                                  #
#Autor             => FERNANDO HERRERA HERNANDEZ                           #
#Sistema           => AFI                                                  #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg ARRAY[20000] OF RECORD
        n_unico       CHAR(18),
        cod_operacion CHAR(2) ,
        diag_proceso  CHAR(3) ,
        rdeta_desc_c  CHAR(60),
        frecafor      DATE
    END RECORD

    DEFINE
        n_unico       CHAR(18),
        cod_operacion DATE    ,
        diag_proceso  DATE    ,
        rdeta_desc_c  INTEGER

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
    CALL STARTLOG("AFIB026.log")
    CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY   = TODAY
    LET HORA  = TIME

    SELECT *, USER
    INTO   g_paramgrales.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "AFIB0231" ATTRIBUTE( BORDER)

    DISPLAY " AFIB026 CONSULTA RESULTADO CERTIF LIB PENDIENTES NO AFILIADOS                 " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    MENU " Cert Lib Pendientes No Afil "
        COMMAND "Consulta" " Consulta Certificacion"
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

    DISPLAY "       CURP         COD OP DIAGNOSTICO                             FECHA CERT  " AT 5,1 ATTRIBUTE(REVERSE)

    LET int_flag = FALSE

    CONSTRUCT cla_where ON n_unico,
                           cod_operacion,
                           diag_proceso,
                           rdeta_desc_c
                      FROM n_unico,
                           cod_operacion,
                           diag_proceso,
                           rdeta_desc_c

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        RETURN
    END IF

    LET sel_where = " SELECT c.n_unico,",
                           " a.cod_operacion,",
                           " a.diag_proceso[1,3],",
                           " b.rdeta_desc_c,",
                           " c.frecafor ",
                    " FROM   safre_tmp:detalle_ind3 a, ",
                    "        OUTER(safre_af:tab_rdeta b), ",
                    "        safre_af:afi_solicitud c ",
                    " WHERE ", cla_where CLIPPED,
                    " AND    c.n_unico  =  a.curp_solicitud ",
                    " AND    c.tipo_solicitud = 8 ",
                    " AND    a.diag_proceso[1,3] = b.rdeta_cod ",
                    #" AND    c.status_captura = b.rdeta_cod ",
                    #" AND    c.status_captura = a.diag_proceso[1,3] ",
                    " AND    b.modulo_cod = 'afi' ",
                    " ORDER BY 2 DESC, 3" CLIPPED

    LET sel_where = sel_where CLIPPED

    PREPARE qry_consul FROM sel_where

    DECLARE cursor_c CURSOR FOR qry_consul

    LET pos = 1

    FOREACH cursor_c INTO reg[pos].n_unico THRU reg[pos].frecafor
        {IF reg[pos].cod_operacion = '09' THEN
            LET reg[pos].rdeta_desc_c = 'ACEPTADO'
        END IF
        IF reg[pos].cod_operacion = '10' THEN
            LET reg[pos].rdeta_desc_c = 'ACEPTADO'
        END IF
        IF reg[pos].cod_operacion = '11' THEN
            LET reg[pos].rdeta_desc_c = 'RECHAZADO'
        END IF
        IF reg[pos].cod_operacion = '12' THEN
            LET reg[pos].rdeta_desc_c = 'RECHAZADO'
        END IF
        IF reg[pos].cod_operacion = '13' THEN
            LET reg[pos].rdeta_desc_c = 'RECHAZADO'
        END IF}

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

