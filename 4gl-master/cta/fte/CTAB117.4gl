############################################################################
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       => E.F.P.                                               #
#Programa CTAB117  => CONSULTA ORDEN DE SELECCION ACEPTADO                 #
#Fecha             => 21 DE SEPTIEMBRE DE 2005                             #
#Autor             => EDUARDO JOAQUIN RESENDIZ MEDINA                      #
#Sistema           => CTA                                                  #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg ARRAY[20000] OF RECORD
        nss             CHAR(11),
        curp            CHAR(18),
        cod_result      CHAR(2) ,
        motivo_procede  CHAR(3) ,
        fecha_operacion DATE
    END RECORD

    DEFINE reg_7 ARRAY[20000] OF RECORD
        nss             CHAR(11),
        curp            CHAR(18),
        cod_result      CHAR(2) ,
        rdeta_desc_c    CHAR(15),
        motivo_procede  CHAR(3) ,
        desc_motivo     CHAR(40),
        fecha_operacion DATE
    END RECORD

    DEFINE
        nss             CHAR(11),
        curp            CHAR(18),
        cod_result      CHAR(2) ,
        motivo_procede  CHAR(3) ,
        rdeta_desc_c    CHAR(15),
        fecha_operacion DATE    ,
        desc_motivo     CHAR(40)

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
    WHERE  modulo_cod = 'cta'

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "CTAB1171" ATTRIBUTE( BORDER)

    DISPLAY " CTAB117     CONSULTA RESULTADO ORDEN DE SELECCION ACEPTADO                   " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    MENU " ORDEN DE SELECCION ACEPTADO "
        COMMAND "Consulta" " Consulta Orden"
            CALL Consulta()
        COMMAND "Salir " " Salir de Programa"
            EXIT MENU
    END MENU

END FUNCTION

FUNCTION Inicializa()
#iz------------------

    DEFINE j SMALLINT

    INITIALIZE reg TO NULL

{    FOR j = 1 TO 12
        DISPLAY reg[i].* TO scr_1[i].* ATTRIBUTE (NORMAL)
    END FOR
}
    FOR j = 1 TO 12
        DISPLAY reg_7[i].* TO scr_1[i].* ATTRIBUTE (NORMAL)
    END FOR

    CLEAR FORM

END FUNCTION

FUNCTION Consulta()
#C-----------------

    DISPLAY "    NSS            CURP        COD.RESULT  COD/DESC.MOTIVO         FECHA OPER  " AT 5,1 ATTRIBUTE(REVERSE)

    LET int_flag = FALSE

    CONSTRUCT cla_where ON nss,
                           curp,
                           cod_result
--                           motivo_procede,

                      FROM nss,
                           curp,
                           cod_result
--                           motivo_procede,


    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        RETURN
    END IF

    LET sel_where = " SELECT a.nss,a.curp,",
                           " a.cod_result,",
                           " a.motivo_procede,",
                           " a.fecha_operacion ",
                    " FROM   cta_cza_afil_acep a",
                    " WHERE ", cla_where CLIPPED,
                    " ORDER BY fecha_operacion desc, nss " CLIPPED

    LET sel_where = sel_where CLIPPED

    #LET sel_where = "echo ",sel_where clipped, " > x.sql"
    #RUN sel_where

    PREPARE qry_consul FROM sel_where

    DECLARE cursor_c CURSOR FOR qry_consul

    LET pos = 1

    FOREACH cursor_c INTO reg[pos].nss THRU reg[pos].fecha_operacion
 
        IF reg[pos].cod_result = '01' THEN
            LET reg_7[pos].rdeta_desc_c = 'PROCEDENTE'
        END IF

        IF reg[pos].cod_result = '02' THEN
            LET reg_7[pos].rdeta_desc_c = 'NO PROCEDENTE'
        END IF

        IF reg[pos].motivo_procede = '000' THEN
            LET reg_7[pos].desc_motivo = 'SOLICITUD ACEPTADA'
        END IF

        IF reg[pos].motivo_procede = '001' THEN
            LET reg_7[pos].desc_motivo = 'EN ALGUN PROC.OPERATIVO'
        END IF

        IF reg[pos].motivo_procede = '001' THEN
            LET reg_7[pos].desc_motivo = 'INCONSISTENCIA DE DATOS DE TRABAJADOR'
        END IF

        LET reg_7[pos].nss             = reg[pos].nss
        LET reg_7[pos].curp            = reg[pos].curp
        LET reg_7[pos].cod_result      = reg[pos].cod_result
        LET reg_7[pos].rdeta_desc_c    = reg_7[pos].rdeta_desc_c
        LET reg_7[pos].motivo_procede  = reg[pos].motivo_procede
        LET reg_7[pos].desc_motivo     = reg_7[pos].desc_motivo
        LET reg_7[pos].fecha_operacion = reg[pos].fecha_operacion

        LET pos = pos + 1

    END FOREACH

    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)
        DISPLAY ARRAY reg_7 TO scr_1.*

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
