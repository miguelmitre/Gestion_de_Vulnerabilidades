############################################################################
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       => E.F.P.                                               #
#Programa AFIB025  => CONSULTA ARCHIVO NOTIFICACION DOMICILIOS OP 17       #
#Fecha             => 23 DE FEBRERO DE 2007                                #
#Autor             => FERNANDO HERRERA HERNANDEZ                           #
#Sistema           => afi                                                  #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg ARRAY[20000] OF RECORD
        nss                 CHAR(11),
        curp                CHAR(18),
        tipo_ent_not        CHAR(02),     
        fecha_act           DATE,
        cod_resultado       CHAR(02),
        diag_proceso        CHAR(15),
        ldesc_proc          CHAR(40)
    END RECORD

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
        y         SMALLINT  ,
        pos       INTEGER   ,
        sel_where CHAR(2000),
        cla_where CHAR(2000),
        ldiag_proceso CHAR(03)

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP
    CALL STARTLOG("AFIB035.log")
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
    WHERE  modulo_cod = 'afi'

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "AFIB0351" ATTRIBUTE( BORDER)

    DISPLAY " AFIB035      CONSULTA NOTIFICACION DOMICILIOS OPERACION 17                   " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)
    
    MENU "MENU   "
        COMMAND "Consulta" " Consulta Notificacion Domicilios Op 17"
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

    DISPLAY "                                TIP  FECHA    COD                              " AT 5,1 ATTRIBUTE(REVERSE)
    DISPLAY "    NSS             CURP        NOT ACTUALIZA RES   DIAGNOSTICO PROCESO        " AT 6,1 ATTRIBUTE(REVERSE)

    LET int_flag = FALSE
 
    CONSTRUCT cla_where ON nss,     
                           curp,
                           tipo_ent_not,
                           fecha_act
                           
                      FROM nss,     
                           curp, 
                           tipo_ent_not,
                           fecha_act

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        RETURN
    END IF

    LET sel_where = " SELECT nss,",
                           " curp,",
                           " tipo_ent_not,",
                           " fecha_act,",
                           " cod_resultado,",
                           " diag_proceso",
                    " FROM   afi_not_domicilio",
                    " WHERE ", cla_where CLIPPED,
                    " ORDER BY 1,3,5,4 DESC" CLIPPED

    LET sel_where = sel_where CLIPPED

    #LET sel_where = "echo ",sel_where clipped, " > x.sql"  
    #RUN sel_where

    PREPARE qry_consul FROM sel_where 

    DECLARE cursor_c CURSOR FOR qry_consul

    LET pos = 1

    FOREACH cursor_c INTO reg[pos].nss THRU reg[pos].ldesc_proc

        LET ldiag_proceso = reg[pos].diag_proceso[1,3]

        SELECT a.rdeta_desc_c
          INTO reg[pos].ldesc_proc
          FROM tab_rdeta a
         WHERE a.rdeta_cod  = ldiag_proceso
           AND a.modulo_cod = 'afi'

        IF ldiag_proceso = "000" THEN
           LET reg[pos].ldesc_proc = 'ACEPTADO'
        END IF

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
