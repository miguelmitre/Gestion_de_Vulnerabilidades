############################################################################
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       => E.F.P.                                               #
#Programa AFIB025  => CONSULTA AFILIACION POR TRASPASO AFORE POR INTERNET  #
#Fecha             => 30 DE SEPTIEMBRE DE 2005                             #
#Autor             => EDUARDO JOAQUIN RESENDIZ MEDINA                      #
#Sistema           => xxx                                                  #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg ARRAY[20000] OF RECORD
        n_seguro           CHAR(11),
        folio_sol          CHAR(10),
        n_folio            SMALLINT,
        fecha_presentacion DATE    ,
        status_interno     SMALLINT
    END RECORD

    DEFINE v_status_interno SMALLINT

    DEFINE 
        n_seguro      CHAR(11),
        folio_sol     CHAR(10),
        n_folio       CHAR(08),
        rdeta_desc_c  DATE

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
        cla_where CHAR(1000),
        v_sel_1   CHAR(150)

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP
    CALL STARTLOG("AFIB025.log")
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

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "AFIB0251" ATTRIBUTE( BORDER)

    DISPLAY " AFIB025      CONSULTA AFILIACION TRASPASO AFORE POR INTERNET           " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    MENU " Afiliacion por Traspasos por Internet"
        COMMAND "Consulta" " Consulta Afiliacion por Traspaso por Internet"
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

    DISPLAY "TIPO DE SOLICITUD 9 (TRASPASO POR INTENET)" AT 4,1 --ATTRIBUTE(REVERSE)
    DISPLAY "     NSS    FOLIO SOLICITUD   FOLIO SAFRE   FECHA PRESENTACION   EDO.INTERNO" AT 5,1 ATTRIBUTE(REVERSE)

    LET int_flag = FALSE

    CONSTRUCT cla_where ON nss,
                           folio_sol,
                           n_folio
                           
                      FROM n_seguro,
                           folio_sol,
                           n_folio

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        RETURN
    END IF

    LET sel_where = " SELECT nss,",
                           " folio_sol,",
                           " n_folio,",
                           " fecha_presentacion",
                    " FROM   afi_det_internet",
                    " WHERE ", cla_where CLIPPED,
                    " ORDER BY 1,3" CLIPPED

    LET sel_where = sel_where CLIPPED

    #LET sel_where = "echo ",sel_where clipped, " > x.sql"  
    #RUN sel_where

    PREPARE qry_consul FROM sel_where 

    DECLARE cursor_c CURSOR FOR qry_consul

    LET pos = 1

    FOREACH cursor_c INTO reg[pos].n_seguro THRU reg[pos].fecha_presentacion


        LET v_sel_1 =  "SELECT status_interno ",
                       "FROM   afi_solicitud ",
                       "WHERE  n_seguro = ","'",reg[pos].n_seguro CLIPPED,"'",
                       "AND    n_folio  = ", reg[pos].n_folio 
        
        PREPARE primera_ins FROM v_sel_1
        EXECUTE primera_ins INTO v_status_interno

        LET reg[pos].status_interno = v_status_interno

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
