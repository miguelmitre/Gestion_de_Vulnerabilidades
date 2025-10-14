############################################################################
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       => E.F.P.                                               #
#Programa AFIB025  => CONSULTA AFILIACION REGISTRO POR INTERNET            #
#Fecha             => 31 DE JULIO DE 2006                                  #
#Autor             => VERONICA LOPEZ SANCHEZ                               #
#Sistema           => afi                                                  #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg ARRAY[20000] OF RECORD
        n_seguro            CHAR(11),
        curp                CHAR(18),
        n_folio             DECIMAL(10,0),
        fecha_rec_sol_elect DATE,
        tipo_trab           SMALLINT,
        desc_solicitud      CHAR(15)
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
        cla_where CHAR(2000)

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP
    CALL STARTLOG("AFIB029.log")
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

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "AFIB0291" ATTRIBUTE( BORDER)

    DISPLAY " AFIB029      CONSULTA AFILIACION REGISTRO AFORE POR INTERNET                 " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)
    
    MENU "MENU   "
        COMMAND "Consulta" " Consulta Afiliacion Registro por Internet"
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

    DISPLAY "                                     FOLIO      FECHA    TIPO                 " AT 5,1 ATTRIBUTE(REVERSE)
    DISPLAY "    NSS             CURP             SAFRE     PRESENT   SOLIC  DESCRIPCION   " AT 6,1 ATTRIBUTE(REVERSE)

    LET int_flag = FALSE
 
    CONSTRUCT cla_where ON n_seguro,
                           curp,
                           n_folio,
                           fecha_rec_sol_elect
                           
                      FROM n_seguro,
                           curp, 
                           n_folio,
                           fecha_rec_sol_elect

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        RETURN
    END IF

    LET sel_where = " SELECT n_seguro,",
                           " curp,",
                           " n_folio,",
                           " fecha_rec_sol_elect,",
                           " tipo_trab",
                    " FROM   afi_det_reg_internet",
                    " WHERE ", cla_where CLIPPED,
                    " ORDER BY 1,3" CLIPPED

    LET sel_where = sel_where CLIPPED

    #LET sel_where = "echo ",sel_where clipped, " > x.sql"  
    #RUN sel_where

    PREPARE qry_consul FROM sel_where 

    DECLARE cursor_c CURSOR FOR qry_consul

    LET pos = 1

    FOREACH cursor_c INTO reg[pos].n_seguro THRU reg[pos].tipo_trab

        IF reg[pos].tipo_trab = 1 THEN
            LET reg[pos].tipo_trab = 11
            LET reg[pos].desc_solicitud = "REG. INTERNET"
        ELSE
            LET reg[pos].tipo_trab = 12
            LET reg[pos].desc_solicitud = "NO AFILIADOS"
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
