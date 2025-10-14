############################################################################
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       => E.F.P.                                               #
#Programa ACRB041  => CONSULTA NSS CREDITOS EN GARANTIA POR USO DE CREDITO #
#Fecha             => 06 FEBRERO 2003                                      #
#Autor             => JOSE LUIS SALDIVAR C                                 #
#Sistema           => ACR                                                  #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg ARRAY[20000] OF RECORD
        folio              INTEGER ,
        nss_afore          CHAR(11),
        fecha_presentacion DATE    ,
        fecha_traspaso     DATE    ,
        estado             SMALLINT,
        desc_estado        CHAR(9)
    END RECORD

    DEFINE 
        nss_afore          CHAR(11),
        fecha_presentacion DATE    ,
        fecha_traspaso     DATE    ,
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
    WHERE  modulo_cod = 'acr'

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "ACRB0411" ATTRIBUTE( BORDER)

    DISPLAY " ACRB041         CONSULTA REGISTROS CON USO DE CREDITO                        "
    AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    MENU " USO DE CREDITO "
        COMMAND "Consulta" " Consulta Uso Credito "
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

    DISPLAY "   FOLIO       NSS      FECHA SOLICITUD  FECHA TRASPASO  ESTADO SOLICITUD       " AT 6,1 ATTRIBUTE(REVERSE)

    LET int_flag = FALSE

    CONSTRUCT cla_where ON folio,
                           nss_afore,
                           fecha_presentacion,
                           fecha_traspaso
                      FROM folio,
                           nss_afore,
                           fecha_presentacion,
                           fecha_traspaso

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        RETURN
    END IF

    LET sel_where = " SELECT a.folio,",
                           " a.nss_afore,",
                           " a.fecha_presentacion,",
                           " a.fecha_traspaso,",
                           " a.estado ",
                    " FROM   acr_det_garantia a ",
                    " WHERE ", cla_where CLIPPED

    LET sel_where = sel_where CLIPPED

    #LET sel_where = "echo ",sel_where clipped, " > x.sql"  
    #RUN sel_where

    PREPARE qry_consul FROM sel_where 

    DECLARE cursor_c CURSOR FOR qry_consul

    LET pos = 1

    FOREACH cursor_c INTO reg[pos].folio THRU reg[pos].estado
        IF reg[pos].estado = 0 THEN
            SELECT 'X'
            FROM   dis_cuenta dcta
            WHERE  dcta.folio = reg[pos].folio
            AND    dcta.nss = reg[pos].nss_afore
            AND    dcta.subcuenta = 4
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
                LET reg[pos].desc_estado = 'LIQUIDADA'
            ELSE
                LET reg[pos].desc_estado = 'PROVISIONADA'
            END IF
        ELSE
            SELECT 'X'
            FROM   acr_det_garantia adg
            WHERE  adg.folio = reg[pos].folio
            AND    adg.nss_afore = reg[pos].nss_afore
            AND    adg.cod_result_operac = '02'
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
                LET reg[pos].desc_estado = 'DEV. RECH.'
            ELSE
                LET reg[pos].desc_estado = 'DEVUELTA'
            END IF
        END IF

        LET pos = pos + 1
    END FOREACH

    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)
        DISPLAY ARRAY reg TO scr_1.*
	ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")

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

