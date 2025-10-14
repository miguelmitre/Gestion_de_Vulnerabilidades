############################################################################
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       => E.F.P.                                               #
#Programa TAAB013  => CONSULTA NSS TRASPASO DE AFORE RECEPTORA             #
#Fecha             => 10 DE OCTUBRE DE 2001                                #
#Autor             => MAURO MUNIZ CABALLERO                                #
#Sistema           => TAA                                                  #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE 
        vfolio     INTEGER,
        partic_v97 DECIMAL(18,6),
        partic_v92 DECIMAL(18,6),
        saldo_v97  DECIMAL(18,6),
        saldo_v92  DECIMAL(18,6),
        saldo_ret  DECIMAL(18,6),
        saldo_ces  DECIMAL(18,6),
        saldo_pat  DECIMAL(18,6),
        saldo_ven  DECIMAL(18,6),
        saldo_soc  DECIMAL(18,6),
        saldo_sar  DECIMAL(18,6),
        saldo_acr  DECIMAL(18,6),
        saldo_aar  DECIMAL(18,6)

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

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "TAAB0131" ATTRIBUTE( BORDER)

    DISPLAY " TAAB013         CONSULTA SALDOS PREVIOS AFORE RECEPTORA                       " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    MENU " SALDOS PREVIOS "
        COMMAND "Consulta" " Consulta Saldos Previos "
            CALL Consulta()
        COMMAND "Salir " " Salir de Programa"
            EXIT MENU
    END MENU

END FUNCTION

FUNCTION Consulta()
#C-----------------

    INPUT BY NAME vfolio

    SELECT "X"
    FROM   taa_sum_recepcion
    WHERE  folio = vfolio  

    IF SQLCA.SQLCODE <> 0 THEN
        ERROR "REGISTROS CON ESAS CONDICIONES NO EXISTEN"
        RETURN
    END IF

END FUNCTION

