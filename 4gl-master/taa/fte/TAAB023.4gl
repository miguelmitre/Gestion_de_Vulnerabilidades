#######################################################################
#Proyecto          => AFORES ( MEXICO )                               #
#Propietario       => E.F.P.                                          #
#Programa TAAB023  => LANZA PROVISION VIRTUAL REGISTROS APORTACIONES  #
#Fecha             => 13 DE FEBRERO DE 2009                           #
#Autor             => FERNANDO HERRERA HERNANDEZ                      #
#Sistema           => TRA                                             #
#######################################################################

DATABASE safre_af

GLOBALS

    DEFINE
        HOY          DATE,
        fecha_ini    DATETIME YEAR TO SECOND

    DEFINE
        opc             CHAR(01),
        vusuario        CHAR(8) ,
        vfolio          INTEGER ,
        bnd_proceso     SMALLINT

    DEFINE
       exe_nohup CHAR(500)

    DEFINE g_param_taa      RECORD LIKE seg_modulo.*
    DEFINE reg_bat RECORD
        pid            INTEGER,
        proceso_cod    INTEGER,
        opera_cod      INTEGER,
        folio          INTEGER
    END RECORD

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG("TAAB023.log")
    CALL inicio()

    IF NOT bnd_proceso THEN
        DEFER INTERRUPT
            OPTIONS INPUT WRAP,
            PROMPT LINE LAST - 1,
            ACCEPT KEY CONTROL-I

        CALL abre_ventana()
    ELSE
        CALL valida_folio(vfolio)
    END IF

    CALL lanza_provision()

END MAIN

FUNCTION inicio()

    LET reg_bat.pid            = ARG_VAL(1)
    LET reg_bat.proceso_cod    = ARG_VAL(2)
    LET reg_bat.opera_cod      = ARG_VAL(3)
    LET reg_bat.folio          = ARG_VAL(4)

    LET bnd_proceso = 0

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
        LET vfolio      = reg_bat.folio
    END IF

    LET HOY       = TODAY
    LET fecha_ini = CURRENT

    SELECT *, USER
    INTO   g_param_taa.*, vusuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'taa'

END FUNCTION

FUNCTION abre_ventana()

    OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TAAB0021" ATTRIBUTE(BORDER)
    DISPLAY " TAAB023                PROVISION VIRTUAL DE REGISTROS                         " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                              < Ctrl-C > Salir                                 " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfolio
        AFTER FIELD vfolio
            CALL valida_folio(vfolio)
            EXIT INPUT

        ON KEY (INTERRUPT)
            EXIT PROGRAM
    END INPUT

END FUNCTION

FUNCTION valida_folio(p_folio)

    DEFINE p_folio            INTEGER
    DEFINE reg_ctr_traspaso   RECORD LIKE taa_ctr_traspaso.*

    SELECT *
      INTO reg_ctr_traspaso.*
      FROM taa_ctr_traspaso
     WHERE folio = p_folio

    IF reg_ctr_traspaso.fin_incorpora IS NOT NULL OR
       reg_ctr_traspaso.fin_incorpora <> DATETIME (1899-12-31) YEAR TO DAY THEN
         IF reg_ctr_traspaso.fin_provision IS NOT NULL OR
            reg_ctr_traspaso.fin_provision <> DATETIME (1899-12-31) YEAR TO DAY THEN
             IF bnd_proceso THEN
                 DISPLAY "LOS REGISTROS YA FUERON PROVISIONADOS"
             ELSE
                 PROMPT "LOS REGISTROS YA FUERON PROVISIONADOS. [Enter] p/salir "
                    FOR opc
                 EXIT PROGRAM
             END IF
         ELSE
             IF reg_ctr_traspaso.ini_provision IS NOT NULL OR
                reg_ctr_traspaso.ini_provision <> DATETIME (1899-12-31) YEAR TO DAY THEN
                 IF bnd_proceso THEN
                     DISPLAY "SE CONTINUA EJECUTANDOSE LA PROVISION POR NOHUP."
                 ELSE
                     PROMPT "SE CONTINUA EJECUTANDOSE LA PROVISION POR NOHUP. [Enter] p/salir "
                        FOR opc
                     EXIT PROGRAM
                 END IF
             END IF
         END IF
    ELSE
        IF bnd_proceso THEN
            DISPLAY "NO SE HA TERMINADO DE EJECUTAR LA INCORPORACIÖN DE REGISTROS."
        ELSE
            PROMPT "NO SE HA TERMINADO LA INCORPORACIÖN DE REGISTROS. [Enter] p/salir "
               FOR opc
            EXIT PROGRAM
        END IF
    END IF

END FUNCTION

FUNCTION lanza_provision()

    IF bnd_proceso THEN
        DISPLAY "PROCESO EJECUTANDOSE POR NO-HUP."
    ELSE
        PROMPT "PROCESO EJECUTANDOSE POR NO-HUP. [Enter] p/salir" FOR opc
    END IF

    LET exe_nohup = "nohup time fglgo ",g_param_taa.ruta_exp CLIPPED,
                    "/TAAB0231 ",vfolio, " &"
    RUN exe_nohup

END FUNCTION
