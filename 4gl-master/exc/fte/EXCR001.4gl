################################################################################
#Proyecto          => SISTEMA DE AFORES ( SAFRE )                              #
#Owner             => E.F.P.                                                   #
#Programa EXCR001  => REVERSO DE PAGOS EN EXCESO                               #
#Fecha creacion    => 31 DE MAYO DE 2002                                       #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ                          #
#Actualizado       => 03 de junio de 2002.                                     #
#Sistema           => EXC                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD 
           folio         INTEGER,
	   fecha_proceso DATE
    END RECORD

    DEFINE x_fecha_recepcion DATE,
           HOY               DATE,
           enter             CHAR(1),
           sw                SMALLINT

END GLOBALS
####################################################
MAIN

    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP,
        PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I

    LET sw = 0

    CALL init()

    OPEN WINDOW excr0011 AT 4,4 WITH FORM "EXCR0011" ATTRIBUTE(BORDER)
    DISPLAY "                             <CTRL-C> SALIR                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " EXCR001                REVERSO DE PAGOS EN EXCESO                             " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    LET reg_1.fecha_proceso = NULL
    LET reg_1.folio = NULL

    INPUT BY NAME reg_1.* WITHOUT DEFAULTS
        BEFORE FIELD folio
           IF sw = 0 THEN
              LET sw = 1
              DISPLAY reg_1.folio TO folio
           END IF

        AFTER FIELD folio
            IF reg_1.folio IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO" 
                NEXT FIELD folio
            END IF
{
            SELECT "X"
            FROM   dis_cuenta
            WHERE  folio = reg_1.folio
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
               PROMPT "FOLIO YA LIQUIDADO ,NO PUEDE EJECUTAR EL REVERSO" 
               FOR enter
               NEXT FIELD folio
            END IF
}
        AFTER FIELD fecha_proceso
            IF reg_1.fecha_proceso IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO" 
                NEXT FIELD fecha_proceso
            END IF
        ON KEY (ESC)
            IF reg_1.folio IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO" 
                NEXT FIELD folio
            END IF
{
            SELECT "X"
            FROM   dis_cuenta
            WHERE  folio = reg_1.folio
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
               PROMPT "FOLIO YA LIQUIDADO ,NO PUEDE EJECUTAR EL REVERSO" 
               FOR enter
               NEXT FIELD folio
            END IF
}
            IF reg_1.fecha_proceso IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO" 
                NEXT FIELD fecha_proceso
            END IF

            EXIT INPUT

        ON KEY (CONTROL-C)
            PROMPT " PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
    END INPUT
 
    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

    CALL primer_paso()

    PROMPT " PROCESO FINALIZADO ...<ENTER> PARA CONTINUAR " FOR CHAR enter

    CLOSE WINDOW excr0011
END MAIN
############################################################
FUNCTION init()

    LET HOY = TODAY
END FUNCTION
############################################################
FUNCTION primer_paso()

    DEFINE reg_6 RECORD
           nss                   LIKE cta_ctr_cuenta.nss,
           activo_marca          SMALLINT,
           fecha_act_marca       DATE,
           marca_cod             SMALLINT
    END RECORD
{
    SELECT fecha_recepcion
    INTO   x_fecha_recepcion
    FROM   exc_cza_exceso
    WHERE  folio = reg_1.folio
}
    DELETE
    FROM   cta_act_marca
    WHERE  fecha_ini = reg_1.fecha_proceso
    AND    marca_cod in (540,542)

    DELETE
    FROM   cta_his_marca
    WHERE  fecha_ini = reg_1.fecha_proceso
    AND    marca_cod in (540,542)

    DELETE FROM exc_cza_exceso
    WHERE  folio = reg_1.folio
    AND    fecha_recepcion = reg_1.fecha_proceso

    DELETE FROM exc_det_exceso
    WHERE  folio = reg_1.folio

    DELETE FROM exc_dep_exceso
    WHERE  folio = reg_1.folio
    AND    fecha_estado = reg_1.fecha_proceso

    DELETE FROM exc_sum_exceso
    WHERE  folio = reg_1.folio
    AND    fecha_estado = reg_1.fecha_proceso

    DELETE FROM exc_exceso_int_viv
    WHERE  folio = reg_1.folio

    DELETE FROM exc_exceso_plu_min
    WHERE  folio = reg_1.folio

    DELETE FROM exc_exceso_comis
    WHERE  folio = reg_1.folio

    DELETE FROM dis_ctrl_proceso
    WHERE  proceso_cod = "EXC"
    AND    folio = reg_1.folio
    AND    fecha_proceso = reg_1.fecha_proceso

    DELETE FROM dis_provision
    WHERE  folio = reg_1.folio

    DELETE FROM dis_cuenta
    WHERE  folio = reg_1.folio

END FUNCTION
