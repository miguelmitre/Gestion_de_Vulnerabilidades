##############################################################################
#Owner             => E.F.P.
#Programa TRAB0021 => REVERSA PROVISION DE ICEFAS                 
#Fecha creacion    => 20 DE ABRIL DE 1998
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 08 DE FEBRERO DEL 2005
#Sistema           => TRA-ICE-IMSS
##############################################################################
DATABASE safre_af
GLOBALS

    DEFINE vtipo_icefa CHAR(001)

    DEFINE reg RECORD #reg
        folio                 INTEGER ,
        descripcion           CHAR(30)
    END RECORD

    DEFINE g_reg RECORD #g_reg
        n_seguro             LIKE tra_det_trasp_sal.n_seguro        ,
        cve_ced_cuenta       LIKE tra_det_trasp_sal.cve_ced_cuenta  ,
        nro_ctrl_icefa       LIKE tra_det_trasp_sal.nro_ctrl_icefa
    END RECORD

    DEFINE reg_4 RECORD #glo #reg_4
        aceptada              SMALLINT ,
        aceptada_icefa              SMALLINT ,
        devuelta              SMALLINT ,
        no_atendida           SMALLINT ,
        rechazada             SMALLINT ,
        complementarios       SMALLINT
    END RECORD

    DEFINE #date
        HOY                   DATE

    DEFINE #char
        enter                 CHAR(1)

    DEFINE #smallint
        s_status              SMALLINT
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I



    CALL init()
    OPEN WINDOW trab00211 AT 4,4 WITH FORM "TRAB00211" ATTRIBUTE(BORDER)
    DISPLAY " TRAB0021            REVERSA PROVISION ICEFA-AFORE IMSS                        " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    LET reg.folio = NULL
    INPUT BY NAME reg.folio WITHOUT DEFAULTS
        AFTER FIELD folio
            IF reg.folio = "" THEN
                ERROR "FOLIO NO PUEDE SER NULO"
                NEXT FIELD FOLIO
            ELSE
                WHILE TRUE
                    PROMPT " ESTA SEGURO S/N " FOR CHAR enter
                    IF enter MATCHES "[SsNn]" THEN
                        IF enter MATCHES "[Ss]" THEN
                            EXIT WHILE
                        ELSE
                            DISPLAY "PROCESO FORZADO A FINALIZAR" AT 17,2 
                            SLEEP 2
                            EXIT PROGRAM
                        END IF
                    END IF
                END WHILE
                DISPLAY "PROCESANDO INFORMACION" AT 19,2 ATTRIBUTE(REVERSE)
                SLEEP 2

                WHENEVER ERROR CONTINUE
                    SELECT estado
                    INTO   reg_4.aceptada_icefa
                    FROM   tra_status
                    WHERE  des_estado = "ACEPTADA_ICEFA"

                    UPDATE tra_cza_traspaso
                    SET    tra_cza_traspaso.estado = reg_4.aceptada_icefa
                    WHERE  tra_cza_traspaso.folio  = reg.folio

                    UPDATE tra_det_trasp_sal
                    SET    tra_det_trasp_sal.estado = reg_4.aceptada_icefa
                    WHERE  tra_det_trasp_sal.folio  = reg.folio

                    UPDATE tra_det_trasp_int
                    SET    tra_det_trasp_int.estado = reg_4.aceptada_icefa
                    WHERE  tra_det_trasp_int.folio  = reg.folio

                    DELETE
                    FROM   tra_his_dep_icefa
                    WHERE  folio  = reg.folio

                    DELETE 
                    FROM   dis_provision
                    WHERE  folio = reg.folio

                    DELETE 
                    FROM   excep_trasp_sal
                    WHERE  folio  = reg.folio
                   
                    DELETE 
                    FROM   excep_trasp_int
                    WHERE  folio  = reg.folio

                WHENEVER ERROR STOP

                SELECT a.tipo_icefa 
		INTO   vtipo_icefa
		FROM   tra_det_trasp_sal a
		WHERE a.folio = reg.folio
		GROUP BY 1 
 
		IF vtipo_icefa = "N" THEN
                   CALL actualiza_edo()
                END IF

                EXIT INPUT
            END IF
    END INPUT
    PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
    CLOSE WINDOW trab00211
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT estado
    INTO   reg_4.aceptada_icefa
    FROM   tra_status
    WHERE  des_estado = "ACEPTADA_ICEFA"
END FUNCTION

FUNCTION actualiza_edo()
#ae-----------------------

DEFINE reg_llave RECORD 
       n_seguro       char(011),
       n_seguro_ent   char(011),
       rfc_ent        char(013),
       cve_ced_cuenta char(003),
       nro_ctrl_icefa char(030)
 END RECORD

 DEFINE reg_cta RECORD LIKE dis_cuenta.*

 DECLARE cur_act CURSOR FOR 

        SELECT a.n_seguro      ,
               a.n_seguro_ent  ,
               a.rfc_ent       ,
               a.cve_ced_cuenta,
               a.nro_ctrl_icefa
        FROM   tra_det_trasp_sal a
        WHERE  a.folio    = reg.folio

 FOREACH cur_act INTO reg_llave.*

    UPDATE  tra_mae_icefa
    SET     tra_mae_icefa.status      = 41
    WHERE   tra_mae_icefa.n_seguro    = reg_llave.n_seguro
    AND     tra_mae_icefa.nss         = reg_llave.n_seguro_ent
    AND     tra_mae_icefa.rfc         = reg_llave.rfc_ent
    AND     tra_mae_icefa.icefa_cod   = reg_llave.cve_ced_cuenta
    AND     tra_mae_icefa.nro_int_cta = reg_llave.nro_ctrl_icefa
--    AND     tra_mae_icefa.status      = 7

 END FOREACH
END FUNCTION
