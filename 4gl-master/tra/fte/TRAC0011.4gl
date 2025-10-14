##############################################################################
#Owner             => E.F.P.
#Programa TRAC0011 => REVERSA CARGA DE ARCHIVO ENVIADO POR PROCESAR
#Fecha creacion    => 04 DE MAYO DE 1999
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 07 DE FEBRERO DEL 2005  
#Sistema           => TRA-ICE-IMSS
##############################################################################
DATABASE safre_af
GLOBALS
     DEFINE reg_llave RECORD 
            n_seguro       char(011),
            n_seguro_ent   char(011),
            rfc_ent        char(013),
            cve_ced_cuenta char(003),
            nro_ctrl_icefa char(030)
     END RECORD
    DEFINE reg_1 RECORD #glo #reg_1
        folio                 INTEGER ,
        devol_solicit         CHAR(01) ,
        trasp_cuentas         CHAR(01) ,
        trasp_complem         CHAR(01) ,
        solic_no_aten         CHAR(01) ,
        solic_rechaza         CHAR(01) ,
        solic_devol_conf      CHAR(01) ,
        devol_planchadas      CHAR(01) ,
        devol_rechazadas      CHAR(01)
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        enter                 CHAR(1) ,
        usuario               CHAR(8)

    DEFINE #glo #smallint
        s_codigo_afore        SMALLINT
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS 
        INPUT WRAP,
        PROMPT LINE LAST  ,
        ACCEPT KEY CONTROL-I


UPDATE tra_mae_icefa 
SET nss = " "
WHERE nss IS NULL

UPDATE tra_mae_icefa 
SET rfc = " "
WHERE rfc IS NULL

UPDATE tra_mae_icefa 
SET nro_int_cta = " "
WHERE nro_int_cta IS NULL

    CALL init()
    OPEN WINDOW trac00111 AT 4,4 WITH FORM "TRAC00111" ATTRIBUTE(BORDER)
    DISPLAY " TRAC0011  REVERSA CARGA DE ARCHIVO ENVIADO POR PROCESAR IMSS                  " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    LET reg_1.folio = NULL
    INPUT BY NAME reg_1.* WITHOUT DEFAULTS
        AFTER FIELD folio
	    IF reg_1.folio IS NULL THEN
	        ERROR "Campo NO puede ser NULO"
	        NEXT FIELD folio
	    END IF

        AFTER FIELD devol_solicit
	    IF reg_1.devol_solicit <> "X" THEN
	        ERROR "SOLO PUEDE MARCAR CON 'X' "
	        NEXT FIELD devol_solicit
	    END IF

        AFTER FIELD trasp_cuentas
	    IF reg_1.trasp_cuentas <> "X" THEN
	        ERROR "SOLO PUEDE MARCAR CON 'X' "
	        NEXT FIELD trasp_cuentas
	    END IF

        AFTER FIELD trasp_complem
	    IF reg_1.trasp_complem <> "X" THEN
	        ERROR "SOLO PUEDE MARCAR CON 'X' "
	        NEXT FIELD trasp_complem
	    END IF

        AFTER FIELD solic_no_aten
	    IF reg_1.solic_no_aten <> "X" THEN
	        ERROR "SOLO PUEDE MARCAR CON 'X' "
	        NEXT FIELD solic_no_aten
	    END IF

        AFTER FIELD solic_rechaza
	    IF reg_1.solic_rechaza <> "X" THEN
	        ERROR "SOLO PUEDE MARCAR CON 'X' "
	        NEXT FIELD solic_rechaza
	    END IF

        AFTER FIELD solic_devol_conf   
	    IF reg_1.solic_devol_conf <> "X" THEN
	        ERROR "SOLO PUEDE MARCAR CON 'X' "
	        NEXT FIELD solic_devol_conf   
	    END IF

        AFTER FIELD devol_planchadas
	    IF reg_1.devol_planchadas <> "X" THEN
	        ERROR "SOLO PUEDE MARCAR CON 'X' "
	        NEXT FIELD devol_planchadas   
	    END IF

        AFTER FIELD devol_rechazadas
	    IF reg_1.devol_rechazadas <> "X" THEN
	        ERROR "SOLO PUEDE MARCAR CON 'X' "
	        NEXT FIELD devol_rechazadas   
	    END IF

        ON KEY (ESC)

            WHILE TRUE
                PROMPT " ESTA SEGURO S/N " FOR CHAR enter
                IF enter MATCHES "[SsNn]" THEN
                    IF enter MATCHES "[Ss]" THEN
                        EXIT INPUT
                    ELSE
                        PROMPT " PROCESO CANCELADO ...<ENTER> PARA SALIR "
                        FOR CHAR enter
                        EXIT PROGRAM
                    END IF
                END IF
            END WHILE


        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
    END INPUT

    DISPLAY "REVERSANDO OPERACION" AT 19,2 ATTRIBUTE(REVERSE)

    CALL primer_paso() #pp

    PROMPT "PROCESO FINALIZADO... PRESIONE <ENTER> PARA SALIR" FOR CHAR enter
    CLOSE WINDOW trac00111
END MAIN

FUNCTION init()
#i-------------
    SELECT codigo_afore   ,
           USER 
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

    LET HOY = TODAY
END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    IF reg_1.devol_solicit = "X" THEN
        CALL devolucion_solicitudes() #ds
        RETURN
    END IF

    IF reg_1.trasp_cuentas = "X" THEN
        CALL traspaso_cuentas() #tc
        RETURN
    END IF

    IF reg_1.trasp_complem = "X" THEN
        CALL traspaso_cuentas() #tc
        RETURN
    END IF

    IF reg_1.solic_no_aten = "X" THEN
        CALL solic_no_atendidas() #sna
        RETURN
    END IF

    IF reg_1.solic_rechaza = "X" THEN
        CALL solic_rechazadas() #sr
        RETURN
    END IF

    IF reg_1.solic_devol_conf = "X" THEN
        CALL solic_dev_confrontadas() #sdc
        RETURN
    END IF

    IF reg_1.devol_planchadas = "X" THEN
        CALL planchadas() #p
        RETURN
    END IF

    IF reg_1.devol_rechazadas = "X" THEN
        CALL rechazadas() #r
        RETURN
    END IF
END FUNCTION

FUNCTION devolucion_solicitudes()
#ds------------------------------


DECLARE cur_act4 CURSOR FOR 

 SELECT a.n_seguro      ,
        a.n_seguro_ent  ,
        a.rfc_ent       ,
        a.cve_ced_cuenta,
        a.nro_ctrl_icefa
 FROM   tra_det_devol a
 WHERE  a.folio         = reg_1.folio

 FOREACH cur_act4 INTO reg_llave.*
    CALL actualiza_edo(reg_llave.*,5)
  END FOREACH

    DELETE
    FROM   glo_folio
    WHERE  folio = reg_1.folio

    DELETE 
    FROM   tra_cza_devol
    WHERE  folio = reg_1.folio

    DELETE 
    FROM   tra_det_devol
    WHERE  folio = reg_1.folio

    DELETE 
    FROM   tra_sum_devol
    WHERE  folio = reg_1.folio
END FUNCTION

FUNCTION traspaso_cuentas()
#tc------------------------
 
    DELETE
    FROM   glo_folio
    WHERE  folio = reg_1.folio

    DELETE
    FROM   tra_cza_traspaso
    WHERE  folio = reg_1.folio

DECLARE cur_act1 CURSOR FOR 
 SELECT a.n_seguro      ,
        a.n_seguro_ent  ,
        a.rfc_ent       ,
        a.cve_ced_cuenta,
        a.nro_ctrl_icefa
 FROM   tra_det_trasp_sal a
 WHERE  a.folio         = reg_1.folio
 AND     a.tipo_icefa = "N"

 FOREACH cur_act1 INTO reg_llave.*
    CALL actualiza_edo(reg_llave.*,41)
  END FOREACH

    DELETE
    FROM   tra_det_trasp_sal
    WHERE  folio = reg_1.folio

    DELETE
    FROM   tra_det_trasp_int
    WHERE  folio = reg_1.folio

    DELETE
    FROM   tra_sum_traspaso
    WHERE  folio = reg_1.folio


END FUNCTION

FUNCTION solic_no_atendidas()
#sna-------------------------

DECLARE cur_act2 CURSOR FOR 

 SELECT a.n_seguro      ,
        a.n_seguro_ent_ced  ,
        a.rfc_ent_ced       ,
        a.cve_ced_cuenta,
        a.nro_ctrl_icefa
 FROM   tra_det_no_aten a
 WHERE  a.folio         = reg_1.folio

 FOREACH cur_act2 INTO reg_llave.*
    CALL actualiza_edo(reg_llave.*,6)
  END FOREACH

    DELETE
    FROM   glo_folio
    WHERE  folio = reg_1.folio

    DELETE
    FROM   tra_cza_sol_no_ate
    WHERE  folio = reg_1.folio

    DELETE
    FROM   tra_det_no_aten
    WHERE  folio = reg_1.folio

    DELETE
    FROM   tra_sum_no_aten
    WHERE  folio = reg_1.folio

END FUNCTION

FUNCTION solic_rechazadas()
#sr------------------------



DECLARE cur_act3 CURSOR FOR 

 SELECT a.n_seguro      ,
        a.n_seguro_ent  ,
        a.rfc_ent       ,
        a.cve_ced_cuenta,
        a.nro_ctrl_icefa
 FROM   tra_det_rechazo a
 WHERE  a.folio         = reg_1.folio

 FOREACH cur_act3 INTO reg_llave.*
    CALL actualiza_edo(reg_llave.*,3)
  END FOREACH

    DELETE
    FROM   glo_folio
    WHERE  folio = reg_1.folio


    DELETE
    FROM   tra_cza_sol_rch
    WHERE  folio = reg_1.folio

    DELETE
    FROM   tra_det_rechazo
    WHERE  folio = reg_1.folio

    DELETE
    FROM   tra_sum_rechazo
    WHERE  folio = reg_1.folio

    DELETE
    FROM   tra_no_actualiza
    WHERE  folio = reg_1.folio
END FUNCTION

FUNCTION solic_dev_confrontadas()
#sdc-----------------------------
    DELETE
    FROM   glo_folio
    WHERE  folio = reg_1.folio

    DELETE
    FROM   dev_cza_confronta
    WHERE  folio = reg_1.folio

    DELETE
    FROM   dev_det_confronta
    WHERE  folio = reg_1.folio

    DELETE
    FROM   dev_sum_confronta
    WHERE  folio_devol = reg_1.folio

END FUNCTION

FUNCTION planchadas()
#p-------------------
   {
    DELETE
    FROM   glo_folio
    WHERE  folio = reg_1.folio
   }

    DELETE
    FROM   dev_cza_planchada
    WHERE  folio = reg_1.folio

    DELETE
    FROM   dev_det_planchada
    WHERE  folio = reg_1.folio

    DELETE
    FROM   dev_sum_planchada
    WHERE  folio = reg_1.folio

END FUNCTION

FUNCTION rechazadas()
#r-------------------
   {
    DELETE
    FROM   glo_folio
    WHERE  folio = reg_1.folio
   }

    DELETE
    FROM   dev_cza_rechazada
    WHERE  folio = reg_1.folio

    DELETE
    FROM   dev_det_rechazada
    WHERE  folio = reg_1.folio

    DELETE
    FROM   dev_sum_rechazada
    WHERE  folio = reg_1.folio

END FUNCTION

FUNCTION actualiza_edo(reg_llave,edo_ant)
#ae------------------------------------
     DEFINE edo_ant SMALLINT
     DEFINE reg_llave RECORD 
            n_seguro       char(011),
            n_seguro_ent   char(011),
            rfc_ent        char(013),
            cve_ced_cuenta char(003),
            nro_ctrl_icefa char(030)
     END RECORD

     UPDATE tra_mae_icefa
     SET    tra_mae_icefa.status      = 2
     WHERE  tra_mae_icefa.n_seguro    = reg_llave.n_seguro
     AND    tra_mae_icefa.nss         = reg_llave.n_seguro_ent
     AND    tra_mae_icefa.rfc         = reg_llave.rfc_ent
     AND    tra_mae_icefa.icefa_cod   = reg_llave.cve_ced_cuenta
     AND    tra_mae_icefa.nro_int_cta = reg_llave.nro_ctrl_icefa
     AND    tra_mae_icefa.status       = edo_ant

END FUNCTION

