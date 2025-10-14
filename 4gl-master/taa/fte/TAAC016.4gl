#############################################################################
#Proyecto            => SAFRE ( MEXICO )                                    #
#Propietario         => E.F.P.                                              #
#Programa            => TAAC016                                             #
#Descripcion         => REVERSO DE LA APERTURA DE CUENTAS POR TRASPASO      #
#Por                 => JOSUE LISANDRO HUERTA SIERRA                        #
#Fecha               => 17 DE JUNIO DE 2008                                 #
#Req: 168            => JCPV 29/09/2011. ln 422 -692 (constraints)          #
#Req: 1038           => JCPV 15/11/2012 Apertura de Cuentas Certificadas.   #
#Req: 1331					 => FSR Marca de acreditados pasa a liquidación 
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg_mae          RECORD LIKE afi_mae_afiliado.*
    DEFINE reg_his          RECORD LIKE afi_his_afiliado.*
    DEFINE vfolio           INTEGER
    DEFINE ind_proceso      SMALLINT
    DEFINE enter            SMALLINT
    DEFINE vtipo_traspaso   SMALLINT
    DEFINE bnd_proceso      SMALLINT
    DEFINE opcion           SMALLINT
#CPL-1331    DEFINE id_garantia      CHAR(1)
#CPL-1331    DEFINE id_modifica      CHAR(1)
    DEFINE hoy              DATE
    DEFINE vfecha_liquida   DATE
    DEFINE qry_desmarca     CHAR(200)
    DEFINE qry_cta_adm_rev  CHAR(200)
    DEFINE qry_rev_marca    CHAR(200)

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
        PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I
        DEFER INTERRUPT
    CALL STARTLOG("TAAC016.log")

    LET ind_proceso    = ARG_VAL(1)
    LET vfolio         = ARG_VAL(2)
    LET vfecha_liquida = ARG_VAL(3)

    CALL inicio()

    IF bnd_proceso THEN
        ERROR "REVERSANDO APERTURA DE LA CUENTA..."
        CALL rev_traspasos()
        ERROR "TERMINO EL REVERSO "
    ELSE
        EXIT PROGRAM
    END IF

END MAIN

FUNCTION inicio()

    LET hoy            = TODAY
    LET qry_desmarca   = " EXECUTE PROCEDURE reversa_desmarca( ?, ?, ?, ?) "
    PREPARE prp_rev_desmarca FROM qry_desmarca

{
    LET qry_cta_adm_rev = "EXECUTE FUNCTION fn_reverso_reg_cuenta ( ?, ?, ?, ?)"
    PREPARE prp_rev_cta_admin FROM qry_cta_adm_rev
}

    LET qry_rev_marca = "EXECUTE PROCEDURE reversa_marca ( ?, ?, ?)"
    PREPARE prp_rev_marca FROM qry_rev_marca

    IF ind_proceso THEN
        LET bnd_proceso    = 1
    ELSE
        LET vfolio         = NULL
        LET vfecha_liquida = NULL
        CALL proceso_principal() RETURNING bnd_proceso
    END IF

END FUNCTION

FUNCTION proceso_principal()

    DEFINE cont_reg_liq   INTEGER
    DEFINE cont_reg_prov  INTEGER
    DEFINE bnd_apertura   SMALLINT

    OPEN WINDOW ventana1 AT 3,2 WITH FORM "TAAC0161" ATTRIBUTE(BORDER)
    DISPLAY "                       TRASPASO AFORE RECEPTORA                                " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY hoy USING "DD/MM/YYYY" AT 1,68 ATTRIBUTE(REVERSE)
    DISPLAY "                [Esc]  Aceptar                       [Ctrl + C]  Salir          " AT 3,1 ATTRIBUTE(REVERSE)



    INPUT BY NAME vfecha_liquida, vfolio

        ON KEY (ESC)
            IF vfecha_liquida IS NULL AND
               vfolio IS NULL THEN
                ERROR  "INGRESE UNA OPCION VALIDA, NO NULOS"
                SLEEP 2
                NEXT FIELD vfecha_liquida
            ELSE
                IF vfecha_liquida IS NOT NULL AND
                   vfolio IS NOT NULL THEN
                    SELECT 'X'
                      FROM taa_viv_recepcion a
                     WHERE a.folio             = vfolio
                       AND a.fecha_mov_banxico = vfecha_liquida
                       AND a.ident_operacion   = '09'
                     GROUP BY 1

                    IF SQLCA.SQLCODE <> 0 THEN
                        PROMPT " NO EXISTE INFORMACION PARA LOS VALORES INDICADOS,",
                               " [Enter] continuar " 
                           FOR enter
                        NEXT FIELD vfecha_liquida
                    END IF
                ELSE
                    IF vfecha_liquida IS NULL THEN
                        SELECT t.fecha_mov_banxico
                          INTO vfecha_liquida
                          FROM taa_viv_recepcion t
                         WHERE t.folio           = vfolio
                           AND t.ident_operacion = '09'
                         GROUP BY 1

                        IF SQLCA.SQLCODE <> 0 THEN
                            PROMPT " NO EXISTEN REGISTROS PARA EL FOLIO DE LIQUIDACION,",
                                   " [Enter] continuar " 
                               FOR enter
                            NEXT FIELD vfecha_liquida
                        END IF
                    ELSE
                        SELECT t.folio
                          INTO vfolio
                          FROM taa_viv_recepcion t
                         WHERE t.fecha_mov_banxico = vfecha_liquida
                           AND ident_operacion     = '09'
                         GROUP BY 1

                        IF SQLCA.SQLCODE <> 0 THEN
                            PROMPT " NO EXISTEN REGISTROS PARA LA FECHA DE LIQUIDACION,",
                                   " [Enter] continuar " 
                               FOR enter
                            NEXT FIELD vfolio
                        END IF
                    END IF
                END IF
            END IF

            EXIT INPUT

        ON KEY (INTERRUPT)
            RETURN 0
    END INPUT

    SELECT COUNT(*)
      INTO cont_reg_liq
      FROM dis_cuenta
     WHERE folio = vfolio

    SELECT COUNT(*)
      INTO cont_reg_prov
      FROM dis_provision
     WHERE folio = vfolio

    IF cont_reg_liq > 0 OR
       cont_reg_prov > 0 THEN
        IF cont_reg_liq > 0 THEN
            PROMPT " FOLIO LIQUIDADO, NO PUEDE REVERSAR APERTURA,",
                   " [Enter] continuar " 
               FOR enter
            LET bnd_apertura = FALSE
        ELSE
            PROMPT " FOLIO PROVISIONADO, NO PUEDE REVERSAR APERTURA,",
                   " [Enter] continuar " 
               FOR enter
            LET bnd_apertura = FALSE
        END IF
    ELSE
        IF hoy < vfecha_liquida THEN
            LET bnd_apertura = FALSE
            PROMPT " FECHA LIQUIDACION MENOR A LA DEL DIA, VERIFIQUE,",
                   " [Enter] continuar " 
               FOR enter
        ELSE
            LET bnd_apertura = TRUE
        END IF
    END IF

    IF bnd_apertura THEN
        RETURN 1
    ELSE
        RETURN 0
    END IF

    CLOSE WINDOW ventana1
END FUNCTION


FUNCTION rev_traspasos()

    DEFINE rcta_admin            SMALLINT
   #CPL-1331 DEFINE edo_proc              SMALLINT
   #CPL-1331 DEFINE edo_acr               SMALLINT
   #CPL-1331 DEFINE edo_mod               SMALLINT
    DEFINE consecutivo_cuenta    INTEGER
    DEFINE vfecha_taa_ced        DATE
    DEFINE bnd_historico         SMALLINT
    
    DEFINE reg_desmarca          RECORD
      nss                        CHAR(11),
      marca_cod                  SMALLINT,
      correlativo                INTEGER ,
      fecha_ini                  DATE
    END RECORD

   #CPL-1331 LET edo_proc = 237
   #CPL-1331 LET edo_acr  = 230
   #CPL-1331 LET edo_mod  = 610

    DECLARE cur_cta_trasp CURSOR FOR
     SELECT viv.tipo_traspaso ,
            viv.ident_garantia,
            viv.ind_nom_mod   ,
            m.*
       FROM afi_mae_afiliado m, taa_viv_recepcion viv
      WHERE viv.folio             = vfolio
        AND viv.fecha_mov_banxico = vfecha_liquida
        AND m.n_seguro            = viv.nss
        AND m.finicta             = viv.fecha_mov_banxico
        AND viv.ident_operacion   = '09'
        AND viv.tipo_traspaso     = 73                                #1038

    FOREACH cur_cta_trasp INTO vtipo_traspaso,
                            #CPL-1331   id_garantia   ,
                            #CPL-1331   id_modifica   ,
                               reg_mae.*      

      ####  REVERSAR fn_cuenta_entrante  #####

         ##EXECUTE prp_rev_cta_admin USING '0',
                                         ##reg_mae.n_seguro,
                                         ##reg_mae.n_folio,
                                         ##reg_mae.tipo_solicitud
                                    ##INTO rcta_admin

      ####  REVERSAR sp_recupera_marca UNIFICACION #####
      {  IF vtipo_traspaso = 12 OR
           vtipo_traspaso = 20 THEN

             EXECUTE prp_rev_marca USING reg_mae.n_seguro,
                                         '243',
                                         '0'

             EXECUTE prp_rev_marca USING reg_mae.n_seguro,
                                         '244',
                                         '0'
        END IF } #CPL-1331

        IF  vtipo_traspaso    =  73  THEN               #traspasos indebidos
          DELETE FROM afi_solicitud
          WHERE n_seguro       = reg_mae.n_seguro
           AND status_interno = 100
           AND tipo_solicitud = reg_mae.tipo_solicitud
           AND n_folio        = reg_mae.n_folio
           AND finicta        = vfecha_liquida
        ELSE
        UPDATE afi_solicitud            #para apertura de ctas certificadas
           SET status_interno =  65                                   #1038 =75
         WHERE n_seguro       = reg_mae.n_seguro
           AND status_interno = 100
           AND tipo_solicitud = reg_mae.tipo_solicitud
           AND n_folio        = reg_mae.n_folio
           AND finicta        = vfecha_liquida
        END IF
   #### REVERSAR MARCA ACREDITADOS Y MODIFICACION NOMBRE ####
    {   #CPL-1331 IF id_garantia = '1' THEN

             EXECUTE prp_rev_marca USING reg_mae.n_seguro,
                                         edo_proc,
                                         vfolio
            LET id_garantia = NULL
        END IF

        IF id_garantia = '2' THEN
             EXECUTE prp_rev_marca USING reg_mae.n_seguro,
                                         edo_acr,
                                         vfolio
            LET id_garantia = NULL
        END IF

        IF id_modifica = '1' THEN
             EXECUTE prp_rev_marca USING reg_mae.n_seguro,
                                         edo_mod,
                                         vfolio
            LET id_modifica = NULL
        END IF #CPL-1331}

     #### REVERSAR fn_regimen_inv
        DELETE
          FROM cta_regimen
         WHERE nss = reg_mae.n_seguro

        DELETE
          FROM cta_nss_regimen
         WHERE nss = reg_mae.n_seguro

        DELETE
          FROM tes_solicitud
         WHERE nss             = reg_mae.n_seguro
           AND fecha_solicitud = vfecha_liquida
           AND tipo_traspaso   = 5
           AND estado          = 100

        DELETE
           FROM cta_solicitud_regimen
          WHERE nss             = reg_mae.n_seguro
            AND fecha_solicitud = vfecha_liquida

        DELETE
           FROM cta_sol_regimen_total
          WHERE nss             = reg_mae.n_seguro
            AND fecha_solicitud = vfecha_liquida

        DELETE
          FROM afi_mae_patron
         WHERE n_folio        = reg_mae.n_folio
           AND tipo_solicitud = reg_mae.tipo_solicitud

        DELETE
          FROM afi_mae_benefici
         WHERE n_seguro       = reg_mae.n_seguro
           AND n_folio        = reg_mae.n_folio
           AND tipo_solicitud = reg_mae.tipo_solicitud

        SELECT max(fecha_trasp)
          INTO vfecha_taa_ced
          FROM taa_cd_det_cedido
         WHERE n_seguro    = reg_mae.n_seguro
           AND fecha_trasp < vfecha_liquida
           AND estado      = 99

        UPDATE taa_cd_det_cedido
           SET estado      = 103
         WHERE n_seguro    = reg_mae.n_seguro
           AND fecha_trasp = vfecha_taa_ced
           AND estado      = 99

        DELETE
          FROM afi_mae_afiliado
         WHERE n_seguro       = reg_mae.n_seguro
           AND n_folio        = reg_mae.n_folio
           AND tipo_solicitud = reg_mae.tipo_solicitud
           AND finicta        = vfecha_liquida

     #### REVERSAR INFORMACION HISTORICA #####
        DECLARE cur_historia CURSOR FOR
         SELECT *
           FROM afi_his_afiliado h
          WHERE h.n_seguro = reg_mae.n_seguro
          ORDER BY finicta DESC

        FOREACH cur_historia INTO reg_his.*

       #### REVERSAR fn_cuenta_saliente #####
             ##EXECUTE prp_rev_cta_admin USING '1',
             ##                              reg_his.n_seguro,
             ##                              reg_his.n_folio,
             ##                              reg_his.tipo_solicitud
             ##                         INTO rcta_admin

          ### REVERSAR LA DESMARCA ####
            DECLARE cur_rev_desmarca CURSOR FOR
             SELECT a.nss, a.marca_cod, a.correlativo, a.fecha_ini
               FROM cta_his_marca a
              WHERE a.nss     = reg_his.n_seguro
                AND fecha_fin = vfecha_liquida

            LET bnd_historico = TRUE

            FOREACH cur_rev_desmarca INTO reg_desmarca.*
---- 168 ---->
             SELECT 'X'
             FROM cta_his_marca chm, tab_marca tm
             WHERE chm.nss         =  reg_desmarca.nss
              AND  chm.marca_cod   =  reg_desmarca.marca_cod
              AND  chm.correlativo =  reg_desmarca.correlativo
              AND  chm.fecha_ini   =  reg_desmarca.fecha_ini
              AND  tm.marca_cod    =  chm.marca_cod
              IF SQLCA.SQLCODE <> 0 THEN
                ELSE
---- 168 <----
                CALL reversa_desmarca(reg_desmarca.*)
              END IF                                           #168
            END FOREACH

          ### REGENERA REGIMEN   #####

            INSERT INTO cta_regimen
                 SELECT nss,
                        subcuenta,
                        codigo_siefore,
                        porcentaje
                   FROM cta_his_regimen
                  WHERE nss        = reg_his.n_seguro
                    AND factualiza = vfecha_liquida

            INSERT INTO cta_nss_regimen
                 SELECT his.nss,
                        tab.grupo_regimen,
                        his.codigo_siefore,
                        his.usuario,
                        his.factualiza
                   FROM tab_agrupa_subcta_regimen tab, cta_his_regimen his
                  WHERE his.nss        = reg_his.n_seguro
                    AND his.subcuenta  = tab.subcuenta
                    AND his.factualiza = vfecha_liquida                    
                  GROUP BY 1,2,3,4,5

            DELETE
              FROM cta_his_regimen
             WHERE nss        = reg_his.n_seguro
               AND factualiza = vfecha_liquida

            INSERT INTO afi_mae_afiliado VALUES(reg_his.*)

            DELETE
              FROM afi_his_afiliado
             WHERE n_seguro       = reg_his.n_seguro
               AND n_folio        = reg_his.n_folio
               AND tipo_solicitud = reg_his.tipo_solicitud

            EXIT FOREACH
        END FOREACH

        IF bnd_historico THEN
             UPDATE cta_ctr_cuenta
                SET fecha_edad         = reg_his.finicta
              WHERE nss                = reg_his.n_seguro
            LET bnd_historico = FALSE
        ELSE
            ---- chm is referenced from ccc by nss ----
---- 168 ----> chm is referenced by cam TO ----
            DELETE FROM cta_act_marca
            WHERE nss = reg_mae.n_seguro
---- 168 <----
            DELETE
              FROM cta_his_marca 
              WHERE nss = reg_mae.n_seguro
            DELETE
              FROM cta_ctr_cuenta
              WHERE nss = reg_mae.n_seguro
            LET bnd_historico = FALSE
        END IF

    END FOREACH
END FUNCTION

FUNCTION reversa_desmarca(reg)

    DEFINE reg RECORD
               vnss         CHAR(11),
               vmarca       SMALLINT,
               vcorrelativo INTEGER,
               vfecha       DATE
    END RECORD

    EXECUTE prp_rev_desmarca USING reg.vnss,
                                   reg.vmarca,
                                   reg.vcorrelativo,
                                   reg.vfecha

END FUNCTION

