#############################################################################
#Proyecto            => SAFRE ( MEXICO )                                    #
#Propietario         => E.F.P.                                              #
#Programa            => TAAC016A                                            #
#Descripcion         => REVERSO DE LA APERTURA DE CUENTAS CERTIFICADAS      #
#Fecha               => 22 DE noviembre DE 2012                             #
#Req: 1038           => JCPV 15/11/2012 Apertura de Cuentas Certificadas    #
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
    DEFINE id_garantia      CHAR(1)
    DEFINE id_modifica      CHAR(1)
    DEFINE hoy              DATE
    DEFINE vfecha_recep     DATE
    DEFINE vtipo_solic			INTEGER
    DEFINE qry_desmarca     CHAR(200)
    DEFINE qry_cta_adm_rev  CHAR(200)
    DEFINE qry_rev_marca    CHAR(200)

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
        PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I
        DEFER INTERRUPT
    CALL STARTLOG("/safre_lst/TAAC016A.log")

    LET ind_proceso    = ARG_VAL(1)
    LET vfecha_recep   = ARG_VAL(2)
    LET vtipo_solic      = ARG_VAL(3)
    

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
        LET vfecha_recep   = NULL
        LET vtipo_solic = NULL
        CALL proceso_principal() RETURNING bnd_proceso
    END IF

END FUNCTION

FUNCTION proceso_principal()

    DEFINE cont_reg_liq   INTEGER
    DEFINE cont_reg_prov  INTEGER
    DEFINE bnd_apertura   SMALLINT

    OPEN WINDOW ventana1 AT 3,2 WITH FORM "TAAC016A1" ATTRIBUTE(BORDER)
    DISPLAY "             REVERSAR APERTURA DE CUENTAS CERTIFICADAS TAA RECEPTORA            " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY hoy USING "DD/MM/YYYY" AT 1,68 ATTRIBUTE(REVERSE)
    DISPLAY "                [Esc]  Aceptar                       [Ctrl + C]  Salir          " AT 3,1 ATTRIBUTE(REVERSE)



    INPUT BY NAME vfecha_recep, vtipo_solic

        ON KEY (ESC)
        	AFTER FIELD vfecha_recep
            IF vfecha_recep IS NULL THEN
                ERROR  "INGRESE UNA OPCION VALIDA, NO NULOS"
                SLEEP 2
                NEXT FIELD vfecha_recep
            ELSE
            	  NEXT FIELD vtipo_solic
            END IF
            
          AFTER FIELD vtipo_solic
          IF vtipo_solic IS NULL THEN
            ERROR "Tipo Solicitud  NO puede ser NULO"
            NEXT FIELD vtipo_solic
          ELSE 					
                      SELECT 'X'
                      FROM afi_solicitud a                     
                       WHERE a.fentcons = vfecha_recep
                       AND a.tipo_solicitud = vtipo_solic				#1142
                       AND   a.status_interno = 70
                     GROUP BY 1

                    IF SQLCA.SQLCODE <> 0 THEN
                        PROMPT " NO EXISTE INFORMACION PARA LOS VALORES INDICADOS,",
                               " [Enter] continuar " 
                           FOR enter
                        NEXT FIELD vfecha_recep
                    END IF                 
            END IF

         EXIT INPUT

        ON KEY (INTERRUPT)
            RETURN 0
    END INPUT
          
    RETURN 1
  

    CLOSE WINDOW ventana1
END FUNCTION


FUNCTION rev_traspasos()

    DEFINE rcta_admin            SMALLINT
    DEFINE edo_proc              SMALLINT
    DEFINE edo_acr               SMALLINT
    DEFINE edo_mod               SMALLINT
    DEFINE consecutivo_cuenta    INTEGER
    DEFINE vfecha_taa_ced        DATE
    DEFINE bnd_historico         SMALLINT
    
    DEFINE reg_desmarca          RECORD
      nss                        CHAR(11),
      marca_cod                  SMALLINT,
      correlativo                INTEGER ,
      fecha_ini                  DATE
    END RECORD

    LET edo_proc = 237
    LET edo_acr  = 230
    LET edo_mod  = 610

    DECLARE cur_cta_trasp CURSOR FOR
     SELECT aso.*
   
       FROM afi_solicitud aso, afi_mae_afiliado ama
       WHERE aso.fentcons          = vfecha_recep
        AND aso.fentcons          = ama.fentcons
        AND aso.tipo_solicitud = vtipo_solic					#1142
        AND aso.tipo_solicitud = ama.tipo_solicitud
        AND aso.n_seguro          = ama.n_seguro
        AND aso.n_folio           = ama.n_folio
        AND aso.tipo_solicitud    = ama.tipo_solicitud
        AND aso.status_interno    = 70
        

    FOREACH cur_cta_trasp INTO reg_mae.*
                   

       
        UPDATE afi_solicitud
           SET status_interno =  65,                                   #1038 =75
           		finicta = NULL										#1142
         WHERE n_seguro       = reg_mae.n_seguro
           AND n_folio        = reg_mae.n_folio
           AND tipo_solicitud = reg_mae.tipo_solicitud
           AND fentcons       = vfecha_recep
           AND tipo_solicitud = vtipo_solic                        #1142
           AND status_interno = 70

    
     

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
           AND fecha_solicitud = vfecha_recep
           AND tipo_traspaso   = 5
           AND estado          = 100

        DELETE
           FROM cta_solicitud_regimen
          WHERE nss             = reg_mae.n_seguro
            AND fecha_solicitud = vfecha_recep
 
#TEMPORAL no existe en MAYA
       DELETE
           FROM cta_sol_regimen_total
          WHERE nss             = reg_mae.n_seguro
            AND fecha_solicitud = vfecha_recep
            
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
           AND fecha_trasp < vfecha_recep
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
           AND tipo_solicitud = vtipo_solic								#reg_mae.tipo_solicitud
           AND fentcons       = vfecha_recep

     #### REVERSAR INFORMACION HISTORICA #####
        DECLARE cur_historia CURSOR FOR
         SELECT *
           FROM afi_his_afiliado h
          WHERE h.n_seguro = reg_mae.n_seguro
          ORDER BY finicta DESC

        FOREACH cur_historia INTO reg_his.*        

          ### REVERSAR LA DESMARCA ####
            DECLARE cur_rev_desmarca CURSOR FOR
             SELECT a.nss, a.marca_cod, a.correlativo, a.fecha_ini
               FROM cta_his_marca a
              WHERE a.nss     = reg_his.n_seguro
                AND fecha_fin = vfecha_recep

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
                    AND factualiza = vfecha_recep

            INSERT INTO cta_nss_regimen
                 SELECT his.nss,
                        tab.grupo_regimen,
                        his.codigo_siefore,
                        his.usuario,
                        his.factualiza
                   FROM tab_agrupa_subcta_regimen tab, cta_his_regimen his
                  WHERE his.nss        = reg_his.n_seguro
                    AND his.subcuenta  = tab.subcuenta
                    AND his.factualiza = vfecha_recep                    
                  GROUP BY 1,2,3,4,5

            DELETE
              FROM cta_his_regimen
             WHERE nss        = reg_his.n_seguro
               AND factualiza = vfecha_recep

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

