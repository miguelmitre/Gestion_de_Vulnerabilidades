#*************************************************************************
#*******
#******* PROGRAMA : CTAB1131.4gl
#*******
#******* OBJETIVO : Provision del proceso de Transferencia de Decimos
#*******            Registros identificados como en proceso de transferencia
#*******            y que aun no se haya concluido dicha  transferencia.
#*******
#******* PARTE 2  : PROVISION
#******* ELABORO  : Claudia U.
#******* FECHA    : 8-JUNIO-2005
#*******
#******* Fecha actualiza   => 14 DE ENERO DE 2008
#******* Elaboro  : MAURO MUÑIZ CABALLERO
#    ADECUACIONES POR ULTIMO DECIMO ANTES DE CORTE POR EDAD
#*************************************************************************


DATABASE safre_af


DEFINE p_fecha_corte      ,
       g_fecha_provision  DATE

DEFINE g_precio_sb1       ,
       g_precio_sb2       DECIMAL(19,14)

DEFINE g_tipo_mov_cargo   ,
       g_tipo_mov_abono   ,
       g_siefore_abono    SMALLINT

DEFINE g_folio            INTEGER

DEFINE g_folio_sua        CHAR(6)

MAIN
    LET p_fecha_corte     = ARG_VAL(1)
    LET g_fecha_provision = ARG_VAL(2)
    LET g_folio           = ARG_VAL(3)

    CALL fn_inicia_variables ()
    CALL fn_provision_decimos( p_fecha_corte ) 

END MAIN

FUNCTION fn_inicia_variables ()

DEFINE   v_saldo_sub_sie     CHAR(100),
         v_prov_cargo        CHAR(300),
         v_prov_abono        CHAR(300),
         v_act_nss_his_dec   CHAR(100)


LET v_saldo_sub_sie = "EXECUTE FUNCTION fn_saldo_sub_dec ( ?,?,?,? )"
LET v_prov_cargo= "EXECUTE FUNCTION fn_prov_cargo ( ?,?,?,?,?,?,?,?,?,?,? )"
LET v_prov_abono= "EXECUTE FUNCTION fn_prov_abono_sie ( ?,?,?,?,?,?,?,?,?,?,? )"
LET v_act_nss_his_dec = "UPDATE cta_his_nss_decimo ",
                           "SET proceso_cod   = ? ,",
                              " fecha_proceso = ? ,",
                                    " usuario = USER ",
                         "WHERE nss = ? ",
                          " AND proceso_cod = 2 "


LET g_tipo_mov_cargo  = 215
LET g_tipo_mov_abono  =  15
LET g_siefore_abono   =   1

LET g_folio_sua = NULL 

PREPARE eje_saldo_sie  FROM  v_saldo_sub_sie
PREPARE eje_prov_cargo FROM  v_prov_cargo
PREPARE eje_prov_abono FROM  v_prov_abono
PREPARE eje_act_his    FROM  v_act_nss_his_dec

SELECT precio_del_dia
  INTO g_precio_sb1
  FROM glo_valor_accion
 WHERE codigo_siefore  = 1
   AND fecha_valuacion = g_fecha_provision

SELECT precio_del_dia
  INTO g_precio_sb2
  FROM glo_valor_accion
 WHERE codigo_siefore  = 2
   AND fecha_valuacion = g_fecha_provision

END FUNCTION

FUNCTION fn_provision_decimos( l_fecha_corte )

DEFINE  l_fecha_corte     DATE
DEFINE  reg_decimo        RECORD
        nss               CHAR(11),
        subcuenta         SMALLINT,
        siefore           SMALLINT,
        monto_decimo      DECIMAL(22,6),
        no_transferencia  SMALLINT
        END RECORD

DEFINE  v_fecha_fin       DATE

DEFINE  v_proceso_cod     ,
        v_status          ,
        v_provisiona      SMALLINT
DEFINE  v_consecutivo     INTEGER

DEFINE  v_monto_provision  ,
        v_monto_cprov_acc  ,
        v_monto_cprov_pes  ,
        v_monto_aprov_acc  ,
        v_monto_aprov_pes  DECIMAL(22,6)

DEFINE  r_subcuenta        SMALLINT

DEFINE  r_saldo_acciones   ,
        r_saldo_pesos      DECIMAL(22,6)


---   Verifica que haya finalizado el proceso anterior
---   Congelacion de saldos.

    SELECT proceso_cod,
           fecha_fin
      INTO v_proceso_cod,
           v_fecha_fin
      FROM cta_ctr_decimo
     WHERE fecha_corte = l_fecha_corte
       AND proceso_cod = 2

   IF v_proceso_cod IS NULL OR
      v_proceso_cod = 0 THEN
      ERROR " NO EXISTE EL PROCESO ANTERIOR PARA LA FECHA DE CORTE."
      SLEEP 3
      ERROR ""
   ELSE 
      IF v_fecha_fin IS NULL OR
         v_fecha_fin = "12/31/1899" THEN
         ERROR " PROCESO ANTERIOR AUN NO FINALIZA."
         SLEEP 3
         ERROR ""
      ELSE
         LET v_proceso_cod = v_proceso_cod + 1
      END IF
   END IF

---  Identifica  NSS

    CREATE TEMP TABLE nss_ced_glob
    (nss CHAR(11))

    INSERT INTO nss_ced_glob
    SELECT t.nss
    FROM   cta_act_marca t
    WHERE  t.marca_cod in(SELECT UNIQUE t.marca_cod
                            FROM taa_cd_tipo_traspaso t)
    AND    t.nss IN(SELECT r.nss
                    FROM   cta_ctr_cuenta r
                    WHERE  r.ind_transferencia = 1)

    INSERT INTO nss_ced_glob
    SELECT c.nss
    FROM   cta_act_marca c
    WHERE  c.nss IN(SELECT r.nss
                    FROM   cta_ctr_cuenta r
                    WHERE  r.ind_transferencia = 1)
    AND    c.marca_cod BETWEEN 800 AND 899

    INSERT INTO nss_ced_glob
    SELECT c.nss
    FROM   cta_act_marca c
    WHERE  c.marca_cod = 280
    AND    c.nss IN(SELECT r.nss
                    FROM   cta_ctr_cuenta r
                    WHERE  r.ind_transferencia = 1)

    CREATE INDEX nss_ced_glob1
    ON nss_ced_glob(nss)

    UPDATE STATISTICS FOR TABLE nss_ced_glob

    SELECT nss
      FROM cta_ctr_cuenta
     WHERE nss NOT IN ( SELECT nss
                          FROM nss_ced_glob )
       AND ind_transferencia = 1
      INTO TEMP tmp_ctr_cuenta

    CREATE INDEX tmp_nss_ctr1 ON tmp_ctr_cuenta(nss)

    UPDATE STATISTICS FOR TABLE tmp_ctr_cuenta

   LET v_consecutivo = 0

   DECLARE c_iden  CURSOR FOR

      SELECT ch.nss         ,
             cs.subcuenta   ,
             2  siefore     ,
             0  monto_decimo,
             1  no_transferencia
        FROM cta_his_nss_decimo ch,
             cta_regimen        cs,
             tmp_ctr_cuenta     cc
       WHERE ch.nss = cc.nss
         AND cs.nss = cc.nss
         AND cs.codigo_siefore = 1
       ORDER BY 1,2

   FOREACH c_iden INTO reg_decimo.*

      DECLARE c_saldo CURSOR FOR eje_saldo_sie

--  Obtiene el saldo actual de cada subcuenta 

         OPEN c_saldo USING reg_decimo.nss      ,
                            reg_decimo.subcuenta,
                            reg_decimo.siefore  ,
                            g_fecha_provision
         FETCH c_saldo INTO r_subcuenta,
                            r_saldo_acciones,
                            r_saldo_pesos

         IF r_saldo_acciones IS NULL THEN
            LET r_saldo_acciones = 0
         END IF

         IF r_saldo_pesos IS NULL THEN
            LET r_saldo_pesos = 0
         END IF

         IF r_saldo_acciones > 0 THEN
             LET v_provisiona      = 1
             LET v_monto_provision = r_saldo_acciones
         ELSE
             LET v_provisiona      = 0
             LET v_monto_provision = 0
         END IF

       CLOSE c_saldo
       FREE  c_saldo

       IF v_provisiona = 1 THEN

          LET v_consecutivo = v_consecutivo + 1

          LET v_monto_cprov_acc =   v_monto_provision * (-1)
          LET v_monto_cprov_pes =   v_monto_cprov_acc * g_precio_sb2

       -- Convierte los montos de la SB2  a SB1 para hacer el abono

          LET v_monto_aprov_pes = v_monto_cprov_pes * (-1)

          LET v_monto_aprov_acc =  v_monto_aprov_pes / g_precio_sb1 


       -- Llama funcion para provisionar el cargo

          CALL fn_provision   ( reg_decimo.nss      ,
                                reg_decimo.subcuenta,
                                g_tipo_mov_cargo    ,
                                reg_decimo.siefore  ,
                                reg_decimo.no_transferencia,
                                v_monto_cprov_acc   ,
                                v_monto_cprov_pes   ,
                                v_consecutivo
                              )

       -- Llama funcion para provisionar el abono

          CALL fn_provision   ( reg_decimo.nss      ,
                                reg_decimo.subcuenta,
                                g_tipo_mov_abono    ,
                                g_siefore_abono     ,
                                reg_decimo.no_transferencia,
                                v_monto_aprov_acc   ,
                                v_monto_aprov_pes   ,
                                v_consecutivo
                              )
       END IF -- Fin de v_provisiona = 1

      --- Actualiza  el proceso en cta_his_nss_decimo por NSS

       EXECUTE eje_act_his USING v_proceso_cod    ,
                                 g_fecha_provision,
                                 reg_decimo.nss

       LET r_saldo_acciones = 0
       LET r_saldo_pesos    = 0

   END FOREACH
   CLOSE c_iden
   FREE  c_iden

   --- Actualiza la finalizacion del proceso 

   UPDATE cta_ctr_decimo
      SET fecha_fin = CURRENT,
          folio     = g_folio
    WHERE fecha_corte = l_fecha_corte 
      AND proceso_cod = v_proceso_cod

END FUNCTION

FUNCTION fn_provision ( l_nss,
                        l_subcuenta,
                        l_tipo_mov ,
                        l_siefore  ,
                        l_no_transf,
                        l_monto_acc,
                        l_monto_pes,
                        l_consecutivo
                      )

DEFINE  l_nss         CHAR(11)

DEFINE  l_subcuenta   ,
        l_tipo_mov    ,
        l_siefore     ,
        l_no_transf   SMALLINT

DEFINE  l_monto_acc   ,
        l_monto_pes   DECIMAL(22,6)

DEFINE  l_consecutivo INTEGER

DEFINE  v_id_aportante CHAR(11)
DEFINE  v_status       SMALLINT
        

IF l_tipo_mov = g_tipo_mov_cargo THEN
   -- Provisiona el cargo
   LET v_id_aportante = "TDC-2-SALDO"

   DECLARE c_cargo CURSOR FOR eje_prov_cargo
      OPEN c_cargo USING  g_folio          ,
                          g_folio_sua      ,
                          l_nss            ,
                          l_subcuenta      ,
                          l_tipo_mov       ,
                          l_consecutivo    ,
                          l_siefore        ,
                          l_monto_acc      ,
                          l_monto_pes      ,
                          v_id_aportante   ,
                          g_fecha_provision
       FETCH c_cargo INTO v_status
   CLOSE c_cargo
   FREE  c_cargo

ELSE
   -- Provisiona el abono

   LET v_id_aportante = "TDA-1-SALDO"

   DECLARE c_abono CURSOR FOR eje_prov_abono
      OPEN c_abono USING  g_folio          ,
                          g_folio_sua      ,
                          l_nss            ,
                          l_subcuenta      ,
                          l_siefore        ,
                          l_tipo_mov       ,
                          l_consecutivo    ,
                          l_monto_acc      ,
                          l_monto_pes      ,
                          v_id_aportante   ,
                          g_fecha_provision
       FETCH c_abono INTO v_status
   CLOSE c_abono
   FREE  c_abono

END IF

END FUNCTION

