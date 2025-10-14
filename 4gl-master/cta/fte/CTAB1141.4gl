#*************************************************************************
#*******
#******* PROGRAMA : CTAB1141.4gl
#*******
#******* OBJETIVO : Provision del proceso de Transferencia de Decimos
#*******            Registros identificados como en proceso de transferencia
#*******            y que aun no se haya concluido dicha  transferencia.
#*******
#******* PARTE 2  : LIQUIDACION
#******* ELABORO  : Claudia U.
#******* FECHA    : 8-JUNIO-2005
#*******
#******* Fecha actualiza   => 14 DE ENERO DE 2008
#******* Elaboro  : MAURO MUÑIZ CABALLERO
#    ADECUACIONES POR ULTIMO DECIMO ANTES DE CORTE POR EDAD
#*************************************************************************

DATABASE safre_af

DEFINE g_fecha_corte      ,
       g_fecha_liquida    DATE

DEFINE g_precio_sb1       ,
       g_precio_sb2       DECIMAL(19,14)

DEFINE g_tipo_mov_cargo   ,
       g_tipo_mov_abono   ,
       g_siefore_cargo    ,
       g_siefore_abono    ,
       g_proceso_cod      SMALLINT

DEFINE g_folio            INTEGER

DEFINE g_folio_sua        CHAR(6)
DEFINE g_usuario          CHAR(8)
DEFINE g_status           SMALLINT

MAIN

    LET g_folio         = ARG_VAL(1)
    LET g_proceso_cod   = ARG_VAL(2)
    LET g_fecha_corte   = ARG_VAL(3)
    LET g_fecha_liquida = ARG_VAL(4)

    CALL fn_inicia_variables ()
    CALL fn_liquida_decimos( g_fecha_corte )

END MAIN

FUNCTION fn_inicia_variables ()

DEFINE   v_saldo_sub_sie     CHAR(200),
         v_prov_cargo        CHAR(300),
         v_prov_abono        CHAR(300),
         v_act_nss_his_dec   CHAR(200),
         v_ins_his_dec       CHAR(200)

LET v_saldo_sub_sie = "EXECUTE FUNCTION fn_saldo_sub_dec ( ?,?,?,? )"
LET v_prov_cargo= "EXECUTE FUNCTION fn_prov_cargo ( ?,?,?,?,?,?,?,?,?,?,? )"
LET v_prov_abono= "EXECUTE FUNCTION fn_prov_abono_sie ( ?,?,?,?,?,?,?,?,?,?,? )"
LET v_act_nss_his_dec = "UPDATE cta_his_nss_decimo ",
                           "SET proceso_cod   = ? ,",
                              " fecha_proceso = ? ,",
                                    " usuario = ? ",
                         "WHERE nss = ? ",
                          " AND proceso_cod = 3 "

LET v_ins_his_dec = "INSERT INTO cta_his_decimo VALUES(?,?,?,?,?,?,?,?,?,?,? )"

LET g_tipo_mov_cargo  = 215
LET g_tipo_mov_abono  =  15
LET g_siefore_abono   =   1

LET g_folio_sua = NULL 

PREPARE eje_ind_transf FROM "EXECUTE FUNCTION fn_ind_transferencia(?,?,?)"
PREPARE eje_saldo_sie  FROM  v_saldo_sub_sie
PREPARE eje_prov_cargo FROM  v_prov_cargo
PREPARE eje_prov_abono FROM  v_prov_abono
PREPARE eje_act_his    FROM  v_act_nss_his_dec
PREPARE eje_ins_his    FROM  v_ins_his_dec

SELECT precio_del_dia,USER
  INTO g_precio_sb1,
       g_usuario
  FROM glo_valor_accion
 WHERE codigo_siefore  = 1
   AND fecha_valuacion = g_fecha_liquida

SELECT precio_del_dia
  INTO g_precio_sb2
  FROM glo_valor_accion
 WHERE codigo_siefore  = 2
   AND fecha_valuacion = g_fecha_liquida

END FUNCTION

FUNCTION fn_liquida_decimos( l_fecha_corte )

DEFINE  l_fecha_corte     DATE

DEFINE  reg_decimo        RECORD LIKE dis_provision.*

DEFINE  v_monto_liquida    ,
        v_monto_cliq_acc  ,
        v_monto_cliq_pes  ,
        v_monto_aliq_acc  ,
        v_monto_aliq_pes  DECIMAL(22,6)

DEFINE  v_liquida          SMALLINT

DEFINE  v_consecutivo      INTEGER
DEFINE  v_no_transferencia CHAR(02)

DEFINE  r_subcuenta        SMALLINT

DEFINE  r_saldo_acciones   ,
        r_saldo_pesos      DECIMAL(22,6)

---  Identifica  NSS

   LET v_consecutivo = 0

    CREATE TEMP TABLE nss_ced_glob
    (nss CHAR(11))

    INSERT INTO nss_ced_glob
    SELECT t.nss
    FROM   cta_act_marca t
    WHERE  t.marca_cod in(SELECT UNIQUE d.marca_cod
                            FROM taa_cd_tipo_traspaso d)
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

   DECLARE c_iden  CURSOR FOR

---  NSS  recien  identificados  1a  Transferencia

      SELECT dp.*
        FROM dis_provision  dp,
             tmp_ctr_cuenta cc
       WHERE dp.nss               = cc.nss
         AND dp.folio             = g_folio
         AND dp.tipo_movimiento   = 215
       ORDER BY dp.nss, dp.subcuenta

   LET v_no_transferencia = 10

   FOREACH c_iden INTO reg_decimo.*

      LET v_liquida = 0
      LET r_saldo_acciones = 0

      DECLARE c_saldo CURSOR FOR eje_saldo_sie

--  Obtiene el saldo actual de cada subcuenta 

         OPEN c_saldo USING reg_decimo.nss      ,
                            reg_decimo.subcuenta,
                            reg_decimo.siefore  ,
                            g_fecha_liquida
         FETCH c_saldo INTO r_subcuenta         ,
                            r_saldo_acciones    ,
                            r_saldo_pesos

         LET reg_decimo.monto_en_acciones = reg_decimo.monto_en_acciones * (-1)

         IF r_saldo_acciones IS NULL THEN
            LET r_saldo_acciones = 0
         END IF

         IF r_saldo_pesos IS NULL THEN
            LET r_saldo_pesos = 0
         END IF

         IF r_saldo_acciones > 0 THEN
               LET v_liquida      = 1
               LET v_monto_liquida= r_saldo_acciones
         ELSE
               LET v_liquida      = 0
               LEt v_monto_liquida= 0
         END IF

         EXECUTE eje_ind_transf INTO  g_status
         USING reg_decimo.nss, "2", g_fecha_liquida

       CLOSE c_saldo
       FREE  c_saldo

       IF v_liquida = 1 THEN

          LET v_monto_cliq_acc =  v_monto_liquida  * (-1)
          LET v_monto_cliq_pes =  v_monto_cliq_acc * g_precio_sb2

       -- Convierte los montos de la SB2  a SB1 para hacer el abono

          LET v_monto_aliq_pes =  v_monto_cliq_pes * (-1)
          LET v_monto_aliq_acc =  v_monto_aliq_pes / g_precio_sb1

       -- Llama funcion para liquidar el cargo

          CALL fn_liquida   (   reg_decimo.nss        ,
                                reg_decimo.subcuenta  ,
                                reg_decimo.tipo_movimiento,
                                reg_decimo.siefore    ,
                                v_no_transferencia    ,
                                v_monto_cliq_acc      ,
                                v_monto_cliq_pes      ,
                                reg_decimo.consecutivo_lote,
                                g_precio_sb2          ,
                                reg_decimo.id_aportante
                              )

       -- Llama funcion para provisionar el abono

          CALL fn_liquida     ( reg_decimo.nss             ,
                                reg_decimo.subcuenta       ,
                                g_tipo_mov_abono           ,
                                g_siefore_abono            ,
                                v_no_transferencia         ,
                                v_monto_aliq_acc           ,
                                v_monto_aliq_pes           ,
                                reg_decimo.consecutivo_lote,
                                g_precio_sb1               ,
                                reg_decimo.id_aportante
                              )

          EXECUTE eje_ins_his USING reg_decimo.nss       ,
                                    v_no_transferencia   ,
                                    g_folio              ,
                                    reg_decimo.subcuenta ,
                                    g_siefore_abono      ,
                                    g_precio_sb1         ,
                                    g_fecha_liquida      ,
                                    v_monto_aliq_acc     ,
                                    v_monto_aliq_pes     ,
                                    g_fecha_liquida      ,
                                    g_usuario

       END IF -- Fin de v_liquida = 1

      --- Actualiza  el proceso en cta_his_nss_decimo por NSS

       EXECUTE eje_act_his USING g_proceso_cod    ,
                                 g_fecha_liquida  ,
                                 g_usuario        ,
                                 reg_decimo.nss   

      LET r_saldo_acciones = 0
      LET r_saldo_pesos    = 0

   END FOREACH
   CLOSE c_iden
   FREE  c_iden

   --- Actualiza la finalizacion del proceso 

   UPDATE cta_ctr_decimo
      SET fecha_fin = CURRENT
    WHERE fecha_corte = l_fecha_corte 
      AND proceso_cod = g_proceso_cod

END FUNCTION

FUNCTION fn_liquida   ( l_nss        ,
                        l_subcuenta  ,
                        l_tipo_mov   ,
                        l_siefore    ,
                        l_no_transf  ,
                        l_monto_acc  ,
                        l_monto_pes  ,
                        l_consecutivo,
                        l_precio     ,
                        l_aportante
                      )

DEFINE  l_nss         ,
        l_aportante   CHAR(11)
DEFINE  l_curp        CHAR(18)
DEFINE  l_sucursal    CHAR(10)

DEFINE  l_subcuenta   ,
        l_tipo_mov    ,
        l_dias        ,
        l_estado      ,
        l_etiqueta    ,
        l_siefore     SMALLINT

DEFINE  l_no_transf   CHAR(02)

DEFINE  l_monto_acc   ,
        l_monto_pes   DECIMAL(22,6)

DEFINE  l_precio      DECIMAL(22,6)

DEFINE  l_consecutivo INTEGER

IF l_tipo_mov = g_tipo_mov_abono THEN
   LET l_aportante = "TDA-",l_siefore USING "&&","-",l_no_transf  
END IF

INSERT INTO dis_cuenta VALUES ( l_tipo_mov     ,
                                l_subcuenta    ,
                                l_siefore      ,
                                g_folio        ,
                                l_consecutivo  ,
                                l_nss          ,
                                l_curp         ,
                                g_folio_sua    ,
                                g_fecha_liquida,
                                g_fecha_liquida,
                                g_fecha_liquida,
                                l_monto_pes    ,
                                l_monto_acc    ,
                                l_precio       ,
                                l_dias         ,
                                l_sucursal     ,
                                l_aportante    ,
                                l_estado       ,
                                TODAY          ,
                                USER           ,
                                TODAY          ,
                                l_etiqueta
                              );

END FUNCTION

