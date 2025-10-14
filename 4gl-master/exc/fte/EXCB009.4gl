#*********************************************************************#
#Proyecto          => Sistema de safre_af.( MEXICO )                  #
#Propietario       => E.F.P.                                          #
#Programa          => EXCB009                                         #
#Descripcion       => DEVOLUCION DE PAGOS EN EXCESO ISSSTE LIQUIDACION#
#Fecha Inicio      => 29 ENERO 2010                                   #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ                 #
#                     STEFANIE DANIELA VERA PIÑA                      #
#Sistema           => EXC.                                            #
#*********************************************************************#
DATABASE safre_af

GLOBALS

   DEFINE g_bat  RECORD LIKE dis_ctrl_proceso.*

   DEFINE g_cue RECORD LIKE dis_cuenta.*
   	
   DEFINE gparam_dev  RECORD LIKE seg_modulo.*

   DEFINE
      generar            CHAR(12),
      generar2           CHAR(10),
      gusuario           CHAR(08),
      hora_final         CHAR(08),
      cla_sel            CHAR(500),
      vhora_final        CHAR(08),
      vresultado         CHAR(50),
      vparametro1        CHAR(10),
      ejecuta_procedure  CHAR(500),
      ejecuta_marca      CHAR(200),
      ejecuta_desmarca   CHAR(200),
      ejecuta_fn_mayor   CHAR(200),
      ejecuta_fn_paga    CHAR(200),
      ejecuta_fn_menor   CHAR(200),
      ejecuta_rev_marca  CHAR(200),
      vnom_archivo       CHAR(20)
   
   DEFINE
      hoy                ,
      hoy2               ,
      generar1           DATE
      
   DEFINE 
      generar3           ,
      vconsecutivo       ,
      vfolio             ,
      vparametro2        INTEGER

   DEFINE
      vetapa_cod         ,
      x_afore_local      SMALLINT

END GLOBALS
#*********************************************************************
MAIN

   OPTIONS 
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT

   LET vparametro1 = ARG_VAL(1)
   LET vparametro2 = ARG_VAL(2)

   CALL inicializa()

   CALL Proceso()

END MAIN
#*********************************************************************
FUNCTION inicializa()

   LET hoy = TODAY

   SELECT *,
          USER
   INTO   gparam_dev.*,
          gusuario
   FROM   seg_modulo
   WHERE  modulo_cod = "exc"

   SELECT codigo_afore
   INTO   x_afore_local
   FROM   tab_afore_local
   GROUP BY 1

   LET ejecuta_procedure = "EXECUTE PROCEDURE fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE clausula_sql3 FROM ejecuta_procedure

   LET ejecuta_rev_marca = "EXECUTE PROCEDURE reversa_marca (?,?,?)"
   PREPARE cla_rev_marca FROM ejecuta_rev_marca

   LET ejecuta_marca = "EXECUTE PROCEDURE marca_cuenta (?,?,?,?,?,?,?,?)"
   PREPARE clausula_sql4 FROM ejecuta_marca

END FUNCTION
#*********************************************************************
FUNCTION Proceso()

   LET hoy = TODAY

   SELECT parametro1
   INTO   vnom_archivo
   FROM   dis_ctrl_proceso
   WHERE  folio = vparametro2
   AND    proceso_cod = 'EXC'
   AND    etapa_cod = 1 
   
   IF vparametro1 = "RCV" THEN
      
      LET cla_sel = " SELECT * ",
                    " FROM   dis_ctrl_proceso ",
                    " WHERE  folio = ",vparametro2,
                    " AND    proceso_cod = 'EXC' ",
                    " AND    etapa_cod = 5 "        -- ETAPA 5

      PREPARE claexe2 FROM cla_sel

      DECLARE cur_proceso2 CURSOR FOR claexe2

      OPEN cur_proceso2
         FETCH cur_proceso2 INTO g_bat.*
      CLOSE cur_proceso2

      LET generar1 = g_bat.parametro1 CLIPPED
      LET generar2 = g_bat.parametro2 CLIPPED
      LET generar3 = g_bat.folio CLIPPED

      CALL Proceso_principal(generar1,
                             generar2,
                             generar3)
           RETURNING vfolio

      LET vhora_final = TIME

      LET vresultado   = "Terminada liquidacion de registros de RCV "

      CALL Actualiza_etapa(vfolio,5,vresultado)

      ERROR "PROCESO TERMINADO DE PAGOS EN EXCESO"
   ELSE
      LET cla_sel = " SELECT * ",
                    " FROM   dis_ctrl_proceso ",
                    " WHERE  folio = ",vparametro2,
                    " AND    proceso_cod = 'EXC' ",
                    " AND    etapa_cod = 7 "        -- ETAPA 7

      PREPARE claexe3 FROM cla_sel

      DECLARE cur_proceso3 CURSOR FOR claexe3

      OPEN cur_proceso3
         FETCH cur_proceso3 INTO g_bat.*
      CLOSE cur_proceso3

      LET generar1 = g_bat.parametro1 CLIPPED
      LET generar2 = g_bat.parametro2 CLIPPED
      LET generar3 = g_bat.folio CLIPPED

      CALL Proceso_principal(generar1,
                             generar2,
                             generar3)
           RETURNING vfolio

      LET vhora_final = TIME

      LET vresultado   = "Terminada liquidacion de registros de VIVIENDA"

      CALL Actualiza_etapa(vfolio,7,vresultado)

      ERROR "PROCESO TERMINADO DE PAGOS EN EXCESO"
   END IF

END FUNCTION
#*********************************************************************
FUNCTION Proceso_principal(x_fecha,
                           x_origen,
                           x_folio)

  DEFINE g_reg RECORD LIKE dis_cuenta.*

   DEFINE
     cla_sel                     CHAR(200),
     vopc                        CHAR(1),
     x_nss                       CHAR(11),
     x_origen                    CHAR(10)

   DEFINE
      x_fecha                    ,
      x_fecha_valor              ,
      hoy2                       ,
      x_fecha_conversion         DATE

    DEFINE
      x_folio                    ,
      vfolio                     INTEGER

   DEFINE vmarca_cod             SMALLINT,
          vrechazo_cod           SMALLINT,
          ejecuta_procedure      CHAR(200),
          x_status               SMALLINT


   LET hoy2 = TODAY

   LET x_origen = x_origen CLIPPED

   IF x_origen = "RCV"
   OR x_origen = "RCV-ISSSTE" THEN

      IF x_origen = "RCV" THEN
         INSERT INTO safre_tmp:exc_dis_provision 
         SELECT * 
         FROM   dis_provision
         WHERE  folio = x_folio
         AND    subcuenta IN (1,2)

         DELETE FROM  dis_provision
         WHERE folio = x_folio
         AND    subcuenta IN (1,2)
      ELSE
         INSERT INTO safre_tmp:exc_dis_provision 
         SELECT * 
         FROM   dis_provision
         WHERE  folio = x_folio
         AND    subcuenta IN (13,19,30,31,33)

         DELETE FROM  dis_provision
         WHERE folio = x_folio
         AND    subcuenta IN (13,19,30,31,33)
      END IF

      LET vopc = 1

      CALL recalcula_provision(x_folio,vopc)

      IF x_origen = "RCV" THEN
         LET cla_sel = " SELECT folio,nss,consecutivo_lote,subcuenta ",
                       " FROM   dis_provision ",
                       " WHERE  folio = ",x_folio CLIPPED,
                       " AND    subcuenta IN (1,2) ",
                       " AND    estado = 6 ",
                       " GROUP BY 1,2,3,4 "
      ELSE
         LET cla_sel = " SELECT folio,nss,consecutivo_lote,subcuenta ",
                       " FROM   dis_provision ",
                       " WHERE  folio = ",x_folio CLIPPED,
                       " AND    subcuenta IN (13,19,30,31,33) ",
                       " AND    estado = 6 ",
                       " GROUP BY 1,2,3,4 "
      END IF

      PREPARE claexe5 FROM cla_sel

      DECLARE cur_proceso5 CURSOR FOR claexe5

      FOREACH cur_proceso5 INTO g_cue.folio,
                                g_cue.nss,
                                g_cue.consecutivo_lote,
                                g_cue.subcuenta

         LET x_fecha_valor = x_fecha

         LET x_fecha_conversion = x_fecha

         LET ejecuta_procedure = "EXECUTE PROCEDURE fn_liquida_exc (?,?,?,?,?,?)"

         PREPARE clausula_sql2 FROM ejecuta_procedure

         DECLARE cursor_liquida1 CURSOR FOR clausula_sql2

         OPEN cursor_liquida1 USING g_cue.folio,
                                    g_cue.nss,
                                    g_cue.consecutivo_lote,
                                    g_cue.subcuenta,
                                    x_fecha_conversion,
                                    x_fecha_conversion

         FETCH cursor_liquida1 INTO x_status

         CLOSE cursor_liquida1

         IF x_origen = "RCV" THEN
            CALL des_marca(g_cue.nss,                 -- nss
                           540,                       -- marca_entra
                           g_cue.consecutivo_lote,    -- correlativo
                           0,                         -- estado_marca
                           0,                         -- marca_causa
                           gusuario                   -- usuario
                          )
         ELSE
            CALL des_marca(g_cue.nss,                 -- nss
                           543,                       -- marca_entra
                           g_cue.consecutivo_lote,    -- correlativo
                           0,                         -- estado_marca
                           0,                         -- marca_causa
                           gusuario                   -- usuario
                          )
         END IF

      END FOREACH
   ELSE
      IF x_origen = "VIVIENDA" THEN
         INSERT INTO safre_tmp:exc_dis_provision 
         SELECT * 
         FROM   dis_provision
         WHERE  folio = x_folio
         AND    subcuenta IN (4)

         DELETE FROM  dis_provision
         WHERE folio = x_folio
         AND    subcuenta IN (4)
      ELSE
         INSERT INTO safre_tmp:exc_dis_provision 
         SELECT * 
         FROM   dis_provision
         WHERE  folio = x_folio
         AND    subcuenta IN (14,35)

         DELETE FROM  dis_provision
         WHERE folio = x_folio
         AND    subcuenta IN (14,35)
      END IF

      LET vopc = 2

      CALL recalcula_provision(x_folio,vopc)

      IF x_origen = "VIVIENDA" THEN
         LET cla_sel = " SELECT folio,nss,consecutivo_lote,subcuenta ",
                       " FROM   dis_provision ",
                       " WHERE  folio = ",x_folio CLIPPED,
                       " AND    subcuenta IN (4) ",
                       " AND    estado = 6 ",
                       " GROUP BY 1,2,3,4"
      ELSE
         LET cla_sel = " SELECT folio,nss,consecutivo_lote,subcuenta ",
                       " FROM   dis_provision ",
                       " WHERE  folio = ",x_folio CLIPPED,
                       " AND    subcuenta IN (14,35) ",
                       " AND    estado = 6 ",
                       " GROUP BY 1,2,3,4"
      END IF

      PREPARE claexe9 FROM cla_sel

      DECLARE cur_proceso9 CURSOR FOR claexe9

      FOREACH cur_proceso9 INTO g_cue.folio,
                                g_cue.nss,
                                g_cue.consecutivo_lote,
                                g_cue.subcuenta

         LET x_fecha_valor = hoy2

         LET x_fecha_conversion = hoy2

         LET ejecuta_procedure = "EXECUTE PROCEDURE fn_liquida_exc (?,?,?,?,?,?)"

         PREPARE clausula_sql31 FROM ejecuta_procedure

         DECLARE cursor_liquida31 CURSOR FOR clausula_sql31

         OPEN cursor_liquida31 USING g_cue.folio,
                                    g_cue.nss,
                                    g_cue.consecutivo_lote,
                                    g_cue.subcuenta,
                                    x_fecha_valor,
                                    x_fecha_conversion

         FETCH cursor_liquida31 INTO x_status
         CLOSE cursor_liquida31

         IF x_origen = "VIVIENDA" THEN
            CALL des_marca(g_cue.nss,                 -- nss
                           542,                       -- marca_entra
                           g_cue.consecutivo_lote,    -- correlativo
                           0,                         -- estado_marca
                           0,                         -- marca_causa
                           gusuario                   -- usuario
                          )
         ELSE
            CALL des_marca(g_cue.nss,                 -- nss
                           544,                       -- marca_entra
                           g_cue.consecutivo_lote,    -- correlativo
                           0,                         -- estado_marca
                           0,                         -- marca_causa
                           gusuario                   -- usuario
                          )
         END IF

      END FOREACH

   END IF

   IF x_origen = "RCV" THEN
      UPDATE exc_dep_exceso
      SET    estado = 3
      WHERE  folio = x_folio
      AND    ident_pago[14,15] = "71"
   ELSE
      UPDATE exc_dep_exceso_issste
      SET    estado = 3
      WHERE  folio = x_folio
      AND    ident_pago[14,15] NOT IN ("72","74")
   END IF

   IF x_origen = "VIVIENDA" THEN
      UPDATE exc_dep_exceso
      SET    estado = 3
      WHERE  folio = x_folio
      AND    ident_pago[14,15] = "73"
   ELSE
      UPDATE exc_dep_exceso_issste
      SET    estado = 3
      WHERE  folio = x_folio
      AND    ident_pago[14,15] IN ("72","74")
   END IF

   RETURN x_folio

END FUNCTION
#*********************************************************************
FUNCTION Actualiza_etapa(vfolio,vetapa_cod,vresultado)

   DEFINE vfolio       INTEGER,
          vetapa_cod   DECIMAL(2,0),
          vresultado   CHAR(50)

   LET cla_sel = " SELECT MAX(consecutivo) ",
                 " FROM   dis_ctrl_proceso ",
                 " WHERE  folio = ",vfolio,
                 " AND    proceso_cod = 'EXC' ",
                 " AND    etapa_cod = ",vetapa_cod

   PREPARE claexe6 FROM cla_sel

   DECLARE cur_proceso6 CURSOR FOR claexe6

   OPEN cur_proceso6
      FETCH cur_proceso6 INTO vconsecutivo
   CLOSE cur_proceso6

   LET vhora_final   = TIME

   LET cla_sel = " UPDATE dis_ctrl_proceso ",
                 " SET    hora_final = ","'",vhora_final,"',",
                 "        folio      = ",vfolio,",",
                 "        resultado  = ","'",vresultado,"'",
                 " WHERE  folio = ",vfolio,
                 " AND    proceso_cod   = 'EXC'",
                 " AND    etapa_cod     = ",vetapa_cod,
                 " AND    consecutivo  = ",vconsecutivo CLIPPED

   PREPARE claexe7 FROM cla_sel

   EXECUTE claexe7

END FUNCTION
#******************************************************************************
FUNCTION des_marca(x_nss,
                   x_marca_entra,
                   x_consecutivo_lote,
                   x_marca_estado,
                   x_marca_causa,
                   x_usuario )

   DEFINE x_nss             CHAR(11),
          x_operacion       CHAR(1),
          x_marca_entra     SMALLINT,
          x_consecutivo_lote     SMALLINT,
          x_marca_estado    SMALLINT,
          x_marca_causa     SMALLINT,
          x_codigo_rechazo  SMALLINT,
          x_usuario         CHAR(8),
          xx_codigo_marca   SMALLINT,
          xx_codigo_rechazo SMALLINT,
          ejecuta_procedure CHAR(200)

   LET ejecuta_procedure = "EXECUTE PROCEDURE desmarca_cuenta (?,?,?,?,?,?) "

   PREPARE clausula_sql FROM ejecuta_procedure

   EXECUTE clausula_sql USING x_nss,
                               x_marca_entra,
                               x_consecutivo_lote,
                               x_marca_estado,
                               x_marca_causa,
                               x_usuario

END FUNCTION
#*********************************************************************
FUNCTION recalcula_provision(vfolio,vopc)

   DEFINE
      diag_dev             CHAR(2),
      tipo_rech            CHAR(3),
      vnss                 CHAR(11),
      vcurp                CHAR(18),
      vopc                 CHAR(1)
                           
   DEFINE                  
      vsaldo_acc           DECIMAL(18,6),
      vsaldo_pes           DECIMAL(18,6)
      
   DEFINE
      vconsec_reg_lote     ,
      vfolio               INTEGER

   IF vopc = "1" THEN
      IF x_afore_local = 578 THEN

         IF vparametro1 = "RCV" THEN   #-- ARCHIVO IMSS --#
            DECLARE cur_1 CURSOR FOR
            SELECT a.curp,
                   a.consec_reg_lote,
                   b.n_seguro
            FROM   exc_det_exceso a, afi_mae_afiliado b
            WHERE  a.folio = vfolio
            AND    a.result_operacion in ("01","04")
            AND    clave_ent_orig  = "001"
            AND    a.nss = b.n_seguro
            AND    b.tipo_solicitud = 8

            FOREACH cur_1 INTO vcurp,vconsec_reg_lote,vnss

               LET vsaldo_acc = 0
               LET vsaldo_pes = 0

               CALL saldo_al_dia(vnss,
                                 0,
                                 0)
                    RETURNING vsaldo_acc,
                              vsaldo_pes

               CALL valida_sufici_imss(vfolio,vcurp,vnss,vconsec_reg_lote)

               CALL provisiona_cuenta_imss(vfolio,vcurp,vnss,vconsec_reg_lote) 

            END FOREACH

            CALL reversa_parciales(vfolio)
         ELSE
            DECLARE cur_8 CURSOR FOR
            SELECT a.curp,
                   a.consec_reg_lote,
                   b.n_seguro
            FROM   exc_det_exceso_issste a, afi_mae_afiliado b
            WHERE  a.folio = vfolio
            AND    a.result_operacion in ("01","04")
            AND    clave_ent_orig  = "001"
            AND    a.curp = b.n_unico
            AND    b.tipo_solicitud = 8

            FOREACH cur_8 INTO vcurp,vconsec_reg_lote,vnss

               LET vsaldo_acc = 0
               LET vsaldo_pes = 0

               CALL saldo_al_dia(vnss,
                                 0,
                                 0)
                    RETURNING vsaldo_acc,
                              vsaldo_pes

               CALL valida_sufici_issste(vfolio,vcurp,vnss,vconsec_reg_lote)

               CALL provisiona_cuenta_issste(vfolio,vcurp,vnss,vconsec_reg_lote) 

            END FOREACH

            CALL reversa_parciales(vfolio)
         END IF
      ELSE
         IF vparametro1 = "RCV" THEN   #-- ARCHIVO IMSS --#
            DECLARE cur_7 CURSOR FOR
            SELECT a.curp,
                   a.consec_reg_lote,
                   b.n_seguro
            FROM   exc_det_exceso a, afi_mae_afiliado b
            WHERE  a.folio = vfolio
            AND    a.result_operacion in ("01","04")
            AND    clave_ent_orig  = "001"
            AND    a.nss = b.n_seguro

            FOREACH cur_7 INTO vcurp,vconsec_reg_lote,vnss

               LET vsaldo_acc = 0
               LET vsaldo_pes = 0

               CALL saldo_al_dia(vnss,
                                 0,
                                 0)
                    RETURNING vsaldo_acc,
                              vsaldo_pes

               CALL valida_sufici_imss(vfolio,vcurp,vnss,vconsec_reg_lote)

               CALL provisiona_cuenta_imss(vfolio,vcurp,vnss,vconsec_reg_lote) 
            END FOREACH
         ELSE
            DECLARE cur_9 CURSOR FOR
            SELECT a.curp,
                   a.consec_reg_lote,
                   b.n_seguro
            FROM   exc_det_exceso_issste a, afi_mae_afiliado b
            WHERE  a.folio = vfolio
            AND    a.result_operacion in ("01","04")
            AND    clave_ent_orig  = "001"
            AND    a.curp = b.n_unico

            FOREACH cur_9 INTO vcurp,vconsec_reg_lote,vnss

               LET vsaldo_acc = 0
               LET vsaldo_pes = 0

               CALL saldo_al_dia(vnss,
                                 0,
                                 0)
                    RETURNING vsaldo_acc,
                              vsaldo_pes

               CALL valida_sufici_issste(vfolio,vcurp,vnss,vconsec_reg_lote)

               CALL provisiona_cuenta_issste(vfolio,vcurp,vnss,vconsec_reg_lote) 
            END FOREACH
         END IF
      END IF
   ELSE
      IF vparametro1 = "VIVIENDA" THEN #-- ARCHIVO IMSS --#
         DECLARE cur_4 CURSOR FOR
         SELECT a.curp,
                a.consec_reg_lote,
                b.n_seguro
         FROM   exc_det_exceso a, afi_mae_afiliado b
         WHERE  a.folio = vfolio
         AND    a.result_operacion in ("01","04")
         AND    clave_ent_orig  = "002"
         AND    a.nss = b.n_seguro

         FOREACH cur_4 INTO vcurp,vconsec_reg_lote,vnss

            LET vsaldo_acc = 0
            LET vsaldo_pes = 0

            CALL saldo_al_dia(vnss,
                              0,
                              0)
                 RETURNING vsaldo_acc,
                           vsaldo_pes

            CALL valida_sufici_imss(vfolio,vcurp,vnss,vconsec_reg_lote)

            CALL provisiona_cuenta_imss(vfolio,vcurp,vnss,vconsec_reg_lote)

         END FOREACH
      ELSE
         DECLARE cur_10 CURSOR FOR
         SELECT a.curp,
                a.consec_reg_lote,
                b.n_seguro
         FROM   exc_det_exceso_issste a, afi_mae_afiliado b
         WHERE  a.folio = vfolio
         AND    a.result_operacion in ("01","04")
         AND    clave_ent_orig  = "002"
         AND    a.curp = b.n_unico

         FOREACH cur_10 INTO vcurp,vconsec_reg_lote,vnss

            LET vsaldo_acc = 0
            LET vsaldo_pes = 0

            CALL saldo_al_dia(vnss,
                              0,
                              0)
                 RETURNING vsaldo_acc,
                           vsaldo_pes

            CALL valida_sufici_issste(vfolio,vcurp,vnss,vconsec_reg_lote)

            CALL provisiona_cuenta_issste(vfolio,vcurp,vnss,vconsec_reg_lote)
         END FOREACH
      END IF
   END IF

END FUNCTION
#*********************************************************************
FUNCTION saldo_al_dia(v_nss,
                      v_subcuenta,
                      v_grupo)

   DEFINE v_saldo_dia        CHAR(100),
          v_nss              CHAR(11),
          v_subcuenta        SMALLINT,
          v_grupo            SMALLINT

   DEFINE f_subcuenta        SMALLINT,
          f_siefore          SMALLINT,
          f_monto_acc        DECIMAL(16,6),
          f_monto_pes        DECIMAL(16,6)

   DEFINE v_saldo_acc        DECIMAL(16,6),
          v_saldo_pes        DECIMAL(16,6),
          hoy          DATE

   LET hoy = TODAY

   WHENEVER ERROR CONTINUE
      DROP TABLE temp_saldo_act
   WHENEVER ERROR STOP

   CREATE TEMP TABLE temp_saldo_act
   (nss            CHAR(11),
    subcuenta      SMALLINT,
    siefore        SMALLINT,
    acciones       DECIMAL(16,6),
    pesos          DECIMAL(16,6)
   )

   LET v_saldo_dia = "EXECUTE FUNCTION fn_saldo_dia ( ?,?,?,? ) "

   PREPARE eje_saldo_dia FROM v_saldo_dia

   LET v_saldo_acc  = 0
   LET v_saldo_pes  = 0

   DECLARE c_saldo CURSOR FOR  eje_saldo_dia
   FOREACH c_saldo  USING v_nss,
                          v_subcuenta,
                          v_grupo,
                          hoy
                     INTO f_subcuenta,
                          f_siefore,
                          f_monto_acc,
                          f_monto_pes

        INSERT INTO temp_saldo_act
        VALUES (v_nss,
                f_subcuenta,
                f_siefore,
                f_monto_acc,
                f_monto_pes
               )

        LET v_saldo_acc  = v_saldo_acc + f_monto_acc
        LET v_saldo_pes  = v_saldo_pes + f_monto_pes
   END FOREACH

   RETURN v_saldo_acc,
          v_saldo_pes
END FUNCTION
#*********************************************************************
FUNCTION valida_sufici_imss(vfolio,vcurp,vnss,vconsec_reg_lote)

   DEFINE g_reg  RECORD LIKE exc_det_exceso.*

   DEFINE 
      diag_dev              CHAR(2),
      tipo_rech             CHAR(3),
      vcurp                 CHAR(18),
      vnss                  CHAR(11)

   DEFINE
      vfecha_valor          DATE

   DEFINE
      vpesos                DECIMAL(16,2)

   DEFINE
      vconsec_reg_lote      ,
      vfolio                INTEGER

   DEFINE
      mig_marca_entra       ,
      vban                  ,
      vban1                 ,
      vban2                 SMALLINT

   LET vpesos = 0
   LET vban   = 0
   LET vban1  = 0
   LET vban2  = 0

   DECLARE cur_2 CURSOR FOR
   SELECT clave_ent_orig,
          monto_ret,
          monto_act_ret,
          mto_act_ret_rend,
          monto_ces_vej_pat,
          monto_act_cv_pat,
          mto_act_cv_rend_pat,
          monto_aport_pat
   FROM   exc_det_exceso
   WHERE  folio = vfolio
   AND    nss  = vnss
   AND    consec_reg_lote = vconsec_reg_lote

   FOREACH cur_2 INTO g_reg.clave_ent_orig,
                      g_reg.monto_ret,
                      g_reg.monto_act_ret,
                      g_reg.mto_act_ret_rend,
                      g_reg.monto_ces_vej_pat,
                      g_reg.monto_act_cv_pat,
                      g_reg.mto_act_cv_rend_pat,
                      g_reg.monto_aport_pat

      IF g_reg.clave_ent_orig  = "001" THEN
         IF g_reg.monto_ret > 0 THEN

            SELECT SUM((A.monto_en_acciones)* B.precio_del_dia)
            INTO   vpesos
            FROM   dis_cuenta A,
                   glo_valor_accion B
            WHERE  A.nss = vnss
            AND    A.subcuenta = 1
            AND    A.tipo_movimiento = 1
            AND    A.siefore = B.codigo_siefore
            AND    B.fecha_valuacion = hoy

            IF vpesos >= g_reg.monto_ret THEN
               LET vban = 1
            ELSE
               IF  vpesos  < g_reg.monto_ret
               AND vpesos  > 0  THEN
                  LET vban1 = 1

               ELSE
                 LET vban2 = 1
               END IF
            END IF
         END IF

         IF g_reg.monto_act_ret > 0 THEN

            SELECT SUM((A.monto_en_acciones)* B.precio_del_dia)
            INTO   vpesos
            FROM   dis_cuenta A,
                   glo_valor_accion B
            WHERE  A.nss = vnss
            AND    A.subcuenta = 1
            AND    A.tipo_movimiento = 2
            AND    A.siefore = B.codigo_siefore
            AND    B.fecha_valuacion = hoy

            IF vpesos >= g_reg.monto_act_ret THEN
               LET vban = 1
               LET vban2 = 0
            ELSE
               IF  vpesos  < g_reg.monto_act_ret
               AND vpesos  > 0  THEN
                  LET vban1 = 1
                  LET vban2 = 0
               ELSE
                 LET vban2 = 1
               END IF
            END IF
         END IF

         IF g_reg.mto_act_ret_rend > 0 THEN

            SELECT SUM((A.monto_en_acciones)* B.precio_del_dia)
            INTO   vpesos
            FROM   dis_cuenta A,
                   glo_valor_accion B
            WHERE  A.nss = vnss
            AND    A.subcuenta = 1
            AND    A.tipo_movimiento = 3
            AND    A.siefore = B.codigo_siefore
            AND    B.fecha_valuacion = hoy

            IF vpesos >= g_reg.mto_act_ret_rend THEN
               LET vban = 1
               LET vban2 = 0
            ELSE
               IF  vpesos  < g_reg.mto_act_ret_rend
               AND vpesos  > 0  THEN
                  LET vban1 = 1
                  LET vban2 = 0
               ELSE
                  LET vban2 = 1
               END IF
            END IF
         END IF

         IF g_reg.monto_ces_vej_pat > 0 THEN

            SELECT SUM((A.monto_en_acciones)* B.precio_del_dia)
            INTO   vpesos
            FROM   dis_cuenta A,
                   glo_valor_accion B
            WHERE  A.nss = vnss
            AND    A.subcuenta = 2
            AND    A.tipo_movimiento = 1
            AND    A.siefore = B.codigo_siefore
            AND    B.fecha_valuacion = hoy

            IF vpesos >= g_reg.monto_ces_vej_pat THEN
               LET vban = 1
               LET vban2 = 0
            ELSE    
               IF  vpesos  < g_reg.monto_ces_vej_pat
               AND vpesos  > 0  THEN
                  LET vban1 = 1
                  LET vban2 = 0
               ELSE
                 LET vban2 = 1
               END IF
            END IF
         END IF

         IF g_reg.monto_act_cv_pat > 0 THEN

            SELECT SUM((A.monto_en_acciones)* B.precio_del_dia)
            INTO   vpesos
            FROM   dis_cuenta A,
                   glo_valor_accion B
            WHERE  A.nss = vnss
            AND    A.subcuenta = 2
            AND    A.tipo_movimiento = 2
            AND    A.siefore = B.codigo_siefore
            AND    B.fecha_valuacion = hoy

            IF vpesos >= g_reg.monto_act_cv_pat THEN
               LET vban = 1
               LET vban2 = 0
            ELSE    
               IF  vpesos  < g_reg.monto_act_cv_pat
               AND vpesos  > 0  THEN
                  LET vban1 = 1
                  LET vban2 = 0
               ELSE
                 LET vban2 = 1
               END IF
            END IF
         END IF

         IF g_reg.mto_act_cv_rend_pat > 0 THEN

            SELECT SUM((A.monto_en_acciones)* B.precio_del_dia)
            INTO   vpesos
            FROM   dis_cuenta A,
                   glo_valor_accion B
            WHERE  A.nss = vnss
            AND    A.subcuenta = 2
            AND    A.tipo_movimiento = 3
            AND    A.siefore = B.codigo_siefore
            AND    B.fecha_valuacion = hoy

            IF vpesos >= g_reg.mto_act_cv_rend_pat THEN
               LET vban = 1
               LET vban2 = 0
            ELSE    
               IF  vpesos  < g_reg.mto_act_cv_rend_pat
               AND vpesos  > 0  THEN
                  LET vban1 = 1
                  LET vban2 = 0
               ELSE
                 LET vban2 = 1
               END IF
            END IF
         END IF
      END IF

      IF g_reg.clave_ent_orig  = "002" THEN
         IF g_reg.monto_aport_pat > 0 THEN

            LET vfecha_valor = MDY(MONTH(hoy),1,YEAR(hoy))

            SELECT SUM((A.monto_en_acciones)* B.precio_del_dia)
            INTO   vpesos
            FROM   dis_cuenta A,
                   glo_valor_accion B
            WHERE  A.nss = vnss
            AND    A.subcuenta = 4
            AND    A.siefore = B.codigo_siefore
            AND    B.fecha_valuacion = vfecha_valor

            IF vpesos >= g_reg.monto_aport_pat THEN
               LET vban = 1
               LET vban2 = 0
            ELSE
               IF  vpesos  < g_reg.monto_aport_pat
               AND vpesos  > 0  THEN
                  LET vban1 = 1
                  LET vban2 = 0
               ELSE
                 LET vban2 = 1
               END IF
            END IF
         END IF
      END IF

      IF vban1 = 1 THEN
         LET tipo_rech = "100"
         LET diag_dev = "04"   #--PARCIAL--#

         IF vparametro1 = "RCV"
         OR vparametro1 = "VIVIENDA" THEN   #-- ARCHIVO IMSS --#
            UPDATE exc_det_exceso
            SET    result_operacion = diag_dev
            WHERE  nss  = vnss
            AND    folio = vfolio
            AND    consec_reg_lote = vconsec_reg_lote
         ELSE
            UPDATE exc_det_exceso_issste
            SET    result_operacion = diag_dev
            WHERE  curp  = vcurp
            AND    folio = vfolio
            AND    consec_reg_lote = vconsec_reg_lote
         END IF
      ELSE
         IF vban = 1 THEN
            LET tipo_rech = "100"
            LET diag_dev = "01"   #--ACEPTADO--#
         END IF
      END IF

      IF  g_reg.clave_ent_orig  = "001"
      AND vban2 = 1 THEN

         LET tipo_rech = "592"  #--RECHAZADO--#
         LET diag_dev = "02"    #--RECHAZADO--#
      END IF

      IF  g_reg.clave_ent_orig  = "002"
      AND vban2 = 1 THEN

         LET tipo_rech = "593"  #-- RECHAZADO --#
         LET diag_dev = "02"    #--RECHAZADO --#
      END IF

      IF tipo_rech > 100 THEN

         IF g_reg.clave_ent_orig = "001" THEN
            IF vparametro1 = "RCV" THEN
              LET mig_marca_entra = 540
            ELSE
               LET mig_marca_entra = 543
            END IF
         ELSE
            IF vparametro1 = "VIVIENDA" THEN
               LET mig_marca_entra = 542
            ELSE
               LET mig_marca_entra = 544
            END IF
         END IF

         CALL actualiza_reg(vfolio,
                            vcurp,
                            vnss,
                            diag_dev,
                            tipo_rech,
                            vconsec_reg_lote,
                            "",
                            mig_marca_entra,
                            gusuario)
      END IF 
   END FOREACH
END FUNCTION
#*********************************************************************
FUNCTION valida_sufici_issste(vfolio,vcurp,vnss,vconsec_reg_lote)

   DEFINE g_reg  RECORD LIKE exc_det_exceso_issste.*

   DEFINE 
      diag_dev              CHAR(2),
      tipo_rech             CHAR(3),
      vcurp                 CHAR(18),
      vnss                  CHAR(11)

   DEFINE
      vpesos                DECIMAL(16,2)

   DEFINE
      vconsec_reg_lote      ,
      vfolio                INTEGER

   DEFINE
      mig_marca_entra       ,
      vban                  ,
      vban1                 ,
      vban2                 SMALLINT

   LET vpesos = 0
   LET vban   = 0
   LET vban1  = 0
   LET vban2  = 0

   DECLARE cur_13 CURSOR FOR 
   SELECT clave_ent_orig,
          impt_sar_issste,
          impt_ret_issste,
          impt_cv_patron,
          impt_ahorro_solid,
          impt_fondo_viv92,
          impt_fondo_viv08
   FROM   exc_det_exceso_issste
   WHERE  folio = vfolio
   AND    curp  = vcurp
   AND    consec_reg_lote = vconsec_reg_lote

   FOREACH cur_13 INTO g_reg.clave_ent_orig,
                      g_reg.impt_sar_issste,
                      g_reg.impt_ret_issste,
                      g_reg.impt_cv_patron,
                      g_reg.impt_ahorro_solid,
                      g_reg.impt_fondo_viv92,
                      g_reg.impt_fondo_viv08
 
      IF g_reg.clave_ent_orig  = "001" THEN
         IF g_reg.impt_sar_issste > 0 THEN

            IF vnom_archivo[1,4] = "BANX" THEN
               SELECT SUM(pesos)
               INTO   vpesos
               FROM   temp_saldo_act
               WHERE  nss = vnss
               AND    subcuenta = 19
            ELSE
               SELECT SUM(pesos)
               INTO   vpesos
               FROM   temp_saldo_act
               WHERE  nss = vnss
               AND    subcuenta = 13
            END IF

            IF vpesos >= g_reg.impt_sar_issste THEN
               LET vban = 1
            ELSE    
               IF  vpesos  < g_reg.impt_sar_issste
               AND vpesos  > 0  THEN
                  LET vban1 = 1

               ELSE
                 LET vban2 = 1
               END IF
            END IF
         END IF
 
         IF g_reg.impt_ret_issste > 0 THEN
         
            SELECT SUM(pesos)
            INTO   vpesos
            FROM   temp_saldo_act
            WHERE  nss = vnss
            AND    subcuenta = 30
         
            IF vpesos >= g_reg.impt_ret_issste THEN
               LET vban = 1
               LET vban2 = 0
            ELSE
               IF  vpesos  < g_reg.impt_ret_issste
               AND vpesos  > 0  THEN
                  LET vban1 = 1
                  LET vban2 = 0
               ELSE
                 LET vban2 = 1
               END IF
            END IF
         END IF
 
         IF g_reg.impt_cv_patron > 0 THEN
         
            SELECT SUM(pesos)
            INTO   vpesos
            FROM   temp_saldo_act
            WHERE  nss = vnss
            AND    subcuenta = 31
         
            IF vpesos >= g_reg.impt_cv_patron THEN
               LET vban = 1
               LET vban2 = 0
            ELSE    
               IF  vpesos  < g_reg.impt_cv_patron
               AND vpesos  > 0  THEN
                  LET vban1 = 1
                  LET vban2 = 0
               ELSE
               	 LET vban2 = 1
               END IF
            END IF
         END IF
 
         IF g_reg.impt_ahorro_solid > 0 THEN
         
            SELECT SUM(pesos)
            INTO   vpesos
            FROM   temp_saldo_act
            WHERE  nss = vnss
            AND    subcuenta = 33
         
            IF vpesos >= g_reg.impt_ahorro_solid THEN
               LET vban = 1
               LET vban2 = 0
            ELSE    
               IF  vpesos  < g_reg.impt_ahorro_solid
               AND vpesos  > 0  THEN
                  LET vban1 = 1
                  LET vban2 = 0
               ELSE
                 LET vban2 = 1
               END IF
            END IF
         END IF
      END IF
 
      IF g_reg.clave_ent_orig  = "002" THEN
         IF g_reg.impt_fondo_viv92 > 0 THEN
 
            SELECT SUM(pesos)
            INTO   vpesos
            FROM   temp_saldo_act
            WHERE  nss = vnss
            AND    subcuenta = 14

            IF vpesos >= g_reg.impt_fondo_viv92 THEN
               LET vban = 1
               LET vban2 = 0
            ELSE
               IF  vpesos  < g_reg.impt_fondo_viv92
               AND vpesos  > 0  THEN
                  LET vban1 = 1
                  LET vban2 = 0
               ELSE
                 LET vban2 = 1
               END IF
            END IF
         END IF
      
         IF g_reg.impt_fondo_viv08 > 0 THEN
 
            SELECT SUM(pesos)
            INTO   vpesos
            FROM   temp_saldo_act
            WHERE  nss = vnss
            AND    subcuenta = 35
 
            IF vpesos >= g_reg.impt_fondo_viv08 THEN
               LET vban = 1
               LET vban2 = 0
            ELSE
               IF  vpesos  < g_reg.impt_fondo_viv08
               AND vpesos  > 0  THEN
                  LET vban1 = 1
                  LET vban2 = 0
               ELSE
                 LET vban2 = 1
               END IF     
            END IF    
         END IF
      END IF 

      IF vban1 = 1 THEN
         LET tipo_rech = "100"
         LET diag_dev = "04"   #--PARCIAL--#
         
         UPDATE exc_det_exceso_issste
         SET    result_operacion = diag_dev
         WHERE curp  = vcurp
         AND   folio = vfolio
         AND   consec_reg_lote = vconsec_reg_lote
 
      ELSE
         IF vban = 1 THEN
            LET tipo_rech = "100"
            LET diag_dev = "01"   #--ACEPTADO--#
         END IF
      END IF

      IF g_reg.clave_ent_orig  = "001" AND
         vban2 = 1 THEN

         LET tipo_rech = "592"  #--RECHAZADO--#
         LET diag_dev = "02"    #--RECHAZADO--#
      END IF

      IF g_reg.clave_ent_orig  = "002" AND
         vban2 = 1 THEN

         LET tipo_rech = "593"  #-- RECHAZADO --#
         LET diag_dev = "02"    #--RECHAZADO --#
      END IF     

      IF tipo_rech > 100 THEN

         IF g_reg.clave_ent_orig = "001" THEN
            LET mig_marca_entra = 543
         ELSE
            LET mig_marca_entra = 544
         END IF

         CALL actualiza_reg(vfolio,
                            vcurp,
                            vnss,
                            diag_dev,
                            tipo_rech,
                            vconsec_reg_lote,
                            "",
                            mig_marca_entra,
                            gusuario)
      END IF 
   END FOREACH
END FUNCTION
#*********************************************************************
FUNCTION provisiona_cuenta_imss(vfolio,vcurp,vnss,vconsec_reg_lote)

   DEFINE g_reg  RECORD LIKE exc_det_exceso.*

   DEFINE
      vcurp                   CHAR(18),
      vnss                    CHAR(11)

   DEFINE
      vfecha_valor            DATE

   DEFINE
      vacciones               DECIMAL(16,6),
      vpesos                  DECIMAL(16,2),
      vprecio_del_dia         DECIMAL(16,6)

   DEFINE
      vconsec_reg_lote        ,
      vfolio                  INTEGER

   DEFINE
      vcodigo_siefore         ,
      vtipo_movimiento        SMALLINT

   DECLARE cur_3 CURSOR FOR
   SELECT *
   FROM   exc_det_exceso
   WHERE  folio = vfolio
   AND    nss  = vnss
   AND    consec_reg_lote = vconsec_reg_lote

   FOREACH cur_3 INTO g_reg.*

      LET g_reg.nss = vnss

      IF g_reg.clave_ent_orig  = "001" THEN
         IF g_reg.monto_ret > 0 THEN

            LET vtipo_movimiento = 540

            SELECT SUM((A.monto_en_acciones)* B.precio_del_dia),
                   SUM(A.monto_en_acciones)
            INTO   vpesos,
                   vacciones
            FROM   dis_cuenta A,
                   glo_valor_accion B
            WHERE  A.nss = g_reg.nss
            AND    A.subcuenta = 1
            AND    A.tipo_movimiento = 1
            AND    A.siefore = B.codigo_siefore
            AND    B.fecha_valuacion = hoy

            SELECT a.codigo_siefore,
                   b.precio_del_dia
            INTO   vcodigo_siefore,
                   vprecio_del_dia
            FROM   cta_regimen a, glo_valor_accion b
            WHERE  a.nss = g_reg.nss
            AND    a.subcuenta = 1
            AND    a.codigo_siefore = b.codigo_siefore
            AND    b.fecha_valuacion = hoy


            IF vpesos >= g_reg.monto_ret THEN

               LET vacciones = g_reg.monto_ret / vprecio_del_dia

               CALL provisiona_cuenta(g_reg.monto_ret,        --- pesos
                                      vacciones,              --- acciones
                                      g_reg.nss,              --- solicitud
                                      g_reg.consec_reg_lote,
                                      g_reg.folio,
                                      g_reg.curp,
                                      1,                      --- subcuenta
                                      vtipo_movimiento,       --- tipo_movimiento
                                      vprecio_del_dia,        --- precio accion
                                      hoy,                    --- fecha valuacion
                                      vcodigo_siefore)        --- siefore

            ELSE
               IF  vpesos  < g_reg.monto_ret
               AND vpesos  > 0  THEN

                   CALL provisiona_cuenta(vpesos,                 --- pesos
                                          vacciones,              --- acciones
                                          g_reg.nss,              --- solicitud
                                          g_reg.consec_reg_lote,
                                          g_reg.folio,
                                          g_reg.curp,
                                          1,                      --- subcuenta
                                          vtipo_movimiento,       --- tipo_movimiento
                                          vprecio_del_dia,        --- precio accion
                                          hoy,                    --- fecha valuacion
                                          vcodigo_siefore)        --- siefore
               END IF
            END IF
         END IF

         IF g_reg.monto_act_ret > 0 THEN

            LET vtipo_movimiento = 545

            SELECT SUM((A.monto_en_acciones)* B.precio_del_dia),
                   SUM(A.monto_en_acciones)
            INTO   vpesos,
                   vacciones
            FROM   dis_cuenta A,
                   glo_valor_accion B
            WHERE  A.nss = g_reg.nss
            AND    A.subcuenta = 1
            AND    A.tipo_movimiento = 2
            AND    A.siefore = B.codigo_siefore
            AND    B.fecha_valuacion = hoy

            SELECT a.codigo_siefore ,
                   b.precio_del_dia
            INTO   vcodigo_siefore,
                   vprecio_del_dia
            FROM   cta_regimen a, glo_valor_accion b
            WHERE  a.nss = g_reg.nss
            AND    a.subcuenta = 1
            AND    a.codigo_siefore = b.codigo_siefore
            AND    b.fecha_valuacion = hoy


            IF vpesos >= g_reg.monto_act_ret THEN

               LET vacciones = g_reg.monto_act_ret / vprecio_del_dia

               CALL provisiona_cuenta(g_reg.monto_act_ret,    --- pesos
                                      vacciones,              --- acciones
                                      g_reg.nss,              --- solicitud
                                      g_reg.consec_reg_lote,
                                      g_reg.folio,
                                      g_reg.curp,
                                      1,                      --- subcuenta
                                      vtipo_movimiento,       --- tipo_movimiento
                                      vprecio_del_dia,        --- precio accion
                                      hoy,                    --- fecha valuacion
                                      vcodigo_siefore)        --- siefore
            ELSE
               IF  vpesos  < g_reg.monto_act_ret
               AND vpesos  > 0  THEN

                  CALL provisiona_cuenta(vpesos,               --- pesos
                                         vacciones,            --- acciones
                                         g_reg.nss,              --- solicitud
                                         g_reg.consec_reg_lote,
                                         g_reg.folio,
                                         g_reg.curp,
                                         1,                    --- subcuenta
                                         vtipo_movimiento,     --- tipo_movimiento
                                         vprecio_del_dia,      --- precio accion
                                         hoy,                  --- fecha valuacion
                                         vcodigo_siefore)      --- siefore
               END IF
            END IF
         END IF

         IF g_reg.mto_act_ret_rend > 0 THEN

            LET vtipo_movimiento = 550

            SELECT SUM((A.monto_en_acciones)* B.precio_del_dia),
                   SUM(A.monto_en_acciones)
            INTO   vpesos,
                   vacciones
            FROM   dis_cuenta A,
                   glo_valor_accion B
            WHERE  A.nss = g_reg.nss
            AND    A.subcuenta = 1
            AND    A.tipo_movimiento = 3
            AND    A.siefore = B.codigo_siefore
            AND    B.fecha_valuacion = hoy

            SELECT a.codigo_siefore ,
                   b.precio_del_dia 
            INTO   vcodigo_siefore,
                   vprecio_del_dia
            FROM   cta_regimen a, glo_valor_accion b
            WHERE  a.nss = g_reg.nss
            AND    a.subcuenta = 1
            AND    a.codigo_siefore = b.codigo_siefore
            AND    b.fecha_valuacion = hoy


            IF vpesos >= g_reg.mto_act_ret_rend THEN

               LET vacciones = g_reg.mto_act_ret_rend / vprecio_del_dia

               CALL provisiona_cuenta(g_reg.mto_act_ret_rend, --- pesos
                                      vacciones,              --- acciones
                                      g_reg.nss,              --- solicitud
                                      g_reg.consec_reg_lote,
                                      g_reg.folio,
                                      g_reg.curp,
                                      1,                      --- subcuenta
                                      vtipo_movimiento,       --- tipo_movimiento
                                      vprecio_del_dia,        --- precio accion
                                      hoy,                    --- fecha valuacion
                                      vcodigo_siefore)        --- siefore
            ELSE    
               IF  vpesos  < g_reg.mto_act_ret_rend
               AND vpesos  > 0  THEN

                  CALL provisiona_cuenta(vpesos,               --- pesos
                                         vacciones,            --- acciones
                                         g_reg.nss,              --- solicitud
                                         g_reg.consec_reg_lote,
                                         g_reg.folio,
                                         g_reg.curp,
                                         1,                    --- subcuenta
                                         vtipo_movimiento,     --- tipo_movimiento
                                         vprecio_del_dia,      --- precio accion
                                         hoy,                  --- fecha valuacion
                                         vcodigo_siefore)      --- siefore
               END IF
            END IF
         END IF

         IF g_reg.monto_ces_vej_pat > 0 THEN

            LET vtipo_movimiento = 540

            SELECT SUM((A.monto_en_acciones)* B.precio_del_dia),
                   SUM(A.monto_en_acciones)
            INTO   vpesos,
                   vacciones
            FROM   dis_cuenta A,
                   glo_valor_accion B
            WHERE  A.nss = g_reg.nss
            AND    A.subcuenta = 2
            AND    A.tipo_movimiento = 1
            AND    A.siefore = B.codigo_siefore
            AND    B.fecha_valuacion = hoy

            SELECT a.codigo_siefore ,
                   b.precio_del_dia 
            INTO   vcodigo_siefore,
                   vprecio_del_dia
            FROM   cta_regimen a, glo_valor_accion b
            WHERE  a.nss = g_reg.nss
            AND    a.subcuenta = 2
            AND    a.codigo_siefore = b.codigo_siefore
            AND    b.fecha_valuacion = hoy


            IF vpesos >= g_reg.monto_ces_vej_pat THEN

               LET vacciones = g_reg.monto_ces_vej_pat / vprecio_del_dia

               CALL provisiona_cuenta(g_reg.monto_ces_vej_pat,--- pesos
                                      vacciones,              --- acciones
                                      g_reg.nss,              --- solicitud
                                      g_reg.consec_reg_lote,
                                      g_reg.folio,
                                      g_reg.curp,
                                      2,                      --- subcuenta
                                      vtipo_movimiento,       --- tipo_movimiento
                                      vprecio_del_dia,        --- precio accion
                                      hoy,                    --- fecha valuacion
                                      vcodigo_siefore)        --- siefore
            ELSE    
               IF  vpesos  < g_reg.monto_ces_vej_pat
               AND vpesos  > 0  THEN

                   CALL provisiona_cuenta(vpesos,               --- pesos
                                          vacciones,            --- acciones
                                          g_reg.nss,              --- solicitud
                                          g_reg.consec_reg_lote,
                                          g_reg.folio,
                                          g_reg.curp,
                                          2,                    --- subcuenta
                                          vtipo_movimiento,     --- tipo_movimiento
                                          vprecio_del_dia,      --- precio accion
                                          hoy,                  --- fecha valuacion
                                          vcodigo_siefore)      --- siefore
               END IF
            END IF
         END IF

         IF g_reg.monto_act_cv_pat > 0 THEN

            LET vtipo_movimiento = 545

            SELECT SUM((A.monto_en_acciones)* B.precio_del_dia),
                   SUM(A.monto_en_acciones)
            INTO   vpesos,
                   vacciones
            FROM   dis_cuenta A,
                   glo_valor_accion B
            WHERE  A.nss = g_reg.nss
            AND    A.subcuenta = 2
            AND    A.tipo_movimiento = 2
            AND    A.siefore = B.codigo_siefore
            AND    B.fecha_valuacion = hoy

            SELECT a.codigo_siefore ,
                   b.precio_del_dia 
            INTO   vcodigo_siefore,
                   vprecio_del_dia
            FROM   cta_regimen a, glo_valor_accion b
            WHERE  a.nss = g_reg.nss
            AND    a.subcuenta = 2
            AND    a.codigo_siefore = b.codigo_siefore
            AND    b.fecha_valuacion = hoy


            IF vpesos >= g_reg.monto_act_cv_pat THEN

               LET vacciones = g_reg.monto_act_cv_pat / vprecio_del_dia

               CALL provisiona_cuenta(g_reg.monto_act_cv_pat, --- pesos
                                      vacciones,              --- acciones
                                      g_reg.nss,              --- solicitud
                                      g_reg.consec_reg_lote,
                                      g_reg.folio,
                                      g_reg.curp,
                                      2,                      --- subcuenta
                                      vtipo_movimiento,       --- tipo_movimiento
                                      vprecio_del_dia,        --- precio accion
                                      hoy,                    --- fecha valuacion
                                      vcodigo_siefore)        --- siefore
            ELSE    
               IF  vpesos  < g_reg.monto_act_cv_pat
               AND vpesos  > 0  THEN

                   CALL provisiona_cuenta(vpesos,               --- pesos
                                          vacciones,            --- acciones
                                          g_reg.nss,              --- solicitud
                                          g_reg.consec_reg_lote,
                                          g_reg.folio,
                                          g_reg.curp,
                                          2,                    --- subcuenta
                                          vtipo_movimiento,     --- tipo_movimiento
                                          vprecio_del_dia,      --- precio accion
                                          hoy,                  --- fecha valuacion
                                          vcodigo_siefore)      --- siefore
               END IF
            END IF
         END IF

         IF g_reg.mto_act_cv_rend_pat > 0 THEN

            LET vtipo_movimiento = 550

            SELECT SUM((A.monto_en_acciones)* B.precio_del_dia),
                   SUM(A.monto_en_acciones)
            INTO   vpesos,
                   vacciones
            FROM   dis_cuenta A,
                   glo_valor_accion B
            WHERE  A.nss = g_reg.nss
            AND    A.subcuenta = 2
            AND    A.tipo_movimiento = 3
            AND    A.siefore = B.codigo_siefore
            AND    B.fecha_valuacion = hoy

            SELECT a.codigo_siefore ,
                   b.precio_del_dia 
            INTO   vcodigo_siefore,
                   vprecio_del_dia
            FROM   cta_regimen a, glo_valor_accion b
            WHERE  a.nss = g_reg.nss
            AND    a.subcuenta = 2
            AND    a.codigo_siefore = b.codigo_siefore
            AND    b.fecha_valuacion = hoy


            IF vpesos >= g_reg.mto_act_cv_rend_pat THEN

               LET vacciones = g_reg.mto_act_cv_rend_pat / vprecio_del_dia

               CALL provisiona_cuenta(g_reg.mto_act_cv_rend_pat, --- pesos
                                      vacciones,                 --- acciones
                                      g_reg.nss,              --- solicitud
                                      g_reg.consec_reg_lote,
                                      g_reg.folio,
                                      g_reg.curp,
                                      2,                         --- subcuenta
                                      vtipo_movimiento,          --- tipo_movimiento
                                      vprecio_del_dia,           --- precio accion
                                      hoy,                       --- fecha valuacion
                                      vcodigo_siefore)           --- siefore
            ELSE    
               IF  vpesos  < g_reg.mto_act_cv_rend_pat
               AND vpesos  > 0  THEN

                   CALL provisiona_cuenta(vpesos,               --- pesos
                                          vacciones,            --- acciones
                                          g_reg.nss,              --- solicitud
                                          g_reg.consec_reg_lote,
                                          g_reg.folio,
                                          g_reg.curp,
                                          2,                    --- subcuenta
                                          vtipo_movimiento,     --- tipo_movimiento
                                          vprecio_del_dia,      --- precio accion
                                          hoy,                  --- fecha valuacion
                                          vcodigo_siefore)      --- siefore
               END IF
            END IF
         END IF
      END IF


      IF g_reg.clave_ent_orig  = "002" THEN
         IF g_reg.monto_aport_pat > 0 THEN

            LET vtipo_movimiento = 540

            LET vfecha_valor = MDY(MONTH(hoy),1,YEAR(hoy))

            SELECT SUM((A.monto_en_acciones)* B.precio_del_dia),
                   SUM(A.monto_en_acciones)
            INTO   vpesos,
                   vacciones
            FROM   dis_cuenta A,
                   glo_valor_accion B
            WHERE  A.nss = g_reg.nss
            AND    A.subcuenta = 4
            AND    A.siefore = B.codigo_siefore
            AND    B.fecha_valuacion = vfecha_valor

            SELECT a.codigo_siefore,
                   b.precio_del_dia
            INTO   vcodigo_siefore,
                   vprecio_del_dia
            FROM   cta_regimen a, glo_valor_accion b
            WHERE  a.nss = g_reg.nss
            AND    a.subcuenta = 4
            AND    a.codigo_siefore = b.codigo_siefore
            AND    b.fecha_valuacion = vfecha_valor


            IF vpesos >= g_reg.monto_aport_pat THEN

               LET vacciones = g_reg.monto_aport_pat / vprecio_del_dia

               CALL provisiona_cuenta(g_reg.monto_aport_pat,  --- pesos
                                      vacciones,              --- acciones
                                      g_reg.nss,              --- solicitud
                                      g_reg.consec_reg_lote,
                                      g_reg.folio,
                                      g_reg.curp,
                                      4,                      --- subcuenta
                                      vtipo_movimiento,       --- tipo_movimiento
                                      vprecio_del_dia,        --- precio accion
                                      vfecha_valor,           --- fecha valuacion
                                      vcodigo_siefore)        --- siefore
            ELSE    
              IF  vpesos  < g_reg.monto_aport_pat
              AND vpesos  > 0  THEN

                  CALL provisiona_cuenta(vpesos,               --- pesos
                                         vacciones,            --- acciones
                                         g_reg.nss,              --- solicitud
                                         g_reg.consec_reg_lote,
                                         g_reg.folio,
                                         g_reg.curp,
                                         4,                    --- subcuenta
                                         vtipo_movimiento,     --- tipo_movimiento
                                         vprecio_del_dia,      --- precio accion
                                         vfecha_valor,         --- fecha valuacion
                                         vcodigo_siefore)      --- siefore
              END IF
            END IF
         END IF
      END IF
   END FOREACH
END FUNCTION
#*********************************************************************
FUNCTION provisiona_cuenta_issste(vfolio,vcurp,vnss,vconsec_reg_lote)

   DEFINE g_reg  RECORD LIKE exc_det_exceso_issste.*

   DEFINE
      vcurp                   CHAR(18),
      vnss                    CHAR(11)
 
   DEFINE
      vacciones               DECIMAL(16,6),
      vpesos                  DECIMAL(16,6),
      vprecio_del_dia         DECIMAL(16,6)

   DEFINE
      vconsec_reg_lote        ,
      vfolio                  INTEGER

   DEFINE
      vcodigo_siefore         ,
      vtipo_movimiento        SMALLINT
      
   DECLARE cur_14 CURSOR FOR
   SELECT *
   FROM   exc_det_exceso_issste
   WHERE  folio = vfolio
   AND    curp  = vcurp
   AND    consec_reg_lote = vconsec_reg_lote

   FOREACH cur_14 INTO g_reg.*
 
      LET g_reg.nss = vnss

      IF g_reg.clave_ent_orig   = "001" AND
         g_reg.result_operacion = "01" THEN
         LET vtipo_movimiento = 543 --- LIQUIDA RCV TOTAL
      END IF
      
      IF g_reg.clave_ent_orig   = "001" AND 
         g_reg.result_operacion = "04" THEN
         LET vtipo_movimiento = 544 --- LIQUIDA RCV PARCIAL
      END IF
      
      IF g_reg.clave_ent_orig   = "002" AND
         g_reg.result_operacion = "01" THEN
         LET vtipo_movimiento = 553 --- LIQUIDA FOND VIV TOTAL
      END IF
      
      IF g_reg.clave_ent_orig   = "002" AND 
         g_reg.result_operacion = "04" THEN
         LET vtipo_movimiento = 554 --- LIQUIDA FOND VIV PARCIAL
      END IF   
   
      IF g_reg.clave_ent_orig  = "001" THEN
         IF g_reg.impt_sar_issste > 0 THEN
 
            IF vnom_archivo[1,4] = "BANX" THEN
               SELECT SUM(pesos),
                      SUM(acciones)
               INTO   vpesos,
                      vacciones
               FROM   temp_saldo_act
               WHERE  nss = g_reg.nss
               AND    subcuenta = 19
 
               IF vpesos >= g_reg.impt_sar_issste THEN
 
                  CALL provisiona_cuenta(g_reg.impt_sar_issste,  --- pesos
                                         0,                      --- acciones
                                         g_reg.nss,              --- solicitud
                                         g_reg.consec_reg_lote,
                                         g_reg.folio,
                                         g_reg.curp,
                                         19,                     --- subcuenta
                                         vtipo_movimiento,       --- tipo_movimiento
                                         0,                      --- precio accion
                                         hoy,                    --- fecha valuacion
                                         0)                      --- siefore
 
               ELSE
                  IF  vpesos  < g_reg.impt_sar_issste
                  AND vpesos  > 0  THEN

                      CALL provisiona_cuenta(vpesos,                 --- pesos
                                             0,                      --- acciones
                                             g_reg.nss,              --- solicitud
                                             g_reg.consec_reg_lote,
                                             g_reg.folio,
                                             g_reg.curp,
                                             19,                     --- subcuenta
                                             vtipo_movimiento,       --- tipo_movimiento
                                             0,                      --- precio accion
                                             hoy,                    --- fecha valuacion
                                             0)                      --- siefore
                  END IF                    
               END IF
            ELSE
               SELECT SUM(pesos),
                      SUM(acciones)
               INTO   vpesos,
                      vacciones
               FROM   temp_saldo_act
               WHERE  nss = g_reg.nss
               AND    subcuenta = 13

               SELECT a.codigo_siefore ,
                      b.precio_del_dia 
               INTO   vcodigo_siefore,
                      vprecio_del_dia
               FROM   cta_regimen a, glo_valor_accion b
               WHERE  a.nss = g_reg.nss
               AND    a.subcuenta = 13
               AND    a.codigo_siefore = b.codigo_siefore
               AND    b.fecha_valuacion = hoy
 
               IF vpesos >= g_reg.impt_sar_issste THEN
 
               	  LET vacciones = g_reg.impt_sar_issste / vprecio_del_dia
 
                  CALL provisiona_cuenta(g_reg.impt_sar_issste,  --- pesos
                                         vacciones,              --- acciones
                                         g_reg.nss,              --- solicitud
                                         g_reg.consec_reg_lote,
                                         g_reg.folio,
                                         g_reg.curp,
                                         13,                     --- subcuenta
                                         vtipo_movimiento,       --- tipo_movimiento
                                         vprecio_del_dia,        --- precio accion
                                         hoy,                    --- fecha valuacion
                                         vcodigo_siefore)        --- siefore
 
               ELSE
                  IF  vpesos  < g_reg.impt_sar_issste
                  AND vpesos  > 0  THEN

                      CALL provisiona_cuenta(vpesos,                 --- pesos
                                             vacciones,              --- acciones
                                             g_reg.nss,              --- solicitud
                                             g_reg.consec_reg_lote,
                                             g_reg.folio,
                                             g_reg.curp,
                                             13,                     --- subcuenta
                                             vtipo_movimiento,       --- tipo_movimiento
                                             vprecio_del_dia,        --- precio accion
                                             hoy,                    --- fecha valuacion
                                             vcodigo_siefore)        --- siefore
                  END IF                    
               END IF         	
            END IF
         END IF    
 
         IF g_reg.impt_ret_issste > 0 THEN

            SELECT SUM(pesos),
                   SUM(acciones)
            INTO   vpesos,
                   vacciones
            FROM   temp_saldo_act
            WHERE  nss = g_reg.nss
            AND    subcuenta = 30
 
            SELECT a.codigo_siefore ,
                   b.precio_del_dia 
            INTO   vcodigo_siefore,
                   vprecio_del_dia
            FROM   cta_regimen a, glo_valor_accion b
            WHERE  a.nss = g_reg.nss
            AND    a.subcuenta = 30
            AND    a.codigo_siefore = b.codigo_siefore
            AND    b.fecha_valuacion = hoy
               
            IF vpesos >= g_reg.impt_ret_issste THEN

               LET vacciones = g_reg.impt_ret_issste / vprecio_del_dia

               CALL provisiona_cuenta(g_reg.impt_ret_issste,  --- pesos
                                      vacciones,              --- acciones
                                      g_reg.nss,              --- solicitud
                                      g_reg.consec_reg_lote,
                                      g_reg.folio,
                                      g_reg.curp,
                                      30,                     --- subcuenta
                                      vtipo_movimiento,       --- tipo_movimiento
                                      vprecio_del_dia,        --- precio accion
                                      hoy,                    --- fecha valuacion
                                      vcodigo_siefore)        --- siefore
            ELSE    
               IF  vpesos  < g_reg.impt_ret_issste
               AND vpesos  > 0  THEN
               
                  CALL provisiona_cuenta(vpesos,               --- pesos
                                         vacciones,            --- acciones
                                         g_reg.nss,              --- solicitud
                                         g_reg.consec_reg_lote,
                                         g_reg.folio,
                                         g_reg.curp,
                                         30,                   --- subcuenta
                                         vtipo_movimiento,     --- tipo_movimiento
                                         vprecio_del_dia,      --- precio accion
                                         hoy,                  --- fecha valuacion
                                         vcodigo_siefore)      --- siefore
               END IF                    
            END IF
         END IF

         IF g_reg.impt_cv_patron > 0 THEN

            SELECT SUM(pesos),
                   SUM(acciones)
            INTO   vpesos,
                   vacciones
            FROM   temp_saldo_act
            WHERE  nss = g_reg.nss
            AND    subcuenta = 31

            SELECT a.codigo_siefore ,
                   b.precio_del_dia 
            INTO   vcodigo_siefore,
                   vprecio_del_dia
            FROM   cta_regimen a, glo_valor_accion b
            WHERE  a.nss = g_reg.nss
            AND    a.subcuenta = 31
            AND    a.codigo_siefore = b.codigo_siefore
            AND    b.fecha_valuacion = hoy

            IF vpesos >= g_reg.impt_cv_patron THEN

               LET vacciones = g_reg.impt_cv_patron / vprecio_del_dia

               CALL provisiona_cuenta(g_reg.impt_cv_patron,   --- pesos
                                      vacciones,              --- acciones
                                      g_reg.nss,              --- solicitud
                                      g_reg.consec_reg_lote,
                                      g_reg.folio,
                                      g_reg.curp,
                                      31,                     --- subcuenta
                                      vtipo_movimiento,       --- tipo_movimiento
                                      vprecio_del_dia,        --- precio accion
                                      hoy,                    --- fecha valuacion
                                      vcodigo_siefore)        --- siefore
            ELSE    
               IF  vpesos  < g_reg.impt_cv_patron
               AND vpesos  > 0  THEN

                  CALL provisiona_cuenta(vpesos,               --- pesos
                                         vacciones,            --- acciones
                                         g_reg.nss,              --- solicitud
                                         g_reg.consec_reg_lote,
                                         g_reg.folio,
                                         g_reg.curp,
                                         31,                   --- subcuenta
                                         vtipo_movimiento,     --- tipo_movimiento
                                         vprecio_del_dia,      --- precio accion
                                         hoy,                  --- fecha valuacion
                                         vcodigo_siefore)      --- siefore
               END IF
            END IF
         END IF

         IF g_reg.impt_ahorro_solid > 0 THEN

            SELECT SUM(pesos),
                   SUM(acciones)
            INTO   vpesos,
                   vacciones
            FROM   temp_saldo_act
            WHERE  nss = g_reg.nss
            AND    subcuenta = 33
            
            SELECT a.codigo_siefore ,
                   b.precio_del_dia 
            INTO   vcodigo_siefore,
                   vprecio_del_dia
            FROM   cta_regimen a, glo_valor_accion b
            WHERE  a.nss = g_reg.nss
            AND    a.subcuenta = 33
            AND    a.codigo_siefore = b.codigo_siefore
            AND    b.fecha_valuacion = hoy
            
            IF vpesos >= g_reg.impt_ahorro_solid THEN

               LET vacciones = g_reg.impt_ahorro_solid / vprecio_del_dia

               CALL provisiona_cuenta(g_reg.impt_ahorro_solid,   --- pesos
                                      vacciones,              --- acciones
                                      g_reg.nss,              --- solicitud
                                      g_reg.consec_reg_lote,
                                      g_reg.folio,
                                      g_reg.curp,
                                      33,                     --- subcuenta
                                      vtipo_movimiento,       --- tipo_movimiento
                                      vprecio_del_dia,        --- precio accion
                                      hoy,                    --- fecha valuacion
                                      vcodigo_siefore)        --- siefore
            ELSE    
               IF  vpesos  < g_reg.impt_ahorro_solid
               AND vpesos  > 0  THEN

                   CALL provisiona_cuenta(vpesos,               --- pesos
                                          vacciones,            --- acciones
                                          g_reg.nss,              --- solicitud
                                          g_reg.consec_reg_lote,
                                          g_reg.folio,
                                          g_reg.curp,
                                          33,                   --- subcuenta
                                          vtipo_movimiento,     --- tipo_movimiento
                                          vprecio_del_dia,      --- precio accion
                                          hoy,                  --- fecha valuacion
                                          vcodigo_siefore)      --- siefore
               END IF                    
            END IF    
         END IF
      END IF

      IF g_reg.clave_ent_orig  = "002" THEN
         IF g_reg.impt_fondo_viv92 > 0 THEN
         
            SELECT SUM(pesos),
                   SUM(acciones)
            INTO   vpesos,
                   vacciones
            FROM   temp_saldo_act
            WHERE  nss = g_reg.nss
            AND    subcuenta = 14

            SELECT a.codigo_siefore ,
                   b.precio_del_dia 
            INTO   vcodigo_siefore,
                   vprecio_del_dia
            FROM   cta_regimen a, glo_valor_accion b
            WHERE  a.nss = g_reg.nss
            AND    a.subcuenta = 14
            AND    a.codigo_siefore = b.codigo_siefore
            AND    b.fecha_valuacion = hoy
             
            IF vpesos >= g_reg.impt_fondo_viv92 THEN

               LET vacciones = g_reg.impt_fondo_viv92 / vprecio_del_dia

               CALL provisiona_cuenta(g_reg.impt_fondo_viv92,   --- pesos
                                      vacciones,              --- acciones
                                      g_reg.nss,              --- solicitud
                                      g_reg.consec_reg_lote,
                                      g_reg.folio,
                                      g_reg.curp,
                                      14,                     --- subcuenta
                                      vtipo_movimiento,       --- tipo_movimiento
                                      vprecio_del_dia,        --- precio accion
                                      hoy,                    --- fecha valuacion
                                      vcodigo_siefore)        --- siefore
            ELSE    
              IF  vpesos  < g_reg.impt_fondo_viv92
              AND vpesos  > 0  THEN

                  CALL provisiona_cuenta(vpesos,               --- pesos
                                         vacciones,            --- acciones
                                         g_reg.nss,              --- solicitud
                                         g_reg.consec_reg_lote,
                                         g_reg.folio,
                                         g_reg.curp,
                                         14,                   --- subcuenta
                                         vtipo_movimiento,     --- tipo_movimiento
                                         vprecio_del_dia,      --- precio accion
                                         hoy,                  --- fecha valuacion
                                         vcodigo_siefore)      --- siefore
              END IF
            END IF
         END IF

         IF g_reg.impt_fondo_viv08 > 0 THEN

            SELECT SUM(pesos),
                   SUM(acciones)
            INTO   vpesos,
                   vacciones
            FROM   temp_saldo_act
            WHERE  nss = g_reg.nss
            AND    subcuenta = 35
           
            SELECT a.codigo_siefore ,
                   b.precio_del_dia 
            INTO   vcodigo_siefore,
                   vprecio_del_dia
            FROM   cta_regimen a, glo_valor_accion b
            WHERE  a.nss = g_reg.nss
            AND    a.subcuenta = 35
            AND    a.codigo_siefore = b.codigo_siefore
            AND    b.fecha_valuacion = hoy
           
            IF vpesos >= g_reg.impt_fondo_viv08 THEN

               LET vacciones = g_reg.impt_fondo_viv08 / vprecio_del_dia

               CALL provisiona_cuenta(g_reg.impt_fondo_viv08,   --- pesos
                                      vacciones,              --- acciones
                                      g_reg.nss,              --- solicitud
                                      g_reg.consec_reg_lote,
                                      g_reg.folio,
                                      g_reg.curp,
                                      35,                     --- subcuenta
                                      vtipo_movimiento,       --- tipo_movimiento
                                      vprecio_del_dia,        --- precio accion
                                      hoy,                    --- fecha valuacion
                                      vcodigo_siefore)        --- siefore
            ELSE    
               IF  vpesos  < g_reg.impt_fondo_viv08
               AND vpesos  > 0  THEN

                   CALL provisiona_cuenta(vpesos,               --- pesos
                                          vacciones,            --- acciones
                                          g_reg.nss,              --- solicitud
                                          g_reg.consec_reg_lote,
                                          g_reg.folio,
                                          g_reg.curp,
                                          35,                   --- subcuenta
                                          vtipo_movimiento,     --- tipo_movimiento
                                          vprecio_del_dia,      --- precio accion
                                          hoy,                  --- fecha valuacion
                                          vcodigo_siefore)      --- siefore
               END IF
            END IF
         END IF
      END IF
   END FOREACH
END FUNCTION
#*********************************************************************
FUNCTION provisiona_cuenta(monto_reg,        --- pesos            
                           monto_acc_reg,    --- acciones         
                           reg_2,            --- solicitud        
                           subcuenta,        --- subcuenta        
                           movimiento,       --- tipo_movimiento  
                           precio_dia,       --- precio accion    
                           x_fecha_proceso,  --- fecha valuacion  
                           siefore)          --- siefore          

   DEFINE  monto_reg          DECIMAL(16,6),
           monto_acc_reg      DECIMAL(16,6),
           subcuenta          INTEGER,
           movimiento         INTEGER,
           precio_dia         DECIMAL(16,6),
           monto_en_pesos     DECIMAL(16,6),
           monto_en_acciones  DECIMAL(16,6),
           vfecha_archivo     DATE,
           aux_id_aportante   CHAR(20),
           x_fecha_proceso    DATE,
           siefore            CHAR(6),
           x_status           SMALLINT,
           vfolio_sua         CHAR(6),
           vfecha_proceso     DATE

   DEFINE reg_2 RECORD
      nss                 CHAR(11),
      consec_reg_lote     INTEGER,
      folio               INTEGER,
      curp                CHAR(18)
   END RECORD

   DEFINE f_saldo_act  SMALLINT

   LET f_saldo_act = 0

   IF vparametro1 = "RCV"
   OR vparametro1 = "VIVIENDA" THEN #-- ARCHIVO IMSS --#
      LET aux_id_aportante = "PAG-EXC-PAT"

      LET monto_en_pesos    = monto_reg * -1

      LET monto_en_acciones = monto_acc_reg * -1

      IF subcuenta = 4 THEN
         LET vfecha_proceso = hoy
      ELSE
         LET vfecha_proceso = x_fecha_proceso
      END IF

      INSERT INTO dis_provision
      VALUES (movimiento            ,
              subcuenta             ,
              siefore               ,
              reg_2.folio           ,
              reg_2.consec_reg_lote ,
              reg_2.nss             ,
              ''                    , --curp
              ''                    ,
              vfecha_proceso        , --fecha_pago
              x_fecha_proceso       , --fecha_valor
              vfecha_proceso        , --fecha_conversion
              monto_en_pesos        ,
              monto_en_acciones     ,
              precio_dia            ,
              '0'                   ,--dias cotizados
              ''                    ,--sucursal
              aux_id_aportante      ,
              '6'                   ,--estado
              hoy                   ,--fecha_proceso
              USER                  ,
              hoy                   , --fecha_archivo
              '1'                   --etiqueta
              );

   ELSE
      CALL saldo_actualizado(reg_2.nss,
                             subcuenta,
                             siefore,
                             monto_acc_reg,      --acciones
                             monto_reg)          --pesos
         RETURNING f_saldo_act

      IF f_saldo_act = 100 THEN

         LET monto_en_pesos    = monto_reg * -1

         LET monto_en_acciones = monto_acc_reg * -1

         LET aux_id_aportante = "EXC-ISSSTE"

         LET vfolio_sua = ""
         DECLARE cursor_prov_cargo CURSOR FOR clausula_sql3

         OPEN cursor_prov_cargo USING reg_2.folio,
                                      vfolio_sua,
                                      reg_2.nss,
                                      subcuenta,
                                      movimiento,
                                      reg_2.consec_reg_lote,
                                      siefore,
                                      monto_en_acciones,
                                      monto_en_pesos,
                                      aux_id_aportante,
                                      x_fecha_proceso

         FETCH cursor_prov_cargo INTO x_status

         CLOSE cursor_prov_cargo

         CALL actualiza_saldos_tmp(reg_2.nss,
                                   subcuenta,
                                   siefore,
                                   monto_acc_reg,  --acciones
                                   monto_reg)      --pesos
      END IF
   END IF
END FUNCTION
#*********************************************************************
FUNCTION saldo_actualizado(v_nss,
                           v_subcuenta,
                           v_siefore,
                           v_monto_acc,
                           v_monto_pes)

   DEFINE v_saldo_dia        CHAR(100),
          v_nss              CHAR(11),
          v_subcuenta        SMALLINT,
          v_grupo            SMALLINT,
          v_siefore          SMALLINT,
          v_monto_acc        DECIMAL(16,6),
          v_monto_pes        DECIMAL(16,6)

   DEFINE f_subcuenta        SMALLINT,
          f_siefore          SMALLINT,
          f_monto_acc        DECIMAL(16,6),
          f_monto_pes        DECIMAL(16,6)

   DEFINE v_saldo_acc        DECIMAL(16,6),
          v_saldo_pes        DECIMAL(16,6),
          hoy                DATE

   DEFINE li_flag            SMALLINT,
          aux_bandera        SMALLINT

   LET hoy     = TODAY
   LET li_flag = 0
   LET v_grupo = 0

-- VERIFICA TABLA TEMPORAL
   WHENEVER ERROR CONTINUE
    SELECT COUNT(*)
    FROM temp_saldo_act
   WHENEVER ERROR STOP

   IF SQLCA.SQLCODE != 0 THEN
      CREATE TEMP TABLE temp_saldo_act
      (nss            CHAR(11),
       subcuenta      SMALLINT,
       siefore        SMALLINT,
       acciones       DECIMAL(16,6),
       pesos          DECIMAL(16,6)
      )
   END IF

-- VERIFICA SI EXISTE NSS
   SELECT COUNT(*)
   INTO li_flag
   FROM temp_saldo_act
   WHERE nss = v_nss

   IF li_flag = 0 THEN
-- INSERTA SALDO DE NSS
      LET v_saldo_dia = "EXECUTE FUNCTION fn_saldo_dia ( ?,?,?,? ) "
      LET li_flag = 0

      PREPARE eje_saldo_dia_act FROM v_saldo_dia
      DECLARE c_saldo_act CURSOR FOR  eje_saldo_dia_act

      FOREACH c_saldo_act USING v_nss,
                                li_flag,  --Subcuenta 0
                                v_grupo,  --Grupo     0
                                hoy
                          INTO f_subcuenta,
                               f_siefore,
                               f_monto_acc,
                               f_monto_pes

         INSERT INTO temp_saldo_act
         VALUES (v_nss,
                 f_subcuenta,
                 f_siefore,
                 f_monto_acc,
                 f_monto_pes
                )
      END FOREACH
      UPDATE STATISTICS FOR TABLE temp_saldo_act
   END IF

   LET f_monto_acc = 0
   LET f_monto_pes = 0

-- RECUPERA SALDO ACTUAL
   SELECT acciones,
          pesos
   INTO   f_monto_acc,
          f_monto_pes
   FROM   temp_saldo_act
   WHERE  nss       = v_nss
   AND    subcuenta = v_subcuenta
   AND    siefore   = v_siefore
   
   IF v_subcuenta = 14 OR
   	  v_subcuenta = 35 THEN
      IF v_monto_acc <= f_monto_acc THEN
         LET aux_bandera = 100   -- "SUFICIENTE"
      ELSE
         LET aux_bandera = 593 
display "INSUFICIENTE 593"    
      END IF   	  	
   ELSE
      IF v_monto_acc <= f_monto_acc OR
         v_monto_pes <= f_monto_pes THEN
         LET aux_bandera = 100   -- "SUFICIENTE"
      ELSE
        LET aux_bandera = 592
      END IF   	
   END IF

   RETURN aux_bandera
END FUNCTION
#*********************************************************************
FUNCTION actualiza_saldos_tmp(v_nss,
                              v_subcuenta,
                              v_siefore,
                              v_monto_acc,
                              v_monto_pes)

   DEFINE v_nss              CHAR(11),
          v_subcuenta        SMALLINT,
          v_grupo            SMALLINT,
          v_siefore          SMALLINT,
          v_monto_acc        DECIMAL(16,6),
          v_monto_pes        DECIMAL(16,6)

   DEFINE f_monto_acc        DECIMAL(16,6),
          f_monto_pes        DECIMAL(16,6)

   SELECT acciones,
          pesos
   INTO   f_monto_acc,
          f_monto_pes
   FROM   temp_saldo_act
   WHERE  nss       = v_nss
   AND    subcuenta = v_subcuenta
   AND    siefore   = v_siefore

   LET f_monto_acc = f_monto_acc - v_monto_acc
   LET f_monto_pes = f_monto_pes - v_monto_pes

   UPDATE temp_saldo_act
   SET    acciones = f_monto_acc,
          pesos    = f_monto_pes
   WHERE  nss       = v_nss
   AND    subcuenta = v_subcuenta
   AND    siefore   = v_siefore
END FUNCTION
#*********************************************************************
FUNCTION reversa_parciales(vfolio)

   DEFINE
      vcurp             CHAR(18),
      vnss              CHAR(11),
      vclave_ent_orig   CHAR(3),
      vtipo_rech        CHAR(3)
      
   DEFINE
      vconsec_reg_lote  ,
      vfolio            INTEGER
      
   DEFINE
      vmarca_entra      ,
      vmarca_cod        SMALLINT

   IF vparametro1 = "RCV"
   OR vparametro1 = "VIVIENDA" THEN #-- ARCHIVO IMSS --#

      DECLARE cur_11 CURSOR FOR
      SELECT nss
      FROM   exc_det_exceso
      WHERE  folio = vfolio
      AND    result_operacion = "04"
      GROUP BY 1

      FOREACH cur_11 INTO vnss

         DISPLAY "REVERSO DE PARCIALES"
         DISPLAY "vnss ",vnss

         DECLARE cur_12 CURSOR FOR
         SELECT consec_reg_lote,
                clave_ent_orig
         FROM   exc_det_exceso
         WHERE  folio = vfolio
         AND    nss = vnss
         AND    result_operacion IN ("01","04")

         FOREACH cur_12 INTO vconsec_reg_lote,
                             vclave_ent_orig

            SELECT n_seguro
            INTO   vnss
            FROM   afi_mae_afiliado
            WHERE  n_seguro = vnss
            AND    tipo_solicitud = 8

            IF vclave_ent_orig = "001" THEN
               SELECT marca_cod 
               INTO   vmarca_cod
               FROM   cta_act_marca
               WHERE  nss = vnss
               AND    correlativo = vconsec_reg_lote
               AND    marca_cod = 540
            ELSE
               SELECT marca_cod
               INTO   vmarca_cod
               FROM   cta_act_marca
               WHERE  nss = vnss
               AND    correlativo = vconsec_reg_lote
               AND    marca_cod = 542
            END IF

            EXECUTE cla_rev_marca USING vnss,
                                  vmarca_cod,
                                  vconsec_reg_lote

            DELETE
            FROM dis_provision 
            WHERE  folio = vfolio
            AND    nss = vnss
            AND    consecutivo_lote = vconsec_reg_lote

            IF vclave_ent_orig  = "001" THEN
               LET vtipo_rech = "592"  #-- RECHAZADO --#
               LET vmarca_entra = 540
            END IF

            IF vclave_ent_orig  = "002" THEN
               LET vtipo_rech = "593"  #-- RECHAZADO --#
               LET vmarca_entra = 542
            END IF

            CALL actualiza_reg(vfolio,
                               vcurp,
                               vnss,
                               "02",
                               vtipo_rech,
                               vconsec_reg_lote,
                               "",
                               vmarca_entra,
                               gusuario)
         END FOREACH
      END FOREACH 
   ELSE
      DECLARE cur_5 CURSOR FOR
      SELECT curp
      FROM   exc_det_exceso_issste
      WHERE  folio = vfolio
      AND    result_operacion = "04"
      GROUP BY 1

      FOREACH cur_5 INTO vcurp

         DISPLAY "REVERSO DE PARCIALES"
         DISPLAY "vcurp ",vcurp

         DECLARE cur_6 CURSOR FOR
         SELECT consec_reg_lote,
                clave_ent_orig
         FROM   exc_det_exceso_issste
         WHERE  folio = vfolio
         AND    curp = vcurp
         AND    result_operacion IN ("01","04")

         FOREACH cur_6 INTO vconsec_reg_lote,
                            vclave_ent_orig

            SELECT n_seguro
            INTO   vnss
            FROM   afi_mae_afiliado
            WHERE  n_unico = vcurp
            AND    tipo_solicitud = 8

            IF vclave_ent_orig = "001" THEN
               SELECT marca_cod 
               INTO   vmarca_cod
               FROM   cta_act_marca
               WHERE  nss = vnss
               AND    correlativo = vconsec_reg_lote
               AND    marca_cod = 543
            ELSE
               SELECT marca_cod 
               INTO   vmarca_cod
               FROM   cta_act_marca
               WHERE  nss = vnss
               AND    correlativo = vconsec_reg_lote
               AND    marca_cod = 544
            END IF

            EXECUTE cla_rev_marca USING vnss,
                                  vmarca_cod,
                                  vconsec_reg_lote

            DELETE
            FROM dis_provision
            WHERE  folio = vfolio
            AND    nss = vnss
            AND    consecutivo_lote = vconsec_reg_lote

            IF vclave_ent_orig  = "001" THEN
               LET vtipo_rech = "592"  #-- RECHAZADO --#
               LET vmarca_entra = 543
            END IF

            IF vclave_ent_orig  = "002" THEN
               LET vtipo_rech = "593"  #-- RECHAZADO --#
               LET vmarca_entra = 544
            END IF              

            CALL actualiza_reg(vfolio,
                               vcurp,
                               vnss,
                               "02",
                               vtipo_rech,
                               vconsec_reg_lote,
                               "",
                               vmarca_entra,
                               gusuario)
         END FOREACH
      END FOREACH
   END IF
END FUNCTION
#*********************************************************************
FUNCTION actualiza_reg(rfolio,
                       vcurp,
                       vnss,
                       res_op,
                       tipo_rech,
                       x_consecutivo,
                       x_folio_sua,
                       x_mig_marca_entra,
                       x_gusuario)

   DEFINE rfolio               INTEGER,
          vcurp                CHAR(18),
          vnss                 CHAR(11),
          res_op               CHAR(02),
          tipo_rech            CHAR(03),
          x_consecutivo        INTEGER,
          x_folio_sua          CHAR(6),
          x_mig_marca_entra    SMALLINT,
          x_gusuario           CHAR(8),
          x_vestado_proceso    SMALLINT,
          x_mig_codigo_rechazo SMALLINT


   IF tipo_rech = 588 OR
      tipo_rech = 528 OR
      tipo_rech = 529 OR
      tipo_rech = 530 OR
      tipo_rech = 575 OR
      tipo_rech = 586 OR
      tipo_rech = 587 THEN
   ELSE
      CALL marca(vnss,              -- nss
                 x_mig_marca_entra, -- marca_entra
                 x_consecutivo,     -- correlativo
                 30,                -- marca_estado
                 tipo_rech,         -- codigo_rechazo
                 0,                 -- marca_causa
                 " ",               -- fecha_causa
                 x_gusuario         -- usuario
                )              

           RETURNING x_vestado_proceso,
                     x_mig_codigo_rechazo
   END IF

   IF vparametro1 = "RCV"
   OR vparametro1 = "VIVIENDA" THEN #-- ARCHIVO IMSS --#
      UPDATE exc_det_exceso
      SET    result_operacion = res_op,
             tipo_diagnostico = tipo_rech
      WHERE  nss = vnss
      AND    folio = rfolio
      AND    consec_reg_lote = x_consecutivo
   ELSE
      UPDATE exc_det_exceso_issste
      SET    result_operacion = res_op,
             tipo_diagnostico = tipo_rech
      WHERE  curp = vcurp
      AND    folio = rfolio
      AND    consec_reg_lote = x_consecutivo
   END IF

END FUNCTION
#*********************************************************************
FUNCTION marca(x_nss,
               x_marca_entra,
               x_consec_reg_lote,
               x_marca_estado,
               x_codigo_rechazo,
               x_marca_causa,
               x_fecha_causa,
               x_usuario)

   DEFINE x_nss             CHAR(11),
          x_operacion       CHAR(1),
          x_marca_entra     SMALLINT,
          x_consec_reg_lote SMALLINT,
          x_marca_estado    SMALLINT,
          x_codigo_rechazo  SMALLINT,
          x_marca_causa     SMALLINT,
          x_fecha_causa     DATE,
          x_usuario         CHAR(8),
          xx_codigo_marca   SMALLINT,
          xx_codigo_rechazo SMALLINT,
          ejecuta_procedure CHAR(200)

   DECLARE cursor_marca CURSOR FOR clausula_sql4

   OPEN cursor_marca USING x_nss,
                           x_marca_entra,
                           x_consec_reg_lote,
                           x_marca_estado,
                           x_codigo_rechazo,
                           x_marca_causa,
                           x_fecha_causa,
                           x_usuario

      FETCH cursor_marca INTO xx_codigo_marca,
                              xx_codigo_rechazo

   CLOSE cursor_marca

   RETURN xx_codigo_marca,
          xx_codigo_rechazo

END FUNCTION
