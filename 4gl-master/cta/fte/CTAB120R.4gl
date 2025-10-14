#############################################################################
#Proyecto            => SAFRE ( MEXICO )                                    #
#Propietario         => E.F.P.                                              #
#Programa            => CTAB120                                             #
#Descripcion         => GENERA REPORTE DE REGISTROS SALDO CERO              #
#Por                 => OMAR SABNDOVAL BADILLO                              #
#Fecha               => 10 de noviembre de 2006                             #
#Sistema             => CTA.                                                #
#############################################################################

DATABASE safre_af

GLOBALS

   DEFINE g_parametro RECORD LIKE seg_modulo.*

   DEFINE hoy         DATE,
          opc         CHAR(1),
          v_usuario   CHAR(8),
          sp_01       CHAR(50)

   DEFINE fecha_inicio_sc DATE,
          p_fecha_corte   DATE,
          p_fecha_lote    DATE

   DEFINE l_reg_01 RECORD
          nss              CHAR(11),
          tipo_solicitud   SMALLINT,
          curp             CHAR(18),
          fentcons         DATE
   END RECORD

   DEFINE l_reg_02 RECORD
          nss              CHAR(11),
          tipo_solicitud   SMALLINT,
          curp             CHAR(18),
          fentcons         DATE
   END RECORD

   DEFINE cve_ent_origen CHAR(3)

   DEFINE dis_reg  RECORD
          fecha_conversion   DATE,
          tipo_movimiento    SMALLINT
   END RECORD

   DEFINE vmarca          CHAR(200)

   DEFINE vmarca_ent      SMALLINT,
          xmarca          SMALLINT,
          xrechazo        SMALLINT,
          vcorrelativo    SMALLINT,
          vestado_marca   SMALLINT,
          vcodigo_rechazo SMALLINT,
          vmarca_causa    SMALLINT,
          vfecha_causa    DATE

END GLOBALS

MAIN

   CALL inicio()
   CALL proceso()

END MAIN

FUNCTION inicio()
#i---------------

   LET p_fecha_corte = ARG_VAL(1)
   LET p_fecha_lote  = ARG_VAL(2)

   SELECT *,
          USER
   INTO   g_parametro.*,
          v_usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"

   SELECT codigo_afore
   INTO   cve_ent_origen
   FROM   tab_afore_local

   LET hoy = TODAY

   LET sp_01 = "EXECUTE FUNCTION fn_fecha_ini_saldo_cero(?)"

   LET vmarca = " EXECUTE PROCEDURE marca_cuenta (?,?,?,?,?,?,?,?)"

   WHENEVER ERROR CONTINUE
     DROP TABLE tmp_cuentas_cero
     DROP TABLE nss_inhab
   WHENEVER ERROR STOP

   CREATE TEMP TABLE tmp_cuentas_cero (nss CHAR(11))
   CREATE TEMP TABLE nss_inhab (nss  CHAR(11))

END FUNCTION

FUNCTION proceso()
#p----------------

   DEFINE v_nss              CHAR(11),
          v_tipo_solicitud   SMALLINT,
          v_tipo_cuenta      CHAR(2),
          v_tipo_trabajador  CHAR(2),
          v_fecha_conversion,
          v_fecha_operacion  DATE

   PREPARE marcaje FROM vmarca

   PREPARE proc_01 FROM sp_01
   EXECUTE proc_01 USING p_fecha_corte
                    INTO fecha_inicio_sc

   UPDATE cta_ctr_cuenta
   SET    ind_saldo_cero   = 0,
          fecha_saldo_cero = NULL

   LET vmarca_ent = 150
   LET vcorrelativo = 0
   LET vmarca_causa = 0
   LET vfecha_causa = ''
   LET vestado_marca = 0

---para los tipos de cuenta 01

   DECLARE cur_tra01 CURSOR FOR
   SELECT a.n_seguro,
          a.tipo_solicitud,
          a.n_unico,
          a.fentcons
   FROM   afi_mae_afiliado a,
          safre_tmp:cuota_afore c
   WHERE  a.n_seguro = c.nss
     AND  a.n_seguro NOT IN(SELECT nss
                          FROM   safre_tmp:tmp_saldo_corte
                         )
   AND   a.fentcons <= fecha_inicio_sc
   ORDER BY 1,2

   LET v_tipo_cuenta = "01"

   FOREACH cur_tra01 INTO l_reg_01.*

      INSERT INTO tmp_cuentas_cero VALUES ( l_reg_01.nss )

      CALL tipo_trabajador(l_reg_01.nss,
                           l_reg_01.tipo_solicitud)
           RETURNING v_nss,
                     v_tipo_trabajador

-----  MARCAJE DE CUENTA -----------------

   LET xrechazo = 0

   DECLARE cur_mar01 CURSOR FOR marcaje
   OPEN  cur_mar01 USING l_reg_01.nss,      #pnss
                       vmarca_ent,          #marca_entra
                       vcorrelativo,        #correlativo
                       vestado_marca,       #estado_marca
                       vcodigo_rechazo,     #codigo_rechazo
                       vmarca_causa,        #marca_causa
                       vfecha_causa,        #fecha_causa
                       v_usuario            #usuario

   FETCH cur_mar01 INTO xmarca,
                      xrechazo
   CLOSE cur_mar01

-----------------------

   IF xrechazo <> 0 THEN

      INSERT INTO cta_rch_saldo_cero -- Almacena cuentas que no pudo marcar
         VALUES(l_reg_01.nss           ,
                l_reg_01.tipo_solicitud,
                l_reg_01.curp          ,
                v_nss                  ,
                v_tipo_trabajador      ,
                v_tipo_cuenta          ,
                l_reg_01.fentcons      ,
                "1"                    ,
                "",
                "",
                "",
                p_fecha_corte          ,
                xrechazo               ,
                hoy
                )
    ELSE
         LET dis_reg.fecha_conversion = NULL
         LET dis_reg.tipo_movimiento = 0

         CALL genera_tmp_cuenta (l_reg_01.nss     ,
                                 l_reg_01.fentcons,
                                 p_fecha_corte
                             )
         DECLARE cur_sc3x CURSOR FOR
            SELECT
                   fecha_conversion,
                   tipo_movimiento
            FROM   tmp_dis_cuenta
            WHERE  fecha_conversion <= fecha_inicio_sc
            AND    tipo_movimiento NOT BETWEEN 100 AND 114
            ORDER BY 2 DESC

            FOREACH cur_sc3x INTO dis_reg.*
               EXIT FOREACH
            END FOREACH
         CLOSE cur_sc3x

         IF dis_reg.fecha_conversion IS NOT NULL AND
            dis_reg.fecha_conversion <> "12311899" THEN
            LET v_tipo_cuenta = "02"
            LET v_fecha_operacion = dis_reg.fecha_conversion
         ELSE 
            LET v_tipo_cuenta = "01"
            LET dis_reg.tipo_movimiento = 0
            LET v_fecha_operacion = l_reg_01.fentcons
         END  IF
    
         INSERT INTO cta_saldo_cero
         VALUES(l_reg_01.nss           ,
                l_reg_01.tipo_solicitud,
                l_reg_01.curp          ,
                v_nss                  ,
                v_tipo_trabajador      ,
                v_tipo_cuenta          ,
                l_reg_01.fentcons      ,
                "1"                    ,
                v_fecha_operacion     ,
                dis_reg.tipo_movimiento,
                "",
                p_fecha_corte          ,
                hoy
                )

         UPDATE cta_ctr_cuenta
         SET    ind_saldo_cero   = 1,
                fecha_saldo_cero = v_fecha_operacion
         WHERE  nss              = l_reg_01.nss

   END IF


   END FOREACH
   CLOSE cur_tra01

   CALL Actualiza_etapa(1) --Termina proceso cuentas '01' saldo cero

   CALL Ingresa_etapa(2)   --Inicia proceso de cuentas '02' saldo cero
--para los tipos de cuenta 02

   INSERT INTO tmp_cuentas_cero
   SELECT nss
   FROM   safre_tmp:tmp_saldo_corte
   WHERE  subcuenta NOT IN(14,19)
     AND  monto_en_acciones > 0
   GROUP BY 1

   INSERT INTO tmp_cuentas_cero
   SELECT nss
   FROM   safre_tmp:tmp_saldo_corte
   WHERE  subcuenta IN(14,19)
     AND  monto_en_pesos > 0
   GROUP BY 1

   CREATE INDEX tmp_ctas_cero1 ON tmp_cuentas_cero ( nss )
   UPDATE STATISTICS FOR TABLE tmp_cuentas_cero

   DECLARE cur_tra02 CURSOR FOR
   SELECT a.n_seguro,
          a.tipo_solicitud,
          a.n_unico,
          a.fentcons
   FROM   afi_mae_afiliado a,
          safre_tmp:cuota_afore c
   WHERE  a.n_seguro = c.nss
     AND  a.n_seguro NOT IN(SELECT nss
                            FROM tmp_cuentas_cero
                         )
     AND  a.fentcons <= fecha_inicio_sc
   ORDER BY 1

   LET v_tipo_cuenta = "02"

   FOREACH cur_tra02 INTO l_reg_02.*

      CALL genera_tmp_cuenta (l_reg_02.nss     ,
                              l_reg_02.fentcons,
                              p_fecha_corte
                             )
      SELECT "X"
      FROM   tmp_dis_cuenta
      WHERE  fecha_conversion >  fecha_inicio_sc
      AND    fecha_conversion <= p_fecha_corte
      GROUP BY 1

      IF SQLCA.SQLCODE = NOTFOUND THEN

         DECLARE cur_sc3 CURSOR FOR
            SELECT
                   fecha_conversion,
                   tipo_movimiento
            FROM   tmp_dis_cuenta
            WHERE  fecha_conversion <= fecha_inicio_sc
            AND    tipo_movimiento NOT BETWEEN 100 AND 114
            ORDER BY 2 DESC

            FOREACH cur_sc3 INTO dis_reg.*
               EXIT FOREACH
            END FOREACH
         CLOSE cur_sc3

         CALL tipo_trabajador(l_reg_02.nss,
                              l_reg_02.tipo_solicitud)
              RETURNING v_nss,
                        v_tipo_trabajador

-----  MARCAJE DE CUENTA -----------------

   LET xrechazo = 0

   DECLARE cur_mar02 CURSOR FOR marcaje
   OPEN  cur_mar02 USING l_reg_02.nss,        #pnss
                       vmarca_ent,          #marca_entra
                       vcorrelativo,        #correlativo
                       vestado_marca,       #estado_marca
                       vcodigo_rechazo,     #codigo_rechazo
                       vmarca_causa,        #marca_causa
                       vfecha_causa,        #fecha_causa
                       v_usuario            #usuario

   FETCH cur_mar02 INTO xmarca,
                        xrechazo
   CLOSE cur_mar02

-----------------------

   IF xrechazo <> 0 THEN

      INSERT INTO cta_rch_saldo_cero -- Almacena cuentas que no pudo marcar
         VALUES(l_reg_02.nss           ,
                l_reg_02.tipo_solicitud,
                l_reg_02.curp          ,
                v_nss                  ,
                v_tipo_trabajador      ,
                v_tipo_cuenta          ,
                l_reg_02.fentcons      ,
                "1"                    ,
                v_fecha_conversion     ,
                dis_reg.tipo_movimiento,
                "",
                p_fecha_corte          ,
                xrechazo               ,
                hoy )
   ELSE
         INSERT INTO cta_saldo_cero
         VALUES(l_reg_02.nss           ,
                l_reg_02.tipo_solicitud,
                l_reg_02.curp          ,
                v_nss                  ,
                v_tipo_trabajador      ,
                v_tipo_cuenta          ,
                l_reg_02.fentcons      ,
                "1"                    ,
                v_fecha_conversion     ,
                dis_reg.tipo_movimiento,
                "",
                p_fecha_corte          ,
                hoy
                )
         UPDATE cta_ctr_cuenta
         SET    ind_saldo_cero   = 1,
                fecha_saldo_cero = v_fecha_conversion
         WHERE  nss              = l_reg_02.nss
      END IF
END IF

END FOREACH
CLOSE cur_tra02

   UPDATE STATISTICS FOR TABLE cta_saldo_cero

   CALL Actualiza_etapa(2)   --Termina proceso cuentas '02' saldo cero
   CALL ingresa_totales()

END FUNCTION

FUNCTION tipo_trabajador(xnss, xtipo_solicitud)
#tt--------------------------------------------

   DEFINE xnss              CHAR(11),
          xtipo_solicitud   SMALLINT,
          xtipo_trabajador  CHAR(2)

   DEFINE vnss              CHAR(11),
          vtipo_solicitud   SMALLINT

   SELECT nss,
          tipo_solicitud
   INTO   vnss,
          vtipo_solicitud
   FROM   safre_tmp:cuota_afore
   WHERE  nss = xnss

   IF vtipo_solicitud = 8 OR vtipo_solicitud = 12 THEN
      LET xtipo_trabajador = "02"
      LET xnss = ""
   ELSE
      IF vtipo_solicitud = 5 THEN
         LET xtipo_trabajador = "03"
         LET xnss = vnss
      ELSE
         LET xtipo_trabajador = "01"
         LET xnss = vnss
      END IF
   END IF

   RETURN xnss,
          xtipo_trabajador

END FUNCTION

FUNCTION genera_tmp_cuenta (p_nss  ,
                            p_fecha_ini ,
                            p_fecha_fin )

   DEFINE p_nss           CHAR(11),
          p_fecha_ini             ,
          p_fecha_fin     DATE

   DEFINE v_nombre_tabla  CHAR(20) ,
          v_anio                   ,
          v_anio_ini               ,
          v_anio_fin      SMALLINT

   DEFINE v_anio_c        CHAR(02) ,
          sel_his         CHAR(1500)

   DEFINE v_fecha         DATE

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta;
   WHENEVER ERROR STOP

   LET v_anio_ini = YEAR( p_fecha_ini )
   LET v_anio_fin = YEAR( p_fecha_fin )

   FOR v_anio = v_anio_ini TO v_anio_fin

      LET v_fecha  = MDY(1,1,v_anio)
      LET v_anio_c = v_fecha USING "YY"

      LET v_nombre_tabla = "dis_cuenta",v_anio_c CLIPPED

      SELECT "tabla"
      FROM   systables
      WHERE  tabname = v_nombre_tabla

      IF SQLCA.SQLCODE = 0 THEN

         LET sel_his = sel_his CLIPPED,
                     " SELECT fecha_conversion,tipo_movimiento ",
                     "  FROM ",v_nombre_tabla          ,
                     " WHERE nss = ","'",p_nss,"'"  ,
                     "  UNION ALL "
      END IF
   END FOR

   LET sel_his = sel_his CLIPPED,
              " SELECT fecha_conversion,tipo_movimiento ",
               "  FROM dis_cuenta ",
               " WHERE nss = ","'",p_nss,"'"  ,
               "   AND fecha_conversion <= ","'",p_fecha_fin,"'",
               " INTO TEMP tmp_dis_cuenta "

   PREPARE eje_prioridad FROM "SET PDQPRIORITY HIGH"
   EXECUTE eje_prioridad
   PREPARE eje_sel_his FROM sel_his
   EXECUTE eje_sel_his

   CREATE INDEX tmp_dis_cuenta1 on tmp_dis_cuenta ( tipo_movimiento )
   UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

   DELETE
   FROM  tmp_dis_cuenta
   WHERE tipo_movimiento = 999

   CREATE INDEX tmp_dis_cuenta2 on tmp_dis_cuenta ( fecha_conversion )
   UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

END FUNCTION

FUNCTION ingresa_totales()
#it-----------------------

   DEFINE xx_id_cuenta          CHAR(2),
          xx_id_trabajador      CHAR(2),
          x_registros           INTEGER

   DECLARE cur_tot CURSOR FOR
   SELECT COUNT(*),
          a.id_cuenta,
          a.tipo_trabajador
   FROM   cta_saldo_cero a
   WHERE  a.fecha_corte = p_fecha_corte
   GROUP BY 2,3
   ORDER BY 2

   FOREACH cur_tot INTO x_registros,
                        xx_id_cuenta,
                        xx_id_trabajador

      INSERT INTO cta_total_saldo_cero VALUES (p_fecha_corte     ,
                                               "1"             ,
                                               xx_id_cuenta    ,
                                               xx_id_trabajador,
                                               x_registros     )
   END FOREACH
   CLOSE cur_tot

END FUNCTION

FUNCTION Ingresa_etapa(vetapa_cod)
#ie-------------------------------

   DEFINE vetapa_cod         SMALLINT
   DEFINE fecha_inicial      DATETIME YEAR TO SECOND,
          fecha_finaliza     DATETIME YEAR TO SECOND

   LET fecha_inicial  = CURRENT
   LET fecha_finaliza = NULL

   INSERT INTO cta_ctr_saldo_cero
   VALUES (p_fecha_corte   ,
           vetapa_cod      ,
           fecha_inicial   ,
           fecha_finaliza  ,
           v_usuario        )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTAR EN TABLA cta_ctr_saldo_cero Etapa ",
            vetapa_cod," ",STATUS
      EXIT PROGRAM
   END IF

END FUNCTION

FUNCTION Actualiza_etapa(vetapa_cod)
#ae---------------------------------

   DEFINE vetapa_cod   SMALLINT

   DEFINE fecha_inicial      DATETIME YEAR TO SECOND,
          fecha_finaliza     DATETIME YEAR TO SECOND

   DEFINE cla_sel            CHAR(400)

   LET fecha_finaliza = CURRENT

   UPDATE cta_ctr_saldo_cero
      SET fecha_fin = fecha_finaliza
    WHERE fecha_corte = p_fecha_corte
      AND etapa = vetapa_cod

END FUNCTION

