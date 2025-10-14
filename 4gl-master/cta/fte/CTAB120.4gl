#############################################################################
#Proyecto            => SAFRE ( MEXICO )                                    #
#Propietario         => E.F.P.                                              #
#Programa            => CTAB120                                             #
#Descripcion         => GENERA REPORTE DE REGISTROS SALDO CERO RECURRENTE   #
#Por                 => OMAR SABNDOVAL BADILLO                              #
#Fecha               => 28 de noviembre de 2006                             #
#Fecha actualiza     => 21 DE JUNIO DE 2007                                 #
#Por                 => MAURO MUNIZ CABALLERO                               #
#    Nuevo marcaje identificacion cuentas saldo cero marca 151              #
#Fecha actualiza     => 21 DE JUNIO DE 2007                                 #
#Por                 => MAURO MUNIZ CABALLERO                               #
#    Nuevo marcaje identificacion cuentas saldo cero marca 151              #
#Por                 => 22 DE JUNIO de 2011  DMR                            #
#  Nueva identificacion de cuentas 04 independientes de habilitacion Normal #
#Sistema             => CTA                                                 #
#############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      opc                  CHAR(1),
      cve_ent_origen       CHAR(3),
      v_usuario            CHAR(8),
      sp_01                CHAR(50),
      vmarca               CHAR(200),
      query_01             CHAR(300),
      sel_his              CHAR(2500)

   DEFINE
      hoy                  DATE,
      fecha_inicio_sc      DATE,
      fecha_inicio_ant     DATE,
      p_fecha_corte        DATE,
      p_fecha_lote         DATE

   DEFINE
      paso                 SMALLINT,
      vmarca_ent           SMALLINT,
      xmarca               SMALLINT,
      xrechazo             SMALLINT,
      vcorrelativo         SMALLINT,
      vestado_marca        SMALLINT,
      vcodigo_rechazo      SMALLINT,
      vmarca_causa         SMALLINT,
      vfecha_causa         DATE

   DEFINE
      g_parametro RECORD LIKE seg_modulo.*,
      b_reg       RECORD LIKE cta_saldo_cero.*

   DEFINE
      l_reg_01    RECORD
                     nss              CHAR(11),
                     tipo_solicitud   SMALLINT,
                     curp             CHAR(18),
                     fentcons         DATE
                  END RECORD

   DEFINE
      l_reg_02    RECORD
                     nss              CHAR(11),
                     tipo_solicitud   SMALLINT,
                     curp             CHAR(18),
                     fentcons         DATE
                  END RECORD
END GLOBALS


MAIN
   CALL inicio()
   IF paso = 1 THEN
      CALL crea_tablas()
      CALL genera_cuota()
      CALL fn_ejecuta(p_fecha_corte,1)
   ELSE
      CALL proceso()
      CALL rep_nss_sdo()
   END IF
END MAIN


FUNCTION inicio()
#i---------------
   LET paso          = ARG_VAL(1)
   LET p_fecha_corte = ARG_VAL(2)
   LET p_fecha_lote  = ARG_VAL(3)

   LET hoy = TODAY

   SELECT *,
          USER
   INTO   g_parametro.*,
          v_usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"

   SELECT codigo_afore
   INTO   cve_ent_origen
   FROM   tab_afore_local

   LET sp_01 = "EXECUTE FUNCTION fn_fecha_ini_saldo_cero(?)"
   PREPARE proc_01 FROM sp_01

   LET vmarca = " EXECUTE PROCEDURE marca_cuenta (?,?,?,?,?,?,?,?)"

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_cuentas_cero
   WHENEVER ERROR STOP

   CREATE TEMP TABLE tmp_cuentas_cero (nss CHAR(11)) WITH NO LOG
END FUNCTION


FUNCTION crea_tablas()
#---------------------
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE
      DROP TABLE cuota_afore
      DROP TABLE tmp_sdo_saldo_cero
      DROP TABLE nss_inhab
   WHENEVER ERROR STOP

   CREATE TABLE cuota_afore
   (
     nss CHAR(11),
     tipo_solicitud SMALLINT,
     curp CHAR(18),
     fentcons DATE
    )

   CREATE TABLE tmp_sdo_saldo_cero
   (
     nss CHAR(11),
     subcuenta SMALLINT,
     siefore SMALLINT,
     fecha_conversion DATE,
     monto_en_acciones DECIMAL(22,6),
     monto_en_pesos DECIMAL(22,6)
    )

   CREATE TEMP TABLE nss_inhab (nss  CHAR(11)) WITH NO LOG
END FUNCTION


FUNCTION proceso()
#p----------------
   DEFINE
      v_tipo_cuenta         CHAR(2),
      v_tipo_trabajador     CHAR(2),
      v_nss                 CHAR(11),
      nss_sc                CHAR(11)

   DEFINE
      v_fecha_conversion    DATE,
      v_fecha_operacion     DATE,
      v_fecha_corte_ant     DATE

   DEFINE
      v_tipo_solicitud      SMALLINT,
      v_tipo_movimiento     SMALLINT

   DEFINE
      dis_reg RECORD
                 fecha_conversion   DATE,
                 tipo_movimiento    SMALLINT
              END RECORD

   EXECUTE proc_01 USING p_fecha_corte
                   INTO fecha_inicio_sc

   LET vmarca_ent    = 151
   LET vcorrelativo  = 0
   LET vmarca_causa  = 0
   LET vfecha_causa  = ''
   LET vestado_marca = 0

   LET v_fecha_corte_ant = MDY(MONTH(p_fecha_corte),1,YEAR(p_fecha_corte))
   LET v_fecha_corte_ant = v_fecha_corte_ant - 1 UNITS DAY

   #CALL llena_nss_pendiente()

   -------------------------INHABILITAR----------------------------
   ---para los tipos de cuenta 03 No ha recibido ninguna aportacion

   SELECT MAX(a.fecha_inicio)
   INTO   fecha_inicio_ant
   FROM   cta_cza_saldo_cero a
   WHERE  a.fecha_corte < p_fecha_corte

   DECLARE cur_tra01 CURSOR FOR
   SELECT c.nss,
          c.tipo_solicitud,
          c.curp,
          c.fentcons
   FROM   safre_tmp:cuota_afore c
   WHERE  c.nss NOT IN(SELECT s.nss
                            FROM   safre_tmp:tmp_sdo_saldo_cero s)
   {AND    c.nss NOT IN(SELECT n.nss
                            FROM   safre_tmp:nss_pendiente_liq n)}
   AND      c.fentcons <= fecha_inicio_sc
   ORDER BY 1,2


   LET v_tipo_cuenta = "03"

   FOREACH cur_tra01 INTO l_reg_01.*
      INSERT INTO tmp_cuentas_cero VALUES(l_reg_01.nss)

      CALL tipo_trabajador(l_reg_01.nss,
                           l_reg_01.tipo_solicitud)
           RETURNING v_nss, v_tipo_trabajador

      -----  MARCAJE DE CUENTA -----

      LET xrechazo = 0

      CALL marca_cuenta (l_reg_01.nss,    
                         vmarca_ent,      
                         vestado_marca,   
                         vcodigo_rechazo, 
                         v_usuario,       
                         vcorrelativo) 

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
                l_reg_01.fentcons      ,
                ""                     ,
                ""                     ,
                p_fecha_corte          ,
                xrechazo               ,
                hoy)
      ELSE
         LET dis_reg.fecha_conversion = NULL
         LET v_fecha_operacion = NULL
         LET dis_reg.tipo_movimiento = 0
         LET v_tipo_cuenta  = "03"

         CALL genera_tmp_cuenta (l_reg_01.nss     ,
                                 l_reg_01.fentcons,
                                 p_fecha_corte)

         DECLARE cur_sc3x CURSOR FOR
         SELECT fecha_conversion,
                tipo_movimiento
         FROM   tmp_dis_cuenta
         WHERE  fecha_conversion <= fecha_inicio_sc
         AND    tipo_movimiento NOT BETWEEN 100 AND 114
         AND tipo_movimiento NOT BETWEEN 900 AND 999
         ORDER BY 2 DESC

         FOREACH cur_sc3x INTO dis_reg.*
            EXIT FOREACH
         END FOREACH

         CLOSE cur_sc3x

         IF dis_reg.fecha_conversion IS NOT NULL AND
            dis_reg.fecha_conversion <> "12311899" THEN
            LET v_tipo_cuenta = "03"
            LET v_fecha_operacion = dis_reg.fecha_conversion
         ELSE
            LET v_tipo_cuenta = "03"
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
                v_fecha_operacion      ,
                dis_reg.tipo_movimiento,
                ""                     ,
                p_fecha_corte          ,
                hoy)

         UPDATE cta_ctr_cuenta
         SET    ind_saldo_cero   = 1,
                fecha_saldo_cero = v_fecha_operacion
         WHERE  nss              = l_reg_01.nss
      END IF
   END FOREACH

   CLOSE cur_tra01

   --CALL Actualiza_etapa(2) --Termina proceso cuentas '01' saldo cero
   --CALL Ingresa_etapa(3)   --Inicia proceso de cuentas '02' saldo cero

   --para los tipos de cuenta 03 en algun momento tuvo recursos 

   INSERT INTO tmp_cuentas_cero
   SELECT nss
   FROM   safre_tmp:tmp_sdo_saldo_cero
   WHERE  subcuenta <> 19
   AND    monto_en_acciones > 0
   GROUP BY 1

   INSERT INTO tmp_cuentas_cero
   SELECT nss
   FROM   safre_tmp:tmp_sdo_saldo_cero
   WHERE  subcuenta = 19
   AND    monto_en_pesos > 0
   GROUP BY 1

   CREATE INDEX tmp_ctas_cero1 ON tmp_cuentas_cero ( nss )
   UPDATE STATISTICS FOR TABLE tmp_cuentas_cero

   DECLARE cur_tra02 CURSOR FOR
   SELECT m.nss,
          m.tipo_solicitud,
          m.curp,
          m.fentcons
   FROM   safre_tmp:cuota_afore m
   WHERE  m.nss NOT IN(SELECT c.nss
                            FROM   tmp_cuentas_cero c)
   {AND    m.nss NOT IN(SELECT n.nss
                            FROM   safre_tmp:nss_pendiente_liq n)}
   --AND    m.fentcons >  fecha_inicio_ant
   AND    m.fentcons <= fecha_inicio_sc
   ORDER BY 1

   LET v_tipo_cuenta = "03"

   FOREACH cur_tra02 INTO l_reg_02.*
      CALL genera_tmp_cuenta (l_reg_02.nss     ,
                              l_reg_02.fentcons,
                              p_fecha_corte)

      LET v_fecha_conversion = l_reg_02.fentcons
      LET v_tipo_movimiento  = 0

      SELECT "X"
      FROM   tmp_dis_cuenta
      WHERE  fecha_conversion >  fecha_inicio_sc
      AND    fecha_conversion <= p_fecha_corte
      AND    tipo_movimiento NOT BETWEEN 100 AND 110
      AND tipo_movimiento NOT BETWEEN 900 AND 999
      GROUP BY 1

      IF SQLCA.SQLCODE = NOTFOUND THEN
         DECLARE cur_02 CURSOR FOR
         SELECT fecha_conversion,
                tipo_movimiento
         FROM   tmp_dis_cuenta
         WHERE  fecha_conversion <= fecha_inicio_sc
         AND    tipo_movimiento NOT BETWEEN 100 AND 110
         AND tipo_movimiento NOT BETWEEN 900 AND 999
         ORDER BY 1 DESC

         FOREACH cur_02 INTO v_fecha_conversion, v_tipo_movimiento
            EXIT FOREACH
         END FOREACH

         IF v_tipo_movimiento IS NOT NULL AND
            v_tipo_movimiento <> 0 THEN

            CALL tipo_trabajador(l_reg_02.nss, l_reg_02.tipo_solicitud)
            RETURNING v_nss, v_tipo_trabajador

            -----  MARCAJE DE CUENTA -----

            LET xrechazo = 0

            CALL marca_cuenta (l_reg_02.nss,
                               vmarca_ent,
                               vestado_marca,
                               vcodigo_rechazo,
                               v_usuario,
                               vcorrelativo)

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
                      ""                     ,
                      p_fecha_corte          ,
                      xrechazo               ,
                      hoy)
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
                      v_tipo_movimiento      ,
                      ""                     ,
                      p_fecha_corte          ,
                      hoy)

               UPDATE cta_ctr_cuenta
               SET    ind_saldo_cero   = 1,
                      fecha_saldo_cero = v_fecha_conversion
               WHERE  nss              = l_reg_02.nss
            END IF -- DEL RECHAZO
         END IF -- DEL ULTIMO  MOVIMIENTO
      END IF -- DE LOS MOVIMIENTOS  QUE NO TUVO EN 7 BIMESTRES
   END FOREACH

   CLOSE cur_tra02

   -----------------------RECHAZOS--------------------------

   UPDATE STATISTICS FOR TABLE cta_saldo_cero
   CALL Actualiza_etapa(2) --Termina proceso cuentas '03' saldo cero

   -----------------------HABILITAR-------------------------
   CALL Ingresa_etapa(3)   --Inicia proceso de cuentas '04' saldo cero
   CALL proceso_ctas04()
   CALL Actualiza_etapa(3) --Termina proceso cuentas '04' saldo cero
   CALL ingresa_totales()
END FUNCTION


FUNCTION proceso_ctas04()
   DEFINE 
      v_tipo_cuenta      CHAR(2),
      v_tipo_trabajador  CHAR(2),
      v_nss              CHAR(11),
      nss_sc             CHAR(11)

   DEFINE 
      v_fecha_conversion DATE,
      v_fecha_operacion  DATE,
      v_fecha_corte_ant  DATE

   DEFINE 
      v_tipo_solicitud   SMALLINT,
      v_tipo_movimiento  SMALLINT

   DEFINE 
      cfecha_inicio      DATE

   DEFINE
      saldo_acc          DEC(18,6),
      saldo_pes          DEC(16,2)

   DEFINE
      l_reg_04   RECORD
                    nss              CHAR(11),
                    tipo_solicitud   SMALLINT,
                    curp             CHAR(18),
                    fentcons         DATE
                 END RECORD

   DEFINE 
      dis_reg    RECORD
                    fecha_conversion DATE,
                    tipo_movimiento  SMALLINT
                 END RECORD

   LET cfecha_inicio = MDY(MONTH(p_fecha_corte),1,YEAR(p_fecha_corte))

   DECLARE cur_04 CURSOR FOR
   SELECT m.nss,
          m.tipo_solicitud,
          m.curp,
          m.fentcons
   FROM   safre_tmp:cuota_afore m,
          cta_act_marca c
   WHERE  m.nss       = c.nss
   AND    c.marca_cod = 160           #INHABILITADO SALDOS CERO

   LET v_tipo_cuenta = "04"

   FOREACH cur_04 INTO l_reg_04.*
      LET v_fecha_conversion = l_reg_04.fentcons
      LET v_tipo_movimiento  = 0

{     DECLARE cur_041 CURSOR FOR
      SELECT fecha_conversion,
             tipo_movimiento
      FROM   tmp_dis_cuenta
      WHERE  fecha_conversion <= fecha_inicio_sc
      AND    tipo_movimiento NOT BETWEEN 100 AND 114
      AND    tipo_movimiento NOT BETWEEN 900 AND 999
      ORDER BY 2 DESC

      FOREACH cur_041 INTO dis_reg.*
         EXIT FOREACH
      END FOREACH

      CLOSE cur_041

      IF dis_reg.fecha_conversion IS NOT NULL   AND
         dis_reg.fecha_conversion <> "12311899" THEN
         LET v_fecha_operacion     = dis_reg.fecha_conversion
      ELSE
         LET dis_reg.tipo_movimiento = 0
         LET v_fecha_operacion       = l_reg_01.fentcons
      END  IF   }

      SELECT 'X'
      FROM   cta_saldo_cero
      WHERE  nss         = l_reg_04.nss
      AND    fecha_corte = p_fecha_corte
      AND    id_cuenta   = '03'
      GROUP BY 1

      IF STATUS = NOTFOUND THEN
         CALL tipo_trabajador(l_reg_04.nss, l_reg_04.tipo_solicitud)
         RETURNING v_nss, v_tipo_trabajador

         LET saldo_acc  = 0
         LET saldo_pes  = 0

         SELECT NVL(sum(monto_en_acciones),0)
         INTO   saldo_acc
         FROM   dis_cuenta
         WHERE  nss = l_reg_04.nss
         AND    subcuenta NOT IN (19)
         AND    tipo_movimiento <> 0 

         IF saldo_acc < 0 THEN
            LET saldo_acc = 0
         END IF

         SELECT NVL(sum(monto_en_pesos),0)
         INTO   saldo_pes
         FROM   dis_cuenta
         WHERE  nss = l_reg_04.nss
         AND    subcuenta IN (19)
         AND    tipo_movimiento <> 0 

         IF saldo_pes < 0 THEN
            LET saldo_pes = 0
         END IF

         IF saldo_acc > 0 OR saldo_pes > 0 THEN
            DECLARE cur_041 CURSOR FOR
            SELECT fecha_conversion, tipo_movimiento
            FROM   dis_cuenta
	    WHERE  nss = l_reg_04.nss
            AND    tipo_movimiento NOT BETWEEN 100 AND 114
            AND    tipo_movimiento NOT BETWEEN 900 AND 999
            ORDER BY 2 DESC

            FOREACH cur_041 INTO dis_reg.*
               EXIT FOREACH
            END FOREACH

            CLOSE cur_041

            IF dis_reg.fecha_conversion IS NOT NULL   AND
               dis_reg.fecha_conversion <> "12311899" THEN
               LET v_fecha_operacion     = dis_reg.fecha_conversion
            ELSE
               LET dis_reg.tipo_movimiento = 0
               LET v_fecha_operacion       = l_reg_01.fentcons
            END  IF
 
            INSERT INTO cta_saldo_cero
            VALUES (l_reg_04.nss           ,
                    l_reg_04.tipo_solicitud,
                    l_reg_04.curp          ,
                    v_nss                  ,
                    v_tipo_trabajador      ,
                    v_tipo_cuenta          ,
                    l_reg_04.fentcons      ,
                    "1"                    ,
                    v_fecha_operacion      ,
                    dis_reg.tipo_movimiento,
                    ""                     ,
                    p_fecha_corte          ,
                    hoy)
         END IF
      END IF
   END FOREACH

   CLOSE cur_04
END FUNCTION


{FUNCTION llena_nss_pendiente()
   DEFINE
      vfolio_dis        INTEGER
   DEFINE
      vfecha_liq        DATE
   DEFINE
      vfecha_lim        DATE

   DATABASE safre_tmp
   WHENEVER ERROR CONTINUE
      DROP TABLE nss_pendiente_liq
   WHENEVER ERROR STOP

   CREATE TABLE nss_pendiente_liq
   (
     nss  CHAR(11)
   )

   LET vfecha_lim = MDY(MONTH(p_fecha_corte), 1, YEAR(p_fecha_corte))
   LET vfecha_lim = vfecha_lim - (16 UNITS MONTH)

   -----PENDIENTE DE LIQUIDAR EN DISPERSION--------

   DECLARE cur_pend_disp CURSOR FOR
   SELECT folio, fech_liquidacion
   FROM  safre_af:dis_dep_aporte
   WHERE estado            =  2
   AND   fech_liquidacion >= p_fecha_corte

   FOREACH cur_pend_disp INTO vfolio_dis, vfecha_liq
      INSERT INTO nss_pendiente_liq
      SELECT unique a.nss
      FROM  safre_af:dis_provision a
      WHERE a.folio = vfolio_dis
   END FOREACH

   -----PENDIENTE DE LIQUIDAR EN RECAUDACION ISSSTE--------
   DECLARE cur_pend_isss CURSOR FOR
   SELECT folio, fech_liquidacion
   FROM  safre_af:dis_dep_apo_isss
   WHERE estado            =  2
   AND   fech_liquidacion >= p_fecha_corte

   FOREACH cur_pend_isss INTO vfolio_dis, vfecha_liq
      INSERT INTO nss_pendiente_liq
      SELECT unique a.nss
      FROM  safre_af:dis_provision a
      WHERE a.folio = vfolio_dis
   END FOREACH

   -----PENDIENTE DE LIQUIDAR EN CREDITOS EN GARANTIA-----

   INSERT INTO nss_pendiente_liq
   SELECT unique a.n_seguro
   FROM  safre_af:acr_det_dev_cred a
   WHERE a.fecha_mov_banxico >= p_fecha_corte

   -----PENDIENTE DE LIQUIDAR ACREDITADOS------

   INSERT INTO nss_pendiente_liq
   SELECT unique a.n_seguro
   FROM  safre_af:acr_det_devuelto a
   WHERE a.fecha_mov_banxico >= p_fecha_corte

   -----PENDIENTE DE LIQUIDAR ACREDITADOS FOVISSSTE------

   INSERT INTO nss_pendiente_liq
   SELECT unique a.nss_fovissste
   FROM  safre_af:acr_det_dev_issste a
   WHERE a.fecha_movimiento >= p_fecha_corte

   ----CUENTA CON UNA MARCA DENTRO DE LOS ULTIMOS 8 BIMESTRES-----

   INSERT INTO nss_pendiente_liq
   SELECT UNIQUE nss
   FROM safre_af:cta_his_marca
   WHERE fecha_ini >= vfecha_lim
   --AND marca_cod NOT IN (140)

   ----CUENTA CON INHABILITACION POR RETIRO

   INSERT INTO nss_pendiente_liq
   SELECT UNIQUE nss
   FROM safre_af:cta_act_marca
   WHERE marca_cod = 140

   ----CUENTA EN CON ACLARACION EN UNIDAD SERVICIOS----
   ---INSERT INTO safre_tmp:nss_pendiente_liq
   ---SELECT UNIQUE n_seguro
   ---FROM rec_solicitud
   ---WHERE freclamo >= vfecha_lim

   ----PENDIENTE LIQUIDACION INTERESES EN TRANSITO----
   INSERT INTO nss_pendiente_liq
   SELECT UNIQUE nss
   FROM safre_af:cta_interes_rcv
   WHERE fecha_conversion >= p_fecha_corte

   CREATE INDEX nss_pen_liq1 on nss_pendiente_liq (nss);
   UPDATE STATISTICS FOR TABLE nss_pendiente_liq

   DATABASE safre_af
END FUNCTION}


FUNCTION tipo_trabajador(xnss,xtipo_solicitud)
#tt-------------------------------------------
   DEFINE
      xnss              CHAR(11),
      xtipo_solicitud   SMALLINT,
      xtipo_trabajador  CHAR(2)

   DEFINE
      vnss              CHAR(11),
      vtipo_solicitud   SMALLINT

   SELECT n_seguro,
          tipo_solicitud
   INTO   vnss,
          vtipo_solicitud
   FROM   afi_mae_afiliado
   WHERE  n_seguro = xnss

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

   RETURN xnss, xtipo_trabajador
END FUNCTION


FUNCTION genera_tmp_cuenta (p_nss, p_fecha_ini, p_fecha_fin)
#gtm--------------------------------------------------------
   DEFINE
      p_nss           CHAR(11),
      p_fecha_ini     DATE    ,
      p_fecha_fin     DATE

   DEFINE
      v_nombre_tabla  CHAR(20),
      v_anio          SMALLINT,
      v_anio_ini      SMALLINT,
      v_anio_fin      SMALLINT

   DEFINE
      v_anio_c        CHAR(02)

   DEFINE
      v_fecha         DATE

    WHENEVER ERROR CONTINUE
       DROP TABLE tmp_dis_cuenta
    WHENEVER ERROR STOP

   LET sel_his = ""

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
                     " FROM  ",v_nombre_tabla ,
                     " WHERE nss = ","'",p_nss,"'" ,
                     " UNION ALL "
      END IF
   END FOR

   LET sel_his = sel_his CLIPPED,
               " SELECT fecha_conversion,tipo_movimiento ",
               " FROM dis_cuenta ",
               " WHERE nss = ","'",p_nss,"'"  ,
               " AND fecha_conversion <= ","'",p_fecha_fin,"'",
               " INTO TEMP tmp_dis_cuenta "

   PREPARE eje_sel_his FROM sel_his
   EXECUTE eje_sel_his

   CREATE INDEX tmp_dis_cuenta2 ON tmp_dis_cuenta ( fecha_conversion,tipo_movimiento )
   UPDATE STATISTICS FOR TABLE tmp_dis_cuenta
END FUNCTION


FUNCTION ingresa_totales()
#it-----------------------
   DEFINE
      xx_id_cuenta       CHAR(2),
      xx_id_trabajador   CHAR(2),
      x_operacion        CHAR(1),
      x_registros        INTEGER 

   DECLARE cur_tot CURSOR FOR
   SELECT a.operacion,
          a.id_cuenta,
          a.tipo_trabajador,
          COUNT(*)
   FROM   cta_saldo_cero a
   WHERE  a.fecha_corte = p_fecha_corte
   GROUP BY 1,2,3
   ORDER BY 1,2,3

   FOREACH cur_tot INTO x_operacion ,
                        xx_id_cuenta,
                        xx_id_trabajador,
                        x_registros

      INSERT INTO cta_total_saldo_cero
      VALUES (p_fecha_corte   ,
              x_operacion     ,
              xx_id_cuenta    ,
              xx_id_trabajador,
              x_registros     )
   END FOREACH
   CLOSE cur_tot
END FUNCTION


FUNCTION Ingresa_etapa(vetapa_cod)
#ie-------------------------------
   DEFINE
      vetapa_cod         SMALLINT

   DEFINE
      fecha_inicial      DATETIME YEAR TO SECOND,
      fecha_finaliza     DATETIME YEAR TO SECOND

   LET fecha_inicial  = CURRENT
   LET fecha_finaliza = NULL

   INSERT INTO cta_ctr_saldo_cero
   VALUES (p_fecha_corte  ,
           vetapa_cod     ,
           fecha_inicial  ,
           fecha_finaliza ,
           v_usuario)

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTAR EN TABLA cta_ctr_saldo_cero Etapa ",
            vetapa_cod," ",STATUS
      EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION Actualiza_etapa(vetapa_cod)
#ae---------------------------------
   DEFINE
      vetapa_cod      SMALLINT

   DEFINE
      fecha_inicial   DATETIME YEAR TO SECOND,
      fecha_finaliza  DATETIME YEAR TO SECOND

   DEFINE
      cla_sel         CHAR(400)

   LET fecha_finaliza = CURRENT

   UPDATE cta_ctr_saldo_cero
   SET   fecha_fin = fecha_finaliza
   WHERE fecha_corte = p_fecha_corte
   AND   etapa = vetapa_cod
END FUNCTION


FUNCTION genera_cuota()
#gc--------------------
   INSERT INTO nss_inhab
   SELECT t.n_seguro
   FROM  safre_af:taa_cd_det_cedido t
   WHERE t.estado IN (103,12)
   AND   t.fecha_trasp <= p_fecha_corte

   INSERT INTO nss_inhab
   SELECT t.nss_cta1
   FROM  safre_af:uni_unificado t
   WHERE t.estado IN (60,100)
   AND   t.fliquida <= p_fecha_corte 

   INSERT INTO nss_inhab
   SELECT c.nss
   FROM  safre_af:cta_act_marca c
   WHERE c.marca_cod = 150

   CREATE INDEX nss_inhab1 on nss_inhab (nss)
   UPDATE STATISTICS FOR TABLE nss_inhab

   INSERT INTO cuota_afore
   SELECT a.n_seguro, a.tipo_solicitud, a.n_unico, a.fentcons
   FROM safre_af:afi_mae_afiliado a
   WHERE a.n_seguro NOT IN(SELECT nss
                           FROM nss_inhab )
   AND a.fentcons <= p_fecha_corte
   AND a.finicta  <= p_fecha_corte

   INSERT INTO cuota_afore
   SELECT b.n_seguro,
          b.tipo_solicitud,
          a.n_unico,
          a.fentcons
   FROM   safre_af:afi_mae_afiliado a ,
          safre_af:afi_his_afiliado b
   WHERE  a.n_seguro = b.n_seguro
   AND    b.n_seguro NOT IN(SELECT nss
                            FROM nss_inhab)
   AND    a.fentcons >  p_fecha_corte
   AND    b.fentcons <= p_fecha_corte
   AND    b.tipo_solicitud = 5
   AND    a.tipo_solicitud <> 2

   CREATE INDEX cuota_afo1 on cuota_afore (nss)
   CREATE INDEX cuota_afo2 on cuota_afore (fentcons)

   UPDATE STATISTICS FOR TABLE cuota_afore;
END FUNCTION


FUNCTION fn_ejecuta( p_fecha_corte, p_tipo )
#fe-----------------------------------------
   DEFINE
      p_fecha_corte  DATE,
      p_tipo         SMALLINT,
      v_comando      CHAR(200)

   LET v_comando = "nohup time ./eje_sdo_saldo_cero.sh ",p_fecha_corte," ",
                   v_usuario CLIPPED," 1> ",v_usuario CLIPPED,".salida ",
                   "2> ",v_usuario CLIPPED,".error &"

   RUN v_comando
END FUNCTION


FUNCTION marca_cuenta(vnss, vmarca_entra, vmarca_edo, vcodigo_rech,
                      vusuario, vcorrelativo)
#mc---------------------
   DEFINE
      vnss           CHAR(11),
      vmarca_entra   SMALLINT,
      vmarca_edo     SMALLINT,
      vcodigo_rech   SMALLINT,
      vusuario       CHAR(08),
      vcorrelativo   INTEGER,
      vmarca_causa   SMALLINT,
      vfecha_causa   DATE

   LET vmarca_causa = 0
   LET vfecha_causa = ""

   PREPARE marcaje FROM vmarca
   DECLARE cur_mar01 CURSOR FOR marcaje  

   OPEN  cur_mar01
   USING vnss        ,
         vmarca_entra,
         vcorrelativo,
         vmarca_edo  ,
         vcodigo_rech,
         vmarca_causa,
         vfecha_causa,
         vusuario

   FETCH cur_mar01
   INTO  xmarca, xrechazo

   CLOSE cur_mar01
   FREE  cur_mar01
END FUNCTION


FUNCTION rep_nss_sdo()
   DEFINE
      hora        CHAR(08),
      G_LISTA     CHAR(100),
      ejecuta     CHAR(100)

   LET hora = TIME

   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp
      DROP TABLE nss_borra
   WHENEVER ERROR STOP

   CREATE table nss_borra
   (
    nss         CHAR(11),
    fecha_corte DATE
   )

   DATABASE safre_af

   LET G_LISTA = g_parametro.ruta_listados CLIPPED,"/","NSS_SDO_POST.",
                 p_fecha_corte USING "DDMMYYYY" CLIPPED,".",hora

   START REPORT listado_ctas TO G_LISTA

      CALL genera()

   LET ejecuta = "chmod 777 ", G_LISTA CLIPPED
   RUN ejecuta
END FUNCTION


FUNCTION genera()
   DEFINE
      nssx         CHAR(11),
      n_unicox     CHAR(18),
      nombre       CHAR(50),
      saldo_acc    DEC(18,6),
      saldo_pes    DEC(16,2),
      num_reg      INTEGER,
      t_monto_acc  DECIMAL(18,6),
      t_monto_pes  DECIMAL(16,2)

   DEFINE
      cad_ano      CHAR(12),
      ano_ant      SMALLINT,
      cadena       CHAR(400),
      caden2       CHAR(400),
      fcorte       DATE,
      fecha_atras  DATE,
      saldo_atras  LIKE dis_cuenta.monto_en_acciones,
      saldo_atras2 LIKE dis_cuenta.monto_en_acciones,
      sdo_atras_p  LIKE dis_cuenta.monto_en_acciones,
      sdo_atras2_p LIKE dis_cuenta.monto_en_acciones

   DEFINE
      reg_nom      RECORD
                      nombre   CHAR(40),
                      paterno  CHAR(40),
                      materno  CHAR(40)
                   END RECORD

   LET t_monto_acc = 0
   LET t_monto_pes = 0

   DECLARE apt_ctas_1 CURSOR FOR
   SELECT nss FROM cta_saldo_cero
   WHERE fecha_corte = p_fecha_corte
   AND   id_cuenta   = "03"

   FOREACH apt_ctas_1 INTO nssx
      LET saldo_acc  = 0
      LET saldo_pes  = 0

      SELECT NVL(sum(monto_en_acciones),0)
      INTO   saldo_acc
      FROM   dis_cuenta
      WHERE  nss = nssx
      AND    subcuenta NOT IN (19)
      AND    tipo_movimiento <> 0

      IF saldo_acc < 0 THEN
         LET saldo_acc = 0
      END IF

      SELECT NVL(sum(monto_en_pesos),0)
      INTO   saldo_pes
      FROM   dis_cuenta
      WHERE  nss = nssx
      AND    subcuenta IN (19)
      AND    tipo_movimiento <> 0

      IF saldo_pes < 0 THEN
         LET saldo_pes = 0
      END IF

      IF saldo_acc > 0 OR saldo_pes > 0 THEN
         SELECT b.nombres, b.paterno, b.materno, b.n_unico
         INTO  reg_nom.*, n_unicox
         FROM  afi_mae_afiliado b
         WHERE b.n_seguro = nssx

         LET t_monto_acc = t_monto_acc + saldo_acc

         LET t_monto_pes = t_monto_pes + saldo_pes

         LET nombre = reg_nom.nombre  CLIPPED," ",
                      reg_nom.materno CLIPPED," ",
                      reg_nom.paterno CLIPPED

         LET num_reg = num_reg + 1

         INSERT INTO safre_tmp:nss_borra
         VALUES(nssx,p_fecha_corte)

         OUTPUT TO REPORT listado_ctas(nssx,
                                       n_unicox,
                                       saldo_acc,
                                       saldo_pes,
                                       nombre,
                                       num_reg,
                                       t_monto_acc,
                                       t_monto_pes
                                       )

         INITIALIZE nombre, reg_nom.* TO NULL
      ELSE
         LET ano_ant = YEAR(TODAY) - 1
         LET ano_ant = ano_ant - 2000

         LET fcorte      = p_fecha_corte

         PREPARE eje_inic FROM
         "EXECUTE FUNCTION fn_fecha_ini_saldo_cero(?)"
         EXECUTE eje_inic USING fcorte
                          INTO  fecha_atras

         LET saldo_atras = 0

         SELECT NVL(sum(monto_en_acciones),0)
         INTO   saldo_atras
         FROM   dis_cuenta
         WHERE  nss = nssx
         AND    subcuenta NOT IN (19)
         AND    tipo_movimiento = 1
         AND    fecha_conversion between fecha_atras and fcorte

         LET sdo_atras_p = 0

         SELECT NVL(sum(monto_en_pesos),0)
         INTO   sdo_atras_p
         FROM   dis_cuenta
         WHERE  nss = nssx
         AND    subcuenta IN (19)
         AND    tipo_movimiento = 1
         AND    fecha_conversion between fecha_atras and fcorte

         LET saldo_atras2 = 0
         LET sdo_atras2_p = 0

         LET cad_ano = "dis_cuenta",ano_ant USING "&&"

         SELECT "OK"
         FROM  systables
         WHERE tabname = cad_ano

         IF STATUS <> NOTFOUND THEN
            LET cadena = " SELECT NVL(sum(monto_en_acciones),0) ",
                         " FROM  ",cad_ano,
                         " WHERE  nss = '",nssx,"'",
                         " AND    subcuenta NOT IN (19) ",
                         " AND    tipo_movimiento = 1 ",
                         " AND    fecha_conversion between '",fecha_atras,"'",
                               " and '",fcorte,"'" 
 
            PREPARE eje_cad FROM cadena
            EXECUTE eje_cad INTO saldo_atras2

            LET caden2 = " SELECT NVL(sum(monto_en_pesos),0) ",
                         " FROM  ",cad_ano,
                         " WHERE  nss = '",nssx,"'",
                         " AND    subcuenta IN (19) ",
                         " AND    tipo_movimiento = 1 ",
                         " AND    fecha_conversion between '",fecha_atras,"'",
                               " and '",fcorte,"'"

            PREPARE eje_cad2 FROM caden2
            EXECUTE eje_cad2 INTO sdo_atras2_p
         ELSE
            LET saldo_atras2 = 0
            LET sdo_atras2_p = 0
         END IF

         IF saldo_atras  > 0 OR saldo_atras2 > 0 OR sdo_atras_p > 0 OR
            sdo_atras2_p > 0
         THEN
            SELECT b.nombres, b.paterno, b.materno, b.n_unico
            INTO  reg_nom.*, n_unicox
            FROM  afi_mae_afiliado b
            WHERE b.n_seguro = nssx

            LET saldo_acc = saldo_atras + saldo_atras2
            LET t_monto_acc = t_monto_acc + (saldo_atras + saldo_atras2)

            LET saldo_pes = sdo_atras_p + sdo_atras2_p
            LET t_monto_pes = t_monto_pes + (sdo_atras_p + sdo_atras2_p)

            LET nombre = reg_nom.nombre  CLIPPED," ",
                         reg_nom.materno CLIPPED," ",
                         reg_nom.paterno CLIPPED

            LET num_reg = num_reg + 1

            INSERT INTO safre_tmp:nss_borra
            VALUES(nssx,p_fecha_corte)

            OUTPUT TO REPORT listado_ctas(nssx,
                                       n_unicox,
                                       saldo_acc,
                                       saldo_pes,
                                       nombre,
                                       num_reg,
                                       t_monto_acc,
                                       t_monto_pes
                                       )

            INITIALIZE nombre, reg_nom.* TO NULL
         END IF
      END IF
   END FOREACH
   FINISH REPORT listado_ctas
END FUNCTION


################################################################################
REPORT listado_ctas(nssr, n_unicor, saldo_accr, saldo_pesr, nombre, num_reg,
                    t_monto_accr, t_monto_pesr)
#cl-------------

   DEFINE
      nssr               CHAR(11),
      n_unicor           CHAR(18),
      saldo_accr         DEC(18,6),
      saldo_pesr         DEC(16,2),
      nombre             CHAR(40),
      num_reg            INTEGER,
      t_monto_accr       DECIMAL(18,6),
      t_monto_pesr       DECIMAL(16,2),
      L1                 CHAR(01),
      L2                 CHAR(02),
      L3                 CHAR(03),
      L4                 CHAR(04),
      L5                 CHAR(05),
      L6                 CHAR(06),
      L7                 CHAR(07),
      L8                 CHAR(08),
      L9                 CHAR(09),
      L10                CHAR(10),
      L11                CHAR(11)

   OUTPUT
      LEFT MARGIN    0
      RIGHT MARGIN   0
      TOP MARGIN     0
      BOTTOM MARGIN  0
      PAGE LENGTH   60

   FORMAT
      PAGE HEADER
         LET L1  = "\304"
         LET L2  = "\304\304"
         LET L3  = "\304\304\304"
         LET L4  = "\304\304\304\304"
         LET L5  = "\304\304\304\304\304"
         LET L6  = "\304\304\304\304\304\304"
         LET L7  = "\304\304\304\304\304\304\304"
         LET L8  = "\304\304\304\304\304\304\304\304"
         LET L9  = "\304\304\304\304\304\304\304\304\304"
         LET L10 = "\304\304\304\304\304\304\304\304\304\304"
         LET L11 = "\304\304\304\304\304\304\304\304\304\304\304"

         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'

         PRINT COLUMN 37, " REPORTE DE CUENTAS CON SALDO POSTERIOR A FECHA CORTE",
               COLUMN 113,"pagina : ",PAGENO USING "###"

         PRINT
         PRINT COLUMN 02,"FECHA PROCESO : ", hoy USING "DD/MM/YYYY",
               COLUMN 117,"CTAM005"
         PRINT
         PRINT '\033e\033(s218T\033(s17H\033(s7B'
         PRINT
               COLUMN 01,"\332",L10,L1,
                         "\302",L10,L4,L4,
                         "\302",L10,L10,L10,L10,
                         "\302",L10,L4,L1,L1,L1,
                         "\302",L10,L4,L1,L1,L1,
                         "\302",L10,L4,L1,L1,L1,
                         "\277"

         PRINT COLUMN 01, "|",
               COLUMN 02, "   N S S   ",
               COLUMN 13, "|",
               COLUMN 15, "     C U R P      ",
               COLUMN 33, "|",
               COLUMN 35, "N O M B R E",
               COLUMN 75, "|",
               COLUMN 76, "  SALDO ACCIONES ",
               COLUMN 93, "|",
               COLUMN 94, "  SALDO  PESOS   ",
               COLUMN 111,"|",
               COLUMN 112,"FECHA CORTE    ",
               COLUMN 127,"|"

         PRINT
             COLUMN 01,"\300",L10,L1,
                       "\301",L10,L4,L4,
                       "\301",L10,L10,L10,L10,
                       "\301",L10,L4,L1,L1,L1,
                       "\301",L10,L4,L1,L1,L1,
                       "\301",L10,L4,L1,L1,L1,
                       "\331"

    ON EVERY ROW
      PRINT COLUMN 02,nssr,
            COLUMN 15,n_unicor,
            COLUMN 35,nombre,
            COLUMN 76,saldo_accr USING "##########&.&&&&&&",
            COLUMN 94,saldo_pesr USING "############&.&&",
            COLUMN 115,p_fecha_corte

    ON LAST ROW
       SKIP 2 LINES
       PRINT
             COLUMN 01,"\332",L10,L1,
                       "\302",L10,L4,L4,
                       "\302",L10,L10,L10,L10,
                       "\302",L10,L4,L1,L1,L1,
                       "\302",L10,L4,L1,L1,L1,
                       "\302",L10,L4,L1,L1,L1,
                       "\277"

      PRINT COLUMN 01,"|",
            COLUMN 02,num_reg  USING "#######&",
            COLUMN 13,"|",
            COLUMN 15,"T O T A L E S : ",
            COLUMN 75,"|",
            COLUMN 76, t_monto_accr  USING "##########&.&&&&&&",
            COLUMN 93,"|",
            COLUMN 94, t_monto_pesr  USING "###########&.&&",
            COLUMN 111,"|",
            COLUMN 127,"|"

      PRINT
            COLUMN 01,"\300",L10,L1,
                      "\301",L10,L4,L4,
                      "\301",L10,L10,L10,L10,
                      "\301",L10,L4,L1,L1,L1,
                      "\301",L10,L4,L1,L1,L1,
                      "\301",L10,L4,L1,L1,L1,
                      "\331"
END REPORT
 
