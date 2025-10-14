################################################################################
#Project           => SAFRE (Mexico)                                           #
#Owner             => E.F.P.                                                   #
#Programa DISB007B => LIQUIDACION DE APORTACIONES / GENERACION SALDOS CTA.IND  #
#Fecha creacion    => 1 DE ENERO DE 1997                                       #
#By                => J. DAVID HERNANDEZ O.                                    #
#Fecha actualiz.   => 10 AGOSTO 1997                                           #
#Actualizacion     => HECTOR M. FERNANDEZ A.                                   #
#Fecha actualiz.   => 08 MAYO 2001.                                            #
#Actualizacion     => GERARDO ALFONSO VEGA PAREDES.                            #
#Fecha actualiz.   => 26 OCTUBRE 2001                                          #
#Actualizacion     => GERARDO ALFONSO VEGA PAREDES.                            #
#Sistema           => DIS                                                      #
--------------------------------------------------------------------------------
#Por               => ALEJANDRO RAMIREZ.                                       #
#Fecha ult. modif. => 06 Jul 2005.                                             #
#Descripcion       => (v2) Impedir el lanzamineto de DISB078 cuando sea VIV    #
--------------------------------------------------------------------------------
#Por               => ALEJANDRO RAMIREZ.                                       #
#Fecha ult. modif. => 12 ago 2005.                                             #
#Descripcion       => (v4) Agrego la liquidacion de remanente y el folio se    #
#                  => ingresa en una tabla reconocida por devolu. de pagos     #
--------------------------------------------------------------------------------
#Por               => ALEJANDRO RAMIREZ.                                       #
#Fecha ult. modif. => 12 ago 2005.                                             #
#Descripcion       => (v5) Unifica versiones.                                  #
#                  => Al momento de liquidar  calcular intereses aclaraciones  #
#                  => ejecutando el programa DISB047B.4gl difer AZT.           #
#                  => (v3c) spl sin convertir pesos en rem                     #
--------------------------------------------------------------------------------
#Por               => ALEJANDRO RAMIREZ.                                       #
#Fecha ult. modif. => 22 may 2006.                                             #
#Descr.            => (v6) Se integraron los update de los programas alternos a#
#                  => este programa debido a que se bloqueaban entre si.       #
--------------------------------------------------------------------------------
#Por               => ALEJANDRO RAMIREZ.  22 may 2006                          #
#Descr.            => (c22-11) Nueva circular                                  #
--------------------------------------------------------------------------------
#Por               => DMR 22 Ene 2013                                          #
#Descr.            => Liquidaciones de Acl. Esp por separado RCV, EST y VIV    #
--------------------------------------------------------------------------------
#Modi.             => DMR 15 Febrero 2013                                      #
#Descr.            => Estandarizacion de version afores, SQLCA.SQLCODE y Liq   #
#                  => sin prioridad en prog liq, se aplica en las rutinas de   #
#                  => liq. liq_apor_est_esp y liq_apor_rcv_esp                 #
################################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      arr_c                SMALLINT,
      arr_l                SMALLINT,
      i                    SMALLINT,
      vsaldo_pesos         DECIMAL(16,2),
      totala               DECIMAL(16,2),
      totalc               DECIMAL(16,2)

   DEFINE
      hoy                  ,
      fecha1               ,
      fecha2               ,
      z_fecha              DATE

   DEFINE
      aux_pausa            ,
      tipo                 ,
      uno                  ,
      dos                  ,
      tres                 ,
      enter                CHAR(1)

   DEFINE
      salir                ,
      cont                 ,
      sigue                SMALLINT

   DEFINE
      tipo_desc            CHAR(30)

   DEFINE
      aa                   ,
      ab                   ,
      ac                   ,
      ad                   DECIMAL(16,6)

   DEFINE
      importe_liquida      ,
      importe_comision     ,
      importe_total        DECIMAL(16,6)

   DEFINE
      tipo_liquida         ,
      tipo_comision        CHAR(2)

   DEFINE
      total_accion         DECIMAL(16,6),
      opc                  CHAR(01),
      vmenos               SMALLINT,
      vsalida              CHAR(200)

   DEFINE
      prioridad            CHAR(25)

   DEFINE
      vfolio               INTEGER,
      vtipo                CHAR(01),
      vsubcuenta           CHAR(06),
      vfecha_pro           DATE,
      vfecha_liquidacion   DATE

   DEFINE
      cla_sel              CHAR(450),
      cla_upd              CHAR(450),
      vhora_max            CHAR(08),
      vhora_final          CHAR(08),
      vrow                 INTEGER

   DEFINE
      ejecuta              CHAR(200)

   DEFINE
      gusuario             CHAR(08),
      hora_inicial         CHAR(08),
      hora_final           CHAR(08),
      cve_afore            SMALLINT

   DEFINE
      valor_accion         LIKE glo_valor_accion.precio_del_dia

   DEFINE
      g_bat         RECORD LIKE dis_ctrl_proceso.*,
      g_param       RECORD LIKE dis_parametro.*

   DEFINE
      l_depositos   RECORD
                       num                  SMALLINT,
                       cuenta               SMALLINT,
                       valor                DECIMAL(16,6)
                    END RECORD

   DEFINE
      reg_bat       RECORD 
                       pid                  INTEGER,
                       proceso_cod          INTEGER,
                       opera_cod            INTEGER,
                       vfolio               INTEGER,
                       vtipo                CHAR(01),
                       vsubcuenta           CHAR(06),
                       vfecha_pro           DATE,
                       vfecha_liquidacion   DATE
                    END RECORD

   DEFINE
      g_sal ARRAY[500] OF RECORD
                             folio          INTEGER,
                             fecha_archivo  DATE,
                             aportacion     DECIMAL(16,2),
                             comision       DECIMAL(16,2),
                             seleccion      CHAR(01)
                          END RECORD
END GLOBALS


MAIN
   DISPLAY " "
   DISPLAY ".1"

   CALL STARTLOG("DISB007B.log")

   LET reg_bat.pid                = ARG_VAL(1)
   LET reg_bat.proceso_cod        = ARG_VAL(2)
   LET reg_bat.opera_cod          = ARG_VAL(3)
   LET reg_bat.vfolio             = ARG_VAL(4)
   LET reg_bat.vtipo              = ARG_VAL(5)
   LET reg_bat.vsubcuenta         = ARG_VAL(6)
   LET reg_bat.vfecha_pro         = ARG_VAL(7)
   LET reg_bat.vfecha_liquidacion = ARG_VAL(8)

   IF reg_bat.pid <> 0 THEN
      CALL Inserta_proceso()
   END IF

   DISPLAY "PID                   : ",reg_bat.pid
   DISPLAY "Proceso_cod           : ",reg_bat.proceso_cod
   DISPLAY "Opera_cod             : ",reg_bat.opera_cod
   DISPLAY "Folio                 : ",reg_bat.vfolio
   DISPLAY "Tipo liquidacion      : ",reg_bat.vtipo
   DISPLAY "Subcuenta             : ",reg_bat.vsubcuenta
   DISPLAY "Fecha_proceso         : ",reg_bat.vfecha_pro
   DISPLAY "Fecha_liquidacion     : ",reg_bat.vfecha_liquidacion

   LET vfolio     = reg_bat.vfolio
   LET vtipo      = reg_bat.vtipo
   LET vsubcuenta = reg_bat.vsubcuenta
   LET vfecha_pro = reg_bat.vfecha_pro
   LET fecha2     = reg_bat.vfecha_liquidacion

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM   dis_ctrl_proceso ",
                 "WHERE  proceso_cod = 'DISB007B' "," ",
                 "AND    etapa_cod = 2 "," ",
                 "AND    parametro3 =","'",vsubcuenta,"'" CLIPPED

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vrow
   CLOSE cur_proceso9

   SELECT *
   INTO g_bat.*
   FROM dis_ctrl_proceso
   WHERE proceso_cod = "DISB007B"
   AND etapa_cod   = 2
   AND consecutivo = vrow
   AND parametro3  = vsubcuenta

   SELECT *,user
   INTO   g_param.*,gusuario
   FROM   dis_parametro
 
   LET hoy = TODAY

   SELECT  codigo_afore                   --v5
   INTO    cve_afore                      --v5
   FROM    tab_afore_local                --v5

   CALL Proceso_principal(vfolio)

   IF  vsubcuenta <> "GAR" AND vsubcuenta <> "VIV"  THEN                 --v4
       ---- Identifica nss que cambiaron de siefore ----
       LET ejecuta = "nohup time fglgo DISB078B.4gi ",vfolio CLIPPED," &"
       RUN ejecuta
   END IF                                                                --v2

   ---- Habilita cuentas inhabilitadas por retiro  ----
   IF  vsubcuenta = "VIV" THEN                                           --v4
      --Para devolucion de pagos                                         --v4
      INSERT INTO safre_tmp:tmp_folios_pgo13                             --v4
      VALUES (vfolio,hoy)                                                --v4
   END IF                                                                --v4

   LET ejecuta = "nohup time fglgo DISB028.4gi ",vfolio CLIPPED," &"
   RUN ejecuta

   LET vhora_final = TIME

   LET cla_upd = " UPDATE dis_ctrl_proceso ",
                 " SET hora_final    = ","'",vhora_final,"'",",",
                     " resultado     = 'LIQUIDADO' ",
                 " WHERE proceso_cod = 'DISB007B' ",
                 " AND etapa_cod   = 2 ",
                 " AND consecutivo = ",vrow,
                 " AND parametro3  = ","'",vsubcuenta,"'" CLIPPED

   PREPARE claexe10 FROM cla_upd
   EXECUTE claexe10

   DISPLAY " "
   DISPLAY " "
   DISPLAY "Proceso Finalizado ..."

   CALL actualiza_bat_f(reg_bat.vfolio)
END MAIN


FUNCTION Inserta_proceso()
   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES 
      (
       TODAY,                      -- fecha_proceso
       "DISB007B",                 -- proceso_cod
       2,                          -- etapa_cod   -- LECTURA
       hora_inicial,               -- hora_inicial
       hora_final,                 -- hora_final
       reg_bat.vfolio,             -- parametro1
       reg_bat.vtipo,              -- parametro2
       reg_bat.vsubcuenta,         -- parametro3
       reg_bat.vfecha_pro,         -- parametro4
       reg_bat.vfecha_liquidacion, -- parametro5
       reg_bat.vfolio,             -- folio
       "PROCESANDO",               -- resultado
       gusuario,                   -- usuario
       0
      )

   IF STATUS < 0 THEN
      DISPLAY "Program Stopped"
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Liquidacion ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION Proceso_principal(vfolio)
   DEFINE
      x_reg         RECORD LIKE dis_provision.*

   DEFINE
      text          CHAR(1),
      vfolio        INTEGER,
      vfolio_nuevo  INTEGER,
      nom_spl       CHAR(150)

   LET vfolio_nuevo = vfolio

   CASE
      WHEN vtipo = "A" AND (vsubcuenta = "RCV" OR vsubcuenta = "VIV")

         IF vsubcuenta <> "VIV" THEN
            SELECT precio_del_dia              ----jerry
            INTO   valor_accion                ----jerry
            FROM   glo_valor_accion            ----jerry
            WHERE  fecha_valuacion = fecha2    ----jerry
            AND    codigo_siefore  = 1         ----jerry

            LET nom_spl = "EXECUTE PROCEDURE liq_apor_rcv ( ",
                          "'",fecha2,"'",",",
                          "'",vfecha_pro,"'",",",
                              vfolio, ",",
                              vfolio_nuevo, ",",
                              valor_accion,
                              ")"

            LET nom_spl = nom_spl CLIPPED 
            WHENEVER ERROR STOP
            PREPARE eje_spl1 FROM nom_spl
            EXECUTE eje_spl1
            WHENEVER ERROR CONTINUE
         END IF

         SELECT precio_del_dia              ----jerry
         INTO   valor_accion                ----jerry
         FROM   glo_valor_accion            ----jerry
         WHERE  fecha_valuacion = fecha2    ----jerry
         AND    codigo_siefore  = 11        ----jerry

         LET nom_spl = "EXECUTE PROCEDURE liq_apor_viv ( ",   ----jerry
                       "'",fecha2,"'", ",",
                       "'",vfecha_pro,"'",",",
                           vfolio, ",",
                           vfolio_nuevo, ",",
                           valor_accion,
                           ")"

         LET nom_spl = nom_spl CLIPPED    ----jerry
         WHENEVER ERROR STOP              ----jerry
         PREPARE eje_spl44 FROM nom_spl   ----jerry eje_spl44
         EXECUTE eje_spl44                ----jerry eje_spl44
         WHENEVER ERROR CONTINUE          ----jerry

         CALL Actualiza_estados() --v6

         CALL Actualiza_fecha_ult_rcv(vfolio,fecha2)
         CALL Actualiza_fecha_vol_pat(vfolio,fecha2)
         CALL Actualiza_fecha_ult_general(vfolio,fecha2)
         CALL Actualiza_sal_pro(vfolio,fecha2)

      WHEN vtipo = "A" AND vsubcuenta = "EST"                       #APORTE EST.
         SELECT precio_del_dia              ----jerry
         INTO   valor_accion                ----jerry
         FROM   glo_valor_accion            ----jerry
         WHERE  fecha_valuacion = fecha2    ----jerry
         AND    codigo_siefore  = 1         ----jerry

         LET nom_spl = "EXECUTE PROCEDURE liq_apor_est ( ",
                       "'",fecha2,"'", ",",
                     "'",vfecha_pro,"'",",",
                         vfolio, ",",
                         vfolio_nuevo, ",",
                         valor_accion,
                         ")"

         LET nom_spl = nom_spl CLIPPED 
         WHENEVER ERROR STOP
         PREPARE eje_spl2 FROM nom_spl
         EXECUTE eje_spl2
         WHENEVER ERROR CONTINUE

         CALL Actualiza_estados() --v6

         CALL Actualiza_dias_cot(vfolio)
         CALL Actualiza_fecha_ult_rcv(vfolio,fecha2)
         CALL Actualiza_fecha_ult_general(vfolio,fecha2)
         CALL Actualiza_sal_pro(vfolio,fecha2)

{     WHEN vtipo = "I" AND vsubcuenta = "TODAS"          # DMR liq. sep Acl. Esp
         SELECT precio_del_dia              ----jerry
         INTO   valor_accion                ----jerry
         FROM   glo_valor_accion            ----jerry
         WHERE  fecha_valuacion = fecha2    ----jerry
         AND    codigo_siefore  = 1         ----jerry

         LET nom_spl = "EXECUTE PROCEDURE liq_int_rcv ( ",
                       "'",fecha2,"'", ",",
                       "'",vfecha_pro,"'",",",
                           vfolio, ",",
                           vfolio_nuevo, ",",
                           valor_accion,
                           ")"

         LET nom_spl = nom_spl CLIPPED 
         WHENEVER ERROR STOP
         PREPARE eje_spl3 FROM nom_spl
         EXECUTE eje_spl3
         WHENEVER ERROR CONTINUE

         SELECT precio_del_dia              ----jerry
         INTO   valor_accion                ----jerry
         FROM   glo_valor_accion            ----jerry
         WHERE  fecha_valuacion = fecha2    ----jerry
         AND    codigo_siefore  = 11        ----jerry

         LET nom_spl = "EXECUTE PROCEDURE liq_apor_viv ( ",
                       "'",fecha2,"'", ",",
                       "'",vfecha_pro,"'",",",
                           vfolio, ",",
                           vfolio_nuevo, ",",
                           valor_accion,
                           ")"

         LET nom_spl = nom_spl CLIPPED 
         WHENEVER ERROR STOP
         PREPARE eje_spl4 FROM nom_spl
         EXECUTE eje_spl4
         WHENEVER ERROR CONTINUE

         CALL Actualiza_estados()  --v6

         CALL Actualiza_dias_cot(vfolio)
         CALL Actualiza_fecha_ult_rcv(vfolio,fecha2)
         CALL Actualiza_fecha_vol_pat(vfolio,fecha2)
         CALL Actualiza_fecha_ult_general(vfolio,fecha2)
         CALL Actualiza_sal_pro(vfolio,fecha2)               # DMR 22/01/13 }

      WHEN vtipo = "I" AND (vsubcuenta = "RCV" OR vsubcuenta = "VIV")

         IF vsubcuenta <> "VIV" THEN
            SELECT precio_del_dia              ----jerry
            INTO   valor_accion                ----jerry
            FROM   glo_valor_accion            ----jerry
            WHERE  fecha_valuacion = fecha2    ----jerry
            AND    codigo_siefore  = 1         ----jerry

            LET nom_spl = "EXECUTE PROCEDURE liq_apor_rcv_esp( ",
                          "'",fecha2,"'",",",
                          "'",vfecha_pro,"'",",",
                              vfolio, ",",
                              vfolio_nuevo, ",",
                              valor_accion,
                              ")"

            LET nom_spl = nom_spl CLIPPED 
            WHENEVER ERROR STOP
            PREPARE eje_spl1e FROM nom_spl
            EXECUTE eje_spl1e
            WHENEVER ERROR CONTINUE
         END IF

         SELECT precio_del_dia              ----jerry
         INTO   valor_accion                ----jerry
         FROM   glo_valor_accion            ----jerry
         WHERE  fecha_valuacion = fecha2    ----jerry
         AND    codigo_siefore  = 11        ----jerry

         LET nom_spl = "EXECUTE PROCEDURE liq_apor_viv ( ",   ----jerry
                       "'",fecha2,"'", ",",
                       "'",vfecha_pro,"'",",",
                           vfolio, ",",
                           vfolio_nuevo, ",",
                           valor_accion,
                           ")"

         LET nom_spl = nom_spl CLIPPED    ----jerry
         WHENEVER ERROR STOP              ----jerry
         PREPARE eje_spl44e FROM nom_spl  ----jerry eje_spl44
         EXECUTE eje_spl44e               ----jerry eje_spl44
         WHENEVER ERROR CONTINUE          ----jerry

         CALL Actualiza_estados() --v6

         CALL Actualiza_fecha_ult_rcv(vfolio,fecha2)
         CALL Actualiza_fecha_vol_pat(vfolio,fecha2)
         CALL Actualiza_fecha_ult_general(vfolio,fecha2)
         CALL Actualiza_sal_pro(vfolio,fecha2)

      WHEN vtipo = "I" AND vsubcuenta = "EST"                       #APORTE EST.
         SELECT precio_del_dia              ----jerry
         INTO   valor_accion                ----jerry
         FROM   glo_valor_accion            ----jerry
         WHERE  fecha_valuacion = fecha2    ----jerry
         AND    codigo_siefore  = 1         ----jerry

         LET nom_spl = "EXECUTE PROCEDURE liq_apor_est_esp( ",
                       "'",fecha2,"'", ",",
                     "'",vfecha_pro,"'",",",
                         vfolio, ",",
                         vfolio_nuevo, ",",
                         valor_accion,
                         ")"

         LET nom_spl = nom_spl CLIPPED 
         WHENEVER ERROR STOP
         PREPARE eje_spl2e FROM nom_spl
         EXECUTE eje_spl2e
         WHENEVER ERROR CONTINUE

         CALL Actualiza_estados() --v6

         CALL Actualiza_dias_cot(vfolio)
         CALL Actualiza_fecha_ult_rcv(vfolio,fecha2)
         CALL Actualiza_fecha_ult_general(vfolio,fecha2)
         CALL Actualiza_sal_pro(vfolio,fecha2)

      WHEN vtipo = "C" AND vsubcuenta = "GAR"                    # VIV GAR
         SELECT precio_del_dia              ----jerry
         INTO   valor_accion                ----jerry
         FROM   glo_valor_accion            ----jerry
         WHERE  fecha_valuacion = fecha2    ----jerry
         AND    codigo_siefore  = 11        ----jerry

         LET nom_spl = "EXECUTE PROCEDURE liq_viv_gar ( ",
                       "'",fecha2,"'",",",
                       "'",vfecha_pro,"'",",",
                           vfolio, ",",
                           vfolio_nuevo, ",",
                           valor_accion,
                           ")"

         LET nom_spl = nom_spl CLIPPED
         WHENEVER ERROR STOP
         PREPARE eje_spl5 FROM nom_spl
         EXECUTE eje_spl5
         WHENEVER ERROR CONTINUE

         CALL Actualiza_estados()  --v6
   END CASE
END FUNCTION


FUNCTION Actualiza_estados()
   CASE 
      WHEN vtipo = "A" AND (vsubcuenta = "RCV" OR vsubcuenta = "VIV")   --v4
         UPDATE dis_dep_aporte
         SET    estado = 3,      
                fech_liquidacion = fecha2   
         WHERE  folio = vfolio
         AND    ident_pago[14,15] IN ("41","43","45","46","47","48","49","40","11","23","31","32")  --c22-11
         AND    estado = 2

      WHEN vtipo = "A" AND vsubcuenta = "EST"
         UPDATE dis_dep_aporte
         SET    estado = 3,      
                fech_liquidacion = fecha2   
         WHERE  folio = vfolio
         AND    ident_pago[14,15] IN ("42","12")
         AND    estado = 2

      WHEN vtipo = "I" AND (vsubcuenta = "RCV" OR vsubcuenta = "VIV")   --v4
         UPDATE dis_dep_aporte
         SET    estado = 3,      
                fech_liquidacion = fecha2   
         WHERE  folio = vfolio
         AND    ident_pago[14,15] NOT IN ("42","12","44","24")          --c22-11
         AND    estado = 2

      WHEN vtipo = "I" AND vsubcuenta = "EST"
         UPDATE dis_dep_aporte
         SET    estado = 3,      
                fech_liquidacion = fecha2   
         WHERE  folio = vfolio
         AND    ident_pago[14,15] IN ("42","12")
         AND    estado = 2

{     WHEN vtipo = "I" AND vsubcuenta = "TODAS"        # DMR Liq. sepa Acl. Esp
         UPDATE dis_dep_aporte
         SET    estado = 3,      
                fech_liquidacion = fecha2   
         WHERE  folio = vfolio
         AND    estado = 2
         AND    ident_pago[14,15] NOT IN ("44","24")   # DMR 22/01/2013  }

      WHEN vtipo = "C" AND vsubcuenta = "GAR"
         UPDATE dis_dep_aporte
         SET    estado = 3,      
                fech_liquidacion = fecha2   
         WHERE  folio = vfolio
         AND    ident_pago[14,15] IN ("44","24")
         AND    estado = 2
   END CASE
END FUNCTION


FUNCTION Actualiza_sal_pro(vfolio, fecha2)
   DEFINE
      vfolio        INTEGER,
      fecha2        DATE,
      pnss          CHAR(11),
      pperiodo      CHAR(6),
      per_en_tab    CHAR(6),
      pconsec       INTEGER,
      psueldo_base  DECIMAL(12,2),
      num_salarios  DECIMAL(12,2),
      fec_act_vig   DATE,
      sal_min_vig   LIKE tabsalario_minimo2.monto_sm,
      pident_arch   CHAR(1)

   SELECT ident_arch
   INTO   pident_arch
   FROM   dis_cza_aporte
   WHERE  folio = vfolio

   IF pident_arch <> 2 AND pident_arch <> 4 THEN
      DISPLAY " ACTUALIZANDO SALARIOS "

      DECLARE cur_sal_pro CURSOR FOR
      SELECT UNIQUE nss, consecutivo_lote
      FROM  dis_cuenta
      WHERE folio     = vfolio

      FOREACH cur_sal_pro INTO pnss, pconsec
         SELECT periodo_pago, ult_salario_diario/100
         INTO   pperiodo, psueldo_base
         FROM   dis_det_aporte
         WHERE  folio = vfolio
         AND    consec_reg_lote = pconsec

         SELECT monto_sm
         INTO sal_min_vig
         FROM tabsalario_minimo2
         WHERE zona_cod = "A"
         AND   fecha_desde_sm IN (SELECT MAX(fecha_desde_sm)
                                  FROM tabsalario_minimo2
                                  WHERE zona_cod = "A")

         LET num_salarios = psueldo_base / sal_min_vig

         SELECT "OK"
         FROM  cta_his_sm
         WHERE nss = pnss
         GROUP BY 1

         IF SQLCA.SQLCODE = 100 THEN
            INSERT INTO cta_his_sm
            VALUES(pnss, vfolio, fecha2, pperiodo, psueldo_base,
                   num_salarios,"",hoy,gusuario)
         ELSE
            SELECT periodo_pago, fecha_act
            INTO   per_en_tab, fec_act_vig
            FROM   cta_his_sm
            WHERE  nss = pnss

            IF pperiodo > per_en_tab THEN
               UPDATE cta_his_sm
               SET folio        = vfolio,
                   fecha_liq    = fecha2,
                   periodo_pago = pperiodo,
                   salario_dia_int = psueldo_base,
                   num_sal_min     = num_salarios,
                   fecha_act_ant   = fec_act_vig,
                   fecha_act       = hoy,
                   usuario         = gusuario
               WHERE  nss  = pnss
            END IF
         END IF
      END FOREACH
      DISPLAY " SALARIOS ACTUALIZADOS "
   ELSE
      DISPLAY " ACLARACIONES ESP. NO ACTUALIZA SM "
   END IF
END FUNCTION


FUNCTION Actualiza_dias_cot(vfolio)  --v6
   DEFINE
      vfolio               INTEGER

   DEFINE
      g_reg1 RECORD
                nss        CHAR(11),
                dias_cot   INTEGER,
                dias_inca  INTEGER,
                dias_ause  INTEGER
             END RECORD

-- LET ejecuta = "nohup time fglgo DISB043.4gi ",vfolio CLIPPED," &"
-- RUN ejecuta

   DISPLAY "Actualiza_dias_cot ***************"

   DECLARE cur_dias CURSOR FOR
   SELECT n_seguro,
          SUM(dias_cotz_bimestre),
          SUM(dias_incap_bimest),
          SUM(dias_ausent_bimest)
   FROM   dis_det_aporte
   WHERE  folio = vfolio
   GROUP  BY 1

   FOREACH cur_dias INTO g_reg1.*
      IF g_reg1.dias_cot IS NULL THEN
         LET g_reg1.dias_cot = 0
      END IF

      IF g_reg1.dias_inca IS NULL THEN
         LET g_reg1.dias_inca = 0
      END IF

      IF g_reg1.dias_ause IS NULL THEN
         LET g_reg1.dias_ause = 0
      END IF

      LET g_reg1.dias_cot = g_reg1.dias_cot  -
                            g_reg1.dias_inca -
                            g_reg1.dias_ause

      UPDATE cta_ctr_cuenta
      SET    dias_cotizados = dias_cotizados + g_reg1.dias_cot
      WHERE  nss = g_reg1.nss

      LET g_reg1.dias_cot = 0
      LET g_reg1.dias_inca = 0
      LET g_reg1.dias_ause = 0
   END FOREACH
END FUNCTION


FUNCTION Actualiza_fecha_ult_rcv(vfolio,fecha2)  --v6
   DEFINE
      vfolio    INTEGER,
      fecha2    DATE,
      vnss      CHAR(11)

-- LET ejecuta = "nohup time fglgo DISB044.4gi ",vfolio CLIPPED," ",fecha2," &"
-- RUN ejecuta

   DISPLAY "Actualiza_fecha_ult_rcv ***************"

   DECLARE cur_pri_rcv CURSOR FOR
   SELECT unique a.nss
   FROM   dis_cuenta a,
          cta_ctr_cuenta b
   WHERE  folio           = vfolio
   AND    subcuenta      IN (1,2,5,6,9)
   AND    tipo_movimiento = 1
   AND    a.nss           = b.nss
   AND   (fecha_pri_rcv IS NULL OR fecha_pri_rcv = "01/01/0001")

   FOREACH cur_pri_rcv INTO vnss

      UPDATE cta_ctr_cuenta
      SET    fecha_pri_rcv = fecha2
      WHERE  nss           = vnss

   END FOREACH

   ----------------------------------------------------------------------
   ----------------------------------------------------------------------
   --------------------------Actualiza_ult_rcv---------------------------

   DECLARE cur_ult_rcv CURSOR FOR
   SELECT unique nss
   FROM   dis_cuenta
   WHERE  folio           = vfolio
   AND    subcuenta       IN (1,2,5,6,9)
   AND    tipo_movimiento = 1

   FOREACH cur_ult_rcv INTO vnss
      UPDATE cta_ctr_cuenta
      SET    fecha_ult_rcv = fecha2,
             ind_actividad = 1
      WHERE  nss = vnss
      AND    ind_actividad = 0
   END FOREACH
END FUNCTION


FUNCTION Actualiza_fecha_vol_pat(vfolio,fecha2)  --v6
   DEFINE
      vfolio     INTEGER,
      fecha2     DATE,
      vnss       CHAR(11)

-- LET ejecuta = "nohup time fglgo DISB046.4gi ",vfolio CLIPPED," ",fecha2," &"
-- RUN ejecuta

   DISPLAY "Actualiza_fecha_vol_pat ***************"

   DECLARE cur_fecha CURSOR FOR
   SELECT unique a.nss
   FROM   dis_cuenta a,
          cta_ctr_cuenta b
   WHERE  folio           = vfolio
   AND    subcuenta       = 3
   AND    tipo_movimiento = 1
   AND    a.nss           = b.nss
   AND    fecha_vol_pat IS NULL

   FOREACH cur_fecha INTO vnss

      UPDATE cta_ctr_cuenta
      SET    fecha_vol_pat = fecha2
      WHERE  nss = vnss

   END FOREACH
END FUNCTION


FUNCTION Actualiza_fecha_ult_general(vfolio,fecha2)  --v6
   DEFINE
      vfolio     INTEGER,
      fecha2     DATE,
      vnss       CHAR(11)

-- LET ejecuta = "nohup time fglgo DISB045.4gi ",vfolio CLIPPED," ",fecha2," &"
-- RUN ejecuta

   DISPLAY "Actualiza_fecha_ult_general ***************"

   DECLARE cur_pri_general CURSOR FOR
   SELECT unique a.nss
   FROM   dis_cuenta a,
          cta_ctr_cuenta b
   WHERE  folio           = vfolio
   AND    subcuenta       IN (1,2,3,4,5,6,9)
   AND    tipo_movimiento = 1
   AND    a.nss           = b.nss
   AND   (fecha_pri_general IS NULL OR fecha_pri_general = "01/01/0001")

   FOREACH cur_pri_general INTO vnss

      UPDATE cta_ctr_cuenta
      SET    fecha_pri_general = fecha2
      WHERE  nss               = vnss

   END FOREACH

   ----------------------------------------------------------------------
   ----------------------------------------------------------------------
   --------------------------Actualiza_ult_general-----------------------

   DECLARE cur_ult_general CURSOR FOR
   SELECT unique nss
   FROM   dis_cuenta
   WHERE  folio           = vfolio
   AND    subcuenta       IN (1,2,3,4,5,6,9)
   AND    tipo_movimiento = 1

   FOREACH cur_ult_general INTO vnss

      UPDATE cta_ctr_cuenta
      SET    fecha_ult_general = fecha2
      WHERE  nss = vnss

   END FOREACH
END FUNCTION


FUNCTION actualiza_bat_f(v_folio)   ---- FUNCION PARA EJECUTAR DESDE BATCH
   DEFINE
      v_cat          CHAR(600),
      vv_fecha_log   CHAR(030),
      vv_prog        CHAR(010),
      paso           CHAR(100)

   DEFINE
      v_fecha_log    DATETIME YEAR TO SECOND
   
   DEFINE
      v_folio        INTEGER

   DEFINE
      reg_ruta       RECORD LIKE seg_modulo.*

   SELECT A.*
   INTO   reg_ruta.*
   FROM   seg_modulo A
   WHERE  modulo_cod = "bat"
 
   UPDATE bat_ctr_operacion
   SET    folio       = v_folio,      
          estado_cod  = 4,
          fecha_fin   = CURRENT
   WHERE  pid         = reg_bat.pid
   AND    proceso_cod = reg_bat.proceso_cod
   AND    opera_cod   = reg_bat.opera_cod

   UPDATE bat_ctr_proceso
   SET    folio       = v_folio,      
          estado_cod  = 4,
          fecha_fin   = CURRENT
   WHERE  pid         = reg_bat.pid
   AND    proceso_cod = reg_bat.proceso_cod

   UPDATE bat_tmp_predecesor
   SET    bandera_ejecuta  = 1
   WHERE  pid_prod         = reg_bat.pid
   AND    proceso_cod_prod = reg_bat.proceso_cod
   AND    opera_cod_prod   = reg_bat.opera_cod

   LET v_fecha_log  = CURRENT
   LET vv_fecha_log = v_fecha_log

   SELECT A.programa_cod 
   INTO   vv_prog 
   FROM   bat_ctr_operacion A
   WHERE  A.pid         = reg_bat.pid
   AND    A.proceso_cod = reg_bat.proceso_cod
   AND    A.opera_cod   = reg_bat.opera_cod

   LET paso = "nohup:",
       reg_bat.pid         USING"&&&&&",":",
       reg_bat.proceso_cod USING"&&&&&",":",
       reg_bat.opera_cod   USING"&&&&&"

   LET v_cat = "echo '"                 ,
                vv_fecha_log[1,4]       ,   
                vv_fecha_log[6,7]       ,  
                vv_fecha_log[9,10]      ,  
                vv_fecha_log[12,13]     ,   
                vv_fecha_log[15,16]     ,    
                vv_fecha_log[18,19]     ,
                "|"                     ,
                vv_prog  CLIPPED        ,
                "|"                     ,
                "FINOK"                 ,
                "|"                     ,
                reg_ruta.ruta_listados CLIPPED,  
                "/"                     ,
                paso CLIPPED            ,
                "'"                     ,
                " >> "                  ,
                reg_ruta.ruta_envio CLIPPED,
                "/"                     ,
                "aad_safre.log"

   LET v_cat = v_cat CLIPPED
   RUN v_cat
END FUNCTION

