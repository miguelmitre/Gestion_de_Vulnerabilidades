################################################################################
#Project           => SAFRE (Mexico)                                           #
#Owner             => E.F.P.                                                   #
#Programa DISB007A => LIQUIDACION DE REDENCION DE BONO PENSION ANTICIPADO      #
#                  => ISSSTE.                                                  #
#Por               => DMR                                                      #
#Fecha creacion    => 21 DE Septiembre 2009                                    #
#Sistema           => DIS                                                      #
#Modificacion      =>                                                          #
#Fecha             =>                                                          #
################################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      vsaldo_pesos       DECIMAL(16,2)

   DEFINE
      hoy                ,
      fecha2             DATE

   DEFINE
      opc                CHAR(01),
      g_param            RECORD LIKE dis_parametro.*

   DEFINE
      prioridad          CHAR(25)

   DEFINE
      vfolio             INTEGER,
      vtipo              CHAR(01),
      vsubcuenta         CHAR(06),
      vfecha_pro         DATE,
      vfecha_liquidacion DATE

   DEFINE g_bat          RECORD LIKE dis_ctrl_proceso.*

   DEFINE
      cla_sel            CHAR(450),
      cla_upd            CHAR(450),
      vhora_final        CHAR(08),
      vrow               INTEGER

   DEFINE ejecuta        CHAR(200)

   DEFINE reg_bat        RECORD
      pid                   INTEGER,
      proceso_cod           INTEGER,
      opera_cod             INTEGER,
      vfolio                INTEGER,
      vtipo                 CHAR(01),
      vsubcuenta            CHAR(06),
      vfecha_pro            DATE,
      vfecha_liquidacion    DATE
                         END RECORD

   DEFINE gusuario       CHAR(08),
          hora_inicial   CHAR(08),
          hora_final     CHAR(08),
          cve_afore      SMALLINT
END GLOBALS


MAIN
   DISPLAY " "
   DISPLAY ".1"

   CALL STARTLOG("DISB007A.log")

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

   LET vfolio             = reg_bat.vfolio
   LET vtipo              = reg_bat.vtipo
   LET vsubcuenta         = reg_bat.vsubcuenta
   LET vfecha_pro         = reg_bat.vfecha_pro
   LET fecha2             = reg_bat.vfecha_liquidacion

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM   dis_ctrl_proceso ",
                 "WHERE  proceso_cod = 'DISB007A' "," ",
                 "AND    etapa_cod = 2 "," ",
                 "AND    parametro3 =","'",vsubcuenta,"'" CLIPPED

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vrow
   CLOSE cur_proceso9

   SELECT *
   INTO  g_bat.*
   FROM  dis_ctrl_proceso
   WHERE proceso_cod = "DISB007A"
   AND   etapa_cod   = 2
   AND   consecutivo = vrow
   AND   parametro3  = vsubcuenta

   SELECT *,user
   INTO   g_param.*,gusuario
   FROM   dis_parametro
 
   LET hoy = TODAY

   SELECT codigo_afore                    --v5
   INTO   cve_afore                       --v5
   FROM   tab_afore_local                 --v5

   LET prioridad = "set pdqpriority high"
   PREPARE clapri FROM prioridad
   EXECUTE clapri

   CALL Proceso_principal(vfolio)

   LET vhora_final = TIME

   LET cla_upd = " UPDATE dis_ctrl_proceso ",
                 " SET hora_final    = ","'",vhora_final,"'",",",
                     " resultado     = 'LIQUIDADO' ",
                 " WHERE proceso_cod = 'DISB007A' ",
                 " AND etapa_cod  = 2 ",
                 " AND consecutivo   = ",vrow,
                 " AND parametro3 = ","'",vsubcuenta,"'" CLIPPED

   PREPARE claexe10 FROM cla_upd
   EXECUTE claexe10

   DISPLAY " "
   DISPLAY " "
   DISPLAY "Proceso Finalizado ..."

   CALL actualiza_bat_f(reg_bat.vfolio)
END MAIN


FUNCTION Inserta_proceso()
   LET hora_inicial = TIME
   LET hora_final   = null

   INSERT INTO dis_ctrl_proceso VALUES
      (TODAY,                      -- fecha_proceso
       "DISB007A",                 -- proceso_cod
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
       0)

   IF STATUS < 0 THEN
      DISPLAY "Program stopped"
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Liquidacion ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION Proceso_principal(vfolio)
    DEFINE
        x_reg        RECORD LIKE dis_provision.*

    DEFINE
        text         CHAR(1),
        vfolio       INTEGER,
        vfolio_nuevo INTEGER,
        nom_spl      CHAR(150)

    LET vfolio_nuevo = vfolio

    CALL liquida_reden_ant(vfolio)
    CALL desmarca_bonos(vfolio)

    CALL Actualiza_estados() --v6
END FUNCTION


FUNCTION Actualiza_estados()
   UPDATE dis_dep_bono_ant
   SET    estado = 3,
          fech_liquidacion  = fecha2
   WHERE  folio = vfolio
   AND    ident_pago[14,15] IN ("49","41")
   AND    estado = 2
END FUNCTION


FUNCTION actualiza_bat_f(v_folio)   ---- FUNCION PARA EJECUTAR DESDE BATCH
   DEFINE v_cat          CHAR(600),
          vv_fecha_log   CHAR(030),
          vv_prog        CHAR(010),
          paso           CHAR(100)

   DEFINE v_fecha_log    DATETIME YEAR TO SECOND
   
   DEFINE v_folio        INTEGER

   DEFINE reg_ruta       RECORD LIKE seg_modulo.*

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

   LET v_fecha_log = CURRENT
   LET vv_fecha_log = v_fecha_log

   SELECT A.programa_cod
   INTO   vv_prog
   FROM   bat_ctr_operacion A
   WHERE  A.pid         = reg_bat.pid
   AND    A.proceso_cod = reg_bat.proceso_cod
   AND    A.opera_cod   = reg_bat.opera_cod

   LET paso = "nohup:"                  ,
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


FUNCTION liquida_reden_ant(vfolio)
   DEFINE
      vfolio        INTEGER,
      reg_p         RECORD LIKE dis_provision.*,
      cta_abo       SMALLINT,
      cta_car       SMALLINT,
      mov_abo       SMALLINT,
      mov_car       SMALLINT

   LET cta_abo = 30     # Retiro Issste
   LET cta_car = 36     # Bono

   LET mov_abo = 33     # Aporte
   LET mov_car = 27     # Redencion

   DECLARE curpr CURSOR FOR
   SELECT * FROM dis_provision
   WHERE folio = vfolio
   AND   subcuenta = 36                     # Subcuenta del Bono

   FOREACH curpr INTO reg_p.*

      IF reg_p.tipo_movimiento = 34 THEN    # Movimiento Ajuste
         INSERT INTO dis_cuenta
         VALUES (reg_p.*)
      ELSE
         SELECT precio_del_dia
         INTO  reg_p.precio_accion
         FROM  glo_valor_accion
         WHERE fecha_valuacion = fecha2
         AND   codigo_siefore  = 13        # Siefore del Bono
 
         # Registro del cargo

         LET reg_p.tipo_movimiento = mov_car
         LET reg_p.subcuenta       = cta_car
         LET vsaldo_pesos          = reg_p.monto_en_acciones*reg_p.precio_accion
         LET reg_p.monto_en_pesos  = vsaldo_pesos
         LET reg_p.id_aportante    = "REDEN. ANTIC. BONO"
         LET reg_p.estado          = 8
         LET reg_p.fecha_conversion= fecha2
         LET reg_p.fecha_proceso   = TODAY

         INSERT INTO dis_cuenta
         VALUES (reg_p.*)

         # Registro del Abono

         LET reg_p.tipo_movimiento = mov_abo
         LET reg_p.subcuenta       = cta_abo

         SELECT codigo_siefore
         INTO   reg_p.siefore
         FROM   cta_regimen
         WHERE  nss       = reg_p.nss
         AND    subcuenta = cta_abo

         IF reg_p.siefore IS NULL THEN
            DISPLAY "NSS ",reg_p.nss," No existe en CTA_REGIMEN, Subcuenta ",cta_abo
         END IF

         SELECT precio_del_dia
         INTO   reg_p.precio_accion
         FROM   glo_valor_accion
         WHERE  fecha_valuacion = fecha2
         AND    codigo_siefore  = reg_p.siefore

         LET vsaldo_pesos            = reg_p.monto_en_pesos * (-1)
         LET reg_p.monto_en_pesos    = vsaldo_pesos
         LET reg_p.monto_en_acciones = reg_p.monto_en_pesos/reg_p.precio_accion
         LET reg_p.id_aportante      = "REDEN. ANTIC. BONO"
         LET reg_p.estado            = 8
         LET reg_p.fecha_proceso     = TODAY
         LET reg_p.fecha_conversion  = fecha2

         INSERT INTO dis_cuenta
         VALUES (reg_p.*)
      END IF
   END FOREACH
END FUNCTION


FUNCTION desmarca_bonos(vfolio)
   DEFINE vfolio    INTEGER,
          nssx      CHAR(11),
          reg_acti  RECORD LIKE cta_act_bono.*

   DECLARE curd CURSOR FOR
   SELECT UNIQUE nss FROM dis_cuenta
   WHERE folio = vfolio

   FOREACH curd INTO nssx
      DECLARE curd2 CURSOR FOR
      SELECT * FROM cta_act_bono
      WHERE nss = nssx
    
      FOREACH curd2 INTO reg_acti.*
         INSERT INTO cta_his_bono
         VALUES (reg_acti.nss, reg_acti.curp, reg_acti.fecha_redencion,
                 reg_acti.fecha_registro, reg_acti.udis, reg_acti.pesos,
                 reg_acti.proceso,TODAY,"RDA","",reg_acti.usuario)

         DELETE FROM cta_act_bono
         WHERE nss = reg_acti.nss
      END FOREACH
   END FOREACH
END FUNCTION


