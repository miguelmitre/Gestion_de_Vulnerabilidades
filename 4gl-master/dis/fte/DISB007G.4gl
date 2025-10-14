################################################################################
#Project           => SAFRE (Mexico)                                           #
#Owner             => E.F.P.                                                   #
#Programa DISB007G => LIQUIDACION DE APORTACIONES / CARGOS CAMBIO DE REGIMEN   #
#                  => ISSSTE.                                                  #
#Por               => DMR                                                      #
#Fecha creacion    => 28 JULIO 2011                                            #
#Sistema           => DIS                                                      #
#Modificacion      =>                                                          #
#Fecha             =>                                                          #
################################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      gusuario           CHAR(08),
      hora_inicial       CHAR(08),
      hora_final         CHAR(08),
      cve_afore          SMALLINT,
      arr_c              SMALLINT,
      arr_l              SMALLINT,
      i                  SMALLINT,
      vsaldo_pesos       DECIMAL(16,2),
      totala             DECIMAL(16,2),
      totalc             DECIMAL(16,2)

   DEFINE
      hoy                ,
      fecha1             ,
      fecha2             ,
      z_fecha            DATE

   DEFINE
      aux_pausa          ,
      tipo               ,
      uno                ,
      dos                ,
      tres               ,
      enter              CHAR(1)

   DEFINE
      salir              ,
      cont               ,
      sigue              SMALLINT

   DEFINE
      tipo_desc          CHAR(30)

   DEFINE
      aa                 ,
      ab                 ,
      ac                 ,
      ad                 DECIMAL(16,6)

   DEFINE
      importe_liquida    ,
      importe_comision   ,
      importe_total      DECIMAL(16,6)

   DEFINE
      tipo_liquida       ,
      tipo_comision      CHAR(2)

   DEFINE
      total_accion       DECIMAL(16,6),
      opc                CHAR(01),
      vmenos             SMALLINT,
      vsalida            CHAR(200)

   DEFINE
      prioridad          CHAR(25)

   DEFINE
      vfolio             INTEGER,
      vtipo              CHAR(01),
      vsubcuenta         CHAR(06),
      vfecha_pro         DATE,
      vfecha_liquidacion DATE

   DEFINE
      cla_sel            CHAR(450),
      cla_upd            CHAR(450),
      vhora_max          CHAR(08),
      vhora_final        CHAR(08),
      vrow               INTEGER

   DEFINE
      ejecuta            CHAR(200)

   DEFINE
      valor_accion       LIKE glo_valor_accion.precio_del_dia

   DEFINE
      g_bat       RECORD LIKE dis_ctrl_proceso.*,
      g_param     RECORD LIKE dis_parametro.*

   DEFINE
      l_depositos RECORD
                     num                    SMALLINT,
                     cuenta                 SMALLINT,
                     valor                  DECIMAL(16,6)
                  END RECORD

   DEFINE
      reg_bat     RECORD
                     pid                    INTEGER,
                     proceso_cod            INTEGER,
                     opera_cod              INTEGER,
                     vfolio                 INTEGER,
                     vtipo                  CHAR(01),
                     vsubcuenta             CHAR(06),
                     vfecha_pro             DATE,
                     vfecha_liquidacion     DATE
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

   CALL STARTLOG("DISB007G.log")

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
                 "WHERE  proceso_cod = 'DISB007G' "," ",
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
   WHERE proceso_cod = "DISB007G"
   AND   etapa_cod   = 2
   AND   consecutivo = vrow
   AND   parametro3  = vsubcuenta

   SELECT *,user
   INTO   g_param.*,gusuario
   FROM   dis_parametro
 
   LET hoy = TODAY

   SELECT codigo_afore                   --v5
   INTO   cve_afore                      --v5
   FROM   tab_afore_local                --v5

   LET prioridad = "set pdqpriority high"
   PREPARE clapri FROM prioridad
   EXECUTE clapri

   CALL Proceso_principal(vfolio)

   LET vhora_final = TIME

   LET cla_upd = " UPDATE dis_ctrl_proceso ",
                 " SET hora_final    = ","'",vhora_final,"'",",",
                     " resultado     = 'LIQUIDADO' ",
                 " WHERE proceso_cod = 'DISB007G' ",
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
   LET hora_final   = null

   INSERT INTO dis_ctrl_proceso VALUES
      (TODAY,                      -- fecha_proceso
       "DISB007G",                 -- proceso_cod
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

   IF vsubcuenta = "RCVI" THEN
      SELECT precio_del_dia              ----jerry
      INTO   valor_accion                ----jerry
      FROM   glo_valor_accion            ----jerry
      WHERE  fecha_valuacion = fecha2    ----jerry
      AND    codigo_siefore  = 1         ----jerry

      LET nom_spl = "EXECUTE PROCEDURE liq_cam_regimen( ",
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

   CALL Actualiza_estados() --v6
END FUNCTION


FUNCTION Actualiza_estados()
   DEFINE
      vnss      CHAR(11)

   IF vsubcuenta = "RCVI" THEN
      UPDATE dis_dep_devorec
      SET    estado = 3,      
             fech_liquidacion = fecha2   
      WHERE  folio = vfolio
      AND    ident_pago[14,15] IN ("71","72","73","74","75")
      AND    estado = 2
   END IF
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

