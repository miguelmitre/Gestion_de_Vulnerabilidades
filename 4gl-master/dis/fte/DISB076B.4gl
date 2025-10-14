-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Modulo       => DIS                                                       --
-- Programa     => DISB076B                                                  --
-- Descripcion  => RESPALDO 888 POR nohup saldo fondos de vivienda           --
-- Autor        => GERARDO ALFONSO VEGA PAREDES                              --
-- Fecha        => 3 agosti 2004.                                            --
-------------------------------------------------------------------------------
DATABASE safre_af

GLOBALS
   DEFINE
      usuario           CHAR(08),
      vsubcuenta        SMALLINT,
      vfecha_conversion DATE,
      g_bat RECORD LIKE dis_ctrl_proceso.*,
      vhora_max         CHAR(08),
      hora_inicial      CHAR(08),
      hora_final        CHAR(08),
      vhora_final       CHAR(08),
      cla_sel           CHAR(450),
      cla_upd           CHAR(450)

   DEFINE reg_bat RECORD
      pid              INTEGER,
      proceso_cod      INTEGER,
      opera_cod        INTEGER,
      fecha_conversion DATE
   END RECORD

   DEFINE vfolio INTEGER,
          vrow   INTEGER

END GLOBALS

MAIN

   DISPLAY " "
   DISPLAY ".1"

   CALL startlog("DISB076B.log")

   LET reg_bat.pid              = ARG_VAL(1)
   LET reg_bat.proceso_cod      = ARG_VAL(2)
   LET reg_bat.opera_cod        = ARG_VAL(3)
   LET reg_bat.fecha_conversion = ARG_VAL(4) 

   IF reg_bat.pid <> 0 THEN 
      CALL Inserta_proceso()
   END IF                   

   DISPLAY "PID                        : ",reg_bat.pid        
   DISPLAY "Proceso_cod                : ",reg_bat.proceso_cod
   DISPLAY "Opera_cod                  : ",reg_bat.opera_cod  
   DISPLAY "Fecha conversion           : ",reg_bat.fecha_conversion

   LET vfecha_conversion = reg_bat.fecha_conversion

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM   dis_ctrl_proceso ",
                 "WHERE  proceso_cod = 'DISB076B' "," ",
                 "AND    etapa_cod   = 4" CLIPPED

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vrow
   CLOSE cur_proceso9

   SELECT *
   INTO   g_bat.*
   FROM   dis_ctrl_proceso
   WHERE  proceso_cod  = "DISB076B"
   AND    etapa_cod    = 4
   AND    consecutivo  = vrow

   SELECT USER
   INTO   usuario
   FROM   dis_parametro

   CALL Respaldo()

   LET vhora_final = TIME

   LET cla_upd =" UPDATE dis_ctrl_proceso ",
                " SET    hora_final    = ","'",vhora_final,"'",",",
                "        resultado     = '888 RESPALDADO' ",
                " WHERE  proceso_cod   = 'DISB076B' ",
                " AND etapa_cod        = 4 ",
                " AND consecutivo      = ",vrow CLIPPED

   PREPARE claupdexe FROM cla_upd
   EXECUTE claupdexe

   DISPLAY " "                         
   DISPLAY " "                         
   DISPLAY "Proceso Finalizado ..."    
                                       
   CALL actualiza_bat_f(vfolio)

END MAIN

FUNCTION Inserta_proceso()

   LET hora_inicial = TIME

   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "DISB076B",              -- proceso_cod
       4,                       -- etapa_cod  -- Respaldo 888
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       vfecha_conversion,       -- parametro1
       NULL,                    -- parametro2
       NULL,                    -- parametro3
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "PROCESANDO",            -- resultado
       usuario,
       0)                 -- usuario
   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Respaldo 888 ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

END FUNCTION

FUNCTION Respaldo()
   DEFINE vchar CHAR(100)

   UNLOAD TO "/safre_dat/resp888/saldo_fov"
   SELECT * 
     FROM cta_saldo_fov

  LET vchar="mv /safre_dat/resp888/saldo_fov /safre_dat/resp888/saldo_fov." 
      CLIPPED,vfecha_conversion USING "dd-mm-yyyy"

  RUN vchar

  ERROR "PROCESO RESPALDO 888 TERMINADO..."

END FUNCTION

FUNCTION actualiza_bat_f(v_folio)   ---- FUNCION PARA EJECUTAR DESDE BATCH

   DEFINE v_cat          CHAR(600),
          vv_fecha_log   CHAR(030),
          vv_prog        CHAR(010),
          paso           CHAR(100)

   DEFINE v_fecha_log DATETIME YEAR TO SECOND
   
   DEFINE v_folio  integer

   DEFINE reg_ruta RECORD LIKE seg_modulo.*

   SELECT A.*
   INTO   reg_ruta.*
   FROM   seg_modulo A
   WHERE  modulo_cod = "bat"
 
   UPDATE bat_ctr_operacion
   SET    folio      = v_folio ,      
          estado_cod = 4    ,
          fecha_fin  = CURRENT
   WHERE pid         = reg_bat.pid
   AND   proceso_cod = reg_bat.proceso_cod
   AND   opera_cod   = reg_bat.opera_cod

   UPDATE bat_ctr_proceso
   SET    folio       = v_folio ,      
          estado_cod  = 4    ,
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

   LET paso = "nohup:"            ,
       reg_bat.pid         USING"&&&&&",":",
       reg_bat.proceso_cod USING"&&&&&",":",
       reg_bat.opera_cod   USING"&&&&&"

   LET v_cat = "echo '"                ,
                vv_fecha_log[1,4]       ,   
                vv_fecha_log[6,7]       ,  
                vv_fecha_log[9,10]      ,  
                vv_fecha_log[12,13]     ,   
                vv_fecha_log[15,16]     ,    
                vv_fecha_log[18,19]     ,
                "|"                    ,
                vv_prog  CLIPPED        ,
                "|"                     ,
                "FINOK"                ,
                "|"                     ,
                reg_ruta.ruta_listados CLIPPED,  
                "/"                     ,
                paso CLIPPED            ,
                "'"                     ,
                " >> "                  ,
                reg_ruta.ruta_envio CLIPPED ,
                "/"                     ,
                "aad_safre.log"

     LET v_cat = v_cat CLIPPED
     RUN v_cat
END FUNCTION

