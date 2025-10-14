-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => MDISB011B                                                 --
-- Descripcion  => RESPALDO 888 POR nohup.                                   --
-- Fecha        => 15 agosto de 2000.                                        --
-- By           => GERARDO ALFONSO VEGA PAREDES                              --
-- Sistema      => DIS                                                       --
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
      cla_sel           CHAR(450)

END GLOBALS

MAIN

   CALL startlog("DISB035B.log")

   OPTIONS
      PROMPT LINE last,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST

   LET vfecha_conversion = ARG_VAL(1)

   LET cla_sel = "SELECT MAX(hora_inicial) ",
                 "FROM dis_ctrl_proceso ",
                 "WHERE proceso_cod = 'DISB035B' ",
                 " AND etapa_cod = 5" CLIPPED

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vhora_max
   CLOSE cur_proceso9

   SELECT *
     INTO g_bat.*
     FROM dis_ctrl_proceso
    WHERE proceso_cod = "DISB035B"
      AND etapa_cod = 5
      AND hora_inicial = vhora_max

   SELECT USER
     INTO usuario
     FROM dis_parametro

   CALL Respaldo()

   LET vhora_final = TIME

   UPDATE dis_ctrl_proceso
      SET hora_final    = vhora_final,
          resultado     = "888 REPALDADO"
    WHERE proceso_cod   = "DISB035B"
      AND etapa_cod     = 5
      AND hora_inicial  = vhora_max

END MAIN

FUNCTION Respaldo()
   DEFINE vchar CHAR(100)

   UNLOAD TO "/safre_mig/resp888/cta888"
   SELECT * 
     FROM dis_cuenta
    WHERE subcuenta IN (4,8) 
      AND tipo_movimiento = 888

  LET vchar="mv /safre_mig/resp888/cta888 /safre_mig/resp888/cta888." 
      CLIPPED,vfecha_conversion USING "dd-mm-yyyy"

  RUN vchar

  ERROR "PROCESO RESPALDO 888 TERMINADO..."

END FUNCTION
