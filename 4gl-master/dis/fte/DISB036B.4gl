-------------------------------------------------------------------------------
-- Proyecto     => AFORE                                                     --
-- Propietario  => E.F.P.                                                    --
-- Programa     => DISB036B                                                  --
-- Descripcion  => ACTUALIZACION ETIQUETAS POR nohup                         --
-- Fecha        => 31 julio 2001.                                            --
-- By           => GERARDO ALFONSO VEGA PAREDES                              --
-- Sistema      => DIS                                                       --
-------------------------------------------------------------------------------

DATABASE safre_af

GLOBALS
   DEFINE
      g_bat RECORD LIKE dis_ctrl_proceso.*,
      vhora_max         CHAR(08),
      hora_inicial      CHAR(08),
      hora_final        CHAR(08),
      vsubcuenta        SMALLINT,
      vfecha_conversion DATE,
      cla_sel           CHAR(450),
      vhora_final       CHAR(08),
      vrow              INTEGER

END GLOBALS

MAIN
   CALL startlog("DISB036B.log")

   OPTIONS
      PROMPT LINE last,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
 
   LET vfecha_conversion = ARG_VAL(1)

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM dis_ctrl_proceso ",
                 "WHERE proceso_cod = 'DISB036B' ",
                 " AND etapa_cod = 6" CLIPPED

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vrow
   CLOSE cur_proceso9

   SELECT *
     INTO g_bat.*
     FROM dis_ctrl_proceso
    WHERE proceso_cod = "DISB036B"
      AND etapa_cod = 6
      AND consecutivo = vrow

   CALL Actualiza()

      LET vhora_final = TIME

      UPDATE dis_ctrl_proceso
         SET hora_final    = vhora_final,
             resultado     = "ETIQUETA ACTUALIZADA"
       WHERE proceso_cod   = "DISB036B"
         AND etapa_cod     = 6
         AND consecutivo  = vrow

END MAIN

FUNCTION Actualiza()
   UPDATE dis_cuenta
      SET etiqueta = 1
    WHERE subcuenta IN (4,8)
      AND tipo_movimiento NOT IN (3,888)
      AND fecha_valor < vfecha_conversion
      AND fecha_conversion < vfecha_conversion
      AND etiqueta = 0
END FUNCTION
