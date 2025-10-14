-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- MOdulo       => DIS                                                       --
-- Programa     => DISB074B                                                  --
-- Descripcion  => AGRUPA INTERESES POR FECHA VALOR Y SEPARA EL REMANENTE    --
--              => EN LA TABLA dis_cuenta_fov                                --
-- Autor        => GERARDO ALFONSO VEGA PAREDES                              --
-- Fecha        => 2-agosto 2004.
-------------------------------------------------------------------------------
DATABASE safre_af

GLOBALS
   DEFINE g_int RECORD
      nss              CHAR(11),
      consecutivo_lote INTEGER,
      subcuenta        SMALLINT,
      fecha_valor      DATE,
      fecha_conversion DATE,
      monto_en_pesos   DECIMAL(16,6)
   END RECORD

   DEFINE vmonto_nor   DECIMAL(16,6),
          vmonto_rem   DECIMAL(16,6),
          vid_aportante  LIKE dis_cuenta.id_aportante

   DEFINE vremanente   CHAR(01),
          usuario      CHAR(08),
          vfolio       INTEGER,
          vconsec      DECIMAL(10,0),
          vcont1       DECIMAL(10,0),
          vcont2       DECIMAL(10,0),
          vcont3       DECIMAL(10,0),
          vcont4       DECIMAL(10,0),
          vcont5       DECIMAL(10,0)

   DEFINE hora_inicial CHAR(8)

   DEFINE hora_final   CHAR(8)

   DEFINE vhora_max    CHAR(8),
          vhora_final  CHAR(8),
          cla_sel      CHAR(450),
          cla_upd      CHAR(450),
          vrow         INTEGER

   DEFINE g_bat RECORD LIKE dis_ctrl_proceso.*

   DEFINE ejecuta CHAR(100)

   DEFINE prioridad CHAR(100)

   DEFINE reg_bat RECORD
      pid         INTEGER,
      proceso_cod INTEGER,
      opera_cod   INTEGER
   END RECORD
          
END GLOBALS

MAIN
   DISPLAY " "
   DISPLAY ".1"

   CALL STARTLOG("DISB074B.log")

   LET reg_bat.pid         = ARG_VAL(1)
   LET reg_bat.proceso_cod = ARG_VAL(2)
   LET reg_bat.opera_cod   = ARG_VAL(3)

   IF reg_bat.pid <> 0 THEN
      CALL Inserta_proceso()
   END IF

   DISPLAY "PID                        : ",reg_bat.pid
   DISPLAY "Proceso_cod                : ",reg_bat.proceso_cod
   DISPLAY "Opera_cod                  : ",reg_bat.opera_cod

   SELECT user
   INTO   usuario
   FROM   dis_parametro

   LET vconsec = 0
   LET vcont1 = 0
   LET vcont2 = 0
   LET vcont3 = 0
   LET vcont4 = 0
   LET vcont5 = 0

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM   dis_ctrl_proceso ",
                 "WHERE  proceso_cod = 'DISB074B' "," ",
                 "AND    etapa_cod   = 2" CLIPPED

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vrow
   CLOSE cur_proceso9

   SELECT *
   INTO   g_bat.*
   FROM   dis_ctrl_proceso
   WHERE  proceso_cod = "DISB074B"
   AND    etapa_cod = 2
   AND    consecutivo = vrow

   INSERT INTO glo_folio VALUES(0)
   SELECT MAX(folio)
   INTO   vfolio
   FROM   glo_folio

   CALL Inicializa_tabla()

   CALL Integra_interes()

   LET vhora_final = TIME

   LET cla_upd="UPDATE dis_ctrl_proceso ",
               "SET hora_final    = ","'",vhora_final,"'",",",
                  " folio         = ",vfolio,",",
                  " resultado     = 'INTERESES AGRUPADOS' ",
               " WHERE proceso_cod   = 'DISB074B' ",
               " AND etapa_cod     = 2 ",
               " AND consecutivo   = ",vrow CLIPPED

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
       "DISB074B",              -- proceso_cod
       2,                       -- etapa_cod  -- Agrupacion intereses
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       "S",                     -- parametro1
       "",                      -- parametro2
       NULL,                    -- parametro3
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "PROCESANDO",             -- resultado
       usuario,
       0)                 -- usuario
   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Agrupacion ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
        
END FUNCTION

FUNCTION Integra_interes()

  DEFINE  ins_cursor CHAR(250 )

  LET prioridad = "set pdqpriority high"
  PREPARE eje_prio FROM prioridad

  EXECUTE eje_prio

  DECLARE curint CURSOR FOR
  SELECT nss,
         consecutivo_lote,
         subcuenta,
         fecha_valor,
         fecha_conversion,
         sum(monto_en_pesos)
  FROM   safre_tmp:cta_interes_fov
  GROUP  BY 1,2,3,4,5
  ORDER  BY 1,2,3,4,5

  LET vremanente = "N"

  LET ins_cursor = "INSERT INTO safre_tmp:dis_cuenta_fov values ( 3, ?,0,?,?,?,NULL,0,?,?,?,?,0,0,0,NULL,?,5,TODAY,?,TODAY,1)"

  PREPARE eje_ins_cursor FROM ins_cursor
  DECLARE ins_cur CURSOR FOR eje_ins_cursor
  OPEN ins_cur
  FOREACH curint INTO g_int.*

    IF g_int.fecha_valor = "04/01/1999" THEN
      LET vmonto_nor=g_int.monto_en_pesos / 46.2096 * 12.7890
      LET vmonto_rem=g_int.monto_en_pesos / 46.2096 * 33.4206
      LET vremanente="S"
    END IF                                                                       
    IF g_int.fecha_valor = "05/01/2000" THEN
      LET vmonto_nor=g_int.monto_en_pesos / 27.6924 * 9.1121
      LET vmonto_rem=g_int.monto_en_pesos / 27.6924 * 18.5803
      LET vremanente="S"
    END IF

    IF g_int.fecha_valor = "04/01/2001" THEN
      LET vmonto_nor=g_int.monto_en_pesos / 27.5539 * 6.0175 
      LET vmonto_rem=g_int.monto_en_pesos / 27.5539 * 21.5364
      LET vremanente="S"
    END IF

    IF g_int.fecha_valor = "04/01/2002" THEN
      LET vmonto_nor=g_int.monto_en_pesos / 16.2064 * 4.2236 
      LET vmonto_rem=g_int.monto_en_pesos / 16.2064 * 11.9828
      LET vremanente="S"
    END IF

    IF g_int.fecha_valor = "04/01/2003" THEN
      LET vmonto_nor=g_int.monto_en_pesos / 49.6032 * 3.3676 
      LET vmonto_rem=g_int.monto_en_pesos / 49.6032 * 46.2356
      LET vremanente="S"
    END IF

   IF vremanente = "S" THEN
      IF vmonto_nor <> 0 THEN
         LET vid_aportante = "INFONAVIT"
      PUT ins_cur FROM g_int.subcuenta,vfolio,g_int.consecutivo_lote,g_int.nss,
                        g_int.fecha_valor,  g_int.fecha_valor, 
                        g_int.fecha_conversion, vmonto_nor, vid_aportante,
                        usuario
   END IF

         IF vmonto_rem <> 0 THEN
            LET vid_aportante = "REMANENTE"
            PUT ins_cur FROM g_int.subcuenta,vfolio,g_int.consecutivo_lote,
                        g_int.nss,
                        g_int.fecha_valor,  g_int.fecha_valor, 
                        g_int.fecha_conversion, vmonto_rem, vid_aportante,
                        usuario
         END IF
         LET vmonto_nor = 0
         LET vmonto_rem = 0
         LET vremanente = "N"
      ELSE
         IF g_int.monto_en_pesos <> 0 THEN
            LET vid_aportante = "INFONAVIT"
            PUT ins_cur FROM g_int.subcuenta, vfolio,g_int.consecutivo_lote,
                        g_int.nss,
                        g_int.fecha_valor,  g_int.fecha_valor, 
                        g_int.fecha_conversion, g_int.monto_en_pesos, 
                        vid_aportante, usuario
         END IF
      END IF

  END FOREACH
  CLOSE ins_cur

END FUNCTION

FUNCTION Inicializa_tabla()

   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp
         DROP TABLE dis_cuenta_fov

         CREATE TABLE dis_cuenta_fov
           (
             tipo_movimiento   smallint not null ,
             subcuenta         smallint not null ,
             siefore           smallint not null ,
             folio             integer  not null ,
             consecutivo_lote  integer,
             nss               char(11) not null ,
             curp              char(18),
             folio_sua         char(6),
             fecha_pago        date,
             fecha_valor       date not null ,
             fecha_conversion  date not null ,
             monto_en_pesos    decimal(22,6) not null ,
             monto_en_acciones decimal(22,6) not null ,
             precio_accion     decimal(22,6),
             dias_cotizados    smallint,
             sucursal          char(10),
             id_aportante      char(11),
             estado            smallint,
             fecha_proceso     date,
             usuario           char(8),
             fecha_archivo     date,
             etiqueta          smallint
          );

       DATABASE safre_af   
   WHENEVER ERROR STOP
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

