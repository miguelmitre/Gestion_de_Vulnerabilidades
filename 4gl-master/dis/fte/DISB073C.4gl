###############################################################################
#Proyecto          => safre_af                                                #
#Owner             => E.F.P  					              # 
#Sistema           => DIS   					              #
#Programa          => DISB073C                                                #
#Descripcion       => CALCULA INTERESES DE FONDOS DE VIVIENDA                 #
#Modificado por    => ALEJANDRO RAMIREZ                                       #
#Fecha             => 3 abril  2006.                                          #
###############################################################################
DATABASE safre_af
GLOBALS
   -----  Variables de captura  -----
   DEFINE
      hist_fecha_aplica	DATE,

      g_reg RECORD
         fecha_aplica     DATE,
         saldo_prom_oper  DECIMAL(16,6),
         tasa_oper	  DECIMAL(10,6),
         monto_int_oper   DECIMAL(16,6),
         ctas_proc	  INTEGER,
         saldo_prom_afore DECIMAL(16,6),
         tasa_afore       DECIMAL(10,6),
         monto_int_afore  DECIMAL(16,6),
         fecha_dispersa   DATE
      END RECORD,
      dias_del_mes  INTEGER,
      max_folio_int INTEGER

   -----  Variables de calculo interes  -----
   DEFINE g_pend1 RECORD
      fecha_valor     DATE,
      nss             CHAR(11),
      curp	      CHAR(18),
      subcuenta	      SMALLINT,
      monto_pesos     DECIMAL(16,6),
      tipo_movimiento SMALLINT
   END RECORD,

   vfecha_pend        DATE,
   vmes_calculo       SMALLINT,
   vfecha_calculo     DATE,
   vtasa_opera        DECIMAL(11,6),
   vprimera_vez       CHAR(01),
   vfecha_masunmes    DATE,
   vpesos             DECIMAL(16,6),
   vmonto_acum        DECIMAL(16,6),
   vint_aplica        DECIMAL(16,6) 

   -----  Variables de calculo saldo promedio  -----
   DEFINE
      cta_saldo_prom RECORD LIKE cta_saldo_prom.*,
      vsaldo_prom  DECIMAL(16,6),
      inpc_t1, 
      inpc_t2  DECIMAL(16,6),
      vfactor  DECIMAL(16,6)

   -----  Variables de tablas   -----
   DEFINE l_reg RECORD LIKE dis_cuenta.*

   -----  Variables por omision  -----
   DEFINE
      hoy     DATE,
      opc     CHAR(1),
      usuario CHAR(8)
         
   -----  Variables ciclicas  -----
   DEFINE
      i, 
      cont INTEGER

   DEFINE hora_inicial CHAR(8)
   DEFINE hora_final   CHAR(8)
   DEFINE vhora_max    CHAR(8),
          vhora_final  CHAR(8),
          cla_sel      CHAR(450),
          cla_upd      CHAR(450),
          vrow         INTEGER

   DEFINE g_bat RECORD LIKE dis_ctrl_proceso.*

   DEFINE reg_bat       RECORD
      pid                INTEGER,
      proceso_cod        INTEGER,
      opera_cod          INTEGER,
      vtasa              DECIMAL(10,6),
      vfecha_aplicacion  DATE
   END RECORD

   DEFINE vfolio INTEGER

END GLOBALS

MAIN

   DISPLAY " "
   DISPLAY ".1"

   CALL STARTLOG("DISB073C.log")

   LET reg_bat.pid               = ARG_VAL(1)
   LET reg_bat.proceso_cod       = ARG_VAL(2)
   LET reg_bat.opera_cod         = ARG_VAL(3)
   LET reg_bat.vtasa             = ARG_VAL(4)
   LET reg_bat.vfecha_aplicacion = ARG_VAL(5)

   IF reg_bat.pid <> 0 THEN
      CALL Inserta_proceso()
   END IF
                                                                     
   DISPLAY "PID                        : ",reg_bat.pid
   DISPLAY "Proceso_cod                : ",reg_bat.proceso_cod
   DISPLAY "Opera_cod                  : ",reg_bat.opera_cod
   DISPLAY "Tasa                       : ",reg_bat.vtasa
   DISPLAY "Fecha aplicacion           : ",reg_bat.vfecha_aplicacion

   LET g_reg.tasa_oper     = reg_bat.vtasa
   LET g_reg.fecha_aplica  = reg_bat.vfecha_aplicacion

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM   dis_ctrl_proceso ",
                 "WHERE  proceso_cod = 'DISB073C' "," ",
                 "AND    etapa_cod = 1" CLIPPED

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vrow
   CLOSE cur_proceso9

   SELECT * 
   INTO   g_bat.*
   FROM   dis_ctrl_proceso
   WHERE  dis_ctrl_proceso.proceso_cod = "DISB073C"
   AND    dis_ctrl_proceso.etapa_cod   = 1
   AND    dis_ctrl_proceso.consecutivo = vrow

   SELECT USER
   INTO   usuario
   FROM   glo_parametro

   LET hoy = TODAY

   CALL Inicializa_tabla()

   CALL agrega()

   LET vhora_final = TIME

   LET cla_upd = "UPDATE dis_ctrl_proceso ",
                 "SET    hora_final  = ","'",vhora_final,"'",",",
                 "       resultado   = 'INTERESES CALCULADOS' ",
                " WHERE  proceso_cod = 'DISB073C' ",
                " AND    etapa_cod   = 1 ",
                " AND    consecutivo = ",vrow CLIPPED

   PREPARE claupdexe FROM cla_upd
   EXECUTE claupdexe

   DISPLAY " "
   DISPLAY " "
   DISPLAY "Proceso Finalizado ..."

   CALL actualiza_bat_f(vfolio)   

END MAIN

FUNCTION Inserta_proceso()

   LET hora_inicial = TIME
   LET hora_final   = null

   INSERT INTO dis_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "DISB073C",              -- proceso_cod
       1,                       -- etapa_cod  -- CALCULO RENDIMIENTOS
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       "14",                    -- parametro1
       g_reg.fecha_aplica,      -- parametro2
       g_reg.tasa_oper,         -- parametro3
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "PROCESANDO",            -- resultado
       usuario,
       0)                 -- usuario

   IF STATUS < 0 THEN
      DISPLAY "Program stopped"
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Calculo Interes ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
END FUNCTION

FUNCTION agrega()
   DEFINE
      mes,
      dia,
      ano         integer,
      dias        smallint,
      vsubcor_int CHAR(4),
      nom_spl     CHAR(150)

   DEFINE vfecha_inicio DATE,
          vfecha_final  DATE

   LET vfecha_inicio = g_reg.fecha_aplica - 1 UNITS MONTH

   CALL ultimo_dia_mes(vfecha_inicio) RETURNING dias 

   LET vfecha_final = MDY(MONTH(vfecha_inicio),dias,YEAR(vfecha_inicio))

   LET hora_inicial = TIME

   DISPLAY "Hora inicial ...",hora_inicial

   DATABASE safre_tmp
LET nom_spl="EXECUTE PROCEDURE calcula_interes_fov2(",g_reg.tasa_oper,",",
                                                "'",g_reg.fecha_aplica,"'",",",
                                                "'",vfecha_inicio,"'",",",
                                                "'",vfecha_final,"'",",",
                                                "'",usuario,"'",
                                                ")" 

   LET nom_spl = nom_spl CLIPPED

   PREPARE eje_spl FROM nom_spl
   EXECUTE eje_spl

   ERROR "TERMINO CALCULO INTERESES SATISFACTORIAMENTE"

   ERROR " "

   LET hora_final = TIME

   DISPLAY "Hora final ...",hora_final
   DATABASE safre_af

END FUNCTION

FUNCTION Inicializa_tabla()

   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp
         DROP TABLE cta_interes_fov2

         CREATE TABLE cta_interes_fov2
           (
             tipo_movimiento   smallint not null,
             subcuenta         smallint not null,
             siefore           smallint not null,
             folio             integer  not null,
             consecutivo_lote  integer,
             nss               char(11) not null,
             curp              char(18),
             folio_sua         char(6),
             fecha_pago        date,
             fecha_valor       date not null,
             fecha_conversion  date not null,
             monto_en_pesos    decimal(22,6) not null,
             monto_en_acciones decimal(22,6) not null,
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

      CREATE index cta_interes_fov_2 on cta_interes_fov2
         (nss,subcuenta,fecha_valor,fecha_conversion);

      DATABASE safre_af   
   WHENEVER ERROR STOP

END FUNCTION

FUNCTION ultimo_dia_mes(fecha)
   DEFINE
      fecha DATE, 
      dias  INTEGER

   CASE MONTH(fecha)
       WHEN  1 LET dias=31
       WHEN  2 IF YEAR(fecha) MOD 4 = 0 THEN
                  LET dias = 29
               ELSE
                  LET dias = 28
               END IF
       WHEN  3 LET dias=31
       WHEN  4 LET dias=30
       WHEN  5 LET dias=31
       WHEN  6 LET dias=30
       WHEN  7 LET dias=31
       WHEN  8 LET dias=31
       WHEN  9 LET dias=30
       WHEN 10 LET dias=31
       WHEN 11 LET dias=30
       WHEN 12 LET dias=31
   END CASE
   RETURN dias
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

