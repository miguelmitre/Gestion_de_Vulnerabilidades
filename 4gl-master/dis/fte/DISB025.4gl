-------------------------------------------------------------
--Proyecto           => Sistema de Afores.( MEXICO )       --
--Propietario        => E.F.P.                             --
--Programa  DISB025  => Reinicio Int. y Reverseo Total.    --
--Sistema            => DIS.                               --
--Por                => MIGUEL ANGEL HERNANDEZ MARTINEZ    --
--Fecha              => 11 DE ENERO 2000.                  --
--Por                => GERARDO ALFONSO VEGA PAREDES       --
--Fecha              => 12 DE ABRIL 2000.                  --
-------------------------------------------------------------
DATABASE safre_af

GLOBALS
   DEFINE 
      hoy           DATE,
      hora          CHAR(08),
      usuario       CHAR(08),
      cta_nss       CHAR(11),
      nss_saldo     CHAR(11),
      fecha_ant     DATE,
      fecha_ant1    DATE,
      ruta          CHAR(300),
      carga         CHAR(500),
      pos           SMALLINT,
      aux_pausa     CHAR(01),
      cla_where     CHAR(200),
      cla_del       CHAR(300),
      sel_where     CHAR(200),
      fecha_proceso DATE,
      fecha_aplica  DATE

   DEFINE v       RECORD
      subcuenta     CHAR(01),
      fecha_aplica  DATE
   END RECORD
 
   DEFINE g       RECORD
      subcuenta     CHAR(01),
      fecha_aplica  DATE
   END RECORD

   DEFINE vvv      RECORD
      subcuenta     CHAR(01),
      fecha_aplica  DATE
   END RECORD

   DEFINE l_record  ARRAY[100] OF RECORD 
      fecha_proceso DATE,
      fecha_aplica  DATE,
      subcuenta     CHAR(01),
      nss           CHAR(11),
      hora_proceso  CHAR(08), 
      usuario       CHAR(08),
      origen_cod    CHAR(01)
   END RECORD,              
   vpasswd          CHAR(01)           

   DEFINE g_reg4 RECORD       
      super_cod     SMALLINT,    
      super_desc    CHAR(30),    
      nip           INTEGER      
   END RECORD,                
   vnip             INTEGER,              
   opc              CHAR(01)               

---------- VARIABLES PARA CALCULO INTERESES ------------
       
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
         
   -----  Variables ciclicas  -----
   DEFINE
      i, 
      cont INTEGER
   DEFINE
      g_param RECORD LIKE dis_parametro.*

   DEFINE vfecha_inicio DATE,
          vfecha_final  DATE,
          cla_spl       CHAR(150),
          dias          SMALLINT

    DEFINE vcampo    CHAR(100),
           vnumero   CHAR(100),
           vnumero_c CHAR(100),
           longitud  char(150),
           vnumerico INTEGER

   DEFINE g_bat RECORD LIKE dis_ctrl_proceso.*

   DEFINE cla_sel     CHAR(450),
          cla_upd     CHAR(450),
          vhora_max   CHAR(08),
          vhora_final CHAR(08),
          vrow        INTEGER


END GLOBALS

MAIN
    OPTIONS PROMPT LINE LAST ,
            INPUT WRAP ,
            ACCEPT KEY control-o
    DEFER INTERRUPT
  
    LET vpasswd = "N"

    CALL Aplica_passwd() RETURNING vpasswd

    IF vpasswd="S" THEN
       CALL inicio()
       CALL proceso_principal()
    END IF

END MAIN

FUNCTION Aplica_passwd()
   OPEN WINDOW ventana_4 AT 08,12 WITH FORM "DISB0254" ATTRIBUTE(BORDER)
   DISPLAY " [Esc] Procesar       DISB025       [Ctrl-c] Cancelar " AT 2,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME g_reg4.*
      AFTER FIELD super_cod
         IF g_reg4.super_cod IS NULL THEN
            ERROR "ID no puede ser nulo"
            NEXT FIELD super_cod
         END IF

         SELECT super_desc,
                nip
         INTO   g_reg4.super_desc,
                vnip 
         FROM   tab_supervisor 
         WHERE  super_cod = g_reg4.super_cod

         IF STATUS = NOTFOUND THEN
            ERROR "No existe este ID "
            NEXT FIELD super_cod
         END IF

         DISPLAY BY NAME g_reg4.super_desc
         NEXT FIELD nip
      AFTER FIELD nip
         IF g_reg4.nip <> vnip THEN
            ERROR "Clave de acceso no existe"
            LET g_reg4.nip = null
            NEXT FIELD super_cod
         END IF

         IF g_reg4.nip IS NULL OR
            g_reg4.nip = 0 THEN
            ERROR "Clave de acceso no existe"
            NEXT FIELD super_cod
         END IF

      ON KEY (ESC)
         IF g_reg4.super_cod IS NULL THEN
            ERROR "ID no puede ser nulo"
            NEXT FIELD super_cod
         END IF

         SELECT "x"
         FROM   tab_supervisor 
         WHERE  super_cod = g_reg4.super_cod

         IF STATUS = NOTFOUND THEN
            ERROR "No existe este ID "
            NEXT FIELD super_cod
         END IF

         IF g_reg4.nip IS NULL OR
            g_reg4.nip = 0 THEN
            ERROR "Clave de acceso no existe"
            NEXT FIELD super_cod
         END IF

         IF g_reg4.nip <> vnip THEN
            ERROR "Clave de acceso no existe"
            LET g_reg4.nip = null
            NEXT FIELD super_cod
         END IF

         ERROR "Acceso aceptado"
         SLEEP 2

         LET vpasswd = "S"
         EXIT INPUT
      ON KEY (INTERRUPT)
         ERROR "Acceso denegado"
         SLEEP 2

         LET vpasswd = "N"
         EXIT INPUT
   END INPUT

   ERROR ""

   CLOSE WINDOW ventana_4
   RETURN vpasswd
END FUNCTION

FUNCTION inicio()
    SELECT USER
    INTO   usuario
    FROM   tab_afore_local

    SELECT *
      INTO g_param.*
      FROM dis_parametro

    LET hoy = TODAY
    LET hora = TIME
END FUNCTION

FUNCTION proceso_principal()
    OPEN WINDOW ventana AT 3,3
    WITH 20 ROWS, 75 COLUMNS
    ATTRIBUTE (BORDER)
   
    DISPLAY " DISB025 " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY hoy USING " dd-mm-yyyy " AT 3,64 ATTRIBUTE (REVERSE)
    DISPLAY "-------------------------------------------------------------------------------" AT 4,1

    MENU "Menu"

      COMMAND "Reverso total"         
              "Reverso total del periodo."
          CALL Reverso_total()


----      COMMAND "Preparacion reinicio"  
----              "Prepara informacion a partir nss de interrupcion"
----          CALL Preparacion_reinicio()


      COMMAND "Reinicio calculo intereses" 
              "Calcula intereses a partir nss interrupcion"
          CALL Reinicio_calculo()


      COMMAND "Consulta "             
              "Consulta de NSS. interrumpido"
          CALL Consulta()


----      COMMAND "Reinicio integracion"
----              "Reinicia integracion de intereses"
----          CALL Reinicio_integracion()


      COMMAND "Salir" "Salir del programa"
          EXIT MENU


    END MENU

    CLOSE WINDOW ventana

END FUNCTION 

FUNCTION Reinicio_calculo()

   OPEN WINDOW ventana_1 AT 7,3 WITH FORM "DISB0251" ATTRIBUTE (BORDER)
   DISPLAY " (ESC) Ejecutar                                             (Ctrl-c) Salir     "  AT 1,1 ATTRIBUTE (REVERSE)

   LET int_flag = FALSE
  
   INPUT BY NAME v.*
      AFTER FIELD subcuenta
         IF v.subcuenta IS NULL THEN
            ERROR "La subcuenta no puede ser nula..."
            NEXT FIELD subcuenta
         END IF

      AFTER FIELD fecha_aplica
         IF v.fecha_aplica IS NULL THEN
            ERROR "La fecha no puede ser nula..."
            NEXT FIELD fecha_aplica
         END IF
                                                                       
         IF DAY(v.fecha_aplica) <> 1 THEN                      
            ERROR "Los intereses deben aplicarse al primer dia del mes"
            NEXT FIELD fecha_aplica
         END IF

         DECLARE cur_viv CURSOR FOR
         SELECT "g"
         FROM   safre_tmp:cta_interes_viv
         WHERE  fecha_conversion = v.fecha_aplica

         OPEN cur_viv
         FETCH cur_viv
         IF STATUS = NOTFOUND THEN
            ERROR "Esta fecha no existe para efectuar el reinicio..."
            CLOSE cur_viv
            NEXT FIELD fecha_aplica
         END IF
         CLOSE cur_viv
 
       ON KEY (ESC)

          IF v.subcuenta IS NULL THEN
             ERROR "La subcuenta no puede ser nula..."
             NEXT FIELD subcuenta
          END IF

          IF v.fecha_aplica IS NULL THEN
             ERROR "La fecha no puede ser nula..."
             NEXT FIELD fecha_aplica
          END IF

          IF DAY(v.fecha_aplica) <> 1 THEN                      
             ERROR "Los intereses deben aplicarse al primer dia del mes"
             NEXT FIELD fecha_aplica
          END IF

          DECLARE cur_viv2 CURSOR FOR
          SELECT "g"
          FROM   safre_tmp:cta_interes_viv
          WHERE  fecha_conversion = v.fecha_aplica

          OPEN cur_viv2
          FETCH cur_viv2
          IF STATUS = NOTFOUND THEN
             ERROR "Esta fecha no existe para efectuar el reinicio..."
             CLOSE cur_viv2
             NEXT FIELD fecha_aplica
          END IF
          CLOSE cur_viv2

          LET int_flag = FALSE
          EXIT INPUT
       ON KEY (Control-c)
          LET int_flag = TRUE
          EXIT INPUT
    END INPUT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      CLEAR FORM
      ERROR "Proceso cancelado..."
      SLEEP 2
      CLEAR SCREEN
      CLOSE WINDOW ventana_1
      RETURN
   END IF

   CLOSE WINDOW ventana_1
       
   CALL Pregunta()
       
   IF aux_pausa MATCHES "[Ss]" THEN

      SELECT tasa_valor
      INTO   vtasa_opera
      FROM   tab_tasa_remanente
      WHERE  tasa_fecha  = v.fecha_aplica
      AND    tasa_origen = "VIV"

      ERROR "BUSCANDO MAXIMO NSS..."
   
      SELECT MAX(e.nss)
      INTO   cta_nss
      FROM   safre_tmp:cta_interes_viv e

      INSERT INTO cta_nss_max VALUES
         (cta_nss,
          v.subcuenta,
          v.fecha_aplica,
          hoy,
          hora,
          "C",     --origen_cod = Calculo intereses
          usuario)

      ERROR "BUSCANDO NSS MAXIMO EN RESPALDO 888..."

      LET fecha_ant = v.fecha_aplica - 1 UNITS MONTH

----      LET ruta = "grep ",cta_nss,g_param.ruta_envio CLIPPED,"/ctas888.",
----                  fecha_ant USING "DD-MM-YYYY",
----                 " > ",g_param.ruta_envio CLIPPED,"/nss_max"

      WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE archivos_viv  
        CREATE TABLE archivos_viv
           (campo CHAR(100))
        DATABASE safre_af
      WHENEVER ERROR STOP

      LET ruta ="wc -l /safre/resp888/saldo888.",fecha_ant USING "DD-MM-YYYY",
                " > ","/safre/resp888/cuantos" CLIPPED
      RUN ruta

      LET ruta = "/safre/resp888/cuantos" CLIPPED
      LOAD FROM ruta INSERT INTO safre_tmp:archivos_viv

      SELECT campo
      INTO   vcampo
      FROM   safre_tmp:archivos_viv

      LET longitud = LENGTH(vcampo)
      FOR i=1 TO longitud
         IF vcampo[i] = "/" THEN
            LET vnumero_c = vcampo[1,i-1]
            EXIT FOR
         END IF
      END FOR           

      LET vnumerico = vnumero_c

      IF vnumerico <> 0 THEN
         LET ruta = "grep ","/safre/resp888/saldo888.",fecha_ant USING "DD-MM-YYYY",
                    " > ","/safre/resp888/nss_max"
         RUN ruta
      END IF

      ERROR "BORRAR NSS MAXIMO DE cta_saldo_viv..."

      LET cla_del = "DELETE FROM cta_saldo_viv ",
                   " WHERE cta_saldo_viv.nss =","'",cta_nss,"'" CLIPPED

      PREPARE claexed2 FROM cla_del
      EXECUTE claexed2

      ERROR "SUBIENDO ARCHIVO DEL RESPALDO 888..."  

-------      LET ruta = g_param.ruta_envio CLIPPED,"/nss_max"

      IF vnumerico <> 0 THEN
         LET ruta = "/safre/resp888/nss_max" CLIPPED
         LOAD FROM ruta
         INSERT INTO cta_saldo_viv
      END IF

      ERROR "BORRAR NSS DE cta_interes_viv..."

      LET cla_del = "DELETE FROM safre_tmp:cta_interes_viv ",
                    " WHERE nss=","'",cta_nss,"'",
                    " AND   subcuenta in (4,8) " CLIPPED

      PREPARE claexed3 FROM cla_del
      EXECUTE claexed3

      ERROR ""



----------------------------------------------------------------------
   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM dis_ctrl_proceso ",
                 "WHERE proceso_cod = 'DISB009B' ",
                 " AND etapa_cod = 3" CLIPPED

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vrow
   CLOSE cur_proceso9

   SELECT * 
     INTO g_bat.*
     FROM dis_ctrl_proceso
    WHERE dis_ctrl_proceso.proceso_cod = "DISB009B"
      AND dis_ctrl_proceso.etapa_cod = 3
      AND dis_ctrl_proceso.consecutivo = vrow
--------------------------------------------------------------------------


      LET vfecha_inicio = v.fecha_aplica - 1 UNITS MONTH

      CALL ultimo_dia_mes(vfecha_inicio) RETURNING dias

      LET vfecha_final = MDY(MONTH(vfecha_inicio),dias,YEAR(vfecha_inicio))

      ERROR "PROCESANDO INFORMACION..."

LET cla_spl = "EXECUTE PROCEDURE dispersa_interes2(",vtasa_opera,",",
                                                   "'",v.fecha_aplica,"'",",",
                                                   "'",vfecha_inicio,"'",",",
                                                   "'",vfecha_final,"'",",",
                                                   "'",usuario,"'",",",
                                                   "'",cta_nss,"'",
                                                   ")"

      LET cla_spl = cla_spl CLIPPED

      PREPARE cla_spl FROM cla_spl
      EXECUTE cla_spl


---------------------------------------------------------------------
   LET vhora_final = TIME

   LET cla_upd = "UPDATE dis_ctrl_proceso ",
                 "SET    hora_final   = ","'",vhora_final,"'",",",
                 "       resultado    = 'INTERESES CALCULADOS' ",
                " WHERE proceso_cod   = 'DISB009B' ",
                " AND etapa_cod       = 3 ",
                " AND consecutivo  = ",vrow
   PREPARE claupdexe FROM cla_upd
   EXECUTE claupdexe
--------------------------------------------------------------------

   ELSE
      ERROR "PROCESO DE REINICIO CANCELADO..."
      SLEEP 2
      ERROR ""
   END IF
END FUNCTION

FUNCTION ultimo_dia_mes(fecha)              
   DEFINE fecha DATE,
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

FUNCTION Reverso_total()
   DEFINE cla_loa   CHAR(200)
   DEFINE vcompleto CHAR(01),
          max_fecha DATE

   OPEN WINDOW ventana_3 AT 7,3 WITH FORM "DISB0252" ATTRIBUTE (BORDER)
   DISPLAY " (ESC) Ejecutar                                             (Ctrl-c) Salir     "  AT 1,1 ATTRIBUTE (REVERSE)

   LET int_flag = FALSE
  
   INPUT BY NAME vvv.fecha_aplica 

      AFTER FIELD fecha_aplica
         IF vvv.fecha_aplica IS NULL THEN
            ERROR "La fecha no puede ser nula..."          
            NEXT FIELD fecha_aplica
         END IF

      ON KEY (ESC)

         IF vvv.fecha_aplica IS NULL THEN
            ERROR "La fecha no puede ser nula..."          
            NEXT FIELD fecha_aplica
         END IF

----         DECLARE cur_int CURSOR FOR
----         SELECT "M"
----         FROM   dis_cuenta 
----         WHERE  fecha_conversion = vvv.fecha_aplica
----         AND    subcuenta in (4,8)
----         AND    tipo_movimiento = 3
        
----         OPEN cur_int
----         FETCH cur_int
----         IF STATUS = NOTFOUND THEN
----            ERROR "No existe esta fecha para el reverso total..."
----            CLOSE cur_int
----            NEXT FIELD fecha_aplica
----         END IF
----         CLOSE cur_int

         LET int_flag = FALSE

         EXIT INPUT

      ON KEY (Control-c)
         LET int_flag = TRUE
         EXIT INPUT
   END INPUT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      CLEAR FORM
      ERROR "Proceso cancelado..."
      SLEEP 2
      CLEAR SCREEN
      CLOSE WINDOW ventana_3
      RETURN
   END IF

    LET fecha_ant1 = vvv.fecha_aplica - 1 UNITS MONTH

    ERROR "PROCESANDO INFORMACION..."

---------   Borra 888 de cta_saldo_viv   -------------
   WHENEVER ERROR CONTINUE
   DROP TABLE cta_saldo_viv;

   CREATE TABLE cta_saldo_viv 
     (
       tipo_movimiento   smallint not null ,
       subcuenta         smallint not null ,
       siefore           smallint,
       folio             integer not null ,
       consecutivo_lote  integer,
       nss               char(11) not null ,
       curp              char(18),
       folio_sua         char(6),
       fecha_pago        date,
       fecha_valor       date,
       fecha_conversion  date,
       monto_en_pesos    decimal(22,6),
       monto_en_acciones decimal(22,6),
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
---     )  in cta_dbs1 extent size 100000 next size 10000 lock mode page;



------   Borra tablas temporales    ----------------------------

      DATABASE safre_tmp
         DROP TABLE cta_interes_viv

         CREATE TABLE cta_interes_viv
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

      CREATE index cta_interes_viv_1 on cta_interes_viv
         (nss,subcuenta,fecha_valor,fecha_conversion);


         DROP TABLE dis_cuenta_int

         CREATE TABLE dis_cuenta_int
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


   WHENEVER ERROR STOP

   DATABASE safre_af   
----------------------------------------------------------------

----------   Suebe saldo 888 a cta_saldo_viv   -----------------
----    LET cla_loa = g_param.ruta_envio CLIPPED,"/saldo888.",
----                   fecha_ant1 USING "DD-MM-YYYY"

    LET cla_loa = "/safre_dat/resp888/saldo888.",fecha_ant1 USING "DD-MM-YYYY"

    LOAD FROM cla_loa INSERT INTO cta_saldo_viv


    CREATE INDEX ix_cta_saldo_viv_1 on cta_saldo_viv(nss,subcuenta);

    UPDATE STATISTICS FOR TABLE cta_saldo_viv;

----------------------------------------------------------------

--------   Borra intereses del periodo de dis_cuenta   ---------
   LET cla_del = "DELETE FROM dis_cuenta ",
                 " WHERE fecha_conversion=","'",vvv.fecha_aplica,"'",
                 " AND subcuenta in (4,8) ",
                 " AND tipo_movimiento = 3 " CLIPPED
   PREPARE claexed6 FROM cla_del
   EXECUTE claexed6
----------------------------------------------------------------


--   LET cla_del = "DELETE FROM cta_saldo_prom ",
--                 " WHERE cta_saldo_prom.subcuenta in (4,8) ",
--                 " AND cta_saldo_prom.fecha_aplica=","'",vvv.fecha_aplica,"'" 
--                CLIPPED
--   PREPARE claexed7 FROM cla_del
--   EXECUTE claexed7

    LET cla_loa = "DELETE ",
                  "FROM cta_tasa_viv ",
                  "WHERE fecha_aplica =","'",vvv.fecha_aplica,"'" CLIPPED

    PREPARE claexe FROM cla_loa
    EXECUTE claexe

    ERROR "REVERSO TOTAL TERMINADO..."
    SLEEP 2
    ERROR ""
    CLEAR SCREEN
    CLOSE WINDOW ventana_3

END FUNCTION

FUNCTION Consulta()
   	
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_4 AT 6,3 WITH FORM "DISB0253" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                           (Ctrl-C) Salir     " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                            N.S.S                                              " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON   fecha_proceso,
                               fecha_aplica 
                          FROM fecha_proceso,
                               fecha_aplica
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_4
         RETURN
      END IF

      LET sel_where = "SELECT fecha_proceso,",
                             "fecha_aplica,",
                             "subcuenta,",
                             "nss,",
                             "hora_proceso,",
                             "usuario,",
                             "origen_cod ",
                      " FROM cta_nss_max WHERE ",
                      cla_where CLIPPED,
                      " ORDER BY 1,2 "
  
      PREPARE query FROM sel_where

      DECLARE cursor_1 CURSOR FOR query

      LET pos = 1
      FOREACH cursor_1 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1) 
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (INTERRUPT)
               EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW ventana_4
      ELSE 
         ERROR "REGISTRO DE NSS...NO EXITTE"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_4
      END IF
   END IF
   CLEAR SCREEN
END FUNCTION

FUNCTION Pregunta()
   PROMPT "Estas seguro de reiniciar el proceso S/N ? "
   ATTRIBUTE(REVERSE)
   FOR CHAR aux_pausa 
END FUNCTION

FUNCTION Reinicio_integracion()
   DEFINE
      g_int RECORD
      nss            CHAR(11),
      subcuenta      INTEGER,
      fecha_valor    DATE,
      monto_en_pesos DECIMAl(16,6)
   END RECORD
   DEFINE
      vnss_max      CHAR(11),
      vfecha_max    DATE,
      vhora_max     CHAR(08),
      cla_sql       CHAR(300),
      vmonto_nor    DECIMAL(16,6),
      vmonto_rem    DECIMAL(16,6),
      vid_aportante CHAR(11)

   OPEN WINDOW ventana_21 AT 7,3 WITH FORM "DISB0251" ATTRIBUTE (BORDER)
   DISPLAY " (ESC) Ejecutar                                             (Ctrl-c) Salir     "  AT 1,1 ATTRIBUTE (REVERSE)

   LET int_flag = FALSE
  
   INPUT BY NAME v.*
      AFTER FIELD subcuenta
         IF v.subcuenta IS NULL THEN
            ERROR "La subcuenta no puede ser nula..."
            SLEEP 2
            ERROR ""
            NEXT FIELD subcuetna 
         END IF

      AFTER FIELD fecha_aplica
         IF v.fecha_aplica IS NULL THEN
            ERROR "La fecha no puede ser nula..."
            SLEEP 2
            ERROR ""
            NEXT FIELD fecha_aplica 
         END IF

      ON KEY (ESC)
         IF v.subcuenta IS NULL THEN
            ERROR "La subcuenta no puede ser nula..."
            SLEEP 2
            ERROR ""
            NEXT FIELD subcuetna 
         END IF
         IF v.fecha_aplica IS NULL THEN
            ERROR "La fecha no puede ser nula..."
            SLEEP 2
            ERROR ""
            NEXT FIELD fecha_aplica 
         END IF
         LET int_flag = FALSE
         EXIT INPUT
      ON KEY (Control-c)
         LET int_flag = TRUE
         EXIT INPUT
   END INPUT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      CLEAR FORM
      ERROR "Fecha cancelada..."
      SLEEP 2
      CLEAR SCREEN
      CLOSE WINDOW ventana_21
      RETURN
   END IF

   ERROR "BUSCANDO FECHA ..."
   DECLARE curfec CURSOR FOR
   SELECT "x"
     FROM dis_cuenta
    WHERE subcuenta in (4,8)
      AND tipo_movimiento = 3
      AND fecha_conversion = v.fecha_aplica
   OPEN curfec
   FETCH curfec
      IF STATUS = 100 THEN 
         ERROR "No existe esta fecha para ser integrada"
         SLEEP 3 
         CLEAR SCREEN
         CLOSE WINDOW ventana_21
         CLOSE curfec
         RETURN
      END IF
   CLOSE curfec

   ERROR "BUSCANDO NSS MAXIMO ..."  

   SELECT MAX(nss)
     INTO vnss_max
     FROM safre_tmp:cta_interes_viv
    WHERE subcuenta in (4,8)
      AND tipo_movimiento = 3
      AND fecha_conversion = v.fecha_aplica

   LET hoy = TODAY
   LET hora = TIME

   INSERT INTO cta_nss_max VALUES
      (vnss_max,
      v.subcuenta,
      v.fecha_aplica,
      hoy,
      hora,
      "I",     --origen_cod = Integracion intereses
      usuario)

   DELETE
     FROM dis_cuenta
    WHERE nss = vnss_max
      AND subcuenta in (4,8)
      AND tipo_movimiento = 3
      AND fecha_conversion = v.fecha_aplica

   LET cla_sql = "SELECT nss,",
                        "subcuenta,",
                        "fecha_valor,",
                        "sum(monto_en_pesos)",
                  " FROM cta_interes_viv",
                 " WHERE nss >= ","'",vnss_max,"'",
                 " GROUP BY 1,2,3",
                 " ORDER BY 1,2,3"      
   PREPARE claexe_int FROM cla_sql
   DECLARE curint CURSOR FOR claexe_int

   ERROR "INTEGRANDO INTERESES ..."  

   LET vid_aportante   = "INFONAVIT"

   FOREACH curint INTO g_int.*

      IF g_int.fecha_valor = "04/01/1999" THEN
         LET vmonto_nor = g_int.monto_en_pesos / 46.2096 * 12.7890
         LET vmonto_rem = g_int.monto_en_pesos / 46.2096 * 33.4206
                                                                       
         INSERT INTO dis_cuenta VALUES
            (
            3,                       # tipo_movimiento
            g_int.subcuenta,         # subcuenta
            0,                       # siefore
            88888,                   # folio
            g_int.nss,               # nss
            NULL,                    # curp
            0,                       # folio_sua
            g_int.fecha_valor,       # fecha_pago
            g_int.fecha_valor,       # fecha_valor
            v.fecha_aplica,          # fecha_conversion
            vmonto_nor,              # monto_en_pesos
            0,                       # monto_en_acciones
            0,                       # precio_accion
            0,                       # dias_cotizados
            NULL,                    # sucursal
            "INFONAVIT",             # id_aportante
            5,                       # estado
            TODAY,                   # fecha_proceso
            usuario,                 # usiario
            TODAY,                   # fecha_archivo
            1                        # etiqueta
            )

         INSERT INTO dis_cuenta VALUES
            (
            3,                       # tipo_movimiento
            g_int.subcuenta,         # subcuenta
            0,                       # siefore
            88888,                   # folio
            g_int.nss,               # nss
            NULL,                    # curp
            0,                       # folio_sua
            g_int.fecha_valor,       # fecha_pago
            g_int.fecha_valor,       # fecha_valor
            v.fecha_aplica,          # fecha_conversion
            vmonto_rem,              # monto_en_pesos
            0,                       # monto_en_acciones
            0,                       # precio_accion
            0,                       # dias_cotizados
            NULL,                    # sucursal
            "REMANENTE",             # id_aportante
            5,                       # estado
            TODAY,                   # fecha_proceso
            usuario,                 # usiario
            TODAY,                   # fecha_archivo
            1                        # etiqueta
            ) 
                                                                  
         LET vmonto_nor = 0
         LET vmonto_rem = 0
      ELSE                        
         INSERT INTO dis_cuenta VALUES
            (
            3,                       # tipo_movimiento
            g_int.subcuenta,         # subcuenta
            0,                       # siefore
            88888,                   # folio
            g_int.nss,               # nss
            NULL,                    # curp
            0,                       # folio_sua
            g_int.fecha_valor,       # fecha_pago
            g_int.fecha_valor,       # fecha_valor
            v.fecha_aplica,          # fecha_conversion
            g_int.monto_en_pesos,    # monto_en_pesos
            0,                       # monto_en_acciones
            0,                       # precio_accion
            0,                       # dias_cotizados
            NULL,                    # sucursal
            vid_aportante,           # id_aportante
            5,                       # estado
            TODAY,                   # fecha_proceso
            usuario,                 # usiario
            TODAY,                   # fecha_archivo
            1                        # etiqueta
            ) 
                                                          
        END IF

  END FOREACH

  WHENEVER ERROR CONTINUE  
     DROP TABLE cta_interes_viv;

     CREATE TABLE cta_interes_viv 
       (
         tipo_movimiento integer,
         subcuenta integer,
         siefore smallint,
         folio decimal(10,0),
         nss char(11),
         curp char(18),
         folio_sua char(6),
         fecha_pago date,
         fecha_valor date,
         fecha_conversion date,
         monto_en_pesos decimal(16,6),
         monto_en_acciones decimal(16,6),
         precio_accion decimal(16,6),
         dias_cotizados integer,
         sucursal char(10),
         id_aportante char(11),
         estado smallint,
         fecha_proceso date,
         usuario char(8),
         fecha_archivo date,
         etiqueta integer
       );

     create index cta_interes_viv_2 on cta_interes_viv 
        (nss,subcuenta,fecha_valor);
     
     create index cta_interes_viv_1 on cta_interes_viv 
        (subcuenta,tipo_movimiento,fecha_conversion);

  WHENEVER ERROR STOP

  ERROR ""
  CLEAR SCREEN
  CLOSE WINDOW  ventana_21
END FUNCTION
