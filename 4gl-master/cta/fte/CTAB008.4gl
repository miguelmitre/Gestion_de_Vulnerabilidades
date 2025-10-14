#********************************************************************#
#Proyecto     => safre_af                                            #
#Propietario  => E.F.P.                                              #
#Programa     => CTAB008                                             #
#Descripcion  => ESTADOS DE CUENTA PARA METRIZADOS                   #
#Fecha        => 4 de septiembre de 2003.                            #
#Por          => MIGUEL ANGEL HERNANDEZ MARTINEZ                     #
#Fecha        => 04 de julio de 2005.                                #
#Fecha        => 04 de julio de 2007.                                #
#Fecha        => 08 de agosto de 2008.                               #
#Sistema      => CTA.                                                #
#********************************************************************#
DATABASE  safre_af

GLOBALS
   DEFINE g_reg      RECORD
          nombre_archivo     CHAR(15)
   END RECORD

   DEFINE gparam_dev RECORD LIKE seg_modulo.*

   DEFINE hoy                DATE,
          usuario            CHAR(08),
          opc                CHAR(01),
          ejecuta            CHAR(200),
          hora_inicial       CHAR(08),
          hora_final         CHAR(08),
          vsubcuenta         CHAR(10),
          vtipo_liquidacion  CHAR(10),
          vexceso            CHAR(01),
          cla_where          CHAR(200),
          fecha_1            DATE,
          fecha_2            DATE,
          vtasa_valor        DECIMAL(16,6)

   DEFINE reg RECORD
          nss                CHAR(11),
          nombre_archivo     CHAR(20),
          fecha_inicio       DATE,
          fecha_fin          DATE,
          fecha_inf_ini      DATE,
          fecha_inf_fin      DATE,
          tipo_salida        SMALLINT,
          tipo_informe       SMALLINT,
          tipo_edo_cta       SMALLINT
   END RECORD

   DEFINE sw                 SMALLINT,
          x_hora_inicio      CHAR(8)

   DEFINE f_hoy       DATE,
          dia         SMALLINT,
          mes         SMALLINT,
       xxx_fecha_inicio DATE

   #Cuatrimestre3 2009
   DEFINE gd_fecha_corte DATE,
          gc_mensaje2    CHAR(100)
   DEFINE gs_afore        SMALLINT
END GLOBALS
#####################################################################
MAIN
   DEFINE li_flag   SMALLINT
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
      DEFER INTERRUPT

   SELECT *,
          USER
   INTO   gparam_dev.*,
          usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"

   LET hoy = TODAY

   SELECT codigo_afore
   INTO   gs_afore
   FROM   safre_af:tab_afore_local
   GROUP BY 1

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "CTAB0081" ATTRIBUTE(BORDER)

   DISPLAY " CTAB008                                                                       " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "ESTADOS DE CUENTA"
      COMMAND "Individual" "Estado de cuenta individual"
         CALL proceso(1)
      COMMAND "Final" "Estado de cuenta final por proceso"
         CALL proceso(2)
      COMMAND KEY(E) "cuatrimEstral" "Estado de cuenta cuatrimestral"
         --CALL valida_preparacion(0) RETURNING li_flag
         --IF (li_flag > 0) THEN
            CALL semestral()
         --END IF

      #Cuatrimestre3 2009
      COMMAND KEY(O) "fOlios" "Archivo de folios del estado de cuenta"
         --CALL valida_preparacion(0) RETURNING li_flag
         --IF li_flag > 0 THEN
            CALL folios()
         --END IF

      COMMAND "Salida" "Salida del programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana1
END MAIN
#####################################################################
FUNCTION proceso(x_tipo_proceso)

   DEFINE vfolio          INTEGER,
          x_tipo_proceso  SMALLINT,
          sw              SMALLINT,
          x_sw            SMALLINT,
          xdias           SMALLINT,
          xxx_sw          SMALLINT

   DEFINE x_hoy         DATE,
          xano          SMALLINT

   DEFINE xfecha_min    DATE

   DEFINE x_mes         INTEGER,
          xmes          INTEGER,
          x_ano         INTEGER,
          x_mensaje     CHAR(70),
          hoy2          DATE

   DEFINE li_ok SMALLINT

   LET hoy = TODAY
   LET x_hora_inicio = TIME
   LET xxx_sw = 0

   DISPLAY "                  CARGA DE PARAMETROS DE ESTADO DE CUENTA                      " AT 5,1 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   IF x_tipo_proceso = 1 THEN --Individual
      OPEN WINDOW ventana2 AT 7,2 WITH FORM "CTAB0082"
      LET x_sw = 0

      INPUT BY NAME reg.nss,
                    reg.fecha_inicio,
                    reg.fecha_fin,
                    reg.tipo_salida

         AFTER FIELD nss
            IF reg.nss IS NULL THEN
               CALL archivo_nss()
                    RETURNING sw,
                              x_mensaje

               IF sw = 1 THEN
                  NEXT FIELD nss
               ELSE
                  IF sw = 2 THEN
                     ERROR x_mensaje
                     NEXT FIELD nss
                  ELSE
                     LET x_sw = 1
                     LET reg.nss = "LISTA  NSS"
                     DISPLAY BY NAME reg.nss
                  END IF
               END IF
            END IF

            ERROR ""

            IF sw <> 0 THEN
               SELECT "X"
               FROM   afi_mae_afiliado
               WHERE  n_seguro = reg.nss
               GROUP BY 1

               IF SQLCA.SQLCODE <> 0 THEN
                  ERROR "TRABAJADOR INEXISTENTE, VERIFIQUE NSS"
                  NEXT FIELD nss
               END IF
            END IF

         AFTER FIELD fecha_inicio
            IF reg.fecha_inicio IS NULL THEN
               ERROR "Fecha de inicio de periodo no puede ser nulo.."
               NEXT FIELD fecha_inicio
            END IF

            IF DAY(reg.fecha_inicio) <> 1 THEN
               ERROR "DEBE SER EL PRIMER DIA DEL MES"
               NEXT FIELD fecha_inicio
            END IF

            {CALL valida_rendimientos(reg.fecha_inicio, x_tipo_proceso)
                 RETURNING li_ok

            IF li_ok != 0 THEN
               ERROR "No existen rendimientos para el periodo indicado"
               SLEEP 1
               ERROR ""
               NEXT FIELD fecha_inicio
            END IF            }

        AFTER FIELD fecha_fin
            IF reg.fecha_fin IS NULL THEN
               ERROR "Fecha de fin de periodo no puede ser nulo.."
               NEXT FIELD fecha_fin
            END IF

        AFTER FIELD tipo_salida
            IF reg.tipo_salida IS NULL THEN
               ERROR "Tipo de salida de generacion de estado de cuenta, no puede ser nulo.."
               NEXT FIELD tipo_salida
            END IF

            EXIT INPUT

        ON KEY(INTERRUPT)
            LET INT_FLAG = TRUE
            EXIT INPUT
      END INPUT

      IF INT_FLAG THEN
         LET INT_FLAG = FALSE
         CLEAR FORM
         CLEAR SCREEN
         CLOSE WINDOW ventana2
         RETURN
      END IF
   ELSE --Final
      SELECT "X"
      FROM   cta_ctr_cuenta
      WHERE  tipo_informe > 0
      AND    fecha_informe <= TODAY
      GROUP BY 1

      IF SQLCA.SQLCODE <> 0 THEN
         PROMPT "NO HAY ESTADOS DE CUENTA PENDIENTES POR GENERAR " ATTRIBUTE (REVERSE) FOR opc ATTRIBUTE (REVERSE)
         RETURN
      END IF

      DISPLAY "Tipo de estado de cuenta : " AT 8,12
      DISPLAY "Fecha de inicio de Informe : " AT 11,10
      DISPLAY "Fecha de fin de Informe : " AT 14,13
      DISPLAY "Tipo de salida : " AT 17,22

      INPUT BY NAME reg.tipo_edo_cta,
                    reg.fecha_inf_ini,
                    reg.fecha_inf_fin,
                    reg.tipo_salida

         BEFORE FIELD tipo_edo_cta
         	  IF gs_afore = 564 THEN --MLM
               DISPLAY "Tipo Cta: <3>TRASP <4>INTERNET <5>RETIRO <6>UNIF <9>INDEBIDO" AT 19,1
               DISPLAY "<10>SEPARACION <11>ORIGEN 51 <14>TRANS.Reg73 <15>TRANS.Reg97" AT 20,1
            ELSE
            	 DISPLAY " Tipo Cta: <3>TRANSPASO <4>INTERNET <5>RETIRO <6>UNIFICACION" AT 19,1
               DISPLAY "           <9>INDEBIDO <10>SEPARACION" AT 20,1

               DISPLAY " Tipo Cta: <3>TRANSPASO <4>INTERNET <5>RETIRO <6>UNIFICACION" AT 19,1
               DISPLAY " <9>INDEBIDO <10>SEPARACION  <14>TRANS.Reg73 <15>TRANS.Reg97" AT 20,1
            END IF

         AFTER FIELD tipo_edo_cta
               DISPLAY "                                                            " AT 19,1
               DISPLAY "                                                            " AT 20,1

            IF reg.tipo_edo_cta IS NULL THEN
               ERROR "Tipo estado de cuenta no puede ser nulo "
               NEXT FIELD tipo_edo_cta
            END IF

         AFTER FIELD fecha_inf_ini
            LET reg.fecha_inf_fin = TODAY

            DISPLAY BY NAME reg.fecha_inf_fin

            IF reg.fecha_inf_ini IS NULL THEN
               ERROR "Fecha de inicio no puede ser nula "
               NEXT FIELD fecha_inf_ini
            END IF

            SELECT MIN(fecha_valuacion)
            INTO   xfecha_min
            FROM   glo_valor_accion

            IF reg.fecha_inf_ini < xfecha_min THEN
               ERROR "La afore opero a partir de la fecha: ",
               xfecha_min USING "DD/MM/YYYY"
               NEXT FIELD fecha_inf_ini
            END IF

            IF reg.fecha_inf_ini > TODAY THEN
               ERROR "La fecha de inicio no puede ser mayor a hoy"
               NEXT FIELD fecha_inf_ini
            END IF

            {CALL valida_rendimientos(reg.fecha_inf_ini, reg.tipo_edo_cta)
                 RETURNING li_ok

            IF li_ok != 0 THEN
               ERROR "No existen rendimientos para el periodo indicado"
               SLEEP 1
               ERROR ""
               NEXT FIELD fecha_inf_ini
            END IF   }

         AFTER FIELD fecha_inf_fin
            IF reg.fecha_inf_fin > TODAY THEN
               ERROR "La fecha de fin de periodo no puede ser mayor a hoy"
               NEXT FIELD fecha_inf_fin
            END IF

            IF reg.fecha_inf_fin < reg.fecha_inf_ini THEN
               ERROR "La fecha final no puede ser menor a la fecha inicial"
               NEXT FIELD fecha_inf_fin
            END IF

         AFTER FIELD tipo_salida
            IF reg.tipo_salida IS NULL THEN
               ERROR "Tipo de salida de generacion de estado de cuenta, no puede ser nulo.."
               NEXT FIELD tipo_salida
            END IF

            EXIT INPUT

         ON KEY(INTERRUPT)
            DISPLAY "                          " AT 8,12
            DISPLAY "                            " AT 11,10
            DISPLAY "                         " AT 14,13
            DISPLAY "                 " AT 17,22
            LET INT_FLAG = TRUE
            EXIT INPUT
      END INPUT

      IF INT_FLAG THEN
         LET INT_FLAG = FALSE
         CLEAR FORM
         CLEAR SCREEN
         RETURN
      END IF
   END IF

   --Acaba lectura de parámetros

   PROMPT "Desea ejecutar el proceso [S/N] ..." ATTRIBUTE (REVERSE)
           FOR opc ATTRIBUTE (REVERSE)

   IF opc MATCHES '[Ss]' THEN

      IF reg.tipo_salida = 1 THEN --Archivo
      	    --Asigna folio
            SELECT "X"
            FROM cta_folio

            IF SQLCA.SQLCODE = 0 THEN
               SELECT folio + 1
               INTO   vfolio
               FROM   cta_folio

               UPDATE cta_folio
               SET    folio = vfolio
            ELSE
               LET vfolio = 1

               INSERT INTO cta_folio
               VALUES (vfolio)
            END IF
      END IF

      IF x_sw = 1 THEN --Individual Lista Nss
         DECLARE cur_nss_tmp CURSOR FOR
         SELECT nss
         FROM   cta_ctr_nss

         FOREACH cur_nss_tmp INTO reg.nss

            IF x_tipo_proceso = 1 THEN
               ----traspaso tipo_informe = 3
               SELECT "X"
               FROM   cta_act_marca
               WHERE  nss = reg.nss
               AND    marca_cod = 120

               IF SQLCA.SQLCODE = 0 THEN
                  LET reg.tipo_informe = 3
               ELSE
                  ----traspaso tipo_informe = 6
                  SELECT "X"
                  FROM   cta_act_marca
                  WHERE  nss = reg.nss
                  AND    marca_cod = 130

                  IF SQLCA.SQLCODE = 0 THEN
                     LET reg.tipo_informe = 6
                  ELSE
                     ----traspaso tipo_informe = 5
                     SELECT "X"
                     FROM   cta_act_marca
                     WHERE  nss = reg.nss
                     AND    marca_cod = 140

                     IF SQLCA.SQLCODE = 0 THEN
                        LET reg.tipo_informe = 5
                     ELSE
                        LET reg.tipo_informe = 1
                     END IF
                  END IF
               END IF
            END IF

            INSERT INTO cta_ctr_proceso
            VALUES (vfolio,              -- folio
                    x_tipo_proceso,      -- tipo_edo
                    reg.nss,             -- nss
                    reg.fecha_inicio,    -- fecha_inicio
                    reg.fecha_fin,       -- fecha_fin
                    x_hora_inicio,       -- hora_inicio
                    "",                  -- hora_fin
                    reg.tipo_salida,     -- tipo_salida
                    reg.tipo_informe,    -- tipo_informe
                    1,                   -- estado
                    TODAY,               -- factualiza
                    USER                 -- usuario
                   )
         END FOREACH
      ELSE
         IF x_tipo_proceso = 2 THEN --Final
         	  #Validar que existan datos
            IF reg.tipo_edo_cta = 14 OR   --regimen 73
            	 reg.tipo_edo_cta = 15 THEN --regimen 97

            	 SELECT 'X'
               FROM   cta_ctr_proceso
               WHERE  tipo_informe = reg.tipo_edo_cta
               AND    estado       = 1
               AND    folio        IS NULL
               AND    fecha_fin BETWEEN reg.fecha_inf_ini AND reg.fecha_inf_fin
               GROUP BY 1

               IF SQLCA.SQLCODE <> 0 THEN
                  ERROR "NO HAY ESTADOS DE CUENTA DE TRANSFERENCIAS POR GENERAR"
                  SLEEP 2
                  ERROR ""
                  RETURN
               ELSE
               	  --Agruparlos en un solo folio
               	  UPDATE cta_ctr_proceso
               	  SET    folio = vfolio
               	  WHERE  tipo_informe = reg.tipo_edo_cta
               	  AND    estado       = 1
               	  AND    folio        IS NULL
                  AND    fecha_fin BETWEEN reg.fecha_inf_ini AND reg.fecha_inf_fin
               END IF
            ELSE
            	 #El resto de los finales que generas EDC normales

               LET f_hoy     = hoy
               LET dia       = DAY(f_hoy)
               LET mes       = MONTH(f_hoy)-1

               LET dia = dia - 1
               LET reg.fecha_inicio  = f_hoy - dia UNITS DAY
               LET reg.fecha_inicio  = reg.fecha_inicio - mes UNITS MONTH

               WHENEVER ERROR CONTINUE
                  DROP TABLE cta_ctr_cuenta_nss
               WHENEVER ERROR STOP

               CREATE TEMP TABLE cta_ctr_cuenta_nss
                  (nss           CHAR(11),
                   fecha_informe DATE,
                   tipo_informe  SMALLINT
                  )

               --Carga tabla con las cuentas marcadas por otros procesos
               --en cta_ctr_cuenta con tipo_informe = capturado
               INSERT INTO cta_ctr_cuenta_nss
               SELECT nss,
                      fecha_informe,
                      tipo_informe
               FROM   cta_ctr_cuenta
               WHERE  tipo_informe = reg.tipo_edo_cta
               AND    fecha_informe BETWEEN reg.fecha_inf_ini AND reg.fecha_inf_fin

               CREATE INDEX cuenta_nss_1 ON cta_ctr_cuenta_nss (nss)

               UPDATE STATISTICS FOR TABLE cta_ctr_cuenta_nss

               DECLARE cur_cuenta_nss CURSOR FOR
               SELECT *
               FROM   cta_ctr_cuenta_nss


               FOREACH cur_cuenta_nss INTO reg.nss,
                                           reg.fecha_fin,
                                           reg.tipo_informe

                  LET f_hoy     = reg.fecha_fin
                  LET dia       = DAY(f_hoy)
                  LET mes       = MONTH(f_hoy)-1

                  LET dia = dia - 1
                  LET reg.fecha_inicio  = f_hoy - dia UNITS DAY
                  LET reg.fecha_inicio  = reg.fecha_inicio - mes UNITS MONTH

                  LET xmes = MONTH(reg.fecha_fin)

                  IF xmes >= 7 THEN
                     LET xxx_fecha_inicio = reg.fecha_inicio + 6 UNITS MONTH
                  ELSE
                     LET xxx_fecha_inicio = reg.fecha_inicio
                  END IF

                  --Inserta y marca los registros a generar
                  INSERT INTO cta_ctr_proceso
                  VALUES (vfolio,              -- folio
                          x_tipo_proceso,      -- tipo_edo
                          reg.nss,             -- nss
                          xxx_fecha_inicio,    -- fecha_inicio
                          reg.fecha_fin,       -- fecha_fin
                          x_hora_inicio,       -- hora_inicio
                          "",                  -- hora_fin
                          reg.tipo_salida,     -- tipo_salida
                          reg.tipo_informe,    -- tipo_informe
                          1,                   -- estado
                          TODAY,               -- factualiza
                          USER                 -- usuario
                        )

                  UPDATE cta_ctr_cuenta
                  SET    tipo_informe = 0,
                         fecha_informe = hoy
                  WHERE  nss = reg.nss

                  LET xxx_fecha_inicio = NULL
                  LET xmes = 0
               END FOREACH
            END IF
         ELSE
            IF x_tipo_proceso = 1 THEN
               ----traspaso tipo_informe = 3
               SELECT "X"
               FROM   cta_act_marca
               WHERE  nss = reg.nss
               AND    marca_cod = 120

               IF SQLCA.SQLCODE = 0 THEN
                  LET reg.tipo_informe = 3
               ELSE
                  ----traspaso tipo_informe = 6
                  SELECT "X"
                  FROM   cta_act_marca
                  WHERE  nss = reg.nss
                  AND    marca_cod = 130

                  IF SQLCA.SQLCODE = 0 THEN
                     LET reg.tipo_informe = 6
                  ELSE
                     ----traspaso tipo_informe = 5
                     SELECT "X"
                     FROM   cta_act_marca
                     WHERE  nss = reg.nss
                     AND    marca_cod = 140

                     IF SQLCA.SQLCODE = 0 THEN
                        LET reg.tipo_informe = 5
                     ELSE
                        LET reg.tipo_informe = 1
                     END IF
                  END IF
               END IF
            END IF

            INSERT INTO cta_ctr_proceso
            VALUES (vfolio,              -- folio
                    x_tipo_proceso,      -- tipo_edo
                    reg.nss,             -- nss
                    reg.fecha_inicio,    -- fecha_inicio
                    reg.fecha_fin,       -- fecha_fin
                    x_hora_inicio,       -- hora_inicio
                    "",                  -- hora_fin
                    reg.tipo_salida,     -- tipo_salida
                    reg.tipo_informe,    -- tipo_informe
                    1,                   -- estado
                    TODAY,               -- factualiza
                    USER                 -- usuario
                   )
         END IF
      END IF

      IF x_tipo_proceso = 2 THEN
         LET xxx_sw = 1
      ELSE
         IF x_tipo_proceso = 1 AND reg.tipo_salida = 2 THEN
            LET xxx_sw = 1
         ELSE
            IF x_tipo_proceso = 3 AND reg.tipo_salida = 2 THEN
               LET xxx_sw = 1
            ELSE
               LET xxx_sw = 0
            END IF
         END IF
      END IF

-- tem miguel      IF xxx_sw = 1 THEN

         CALL ejecuta_lectura(x_tipo_proceso,vfolio)
         ERROR "Ejecutando Lectura y Proceso de Archivo por nohup ..."
         SLEEP 1

         #Verificar que programa se va a ejecutar
         IF reg.tipo_edo_cta = 14 OR   --regimen 73
         	  reg.tipo_edo_cta = 15 THEN --regimen 97

         	  LET ejecuta = "nohup time fglgo RETL836.4gi ",
                           vfolio CLIPPED," ",
                           "1> ", "RETL836.salida ",
                           "2> ", "RETL836.error &"
         ELSE
            LET ejecuta = "nohup time fglgo CTAB0404.4gi " CLIPPED,
                           x_tipo_proceso CLIPPED ," ",
                           vfolio CLIPPED," &"
         END IF

         RUN ejecuta

         ERROR "El proceso se ejecuto satisfactoriamente por nohup ..."
         SLEEP 2

   ELSE
      ERROR "PROCESO CANCELADO .."
   END IF

   IF x_tipo_proceso <> 2 THEN
      CLOSE WINDOW ventana2
   END IF

   CLEAR FORM
   CLEAR SCREEN
   RETURN
END FUNCTION
#####################################################################
FUNCTION archivo_nss()

   DEFINE x_nombre_archivo             CHAR(20),
          ejecuta                      CHAR(200),
          x_mensaje                    CHAR(70),
          x_nss                        CHAR(11),
          opc                          CHAR(1)

   OPEN WINDOW ventana3 AT 8,18 WITH FORM "CTAB0083" ATTRIBUTE(BORDER)
   DISPLAY " <ESC> Carga              <Ctrl-C> Salir " AT 1,1 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   INPUT BY NAME reg.nombre_archivo

      AFTER FIELD nombre_archivo
         IF reg.nombre_archivo IS NULL THEN
            ERROR "NOMBRE DE ARCHIVO NO PUEDE SER NULO .."
            NEXT FIELD nombre_archivo
         END IF
         ERROR ""

      ON KEY (ESC)
         IF reg.nombre_archivo IS NULL THEN
            ERROR "NOMBRE DE ARCHIVO NO PUEDE SER NULO .."
            NEXT FIELD nombre_archivo
         END IF
         ERROR ""

         EXIT INPUT

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      CLOSE WINDOW ventana3
      LET x_mensaje = ""
      RETURN 1,x_mensaje
   END IF

   ERROR "PROCESANDO INFORMACION ..."

   LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                 "; ls > archivos" CLIPPED
   RUN ejecuta

   WHENEVER ERROR CONTINUE
      DROP TABLE archivos_cta
   WHENEVER ERROR STOP

   CREATE TEMP TABLE archivos_cta
      (campo  CHAR(100))

   LET ejecuta = gparam_dev.ruta_rescate CLIPPED,"/archivos" CLIPPED

   LOAD FROM ejecuta INSERT INTO archivos_cta

   LET x_mensaje = ""

   SELECT "X"
   FROM   archivos_cta
   WHERE  campo = reg.nombre_archivo

   IF SQLCA.SQLCODE <> 0 THEN
      PROMPT "NOMBRE DE ARCHIVO INCORRECTO" FOR opc

      CLEAR FORM
      CLEAR SCREEN

      LET sw = 1
   ELSE
      WHENEVER ERROR CONTINUE
         DROP TABLE cta_ctr_nss
      WHENEVER ERROR STOP

      CREATE TEMP TABLE cta_ctr_nss
         (nss  CHAR(11))

      LET ejecuta = gparam_dev.ruta_rescate CLIPPED,"/",reg.nombre_archivo

      LOAD FROM ejecuta INSERT INTO cta_ctr_nss

      LET sw = 0
{ -- tem miguel
      DECLARE cur_ctr_nss CURSOR FOR
      SELECT nss
      FROM   cta_ctr_nss

      FOREACH cur_ctr_nss INTO x_nss
         SELECT "X"
         FROM   afi_mae_afiliado
         WHERE  n_seguro = x_nss
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            LET  x_mensaje  = "TRABAJADOR INEXISTENTE, NSS: ",
                 x_nss CLIPPED,",VERIFIQUE LISTA" CLIPPED
            LET sw = 2
            EXIT FOREACH
         END IF
      END FOREACH
} -- tem miguel
   END IF

   LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                 "; rm archivos "
   RUN ejecuta

   CLOSE WINDOW ventana3

   RETURN sw,
          x_mensaje

END FUNCTION
#####################################################################
FUNCTION consulta()

   DEFINE reg_2  ARRAY[500] OF RECORD
          folio               INTEGER,
          tipo_informe        SMALLINT,
          descripcion         CHAR(40),
          fecha_inicio        DATE,
          fecha_fin           DATE
   END RECORD

   DEFINE pos                 SMALLINT,
          xx_folio_dia        INTEGER

   LET x_hora_inicio = TIME

   OPEN WINDOW ventana4 AT 5,2 WITH FORM "CTAB0084" ATTRIBUTE(BORDER)
   DISPLAY " <ESC> Proceso                                               <Ctrl-C> Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "                        RESUMEN DE ESTADO DE CUENTA DEL NSS                 " AT 5,1 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   INPUT BY NAME reg.nss

      AFTER FIELD nss
         IF reg.nss IS NULL THEN
            ERROR "NSS NO PUEDE SER NULO .."
            NEXT FIELD nss
         END IF
         ERROR ""

      ON KEY (ESC)
         IF reg.nss IS NULL THEN
            ERROR "NSS NO PUEDE SER NULO .."
            NEXT FIELD nss
         END IF
         ERROR ""

         EXIT INPUT

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      CLEAR SCREEN
      CLOSE WINDOW ventana4
      RETURN
   END IF

   ERROR "PROCESANDO INFORMACION ..."

   DECLARE cur_consulta CURSOR FOR
   SELECT a.folio,
          a.tipo_informe,
          b.descripcion,
          a.fecha_inicio,
          a.fecha_fin
   FROM   cta_ctr_proceso a , OUTER tab_tipo_informe b
   WHERE  a.nss = reg.nss
   AND    a.estado = 3
   AND    b.tipo_informe = a.tipo_informe
   ORDER BY 3,4

   LET pos = 1

   FOREACH cur_consulta INTO reg_2[pos].*
      LET pos = pos + 1
   END FOREACH

   INITIALIZE reg_2[pos].* TO NULL

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      DISPLAY ARRAY reg_2 TO scr_1.*
         ON KEY (CONTROL-M)
            LET pos = ARR_CURR()

            LET xx_folio_dia  = 0

            SELECT MAX(folio)
            INTO   xx_folio_dia
            FROM   cta_ctr_proceso
            WHERE  tipo_salida = 2
            AND    estado = 1

            IF xx_folio_dia = 0 OR
               xx_folio_dia IS NULL THEN

               SELECT "X"
               FROM cta_folio

               IF SQLCA.SQLCODE = 0 THEN
                  SELECT folio + 1
                  INTO   xx_folio_dia
                  FROM   cta_folio

                  UPDATE cta_folio
                  SET    folio = xx_folio_dia
               ELSE
                  LET xx_folio_dia = 1

                  INSERT INTO cta_folio
                  VALUES (xx_folio_dia)
               END IF
            END IF

            INSERT INTO cta_ctr_proceso
            VALUES (xx_folio_dia,             -- folio
                    4,                        -- tipo_edo
                    reg.nss,                  -- nss
                    reg_2[pos].fecha_inicio,  -- fecha_inicio
                    reg_2[pos].fecha_fin,     -- fecha_fin
                    x_hora_inicio,            -- hora_inicio
                    "",                       -- hora_fin
                    2,                        -- tipo_salida
                    reg_2[pos].tipo_informe,  -- tipo_informe
                    1,                        -- estado
                    TODAY,                    -- factualiza
                    USER                      -- usuario
                   )

            PROMPT "REGISTRO INGRESADO, PARA REIMPRESION " ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE(REVERSE)

            LET ejecuta="nohup time fglgo CTAB0404.4gi " CLIPPED,
                         4 ," ",
                         xx_folio_dia ," &"
            RUN ejecuta

            EXIT DISPLAY
         ON KEY (INTERRUPT)
            ERROR "SELECCION CANCELADA .."
            SLEEP 3
            ERROR ""
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "NSS NO TIENE REGISTROS "
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana4
      RETURN
   END IF

      CLEAR SCREEN
      CLOSE WINDOW ventana4
      RETURN
END FUNCTION
#####################################################################
FUNCTION calcula_dias(fecha)
#cd------------------------
    DEFINE
        fecha   DATE

    CASE MONTH(fecha)            #Regresa dias del mes anterior
        WHEN  1   RETURN 31
        WHEN  2   IF YEAR(fecha) MOD 4 = 0 THEN
                      RETURN 29
                  ELSE
                      RETURN 28
                  END IF
        WHEN  3   RETURN 31
        WHEN  4   RETURN 30
        WHEN  5   RETURN 31
        WHEN  6   RETURN 30
        WHEN  7   RETURN 31
        WHEN  8   RETURN 31
        WHEN  9   RETURN 30
        WHEN 10   RETURN 31
        WHEN 11   RETURN 30
        WHEN 12   RETURN 31
    END CASE

END FUNCTION #calcula_dias
##############################################################################
FUNCTION ejecuta_lectura(x_tipo_proceso,x_folio_dia)

   DEFINE hora_inicial       CHAR(08),
          hora_final         CHAR(08),
          x_folio_dia        INTEGER,
          x_tipo_proceso     SMALLINT

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso
   VALUES (TODAY,                   -- fecha_proceso
           "CTA",                   -- proceso_cod
           1,                       -- etapa_cod   -- LECTURA
           hora_inicial,            -- hora_inicial
           hora_final,              -- hora_final
           x_tipo_proceso,          -- parametro1
           NULL,                    -- parametro2
           NULL,                    -- parametro3
           NULL,                    -- parametro4
           NULL,                    -- parametro5
           x_folio_dia,             -- folio
           NULL,                    -- resultado
           USER,                    -- usuario
           0                        -- consecutivo
          )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Lectura Archivo",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

END FUNCTION
#*********************************************************************
FUNCTION semestral()

   DEFINE vestado     SMALLINT,
          opc         CHAR(1) ,
          fecha_corte DATE    ,
          ls_mes      SMALLINT

   OPEN WINDOW ventana11 AT 5,2 WITH FORM "CTAB0085" ATTRIBUTE(BORDER)

   DISPLAY "             GENERACION DE ESTADOS DE CUENTA CUATRIMESTRAL            " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)
   
   LET ls_mes = MONTH(hoy)

   INPUT BY NAME fecha_corte,vestado,opc WITHOUT DEFAULTS

   	  BEFORE FIELD fecha_corte
   	  	 IF ls_mes <= 4 THEN
         	  LET fecha_corte = MDY (12,31, YEAR(TODAY) - 1)
         ELSE
            IF ls_mes > 4 AND ls_mes <= 8 THEN
            	  LET fecha_corte = MDY (4,30, YEAR(TODAY))
            ELSE
            	  IF ls_mes > 8 AND ls_mes <= 12 THEN
            	  	 LET fecha_corte = MDY (8,31, YEAR(TODAY))
            	  END IF
            END IF
         END IF

         DISPLAY BY NAME fecha_corte

   	  AFTER FIELD fecha_corte
   	  	 IF fecha_corte IS NULL THEN
   	  	 	  ERROR "Debe indicar fecha de corte"
   	  	 	  SLEEP 2
   	  	 	  ERROR ""
            NEXT FIELD fecha_corte
         ELSE
         	  #Validar fecha valida
         	  IF (MONTH (fecha_corte) = 4 AND DAY(fecha_corte) = 30) OR
         	  	 (MONTH (fecha_corte) = 8 AND DAY(fecha_corte) = 31) OR
         	  	 (MONTH (fecha_corte) = 12 AND DAY(fecha_corte) = 31) THEN
         	  ELSE
         	  	 ERROR "Debe indicar una fecha de corte cuatrimestral valida"
   	  	 	     SLEEP 2
   	  	 	     ERROR ""
               NEXT FIELD fecha_corte
         	  END IF
         END IF

      AFTER FIELD vestado
         IF STATUS IS NULL THEN
            ERROR "Debe ingresar un estado"
            NEXT FIELD vestado
         END IF

         {SELECT "X"
         FROM   cta_nss_ctr a
         WHERE  a.estado = vestado
         AND    a.fecha_generacion = "04/30/2009"
         GROUP BY 1

         IF STATUS <> NOTFOUND THEN
            ERROR "Este ESTADO ya fue emitido"
            NEXT FIELD vestado
         END IF}

      AFTER FIELD opc

         IF STATUS IS NULL THEN
            ERROR "Debe poner alguna opcion a ejecutar"
            NEXT FIELD opc
         END IF
         IF opc MATCHES '[Ss]' THEN
            ERROR "Procesando informacion ..."

            CALL envia_batch(vestado, fecha_corte)
         END IF
         EXIT INPUT
      ON KEY(INTERRUPT)
      ERROR "PROCESO CANCELADO"
      SLEEP 2
      EXIT PROGRAM


   END INPUT

   CLEAR FORM
   CLEAR SCREEN
   EXIT PROGRAM
END FUNCTION
######################################################################
FUNCTION envia_batch(x_estado, ld_fecha_corte)

   DEFINE xx_folio_dia        INTEGER,
          x_tipo_proceso      SMALLINT,
          x_estado            SMALLINT,
          ld_fecha_corte      DATE

   LET x_tipo_proceso = 0

   SELECT "X"
   FROM cta_folio

   IF SQLCA.SQLCODE = 0 THEN
      SELECT folio + 1
      INTO   xx_folio_dia
      FROM   cta_folio

      UPDATE cta_folio
      SET    folio = xx_folio_dia
   ELSE
      LET xx_folio_dia = 1

      INSERT INTO cta_folio
      VALUES (xx_folio_dia)
   END IF

   CALL ejecuta_lectura(x_tipo_proceso,xx_folio_dia)

   ERROR "Ejecutando Lectura y Proceso de Archivo por nohup ..."
   SLEEP 2
   ERROR ""

   LET ejecuta="nohup time fglgo CTAB0404.4gi " CLIPPED,
                   x_tipo_proceso CLIPPED, " ",
                   xx_folio_dia   CLIPPED, " ",
                   x_estado       CLIPPED, " ",
                   ld_fecha_corte        , " &"

   RUN ejecuta

   ERROR "El proceso se ejecuto satisfactoriamente por nohup ..."
   SLEEP 2
   ERROR ""

END FUNCTION
################################################################################
FUNCTION valida_preparacion(li_tipo_prueba)
  DEFINE ld_hoy           DATE,
         ld_sem_ini       DATE,
         ld_sem_fin       DATE,
         arr_cta_ctr_edocta   ARRAY[7] OF RECORD
            fecha_corte       DATE,
            paso              SMALLINT,
            fecha_ini         DATETIME  YEAR TO SECOND,
            fecha_fin         DATETIME  YEAR TO SECOND,
            usuario           CHAR(8),
            fecha_proceso     DATE
         END RECORD,

         li_cont              SMALLINT,
         li_pos               SMALLINT,
         li_tipo_prueba       SMALLINT,
         li_ok                SMALLINT,
         lc_message           CHAR(500),
         lc_ok                CHAR(1),
         lc_pasos_faltantes   CHAR(5),
         lc_linea             SMALLINT

  #Cuatrimestre1 2009
  DEFINE li_mes SMALLINT

  --Inicializar
  LET li_ok = 0
  LET hoy = TODAY
  LET li_cont = 1
  LET lc_linea = 15
  INITIALIZE arr_cta_ctr_edocta TO NULL
  INITIALIZE lc_message TO NULL

  --FECHAS DEL PERIODO CORRESPONDIENTE
  #Cuatrimestre1 2009
  LET li_mes = MONTH(hoy)

  IF li_mes <= 4 THEN
  	  LET ld_sem_fin = MDY (12,31, YEAR(TODAY) - 1)
  ELSE
     IF li_mes > 4 AND li_mes <= 8 THEN
     	  LET ld_sem_fin = MDY (4,30, YEAR(TODAY))
     ELSE
     	  IF li_mes > 8 AND li_mes <= 12 THEN
     	  	 LET ld_sem_fin = MDY (8,31, YEAR(TODAY))
     	  END IF
     END IF
  END IF

  {IF MONTH(hoy) > 6 THEN
     LET ld_sem_ini = MDY(01,01,YEAR(hoy))
     LET ld_sem_fin = MDY(06,30,YEAR(hoy))
  ELSE
     LET ld_sem_ini = MDY(07,01,YEAR(hoy)-1)
     LET ld_sem_fin = MDY(12,31,YEAR(hoy)-1)
  END IF}

  DECLARE cur_val_prep CURSOR FOR
    SELECT *
    FROM   safre_tmp:cta_ctr_edocta
    WHERE  fecha_corte = ld_sem_fin
    --WHERE  DATE (fecha_ini) = ld_sem_ini
    --AND    DATE (fecha_fin) = ld_sem_fin
    ORDER BY paso

  --LET lc_message = "Preparación Incompleta para el semestre" CLIPPED, ASCII 10

  FOREACH cur_val_prep INTO arr_cta_ctr_edocta[li_cont].*
     IF (arr_cta_ctr_edocta[li_cont].fecha_fin IS NULL) THEN
        LET lc_message =    "El paso " CLIPPED,
                            arr_cta_ctr_edocta[li_cont].paso,
                            " no ha concluido" CLIPPED
        DISPLAY lc_message CLIPPED AT lc_linea,1 ATTRIBUTE (REVERSE)
        LET lc_linea = lc_linea + 1
     END IF
     LET li_cont = li_cont + 1
  END FOREACH

  LET lc_pasos_faltantes = "xxxxxx"

  FOR li_pos = 1 TO 5
     CASE arr_cta_ctr_edocta[li_pos].paso
       WHEN 1
         LET lc_pasos_faltantes[1] = '1'
       WHEN 2
         LET lc_pasos_faltantes[2] = '2'
       WHEN 3
         LET lc_pasos_faltantes[3] = '3'
       WHEN 4
         LET lc_pasos_faltantes[4] = '4'
       WHEN 5
         LET lc_pasos_faltantes[5] = '5'
       WHEN 6
         LET lc_pasos_faltantes[6] = '6'
     END CASE
  END FOR

  FOR li_pos = 1 TO 5
     IF lc_pasos_faltantes[li_pos] = 'x' THEN
        LET lc_message = "Falta ejecutar el paso: ", li_pos
        DISPLAY lc_message CLIPPED AT lc_linea,1 ATTRIBUTE (REVERSE)
        LET lc_linea = lc_linea + 1
     END IF
  END FOR

  IF (lc_message IS NOT NULL) THEN
     --PROMPT lc_message CLIPPED FOR CHAR lc_ok
     DISPLAY "Preparación Incompleta para el semestre" AT lc_linea,1 ATTRIBUTE (REVERSE)
     PROMPT "Presione <Enter> para continuar" CLIPPED FOR CHAR lc_ok ATTRIBUTE (REVERSE)
     CALL limpia_msg()
     RETURN 0
  ELSE
     RETURN 1
  END IF
END FUNCTION
################################################################################
FUNCTION limpia_msg()
   DEFINE li_linea  SMALLINT
   FOR li_linea = 15 TO 20
      DISPLAY "                                                               "
      AT li_linea,1
   END FOR
END FUNCTION
################################################################################
FUNCTION valida_rendimientos(ld_fproceso, li_tipo_informe)
   DEFINE li_tipo_informe,
          li_folio          SMALLINT

   DEFINE ld_fproceso,
          ld_frendimiento   DATE,
          li_ok             SMALLINT

--DISPLAY "li_tipo_informe:", li_tipo_informe AT 4,1
--DISPLAY "li_folio:", li_folio AT 5,1
--DISPLAY "ld_fproceso:", ld_fproceso AT 7,1
--PROMPT "<Enter>" ATTRIBUTE (REVERSE) FOR opc

   IF MONTH(ld_fproceso) > 6 THEN
      LET ld_frendimiento = MDY(06,30,YEAR(ld_fproceso))
   ELSE
      LET ld_frendimiento = MDY(12,31,YEAR(ld_fproceso)-1)
   END IF

   SELECT "X"
   FROM   cta_rendimiento_ctr a,
          cta_param_ctr b
   WHERE  a.folio_rendimiento = b.folio_rendimiento
   AND    b.fecha_fin         = ld_frendimiento
   AND    b.tipo_informe      = li_tipo_informe
   GROUP BY 1

   IF SQLCA.SQLCODE = 0 THEN
      LET li_ok = 0
   ELSE
      LET li_ok = 1
   END IF

   RETURN li_ok

END FUNCTION
################################################################################
#Cuatrimestre3 2009
FUNCTION folios()
   DEFINE ls_flag SMALLINT
   DEFINE lar_estados ARRAY[1001] OF RECORD
   	      marca       CHAR(01),
          estado      SMALLINT,
          estado_desc CHAR(49),
          total       INTEGER
   END RECORD

   DEFINE ls_procesar,
          ls_cont    ,
          ls_inserta ,
          ls_estado  SMALLINT

   INITIALIZE gd_fecha_corte TO NULL
   OPEN WINDOW ventana_12 AT 5,2 WITH FORM "CTAB0086" ATTRIBUTE(BORDER)

   DISPLAY " GENERACION DE ARCHIVO DE FOLIOS DEL ESTADO DE CUENTA CUATRIMESTRAL " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 1,69 ATTRIBUTE(REVERSE)

   CALL captura_fecha() RETURNING ls_flag
   LET ls_inserta = 0

   IF ls_flag = 1 THEN
      ERROR "PROCESO CANCELADO..."
      SLEEP 2
      ERROR ""
      CLOSE WINDOW ventana_12
      RETURN
   END IF

   DECLARE cur_estados CURSOR FOR
   SELECT ' '         ,
          a.estado    ,
          b.estad_desc,
          COUNT(a.nss)
   FROM   cta_nss_edo_cta  a,
          OUTER tab_estado b
   WHERE  a.estado = b.estad_cod
   GROUP BY 1,2,3
   ORDER BY 1,2,3

   ERROR "PROCESANDO INFORMACION"

   FOR ls_cont = 1 TO 1001
   	  INITIALIZE lar_estados[ls_cont].* TO NULL
   END FOR

   LET ls_cont = 1
   FOREACH cur_estados INTO lar_estados[ls_cont].*
   	  IF ls_cont > 1000 THEN
   	  	 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
         SLEEP 2
         ERROR ""
         EXIT FOREACH
   	  END IF

   	  LET ls_cont = ls_cont + 1
   END FOREACH
   LET ls_cont = ls_cont - 1

   IF ls_cont <= 0 THEN
   	  ERROR "NO EXISTE INFORMACION CON LOS CRITERIOS INDICADOS..."
      SLEEP 2
      ERROR ""
      CLOSE WINDOW ventana_12
      RETURN
   END IF

   ERROR ""
   DISPLAY " <ESC> GENERAR ARCHIVO                                          <CTRL-C> SALIR" AT 2,1 ATTRIBUTE(REVERSE)

   LET ls_flag = 0

   DISPLAY "       ESTADO                                            TOTAL DE TRABAJADORES" AT 5,1 ATTRIBUTE(REVERSE)

   INPUT ARRAY lar_estados WITHOUT DEFAULTS FROM scr_1.*
   ATTRIBUTES(MAXCOUNT = ls_cont,COUNT = ls_cont)
      ON KEY(CONTROL-C,INTERRUPT)
         LET ls_flag = 1
         EXIT INPUT

      ON KEY(esc)
      	 FOR ls_procesar = 1 TO ls_cont
         	  IF lar_estados[ls_procesar].marca = 'X' THEN
         	  	 LET ls_inserta = 1
         	  	 EXIT FOR
         	  END IF
         END FOR

         IF ls_inserta = 0 THEN
         	  ERROR "DEBE MARCAR AL MENOS UN REGISTRO"
         	  SLEEP 2
            ERROR ""
         	  NEXT FIELD marca
         END IF

      	 LET ls_flag = 0
         EXIT INPUT
   END INPUT

   IF ls_flag = 1 THEN
      ERROR "PROCESO CANCELADO..."
      SLEEP 2
      ERROR ""
      CLOSE WINDOW ventana_12
      RETURN
   END IF

   DATABASE safre_tmp

      WHENEVER ERROR CONTINUE
         DROP TABLE tmp_estados
      WHENEVER ERROR STOP

      CREATE TABLE tmp_estados(
      estado SMALLINT
      )

   INSERT INTO safre_tmp:tmp_estados
   SELECT UNIQUE estado
   FROM   safre_af:cta_nss_edo_cta

   UPDATE STATISTICS FOR TABLE safre_tmp:tmp_estados

   DATABASE safre_af

   FOR ls_procesar = 1 TO ls_cont
   	  IF lar_estados[ls_procesar].marca = 'X' THEN
   	  	 LET ls_estado = lar_estados[ls_procesar].estado

   	  	 DELETE
   	  	 FROM  safre_tmp:tmp_estados
   	  	 WHERE estado = ls_estado
   	  END IF
   END FOR

   CALL envia_folio()

   CLOSE WINDOW ventana12
   EXIT PROGRAM
END FUNCTION
######################################################################
FUNCTION captura_fecha()
   DEFINE ls_flag SMALLINT

   LET ls_flag = 0
   INITIALIZE gd_fecha_corte TO NULL
   DISPLAY " <ESC> ACEPTAR                                                  <CTRL-C> SALIR" AT 2,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME gd_fecha_corte WITHOUT DEFAULTS
   	  AFTER FIELD gd_fecha_corte
         IF gd_fecha_corte IS NULL THEN
            ERROR "DEBE INDICAR FECHA DE CORTE"
            SLEEP 2
            ERROR ""
            NEXT FIELD gd_fecha_corte
         END IF

      ON KEY(ESC)
          IF gd_fecha_corte IS NULL THEN
            ERROR "DEBE INDICAR FECHA DE CORTE"
            SLEEP 2
            ERROR ""
            NEXT FIELD gd_fecha_corte
         END IF
         LET ls_flag = 0
         EXIT INPUT

      ON KEY(INTERRUPT)
        ERROR "PROCESO CANCELADO"
        SLEEP 2
        LET ls_flag = 1
        EXIT INPUT
   END INPUT

   RETURN ls_flag
END FUNCTION
################################################################################
#Cuatrimestre3 2009
FUNCTION envia_folio()

   DEFINE li_folio_dia        INTEGER,
          x_tipo_proceso      SMALLINT,
          x_estado            SMALLINT

   DEFINE lc_msg           CHAR(400),
          lc_ok            CHAR(1)

   LET x_tipo_proceso = 0
   LET x_estado       = 10

   SELECT "X"
   FROM cta_folio
   GROUP BY 1

   #Asignar folio
   IF SQLCA.SQLCODE = 0 THEN
      SELECT folio + 1
      INTO   li_folio_dia
      FROM   cta_folio

      UPDATE cta_folio
      SET    folio = li_folio_dia
   ELSE
      LET li_folio_dia = 1

      INSERT INTO cta_folio
      VALUES (li_folio_dia)
   END IF

   #Registro de control
   CALL ejecuta_lectura(x_tipo_proceso,li_folio_dia)

   ERROR "Ejecutando Lectura y Proceso de Archivo por nohup ..."
   SLEEP 2
   ERROR ""

   LET ejecuta="nohup time fglgo CTAB050.4gi ",
                   gd_fecha_corte CLIPPED ," ",
                   li_folio_dia   CLIPPED," ",
                   x_estado CLIPPED,--" &"
                 " 1> ", usuario CLIPPED, ".CTAB050.salida  ",
                 " 2> ", usuario CLIPPED, ".CTAB050.error  & "

   RUN ejecuta

   ERROR "El proceso se ejecuto satisfactoriamente por nohup ..."
   SLEEP 2
   ERROR ""

   ERROR "Verifique ARCHIVOS: ", usuario CLIPPED,".CTAB050.salida y ", usuario CLIPPED,".CTAB050.error"

   LET lc_msg = "Folio: ",li_folio_dia USING "<<<<<<<<<<<<", " Proc Cod: CTAB050"
   PROMPT lc_msg CLIPPED FOR CHAR lc_ok ATTRIBUTE (REVERSE)

END FUNCTION
################################################################################