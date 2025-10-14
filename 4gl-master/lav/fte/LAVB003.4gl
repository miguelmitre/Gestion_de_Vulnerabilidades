################################################################################
#Project           => SAFRE (Mexico)
#Owner             => E.F.P.
#Programa LAVB003  => Proceso lavado de dinero
#Por               => OMAR SANDOVAL BADILLO             
#Fecha creacion    => 07 de julio de 2005
#Sistema           => LAV
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE afi_nombres        CHAR(40),
          afi_paterno        CHAR(40),
          afi_materno        CHAR(40),
          afi_n_rfc          CHAR(13),
          afi_n_unico        CHAR(18),
          afi_fena           DATE,
          afi_n_folio        INTEGER,
          afi_tipo_solicitud SMALLINT

   DEFINE reg_1              RECORD
          fecha_inicio       DATE,
          fecha_fin          DATE
   END RECORD

   DEFINE reg_5 ARRAY[500]   OF RECORD
          folio              INTEGER,
          nss                CHAR(11),
          nombre             CHAR(20),
          tipo_operacion     CHAR(2),
          fecha_operacion    DATE,
          monto_en_pesos     DECIMAL(16,6)
   END RECORD

   DEFINE reg_2 ARRAY[30000]   OF RECORD 
          folio              LIKE lav_det_lavado.folio,
          nss                LIKE lav_det_lavado.nss,
          tipo_operacion     LIKE lav_det_lavado.tipo_operacion,
          fecha_operacion    DATE,
          monto_en_pesos     DECIMAL(16,2)
   END RECORD

   DEFINE reg_9 ARRAY[5000]   OF RECORD 
          folio              INTEGER,
          nss                CHAR(11),
          tipo_operacion     SMALLINT,
          fecha_operacion    DATE,
          monto_en_pesos     DECIMAL(16,6),
          subcuenta          SMALLINT,
          tipo_clasifica     SMALLINT,
          criterio           SMALLINT,
          usuario            CHAR(8),
          factualiza         DATE
   END RECORD

   DEFINE total_reg          SMALLINT,
          total_monto        DECIMAL(16,6)

   DEFINE cla_where          CHAR(100),
          sel_where          CHAR(100),
          pos                SMALLINT,
          x_folio            INTEGER,
          vruta_envio        CHAR(40),
          vruta_listados     CHAR(40)

   DEFINE reg                RECORD
          hora               CHAR(6)
   END RECORD

   DEFINE arr_c              SMALLINT,
          arr_l              SMALLINT,
          arr_t              SMALLINT,
          i                  SMALLINT,
          ii                 SMALLINT,
          totala             INTEGER,
          totalc             DECIMAL(16,6),
          totald             DECIMAL(16,6)

   DEFINE HOY                DATE

   DEFINE opc                CHAR(01),
          vmenos             SMALLINT

   DEFINE vprecio_accion     DECIMAL(16,6),
          consecutivo        SMALLINT

   DEFINE txt_sel            CHAR(400),
          txt_pdq            CHAR(100),
          g_impre            CHAR(200),
          x_status           CHAR(1),
          dias               SMALLINT,
          x_operacion        CHAR(1),
          x_periodo          CHAR(6),
          x_fecha            DATE,
          x_sw               SMALLINT,
          g_lista            CHAR(300),
          hora               CHAR(08),
          sw_1               SMALLINT

   DEFINE errvar             CHAR(250)

   DEFINE ga_1 ARRAY[5000]    OF RECORD
          folio              LIKE  lav_det_lavado.folio,
          nss                LIKE  lav_det_lavado.nss,
          nombres            LIKE  afi_mae_afiliado.nombres,
          tipo_operacion     LIKE  lav_det_lavado.tipo_operacion,
          fecha_movimiento   LIKE  lav_det_lavado.fecha_operacion,
          monto_en_pesos     LIKE  lav_det_lavado.monto_en_pesos,
          subcuenta          SMALLINT,
          tipo_clasifica     SMALLINT,
          criterio           SMALLINT
   END RECORD

   DEFINE ga_2 ARRAY[1000]    OF RECORD
          tipo_movimiento    LIKE dis_cuenta.tipo_movimiento,
          fecha_conversion   LIKE dis_cuenta.fecha_conversion,
          monto_en_pesos     LIKE dis_cuenta.monto_en_pesos
   END RECORD

   DEFINE x_cod_afo           CHAR(3)   
END GLOBALS
################################################################################
MAIN
   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT 

   CALL STARTLOG("LAVB003.log")

   LET HOY = TODAY

   OPEN WINDOW ventana AT 2,2 WITH FORM "LAVB0031" ATTRIBUTE(BORDER)
   DISPLAY " LAVB003               CLIENTES DE BAJO Y ALTO RIESGO                          " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd/mm/yyyy " AT 3,67 ATTRIBUTE(REVERSE)
   DISPLAY " Detectar registros < ESC >                                                  " AT 4,1 ATTRIBUTE(REVERSE)

   CALL inicia()

   MENU " ALTO RIESGO "
      COMMAND "Detecta" " Detecta lavado de dinero por operacion"
         CALL proceso()
      COMMAND "Consulta" " Consulta lavado de dinero por operacion"
         CALL consulta()
      COMMAND KEY(L) "cLasificacion" " Consulta lavado de dinero por operacion"
         CALL clasifica()
      COMMAND "Salir" " Salir del Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana
END MAIN
###############################################################################
FUNCTION inicia()

   INITIALIZE reg_1.* TO NULL
   DISPLAY BY NAME reg_1.*

   SELECT codigo_afore
   INTO   x_cod_afo
   FROM   tab_afore_local

   SELECT ruta_envio,
          ruta_listados
   INTO   vruta_envio,
          vruta_listados
   FROM   seg_modulo
   WHERE  modulo_cod = "lav"

   LET txt_sel = " SELECT * ",
                 " FROM   lav_clasifica_cliente ",
                 " WHERE  folio = ? ",
                 " ORDER BY 1,4 "
   PREPARE query1 FROM txt_sel

   LET txt_sel = " SELECT nombres,",
                        " paterno,",
                        " materno,",
                        " n_rfc,",
                        " n_unico,",
                        " fena,",
                        " n_folio,",
                        " tipo_solicitud ",
                 " FROM   afi_mae_afiliado ",
                 " WHERE  n_seguro = ? "
   PREPARE query2 FROM txt_sel

   LET txt_sel = " SELECT folio,",
                        " tipo_movimiento,",
                        " subcuenta,",
                        " nss,",
                        " monto_en_pesos,",
                        " fecha_conversion,",
                        " sucursal ",
                 " FROM   dis_cuenta ",
                 " WHERE  subcuenta IN(3,10,11,12) ",
                 " AND    tipo_movimiento = 1 ",
                 " AND    fecha_conversion BETWEEN ? AND ? ",
                 " AND    id_aportante[1,3] NOT IN ('MQ-','MC-','UM-','UQ-','MA-','MS-','AF-','AC-','UR-','US-','AA-','AS-','UY-','UZ-','COR')",
                 " ORDER BY 4,5 "
   PREPARE query3 FROM txt_sel

   LET txt_sel = " SELECT nss,",
                        " tipo_operacion ",
                 " FROM   lav_clasifica_cliente ",
                 " WHERE  folio = ? ",
                 " ORDER BY 1,2 "
   PREPARE query8 FROM txt_sel

   LET txt_sel = " INSERT INTO lav_clasifica_cliente ",
                 " VALUES (?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " USER,",
                         " TODAY)"
   PREPARE query9 FROM txt_sel

END FUNCTION
#############################################################################
FUNCTION proceso()

   LET int_flag = FALSE

   INPUT BY NAME reg_1.fecha_inicio,
                 reg_1.fecha_fin  WITHOUT DEFAULTS

      AFTER FIELD fecha_inicio
         IF reg_1.fecha_inicio IS NULL THEN
            ERROR "Fecha de inicio no puede ser NULA"
            NEXT FIELD fecha_inicio
         END IF
   
         IF reg_1.fecha_inicio > HOY THEN
            ERROR "LA FECHA INICIO NO PUEDE SER MAYOR AL DIA DE HOY"
            NEXT FIELD fecha_inicio
         END IF   

         LET reg_1.fecha_fin = TODAY

         CALL habil_siguiente(reg_1.fecha_fin)
              RETURNING x_fecha

         LET reg_1.fecha_fin = x_fecha
         DISPLAY BY NAME reg_1.fecha_fin

      AFTER FIELD fecha_fin
         IF reg_1.fecha_fin IS NULL THEN
            ERROR "Fecha de fin no puede ser NULA"
            NEXT FIELD fecha_fin
         END IF

         IF reg_1.fecha_fin > HOY THEN
            ERROR "LA FECHA FIN NO PUEDE SER MAYOR AL DIA DE HOY"
            NEXT FIELD fecha_fin
         END IF

         IF reg_1.fecha_fin < reg_1.fecha_inicio THEN
            ERROR "LA FECHA FIN NO PUEDE SER MENOR A LA FECHA INICIO"
            NEXT FIELD fecha_inicio
         END IF

         IF reg_1.fecha_fin IS NOT NULL THEN
            SELECT "X"
            FROM   lav_clasifica_cliente
            WHERE  fecha_movimiento BETWEEN reg_1.fecha_inicio 
                                        AND reg_1.fecha_fin
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
               ERROR "YA HAY REGISTROS EN EL RANGO DE FECHAS "
               SLEEP 2
               ERROR ""
               NEXT FIELD fecha_inicio
            END IF
         END IF


      ON KEY (ESC)
         IF reg_1.fecha_inicio IS NULL THEN
            ERROR "Fecha de inicio no puede ser NULA"
            NEXT FIELD fecha_inicio
         END IF

         IF reg_1.fecha_inicio > HOY THEN
            ERROR "LA FECHA INICIO NO PUEDE SER MAYOR AL DIA DE HOY"
            NEXT FIELD fecha_inicio
         END IF

         IF reg_1.fecha_fin IS NULL THEN
            ERROR "Fecha de fin no puede ser NULA"
            NEXT FIELD fecha_fin
         END IF

         IF reg_1.fecha_fin > HOY THEN
            ERROR "LA FECHA FIN NO PUEDE SER MAYOR AL DIA DE HOY"
            NEXT FIELD fecha_fin
         END IF

         IF reg_1.fecha_fin < reg_1.fecha_inicio THEN
            ERROR "LA FECHA FIN NO PUEDE SER MENOR A LA FECHA INICIO"
            NEXT FIELD fecha_inicio
         END IF

         IF reg_1.fecha_fin IS NOT NULL THEN
            SELECT "X"
            FROM   lav_clasifica_cliente
            WHERE  fecha_movimiento BETWEEN reg_1.fecha_inicio
                                        AND reg_1.fecha_fin
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
               ERROR "YA HAY REGISTROS EN EL RANGO DE FECHAS "
               SLEEP 2
               ERROR ""
               NEXT FIELD fecha_inicio
            END IF
         END IF

         LET int_flag = FALSE
         EXIT INPUT

      ON KEY (CONTROL-C)
         LET int_flag = TRUE
         EXIT INPUT
   END INPUT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..." 
      INITIALIZE reg_1.* TO NULL
      DISPLAY BY NAME reg_1.*
      CLEAR SCREEN
      RETURN
   END IF

   ERROR "PROCESANDO INFORMACION ... "

   CALL detecta(reg_1.fecha_inicio,
                reg_1.fecha_fin)
        RETURNING x_folio,
                  x_sw

   IF x_sw = 1 THEN
      CALL despliega(x_folio)
   ELSE
      DELETE 
      FROM  lav_folio
      WHERE folio = x_folio

      PROMPT " NO HAY REGISTROS EN EL PERIODO CAPTURADO " ATTRIBUTE(REVERSE)
      FOR opc ATTRIBUTE(REVERSE)
   END IF

   CLEAR SCREEN
END FUNCTION
#######################################################################
FUNCTION despliega(x_folio)

   DEFINE x_folio     INTEGER,
          i           SMALLINT

   OPEN WINDOW ventana1 AT 10,2 WITH FORM "LAVB0032" 
   DISPLAY "                  REGISTRO DETECTADOS CON APORTACIONES                         " AT 1,1 ATTRIBUTE(REVERSE)

   DECLARE cur_individual CURSOR FOR query1
   LET i = 1
   FOREACH cur_individual USING x_folio 
                           INTO reg_2[i].*

      LET reg_5[i].folio           = reg_2[i].folio
      LET reg_5[i].nss             = reg_2[i].nss
      LET reg_5[i].tipo_operacion  = reg_2[i].tipo_operacion
      LET reg_5[i].fecha_operacion = reg_2[i].fecha_operacion
      LET reg_5[i].monto_en_pesos  = reg_2[i].monto_en_pesos

      DECLARE cursor_fx CURSOR FOR query2
      OPEN cursor_fx USING reg_2[i].nss
      FETCH cursor_fx INTO afi_nombres,
                           afi_paterno,
                           afi_materno,
                           afi_n_rfc,
                           afi_n_unico,
                           afi_fena,
                           afi_n_folio,
                           afi_tipo_solicitud
      CLOSE cursor_fx

      LET reg_5[i].nombre = afi_nombres CLIPPED," ",afi_paterno CLIPPED
      LET i = i + 1
   END FOREACH

   IF (i-1) >= 1 THEN
      CALL SET_COUNT(i-1)

      DISPLAY ARRAY reg_5 TO scr_1.*
         ON KEY (CONTROL-C)
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "NO SE ENCONTRARON REGISTROS ..."
      SLEEP 2
      ERROR ""
   END IF

   CLEAR SCREEN 
   CLOSE WINDOW ventana1
END FUNCTION
#############################################################################
FUNCTION detecta(x_fecha_inicio,
                 x_fecha_fin )

   DEFINE det RECORD
       folio             INTEGER,
       nss               CHAR(11),
       tipo_operacion    SMALLINT,
       fecha_movimiento  DATE,
       monto_en_pesos    DECIMAL(16,2),
       subcuenta         SMALLINT,
       tipo_clasifica    SMALLINT,
       criterio          SMALLINT
   END RECORD

  
   DEFINE x_tipo_reporte       SMALLINT,
          xx_tipo_reporte      CHAR(1),
          x_fecha_inicio       DATE,
          x_fecha_fin          DATE,
          x_fecha_periodo_ini  CHAR(10),
          x_fecha_periodo_fin  CHAR(10),
          x_fecha_periodo      CHAR(10),
          xx_fecha_periodo     CHAR(8),
          vfolio               INTEGER,
          pos                  SMALLINT,
          i                    SMALLINT

   DEFINE reg_3                RECORD
          folio                INTEGER,
          subcuenta            SMALLINT,
          tipo_movimiento      SMALLINT,
          id_aportante         CHAR(11),
          nss                  CHAR(11),
          monto_en_pesos       DECIMAL(16,6),
          fecha_conversion     DATE,
          sucursal             CHAR(10)
   END RECORD

   DEFINE x_tipo_operacion     CHAR(2)

   DEFINE hoy2                 DATE,
          x_fecha_aplica       DATE

   DEFINE x_ano_cuenta         CHAR(15),
          xx_sw                SMALLINT

   DEFINE afi_nombres            CHAR(40),
          afi_paterno            CHAR(40),
          afi_materno            CHAR(40),
          afi_n_rfc              CHAR(13),
          afi_n_unico            CHAR(18),
          afi_fena               DATE,
          afi_n_folio            INTEGER,
          afi_tipo_solicitud     SMALLINT

   LET xx_sw = 0

   INSERT INTO lav_folio
   VALUES(0)

   SELECT MAX(folio)
   INTO   vfolio
   FROM   lav_folio

################### DETECTA TODAS LAS APORTACIONES #####################

   LET pos = 1
   DECLARE cursor_1 CURSOR FOR query3
   FOREACH cursor_1 USING x_fecha_inicio,
                          x_fecha_fin 
                    INTO  reg_3.folio,
                          reg_3.tipo_movimiento,
                          reg_3.subcuenta,
                          reg_3.nss,
                          reg_3.monto_en_pesos,
                          reg_3.fecha_conversion

      IF reg_3.tipo_movimiento = 1 THEN
         LET x_tipo_operacion = "20"

         IF x_tipo_operacion = "20" THEN
            LET xx_sw = 1

            LET det.folio            = vfolio
            LET det.nss              = reg_3.nss
            LET det.tipo_operacion   = x_tipo_operacion
            LET det.fecha_movimiento = reg_3.fecha_conversion
            LET det.monto_en_pesos   = reg_3.monto_en_pesos
            LET det.subcuenta        = reg_3.subcuenta
            LET det.tipo_clasifica   = 0
            LET det.criterio         = " "

            EXECUTE query9 USING det.folio,
                                 det.nss,
                                 det.tipo_operacion,
                                 det.fecha_movimiento,
                                 det.monto_en_pesos,
                                 det.subcuenta,
                                 det.tipo_clasifica,
                                 det.criterio

            LET pos = pos + 1
         END IF
         LET reg_3.monto_en_pesos = 0
      END IF
   END FOREACH

   RETURN vfolio,
          xx_sw

END FUNCTION
################################################################
FUNCTION crea_tabla_temp()

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_cuenta_lav

      CREATE TEMP TABLE tmp_cuenta_lav
      (nss               CHAR(11),
       tipo_movimiento   SMALLINT,
       fecha_conversion  DATE,
       monto_en_pesos    DECIMAL(16,6)
      )

   WHENEVER ERROR STOP
END FUNCTION
################################################################
FUNCTION consulta()

   DEFINE x_folio             INTEGER,
          x_nss               CHAR(11),
          periodo_reporte     DATE,
          x_tipo_clasifica    SMALLINT,
          x_criterio          SMALLINT,
          pos                 SMALLINT,
          row_cnt             SMALLINT

   DEFINE sql_stat            INTEGER,
          item_row_cnt        SMALLINT,
          cont_inp            SMALLINT,
          cur_row             SMALLINT,
          scr_row             SMALLINT,
          p_cur_row           SMALLINT

   DEFINE w          SMALLINT,
          x_ano      SMALLINT,
          xxx_ano    SMALLINT,
          ano_cuenta CHAR(4),
          sql_text   CHAR(650)

   CALL crea_tabla_temp()

   OPEN WINDOW ventana3 AT 5,2 WITH FORM "LAVB0033" 
   DISPLAY " < ESC > Consultar                                                           " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "                           CONSULTA DE REGISTROS                             " AT 6,1 ATTRIBUTE(REVERSE)

   LET int_flag = FALSE
   CONSTRUCT cla_where ON a.folio,
                          a.nss,
                          a.fecha_movimiento,
                          a.tipo_clasifica,
                          a.criterio
                     FROM x_folio,
                          x_nss,
                          x_fecha_movimiento,
                          x_tipo_clasifica,
                          x_criterio

      ON KEY (ESC)
         ERROR "PROCESANDO INFORMACION..."
         LET int_flag = FALSE
         EXIT CONSTRUCT
      ON KEY (CONTROL-C)
         LET int_flag = TRUE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..."
      CLEAR SCREEN
      CLOSE WINDOW ventana3
      RETURN
   END IF

   LET txt_sel = " SELECT a.* ",
                 " FROM   lav_clasifica_cliente a",
                 " WHERE  ",cla_where CLIPPED,
                 " ORDER BY 1,4 "

   PREPARE query18 FROM txt_sel
   DECLARE cursor_consulta CURSOR FOR query18

   LET i = 1
   LET x_ano = YEAR (TODAY)

   FOREACH cursor_consulta INTO reg_9[i].*

      LET ga_1[i].folio            = reg_9[i].folio
      LET ga_1[i].nss              = reg_9[i].nss
      LET ga_1[i].tipo_operacion   = reg_9[i].tipo_operacion
      LET ga_1[i].fecha_movimiento = reg_9[i].fecha_operacion
      LET ga_1[i].monto_en_pesos   = reg_9[i].monto_en_pesos
      LET ga_1[i].subcuenta        = reg_9[i].subcuenta     
      LET ga_1[i].tipo_clasifica   = reg_9[i].tipo_clasifica
      LET ga_1[i].criterio         = reg_9[i].criterio
      
{
      LET xxx_ano = YEAR (ga_1[i].fecha_movimiento)
      LET w = 0
      WHENEVER ERROR CONTINUE
      FOR w = 1997 TO xxx_ano
         LET ano_cuenta = w
         IF ano_cuenta = x_ano THEN
            LET ano_cuenta = "   "
         END IF
         LET sql_text =  " INSERT INTO tmp_cuenta_lav ",
                      " SELECT nss,",
                              "tipo_movimiento,",
                              "fecha_conversion,",
                              "monto_en_pesos ",
                      " FROM   dis_cuenta",ano_cuenta[3,4] CLIPPED,
                      " WHERE  nss = ","'",ga_1[i].nss CLIPPED,"'",
                      " AND    subcuenta in (3,10) ",
                      " AND    tipo_movimiento IN (1,10,490) ",
           "AND    id_aportante[1,3] NOT IN ('MQ-','MC-','UM-','UQ-','MA-','MS-','AF-','AC-','UR-','US-','AA-','AS-','UY-','UZ-','COR')"
         PREPARE cta_execute FROM sql_text
         EXECUTE cta_execute
      END FOR
}

      DECLARE cursor_confx CURSOR FOR query2
      OPEN cursor_confx USING reg_9[i].nss
      FETCH cursor_confx INTO afi_nombres,
                              afi_paterno,
                              afi_materno,
                              afi_n_rfc,
                              afi_n_unico,
                              afi_fena,
                              afi_n_folio,
                              afi_tipo_solicitud
      CLOSE cursor_confx

      LET ga_1[i].nombres = afi_nombres CLIPPED," ",afi_paterno CLIPPED
      LET i = i + 1
   END FOREACH
{
   CREATE INDEX tmp_cuenta_1 ON tmp_cuenta_lav (nss,
                                            tipo_movimiento,
                                            fecha_conversion)
   UPDATE STATISTICS FOR TABLE tmp_cuenta_lav
   WHENEVER ERROR STOP
}

   LET i = i - 1

   IF (I) >= 1 THEN
      CALL SET_COUNT(I)
      
      DISPLAY ARRAY ga_1 to scr_1.*
         ON KEY (CONTROL-M)
            LET i = ARR_CURR()

         ON KEY (CONTROL-C)
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "NO SE ENCONTRARON REGISTROS..."
      SLEEP 2
      ERROR ""
   END IF

   DROP TABLE tmp_cuenta_lav 
   CLEAR SCREEN 
   CLOSE WINDOW ventana3

END FUNCTION
###############################################################
FUNCTION clasifica()

   DEFINE x_folio             INTEGER,
          x_nss               CHAR(11),
          x_subcuenta         SMALLINT,
          pos                 SMALLINT,
          row_cnt             SMALLINT

   DEFINE sql_stat            INTEGER,
          item_row_cnt        SMALLINT,
          cont_inp            SMALLINT,
          cur_row             SMALLINT,
          scr_row             SMALLINT,
          p_cur_row           SMALLINT

   DEFINE w          SMALLINT,
          x_ano      SMALLINT,
          xxx_ano    SMALLINT,
          ano_cuenta CHAR(4),
          sql_text   CHAR(650),
          salir      SMALLINT

   DEFINE ga_1x ARRAY[5000]    OF RECORD
          folio              LIKE  lav_det_lavado.folio,
          nss                LIKE  lav_det_lavado.nss,
          nombres            LIKE  afi_mae_afiliado.nombres,
          tipo_operacion     LIKE  lav_det_lavado.tipo_operacion,
          fecha_movimiento   LIKE  lav_det_lavado.fecha_operacion,
          monto_en_pesos     LIKE  lav_det_lavado.monto_en_pesos,
          subcuenta          SMALLINT,
          tipo_clasifica     SMALLINT,
          criterio           SMALLINT
   END RECORD

   CALL crea_tabla_temp()

   OPEN WINDOW ventana3x AT 5,2 WITH FORM "LAVB0034" 
   DISPLAY " < ESC > Detectar registros " AT 1,1 
   DISPLAY "                          C L A S I F I C A C I O N                            " AT 2,1 ATTRIBUTE(REVERSE)
   #DISPLAY "                          REGISTROS PARA CLASIFICAR                            " AT 7,1 ATTRIBUTE(REVERSE)

   LET int_flag = FALSE

   CONSTRUCT cla_where ON folio,
                          nss,
                          subcuenta
                     FROM x_folio,
                          x_nss,
                          x_subcuenta

      ON KEY (ESC)
         ERROR "PROCESANDO INFORMACION..."
         LET int_flag = FALSE
         EXIT CONSTRUCT
      ON KEY (CONTROL-C)
         LET int_flag = TRUE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..."
      CLEAR SCREEN
      CLOSE WINDOW ventana3x
      RETURN
   END IF

   LET salir = FALSE	
   WHILE NOT salir

      INITIALIZE ga_1x TO NULL

      LET txt_sel = " SELECT * ",
                    " FROM   lav_clasifica_cliente ",
                    " WHERE  ",cla_where CLIPPED,
                    " ORDER BY 1,4 "

      PREPARE query18x FROM txt_sel
      DECLARE cursor_clasifica SCROLL CURSOR FOR query18x

      LET i = 1

      FOREACH cursor_clasifica INTO reg_9[i].*

         IF reg_9[i].folio = 0 OR reg_9[i].folio IS NULL THEN
            EXIT FOREACH
         END IF

         LET ga_1x[i].folio            = reg_9[i].folio
         LET ga_1x[i].nss              = reg_9[i].nss
         LET ga_1x[i].tipo_operacion   = reg_9[i].tipo_operacion
         LET ga_1x[i].fecha_movimiento = reg_9[i].fecha_operacion
         LET ga_1x[i].monto_en_pesos   = reg_9[i].monto_en_pesos
         LET ga_1x[i].subcuenta        = reg_9[i].subcuenta
         LET ga_1x[i].tipo_clasifica   = reg_9[i].tipo_clasifica

         IF reg_9[i].criterio IS NULL THEN 
            LET ga_1x[i].criterio = " "
         ELSE
            LET ga_1x[i].criterio = reg_9[i].criterio
         END IF

         DECLARE cursor_x CURSOR FOR query2
         OPEN cursor_x USING reg_9[i].nss
         FETCH cursor_x INTO afi_nombres,
                             afi_paterno,
                             afi_materno,
                             afi_n_rfc,
                             afi_n_unico,
                             afi_fena,
                             afi_n_folio,
                             afi_tipo_solicitud
         CLOSE cursor_x
   
         LET ga_1x[i].nombres = afi_nombres CLIPPED," ",
                                afi_paterno CLIPPED," ",
                                afi_materno CLIPPED
         LET i = i + 1
      END FOREACH 

      ERROR ""
      LET i = i - 1

      IF (i) >= 1 THEN
         CALL SET_COUNT(i)
         DISPLAY "                      SELECCION DEL REGISTRO A CLASIFICAR                      " AT 6,1 ATTRIBUTE(REVERSE)

         DISPLAY ARRAY ga_1x TO scr_1.*
            ON KEY (CONTROL-M)
               LET i = ARR_CURR()

               SELECT "X"
               FROM   lav_clasifica_cliente b
               WHERE  b.folio = ga_1x[i].folio
               AND    b.nss   = ga_1x[i].nss
               AND    b.tipo_clasifica  = ga_1x[i].tipo_clasifica
               AND    b.criterio        IS NOT NULL

               IF SQLCA.SQLCODE = 0 THEN
                  ERROR "EL REGISTRO NO PUEDE SER CLASIFICADO MAS DE UNA VEZ "
                  SLEEP 2
               ELSE
                  CALL clasifica_desc(ga_1x[i].nss,
                                      ga_1x[i].nombres,
                                      ga_1x[i].tipo_operacion)
                  EXIT DISPLAY
               END IF
            
            ON KEY (INTERRUPT)
               LET salir = TRUE
               INITIALIZE ga_1x TO NULL
               EXIT DISPLAY
         END DISPLAY
      ELSE
         ERROR "NO SE ENCONTRARON REGISTRON ..."
         SLEEP 2
         EXIT WHILE 
      END IF
   END WHILE

   CLEAR SCREEN 
   CLOSE WINDOW ventana3x

END FUNCTION
###############################################################
FUNCTION proc_items(p_cur_row)

   DEFINE sql_stat      INTEGER,
          p_cur_row     SMALLINT,
          item_row_cnt  SMALLINT

   CALL sel_ord_item(p_cur_row)
        RETURNING sql_stat,
                  item_row_cnt
 
   CALL disp_four_items()

   RETURN sql_stat,
          item_row_cnt

END FUNCTION
###############################################################
FUNCTION disp_array_items(p_item_row_cnt)

   DEFINE p_item_row_cnt SMALLINT

   CALL SET_COUNT(p_item_row_cnt)

   DISPLAY ARRAY ga_2 TO scr_2.*
      ON KEY(ESC)
         EXIT DISPLAY
   END DISPLAY
END FUNCTION
#############################################################
FUNCTION sel_ord_item(p_cur_row)

   DEFINE sql_text   CHAR(650),
          sql_stat   INTEGER,
          p_cur_row  SMALLINT,
          row_cnt    SMALLINT,
          x_tablas   CHAR(20)

   CALL null_items()

   LET sql_text = " SELECT CASE ",
                        " WHEN tipo_movimiento = 1 THEN 20 ",
                        " WHEN tipo_movimiento = 490 THEN 21 ",
                        " END AS tipo_movimiento ,",
                        " fecha_conversion,",
                        " monto_en_pesos ",
                  " FROM  tmp_cuenta_lav ",
                  " WHERE nss   = ? ",
                  " AND   tipo_movimiento IN (1,490) ",
                  " AND   fecha_conversion <= ? ",
                  " GROUP BY 1,2,3 ",
                  " ORDER BY 2 DESC " CLIPPED

   PREPARE sel_item_stmt FROM sql_text
   DECLARE sel_item_curs CURSOR FOR sel_item_stmt

   WHENEVER ERROR CONTINUE

LET ga_1[p_cur_row].fecha_movimiento = MDY(MONTH(ga_1[p_cur_row].fecha_movimiento),DAY(ga_1[p_cur_row].fecha_movimiento),YEAR(ga_1[p_cur_row].fecha_movimiento))

   OPEN sel_item_curs USING ga_1[p_cur_row].nss,
                            ga_1[p_cur_row].fecha_movimiento

   WHENEVER ERROR STOP

   LET sql_stat = SQLCA.SQLCODE

   LET row_cnt = 1

   WHILE ((NOT sql_stat) AND (row_cnt<= 100))
      WHENEVER ERROR CONTINUE
      FETCH sel_item_curs INTO ga_2[row_cnt].*
      WHENEVER ERROR STOP

      LET sql_stat = SQLCA.SQLCODE

      IF (NOT sql_stat) THEN
         LET row_cnt = row_cnt + 1
      END IF
   END WHILE

   IF (sql_stat = 100) THEN
      LET sql_stat = 0
   END IF

   RETURN sql_stat,
          row_cnt - 1
END FUNCTION
################################################################
FUNCTION disp_four_items()

   DEFINE i SMALLINT

   FOR i = 1 TO 3
      DISPLAY ga_2[i].* TO scr_2[i].*
   END FOR
END FUNCTION
###############################################################
FUNCTION null_items()

   DEFINE i SMALLINT

   INITIALIZE ga_2[1].* TO NULL

   FOR i = 2 TO 100
      LET ga_2[i].* = ga_2[1].*
   END FOR
END FUNCTION
###############################################################
FUNCTION clasifica_desc(l_rec_nss,
                        l_rec_nombres,
                        tipo_operacion)

   DEFINE l_rec_nss            CHAR(11),
          tipo_criterio        SMALLINT,
          tipo_clasifica       SMALLINT,
          l_rec_nombres        CHAR(32),
          pos                  SMALLINT,
          tipo_operacion       SMALLINT,
          desc_operacion       CHAR(15)

   DEFINE intimidado             ,
          sobornado              ,
          reportados             ,
          falsos                 ,
          niegan                 ,
          buscados               CHAR(1)

   OPEN WINDOW ventana7 AT 4,2 WITH FORM "LAVB0035" 
   DISPLAY " LAVB003                                                                     " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd/mm/yyyy " AT 3,67 ATTRIBUTE(REVERSE)
   DISPLAY " < ESC > Grabar registro                                    < Ctrl-C > Salir   " AT 2,1 
   DISPLAY "                           A L T O   R I E S G O                               " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "                      TIPOS DE CRITERIO DE ALTO RIESGO                         " AT 6,1 ATTRIBUTE(REVERSE)

   IF tipo_operacion = 20 THEN
      LET desc_operacion = "APORTE"
   END IF

   DISPLAY l_rec_nss      TO nss
   DISPLAY tipo_operacion TO tipo_operacion
   DISPLAY desc_operacion TO desc_operacion
   DISPLAY l_rec_nombres  TO nombre

   LET tipo_criterio = NULL

   INPUT BY NAME intimidado,
                 sobornado,
                 reportados,
                 falsos,
                 niegan,
                 buscados WITHOUT DEFAULTS

      AFTER FIELD intimidado
         IF intimidado IS NULL THEN
         ELSE
            LET tipo_criterio = 1
            NEXT FIELD intimidado
         END IF

      AFTER FIELD sobornado
         IF sobornado IS NULL THEN
         ELSE
            LET tipo_criterio = 2
            NEXT FIELD sobornado
         END IF

      AFTER FIELD reportados
         IF reportados IS NULL THEN
         ELSE
            LET tipo_criterio = 3
            NEXT FIELD reportados
         END IF

      AFTER FIELD falsos
         IF falsos IS NULL THEN
         ELSE
            LET tipo_criterio = 4
            NEXT FIELD falsos
         END IF

      AFTER FIELD niegan
         IF niegan IS NULL THEN
         ELSE
            LET tipo_criterio = 5
            NEXT FIELD niegan
         END IF

      AFTER FIELD buscados
         IF buscados IS NULL THEN
         ELSE
            LET tipo_criterio = 6
            NEXT FIELD buscados
         END IF

      ON KEY(ESC)
         IF tipo_criterio IS NULL THEN
            ERROR "DEBE SELECCIONAR UNA OPCION ..."
         ELSE
            UPDATE lav_clasifica_cliente
            SET    tipo_clasifica = 1,
                   criterio       = tipo_criterio
            WHERE  folio = reg_9[i].folio
            AND    nss   = l_rec_nss

            EXIT INPUT
         END IF

      ON KEY(CONTROL-C)
         EXIT INPUT
   END INPUT
      
   CLOSE WINDOW ventana7
END FUNCTION
################################################################
FUNCTION habil_siguiente(diaActual)

   DEFINE diaTmp        DATE,
          contador      SMALLINT,
          diaActual     DATE

   DEFINE diaHabilSig   DATE,
          diaSemana     SMALLINT,
          feriado       SMALLINT,
          finSemana     SMALLINT

   LET diaHabilSig = diaActual

   WHILE TRUE
      LET feriado   = 0
      LET finSemana = 0
      LET diaSemana = WEEKDAY(diaHabilSig)  

      IF diaSemana = 0 OR diaSemana = 6 THEN
         LET finSemana = 1
      END IF

      SELECT *
      FROM   tab_feriado
      WHERE  feria_fecha = diaHabilSig

      IF STATUS <> NOTFOUND THEN
         LET feriado = 1
      END IF

      IF feriado = 1 OR finSemana = 1 THEN
         LET diaHabilSig = diaHabilSig + 1 UNITS DAY
      ELSE
         EXIT WHILE
      END IF
   END WHILE

   RETURN diaHabilSig
END FUNCTION
###############################################################
FUNCTION fin_mes(fecha)

   DEFINE mes          SMALLINT,
          fecha        DATE,
          bisiesto     ,
          ano          ,
          v_ban        ,
          ult_dia      SMALLINT,
          v_fecha      CHAR(12),
          v_ano        ,
          v_mes        ,
          v_ult_dia    CHAR(5)

   LET v_ban = FALSE
   LET ano = YEAR(fecha)
   LET bisiesto =  ano MOD 4

   IF bisiesto = 0 THEN
      LET v_ban = TRUE
   END IF

   LET mes = MONTH(fecha)

   IF mes = 4 OR mes = 6 OR mes = 9 OR mes = 11 THEN
      LET ult_dia = 30
   ELSE
      IF mes = 2 THEN
         IF v_ban THEN
            LET ult_dia = 29
         ELSE
            LET ult_dia = 28
         END IF
      ELSE
         LET ult_dia = 31
      END IF
   END IF

   LET v_mes  = mes
   LET v_ano = ano
   LET v_ult_dia = ult_dia
   LET v_fecha = v_mes CLIPPED,"/",v_ult_dia CLIPPED, "/", v_ano 
   LET fecha = v_fecha

   RETURN fecha,ult_dia 
END FUNCTION
###########################################################################
