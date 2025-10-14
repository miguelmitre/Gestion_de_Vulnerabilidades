###############################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )
#Propietario       => E.F.P.
#Programa          => COML001
#Descripcion       => Comisiones Promotores
#Fecha             => 3 Mayo 2002.
#By                => GERARDO ALFONSO VEGA PAREDES.
#Sistema           => COM.
###############################################################################

DATABASE safre_af

GLOBALS
   DEFINE g_reg RECORD
      codven         CHAR(10),
      paterno        CHAR(40),
      materno        CHAR(40),
      nombres        CHAR(40),
      diag_proceso   CHAR(02),
      monto_comision DECIMAL(12,2)
   END RECORD

   DEFINE g_glo RECORD
       fecha_corte       DATE,
       total_afiliados   INTEGER,
       coduni_n1         CHAR(10),
       prom_salario_base DECIMAL(12,2),
       comis_calculada   DECIMAL(12,2)
   END RECORD

   DEFINE g_fec RECORD
      fecha_desde DATE,
      fecha_hasta DATE
   END RECORD
      
   DEFINE param RECORD LIKE com_parametro.*

   DEFINE hoy  DATE,
          hora CHAR(08)

   DEFINE usuario  CHAR(08),
          salida   CHAR(300),
          opc      CHAR(01),
          vimprime CHAR(800)

   DEFINE vconsecutivo SMALLINT

   DEFINE tipo_rep CHAR(01)

END GLOBALS


MAIN
   OPTIONS
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I
   DEFER INTERRUPT

   LET hoy = TODAY

   SELECT *,
          user
   INTO   param.*,
          usuario
   FROM   com_parametro   

   OPEN WINDOW win01 AT 2,2 WITH FORM "COML0011" ATTRIBUTE(BORDER)
   DISPLAY " COML001                PAGO COMISIONES PROMOTORES                       " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY " [ESC] Procesar                                         [Ctrl-c] Cancelar "  AT 1,1
   DISPLAY hoy USING "DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)

   MENU "Reportes"
      COMMAND "Promotores"     "Comisiones Promotores"
         LET tipo_rep = "p"
         CALL Promotores()
      COMMAND "Global"         "Comisiones Globales por fecha corte"
         LET tipo_rep = "g"
         CALL Promotores()
      
------      COMMAND "Estado cuenta"  "Estado de cuenta Promotor"
------         ERROR "NO ESTA DISPONIBLE ESTA OPCION"
---         CALL Estado_cuenta()
------      COMMAND "Responsables"   "Comisiones Responsables"
------         CALL Responsable()
------      COMMAND "Bono"   "Comisiones Responsables"
------         CALL Bono()
      COMMAND "Salida"         "Salida"
         EXIT MENU
   END MENU

   CLOSE WINDOW win01
END MAIN

FUNCTION Promotores()

   INPUT BY NAME g_fec.* 
      AFTER FIELD fecha_desde
         IF g_fec.fecha_desde IS NULL THEN
            ERROR "La fecha NO pude ser nula"
            NEXT FIELD fecha_desde
         END IF

      AFTER FIELD fecha_hasta
         IF g_fec.fecha_hasta IS NULL THEN
            ERROR "La fecha NO pude ser nula"
            NEXT FIELD fecha_hasta
         END IF

         DECLARE cur_fecha CURSOR FOR
         SELECT "G"
         FROM   com_comis_resumen
         WHERE  fecha_corte BETWEEN g_fec.fecha_desde AND g_fec.fecha_hasta
         AND    nivel = 1
         OPEN cur_fecha
         FETCH cur_fecha
         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "NO EXISTEN DATOS EN ESTE PERIODO"
            CLOSE cur_fecha
            NEXT FIELD fecha_desde
         END IF
         CLOSE cur_fecha

      ON KEY (ESC)
         IF g_fec.fecha_desde IS NULL THEN
            ERROR "La fecha NO pude ser nula"
            NEXT FIELD fecha_desde
         END IF

         IF g_fec.fecha_hasta IS NULL THEN
            ERROR "La fecha NO pude ser nula"
            NEXT FIELD fecha_hasta
         END IF

         LET INT_FLAG = FALSE

         EXIT INPUT
      ON KEY (INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      ERROR "PROCESO CANCELADO"
      SLEEP 2 
      RETURN
   END IF
   
   IF tipo_rep = "p" THEN
      CALL Genera_rep_promotor()
   ELSE
      CALL Genera_rep_global()
   END IF

   PROMPT "REPORTE GENERADO, OPRIMA ENTER PARA SALIR" for opc

END FUNCTION

FUNCTION Genera_rep_promotor()
   LET hora = TIME
 
   LET vconsecutivo = 0

   LET salida = param.ruta_spool CLIPPED,"/",usuario CLIPPED,
                ".PAGO_PROMOTORES.",hoy USING "DD-MM-YYYY",".",hora CLIPPED

   START REPORT rep_comision TO salida

   DECLARE cur_com CURSOR FOR
   SELECT a.codven,
          b.paterno,
          b.materno,
          b.nombres,
          b.diag_proceso,
          a.comis_calculada
--          a.total_comision
   FROM   com_comis_resumen a,
          pro_mae_promotor  b
   WHERE  a.codven = b.codven
   AND    a.fecha_corte BETWEEN g_fec.fecha_desde AND g_fec.fecha_hasta
   AND    a.nivel = 1
   
   FOREACH cur_com INTO g_reg.*
      OUTPUT TO REPORT rep_comision(g_reg.*)
   END FOREACH

   FINISH REPORT rep_comision

   LET vimprime = "lp ",salida
--   LET vimprime = "vi ",salida

   RUN vimprime

END FUNCTION

REPORT rep_comision(g_reg)
   DEFINE g_reg RECORD 
      codven         CHAR(10),
      paterno        CHAR(40),
      materno        CHAR(40),
      nombres        CHAR(40),
      diag_proceso   CHAR(02),
      monto_comision DECIMAL(12,2)
   END RECORD

   OUTPUT
      PAGE LENGTH   90
      TOP MARGIN    0
      BOTTOM MARGIN 0
      RIGHT MARGIN  0
      LEFT MARGIN   0

   FORMAT
      PAGE HEADER

         PRINT '\033e\033(s218T\033(s12H\033(s7B'

         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'

         PRINT COLUMN 005, "COML001",
               COLUMN 025, "Pago de comisiones a Promotores del periodo ",
                           g_fec.fecha_desde USING "DD-MM-YYYY"," al ",
                           g_fec.fecha_hasta USING "DD-MM-YYYY",
               COLUMN 111, TODAY             USING "DD-MM-YYYY"

         SKIP 5 LINES
                         -- 678901234567890123456789012345678901234567890
         PRINT COLUMN 005, "Consecutivo",
                         -- 01234567890123456789012345678901234567890
               COLUMN 024, "Registro Consar",
                         -- 4567890123456789012345678901234567890
               COLUMN 050, "Nombre del Promotor",
                         -- 01234567890123456789012345678901234567890
               COLUMN 100, "Estado",
                         -- 01234567890123456789012345678901234567890
               COLUMN 114, "Importe"

          SKIP 3 LINES

      ON EVERY ROW
         LET vconsecutivo = vconsecutivo + 1

         PRINT COLUMN 009, vconsecutivo    USING "-----",
               COLUMN 026, g_reg.codven,
               COLUMN 050, g_reg.paterno[1,15]   CLIPPED,
               COLUMN 066, g_reg.materno[1,15]   CLIPPED,
               COLUMN 077, g_reg.nombres[1,15]   CLIPPED,
               COLUMN 102, g_reg.diag_proceso,
               COLUMN 110, g_reg.monto_comision  USING "---,---.--"
      ON LAST ROW
         PRINT COLUMN 005, "--------------------------------------------------------------------------------------------------------------------"
         PRINT COLUMN 009, COUNT(*)                  USING "-----",
               COLUMN 110, SUM(g_reg.monto_comision) USING "---,---.--"
END REPORT

FUNCTION Responsable()

   INPUT BY NAME g_fec.* 
      AFTER FIELD fecha_desde
         IF g_fec.fecha_desde IS NULL THEN
            ERROR "La fecha NO pude ser nula"
            NEXT FIELD fecha_desde
         END IF

      AFTER FIELD fecha_hasta
         IF g_fec.fecha_hasta IS NULL THEN
            ERROR "La fecha NO pude ser nula"
            NEXT FIELD fecha_hasta
         END IF

         DECLARE cur_fecha2 CURSOR FOR
         SELECT "G"
         FROM   com_comis_resumen
         WHERE  fecha_corte BETWEEN g_fec.fecha_desde AND g_fec.fecha_hasta
         AND    nivel <> 1
         OPEN cur_fecha2
         FETCH cur_fecha
         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "NO EXISTEN DATOS EN ESTE PERIODO"
            CLOSE cur_fecha2
            NEXT FIELD fecha_desde
         END IF
         CLOSE cur_fecha2

      ON KEY (ESC)
         IF g_fec.fecha_desde IS NULL THEN
            ERROR "La fecha NO pude ser nula"
            NEXT FIELD fecha_desde
         END IF

         IF g_fec.fecha_hasta IS NULL THEN
            ERROR "La fecha NO pude ser nula"
            NEXT FIELD fecha_hasta
         END IF

         LET INT_FLAG = FALSE

         EXIT INPUT
      ON KEY (INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      ERROR "PROCESO CANCELADO"
      SLEEP 2 
      RETURN
   END IF
   
   CALL Genera_rep_responsable()

   PROMPT "REPORTE GENERADO, OPRIMA ENTER PARA SALIR" for opc

END FUNCTION

FUNCTION Genera_rep_responsable()
   LET hora = TIME
 
   LET vconsecutivo = 0

   LET salida = param.ruta_spool CLIPPED,"/",usuario CLIPPED,
                ".PAGO_RESPONSABLE.",hoy USING "DD-MM-YYYY",".",hora CLIPPED

   START REPORT rep_responsable TO salida

   DECLARE cur_com2 CURSOR FOR
   SELECT a.codven,
          b.paterno,
          b.materno,
          b.nombres,
          b.diag_proceso,
          a.total_comision
   FROM   com_comis_resumen a,
          pro_mae_promotor  b
   WHERE  a.codven = b.codven
   AND    a.fecha_corte BETWEEN g_fec.fecha_desde AND g_fec.fecha_hasta
   AND    a.nivel = 1
   
   FOREACH cur_com2 INTO g_reg.*
      OUTPUT TO REPORT rep_responsable(g_reg.*)
   END FOREACH

   FINISH REPORT rep_responsable

   LET vimprime = "lp ",salida

   RUN vimprime

END FUNCTION

REPORT rep_responsable(g_reg)
   DEFINE g_reg RECORD 
      codven         CHAR(10),
      paterno        CHAR(40),
      materno        CHAR(40),
      nombres        CHAR(40),
      diag_proceso   CHAR(02),
      monto_comision DECIMAL(12,2)
   END RECORD

   OUTPUT
      PAGE LENGTH   90
      TOP MARGIN    0
      BOTTOM MARGIN 0
      RIGHT MARGIN  0
      LEFT MARGIN   0

   FORMAT
      PAGE HEADER

         PRINT '\033e\033(s218T\033(s12H\033(s7B'

         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'

         PRINT COLUMN 005, "COML001",
               COLUMN 025, "Pago de comisiones a Responable del periodo ",
                           g_fec.fecha_desde USING "DD-MM-YYYY"," al ",
                           g_fec.fecha_hasta USING "DD-MM-YYYY",
               COLUMN 111, TODAY             USING "DD-MM-YYYY"

         SKIP 5 LINES
                         -- 678901234567890123456789012345678901234567890
         PRINT COLUMN 005, "Consecutivo",
                         -- 01234567890123456789012345678901234567890
               COLUMN 024, "Registro Consar",
                         -- 4567890123456789012345678901234567890
               COLUMN 050, "Nombre del Promotor",
                         -- 01234567890123456789012345678901234567890
               COLUMN 100, "Estado",
                         -- 01234567890123456789012345678901234567890
               COLUMN 114, "Importe"

          SKIP 3 LINES

      ON EVERY ROW
         LET vconsecutivo = vconsecutivo + 1

         PRINT COLUMN 009, vconsecutivo    USING "-----",
               COLUMN 026, g_reg.codven,
               COLUMN 050, g_reg.paterno[1,15]   CLIPPED,
               COLUMN 066, g_reg.materno[1,15]   CLIPPED,
               COLUMN 077, g_reg.nombres[1,15]   CLIPPED,
               COLUMN 102, g_reg.diag_proceso,
               COLUMN 110, g_reg.monto_comision  USING "---,---.--"
      ON LAST ROW
         PRINT COLUMN 005, "--------------------------------------------------------------------------------------------------------------------"
         PRINT COLUMN 009, COUNT(*)                  USING "-----",
               COLUMN 110, SUM(g_reg.monto_comision) USING "---,---.--"
END REPORT

FUNCTION Bono()

   INPUT BY NAME g_fec.* 
      AFTER FIELD fecha_desde
         IF g_fec.fecha_desde IS NULL THEN
            ERROR "La fecha NO pude ser nula"
            NEXT FIELD fecha_desde
         END IF

      AFTER FIELD fecha_hasta
         IF g_fec.fecha_hasta IS NULL THEN
            ERROR "La fecha NO pude ser nula"
            NEXT FIELD fecha_hasta
         END IF

         DECLARE cur_fecha3 CURSOR FOR
         SELECT "G"
         FROM   com_bono_resumen
         WHERE  fecha_corte BETWEEN g_fec.fecha_desde AND g_fec.fecha_hasta
         AND    nivel <> 1
         OPEN cur_fecha3
         FETCH cur_fecha3
         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "NO EXISTEN DATOS EN ESTE PERIODO"
            CLOSE cur_fecha3
            NEXT FIELD fecha_desde
         END IF
         CLOSE cur_fecha3

      ON KEY (ESC)
         IF g_fec.fecha_desde IS NULL THEN
            ERROR "La fecha NO pude ser nula"
            NEXT FIELD fecha_desde
         END IF

         IF g_fec.fecha_hasta IS NULL THEN
            ERROR "La fecha NO pude ser nula"
            NEXT FIELD fecha_hasta
         END IF

         LET INT_FLAG = FALSE

         EXIT INPUT
      ON KEY (INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      ERROR "PROCESO CANCELADO"
      SLEEP 2 
      RETURN
   END IF
   
   CALL Genera_rep_bono()

   PROMPT "REPORTE GENERADO, OPRIMA ENTER PARA SALIR" for opc

END FUNCTION

FUNCTION Genera_rep_bono()
   LET hora = TIME
 
   LET vconsecutivo = 0

   LET salida = param.ruta_spool CLIPPED,"/",usuario CLIPPED,
                ".PAGO_BONO.",hoy USING "DD-MM-YYYY",".",hora CLIPPED

   START REPORT rep_bono TO salida

   DECLARE cur_com3 CURSOR FOR
   SELECT a.codven,
          b.paterno,
          b.materno,
          b.nombres,
          b.diag_proceso,
          a.total_bono
   FROM   com_bono_resumen a,
          pro_mae_promotor  b
   WHERE  a.codven = b.codven
   AND    a.fecha_corte BETWEEN g_fec.fecha_desde AND g_fec.fecha_hasta
   
   FOREACH cur_com3 INTO g_reg.*
      OUTPUT TO REPORT rep_bono(g_reg.*)
   END FOREACH

   FINISH REPORT rep_bono

   LET vimprime = "lp ",salida

   RUN vimprime

END FUNCTION

REPORT rep_bono(g_reg)
   DEFINE g_reg RECORD 
      codven         CHAR(10),
      paterno        CHAR(40),
      materno        CHAR(40),
      nombres        CHAR(40),
      diag_proceso   CHAR(02),
      monto_comision DECIMAL(12,2)
   END RECORD

   OUTPUT
      PAGE LENGTH   90
      TOP MARGIN    0
      BOTTOM MARGIN 0
      RIGHT MARGIN  0
      LEFT MARGIN   0

   FORMAT
      PAGE HEADER

         PRINT '\033e\033(s218T\033(s12H\033(s7B'

         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'

         PRINT COLUMN 005, "COML001",
               COLUMN 025, "Pago de Bono del periodo ",
                           g_fec.fecha_desde USING "DD-MM-YYYY"," al ",
                           g_fec.fecha_hasta USING "DD-MM-YYYY",
               COLUMN 111, TODAY             USING "DD-MM-YYYY"

         SKIP 5 LINES
                         -- 678901234567890123456789012345678901234567890
         PRINT COLUMN 005, "Consecutivo",
                         -- 01234567890123456789012345678901234567890
               COLUMN 024, "Registro Consar",
                         -- 4567890123456789012345678901234567890
               COLUMN 050, "Nombre del Promotor",
                         -- 01234567890123456789012345678901234567890
               COLUMN 100, "Estado",
                         -- 01234567890123456789012345678901234567890
               COLUMN 114, "Importe"

          SKIP 3 LINES

      ON EVERY ROW
         LET vconsecutivo = vconsecutivo + 1

         PRINT COLUMN 009, vconsecutivo    USING "-----",
               COLUMN 026, g_reg.codven,
               COLUMN 050, g_reg.paterno[1,15]   CLIPPED,
               COLUMN 066, g_reg.materno[1,15]   CLIPPED,
               COLUMN 077, g_reg.nombres[1,15]   CLIPPED,
               COLUMN 102, g_reg.diag_proceso,
               COLUMN 110, g_reg.monto_comision  USING "---,---.--"
      ON LAST ROW
         PRINT COLUMN 005, "--------------------------------------------------------------------------------------------------------------------"
         PRINT COLUMN 009, COUNT(*)                  USING "-----",
               COLUMN 110, SUM(g_reg.monto_comision) USING "---,---.--"
END REPORT

FUNCTION Genera_rep_global()
   LET hora = TIME
 
   LET vconsecutivo = 0

   LET salida = param.ruta_spool CLIPPED,"/",usuario CLIPPED,
                ".PAGO_GLOBAL.",hoy USING "DD-MM-YYYY",".",hora CLIPPED

   START REPORT rep_global TO salida

   DECLARE cur_global CURSOR FOR
   SELECT fecha_corte,
          total_afiliados,
          coduni_n1,
          prom_salario_base,
          comis_calculada
   FROM   com_comis_resumen
   WHERE  fecha_corte BETWEEN g_fec.fecha_desde AND g_fec.fecha_hasta
   AND    nivel = 1
   
   FOREACH cur_global INTO g_glo.*
      OUTPUT TO REPORT rep_global(g_glo.*)
   END FOREACH

   FINISH REPORT rep_global

   LET vimprime = "lp ",salida
--   LET vimprime = "vi ",salida

   RUN vimprime

END FUNCTION

REPORT rep_global(g_glo)
   DEFINE g_glo RECORD
       fecha_corte       DATE,
       total_afiliados   INTEGER,
       coduni_n1         CHAR(10),
       prom_salario_base DECIMAL(12,2),
       comis_calculada   DECIMAL(12,2)
   END RECORD

   OUTPUT
      PAGE LENGTH   90
      TOP MARGIN    0
      BOTTOM MARGIN 0
      RIGHT MARGIN  0
      LEFT MARGIN   0

   FORMAT
      PAGE HEADER

         PRINT '\033e\033(s218T\033(s12H\033(s7B'

         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'

         PRINT COLUMN 005, "COML001",
               COLUMN 025, "Pago de comisiones Globales del periodo ",
                           g_fec.fecha_desde USING "DD-MM-YYYY"," al ",
                           g_fec.fecha_hasta USING "DD-MM-YYYY",
               COLUMN 111, TODAY             USING "DD-MM-YYYY"

         SKIP 5 LINES
                         -- 678901234567890123456789012345678901234567890
         PRINT COLUMN 005, "Grupo Venta",
                         -- 01234567890123456789012345678901234567890
               COLUMN 024, "Total Afiliados",
                         -- 4567890123456789012345678901234567890
               COLUMN 050, "Promedio salario",   
                         -- 01234567890123456789012345678901234567890
               COLUMN 100, "Total Comision"
                         -- 01234567890123456789012345678901234567890

          SKIP 3 LINES

      ON EVERY ROW
         LET vconsecutivo = vconsecutivo + 1

         PRINT COLUMN 009, g_glo.coduni_n1,
               COLUMN 026, g_glo.total_afiliados    USING "------",
               COLUMN 050, g_glo.prom_salario_base  USING "---,---.--",
               COLUMN 066, g_glo.comis_calculada    USING "---,---.--"
      ON LAST ROW
         PRINT COLUMN 005, "--------------------------------------------------------------------------------------------------------------------"
         PRINT COLUMN 009, COUNT(*)                   USING "-----",
               COLUMN 066, SUM(g_glo.comis_calculada) USING "---,---.--"
END REPORT
