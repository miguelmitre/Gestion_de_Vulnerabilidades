-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => AFIL018                                                   --
-- Descripcion  => REPORTE DE REGISTORS CAPTURADOS.                          --
-- Sistema      => AFI                                                       --
-- Por          => GERARDO ALFONSO VEGA PAREDES                              --
-- Fecha        => 25 julio 2002.                                            --
-- Modificado   =>                                                           --
-- Fecha        =>                                                           --
-------------------------------------------------------------------------------
DATABASE safre_af

GLOBALS
   DEFINE g_reg RECORD
      usuario           CHAR(08),
      n_seguro          CHAR(11),
      n_folio          DECIMAL(10,0),
      tipo_solicitud    SMALLINT,
      paterno           CHAR(40),
      materno           CHAR(40),
      nombres           CHAR(40),
      nombre            CHAR(50),
      cod_promotor      CHAR(10),
      estado_sol        CHAR(1),
      estado_exp        CHAR(1),
      lote              INTEGER,
      frecafor          DATE,
      fecha_elaboracion DATE
   END RECORD

   DEFINE g_param RECORD LIKE seg_modulo.*

   DEFINE hoy      DATE,
          num      INTEGER,
          opc      CHAR(1),
	  enter    CHAR(1),
          hora1    CHAR(4),
          usuario  CHAR(8),
          hora     CHAR(8),
          g_eco    CHAR(100),
          nom_arch CHAR(100)

   DEFINE cla_where CHAR(300),
          cla_sel   CHAR(900)

   DEFINE salida   CHAR(300),
          vimprime CHAR(500)

END GLOBALS

MAIN
   OPTIONS                 
      PROMPT LINE LAST,    
      INPUT WRAP,          
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST    
   DEFER INTERRUPT         

   CALL Inicializa()

   CALL Genera_consulta()

END MAIN

FUNCTION Inicializa()

   SELECT *,
          USER
   INTO   g_param.*,
          usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "afi"

   LET hoy = TODAY

   LET hora = TIME

   LET hora1 = hora[1,2],hora[4,5]

   LET num = 1

   LET salida = g_param.ruta_listados CLIPPED,"/",usuario CLIPPED,
                ".DIARIA.", hoy USING "DDMMYY","_", hora1

   LET nom_arch = usuario CLIPPED,
                  ".DIARIA.", hoy USING "DDMMYY","_", hora1

END FUNCTION

FUNCTION Genera_consulta()
   OPEN WINDOW ventana1 AT 2,2 WITH FORM "AFIL0181" ATTRIBUTE(BORDER)

   DISPLAY " AFIL018               REPORTE DE CAPTURA DIARIA                             " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   CONSTRUCT cla_where ON a.frecafor, a.tipo_solicitud 
		     FROM frecafor, tipo_solicitud

      ON KEY (ESC)
         LET INT_FLAG = FALSE
         EXIT CONSTRUCT
      ON KEY (INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT CONSTRUCT 
   END CONSTRUCT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      ERROR "PROCESO CANCELADO..."
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana1
      RETURN
   END IF

   ERROR "GENERANDO REPORTE..."

   LET cla_sel = "SELECT  a.usuario,",
                        " a.n_seguro,",
                        " a.n_folio,",
                        " a.tipo_solicitud,",
                        " a.paterno,",
                        " a.materno,",
                        " a.nombres,",
                        "'',",
                        " a.cod_promotor,",
                        " b.estado_sol,",
                        " b.estado_exp, ",
                        "'',",
                        " a.frecafor, ",
                        " a.fecha_elaboracion ",
                 "FROM   afi_solicitud a, OUTER(afi_recepcion b) ",
                 "WHERE a.n_seguro = b.n_seguro ",
                 "AND   a.n_folio = b.n_folio ",
                 "AND   a.tipo_solicitud = b.tipo_solicitud ",
                 "AND   ",cla_where CLIPPED," ",
                 "ORDER  BY a.usuario, a.n_seguro "

   START REPORT rep_solicitud TO salida

      PREPARE claexe FROM cla_sel
      DECLARE cur_solicitud CURSOR FOR claexe

      FOREACH cur_solicitud INTO g_reg.*
         LET g_reg.nombre = g_reg.paterno CLIPPED," ",
                            g_reg.materno CLIPPED," ",
                            g_reg.nombres CLIPPED

         SELECT MAX(e.lote)
         INTO   g_reg.lote
         FROM   afi_expediente e
         WHERE  e.n_seguro = g_reg.n_seguro
         AND    e.n_folio = g_reg.n_folio
         AND    e.tipo_solicitud = g_reg.tipo_solicitud

         OUTPUT TO REPORT rep_solicitud(g_reg.*)
      END FOREACH

   FINISH REPORT rep_solicitud

   LET g_eco = "echo ", nom_arch CLIPPED, " > ",
               g_param.ruta_listados CLIPPED,"/rescate.DIARIA.",
               hoy USING "DDMMYY","_",hora1

    RUN g_eco

   LET vimprime = "lp ",salida
   RUN vimprime

   ERROR ""

   PROMPT "REPORTE FINALIZADO OPRIMA [ENTER] P/SALIR..." for opc

END FUNCTION

REPORT rep_solicitud(g_reg)

   DEFINE g_reg RECORD
      usuario           CHAR(08),
      n_seguro          CHAR(11),
      n_folio          DECIMAL(10,0),
      tipo_solicitud    SMALLINT,
      paterno           CHAR(40),
      materno           CHAR(40),
      nombres           CHAR(40),
      nombre            CHAR(50),
      cod_promotor      CHAR(10),
      estado_sol        CHAR(1),
      estado_exp        CHAR(1),
      lote              INTEGER,
      frecafor          DATE,
      fecha_elaboracion DATE
   END RECORD

   OUTPUT
      PAGE   LENGTH 170
      TOP    MARGIN 0
      BOTTOM MARGIN 0
      RIGHT  MARGIN 0
      LEFT   MARGIN 0

   FORMAT
      PAGE HEADER
	      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
--          10        20        30        40        50        60        70        80        90        10       
-- 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
--     USUARIO      NSS        FOLIO   T.SOL.  COD ERROR MOD    FECHA MODIFICA
--     XXXXXXXX XXXXXXXXXXX XXXXXXXXXX    X       XXXXXXXXXX      DD-MM-YYYY

         PRINT COLUMN 001, "AFIL018",
               COLUMN 030, "REPORTE CAPTURA DIARIA",
               COLUMN 070, TODAY USING "DD-MM-YYYY"
                            
         PRINT COLUMN 001, "------------------------------------------------------------------------------------------"

         PRINT COLUMN 001, "NUM ",
               COLUMN 005, "USUARIO",
               COLUMN 018, "NSS",
               COLUMN 029, "FOLIO",
               COLUMN 037, "T.SOL.",
               COLUMN 045, "NOMBRE",
               COLUMN 097, "PROMOTOR",
               COLUMN 109, "EDO.SOL.",
               COLUMN 118, "EDO.EXP.",
               COLUMN 127, "LOTE",
               COLUMN 135, "F. RECEP.",
               COLUMN 147, "F. FIRMA. "
         PRINT COLUMN 001, "------------------------------------------------------------------------------------------"

      ON EVERY ROW

         PRINT COLUMN 001, num USING "&&&",
               COLUMN 005, g_reg.usuario,
               COLUMN 014, g_reg.n_seguro,
               COLUMN 026, g_reg.n_folio,
               COLUMN 040, g_reg.tipo_solicitud    USING "#",
               COLUMN 045, g_reg.nombre,
               COLUMN 097, g_reg.cod_promotor,
               COLUMN 112, g_reg.estado_sol,
               COLUMN 121, g_reg.estado_exp,
               COLUMN 127, g_reg.lote USING "#####",
               COLUMN 135, g_reg.frecafor,
               COLUMN 147, g_reg.fecha_elaboracion

               LET num = num +1

END REPORT

