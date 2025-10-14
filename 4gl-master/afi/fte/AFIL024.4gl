-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => AFIL024                                                   --
-- Descripcion  => REPORTE DE ESTADO DE AFILIACION tipo_solicitud 1 y 2      --
-- Sistema      => AFI                                                       --
-- Por          => GERARDO ALFONSO VEGA PAREDES                              --
-- Fecha        => 01 diciembre 2002.                                        --
-- Modificado   =>                                                           --
-- Fecha        =>                                                           --
-------------------------------------------------------------------------------
DATABASE safre_af

GLOBALS
   DEFINE g_reg RECORD
      n_seguro         CHAR(11),
      n_unico          CHAR(18),
      paterno          CHAR(40),
      materno          CHAR(40),
      nombres          CHAR(40),
      status_interno   SMALLINT,
      frecafor         DATE,
      fentcons         DATE
   END RECORD

   DEFINE g_param RECORD LIKE seg_modulo.*

   DEFINE hoy     DATE,
          hora    CHAR(08),
          usuario CHAR(08),
          opc     CHAR(01)

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

   LET hora = hora[1,2],hora[4,5] CLIPPED

   LET salida = g_param.ruta_listados CLIPPED,"/",usuario CLIPPED,
                ".REPORTE_ESTADO_AFILIACION.",
                hoy USING "DDMMYY","_",
                hora

END FUNCTION

FUNCTION Genera_consulta()
   OPEN WINDOW ventana1 AT 2,2 WITH FORM "AFIL0241" ATTRIBUTE(BORDER)

   DISPLAY " < ESC > Aceptar Datos                                      < Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " AFIL024               REPORTE ESTADO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   CONSTRUCT cla_where ON frecafor,fentcons FROM frecafor,fentcons
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

   LET cla_sel = "SELECT n_seguro,",
                        "n_unico,",
                        "paterno,",
                        "materno,",
                        "nombres,",
                        "status_interno,",
                        "frecafor,",
                        "fentcons ",
                 "FROM   afi_mae_afiliado ",
                 "WHERE ",cla_where CLIPPED," ",
		 #" AND  tipo_solicitud = 2 ",
                 "ORDER  BY status_interno "

   START REPORT rep_solicitud TO salida

      PREPARE claexe FROM cla_sel
      DECLARE cur_solicitud CURSOR FOR claexe

      FOREACH cur_solicitud INTO g_reg.*
         OUTPUT TO REPORT rep_solicitud(g_reg.*)
      END FOREACH

   FINISH REPORT rep_solicitud

   LET vimprime = "lp ",salida
   RUN vimprime

   ERROR ""

   PROMPT "REPORTE FINALIZADO OPRIMA [ENTER] P/SALIR..." for opc

END FUNCTION

REPORT rep_solicitud(g_reg)

   DEFINE g_reg RECORD
      n_seguro         CHAR(11),
      n_unico          CHAR(18),
      paterno          CHAR(40),
      materno          CHAR(40),
      nombres          CHAR(40),
      status_interno   SMALLINT,
      frecafor         DATE,
      fentcons         DATE
   END RECORD

   DEFINE vnombre      CHAR(25),
	  vdescripcion CHAR(25)

   OUTPUT
      PAGE   LENGTH 90
      TOP    MARGIN 0
      BOTTOM MARGIN 0
      RIGHT  MARGIN 0
      LEFT   MARGIN 0

   FORMAT
      PAGE HEADER
--          10        20        30        40        50        60        70        80        90        10       
-- 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
--          NSS           CURP               NOMBRE             EDO.       DESCRIPCION           F.RECEP.    F.CERT.
--     XXXXXXXXXXX XXXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXX XXX XXXXXXXXXXXXXXXXXXXXXXXXX  DD-MM-YYYY  DD-MM-YYYY

         PRINT COLUMN 001, "AFIL024",
               COLUMN 030, "ESTADOS DE AFILIACION",
               COLUMN 070, TODAY USING "DD-MM-YYYY"
                            
         PRINT COLUMN 001, "------------------------------------------------------------------------------------------"
         PRINT COLUMN 010, "NSS",
               COLUMN 024, "CURP",
               COLUMN 043, "NOMBRE",
               COLUMN 062, "EDO.",
               COLUMN 073, "DESCRIPCION",
               COLUMN 095, "F.RECEP.",
	       COLUMN 107, "F.CERT."
         PRINT COLUMN 001, "------------------------------------------------------------------------------------------"

      ON EVERY ROW

	 LET vnombre = g_reg.paterno CLIPPED," ",
		       g_reg.materno CLIPPED," ",
		       g_reg.nombres CLIPPED

         SELECT estado_desc
	 INTO   vdescripcion
	 FROM   tab_status_afi
	 WHERE  estado_cod = g_reg.status_interno

         PRINT COLUMN 005, g_reg.n_seguro,
               COLUMN 017, g_reg.n_unico,
               COLUMN 036, vnombre,
               COLUMN 062, g_reg.status_interno USING "&&&",
	       COLUMN 066, vdescripcion,
               COLUMN 093, g_reg.frecafor    USING 'DD-MM-YYYY',
               COLUMN 105, g_reg.fentcons    USING 'DD-MM-YYYY'
END REPORT
