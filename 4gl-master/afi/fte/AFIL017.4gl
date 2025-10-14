-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => AFIL017                                                   --
-- Descripcion  => GENERA ARCHIVO PLANO DE RECHAZOS A REENVIAR               --
-- Sistema      => AFI                                                       --
-- Por          => GERARDO ALFONSO VEGA PAREDES                              --
-- Fecha        => 26 julio 2002.                                            --
-- Modificado   =>                                                           --
-- Fecha        =>                                                           --
-------------------------------------------------------------------------------
DATABASE safre_af

GLOBALS
   DEFINE g_reg RECORD
      n_seguro       CHAR(11),
      n_folio          DECIMAL(10,0),
      tipo_solicitud SMALLINT,
      paterno        CHAR(40),
      materno        CHAR(40),
      nombres        CHAR(40),
      n_rfc          CHAR(13),
      n_unico        CHAR(18),
      frecafor       DATE,
      usuario        CHAR(08)
   END RECORD

   DEFINE g_param RECORD LIKE seg_modulo.*

   DEFINE hoy      DATE,
          hora     CHAR(08),
          hora1    CHAR(04),
          usuario  CHAR(08),
          opc      CHAR(01),
          g_eco    CHAR(100),
          nom_arch CHAR(100)

   DEFINE cla_where CHAR(300),
          cla_sel   CHAR(900)

   DEFINE salida CHAR(300)

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

   LET salida = g_param.ruta_listados CLIPPED,"/",usuario CLIPPED,
                ".REENV.", hoy USING "DDMMYY","_", hora1

   LET nom_arch = usuario CLIPPED,
                  ".REENV.", hoy USING "DDMMYY","_", hora1

END FUNCTION

FUNCTION Genera_consulta()
   OPEN WINDOW ventana1 AT 2,2 WITH FORM "AFIL0171" ATTRIBUTE(BORDER)

   DISPLAY " AFIL017              GENERA ARCHIVO DE RECHAZOS A REENVIAR                    " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   CONSTRUCT cla_where ON frecafor FROM frecafor
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

   LET cla_sel = "SELECT s.n_seguro,",
                        "s.n_folio,",
                        "s.tipo_solicitud,",
                        "s.paterno,",
                        "s.materno,",
                        "s.nombres,",
                        "s.n_rfc,",
                        "s.n_unico,",
                        "s.frecafor,",
                        "b.usuario ",
                 "FROM   afi_solicitud s, afi_ctr_logico b ",
                 "WHERE  s.status_interno = 20 ",
                 "AND    s.n_seguro       = b.n_seguro ",
                 "AND    s.n_folio        = b.n_folio ",
                 "AND    s.tipo_solicitud = b.tipo_solicitud ",
                 "AND    b.operacion = 'RECHAZO A REENVIAR' "," ",
                 "AND  ",cla_where CLIPPED," ",
                 "ORDER  BY s.n_seguro "

   START REPORT rep_solicitud TO salida

      PREPARE claexe FROM cla_sel
      DECLARE cur_solicitud CURSOR FOR claexe

      FOREACH cur_solicitud INTO g_reg.*
         OUTPUT TO REPORT rep_solicitud(g_reg.*)
      END FOREACH

   FINISH REPORT rep_solicitud

   LET g_eco = "echo ", nom_arch CLIPPED, " > ",
               g_param.ruta_listados CLIPPED,"/rescate.REENV.",
               hoy USING "DDMMYY","_",hora1

   RUN g_eco

   ERROR ""

   PROMPT "REPORTE FINALIZADO OPRIMA [ENTER] P/SALIR..." for opc

END FUNCTION

REPORT rep_solicitud(g_reg)

   DEFINE g_reg RECORD
      n_seguro       CHAR(11),
      n_folio          DECIMAL(10,0),
      tipo_solicitud SMALLINT,
      paterno        CHAR(40),
      materno        CHAR(40),
      nombres        CHAR(40),
      n_rfc          CHAR(13),
      n_unico        CHAR(18),
      frecafor       DATE,
      usuario        CHAR(08)
   END RECORD

   OUTPUT
      PAGE   LENGTH 90
      TOP    MARGIN 0
      BOTTOM MARGIN 0
      RIGHT  MARGIN 0
      LEFT   MARGIN 0

   FORMAT
      ON EVERY ROW
         PRINT COLUMN 001, g_reg.*

     
END REPORT
