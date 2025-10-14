-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => AFIL016                                                   --
-- Descripcion  => REPORTE DE REGISTORS MODIFICADOS.                         --
-- Sistema      => AFI                                                       --
-- Por          => GERARDO ALFONSO VEGA PAREDES                              --
-- Fecha        => 25 julio 2002.                                            --
-- Modificado   =>                                                           --
-- Fecha        =>                                                           --
-------------------------------------------------------------------------------
DATABASE safre_af

GLOBALS
   DEFINE g_reg RECORD
       usuario          CHAR(08),
       n_seguro         LIKE afi_mae_afiliado.n_seguro,
       n_unico          LIKE afi_mae_afiliado.n_unico,
       n_rfc            LIKE afi_mae_afiliado.n_rfc,
       paterno          CHAR(40),
       materno          CHAR(40),
       nombres          CHAR(40),
       fena             DATE,
       cod_promotor     LIKE afi_mae_afiliado.cod_promotor,
       frecafor         LIKE afi_mae_afiliado.frecafor,
       folio_solicitud  CHAR(08),
       tipo_solicitud   SMALLINT,
       sexo             LIKE afi_mae_afiliado.sexo,
       estadon          LIKE afi_mae_afiliado.estadon,
       nacionalidad     LIKE afi_mae_afiliado.nacionalidad,
       tip_prob         LIKE afi_mae_afiliado.tip_prob,
       fol_prob         LIKE afi_mae_afiliado.fol_prob,
       doc_prob         LIKE afi_mae_afiliado.doc_prob,
       ind_infonavit    LIKE afi_mae_afiliado.ind_infonavit, 
       cod_error_origen SMALLINT,
       fecha_modifica   DATE
   END RECORD

   DEFINE g_param RECORD LIKE seg_modulo.*

   DEFINE hoy      DATE,
          hora     CHAR(08),
          hora1    CHAR(04),
          usuario  CHAR(08),
          opc      CHAR(01),
          g_eco    CHAR(100),
          nom_arch CHAR(100)

   DEFINE cla_where CHAR(1000),
          cla_sel   CHAR(1000)

   DEFINE salida   CHAR(300),
          vimprime CHAR(500)

   DEFINE mostrar CHAR(1000)

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
                ".MOD.", hoy USING "DDMMYY","_", hora1

   LET nom_arch = usuario CLIPPED,
                  ".MOD.", hoy USING "DDMMYY","_", hora1

END FUNCTION

FUNCTION Genera_consulta()
   OPEN WINDOW ventana1 AT 2,2 WITH FORM "AFIL0161" ATTRIBUTE(BORDER)

   DISPLAY " AFIL016               REPORTE REGISTROS MODIFICADOS                         " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   CONSTRUCT cla_where ON fecha_modifica FROM fecha_modifica
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

   LET cla_sel = "SELECT b.usuario,",
                        "b.n_seguro,",
                        "b.n_unico,",
                        "b.n_rfc,",
                        "b.paterno,",
                        "b.materno,",
                        "b.nombres,",
                        "b.fena,",
                        "a.cod_promotor,",
                        "a.frecafor,",
                        "b.n_folio,",
                        "b.tipo_solicitud,",
                        "b.sexo,",
                        "b.estadon,",
                        "b.nacionalidad,",
                        "b.tip_prob,",
                        "b.fol_prob,",
                        "b.doc_prob,",
                        "a.ind_infonavit,",
                        "b.cod_error_origen,",
                        "b.fecha_modifica ",
                 "FROM   afi_mae_afiliado a, afi_mae_modifica b ",
                 "WHERE  a.n_seguro = b.n_seguro ",
                 "AND    a.n_folio  = b.n_folio ",
                 "AND    a.tipo_solicitud = b.tipo_solicitud ",
                 "AND    a.status_interno = 130 ",
                 "AND ",cla_where CLIPPED," ",
                 "ORDER  BY b.usuario,b.tipo_solicitud "

      --LET mostrar = "echo ",cla_sel clipped, " > x.sql"
      --RUN mostrar 

   START REPORT rep_solicitud TO salida

      PREPARE claexe FROM cla_sel
      DECLARE cur_solicitud CURSOR FOR claexe

      FOREACH cur_solicitud INTO g_reg.*
         OUTPUT TO REPORT rep_solicitud(g_reg.*)
      END FOREACH

   FINISH REPORT rep_solicitud

   LET g_eco = "echo ", nom_arch CLIPPED, " > ",              
               g_param.ruta_listados CLIPPED,"/rescate.MOD.",
               hoy USING "DDMMYY","_",hora1                   

   RUN g_eco

   LET vimprime = "lp ",salida
   RUN vimprime

   ERROR ""

   PROMPT "REPORTE FINALIZADO OPRIMA [ENTER] P/SALIR..." for opc

END FUNCTION

REPORT rep_solicitud(g_reg)

   DEFINE g_reg RECORD
       usuario          CHAR(08),
       n_seguro         LIKE afi_mae_afiliado.n_seguro,
       n_unico          LIKE afi_mae_afiliado.n_unico,
       n_rfc            LIKE afi_mae_afiliado.n_rfc,
       paterno          CHAR(40),
       materno          CHAR(40),
       nombres          CHAR(40),
       fena             DATE,
       cod_promotor     LIKE afi_mae_afiliado.cod_promotor,
       frecafor         LIKE afi_mae_afiliado.frecafor,
       folio_solicitud  CHAR(08),
       tipo_solicitud   SMALLINT,
       sexo             LIKE afi_mae_afiliado.sexo,
       estadon          LIKE afi_mae_afiliado.estadon,
       nacionalidad     LIKE afi_mae_afiliado.nacionalidad,
       tip_prob         LIKE afi_mae_afiliado.tip_prob,
       fol_prob         LIKE afi_mae_afiliado.fol_prob,
       doc_prob         LIKE afi_mae_afiliado.doc_prob,
       ind_infonavit    LIKE afi_mae_afiliado.ind_infonavit,
       cod_error_origen SMALLINT,
       fecha_modifica   DATE
   END RECORD

   OUTPUT
      PAGE   LENGTH 1
      TOP    MARGIN 0
      BOTTOM MARGIN 0
      RIGHT  MARGIN 0
      LEFT   MARGIN 0

   FORMAT
      ON EVERY ROW
         PRINT COLUMN 001, g_reg.*

END REPORT

