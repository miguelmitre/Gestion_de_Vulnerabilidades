-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => AFIL014                                                   --
-- Descripcion  => GENERA ARCHIVO CONFIRMACIONES DIARIAS.                    --
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
      spaterno         CHAR(40),
      smaterno         CHAR(40),
      snombres         CHAR(40),
      frecafor         DATE,
      tipo_solicitud   SMALLINT,
      n_folio          DECIMAL(10,0),
      n_seguro         CHAR(11),
      n_rfc            CHAR(13),
      estadon          SMALLINT,
      tip_prob         CHAR(01),
      fol_prob         CHAR(10),
      doc_prob         CHAR(16),
      status_interno   SMALLINT,
      cod_afore_ced    SMALLINT,
      folio_edo_cta    CHAR(08),
      cod_promotor     CHAR(10),
      ppaterno         CHAR(40),
      pmaterno         CHAR(40),
      pnombres         CHAR(40)
   END RECORD

   DEFINE g_param RECORD LIKE seg_modulo.*

   DEFINE hoy      DATE,
          hora     CHAR(8),
          hora1    CHAR(4),
          usuario  CHAR(8),
          opc      CHAR(1),
          max_hora CHAR(8),
          g_eco    CHAR(100),
          nom_arch CHAR(100)

   DEFINE cla_where CHAR(300),
          cla_sel   CHAR(900)

   DEFINE salida CHAR(300)
   DEFINE enter  CHAR(01)

END GLOBALS

MAIN
   OPTIONS                 
      PROMPT LINE LAST,    
      INPUT WRAP,          
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST    
#   DEFER INTERRUPT         

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

   LET hora  = TIME
   LET hora1 = hora[1,2],hora[4,5]


   LET salida = g_param.ruta_listados CLIPPED,"/",usuario CLIPPED,
                ".CONF.", hoy USING "DDMMYY","_",hora1
{
   LET salida = "laura.txt"
}
   LET nom_arch = usuario CLIPPED,".CONF.",
                  hoy USING "DDMMYY","_",hora1

   LET cla_sel = "SELECT a.calle,",
                        "a.numero,",
                        "a.depto,",
                        "a.colonia,",
                        "b.deleg_desc,",
                        "c.ciudad_desc,",
                        "d.estad_desc,",
                        "a.codpos, ",
                        "a.marca_envio ",
                 "FROM   afi_domicilio  a,",
                        "tab_delegacion b,",
                        "tab_ciudad     c,",
                        "tab_estado     d ",
                 "WHERE  nss            = ? ",
                 "AND    n_folio        = ? ",
                 "AND    tipo_solicitud = ? ",
                 "AND    a.delega       = b.deleg_cod ",
                 "AND    a.ciudad       = c.ciudad_cod ",
                 "AND    a.estado       = d.estad_cod ",
                 "ORDER BY a.marca_envio desc " CLIPPED

   PREPARE cla_dom FROM cla_sel
   DECLARE cur_dom CURSOR FOR cla_dom



   LET cla_sel = "SELECT cve_lada,",
                        "telefono ",
                 "FROM   afi_telefono ",
                 "WHERE  nss            = ? ",
                 "AND    n_folio        = ? ",
                 "AND    tipo_solicitud = ? " CLIPPED
      
   PREPARE cla_tel FROM cla_sel
   DECLARE cur_tel CURSOR FOR cla_tel



   LET cla_sel = "SELECT estado_sol,",
                        "estado_exp ",
                 "FROM   afi_recepcion ",
                 "WHERE  n_seguro       = ? ",
                 "AND    n_folio        = ? ",
                 "AND    tipo_solicitud = ? " CLIPPED
      
   PREPARE cla_rec FROM cla_sel
   DECLARE cur_rec CURSOR FOR cla_rec



   LET cla_sel = "SELECT MAX(hora) ",
                 "FROM   afi_ctr_logico ",
                 "WHERE  n_seguro       = ? ",
                 "AND    n_folio        = ? ",
                 "AND    tipo_solicitud = ? ",
                 "AND    operacion matches 'CONFIRMA*' "

   PREPARE cla_hor FROM cla_sel
   DECLARE cur_hor CURSOR FOR cla_hor

END FUNCTION

FUNCTION Genera_consulta()
   OPEN WINDOW ventana1 AT 2,2 WITH FORM "AFIL0141" ATTRIBUTE(BORDER)

   DISPLAY " AFIL014            GENERA ARCHIVO CONFIRMACION DIARIA                      " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   CONSTRUCT cla_where ON   c.factualiza,c.tipo_solicitud 
                       FROM factualiza,tipo_solicitud

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

   INITIALIZE cla_sel TO NULL

   LET cla_sel = "SELECT c.usuario,",
                        "s.paterno,",
                        "s.materno,",
                        "s.nombres,",
                        "c.factualiza,",
                        "s.tipo_solicitud,",
                        "s.n_folio,",
                        "s.n_seguro,",
                        "s.n_rfc,",
                        "s.estadon,",
                        "s.tip_prob,",
                        "s.fol_prob,",
                        "s.doc_prob,",
                        "s.status_interno,",
                        "s.cod_afore_ced,",
                        "s.folio_edo_cta,",
                        "s.cod_promotor,",
                        "p.paterno,",
                        "p.materno,",
                        "p.nombres, ",
                        "max(c.hora) ",
                 "FROM   afi_solicitud s,pro_mae_promotor p, ",
                 "OUTER (afi_ctr_logico c) ",
                 "WHERE  s.cod_promotor  = p.cod_promotor ",
                 --"AND    s.status_interno = 15 ",
                 "AND    s.n_seguro = c.n_seguro ",
                 "AND    s.n_folio = c.n_folio ",
                 "AND    s.tipo_solicitud = c.tipo_solicitud ",
                 "AND    s.status_interno IN(15,20) ",
                 "AND    c.operacion MATCHES 'CONFIRMA*' ",
                 "AND  ",cla_where CLIPPED," ",
                 "GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20 ",
                 "ORDER  BY c.usuario, s.n_seguro"

   START REPORT rep_solicitud TO salida

      PREPARE claexe FROM cla_sel
      DECLARE cur_solicitud CURSOR FOR claexe

      FOREACH cur_solicitud INTO g_reg.*, max_hora
         OUTPUT TO REPORT rep_solicitud(g_reg.*)
      END FOREACH

   FINISH REPORT rep_solicitud

   LET g_eco = "echo ", nom_arch CLIPPED, " > ",
               g_param.ruta_listados CLIPPED,"/rescate.CONF.",
               hoy USING "DDMMYY","_",hora1

   RUN g_eco

   ERROR ""

   DISPLAY salida At 18,1
   PROMPT "REPORTE FINALIZADO OPRIMA [ENTER] P/SALIR..." for opc

END FUNCTION

REPORT rep_solicitud(g_reg)

   DEFINE g_reg RECORD
      usuario          CHAR(08),
      spaterno         CHAR(40),
      smaterno         CHAR(40),
      snombres         CHAR(40),
      frecafor         DATE,
      tipo_solicitud   SMALLINT,
      n_folio          DECIMAL(10,0),
      n_seguro         CHAR(11),
      n_rfc            CHAR(13),
      estadon          SMALLINT,
      tip_prob         CHAR(01),
      fol_prob         CHAR(10),
      doc_prob         CHAR(16),
      status_interno   SMALLINT,
      cod_afore_ced    SMALLINT,
      folio_edo_cta    CHAR(08),
      cod_promotor     CHAR(10),
      ppaterno         CHAR(40),
      pmaterno         CHAR(40),
      pnombres         CHAR(40)
   END RECORD                  
   
   DEFINE g_afo RECORD
      afore_desc       CHAR(30)
   END RECORD

   DEFINE g_dom RECORD
      calle        CHAR(40),
      numero       CHAR(10),
      depto        CHAR(10),
      colonia      CHAR(40),
      delega       CHAR(40),
      cuidad       CHAR(40),
      estado       CHAR(40),
      codpos       CHAR(05)
   END RECORD

   DEFINE vmarca_envio CHAR(01)

   DEFINE g_tel RECORD
      cve_lada  CHAR(03),
      telefono  CHAR(40)
   END RECORD

   DEFINE g_rec RECORD
      estado_sol   SMALLINT,
      estado_exp   SMALLINT      
   END RECORD

   DEFINE vhora_confirmacion CHAR(08)

   OUTPUT
      PAGE   LENGTH 1
      TOP    MARGIN 0
      BOTTOM MARGIN 0
      RIGHT  MARGIN 0
      LEFT   MARGIN 0

   FORMAT
      ON EVERY ROW

         ---- TRAE AFORE ----
         SELECT  afore_desc
         INTO    g_afo.afore_desc
         FROM    tab_afore
         WHERE   afore_cod = g_reg.cod_afore_ced

         ----  TRAE DOMICILIO  ----
         OPEN cur_dom USING g_reg.n_seguro,g_reg.n_folio,g_reg.tipo_solicitud
         WHILE TRUE
            FETCH cur_dom INTO g_dom.*,vmarca_envio
            IF STATUS = NOTFOUND THEN
               CLOSE cur_dom
               EXIT WHILE
            END IF
            
            IF vmarca_envio = "X" THEN
               CLOSE cur_dom
               EXIT WHILE
            ELSE
               CLOSE cur_dom
               EXIT WHILE
            END IF

         END WHILE

         ---- TRAE TELEFONO ----
         OPEN cur_tel USING g_reg.n_seguro,g_reg.n_folio,g_reg.tipo_solicitud
         FETCH cur_tel INTO g_tel.*
         CLOSE cur_tel

         ---- TRAE RECEPCION ----
         OPEN cur_rec USING g_reg.n_seguro,g_reg.n_folio,g_reg.tipo_solicitud
         FETCH cur_rec INTO g_rec.*
         CLOSE cur_rec

         ---- TRAE HORA CONFIRMACION ----
         OPEN cur_hor USING g_reg.n_seguro,g_reg.n_folio,g_reg.tipo_solicitud
         FETCH cur_hor INTO vhora_confirmacion
         CLOSE cur_hor

         PRINT COLUMN 001, g_reg.*,
                           g_afo.*,
                           g_dom.*,
                           g_tel.*,
                           g_rec.*,
                           vhora_confirmacion

         INITIALIZE  g_afo.*, g_dom.*, g_tel.*, g_rec.*,
                           vhora_confirmacion TO NULL
END REPORT

