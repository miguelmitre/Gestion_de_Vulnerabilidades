###############################################################################
#Proyecto          => SAFRE                                                   #
#Propietario       => E.F.P.                                                  #
#Programa  CTAB011 => Formato #2 del Estado de Cuenta                         #
#Autor             => Omar Sandoval Badillo                                   #
#Fecha             => 29 de Agosto de 2005.                                   #
###############################################################################
DATABASE safre_af
GLOBALS
   DEFINE g_param    RECORD LIKE seg_modulo.*

   DEFINE aux_pausa       CHAR(1),
          pos             SMALLINT,
          pos1            SMALLINT,
          sw_1            SMALLINT,
          hora            CHAR(8),
          hoy             DATE,
          usuario         CHAR(8),
          opc             CHAR(1),
          g_impre         CHAR(300),
          g_lista         CHAR(300)

END GLOBALS
#########################################################################
MAIN
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I

   DEFER INTERRUPT

   LET hoy = TODAY
   LET pos = 1

   CALL inicio()

   PROMPT "DESEA GENERAR REPORTE [S/N]" FOR opc

   IF opc = "S" OR opc = "s" THEN
      CALL impresion()
      ERROR " LISTADO GENERADO "
   ELSE
      ERROR "REPORTE CANCELADO "
   END IF

END MAIN
#########################################################################
FUNCTION inicio()
   SELECT USER,
          *
   INTO   usuario,
          g_param.*
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"
END FUNCTION
#########################################################################
FUNCTION impresion()
   DEFINE i      ,
          pos    INTEGER

   LET hora = TIME

   LET g_impre = g_param.ruta_listados CLIPPED,"/",usuario CLIPPED,
                 hoy USING "DD-MM-YYYY","_",hora CLIPPED

   START REPORT rpt_cifras TO g_impre
      ERROR "PROCESANDO INFORMACION"
      OUTPUT TO REPORT rpt_cifras()
   FINISH REPORT rpt_cifras

   ERROR "LISTADO GENERADO..."

   LET g_lista = "lp ",g_impre
   RUN g_lista

END FUNCTION
#########################################################################
REPORT rpt_cifras()

   DEFINE tot_in_ac,
          tot_ac,
          tot_cero INTEGER

   DEFINE t_afi_in_ac,
          t_afi_ac,
          t_afi_cero,
          t_asi_in_ac,
          t_asi_ac,
          t_asi_cero     INTEGER

   DEFINE x_mes         SMALLINT,
          x_tot,
          x_tot1,
          x_tot2,
          x_tot3,
          x_tot4,
          x_tot5,
          x_tot6        INTEGER

   OUTPUT
      TOP MARGIN     1
      BOTTOM MARGIN  0
      LEFT MARGIN    0
      RIGHT MARGIN   0
      PAGE LENGTH    80
   FORMAT
      PAGE HEADER

          LET tot_in_ac   = 0
          LET tot_ac   = 0
          LET tot_cero    = 0

          LET t_afi_in_ac   = 0
          LET t_afi_ac   = 0
          LET t_afi_cero   = 0
          LET t_asi_in_ac   = 0
          LET t_asi_ac   = 0
          LET t_asi_cero     = 0

          LET x_mes         = 0
          LET x_tot   = 0
          LET x_tot1   = 0
          LET x_tot2   = 0
          LET x_tot3   = 0
          LET x_tot4   = 0
          LET x_tot5   = 0
          LET x_tot6    = 0

        select count(*)
        INTO   t_afi_ac
        from safre_tmp:tmp_ind_activo_ac a,safre_tmp:cuota b
        where a.nss = b.n_seguro
        and b.tipo_solicitud <> 5

        select count(*)
        INTO   t_afi_in_ac
        from safre_tmp:tmp_ind_activo_in a, safre_tmp:cuota b
        where a.nss = b.n_seguro
        and b.tipo_solicitud <> 5

        SELECT COUNT(*) 
        INTO   t_afi_cero
        FROM   cta_nss_sdo_cero a, cta_nss_edo_cta b
        WHERE  a.nss = b.nss
        AND    b.estado <> 70
        AND    a.nss not in (select c.nss
                             from safre_tmp:tmp_ind_activo_ac c)

        select count(*)
        INTO   t_asi_ac
        from safre_tmp:tmp_ind_activo_ac a,safre_tmp:cuota b
        where a.nss = b.n_seguro
        and b.tipo_solicitud = 5

        select count(*)
        INTO   t_asi_in_ac
        from safre_tmp:tmp_ind_activo_in a, safre_tmp:cuota b
        where a.nss = b.n_seguro
        and b.tipo_solicitud = 5

        SELECT COUNT(*)
        INTO   t_asi_cero
        FROM   cta_nss_sdo_cero a, cta_nss_edo_cta b
        WHERE  a.nss = b.nss
        AND    b.estado = 70
        AND    a.nss not in (select c.nss
                             from safre_tmp:tmp_ind_activo_ac c)

        LET tot_in_ac = t_afi_in_ac + t_asi_in_ac

        LET tot_ac    = t_afi_ac + t_asi_ac

        LET tot_cero  = t_afi_cero + t_asi_cero

         PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d' 
         PRINT COLUMN 01,'\033e\033(s218T\033(s12H\033(s7B'
         PRINT COLUMN 08,"CTAB011                                            ",
                         "                                                   ",
                         hoy USING "DD/MM/YYYY"
         SKIP 2 LINE
###primer bloque
         PRINT COLUMN 08,"PROCESO DE EMISION, ENVIO Y DEVOLUCION DE ESTADOS ",
                         "DE CUENTA PARA EL PERIODO DEL 1 DE ENERO AL 30 DE ",
                         "JUNIO DE 2005"
         SKIP 2 LINE
         PRINT '\033e\033(s218T\033(s16H\033(s0B'
         PRINT COLUMN 11,"Numero de cuentas administradas al                ",                           "    Estados de cuenta emitidos                    ",                           "            Estados de cuenta enviados"
         SKIP 1 LINE
         PRINT COLUMN 18,"30 de Junio de 2005"       
         SKIP 1 LINE
         PRINT COLUMN 11,"Activas  Inactivas  Con saldo cero            ",
                         "Activas          Inactivas      Con saldo cero",
                         "           Activas         Inactivas      Con ",
                         "saldo cero"
         SKIP 1 LINE
         PRINT COLUMN 54,"T. Reg. T. Asig  T. Reg. T. Asig  ",
                         "T. Reg. T. Asig       T. Reg. T. Asig  ",
                         "T. Reg. T. Asig  T. Reg. T. Asig"

         SKIP 1 LINE
         PRINT COLUMN 01,'\033e\033(s218T\033(s16H\033(s7B'
         PRINT COLUMN 11,tot_ac USING "<<<<<<<",
               COLUMN 21,tot_in_ac USING "<<<<<<<",
               COLUMN 34,tot_cero USING "<<<<<<<",
               COLUMN 54,t_afi_ac USING "<<<<<<<",
               COLUMN 62,t_asi_ac USING "<<<<<<<",
               COLUMN 71,t_afi_in_ac USING "<<<<<<<",
               COLUMN 79,t_asi_in_ac USING "<<<<<<<",
               COLUMN 88,t_afi_cero USING "<<<<<<<",
               COLUMN 96,t_asi_cero USING "<<<<<<<",
               COLUMN 110,0 USING "<<<<<<<",
               COLUMN 118,0 USING "<<<<<<<",
               COLUMN 127,0 USING "<<<<<<<",
               COLUMN 135,0 USING "<<<<<<<",
               COLUMN 144,0 USING "<<<<<<<",
               COLUMN 152,0 USING "<<<<<<<"
         SKIP 2 LINE
         PRINT COLUMN 11,"--------------------------------------------",
                         "--------------------------------------------",
                         "--------------------------------------------",
                         "----------------"
         SKIP 3 LINE

###segundo bloque

         DECLARE cur_1 CURSOR FOR
         SELECT MONTH(fecha_fin) mes,
                COUNT(UNIQUE nss) tot
         FROM   cta_ctr_proceso
         WHERE  tipo_informe = 3
         AND    estado = 3
         AND    fecha_fin between "01/01/2005" AND "06/30/2005"
         GROUP BY 1
         ORDER BY 1

         FOREACH cur_1 INTO x_mes,x_tot
            CASE x_mes
               WHEN 1 LET x_tot1 = x_tot
               WHEN 2 LET x_tot2 = x_tot
               WHEN 3 LET x_tot3 = x_tot
               WHEN 4 LET x_tot4 = x_tot
               WHEN 5 LET x_tot5 = x_tot
               WHEN 6 LET x_tot6 = x_tot
            END CASE
         END FOREACH

         PRINT COLUMN 01,'\033e\033(s218T\033(s12H\033(s7B'
         PRINT COLUMN 28,"ESTADO DE CUENTA EMITIDOS Y ENVIADOS ",
                         "POR MOTIVO DE UN TRASPASO DE CUENTA"
         SKIP 2 LINE

         PRINT '\033e\033(s218T\033(s16H\033(s0B'
         PRINT COLUMN 20,"Enero                 Febrero                ",
                         "Marzo                  Abril                   ",
                         "Mayo                   Junio"         
         SKIP 1 LINE
         PRINT COLUMN 13,"Emitidos   Enviados    Emitidos   Enviados    ",
                         "Emitidos   Enviados    Emitidos   Enviados    ",
                         "Emitidos   Enviados    Emitidos   Enviados"
         SKIP 2 LINE
         PRINT COLUMN 01,'\033e\033(s218T\033(s16H\033(s7B'
         PRINT COLUMN 13,x_tot1 USING "<<<<",
               COLUMN 24,x_tot1 USING "<<<<",
               COLUMN 36,x_tot2 USING "<<<<",
               COLUMN 47,x_tot2 USING "<<<<",
               COLUMN 59,x_tot3 USING "<<<<",
               COLUMN 70,x_tot3 USING "<<<<",
               COLUMN 82,x_tot4 USING "<<<<",
               COLUMN 93,x_tot4 USING "<<<<",
               COLUMN 105,x_tot5 USING "<<<<",
               COLUMN 116,x_tot5 USING "<<<<",
               COLUMN 128,x_tot6 USING "<<<<",
               COLUMN 139,x_tot6 USING "<<<<"
         SKIP 2 LINE
         PRINT COLUMN 11,"--------------------------------------------",
                         "--------------------------------------------",
                         "--------------------------------------------",
                         "----------------"
         SKIP 3 LINE

###tercer bloque
         DECLARE cur_2 CURSOR FOR
         SELECT MONTH(fecha_fin) mes,
                COUNT(UNIQUE nss) tot
         FROM   cta_ctr_proceso
         WHERE  tipo_informe = 5
         AND    estado = 3
         AND    fecha_fin between "01/01/2005" AND "06/30/2005"
         GROUP BY 1
         ORDER BY 1

         FOREACH cur_2 INTO x_mes,x_tot
            CASE x_mes
               WHEN 1 LET x_tot1 = x_tot
               WHEN 2 LET x_tot2 = x_tot
               WHEN 3 LET x_tot3 = x_tot
               WHEN 4 LET x_tot4 = x_tot
               WHEN 5 LET x_tot5 = x_tot
               WHEN 6 LET x_tot6 = x_tot
            END CASE
         END FOREACH

         PRINT COLUMN 01,'\033e\033(s218T\033(s12H\033(s7B'
         PRINT COLUMN 22,"ESTADO DE CUENTA EMITIDOS Y ENVIADOS POR ",
                         "MOTIVO DE UNA DISPOSICION TOTAL DE RECURSOS"
         SKIP 2 LINE
         PRINT '\033e\033(s218T\033(s16H\033(s0B'
         PRINT COLUMN 20,"Enero                 Febrero                ",
                         "Marzo                  Abril                   ",
                         "Mayo                   Junio"         
         SKIP 1 LINE
         PRINT COLUMN 13,"Emitidos   Enviados    Emitidos   Enviados    ",
                         "Emitidos   Enviados    Emitidos   Enviados    ",
                         "Emitidos   Enviados    Emitidos   Enviados"
         SKIP 2 LINE
         PRINT COLUMN 01,'\033e\033(s218T\033(s16H\033(s7B'
         PRINT COLUMN 13,x_tot1 USING "<<<<",
               COLUMN 24,x_tot1 USING "<<<<",
               COLUMN 36,x_tot2 USING "<<<<",
               COLUMN 47,x_tot2 USING "<<<<",
               COLUMN 59,x_tot3 USING "<<<<",
               COLUMN 70,x_tot3 USING "<<<<",
               COLUMN 82,x_tot4 USING "<<<<",
               COLUMN 93,x_tot4 USING "<<<<",
               COLUMN 105,x_tot5 USING "<<<<",
               COLUMN 116,x_tot5 USING "<<<<",
               COLUMN 128,x_tot6 USING "<<<<",
               COLUMN 139,x_tot6 USING "<<<<"

         SKIP 2 LINE
         PRINT COLUMN 11,"--------------------------------------------",
                         "--------------------------------------------",
                         "--------------------------------------------",
                         "----------------"
         SKIP 3 LINE

###cuarto bloque
         DECLARE cur_3 CURSOR FOR
         SELECT MONTH(fecha_fin) mes,
                COUNT(UNIQUE nss) tot
         FROM   cta_ctr_proceso
         WHERE  tipo_informe = 1
         AND    estado = 3
         AND    fecha_fin between "01/01/2005" AND "06/30/2005"
         AND    nss IN (SELECT n_seguro
                        FROM   afi_mae_afiliado
                        WHERE  tipo_solicitud = 5)
         GROUP BY 1
         ORDER BY 1

         FOREACH cur_3 INTO x_mes,x_tot
            CASE x_mes
               WHEN 1 LET x_tot1 = x_tot
               WHEN 2 LET x_tot2 = x_tot
               WHEN 3 LET x_tot3 = x_tot
               WHEN 4 LET x_tot4 = x_tot
               WHEN 5 LET x_tot5 = x_tot
               WHEN 6 LET x_tot6 = x_tot
            END CASE
         END FOREACH

         PRINT COLUMN 01,'\033e\033(s218T\033(s12H\033(s7B'
         PRINT COLUMN 33,"ESTADOS DE CUENTA EMITIDOS A SOLICITUD ",
                         "DEL TRABAJADOR ASIGNADO"
         SKIP 2 LINE
         PRINT '\033e\033(s218T\033(s16H\033(s0B'
         PRINT COLUMN 20,"Enero                 Febrero                ",
                         "Marzo                  Abril                   ",
                         "Mayo                   Junio"         
         SKIP 2 LINE
         PRINT COLUMN 01,'\033e\033(s218T\033(s16H\033(s7B'
         PRINT COLUMN 19,x_tot1,
               COLUMN 42,x_tot2,
               COLUMN 64,x_tot3,
               COLUMN 87,x_tot4,
               COLUMN 111,x_tot5,
               COLUMN 134,x_tot6
         SKIP 2 LINE
         PRINT COLUMN 11,"--------------------------------------------",
                         "--------------------------------------------",
                         "--------------------------------------------",
                         "----------------"
         SKIP 1 LINE

      PAGE TRAILER
         PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'
         PRINT COLUMN 128," Pagina : ",PAGENO USING"<<<<<"

END REPORT
#########################################################################

