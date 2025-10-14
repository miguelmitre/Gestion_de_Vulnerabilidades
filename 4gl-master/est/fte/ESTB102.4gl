################################################################################
#Proyecto          => SISTEMA DE AFORE( SAFRE )                                #
#PROPIETARIO       => EFP                                                      #
#Programa ESTB102  => Generacion de Saldos de Ahorro Voluntario por Perfil de  #
#                     los Trabajadores (Anexo C).                              #
#Fecha creacion    => 22 DE MAYO 2008                                          #
#Por               => JRC                                                      #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE
      fecproc     ,
      fecini      ,
      fecorte     DATE,
      nomarch     CHAR(120),
      ar_sdo      ARRAY[2,12] OF RECORD
      n_imss      ,
      n_issst     ,
      n_asig      ,
      n_indep     INTEGER,
      t_volun     ,
      t_cpers     ,
      t_ahorr     ,
      t_compl     ,
      t_siefo     DECIMAL(16, 6)
      END RECORD  ,
      t_imss      ,
      t_issst     ,
      t_asig      ,
      t_indep     ARRAY[2] OF INTEGER,
      t_volun     ,
      t_persp     ,
      t_ahorr     ,
      t_compl     ,
      t_total     ARRAY[2] OF DECIMAL(16, 6),
      ar_ccon     ARRAY[10] OF INTEGER,
      ar_prec     ARRAY[6] OF DECIMAL(16, 6),
      cve_afo     SMALLINT,
      raz_soc     CHAR(50)

END GLOBALS


MAIN

   DEFER INTERRUPT
      OPTIONS INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I

   CALL STARTLOG("ESTB102.log")

   LET fecproc = TODAY

   INITIALIZE fecorte TO NULL

   OPEN WINDOW estb1021 AT 4, 4 WITH FORM "ESTB1021" ATTRIBUTE( BORDER )
   DISPLAY "<ESC> : Genera" AT 1, 2
   DISPLAY "<CTRL-C> : Salir" AT 1, 58
   DISPLAY " ESTB102              GENERACION DE SALDOS DE AHORRO             ",
   fecproc USING "DD-MM-YYYY " AT 3, 1 ATTRIBUTE( REVERSE )

   CALL ExtraePeriodo()
   CLOSE WINDOW estb1021

END MAIN


FUNCTION ExtraePeriodo()
#-----------------------

   DEFINE
      resp    ,
      enter   CHAR( 1 )

   INPUT BY NAME fecorte WITHOUT DEFAULTS
      AFTER FIELD fecorte
         IF( fecorte IS NULL ) THEN
            ERROR " LA FECHA DE CORTE NO DEBE SER NULA"
            NEXT FIELD fecorte
         END IF

         IF( NOT FechaCorteOK() ) THEN
            ERROR " LA FECHA DE CORTE ES INVALIDA"
            NEXT FIELD fecorte
         END IF

      ON KEY ( ESC )

         IF( fecorte IS NULL ) THEN
            ERROR " LA FECHA DE CORTE NO DEBE SER NULA"
            NEXT FIELD feccte
         END IF

         IF( NOT FechaCorteOK() ) THEN
            ERROR " LA FECHA DE CORTE ES INVALIDA"
            NEXT FIELD fecorte
         END IF

         WHILE TRUE
            PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
               IF enter MATCHES "[sSnN]" THEN
                  IF enter MATCHES "[sS]" THEN
                     EXIT WHILE
                  ELSE
                     PROMPT" PROCESO CANCELADO...<ENTER> PARA CONTINUAR "
                     FOR CHAR enter
                     EXIT PROGRAM
                  END IF
               END IF
         END WHILE

         IF( NOT Anexo_A_OK() ) THEN
            PROMPT "ANEXO A NO EJECUTADO, PROCESO CANCELADO...<ENTER> PARA CONTINUAR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF

         DISPLAY " PROCESANDO INFORMACION..." AT 19, 1 ATTRIBUTE( REVERSE )

         CALL GenReporteSaldos() 

         DISPLAY "ARCHIVO GENERADO : ", nomarch AT 13, 5 ATTRIBUTE( BOLD )

         PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter

         EXIT INPUT

      ON KEY ( INTERRUPT, CONTROL-C )
         CLEAR FORM
         ERROR " PROCESO CANCELADO..."
         SLEEP 3
         EXIT INPUT
   END INPUT

END FUNCTION


FUNCTION Anexo_A_OK()
#---------------------

   SELECT 1
   FROM   safre_tmp:cta_ctr_fmto_a
   WHERE  fecha_corte = fecorte
   AND paso = 2
   AND fecha_fin IS NOT NULL

   IF ( SQLCA.SQLCODE <> NOTFOUND ) THEN
      RETURN TRUE
   END IF

   RETURN FALSE

END FUNCTION


FUNCTION FechaCorteOK()
#----------------------

   DEFINE
      ndias          ARRAY[12] OF SMALLINT,
      dia, mes, anio SMALLINT,
      feccad         CHAR(10)

   LET ndias[01] = 31      #* Enero *
   LET ndias[02] = 28      #* Febrero no Bisiesto
   LET ndias[03] = 31      #* Marzo *
   LET ndias[04] = 30      #* Abril *
   LET ndias[05] = 31      #* Mayo  *
   LET ndias[06] = 30      #* Junio *
   LET ndias[07] = 31      #* Julio *
   LET ndias[08] = 31      #* Agosto *
   LET ndias[09] = 30      #* Septiembre *
   LET ndias[10] = 31      #* Octubre *
   LET ndias[11] = 30      #* Noviembre *
   LET ndias[12] = 31      #* Diciembre *

   LET dia = DAY( fecorte )
   LET mes = MONTH( fecorte )
   LET anio = YEAR( fecorte )

   IF( mes = 2 ) THEN   #* Es Febrero *
      IF( ( anio MOD 4 = 0 AND anio MOD 100 <> 0 )
         OR anio MOD 400  = 0 ) THEN    #* Es BISIESTO *
         LET ndias[02] = 29
      END IF
   END IF

   IF( dia <> ndias[ mes ] ) THEN
      RETURN FALSE
   END IF

   LET feccad = mes USING "&&", "/01/", anio USING "&&&&"

   LET fecini = feccad

   RETURN TRUE

END FUNCTION


FUNCTION UltDiaHabilDelMes( Fecha )
#----------------------------------

   DEFINE
      Fecha, sig_fecha        DATE,
      dia_semana, ant_habil   SMALLINT

   LET sig_fecha  = Fecha

   WHILE TRUE
      LET dia_semana = WEEKDAY(sig_fecha)

      IF dia_semana = 0 OR dia_semana = 6 THEN
         LET sig_fecha = sig_fecha - 1
         CONTINUE WHILE
      END IF

      SELECT 1
      FROM tab_feriado
      WHERE feria_fecha = sig_fecha

      IF STATUS <> NOTFOUND THEN
         LET sig_fecha  = sig_fecha - 1
         CONTINUE WHILE
      ELSE
         EXIT WHILE
      END IF

   END WHILE

   RETURN sig_fecha

END FUNCTION


FUNCTION GenReporteSaldos()
#--------------------------

   DEFINE RutaArch LIKE seg_modulo.ruta_envio

   SELECT ruta_envio
   INTO RutaArch 
   FROM seg_modulo 
   WHERE modulo_cod = 'est'

   LET nomarch = RutaArch CLIPPED, "/FORMATO_C_", TODAY  USING "DDMMYY"

   START REPORT rep_saldos TO nomarch

   SELECT codigo_afore,
          razon_social
   INTO   cve_afo,
          raz_soc 
   FROM tab_afore_local	

   CALL IniciaArr()

   CALL CreaTabTemp()

   IF cve_afo = 574 THEN
      CALL LlenaTabTempSCT()     #* Llena Tabla Temp solo p/Scotia *
   ELSE
      CALL LlenaTabTemp()        #* Llena Tabla Temp p/las demas Afores *
   END IF

   #* IF p/Ident. AFORE p/Detalle 1 (Trab. ke SI Manifestaron) *
   IF cve_afo = 562 OR cve_afo = 532 OR cve_afo = 574 THEN   #* Inv, HSBC, Sct *
      CALL GeneraDetalle( 1 )
   END IF

   #* IF p/Ident. AFORE p/Detalle 2 (Trab. ke NO Manifestaron) *
   #* Xxi, Inv, Mlm, Cop, Sct *
   IF cve_afo = 516 OR cve_afo = 562 OR cve_afo = 564 OR cve_afo = 568 
   OR cve_afo = 574 THEN

      CALL GeneraDetalle( 2 )
   END IF

   CALL GeneraCifControl()

   OUTPUT TO REPORT rep_saldos()    #* Imprime Reporte *
   FINISH REPORT rep_saldos         #* Cierra Reporte  *
   DROP TABLE t_anexoc


END FUNCTION


FUNCTION IniciaArr()
#-------------------

   DEFINE i, j   SMALLINT

   FOR i = 1 TO 2
      FOR j = 1 TO 12
         LET ar_sdo[i,j].n_imss  = 0
         LET ar_sdo[i,j].n_issst = 0
         LET ar_sdo[i,j].n_asig  = 0
         LET ar_sdo[i,j].n_indep = 0
         LET ar_sdo[i,j].t_volun = 0
         LET ar_sdo[i,j].t_cpers = 0
         LET ar_sdo[i,j].t_ahorr = 0
         LET ar_sdo[i,j].t_compl = 0
         LET ar_sdo[i,j].t_siefo = 0
      END FOR
   END FOR

   FOR i = 1 TO 10
      LET ar_ccon[i] = 0
   END FOR

   FOR i = 1 TO 6
      LET ar_prec[i] = 0
   END FOR

END FUNCTION


FUNCTION CreaTabTemp()
#---------------------

   CREATE TEMP TABLE t_anexoc
   (nss      CHAR(11),
    sief     SMALLINT,
    subcta   SMALLINT,
    t_afil   SMALLINT,
    edad     SMALLINT,
    monto    DECIMAL(16, 6)
    ) WITH NO LOG

END FUNCTION


FUNCTION LlenaTabTemp()
#-----------------------

   DEFINE
      cadsql     CHAR(500),
      c_consub   CHAR(60)

   CASE
      WHEN cve_afo = 562    #* Invercap *
         LET c_consub = " AND subcuenta IN ( 3, 10, 11, 12, 15, 16, 20, 21, 23 ) "

      OTHERWISE    #* Aplica p/Coppel, XXI, Invercap, MetLife, HSBC *
         LET c_consub = "AND subcuenta IN ( 3, 10, 11, 12, 15, 16, 20, 21 ) "
   END CASE

   LET cadsql = "INSERT INTO t_anexoc ",  #-- AFILIADOS IMSS Y ASIGNADOS --#
                "SELECT a.nss, siefore, subcuenta, ",
                "CASE ",
                "WHEN b.tipo_solicitud = 5 THEN 3 ",
                "ELSE 1 ",
                "END, ",
                "YEAR(TODAY) - YEAR(fena), SUM(monto_en_acciones) ",
                "FROM dis_cuenta a, afi_mae_afiliado b ",
                "WHERE fecha_conversion <= '", fecorte,  "' ", c_consub CLIPPED, " ",
                "AND siefore IN (1, 2, 3, 4, 5, 6 ) ",
                "AND ( ( a.nss = b.n_seguro AND b.tipo_solicitud NOT IN ( 8, 12, 5 ) ) ",
                "OR ( a.nss = b.n_seguro AND b.tipo_solicitud = 5 ) ) ",
                "GROUP BY 1, 2, 3, 4, 5 ",
                "HAVING SUM(monto_en_acciones) > 0; "

   PREPARE idsql10 FROM cadsql
   EXECUTE idsql10

   LET cadsql = "INSERT INTO t_anexoc ", #-- AFILIADOS ISSSTE E INDEPENDIENTES --#
                "SELECT a.nss, siefore, subcuenta, ",
                "CASE ",
                "WHEN c.tipo_trab_ind = 1 THEN 4 ",
                "ELSE 2 ",
                "END, ",
                "YEAR(TODAY) - YEAR(fena), SUM(monto_en_acciones) ",
                "FROM dis_cuenta a, afi_mae_afiliado b, cta_ctr_reg_ind c ",
                "WHERE fecha_conversion <= '", fecorte,  "' ", c_consub CLIPPED, " ",
                "AND siefore IN (1, 2, 3, 4, 5, 6 ) ",
                "AND a.nss = c.nti AND tipo_trab_ind IN ( '1', '2' ) ",
                "AND a.nss = b.n_seguro AND b.tipo_solicitud IN ( 8, 12 ) ",
                "GROUP BY 1, 2, 3, 4, 5 ",
                "HAVING SUM(monto_en_acciones) > 0 "

   PREPARE idsql11 FROM cadsql
   EXECUTE idsql11

END FUNCTION


FUNCTION LlenaTabTempSCT()
#-------------------------

   DEFINE
      cadsql     CHAR(700),
      c_consub   CHAR(60)

   #* 1er Recuadro: IMSS y Asignados *

   LET cadsql = "INSERT INTO t_anexoc ",
                "SELECT a.nss, siefore, subcuenta, ",
                "CASE ",
                "WHEN c.tipo_solicitud = 5 THEN 3 ",
                "ELSE 1 ",
                "END, YEAR(TODAY) - YEAR(fena), SUM(monto_en_acciones) ",
                "FROM int_det_voluntaria a, dis_cuenta b, afi_mae_afiliado c ",
                "WHERE fecha_conversion <= '", fecorte, "' ",
                "AND a.nss = b.nss AND a.folio = b.folio ",
                "AND subcuenta IN ( 10, 16 ) AND siefore IN (1, 2, 3, 4, 5, 6 ) ",
                "AND tipo_vol in ( '01', '04' ) AND resul_operacion = '01' ",
                "AND ( ( a.nss = c.n_seguro AND c.tipo_solicitud NOT IN ( 8, 12, 5 ) ) ",
                "OR ( a.nss = c.n_seguro AND c.tipo_solicitud = 5 ) ) ",
                "GROUP BY 1, 2, 3, 4, 5 ",
                "HAVING SUM(monto_en_acciones) > 0 "

   PREPARE idsql21 FROM cadsql
   EXECUTE idsql21
   FREE idsql21

   #* 1er Recuadro: ISSSTE e Independientes *

   LET cadsql = "INSERT INTO t_anexoc ",
                "SELECT a.nss, siefore, subcuenta, ",
                "CASE ",
                "WHEN d.tipo_trab_ind = 1 THEN 4 ",
                "ELSE 2 ",
                "END, YEAR(TODAY) - YEAR(fena), SUM(monto_en_acciones) ",
                "FROM int_det_voluntaria a, dis_cuenta b, afi_mae_afiliado c,  ",
                "cta_ctr_reg_ind d ",
                "WHERE fecha_conversion <= '", fecorte, "' ",
                "AND a.nss = b.nss AND a.folio = b.folio ",
                "AND subcuenta IN ( 10, 16 ) AND siefore IN (1, 2, 3, 4, 5, 6 ) ",
                "AND tipo_vol in ( '01', '04' ) AND resul_operacion = '01' ",
                "AND a.nss = d.nti AND tipo_trab_ind IN ( '1', '2' ) ",
                "AND a.nss = c.n_seguro AND c.tipo_solicitud IN ( 8, 12 ) ",
                "GROUP BY 1, 2, 3, 4, 5 ",
                "HAVING SUM(monto_en_acciones) > 0 "

   PREPARE idsql22 FROM cadsql
   EXECUTE idsql22
   FREE idsql22

   #* 2o Recuadro: IMSS y Asignados *

   LET cadsql = "INSERT INTO t_anexoc ",
                "SELECT a.nss, siefore, subcuenta, ",
                "CASE ",
                "WHEN c.tipo_solicitud = 5 THEN 3 ",
                "ELSE 1 ",
                "END, YEAR(TODAY) - YEAR(fena), SUM(monto_en_acciones) ",
                "FROM int_det_voluntaria a, dis_cuenta b, afi_mae_afiliado c ",
                "WHERE fecha_conversion <= '", fecorte, "' ",
                "AND a.nss = b.nss AND a.folio = b.folio ",
                "AND subcuenta IN ( 10, 12, 16 ) AND siefore IN ( 1, 2, 3, 4, 5, 6 ) ",
                "AND tipo_vol IN ( '02', '03', '05' ) AND resul_operacion = '01' ",
                "AND ( ( a.nss = c.n_seguro AND c.tipo_solicitud NOT IN ( 8, 12, 5 ) ) ",
                "OR ( a.nss = c.n_seguro AND c.tipo_solicitud = 5 ) ) ",
                "GROUP BY 1, 2, 3, 4, 5 ",
                "HAVING SUM(monto_en_acciones) > 0 "

   PREPARE idsql23 FROM cadsql
   EXECUTE idsql23
   FREE idsql23

   LET cadsql = "INSERT INTO t_anexoc ",
                "SELECT a.nss, siefore, subcuenta, ",
                "CASE ",
                "WHEN b.tipo_solicitud = 5 THEN 3 ",
                "ELSE 1 ",
                "END, YEAR(TODAY) - YEAR(fena), SUM(monto_en_acciones) ",
                "FROM dis_cuenta a, afi_mae_afiliado b ",
                "WHERE fecha_conversion <= '", fecorte, "' ",
                "AND subcuenta IN ( 3, 11, 15 ) AND siefore IN ( 1, 2, 3, 4, 5, 6 ) ",
                "AND ( ( a.nss = b.n_seguro AND b.tipo_solicitud NOT IN ( 8, 12, 5 ) ) ",
                "OR ( a.nss = b.n_seguro AND b.tipo_solicitud = 5 ) ) ",
                "GROUP BY 1, 2, 3, 4, 5 ",
                "HAVING SUM(monto_en_acciones) > 0 "

   PREPARE idsql24 FROM cadsql
   EXECUTE idsql24
   FREE idsql24

   #* 2o Recuadro: ISSSTE e Independientes *

   LET cadsql = "INSERT INTO t_anexoc ",
                "SELECT a.nss, siefore, subcuenta, ",
                "CASE ",
                "WHEN d.tipo_trab_ind = 1 THEN 4 ",
                "ELSE 2 ",
                "END, YEAR(TODAY) - YEAR(fena), SUM(monto_en_acciones) ",
                "FROM int_det_voluntaria a, dis_cuenta b, afi_mae_afiliado c, ",
                "cta_ctr_reg_ind d ",
                "WHERE fecha_conversion <= '", fecorte, "' ",
                "AND a.nss = b.nss AND a.folio = b.folio ",
                "AND subcuenta IN ( 10, 12, 16 ) AND siefore IN ( 1, 2, 3, 4, 5, 6 ) ",
                "AND tipo_vol IN ( '02', '03', '05' ) AND resul_operacion = '01' ",
                "AND a.nss = d.nti AND tipo_trab_ind IN ( '1', '2' ) ",
                "AND a.nss = c.n_seguro AND c.tipo_solicitud IN ( 8, 12 ) ",
                "GROUP BY 1, 2, 3, 4, 5 ",
               "HAVING SUM(monto_en_acciones) > 0 "

   PREPARE idsql25 FROM cadsql
   EXECUTE idsql25
   FREE idsql25

   LET cadsql = "INSERT INTO t_anexoc ",
                "SELECT a.nss, siefore, subcuenta, ",
                "CASE ",
                "WHEN d.tipo_trab_ind = 1 THEN 4 ",
                "ELSE 2 ",
                "END, YEAR(TODAY) - YEAR(fena), sum(monto_en_acciones) ",
                "FROM dis_cuenta a, afi_mae_afiliado b, cta_ctr_reg_ind d ",
                "WHERE fecha_conversion <= '", fecorte, "' ",
                "AND subcuenta IN ( 3, 11, 15 ) AND siefore IN ( 1, 2, 3, 4, 5, 6 ) ",
                "AND a.nss = d.nti AND tipo_trab_ind IN ( '1', '2' ) ",
                "AND a.nss = b.n_seguro AND b.tipo_solicitud IN ( 8, 12 ) ",
               "GROUP BY 1, 2, 3, 4, 5 ",
               "HAVING SUM(monto_en_acciones) > 0 "

   PREPARE idsql26 FROM cadsql
   EXECUTE idsql26
   FREE idsql26

END FUNCTION


FUNCTION PreparaCuerote( n_det )
#-------------------------------

   DEFINE
      n_det       SMALLINT,
      c_selsub    CHAR(100),
      c_consub    CHAR(60),
      c_conafil   CHAR(150),
      cadsql      CHAR(5000)

   CASE n_det
      WHEN 1    #* Es Detalle 1 *
         CASE cve_afo
            WHEN 562    #* Invercap *
               LET c_selsub = "DECODE( subcta, 23, 5, 5 ), "
               LET c_consub = "subcta = 23 " 

            WHEN 532    #* HSBC     *
               LET c_selsub = "DECODE( subcta, ",
                              "3, 5, 10, 5, 20, 6, 21, 6, 15, 7, 16, 7, 11, 8, 12, 8 ), "

               LET c_consub = "subcta IN ( 3, 10, 11, 12, 15, 16, 20, 21 ) "

            WHEN 574    #* Scotia   *
               LET c_selsub = "DECODE( subcta, 10, 5, 16, 7 ), "
               LET c_consub = "subcta IN ( 10, 16 ) "
         END CASE

      WHEN 2    #* Es Detalle 2 *
                #* Por el momento, aplica = p/Coppel, Invercap y MetLife *
         IF cve_afo = 574 THEN
            LET c_selsub = "DECODE( subcta, ",
                           "3, 5, 10, 5, 11, 8, 12, 8, 15, 7, 16, 7 ), "

            LET c_consub = "subcta IN ( 3, 10, 11, 12, 15, 16 ) " 
         ELSE
            LET c_selsub = "DECODE( ",
                           "subcta, 3, 5, 10, 5, 20, 6, 21, 6, 15, 7, 16, 7, 11, 8, 12, 8 ), "

            LET c_consub = "subcta IN ( 3, 10, 11, 12, 15, 16, 20, 21 ) "
         END IF
   END CASE

   LET cadsql = "SELECT ", c_selsub CLIPPED, " t_afil, ",
                "CASE ",
                "WHEN a.sief = 1 AND a.edad >= 56 THEN 1 ",
                "WHEN a.sief = 1 AND a.edad < 56 THEN 1 ",
                "WHEN a.sief = 1 AND b.ind_edad = 1 THEN 1 ",
                "WHEN a.sief = 1 THEN 13 ",
                "WHEN a.sief = 2 AND a.edad BETWEEN 46 AND 55 THEN 3 ",
                "WHEN a.sief = 2 AND a.edad < 46 THEN 3 ",
                "WHEN a.sief = 2 AND b.ind_edad = 2 THEN 3 ",
                "WHEN a.sief = 2 THEN 14 ",
                "WHEN a.sief = 3 AND a.edad BETWEEN 37 AND 45 THEN 5 ",
                "WHEN a.sief = 3 AND a.edad  < 37 THEN 5 ",
                "WHEN a.sief = 3 AND b.ind_edad = 3 THEN 5 ",
                "WHEN a.sief = 3 THEN 15 ",
                "WHEN a.sief = 4 AND a.edad BETWEEN 27 AND 36 THEN 7 ",
                "WHEN a.sief = 4 AND a.edad  < 27 THEN 7 ",
                "WHEN a.sief = 4 AND b.ind_edad = 4 THEN 7 ",
                "WHEN a.sief = 4 THEN 16 ",
                "WHEN a.sief = 5 AND a.edad  <= 26 THEN 9 ",
                "WHEN a.sief = 5 AND b.ind_edad = 5 THEN 9 ",
                "WHEN a.sief = 5 THEN 17 ",
                "WHEN a.sief = 6 THEN 11 ",
                "END, COUNT(UNIQUE a.nss), SUM(a.monto) FROM t_anexoc a, safre_af:cta_ctr_cuenta b ",
                "WHERE ",  c_consub CLIPPED, " ",
                "AND a.nss = b.nss ",
                "GROUP BY 1, 2, 3 ",
                "ORDER BY 1, 2, 3 "

   RETURN cadsql

END FUNCTION


FUNCTION GeneraDetalle( d )
#--------------------------

   DEFINE
      d              ,
      colmto         ,
      coltipo        ,
      n_sief         ,
      sief           SMALLINT,
      noafil         INTEGER,
      totacc         DECIMAL(16, 6),
      cadsql         CHAR(5000)

   CALL IniciaArr()

   CALL PreparaCuerote( d ) RETURNING cadsql

   PREPARE idsql01 FROM cadsql
   DECLARE cursor02 CURSOR FOR idsql01

   FOREACH cursor02 INTO colmto, coltipo, n_sief, noafil, totacc

      IF colmto = 5 AND n_sief = 13 THEN #-- VOLUNTARIAS --#
         LET coltipo = 3
         LET n_sief = 1
      END IF

      IF colmto = 6 THEN #-- PERSPECTIVA LARGO PLAZO --#
         CASE n_sief
            WHEN 13
               LET coltipo = 3
               LET n_sief = 1
            WHEN 14
               LET coltipo = 3
               LET n_sief = 3
            WHEN 15
               LET coltipo = 3
               LET n_sief = 5
            WHEN 16
               LET coltipo = 3
               LET n_sief = 7
            WHEN 17
               LET coltipo = 3
               LET n_sief = 9
         END CASE
      END IF

      IF colmto = 7 THEN #-- LARGO PLAZO --#
         CASE n_sief
            WHEN 13
               LET coltipo = 3
               LET n_sief = 1
            WHEN 14
               LET coltipo = 3
               LET n_sief = 3
            WHEN 15
               LET coltipo = 3
               LET n_sief = 5
            WHEN 16
               LET coltipo = 3
               LET n_sief = 7
            WHEN 17
               LET coltipo = 3
               LET n_sief = 9
         END CASE
      END IF

      IF colmto = 8 THEN #-- COMPLEMENTARIAS --#
         CASE n_sief
            WHEN 13
               LET coltipo = 3
               LET n_sief = 1
            WHEN 14
               LET coltipo = 3
               LET n_sief = 3
            WHEN 15
               LET coltipo = 3
               LET n_sief = 5
            WHEN 16
               LET coltipo = 3
               LET n_sief = 7
            WHEN 17
               LET coltipo = 3
               LET n_sief = 9
         END CASE
      END IF

      CASE coltipo
         WHEN 1
            LET ar_sdo[d,n_sief].n_imss = ar_sdo[d,n_sief].n_imss + noafil
         WHEN 2
            LET ar_sdo[d,n_sief].n_issst = ar_sdo[d,n_sief].n_issst + noafil
         WHEN 3
            LET ar_sdo[d,n_sief].n_asig = ar_sdo[d,n_sief].n_asig + noafil
         WHEN 4
            LET ar_sdo[d,n_sief].n_indep = ar_sdo[d,n_sief].n_indep + noafil
      END CASE

      CASE colmto
         WHEN 5
            LET ar_sdo[d,n_sief].t_volun = ar_sdo[d,n_sief].t_volun + totacc
         WHEN 6
            LET ar_sdo[d,n_sief].t_cpers = ar_sdo[d,n_sief].t_cpers + totacc
         WHEN 7
            LET ar_sdo[d,n_sief].t_ahorr = ar_sdo[d,n_sief].t_ahorr + totacc
         WHEN 8
            LET ar_sdo[d,n_sief].t_compl = ar_sdo[d,n_sief].t_compl + totacc
      END CASE

      LET ar_sdo[d,n_sief].t_siefo = ar_sdo[d,n_sief].t_siefo + totacc
   END FOREACH

END FUNCTION


FUNCTION GeneraCifControl()
#--------------------------

   DEFINE
      col        ,
      n_sief     SMALLINT,
      n_afil     INTEGER,
      cadsql     CHAR(500),
      fechabil   DATE,
      montoDia   DECIMAL(16, 6),
      c_consub   CHAR(60)

   #* Cuentas c/Saldo Cero (1) *

   SELECT COUNT(*) 
   INTO ar_ccon[1] 
   FROM cta_act_marca
   WHERE marca_cod = 150 
   AND fecha_ini <= fecorte

   #* Cuentas Inhabilitadas por Traspaso (2) *

   SELECT COUNT(*)
   INTO ar_ccon[2] 
   FROM cta_act_marca
   WHERE marca_cod = 120 
   AND fecha_ini <= fecorte

   #* Cuentas Inhabilitadas por UNIFICACION (3) *

   SELECT COUNT(*)
   INTO ar_ccon[3]
   FROM cta_act_marca
   WHERE marca_cod = 130
   AND fecha_ini <= fecorte

   #* Cuentas IMSS (4), ISSSTE (5), Asignadas (6) e Independientes (7)*

   IF cve_afo = 568 THEN    #* Solo p/Coppel

      LET cadsql = "SELECT ",
                   "DECODE( tipo_solicitud, 5, 6, 8, 7, 4 ), COUNT(*) ",
                   "FROM safre_tmp:cuota ",
                   "WHERE tipo_solicitud IN ( 5, 8 ) ",
                   "OR tipo_solicitud NOT IN ( 5, 8, 12 ) ",
                   "GROUP BY 1 ",
                   "UNION ",
                   "SELECT 5, COUNT(UNIQUE a.nss) ",
                   "FROM safre_tmp:cta_formato_nss a, cta_ctr_reg_ind b ",
                   "WHERE a.nss = b.nti AND tipo_trab_ind = '2' ",
                   "GROUP BY 1 "
   ELSE
      LET cadsql = "SELECT ",
                   "CASE ",
                   "WHEN b.tipo_solicitud NOT IN ( 8, 12, 5 ) THEN 4 ",
                   "ELSE 6 ",
                   "END CASE, COUNT(UNIQUE a.nss) ",
                   "FROM safre_tmp:cta_formato_nss a, afi_mae_afiliado b ",
                   "WHERE ( a.nss = b.n_seguro AND b.tipo_solicitud NOT IN ( 8, 12, 5 ) ) ",
                   "OR ( a.nss = b.n_seguro AND b.tipo_solicitud = 5 ) ",
                   "GROUP BY 1 ",
                   "UNION ",
                   "SELECT ",
                   "CASE ",
                   "WHEN b.tipo_trab_ind = 1 THEN 7 ",
                   "ELSE 5 ",
                   "END CASE, COUNT(UNIQUE a.nss) ",
                   "FROM safre_tmp:cta_formato_nss a, cta_ctr_reg_ind b ",
                   "WHERE a.nss = b.nti AND tipo_trab_ind IN ( '1', '2' ) ",
                   "GROUP BY 1 ",
                   "ORDER BY 1 "
   END IF

   PREPARE idsql02 FROM cadsql
   DECLARE cursor03 CURSOR FOR idsql02

   FOREACH cursor03 INTO col, n_afil
      LET ar_ccon[col] = n_afil
   END FOREACH

   #* Acumula p/Total Numero de Ctas. *

   LET ar_ccon[8] = ar_ccon[1] +
                    ar_ccon[2] +
                    ar_ccon[3] +
                    ar_ccon[4] +
                    ar_ccon[5] +
                    ar_ccon[6] +
                    ar_ccon[7]

   CASE
      WHEN cve_afo = 562   #* Invercap *
         LET c_consub = "subcuenta IN ( 3, 10, 11, 12, 15, 16, 20, 21, 23 ) "

      WHEN cve_afo = 574   #* Scotia   *
         LET c_consub = "subcuenta IN ( 3, 10, 11, 12, 15, 16 ) "

      OTHERWISE            #* Aplica p/Coppel, XXI, Invercap, MetLife y HSBC *
         LET c_consub = "subcuenta IN ( 3, 10, 11, 12, 15, 16, 20, 21 ) "
   END CASE

   #* No. de Ctas. c/Aportaciones Voluntarias: Al dia del corte (9) *

   LET cadsql = "SELECT COUNT(UNIQUE nss), SUM(monto_en_acciones) ",
                "FROM dis_cuenta ",
                "WHERE ", c_consub CLIPPED, " ",
                "AND fecha_conversion <= '", fecorte, "' ",
                "HAVING SUM(monto_en_acciones) > 0 "

   PREPARE idsql31 FROM cadsql
   EXECUTE idsql31 INTO ar_ccon[9] 

   #* No. de Ctas. c/Aportaciones Voluntarias: c/Aport. durante el mes (9) *

   LET cadsql = "SELECT COUNT(UNIQUE nss) ",
                "FROM dis_cuenta ", 
                "WHERE ", c_consub CLIPPED, " ",
                "AND fecha_conversion BETWEEN '", fecini, "' AND '", fecorte, "' ",
                "AND tipo_movimiento = 1 "

   PREPARE idsql32 FROM cadsql
   EXECUTE idsql32 INTO ar_ccon[10] 

   CALL UltDiaHabilDelMes( fecorte ) RETURNING fechabil

   DECLARE cursor20 CURSOR FOR
   SELECT codigo_siefore, NVL( precio_del_dia, 0 )
   FROM glo_valor_accion
   WHERE fecha_valuacion = fechabil 
   AND codigo_siefore IN ( 1, 2, 3, 4, 5, 6 )

   FOREACH cursor20 INTO n_sief, montoDia
      LET ar_prec[n_sief] = montoDia
   END FOREACH

END FUNCTION


FUNCTION IniciaTots()
#--------------------

   DEFINE i SMALLINT

   FOR i = 1 TO 2
      LET t_imss[i]  = 0
      LET t_issst[i] = 0
      LET t_asig[i]  = 0
      LET t_indep[i] = 0
      LET t_volun[i] = 0
      LET t_persp[i] = 0
      LET t_ahorr[i] = 0
      LET t_compl[i] = 0
      LET t_total[i] = 0
   END FOR

END FUNCTION


REPORT rep_saldos()
#------------------

   DEFINE
      i,
      j       SMALLINT,
      Tit_1   CHAR(14)

   OUTPUT
      PAGE LENGTH    90
      LEFT MARGIN     0
      RIGHT MARGIN  150
      TOP MARGIN      0
      BOTTOM MARGIN   0

   FORMAT
      PAGE HEADER

         SKIP 1 LINES

         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
         PRINT COLUMN 1, '\033e\033(s218T\033(s15H\033(s7B'
         PRINT COLUMN 042,"SALDOS DE AHORRO VOLUNTARIO POR PERFIL DE LOS TRABAJADORES",
               COLUMN 126, "PAGINA : ", PAGENO USING "####"
         PRINT COLUMN 126, "FECHA  : ", TODAY USING "DD/MM/YYYY"

         SKIP 1 LINE

         PRINT COLUMN 002, "MES QUE SE REPORTA: ", fecini USING "MM/YYYY"
         PRINT COLUMN 009, "CLAVE AFORE: ", cve_afo
         PRINT COLUMN 015, "AFORE: ", raz_soc

         SKIP 1 LINE

      ON EVERY ROW
         CALL IniciaTots()    #* Inicializa Variables de Totales Verticales *

         FOR j = 1 TO 2
            CASE j
               WHEN 1
                  #* Primero, Trabajadores que SI manifestaron *
                  PRINT COLUMN 001, "1. Recursos de Trabajadores que manifestaron obtene",
                                    "r el beneficio de la deduccion fiscal de la Ley del Impuesto sobre",
                                    "la Renta (Art. 176 y 218)."

               WHEN 2
                  #* Segundo, Trabajadores que NO manifestaron *
                  PRINT COLUMN 001, "2. Recursos de Trabajadores que no manifestaron obt",
                                    "ener el beneficio de la deduccion fiscal de la Ley del Impuesto so",
                                    "bre la Renta (Art. 176 y 218)."
            END CASE

            PRINT "-----------------------------------------------------------------",
                  "----------------------------------------------------------------------",
                  "---------"

            PRINT COLUMN 027, "Numero de Cuentas", 
                  COLUMN 89, "Titulos por Subcuenta"
            PRINT COLUMN 074, "A H O R R O  V O L U N T A R I O"
            PRINT COLUMN 002, "PERFIL DEL",      COLUMN 016, "Afiliados", 
                  COLUMN 026, "Afiliados",       COLUMN 047, "Indepen-",
                  COLUMN 060, "Aportaciones",    COLUMN 076, "Con Perspectiva", 
                  COLUMN 098, "De Ahorro",       COLUMN 114, "Complementarias"
            PRINT COLUMN 002, "TRABAJADOR",      COLUMN 018, "IMSS",
                  COLUMN 027, "ISSSTE",          COLUMN 036, "Asignados",
                  COLUMN 047, "dientes",         COLUMN 061, "Voluntarias",     
                  COLUMN 078, "Largo Plazo",     COLUMN 097, "Largo Plazo",     
                  COLUMN 116, "De Retiro",       COLUMN 131, "Total Siefore"
            PRINT "-----------------------------------------------------------------",
                  "----------------------------------------------------------------------",
                  "---------"

            SKIP 1 LINES

            FOR i = 1 TO 12
               CASE i
                  WHEN 1
                     LET Tit_1 = "SB1 ASIGNADO"
                  WHEN 2
                     LET Tit_1 = "     ELEGIDO"
                  WHEN 3
                     LET Tit_1 = "SB2 ASIGNADO"
                  WHEN 4
                     LET Tit_1 = "     ELEGIDO"
                  WHEN 5
                     LET Tit_1 = "SB3 ASIGNADO"
                  WHEN 6
                     LET Tit_1 = "     ELEGIDO"
                  WHEN 7
                     LET Tit_1 = "SB4 ASIGNADO"
                  WHEN 8
                     LET Tit_1 = "     ELEGIDO"
                  WHEN 9
                     LET Tit_1 = "SB5 ASIGNADO"
                  WHEN 10
                     LET Tit_1 = "     ELEGIDO"
                  WHEN 11
                     LET Tit_1 = "Sief. Ad. C.P."
                  WHEN 12
                     LET Tit_1 = "Sief. Ad. L.P."
               END CASE

               PRINT COLUMN 002, Tit_1,
                     COLUMN 019, ar_sdo[j,i].n_imss  USING "#####&",
                     COLUMN 029, ar_sdo[j,i].n_issst USING "#####&",
                     COLUMN 039, ar_sdo[j,i].n_asig  USING "#####&",
                     COLUMN 048, ar_sdo[j,i].n_indep USING "#####&",
                     COLUMN 055, ar_sdo[j,i].t_volun USING "#########&.&&&&&&",
                     COLUMN 073, ar_sdo[j,i].t_cpers USING "#########&.&&&&&&",
                     COLUMN 091, ar_sdo[j,i].t_ahorr USING "#########&.&&&&&&",
                     COLUMN 108, ar_sdo[j,i].t_compl USING "#########&.&&&&&&",
                     COLUMN 127, ar_sdo[j,i].t_siefo USING "#########&.&&&&&&" 
               
               LET t_imss[j]  = t_imss[j] + ar_sdo[j,i].n_imss
               LET t_issst[j] = t_issst[j] + ar_sdo[j,i].n_issst
               LET t_asig[j]  = t_asig[j] + ar_sdo[j,i].n_asig
               LET t_indep[j] = t_indep[j] + ar_sdo[j,i].n_indep
               LET t_volun[j] = t_volun[j] + ar_sdo[j,i].t_volun
               LET t_persp[j] = t_persp[j] + ar_sdo[j,i].t_cpers
               LET t_ahorr[j] = t_ahorr[j] + ar_sdo[j,i].t_ahorr
               LET t_compl[j] = t_compl[j] + ar_sdo[j,i].t_compl
               LET t_total[j] = t_total[j] + ar_sdo[j,i].t_siefo

            END FOR

            PRINT "-----------------------------------------------------------------",
                  "----------------------------------------------------------------------",
                  "---------"

            PRINT COLUMN 002, "Total Afore",
                  COLUMN 019, t_imss[j]  USING "#####&",
                  COLUMN 029, t_issst[j] USING "#####&",
                  COLUMN 039, t_asig[j]  USING "#####&",
                  COLUMN 048, t_indep[j] USING "#####&",
                  COLUMN 055, t_volun[j] USING "#########&.&&&&&&",
                  COLUMN 073, t_persp[j] USING "#########&.&&&&&&",
                  COLUMN 091, t_ahorr[j] USING "#########&.&&&&&&",
                  COLUMN 108, t_compl[j] USING "#########&.&&&&&&",
                  COLUMN 127, t_total[j] USING "#########&.&&&&&&"

            PRINT "-----------------------------------------------------------------",
                  "----------------------------------------------------------------------",
                  "---------"

            SKIP 2 LINES

         END FOR


      ON LAST ROW
         #* Imprime Sumario, suma totales verticales Detalle 1 y 2 *
         PRINT COLUMN 001, "3. Totales (suma del cuadro 1 y 2)"

         PRINT "-----------------------------------------------------------------",
               "----------------------------------------------------------------------",
               "---------"

         PRINT COLUMN 027, "Numero de Cuentas", COLUMN 89, "Titulos por Subcuenta"
         PRINT COLUMN 074, "A H O R R O  V O L U N T A R I O"
         PRINT COLUMN 002, "PERFIL DEL",      COLUMN 016, "Afiliados",
               COLUMN 026, "Afiliados",       COLUMN 047, "Indepen-",
               COLUMN 060, "Aportaciones",    COLUMN 076, "Con Perspectiva",
               COLUMN 098, "De Ahorro",       COLUMN 114, "Complementarias"
         PRINT COLUMN 002, "TRABAJADOR",      COLUMN 018, "IMSS",
               COLUMN 027, "ISSSTE",          COLUMN 036, "Asignados",
               COLUMN 047, "dientes",         COLUMN 061, "Voluntarias",
               COLUMN 078, "Largo Plazo",     COLUMN 097, "Largo Plazo",
               COLUMN 116, "De Retiro",       COLUMN 131, "Total Siefore"

         PRINT "-----------------------------------------------------------------",
               "----------------------------------------------------------------------",
               "---------"

         SKIP 1 LINES

         PRINT COLUMN 002, "Total Afore", 
               COLUMN 019, t_imss[1] + t_imss[2]   USING "#####&",
               COLUMN 029, t_issst[1] + t_issst[2] USING "#####&",
               COLUMN 039, t_asig[1] + t_asig[2]   USING "#####&",
               COLUMN 048, t_indep[1] + t_indep[2] USING "#####&",
               COLUMN 055, t_volun[1] + t_volun[2] USING "#########&.&&&&&&",
               COLUMN 073, t_persp[1] + t_persp[2] USING "#########&.&&&&&&",
               COLUMN 091, t_ahorr[1] + t_ahorr[2] USING "#########&.&&&&&&",
               COLUMN 109, t_compl[1] + t_compl[2] USING "#########&.&&&&&&",
               COLUMN 127, t_total[1] + t_total[2] USING "#########&.&&&&&&"

         PRINT "-----------------------------------------------------------------",
               "----------------------------------------------------------------------",
               "---------"

         SKIP 2 LINES

         #* Imprime Cifras de Control *
         PRINT COLUMN 001, "4. Cifras de Control al cierre del mes"

         PRINT "-----------------------------------------------------------------",
               "----------------------------------------------------------------------",
               "---------"

         PRINT COLUMN 055, "Numero de Cuentas", 
               COLUMN 111, "Num. de Ctas. c/Aport. Voluntarias"
         PRINT COLUMN 002, "CIFRAS TOTALES",    COLUMN 018, "Saldo Cero",
               COLUMN 030, "Inhabilitadas",     COLUMN 046, "Inhabilitadas",
               COLUMN 060, "Afiliados",         COLUMN 071, "Afiliados",
               COLUMN 093, "Indepen-",          COLUMN 115, "Al Dia",
               COLUMN 127, "Que realizaron"
         PRINT COLUMN 030, "Por Traspaso",      COLUMN 044, "Por Unificacion", 
               COLUMN 062, "IMSS",              COLUMN 072, "ISSSTE", 
               COLUMN 082, "Asignados",         COLUMN 093, "dientes", 
               COLUMN 106, "Total",             COLUMN 113, "del Corte",
               COLUMN 126, "Aport. en el mes"

         PRINT "-----------------------------------------------------------------",
               "----------------------------------------------------------------------",
               "---------"

         PRINT COLUMN 002, "Total Afore",
               COLUMN 022, ar_ccon[1] USING "#####&",
               COLUMN 035, ar_ccon[2] USING "#####&",
               COLUMN 051, ar_ccon[3] USING "#####&", 
               COLUMN 062, ar_ccon[4] USING "#######&",
               COLUMN 073, ar_ccon[5] USING "#####&",
               COLUMN 084, ar_ccon[6] USING "#####&",
               COLUMN 094, ar_ccon[7] USING "#####&",
               COLUMN 105, ar_ccon[8] USING "######&",
               COLUMN 115, ar_ccon[9] USING "#####&",
               COLUMN 133, ar_ccon[10] USING "#####&"

         PRINT "-----------------------------------------------------------------",
               "----------------------------------------------------------------------",
               "---------"

         SKIP 2 LINES

         #* Imprime los Precios de Accion *
         PRINT COLUMN 94, "Precios"
         PRINT COLUMN 055, "   SB1    ",
               COLUMN 070, "   SB2    ",
               COLUMN 085, "   SB3    ",
               COLUMN 100, "   SB4    ",
               COLUMN 115, "   SB5    ",
               COLUMN 130, "   SACP   "
         PRINT COLUMN 055, ar_prec[1] USING "###.######",
               COLUMN 070, ar_prec[2] USING "###.######",
               COLUMN 085, ar_prec[3] USING "###.######",
               COLUMN 100, ar_prec[4] USING "###.######",
               COLUMN 115, ar_prec[5] USING "###.######",
               COLUMN 130, ar_prec[6] USING "###.######"

END REPORT
