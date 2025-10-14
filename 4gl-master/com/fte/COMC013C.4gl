################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.        					       #
#Programa          => COMC013C                                                 #
#Descripcion       => GENERACION DEL  ESTADO DE CUENTA PARA EL PROMOTOR        #
#Fecha             => 22 Octub 2004.     				       #
#By                => JOSE ALEJANDRO RAMIREZ.        	                       #
#Sistema           => COM. 					               #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE 

   enter           char(01),
   g_param_dis     RECORD LIKE seg_modulo.* ,
   w_codigo_afore  LIKE tab_afore_local.codigo_afore,
   g_usuario       CHAR (08),


   vmenu     CHAR(01),
   aux_pausa	CHAR(1),
   HOY	DATE,
   SW_1      SMALLINT,
   cla_sel 	CHAR(1500), 
   cla_sel2	CHAR(800), 
   vaccion   smallint,
   cla_where CHAR(800), 
   vcomando  SMALLINT,
   opc 	CHAR(01),
   total     DECIMAL(12,2),
   pagada    DECIMAL(12,2),
   registros INTEGER,
   longitud  integer,

   vregis             INTEGER,
   cont_de_registros  INTEGER,
   sumtot_afil        INTEGER,
   sumtot_sm          DECIMAL(8,2),
   sumtot_promsm      DECIMAL(6,2),
   sumtot_comi        DECIMAL(12,2),
   vnom_jefe          CHAR(50)

   DEFINE fcodven           CHAR(10)      
   DEFINE xcodven           CHAR(10)      
   DEFINE xfecha_corte      CHAR(8)      
   DEFINE xfecha_corte2     DATE
        
   DEFINE g_grep_00         CHAR(1000)
   DEFINE g_grep_03         CHAR(1000)
   DEFINE columnas          CHAR(200)
   DEFINE comando           CHAR(200)
   DEFINE tott_num_sm        LIKE com_comis_detalle.num_sm

END GLOBALS

MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o
	
	DEFER INTERRUPT

        CALL STARTLOG("COMC013C.log")

        LET xcodven       = ARG_VAL(1)
        LET xfecha_corte2 = ARG_VAL(2)


        SELECT  ruta_rescate
        INTO    g_param_dis.ruta_rescate
        FROM    seg_modulo
        WHERE   modulo_cod='com'

        SELECT  codigo_afore,USER
        INTO    w_codigo_afore,g_usuario
        FROM    tab_afore_local

	LET HOY = TODAY
        LET vregis = 0

        CALL imprime()
END MAIN


FUNCTION imprime()

   DEFINE G_IMPRE    CHAR(2000)
   DEFINE G_LISTA    CHAR(300)
   DEFINE impresion  CHAR(300)
   DEFINE tcodven    LIKE com_comis_detalle.codven
   DEFINE tcoduni_n1 LIKE com_comis_detalle.coduni_n1

   DEFINE nuevo RECORD 
      n_folio            LIKE com_comis_detalle.n_folio,
      nss                LIKE com_comis_detalle.nss,
      codven             LIKE com_comis_detalle.codven,
      coduni_n1          LIKE com_comis_detalle.coduni_n1,
      fentcons           LIKE com_comis_detalle.fentcons,
      salario_base_comis LIKE com_comis_detalle.salario_base_comis,
      num_sm             LIKE com_comis_detalle.num_sm,
      monto_comision     LIKE com_comis_detalle.monto_comision,
      estado_comision    LIKE com_comis_detalle.estado_comision,
      nombre_pro         CHAR(60),
      nombre_cli         CHAR(60),
      tipo_solicitud     SMALLINT,
      fentcons_afi       DATE
   END RECORD,

   mandar RECORD
      n_folio            LIKE com_comis_detalle.n_folio,
      nss                LIKE com_comis_detalle.nss,
      codven             LIKE com_comis_detalle.codven,
      coduni_n1          LIKE com_comis_detalle.coduni_n1,
      fentcons           LIKE com_comis_detalle.fentcons,
      salario_base_comis LIKE com_comis_detalle.salario_base_comis,
      num_sm             LIKE com_comis_detalle.num_sm,
      monto_comision     LIKE com_comis_detalle.monto_comision,
      estado_comision    LIKE com_comis_detalle.estado_comision,
      nombre_pro         CHAR(45),
      nombre_cli         CHAR(45)
   END RECORD,

      xpaterno           CHAR(40),
      xmaterno           CHAR(40),
      xnombres           CHAR(40),
      xpaterno2          CHAR(40),
      xmaterno2          CHAR(40),
      xnombres2          CHAR(40),
      vdescrip           CHAR(40),
      vcausa             CHAR(100)

    WHENEVER ERROR CONTINUE
    DROP TABLE temp_listo

    LET tott_num_sm = 0

    CREATE TEMP TABLE temp_listo
    (
      codven char(10), 
      nom_pro char(50),
      cencos char(10),
      folio_clie integer,
      nss_clie char(11),
      nom_clie char(50),
      fec_certif date,
      sala_base_comis decimal(12,2),
      num_sm decimal(6,2),
      monto_comis decimal(12,2),
      edo_comis smallint,
      descr_edo CHAR(40),
      descr_causa CHAR(70)
    )

    WHENEVER ERROR STOP
  

    LET G_IMPRE = g_param_dis.ruta_rescate CLIPPED,"/",g_usuario CLIPPED,
                              ".2RESDO_CTA_PRO",HOY USING "DDMMYYYY"

    LET G_LISTA = g_param_dis.ruta_rescate CLIPPED,"/",g_usuario CLIPPED,
                              ".2AEDO_CTA_PRO",HOY USING "DDMMYYYY"

    -- PARA IMPRESION
    START REPORT rpt_cuenta_imp TO G_IMPRE

    LET cla_sel ="SELECT b.n_folio,               ",
                 "b.n_seguro,                     ",
                 "b.codven,                       ",
                 "p.agenc_cod,                    ",
                 "b.frecafor,                     ",
                 "0,                              ",
                 "0,                              ",
                 "0,                              ",
                 "b.status_interno,               ",
                 "b.tipo_solicitud,               ",
                 "b.fentcons                      ",
                 "FROM   afi_solicitud  b,        ",
                 "       tab_status_afi c,        ",
                 "       pro_mae_promotor p       ",
                 "WHERE b.codven = '",xcodven CLIPPED, 
                 "' AND b.codven = p.cod_promotor ", 
                 "  AND b.frecafor <= '",xfecha_corte2 CLIPPED,
                 "' AND   b.status_interno = c.estado_cod ",
                 "AND   b.status_interno < 100    ",
                 "AND   b.codven not in (select codven from com_comis_detalle",
                 "     where  fecha_corte='",xfecha_corte2 CLIPPED,
                 "') GROUP BY 1,2,3,4,5,9,10,11      "

    ERROR "Preparando la informacion para imprimir..."
         PREPARE claexe8 FROM cla_sel

         DECLARE cursor_12 CURSOR FOR claexe8
         FOREACH cursor_12 INTO  nuevo.n_folio,
                        nuevo.nss,
                        nuevo.codven,
                        nuevo.coduni_n1,
                        nuevo.fentcons,
                        nuevo.salario_base_comis,
                        nuevo.num_sm,
                        nuevo.monto_comision,
                        nuevo.estado_comision,
                        nuevo.tipo_solicitud,
                        nuevo.fentcons_afi


         INITIALIZE mandar.* TO NULL



         SELECT estado_desc
         INTO   vdescrip
         FROM   tab_status_afi
         WHERE  estado_cod = nuevo.estado_comision


         IF vdescrip IS NULL THEN
            LET vdescrip = ' '
         END IF


         SELECT MAX(observacion)
         INTO   vcausa
         FROM   afi_rechaza_cert
         WHERE  n_seguro=nuevo.nss
         AND    f_rechazo=nuevo.fentcons_afi
         AND    n_folio  =nuevo.n_folio
         AND    tipo_solicitud = nuevo.tipo_solicitud

         IF vcausa IS NULL THEN
            LET vcausa = ' '
         END IF

         LET mandar.n_folio             = nuevo.n_folio
         LET mandar.nss                 = nuevo.nss
         LET mandar.codven              = nuevo.codven
         LET mandar.coduni_n1           = nuevo.coduni_n1
         LET mandar.fentcons            = nuevo.fentcons
         LET mandar.salario_base_comis  = nuevo.salario_base_comis
         LET mandar.num_sm              = nuevo.num_sm
         LET mandar.monto_comision      = nuevo.monto_comision
         LET mandar.estado_comision     = nuevo.estado_comision

 
         SELECT paterno,materno,nombres
         INTO   xpaterno,xmaterno,xnombres
         FROM   pro_mae_promotor
         WHERE  cod_promotor = nuevo.codven

         IF     xpaterno IS NULL THEN
                LET xpaterno = ' '
         END IF

         IF     xmaterno IS NULL THEN
                LET xmaterno = ' '
         END IF

         IF     xnombres IS NULL THEN
                LET xnombres = ' '
         END IF

         LET mandar.nombre_pro = xpaterno CLIPPED||' '||
                                 xmaterno CLIPPED||' '||
                                 xnombres CLIPPED
 
         SELECT paterno,materno,nombres
         INTO   xpaterno2,xmaterno2,xnombres2
         FROM   afi_solicitud
         WHERE  n_seguro = nuevo.nss
         AND    tipo_solicitud = nuevo.tipo_solicitud
         AND    n_folio = nuevo.n_folio

         IF     xpaterno2 IS NULL THEN
                LET xpaterno2 = ' '
         END IF

         IF     xmaterno2 IS NULL THEN
                LET xmaterno2 = ' '
         END IF

         IF     xnombres2 IS NULL THEN
                LET xnombres2 = ' '
         END IF

         LET mandar.nombre_cli = xpaterno2 CLIPPED||' '||
                           xmaterno2 CLIPPED||' '||
                           xnombres2 CLIPPED


   LET vregis = vregis + 1

   -- PARA LA IMPRESION
   OUTPUT TO REPORT rpt_cuenta_imp(mandar.*,vdescrip,vcausa)

   END FOREACH

   FINISH REPORT rpt_cuenta_imp
-- ERROR "LISTADO 1 GENERADO...."
-- SLEEP 2
-- ERROR ""

   unload to G_LISTA
   select * from temp_listo order by 1,11 desc

   LET columnas = "COD PROMOTOR'|'NOM PROMOTOR'|'CENTRO COSTOS'|'FOLIO CLIEN'|'NSS CLIE'|',",
                  "NOM CLIEN'|'FEC CERTIF'|'SALAR BASE COMI'|'VSM'|'MONTO COMIS'|'ESTADO COMIS'|'DESCRIP EDO'|'DESCRIP CAUSA"

   LET comando ="cd ",g_param_dis.ruta_rescate CLIPPED,"; echo ",columnas CLIPPED,"> alguno3" CLIPPED
   RUN comando

   LET comando ="cd ",g_param_dis.ruta_rescate CLIPPED,"; cat ",G_LISTA CLIPPED,">> alguno3" CLIPPED,";mv alguno3 ",G_LISTA CLIPPED
   RUN comando
 

  LET impresion = "lp ",G_IMPRE
--  RUN impresion

   ERROR "LISTADO 1 GENERADO...."
   SLEEP 2
   ERROR ""

END FUNCTION

REPORT rpt_cuenta_imp(lnuevo,vdescrip_edo,vcausa_edo)

  DEFINE
    lnuevo RECORD
      n_folio                    LIKE com_comis_detalle.n_folio,
      nss                        LIKE com_comis_detalle.nss,
      codven                     LIKE com_comis_detalle.codven,
      coduni_n1                  LIKE com_comis_detalle.coduni_n1,
      fentcons                   LIKE com_comis_detalle.fentcons,
      salario_base_comis         LIKE com_comis_detalle.salario_base_comis,
      num_sm                     LIKE com_comis_detalle.num_sm,
      monto_comision             LIKE com_comis_detalle.monto_comision,
      estado_comision            LIKE com_comis_detalle.estado_comision,
      nombre_pro                 CHAR(45),
      nombre_cli                 CHAR(45)
   END RECORD

   DEFINE vdescrip_edo           CHAR(40)
   DEFINE vcausa_edo             CHAR(100)

      --enter                      char(01)
     
   DEFINE subt_num_sm        LIKE com_comis_detalle.num_sm
 OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   90
      ORDER BY lnuevo.coduni_n1,lnuevo.codven,lnuevo.estado_comision desc
  FORMAT
     PAGE HEADER

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
      PRINT COLUMN 02,'\033(s7B',"COMC013                                                                                                                                     ",today USING "dd-mm-yyyy",'\033(s0B'
      SKIP 2 LINE
      PRINT COLUMN 80,'\033(s7B',"ESTADO DE CUENTA DEL PROMOTOR",'\033(s0B'
      SKIP 1 LINE
 

      BEFORE GROUP OF lnuevo.codven

      LET subt_num_sm = 0  

      PRINT COLUMN  001,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '______________________________________'

      SKIP 1 LINE


      PRINT COLUMN 01,'\033(s7B',"Num promotor  Nombre promotor                    Grp Venta   Fecha corte   ",'\033(s0B'
      PRINT COLUMN  001,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '______________________________________'
      SKIP 1 LINE
      PRINT COLUMN 002,lnuevo.codven CLIPPED,
            COLUMN 013,lnuevo.nombre_pro CLIPPED,
            COLUMN 057,lnuevo.coduni_n1 CLIPPED,
            COLUMN 067,xfecha_corte2 USING "DD-MM-YYYY"

      SKIP 1 LINE


      PRINT COLUMN 05,"FOLIO        NSS        NOMBRE DEL TRABAJADOR                        FEC CERTIF        SALARIO     VSM        COMSION  EDO COMIS   DESCR EDO"

      PRINT COLUMN  005,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '_____'
      SKIP 1 LINE

      ON EVERY ROW

   -- display "MAY",sum_sala_may_seis 
   -- prompt '' for enter

      PRINT COLUMN 004,lnuevo.n_folio              USING "&&&&&&&&&",
            COLUMN 015,lnuevo.nss          CLIPPED,
            COLUMN 028,lnuevo.nombre_cli,
            COLUMN 073,lnuevo.fentcons             USING "dd-mm-yyyy",
            COLUMN 085,lnuevo.salario_base_comis   USING "#########&&.&&",
            COLUMN 100,lnuevo.num_sm               USING "####&.&&",
            COLUMN 110,lnuevo.monto_comision       USING "#########&&.&&",
            COLUMN 126,lnuevo.estado_comision      USING "##&",
            COLUMN 130,vdescrip_edo CLIPPED

      PRINT COLUMN 130,vcausa_edo CLIPPED


      INSERT INTO temp_listo
      VALUES(lnuevo.codven,lnuevo.nombre_pro,lnuevo.coduni_n1,lnuevo.n_folio,lnuevo.nss,lnuevo.nombre_cli,lnuevo.fentcons,lnuevo.salario_base_comis,lnuevo.num_sm,lnuevo.monto_comision,lnuevo.estado_comision,vdescrip_edo,vcausa_edo)


      ----@@@@
      AFTER GROUP OF lnuevo.codven
      PRINT COLUMN 085,'___________________________________________________________'
      PRINT COLUMN 085,GROUP SUM(lnuevo.salario_base_comis) USING "#########&&.&&"," ",GROUP SUM(lnuevo.num_sm) USING "####&.&&","  ",GROUP SUM(lnuevo.monto_comision) USING "#########&&.&&",'          ',subt_num_sm USING "####&.&&"
      PRINT COLUMN 085,'    TOTA_SALA.  TOTA_VSM    TOTA_COMIS.        TOTA_CALIDAD'

      LET tott_num_sm = tott_num_sm + subt_num_sm


      SKIP TO TOP OF PAGE
      ---------------SKIP  2 LINES   OJO


      PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"


      ON LAST ROW

      PRINT COLUMN 023,'_____________________________________________________________________________________________________________________________________________'
      PRINT COLUMN 085,SUM(lnuevo.salario_base_comis) USING "#########&&.&&"," ",SUM(lnuevo.num_sm) USING "####&.&&","  ",SUM(lnuevo.monto_comision) USING "#########&&.&&",'          ',tott_num_sm USING "####&.&&"
      PRINT COLUMN 085,'    TOTA_SALA.  TOTA_VSM    TOTA_COMIS.        TOTA_CALIDAD'
      PRINT COLUMN 085,'                      GRAN  TOTAL                          '

      SKIP  3 LINES

      END REPORT
