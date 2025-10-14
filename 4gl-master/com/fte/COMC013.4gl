################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.        					       #
#Programa          => COMC013                                                  #
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


      g_reg0 RECORD
	      fecha_corte  DATE,
              codven       CHAR(10)
      END RECORD,

      g_reg2 RECORD
	      paterno       CHAR(40),
      	materno       CHAR(40),
      	nombres       CHAR(40)
      END RECORD,


      g_reg ARRAY[1000] OF RECORD 
         codven             LIKE com_comis_detalle.codven, 
         coduni_n1          LIKE com_comis_detalle.coduni_n1,
	      vnom               CHAR(55)
      END RECORD, 

      g_reg3 ARRAY[1000] OF RECORD
           n_folio     	    LIKE com_comis_detalle.n_folio,
           nss                LIKE com_comis_detalle.nss,
     	   tipo_solicitud     LIKE com_comis_detalle.tipo_solicitud,
	   fentcons  	    LIKE com_comis_detalle.fentcons,
         fecha_corte        LIKE com_comis_detalle.fecha_corte,
         salario_base_comis LIKE com_comis_detalle.salario_base_comis,
	      num_sm             LIKE com_comis_detalle.num_sm,
	      monto_comision     LIKE com_comis_detalle.monto_comision,
     	   estado_comision    LIKE com_comis_detalle.estado_comision
      END RECORD, 

      g_reg33 ARRAY[1000] OF RECORD
         n_folio     	    LIKE afi_solicitud.n_folio,
  	      n_seguro           LIKE afi_solicitud.n_seguro,
	      tipo_solicitud     LIKE afi_solicitud.tipo_solicitud,
	      frecafor  	    LIKE afi_solicitud.fentcons,
         salario_base_comis LIKE afi_solicitud.salario_base_comis,
         status_interno     LIKE afi_solicitud.status_interno,
	      estado_desc        LIKE afi_status_int.estado_desc
      END RECORD, 

   g_reg4 ARRAY[6] OF RECORD	
      seguro         LIKE pro_mae_promotor.seguro,
      paterno        LIKE pro_mae_promotor.paterno,
      materno        LIKE pro_mae_promotor.materno,
      nombres        LIKE pro_mae_promotor.nombres
   END RECORD,

   g_reg5 ARRAY[6] OF RECORD	
      seguro         LIKE pro_mae_promotor.seguro,
      nombre         CHAR(50)
   END RECORD,

   g_reg6 ARRAY[6] OF RECORD	
      n_folio        LIKE afi_solicitud.n_folio,
      n_seguro       LIKE afi_solicitud.n_seguro,
      tipo_solicitud LIKE afi_solicitud.tipo_solicitud,
      paterno        LIKE afi_solicitud.paterno,
      materno        LIKE afi_solicitud.materno,
      nombres        LIKE afi_solicitud.nombres,
      observacion    LIKE afi_rechaza_cert.observacion
   END RECORD,

   g_reg7 ARRAY[6] OF RECORD	
      n_folio        LIKE afi_solicitud.n_folio,
      n_seguro       LIKE afi_solicitud.n_seguro,
      tipo_solicitud LIKE afi_solicitud.tipo_solicitud,
      nombre         CHAR(50),
      observacion     LIKE afi_rechaza_cert.observacion
   END RECORD,

   nombre CHAR(50),

      vmenu     CHAR(01),
      aux_pausa	CHAR(1),
      HOY	DATE,
      SW_1      SMALLINT,
      cla_sel 	CHAR(1500), 
      cla_sel2	CHAR(800), 
      cla_sell	CHAR(800), 
      vaccion   smallint,
      cla_where CHAR(800), 
      vcomando  SMALLINT,
      opc 	CHAR(01),
      total     DECIMAL(12,2),
      pagada    DECIMAL(12,2),
      registros INTEGER,
      longitud  integer,

    l_record_1  ARRAY[700] OF RECORD
         n_folio     	    LIKE afi_solicitud.n_folio,
  	      n_seguro           LIKE afi_solicitud.n_seguro,
	      tipo_solicitud     LIKE afi_solicitud.tipo_solicitud,
	      frecafor  	    LIKE afi_solicitud.fentcons,
	      vfecha_corte       LIKE com_comis_detalle.fecha_corte,
         salario_base_comis LIKE afi_solicitud.salario_base_comis,
	      num_sm             LIKE com_comis_detalle.num_sm,
         monto_comision     LIKE com_comis_detalle.monto_comision
    END RECORD,
    
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
   DEFINE ejecuta           CHAR(200)
   DEFINE tott_num_sm        LIKE com_comis_detalle.num_sm
   DEFINE ban_solo_admon    SMALLINT

END GLOBALS

MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o
	
	DEFER INTERRUPT

        CALL STARTLOG("COMC0131.log")

        SELECT  ruta_rescate
        INTO    g_param_dis.ruta_rescate
        FROM    seg_modulo
        WHERE   modulo_cod='com'

      --LET g_param_dis.ruta_listados="/home/aramirez/ACTINVE/new_version" --ojo
      --LET g_param_dis.ruta_listados="/safre/com/fte/alex/pago_comer" --ojo

        SELECT  codigo_afore,USER
        INTO    w_codigo_afore,g_usuario
        FROM    tab_afore_local

	LET HOY = TODAY
        LET vregis = 0

	OPEN WINDOW ventana_1 AT 2,2 WITH FORM "COMC0131" ATTRIBUTE( BORDER)
	DISPLAY " COMC013            ESTADO DE CUENTA MENSUAL DE COMISIONES                    " AT 3,1 ATTRIBUTE(REVERSE) 

	DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

	DISPLAY "                                                                               " AT 7,1 ATTRIBUTE(REVERSE)

	DISPLAY "                                                                               " AT 09,1 ATTRIBUTE(REVERSE)

  	DISPLAY "                                                                               " AT 19,1 ATTRIBUTE(REVERSE)

	MENU "CONSULTA"
           COMMAND "Consulta" "Consulta Comisiones"
              CALL Inicializa2()
	      CALL Consulta()
           COMMAND "Salir" "Salir del Programa"
	      EXIT MENU
	END MENU
        CLOSE WINDOW ventana_1
END MAIN

FUNCTION Inicializa()
    define i smallint
        LET sw_1 = 0
	INITIALIZE g_reg TO NULL 



        for i=1 to 9
	   DISPLAY g_reg[i].* TO scr_1[i].*
        end for

	LET total = 0
	LET registros = 0
        DISPLAY registros TO scr_3.*

        CLEAR SCREEN
END FUNCTION

FUNCTION Inicializa2()
   LET cla_where=NULL
   LET cla_sel=NULL
   INITIALIZE g_reg0.* TO NULL    --g_reg2.* ojo
   DISPLAY BY NAME g_reg0.*        --,g_reg2.* ojo
END FUNCTION

FUNCTION Consulta()
   DEFINE
      pat CHAR(40),
      mat char(40),
      nom char(40),
      pos SMALLINT,
      xcodven LIKE com_comis_detalle.codven,
      vvalor    CHAR(01) 

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " [ESC] Procesar " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-C] Salir " AT 1,63 ATTRIBUTE(REVERSE)


      LET INT_FLAG = FALSE
      CONSTRUCT cla_where 
	 ON    a.fecha_corte,
               a.codven
         FROM  com_comis_detalle.fecha_corte, 
               com_comis_detalle.codven

         ON KEY (ESC)

         LET xfecha_corte  = get_fldbuf(com_comis_detalle.fecha_corte)
         LET xcodven       = get_fldbuf(com_comis_detalle.codven)

            LET vcomando = 2
            EXIT CONSTRUCT
         ON KEY (INTERRUPT)
            LET vcomando=1
            EXIT CONSTRUCT
       END CONSTRUCT


            
       IF vcomando = 1 THEN
          LET INT_FLAG = FALSE
          ERROR "Operacion abortada"
          LET vcomando = 0
          RETURN
       END IF
    
       IF xfecha_corte IS NULL OR xfecha_corte= ' ' THEN
          ERROR "Es preciso ingresar la fecha de corte"
          SLEEP 3
          LET INT_FLAG = FALSE
          ERROR ""
          LET vcomando = 0
          RETURN
       END IF

       LET xfecha_corte2 = xfecha_corte[3,4]||'/'||xfecha_corte[1,2]||'/'||
                           xfecha_corte[5,8]

       IF xcodven IS NULL THEN
          ERROR "Es preciso ingresar codigo del promotor"
          SLEEP 3
          LET INT_FLAG = FALSE
          ERROR ""
          LET vcomando = 0
          RETURN
       END IF

       LET ban_solo_admon = 0
      
       IF xcodven='1234567' THEN 
          LET cla_where = cla_where[1,26]
          LET ban_solo_admon = 1
       END IF 

         LET cla_sel ="SELECT a.nivel,",
                           "a.agenc_cod,'",xfecha_corte2,
                           "', a.cod_promotor ",
                    "FROM   pro_mae_promotor a ",
                    "WHERE a.cod_promotor='",xcodven CLIPPED,
                    "' ORDER BY 2"


         PREPARE claexe2 FROM cla_sel
         DECLARE cursor_2 SCROLL CURSOR FOR claexe2
         OPEN cursor_2

         CALL primer_row()
   DISPLAY " [Ctrl-F]Concurso" AT 6,01 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-E]Contratos pendientes" AT 6,18 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-B]Contratos rechazados   " AT 6,47 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-I]Imprime  [Ctrl-P]Clientes promotor" AT 7,01 ATTRIBUTE(REVERSE)
	 CALL ver_arreglo(xcodven)

END FUNCTION


FUNCTION primer_row()

   FETCH FIRST cursor_2 INTO g_reg0.fecha_corte  --,g_reg2.* ojo
   IF STATUS=100 THEN
      ERROR "No hay registros en esta direccion "
   ELSE
      LET g_reg0.fecha_corte=xfecha_corte2
      LET g_reg0.codven=xcodven
      
      DISPLAY BY NAME g_reg0.*
   END IF

END FUNCTION



FUNCTION imprime(ban_agreg,ycodven)

   DEFINE G_IMPRE    CHAR(2000)
   DEFINE G_LISTA    CHAR(300)
   DEFINE impresion  CHAR(300)
   DEFINE tcodven    LIKE com_comis_detalle.codven
   DEFINE ycodven    LIKE com_comis_detalle.codven
   DEFINE tcoduni_n1 LIKE com_comis_detalle.coduni_n1
   DEFINE ban_agreg  SMALLINT

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


    IF ban_agreg = 1 THEN
    --*************************************************
    -- Redirecciono la ejecucion a integrar los reg pendientes y rechazados 
    --*************************************************
   LET ejecuta = "nohup time fglgo COMC013C.4gi ",ycodven," ",xfecha_corte2," &"
   RUN ejecuta

--   ERROR "LISTADO 1 GENERADO...."
--   SLEEP 2
--   ERROR ""


    ELSE



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
                                ".RESDO_CTA_PRO",HOY USING "DDMMYYYY"

      LET G_LISTA = g_param_dis.ruta_rescate CLIPPED,"/",g_usuario CLIPPED,
                                ".AEDO_CTA_PRO",HOY USING "DDMMYYYY"

      -- PARA IMPRESION
      START REPORT rpt_cuenta_imp TO G_IMPRE

      LET cla_sel ="SELECT a.n_folio,               ",
                   "a.nss,                          ",
                   "a.codven,                       ",
                   "a.coduni_n1,                    ",
                   "a.fentcons,                     ",
                   "a.salario_base_comis,           ",
                   "a.num_sm,                       ",
                   "a.monto_comision,               ",
                   "a.estado_comision,              ",
                   "a.tipo_solicitud,               ",
                   "a.fentcons                      ",
                   "FROM   com_comis_detalle a      ",
                   "WHERE ",cla_where CLIPPED        ,
                   " AND   a.nivel = 1              ",
                   "UNION ALL                       ",
                   "SELECT b.n_folio,               ",
                   "b.n_seguro,                     ",
                   "a.codven,                       ",
                   "a.coduni_n1,                    ",
                   "b.frecafor,                     ",
                   "0,                              ",
                   "0,                              ",
                   "0,                              ",
                   "b.status_interno,               ",
                   "b.tipo_solicitud,               ",
                   "b.fentcons                      ",
                   "FROM   com_comis_detalle a,     ",
                   "       afi_solicitud  b,        ",
                   "       tab_status_afi c         ",
                   "WHERE ",cla_where CLIPPED        , 
                   " AND   a.nivel  = 1             ",
                   "AND   a.codven =b.codven        ",
                   "AND   b.status_interno = c.estado_cod ",
                   "AND   b.status_interno < 100 ",
                   "AND   b.frecafor <= '",xfecha_corte2 CLIPPED,
                   "' GROUP BY 1,2,3,4,5,6,7,8,9,10,11 "

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



        ---Estados de las solicitudes
        IF nuevo.estado_comision < 100 THEN

           SELECT estado_desc
           INTO   vdescrip
           FROM   tab_status_afi
           WHERE  estado_cod = nuevo.estado_comision

           --PARA LA CAUSA DEL RECH O PENDI
           SELECT MAX(observacion)
           INTO   vcausa
           FROM   afi_rechaza_cert
           WHERE  n_seguro=nuevo.nss
           AND    f_rechazo=nuevo.fentcons_afi
           AND    n_folio  =nuevo.n_folio
           AND    tipo_solicitud = nuevo.tipo_solicitud
 

           IF nuevo.estado_comision = 40 OR
              nuevo.estado_comision = 42 OR
              nuevo.estado_comision = 45  THEN

              LET nuevo.estado_comision = 0  ---Rechazados
           ELSE
              LET nuevo.estado_comision = 1  ---Pendientes
           END IF
        END IF


       IF vdescrip IS NULL THEN
          LET vdescrip = ' '
       END IF

       IF vcausa IS NULL THEN
            LET vcausa = ' '
       END IF


      -- IF nuevo.codven IS NULL THEN
      --    EXIT FOREACH
      -- END IF

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
   ERROR "LISTADO 1 GENERADO...."
   SLEEP 2
   ERROR ""

   unload to G_LISTA
   select * from temp_listo order by 1,11 desc

   LET columnas = "COD PROMOTOR'|'NOM PROMOTOR'|'CENTRO COSTOS'|'FOLIO CLIEN'|'NSS CLIE'|',",
                  "NOM CLIEN'|'FEC CERTIF'|'SALAR BASE COMI'|'VSM'|'MONTO COMIS'|'ESTADO COMIS'|'DESCRIP EDO'|'DESCRIP CAUSA"

   LET comando ="cd ",g_param_dis.ruta_rescate CLIPPED,"; echo ",columnas CLIPPED,"> alguno2" CLIPPED
   RUN comando

   LET comando ="cd ",g_param_dis.ruta_rescate CLIPPED,"; cat ",G_LISTA CLIPPED,">> alguno2" CLIPPED,";mv alguno2 ",G_LISTA CLIPPED
   RUN comando
 

  LET impresion = "lp ",G_IMPRE
--  RUN impresion


  --**********************************************************
  --Se manda inprimir todos los promotores que solo tuvieron pendientes o
  --rechazados.
  --**********************************************************

  IF ban_solo_admon = 1 THEN

 -- ERROR "Buscando Informacion"
    LET ejecuta = "nohup time fglgo COMC013D.4gi ",xfecha_corte2," &"
    RUN ejecuta
  END IF

  END IF



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

      BEFORE GROUP OF lnuevo.estado_comision




      CASE lnuevo.estado_comision
           WHEN 200 
              PRINT COLUMN 060,'S/SAL EN REG INSER PAGO=0'
           WHEN 220 
              PRINT COLUMN 060,'CLIENTES CON MENOS DE 6 SALARIOS'
           WHEN 230 
              PRINT COLUMN 060,'CLIENTES CON IGUAL O MAS DE 6 SALARIOS'
           WHEN 260 
              PRINT COLUMN 060,'CLIENTES CON MENOS DE 6 SALARIOS CON RECAU.'
           WHEN 270 
              PRINT COLUMN 060,'CLIENTES CON IGUAL O MAS DE 6 SALARIOS CON REC.'
           WHEN 290 
               PRINT COLUMN 060,'CLIENTES CON PREMIO DE CONCURSO'
           WHEN 250 
               PRINT COLUMN 060,'CLIENTES CON RECAU. MENOR A 6 SALARIOS c/edo'
           WHEN 260 
               PRINT COLUMN 060,'CLIENTES CON RECAU. MENOR A 6 SALARIOS ins reg'
           WHEN 280 
               PRINT COLUMN 060,'CLIENTES CON RECAU. MAS DE  6 SALARIOS ins reg'
           WHEN 0 
               PRINT COLUMN 060,'CLIENTES RECHAZADOS'
           WHEN 1 
               PRINT COLUMN 060,'CLIENTES PENDIENTES'
           OTHERWISE
               PRINT COLUMN 060,'OTROS ESTADOS'
       END CASE

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

      IF lnuevo.estado_comision = 1 OR 
         lnuevo.estado_comision = 0 THEN
            PRINT COLUMN 130,vcausa_edo CLIPPED
      END IF


      INSERT INTO temp_listo
      VALUES(lnuevo.codven,lnuevo.nombre_pro,lnuevo.coduni_n1,lnuevo.n_folio,lnuevo.nss,lnuevo.nombre_cli,lnuevo.fentcons,lnuevo.salario_base_comis,lnuevo.num_sm,lnuevo.monto_comision,lnuevo.estado_comision,vdescrip_edo,vcausa_edo)


      AFTER GROUP OF lnuevo.estado_comision



      ---ojo
      LET subt_num_sm = subt_num_sm + GROUP AVG(lnuevo.num_sm)


      SKIP  1 LINES
      PRINT COLUMN 085,GROUP SUM(lnuevo.salario_base_comis) USING "#########&&.&&"," ",GROUP SUM(lnuevo.num_sm) USING "####&.&&","  ",GROUP SUM(lnuevo.monto_comision) USING "#########&&.&&",'          ',GROUP AVG(lnuevo.num_sm) USING "####&.&&"
      PRINT COLUMN 085,'    SUBT_SALA.  SUBT_VSM    SUBT_COMIS.        SUBT_CALIDAD'
      SKIP  2 LINES

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


FUNCTION ver_arreglo(xcodven2)
   DEFINE
      pat    CHAR(40),
      mat    char(40),
      nom    char(40),
      pos    SMALLINT,
      fhasta DATE,
      i      SMALLINT,
      xcodven2 CHAR(10),
      ban_agregados SMALLINT 

       LET total = 0
       LET pagada = 0
       LET registros = 0
       LET ban_agregados = 0

       LET cla_sel2="SELECT a.codven,a.coduni_n1,", 
                    "trim(b.nombres)||' '||trim(b.paterno)||' '||",
                    "trim(b.materno)",
                    "FROM   com_comis_detalle a,pro_mae_promotor b ",
                    "WHERE ",cla_where CLIPPED,
                    " AND    a.codven=b.cod_promotor ",
                    " GROUP BY 1,2,3 " CLIPPED,
                    " ORDER BY 1,2 " CLIPPED


       ERROR "Buscando Informacion"

       PREPARE claexe FROM cla_sel2
       DECLARE cursor_1111 CURSOR FOR claexe
       LET pos = 1
       FOREACH cursor_1111 INTO g_reg[pos].*


                 LET registros = registros  + 1
	         LET pos = pos + 1
                 IF pos >= 9000 THEN
                    ERROR "Sobrepaso la capacidad del arreglo"
                    EXIT FOREACH
                 END IF 
         END FOREACH

         DISPLAY registros TO scr_3.*

	 ERROR ""

	 CALL  SET_COUNT(pos-1)

	 IF (pos-1) >= 1 THEN
	    DISPLAY ARRAY g_reg TO scr_1.*

               ON KEY (CONTROL-I)
                  LET i = ARR_CURR()
                  CALL imprime(ban_agregados,g_reg[i].codven)
               ON KEY (CONTROL-F)
                  LET i = ARR_CURR()
	          CALL Busca_concurso(g_reg[i].codven,g_reg[i].coduni_n1)
               ON KEY (CONTROL-E)
                  LET i = ARR_CURR()
	          CALL Busca_pendientes(g_reg[i].codven)
               ON KEY (CONTROL-P)
                  LET i = ARR_CURR()
	          CALL Busca_afiliados(g_reg[i].codven,g_reg[i].coduni_n1)
	       ON KEY (CONTROL-B)
                  LET i = ARR_CURR()

	          CALL Busca_rechazos(g_reg[i].codven)
               ON KEY (INTERRUPT)
                  EXIT DISPLAY
	    END DISPLAY
	 ELSE

           LET ban_agregados = 1
           --********************************************************
           --Para cuando se trate de un promotor sin afiliados y sin
           --aportaciones pero si pendientes y rechazados
           --********************************************************
           LET cla_sell="SELECT a.cod_promotor,a.agenc_cod,",
                    "trim(a.nombres)||' '||trim(a.paterno)||' '||",
                    "trim(a.materno) ",
                    "FROM   pro_mae_promotor a ",
                    "WHERE  a.cod_promotor='",xcodven2,
                    "' GROUP BY 1,2,3 " CLIPPED,
                    " ORDER BY 1,2 " CLIPPED


           ERROR "Buscando Informacion"

           PREPARE claexel FROM cla_sell
           DECLARE cursor_1111l CURSOR FOR claexel
           LET pos = 1
           FOREACH cursor_1111l INTO g_reg[pos].*


                     LET registros = registros  + 1
                     LET pos = pos + 1
                     IF pos >= 9000 THEN
                        ERROR "Sobrepaso la capacidad del arreglo"
                        EXIT FOREACH
                     END IF
           END FOREACH

           DISPLAY registros TO scr_3.*

           ERROR ""

           CALL  SET_COUNT(pos-1)

           IF (pos-1) >= 1 THEN
              DISPLAY ARRAY g_reg TO scr_1.*

                 ON KEY (CONTROL-I)
                  LET i = ARR_CURR()
                    CALL imprime(ban_agregados,g_reg[i].codven)
                 ON KEY (CONTROL-F)
                    LET i = ARR_CURR()
                    CALL Busca_concurso(g_reg[i].codven,g_reg[i].coduni_n1)
                 ON KEY (CONTROL-E)
                    LET i = ARR_CURR()
                    CALL Busca_pendientes(g_reg[i].codven)
                 ON KEY (CONTROL-P)
                    LET i = ARR_CURR()
                    CALL Busca_afiliados(g_reg[i].codven,g_reg[i].coduni_n1)
                 ON KEY (CONTROL-B)
                    LET i = ARR_CURR()
              --  display "g_reg[i].codven:          ....",g_reg[i].codven
              --  sleep 10 
                    CALL Busca_rechazos(g_reg[i].codven)
                 ON KEY (INTERRUPT)
                    EXIT DISPLAY
              END DISPLAY
          ELSE
            ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""

              
          END IF
      END IF  --del else


      call Inicializa()
      CLEAR SCREEN
END FUNCTION

FUNCTION Obtiene_solicitudes()

         --Obtiene las solicitudes pendientes
         LET g_grep_00="SELECT a.n_folio,",
                       "a.n_seguro,",
                       "a.tipo_solicitud,",
                       "a.frecafor,",
                       "a.salario_base_comis,",
                       "a.status_interno, ",
                       "b.estado_desc ",
                "FROM   afi_solicitud a,",
                "       tab_status_afi b ",
                "WHERE  a.status_interno = b.estado_cod ",
               " AND    status_interno not in (40,42,45) ",
               " AND    status_interno < 100 ",
               " AND    a.frecafor <= '",xfecha_corte2,
               "' AND a.codven = ? ",
               " ORDER BY 4,1,2" CLIPPED

        --Obtiene el nombre de las soli. pendientes y rechazadas 
        LET g_grep_03="SELECT n_seguro,paterno,materno,nombres ",
                      "FROM   afi_solicitud ",
                      "WHERE  n_folio= ? ",
                      " AND    tipo_solicitud = ? " CLIPPED


END FUNCTION

FUNCTION Busca_concurso(vcodven3,vcoduni_n13)

  DEFINE g_reg00 ARRAY[1000] OF RECORD
         codven             LIKE com_comis_detalle.codven,
         coduni_n1          LIKE com_comis_detalle.coduni_n1,
         fecha_corte        LIKE com_comis_detalle.fecha_corte,
         vtot_afil          INTEGER,
         vnum_sm            LIKE com_comis_detalle.monto_comision,
         vprom_sm           LIKE com_comis_detalle.monto_comision,
         vmonto_comi        LIKE com_comis_detalle.monto_comision,
         vedo               LIKE com_comis_detalle.estado_comision
      END RECORD

  DEFINE cla_sel2   CHAR(1500) 
  DEFINE vcodven3 LIKE com_comis_detalle.codven,
         vcoduni_n13 LIKE com_comis_detalle.coduni_n1 
  DEFINE pos4        SMALLINT
  DEFINE total4     DECIMAL(12,2) 
  DEFINE registros4  INTEGER

  OPEN WINDOW ventana_10 AT 10,2 WITH FORM "COMC0136" ATTRIBUTE( BORDER)
  DISPLAY "             CONSULTA  DE  PRODUCCION  PREMIOS                                 " AT 2,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                     [Enter] Nombre Cliente    " AT 04,1 ATTRIBUTE(REVERSE)


  DISPLAY "                                                                                   " AT 11,1 ATTRIBUTE(REVERSE)


   LET total4 = 0
   LET registros4 = 0
   LET cla_sel2="SELECT codven,",              --codven
                       "coduni_n1,",           --coduni_n1
                       "fecha_corte,",         --fecha_corte
                       "porcent_comision,",    --vtot_afil
                       "salario_base_comis,",  --vsum_sm
                       "num_sm,",              --vprom_sm
                       "monto_comision,",      --vmonto_comi
                       "estado_comision ",       --vedo
                "FROM   com_comis_detalle ",
                "WHERE fecha_corte='",xfecha_corte2 CLIPPED, 
                "' AND codven='",vcodven3 CLIPPED,
                "' AND estado_comision=170 ",
                "  AND coduni_n1='",vcoduni_n13 CLIPPED,
                "' ORDER BY 1,3 " CLIPPED

    ERROR "Buscando Informacion"

         PREPARE claexe4 FROM cla_sel2
         DECLARE cursor_111 CURSOR FOR claexe4
         LET pos4 = 1
         FOREACH cursor_111 INTO g_reg00[pos4].*

                 LET total4 = total4 + g_reg00[pos4].vmonto_comi
                 LET registros4 = registros4  + 1
                 LET pos4 = pos4 + 1
                 IF pos4 >= 9000 THEN
                    ERROR "Sobrepaso la capacidad del arreglo"
                    EXIT FOREACH
                 END IF
         END FOREACH

         DISPLAY registros4,total4 TO scr_3.*

         ERROR ""

         CALL  SET_COUNT(pos4-1)

         IF (pos4-1) >= 1 THEN
            DISPLAY ARRAY g_reg00 TO scr_1.*
            END DISPLAY
         ELSE
            ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
         END IF
       CLOSE WINDOW ventana_10
END FUNCTION


FUNCTION Busca_rechazos(vcodven3)

   DEFINE g_reg8 ARRAY[1000] OF RECORD
         n_folio            LIKE afi_solicitud.n_folio,
         n_seguro           LIKE afi_solicitud.n_seguro,
         tipo_solicitud     LIKE afi_solicitud.tipo_solicitud,
         frecafor           LIKE afi_solicitud.fentcons,
         salario_base_comis LIKE afi_solicitud.salario_base_comis,
         status_interno     LIKE afi_solicitud.status_interno,
         descripcion        LIKE afi_status_int.estado_desc
      END RECORD

   DEFINE registros22 INTEGER,
	  pos        SMALLINT,
	  parte1     CHAR(26),
	  parte2     CHAR(10),
	  parte3     CHAR(100),
          i          SMALLINT,
          vcodven3   LIKE com_comis_detalle.codven,
          cla_sel4   CHAR(1500)

-- DISPLAY "                                        " AT 8,41

   OPEN WINDOW ventana_22 AT 10,2 WITH FORM "COMC0034" 

   DISPLAY "                          ESTADO SOLICITUDES RECHAZADAS                     " AT 2,1 ATTRIBUTE(REVERSE) 


   DISPLAY "                                                     [Enter] Nombre Cliente    " AT 04,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                                               " AT 11,1 ATTRIBUTE(REVERSE)

   LET registros22 = 0
   LET cla_sel4="SELECT a.n_folio,",
                       "a.n_seguro,",
		       "a.tipo_solicitud,",
                       "a.frecafor,",
                       "a.salario_base_comis,",
                       "a.status_interno, ",
		       "b.estado_desc ",
                "FROM   afi_solicitud a,",
		"       tab_status_afi b ",
                "WHERE  a.status_interno = b.estado_cod ",
               " AND    status_interno in (40,42,45) ",
--             " AND    status_interno < 100 ",
               " AND    a.frecafor <= '",xfecha_corte2,
               "' AND a.codven = '",vcodven3 CLIPPED,
     	       "' ORDER BY 4,1,2" CLIPPED

         ERROR "Buscando Informacion"

         PREPARE claexe_pend4 FROM cla_sel4

         DECLARE cursor_pend4 CURSOR FOR claexe_pend4
	 LET pos = 1
         FOREACH cursor_pend4 INTO g_reg8[pos].*
		 LET registros22 = registros22 + 1
	         LET pos = pos + 1
                 IF pos >= 9000 THEN
                    ERROR "Sobrepaso la capacidad del arreglo"
                    EXIT FOREACH
                 END IF 
         END FOREACH

         DISPLAY registros22 TO scr_1.*

	 ERROR ""

	 CALL  SET_COUNT(pos-1)

	 IF (pos-1) >= 1 THEN
	    DISPLAY ARRAY g_reg8 TO scr_0.*
               ON KEY (CONTROL-M)
                  LET i = ARR_CURR()
              CALL Mostrar_nombre_solicitud(g_reg8[i].n_folio,g_reg8[i].tipo_solicitud)

               ON KEY (INTERRUPT)
                  EXIT DISPLAY
	    END DISPLAY
	 ELSE
	    ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
	 END IF
	 CLOSE WINDOW ventana_22

END FUNCTION






FUNCTION Busca_pendientes(vcodven2)

   DEFINE g_reg8 ARRAY[1000] OF RECORD
         n_folio            LIKE afi_solicitud.n_folio,
         n_seguro           LIKE afi_solicitud.n_seguro,
         tipo_solicitud     LIKE afi_solicitud.tipo_solicitud,
         frecafor           LIKE afi_solicitud.fentcons,
         salario_base_comis LIKE afi_solicitud.salario_base_comis,
         status_interno     LIKE afi_solicitud.status_interno,
         descripcion        LIKE afi_status_int.estado_desc
      END RECORD

   DEFINE registros2 INTEGER,
	  pos        SMALLINT,
	  parte1     CHAR(26),
	  parte2     CHAR(10),
	  parte3     CHAR(100),
          i          SMALLINT,
          vcodven2   LIKE com_comis_detalle.codven,
          cla_sel3   CHAR(1500)

-- DISPLAY "                                        " AT 8,41

   OPEN WINDOW ventana_2 AT 10,2 WITH FORM "COMC0033" 

   DISPLAY "                      ESTADO SOLICITUDES PENDIENTES                               " AT 2,1 ATTRIBUTE(REVERSE) 


   DISPLAY "                                                     [Enter] Nombre Cliente    " AT 04,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                                               " AT 11,1 ATTRIBUTE(REVERSE)

   LET registros2 = 0

         CALL Obtiene_solicitudes()

         PREPARE  query_F_010_1    FROM g_grep_00

         DECLARE cur_ver CURSOR FOR query_F_010_1
         OPEN    cur_ver USING vcodven2
	 LET pos = 1
         FOREACH cur_ver INTO g_reg8[pos].*
                 LET registros2 = registros2 + 1
                 LET pos = pos + 1
                 IF pos >= 9000 THEN
                    ERROR "Sobrepaso la capacidad del arreglo"
                    EXIT FOREACH
                 END IF
         END FOREACH



         DISPLAY registros2 TO scr_1.*

	 ERROR ""

	 CALL  SET_COUNT(pos-1)

	 IF (pos-1) >= 1 THEN
	    DISPLAY ARRAY g_reg8 TO scr_0.*
               ON KEY (CONTROL-M)
                  LET i = ARR_CURR()
                  CALL Mostrar_nombre_solicitud(g_reg8[i].n_folio,g_reg8[i].tipo_solicitud)
               ON KEY (INTERRUPT)
                  EXIT DISPLAY
	    END DISPLAY
	 ELSE
	    ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
	 END IF
	 CLOSE WINDOW ventana_2
END FUNCTION


FUNCTION Busca_afiliados(vcodven,vcoduni_n1)

   DEFINE registros2 INTEGER
   DEFINE reg1       INTEGER
   DEFINE reg2       INTEGER,
          monto1     LIKE com_comis_detalle.monto_comision,
          monto2     LIKE com_comis_detalle.monto_comision,
          mto_comi   LIKE com_comis_detalle.monto_comision,
	  pos        SMALLINT,
	  parte1     CHAR(26),
	  parte2     CHAR(10),
	  parte3     CHAR(100),
          i          SMALLINT,
          vcodven    CHAR(10),
          vcoduni_n1 LIKE com_comis_detalle.coduni_n1,
          vcod_tipo  LIKE com_comis_detalle.cod_tipo_prom, 
          vfec_corte LIKE com_comis_detalle.fecha_corte,
          vestado    LIKE com_comis_detalle.estado_comision
   DEFINE calidad1   LIKE com_comis_detalle.num_sm
   DEFINE calidad2   LIKE com_comis_detalle.num_sm   



   OPEN WINDOW ventana_2 AT 9,2 WITH FORM "COMC0132" 

   DISPLAY "                         CLIENTES DEL PROMOTOR                                " AT 2,1 ATTRIBUTE(REVERSE) 


   DISPLAY "             [Ctrl-C] Salir                          [Enter] Nombre Cliente    " AT 04,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                                               " AT 10,1 ATTRIBUTE(REVERSE)


   LET registros2 = 0
   LET reg1 = 0
   LET reg2 = 0
   LET monto1      = 0
   LET monto2      = 0
   LET mto_comi    = 0

   LET cla_sel2="SELECT n_folio,",
                       "nss,",
		       "tipo_solicitud,",
                       "fentcons,",
                       "fecha_corte,",
                       "salario_base_comis, ",
		       "num_sm, ",
		       "monto_comision, ",
		       "estado_comision ",
                "FROM   com_comis_detalle ",
                "WHERE codven = '",vcodven CLIPPED, 
                "' AND coduni_n1 = '",vcoduni_n1 CLIPPED,
                "' AND fecha_corte = '",xfecha_corte2 CLIPPED,
            --  "' AND estado_comision in (220,230) ",  ---220 
     	        "'ORDER BY estado_comision" CLIPPED

         ERROR "Buscando Informacion"

         PREPARE claexe_pend FROM cla_sel2
         DECLARE cursor_pend2 CURSOR FOR claexe_pend

	 LET pos = 1
	 FOREACH cursor_pend2 INTO g_reg3[pos].*
		 LET registros2 = registros2 + 1

       IF g_reg3[pos].estado_comision = 220 THEN
		    LET monto1 = monto1 + g_reg3[pos].num_sm
		    LET reg1 = reg1 + 1
       END IF

       IF g_reg3[pos].estado_comision = 230 THEN
		    LET monto2 = monto2 + g_reg3[pos].num_sm
		    LET reg2 = reg2 + 1
       END IF

       --Para el monto de la comision
       LET mto_comi = mto_comi + g_reg3[pos].monto_comision
 
       LET pos = pos + 1
       IF pos >= 9000 THEN
             ERROR "Sobrepaso la capacidad del arreglo"
             EXIT FOREACH
       END IF 
    END FOREACH

    IF reg1 = 0 THEN
       LET calidad1 = 0
    ELSE
       LET calidad1 = monto1 / reg1 
    END IF
    IF reg2 = 0 THEN
       LET calidad2 = 0
    ELSE
       LET calidad2 = monto2 / reg2 
    END IF

    IF calidad1 IS NULL THEN LET calidad1 = 0 END IF
    IF calidad2 IS NULL THEN LET calidad2 = 0 END IF

    DISPLAY registros2 TO scr_1.registros2
    DISPLAY monto1   TO scr_1.monto1
    DISPLAY calidad1 TO scr_1.calidad1
    DISPLAY monto2   TO scr_1.monto2
    DISPLAY calidad2 TO scr_1.calidad2
    DISPLAY mto_comi TO scr_1.mto_comi
    

	 ERROR ""

	 CALL  SET_COUNT(pos-1)

	 IF (pos-1) >= 1 THEN
	    DISPLAY ARRAY g_reg3 TO scr_0.*
               ON KEY (CONTROL-M)
                  LET i = ARR_CURR()
                  CALL Mostrar_nombre2(g_reg3[i].nss)
               ON KEY (INTERRUPT)
                  EXIT DISPLAY
	    END DISPLAY
	 ELSE
	    ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
	 END IF
----         CLEAR SCREEN

         
	 CLOSE WINDOW ventana_2
END FUNCTION

------------------------------------------------------

FUNCTION Busca_estado(vestado)
  DEFINE vestado    LIKE com_comis_detalle.estado_comision
  DEFINE pos        INTEGER

  DEFINE l_record   ARRAY[300] OF RECORD
                codigo         SMALLINT,
                descripcion    CHAR(70)
         END RECORD
         DECLARE cursor_1 CURSOR FOR
         SELECT codigo,descripcion FROM tab_edo_comision
         WHERE codigo  = vestado 
         ORDER BY 1
         LET pos = 1
         FOREACH cursor_1 INTO l_record[pos].*
                 LET pos = pos + 1
         END FOREACH
         IF (pos-1) >= 1 THEN
            CALL  SET_COUNT(pos-1)
         -- OPEN WINDOW ventana_2 AT 5,10 WITH FORM "COMC0135" ATTRIBUTE( BORDER)
                                  -- r,c
            OPEN WINDOW ventana_2 AT 5,3 WITH FORM "COMC0135" ATTRIBUTE( BORDER)

 
            DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(REVERSE,yellow)
            DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,YELLOW)
            DISPLAY "                    Descripcion de Estado                                        " AT 3,1 ATTRIBUTE(REVERSE,green)
 
            DISPLAY ARRAY l_record TO scr_9.*
                    ON KEY (INTERRUPT)
                       EXIT DISPLAY
            END DISPLAY
            CLOSE WINDOW ventana_2
         ELSE
            ERROR "ARCHIVO DE CANAL.... VACIO"
         END IF


END FUNCTION


----------------------------------------------------------------------

FUNCTION Mostrar_nombre2(vnss)
   DEFINE vnss    CHAR(11),
          pos     INTEGER

   OPEN WINDOW ventana001 AT 09,2 WITH FORM "COMC0134" 

   DISPLAY "                             NOMBRE DEL CLIENTE                                " AT 2,1 ATTRIBUTE(REVERSE) 

   DISPLAY "                                                              [Ctrl-C] Salir   " AT 04,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                                               " AT 11,1 ATTRIBUTE(REVERSE)


   LET cla_sel2="SELECT a.n_seguro,",
                       "a.paterno,",
                       "a.materno,",
                       "a.nombres ",
                "FROM   afi_mae_afiliado a ",
                "WHERE  a.n_seguro  = '",vnss CLIPPED,
                "'"

         ERROR "Buscando Informacion"

         PREPARE claexe_cte2 FROM cla_sel2
         DECLARE cursor_cte2 CURSOR FOR claexe_cte2
	 LET pos = 1
	 FOREACH cursor_cte2 INTO g_reg4[pos].*
            LET g_reg5[pos].seguro         = g_reg4[pos].seguro
     
            LET g_reg5[pos].nombre = g_reg4[pos].paterno CLIPPED," ",
                                     g_reg4[pos].materno CLIPPED," ",
                                     g_reg4[pos].nombres CLIPPED
	         LET pos = pos + 1
                 IF pos >= 900 THEN
                    ERROR "Sobrepaso la capacidad del arreglo"
                    EXIT FOREACH
                 END IF 
         END FOREACH

	 ERROR ""

	 CALL  SET_COUNT(pos-1)

	 IF (pos-1) >= 1 THEN
	    DISPLAY ARRAY g_reg5 TO scr_0.*
               ON KEY (INTERRUPT)
                  EXIT DISPLAY
	    END DISPLAY
	 ELSE
	    ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
	 END IF
----         CLEAR SCREEN
	 CLOSE WINDOW ventana001
END FUNCTION
----------------------------------------------------------------------
FUNCTION Mostrar_nombre_solicitud(vn_folio,vtipo_solicitud)
   DEFINE vn_folio    INTEGER
   DEFINE vtipo_solicitud  SMALLINT
   DEFINE vnss    CHAR(11)
   DEFINE pos     INTEGER

   OPEN WINDOW ventana001 AT 09,2 WITH FORM "COMC0134" 

   DISPLAY "                             NOMBRE DEL CLIENTE                                " AT 2,1 ATTRIBUTE(REVERSE) 

   DISPLAY "                                                              [Ctrl-C] Salir   " AT 04,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                                               " AT 11,1 ATTRIBUTE(REVERSE)


CALL Obtiene_solicitudes()               ---ojito
PREPARE claexe_cte4    FROM g_grep_03    
DECLARE cursor_cte4 CURSOR FOR claexe_cte4
OPEN    cursor_cte4 USING vn_folio,vtipo_solicitud 
LET pos = 1
FOREACH cursor_cte4 INTO g_reg4[pos].*

            LET g_reg5[pos].seguro         = g_reg4[pos].seguro
     
            LET g_reg5[pos].nombre = g_reg4[pos].paterno CLIPPED," ",
                                     g_reg4[pos].materno CLIPPED," ",
                                     g_reg4[pos].nombres CLIPPED
	         LET pos = pos + 1
                 IF pos >= 900 THEN
                    ERROR "Sobrepaso la capacidad del arreglo"
                    EXIT FOREACH
                 END IF 
         END FOREACH

	 ERROR ""

	 CALL  SET_COUNT(pos-1)

	 IF (pos-1) >= 1 THEN
	    DISPLAY ARRAY g_reg5 TO scr_0.*
               ON KEY (INTERRUPT)
                  EXIT DISPLAY
	    END DISPLAY
	 ELSE
	    ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
	 END IF
	 CLOSE WINDOW ventana001
END FUNCTION
----------------------------------------------------------------------
