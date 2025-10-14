#s############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                           #
#Propietario       => E.F.P.       					                             #
#Programa          => GENERACION DE ESTADISTICA APORT.VOL.CONSAR 19-5        #
#                     (formato 25)                                           # 
#Fecha             => 19 junio 2006                                          #
#Por               => JESUS DAVID YAÑEZ MORENO                               #
##############################################################################
DATABASE safre_af
GLOBALS

	DEFINE g_cta RECORD 
		 nss               CHAR(011) ,
		 t_aport    	     CHAR(002) ,
		 t_opera    	     CHAR(002) , 
		 t_depos           CHAR(002) ,
		 f_recep     	     DATE      ,
		 f_abono           DATE      ,
		 f_inver   	       DATE      ,
		 t_sief            CHAR(003) ,
		 cve_sief          CHAR(003) ,
		 subt_sief         CHAR(003) ,
		 curp              CHAR(018) ,
		 rfc               CHAR(013) ,
		 nombre            CHAR(020) ,
		 paterno           CHAR(020) ,
		 materno           CHAR(020) ,
		 f_nacim  	       DATE      ,
		 m_aport  	       DEC(16,2) ,
		 m_accion          DEC(16,6) ,
		 folio             INTEGER   ,
		 t_afil		         SMALLINT  
	END RECORD

	DEFINE gg  RECORD
		 impt_aportacion   DECIMAL(16,6)
	END RECORD
		
	DEFINE g_tabafo      RECORD LIKE tab_afore_local.*

	DEFINE g_paramgrales   	RECORD LIKE seg_modulo.*		
		
	DEFINE 
	   pso                CHAR(500)  ,
		 txt                CHAR(1500) ,
		 x_busca            CHAR(100)  ,
		 nom_dis_cuenta     CHAR(020)  ,
		 G_LISTA	       	  CHAR(500)  ,
		 G_LISTAP	       	  CHAR(500)  ,
		 G_LISTAr	       	  CHAR(500)  ,
		 aux_pausa       	  CHAR(1)    , 		 
		 usuario            CHAR(010)  ,
		 vecho              CHAR(400)  ,
		 enter              CHAR(001)  

	DEFINE 
	   HOY                ,
	   fecini             , 
	   fecfin             ,	
	   fecha_retro1       DATE

	DEFINE 
	   mont1              DECIMAL(16,6),
		 mont2              DECIMAL(16,6),
		 mont3              DECIMAL(16,6)
		 
	DEFINE 
	   num_regs           ,
		 cuantos1           ,
		 cuantos2           ,
		 cuantos3           INTEGER   
		 
	DEFINE
	   ban                ,
	   bn                 ,
	   sw                 SMALLINT
	   
END GLOBALS


MAIN
	
   OPTIONS 
	    INPUT WRAP, 
	    PROMPT LINE LAST, 
	    ACCEPT KEY CONTROL-I
	   
	 DEFER INTERRUPT
	
 	 CALL STARTLOG("ESTM011.log")
	
	 LET HOY = TODAY
   
	 OPEN WINDOW ventana_1 AT 2, 2 WITH FORM "ESTM0111" ATTRIBUTE( BORDER )
	 DISPLAY "ESTM011       ESTADISTICA DE APORTACIONES VOLUNTARIAS                                   " AT 3, 1 ATTRIBUTE( REVERSE )
   DISPLAY " < ESC > Procesar                                          < Ctrl-C > Salir " AT 1,1 ATTRIBUTE( REVERSE )
	 DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

	 CALL init()
	
	 LET sw = 0
	 
   LET int_flag = FALSE 

   INPUT BY NAME fecini, fecfin
      AFTER FIELD fecini
         DISPLAY "                                       " AT 19, 1
         IF fecini IS NULL THEN
            DISPLAY  "LA FECHA INICIAL NO DEBE SER NULA" AT 19, 1
            NEXT FIELD fecini
         END IF

      AFTER FIELD fecfin
         DISPLAY "                                       " AT 19, 1
         IF fecfin IS NULL THEN
            DISPLAY  "LA FECHA FINAL NO DEBE SER NULA" AT 19, 1
            NEXT FIELD fecfin
         END IF

      ON KEY( ESC )
         DISPLAY "                                       " AT 19, 1
         IF fecini IS NULL THEN
            DISPLAY  "LA FECHA INICIAL NO DEBE SER NULA" AT 19, 1
            NEXT FIELD fecini
         END IF

         IF fecfin IS NULL THEN
            DISPLAY  "LA FECHA FINAL NO DEBE SER NULA" AT 19, 1
            NEXT FIELD fecfin
         END IF

         IF( fecini > fecfin ) THEN
            ERROR "LA FECHA INICIAL NO DEBE SER MAYOR A LA FECHA FINAL"
            NEXT FIELD fecini
         END IF

         EXIT INPUT

      ON KEY ( INTERRUPT, CONTROL-C )
      	 PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
      	 FOR CHAR enter
         LET ban = 1
         EXIT INPUT      	 
   END INPUT

   IF ban = 1 THEN
      CLOSE WINDOW ventana_1
   END IF

   PROMPT " DESEA GENERAR EL ARCHIVO S/N ? "
   FOR enter

   IF enter MATCHES "[Ss]" THEN
      
       CALL query()
       
       DISPLAY " ESTADISTICA GENERADA EN : "  AT 17, 2

       LET G_LISTA = G_LISTA[11,500] CLIPPED

       DISPLAY G_LISTA AT 18, 2

      PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR "
      FOR enter
      LET ban = 1       
       
   ELSE
      PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
      FOR enter
      LET ban = 1
   END IF

   IF ban = 1 THEN
     CLOSE WINDOW ventana_1
   END IF

END MAIN


FUNCTION init()
#--------------

	LET bn = 0 
	LET num_regs = 0

	SELECT user 
	INTO   usuario 
	FROM   tab_afore_local
	GROUP BY 1

	SELECT * 
	INTO   g_paramgrales.* 
	FROM   seg_modulo
	WHERE  modulo_cod = "est"

	SELECT codigo_afore 
	INTO   g_tabafo.codigo_afore
	FROM   tab_afore_local

	CREATE TEMP TABLE est_ascii(simbolo char(01))

	INSERT into est_ascii VALUES ("[")
	INSERT INTO est_ascii VALUES ('"')
	INSERT INTO est_ascii VALUES ("]")
	INSERT INTO est_ascii VALUES ("#")
	INSERT INTO est_ascii VALUES ("$")
	INSERT INTO est_ascii VALUES ("%")
	INSERT INTO est_ascii VALUES ("&")
	INSERT INTO est_ascii VALUES ("=") 
	INSERT INTO est_ascii VALUES ("/")
	INSERT INTO est_ascii VALUES ("?")
	INSERT INTO est_ascii VALUES ("-")
	INSERT INTO est_ascii VALUES ("'")
	INSERT INTO est_ascii VALUES ("(")
	INSERT INTO est_ascii VALUES (")")
	INSERT INTO est_ascii VALUES ("^")
	INSERT INTO est_ascii VALUES ("!") 
	INSERT INTO est_ascii VALUES ("~")
	INSERT INTO est_ascii VALUES ("_")
	INSERT INTO est_ascii VALUES (".")
	INSERT INTO est_ascii VALUES (":")
	INSERT INTO est_ascii VALUES (",")
	INSERT INTO est_ascii VALUES (";")
	INSERT INTO est_ascii VALUES ("<")
	INSERT INTO est_ascii VALUES (">") 
	INSERT INTO est_ascii VALUES ("@")
	INSERT INTO est_ascii VALUES ("|")
	INSERT INTO est_ascii VALUES ("{")
	INSERT INTO est_ascii VALUES ("}")
	INSERT INTO est_ascii VALUES ("+")
	INSERT INTO est_ascii VALUES ("*")
	INSERT INTO est_ascii VALUES ("`")
	INSERT INTO est_ascii VALUES ("¿") 
	INSERT INTO est_ascii VALUES ("¡")
	INSERT INTO est_ascii VALUES ("Ä")
	INSERT INTO est_ascii VALUES ("É")
	INSERT INTO est_ascii VALUES ("Í")
	INSERT INTO est_ascii VALUES ("Ó")
	INSERT INTO est_ascii VALUES ("Ú")
	INSERT INTO est_ascii VALUES ("¨")
	INSERT INTO est_ascii VALUES ("Ä") 
	INSERT INTO est_ascii VALUES ("Ë")
	INSERT INTO est_ascii VALUES ("Ï")
	INSERT INTO est_ascii VALUES ("Ö")
	INSERT INTO est_ascii VALUES ("Ö")
	INSERT INTO est_ascii VALUES ("Ü")
	INSERT INTO est_ascii VALUES ("á")
	INSERT INTO est_ascii VALUES ("é")
	INSERT INTO est_ascii VALUES ("í") 
	INSERT INTO est_ascii VALUES ("ó")
	INSERT INTO est_ascii VALUES ("ú")
	INSERT INTO est_ascii VALUES ("ä")
	INSERT INTO est_ascii VALUES ("ë")
	INSERT INTO est_ascii VALUES ("ï")
	INSERT INTO est_ascii VALUES ("ö")
	INSERT INTO est_ascii VALUES ("ü")
	INSERT INTO est_ascii VALUES ("´") 
	INSERT INTO est_ascii VALUES ("Á")
	INSERT INTO est_ascii VALUES ("Ð")
END FUNCTION


FUNCTION query()
#---------------
   
   DEFINE 
		  g_qry1 	CHAR(600)

	 LET g_qry1 = ' SELECT UNIQUE  ',
	 	            ' "01" , ',
	 	            ' CASE WHEN b.forma_pago = 1 THEN "00" ',
	 	            '      ELSE "01" ',
	 	            ' END CASE , ', 
	 	            ' MDY(b.fecha[1,2],b.fecha[3,4],b.fecha[5,8]) , ',
	 	            ' MDY(b.fecha[1,2],b.fecha[3,4],b.fecha[5,8])   ',
	 	            ' FROM int_det_voluntaria b ',
	 	            ' WHERE  b.folio   = ? ',   # reg_cta.folio
	 	            ' AND    b.nss     = ? ',   # reg_cta.nss
	 	            ' AND    b.monto_neto  = ? ',   # reg_cta.m_aport
	 	            ' AND    resul_operacion = "01" ',  # procedentes
	 	            ' AND    fecha_liquida = ? '  # fecha_liquidacion

 	 PREPARE qry1 FROM g_qry1 
	 DECLARE cur_qry1 CURSOR FOR qry1

	 LET G_LISTAP = g_paramgrales.ruta_envio CLIPPED, "/",
		              HOY USING "yyyymmdd" CLIPPED, "_AF_",
		              g_tabafo.codigo_afore USING"&&&","_000", ".0516p" CLIPPED

	 LET G_LISTA = g_paramgrales.ruta_envio CLIPPED, "/",
		             HOY USING "yyyymmdd" CLIPPED, "_AF_",
		             g_tabafo.codigo_afore USING"&&&","_000", ".0516" CLIPPED

	 START REPORT listado TO G_LISTAP


	 DECLARE cur_1 CURSOR FOR 
	 SELECT a.nombre_tabla 
	 FROM   taa_cd_tab_cuenta a
	 
	 FOREACH cur_1 INTO nom_dis_cuenta 
	
			LET txt = ' SELECT a.nss                          , ',     #nss
			          '  CASE WHEN a.subcuenta = 10 THEN "05"   ',     #t_aport
			          '   WHEN a.subcuenta = 12 THEN "06"       ',     
			          '   WHEN a.subcuenta = 15 THEN "07"       ',
			          '   WHEN a.subcuenta = 16 THEN "07"       ',#MOD 210307 APOR_LARGO_VENT
			          '  END CASE         , ',
			          '  ""             , ',                           #t_opera
			          '  ""             , ',                           #t_depos
			          '  ""               , ',                         #f_recep
			          '  ""               , ',                         #f_abono
			          '  a.fecha_conversion                   , ',     #f_inver
			          '  CASE WHEN a.siefore = 1 THEN "002"     ',     #t_sief
			          '   WHEN a.siefore = 2 THEN "002"         ',     
			          '   WHEN a.siefore = 3 THEN "002"         ',     -- Antes 017
			          '   WHEN a.siefore = 4 THEN "002"         ',     
			          '   WHEN a.siefore = 5 THEN "002"         ',     
			          '  END CASE           , ',
			          '  " "                , ',                       #cve_sief
			          '  CASE WHEN a.siefore = 1 THEN "001"  ',        #subt_sief
			          '   WHEN a.siefore = 2 THEN "002"      ',
			          '   WHEN a.siefore = 3 THEN "003"      ',
			          '   WHEN a.siefore = 4 THEN "004"      ',
			          '   WHEN a.siefore = 5 THEN "005"      ',
			          '  END CASE    , ',
			          '  c.n_unico   , ',                              #curp
			          '  c.n_rfc     , ',                              #rfc
			          '  c.nombres   , ',                              #nombre
			          '  c.paterno   , ',                              #paterno
			          '  c.materno   , ',                              #materno
			          '  c.fena      , ',                              #f_nacim
			          '  a.monto_en_pesos  , ',                        #m_aport
			          '  a.monto_en_acciones,    ',                    #m_accion
			          '  a.folio                ',                     #folio dis_cuenta
			          ' FROM  ',nom_dis_cuenta CLIPPED,' a, ',
			          '   afi_mae_afiliado   c     ',
			          ' WHERE fecha_conversion BETWEEN "', fecini, '" AND "', fecfin, '" ',
			          ' AND   a.tipo_movimiento IN (1,123)       ', #CPL-1536 Se agrega el tipo mov 123-Aport Vol Redes Comerciales
			          ' AND   a.subcuenta   IN ( 10, 12, 15, 16 ) ',#MOD 210307 APOR_LARGO_VNT16
			          ' AND   a.nss          = c.n_seguro   ',
			          ' AND   a.id_aportante MATCHES "VE*"  '
			          
      PREPARE qry FROM txt
      DECLARE cur_2 CURSOR FOR qry 
      	
      FOREACH cur_2 INTO  g_cta.*
      	
			   LET g_cta.nombre = formato_nombre(g_cta.nombre) 
			   LET g_cta.paterno= formato_nombre(g_cta.paterno) 
			   LET g_cta.materno= formato_nombre(g_cta.materno) 
         
			   IF g_cta.f_inver < '01/14/2005' THEN 
			   	LET g_cta.subt_sief = "001"
			   END IF
         
			   CASE g_cta.t_sief 
			   	WHEN "002" 
			   		LET g_cta.cve_sief = g_tabafo.codigo_afore
         
			   	WHEN "017" 
			   		LET g_cta.cve_sief = g_tabafo.codigo_afore + 100
			   END CASE
         
			   FOREACH cur_qry1 USING g_cta.folio, g_cta.nss, g_cta.m_aport, g_cta.f_inver
			   INTO g_cta.t_opera, g_cta.t_depos, g_cta.f_recep, g_cta.f_abono 
			   
			      EXIT FOREACH
		     END FOREACH
      
			   IF( g_cta.f_recep IS NULL OR g_cta.f_recep = ""   ) THEN
			   	 LET g_cta.t_opera = "01"
			   	 LET g_cta.t_depos  = "00"
			   	 LET g_cta.f_recep = 
			   	 habil_anterior(g_cta.f_inver) 
			   	 LET g_cta.f_abono     = g_cta.f_recep
			   END IF

			   LET g_cta.f_abono = habil_anterior(g_cta.f_inver)
			   LET g_cta.t_afil = DekeTipoEsElNSS( g_cta.nss )
			   LET num_regs = num_regs + 1
			   
			   OUTPUT TO REPORT listado( '301' )
			
		  END FOREACH
	 END FOREACH

	 CALL ProcesaRetiros()   #* Agregado 25/08/08 *

   IF num_regs = 0 THEN 
		  LET num_regs = 1
		  LET bn       = 1 
		  OUTPUT TO REPORT listado( '301' )
	 END IF 

	 FINISH REPORT listado
	--FINISH REPORT listador
	
	 IF num_regs > 1 THEN
		 LET num_regs = num_regs + 1
		 LET pso = "sed 's/245000001/245", num_regs USING"&&&&&&", "/g' ",
			         G_LISTAP CLIPPED, " > ", G_LISTA
	 ELSE
		  IF bn = 1 THEN
			   LET pso = "grep '^000' ", G_LISTAP CLIPPED, " > ", G_LISTA CLIPPED
			   LET bn = 0
		  ELSE
			   LET num_regs = num_regs + 1
			   LET pso = "sed 's/245000001/245", num_regs USING"&&&&&&", "/g' ",
				           G_LISTAP CLIPPED, " > ", G_LISTA
		  END IF
	 END IF
	 
	 RUN pso 
	 
	 LET pso = 'rm ', G_LISTAP
	 
	 RUN pso

	 LET G_LISTA = "chmod 777 ", g_paramgrales.ruta_envio CLIPPED, "/",
		             HOY USING "yyyymmdd" CLIPPED, "_AF_",
		             g_tabafo.codigo_afore USING"&&&", "_000", ".0516" CLIPPED
 	 RUN G_LISTA

	 LET vecho = "echo ", HOY USING"YYYYMMDD" CLIPPED, "_AF_", 
		           g_tabafo.codigo_afore USING "&&&", "_000", ".0516" CLIPPED, " > ", 
		           g_paramgrales.ruta_envio CLIPPED, "/rescate.0516"
	RUN vecho
	
END FUNCTION


FUNCTION ProcesaRetiros()
#-------------------------

   INITIALIZE g_cta.* TO NULL
   
   LET g_cta.t_opera = '01'
   LET g_cta.t_depos = '09'
   LET g_cta.t_sief = '002'
   LET g_cta.cve_sief = g_tabafo.codigo_afore

   DECLARE cursor01 CURSOR FOR
   SELECT nss, 
          DECODE( subcuenta,  3, '05', 10, '05', 23, '05', 11, '06', 12, '06', 15, '07', 16, '07', 21, '07' ),
          fecha_conversion,
          LPAD( siefore, 3, 0 ),
          SUM(monto_en_pesos), SUM(monto_en_acciones), folio
   FROM   dis_cuenta
   WHERE  fecha_conversion BETWEEN fecini AND fecfin
   AND    tipo_movimiento IN ( 10, 490, 897,898 )
   AND    subcuenta IN ( 3, 10, 11, 12, 15, 16, 21, 23 )
   AND    siefore IN ( 1, 2, 3, 4, 5, 6 )
   AND    id_aportante MATCHES "RET*"
   GROUP BY 1, 2, 3, 4, 7         
   
   FOREACH cursor01 INTO g_cta.nss, 
   	                     g_cta.t_aport, 
   	                     g_cta.f_abono, 
   	                     g_cta.subt_sief,
                         g_cta.m_aport, 
                         g_cta.m_accion, 
                         g_cta.folio
                         
      LET g_cta.f_inver = g_cta.f_abono

      SQL
      SELECT FIRST 1 fecha_solic 
      INTO $g_cta.f_recep
      FROM   ret_cta_vol
      WHERE  n_seguro = $g_cta.nss AND n_folio_liq = $g_cta.folio
      END SQL

      SQL
      SELECT FIRST 1 n_unico, 
                     n_rfc, 
                     nombres, 
                     paterno, 
                     materno, 
                     fena
      INTO   $g_cta.curp, 
             $g_cta.rfc, 
             $g_cta.nombre, 
             $g_cta.paterno,
             $g_cta.materno, 
             $g_cta.f_nacim
      FROM   afi_mae_afiliado
      WHERE  n_seguro = $g_cta.nss
      END SQL

      LET g_cta.t_afil = DekeTipoEsElNSS( g_cta.nss )
      LET num_regs = num_regs + 1
      
      OUTPUT TO REPORT listado( '302'  )
   END FOREACH
END FUNCTION


FUNCTION DekeTipoEsElNSS( l_nss )
#--------------------------------

   DEFINE
      l_nss                CHAR(11),
      l_IMSS, l_ISSSTE,
      l_tmp                SMALLINT,
      m_afil               DECIMAL(16, 6)

   LET l_IMSS = FALSE
   LET l_ISSSTE = FALSE

   SQL
      SELECT FIRST 1 1 
      FROM   afi_mae_afiliado
      WHERE  n_seguro = $l_nss
      AND    tipo_solicitud NOT IN ( 8, 12 )
   END SQL
   
   IF( SQLCA.SQLCODE <> NOTFOUND ) THEN
      LET l_IMSS = TRUE    #* Es Afiliado del IMSS *
   END IF

   SQL
      SELECT FIRST 1 tipo_trab_ind 
      INTO   $l_tmp 
      FROM   cta_ctr_reg_ind
      WHERE  nti = $l_nss AND tipo_trab_ind IN ( 1, 2 )
   END SQL
   
   IF( SQLCA.SQLCODE <> NOTFOUND ) THEN
      IF( l_tmp = 1 ) THEN
         RETURN 3            #* Es Independiente *
      END IF
      LET l_ISSSTE = TRUE    #* Es Afiliado ISSSTE *
   END IF
   
   IF( l_IMSS ) THEN
      SELECT NVL( SUM( monto_en_acciones ), 0 ) 
      INTO   m_afil
      FROM   dis_cuenta
      WHERE  nss = l_nss
      AND    subcuenta IN ( 13, 14, 19, 30, 31, 32, 33, 35 )
      
      IF( m_afil > 0 ) THEN
         RETURN 4   #* Es Mixto IMSS *
      END IF
      RETURN 1      #* Es Solo Afiliado IMSS *
   END IF

   IF( l_ISSSTE ) THEN
      SELECT NVL( SUM( monto_en_acciones ), 0 ) 
      INTO   m_afil
      FROM   dis_cuenta
      WHERE   nss = l_nss AND subcuenta IN ( 1, 2, 7, 8 )
      
      IF( m_afil > 0 ) THEN
         RETURN 5   #* Es Mixto ISSSTE *
      END IF
      
      RETURN 2      #* Es Solo Afiliado ISSSTE *
   END IF
   RETURN 0    #* Tipo de Afiliado no identificado *
   
END FUNCTION


REPORT listado( d )
#-------------------

	DEFINE
		d			CHAR(3),
		fmtA		CHAR(16),
		fmtX		CHAR(15),
		t_benef	CHAR(1)

   OUTPUT
   PAGE LENGTH 100000
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

   FORMAT
   PAGE HEADER
       PRINT COLUMN 01, "000",
             COLUMN 04, "0516",
             COLUMN 08, "001",
             COLUMN 11, g_tabafo.codigo_afore USING"&&&",
             COLUMN 14, HOY USING "YYYYMMDD",
             COLUMN 22, "245",
             COLUMN 25, num_regs USING "&&&&&&",
             COLUMN 31, 215 SPACES
             
   ON EVERY ROW
      IF g_cta.nss[1] = "I" THEN 
         LET g_cta.nss = "           "
      END IF
      
		  LET t_benef = '2'		#* Coppel solo S/Benef. Fiscal
		  LET fmtA = "&&&&&&&&&&&&&&&&"
		  LET fmtX = "&&&&&&&&&&&&&&&"
		  
      IF( d = '302' ) THEN
			   LET t_benef = ' '
			   LET fmtA = "-&&&&&&&&&&&&&&&"
			   LET fmtX = "-&&&&&&&&&&&&&&"
      END IF
      
      PRINT COLUMN 01, d,
            COLUMN 04, g_cta.t_aport                   ,
            COLUMN 06, g_cta.t_opera                    ,
            COLUMN 08, g_cta.t_depos                     ,
            COLUMN 10, g_cta.f_recep USING"YYYYMMDD"   ,
            COLUMN 18, g_cta.f_abono     USING"YYYYMMDD"   ,
            COLUMN 26, g_cta.f_inver USING"YYYYMMDD"   ,   
            COLUMN 34, g_cta.t_sief                      ,
            COLUMN 37, g_cta.cve_sief                     ,
            COLUMN 40, g_cta.subt_sief                   ,
            COLUMN 43, g_cta.nss                               ,
            COLUMN 54, g_cta.curp                              ,
            COLUMN 72, g_cta.rfc                               ,
            COLUMN 85, g_cta.nombre                            ,
            COLUMN 125, g_cta.paterno                          ,
            COLUMN 165, g_cta.materno                          ,
            COLUMN 205, g_cta.f_nacim USING"YYYYMMDD" ,
            COLUMN 213, g_cta.m_aport * 100 USING fmtA,
            COLUMN 229, g_cta.m_accion * 1000000 USING fmtX,
				    COLUMN 244, g_cta.t_afil USING '&',
				    COLUMN 245, t_benef
END REPORT


FUNCTION habil_anterior(diaActual)
#------------------------------------

   DEFINE 
      diaTmp          DATE,
      contador        SMALLINT,
      diaActual       DATE,
      diaHabilAnt     DATE,
      diaSemana       SMALLINT,
      feriado         SMALLINT,
      finSemana       SMALLINT

   LET diaHabilAnt = diaActual - 1 UNITS DAY

   WHILE TRUE
      LET feriado   = 0
      LET finSemana = 0
      LET diaSemana = WEEKDAY(diaHabilAnt)

      IF diaSemana = 0 OR diaSemana = 6 THEN
         LET finSemana = 1
      END IF

      SELECT * 
      FROM   tab_feriado
			WHERE  feria_fecha = diaHabilAnt
			
      IF STATUS <> NOTFOUND THEN
         LET feriado = 1
      END IF

      IF feriado = 1 OR finSemana = 1 THEN
          LET diaHabilAnt = diaHabilAnt - 1 UNITS DAY
      ELSE
         EXIT WHILE
      END IF
   END WHILE
   RETURN diaHabilAnt
END FUNCTION #habil_anterior


FUNCTION formato_nombre(cadena)
#------------------------------

	DEFINE 
	   cad_in CHAR(001),
	   cadena CHAR(40)
	   
	DEFINE 
	   in SMALLINT
	   

	FOR in = 1 TO LENGTH(cadena)
		 LET cad_in = cadena[in]

		 SELECT "OK" 
		 FROM   est_ascii a 
		 WHERE  a.simbolo = cad_in
		 
		 IF STATUS <> NOTFOUND THEN
			  LET cadena[in] = 'Ñ'
		 END IF
		 
	END FOR
	RETURN cadena
END FUNCTION
