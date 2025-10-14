############################################################################
#Proyecto		:	AFORE ( MEXICO )                           #
#Propietario		:	E.F.P.					   #
#Programa TAAC010	:	Recibe archivo de notificacion de cuentas. #
#Fecha creacion		:	09 de Mayo de 2003			   #
#Autor			:	Fernando Herrera Hernandez		   #
#Sistema		:	TAA					   #
#Mod REQ:CPL-550	:       Juan de la cruz P.V.                       #
#Mod REQ:CPL-649	:JCPV 14/09/2011.   ADD Origenes 12 y 24           #
############################################################################
DATABASE safre_af
   GLOBALS
      DEFINE g_param_taa			RECORD LIKE seg_modulo.*
      DEFINE reg_cza_notifica 			RECORD	
             tipo_registro			CHAR(02),
	     ident_servicio			CHAR(02),
	     ident_operacion			CHAR(02),
	     tipo_ent_origen			CHAR(02),
	     cve_ent_origen			CHAR(03),
	     tipo_ent_destino			CHAR(02),
	     cve_ent_destino			CHAR(03),
	     f_transf_lote			DATE,
	     consec_dia				DECIMAL(3,0)
      						END RECORD
      DEFINE reg_det_notifica			RECORD
	     tipo_registro			CHAR(02),
	     cont_servicio			DECIMAL(9,0),
	     tipo_ent_recep			CHAR(02),
	     cve_ent_recep			CHAR(03),
	     tipo_ent_ced			CHAR(02),
	     cve_ent_ced			CHAR(03),
	     tipo_traspaso			CHAR(02),
	     fecha_presentacion			DATE,
  	     nss_afore				CHAR(11),
             curp_bdnsar                        CHAR(18),     --CPL-550
	     rfc_afore				CHAR(13),
	     paterno				CHAR(40),
	     materno				CHAR(40),
	     nombres				CHAR(40),
	     cve_sector				CHAR(1),
	     nss_ent_ced			CHAR(11),
	     nombre_imss			CHAR(50),
	     fecha_actualiza			DATE,
	     usuario				CHAR(08),
	     repetido				SMALLINT
						END RECORD

      DEFINE reg_sum_notifica			RECORD
	     tipo_registro			CHAR(02),
	     ident_servicio			CHAR(02),
	     ident_operacion			CHAR(02),
	     tipo_ent_origen			CHAR(02),
	     cve_ent_origen			CHAR(03),
	     tipo_ent_destino			CHAR(02),
	     cve_ent_destino			CHAR(03),
	     reg_det				DECIMAL(9,0),
	     f_transf_lote			DATE,
   	     consec_dia				DECIMAL(9,0)
						END RECORD

      DEFINE hoy				DATE,
	     cuantos				INTEGER,
	     contador				INTEGER,
             g_usuario				CHAR(8),
	     g_lista				CHAR(100),
	     g_list1				CHAR(100),
	     g_listd				CHAR(100),
	     aux_pausa				CHAR(1)

      DEFINE g_afore				RECORD LIKE tab_afore_local.*

      DEFINE bnd_proceso			SMALLINT

      DEFINE generar				CHAR(020),
	     enter				CHAR(001),
             archivo_traspaso			CHAR(500),
             bandera				SMALLINT,
	     vf_transf_lote			CHAR(008),
	     vf_transf_lote9			CHAR(010),
             vfecha_presentacion                CHAR(008), 
             vfecha_presentacion8		CHAR(010) 

      DEFINE cont_rech				INTEGER
      DEFINE cont_recd				INTEGER
      DEFINE cont_acept				INTEGER

   END GLOBALS
############################################################################
MAIN

   DISPLAY " "
   DISPLAY ".1"

   CALL STARTLOG("TAAC010.log")
   CALL inicio() #i

   IF NOT bnd_proceso THEN
      DEFER INTERRUPT
      OPTIONS
        INPUT WRAP,
        MESSAGE LINE LAST,
        PROMPT  LINE LAST,
        ERROR   LINE LAST
    
      CALL proceso_principal() #pp
   ELSE
      CALL validacion_previa() #vp
      CALL lee_archivo_plano() #lap
   END IF

   #CALL imprime_reporte()      #ir

END MAIN
############################################################################
FUNCTION proceso_principal()
#pp-------------------------

   OPEN WINDOW ventana_1 AT 4,4 WITH FORM "TAAC010" 
   ATTRIBUTE(BORDER)  
   DISPLAY "                            < CTRL - C > Salir                                 " AT 1,1 
   ATTRIBUTE(REVERSE)
   DISPLAY " TAAC010              ARCHIVO DE NOTIFICACION DE CUENTAS                       " AT 3,1
   ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,65
   ATTRIBUTE(REVERSE)
   DISPLAY "Total de registros en el archivo: ", contador   AT 13,05
   DISPLAY "Total de registros aceptados    : ", cont_acept AT 14,05
   DISPLAY "Total de registros rechazados   : ", cont_rech  AT 15,05     
   DISPLAY "Total de registros rech. dup.   : ", cont_recd  AT 16,05     

   INPUT BY NAME generar

      AFTER FIELD generar
         IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
         END IF

         WHENEVER ERROR CONTINUE
            LET archivo_traspaso = g_param_taa.ruta_rescate CLIPPED, "/",
				   generar CLIPPED

            DATABASE safre_tmp
            LOAD FROM archivo_traspaso 
            INSERT INTO tmp_pla_notifica
   
            DATABASE safre_af

            SELECT COUNT(*)
              INTO cuantos
              FROM safre_tmp:tmp_pla_notifica 

            IF cuantos = 0 THEN
               DISPLAY " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
               AT 19,2
               ATTRIBUTE(REVERSE)
               SLEEP 3
               NEXT FIELD generar
            ELSE
               EXIT INPUT
            END IF
         WHENEVER ERROR STOP

         ON KEY (INTERRUPT)
            ERROR "PROCESO CANCELADO" 
            ATTRIBUTE(REVERSE)
            SLEEP 2
            EXIT PROGRAM 

   END INPUT

   ERROR "PROCESANDO INFORMACION "

   CALL validacion_previa() #vp
   CALL lee_archivo_plano() #lap 

   ERROR ""

   PROMPT "Presione <enter> para finalizar " FOR enter

END FUNCTION
############################################################################
FUNCTION inicio()
#i---------------

   LET bnd_proceso = 0
   LET contador    = 0
   LET cuantos     = 0
   LET cont_rech   = 0
   LET cont_recd   = 0
   LET cont_acept  = 0


   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp
      DROP TABLE tmp_pla_notifica
   WHENEVER ERROR STOP

   CREATE TABLE tmp_pla_notifica
      (n_registros	CHAR(300))                              --CPL-550

   DATABASE safre_af

   LET hoy = TODAY

   SELECT *, USER
     INTO g_param_taa.*, g_usuario
     FROM seg_modulo 
    WHERE modulo_cod = 'taa'

END FUNCTION
############################################################################
FUNCTION validacion_previa()
#vp-------------------------

   DEFINE
     vtipo_registro				CHAR(02),
     sw_1					SMALLINT,
     sw_2					SMALLINT,
     sw_9					SMALLINT

   DECLARE c_1 CURSOR FOR
   SELECT  UNIQUE(n_registros[1,2])
   FROM    safre_tmp:tmp_pla_notifica

   LET sw_1 = 0   
   LET sw_2 = 0   
   LET sw_9 = 0   

   FOREACH c_1 INTO vtipo_registro
      CASE vtipo_registro
         WHEN '01'
            LET sw_1 = 1
         WHEN '02'
            LET sw_2 = 1
         WHEN '09'
            LET sw_9 = 1
      END CASE
   END FOREACH
    
   IF sw_1 = 0 THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped, NO EXISTE ENCABEZADO"
     ELSE
        PROMPT "SE RECHAZA EL LOTE. NO EXISTE ENCABEZADO" FOR enter
     END IF
     EXIT PROGRAM
   END IF

   IF sw_2 = 0 THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped, NO EXISTE REGISTROS DE DETALLE"
     ELSE
        PROMPT "SE RECHAZA EL LOTE. NO EXISTE REGISTROS DE DETALLE" FOR enter
     END IF
     EXIT PROGRAM
   END IF

   IF sw_9 = 0 THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped, NO EXISTE SUMARIO"
     ELSE
        PROMPT "SE RECHAZA EL LOTE. NO EXISTE SUMARIO" FOR enter
     END IF
     EXIT PROGRAM
   END IF

END FUNCTION
############################################################################
FUNCTION lee_archivo_plano()
#lap------------------------

   DEFINE 
     cont				INTEGER,
     carga_reg				CHAR(300),                --CPL-550
     v2ident_operacion			CHAR(002),
     v2carga_reg			CHAR(002)

   SELECT *
   INTO   g_afore.*
   FROM	  tab_afore_local

   LET g_lista = g_param_taa.ruta_listados CLIPPED, "/", g_usuario CLIPPED,
		 ".not53.", hoy USING "DDMMYY"

   LET g_list1 = g_param_taa.ruta_listados CLIPPED, "/", g_usuario CLIPPED,
		 ".not_rech.", hoy USING "DDMMYY"

   LET g_listd = g_param_taa.ruta_listados CLIPPED, "/", g_usuario CLIPPED,
		 ".not_rech_dup53.", hoy USING "DDMMYY"

   START REPORT listados TO g_lista
   START REPORT listrech TO g_list1
   START REPORT listrecd TO g_listd

   DECLARE c_2 CURSOR FOR
   SELECT  *
   FROM    safre_tmp:tmp_pla_notifica

   LET bandera           = 0
   LET cont              = 0
   LET v2ident_operacion = ""

   FOREACH c_2 INTO carga_reg
      LET cont = cont + 1

          #---ENCABEZADO NOTIFICACION DE CUENTAS AUTOMATICO---#

      IF carga_reg[1,2] = '01' THEN 
         LET v2ident_operacion                    = '01'
         LET reg_cza_notifica.tipo_registro       = carga_reg[001,002]
         LET reg_cza_notifica.ident_servicio      = carga_reg[003,004]
         LET reg_cza_notifica.ident_operacion     = carga_reg[005,006]
         LET reg_cza_notifica.tipo_ent_origen     = carga_reg[007,008]
         LET reg_cza_notifica.cve_ent_origen      = carga_reg[009,011]
         LET reg_cza_notifica.tipo_ent_destino    = carga_reg[012,013] 
         LET reg_cza_notifica.cve_ent_destino     = carga_reg[014,016]
         LET vf_transf_lote		          = carga_reg[020,027]
         LET vf_transf_lote9                      = vf_transf_lote[5,6], "/",
						    vf_transf_lote[7,8], "/",
						    vf_transf_lote[1,4] 
         LET reg_cza_notifica.f_transf_lote       = vf_transf_lote9
         LET reg_cza_notifica.consec_dia          = carga_reg[028,030]

         INSERT INTO taa_cza_notifica VALUES(reg_cza_notifica.*)
  						  
      END IF

          #---DETALLE NOTIFICACION DE CUENTAS AUTOMATICO---#
      IF carga_reg[1,2] = '02' AND v2ident_operacion = '01' THEN
       LET contador = contador + 1
       DISPLAY "Total de registros en el archivo: ", contador AT 13,05
       LET reg_det_notifica.tipo_registro       = carga_reg[001,002]
       LET reg_det_notifica.cont_servicio       = carga_reg[003,012]
       LET reg_det_notifica.tipo_ent_recep      = carga_reg[013,014]
       LET reg_det_notifica.cve_ent_recep       = carga_reg[015,017]
       LET reg_det_notifica.tipo_ent_ced        = carga_reg[018,019]
       LET reg_det_notifica.cve_ent_ced         = carga_reg[020,022]
       LET reg_det_notifica.tipo_traspaso       = carga_reg[023,024]
       LET vfecha_presentacion	          = carga_reg[025,032]
       LET vfecha_presentacion8	          = vfecha_presentacion[5,6],
  			   	    "/",
 				    vfecha_presentacion[7,8],
 				    "/",
					 	    vfecha_presentacion[1,4] 
       LET reg_det_notifica.fecha_presentacion  = vfecha_presentacion8
       LET reg_det_notifica.nss_afore		= carga_reg[033,043]  --CPL-550
       LET reg_det_notifica.rfc_afore		= ' '                 --CPL-550
       LET reg_det_notifica.curp_bdnsar         = carga_reg[044,061]  --CPL-550
       LET reg_det_notifica.paterno		= carga_reg[062,101]  --CPL-550
       LET reg_det_notifica.materno		= carga_reg[102,141]  --CPL-550
       LET reg_det_notifica.nombres		= carga_reg[142,181]  --CPL-550
       LET reg_det_notifica.cve_sector	        =  ' '                --CPL-550
       LET reg_det_notifica.nss_ent_ced	        =  ' '                --CPL-550
       LET reg_det_notifica.nombre_imss	        = carga_reg[182,231]  --CPL-550
       LET reg_det_notifica.fecha_actualiza 	= hoy
       LET reg_det_notifica.usuario		= g_usuario
       LET reg_det_notifica.repetido		= 0 

         SELECT 'X' 
         FROM   taa_det_notifica
         WHERE  @nss_afore          = reg_det_notifica.nss_afore
         AND    @fecha_presentacion = reg_det_notifica.fecha_presentacion
         AND    @cve_ent_ced        = reg_det_notifica.cve_ent_ced
         GROUP BY 1
         IF STATUS = NOTFOUND THEN
            SELECT 'X'
            FROM   afi_solicitud
            WHERE  @n_seguro = reg_det_notifica.nss_afore
            AND    @status_interno in (50, 51, 65, 70, 71, 72)
            GROUP BY 1
            IF STATUS <> NOTFOUND THEN
               LET cont_acept			= cont_acept + 1
               DISPLAY "Total de registros aceptados    : ", cont_acept 
               AT 14,05 
               LET reg_det_notifica.repetido		= 0
               INSERT INTO taa_det_notifica VALUES(reg_det_notifica.*) 
               OUTPUT TO REPORT listados(reg_det_notifica.*) 
            ELSE
               LET cont_rech                         = cont_rech + 1 
               DISPLAY "Total de registros rechazados   : ", cont_rech 
	       AT 15,05 
               LET reg_det_notifica.repetido		= 2
               INSERT INTO taa_det_notifica VALUES(reg_det_notifica.*) 
               OUTPUT TO REPORT listrech(reg_det_notifica.*)
            END IF
         ELSE
            LET cont_recd                             = cont_recd + 1 
            DISPLAY "Total de registros rech. dup.   : ", cont_recd AT 16,05 
            LET reg_det_notifica.repetido		 = 1 
            INSERT INTO taa_det_notifica VALUES(reg_det_notifica.*) 
            OUTPUT TO REPORT listrecd(reg_det_notifica.*)
         END IF 
      END IF

          #---SUMARIO NOTIFICACION DE CUENTAS AUTOMATICO---#
      IF carga_reg[1,2] = '09' AND v2ident_operacion = '01' THEN
         LET v2ident_operacion                    = ""
         LET reg_sum_notifica.tipo_registro       = carga_reg[001,002]
         LET reg_sum_notifica.ident_servicio      = 
             reg_cza_notifica.ident_servicio
         LET reg_sum_notifica.ident_operacion     = 
             reg_cza_notifica.ident_operacion
         LET reg_sum_notifica.tipo_ent_origen     = 
             reg_cza_notifica.tipo_ent_origen
         LET reg_sum_notifica.cve_ent_origen      = 
             reg_cza_notifica.cve_ent_origen
         LET reg_sum_notifica.cve_ent_destino     = 
             reg_cza_notifica.cve_ent_destino
         LET reg_sum_notifica.tipo_ent_destino    = 
             reg_cza_notifica.tipo_ent_destino
         LET reg_sum_notifica.cve_ent_destino     = 
             reg_cza_notifica.cve_ent_destino
         LET reg_sum_notifica.reg_det		  = carga_reg[003,011]
         LET reg_sum_notifica.f_transf_lote 	  = 
             reg_cza_notifica.f_transf_lote 
         LET reg_sum_notifica.consec_dia          = reg_cza_notifica.consec_dia

         INSERT INTO taa_sum_notifica VALUES(reg_sum_notifica.*)
      END IF

   END FOREACH

   FINISH REPORT listrecd
   FINISH REPORT listrech
   FINISH REPORT listados

END FUNCTION
############################################################################
{FUNCTION imprime_reporte()
#ir-----------------------

   INITIALIZE r_taa_notifica.* TO NULL

   SELECT *
   INTO   g_afore.*
   FROM	  tab_afore_local

   LET g_lista = g_param_taa.ruta_listados CLIPPED, "/", g_usuario CLIPPED,
		 ".not_", reg_cza_notifica.ident_operacion,
                 ".", hoy USING "DDMMYY"

   LET g_list1 = g_param_taa.ruta_listados CLIPPED, "/", g_usuario CLIPPED,
		 ".not_rech", reg_cza_notifica.ident_operacion,
                 ".", hoy USING "DDMMYY"

   START REPORT listado TO g_lista
   START REPORT lisrech TO g_list1
   DECLARE c_rep CURSOR FOR
    SELECT *
      FROM taa_det_notifica
   IF STATUS = NOTFOUND THEN
      IF bnd_proceso THEN
         DISPLAY "Program stopped, NO SE IMPRIME REPORTE"
      ELSE
         ERROR "NO SE IMPRIME REPORTE"
         SLEEP 2
      END IF
   ELSE
     FOREACH c_rep INTO r_taa_notifica.* 
        IF r_taa_notifica.repetido = 0 THEN
           OUTPUT TO REPORT listados(r_taa_notifica.*) 
        ELSE
           OUTPUT TO REPORT listrech(r_taa_notifica.*)
        END IF
     END REPORT
   END IF 
   
   FINISH REPORT listrech
   FINISH REPORT listados

   IF bnd_proceso THEN
      DISPLAY "SE CONCLUYE REPORTE"
   ELSE
      ERROR "SE CONCLUYE PROCESO"
      SLEEP 2
   END IF

END FUNCTION}
############################################################################
REPORT listados(l_taa_notifica)
#-----------------------------

   DEFINE l_taa_notifica			RECORD LIKE taa_det_notifica.*

   DEFINE campo					RECORD
	  cod_afore				SMALLINT,
	  raz_social				CHAR(50),
	  des_titulo				CHAR(23)
    						END RECORD

   DEFINE vafore_desc				CHAR(30),
          vcurp					CHAR(18),
          vfrecafor                             DATE

   OUTPUT
      PAGE   LENGTH 	90
      LEFT   MARGIN	 0
      RIGHT  MARGIN	 0
      TOP    MARGIN	 0
      BOTTOM MARGIN      0 

   FORMAT
     PAGE HEADER

     SELECT a.codigo_afore, a.razon_social, b.afore_desc
       INTO campo.*
       FROM tab_afore_local a, tab_afore b
      WHERE a.codigo_afore = b.afore_cod

     PRINT COLUMN 004, '\033e\033(s218T\033(s11H\033(s7B', 
                       'INFORMACION DE NOTIFICACION DE CUENTAS A AFORE ',
                       campo.des_titulo CLIPPED, ' ', hoy USING "DD-MM-YYYY"

     PRINT COLUMN 004, '\033e\033(s218T\033(s11H\033(s7B',
                       'Pagina: ', pageno USING "&&&", 
                       ' REGISTROS ACEPTADOS'

     PRINT COLUMN 004, '\033e\033(s218T\033(s11H\033(s7B',
                       'NOMBRE DEL ARCHIVO : ', generar,  
                       '     FECHA DE PRESENTACION : ', 
                       vfecha_presentacion8[4,5], "/",
                       vfecha_presentacion8[1,2], "/",
                       vfecha_presentacion8[7,10] 

     PRINT 
     PRINT COLUMN 001, '\033e\033(s218T\033(s11H\033(s7N'
     PRINT COLUMN 002, campo.cod_afore,'    ', campo.raz_social
     PRINT
     PRINT COLUMN 001, "\332\304\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\302\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\302\304\304\304\304\304\304",
                       "\304\302\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\302\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\304\277"
     PRINT COLUMN 001, "\263",
           COLUMN 003, "NSS ",
           COLUMN 015, "\263",
           COLUMN 017, "CURP",
           COLUMN 033, "\263",
           COLUMN 035, "CLAVE",
           COLUMN 041, "\263",
           COLUMN 043, "AFORE",
           COLUMN 075, "\263",
           COLUMN 077, "FEC. PRES.",
           COLUMN 089, "\263" 

     PRINT COLUMN 001, "\300\304\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\301\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\301\304\304\304\304\304\304",
                       "\304\301\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\301\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\304\331"

     ON EVERY ROW
        LET vafore_desc = NULL
        SELECT b.afore_desc
        INTO   vafore_desc
        FROM   tab_afore b
        WHERE  b.afore_cod = l_taa_notifica.cve_ent_ced
        IF STATUS = NOTFOUND THEN
           LET vafore_desc = NULL
        END IF

        SELECT MAX(frecafor)
          INTO vfrecafor
          FROM afi_solicitud 
         WHERE n_seguro       = l_taa_notifica.nss_afore
           AND status_interno > 45

        SELECT b.n_unico
        INTO   vcurp
        FROM   afi_solicitud b
        WHERE  b.n_seguro       = l_taa_notifica.nss_afore
        AND    b.frecafor       = vfrecafor
        AND    b.status_interno > 45

        PRINT 
        PRINT COLUMN 004, l_taa_notifica.nss_afore,
              COLUMN 017, vcurp,
              COLUMN 035, l_taa_notifica.cve_ent_ced USING "&&&",
              COLUMN 043, vafore_desc, 
              COLUMN 077, l_taa_notifica.fecha_presentacion USING "DD-MM-YYYY"

END REPORT
############################################################################
REPORT listrecd(l_taa_notifica)
#-----------------------------

   DEFINE l_taa_notifica			RECORD LIKE taa_det_notifica.*

   DEFINE campo					RECORD
	  cod_afore				SMALLINT,
	  raz_social				CHAR(50),
	  des_titulo				CHAR(23)
    						END RECORD

   DEFINE vafore_desc				CHAR(30),
          vcurp					CHAR(18),
          vfrecafor                             DATE

   OUTPUT
      PAGE   LENGTH 	90
      LEFT   MARGIN	 0
      RIGHT  MARGIN	 0
      TOP    MARGIN	 0
      BOTTOM MARGIN      0 

   FORMAT
     PAGE HEADER

     SELECT a.codigo_afore, a.razon_social, b.afore_desc
       INTO campo.*
       FROM tab_afore_local a, tab_afore b
      WHERE a.codigo_afore = b.afore_cod

     PRINT COLUMN 004, '\033e\033(s218T\033(s11H\033(s7B', 
                       'INFORMACION DE NOTIFICACION DE CUENTAS A AFORE ',
                       campo.des_titulo CLIPPED, ' ', hoy USING "DD-MM-YYYY"

     PRINT COLUMN 004, '\033e\033(s218T\033(s11H\033(s7B',
                       'NOMBRE DEL ARCHIVO : ', generar,  
                       '     FECHA DE PRESENTACION : ', 
                       vfecha_presentacion8[4,5], "/",
                       vfecha_presentacion8[1,2], "/",
                       vfecha_presentacion8[7,10] 

     PRINT COLUMN 004, '\033e\033(s218T\033(s11H\033(s7B',
                       'Pagina: ', pageno USING "&&&", 
                       ' REGISTRO RECHAZADOS, DUPLICADOS EN EL ARCHIVO'

     PRINT 
     PRINT COLUMN 001, '\033e\033(s218T\033(s11H\033(s7N'
     PRINT COLUMN 002, campo.cod_afore,'    ', campo.raz_social
     PRINT
     PRINT COLUMN 001, "\332\304\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\302\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\302\304\304\304\304\304\304",
                       "\304\302\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\302\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\304\277"
     PRINT COLUMN 001, "\263",
           COLUMN 003, "NSS ",
           COLUMN 015, "\263",
           COLUMN 017, "CURP",
           COLUMN 033, "\263",
           COLUMN 035, "CLAVE",
           COLUMN 041, "\263",
           COLUMN 043, "AFORE",
           COLUMN 075, "\263",
           COLUMN 077, "FEC. PRES.",
           COLUMN 089, "\263" 
     PRINT COLUMN 001, "\300\304\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\301\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\301\304\304\304\304\304\304",
                       "\304\301\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\301\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\304\331"

     ON EVERY ROW
        LET vafore_desc = NULL
        SELECT b.afore_desc
        INTO   vafore_desc
        FROM   tab_afore b
        WHERE  b.afore_cod = l_taa_notifica.cve_ent_ced
        IF STATUS = NOTFOUND THEN
           LET vafore_desc = NULL
        END IF

        SELECT MAX(frecafor)
          INTO vfrecafor
          FROM afi_solicitud
         WHERE n_seguro       = l_taa_notifica.nss_afore
           AND status_interno > 45

        SELECT b.n_unico
        INTO   vcurp
        FROM   afi_solicitud b
        WHERE  b.n_seguro       = l_taa_notifica.nss_afore
        AND    b.frecafor       = vfrecafor
        AND    b.status_interno > 45

        PRINT 
        PRINT COLUMN 004, l_taa_notifica.nss_afore,
              COLUMN 017, vcurp,
              COLUMN 035, l_taa_notifica.cve_ent_ced USING "&&&",
              COLUMN 043, vafore_desc, 
              COLUMN 077, l_taa_notifica.fecha_presentacion USING "DD-MM-YYYY"

END REPORT
############################################################################
REPORT listrech(l_taa_notifica)
#-----------------------------

   DEFINE l_taa_notifica			RECORD LIKE taa_det_notifica.*

   DEFINE campo					RECORD
	  cod_afore				SMALLINT,
	  raz_social				CHAR(50),
	  des_titulo				CHAR(23)
    						END RECORD

   DEFINE vafore_desc				CHAR(30),
          vcurp					CHAR(18),
          vfrecafor                             DATE

   OUTPUT
      PAGE   LENGTH 	90
      LEFT   MARGIN	 0
      RIGHT  MARGIN	 0
      TOP    MARGIN	 0
      BOTTOM MARGIN      0 

   FORMAT
     PAGE HEADER

     SELECT a.codigo_afore, a.razon_social, b.afore_desc
       INTO campo.*
       FROM tab_afore_local a, tab_afore b
      WHERE a.codigo_afore = b.afore_cod

     PRINT COLUMN 004, '\033e\033(s218T\033(s11H\033(s7B', 
                       'INFORMACION DE NOTIFICACION DE CUENTAS A AFORE ',
                       campo.des_titulo CLIPPED, hoy USING "DD-MM-YYYY"

     PRINT COLUMN 004, '\033e\033(s218T\033(s11H\033(s7B',
                       'NOMBRE DEL ARCHIVO : ', generar,  
                       '     FECHA DE PRESENTACION : ', 
                       vfecha_presentacion8[4,5], "/",
                       vfecha_presentacion8[1,2], "/",
                       vfecha_presentacion8[7,10] 

     PRINT COLUMN 004, '\033e\033(s218T\033(s11H\033(s7B',
                       'Pagina: ', pageno USING "&&&", 
                       ' REGISTRO RECHAZADOS, YA NOTIFICADOS'

     PRINT 
     PRINT COLUMN 001, '\033e\033(s218T\033(s11H\033(s7N'
     PRINT COLUMN 002, campo.cod_afore,'    ', campo.raz_social
     PRINT
     PRINT COLUMN 001, "\332\304\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\302\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\302\304\304\304\304\304\304",
                       "\304\302\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\302\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\304\277"
     PRINT COLUMN 001, "\263",
           COLUMN 003, "NSS ",
           COLUMN 015, "\263",
           COLUMN 017, "CURP",
           COLUMN 033, "\263",
           COLUMN 035, "CLAVE",
           COLUMN 041, "\263",
           COLUMN 043, "AFORE",
           COLUMN 075, "\263",
           COLUMN 077, "FEC. PRES.",
           COLUMN 089, "\263" 
     PRINT COLUMN 001, "\300\304\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\301\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\301\304\304\304\304\304\304",
                       "\304\301\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\304\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\301\304\304\304",
                       "\304\304\304\304\304\304\304\304\304\304\331"

     ON EVERY ROW
        LET vafore_desc = NULL
        SELECT b.afore_desc
        INTO   vafore_desc
        FROM   tab_afore b
        WHERE  b.afore_cod = l_taa_notifica.cve_ent_ced
        IF STATUS = NOTFOUND THEN
           LET vafore_desc = NULL
        END IF

        SELECT MAX(frecafor)
          INTO vfrecafor
          FROM afi_solicitud
         WHERE n_seguro       = l_taa_notifica.nss_afore
           AND status_interno > 45

        SELECT b.n_unico
        INTO   vcurp
        FROM   afi_solicitud b
        WHERE  b.n_seguro       = l_taa_notifica.nss_afore
        AND    b.frecafor       = vfrecafor
        AND    b.status_interno > 45

        PRINT 
        PRINT COLUMN 004, l_taa_notifica.nss_afore,
              COLUMN 017, vcurp,
              COLUMN 035, l_taa_notifica.cve_ent_ced USING "&&&",
              COLUMN 043, vafore_desc, 
              COLUMN 077, l_taa_notifica.fecha_presentacion USING "DD-MM-YYYY"

END REPORT
############################################################################
