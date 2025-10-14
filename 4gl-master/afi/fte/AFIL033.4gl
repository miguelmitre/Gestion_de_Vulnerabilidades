#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa AFIL033  => RESULTADOS DEL PROCESO DE MODIFICACIONES              #
#Fecha             => 20 DE SEPTIEMBRE DE 1999.                             # 
#Por               => MAURO MUNIZ CABALLERO                                 #
#Modificado        => FERNANDO HERRERA HERNANDEZ.                           #
#Fecha             => 05 DE AGOSTO DE 2003. VERSION 2                       #
#Sistema           => AFI 		                                    #
#############################################################################
DATABASE safre_af
GLOBALS
        DEFINE reg RECORD
            nss_solicitud      	CHAR(11) ,
            tipo_solicitud     	CHAR(1)  ,
            folio_solicitud    	DECIMAL(8,0) ,
            diag_proceso_1     	CHAR(3)  ,
            descripcion_1      	CHAR(50)  
        END RECORD

        DEFINE reg_mod RECORD
            marca              	CHAR(1)  ,
            nss_solicitud      	CHAR(11) ,
            curp_solicitud     	CHAR(18) ,
            rfc_trabajador     	CHAR(13) ,
            paterno            	CHAR(40) ,
            materno            	CHAR(40) ,
            nombres            	CHAR(40) ,
            tipo_solicitud     	CHAR(1)  ,
            folio_solicitud    	DECIMAL(8,0) ,
            doc_prob           	CHAR(16) ,
            cod_operacion      	CHAR(2)  ,
            diag_proceso       	CHAR(3)  ,
            descripcion        	CHAR(50) ,
            fecha_lote         	DATE     ,
            usuario            	CHAR(8)  ,
	    tip_prob           	CHAR(1)  ,
            fecha_emision      	CHAR(10) ,
	    folio_rec	       	INTEGER  ,
	    id_rec	       	CHAR(1)  ,
	    cod_error_origen  	SMALLINT
        END RECORD

        DEFINE 
            HOY      		DATE,
            HORA     		CHAR(8)
       
        DEFINE  
            num_enviados        ,
            num_aceptados       ,
            num_pendientes      ,
            num_rechazados      ,
            num_renapo          ,
            num_total_afi       INTEGER

        DEFINE 
            aux_n_seguro  	CHAR(11),
            g_usuario     	CHAR(8),
            G_LISTA       	CHAR(100),
            D_LISTA       	CHAR(100),
            g_imprime     	CHAR(100),
            corr          	CHAR(100),
            G_PARAMETRO	  	CHAR(2),
            enter         	CHAR(1),
            fecha_lote    	CHAR(8)

        DEFINE g_afore		RECORD LIKE tab_afore.*
        DEFINE g_seg_modulo	RECORD LIKE seg_modulo.*
        DEFINE fec_emi		CHAR(10)

END GLOBALS

MAIN

    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY CONTROL-I,
        COMMENT LINE LAST
    DEFER INTERRUPT 

    CALL STARTLOG("AFIL033.log")
    CALL inicio()     		#i
    CALL proceso_principal()  	#pp

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY  = TODAY
    LET HORA = TIME

    SELECT *, USER 
    INTO   g_afore.*,g_usuario 
    FROM   tab_afore
    WHERE  @marca = 1

    SELECT * 
    INTO   g_seg_modulo.* 
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

END FUNCTION

FUNCTION crea_tablas()
#ct-------------------

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE afi_mod
    WHENEVER ERROR STOP

    CREATE TABLE afi_mod
    (
     marca               CHAR(1)   ,
     nss_solicitud       CHAR(11)  ,
     curp_solicitud      CHAR(18)  ,
     rfc_trabajador      CHAR(13)  ,
     paterno             CHAR(40)  ,
     materno             CHAR(40)  ,
     nombres             CHAR(40)  ,
     tipo_solicitud      CHAR(1)   ,
     folio_solicitud     CHAR(8)   ,
     doc_prob            CHAR(16)  ,
     cod_operacion       CHAR(2)   ,
     diag_proceso        CHAR(3)   ,
     descripcion         CHAR(40)  , 
     fecha_lote          DATE      ,
     usuario             CHAR(8)   ,
     tip_prob            CHAR(1)   ,
     fecha_emision	 CHAR(10)  ,
     folio_rec		 INTEGER   ,
     id_rec		 CHAR(1)   ,
     cod_error_origen	 SMALLINT 
    )

    DATABASE safre_af
     
END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------


    OPEN WINDOW AFIL0331 AT 4,4 WITH FORM "AFIL0331" ATTRIBUTE(BORDER)
    DISPLAY " AFIL033   REPORTE DEL RESULTADO DE MODIFICACIONES DE PROCESAR                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " < CTRL-C > Salir " AT 1,26
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)   

    WHILE TRUE
        PROMPT "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? " FOR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                EXIT WHILE
            ELSE
                ERROR "PROCESO CANCELADO" ATTRIBUTE(REVERSE)
                SLEEP 2
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

    DISPLAY "PROCESANDO INFORMACION " AT 18,3 ATTRIBUTE(REVERSE)  
 
    CALL crea_tablas()  #ct

    SELECT campo8
    INTO   fecha_lote
    FROM   safre_tmp:cza_procesar

  LET reg_mod.fecha_lote = MDY(fecha_lote[5,6],fecha_lote[7,8],fecha_lote[1,4])
    
    DECLARE cur2 CURSOR FOR
    SELECT nss_solicitud        ,
           curp_solicitud       ,
           rfc_trabajador       , 
           paterno              , 
           materno              , 
           nombres              ,
           folio_solicitud      ,
           doc_prob             ,
           cod_operacion        ,
           diag_proceso[1,3]    ,
	   tip_prob             ,
           @fec_emision_certif  
    FROM   safre_tmp:det_procesar 
    ORDER BY 9,10 

    FOREACH cur2 INTO reg_mod.nss_solicitud,
                      reg_mod.curp_solicitud,
                      reg_mod.rfc_trabajador,
                      reg_mod.paterno,
                      reg_mod.materno,
                      reg_mod.nombres,
                      reg_mod.folio_solicitud,
                      reg_mod.doc_prob,
                      reg_mod.cod_operacion,
                      reg.diag_proceso_1,
                      reg_mod.tip_prob,
		      reg_mod.fecha_emision

    LET reg_mod.fecha_emision = reg_mod.fecha_emision CLIPPED 
    LET fec_emi		      = reg_mod.fecha_emision[5,6], "/",
				reg_mod.fecha_emision[7,8], "/",
				reg_mod.fecha_emision[1,4]
    LET reg_mod.fecha_emision = fec_emi

    SELECT @folio_rec, @id_rec, @cod_error_origen
      INTO reg_mod.folio_rec, reg_mod.id_rec, reg_mod.cod_error_origen
      FROM afi_mae_modifica
     WHERE @n_seguro 	= reg_mod.nss_solicitud
       --AND @f_respuesta = reg_mod.fecha_emision 
     GROUP BY 1,2,3

    SELECT tipo_solicitud, usuario
    INTO   reg_mod.tipo_solicitud, reg_mod.usuario 
    FROM   afi_mae_afiliado
    WHERE  n_seguro = reg_mod.nss_solicitud
    AND    n_folio  = reg_mod.folio_solicitud

    LET reg.nss_solicitud   = reg_mod.nss_solicitud
    LET reg.tipo_solicitud  = reg_mod.tipo_solicitud
    LET reg.folio_solicitud = reg_mod.folio_solicitud

    CASE reg_mod.cod_operacion 
      WHEN "01"
        CASE reg.diag_proceso_1 
	  WHEN "001" 
            LET reg.descripcion_1    = "ENVIADO A RENAPO"
            LET reg_mod.diag_proceso = reg.diag_proceso_1
            LET reg_mod.descripcion  = reg.descripcion_1
            LET reg_mod.marca        = "1"
            INSERT INTO safre_tmp:afi_mod VALUES(reg_mod.*) 
          WHEN "002"
            LET reg.descripcion_1    = "ACEPTADO PROCESAR"
            LET reg_mod.diag_proceso = reg.diag_proceso_1
            LET reg_mod.descripcion  = reg.descripcion_1
            LET reg_mod.marca        = "3"
            INSERT INTO safre_tmp:afi_mod VALUES(reg_mod.*) 
          WHEN "003"
            LET reg.descripcion_1    = "ACEPTADO NO ENVIADO"
            LET reg_mod.diag_proceso = reg.diag_proceso_1
            LET reg_mod.descripcion  = reg.descripcion_1
            LET reg_mod.marca        = "3"
            INSERT INTO safre_tmp:afi_mod VALUES(reg_mod.*) 
        END CASE
      WHEN "02"
        SELECT rdeta_desc_l
        INTO   reg.descripcion_1 
        FROM   tab_rdeta
        WHERE  rdeta_cod   = reg.diag_proceso_1
        AND    @modulo_cod = 'afi'

        IF SQLCA.SQLCODE <> 0 THEN
            LET reg.descripcion_1 = "SIN DESCRIPCION"
        END IF

        LET reg_mod.diag_proceso = reg.diag_proceso_1
        LET reg_mod.descripcion  = reg.descripcion_1
        LET reg_mod.marca        = "2"
        INSERT INTO safre_tmp:afi_mod VALUES(reg_mod.*) 
      WHEN "03"
        LET reg.descripcion_1    = "PEND. CONF. RENAPO"
        LET reg_mod.diag_proceso = reg.diag_proceso_1
        LET reg_mod.descripcion  = reg.descripcion_1
        LET reg_mod.marca	 = "3"
        INSERT INTO safre_tmp:afi_mod VALUES(reg_mod.*)
    END CASE

    END FOREACH

    INITIALIZE reg_mod.* TO NULL

    DECLARE cur_mod CURSOR FOR
    SELECT *
    FROM   safre_tmp:afi_mod
    ORDER BY 11,12,8,9,1

    LET G_LISTA = g_seg_modulo.ruta_listados CLIPPED,"/",
                  g_usuario CLIPPED,
                  ".RESULTADO_PROCESAR." CLIPPED,
                  HOY USING "ddmmyy", "_", HORA[1,2], HORA[4,5]
    LET D_LISTA = G_LISTA CLIPPED
    PROMPT D_LISTA FOR enter 

    START REPORT listado TO G_LISTA

    FOREACH cur_mod INTO reg_mod.*

	CASE reg_mod.cod_operacion 
	    WHEN "01" LET reg_mod.cod_operacion = "A"
	    WHEN "02" LET reg_mod.cod_operacion = "R"
            WHEN "03" LET reg_mod.cod_operacion = "P"
        END CASE

        OUTPUT TO REPORT listado(reg_mod.*)
    END FOREACH

    FINISH REPORT listado

    LET G_LISTA = "chmod 777 ",g_seg_modulo.ruta_listados CLIPPED,"/",
                  g_usuario CLIPPED,
                  ".RESULTADO_PROCESAR." CLIPPED,
                  HOY USING "ddmmyy", "_", HORA[1,2], HORA[4,5]

    RUN G_LISTA

    LET g_imprime = "lp ",g_seg_modulo.ruta_listados CLIPPED,"/",
                    g_usuario CLIPPED,
                    ".RESULTADO_PROCESAR." CLIPPED,
                    HOY USING "ddmmyy", "_", HORA[1,2], HORA[4,5]

    RUN g_imprime

END FUNCTION

REPORT listado(reg_mod)
#----------------------

    DEFINE reg_mod RECORD
        marca              CHAR(1)  ,
        nss_solicitud      CHAR(11) ,
        curp_solicitud     CHAR(18) ,
        rfc_trabajador     CHAR(13) ,
        paterno            CHAR(40) ,
        materno            CHAR(40) ,
        nombres            CHAR(40) ,
        tipo_solicitud     CHAR(1)  ,
        folio_solicitud    DECIMAL(8,0) ,
        doc_prob           CHAR(16) ,
        cod_operacion      CHAR(2)  ,
        diag_proceso       CHAR(3)  ,
        descripcion        CHAR(40) ,
        fecha_lote         DATE     ,
        usuario            CHAR(8)  ,
	tip_prob           CHAR(1)  ,
        fecha_emision      CHAR(10) ,
        folio_rec	   INTEGER  ,
        id_rec		   CHAR(1)  ,
        cod_error_origen   SMALLINT 
    END RECORD

    DEFINE
        razon_social       CHAR(40)  ,
        nombre             CHAR(40)  ,
        des_resultado      CHAR(10)

    OUTPUT
	PAGE LENGTH   90
	LEFT MARGIN   0
	RIGHT MARGIN  0
	TOP MARGIN    0
	BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER     
        SELECT A.razon_social
        INTO   razon_social
        FROM   tab_afore_local A

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT
            COLUMN 01,"=================================================",
            COLUMN 50,"==================================================",
            COLUMN 100,"==================================================",
	    COLUMN 150,"============================="
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT
            COLUMN 03,razon_social ,
            COLUMN 20,"LISTADO DE MODIFICACIONES ",des_resultado,
            COLUMN 90,"FECHA   :",hoy USING "DD/MM/YYYY",
            COLUMN 120,"NUM.PAG.:",pageno USING "####,###"
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT
            COLUMN 40,"PROCESO RESPUESTA DE PROCESAR A MODIFICACIONES", 
            COLUMN 90,"FECHA LOTE:",reg_mod.fecha_lote USING "DD/MM/YYYY"
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT
            COLUMN 01,"=================================================",
            COLUMN 50,"==================================================",
            COLUMN 100,"==================================================",
	    COLUMN 150,"============================="

        PRINT
        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"--------------------------------------------------",
            COLUMN 100,"--------------------------------------------------",
	    COLUMN 150,"-----------------------------"
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT
            COLUMN 001,"FOL REC"    		,
            COLUMN 010,"ID"         		,
            COLUMN 014,"NSS"        		,
            COLUMN 026,"CURP"       		,
            COLUMN 045,"RFC"        		,
            COLUMN 059,"NOMBRE"     		,
            COLUMN 110,"SOLICITUD"  		,
            COLUMN 120,"DOCTO. PROBATORIO"	,
            COLUMN 139,"DIAG"			,
	    COLUMN 146,"DESCRIPCION"		,
	    COLUMN 167,"USUARIO"		,
            COLUMN 175,"COD ERR OR"

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"--------------------------------------------------",
            COLUMN 100,"--------------------------------------------------",
	    COLUMN 150,"-----------------------------"
    BEFORE GROUP OF reg_mod.diag_proceso
        IF reg_mod.cod_operacion = "A" THEN
            CASE reg_mod.diag_proceso
                WHEN "001" LET des_resultado = "ENVIADA"
                WHEN "002" LET des_resultado = "ACEPTADA"
                WHEN "003" LET des_resultado = "ACEP. NO ENV."
            END CASE
        END IF
    BEFORE GROUP OF reg_mod.cod_operacion
        IF reg_mod.cod_operacion = "R" THEN
            LET des_resultado = "RECHAZADA"
        END IF
        IF reg_mod.cod_operacion = "P" THEN
            LET des_resultado = "PENDIENTE"
        END IF

    ON EVERY ROW
        LET nombre = reg_mod.paterno CLIPPED," ",
                     reg_mod.materno CLIPPED," ",
                     reg_mod.nombres CLIPPED

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT   
            COLUMN 01,reg_mod.folio_rec USING "--------",
            COLUMN 10,reg_mod.id_rec USING "-",
            COLUMN 14,reg_mod.nss_solicitud       ,
            COLUMN 26,reg_mod.curp_solicitud      ,
            COLUMN 45,reg_mod.rfc_trabajador      ,
            COLUMN 59,nombre                 CLIPPED,
            COLUMN 110,reg_mod.tipo_solicitud USING "&"  ,
            COLUMN 112,reg_mod.folio_solicitud USING "--------" ,
            COLUMN 121,reg_mod.tip_prob           ,
            COLUMN 123,reg_mod.doc_prob           ,
            COLUMN 140,reg_mod.cod_operacion CLIPPED,
            COLUMN 142,reg_mod.diag_proceso       ,
            COLUMN 146,reg_mod.descripcion   CLIPPED,
            COLUMN 167,reg_mod.usuario CLIPPED,
            COLUMN 177,reg_mod.cod_error_origen  #USING "----"

    ON LAST ROW
        SELECT COUNT(*)
        INTO   num_enviados
        FROM   safre_tmp:det_procesar
        WHERE  cod_operacion = "01"
	AND    diag_proceso[1,3] = "001"

        SELECT COUNT(*)
        INTO   num_aceptados
        FROM   safre_tmp:det_procesar
        WHERE  cod_operacion = "01"
	AND    diag_proceso[1,3] = "002"

        SELECT COUNT(*)
        INTO   num_pendientes
        FROM   safre_tmp:det_procesar
        WHERE  cod_operacion = "01"
	AND    diag_proceso[1,3] = "003"

        SELECT COUNT(*)
        INTO   num_rechazados
        FROM   safre_tmp:det_procesar
        WHERE  cod_operacion = "02"

        SELECT COUNT(*)
        INTO   num_renapo
        FROM   safre_tmp:det_procesar
        WHERE  cod_operacion = "03"

        LET num_total_afi = num_enviados   + num_aceptados  + 
			    num_pendientes + num_rechazados + 
                            num_renapo

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT
        PRINT
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"--------------------------------------------------",
            COLUMN 100,"--------------------------------------------------",
	    COLUMN 150,"-----------------------------"
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT   
            COLUMN 01,"NUMERO DE MODIFICACIONES ENVIADAS A RENAPO       --->",
                      num_enviados 
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT   
            COLUMN 01,"NUMERO DE MODIFICACIONES ACEPTADAS POR PROCESAR  --->",
                      num_aceptados 
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT   
            COLUMN 01,"NUMERO DE MODIFICACIONES ACEPTADAS NO ENVIADAS   --->",
                      num_pendientes 
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT   
            COLUMN 01,"NUMERO DE MODIFICACIONES RECHAZADAS POR PROCESAR --->",
                      num_rechazados
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 01,"NUMERO DE MODIFICACIONES PENDIENTES POR RENAPO   --->",
                      num_renapo
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT   
            COLUMN 01,"TOTAL DE REGISTROS PROCESADOS                  ----->",
                      num_total_afi
         
END REPORT
