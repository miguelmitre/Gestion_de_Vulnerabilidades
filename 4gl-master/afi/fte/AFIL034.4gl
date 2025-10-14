#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.        			                    #
#Programa AFIL034  => RESULTADOS DEL PROCESO DE DISPERSION CURP STATUS 160  #
#Fecha             => 6 DE OCTUBRE DE 2000.                                 # 
#Por               => MAURO MUNIZ CABALLERO                                 #
#Modificado por    => FERNANDO HERRERA HERNANDEZ                            #
#Fecha             => 5 DE AGOSTO DE 2003. VERSION 2                        #
#Sistema           => AFI 					            #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg RECORD
        nss       CHAR(11),
	paterno   CHAR(40),
	materno   CHAR(40),
	nombres   CHAR(40),
	tip_prob  SMALLINT,
	fol_prob  CHAR(10),
	doc_prob  CHAR(16),
	fentcons  DATE,
	cod_oper  CHAR(2),
	des_oper  CHAR(9),
	diag_pro  CHAR(8),
	des_rech  CHAR(30),
	st_renapo CHAR(2),
        caja      SMALLINT,
        posicion  SMALLINT,
	st_int    SMALLINT,
	nom_arch  CHAR(20),
	nom_comp  CHAR(120)
    END RECORD

    DEFINE 
        HOY            DATE,
        fecha_lote     DATE,
        HORA           CHAR(8),
        c_8_lote       CHAR(8),
        generar        CHAR(20)
       
    DEFINE  
	bnd_titulo     ,
	bnd_verif      ,
	cod_ant        ,
	diag_ant       ,
	accion         SMALLINT,
        num_acep_160   ,
        num_rech_160   ,
        num_aceptados  ,
        num_rechazados ,
        num_total_afi  INTEGER

    DEFINE 
        enter          CHAR(1),
        g_usuario      CHAR(8),
        v_nom_arch     CHAR(20) ,
        v_titulo       CHAR(50) ,
        G_LISTA        CHAR(100),
        D_LISTA        CHAR(100),
        g_imprime      CHAR(100)

    DEFINE g_afore         RECORD LIKE tab_afore.*
    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*
    DEFINE g_afore_local   RECORD LIKE tab_afore_local.*

END GLOBALS

MAIN

    DEFER INTERRUPT 
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY CONTROL-I,
        COMMENT LINE LAST

    CALL STARTLOG("AFIL034.log")
    CALL inicio()     		  #i

    IF accion THEN
	CALL imprime_informe()    #ii
    ELSE
        CALL proceso_principal()  # pp
    END IF

END MAIN

FUNCTION inicio()
#i---------------

    LET accion  = ARG_VAL(1)
    --LET generar = ARG_VAL(2)
    LET HOY     = TODAY
    LET HORA    = TIME

    SELECT *,USER 
    INTO   g_afore.*,g_usuario 
    FROM   tab_afore
    WHERE  @marca = 1

    SELECT @razon_social
    INTO   g_afore_local.razon_social
    FROM   tab_afore_local

    SELECT * 
    INTO   g_seg_modulo.* 
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    LET G_LISTA = g_seg_modulo.ruta_listados CLIPPED,"/",
                  g_usuario CLIPPED,
                  ".RESULTADO_DISP_CURP." CLIPPED,
                  HOY USING "DDMMYY","_",HORA[1,2], HORA[4,5]
    LET D_LISTA = G_LISTA CLIPPED

    SELECT campo8 
    INTO   c_8_lote
    FROM   safre_tmp:encabezado1_curp

    LET fecha_lote = MDY(c_8_lote[5,6],c_8_lote[7,8],c_8_lote[1,4])

    LET num_aceptados  = 0
    LET num_rechazados = 0

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW AFIL0341 AT 4,4 WITH FORM "AFIL0341" ATTRIBUTE(BORDER)
    DISPLAY " AFIL034       REPORTE DEL RESULTADO DE DISPERSION DE CURP                     " AT 3,1 ATTRIBUTE(REVERSE)
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

    PROMPT D_LISTA FOR enter
    DISPLAY "PROCESANDO INFORMACION " AT 18,3 ATTRIBUTE(REVERSE)  

    CALL imprime_informe()  #ii

    PROMPT "Proceso finalizado, presione [Enter] para salir " 
    FOR enter

    CLOSE WINDOW AFIL0341

END FUNCTION

FUNCTION imprime_informe()
#ii-----------------------

    LET bnd_titulo = 0
    LET bnd_verif  = 0
    LET cod_ant    = 0
    LET diag_ant   = 0

    SELECT count(*), ar.nom_arch
    INTO   num_acep_160, v_nom_arch
    FROM   safre_tmp:afi_renapo ar
    WHERE  ar.st_int = 160
    AND    ar.cod_oper = '01'
    GROUP BY 2

    IF num_acep_160 IS NULL THEN
        LET num_acep_160 = 0
    END IF

    SELECT count(*)
    INTO   num_rech_160
    FROM   safre_tmp:afi_renapo
    WHERE  st_int = 160
    AND    cod_oper = '02'

    IF num_rech_160 IS NULL THEN
        LET num_rech_160 = 0
    END IF

    SELECT count(*)
    INTO   num_aceptados
    FROM   safre_tmp:afi_renapo
    WHERE  st_int <> 160
    AND    cod_oper = '01'

    IF num_aceptados IS NULL THEN
        LET num_aceptados = 0
    END IF

    SELECT count(*)
    INTO   num_rechazados
    FROM   safre_tmp:afi_renapo
    WHERE  st_int <> 160
    AND    cod_oper = '02'

    IF num_rechazados IS NULL THEN
        LET num_rechazados = 0
    END IF

    START REPORT listado TO G_LISTA

    --- Registros st_int 160 y cod. oper. 01

    DECLARE cur1 CURSOR FOR
    SELECT *
    FROM   safre_tmp:afi_renapo
    WHERE  st_int = 160
    AND    cod_oper = '01'

    LET reg.des_oper = 'ACEPTADA'

    FOREACH cur1 INTO reg.nss       ,
	              reg.paterno   ,
	              reg.materno   ,
	              reg.nombres   ,
	              reg.tip_prob  ,
	              reg.fol_prob  ,
	              reg.doc_prob  ,
	              reg.fentcons  ,
	              reg.cod_oper  ,
	              reg.diag_pro  ,
	              reg.des_rech  ,
	              reg.st_renapo ,
	              reg.caja      ,
	              reg.posicion  ,
	              reg.st_int    ,
		      reg.nom_arch

        LET reg.nom_comp = reg.paterno CLIPPED,' ',
                           reg.materno CLIPPED,' ',
                           reg.nombres CLIPPED

        LET bnd_titulo = bnd_titulo + 1

	IF bnd_titulo = 1 THEN
            LET v_titulo = 'REGISTROS CON STATUS INTERNO 160 ACEPTADOS'
        ELSE
	    LET v_titulo = ''
        END IF

        OUTPUT TO REPORT listado(reg.*)

        END FOREACH

	LET bnd_titulo = 0

    --- Registros st_int 160 y cod. oper. 02

    DECLARE cur2 CURSOR FOR
    SELECT *
    FROM   safre_tmp:afi_renapo
    WHERE  st_int = 160
    AND    cod_oper = '02'

    LET reg.des_oper = 'RECHAZADA'

    FOREACH cur2 INTO reg.nss       ,
	              reg.paterno   ,
	              reg.materno   ,
	              reg.nombres   ,
	              reg.tip_prob  ,
	              reg.fol_prob  ,
	              reg.doc_prob  ,
	              reg.fentcons  ,
	              reg.cod_oper  ,
	              reg.diag_pro  ,
	              reg.des_rech  ,
	              reg.st_renapo ,
	              reg.caja      ,
	              reg.posicion  ,
	              reg.st_int    ,
		      reg.nom_arch

        LET reg.nom_comp = reg.paterno CLIPPED,' ',
                           reg.materno CLIPPED,' ',
                           reg.nombres CLIPPED

        LET bnd_titulo = bnd_titulo + 1

	IF bnd_titulo = 1 THEN
            LET v_titulo = 'REGISTROS CON STATUS INTERNO 160 RECHAZADOS'
        ELSE
	    LET v_titulo = ''
        END IF

        OUTPUT TO REPORT listado(reg.*)

        END FOREACH

	LET bnd_titulo = 0

    --- Registros st_int <> 160 y cod. oper. 02

    DECLARE cur3 CURSOR FOR
    SELECT *
    FROM   safre_tmp:afi_renapo
    WHERE  st_int <> 160
    AND    cod_oper = '02'

    LET reg.des_oper = 'RECHAZADA'

    FOREACH cur3 INTO reg.nss       ,
	              reg.paterno   ,
	              reg.materno   ,
	              reg.nombres   ,
	              reg.tip_prob  ,
	              reg.fol_prob  ,
	              reg.doc_prob  ,
	              reg.fentcons  ,
	              reg.cod_oper  ,
	              reg.diag_pro  ,
	              reg.des_rech  ,
	              reg.st_renapo ,
	              reg.caja      ,
	              reg.posicion  ,
	              reg.st_int    ,
		      reg.nom_arch

        LET reg.nom_comp = reg.paterno CLIPPED,' ',
                           reg.materno CLIPPED,' ',
                           reg.nombres CLIPPED

        IF reg.st_renapo <> 16 AND
           reg.st_renapo <> 17 AND 
           reg.st_renapo <> 18 THEN
            LET bnd_titulo = bnd_titulo + 1

	    IF bnd_titulo = 1 THEN
                LET v_titulo = 'REGISTROS CON RESPUESTA PROCESAR RECHAZADOS'
            ELSE
	        LET v_titulo = ''
            END IF

            OUTPUT TO REPORT listado(reg.*)
        END IF

        END FOREACH

	LET bnd_titulo = 0

        FINISH REPORT listado

        LET G_LISTA = "chmod 777 ",g_seg_modulo.ruta_listados CLIPPED,"/",
                      g_usuario CLIPPED,
                      ".RESULTADO_DISP_CURP." CLIPPED,
                      HOY USING "DDMMYY","_",HORA[1,2], HORA[4,5]

        RUN G_LISTA

        LET g_imprime = "lp ",g_seg_modulo.ruta_listados CLIPPED,"/",
                        g_usuario CLIPPED,
                        ".RESULTADO_DISP_CURP." CLIPPED,
                        HOY USING "DDMMYY","_",HORA[1,2], HORA[4,5]

        RUN g_imprime


END FUNCTION

REPORT listado(reg)
#----------------------

    DEFINE reg RECORD
        nss       CHAR(11),
	paterno   CHAR(40),
	materno   CHAR(40),
	nombres   CHAR(40),
	tip_prob  SMALLINT,
	fol_prob  CHAR(10),
	doc_prob  CHAR(16),
	fentcons  DATE,
	cod_oper  CHAR(2),
	des_oper  CHAR(9),
	diag_pro  CHAR(8),
	des_rech  CHAR(30),
	st_renapo CHAR(2),
        caja      SMALLINT,
        posicion  SMALLINT,
	st_int    SMALLINT,
	nom_arch  CHAR(20),
        nom_comp  CHAR(120)
    END RECORD

    DEFINE
        razon_social     CHAR(40) ,
        nombre           CHAR(40) ,
        des_resultado    CHAR(10) ,
        lnombres	 CHAR(40) ,
  	lpaterno	 CHAR(40) ,
	lmaterno	 CHAR(40) ,
        lnom_comp	 CHAR(120) 


    OUTPUT
	PAGE LENGTH   90
	LEFT MARGIN   0
	RIGHT MARGIN  0
	TOP MARGIN    0
	BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER     
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT
            COLUMN 01,"=================================================",
            COLUMN 50,"==================================================",
            COLUMN 100,"==================================================",
	    COLUMN 150,"============================="
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT
            COLUMN 03,g_afore_local.razon_social ,
            COLUMN 20,"LISTADO DE DISPERSION DE CURP ",
            COLUMN 100,"FECHA   :",hoy USING "DD/MM/YYYY",
            COLUMN 130,"NUM.PAG.:",pageno USING "####,###"
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT
            COLUMN 54,"PROCESO RESPUESTA DE RENAPO ", 
            COLUMN 100,"NOMBRE LOTE: ",v_nom_arch
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT
            COLUMN 01,"=================================================",
            COLUMN 50,"==================================================",
            COLUMN 100,"==================================================",
	    COLUMN 150,"============================="

        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"--------------------------------------------------",
            COLUMN 100,"--------------------------------------------------",
	    COLUMN 150,"-----------------------------"
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT
            COLUMN 003,"NSS",
            COLUMN 015,"NOMBRE RENAPO",
            COLUMN 056,"NOMBRE MAESTRO DE AFILIADOS",
            COLUMN 097,"DOCUMENTO PROBATORIO"        ,
            COLUMN 127,"F.CERT."     ,
            COLUMN 97,"RESPUESTA" ,
            COLUMN 138,"DIAGNOSTICO" ,
            COLUMN 191,"ST.RENAPO" ,
            COLUMN 201,"CAJA " ,
            COLUMN 207,"POSICION " 
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"--------------------------------------------------",
            COLUMN 100,"--------------------------------------------------",
	    COLUMN 150,"-----------------------------"

    ON EVERY ROW

        SELECT @nombres, @paterno, @materno, @fentcons
          INTO lnombres, lpaterno, lmaterno, reg.fentcons
          FROM afi_mae_afiliado
         WHERE @n_seguro = reg.nss
        LET lnom_comp = lnombres CLIPPED, " ",
                        lpaterno CLIPPED, " ",
                        lmaterno CLIPPED 

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT   
            COLUMN 064,v_titulo
        PRINT   
            COLUMN 003,reg.nss,
            COLUMN 015,reg.nom_comp[1,40],
            COLUMN 056,lnom_comp[1,40],
            COLUMN 097,reg.tip_prob USING '&',
            COLUMN 099,reg.fol_prob,
            COLUMN 110,reg.doc_prob,
            COLUMN 127,reg.fentcons USING 'DD/MM/YYYY',
            COLUMN 138,reg.cod_oper,
            COLUMN 141,reg.des_oper,
            COLUMN 151,reg.diag_pro,
            COLUMN 160,reg.des_rech,
            COLUMN 191,reg.st_renapo ,
            COLUMN 201,reg.caja USING '----&',
            COLUMN 207,reg.posicion USING '----&'
    ON LAST ROW

        LET num_total_afi = num_aceptados + num_rechazados +
                            num_acep_160  + num_rech_160 

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
COLUMN 01,"NUMERO DE MODIFICACIONES ACEPTADAS POR RENAPO (ST. INT. 160)  --->",
                      num_acep_160 
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT   
COLUMN 01,"NUMERO DE MODIFICACIONES RECHAZADAS POR RENAPO (ST. INT. 160) --->",
                      num_rech_160 
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT   
COLUMN 01,"NUMERO DE DISPERSIONES ACEPTADAS POR RENAPO                   --->",
                      num_aceptados 
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT   
COLUMN 01,"NUMERO DE DISPERSIONES RECHAZADAS POR RENAPO                  --->",
                      num_rechazados
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'  
        PRINT   
COLUMN 01,"TOTAL DE REGISTROS PROCESADOS                                 --->",
                      num_total_afi
         
END REPORT

