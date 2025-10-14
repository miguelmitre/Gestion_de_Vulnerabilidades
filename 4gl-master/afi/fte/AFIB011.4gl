##############################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                            #
#Propietario       => E.F.P.                                                 #
#Programa AFIB011  => GENERA ARCHIVO DOMICILIOS TRABAJADORES CURP ASIGNADA   #
#                  => AFORE CEDENTE                                          #
#Por               => MAURO MUNIZ CABALLERO                                  #
#Fecha creacion    => 21 DE JUNIO DE 2002                                    #
#Actualizacion     =>                                                        #
#Fecha actualiz.   =>                                                        #
#Sistema           => AFI                                                    #
##############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg_cza_dom RECORD
         clave_dependencia    CHAR(5) ,
         anyo                 CHAR(2) ,
         dia_juliano          CHAR(3) ,
         consecutivo          CHAR(1) ,
         tipo_movimiento      CHAR(1) ,
         n_registros          INTEGER  ,
         fecha_creacion       CHAR(08) 
    END RECORD

    DEFINE reg_det_dom RECORD
         n_unico              CHAR(18) ,
         n_seguro             CHAR(11) ,
         calle                CHAR(40) ,
         colonia              CHAR(25) ,
         codigo_postal        CHAR(05) ,
         ciudad               CHAR(35) ,
         entidad              CHAR(30) ,
         pais                 CHAR(20) ,
         folio_prob           CHAR(10) 
    END RECORD

    DEFINE reg_det_mae RECORD
         n_unico              CHAR(18) ,
         n_seguro             CHAR(11) ,
         calle                CHAR(40) ,
         numero               CHAR(10) ,
         depto                CHAR(10) ,
         colonia              CHAR(60) ,
         codigo_postal        CHAR(05) ,
         ciudad               SMALLINT ,
         entidad              SMALLINT ,
         pais                 CHAR(3)  ,
         folio_prob           DECIMAL(10,0)
    END RECORD

    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE 
        fecha_envio         ,
        HOY                 ,
        vfecha_creacion     DATE
     
    DEFINE 
        enter    	CHAR(1) ,
        dia_juliano     CHAR(3) ,
        anyo_envio      CHAR(4) ,
        HORA            CHAR(5) , 
        c5_HORA         CHAR(5) ,
        c8_HOY          CHAR(8) ,
        c8_presenta     CHAR(8) ,
        c10_HOY         CHAR(10) ,
        c10_emision     CHAR(10) ,
        cat             CHAR(300) ,
        g_cza           CHAR(100) ,
        g_det           CHAR(100) ,
        g_sum           CHAR(100) 

    DEFINE 
        dia_ene              ,
        dia_feb              ,
        dia_mar              ,
        dia_abr              ,
        dia_may              ,
        dia_jun              ,
        dia_jul              ,
        dia_ago              ,
        dia_sep              ,
        dia_oct              ,
        dia_nov              ,
        dia_dic              ,
        dia_julio             ,
        sw_1                  ,
        s_lotes_num           ,
        s_lotes_correlativo   ,
        s_codigo_afore        SMALLINT

    DEFINE 
        cont_reg              DECIMAL(10,0)

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST  ,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG('AFIB011.log')
    CALL inicio() #i
    CALL proceso_principal()  #pp

END MAIN

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW AFIB0111 AT 4,4 WITH FORM "AFIB0111" ATTRIBUTE(BORDER)
    DISPLAY " AFIB011   GENERA ARCHIVO DOMICILIOS P/TRAB. C/CURP ASIG.                      " AT 3,1 ATTRIBUTE(REVERSE)   
    DISPLAY " < CTRL-C > Salir " AT 1,26              
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)

    DISPLAY "Ruta envio de archivo : ", g_seg_modulo.ruta_envio AT 5,9

    INPUT BY NAME fecha_envio WITHOUT DEFAULTS
        AFTER FIELD fecha_envio
            IF fecha_envio IS NULL THEN
                ERROR "LA FECHA DE ENVIO NO PUEDE SER NULA"
                NEXT FIELD fecha_envio
            END IF
        EXIT INPUT

        ON KEY ( INTERRUPT )
	    EXIT PROGRAM

    END INPUT                    

    WHILE TRUE
        PROMPT "ESTA SEGURO DE GENERAR EL PROCESO [S/N] : ? " FOR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                EXIT WHILE
            ELSE
                DISPLAY "PROCESO CANCELADO" AT 19,1 ATTRIBUTE(REVERSE) SLEEP 2
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

    DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    CALL genera_det_edo_cta()    #gdds
    CALL calcula_dia_juliano()   #cdj
    CALL genera_cza_edo_cta()    #gcds

    LET cat = "cat ",g_seg_modulo.ruta_envio CLIPPED,"/CDC ",
                     g_seg_modulo.ruta_envio CLIPPED,"/DDC > ",
                     g_seg_modulo.ruta_envio CLIPPED,"/",
                     "DIR_RENAPO.",HOY USING"YYYYMMDD"

    RUN cat

    PROMPT "Total de registros enviados : ", cont_reg ,", presione <enter> "
    FOR enter
    DISPLAY " PROCESO FINALIZADO     " AT 19,1 ATTRIBUTE(REVERSE)
    SLEEP 1
    CLOSE WINDOW AFIB0111

END FUNCTION

FUNCTION inicio()
#i-------------

    LET HOY     = TODAY
    LET HORA    = TIME
    LET fecha_envio = HOY

    SELECT codigo_afore
    INTO   s_codigo_afore
    FROM   tab_afore_local

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    LET dia_ene = 31
    IF YEAR(fecha_envio) MOD 4 = 0 THEN
        LET dia_feb = 29
    ELSE
        LET dia_feb = 28
    END IF                                                   
    LET dia_mar = 31
    LET dia_abr = 30
    LET dia_may = 31
    LET dia_jun = 30
    LET dia_jul = 31
    LET dia_ago = 31
    LET dia_sep = 30
    LET dia_oct = 31
    LET dia_nov = 30
    LET dia_dic = 31

END FUNCTION

FUNCTION calcula_dia_juliano()
#cdj--------------------------

    DEFINE dia_mes   SMALLINT

    LET dia_mes = DAY(fecha_envio)

    CASE MONTH(fecha_envio)
        WHEN 1 
            LET dia_julio = dia_mes
        WHEN 2 
            LET dia_julio = dia_ene + dia_mes
        WHEN 3 
            LET dia_julio = dia_ene + dia_feb + dia_mes
        WHEN 4 
            LET dia_julio = dia_ene + dia_feb + dia_mar + dia_mes
        WHEN 5 
            LET dia_julio = dia_ene + dia_feb + dia_mar +
                            dia_abr + dia_mes
        WHEN 6 
            LET dia_julio = dia_ene + dia_feb + dia_mar +
                            dia_abr + dia_may + dia_mes
        WHEN 7 
            LET dia_julio = dia_ene + dia_feb + dia_mar +
                            dia_abr + dia_may + dia_jun + dia_mes
        WHEN 8 
            LET dia_julio = dia_ene + dia_feb + dia_mar +
                            dia_abr + dia_may + dia_jun + 
                            dia_jul + dia_mes
        WHEN 9 
            LET dia_julio = dia_ene + dia_feb + dia_mar +
                            dia_abr + dia_may + dia_jun + 
                            dia_jul + dia_ago + dia_mes
        WHEN 10
            LET dia_julio = dia_ene + dia_feb + dia_mar +
                            dia_abr + dia_may + dia_jun + 
                            dia_jul + dia_ago + dia_sep + dia_mes
        WHEN 11 
            LET dia_julio = dia_ene + dia_feb + dia_mar +
                            dia_abr + dia_may + dia_jun + 
                            dia_jul + dia_ago + dia_sep + 
                            dia_oct + dia_mes
        WHEN 12 
            LET dia_julio = dia_ene + dia_feb + dia_mar +
                            dia_abr + dia_may + dia_jun + 
                            dia_jul + dia_ago + dia_sep + 
                            dia_oct + dia_nov + dia_mes
    END CASE

    LET dia_juliano = dia_julio USING "&&&"

END FUNCTION

FUNCTION genera_cza_edo_cta()
#gcds--------------------------

    LET anyo_envio = YEAR(fecha_envio)

    LET reg_cza_dom.clave_dependencia = "00641"
    LET reg_cza_dom.anyo              = anyo_envio[3,4]
    LET reg_cza_dom.dia_juliano       = dia_juliano 
    LET reg_cza_dom.consecutivo       = "1"
    LET reg_cza_dom.tipo_movimiento   = "D"
    LET reg_cza_dom.n_registros       = cont_reg
    LET reg_cza_dom.fecha_creacion    = fecha_envio USING "YYYYMMDD"

    LET g_cza = g_seg_modulo.ruta_envio CLIPPED,"/","CDC"

    START REPORT listado_1 TO g_cza                              
        OUTPUT TO REPORT listado_1(reg_cza_dom.*) #1
    FINISH REPORT listado_1

END FUNCTION

REPORT listado_1(reg_cza_dom)
#1---------------------------------

    DEFINE reg_cza_dom RECORD
         clave_dependencia    CHAR(5) ,
         anyo                 CHAR(2) ,
         dia_juliano          CHAR(3) ,
         consecutivo          CHAR(1) ,
         tipo_movimiento      CHAR(1) ,
         n_registros          INTEGER  ,
         fecha_creacion       CHAR(08) 
    END RECORD

    OUTPUT
        PAGE LENGTH 1
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW 
        PRINT
            COLUMN 01,reg_cza_dom.clave_dependencia ,
                      reg_cza_dom.anyo              ,
                      reg_cza_dom.dia_juliano       ,
                      reg_cza_dom.consecutivo       ,
                      reg_cza_dom.tipo_movimiento   ,
                      reg_cza_dom.n_registros       USING"&&&&&&&&", 
                      reg_cza_dom.fecha_creacion    

END REPORT

FUNCTION genera_det_edo_cta()
#gdds--------------------------

    DECLARE cur_1 CURSOR FOR
    SELECT a.n_unico, 
           a.n_seguro, 
           b.calle,
           b.numero,
           b.depto,
           b.colonia,
           b.codpos,
           b.ciudad,
           b.estado,
           a.nacionalidad,
           a.fol_prob 
    FROM   afi_mae_afiliado a, afi_domicilio b, afi_dispersa_curp c
    WHERE  a.n_seguro = b.nss
    AND    a.n_seguro = c.n_seguro
    AND    a.n_folio = b.n_folio
    AND    a.tipo_solicitud = b.tipo_solicitud
    AND    c.cod_operacion = '01'
    AND    c.marca_dom = 1
    AND    a.tip_prob <> '0'
    AND    b.marca_envio = 'X'

        LET cont_reg = 1

        LET g_det = g_seg_modulo.ruta_envio CLIPPED,"/","DDC"

    START REPORT listado_2 TO g_det
        FOREACH cur_1 INTO reg_det_mae.*

        LET reg_det_dom.n_unico      = reg_det_mae.n_unico
        LET reg_det_dom.n_seguro     = reg_det_mae.n_seguro
        LET reg_det_dom.calle        = reg_det_mae.calle  CLIPPED," ",
                                       reg_det_mae.numero CLIPPED," ",
                                       reg_det_mae.depto  CLIPPED
        LET reg_det_dom.colonia      = reg_det_mae.colonia CLIPPED
        LET reg_det_dom.codigo_postal= reg_det_mae.codigo_postal 
        LET reg_det_dom.folio_prob   = reg_det_mae.folio_prob USING "&&&&&&&&&&"

        SELECT ciudad_desc
        INTO   reg_det_dom.ciudad
        FROM   tab_ciudad
        WHERE  ciudad_cod = reg_det_mae.ciudad
        AND    estad_cod = reg_det_mae.entidad

        SELECT estad_desc
        INTO   reg_det_dom.entidad
        FROM   tab_estado
        WHERE  estad_cod = reg_det_mae.entidad

        IF reg_det_mae.pais = "MEX" THEN
            LET reg_det_dom.pais = "MEXICO"
        ELSE
            SELECT pais_desc
            INTO   reg_det_dom.pais
            FROM   tab_pais
            WHERE  pais_cod = reg_det_mae.pais
        END IF

        OUTPUT TO REPORT listado_2(reg_det_dom.*) #2  

        DISPLAY "Registros procesados : ", cont_reg AT 16,23

        LET cont_reg = cont_reg + 1                         

        UPDATE afi_dispersa_curp
        SET    marca_dom = 2
        WHERE  n_seguro = reg_det_mae.n_seguro        

        END FOREACH

        LET cont_reg = cont_reg - 1

    FINISH REPORT listado_2 #2

END FUNCTION

REPORT listado_2(reg_det_dom)
#2---------------------------------

    DEFINE reg_det_dom RECORD
         n_unico              CHAR(18) ,
         n_seguro             CHAR(11) ,
         calle                CHAR(40) ,
         colonia              CHAR(25) ,
         codigo_postal        CHAR(05) ,
         ciudad               CHAR(35) ,
         entidad              CHAR(30) ,
         pais                 CHAR(20) ,
         folio_prob           CHAR(10) 
    END RECORD

    OUTPUT
        PAGE LENGTH 1
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW 
        PRINT
            COLUMN 01,reg_det_dom.n_unico       ,
                     reg_det_dom.n_seguro      ,
                     7 SPACES                  ,
                     reg_det_dom.calle         ,
                     reg_det_dom.colonia       ,
                     reg_det_dom.codigo_postal ,
                     reg_det_dom.ciudad        ,
                     reg_det_dom.entidad       ,
                     reg_det_dom.pais          ,
                     reg_det_dom.folio_prob    

END REPORT

