#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.        			                    #
#Programa AFIL003  => RESULTADOS DEL PROCESO DE SUBIDA DE AFILIADOS         #
#Sistema           => AFI 					            #
#Autor             => MAURO MUNIZ CABALLERO                                 #
#Fecha             => 10 FEBRERO  1997                                      # 
#############################################################################
DATABASE safre_af
GLOBALS
        DEFINE reg RECORD
            marca                    SMALLINT  ,
            nss_solicitud            CHAR(11)  ,
            curp_solicitud           CHAR(18)  ,
            rfc_trabajador           CHAR(13)  ,
            paterno                  CHAR(40)  ,
            materno                  CHAR(40)  ,
            nombres                  CHAR(40)  ,
            clave_promotor           CHAR(10)  
        END RECORD
 
        DEFINE 
            HOY                      DATE,
            HORA                     CHAR(8)
       
        DEFINE  
            num_aceptados            ,
            num_rechazados           ,
            num_pendientes           ,
            num_total_afi            INTEGER

        DEFINE  aux_n_seguro             CHAR(11)
        DEFINE  g_afore			RECORD LIKE tab_afore_local.*
        DEFINE  g_usuario		CHAR(8)
        DEFINE  G_LISTA			CHAR(100)
        DEFINE  corr			CHAR(100)
        DEFINE  G_PARAMETRO		CHAR(2)
        DEFINE  g_paramgrales		RECORD LIKE glo_parametro.*
END GLOBALS

MAIN
    WHENEVER ERROR CONTINUE
        DROP TABLE afi_1
    WHENEVER ERROR STOP

    CREATE TABLE afi_1
    (
     marca                    SMALLINT  ,
     nss_solicitud            CHAR(11)  ,
     curp_solicitud           CHAR(18)  ,
     rfc_trabajador           CHAR(13)  ,
     paterno                  CHAR(40)  ,
     materno                  CHAR(40)  ,
     nombres                  CHAR(40)  ,
     clave_promotor           CHAR(10)  
    )
     
    CALL init()

    DECLARE cur2 CURSOR FOR
    SELECT 1                    ,
           nss_solicitud        ,
           curp_solicitud       ,
           rfc_trabajador       , 
           paterno              , 
           materno              , 
           nombres              ,
           clave_promotor        
    FROM   safre_tmp:detalle1 

    IF STATUS = NOTFOUND THEN
        EXIT PROGRAM
    END IF
    
    FOREACH cur2 INTO reg.*
        INSERT INTO afi_1 VALUES(reg.*)
    END FOREACH

    DECLARE cur3 CURSOR FOR
    SELECT n_seguro
    FROM   afi_rechaza_cert
    WHERE  f_rechazo = TODAY

    FOREACH cur3 INTO aux_n_seguro    
        UPDATE afi_1
        SET    marca = 2
        WHERE  nss_solicitud = aux_n_seguro    
    END FOREACH

	SELECT *,USER INTO g_afore.*,g_usuario FROM tab_afore_local
	SELECT * INTO g_paramgrales.* FROM glo_parametro
    DECLARE cur4 CURSOR FOR
    SELECT *
    FROM   afi_1
    ORDER BY marca

	LET G_LISTA = g_paramgrales.ruta_spool CLIPPED,"/",g_usuario CLIPPED,".TRASP_AL_MAESTRO." CLIPPED,HOY USING "dd-mm-yy","_",HORA CLIPPED
    START REPORT listado TO G_LISTA
        FOREACH cur4 INTO reg.*
            OUTPUT TO REPORT listado(reg.*)
        END FOREACH
    FINISH REPORT listado
	LET G_LISTA = "chmod 777 ",g_paramgrales.ruta_spool CLIPPED,"/",g_usuario CLIPPED,".TRASP_AL_MAESTRO." CLIPPED,HOY USING "dd-mm-yy","_",HORA CLIPPED
	RUN G_LISTA
       # LET corr = "fglgo AFIL027 AFORE" CLIPPED 
        #RUN corr
END MAIN

FUNCTION init()
#-------------
    LET HOY  = TODAY
    LET HORA  = TIME
END FUNCTION

REPORT listado(reg)
#------------------
    DEFINE reg RECORD
        marca                    SMALLINT  ,
        nss_solicitud            CHAR(11)  ,
        curp_solicitud           CHAR(18)  ,
        rfc_trabajador           CHAR(13)  ,
        paterno                  CHAR(40)  ,
        materno                  CHAR(40)  ,
        nombres                  CHAR(40)  ,
        clave_promotor           CHAR(10)
    END RECORD

    DEFINE
        razon_social             CHAR(40)  ,
        nombre                   CHAR(40)  ,
        des_resultado            CHAR(10)

    OUTPUT
	PAGE LENGTH 60
	LEFT MARGIN   0
	RIGHT MARGIN  0
	TOP MARGIN    0
	BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER     
        SELECT A.razon_social
        INTO   razon_social
        FROM   tab_afore_local A

        PRINT
            COLUMN 01,"=================================================",
            COLUMN 50,"============================="
        PRINT
            COLUMN 03,razon_social                       ,
            COLUMN 60,"FECHA   :",hoy USING "DD/MM/YY"
        PRINT
            COLUMN 20,"PROCESO DE RECEPCION DE AFILIADOS", 
            COLUMN 60,"NUM.PAG.:",pageno USING "####,###"
        PRINT
            COLUMN 01,"=================================================",
            COLUMN 50,"============================="

    BEFORE GROUP OF reg.marca
        IF reg.marca = 1 THEN
            LET des_resultado = "ACEPTADOS"
        ELSE
            LET des_resultado = "RECHAZADOS"
        END IF

        PRINT
        PRINT
        PRINT
            COLUMN 20,"LISTADO DE AFILIADOS ",des_resultado
        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"-----------------------------"
        PRINT
        PRINT
            COLUMN 01,"NSS"        ,
            COLUMN 14,"CURP"       ,
            COLUMN 32,"RFC"        ,
            COLUMN 47,"NOMBRE"
        PRINT
            COLUMN 01,"CLA.PROMOTOR"
        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"-----------------------------"

    ON EVERY ROW
        LET nombre = reg.paterno CLIPPED," ",
                     reg.materno CLIPPED," ",
                     reg.nombres CLIPPED

        PRINT   
            COLUMN 01,reg.nss_solicitud       ,
            COLUMN 14,reg.curp_solicitud      ,
            COLUMN 27,reg.rfc_trabajador      ,
            COLUMN 47,nombre                 CLIPPED
        PRINT   
            COLUMN 01,reg.clave_promotor

    ON LAST ROW
        SELECT COUNT(*)
        INTO   num_aceptados
        FROM   safre_tmp:detalle1
        WHERE  cod_operacion = "01"

        SELECT COUNT(*)
        INTO   num_rechazados
        FROM   safre_tmp:detalle1
        WHERE  cod_operacion = "02"

        SELECT COUNT(*)
        INTO   num_pendientes
        FROM   safre_tmp:detalle1
        WHERE  cod_operacion ="03"

       # SELECT COUNT(*)
        #INTO   num_rechazados
        #FROM   afi_1
        #WHERE  marca = 2
	LET num_total_afi = 0

        LET num_total_afi = num_aceptados + num_rechazados + num_pendientes

        PRINT
        PRINT
        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"-----------------------------"
        PRINT   
            COLUMN 01,"NUMERO DE AFILIADOS ACEPTADOS  ----->",num_aceptados 
        PRINT   
            COLUMN 01,"NUMERO DE AFILIADOS PENDIENTES ----->",num_pendientes 
        PRINT   
            COLUMN 01,"NUMERO DE AFILIADOS RECHAZADOS ----->",num_rechazados
        PRINT   
            COLUMN 01,"TOTAL DE REGISTROS PROCESADOS  ----->",num_total_afi
         
END REPORT
