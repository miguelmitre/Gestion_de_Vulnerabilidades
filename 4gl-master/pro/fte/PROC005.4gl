################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
#Sistema           => PRO                                                      #
#Programa PROC005  => GENERA ENCABEZADO Y SUMARIO DE TRANSACCIONES             #
#Fecha creacion    => 18 DE OCTUBRE DE 1999                                    #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha actualiz.   => 29 DE NOVIEMBRE DEL 2006                                 #
#Actualizacion     => LAURA EUGENIA CORTES GUZMAN                              #
#Modificado Por    => Isabel Fonseca Frias                                     #
#Fecha             => 9 de Febrero del 2008                                    #
#Observacion       => Se agregaron modificaciones de acuerdo a MPT version     #
#                  => 3.0   (v1)                                               #
#CUO               => FSR se agrega detalle 302 05/08/2015                     #
#PST-1881          => FSR Se agrega remplazo de Ñ por #                        #
################################################################################
DATABASE safre_af 
GLOBALS
    DEFINE parametro           RECORD LIKE seg_modulo.*,

           reg_1 RECORD 
               fecha_genera          LIKE pro_ctr_envio.fecha_genera     ,
               tipo_operacion        LIKE pro_ctr_envio.tipo_operacion   ,
               nro_de_registros      LIKE pro_ctr_envio.nro_de_registros
           END RECORD,

           reg_2 RECORD 
               procesado             LIKE pro_estado.estado ,
               enviado               LIKE pro_estado.estado
           END RECORD,

           reg_fecha  RECORD 
                fecha_genera          DATE   
           END RECORD,

            pos                   SMALLINT    ,

           HOY                   DATE,
           vfecha_genera         DATE,
           vfenvio               DATE ,

           ch                    CHAR(100) ,
           borra_lineas          CHAR(200) ,
           PRO_DET               CHAR(500) ,
           G_LISTA_CZA           CHAR(100) ,
           RUTA                  CHAR(100) ,
           nom_archivo           CHAR(300) ,
           enter                 CHAR(001) ,
           cat                   CHAR(300) ,
           v_ruta                CHAR(100) ,    
           ejecuta               CHAR(500) ,
           gc_usuario            CHAR(08)  ,  

           cont_de_registros     ,
           tot_registros         ,
           gl_lot_afore          INTEGER
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG(FGL_GETENV("USER")||".PROC005.log")
    --CALL ERRORLOG ("Versión :CUO")    
    
    CALL init() #i
    OPEN WINDOW proc0051 AT 4,4 WITH FORM "PROC0051" ATTRIBUTE(BORDER)

    DISPLAY "                           < CTRL-C > Sal",
            "ir                                    " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY " PROC005            GENERA LOTE PARA ENVIAR A ",
            "CONSAR                           " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY " NRO.LOTE   :",gl_lot_afore AT 18,1

    INPUT BY NAME nom_archivo WITHOUT DEFAULTS
        BEFORE FIELD nom_archivo

        WHILE TRUE
            PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
            IF enter MATCHES "[sSnN]" THEN
                IF enter MATCHES "[sS]" THEN

                    SELECT "OK"
                    FROM   pro_ctr_envio A
                    WHERE  A.estado = reg_2.procesado
                    GROUP BY 1

                    IF STATUS = NOTFOUND THEN
                        DELETE
                        FROM  pro_lote_envio
                        WHERE nro_lote = gl_lot_afore

                        PROMPT " NO HAY REGISTROS PARA ENVIAR...<ENTER> PARA",
                               " SALIR " FOR CHAR enter
                        EXIT PROGRAM
                    ELSE
                       DECLARE  cur_fecha CURSOR FOR
                         SELECT A.fecha_genera
                         FROM   pro_ctr_envio  A
                         WHERE  A.estado = reg_2.procesado     
        
                       FOREACH cur_fecha INTO reg_fecha.*
                          IF reg_fecha.fecha_genera <> HOY  THEN
                             DISPLAY "NO SE PUEDE GENERAR LOTE DE ARCHIVOS DE ",
                                     "DIAS DIFERENTES AL ACTUAL" AT 18,1
                             PROMPT  "PRESIONE <ENTER> PARA SALIR"  
                                     FOR CHAR enter
                             EXIT PROGRAM
                          END IF
                       END FOREACH

                       SELECT "OK" FROM   pro_ctr_lote 
                       WHERE  fecha_envio = HOY

                       IF STATUS <> NOTFOUND THEN
                          PROMPT " EL LOTE YA FUE GENERADO, NO PUEDE GENERAR",                                   " OTRO...<ENTER> PARA SALIR " FOR CHAR enter
                               EXIT PROGRAM
                       ELSE
                           INSERT INTO pro_lote_envio VALUES(gl_lot_afore,HOY)
                       END IF
                    END IF    
                    EXIT WHILE
     
                ELSE

                    PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR "
                    FOR CHAR enter
                    EXIT PROGRAM
                END IF
            END IF
        END WHILE

        DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

        CALL primer_paso()  #pp
        CALL segundo_paso() #sp
    END INPUT
    CLOSE WINDOW proc0051
END MAIN

FUNCTION segundo_paso()
#sp--------------------
    DEFINE sw_1                  SMALLINT
    DEFINE lc_archivo1           CHAR(50), 
           sql_text              CHAR(650)     

   #OBTENEMOS LA INFORMACION A GENERAR E IDENTIFICAMOS EL DETALLE 
     
    LET sql_text =  "  SELECT fecha_genera     ,",
                    "         tipo_operacion   ,",
                    "        CASE               ",
                    "         WHEN tipo_operacion= 'ALT' THEN '301' ",
                    "         WHEN tipo_operacion= 'REA' THEN '301' ",
                    "         WHEN tipo_operacion= 'REC' THEN '301' ",
                    "         WHEN tipo_operacion= 'REV' THEN '302' ",
                    "         WHEN tipo_operacion= 'BAJ' THEN '303'	",		
                    "         WHEN tipo_operacion= 'MOD' THEN '304'	",
                    "        END AS detalle,  ",		
                    "        nro_de_registros ",
                    " FROM   pro_ctr_envio    ",
                    " WHERE  estado = ",reg_2.procesado ,
                    " INTO TEMP pro_ctr_envio2 " CLIPPED

   PREPARE sel_item_stmt FROM sql_text   
   EXECUTE sel_item_stmt   


    DECLARE cur_1 CURSOR FOR
       SELECT A.fecha_genera     ,
              A.tipo_operacion   ,
              A.nro_de_registros
       FROM   pro_ctr_envio2 A
       ORDER BY detalle ASC
       
       LET PRO_DET       = NULL

       LET cont_de_registros = 0
       LET sw_1 = 0
    FOREACH cur_1 INTO reg_1.*
        LET cont_de_registros = cont_de_registros + reg_1.nro_de_registros

        CASE reg_1.tipo_operacion
            WHEN "ALT"
                    IF sw_1 = 0 THEN
                        LET sw_1 = 1
                        LET PRO_DET = PRO_DET CLIPPED," ",
                                      parametro.ruta_envio CLIPPED,
                                      "/",reg_1.tipo_operacion
                    END IF

            WHEN "REA"
                    IF sw_1 = 0 THEN
                        LET sw_1 = 1
                        LET reg_1.tipo_operacion = "ALT"
                        LET PRO_DET = PRO_DET CLIPPED," ",
                                      parametro.ruta_envio CLIPPED,
                                      "/",reg_1.tipo_operacion
                        LET reg_1.tipo_operacion = "REA"
                    END IF

            WHEN "REE"                                                --(v1)
                     LET PRO_DET = PRO_DET CLIPPED," ",               --(v1)
                                   parametro.ruta_envio CLIPPED,      --(v1)
                                   "/",reg_1.tipo_operacion           --(v1)

																														 
            WHEN "REC"
                    IF sw_1 = 0 THEN
                        LET sw_1 = 1
                        LET reg_1.tipo_operacion = "ALT"
                        LET PRO_DET = PRO_DET CLIPPED," ",
                                      parametro.ruta_envio CLIPPED,
                                      "/",reg_1.tipo_operacion
                        LET reg_1.tipo_operacion = "REC"
                    END IF

            WHEN "REV"
                     LET PRO_DET = PRO_DET CLIPPED," ",
                                   parametro.ruta_envio CLIPPED,
                                   "/",reg_1.tipo_operacion
  
            WHEN "MOD"
                     LET PRO_DET = PRO_DET CLIPPED," ",
                                   parametro.ruta_envio CLIPPED,
                                   "/",reg_1.tipo_operacion

            WHEN "BAJ"
                     LET PRO_DET = PRO_DET CLIPPED," ",
                                   parametro.ruta_envio CLIPPED,
                                   "/",reg_1.tipo_operacion
        END CASE

        UPDATE pro_ctr_envio
        SET    pro_ctr_envio.estado         = reg_2.enviado ,
               pro_ctr_envio.fenvio         = HOY           ,
               pro_ctr_envio.nro_lote       = gl_lot_afore
        WHERE  pro_ctr_envio.tipo_operacion = reg_1.tipo_operacion
        AND    pro_ctr_envio.estado         = reg_2.procesado

        CASE reg_1.tipo_operacion
            WHEN "ALT"
                UPDATE pro_solicitud
                SET    pro_solicitud.num_lote       = gl_lot_afore,
                       status_interno               = 3
                WHERE  pro_solicitud.fenvio         = reg_1.fecha_genera
                AND    pro_solicitud.status_interno = 1

          		UPDATE pro_certificado_prov
          		 SET   fenvio = HOY 
          		WHERE  operacion =301 
          		AND    folio =0
          		AND    fenvio is null
          		                
            WHEN "MOD"
              UPDATE pro_solicitud
                SET status_interno = 12
              WHERE  pro_solicitud.fenvio         = reg_1.fecha_genera
              AND    pro_solicitud.status_interno = 11
                
            WHEN "BAJ" 
                UPDATE pro_mae_promotor 
                SET    status_interno                  = 3
                WHERE  pro_mae_promotor.status_interno = 1
            WHEN "REA"
                UPDATE pro_solicitud
                SET    pro_solicitud.num_lote = gl_lot_afore
                WHERE  pro_solicitud.fenvio         = reg_1.fecha_genera
                AND    pro_solicitud.status_interno = 8
            WHEN "REE"                                                   --(v1)
                UPDATE pro_solicitud                                     --(v1)
                SET    pro_solicitud.num_lote = gl_lot_afore             --(v1)
                WHERE  pro_solicitud.fenvio         = reg_1.fecha_genera --(v1)
                AND    pro_solicitud.status_interno = 8                  --(v1)
            WHEN "REC"
                UPDATE pro_solicitud
                SET    pro_solicitud.num_lote = gl_lot_afore
                WHERE  pro_solicitud.fenvio         = reg_1.fecha_genera
                AND    pro_solicitud.status_interno = 21
           #CUO 
           WHEN "REV"
           	   UPDATE pro_capacitacion
           	   SET    pro_capacitacion.estado = 3
           	   WHERE  pro_capacitacion.estado = 2
           	   AND    pro_capacitacion.fecha_genera = HOY
          
          		UPDATE pro_certificado_prov
          		 SET   fenvio = HOY 
          		WHERE  operacion =302 
          		AND    folio =0
          		AND    fenvio is null
        END CASE
    END FOREACH

    INSERT INTO pro_ctr_lote VALUES (gl_lot_afore,HOY,"",cont_de_registros)

    
    LET lc_archivo1 = "paso_", gc_usuario
    LET nom_archivo = HOY USING"YYYYMMDD",".080"
    
    LET cat = "cat ",parametro.ruta_envio CLIPPED,"/CZATRAN ",
                     PRO_DET CLIPPED,
                     " > ",RUTA CLIPPED,"/",lc_archivo1
                     
    RUN cat
    
    LET cat = "cd ",parametro.ruta_envio CLIPPED,           
              "/;sed 's/Ñ/#/g' ",lc_archivo1 CLIPPED," >", nom_archivo CLIPPED
    
    RUN cat
    
    LET cat = "cd ",parametro.ruta_envio CLIPPED,
              "/;rm ", lc_archivo1
    
    RUN cat         

    LET v_ruta = parametro.ruta_envio

    LET ejecuta = "echo ", 
                  nom_archivo, 
                  " > ", 
                  v_ruta CLIPPED, 
                  "/rescate.080"
    RUN ejecuta            

    DISPLAY " EL LOTE HA SIDO GENERADO EN LA RUTA : " AT 5,12 
    DISPLAY RUTA CLIPPED AT 7,13
    DISPLAY " CON EL NOMBRE : " AT 9,12
         
    DISPLAY BY NAME nom_archivo
    DISPLAY " TOTAL DE REGISTROS : ",cont_de_registros USING "#####&" AT 14,1
    PROMPT " PRESIONE <ENTER> PARA FINALIZAR " FOR CHAR enter
END FUNCTION

FUNCTION init()
#i-------------
    LET HOY  = TODAY
    
    SELECT *, USER
    INTO   parametro.*, gc_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = "pro"

    LET RUTA = parametro.ruta_envio

    SELECT estado
    INTO   reg_2.procesado
    FROM   pro_estado
    WHERE  descripcion = "PROCESADO"

    SELECT estado
    INTO   reg_2.enviado
    FROM   pro_estado
    WHERE  descripcion = "ENVIADO"

    SELECT MAX(nro_lote) + 1
    INTO   gl_lot_afore
    FROM   pro_lote_envio

    IF gl_lot_afore IS NULL THEN
        LET gl_lot_afore = 1
    END IF
   
END FUNCTION

REPORT cza_tran()
#ct--------------
    DEFINE tot_301           SMALLINT,
           tot_302           SMALLINT,
           tot_303           SMALLINT,
           tot_304           SMALLINT,
           tot_308           SMALLINT,                             --(v1)
           fec_transm        CHAR(8) ,

           cve_afore         CHAR(03),
    
           clave_afore       LIKE tab_afore_local.codigo_afore

    OUTPUT
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0
        PAGE LENGTH   2

    FORMAT
    FIRST PAGE HEADER
        SELECT codigo_afore
        INTO   clave_afore
        FROM   tab_afore_local

        LET cve_afore   = clave_afore CLIPPED
        LET fec_transm  = HOY USING"YYYYMMDD"

        LET tot_301 = 0
        LET tot_302 = 0
        LET tot_303 = 0
        LET tot_304 = 0
        LET tot_308 = 0                                            --(v1)  

        SELECT sum(nro_de_registros) 
        INTO   tot_301
        FROM   pro_ctr_envio A
        WHERE  A.estado = reg_2.procesado
        AND    A.tipo_operacion IN('ALT','REA','REC')

        IF tot_301 IS NULL OR tot_301 = 0 THEN
           LET tot_301 = 0
        END IF

        SELECT sum(nro_de_registros) 
        INTO   tot_302
        FROM   pro_ctr_envio A
        WHERE  A.estado = reg_2.procesado
        AND    A.tipo_operacion = 'REV'

        IF tot_302 IS NULL OR tot_302 = 0 THEN
           LET tot_302 = 0
        END IF

        SELECT sum(nro_de_registros)
        INTO   tot_303
        FROM   pro_ctr_envio A
        WHERE  A.estado = reg_2.procesado
        AND    A.tipo_operacion = 'BAJ'

        IF tot_303 IS NULL OR tot_303 = 0 THEN
           LET tot_303 = 0
        END IF

        SELECT sum(nro_de_registros)
        INTO   tot_304
        FROM   pro_ctr_envio A
        WHERE  A.estado = reg_2.procesado
        AND    A.tipo_operacion = 'MOD'

        IF tot_304 IS NULL OR tot_304 = 0 THEN
           LET tot_304 = 0
        END IF

        SELECT sum(nro_de_registros)                              --(v1)
        INTO   tot_308                                            --(v1)
        FROM   pro_ctr_envio A                                    --(v1)
        WHERE  A.estado = reg_2.procesado                         --(v1) 
        AND    A.tipo_operacion = 'REE'                           --(v1)

        IF tot_308 IS NULL OR tot_308 = 0 THEN                    --(v1)
           LET tot_308 = 0                                        --(v1)
        END IF                                                    --(v1)


        PRINT COLUMN 001,'01'                      ,
              COLUMN 003,'000'                     ,
              COLUMN 006,'080'                     ,
              COLUMN 009,'01'                      ,
              COLUMN 011,cve_afore   CLIPPED       ,
              COLUMN 014,'03'                      ,
              COLUMN 016,'001'                     ,
              COLUMN 019,fec_transm                ,
              COLUMN 027,'001'                     ,
              COLUMN 030,tot_301 USING "&&&&&"     ,
              COLUMN 035,tot_302 USING "&&&&&"     ,
              COLUMN 040,tot_303 USING "&&&&&"     ,
              COLUMN 045,tot_304 USING "&&&&&"     ,
              COLUMN 050,tot_308 USING "&&&&&"     ,
              COLUMN 055,'  '                      ,
              COLUMN 057,'         '               ,
              COLUMN 066,835 SPACES
END REPORT

FUNCTION primer_paso()
#pp-------------------
###### GENERA ENCABEZADO LOTE DE TRANSACCIONES #######

    LET G_LISTA_CZA = parametro.ruta_envio CLIPPED,"/czatran"

    START REPORT cza_tran TO G_LISTA_CZA
        OUTPUT TO REPORT cza_tran()  #ct
    FINISH REPORT cza_tran

    LET ch = "chmod 777 ",G_LISTA_CZA
    RUN ch

    LET borra_lineas = "cd ",
                       RUTA CLIPPED,
                       ";sed -e '/^$/d' ",
                       G_LISTA_CZA CLIPPED,
                       " > CZATRAN"
    RUN borra_lineas

END FUNCTION
