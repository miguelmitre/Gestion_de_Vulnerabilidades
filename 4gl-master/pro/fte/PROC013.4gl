###############################################################################
#Proyecto          => SISTEMA DE AFORES ( MEXICO )                            #
#Programa PROC013  => GENERA ARCHIVO DE ENVIO DE SOLICITUDES DE EXAMEN PARA   #
#                     PROMOTORES (0802)                                       #
#Modulo            => PRO                                                     #
#Elaborado Por     => STEFANIE DANIELA VERA PIÑA                              #
#Fecha Elaboracion => 27-ABRIL-2007                                           #
###############################################################################
DATABASE safre_af
GLOBALS
    DEFINE 
        g_seg_modulo          RECORD LIKE seg_modulo.*  ,
        reg                   RECORD LIKE pro_ctr_examen.* 

    DEFINE reg_2 RECORD #reg_2
        capturado             LIKE pro_status_interno.status_interno,
        lote_generado         LIKE pro_status_interno.status_interno
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        borra_lineas          CHAR(500) ,
        ch                    CHAR(500) ,
        c7_nombre_plano       CHAR(007) ,
        c12_nombre_plano      CHAR(024) ,
        elimina               CHAR(500) ,
        elimina1              CHAR(500) ,
        G_LISTA_1             CHAR(500) ,
        G_LISTA_2             CHAR(500) ,
        G_LISTA               CHAR(500) ,
        enter                 CHAR(001) ,
        vcve_solicitud        CHAR(013)

    DEFINE #glo #smallint
        s_codigo_afore        SMALLINT

    DEFINE #glo #integer
        ultimo_folio          ,
        cont_captu_ok         ,
        cont_reg              INTEGER
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I
	CALL STARTLOG ("PROC013.log")

    CALL init() #i
    OPEN WINDOW proc0131 AT 4,4 WITH FORM "PROC0131" ATTRIBUTE(BORDER)
    DISPLAY " PROC013   GENERA ARCHIVO DE ENVIO DE SOLICITUDES DE EXAMEN                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)

    SELECT "OK"
    FROM   pro_ctr_examen
    WHERE  estado_registro = reg_2.capturado
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        PROMPT" NO EXISTEN REGISTROS PARA PROCESAR...<ENTER> PARA SALIR "
        FOR CHAR enter
        EXIT PROGRAM
    END IF
    
    WHILE TRUE
        PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                EXIT WHILE
            ELSE
                PROMPT" PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

    LET cont_captu_ok = 0

    SELECT COUNT(*)
    INTO   cont_captu_ok
    FROM   pro_ctr_examen A
    WHERE  A.estado_registro = reg_2.capturado

    CALL primer_paso()  

 

    DISPLAY "TOTAL REGISTROS ENVIADOS          : ",cont_captu_ok    AT 07,19

    DISPLAY "EL ARCHIVO HA SIDO GENERADO EN LA RUTA : "  AT 09,19
    DISPLAY G_LISTA CLIPPED AT 10,19
    DISPLAY "CON EL NOMBRE : ",c12_nombre_plano AT 11,19

    DISPLAY " FOLIO NUMERO : ",ultimo_folio  AT 18,01
    PROMPT  " PROCESO FINALIZADO... <ENTER> PARA SALIR " FOR CHAR enter
    CLOSE WINDOW proc0131
END MAIN


FUNCTION init()
#--------------
    LET HOY     = TODAY

    SELECT codigo_afore   
    INTO   s_codigo_afore 
    FROM   tab_afore_local

    SELECT MAX(A.folio) + 1
    INTO   ultimo_folio
    FROM   glo_folio A

    INSERT INTO glo_folio VALUES (ultimo_folio)
    
    SELECT A.status_interno
    INTO   reg_2.capturado
    FROM   pro_status_interno A
    WHERE  A.desc_status_corta = "CAPTURADO"

    SELECT A.status_interno
    INTO   reg_2.lote_generado
    FROM   pro_status_interno A
    WHERE  A.desc_status_corta = "LOTE GENERADO"

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "pro"

    LET G_LISTA          = g_seg_modulo.ruta_envio
    LET c7_nombre_plano  = "DETALLE"
    LET c12_nombre_plano = HOY USING"YYYYMMDD","_","AF","_",s_codigo_afore USING"&&&","_","000",".0802"


    WHENEVER ERROR CONTINUE
        LET elimina = "rm -f ",g_seg_modulo.ruta_envio CLIPPED,"/",c12_nombre_plano

        RUN elimina
    WHENEVER ERROR STOP
END FUNCTION


FUNCTION primer_paso() 
#---------------------
    SELECT  "OK"
    FROM    pro_ctr_examen A
    WHERE   A.estado_registro  = reg_2.capturado
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        RETURN
    END IF

    DECLARE cur_1 CURSOR FOR
    SELECT  A.*
    FROM    pro_ctr_examen A
    WHERE   A.estado_registro  = reg_2.capturado

    LET G_LISTA_1 = G_LISTA CLIPPED,"/",c7_nombre_plano
                                             
    START REPORT listado_detalle TO G_LISTA_1
        LET cont_reg = 0

        FOREACH cur_1 INTO reg.*
            LET cont_reg = cont_reg + 1
            DISPLAY "TOTAL REGISTROS PROCESADOS: ",cont_reg AT 13,21
     
            OUTPUT TO REPORT listado_detalle(reg.*)

            LET vcve_solicitud = s_codigo_afore USING"&&&",
                                 reg.fecha_examen USING "DDMMYY",        
                                 reg.consecutivo USING"&&&&" 

            UPDATE pro_ctr_examen
            SET    estado_registro  = reg_2.lote_generado,
                   folio_envio      = ultimo_folio,
                   fecha_envio      = HOY,
                   cve_solicitud    = vcve_solicitud
            WHERE  consecutivo      = reg.consecutivo

                    
        END FOREACH

            INSERT INTO pro_envio_examen
            VALUES (ultimo_folio              ,
                   HOY                        ,
                   c12_nombre_plano           ,
                   HOY                        ,
                   cont_reg
                   ) 

    FINISH REPORT listado_detalle

    LET ch = "chmod 777",g_seg_modulo.ruta_envio CLIPPED,c12_nombre_plano
    run ch

    LET G_LISTA_2 = G_LISTA CLIPPED,"/",c12_nombre_plano
    
    LET borra_lineas = "sed -e '/^$/d' ",G_LISTA_1 CLIPPED,">",G_LISTA_2 CLIPPED
    run borra_lineas


    LET ch = "cp ",G_LISTA_2 CLIPPED," ",G_LISTA_1 CLIPPED

    RUN ch

    WHENEVER ERROR CONTINUE
        LET elimina1 = "rm -f ",g_seg_modulo.ruta_envio CLIPPED,"/",c7_nombre_plano

        RUN elimina1
    WHENEVER ERROR STOP
END FUNCTION


REPORT listado_detalle(reg_1)
#----------------------------
    DEFINE #loc #reg_1 
        reg_1                 RECORD LIKE pro_ctr_examen.*

    DEFINE
        vdesc_sede            LIKE pro_sede_examen.desc_sede,
        vedo_sede             LIKE pro_sede_examen.edo_sede

    OUTPUT
    PAGE LENGTH    1000
    LEFT MARGIN    0
    RIGHT MARGIN   0
    TOP MARGIN     0
    BOTTOM MARGIN  0

    FORMAT
        FIRST PAGE HEADER           

        LET cont_captu_ok = cont_captu_ok  + 1

        PRINT
            COLUMN 001,"000"                           ,#tipo registro
            COLUMN 004,"0802"                          ,#tipo archivo
            COLUMN 008,"001"                           ,#tipo entidad 
            COLUMN 011,s_codigo_afore USING"&&&"       ,#clave entidad 
            COLUMN 014,HOY            USING "YYYYMMDD" ,#fecha informacion    
            COLUMN 022,"337"                           ,#tamaño del registro
            COLUMN 025,cont_captu_ok  USING"&&&&&"     ,#numero de registros   
            COLUMN 030,308 SPACES

    ON EVERY ROW

        SELECT desc_sede,edo_sede
        INTO   vdesc_sede,vedo_sede
        FROM   pro_sede_examen
        WHERE  cod_sede = reg_1.sede 
 
        LET vdesc_sede = vdesc_sede CLIPPED

      
         PRINT
            COLUMN 001,"301"                               ,#tipo de registro
            COLUMN 004,s_codigo_afore USING"&&&"           ,#clave entidad
            COLUMN 007,reg_1.fecha_examen USING "DDMMYY"   ,#fecha aplica examen
            COLUMN 013,reg_1.consecutivo USING"&&&&"       ,#consecutivo 
            COLUMN 017,s_codigo_afore USING"&&&"           ,#clave entidad
            COLUMN 020,reg_1.cod_promotor                  ,
            COLUMN 030,reg_1.paterno                       ,
            COLUMN 070,reg_1.materno                       ,
            COLUMN 110,reg_1.nombres                       ,
            COLUMN 150,reg_1.curp                          ,
            COLUMN 168,reg_1.tpo_examen                    ,#tipo examen
            COLUMN 169,vdesc_sede                          ,#sede examen
            COLUMN 319,vedo_sede    USING"&&"              ,#clave estado
            COLUMN 321,reg_1.evento USING"&&"              ,#tipo evento
            COLUMN 323,reg_1.fecha_examen USING "YYYYMMDD" ,#fecha examen
            COLUMN 331,reg_1.hora_examen                   ,#hora examen
            COLUMN 335,reg_1.num_vuelta  USING"&&&"         #num vuelta 
END REPORT
