#################################################################################
#Proyecto          => SISTEMA DE AFORES (SAFRE)                                 #
#Owner             => E.F.P.                                                    #
#Programa RETC830  => GENERA NOTIFICACION DE APORTACIONES EXTEMPORANEAS (14)    #
#Fecha creacion    => 27 DE ABRIL DEL 2004                                      #
#By                => FRANCO ESTEBAN ULLOA VIDELA                               #
#Fecha actualiz.   => 18 DE NOVIEMBRE DE 2004                                   #
#Actualizacion     => JUAN CARLOS MENDOZA MORENO                                #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#Fecha Actualiza   => 2 DE MARZO DE 2011                                        #
#                  => Se rearregla el codigo del programa                       #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af
GLOBALS
    DEFINE
        g_seg_modulo          RECORD LIKE seg_modulo.*

    DEFINE gr_edo RECORD
        capturado             LIKE ret_estado.estado_solicitud ,
        enviado               LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_afore RECORD
        xxi                 SMALLINT    
    END RECORD

    DEFINE gr_cont  RECORD
        extemp      INTEGER,
        especial    INTEGER
    END RECORD

    DEFINE
        c12_nom_plano           CHAR(012) ,
        borra_lineas            CHAR(100) ,
        ch                      CHAR(100) ,
        G_LISTA_1               CHAR(100) ,
        G_LISTA_2               CHAR(100) ,
        enter                   CHAR(001) 

    DEFINE
        gs_procesa              ,
        gs_codigo_afore         SMALLINT

    DEFINE
        gi_tot_registros        INTEGER

    DEFINE #glo #date
        HOY                     DATE

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I ,
        MESSAGE LINE LAST -1

    CALL STARTLOG("RETC830.log")
    CALL init()

    IF f_inicia_proceso() = 1 THEN

        CALL f_obtiene_registros()
        
        LET G_LISTA_2    = g_seg_modulo.ruta_envio CLIPPED,"/",c12_nom_plano
        LET borra_lineas = "sed -e '/^$/d' ",G_LISTA_1 CLIPPED,">",
                           G_LISTA_2 CLIPPED

        RUN borra_lineas
        LET ch = "chmod 777 ",G_LISTA_2
        RUN ch
        
        DISPLAY " EL LOTE HA SIDO GENERADO EN LA RUTA : " AT 09,19 
        DISPLAY g_seg_modulo.ruta_envio CLIPPED AT 11,20
        DISPLAY "                                                " AT 13,11
        DISPLAY " CON EL NOMBRE : ",c12_nom_plano AT 13,19
        PROMPT " PRESIONE <ENTER> PARA FINALIZAR " FOR CHAR enter

    ELSE
        PROMPT" PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter     
    END IF 
    
    CLOSE WINDOW retc830

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    -- -----------------------------------------------------------------------------
    
    LET HOY              = TODAY
    LET gr_cont.extemp   = 0
    LET gr_cont.especial = 0
    
    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    ----- CODIGOS AFORES -----
    SELECT codigo_afore
    INTO   gs_codigo_afore 
    FROM   tab_afore_local

    SELECT afore_cod
    INTO   gr_afore.xxi 
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*XXI*"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   ret_estado A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    LET c12_nom_plano = HOY USING"YYYYMMDD",".014"
    LET G_LISTA_1     = g_seg_modulo.ruta_envio CLIPPED,"/PASO.014"

END FUNCTION

#---------------------------------------------------------------------------#
# f_inicia_proceso : Muestra la pantalla de inicio y determina si se        #
#                    ejecuta o no el proceso                                #
#---------------------------------------------------------------------------#
FUNCTION f_inicia_proceso()

    DEFINE 
        ls_flag             ,
        ls_tot_ext          ,
        ls_tot_esp          SMALLINT

    -- -----------------------------------------------------------------------------

    OPEN WINDOW retc830 AT 4,4 WITH FORM "RETC8301" ATTRIBUTE(BORDER)
    DISPLAY " RETC830                                                                       " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "   GENERA NOTIFICACION DE APORTACIONES EXTEMPORANEAS ( OPERACION 14 )           " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 1,65 ATTRIBUTE(REVERSE)

    LET ls_flag = 1

    -- Determina cuantas especiales y extemporanes existen por enviar
    SELECT COUNT(*)
    INTO   ls_tot_esp
    FROM   ret_especial     
    WHERE  fecha_envio IS NULL
    AND    estado_registro < gr_edo.enviado

    SELECT COUNT(*)
    INTO   ls_tot_ext
    FROM   ret_extemporanea
    WHERE  estado_registro < gr_edo.enviado

    IF ls_tot_ext = 0 AND ls_tot_esp = 0 THEN
        PROMPT " NO HAY INFORMACION A GENERAR, PRESIONE ENTER PARA CONTINUAR:"
        FOR CHAR enter
        LET ls_flag = 0
    ELSE
        DISPLAY "TOTAL DE REGISTROS A GENERAR" AT 8,17
        DISPLAY "ESPECIALES     : ", ls_tot_esp USING "###&" AT 10,29
        DISPLAY "EXTEMPORANEAS  : ", ls_tot_ext USING "###&" AT 11,29
   
        WHILE TRUE
            PROMPT "¿DESEA GENERAR EL ARCHIVO DE LA OP. 14? (S/N) " FOR CHAR enter
            IF enter MATCHES "[sSnN]" THEN
                IF enter MATCHES "[sS]" THEN
                    DISPLAY "                                                                 " AT 8,1
                    DISPLAY "                                                                 " AT 10,1
                    DISPLAY "                                                                 " AT 11,1
                    DISPLAY " EXTEMPORANEAS PROCESADAS : ", gr_cont.extemp   AT 06,19
                    DISPLAY "    ESPECIALES PROCESADAS : ", gr_cont.especial AT 07,19
                    
                    DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
                    LET ls_flag = 1
                ELSE
                    LET ls_flag = 0
                END IF
                
                EXIT WHILE
            END IF
        END WHILE
    END IF
    
    RETURN ls_flag

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_registros : Selecciona los registros para generar la oper 14    #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_registros()

    DEFINE lr_especial RECORD LIKE ret_especial.*
 
    DEFINE lr_registros RECORD
        nss                 LIKE ret_extemporanea.nss               ,
        consecutivo         LIKE ret_extemporanea.consecutivo_lote  ,
        sec_pension         CHAR(002) ,
        periodo_pago        CHAR(006) ,
        id_tramite          SMALLINT  
    END RECORD

    DEFINE li_ultimo_consec LIKE ret_consec_tramite.consec_tramite

    DEFINE
        consec_aux       ,          
        cont_aux         INTEGER

    -- -----------------------------------------------------------------------------

    LET cont_aux      = 0

    INITIALIZE lr_registros.*   TO NULL
    INITIALIZE li_ultimo_consec TO NULL

    DECLARE cur_1 CURSOR FOR
    SELECT  A.nss               ,
            A.consecutivo_lote  ,
            ""                  ,
            ""                  ,
            ""
    FROM    ret_extemporanea A
    WHERE   A.estado_registro  = gr_edo.capturado
    UNION ALL
    SELECT  B.nss               ,
            B.consecutivo_lote  ,
            ""                  ,         
            ""                  ,         
            ""             
    FROM    ret_especial B
    WHERE   B.consec_tramite = 0
    ORDER BY 1

    START REPORT rpt_oper_14 TO G_LISTA_1
        LET gi_tot_registros = 0
        FOREACH cur_1 INTO lr_registros.*
	    
            LET gi_tot_registros = gi_tot_registros + 1

            SELECT "OK"
            FROM   ret_extemporanea
            WHERE  nss              = lr_registros.nss
            AND    estado_registro  = gr_edo.capturado
            AND    consecutivo_lote = lr_registros.consecutivo
            GROUP BY 1

            -- Introducimos las aportaciones extemporaneas
            IF STATUS <> NOTFOUND THEN 

                SELECT id_tramite   ,
                       periodo_pago 
                INTO   lr_registros.id_tramite   ,
                       lr_registros.periodo_pago
                FROM   ret_extemporanea
                WHERE  nss              = lr_registros.nss
                AND    consecutivo_lote = lr_registros.consecutivo
                AND    estado_registro  = gr_edo.capturado
       
                SELECT MAX(sec_pension)
                INTO   lr_registros.sec_pension
                FROM   ret_det_datamart
                WHERE  nss                = lr_registros.nss
                AND    tipo_prestacion IN(0,1)
                AND    diag_datamart   IN(101,300,302,303)
                
                -- Cambio solicitado por Afore XXI. Todas las aportaciones extemporanes
                -- se reportaran como Especiales en el archivo de la op. 14
                IF gs_codigo_afore = gr_afore.xxi THEN
                    LET lr_registros.id_tramite = 2
                END IF                
                
                IF STATUS <> NOTFOUND THEN
                    OUTPUT TO REPORT rpt_oper_14(lr_registros.*) #ld
                END IF

                LET li_ultimo_consec = f_obtiene_ult_consec()

                UPDATE ret_extemporanea
                SET    fecha_envio      = HOY           ,
                       consec_tramite   = li_ultimo_consec ,
                       estado_registro  = gr_edo.enviado
                WHERE  nss              = lr_registros.nss
                AND    consecutivo_lote = lr_registros.consecutivo
                AND    estado_registro  = gr_edo.capturado

                UPDATE ret_aporte
                SET    consec_tramite   = li_ultimo_consec
                WHERE  nss              = lr_registros.nss 
                AND    consecutivo_lote = lr_registros.consecutivo
                AND    consec_tramite   = 0
            
                LET gr_cont.extemp = gr_cont.extemp + 1
                DISPLAY " EXTEMPORANEAS PROCESADAS : ", gr_cont.extemp   AT 06,19
            
            END IF    

            -- Introducimos las aportaciones especiales
            SELECT "OK"
            FROM   ret_especial
            WHERE  nss              = lr_registros.nss
            AND    consecutivo_lote = lr_registros.consecutivo
            AND    consec_tramite   = 0
            AND    estado_registro  = gr_edo.capturado
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                LET lr_registros.id_tramite   = 2
                LET lr_registros.periodo_pago = "000101"

                SELECT COUNT(*)
                INTO   cont_aux
                FROM   ret_especial
                WHERE  nss             = lr_registros.nss
                AND    consec_tramite  = 0
                AND    estado_registro = gr_edo.capturado
                 
                -- Existe mas de una aportacion especial para el nss
                IF cont_aux > 1 THEN
                    LET consec_aux = li_ultimo_consec
                    
                    DECLARE cur_4 CURSOR FOR
                    SELECT *
                    FROM   ret_especial 
                    WHERE  nss             = lr_registros.nss
                    AND    consec_tramite  = 0
                    AND    estado_registro = gr_edo.capturado

                    FOREACH cur_4 INTO lr_especial.*
                        LET lr_registros.sec_pension  = lr_especial.sec_pension
                        
                        LET li_ultimo_consec = f_obtiene_ult_consec()

                        UPDATE ret_especial 
                        SET    consec_tramite  = li_ultimo_consec ,
                               fecha_envio     = HOY ,
                               estado_registro = gr_edo.enviado
                        WHERE  nss             = lr_especial.nss
                        AND    sec_pension     = lr_registros.sec_pension
                        AND    consec_tramite  = 0

                        LET gr_cont.especial = gr_cont.especial + 1
                        DISPLAY "    ESPECIALES PROCESADAS : ", gr_cont.especial AT 07,19

                    END FOREACH

                    OUTPUT TO REPORT rpt_oper_14(lr_registros.*) #ld
                ELSE
                    -- Solo existe una aportacion especial para el nss
                    SELECT sec_pension 
                    INTO   lr_registros.sec_pension
                    FROM   ret_especial
                    WHERE  nss            = lr_registros.nss
                    AND    consec_tramite = 0 

                    LET li_ultimo_consec = f_obtiene_ult_consec()

                    UPDATE ret_especial 
                    SET    consec_tramite  = li_ultimo_consec ,
                           fecha_envio     = HOY ,
                           estado_registro = gr_edo.enviado
                    WHERE  nss             = lr_registros.nss
                    AND    sec_pension     = lr_registros.sec_pension
                    AND    consec_tramite  = 0

                    LET gr_cont.especial = gr_cont.especial + 1
                    DISPLAY "    ESPECIALES PROCESADAS : ", gr_cont.especial AT 07,19

                END IF

                OUTPUT TO REPORT rpt_oper_14(lr_registros.*) #ld
            END IF       
        END FOREACH

    FINISH REPORT rpt_oper_14

    LET ch = "chmod 777 ",g_seg_modulo.ruta_envio CLIPPED,"/PASO.014"
    RUN ch
     
END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_ult_consec : Obtiene el ultimo consecutivo que se asignara al   #
#                        registro de extemporanea/especial                  #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_ult_consec()

    DEFINE li_consec LIKE ret_consec_tramite.consec_tramite

    -- -----------------------------------------------------------------------------
    
    SELECT MAX(consec_tramite) + 1
    INTO   li_consec
    FROM   ret_consec_tramite

    IF li_consec IS NULL OR li_consec <= 0 THEN
        LET li_consec = 1
        
        INSERT INTO ret_consec_tramite
        VALUES(li_consec)
    ELSE
        UPDATE ret_consec_tramite
        SET    consec_tramite = li_consec
    END IF

    RETURN li_consec

END FUNCTION

#---------------------------------------------------------------------------#
# pr_reporte : Genera el detalle de la operacion 14                         #
#---------------------------------------------------------------------------#
REPORT rpt_oper_14(pr_reporte)

    DEFINE pr_reporte RECORD
        nss                 LIKE ret_extemporanea.nss               ,
        consecutivo         LIKE ret_extemporanea.consecutivo_lote  ,
        sec_pension         CHAR(002) ,
        periodo_pago        CHAR(006) ,
        id_tramite          SMALLINT  
    END RECORD
   
    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH   60
	    LEFT MARGIN    0
	    RIGHT MARGIN   0
	    TOP MARGIN     0
	    BOTTOM MARGIN  0

    FORMAT
    FIRST PAGE HEADER
        PRINT
            COLUMN 001,"01"                         ,
            COLUMN 003,"04"                         ,
            COLUMN 005,"01"                         ,
            COLUMN 007,gs_codigo_afore USING"&&&"   ,
            COLUMN 010,"03"                         ,
            COLUMN 012,"001"                        ,
            COLUMN 015,HOY USING"YYYYMMDD"          ,
            COLUMN 023,078 SPACES

    ON EVERY ROW
        PRINT
            COLUMN 001,"03"                             ,
            COLUMN 003,"04"                             ,
            COLUMN 005,"14"                             ,
            COLUMN 007,pr_reporte.nss                   ,
            COLUMN 018,pr_reporte.sec_pension USING"&&" ,
            COLUMN 020,pr_reporte.periodo_pago          ,
            COLUMN 026,pr_reporte.id_tramite USING"&"   ,
            COLUMN 027,74 SPACES

    ON LAST ROW
        PRINT
            COLUMN 001,"09"                             ,
            COLUMN 003,"04"                             ,
            COLUMN 005,"01"                             ,
            COLUMN 007,gs_codigo_afore USING"&&&"       ,
            COLUMN 010,"03"                             ,
            COLUMN 012,"001"                            ,
            COLUMN 015,HOY USING "YYYYMMDD"             ,
            COLUMN 023,gi_tot_registros USING "&&&&&&"  ,
            COLUMN 029,72 SPACES
END REPORT
