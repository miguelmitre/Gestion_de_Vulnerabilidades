################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
#Owner             => E.F.P.                                                   #
#Programa RETC845  => CONSULTA DE APORTACIONES EXTEMPORANEAS RECIBIDAS         #
#Fecha creacion    => 12 DE DICIEMBRE DE 2005                                  #
#Elaborado por     => JUAN CARLOS MENDOZA MORENO                               #
#Fecha modificacion=>                                                          #
#Modificado por    =>                                                          #
#Sistema           => RET                                                      #
#Version           => 1.0                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE #date
        HOY                  , 
        fecha_ini            ,
        fecha_fin            DATE
    
    DEFINE #char
        vcapturado           CHAR(15),
        venviado             CHAR(15),
        enter                CHAR(01)

    DEFINE #smallint
        indicador            SMALLINT

    DEFINE #integer 
        i                    INTEGER                             
END GLOBALS 

MAIN     
    CALL STARTLOG("RETC845.log")
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     

    CALL init()#i

    OPEN WINDOW retc8451 AT 2,2 WITH FORM "RETC8451" ATTRIBUTE (BORDER)
    DISPLAY " RETC845(1.0) CONSULTA DE APORTACIONES EXTEMPORANEAS Y ESPECIALES             " AT 3,1  ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

        MENU "CONSULTA" 
            COMMAND KEY(E) "(E)xtemporaneas" "Consultar aportaciones extemporaneas recibidas"
                LET indicador = 1
                CALL primer_paso(indicador) #pp

            COMMAND KEY(P) "Es(P)eciales" "Consultar aportaciones especiales"
                LET indicador = 2
                CALL primer_paso(indicador) #pp

            COMMAND KEY(S) "(S)alida" "Salir del Programa"
                EXIT PROGRAM
        END MENU

    CLOSE WINDOW retc8451

END MAIN 

FUNCTION init()
#i-------------

    LET HOY=FGL_GETENV("FECHA")
    
    IF HOY IS NULL OR (HOY="12/31/1899") THEN
       LET HOY=TODAY
    END IF

    LET fecha_ini = NULL
    LET fecha_fin = NULL
    
    SELECT descripcion
    INTO   vcapturado
    FROM   ret_estado
    WHERE  estado_solicitud = 0
    
    SELECT descripcion
    INTO   venviado
    FROM   ret_estado
    WHERE  estado_solicitud = 4

END FUNCTION 

FUNCTION primer_paso(indica)
#pp------------------------

    DEFINE #loc #smallint 
        indica               SMALLINT 
        

    OPEN WINDOW retc8452 AT 2,2 WITH FORM "RETC8452" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                                                <ESC> CONSULTAR " AT 1,1
    DISPLAY " RETC845(1.0) CONSULTA DE APORTACIONES EXTEMPORANEAS Y ESPECIALES             " AT 3,1  ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    INPUT BY NAME fecha_ini ,
                  fecha_fin  WITHOUT DEFAULTS

        BEFORE INPUT
            LET fecha_ini = HOY
            LET fecha_fin = HOY
            
            DISPLAY BY NAME fecha_ini ,
                            fecha_fin
        
        AFTER FIELD fecha_ini
            IF fecha_ini IS NULL OR fecha_ini = "" THEN
                ERROR "  LA FECHA INICIAL NO PUEDE SER NULA  "
                NEXT FIELD fecha_ini
            END IF  
            	
            IF fecha_ini > HOY  THEN 
                ERROR "  LA FECHA NO PUEDE SER MAYOR A LA DE HOY  "
                NEXT FIELD fecha_ini
            END IF
            
        AFTER FIELD fecha_fin    
            IF fecha_fin IS NULL OR fecha_fin = "" THEN
                ERROR "  LA FECHA FINAL NO PUEDE SER NULA  "
                NEXT FIELD fecha_fin
            END IF  

            IF fecha_fin > HOY  THEN 
                ERROR "  LA FECHA NO PUEDE SER MAYOR A LA DE HOY  "
                NEXT FIELD fecha_fin
            END IF

            IF fecha_fin < fecha_ini THEN 
                ERROR "  LA FECHA FINAL NO PUEDE SER MENOR A LA FECHA INICIAL  "
                NEXT FIELD fecha_fin
            END IF     
        
        AFTER INPUT
            IF fecha_ini IS NULL OR fecha_ini = "" THEN
                ERROR "  LA FECHA INICIAL NO PUEDE SER NULA  "
                NEXT FIELD fecha_ini
            END IF  
            	
            IF fecha_ini > HOY THEN 
                ERROR "  LA FECHA NO PUEDE SER MAYOR A LA DE HOY  "
                NEXT FIELD fecha_ini
            END IF

            IF fecha_fin IS NULL OR fecha_fin = "" THEN
                ERROR "  LA FECHA FINAL NO PUEDE SER NULA  "
                NEXT FIELD fecha_fin
            END IF  

            IF fecha_fin > HOY  THEN 
                ERROR "  LA FECHA NO PUEDE SER MAYOR A LA DE HOY  "
                NEXT FIELD fecha_fin
            END IF

            IF fecha_fin < fecha_ini THEN 
                ERROR "  LA FECHA FINAL NO PUEDE SER MENOR A LA FECHA INICIAL  "
                NEXT FIELD fecha_fin
            END IF
            
            IF indica = 1 THEN 
                CALL consulta_x_fecha()
            ELSE 
                CALL consulta_especial()
            END IF 
            	
            CONTINUE INPUT

        ON KEY (INTERRUPT)
            EXIT INPUT

    END INPUT
    CLOSE WINDOW retc8452

END FUNCTION 

FUNCTION consulta_x_fecha()
#cxf-----------------------
    DEFINE #integer
        cur                  INTEGER
        
    DEFINE #char
        txt_sel              CHAR(200)

    DEFINE la_reg1 ARRAY[5000] OF RECORD
        fecha_proceso        LIKE ret_ctr_extemp.fecha_proceso  ,
        tot_procesados       LIKE ret_ctr_extemp.tot_procesados ,
        tot_ret_tipo_a       LIKE ret_ctr_extemp.tot_ret_tipo_a ,
        tot_ret_tipo_b       LIKE ret_ctr_extemp.tot_ret_tipo_b ,
        tot_extemp_a         LIKE ret_ctr_extemp.tot_extemp_a   ,
        tot_extemp_b         LIKE ret_ctr_extemp.tot_extemp_b   ,
        tot_extemp_c         LIKE ret_ctr_extemp.tot_extemp_c   ,
        id_fin_proceso       LIKE ret_ctr_extemp.id_fin_proceso
    END RECORD 

{
    OPEN WINDOW retc8453 AT 2,2 WITH FORM "RETC8453" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                               <ENTER> VER DETALLE DE REGISTROS " AT 1,1
    DISPLAY " RETC845(1.0)   CONSULTA DE APORTACIONES EXTEMPORANEAS RECIBIDAS               " AT 3,1  ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                              " AT 7,1  ATTRIBUTE(REVERSE) 

    CLEAR FORM
}
    --ARMA CRITERIO DE CONSULTA
    
    LET txt_sel =  " SELECT * ",
                   " FROM   ret_ctr_extemp ",
                   " WHERE  fecha_proceso BETWEEN  ' ",fecha_ini," ' AND ' ",fecha_fin," ' ",
                   " ORDER BY 1 "
               
    PREPARE select1 FROM txt_sel
    
    --CARGA LOS REGISTROS EN EL ARREGLO
    
    DECLARE cur_sel CURSOR FOR select1
    
    LET i = 1

    FOREACH cur_sel INTO la_reg1[i].*
        DISPLAY "  UTILIZE LAS FLECHAS ARRIBA/ABAJO PARA DESPLAZARSE  " AT 21,1 ATTRIBUTE(REVERSE)
        LET i = i + 1
    END FOREACH 

	LET i = i - 1
	
    IF i = 0 THEN
        ERROR "  NO EXISTEN REGISTROS...  "
        RETURN 
    END IF

    -- MUESTRA EN PANTALLA LOS REGISTROS SELECCIONADOS
    
    OPEN WINDOW retc8453 AT 2,2 WITH FORM "RETC8453" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                               <ENTER> VER DETALLE DE REGISTROS " AT 1,1
    DISPLAY " RETC845(1.0)   CONSULTA DE APORTACIONES EXTEMPORANEAS RECIBIDAS               " AT 3,1  ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                              " AT 7,1  ATTRIBUTE(REVERSE) 

    IF i = 5000 THEN 
        PROMPT " SOLO SE MOSTRARAN 5,000 REGISTROS...<ENTER> PARA CONTINUAR " ATTRIBUTE(REVERSE)
        FOR CHAR enter
        RETURN  
    END IF  

    DISPLAY "HAY ",i USING "<<<&" , " REGISTROS, UTILIZE LAS FLECHAS ARRIBA-ABAJO PARA DESPLAZARSE  " AT 21,1 ATTRIBUTE(REVERSE)
    
    CALL SET_COUNT(i)
    
    DISPLAY ARRAY la_reg1 TO sa_reg1.*
    
        ON KEY (CONTROL-C)
            CLEAR FORM 
            EXIT DISPLAY

        ON KEY (INTERRUPT)
            CLEAR FORM 
            EXIT DISPLAY

        ON KEY (CONTROL-M)
            LET cur = ARR_CURR()
            CALL consulta_registro(la_reg1[cur].fecha_proceso) #cr
    END DISPLAY
    
    CLOSE WINDOW retc8453

END FUNCTION 

FUNCTION consulta_registro(fecha_proceso)
#cr--------------------------------------

    DEFINE #date
        fecha_proceso        LIKE ret_ctr_extemp.fecha_proceso
        
    DEFINE #char
        txt_sel2             CHAR(600)
    
    DEFINE #integer
        j                    INTEGER 
    
    DEFINE la_reg2 ARRAY[5000] OF RECORD
        nss                  LIKE ret_extemporanea.nss              ,
        fecha_conversion     LIKE ret_extemporanea.fecha_conversion ,
        consecutivo_lote     LIKE ret_extemporanea.consecutivo_lote ,
        periodo_pago         LIKE ret_extemporanea.periodo_pago     ,
        fecha_ini_pen        LIKE ret_transf_rx.fecha_ini_pen       ,
        tipo_retiro          LIKE ret_transf_rx.tipo_retiro         ,
        sec_pension          LIKE ret_transf_rx.sec_pension         ,
        id_tramite           LIKE ret_extemporanea.id_tramite       ,
        descripcion          LIKE ret_estado.descripcion
    END RECORD      
        
    -- BUSCA LOS DATOS A MOSTRAR
    
    LET txt_sel2 = " SELECT A.nss             ,  ",
                   "        A.fecha_conversion,  ",
                   "        A.consecutivo_lote,  ",
                   "        A.periodo_pago    ,  ",
                   "        B.fecha_ini_pen   ,  ",
                   "        B.tipo_retiro     ,  ",
                   "        B.sec_pension     ,  ",
                   "        A.id_tramite      ,  ",
                   "        C.descripcion        ",
                   " FROM   ret_extemporanea  A, ",
                   "        ret_transf_rx     B, ",
                   "        ret_estado        C  ",
                   " WHERE  A.fecha_conversion = ' ",fecha_proceso," ' ",
                   " AND    A.nss              = B.nss         ",
                   " AND    A.consecutivo_his  = B.consecutivo ",
                   " AND    A.estado_registro  = C.estado_solicitud ",
                   " ORDER BY 6,1 "

               
    PREPARE select2 FROM txt_sel2
    DECLARE cur_sel2 CURSOR FOR select2
    
    LET j = 1

    FOREACH cur_sel2 INTO la_reg2[j].*
        LET j = j + 1
        IF j > 5000 THEN
           ERROR " SE HA LLEGADO AL LIMITE DE REGISTROS A MOSTRAR QUE ES 5000"
           EXIT FOREACH
        END IF
    END FOREACH 

    LET j = j - 1
    
    IF j = 0 THEN
        ERROR " NO HAY DETALLES A MOSTRAR...  "
        SLEEP 2
        ERROR ""
        RETURN 
    END IF


    OPEN WINDOW retc8454 AT 2,2 WITH FORM "RETC8454" ATTRIBUTE (BORDER)
    DISPLAY "                                <CTRL-C> SALIR                                 " AT 1,1
    DISPLAY " RETC845(1.0)   CONSULTA DE APORTACIONES EXTEMPORANEAS RECIBIDAS               " AT 3,1  ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                              " AT 6,1  ATTRIBUTE(REVERSE) 

    CALL SET_COUNT(j)

    IF i = 5000 THEN 
        PROMPT " SOLO SE MOSTRARAN 5,000 REGISTROS...<ENTER> PARA SALIR  " ATTRIBUTE(REVERSE)
        FOR CHAR enter
    END IF  

    DISPLAY ARRAY la_reg2 TO sa_reg2.*

        ON KEY (CONTROL-C)
            EXIT DISPLAY 

        ON KEY (INTERRUPT)
            EXIT DISPLAY 
    END DISPLAY
    
    CLOSE WINDOW retc8454

END FUNCTION

FUNCTION consulta_especial()
#sp-------------------

    DEFINE #char
        txt_sel2             CHAR(600)

    DEFINE la_reg3 ARRAY[5000] OF RECORD #la_reg3
        nss                  LIKE ret_especial.nss                 ,
        sec_pension          LIKE ret_especial.sec_pension         ,
        fecha_carga          LIKE ret_cza_especial.fecha_recepcion ,
        folio_carga          LIKE ret_especial.folio               ,
        usr_carga            LIKE ret_cza_especial.usr_recepcion   ,
        estado               LIKE ret_estado.descripcion           ,
        fecha_envio          LIKE ret_cza_especial.fecha_envio
    END RECORD 
        
{        
    OPEN WINDOW retc8455 AT 2,2 WITH FORM "RETC8455" ATTRIBUTE (BORDER)
    DISPLAY "                               <CTRL-C> SALIR                                  " AT 1,1
    DISPLAY " RETC845(1.0)        CONSULTA DE APORTACIONES ESPECIALES                       " AT 3,1  ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                               " AT 6,1  ATTRIBUTE(REVERSE) 

    CLEAR FORM
}
    --ARMA CRITERIO DE CONSULTA

    LET txt_sel2 = " SELECT A.nss             ,  ",
                   "        A.sec_pension     ,  ",
                   "        B.fecha_recepcion ,  ",
                   "        A.folio           ,  ",
                   "        B.usr_recepcion   ,  ",
                   "        ''                ,  ",  #estado
                   "        B.fecha_envio        ",
                   " FROM   ret_especial     A , ",
                   "        ret_cza_especial B   ",
                   " WHERE  A.folio           = B.folio ",
                   " AND    B.fecha_recepcion BETWEEN ' ",fecha_ini," ' AND ' ",fecha_fin," ' ",
                   " ORDER BY 4,1 "

    PREPARE sel_esp FROM txt_sel2
    
    --CARGA REGISTROS EN EL ARREGLO
    
    DECLARE cur_sel_esp CURSOR FOR sel_esp
    
    LET i = 1

    FOREACH cur_sel_esp INTO la_reg3[i].*
        
        IF la_reg3[i].fecha_envio IS NULL OR la_reg3[i].fecha_envio = "" THEN
            LET la_reg3[i].estado = vcapturado
        ELSE 
            LET la_reg3[i].estado = venviado
        END IF 
        LET i = i + 1
        
        IF i > 5000 THEN
           EXIT FOREACH 
        END IF
        
    END FOREACH 

	LET i = i - 1
	
    IF i = 0 THEN
        ERROR "  NO EXISTEN REGISTROS CON ESE CRITERIO. "
        RETURN 
    END IF

    --MUESTRA EN PANTALLA LOS REGISTROS SELECCIONADOS

    OPEN WINDOW retc8455 AT 2,2 WITH FORM "RETC8455" ATTRIBUTE (BORDER)
    DISPLAY "                               <CTRL-C> SALIR                                  " AT 1,1
    DISPLAY " RETC845(1.0)        CONSULTA DE APORTACIONES ESPECIALES                       " AT 3,1  ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                               " AT 6,1  ATTRIBUTE(REVERSE) 
   
    IF i = 5000 THEN 
        PROMPT " SOLO SE MOSTRARAN 5,000 REGISTROS...<ENTER> PARA CONTINUAR " ATTRIBUTE(REVERSE)
        FOR CHAR enter
    END IF  

    DISPLAY "HAY ",i USING "<<<&" , " REGISTROS, UTILIZE LAS FLECHAS ARRIBA-ABAJO PARA DESPLAZARSE  " AT 21,1 ATTRIBUTE(REVERSE)

    CALL SET_COUNT(i)
            
    DISPLAY ARRAY la_reg3 TO sa_especial.*
    
        ON KEY (CONTROL-C)
            CLEAR FORM 
            EXIT DISPLAY

        ON KEY (INTERRUPT)
            CLEAR FORM 
            EXIT DISPLAY
    END DISPLAY
    
    CLOSE WINDOW retc8455

END FUNCTION 
