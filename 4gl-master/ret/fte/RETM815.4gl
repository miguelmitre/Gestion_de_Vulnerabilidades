################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
#Owner             => E.F.P.                                                   #
#Programa RETM815  => CONSULTA DE PROCESOS DE TRANSFERENCIAS                   #
#Fecha creacion    => 23 DE MARZO DEL 2005                                     #
#Elaborado por     => JUAN CARLOS MENDOZA MORENO                               #
#Fecha modificacion=>                                                          #
#Modificado por    =>                                                          #
#Sistema           => RET                                                      #
#Version           => 1.0                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE ga_reg_1 ARRAY[5000] OF RECORD #glo #ga_reg_1
        nss                  LIKE ret_transf_rx.nss                ,
        n_rfc                LIKE afi_mae_afiliado.n_rfc           ,
        curp                 LIKE ret_transf_rx.curp               ,
        paterno_afore        LIKE ret_transf_rx.paterno_afore      ,
        materno_afore        LIKE ret_transf_rx.materno_afore      ,
        nombre_afore         LIKE ret_transf_rx.nombre_afore       ,
        tipo_retiro          LIKE ret_transf_rx.tipo_retiro        ,
        desc_tipo_ret        LIKE tab_retiro.descripcion           ,
        tipo_prestacion      LIKE ret_transf_rx.tipo_prestacion    ,
        desc_prestacion      LIKE tab_prestacion.descripcion       ,
        regimen              LIKE ret_transf_rx.regimen            ,
        desc_regimen         LIKE tab_regimen.descripcion          , 
        tipo_seguro          LIKE ret_transf_rx.tipo_seguro        ,
        desc_seguro          LIKE tab_seguro.descripcion           ,
        tipo_pension         LIKE ret_transf_rx.tipo_pension       ,
        desc_pension         LIKE tab_pension.descripcion          ,
        sec_pension          LIKE ret_transf_rx.sec_pension        ,
        diag_procesar        LIKE ret_transf_rx.diag_registro      ,
        fecha_resolucion     LIKE ret_transf_rx.fecha_resolucion   ,
        fecha_ini_pen        LIKE ret_transf_rx.fecha_ini_pen      ,
        porcentaje_val       LIKE ret_transf_rx.porcentaje_val     ,
        porcentaje_ret97     LIKE ret_transf_rx.porcentaje_ret97   ,
        porcentaje_cv        LIKE ret_transf_rx.porcentaje_cv      ,
        porcentaje_cs        LIKE ret_transf_rx.porcentaje_cs      ,
        porcentaje_viv       LIKE ret_transf_rx.porcentaje_viv     ,
        folio                LIKE ret_transf_rx.folio              ,
        consecutivo          INTEGER                        ,
        estado_solicitud     LIKE ret_transf_rx.estado_solicitud   ,
        desc_estado          LIKE ret_estado.descripcion           ,
        diag_afore           LIKE ret_transf_tx.diag_registro      ,
        desc_diag_afore      LIKE tab_diagnostico.descripcion      ,
        fecha_carga          LIKE ret_cza_lote.fecha_carga         ,
        archivo_carga        LIKE ret_cza_lote.nom_archivo         ,
        fecha_provision      LIKE dis_provision.fecha_conversion   ,
        usr_provision        LIKE dis_provision.usuario            ,
        fecha_envio          LIKE ret_ctr_envio_lote.fecha_envio   ,
        usr_envio            LIKE ret_ctr_envio_lote.usuario_envio ,
        fecha_liquida        LIKE dis_cuenta.fecha_conversion      ,
        usr_liquida          LIKE dis_cuenta.usuario               ,
        edo_marca            CHAR(30)
    END RECORD
  
    DEFINE #glo #date
        HOY                  DATE
        
    DEFINE #glo #char
        txt_1                CHAR(2900) ,
        x_busca              CHAR(1200) ,
        enter                CHAR(0001) 

    DEFINE #glo #smallint
        cont_reg             SMALLINT
        
    DEFINE gr_reg_1 RECORD #glo #gr_reg_1
        tipo_retiro          LIKE tab_retiro.tipo_retiro ,
        desc_tipo_ret        LIKE tab_retiro.descripcion           
    END RECORD

    DEFINE gr_reg_2 RECORD #glo #gr_reg_2
        tipo_prestacion      LIKE tab_prestacion.tipo_prestacion ,
        desc_prestacion      LIKE tab_prestacion.descripcion           
    END RECORD

    DEFINE gr_reg_3 RECORD #glo #gr_reg_3
        regimen              LIKE tab_regimen.regimen    ,
        desc_regimen         LIKE tab_regimen.descripcion           
    END RECORD

    DEFINE gr_reg_4 RECORD #glo #gr_reg_4
        tipo_seguro          LIKE tab_seguro.clave      ,
        desc_seguro          LIKE tab_seguro.descripcion           
    END RECORD

    DEFINE gr_reg_5 RECORD #glo #gr_reg_5
        tipo_pension         LIKE tab_pension.tipo_pension ,
        desc_pension         LIKE tab_pension.descripcion           
    END RECORD

    DEFINE gr_reg_7 RECORD #glo #gr_reg_7
        estado_solicitud     LIKE ret_estado.estado_solicitud ,
        desc_estado          LIKE ret_estado.descripcion           
    END RECORD

    DEFINE gr_reg_8 RECORD #glo #gr_reg_8
        diag_afore           LIKE tab_diagnostico.cod_diagnostico ,
        desc_diag_afore      LIKE tab_diagnostico.descripcion           
    END RECORD

    DEFINE gr_reg_9 RECORD #glo #gr_reg_9
        folio                LIKE ret_transf_rx.folio      ,
        fecha_carga         LIKE ret_cza_lote.fecha_carga  ,
        nom_archivo          LIKE ret_cza_lote.nom_archivo
    END RECORD

    DEFINE #glo #integer
        i                    ,
        vdiag_procesar       INTEGER 
        
        
END GLOBALS

MAIN   
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     

    CALL init()

    OPEN WINDOW retm8151 AT 2,2 WITH 21 ROWS, 78 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETM815(1.0)     CONSULTA DE PROCESOS DE TRANSFERENCIAS                       " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

           MENU "MENU" 
               COMMAND KEY(C) "(C)onsulta" "Consultar Proceso de Transferencias"
                   CALL consulta() #c
               COMMAND KEY(S) "(S)alida" "Salir del Programa"
                   EXIT PROGRAM
           END MENU
    CLOSE WINDOW RETM8151
END MAIN

FUNCTION init()
#i-------------

    LET HOY = TODAY
    INITIALIZE gr_reg_1.* TO NULL
    INITIALIZE gr_reg_2.* TO NULL
    INITIALIZE gr_reg_3.* TO NULL
    INITIALIZE gr_reg_4.* TO NULL
    INITIALIZE gr_reg_5.* TO NULL
    INITIALIZE gr_reg_7.* TO NULL
    INITIALIZE gr_reg_8.* TO NULL
    INITIALIZE gr_reg_9.* TO NULL
    
END FUNCTION

FUNCTION consulta()
#c-----------------
    DEFINE #loc #integer
        arr_c                ,
        i                    ,
        vdiag_proc           ,
        vfolio_transf        INTEGER
        
    DEFINE #loc #char
        resp                 CHAR(001) ,
        condicion            CHAR(180)    
        

    OPEN WINDOW retm8152 AT 2,2 WITH FORM "RETM8151" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                                <CTRL-V> VER DETALLE DE SALDOS " AT 1,1
    DISPLAY " RETM815(1.0)                 DATOS DEL AFILIADO                              " AT 3,1  ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "                    DATOS DE LA SOLICITUD DE TRANSFERENCIA                    " AT 7,1  ATTRIBUTE(REVERSE)
    DISPLAY "                           DATOS DE CONTROL INTERNO                           " AT 14,1 ATTRIBUTE(REVERSE)

    CALL init()
 
    CLEAR FORM

    LET int_flag = FALSE 

    CONSTRUCT x_busca ON  A.nss              ,
                          B.n_rfc            ,
                          A.curp             ,
                          A.paterno_afore    ,
                          A.materno_afore    ,
                          A.nombre_afore     ,
                          A.tipo_retiro      ,
                          A.tipo_prestacion  ,
                          A.regimen          ,
                          A.tipo_seguro      ,
                          A.tipo_pension     ,
                          A.sec_pension      ,
                          A.diag_registro    ,
                          A.fecha_resolucion ,
                          A.fecha_ini_pen    ,
                          A.folio            ,
                          A.estado_solicitud ,
                          C.fecha_carga      ,
                          F.fecha_envio      ,
                          A.consecutivo      ,
                          G.diag_registro    ,
                          C.nom_archivo      ,
                          F.usuario_envio
                     FROM nss                ,
                          n_rfc              ,
                          curp               ,
                          paterno_afore      ,
                          materno_afore      ,
                          nombre_afore       ,
                          tipo_retiro        ,
                          tipo_prestacion    ,
                          regimen            ,
                          tipo_seguro        ,
                          tipo_pension       ,
                          sec_pension        ,
                          diag_procesar      ,
                          fecha_resolucion   ,
                          fecha_ini_pen      ,
                          folio              ,
                          estado_solicitud   ,
                          fecha_carga        ,
                          fecha_envio        ,
                          consecutivo        ,
                          diag_afore         ,
                          nom_archivo        ,
                          usr_envio

        BEFORE CONSTRUCT 
            LET condicion = " AND    A.nss         = G.nss         ",
                            " AND    A.consecutivo = G.consecutivo ",
                            " AND    A.folio       = G.folio       "
--TIPO DE RETIRO
        BEFORE FIELD tipo_retiro
            MESSAGE "  PULSE <CTRL-W> PARA VER TIPO DE RETIRO  "
            	
        AFTER FIELD tipo_retiro
            MESSAGE ""
            IF FIELD_TOUCHED(tipo_retiro) THEN
            	
                LET gr_reg_1.tipo_retiro = GET_FLDBUF(tipo_retiro)
                
                IF gr_reg_1.tipo_retiro IS NULL OR gr_reg_1.tipo_retiro = "" THEN
                	
                    LET gr_reg_1.desc_tipo_ret = NULL
                    DISPLAY gr_reg_1.desc_tipo_ret TO desc_tipo_ret
                ELSE 
                    SELECT B.descripcion
                    INTO   gr_reg_1.desc_tipo_ret
                    FROM   tab_tramite_retiro A ,
                           tab_retiro         B
                    WHERE  A.tipo_retiro = B.tipo_retiro
                    AND    A.cod_tramite = 1
                    AND    A.tipo_retiro = gr_reg_1.tipo_retiro
                    
                    IF STATUS = NOTFOUND THEN
                    	
                        LET gr_reg_1.tipo_retiro   = NULL
                        LET gr_reg_1.desc_tipo_ret = NULL
                    
                        DISPLAY gr_reg_1.tipo_retiro TO tipo_retiro
                        DISPLAY gr_reg_1.desc_tipo_ret TO desc_tipo_ret

                        PROMPT "  TIPO DE RETIRO NO EXISTE, DESEA ABRIR VENTANA DE AYUDA (S/N)?  "
                        FOR CHAR resp
                        
                        IF resp MATCHES "[Ss]" THEN
                        	
                            CALL ventana_ayuda(1) RETURNING gr_reg_1.*
                            
                            IF INT_FLAG = TRUE THEN
                            	
                               LET INT_FLAG = FALSE
                               LET gr_reg_1.tipo_retiro   = NULL
                               LET gr_reg_1.desc_tipo_ret = NULL
                            END IF
                            
                            DISPLAY gr_reg_1.tipo_retiro   TO tipo_retiro
                            DISPLAY gr_reg_1.desc_tipo_ret TO desc_tipo_ret
                        ELSE 
                            LET gr_reg_1.tipo_retiro   = NULL
                            DISPLAY gr_reg_1.tipo_retiro TO tipo_retiro
                        END IF
                    
                        IF (FGL_LASTKEY() = FGL_KEYVAL("UP"))     OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("DOWN"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("LEFT"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("RETURN")) OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("ACCEPT")) THEN
                            CONTINUE CONSTRUCT 
                        END IF 
                    ELSE
                        DISPLAY gr_reg_1.tipo_retiro TO tipo_retiro
                        DISPLAY gr_reg_1.desc_tipo_ret TO desc_tipo_ret
                    END IF
                END IF 
            END IF

--TIPO DE PRESTACION
        BEFORE FIELD tipo_prestacion
            MESSAGE "  PULSE <CTRL-W> PARA VER TIPO DE PRESTACION  "
            	
        AFTER FIELD tipo_prestacion
            MESSAGE ""
            IF FIELD_TOUCHED(tipo_prestacion) THEN
                
                LET gr_reg_2.tipo_prestacion = GET_FLDBUF(tipo_prestacion)
                
                IF gr_reg_2.tipo_prestacion IS NULL OR gr_reg_2.tipo_prestacion = "" THEN
                	
                    LET gr_reg_2.desc_prestacion = NULL
                    DISPLAY gr_reg_2.desc_prestacion TO desc_prestacion
                ELSE
                    IF gr_reg_1.tipo_retiro = "A" AND gr_reg_2.tipo_prestacion = 0 THEN
                    	
                        ERROR "  TIPO DE PRESTACION NO VALIDO PARA EL TIPO DE RETIRO  "
                        LET gr_reg_2.tipo_prestacion = NULL
                        LET gr_reg_2.desc_prestacion = NULL
                        DISPLAY gr_reg_2.desc_prestacion TO desc_prestacion
                        DISPLAY gr_reg_2.tipo_prestacion TO tipo_prestacion
                        NEXT FIELD tipo_prestacion
                    ELSE
                        IF (gr_reg_1.tipo_retiro = "B" OR 
                            gr_reg_1.tipo_retiro = "C" ) AND gr_reg_2.tipo_prestacion = 1 THEN
                            	
                            ERROR "  TIPO DE PRESTACION NO VALIDO PARA EL TIPO DE RETIRO  "
                            LET gr_reg_2.tipo_prestacion = NULL
                            LET gr_reg_2.desc_prestacion = NULL
                            DISPLAY gr_reg_2.desc_prestacion TO desc_prestacion
                            DISPLAY gr_reg_2.tipo_prestacion TO tipo_prestacion
                            NEXT FIELD tipo_prestacion
                        END IF
                    END IF    	
                    	
                    SELECT C.descripcion
                    INTO   gr_reg_2.desc_prestacion
                    FROM   ret_matriz_derecho A ,
                           tab_tramite_retiro B ,
                           tab_prestacion     C
                    WHERE  A.tipo_retiro     = B.tipo_retiro
                    AND    A.tipo_prestacion = C.tipo_prestacion
                    AND    B.cod_tramite     = 1
                    AND    C.tipo_prestacion = gr_reg_2.tipo_prestacion
                    GROUP BY 1
                    
                    IF STATUS = NOTFOUND THEN
                    	
                        LET gr_reg_2.tipo_prestacion = NULL
                        LET gr_reg_2.desc_prestacion = NULL
                        DISPLAY gr_reg_2.tipo_prestacion TO tipo_prestacion
                        DISPLAY gr_reg_2.desc_prestacion TO desc_prestacion

                        PROMPT "  TIPO DE PRESTACION NO EXISTE, DESEA ABRIR VENTANA DE AYUDA (S/N)?  "
                        FOR CHAR resp
                        
                        IF resp MATCHES "[Ss]" THEN
                        	
                            CALL ventana_ayuda(2) RETURNING gr_reg_2.*
                            
                            IF INT_FLAG = TRUE THEN
                                LET INT_FLAG = FALSE
                                LET gr_reg_2.tipo_prestacion = NULL
                                LET gr_reg_2.desc_prestacion = NULL
                            END IF
                            
                            DISPLAY gr_reg_2.tipo_prestacion TO tipo_prestacion
                            DISPLAY gr_reg_2.desc_prestacion TO desc_prestacion
                        END IF
                    
                        IF (FGL_LASTKEY() = FGL_KEYVAL("UP"))     OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("DOWN"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("LEFT"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("RETURN")) OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("ACCEPT")) THEN
                            CONTINUE CONSTRUCT 
                        END IF 
                    ELSE
                        DISPLAY gr_reg_2.tipo_prestacion TO tipo_prestacion
                        DISPLAY gr_reg_2.desc_prestacion TO desc_prestacion
                    END IF
                END IF 
            END IF

--REGIMEN
        BEFORE FIELD regimen
            MESSAGE "  PULSE <CTRL-W> PARA VER EL TIPO DE REGIMEN  "
            	
        AFTER FIELD regimen
            MESSAGE ""
            IF FIELD_TOUCHED(regimen) THEN
                
                LET gr_reg_3.regimen = GET_FLDBUF(regimen)

                IF gr_reg_3.regimen IS NULL OR gr_reg_3.regimen = "" THEN
                    LET gr_reg_3.desc_regimen = NULL
                    DISPLAY gr_reg_3.desc_regimen TO desc_regimen
                ELSE 
                    IF gr_reg_1.tipo_retiro = "B" AND gr_reg_3.regimen = "97" THEN
                        ERROR "  TIPO DE REGIMEN NO VALIDO PARA EL TIPO DE RETIRO  "
                        LET gr_reg_3.regimen      = NULL
                        LET gr_reg_3.desc_regimen = NULL
                        DISPLAY gr_reg_3.desc_regimen TO desc_regimen
                        DISPLAY gr_reg_3.regimen      TO regimen
                        NEXT FIELD regimen
                    ELSE
                        IF (gr_reg_1.tipo_retiro = "A" OR 
                            gr_reg_1.tipo_retiro = "C" ) AND gr_reg_3.regimen = "73" THEN
                            ERROR "  TIPO DE REGIMEN NO VALIDO PARA EL TIPO DE RETIRO  "
                            LET gr_reg_3.regimen   = NULL
                            LET gr_reg_3.desc_regimen = NULL
                            DISPLAY gr_reg_3.desc_regimen TO desc_regimen
                            DISPLAY gr_reg_3.regimen TO regimen
                            NEXT FIELD regimen
                        END IF
                    END IF    	

                    SELECT A.descripcion
                    INTO   gr_reg_3.desc_regimen
                    FROM   tab_regimen A 
                    WHERE  A.regimen = gr_reg_3.regimen
                    
                    IF STATUS = NOTFOUND THEN
                        LET gr_reg_3.regimen      = NULL
                        LET gr_reg_3.desc_regimen = NULL
                    
                        DISPLAY gr_reg_3.regimen      TO regimen
                        DISPLAY gr_reg_3.desc_regimen TO desc_regimen
                        PROMPT "  TIPO DE REGIMEN NO EXISTE, DESEA ABRIR VENTANA DE AYUDA (S/N)?  "
                        FOR CHAR resp
                        
                        IF resp MATCHES "[Ss]" THEN
                            CALL ventana_ayuda(3) RETURNING gr_reg_3.*
                            
                            IF INT_FLAG = TRUE THEN
                                LET INT_FLAG = FALSE
                                LET gr_reg_3.regimen      = NULL
                                LET gr_reg_3.desc_regimen = NULL
                            END IF
                            
                            DISPLAY gr_reg_3.regimen      TO regimen
                            DISPLAY gr_reg_3.desc_regimen TO desc_regimen
                        END IF
                    
                        IF (FGL_LASTKEY() = FGL_KEYVAL("UP"))     OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("DOWN"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("LEFT"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("RETURN")) OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("ACCEPT")) THEN
                            CONTINUE CONSTRUCT 
                        END IF 
                    ELSE
                        DISPLAY gr_reg_3.regimen      TO regimen
                        DISPLAY gr_reg_3.desc_regimen TO desc_regimen
                    END IF 
                END IF 
            END IF

--TIPO SEGURO	
        BEFORE FIELD tipo_seguro
            MESSAGE "  PULSE <CTRL-W> PARA VER EL TIPO DE SEGURO  "
            	
        AFTER FIELD tipo_seguro
            MESSAGE ""
            IF FIELD_TOUCHED(tipo_seguro) THEN
                
                LET gr_reg_4.tipo_seguro = GET_FLDBUF(tipo_seguro)

                IF gr_reg_4.tipo_seguro IS NULL OR gr_reg_4.tipo_seguro = "" THEN
                    LET gr_reg_4.desc_seguro = NULL
                    DISPLAY gr_reg_4.desc_seguro TO desc_seguro
                ELSE
                    IF gr_reg_1.tipo_retiro = "A" AND gr_reg_4.tipo_seguro = "IV" THEN
                        ERROR "  TIPO DE SEGURO NO VALIDO PARA EL TIPO DE RETIRO  "
                        LET gr_reg_4.tipo_seguro = NULL
                        LET gr_reg_4.desc_seguro = NULL
                        DISPLAY gr_reg_4.desc_seguro TO desc_seguro
                        DISPLAY gr_reg_4.tipo_seguro TO tipo_seguro
                        NEXT FIELD tipo_seguro
                    ELSE
                        IF gr_reg_1.tipo_retiro = "B"  AND 
                           (gr_reg_4.tipo_seguro = "CV" OR 
                            gr_reg_4.tipo_seguro = "IM" ) THEN
                            	
                            ERROR "  TIPO DE SEGURO NO VALIDO PARA EL TIPO DE RETIRO  "
                            LET gr_reg_4.tipo_seguro = NULL
                            LET gr_reg_4.desc_seguro = NULL
                            DISPLAY gr_reg_4.desc_seguro TO desc_seguro
                            DISPLAY gr_reg_4.tipo_seguro TO tipo_seguro
                            NEXT FIELD tipo_seguro
                        ELSE
                            IF gr_reg_1.tipo_retiro = "C" AND 
                               (gr_reg_4.tipo_seguro = "IM" OR 
                                gr_reg_4.tipo_seguro = "RT" OR 
                                gr_reg_4.tipo_seguro = "IV" ) THEN 
                        
                                ERROR "  TIPO DE SEGURO NO VALIDO PARA EL TIPO DE RETIRO  "
                                LET gr_reg_4.tipo_seguro = NULL
                                LET gr_reg_4.desc_seguro = NULL
                                DISPLAY gr_reg_4.desc_seguro TO desc_seguro
                                DISPLAY gr_reg_4.tipo_seguro TO tipo_seguro
                                NEXT FIELD tipo_seguro
                            END IF
                        END IF
                    END IF 
                    SELECT B.descripcion
                    INTO   gr_reg_4.desc_seguro
                    FROM   ret_matriz_derecho A,
                           tab_seguro         B,
                           tab_tramite_retiro C
                    WHERE  A.tipo_seguro = B.clave
                    AND    A.tipo_retiro = C.tipo_retiro
                    AND    C.cod_tramite = 1
                    AND    A.tipo_seguro = gr_reg_4.tipo_seguro
                    GROUP BY 1
                    ORDER BY 1
                    
                    IF STATUS = NOTFOUND THEN
                        LET gr_reg_4.tipo_seguro = NULL
                        LET gr_reg_4.desc_seguro = NULL
                    
                        DISPLAY gr_reg_4.tipo_seguro TO tipo_seguro
                        DISPLAY gr_reg_4.desc_seguro TO desc_seguro
                        PROMPT "  TIPO DE SEGURO NO EXISTE, DESEA ABRIR VENTANA DE AYUDA (S/N)?  "
                        FOR CHAR resp
                        
                        IF resp MATCHES "[Ss]" THEN
                            CALL ventana_ayuda(4) RETURNING gr_reg_4.*
                            
                            IF INT_FLAG = TRUE THEN
                                LET INT_FLAG = FALSE
                                LET gr_reg_4.tipo_seguro = NULL
                                LET gr_reg_4.desc_seguro = NULL
                            END IF
                            
                            DISPLAY gr_reg_4.tipo_seguro TO tipo_seguro
                            DISPLAY gr_reg_4.desc_seguro TO desc_seguro
                        END IF
                    
                        IF (FGL_LASTKEY() = FGL_KEYVAL("UP"))     OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("DOWN"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("LEFT"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("RETURN")) OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("ACCEPT")) THEN
                            CONTINUE CONSTRUCT 
                        END IF 
                    ELSE
                        DISPLAY gr_reg_4.tipo_seguro TO tipo_seguro
                        DISPLAY gr_reg_4.desc_seguro TO desc_seguro
                    END IF
                END IF
            END IF

--TIPO PENSION
        BEFORE FIELD tipo_pension
            MESSAGE "  PULSE <CTRL-W> PARA VER EL TIPO DE PENSION  "
            	
        AFTER FIELD tipo_pension
            MESSAGE ""
            IF FIELD_TOUCHED(tipo_pension) THEN
                
                LET gr_reg_5.tipo_pension = GET_FLDBUF(tipo_pension)

                IF gr_reg_5.tipo_pension IS NULL OR gr_reg_5.tipo_pension = "" THEN
                    LET gr_reg_5.desc_pension = NULL
                    DISPLAY gr_reg_5.desc_pension TO desc_pension
                ELSE
                    IF gr_reg_1.tipo_retiro = "C" AND 
                       (gr_reg_5.tipo_pension = "AS" OR 
                        gr_reg_5.tipo_pension = "IN" OR  
                        gr_reg_5.tipo_pension = "IP" OR  
                        gr_reg_5.tipo_pension = "OR" OR  
                        gr_reg_5.tipo_pension = "VI" OR  
                        gr_reg_5.tipo_pension = "VO" ) THEN
                        
                        ERROR "  TIPO DE PENSION NO VALIDO PARA EL TIPO DE RETIRO  "
                        LET gr_reg_5.tipo_pension = NULL
                        LET gr_reg_5.desc_pension = NULL
                        DISPLAY gr_reg_5.desc_pension TO desc_pension
                        DISPLAY gr_reg_5.tipo_pension TO tipo_pension
                        NEXT FIELD tipo_pension
                    END IF 
                     
                    SELECT B.descripcion
                    INTO   gr_reg_5.desc_pension
                    FROM   ret_matriz_derecho A ,
                           tab_pension        B ,
                           tab_tramite_retiro C
                    WHERE  A.tipo_retiro  = C.tipo_retiro
                    AND    A.tipo_pension = B.tipo_pension
                    AND    C.cod_tramite  = 1
                    AND    A.tipo_pension = gr_reg_5.tipo_pension
                    GROUP BY 1
                    ORDER BY 1
                    
                    IF STATUS = NOTFOUND THEN
                        LET gr_reg_5.tipo_pension = NULL
                        LET gr_reg_5.desc_pension = NULL
                    
                        DISPLAY gr_reg_5.tipo_pension TO tipo_pension
                        DISPLAY gr_reg_5.desc_pension TO desc_pension
                        PROMPT "  TIPO DE PENSION NO EXISTE, DESEA ABRIR VENTANA DE AYUDA (S/N)?  "
                        FOR CHAR resp
                        
                        IF resp MATCHES "[Ss]" THEN
                            CALL ventana_ayuda(5) RETURNING gr_reg_5.*
                            
                            IF INT_FLAG = TRUE THEN
                                LET INT_FLAG = FALSE
                                LET gr_reg_5.tipo_pension = NULL
                                LET gr_reg_5.desc_pension = NULL
                            END IF
                            
                            DISPLAY gr_reg_5.tipo_pension TO tipo_pension
                            DISPLAY gr_reg_5.desc_pension TO desc_pension
                        END IF
                    
                        IF (FGL_LASTKEY() = FGL_KEYVAL("UP"))     OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("DOWN"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("LEFT"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("RETURN")) OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("ACCEPT")) THEN
                            CONTINUE CONSTRUCT 
                        END IF 
                    ELSE
                        DISPLAY gr_reg_5.tipo_pension TO tipo_pension
                        DISPLAY gr_reg_5.desc_pension TO desc_pension
                    END IF
                END IF
            END IF

--DIAGNOSTICO PROCESAR
        BEFORE FIELD diag_procesar
            MESSAGE "  PULSE <CTRL-W> PARA VER LOS DIAGNOSTICOS DE PROCESAR  "
            	
        AFTER FIELD diag_procesar
            MESSAGE ""
            IF FIELD_TOUCHED(diag_procesar) THEN
                
                LET vdiag_proc = GET_FLDBUF(diag_procesar)

                IF vdiag_proc IS NULL OR vdiag_proc = "" THEN
                    CONTINUE CONSTRUCT
                ELSE 
                    SELECT cod_diagnostico
                    INTO   vdiag_procesar
                    FROM   tab_diagnostico
                    WHERE  entidad         = 3
                    AND    modulo_cod      = "ret"
                    AND    cod_diagnostico = vdiag_proc
                    ORDER BY 1
                    
                    IF STATUS = NOTFOUND THEN
                        LET vdiag_procesar = NULL
                    
                        DISPLAY vdiag_procesar TO diag_procesar
                        PROMPT "  DIAGNOSTICO DE PROCESAR NO EXISTE, DESEA ABRIR VENTANA DE AYUDA (S/N)?  "
                        FOR CHAR resp
                        
                        IF resp MATCHES "[Ss]" THEN
                            CALL ventana_ayuda(6) RETURNING vdiag_procesar
                            
                            IF INT_FLAG = TRUE THEN
                                LET INT_FLAG = FALSE
                                LET vdiag_procesar = NULL
                            END IF
                            
                            DISPLAY vdiag_procesar TO diag_procesar
                        END IF
                    
                        IF (FGL_LASTKEY() = FGL_KEYVAL("UP"))     OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("DOWN"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("LEFT"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("RETURN")) OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("ACCEPT")) THEN
                            CONTINUE CONSTRUCT 
                        END IF 
                    ELSE
                        DISPLAY vdiag_procesar TO diag_procesar
                    END IF
                END IF
            END IF

--FOLIO
        BEFORE FIELD folio

            MESSAGE "  PULSE <CTRL-W> PARA VER LOS FOLIOS  "
            	
        AFTER FIELD folio
            MESSAGE ""
            IF FIELD_TOUCHED(folio) THEN
                
                LET gr_reg_9.folio = GET_FLDBUF(folio)

                IF gr_reg_9.folio IS NULL OR gr_reg_9.folio = "" THEN
                    CONTINUE CONSTRUCT
                ELSE 
                    SELECT A.folio
                    INTO   vfolio_transf
                    FROM   ret_transf_rx A ,
                           ret_cza_lote  B
                    WHERE  A.folio = B.folio
                    AND    A.folio = gr_reg_9.folio
                    GROUP BY 1
                    ORDER BY 1
                    
                    IF STATUS = NOTFOUND THEN
                        LET vfolio_transf = NULL
                    
                        DISPLAY vfolio_transf TO folio
                        PROMPT "  EL FOLIO NO EXISTE, DESEA ABRIR VENTANA DE AYUDA (S/N)?  "
                        FOR CHAR resp
                        
                        IF resp MATCHES "[Ss]" THEN
                            CALL ventana_ayuda(9) RETURNING vfolio_transf
                            
                            IF INT_FLAG = TRUE THEN
                                LET INT_FLAG = FALSE
                                LET vfolio_transf = NULL
                            END IF
                            
                            DISPLAY vfolio_transf TO folio
                        END IF
                    
                        IF (FGL_LASTKEY() = FGL_KEYVAL("UP"))     OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("DOWN"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("LEFT"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("RETURN")) OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("ACCEPT")) THEN
                            CONTINUE CONSTRUCT 
                        END IF 
                    ELSE
                        DISPLAY vfolio_transf TO folio
                    END IF
                END IF
            END IF

--ESTADO SOLICITUD
        BEFORE FIELD estado_solicitud
            MESSAGE "  PULSE <CTRL-W> PARA VER LOS TIPOS DE ESTADO  "
            	
        AFTER FIELD estado_solicitud
            MESSAGE ""
            IF FIELD_TOUCHED(estado_solicitud) THEN
                
                LET gr_reg_7.estado_solicitud = GET_FLDBUF(estado_solicitud)

                IF gr_reg_7.estado_solicitud IS NULL OR gr_reg_7.estado_solicitud = "" THEN
                    LET gr_reg_7.desc_estado = NULL
                    DISPLAY gr_reg_7.desc_estado TO desc_estado
                ELSE 
                    SELECT descripcion
                    INTO   gr_reg_7.desc_estado
                    FROM   ret_estado
                    WHERE  estado_solicitud IN (1,2,4,8)
                    AND    estado_solicitud = gr_reg_7.estado_solicitud
                    
                    
                    IF STATUS = NOTFOUND THEN
                        LET gr_reg_7.estado_solicitud = NULL
                        LET gr_reg_7.desc_estado      = NULL
                    
                        DISPLAY gr_reg_7.estado_solicitud TO estado_solicitud
                        DISPLAY gr_reg_7.desc_estado      TO desc_estado
                        PROMPT "  ESTADO DE SOLICITUD NO EXISTE, DESEA ABRIR VENTANA DE AYUDA (S/N)?  "
                        FOR CHAR resp
                        
                        IF resp MATCHES "[Ss]" THEN
                            CALL ventana_ayuda(7) RETURNING gr_reg_7.*
                            
                            IF INT_FLAG = TRUE THEN
                                LET INT_FLAG = FALSE
                                LET gr_reg_7.estado_solicitud = NULL
                                LET gr_reg_7.desc_estado      = NULL
                            END IF
                            
                            DISPLAY gr_reg_7.estado_solicitud TO estado_solicitud
                            DISPLAY gr_reg_7.desc_estado      TO desc_estado
                        END IF
                    
                        IF (FGL_LASTKEY() = FGL_KEYVAL("UP"))     OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("DOWN"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("LEFT"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("RETURN")) OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("ACCEPT")) THEN
                            CONTINUE CONSTRUCT 
                        END IF 
                    ELSE
                        IF gr_reg_7.estado_solicitud = 2 THEN
                            LET gr_reg_7.desc_estado = "PROVISIONADO"
                        END IF 
                      
                        DISPLAY gr_reg_7.estado_solicitud TO estado_solicitud
                        DISPLAY gr_reg_7.desc_estado      TO desc_estado
                    END IF
                END IF
            END IF

--DIAGNOSTICO AFORE
        BEFORE FIELD diag_afore
            
            IF INFIELD(diag_afore) THEN
                LET condicion = " AND    A.nss         = G.nss         ",
                                " AND    A.consecutivo = G.consecutivo ",
                                " AND    A.folio       = G.folio       "
            ELSE 
                LET condicion = " AND 1 = 1 "
            END IF  
            	 
            MESSAGE "  PULSE <CTRL-W> PARA VER LOS DIAGNOSTICOS DE LA AFORE  "
            	
        AFTER FIELD diag_afore
            MESSAGE ""
            IF FIELD_TOUCHED(diag_afore) THEN
                
                LET gr_reg_8.diag_afore = GET_FLDBUF(diag_afore)

                IF gr_reg_8.diag_afore IS NULL OR gr_reg_8.diag_afore = "" THEN
                    LET gr_reg_8.desc_diag_afore = NULL
                    DISPLAY gr_reg_8.desc_diag_afore TO desc_diag_afore
                ELSE 
                    SELECT descripcion
                    INTO   gr_reg_8.desc_diag_afore
                    FROM   tab_diagnostico
                    WHERE  entidad         = 1
                    AND    modulo_cod      = "ret"
                    AND    cod_diagnostico = gr_reg_8.diag_afore
                    
                    IF STATUS = NOTFOUND THEN
                        LET gr_reg_8.diag_afore      = NULL
                        LET gr_reg_8.desc_diag_afore = NULL
                    
                        DISPLAY gr_reg_8.diag_afore      TO diag_afore
                        DISPLAY gr_reg_8.desc_diag_afore TO desc_diag_afore
                        PROMPT "  DIAGNOSTICO DE AFORE NO EXISTE, DESEA ABRIR VENTANA DE AYUDA (S/N)?  "
                        FOR CHAR resp
                        
                        IF resp MATCHES "[Ss]" THEN
                            CALL ventana_ayuda(8) RETURNING gr_reg_8.*
                            
                            IF INT_FLAG = TRUE THEN
                                LET INT_FLAG = FALSE
                                LET gr_reg_8.diag_afore      = NULL
                                LET gr_reg_8.desc_diag_afore = NULL
                            END IF
                            
                            DISPLAY gr_reg_8.diag_afore      TO diag_afore
                            DISPLAY gr_reg_8.desc_diag_afore TO desc_diag_afore
                        END IF
                    
                        IF (FGL_LASTKEY() = FGL_KEYVAL("UP"))     OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("DOWN"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("LEFT"))   OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("RETURN")) OR 
                           (FGL_LASTKEY() = FGL_KEYVAL("ACCEPT")) THEN
                            CONTINUE CONSTRUCT 
                        END IF 
                    ELSE
                        DISPLAY gr_reg_8.diag_afore      TO diag_afore
                        DISPLAY gr_reg_8.desc_diag_afore TO desc_diag_afore
                    END IF
                END IF
            END IF
   
        ON KEY (CONTROL-F) 
            NEXT FIELD folio 

        ON KEY (CONTROL-W)
            CASE
                WHEN INFIELD(tipo_retiro)
                    CALL ventana_ayuda(1) RETURNING gr_reg_1.*
                    
                    IF INT_FLAG = TRUE THEN
                       LET INT_FLAG = FALSE
                       LET gr_reg_1.tipo_retiro   = NULL
                       LET gr_reg_1.desc_tipo_ret = NULL
                    END IF
                    
                    DISPLAY gr_reg_1.tipo_retiro TO tipo_retiro
                    DISPLAY gr_reg_1.desc_tipo_ret TO desc_tipo_ret
                
                WHEN INFIELD(tipo_prestacion)
                    CALL ventana_ayuda(2) RETURNING gr_reg_2.*
                    
                    IF INT_FLAG = TRUE THEN
                       LET INT_FLAG = FALSE
                       LET gr_reg_2.tipo_prestacion = NULL
                       LET gr_reg_2.desc_prestacion = NULL
                    END IF
                    
                    DISPLAY gr_reg_2.tipo_prestacion TO tipo_prestacion
                    DISPLAY gr_reg_2.desc_prestacion TO desc_prestacion
                
                WHEN INFIELD(regimen)
                    CALL ventana_ayuda(3) RETURNING gr_reg_3.*
                    
                    IF INT_FLAG = TRUE THEN
                       LET INT_FLAG = FALSE
                       LET gr_reg_3.regimen      = NULL
                       LET gr_reg_3.desc_regimen = NULL
                    END IF
                    
                    DISPLAY gr_reg_3.regimen      TO regimen
                    DISPLAY gr_reg_3.desc_regimen TO desc_regimen
                
                WHEN INFIELD(tipo_seguro)
                    CALL ventana_ayuda(4) RETURNING gr_reg_4.*
                    
                    IF INT_FLAG = TRUE THEN
                       LET INT_FLAG = FALSE
                       LET gr_reg_4.tipo_seguro = NULL
                       LET gr_reg_4.desc_seguro = NULL
                    END IF
                    
                    DISPLAY gr_reg_4.tipo_seguro TO tipo_seguro
                    DISPLAY gr_reg_4.desc_seguro TO desc_seguro
                    
                WHEN INFIELD(tipo_pension)
                    CALL ventana_ayuda(5) RETURNING gr_reg_5.*
                    
                    IF INT_FLAG = TRUE THEN
                       LET INT_FLAG = FALSE
                       LET gr_reg_5.tipo_pension = NULL
                       LET gr_reg_5.desc_pension = NULL
                    END IF
                    
                    DISPLAY gr_reg_5.tipo_pension TO tipo_pension
                    DISPLAY gr_reg_5.desc_pension TO desc_pension
                    
                WHEN INFIELD(diag_procesar)
                    CALL ventana_ayuda(6) RETURNING vdiag_procesar
                    
                    IF INT_FLAG = TRUE THEN
                       LET INT_FLAG = FALSE
                       LET vdiag_procesar = NULL
                    END IF
                    
                    DISPLAY vdiag_procesar TO diag_procesar
                
                WHEN INFIELD(estado_solicitud)
                    CALL ventana_ayuda(7) RETURNING gr_reg_7.*
                    
                    IF INT_FLAG = TRUE THEN
                       LET INT_FLAG = FALSE
                       LET gr_reg_7.estado_solicitud = NULL
                       LET gr_reg_7.desc_estado      = NULL
                    END IF
                    
                    DISPLAY gr_reg_7.estado_solicitud TO estado_solicitud
                    DISPLAY gr_reg_7.desc_estado      TO desc_estado
                    
                WHEN INFIELD(diag_afore)
                    CALL ventana_ayuda(8) RETURNING gr_reg_8.*
                    
                    IF INT_FLAG = TRUE THEN
                       LET INT_FLAG = FALSE
                       LET gr_reg_8.diag_afore      = NULL
                       LET gr_reg_8.desc_diag_afore = NULL
                    END IF
                    
                    DISPLAY gr_reg_8.diag_afore      TO diag_afore
                    DISPLAY gr_reg_8.desc_diag_afore TO desc_diag_afore

                WHEN INFIELD(folio)
                    CALL ventana_ayuda(9) RETURNING vfolio_transf
                    
                    IF INT_FLAG = TRUE THEN
                       LET INT_FLAG = FALSE
                       LET vfolio_transf = NULL
                    END IF
                    
                    DISPLAY vfolio_transf TO folio

            END CASE

        ON KEY (CONTROL-C)
            LET INT_FLAG = TRUE
            EXIT CONSTRUCT

        ON KEY (INTERRUPT)
            LET INT_FLAG = TRUE
                EXIT CONSTRUCT

        ON KEY (Esc)
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT
    END CONSTRUCT

    IF int_flag = TRUE THEN
       LET int_flag = FALSE
        CLOSE WINDOW retm8152
        RETURN
    END IF

    DISPLAY "  PROCESANDO INFORMACION...  " AT 21,1
    ATTRIBUTE(REVERSE)

    LET txt_1 = " SELECT A.nss             ,           ",
                       " B.n_rfc           ,           ",
                       " A.curp            ,           ",
                       " A.paterno_afore   ,           ",
                       " A.materno_afore   ,           ",
                       " A.nombre_afore    ,           ",
                       " A.tipo_retiro     ,           ",
                       " ''                ,           ", #desc_tipo_ret
                       " A.tipo_prestacion ,           ",
                       " ''                ,           ", #desc_prestacion
                       " A.regimen         ,           ",
                       " ''                ,           ", #desc_regimen
                       " A.tipo_seguro     ,           ",
                       " ''                ,           ", #desc_seguro
                       " A.tipo_pension    ,           ",
                       " ''                ,           ", #desc_pension
                       " A.sec_pension     ,           ",
                       " A.diag_registro   ,           ",
                       " A.fecha_resolucion,           ",
                       " A.fecha_ini_pen   ,           ",
                       " A.porcentaje_val  ,           ",
                       " A.porcentaje_ret97,           ",
                       " A.porcentaje_cv   ,           ",
                       " A.porcentaje_cs   ,           ",
                       " A.porcentaje_viv  ,           ",
                       " A.folio           ,           ",
                       " A.consecutivo     ,           ",
                       " A.estado_solicitud,           ",
                       " ''                ,           ", #desc_estado
                       " G.diag_registro   ,           ",
                       " ''                ,           ", #desc_diag_afore
                       " C.fecha_carga     ,           ",
                       " C.nom_archivo     ,           ",
                       " ''                ,           ", #fecha_provision
                       " ''                ,           ", #usr_provision
                       " ''                ,           ", #fecha_envio
                       " ''                ,           ", #usr_envio
                       " ''                ,           ", #fecha_liquida
                       " ''                            ", #usr_liquida
                " FROM   ret_transf_rx             A , ",
                       " afi_mae_afiliado          B , ",
                       " ret_cza_lote              C , ",
                       " OUTER ret_transf_tx       G , ",
                       " OUTER ret_ctr_envio_lote  F   ",
                " WHERE ",x_busca CLIPPED,         
                " AND    A.nss         = B.n_seguro    ",
                " AND    A.folio       = C.folio       ",
                " AND    A.folio       = F.folio       ",
                " AND    A.tipo_retiro = F.tipo_retiro ",
                condicion CLIPPED,
                " ORDER BY 7,1                         "

    PREPARE pre_reg1 FROM txt_1
    DECLARE cur_reg1 CURSOR FOR pre_reg1

    LET i = 1
    FOREACH cur_reg1 INTO ga_reg_1[i].*
 
        SELECT descripcion 
        INTO   ga_reg_1[i].desc_tipo_ret 
        FROM   tab_retiro
        WHERE  tipo_retiro = ga_reg_1[i].tipo_retiro

        SELECT descripcion
        INTO   ga_reg_1[i].desc_prestacion
        FROM   tab_prestacion
        WHERE  tipo_prestacion = ga_reg_1[i].tipo_prestacion

        SELECT descripcion
        INTO   ga_reg_1[i].desc_regimen
        FROM   tab_regimen
        WHERE  regimen = ga_reg_1[i].regimen

        SELECT descripcion
        INTO   ga_reg_1[i].desc_seguro
        FROM   tab_seguro
        WHERE  clave = ga_reg_1[i].tipo_seguro

        SELECT descripcion
        INTO   ga_reg_1[i].desc_pension
        FROM   tab_pension
        WHERE  tipo_pension = ga_reg_1[i].tipo_pension

        SELECT descripcion
        INTO   ga_reg_1[i].desc_estado        
        FROM   ret_estado
        WHERE  estado_solicitud = ga_reg_1[i].estado_solicitud

        IF  ga_reg_1[i].estado_solicitud = 2 THEN
            LET ga_reg_1[i].desc_estado = "PROVISIONADO"
        END IF

        SELECT A.fecha_conversion ,
               A.usuario
        INTO   ga_reg_1[i].fecha_provision ,
               ga_reg_1[i].usr_provision 
        FROM   dis_provision A
        WHERE  A.nss              = ga_reg_1[i].nss
        AND    A.consecutivo_lote = ga_reg_1[i].consecutivo
        GROUP BY 1,2
        
        SELECT B.fecha_envio   ,
               B.usuario_envio 
        INTO   ga_reg_1[i].fecha_envio ,
               ga_reg_1[i].usr_envio
        FROM   ret_ctr_envio_lote B
        WHERE  B.folio       = ga_reg_1[i].folio
        AND    B.tipo_retiro = ga_reg_1[i].tipo_retiro
        GROUP BY 1,2
      
        SELECT C.fecha_conversion ,
               C.usuario
        INTO   ga_reg_1[i].fecha_liquida ,
               ga_reg_1[i].usr_liquida 
        FROM   dis_cuenta C
        WHERE  C.nss              = ga_reg_1[i].nss
        AND    C.consecutivo_lote = ga_reg_1[i].consecutivo
        GROUP BY 1,2

        SELECT descripcion
        INTO   ga_reg_1[i].desc_diag_afore        
        FROM   tab_diagnostico
        WHERE  cod_diagnostico = ga_reg_1[i].diag_afore

        SELECT B.marca_desc 
        INTO   ga_reg_1[i].edo_marca
        FROM   cta_act_marca A ,
               tab_marca     B
        WHERE  A.marca_cod = B.marca_cod
        AND    A.nss         = ga_reg_1[i].nss
        AND    A.correlativo = ga_reg_1[i].consecutivo
        
        IF STATUS = NOTFOUND THEN
            LET ga_reg_1[i].edo_marca = "CUENTA LIBRE"
        END IF 
                  
        LET i = i + 1
    END FOREACH    

    LET cont_reg = i-1

    DISPLAY "  TOTAL DE REGISTROS  >> ",cont_reg," " AT 21,1
    ATTRIBUTE(REVERSE)

    IF i <= 1 THEN
        INITIALIZE ga_reg_1[i].* TO NULL
        CLEAR FORM
        ERROR "  NO EXISTE REGISTRO  " ATTRIBUTE(NORMAL)
        CLOSE WINDOW retm8152
        RETURN
    END IF

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY ga_reg_1 TO scr_1.*
        ON KEY (CONTROL-V)
            LET arr_c = ARR_CURR()
            
            CALL muestra_detalle(ga_reg_1[arr_c].nss           ,
                                 ga_reg_1[arr_c].consecutivo   ,
                                 ga_reg_1[arr_c].folio         ,
                                 ga_reg_1[arr_c].nombre_afore  ,
                                 ga_reg_1[arr_c].paterno_afore ,
                                 ga_reg_1[arr_c].materno_afore
                                )

        ON KEY ( CONTROL-C )
            CALL init()
            EXIT DISPLAY
        ON KEY ( INTERRUPT )
            CALL init()
            EXIT DISPLAY
    END DISPLAY

    CLOSE WINDOW retm8152 
END FUNCTION

FUNCTION muestra_detalle(la_reg_1)
#md----------------------------------------------                        
                       
    DEFINE la_reg_1 RECORD #loc #la_reg_1
        nss                  LIKE ret_transf_rx.nss               ,
        consecutivo          INTEGER                              ,
        folio                LIKE ret_transf_rx.folio             , 
        nombre_afore         LIKE ret_transf_rx.nombre_afore      ,
        paterno_afore        LIKE ret_transf_rx.paterno_afore     ,
        materno_afore        LIKE ret_transf_rx.materno_afore     
    END RECORD 

    DEFINE la_reg_2 ARRAY[100] OF RECORD #loc #la_reg_2	 
        siefore              LIKE dis_cuenta.siefore              ,
        subcuenta            LIKE dis_cuenta.subcuenta            ,   
        mto_acc_prov         LIKE dis_provision.monto_en_acciones ,
        mto_pes_prov         LIKE dis_provision.monto_en_pesos    ,
        mto_acc_liq          LIKE dis_cuenta.monto_en_acciones    ,
        mto_pes_liq          LIKE dis_cuenta.monto_en_pesos
    END RECORD 
    
    DEFINE lr_precios ARRAY[4] OF RECORD #loc #lr_precios
        sief                 LIKE dis_provision.siefore           ,
        prec_sief_prov       LIKE dis_provision.precio_accion     ,
        f_valor_prov         LIKE dis_provision.fecha_valor       ,
        f_conver_prov        LIKE dis_provision.fecha_conversion  ,
        prec_sief_liq        LIKE dis_cuenta.precio_accion        ,
        f_valor_liq          LIKE dis_cuenta.fecha_valor          ,
        f_conver_liq         LIKE dis_cuenta.fecha_conversion
    END RECORD     
     
   DEFINE vsiefore           LIKE dis_cuenta.siefore              
   DEFINE vsubcuenta         LIKE dis_cuenta.subcuenta     
        
   DEFINE sel_where1         CHAR(500) ,
          sel_where2         CHAR(500)
          
   DEFINE i,j,k              INTEGER        
          
    CALL init()
    
    OPEN WINDOW retm8153 AT 2,2 WITH FORM "RETM8152" ATTRIBUTE (BORDER)
    DISPLAY "                              <CTRL-C> SALIR                                   " AT 1,1
    DISPLAY " RETM815(1.0)            DETALLES DE TRANSFERENCIAS                            " AT 3,1  ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "                   P R O V I S I O N            L I Q U I D A C I O N          " AT 8,1  ATTRIBUTE(REVERSE)
    DISPLAY "SIEF SUBCTA   ACCIONES        PESOS                ACCIONES         PESOS      " AT 14,1 ATTRIBUTE(REVERSE)

    CLEAR FORM

    DISPLAY la_reg_1.nss           ,
            la_reg_1.nombre_afore  ,
            la_reg_1.paterno_afore ,
            la_reg_1.materno_afore  TO nss           , 
                                       nombre_afore  ,
                                       paterno_afore ,
                                       materno_afore

    DECLARE cur_precios CURSOR FOR 
    SELECT A.siefore          ,
           A.precio_accion    ,
           A.fecha_valor      ,
           A.fecha_conversion ,
           B.precio_accion    ,
           B.fecha_valor      ,
           B.fecha_conversion
    FROM   dis_provision A ,
           OUTER dis_cuenta    B
    WHERE  A.nss              = la_reg_1.nss
    AND    A.consecutivo_lote = la_reg_1.consecutivo
    AND    A.folio            = la_reg_1.folio
    AND    A.siefore          IN (1,2,11)
    AND    A.nss              = B.nss
    AND    A.consecutivo_lote = B.consecutivo_lote
    AND    A.folio            = B.folio
    AND    A.siefore          = B.siefore
    GROUP BY 1,2,3,4,5,6,7
    ORDER BY 1

    LET k = 1
    FOREACH cur_precios INTO lr_precios[k].*
        LET k = k +1
    END FOREACH     

    CALL SET_COUNT(k-1)

    INPUT ARRAY lr_precios WITHOUT  DEFAULTS FROM scr_2.*
        BEFORE INPUT 
            EXIT INPUT     
    END INPUT  

    LET int_flag = FALSE 

    IF int_flag = TRUE THEN
       LET int_flag = FALSE
        CLOSE WINDOW retm8153
        RETURN
    END IF


    LET sel_where1 = "SELECT A.siefore                ,",
                            "A.subcuenta              ,",
                            "sum(-A.monto_en_acciones),",
                            "sum(-A.monto_en_pesos)    ", 
                     "FROM   dis_provision A           ",
                     "WHERE  A.nss              = ",la_reg_1.nss         ,       
                     "AND    A.consecutivo_lote = ",la_reg_1.consecutivo ,
                     "AND    A.folio            = ",la_reg_1.folio       , 
                     "GROUP BY 1,2                ",                     
                     "ORDER BY 1,2                "
                       
    PREPARE query11 FROM sel_where1
    DECLARE cursor_1 CURSOR FOR query11

    LET sel_where2 = "SELECT B.siefore                ,",
                            "B.subcuenta              ,",
                            "sum(-B.monto_en_acciones),",
                            "sum(-B.monto_en_pesos)    ", 
                     "FROM   dis_cuenta B              ",
                     "WHERE  B.nss              = ",la_reg_1.nss         ,    
                     "AND    B.consecutivo_lote = ",la_reg_1.consecutivo ,
                     "AND    B.folio            = ",la_reg_1.folio       ,
                     "GROUP BY 1,2                "                      ,
                     "ORDER BY 1,2                "
                     
    PREPARE query22 FROM sel_where2
    DECLARE cursor_2 CURSOR FOR query22

    LET i = 1

    FOREACH cursor_1 INTO la_reg_2[i].siefore      ,
                          la_reg_2[i].subcuenta    ,
                          la_reg_2[i].mto_acc_prov ,
                          la_reg_2[i].mto_pes_prov
        DISPLAY "  MONTOS PROVISIONADOS  " AT 21,1 ATTRIBUTE(REVERSE) 
        LET i = i + 1    
    END FOREACH


    LET j = 1
    FOREACH cursor_2 INTO vsiefore                ,
                          vsubcuenta              ,
                          la_reg_2[j].mto_acc_liq ,
                          la_reg_2[j].mto_pes_liq
        DISPLAY "  MONTOS PROVISIONADOS Y TRANSFERIDOS  " AT 21,1 ATTRIBUTE(REVERSE) 
        LET j = j + 1                      
    END FOREACH 


    IF i = 1 THEN
        CLEAR FORM
        ERROR "  EL NSS NO HA SIDO PROVISIONADO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW retm8153
        RETURN
    END IF

    CALL SET_COUNT(i-1)


    DISPLAY ARRAY la_reg_2 TO scr_3.*

        ON KEY ( CONTROL-C )
            CALL init()
            EXIT DISPLAY
        ON KEY ( INTERRUPT )
            CALL init()
            EXIT DISPLAY
    END DISPLAY

CLOSE WINDOW retm8153
END FUNCTION

FUNCTION ventana_ayuda(opcion)
    DEFINE #loc #smallint 
        cur                  ,
        opcion               SMALLINT     

    DEFINE la_tipo_ret ARRAY[5] OF  RECORD #loc #la_tipo_ret
        tipo_retiro          LIKE tab_retiro.tipo_retiro ,
        desc_tipo_ret        LIKE tab_retiro.descripcion        
    END RECORD 

    DEFINE la_tipo_pres ARRAY[5] OF  RECORD #loc #la_tipo_pres
        tipo_prestacion      LIKE tab_prestacion.tipo_prestacion ,
        desc_prestacion      LIKE tab_prestacion.descripcion        
    END RECORD 

    DEFINE la_regimen ARRAY[3] OF  RECORD #loc #la_regimen
        regimen              LIKE tab_regimen.regimen    ,
        desc_regimen         LIKE tab_regimen.descripcion        
    END RECORD 

    DEFINE la_tipo_seguro ARRAY[5] OF  RECORD #loc #la_tipo_seguro
        tipo_seguro          LIKE tab_seguro.clave    ,
        desc_seguro          LIKE tab_seguro.descripcion        
    END RECORD 

    DEFINE la_tipo_pension ARRAY[9] OF  RECORD #loc #la_tipo_pension
        tipo_pension         LIKE tab_pension.tipo_pension ,
        desc_pension         LIKE tab_pension.descripcion        
    END RECORD 

    DEFINE la_diag_procesar ARRAY[9] OF  RECORD #loc #la_diag_procesar
        diag_procesar        LIKE tab_diagnostico.cod_diagnostico ,
        desc_diag_proc       LIKE tab_diagnostico.descripcion        
    END RECORD 

    DEFINE la_estado ARRAY[9] OF  RECORD #loc #la_estado
        estado_solicitud     LIKE ret_estado.estado_solicitud ,
        desc_estado          LIKE ret_estado.descripcion        
    END RECORD 

    DEFINE la_diag_afore ARRAY[9] OF  RECORD #loc #la_diag_afore
        diag_afore           LIKE tab_diagnostico.cod_diagnostico ,
        desc_diag_afore      LIKE tab_diagnostico.descripcion        
    END RECORD 

    DEFINE la_folio ARRAY[1000] OF  RECORD #loc #la_folio
        folio                LIKE ret_transf_rx.folio      ,
        fecha_carga          LIKE ret_cza_lote.fecha_carga ,
        nom_archivo          LIKE ret_cza_lote.nom_archivo
    END RECORD 

    CASE opcion
        WHEN 1
        
            OPEN WINDOW retm8153 AT 10,21 WITH FORM "RETM8153" ATTRIBUTE(BORDER)
            DISPLAY "            <ENTER> PARA SELECCIONAR RETIRO                  " AT 1,1
            DISPLAY "               USE LAS FLECHAS PARA MOVER                    " AT 2,1
            DISPLAY " RETIRO               DESCRIPCION                            " AT 3,1 ATTRIBUTE(REVERSE)
            
            DECLARE etiq_tipo_ret CURSOR FOR
            SELECT A.tipo_retiro ,
                   B.descripcion
            FROM   tab_tramite_retiro A ,
                   tab_retiro         B
            WHERE  A.tipo_retiro = B.tipo_retiro
            AND    A.cod_tramite = 1
            ORDER BY A.tipo_retiro
              
            LET i = 1
              
            FOREACH etiq_tipo_ret INTO la_tipo_ret[i].*
               LET i = i + 1
            END FOREACH
              	
            FREE etiq_tipo_ret
             
            LET INT_FLAG = FALSE
             
            CALL SET_COUNT(i-1)
             
            DISPLAY ARRAY la_tipo_ret TO sa_tipo_ret.*
                ON KEY (CONTROL-M)
                    LET cur = ARR_CURR()
                    EXIT DISPLAY 
                ON KEY (CONTROL-C)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_tipo_ret[cur].tipo_retiro   = NULL 
                    LET la_tipo_ret[cur].desc_tipo_ret = NULL 
                    EXIT DISPLAY
                ON KEY (INTERRUPT)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_tipo_ret[cur].tipo_retiro   = NULL 
                    LET la_tipo_ret[cur].desc_tipo_ret = NULL 
                    EXIT DISPLAY
                ON KEY (ESC)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_tipo_ret[cur].tipo_retiro   = NULL 
                    LET la_tipo_ret[cur].desc_tipo_ret = NULL 
                    EXIT DISPLAY
            END DISPLAY
            CLOSE WINDOW retm8153
            
            RETURN la_tipo_ret[cur].tipo_retiro,
                   la_tipo_ret[cur].desc_tipo_ret
        
        WHEN 2
            OPEN WINDOW retm8154 AT 11,26 WITH FORM "RETM8154" ATTRIBUTE(BORDER)
            DISPLAY " <ENTER> PARA SELECCIONAR PRESTACION              " AT 1,1
            DISPLAY "     USE LAS FLECHAS PARA MOVER                   " AT 2,1
            DISPLAY " PRESTACION       DESCRIPCION                     " AT 3,1 ATTRIBUTE(REVERSE)
            
            IF gr_reg_1.tipo_retiro IS NULL OR gr_reg_1.tipo_retiro = "" THEN
                DECLARE etiq_tipo_pres CURSOR FOR
                SELECT C.tipo_prestacion ,
                       C.descripcion
                FROM   ret_matriz_derecho A ,
                       tab_tramite_retiro B ,
                       tab_prestacion     C
                WHERE  A.tipo_retiro     = B.tipo_retiro
                AND    A.tipo_prestacion = C.tipo_prestacion
                AND    B.cod_tramite = 1
                GROUP BY 1,2
                ORDER BY 1,2
                
                LET i = 1
                
                FOREACH etiq_tipo_pres INTO la_tipo_pres[i].*
                   LET i = i + 1
                END FOREACH
                	
  	            FREE etiq_tipo_pres
  	        ELSE
                DECLARE etiq_tipo_pres1 CURSOR FOR
                SELECT C.tipo_prestacion ,
                       C.descripcion
                FROM   ret_matriz_derecho A ,
                       tab_tramite_retiro B ,
                       tab_prestacion     C
                WHERE  A.tipo_retiro     = B.tipo_retiro
                AND    A.tipo_prestacion = C.tipo_prestacion
                AND    B.cod_tramite     = 1
                AND    A.tipo_retiro     = gr_reg_1.tipo_retiro
                GROUP BY 1,2
                ORDER BY 1,2
                
                LET i = 1
                
                FOREACH etiq_tipo_pres1 INTO la_tipo_pres[i].*
                   LET i = i + 1
                END FOREACH
                	
  	            FREE etiq_tipo_pres1
  	        END IF 
            
            LET INT_FLAG = FALSE
            
            CALL SET_COUNT(i-1)
            
            DISPLAY ARRAY la_tipo_pres TO sa_tipo_pres.*
                ON KEY (CONTROL-M)
                    LET cur = ARR_CURR()
                    EXIT DISPLAY 
                ON KEY (CONTROL-C)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_tipo_pres[cur].tipo_prestacion = NULL 
                    LET la_tipo_pres[cur].desc_prestacion = NULL 
                    EXIT DISPLAY
                ON KEY (INTERRUPT)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_tipo_pres[cur].tipo_prestacion = NULL 
                    LET la_tipo_pres[cur].desc_prestacion = NULL 
                    EXIT DISPLAY
                ON KEY (ESC)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_tipo_pres[cur].tipo_prestacion = NULL 
                    LET la_tipo_pres[cur].desc_prestacion = NULL 
                    EXIT DISPLAY
            END DISPLAY
            CLOSE WINDOW retm8154
            
            RETURN la_tipo_pres[cur].tipo_prestacion ,
                   la_tipo_pres[cur].desc_prestacion

        WHEN 3
            OPEN WINDOW retm8155 AT 12,26 WITH FORM "RETM8155" ATTRIBUTE(BORDER)
            DISPLAY "       <ENTER> PARA SELECCIONAR REGIMEN                " AT 1,1
            DISPLAY "          USE LAS FLECHAS PARA MOVER                   " AT 2,1
            DISPLAY " REGIMEN             DESCRIPCION                       " AT 3,1 ATTRIBUTE(REVERSE)
            
            IF gr_reg_1.tipo_retiro IS NULL OR gr_reg_1.tipo_retiro = "" THEN
                DECLARE etiq_regimen CURSOR FOR
                SELECT C.regimen ,
                       C.descripcion
                FROM   tab_regimen C          
                ORDER BY 1,2
                
                LET i = 1
                
                FOREACH etiq_regimen INTO la_regimen[i].*
                   LET i = i + 1
                END FOREACH
                	
  	            FREE etiq_regimen
  	        ELSE
                DECLARE etiq_regimen1 CURSOR FOR
                SELECT C.regimen ,
                       C.descripcion
                FROM   tab_regimen        C ,
                       ret_matriz_derecho D
                WHERE  C.regimen = D.regimen
                AND    D.tipo_retiro = gr_reg_1.tipo_retiro
                GROUP BY 1,2      
                ORDER BY 1,2
                
                LET i = 1
                
                FOREACH etiq_regimen1 INTO la_regimen[i].*
                   LET i = i + 1
                END FOREACH
                	
  	            FREE etiq_regimen1
  	        END IF 
            
            LET INT_FLAG = FALSE
            
            CALL SET_COUNT(i-1)
            
            DISPLAY ARRAY la_regimen TO sa_regimen.*
                ON KEY (CONTROL-M)
                    LET cur = ARR_CURR()
                    EXIT DISPLAY 
                ON KEY (CONTROL-C)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_regimen[cur].regimen      = NULL 
                    LET la_regimen[cur].desc_regimen = NULL 
                    EXIT DISPLAY
                ON KEY (INTERRUPT)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_regimen[cur].regimen      = NULL 
                    LET la_regimen[cur].desc_regimen = NULL 
                    EXIT DISPLAY
                ON KEY (ESC)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_regimen[cur].regimen      = NULL 
                    LET la_regimen[cur].desc_regimen = NULL 
                    EXIT DISPLAY
            END DISPLAY
            CLOSE WINDOW retm8155
            
            RETURN la_regimen[cur].regimen     ,
                   la_regimen[cur].desc_regimen

        WHEN 4
            OPEN WINDOW retm8156 AT 13,26 WITH FORM "RETM8156" ATTRIBUTE(BORDER)
            DISPLAY "      <ENTER> PARA SELECCIONAR SEGURO                  " AT 1,1
            DISPLAY "          USE LAS FLECHAS PARA MOVER                   " AT 2,1
            DISPLAY " SEGURO              DESCRIPCION                       " AT 3,1 ATTRIBUTE(REVERSE)
            
            IF gr_reg_1.tipo_retiro IS NULL OR gr_reg_1.tipo_retiro = "" THEN
                DECLARE etiq_tipo_seguro CURSOR FOR
                SELECT A.tipo_seguro ,
                       B.descripcion
                FROM   ret_matriz_derecho A,
                       tab_seguro         B,
                       tab_tramite_retiro C
                WHERE  A.tipo_seguro = B.clave
                AND    A.tipo_retiro = C.tipo_retiro
                AND    C.cod_tramite = 1
                GROUP BY 1,2
                ORDER BY 1,2
                
                LET i = 1
                
                FOREACH etiq_tipo_seguro INTO la_tipo_seguro[i].*
                   LET i = i + 1
                END FOREACH
                	
  	            FREE etiq_tipo_seguro
  	        ELSE
                DECLARE etiq_tipo_seguro1 CURSOR FOR
                SELECT A.tipo_seguro ,
                       B.descripcion
                FROM   ret_matriz_derecho A,
                       tab_seguro         B,
                       tab_tramite_retiro C
                WHERE  A.tipo_seguro = B.clave
                AND    A.tipo_retiro = C.tipo_retiro
                AND    C.cod_tramite = 1
                AND    A.tipo_retiro = gr_reg_1.tipo_retiro
                GROUP BY 1,2
                ORDER BY 1,2
                
                LET i = 1
                
                FOREACH etiq_tipo_seguro1 INTO la_tipo_seguro[i].*
                   LET i = i + 1
                END FOREACH
                	
  	            FREE etiq_tipo_seguro1
  	        END IF
            
            LET INT_FLAG = FALSE
            
            CALL SET_COUNT(i-1)
            
            DISPLAY ARRAY la_tipo_seguro TO sa_tipo_seguro.*
                ON KEY (CONTROL-M)
                    LET cur = ARR_CURR()
                    EXIT DISPLAY 
                ON KEY (CONTROL-C)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_tipo_seguro[cur].tipo_seguro = NULL 
                    LET la_tipo_seguro[cur].desc_seguro = NULL 
                    EXIT DISPLAY
                ON KEY (INTERRUPT)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_tipo_seguro[cur].tipo_seguro = NULL 
                    LET la_tipo_seguro[cur].desc_seguro = NULL 
                    EXIT DISPLAY
                ON KEY (ESC)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_tipo_seguro[cur].tipo_seguro = NULL 
                    LET la_tipo_seguro[cur].desc_seguro = NULL 
                    EXIT DISPLAY
            END DISPLAY
            CLOSE WINDOW retm8156
            
            RETURN la_tipo_seguro[cur].tipo_seguro ,
                   la_tipo_seguro[cur].desc_seguro

        WHEN 5
            OPEN WINDOW retm8157 AT 14,26 WITH FORM "RETM8157" ATTRIBUTE(BORDER)
            DISPLAY "      <ENTER> PARA SELECCIONAR PENSION                 " AT 1,1
            DISPLAY "          USE LAS FLECHAS PARA MOVER                   " AT 2,1
            DISPLAY " PENSION             DESCRIPCION                       " AT 3,1 ATTRIBUTE(REVERSE)
            
            IF gr_reg_1.tipo_retiro IS NULL OR gr_reg_1.tipo_retiro = "" THEN
                DECLARE etiq_tipo_pension CURSOR FOR
                SELECT A.tipo_pension ,
                       B.descripcion
                FROM   ret_matriz_derecho A ,
                       tab_pension        B ,
                       tab_tramite_retiro C
                WHERE  A.tipo_retiro  = C.tipo_retiro
                AND    A.tipo_pension = B.tipo_pension
                AND    C.cod_tramite  = 1
                GROUP BY 1,2
                ORDER BY 1,2
                
                LET i = 1
                
                FOREACH etiq_tipo_pension INTO la_tipo_pension[i].*
                   LET i = i + 1
                END FOREACH
                	
  	            FREE etiq_tipo_pension
  	        ELSE
                DECLARE etiq_tipo_pension1 CURSOR FOR
                SELECT A.tipo_pension ,
                       B.descripcion
                FROM   ret_matriz_derecho A ,
                       tab_pension        B ,
                       tab_tramite_retiro C
                WHERE  A.tipo_retiro  = C.tipo_retiro
                AND    A.tipo_pension = B.tipo_pension
                AND    C.cod_tramite  = 1
                AND    A.tipo_retiro  = gr_reg_1.tipo_retiro
                GROUP BY 1,2
                ORDER BY 1,2
                
                LET i = 1
                
                FOREACH etiq_tipo_pension1 INTO la_tipo_pension[i].*
                   LET i = i + 1
                END FOREACH
                	
  	            FREE etiq_tipo_pension1
  	        END IF

            LET INT_FLAG = FALSE
            
            CALL SET_COUNT(i-1)
            
            DISPLAY ARRAY la_tipo_pension TO sa_tipo_pension.*
                ON KEY (CONTROL-M)
                    LET cur = ARR_CURR()
                    EXIT DISPLAY 
                ON KEY (CONTROL-C)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_tipo_pension[cur].tipo_pension = NULL 
                    LET la_tipo_pension[cur].desc_pension = NULL 
                    EXIT DISPLAY
                ON KEY (INTERRUPT)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_tipo_pension[cur].tipo_pension = NULL 
                    LET la_tipo_pension[cur].desc_pension = NULL 
                    EXIT DISPLAY
                ON KEY (ESC)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_tipo_pension[cur].tipo_pension = NULL 
                    LET la_tipo_pension[cur].desc_pension = NULL 
                    EXIT DISPLAY
            END DISPLAY
            CLOSE WINDOW retm8157
            
            RETURN la_tipo_pension[cur].tipo_pension ,
                   la_tipo_pension[cur].desc_pension

        WHEN 6
            OPEN WINDOW retm8158 AT 11,20 WITH FORM "RETM8158" ATTRIBUTE(BORDER)
            DISPLAY "     <ENTER> PARA SELECCIONAR DIAGNOSTICO          " AT 1,1
            DISPLAY "         USE LAS FLECHAS PARA MOVER                " AT 2,1
            DISPLAY " DIAGNOSTICO         DESCRIPCION                   " AT 3,1 ATTRIBUTE(REVERSE)
            
            DECLARE etiq_diag_procesar CURSOR FOR
            SELECT cod_diagnostico ,
                   descripcion
            FROM   tab_diagnostico
            WHERE  entidad    = 3
            AND    modulo_cod = "ret"
            ORDER BY 1

            LET i = 1
            
            FOREACH etiq_diag_procesar INTO la_diag_procesar[i].*
               LET i = i + 1
            END FOREACH
            	
  	        FREE etiq_diag_procesar
            
            LET INT_FLAG = FALSE
            
            CALL SET_COUNT(i-1)
            
            DISPLAY ARRAY la_diag_procesar TO sa_diag_procesar.*
                ON KEY (CONTROL-M)
                    LET cur = ARR_CURR()
                    EXIT DISPLAY 
                ON KEY (CONTROL-C)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_diag_procesar[cur].diag_procesar = NULL 
                    LET la_diag_procesar[cur].desc_diag_proc = NULL 
                    EXIT DISPLAY
                ON KEY (INTERRUPT)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_diag_procesar[cur].diag_procesar = NULL 
                    LET la_diag_procesar[cur].desc_diag_proc = NULL 
                    EXIT DISPLAY
                ON KEY (ESC)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_diag_procesar[cur].diag_procesar = NULL 
                    LET la_diag_procesar[cur].desc_diag_proc = NULL 
                    EXIT DISPLAY
            END DISPLAY
            CLOSE WINDOW retm8158
            
            RETURN la_diag_procesar[cur].diag_procesar 

        WHEN 7
            OPEN WINDOW retm8159 AT 14,26 WITH FORM "RETM8159" ATTRIBUTE(BORDER)
            DISPLAY " <ENTER> PARA SELECCIONAR ESTADO " AT 1,1
            DISPLAY "    USE LAS FLECHAS PARA MOVER   " AT 2,1
            DISPLAY " ESTADO     DESCRIPCION          " AT 3,1 ATTRIBUTE(REVERSE)
            
            DECLARE etiq_estado_solicitud CURSOR FOR
            SELECT *
            FROM   ret_estado
            WHERE  estado_solicitud IN (1,2,4,8)
            ORDER BY 1

            LET i = 1
            
            FOREACH etiq_estado_solicitud INTO la_estado[i].*
               IF la_estado[i].estado_solicitud = 2 THEN
                   LET la_estado[i].desc_estado = "PROVISIONADO"
               END IF
               	
               LET i = i + 1
            END FOREACH
            	
  	        FREE etiq_estado_solicitud
            
            LET INT_FLAG = FALSE
            
            CALL SET_COUNT(i-1)
            
            DISPLAY ARRAY la_estado TO sa_estado.*
                ON KEY (CONTROL-M)
                    LET cur = ARR_CURR()
                    EXIT DISPLAY 
                ON KEY (CONTROL-C)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_estado[cur].estado_solicitud = NULL 
                    LET la_estado[cur].desc_estado      = NULL 
                    EXIT DISPLAY
                ON KEY (INTERRUPT)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_estado[cur].estado_solicitud = NULL 
                    LET la_estado[cur].desc_estado      = NULL 
                    EXIT DISPLAY
                ON KEY (ESC)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_estado[cur].estado_solicitud = NULL 
                    LET la_estado[cur].desc_estado      = NULL 
                    EXIT DISPLAY
            END DISPLAY
            CLOSE WINDOW retm8159
            
            RETURN la_estado[cur].estado_solicitud     ,
                   la_estado[cur].desc_estado

        WHEN 8
            OPEN WINDOW retm81510 AT 12,26 WITH FORM "RETM81510" ATTRIBUTE(BORDER)
            DISPLAY "       <ENTER> PARA SELECCIONAR DIAGNOSTICO            " AT 1,1
            DISPLAY "          USE LAS FLECHAS PARA MOVER                   " AT 2,1
            DISPLAY " DIAGNOSTICO           DESCRIPCION                     " AT 3,1 ATTRIBUTE(REVERSE)
            
            DECLARE etiq_diag_afore CURSOR FOR
            SELECT cod_diagnostico ,
                   descripcion
            FROM   tab_diagnostico
            WHERE  entidad    = 1
            AND    modulo_cod = "ret"
            ORDER BY 1

            LET i = 1
            
            FOREACH etiq_diag_afore INTO la_diag_afore[i].*
               LET i = i + 1
            END FOREACH
            	
  	        FREE etiq_diag_afore
            
            LET INT_FLAG = FALSE
            
            CALL SET_COUNT(i-1)
            
            DISPLAY ARRAY la_diag_afore TO sa_diag_afore.*
                ON KEY (CONTROL-M)
                    LET cur = ARR_CURR()
                    EXIT DISPLAY 
                ON KEY (CONTROL-C)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_diag_afore[cur].diag_afore      = NULL 
                    LET la_diag_afore[cur].desc_diag_afore = NULL 
                    EXIT DISPLAY
                ON KEY (INTERRUPT)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_diag_afore[cur].diag_afore      = NULL 
                    LET la_diag_afore[cur].desc_diag_afore = NULL 
                    EXIT DISPLAY
                ON KEY (ESC)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_diag_afore[cur].diag_afore      = NULL 
                    LET la_diag_afore[cur].desc_diag_afore = NULL 
                    EXIT DISPLAY
            END DISPLAY
            CLOSE WINDOW retm81510
            
            RETURN la_diag_afore[cur].diag_afore     ,
                   la_diag_afore[cur].desc_diag_afore

        WHEN 9
            OPEN WINDOW retm81511 AT 14,25 WITH FORM "RETM81511" ATTRIBUTE(BORDER)
            DISPLAY "       <ENTER> PARA SELECCIONAR FOLIO                  " AT 1,1
            DISPLAY "          USE LAS FLECHAS PARA MOVER                   " AT 2,1
            DISPLAY "     FOLIO    FECHA DE CARGA     NOMBRE DE ARCHIVO     " AT 3,1 ATTRIBUTE(REVERSE)
            
            DECLARE etiq_folio  CURSOR FOR
            SELECT A.folio       ,
                   B.fecha_carga ,
                   B.nom_archivo
            FROM   ret_transf_rx A ,
                   ret_cza_lote  B
            WHERE  A.folio = B.folio
            GROUP BY 1,2,3
            ORDER BY A.folio DESC

            LET i = 1
            
            FOREACH etiq_folio INTO la_folio[i].*
               LET i = i + 1
            END FOREACH
            	
  	        FREE etiq_folio
            
            LET INT_FLAG = FALSE
            
            CALL SET_COUNT(i-1)
            
            DISPLAY ARRAY la_folio TO sa_folio.*
                ON KEY (CONTROL-M)
                    LET cur = ARR_CURR()
                    EXIT DISPLAY 
                ON KEY (CONTROL-C)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_folio[cur].folio = NULL 
                    EXIT DISPLAY
                ON KEY (INTERRUPT)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_folio[cur].folio = NULL 
                    EXIT DISPLAY
                ON KEY (ESC)
                    LET INT_FLAG = TRUE 
                    LET cur = ARR_CURR()
                    LET la_folio[cur].folio = NULL 
                    EXIT DISPLAY
            END DISPLAY
            CLOSE WINDOW retm81511
            
            RETURN la_folio[cur].folio

    END CASE 
END FUNCTION         
