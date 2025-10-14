-------------------------------------------------------------------------------
-- Proyecto      = Sistema de Afores.( MEXICO ) 
-- Propietario   = EFP                         
-- Programa      = AFIP004
-- Descripcion   = LIQUIDACION ACTIVACION DE MENORES DE EDAD
-------------------------------------------------------------------------------

DATABASE safre_af

-------------------------------------------------------------------------------

GLOBALS 

 DEFINE vusu               VARCHAR(10)
 DEFINE vsubtitulo         VARCHAR(45)
 DEFINE vprog              VARCHAR(15)
 DEFINE vbasura            CHAR(1)
 DEFINE v_arch             VARCHAR(50)
 DEFINE v_ruta_envio       CHAR(40)
 DEFINE v_ruta_rescate     CHAR(40)
 DEFINE v_ruta_listados    CHAR(40)
 DEFINE v_ruta_exp         CHAR(40)
 DEFINE lc_comando         CHAR(3000) 
 DEFINE lc_pausa           CHAR(1)
 
 DEFINE ld_folio_lote      DECIMAL(10)
 DEFINE li_folio_lote      INTEGER  
 DEFINE lv_folio_lote      VARCHAR(10)

 DEFINE v_ren              SMALLINT 
 DEFINE v_col              SMALLINT   
 
END GLOBALS 

-------------------------------------------------------------------------------

MAIN 

  CALL STARTLOG(FGL_GETENV("USER")||"AFIP004.log")  

  OPTIONS INPUT WRAP,
          PROMPT  LINE LAST,
          ERROR   LINE LAST,
          COMMENT LINE LAST,
          MESSAGE LINE LAST

  DEFER INTERRUPT

  -- POSICION DE VENTANA DE MENSAJES AL USUARIO 
  LET v_ren = 9
  LET v_col = 7 
  
  LET INT_FLAG = FALSE

  CALL fn_get_usuario()
       RETURNING vusu

  -- RUTAS   
  LET v_ruta_envio     = ""
  LET v_ruta_rescate   = ""
  LET v_ruta_listados  = ""
     
  SELECT ruta_envio,      ruta_rescate, 
         ruta_listados,   ruta_exp
    INTO v_ruta_envio,    v_ruta_rescate, 
         v_ruta_listados, v_ruta_exp 
    FROM safre_af:seg_modulo
   WHERE modulo_cod = "afi"
   
  LET vsubtitulo   = "LIQUIDACION DE ACTIVACION DE MENORES DE EDAD" 
  LET vprog        = "AFIP004" 

  OPEN WINDOW v1 AT 2,2 WITH 21 ROWS , 78 COLUMNS ATTRIBUTE(BORDER)

    CALL fn_encabezado(vsubtitulo,vprog,3)
          
    MENU ""
           
      COMMAND "Provision"
              CALL pide_folio(0)  -- PENDIENTE 
                   RETURNING ld_folio_lote
                     
              IF ld_folio_lote > 0 THEN 
                 CALL provision()
              END IF  
               
      COMMAND "Liquidacion" 
              CALL pide_folio(1)  -- PROVISIONADO
                   RETURNING ld_folio_lote 
                   
              IF ld_folio_lote > 0 THEN      
                 CALL liquidacion()
              END IF  
              
      COMMAND "Reportes"
      
              MENU "REPORTES"              
                   COMMAND "Provision"
                            CALL pide_folio(0)  -- PENDIENTE 
                                 RETURNING ld_folio_lote
                     
                            IF ld_folio_lote > 0 THEN 
                               CALL rep_pro_liq("PROVISION",ld_folio_lote)
                            END IF                             
                        
                   COMMAND "Liquidacion"
                           CALL pide_folio(1)  -- PROVISIONADO
                                RETURNING ld_folio_lote
                     
                           IF ld_folio_lote > 0 THEN 
              	              CALL rep_pro_liq("LIQUIDACION",ld_folio_lote)
                           END IF    
                           
                   COMMAND "Regresar"
                           EXIT MENU
              END MENU    
                                        
      COMMAND "Salir" "Salir"
              EXIT MENU
              
    END MENU
    CLOSE WINDOW v1

END MAIN 

-------------------------------------------------------------------------------

FUNCTION provision()

  DEFINE ls_subuenta      SMALLINT
  DEFINE ls_siefore       SMALLINT
  DEFINE ld_precio        DECIMAL(19,14)
  DEFINE ld_acciones      DECIMAL(22,6)
  DEFINE ld_pesos         DECIMAL(22,6)
  DEFINE ls_proceso       SMALLINT 
  DEFINE lr_reg           RECORD LIKE afi_act_menor_edad.* 
  DEFINE lr_sol           RECORD LIKE afi_solicitud.* 
  DEFINE lr_saldo         RECORD 
                            subcuenta   SMALLINT,
                            siefore     SMALLINT,
                            acciones    DECIMAL(16,6),
                            pesos       DECIMAL(16,6)
                          END RECORD
  DEFINE ld_accion_neg    DECIMAL(16,6)
  DEFINE ld_pesos_neg     DECIMAL(16,6)
  DEFINE ld_precio_acc    DECIMAL(19,14) 
  DEFINE lr_tpomov        RECORD 
                            unificado   SMALLINT,
                            unificador  SMALLINT
                          END RECORD
  DEFINE lc_nss_cargo     CHAR(11)
  DEFINE lc_nss_abono     CHAR(11)    
  DEFINE ls_realiza_ap    SMALLINT 

  DEFINE lr_afisol_b      RECORD LIKE afi_solicitud.*
  DEFINE lr_act_menor_b   RECORD LIKE afi_act_menor_edad.*
  DEFINE ls_marca_622     SMALLINT 
  DEFINE ls_marca_621     SMALLINT 
  DEFINE ls_marca_150     SMALLINT   -- MARCA DE INHABILITACION    
  
  DEFINE ls_estado_marca  SMALLINT
  DEFINE ls_marca_causa   SMALLINT
  
  DEFINE v_estado_marca   SMALLINT 
  DEFINE v_codigo_rechazo SMALLINT 
  DEFINE v_marca_causa    SMALLINT
  DEFINE v_fecha_causa    DATE 
  DEFINE v_correlativo    INTEGER 
                        
                          
  -- NUEVOS TIPOS DE MOVIMIENTO 
  LET lr_tpomov.unificado =  1324  -- CARGO ACTIVACION DE MENORES 
  LET lr_tpomov.unificador = 1323  -- ABONO ACTIVACION DE MENORES
    
  -- INDICADOR DEL PROCESO
  LET ls_proceso = 0 
  LET ls_marca_622 = 622
  LET ls_marca_621 = 621
  LET ls_marca_150 = 150 
  
  IF pregunta("PROVISION",ld_folio_lote) THEN 
  
     LET lc_comando = '',
                      '\n  SELECT * FROM afi_act_menor_edad           ',
                      '\n   WHERE folio_lote      = ',ld_folio_lote,' ',
                      '\n     AND edo_liquidacion =  0                '
                      
     PREPARE pro_sel FROM lc_comando
     DECLARE pro_cur CURSOR WITH HOLD FOR pro_sel
     FOREACH pro_cur INTO lr_reg.*
     
       LET ls_realiza_ap = 0 
       
       -- ASEGURARSE DE QUE EXISTAN DATOS PARA HACER LA PROVISION 
       CALL revisa_provision(lr_reg.*)
            RETURNING ls_realiza_ap    
      
       IF ls_realiza_ap = 1 THEN   
     
          -- OBTENER DATOS DE AFI_SOLICITUD 
          SELECT * INTO lr_sol.*
            FROM afi_solicitud
           WHERE n_folio        = lr_reg.n_folio 
             AND tipo_solicitud = lr_reg.tipo_solicitud
          
          -- OBTENER LOS SALDOS 
          IF lr_reg.tipo_solicitud = 33 THEN     
          	  LET lc_comando = 'EXECUTE FUNCTION fn_saldo_dia("',lr_reg.nti_anterior,'",0,0,TODAY)'
          	  LET lc_nss_cargo = lr_reg.nti_anterior
             LET lc_nss_abono = lr_sol.n_seguro
          ELSE
             LET lc_comando = 'EXECUTE FUNCTION fn_saldo_dia("',lr_sol.n_seguro,'",0,0,TODAY)' 	  
          	  LET lc_nss_cargo = lr_sol.n_seguro
             LET lc_nss_abono = lr_sol.n_seguro
          END IF 
              
          --DISPLAY lc_comando CLIPPED  
          PREPARE sal_dia FROM lc_comando
          DECLARE sal_cur CURSOR FOR sal_dia
          FOREACH sal_cur INTO lr_saldo.* 
              
            -- OBTENER EL PRECIO DE LA ACCION 
            SELECT MAX(precio_del_dia)  INTO ld_precio_acc
              FROM glo_valor_accion 
             WHERE codigo_siefore  = lr_saldo.siefore
               AND fecha_valuacion = TODAY
               
            --OBTENER LOS MONTOS NEGATIVOS PARA EL CARGO    
            LET ld_accion_neg = lr_saldo.acciones * (-1) 
            LET ld_pesos_neg  = lr_saldo.pesos    * (-1)
            
            # ---------------------------------------------
            # -- CARGO
            # -- 1324 CARGO ACTIVACION DE MENORES 
            # ---------------------------------------------
            INSERT INTO dis_provision VALUES
                       (lr_tpomov.unificado         , # --tipo_movimiento 1324 
                        lr_saldo.subcuenta          , # --subcuenta
                        lr_saldo.siefore            , # --siefore
                        ld_folio_lote               , # --folio
                        0                           , # --consecutivo_lote
                        lc_nss_cargo                , # --nss
                        lr_sol.n_unico              , # --curp
                        "0"                         , # --folio_sua
                        TODAY                       , # --fecha_pago
                        TODAY                       , # --fecha_valor
                        TODAY                       , # --fecha_conversion
                        ld_pesos_neg                , # --monto_en_pesos
                        ld_accion_neg               , # --monto_en_acciones
                        ld_precio_acc               , # --precio_accion
                        0                           , # --dias_cotizados
                        ""                          , # --sucursal
                        "op_20"                     , # --id_aportante
                        0                           , # --estado
                        TODAY                       , # --fecha_proceso
                        USER                        , # --usuario
                        TODAY                       , # --fecha_archivo
                        0                             # --etiqueta
                        )                    
          
            -- AL INSERTAR EL REGISTRO DE CARGO, SE INSERTA EL ABONO           
            IF SQLCA.SQLERRD[3] > 0 THEN 
            	
            	  LET ls_proceso = 1 
          
               # -------------------------------------------------
               # -- ABONO
               # -- 1323 ABONO ACTIVACION MENORES 
               # -------------------------------------------------
               INSERT INTO dis_provision VALUES
                       (lr_tpomov.unificador        , # --tipo_movimiento 1323
                        lr_saldo.subcuenta          , # --subcuenta
                        lr_saldo.siefore            , # --siefore
                        ld_folio_lote               , # --folio
                        0                           , # --consecutivo_lote
                        lc_nss_abono                , # --nss
                        lr_sol.n_unico              , # --curp
                        "0"                         , # --folio_sua
                        TODAY                       , # --fecha_pago
                        TODAY                       , # --fecha_valor
                        TODAY                       , # --fecha_conversion
                        lr_saldo.pesos              , # --monto_en_pesos
                        lr_saldo.acciones           , # --monto_en_acciones
                        ld_precio_acc               , # --precio_accion
                        0                           , # --dias_cotizados
                        ""                          , # --sucursal
                        "op_20"                     , # --id_aportante
                        0                           , # --estado
                        TODAY                       , # --fecha_proceso
                        USER                        , # --usuario
                        TODAY                       , # --fecha_archivo
                        0                             # --etiqueta
                        )                        
                        
                        
               -- ACTUALIZAR EL ESTADO DE LIQUIDACION DEL REGISTRO          
               UPDATE afi_act_menor_edad
                  SET edo_liquidacion = 1   -- PROVISIONADO            
                WHERE id_act_menor_edad = lr_reg.id_act_menor_edad
                  AND folio_lote        = ld_folio_lote  
                  
            END IF 
            -- SQLCA.SQLERRD[3]
          
          END FOREACH 
          FREE sal_cur
          
       ELSE 

          ------------------------
          -- DESMARCAR LAS CUENTAS
          ------------------------
          
          DECLARE desmarca_cur_b CURSOR WITH HOLD FOR 
            SELECT b.*, a.*
              FROM afi_solicitud      b,
                   afi_act_menor_edad a
             WHERE a.folio_lote     = ld_folio_lote 
               AND a.n_folio        = b.n_folio 
               AND a.tipo_solicitud = b.tipo_solicitud
               
          FOREACH desmarca_cur_b INTO lr_afisol_b.*, lr_act_menor_b.*
          
            LET lc_comando = '',
                             '\n SELECT FIRST 1 correlativo                    ',
                             '\n   FROM cta_act_marca                          ',
                             '\n  WHERE nss       = "',lr_afisol_b.n_seguro,'" ',
                             '\n    AND marca_cod =  ',ls_marca_622,'          '
                             
            PREPARE mar_sel_b FROM lc_comando
            EXECUTE mar_sel_b INTO v_correlativo
                      
            IF (SQLCA.SQLCODE = NOTFOUND) THEN 
            	 -- NO REALIZAR PROCESO 
            ELSE 
            
               LET ls_estado_marca = 0 
               LET ls_marca_causa  = 0 
          
               -- EN PROCESO DE ACTIVACION MENORES NO AFILIADOS
               CALL fn_desmarca_cuenta(lr_afisol_b.n_seguro, ls_marca_622  , v_correlativo,
                                       ls_estado_marca     , ls_marca_causa, vusu         )
                                       
          
               -- PARA TIPO_SOLICITUD 33 - QUITAR LA MARCA 621  
               -- EN PROCESO DE ACTIVACION MENORES -PASA A CTA IMSS                                
               IF lr_afisol_b.tipo_solicitud  = 33 THEN 
               	
                  CALL fn_desmarca_cuenta(lr_act_menor_b.nti_anterior, ls_marca_621  , v_correlativo,
                                          ls_estado_marca            , ls_marca_causa, vusu         )
                                          
               END IF 	
               
               -- ACTUALIZAR EL ESTADO DE LIQUIDACION EN LA TABLA afi_act_menor_edad
               UPDATE afi_act_menor_edad
                  SET edo_liquidacion   = 3  -- NO LIQUIDADO S/F
                WHERE id_act_menor_edad = lr_act_menor_b.id_act_menor_edad   
                  AND folio_lote        = ld_folio_lote
               
               ------------------------------------
               -- INHABILITAR CUENTAS TIPO 26 o 32
               ------------------------------------          
               IF lr_act_menor_b.tipo_solicitud = 33 THEN         	
          
                  LET v_estado_marca   = 0  
                  LET v_codigo_rechazo = 0
                  LET v_marca_causa    = 0             
                  LET v_fecha_causa    = ""
                                          
               	 -- HUBO CAMBIO DE NTI POR NSS                              
                  -- INHABILITAR LA CUENTA CON TIPO_SOLICITUD 26 o 32 CON MARCA 150    
                  CALL fn_marca_cuenta(lr_act_menor_b.nti_anterior, ls_marca_150    , v_correlativo,
                                       v_estado_marca             , v_codigo_rechazo, v_marca_causa,
                                       v_fecha_causa              , vusu            )
                                       
               END IF -- lr_act_menor_B.tipo_solicitud 
               
            END IF -- SQLCA.SQLCODE = NOTFOUND
             
          END FOREACH 
          FREE desmarca_cur_b                      
       
       END IF -- ls_realiza_ap  
       -- REVISAR SI SE PUEDE HACER LA PROVISION   
       
     END FOREACH   
     FREE pro_cur        
  
  END IF 
  -- pregunta
  
  IF ls_proceso = 1 THEN 
     PROMPT " PROVISION TERMINADA...<ENTER> PARA SALIR " FOR lc_pausa
     CALL rep_pro_liq("PROVISION",ld_folio_lote)
  ELSE 
     PROMPT " NO SE REALIZO LA PROVISION...<ENTER> PARA SALIR " FOR lc_pausa
  END IF    

END FUNCTION 

-------------------------------------------------------------------------------

FUNCTION liquidacion()

  DEFINE hay_provision     SMALLINT 
  DEFINE hay_liquidacion   SMALLINT
  DEFINE ls_proceso        SMALLINT  
  DEFINE lr_pro            RECORD LIKE dis_provision.*
  DEFINE lr_afisol         RECORD LIKE afi_solicitud.*
  DEFINE lr_act_menor      RECORD LIKE afi_act_menor_edad.*
  DEFINE lr_afi_mae_afi    RECORD LIKE afi_mae_afiliado.*
  DEFINE ls_marca_622      SMALLINT 
  DEFINE ls_marca_621      SMALLINT 
  DEFINE ls_marca_150      SMALLINT   -- MARCA DE INHABILITACION 
  
  DEFINE ls_estado_marca   SMALLINT
  DEFINE ls_marca_causa    SMALLINT
  
  DEFINE v_estado_marca     SMALLINT 
  DEFINE v_codigo_rechazo   SMALLINT 
  DEFINE v_marca_causa      SMALLINT
  DEFINE v_fecha_causa      DATE 
  DEFINE v_correlativo      INTEGER 

  
  -- INDICADOR DEL PROCESO 
  LET ls_proceso   = 0
  LET ls_marca_622 = 622
  LET ls_marca_621 = 621
  LET ls_marca_150 = 150 
  
  -- REVISAR SI HAY DATOS DE PROVISION
  LET hay_provision = 0 
  SELECT COUNT(*) INTO hay_provision 
    FROM dis_provision
   WHERE folio = ld_folio_lote
     
  IF hay_provision = 0 THEN 
     PROMPT "NO EXISTE INFORMACION DE PROVISION PARA EL FOLIO...<ENTER> PARA CONTINUAR " FOR lc_pausa  	
     RETURN
  END IF 	
  
  -- REVISAR SI NO SE HA HECHO LA LIQUIDACION PARA ESTE FOLIO 
  LET hay_liquidacion = 0 
  SELECT COUNT(*) INTO hay_liquidacion
    FROM dis_cuenta 
   WHERE folio = ld_folio_lote    
  
  IF hay_liquidacion > 0 THEN 
     PROMPT "ESTE FOLIO YA HA SIDO LIQUIDADO ANTERIORMENTE...<ENTER> PARA CONTINUAR " FOR lc_pausa
     RETURN
  END IF 	
  
  IF pregunta("LIQUIDACION",ld_folio_lote) THEN 
  	
  	 DECLARE liq_cur CURSOR FOR 
      SELECT * 
  	    FROM dis_provision
  	   WHERE folio = ld_folio_lote
  	     AND monto_en_acciones <> 0 
  	     
     FOREACH liq_cur INTO lr_pro.*
      
  	   INSERT INTO dis_cuenta VALUES (lr_pro.*) 
  	   
     END FOREACH 	
     FREE liq_cur 
  	
     ------------------------
     -- DESMARCAR LAS CUENTAS
     ------------------------
     
     DECLARE desmarca_cur CURSOR WITH HOLD FOR 
       SELECT b.*, a.*
         FROM afi_solicitud      b,
              afi_act_menor_edad a
        WHERE a.folio_lote     = ld_folio_lote 
          AND a.n_folio        = b.n_folio 
          AND a.tipo_solicitud = b.tipo_solicitud
          
     FOREACH desmarca_cur INTO lr_afisol.*, lr_act_menor.*
     
       LET lc_comando = '',
                        '\n SELECT FIRST 1 correlativo                  ',
                        '\n   FROM cta_act_marca                        ',
                        '\n  WHERE nss       = "',lr_afisol.n_seguro,'" ',
                        '\n    AND marca_cod =  ',ls_marca_622,'        '
                        
       PREPARE mar_sel2 FROM lc_comando
       EXECUTE mar_sel2 INTO v_correlativo
                 
       IF (SQLCA.SQLCODE = NOTFOUND) THEN 
       	 -- NO REALIZAR PROCESO 
       ELSE 
       
          LET ls_estado_marca = 0 
          LET ls_marca_causa  = 0 

          -- EN PROCESO DE ACTIVACION MENORES NO AFILIADOS
          CALL fn_desmarca_cuenta(lr_afisol.n_seguro, ls_marca_622  , v_correlativo,
                                  ls_estado_marca   , ls_marca_causa, vusu        )
                                  

          -- PARA TIPO_SOLICITUD 33 - QUITAR LA MARCA 621  
          -- EN PROCESO DE ACTIVACION MENORES -PASA A CTA IMSS                                
          IF lr_afisol.tipo_solicitud  = 33 THEN 
          	
             CALL fn_desmarca_cuenta(lr_act_menor.nti_anterior, ls_marca_621  , v_correlativo,
                                     ls_estado_marca          , ls_marca_causa, vusu        )
                                     
          END IF 	
          
          -- ACTUALIZAR EL ESTADO DE LIQUIDACION EN LA TABLA afi_act_menor_edad
          UPDATE afi_act_menor_edad
             SET edo_liquidacion   = 2  -- LIQUIDADO
           WHERE id_act_menor_edad = lr_act_menor.id_act_menor_edad   
             AND folio_lote        = ld_folio_lote
          
          ------------------------------------
          -- INHABILITAR CUENTAS TIPO 26 o 32
          ------------------------------------          
          IF lr_act_menor.tipo_solicitud = 33 THEN         	
  
             LET v_estado_marca   = 0  
             LET v_codigo_rechazo = 0
             LET v_marca_causa    = 0             
             LET v_fecha_causa    = ""
                                     
          	 -- HUBO CAMBIO DE NTI POR NSS                              
             -- INHABILITAR LA CUENTA CON TIPO_SOLICITUD 26 o 32 CON MARCA 150    
             CALL fn_marca_cuenta(lr_act_menor.nti_anterior, ls_marca_150    , v_correlativo,
                                  v_estado_marca           , v_codigo_rechazo, v_marca_causa,
                                  v_fecha_causa            , vusu            )
                                  
          END IF -- lr_act_menor.tipo_solicitud 
          
       END IF -- SQLCA.SQLCODE = NOTFOUND
        
     END FOREACH 
     FREE desmarca_cur  
     
     PROMPT " LIQUIDACION TERMINADA...<ENTER> PARA SALIR " FOR lc_pausa
     
     CALL rep_pro_liq("LIQUIDACION",ld_folio_lote)
  
  END IF 
  -- pregunta 

END FUNCTION 

-------------------------------------------------------------------------------

FUNCTION pide_folio(ps_edo_liquidacion)

  DEFINE ps_edo_liquidacion  SMALLINT 
  DEFINE ls_texto            VARCHAR(15)

  DEFINE pi_folio_lote       INTEGER
  DEFINE ls_existe_folio     SMALLINT 
  DEFINE ls_status           SMALLINT 
  DEFINE ls_mensaje          VARCHAR(250)
  
  -- ps_edo_liquidacion
  -- 0 = PENDIENTE
  -- 1 = PROVISIONADO
  -- 2 = LIQUIDADO 
  
  CASE 
    WHEN ps_edo_liquidacion = 0
  	     LET ls_texto = "PROVISION"
    WHEN ps_edo_liquidacion = 1
  	     LET ls_texto = "LIQUIDACION"
  	OTHERWISE 
  	     LET ls_texto = ""
  END CASE 
  	        
  -- MOSTRAR EL FOLIO MENOR CON REGISTROS POR LIQUIDAR  
  SELECT NVL(MAX(a.folio_lote),0) INTO pi_folio_lote
    FROM afi_act_menor_edad a,
         afi_mae_afiliado   b
   WHERE a.n_folio         = b.n_folio  
     AND a.tipo_solicitud  = b.tipo_solicitud 
     AND a.nti_anterior    = b.n_seguro
     AND b.status_interno  = 200
     AND a.edo_liquidacion = ps_edo_liquidacion     
      
  IF pi_folio_lote = 0 THEN 
  	
     -- O INDICAR QUE NO HAY FOLIOS POR LIQUIDAR 
     LET ls_mensaje = "NO HAY REGISTROS PARA REALIZAR UNA ",ls_texto
  	 ERROR ls_mensaje
  	 
  ELSE  

     OPEN WINDOW v3 AT 6,2 WITH FORM "AFIP0041" ATTRIBUTE(BORDER, MESSAGE LINE LAST -1)
       
       --CAPTURA EL NOMBRE DE ARCHIVO A PROCESAR
       INPUT BY NAME pi_folio_lote WITHOUT DEFAULTS
       	
          AFTER FIELD pi_folio_lote
          
             LET ls_existe_folio = 0 
             
             IF pi_folio_lote IS NULL OR pi_folio_lote = 0 OR pi_folio_lote = "" THEN
                ERROR " FOLIO LOTE NO PUEDE SER NULO O CERO "    
                INITIALIZE pi_folio_lote TO NULL
                DISPLAY BY NAME pi_folio_lote
                NEXT FIELD pi_folio_lote
             END  IF
       
             LET lc_comando = '',
                              '\n SELECT COUNT(*)                                 ',
                              '\n   FROM safre_af:afi_act_menor_edad              ',
                              '\n  WHERE folio_lote = "',pi_folio_lote CLIPPED,'" '
             -- DISPLAY lc_comando CLIPPED                           
             PREPARE gen01 FROM lc_comando
             EXECUTE gen01 INTO ls_existe_folio                 
       
             IF ls_existe_folio = 0 THEN
                ERROR " EL FOLIO LOTE... NO EXISTE "
                INITIALIZE pi_folio_lote TO NULL
                DISPLAY BY NAME pi_folio_lote
                NEXT FIELD pi_folio_lote
             ELSE           
                LET lc_comando = '',
                                 '\n SELECT COUNT(*)                                      ',
                                 '\n   FROM safre_af:afi_act_menor_edad                   ',
                                 '\n  WHERE folio_lote      = "',pi_folio_lote CLIPPED,'" ',
                                 '\n    AND edo_liquidacion =  ',ps_edo_liquidacion,'     ' 
                -- DISPLAY lc_comando CLIPPED                           
                PREPARE gen01b FROM lc_comando
                EXECUTE gen01b INTO ls_existe_folio 
                IF ls_existe_folio = 0 THEN 
                	
                	 LET ls_mensaje = " EXISTE FOLIO, PERO SIN REGISTROS A ",ls_texto
                   ERROR ls_mensaje
                   INITIALIZE pi_folio_lote TO NULL
                   DISPLAY BY NAME pi_folio_lote
                   NEXT FIELD pi_folio_lote
                   
                ELSE 
                
                   -- ESTO PARA VALIDAR QUE YA SE HAYA EJECUTADO EL AFIB003 
                   SELECT COUNT(*) INTO ls_existe_folio
                     FROM afi_act_menor_edad a,
                          afi_mae_afiliado   b
                    WHERE a.folio_lote      = pi_folio_lote
                      AND a.n_folio         = b.n_folio  
                      AND a.tipo_solicitud  = b.tipo_solicitud 
                      AND a.nti_anterior    = b.n_seguro
                      AND b.status_interno  = 200
                      AND a.edo_liquidacion = ps_edo_liquidacion     
                      
                   IF ls_existe_folio = 0 THEN                 	
                   	  LET ls_mensaje = " EXISTE FOLIO, PERO NO SE HA REALIZADO ",ls_texto
                      ERROR ls_mensaje
                      INITIALIZE pi_folio_lote TO NULL
                      DISPLAY BY NAME pi_folio_lote
                      NEXT FIELD pi_folio_lote                      
                   END IF 
                   
                END IF
                
             END IF
       
          ON KEY (ESC)    
          
             IF pi_folio_lote IS NULL OR pi_folio_lote = 0 OR pi_folio_lote = "" THEN
                ERROR " EL FOLIO LOTE NO PUEDE SER NULO O CERO"
                INITIALIZE pi_folio_lote TO NULL
                DISPLAY BY NAME pi_folio_lote
                NEXT FIELD pi_folio_lote
             END IF
                
             LET lc_comando = '',
                              '\n SELECT COUNT(*)                                 ',
                              '\n   FROM safre_af:afi_act_menor_edad              ',
                              '\n  WHERE folio_lote = "',pi_folio_lote CLIPPED,'" '
             -- DISPLAY lc_comando CLIPPED                           
             PREPARE gen02 FROM lc_comando
             EXECUTE gen02 INTO ls_existe_folio                 
       
             IF ls_existe_folio = 0 THEN
                ERROR " EL FOLIO LOTE... NO EXISTE "
                INITIALIZE pi_folio_lote TO NULL
                DISPLAY BY NAME pi_folio_lote
                NEXT FIELD pi_folio_lote
             ELSE           
                LET lc_comando = '',
                                 '\n SELECT COUNT(*)                                      ',
                                 '\n   FROM safre_af:afi_act_menor_edad                   ',
                                 '\n  WHERE folio_lote      = "',pi_folio_lote CLIPPED,'" ',
                                 '\n    AND edo_liquidacion = ',ps_edo_liquidacion,'      ' 
                -- DISPLAY lc_comando CLIPPED                           
                PREPARE gen02b FROM lc_comando
                EXECUTE gen02b INTO ls_existe_folio 
                IF ls_existe_folio = 0 THEN 
                	 LET ls_mensaje = " EXISTE FOLIO, PERO SIN REGISTROS A ",ls_texto
                   ERROR ls_mensaje
                   INITIALIZE pi_folio_lote TO NULL
                   DISPLAY BY NAME pi_folio_lote
                   NEXT FIELD pi_folio_lote
                ELSE                 
                   -- ESTO PARA VALIDAR QUE YA SE HAYA EJECUTADO EL AFIB003 
                   SELECT COUNT(*) INTO ls_existe_folio
                     FROM afi_act_menor_edad a,
                          afi_mae_afiliado   b
                    WHERE a.folio_lote      = pi_folio_lote
                      AND a.n_folio         = b.n_folio  
                      AND a.tipo_solicitud  = b.tipo_solicitud 
                      AND a.nti_anterior    = b.n_seguro
                      AND b.status_interno  = 200
                      AND a.edo_liquidacion = ps_edo_liquidacion     
                      
                   IF ls_existe_folio = 0 THEN     
                   	  LET ls_mensaje = " EXISTE FOLIO, PERO NO SE HA REALIZADO ",ls_texto
                      ERROR ls_mensaje
                      INITIALIZE pi_folio_lote TO NULL
                      DISPLAY BY NAME pi_folio_lote
                      NEXT FIELD pi_folio_lote                      
                   END IF                                      
                END IF 	 
             END IF
             
             EXIT INPUT 
       
          ON KEY (INTERRUPT, CONTROL-C)
             PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR lc_pausa
             LET pi_folio_lote = 0 
             EXIT INPUT
               
       END INPUT
       
       IF INT_FLAG = TRUE THEN 
          LET INT_FLAG = FALSE
       END IF 
       
     CLOSE WINDOW v3 
           
  END IF    
  
  RETURN pi_folio_lote

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION rep_pro_liq(p_reporte, pi_folio_lote) 

  DEFINE p_reporte        VARCHAR(15)
  DEFINE pi_folio_lote    DECIMAL(10,0)
                          
  DEFINE li_folio_lote    INTEGER  
  DEFINE lv_folio_lote    VARCHAR(10)
                          
  DEFINE lr_reporte       VARCHAR(100)
  DEFINE lr_nombre_rep    VARCHAR(050)
                          
  DEFINE lv_afore_local   CHAR(03)
  DEFINE ls_registros     SMALLINT
  DEFINE lr_reg           RECORD   
                            renglon         SMALLINT   ,
                            registro        VARCHAR(20),
                            nss             VARCHAR(20),
                            curp            VARCHAR(20),
                            movimiento      VARCHAR(20),
                            acciones        VARCHAR(20),
                            pesos           VARCHAR(20)
                          END RECORD  
                        
  -- CREAR TABLA TEMPORAL 
  WHENEVER ERROR CONTINUE
    DROP TABLE tmp_reporte_proliq
  WHENEVER ERROR STOP

  CREATE TABLE tmp_reporte_proliq
  ( 
   renglon      SMALLINT   ,
   registro     VARCHAR(20),
   nss          VARCHAR(20), 
   curp         VARCHAR(20),
   movimiento   VARCHAR(20),
   acciones     VARCHAR(20),
   pesos        VARCHAR(20)
  )

  -- AFORE LOCAL                         
  SELECT codigo_afore INTO lv_afore_local 
    FROM tab_afore_local

  -- NOMBRE DEL ARCHIVO RESPUESTA 
  LET li_folio_lote = pi_folio_lote 
  LET lv_folio_lote = li_folio_lote 
  LET lr_nombre_rep = "REPORTE_",p_reporte CLIPPED,"_FOLIO_",lv_folio_lote CLIPPED,".txt"
  LET lr_reporte    = v_ruta_listados CLIPPED,"/",lr_nombre_rep CLIPPED
    
  -- ENCABEZADO
  INSERT INTO tmp_reporte_proliq VALUES                                     
  (0, "REGISTRO", "NSS", "CURP", "MOVIMIENTO", "ACCIONES", "PESOS")
  
  CASE 
     WHEN p_reporte = "PROVISION"
     
          LET lc_comando = '',
                           '\n SELECT nss, curp, tipo_movimiento,                 ',
                           '\n        SUM(monto_en_acciones), SUM(monto_en_pesos) ',
                           '\n   FROM dis_provision                               ',
                           '\n  WHERE folio = ',pi_folio_lote,'                   ',
                           '\n  GROUP BY 1,2,3                                    '
                           
     WHEN p_reporte = "LIQUIDACION"
     
          LET lc_comando = '',
                           '\n SELECT nss, curp, tipo_movimiento,                 ',
                           '\n        SUM(monto_en_acciones), SUM(monto_en_pesos) ',
                           '\n   FROM dis_cuenta                                  ',
                           '\n  WHERE folio = ',pi_folio_lote,'                   ',
                           '\n  GROUP BY 1,2,3                                    '          
  END CASE    

  -- DETALLE    
  LET ls_registros = 0  
  
  PREPARE reporte_proliq FROM lc_comando     
  DECLARE reporte_cur CURSOR FOR reporte_proliq
  
  FOREACH reporte_cur INTO lr_reg.nss     , lr_reg.curp , lr_reg.movimiento,
                           lr_reg.acciones, lr_reg.pesos

    LET ls_registros    = ls_registros + 1 
    LET lr_reg.renglon  = 1                                 
    LET lr_reg.registro = ls_registros 
    
    INSERT INTO tmp_reporte_proliq VALUES (lr_reg.*)                             

  END FOREACH
  FREE reporte_cur
  
  -- GENERAR EL ARCHIVO MEDIANTE UN UNLOAD 
  UNLOAD TO lr_reporte
  SELECT TRIM(registro)  , TRIM(nss)     , TRIM(curp) ,
         TRIM(movimiento), TRIM(acciones), TRIM(pesos)         
    FROM tmp_reporte_proliq 
   ORDER BY renglon 
  
  -- AVISO AL USUARIO   
  OPEN WINDOW wauxi01 AT v_ren,v_col WITH 7 rows, 68 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST )
     DISPLAY  "SE GENERO REPORTE DE ",p_reporte CLIPPED," FOLIO ",lv_folio_lote CLIPPED AT 2,2
     DISPLAY  "RUTA      : ",v_ruta_listados    CLIPPED                                 AT 3,2
     DISPLAY  "NOMBRE    : ",lr_nombre_rep      CLIPPED                                 AT 4,2
     DISPLAY  "REGISTROS : ",ls_registros       CLIPPED                                 AT 5,2
     PROMPT "PRESIONE <ENTER> PARA CONTINUAR " FOR CHAR lc_pausa        
  CLOSE WINDOW wauxi01

  -- BORRAR TABLA TEMPORAL 
  WHENEVER ERROR CONTINUE
    DROP TABLE tmp_reporte_proliq
  WHENEVER ERROR STOP

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION fn_get_usuario()

  -- OBTENER EL LOGIN DEL USUARIO QUE ESTA EJECUTANDO EL PROCESO ACTUAL 

  DEFINE v_usuario   VARCHAR(10) 
  
  SELECT MAX(USER) 
    INTO v_usuario
    FROM tab_afore_local
	
  RETURN v_usuario 

END FUNCTION 

-------------------------------------------------------------------------------

FUNCTION fn_desmarca_cuenta(p_n_seguro    , p_marca      , p_correlativo,
                            p_estado_marca, p_marca_causa, p_usu        )

  DEFINE p_n_seguro        CHAR(11)  -- pnss          
  DEFINE p_marca           SMALLINT  -- pmarca_entra  
  DEFINE p_correlativo     INTEGER   -- pcorrelativo  
  DEFINE p_estado_marca    SMALLINT  -- pestado_marca 
  DEFINE p_marca_causa     SMALLINT  -- pmarca_causa  
  DEFINE p_usu             CHAR(08)  -- pusuario    
  
  LET lc_comando = '',
                   '\n EXECUTE PROCEDURE desmarca_cuenta("',p_n_seguro,'",    ',
                   '\n                                    ',p_marca,',        ',
                   '\n                                    ',p_correlativo,',  ',
                   '\n                                    ',p_estado_marca,', ',
                   '\n                                    ',p_marca_causa,',  ',
                   '\n                                   "',vusu,'"          )'
                   
   --DISPLAY lc_comando CLIPPED  
   --DISPLAY "DESMARCA CUENTA ",p_n_seguro              
   --DISPLAY "MARCA ",p_marca
   
   PREPARE eje_des FROM lc_comando
   EXECUTE eje_des
   
END FUNCTION 

-------------------------------------------------------------------------------

FUNCTION fn_marca_cuenta(p_n_seguro    , p_marca         , p_correlativo,
                         p_estado_marca, p_codigo_rechazo, p_marca_causa,
                         p_fecha_causa , p_usu           )

  DEFINE p_n_seguro        CHAR(11)  -- pnss          
  DEFINE p_marca           SMALLINT  -- pmarca_entra  
  DEFINE p_correlativo     INTEGER   -- pcorrelativo  
  DEFINE p_estado_marca    SMALLINT  -- pestado_marca 
  DEFINE p_codigo_rechazo  SMALLINT  -- pcodigo_rechazo
  DEFINE p_marca_causa     SMALLINT  -- pmarca_causa  
  DEFINE p_fecha_causa     DATE      -- pfecha_causa  
  DEFINE p_usu             CHAR(08)  -- pusuario    
  
  DEFINE v_marca_causa     SMALLINT 
  DEFINE v_codigo_rechazo  SMALLINT 
  
  LET lc_comando = '',
                   '\n EXECUTE PROCEDURE marca_cuenta ( "',p_n_seguro,'",      ',
                   '\n                                   ',p_marca,',          ',
                   '\n                                   ',p_correlativo,',    ',
                   '\n                                   ',p_estado_marca,',   ',
                   '\n                                   ',p_codigo_rechazo,', ',
                   '\n                                   ',p_marca_causa,',    ',
                   '\n                                  "',p_fecha_causa,'",   ',
                   '\n                                  "',p_usu,'")           '                   
   
   -- DISPLAY lc_comando CLIPPED  
   -- DISPLAY "MARCAJE DE CUENTA ",p_n_seguro              
   -- DISPLAY "MARCA ",p_marca
   
   PREPARE eje_mar FROM lc_comando
   DECLARE cur_mar CURSOR FOR eje_mar
      OPEN cur_mar
     FETCH cur_mar INTO v_marca_causa, v_codigo_rechazo
     CLOSE cur_mar 
     
   -- DISPLAY "RESPUESTA ",v_marca_causa," - ",v_codigo_rechazo  

END FUNCTION

-------------------------------------------------------------------------------


FUNCTION pregunta(texto,pfolio)

  DEFINE texto     VARCHAR(30)
  DEFINE pfolio    VARCHAR(10)
  DEFINE sigue     SMALLINT
  DEFINE confirma  CHAR(1)
  
  
  LET li_folio_lote = pfolio
  LET lv_folio_lote = li_folio_lote
  
  LET texto = UPSHIFT(texto)

  LET   sigue = 1
  WHILE sigue = 1

    # Esperar un SI o NO como respuesta 

    PROMPT "DESEA REALIZAR LA ",texto CLIPPED," DEL FOLIO ",lv_folio_lote CLIPPED," [S/N]: " FOR confirma

    LET confirma = UPSHIFT(confirma)

    IF ( confirma MATCHES "[Ss]" )  THEN
        LET sigue = 0 
        RETURN TRUE
    ELSE
       IF confirma MATCHES "[Nn]" THEN
           LET sigue = 0 
           RETURN FALSE 
        ELSE 
           ERROR "Confirme con 'S', Cancele con 'N'"
           LET sigue = 1
        END IF
    END IF

  END WHILE

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION fn_encabezado(subtitulo,prog,linea)

  -- PONER EL fn_encabezado ESTANDAR EN LAS VENTANAS QUE SE 
  -- UTILICEN DENTRO DE LOS MODULOS DE SAFRE

  DEFINE subtitulo  CHAR(45)
  DEFINE prog       CHAR(15)
  DEFINE linea      SMALLINT  
  DEFINE linea2     SMALLINT 

  DEFINE hoy_hoy    DATE
  DEFINE i          SMALLINT 

  LET hoy_hoy = TODAY
  LET linea2  = linea + 2 
	   
  -- CENTRAR EL TITULO
  LET i = LENGTH(subtitulo CLIPPED)
  LET i = ((80 - i) / 2) + 1

  DISPLAY "                                                                              "
       AT linea,1 ATTRIBUTE(REVERSE)
  DISPLAY hoy_hoy   USING "DD-MM-YYYY " AT linea,69 ATTRIBUTE(REVERSE)
  DISPLAY prog      CLIPPED             AT linea,01 ATTRIBUTE(REVERSE) 
  DISPLAY subtitulo CLIPPED             AT linea,i  ATTRIBUTE(REVERSE) 

  DISPLAY " <ESC> Procesar "            AT linea2,1  
  DISPLAY " <Ctrl-C> Salir "            AT linea2,63 

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION revisa_provision(pr_reg)

  DEFINE pr_reg            RECORD LIKE afi_act_menor_edad.* 
   
  DEFINE ls_proceso_pro    SMALLINT 
  DEFINE lr_sol            RECORD LIKE afi_solicitud.* 
  DEFINE lr_saldo          RECORD 
                             subcuenta   SMALLINT,
                             siefore     SMALLINT,
                             acciones    DECIMAL(16,6),
                             pesos       DECIMAL(16,6)
                           END RECORD
  DEFINE ld_accion_neg     DECIMAL(16,6)
  DEFINE ld_pesos_neg      DECIMAL(16,6)
  DEFINE ld_precio_acc     DECIMAL(19,14) 
  DEFINE lc_nss_cargo      CHAR(11)
  DEFINE lc_nss_abono      CHAR(11)   
  DEFINE ls_bandera        SMALLINT   
                        
  -- INDICADOR DEL PROCESO
  LET ls_proceso_pro = 1
  LET ls_bandera     = 0 

  -- REVISANDO SI HAY SALDO    
  -- OBTENER DATOS DE AFI_SOLICITUD 
  
  SELECT * INTO lr_sol.*
    FROM afi_solicitud
   WHERE n_folio        = pr_reg.n_folio 
     AND tipo_solicitud = pr_reg.tipo_solicitud
  
  -- OBTENER LOS SALDOS 
  IF pr_reg.tipo_solicitud = 33 THEN     
  	 LET lc_comando = 'EXECUTE FUNCTION fn_saldo_dia("',pr_reg.nti_anterior,'",0,0,TODAY)'
  	 LET lc_nss_cargo = pr_reg.nti_anterior
     LET lc_nss_abono = lr_sol.n_seguro
  ELSE
     LET lc_comando = 'EXECUTE FUNCTION fn_saldo_dia("',lr_sol.n_seguro,'",0,0,TODAY)' 	  
  	 LET lc_nss_cargo = lr_sol.n_seguro
     LET lc_nss_abono = lr_sol.n_seguro
  END IF 
      
  --DISPLAY lc_comando CLIPPED  
  PREPARE sal_dia_pro FROM lc_comando
  DECLARE sal_cur_pro CURSOR FOR sal_dia_pro
  FOREACH sal_cur_pro INTO lr_saldo.* 
  
    LET ls_bandera = ls_bandera + 1 

    --DISPLAY "SUBCUENTA (",lr_saldo.subcuenta,")"
    --DISPLAY "SIEFORE   (",lr_saldo.siefore,")"
    --DISPLAY "ACCIONES  (",lr_saldo.acciones,")"
    --DISPLAY "PESOS     (",lr_saldo.pesos,")"
                        
    -- OBTENER EL PRECIO DE LA ACCION 
    SELECT MAX(precio_del_dia)  INTO ld_precio_acc
      FROM glo_valor_accion 
     WHERE codigo_siefore  = lr_saldo.siefore
       AND fecha_valuacion = TODAY
    --DISPLAY "PRECIO     (",ld_precio_acc,")"
       
    --OBTENER LOS MONTOS NEGATIVOS PARA EL CARGO    
    LET ld_accion_neg = lr_saldo.acciones * (-1) 
    LET ld_pesos_neg  = lr_saldo.pesos    * (-1)

    IF lr_saldo.subcuenta = 0     AND 
       lr_saldo.siefore   = 0     AND 
       ld_precio_acc      = 0     AND 
       ld_accion_neg      IS NULL AND
       ld_pesos_neg       IS NULL THEN 
   
       LET ls_proceso_pro = 0
       EXIT FOREACH 
       
    END IF 
  
  END FOREACH 
  FREE sal_cur_pro
  
  IF ls_proceso_pro = 1 AND ls_bandera > 0 THEN 
     DISPLAY "ES POSIBLE REALIZAR PROVISION PARA ",pr_reg.curp," " AT 10,2 
  ELSE 
     DISPLAY "NO HAY SALDOS PARA PROVISIONAR ",pr_reg.curp," " AT 10,2
     LET ls_proceso_pro = 0  
  END IF    

  RETURN ls_proceso_pro

END FUNCTION      

-------------------------------------------------------------------------------
