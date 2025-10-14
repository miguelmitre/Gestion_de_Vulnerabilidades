################################################################################
#Owner             => E.F.P.                                                   #
#Programa PENC205  => PRELIQUIDACION DE RETIROS PROGRAMADOS -PENSION MINIMA    #
#                     GARANTIZADA (RETIRO S)                                   #
#Sistema           => PEN                                                      #
#Fecha creacion    => 6 DE ABRIL DE 2010                                       #
#By                => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiz.   => 15 DE FEBRERO DE 2011                                    #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
#                  => Se modifica para ajustarse a los cambios realizados en el#
#                     proceso de acuerdo a lo realizado para la generacion de  #
#                     la operación 78 y carga de la 79                         #
#Actualizacion     => v1.2 ISAI JIMENEZ ROJAS                                  #
#                  => Liquidar el SALDO en caso de ser el ultimo pago          #
#Actualizacion     => v1.3 ISAI JIMENEZ ROJAS                                  #
#                  => Descarta pago de vienda en caso de diagnostico 440 y 401 #
#Observaciones     => La Liquidacion es automatica                             #
################################################################################

DATABASE safre_af

GLOBALS
    DEFINE gar_precio_acc     ARRAY [90] OF RECORD #Arreglo para los precios_accion
           estado             SMALLINT     ,
           fecha              DATE         ,
           siefore            SMALLINT     ,
           precio_dia         DECIMAL(16,6)
           END RECORD

    DEFINE gr_edo             RECORD
           capturado          LIKE pen_estado_pmg.estado_solicitud    ,
           recibido           LIKE pen_estado_pmg.estado_solicitud    ,
           en_pago            LIKE pen_estado_pmg.estado_solicitud    ,
           preliquidado       LIKE pen_estado_pmg.estado_solicitud    ,
           liquidado          LIKE pen_estado_pmg.estado_solicitud    ,
           rechazado          LIKE pen_estado_pmg.estado_solicitud
           END RECORD

    DEFINE gr_noti            RECORD
           agotamiento        LIKE tab_tipo_notifica_pmg.clave        ,
           conclusion         LIKE tab_tipo_notifica_pmg.clave        
           END RECORD
    
    DEFINE enter              CHAR(001)
    DEFINE gc_usuario         CHAR(020)

    DEFINE gs_procesa         SMALLINT
    DEFINE gs_cod_afore       SMALLINT
    DEFINE gc_mensaje         CHAR(80)


    DEFINE HOY                DATE

END GLOBALS

#==============================================================================#
#                                                                              #
#==============================================================================#
MAIN

    DEFINE li_folio_pre       INTEGER   

    DEFER INTERRUPT
    
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST -1  ,
        ACCEPT KEY CONTROL-I ,
        MESSAGE LINE LAST 
        
      

    CALL init()
    
    CALL STARTLOG(gc_usuario CLIPPED||".PENC205.log")


    CALL f_busca_regs_pago() RETURNING gs_procesa, li_folio_pre

    IF gs_procesa THEN
        CALL f_preliquida_registros(li_folio_pre)
        CALL f_actualiza_tablas(li_folio_pre)
    ELSE
        CALL f_error_msg("PROCESO CANCELADO")
    END IF

    CLOSE WINDOW penc2051

END MAIN


#==============================================================================#
# init : Inicializa las variables globales que se usaran en el programa     #
#==============================================================================#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    LET HOY         = TODAY

    ----- CODIGOS AFORES -----
    SELECT codigo_afore   ,
           USER
    INTO   gs_cod_afore ,
           gc_usuario
    FROM   tab_afore_local


    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "RECIBIDO"

    SELECT A.estado_solicitud
    INTO   gr_edo.en_pago
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "EN PROCESO DE PAGO"

    SELECT A.estado_solicitud
    INTO   gr_edo.preliquidado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "PRELIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.rechazado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "RECHAZADO"

    ----- TIPO DE NOTIFICACION -----
    SELECT clave
    INTO   gr_noti.agotamiento
    FROM   tab_tipo_notifica_pmg
    WHERE  descripcion MATCHES '*AGOTAMIENTO*'
    
    SELECT clave
    INTO   gr_noti.conclusion
    FROM   tab_tipo_notifica_pmg
    WHERE  descripcion MATCHES '*CONCLUSION*'

    ----- CALCULA PROPORCIONAL PMG -----
    LET lc_prepare = " EXECUTE FUNCTION fn_calcula_prop_pmg(?,?,?,?) "
    PREPARE eje_prop_pmg FROM lc_prepare

    LET lc_prepare = " "

    ----- SALDO AL DIA  -----
    LET lc_prepare = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "
    PREPARE eje_saldo_dia FROM lc_prepare

    LET lc_prepare = " "

    ----- DESMARCA CUENTA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION


#==============================================================================#
# f_busca_regs_pago : Realiza la busqueda de los registros correspondientes #
#                     al periodo indicado por el usuario                    #
#==============================================================================#
FUNCTION f_busca_regs_pago()

    DEFINE li_folio     INTEGER 
    DEFINE ls_salida    SMALLINT

    CALL f_captura_folio() RETURNING li_folio, ls_salida

    IF ls_salida = 1 THEN
       CALL f_despliega_datos(li_folio, HOY) RETURNING ls_salida
    END IF

    RETURN ls_salida, li_folio

END FUNCTION

#==============================================================================#
# f_captura_folio : Captura el folio con el que realizara la busqueda el    #
#                   programa                                                #
#==============================================================================#
FUNCTION f_captura_folio()

    DEFINE li_folio    INTEGER 
    DEFINE ls_salida   SMALLINT 
    DEFINE ls_estado   SMALLINT 
    DEFINE lc_mensaje  CHAR(100)

    OPEN WINDOW penc2051 AT 2,2 WITH FORM "PENC2051" ATTRIBUTE(BORDER)
    DISPLAY " <ESC> - Ejecutar                                            <CTRL-C> - Salir " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENC205        PRELIQUIDACION MENSUAL DE REGISTROS DE PMG                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    LET ls_salida = 1
    LET li_folio  = 0 
    
    CALL f_recomienda_folio()
         RETURNING li_folio
         
    DISPLAY BY NAME li_folio          

    INPUT BY NAME li_folio WITHOUT DEFAULTS

        AFTER FIELD li_folio
        	  IF li_folio IS NULL  OR 
        	  	 li_folio <= 0     THEN 
        	  	 ERROR "No es un numero de folio valido" ATTRIBUTE(REVERSE)
               SLEEP 2
               ERROR ""
               NEXT FIELD li_folio
            END IF    

        ON KEY (CONTROL-C,INTERRUPT)
            LET ls_salida = 0
            INITIALIZE li_folio TO NULL
            EXIT INPUT

        ON KEY (ESC)
        	  IF li_folio IS NULL  OR 
        	  	 li_folio <= 0     THEN 
        	  	 ERROR "No es un numero de folio valido" ATTRIBUTE(REVERSE)
               SLEEP 2
               ERROR ""
               NEXT FIELD li_folio
            ELSE
            	 CALL f_valida_ejecucion(li_folio) RETURNING ls_salida
               EXIT INPUT    
            END IF    
            
    END INPUT

    RETURN li_folio, ls_salida

END FUNCTION

#==============================================================================#
# f_despliega_datos : Realiza la consulta general y muestra los registros   #
#                     que cumplen con los parametros indicados              #
#==============================================================================#
FUNCTION f_despliega_datos(pi_folio, pdt_fecha_proceso)

    DEFINE pi_folio           INTEGER 
    DEFINE pdt_fecha_proceso  DATE
    
    DEFINE ls_ren_act         SMALLINT 
    DEFINE ls_ren_pan         SMALLINT     

    DEFINE lr_datos_mto       RECORD
    	                          nss                  LIKE pen_ctr_pago_det_iss.nss,
                                consecutivo          LIKE pen_ctr_pago_det_iss.consecutivo,
                                sec_contrato         LIKE pen_ctr_pago_det_iss.sec_contrato,
                                fecha_pago_estimada  LIKE pen_ctr_pago_det_iss.fecha_pago_estimada,
                                mto_pago_pesos       LIKE pen_ctr_pago_det_iss.mto_pago_pesos,
                                num_mensualidad      LIKE pen_ctr_pago_det_iss.num_mensualidad,
                                folio_lote           LIKE pen_solicitud_iss.folio_lote,
                                diag_registro        LIKE pen_solicitud_iss.diag_registro,
                                marca_ult_pago       LIKE pen_ctr_pago_det_iss.marca_ult_pago
                              END RECORD   
                              
    DEFINE lar_det_montos     ARRAY[500] OF RECORD
    	                          selecc               CHAR(1),
  	                            nss                  LIKE pen_ctr_pago_det_iss.nss,
                                consecutivo          LIKE pen_ctr_pago_det_iss.consecutivo,
                                sec_contrato         LIKE pen_ctr_pago_det_iss.sec_contrato,
                                fecha_pago_estimada  LIKE pen_ctr_pago_det_iss.fecha_pago_estimada,
                                mto_pago_pesos       LIKE pen_ctr_pago_det_iss.mto_pago_pesos,
                                num_mensualidad      LIKE pen_ctr_pago_det_iss.num_mensualidad,
                                folio_lote           LIKE pen_solicitud_iss.folio_lote,
                                diag_registro        LIKE pen_solicitud_iss.diag_registro,
                                marca_ult_pago       LIKE pen_ctr_pago_det_iss.marca_ult_pago
                              END RECORD
                              
    DEFINE lr_det_sub         RECORD LIKE pen_detalle_sol.*
                              
    DEFINE lar_det_sub        ARRAY[500] OF RECORD 
                                mto_30         LIKE pen_detalle_sol.monto_en_pesos,
                                acc_30         LIKE pen_detalle_sol.monto_en_acciones,
                                mto_31         LIKE pen_detalle_sol.monto_en_pesos,
                                acc_31         LIKE pen_detalle_sol.monto_en_acciones,
                                mto_32         LIKE pen_detalle_sol.monto_en_pesos,
                                acc_32         LIKE pen_detalle_sol.monto_en_acciones,
                                mto_33         LIKE pen_detalle_sol.monto_en_pesos,
                                acc_33         LIKE pen_detalle_sol.monto_en_acciones,
                                mto_34         LIKE pen_detalle_sol.monto_en_pesos,
                                acc_34         LIKE pen_detalle_sol.monto_en_acciones,
                                mto_35         LIKE pen_detalle_sol.monto_en_pesos,
                                acc_35         LIKE pen_detalle_sol.monto_en_acciones,
                                mto_tot        LIKE pen_detalle_sol.monto_en_pesos
                              END RECORD

    DEFINE ls_opera           SMALLINT   
    DEFINE li_cnt             INTEGER
    DEFINE li_sub             INTEGER
    DEFINE ld_tot_general     LIKE pen_detalle_sol.monto_en_pesos

    #-- --------------------------------------------------------------------------------
   
    OPEN WINDOW penc2052 AT 2,2 WITH FORM "PENC2052" 
    DISPLAY " <CTRL-P> - Preliquidar y Liquidar Cuentas                <CTRL-C> - Regresar " AT 1,1 ATTRIBUTE(REVERSE)    
    DISPLAY " PENC205          PRELIQUIDACION MENSUAL DE REGISTROS DE PMG                  " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 2,68 ATTRIBUTE(REVERSE)    
    
    CALL f_tablas_tmp()

    FOR  li_cnt = 1 TO 500
        INITIALIZE lar_det_montos[li_cnt].* TO NULL
        LET lar_det_sub[li_cnt].mto_30  = 0
        LET lar_det_sub[li_cnt].acc_30  = 0
        LET lar_det_sub[li_cnt].mto_31  = 0
        LET lar_det_sub[li_cnt].acc_31  = 0
        LET lar_det_sub[li_cnt].mto_32  = 0
        LET lar_det_sub[li_cnt].acc_32  = 0
        LET lar_det_sub[li_cnt].mto_33  = 0
        LET lar_det_sub[li_cnt].acc_33  = 0
        LET lar_det_sub[li_cnt].mto_34  = 0
        LET lar_det_sub[li_cnt].acc_34  = 0        
        LET lar_det_sub[li_cnt].mto_35  = 0
        LET lar_det_sub[li_cnt].acc_35  = 0
        LET lar_det_sub[li_cnt].mto_tot = 0
    END FOR

    LET ls_opera  = 1
    LET li_cnt    = 0
    LET li_sub    = 0
    
    -- SUMA DE TODOS LOS MONTOS EN PESOS 
    LET ld_tot_general = 0 

    DECLARE cur_nss CURSOR WITH HOLD FOR
    	
      SELECT A.nss,                  A.consecutivo,      A.sec_contrato,    
             A.fecha_pago_estimada,  A.mto_pago_pesos,   A.num_mensualidad,
             B.folio_lote,           B.diag_registro,    A.marca_ult_pago
             
        FROM pen_ctr_pago_det_iss  A,    -- MLM-3197
             pen_solicitud_iss     B     -- MLM-3197
             
       WHERE A.nss          =  B.nss
         AND A.consecutivo  =  B.consecutivo
       --AND A.estado       =  B.estado_solicitud
         AND A.estado       =  gr_edo.recibido
         AND A.folio_op72   =  pi_folio
         
       ORDER BY 5,2

    FOREACH cur_nss INTO lr_datos_mto.*

        LET li_cnt = li_cnt + 1
        LET lar_det_montos[li_cnt].selecc               = ""
    	  LET lar_det_montos[li_cnt].nss                  = lr_datos_mto.nss
        LET lar_det_montos[li_cnt].consecutivo          = lr_datos_mto.consecutivo
        LET lar_det_montos[li_cnt].sec_contrato         = lr_datos_mto.sec_contrato
        LET lar_det_montos[li_cnt].fecha_pago_estimada  = lr_datos_mto.fecha_pago_estimada
        LET lar_det_montos[li_cnt].mto_pago_pesos       = lr_datos_mto.mto_pago_pesos
        LET lar_det_montos[li_cnt].num_mensualidad      = lr_datos_mto.num_mensualidad   
        LET lar_det_montos[li_cnt].folio_lote           = lr_datos_mto.folio_lote
        LET lar_det_montos[li_cnt].diag_registro        = lr_datos_mto.diag_registro
        LET lar_det_montos[li_cnt].marca_ult_pago       = lr_datos_mto.marca_ult_pago

        INSERT INTO tmp_por_liquidar
        VALUES(lr_datos_mto.*)

        DECLARE cur_prop CURSOR WITH HOLD FOR 
          SELECT a.nss,                 a.consecutivo,      
                 a.folio_lote,          a.tipo_retiro,      
                 a.siefore,             a.subcuenta,        
                 a.fecha_valuacion,     a.monto_en_pesos/b.precio_del_dia,
                 a.monto_en_pesos,      a.estado_sub_viv,
                 a.num_mensualidad  
            FROM pen_detalle_sol a,   glo_valor_accion b
           WHERE a.nss              = lr_datos_mto.nss
             AND a.consecutivo      = lr_datos_mto.consecutivo
             AND a.subcuenta       IN (30,31,32,33,34,35) 
             AND a.folio_lote       = pi_folio
             AND a.siefore          = b.codigo_siefore
             AND b.fecha_valuacion  = pdt_fecha_proceso
        	
        FOREACH cur_prop  INTO  lr_det_sub.*
        	
        	LET li_sub = li_sub + 1 
        	
    	    LET lar_det_sub[li_cnt].mto_tot = lar_det_sub[li_cnt].mto_tot + lr_det_sub.monto_en_pesos		  
    	    
    	    LET ld_tot_general = ld_tot_general + lr_det_sub.monto_en_pesos
        	 
        	CASE lr_det_sub.subcuenta 
        		   WHEN 30   
        		   	    LET lar_det_sub[li_cnt].mto_30  = lr_det_sub.monto_en_pesos
        		   	    LET lar_det_sub[li_cnt].acc_30  = lr_det_sub.monto_en_acciones
        		   WHEN 31 
        		   	    LET lar_det_sub[li_cnt].mto_31  = lr_det_sub.monto_en_pesos
        		   	    LET lar_det_sub[li_cnt].acc_31  = lr_det_sub.monto_en_acciones
        		   WHEN 32 
        		   	    LET lar_det_sub[li_cnt].mto_32  = lr_det_sub.monto_en_pesos
        		   	    LET lar_det_sub[li_cnt].acc_32  = lr_det_sub.monto_en_acciones
        		   WHEN 33
        		   	    LET lar_det_sub[li_cnt].mto_33  = lr_det_sub.monto_en_pesos
        		   	    LET lar_det_sub[li_cnt].acc_33  = lr_det_sub.monto_en_acciones
        		   WHEN 34
        		   	    LET lar_det_sub[li_cnt].mto_34  = lr_det_sub.monto_en_pesos
        		   	    LET lar_det_sub[li_cnt].acc_34  = lr_det_sub.monto_en_acciones
        		   WHEN 35
        		   	    LET lar_det_sub[li_cnt].mto_35  = lr_det_sub.monto_en_pesos
        		   	    LET lar_det_sub[li_cnt].acc_35  = lr_det_sub.monto_en_acciones
        	END CASE 	   	

        END FOREACH -- Pago prop
        FREE cur_prop

    END FOREACH

    IF (li_cnt = 0) THEN
        LET ls_opera = 0
        CALL f_error_msg("NO EXISTEN REGISTROS EN EL FOLIO INDICADO")
    ELSE
    	  -- MOSTRAR EL TOTAL 
    	  DISPLAY BY NAME ld_tot_general
    	  
    	  -- MOTRAR LOS DATOS GENERALES 
        CALL SET_COUNT(li_cnt)
        INPUT ARRAY lar_det_montos WITHOUT DEFAULTS FROM scr_arr.*  
        	
           -- MOSTRAR DATOS ADICIONALES
           BEFORE ROW 
             LET ls_ren_act = ARR_CURR()
             LET ls_ren_pan = SCR_LINE()
             
             -- AVISAR QUE SE HA LLEGADO AL FINAL DE LOS REGITROS
             IF ls_ren_act > li_cnt AND fgl_lastkey()=fgl_keyval("DOWN") THEN 
                ERROR "NO HAY MAS REGISTROS"
                SLEEP 2
                ERROR ""
                INITIALIZE lar_det_sub[ls_ren_act].* TO NULL 
                DISPLAY BY NAME lar_det_sub[ls_ren_act].* 
             ELSE 
                IF ls_ren_act <= li_cnt THEN
                   DISPLAY BY NAME lar_det_sub[ls_ren_act].* 
                END IF 
             END IF                     	   	
        
            ON KEY (CONTROL-P)
                WHILE TRUE
                    PROMPT "EJECUTAR LA PRELIQUIDACION Y LIQUIDACION DE PMG (S/N)? " 
                           ATTRIBUTE(REVERSE) FOR CHAR enter
                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[sS]" THEN
                            LET ls_opera = 1
                        ELSE
                            LET ls_opera = 0
                        END IF
                        EXIT INPUT
                    END IF
                END WHILE
        
            ON KEY (CONTROL-C, INTERRUPT)
                LET ls_opera = 0
                EXIT INPUT
        
        END INPUT
    END IF

    CLOSE WINDOW penc2052

    RETURN ls_opera

END FUNCTION

#==============================================================================#
# f_preliquida_registros : Realiza la preliquidacion de los nss que entran  #
#                          en el rango de fechas capturado por el usuario   #
#==============================================================================#
FUNCTION f_preliquida_registros(p_folio)
    
    DEFINE p_folio            INTEGER
    
    DEFINE lr_pagos           RECORD
                              nss                  LIKE pen_ctr_pago_det_iss.nss,
                              consecutivo          LIKE pen_solicitud_iss.consecutivo,
                              sec_contrato         LIKE pen_solicitud_iss.sec_contrato,
                              fecha_pago_estimada  LIKE pen_ctr_pago_det_iss.fecha_pago_estimada,
                              mto_pago_pesos       LIKE pen_ctr_pago_det_iss.mto_pago_pesos,
                              num_mensualidad      LIKE pen_ctr_pago_det_iss.num_mensualidad,
                              folio_lote           LIKE pen_solicitud_iss.folio_lote,
                              diag_registro        LIKE pen_solicitud_iss.diag_registro,
                              marca_ult_pago       LIKE pen_ctr_pago_det_iss.marca_ult_pago      
                              END RECORD 

    DEFINE lr_detalle_pago    RECORD LIKE pen_detalle_sol.*
    DEFINE lr_preliquida      RECORD LIKE pen_preliquida_pmg.*
    DEFINE v_tipo_retiro      CHAR(01)

    DEFINE ls_flag            SMALLINT
    DEFINE ls_sie             SMALLINT

    -------------------------------------------------------------------------

    -- Inicializamos los valores que son generales para la preliquidacion
    LET lr_preliquida.folio             = -1
    LET lr_preliquida.folio_sua         = NULL
    LET lr_preliquida.fecha_pago        = HOY
    LET lr_preliquida.fecha_conversion  = HOY
    LET lr_preliquida.dias_cotizados    = 0
    LET lr_preliquida.sucursal          = " "
    LET lr_preliquida.id_aportante      = "RETIRO" #-- PENDIENTE ****
    LET lr_preliquida.estado            = 6
    LET lr_preliquida.fecha_proceso     = HOY
    LET lr_preliquida.usuario           = gc_usuario
    LET lr_preliquida.fecha_archivo     = HOY
    LET lr_preliquida.etiqueta          = 1
    LET lr_preliquida.estado_pmg        = 60 --gr_edo.preliquidado

    -------------------------------------------------------------------------

    ---------------------------------------------------
    --SELECCIONA LOS REGISTROS IDENTIFICADOS PARA PAGO
    ---------------------------------------------------
    DECLARE cur_uno CURSOR FOR
    SELECT  *
    FROM    tmp_por_liquidar
    ORDER BY nss

    FOREACH cur_uno INTO lr_pagos.*
      
DISPLAY "PROCESANDO NSS:",lr_pagos.nss          --debug
CALL ERRORLOG("PROCESANDO NSS:"||lr_pagos.nss)  --debug

        SELECT tipo_retiro
        INTO   v_tipo_retiro
        FROM   pen_solicitud_iss
        WHERE  nss = lr_pagos.nss 
        AND    consecutivo = lr_pagos.consecutivo
        IF v_tipo_retiro = 'G' THEN 
        	LET lr_preliquida.tipo_movimiento = 857;
        ELSE
        	LET lr_preliquida.tipo_movimiento = 859;
        END IF

        -- Verificamos si el monto a pagar no sobregira la cuenta
        CALL f_verifica_sobregiro(lr_pagos.nss,
                                  lr_pagos.consecutivo,
                                  lr_pagos.mto_pago_pesos)
            RETURNING ls_flag

        IF ls_flag = 0 AND lr_pagos.marca_ult_pago IS NULL THEN
            
            --DETERMINA MONTO A PAGAR CON BASE A LO CALCULADO EN PEN_DETALLE_SOL
            
            -- Inicializamos los valores que son propios de cada nss
            LET lr_preliquida.nss               = lr_pagos.nss
            LET lr_preliquida.consecutivo_lote  = lr_pagos.consecutivo
            LET lr_preliquida.mensualidad       = lr_pagos.num_mensualidad
            
            INITIALIZE lr_detalle_pago.* TO NULL

            --SELECCIONA CADA UNO DE LOS MOVIMIENTOS PROVISIONADOS
            DECLARE cur_det CURSOR FOR
            SELECT *
            FROM   pen_detalle_sol
            WHERE  nss             = lr_pagos.nss
            AND    consecutivo     = lr_pagos.consecutivo
            AND    num_mensualidad = lr_pagos.num_mensualidad
            AND    folio_lote      = p_folio  
            ORDER BY siefore, subcuenta
            
            FOREACH cur_det INTO lr_detalle_pago.*
DISPLAY "NSS:",lr_pagos.nss," Consecutivo:",lr_pagos.consecutivo," Subcuenta: ",lr_detalle_pago.subcuenta," Siefore:",lr_detalle_pago.siefore
CALL ERRORLOG("NSS:"||lr_pagos.nss||" Consecutivo:"||lr_pagos.consecutivo||" Subcuenta: "||lr_detalle_pago.subcuenta||" Siefore:"||lr_detalle_pago.siefore)
                -- Insertamos la tabla pen_detalle_sol en su tabla temporal correspondiente
                INSERT INTO tmp_detalle_sol
                VALUES (lr_detalle_pago.*)
            
                LET ls_sie                          = lr_detalle_pago.siefore
                LET lr_preliquida.precio_accion     = gar_precio_acc[ls_sie].precio_dia
            
                LET lr_preliquida.subcuenta         = lr_detalle_pago.subcuenta
                LET lr_preliquida.siefore           = lr_detalle_pago.siefore
                LET lr_preliquida.monto_en_pesos    = lr_detalle_pago.monto_en_pesos * -1
            
                IF lr_detalle_pago.subcuenta = 4 THEN
                    LET lr_preliquida.monto_en_acciones = lr_detalle_pago.monto_en_acciones * -1
                    LET lr_preliquida.fecha_valor       = lr_detalle_pago.fecha_valuacion
                ELSE
                    LET lr_preliquida.monto_en_acciones = (lr_detalle_pago.monto_en_pesos/lr_preliquida.precio_accion) * -1
                    LET lr_preliquida.fecha_valor       = HOY
            
                    UPDATE tmp_detalle_sol
                    SET    fecha_valuacion      = lr_preliquida.fecha_valor      ,
                           monto_en_acciones    = lr_preliquida.monto_en_acciones
                    WHERE  nss                  = lr_pagos.nss
                    AND    consecutivo          = lr_pagos.consecutivo
                    AND    subcuenta            = lr_detalle_pago.subcuenta
                    AND    num_mensualidad      = lr_pagos.num_mensualidad
                END IF
            
                --INSERTA MOVIMIENTO DE PRELIQUIDACION PREVIO
                INSERT INTO tmp_preliquida_pmg
                VALUES (lr_preliquida.*)
            
                INITIALIZE lr_detalle_pago.* TO NULL
            
            END FOREACH -- Detalles de pago por subcuenta
            
        ELSE
        	
            --REGISTRA MOVIMIENTOS DE PRELIQUIDACION CON BASE AL SALDO
            CALL f_agota_saldo(lr_pagos.nss         ,
                               lr_pagos.consecutivo ,
                               lr_pagos.num_mensualidad    )
            
        END IF


    END FOREACH -- NSS con pago 1

END FUNCTION

#==============================================================================#
# f_agota_saldo : inserta movimientos de liquidacion con base al saldo que     #
#                 tenga el trabajador en la subcuenta                          #
# Autor : ISAI JIMENEZ ROJAS                                                   #
#==============================================================================#
FUNCTION f_agota_saldo(p_nss, p_consecutivo, p_num_mensualidad)
   
    DEFINE p_nss              LIKE pen_detalle_sol.nss
    DEFINE p_consecutivo      LIKE pen_detalle_sol.consecutivo
    DEFINE p_num_mensualidad  LIKE pen_detalle_sol.num_mensualidad

    DEFINE lr_detalle_pago    RECORD LIKE pen_detalle_sol.*
    DEFINE lr_preliquida      RECORD LIKE pen_preliquida_pmg.*
    DEFINE lr_saldo           RECORD
                                subcuenta          SMALLINT     ,
                                siefore            SMALLINT     ,
                                monto_acc          DECIMAL(16,6),
                                monto_pesos        DECIMAL(16,6)
                              END RECORD

    DEFINE ls_subcta          SMALLINT
    DEFINE ls_sie             SMALLINT 
    DEFINE ls_gr_saldo        SMALLINT
    DEFINE ls_grupo           SMALLINT
    DEFINE ls_estado_sub_viv  SMALLINT



    ---------------------------------------------------------------------------
    
    INITIALIZE lr_preliquida.* TO NULL
    
    -- Inicializamos los valores que son propios de cada nss
    LET lr_preliquida.nss               = p_nss
    LET lr_preliquida.consecutivo_lote  = p_consecutivo
    LET lr_preliquida.mensualidad       = p_num_mensualidad
            
    LET lr_preliquida.folio             = -1
    LET lr_preliquida.folio_sua         = NULL
    LET lr_preliquida.fecha_pago        = HOY
    LET lr_preliquida.fecha_conversion  = HOY
    LET lr_preliquida.dias_cotizados    = 0
    LET lr_preliquida.sucursal          = " "
    LET lr_preliquida.id_aportante      = "RETIRO" #-- PENDIENTE ****
    LET lr_preliquida.estado            = 6
    LET lr_preliquida.fecha_proceso     = HOY
    LET lr_preliquida.usuario           = gc_usuario
    LET lr_preliquida.fecha_archivo     = HOY
    LET lr_preliquida.etiqueta          = 1
    LET lr_preliquida.estado_pmg        = gr_edo.preliquidado
    LET lr_preliquida.tipo_movimiento   = 857


    LET ls_subcta = 0 
    LET ls_grupo  = 0

    --SELECCIONA SALDO DE CADA UNA DE LAS SUBCUENTAS
    DECLARE cur_agota_saldo CURSOR WITH HOLD FOR eje_saldo_dia

    --RECUPERA SALDO DE CADA SUBCUENTA
    FOREACH cur_agota_saldo USING p_nss      ,
                                  ls_subcta  ,
                                  ls_gr_saldo,
                                  HOY
                            INTO  lr_saldo.*

        IF lr_saldo.subcuenta != 30 AND 
           lr_saldo.subcuenta != 31 AND 
           lr_saldo.subcuenta != 31 AND 
           lr_saldo.subcuenta != 32 AND 
           lr_saldo.subcuenta != 33 AND 
           lr_saldo.subcuenta != 34 AND 
           lr_saldo.subcuenta != 35 THEN
           --subcuentas no validas para pmg issste
           CONTINUE FOREACH
        ELSE
DISPLAY "AGOTANDO SALDO DE NSS: ",p_nss,"SUBCUENTA:",lr_saldo.subcuenta," SIEFORE:",lr_saldo.siefore," MONTO: ",lr_saldo.monto_pesos                 --debug
CALL ERRORLOG("AGOTANDO SALDO DE NSS: "||p_nss||"SUBCUENTA:"||lr_saldo.subcuenta||" SIEFORE:"||lr_saldo.siefore||" MONTO: "||lr_saldo.monto_pesos)   --debug

             -- SE RECUPERA EL MONTO PROVISIONADO EN OP78 PARA LA SUBCUENTA
             SELECT *
             INTO   lr_detalle_pago.*
             FROM   pen_detalle_sol
             WHERE  nss             = p_nss
             AND    consecutivo     = p_consecutivo
             AND    num_mensualidad = p_num_mensualidad
             AND    subcuenta       = ls_subcta

             LET ls_sie                          = lr_saldo.siefore
             LET lr_preliquida.precio_accion     = gar_precio_acc[ls_sie].precio_dia
            
             LET lr_preliquida.subcuenta         = lr_saldo.subcuenta
             LET lr_preliquida.siefore           = lr_saldo.siefore
             LET lr_preliquida.monto_en_pesos    = lr_saldo.monto_pesos * -1
            
             LET lr_preliquida.monto_en_acciones = lr_saldo.monto_acc * -1
             LET lr_preliquida.fecha_valor       = HOY
            
             --INSERTA MOVIMIENTO PRELIQUIDADO DE FORMA TEMPORAL
             INSERT INTO tmp_preliquida_pmg
             VALUES (lr_preliquida.*)
            
             INITIALIZE lr_detalle_pago.* TO NULL
        END IF
       
    END FOREACH

END FUNCTION

#==============================================================================#
# f_actualiza_tablas : Se inserta en las tablas fisicas los registros          #
#                      almacenados en las tablas temporales y se actualizan    #
#                      el resto de las tablas involucradas                     #
#==============================================================================#
FUNCTION f_actualiza_tablas(li_folio_pre)

    DEFINE li_folio_pre    INTEGER   

    DEFINE lr_tmp_detalle  RECORD LIKE pen_detalle_sol.*
    DEFINE lr_preliquida   RECORD
                           nss             LIKE pen_preliquida_pmg.nss,
                           consecutivo     LIKE pen_preliquida_pmg.consecutivo_lote,
                           mensualidad     LIKE pen_ctr_pago_det.num_mensualidad
                           END RECORD
    DEFINE li_folio        INTEGER 
    DEFINE li_cont_tot     INTEGER
    DEFINE ls_edo_pago     SMALLINT


    ----------------------------------------------------------------------------

    CALL f_obtiene_folio() RETURNING li_folio
         
    LET li_cont_tot = 0

    UPDATE tmp_preliquida_pmg
    SET    folio = li_folio
    WHERE  folio = -1

    DECLARE cur_tmp_det CURSOR FOR
    SELECT *
    FROM   tmp_detalle_sol
    WHERE  siefore <> 11
    ORDER BY nss, siefore, subcuenta

    FOREACH cur_tmp_det INTO lr_tmp_detalle.*

      UPDATE pen_detalle_sol
         SET monto_en_acciones    = lr_tmp_detalle.monto_en_acciones,
             fecha_valuacion      = lr_tmp_detalle.fecha_valuacion
       WHERE nss                  = lr_tmp_detalle.nss
         AND consecutivo          = lr_tmp_detalle.consecutivo
         AND folio_lote           = lr_tmp_detalle.folio_lote
         AND siefore              <> 11
         AND subcuenta            = lr_tmp_detalle.subcuenta

    END FOREACH -- Detalle sol

    ----------------------------------------------------------------------------
    
    --SELECCIONA CADA UNA DE LAS SOLICITUDES DEL FOLIO PRELIQUIDADO
    DECLARE cur_preliq CURSOR FOR
    SELECT UNIQUE(nss)     ,
           consecutivo_lote,
           mensualidad
    FROM   tmp_preliquida_pmg
    WHERE  folio = li_folio

    FOREACH cur_preliq INTO lr_preliquida.*

        LET li_cont_tot         = li_cont_tot + 1

        LET ls_edo_pago = gr_edo.en_pago
       
        UPDATE pen_solicitud_iss
        SET    fecha_est_pago   = HOY,
               estado_solicitud = ls_edo_pago
        WHERE nss              = lr_preliquida.nss
        AND   consecutivo      = lr_preliquida.consecutivo
        AND   estado_solicitud <> gr_edo.rechazado


        UPDATE pen_ctr_pago_det_iss
        SET   fecha_liquida    = HOY               ,
              folio_liquida    = li_folio          ,
              estado           = 60 --gr_edo.preliquidado
        WHERE nss              = lr_preliquida.nss
        AND   consecutivo      = lr_preliquida.consecutivo
        AND   num_mensualidad  = lr_preliquida.mensualidad
        AND   estado           = gr_edo.recibido

        INSERT INTO pen_preliquida_pmg
        SELECT *
        FROM   tmp_preliquida_pmg
        WHERE  nss              = lr_preliquida.nss
        AND    consecutivo_lote = lr_preliquida.consecutivo
        AND    folio            = li_folio
   
    END FOREACH -- Siguiente NSS

    #-- Despliega resultados
    DISPLAY "          TOTAL DE REGISTROS PRELIQUIDADOS : ", li_cont_tot,"          " AT 6,10
    DISPLAY "          FOLIO DE LIQUIDACION             : ", li_folio,   "          " AT 8,10
    
    # INICIAR LA LIQUIDACION 
    CALL f_liquida_pmg_iss(li_folio,li_folio_pre)

    PROMPT "PROCESO DE PRELIQUIDACION Y LIQUIDACION FINALIZADO, PRESIONE <ENTER>:" FOR CHAR ENTER 

END FUNCTION


#==============================================================================#
# f_recomienda_folio : Revisa el ultimo folio que cumpla con la condicion   #
#==============================================================================#
FUNCTION f_recomienda_folio()

  DEFINE li_folio   INTEGER  

  -- Validamos que existan registros
  SELECT MAX(folio_op72)
    INTO li_folio
    FROM pen_ctr_pago_det_iss
   WHERE estado = 40
  
  IF SQLCA.SQLCODE = NOTFOUND OR li_folio IS NULL THEN 
  	 LET li_folio = 0 
  END IF  	  
  
  RETURN li_folio

END FUNCTION


#==============================================================================#
# f_valida_ejecucion : Valida que se cumplan las condiciones previas para   #
#                      ejecutar el programa                                 #
#==============================================================================#
FUNCTION f_valida_ejecucion(li_folio)

    DEFINE li_folio   INTEGER 
    
    DEFINE ls_salida  SMALLINT
    DEFINE li_regs    INTEGER

    LET li_regs   = 0
    LET ls_salida = 1

    -- --------------------------------------------------------------------

    -- Validamos y obtenemos precios de accion
    CALL f_obtiene_precios_accion(HOY)

    -- Validamos que existan registros
    SELECT NVL(COUNT(UNIQUE folio_op72),0)
      INTO li_regs
      FROM pen_ctr_pago_det_iss
     WHERE folio_op72 = li_folio 
       AND estado     = 40 

    IF li_regs = 0 THEN
        CALL f_error_msg ("NO EXISTEN REGISTROS PARA EL FOLIO CAPTURADO")
        LET ls_salida = 0
    END IF

    RETURN ls_salida

END FUNCTION

#==============================================================================#
# f_verifica_sobregiro : Verifica si el pago que se debe realizar no        #
#                        sobregira la cuenta individual del nss             #
#==============================================================================#
FUNCTION f_verifica_sobregiro(pr_sobregiro)

    DEFINE pr_sobregiro         RECORD     
                                  nss             LIKE pen_solicitud_iss.nss,
                                  consecutivo     LIKE pen_solicitud_iss.consecutivo,
                                  total_pesos     LIKE pen_ctr_pago_det_iss.mto_pago_pesos
                                END RECORD
                               
    DEFINE lr_saldo             RECORD
                                  subcuenta       SMALLINT     ,
                                  siefore         SMALLINT     ,
                                  monto_acc       DECIMAL(16,6),
                                  monto_pesos     DECIMAL(16,6)
                                END RECORD
                               
    DEFINE ls_sobregiro         SMALLINT 
    DEFINE ls_subcta            SMALLINT 
    DEFINE ls_gr_saldo          SMALLINT 
    DEFINE ld_saldo_dia_pesos   DECIMAL(22,6)

    -- --------------------------------------------------------------------
    
    LET ls_sobregiro        = 0
    LET ls_gr_saldo         = 0
    LET ld_saldo_dia_pesos  = 0
    
    FOR ls_subcta = 30 TO 35 
    
      DECLARE cur_sct_sgiro CURSOR FOR eje_saldo_dia

      FOREACH cur_sct_sgiro USING pr_sobregiro.nss ,
                                    ls_subcta        ,
                                    ls_gr_saldo      ,
                                    HOY               
                              INTO  lr_saldo.*        
	
         LET ld_saldo_dia_pesos = ld_saldo_dia_pesos + lr_saldo.monto_pesos
           
      END FOREACH
        
    END FOR 
    
    -- Si el monto a pagar es mayor al monto en pesos de la cuenta entonces se
    -- detecta un sobregiro
    IF pr_sobregiro.total_pesos > ld_saldo_dia_pesos THEN
        LET ls_sobregiro = 1
    END IF
    
    RETURN ls_sobregiro

END FUNCTION

#==============================================================================#
# f_tablas_tmp : Genera las tablas temporales donde se almacenan los        #
#                calculos y cambios                                         #
#==============================================================================#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_por_liquidar
        DROP TABLE tmp_preliquida_pmg
        DROP TABLE tmp_detalle_sol
    WHENEVER ERROR STOP

    CREATE TEMP TABLE tmp_por_liquidar
    (
      nss                  CHAR(11),
      consecutivo          DECIMAL(11,0),
      sec_contrato         SMALLINT,
      fecha_pago_estimada  DATE,
      mto_pago_pesos       DECIMAL(16,2),
      num_mensualidad      SMALLINT,
      folio_lote           INTEGER,
      diag_registro        CHAR(3),
      marca_ult_pago       CHAR(1)
    )

    #-- --------------------------------------------------------------------

    SELECT *
      FROM pen_preliquida_pmg
     WHERE 1 = 0
      INTO TEMP tmp_preliquida_pmg

    #-- --------------------------------------------------------------------

    SELECT *
      FROM pen_detalle_sol
     WHERE 1 = 0
      INTO TEMP tmp_detalle_sol

    #-- --------------------------------------------------------------------

END FUNCTION


#==============================================================================#
# f_error_msg : Formatea y despliega los mensajes de error en la pantalla   #
#==============================================================================#
FUNCTION f_error_msg(pc_mensaje)

    DEFINE  pc_mensaje  CHAR(75)

    LET pc_mensaje = " ", pc_mensaje CLIPPED , "PRESIONE <ENTER>: "
    PROMPT pc_mensaje CLIPPED ATTRIBUTE(REVERSE) FOR CHAR enter

END FUNCTION

#==============================================================================#
# f_obtiene_nombres : Regresa los nombres del nss dado                      #
#==============================================================================#
FUNCTION f_obtiene_nombres(pc_nss)

    DEFINE pc_nss LIKE pen_solicitud_iss.nss

    DEFINE lr_afi RECORD
        ap_paterno      LIKE afi_mae_afiliado.paterno   ,
        ap_materno      LIKE afi_mae_afiliado.materno   ,
        nombres         LIKE afi_mae_afiliado.nombres
    END RECORD

    --  ------------------------------------------------------------------------

    SELECT paterno  ,
           materno  ,
           nombres
    INTO   lr_afi.*
    FROM   afi_mae_afiliado
    WHERE  n_seguro = pc_nss

    RETURN lr_afi.*

END FUNCTION

#==============================================================================#
# f_obtiene_folio: Obtiene el ultimo folio para asignarse en                #
#                  el proceso de preliquidacion                             #
#==============================================================================#
FUNCTION f_obtiene_folio()

    DEFINE  li_ult_folio    INTEGER

    SELECT NVL(MAX(folio),0) + 1
      INTO li_ult_folio
      FROM glo_folio

    INSERT INTO glo_folio
    VALUES (li_ult_folio)

    RETURN li_ult_folio

END FUNCTION

#==============================================================================#
# f_obtiene_precios_accion : Obtiene los precios de accion para la fecha    #
#                            dada por el usuario                            #
#==============================================================================#
FUNCTION f_obtiene_precios_accion(pdt_fec_precios)

    DEFINE pdt_fec_precios    DATE
    DEFINE lr_precio_acc      RECORD
                                estado      SMALLINT     ,
                                fecha       DATE         ,
                                siefore     SMALLINT     ,
                                precio_dia  DECIMAL(16,6)
                              END RECORD
  
    DEFINE lc_exe_precios     CHAR(100) 
    DEFINE lc_mensaje         CHAR(100) 
    DEFINE lc_siefore         CHAR(002)
    DEFINE li_cont            SMALLINT

    LET li_cont = 0

    LET lc_exe_precios = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM lc_exe_precios

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING pdt_fec_precios
                      INTO lr_precio_acc.*

        -- Solo se valida que existan los precios de las siefores basicas (1 a 5)
        IF (lr_precio_acc.siefore < 10 AND lr_precio_acc.estado <> 0) THEN
            LET lc_siefore = lr_precio_acc.siefore
            LET lc_mensaje = " FALTAN PRECIOS ACCION: DIA ", pdt_fec_precios,
                             ", SIEFORE: ", lc_siefore

            LET lc_mensaje = lc_mensaje CLIPPED
            CALL f_error_msg(lc_mensaje)
            EXIT PROGRAM
        ELSE
            -- Se almacena cada precio de accion el lugar correspondiente
            -- dentro del arreglo de precios.
            LET li_cont                   = lr_precio_acc.siefore
            LET gar_precio_acc[li_cont].* = lr_precio_acc.*
        END IF

    END FOREACH

END FUNCTION

#==============================================================================#

{
FUNCTION f_agota_saldo(p_nss, p_consecutivo, p_num_mensualidad)
   
    DEFINE p_nss              LIKE pen_detalle_sol.nss
    DEFINE p_consecutivo      LIKE pen_detalle_sol.consecutivo
    DEFINE p_num_mensualidad  LIKE pen_detalle_sol.num_mensualidad

    DEFINE lr_detalle_pago    RECORD LIKE pen_detalle_sol.*
    DEFINE lr_preliquida      RECORD LIKE pen_preliquida_pmg.*
    DEFINE lr_saldo           RECORD
                                subcuenta          SMALLINT     ,
                                siefore            SMALLINT     ,
                                monto_acc          DECIMAL(16,6),
                                monto_pesos        DECIMAL(16,6)
                              END RECORD

    DEFINE ls_subcta          SMALLINT
    DEFINE ls_sie             SMALLINT 
    DEFINE ls_gr_saldo        SMALLINT
    DEFINE ls_grupo           SMALLINT
    DEFINE ls_estado_sub_viv  SMALLINT

DISPLAY "AGOTANDO SALDO DE NSS: ",p_nss
CALL ERRORLOG("AGOTANDO SALDOS NSS:"||p_nss)
    ---------------------------------------------------------------------------
    
    INITIALIZE lr_preliquida.* TO NULL
    
    -- Inicializamos los valores que son propios de cada nss
    LET lr_preliquida.nss               = p_nss
    LET lr_preliquida.consecutivo_lote  = p_consecutivo
    LET lr_preliquida.mensualidad       = p_num_mensualidad
            
    LET lr_preliquida.folio             = -1
    LET lr_preliquida.folio_sua         = NULL
    LET lr_preliquida.fecha_pago        = HOY
    LET lr_preliquida.fecha_conversion  = HOY
    LET lr_preliquida.dias_cotizados    = 0
    LET lr_preliquida.sucursal          = " "
    LET lr_preliquida.id_aportante      = "RETIRO" #-- PENDIENTE ****
    LET lr_preliquida.estado            = 6
    LET lr_preliquida.fecha_proceso     = HOY
    LET lr_preliquida.usuario           = gc_usuario
    LET lr_preliquida.fecha_archivo     = HOY
    LET lr_preliquida.etiqueta          = 1
    LET lr_preliquida.estado_pmg        = gr_edo.preliquidado

    --OBTIENE EL GRUPO ASOCIADO CON LA SOLICITUD
    SELECT grupo
      INTO ls_grupo
      FROM pen_solicitud_iss
     WHERE nss          = p_nss
       AND consecutivo  = p_consecutivo
    
    --SELECCIONA SUBCUENTAS ASOCIADAS AL GRUPO DE LA SOLICITUD
    DECLARE cur_subcta CURSOR FOR
     SELECT subcuenta
       FROM tab_agrupa_subcta
      WHERE grupo = ls_grupo
      ORDER BY 1
    
    --DE CADA SUBCUENTA RECUPERA EL SALDO
    FOREACH cur_subcta INTO ls_subcta
        
        DECLARE cur_agota_saldo CURSOR WITH HOLD FOR eje_saldo_dia

        --RECUPERA SALDO DE CADA SUBCUENTA
        FOREACH cur_agota_saldo USING p_nss,
                                      ls_subcta,
                                      ls_gr_saldo,
                                      HOY               
                                INTO  lr_saldo.*

             -- SE RECUPERA EL MONTO INFORMADO EN OP78 PARA LA SUBCUENTA
             SELECT *
               INTO lr_detalle_pago.*
               FROM pen_detalle_sol
              WHERE nss             = p_nss
                AND consecutivo     = p_consecutivo
                AND num_mensualidad = p_num_mensualidad
                AND subcuenta       = ls_subcta

             -- inserta el registro identificado a actualizar
             -- Insertamos la tabla pen_detalle_sol en su tabla temporal correspondiente
             
             INSERT INTO tmp_detalle_sol
             VALUES (lr_detalle_pago.*)
            
             LET ls_sie                          = lr_saldo.siefore
             LET lr_preliquida.precio_accion     = gar_precio_acc[ls_sie].precio_dia
            
             LET lr_preliquida.subcuenta         = lr_saldo.subcuenta
             LET lr_preliquida.siefore           = lr_saldo.siefore
             LET lr_preliquida.monto_en_pesos    = lr_saldo.monto_pesos * -1
            
             IF lr_saldo.subcuenta = 4 THEN
                 LET lr_preliquida.monto_en_acciones = lr_saldo.monto_acc * -1
                 LET lr_preliquida.fecha_valor       = lr_detalle_pago.fecha_valuacion
             ELSE
                 LET lr_preliquida.monto_en_acciones = lr_saldo.monto_acc * -1
                 LET lr_preliquida.fecha_valor       = HOY
            
                 UPDATE tmp_detalle_sol
                 SET    fecha_valuacion      = lr_preliquida.fecha_valor,
                        monto_en_acciones    = lr_preliquida.monto_en_acciones
                 WHERE  nss                  = p_nss
                 AND    consecutivo          = p_consecutivo
                 AND    subcuenta            = ls_subcta
                 AND    num_mensualidad      = p_num_mensualidad
             END IF
            
             INSERT INTO tmp_preliquida_pmg
             VALUES (lr_preliquida.*)
            
             INITIALIZE lr_detalle_pago.* TO NULL
             
        END FOREACH
        
    END FOREACH
                     
END FUNCTION
}