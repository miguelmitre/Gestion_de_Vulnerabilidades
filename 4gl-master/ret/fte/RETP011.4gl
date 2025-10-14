#################################################################################
#Proyecto           => SISTEMA DE AFORES ( MEXICO )                             #
#Owner              => E.F.P.                                                   #
#Programa RETP011   => REALIZA EL CALCULO DE AJUSTE DE INTERESES Y GENERA EL    #
#                      ARCHIVO PLANO DE INFORME A PROCESAR, PARA LOS REGISTROS  #
#                      DE DEVOLUCION DE VIVIENDA                                #
#Fecha creacion    => 4 DE JUNIO DE 2013                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   =>                                                           #
#Actualizacion     =>                                                           #
#                  =>                                                           #
#                                                                               #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS

    DEFINE gr_modulo RECORD LIKE seg_modulo.*

    DEFINE gr_edo RECORD
        capturado           LIKE ret_estado.estado_solicitud    ,
        confirmado          LIKE ret_estado.estado_solicitud    ,
        enviado             LIKE ret_estado.estado_solicitud    ,
        rechazado           LIKE ret_estado.estado_solicitud    ,
        liquidado           LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_datos RECORD
        folio           LIKE ret_ajuste_vivienda_nss.folio  ,
        procesa         SMALLINT
    END RECORD

    DEFINE gr_movimiento RECORD
        cancela_int                 SMALLINT    ,
        retiro                      SMALLINT
    END RECORD

    DEFINE gd_precio_viv LIKE glo_valor_accion.precio_del_dia

    DEFINE
        gs_codigo_afore         SMALLINT

    DEFINE
        gdt_fecha_viv           ,
        HOY                     DATE

    DEFINE
        gc_usuario              CHAR(15)    ,
        enter                   CHAR

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_crea_log()
    CALL init()

    CALL f_captura_folio() RETURNING gr_datos.*

    IF (gr_datos.procesa = TRUE) THEN

        CALL f_tablas_tmp()

        CALL f_actualiza_ajuste_viv(gr_datos.folio)

        CALL f_calcula_interes(gr_datos.folio)

        CALL f_inserta_registros(gr_datos.folio)

        -- Generar formato TXT
        CALL f_genera_archivo_txt(gr_datos.folio)

        CALL f_lib_error_msg ("PROCESO TERMINADO CORRECTAMENTE")

    END IF

    CLOSE WINDOW retp0111

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    -- -----------------------------------------------------------------------------

    LET HOY             = TODAY
    LET gdt_fecha_viv   = MDY(MONTH(HOY),01,YEAR(HOY))
    LET gd_precio_viv   = f_lib_obten_precio_accion(gdt_fecha_viv, 11)
    LET gc_usuario      = f_lib_obten_user()

    ----- CODIGOS AFORES -----
    SELECT codigo_afore
    INTO   gs_codigo_afore
    FROM   tab_afore_local

    SELECT codigo
    INTO   gr_movimiento.cancela_int
    FROM   tab_movimiento
    WHERE  descripcion  = "CANCELACION DE INTERESES DE VIVIENDA"

    SELECT codigo
    INTO   gr_movimiento.retiro
    FROM   tab_movimiento
    WHERE  descripcion  = "RETIRO DISPOSICION VIVIENDA 97 REG 73"

    ----- ESTADOS DE SOLICITUD -----
    SELECT *
    INTO   gr_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   ret_estado A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.confirmado
    FROM   ret_estado A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.rechazado
    FROM   ret_estado A
    WHERE  A.descripcion = "RECHAZADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_folio : Permite seleccionar el folio que se utilizara para el   #
#                   calculo de intereses de vivienda                        #
#---------------------------------------------------------------------------#
FUNCTION f_captura_folio()

    DEFINE lr_captura RECORD
        folio           LIKE ret_ajuste_vivienda_nss.folio  ,
        procesa         SMALLINT
    END RECORD

    -- -----------------------------------------------------------------------------

    CALL f_abre_ventana()

    -- Obtenemos el folio de la operacion deseada
    SELECT NVL(MAX(folio), -1)
    INTO   lr_captura.folio
    FROM   ret_ajuste_vivienda_nss
    WHERE  estado   = gr_edo.capturado

    IF lr_captura.folio < 0 THEN
        CALL f_lib_error_msg("NO EXISTEN REGISTROS PARA PROVISIONAR")
        LET lr_captura.procesa  = FALSE
    ELSE
        LET lr_captura.procesa  = TRUE

        DISPLAY BY NAME lr_captura.folio

        INPUT BY NAME lr_captura.folio WITHOUT DEFAULTS

            AFTER FIELD folio
                IF lr_captura.folio IS NULL OR lr_captura.folio = 0 THEN
                    CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO O CERO")
                    NEXT FIELD folio
                END IF

                SELECT "OK"
                FROM   ret_ajuste_vivienda_nss
                WHERE  folio    = lr_captura.folio
                AND    estado   = gr_edo.capturado
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    CALL f_lib_error_msg("FOLIO INEXISTENTE")
                    INITIALIZE lr_captura.folio TO NULL
                    DISPLAY lr_captura.folio TO folio
                    NEXT FIELD folio
                END IF

            ON KEY (ESC)
                IF lr_captura.folio IS NULL OR lr_captura.folio = 0 THEN
                    CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO O CERO")
                    NEXT FIELD folio
                END IF

                SELECT "OK"
                FROM   ret_ajuste_vivienda_nss
                WHERE  folio    = lr_captura.folio
                AND    estado   = gr_edo.capturado
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    CALL f_lib_error_msg("FOLIO INEXISTENTE")
                    INITIALIZE lr_captura.folio TO NULL
                    DISPLAY lr_captura.folio TO folio
                    NEXT FIELD folio
                ELSE
                    IF f_lib_pregunta("¿DESEA REALIZAR LA CANCELACION DE INTERESES? (S/N) : ") = TRUE THEN
                        DISPLAY lr_captura.folio TO folio
                        LET lr_captura.procesa  = TRUE
                    ELSE
                        CALL f_lib_error_msg("PROCESO CANCELADO")
                        LET lr_captura.procesa  = FALSE
                    END IF

                    EXIT INPUT

                END IF

            ON KEY (CONTROL-C, INTERRUPT)
                CALL f_lib_error_msg("PROCESO CANCELADO")
                LET lr_captura.procesa  = FALSE
                EXIT INPUT
        END INPUT
    END IF

    RETURN lr_captura.*

END FUNCTION


#---------------------------------------------------------------------------#
# f_actualiza_ajuste_viv : Actualiza los montos actuales de las subcuentas  #
#                          de vivienda en la tabla ret_ajuste_vivienda_nss  #
#---------------------------------------------------------------------------#
FUNCTION f_actualiza_ajuste_viv(pi_folio)

    DEFINE
        pi_folio            INTEGER

    DEFINE lr_ajuste RECORD LIKE ret_ajuste_vivienda_nss.*

    DEFINE lr_saldo RECORD
        nss             LIKE dis_cuenta.nss         ,
        subcuenta       LIKE dis_cuenta.subcuenta   ,
        grupo           SMALLINT                    ,
        fecha_saldo     DATE
    END RECORD


    -- -----------------------------------------------------------------------------

    DISPLAY "ACTUALIZANDO SALDOS AL DIA ...." AT 7,2

    INITIALIZE lr_ajuste.* TO NULL
    INITIALIZE lr_saldo.* TO NULL

    DECLARE cur_update_ajuste CURSOR FOR
        SELECT *
        FROM   ret_ajuste_vivienda_nss
        WHERE  folio    = pi_folio
        AND    estado   = gr_edo.capturado
        ORDER BY nss, consecutivo

    FOREACH cur_update_ajuste INTO lr_ajuste.*

        LET lr_saldo.nss            = lr_ajuste.nss
        LET lr_saldo.grupo          = 0
        LET lr_saldo.fecha_saldo    = HOY --gdt_fecha_viv
        
        -- Saldos actuales de vivienda 97
        LET lr_saldo.subcuenta      = 4

        CALL f_obten_saldo_dia(lr_saldo.*) RETURNING lr_ajuste.viv_97_aivs_cta  ,
                                                     lr_ajuste.viv_97_pesos_cta

        LET lr_ajuste.viv_97_aivs_dif   = lr_ajuste.viv_97_aivs_cta - lr_ajuste.viv_97_aivs
        LET lr_ajuste.viv_97_pesos_dif  = lr_ajuste.viv_97_pesos_cta - lr_ajuste.viv_97_pesos
        
        -- Saldos actuales de vivienda 92
        LET lr_saldo.subcuenta      = 8

        CALL f_obten_saldo_dia(lr_saldo.*) RETURNING lr_ajuste.viv_92_aivs_cta  ,
                                                     lr_ajuste.viv_92_pesos_cta

        LET lr_ajuste.viv_92_aivs_dif   = lr_ajuste.viv_92_aivs_cta - lr_ajuste.viv_92_aivs
        LET lr_ajuste.viv_92_pesos_dif  = lr_ajuste.viv_92_pesos_cta - lr_ajuste.viv_92_pesos

        IF lr_ajuste.viv_97_aivs_cta > 0 OR lr_ajuste.viv_92_aivs_cta > 0 THEN
		        IF lr_ajuste.viv_92_aivs > 0 AND lr_ajuste.viv_97_aivs = 0 AND lr_ajuste.viv_97_aivs_cta > 0 THEN  #Si unicamente se pide Viv92, se toman los recursos de Viv97
                  LET lr_ajuste.viv_97_aivs = lr_ajuste.viv_92_aivs
                  LET lr_ajuste.viv_97_pesos = lr_ajuste.viv_92_pesos

                  LET lr_ajuste.viv_97_aivs_dif   = lr_ajuste.viv_97_aivs_cta - lr_ajuste.viv_97_aivs
                  LET lr_ajuste.viv_97_pesos_dif  = lr_ajuste.viv_97_pesos_cta - lr_ajuste.viv_97_pesos

                  LET lr_ajuste.viv_92_aivs_dif   = lr_ajuste.viv_92_aivs
                  LET lr_ajuste.viv_92_pesos_dif  = lr_ajuste.viv_92_pesos
                  LET lr_ajuste.viv_92_aivs       = 0
                  LET lr_ajuste.viv_92_pesos      = 0 
		        END IF
		        
		        IF lr_ajuste.viv_97_aivs_dif <= 0 AND lr_ajuste.viv_92_aivs_dif <= 0 THEN  #Se liquida el saldo total de la cuenta aunque no se cubra el importe recibido
		        	  LET lr_ajuste.viv_97_aivs  = lr_ajuste.viv_97_aivs_cta
		            LET lr_ajuste.viv_97_pesos = lr_ajuste.viv_97_pesos_cta
		            LET lr_ajuste.viv_92_aivs  = lr_ajuste.viv_92_aivs_cta
		            LET lr_ajuste.viv_92_pesos = lr_ajuste.viv_92_pesos_cta
		        ELSE
					      IF lr_ajuste.viv_97_aivs_dif < 0 AND lr_ajuste.viv_92_aivs_cta > 0 AND lr_ajuste.viv_92_aivs = 0 THEN  #Cuando se agota el saldo de la Viv97 se toma de la Viv92
					            LET lr_ajuste.viv_97_aivs  = lr_ajuste.viv_97_aivs_cta
					            LET lr_ajuste.viv_97_pesos = lr_ajuste.viv_97_pesos_cta
					            LET lr_ajuste.viv_92_aivs  = lr_ajuste.viv_97_aivs_dif * -1
					            
					            IF lr_ajuste.viv_92_aivs <= lr_ajuste.viv_92_aivs_cta THEN
					                LET lr_ajuste.viv_92_pesos = lr_ajuste.viv_92_aivs * gd_precio_viv
					            ELSE
					                LET lr_ajuste.viv_92_aivs = lr_ajuste.viv_92_aivs_cta
					                LET lr_ajuste.viv_92_pesos = lr_ajuste.viv_92_aivs * gd_precio_viv
					            END IF 
					            
					            LET lr_ajuste.viv_92_aivs_dif   = lr_ajuste.viv_92_aivs_cta - lr_ajuste.viv_92_aivs
					            LET lr_ajuste.viv_92_pesos_dif  = lr_ajuste.viv_92_pesos_cta - lr_ajuste.viv_92_pesos
					      ELSE
					            IF lr_ajuste.viv_97_aivs_dif < 0 AND lr_ajuste.viv_92_aivs_cta <= 0 AND lr_ajuste.viv_92_aivs = 0 THEN #Se agota el saldo de vivienda, pero no hay saldo en Viv92
					                LET lr_ajuste.viv_97_aivs  = lr_ajuste.viv_97_aivs_cta
					                LET lr_ajuste.viv_97_pesos = lr_ajuste.viv_97_pesos_cta
					                LET lr_ajuste.viv_92_aivs       = 0
					                LET lr_ajuste.viv_92_pesos      = 0
					                LET lr_ajuste.viv_92_aivs_dif   = 0
					                LET lr_ajuste.viv_92_pesos_dif  = 0
					            ELSE
					            	  IF lr_ajuste.viv_97_aivs_dif < 0 AND lr_ajuste.viv_92_aivs > 0 AND lr_ajuste.viv_92_aivs_cta > 0 THEN  #Se agota el saldo de Viv97 y hay sobrante en la Viv92
					            	       IF lr_ajuste.viv_92_aivs_dif > 0 THEN
					            	       	    LET lr_ajuste.viv_97_aivs  = lr_ajuste.viv_97_aivs_cta
					                          LET lr_ajuste.viv_97_pesos = lr_ajuste.viv_97_pesos_cta
					            	            LET lr_ajuste.viv_92_aivs = lr_ajuste.viv_92_aivs + (lr_ajuste.viv_97_aivs_dif * -1)

					            	            IF lr_ajuste.viv_92_aivs > lr_ajuste.viv_92_aivs_cta THEN
					            	            	  LET lr_ajuste.viv_92_aivs = lr_ajuste.viv_92_aivs_cta
					                              LET lr_ajuste.viv_92_pesos = lr_ajuste.viv_92_aivs * gd_precio_viv
					                          ELSE
					                              LET lr_ajuste.viv_92_pesos = lr_ajuste.viv_92_aivs * gd_precio_viv
					            	            END IF
					            	       END IF
					            	  ELSE
													    IF lr_ajuste.viv_92_aivs_dif < 0 AND lr_ajuste.viv_97_aivs_cta <= 0 AND lr_ajuste.viv_97_aivs = 0 THEN #Se agota el saldo de vivienda 92, pero no hay saldo en Viv97
													        LET lr_ajuste.viv_92_aivs  = lr_ajuste.viv_92_aivs_cta
													        LET lr_ajuste.viv_92_pesos = lr_ajuste.viv_92_pesos_cta
													        LET lr_ajuste.viv_97_aivs       = 0
													        LET lr_ajuste.viv_97_pesos      = 0
													        LET lr_ajuste.viv_97_aivs_dif   = 0
													        LET lr_ajuste.viv_97_pesos_dif  = 0
													    ELSE
													    	  IF lr_ajuste.viv_92_aivs_dif < 0 AND lr_ajuste.viv_97_aivs > 0 AND lr_ajuste.viv_97_aivs_cta > 0 THEN  #Se agota el saldo de Viv92 y hay sobrante en la Viv97
													    	       IF lr_ajuste.viv_97_aivs_dif > 0 THEN
                          	                LET lr_ajuste.viv_92_aivs  = lr_ajuste.viv_92_aivs_cta
													                  LET lr_ajuste.viv_92_pesos = lr_ajuste.viv_92_pesos_cta
													    	            LET lr_ajuste.viv_97_aivs = lr_ajuste.viv_97_aivs + (lr_ajuste.viv_92_aivs_dif * -1)

													    	            IF lr_ajuste.viv_97_aivs > lr_ajuste.viv_97_aivs_cta THEN
													    	            	  LET lr_ajuste.viv_97_aivs = lr_ajuste.viv_97_aivs_cta
													                      LET lr_ajuste.viv_97_pesos = lr_ajuste.viv_97_aivs * gd_precio_viv
													                  ELSE
													                      LET lr_ajuste.viv_97_pesos = lr_ajuste.viv_97_aivs * gd_precio_viv
													    	            END IF
													    	       END IF
													    	  END IF
													    END IF
					            	  END IF
					            END IF
					      END IF
		        END IF
        END IF

        UPDATE ret_ajuste_vivienda_nss
        SET    viv_97_aivs          = lr_ajuste.viv_97_aivs         ,
               viv_92_aivs          = lr_ajuste.viv_92_aivs         ,
               viv_92_aivs_cta      = lr_ajuste.viv_92_aivs_cta     ,
               viv_92_aivs_dif      = lr_ajuste.viv_92_aivs_dif     ,
               viv_97_aivs_cta      = lr_ajuste.viv_97_aivs_cta     ,
               viv_97_aivs_dif      = lr_ajuste.viv_97_aivs_dif     ,
               viv_97_pesos         = lr_ajuste.viv_97_pesos        ,
               viv_92_pesos         = lr_ajuste.viv_92_pesos        ,
               viv_92_pesos_cta     = lr_ajuste.viv_92_pesos_cta    ,
               viv_92_pesos_dif     = lr_ajuste.viv_92_pesos_dif    ,
               viv_97_pesos_cta     = lr_ajuste.viv_97_pesos_cta    ,
               viv_97_pesos_dif     = lr_ajuste.viv_97_pesos_dif
        WHERE  folio                = lr_ajuste.folio
        AND    nss                  = lr_ajuste.nss
        AND    consecutivo          = lr_ajuste.consecutivo
        AND    estado               = gr_edo.capturado

        INITIALIZE lr_ajuste.* TO NULL
        INITIALIZE lr_saldo.* TO NULL

    END FOREACH

    DISPLAY "(TERMINADO)" AT 7,40

END FUNCTION

#---------------------------------------------------------------------------#
# f_calcula_interes : Realiza el calculo del ajuste de intereses de las     #
#                     subcuentas de vivienda de acuerdo a los datos         #
#                     cargados. Una vez calculado, se inserta la provision  #
#                     temporal en caso de ser necesario                     #
#---------------------------------------------------------------------------#
FUNCTION f_calcula_interes(pi_folio)

    DEFINE
        pi_folio            INTEGER

    DEFINE lr_notifica  RECORD LIKE ret_notifica_vivienda.*

    -- -----------------------------------------------------------------------------

    DISPLAY "CALCULANDO INTERESES DE VIVIENDA ...." AT 8,2

    INITIALIZE lr_notifica.* TO NULL

    DECLARE cur_ajuste CURSOR FOR
        SELECT *
        FROM   ret_notifica_vivienda
        WHERE  folio_carga  = pi_folio
        AND    estado       = gr_edo.capturado
        ORDER BY nss, consecutivo

    FOREACH cur_ajuste INTO lr_notifica.*

        -- Calcula el interes para la vivienda 92
        CALL f_inserta_interes(pi_folio                     ,
                               lr_notifica.nss              ,
                               lr_notifica.curp             ,
                               lr_notifica.consecutivo      ,
                               lr_notifica.diag_infonavit   ,
                               92
                              )

        -- Calcula el interes para la vivienda 97
        CALL f_inserta_interes(pi_folio                     ,
                               lr_notifica.nss              ,
                               lr_notifica.curp             ,
                               lr_notifica.consecutivo      ,
                               lr_notifica.diag_infonavit   ,
                               97
                              )

        INITIALIZE lr_notifica.* TO NULL

    END FOREACH

    DISPLAY "(TERMINADO)" AT 8,40

END FUNCTION

#---------------------------------------------------------------------------#
# f_inserta_interes : Calcula e inserta el interes para el tipo de vivienda #
#                     dado                                                  #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_interes(pr_datos_sol)

    DEFINE pr_datos_sol RECORD
        folio                   LIKE ret_notifica_vivienda.folio_carga      ,
        nss                     LIKE ret_notifica_vivienda.nss              ,
        curp                    LIKE ret_notifica_vivienda.curp             ,
        consecutivo             LIKE ret_notifica_vivienda.consecutivo      ,
        diag_infonavit          LIKE ret_notifica_vivienda.diag_infonavit   ,
        tipo_vivienda           SMALLINT
    END RECORD

    DEFINE lr_montos RECORD
        pesos           LIKE ret_ajuste_vivienda_nss.viv_97_pesos   ,
        aivs            LIKE ret_ajuste_vivienda_nss.viv_97_aivs    ,
        diff            LIKE ret_ajuste_vivienda_nss.viv_97_aivs_dif,
        subcuenta       SMALLINT
    END RECORD

    DEFINE lr_ajuste    RECORD LIKE ret_ajuste_vivienda_nss.*
    DEFINE lr_interes   RECORD LIKE ret_int_vivienda.*

    DEFINE ld_pesos_ajuste LIKE ret_ajuste_vivienda_nss.viv_97_pesos

    -- -----------------------------------------------------------------------------

    INITIALIZE
        lr_interes.*        ,
        lr_ajuste.*         TO NULL

    LET lr_montos.pesos = 0
    LET lr_montos.aivs  = 0
    LET lr_montos.diff  = 0

    SELECT *
    INTO   lr_ajuste.*
    FROM   ret_ajuste_vivienda_nss
    WHERE  folio        = pr_datos_sol.folio
    AND    estado       = gr_edo.capturado
    AND    nss          = pr_datos_sol.nss
    AND    consecutivo  = pr_datos_sol.consecutivo

    LET lr_interes.folio            = lr_ajuste.folio
    LET lr_interes.nss              = lr_ajuste.nss
    LET lr_interes.consecutivo      = lr_ajuste.consecutivo
    LET lr_interes.tipo_vivienda    = pr_datos_sol.tipo_vivienda

    -- Seleccionamos los montos que se usaran dependiendo del tipo de vivienda
    IF (pr_datos_sol.tipo_vivienda = 92) THEN
        LET lr_montos.pesos     = lr_ajuste.viv_92_pesos
        LET lr_montos.aivs      = lr_ajuste.viv_92_aivs
        LET lr_montos.diff      = lr_ajuste.viv_92_aivs_dif
        
        IF lr_ajuste.viv_92_aivs_cta = 0 THEN
             LET lr_montos.pesos     = 0
             LET lr_montos.aivs      = 0
             LET lr_montos.diff      = 0
        END IF
        
        LET lr_montos.subcuenta = 8
    ELSE
        LET lr_montos.pesos     = lr_ajuste.viv_97_pesos
        LET lr_montos.aivs      = lr_ajuste.viv_97_aivs
        LET lr_montos.diff      = lr_ajuste.viv_97_aivs_dif
        
        IF lr_ajuste.viv_97_aivs_cta = 0 THEN
             LET lr_montos.pesos     = 0
             LET lr_montos.aivs      = 0
             LET lr_montos.diff      = 0
        END IF
        
        LET lr_montos.subcuenta = 4
    END IF
#CPL-2352 INI
    {IF(lr_montos.diff < 0) THEN
        LET lr_montos.pesos     = 0
        LET lr_montos.aivs      = 0
    END IF}
#CPL-2352 FIN
    LET ld_pesos_ajuste = lr_montos.aivs * gd_precio_viv
    
    IF (pr_datos_sol.diag_infonavit = "101") THEN
    	  
    	  IF (ld_pesos_ajuste - lr_montos.pesos) < 0 THEN
    	  	   LET lr_interes.interes_pesos    = 0
             LET lr_interes.interes_aivs     = 0
             
             LET lr_interes.retiro_pesos     = lr_montos.pesos * -1
             LET lr_interes.retiro_aivs      = lr_montos.aivs * -1
    	  ELSE
    	       LET lr_interes.interes_pesos    = (ld_pesos_ajuste - lr_montos.pesos) * -1
             LET lr_interes.interes_aivs     = lr_interes.interes_pesos/gd_precio_viv
             
             LET lr_interes.retiro_pesos     = (lr_interes.interes_pesos + ld_pesos_ajuste) * -1
             LET lr_interes.retiro_aivs      = (lr_montos.aivs + lr_interes.interes_aivs) * -1
    	  END IF
    ELSE
        LET lr_interes.interes_pesos    = 0
        LET lr_interes.interes_aivs     = 0
        LET lr_interes.retiro_pesos     = 0
        LET lr_interes.retiro_aivs      = 0
    END IF

    INSERT INTO tmp_int_vivienda
    VALUES (lr_interes.*)

    -- Se inserta el movimiento de intereses de vivienda en caso de que exista
    IF (lr_interes.interes_aivs <> 0) THEN
        CALL f_provisiona_mov(lr_interes.folio              ,
                              lr_interes.nss                ,
                              pr_datos_sol.curp             ,
                              gr_movimiento.cancela_int     ,
                              lr_interes.consecutivo        ,
                              lr_montos.subcuenta           ,
                              lr_interes.interes_pesos      ,
                              lr_interes.interes_aivs
                             )
    END IF

    -- Se inserta el movimiento de retiro en caso de que exista
    IF (lr_interes.retiro_aivs <> 0) THEN
        CALL f_provisiona_mov(lr_interes.folio          ,
                              lr_interes.nss            ,
                              pr_datos_sol.curp         ,
                              gr_movimiento.retiro      ,
                              lr_interes.consecutivo    ,
                              lr_montos.subcuenta       ,
                              lr_interes.retiro_pesos   ,
                              lr_interes.retiro_aivs
                             )
    END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_inserta_registros : Consolida en las tablas fisicas los registros que   #
#                       se almacenaron en tablas temporales y actualiza     #
#                       estados                                             #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_registros(pi_folio)

    DEFINE
        pi_folio        INTEGER

    DEFINE lr_solicitud RECORD
        nss             LIKE ret_solicitud_tx.nss           ,
        consecutivo     LIKE ret_solicitud_tx.consecutivo
    END RECORD

    -- -----------------------------------------------------------------------------

    DISPLAY "CONSOLIDANDO TABLAS FISICAS ...." AT 9,2

    -- Copiamos la provision de la tabla temporal a la definitiva
    INSERT INTO dis_provision
    SELECT *
    FROM   tmp_provision
    WHERE  folio = pi_folio

    -- Copiamos la provision de la tabla temporal a dis_cuenta
    INSERT INTO dis_cuenta
    SELECT *
    FROM   tmp_provision
    WHERE  folio = pi_folio

    -- Copiamos los calculos de intereses de la tabla temporal a la definitiva
    INSERT INTO ret_int_vivienda
    SELECT *
    FROM   tmp_int_vivienda
    WHERE  folio = pi_folio

    DECLARE cur_act_sol CURSOR FOR
        SELECT UNIQUE(nss)      ,
               consecutivo_lote
        FROM   dis_cuenta
        WHERE  folio    = pi_folio
        ORDER BY 1,2

    FOREACH cur_act_sol INTO lr_solicitud.*

        -- Actualiza el folio y el estado de la solicitud de los registros provisionados
        UPDATE ret_ajuste_vivienda_nss
        SET    estado       = gr_edo.liquidado
        WHERE  folio        = pi_folio
        AND    nss          = lr_solicitud.nss
        AND    consecutivo  = lr_solicitud.consecutivo
        AND    estado       = gr_edo.capturado

        UPDATE ret_notifica_vivienda
        SET    estado       = gr_edo.liquidado
        WHERE  folio_carga  = pi_folio
        AND    nss          = lr_solicitud.nss
        AND    consecutivo  = lr_solicitud.consecutivo
        AND    estado       = gr_edo.capturado

    END FOREACH

    -- Rechaza los registros que no tuvieron una solicitud previa
    UPDATE ret_ajuste_vivienda_nss
    SET    estado   = gr_edo.rechazado
    WHERE  folio    = pi_folio
    AND    estado   = gr_edo.capturado

    UPDATE ret_notifica_vivienda
    SET    estado       = gr_edo.rechazado
    WHERE  folio_carga  = pi_folio
    AND    estado       = gr_edo.capturado

    DISPLAY "(TERMINADO)" AT 9,40

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_archivo_txt : Genera el archivo que se reporta a PROCESAR con    #
#                        los ajustes de intereses de vivienda               #
#---------------------------------------------------------------------------#
FUNCTION f_genera_archivo_txt(pi_folio)

    DEFINE
        pi_folio                INTEGER

    DEFINE lr_interes   RECORD LIKE ret_int_vivienda.*

    DEFINE
        lc_nombre_rep           CHAR(100)   ,
        lc_ruta_reporte         CHAR(200)

    -- -----------------------------------------------------------------------------

    LET lc_nombre_rep   = gc_usuario CLIPPED            ,
                          "_CARGO_CI_VIV_"              ,
                          HOY USING "YYYYMMDD"          ,
                          ".txt"

    LET lc_ruta_reporte = gr_modulo.ruta_envio CLIPPED  ,
                          "/"                           ,
                          lc_nombre_rep CLIPPED


    INITIALIZE lr_interes.* TO NULL

    START REPORT rpt_interes_txt TO lc_ruta_reporte

    DECLARE cur_formato_txt CURSOR FOR
        SELECT *
        FROM   ret_int_vivienda
        WHERE  folio        = pi_folio
        ORDER BY consecutivo        ,
                 interes_aivs DESC

    FOREACH cur_formato_txt INTO lr_interes.*

        OUTPUT TO REPORT rpt_interes_txt(lr_interes.*)

        INITIALIZE lr_interes.* TO NULL

    END FOREACH

    FINISH REPORT rpt_interes_txt

    CALL f_lib_borra_lineas(gr_modulo.ruta_envio, lc_nombre_rep)

    DISPLAY "SE GENERO EL ARCHIVO DE CANCELACION DE INTERESES CON EL NOMBRE:" AT 16,2
    DISPLAY lc_ruta_reporte AT 17,5

END FUNCTION

#---------------------------------------------------------------------------#
# f_marca_cuenta : Ejecuta el script para realizar la marca de la cuenta    #
#---------------------------------------------------------------------------#
FUNCTION f_marca_cuenta(pr_marca)

    DEFINE pr_marca RECORD
        nss         LIKE ret_parcial.nss          ,
        consec      LIKE ret_parcial.consecutivo
    END RECORD

    DEFINE lr_dat RECORD
        edo_marca       SMALLINT ,
        cod_rechazo     SMALLINT ,
        marca_causa     SMALLINT ,
        fec_causa       DATE
    END RECORD

    DEFINE
        ls_tipo_pres        ,
        ls_marca_res        ,
        ls_cod_rech         ,
        ls_movim            SMALLINT

    DEFINE
        lc_prepare          CHAR(100)   ,
        lc_desc_rechazo     CHAR(50)    ,
        lc_usuario          CHAR(15)

    -- ---------------------------------------------------------------------------------

    ----- MARCAJE DE CUENTA -----
    LET lc_prepare = "EXECUTE PROCEDURE marca_cuenta(?,?,?,?,?,?,?,?)"
    PREPARE eje_marca FROM lc_prepare

    LET lc_prepare = " "

    LET lc_usuario          = gc_usuario
    LET lc_desc_rechazo     = " "
    LET lr_dat.edo_marca    = 0
    LET lr_dat.cod_rechazo  = 0
    LET lr_dat.marca_causa  = 0
    INITIALIZE lr_dat.fec_causa TO NULL

    LET ls_movim = 830

    --MARCAJE--
    EXECUTE eje_marca USING pr_marca.nss            ,--nss
                            ls_movim                ,--marca entrante
                            pr_marca.consec         ,--consecutivo
                            lr_dat.edo_marca        ,--estado_marco
                            lr_dat.cod_rechazo      ,--codigo de rechazo
                            lr_dat.marca_causa      ,--marca_causa
                            lr_dat.fec_causa        ,--fecha_causa
                            lc_usuario               --usuario
                      INTO  ls_marca_res   ,
                            ls_cod_rech

END FUNCTION

#---------------------------------------------------------------------------#
# f_provisiona_mov : Inserta el movimiento de provision de acuerdo a los    #
#                    parametros dados                                       #
#---------------------------------------------------------------------------#
FUNCTION f_provisiona_mov(pr_provision)

    DEFINE pr_provision RECORD
        folio               LIKE dis_provision.folio                ,
        nss                 LIKE dis_provision.nss                  ,
        curp                LIKE dis_provision.curp                 ,
        tipo_movimiento     LIKE dis_provision.tipo_movimiento      ,
        consecutivo         LIKE dis_provision.consecutivo_lote     ,
        subcuenta           LIKE dis_provision.subcuenta            ,
        monto_en_pesos      LIKE dis_provision.monto_en_pesos       ,
        monto_en_acciones   LIKE dis_provision.monto_en_acciones
    END RECORD

    DEFINE lr_provision RECORD LIKE dis_provision.*

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_provision.* TO NULL

    LET lr_provision.tipo_movimiento    = pr_provision.tipo_movimiento
    LET lr_provision.subcuenta          = pr_provision.subcuenta
    LET lr_provision.siefore            = 11
    LET lr_provision.folio              = pr_provision.folio
    LET lr_provision.consecutivo_lote   = pr_provision.consecutivo
    LET lr_provision.nss                = pr_provision.nss
    LET lr_provision.curp               = pr_provision.curp
    LET lr_provision.fecha_pago         = HOY
    LET lr_provision.fecha_valor        = gdt_fecha_viv
    LET lr_provision.fecha_conversion   = HOY
    LET lr_provision.precio_accion      = gd_precio_viv
    LET lr_provision.dias_cotizados     = 0
    LET lr_provision.id_aportante       = "CANINT"
    LET lr_provision.estado             = 6
    LET lr_provision.fecha_proceso      = HOY
    LET lr_provision.usuario            = gc_usuario
    LET lr_provision.fecha_archivo      = HOY
    LET lr_provision.etiqueta           = 1

    IF lr_provision.siefore = 11 THEN
        LET lr_provision.monto_en_pesos     = f_lib_redondea_val(pr_provision.monto_en_pesos, 2)
        --LET lr_provision.monto_en_acciones  = f_lib_redondea_val(pr_provision.monto_en_acciones, 2)
        LET lr_provision.monto_en_acciones  = pr_provision.monto_en_acciones
    END IF

    INSERT INTO tmp_provision
    VALUES (lr_provision.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_saldo_dia : Obtiene para un nss el saldo al dia de una subcuenta  #
#                     en particular para una fecha                          #
#---------------------------------------------------------------------------#
FUNCTION f_obten_saldo_dia(pr_saldo)

    DEFINE pr_saldo RECORD
        nss             LIKE dis_cuenta.nss         ,
        subcuenta       LIKE dis_cuenta.subcuenta   ,
        grupo           SMALLINT                    ,
        fecha_saldo     DATE
    END RECORD

    DEFINE lr_saldo_dia RECORD
        subcuenta   LIKE dis_provision.subcuenta        ,
        siefore     LIKE dis_provision.siefore          ,
        monto_acc   LIKE dis_provision.monto_en_acciones,
        monto_pes   LIKE dis_provision.monto_en_pesos
    END RECORD

    DEFINE
        lc_prepare      CHAR(400)

    -- -----------------------------------------------------------------------------

    LET lc_prepare                  = " "
    LET lr_saldo_dia.subcuenta      = 0
    LET lr_saldo_dia.siefore        = 0
    LET lr_saldo_dia.monto_acc      = 0
    LET lr_saldo_dia.monto_pes      = 0

    ----- SALDO AL DIA -----
    LET lc_prepare = " EXECUTE FUNCTION fn_saldo_dia(?,?,?,?) "
    PREPARE prp_saldo_dia FROM lc_prepare
    DECLARE cur_saldo_dia CURSOR FOR prp_saldo_dia

    OPEN  cur_saldo_dia USING pr_saldo.*
    FETCH cur_saldo_dia INTO  lr_saldo_dia.*
    CLOSE cur_saldo_dia

    IF lr_saldo_dia.monto_acc IS NULL THEN
        LET lr_saldo_dia.monto_acc = 0
    END IF

    IF lr_saldo_dia.monto_pes IS NULL THEN
        LET lr_saldo_dia.monto_pes = 0
    END IF

    RETURN lr_saldo_dia.monto_acc   ,
           lr_saldo_dia.monto_pes

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran los resultados de la   #
#                  provision de dispocisiones                               #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW retp0111 AT 4,4 WITH FORM "RETP0111" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                        < ESC > Ejecutar      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETP011               CALCULO DE INTERESES DE VIVIENDA                        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_tmp : Crea las tablas temporales que se usaran en el proceso     #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_envio
        DROP TABLE tmp_int_vivienda
        DROP TABLE tmp_provision
        DROP TABLE tmp_solicitud_tx
        DROP TABLE tmp_beneficiario
    WHENEVER ERROR STOP

    -- -----------------------------------------------------

    SELECT *
    FROM   ret_envio
    WHERE  1 = 0
    INTO TEMP tmp_envio

    -- -----------------------------------------------------

    SELECT *
    FROM   ret_int_vivienda
    WHERE  1 = 0
    INTO TEMP tmp_int_vivienda

    -- -----------------------------------------------------

    SELECT *
    FROM   dis_provision
    WHERE  1 = 0
    INTO TEMP tmp_provision

    -- -----------------------------------------------------

    SELECT *
    FROM   ret_solicitud_tx
    WHERE  1 = 0
    INTO TEMP tmp_solicitud_tx

    -- -----------------------------------------------------

    SELECT *
    FROM   ret_beneficiario
    WHERE  1 = 0
    INTO TEMP tmp_beneficiario

    -- -----------------------------------------------------

END FUNCTION



#---------------------------------------------------------------------------#
# rpt_canint : Genera el archivo plano del archivo de cancelacion           #
#---------------------------------------------------------------------------#
REPORT rpt_interes_txt(pr_datos_txt)

    DEFINE pr_datos_txt RECORD LIKE ret_int_vivienda.*

    DEFINE
        lr_cadena_AIVS          CHAR(15)    ,
        lc_cad_16               CHAR(16)    ,
        lc_cancela              CHAR(18)    ,
        lc_movimiento           CHAR(02)

    DEFINE
        ls_cont                 SMALLINT

    -- -----------------------------------------------------------------------------

    OUTPUT
      PAGE LENGTH   10000
    	LEFT MARGIN   0
	    RIGHT MARGIN  0
	    TOP MARGIN    0
	    BOTTOM MARGIN 0

    FORMAT
        -- ENCABEZADO
        FIRST PAGE HEADER
            LET ls_cont = 0

            PRINT
                COLUMN 001, "01"                                    , -- Tipo registro
                COLUMN 003, "04"                                    , -- Id de servicio
                COLUMN 005, "62"                                    , -- Id operacion
                COLUMN 007, "01"                                    , -- Tipo entidad origen
                COLUMN 009, gs_codigo_afore USING "&&&"             , -- Clave entidad origen
                COLUMN 012, "03"                                    , -- Tipo entidad destino
                COLUMN 014, "001"                                   , -- Clave entidad destino
                COLUMN 017, HOY USING "YYYYMMDD"                    , -- Fecha valor
                COLUMN 025, 36 SPACES

        -- DETALLE
        ON EVERY ROW
            LET ls_cont = ls_cont + 1

            -- Formateamos los montos pagados a cadenas
            LET lc_cad_16   = " "

            IF pr_datos_txt.interes_aivs IS NULL THEN
                LET pr_datos_txt.interes_aivs   = 0
            END IF

            LET lc_cad_16       = pr_datos_txt.interes_aivs USING "&&&&&&&&&.&&&&&&"
            LET lr_cadena_AIVS  = lc_cad_16[01,09]  ,
                                  lc_cad_16[11,16]

            LET lc_cancela  = "Cancelacioninteres"

            IF (pr_datos_txt.tipo_vivienda = 97) THEN
                LET lc_movimiento   = "32"
            ELSE
                LET lc_movimiento   = "33"
            END IF

            PRINT
                COLUMN 001, "03"                                    , -- Tipo registro
                COLUMN 003, "04"                                    , -- Id de servicio
                COLUMN 005, "62"                                    , -- Id operacion
                COLUMN 007, pr_datos_txt.nss                        ,
                COLUMN 018, lc_cancela                              ,
                COLUMN 036, HOY USING "YYYYMMDD"                    ,
                COLUMN 044, lc_movimiento USING "&&"                ,
                COLUMN 046, lr_cadena_AIVS

        -- SUMARIO
        ON LAST ROW

            PRINT
                COLUMN 001, "09"                                ,-- tipo de registro
                COLUMN 003, "04"                                ,-- identificador de servicio
                COLUMN 005, "62"                                ,-- ID operacion
                COLUMN 007, "01"                                ,-- tipo entidad origen
                COLUMN 009, gs_codigo_afore USING "&&&"         ,-- clave entidad origen
                COLUMN 012, "03"                                ,-- tipo entidad destino
                COLUMN 014, "001"                               ,-- clave entidad destino
                COLUMN 017, HOY USING "YYYYMMDD"                ,-- fecha de operacion
                COLUMN 025, ls_cont USING "&&&&&&"              ,-- numero de registros
                COLUMN 031, 30 SPACES

END REPORT

