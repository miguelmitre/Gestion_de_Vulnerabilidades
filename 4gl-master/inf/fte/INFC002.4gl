#################################################################################
#Owner             => E.F.P.                                                    #
#Proyecto          => SISTEMA DE AFORES( MEXICO )                               #
#Programa INFC002  => LIQUIDACION DE OPERACION 98                               #
#                                                                               #
#Fecha creacion    => 3 DE JUNIO DE 2003                                        #
#By                => JOSE LUIS SALDIVAR CARDOSO                                #
#Fecha actualiz.   => 20 DE JUNIO DE 2013                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => - Se mejora el codigo del programa                        #
#                     - Se modifica para que liquide los recursos de acuerdo a  #
#                       lo que se almacena en inf_his_oper97 (CPL-1320)         #
#                                                                               #
#Sistema           => INF                                                       #
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gr_captura RECORD
        procesa         SMALLINT                    ,
	    folio           LIKE inf_his_oper97.folio
    END RECORD

    DEFINE gr_edo RECORD
        procesado       LIKE ret_estado.estado_solicitud ,
        liquidado       LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE
        enter                   CHAR(001)   ,
        gc_usuario              CHAR(020)

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("INFC002")
    CALL init()

    CALL f_abre_ventana()
    CALL f_captura_folio() RETURNING gr_captura.*

    IF gr_captura.procesa = 1 THEN 
        CALL primer_paso(gr_captura.folio)
        CALL f_lib_error_msg("PROCESO FINALIZADO CORRECTAMENTE")
    END IF

	CLOSE WINDOW infc0021

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    -- -----------------------------------------------------------------------------

    LET HOY             = TODAY
    LET gc_usuario      = f_lib_obten_user()

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.procesado
    FROM   ret_estado A
    WHERE  descripcion = "PROCESADO"

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Realiza la liquidacion de recursos                          #
#---------------------------------------------------------------------------#
FUNCTION primer_paso(pi_folio)

    DEFINE pi_folio LIKE inf_his_oper97.folio

    DEFINE lr_his_op97 RECORD LIKE inf_his_oper97.*

    DEFINE lr_datos_liq RECORD
        tipo_movimiento     LIKE dis_cuenta.tipo_movimiento     ,
        subcuenta           LIKE dis_cuenta.subcuenta           ,
        folio               LIKE dis_cuenta.folio               ,
        nss                 LIKE dis_cuenta.nss                 ,
        curp                LIKE dis_cuenta.curp                ,
        fecha_valor         LIKE dis_cuenta.fecha_valor         ,
        pesos               LIKE dis_cuenta.monto_en_pesos      ,
        acciones            LIKE dis_cuenta.monto_en_acciones
    END RECORD

    DEFINE lr_movimiento RECORD
        cargo       SMALLINT    ,
        abono       SMALLINT
    END RECORD 

    -- -----------------------------------------------------------------------------

    DECLARE cur_op97 CURSOR FOR
        SELECT *
        FROM   inf_his_oper97
        WHERE  folio    = pi_folio
        ORDER BY nss

    FOREACH cur_op97 INTO lr_his_op97.*

        INITIALIZE lr_datos_liq.*  TO NULL 
        INITIALIZE lr_movimiento.* TO NULL

        LET lr_datos_liq.folio          = pi_folio
        LET lr_datos_liq.nss            = lr_his_op97.nss
        LET lr_datos_liq.curp           = lr_his_op97.n_unico
        LET lr_datos_liq.fecha_valor    = lr_his_op97.fecha_valor_devol

        CASE lr_his_op97.opera_devolucion 
	    
	        WHEN "97"
	            LET lr_movimiento.cargo     = 565
	            LET lr_movimiento.abono     = 560
	        
	        WHEN "98"
	            LET lr_movimiento.cargo     = 575
	            LET lr_movimiento.abono     = 570
        
        END CASE

        IF lr_his_op97.tipo_viv = "92" THEN
            LET lr_datos_liq.subcuenta  = 8
        ELSE
            LET lr_datos_liq.subcuenta  = 4
        END IF 

        -- ABONO
        LET lr_datos_liq.tipo_movimiento    = lr_movimiento.abono
        LET lr_datos_liq.pesos              = lr_his_op97.mto_pesos_dev      
        LET lr_datos_liq.acciones           = lr_his_op97.mto_parti_dev
        
        CALL f_inserta_liquidacion(lr_datos_liq.*)
            
        IF lr_his_op97.cargo_abono = "S" THEN
            -- CARGO
            LET lr_datos_liq.tipo_movimiento    = lr_movimiento.cargo
            LET lr_datos_liq.pesos              = lr_his_op97.mto_pesos_dev * -1      
            LET lr_datos_liq.acciones           = lr_his_op97.mto_parti_dev * -1
            
            CALL f_inserta_liquidacion(lr_datos_liq.*)
        END IF

    END FOREACH

    UPDATE inf_his_oper97
    SET    estado       = gr_edo.liquidado
    WHERE  folio        = gr_captura.folio
    AND    estado       = gr_edo.procesado

END FUNCTION

#-------------------------------------------------------------------------------#
# f_captura_folio : Captura el folio de la liquidacion                          #
#-------------------------------------------------------------------------------#
FUNCTION f_captura_folio()

    DEFINE lr_captura RECORD
        procesa             SMALLINT    ,
	    folio               INTEGER
    END RECORD

    -- -----------------------------------------------------------------------------
    
    LET lr_captura.procesa = 1

    SELECT MAX(folio)
    INTO   lr_captura.folio
    FROM   inf_his_oper97
    WHERE  estado = gr_edo.procesado

    INPUT BY NAME lr_captura.folio WITHOUT DEFAULTS
        
        AFTER FIELD folio
            IF lr_captura.folio IS NULL THEN
                CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO")
                NEXT FIELD folio
            ELSE
                SELECT "OK"
                FROM   inf_his_oper97
                WHERE  folio    = lr_captura.folio
		        GROUP BY 1

		        IF STATUS <> NOTFOUND THEN
                    SELECT "OK"
                    FROM   inf_his_oper97
                    WHERE  folio    = lr_captura.folio
                    AND    estado   = gr_edo.liquidado
		            GROUP BY 1

			        IF STATUS <> NOTFOUND THEN
			            CALL f_lib_error_msg("EL FOLIO YA FUE LIQUIDADO")
			            NEXT FIELD folio
			        END IF
                ELSE
			        CALL f_lib_error_msg("EL FOLIO CAPTURADO NO EXISTE")
                END IF
            END IF 
             
        ON KEY (ESC)
            IF lr_captura.folio IS NULL THEN
                CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO")
                NEXT FIELD folio
            ELSE
                SELECT "OK"
                FROM   inf_his_oper97
                WHERE  folio    = lr_captura.folio
		        GROUP BY 1

		        IF STATUS <> NOTFOUND THEN
                    SELECT "OK"
                    FROM   inf_his_oper97
                    WHERE  folio    = lr_captura.folio
                    AND    estado   = gr_edo.liquidado
		            GROUP BY 1

			        IF STATUS <> NOTFOUND THEN
			            CALL f_lib_error_msg("EL FOLIO YA FUE LIQUIDADO")
			            NEXT FIELD folio
			        END IF
                ELSE
			        CALL f_lib_error_msg("EL FOLIO CAPTURADO NO EXISTE")
                END IF
            END IF 

            WHILE TRUE
                PROMPT "¿DESEA EJECUTAR LA LIQUIDACION? (S/N) : " FOR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        LET lr_captura.procesa = 1
                        EXIT INPUT
                    ELSE
                        CALL f_lib_error_msg("PROCESO CANCELADO")
                        LET lr_captura.procesa = 0
                        EXIT INPUT
                    END IF
                END IF
            END WHILE

        ON KEY (CONTROL-C)
            CALL f_lib_error_msg("PROCESO CANCELADO")
            LET lr_captura.procesa = 0
            EXIT INPUT

        ON KEY (INTERRUPT)
            CALL f_lib_error_msg("PROCESO CANCELADO")
            LET lr_captura.procesa = 0
            EXIT INPUT
    
    END INPUT

    RETURN lr_captura.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestra la captura de folio y   #
#                  los resultados de la liquidacion                         #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW infc0021 AT 4,4 WITH FORM "INFC0021" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-C> - Salir                                          <ESC> - Ejecutar    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " INFC002         LIQUIDACION DE DEVOLUCION OPERACION 97 - 98                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_inserta_liquidacion : Inserta el movimiento de liquidacion en dis_cuenta#
#---------------------------------------------------------------------------#
FUNCTION f_inserta_liquidacion(pr_liquida)

    DEFINE pr_liquida RECORD
        tipo_movimiento     LIKE dis_cuenta.tipo_movimiento     ,
        subcuenta           LIKE dis_cuenta.subcuenta           ,
        folio               LIKE dis_cuenta.folio               ,
        nss                 LIKE dis_cuenta.nss                 ,
        curp                LIKE dis_cuenta.curp                ,
        fecha_valor         LIKE dis_cuenta.fecha_valor         ,
        monto_en_pesos      LIKE dis_cuenta.monto_en_pesos      ,
        monto_en_acciones   LIKE dis_cuenta.monto_en_acciones
    END RECORD 
    
    DEFINE lr_dis_cuenta RECORD LIKE dis_cuenta.*
        
    -- -----------------------------------------------------------------------------

    INITIALIZE lr_dis_cuenta.* TO NULL 

    LET lr_dis_cuenta.tipo_movimiento       = pr_liquida.tipo_movimiento 
    LET lr_dis_cuenta.subcuenta             = pr_liquida.subcuenta       
    LET lr_dis_cuenta.siefore               = 11
    LET lr_dis_cuenta.folio                 = pr_liquida.folio           
    LET lr_dis_cuenta.consecutivo_lote      = 0
    LET lr_dis_cuenta.nss                   = pr_liquida.nss 
    LET lr_dis_cuenta.curp                  = pr_liquida.curp
    LET lr_dis_cuenta.folio_sua             = " "
    LET lr_dis_cuenta.fecha_pago            = HOY
    LET lr_dis_cuenta.fecha_valor           = pr_liquida.fecha_valor
    LET lr_dis_cuenta.fecha_conversion      = HOY
    LET lr_dis_cuenta.monto_en_pesos        = pr_liquida.monto_en_pesos
    LET lr_dis_cuenta.monto_en_acciones     = pr_liquida.monto_en_acciones
    LET lr_dis_cuenta.precio_accion         = 0
    LET lr_dis_cuenta.dias_cotizados        = 0
    LET lr_dis_cuenta.sucursal              = " "
    LET lr_dis_cuenta.id_aportante          = "DINF"
    LET lr_dis_cuenta.estado                = 8
    LET lr_dis_cuenta.fecha_proceso         = HOY
    LET lr_dis_cuenta.usuario               = gc_usuario
    LET lr_dis_cuenta.fecha_archivo         = HOY
    LET lr_dis_cuenta.etiqueta              = 0

    INSERT INTO dis_cuenta
    VALUES (lr_dis_cuenta.*)

END FUNCTION
