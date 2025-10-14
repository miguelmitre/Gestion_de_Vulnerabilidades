################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
#Programa INFC002  => LIQUIDACION DE OPERACION 98                              #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#                  => FECHA CREACION                                           #
#Fecha actualiz.   => 03 DE JUNIO 2003                                         #
#Actualizacion     => JOSE LUIS SALDIVAR CARDOSO                               #
#Sistema           => INF                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD #glo #reg_1
	folio                 INTEGER
    END RECORD

    DEFINE reg_2 RECORD #glo #reg_2
        procesado             LIKE ret_status.status ,
        liquidado             LIKE ret_status.status
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        c10_usuario           CHAR(10) ,
        enter                 CHAR(01)

    DEFINE #glo #smallint
        s_codigo_afore        SMALLINT
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
	INPUT WRAP         ,
        PROMPT LINE LAST   ,
	ACCEPT KEY CONTROL-A

    CALL init() #i
    OPEN WINDOW infc0021 AT 4,4 WITH FORM "INFC0021" ATTRIBUTE(BORDER)
    DISPLAY "                            < Ctrl-C > Salir                                   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " INFC002         LIQUIDACION DE DEVOLUCION OPERACION 97 - 98                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

        INPUT BY NAME reg_1.folio WITHOUT DEFAULTS
	    AFTER FIELD folio
	        IF reg_1.folio IS NULL THEN
		    ERROR "CAMPO NO PUEDE SER NULO"
		    NEXT FIELD folio
                ELSE
                    SELECT "OK"
                    FROM   inf_his_oper97
                    WHERE  folio = reg_1.folio
		    GROUP BY 1

		    IF STATUS <> NOTFOUND THEN
                        SELECT "OK"
                        FROM   inf_his_oper97
                        WHERE  folio  = reg_1.folio
                        AND    estado = reg_2.liquidado
		        GROUP BY 1

			IF STATUS <> NOTFOUND THEN
			    ERROR"FOLIO YA LIQUIDADO"
			    NEXT FIELD folio
			END IF
                    ELSE
			ERROR"FOLIO INEXISTENTE"
			NEXT FIELD folio
		    END IF
                END IF

            ON KEY (ESC)
		IF esta_seguro() THEN #es
	            CALL primer_paso() #pp

		    PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR" 
		    FOR CHAR enter
		ELSE
	            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR" 
                    FOR CHAR enter
		END IF
	        EXIT PROGRAM

	    ON KEY (INTERRUPT)
	        PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR" FOR CHAR enter
	        EXIT PROGRAM

	END INPUT
	CLOSE WINDOW infc0021
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT A.codigo_afore ,
           USER
    INTO   s_codigo_afore ,
           c10_usuario
    FROM   tab_afore_local A

    SELECT A.status
    INTO   reg_2.liquidado
    FROM   ret_status A
    WHERE  descripcion = "LIQUIDADO"

    SELECT A.status
    INTO   reg_2.procesado
    FROM   ret_status A
    WHERE  descripcion = "PROCESADO"

    SELECT MAX(folio)
    INTO   reg_1.folio
    FROM   inf_his_oper97
    WHERE  estado = reg_2.procesado
END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    DEFINE #loc #l_ret
        l_ret                 RECORD LIKE inf_his_oper97.*

    DECLARE cur_1 CURSOR FOR
    SELECT *
    FROM   inf_his_oper97
    WHERE  folio = reg_1.folio

    FOREACH cur_1 INTO l_ret.*
        CALL actualiza_cuenta_ind(l_ret.*) #aci
    END FOREACH

    UPDATE inf_his_oper97
    SET    estado = reg_2.liquidado
    WHERE  folio      = reg_1.folio
    AND    estado     = reg_2.procesado
END FUNCTION

FUNCTION actualiza_cuenta_ind(l_ret)
#aci--------------------------------
    DEFINE #loc #l_ret
        l_ret                 RECORD LIKE inf_his_oper97.*

    DEFINE #loc #decimal
	d6_monto_en_acc       DECIMAL(16,6),
	d6_monto_en_pesos     DECIMAL(16,6)

    DEFINE #loc #smallint
	movimiento_cargo      ,
	movimiento_abono      SMALLINT

    LET d6_monto_en_pesos = l_ret.mto_pesos_dev 
    LET d6_monto_en_acc   = l_ret.mto_parti_dev

    {
    CASE l_ret.origen_devolucion
	WHEN "06" --Reverso parcial originado por la ICEFA
	WHEN "07" --Reverso parcial originado por la AFORE
	WHEN "08" --Reverso total originado por la AFORE
	WHEN "09" --Reverso total originado por la ICEFA
    END CASE
    }

    LET movimiento_cargo = 0
    LET movimiento_abono = 0

    CASE l_ret.opera_devolucion 
	WHEN "97"
	    LET movimiento_cargo = 565
	    LET movimiento_abono = 560
	WHEN "98"
	    LET movimiento_cargo = 575
	    LET movimiento_abono = 570
    END CASE
    IF l_ret.tipo_viv = "92" THEN
        IF l_ret.cargo_abono = "S"   THEN

            ---CARGO--------------
            INSERT INTO dis_cuenta
                 VALUES(movimiento_cargo        ,#tipo_movimiento
	                8                       ,#subcuenta
	                11                      ,#siefore
	                reg_1.folio             ,#folio
                        000                     ,#consecutivo
	                l_ret.nss               ,#nss
	                NULL                    ,#curp
	                ""                      ,#folio_sua
	                HOY                     ,#fecha_pago
	                l_ret.fecha_valor_devol ,#fecha_valor
	                HOY                     ,#fecha_conversion
	                - d6_monto_en_pesos     ,#monto_en_pesos
	                - d6_monto_en_acc       ,#monto_accion
	                0                       ,#precio_accion
	                0                       ,#dias_cotizados
	                ""                      ,#sucursal
	                "DINF"                  ,#id_aportante
	                8                       ,#estado
	                HOY                     ,#fecha_proceso
	                c10_usuario             ,
	                ""                      ,#fecha_archivo
	                0                        #etiqueta
	                )  
	    ---ABONO-------------
            INSERT INTO dis_cuenta
                 VALUES(movimiento_abono        ,#tipo_movimiento
	                8                       ,#subcuenta
	                11                      ,#siefore
	                reg_1.folio             ,#folio
                        000                     ,#consecutivo
	                l_ret.nss               ,#nss
	                NULL                    ,#curp
	                ""                      ,#folio_sua
	                HOY                     ,#fecha_pago
	                l_ret.fecha_valor_devol ,#fecha_valor
	                HOY                     ,#fecha_conversion
	                d6_monto_en_pesos       ,#monto_en_pesos
	                d6_monto_en_acc         ,#monto_accion
	                0                       ,#precio_accion
	                0                       ,#dias_cotizados
	                ""                      ,#sucursal
	                "DINF"                  ,#id_aportante
	                8                       ,#estado
	                HOY                     ,#fecha_proceso
	                c10_usuario             ,
	                ""                      ,#fecha_archivo
	                0                        #etiqueta
	                )  
        ELSE
	    ---ABONO-------------
            INSERT INTO dis_cuenta
                 VALUES(movimiento_abono        ,#tipo_movimiento
	                8                       ,#subcuenta
	                11                      ,#siefore
	                reg_1.folio             ,#folio
                        000                     ,#consecutivo
	                l_ret.nss               ,#nss
	                NULL                    ,#curp
	                ""                      ,#folio_sua
	                HOY                     ,#fecha_pago
	                l_ret.fecha_valor_devol ,#fecha_valor
	                HOY                     ,#fecha_conversion
	                d6_monto_en_pesos       ,#monto_en_pesos
	                d6_monto_en_acc         ,#monto_accion
	                0                       ,#precio_accion
	                0                       ,#dias_cotizados
	                ""                      ,#sucursal
	                "DINF"                  ,#id_aportante
	                8                       ,#estado
	                HOY                     ,#fecha_proceso
	                c10_usuario             ,
	                ""                      ,#fecha_archivo
	                0                        #etiqueta
	                )  
        END IF
    ELSE
        IF l_ret.cargo_abono = "S"   THEN
            ---CARGO--------------
            INSERT INTO dis_cuenta
                 VALUES(movimiento_cargo        ,#tipo_movimiento
	                4                       ,#subcuenta
	                11                      ,#siefore
	                reg_1.folio             ,#folio
                        000                     ,#consecutivo
	                l_ret.nss               ,#nss
	                NULL                    ,#curp
	                ""                      ,#folio_sua
	                HOY                     ,#fecha_pago
	                l_ret.fecha_valor_devol ,#fecha_valor
	                HOY                     ,#fecha_conversion
	                - d6_monto_en_pesos     ,#monto_en_pesos
	                - d6_monto_en_acc       ,#monto_accion
	                0                       ,#precio_accion
	                0                       ,#dias_cotizados
	                ""                      ,#sucursal
	                "DINF"                  ,#id_aportante
	                8                       ,#estado
	                HOY                     ,#fecha_proceso
	                c10_usuario             ,
	                ""                      ,#fecha_archivo
	                0                        #etiqueta
	                )  

            ---ABONO--------------
            INSERT INTO dis_cuenta
                 VALUES(movimiento_abono        ,#tipo_movimiento
	                4                       ,#subcuenta
	                11                      ,#siefore
	                reg_1.folio             ,#folio
                        000                     ,#consecutivo
	                l_ret.nss               ,#nss
	                NULL                    ,#curp
	                ""                      ,#folio_sua
	                HOY                     ,#fecha_pago
	                l_ret.fecha_valor_devol ,#fecha_valor
	                HOY                     ,#fecha_conversion
	                d6_monto_en_pesos       ,#monto_en_pesos
	                d6_monto_en_acc         ,#monto_accion
	                0                       ,#precio_accion
	                0                       ,#dias_cotizados
	                ""                      ,#sucursal
	                "DINF"                  ,#id_aportante
	                8                       ,#estado
	                HOY                     ,#fecha_proceso
	                c10_usuario             ,
	                ""                      ,#fecha_archivo
	                0                        #etiqueta
	                )  
        ELSE
            ---ABONO--------------
            INSERT INTO dis_cuenta
                 VALUES(movimiento_abono        ,#tipo_movimiento
	                4                       ,#subcuenta
	                11                      ,#siefore
	                reg_1.folio             ,#folio
                        000                     ,#consecutivo
	                l_ret.nss               ,#nss
	                NULL                    ,#curp
	                ""                      ,#folio_sua
	                HOY                     ,#fecha_pago
	                l_ret.fecha_valor_devol ,#fecha_valor
	                HOY                     ,#fecha_conversion
	                d6_monto_en_pesos       ,#monto_en_pesos
	                d6_monto_en_acc         ,#monto_accion
	                0                       ,#precio_accion
	                0                       ,#dias_cotizados
	                ""                      ,#sucursal
	                "DINF"                  ,#id_aportante
	                8                       ,#estado
	                HOY                     ,#fecha_proceso
	                c10_usuario             ,
	                ""                      ,#fecha_archivo
	                0                        #etiqueta
	                )  
        END IF
    END IF
END FUNCTION

FUNCTION esta_seguro()
#es-------------------
    WHILE TRUE
        PROMPT " ESTA SEGURO S/N" FOR CHAR enter
        IF enter MATCHES "[SsNn]" THEN
            IF enter MATCHES "[Ss]" THEN
                RETURN TRUE
            ELSE
                RETURN FALSE
            END IF
        END IF
    END WHILE
END FUNCTION
