DATABASE safre_af 
GLOBALS

    DEFINE #glo  #g_ret_parametro
	g_ret_parametro       RECORD LIKE ret_parametro.*

    DEFINE #glo  #g_lp_impresoras
	g_lp_impresoras       RECORD LIKE tab_cmd_impresora.*

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        enter                 CHAR(1)
END GLOBALS

FUNCTION tipo_movimiento(reg_1)
#tm------------------------------
    DEFINE reg_1 RECORD #loc #reg_1
        tipo_seguro           LIKE ret_resol_retiro.tipo_seguro ,
        tipo_pension          LIKE ret_resol_retiro.tipo_pension ,
        tipo_prestacion       LIKE ret_resol_retiro.tipo_prestacion
    END RECORD
 
    DEFINE
        v_tipo_mov            INTEGER

    IF reg_1.tipo_prestacion = 11 THEN
        SELECT tipo_movimiento
        INTO   v_tipo_mov
        FROM   ret_matriz
        WHERE  tipo_seguro     = reg_1.tipo_seguro
        AND    tipo_pension    = reg_1.tipo_pension
        AND    tipo_prestacion = reg_1.tipo_prestacion

        RETURN v_tipo_mov
    ELSE

    CASE reg_1.tipo_seguro
        WHEN "VV" ---RETIRO DE APORTACIONES VOLUNTARIAS
            RETURN 490

        WHEN "RI"
            CASE reg_1.tipo_pension
                WHEN "CE"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 462
                    END CASE
                WHEN "VE"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 463
                    END CASE
            END CASE
                                                    
        WHEN "IM"
            CASE reg_1.tipo_pension
                WHEN "IN"
                    CASE reg_1.tipo_prestacion
                        WHEN 1
                            RETURN 401
                        WHEN 3
                            RETURN 457
                        WHEN 4
                            RETURN 437
                        WHEN 5
                            RETURN 415
                    END CASE
                WHEN "VI"
                    CASE reg_1.tipo_prestacion
                        WHEN 1
                            RETURN 402
                        WHEN 3
                            RETURN 458
                        WHEN 4
                            RETURN 438
                        WHEN 5
                            RETURN 416
                    END CASE
                WHEN "VO"
                    CASE reg_1.tipo_prestacion
                        WHEN 1
                            RETURN 403
                        WHEN 3
                            RETURN 459
                        WHEN 4
                            RETURN 439
                        WHEN 5
                            RETURN 417
                    END CASE
                WHEN "OR"
                    CASE reg_1.tipo_prestacion
                        WHEN 1
                            RETURN 404
                        WHEN 3
                            RETURN 460
                        WHEN 4
                            RETURN 440
                        WHEN 5
                            RETURN 418
                    END CASE
                WHEN "AS"
                    CASE reg_1.tipo_prestacion
                        WHEN 1
                            RETURN 405
                        WHEN 3
                            RETURN 461
                        WHEN 4
                            RETURN 441
                        WHEN 5
                            RETURN 419
                    END CASE
            END CASE

        WHEN "RT"
            CASE reg_1.tipo_pension
                WHEN "CE"
                    CASE reg_1.tipo_prestacion
                        WHEN 8 
                           RETURN  491
                    END CASE
                WHEN "IN"
                    CASE reg_1.tipo_prestacion
                        WHEN 8 
                           RETURN  492
                    END CASE
                WHEN "IP"
                    CASE reg_1.tipo_prestacion
                        WHEN 1
                            RETURN 408
                        WHEN 3
                            RETURN 464
                        WHEN 4
                            RETURN 436
                        WHEN 5
                            RETURN 422
                    END CASE
                WHEN "VI"
                    CASE reg_1.tipo_prestacion
                        WHEN 1
                            RETURN 409
                        WHEN 3
                            RETURN 465
                        WHEN 4
                            RETURN 446
                        WHEN 5
                            RETURN 423
                        WHEN 8
                            RETURN 491
                    END CASE
                WHEN "VE"
                    CASE reg_1.tipo_prestacion
                        WHEN 8 
                           RETURN  492
                    END CASE
                WHEN "VO"
                    CASE reg_1.tipo_prestacion
                        WHEN 1
                            RETURN 410
                        WHEN 3
                            RETURN 466
                        WHEN 4
                            RETURN 448
                        WHEN 5
                            RETURN 424
                        WHEN 8
                            RETURN 491

                    END CASE
                WHEN "OR"
                    CASE reg_1.tipo_prestacion
                        WHEN 1
                            RETURN 411
                        WHEN 3
                            RETURN 467
                        WHEN 4
                            RETURN 447
                        WHEN 5
                            RETURN 425
                        WHEN 8
                            RETURN 491
                    END CASE
                WHEN "AS"
                    CASE reg_1.tipo_prestacion
                        WHEN 1
                            RETURN 412
                        WHEN 3
                            RETURN 468
                        WHEN 4
                            RETURN 448
                        WHEN 5
                            RETURN 426
                        WHEN 8
                            RETURN 491
                    END CASE
            END CASE

        WHEN "CV"
            CASE reg_1.tipo_pension
                WHEN "VE"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 471
                        WHEN 4
                            RETURN 442
                        WHEN 5
                            RETURN 429
                    END CASE
                WHEN "CE"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 472
                        WHEN 4
                            RETURN 443
                        WHEN 5
                            RETURN 430
                    END CASE
                WHEN "IN"
                    CASE reg_1.tipo_prestacion
                        WHEN 5
                            RETURN 430
                    END CASE
            END CASE

        WHEN "NP"
            CASE reg_1.tipo_pension
                WHEN "IP"
                    CASE reg_1.tipo_prestacion
                        WHEN 5
                            RETURN 436
                    END CASE
                WHEN "IN"
                    CASE reg_1.tipo_prestacion
                        WHEN 5
                            RETURN 437
                    END CASE
                WHEN "VI"
                    CASE reg_1.tipo_prestacion
                        WHEN 5
                            RETURN 438
                    END CASE
                WHEN "VO"
                    CASE reg_1.tipo_prestacion
                        WHEN 5
                            RETURN 439
                    END CASE
                WHEN "OR"
                    CASE reg_1.tipo_prestacion
                        WHEN 5
                            RETURN 440
                    END CASE
                WHEN "AS"
                    CASE reg_1.tipo_prestacion
                        WHEN 5
                            RETURN 441
                    END CASE
                WHEN "CE"
                    CASE reg_1.tipo_prestacion
                        WHEN 5
                            RETURN 443
                    END CASE
                WHEN "VE"
                    CASE reg_1.tipo_prestacion
                        WHEN 5
                            RETURN 442
                    END CASE
            END CASE
    
        WHEN "TJ"
            CASE reg_1.tipo_pension
                WHEN "IP"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 475
                        WHEN 5
                            RETURN 446
                    END CASE
                WHEN "IN"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 476
                        WHEN 5
                            RETURN 447
                    END CASE
                WHEN "VI"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 477
                        WHEN 5
                            RETURN 448
                    END CASE
                WHEN "VO"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 478
                        WHEN 5
                            RETURN 449
                    END CASE
                WHEN "OR"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 479
                        WHEN 5
                            RETURN 450
                    END CASE
                WHEN "AS"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 480
                        WHEN 5
                            RETURN 451
                    END CASE
                WHEN "RE"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 481
                        WHEN 5
                            RETURN 452
                    END CASE
                WHEN "VE"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 482
                        WHEN 5
                            RETURN 453
                    END CASE
                WHEN "CE"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 483
                        WHEN 5
                            RETURN 454
                    END CASE
            END CASE

        WHEN "PP"
            CASE reg_1.tipo_pension
                WHEN "RE"
                    CASE reg_1.tipo_prestacion
                        WHEN 1
                            RETURN 495
                        WHEN 3
                            RETURN 495
                        WHEN 4
                            RETURN 433
                        WHEN 5
                            RETURN 433
                    END CASE
                WHEN "IP"
                    CASE reg_1.tipo_prestacion
                        WHEN 4
                            RETURN 433
                        WHEN 5
                            RETURN 433
                    END CASE
                WHEN "IN"
                    CASE reg_1.tipo_prestacion
                        WHEN 4
                            RETURN 433
                        WHEN 5
                            RETURN 433
                    END CASE
                WHEN "VI"
                    CASE reg_1.tipo_prestacion
                        WHEN 4
                            RETURN 433
                        WHEN 5
                            RETURN 433
                    END CASE
                WHEN "VO"
                    CASE reg_1.tipo_prestacion
                        WHEN 4
                            RETURN 433
                        WHEN 5
                            RETURN 433
                    END CASE
                WHEN "OR"
                    CASE reg_1.tipo_prestacion
                        WHEN 4
                            RETURN 433
                        WHEN 5
                            RETURN 433
                    END CASE
                WHEN "CE"
                    CASE reg_1.tipo_prestacion
                        WHEN 1
                            RETURN 496
                        WHEN 3
                            RETURN 497
                        WHEN 4
                            RETURN 433
                        WHEN 5
                            RETURN 433
                    END CASE
                WHEN "VE"
                    CASE reg_1.tipo_prestacion
                        WHEN 1
                            RETURN 498
                        WHEN 3
                            RETURN 499
                        WHEN 4
                            RETURN 433
                        WHEN 5
                            RETURN 433
                    END CASE
                WHEN "AS"
                    CASE reg_1.tipo_prestacion
                        WHEN 4
                            RETURN 433
                        WHEN 5
                            RETURN 433
                    END CASE
	         END CASE

        WHEN "IV"
            CASE reg_1.tipo_pension
                WHEN "IN"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 457
                        WHEN 4
                            RETURN 437
                        WHEN 5
                            RETURN 415
                        WHEN 8
                            RETURN 492
                    END CASE
                WHEN "VI"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 458
                        WHEN 4
                            RETURN 438
                        WHEN 5
                            RETURN 416
                        WHEN 8
                            RETURN 491
                    END CASE
                WHEN "VO"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 459
                        WHEN 4
                            RETURN 439
                        WHEN 5
                            RETURN 417
                        WHEN 8
                            RETURN 491
                    END CASE
                WHEN "OR"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 460
                        WHEN 4
                            RETURN 440
                        WHEN 5
                            RETURN 418
                        WHEN 8
                            RETURN 491
                    END CASE
                WHEN "AS"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 461
                        WHEN 4
                            RETURN 441
                        WHEN 5
                            RETURN 419
                        WHEN 8
                            RETURN 491
                    END CASE
                WHEN "VE"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 471
                        WHEN 4
                            RETURN 442
                        WHEN 5
                            RETURN 429
                        WHEN 8
                            RETURN 492
                        WHEN 9
                            RETURN 428
                    END CASE
                WHEN "CE"
                    CASE reg_1.tipo_prestacion
                        WHEN 3
                            RETURN 472
                        WHEN 4
                            RETURN 443
                        WHEN 5
                            RETURN 430
                        WHEN 8
                            RETURN 491
                    END CASE
	         END CASE

        OTHERWISE
            CASE reg_1.tipo_prestacion
                WHEN 6
                    RETURN 486
                WHEN 7
                    RETURN 487
            END CASE
    END CASE
    END IF
END FUNCTION

{
FUNCTION consulta_ctrl_cta(c11_n_seguro)
#ccc------------------------------------
    DEFINE #loc #char
        c11_n_seguro          CHAR(11)

    DEFINE reg_3 RECORD #loc #reg_3
        estado_cuenta         LIKE cta_ctr_cuenta.estado_cuenta ,
        estado_proceso        LIKE cta_ctr_cuenta.estado_proceso
    END RECORD

    SELECT estado_cuenta  ,
           estado_proceso
    INTO   reg_3.*
    FROM   cta_ctr_cuenta
    WHERE  nss = c11_n_seguro

    IF STATUS = NOTFOUND THEN
        RETURN 18,18
    ELSE
        RETURN reg_3.estado_cuenta,reg_3.estado_proceso
    END IF

END FUNCTION
}

{
FUNCTION actualiza_ctrl_cta(reg_2)
#acc------------------------------
    DEFINE reg_2 RECORD
        n_seguro              CHAR(11) ,
        estado_proceso        SMALLINT
    END RECORD

    UPDATE cta_ctr_cuenta
    SET    cta_ctr_cuenta.estado_proceso    = reg_2.estado_proceso ,
           cta_ctr_cuenta.fecha_edo_proceso = TODAY                ,
           cta_ctr_cuenta.cambio_estado     = 1                    ,
           cta_ctr_cuenta.usuario           = usuario
    WHERE  cta_ctr_cuenta.nss = reg_2.n_seguro
       
END FUNCTION
}

{
FUNCTION autoriza_retiro(c11_n_seguro)
#ar-----------------------------------
    DEFINE reg_4 RECORD #glo #reg_4
        estado_cuenta         LIKE cta_ctr_cuenta.estado_cuenta ,
        estado_proceso        LIKE cta_ctr_cuenta.estado_proceso
    END RECORD

    DEFINE #loc #char
        c11_n_seguro          CHAR(11)

    CALL consulta_ctrl_cta(c11_n_seguro) #ccc
        RETURNING reg_4.estado_cuenta ,
                  reg_4.estado_proceso

    CASE reg_4.estado_cuenta
        WHEN 0
        WHEN 1
            CASE reg_4.estado_proceso
                WHEN 0
                    RETURN 1,reg_4.estado_proceso

                OTHERWISE 
                    RETURN 2,reg_4.estado_proceso
            END CASE

        WHEN 2
            CASE reg_4.estado_proceso
                WHEN 0
                    RETURN 1,reg_4.estado_proceso

                OTHERWISE 
                    RETURN 2,reg_4.estado_proceso
            END CASE

        WHEN 5
            RETURN 3,reg_4.estado_proceso

        WHEN 18
            RETURN 18,18

    END CASE
END FUNCTION
}

FUNCTION funcion_tipo_retiro()
#ftr--------------------------
    DEFINE arr_1 ARRAY[10] OF RECORD #loc #arr_1
        codigo	               LIKE ret_tipo_retiro.codigo     ,
        desc_larga             LIKE ret_tipo_retiro.desc_larga
    END RECORD

    DEFINE arr_3 ARRAY[10] OF RECORD #loc #arr_3
	desc_corta             LIKE ret_tipo_retiro.desc_corta
    END RECORD

    DEFINE #loc #smallint
	i                      ,
	arr_c                  ,
     	cont_1	               SMALLINT

    FOR i = 1 TO 10
        INITIALIZE arr_1[i].* TO NULL
        INITIALIZE arr_3[i].* TO NULL
    END FOR

    LET arr_c = 0       LET cont_1  = 0

    DECLARE cur_3 CURSOR FOR
    SELECT *
    FROM   ret_tipo_retiro

    LET cont_1 = 1

    FOREACH cur_3 INTO arr_1[cont_1].codigo     ,
		       arr_3[cont_1].desc_corta ,
		       arr_1[cont_1].desc_larga
	LET cont_1 = cont_1 + 1
    END FOREACH

    OPEN WINDOW glob_ret1 AT 8,13 WITH FORM "GLOB_RET1" ATTRIBUTE(BORDER) 
        DISPLAY "TIPOS DE RETIRO" AT 1,21 

        CALL SET_COUNT(cont_1-1)

        DISPLAY ARRAY arr_1 TO scr_1.*
            ON KEY ( RETURN )
                LET arr_c = ARR_CURR()
                EXIT DISPLAY

            ON KEY ( INTERRUPT )
                LET arr_c                   = 1
                LET arr_1[arr_c].codigo     = NULL
	        LET arr_3[arr_c].desc_corta = NULL
	        LET arr_1[arr_c].desc_larga = NULL

                EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW glob_ret1

    RETURN arr_1[arr_c].codigo     ,
	   arr_3[arr_c].desc_corta ,
	   arr_1[arr_c].desc_larga
END FUNCTION

FUNCTION despliega_tipo_pago()
#dtp--------------------------
    DEFINE l_reg ARRAY[1000] OF RECORD
        codigo                INTEGER ,
        descripcion	      CHAR(50)
    END RECORD

    DEFINE
        x_x		      CHAR(100) ,
        x_buscar	      CHAR(030)

    DEFINE
        pos                   ,
        aux_val               SMALLINT

    FOR pos = 1 TO 10
        INITIALIZE l_reg[pos].* TO NULL
    END FOR

    LET pos = 0
    LET aux_val = 0
    INITIALIZE x_x, x_buscar TO NULL

    OPEN WINDOW glob_ret2 AT 05,12 WITH FORM "GLOB_RET2" ATTRIBUTE(BORDER)
    DISPLAY "              TIPOS  DE COMPROBANTES (ICEFAS)                                  " AT 2,1 ATTRIBUTE(REVERSE)

    INPUT BY NAME x_buscar
        BEFORE FIELD x_buscar
            LET x_buscar = "*"

        AFTER FIELD x_buscar
	    IF x_buscar IS NULL THEN
	        ERROR "   DESCRIPCION A BUSCAR NO PUEDE SER NULA" ATTRIBUTE(NORMAL)
	        NEXT FIELD x_buscar
	    ELSE
	        EXIT INPUT
	    END IF
	END INPUT

	WHILE TRUE
	    LET x_x = " SELECT * FROM tab_pago ",
	              " WHERE  descripcion   MATCHES ",'"',x_buscar CLIPPED,'"',
	              " ORDER BY 1 " CLIPPED

	    PREPARE pre_5 FROM x_x
	    DECLARE cur_5 CURSOR FOR pre_5

	    LET pos = 1
	    FOREACH cur_5 INTO l_reg[pos].*
	        LET pos = pos + 1
		IF pos >= 1000 THEN
		    ERROR "   FUE SOBREPASADA LA CAPACIDAD MAXIMA DEL ARREGLO" ATTRIBUTE(NORMAL)
		    EXIT FOREACH
		END IF
	    END FOREACH

	    IF (pos-1) < 1 THEN
	        ERROR "   CATALOGO CON TIPOS DE PAGO VACIA" ATTRIBUTE(NORMAL)
	    END IF

	    CALL SET_COUNT(pos-1)

	    DISPLAY ARRAY l_reg TO scr_1.*
	        ON KEY ( INTERRUPT )
		    LET pos = 0
		    EXIT DISPLAY

		ON KEY ( CONTROL-M )
		    LET pos = ARR_CURR()
		    EXIT DISPLAY
	    END DISPLAY

	    IF pos <> 0 THEN
	        EXIT WHILE
	    END IF
	END WHILE

	CLOSE WINDOW glob_ret2

	RETURN l_reg[pos].codigo,l_reg[pos].descripcion

END FUNCTION

FUNCTION agrega_abono_cta(reg_3,sw_2)
#aac---------------------------------
    DEFINE reg_3 RECORD #loc #reg_3
        n_seguro              LIKE ret_abono_cta.n_seguro       ,
        consecutivo           LIKE ret_abono_cta.consecutivo    ,
        paterno               LIKE ret_abono_cta.paterno        ,
        materno               LIKE ret_abono_cta.materno        ,
        nombres               LIKE ret_abono_cta.nombres        ,
        porcentaje            LIKE ret_abono_cta.porcentaje     ,
        monto_en_pesos        LIKE ret_abono_cta.monto_en_pesos
    END RECORD  

    DEFINE arr_3 ARRAY[5] OF RECORD #loc #arr_3
        paterno               LIKE ret_abono_cta.paterno        ,
        materno               LIKE ret_abono_cta.materno        ,
        nombres               LIKE ret_abono_cta.nombres        ,
        porcentaje            LIKE ret_abono_cta.porcentaje     ,
        monto_en_pesos        LIKE ret_abono_cta.monto_en_pesos ,
        cod_banco             LIKE ret_abono_cta.cod_banco      ,
        cod_ciudad            LIKE ret_abono_cta.cod_ciudad     ,
        cod_sucursal          LIKE ret_abono_cta.cod_sucursal   ,
        nro_cuenta            LIKE ret_abono_cta.nro_cuenta
    END RECORD

  DEFINE arr_9 ARRAY[5] OF RECORD #loc #arr_9
--    DEFINE arr_9 ARRAY[10] OF RECORD #loc #arr_9
        paterno               LIKE ret_abono_cta.paterno        ,
        materno               LIKE ret_abono_cta.materno        ,
        nombres               LIKE ret_abono_cta.nombres        ,
        porcentaje            LIKE ret_abono_cta.porcentaje     ,
        monto_en_pesos        LIKE ret_abono_cta.monto_en_pesos ,
        cod_banco             LIKE ret_abono_cta.cod_banco      ,
        cod_ciudad            LIKE ret_abono_cta.cod_ciudad     ,
        cod_sucursal          LIKE ret_abono_cta.cod_sucursal   ,
        nro_cuenta            LIKE ret_abono_cta.nro_cuenta
    END RECORD

    DEFINE l_reg ARRAY[1000] OF RECORD
        estad_cod             SMALLINT ,
        estad_desc            CHAR(40) ,
        ciudad_cod            SMALLINT ,
        ciudad_desc           CHAR(40)
    END RECORD                              

    DEFINE #loc #smallint
        j                     ,
        i                     ,
        pos                   ,
        sw_1                  ,
        sw_2                  ,
        sw_3                  ,
        arr_c                 ,
        arr_c2                ,
        arr_l                 ,
        aux_val               SMALLINT   

    DEFINE #loc #decimal
        d_suma_porcentaje     DECIMAL(16,6)

    DEFINE
        x_x                   CHAR(200) ,
        x_buscar              CHAR(200)

    DEFINE vcodigo            CHAR(3)

    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-B ,
        ACCEPT KEY CONTROL-I 

    OPEN WINDOW glo_ret3 AT 6,5 WITH FORM "GLOB_RET3" ATTRIBUTE(BORDER)
    DISPLAY "                      CAPTURA DE BENEFICIARIOS                                 " AT 1,1 ATTRIBUTE(REVERSE)

    LET sw_1 = 0
    FOR pos = 1 TO 4
        INITIALIZE arr_3[pos].* TO NULL
    END FOR

    LET sw_3 = 0 
    INPUT ARRAY arr_3 WITHOUT DEFAULTS FROM scr_3.*
        BEFORE FIELD paterno
            LET arr_c  = ARR_CURR()
            LET arr_l  = SCR_LINE()

	    IF arr_c2 < arr_c THEN
	        LET arr_c2 = arr_c
            END IF

            IF sw_2 = 0 THEN
                IF sw_1 = 0 AND sw_2 = 0 THEN
		    LET sw_1                        = 1
                    LET arr_3[arr_c].paterno        = reg_3.paterno
                    LET arr_3[arr_c].materno        = reg_3.materno
                    LET arr_3[arr_c].nombres        = reg_3.nombres
                    LET arr_3[arr_c].porcentaje     = reg_3.porcentaje
                    LET arr_3[arr_c].monto_en_pesos = reg_3.monto_en_pesos
                    LET arr_3[arr_c].cod_banco      = 44
                END IF
	    ELSE
                IF sw_3 = 0 THEN
	            DECLARE cur_16 CURSOR FOR
                    SELECT paterno        ,
                           materno        ,
                           nombres        ,
                           porcentaje     ,
                           monto_en_pesos ,
                           cod_banco      ,
                           cod_ciudad     ,
                           cod_sucursal   ,
                           nro_cuenta
                    FROM   ret_abono_cta
                    WHERE  consecutivo = reg_3.consecutivo 

                    LET pos = 1
                    FOREACH cur_16 INTO arr_3[pos].*
                      LET arr_9[pos].* = arr_3[pos].*

                      DISPLAY arr_3[pos].paterno        TO scr_3[pos].paterno
                      DISPLAY arr_3[pos].materno        TO scr_3[pos].materno
                      DISPLAY arr_3[pos].nombres        TO scr_3[pos].nombres
                      DISPLAY arr_3[pos].porcentaje     TO scr_3[pos].porcentaje
                      DISPLAY arr_3[pos].monto_en_pesos TO scr_3[pos].monto_en_pesos
                      DISPLAY arr_3[pos].cod_banco      TO scr_3[pos].cod_banco 
                      DISPLAY arr_3[pos].cod_ciudad     TO scr_3[pos].cod_ciudad
                      DISPLAY arr_3[pos].cod_sucursal   TO scr_3[pos].cod_sucursal
                      LET pos = pos + 1
                    END FOREACH
                END IF

                DELETE
                FROM  ret_abono_cta
                WHERE consecutivo = reg_3.consecutivo
            END IF

            DISPLAY arr_3[arr_c].paterno        TO scr_3[arr_l].paterno
            DISPLAY arr_3[arr_c].materno        TO scr_3[arr_l].materno
            DISPLAY arr_3[arr_c].nombres        TO scr_3[arr_l].nombres
            DISPLAY arr_3[arr_c].porcentaje     TO scr_3[arr_l].porcentaje
            DISPLAY arr_3[arr_c].monto_en_pesos TO scr_3[arr_l].monto_en_pesos
            DISPLAY arr_3[arr_c].cod_banco      TO scr_3[arr_l].cod_banco 
            DISPLAY arr_3[arr_c].cod_ciudad     TO scr_3[arr_l].cod_ciudad
            DISPLAY arr_3[arr_c].cod_sucursal   TO scr_3[arr_l].cod_sucursal

        AFTER FIELD paterno

        AFTER FIELD materno

        AFTER FIELD nombres

        AFTER FIELD porcentaje
	    IF arr_3[arr_c].porcentaje <= 1 THEN
	        ERROR "   PORCENTAJE NO PUEDE SER MENOR A 1" ATTRIBUTE(NORMAL)
		NEXT FIELD porcentaje
	    END IF

	    IF arr_3[arr_c].porcentaje > 100 THEN
	        ERROR "   PORCENTAJE NO PUEDE SER MAYOR A 100" ATTRIBUTE(NORMAL)
		NEXT FIELD porcentaje
	    END IF

        AFTER FIELD monto_en_pesos
        AFTER FIELD cod_banco
{svera
            IF arr_3[arr_c].cod_banco IS NULL THEN
               CALL despliega_tipo_banco()
	       RETURNING vcodigo
	       LET arr_3[arr_c].cod_banco = vcodigo
	       DISPLAY arr_3[arr_c].cod_banco TO scr_3[arr_c].cod_banco
	       IF vcodigo IS NULL THEN
		  NEXT FIELD cod_banco
	       END IF
	    END IF
svera}
        AFTER FIELD cod_ciudad
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD cod_banco
            END IF

            IF arr_3[arr_c].cod_ciudad IS NULL THEN
               LET pos = 0
               LET aux_val = 0
               INITIALIZE x_x, x_buscar TO NULL
                                                
               OPEN WINDOW glob_ret8 AT 08,07 WITH FORM "GLOB_RET8" ATTRIBUTE(BORDER)
               DISPLAY "                      ESTADO / CIUDADES                                   " AT 1,1 ATTRIBUTE(REVERSE)
    
               LET int_flag = FALSE 
    
               CONSTRUCT x_buscar ON A.estad_cod,
                                     B.estad_desc,
                                     A.ciudad_desc
                  FROM estad_cod,estad_desc,ciudad_desc
                  ON KEY (control-m)
                     LET int_flag = FALSE
                     EXIT CONSTRUCT
                  ON KEY (control-c)
                     LET int_flag = TRUE
                     EXIT CONSTRUCT
               END CONSTRUCT                          
    
               IF int_flag = TRUE THEN
                  LET int_flag = FALSE
                  ERROR "BUSQUEDA CANCELADA..."
                  SLEEP 2
                  ERROR ""
                  CLOSE WINDOW glob_ret8
                  NEXT FIELD  cod_ciudad
               ELSE                    
    
                  WHILE TRUE
                     LET x_x = " SELECT A.estad_cod,B.estad_desc,A.ciudad_cod,A.ciudad_desc FROM tab_ciudad A,tab_estado B ",
                               " WHERE  A.estad_cod = B.estad_cod ",
                               " AND    ",
                               x_buscar CLIPPED,
                               " ORDER BY 1 " 
    
                     PREPARE pre_18 FROM x_x
                     DECLARE cur_18 CURSOR FOR pre_18
     
                     LET pos = 1
                     FOREACH cur_18 INTO l_reg[pos].*
                        LET pos = pos + 1
                        IF pos >= 1000 THEN
                           ERROR "   FUE SOBREPASADA LA CAPACIDAD MAXIMA DEL ARREGLO" ATTRIBUTE(NORMAL)
                           EXIT FOREACH
                        END IF
                     END FOREACH
                                                                               
                     IF (pos-1) < 1 THEN
                        ERROR "   REGISTRO INEXISTENTE" ATTRIBUTE(NORMAL)
                        SLEEP 2
                        ERROR ""
                        CLOSE WINDOW glob_ret8
                        NEXT FIELD  cod_ciudad
    --                    ELSE                                        
    --                  ERROR "   CATALOGO CON CIUDADES VACIO" ATTRIBUTE(NORMAL)
                     END IF
     
                     CALL SET_COUNT(pos-1)
     
                     DISPLAY ARRAY l_reg TO scr_1.*
                        ON KEY ( INTERRUPT )
                           LET pos = 0
                           EXIT DISPLAY
                        ON KEY ( CONTROL-M )
                           LET pos = ARR_CURR()
                           LET arr_3[arr_c].cod_ciudad = l_reg[pos].ciudad_cod
                           EXIT DISPLAY
                     END DISPLAY
    
                     IF pos <> 0 THEN
                        EXIT WHILE
                        END IF
                  END WHILE
    
                  CLOSE WINDOW glob_ret8 
                  DISPLAY BY NAME arr_3[arr_c].cod_ciudad
               END IF
            ELSE
               SELECT "X"
               FROM    tab_ciudad   
               WHERE   ciudad_cod  = arr_3[arr_c].cod_ciudad
      
               IF STATUS = NOTFOUND THEN
                  ERROR "   CODIGO INEXISTENTE"  ATTRIBUTE(NORMAL)
                  NEXT FIELD cod_ciudad 
               END IF                                  
            END IF

        AFTER FIELD cod_sucursal

        ON KEY (CONTROL-B)
            LET sw_3  = 1
            LET arr_c = ARR_CURR()

            INITIALIZE arr_3[arr_c].* TO NULL

            FOR i = arr_c TO 4
                LET j          = i + 1
                LET arr_3[i].* = arr_3[j].* 

                INITIALIZE arr_3[j].* TO NULL

                DISPLAY arr_3[i].paterno        TO scr_3[i].paterno
                DISPLAY arr_3[i].materno        TO scr_3[i].materno
                DISPLAY arr_3[i].nombres        TO scr_3[i].nombres
                DISPLAY arr_3[i].porcentaje     TO scr_3[i].porcentaje
                DISPLAY arr_3[i].monto_en_pesos TO scr_3[i].monto_en_pesos
                DISPLAY arr_3[i].cod_banco      TO scr_3[i].cod_banco 
                DISPLAY arr_3[i].cod_ciudad     TO scr_3[i].cod_ciudad
                DISPLAY arr_3[i].cod_sucursal   TO scr_3[i].cod_sucursal
            END FOR
        
           
        ON KEY (ESC)
            FOR arr_c = 1 TO 5 
                IF      arr_3[arr_c].paterno        IS NULL
                    AND arr_3[arr_c].materno        IS NULL
                    AND arr_3[arr_c].nombres        IS NULL
                    AND arr_3[arr_c].porcentaje     IS NULL
                    AND arr_3[arr_c].monto_en_pesos IS NULL
                    AND arr_3[arr_c].cod_banco      IS NULL
                    AND arr_3[arr_c].cod_ciudad     IS NULL
                    AND arr_3[arr_c].cod_sucursal   IS NULL
                THEN
                    EXIT FOR
                ELSE
                    IF arr_3[arr_c].paterno IS NULL THEN
                        ERROR " PATERNO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD paterno
                    END IF
{
                    IF arr_3[arr_c].materno IS NULL THEN
                        ERROR " MATERNO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD materno
                    END IF
}
                    IF arr_3[arr_c].nombres IS NULL THEN
                        ERROR " NOMBRES NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD nombres
                    END IF
    
                    IF arr_3[arr_c].porcentaje IS NULL THEN
                        ERROR " PORCENTAJE NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD porcentaje
                    END IF

                    IF arr_3[arr_c].cod_banco IS NULL THEN
                        ERROR " BANCO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD cod_banco
                    END IF

                    IF arr_3[arr_c].cod_ciudad IS NULL THEN
                        ERROR " CIUDAD NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD cod_ciudad
                    END IF

                    IF arr_3[arr_c].nro_cuenta IS NULL THEN
                        ERROR " CUENTA NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD nro_cuenta
                    END IF
                END IF

	        IF arr_3[arr_c].porcentaje <= 1 THEN
		    ERROR "   PORCENTAJE NO PUEDE SER MENOR A 1"
                          ATTRIBUTE(NORMAL)
		    NEXT FIELD porcentaje
	        END IF

	        IF arr_3[arr_c].porcentaje > 100 THEN
		    ERROR "   PORCENTAJE NO PUEDE SER MAYOR A 100"
                          ATTRIBUTE(NORMAL)
		    NEXT FIELD porcentaje
	        END IF

                LET d_suma_porcentaje = 0
                FOR pos = 1 TO 5
	            IF arr_3[pos].porcentaje IS NULL THEN
	                EXIT FOR
                    ELSE
		        IF arr_3[pos].porcentaje > 0 THEN
	                    LET d_suma_porcentaje = d_suma_porcentaje +
					            arr_3[pos].porcentaje
		        END IF
		    END IF
                END FOR

                IF d_suma_porcentaje > 100 THEN
	            ERROR "   SUMA EXCEDE EL 100 PORCIENTO" ATTRIBUTE(NORMAL)
		    NEXT FIELD porcentaje
                ELSE
                    IF d_suma_porcentaje < 100 THEN
		        ERROR "   SUMA ES INFERIOR AL 100 PORCIENTO" ATTRIBUTE(NORMAL)
		        NEXT FIELD porcentaje
		    END IF
                END IF
            END FOR
            EXIT INPUT

        ON KEY (INTERRUPT)
            FOR i = 1 TO 5
                LET arr_3[i].* = arr_9[i].*
            END FOR
            EXIT INPUT
    END INPUT

    DELETE
    FROM   ret_abono_cta
    WHERE  consecutivo = reg_3.consecutivo

    FOR pos = 1 TO 5 #arr_c2
	IF arr_3[pos].porcentaje IS NULL THEN
	    EXIT FOR
        ELSE
            INSERT INTO ret_abono_cta
                VALUES(reg_3.n_seguro            ,
                       reg_3.consecutivo         ,
                       arr_3[pos].paterno        ,
                       arr_3[pos].materno        ,
                       arr_3[pos].nombres        ,
                       arr_3[pos].porcentaje     ,
                       arr_3[pos].monto_en_pesos ,
		       arr_3[pos].cod_banco      ,
                       arr_3[pos].cod_ciudad     ,
                       arr_3[pos].cod_sucursal   ,
                       arr_3[pos].nro_cuenta
                      )
        END IF
    END FOR
    CLOSE WINDOW glo_ret3
END FUNCTION

FUNCTION agrega_orden_de_pago(reg_7,sw_2)
#aodp------------------------------------
    DEFINE reg_7 RECORD #loc #reg_7
        n_seguro              LIKE ret_orden_pago.n_seguro     ,
        consecutivo           LIKE ret_orden_pago.consecutivo  ,
        paterno               LIKE ret_orden_pago.paterno      ,
        materno               LIKE ret_orden_pago.materno      ,
        nombres               LIKE ret_orden_pago.nombres      ,
        porcentaje            LIKE ret_orden_pago.porcentaje   ,
        monto_en_pesos        LIKE ret_orden_pago.monto_en_pesos
    END RECORD  

    DEFINE arr_7 ARRAY[5] OF RECORD #loc #arr_7
        paterno               LIKE ret_orden_pago.paterno        ,
        materno               LIKE ret_orden_pago.materno        ,
        nombres               LIKE ret_orden_pago.nombres        ,
        porcentaje            LIKE ret_orden_pago.porcentaje     ,
        monto_en_pesos        LIKE ret_orden_pago.monto_en_pesos ,
        cod_banco             LIKE ret_orden_pago.cod_banco      ,
        cod_ciudad            LIKE ret_orden_pago.cod_ciudad     ,
        cod_sucursal          LIKE ret_orden_pago.cod_sucursal
    END RECORD

    DEFINE arr_8 ARRAY[5] OF RECORD #loc #arr_8
        paterno               LIKE ret_orden_pago.paterno        ,
        materno               LIKE ret_orden_pago.materno        ,
        nombres               LIKE ret_orden_pago.nombres        ,
        porcentaje            LIKE ret_orden_pago.porcentaje     ,
        monto_en_pesos        LIKE ret_orden_pago.monto_en_pesos ,
        cod_banco             LIKE ret_orden_pago.cod_banco      ,
        cod_ciudad            LIKE ret_orden_pago.cod_ciudad     ,
        cod_sucursal          LIKE ret_orden_pago.cod_sucursal
    END RECORD

    DEFINE l_reg ARRAY[1000] OF RECORD
        estad_cod             SMALLINT,
        estad_desc            CHAR(40),
        ciudad_cod            SMALLINT,
        ciudad_desc           CHAR(40)
    END RECORD                              

    DEFINE #loc #smallint
        j                     ,
        i                     ,
        pos                   ,
        sw_1                  ,
        sw_2                  ,
        sw_3                  ,
        arr_c                 ,
        arr_c2                ,
        arr_l                 SMALLINT,
        aux_val               SMALLINT   

    DEFINE #loc #decimal
        d_suma_porcentaje     DECIMAL(16,6)

    DEFINE
        x_x                   CHAR(200) ,
        x_buscar              CHAR(200)

    DEFINE vcodigo            CHAR(3)

    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-B ,
        ACCEPT KEY CONTROL-I 

    OPEN WINDOW glo_ret7 AT 6,5 WITH FORM "GLOB_RET7" ATTRIBUTE(BORDER)
    DISPLAY "                      CAPTURA DE BENEFICIARIOS                                 " AT 1,1 ATTRIBUTE(REVERSE)

    LET sw_1 = 0
    FOR pos = 1 TO 4
        INITIALIZE arr_7[pos].* TO NULL
    END FOR

                let sw_3 = 0 
    INPUT ARRAY arr_7 WITHOUT DEFAULTS FROM scr_7.*
        BEFORE FIELD paterno
            LET arr_c  = ARR_CURR()
            LET arr_l  = SCR_LINE()

	    IF arr_c2 < arr_c THEN
	        LET arr_c2 = arr_c
            END IF

            IF sw_2 = 0 THEN
                IF sw_1 = 0 AND sw_2 = 0 THEN
		    LET sw_1                        = 1
                    LET arr_7[arr_c].paterno        = reg_7.paterno
                    LET arr_7[arr_c].materno        = reg_7.materno
                    LET arr_7[arr_c].nombres        = reg_7.nombres
                    LET arr_7[arr_c].porcentaje     = reg_7.porcentaje
                    LET arr_7[arr_c].monto_en_pesos = reg_7.monto_en_pesos
                    LET arr_7[arr_c].cod_banco      = 44
                END IF
	    ELSE
                IF sw_3 = 0 THEN
{svera
                    SELECT "OK"
                    FROM   ret_orden_pago
                    WHERE  consecutivo = reg_7.consecutivo
                    GROUP BY 1

                    IF STATUS = NOTFOUND THEN
                        LET arr_7[arr_c].paterno        = reg_7.paterno
                        LET arr_7[arr_c].materno        = reg_7.materno
                        LET arr_7[arr_c].nombres        = reg_7.nombres
                        LET arr_7[arr_c].porcentaje     = reg_7.porcentaje
                        LET arr_7[arr_c].monto_en_pesos = reg_7.monto_en_pesos
                    ELSE
svera}
	                DECLARE cur_19 CURSOR FOR
                        SELECT paterno        ,
                               materno        ,
                               nombres        ,
                               porcentaje     ,
                               monto_en_pesos ,
                               cod_banco      ,
                               cod_ciudad     ,
                               cod_sucursal
                        FROM   ret_orden_pago
                        WHERE  consecutivo = reg_7.consecutivo 

                        LET pos = 1
                        FOREACH cur_19 INTO arr_7[pos].*
                            LET arr_8[pos].* = arr_7[pos].*

                            DISPLAY arr_7[pos].paterno        TO scr_7[pos].paterno
                            DISPLAY arr_7[pos].materno        TO scr_7[pos].materno
                            DISPLAY arr_7[pos].nombres        TO scr_7[pos].nombres
                            DISPLAY arr_7[pos].porcentaje     TO scr_7[pos].porcentaje
                            DISPLAY arr_7[pos].monto_en_pesos TO scr_7[pos].monto_en_pesos
                            DISPLAY arr_7[pos].cod_banco      TO scr_7[pos].cod_banco 
                            DISPLAY arr_7[pos].cod_ciudad     TO scr_7[pos].cod_ciudad
                            DISPLAY arr_7[pos].cod_sucursal   TO scr_7[pos].cod_sucursal
                            LET pos = pos + 1
                        END FOREACH
                    --sveraEND IF
                END IF

                DELETE
                FROM  ret_abono_cta
                WHERE consecutivo = reg_7.consecutivo
            END IF

            DISPLAY arr_7[arr_c].paterno        TO scr_7[arr_l].paterno
            DISPLAY arr_7[arr_c].materno        TO scr_7[arr_l].materno
            DISPLAY arr_7[arr_c].nombres        TO scr_7[arr_l].nombres
            DISPLAY arr_7[arr_c].porcentaje     TO scr_7[arr_l].porcentaje
            DISPLAY arr_7[arr_c].monto_en_pesos TO scr_7[arr_l].monto_en_pesos
            DISPLAY arr_7[arr_c].cod_banco      TO scr_7[arr_l].cod_banco 
            DISPLAY arr_7[arr_c].cod_ciudad     TO scr_7[arr_l].cod_ciudad
            DISPLAY arr_7[arr_c].cod_sucursal   TO scr_7[arr_l].cod_sucursal

        AFTER FIELD paterno

        AFTER FIELD materno

        AFTER FIELD nombres

        AFTER FIELD porcentaje
	    IF arr_7[arr_c].porcentaje <= 1 THEN
	        ERROR "   PORCENTAJE NO PUEDE SER MENOR A 1" ATTRIBUTE(NORMAL)
		NEXT FIELD porcentaje
	    END IF

	    IF arr_7[arr_c].porcentaje > 100 THEN
	        ERROR "   PORCENTAJE NO PUEDE SER MAYOR A 100" ATTRIBUTE(NORMAL)
		NEXT FIELD porcentaje
	    END IF

        AFTER FIELD monto_en_pesos
        AFTER FIELD cod_banco
{svera
            IF arr_7[arr_c].cod_banco IS NULL THEN
               CALL despliega_tipo_banco()
	       RETURNING vcodigo
	       LET arr_7[arr_c].cod_banco = vcodigo
	       DISPLAY arr_7[arr_c].cod_banco TO scr_7[arr_c].cod_banco
	       IF vcodigo IS NULL THEN
		  NEXT FIELD cod_banco
	       END IF
	    END IF
svera}
        AFTER FIELD cod_ciudad
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD cod_banco
            END IF

            IF arr_7[arr_c].cod_ciudad IS NULL THEN
               LET pos = 0
               LET aux_val = 0
               INITIALIZE x_x, x_buscar TO NULL
                                                
               OPEN WINDOW glob_ret8 AT 08,07 WITH FORM "GLOB_RET8" ATTRIBUTE(BORDER)
               DISPLAY "                      ESTADO / CIUDADES                                   " AT 1,1 ATTRIBUTE(REVERSE)
    
               LET int_flag = FALSE 
    
               CONSTRUCT x_buscar ON A.estad_cod,
                                     B.estad_desc,
                                     A.ciudad_desc
                  FROM estad_cod,estad_desc,ciudad_desc
                  ON KEY (control-m)
                     LET int_flag = FALSE
                     EXIT CONSTRUCT
                  ON KEY (control-c)
                     LET int_flag = TRUE
                     EXIT CONSTRUCT
               END CONSTRUCT                          
    
               IF int_flag = TRUE THEN
                  LET int_flag = FALSE
                  ERROR "BUSQUEDA CANCELADA..."
                  SLEEP 2
                  ERROR ""
                  CLOSE WINDOW glob_ret8
                  NEXT FIELD  cod_ciudad
               ELSE                    
    
                  WHILE TRUE
                     LET x_x = " SELECT A.estad_cod,B.estad_desc,A.ciudad_cod,A.ciudad_desc FROM tab_ciudad A,tab_estado B ",
                               " WHERE  A.estad_cod = B.estad_cod ",
                               " AND    ",
                               x_buscar CLIPPED,
                               " ORDER BY 1 " 
    
                     PREPARE pre_20 FROM x_x
                     DECLARE cur_20 CURSOR FOR pre_20
     
                     LET pos = 1
                     FOREACH cur_20 INTO l_reg[pos].*
                        LET pos = pos + 1
                        IF pos >= 1000 THEN
                           ERROR "   FUE SOBREPASADA LA CAPACIDAD MAXIMA DEL ARREGLO" ATTRIBUTE(NORMAL)
                           EXIT FOREACH
                        END IF
                     END FOREACH
                                                                               
                     IF (pos-1) < 1 THEN
                        ERROR "   REGISTRO INEXISTENTE" ATTRIBUTE(NORMAL)
                        SLEEP 2
                        ERROR ""
                        CLOSE WINDOW glob_ret8
                        NEXT FIELD  cod_ciudad
    --                    ELSE                                        
    --                  ERROR "   CATALOGO CON CIUDADES VACIO" ATTRIBUTE(NORMAL)
                     END IF
     
                     CALL SET_COUNT(pos-1)
     
                     DISPLAY ARRAY l_reg TO scr_1.*
                        ON KEY ( INTERRUPT )
                           LET pos = 0
                           EXIT DISPLAY
                        ON KEY ( CONTROL-M )
                           LET pos = ARR_CURR()
                           LET arr_7[arr_c].cod_ciudad = l_reg[pos].ciudad_cod
                           EXIT DISPLAY
                     END DISPLAY
    
                     IF pos <> 0 THEN
                        EXIT WHILE
                        END IF
                  END WHILE
    
                  CLOSE WINDOW glob_ret8 
                  DISPLAY BY NAME arr_7[arr_c].cod_ciudad
               END IF
            ELSE
               SELECT "X"
               FROM    tab_ciudad   
               WHERE   ciudad_cod  = arr_7[arr_c].cod_ciudad
      
               IF STATUS = NOTFOUND THEN
                  ERROR "   CODIGO INEXISTENTE"  ATTRIBUTE(NORMAL)
                  NEXT FIELD cod_ciudad 
               END IF                                  
            END IF

        AFTER FIELD cod_sucursal

        ON KEY (CONTROL-B)
            LET sw_3  = 1
            LET arr_c = ARR_CURR()

            INITIALIZE arr_7[arr_c].* TO NULL

            FOR i = arr_c TO 4
                LET j          = i + 1
                LET arr_7[i].* = arr_7[j].* 

                INITIALIZE arr_7[j].* TO NULL

                DISPLAY arr_7[i].paterno        TO scr_7[i].paterno
                DISPLAY arr_7[i].materno        TO scr_7[i].materno
                DISPLAY arr_7[i].nombres        TO scr_7[i].nombres
                DISPLAY arr_7[i].porcentaje     TO scr_7[i].porcentaje
                DISPLAY arr_7[i].monto_en_pesos TO scr_7[i].monto_en_pesos
                DISPLAY arr_7[i].cod_banco      TO scr_7[i].cod_banco 
                DISPLAY arr_7[i].cod_ciudad     TO scr_7[i].cod_ciudad
                DISPLAY arr_7[i].cod_sucursal   TO scr_7[i].cod_sucursal
            END FOR
        
           
        ON KEY (ESC)
            FOR arr_c = 1 TO 5 
                IF  arr_7[arr_c].paterno        IS NULL
                AND arr_7[arr_c].materno        IS NULL
                AND arr_7[arr_c].nombres        IS NULL
                AND arr_7[arr_c].porcentaje     IS NULL
                AND arr_7[arr_c].monto_en_pesos IS NULL
                AND arr_7[arr_c].cod_banco      IS NULL
                AND arr_7[arr_c].cod_ciudad     IS NULL
                AND arr_7[arr_c].cod_banco      IS NULL
                THEN
                    EXIT FOR
                ELSE
                    IF arr_7[arr_c].paterno IS NULL THEN
                        ERROR " PATERNO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD paterno
                    END IF
{
                    IF arr_7[arr_c].materno IS NULL THEN
                        ERROR " MATERNO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD materno
                    END IF
}
                    IF arr_7[arr_c].nombres IS NULL THEN
                        ERROR " NOMBRES NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD nombres
                    END IF
    
                    IF arr_7[arr_c].porcentaje IS NULL THEN
                        ERROR " PORCENTAJE NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD porcentaje
                    END IF

                    {
                    IF arr_7[arr_c].cod_banco IS NULL THEN
                        ERROR " BANCO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD cod_banco
                    END IF
                    }

                    {
                    IF arr_7[arr_c].cod_ciudad IS NULL THEN
                        ERROR " CIUDAD NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD cod_ciudad
                    END IF
                    }
                END IF

	        IF arr_7[arr_c].porcentaje <= 1 THEN
		    ERROR "   PORCENTAJE NO PUEDE SER MENOR A 1"
                          ATTRIBUTE(NORMAL)
		    NEXT FIELD porcentaje
	        END IF

	        IF arr_7[arr_c].porcentaje > 100 THEN
		    ERROR "   PORCENTAJE NO PUEDE SER MAYOR A 100"
                          ATTRIBUTE(NORMAL)
		    NEXT FIELD porcentaje
	        END IF

                LET d_suma_porcentaje = 0
                FOR pos = 1 TO 5
	            IF arr_7[pos].porcentaje IS NULL THEN
	                EXIT FOR
                    ELSE
		        IF arr_7[pos].porcentaje > 0 THEN
	                    LET d_suma_porcentaje = d_suma_porcentaje +
					            arr_7[pos].porcentaje
		        END IF
		    END IF
                END FOR

                IF d_suma_porcentaje > 100 THEN
	            ERROR "   SUMA EXCEDE EL 100 PORCIENTO" ATTRIBUTE(NORMAL)
		    NEXT FIELD porcentaje
                ELSE
                    IF d_suma_porcentaje < 100 THEN
		        ERROR "   SUMA ES INFERIOR AL 100 PORCIENTO" ATTRIBUTE(NORMAL)
		        NEXT FIELD porcentaje
		    END IF
                END IF
            END FOR
            EXIT INPUT

        ON KEY (INTERRUPT)
            FOR i = 1 TO 5
                LET arr_7[i].* = arr_8[i].*
            END FOR
            EXIT INPUT
    END INPUT

    DELETE
    FROM   ret_orden_pago
    WHERE  consecutivo = reg_7.consecutivo

    FOR pos = 1 TO 5 #arr_c2
	IF arr_7[pos].porcentaje IS NULL THEN
	    EXIT FOR
        ELSE
            INSERT INTO ret_orden_pago
                VALUES(reg_7.n_seguro            ,
                       reg_7.consecutivo         ,
                       arr_7[pos].paterno        ,
                       arr_7[pos].materno        ,
                       arr_7[pos].nombres        ,
                       arr_7[pos].porcentaje     ,
                       arr_7[pos].monto_en_pesos ,
		       arr_7[pos].cod_banco      ,
                       arr_7[pos].cod_ciudad     ,
                       arr_7[pos].cod_sucursal
                      )
        END IF
    END FOR
    CLOSE WINDOW glo_ret7
END FUNCTION

FUNCTION despliega_tipo_banco()
#dtba----------------------------------
    DEFINE l_reg ARRAY[100] OF RECORD
        ereca_cod             CHAR(02) ,
	ereca_desc	      CHAR(50)
     END RECORD

     DEFINE x_x		      CHAR(200),
            x_buscar	      CHAR(030)

     DEFINE vcodigo               CHAR(03)  

     DEFINE pos               SMALLINT

     INITIALIZE vcodigo TO NULL
     OPEN WINDOW retm0019 AT 05,12 WITH FORM "RETM0019" ATTRIBUTE(BORDER)
     DISPLAY "             TIPO DE BANCO                           " AT 2,1 ATTRIBUTE(REVERSE)
     INPUT BY NAME x_buscar
         BEFORE FIELD x_buscar
             LET x_buscar = "*"

	 AFTER FIELD x_buscar
	     IF x_buscar IS NULL THEN
	         ERROR "DESCRIPCION A BUSCAR NO PUEDE SER NULA"
		 NEXT FIELD x_buscar
	     ELSE
	         EXIT INPUT
	     END IF
     END INPUT

       WHILE TRUE
	 LET x_x = " SELECT * FROM tab_banco ",
		   " WHERE ereca_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	           " ORDER BY 1 " CLIPPED

	 PREPARE pre_13 FROM x_x
	 DECLARE cur_13 CURSOR FOR pre_13
	 LET pos = 1
	 FOREACH cur_13 INTO l_reg[pos].*
	     LET pos = pos + 1
	     IF pos >= 100 THEN
		 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
		 EXIT FOREACH
	     END IF
	 END FOREACH

	 IF (pos-1) < 1 THEN
	     ERROR "ARCHIVO TIPO DE SEGURO VACIO"
	 END IF

	 CALL SET_COUNT(pos-1)
	 DISPLAY ARRAY l_reg TO scr_1.*
	  
	     ON KEY ( CONTROL-C )
		 LET vcodigo = NULL
	         LET pos = 1
   	     EXIT DISPLAY
	  
	     ON KEY ( CONTROL-M )
	         LET pos = ARR_CURR()
                 LET vcodigo             = l_reg[pos].ereca_cod
                 EXIT DISPLAY
	 END DISPLAY
 	  
	 IF pos <> 0 THEN
	     EXIT WHILE
	 END IF
 	  
     END WHILE
     CLOSE WINDOW retm0019

     RETURN vcodigo

END FUNCTION

FUNCTION agrega_cheque(reg_3,sw_2)
#ac-------------------------------
    DEFINE reg_3 RECORD #loc #reg_3
        num_resolucion        CHAR(06) ,
        n_seguro              CHAR(11) ,
        tipo_prestacion       SMALLINT ,
        paterno               CHAR(40) ,
        materno               CHAR(40) ,
        nombres               CHAR(40) ,
        consecutivo           INTEGER
    END RECORD   

    DEFINE arr_3 ARRAY[5] OF RECORD #loc #arr_3
        paterno               CHAR(30)     ,
        materno               CHAR(30)     ,
        nombres               CHAR(30)     ,
        porcentaje            DECIMAL(5,2) ,
        cod_plaza             SMALLINT
    END RECORD

    DEFINE #loc #smallint
        pos                   ,
        sw_1                  ,
        sw_2                  ,
        sw_3                  ,
        sw_4                  ,
        arr_c                 ,
        arr_l                 SMALLINT 

    DEFINE #loc #decimal
        d_suma_porcentaje     DECIMAL(16,6)


    OPEN WINDOW glob_ret4 AT 7,4 WITH FORM "GLOB_RET4" ATTRIBUTE(BORDER)
    DISPLAY "                       CAPTURA DE BENEFICIARIOS                                " AT 1,1 ATTRIBUTE(REVERSE)

    LET sw_1 = 0
    LET sw_4 = 0

    FOR pos = 1 TO 5
        INITIALIZE arr_3[pos].* TO NULL
    END FOR

    INPUT ARRAY arr_3 WITHOUT DEFAULTS FROM scr_7.*
        BEFORE FIELD paterno
            LET arr_c  = ARR_CURR()
            LET arr_l  = SCR_LINE()

            IF sw_2 = 3 THEN
               DELETE
               FROM  ret_cheque
               WHERE consecutivo = reg_3.consecutivo
            END IF

            IF sw_2 = 0 THEN
                IF sw_1 = 0 AND sw_2 = 0 THEN
		    LET sw_1 = 1
                    LET arr_3[arr_c].paterno    = reg_3.paterno
                    LET arr_3[arr_c].materno    = reg_3.materno
                    LET arr_3[arr_c].nombres    = reg_3.nombres
                    LET arr_3[arr_c].porcentaje = 100
                    LET arr_3[arr_c].cod_plaza  = 0
                END IF
	    ELSE
                IF sw_4 = 0 THEN
		    DECLARE cur_12 CURSOR FOR
                    SELECT paterno    ,
                           materno    ,
                           nombres    ,
                           porcentaje ,
                           cod_plaza
                    FROM   ret_cheque
                    WHERE  consecutivo = reg_3.consecutivo 
    
                    LET pos = 1

                    FOREACH cur_12 INTO arr_3[pos].*
                        DISPLAY arr_3[pos].paterno    TO scr_7[pos].paterno
                        DISPLAY arr_3[pos].materno    TO scr_7[pos].materno
                        DISPLAY arr_3[pos].nombres    TO scr_7[pos].nombres
                        DISPLAY arr_3[pos].porcentaje TO scr_7[pos].porcentaje
                        DISPLAY arr_3[pos].cod_plaza  TO scr_7[pos].cod_plaza 
                        LET pos = pos + 1
                        LET sw_4 = 1
                    END FOREACH

                END IF
                IF sw_4 = 0 THEN
                   LET arr_3[arr_c].paterno    = reg_3.paterno
                   LET arr_3[arr_c].materno    = reg_3.materno
                   LET arr_3[arr_c].nombres    = reg_3.nombres
                   LET arr_3[arr_c].porcentaje = 100
                   LET arr_3[arr_c].cod_plaza  = 0
                END IF
            END IF

            DISPLAY arr_3[arr_c].paterno    TO scr_7[arr_l].paterno
            DISPLAY arr_3[arr_c].materno    TO scr_7[arr_l].materno
            DISPLAY arr_3[arr_c].nombres    TO scr_7[arr_l].nombres
            DISPLAY arr_3[arr_c].porcentaje TO scr_7[arr_l].porcentaje
            DISPLAY arr_3[arr_c].cod_plaza  TO scr_7[arr_l].cod_plaza 

        AFTER FIELD paterno

        AFTER FIELD materno

        AFTER FIELD nombres

        AFTER FIELD porcentaje
	    IF arr_3[arr_c].porcentaje <= 1 THEN
		ERROR "   PORCENTAJE NO PUEDE SER MENOR A 1" ATTRIBUTE(NORMAL)
		NEXT FIELD porcentaje
	    END IF

	    IF arr_3[arr_c].porcentaje > 100 THEN
		ERROR "   PORCENTAJE NO PUEDE SER MAYOR A 100" ATTRIBUTE(NORMAL)
		NEXT FIELD porcentaje
	    END IF

        AFTER FIELD cod_plaza
            
{            IF arr_3[arr_c].cod_plaza IS NULL THEN
                CALL despliega_plaza() #dp
                RETURNING arr_3[arr_c].cod_plaza
            END IF
}            
            IF arr_3[arr_c].cod_plaza IS NULL THEN
                LET arr_3[arr_c].cod_plaza = 0
            END IF

        ON KEY (ESC)
            FOR arr_c = 1 TO 5 
                IF  arr_3[arr_c].paterno    IS NULL
                AND arr_3[arr_c].materno    IS NULL
                AND arr_3[arr_c].nombres    IS NULL
                AND arr_3[arr_c].porcentaje IS NULL
##                AND arr_3[arr_c].cod_plaza  IS NULL
                THEN
                    EXIT FOR
                ELSE
                    IF arr_3[arr_c].paterno IS NULL THEN
                        ERROR " PATERNO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD paterno
                    END IF
{
                    IF arr_3[arr_c].materno IS NULL THEN
                        ERROR " MATERNO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD materno
                    END IF
}
                    IF arr_3[arr_c].nombres IS NULL THEN
                        ERROR " NOMBRES NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD nombres
                    END IF

                    IF arr_3[arr_c].porcentaje IS NULL THEN
                        ERROR " PORCENTAJE NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD porcentaje
                    END IF

                  {
                    IF arr_3[arr_c].cod_plaza IS NULL THEN
                        ERROR " PLAZA NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                        NEXT FIELD cod_plaza
                    END IF
                  }
                END IF

	        IF arr_3[arr_c].porcentaje < 1 THEN
		    ERROR "   PORCENTAJE NO PUEDE SER MENOR A 1"
                          ATTRIBUTE(NORMAL)
		    NEXT FIELD porcentaje
	        END IF

	        IF arr_3[arr_c].porcentaje > 100 THEN
		    ERROR "   PORCENTAJE NO PUEDE SER MAYOR A 100"
                          ATTRIBUTE(NORMAL)
		    NEXT FIELD porcentaje
	        END IF

                LET d_suma_porcentaje = 0

                FOR pos = 1 TO 5
	            IF arr_3[pos].porcentaje IS NULL THEN
	                EXIT FOR
                    ELSE
		        IF arr_3[pos].porcentaje > 0 THEN
	                    LET d_suma_porcentaje = d_suma_porcentaje +
					            arr_3[pos].porcentaje
		        END IF
		    END IF
                END FOR

                IF d_suma_porcentaje > 100 THEN
	            ERROR "   SUMA EXCEDE EL 100 PORCIENTO"
		    NEXT FIELD porcentaje
                ELSE
                    IF d_suma_porcentaje < 100 THEN
		        ERROR "   SUMA ES INFERIOR AL 100 PORCIENTO"
		        NEXT FIELD porcentaje
		    END IF
                END IF
            
             
                IF arr_3[arr_c].cod_plaza IS NULL THEN
                    LET arr_3[arr_c].cod_plaza = 0
                END IF
            
            END FOR

            EXIT INPUT

        ON KEY (INTERRUPT)
            LET sw_3 = 1
            EXIT INPUT

    END INPUT

    IF sw_3 = 1 THEN
        CLOSE WINDOW glob_ret4
        RETURN
    END IF

    DELETE
    FROM  ret_cheque
    WHERE consecutivo = reg_3.consecutivo

    FOR pos = 1 TO 5
	IF arr_3[pos].porcentaje IS NULL THEN
	    EXIT FOR
        ELSE
            INSERT INTO ret_cheque VALUES(reg_3.n_seguro        ,
                                          reg_3.tipo_prestacion ,
                                          arr_3[pos].paterno    ,
                                          arr_3[pos].materno    ,
                                          arr_3[pos].nombres    ,
                                          arr_3[pos].porcentaje ,
                                          arr_3[pos].cod_plaza  ,
                                          reg_3.consecutivo
                                         )
        END IF
    END FOR
    CLOSE WINDOW glob_ret4
END FUNCTION

FUNCTION actualiza_cuenta_ind(reg_4)
#aci--------------------------------
    DEFINE reg_4 RECORD
        folio                 INTEGER                           ,
        n_seguro              LIKE dis_cuenta.nss               ,
        subcuenta             LIKE dis_cuenta.subcuenta         ,
        tipo_movimiento       LIKE dis_cuenta.tipo_movimiento   ,
        fecha_pago            LIKE dis_cuenta.fecha_pago        ,
        fecha_valor           LIKE dis_cuenta.fecha_valor       ,
        fecha_conversion      LIKE dis_cuenta.fecha_conversion  ,
        precio_accion         LIKE dis_cuenta.precio_accion     ,
        monto_en_acciones     LIKE dis_cuenta.monto_en_acciones ,
        monto_en_pesos        LIKE dis_cuenta.monto_en_pesos    ,
        consecutivo           INTEGER                           ,
        usuario               CHAR(8)
    END RECORD

    DEFINE #loc #char
        c11_id_aportante      CHAR(11)

    IF reg_4.tipo_movimiento = 560 THEN
        LET c11_id_aportante = "DEV_APL"
    ELSE
        LET c11_id_aportante = "RETIRO"
    END IF

    INSERT INTO dis_cuenta
	VALUES(reg_4.tipo_movimiento   ,
               reg_4.subcuenta         ,
               1                       ,
               reg_4.folio             ,     
               reg_4.consecutivo       ,
               reg_4.n_seguro          ,
               ""                      ,#curp
               ""                      ,#folio_sua
               reg_4.fecha_pago        ,#fecha_pago
               reg_4.fecha_valor       ,#fecha_valor
               reg_4.fecha_conversion  ,#fecha_conversion
               reg_4.monto_en_pesos    ,#monto_en_pesos
               reg_4.monto_en_acciones ,#monto_en_acciones
               reg_4.precio_accion     ,#precio_accion
               0                       ,#dias_cotizados
               ""                      ,#sucursal
               c11_id_aportante        ,#id_aportante
               8                       ,#status
               HOY                     ,#fecha_proceso
               reg_4.usuario           ,#usuario
               ""                      ,#fecha_archivo
               0                        #etiqueta 
              )
END FUNCTION

FUNCTION habil_siguiente(diaActual,numDiaHabil)
#hs--------------------------------------------
   DEFINE 
       diaTmp	                ,
       diaHabilSig	        ,
       diaActual	        DATE
   
   DEFINE
       cont_1                   ,
       numDiaHabil              ,
       contador	                ,
       diaSemana	        ,
       feriado	                ,
       finSemana	        SMALLINT
	  

   LET cont_1      = 0
   LET diaHabilSig = diaActual

   WHILE TRUE
       LET feriado   = 0
       LET finSemana = 0
       LET diaSemana = WEEKDAY(diaHabilSig)  

       IF diaSemana = 0 OR diaSemana = 6 THEN
      	   LET finSemana = 1
       END IF
  	     
       SELECT *
       FROM   tab_feriado 
       WHERE  feria_fecha = diaHabilSig
    	
       IF STATUS <> NOTFOUND THEN
       	   LET feriado = 1
       END IF 
		
       IF feriado = 1 OR finSemana = 1 THEN
           LET diaHabilSig = diaHabilSig + 1 UNITS DAY
       ELSE
           LET cont_1 = cont_1 + 1
           IF cont_1 = numDiaHabil THEN
               EXIT WHILE
           ELSE
               LET diaHabilSig = diaHabilSig + 1 UNITS DAY
           END IF
       END IF
   END WHILE

   RETURN diaHabilSig
END FUNCTION

FUNCTION provision(reg_4)
#p-----------------------
    DEFINE reg_4 RECORD
        folio                 INTEGER                           ,
        n_seguro              LIKE dis_cuenta.nss               ,
        subcuenta             LIKE dis_cuenta.subcuenta         ,
        siefore               LIKE dis_provision.siefore        ,
        tipo_movimiento       LIKE dis_cuenta.tipo_movimiento   ,
        fecha_pago            LIKE dis_cuenta.fecha_pago        ,
        fecha_valor           LIKE dis_cuenta.fecha_valor       ,
        fecha_conversion      LIKE dis_cuenta.fecha_conversion  ,
        precio_accion         LIKE dis_cuenta.precio_accion     ,
        monto_en_acciones     LIKE dis_cuenta.monto_en_acciones ,
        monto_en_pesos        LIKE dis_cuenta.monto_en_pesos    ,
        consecutivo           INTEGER                           ,
        usuario               CHAR(8)
    END RECORD

    INSERT INTO dis_provision
	VALUES(reg_4.tipo_movimiento   ,
               reg_4.subcuenta         ,
               reg_4.siefore           ,
               reg_4.folio             ,     
               reg_4.consecutivo       ,
               reg_4.n_seguro          ,
               ""                      ,#curp
               ""                      ,#folio_sua
               reg_4.fecha_pago        ,#fecha_pago
               reg_4.fecha_valor       ,#fecha_valor
               reg_4.fecha_conversion  ,#fecha_conversion
               reg_4.monto_en_pesos    ,#monto_en_pesos
               reg_4.monto_en_acciones ,#monto_en_acciones
               reg_4.precio_accion     ,#precio_accion
               0                       ,#dias_cotizados
               ""                      ,#sucursal
               "RETIRO"                ,#id_aportante
               7                       ,#status
               HOY                     ,#fecha_proceso
               reg_4.usuario           ,#usuario
               ""                      ,#fecha_archivo
               0                        #etiqueta 
              )
END FUNCTION

FUNCTION calcula_saldo_por_fecha_conversion(reg_5)
#-------------------------------------------------
    DEFINE reg_5 RECORD #reg_5
        n_seguro              LIKE afi_mae_afiliado.n_seguro ,
        subcuenta             LIKE dis_cuenta.subcuenta      ,
        fecha_conversion      LIKE dis_cuenta.fecha_conversion
    END RECORD

    DEFINE #loc #decimal
        d6_monto              DECIMAL(16,6)

    LET d6_monto = 0

    SELECT SUM(A.monto_en_acciones)
    INTO   d6_monto
    FROM   dis_cuenta A
    WHERE  A.nss                = reg_5.n_seguro
    AND    A.subcuenta          = reg_5.subcuenta
    AND    A.tipo_movimiento    > 0
    AND    A.fecha_conversion  <= reg_5.fecha_conversion

    IF STATUS = NOTFOUND THEN
        LET d6_monto = 0
    ELSE
        IF d6_monto IS NULL THEN
            LET d6_monto = 0
        END IF

        IF d6_monto < 0 THEN
            LET d6_monto = 0
        END IF
    END IF
    RETURN d6_monto
END FUNCTION

FUNCTION calcula_isr(reg_5)
#cisr----------------------
    DEFINE reg_5 RECORD #loc #reg_5
        n_seguro              LIKE ret_resol_retiro.n_seguro         ,
        consecutivo           LIKE ret_resol_retiro.consecutivo      ,
        monto_en_pesos        LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE reg_6 RECORD #loc #reg_6
        semanas_cotiz         LIKE ret_resol_retiro.semanas_cotizadas ,
        salario_minimo        LIKE tabsalario_minimo2.monto_sm     ,
        tot_anos              INTEGER                              ,
        mto_no_grabable       DECIMAL(16,6)                        ,
        mto_grabable          DECIMAL(16,6)                        ,
        retencion             DECIMAL(16,6)                        ,
        mto_neto              DECIMAL(16,6)
    END RECORD

    DEFINE #loc #smallint
        s_resto               ,
        s_estadop             SMALLINT

    DEFINE #loc #integer
        i_delegap             INTEGER
{
    SELECT C.estad_cod,
           C.deleg_cod
    INTO   s_estadop ,
           i_delegap
    FROM   afi_mae_afiliado A, afi_domicilio B, tab_codpos C
    WHERE  A.n_seguro       = reg_5.n_seguro
    AND    A.n_seguro       = B.nss
    AND    A.n_folio        = B.n_folio
    AND    A.tipo_solicitud = B.tipo_solicitud
    AND    B.marca_envio    = "X"
    AND    B.codpos         = C.cpos_cod
}

    SELECT B.estado   ,
           B.delega    
    INTO   s_estadop ,
           i_delegap
    FROM   afi_mae_afiliado A, afi_domicilio B
    WHERE  A.n_seguro       = reg_5.n_seguro
    AND    A.n_seguro       = B.nss
    AND    A.n_folio        = B.n_folio
    AND    A.tipo_solicitud = B.tipo_solicitud
    AND    B.marca_envio    = "X"

    IF s_estadop = 0 OR s_estadop = "" THEN
        PROMPT "ESTADO INVALIDO EN DIRECCION DEL AFILIADO, NSS = : ",
               reg_5.n_seguro
        FOR CHAR ENTER
    END IF

    IF i_delegap = 0 OR i_delegap = "" THEN
        PROMPT "DELEGACION INVALIDA EN DIRECCION DEL AFILIADO, NSS = : ",
               reg_5.n_seguro FOR CHAR ENTER
    END IF


    SELECT B.monto_sm
    INTO   reg_6.salario_minimo
    FROM   tab_zona_geo A, tabsalario_minimo2 B
    WHERE  A.estad_cod = s_estadop
    AND    A.deleg_cod = i_delegap
    AND    A.zona_cod  = B.zona_cod 

    IF reg_6.salario_minimo = 0 OR reg_6.salario_minimo = "" OR
       reg_6.salario_minimo IS NULL   
    THEN
        LET reg_6.salario_minimo = 0
        PROMPT "SALARIO MINIMO INVALIDO, NSS = ",reg_5.n_seguro
        FOR CHAR ENTER
        SELECT B.monto_sm
        INTO   reg_6.salario_minimo
        FROM   tabsalario_minimo2 B
        WHERE  B.zona_cod = 'C'
    END IF

    LET reg_6.semanas_cotiz = 0

    SELECT semanas_cotizadas
    INTO   reg_6.semanas_cotiz
    FROM   ret_resol_retiro
    WHERE  n_seguro    = reg_5.n_seguro
    AND    consecutivo = reg_5.consecutivo

    IF reg_6.semanas_cotiz = 0 OR reg_6.semanas_cotiz = "" THEN
        LET reg_6.mto_neto  = reg_5.monto_en_pesos
	LET reg_6.retencion = 0

        LET reg_6.retencion = reg_5.monto_en_pesos * 0.20
        LET reg_6.mto_neto  = reg_5.monto_en_pesos - reg_6.retencion

        PROMPT "SEMANAS COTIZADAS INVALIDAS, NSS = ",reg_5.n_seguro
        FOR CHAR ENTER

        RETURN reg_6.mto_neto, reg_6.retencion
   ELSE    
       LET s_resto        = reg_6.semanas_cotiz MOD 52
       LET reg_6.tot_anos = reg_6.semanas_cotiz / 52

       IF s_resto >= 26 THEN
           LET reg_6.tot_anos = reg_6.tot_anos + 1
       END IF

       IF reg_6.tot_anos >= 7 THEN
           LET reg_6.tot_anos = 7
       END IF

       LET reg_6.mto_no_grabable = reg_6.tot_anos * reg_6.salario_minimo * 90

       IF reg_5.monto_en_pesos > reg_6.mto_no_grabable THEN
           LET reg_6.mto_grabable = reg_5.monto_en_pesos - reg_6.mto_no_grabable
           LET reg_6.retencion    = reg_6.mto_grabable * 0.20
           LET reg_6.mto_neto     = reg_5.monto_en_pesos - reg_6.retencion
       ELSE
           LET reg_6.retencion    = 0
           LET reg_6.mto_neto     = reg_5.monto_en_pesos
       END IF

       RETURN reg_6.mto_neto, reg_6.retencion
   END IF  
END FUNCTION

FUNCTION calcula_isr2(reg_5)
#cisr2----------------------
    DEFINE reg_5 RECORD #loc #reg_5
        n_seguro              LIKE ret_resol_retiro.n_seguro         ,
        consecutivo           LIKE ret_resol_retiro.consecutivo      ,
        monto_en_pesos        LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE reg_6 RECORD #loc #reg_6
        semanas_cotiz         LIKE ret_resol_retiro.semanas_cotizadas ,
        salario_minimo        LIKE tabsalario_minimo2.monto_sm     ,
        tot_anos              INTEGER                              ,
        mto_no_grabable       DECIMAL(16,6)                        ,
        mto_grabable          DECIMAL(16,6)                        ,
        retencion             DECIMAL(16,6)                        ,
        mto_neto              DECIMAL(16,6)
    END RECORD

    DEFINE #loc #smallint
        s_descuenta_anos      ,
        s_ano_cotizacion      ,
        s_resto               ,
        s_estadop             SMALLINT

    DEFINE #loc #integer
        i_delegap             INTEGER

    LET s_estadop = 0
    LET i_delegap = 0
{
    SELECT C.estad_cod,
           C.deleg_cod
    INTO   s_estadop ,
           i_delegap
    FROM   afi_mae_afiliado A, afi_domicilio B, tab_codpos C
    WHERE  A.n_seguro       = reg_5.n_seguro
    AND    A.n_seguro       = B.nss
    AND    A.n_folio        = B.n_folio
    AND    A.tipo_solicitud = B.tipo_solicitud
    AND    B.marca_envio    = "X"
    AND    B.codpos         = C.cpos_cod
}

    SELECT B.estado   ,
           B.delega    
    INTO   s_estadop ,
           i_delegap
    FROM   afi_mae_afiliado A, afi_domicilio B
    WHERE  A.n_seguro       = reg_5.n_seguro
    AND    A.n_seguro       = B.nss
    AND    A.n_folio        = B.n_folio
    AND    A.tipo_solicitud = B.tipo_solicitud
    AND    B.marca_envio    = "X"

    IF s_estadop = 0 OR s_estadop = "" THEN
        PROMPT "ESTADO INVALIDO EN DIRECCION DEL AFILIADO, NSS = : ",
               reg_5.n_seguro
        FOR CHAR ENTER
    END IF

    IF i_delegap = 0 OR i_delegap = "" THEN
        PROMPT "DELEGACION INVALIDA EN DIRECCION DEL AFILIADO, NSS = : ",
               reg_5.n_seguro FOR CHAR ENTER
    END IF

    LET reg_6.salario_minimo = 0

    SELECT B.monto_sm
    INTO   reg_6.salario_minimo
    FROM   tab_zona_geo A, tabsalario_minimo2 B
    WHERE  A.estad_cod = s_estadop
    AND    A.deleg_cod = i_delegap
    AND    A.zona_cod  = B.zona_cod 

    IF reg_6.salario_minimo = 0 OR reg_6.salario_minimo = "" THEN
        PROMPT "SALARIO MINIMO INVALIDO, NSS = ",reg_5.n_seguro
        FOR CHAR ENTER
        SELECT B.monto_sm
        INTO   reg_6.salario_minimo
        FROM   tabsalario_minimo2 B
        WHERE  B.zona_cod = 'C'
    END IF

    LET reg_6.semanas_cotiz = 0

    SELECT semanas_cotizadas
    INTO   reg_6.semanas_cotiz
    FROM   ret_resol_retiro
    WHERE  n_seguro    = reg_5.n_seguro
    AND    consecutivo = reg_5.consecutivo

    IF reg_6.semanas_cotiz = 0 OR reg_6.semanas_cotiz = "" THEN
        LET reg_6.mto_neto  = reg_5.monto_en_pesos
	LET reg_6.retencion = 0

        LET reg_6.retencion = reg_5.monto_en_pesos * 0.20
        LET reg_6.mto_neto  = reg_5.monto_en_pesos - reg_6.retencion

        PROMPT"SEMANAS COTIZADAS INVALIDAS, NSS = ",reg_5.n_seguro
        FOR CHAR ENTER

        RETURN reg_6.mto_neto, reg_6.retencion
   ELSE    
        LET s_resto        = reg_6.semanas_cotiz MOD 52
        LET reg_6.tot_anos = reg_6.semanas_cotiz / 52

        IF s_resto >= 26 THEN
            LET reg_6.tot_anos = reg_6.tot_anos + 1
        END IF
    
        LET s_ano_cotizacion = reg_5.n_seguro[3,4]

        IF reg_6.tot_anos >= 5 THEN
            IF s_ano_cotizacion > 92 THEN
                LET s_descuenta_anos = s_ano_cotizacion - 92
                LET reg_6.tot_anos = 5 - s_descuenta_anos
            ELSE
                LET reg_6.tot_anos = 5
            END IF
        END IF

        LET reg_6.mto_no_grabable = reg_6.tot_anos * reg_6.salario_minimo * 90

        IF reg_5.monto_en_pesos > reg_6.mto_no_grabable THEN
            LET reg_6.mto_grabable = reg_5.monto_en_pesos - reg_6.mto_no_grabable
            LET reg_6.retencion    = reg_6.mto_grabable * 0.20
            LET reg_6.mto_neto     = reg_5.monto_en_pesos - reg_6.retencion
        ELSE
            LET reg_6.retencion    = 0
            LET reg_6.mto_neto     = reg_5.monto_en_pesos
        END IF

        RETURN reg_6.mto_neto, reg_6.retencion
   END IF  
END FUNCTION

FUNCTION consulta_cheque(i_consecutivo)
#cc------------------------------------
    DEFINE arr_3 ARRAY[5] OF RECORD #loc #arr_3
        paterno               CHAR(30)     ,
        materno               CHAR(30)     ,
        nombres               CHAR(30)     ,
        porcentaje            DECIMAL(5,2) ,
        cod_plaza             SMALLINT
    END RECORD

    DEFINE #glo #integer
        pos_1                   ,
        i_consecutivo           INTEGER

    OPEN WINDOW retm0017 AT 7,3 WITH FORM "RETM0017" ATTRIBUTE(BORDER)
    DISPLAY "                       CONSULTA DE BENEFICIARIOS                               " AT 1,1 ATTRIBUTE(REVERSE)

        DECLARE cur_14 CURSOR FOR
        SELECT paterno    ,
               materno    ,
               nombres    ,
               porcentaje ,
               cod_plaza
        FROM   ret_cheque
        WHERE  consecutivo = i_consecutivo

        LET pos_1 = 1
        FOREACH cur_14 INTO arr_3[pos_1].*
            LET pos_1 = pos_1 + 1
        END FOREACH

        CALL SET_COUNT(pos_1)

        DISPLAY ARRAY arr_3 TO scr_7.*
            ON KEY (INTERRUPT)
                EXIT DISPLAY
            ON KEY (CONTROL-C)
                EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW retm0017
END FUNCTION

{
FUNCTION modifica_cheque(reg_3)
#mac---------------------------
    DEFINE reg_3 RECORD #loc #reg_3
        num_resolucion        CHAR(06) ,
        n_seguro              CHAR(11) ,
        tipo_prestacion       SMALLINT ,
        paterno               CHAR(40) ,
        materno               CHAR(40) ,
        nombres               CHAR(40) ,
        consecutivo           INTEGER
    END RECORD

    DEFINE arr_3 ARRAY[5] OF RECORD #loc #arr_3
        paterno               CHAR(30)     ,
        materno               CHAR(30)     ,
        nombres               CHAR(30)     ,
        porcentaje            DECIMAL(5,2) ,
        cod_plaza             SMALLINT
    END RECORD

    DEFINE #glo #smallint
        arr_c3                ,
        arr_c                 SMALLINT

    DEFINE #glo #integer
        pos_1                 ,
        i_consecutivo         INTEGER

    DEFINE #loc #decimal
        d_suma_porcentaje     DECIMAL(16,6)

    OPEN WINDOW retm0017 AT 8,6 WITH FORM "RETM0017" ATTRIBUTE(BORDER)
    DISPLAY "                     CAPTURA DE BENEFICIARIOS                                  " AT 1,1 ATTRIBUTE(REVERSE)

        DECLARE cur_15 CURSOR FOR
        SELECT paterno    ,
               materno    ,
               nombres    ,
               porcentaje ,
               cod_plaza
        FROM   ret_cheque
        WHERE  consecutivo = reg_3.consecutivo

        LET pos_1 = 1
        FOREACH cur_15 INTO arr_3[pos_1].*
            LET arr_4[pos_1].paterno    = arr_3[pos_1].paterno
            LET arr_4[pos_1].materno    = arr_3[pos_1].materno
            LET arr_4[pos_1].nombres    = arr_3[pos_1].nombres
            LET arr_4[pos_1].porcentaje = arr_3[pos_1].porcentaje
            LET arr_4[pos_1].cod_plaza  = arr_3[pos_1].cod_plaza

            LET pos_1 = pos_1 + 1
        END FOREACH
	LET arr_c3 = pos_1

        CALL SET_COUNT(pos_1)

        INPUT ARRAY arr_3 WITHOUT DEFAULTS FROM scr_7.*
            BEFORE FIELD paterno
                LET arr_c  = ARR_CURR()

            AFTER FIELD paterno
                IF arr_3[arr_c].paterno IS NULL THEN
                    ERROR "PATERNO NO PUEDE SER NULO"
                END IF

            AFTER FIELD materno

            AFTER FIELD nombres

            AFTER FIELD porcentaje
	        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
		    IF arr_c > 1 THEN
		        LET arr_c = arr_c -1
                    END IF
	        END IF

	        IF arr_3[arr_c].porcentaje <= 1 THEN
		    ERROR "PORCENTAJE NO PUEDE SER MENOR A 1"
		    NEXT FIELD porcentaje
	        END IF

	        IF arr_3[arr_c].porcentaje > 100 THEN
		    ERROR "PORCENTAJE NO PUEDE SER MAYOR A 100"
		    NEXT FIELD porcentaje
	        END IF

            ON KEY (ESC)
                IF  arr_3[arr_c].paterno    IS NULL
                AND arr_3[arr_c].materno    IS NULL
                AND arr_3[arr_c].nombres    IS NULL
                AND arr_3[arr_c].porcentaje IS NULL
                AND arr_3[arr_c].cod_plaza  IS NULL
                THEN
                ELSE
                    IF arr_3[arr_c].paterno IS NULL THEN
                        ERROR "PATERNO NO PUEDE SER NULO"
                        NEXT FIELD paterno
                    END IF
     
#                   IF arr_3[arr_c].materno IS NULL THEN
#                       ERROR "MATERNO NO PUEDE SER NULO"
#                       NEXT FIELD materno
#                   END IF
     
                    IF arr_3[arr_c].nombres IS NULL THEN
                        ERROR "NOMBRES NO PUEDE SER NULO"
                        NEXT FIELD nombres
                    END IF

                    IF arr_3[arr_c].porcentaje IS NULL THEN
                        ERROR "PORCENTAJE NO PUEDE SER NULO"
                        NEXT FIELD porcentaje
                    END IF
    
                    IF arr_3[arr_c].cod_plaza IS NULL THEN
                        ERROR "PLAZA NO PUEDE SER NULO"
                        NEXT FIELD cod_plaza
                    END IF
                END IF

	        IF arr_3[arr_c].porcentaje <= 1 THEN
		    ERROR "PORCENTAJE NO PUEDE SER MENOR A 1"
		    NEXT FIELD porcentaje
	        END IF

	        IF arr_3[arr_c].porcentaje > 100 THEN
		    ERROR "PORCENTAJE NO PUEDE SER MAYOR A 100"
		    NEXT FIELD porcentaje
	        END IF

                LET d_suma_porcentaje = 0
                FOR pos = 1 TO 5
		    IF arr_3[pos].porcentaje > 0 THEN
	                LET d_suma_porcentaje = d_suma_porcentaje +
					    arr_3[pos].porcentaje
		    END IF
                END FOR

                IF d_suma_porcentaje > 100 THEN
	            ERROR "SUMA EXCEDE EL 100 PORCIENTO"
		    NEXT FIELD porcentaje
                ELSE
                    IF d_suma_porcentaje < 100 THEN
		        ERROR "SUMA ES INFERIOR AL 100 PORCIENTO"
		        NEXT FIELD porcentaje
		    END IF
                END IF

                DELETE 
                FROM  ret_cheque
                WHERE consecutivo = reg_3.consecutivo

                FOR pos = 1 TO 5
                    IF arr_3[pos].porcentaje > 0 THEN
                        INSERT INTO ret_cheque
                            VALUES(reg_3.n_seguro        ,
                                   reg_3.tipo_prestacion ,
                                   arr_3[pos].paterno    ,
                                   arr_3[pos].materno    ,
                                   arr_3[pos].nombres    ,
                                   arr_3[pos].porcentaje ,
                                   arr_3[pos].cod_plaza  ,
                                   reg_3.consecutivo
                                  )
                    END IF
                END FOR

                EXIT INPUT

        ON KEY (INTERRUPT)
            EXIT INPUT

        ON KEY (CONTROL-C)
            EXIT INPUT

    END INPUT
    CLOSE WINDOW retm0017
END FUNCTION
}
FUNCTION consulta_orden_de_pago(i_consecutivo)
#codp-----------------------------------------
    DEFINE arr_7 ARRAY[5] OF RECORD #loc #arr_7
        paterno               LIKE ret_orden_pago.paterno        ,
        materno               LIKE ret_orden_pago.materno        ,
        nombres               LIKE ret_orden_pago.nombres        ,
        porcentaje            LIKE ret_orden_pago.porcentaje     ,
        monto_en_pesos        LIKE ret_orden_pago.monto_en_pesos ,
        cod_banco             LIKE ret_orden_pago.cod_banco      ,
        cod_ciudad            LIKE ret_orden_pago.cod_ciudad     ,
        cod_sucursal          LIKE ret_orden_pago.cod_sucursal
    END RECORD

    DEFINE #glo #integer
        pos_1                   ,
        i_consecutivo           INTEGER

    OPEN WINDOW glo_ret7 AT 6,5 WITH FORM "GLOB_RET7" ATTRIBUTE(BORDER)
    DISPLAY "                      CAPTURA DE BENEFICIARIOS                                 " AT 1,1 ATTRIBUTE(REVERSE)

        DECLARE cur_17 CURSOR FOR
        SELECT paterno        ,
               materno        ,
               nombres        ,
               porcentaje     ,
               monto_en_pesos ,
               cod_banco      ,
               cod_ciudad     ,
               cod_sucursal
        FROM   ret_orden_pago
        WHERE  consecutivo = i_consecutivo

        LET pos_1 = 1
        FOREACH cur_17 INTO arr_7[pos_1].*
            LET pos_1 = pos_1 + 1
        END FOREACH

        CALL SET_COUNT(pos_1)

        DISPLAY ARRAY arr_7 TO scr_7.*
            ON KEY (CONTROL-C)
                EXIT DISPLAY

            ON KEY (INTERRUPT)
                EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW glo_ret7
END FUNCTION

FUNCTION consulta_abono_cta(i_consecutivo)
#cac---------------------------------------
    DEFINE arr_3 ARRAY[5] OF RECORD #loc #arr_3
        paterno               LIKE ret_abono_cta.paterno        ,
        materno               LIKE ret_abono_cta.materno        ,
        nombres               LIKE ret_abono_cta.nombres        ,
        porcentaje            LIKE ret_abono_cta.porcentaje     ,
        monto_en_pesos        LIKE ret_abono_cta.monto_en_pesos ,
        cod_banco             LIKE ret_abono_cta.cod_banco      ,
        cod_ciudad            LIKE ret_abono_cta.cod_ciudad     ,
        cod_sucursasl         LIKE ret_abono_cta.cod_sucursal   ,
        nro_cuenta            LIKE ret_abono_cta.nro_cuenta
    END RECORD

    DEFINE #glo #integer
        pos_1                   ,
        i_consecutivo           INTEGER

    OPEN WINDOW glob_ret3 AT 6,5 WITH FORM "GLOB_RET3" ATTRIBUTE(BORDER)
    DISPLAY "                     CAPTURA DE BENEFICIARIOS                                  " AT 1,1 ATTRIBUTE(REVERSE)

        DECLARE cur_9 CURSOR FOR
        SELECT paterno        ,
               materno        ,
               nombres        ,
               porcentaje     ,
               monto_en_pesos ,
               cod_banco      ,
               cod_ciudad     ,
               cod_sucursal   ,
               nro_cuenta
        FROM   ret_abono_cta
        WHERE  consecutivo = i_consecutivo

        LET pos_1 = 1
        FOREACH cur_9 INTO arr_3[pos_1].*
            LET pos_1 = pos_1 + 1
        END FOREACH

        CALL SET_COUNT(pos_1)

        DISPLAY ARRAY arr_3 TO scr_3.*
            ON KEY (CONTROL-C)
                EXIT DISPLAY

            ON KEY (INTERRUPT)
                EXIT DISPLAY
        END DISPLAY
    CLOSE WINDOW glob_ret3
END FUNCTION

{
FUNCTION recupera_saldo (nss, subcuenta, fecha_hasta)
#rs---------------------------------------------------
    DEFINE #loc date
        fecha_tope 	      ,
        fecha_hasta	      DATE

    DEFINE #loc char
        anio_2  	      CHAR(02) ,
        anio_4  	      CHAR(04) ,
        nss                   CHAR(11) ,
        subcuenta             CHAR(02)

   DEFINE #loc #decimal
       monto_actual           DECIMAL(22,6) ,
       monto_acciones         DECIMAL(22,6) ,
       d11_precio_del_dia     DECIMAL(11,6) ,
       monto_pesos            DECIMAL(22,6)


   DEFINE comando_100      CHAR(200)
   DEFINE comando_200      CHAR(200)
   DEFINE comando_300      CHAR(200)
   DEFINE tabla		   CHAR(12)
   DEFINE valor_accion     DECIMAL(11,6)
   DEFINE reg_resultado    RECORD
          total_pesos      DECIMAL(22,6) ,
          total_acciones   DECIMAL(22,6) 
   END RECORD

   LET comando_100    = " "
   LET comando_200    = " "
   LET comando_300    = " "
   LET monto_pesos    = 0
   LET monto_acciones = 0
   LET monto_actual   = 0
   LET fecha_tope = fecha_hasta + 1
   LET anio_4 = YEAR(fecha_hasta)
   LET anio_2 = anio_4[3,4]
   LET tabla = "dis_cuenta", anio_2

# Checa si existe la tabla
   LET comando_100 = 'SELECT "OK" FROM ', tabla," GROUP BY 1 "
   PREPARE query100 FROM comando_100
   EXECUTE query100         

   IF STATUS = NOTFOUND THEN
      LET tabla = "dis_cuenta"
   END IF

# Hace el sum de los saldos sobre la tabla
   LET comando_200 = 
       'SELECT SUM(monto_en_pesos), SUM(monto_en_acciones) FROM ',
       tabla, ' WHERE nss = ', nss, ' AND subcuenta = ', subcuenta,
       ' AND fecha_conversion < "', fecha_tope, '"'
   PREPARE query200 FROM comando_200
   DECLARE cur_200  CURSOR FOR query200
   FOREACH cur_200  INTO reg_resultado.*
      LET monto_pesos    = reg_resultado.total_pesos    
      LET monto_acciones = reg_resultado.total_acciones
   END FOREACH

   SELECT precio_del_dia 
   INTO   d11_precio_del_dia
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = fecha_hasta

   LET monto_actual = monto_acciones * d11_precio_del_dia
   
   RETURN monto_pesos, monto_acciones, monto_actual

END FUNCTION
}

FUNCTION recupera_saldo (nss, subcuenta, fecha_hasta)
#rs---------------------------------------------------
    DEFINE #loc date
        fecha_hasta	      DATE

    DEFINE #loc char
        nss                   CHAR(11) ,
        subcuenta             CHAR(2)

   DEFINE monto_pesos      DECIMAL(22,6)
   DEFINE monto_acciones   DECIMAL(22,6)
   DEFINE monto_actual     DECIMAL(22,6)

   DEFINE fecha_tope 	   DATE
   DEFINE anio_4  	   CHAR(4)
   DEFINE valor_accion     DECIMAL(11,6)

   --WHENEVER ERROR CONTINUE  

   LET monto_pesos    = 0
   LET monto_acciones = 0
   LET monto_actual   = 0
   LET fecha_tope = fecha_hasta + 1
   LET anio_4 = YEAR(fecha_hasta)

   IF  anio_4 = "1997"             THEN
       SELECT SUM(a.monto_en_pesos), SUM(a.monto_en_acciones)
       INTO   monto_pesos,         monto_acciones
       FROM   dis_cuenta97 a
       WHERE  a.nss              = nss
       AND    a.subcuenta        = subcuenta
       AND    a.fecha_conversion < fecha_tope
   ELSE  
       IF  anio_4 = "1998"             THEN
           SELECT SUM(a.monto_en_pesos), SUM(a.monto_en_acciones)
           INTO   monto_pesos,         monto_acciones
           FROM   dis_cuenta98 a
           WHERE  a.nss              = nss
           AND    a.subcuenta        = subcuenta
           AND    a.fecha_conversion < fecha_tope
       ELSE  
           IF  anio_4 = "1999"             THEN
               SELECT SUM(a.monto_en_pesos), SUM(a.monto_en_acciones)
               INTO   monto_pesos,         monto_acciones
               FROM   dis_cuenta99 a
               WHERE  a.nss              = nss
               AND    a.subcuenta        = subcuenta
               AND    a.fecha_conversion < fecha_tope
           ELSE  
               IF  anio_4 = "2000"             THEN
                   SELECT SUM(a.monto_en_pesos), SUM(a.monto_en_acciones)
                   INTO   monto_pesos, monto_acciones
                   FROM   dis_cuenta00 a
                   WHERE  a.nss              = nss
                   AND    a.subcuenta        = subcuenta
                   AND    a.fecha_conversion <= fecha_hasta
               ELSE   
                   SELECT SUM(a.monto_en_pesos), SUM(a.monto_en_acciones)
                   INTO   monto_pesos,         monto_acciones
                   FROM   dis_cuenta a
                   WHERE  a.nss              = nss
                   AND    a.subcuenta        = subcuenta
                   AND    a.fecha_conversion < fecha_tope
               END IF
           END IF
       END IF
   END IF

   LET valor_accion = 1
   IF monto_acciones IS NULL OR monto_acciones = " " THEN
       LET monto_acciones = 0
   END IF
   IF monto_pesos IS NULL OR monto_pesos = " " THEN
       LET monto_pesos = 0
   END IF
   LET monto_actual = monto_acciones * valor_accion

   RETURN monto_pesos, monto_acciones, monto_actual

END FUNCTION

FUNCTION recupera_saldo2(nss, subcuenta, fecha_hasta)
#rs---------------------------------------------------
    DEFINE #loc date
        fecha_hasta	      DATE

    DEFINE #loc char
        nss                   CHAR(11) ,
        subcuenta             CHAR(2)

   DEFINE monto_pesos      DECIMAL(22,6)
   DEFINE monto_acciones   DECIMAL(22,6)
   DEFINE monto_actual     DECIMAL(22,6)

   DEFINE fecha_tope 	   DATE
   DEFINE anio_4  	   CHAR(4)
   DEFINE valor_accion     DECIMAL(11,6)

   --WHENEVER ERROR CONTINUE  

   LET monto_pesos    = 0
   LET monto_acciones = 0
   LET monto_actual   = 0
--   LET fecha_tope = fecha_hasta + 1
   LET anio_4 = YEAR(fecha_hasta)

   IF  anio_4 = "1997"             THEN
       SELECT SUM(a.monto_en_acciones)
       INTO   monto_acciones
       FROM   dis_cuenta97 a
       WHERE  a.nss              = nss
       AND    a.subcuenta        = subcuenta
       AND    a.fecha_valor <= fecha_hasta        
   ELSE  
       IF  anio_4 = "1998"             THEN
           SELECT SUM(a.monto_en_acciones)
           INTO   monto_acciones
           FROM   dis_cuenta98 a
           WHERE  a.nss              = nss
           AND    a.subcuenta        = subcuenta
           AND    a.fecha_valor <= fecha_hasta        
       ELSE  
           IF  anio_4 = "1999"             THEN
               SELECT SUM(a.monto_en_acciones)
               INTO   monto_acciones
               FROM   dis_cuenta99 a
               WHERE  a.nss              = nss
               AND    a.subcuenta        = subcuenta
               AND    a.fecha_valor <= fecha_hasta        
           ELSE  
               IF  anio_4 = "2000"             THEN
                   SELECT SUM(a.monto_en_acciones)
                   INTO   monto_acciones
                   FROM   dis_cuenta00 a
                   WHERE  a.nss              = nss
                   AND    a.subcuenta        = subcuenta
                   AND    a.fecha_valor <= fecha_hasta        
               ELSE   
                   SELECT SUM(a.monto_en_acciones)
                   INTO   monto_acciones
                   FROM   dis_cuenta a
                   WHERE  a.nss              = nss
                   AND    a.subcuenta        = subcuenta
                   AND    a.fecha_valor <= fecha_hasta        
               END IF
           END IF
       END IF
   END IF

   LET valor_accion = 1
   IF monto_acciones IS NULL OR monto_acciones = " " THEN
       LET monto_acciones = 0
   END IF
{
   IF monto_pesos IS NULL OR monto_pesos = " " THEN
       LET monto_pesos = 0
   END IF
}
--   LET monto_actual = monto_acciones * valor_accion
   RETURN monto_acciones

END FUNCTION

FUNCTION suma_dias_cotizados(nss, subcuenta, fecha_hasta)
#rs---------------------------------------------------
    DEFINE #loc date
        fecha_hasta	      DATE

    DEFINE #loc char
        nss                   CHAR(11) ,
        subcuenta             CHAR(2)  ,
        dias_cotizados        INTEGER  ,
        dias_cotizados_97     INTEGER  ,
        dias_cotizados_98     INTEGER  ,
        dias_cotizados_99     INTEGER  ,
        dias_cotizados_00     INTEGER  ,
        dias_cotizados_01     INTEGER


   LET dias_cotizados = 0

   SELECT SUM(a.dias_cotizados)
   INTO   dias_cotizados_97
   FROM   dis_cuenta97 a
   WHERE  a.nss              = nss
   AND    a.subcuenta        = subcuenta
   AND    a.fecha_valor <= fecha_hasta        

   IF dias_cotizados_97 IS NULL OR dias_cotizados_97 = " " THEN
       LET dias_cotizados_97 = 0
   END IF

   SELECT SUM(a.dias_cotizados)
   INTO   dias_cotizados_98
   FROM   dis_cuenta98 a
   WHERE  a.nss              = nss
   AND    a.subcuenta        = subcuenta
   AND    a.fecha_valor <= fecha_hasta        

   IF dias_cotizados_98 IS NULL OR dias_cotizados_98 = " " THEN
       LET dias_cotizados_98 = 0
   END IF
   SELECT SUM(a.dias_cotizados)
   INTO   dias_cotizados_99
   FROM   dis_cuenta99 a
   WHERE  a.nss              = nss
   AND    a.subcuenta        = subcuenta
   AND    a.fecha_valor <= fecha_hasta        

   IF dias_cotizados_99 IS NULL OR dias_cotizados_99 = " " THEN
       LET dias_cotizados_99 = 0
   END IF
   SELECT SUM(a.dias_cotizados)
   INTO   dias_cotizados_00
   FROM   dis_cuenta00 a
   WHERE  a.nss              = nss
   AND    a.subcuenta        = subcuenta
   AND    a.fecha_valor <= fecha_hasta        

   IF dias_cotizados_00 IS NULL OR dias_cotizados_00 = " " THEN
       LET dias_cotizados_00 = 0
   END IF
   SELECT SUM(a.dias_cotizados)
   INTO   dias_cotizados_01
   FROM   dis_cuenta a
   WHERE  a.nss              = nss
   AND    a.subcuenta        = subcuenta
   AND    a.fecha_valor <= fecha_hasta        

   IF dias_cotizados_01 IS NULL OR dias_cotizados_01 = " " THEN
       LET dias_cotizados_01 = 0
   END IF

   LET dias_cotizados = dias_cotizados_97 +
                        dias_cotizados_98 +
                        dias_cotizados_99 +
                        dias_cotizados_00 +
                        dias_cotizados_01

   RETURN dias_cotizados

END FUNCTION

FUNCTION valida_combina(tipo_seguro,tipo_pension,tipo_presta)
#vc---------------------------------------------------------
    DEFINE
        tipo_seguro           CHAR(2)  ,
        tipo_pension          CHAR(2)  ,
        tipo_presta           SMALLINT ,
        vcombina              SMALLINT       

   LET vcombina = 0

   SELECT UNIQUE "X" FROM ret_matriz A
   WHERE A.tipo_seguro  = tipo_seguro
   AND   A.tipo_pension = tipo_pension
   AND   A.tipo_prestacion  = tipo_presta
   IF STATUS = NOTFOUND THEN
      LET vcombina = 0
   ELSE
      LET vcombina = 1
   END IF
   RETURN vcombina
END FUNCTION

FUNCTION num_dias_mes(mes)
#ndm----------------------
    DEFINE 
        mes       CHAR(02) ,
        num_dias  CHAR(02)

    CASE mes
        WHEN "01"  
            LET num_dias = 31
        WHEN "02"  
            LET num_dias = 28
        WHEN "03"  
            LET num_dias = 31
        WHEN "04"  
            LET num_dias = 30
        WHEN "05"  
            LET num_dias = 31
        WHEN "06"  
            LET num_dias = 30
        WHEN "07"  
            LET num_dias = 31
        WHEN "08"  
            LET num_dias = 31
        WHEN "09"  
            LET num_dias = 30
        WHEN "10" 
            LET num_dias = 31
        WHEN "11" 
            LET num_dias = 30
        WHEN "12" 
            LET num_dias = 31
    END CASE
RETURN num_dias
END FUNCTION
{
Function inhabilita(reg_1)
#fh-----------------------
    DEFINE
        precio_accion         LIKE glo_valor_accion.precio_del_dia  

    DEFINE reg_1 RECORD #glo #reg_1
        folio                 INTEGER  ,
        n_seguro              CHAR(11) ,
	regimen               CHAR(02) ,
        tipo_seguro           CHAR(02) ,
        tipo_pension          CHAR(02) ,
        tipo_prestacion       SMALLINT ,
        consecutivo           INTEGER
    END RECORD

    DEFINE reg_2 RECORD #glo #reg_2
        tipo_movimiento       INTEGER       ,
        acciones              DECIMAL(16,6) ,
        pesos_viv             DECIMAL(16,6)
    END RECORD

    DEFINE #char
        HORA                  CHAR(0008)
    

    LET HOY  = TODAY
    LET HORA = TIME

    SELECT A.precio_del_dia
    INTO   precio_accion
    FROM   glo_valor_accion A
    WHERE  A.fecha_valuacion = HOY

    LET reg_2.acciones = 0

    SELECT SUM(monto_en_acciones)
    INTO   reg_2.acciones
    FROM   dis_cuenta
    WHERE  nss             = reg_1.n_seguro
    AND    subcuenta  NOT IN(4,8)
    AND    tipo_movimiento > 0

    IF reg_2.acciones IS NULL THEN
        LET reg_2.acciones = 0
    END IF

    LET reg_2.pesos_viv = 0

    SELECT SUM(monto_en_pesos)
    INTO   reg_2.pesos_viv
    FROM   dis_cuenta
    WHERE  nss             =  reg_1.n_seguro
    AND    subcuenta       IN (4,8)
    AND    tipo_movimiento <> 888

    IF reg_2.pesos_viv IS NULL THEN
        LET reg_2.pesos_viv = 0
    END IF

    IF reg_2.acciones <= 0 AND reg_2.pesos_viv <= 0 THEN
        INSERT INTO safre_tmp:borrar values(reg_1.n_seguro  ,
                             	            reg_2.pesos_viv ,
					    reg_2.acciones
					   )

	#ff
        CALL tipo_movimiento2(reg_1.regimen         ,
			      reg_1.tipo_seguro     ,
                              reg_1.tipo_pension    ,
                              reg_1.tipo_prestacion 
                              ) #tm2
        RETURNING reg_2.tipo_movimiento


        --Si la cuenta ya fue inhabilitada no debe de inhabilitar-----------
        SELECT "X" 
	FROM   cta_act_marca 
        WHERE  nss       = reg_1.n_seguro
        AND    marca_cod = 5                      
	GROUP BY 1
               
	IF STATUS = NOTFOUND THEN
            SELECT "X"
            FROM   cta_ctr_cuenta
            WHERE  nss = reg_1.n_seguro

            IF STATUS <> NOTFOUND THEN      
                UPDATE cta_ctr_cuenta
                SET    estado_cuenta     = 5                     ,
	               fecha_edo_cuenta  = HOY                   ,     
                       marca_cod         = reg_2.tipo_movimiento ,
                       activo_marca      = 1                     ,
                       fecha_act_marca   = HOY                   ,     
		       tipo_informe      = 5                     ,
		       fecha_informe     = HOY
                WHERE  nss = reg_1.n_seguro

                INSERT INTO cta_act_marca 
		VALUES(reg_1.n_seguro        ,#nss         char(11)
                       5                     ,#marca_cod   smallint
                       HOY                   ,#fecha_ini   date
                       HORA                  ,#hora_ini    datetime
                       0                     ,#estado      smallint
                       reg_2.tipo_movimiento ,#marca_causa smallint
                       HOY                   ,#fecha_causa date
                       reg_1.consecutivo     ,#correlativo integer
                       USER                   #usuario     char(8)
                      )

                INSERT INTO cta_his_marca 
		VALUES(reg_1.n_seguro        ,#nss         char(11)
                       5                     ,#marca_cod   smallint
                       HOY                   ,#fecha_ini   date
		       NULL                  ,#fecha_fin   date
                       HORA                  ,#hora_ini    datetime
                       0                     ,#estado      smallint
		       0                     ,#rechazo_cod smallint
                       reg_2.tipo_movimiento ,#marca_causa smallint
                       HOY                   ,#fecha_causa date
                       reg_1.consecutivo     ,#correlativo integer
                       USER                   #usuario     char(8)
                      )

                INSERT INTO ret_his_inhabilita
		VALUES (reg_1.folio           ,
                        reg_1.n_seguro        ,
                        reg_2.tipo_movimiento ,
                        reg_2.acciones        ,
                        reg_2.pesos_viv       ,
                        HOY                   ,
                        HOY                   ,
			reg_1.consecutivo     ,
			1                     -- inhabilito
		       )
            ELSE
                INSERT INTO ret_his_inhabilita
		VALUES (reg_1.folio           ,
                        reg_1.n_seguro        ,
                        reg_2.tipo_movimiento ,
                        reg_2.acciones        ,
                        reg_2.pesos_viv       ,
                        HOY                   ,
                        HOY                   ,
			reg_1.consecutivo     ,
			3                     -- No existe registro en
		       )                      -- cta_ctr_cuenta
	    END IF
	ELSE
            INSERT INTO ret_his_inhabilita
	    VALUES (reg_1.folio           ,
                    reg_1.n_seguro        ,
                    reg_2.tipo_movimiento ,
                    reg_2.acciones        ,
                    reg_2.pesos_viv       ,
                    HOY                   ,
                    HOY                   ,
		    reg_1.consecutivo     ,
		    2                     -- No inhabilito, ya se encontraba
		   )                      -- inhabilitado
        END IF
        --------------------------------------------------------------------
    END IF
END FUNCTION

FUNCTION tipo_movimiento2(reg_1)
#tm2----------------------------
    DEFINE reg_1 RECORD
	regimen               LIKE ret_matriz.regimen         ,
        tipo_seguro           LIKE ret_matriz.tipo_seguro     ,
        tipo_pension          LIKE ret_matriz.tipo_pension    ,
        tipo_prestacion       LIKE ret_matriz.tipo_prestacion
    END RECORD

    DEFINE #integer
	i_tipo_movimiento     INTEGER,
        regi                  LIKE ret_matriz.regimen
 
    IF  reg_1.tipo_seguro     = " " AND
        reg_1.tipo_pension    = " " AND
        reg_1.tipo_prestacion = " "  
    THEN
        DECLARE curtip CURSOR FOR
        SELECT tipo_movimiento,regimen
        FROM   ret_matriz
        WHERE  tipo_seguro     IS NULL
        AND    tipo_pension    IS NULL
        AND    tipo_prestacion IS NULL
        FOREACH curtip INTO i_tipo_movimiento, regi
           IF regi = reg_1.regimen THEN
              EXIT FOREACH 
           END IF           
        END FOREACH
    ELSE
        DECLARE curti2 CURSOR FOR
        SELECT tipo_movimiento,regimen
        FROM   ret_matriz
        WHERE  tipo_seguro     = reg_1.tipo_seguro
        AND    tipo_pension    = reg_1.tipo_pension
        AND    tipo_prestacion = reg_1.tipo_prestacion
        FOREACH curti2 INTO i_tipo_movimiento, regi
           IF regi = reg_1.regimen THEN
              EXIT FOREACH 
           END IF           
        END FOREACH
    END IF

    IF STATUS = NOTFOUND THEN
	RETURN 0
    ELSE 
	RETURN i_tipo_movimiento
    END IF
END FUNCTION
}
FUNCTION imprime_reporte_inhabilita(folio_op,tipo_retiro)
#iri----------------------------------------------------

    DEFINE reg_3 RECORD #loc #reg_3
        folio                 INTEGER       ,
        n_seguro              CHAR(11)      ,
	nombre                CHAR(50)      ,
        tipo_movimiento       INTEGER       ,
        acciones              DECIMAL(16,6) ,
        pesos_viv             DECIMAL(16,6) ,
        fech_val_acc          DATE          ,   
        consecutivo           INTEGER 
    END RECORD

    DEFINE 
	precio_accion         LIKE glo_valor_accion.precio_del_dia   

    DEFINE #integer
	folio_op                    ,
        reg_procesados        INTEGER

    DEFINE #smallint
	tipo_retiro  SMALLINT

    DEFINE #char(40) 
	vnombres              ,
	vpaterno              , 
	vmaterno     CHAR(40) ,
	lp                    ,
        G_LISTA      CHAR(100),
        ch           CHAR(110),
        afore_local  CHAR(03) ,
        HORA         CHAR(05)

    SELECT *
      INTO g_ret_parametro.*
      FROM ret_parametro

    SELECT codigo_afore
      INTO afore_local
      FROM tab_afore_local

    SELECT *
      INTO g_lp_impresoras.*
      FROM tab_cmd_impresora
     WHERE codigo_afore = afore_local 

    SELECT A.precio_del_dia
      INTO precio_accion
      FROM glo_valor_accion A
     WHERE A.fecha_valuacion = HOY

    LET HORA = TIME

    LET G_LISTA = g_ret_parametro.ruta_spool CLIPPED,"/",
		 HOY USING "DDMMYYYY",".",tipo_retiro USING "&&&",".",HORA

    LET lp = g_lp_impresoras.comando_impresion,G_LISTA

    DECLARE cur_rep CURSOR FOR 
    SELECT folio             , 
           n_seguro          ,
           tipo_movimiento   , 
           acciones          ,
           pesos_viv         ,
           fech_val_acc      , 
           consecutivo         
    FROM   ret_his_inhabilita   
    WHERE  @folio = folio_op   
    AND    @status = 1

    START REPORT listado_100 TO G_LISTA

    LET reg_procesados = 0
    FOREACH cur_rep INTO reg_3.folio, 
                       reg_3.n_seguro,
                       reg_3.tipo_movimiento,
                       reg_3.acciones,
                       reg_3.pesos_viv,
                       reg_3.fech_val_acc,      
                       reg_3.consecutivo       

        LET reg_procesados = reg_procesados + 1      
        LET reg_3.acciones = reg_3.acciones * precio_accion

        SELECT nombres,paterno,materno 
	INTO   vnombres , vpaterno , vmaterno
        FROM   afi_mae_afiliado
       --FROM   ret_resol_retiro
	WHERE  n_seguro = reg_3.n_seguro
       -- AND    consecutivo = reg_3.consecutivo 

	LET reg_3.nombre = vpaterno CLIPPED ," ",vmaterno CLIPPED," ",vnombres CLIPPED


        OUTPUT TO REPORT listado_100(reg_3.folio           ,
                                     reg_3.n_seguro        ,
                                     reg_3.nombre          ,
                                     reg_3.tipo_movimiento ,
                                     reg_3.acciones        ,
                                     reg_3.pesos_viv       ,
                                     reg_3.fech_val_acc    , 
                                     reg_3.consecutivo      
                                     )
        INITIALIZE reg_3.* TO NULL
    END FOREACH
    FINISH REPORT listado_100
    LET ch = "chmod 777 ",G_LISTA
    RUN ch
    RUN lp
END FUNCTION 

FUNCTION imprime_reporte_inhabilita2(folio_op,tipo_retiro)
#iri----------------------------------------------------

    DEFINE reg_3 RECORD #loc #reg_3
        folio                 INTEGER       ,
        n_seguro              CHAR(11)      ,
	nombre                CHAR(50)      ,
        tipo_movimiento       INTEGER       ,
        acciones              DECIMAL(16,6) ,
        pesos_viv             DECIMAL(16,6) ,
        fech_val_acc          DATE          ,   
        consecutivo           INTEGER 
    END RECORD

    DEFINE 
	precio_accion         LIKE glo_valor_accion.precio_del_dia   

    DEFINE #integer
	folio_op                    ,
        reg_procesados        INTEGER

    DEFINE #smallint
	tipo_retiro  SMALLINT

    DEFINE #char(40) 
	vnombres              ,
	vpaterno              , 
	vmaterno     CHAR(40) ,
	lp                    ,
        G_LISTA      CHAR(100),
        ch           CHAR(110),
        afore_local  CHAR(03) ,
        HORA         CHAR(05)

    SELECT *
    INTO   g_ret_parametro.*
    FROM   ret_parametro

    SELECT codigo_afore
    INTO   afore_local
    FROM   tab_afore_local

    SELECT *
    INTO   g_lp_impresoras.*
    FROM   tab_cmd_impresora
    WHERE  codigo_afore = afore_local 

    SELECT A.precio_del_dia
    INTO   precio_accion
    FROM   glo_valor_accion A
    WHERE  A.fecha_valuacion = HOY

    LET HORA = TIME

    LET G_LISTA = g_ret_parametro.ruta_spool CLIPPED,"/",
		 HOY USING "DDMMYYYY",".",tipo_retiro USING "&&&",".",HORA

    LET lp = g_lp_impresoras.comando_impresion CLIPPED," ",G_LISTA

    DECLARE cur_rep2 CURSOR FOR 
    SELECT folio           , 
           n_seguro        ,
           tipo_movimiento , 
           acciones        ,
           pesos_viv       ,
           fech_val_acc    , 
           consecutivo         
    FROM   ret_his_inhabilita   
    WHERE  @folio  = folio_op   
    AND    @status = 1

    START REPORT listado_100 TO G_LISTA

    LET reg_procesados = 0
    FOREACH cur_rep2 INTO reg_3.folio, 
                       reg_3.n_seguro,
                       reg_3.tipo_movimiento,
                       reg_3.acciones,
                       reg_3.pesos_viv,
                       reg_3.fech_val_acc,      
                       reg_3.consecutivo       

        LET reg_procesados = reg_procesados + 1      
        LET reg_3.acciones = reg_3.acciones * precio_accion

        SELECT nombres,paterno,materno 
	INTO   vnombres , vpaterno , vmaterno
        FROM   afi_mae_afiliado
       --FROM   ret_resol_retiro
	WHERE  n_seguro = reg_3.n_seguro
       -- AND    consecutivo = reg_3.consecutivo 

	LET reg_3.nombre = vpaterno CLIPPED ," ",vmaterno CLIPPED," ",vnombres CLIPPED


        OUTPUT TO REPORT listado_100(reg_3.folio           ,
                                     reg_3.n_seguro        ,
                                     reg_3.nombre          ,
                                     reg_3.tipo_movimiento ,
                                     reg_3.acciones        ,
                                     reg_3.pesos_viv       ,
                                     reg_3.fech_val_acc    , 
                                     reg_3.consecutivo      
                                     )
        INITIALIZE reg_3.* TO NULL
    END FOREACH
    FINISH REPORT listado_100
    LET ch = "chmod 777 ",G_LISTA
    RUN ch

    RETURN lp, reg_procesados

END FUNCTION 


REPORT listado_100(reg_3)
#l1--------------------
    DEFINE reg_3 RECORD #loc #reg_3
        folio                 INTEGER       ,
        n_seguro              CHAR(11)      ,
	nombre                CHAR(50)      ,
        tipo_movimiento       INTEGER       ,
        acciones              DECIMAL(8,2) ,
        pesos_viv             DECIMAL(8,2) ,
        fech_val_acc          DATE          ,   
        consecutivo           INTEGER 
    END RECORD

    DEFINE L1               CHAR(01)
    DEFINE L5               CHAR(05)
    DEFINE L10              CHAR(10)
    DEFINE L11              CHAR(11)
    DEFINE L18              CHAR(18)                      


    OUTPUT
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0
        PAGE LENGTH  90

    FORMAT

    PAGE HEADER
 
    LET L1  = "\304"
    LET L5  = "\304\304\304\304\304"
    LET L10 = "\304\304\304\304\304\304\304\304\304\304"
    LET L11 = "\304\304\304\304\304\304\304\304\304\304\304"
    LET L18 =
    "\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304"

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'

        PRINT COLUMN 42,"S U B D I R E C C I O N  D E  B E N E F I C I O S"

        SKIP 1 LINES

        PRINT COLUMN 42,"              CUENTAS INHABILITADAS              "

        SKIP 2 LINES

        PRINT COLUMN 105,"PROG.    : RETC023"
        PRINT
        PRINT COLUMN 105,"PAGINA   :    ",PAGENO USING "####"
        PRINT
        PRINT COLUMN 105,"FECHA : ", TODAY USING "DD/MM/YYYY"

    PRINT
        COLUMN 001,"\332",L10,L1,"\302",L10,L10,L10,L10,L5,L1,L1,L1,L1,"\302",
                    L10,"\302",L1,L1,L1,L1,L10,L10,L10,L10,L1,L1,L1,
                    "\277"
        PRINT
            COLUMN 001,"|"                            ,
            COLUMN 013,"|"                            ,
            COLUMN 063,"|"                            ,
            COLUMN 074,"|"                            ,
            COLUMN 082,"  T O T A L  E N  P E S O S " ,
            COLUMN 122,"|"                     
        PRINT
            COLUMN 001,"|"                            ,
            COLUMN 013,"|"                            ,
            COLUMN 063,"|"                            ,
            COLUMN 074,"|"                            ,
            COLUMN 122,"|"                     
        PRINT
            COLUMN 001,"|"                     ,
            COLUMN 006,"NSS"                   ,
            COLUMN 013,"|"                     ,
            COLUMN 027,"NOMBRE DEL TRABAJADOR" ,
            COLUMN 063,"|"                     ,
            COLUMN 064,"TIPO MVTO."            ,
            COLUMN 074,"|"                     ,
            COLUMN 085,"RCV SAR VOL "          ,
            COLUMN 098,"  VIVIENDA  "          ,
            COLUMN 122,"|"                     

        PRINT
            COLUMN 001,"\300",L10,L1,"\301",L10,L10,L10,L10,L5,L1,L1,L1,L1,
                       "\301",L10,"\301",L1,L1,L1,L1,L10,L10,L10,L10,L1,L1,L1,
                       "\331"


    BEFORE GROUP OF reg_3.folio
        PRINT
        PRINT
        PRINT
            COLUMN 002,"FOLIO  :",reg_3.folio
        SKIP 1 LINES

    ON EVERY ROW
        PRINT
        PRINT
            COLUMN 002,reg_3.n_seguro                         ,
            COLUMN 014,reg_3.nombre                           ,
            COLUMN 064,reg_3.tipo_movimiento USING "###"      ,
            COLUMN 080,reg_3.acciones        ,        
            COLUMN 098,reg_3.pesos_viv       
          
END REPORT


FUNCTION determina_fech_compara(HOY,dias_correr)
#dfc-------------------------------------------
    DEFINE #date
         HOY                ,
         d_fecha_fin    DATE

    DEFINE #char
         c10_fecha_fin  CHAR(10),
         ult_dia_mes    CHAR(02)

    DEFINE #integer
         dias_correr    INTEGER

         IF MONTH(HOY)  =  01   THEN
             IF DAY(HOY) = 01 THEN
                 LET c10_fecha_fin = '12/31/',YEAR(HOY) - 1
                 LET d_fecha_fin   = c10_fecha_fin
                 CALL habil_anterior(d_fecha_fin,1) RETURNING d_fecha_fin
             ELSE
                 CALL habil_anterior(HOY,dias_correr)
                      RETURNING d_fecha_fin
             END IF
         ELSE
             IF DAY(HOY) = 01 THEN
                 CALL num_dias_mes(MONTH(HOY)-1) RETURNING ult_dia_mes
                 LET c10_fecha_fin = MDY(MONTH(HOY)-1,ult_dia_mes,YEAR(HOY))
                 LET d_fecha_fin   = c10_fecha_fin
                 CALL habil_anterior(d_fecha_fin,1) RETURNING d_fecha_fin
             ELSE
                 CALL habil_anterior(HOY,dias_correr)
                      RETURNING d_fecha_fin
             END IF
         END IF

    RETURN d_fecha_fin
END FUNCTION


FUNCTION habil_anterior(diaActual,num_dia)
#ha---------------------------------------
    DEFINE #smallint
        cont_1                 ,
        feriado                ,
        finSemana              ,
        diaSemana              ,
        contador               ,
        num_dia                SMALLINT

    DEFINE #date
        diaActual              ,
        diaHabilAnt            ,
        diaTmp                 DATE

    LET cont_1      = 0
   #LET diaHabilAnt = diaActual - 1 UNITS DAY
    LET diaHabilAnt = diaActual

    WHILE TRUE
        LET feriado   = 0
        LET finSemana = 0
        LET diaSemana = WEEKDAY(diaHabilAnt)

        IF diaSemana = 0 OR diaSemana = 6 THEN
            LET finSemana = 1
        ELSE
            SELECT *
            FROM   tab_feriado
            WHERE  feria_fecha = diaHabilAnt

            IF STATUS <> NOTFOUND THEN
                LET feriado = 1
            END IF
        END IF

        IF feriado = 1 OR finSemana = 1 THEN
            LET diaHabilAnt = diaHabilAnt - 1 UNITS DAY
        ELSE
            LET cont_1      = cont_1 + 1

            IF cont_1 = num_dia THEN
                EXIT WHILE
            ELSE
                LET diaHabilAnt = diaHabilAnt - 1 UNITS DAY
            END IF
        END IF

    END WHILE

    RETURN diaHabilAnt
END FUNCTION



