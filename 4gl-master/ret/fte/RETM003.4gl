################################################################################
#Proyecto          => SISTEMA DE AFORES.( MEXICO )                             #
#Owner             => E.F.P                                                    #
#Programa RETM003  => MANTENEDOR DE RETIROS VOLUNTARIOS                        #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha creacion    => 23 ENERO DE 1998                                         #
#Actualizacion     => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha actualiz.   => 15 DE MAYO DEL 2002                                      #
#Sistema           => RET                                                      #
#Re-Actualizado    => 27 DE JUNIO DEL 2008
#By                => JRC & SDVP
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE
        acciones           LIKE dis_cuenta.monto_en_acciones ,
        pesos              LIKE dis_cuenta.monto_en_pesos    ,
        monto_paso         LIKE dis_cuenta.monto_en_pesos

    DEFINE reg_3 RECORD #glo #reg_3
        precapturado       ,
        capturado          ,
	    liquidado          ,
		rechazado          SMALLINT
    END RECORD

    DEFINE reg_1 RECORD #glo #reg_1
        n_folio               LIKE afi_mae_afiliado.n_folio  ,
        n_seguro              LIKE afi_mae_afiliado.n_seguro ,
        n_rfc                 LIKE afi_mae_afiliado.n_rfc    ,
        n_unico               LIKE afi_mae_afiliado.n_unico  ,
        fentcons              LIKE afi_mae_afiliado.fentcons ,
        paterno               LIKE afi_mae_afiliado.paterno  ,
        materno               LIKE afi_mae_afiliado.materno  ,
        nombres               LIKE afi_mae_afiliado.nombres 
    END RECORD

    DEFINE reg_2_array ARRAY[5000] OF RECORD #glo #reg_2_array
        var_nula              CHAR(1),
        n_folio               LIKE afi_mae_afiliado.n_folio            ,
        n_seguro              LIKE afi_mae_afiliado.n_seguro           ,
        n_rfc                 LIKE afi_mae_afiliado.n_rfc              ,
        n_unico               LIKE afi_mae_afiliado.n_unico            ,
        fentcons              LIKE afi_mae_afiliado.fentcons           ,
        paterno               LIKE afi_mae_afiliado.paterno            ,
        materno               LIKE afi_mae_afiliado.materno            ,
        nombres               LIKE afi_mae_afiliado.nombres            ,
        monto_en_acciones_sb1 LIKE dis_cuenta.monto_en_acciones        ,
        monto_en_acciones_sb3 LIKE dis_cuenta.monto_en_acciones        ,
        monto_en_pesos        LIKE dis_cuenta.monto_en_pesos           ,
        fecha_ult_ret         LIKE ret_cta_vol.fecha_ult_ret           ,
        n_folio_sol           LIKE ret_cta_vol.n_folio_sol             ,
        tipo_ret              LIKE ret_cta_vol.tipo_ret                ,
        des_tipo_ret          CHAR(40)                                 ,
        mto_solic             LIKE ret_cta_vol.mto_solic               ,
        porcentaje_solic      LIKE ret_cta_vol.porcentaje_solic        ,
        tipo_pago             LIKE ret_cta_vol.tipo_pago               ,
        edad                  LIKE ret_cta_vol.edad                    ,
        des_tipo_pago         CHAR(40)                                 ,
        fecha_solic           LIKE ret_cta_vol.fecha_solic             ,
        fecha_captura         LIKE ret_cta_vol.fecha_captura           ,
        ultimo_proceso        LIKE ret_cta_vol.ultimo_proceso          ,
        pension_invalidez     LIKE ret_cta_vol.pension_invalidez       ,
        deduccion             LIKE ret_cta_vol.deduccion               ,
        fecha_deduccion       LIKE ret_cta_vol.fecha_deduccion         ,
        mto_deducido          LIKE ret_cta_vol.mto_deducido            ,
		  cod_rechazo_ent			LIKE ret_cta_vol.cod_rechazo_ent,   #* Rech. *
        consecutivo           INTEGER                                  ,
        usuario               LIKE ret_cta_vol.usuario                 ,
        estado                LIKE ret_cta_vol.estado
    END RECORD
                                                                       
    DEFINE reg_2 RECORD #glo #reg_2
        mto_acc_sub3_sb1      LIKE dis_cuenta.monto_en_acciones        ,
        mto_acc_sub3_sb3      LIKE dis_cuenta.monto_en_acciones        ,
        mto_acc_sub10_sb1     LIKE dis_cuenta.monto_en_acciones        ,
        mto_acc_sub10_sb3     LIKE dis_cuenta.monto_en_acciones        ,
        monto_en_acciones_sb1 LIKE dis_cuenta.monto_en_acciones        ,
        monto_en_acciones_sb3 LIKE dis_cuenta.monto_en_acciones        ,
        monto_en_acciones_10  LIKE dis_cuenta.monto_en_acciones        ,
        monto_en_acciones_3   LIKE dis_cuenta.monto_en_acciones        ,
        monto_en_pesos        LIKE dis_cuenta.monto_en_pesos           ,
        fecha_ult_ret         LIKE ret_cta_vol.fecha_ult_ret           ,
        n_folio_sol           LIKE ret_cta_vol.n_folio_sol             ,
        tipo_ret              LIKE ret_cta_vol.tipo_ret                ,
        des_tipo_ret          CHAR(40)                                 ,
        mto_solic             LIKE ret_cta_vol.mto_solic               ,
        porcentaje_solic      LIKE ret_cta_vol.porcentaje_solic        ,
        tipo_pago             LIKE ret_cta_vol.tipo_pago               ,
        des_tipo_pago         CHAR(40)                                 ,
        edad                  LIKE ret_cta_vol.edad                    ,
        fecha_solic           LIKE ret_cta_vol.fecha_solic             ,
        fecha_captura         LIKE ret_cta_vol.fecha_captura           ,
        ultimo_proceso        LIKE ret_cta_vol.ultimo_proceso          ,
        pension_invalidez     LIKE ret_cta_vol.pension_invalidez       ,
        deduccion             LIKE ret_cta_vol.deduccion               ,
        fecha_deduccion       LIKE ret_cta_vol.fecha_deduccion         ,
        mto_deducido          LIKE ret_cta_vol.mto_deducido            ,
		  cod_rechazo_ent			LIKE ret_cta_vol.cod_rechazo_ent,   #* Rech. *
        consecutivo           INTEGER                                  ,
        usuario               LIKE ret_cta_vol.usuario                 ,
        estado                LIKE ret_cta_vol.estado
    END RECORD                                                           
                                                                       
    DEFINE reg_modi RECORD #glo #reg_1a                        
        n_folio               LIKE afi_mae_afiliado.n_folio            ,
        n_seguro              LIKE afi_mae_afiliado.n_seguro           ,
        n_rfc                 LIKE afi_mae_afiliado.n_rfc              ,
        n_unico               LIKE afi_mae_afiliado.n_unico            ,
        fentcons              LIKE afi_mae_afiliado.fentcons           ,
        folio_liq             SMALLINT                                 ,
        paterno               LIKE afi_mae_afiliado.paterno            ,
        materno               LIKE afi_mae_afiliado.materno            ,
        nombres               LIKE afi_mae_afiliado.nombres            , 
        monto_en_acciones     LIKE dis_cuenta.monto_en_acciones        , 
        monto_en_pesos        LIKE dis_cuenta.monto_en_pesos           , 
        fecha_ult_ret         LIKE ret_cta_vol.fecha_ult_ret           ,
        n_folio_sol           LIKE ret_cta_vol.n_folio_sol             ,
        tipo_ret              LIKE ret_cta_vol.tipo_ret                ,
        des_tipo_ret          CHAR(40)                                 ,
        mto_solic             LIKE ret_cta_vol.mto_solic               ,
        porcentaje_solic      LIKE ret_cta_vol.porcentaje_solic        ,
        tipo_pago             LIKE ret_cta_vol.tipo_pago               ,
        des_tipo_pago         CHAR(40)                                 ,
        tipo_identif          LIKE ret_cta_vol.tipo_identif            ,
        des_tipo_identif      CHAR(40)                                 ,
        fecha_solic           LIKE ret_cta_vol.fecha_solic             ,
        fecha_captura         LIKE ret_cta_vol.fecha_captura           ,
        ultimo_proceso        LIKE ret_cta_vol.ultimo_proceso          ,
        consecutivo           INTEGER                                  ,
        usuario               LIKE ret_cta_vol.usuario                 ,
        estado                LIKE ret_cta_vol.estado
    END RECORD                                               

    DEFINE reg_20 RECORD #glo #reg_20
        estado_marca          SMALLINT ,
        codigo_rechazo        SMALLINT ,
        marca_causa           SMALLINT ,
        fecha_causa           DATE
    END RECORD

    DEFINE  #glo #date 
        HOY                   ,
        vfecha_traspaso       DATE

    DEFINE #glo #char
        ind_cambio_recursos   CHAR(01) ,
        usuario               CHAR(08) ,
        enter                 CHAR(01) ,
        aux_pausa             CHAR(01) ,
        c8_usuario            CHAR(08) ,
        v_desmarca            CHAR(100), 
        v_desmarca2           CHAR(100), 
        v_marca               CHAR(100)

    DEFINE #glo #smallint
        folio_reg             ,
        sw_1                  ,
        sw_2                  ,
        sw_3                  ,
        sw_4                  ,
        sw_5                  ,
        sw_10                 ,
        s_codigo_afore        ,
        vban                  ,
        v_ind_reg             ,
        v_marca_ent           ,
        v_cod_rechazo         ,
        v_marca_res           SMALLINT

    DEFINE #glo #integer
        ultimo_folio          ,
        i_tipo_movimiento     INTEGER

    DEFINE #glo #decimal
        aux_monto_en_pesos    DECIMAL(16,6) ,
        mto_ult_rete_peso     DECIMAL(16,6) ,
        monto_acc_sb1         DECIMAL(16,6) ,
        monto_acc_sb3         DECIMAL(16,6) ,
        monto_pesos_sb1       DECIMAL(16,6) ,
        monto_pesos_sb3       DECIMAL(16,6) ,
        monto_acc             DECIMAL(16,6) ,
        monto_en_acciones_3   DECIMAL(16,6) ,
        monto_en_acciones_10  DECIMAL(16,6) ,
        monto_en_acciones     DECIMAL(16,6) ,
        precio_dia_sb1        DECIMAL(11,6) ,
        precio_dia_sb3        DECIMAL(11,6) ,
        precio_accion_fin     DECIMAL(16,6) ,
        vmto_solic_pesos      DECIMAL(16,6) ,
        vprecio_dia           LIKE glo_valor_accion.precio_del_dia
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS INPUT WRAP, PROMPT LINE LAST, ACCEPT KEY CONTROL-I
 
    LET HOY = TODAY - 1

    CALL init() #i
    OPEN WINDOW retm0031 AT 3,3 WITH FORM "RETM0031" ATTRIBUTE( BORDER)
    DISPLAY " RETM003            DATOS DEL REGISTRO DE AFILIACION                           " AT 3,1 ATTRIBUTE( REVERSE ) 
    DISPLAY "           DISPOSICION DE RECURSOS DE APORTACIONES VOLUNTARIAS                 " AT 8,1 ATTRIBUTE( REVERSE )
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE( REVERSE )

    CALL inicializa() #i
    
    MENU "VOLUNTARIAS"
        COMMAND "Agrega" "AGREGA SOLICITUD"
            CALL agrega()     #a
            CALL inicializa() #i

        COMMAND "Consulta" "CONSULTA SOLICITUD"
            CALL consulta( 1 )   #c
--            CALL inicializa() #i

        COMMAND "Modifica" "MODIFICA SOLICITUD"
            CALL modifica()   #m
--            CALL inicializa() #i

        COMMAND "Elimina" "ELIMINA SOLICITUD"
            CALL elimina()    #e
--            CALL inicializa() #i

        COMMAND "Rechaza" "RECHAZA SOLICITUD"
            CALL consulta( 2 )
            --CALL RechazaSolic()

        COMMAND "Liquidacion" "LIQUIDA APORTACIONES VOLUNTARIAS"
            CALL liquidacion() #l
            LET sw_2 = 0

      COMMAND "Salir" "Salir del Programa"
        EXIT MENU

    END MENU
    CLOSE WINDOW RETM0031
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

    SELECT A.estado_solicitud
    INTO   reg_3.capturado
    FROM   ret_estado A
    WHERE A.descripcion = "CAPTURADO"
    
    SELECT A.estado_solicitud
    INTO   reg_3.precapturado
    FROM   ret_estado A
    WHERE A.descripcion = "PRECAPTURADO"

    SELECT A.estado_solicitud
    INTO   reg_3.liquidado
    FROM   ret_estado A
    WHERE A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud
	 INTO   reg_3.rechazado
	 FROM   ret_estado A
	 WHERE A.descripcion = "RECHAZADO"

    LET v_marca_ent = 0

    LET v_marca = " EXECUTE PROCEDURE marca_cuenta (?,?,?,?,?,?,?,?) "
    PREPARE eje_marca FROM v_marca

    LET v_cod_rechazo     = 0
END FUNCTION

FUNCTION inicializa()
#i-------------------
    INITIALIZE reg_1.* TO NULL
    CLEAR FORM
END FUNCTION

FUNCTION agrega()
#a---------------
	DEFINE 
		reg_8 			RECORD #loc #reg_8
			n_folio     LIKE afi_mae_afiliado.n_folio,
			n_seguro    LIKE afi_mae_afiliado.n_seguro,
			n_rfc       LIKE afi_mae_afiliado.n_rfc,
			n_unico     LIKE afi_mae_afiliado.n_unico,
			fentcons    LIKE afi_mae_afiliado.fentcons,
			paterno     LIKE afi_mae_afiliado.paterno,
			materno     LIKE afi_mae_afiliado.materno,
			nombres     LIKE afi_mae_afiliado.nombres 
		END RECORD

	DEFINE #loc #smallint
		sw_5           SMALLINT

	DEFINE  #looc #date
		fecha_ini6,
		fecha_fin6     DATE

    OPTIONS INPUT WRAP 

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " [ Esc    ] Ingreso              [ Ctrl-C ] Salir                              " AT 1,1 ATTRIBUTE ( BOLD )
    DISPLAY " [ Ctrl-B ] Detalle de solicitud " AT 2, 1
    DISPLAY " AGREGA " AT 1, 65 ATTRIBUTE( REVERSE, BOLD )
    DISPLAY "            DISPOSICION DE RECURSOS DE APORTACIONES VOLUNTARIAS                " AT 8, 1 ATTRIBUTE( REVERSE )

    LET sw_1                 = 0
    LET sw_5                 = 0
    LET monto_en_acciones_3  = 0
    LET monto_en_acciones_10 = 0

    INPUT BY NAME reg_1.n_folio, 
                  reg_1.n_seguro,
                  reg_1.n_rfc,
                  reg_1.n_unico WITHOUT DEFAULTS

        AFTER FIELD n_folio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD n_unico
            END IF

            IF reg_1.n_folio IS NULL THEN
                NEXT FIELD n_seguro
            ELSE
                IF sw_5 = 1 AND reg_1.n_folio = reg_8.n_folio THEN
                    INITIALIZE reg_1.* TO NULL
                    CLEAR FORM
                    NEXT FIELD n_seguro
                END IF
            END IF

            SELECT n_folio,
                   n_seguro,
                   n_rfc,
                   n_unico,
                   fentcons,
                   paterno,
                   materno,
                   nombres,
                   USER
            INTO   reg_1.*, c8_usuario
            FROM   afi_mae_afiliado
            WHERE  n_folio = reg_1.n_folio

            IF STATUS = NOTFOUND THEN
                LET folio_reg = TRUE
                ERROR ""
                ERROR "   FOLIO NO EXISTE EN MAEAFILI...NO ES UN AFILIADO ",
                      "DE LA AFORE" ATTRIBUTE( NORMAL )
                LET reg_1.n_seguro = NULL
                LET reg_1.n_rfc    = NULL
                LET reg_1.n_unico  = NULL
                LET reg_1.fentcons = NULL
                LET reg_1.paterno  = NULL
                LET reg_1.materno  = NULL
                LET reg_1.nombres  = NULL
                INITIALIZE reg_8.* TO NULL
                CLEAR FORM
                NEXT FIELD n_folio
            END IF
            DISPLAY BY NAME reg_1.*

        AFTER FIELD n_seguro
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD n_folio
            END IF
            IF reg_1.n_seguro IS NULL THEN
                NEXT FIELD n_rfc
            END IF

            SELECT n_folio,
                   n_seguro,
                   n_rfc,
                   n_unico,
                   fentcons,
                   paterno,
                   materno,
                   nombres,
                   USER
            INTO  reg_1.*,c8_usuario
            FROM  afi_mae_afiliado
            WHERE n_seguro = reg_1.n_seguro

            IF STATUS = NOTFOUND THEN
                ERROR ""
                ERROR "   NSS INEXISTENTE " ATTRIBUTE( NORMAL )
                NEXT FIELD n_unico
            END IF
            DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres

        AFTER FIELD n_unico
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD n_rfc
            END IF
            IF reg_1.n_unico IS NULL THEN
                NEXT FIELD n_folio
            END IF

            SELECT n_folio,
                   n_seguro,
                   n_rfc,
                   n_unico,
                   fentcons,
                   paterno,
                   materno,
                   nombres,
                   USER
            INTO  reg_1.*,c8_usuario
            FROM  afi_mae_afiliado
            WHERE n_unico = reg_1.n_unico

            IF STATUS = NOTFOUND THEN
                ERROR ""
                ERROR "   CURP INEXISTENTE " ATTRIBUTE( NORMAL )
                NEXT FIELD n_unico
            END IF
            DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres

        ON KEY( CONTROL-B )
            INITIALIZE reg_2.* TO NULL
            DISPLAY "CAPTURA" AT 20, 41
            LET reg_8.*           = reg_1.*
            LET reg_2.fecha_solic = HOY

				SELECT 1 FROM ret_cta_vol
				WHERE n_seguro = reg_1.n_seguro
				AND estado NOT IN ( 8, 20 )   #* 8 = Liq., 20 = Rech. *
				--AND estado <> 8
				IF STATUS <> NOTFOUND THEN
					LET sw_5 = 1
					ERROR "   TRABAJADOR CON OTRA SOLICITUD EN ESTADO CAPTURADA " 
						ATTRIBUTE( NORMAL )
					NEXT FIELD n_folio
				END IF

            SELECT NVL( SUM(A.monto_en_acciones), 0 )
            INTO reg_2.monto_en_acciones_3
            FROM dis_cuenta A
            WHERE A.nss = reg_1.n_seguro
            AND A.subcuenta = 3

            SELECT NVL( SUM(A.monto_en_acciones), 0 )
            INTO reg_2.monto_en_acciones_10
            FROM dis_cuenta A
            WHERE A.nss = reg_1.n_seguro
            AND A.subcuenta = 10

				IF( reg_2.monto_en_acciones_3 = 0 
					AND reg_2.monto_en_acciones_10 = 0 ) THEN
					LET sw_5 = 1
					ERROR "   TRABAJADOR CON SALDO CERO" ATTRIBUTE ( NORMAL )
					NEXT FIELD n_folio
				END IF

				INITIALIZE  fecha_ini6 TO NULL
				SELECT MAX(fecha_ult_ret)
				INTO fecha_ini6
				FROM ret_cta_vol
				WHERE n_seguro = reg_1.n_seguro
				AND   estado  = 8

				IF fecha_ini6 IS NULL OR fecha_ini6 = " " THEN
					SELECT MIN(fecha_conversion)
					INTO fecha_ini6
					FROM dis_cuenta
					WHERE nss = reg_1.n_seguro
					AND subcuenta IN ( 3, 10 )
					AND tipo_movimiento = 1
				END IF
				LET reg_2.fecha_ult_ret = fecha_ini6
				CALL regresa_mes6( fecha_ini6 ) RETURNING fecha_fin6

{-- de manera momentanea mientras aclaro con STEFANIE
				IF fecha_fin6 > HOY THEN
					LET sw_5 = 1
					ERROR "   AUN NO HAN TRANSCURRIDO 6 MESES DEL ULTIMO RETIRO" 
						ATTRIBUTE ( NORMAL )
					NEXT FIELD n_folio
				END IF
}
            CALL valida_cta_saldo_vol( reg_1.n_seguro )

            SELECT MAX(A.consecutivo) + 1
            INTO reg_2.consecutivo
            FROM ret_consecutivo A
            IF reg_2.consecutivo IS NULL THEN
               LET reg_2.consecutivo = 1
            END IF
            INSERT INTO ret_consecutivo VALUES (reg_2.consecutivo)

            DISPLAY BY NAME reg_2.consecutivo
            LET sw_2 = 0
           
            IF reg_1.n_folio IS NULL THEN
                ERROR ""
                ERROR "   EL FOLIO DE LA SOLICITUD DE REGISTRO NO PUEDE SER ",
                      "NULO" ATTRIBUTE( NORMAL )
                NEXT FIELD n_folio
            END IF 

            INPUT BY NAME reg_2.n_folio_sol,
                          reg_2.tipo_ret,
                          reg_2.mto_solic,
                          reg_2.porcentaje_solic,
                          reg_2.tipo_pago,
                          reg_2.edad,
                          reg_2.fecha_solic,
                          reg_2.pension_invalidez,
                          reg_2.deduccion,
                          reg_2.fecha_deduccion,
                          reg_2.mto_deducido WITHOUT DEFAULTS

                BEFORE FIELD n_folio_sol
                    IF sw_2 = 0 THEN
                        SELECT NVL( SUM(A.monto_en_acciones), 0 )
                        INTO   reg_2.monto_en_acciones_3
                        FROM   dis_cuenta A
                        WHERE  A.nss             = reg_1.n_seguro
                        AND    A.subcuenta       = 3

                        SELECT NVL( SUM(A.monto_en_acciones), 0 )
                        INTO   reg_2.monto_en_acciones_10
                        FROM   dis_cuenta A
                        WHERE  A.nss             = reg_1.n_seguro
                        AND    A.subcuenta       = 10

                       IF reg_2.monto_en_acciones_3  = 0 
							  	AND reg_2.monto_en_acciones_10 = 0 THEN
								  LET sw_5 = 1
								  ERROR "   TRAJAJADOR CON SALDO CERO" ATTRIBUTE( NORMAL )
								  SLEEP 2
								  EXIT PROGRAM
							  END IF

                        ----- SALDO EN SIEFORE 1 -----

                        SELECT NVL( SUM(A.monto_en_acciones), 0 )
                        INTO   reg_2.mto_acc_sub3_sb1
                        FROM   dis_cuenta A
                        WHERE  A.nss             = reg_1.n_seguro
                        AND    A.subcuenta       = 3
                        AND    A.siefore         = 1

                        SELECT NVL( SUM(A.monto_en_acciones), 0 )
                        INTO   reg_2.mto_acc_sub10_sb1
                        FROM   dis_cuenta A
                        WHERE  A.nss             = reg_1.n_seguro
                        AND    A.subcuenta       = 10
                        AND    A.siefore         = 1

                        CALL precio_accion( HOY, 1 ) RETURNING precio_dia_sb1

                        LET reg_2.monto_en_acciones_sb1 = reg_2.mto_acc_sub3_sb1  +
                                                          reg_2.mto_acc_sub10_sb1
                        LET reg_2.monto_en_pesos = reg_2.monto_en_acciones_sb1 *
                                                   precio_dia_sb1

                        ----- SALDO EN SIEFORE 3 -----

                        SELECT NVL( SUM(A.monto_en_acciones), 0 )
                        INTO   reg_2.mto_acc_sub3_sb3
                        FROM   dis_cuenta A
                        WHERE  A.nss             = reg_1.n_seguro
                        AND    A.subcuenta       = 3
                        AND    A.siefore         = 3

                        SELECT NVL( SUM(A.monto_en_acciones), 0 )
                        INTO   reg_2.mto_acc_sub10_sb3
                        FROM   dis_cuenta A
                        WHERE  A.nss             = reg_1.n_seguro
                        AND    A.subcuenta       = 10
                        AND    A.siefore         = 3

                        --svera CALL precio_accion( HOY, 3 )
                        --svera RETURNING precio_dia_sb3

                        LET reg_2.monto_en_acciones_sb3 = reg_2.mto_acc_sub3_sb3 +
                                                          reg_2.mto_acc_sub10_sb3
                        LET reg_2.monto_en_pesos = reg_2.monto_en_pesos 
                            --svera                      (reg_2.monto_en_acciones_sb3 * precio_dia_sb3 )

                        -------------------------

                        DISPLAY reg_2.monto_en_acciones_sb1 TO monto_en_acciones_sb1
                        DISPLAY reg_2.monto_en_acciones_sb3 TO monto_en_acciones_sb3
                        DISPLAY reg_2.monto_en_pesos    TO monto_en_pesos
                        DISPLAY reg_2.fecha_ult_ret     TO fecha_ult_ret
                        LET sw_2                 = 1
                        LET sw_4                 = 0
                        LET reg_2.fecha_captura  = HOY
                        LET reg_2.usuario        = c8_usuario
                        LET reg_2.ultimo_proceso = HOY

                        DISPLAY reg_2.fecha_ult_ret  TO fecha_ult_ret
                        DISPLAY reg_2.fecha_captura  TO fecha_captura
                        DISPLAY reg_2.usuario        TO usuario
                        DISPLAY reg_2.ultimo_proceso TO ultimo_proceso
                    END IF

                AFTER FIELD n_folio_sol
                    IF reg_2.n_folio_sol IS NULL THEN
                        ERROR ""
                        ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                        NEXT FIELD n_folio_sol
                    ELSE
                       SELECT "OK"
                       FROM ret_cta_vol
                       WHERE n_folio_sol = reg_2.n_folio_sol
                       GROUP BY 1

                       IF STATUS <> NOTFOUND THEN
                           eRROR ""
                           ERROR "   SOLICITUD YA INGRESADA " ATTRIBUTE( NORMAL )
                           NEXT FIELD n_folio_sol
                       END IF
                    END IF

                AFTER FIELD tipo_ret
                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                        NEXT FIELD n_folio_sol
                    END IF

                    IF reg_2.tipo_ret IS NULL THEN
                       CALL despliega_tipo_ret() #dtr
                       RETURNING reg_2.tipo_ret, reg_2.des_tipo_ret

                        IF reg_2.tipo_ret = 1 THEN
                            LET reg_2.porcentaje_solic = ""
                            LET reg_2.mto_solic        = ""
                            DISPLAY reg_2.tipo_ret         TO tipo_ret
                            DISPLAY reg_2.des_tipo_ret     TO des_tipo_ret
                            DISPLAY reg_2.mto_solic        TO mto_solic
                            DISPLAY reg_2.porcentaje_solic TO porcentaje_solic
                            NEXT FIELD mto_solic
                        ELSE
                            LET reg_2.porcentaje_solic = 100
                            LET reg_2.mto_solic        = reg_2.monto_en_pesos

                            DISPLAY reg_2.monto_en_pesos   TO monto_en_pesos
                            DISPLAY reg_2.tipo_ret         TO tipo_ret
                            DISPLAY reg_2.des_tipo_ret     TO des_tipo_ret
                            DISPLAY reg_2.mto_solic        TO mto_solic
                            DISPLAY reg_2.porcentaje_solic TO porcentaje_solic
                            NEXT FIELD tipo_pago
                        END IF
                    ELSE
                        SELECT des_tipo_ret
                        INTO reg_2.des_tipo_ret
                        FROM tab_retiro_old
                        WHERE tipo_ret = reg_2.tipo_ret

                        IF STATUS = NOTFOUND THEN
                            ERROR "   TIPO DE RETIRO INEXISTENTE " 
									 ATTRIBUTE( NORMAL )
                            NEXT FIELD tipo_ret
                        ELSE
                            IF reg_2.tipo_ret = 1 THEN
                                LET reg_2.porcentaje_solic = ""
                                LET reg_2.mto_solic        = ""
                                DISPLAY reg_2.tipo_ret         TO tipo_ret
                                DISPLAY reg_2.des_tipo_ret     TO des_tipo_ret
                                DISPLAY reg_2.mto_solic        TO mto_solic
                                DISPLAY reg_2.porcentaje_solic TO porcentaje_solic
                            ELSE
                                LET reg_2.porcentaje_solic = 100
                                LET reg_2.mto_solic        = reg_2.monto_en_pesos
                                DISPLAY reg_2.tipo_ret         TO tipo_ret
                                DISPLAY reg_2.des_tipo_ret     TO des_tipo_ret
                                DISPLAY reg_2.mto_solic        TO mto_solic
                                DISPLAY reg_2.porcentaje_solic TO porcentaje_solic
                                NEXT FIELD tipo_pago
                            END IF
                        END IF
                    END IF

                AFTER FIELD mto_solic
                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                        NEXT FIELD tipo_ret
                    END IF
                    IF reg_2.mto_solic IS NULL THEN
                        NEXT FIELD porcentaje_solic
                    END IF
                    IF reg_2.mto_solic <= 0 THEN
                        ERROR "   MONTO INVALIDO" ATTRIBUTE( NORMAL )
                    ELSE
                        LET reg_2.porcentaje_solic = (( reg_2.mto_solic * 100 )/
                                                        reg_2.monto_en_pesos  )

                        IF reg_2.mto_solic > reg_2.monto_en_pesos THEN
                           ERROR " MONTO INVALIDO, VERIFIQUELO " 
                                 ATTRIBUTE( NORMAL )
                           NEXT FIELD mto_solic
                        END IF
                    END IF
                    DISPLAY reg_2.porcentaje_solic TO porcentaje_solic
                    NEXT FIELD tipo_pago

                AFTER FIELD porcentaje_solic
                    IF reg_2.porcentaje_solic <= 0 THEN
                        ERROR "   PORCENTAJE INVALIDO" ATTRIBUTE( NORMAL )
                    ELSE
                        LET monto_paso = ( reg_2.monto_en_pesos *
                                         ( reg_2.porcentaje_solic / 100 ) )
                        LET reg_2.mto_solic = monto_paso 
                        DISPLAY reg_2.mto_solic TO mto_solic
                    END IF

                AFTER FIELD tipo_pago
                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                        IF reg_2.tipo_ret = 2 THEN
                            NEXT FIELD tipo_ret
                        ELSE
                            NEXT FIELD porcentaje_solic
                        END IF
                    END IF

                    IF reg_2.tipo_pago IS NULL THEN
                       CALL despliega_tipo_pago() #dtp
                           RETURNING reg_2.tipo_pago, reg_2.des_tipo_pago
                       DISPLAY reg_2.des_tipo_pago TO des_tipo_pago

							CASE reg_2.tipo_pago
								WHEN 1
									CALL agrega_orden_de_pago( reg_1.n_seguro,
									reg_2.consecutivo,
									reg_1.paterno,
									reg_1.materno,
									reg_1.nombres,
									100,
									0,
									sw_2 )

								WHEN 2
									{
									CALL agrega_abono_cta( reg_1.n_seguro,
									"",
									reg_1.paterno,
									reg_1.materno,
									reg_1.nombres,
									reg_2.consecutivo,
									sw_4 ) #aac
									}
									LET sw_2 = 1

								WHEN 3
									let sw_2 = 0
									CALL agrega_cheque( "",    #num_resolu
									reg_1.n_seguro,
									"",                        #tipo_prest
									reg_1.paterno,
									reg_1.materno,
									reg_1.nombres,
									reg_2.consecutivo,
									sw_2 ) #glo #ac 

									LET sw_2 = 1

								OTHERWISE
									ERROR ""
									ERROR "   TIPO DE PAGO INEXISTENTE"
									ATTRIBUTE ( NORMAL )
									NEXT FIELD tipo_pago
							END CASE
                    ELSE
							SELECT descripcion 
							INTO   reg_2.des_tipo_pago
							FROM   tab_pago
							WHERE  tipo_pago = reg_2.tipo_pago

							CASE reg_2.tipo_pago
								WHEN 1
									CALL agrega_orden_de_pago( reg_1.n_seguro,
									reg_2.consecutivo,
									reg_1.paterno,
									reg_1.materno,
									reg_1.nombres,
									100,
									0,
									sw_2 )

								WHEN 2
									CALL agrega_abono_cta( reg_1.n_seguro,
									reg_2.consecutivo,
									reg_1.paterno,
									reg_1.materno,
									reg_1.nombres,
									100,
									reg_2.mto_solic,
									sw_4 )   #aac

									LET sw_2 = 1

								WHEN 3
								CALL agrega_cheque( "",    #num_resolu
								reg_1.n_seguro,
								"",                        #tipo_prest
								reg_1.paterno,
								reg_1.materno,
								reg_1.nombres,
								reg_2.consecutivo,
								sw_2 )       #glo #ac 

								LET sw_2 = 1

								OTHERWISE
									ERROR ""
									ERROR "   TIPO DE PAGO INEXISTENTE"
									ATTRIBUTE( NORMAL )
									NEXT FIELD tipo_pago
							END CASE
                        DISPLAY reg_2.des_tipo_pago TO des_tipo_pago
                    END IF
 
                AFTER FIELD edad
                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                        NEXT FIELD tipo_pago
                    END IF

                    IF reg_2.edad IS NULL THEN
                        ERROR ""
                        ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                        NEXT FIELD edad
                    END IF
               
                AFTER FIELD fecha_solic
                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                        NEXT FIELD edad
                    END IF

                    IF reg_2.fecha_solic IS NULL THEN
                        ERROR ""
                        ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                        NEXT FIELD fecha_solic
                    END IF

                    IF reg_2.fecha_solic > HOY THEN
                        ERROR ""
                        ERROR "   FECHA NO PUEDE SER SUPERIOR A LA ACTUAL " ATTRIBUTE( NORMAL )
                        NEXT FIELD fecha_solic
                    END IF

                AFTER FIELD pension_invalidez
                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                        NEXT FIELD fecha_solic
                    END IF

                    IF reg_2.pension_invalidez IS NULL THEN
                        ERROR ""
                        ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                        NEXT FIELD pension_invalidez
                    ELSE
                        CASE reg_2.pension_invalidez
                            WHEN  "S"
                            WHEN  "N"
                        OTHERWISE
                            ERROR ""
                            ERROR "   CAMPO DEBE SER S o N " ATTRIBUTE( NORMAL )
                            NEXT FIELD pension_invalidez
                        END CASE
                    END IF

                AFTER FIELD deduccion
                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                        NEXT FIELD pension_invalidez
                    END IF

                    IF reg_2.deduccion IS NULL THEN
                        ERROR ""
                        ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                        NEXT FIELD deduccion
                    ELSE
                        CASE reg_2.deduccion
                            WHEN  "S"
                            WHEN  "N"
                        OTHERWISE
                            ERROR ""
                            ERROR "   CAMPO DEBE SER S o N " ATTRIBUTE( NORMAL )
                            NEXT FIELD deduccion
                        END CASE

                        IF reg_2.deduccion = "S" THEN
                            NEXT FIELD fecha_deduccion
                        ELSE
                            NEXT FIELD n_folio_sol
                        END IF
                    END IF

                AFTER FIELD fecha_deduccion
                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                        NEXT FIELD deduccion
                    END IF

                    IF reg_2.deduccion = "S" THEN
                        IF reg_2.fecha_deduccion IS NULL THEN
                           ERROR ""
                           ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                           NEXT FIELD fecha_deduccion
                        END IF

                        IF reg_2.fecha_deduccion > HOY THEN
                            ERROR ""
                            ERROR "   FECHA NO PUEDE SER SUPERIOR A LA ACTUAL " ATTRIBUTE( NORMAL )
                            NEXT FIELD fecha_deduccion
                        END IF
                    END IF

                AFTER FIELD mto_deducido
                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                        NEXT FIELD fecha_deduccion
                    END IF

                    IF reg_2.deduccion = "S" THEN
                         IF reg_2.mto_deducido IS NULL THEN
                             ERROR ""
                             ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                             NEXT FIELD mto_deducido
                         END IF

                         IF reg_2.mto_deducido <= 0 THEN
                             ERROR "   MONTO INVALIDO" ATTRIBUTE( NORMAL )
                             NEXT FIELD mto_deducido
                         END IF
                    END IF

                ON KEY ( ESC )
                    IF reg_2.n_folio_sol IS NULL THEN
                        ERROR ""
                        ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                        NEXT FIELD n_folio_sol
                    END IF

                    IF reg_2.tipo_ret IS NULL THEN
                        ERROR ""
                        ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE( NORMAL )
                        NEXT FIELD tipo_ret
                    END IF

                    IF reg_2.tipo_pago IS NULL THEN
                        ERROR ""
                        ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE( NORMAL )
                        NEXT FIELD tipo_pago
                    ELSE
                        SELECT descripcion
                        INTO   reg_2.des_tipo_pago
                        FROM   tab_pago
                        WHERE  tipo_pago = reg_2.tipo_pago

                        IF STATUS = NOTFOUND THEN
                           ERROR ""
                           ERROR "   TIPO DE PAGO INEXISTENTE" ATTRIBUTE( NORMAL )
                           NEXT FIELD tipo_pago
                        END IF
                        DISPLAY reg_2.des_tipo_pago TO des_tipo_pago
                    END IF

                    IF reg_2.edad IS NULL THEN
                        ERROR ""
                        ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                        NEXT FIELD edad
                    END IF

                    IF reg_2.fecha_solic IS NULL THEN
                        ERROR ""
                        ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                        NEXT FIELD fecha_solic
                    END IF

                    IF reg_2.fecha_solic > HOY THEN
                        ERROR ""
                        ERROR "   FECHA NO PUEDE SER SUPERIOR A LA ACTUAL " 
									ATTRIBUTE( NORMAL )
                        NEXT FIELD fecha_solic
                    END IF

                    IF reg_2.pension_invalidez IS NULL THEN
                        ERROR ""
                        ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                        NEXT FIELD pension_invalidez
                    ELSE
                        CASE reg_2.pension_invalidez
                            WHEN  "S"
                            WHEN  "N"
                        OTHERWISE
                            ERROR ""
                            ERROR "   CAMPO DEBE SER S o N " ATTRIBUTE( NORMAL )
                            NEXT FIELD pension_invalidez
                        END CASE
                    END IF

                    IF reg_2.deduccion IS NULL THEN
                        ERROR ""
                        ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                        NEXT FIELD deduccion
                    ELSE
                        CASE reg_2.deduccion
                            WHEN  "S"
                            WHEN  "N"
                        OTHERWISE
                            ERROR ""
                            ERROR "   CAMPO DEBE SER S o N " ATTRIBUTE( NORMAL )
                            NEXT FIELD deduccion
                        END CASE

                    END IF

                    IF reg_2.deduccion = "S" THEN
                        IF reg_2.fecha_deduccion IS NULL THEN
                           ERROR ""
                           ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                           NEXT FIELD fecha_deduccion
                        END IF

                        IF reg_2.fecha_deduccion > HOY THEN
                            ERROR ""
                            ERROR "   FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
ATTRIBUTE( NORMAL )
                            NEXT FIELD fecha_deduccion
                        END IF
                    END IF

                    IF reg_2.deduccion = "S" THEN
                         IF reg_2.mto_deducido IS NULL THEN
                             ERROR ""
                             ERROR "   CAMPO NO PUEDE SER NULO "
                             ATTRIBUTE( NORMAL )
                             NEXT FIELD mto_deducido
                         END IF

                         IF reg_2.mto_deducido <= 0 THEN
                             ERROR "   MONTO INVALIDO" ATTRIBUTE( NORMAL )
                             NEXT FIELD mto_deducido
                         END IF
                    END IF

                    INSERT INTO ret_cta_vol
                    VALUES( reg_1.n_folio,
                           reg_2.n_folio_sol,
                           "S",                    #tipo_id
                           "",                     #n_folio_liq
                           reg_1.n_seguro,
                           reg_1.n_unico,
                           reg_1.n_rfc,
                           reg_1.paterno,
                           reg_1.materno,
                           reg_1.nombres,
                           reg_2.edad,
                           reg_2.tipo_ret,
                           reg_2.deduccion,
                           reg_2.pension_invalidez,
                           0,
                           0,
                           reg_2.mto_deducido,
                           reg_2.mto_solic,
                           reg_2.porcentaje_solic,
                           reg_2.tipo_pago,
                           "",                     #tipo_identif
									NULL,							#* Rech., cod_rechazo_ent *
                           reg_2.fecha_solic,
                           reg_2.fecha_captura,
                           reg_2.fecha_ult_ret,
                           reg_2.fecha_deduccion, 
                           reg_2.ultimo_proceso,
                           "",                     #diagnostico_pago
                           reg_3.capturado,        #estado
                           c8_usuario,             #usuario
                           reg_2.consecutivo,
                           "",                     #paterno_sol
                           "",                     #materno_sol
                           "" )                    #nombre_sol

                   LET i_tipo_movimiento = 490                  

                   LET reg_20.estado_marca   = 0
                   LET reg_20.codigo_rechazo = 0
                   LET reg_20.marca_causa    = 0
                   LET reg_20.fecha_causa    = NULL

                   DECLARE cur_sp CURSOR FOR eje_marca
                   OPEN cur_sp USING reg_1.n_seguro        , # nss
                                     i_tipo_movimiento     , # marca entrante
                                     reg_2.consecutivo     , # correlativo
                                     reg_20.estado_marca   ,
                                     reg_20.codigo_rechazo ,
                                     reg_20.marca_causa    ,
                                     reg_20.fecha_causa    ,
                                     usuario

                   FETCH cur_sp INTO v_marca_res  , # misma marca si convive o
                                     v_cod_rechazo  # marca_activa que rechaza
                                                    # codigo de rechazo
                   CLOSE cur_sp

                   DISPLAY "SOLICITUD INSERTADA" AT 20,2 ATTRIBUTE( REVERSE )
                   SLEEP 3
                   LET sw_2 = 0
                   INITIALIZE reg_2.*  TO NULL
                   INITIALIZE reg_1.*  TO NULL
                   LET sw_1 = 1
                   EXIT INPUT

                ON KEY ( CONTROL-C )
                    DELETE
                    FROM   ret_consecutivo
                    WHERE  consecutivo = reg_2.consecutivo

                    INITIALIZE reg_2.* TO NULL
                    CLEAR FORM
                    DISPLAY "         " AT 19, 41
                    EXIT INPUT
                    
                ON KEY ( INTERRUPT )
                    DELETE FROM ret_consecutivo
                    WHERE consecutivo = reg_2.consecutivo

                    INITIALIZE reg_2.* TO NULL
                    CLEAR FORM
                    DISPLAY "         " AT 19, 41
                    EXIT INPUT
            END INPUT
            IF sw_1 = 1 THEN
                EXIT INPUT
            END IF
	END INPUT
END FUNCTION

FUNCTION consulta( tipo_cons )
#c----------------------------

	DEFINE #loc #char
		c9des_estado_solic   CHAR(009),
		hace_el_input        CHAR(300),
		hace_el_select       CHAR(500)

	DEFINE #loc #smallint
		s_tot_registros,
		arr_c, scr_l, pos, 
		tipo_cons, i     		SMALLINT,    #* Rech. *
		cad_edo					CHAR(60),    #* Rech. *
		cadsql					CHAR(900)    #* Rech. *

    DISPLAY "" AT 1, 1
    DISPLAY "" AT 2, 1
    DISPLAY " [ Ctrl-C ] Regresa al menu "        AT 1, 1
	 IF( tipo_cons = 1 ) THEN
		 DISPLAY " [ Ctrl-B ] Consulta beneficiarios " AT 2, 1
		 DISPLAY " CONSULTA" AT 1, 65 ATTRIBUTE( REVERSE, BOLD )
	 ELSE
	 	 DISPLAY " [ ESC ] Buscar " AT 2, 1
		 DISPLAY " RECHAZA" AT 1, 65 ATTRIBUTE( REVERSE, BOLD )
	 END IF
    DISPLAY "            DISPOSICION DE RECURSOS DE APORTACIONES VOLUNTARIAS                " AT 8, 1 ATTRIBUTE( REVERSE )

    LET sw_1          = 0
    LET INT_FLAG      = FALSE
    LET hace_el_input = NULL
    INITIALIZE reg_1.* TO NULL

    CONSTRUCT hace_el_input ON A.n_folio,
                               A.n_seguro,
                               A.n_rfc,
                               A.n_unico,
                               A.paterno,
                               A.materno,
                               A.nombres
                          FROM n_folio,
                               n_seguro,
                               n_rfc,
                               n_unico,
                               paterno,
                               materno,
                               nombres

        ON KEY( ESC )
            ERROR ""
            ERROR " PROCESANDO INFORMACION..."
            LET int_flag = FALSE
            EXIT CONSTRUCT

         ON KEY( CONTROL-C )
            LET int_flag = TRUE
            CLEAR FORM
            EXIT CONSTRUCT
    END CONSTRUCT

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR ""
        ERROR " BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        RETURN
    END IF 

	LET hace_el_select = 
		"SELECT A.n_folio, ",
			"A.n_seguro, ",
			"A.n_rfc, ",
			"A.n_unico, ",
			"A.fentcons, ",
			"A.paterno, ",
			"A.materno, ",
			"A.nombres, ",
			"USER ",
		"FROM afi_mae_afiliado A, ret_cta_vol B ",
		"WHERE ", hace_el_input CLIPPED, " ",
		"AND A.n_seguro = B.n_seguro ",
		"GROUP BY 1, 2, 3, 4, 5, 6, 7, 8, 9 ",
		"ORDER BY 2, 3 " 

	LET pos = 1
	PREPARE cur1 FROM hace_el_select
	DECLARE cursor_1 CURSOR FOR cur1
	FOREACH cursor_1 INTO reg_1.*, c8_usuario   
		LET cad_edo = " "
		IF( tipo_cons = 2 ) THEN    #* Rech. *
			LET cad_edo = "AND A.estado IN ( 0, 3 , -1 ) AND cod_rechazo_ent IS NULL"
		END IF
		LET cadsql = 
			"SELECT 0, ",
			"0, ",
			"0, ",
			"0, ",
			"0, ",
			"0, ",
			"0, ",
			"0, ",
			"0, ",
			"A.fecha_ult_ret, ",     #fecha_ult_ret
			"A.n_folio_sol, ",       #n_folio_sol
			"A.tipo_ret, ",          #tipo_ret
			"B.des_tipo_ret, ",      #des_tipo_ret
			"A.mto_solic, ",         #mto_solic
			"A.porcentaje_solic, ",  #porcentaje_solic
			"A.tipo_pago, ",         #tipo_pago
			"'', ",                  #des_tipo_pago
			"A.edad, ",              #edad
			"A.fecha_solic, ",       #fecha_solic
			"A.fecha_captura, ",     #fecha_captura
			"A.ultimo_proceso, ",    #ultimo_proceso
			"A.pension_invalidez, ", #pension_invalidez
			"A.deduccion, ",         #deduccion
			"A.fecha_deduccion, ",   #fecha_deduccion
			"A.mto_deducido, ",      #mto_deducido
			"A.cod_rechazo_ent, ",   #* Rech. *
			"A.consecutivo, ",       #consecutivo
			"A.usuario, ",           #usuario
			"A.estado ",             #estado
			"FROM ret_cta_vol A, tab_retiro_old B ",
			"WHERE A.n_seguro = '", reg_1.n_seguro, "' ",
			"AND A.tipo_ret = B.tipo_ret ", 
			cad_edo CLIPPED, " ",      -- Rech., P/Rechazar estado in ( 0, 3 ,-1) *
			"ORDER BY fecha_captura "

		  PREPARE idsql01 FROM cadsql
        DECLARE cursor_con_2 CURSOR FOR idsql01
        FOREACH cursor_con_2 INTO reg_2.*
            LET aux_monto_en_pesos          = 0
            LET precio_dia_sb1              = 0
            LET precio_dia_sb3              = 0
            LET reg_2.monto_en_acciones_sb1 = 0
            LET reg_2.monto_en_acciones_sb3 = 0
            LET reg_2.monto_en_pesos        = 0

            CALL precio_accion( reg_2.fecha_captura, 1 )
            RETURNING precio_dia_sb1         

            SELECT NVL( SUM(A.monto_en_acciones), 0 ),
                   NVL( SUM(A.monto_en_acciones), 0 ) * precio_dia_sb1
            INTO reg_2.monto_en_acciones_sb1,
                 reg_2.monto_en_pesos
            FROM dis_cuenta A
            WHERE A.nss = reg_1.n_seguro
            AND A.subcuenta IN ( 3, 10 )
            AND A.fecha_conversion <= reg_2.fecha_captura
            AND A.siefore = 1

            LET aux_monto_en_pesos = reg_2.monto_en_pesos
            LET reg_2.monto_en_pesos = 0 

            SELECT NVL( SUM(A.monto_en_acciones), 0 ),
                   NVL( SUM(A.monto_en_acciones), 0 ) * precio_dia_sb3
            INTO   reg_2.monto_en_acciones_sb3,
                   reg_2.monto_en_pesos
            FROM   dis_cuenta A
            WHERE  A.nss             = reg_1.n_seguro
            AND    A.subcuenta       IN( 3, 10 )
            AND    A.fecha_conversion <= reg_2.fecha_captura
            AND    A.siefore         = 3

            LET reg_2.monto_en_pesos = reg_2.monto_en_pesos + aux_monto_en_pesos

            SELECT descripcion
            INTO   reg_2.des_tipo_pago
            FROM   tab_pago
            WHERE  tipo_pago = reg_2.tipo_pago

            DISPLAY reg_2.des_tipo_pago TO des_tipo_pago

            LET reg_2_array[pos].var_nula              =  " "
            LET reg_2_array[pos].n_folio               =  reg_1.n_folio
            LET reg_2_array[pos].n_seguro              =  reg_1.n_seguro 
            LET reg_2_array[pos].n_rfc                 =  reg_1.n_rfc   
            LET reg_2_array[pos].n_unico               =  reg_1.n_unico  
            LET reg_2_array[pos].fentcons              =  reg_1.fentcons 
            LET reg_2_array[pos].paterno               =  reg_1.paterno  
            LET reg_2_array[pos].materno               =  reg_1.materno  
            LET reg_2_array[pos].nombres               =  reg_1.nombres 
            LET reg_2_array[pos].monto_en_acciones_sb1 =  reg_2.monto_en_acciones_sb1
            LET reg_2_array[pos].monto_en_acciones_sb3 =  reg_2.monto_en_acciones_sb3
            LET reg_2_array[pos].monto_en_pesos        =  reg_2.monto_en_pesos
            LET reg_2_array[pos].fecha_ult_ret         =  reg_2.fecha_ult_ret
            LET reg_2_array[pos].n_folio_sol           =  reg_2.n_folio_sol
            LET reg_2_array[pos].tipo_ret              =  reg_2.tipo_ret
            LET reg_2_array[pos].des_tipo_ret          =  reg_2.des_tipo_ret
            LET reg_2_array[pos].mto_solic             =  reg_2.mto_solic
            LET reg_2_array[pos].porcentaje_solic      =  reg_2.porcentaje_solic
            LET reg_2_array[pos].tipo_pago             =  reg_2.tipo_pago
            LET reg_2_array[pos].des_tipo_pago         =  reg_2.des_tipo_pago
            LET reg_2_array[pos].edad                  =  reg_2.edad
            LET reg_2_array[pos].fecha_solic           =  reg_2.fecha_solic
            LET reg_2_array[pos].fecha_captura         =  reg_2.fecha_captura
            LET reg_2_array[pos].ultimo_proceso        =  reg_2.ultimo_proceso
            LET reg_2_array[pos].pension_invalidez     =  reg_2.pension_invalidez
            LET reg_2_array[pos].deduccion             =  reg_2.deduccion
            LET reg_2_array[pos].fecha_deduccion       =  reg_2.fecha_deduccion
            LET reg_2_array[pos].mto_deducido          =  reg_2.mto_deducido
				LET reg_2_array[pos].cod_rechazo_ent = reg_2.cod_rechazo_ent   #* Rech. *
            LET reg_2_array[pos].consecutivo           =  reg_2.consecutivo
            LET reg_2_array[pos].usuario               =  reg_2.usuario
            LET reg_2_array[pos].estado                =  reg_2.estado
            LET pos = pos + 1
        END FOREACH
    END FOREACH       

    CALL SET_COUNT( pos - 1 )
    ERROR ""

	 DISPLAY "                 " AT 2, 1
    LET s_tot_registros = pos - 1
    IF( pos - 1 ) >= 1 THEN
        INPUT ARRAY reg_2_array WITHOUT DEFAULTS FROM scr_3.*
            BEFORE ROW
                LET arr_c = ARR_CURR()
                IF arr_c >= pos THEN
                   ERROR "   NO HAY MAS REGISTROS HACIA ABAJO" 
						 	ATTRIBUTE( NORMAL )
                END IF

                SELECT descripcion
                INTO   c9des_estado_solic
                FROM   ret_estado A
                WHERE  A.estado_solicitud = reg_2_array[arr_c].estado

                DISPLAY "Total Registros ", s_tot_registros AT 19, 56
                DISPLAY c9des_estado_solic AT 20, 41

					 IF( tipo_cons = 2 ) THEN    #* Rech., Solo opcion Rechaza *
					 	NEXT FIELD cod_rechazo_ent
					 END IF

				AFTER FIELD cod_rechazo_ent
					IF( tipo_cons = 2 ) THEN    #* Rech., Solo opcion Rechaza *
						IF( reg_2_array[arr_c].cod_rechazo_ent IS NOT NULL ) THEN
							SELECT 1 FROM ret_rechazo_grl
								WHERE cod_rechazo_ent = reg_2_array[arr_c].cod_rechazo_ent
								AND entidad = 1
								AND tipo_retiro IN ( "D", "G" )

							IF SQLCA.SQLCODE = NOTFOUND THEN
								ERROR "NO EXISTE ESTE CODIGO DE RECHAZO"
								LET reg_2_array[arr_c].cod_rechazo_ent = NULL
								NEXT FIELD cod_rechazo_ent
							END IF
						ELSE
							-- Se despliega ventana de ayuda de codigo de rechazo
							CALL despliega_cod_rechazo_ent( reg_2_array[arr_c].tipo_ret )
								RETURNING reg_2_array[arr_c].cod_rechazo_ent

							IF( reg_2_array[arr_c].cod_rechazo_ent <= 0 ) THEN
								ERROR "NO EXISTE ESTE CODIGO DE RECHAZO "
								LET reg_2_array[arr_c].cod_rechazo_ent = NULL
								NEXT FIELD cod_rechazo_ent
							END IF
						END IF
						
						DISPLAY BY NAME reg_2_array[arr_c].cod_rechazo_ent
						PROMPT "CONFIRMA RECHAZO DE SOLICITUD (S/N)" FOR CHAR enter
						
						IF enter MATCHES "[NnSs]" THEN
							CASE 
								WHEN enter MATCHES "[Ss]"
									UPDATE ret_cta_vol 
										SET cod_rechazo_ent   = reg_2_array[arr_c].cod_rechazo_ent,
										    estado  = reg_3.rechazado
										WHERE n_seguro = reg_2_array[arr_c].n_seguro
										AND consecutivo = reg_2_array[arr_c].consecutivo

                              LET v_marca_ent = 490

                              LET v_desmarca ="EXECUTE PROCEDURE desmarca_cuenta('",
                                               reg_2_array[arr_c].n_seguro,"',",
                                               v_marca_ent,",",
                                               reg_2_array[arr_c].consecutivo,",",
                                               0 ,",",
                                               0 ,",",
                                               "'",usuario,"'",
                                               ")"
 
                             PREPARE eje_desmarca FROM v_desmarca
                             EXECUTE eje_desmarca
 
									DISPLAY "RECHAZO CONFIRMADO... " AT 21, 2 
										ATTRIBUTE( REVERSE )
									SLEEP 3

								WHEN enter MATCHES "[Nn]"
									DISPLAY "RECHAZO CANCELADO... " AT 21, 2 
										ATTRIBUTE( REVERSE )
									SLEEP 3
							END CASE
						END IF
						EXIT INPUT
					END IF

            ON KEY( CONTROL-B )
					IF( tipo_cons = 1 ) THEN   #* Rech., Solo opcion Consulta *
						LET arr_c = ARR_CURR()
						CASE reg_2_array[arr_c].tipo_pago
							WHEN 1
								CALL consulta_orden_de_pago( reg_2_array[arr_c].consecutivo ) 
							WHEN 2
								CALL consulta_abono_cta( reg_2_array[arr_c].consecutivo )
							WHEN 3
								CALL consulta_cheque( reg_2_array[arr_c].consecutivo ) #cc
						END CASE
					END IF

            ON KEY( INTERRUPT, CONTROL-C )
                DISPLAY "Total Registros               " AT 19, 56
                DISPLAY "         " AT 19, 41
                EXIT INPUT
        END INPUT
    ELSE
        ERROR "   NO EXISTEN REGISTROS" ATTRIBUTE ( NORMAL )
    END IF 
	 CLEAR FORM
END FUNCTION

FUNCTION modifica()
#m-----------------
    DEFINE #loc #char
        c9des_estado_solic    CHAR(009) ,
        hace_el_input         CHAR(300) ,
        hace_el_select        CHAR(500)

    DEFINE #loc #smallint
        s_tot_registros       ,
        arr_c                 ,
        scr_l                 ,
        pos                   ,
        ban                   ,
        estado_mod            SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " [ Ctrl-C ] Regresa al menu "   AT 1,1
    DISPLAY " [ ESC    ] Modifica        "   AT 1,36
    DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE( REVERSE, BOLD )
    DISPLAY " RETM003    DISPOSICION DE RECURSOS DE APORTACIONES VOLUNTARIAS                " AT 8,1 ATTRIBUTE( REVERSE )

    LET ban           = 0
    LET estado_mod    = 0
    LET INT_FLAG      = FALSE
    LET hace_el_input = NULL

    CONSTRUCT hace_el_input ON A.n_folio  ,
                               A.n_seguro ,
                               A.n_rfc    ,
                               A.n_unico  ,
                               A.paterno  ,
                               A.materno  ,
                               A.nombres
                          FROM n_folio    ,
                               n_seguro   ,
                               n_rfc      ,
                               n_unico    ,
                               paterno    ,
                               materno    ,
                               nombres
        ON KEY (ESC)
            ERROR ""
            ERROR " PROCESANDO INFORMACION..."
            LET int_flag = FALSE
            EXIT CONSTRUCT

        ON KEY (CONTROL-C)
            LET int_flag = TRUE
            CLEAR FORM
            EXIT CONSTRUCT
    END CONSTRUCT

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR ""
        ERROR " BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        RETURN
    END IF 

    LET hace_el_select = " SELECT A.n_folio  ,",
                         "        A.n_seguro ,",
                         "        A.n_rfc    ,",
                         "        A.n_unico  ,",
                         "        A.fentcons ,",
                         "        A.paterno  ,",
                         "        A.materno  ,",
                         "        A.nombres  ,",
                         "        USER        ",
                         " FROM   afi_mae_afiliado A, ret_cta_vol B",
                         " WHERE ",hace_el_input CLIPPED,
                         " AND    A.n_seguro = B.n_seguro ",
                         " GROUP BY 1,2,3,4,5,6,7,8,9 ",
                         " ORDER BY 2,3" CLIPPED

    PREPARE pre_8 FROM hace_el_select
    DECLARE cur_8 CURSOR FOR pre_8

    LET pos = 1
    FOREACH cur_8 INTO reg_1.*,c8_usuario   
        LET ban = 0

        DECLARE cur_9 CURSOR FOR
        SELECT 0,
               0,
               0,
               0,
               0,
               0,
               0,
               0,
               0,
               A.fecha_ult_ret,
               A.n_folio_sol,
               A.tipo_ret,
               B.des_tipo_ret,
               A.mto_solic,
               A.porcentaje_solic,
               A.tipo_pago,
               "",                     #des_tipo_pago
               A.edad,
               A.fecha_solic,
               A.fecha_captura,
               A.ultimo_proceso,
               A.pension_invalidez,    #pension_invalidez
               A.deduccion,            #deduccion
               A.fecha_deduccion,      #fecha_deduccion
               A.mto_deducido,         #mto_deducido
					'', 							#* Rech., cod_rechazo_ent *
               A.consecutivo,
               A.usuario,
               A.estado
        FROM  ret_cta_vol A, tab_retiro_old B
        WHERE A.n_seguro  = reg_1.n_seguro
        AND   A.tipo_ret  = B.tipo_ret
		  AND A.cod_rechazo_ent IS NULL      -- Rech. *
        ORDER BY fecha_captura

        FOREACH cur_9 INTO reg_2.*
            LET aux_monto_en_pesos          = 0
            LET precio_dia_sb1              = 0
            LET precio_dia_sb3              = 0
            LET reg_2.monto_en_acciones_sb1 = 0
            LET reg_2.monto_en_acciones_sb3 = 0
            LET reg_2.monto_en_pesos        = 0

            CALL precio_accion( reg_2.fecha_captura, 1 )
            RETURNING precio_dia_sb1

            SELECT NVL( SUM(A.monto_en_acciones), 0 ),
                   NVL( SUM(A.monto_en_acciones), 0 ) * precio_dia_sb1
            INTO   reg_2.monto_en_acciones_sb1,
                   reg_2.monto_en_pesos
            FROM   dis_cuenta A
            WHERE  A.nss             = reg_1.n_seguro
            AND    A.subcuenta       IN( 3, 10 )
            AND    A.fecha_conversion <= reg_2.fecha_captura
            AND    A.siefore         = 1

            LET aux_monto_en_pesos = reg_2.monto_en_pesos
            LET reg_2.monto_en_pesos = 0 

            --sveraCALL precio_accion(reg_2.fecha_captura,3)
            --sveraRETURNING precio_dia_sb3

            SELECT NVL( SUM(A.monto_en_acciones), 0 ),
                   NVL( SUM(A.monto_en_acciones), 0 ) * precio_dia_sb3
            INTO   reg_2.monto_en_acciones_sb3,
                   reg_2.monto_en_pesos
            FROM   dis_cuenta A
            WHERE  A.nss             = reg_1.n_seguro
            AND    A.subcuenta       IN ( 3, 10 )
            AND    A.fecha_conversion <= reg_2.fecha_captura
            AND    A.siefore         = 3

            LET reg_2.monto_en_pesos = reg_2.monto_en_pesos + aux_monto_en_pesos

            SELECT descripcion
            INTO   reg_2.des_tipo_pago
            FROM   tab_pago
            WHERE  tipo_pago = reg_2.tipo_pago

            DISPLAY reg_2.des_tipo_pago TO des_tipo_pago

            LET reg_2_array[pos].var_nula              =  " "
            LET reg_2_array[pos].n_folio               =  reg_1.n_folio
            LET reg_2_array[pos].n_seguro              =  reg_1.n_seguro 
            LET reg_2_array[pos].n_rfc                 =  reg_1.n_rfc   
            LET reg_2_array[pos].n_unico               =  reg_1.n_unico  
            LET reg_2_array[pos].fentcons              =  reg_1.fentcons 
            LET reg_2_array[pos].paterno               =  reg_1.paterno  
            LET reg_2_array[pos].materno               =  reg_1.materno  
            LET reg_2_array[pos].nombres               =  reg_1.nombres 
            LET reg_2_array[pos].monto_en_acciones_sb1 =  reg_2.monto_en_acciones_sb1
            LET reg_2_array[pos].monto_en_acciones_sb3 =  reg_2.monto_en_acciones_sb3
            LET reg_2_array[pos].monto_en_pesos        =  reg_2.monto_en_pesos
            LET reg_2_array[pos].fecha_ult_ret         =  reg_2.fecha_ult_ret
            LET reg_2_array[pos].n_folio_sol           =  reg_2.n_folio_sol
            LET reg_2_array[pos].tipo_ret              =  reg_2.tipo_ret
            LET reg_2_array[pos].des_tipo_ret          =  reg_2.des_tipo_ret
            LET reg_2_array[pos].edad                  =  reg_2.edad
            LET reg_2_array[pos].mto_solic             =  reg_2.mto_solic
            LET reg_2_array[pos].porcentaje_solic      =  reg_2.porcentaje_solic
            LET reg_2_array[pos].tipo_pago             =  reg_2.tipo_pago
            LET reg_2_array[pos].des_tipo_pago         =  reg_2.des_tipo_pago
            LET reg_2_array[pos].fecha_solic           =  reg_2.fecha_solic
            LET reg_2_array[pos].fecha_captura         =  reg_2.fecha_captura
            LET reg_2_array[pos].pension_invalidez     =  reg_2.pension_invalidez
            LET reg_2_array[pos].deduccion             =  reg_2.deduccion
            LET reg_2_array[pos].fecha_deduccion       =  reg_2.fecha_deduccion
            LET reg_2_array[pos].mto_deducido          =  reg_2.mto_deducido
            LET reg_2_array[pos].consecutivo           =  reg_2.consecutivo
            LET reg_2_array[pos].usuario               =  reg_2.usuario
            LET reg_2_array[pos].ultimo_proceso        =  reg_2.ultimo_proceso
            LET reg_2_array[pos].estado                =  reg_2.estado
            LET pos = pos + 1
        END FOREACH
    END FOREACH       

    CALL SET_COUNT( pos - 1 )
    ERROR ""

    LET s_tot_registros = pos - 1
    IF( pos - 1 ) >= 1 THEN
        INPUT ARRAY reg_2_array WITHOUT DEFAULTS FROM scr_3.*
            BEFORE ROW
                LET arr_c = ARR_CURR()
                IF arr_c >= pos THEN
                   ERROR "   NO HAY MAS REGISTROS HACIA ABAJO" ATTRIBUTE( NORMAL )
                END IF

                SELECT descripcion
                INTO   c9des_estado_solic
                FROM   ret_estado A
                WHERE  A.estado_solicitud = reg_2_array[arr_c].estado

                DISPLAY "Total Registros ", s_tot_registros AT 19, 56
                DISPLAY c9des_estado_solic AT 20, 41

            ON KEY( CONTROL-M )
                LET arr_c = ARR_CURR()
                IF reg_2_array[arr_c].estado = reg_3.capturado THEN
                    CALL modifica_seleccion( reg_2_array[arr_c].* ) #ms
                ELSE
                    PROMPT" ESTA SOLICITUD NO SE PUEDE MODIFICAR...<ENTER>",
                          " PARA CONTINUAR" FOR CHAR enter
                    EXIT INPUT
                END IF

            ON KEY( CONTROL-V )
                LET arr_c = ARR_CURR()

                CASE reg_2_array[arr_c].tipo_pago
                    WHEN 2
                       #CALL consulta_abono_cta(reg_2_array[arr_c].consecutivo) 
                    WHEN 3
                        CALL consulta_cheque(reg_2_array[arr_c].consecutivo) #cc
                END CASE

            ON KEY ( INTERRUPT )
                DISPLAY "Total Registros               " AT 19,56
                DISPLAY "         " AT 19,41
                EXIT INPUT

            ON KEY ( CONTROL-C )
                DISPLAY "Total Registros               " AT 19,56
                DISPLAY "         " AT 19,41
                EXIT INPUT
        END INPUT
    ELSE
        ERROR ""
        PROMPT "NO HAY SOLICITUDES...<ENTER> PARA CONTINUAR" FOR CHAR enter
        ERROR ""
    END IF     
CLEAR FORM
END FUNCTION

FUNCTION despliega_tipo_ret()
#dtr-------------------------
    DEFINE l_reg ARRAY[1000] OF RECORD
        codigo                INTEGER ,
        descripcion         CHAR(50)
    END RECORD

    DEFINE
        x_x            CHAR(100) ,
        x_buscar         CHAR(030)

    DEFINE
        pos                   ,
        aux_val               SMALLINT

    OPEN WINDOW pantallap1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
    DISPLAY "              TIPOS  DE COMPROBANTES (ICEFAS)            "
            AT 2,1 ATTRIBUTE( REVERSE )

    INPUT BY NAME x_buscar
        BEFORE FIELD x_buscar
            LET x_buscar = "*"

        AFTER FIELD x_buscar
            IF x_buscar IS NULL THEN
                ERROR ""
                ERROR "   DESCRIPCION A BUSCAR NO PUEDE SER NULA" 
                      ATTRIBUTE( NORMAL )
                NEXT FIELD x_buscar
            ELSE
                EXIT INPUT
            END IF
   END INPUT

   WHILE TRUE
       LET x_x = " SELECT * FROM tab_retiro_old ",
                 " WHERE  des_tipo_ret MATCHES ",'"',x_buscar CLIPPED,'"',
                 " ORDER BY 1 " CLIPPED

       PREPARE pre_4 FROM x_x
       DECLARE cur_4 CURSOR FOR pre_4

       LET pos = 1
       FOREACH cur_4 INTO l_reg[pos].*
           LET pos = pos + 1
           IF pos >= 1000 THEN
                ERROR ""
               ERROR "   FUE SOBREPASADA LA CAPACIDAD MAXIMA DEL ARREGLO" 
                     ATTRIBUTE( NORMAL )
               EXIT FOREACH
           END IF
       END FOREACH

       IF (pos-1) < 1 THEN
                ERROR ""
           ERROR "   CATALOGO CON TIPOS DE RETITOS VACIO" ATTRIBUTE( NORMAL )
       END IF

       CALL SET_COUNT(pos-1)
       DISPLAY ARRAY l_reg TO scr_1.*
           ON KEY ( INTERRUPT )
                 LET pos = 0
                 EXIT DISPLAY

           ON KEY ( CONTROL-C )
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
   CLOSE WINDOW pantallap1
   RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION

FUNCTION despliega_tipo_identif()
#dti-----------------------------
    DEFINE l_reg ARRAY[1000] OF RECORD
        codigo                INTEGER ,
        descripcion         CHAR(50)
    END RECORD

    DEFINE
        x_x            CHAR(100) ,
        x_buscar         CHAR(030)

    DEFINE
        pos                   ,
        aux_val               SMALLINT

    OPEN WINDOW pantallap1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
    DISPLAY "              TIPOS  DE COMPROBANTES (ICEFAS)            "
            AT 2,1 ATTRIBUTE( REVERSE )

    INPUT BY NAME x_buscar
        BEFORE FIELD x_buscar
            LET x_buscar = "*"

        AFTER FIELD x_buscar
       IF x_buscar IS NULL THEN
           ERROR ""
           ERROR "   DESCRIPCION A BUSCAR NO PUEDE SER NULA" ATTRIBUTE( NORMAL )
           NEXT FIELD x_buscar
       ELSE
           EXIT INPUT
       END IF
   END INPUT

   WHILE TRUE
       LET x_x = " SELECT * FROM tab_identificacion ",
              " WHERE  des_tipo_identif MATCHES ",'"',x_buscar CLIPPED,'"',
              " ORDER BY 1 " CLIPPED

       PREPARE pre_6 FROM x_x
       DECLARE cur_6 CURSOR FOR pre_6

       LET pos = 1
       FOREACH cur_6 INTO l_reg[pos].*
           LET pos = pos + 1
           IF pos >= 1000 THEN
              ERROR ""
              ERROR "   FUE SOBREPASADA LA CAPACIDAD MAXIMA DEL ARREGLO" 
                    ATTRIBUTE( NORMAL )
               EXIT FOREACH
           END IF
       END FOREACH

       IF (pos-1) < 1 THEN
              ERROR ""
           ERROR "   CATALOGO CON TIPOS DE IDENTIFICACIONES VACIO" 
                 ATTRIBUTE( NORMAL )
       END IF

       CALL SET_COUNT(pos-1)
       DISPLAY ARRAY l_reg TO scr_1.*
           ON KEY ( INTERRUPT )
                LET pos = 0
                EXIT DISPLAY

           ON KEY ( CONTROL-C )
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
   CLOSE WINDOW pantallap1
   RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION

{
FUNCTION RechazaSolic( l_treti )
	DEFINE
		l_treti    				CHAR(1),
		cod_rechazo_ent  		LIKE ret_parcial.cod_rechazo_ent,
		cod_rec_ent_des  		CHAR(60),
		ventidad 				SMALLINT

   --SELECT e.entidad INTO ventidad FROM tab_entidad e
	--WHERE e.descripcion = "AFORE"
   LET ventidad = 1

	INPUT BY NAME cod_rechazo_ent
		AFTER FIELD cod_rechazo_ent
			IF FGL_LASTKEY() = FGL_KEYVAL( "LEFT" ) THEN
				LET cod_rechazo_ent = NULL
				NEXT FIELD PREVIOUS
			END IF

		IF cod_rechazo_ent IS NOT NULL THEN
			#* Valida que el codigo sea valido en catalogo de rechazos *
			SELECT r.cod_rechazo_ent
				FROM ret_rechazo_grl r
				WHERE r.cod_rechazo_ent = cod_rechazo_ent
				AND r.entidad = ventidad 
				AND r.tipo_retiro IN ( "D", "G" )         

			IF SQLCA.SQLCODE = NOTFOUND THEN
				ERROR "NO EXISTE ESTE CODIGO DE RECHAZO"
				LET cod_rechazo_ent = NULL
				NEXT FIELD cod_rechazo_ent
			ELSE
				DISPLAY BY NAME cod_rechazo_ent
				EXIT INPUT
			END IF
		ELSE      #* Se despliega ventana de ayuda de codigo de rechazo *
			CALL despliega_cod_rechazo_ent( l_treti )
				RETURNING cod_rechazo_ent --, cod_rec_ent_des

			IF cod_rechazo_ent <= 0 THEN
				ERROR "NO EXISTE ESTE CODIGO DE RECHAZO "
				LET cod_rechazo_ent = NULL
				NEXT FIELD cod_rechazo_ent
			END IF
			DISPLAY BY NAME cod_rechazo_ent
			EXIT INPUT
		END IF
	END INPUT
	RETURN cod_rechazo_ent, ventidad
END FUNCTION
}

FUNCTION despliega_cod_rechazo_ent( tipo_retiro )
	DEFINE 
		l_reg 					ARRAY[100] OF RECORD
			cod_rechazo_ent   SMALLINT,
			des_corta         CHAR(60)
		END RECORD,
		x_x               	CHAR(200),
		x_buscar          	CHAR(030),
		codigo, pos       	SMALLINT,
		descripcion       	CHAR(60),
		tipo_retiro, tr      CHAR(1)

	LET tr = tipo_retiro
	OPEN WINDOW retm0064 AT 5, 10 WITH FORM "RETM0064" ATTRIBUTE( BORDER )
	DISPLAY "                      TIPOS DE RECHAZOS                  " 
		AT 2, 1 ATTRIBUTE( REVERSE )
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
		LET x_x = " SELECT cod_rechazo_ent, des_larga FROM ret_rechazo_grl ",
			"WHERE des_corta MATCHES ", '"', x_buscar CLIPPED, '" ',
			--"AND tipo_retiro = '", tr CLIPPED, "' ",
			"AND tipo_retiro IN ( 'D', 'G' ) ",
			"AND entidad = 1 ",
			"ORDER BY 1 "

		PREPARE pre_9 FROM x_x
		DECLARE cur_91 CURSOR FOR pre_9
		LET pos = 1
		FOREACH cur_91 INTO l_reg[pos].*
			LET pos = pos + 1
			IF pos >= 1000 THEN
				ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
				EXIT FOREACH
			END IF
		END FOREACH

		IF( pos - 1 ) < 1 THEN
			ERROR "CATALOGO DE RECHAZOS VACIO "
		END IF

		CALL SET_COUNT( pos - 1 )
		DISPLAY ARRAY l_reg TO scr_1.*
			ON KEY ( CONTROL-C )
				LET pos = 0
				EXIT DISPLAY

			ON KEY ( INTERRUPT )
				LET pos = 0
				EXIT DISPLAY

			ON KEY ( CONTROL-M )
				LET pos = ARR_CURR()
				LET codigo = l_reg[pos].cod_rechazo_ent
				LET descripcion = l_reg[pos].des_corta
				EXIT DISPLAY
		END DISPLAY
		IF pos <> 0 THEN
			EXIT WHILE
		END IF
	END WHILE
	CLOSE WINDOW retm0064
	RETURN codigo   --, descripcion
END FUNCTION

FUNCTION liquidacion()
#l--------------------
	DEFINE #loc #reg_5
		reg_5 RECORD            	LIKE ret_cta_vol.*
		DEFINE arr_1 ARRAY[100] OF RECORD
			marca                   CHAR(01) ,
			n_seguro                CHAR(11) ,
			nombre                  CHAR(43) ,
			fecha_solic             DATE     ,       
			monto_en_pesos          DECIMAL(22,6)
		END RECORD
		DEFINE reg_6 RECORD    #loc #reg_6
			n_seguro                CHAR(11) ,
			paterno                 CHAR(40) ,
			materno                 CHAR(40) ,
			nombres                 CHAR(40) ,
			tipo_ret                SMALLINT ,
			fecha_solic             DATE     ,
			mto_solic               DECIMAL(16,6)
		END RECORD
		DEFINE reg_7 RECORD    #loc #reg_7
			fecha_liquidacion       DATE
		END RECORD
		DEFINE    #loc #integer
			pos, arr_c, arr_l       INTEGER,
			totreg, band_X			  	SMALLINT     #* No. de Regs. a Liquidar *
		DEFINE    #loc #decimal
			prov_tot_acciones       DECIMAL(16,6)

	INITIALIZE reg_5.*, reg_6.*, reg_7.* TO NULL
	LET ultimo_folio = 0
	LET pos = 0
	LET arr_c = 0
	LET arr_l = 0
	LET prov_tot_acciones = 0
	LET totreg = 0
	LET band_X = FALSE
    
	FOR pos = 1 TO 100
		INITIALIZE arr_1[pos].* TO NULL
	END FOR
	LET pos = 0

	OPEN WINDOW retm0032 AT 3,2 WITH FORM "RETM0032" ATTRIBUTE( BORDER )
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " <Ctrl-B> Detalle Acciones a Liquidar                      <Ctrl-C> Salir                                   " AT 1,1 ATTRIBUTE ( REVERSE )
	DISPLAY " RETM003          LIQUIDACION DE APORTACIONES VOLUNTARIAS                      " AT 3,1 ATTRIBUTE( REVERSE )
	DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE( REVERSE )

	DECLARE cur_1 CURSOR FOR
		SELECT n_seguro, paterno, materno, nombres,
			tipo_ret, fecha_solic, mto_solic
			FROM ret_cta_vol
			WHERE estado IN ( 0, 3 ) AND cod_rechazo_ent IS NULL   -- Rech. *
	LET pos = 0
	FOREACH cur_1 INTO reg_6.*
		LET pos = pos + 1
		LET arr_1[pos].n_seguro = reg_6.n_seguro
		LET arr_1[pos].nombre[1,26] = reg_6.paterno CLIPPED, " ",
			reg_6.materno CLIPPED, " ", reg_6.nombres CLIPPED
		LET arr_1[pos].marca = 'X'

		IF reg_6.tipo_ret = 2 THEN
			LET monto_acc_sb1   = 0
			LET monto_acc_sb3   = 0
			LET monto_pesos_sb1 = 0
			LET monto_pesos_sb3 = 0

			CALL calcula_mto_solic( reg_6.n_seguro, HOY, reg_6.mto_solic )
				RETURNING monto_acc_sb1, monto_acc_sb3, monto_pesos_sb1,
					monto_pesos_sb3

			IF monto_pesos_sb1 IS NULL THEN
				LET monto_pesos_sb1 = 0
			END IF
			IF monto_pesos_sb3 IS NULL THEN
				LET monto_pesos_sb3 = 0
			END IF

			LET arr_1[pos].monto_en_pesos = monto_pesos_sb1 + monto_pesos_sb3
			LET prov_tot_acciones = prov_tot_acciones + arr_1[pos].monto_en_pesos
		ELSE
			LET arr_1[pos].monto_en_pesos = reg_6.mto_solic
			LET prov_tot_acciones = prov_tot_acciones + arr_1[pos].monto_en_pesos
		END IF
		LET arr_1[pos].fecha_solic = reg_6.fecha_solic 
	END FOREACH

	DISPLAY prov_tot_acciones TO total_en_acciones
	CALL SET_COUNT( pos )
	LET totreg = pos       #* Pone el total de Regs. obtenidos por fecha *
	LET sw_1 = 0

	INPUT ARRAY arr_1 WITHOUT DEFAULTS FROM scr_1.*
		BEFORE FIELD monto_en_pesos
			LET arr_c = ARR_CURR()
			LET arr_l = SCR_LINE()

		AFTER FIELD monto_en_pesos

		ON KEY( CONTROL-B )
			CALL detalle_acc_liq() #dal

		ON KEY( ESC )
			LET band_X = FALSE
			FOR pos = 1 TO totreg
				IF arr_1[pos].marca = "X" THEN
					LET band_X = TRUE
				END IF
			END FOR
			IF NOT band_X THEN
				ERROR "NO HAY SOLICITUDES MARCADAS"
				SLEEP 3
				NEXT FIELD marca
			END IF
			WHILE TRUE
				PROMPT " ESTA SEGURO DE QUE SON TODAS LAS SOLICITUDES QUE ",
					"DESEA LIQUIDAR (S/N): " FOR CHAR enter
				IF enter MATCHES "[NnSs]" THEN
					EXIT WHILE
				END IF
			END WHILE
			IF enter MATCHES "[Nn]" THEN
				NEXT FIELD marca
			END IF
			EXIT INPUT

		ON KEY( INTERRUPT )
			LET sw_1 = 1
			EXIT INPUT
	END INPUT
	IF sw_1 = 1 THEN
		CLOSE WINDOW retm0032
		RETURN
	END IF
	LET reg_7.fecha_liquidacion = HOY

	INPUT BY NAME reg_7.fecha_liquidacion WITHOUT DEFAULTS
		BEFORE FIELD fecha_liquidacion
			DISPLAY "FECHA DE LIQUIDACION :" AT 19, 9 

		AFTER FIELD fecha_liquidacion
			IF reg_7.fecha_liquidacion IS NULL THEN
				ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
				NEXT FIELD fecha_liquidacion
			END IF

		ON KEY( ESC )
			IF reg_7.fecha_liquidacion IS NULL THEN
				ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
				NEXT FIELD fecha_liquidacion
			END IF
			EXIT INPUT

		ON KEY( INTERRUPT )
			LET sw_1 = 1
			EXIT INPUT

		ON KEY( CONTROL-C )
			LET sw_1 = 1
			EXIT INPUT
	END INPUT
	IF sw_1 = 1 THEN
		CLOSE WINDOW retm0032
		RETURN
	END IF

	--CALL valida_cta_saldo_vol()   #* Invocacion original erronea *

	FOR pos = 1 TO 100
		IF arr_1[pos].marca = "X" THEN
			CALL valida_cta_saldo_vol( arr_1[pos].n_seguro )    #* Asi debe ser *

			UPDATE ret_cta_vol
				SET ret_cta_vol.estado = 7,
				ret_cta_vol.fecha_ult_ret = reg_7.fecha_liquidacion
				WHERE  ret_cta_vol.n_seguro = arr_1[pos].n_seguro 
					AND ret_cta_vol.estado IN( 0, 3 )  ------------------CAPTURADO
		END IF
	END FOR

	DECLARE cur_2 CURSOR FOR
		SELECT * FROM ret_cta_vol WHERE estado = 7 

	SELECT MAX( folio ) + 1 INTO ultimo_folio FROM glo_folio
	IF ultimo_folio IS NULL THEN
		LET ultimo_folio = 1
	END IF

	INSERT INTO glo_folio VALUES( ultimo_folio )

	FOREACH cur_2 INTO reg_5.*
		IF reg_5.tipo_ret = "2" THEN -- RETIRO TOTAL --
			CALL calcula_saldo( reg_5.n_seguro, reg_5.consecutivo,
				reg_5.fecha_ult_ret, reg_5.mto_en_acc_3 )   #cs

			UPDATE ret_cta_vol
				SET ret_cta_vol.n_folio_liq = ultimo_folio ,
				ret_cta_vol.estado = 8
				WHERE  ret_cta_vol.n_seguro = reg_5.n_seguro
					AND ret_cta_vol.estado = 7
		ELSE
			-- RETIRO PARCIAL --
			LET monto_acc_sb1   = 0
			LET monto_acc_sb3   = 0
			LET monto_pesos_sb1 = 0
			LET monto_pesos_sb3 = 0

			CALL calcula_mto_solic( reg_5.n_seguro, reg_5.fecha_ult_ret, 
				reg_5.mto_solic ) RETURNING monto_acc_sb1, monto_acc_sb3, 
				monto_pesos_sb1, monto_pesos_sb3

			IF monto_acc_sb1 IS NULL THEN
				LET monto_acc_sb1 = 0
			END IF
			IF monto_acc_sb3 IS NULL THEN
				LET monto_acc_sb3 = 0
			END IF
			CALL calcula_saldo_3( reg_5.n_seguro, reg_5.consecutivo,
				reg_5.fecha_ult_ret, monto_acc_sb1 + monto_acc_sb3 )   #cs3

			UPDATE ret_cta_vol_scot 
				SET ret_cta_vol_scot.liquidado_sn = "S"
					WHERE  ret_cta_vol_scot.n_seguro = reg_5.n_seguro
						AND ret_cta_vol_scot.consecutivo = reg_5.consecutivo

			UPDATE ret_cta_vol
            SET ret_cta_vol.n_folio_liq = ultimo_folio, ret_cta_vol.estado = 8
					WHERE ret_cta_vol.n_seguro = reg_5.n_seguro
						AND ret_cta_vol.estado = 7
		END IF
		LET v_marca_ent = 490
		LET v_desmarca2 = "EXECUTE PROCEDURE desmarca_cuenta('",
			reg_5.n_seguro, "',", v_marca_ent, ",", reg_5.consecutivo, ",",
			0 ,",", 0 , ",", "'", usuario, "'", ")"

		PREPARE eje_desmarca2 FROM v_desmarca2
		EXECUTE eje_desmarca2
	END FOREACH 
	DISPLAY "FOLIO : ", ultimo_folio, "" AT 19, 2
	PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
	CLOSE WINDOW RETM0032
END FUNCTION

FUNCTION calcula_saldo(reg_09)
#cs---------------------------
    DEFINE reg_09 RECORD #loc #reg_09
        nss                   CHAR(11)    ,
        consecutivo           INTEGER     ,
        fecha_retiro          DATE        ,
        monto_en_acciones     DECIMAL(16,6)
    END RECORD

    DEFINE reg_10 RECORD #loc #reg_10
        subcuenta             SMALLINT      ,
        fecha_conversion      DATE          ,
        saldo_acciones        DECIMAL(16,6) ,
        siefore               SMALLINT
    END RECORD

    DEFINE reg_12 RECORD #loc #reg_12
        mto_neto_pesos        DECIMAL(16,6) ,
        mto_rete_pesos        DECIMAL(16,6) ,
        mto_neto_accion       DECIMAL(16,6) ,
        mto_rete_accion       DECIMAL(16,6) ,
        mto_rendimiento       DECIMAL(16,6) 
    END RECORD

    DEFINE reg_14 RECORD #loc #reg_14
        deduccion             CHAR(1)       ,
        fecha_deduccion       DATE          ,
        pension_invalidez     CHAR(1)       ,
        edad                  SMALLINT      , 
        mto_solic             DECIMAL(16,6)
    END RECORD

    DEFINE reg_16 RECORD #loc #reg_16
        subcuenta             SMALLINT      ,
        fecha_conversion      DATE          ,
        saldo_acciones        DECIMAL(16,6) ,
        siefore               SMALLINT
    END RECORD

    DEFINE #loc #decimal
        aux_mto_neto_accion   ,
        vmto_ret20_acc        ,
        vmto_ret20_pes        ,
        v_mto_isr20           ,
        vtot_rete_acc_sub3    ,
        vtot_rete_acc_sub10   DECIMAL(16,6)

    DEFINE #loc #smallint
        reg_inv               ,
        reg_inv_sucta3        ,        
        reg_inv_sucta10       SMALLINT


    LET aux_mto_neto_accion    = 0
    LET reg_10.saldo_acciones  = 0
    LET reg_12.mto_neto_pesos  = 0
    LET reg_12.mto_rete_pesos  = 0
    LET reg_12.mto_neto_accion = 0
    LET reg_12.mto_rete_accion = 0
    LET reg_12.mto_rendimiento = 0 
    LET reg_16.saldo_acciones  = 0
    LET vmto_ret20_pes         = 0
    LET vmto_ret20_acc         = 0
    LET vtot_rete_acc_sub3     = 0
    LET vtot_rete_acc_sub10    = 0
    LET vprecio_dia            = 0
    LET v_mto_isr20            = 0
    LET monto_acc_sb1          = 0
    LET monto_acc_sb3          = 0
    LET monto_pesos_sb1        = 0
    LET monto_pesos_sb3        = 0
    LET vmto_solic_pesos       = 0


    SELECT codigo_siefore
    INTO   reg_inv_sucta3
    FROM   cta_regimen 
    WHERE  nss       = reg_09.nss
    AND    subcuenta = 3

    SELECT codigo_siefore
    INTO   reg_inv_sucta10
    FROM   cta_regimen
    WHERE  nss       = reg_09.nss
    AND    subcuenta = 10

    SELECT fecha_traspaso
    INTO   vfecha_traspaso
    FROM   tes_solicitud
    WHERE  nss           = reg_09.nss
    AND    tipo_traspaso = 9 
    AND    estado        = 103

    IF STATUS <> NOTFOUND THEN
        LET ind_cambio_recursos = "S"
    ELSE
        LET ind_cambio_recursos = "N"
    END IF

    SELECT deduccion         ,
           fecha_deduccion   ,
           pension_invalidez ,
           edad              ,
           mto_solic 
    INTO   reg_14.*
    FROM   ret_cta_vol
    WHERE  n_seguro    = reg_09.nss
    AND    consecutivo = reg_09.consecutivo

    CALL calcula_mto_solic(reg_09.nss,reg_09.fecha_retiro,reg_14.mto_solic)
    RETURNING monto_acc_sb1,
              monto_acc_sb3,
              monto_pesos_sb1,
              monto_pesos_sb3

    IF monto_pesos_sb1 IS NULL THEN
        LET monto_pesos_sb1 = 0
    END IF

    IF monto_pesos_sb3 IS NULL THEN
        LET monto_pesos_sb3 = 0
    END IF

    LET vmto_solic_pesos = monto_pesos_sb1 + monto_pesos_sb3

    IF reg_14.pension_invalidez = "S"
    OR reg_14.edad >= 65
    OR reg_14.deduccion = "N" THEN
        LET vban = 1
    ELSE
        IF reg_14.deduccion = "S" THEN

            LET vban = 2

            CALL calcula_isr_20(reg_09.nss)
            RETURNING v_mto_isr20

            IF v_mto_isr20 IS NULL
            OR v_mto_isr20 = " "
            OR v_mto_isr20 < 0  THEN
                LET v_mto_isr20 = 0
            END IF
        END IF
    END IF

    DECLARE cur_3 CURSOR FOR
    SELECT  subcuenta        ,
            fecha_conversion ,
            saldo_acciones   ,
            siefore 
    FROM    cta_saldo_vol
    WHERE   nss            = reg_09.nss
    AND     saldo_acciones > 0
    AND     subcuenta in (3,10)
    ORDER BY fecha_conversion asc

    LET sw_10 = 1
    FOREACH cur_3 INTO reg_10.*
        IF reg_10.subcuenta = 3 THEN
            LET reg_inv = reg_inv_sucta3
        ELSE
            LET reg_inv = reg_inv_sucta10
        END IF 
             
        IF reg_inv = 1  
        OR ind_cambio_recursos = "N" THEN

            CALL calculo_ISR(reg_09.nss, 
                             reg_10.fecha_conversion ,
                             reg_09.fecha_retiro     ,
                             reg_10.saldo_acciones   , 
                             reg_10.saldo_acciones   ,
                             reg_10.siefore          ,
                             reg_10.siefore
                            )#cisr
            RETURNING reg_12.mto_neto_accion ,
                      reg_12.mto_rete_accion ,
                      reg_12.mto_rendimiento

            LET aux_mto_neto_accion = reg_12.mto_neto_accion

            IF  vban = 1 THEN

                CALL liquida_total(reg_09.nss              ,
                                   reg_09.consecutivo      ,
                                   reg_09.fecha_retiro     ,
                                   reg_10.fecha_conversion ,
                                   reg_10.subcuenta        ,
                                   reg_10.saldo_acciones   ,
                                   reg_12.mto_neto_accion  ,
                                   reg_12.mto_rete_accion  ,
                                   reg_12.mto_rendimiento  ,
                                   0                       ,
                                   0                       ,
                                   reg_10.siefore          ,
                                   reg_12.mto_neto_accion  ,
                                   1                       )#lt
            ELSE
                CALL calcula_saldo_20(reg_09.nss            ,
                                      2                     ,
                                      reg_10.saldo_acciones ,
                                      reg_10.siefore        ,
                                      v_mto_isr20           ,
                                      vmto_solic_pesos )#cs2
                RETURNING vmto_ret20_pes ,
                          vmto_ret20_acc ,
                          vprecio_dia 

                LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                LET reg_12.mto_neto_accion = reg_12.mto_neto_accion - vmto_ret20_acc

                LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                CALL liquida_total(reg_09.nss              , -- LIQUIDA NETO Y 0.05% ISR  --
                                   reg_09.consecutivo      ,
                                   reg_09.fecha_retiro     ,
                                   reg_10.fecha_conversion ,
                                   reg_10.subcuenta        ,
                                   reg_10.saldo_acciones   ,
                                   reg_12.mto_neto_accion  ,
                                   reg_12.mto_rete_accion  ,
                                   reg_12.mto_rendimiento  ,
                                   0                       ,
                                   0                       ,
                                   reg_10.siefore          ,
                                   aux_mto_neto_accion     ,
                                   1                       )#lt

                CALL liquida_total_20(reg_09.nss               , -- LIQUIDA 20% ISR --
                                      reg_09.consecutivo       ,
                                      reg_09.fecha_retiro      ,
                                      reg_10.fecha_conversion  ,
                                      reg_10.subcuenta         ,
                                      -vmto_ret20_acc          ,
                                      -vmto_ret20_pes          ,
                                      reg_10.siefore           ,
                                      vprecio_dia              )#lt2
            END IF
        ELSE 
            IF reg_10.fecha_conversion < vfecha_traspaso
            AND reg_10.siefore = 3 THEN

                CALL calculo_ISR(reg_09.nss,
                                 reg_10.fecha_conversion ,
                                 reg_09.fecha_retiro     ,
                                 reg_10.saldo_acciones   ,
                                 reg_10.saldo_acciones   ,--tot_accion
                                 reg_10.siefore          ,
                                 reg_inv
                                )#cisr
                RETURNING reg_12.mto_neto_accion ,
                          reg_12.mto_rete_accion ,
                          reg_12.mto_rendimiento

                LET aux_mto_neto_accion = reg_12.mto_neto_accion

                IF vban = 1 THEN
                    CALL liquida_total(reg_09.nss              ,
                                       reg_09.consecutivo      ,
                                       reg_09.fecha_retiro     ,
                                       reg_10.fecha_conversion ,
                                       reg_10.subcuenta        ,
                                       reg_10.saldo_acciones   ,
                                       reg_12.mto_neto_accion  ,
                                       reg_12.mto_rete_accion  ,
                                       reg_12.mto_rendimiento  ,
                                       0                       ,
                                       0                       ,
                                       3                       ,
                                       aux_mto_neto_accion     ,   
                                       1                        )#lt
                ELSE
                    CALL calcula_saldo_20(reg_09.nss            ,
                                          2                     ,
                                          reg_10.saldo_acciones ,
                                          reg_10.siefore        ,
                                          v_mto_isr20           ,
                                          vmto_solic_pesos )#cs2
                    RETURNING vmto_ret20_pes ,
                              vmto_ret20_acc ,
                              vprecio_dia 

                    LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                    LET reg_12.mto_neto_accion = reg_12.mto_neto_accion - vmto_ret20_acc

                    LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                    CALL liquida_total(reg_09.nss              , -- LIQUIDA NETO Y 0.05% ISR  --
                                   reg_09.consecutivo      ,
                                   reg_09.fecha_retiro     ,
                                   reg_10.fecha_conversion ,
                                   reg_10.subcuenta        ,
                                   reg_10.saldo_acciones   ,
                                   reg_12.mto_neto_accion  ,
                                   reg_12.mto_rete_accion  ,
                                   reg_12.mto_rendimiento  ,
                                   0                       ,
                                   0                       ,
                                   reg_10.siefore          ,
                                   aux_mto_neto_accion     ,
                                   1                       )#lt

                   CALL liquida_total_20(reg_09.nss               , -- LIQUIDA 20% ISR --
                                         reg_09.consecutivo       ,
                                         reg_09.fecha_retiro      ,
                                         reg_10.fecha_conversion  ,
                                         reg_10.subcuenta         ,
                                         -vmto_ret20_acc          ,
                                         -vmto_ret20_pes          ,
                                         reg_10.siefore           ,
                                         vprecio_dia              )#lt2
                END IF
            END IF

            IF reg_10.fecha_conversion = vfecha_traspaso THEN

                DECLARE cur_5 CURSOR FOR 
                SELECT  subcuenta        ,
                        fecha_conversion ,
                        saldo_acciones   ,
                        siefore
                FROM    cta_saldo_vol
                WHERE   nss            = reg_09.nss
                AND     saldo_acciones > 0
                AND     subcuenta =  reg_10.subcuenta
                AND     fecha_conversion < vfecha_traspaso
                ORDER BY fecha_conversion asc
 
                FOREACH cur_5 INTO reg_16.*                    

                    CALL calculo_ISR(reg_09.nss,
                                     reg_16.fecha_conversion ,
                                     reg_09.fecha_retiro     ,
                                     reg_16.saldo_acciones   ,
                                     reg_16.saldo_acciones   ,
                                     reg_16.siefore          ,
                                     reg_inv
                                    )#cisr
                    RETURNING reg_12.mto_neto_accion ,
                              reg_12.mto_rete_accion ,
                              reg_12.mto_rendimiento
 

                    LET reg_12.mto_rete_accion = reg_12.mto_rete_accion * -1

                    IF reg_16.subcuenta = 3 THEN
                        LET vtot_rete_acc_sub3 = vtot_rete_acc_sub3 + reg_12.mto_rete_accion
                    ELSE
                        LET vtot_rete_acc_sub10 = vtot_rete_acc_sub10 + reg_12.mto_rete_accion
                    END IF

                END FOREACH

                IF reg_10.subcuenta = 3 THEN
 
                    LET reg_12.mto_neto_accion = reg_10.saldo_acciones - vtot_rete_acc_sub3

                    LET reg_12.mto_rete_accion = vtot_rete_acc_sub3 * -1     
                ELSE
                    LET reg_12.mto_neto_accion = reg_10.saldo_acciones - vtot_rete_acc_sub10

                    LET reg_12.mto_rete_accion = vtot_rete_acc_sub10 * -1
                END IF

                LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                LET aux_mto_neto_accion = reg_12.mto_neto_accion

                IF vban = 1 THEN
                    CALL liquida_total(reg_09.nss              ,
                                       reg_09.consecutivo      ,
                                       reg_09.fecha_retiro     ,
                                       reg_10.fecha_conversion ,
                                       reg_10.subcuenta        ,
                                       reg_10.saldo_acciones   ,
                                       reg_12.mto_neto_accion  ,
                                       reg_12.mto_rete_accion  ,
                                       reg_12.mto_rendimiento  ,
                                       0                       ,
                                       0                       ,
                                       3                       ,
                                       aux_mto_neto_accion     ,
                                       1                       )#lt

                ELSE
                    CALL calcula_saldo_20(reg_09.nss            ,
                                          2                     ,
                                          reg_10.saldo_acciones ,
                                          reg_10.siefore        ,
                                          v_mto_isr20           ,
                                          vmto_solic_pesos )#cs2
                    RETURNING vmto_ret20_pes ,
                              vmto_ret20_acc ,
                              vprecio_dia 
       
                    LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                    LET reg_12.mto_neto_accion = reg_12.mto_neto_accion - vmto_ret20_acc

                    LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                    CALL liquida_total(reg_09.nss              , -- LIQUIDA NETO Y 0.05% ISR  --
                                       reg_09.consecutivo      ,
                                       reg_09.fecha_retiro     ,
                                       reg_10.fecha_conversion ,
                                       reg_10.subcuenta        ,
                                       reg_10.saldo_acciones   ,
                                       reg_12.mto_neto_accion  ,
                                       reg_12.mto_rete_accion  ,
                                       reg_12.mto_rendimiento  ,
                                       0                       ,
                                       0                       ,
                                       3                       ,
                                       aux_mto_neto_accion     ,
                                       1                       )#lt

                    CALL liquida_total_20(reg_09.nss               , -- LIQUIDA 20% ISR --
                                          reg_09.consecutivo       ,
                                          reg_09.fecha_retiro      ,
                                          reg_10.fecha_conversion  ,
                                          reg_10.subcuenta         ,
                                          -vmto_ret20_acc          ,
                                          -vmto_ret20_pes          ,
                                          reg_10.siefore           ,
                                          vprecio_dia              )#lt2
                END IF
            END IF

            IF reg_10.fecha_conversion > vfecha_traspaso THEN

                CALL calculo_ISR(reg_09.nss,
                                 reg_10.fecha_conversion ,
                                 reg_09.fecha_retiro     ,
                                 reg_10.saldo_acciones   ,
                                 reg_10.saldo_acciones   ,--tot_accion
                                 reg_10.siefore          ,
                                 reg_inv )#cisr
                RETURNING reg_12.mto_neto_accion ,
                          reg_12.mto_rete_accion ,
                          reg_12.mto_rendimiento

                LET aux_mto_neto_accion = reg_12.mto_neto_accion

                IF vban = 1 THEN
                    CALL liquida_total(reg_09.nss              ,
                                       reg_09.consecutivo      ,
                                       reg_09.fecha_retiro     ,
                                       reg_10.fecha_conversion ,
                                       reg_10.subcuenta        ,
                                       reg_10.saldo_acciones   ,
                                       reg_12.mto_neto_accion  ,
                                       reg_12.mto_rete_accion  ,
                                       reg_12.mto_rendimiento  ,
                                       0                       ,
                                       0                       ,
                                       3                       ,
                                       aux_mto_neto_accion     ,   
                                       1                        )#lt
                ELSE
                    CALL calcula_saldo_20(reg_09.nss            ,
                                          2                     ,
                                          reg_10.saldo_acciones ,
                                          reg_10.siefore        ,
                                          v_mto_isr20           ,
                                          vmto_solic_pesos )#cs2
                    RETURNING vmto_ret20_pes ,
                              vmto_ret20_acc ,
                              vprecio_dia 

                    LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                    LET reg_12.mto_neto_accion = reg_12.mto_neto_accion - vmto_ret20_acc

                    LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                    CALL liquida_total(reg_09.nss              , -- LIQUIDA NETO Y 0.05% ISR  --
                                   reg_09.consecutivo      ,
                                   reg_09.fecha_retiro     ,
                                   reg_10.fecha_conversion ,
                                   reg_10.subcuenta        ,
                                   reg_10.saldo_acciones   ,
                                   reg_12.mto_neto_accion  ,
                                   reg_12.mto_rete_accion  ,
                                   reg_12.mto_rendimiento  ,
                                   0                       ,
                                   0                       ,
                                   reg_10.siefore          ,
                                   aux_mto_neto_accion     ,
                                   1                       )#lt

                   CALL liquida_total_20(reg_09.nss               , -- LIQUIDA 20% ISR --
                                         reg_09.consecutivo       ,
                                         reg_09.fecha_retiro      ,
                                         reg_10.fecha_conversion  ,
                                         reg_10.subcuenta         ,
                                         -vmto_ret20_acc          ,
                                         -vmto_ret20_pes          ,
                                         reg_10.siefore           ,
                                         vprecio_dia              )#lt2
                END IF
            END IF
        END IF
    END FOREACH
END FUNCTION

FUNCTION calcula_isr_20( nss )
#ci2-----------------------
    DEFINE #loc #smallint
        subcuenta             ,                                 
        grupo                 ,
        v_subcuenta           ,
        v_siefore             ,
        v_tipo_ret            SMALLINT

    DEFINE #loc #decimal
        v_mto_solic           ,
        v_saldo               ,
        v_mto_acc             ,
        v_mto_pes             ,
        v_mto_real_deduc      ,
        v_inpc_deduccion      ,
        v_inpc_actual         ,
        v_factor_inpc         ,
        v_isr_deduc           ,
        v_mto_deducido        DECIMAL (16,6)

    DEFINE #loc #date     
        fecha_saldo           ,
        v_fecha_deduccion     DATE

    DEFINE #loc #char
        nss                   CHAR(011),
        v_saldo_dia           CHAR(100)

    LET v_saldo          = 0
    LET v_mto_solic      = 0
    LET v_mto_acc        = 0
    LET v_mto_pes        = 0
    LET v_mto_real_deduc = 0
    LET v_inpc_deduccion = 0
    LET v_inpc_actual    = 0
    LET v_factor_inpc    = 0
    LET v_isr_deduc      = 0
    LET v_mto_deducido   = 0

    SELECT tipo_ret       ,
           mto_solic      ,
           mto_deducido   ,
           fecha_deduccion
    INTO   v_tipo_ret       ,
           v_mto_solic      ,
           v_mto_deducido   ,
           v_fecha_deduccion
    FROM   ret_cta_vol
    WHERE  n_seguro = nss
    AND    estado   = 7

    CALL  precio_accion(HOY,1)
    RETURNING precio_dia_sb1

    --sveraCALL  precio_accion(HOY,3)
    --sveraRETURNING precio_dia_sb3

    SELECT SUM(A.monto_en_acciones) * precio_dia_sb1
    INTO   v_mto_pes
    FROM   dis_cuenta A
    WHERE  A.nss       = nss
    AND    A.subcuenta IN (3,10)
    AND    A.siefore   = 1

    IF v_mto_pes IS NULL
    OR v_mto_pes = " " THEN
        LET v_mto_pes = 0
    END IF

    LET v_saldo = v_saldo + v_mto_pes

    SELECT SUM(A.monto_en_acciones) * precio_dia_sb3
    INTO   v_mto_pes
    FROM   dis_cuenta A
    WHERE  A.nss       = nss
    AND    A.subcuenta IN (3,10)
    AND    A.siefore   = 3

    IF v_mto_pes IS NULL
    OR v_mto_pes = " " THEN
        LET v_mto_pes = 0
    END IF

    LET v_saldo = v_saldo + v_mto_pes

    IF v_tipo_ret = 2 THEN
        LET v_mto_solic = v_saldo
    END IF

    LET v_mto_real_deduc = v_mto_solic + v_mto_deducido - v_saldo

    --  Obtiene Factor para calcular valor presente.
 
    CALL valores_inpc(v_fecha_deduccion)
    RETURNING v_inpc_actual , v_inpc_deduccion

    LET v_factor_inpc  = v_inpc_actual / v_inpc_deduccion

    --  Actualiza monto al día del retiro.

    LET v_mto_real_deduc = v_mto_real_deduc * v_factor_inpc

    LET v_isr_deduc = v_mto_real_deduc * 0.20

    RETURN v_isr_deduc
END FUNCTION

FUNCTION liquida_total( reg_15, vindicador )
#lt--------------------------------------
    DEFINE reg_15 RECORD #loc #reg_15
        nss                   CHAR(11)      ,
        consecutivo           INTEGER       ,
        fecha_retiro          DATE          ,
        fecha_conversion      DATE          ,
        subcuenta             SMALLINT      ,
        saldo_acciones        DECIMAL(16,6) ,
        mto_neto_accion       DECIMAL(16,6) ,
        mto_rete_accion       DECIMAL(16,6) ,
        mto_rendimiento       DECIMAL(16,6) ,
        mto_neto_pesos        DECIMAL(16,6) ,
        mto_rete_pesos        DECIMAL(16,6) ,
        siefore               SMALLINT      ,
        aux_mto_neto_acc      DECIMAL(16,6)
    END RECORD

    DEFINE #loc #decimal
        suma                  DECIMAL(16,6) ,
        sum_mto_neto          DECIMAL(16,6) ,
        sum_mto_rete          DECIMAL(16,6) ,
        sum_mto_bruto         DECIMAL(16,6) ,
        vprecio_accion        DECIMAL(16,6) ,
        v_mto_isr20           DECIMAL(16,6)

    DEFINE #loc #smallint
        vindicador           SMALLINT

    IF reg_15.siefore = 1 THEN
        CALL precio_accion(HOY,1)
        RETURNING vprecio_accion
    ELSE 
        prompt "para 6" for char enter
        CALL precio_accion(HOY,3)
        RETURNING vprecio_accion
    END IF

    LET reg_15.mto_neto_pesos = reg_15.mto_neto_accion*vprecio_accion
    LET reg_15.mto_rete_pesos = reg_15.mto_rete_accion*vprecio_accion

    INSERT INTO ret_pago_vol VALUES( ultimo_folio            ,
                                    reg_15.nss              ,
                                    reg_15.consecutivo      ,
                                    reg_15.mto_neto_pesos   ,
                                    reg_15.mto_rete_pesos   ,
                                    reg_15.mto_rendimiento  ,
                                    usuario                 ,
                                    reg_15.fecha_conversion ,
                                    reg_15.fecha_retiro )

    IF reg_15.mto_neto_pesos < 0 THEN
        INSERT INTO dis_cuenta
        VALUES( "490"                  ,--tipo_movimiento
               reg_15.subcuenta       ,--subcuenta
               reg_15.siefore         ,--siefore
               ultimo_folio           ,--folio
               reg_15.consecutivo     ,--consecutivo
               reg_15.nss             ,--nss
               ""                     ,--n_unico
               ""                     ,--folio_sua
               reg_15.fecha_retiro    ,--fecha_pago
               reg_15.fecha_retiro    ,--fecha_valor
               reg_15.fecha_retiro    ,--fecha_conversion
               reg_15.mto_neto_pesos  ,--monto_en_pesos
               reg_15.mto_neto_accion ,--monto_en_acciones
               vprecio_accion         ,--precio_accion
               ""                     ,--dias_cotizados
               ""                     ,--sucursal
               "RETIRO"               ,--id_aportante
               8                      ,--estado
               HOY                    ,--fecha_proceso
               usuario                ,--usuario
               HOY                    ,--fecha_archivo
               0 )                     --etiqueta

    END IF

    IF reg_15.mto_rete_pesos < 0 THEN
        INSERT INTO dis_cuenta
        VALUES("10  "                 ,--tipo_movimiento
               reg_15.subcuenta       ,--subcuenta
               reg_15.siefore         ,--siefore
               ultimo_folio           ,--folio
               reg_15.consecutivo      ,--consecutivo
               reg_15.nss             ,--nss
               ""                     ,--n_unico
               ""                     ,--folio_sua
               reg_15.fecha_retiro    ,--fecha_pago
               reg_15.fecha_retiro    ,--fecha_valor
               reg_15.fecha_retiro    ,--fecha_conversion
               reg_15.mto_rete_pesos  ,--monto_en_pesos
               reg_15.mto_rete_accion ,--monto_en_acciones
               vprecio_accion         ,--precio_accion
               ""                     ,--dias_cotizados
               ""                     ,--sucursal
               "RETIRO"               ,--id_aportante
               8                      ,--estado
               HOY                    ,--fecha_proceso
               usuario                ,--usuario
               HOY                    ,--fecha_archivo
               0 )                     --etiqueta
    END IF

    CASE vindicador 
        WHEN  1 -- LIQUIDA RETIRO TOTAL --
            IF reg_15.aux_mto_neto_acc < 0 THEN
                LET suma = (reg_15.aux_mto_neto_acc + reg_15.mto_rete_accion)
    
                UPDATE cta_saldo_vol
                SET    saldo_acciones   = (saldo_acciones+suma),
                       fecha_saldo      = HOY   
                WHERE  nss              = reg_15.nss
                AND    fecha_conversion = reg_15.fecha_conversion
                AND    saldo_acciones   = reg_15.saldo_acciones
            END IF
        WHEN  2 -- LIQUIDA RETIRO PARCIAL --
            UPDATE cta_saldo_vol
            SET    saldo_acciones   = (saldo_acciones-reg_15.aux_mto_neto_acc),
                   fecha_saldo      = HOY
            WHERE  nss              = reg_15.nss
            AND    fecha_conversion = reg_15.fecha_conversion
            AND    saldo_acciones   = reg_15.saldo_acciones
        WHEN  3 -- LIQUIDA RETIRO PARCIAL --
            UPDATE cta_saldo_vol
            SET    saldo_acciones   = (saldo_acciones+reg_15.aux_mto_neto_acc) ,
                   fecha_saldo      = HOY
            WHERE  nss              = reg_15.nss
            AND    fecha_conversion = reg_15.fecha_conversion
            AND    saldo_acciones   = reg_15.saldo_acciones
    END CASE
END FUNCTION

FUNCTION liquida_total_20(reg_21)
#lt2-----------------------------
    DEFINE reg_21 RECORD #loc #reg_21
        nss                   CHAR(11)      ,
        consecutivo           INTEGER       ,
        fecha_retiro          DATE          ,
        fecha_conversion      DATE          ,
        subcuenta             SMALLINT      ,
        mto_accion            DECIMAL(16,6) ,
        mto_pesos             DECIMAL(16,6) ,
        siefore               SMALLINT      ,
        precio_dia            DECIMAL(11,6) 
    END RECORD

    IF reg_21.mto_accion < 0 THEN
        INSERT INTO dis_cuenta
        VALUES("20"                   ,--tipo_movimiento
               reg_21.subcuenta       ,--subcuenta
               reg_21.siefore         ,--siefore
               ultimo_folio           ,--folio
               reg_21.consecutivo     ,--consecutivo
               reg_21.nss             ,--nss
               ""                     ,--n_unico
               ""                     ,--folio_sua
               reg_21.fecha_retiro    ,--fecha_pago
               reg_21.fecha_retiro    ,--fecha_valor
               reg_21.fecha_retiro    ,--fecha_conversion
               reg_21.mto_pesos       ,--monto_en_pesos
               reg_21.mto_accion      ,--monto_en_acciones
               reg_21.precio_dia      ,--precio_accion
               ""                     ,--dias_cotizados
               ""                     ,--sucursal
               "RETIRO"               ,--id_aportante
               8                      ,--estado
               HOY                    ,--fecha_proceso
               usuario                ,--usuario
               HOY                    ,--fecha_archivo
               0 )                     --etiqueta
    END IF
END FUNCTION

FUNCTION calcula_saldo_3( reg_09 )
#cs3----------------------------
    DEFINE reg_09 RECORD #loc #reg_09
        nss                   CHAR(11)    ,
        consecutivo           INTEGER     ,
        fecha_retiro          DATE        ,
        monto_en_acciones     DECIMAL(16,6)
    END RECORD

    DEFINE reg_10 RECORD #loc #reg_10
        subcuenta             SMALLINT      ,
        fecha_conversion      DATE          ,
        saldo_acciones        DECIMAL(16,6) ,
        siefore               SMALLINT
    END RECORD

    DEFINE reg_12 RECORD #loc #reg_12
        mto_neto_pesos        DECIMAL(16,6) ,
        mto_rete_pesos        DECIMAL(16,6) ,
        mto_neto_accion       DECIMAL(16,6) ,
        mto_rete_accion       DECIMAL(16,6) ,
        mto_rendimiento       DECIMAL(16,6)
    END RECORD

    DEFINE reg_14 RECORD #loc #reg_14
        deduccion             CHAR(1)       ,
        fecha_deduccion       DATE          ,
        mto_deducido          DECIMAL(16,6) ,
        pension_invalidez     CHAR(1)       ,
        edad                  SMALLINT      ,
        mto_solic             DECIMAL(16,6)
    END RECORD

    DEFINE reg_16 RECORD #loc #reg_16
        subcuenta             SMALLINT      ,
        fecha_conversion      DATE          ,
        saldo_acciones        DECIMAL(16,6) ,
        siefore               SMALLINT
    END RECORD

    DEFINE #loc #decimal
        aux_suma              ,
        tot_rete_mas_neto     ,
        tot_accion            ,
        suma                  ,
        vmto_ret20_acc        ,
        vmto_ret20_pes        ,
        vret20_pes_sub3       ,
        vret20_pes_sub10      ,
        vret20_acc_sub3       ,
        vret20_acc_sub10      ,
        vsaldo_pesos          ,
        vsaldo_total_pesos    ,
        v_mto_isr20           ,
        vtot_rete_acc_sub3    ,
        vtot_rete_acc_sub10   DECIMAL(16,6)

    DEFINE #loc #smallint
        reg_inv               ,
        reg_inv_sucta3        ,        
        reg_inv_sucta10      SMALLINT

    LET aux_suma               = 0
    LET reg_10.saldo_acciones  = 0
    LET reg_12.mto_neto_pesos  = 0
    LET reg_12.mto_rete_pesos  = 0
    LET reg_12.mto_neto_accion = 0
    LET reg_12.mto_rete_accion = 0
    LET reg_12.mto_rendimiento = 0
    LET reg_14.mto_deducido    = 0
    LET reg_14.mto_solic       = 0
    LET reg_16.saldo_acciones  = 0
    LET tot_rete_mas_neto      = 0
    LET tot_accion             = 0
    LET suma                   = 0
    LET vmto_ret20_acc         = 0
    LET vmto_ret20_pes         = 0
    LET vret20_pes_sub3        = 0
    LET vret20_pes_sub10       = 0
    LET vret20_acc_sub3        = 0
    LET vret20_acc_sub10       = 0
    LET vsaldo_pesos           = 0
    LET vsaldo_total_pesos     = 0
    LET v_mto_isr20            = 0
    LET vtot_rete_acc_sub3     = 0
    LET vtot_rete_acc_sub10    = 0
    LET vmto_solic_pesos       = 0

    SELECT codigo_siefore
    INTO   reg_inv_sucta3
    FROM   cta_regimen 
    WHERE  nss       = reg_09.nss
    AND    subcuenta = 3

    SELECT codigo_siefore
    INTO   reg_inv_sucta10
    FROM   cta_regimen
    WHERE  nss       = reg_09.nss
    AND    subcuenta = 10

    SELECT fecha_traspaso
    INTO   vfecha_traspaso
    FROM   tes_solicitud
    WHERE  nss           = reg_09.nss
    AND    tipo_traspaso = 9
    AND    estado        = 103

    IF STATUS <> NOTFOUND THEN
        LET ind_cambio_recursos = "S"
    ELSE
        LET ind_cambio_recursos = "N"
    END IF

    SELECT deduccion         ,
           fecha_deduccion   ,
           mto_deducido      ,
           pension_invalidez ,
           edad              ,
           mto_solic
    INTO   reg_14.*
    FROM   ret_cta_vol
    WHERE  n_seguro    = reg_09.nss
    AND    consecutivo = reg_09.consecutivo

    CALL calcula_mto_solic(reg_09.nss,reg_09.fecha_retiro,reg_14.mto_solic)
    RETURNING monto_acc_sb1,
              monto_acc_sb3,
              monto_pesos_sb1,
              monto_pesos_sb3

    IF monto_pesos_sb1 IS NULL THEN
        LET monto_pesos_sb1 = 0
    END IF

    IF monto_pesos_sb3 IS NULL THEN
        LET monto_pesos_sb3 = 0
    END IF

    LET vmto_solic_pesos = monto_pesos_sb1 + monto_pesos_sb3

    IF reg_14.pension_invalidez = "S"
    OR reg_14.edad >= 65
    OR reg_14.deduccion = "N" THEN
        LET vban = 1
    ELSE
        IF reg_14.deduccion = "S" THEN

            LET vban = 2

            CALL calcula_isr_20(reg_09.nss)
            RETURNING v_mto_isr20

            IF v_mto_isr20 IS NULL
            OR v_mto_isr20 = " "
            OR v_mto_isr20 < 0  THEN
                LET v_mto_isr20 = 0
            END IF

        END IF
    END IF

    DECLARE cur_7 CURSOR FOR
    SELECT  subcuenta        ,
            fecha_conversion ,
            saldo_acciones   ,
            siefore
    FROM    cta_saldo_vol
    WHERE   nss            = reg_09.nss
    AND     saldo_acciones > 0
    AND     subcuenta in (3,10)
    ORDER BY fecha_conversion asc

    LET tot_accion = reg_09.monto_en_acciones

    FOREACH cur_7 INTO reg_10.*
        IF reg_10.subcuenta = 3 THEN
            LET reg_inv = reg_inv_sucta3
        ELSE
            LET reg_inv = reg_inv_sucta10
        END IF 

        IF reg_inv = 1  
        OR ind_cambio_recursos = "N"  THEN 

            IF tot_accion > 0 THEN

                CALL calculo_ISR(reg_09.nss              ,
                                 reg_10.fecha_conversion ,
                                 reg_09.fecha_retiro     ,
                                 reg_10.saldo_acciones   ,
                                 reg_10.saldo_acciones   ,
                                 reg_10.siefore          ,
                                 reg_10.siefore
                                )#cisr
                RETURNING reg_12.mto_neto_accion ,
                          reg_12.mto_rete_accion ,
                          reg_12.mto_rendimiento

                LET tot_rete_mas_neto = -reg_12.mto_neto_accion +
                                        -reg_12.mto_rete_accion

                IF tot_accion >= tot_rete_mas_neto THEN
 
                    IF vban = 1 THEN
                        CALL liquida_total(reg_09.nss              ,
                                           reg_09.consecutivo      ,
                                           reg_09.fecha_retiro     ,
                                           reg_10.fecha_conversion ,
                                           reg_10.subcuenta        ,
                                           reg_10.saldo_acciones   ,
                                           reg_12.mto_neto_accion  ,
                                           reg_12.mto_rete_accion  ,
                                           reg_12.mto_rendimiento  ,
                                           0                       ,
                                           0                       ,
                                           reg_10.siefore          ,
                                           tot_rete_mas_neto       ,
                                           2                       )#lt
                    ELSE
                        CALL calcula_saldo_20(reg_09.nss            ,
                                              1                     ,
                                              tot_rete_mas_neto     ,
                                              reg_10.siefore        ,
                                              v_mto_isr20           ,
                                              vmto_solic_pesos )#cs2
                        RETURNING vmto_ret20_pes ,
                                  vmto_ret20_acc ,
                                  vprecio_dia

                        LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                        LET reg_12.mto_neto_accion = reg_12.mto_neto_accion - vmto_ret20_acc

                        LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                        CALL liquida_total(reg_09.nss              , -- LIQUIDA NETO Y 0.05% ISR  --
                                           reg_09.consecutivo      ,
                                           reg_09.fecha_retiro     ,
                                           reg_10.fecha_conversion ,
                                           reg_10.subcuenta        ,
                                           reg_10.saldo_acciones   ,
                                           reg_12.mto_neto_accion  ,
                                           reg_12.mto_rete_accion  ,
                                           reg_12.mto_rendimiento  ,
                                           0                       ,
                                           0                       ,
                                           reg_10.siefore          ,
                                           tot_rete_mas_neto       ,
                                           2                       )#lt

                        CALL liquida_total_20(reg_09.nss               , -- LIQUIDA 20% ISR --
                                              reg_09.consecutivo       ,
                                              reg_09.fecha_retiro      ,
                                              reg_10.fecha_conversion  ,
                                              reg_10.subcuenta         ,
                                              -vmto_ret20_acc          ,
                                              -vmto_ret20_pes          ,
                                              reg_10.siefore           ,
                                              vprecio_dia              )#lt2
                    END IF

                    IF reg_10.siefore = 1 THEN
                        CALL precio_accion(HOY,1)
                        RETURNING precio_accion_fin
                    ELSE 
              
                        prompt "para 7" for char enter
                        CALL precio_accion(HOY,3)
                        RETURNING precio_accion_fin
                    END IF

                    LET reg_12.mto_neto_pesos = reg_12.mto_neto_accion *
                                                precio_accion_fin
                    LET reg_12.mto_rete_pesos = reg_12.mto_rete_accion *
                                                precio_accion_fin
                    LET mto_ult_rete_peso     = mto_ult_rete_peso     +
      	    	  		                reg_12.mto_neto_pesos +
					        reg_12.mto_rete_pesos

                    LET tot_accion = tot_accion - tot_rete_mas_neto
                ELSE
                    IF reg_10.siefore = 1 THEN
                        CALL precio_accion(HOY,1)
                        RETURNING precio_accion_fin
                    ELSE
                        prompt "para 8" for char enter
                        CALL precio_accion(HOY,3)
                        RETURNING precio_accion_fin
                    END IF

                    LET reg_12.mto_rete_accion = reg_12.mto_rete_accion*tot_accion/
                                                 reg_10.saldo_acciones
                    LET reg_12.mto_neto_accion = -(tot_accion+reg_12.mto_rete_accion)
                    LET reg_12.mto_neto_pesos  = reg_12.mto_neto_accion *
                                                 precio_accion_fin
                    LET reg_12.mto_rete_pesos  = -mto_ult_rete_peso-
           				         reg_12.mto_neto_pesos
                    LET suma = (reg_12.mto_neto_accion+reg_12.mto_rete_accion)
                    LET tot_accion = tot_accion + suma

                    IF vban = 1 THEN
                        CALL liquida_total(reg_09.nss              ,
                                           reg_09.consecutivo      ,
                                           reg_09.fecha_retiro     ,
                                           reg_10.fecha_conversion ,
                                           reg_10.subcuenta        ,
                                           reg_10.saldo_acciones   ,
                                           reg_12.mto_neto_accion  ,
                                           reg_12.mto_rete_accion  ,
                                           reg_12.mto_rendimiento  ,
                                           0                       ,
                                           0                       ,
                                           reg_10.siefore          ,
                                           suma                    ,
                                           3                       )#lt
                    ELSE
                        LET aux_suma = suma * -1 

                        CALL calcula_saldo_20(reg_09.nss            ,
                                              1                     ,
                                              aux_suma              ,
                                              reg_10.siefore        ,
                                              v_mto_isr20           ,
                                              vmto_solic_pesos )#cs2
                        RETURNING vmto_ret20_pes ,
                                  vmto_ret20_acc ,
                                  vprecio_dia
 
                        LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1
                        LET reg_12.mto_neto_accion = reg_12.mto_neto_accion - vmto_ret20_acc
                        LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                        CALL liquida_total(reg_09.nss              , -- LIQUIDA NETO Y 0.05% ISR  --
                                           reg_09.consecutivo      ,
                                           reg_09.fecha_retiro     ,
                                           reg_10.fecha_conversion ,
                                           reg_10.subcuenta        ,
                                           reg_10.saldo_acciones   ,
                                           reg_12.mto_neto_accion  ,
                                           reg_12.mto_rete_accion  ,
                                           reg_12.mto_rendimiento  ,
                                           0                       ,
                                           0                       ,
                                           reg_10.siefore          ,
                                           suma                    ,
                                           3                       )#lt
  
                        CALL liquida_total_20(reg_09.nss               , -- LIQUIDA 20% ISR --
                                              reg_09.consecutivo       ,
                                              reg_09.fecha_retiro      ,
                                              reg_10.fecha_conversion  ,
                                              reg_10.subcuenta         ,
                                              -vmto_ret20_acc          ,
                                              -vmto_ret20_pes          ,
                                              reg_10.siefore           ,
                                              vprecio_dia              )#lt2
                    END IF
                    EXIT FOREACH
                END IF
            END IF
        ELSE
            IF tot_accion > 0 THEN 
                IF reg_10.fecha_conversion < vfecha_traspaso 
                AND reg_10.siefore = 3 THEN   

                    CALL calculo_ISR(reg_09.nss              ,
                                     reg_10.fecha_conversion ,
                                     reg_09.fecha_retiro     ,
                                     reg_10.saldo_acciones   ,
                                     reg_10.saldo_acciones   ,#lo agregue recien
                                     reg_10.siefore          ,
                                     reg_inv
                                    )#cisr
                    RETURNING reg_12.mto_neto_accion ,
                              reg_12.mto_rete_accion ,
                              reg_12.mto_rendimiento

                    LET tot_rete_mas_neto = -reg_12.mto_neto_accion +
                                            -reg_12.mto_rete_accion


                    IF tot_accion >= tot_rete_mas_neto THEN  
                        IF reg_10.siefore = 1 THEN
                            CALL precio_accion(HOY,1)
                            RETURNING precio_accion_fin
                        ELSE
                            prompt "para 9" for char enter
                            CALL precio_accion(HOY,3)
                            RETURNING precio_accion_fin
                        END IF

                        LET reg_12.mto_neto_pesos = reg_12.mto_neto_accion *
                                                    precio_accion_fin
                        LET reg_12.mto_rete_pesos = reg_12.mto_rete_accion *
                                                    precio_accion_fin
                        LET mto_ult_rete_peso     = mto_ult_rete_peso     +
	                                            reg_12.mto_neto_pesos +
             	                                    reg_12.mto_rete_pesos
 
                        IF vban = 1 THEN                
                            CALL liquida_total(reg_09.nss              ,
                                               reg_09.consecutivo      ,
                                               reg_09.fecha_retiro     ,
                                               reg_10.fecha_conversion ,
                                               reg_10.subcuenta        ,
                                               reg_10.saldo_acciones   ,
                                               reg_12.mto_neto_accion  ,
                                               reg_12.mto_rete_accion  ,
                                               reg_12.mto_rendimiento  ,
                                               0                       ,
                                               0                       ,
                                               3                       ,
                                               tot_rete_mas_neto       ,
                                               2                       )#lt
                        ELSE
                            CALL calcula_saldo_20(reg_09.nss            ,
                                                  1                     ,
                                                  tot_rete_mas_neto     ,
                                                  reg_10.siefore        ,
                                                  v_mto_isr20           ,
                                                  vmto_solic_pesos )#cs2
                            RETURNING vmto_ret20_pes ,
                                      vmto_ret20_acc ,
                                      vprecio_dia

                            LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1
                            LET reg_12.mto_neto_accion = reg_12.mto_neto_accion - vmto_ret20_acc
                            LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                            CALL liquida_total(reg_09.nss              , -- LIQUIDA NETO Y 0.05% ISR--
                                               reg_09.consecutivo      ,
                                               reg_09.fecha_retiro     ,
                                               reg_10.fecha_conversion ,
                                               reg_10.subcuenta        ,
                                               reg_10.saldo_acciones   ,
                                               reg_12.mto_neto_accion  ,
                                               reg_12.mto_rete_accion  ,
                                               reg_12.mto_rendimiento  ,
                                               0                       ,
                                               0                       ,
                                               3                       ,
                                               tot_rete_mas_neto       ,
                                               2                       )#lt

                            CALL liquida_total_20(reg_09.nss               , -- LIQUIDA 20% ISR --
                                                  reg_09.consecutivo       ,
                                                  reg_09.fecha_retiro      ,
                                                  reg_10.fecha_conversion  ,
                                                  reg_10.subcuenta         ,
                                                  -vmto_ret20_acc          ,
                                                  -vmto_ret20_pes          ,
                                                  reg_10.siefore           ,
                                                  vprecio_dia              )#lt2
                        END IF

                        LET tot_accion = tot_accion - tot_rete_mas_neto
                    ELSE
                        IF reg_10.siefore = 1 THEN
                            CALL precio_accion(HOY,1)
                            RETURNING precio_accion_fin
                        ELSE
                            prompt "para 10" for char enter
                            CALL precio_accion(HOY,3)
                            RETURNING precio_accion_fin
                        END IF

                        LET reg_12.mto_rete_accion = reg_12.mto_rete_accion*tot_accion/
                                                     reg_10.saldo_acciones
                        LET reg_12.mto_neto_accion = -(tot_accion+reg_12.mto_rete_accion)
                        LET reg_12.mto_neto_pesos  = reg_12.mto_neto_accion *
                                                     precio_accion_fin
                        LET reg_12.mto_rete_pesos  = -mto_ult_rete_peso-
	          		                     reg_12.mto_neto_pesos
                         LET suma = (reg_12.mto_neto_accion+reg_12.mto_rete_accion)
                         LET tot_accion = tot_accion + suma

                         IF vban = 1 THEN
                             CALL liquida_total(reg_09.nss              ,
                                                reg_09.consecutivo      ,
                                                reg_09.fecha_retiro     ,
                                                reg_10.fecha_conversion ,
                                                reg_10.subcuenta        ,
                                                reg_10.saldo_acciones   ,
                                                reg_12.mto_neto_accion  ,
                                                reg_12.mto_rete_accion  ,
                                                reg_12.mto_rendimiento  ,
                                                0                       ,
                                                0                       ,
                                                3                       ,
                                                suma                    ,
                                                3                       )#lt
                         ELSE
                             LET aux_suma = suma * -1

                             CALL calcula_saldo_20(reg_09.nss            ,
                                                   2                     ,
                                                   aux_suma              ,
                                                   reg_10.siefore        ,
                                                   v_mto_isr20           ,
                                                   vmto_solic_pesos )#cs2
                             RETURNING vmto_ret20_pes ,
                                       vmto_ret20_acc ,
                                       vprecio_dia

                             LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1
                             LET reg_12.mto_neto_accion = reg_12.mto_neto_accion - vmto_ret20_acc
                             LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                             CALL liquida_total(reg_09.nss              , -- LIQUIDA NETO Y 0.05% ISR--
                                                reg_09.consecutivo      ,
                                                reg_09.fecha_retiro     ,
                                                reg_10.fecha_conversion ,
                                                reg_10.subcuenta        ,
                                                reg_10.saldo_acciones   ,
                                                reg_12.mto_neto_accion  ,
                                                reg_12.mto_rete_accion  ,
                                                reg_12.mto_rendimiento  ,
                                                0                       ,
                                                0                       ,
                                                3                       ,
                                                suma                    ,
                                                3                       )#lt

                             CALL liquida_total_20(reg_09.nss               , -- LIQUIDA 20% ISR --
                                                   reg_09.consecutivo       ,
                                                   reg_09.fecha_retiro      ,
                                                   reg_10.fecha_conversion  ,
                                                   reg_10.subcuenta         ,
                                                   -vmto_ret20_acc          ,
                                                   -vmto_ret20_pes          ,
                                                   reg_10.siefore           ,
                                                   vprecio_dia              )#lt2
                        END IF
                        EXIT FOREACH
                    END IF 
                END IF 

                IF reg_10.fecha_conversion = vfecha_traspaso THEN  
  
                    DECLARE cur_12 CURSOR FOR
                    SELECT  subcuenta        ,
                            fecha_conversion ,
                            saldo_acciones   ,
                            siefore
                    FROM    cta_saldo_vol
                    WHERE   nss              = reg_09.nss
                    AND     saldo_acciones   > 0
                    AND     subcuenta        =  reg_10.subcuenta
                    AND     fecha_conversion < vfecha_traspaso
                    ORDER BY fecha_conversion asc

                    FOREACH cur_12 INTO reg_16.*
                        CALL calculo_ISR(reg_09.nss              ,
                                         reg_16.fecha_conversion ,
                                         reg_09.fecha_retiro     ,
                                         reg_16.saldo_acciones   ,
                                         reg_16.saldo_acciones   ,#lo agregue recien
                                         reg_16.siefore          ,
                                         reg_inv
                                        )#cisr
                        RETURNING reg_12.mto_neto_accion ,
                                  reg_12.mto_rete_accion ,
                                  reg_12.mto_rendimiento

                        LET reg_12.mto_rete_accion = reg_12.mto_rete_accion * -1

                        IF reg_16.subcuenta = 3 THEN
                            LET vtot_rete_acc_sub3 = vtot_rete_acc_sub3 + reg_12.mto_rete_accion
                        ELSE
                            LET vtot_rete_acc_sub10 = vtot_rete_acc_sub10 + reg_12.mto_rete_accion
                        END IF
                    END FOREACH

                    IF reg_10.subcuenta = 3 THEN

                        LET reg_12.mto_neto_accion = reg_10.saldo_acciones - vtot_rete_acc_sub3
            
                         LET reg_12.mto_rete_accion = vtot_rete_acc_sub3 * -1
                    ELSE
                        LET reg_12.mto_neto_accion = reg_10.saldo_acciones - vtot_rete_acc_sub10
       
                        LET reg_12.mto_rete_accion = vtot_rete_acc_sub10 * -1
                    END IF

                    LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1


                    LET tot_rete_mas_neto = -reg_12.mto_neto_accion +
                                            -reg_12.mto_rete_accion

                    IF tot_accion >= tot_rete_mas_neto THEN 
                        IF reg_10.siefore = 1 THEN
                            CALL precio_accion(HOY,1)
                            RETURNING precio_accion_fin
                        ELSE
                            prompt "para 11" for char enter
                            CALL precio_accion(HOY,3)
                            RETURNING precio_accion_fin
                        END IF

                        LET reg_12.mto_neto_pesos = reg_12.mto_neto_accion *
                                                    precio_accion_fin
                        LET reg_12.mto_rete_pesos = reg_12.mto_rete_accion *
                                                    precio_accion_fin

                        LET mto_ult_rete_peso     = mto_ult_rete_peso     +
          	  		                    reg_12.mto_neto_pesos +
         		                            reg_12.mto_rete_pesos
 
                        IF vban = 1 THEN 
                             CALL liquida_total(reg_09.nss              ,
                                                reg_09.consecutivo      ,
                                                reg_09.fecha_retiro     ,
                                                reg_10.fecha_conversion ,
                                                reg_10.subcuenta        ,
                                                reg_10.saldo_acciones   ,
                                                reg_12.mto_neto_accion  ,
                                                reg_12.mto_rete_accion  ,
                                                reg_12.mto_rendimiento  ,
                                                0                       ,
                                                0                       ,
                                                3                       ,
                                                tot_rete_mas_neto       ,
                                                2                       )#lt
                        ELSE
                            CALL calcula_saldo_20(reg_09.nss            ,
                                                  1                     ,
                                                  tot_rete_mas_neto     ,
                                                  reg_10.siefore        ,
                                                  v_mto_isr20           ,
                                                  vmto_solic_pesos )#cs2
                            RETURNING vmto_ret20_pes ,
                                      vmto_ret20_acc ,
                                      vprecio_dia

                            LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                            LET reg_12.mto_neto_accion = reg_12.mto_neto_accion - vmto_ret20_acc

                            LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                            CALL liquida_total(reg_09.nss              , -- LIQUIDA NETO Y 0.05% ISR  --
                                               reg_09.consecutivo      ,
                                               reg_09.fecha_retiro     ,
                                               reg_10.fecha_conversion ,
                                               reg_10.subcuenta        ,
                                               reg_10.saldo_acciones   ,
                                               reg_12.mto_neto_accion  ,
                                               reg_12.mto_rete_accion  ,
                                               reg_12.mto_rendimiento  ,
                                               0                       ,
                                               0                       ,
                                               3                       ,
                                               tot_rete_mas_neto       ,
                                               2                       )#lt

                            CALL liquida_total_20(reg_09.nss               , -- LIQUIDA 20% ISR --
                                                  reg_09.consecutivo       ,
                                                  reg_09.fecha_retiro      ,
                                                  reg_10.fecha_conversion  ,
                                                  reg_10.subcuenta         ,
                                                  -vmto_ret20_acc          ,
                                                  -vmto_ret20_pes          ,
                                                  reg_10.siefore           ,
                                                  vprecio_dia              )#lt2
                        END IF

                        LET tot_accion = tot_accion - tot_rete_mas_neto
                    ELSE
                        IF reg_10.siefore = 1 THEN
                            CALL precio_accion(HOY,1)
                            RETURNING precio_accion_fin
                        ELSE
                            prompt "para 1" for char enter
                            CALL precio_accion(HOY,3)
                            RETURNING precio_accion_fin
                        END IF

                        LET reg_12.mto_rete_accion = reg_12.mto_rete_accion*tot_accion/
                                                     reg_10.saldo_acciones

                        LET reg_12.mto_neto_accion = -(tot_accion+reg_12.mto_rete_accion)

                        LET reg_12.mto_neto_pesos  = reg_12.mto_neto_accion *
                                                     precio_accion_fin
            	        
                        LET reg_12.mto_rete_pesos  = -mto_ult_rete_peso-
	                                             reg_12.mto_neto_pesos

                        LET suma = (reg_12.mto_neto_accion+reg_12.mto_rete_accion)
                        LET tot_accion = tot_accion + suma

                        IF vban = 1 THEN 
                            CALL liquida_total(reg_09.nss              ,
                                               reg_09.consecutivo      ,
                                               reg_09.fecha_retiro     ,
                                               reg_10.fecha_conversion ,
                                               reg_10.subcuenta        ,
                                               reg_10.saldo_acciones   ,
                                               reg_12.mto_neto_accion  ,
                                               reg_12.mto_rete_accion  ,
                                               reg_12.mto_rendimiento  ,
                                               0                       ,
                                               0                       ,
                                               3                       ,
                                               suma                    ,
                                               3                       )#lt
                        ELSE
                            LET aux_suma = suma * -1

                            CALL calcula_saldo_20(reg_09.nss            ,
                                                  1                     ,
                                                  aux_suma              ,
                                                  reg_10.siefore        ,
                                                  v_mto_isr20           ,
                                                  vmto_solic_pesos )#cs2
                            RETURNING vmto_ret20_pes ,
                                      vmto_ret20_acc ,
                                      vprecio_dia

                            LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1
  
                            LET reg_12.mto_neto_accion = reg_12.mto_neto_accion - vmto_ret20_acc

                            LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                            CALL liquida_total(reg_09.nss              , -- LIQUIDA NETO Y 0.05% ISR  --
                                               reg_09.consecutivo      ,
                                               reg_09.fecha_retiro     ,
                                               reg_10.fecha_conversion ,
                                               reg_10.subcuenta        ,
                                               reg_10.saldo_acciones   ,
                                               reg_12.mto_neto_accion  ,
                                               reg_12.mto_rete_accion  ,
                                               reg_12.mto_rendimiento  ,
                                               0                       ,
                                               0                       ,
                                               3                       ,
                                               suma                    ,
                                               3                       )#lt

                            CALL liquida_total_20(reg_09.nss               , -- LIQUIDA 20% ISR --
                                                  reg_09.consecutivo       ,
                                                  reg_09.fecha_retiro      ,
                                                  reg_10.fecha_conversion  ,
                                                  reg_10.subcuenta         ,
                                                  -vmto_ret20_acc          ,
                                                  -vmto_ret20_pes          ,
                                                  reg_10.siefore           ,
                                                  vprecio_dia              )#lt2
                        END IF
                        EXIT FOREACH
                    END IF 
                END IF  

                IF reg_10.fecha_conversion > vfecha_traspaso THEN   

                    CALL calculo_ISR(reg_09.nss              ,
                                     reg_10.fecha_conversion ,
                                     reg_09.fecha_retiro     ,
                                     reg_10.saldo_acciones   ,
                                     reg_10.saldo_acciones   ,#lo agregue recien
                                     reg_10.siefore          ,
                                     reg_inv
                                    )#cisr
                    RETURNING reg_12.mto_neto_accion ,
                              reg_12.mto_rete_accion ,
                              reg_12.mto_rendimiento

                    LET tot_rete_mas_neto = -reg_12.mto_neto_accion +
                                            -reg_12.mto_rete_accion


                    IF tot_accion >= tot_rete_mas_neto THEN  
                        IF reg_10.siefore = 1 THEN
                            CALL precio_accion(HOY,1)
                            RETURNING precio_accion_fin
                        ELSE

                              prompt "para 2" for char enter
                            CALL precio_accion(HOY,3)
                            RETURNING precio_accion_fin
                        END IF

                        LET reg_12.mto_neto_pesos = reg_12.mto_neto_accion *
                                                    precio_accion_fin
                        LET reg_12.mto_rete_pesos = reg_12.mto_rete_accion *
                                                    precio_accion_fin

                        LET mto_ult_rete_peso     = mto_ult_rete_peso     +
	                                            reg_12.mto_neto_pesos +
             	                                    reg_12.mto_rete_pesos
 
                        IF vban = 1 THEN                
                            CALL liquida_total(reg_09.nss              ,
                                               reg_09.consecutivo      ,
                                               reg_09.fecha_retiro     ,
                                               reg_10.fecha_conversion ,
                                               reg_10.subcuenta        ,
                                               reg_10.saldo_acciones   ,
                                               reg_12.mto_neto_accion  ,
                                               reg_12.mto_rete_accion  ,
                                               reg_12.mto_rendimiento  ,
                                               0                       ,
                                               0                       ,
                                               3                       ,
                                               tot_rete_mas_neto       ,
                                               2                       )#lt
                        ELSE
                            CALL calcula_saldo_20(reg_09.nss            ,
                                                  1                     ,
                                                  tot_rete_mas_neto     ,
                                                  reg_10.siefore        ,
                                                  v_mto_isr20           ,
                                                  vmto_solic_pesos )#cs2
                            RETURNING vmto_ret20_pes ,
                                      vmto_ret20_acc ,
                                      vprecio_dia

                            LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1
 
                            LET reg_12.mto_neto_accion = reg_12.mto_neto_accion - vmto_ret20_acc

                            LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                            CALL liquida_total(reg_09.nss              , -- LIQUIDA NETO Y 0.05% ISR--
                                               reg_09.consecutivo      ,
                                               reg_09.fecha_retiro     ,
                                               reg_10.fecha_conversion ,
                                               reg_10.subcuenta        ,
                                               reg_10.saldo_acciones   ,
                                               reg_12.mto_neto_accion  ,
                                               reg_12.mto_rete_accion  ,
                                               reg_12.mto_rendimiento  ,
                                               0                       ,
                                               0                       ,
                                               3                       ,
                                               tot_rete_mas_neto       ,
                                               2                       )#lt

                            CALL liquida_total_20(reg_09.nss               , -- LIQUIDA 20% ISR --
                                                  reg_09.consecutivo       ,
                                                  reg_09.fecha_retiro      ,
                                                  reg_10.fecha_conversion  ,
                                                  reg_10.subcuenta         ,
                                                  -vmto_ret20_acc          ,
                                                  -vmto_ret20_pes          ,
                                                  reg_10.siefore           ,
                                                  vprecio_dia              )#lt2
                        END IF

                        LET tot_accion = tot_accion - tot_rete_mas_neto
                    ELSE
                        IF reg_10.siefore = 1 THEN
                            CALL precio_accion(HOY,1)
                            RETURNING precio_accion_fin
                        ELSE
                              prompt "para 3" for char enter
                            CALL precio_accion(HOY,3)
                            RETURNING precio_accion_fin
                        END IF

                        LET reg_12.mto_rete_accion = reg_12.mto_rete_accion*tot_accion/
                                                     reg_10.saldo_acciones

                        LET reg_12.mto_neto_accion = -(tot_accion+reg_12.mto_rete_accion)

                        LET reg_12.mto_neto_pesos  = reg_12.mto_neto_accion *
                                                     precio_accion_fin
	             	    
                        LET reg_12.mto_rete_pesos  = -mto_ult_rete_peso-
	          		                     reg_12.mto_neto_pesos


                         LET suma = (reg_12.mto_neto_accion+reg_12.mto_rete_accion)
                         LET tot_accion = tot_accion + suma

                         IF vban = 1 THEN
                             CALL liquida_total(reg_09.nss              ,
                                                reg_09.consecutivo      ,
                                                reg_09.fecha_retiro     ,
                                                reg_10.fecha_conversion ,
                                                reg_10.subcuenta        ,
                                                reg_10.saldo_acciones   ,
                                                reg_12.mto_neto_accion  ,
                                                reg_12.mto_rete_accion  ,
                                                reg_12.mto_rendimiento  ,
                                                0                       ,
                                                0                       ,
                                                3                       ,
                                                suma                    ,
                                                3                       )#lt
                         ELSE
                             LET aux_suma = suma * -1

                             CALL calcula_saldo_20(reg_09.nss            ,
                                                   2                     ,
                                                   aux_suma              ,
                                                   reg_10.siefore        ,
                                                   v_mto_isr20           ,
                                                   vmto_solic_pesos )#cs2
                             RETURNING vmto_ret20_pes ,
                                       vmto_ret20_acc ,
                                       vprecio_dia

                             LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1
  
                             LET reg_12.mto_neto_accion = reg_12.mto_neto_accion - vmto_ret20_acc

                             LET reg_12.mto_neto_accion = reg_12.mto_neto_accion * -1

                             CALL liquida_total(reg_09.nss              , -- LIQUIDA NETO Y 0.05% ISR--
                                                reg_09.consecutivo      ,
                                                reg_09.fecha_retiro     ,
                                                reg_10.fecha_conversion ,
                                                reg_10.subcuenta        ,
                                                reg_10.saldo_acciones   ,
                                                reg_12.mto_neto_accion  ,
                                                reg_12.mto_rete_accion  ,
                                                reg_12.mto_rendimiento  ,
                                                0                       ,
                                                0                       ,
                                                3                       ,
                                                suma                    ,
                                                3                       )#lt

                             CALL liquida_total_20(reg_09.nss               , -- LIQUIDA 20% ISR --
                                                   reg_09.consecutivo       ,
                                                   reg_09.fecha_retiro      ,
                                                   reg_10.fecha_conversion  ,
                                                   reg_10.subcuenta         ,
                                                   -vmto_ret20_acc          ,
                                                   -vmto_ret20_pes          ,
                                                   reg_10.siefore           ,
                                                   vprecio_dia              )#lt2
                        END IF
                        EXIT FOREACH
                    END IF 
                END IF 
            END IF  
        END IF 
    END FOREACH 
END FUNCTION

FUNCTION calcula_saldo_20(reg_18)
#cs2-----------------------------
    DEFINE reg_18 RECORD #loc #reg_18
        nss                   CHAR(11)      ,
        tipo_ret              SMALLINT      , 
        saldo_acciones        DECIMAL(16,6) ,
        siefore               SMALLINT      ,
        mto_isr20             DECIMAL(16,6) ,
        mto_total_pesos       DECIMAL(16,6) 
    END RECORD

    DEFINE reg_19 RECORD #loc #reg_19
        mto_aporte_pesos      DECIMAL(16,6) ,
        mto_isr_20_pesos      DECIMAL(16,6) ,  
        mto_isr_20_acc        DECIMAL(16,6) ,  
        precio_dia            LIKE glo_valor_accion.precio_del_dia
    END RECORD

    LET reg_19.mto_aporte_pesos = 0
    LET reg_19.mto_isr_20_pesos = 0
    LET reg_19.mto_isr_20_acc   = 0
    LET reg_19.precio_dia       = 0

    CALL precio_accion( HOY, 1 ) RETURNING precio_dia_sb1

#ff
    {
    --prompt "para 5" for char enter
    CALL precio_accion(HOY,3)
    RETURNING precio_dia_sb3
    }

    IF reg_18.siefore = 1 THEN
        LET reg_19.mto_aporte_pesos = reg_18.saldo_acciones * precio_dia_sb1
    ELSE 
        LET reg_19.mto_aporte_pesos = reg_18.saldo_acciones * precio_dia_sb3
    END IF

    LET reg_19.mto_isr_20_pesos = (reg_19.mto_aporte_pesos * reg_18.mto_isr20) / reg_18.mto_total_pesos

    IF reg_18.siefore = 1 THEN
        LET reg_19.mto_isr_20_acc = reg_19.mto_isr_20_pesos / precio_dia_sb1
 
        LET reg_19.precio_dia = precio_dia_sb1
    ELSE 
        LET reg_19.mto_isr_20_acc = reg_19.mto_isr_20_pesos / precio_dia_sb3
  
        LET reg_19.precio_dia = precio_dia_sb3
    END IF

    RETURN reg_19.mto_isr_20_pesos,
           reg_19.mto_isr_20_acc  ,
           reg_19.precio_dia

END FUNCTION

FUNCTION calculo_ISR(reg_11)
#cisr-----------------------
    DEFINE reg_11 RECORD #loc #reg_11
        nss                   CHAR(11)      ,
        fecha_ini             DATE          ,
        fecha_fin             DATE          ,
        monto_en_acciones     DECIMAL(16,6) ,
        tot_accion            DECIMAL(16,6) ,
        siefore               SMALLINT      ,
        reg_inv               SMALLINT
    END RECORD

    DEFINE reg_12 RECORD #loc #reg_12
        saldo_acciones        DECIMAL(16,6) ,
        fecha_conversion      DATE
    END RECORD
 
    DEFINE #loc #decimal
        precio_accion_ini     DECIMAL(16,6) ,
        dif_precio_accion     DECIMAL(16,6) ,
        rend_anualizado       DECIMAL(16,6) ,
        rend_en_pesos         DECIMAL(16,6) ,
        mto_rete_accion       DECIMAL(16,6) ,
        mto_total             DECIMAL(16,6) ,
        mto_neto              DECIMAL(16,6)

    DEFINE #loc #decimal
       valor_retencion        DECIMAL(16,6) ,
       saldo_pesos            DECIMAL(16,6) ,
       saldo_acciones         DECIMAL(16,6) ,
       monto_ret_pesos        DECIMAL(16,6) ,
       monto_tot_isr          DECIMAL(16,6) ,
       precio_dia_010102      DECIMAL(16,6) , 
       precio_dia_hoy         DECIMAL(16,6) ,  
       monto_en_accion_pa     DECIMAL(16,6) ,
       monto_accion_base      DECIMAL(16,6)

    LET valor_retencion    = 0   
    LET saldo_pesos        = 0   
    LET saldo_acciones     = 0   
    LET monto_ret_pesos    = 0   
    LET monto_tot_isr      = 0   
    LET precio_dia_hoy     = 0     

    LET precio_accion_ini  = 0
    LET precio_accion_fin  = 0
    LET dif_precio_accion  = 0
    LET rend_anualizado    = 0
    LET mto_rete_accion    = 0
    LET rend_en_pesos      = 0

    LET valor_retencion    = 0.0060
    LET monto_en_accion_pa = reg_11.tot_accion
    LET monto_tot_isr      = 0 
    LET monto_accion_base  = 0 
    LET rend_en_pesos      = 0
   
    CALL precio_accion(reg_11.fecha_ini,reg_11.siefore)
    RETURNING precio_accion_ini

    CALL precio_accion(reg_11.fecha_fin,reg_11.reg_inv)
    RETURNING precio_accion_fin

    IF monto_en_accion_pa >= reg_11.monto_en_acciones THEN
        LET monto_accion_base = reg_11.monto_en_acciones *
                                (precio_accion_ini/precio_accion_fin)
        LET monto_tot_isr     = monto_accion_base * valor_retencion *
                                (reg_11.fecha_fin - reg_11.fecha_ini )/365
        LET mto_neto          = reg_11.monto_en_acciones - monto_tot_isr
        LET rend_en_pesos     = (reg_11.monto_en_acciones - monto_accion_base)
                                 * precio_accion_fin
        LET monto_en_accion_pa = monto_en_accion_pa - reg_11.monto_en_acciones 
    ELSE 
        LET monto_accion_base = monto_en_accion_pa *
                                (precio_accion_ini / precio_accion_fin)
        LET monto_tot_isr     = monto_accion_base * valor_retencion *
                                (reg_11.fecha_fin - reg_11.fecha_ini )/365   
        LET mto_neto          = monto_en_accion_pa - monto_tot_isr
        LET rend_en_pesos     = (monto_en_accion_pa - monto_accion_base) *
                                 precio_accion_fin  
        LET monto_en_accion_pa = 0
    END IF 
    LET mto_rete_accion = monto_tot_isr 
    RETURN -mto_neto, -mto_rete_accion, rend_en_pesos
END FUNCTION

FUNCTION precio_accion(f_fecha_valuacion,vsiefore)
#pa-----------------------------------------------
    DEFINE #loc #decimal
        d6_precio_del_dia     DECIMAL(16,6)

    DEFINE #loc #date
        f_fecha_valuacion     DATE

    DEFINE #loc #smallint
        vsiefore              SMALLINT

    IF vsiefore = 3 THEN
        SELECT precio_del_dia
        INTO   d6_precio_del_dia
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = f_fecha_valuacion
        AND    codigo_siefore  = 3

        IF STATUS = NOTFOUND THEN
            PROMPT " NO EXISTE PRECIO DE ACCION DE: ",f_fecha_valuacion," SIEFORE 3"
            FOR CHAR enter
            EXIT PROGRAM
        END IF
    ELSE
        IF f_fecha_valuacion >= "01142005" THEN
            SELECT precio_del_dia
            INTO   d6_precio_del_dia
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = f_fecha_valuacion
            AND    codigo_siefore  = 1

            IF STATUS = NOTFOUND THEN
                PROMPT " NO EXISTE PRECIO DE ACCION DE: ",f_fecha_valuacion," SIEFORE 1"
                FOR CHAR enter
                EXIT PROGRAM
            END IF

        ELSE
            SELECT precio_del_dia
            INTO   d6_precio_del_dia
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = f_fecha_valuacion
            AND    codigo_siefore  = 2

            IF STATUS = NOTFOUND THEN
                PROMPT " NO EXISTE PRECIO DE ACCION DE: ",f_fecha_valuacion," SIEFORE 2"
                FOR CHAR enter
                EXIT PROGRAM
            END IF

        END IF
    END IF
    RETURN d6_precio_del_dia
END FUNCTION

FUNCTION modifica_seleccion( reg_2 )
#ms-------------------------------
    DEFINE arr_2 ARRAY[5] OF RECORD #loc #arr_2
        n_seguro              LIKE ret_cheque.n_seguro        ,
        tipo_prestacion       LIKE ret_cheque.tipo_prestacion ,
        paterno               LIKE ret_cheque.paterno         ,
        materno               LIKE ret_cheque.materno         ,
        nombres               LIKE ret_cheque.nombres         ,
        porcentaje            LIKE ret_cheque.porcentaje      ,
        cod_plaza             LIKE ret_cheque.cod_plaza       ,
        consecutivo           LIKE ret_cheque.consecutivo
    END RECORD

    DEFINE arr_3 ARRAY[5] OF RECORD #loc #arr_3
        n_seguro              LIKE ret_abono_cuenta.n_seguro        ,
        consecutivo           LIKE ret_abono_cuenta.consecutivo     ,
        tipo_prestacion       LIKE ret_abono_cuenta.tipo_prestacion ,
        paterno               LIKE ret_abono_cuenta.paterno         ,
        materno               LIKE ret_abono_cuenta.materno         ,
        nombres               LIKE ret_abono_cuenta.nombres         ,
        porcentaje            LIKE ret_abono_cuenta.porcentaje      ,
        cod_banco             SMALLINT                              ,
        cod_plaza             LIKE ret_abono_cuenta.cod_plaza       ,
        cod_sucursal          SMALLINT                              ,
        nro_cuenta            LIKE ret_abono_cuenta.nro_cuenta
    END RECORD

    DEFINE reg_2 RECORD #loc #reg_2
        var_nula              CHAR(1)                                  ,
        n_folio               LIKE afi_mae_afiliado.n_folio            ,
        n_seguro              LIKE afi_mae_afiliado.n_seguro           ,
        n_rfc                 LIKE afi_mae_afiliado.n_rfc              ,
        n_unico               LIKE afi_mae_afiliado.n_unico            ,
        fentcons              LIKE afi_mae_afiliado.fentcons           ,
        paterno               LIKE afi_mae_afiliado.paterno            ,
        materno               LIKE afi_mae_afiliado.materno            ,
        nombres               LIKE afi_mae_afiliado.nombres            ,
        monto_en_acciones_sb1 LIKE dis_cuenta.monto_en_acciones        ,
        monto_en_acciones_sb3 LIKE dis_cuenta.monto_en_acciones        ,
        monto_en_pesos        LIKE dis_cuenta.monto_en_pesos           ,
        fecha_ult_ret         LIKE ret_cta_vol.fecha_ult_ret           ,
        n_folio_sol           LIKE ret_cta_vol.n_folio_sol             ,
        tipo_ret              LIKE ret_cta_vol.tipo_ret                ,
        des_tipo_ret          CHAR(40)                                 ,
        mto_solic             LIKE ret_cta_vol.mto_solic               ,
        porcentaje_solic      LIKE ret_cta_vol.porcentaje_solic        ,
        tipo_pago             LIKE ret_cta_vol.tipo_pago               ,
        edad                  LIKE ret_cta_vol.edad                    ,
        des_tipo_pago         CHAR(40)                                 ,
        fecha_solic           LIKE ret_cta_vol.fecha_solic             ,
        fecha_captura         LIKE ret_cta_vol.fecha_captura           ,
        ultimo_proceso        LIKE ret_cta_vol.ultimo_proceso          ,
        pension_invalidez     LIKE ret_cta_vol.pension_invalidez       ,
        deduccion             LIKE ret_cta_vol.deduccion               ,
        fecha_deduccion       LIKE ret_cta_vol.fecha_deduccion         ,
        mto_deducido          LIKE ret_cta_vol.mto_deducido            ,
		  cod_rechazo_ent       LIKE ret_cta_vol.cod_rechazo_ent,   #* Rech. *
        consecutivo           INTEGER                                  ,
        usuario               LIKE ret_cta_vol.usuario                 ,
        estado                LIKE ret_cta_vol.estado
    END RECORD

    DEFINE reg_9 RECORD #loc #reg_9
        var_nula              CHAR(1)                                  ,
        n_folio               LIKE afi_mae_afiliado.n_folio            ,
        n_seguro              LIKE afi_mae_afiliado.n_seguro           ,
        n_rfc                 LIKE afi_mae_afiliado.n_rfc              ,
        n_unico               LIKE afi_mae_afiliado.n_unico            ,
        fentcons              LIKE afi_mae_afiliado.fentcons           ,
        paterno               LIKE afi_mae_afiliado.paterno            ,
        materno               LIKE afi_mae_afiliado.materno            ,
        nombres               LIKE afi_mae_afiliado.nombres            ,
        monto_en_acciones_sb1 LIKE dis_cuenta.monto_en_acciones        ,
        monto_en_acciones_sb3 LIKE dis_cuenta.monto_en_acciones        ,
        monto_en_pesos        LIKE dis_cuenta.monto_en_pesos           ,
        fecha_ult_ret         LIKE ret_cta_vol.fecha_ult_ret           ,
        n_folio_sol           LIKE ret_cta_vol.n_folio_sol             ,
        tipo_ret              LIKE ret_cta_vol.tipo_ret                ,
        des_tipo_ret          CHAR(40)                                 ,
        mto_solic             LIKE ret_cta_vol.mto_solic               ,
        porcentaje_solic      LIKE ret_cta_vol.porcentaje_solic        ,
        tipo_pago             LIKE ret_cta_vol.tipo_pago               ,
        edad                  LIKE ret_cta_vol.edad                    ,
        des_tipo_pago         CHAR(40)                                 ,
        fecha_solic           LIKE ret_cta_vol.fecha_solic             ,
        fecha_captura         LIKE ret_cta_vol.fecha_captura           ,
        ultimo_proceso        LIKE ret_cta_vol.ultimo_proceso          ,
        pension_invalidez     LIKE ret_cta_vol.pension_invalidez       ,
        deduccion             LIKE ret_cta_vol.deduccion               ,
        fecha_deduccion       LIKE ret_cta_vol.fecha_deduccion         ,
        mto_deducido          LIKE ret_cta_vol.mto_deducido            ,
		  cod_rechazo_ent       LIKE ret_cta_vol.cod_rechazo_ent,   #* Rech. *
        consecutivo           INTEGER                                  ,
        usuario               LIKE ret_cta_vol.usuario                 ,
        estado                LIKE ret_cta_vol.estado
    END RECORD

    DEFINE #loc #smallint
        cont_1, sw_6, sw_2    SMALLINT

    LET reg_9.* = reg_2.*

    SELECT descripcion
    INTO   reg_2.des_tipo_pago
    FROM   tab_pago
    WHERE  tipo_pago = reg_2.tipo_pago

    DISPLAY reg_2.des_tipo_pago TO des_tipo_pago

    LET sw_2 = 0
    LET sw_6 = 0

    INPUT BY NAME reg_2.n_folio_sol      ,
                  reg_2.tipo_ret         ,
                  reg_2.mto_solic        ,
                  reg_2.porcentaje_solic ,
                  reg_2.tipo_pago        ,
                  reg_2.fecha_solic      WITHOUT DEFAULTS

        BEFORE FIELD n_folio_sol
            IF sw_2 = 0 THEN

                LET aux_monto_en_pesos          = 0
                LET precio_dia_sb1              = 0
                LET precio_dia_sb3              = 0
                LET reg_2.monto_en_acciones_sb1 = 0
                LET reg_2.monto_en_acciones_sb3 = 0
                LET reg_2.monto_en_pesos        = 0

                CALL precio_accion( HOY, 1 ) RETURNING precio_dia_sb1

                SELECT NVL( SUM(A.monto_en_acciones), 0 ),
                       NVL( SUM(A.monto_en_acciones), 0 ) * precio_dia_sb1
                INTO   reg_2.monto_en_acciones_sb1,
                       reg_2.monto_en_pesos
                FROM   dis_cuenta A
                WHERE  A.nss             = reg_1.n_seguro
                AND    A.subcuenta       IN(3,10)
                AND    A.siefore         = 1
                AND    A.tipo_movimiento <> 0

                LET aux_monto_en_pesos = reg_2.monto_en_pesos
                LET reg_2.monto_en_pesos = 0 

                --sveraCALL precio_accion(HOY,3)
                --sveraRETURNING precio_dia_sb3

                SELECT NVL( SUM(A.monto_en_acciones), 0 ),
                       NVL( SUM(A.monto_en_acciones), 0 ) * precio_dia_sb3
                INTO   reg_2.monto_en_acciones_sb3,
                       reg_2.monto_en_pesos
                FROM   dis_cuenta A
                WHERE  A.nss             = reg_1.n_seguro
                AND    A.subcuenta       IN ( 3, 10 )
                AND    A.siefore         = 3
                AND    A.tipo_movimiento <> 0
 
                LET reg_2.monto_en_pesos = reg_2.monto_en_pesos + aux_monto_en_pesos

                DISPLAY reg_2.monto_en_acciones_sb1 TO monto_en_acciones_sb1
                DISPLAY reg_2.monto_en_acciones_sb3 TO monto_en_acciones_sb3
                DISPLAY reg_2.monto_en_pesos TO monto_en_pesos

                LET sw_2                 = 1
                LET reg_2.fecha_captura  = HOY
                LET reg_2.usuario        = c8_usuario
                LET reg_2.ultimo_proceso = HOY

                DISPLAY reg_2.fecha_captura  TO fecha_captura
                DISPLAY reg_2.usuario        TO usuario
                DISPLAY reg_2.ultimo_proceso TO ultimo_proceso
            END IF

        AFTER FIELD n_folio_sol
            IF reg_2.n_folio_sol IS NULL THEN
                ERROR ""
                ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                NEXT FIELD n_folio_sol
            ELSE
                SELECT "OK"
                FROM   ret_cta_vol
                WHERE  n_folio_sol <> reg_9.n_folio_sol
                AND    n_folio_sol  = reg_2.n_folio_sol
                GROUP BY 1

                IF STATUS <> NOTFOUND THEN
                    ERROR ""
                    ERROR "   SOLICITUD YA INGRESADA " ATTRIBUTE( NORMAL )
                    NEXT FIELD n_folio_sol
                END IF
            END IF

        AFTER FIELD tipo_ret
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD n_folio_sol
            END IF

            IF reg_2.tipo_ret IS NULL THEN
                CALL despliega_tipo_ret() #dtr
                    RETURNING reg_2.tipo_ret    ,
                              reg_2.des_tipo_ret

                IF reg_2.tipo_ret = 1 THEN
                    DISPLAY reg_2.tipo_ret     TO tipo_ret
                    DISPLAY reg_2.des_tipo_ret TO des_tipo_ret
                    NEXT FIELD mto_solic
                ELSE
                    LET reg_2.mto_solic        = reg_2.monto_en_pesos
                    LET reg_2.porcentaje_solic = 100

                    DISPLAY reg_2.tipo_ret         TO tipo_ret
                    DISPLAY reg_2.des_tipo_ret     TO des_tipo_ret
                    DISPLAY reg_2.mto_solic        TO mto_solic
                    DISPLAY reg_2.porcentaje_solic TO porcentaje_solic
                    NEXT FIELD tipo_pago
                END IF
            ELSE
                SELECT des_tipo_ret
                INTO   reg_2.des_tipo_ret
                FROM   tab_retiro_old
                WHERE  tipo_ret = reg_2.tipo_ret
   
                IF STATUS = NOTFOUND THEN
                    ERROR ""
                    ERROR "   TIPO DE RETIRO INEXISTENTE " ATTRIBUTE( NORMAL )
                    NEXT FIELD tipo_ret
                ELSE
                    IF reg_2.tipo_ret = 1 THEN
                        DISPLAY reg_2.tipo_ret     TO tipo_ret
                        DISPLAY reg_2.des_tipo_ret TO des_tipo_ret
                    ELSE
                        LET reg_2.mto_solic        = reg_2.monto_en_pesos
                        LET reg_2.porcentaje_solic = 100
   
                        DISPLAY reg_2.tipo_ret         TO tipo_ret
                        DISPLAY reg_2.des_tipo_ret     TO des_tipo_ret
                        DISPLAY reg_2.mto_solic        TO mto_solic
                        DISPLAY reg_2.porcentaje_solic TO porcentaje_solic
                        NEXT FIELD tipo_pago
                    END IF
                END IF
            END IF
    
        AFTER FIELD mto_solic
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD tipo_ret
            END IF

            IF reg_2.tipo_ret = 1 AND reg_2.mto_solic IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE( NORMAL ) 
                NEXT FIELD mto_solic
            END IF

            IF reg_2.mto_solic IS NULL THEN
                NEXT FIELD porcentaje_solic
            END IF

            IF reg_2.mto_solic <= 0 THEN
                ERROR ""
                ERROR "   MONTO INVALIDO" ATTRIBUTE( NORMAL )
                NEXT FIELD mto_solic
            ELSE
                IF reg_2.mto_solic > reg_2.monto_en_pesos THEN
                    ERROR "   MONTO SOLICITADO NO PUEDE SER SUPERIOR ",
                          "AL SALDO DE LA CUENTA" ATTRIBUTE( NORMAL )
                          NEXT FIELD mto_solic
                END IF

                LET reg_2.porcentaje_solic = (( reg_2.mto_solic * 100 )/
                                                reg_2.monto_en_pesos  )

                DISPLAY reg_2.porcentaje_solic TO porcentaje_solic
                NEXT FIELD tipo_pago
            END IF

        AFTER FIELD porcentaje_solic
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD mto_solic
            END IF

            IF reg_2.porcentaje_solic <= 0 THEN
                ERROR ""
                ERROR "   PORCENTAJE INVALIDO" ATTRIBUTE( NORMAL )
            ELSE
                LET monto_paso = (reg_2.monto_en_pesos *
                                 (reg_2.porcentaje_solic / 100))
                LET reg_2.mto_solic = monto_paso
                DISPLAY reg_2.mto_solic TO mto_solic
            END IF

        BEFORE FIELD tipo_pago
            IF sw_6 = 0 THEN
                IF reg_9.tipo_pago = 3 THEN
                    DECLARE cur_10 CURSOR FOR
                    SELECT *
                    FROM   ret_cheque
                    WHERE  consecutivo = reg_9.consecutivo

                    LET cont_1 = 1
                    FOREACH cur_10 INTO arr_2[cont_1].*
                        LET cont_1 = cont_1 + 1
                    END FOREACH
                    LET sw_6 = 1
                ELSE
                    DECLARE cur_11 CURSOR FOR
                    SELECT *
                    FROM   ret_abono_cuenta
                    WHERE  consecutivo = reg_9.consecutivo

                    LET cont_1 = 1
                    FOREACH cur_11 INTO arr_3[cont_1].*
                        LET cont_1 = cont_1 + 1
                    END FOREACH
                    LET sw_6 = 1
                END IF
            END IF

        AFTER FIELD tipo_pago
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD porcentaje_solic
            END IF

            IF reg_2.tipo_pago IS NULL THEN
                CALL despliega_tipo_pago() #dtp
                    RETURNING reg_2.tipo_pago,
                              reg_2.des_tipo_pago

                    DISPLAY reg_2.des_tipo_pago TO des_tipo_pago

                    CASE reg_2.tipo_pago
							 WHEN 1
								  CALL agrega_orden_de_pago(reg_1.n_seguro    ,
														reg_2.consecutivo ,
												 reg_1.paterno     ,
												 reg_1.materno     ,
												 reg_1.nombres     ,
												 100               ,
												 0                 ,
												 sw_2
												 )
                        WHEN 2
			   {
                            CALL agrega_abono_cta(reg_1.n_seguro    ,
                                                  ""                ,
                                                  reg_1.paterno     ,
                                                  reg_1.materno     ,
                                                  reg_1.nombres     ,
                                                  reg_2.consecutivo ,
                                                  sw_4
                                                 ) #aac
			    }
                            LET sw_2 = 1

                        WHEN 3
                            CALL agrega_cheque(""                ,#num_resolu
                                               reg_1.n_seguro    ,
                                               ""                ,#tipo_prest
                                               reg_1.paterno     ,
                                               reg_1.materno     ,
                                               reg_1.nombres     ,
                                               reg_2.consecutivo ,
                                               sw_2
                                              ) #glo #ac 
                            LET sw_2 = 1

                        OTHERWISE
                            ERROR ""
                            ERROR "   TIPO DE PAGO INEXISTENTE"
                                  ATTRIBUTE( NORMAL )
                            NEXT FIELD tipo_pago
                    END CASE
            ELSE
                SELECT descripcion
                INTO   reg_2.des_tipo_pago
                FROM   tab_pago
                WHERE  tipo_pago = reg_2.tipo_pago

                DISPLAY reg_2.des_tipo_pago TO des_tipo_pago

                CASE reg_2.tipo_pago
						 WHEN 1
						LET sw_2 = 1
							  CALL agrega_orden_de_pago(reg_1.n_seguro    ,
									  reg_2.consecutivo ,
									  reg_1.paterno     ,
									  reg_1.materno     ,
									  reg_1.nombres     ,
									  100               ,
									  0                 ,
									  sw_2
						  )
                    WHEN 2
                        LET sw_2 = 1
		{
                        CALL agrega_abono_cta(reg_1.n_seguro    ,
                                              ""                ,
                                              reg_1.paterno     ,
                                              reg_1.materno     ,
                                              reg_1.nombres     ,
                                              reg_2.consecutivo ,
                                              sw_2 ) #glo #aac
		}
                    WHEN 3
                        LET sw_2 = 1
                        CALL agrega_cheque(""                ,#num_resolu
                                           reg_1.n_seguro    ,
                                           ""                ,#tipo_prest
                                           reg_1.paterno     ,
                                           reg_1.materno     ,
                                           reg_1.nombres     ,
                                           reg_2.consecutivo ,
                                           sw_2 ) #glo #ac 

                    OTHERWISE
                        ERROR ""
                        ERROR "   TIPO DE PAGO INEXISTENTE" ATTRIBUTE( NORMAL )
                        NEXT FIELD tipo_pago
                END CASE
                DISPLAY reg_2.des_tipo_pago TO des_tipo_pago
            END IF
                
        AFTER FIELD fecha_solic
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD tipo_pago
            END IF

            IF reg_2.fecha_solic IS NULL THEN
                ERROR ""
                ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                NEXT FIELD fecha_solic
            END IF

            IF reg_2.fecha_solic > HOY THEN
                ERROR ""
                ERROR "   FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
                    ATTRIBUTE( NORMAL )
                    NEXT FIELD fecha_solic
            END IF

        ON KEY( ESC )
            IF reg_2.n_folio_sol IS NULL THEN
                ERROR ""
                ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                NEXT FIELD n_folio_sol
            ELSE
                SELECT "OK"
                FROM   ret_cta_vol
                WHERE  n_folio_sol <> reg_9.n_folio_sol
                AND    n_folio_sol  = reg_2.n_folio_sol
                GROUP BY 1

                IF STATUS <> NOTFOUND THEN
                    ERROR ""
                    ERROR "   SOLICITUD YA INGRESADA " ATTRIBUTE( NORMAL )
                    NEXT FIELD n_folio_sol
                END IF
            END IF

            IF reg_2.tipo_ret IS NULL THEN
                ERROR ""
                ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE( NORMAL )
                NEXT FIELD tipo_ret
            ELSE
                SELECT des_tipo_ret
                INTO   reg_2.des_tipo_ret
                FROM   tab_retiro_old
                WHERE  tipo_ret = reg_2.tipo_ret
   
                IF STATUS = NOTFOUND THEN
                    ERROR ""
                    ERROR "   TIPO DE RETIRO INEXISTENTE " ATTRIBUTE( NORMAL )
                    NEXT FIELD tipo_ret
                END IF
            END IF

            IF reg_2.tipo_pago IS NULL THEN
                ERROR ""
                ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE( NORMAL )
                NEXT FIELD tipo_pago
            ELSE
                SELECT descripcion
                INTO   reg_2.des_tipo_pago
                FROM   tab_pago
                WHERE  tipo_pago = reg_2.tipo_pago

                IF STATUS = NOTFOUND THEN
                    ERROR ""
                    ERROR "   TIPO DE PAGO INEXISTENTE" ATTRIBUTE( NORMAL )
                    NEXT FIELD tipo_pago
                END IF
                DISPLAY reg_2.des_tipo_pago TO des_tipo_pago
            END IF

            IF reg_2.fecha_solic IS NULL THEN
                ERROR ""
                ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                NEXT FIELD fecha_solic
            ELSE
                IF reg_2.fecha_solic > HOY THEN
                    ERROR ""
                    ERROR "   FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
                        ATTRIBUTE( NORMAL )
                    NEXT FIELD fecha_solic
                END IF
            END IF

            UPDATE ret_cta_vol
            SET    n_folio_sol       = reg_2.n_folio_sol      ,
                   tipo_ret          = reg_2.tipo_ret         ,
                   mto_solic         = reg_2.mto_solic        ,
                   porcentaje_solic  = reg_2.porcentaje_solic ,
                   tipo_pago         = reg_2.tipo_pago        ,
                   fecha_solic       = reg_2.fecha_solic      ,
                   fecha_captura     = reg_2.fecha_captura    ,
                   ultimo_proceso    = reg_2.ultimo_proceso   ,
                   estado            = reg_3.capturado        ,
                   usuario           = c8_usuario
            WHERE  consecutivo       = reg_2.consecutivo

            DISPLAY "SOLICITUD MODIFICADA" AT 20, 2 ATTRIBUTE( REVERSE )
            SLEEP 3
            LET sw_2 = 0
            INITIALIZE reg_2.* TO NULL
            INITIALIZE reg_1.* TO NULL

            LET sw_1 = 1
            EXIT INPUT

        ON KEY( CONTROL-C )
            LET reg_2.* = reg_9.*
            DISPLAY BY NAME reg_2.*

            IF reg_9.tipo_pago = 3 THEN
                DELETE FROM ret_cheque
                WHERE consecutivo = reg_9.consecutivo

                FOR cont_1 = 1 TO 5
                    IF arr_2[cont_1].consecutivo > 0 THEN
                        INSERT INTO ret_cheque VALUES (arr_2[cont_1].*)
                    END IF
                END FOR
            ELSE
                DELETE FROM ret_abono_cuenta
                WHERE consecutivo = reg_9.consecutivo
                FOR cont_1 = 1 TO 5
                    IF arr_3[cont_1].consecutivo > 0 THEN
                        INSERT INTO ret_abono_cuenta VALUES( arr_3[cont_1].* )
                    END IF
                END FOR
            END IF
            EXIT INPUT

        ON KEY( INTERRUPT )
            LET reg_2.* = reg_9.*
            DISPLAY BY NAME reg_2.*
            EXIT INPUT
    END INPUT
END FUNCTION

FUNCTION elimina()
#e----------------
    DEFINE #loc #char
        c9des_estado_solic    CHAR(009) ,
        hace_el_input         CHAR(300) ,
        hace_el_select        CHAR(500)

    DEFINE #loc #smallint
        s_tot_registros       ,
        arr_c                 ,
        scr_l                 ,
        pos                   SMALLINT

    DISPLAY "" AT 1, 1
    DISPLAY "" AT 2, 1
    DISPLAY " [ Ctrl-C ] Regresa al menu "   AT 1, 1
    DISPLAY " [ Ctrl-V ] Consulta beneficiarios " AT 2, 1
    DISPLAY " [ ENTER  ] Elimina         "   AT 1, 36
    DISPLAY " ELIMINA" AT 1, 65 ATTRIBUTE( REVERSE, BOLD )
    DISPLAY "            DISPOSICION DE RECURSOS DE APORTACIONES VOLUNTARIAS                " AT 8, 1 ATTRIBUTE( REVERSE )

    LET sw_1          = 0
    LET INT_FLAG      = FALSE
    LET hace_el_input = NULL

    INITIALIZE reg_1.* TO NULL

    CONSTRUCT hace_el_input ON A.n_folio  ,
                               A.n_seguro ,
                               A.n_rfc    ,
                               A.n_unico  ,
                               A.paterno  ,
                               A.materno  ,
                               A.nombres
                          FROM n_folio  ,
                               n_seguro ,
                               n_rfc    ,
                               n_unico  ,
                               paterno  ,
                               materno  ,
                               nombres

        ON KEY( ESC )
            ERROR ""
            ERROR " PROCESANDO INFORMACION..."
            LET int_flag = FALSE
            EXIT CONSTRUCT

         ON KEY( CONTROL-C )
            CLEAR FORM
            LET int_flag = TRUE
            EXIT CONSTRUCT
    END CONSTRUCT

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR ""
        ERROR " BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        RETURN
    END IF 

    LET hace_el_select = " SELECT A.n_folio  ,",
                         " A.n_seguro ,",
                         " A.n_rfc    ,",
                         " A.n_unico  ,",
                         " A.fentcons ,",
                         " A.paterno  ,",
                         " A.materno  ,",
                         " A.nombres  ,",
                         " USER        ",
                         " FROM   afi_mae_afiliado A, ret_cta_vol B",
                         " WHERE ",hace_el_input CLIPPED,
                         " AND    A.n_seguro = B.n_seguro ",
                         " GROUP BY 1, 2, 3, 4, 5, 6, 7, 8, 9 ",
                         " ORDER BY 2, 3" CLIPPED

    LET pos = 1
    PREPARE pre_13 FROM hace_el_select
    DECLARE cur_13 CURSOR FOR pre_13

    FOREACH cur_13 INTO reg_1.*,c8_usuario   
        DECLARE cur_14 CURSOR FOR
        SELECT 0,
               0,
               0,
               0,
               0,
               0,
               0,
               0,
               0,
               A.fecha_ult_ret,
               A.n_folio_sol,
               A.tipo_ret,
               B.des_tipo_ret,
               A.mto_solic,
               A.porcentaje_solic,
               A.tipo_pago,
               "",                    #des_tipo_pago
               A.edad,
               A.fecha_solic,
               A.fecha_captura,
               A.ultimo_proceso,
               A.pension_invalidez,
               A.deduccion,
               A.fecha_deduccion,
               A.mto_deducido,
					'',							#* Rech., cod_rechazo_ent *
               A.consecutivo,
               A.usuario,
               A.estado
        FROM  ret_cta_vol A, tab_retiro_old B
        WHERE A.n_seguro  = reg_1.n_seguro
        AND   A.tipo_ret  = B.tipo_ret
		  AND cod_rechazo_ent IS NULL      -- Rech. *
        ORDER BY fecha_captura

        FOREACH cur_14 INTO reg_2.*
            LET aux_monto_en_pesos          = 0
            LET precio_dia_sb1              = 0
            LET precio_dia_sb3              = 0
            LET reg_2.monto_en_acciones_sb1 = 0
            LET reg_2.monto_en_acciones_sb3 = 0
            LET reg_2.monto_en_pesos        = 0

            CALL precio_accion( reg_2.fecha_captura, 1 )
            RETURNING precio_dia_sb1

            SELECT NVL( SUM(A.monto_en_acciones), 0 ),
                   NVL( SUM(A.monto_en_acciones), 0 ) * precio_dia_sb1
            INTO   reg_2.monto_en_acciones_sb1,
                   reg_2.monto_en_pesos
            FROM   dis_cuenta A
            WHERE  A.nss             = reg_1.n_seguro
            AND    A.subcuenta       IN ( 3, 10 )
            AND    A.fecha_conversion <= reg_2.fecha_captura
            AND    A.siefore         = 1

            LET aux_monto_en_pesos = reg_2.monto_en_pesos
            LET reg_2.monto_en_pesos = 0

            --sveraCALL precio_accion(reg_2.fecha_captura,3)
            --sveraRETURNING precio_dia_sb3

            SELECT NVL( SUM(A.monto_en_acciones), 0 ),
                   NVL( SUM(A.monto_en_acciones), 0 ) * precio_dia_sb3
            INTO   reg_2.monto_en_acciones_sb3,
                   reg_2.monto_en_pesos
            FROM   dis_cuenta A
            WHERE  A.nss             = reg_1.n_seguro
            AND    A.subcuenta       IN(3,10)
            AND    A.fecha_conversion <= reg_2.fecha_captura
            AND    A.siefore         = 3

            LET reg_2.monto_en_pesos = reg_2.monto_en_pesos + aux_monto_en_pesos

            SELECT descripcion
            INTO   reg_2.des_tipo_pago
            FROM   tab_pago
            WHERE  tipo_pago = reg_2.tipo_pago

            DISPLAY reg_2.des_tipo_pago TO des_tipo_pago

            LET reg_2_array[pos].var_nula          =  " "
            LET reg_2_array[pos].n_folio           =  reg_1.n_folio
            LET reg_2_array[pos].n_seguro          =  reg_1.n_seguro 
            LET reg_2_array[pos].n_rfc             =  reg_1.n_rfc   
            LET reg_2_array[pos].n_unico           =  reg_1.n_unico  
            LET reg_2_array[pos].fentcons          =  reg_1.fentcons 
            LET reg_2_array[pos].paterno           =  reg_1.paterno  
            LET reg_2_array[pos].materno           =  reg_1.materno  
            LET reg_2_array[pos].nombres           =  reg_1.nombres 
            LET reg_2_array[pos].monto_en_acciones_sb1 =  reg_2.monto_en_acciones_sb1
            LET reg_2_array[pos].monto_en_acciones_sb3 =  reg_2.monto_en_acciones_sb3
            LET reg_2_array[pos].monto_en_pesos    =  reg_2.monto_en_pesos
            LET reg_2_array[pos].fecha_ult_ret     =  reg_2.fecha_ult_ret
            LET reg_2_array[pos].n_folio_sol       =  reg_2.n_folio_sol
            LET reg_2_array[pos].tipo_ret          =  reg_2.tipo_ret
            LET reg_2_array[pos].des_tipo_ret      =  reg_2.des_tipo_ret
            LET reg_2_array[pos].mto_solic         =  reg_2.mto_solic
            LET reg_2_array[pos].porcentaje_solic  =  reg_2.porcentaje_solic
            LET reg_2_array[pos].tipo_pago         =  reg_2.tipo_pago
            LET reg_2_array[pos].des_tipo_pago     =  reg_2.des_tipo_pago
            LET reg_2_array[pos].edad              =  reg_2.edad
            LET reg_2_array[pos].fecha_solic       =  reg_2.fecha_solic
            LET reg_2_array[pos].fecha_captura     =  reg_2.fecha_captura
            LET reg_2_array[pos].ultimo_proceso    =  reg_2.ultimo_proceso
            LET reg_2_array[pos].pension_invalidez =  reg_2.pension_invalidez
            LET reg_2_array[pos].deduccion         =  reg_2.deduccion
            LET reg_2_array[pos].fecha_deduccion   =  reg_2.fecha_deduccion
            LET reg_2_array[pos].mto_deducido      =  reg_2.mto_deducido
            LET reg_2_array[pos].consecutivo       =  reg_2.consecutivo
            LET reg_2_array[pos].usuario           =  reg_2.usuario
            LET reg_2_array[pos].estado            =  reg_2.estado
            LET pos = pos + 1
        END FOREACH
    END FOREACH       

    CALL SET_COUNT(pos-1)
    ERROR ""

    LET s_tot_registros = pos - 1
    IF( pos - 1 ) >= 1 THEN
        INPUT ARRAY reg_2_array WITHOUT DEFAULTS FROM scr_3.*
            BEFORE ROW
                LET arr_c = ARR_CURR()
                IF arr_c >= pos THEN
                   ERROR "   NO HAY MAS REGISTROS HACIA ABAJO" ATTRIBUTE( NORMAL )
                END IF

                SELECT descripcion
                INTO   c9des_estado_solic
                FROM   ret_estado A
                WHERE  A.estado_solicitud = reg_2_array[arr_c].estado

                DISPLAY "Total Registros ",s_tot_registros AT 19,56
                DISPLAY c9des_estado_solic AT 20, 41

            ON KEY( CONTROL-M )
                IF (reg_2_array[arr_c].estado = reg_3.capturado) OR
                   (reg_2_array[arr_c].estado = reg_3.precapturado ) THEN
                   WHILE TRUE
                      PROMPT " ESTA SEGURO S/N " FOR CHAR aux_pausa

                --      IF enter MATCHES "[SsNn]" THEN
                         IF aux_pausa MATCHES "[Ss]" THEN
                            DELETE
                            FROM   ret_cheque
                            WHERE  n_seguro    = reg_2_array[arr_c].n_seguro
                            AND    consecutivo = reg_2_array[arr_c].consecutivo
                           
                            DELETE
                            FROM   ret_abono_cuenta
                            WHERE  n_seguro    = reg_2_array[arr_c].n_seguro
                            AND    consecutivo = reg_2_array[arr_c].consecutivo
                           
                            DELETE 
                            FROM   ret_cta_vol
                            WHERE  consecutivo = reg_2_array[arr_c].consecutivo

                            LET v_marca_ent = 490

                            LET v_desmarca ="EXECUTE PROCEDURE desmarca_cuenta('",
                                             reg_2_array[arr_c].n_seguro,"',",
                                             v_marca_ent,",",
                                             reg_2_array[arr_c].consecutivo,",",
                                             0 ,",",
                                             0 ,",",
                                             "'",usuario,"'",
                                             ")"

                            PREPARE eje_desmarca3 FROM v_desmarca
                            EXECUTE eje_desmarca3
                            
                            ERROR"   REGISTRO ELIMINADO " ATTRIBUTE( NORMAL )
                            CLEAR FORM
                            EXIT INPUT
                         ELSE
                           { ERROR "ELIMINACION CANCELADA "
                            SLEEP 2
                            ERROR ""
                            CLEAR FORM
                            CLEAR SCREEN
                            EXIT INPUT
                            RETURN}
                            PROMPT " ELIMINACION CANCELADA...<ENTER> PARA",
                                   " SALIR " FOR CHAR aux_pausa
                            CLEAR FORM
                            CLEAR SCREEN 
                            EXIT INPUT
                            
             --            END IF
                      END IF
                   END WHILE
                ELSE
                  PROMPT" REGISTRO LIQUIDADO, NO SE PUEDE ELIMINAR",
                        "...<ENTER> PARA CONTINUAR" FOR CHAR enter
                  EXIT INPUT
                END IF

            ON KEY( CONTROL-V )
                LET arr_c = ARR_CURR()

                CASE reg_2_array[arr_c].tipo_pago
                    WHEN 2
                       #CALL consulta_abono_cta(reg_2_array[arr_c].consecutivo) 
                    WHEN 3
                        CALL consulta_cheque(reg_2_array[arr_c].consecutivo) #cc
                END CASE

            ON KEY( INTERRUPT )
                DISPLAY "Total Registros               " AT 19,56
                DISPLAY "         " AT 19,41
                CLEAR FORM
                EXIT INPUT

            ON KEY( CONTROL-C )
                DISPLAY "Total Registros               " AT 19,56
                DISPLAY "         " AT 19,41
                CLEAR FORM
                EXIT INPUT
        END INPUT
    ELSE
        ERROR "   NO EXISTEN REGISTROS" ATTRIBUTE ( NORMAL )
    END IF     
END FUNCTION

FUNCTION regresa_mes6(fecha_retiro)
DEFINE
   fecha_retiro           DATE,
   fecha_6meses           DATE,
   dia, mes, ano          SMALLINT,
   bis                    SMALLINT,
   cadena                 CHAR(10)

   LET ano   = YEAR(fecha_retiro)
   LET mes   = MONTH(fecha_retiro)
   LET dia   = DAY(fecha_retiro)

   IF mes = 3 OR mes = 5 OR mes = 8 OR mes = 10 OR mes = 12 THEN
      IF mes = 8 THEN
         IF dia >= 29 THEN
            LET bis = (ano+1) MOD 4
            IF bis = 0 THEN
               LET dia = 29
	    ELSE
               LET dia = 28
            END IF
            LET ano = ano + 1
	    LET cadena = "02/",dia USING "&&","/",ano USING "&&&&"
	    LET fecha_6meses = cadena
         ELSE
	    LET fecha_6meses = fecha_retiro + 6 UNITS MONTH
         END IF
      ELSE
	 IF dia = 31 THEN
	    LET dia = 30
	 END IF
         IF mes > 5 THEN
	     IF mes = 10 THEN
	        LET mes = 4
             END IF
	     IF mes = 12 THEN
		LET mes = 6
             END IF
	     LET ano = ano + 1
         ELSE
	     IF mes = 3 THEN
		LET mes = 9
	     END IF
	     IF mes = 5 THEN
	        LET mes = 11
	     END IF
	 END IF
         LET cadena = mes USING "&&","/",dia USING "&&","/",ano USING "&&&&"
	 LET fecha_6meses = cadena
      END IF
   ELSE
      LET fecha_6meses = fecha_retiro + 6 UNITS MONTH
   END IF
   RETURN fecha_6meses
END FUNCTION

FUNCTION valida_cta_saldo_vol( v_nss )
#vcsv--------------------------
	DEFINE    #loc #char 
		enter                  CHAR(01),
		v_nss                  CHAR(11)                   
	DEFINE    #loc #decimal
		v_monto_acc3           DECIMAL(16,6),
		v_monto_acc10          DECIMAL(16,6),
		v_monto_en_acciones    DECIMAL(16,6),
		v_sldo_acc3            DECIMAL(16,6),
		v_sldo_acc10           DECIMAL(16,6) 
	DEFINE    #loc #date
		v_fecha_conversion     DATE
	DEFINE    #loc #smallint
		v_subcuenta            SMALLINT

	UNLOAD TO "sv_resp_cta_saldo_vol.unl"
	SELECT A.* FROM cta_saldo_vol A,ret_cta_vol B
		WHERE A.nss = B.n_seguro
			AND ( B.estado = 3 OR B.estado = 0 )
    
	SELECT SUM( A.monto_en_acciones ) INTO v_monto_acc3
		FROM dis_cuenta A
		WHERE A.nss = v_nss AND A.subcuenta = 3

	SELECT SUM(A.monto_en_acciones) INTO v_monto_acc10
		FROM dis_cuenta A
		WHERE A.nss = v_nss AND A.subcuenta = 10

	IF( v_monto_acc3 IS NULL OR v_monto_acc3 = 0 OR v_monto_acc3 = " " ) 
		AND ( v_monto_acc10 IS NULL OR v_monto_acc10 = 0   
		OR v_monto_acc10 = " " ) THEN
		PROMPT "   TRABAJADOR CON SALDO CERO, NSS", v_nss FOR CHAR enter
		EXIT PROGRAM
	END IF

	SELECT SUM(A.saldo_acciones) INTO v_sldo_acc3          
		FROM cta_saldo_vol A
		WHERE A.nss = v_nss AND A.subcuenta = 3

	IF v_sldo_acc3 IS NULL OR v_sldo_acc3 = " " THEN
		LET v_sldo_acc3 = 0
	END IF

	IF v_sldo_acc3 <> v_monto_acc3 THEN
		PROMPT " NO CUADRA SALDO DE CTA_SALDO_VOL VS DIS_CUENTA (subcta. 3) NSS:",
			v_nss FOR CHAR enter
		EXIT PROGRAM
	END IF

	SELECT SUM(A.saldo_acciones) INTO v_sldo_acc10
		FROM cta_saldo_vol A
		WHERE A.nss = v_nss AND A.subcuenta = 10

	IF v_sldo_acc10 IS NULL OR v_sldo_acc10 = " " THEN
		LET v_sldo_acc10 = 0
	END IF
	IF v_sldo_acc10 <> v_monto_acc10 THEN
		PROMPT " NO CUADRA SALDO DE CTA_SALDO_VOL VS DIS_CUENTA (subcta.10) NSS:",
			v_nss FOR CHAR enter
		EXIT PROGRAM
	END IF

{svera
	DECLARE cur_val2 CURSOR FOR
		SELECT subcuenta, fecha_conversion, monto_en_acciones
			FROM cta_saldo_vol
			WHERE nss = v_nss

	FOREACH cur_val2 INTO v_subcuenta, v_fecha_conversion , v_monto_en_acciones
		SELECT "OK" FROM dis_cuenta
			WHERE nss = v_nss AND subcuenta = v_subcuenta
				AND fecha_conversion  = v_fecha_conversion
				AND monto_en_acciones = v_monto_en_acciones

		IF STATUS = NOTFOUND THEN
			PROMPT " NO CUADRAN DATOS DE CTA_SALDO_VOL VS DIS_CUENTA NSS:", v_nss
				FOR CHAR enter
			EXIT PROGRAM
		END IF
	END FOREACH
svera}
END FUNCTION 

FUNCTION valores_inpc( fdeduccion )
#vi------------------------------
    DEFINE #loc #date
        fdeduccion            DATE

    DEFINE #loc #char
        anio_factual          CHAR(004),
        anio_fdeduccion       CHAR(004),
        c_mes_factual         CHAR(010),
        c_mes_fdeduccion      CHAR(010),
        c10_factual           CHAR(010),
        c10_fdeduccion        CHAR(010),
        mes_factual           CHAR(002),
        mes_fdeduccion        CHAR(002)

    DEFINE #loc #decimal
        inpc_actual           ,
        inpc_deduccion        DECIMAL(16,6)

    LET c10_factual =  HOY
    LET mes_factual = c10_factual[01,02]

    CALL mes_letras(mes_factual)
    RETURNING c_mes_factual

    LET c10_fdeduccion = fdeduccion
    LET mes_fdeduccion = c10_fdeduccion[01,02]

    CALL mes_letras(mes_fdeduccion)
    RETURNING c_mes_fdeduccion

    LET anio_factual = c10_factual[07,10]
    LET anio_fdeduccion = c10_fdeduccion[07,10]

    SELECT valor_inpc
    INTO   inpc_actual
    FROM   tab_inpc
    WHERE  mes_aplica = c_mes_factual
    AND    anio_aplica = anio_factual

    IF STATUS = NOTFOUND THEN
        LET c10_factual = MDY(MONTH(HOY)-1,DAY(HOY),YEAR(HOY))

        LET mes_factual = c10_factual[01,02]

        CALL mes_letras(mes_factual)
        RETURNING c_mes_factual

        SELECT valor_inpc
        INTO   inpc_actual
        FROM   tab_inpc
        WHERE  mes_aplica = c_mes_factual
        AND    anio_aplica = anio_factual

        IF STATUS = NOTFOUND THEN
            PROMPT " NO SE ENCONTRO VALOR DE INPC DEL MES :",c_mes_factual ,"Y AÑO :",anio_factual
            FOR CHAR enter
            EXIT PROGRAM
        END IF
    END IF

    SELECT valor_inpc
    INTO   inpc_deduccion
    FROM   tab_inpc
    WHERE  mes_aplica  = c_mes_fdeduccion
    AND    anio_aplica = anio_fdeduccion

    IF STATUS = NOTFOUND THEN
        PROMPT " NO SE ENCUENTRA CARGADO VALOR DE INPC DEL MES :",c_mes_fdeduccion ,"Y AÑO :",anio_fdeduccion
        FOR CHAR enter
        EXIT PROGRAM
    END IF
    RETURN  inpc_actual,inpc_deduccion
END FUNCTION

FUNCTION mes_letras( mes )
#ml-----------------------
    DEFINE
        mes       CHAR(02) ,
        vmes      CHAR(10)

    CASE mes
        WHEN "01"
            LET vmes = "ENERO" 
        WHEN "02"
            LET vmes = "FEBRERO"
        WHEN "03"
            LET vmes=  "MARZO"
        WHEN "04"
            LET vmes = "ABRIL"
        WHEN "05"
            LET vmes = "MAYO"
        WHEN "06"
            LET vmes = "JUNIO"
        WHEN "07"
            LET vmes = "JULIO"
        WHEN "08"
            LET vmes = "AGOSTO"
        WHEN "09"
            LET vmes = "SEPTIEMBRE"
        WHEN "10"
            LET vmes = "OCTUBRE"
        WHEN "11"
            LET vmes = "NOVIEMBRE"
        WHEN "12"
            LET vmes = "DICIEMBRE"
    END CASE
RETURN  vmes
END FUNCTION

FUNCTION detalle_acc_liq()
#dal----------------------
    DEFINE arr_4 ARRAY[100] OF RECORD #loc #arr_4
        n_seguro                CHAR(11) ,
        nombre                  CHAR(43) ,
        mto_acc_sb1             DECIMAL(22,6),
        mto_acc_sb3             DECIMAL(22,6)
    END RECORD

    DEFINE reg_17 RECORD #loc #reg_17
        n_seguro                CHAR(11) ,
        paterno                 CHAR(40) ,
        materno                 CHAR(40) ,
        nombres                 CHAR(40) ,
        tipo_ret                SMALLINT ,
        mto_solic               DECIMAL(16,6)
    END RECORD 
   
    DEFINE #loc #decimal
        M1                      DECIMAL(22,6) ,
        vmto_acc_sb1            DECIMAL(22,6) ,
        vmto_acc_sb3            DECIMAL(22,6) ,
        vmto_pesos_sb1          DECIMAL(22,6) ,
        vmto_pesos_sb3          DECIMAL(22,6) ,
        vprec_dia_sb1           DECIMAL(11,6) ,
        vprec_dia_sb3           DECIMAL(11,6) ,     
        tot_acc_sb1             DECIMAL(22,6) ,
        tot_acc_sb3             DECIMAL(22,6) 

    DEFINE #loc #integer
        pos                   ,
        arr_c                 ,
        arr_l                 INTEGER

    OPEN WINDOW retm0034 AT 3,2 WITH FORM "RETM0034" ATTRIBUTE( BORDER )
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY "                              <Ctrl-C> Salir                                                                                " AT 1,1 ATTRIBUTE ( REVERSE )
    DISPLAY " RETM003          LIQUIDACION DE APORTACIONES VOLUNTARIAS                                                                   " AT 3,1 ATTRIBUTE( REVERSE )
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE( REVERSE )

    INITIALIZE reg_17.* TO NULL
    LET vprec_dia_sb1  = 0
    LET vprec_dia_sb3  = 0
    LET vmto_acc_sb1   = 0
    LET vmto_acc_sb3   = 0
    LET vmto_pesos_sb1 = 0 
    LET vmto_pesos_sb3 = 0 
    LET tot_acc_sb1    = 0
    LET tot_acc_sb3    = 0
    LET pos            = 0

    FOR pos = 1 TO 100
        INITIALIZE arr_4[pos].* TO NULL
    END FOR
    LET pos     = 0

    CALL precio_accion( HOY, 1 ) RETURNING vprec_dia_sb1

    --sveraCALL precio_accion(HOY,3)
    --sveraRETURNING vprec_dia_sb3

    DECLARE cur_15 CURSOR FOR
    SELECT n_seguro ,
           paterno  ,
           materno  ,
           nombres  ,
           tipo_ret ,
           mto_solic
    FROM   ret_cta_vol
    WHERE  estado = 3
    OR     estado = 0

    LET pos = 0
    FOREACH cur_15 INTO  reg_17.*
        LET pos = pos + 1
        LET arr_4[pos].n_seguro = reg_17.n_seguro
        LET arr_4[pos].nombre[1,26] = reg_17.paterno CLIPPED," ",
                                      reg_17.materno CLIPPED," ",
                                      reg_17.nombres CLIPPED

        IF reg_17.tipo_ret = 2 THEN   -- RETIRO TOTAL --

            SELECT SUM(A.monto_en_acciones)
            INTO   arr_4[pos].mto_acc_sb1
            FROM   dis_cuenta A
            WHERE  A.nss       = reg_17.n_seguro
            AND    A.subcuenta IN (3,10)
            AND    A.siefore   = 1

            IF arr_4[pos].mto_acc_sb1 IS NULL 
            OR arr_4[pos].mto_acc_sb1 = " " THEN
                LET  arr_4[pos].mto_acc_sb1 = 0
            END IF

            SELECT SUM(A.monto_en_acciones)
            INTO   arr_4[pos].mto_acc_sb3
            FROM   dis_cuenta A
            WHERE  A.nss       = reg_17.n_seguro
            AND    A.subcuenta IN (3,10)
            AND    A.siefore   = 3

            IF arr_4[pos].mto_acc_sb3 IS NULL
            OR arr_4[pos].mto_acc_sb3 = " " THEN
                LET  arr_4[pos].mto_acc_sb3 = 0
            END IF

            LET tot_acc_sb1 = tot_acc_sb1 + arr_4[pos].mto_acc_sb1
            LET tot_acc_sb3 = tot_acc_sb3 + arr_4[pos].mto_acc_sb3

        ELSE -- RETIRO PARCIAL --

            SELECT SUM(A.monto_en_acciones),
                   SUM(A.monto_en_acciones) * vprec_dia_sb1
            INTO   vmto_acc_sb1 ,
                   vmto_pesos_sb1
            FROM   dis_cuenta A
            WHERE  A.nss       = reg_17.n_seguro
            AND    A.subcuenta IN (3,10)
            AND    A.siefore   = 1

            IF vmto_pesos_sb1 IS NULL
            OR vmto_pesos_sb1 = " " THEN
                LET vmto_pesos_sb1 = 0
            END IF

            IF vmto_acc_sb1 IS NULL
            OR vmto_acc_sb1 = " " THEN
                LET vmto_acc_sb1 = 0
            END IF

            SELECT SUM(A.monto_en_acciones),
                   SUM(A.monto_en_acciones) * vprec_dia_sb3
            INTO   vmto_acc_sb3 ,
                   vmto_pesos_sb3
            FROM   dis_cuenta A
            WHERE  A.nss       = reg_17.n_seguro
            AND    A.subcuenta IN (3,10)
            AND    A.siefore   = 3

            IF vmto_pesos_sb3 IS NULL
            OR vmto_pesos_sb3 = " " THEN
                LET  vmto_pesos_sb3 = 0
            END IF

            IF vmto_acc_sb3 IS NULL
            OR vmto_acc_sb3 = " " THEN
                LET vmto_acc_sb3 = 0
            END IF

            LET M1 = reg_17.mto_solic - vmto_pesos_sb1

            IF M1 > 0 THEN
                LET arr_4[pos].mto_acc_sb1 = vmto_acc_sb1               
                LET arr_4[pos].mto_acc_sb3 = (M1 * vmto_acc_sb3) / vmto_pesos_sb3

            ELSE
                LET arr_4[pos].mto_acc_sb1 = (reg_17.mto_solic * vmto_acc_sb1 ) / vmto_pesos_sb1
                LET arr_4[pos].mto_acc_sb3 = 0
            END IF
            LET tot_acc_sb1 = tot_acc_sb1 + arr_4[pos].mto_acc_sb1
            LET tot_acc_sb3 = tot_acc_sb3 + arr_4[pos].mto_acc_sb3
        END IF
    END FOREACH

    DISPLAY tot_acc_sb1 to tot_acc_sb1
    DISPLAY tot_acc_sb3 to tot_acc_sb3

    CALL SET_COUNT(pos)
    DISPLAY ARRAY arr_4 TO scr_1.*
        ON KEY ( INTERRUPT )
            LET pos = 0
            EXIT DISPLAY

        ON KEY ( CONTROL-C )
            LET pos = 0
            EXIT DISPLAY
    END DISPLAY
    CLOSE WINDOW retm0034
END FUNCTION

FUNCTION calcula_mto_solic( vn_seguro, vfecha_ult_ret, vmto_solic )
#cms----------------------------------------------------------
    DEFINE #loc #char
        vn_seguro               CHAR(11)

    DEFINE #loc #char
        vfecha_ult_ret          DATE

    DEFINE #loc #decimal
        M1                      DECIMAL(16,6) ,
        mto_acc_sb1             DECIMAL(16,6) ,
        mto_acc_sb3             DECIMAL(16,6) ,
        vmto_acc_sb1            DECIMAL(16,6) ,
        vmto_acc_sb3            DECIMAL(16,6) ,
        vmto_pesos_sb1          DECIMAL(16,6) ,
        vmto_pesos_sb3          DECIMAL(16,6) ,
        vmto_solic              DECIMAL(16,6) ,
        vprec_dia_sb1           DECIMAL(11,6) ,
        vprec_dia_sb3           DECIMAL(11,6) ,     
        tot_acc_sb1             DECIMAL(16,6) ,
        tot_acc_sb3             DECIMAL(16,6) ,
        tot_pesos_sb1           DECIMAL(16,6) ,
        tot_pesos_sb3           DECIMAL(16,6) 

    LET mto_acc_sb1    = 0
    LET mto_acc_sb3    = 0
    LET vprec_dia_sb1  = 0
    LET vprec_dia_sb3  = 0
    LET vmto_acc_sb1   = 0
    LET vmto_acc_sb3   = 0
    LET vmto_pesos_sb1 = 0 
    LET vmto_pesos_sb3 = 0 
    LET tot_acc_sb1    = 0
    LET tot_acc_sb3    = 0

    CALL precio_accion(vfecha_ult_ret,1)
    RETURNING vprec_dia_sb1

    --sveraCALL precio_accion(vfecha_ult_ret,3)
    --sveraRETURNING vprec_dia_sb3

    SELECT SUM(A.monto_en_acciones),
           SUM(A.monto_en_acciones) * vprec_dia_sb1
    INTO   vmto_acc_sb1 ,
           vmto_pesos_sb1
    FROM   dis_cuenta A
    WHERE  A.nss       = vn_seguro
    AND    A.subcuenta IN (3,10)
    AND    A.siefore   = 1

    IF vmto_pesos_sb1 IS NULL
    OR vmto_pesos_sb1 = " " THEN
        LET vmto_pesos_sb1 = 0
    END IF

    IF vmto_acc_sb1 IS NULL
    OR vmto_acc_sb1 = " " THEN
        LET vmto_acc_sb1 = 0
    END IF

    SELECT SUM(A.monto_en_acciones),
           SUM(A.monto_en_acciones) * vprec_dia_sb3
    INTO   vmto_acc_sb3 ,
           vmto_pesos_sb3
    FROM   dis_cuenta A
    WHERE  A.nss       = vn_seguro
    AND    A.subcuenta IN (3,10)
    AND    A.siefore   = 3

    IF vmto_pesos_sb3 IS NULL
    OR vmto_pesos_sb3 = " " THEN
        LET  vmto_pesos_sb3 = 0
    END IF

    IF vmto_acc_sb3 IS NULL
    OR vmto_acc_sb3 = " " THEN
        LET vmto_acc_sb3 = 0
    END IF
    LET M1 = vmto_solic - vmto_pesos_sb1

    IF M1 > 0 THEN
        LET mto_acc_sb1 = vmto_acc_sb1               
        LET mto_acc_sb3 = (M1 * vmto_acc_sb3) / vmto_pesos_sb3
    ELSE
        LET mto_acc_sb1 = (vmto_solic * vmto_acc_sb1 ) / vmto_pesos_sb1
        LET mto_acc_sb3 = 0
    END IF
    LET tot_acc_sb1 = tot_acc_sb1 + mto_acc_sb1
    LET tot_pesos_sb1 = tot_acc_sb1 * vprec_dia_sb1
    LET tot_acc_sb3 = tot_acc_sb3 + mto_acc_sb3
    LET tot_pesos_sb3 = tot_acc_sb3 * vprec_dia_sb3
    RETURN tot_acc_sb1,tot_acc_sb3,tot_pesos_sb1,tot_pesos_sb3
END FUNCTION
