#********************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Propietario       => EFP                                            #
#Programa          => VOLB006                                        #
#Descripcion       => OBTENCION DE INFORMACION DE AFILIADOS A LOS QUE#
#                     SE LES PAGARON INTERESES DURANTE EL EJERCICIO  #
#     				    DEL 2007                                       #
#Fecha             => 25-ENERO-2008                                  #
#Por               => STEFANIE DANIELA VERA PIÑA                     #
#********************************************************************#
DATABASE safre_af
GLOBALS
	DEFINE
		vnss             	CHAR(11),
		vmonto_en_pesos  	LIKE dis_cuenta.monto_en_pesos,
		vmonto_ret_pesos 	LIKE dis_cuenta.monto_en_pesos,
		vmonto_en_acc    	LIKE dis_cuenta.monto_en_acciones,
		vfecha           	DATE,
		vprecio_acc      	DECIMAL(16,6),
		vint_real_pesos  	DECIMAL(16,6),
		vint_nom_pesos   	DECIMAL(16,6),
		vsaldo_pesos     	DECIMAL(16,6),
		enter            	CHAR(01),
		vmax_factualiza  	DATE,
		vmax_rowid       	INTEGER
	DEFINE reg_1 RECORD     #glo #reg_1
		paterno     		LIKE afi_mae_afiliado.paterno,
		materno     		LIKE afi_mae_afiliado.materno,
		nombres     		LIKE afi_mae_afiliado.nombres,
		n_rfc       		LIKE afi_mae_afiliado.n_rfc,
		n_unico     		LIKE afi_mae_afiliado.n_unico,
		calle       		LIKE afi_domicilio.calle,
		numero      		LIKE afi_domicilio.numero,
		depto       		LIKE afi_domicilio.depto,
		colonia     		LIKE afi_domicilio.colonia,
		deleg_desc  		LIKE tab_delegacion.deleg_desc,
		ciudad_desc 		LIKE tab_ciudad.ciudad_desc,
		estad_desc  		LIKE tab_estado.estad_desc,
		codpos      		LIKE afi_domicilio.codpos
	END RECORD
END GLOBALS

MAIN
	DEFINE	
		enter		CHAR( 1 ),
		fecproc	DATE,
		nomarch	CHAR( 100 ),
		ruta		LIKE seg_modulo.ruta_envio,
		archivo	CHAR( 20 ),
		feccad	CHAR( 10 )

	DEFER INTERRUPT
	OPTIONS INPUT WRAP, PROMPT LINE LAST, ACCEPT KEY CONTROL-I
	LET fecproc = TODAY

	OPEN WINDOW vol_sat_1 AT 4, 4 WITH FORM "VOLB0061" ATTRIBUTE( BORDER ) 
	DISPLAY " VOLB006   REPORTE DE AFILIADOS QUE PAGARON INTERESES EN 2007  ",
		fecproc USING "DD-MM-YYYY " AT 3,1 ATTRIBUTE( REVERSE )
	DISPLAY BY NAME fecproc

	WHILE TRUE
		PROMPT " ESTA SEGURO DE GENERAR EL REPORTE S/N ? " FOR CHAR enter
		IF enter MATCHES "[sSnN]" THEN
			IF enter MATCHES "[sS]" THEN
				EXIT WHILE
			ELSE
				PROMPT" PROCESO CANCELADO...<ENTER> PARA CONTINUAR "
					FOR CHAR enter
				CLOSE WINDOW vol_sat_1
				EXIT PROGRAM
			END IF
		END IF
	END WHILE

	SELECT ruta_envio INTO ruta FROM seg_modulo WHERE modulo_cod = 'vol'
	LET feccad  = YEAR( fecproc ) USING "&&&&", MONTH( fecproc ) USING "&&",
		DAY( fecproc ) USING "&&"
	LET archivo = "VOLSAT.", feccad
	LET nomarch = ruta CLIPPED, "/", archivo CLIPPED

	CALL CreaTablaTMP()
	DISPLAY " PROCESANDO INFORMACION..." AT 19, 1 ATTRIBUTE( REVERSE )

	DECLARE cur1 CURSOR FOR
		SELECT UNIQUE( nss ) FROM dis_cuenta
			WHERE tipo_movimiento = 490 
				AND fecha_conversion BETWEEN "01012007" AND "12312007"
	FOREACH cur1 INTO vnss
		SELECT MAX( factualiza ) INTO vmax_factualiza
			FROM afi_domicilio
			WHERE nss = vnss

		SELECT MAX( rowid ) INTO vmax_rowid FROM afi_domicilio
			WHERE nss = vnss AND factualiza = vmax_factualiza

		SELECT A.paterno, A.materno, A.nombres,
			A.n_rfc, A.n_unico,
			B.calle, B.numero, B.depto, B.colonia,
			C.deleg_desc, D.ciudad_desc, E.estad_desc, B.codpos 
			INTO reg_1.paterno, reg_1.materno, reg_1.nombres,
				reg_1.n_rfc, reg_1.n_unico,
				reg_1.calle, reg_1.numero, reg_1.depto, reg_1.colonia,
				reg_1.deleg_desc, reg_1.ciudad_desc, reg_1.estad_desc, reg_1.codpos
			FROM afi_mae_afiliado A, afi_domicilio B, tab_delegacion C, 
				tab_ciudad D, tab_estado E
			WHERE A.n_seguro = vnss       AND A.n_seguro = B.nss
				AND B.marca_envio = "X"    AND B.rowid  = vmax_rowid
				AND B.delega = C.deleg_cod AND B.ciudad = D.ciudad_cod
				AND B.estado = E.estad_cod

		LET vmonto_en_pesos   = 0
		LET vmonto_ret_pesos  = 0
		DECLARE cur2 CURSOR FOR 
			SELECT monto_en_pesos * -1 FROM dis_cuenta
				WHERE nss = vnss AND tipo_movimiento = 10 
					AND fecha_conversion BETWEEN "01012007" AND "12312007"	 
		FOREACH cur2 INTO vmonto_en_pesos
			LET vmonto_ret_pesos = vmonto_ret_pesos + vmonto_en_pesos
		END FOREACH 	  
		LET vfecha = "12312007"
		CALL precio_accion( vfecha ) RETURNING vprecio_acc
		LET vmonto_en_acc = 0
		LET vsaldo_pesos  = 0

		SELECT SUM( monto_en_acciones ) INTO vmonto_en_acc
			FROM dis_cuenta
			WHERE nss = vnss AND subcuenta IN ( 3, 10 )
				AND fecha_conversion <= "12312007"

		LET vsaldo_pesos = vmonto_en_acc * vprecio_acc
		LET vint_real_pesos = 0
		LET vint_nom_pesos  = 0

		CALL ren_fiscal( vnss ) RETURNING vint_real_pesos, vint_nom_pesos

		INSERT INTO safre_tmp:vol_sat
			VALUES( vnss,
				reg_1.paterno, reg_1.materno, reg_1.nombres,     
				reg_1.n_rfc, reg_1.n_unico,     
				reg_1.calle, reg_1.numero, reg_1.depto, reg_1.colonia,     
				reg_1.deleg_desc, reg_1.ciudad_desc, reg_1.estad_desc, 
				reg_1.codpos,
				vint_nom_pesos, vint_real_pesos, vmonto_ret_pesos, 
				vsaldo_pesos )
	END FOREACH 

	LET nomarch = nomarch CLIPPED
	UNLOAD TO nomarch SELECT * FROM safre_tmp:vol_sat;

	DROP TABLE safre_tmp:vol_sat 

	DISPLAY "RUTA: ", ruta AT 10, 15 ATTRIBUTE( BOLD )
	DISPLAY "ARCHIVO GENERADO: ", archivo AT 12, 3 ATTRIBUTE( BOLD )
	PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter
	CLOSE WINDOW vol_sat_1
END MAIN

FUNCTION CreaTablaTMP()
	CREATE TEMP TABLE vol_sat (
		 nss 				CHAR(11),
		 paterno 		CHAR(40),
		 materno 		CHAR(40),
		 nombres 		CHAR(40),
		 n_rfc 			CHAR(13),
		 n_unico 		CHAR(18),
		 calle 			CHAR(40),
		 numero 			CHAR(10),
		 depto 			CHAR(10),
		 colonia	 		CHAR(60),
		 deleg_desc 	CHAR(40),
		 ciudad_desc 	CHAR(40),
		 estad_desc 	CHAR(40),
		 codpos 			CHAR(5),
		 int_nom 		DECIMAL( 16,6 ),
		 int_real 		DECIMAL( 16,6 ),
		 monto_ret 		DECIMAL( 16,6 ),
		 saldo 			DECIMAL( 16,6 ) 
		 ) IN tmp_dbs1;
END FUNCTION

FUNCTION ren_fiscal(x_nss)
#-------------------------
   DEFINE x_nss           	CHAR(11)
   DEFINE vfecha_fiscal   	DATE
   DEFINE sel_9           	CHAR(400)
   DEFINE gr_prom RECORD
      fecha_apo        		DATE,
      fecha_liq        		DATE,
      mto_neto         		DECIMAL(16,6),
      mto_retencion    		DECIMAL(16,6),
      mto_rendimiento  		DECIMAL(16,6)
   END RECORD
   DEFINE
      i_precio_fin         DECIMAL(16,6),
      i_precio_ini         DECIMAL(16,6),
      v_precio_fin         DECIMAL(16,6),
      v_precio_ini         DECIMAL(16,6),
      vtasa_tot            DECIMAL(16,6),
      vtasa_prom           DECIMAL(16,6),
      vdia_prom            INTEGER,
      vdias_prom           INTEGER,
      vconta_prom          INTEGER,
      vacum_tasa           DECIMAL(16,6),
      acum_vdia_prom       INTEGER,
      vrendimiento_nominal DECIMAL(16,6),
      factor_infla         DECIMAL(16,6),
      mac                  DECIMAL(16,6),
      infla_per            DECIMAL(16,6),
      ren_real             DECIMAL(16,6),
      ren_real_tot         DECIMAL(16,6),
      coe_ord              DECIMAL(16,6),
      tasa_real            DECIMAL(16,6),
      tasa_real_tot        DECIMAL(16,6),
      vtasa_rprom          DECIMAL(16,6),
      vret_tot             DECIMAL(16,6),
      vnet_tot             DECIMAL(16,6),
      total_accion         DECIMAL(16,6),
      importe_ini          DECIMAL(16,6),
      importe_fin          DECIMAL(16,6),
      vdatei               DECIMAL(16,6),
      vdatef               DECIMAL(16,6),
      vmf                  INTEGER,
      vmi                  INTEGER

	LET sel_9 = " SELECT a.fecha_aporte, a.fecha_liquidacion, ",
		"a.mto_neto * -1, a.mto_retencion * -1, a.mto_rendimiento ",
		"FROM ret_pago_vol a ",
		"WHERE a.nss = ? ",
		"AND a.fecha_liquidacion BETWEEN '01012007' AND '12312007' "
   PREPARE eje_sel_9 FROM sel_9

   LET vtasa_tot      = 0
   LET vdias_prom     = 0
   LET vconta_prom    = 0
   LET vacum_tasa     = 0
   LET acum_vdia_prom = 0
   LET ren_real_tot   = 0
   LET tasa_real_tot  = 0
   LET vrendimiento_nominal = 0

   DECLARE cur_col CURSOR FOR eje_sel_9
   FOREACH cur_col USING x_nss INTO gr_prom.*
		LET vdatei       = ""
		LET vdatef       = ""
		LET i_precio_fin = 0
		LET i_precio_ini = 0
		LET v_precio_fin = 0
		LET v_precio_ini = 0
	
      CALL precio_accion( gr_prom.fecha_apo ) RETURNING v_precio_ini
      CALL precio_accion( gr_prom.fecha_liq ) RETURNING v_precio_fin

{sveraIF gr_prom.fecha_apo < "01/14/2005" THEN
         LET v_precio_ini = precio_siefore ( 2, gr_prom.fecha_apo )
      ELSE
         LET v_precio_ini = precio_siefore ( 1, gr_prom.fecha_apo )
      END IF
      LET v_precio_fin = precio_siefore ( 1, gr_prom.fecha_liq )
}
      SELECT valor_udi INTO i_precio_ini FROM tab_udi
			WHERE fecha_udi = gr_prom.fecha_apo

      SELECT valor_udi INTO i_precio_fin FROM tab_udi
			WHERE fecha_udi = gr_prom.fecha_liq

      LET factor_infla = 0
      LET infla_per    = 0
      LET mac          = 0
      LET ren_real     = 0
      LET total_accion = 0
      LET importe_ini  = 0
      LET importe_fin  = 0

      LET total_accion = ( gr_prom.mto_neto + gr_prom.mto_retencion ) /
			v_precio_fin
      LET importe_ini = total_accion * v_precio_ini
      LET importe_fin = total_accion * v_precio_fin
      LET gr_prom.mto_rendimiento = 0
      LET gr_prom.mto_rendimiento = importe_fin - importe_ini
      LET factor_infla = ( i_precio_fin / i_precio_ini ) - 1
      LET mac = ( gr_prom.mto_neto + gr_prom.mto_retencion ) -
			gr_prom.mto_rendimiento
      LET infla_per = factor_infla * mac
      LET ren_real = gr_prom.mto_rendimiento - infla_per
      LET ren_real_tot = ren_real_tot + ren_real
      LET vrendimiento_nominal = vrendimiento_nominal + gr_prom.mto_rendimiento
      INITIALIZE gr_prom.* TO NULL
   END FOREACH
   RETURN ren_real_tot, vrendimiento_nominal
END FUNCTION

FUNCTION precio_accion(f_fecha_valuacion)
#pa--------------------------------------
	DEFINE   #loc #decimal
		d6_precio_del_dia     DECIMAL(16,6)
	DEFINE   #loc #date
		f_fecha_valuacion     DATE

    IF f_fecha_valuacion >= "01142005" THEN
        SELECT precio_del_dia INTO d6_precio_del_dia
			  FROM glo_valor_accion
			  WHERE fecha_valuacion = f_fecha_valuacion
				  AND codigo_siefore = 1
        IF STATUS = NOTFOUND THEN
             PROMPT " NO EXISTE PRECIO DE ACCION DE: ", f_fecha_valuacion,
				 	" SIEFORE 1" FOR CHAR enter
             EXIT PROGRAM
        END IF
    ELSE
        SELECT precio_del_dia INTO d6_precio_del_dia
			  FROM glo_valor_accion
			  WHERE fecha_valuacion = f_fecha_valuacion
				  AND codigo_siefore = 2
        IF STATUS = NOTFOUND THEN
             PROMPT " NO EXISTE PRECIO DE ACCION DE: ", f_fecha_valuacion,
					 " SIEFORE 2" FOR CHAR enter
             EXIT PROGRAM
        END IF
    END IF
    RETURN d6_precio_del_dia
END FUNCTION
