#####################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => EFP                                           #
#Programa          => RETC920                                       #
#Descripcion       => RECIBE ARCHIVO DE TRANSFERENCIA DE RETIROS    #
#                     ISSSTE CONTINGENTE                            #
#Sistema           => RET                                           #
#Fecha creacion    => 08 SEPTIEMBRE 2009                            #
#Por               => STEFANIE DANIELA VERA PIÑA                    #
#####################################################################
DATABASE safre_af
GLOBALS

    DEFINE g_seg_modulo  RECORD   LIKE seg_modulo.*

    DEFINE reg_det_ret_isss03  RECORD
	     consecutivo               DECIMAL(11,0),
        tipo_registro             CHAR(2),
        curp                      CHAR(18),
        cve_afore                 CHAR(3),
        nombre_afore              CHAR(40),
        paterno_afore             CHAR(40),
        materno_afore             CHAR(40),
        nss                       CHAR(11),
        num_concesion             CHAR(9),
        sec_pension               CHAR(2),
        regimen                   CHAR(2),
        tipo_retiro               CHAR(1),
        tipo_seguro               CHAR(2),
        tipo_pension              CHAR(2),
        cve_pension               CHAR(3),
        tipo_prestacion           SMALLINT,
        fecha_ini_pen             DATE,
        mto_solic_issste          DECIMAL(13,2),
        estado_sub_viv            CHAR(1),
        int_viv08_bdnsviv         DECIMAL(16,6),
        fecha_valor_viv           DATE,
        int_viv08_afore           DECIMAL(16,6),
		  id_procesar               CHAR(8)
    END RECORD

    DEFINE reg_det_ret_isss04 RECORD
        tipo_registro             CHAR(2),
        cve_afore                 CHAR(3),
        nss                       CHAR(11),
        curp                      CHAR(18),
        cve_siefore               SMALLINT,
        acc_ret_08                DECIMAL(16,6), 
        acc_cv                    DECIMAL(16,6), 
        acc_ahorro_solid          DECIMAL(16,6)
    END RECORD
 
    DEFINE
	     archivo_retiro            CHAR(200),
	     enter                     CHAR(1),
		  ejecuta                   CHAR(300),
	     generar                   CHAR(20),
	     g_usuario                 CHAR(8)

    DEFINE
	    HOY                        DATE

    DEFINE
	    cuantos                    ,
		 vfolio                     ,
		 vtotal_rech                ,
       vtotal_reg03               ,       
       vtotal_reg04               ,       
       vtotal_reg                 INTEGER 

END GLOBALS


MAIN
    OPTIONS
        INPUT WRAP,
        PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I
        DEFER INTERRUPT

    CALL STARTLOG("RETC920.log")
    CALL inicio()

    OPEN WINDOW v1 AT 3,3 WITH FORM "RETC9201"  ATTRIBUTE( BORDER )
    DISPLAY " RETC9201         TRANSFERENCIA DE RETIRO ISSSTE CONTINGENTE                  " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE (REVERSE)

    MENU "MENU"
        COMMAND "Cargar Archivo" "Carga del Archivo "
            CALL proceso_principal()
        COMMAND "Salir" "Salir del menu"
            EXIT MENU
    END MENU

   CLOSE WINDOW v1
END MAIN


FUNCTION inicio()
#----------------

    LET HOY = TODAY

    SELECT USER
    INTO   g_usuario
    FROM   tab_afore_local

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'ret'

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp

        DROP TABLE tmp_retc920

        CREATE TABLE tmp_retc920
          (n_registros           CHAR(248))

        DATABASE safre_af
    WHENEVER ERROR STOP

END FUNCTION


FUNCTION proceso_principal()
#---------------------------

    INPUT BY NAME generar
        AFTER FIELD generar
            IF generar IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD generar
            END IF

            SELECT "X"
            FROM   ret_cza_lote_issste a
            WHERE  a.nom_archivo = generar

            IF SQLCA.SQLCODE = 0 THEN
                PROMPT "ARCHIVO YA PROCESADO,[Enter] P/SALIR" FOR enter
                EXIT INPUT
            END IF

        ON KEY (ESC)

            IF generar IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD generar
            END IF

            LET archivo_retiro = g_seg_modulo.ruta_rescate CLIPPED,
                                 "/",generar CLIPPED

            LOAD FROM archivo_retiro INSERT INTO safre_tmp:tmp_retc920

            SELECT count(*)
            INTO   cuantos
            FROM   safre_tmp:tmp_retc920

            IF cuantos = 0 THEN
                DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
                AT 19,2 ATTRIBUTE(REVERSE)
                SLEEP 3
                NEXT FIELD generar
            END IF

            CALL crea_tablas()
            CALL validacion_previa()
            CALL lee_archivo_plano()

            SELECT MAX(folio)+1
            INTO vfolio
            FROM glo_folio

            INSERT INTO glo_folio
            VALUES(vfolio)

            CALL valida_detalle()

				SELECT COUNT(*)
				INTO   vtotal_rech
				FROM   safre_tmp:ret_tran_rx_isss03
				WHERE  diag_afore IN (1,2,3)

            IF vtotal_rech = 0 THEN
                CALL sube_archivo()
					  
					 DISPLAY "FOLIO : ",vfolio,"" AT 18,2
					 PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
				ELSE	 
                PROMPT " ARCHIVO CON REGISTROS RECHAZADOS AVISAR A SISTEMAS ...<ENTER> PARA CONTINUAR "
					 FOR enter
            END IF 

            EXIT INPUT

        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA CONTINUAR "
            FOR enter
            EXIT INPUT
    END INPUT

    DISPLAY "                                " AT 10,6
    DISPLAY "                                " AT 12,6
    DISPLAY "                                " AT 14,6
    DISPLAY "               " AT 18,2

END FUNCTION


FUNCTION crea_tablas()
#---------------------

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE ret_tran_rx_isss03
        DROP TABLE ret_tran_rx_isss04
    WHENEVER ERROR STOP

    CREATE TABLE ret_tran_rx_isss03(
	     consecutivo       decimal(11,0),
        tipo_registro     CHAR(2),
        curp              CHAR(18),
        cve_afore         CHAR(3),
        nombre_afore      CHAR(40),
        paterno_afore     CHAR(40),
        materno_afore     CHAR(40),
        nss               CHAR(11),
        num_concesion     CHAR(9),
        sec_pension       CHAR(2),
        regimen           CHAR(2),
        tipo_retiro       CHAR(1),
        tipo_seguro       CHAR(2),
        tipo_pension      CHAR(2),
        cve_pension       CHAR(3),
        tipo_prestacion   SMALLINT,
        fecha_ini_pen     DATE,
        mto_solic_issste  DECIMAL(13,2),
        estado_sub_viv    CHAR(1),
        int_viv08_bdnsviv DECIMAL(16,6),
        cecha_valor_viv   DATE,
        int_viv08_afore   DECIMAL(16,6),
		  id_procesar       CHAR(8),
		  diag_afore        SMALLINT,
        estado_solicitud  SMALLINT
      );

    CREATE TABLE ret_tran_rx_isss04(
        tipo_registro     CHAR(2),
        cve_afore         CHAR(3),
        nss               CHAR(11),
        curp              CHAR(18),
        cve_siefore       SMALLINT,
        acc_ret_08        DECIMAL(16,6),
        acc_cv            DECIMAL(16,6),
        acc_ahorro_solid  DECIMAL(16,6)
       );

     DATABASE safre_af
END FUNCTION


FUNCTION validacion_previa()
#---------------------------

    DEFINE
        c2_tipo_registro      CHAR(2)

    DEFINE
        sw_3                  ,
        sw_4                  SMALLINT

    DECLARE cur_2 CURSOR FOR
    SELECT UNIQUE(n_registros[1,2])
    FROM   safre_tmp:tmp_retc920

    LET sw_3 = 0
    LET sw_4 = 0

    FOREACH cur_2 INTO c2_tipo_registro
        CASE c2_tipo_registro
            WHEN "03"
                LET sw_3 = 1
            WHEN "04"
                LET sw_4 = 1
        END CASE
    END FOREACH

    IF sw_3 = 0 THEN
        PROMPT " SE RECHAZA ARCHIVO NO EXISTE DETALLE 03 " 
        FOR enter
        EXIT PROGRAM
    END IF

    IF sw_4 = 0 THEN
        PROMPT " SE RECHAZA ARCHIVO NO EXISTE DETALLE 04 "
        FOR enter
        EXIT PROGRAM
    END IF

END FUNCTION


FUNCTION lee_archivo_plano()
#---------------------------

    DEFINE
        bnd_proc             ,
        cont                 INTEGER

    DEFINE
        carga_reg            CHAR(248) ,
        c_fecha_ini_pen      CHAR(8)   ,
        c_fecha_valor_viv    CHAR(8)   , 
        c_fini_pen           CHAR(10)  ,
        c_fvalor_viv         CHAR(10)  ,
        c2_ident_operacion   CHAR(2)   ,
        c_paso_fecha_mov     CHAR(10)  ,
        aux_valor_aivs_92    ,
        aux_valor_aivs_08    DECIMAL(30,14)


    DECLARE cur_tmp_ret CURSOR FOR
    SELECT  * FROM safre_tmp:tmp_retc920

    LET cont = 0
    LET c2_ident_operacion = ""

    FOREACH cur_tmp_ret INTO carga_reg
        LET cont = cont + 1

                #---DETALLE 03 DE SOLICITUD DE TRANSFERENCIA---#

        IF carga_reg[1,2] = "03" THEN
            LET reg_det_ret_isss03.tipo_registro          = carga_reg[001,002]
            LET reg_det_ret_isss03.curp                   = carga_reg[003,020]
            LET reg_det_ret_isss03.cve_afore              = carga_reg[021,023]
            LET reg_det_ret_isss03.nombre_afore           = carga_reg[024,063]
            LET reg_det_ret_isss03.paterno_afore          = carga_reg[064,103]
            LET reg_det_ret_isss03.materno_afore          = carga_reg[104,143]
            LET reg_det_ret_isss03.nss                    = carga_reg[144,154]
            LET reg_det_ret_isss03.num_concesion          = carga_reg[155,163] 
            LET reg_det_ret_isss03.sec_pension            = carga_reg[164,165]
            LET reg_det_ret_isss03.regimen                = carga_reg[166,167]
            LET reg_det_ret_isss03.tipo_retiro            = carga_reg[168,168]
            LET reg_det_ret_isss03.tipo_seguro            = carga_reg[169,170]
            LET reg_det_ret_isss03.tipo_pension           = carga_reg[171,172]
            LET reg_det_ret_isss03.cve_pension            = carga_reg[173,175]
            LET reg_det_ret_isss03.tipo_prestacion        = carga_reg[176,177]
            LET c_fecha_ini_pen                           = carga_reg[178,185]
            LET reg_det_ret_isss03.mto_solic_issste       = carga_reg[186,200]
            LET reg_det_ret_isss03.estado_sub_viv         = carga_reg[201,201]
            LET reg_det_ret_isss03.int_viv08_bdnsviv      = carga_reg[202,215] 
            LET c_fecha_valor_viv                         = carga_reg[216,223]
            LET reg_det_ret_isss03.int_viv08_afore        = carga_reg[224,237]
				LET reg_det_ret_isss03.id_procesar            = carga_reg[241,248]

            LET c_fini_pen = c_fecha_ini_pen[5,6],"/",
                             c_fecha_ini_pen[7,8],"/",
                             c_fecha_ini_pen[1,4]
            LET reg_det_ret_isss03.fecha_ini_pen  = c_fini_pen

            LET reg_det_ret_isss03.mto_solic_issste = reg_det_ret_isss03.mto_solic_issste /100 

            LET reg_det_ret_isss03.int_viv08_bdnsviv = reg_det_ret_isss03.int_viv08_bdnsviv / 1000000 

            LET c_fvalor_viv = c_fecha_valor_viv[5,6],"/",
                               c_fecha_valor_viv[7,8],"/",
                               c_fecha_valor_viv[1,4]
            LET reg_det_ret_isss03.fecha_valor_viv = c_fvalor_viv

            LET reg_det_ret_isss03.int_viv08_afore = reg_det_ret_isss03.int_viv08_afore / 1000000

            IF reg_det_ret_isss03.tipo_seguro IS NULL 
				OR reg_det_ret_isss03.tipo_seguro = " " THEN
				 
				    LET reg_det_ret_isss03.tipo_seguro = "NA"
			   END IF

            IF reg_det_ret_isss03.cve_pension IS NULL
				OR reg_det_ret_isss03.cve_pension = " " THEN

				    LET reg_det_ret_isss03.cve_pension = "NA"
				END IF	 

            SELECT MAX(consecutivo)+1
            INTO   reg_det_ret_isss03.consecutivo
            FROM   ret_consecutivo

            INSERT INTO ret_consecutivo VALUES (reg_det_ret_isss03.consecutivo)

            INSERT INTO safre_tmp:ret_tran_rx_isss03 VALUES( reg_det_ret_isss03.*,501,1 ) 

        END IF

                #---DETALLE 04 DE SOLICITUD DE TRANSFERENCIA---#

        IF carga_reg[1,2] = "04" THEN
            LET reg_det_ret_isss04.tipo_registro          = carga_reg[001,002]
            LET reg_det_ret_isss04.cve_afore              = carga_reg[003,005]
				LET reg_det_ret_isss04.nss                    = carga_reg[006,016]
				LET reg_det_ret_isss04.curp                   = carga_reg[017,034]
				LET reg_det_ret_isss04.cve_siefore            = carga_reg[035,036]
				LET reg_det_ret_isss04.acc_ret_08             = carga_reg[037,050]
				LET reg_det_ret_isss04.acc_cv                 = carga_reg[051,064]
            LET reg_det_ret_isss04.acc_ahorro_solid       = carga_reg[065,078] 

            LET reg_det_ret_isss04.acc_ret_08 = reg_det_ret_isss04.acc_ret_08 / 1000000

				LET reg_det_ret_isss04.acc_cv = reg_det_ret_isss04.acc_cv / 1000000

				LET reg_det_ret_isss04.acc_ahorro_solid = reg_det_ret_isss04.acc_ahorro_solid / 1000000

				INSERT INTO safre_tmp:ret_tran_rx_isss04 VALUES( reg_det_ret_isss04.* )

        END IF

    END FOREACH
END FUNCTION


FUNCTION valida_detalle()
#------------------------

    DEFINE
	     vnss                CHAR(11)

    DEFINE
	     vmonto_acciones     ,
        vprecio_dia         DECIMAL(16,6),
		  vsuma_tot_mtos      ,
		  vmonto_fov          ,
		  vtot_mto_fov        ,
        vmonto_pesos        ,
        vtot_mto_pes        DECIMAL(16,2)

    DEFINE
	     vdiag_afore         ,
	     vgrupo              ,
		  vrechazo_cod        ,
        vsiefore            ,
		  vsubcuenta          SMALLINT

    DECLARE cur_1 CURSOR FOR
    SELECT * FROM safre_tmp:ret_tran_rx_isss03

    FOREACH cur_1 INTO reg_det_ret_isss03.*

        #----- VALIDA QUE SE ENCUENTRE REGISTRADO EN LA AFORE -----#

        SELECT n_seguro
        INTO   vnss
        FROM   afi_mae_afiliado
        WHERE  n_unico = reg_det_ret_isss03.curp

        IF STATUS = NOTFOUND THEN

            UPDATE safre_tmp:ret_tran_rx_isss03
            SET    safre_tmp:ret_tran_rx_isss03.diag_afore = 503 #--CUENTA INEXISTENTE
            WHERE  safre_tmp:ret_tran_rx_isss03.curp = reg_det_ret_isss03.curp
				AND    safre_tmp:ret_tran_rx_isss03.consecutivo = reg_det_ret_isss03.consecutivo
        ELSE
		      #----- VALIDA MATRIZ DE DERECHOS -----#

            SELECT grupo
				INTO   vgrupo
				FROM   ret_matriz_derecho_issste
            WHERE  regimen         = reg_det_ret_isss03.regimen
            AND    tipo_retiro     = reg_det_ret_isss03.tipo_retiro
            AND    tipo_seguro     = reg_det_ret_isss03.tipo_seguro
            AND    tipo_pension    = reg_det_ret_isss03.tipo_pension
            AND    cve_pension     = reg_det_ret_isss03.cve_pension
            AND    tipo_prestacion = reg_det_ret_isss03.tipo_prestacion
				GROUP BY 1

            IF STATUS = NOTFOUND THEN

                UPDATE safre_tmp:ret_tran_rx_isss03
                SET    safre_tmp:ret_tran_rx_isss03.diag_afore = 1 #-- COMBINACION NO VALIDA
                WHERE  safre_tmp:ret_tran_rx_isss03.curp = reg_det_ret_isss03.curp
					 AND    safre_tmp:ret_tran_rx_isss03.consecutivo = reg_det_ret_isss03.consecutivo
            ELSE
                #----- VALIDA MONTOS SOLICITADOS -----#

                IF reg_det_ret_isss03.mto_solic_issste  <  0
                OR reg_det_ret_isss03.int_viv08_bdnsviv <  0 THEN

                    UPDATE safre_tmp:ret_tran_rx_isss03
                    SET    safre_tmp:ret_tran_rx_isss03.diag_afore = 2 #--MONTO SOLICITADO MENOR A CERO
                    WHERE  safre_tmp:ret_tran_rx_isss03.curp = reg_det_ret_isss03.curp
						  AND    safre_tmp:ret_tran_rx_isss03.consecutivo = reg_det_ret_isss03.consecutivo
                ELSE
                    #----- VALIDA SALDO CERO -----#

                    LET vmonto_acciones = 0

                    SELECT NVL( SUM( c.monto_en_acciones ), 0 )
                    INTO   vmonto_acciones
                    FROM   dis_cuenta c
                    WHERE  c.nss = vnss
                    AND    c.subcuenta IN (SELECT subcuenta FROM tab_agrupa_subcta
						                         WHERE  grupo = vgrupo)

                    IF vmonto_acciones <= 0 THEN 

						      UPDATE safre_tmp:ret_tran_rx_isss03
						      SET    safre_tmp:ret_tran_rx_isss03.diag_afore = 502 #--CUENTA CON SALDO CERO
						      WHERE  safre_tmp:ret_tran_rx_isss03.curp = reg_det_ret_isss03.curp
								AND    safre_tmp:ret_tran_rx_isss03.consecutivo = reg_det_ret_isss03.consecutivo
						  
                    ELSE
                        #----- VALIDA SUFICIENCIA DE SALDO  -----#

                        LET vmonto_acciones = 0
                        LET vtot_mto_pes    = 0
                        LET vtot_mto_fov    = 0
                        LET vsuma_tot_mtos  = 0

                        DECLARE cur_3 CURSOR FOR 
								SELECT subcuenta,siefore,NVL( SUM( monto_en_acciones ), 0 )
								FROM   dis_cuenta 
                        WHERE  nss = vnss
                        AND    subcuenta IN (SELECT subcuenta FROM tab_agrupa_subcta
                                             WHERE  grupo = vgrupo)
                        GROUP BY 1,2

                        FOREACH cur_3 INTO vsubcuenta,vsiefore,vmonto_acciones

                            SELECT precio_del_dia
                            INTO   vprecio_dia
                            FROM   glo_valor_accion
                            WHERE  fecha_valuacion = HOY
                            AND    codigo_siefore = vsiefore

                            IF STATUS = NOTFOUND THEN
									     ERROR " NO EXISTE EL PRECIO DE ACCION DEL DIA "
										  EXIT  PROGRAM
								    END IF		

                            IF  vsubcuenta <> 35  #--SUBCUENTAS DE FOVISSSTE
									 AND vsubcuenta <> 14 THEN

                                LET vmonto_pesos = vmonto_acciones * vprecio_dia

                                LET vtot_mto_pes = vtot_mto_pes + vmonto_pesos
									 ELSE
									     LET vmonto_fov = vmonto_acciones * vprecio_dia

  										  LET vtot_mto_fov = vtot_mto_fov + vmonto_fov
									 END IF		

                        END FOREACH
 
                        LET vsuma_tot_mtos = vtot_mto_pes + vtot_mto_fov

							   IF reg_det_ret_isss03.mto_solic_issste > 0 THEN
									 IF reg_det_ret_isss03.mto_solic_issste > vsuma_tot_mtos THEN

                                UPDATE safre_tmp:ret_tran_rx_isss03
										  SET    safre_tmp:ret_tran_rx_isss03.diag_afore = 3 #--SALDO INSUFICIENTE
										  WHERE  safre_tmp:ret_tran_rx_isss03.curp = reg_det_ret_isss03.curp
										  AND    safre_tmp:ret_tran_rx_isss03.consecutivo = reg_det_ret_isss03.consecutivo
									 END IF
								END IF	  
                    END IF
                END IF
            END IF
        END IF 

    CALL marcaje(vnss,reg_det_ret_isss03.curp)

	 END FOREACH
END FUNCTION


FUNCTION sube_archivo()
#---------------------

    DEFINE reg_det03 RECORD LIKE safre_tmp:ret_tran_rx_isss03.*

    DEFINE reg_det04 RECORD LIKE safre_tmp:ret_tran_rx_isss04.*

    DEFINE 
        v_grupo      SMALLINT

    SELECT COUNT(*)
    INTO   vtotal_reg03
    FROM   safre_tmp:ret_tran_rx_isss03

    SELECT COUNT(*)
    INTO   vtotal_reg04
    FROM   safre_tmp:ret_tran_rx_isss04

    LET vtotal_reg = vtotal_reg03 + vtotal_reg04

    INSERT INTO ret_cza_lote_issste
	 VALUES(vfolio,generar,HOY,vtotal_reg,1)

    DECLARE cursor_det CURSOR FOR
    SELECT *
    FROM   safre_tmp:ret_tran_rx_isss03

    FOREACH cursor_det INTO reg_det03.*

            SELECT grupo
            INTO   v_grupo
            FROM   ret_matriz_derecho_issste
            WHERE  regimen         = reg_det03.regimen
            AND    tipo_retiro     = reg_det03.tipo_retiro
            AND    tipo_seguro     = reg_det03.tipo_seguro
            AND    tipo_pension    = reg_det03.tipo_pension
            AND    cve_pension     = reg_det03.cve_pension
            AND    tipo_prestacion = reg_det03.tipo_prestacion
            GROUP BY 1

        SELECT *
        INTO   reg_det04.*
        FROM   safre_tmp:ret_tran_rx_isss04
        WHERE  curp = reg_det03.curp

        
        INSERT INTO ret_tran_rx_isss03
		  VALUES(vfolio,reg_det03.*,v_grupo,g_usuario)

        INSERT INTO ret_tran_rx_isss04
		  VALUES(vfolio,reg_det03.consecutivo,reg_det04.*,1,g_usuario)

    END FOREACH   
END FUNCTION


FUNCTION marcaje(v_nss,v_curp)
#-----------------------------

    DEFINE
	     v_curp             CHAR(18),
        v_nss              CHAR(11)

    DEFINE
        v_consecutivo      DECIMAL(10,0)

    DEFINE
	     pmarca_causa       ,
		  vcodigo_rechazo    ,
		  vmarca_estado      ,
        v_movimiento       ,
		  xcodigo_marca      ,
		  xcodigo_rechazo    SMALLINT

    DECLARE cur_marcaje CURSOR FOR
    SELECT a.consecutivo ,
           b.movimiento
    FROM   safre_tmp:ret_tran_rx_isss03 a,tab_ret_issste b
    WHERE  a.curp = v_curp
    AND    a.diag_afore = 501
    AND    a.tipo_retiro = b.tipo_retiro

    FOREACH cur_marcaje INTO  v_consecutivo ,
                              v_movimiento

        SELECT "X"
        FROM   cta_act_marca
        WHERE  nss = v_nss
        AND    marca_cod = v_movimiento

        IF SQLCA.SQLCODE <> 0 THEN
            LET vmarca_estado   = 0
            LET vcodigo_rechazo = 0
				LET pmarca_causa    = 0

            LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
                          "'",v_nss,"'",
                          ",",v_movimiento,
                          ",",v_consecutivo,
                          ",",vmarca_estado,
                          ",",vcodigo_rechazo,
                          ",",pmarca_causa,
                          ",","'","'",",",
                          "'",g_usuario,"'",")"

            LET ejecuta = ejecuta CLIPPED

            PREPARE clausula_spl FROM ejecuta
            DECLARE cursor_marca CURSOR FOR clausula_spl

            OPEN cursor_marca

            FETCH cursor_marca INTO xcodigo_marca, xcodigo_rechazo

            IF xcodigo_rechazo = 590 THEN

                UPDATE safre_tmp:ret_tran_rx_isss03
                SET    safre_tmp:ret_tran_rx_isss03.diag_afore = 504 #--CUENTA EN LAUDO EN LA AFORE
                WHERE  safre_tmp:ret_tran_rx_isss03.curp = v_curp
                AND    safre_tmp:ret_tran_rx_isss03.consecutivo = v_consecutivo
            ELSE
				    IF xcodigo_rechazo <> 0 THEN

                    UPDATE safre_tmp:ret_tran_rx_isss03
                    SET    safre_tmp:ret_tran_rx_isss03.diag_afore = 506 #--CUENTA EN PROCESO OPERATIVO EN LA AFORE
                    WHERE  safre_tmp:ret_tran_rx_isss03.curp = v_curp
					     AND    safre_tmp:ret_tran_rx_isss03.consecutivo = v_consecutivo
					 END IF	  
            END IF

            CLOSE cursor_marca

        END IF
    END FOREACH
END FUNCTION
