#############################################################################
#Proyecto          => AFORES ( MEXICO )                                     #
#Propietario       => E.F.P.                                                #
#Programa AFIC007  => CARGA DE ARCHIVO MODIFICACION NSS IMSS                #
#Sistema           => AFI.                                                  #
#Autor             => MAURO MUNIZ CABALLERO                                 #
#Fecha             => 3 DE JULIO DE 2002                                    #
#Autor             => MAURO MUNIZ CABALLERO (Proceso batch)                 #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg_det_not RECORD
        tipo_registro     CHAR(2),
        cve_operacion     CHAR(2),
        n_seguro          CHAR(11),
        n_rfc             CHAR(13),
        n_unico           CHAR(18),
        paterno           CHAR(40),
        materno           CHAR(40),
        nombres           CHAR(40),
        fena              DATE,
        sexo              SMALLINT,
        estadon           SMALLINT,
        status_cta        SMALLINT,
        nss_proc          CHAR(11),
        n_rfc_proc        CHAR(13),
        n_unico_proc      CHAR(18),
        paterno_proc      CHAR(40),
        materno_proc      CHAR(40),
        nombres_proc      CHAR(40),
        fena_proc         DATE,
        fecha_recepcion   DATE,
        sexo_proc         SMALLINT,
        estadon_proc      SMALLINT
    END RECORD

    DEFINE g_paramgrales RECORD LIKE seg_modulo.*
    DEFINE g_param_int   RECORD LIKE seg_modulo.*

    DEFINE
        vresp               CHAR(1),
        aux_pausa           CHAR(1),
        enter               CHAR(1),
        vcod_operacion      CHAR(2),
        vdiag_proceso       CHAR(8),
        g_usuario           CHAR(8),
        vf_rechazo          CHAR(8),
        vfec_emision_certif CHAR(10),
        vn_seguro           CHAR(11),
        varchivo            CHAR(14),
        vn_folio            CHAR(18),
        vcurp               CHAR(18),
        vcurp_solicitud     CHAR(18),
        vvcurp              CHAR(18),
        generar             CHAR(20),
        vdesc_cod_39        CHAR(30),
        carga               CHAR(50),
        ejecuta             CHAR(300),
        v_desmarca	    CHAR(300),
        corr                CHAR(100),
        comma               CHAR(100)

    DEFINE
        vhoy ,
        HOY  DATE

    DEFINE
        xcodigo_marca   ,
        xcodigo_rechazo ,
        edo_proc        ,
        vmarca_estado   ,
        vcodigo_rechazo ,
        contar_det      ,
        bnd_proceso     SMALLINT

    DEFINE
        vcodigo_39       ,
        total_reg        ,
        g_plano_notifica ,
        vtotal           ,
        vaprobados       ,
        vrechazados      ,
        vpendientes      INTEGER

    DEFINE reg_bat RECORD
        pid            INTEGER,
        proceso_cod    INTEGER,
        opera_cod      INTEGER,
        nombre_archivo CHAR(25)
    END RECORD

    DEFINE
	vnombres       CHAR(40),
	vpaterno       CHAR(40),
	vmaterno       CHAR(40),
	vnom_afore     CHAR(122),
	vnom_proce     CHAR(122),
	ultimo	       INTEGER,
	vmarca	       DECIMAL

    DEFINE
	ff_tranf_lote  DATE,
	fn_seguro      CHAR(11),
        fnombres       CHAR(40),
	fpaterno       CHAR(40),
	fmaterno       CHAR(40),
	fnomcomp       CHAR(80),
        G_LISTA2       CHAR(200),
	nom_com_afo    CHAR(80),
	nom_com_bdn    CHAR(80),
        fhora          CHAR(8), 
        f1_hora        CHAR(6) 

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG('AFIC007.log')
    CALL inicio() #i

    IF NOT bnd_proceso THEN
        DEFER INTERRUPT
        OPTIONS INPUT WRAP,
            PROMPT LINE LAST,
            ACCEPT KEY CONTROL-I

        CALL proceso_principal()   #pp
    ELSE
        CALL rescata_valores()     #rv
        CALL actualiza_operacion() #rv
    END IF

END MAIN

FUNCTION inicio()
#i---------------

    LET reg_bat.pid         = ARG_VAL(1)
    LET reg_bat.proceso_cod = ARG_VAL(2)
    LET reg_bat.opera_cod   = ARG_VAL(3)

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
    ELSE
        LET bnd_proceso = 0
    END IF 

    LET vtotal          = 0
    LET edo_proc        = 610
    LET vmarca_estado   = 0
    LET vcodigo_rechazo = 0
    LET HOY             = TODAY
    LET vmarca		= 0
    LET vnom_afore      = NULL
    LET vnom_proce      = NULL

    SELECT *, USER
    INTO   g_paramgrales.* , g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    SELECT *
    INTO   g_param_int.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'int'

    CREATE TEMP TABLE plano_notifica
        (n_registros CHAR(400)) WITH NO LOG

    WHENEVER ERROR CONTINUE
    DATABASE safre_tmp
    DROP TABLE tmp_det_notifica

    CREATE TABLE tmp_det_notifica
        (tipo_registro   CHAR(2),
         cve_operacion   CHAR(2),
         n_seguro        CHAR(11),
         n_rfc           CHAR(13),
         n_unico         CHAR(18),
         paterno         CHAR(40),
         materno         CHAR(40),
         nombres         CHAR(40),
         fena            DATE,
         sexo            SMALLINT,
         estadon         SMALLINT,
         status_cta      SMALLINT,
         nss_proc        CHAR(11),
         n_rfc_proc      CHAR(13),
         n_unico_proc    CHAR(18),
         paterno_proc    CHAR(40),
         materno_proc    CHAR(40),
         nombres_proc    CHAR(40),
         fena_proc       DATE,
         fecha_recepcion DATE,
         sexo_proc       SMALLINT,
         estadon_proc    SMALLINT,
         carta           SMALLINT,
         f_transf_lote   DATE,
         consec_dia      INTEGER,
         estado          SMALLINT,
         factualiza      DATE,
         usuario         CHAR(8)
        )

    DATABASE safre_af
    WHENEVER ERROR STOP

    LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIC0071" ATTRIBUTE(BORDER)
    DISPLAY " AFIC007      CARGA ARCHIVO NOTIF. CAMBIO NOMBRE IMSS                          " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           < Ctrl-C > Salir                                        " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

    DISPLAY g_paramgrales.ruta_rescate AT 6,10

    INPUT BY NAME generar

    AFTER FIELD generar
        IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        SELECT nombre_archivo
        INTO   varchivo
        FROM   afi_ctr_arh_notif
        WHERE  nombre_archivo = generar

        IF STATUS <> NOTFOUND THEN
            ERROR "ARCHIVO YA PROCESADO"
            SLEEP 1
            ERROR " "
            INITIALIZE generar TO NULL
            CLEAR FORM
            NEXT FIELD generar
        END IF

        LET carga = NULL
        LET carga = g_paramgrales.ruta_rescate CLIPPED,"/",
                    generar CLIPPED

        WHENEVER ERROR CONTINUE
            LOAD FROM carga DELIMITER ","
            INSERT INTO plano_notifica
        WHENEVER ERROR STOP

        SELECT COUNT(*) 
        INTO   g_plano_notifica
        FROM   plano_notifica

        IF g_plano_notifica IS NULL OR
           g_plano_notifica = 0 THEN
            ERROR "NOMBRE DE ARCHIVO INCORRECTO O VACIO, VERIFIQUE " 
            SLEEP 3
            EXIT PROGRAM
        END IF

        ERROR "Procesando Informacion"

        CALL rescata_valores()

        {PROMPT "Presione [Enter] para generar las cartas respectivas"
        FOR enter

        LET comma = "fglgo ",g_param_int.ruta_exp CLIPPED,
                    "/INTB0115 S" CLIPPED

        RUN comma}

        EXIT PROGRAM

    ON KEY (control-b)
        CALL despliega_archivos()

    END INPUT

END FUNCTION

FUNCTION rescata_valores()
#rd-----------------------

    CALL actualiza_datos()    #ad
    CALL lista_err()          #le

END FUNCTION

FUNCTION actualiza_datos()
#-------------------------

    DEFINE 
        marca_reg  ,
        cont_marca SMALLINT

    DEFINE 
        cont_reg  INTEGER
 
    DEFINE 
        c_fecha1  CHAR(8),
        c_fecha2  CHAR(8),
        c_fecha3  CHAR(8),
        c_fecha10 CHAR(10),
        c_fecha20 CHAR(10),
        c_fecha30 CHAR(10),
        c_fecha_c CHAR(10),
        carga_reg CHAR(360)

    DEFINE   
        campo_011  CHAR(2),
        campo_012  CHAR(2),
        campo_013  CHAR(2),
        campo_014  CHAR(2),
        campo_015  CHAR(3),
        campo_016  CHAR(2),
        campo_017  CHAR(3),
        campo_018  CHAR(3),
        campo_019  CHAR(8),
        campo_110  CHAR(3),

        campo_01   CHAR(2),
        campo_02   CHAR(2),
        campo_03   CHAR(11),
        campo_04   CHAR(13),
        campo_05   CHAR(18),
        campo_06   CHAR(40),
        campo_07   CHAR(40),
        campo_08   CHAR(40),
        campo_09   DATE,
        campo_10   SMALLINT,
        campo_11   SMALLINT,
        campo_12   SMALLINT,
        campo_13   CHAR(11),
        campo_14   CHAR(13),
        campo_15   CHAR(18),
        campo_16   CHAR(40),
        campo_17   CHAR(40),
        campo_18   CHAR(40),
        campo_19   DATE,
        campo_20   DATE,
        campo_21   SMALLINT,
        campo_22   SMALLINT,

        campo_201  CHAR(02),
        campo_202  CHAR(02),
        campo_203  CHAR(03),
        campo_204  CHAR(02),
        campo_205  CHAR(03),
        campo_206  CHAR(02),
        campo_207  CHAR(02),
        campo_208  CHAR(08),
        campo_209  CHAR(03),
        campo_210  CHAR(09),
        campo_211  CHAR(09)

    LET cont_reg   = 0
    LET cont_marca = 0

    SELECT COUNT(*)
    INTO   total_reg
    FROM   plano_notifica

    DECLARE cursor_1 CURSOR FOR 
    SELECT  * 
    FROM    plano_notifica

    FOREACH cursor_1 INTO carga_reg
        LET cont_reg = cont_reg + 1
        IF cont_reg = 1 THEN
            LET campo_011 = carga_reg[001,002]
            LET campo_012 = carga_reg[003,004]
            LET campo_013 = carga_reg[005,006]
            LET campo_014 = carga_reg[007,008]
            LET campo_015 = carga_reg[009,011]
            LET campo_016 = carga_reg[012,013]
            LET campo_017 = carga_reg[014,016]
            LET campo_018 = carga_reg[017,019]
            LET campo_019 = carga_reg[020,027]
            LET campo_110 = carga_reg[028,030]

            IF campo_011 <> "01" THEN
                IF bnd_proceso THEN
            DISPLAY "Program stopped, Tipo registro debe ser 01 en encabezado"
                    EXIT PROGRAM
                ELSE
                    CLEAR SCREEN
                    DISPLAY "Tipo de registro debe ser 01 en ENCABEZADO" AT 10,1
                    PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR " FOR aux_pausa
                    EXIT PROGRAM
                END IF
            END IF

            IF campo_012 <> "01" THEN
                IF bnd_proceso THEN
DISPLAY "Program stopped, Identificador de servicio debe ser 01 en encabezado"
                    EXIT PROGRAM
                ELSE
                    CLEAR SCREEN
DISPLAY "Identificador de Servicio debe ser 01 en ENCABEZADO" AT 10,1
                    PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR " FOR aux_pausa
                    EXIT PROGRAM
                END IF
            END IF

            IF campo_013 <> "72" THEN
                IF bnd_proceso THEN
DISPLAY "Program stopped, Identificador de operacion debe ser 72 en encabezado"
                    EXIT PROGRAM
                ELSE
                    CLEAR SCREEN
DISPLAY "Identificador de Operacion debe ser 72 en ENCABEZADO" AT 10,1
                    PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR " FOR aux_pausa
                    EXIT PROGRAM
                END IF
            END IF

            IF campo_014 <> "03" THEN
                IF bnd_proceso THEN
DISPLAY "Program stopped, Codigo empresa operadora debe ser 03 en encabezado"
                    EXIT PROGRAM
                ELSE
                    CLEAR SCREEN
DISPLAY "Codigo de Empresa Operadora debe ser 03 en ENCABEZADO" AT 10,1
                    PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR " FOR aux_pausa
                    EXIT PROGRAM
                END IF
            END IF

            IF campo_015 <> "001" THEN
                IF bnd_proceso THEN
DISPLAY "Program stopped, Clave entidad origen debe ser 001 en encabezado"
                    EXIT PROGRAM
                ELSE
                    CLEAR SCREEN
DISPLAY "Clave Entidad Origen debe ser 001 en ENCABEZADO" AT 10,1
                    PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR " FOR aux_pausa
                    EXIT PROGRAM
                END IF
            END IF

            LET c_fecha_c = campo_019[5,6],"/",
                            campo_019[7,8],"/",
                            campo_019[1,4]

            SELECT 'X'
            FROM   afi_cza_notifica acn
            WHERE  acn.f_transf_lote = c_fecha_c
            AND    acn.consec_dia = campo_110
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
                INSERT INTO afi_cza_notifica
                VALUES (campo_011,
                        campo_012,
                        campo_013,
                        campo_014,
                        campo_015,
                        campo_016,
                        campo_017,
                        campo_018,
                        c_fecha_c,
                        campo_110)
            END IF
        END IF

        IF cont_reg <> total_reg  AND  cont_reg <> 1 THEN
            LET campo_01 = carga_reg[001,002]
            LET campo_02 = carga_reg[003,004]
            LET campo_03 = carga_reg[005,015]
            LET campo_04 = carga_reg[016,028]
            LET campo_05 = carga_reg[029,046]
            LET campo_06 = carga_reg[047,086]
            LET campo_07 = carga_reg[087,126]
            LET campo_08 = carga_reg[127,166]
            LET c_fecha1 = carga_reg[167,174]
            LET campo_10 = carga_reg[175,175]
            LET campo_11 = carga_reg[176,177]
            LET campo_12 = carga_reg[178,178]
            LET campo_13 = carga_reg[179,189]
            LET campo_14 = carga_reg[190,202]
            LET campo_15 = carga_reg[203,220]
            LET campo_16 = carga_reg[221,260]
            LET campo_17 = carga_reg[261,300]
            LET campo_18 = carga_reg[301,340]
            LET c_fecha2 = carga_reg[341,348]
            LET c_fecha3 = carga_reg[349,356]
            LET campo_21 = carga_reg[357,357]
            LET campo_22 = carga_reg[358,359]

            LET c_fecha10 = c_fecha1[5,6],"/",
                            c_fecha1[7,8],"/",
                            c_fecha1[1,4]

            LET c_fecha20 = c_fecha2[5,6],"/",
                            c_fecha2[7,8],"/",
                            c_fecha2[1,4]

            LET c_fecha30 = c_fecha3[5,6],"/",
                            c_fecha3[7,8],"/",
                            c_fecha3[1,4]

            LET campo_09 = c_fecha10
            LET campo_19 = c_fecha20
            LET campo_20 = c_fecha30

            LET vn_seguro = campo_03

            LET campo_16 = campo_16 CLIPPED
            LET campo_17 = campo_17 CLIPPED
            LET campo_18 = campo_18 CLIPPED

            IF campo_12 <> 2 THEN
	          SELECT 'X' 
                    FROM  cta_ctr_cuenta
                   WHERE  nss = campo_03

                  IF SQLCA.SQLCODE = 0 THEN
                     SELECT m.paterno,
                            m.materno,
                            m.nombres 
                       INTO reg_det_not.paterno_proc,
                            reg_det_not.materno_proc,
                            reg_det_not.nombres_proc
                       FROM afi_mae_afiliado m
                      WHERE  m.n_seguro = campo_03

                     LET reg_det_not.paterno_proc = 
	 	 	 reg_det_not.paterno_proc CLIPPED
                     LET reg_det_not.materno_proc = 
			 reg_det_not.materno_proc CLIPPED
                     LET reg_det_not.nombres_proc = 
			 reg_det_not.nombres_proc CLIPPED

                     LET vnom_afore = reg_det_not.paterno_proc CLIPPED," ",
                                      reg_det_not.materno_proc CLIPPED," ",
                                      reg_det_not.nombres_proc CLIPPED

                     LET vnom_proce = campo_16 CLIPPED," ",
                                      campo_17 CLIPPED," ",
                                      campo_18 CLIPPED

		     {
                     IF reg_det_not.paterno_proc <> campo_16 OR
                        reg_det_not.materno_proc <> campo_17 OR
                        reg_det_not.nombres_proc <> campo_18 THEN
                     }

                     IF vnom_afore <> vnom_proce THEN
		        LET marca_reg = 0
                     ELSE
  		        LET marca_reg = 9

                        INSERT INTO safre_tmp:tmp_det_notifica
                        VALUES (campo_01,
                                campo_02,
                                campo_03,
                                campo_04,
                                campo_05,
                                campo_06,
                                campo_07,
                                campo_08,
                                campo_09,
                                campo_10,
                                campo_11,
                                campo_12,
                                campo_13,
                                campo_14,
                                campo_15,
                                campo_16,
                                campo_17,
                                campo_18,
                                campo_19,
                                campo_20,
                                campo_21,
                                campo_22,
                                0,
	                        c_fecha_c,
                                campo_110,
                                marca_reg,
                                HOY,
                                g_usuario
		     	        )

                        ### Modif ###
                        LET fhora = TIME
                        LET f1_hora = fhora[1,2],fhora[4,5],fhora[7,8]
                        LET G_LISTA2 = g_paramgrales.ruta_listados CLIPPED,"/",
                                       g_usuario CLIPPED,".NOT_AFI_BDNSAR.",
                                       HOY USING "ddmmyy","_",f1_hora CLIPPED
			START REPORT dif_nombs TO G_LISTA2 
			LET nom_com_afo = reg_det_not.nombres_proc CLIPPED, 
					  " ", 
					  reg_det_not.paterno_proc CLIPPED,
					  " ",
					  reg_det_not.nombres_proc CLIPPED
                        LET nom_com_bdn = campo_08 CLIPPED, " ",
				          campo_06 CLIPPED, " ",
					  campo_07 CLIPPED

			IF nom_com_afo <> nom_com_bdn THEN
                           OUTPUT TO REPORT dif_nombs (campo_03, nom_com_afo,
						       nom_com_bdn, c_fecha_c) 
                        END IF
			FINISH REPORT dif_nombs
                        ### 

                     END IF
                  ELSE
                     LET marca_reg = 2
		  END IF
            ELSE
                LET vmarca = 610
                SELECT @n_folio
                  INTO vn_folio
                  FROM afi_mae_afiliado
                 WHERE @n_seguro = campo_03
		CALL desmarca_cuenta(campo_03, vmarca, g_usuario, vn_folio)
                LET marca_reg = 9
	    END IF

            SELECT 'X'
              FROM afi_det_notifica adn
             WHERE adn.n_seguro = campo_03
               AND adn.f_transf_lote = c_fecha_c
               AND adn.consec_dia = campo_110
             GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
               INSERT INTO afi_det_notifica
               VALUES (campo_01,
                       campo_02,
                       campo_03,
                       campo_04,
                       campo_05,
                       campo_06,
                       campo_07,
                       campo_08,
                       campo_09,
                       campo_10,
                       campo_11,
                       campo_12,
                       campo_13,
                       campo_14,
                       campo_15,
                       campo_16,
                       campo_17,
                       campo_18,
                       campo_19,
                       campo_20,
                       campo_21,
                       campo_22,
                       0,
	               c_fecha_c,
                       campo_110,
                       marca_reg,
                       HOY,
                       g_usuario
	              )

	        IF marca_reg = 0 THEN
                    SELECT COUNT(*)
                    INTO   cont_marca
                    FROM   cta_act_marca
                    WHERE  cta_act_marca.nss = campo_03
                    AND    cta_act_marca.marca_cod = 610

                    IF cont_marca IS NULL OR
                       cont_marca = 0 THEN
                        SELECT @n_folio
                          INTO vn_folio
                          FROM afi_mae_afiliado
                         WHERE @n_seguro = campo_03
                        CALL marca_cuenta()
                    ELSE
                        UPDATE afi_det_notifica
			SET    afi_det_notifica.estado = 1
                        WHERE  afi_det_notifica.n_seguro = campo_03
			AND    afi_det_notifica.factualiza = HOY

                        LET cont_marca = 0
                    END IF
                END IF
	    END IF

            LET marca_reg = 0
            LET vtotal = vtotal + 1
        END IF

        IF cont_reg = total_reg THEN
            LET campo_201 = carga_reg[001,002]
            LET campo_202 = carga_reg[003,004]
            LET campo_203 = carga_reg[005,007]
            LET campo_204 = carga_reg[008,009]
            LET campo_205 = carga_reg[010,012]
            LET campo_206 = carga_reg[013,014]
            LET campo_207 = carga_reg[015,016]
            LET campo_208 = carga_reg[013,020]
            LET campo_209 = carga_reg[021,023]
            LET campo_210 = carga_reg[024,032]
            LET campo_211 = carga_reg[033,041]

            SELECT 'X'
            FROM   afi_sum_notifica asn
            WHERE  asn.f_transf_lote = c_fecha_c
            AND    asn.consec_dia = campo_110
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
                INSERT INTO afi_sum_notifica
                VALUES (campo_201,
                        campo_202,
                        campo_203,
                        campo_204,
                        campo_205,
                        campo_206,
                        campo_207,
                        c_fecha_c,
                        campo_209,
                        campo_210,
                        campo_211)
            END IF
        END IF

    END FOREACH

    INSERT INTO afi_ctr_arh_notif 
    VALUES(generar, vtotal, HOY)

    IF NOT bnd_proceso THEN
        DISPLAY "                 DATOS A PROCESAR                      "
            AT 10,1 ATTRIBUTE ( REVERSE )

        DISPLAY "Total de Registros del lote     : ",
                 vtotal USING "#######&" AT 11,15 ATTRIBUTE ( BOLD )

        PROMPT "Presione [Enter] para continuar" FOR enter
    ELSE
        DISPLAY "                 DATOS A PROCESAR                      "

        DISPLAY "Total de Registros del lote     : ",
                 vtotal USING "#######&" 
    END IF

END FUNCTION

FUNCTION despliega_archivos()
#da--------------------------

    DEFINE
        aux_pausa CHAR(1),
        HOY       DATE,
        SW_1      SMALLINT

    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY control-o

        WHENEVER ERROR STOP

    LET HOY = TODAY

    OPEN WINDOW window_1 AT 2,2 WITH FORM "AFIM0231" ATTRIBUTE(BORDER)
    DISPLAY " AFIM030                 CONSULTA REGISTROS PROCESADOS                        " AT 3,1
    ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

    MENU "CONSULTA ARCHIVOS RESPUESTA PROCESADOS"
        COMMAND "Consulta" "Consultar Archivos PROCESAR"
            CALL Consulta()
        COMMAND "Salir" "Salir del Programa"
            EXIT MENU
    END MENU

    CLOSE WINDOW window_1

END FUNCTION

FUNCTION Consulta()
#C-----------------

    DEFINE ga_record ARRAY[3000] OF RECORD
        nombre_archivo LIKE afi_ctr_arh_notif.nombre_archivo,
        total_reg      LIKE afi_ctr_arh_notif.total_reg,
        total_aprob    INTEGER,
        total_rech     INTEGER,
        fecha_proceso  LIKE afi_ctr_arh_notif.fecha_proceso
    END RECORD

    DEFINE
        vcount     ,
        vtotal     ,
        vaprobados ,
        vrechazos  INTEGER

    DEFINE
        pos        SMALLINT

    LET vcount      = 0
    LET vtotal      = 0
    LET vaprobados  = 0
    LET vrechazos   = 0

    SELECT COUNT(*)
    INTO   vcount
    FROM   afi_ctr_arh_notif

    SELECT SUM(@total_reg)
    INTO   vtotal
    FROM   afi_ctr_arh_notif

    DISPLAY "                                                                               " AT 1,1
    DISPLAY "    CTRL-C cancela                                                             " AT 2,1
    DISPLAY " CONSULTA " AT 2,65

    DECLARE curp_12 CURSOR FOR
    SELECT @nombre_archivo, @total_reg, 0, 0, @fecha_proceso
    FROM afi_ctr_arh_notif
    ORDER BY fecha_proceso

    LET pos = 1

    FOREACH curp_12 INTO ga_record[pos].*
        LET pos = pos + 1
        LET ga_record[pos].total_aprob = 0
        LET ga_record[pos].total_rech  = 0
    END FOREACH

    DISPLAY vcount      TO vcount
    DISPLAY vtotal      TO vtotal
    DISPLAY vaprobados  TO vaprobados
    DISPLAY vrechazos   TO vrechazos

    IF (pos-1) >= 1 THEN
        CALL  SET_COUNT(pos-1)

        DISPLAY BY NAME vtotal

        DISPLAY ARRAY ga_record TO scr_1.*

        ON KEY (INTERRUPT)
            CLEAR FORM
            EXIT DISPLAY

        END DISPLAY
    ELSE
        ERROR "ARCHIVO DE PROCESAR VACIO"
    END IF

END FUNCTION

FUNCTION actualiza_operacion()
#ao---------------------------

   {
    UPDATE bat_ctr_operacion
    SET    estado_operacion = 4,
           fecha_fin        = CURRENT,
           nombre_archivo   = nom_afi
    WHERE  pid              = reg_bat.pid
    AND    proceso_cod      = reg_bat.proceso_cod
    AND    opera_cod        = reg_bat.opera_cod

    UPDATE bat_ctr_proceso
    SET    marca_cod = 4,
           fecha_fin      = CURRENT
    WHERE  pid            = reg_bat.pid
    AND    proceso_cod    = reg_bat.proceso_cod
   }

END FUNCTION

FUNCTION marca_cuenta()
#mc--------------------

    DEFINE 
      pmarca_causa       SMALLINT

    LET pmarca_causa = 0

    LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
                  "'",vn_seguro,"'",
                  ",",edo_proc,
                  ",",vn_folio,
                  ",",vmarca_estado,
                  ",",vcodigo_rechazo,
                  ",",pmarca_causa,
                  ",","''", ",",
                  "'",g_usuario,"'",")"

    LET ejecuta = ejecuta CLIPPED

    PREPARE clausula_spl FROM ejecuta

    DECLARE cursor_marca CURSOR FOR clausula_spl

    OPEN cursor_marca

    FETCH cursor_marca INTO xcodigo_marca, xcodigo_rechazo

    CLOSE cursor_marca

END FUNCTION

FUNCTION lista_err()
#-------------------

     DEFINE hora          CHAR(8)
     DEFINE vhora         CHAR(6)
     DEFINE hoy           DATE
     DEFINE vcod_rechazo  CHAR(8)
     DEFINE vcont         INTEGER
     DEFINE pat_1         CHAR(40)
     DEFINE mat_1         CHAR(40)
     DEFINE nom_1         CHAR(40)
     DEFINE pat_2         CHAR(40)
     DEFINE mat_2         CHAR(40)
     DEFINE nom_2         CHAR(40)

     DEFINE gr_curp RECORD
         n_seguro       CHAR(11),
         n_folio          DECIMAL(10,0),
         tipo_solicitud CHAR(1),
         nombre_1       CHAR(50),
         nombre_2       CHAR(50),
         fecha_lote     DATE,
         fecha_proc     DATE,
         marca          SMALLINT
     END RECORD

     DEFINE G_LISTA         CHAR(200)
     DEFINE G_IMPRIME       CHAR(200)

    DEFINE resp   CHAR(1)

    LET hora = TIME
    LET hoy = TODAY

    LET vhora = hora[1,2],hora[4,5],hora[7,8]

    LET G_LISTA =  g_paramgrales.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED,".NOT_IMSS_RCH.",
                   HOY USING "ddmmyy","_",vhora CLIPPED

    START REPORT listado TO G_LISTA

    DECLARE c_carga CURSOR FOR
        SELECT a.n_seguro,
               c.n_folio,
               c.tipo_solicitud,
               c.paterno, 
               c.materno, 
               c.nombres, 
               a.paterno_proc, 
               a.materno_proc, 
               a.nombres_proc, 
               a.f_transf_lote,
	       a.factualiza,
               ''
        FROM   safre_tmp:tmp_det_notifica a, afi_mae_afiliado c
        WHERE  c.n_seguro = a.n_seguro
        ORDER BY 1

    FOREACH c_carga INTO gr_curp.n_seguro       ,
                         gr_curp.n_folio        ,
                         gr_curp.tipo_solicitud ,
                         pat_1          ,
                         mat_1          ,
                         nom_1          ,
                         pat_2          ,
                         mat_2          ,
                         nom_2          ,
                         gr_curp.fecha_lote     ,
                         gr_curp.fecha_proc     ,
                         gr_curp.marca

        LET gr_curp.nombre_1 = pat_1 CLIPPED," ",
                               mat_1 CLIPPED," ",
                               nom_1 CLIPPED

        LET gr_curp.nombre_2 = pat_2 CLIPPED," ",
                               mat_2 CLIPPED," ",
                               nom_2 CLIPPED

        OUTPUT TO REPORT listado(gr_curp.*)
    END FOREACH 

    FINISH REPORT listado

    ERROR "LISTADO GENERADO" SLEEP 2

    LET G_LISTA =  "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED,".NOT_IMSS_RCH.",
                   HOY USING "ddmmyy","_",vhora CLIPPED

    RUN G_LISTA

    LET G_IMPRIME = "lp ",g_paramgrales.ruta_listados CLIPPED,"/",
                    g_usuario CLIPPED,".NOT_IMSS_RCH.",
                    HOY USING "ddmmyy","_",vhora CLIPPED

    RUN G_IMPRIME

END FUNCTION

REPORT listado(rpt)

    DEFINE rpt RECORD
         n_seguro       CHAR(11),
         n_folio          DECIMAL(10,0),
         tipo_solicitud CHAR(1),
         nombre_1       CHAR(50),
         nombre_2       CHAR(50),
         fecha_lote     DATE,
         fecha_proc     DATE,
         marca          SMALLINT
    END RECORD

    DEFINE
        l_estado     CHAR(16),
        aux_sexo     CHAR(10),
        razon_social CHAR(40),
        vcont        SMALLINT

    OUTPUT
        PAGE LENGTH 90
        TOP  MARGIN 0
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 0

    FORMAT
    PAGE HEADER

    PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 001,"========================================"           ,
            COLUMN 040,"========================================"           ,
            COLUMN 080,"========================================"           ,
            COLUMN 120,"========================================"           ,
            COLUMN 160,"=========="
        PRINT
            COLUMN 001,razon_social                                         ,
            COLUMN 140,"FECHA   :",hoy USING "DD/MM/YY"    
            --COLUMN 149,hora [1,5]
        PRINT
            COLUMN 001,"AFIC007"                                            ,
            COLUMN 035," E S T A D O   D E   N O T I F I C A D O S   P O R   I M S S  R E C H A Z A D O S       ",

            COLUMN 140,"NUM.PAG.:",pageno USING "####,###"
            SKIP 1 LINE
        PRINT
            COLUMN 001,"----------------------------------------"           ,
            COLUMN 040,"----------------------------------------"           ,
            COLUMN 080,"----------------------------------------"           ,
            COLUMN 120,"----------------------------------------"           ,
            COLUMN 160,"----------"
       SKIP 1 LINES
       PRINT
            COLUMN 001, "NSS",
            COLUMN 014, "No. Folio",
            COLUMN 024, "TS",
            COLUMN 027, "Nombre BDSAR",
            COLUMN 079, "Nombre PROCANASE",
            COLUMN 131, "F. Transf.",
            COLUMN 143, "F. Proceso"
       PRINT
            COLUMN 001,"========================================" ,
            COLUMN 040,"========================================" ,
            COLUMN 080,"========================================" ,
            COLUMN 120,"========================================" ,
            COLUMN 160,"=========="

    ON EVERY ROW
       LET vcont = vcont + 1
       
       PRINT
            COLUMN 001, rpt.n_seguro,
            COLUMN 015, rpt.n_folio USING "&&&&&&&&",
            COLUMN 024, rpt.tipo_solicitud,
            COLUMN 027, rpt.nombre_1,
            COLUMN 079, rpt.nombre_2,
            COLUMN 131, rpt.fecha_lote USING "DD/MM/YYYY",
            COLUMN 143, rpt.fecha_proc USING "DD/MM/YYYY"

       SKIP 1 LINES 

    ON LAST ROW

  {
    SELECT count(*)
    INTO   vcodigo_39
    FROM   tmp_det_notifica
    WHERE  factualiza = HOY

       SKIP 1 LINES 
       PRINT
            COLUMN 145, "================"
       PRINT
            COLUMN 100, "TOTAL REGISTROS CODIGO DE NOTIFICACION IMSS :", 
                        vcodigo_39 USING "#####"
  }

       SKIP 2 LINES

END REPORT

##################################################################################
FUNCTION desmarca_cuenta(vnss, vmarca, vusuario, vn_folio)
#dc----------------------------

   DEFINE 
     vnss               CHAR(11),
     vmarca             SMALLINT,
     vusuario           CHAR(08),
     vn_folio          DECIMAL(10,0),
     pestado_marca      SMALLINT,
     pmarca_causa       SMALLINT,
     reg                RECORD
     mcorrelativo       INTEGER
                        END RECORD

   LET pestado_marca = 0
   LET pmarca_causa  = 0

   PREPARE eje_desmarca FROM v_desmarca

   DECLARE c_marca CURSOR FOR
   SELECT @correlativo
     FROM cta_act_marca
    WHERE @nss       = vnss
      AND @marca_cod = vmarca
   FOREACH c_marca INTO reg.*
      EXECUTE eje_desmarca 
      USING vnss,
            vmarca,
            reg.mcorrelativo,
            pestado_marca,
            pmarca_causa,
            vusuario
   END FOREACH

END FUNCTION
##################################################################################


#############################################################################
{
REPORT dif_nombs (rn_seguro, rnom_com_afo, rnom_com_bdns, rfec_notif)

    DEFINE 
         rn_seguro       CHAR(11),
         rnom_com_afo    CHAR(80),
         rnom_com_bdns   CHAR(80),
	 rfec_notif	 DATE

    OUTPUT
        PAGE LENGTH     90
        TOP  MARGIN      0
        BOTTOM MARGIN    0
        LEFT MARGIN      0
        RIGHT MARGIN     0

    FORMAT
    PAGE HEADER

    PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 001,"========================================"           ,
            COLUMN 040,"========================================"           ,
            COLUMN 080,"========================================"           ,
            COLUMN 120,"========================================"           ,
            COLUMN 160,"=========="
        PRINT
            #COLUMN 001,razon_social                                         ,
            COLUMN 140,"FECHA   :",hoy USING "DD/MM/YY"    
        PRINT
            COLUMN 001,"AFIC007"                                            ,
            COLUMN 035," C O M P A R A C I O N   D E   N S S   A F O R E  -  B D N S A R                        ",

            COLUMN 140,"NUM.PAG.:",pageno USING "####,###"
            SKIP 1 LINE
        PRINT
            COLUMN 001,"----------------------------------------"           ,
            COLUMN 040,"----------------------------------------"           ,
            COLUMN 080,"----------------------------------------"           ,
            COLUMN 120,"----------------------------------------"           ,
            COLUMN 160,"----------"
       SKIP 1 LINES
       PRINT
            COLUMN 001, "NSS",
            COLUMN 027, "Nombre AFORE",
            COLUMN 079, "Nombre BDSAR",
            COLUMN 131, "F. Transf."
       PRINT
            COLUMN 001,"========================================" ,
            COLUMN 040,"========================================" ,
            COLUMN 080,"========================================" ,
            COLUMN 120,"========================================" ,
            COLUMN 160,"=========="

    ON EVERY ROW
       
       PRINT
            COLUMN 001, rn_seguro,
            COLUMN 027, rnom_com_afo,
            COLUMN 079, rnom_com_bdns,
            COLUMN 131, rfec_notif USING "DD/MM/YYYY"

       SKIP 1 LINES 

    ON LAST ROW

    SKIP 2 LINES

END REPORT
}
#############################################################################
REPORT dif_nombs (rn_seguro, rnom_com_afo, rnom_com_bdns, rfec_notif)

    DEFINE 
         rn_seguro       CHAR(11),
         rnom_com_afo    CHAR(80),
         rnom_com_bdns   CHAR(80),
	 rfec_notif	 DATE

    OUTPUT
        PAGE LENGTH     90
        TOP  MARGIN      0
        BOTTOM MARGIN    0
        LEFT MARGIN      0
        RIGHT MARGIN     0

    FORMAT
    PAGE HEADER

        PRINT
            COLUMN 140,"FECHA   :",hoy USING "DD/MM/YY"    
        PRINT
            COLUMN 001,"AFIC007"                                            ,
            COLUMN 035," C O M P A R A C I O N   D E   N S S   A F O R E  -  B D N S A R                        ",

            COLUMN 140,"NUM.PAG.:",pageno USING "####,###"
            SKIP 1 LINE
       PRINT
            COLUMN 001, "NSS|",
                        "Nombre AFORE|",
                        "Nombre BDSAR|",
                        "F. Transf.|"

    ON EVERY ROW
       
       PRINT
            COLUMN 001, rn_seguro,                        "|",
                        rnom_com_afo  CLIPPED,            "|",
                        rnom_com_bdns CLIPPED,            "|",
                        rfec_notif    USING "DD/MM/YYYY", "|"

       SKIP 1 LINES 

END REPORT
#############################################################################
