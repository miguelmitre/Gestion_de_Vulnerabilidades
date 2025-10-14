#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa SEPC101  => CARGA DE ARCHIVO CERTIFICACION DE PROCESAR / trasp    #
#Sistema           => SEP.                                                  #
#Autor             => JESUS YAÑEZ MORENO                                    #
#Fecha             => 25 de JULIO DE 2005                                   #
#Fecha             => 27 FEBRERO 2013  CPL-1200                             #
#                    (se cambia status_interno de 70 a 65 en cod_op05())    #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_reg  RECORD
        generar CHAR(20)
    END RECORD

    DEFINE
        aux_status_interno SMALLINT,
        diagnostico        CHAR(03),
        diag               CHAR(3),
        v_fecha2           CHAR(10),
        observacion        CHAR(30),
        sello              CHAR(24)

    DEFINE hoy         DATE
    DEFINE xx_fecha    DATE
    DEFINE aux_pausa   CHAR(1)
    DEFINE enter       CHAR(1)
    DEFINE g_usuario   CHAR(8)
    DEFINE hora        CHAR(8)
    DEFINE carga       CHAR(50)
    DEFINE ejecuta     CHAR(100)
    DEFINE corr        CHAR(100)
    DEFINE G_IMPRE     CHAR(300)
    DEFINE gimpresion  CHAR(300)
    DEFINE total_reg   SMALLINT
    DEFINE g_plano2    SMALLINT
    DEFINE aceptar     SMALLINT
    DEFINE rechazar    SMALLINT
    DEFINE aclaracion  SMALLINT
    DEFINE pendiente   SMALLINT

    DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore
    DEFINE g_paramgrales RECORD LIKE seg_modulo.*

    DEFINE detalle2_1 RECORD
        total         SMALLINT,
        cve_afo_ced   SMALLINT,
        cod_operacion CHAR(2),
        descripcion   CHAR(10)
    END RECORD

    DEFINE detalle2_2 RECORD
        total_1         SMALLINT,
        cod_operacion_1 CHAR(2),
        descripcion_1   CHAR(10)
    END RECORD

    DEFINE  detalle2_3 RECORD
        total_2 SMALLINT
    END RECORD

    DEFINE reg_det RECORD
        tipo_registro        CHAR(02)     ,
        contador_servicio    CHAR(10)     ,      
        clave_operacion      CHAR(02)     ,
        nss_solicitud        CHAR(11)     ,
        curp_solicitud       CHAR(18)     ,
        rfc_trabajador       CHAR(13)     ,
        paterno              CHAR(40)     ,
        materno              CHAR(40)     ,
        nombres              CHAR(40)     ,
        fecha_nacimiento     CHAR(08)     ,
        clave_promotor       CHAR(10)     ,
        fec_recp_sol_afore   CHAR(08)     ,
        folio_solicitud      DECIMAL(16,0),
        sexo                 SMALLINT     ,
        entidad_nacimiento   CHAR(02)     ,
        ind_infonavit        CHAR(01)     ,
        nacionalidad         CHAR(03)     ,
        tip_prob             CHAR(01)     ,
        fol_prob             CHAR(10)     ,
        doc_prob             CHAR(16)     ,
        cod_err_ori          CHAR(04)     ,
        cve_afo_ced          CHAR(03)     ,
        folio_edo_cta        CHAR(08)     ,
        cod_operacion        CHAR(02)     ,
        diag_proceso         CHAR(15)     ,
        ident_lote_origen    CHAR(16)     ,
        nss_oficial          CHAR(11)     ,
        ind_nss_modificado   CHAR(01)     ,
        fec_emision_certif   CHAR(08)     ,
        curp_oficial         CHAR(18)     ,
        ind_curp_modif       CHAR(01)     ,
        n_rfc_bd             CHAR(13)     ,
        nombre_bd            CHAR(50)     ,
        nombre_pcanase       CHAR(50)     ,
        fena_bd              CHAR(08)     ,
        codven_bd            CHAR(10)     ,
        sexo_bd              SMALLINT     ,
        estadon_bd           CHAR(02)     ,
        fprimer_afil         CHAR(08)     ,
        falta_actual         CHAR(08)     ,
        cve_afore            CHAR(03)     ,
        nacionalidad_bd      CHAR(03)     ,
        tip_prob_bd          CHAR(01)     ,
        fol_prob_bd          CHAR(10)     ,
        doc_prob_bd          CHAR(16)     ,
        fecha_recep_sello    CHAR(08)     ,
        hora_recep_sello     CHAR(02)     ,
        minuto_recep_sello   CHAR(02)     ,
        seg_recep_sello      CHAR(02)     ,
        cseg_recep_sello     CHAR(02)     ,
        consecutivo_recep    CHAR(08)     ,
        periodo              CHAR(06)     ,
        salario              DECIMAL(9,2)
    END RECORD

    DEFINE
        x_fecha,f1,f2,f3    CHAR(10),
        falta,fpafil,fnbd   DATE  

    DEFINE reg_bat RECORD
        pid            INTEGER,
        proceso_cod    INTEGER,
        opera_cod      INTEGER,
        nombre_archivo CHAR(25)
    END RECORD

    DEFINE bnd_proceso SMALLINT

    DEFINE reg_carta RECORD LIKE safre_af:int_ctr_carta.*
    DEFINE consulta_carta CHAR(120)

    DEFINE
        c_periodo  CHAR(10),
        f_periodo  DATE

    DEFINE 
	longitud   SMALLINT,
	i	   SMALLINT,
	i2	   SMALLINT,
	archb	   SMALLINT,
	nom_arch   CHAR(20)

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG(FGL_GETENV("USER")||".SEPC101.log")
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
#--------------

    LET reg_bat.pid         = ARG_VAL(1)
    LET reg_bat.proceso_cod = ARG_VAL(2)
    LET reg_bat.opera_cod   = ARG_VAL(3)

    LET bnd_proceso = 0

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
    END IF 

    LET g_reg.generar = "S"

    LET hoy = TODAY

    SELECT  codigo_afore,USER
    INTO    w_codigo_afore,g_usuario
    FROM    tab_afore_local

    SELECT *
    INTO   g_paramgrales.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'sep'

    LET aceptar    = 0
    LET rechazar   = 0
    LET aclaracion = 0
    LET pendiente  = 0
    LET archb      = 0

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE safre_tmp:plano2
        
        CREATE TABLE safre_tmp:plano2
            (n_registros CHAR(570))

        DATABASE safre_af
    WHENEVER ERROR STOP

    INITIALIZE reg_carta.* TO NULL

    LET nom_arch = NULL

END FUNCTION

FUNCTION crea_tablas() 
#ct-------------------

    WHENEVER ERROR CONTINUE
    DATABASE safre_tmp
        DROP TABLE encabezado2
        DROP TABLE detalle2
        DROP TABLE sumario2
    WHENEVER ERROR STOP

    CREATE TABLE encabezado2
        (campo1  CHAR(2),
	 campo2  CHAR(2),
	 campo3  CHAR(2),
	 campo4  CHAR(2),
	 campo5  CHAR(3),
	 campo6  CHAR(2),
	 campo7  CHAR(3),
	 campo8  CHAR(8),
	 campo9  CHAR(3),
	 campo10 CHAR(3),
	 campo11 CHAR(2),
	 campo12 CHAR(8),
	 campo13 CHAR(8),
	 campo14 CHAR(8),
	 campo15 CHAR(8),
	 campo16 CHAR(8),
	 campo17 CHAR(1),
	 campo18 CHAR(9),
	 campo19 CHAR(2),
	 campo20 CHAR(3),
	 campo21 CHAR(2),
	 campo22 CHAR(3))

    CREATE TABLE detalle2
        (tipo_registro                CHAR(02)     ,
         contador_servicio            CHAR(10)     ,      
         clave_operacion              CHAR(02)     ,
         nss_solicitud                CHAR(11)     ,
         curp_solicitud               CHAR(18)     ,
         rfc_trabajador               CHAR(13)     ,
         paterno                      CHAR(40)     ,
         materno                      CHAR(40)     ,
         nombres                      CHAR(40)     ,
         fecha_nacimiento             CHAR(08)     ,
         clave_promotor               CHAR(10)     ,
         fec_recp_sol_afore           CHAR(08)     ,
         folio_solicitud              DECIMAL(16,0),
         sexo                         SMALLINT     ,
         entidad_nacimiento           CHAR(02)     ,
         ind_infonavit                CHAR(01)     ,
         nacionalidad                 CHAR(03)     ,
         tip_prob                     CHAR(01)     ,
         fol_prob                     CHAR(10)     ,
         doc_prob                     CHAR(16)     ,
         cod_err_ori                  CHAR(04)     ,
         cve_afo_ced                  CHAR(03)     ,
         folio_edo_cta                CHAR(08)     ,
         cod_operacion                CHAR(02)     ,
         diag_proceso                 CHAR(15)     ,
         ident_lote_origen            CHAR(16)     ,
         nss_oficial                  CHAR(11)     ,
         ind_nss_modificado           CHAR(01)     ,
         fec_emision_certif           CHAR(08)     ,
         curp_oficial                 CHAR(18)     ,
         ind_curp_modif               CHAR(01)     ,
         n_rfc_bd                     CHAR(13)     ,
         nombre_bd                    CHAR(50)     ,
         nombre_pcanase               CHAR(50)     ,
         fena_bd                      CHAR(08)     ,
         codven_bd                    CHAR(10)     ,
         sexo_bd                      SMALLINT     ,
         estadon_bd                   CHAR(02)     ,
         fprimer_afil                 CHAR(08)     ,
         falta_actual                 CHAR(08)     ,
         cve_afore                    CHAR(03)     ,
         nacionalidad_bd              CHAR(03)     ,
         tip_prob_bd                  CHAR(01)     ,
         fol_prob_bd                  CHAR(10)     ,
         doc_prob_bd                  CHAR(16)     ,
         fecha_recep_sello            CHAR(08)     ,
         hora_recep_sello             CHAR(02)     ,
         minuto_recep_sello           CHAR(02)     ,
         seg_recep_sello              CHAR(02)     ,
         cseg_recep_sello             CHAR(02)     ,
         consecutivo_recep            CHAR(08)     ,
         periodo                      CHAR(06)     ,
         salario                      DECIMAL(9,2))

    CREATE TABLE sumario2
        (campo1 CHAR(2),
         campo2 CHAR(2),
         campo3 CHAR(3),
         campo4 CHAR(8),
         campo5 CHAR(3),
         campo6 CHAR(2),
         campo7 CHAR(2),
         campo8 CHAR(9),
         campo9 CHAR(9))
        
    DATABASE safre_af

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "SEPC1011" ATTRIBUTE(BORDER)

    DISPLAY " SEPC101      CARGA ARCHIVO CERTIFICACION PROCESAR (SOL SEP TRASP)             " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

    DISPLAY g_paramgrales.ruta_rescate AT 7,10

	INPUT BY NAME g_reg.generar
	      AFTER FIELD generar
	          IF g_reg.generar IS NULL THEN
		      ERROR "Campo NO puede ser NULO"
		      NEXT FIELD generar
		  END IF

                  LET nom_arch = g_reg.generar CLIPPED
              
                  LET longitud = LENGTH(g_reg.generar CLIPPED)	

                 -- FOR i = 1 TO longitud
                 --     IF g_reg.generar[i] = "." THEN
                 --        LET i  = i + 1
	         --        LET i2 = i + 2
                 --        IF g_reg.generar[i,i2] = 'taa' THEN
		 --           let g_reg.generar = g_reg.generar[i,i2]
			    LET archb = 1
              --              EXIT FOR
               --          ELSE 
		--            PROMPT 
		--	    "El archivo no es de -Certificacion por Traspaso-.",
		--	    " <ENTER> para continuar." FOR enter
		--	    LET archb = 0
		 --           NEXT FIELD generar 
	          --       END IF
                  --    END IF
	          --END FOR

                  IF archb = 1 THEN
		      LET carga = NULL
		      LET carga = g_paramgrales.ruta_rescate CLIPPED,"/",
                                  nom_arch CLIPPED

	   	      WHENEVER ERROR CONTINUE
                         LOAD FROM carga INSERT INTO safre_tmp:plano2
		      WHENEVER ERROR STOP

                      SELECT COUNT(*) 
                      INTO   g_plano2 
                      FROM   safre_tmp:plano2

	  	      IF g_plano2 IS NULL OR g_plano2 = 0 THEN
		         ERROR "NOMBRE DE ARCHIVO INCORRECTO O VACIO, REVISE " 
                         SLEEP 5
		         EXIT PROGRAM
                      END IF           
                  
		      ERROR "Procesando Informacion"

                      CALL rescata_valores()

                      --CALL reporte()-----------------------miguel

                      --LET ejecuta = "fglgo AFIL003 DH" CLIPPED
                      --RUN ejecuta

		      ERROR "Proceso finalizado " SLEEP 3
	              EXIT PROGRAM
		  ELSE
		      PROMPT 
			    "El archivo no es de -Certificacion por Traspaso-.",
			    " <ENTER> para continuar." FOR enter
		      LET archb = 0
		      NEXT FIELD generar 
		  END IF 

	END INPUT

END FUNCTION

FUNCTION rescata_valores()
#rv-----------------------

    CALL crea_tablas()   #ct
    CALL actualiza_datos()
    CALL revisa_datos()
    CALL despliega_resultados()  #dr

END FUNCTION

FUNCTION actualiza_datos()
#-------------------------

    DEFINE 
        cont_reg             SMALLINT
 
    DEFINE 
        carga_reg            CHAR(570)

    DEFINE   
        campo_011            CHAR(02),
        campo_012            CHAR(02),
        campo_013            CHAR(02),
        campo_014            CHAR(02),
        campo_015            CHAR(03),
        campo_016            CHAR(02),
        campo_017            CHAR(03),
        campo_018            CHAR(08),
        campo_019            CHAR(03),
        campo_110            CHAR(03),
        campo_111            CHAR(02),
        campo_112            CHAR(08),
        campo_113            CHAR(08),
        campo_114            CHAR(08),
        campo_115            CHAR(08),
        campo_116            CHAR(08),
        campo_117            CHAR(01),
        campo_118            CHAR(09),
        campo_119            CHAR(02),
        campo_210            CHAR(03),
        campo_211            CHAR(02),
        campo_212            CHAR(03),

        campo_01             CHAR(02),
        campo_02             CHAR(10),
        campo_03             CHAR(02),
        campo_04             CHAR(11),
        campo_05             CHAR(18),
        campo_06             CHAR(13),
        campo_07             CHAR(40),
        campo_08             CHAR(40),
        campo_09             CHAR(40),
        campo_10             CHAR(08),
        campo_11             CHAR(10),
        campo_12             CHAR(08),
        campo_13             CHAR(10),
        campo_14             CHAR(01),
        campo_15             CHAR(02),
        campo_16             CHAR(01),
        campo_17             CHAR(03),
        campo_18             CHAR(01),
        campo_19             CHAR(10),
        campo_20             CHAR(16),
        campo_21             CHAR(04),
        campo_22             CHAR(03),
        campo_23             CHAR(08),
        campo_24             CHAR(02),
        campo_25             CHAR(15),
        campo_26             CHAR(16),
        campo_27             CHAR(11),
        campo_28             CHAR(01),
        campo_29             CHAR(08),
        campo_30             CHAR(18),
        campo_31             CHAR(01),
        campo_32             CHAR(13),
        campo_33             CHAR(50),
        campo_34             CHAR(150),
        campo_35             CHAR(08),
        campo_36             CHAR(10),
        campo_37             CHAR(01),
        campo_38             CHAR(02),
        campo_39             CHAR(08),
        campo_40             CHAR(08),
        campo_41             CHAR(03),
        campo_42             CHAR(03),
        campo_43             CHAR(01),
        campo_44             CHAR(10),
        campo_45             CHAR(16),
        campo_46             CHAR(01),
        campo_47             CHAR(08),
        campo_48             CHAR(02),
        campo_49             CHAR(02),
        campo_50             CHAR(02),
        campo_51             CHAR(02),
        campo_52             CHAR(08),
        campo_53             CHAR(08),
        campo_54             CHAR(08),
        campo_201            CHAR(02),
        campo_202            CHAR(02), 
        campo_203            CHAR(03),
        campo_204            CHAR(08),
        campo_205            CHAR(03),
        campo_206            CHAR(02),
        campo_207            CHAR(02),
        campo_208            CHAR(09),
        campo_209            CHAR(09)

    LET cont_reg = 0

    SELECT COUNT(*)
    INTO   total_reg
    FROM   safre_tmp:plano2

    DECLARE cursor_1 CURSOR FOR 
    SELECT  * 
    FROM    safre_tmp:plano2

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
            LET campo_018 = carga_reg[017,024]
            LET campo_019 = carga_reg[025,027]
            LET campo_110 = carga_reg[028,030]
            LET campo_111 = carga_reg[031,032]
            LET campo_112 = carga_reg[033,040]
            LET campo_113 = carga_reg[041,048]
            LET campo_114 = carga_reg[049,056]
            LET campo_115 = carga_reg[057,064]
            LET campo_116 = carga_reg[065,072]
            LET campo_117 = carga_reg[073,073]
            LET campo_118 = carga_reg[074,082]
            LET campo_119 = carga_reg[083,084]
            LET campo_210 = carga_reg[085,087]
            LET campo_211 = carga_reg[088,089]
            LET campo_212 = carga_reg[090,092]

      	    INSERT INTO safre_tmp:encabezado2 
            VALUES (campo_011 ,
                    campo_012 ,
                    campo_013 ,
                    campo_014 ,
                    campo_015 ,
                    campo_016 ,
                    campo_017 ,
                    campo_018 ,
                    campo_019 ,
                    campo_110 ,
                    campo_111 ,
                    campo_112 ,
                    campo_113 ,
                    campo_114 ,
                    campo_115 ,
                    campo_116 ,
                    campo_117 ,
                    campo_118 ,
                    campo_119 ,
                    campo_210 ,
                    campo_211 ,
                    campo_212 
                    )
        END IF

        IF cont_reg <> total_reg  AND  cont_reg <> 1 THEN
            LET campo_01 = carga_reg[001,002]
            LET campo_02 = carga_reg[003,012]
            LET campo_03 = carga_reg[013,014]
            LET campo_04 = carga_reg[015,025]
            LET campo_05 = carga_reg[026,043]
            LET campo_06 = carga_reg[044,056]
            LET campo_07 = carga_reg[057,096]
            LET campo_08 = carga_reg[097,136]
            LET campo_09 = carga_reg[137,176]
            LET campo_10 = carga_reg[177,184]
            LET campo_11 = carga_reg[185,194]
            LET campo_12 = carga_reg[195,202]
            LET campo_13 = carga_reg[203,212]
            LET campo_14 = carga_reg[213,213]
            LET campo_15 = carga_reg[214,215]
            LET campo_16 = carga_reg[216,216]
            LET campo_17 = carga_reg[217,219]
            LET campo_18 = carga_reg[220,220]
            LET campo_19 = carga_reg[221,230]
            LET campo_20 = carga_reg[231,246]
            LET campo_21 = carga_reg[247,250]
            LET campo_22 = carga_reg[251,253]
            LET campo_23 = carga_reg[254,261]
            LET campo_24 = carga_reg[262,263]
            LET campo_25 = carga_reg[264,278]
            LET campo_26 = carga_reg[279,294]
            LET campo_27 = carga_reg[295,305]
            LET campo_28 = carga_reg[306,306]
            LET campo_29 = carga_reg[307,314]
            LET campo_30 = carga_reg[315,332]
            LET campo_31 = carga_reg[333,333]
            LET campo_32 = carga_reg[334,346]
            LET campo_33 = carga_reg[347,396]
            LET campo_34 = carga_reg[397,446]
            LET campo_35 = carga_reg[447,454]
            LET campo_36 = carga_reg[455,464]
            LET campo_37 = carga_reg[465,465]
            LET campo_38 = carga_reg[466,467]
            LET campo_39 = carga_reg[468,475]
            LET campo_40 = carga_reg[476,483]
            LET campo_41 = carga_reg[484,486]
            LET campo_42 = carga_reg[487,489]
            LET campo_43 = carga_reg[490,490]
            LET campo_44 = carga_reg[491,500]
            LET campo_45 = carga_reg[501,516]
            LET campo_46 = carga_reg[517,517]
            LET campo_47 = carga_reg[518,525]
            LET campo_48 = carga_reg[526,527]
            LET campo_49 = carga_reg[528,529]
            LET campo_50 = carga_reg[530,531]
            LET campo_51 = carga_reg[532,533]
            LET campo_52 = carga_reg[534,541]
            LET campo_53 = carga_reg[542,547]
            LET campo_54 = carga_reg[548,554]

      	    INSERT INTO safre_tmp:detalle2 
            VALUES (campo_01 ,
                    campo_02 ,
                    campo_03 ,
                    campo_04 ,
                    campo_05 ,
                    campo_06 ,
                    campo_07 ,
                    campo_08 ,
                    campo_09 ,
                    campo_10 ,
                    campo_11 ,
                    campo_12 ,
                    campo_13 ,
                    campo_14 ,
                    campo_15 ,
                    campo_16 ,
                    campo_17 ,
                    campo_18 ,
                    campo_19 ,
                    campo_20 ,
                    campo_21 ,
                    campo_22 ,
                    campo_23 ,
                    campo_24 ,
                    campo_25 ,
                    campo_26 ,
                    campo_27 ,
                    campo_28 ,
                    campo_29 ,
                    campo_30 ,
                    campo_31 ,
                    campo_32 ,
                    campo_33 ,
                    campo_34 ,
                    campo_35 ,
                    campo_36 ,
                    campo_37 ,
                    campo_38 ,
                    campo_39 ,
                    campo_40 ,
                    campo_41 ,
                    campo_42 ,
                    campo_43 ,
                    campo_44 ,
                    campo_45 ,
                    #     campo_46 ,
                    campo_47 ,
                    campo_48 ,
                    campo_49 ,
                    campo_50 ,
                    campo_51 ,
                    campo_52 ,
                    campo_53 ,
                    campo_54 
                    )
        END IF

        IF cont_reg = total_reg THEN
            LET campo_201 = carga_reg[001,002]
            LET campo_202 = carga_reg[003,004]
            LET campo_203 = carga_reg[005,007]
            LET campo_204 = carga_reg[008,015]
            LET campo_205 = carga_reg[016,018]
            LET campo_206 = carga_reg[019,020]
            LET campo_207 = carga_reg[021,022]
            LET campo_208 = carga_reg[023,031]
            LET campo_209 = carga_reg[032,040]

      	    INSERT INTO safre_tmp:sumario2 
            VALUES (campo_201 ,
                    campo_202 ,
                    campo_203 ,
                    campo_204 ,
                    campo_205 ,
                    campo_206 ,
                    campo_207 ,
                    campo_208 ,
                    campo_209
                    )
        END IF

    END FOREACH

END FUNCTION

FUNCTION revisa_datos()
#-----------------------

    DEFINE
        v_fecha2    CHAR(10)

    DEFINE 
        rechazo_lote	 CHAR(3) ,
        rechazo_deta	 CHAR(3) ,
        l_reg		 RECORD LIKE tab_rch_lote.* ,
        x_reg		 RECORD LIKE tab_rdeta.* ,
        aux_pausa	 CHAR(1)

    DEFINE 
        rechazo_09	CHAR(02),
        rechazo_001 	CHAR(02),
        rechazo_002 	CHAR(02),
        rechazo_003 	CHAR(02)

    DEFINE 
        uno  CHAR(3),
        dos  CHAR(3),
        tre  CHAR(3),
        cua  CHAR(3),
        cin  CHAR(3),
        l_status_int	SMALLINT

    DEFINE 
        aux_status_interno SMALLINT

    # ENCABEZADO #
    SELECT campo1       ,
           campo2       ,
           campo3       ,
           campo18 
    INTO   rechazo_001  ,
           rechazo_002  ,
           rechazo_003  ,
           rechazo_lote 
    FROM safre_tmp:encabezado2

    SELECT * 
    INTO   l_reg.* 
    FROM   tab_rch_lote
    WHERE  rlote_cod = rechazo_lote

    IF STATUS  <> NOTFOUND THEN
		IF bnd_proceso THEN
        DISPLAY "Program stopped, ERROR DE PROCESO, NO PUEDE CONTINUAR " 
        EXIT PROGRAM
      ELSE
        CLEAR SCREEN
        DISPLAY l_reg.rlote_cod AT 10,1
        DISPLAY l_reg.rlote_desc_c AT 11,1
        PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR " FOR aux_pausa
        EXIT PROGRAM
      END IF
    END IF

    IF rechazo_001 <> "01" THEN
		IF bnd_proceso THEN
        DISPLAY "Program stopped, Tipo registro de ser 01 en encabezado"
        EXIT PROGRAM
      ELSE
        CLEAR SCREEN
        DISPLAY "Tipo de registro debe ser 01 en ENCABEZADO" AT 10,1
        PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
      END IF
    END IF

    IF rechazo_002 <> "01" THEN
      IF bnd_proceso THEN
        DISPLAY "Program stopped, Identificador servicio de ser 01 en encabezado"
        EXIT PROGRAM
      ELSE
       CLEAR SCREEN
       DISPLAY "Identificador de servicio ser 01 en ENCABEZADO" AT 10,1
       PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR" FOR aux_pausa
       EXIT PROGRAM
      END IF
    END IF

    IF rechazo_003 <> "10" THEN
		IF bnd_proceso THEN
        DISPLAY "Program stopped, Identificador operacion de ser 10 en encabezado"
        EXIT PROGRAM
      ELSE
       CLEAR SCREEN
       DISPLAY "Identificador de operacion ser 10 en ENCABEZADO" AT 10,1
       PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR" FOR aux_pausa
       EXIT PROGRAM
     END IF
    END IF

    # SUMARIO #

    SELECT campo1 
    INTO   rechazo_09 
    FROM   safre_tmp:sumario2

    IF rechazo_09 <> "09" THEN
		IF bnd_proceso THEN
        DISPLAY "Program stopped, Tipo registro de ser 10 en encabezado"
        EXIT PROGRAM
      ELSE
        CLEAR SCREEN
        DISPLAY "Tipo de registro debe ser 09 en RESUMEN" AT 10,1
        PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
      END IF
    END IF

    # DETALLE #

    DECLARE cursor_2 CURSOR FOR 
    SELECT * 
    FROM   safre_tmp:detalle2

    FOREACH cursor_2 INTO reg_det.*

    SELECT a.n_folio INTO   reg_det.folio_solicitud
    FROM   afi_solicitud a
    WHERE  a.n_seguro = reg_det.nss_solicitud
    AND    a.tipo_solicitud = 7
    AND    a.status_interno = 30

        IF reg_det.fec_emision_certif IS NOT NULL THEN 
            LET x_fecha = reg_det.fec_emision_certif[5,6],"/",
                          reg_det.fec_emision_certif[7,8],"/",
                          reg_det.fec_emision_certif[1,4]
        ELSE
            LET x_fecha = NULL
        END IF 

        IF reg_det.falta_actual IS NOT NULL THEN 
            LET f1 = reg_det.falta_actual[5,6],"/",
                     reg_det.falta_actual[7,8],"/",
                     reg_det.falta_actual[1,4]
        ELSE
            LET f1 = NULL
        END IF 
        
        IF reg_det.fprimer_afil IS NOT NULL THEN 
            LET f2 = reg_det.fprimer_afil[5,6],"/",
                     reg_det.fprimer_afil[7,8],"/",
                     reg_det.fprimer_afil[1,4]
        ELSE
            LET f2 = NULL
        END IF 

        IF reg_det.fena_bd IS NOT NULL THEN 
            LET f3 = reg_det.fena_bd[5,6],"/",
                     reg_det.fena_bd[7,8],"/",
                     reg_det.fena_bd[1,4]
        ELSE
            LET f3 = NULL
        END IF 

        LET falta  = f1
        LET fpafil = f2
        LET fnbd   = f3
        LET xx_fecha = x_fecha

        IF reg_det.salario IS NULL THEN
            LET reg_det.salario = 0
        END IF

        LET reg_det.salario = reg_det.salario / 100

        IF reg_det.periodo IS NOT NULL THEN
            LET c_periodo = reg_det.periodo[5,6], '/01/', reg_det.periodo[1,4]
            LET f_periodo = c_periodo
        ELSE
            LET f_periodo = NULL
        END IF   

        LET diag = reg_det.diag_proceso[1,3]

        UPDATE afi_solicitud 
        SET    fentcons       = xx_fecha,
               status         = reg_det.cod_operacion,
               status_captura = diag
        WHERE  n_seguro       = reg_det.nss_solicitud
        AND    n_folio        = reg_det.folio_solicitud
        AND    tipo_solicitud in(7)
        AND    status_interno in(30)

        LET sello = reg_det.fecha_recep_sello ,
                    reg_det.hora_recep_sello ,
                    reg_det.minuto_recep_sello ,
                    reg_det.seg_recep_sello ,
                    reg_det.cseg_recep_sello,
                    reg_det.consecutivo_recep 

# 29-11-2004
        CASE reg_det.cod_operacion
            WHEN '05'
                CALL cod_op05()
            WHEN '02'
                CALL cod_op02()
            WHEN '03'
                CALL cod_op03()
        END CASE

    IF reg_det.cod_operacion = '02' THEN
	LET uno = reg_det.diag_proceso[1,3]
	LET dos = reg_det.diag_proceso[4,6]
	LET tre = reg_det.diag_proceso[7,9]
	LET cua = reg_det.diag_proceso[10,12]
	LET cin = reg_det.diag_proceso[13,15]

	SELECT * 
        INTO   x_reg.* 
        FROM   tab_rdeta
	WHERE  rdeta_cod = uno
        AND    modulo_cod = 'taa'

	IF STATUS <> NOTFOUND THEN
	    CALL Inserta_datos_en_tabla_errores(reg_det.*,x_reg.*)
	    CONTINUE FOREACH
	END IF

	SELECT * 
        INTO   x_reg.* 
        FROM   tab_rdeta
	WHERE  rdeta_cod = dos
        AND    modulo_cod = 'taa'

	IF STATUS <> NOTFOUND THEN
	    CALL Inserta_datos_en_tabla_errores(reg_det.*,x_reg.*)
	    CONTINUE FOREACH
	END IF

	SELECT * 
        INTO   x_reg.* 
        FROM   tab_rdeta
	WHERE  rdeta_cod = tre
        AND    modulo_cod = 'taa'

        IF STATUS <> NOTFOUND THEN
	    CALL Inserta_datos_en_tabla_errores(reg_det.*,x_reg.*)
	    CONTINUE FOREACH
	END IF

	SELECT * 
        INTO   x_reg.* 
        FROM   tab_rdeta
	WHERE  rdeta_cod = cua
        AND    modulo_cod = 'taa'

	IF STATUS <> NOTFOUND THEN
	    CALL Inserta_datos_en_tabla_errores(reg_det.*,x_reg.*)
	    CONTINUE FOREACH
	END IF

	SELECT * 
        INTO   x_reg.* 
        FROM   tab_rdeta
	WHERE  rdeta_cod = cin
        AND    modulo_cod = 'taa'

	IF STATUS <> NOTFOUND THEN
	    CALL Inserta_datos_en_tabla_errores(reg_det.*,x_reg.*)
	    CONTINUE FOREACH
	END IF

	IF reg_det.tipo_registro <> "02" THEN
	    LET x_reg.rdeta_cod = 1
	    CALL Inserta_datos_en_tabla_errores(reg_det.*,x_reg.*)
	    CONTINUE FOREACH
	END IF

	IF reg_det.clave_operacion <> "01" THEN
	    LET x_reg.rdeta_cod = 3
	    CALL Inserta_datos_en_tabla_errores(reg_det.*,x_reg.*)
	    CONTINUE FOREACH
	END IF
    END IF 


    END FOREACH

END FUNCTION

FUNCTION cod_op05()
#co5---------------

    UPDATE afi_solicitud 
    SET    status_interno     = 65, # -- CPL-1200 Se modifica el estatus interno para que ser aperturada por el porgrama Registro
           fol_prob           = reg_det.fol_prob,
           doc_prob           = reg_det.doc_prob,
           salario_actual     = reg_det.salario,
           fecha_actualiza_sa = f_periodo,
           sello_electronico  = sello ,
           fecha_1a_afil      = fpafil
    WHERE  n_seguro = reg_det.nss_solicitud
    AND    n_folio  = reg_det.folio_solicitud
    AND    tipo_solicitud in(7)
    AND    status_interno in(30)

    LET aceptar = aceptar + 1

  --  CALL inserta_afi_ctr(70)

END FUNCTION

FUNCTION cod_op02()
#co2---------------

    UPDATE afi_solicitud 
    SET    status_interno = 40
    WHERE  n_seguro = reg_det.nss_solicitud
    AND    n_folio  = reg_det.folio_solicitud
    AND    tipo_solicitud in(7)
    AND    status_interno in(30)

    LET rechazar = rechazar + 1

   -- CALL inserta_afi_ctr(40)

    LET reg_carta.docto_cod = 30219
    LET reg_carta.opera_cod = 'P'

    IF diag <> '970' THEN
       -- CALL det_carta() #dc
    END IF

END FUNCTION

FUNCTION cod_op03()
#co3---------------

    UPDATE afi_solicitud
    SET    status_interno = 50,
           fol_prob = reg_det.fol_prob,
           doc_prob = reg_det.doc_prob,
           sello_electronico = sello,
           fecha_1a_afil = fpafil
    WHERE  n_seguro = reg_det.nss_solicitud
    AND    n_folio = reg_det.folio_solicitud
    AND    tipo_solicitud in(7)
    AND    status_interno in(30)

    LET pendiente = pendiente + 1

  --  CALL inserta_afi_ctr(50)

    LET diagnostico = reg_det.diag_proceso[1,3]
 
    CASE diagnostico
        WHEN '004' LET observacion = 'NSS EN PROCESO DE RETIRO'
        WHEN '005' LET observacion = 'NSS EN PROCESO DE TRASPASO'
        WHEN '006' LET observacion = 'NSS EN PROCESO DEV PAGOS EXC'
        WHEN '008' LET observacion = 'NSS CON DIFERENCIA EN NOMBRE'
        WHEN '009' LET observacion = 'PROCESO INTERESES 43 BIS'
	WHEN '010' LET observacion = 'PENDIENTE RESPUESTA RENAPO'
    END CASE

    LET v_fecha2 = reg_det.fec_emision_certif[05,06],"/",
                   reg_det.fec_emision_certif[07,08],"/",
                   reg_det.fec_emision_certif[01,04]   

    INSERT INTO afi_rechaza_cert 
    VALUES (reg_det.folio_solicitud,
            2,
            reg_det.nss_solicitud,
            diagnostico,
            v_fecha2,
            "",
            "",
            observacion,
            reg_det.nombre_pcanase
            )

    LET reg_carta.docto_cod = 30227
    LET reg_carta.opera_cod = 'P'

   -- CALL det_carta() #dc

END FUNCTION
{
FUNCTION inserta_afi_ctr(status_interno)
#iac----------------------

    DEFINE fecha_envio    DATE
    DEFINE status_interno SMALLINT
    DEFINE ind_envio      SMALLINT
    DEFINE paterno        CHAR(40)
    DEFINE materno        CHAR(40)
    DEFINE nombres        CHAR(40)
    DEFINE nom_comp       CHAR(50)
    DEFINE ind_nombre     SMALLINT

    LET ind_nombre = 0

    SELECT MAX(p.ind_envio)
    INTO   ind_envio
    FROM   afi_ctr_solicitud p
    WHERE  p.n_seguro       = reg_det.nss_solicitud
    AND    p.n_folio        = reg_det.folio_solicitud
    AND    p.tipo_solicitud = 7

    SELECT p.fecha_envio, p.paterno, p.materno, p.nombres
    INTO   fecha_envio, paterno, materno, nombres
    FROM   afi_solicitud p
    WHERE  p.n_seguro       = reg_det.nss_solicitud
    AND    p.n_folio        = reg_det.folio_solicitud
    AND    p.tipo_solicitud = 7

    LET nom_comp               = paterno CLIPPED, "$",
		                 materno CLIPPED, "$",
		                 nombres CLIPPED

    LET reg_det.nombre_pcanase = reg_det.nombre_pcanase CLIPPED
    LET nom_comp               = nom_comp CLIPPED

    IF nom_comp <> reg_det.nombre_pcanase THEN
       LET ind_nombre = 1
    END IF

    IF fecha_envio IS NULL THEN
       LET fecha_envio = today
    END IF

    INSERT INTO afi_ctr_solicitud
    VALUES(2                           ,
           reg_det.folio_solicitud     ,
           reg_det.nss_solicitud       ,
	   nom_comp                    ,
	   status_interno              ,
           fecha_envio                 ,
           today                       ,
           reg_det.cve_afo_ced         ,
           reg_det.ind_nss_modificado  ,
           x_fecha                     ,
           reg_det.curp_oficial        ,
           reg_det.ind_curp_modif      ,
           reg_det.n_rfc_bd            ,
           reg_det.nombre_bd           ,
           reg_det.nombre_pcanase      ,
           fnbd                        ,
           reg_det.codven_bd           ,
           reg_det.sexo_bd             ,
           reg_det.estadon_bd          ,
           fpafil                      ,
           falta                       ,
           reg_det.cve_afore           ,
           reg_det.nacionalidad_bd     ,
           reg_det.tip_prob_bd         ,
           reg_det.fol_prob_bd         ,
           reg_det.doc_prob_bd         , 
           ind_envio                   ,
	   ind_nombre                  ,
	   reg_det.cod_operacion       ,
           reg_det.diag_proceso        ,
	   g_usuario                   ,
	   today                       )

END FUNCTION
}
FUNCTION Inserta_datos_en_tabla_errores(reg_det,x_reg)
#-----------------------------------------------------

    DEFINE reg_det RECORD
        tipo_registro         CHAR(02)     ,
        contador_servicio     CHAR(10)     ,      
        clave_operacion       CHAR(02)     ,
        nss_solicitud         CHAR(11)     ,
        curp_solicitud        CHAR(18)     ,
        rfc_trabajador        CHAR(13)     ,
        paterno               CHAR(40)     ,
        materno               CHAR(40)     ,
        nombres               CHAR(40)     ,
        fecha_nacimiento      CHAR(08)     ,
        clave_promotor        CHAR(10)     ,
        fec_recp_sol_afore    CHAR(08)     ,
        folio_solicitud       DECIMAL(16,0),
        sexo                  SMALLINT     ,
        entidad_nacimiento    CHAR(02)     ,
        ind_infonavit         CHAR(01)     ,
        nacionalidad          CHAR(03)     ,
        tip_prob              CHAR(01)     ,
        fol_prob              CHAR(10)     ,
        doc_prob              CHAR(16)     ,
        cod_err_ori           CHAR(02)     ,
        cve_afo_ced           CHAR(03)     ,
        folio_edo_cta         CHAR(08)     ,
        cod_operacion         CHAR(02)     ,
        diag_proceso          CHAR(15)     ,
        ident_lote_origen     CHAR(16)     ,
        nss_oficial           CHAR(11)     ,
        ind_nss_modificado    CHAR(01)     ,
        fec_emision_certif    CHAR(08)     ,
        curp_oficial          CHAR(18)     ,
        ind_curp_modif        CHAR(01)     ,
        n_rfc_bd              CHAR(13)     ,
        nombre_bd             CHAR(50)     ,
        nombre_pcanase        CHAR(50)     ,
        fena_bd               CHAR(08)     ,
        codven_bd             CHAR(10)     ,
        sexo_bd               CHAR(01)     ,
        estadon_bd            CHAR(02)     ,
        fprimer_afil          CHAR(08)     ,
        falta_actual          CHAR(08)     ,
        cve_afore             CHAR(03)     ,
        nacionalidad_bd       CHAR(03)     ,
        tip_prob_bd           CHAR(01)     ,
        fol_prob_bd           CHAR(10)     ,
        doc_prob_bd           CHAR(16)     ,
        fecha_recep_sello     CHAR(08)     ,
        hora_recep_sello      CHAR(02)     ,
        minuto_recep_sello    CHAR(02)     ,
        seg_recep_sello       CHAR(02)     ,
        cseg_recep_sello      CHAR(02)     ,
        consecutivo_recep     CHAR(08)     ,
        periodo               CHAR(6)      ,
        salario               DECIMAL(9,2)
    END RECORD

    DEFINE x_reg RECORD LIKE tab_rdeta.*
    DEFINE v_fecha  CHAR(10)
    DEFINE v_fecha1 CHAR(10)

    LET v_fecha = reg_det.fec_emision_certif[05,06],"/",
                  reg_det.fec_emision_certif[07,08],"/",
                  reg_det.fec_emision_certif[01,04]

    LET v_fecha1 = reg_det.falta_actual[05,06],"/",
                   reg_det.falta_actual[07,08],"/",
                   reg_det.falta_actual[01,04]

    SELECT "X" 
    FROM   afi_rechaza_cert
    WHERE  n_seguro  = reg_det.nss_solicitud
    AND    rdeta_cod = x_reg.rdeta_cod
    AND    f_rechazo = v_fecha

    IF STATUS = NOTFOUND THEN
        IF x_reg.rdeta_cod = 14 THEN
	    INSERT INTO afi_rechaza_cert 
            VALUES (reg_det.folio_solicitud,2,
                    reg_det.nss_solicitud,
                    x_reg.rdeta_cod,
                    v_fecha,
                    v_fecha1,
                    reg_det.cve_afore,
                    x_reg.rdeta_desc_c,
                    reg_det.nombre_pcanase)
        ELSE
	    INSERT INTO afi_rechaza_cert 
            VALUES (reg_det.folio_solicitud,2,
                    reg_det.nss_solicitud,
                    x_reg.rdeta_cod,
                    v_fecha,
                    " ",
                    " ",
                    x_reg.rdeta_desc_c,
                    reg_det.nombre_pcanase)
        END IF
    END IF

END FUNCTION
----------------------------------------------------------ini_miguel
FUNCTION reporte()
    
  LET hora = TIME

  LET G_IMPRE = g_paramgrales.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
              ".LIS_",hoy USING "DD-MM-YYYY",
               "_",hora CLIPPED

  START REPORT rpt_detalle2_imp TO  G_IMPRE

  OUTPUT TO REPORT rpt_detalle2_imp(detalle2_1.*,detalle2_2.*,detalle2_3.*)

  FINISH REPORT rpt_detalle2_imp

  LET gimpresion = "lp ",G_IMPRE
  RUN gimpresion
END FUNCTION

REPORT rpt_detalle2_imp(detalle2_1,detalle2_2,detalle2_3)
  DEFINE  detalle2_1           RECORD
           total                 SMALLINT,
           cve_afo_ced           SMALLINT,
           cod_operacion         CHAR(2),
           descripcion           CHAR(10)
  END RECORD

  DEFINE  detalle2_2           RECORD
           total_1               SMALLINT,
           cod_operacion_1       CHAR(2),
           descripcion_1         CHAR(10)
  END RECORD

  DEFINE  detalle2_3           RECORD
           total_2               SMALLINT
  END RECORD
  
  OUTPUT
    TOP MARGIN 1
    BOTTOM MARGIN 0
    LEFT MARGIN 0
    RIGHT MARGIN 0
    PAGE LENGTH 60

  FORMAT
   PAGE HEADER
      PRINT COLUMN 01,"HSBC AFORE",
            COLUMN 68,TODAY USING "DD-MM-YYYY"
      SKIP 2 LINE
      PRINT COLUMN 07,"-------------------------------------------------------------------"
      PRINT COLUMN 08,"RESPUESTA DE CERTIFICACION DE SOLICITUDES DE AFILIADOS POR TRASPASO"
      PRINT COLUMN 07,"-------------------------------------------------------------------"
      SKIP 2 LINE 

   ON EVERY ROW
      PRINT COLUMN 05,"TOTAL DE REGISTROS",
            COLUMN 25,"CLAVE DE LA AFORE",
            COLUMN 45,"DESCRIPCION DE LA OPERACION"
      SKIP 1 LINE

      DECLARE cursor_3           CURSOR FOR
      SELECT COUNT(*),
             cve_afo_ced,
             cod_operacion
      FROM   safre_tmp:detalle2
      GROUP BY 2,3
      ORDER BY 2,3
      FOREACH cursor_3 INTO detalle2_1.total,
                          detalle2_1.cve_afo_ced

       CASE detalle2_1.cod_operacion 
           WHEN "05" 
               LET detalle2_1.descripcion = "ACEPTADO"
           WHEN "02" 
               LET detalle2_1.descripcion = "RECHAZADO"
           WHEN "03" 
               LET detalle2_1.descripcion = "PENDIENTE"
       END CASE

       PRINT COLUMN 04,detalle2_1.total,
             COLUMN 28,detalle2_1.cve_afo_ced,
             COLUMN 48,detalle2_1.descripcion
      END FOREACH

      PRINT COLUMN 05,"TOTAL DE REGISTROS",
            COLUMN 25,"DESCRIPCION DE LA OPERACION"
      SKIP 1 LINE

      DECLARE cursor_4            CURSOR FOR
      SELECT COUNT(*),
             cod_operacion
      FROM   safre_tmp:detalle2
      GROUP BY 2
      ORDER BY 2
      FOREACH cursor_4 INTO detalle2_2.total_1
       CASE detalle2_2.cod_operacion_1
           WHEN "05" 
               LET detalle2_2.descripcion_1 = "ACEPTADO"
           WHEN "02" 
               LET detalle2_2.descripcion_1 = "RECHAZADO"
           WHEN "03" 
               LET detalle2_2.descripcion_1 = "PENDIENTE"
       END CASE

        PRINT COLUMN 04,detalle2_2.total_1,
              COLUMN 25,detalle2_2.descripcion_1
      END FOREACH

      PRINT COLUMN 05,"TOTAL DE REGISTROS"
      SKIP 1 LINE

      DECLARE cursor_5            CURSOR FOR
      SELECT COUNT(*)
      FROM   safre_tmp:detalle2
      FOREACH cursor_5 INTO detalle2_3.total_2
         PRINT COLUMN 04,detalle2_3.total_2
      END FOREACH
  
   PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
          PAUSE "Presione enter para continuar...."
END REPORT
----------------------------------------------------------fin_miguel

FUNCTION despliega_resultados()
#dr----------------------------

    DEFINE total_resp SMALLINT

    LET total_resp = 0
    LET total_resp = aceptar + rechazar + pendiente + aclaracion

  IF bnd_proceso THEN
    DISPLAY "                 TOTAL REGISTROS RECIBIDOS                                     " 

    DISPLAY "Total de Registros del lote     : ",
            total_resp USING "#######&" 

    DISPLAY "Registros certificados  : ",
            aceptar USING "#######&" 

    DISPLAY "Registros rechazados    : ",
            rechazar USING "#######&" 

    DISPLAY "Registros pendientes    : ",
            pendiente USING "#######&" 

    DISPLAY "Registros en aclaracion : ",
            aclaracion USING "#######&" 
  ELSE
    DISPLAY "                 TOTAL REGISTROS RECIBIDOS                                     " AT 10,1 ATTRIBUTE ( REVERSE )

    DISPLAY "Total de Registros del lote     : ",
            total_resp USING "#######&" AT 11,15 ATTRIBUTE ( BOLD )

    DISPLAY "Registros certificados  : ",
            aceptar USING "#######&" AT 12,15 ATTRIBUTE ( BOLD )

    DISPLAY "Registros rechazados    : ",
            rechazar USING "#######&" AT 13,15 ATTRIBUTE ( BOLD )

    DISPLAY "Registros pendientes    : ",
            pendiente USING "#######&" AT 14,15 ATTRIBUTE ( BOLD )   

    DISPLAY "Registros en aclaracion : ",
            aclaracion USING "#######&" AT 15,15 ATTRIBUTE ( BOLD )

    PROMPT "Presione <enter> para continuar " FOR enter
  END IF

END FUNCTION     

FUNCTION actualiza_operacion()
#ao---------------------------

    UPDATE bat_ctr_operacion
    SET    estado_operacion = 4,
           fecha_fin        = CURRENT,
           nombre_archivo   = nom_afi
    WHERE  pid              = reg_bat.pid
    AND    proceso_cod      = reg_bat.proceso_cod
    AND    opera_cod        = reg_bat.opera_cod

    UPDATE bat_ctr_proceso
    SET    estado_proceso = 4,
           fecha_fin      = CURRENT
    WHERE  pid            = reg_bat.pid
    AND    proceso_cod    = reg_bat.proceso_cod

END FUNCTION      
{
FUNCTION det_carta()
#dc-----------------

    SELECT @tipo_solicitud
      INTO reg_carta.tipo_solicitud
      FROM afi_solicitud
     WHERE @n_seguro = reg_det.nss_solicitud
       AND @n_folio  = reg_det.folio_solicitud

    LET reg_carta.nss            = reg_det.nss_solicitud 
    LET reg_carta.n_folio        = reg_det.folio_solicitud
    LET reg_carta.fecha_registro = xx_fecha
    LET reg_carta.edo_genera     = 10
    LET reg_carta.fecha_genera   = TODAY
    LET reg_carta.hora_genera    = TIME
    LET reg_carta.lote_genera    = 0
    LET reg_carta.consecutivo    = 0
    LET reg_carta.id_sepomex     = 0

    LET consulta_carta = "EXECUTE PROCEDURE inserta_carta (?,?,?,?,?,?,",
        		 "?,?,?,?,?,?)"

    PREPARE sql_exe FROM consulta_carta
    EXECUTE sql_exe USING reg_carta.*

    INITIALIZE reg_carta.* TO NULL

END FUNCTION
}
######################################################################
