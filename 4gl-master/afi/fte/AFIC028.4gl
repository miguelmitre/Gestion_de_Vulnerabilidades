###########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                        #
#Propietario       => E.F.P.                                              #
#Programa AFIC028  => CARGA DE ARCHIVO GENERADO POR OPERADORA BDNSAR      #
#Sistema           => AFI.                                                #
#Autor             => FERNANDO HERRERA HERNANDEZ                          #
#Fecha             => 06 DE ABRIL DE 2006 (Circ 7 -12)                    #
#Actualizacion     => FERNANDO HERRERA HERNANDEZ                          #
#Fecha             => 08 DE MARZO DE 2008 (MULTISIEFORES)                 #
###########################################################################
# CPL-2139         -  Mar 2016  ERM Siefore Basica cero y portabilidad    #
#CPL-2372          => FSR 02/08/2016 SE COLOCA TODAY A FINICTA ACEPTADOS  #
###########################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_reg RECORD
        generar CHAR(20)
    END RECORD

    DEFINE hoy               DATE
    DEFINE xx_fecha          DATE
    DEFINE diaSig            DATE
    DEFINE enter             CHAR(1)
    DEFINE aux_pausa         CHAR(1)
    DEFINE sello             CHAR(24)
    DEFINE varchivo          CHAR(40)
    DEFINE carga             CHAR(100)
    DEFINE ejecuta           CHAR(100)
    DEFINE corr              CHAR(100)
    DEFINE total_reg         SMALLINT
    DEFINE g_plano_ind3      SMALLINT
    DEFINE aceptar           SMALLINT
    DEFINE rechazar          SMALLINT
    DEFINE pendiente         SMALLINT
    DEFINE aclaracion        SMALLINT
    DEFINE asignar           SMALLINT
    DEFINE traspasar         SMALLINT
    DEFINE pend_90           SMALLINT
    DEFINE g_paramgrales     RECORD LIKE seg_modulo.*
    DEFINE vgenerar          CHAR(20)

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
        --fec_recp_sol_afore   CHAR(08)     , CPL-2139
        folio_solicitud      DECIMAL(16,0),
        sexo                 SMALLINT     ,
        entidad_nacimiento   CHAR(02)     ,
        tipo_admin           CHAR(02)     ,
        curp_renapo          CHAR(18)     ,
        nss_asoc             CHAR(11)     ,
        cve_afo_nss          CHAR(03)     ,
        tipo_trab            CHAR(01)     ,
        cod_operacion        CHAR(02)     ,
        diag_proceso         CHAR(15)     ,
        ident_lote_origen    CHAR(16)     ,
        depos                CHAR(02)     ,
        fprimer_afil         CHAR(08)     ,
        falta_actual         CHAR(08)     ,
        cve_afore            CHAR(03)     ,
        fecha_recep_sello    CHAR(08)     ,
        hora_recep_sello     CHAR(02)     ,
        minuto_recep_sello   CHAR(02)     ,
        seg_recep_sello      CHAR(02)     ,
        cseg_recep_sello     CHAR(02)     ,
        consecutivo_recep    CHAR(08)     ,
        ind_sie_cero         SMALLINT     ,
        ind_portab           SMALLINT
    END RECORD 

    DEFINE
        x_fecha,f1,f2,f3     CHAR(10),
        falta,fpafil,fnbd    DATE

    DEFINE reg_bat RECORD
        pid                  INTEGER,
        proceso_cod          INTEGER,
        opera_cod            INTEGER,
        nombre_archivo       CHAR(25)
    END RECORD

    DEFINE 
        bnd_proceso          SMALLINT,
        aux_status_interno   SMALLINT,
        cuantos              INTEGER

    DEFINE
        c_periodo            CHAR(10),
        f_periodo            DATE

    DEFINE 
        reg_carta            RECORD LIKE safre_af:int_ctr_carta.*,
        consulta_carta       CHAR(120)       

    DEFINE
	longitud	     SMALLINT,
	i		     SMALLINT,
	i2		     SMALLINT,
	archb		     SMALLINT,
	g_usuario            CHAR(08),
        gv_cod_op            CHAR(2),
        gv_ind_op            SMALLINT,
        con_curp             SMALLINT,
        sin_curp             SMALLINT

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG(FGL_GETENV("USER")||".AFIC028.log")     
    CALL inicio() #i

    IF NOT bnd_proceso THEN
        DEFER INTERRUPT
        OPTIONS INPUT WRAP,
            PROMPT LINE LAST,
            ACCEPT KEY CONTROL-I
        CALL proceso_principal()   #pp
    ELSE
        CALL sube_archivo()
        CALL rescata_valores()     #rv
        CALL actualiza_bat_f(0) #rv
    END IF

END MAIN 

FUNCTION inicio()
#----------------

    LET reg_bat.pid            = ARG_VAL(1)
    LET reg_bat.proceso_cod    = ARG_VAL(2)
    LET reg_bat.opera_cod      = ARG_VAL(3)
    LET reg_bat.nombre_archivo = ARG_VAL(4)

    LET bnd_proceso = 0

    IF reg_bat.pid THEN
       DISPLAY "INICIANDO PROCESO ..."
       LET bnd_proceso = 1
    END IF

    LET hoy        = TODAY
    LET aceptar    = 0
    LET rechazar   = 0
    LET pendiente  = 0
    LET aclaracion = 0
    LET traspasar  = 0
    LET pend_90    = 0
    LET archb      = 0

    SELECT *, USER
    INTO   g_paramgrales.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    WHENEVER ERROR CONTINUE
      DATABASE safre_tmp
      DROP TABLE safre_tmp:plano_ind3

      CREATE TABLE safre_tmp:plano_ind3
      (n_registros  CHAR(570))
    WHENEVER ERROR STOP

    DATABASE safre_af

    INITIALIZE reg_carta.* TO NULL

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIC0011" ATTRIBUTE(BORDER)
    DISPLAY " AFIC028   CARGA ARCHIVO LIBERACION PENDIENTES NO AFILIADOS                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    DISPLAY g_paramgrales.ruta_rescate AT 7,10

    INPUT BY NAME g_reg.generar
      AFTER FIELD generar
        IF g_reg.generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
	ELSE
	    LET vgenerar = g_reg.generar
        END IF
        
        LET longitud = LENGTH(g_reg.generar CLIPPED)
        FOR i = 1 TO longitud
           IF g_reg.generar[i] = "." THEN
              LET i  = i + 1
	      LET i2 = i + 5 
              IF g_reg.generar[i,i2] = 'libind' THEN
		 LET g_reg.generar = g_reg.generar[i,i2]
		 LET archb = 1
                 EXIT FOR
              ELSE 
		 PROMPT "El archivo no es de -Lib Pendientes ",
                        "No Afiliados-. <ENTER> para continuar." FOR enter
		 LET archb = 0
		 NEXT FIELD generar 
	      END IF
           END IF
	END FOR

        SELECT nombre_archivo
        INTO   varchivo
        FROM   afi_ctr_arh_reg
        WHERE  nombre_archivo = vgenerar

        IF STATUS <> NOTFOUND THEN
            ERROR "ARCHIVO YA PROCESADO"
            SLEEP 1
            ERROR " "
            INITIALIZE g_reg.generar TO NULL
            INITIALIZE vgenerar      TO NULL
            CLEAR FORM
            NEXT FIELD generar
        END IF

        IF archb = 1 THEN

        LET carga = NULL
        LET carga = g_paramgrales.ruta_rescate CLIPPED,"/",vgenerar CLIPPED

        WHENEVER ERROR CONTINUE
          LOAD FROM carga INSERT INTO safre_tmp:plano_ind3
        WHENEVER ERROR STOP

        SELECT COUNT(*)
        INTO   g_plano_ind3
        FROM   safre_tmp:plano_ind3

        IF g_plano_ind3 IS NULL OR g_plano_ind3 = 0 THEN
           ERROR "NOMBRE DE ARCHIVO INCORRECTO O VACIO, REVISE."
           SLEEP 5
           EXIT PROGRAM
        END IF

        ERROR "Procesando Informacion"
        CALL rescata_valores()

        --LET ejecuta = "fglgo AFIL003 DH" CLIPPED
        --RUN ejecuta

        EXIT PROGRAM

	ELSE
	  PROMPT "El archivo no es de -Liberacion de Pendientes-.",
	  " <ENTER> para continuar." FOR enter
	  LET archb = 0
	  NEXT FIELD generar 
	END IF
    END INPUT

END FUNCTION

FUNCTION rescata_valores()
#rv-----------------------

    CALL crea_tablas()
    CALL actualiza_datos()
    CALL revisa_datos()
    CALL despliega_resultados()
    CALL actualiza_est()
    CALL apertura_cuenta()

END FUNCTION

FUNCTION sube_archivo()
#sa-------------------

    LET carga = g_paramgrales.ruta_rescate CLIPPED,"/",
                           reg_bat.nombre_archivo CLIPPED

    SELECT nombre_archivo
    INTO   varchivo
    FROM   afi_ctr_arh_reg
    WHERE  nombre_archivo = reg_bat.nombre_archivo

    IF STATUS <> NOTFOUND THEN
        DISPLAY "Program stopped, ARCHIVO YA PROCESADO"
        EXIT PROGRAM
    END IF

    LOAD FROM carga INSERT INTO safre_tmp:plano_ind3

    SELECT count(*)
    INTO   cuantos
    FROM   safre_tmp:plano_ind3

    IF cuantos = 0 OR
       cuantos IS NULL THEN
        DISPLAY  "Program stopped, NOMBRE DE ARCHIVO INCORRECTO O VACIO"
        EXIT PROGRAM
    END IF

END FUNCTION

FUNCTION crea_tablas()
#ct-------------------

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE encabezado_ind3
        DROP TABLE detalle_ind3
        DROP TABLE sumario_ind3
    WHENEVER ERROR STOP

    CREATE TABLE encabezado_ind3
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

    CREATE TABLE detalle_ind3
        (
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
        -- fec_recp_sol_afore   CHAR(08)     ,   PST1943
        folio_solicitud      DECIMAL(16,0),   
        sexo                 SMALLINT     ,
        entidad_nacimiento   CHAR(02)     ,
        tipo_admin           CHAR(02)     ,
        curp_renapo          CHAR(18)     ,
        nss_asoc             CHAR(11)     ,
        cve_afo_nss          CHAR(03)     ,
        tipo_trab            CHAR(01)     ,
        cod_operacion        CHAR(02)     ,
        diag_proceso         CHAR(15)     ,
        ident_lote_origen    CHAR(16)     ,
        depos                CHAR(02)     ,
        fprimer_afil         CHAR(08)     ,
        falta_actual         CHAR(08)     ,
        cve_afore            CHAR(03)     ,
        fecha_recep_sello    CHAR(08)     ,
        hora_recep_sello     CHAR(02)     ,
        minuto_recep_sello   CHAR(02)     ,
        seg_recep_sello      CHAR(02)     ,
        cseg_recep_sello     CHAR(02)     ,
        consecutivo_recep    CHAR(08)     ,
        ind_sie_cero         SMALLINT     ,
        ind_portab           SMALLINT
        )

    CREATE TABLE sumario_ind3
        (campo1  CHAR(2),
         campo2  CHAR(2),
         campo3  CHAR(3),
         campo4  CHAR(8),
         campo5  CHAR(3),
         campo6  CHAR(2),
         campo7  CHAR(2),
         campo8  CHAR(9),
         campo9  CHAR(9))

        DATABASE safre_af

END FUNCTION

FUNCTION actualiza_datos()
#-------------------------

    DEFINE
        cont_reg   SMALLINT,
        carga_reg  CHAR(570)

    DEFINE
        campo_011 CHAR(02),
        campo_012 CHAR(02),
        campo_013 CHAR(02),
        campo_014 CHAR(02),
        campo_015 CHAR(03),
        campo_016 CHAR(02),
        campo_017 CHAR(03),
        campo_018 CHAR(08),
        campo_019 CHAR(03),
        campo_110 CHAR(03),
        campo_111 CHAR(02),
        campo_112 CHAR(08),
        campo_113 CHAR(08),
        campo_114 CHAR(08),
        campo_115 CHAR(08),
        campo_116 CHAR(08),
        campo_117 CHAR(01),
        campo_118 CHAR(09),
        campo_119 CHAR(02),
        campo_210 CHAR(03),
        campo_211 CHAR(02),
        campo_212 CHAR(03),


        campo_01  CHAR(02),
        campo_02  CHAR(10),
        campo_03  CHAR(02),
        campo_04  CHAR(11),
        campo_05  CHAR(18),
        campo_06  CHAR(13),
        campo_07  CHAR(40),
        campo_08  CHAR(40),
        campo_09  CHAR(40),
        campo_10  CHAR(08),
        campo_11  CHAR(10),
        -- campo_12  CHAR(08),
        -- campo_13  CHAR(10),
        campo_14  CHAR(01),
        campo_15  CHAR(02),
        campo_16  CHAR(02),
        campo_17  CHAR(18),
        campo_18  CHAR(11),
        campo_19  CHAR(03),
        campo_20  CHAR(01),
        campo_21  CHAR(02),
        campo_22  CHAR(15),
        campo_23  CHAR(16),
        campo_24  CHAR(02),
        campo_25  CHAR(08),
        campo_26  CHAR(08),
        campo_27  CHAR(03),
        campo_28  CHAR(08),
        campo_29  CHAR(02),         
        campo_30  CHAR(02),         
        campo_31  CHAR(02),         
        campo_32  CHAR(02),         
        campo_33  CHAR(08),         
        campo_34  CHAR(01),         
        campo_35  CHAR(01),         

        campo_201 CHAR(02),
        campo_202 CHAR(02),
        campo_203 CHAR(03),
        campo_204 CHAR(08),
        campo_205 CHAR(03),
        campo_206 CHAR(02),
        campo_207 CHAR(02),
        campo_208 CHAR(09),
        campo_209 CHAR(09)

    LET cont_reg = 0

    SELECT COUNT(*)
    INTO   total_reg
    FROM   safre_tmp:plano_ind3

    DECLARE cursor_1 CURSOR FOR 
    SELECT  * 
    FROM    safre_tmp:plano_ind3

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

            INSERT INTO safre_tmp:encabezado_ind3
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
                    campo_212 )
        END IF

        IF cont_reg <> total_reg AND cont_reg <> 1 THEN
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
            -- LET campo_12 = carga_reg[195,212]  PST1943
            -- LET campo_13 = carga_reg[213,213]  PST1943
            LET campo_14 = carga_reg[213,213]
            LET campo_15 = carga_reg[214,215]
            LET campo_16 = carga_reg[216,217]
            LET campo_17 = carga_reg[218,235]
            LET campo_18 = carga_reg[236,246]
            LET campo_19 = carga_reg[247,249]
            LET campo_20 = carga_reg[250,250]
            LET campo_21 = carga_reg[262,263]
            LET campo_22 = carga_reg[264,278]
            LET campo_23 = carga_reg[279,294]
            LET campo_24 = carga_reg[295,296]
            LET campo_25 = carga_reg[468,475]
            LET campo_26 = carga_reg[476,483]
            LET campo_27 = carga_reg[484,486]
            LET campo_28 = carga_reg[518,525]
            LET campo_29 = carga_reg[526,527]
            LET campo_30 = carga_reg[528,529]
            LET campo_31 = carga_reg[530,531]
            LET campo_32 = carga_reg[532,533]
            LET campo_33 = carga_reg[534,541]
            LET campo_34 = carga_reg[542,542]   --# ind_sie_cero
            LET campo_35 = carga_reg[543,543]   --# ind_portab

        INSERT INTO safre_tmp:detalle_ind3 
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
                -- campo_12 ,   PST1943
                ''       ,      --# el folio que despues de obtiene 
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
                campo_35 )
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

            INSERT INTO safre_tmp:sumario_ind3 
            VALUES (campo_201 ,
                    campo_202 ,
                    campo_203 ,
                    campo_204 ,
                    campo_205 ,
                    campo_206 ,
                    campo_207 ,
                    campo_208 ,
                    campo_209)
        END IF

    END FOREACH

END FUNCTION

FUNCTION revisa_datos()
#----------------------

    DEFINE 
        aux_pausa    CHAR(1),
        rechazo_lote CHAR(3),
        rechazo_deta CHAR(3),
        l_reg RECORD LIKE tab_rch_lote.* ,
        x_reg RECORD LIKE tab_rdeta.* 

    DEFINE 
        rechazo_09  CHAR(02),
        rechazo_001 CHAR(02),
        rechazo_002 CHAR(02),
        rechazo_003 CHAR(02)

    DEFINE 
        uno  CHAR(3),
        dos  CHAR(3),
        tre  CHAR(3),
        cua  CHAR(3),
        cin  CHAR(3),
        diag CHAR(3)

    DEFINE
        l_status_int       SMALLINT

    # ENCABEZADO #

    SELECT campo1,
           campo2,
           campo3,
           campo18 
    INTO   rechazo_001,
           rechazo_002,
           rechazo_003,
           rechazo_lote
    FROM   safre_tmp:encabezado_ind3

    SELECT *
    INTO   l_reg.*
    FROM   tab_rch_lote
    WHERE  rlote_cod = rechazo_lote

    IF STATUS  <> NOTFOUND THEN
        CLEAR SCREEN
        DISPLAY l_reg.rlote_cod AT 10,1
        DISPLAY l_reg.rlote_desc_c AT 11,1
        PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR PROCESAR" FOR aux_pausa
        EXIT PROGRAM
    END IF

    IF rechazo_001 <> "01" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Tipo registro debe ser 01 en ENCABEZADO"
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Tipo registro debe ser 01 en ENCABEZADO" AT 10,1
            PROMPT "ERROR DE PROCESO, NO SE PUEDE CONTINUAR ",
                   " [Enter] p/salir " FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    IF rechazo_002 <> "01" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Identificador de servicio debe ser 01 en ENCABEZADO"
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Identificador de servicio ser 01 en ENCABEZADO" AT 10,1
            PROMPT "ERROR DE PROCESO, NO SE PUEDE CONTINUAR ",
                   " [Enter] p/salir " FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    IF rechazo_003 <> "60" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Identificador de operacion debe ser 60 en ENCABEZADO" 
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Identificador de operacion debe ser 60 en ENCABEZADO" AT 10,1
            PROMPT "ERROR DE PROCESO, NO SE PUEDE CONTINUAR ",
                   " [Enter] p/salir " FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    # SUMARIO #

    SELECT campo1 
    INTO   rechazo_09 
    FROM   safre_tmp:sumario_ind3

    IF rechazo_09 <> "09" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Tipo de registro debe ser 09 en RESUMEN" 
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Tipo de registro debe ser 09 en RESUMEN" AT 10,1
            PROMPT "ERROR DE PROCESO, NO SE PUEDE CONTINUAR ",
                   " [Enter] p/salir " FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    # DETALLE #

    DECLARE cursor_2 CURSOR FOR 
    SELECT *
    FROM   safre_tmp:detalle_ind3
    ORDER BY cod_operacion desc, diag_proceso desc

    FOREACH cursor_2 INTO reg_det.*

      SELECT n_folio, n_seguro
        INTO reg_det.folio_solicitud, reg_det.nss_solicitud
        FROM afi_solicitud
       WHERE n_unico        = reg_det.curp_solicitud
         AND tipo_solicitud = 8
         AND status_interno IN(30,40,50,55)

        {IF reg_det.fec_emision_certif IS NOT NULL THEN 
            LET x_fecha = reg_det.fec_emision_certif[5,6],"/",
                          reg_det.fec_emision_certif[7,8],"/",
                          reg_det.fec_emision_certif[1,4]
        ELSE
            LET x_fecha = NULL
        END IF}

        IF (reg_det.falta_actual IS NOT NULL) AND
           (reg_det.falta_actual[1] <> ' ') THEN 
            LET f1 = reg_det.falta_actual[5,6],"/",
                     reg_det.falta_actual[7,8],"/",
                     reg_det.falta_actual[1,4]
        ELSE
            LET f1 = NULL
        END IF

        IF (reg_det.fprimer_afil IS NOT NULL) AND
           (reg_det.fprimer_afil[1] <> ' ') THEN 
            LET f2 = reg_det.fprimer_afil[5,6],"/",
                     reg_det.fprimer_afil[7,8],"/",
                     reg_det.fprimer_afil[1,4]
        ELSE
            LET f2 = NULL
        END IF

        {IF (reg_det.fena_bd IS NOT NULL) AND
           (reg_det.fena_bd[1] <> ' ') THEN 
            LET f3 = reg_det.fena_bd[5,6],"/",
                     reg_det.fena_bd[7,8],"/",
                     reg_det.fena_bd[1,4]
        ELSE
            LET f3 = NULL
        END IF}

        LET falta    = f1
        LET fpafil   = f2
        LET fnbd     = f3
        LET xx_fecha = f1

        {IF reg_det.salario IS NULL THEN
            LET reg_det.salario = 0
        END IF

        LET reg_det.salario = reg_det.salario / 100

        IF reg_det.periodo IS NOT NULL THEN
            LET c_periodo = reg_det.periodo[5,6], '/01/', reg_det.periodo[1,4]
            LET f_periodo = c_periodo
        ELSE
            LET f_periodo = NULL
        END IF}

        LET diag = reg_det.diag_proceso[1,3]

        UPDATE afi_solicitud
        SET    fentcons       = f1,
               status         = reg_det.cod_operacion,
               status_captura = diag,
               fecha_1a_afil  = fpafil
        WHERE  n_unico        = reg_det.curp_solicitud
        AND    tipo_solicitud = 8
        AND    status_interno in(30,50,55,90)

        LET sello = reg_det.fecha_recep_sello,
                    reg_det.hora_recep_sello,
                    reg_det.minuto_recep_sello,
                    reg_det.seg_recep_sello,
                    reg_det.cseg_recep_sello,
                    reg_det.consecutivo_recep

# 29-11-2004

            SELECT cod_operacion,ind_funcion
            INTO   gv_cod_op, gv_ind_op
            FROM tab_cod_op_afi
            WHERE cod_operacion = reg_det.cod_operacion
            CASE   gv_ind_op
              WHEN   1
                     CALL aceptado()
              WHEN   2
                     CALL rechazado()
              WHEN   3
                     CALL pendiente_03 ()
                     LET reg_carta.docto_cod = 30229
                     CALL det_carta(30229) #dc
              WHEN   4
                     CALL aclarado()
                     LET reg_carta.docto_cod = 30204
                     CALL det_carta(30204) #dc
              WHEN   5
                     LET reg_carta.docto_cod = 30228
                     CALL det_carta(30228) #dc
                     CALL pendiente_90()
              WHEN   6
                     CALL traspaso()
            END CASE

        IF reg_det.cod_operacion = "02" OR
           reg_det.cod_operacion = "11" OR
           reg_det.cod_operacion = "12" OR
           reg_det.cod_operacion = "13" THEN
            LET uno = reg_det.diag_proceso[1,3]
            LET dos = reg_det.diag_proceso[4,6]
            LET tre = reg_det.diag_proceso[7,9]
            LET cua = reg_det.diag_proceso[10,12]
            LET cin = reg_det.diag_proceso[13,15]

            SELECT *
            INTO   x_reg.*
            FROM   tab_rdeta
            WHERE  rdeta_cod = uno
            AND    modulo_cod = 'afi'

            IF STATUS <> NOTFOUND THEN
                IF x_reg.tipo_rechazo = 'D' THEN
                    UPDATE afi_solicitud
                    SET    status_interno = 42,
                           fecha_1a_afil  = fpafil
                    WHERE  n_unico        = reg_det.curp_solicitud
                    AND    tipo_solicitud = 8
                    AND    status_interno = 40

                    CALL inserta_afi_ctr(42)

                    LET reg_carta.docto_cod = 30209
                    CALL det_carta(30209) #dc
                ELSE
                    IF uno <> '020' THEN
                        LET reg_carta.docto_cod = 30233
                        CALL det_carta(30233) #dc
                    ELSE
                        LET reg_carta.docto_cod = 30207
                        CALL det_carta(30207) #dc
                   END IF
                END IF

                CALL Inserta_datos_en_tabla_errores(reg_det.*,x_reg.*)
                CONTINUE FOREACH
            ELSE
                LET x_reg.rdeta_cod    = uno
                LET x_reg.rdeta_desc_c = NULL
            END IF

            SELECT *
            INTO   x_reg.*
            FROM   tab_rdeta
            WHERE  rdeta_cod = dos
            AND    modulo_cod = 'afi'

            IF STATUS <> NOTFOUND THEN
                CALL Inserta_datos_en_tabla_errores(reg_det.*,x_reg.*)
                CONTINUE FOREACH
            END IF

            SELECT *
            INTO   x_reg.*
            FROM   tab_rdeta
            WHERE  rdeta_cod = tre
            AND    modulo_cod = 'afi'

            IF STATUS <> NOTFOUND THEN
                CALL Inserta_datos_en_tabla_errores(reg_det.*,x_reg.*)
                CONTINUE FOREACH
            END IF

            SELECT *
            INTO   x_reg.*
            FROM   tab_rdeta
            WHERE  rdeta_cod = cua
            AND    modulo_cod = 'afi'

            IF STATUS <> NOTFOUND THEN
                CALL Inserta_datos_en_tabla_errores(reg_det.*,x_reg.*)
                CONTINUE FOREACH
            END IF

            SELECT *
            INTO   x_reg.*
            FROM   tab_rdeta
            WHERE  rdeta_cod = cin
            AND    modulo_cod = 'afi'

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

FUNCTION aceptado()
#a-----------------

    CALL habil_siguiente(xx_fecha) RETURNING diaSig

    UPDATE afi_solicitud 
    SET    status_interno     = 60,
           finicta            = TODAY, #CPL-2372,
           #salario_actual     = reg_det.salario,
           fecha_actualiza_sa = f_periodo,
           sello_electronico  = sello,
           fecha_1a_afil      = fpafil
    WHERE  n_unico            = reg_det.curp_solicitud
    AND    tipo_solicitud     = 8
    AND    status_interno    in(30,50,55,90)
    
    #DISPLAY "curp: ", reg_det.curp_solicitud

    LET aceptar = aceptar + 1

    CALL inserta_afi_ctr(60)

END FUNCTION

FUNCTION traspaso()
#t-----------------

    CALL habil_siguiente(xx_fecha) RETURNING diaSig

    UPDATE afi_solicitud
    SET    status_interno     = 70,
           #salario_actual     = reg_det.salario,
           finicta            = TODAY, #CPL-2372,
           fecha_actualiza_sa = f_periodo,
           sello_electronico  = sello,
	          cod_afore_ced      = reg_det.cve_afore,
           fecha_1a_afil      = fpafil
    WHERE  n_unico            = reg_det.curp_solicitud
    AND    tipo_solicitud     = 8
    AND    status_interno    in(30,50,55,90)

    LET aceptar   = aceptar + 1
    LET traspasar = traspasar + 1

    CALL inserta_afi_ctr(70)

END FUNCTION

FUNCTION rechazado()
#r------------------

    UPDATE afi_solicitud
    SET    status_interno = 40,
           fecha_1a_afil  = fpafil
    WHERE  n_unico        = reg_det.curp_solicitud
    AND    tipo_solicitud = 8
    AND    status_interno in(30,50,55,90)

    LET rechazar = rechazar + 1

    CALL inserta_afi_ctr(40)

END FUNCTION

FUNCTION pendiente_03()
#p---------------------

    UPDATE afi_solicitud 
    SET    status_interno = 50,
           fecha_1a_afil  = fpafil
    WHERE  n_unico        = reg_det.curp_solicitud
    AND    tipo_solicitud = 8
    AND    status_interno in(30,50,55,90)

    LET pendiente = pendiente + 1

    CALL inserta_afi_ctr(50)

END FUNCTION

FUNCTION aclarado()
#ac----------------

    UPDATE afi_solicitud
    SET    status_interno = 55,
           fecha_1a_afil  = fpafil
    WHERE  n_unico        = reg_det.curp_solicitud
    AND    tipo_solicitud = 8
    AND    status_interno in(30,50,55,90)

    LET aclaracion = aclaracion + 1

    CALL inserta_afi_ctr(55)

END FUNCTION

FUNCTION pendiente_90()
#p---------------------

    UPDATE afi_solicitud 
    SET    status_interno = 90,
           fecha_1a_afil  = fpafil
    WHERE  n_unico        = reg_det.curp_solicitud
    AND    tipo_solicitud = 8
    AND    status_interno in(30,50,55,90)

    LET pend_90   = pend_90   + 1

    CALL inserta_afi_ctr(90)

END FUNCTION

FUNCTION Inserta_datos_en_tabla_errores(reg_det,x_reg)
#-----------------------------------------------------

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
        -- fec_recp_sol_afore   CHAR(08)     ,   PST1943
        folio_solicitud      DECIMAL(16,0),
        sexo                 SMALLINT     ,
        entidad_nacimiento   CHAR(02)     ,
        tipo_admin           CHAR(02)     ,
        curp_renapo          CHAR(18)     ,
        nss_asoc             CHAR(11)     ,
        cve_afo_nss          CHAR(03)     ,
        tipo_trab            CHAR(01)     ,
        cod_operacion        CHAR(02)     ,
        diag_proceso         CHAR(15)     ,
        ident_lote_origen    CHAR(16)     ,
        depos                CHAR(02)     ,
        fprimer_afil         CHAR(08)     ,
        falta_actual         CHAR(08)     ,
        cve_afore            CHAR(03)     ,
        fecha_recep_sello    CHAR(08)     ,
        hora_recep_sello     CHAR(02)     ,
        minuto_recep_sello   CHAR(02)     ,
        seg_recep_sello      CHAR(02)     ,
        cseg_recep_sello     CHAR(02)     ,
        consecutivo_recep    CHAR(08)     ,
        ind_sie_cero         SMALLINT     ,
        ind_portab           SMALLINT
    END RECORD

    DEFINE x_reg RECORD LIKE tab_rdeta.*
    DEFINE v_fecha  CHAR(10)
    DEFINE v_fecha1 CHAR(10)

    {IF (reg_det.fec_emision_certif IS NOT NULL) AND
       (reg_det.fec_emision_certif[1] <> ' ') THEN
       LET v_fecha = reg_det.fec_emision_certif[05,06],"/",
                     reg_det.fec_emision_certif[07,08],"/",
                     reg_det.fec_emision_certif[01,04]
    ELSE
       LET v_fecha = NULL
    END IF}

    IF (reg_det.falta_actual IS NOT NULL) AND
       (reg_det.falta_actual[1] <> ' ') THEN
        LET v_fecha1 = reg_det.falta_actual[05,06],"/",
                       reg_det.falta_actual[07,08],"/",
                       reg_det.falta_actual[01,04]
    ELSE
        LET v_fecha1 = NULL
    END IF

    SELECT "X" 
    FROM   afi_rechaza_cert
    WHERE  n_folio        = reg_det.folio_solicitud
    AND    tipo_solicitud = 8
    AND    n_seguro       = reg_det.nss_solicitud
    AND    f_rechazo      = v_fecha
    AND    rdeta_cod      = x_reg.rdeta_cod

    IF SQLCA.SQLCODE = 100 THEN
        IF x_reg.rdeta_cod = 14 THEN
            INSERT INTO afi_rechaza_cert
            VALUES (reg_det.folio_solicitud,
                    8,
                    reg_det.nss_solicitud,
                    x_reg.rdeta_cod,
                    v_fecha,
                    v_fecha1,
                    reg_det.cve_afore,
                    x_reg.rdeta_desc_c,
                    #reg_det.nombre_pcanase)
                    NULL)
        ELSE
            INSERT INTO afi_rechaza_cert
            VALUES (reg_det.folio_solicitud,
                    8,
                    reg_det.nss_solicitud,
                    x_reg.rdeta_cod,
                    v_fecha,
                    " ",
                    " ",
                    x_reg.rdeta_desc_c,
                    #reg_det.nombre_pcanase)
                    NULL)
        END IF
    END IF

END FUNCTION

FUNCTION despliega_resultados()
#dr----------------------------

    DEFINE total_resp SMALLINT

    LET total_resp = aceptar + rechazar + pendiente + aclaracion + pend_90

    IF bnd_proceso THEN
        DISPLAY "                TOTAL REGISTROS RECIBIDOS                "

        DISPLAY "Total de Registros del lote     : ", total_resp USING "#######&"

        DISPLAY "Registros certificados     : ", aceptar USING "#######&"

        DISPLAY "Registros rechazados       : ", rechazar USING "#######&"

        DISPLAY "Registros pendientes       : ", pendiente USING "#######&"

        DISPLAY "Registros en aclaracion    : ", aclaracion USING "#######&"

        DISPLAY "Registros pendientes asig. : ", pend_90 USING "#######&"

        INSERT INTO afi_ctr_arh_reg 
        VALUES (vgenerar,
                aceptar,
                rechazar,
                pendiente,
                aclaracion,
                pend_90,
                hoy)
    ELSE
        DISPLAY "                TOTAL REGISTROS RECIBIDOS                 " AT 10,1 ATTRIBUTE ( REVERSE )

        DISPLAY "Total de Registros del lote: ",
            total_resp USING "#######&" AT 11,01 ATTRIBUTE ( BOLD )

        DISPLAY "Registros certificados     : ",
            aceptar USING "#######&" AT 12,01 ATTRIBUTE ( BOLD )

        DISPLAY "Registros rechazados       : ",
            rechazar USING "#######&" AT 13,01 ATTRIBUTE ( BOLD ) 

        DISPLAY "Registros pendientes       : ",
            pendiente USING "#######&" AT 14,01 ATTRIBUTE ( BOLD ) 

        DISPLAY "Registros en aclaracion    : ",
            aclaracion USING "#######&" AT 15,01 ATTRIBUTE ( BOLD ) 

        DISPLAY "Registros pendientes asig. : ",
            pend_90 USING "#######&" AT 16,01 ATTRIBUTE ( BOLD ) 

        PROMPT "Presione <enter> para continuar " FOR enter

        INSERT INTO afi_ctr_arh_reg 
        VALUES (vgenerar, 
                aceptar,
                rechazar,
                pendiente,
                aclaracion,
                pend_90,
                hoy)
    END IF

END FUNCTION

FUNCTION actualiza_est()
#ae---------------------

    DEFINE 
        i      ,
        st_int ,
        total  SMALLINT

    FOR i = 1 TO 6
        CASE i
            WHEN 1 LET st_int = 40
                   LET total  = rechazar
            WHEN 2 LET st_int = 50
                   LET total  = pendiente
            WHEN 3 LET st_int = 55
                   LET total  = aclaracion
            WHEN 4 LET st_int = 60
                   LET total  = aceptar
            WHEN 5 LET st_int = 70
                   LET total  = traspasar
            WHEN 6 LET st_int = 90
                   LET total  = pend_90
        END CASE

        SELECT 'X'
        FROM   est_det_diario edd
        WHERE  edd.fecha_detalle  = hoy
        AND    edd.nombre_archivo = vgenerar
        AND    edd.status_interno = st_int
        AND    edd.tipo_solicitud = 8 

        IF SQLCA.SQLCODE <> 0 THEN
            INSERT INTO est_det_diario
            VALUES (hoy,
                    st_int,
                    total,
                    8,
                    vgenerar)
        END IF  
    END FOR

END FUNCTION

FUNCTION habil_siguiente(diaActual)
#hs--------------------------------

    DEFINE
        diaTmp    DATE,
        contador  SMALLINT,
        diaActual DATE
   
    DEFINE
        diaHabilSig DATE,
        diaSemana   SMALLINT,
        feriado     SMALLINT,
        finSemana   SMALLINT

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
            EXIT WHILE
        END IF
    END WHILE

    RETURN diaHabilSig

END FUNCTION

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
    DEFINE vagenc_cod     CHAR(10)

    LET ind_nombre = 0

    SELECT MAX(p.ind_envio)
    INTO   ind_envio
    FROM   afi_ctr_solicitud p
    WHERE  p.n_seguro       = reg_det.nss_solicitud
    AND    p.n_folio        = reg_det.folio_solicitud
    AND    p.tipo_solicitud = 8

    SELECT p.fecha_envio, p.paterno, p.materno, p.nombres, p.agenc_cod
    INTO   fecha_envio, paterno, materno, nombres, vagenc_cod
    FROM   afi_solicitud p
    WHERE  p.n_seguro       = reg_det.nss_solicitud
    AND    p.n_folio        = reg_det.folio_solicitud
    AND    p.tipo_solicitud = 8

    LET nom_comp               = paterno CLIPPED, "$",
				 materno CLIPPED, "$",
				 nombres CLIPPED

    #LET reg_det.nombre_pcanase = reg_det.nombre_pcanase CLIPPED
    LET nom_comp               = nom_comp CLIPPED

    {IF nom_comp <> reg_det.nombre_pcanase THEN
       LET ind_nombre = 1
    END IF}

    IF fecha_envio IS NULL THEN
       LET fecha_envio = today
    END IF

    INSERT INTO afi_ctr_solicitud
    VALUES(8                           ,
           reg_det.folio_solicitud     ,
           reg_det.nss_solicitud       ,
	   nom_comp                    ,
	   status_interno              ,
           fecha_envio                 ,
           today                       ,
           #reg_det.cve_afo_ced         ,
           NULL                        ,
           #reg_det.ind_nss_modificado  ,
           NULL                        ,
           x_fecha                     ,
           reg_det.curp_solicitud      ,
           #reg_det.ind_curp_modif      ,
           NULL                        ,
           #reg_det.n_rfc_bd            ,
           NULL                        ,
           #reg_det.nombre_bd           ,
           NULL                        ,
           #reg_det.nombre_pcanase      ,
           NULL                        ,
           fnbd                        ,
           #reg_det.codven_bd           ,
           NULL                        ,
           #reg_det.sexo_bd             ,
           NULL                        ,
           #reg_det.estadon_bd          ,
           NULL                        ,
           fpafil                      ,
           falta                       ,
           reg_det.cve_afore           ,
           #reg_det.nacionalidad_bd     ,
           NULL                        ,
           #reg_det.tip_aprob_bd        ,
           NULL                        ,
           #reg_det.fol_aprob_bd        ,
           NULL                        ,
           #reg_det.doc_aprob_bd        ,
           NULL                        ,
           ind_envio                   ,
	   ind_nombre                  ,
	   reg_det.cod_operacion       ,
           reg_det.diag_proceso        ,
           vagenc_cod                  ,
	   g_usuario                   ,
	   today                       )

    --# CPL-2139 Mar 2016 
  
  IF reg_det.nss_solicitud[1,1] <> " " AND reg_det.nss_solicitud IS NOT NULL THEN 
    #borra el posible registro existente en caso de haber exist�do algun reverso incompleto
    DELETE  FROM  afi_indicadores_cert
     WHERE  nss             =  reg_det.nss_solicitud
       AND  n_folio         =  reg_det.folio_solicitud
       AND  tipo_solicitud  =  8
    
    INSERT  INTO  afi_indicadores_cert  VALUES (
                  reg_det.nss_solicitud,         --nss            
                  reg_det.folio_solicitud,       --n_folio        
                  8,                            --tipo_solicitud 
                  reg_det.ind_sie_cero,          --ind_sie_cero   
                  reg_det.ind_portab,            --ind_portab     
                  g_usuario,                     --usuario        
                  hoy)                           --factualiza 
  END IF 
END FUNCTION

FUNCTION actualiza_bat_f(v_folio)
#ab-----------------------------

    DEFINE 
        vv_prog      CHAR(10),
        vv_fecha_log CHAR(30),
        paso         CHAR(100),
        v_cat        CHAR(600),
        v_folio      INTEGER

    DEFINE v_fecha_log DATETIME YEAR TO SECOND
    DEFINE reg_ruta RECORD LIKE seg_modulo.*

    SELECT A.*
    INTO   reg_ruta.*
    FROM   seg_modulo A
    WHERE  modulo_cod = "bat"
 
    UPDATE bat_ctr_operacion
    SET    folio       = NULL ,      
           estado_cod  = 4    ,
           fecha_fin   = CURRENT,
           nom_archivo = reg_bat.nombre_archivo
    WHERE  pid         = reg_bat.pid
    AND    proceso_cod = reg_bat.proceso_cod
    AND    opera_cod   = reg_bat.opera_cod

    UPDATE bat_ctr_proceso
    SET    folio       = NULL ,      
           estado_cod  = 4    ,
           fecha_fin   = CURRENT
    WHERE  pid         = reg_bat.pid
    AND    proceso_cod = reg_bat.proceso_cod

    UPDATE bat_tmp_predecesor
    SET    bandera_ejecuta  = 1
    WHERE  pid_prod         = reg_bat.pid
    AND    proceso_cod_prod = reg_bat.proceso_cod
    AND    opera_cod_prod   = reg_bat.opera_cod

    LET v_fecha_log  = CURRENT
    LET vv_fecha_log = v_fecha_log

    SELECT A.programa_cod 
    INTO   vv_prog 
    FROM   bat_ctr_operacion A
    WHERE  A.pid         = reg_bat.pid
    AND    A.proceso_cod = reg_bat.proceso_cod
    AND    A.opera_cod   = reg_bat.opera_cod

    LET paso = "nohup:"            ,
               reg_bat.pid         USING"&&&&&",":",
               reg_bat.proceso_cod USING"&&&&&",":",
               reg_bat.opera_cod   USING"&&&&&"

    LET v_cat = "echo '"                ,
                vv_fecha_log[1,4]       ,   
                vv_fecha_log[6,7]       ,  
                vv_fecha_log[9,10]      ,  
                vv_fecha_log[12,13]     ,   
                vv_fecha_log[15,16]     ,    
                vv_fecha_log[18,19]     ,
                "|"                    ,
                vv_prog  CLIPPED        ,
                "|"                     ,
                "FINOK"                ,
                "|"                     ,
                reg_ruta.ruta_listados CLIPPED,  
                "/"                     ,
                paso CLIPPED            ,
                "'"                     ,
                " >> "                  ,
                reg_ruta.ruta_envio CLIPPED ,
                "/"                     ,
                "aad_safre.log"

    LET v_cat = v_cat CLIPPED
    RUN v_cat

END FUNCTION

FUNCTION det_carta(docto_cod) #dc
#dc---------------------

    DEFINE docto_cod SMALLINT

    LET reg_carta.docto_cod      = docto_cod

    LET reg_carta.nss            = reg_det.nss_solicitud
    LET reg_carta.n_folio        = reg_det.folio_solicitud  
    LET reg_carta.tipo_solicitud = 8
    LET reg_carta.fecha_registro = xx_fecha
    LET reg_carta.opera_cod      = NULL
    LET reg_carta.edo_genera     = 10
    LET reg_carta.fecha_genera   = TODAY
    LET reg_carta.hora_genera    = TIME
    LET reg_carta.lote_genera    = 0
    LET reg_carta.consecutivo    = 0
    LET reg_carta.id_sepomex     = 0

    LET consulta_carta = "EXECUTE PROCEDURE inserta_carta (?,?,?,?,?,?,",
  			 "?,?,?,?,?,?)"
    PREPARE exe_sql FROM consulta_carta
    EXECUTE exe_sql USING reg_carta.*

    INITIALIZE reg_carta.* TO NULL
                       
END FUNCTION

FUNCTION apertura_cuenta()
#ac-----------------------

   DEFINE 
       enter       CHAR(1),
       generar     CHAR(1),
       aux_pausa   CHAR(1),
       opc         CHAR(1),
       g_usuario   CHAR(8),
       HORA        CHAR(8),
       vnss        CHAR(11),
       operacion   CHAR(40),
       v_sql_1     CHAR(50),
       v_sql_2     CHAR(50),
       HOY         DATE,
       f_ini_tmte  DATE,
       v_query     CHAR(300)

    DEFINE w_aux  RECORD
        n_seguro            LIKE afi_solicitud.n_seguro,
        n_unico             LIKE afi_solicitud.n_unico ,
        n_rfc               LIKE afi_solicitud.n_rfc   ,
        paterno             LIKE afi_solicitud.paterno ,
        materno             LIKE afi_solicitud.materno ,
        nombres             LIKE afi_solicitud.nombres ,
        fena                LIKE afi_solicitud.fena    ,
        sexo                LIKE afi_solicitud.sexo    ,
        frecafor            LIKE afi_solicitud.frecafor,
        status_interno      SMALLINT
    END RECORD

    DEFINE g_afore       RECORD LIKE tab_afore_local.*
    DEFINE g_aficefa     RECORD LIKE afi_icefa.*
    DEFINE gr_ctanssreg  RECORD LIKE cta_nss_regimen.*

    DEFINE
        bnd_proceso     SMALLINT ,
        pestado_marca   SMALLINT ,
        pcodigo_rechazo SMALLINT ,
        ejecuta         CHAR(300),
        xcodigo_marca   SMALLINT ,
        xcodigo_rechazo SMALLINT ,
        pmarca_entra    SMALLINT,
        pmarca_causa    SMALLINT,
        pfecha_causa    SMALLINT,
	xmarca_esado    SMALLINT,
	edo_proc        SMALLINT

    DEFINE consulta_carta CHAR(120)

    DEFINE reg_carta RECORD LIKE int_ctr_carta.*
    DEFINE afi       RECORD LIKE afi_solicitud.*

    DEFINE 
        pat RECORD LIKE afi_patron.* ,
        ben RECORD LIKE afi_beneficiario.*,
        mae RECORD LIKE afi_mae_afiliado.*,
        cta RECORD LIKE cta_ctr_cuenta.*

    DEFINE 
        mensaje   CHAR(050),
        G_LISTA   CHAR(300)

    DEFINE 
        i             ,
        cont          ,
        HAY           ,
        v_porc        SMALLINT,
        v_existe      ,
        v_edad        ,
        v_criterio    ,
        v_ind_edad    SMALLINT,        
        v_crea_fecha  DATE,        
        v_tipo_proc   ,
	v_tipo_trasp  ,
        v_medio       ,
        v_cve_siefore ,
        v_cve_sief_i  ,
        v_cve_sief_f  ,
        v_rechazo     SMALLINT,
        v_folioatencion INTEGER

    DEFINE
        v_curp        CHAR(18),
        v_rfc         CHAR(13),
        v_fena        DATE

    DEFINE regrowid RECORD 
       v_rowid   DECIMAL(10,0)
    END RECORD  

    DEFINE v_afisolreg RECORD
         nss   LIKE afi_solicitud_regimen.nss,
         fol   LIKE afi_solicitud_regimen.n_folio,
         ts    LIKE afi_solicitud_regimen.tipo_solicitud,
         edo   LIKE afi_solicitud_regimen.estado
    END RECORD
    
    -- CPL-3044      
    DEFINE v_siefore      SMALLINT 
    -- CPL-3044

     DEFINE v_tipo_beneficiario INTEGER
   DEFINE v_tramite_ben       INTEGER
   DEFINE v_porcentaje_tot    DECIMAL(6,2)
   DEFINE v_ind_designacion   INTEGER

   DEFINE v_cod_respuesta     CHAR(2)
   DEFINE v_cod_diagnostico   SMALLINT
   DEFINE v_descripcion       VARCHAR(100)

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE safre_tmp:afi_ind
    WHENEVER ERROR STOP

    CREATE TABLE safre_tmp:afi_ind
       (n_seguro CHAR(11) ,
        n_unico  CHAR(18) ,
        n_rfc    CHAR(13) ,
        paterno  CHAR(40) ,
        materno  CHAR(40) ,
        nombres  CHAR(40) ,
        fena     DATE     ,
        sexo     SMALLINT ,
        frecafor DATE     ,
        status_interno  SMALLINT);

    DATABASE safre_af

    SELECT *, USER
    INTO   g_afore.*, g_usuario
    FROM   tab_afore_local

    LET HOY      = TODAY
    LET HORA     = TIME

    LET operacion = 'ALTA EN MAESTRO DE AFILIADOS'

    INITIALIZE reg_carta.* TO NULL  
    INITIALIZE gr_ctanssreg.* TO NULL

    LET v_tipo_trasp = 5
    LET v_tipo_proc  = 1
    LET v_medio      = 10

    LET v_sql_1 = "EXECUTE FUNCTION fn_fnacimiento(?,?)"
    LET v_sql_2 = "EXECUTE FUNCTION fn_regimen_inv(?,?,?,?,?,?)"

    PREPARE stmt1 FROM v_sql_1
    PREPARE stmt2 FROM v_sql_2

    LET mensaje = "NO SE HICIERON TRASPASOS AL MAESTRO DE AFILIADOS"

    DECLARE cursor_a CURSOR FOR 
    SELECT rowid,A.* 
    FROM   afi_solicitud A
    WHERE  A.status_interno = 60
    AND    A.tipo_solicitud = 8
    ORDER  BY n_seguro

    FOREACH cursor_a INTO  regrowid.v_rowid,afi.*

       LET reg_det.nss_solicitud   = afi.n_seguro
       LET reg_det.folio_solicitud = afi.n_folio

       IF afi.n_unico IS NULL THEN
          LET sin_curp = sin_curp + 1
       ELSE
          LET con_curp = con_curp + 1
       END IF

       LET HAY = FALSE

        SELECT COUNT(*)
        INTO   HAY
        FROM   afi_mae_afiliado m
        WHERE  m.n_seguro = afi.n_seguro

        IF HAY THEN
            SELECT *
            INTO   mae.*
            FROM   afi_mae_afiliado ma
            WHERE  ma.n_seguro = afi.n_seguro

            IF SQLCA.SQLCODE = 0 THEN
                INSERT INTO afi_his_afiliado VALUES (mae.*)

                IF SQLCA.SQLCODE = 0 THEN
                    DELETE
                    FROM   afi_mae_afiliado
                    WHERE  n_seguro = afi.n_seguro
                END IF

                SELECT b.*
                INTO   cta.*
                FROM   cta_ctr_cuenta b
                WHERE  b.nss = afi.n_seguro

                IF cta.nss THEN
                    INSERT INTO cta_his_cuenta VALUES (cta.*)
                END IF

                IF mae.tipo_solicitud = 5 THEN
                    LET afi.finitmte = mae.fentcons
                    LET afi.fentcons = afi.fentcons

                    UPDATE afi_det_asignado
                    SET    fecha_afiliacion = afi.fentcons,
                           estado_asignado  = 100
                    WHERE  n_seguro = mae.n_seguro
                    AND    n_folio  = mae.n_folio
                    AND    tipo_solicitud = mae.tipo_solicitud
                END IF

                LET HAY = FALSE
            END IF
        END IF

        LET afi.status_interno = 100
        LET afi.status_captura = 100

        IF NOT HAY THEN
            IF afi.n_unico IS NOT NULL AND
               afi.n_unico <> "                  " AND
                LENGTH(afi.n_unico) = 18 THEN
                LET afi.status_interno = 200
                LET afi.status_captura = 0
            ELSE
                LET afi.status_interno = 100
                LET afi.status_captura = 0
            END IF

            LET afi.status = NULL 

            INSERT INTO afi_mae_afiliado VALUES(afi.*)

            IF SQLCA.SQLCODE <> 0 THEN
                INSERT INTO safre_tmp:nss_dup VALUES (afi.n_seguro)
            END IF

            INSERT INTO afi_mae_patron      -------- Patrones
            SELECT *
            FROM   afi_patron
            WHERE  n_folio = afi.n_folio
            AND    tipo_solicitud = afi.tipo_solicitud

           INSERT INTO afi_mae_benefici    -------- Beneficiarios
           SELECT *
           FROM   afi_beneficiario
           WHERE  n_seguro = afi.n_seguro
           AND    n_folio = afi.n_folio

           --Se registra control de designacion para beneficiarios
              LET v_query = "EXECUTE PROCEDURE fn_control_beneficiarios(?,?,?,?,?,?,?)"
              PREPARE exe_control_beneficiarios FROM v_query

              SELECT SUM(porcentaje)
              INTO v_porcentaje_tot
              FROM afi_mae_benefici
              WHERE n_folio = afi.n_folio
              AND tipo_solicitud = afi.tipo_solicitud

              IF v_porcentaje_tot IS NOT NULL AND v_porcentaje_tot > 0 THEN
                 LET v_tipo_beneficiario = 2      --Beneficiarios Designados

                 LET v_ind_designacion = 1
              ELSE
                 LET v_tipo_beneficiario = 5      --SIN Beneficiarios Designados

                 LET v_ind_designacion = 0
              END IF

              LET v_tramite_ben = 1      --Registro

              DECLARE cur_control_beneficiarios CURSOR FOR exe_control_beneficiarios
              OPEN  cur_control_beneficiarios USING  afi.n_folio,
                                                     afi.tipo_solicitud,
                                                     v_tipo_beneficiario,
                                                     v_tramite_ben,
                                                     v_porcentaje_tot,
                                                     v_ind_designacion,
                                                     g_usuario

              FETCH cur_control_beneficiarios INTO v_cod_respuesta,
                                                   v_cod_diagnostico,
                                                   v_descripcion
              CLOSE cur_control_beneficiarios

           SELECT "X"
           FROM   cta_ctr_cuenta
           WHERE  cta_ctr_cuenta.nss = afi.n_seguro

           IF SQLCA.SQLCODE = 0 THEN
                UPDATE cta_ctr_cuenta
                SET    fecha_pri_rcv      = NULL,
                       fecha_ult_rcv      = NULL,
                       fecha_pri_general  = NULL,
                       fecha_ult_general  = NULL,
                       fecha_vol_pat      = NULL,
                       fecha_vol_ven      = NULL,
                       ind_actividad      = 1,
                       fecha_actividad    = HOY,
                       ind_edad           = 0,
                       fecha_edad         = HOY,
                       criterio_edad      = 0,
                       ind_transferencia  = 0,
                       fecha_ind_transf   = HOY,
                       ind_saldo_cero     = 0,
                       fecha_saldo_cero   = NULL,
                       estado_impresion   = 0,
                       periodo_ult_aporte = NULL,
                       dias_cotizados     = 0,
                       ult_sal_integrado  = 0,
                       tipo_informe       = 0,
                       fecha_informe      = NULL,
                       fecha_registro     = HOY,
                       usuario            = g_usuario
                WHERE  nss                = afi.n_seguro
            ELSE
                INSERT INTO cta_ctr_cuenta     #------ Control cuenta
                VALUES (afi.n_seguro,       #nss
                        "",                 #fecha_pri_rcv
                        "",                 #fecha_ult_rcv
                        "01/01/0001",       #fecha_pri_general
                        "",                 #fecha_ult_general
                        "",                 #fecha_vol_pat
                        "",                 #fecha_vol_ven
                        0,                  #ind_saldo_cero
                        "",                 #fecha_saldo_cero
                        1,                  #ind_actividad
                        HOY,                #fecha_actividad
                        0,                  #ind_edad
                        HOY,                #fecha_edad
                        0,                  #criterio_edad
                        0,                  #ind_transferencia
                        HOY,                #fecha_ind_transf
                        0,                  #estado_impresion,
                        "",                 #periodo_ult_aporte
                        0,                  #dias_cotizados
                        0,                  #ult_sal_integrado
                        0,                  #tipo_informe
                        "",                 #fecha_informe
                        HOY,                #fecha_registro
                        g_usuario           #usuario
                        )
            END IF

            LET v_crea_fecha = HOY

            DECLARE curs1 CURSOR FOR stmt1
            OPEN  curs1 USING afi.n_seguro, v_crea_fecha
            FETCH curs1 INTO v_existe, v_edad, v_criterio, v_ind_edad, v_curp, v_rfc, v_fena
            CLOSE curs1
            
            -- CPL-3044
            --
            LET v_siefore = 0
            
            SELECT codigo_siefore
              INTO v_siefore
              FROM cat_rango_nacimiento
             WHERE id_rango_nacimiento = v_ind_edad
            
            IF SQLCA.SQLCODE = NOTFOUND THEN 
            	  LET v_siefore = v_ind_edad 
            END IF 
            --           
            -- CPL-3044   

            DECLARE curs2 CURSOR FOR stmt2
	             OPEN curs2 USING afi.n_seguro,
	                              v_ind_edad,
	                              v_siefore,             -- CPL-3044
	                              v_tipo_proc,
	                              v_tipo_trasp,
	                              v_medio
            FETCH curs2 INTO v_existe, v_ind_edad, v_rechazo, v_folioatencion
            CLOSE curs2
            
            IF v_rechazo <> 0 THEN
	       INSERT INTO safre_tmp:rch_apertura
	       VALUES (afi.n_seguro,v_rechazo)
	    END IF
       END IF

        LET HORA = TIME

        INSERT INTO afi_ctr_logico
        VALUES (afi.n_folio,
                afi.tipo_solicitud,
                afi.n_seguro,
                afi.status_interno,
                g_usuario,
                HOY,
                HORA,
                operacion)

        INSERT INTO safre_tmp:afi_ind
        VALUES (afi.n_seguro       ,
                afi.n_unico        ,
                afi.n_rfc          ,
                afi.paterno        ,
                afi.materno        ,
                afi.nombres        ,
                afi.fena           ,
                afi.sexo           ,
                afi.frecafor       ,
                afi.status_interno
                )

        UPDATE afi_solicitud 
        SET    afi_solicitud.status         = 100 ,
               afi_solicitud.status_interno = 100 ,
               afi_solicitud.status_captura = 100 ,
               fecha_1a_afil                = fpafil       ---erm 
        WHERE  afi_solicitud.n_seguro       = afi.n_seguro
        AND    afi_solicitud.n_folio        = afi.n_folio
        AND    afi_solicitud.tipo_solicitud = afi.tipo_solicitud

--SE QUEDA TAL CUAL
        LET reg_det.nss_solicitud   = afi.n_seguro
        LET reg_det.folio_solicitud = afi.n_folio

        {IF (afi.finitmte IS NULL) OR 
           (afi.finitmte = '')    THEN}
            LET reg_carta.docto_cod = 30501 
            CALL det_carta(30501) #dc
        {ELSE
            LET reg_carta.docto_cod = 30502
            CALL det_carta(30502) #dc
        END IF}

        SELECT "X"
        FROM   safre_af:rec_solicitud mr
        WHERE  mr.n_seguro   = afi.n_seguro
        AND    mr.origen_rec <> 1
        GROUP BY 1

        IF SQLCA.SQLCODE = 0 THEN
            UPDATE safre_af:rec_solicitud
            SET    safre_af:rec_solicitud.origen_rec = 1
            WHERE  safre_af:rec_solicitud.n_seguro = afi.n_seguro
            AND    safre_af:rec_solicitud.origen_rec <> 1
        END IF      
    END FOREACH

    SELECT "X"
    FROM   safre_tmp:afi_ind
    GROUP BY 1

    LET G_LISTA = g_paramgrales.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                  ".ENVIA_LIBIND_MAESTRO." CLIPPED,
                  HOY USING "dd-mm-yy","_",HORA CLIPPED

    IF STATUS = NOTFOUND THEN 
        START REPORT listado_1 TO G_LISTA
            OUTPUT TO REPORT listado_1(mensaje)
        FINISH REPORT listado_1
    ELSE
        DECLARE cur_1 CURSOR FOR
        SELECT *
        FROM   safre_tmp:afi_ind

        START REPORT listado_2 TO G_LISTA
            FOREACH cur_1 INTO w_aux.*
                OUTPUT TO REPORT listado_2(w_aux.*)
            END FOREACH
        FINISH REPORT listado_2
    END IF 

    LET G_LISTA = "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,
		 "/",g_usuario CLIPPED,".ENVIA_LIBIND_MAESTRO." CLIPPED,
                  HOY USING "dd-mm-yy","_",HORA CLIPPED
    RUN G_LISTA

    CALL despliega_apertura()

    PROMPT "Proceso finalizado, [Enter] para salir"
    FOR enter

END FUNCTION

FUNCTION despliega_apertura()
#dr--------------------------

    DEFINE total_resp SMALLINT

    LET total_resp = con_curp + sin_curp

    IF bnd_proceso THEN
        DISPLAY "          NTI INCORPORADOS AL MAESTRO DE AFILIADOS           "
        DISPLAY "Total de Registros Incorporados : ", total_resp USING "#####&"

        DISPLAY "Registros con curp              : ", con_curp   USING "#####&"

        DISPLAY "Registros sin curp              : ", sin_curp   USING "#####&"
    ELSE
        DISPLAY "          NTI INCORPORADOS AL MAESTRO DE AFILIADOS           "
        AT 10,1 ATTRIBUTE(REVERSE)
----ES EL DE HSBC
        DISPLAY "Tot Reg Incorporados: ", total_resp USING "#####&"
        AT 11,45

        DISPLAY "Registros con curp  : ", con_curp   USING "#####&"
        AT 12,45

        DISPLAY "Registros sin curp  : ", sin_curp   USING "#####&"
        AT 13,45

        DISPLAY "                                                                               " 
        AT 14,45
        DISPLAY "                                                                               " 
        AT 15,45
        DISPLAY "                                                                               " 
        AT 16,45

    END IF

END FUNCTION

REPORT listado_1(mensaje)
#------------------------

    DEFINE
        mensaje             CHAR(50)

    OUTPUT
	LEFT MARGIN   0
	RIGHT MARGIN  0
	TOP MARGIN    0
	BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
        PRINT
            COLUMN 03,"========================================",
            COLUMN 40,"==================================="
        PRINT
            COLUMN 20," TRASPASO NO AFILIADOS AL MAESTRO DE AFILIADOS LIB PEND"
        PRINT
            COLUMN 03,"----------------------------------------",
            COLUMN 40,"-----------------------------------"
        PRINT
            COLUMN 01,"N T I  "  ,
            COLUMN 13,"CURP   "  ,
            COLUMN 33,"R.F.C. "  ,
            COLUMN 48,"Paterno"  ,
            COLUMN 68,"Materno"  ,
            COLUMN 88,"Nombres"       
        PRINT
            COLUMN 05,"Fecha Nac."     ,
            COLUMN 17,"Sexo"           ,
            COLUMN 28,"Fecha Frecafor" ,
            COLUMN 43,"Edo. Afiliado" 
        PRINT
            COLUMN 03,"========================================",
            COLUMN 40,"==================================="
        PRINT 
        PRINT 
        PRINT 
            COLUMN 15,mensaje

END REPORT

REPORT listado_2(w_aux)
#----------------------

    DEFINE w_aux  RECORD
        n_seguro            LIKE afi_solicitud.n_seguro,
        n_unico             LIKE afi_solicitud.n_unico ,
        n_rfc               LIKE afi_solicitud.n_rfc   ,
        paterno             LIKE afi_solicitud.paterno ,
        materno             LIKE afi_solicitud.materno ,
        nombres             LIKE afi_solicitud.nombres ,
        fena                LIKE afi_solicitud.fena    ,
        sexo                LIKE afi_solicitud.sexo    ,
        frecafor            LIKE afi_solicitud.frecafor,
        status_interno      SMALLINT
    END RECORD

    DEFINE 
        l_estado    CHAR(16) ,
        aux_sexo    CHAR(10)

    DEFINE 
        cont        INTEGER

    OUTPUT
	LEFT MARGIN   0
	RIGHT MARGIN  0
	TOP MARGIN    0
	BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
        PRINT
            COLUMN 03,"========================================",
            COLUMN 40,"==================================="
        PRINT
            COLUMN 20," TRASPASO NO AFILIADOS AL MAESTRO DE AFILIADOS LIB PEND"
        PRINT
            COLUMN 03,"----------------------------------------",
            COLUMN 40,"-----------------------------------"
        PRINT
            COLUMN 01,"N T I  "       ,
            COLUMN 13,"CURP   "       ,
            COLUMN 33,"R.F.C. "       ,
            COLUMN 48,"Paterno"       ,
            COLUMN 68,"Materno"       ,
            COLUMN 88,"Nombres"       
        PRINT
            COLUMN 05,"Fecha Nac."    ,
            COLUMN 17,"Sexo"          ,
            COLUMN 28,"Fecha Frecafor",
            COLUMN 43,"Edo. Afiliado" 
        PRINT
            COLUMN 03,"========================================",
            COLUMN 40,"==================================="
    ON EVERY ROW
        IF w_aux.n_unico IS NULL OR w_aux.n_unico = " " THEN
	   LET l_estado = NULL
	END IF
        CASE w_aux.status_interno
	     WHEN   0 LET l_estado = "CAPTURADO"
	     WHEN  10 LET l_estado = "IMCOMPLETO"
	     WHEN  20 LET l_estado = "COMPLETO"
	     WHEN  30 LET l_estado = "ENIVIADO"
	     WHEN  40 LET l_estado = "RECHAZADO"
	     WHEN  50 LET l_estado = "PENDIENTE"
	     WHEN  55 LET l_estado = "ACLARACION"
	     WHEN  60 LET l_estado = "APROBADO"
	     WHEN 100 LET l_estado = "REGISTRADO"
        END CASE

	SELECT sexo_desc 
        INTO   aux_sexo 
        FROM   tab_sexo
	WHERE  sexo_cod = w_aux.sexo

        PRINT
            COLUMN 01,w_aux.n_seguro                   ,
            COLUMN 13,w_aux.n_unico                    ,
            COLUMN 33,w_aux.n_rfc                      ,
            COLUMN 48,w_aux.paterno CLIPPED            ,
            COLUMN 68,w_aux.materno CLIPPED            ,
            COLUMN 88,w_aux.nombres CLIPPED
        PRINT
            COLUMN 05,w_aux.fena  USING "dd-mm-yyyy"   ,
            COLUMN 17,aux_sexo    ,
            COLUMN 28,w_aux.frecafor USING "dd-mm-yyyy",
            COLUMN 43,l_estado CLIPPED
    
    ON LAST ROW
           SELECT COUNT(*)
           INTO   cont
           FROM   safre_tmp:afi_ind

        PRINT
        PRINT
        PRINT
            COLUMN 03,"----------------------------------------",
            COLUMN 40,"-----------------------------------"

        PRINT
        PRINT
           COLUMN 03,"NUMERO TOTAL DE REGISTROS ---> ",cont
END REPORT

