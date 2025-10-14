###########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                        #
#Propietario       => E.F.P.                                              #
#Programa SEPC100  => CARGA DE ARCHIVO GENERADO POR OPERADORA BDNSAR      #
#Sistema           => SEP.                                                #
#Autor             => MAURO MUNIZ CABALLERO                               #
#Fecha             => 17 DE ENERO DE 2001 (Proceso Batch)                 #
#Modificado        => FERNANDO HERRERA HERNANDEZ                          #
#Fecha             => 21 DE JULIO DE 2004 (CIRC 28-8)                     #
#Modificado        => ISABEL FONSECA FRIAS                                #
#Fecha             => 22 DICIEMBRE 2004 (se agrego tabla tab_cod_op_afi)  #
#Modificado        => JOSE LUIS NEGRETE JUAREZ                            #
#Fecha             => 27 FEBRERO 2013  CPL-1200                           #
#                    (se cambia status_interno de 70 a 65 en traspaso())  #
###########################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_reg RECORD
        generar CHAR(20)
    END RECORD

    DEFINE hoy         DATE
    DEFINE xx_fecha    DATE
    DEFINE diaSig      DATE
    DEFINE enter       CHAR(1)
    DEFINE aux_pausa   CHAR(1)
    DEFINE sello       CHAR(24)
    DEFINE varchivo    CHAR(40)
    DEFINE carga       CHAR(50)
    DEFINE ejecuta     CHAR(100)
    DEFINE corr        CHAR(100)
    DEFINE total_reg   SMALLINT
    DEFINE g_plano1    SMALLINT
    DEFINE aceptar     SMALLINT
    DEFINE rechazar    SMALLINT
    DEFINE pendiente   SMALLINT
    DEFINE aclaracion  SMALLINT
    DEFINE asignar     SMALLINT
    DEFINE traspasar   SMALLINT
    DEFINE pend_90     SMALLINT
    DEFINE asig60      SMALLINT
    DEFINE asig61      SMALLINT
    DEFINE g_paramgrales RECORD LIKE seg_modulo.*
    DEFINE vgenerar    CHAR(20)
    DEFINE gv_cod_op   CHAR(2)
    DEFINE gv_ind_op   SMALLINT


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
        tip_aprob            CHAR(01)     ,
        fol_aprob            CHAR(10)     ,
        doc_aprob            CHAR(16)     ,
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
        tip_aprob_bd         CHAR(01)     ,
        fol_aprob_bd         CHAR(10)     ,
        doc_aprob_bd         CHAR(16)     ,
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

    DEFINE 
        bnd_proceso        SMALLINT,
        aux_status_interno SMALLINT,
        cuantos            INTEGER

    DEFINE
        c_periodo  CHAR(10),
        f_periodo  DATE

    DEFINE 
        reg_carta	   RECORD LIKE safre_af:int_ctr_carta.*,
        consulta_carta	   CHAR(120),
        g_usuario          CHAR(8)

    DEFINE
        longitud        SMALLINT,
        i               SMALLINT,
        i2              SMALLINT,
        archb           SMALLINT

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG(FGL_GETENV("USER")||".SEPC100.log")
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
    LET asig60     = 0
    LET asig61     = 0
    LET archb      = 0

    SELECT *, USER
    INTO   g_paramgrales.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'sep'

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE safre_tmp:plano1

        CREATE TABLE safre_tmp:plano1
        (n_registros  CHAR(570))
    WHENEVER ERROR STOP

    DATABASE safre_af

    INITIALIZE reg_carta.* TO NULL

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "SEPC1001" ATTRIBUTE(BORDER)
    DISPLAY " SEPC100      CARGA ARCHIVO RESPUESTA CERTIF. (SOLIC. REG.SEP)                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    DISPLAY g_paramgrales.ruta_rescate AT 7,10

    INPUT BY NAME g_reg.generar
        AFTER FIELD generar
        IF g_reg.generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        SELECT nombre_archivo
        INTO   varchivo
        FROM   afi_ctr_arh_reg
        WHERE  nombre_archivo = g_reg.generar

        IF STATUS <> NOTFOUND THEN
            ERROR "ARCHIVO YA PROCESADO"
            SLEEP 1
            ERROR " "
            INITIALIZE g_reg.generar TO NULL
            CLEAR FORM
            NEXT FIELD generar
        END IF

        LET carga = NULL
        LET carga = g_paramgrales.ruta_rescate CLIPPED,"/",g_reg.generar CLIPPED

        WHENEVER ERROR CONTINUE
            LOAD FROM carga INSERT INTO safre_tmp:plano1
        WHENEVER ERROR STOP

        SELECT COUNT(*)
        INTO   g_plano1
        FROM   safre_tmp:plano1

        IF g_plano1 IS NULL OR g_plano1 = 0 THEN
            ERROR "NOMBRE DE ARCHIVO INCORRECTO O VACIO, REVISE."
            SLEEP 5
            EXIT PROGRAM
        END IF

        ERROR "Procesando Informacion"
        CALL rescata_valores()

        EXIT PROGRAM

    END INPUT

END FUNCTION

FUNCTION rescata_valores()
#rv-----------------------

    CALL crea_tablas()
    CALL actualiza_datos()
    CALL revisa_datos()
    CALL despliega_resultados()
    CALL actualiza_est()

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

    LOAD FROM carga INSERT INTO safre_tmp:plano1

    SELECT count(*)
    INTO   cuantos
    FROM   safre_tmp:plano1

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
        DROP TABLE encabezado1
        DROP TABLE detalle1
        DROP TABLE sumario1
    WHENEVER ERROR STOP

    CREATE TABLE encabezado1
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

    CREATE TABLE detalle1
        (tipo_registro       CHAR(02)     ,
         contador_servicio   CHAR(10)     ,
         clave_operacion     CHAR(02)     ,
         nss_solicitud       CHAR(11)     ,
         curp_solicitud      CHAR(18)     ,
         rfc_trabajador      CHAR(13)     ,
         paterno             CHAR(40)     ,
         materno             CHAR(40)     ,
         nombres             CHAR(40)     ,
         fecha_nacimiento    CHAR(08)     ,
         clave_promotor      CHAR(10)     ,
         fec_recp_sol_afore  CHAR(08)     ,
         folio_solicitud     DECIMAL(16,0),
         sexo                SMALLINT     ,
         entidad_nacimiento  CHAR(02)     ,
         ind_infonavit       CHAR(01)     ,
         nacionalidad        CHAR(03)     ,
         tip_aprob           CHAR(01)     ,
         fol_aprob           CHAR(10)     ,
         doc_aprob           CHAR(16)     ,
         cod_err_ori         CHAR(04)     ,
         cve_afo_ced         CHAR(03)     ,
         folio_edo_cta       CHAR(08)     ,
         cod_operacion       CHAR(02)     ,
         diag_proceso        CHAR(15)     ,
         ident_lote_origen   CHAR(16)     ,
         nss_oficial         CHAR(11)     ,
         ind_nss_modificado  CHAR(01)     ,
         fec_emision_certif  CHAR(08)     ,
         curp_oficial        CHAR(18)     ,
         ind_curp_modif      CHAR(01)     ,
         n_rfc_bd            CHAR(13)     ,
         nombre_bd           CHAR(50)     ,
         nombre_pcanase      CHAR(50)     ,
         fena_bd             CHAR(08)     ,
         codven_bd           CHAR(10)     ,
         sexo_bd             SMALLINT     ,
         estadon_bd          CHAR(02)     ,
         fprimer_afil        CHAR(08)     ,
         falta_actual        CHAR(08)     ,
         cve_afore           CHAR(03)     ,
         nacionalidad_bd     CHAR(03)     ,
         tip_aprob_bd        CHAR(01)     ,
         fol_aprob_bd        CHAR(10)     ,
         doc_aprob_bd        CHAR(16)     ,
         fecha_recep_sello   CHAR(08)     ,
         hora_recep_sello    CHAR(02)     ,
         minuto_recep_sello  CHAR(02)     ,
         seg_recep_sello     CHAR(02)     ,
         cseg_recep_sello    CHAR(02)     ,
         consecutivo_recep   CHAR(08)     ,
         periodo             CHAR(06)     ,
         salario             DECIMAL(9,2))

    CREATE TABLE sumario1
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
        campo_12  CHAR(08),
        campo_13  CHAR(10),
        campo_14  CHAR(01),
        campo_15  CHAR(02),
        campo_16  CHAR(01),
        campo_17  CHAR(03),
        campo_18  CHAR(01),
        campo_19  CHAR(10),
        campo_20  CHAR(16),

        campo_21  CHAR(04),
        campo_22  CHAR(03),
        campo_23  CHAR(08),
        campo_24  CHAR(02),
        campo_25  CHAR(15),
        campo_26  CHAR(16),
        campo_27  CHAR(11),
        campo_28  CHAR(01),
        campo_29  CHAR(08),
        campo_30  CHAR(18),
        campo_31  CHAR(01),

        campo_32  CHAR(13),
        campo_33  CHAR(50),
        campo_34  CHAR(150),
        campo_35  CHAR(08),
        campo_36  CHAR(10),
        campo_37  CHAR(01),
        campo_38  CHAR(02),
        campo_39  CHAR(08),
        campo_40  CHAR(08),
        campo_41  CHAR(03),
        campo_42  CHAR(03),
        campo_43  CHAR(01),
        campo_44  CHAR(10),
        campo_45  CHAR(16),
        campo_46  CHAR(01),
        campo_47  CHAR(08),
        campo_48  CHAR(02),
        campo_49  CHAR(02),
        campo_50  CHAR(02),
        campo_51  CHAR(02),
        campo_52  CHAR(08),
        campo_53  CHAR(06),
        campo_54  CHAR(07),

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
    FROM   safre_tmp:plano1

    DECLARE cursor_1 CURSOR FOR 
    SELECT  * 
    FROM    safre_tmp:plano1

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

            INSERT INTO safre_tmp:encabezado1
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

        INSERT INTO safre_tmp:detalle1 
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
              # campo_46 ,
                campo_47 ,
                campo_48 ,
                campo_49 ,
                campo_50 ,
                campo_51 ,
                campo_52 ,
                campo_53 ,
                campo_54)
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

            INSERT INTO safre_tmp:sumario1 
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
    FROM   safre_tmp:encabezado1

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
            DISPLAY "Program stopped, Tipo de registro debe ser 01 en ENCABEZADO"
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Tipo de registro debe ser 01 en ENCABEZADO" AT 10,1
            PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR PROCESAR" FOR aux_pausa
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
            PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR PROCESAR" FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    IF rechazo_003 <> "10" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Identificador de operacion debe ser 10 en ENCABEZADO" 
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Identificador de operacion debe ser 10 en ENCABEZADO" AT 10,1
            PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR PROCESAR" FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    # SUMARIO #

    SELECT campo1 
    INTO   rechazo_09 
    FROM   safre_tmp:sumario1

    IF rechazo_09 <> "09" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Tipo de registro debe ser 09 en RESUMEN" 
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Tipo de registro debe ser 09 en RESUMEN" AT 10,1
            PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR PROCESAR" FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    # DETALLE #

    DECLARE cursor_2 CURSOR FOR 
    SELECT *
    FROM   safre_tmp:detalle1
    ORDER BY cod_operacion desc, diag_proceso desc

    FOREACH cursor_2 INTO reg_det.*
        IF reg_det.fec_emision_certif IS NOT NULL THEN 
            LET x_fecha = reg_det.falta_actual[5,6],"/",
                          reg_det.falta_actual[7,8],"/",
                          reg_det.falta_actual[1,4]
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
        SET    fentcons       = falta,
               fecha_1a_afil  = fpafil ,
               status         = reg_det.cod_operacion,
               status_captura = diag
        WHERE  n_seguro       = reg_det.nss_solicitud
        --AND    n_folio        = reg_det.folio_solicitud
        AND    tipo_solicitud = 6
        AND    status_interno in(30,40,50,55,90)

        LET sello = reg_det.fecha_recep_sello,
                    reg_det.hora_recep_sello,
                    reg_det.minuto_recep_sello,
                    reg_det.seg_recep_sello,
                    reg_det.cseg_recep_sello,
                    reg_det.consecutivo_recep
# issa (22-12-2004)
            SELECT cod_operacion,ind_funcion
              iNTO gv_cod_Op, gv_ind_op
              FROM tab_cod_op_afi
             WHERE cod_operacion = reg_det.cod_operacion

            CASE   gv_ind_op
              WHEN   1
                     CALL aceptado()
                     LET aceptar = aceptar + 1

                     IF gv_cod_op = 52 or 
                        gv_cod_op = 55 or
                        gv_cod_op = 57 or
                        gv_cod_op = 60  THEN 
                        LET asig60 = asig60 + 1
                     END IF
      
              WHEN   2
                     CALL rechazado()
                     LET rechazar = rechazar + 1
              WHEN   3
                     CALL pendiente_03 ()
                     LET reg_carta.docto_cod = 30229
                     CALL det_carta() #dc
                     LET pendiente = pendiente + 1
              WHEN   4
                     CALL aclarado()
                     LET reg_carta.docto_cod = 30204
                     CALL det_carta() #dc
                     LET aclaracion = aclaracion + 1

              WHEN   5
                     LET reg_carta.docto_cod = 30228
                     CALL det_carta() #dc
                     CALL pendiente_90()
                     LET asignar = asignar + 1
              WHEN  6
                     CALL traspaso()

                     IF gv_cod_op = 54 or
                        gv_cod_op = 56 or
                        gv_cod_op = 58 or
                        gv_cod_op = 61 or
                        gv_cod_op = 63 or
                        gv_cod_op = 65  THEN
                        LET asig61 = asig61 + 1
                     END IF
            END CASE

       { CASE reg_det.cod_operacion
            WHEN "01" CALL aceptado()
                      LET aceptar = aceptar + 1
            WHEN "02" CALL rechazado()
                      LET rechazar = rechazar + 1
            WHEN "03" CALL pendiente_03()
                      LET pendiente = pendiente + 1
                      LET reg_carta.docto_cod = 30229
                      CALL det_carta() #dc
            WHEN "04" CALL aceptado()
                      LET aceptar = aceptar + 1
            WHEN "08" CALL aclarado()
                      LET aclaracion = aclaracion + 1
                      LET reg_carta.docto_cod = 30204
                      CALL det_carta() #dc
            WHEN "09" CALL aceptado()
                      LET aceptar = aceptar + 1
            WHEN "10" CALL aceptado()
                      LET aceptar = aceptar + 1
            WHEN "11" CALL rechazado()
                      LET rechazar = rechazar + 1
            WHEN "12" CALL rechazado()
                      LET rechazar = rechazar + 1
            WHEN "15" CALL aceptado()
                      LET aceptar = aceptar + 1
            WHEN "16" CALL aceptado()
                      LET aceptar = aceptar + 1
            WHEN "90" CALL pendiente_90()
                      LET asignar = asignar + 1
                      LET reg_carta.docto_cod = 30228
                      CALL det_carta() #dc
            WHEN "60" CALL aceptado()
                      LET asig60 = asig60 + 1
            WHEN "61" CALL traspaso()
                      LET asig61 = asig61 + 1
            WHEN "62" CALL aceptado()
                      LET aceptar = aceptar + 1
            WHEN "63" CALL traspaso()
                      LET asig61 = asig61 + 1
            WHEN "64" CALL aceptado()
                      LET aceptar = aceptar + 1
            WHEN "65" CALL traspaso()
                      LET asig61 = asig61 + 1
            WHEN "52" CALL aceptado()
                      LET aceptar = aceptar + 1
                      LET asig60 = asig60 + 1
            WHEN "55" CALL aceptado()
                      LET asig60 = asig60 + 1
            WHEN "57" CALL aceptado()
                      LET asig60 = asig60 + 1
            WHEN "54" CALL traspaso()
                      LET asig61 = asig61 + 1
            WHEN "56" CALL traspaso()
                      LET asig61 = asig61 + 1
            WHEN "58" CALL traspaso()
                      LET asig61 = asig61 + 1
        END CASE}

        #CALL det_carta() #dc

        IF reg_det.cod_operacion = "02" OR
           reg_det.cod_operacion = "11" OR
           reg_det.cod_operacion = "12" OR
           reg_det.cod_operacion = "03" OR
           reg_det.cod_operacion = "08" THEN
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
                    SET    status_interno = 42
                    WHERE  n_seguro = reg_det.nss_solicitud
                    --AND    n_folio = reg_det.folio_solicitud
                    AND    tipo_solicitud = 6
                    AND    status_interno = 40
                    LET reg_carta.docto_cod = 30209
                    CALL det_carta() #dc
                ELSE
                   IF uno <> '020' THEN
                      LET reg_carta.docto_cod = 30233
                      CALL det_carta() #dc
                   ELSE
                      LET reg_carta.docto_cod = 30207
                      CALL det_carta() #dc
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
            INTO x_reg.*
            FROM tab_rdeta
            WHERE rdeta_cod = cin
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
    SET    status_interno = 60,
           finicta = diaSig,
           salario_actual = reg_det.salario,
           fecha_actualiza_sa = f_periodo,
           sello_electronico = sello
    WHERE  n_seguro = reg_det.nss_solicitud
    --AND    n_folio = reg_det.folio_solicitud
    AND    tipo_solicitud = 6
    AND    status_interno in(30,50,55,90)

    --CALL inserta_afi_ctr(60)

END FUNCTION

FUNCTION traspaso()
#t-----------------

    CALL habil_siguiente(xx_fecha) RETURNING diaSig

    UPDATE afi_solicitud
    SET    status_interno = 65,   # -- CPL-1200 Se modifica el estatus interno para que ser aperturada por el porgrama Registro
           finicta = diaSig,
           salario_actual = reg_det.salario,
           fecha_actualiza_sa = f_periodo,
           sello_electronico = sello
    WHERE  n_seguro = reg_det.nss_solicitud
    --AND    n_folio = reg_det.folio_solicitud
    AND    tipo_solicitud = 6
    AND    status_interno in(30,50,55,90)

    --CALL inserta_afi_ctr(70)

END FUNCTION

FUNCTION rechazado()
#r------------------

    UPDATE afi_solicitud
    SET    status_interno = 40
    WHERE  n_seguro = reg_det.nss_solicitud
    --AND    n_folio = reg_det.folio_solicitud
    AND    tipo_solicitud = 6
    AND    status_interno in(30,50,55,90)

    LET aux_status_interno = 40

    --CALL inserta_afi_ctr(40)

END FUNCTION

FUNCTION pendiente_03()
#p---------------------

    UPDATE afi_solicitud 
    SET    status_interno = 50
    WHERE  n_seguro = reg_det.nss_solicitud
    --AND    n_folio = reg_det.folio_solicitud
    AND    tipo_solicitud = 6
    AND    status_interno in(30,50,55,90)

    LET aux_status_interno = 50

    --CALL inserta_afi_ctr(50)

END FUNCTION

FUNCTION aclarado()
#ac----------------

    UPDATE afi_solicitud
    SET    status_interno = 55
    WHERE  n_seguro = reg_det.nss_solicitud
    --AND    n_folio = reg_det.folio_solicitud
    AND    tipo_solicitud = 6
    AND    status_interno in(30,50,55,90)

    LET aux_status_interno = 55

    --CALL inserta_afi_ctr(55)

END FUNCTION

FUNCTION pendiente_90()
#p---------------------

    UPDATE afi_solicitud 
    SET    status_interno = 90
    WHERE  n_seguro = reg_det.nss_solicitud
    --AND    n_folio = reg_det.folio_solicitud
    AND    tipo_solicitud = 6
    AND    status_interno in(30,50,55,90)

    LET aux_status_interno = 50

    --CALL inserta_afi_ctr(90)

END FUNCTION

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
             tip_aprob             CHAR(01)     ,
             fol_aprob             CHAR(10)     ,
             doc_aprob             CHAR(16)     ,
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
             tip_aprob_bd          CHAR(01)     ,
             fol_aprob_bd          CHAR(10)     ,
             doc_aprob_bd          CHAR(16)     ,
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


    IF reg_det.falta_actual MATCHES ' *' THEN
        LET v_fecha1 = NULL
    ELSE
        LET v_fecha1 = reg_det.falta_actual[05,06],"/",
                       reg_det.falta_actual[07,08],"/",
                       reg_det.falta_actual[01,04]
    END IF

    SELECT "X" 
    FROM   afi_rechaza_cert
    WHERE  n_folio        = reg_det.folio_solicitud
    AND    tipo_solicitud = 6
    AND    n_seguro       = reg_det.nss_solicitud
    AND    f_rechazo      = v_fecha
    AND    rdeta_cod      = x_reg.rdeta_cod

    IF SQLCA.SQLCODE = 100 THEN
        IF x_reg.rdeta_cod = 14 THEN
            INSERT INTO afi_rechaza_cert
            VALUES (reg_det.folio_solicitud,
                    1,
                    reg_det.nss_solicitud,
                    x_reg.rdeta_cod,
                    v_fecha,
                    v_fecha1,
                    reg_det.cve_afore,
                    x_reg.rdeta_desc_c,
                    reg_det.nombre_pcanase)
        ELSE
            INSERT INTO afi_rechaza_cert
            VALUES (reg_det.folio_solicitud,
                    1,
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

FUNCTION despliega_resultados()
#dr----------------------------

    DEFINE total_resp SMALLINT

    LET total_resp = aceptar + rechazar + pendiente + aclaracion + 
                     pend_90 + asig60   + asig61    + asignar

    IF bnd_proceso THEN
        DISPLAY "                TOTAL REGISTROS RECIBIDOS                "

        DISPLAY "Total de Registros del lote     : ", total_resp USING "#######&"

        DISPLAY "Registros certificados     : ", aceptar    USING "#######&"

        DISPLAY "Registros rechazados       : ", rechazar   USING "#######&"

        DISPLAY "Registros pendientes       : ", pendiente  USING "#######&"

        DISPLAY "Registros en aclaracion    : ", aclaracion USING "#######&"

        DISPLAY "Registros pendientes asig. : ", pend_90    USING "#######&"

        DISPLAY "Registros asignados cert.  : ", asig60     USING "#######&" 

        DISPLAY "Registros asignados trasp. : ", asig61     USING "#######&" 

        DISPLAY "Registros pend. proc. ext. : ", asignar    USING "#######&" 
    ELSE
        DISPLAY "                TOTAL REGISTROS RECIBIDOS                 " AT 7,10 ATTRIBUTE ( REVERSE )

        DISPLAY "Total de Registros del lote     : ",
            total_resp USING "#######&" AT 9,02 ATTRIBUTE ( CYAN )

        DISPLAY "Reg certificados     : ",
            aceptar USING "#######&" AT 10,02 ATTRIBUTE ( CYAN )

        DISPLAY "Reg rechazados       : ",
            rechazar USING "#######&" AT 11,02 ATTRIBUTE ( CYAN ) 

        DISPLAY "Reg pendientes       : ",
            pendiente USING "#######&" AT 12,02 ATTRIBUTE ( CYAN ) 

        DISPLAY "Reg en aclaracion    : ",
            aclaracion USING "#######&" AT 13,02 ATTRIBUTE ( CYAN ) 

        DISPLAY "Reg pendientes asig. : ",
            pend_90 USING "#######&" AT 14,02 ATTRIBUTE ( CYAN ) 

        DISPLAY "Reg asignados cert.  : ",
            asig60 USING "#######&" AT 15,02 ATTRIBUTE ( CYAN )

        DISPLAY "Reg asignados trasp. : ",
            asig61 USING "#######&" AT 16,02 ATTRIBUTE ( CYAN )

        DISPLAY "Reg pend. proc. ext. : ", 
            asignar USING "#######&" AT 10,40 ATTRIBUTE ( CYAN )

        PROMPT "Presione <enter> para continuar " FOR enter
    END IF

    LET aceptar   = aceptar   + asig60  + asig61
    LET pendiente = pendiente + asignar

    INSERT INTO afi_ctr_arh_reg 
    VALUES (reg_bat.nombre_archivo,
            aceptar,
            rechazar,
            pendiente,
            aclaracion,
            pend_90,
            hoy)

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
        AND    edd.nombre_archivo = g_reg.generar
        AND    edd.status_interno = st_int
        AND    edd.tipo_solicitud = 6

        IF SQLCA.SQLCODE <> 0 THEN
            INSERT INTO est_det_diario
            VALUES (hoy,
                    st_int,
                    total,
                    1,
                    g_reg.generar)
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

    DEFINE fecha_envio DATE
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
    AND    p.tipo_solicitud = 6

    SELECT p.fecha_envio, p.paterno, p.materno, p.nombres
    INTO   fecha_envio, paterno, materno, nombres
    FROM   afi_solicitud p
    WHERE  p.n_seguro       = reg_det.nss_solicitud
    AND    p.n_folio        = reg_det.folio_solicitud
    AND    p.tipo_solicitud = 6

    LET nom_comp               = paterno CLIPPED, "$",
                                 materno CLIPPED, "$",
                                 nombres CLIPPED

    LET reg_det.nombre_pcanase = reg_det.nombre_pcanase CLIPPED
    LET nom_comp               = nom_comp CLIPPED

    IF nom_comp <> reg_det.nombre_pcanase THEN
       LET ind_nombre = 1
    END IF

    SELECT p.fecha_envio
    INTO   fecha_envio
    FROM   afi_solicitud p
    WHERE  p.n_seguro = reg_det.nss_solicitud
    AND    p.n_folio = reg_det.folio_solicitud
    AND    p.tipo_solicitud = 6

    INSERT INTO afi_ctr_solicitud
    VALUES(1                           ,
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
           reg_det.tip_aprob_bd        ,
           reg_det.fol_aprob_bd        ,
           reg_det.doc_aprob_bd        ,
           ind_envio                   ,
           ind_nombre                  ,
           reg_det.cod_operacion       ,
           reg_det.diag_proceso        ,
           g_usuario                   ,
           today                       )

END FUNCTION

FUNCTION actualiza_bat_f(v_folio)
#ab-----------------------------

define v_cat          CHAR(600),
       vv_fecha_log   CHAR(030),
       vv_prog        CHAR(010),
       paso           CHAR(100)

define v_fecha_log DATETIME YEAR TO SECOND

define v_folio  integer
define reg_ruta RECORD LIKE seg_modulo.*

SELECT A.*
INTO reg_ruta.*
FROM  seg_modulo A
WHERE modulo_cod = "bat"
 
UPDATE bat_ctr_operacion
set    folio      = NULL ,      
       estado_cod = 4    ,
       fecha_fin  = CURRENT,
       nom_archivo = reg_bat.nombre_archivo
WHERE pid         = reg_bat.pid
and   proceso_cod = reg_bat.proceso_cod
and   opera_cod   = reg_bat.opera_cod

UPDATE bat_ctr_proceso
set    folio       = NULL ,      
       estado_cod  = 4    ,
       fecha_fin   = CURRENT
WHERE  pid         = reg_bat.pid
and    proceso_cod = reg_bat.proceso_cod

UPDATE bat_tmp_predecesor
SET    bandera_ejecuta  = 1
WHERE  pid_prod         = reg_bat.pid
AND    proceso_cod_prod = reg_bat.proceso_cod
AND    opera_cod_prod   = reg_bat.opera_cod

LET v_fecha_log = CURRENT
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
#################################################################
FUNCTION det_carta() #dc

   LET reg_carta.nss            = reg_det.nss_solicitud
   LET reg_carta.n_folio        = reg_det.folio_solicitud  
   LET reg_carta.tipo_solicitud = 6
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
#################################################################

