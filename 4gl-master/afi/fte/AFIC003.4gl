#Fecha             => 21 DE JULIO DE 2004                                   #
#Modificado        => FERNANDO HERRERA HERNANDEZ (CIRC 28 - 8)              #
#Fecha             => 21 DE JULIO DE 2004                                   #
#Modificado        => FERNANDO HERRERA HERNANDEZ (CIRC 28 - 13)             #
#Fecha             => 13 DE NOVIEMBRE DE 2006 (Se elimina cop op 06         #
#Modificado        => FERNANDO HERRERA HERNANDEZ ( CIRC 28 - 16)            #
#Fecha             => 30 DE MARZO DE 2007                                   #
#Modificado        => FERNANDO HERRERA HERNANDEZ ( CIRC 28 - 18)            #
#Fecha             => 15 DE AGOSTO DE 2008                                  #
#                  => 22 Dic 2009 FOLIO EDOCTA - EDUARDO RESENDIZ MEDINA    #
#                  => 03 Agosto 2010 CIRCULAR UNICA EDUARDO RESENDIZ MEDINA #
#                  => 24 Dic 2010 Cambios layout EDUARDO RESENDIZ MEDINA    #
#                  => 28 Marzo 2011 EDUARDO RESENDIZ MEDINA   TAA X CURP    #
#CPL-1875          => 09/03/2015 FSR CUO II                                 #
#############################################################################
# CPL-1875         -  Presentar cifras de movimientos por codigo operacion  #
#                  -  invalido JGHM Mar 2015                                #
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
        observacion        CHAR(30)

    DEFINE hoy         DATE
    DEFINE xx_fecha    DATE
    DEFINE aux_pausa   CHAR(1)
    DEFINE enter       CHAR(1)
    DEFINE g_usuario   CHAR(8)
    DEFINE hora        CHAR(8)
    DEFINE varchivo    CHAR(40)
    DEFINE vgenerar    CHAR(40)
    DEFINE carga       CHAR(50)
    DEFINE ejecuta     CHAR(100)
    DEFINE corr        CHAR(100)
    DEFINE G_IMPRE     CHAR(300)
    DEFINE gimpresion  CHAR(300)
    DEFINE total_reg   SMALLINT
    DEFINE g_plano2    SMALLINT
    DEFINE aceptar     SMALLINT
    DEFINE aceptar_f   SMALLINT
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
        --rfc_trabajador       CHAR(13)   ,   --24 dic 2010
        tipo_trasp           CHAR(02)     ,      --taa x curp
        paterno              CHAR(40)     ,
        materno              CHAR(40)     ,
        nombres              CHAR(40)     ,
        --fecha_nacimiento     CHAR(08)   ,   --24 dic 2010
        clave_promotor       CHAR(10)     ,
        fec_recp_sol_afore   CHAR(08)     ,
        folio_solicitud      DECIMAL(10,0),                       #CPL-1875 cambio 
        --sexo                 SMALLINT   ,   --24 dic 2010
        cve_afore_ced         CHAR(03)    ,     --24 dic 2010 nueva
        --entidad_nacimiento   CHAR(02)     ,
        --ind_infonavit        CHAR(01)     ,   --24 dic 2010
        --nacionalidad         CHAR(03)     ,   --24 dic 2010
        --tip_prob             CHAR(01)     ,   --24 dic 2010
        --fol_prob             CHAR(10)     ,   --24 dic 2010
        --doc_prob             CHAR(16)     ,   --24 dic 2010
        --cod_err_ori          CHAR(04)     ,   --24 dic 2010
        folio_saftv          CHAR(10)     ,                       -- CPL-1875 ahora espacios 
        #cve_afo_ced          CHAR(03)     ,
        --id_reg_reenvio       CHAR(01)     ,      ---circ unica
        cod_operacion        CHAR(02)     ,
        diag_proceso         CHAR(15)     ,
        ident_lote_origen    CHAR(16)     ,
        --nss_oficial          CHAR(11)     ,     --24 dic 2010
        cve_rend_neto        CHAR(01)     ,                       -- CPL-1875 ahora espacios
        
        #ind_nss_modificado   CHAR(01)     ,
        
        fec_emision_certif   CHAR(08)     ,
        --curp_oficial         CHAR(18)     ,    --24 dic 2010
        --ind_curp_modif       CHAR(01)     ,    --24 dic 2010
        --n_rfc_bd             CHAR(13)     ,    --24 dic 2010
        --nombre_bd            CHAR(50)     ,    --24 dic 2010
        --nombre_pcanase       CHAR(50)     ,    --24 dic 2010
        --fena_bd              CHAR(08)     ,    --24 dic 2010
        --codven_bd            CHAR(10)     ,    --24 dic 2010
        --sexo_bd              SMALLINT     ,    --24 dic 2010
        --estadon_bd           CHAR(02)     ,    --24 dic 2010
        fprimer_afil         CHAR(08)     ,
        falta_actual         CHAR(08)     ,
        cve_afore            CHAR(03)     ,
        --nacionalidad_bd      CHAR(03)     ,    --24 dic 2010
        --tip_prob_bd          CHAR(01)     ,    --24 dic 2010
        --fol_prob_bd          CHAR(10)     ,    --24 dic 2010
        --doc_prob_bd          CHAR(16)     ,    --24 dic 2010
        fecha_recep_sello    CHAR(08)     ,
        hora_recep_sello     CHAR(02)     ,
        minuto_recep_sello   CHAR(02)     ,
        seg_recep_sello      CHAR(02)     ,
        cseg_recep_sello     CHAR(02)     ,
        consecutivo_recep    CHAR(08)     ,
        periodo              CHAR(06)     ,
        salario              DECIMAL(7,2) ,
        --fol_sol              CHAR(08)     ,     --24 dic 2010
        fecha_fin_rend       CHAR(08)     ,
        folio_edo_cta        CHAR(20)     ,
        --cuatrim_emision      DECIMAL(5,0) ,     --24 dic 2010
        {grado_riesgo         CHAR(02)     ,
        vector_riesgo        CHAR(08)     ,
        id_reg_env_verif     CHAR(01)     ,}
        id_bono_issste       CHAR(01),
        folio_traspaso       CHAR(20)              ##--  CPL-1875 Nuevo 
    END RECORD

    DEFINE
        x_fecha,f1,f2,f3,
        xfecha_nac          CHAR(10),
        falta,fpafil,fnbd,
        vfecha_nac          DATE

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
        i          SMALLINT,
        i2         SMALLINT,
        archb      SMALLINT,
        nom_arch   CHAR(20),
        sello      CHAR(24)

     DEFINE
        id_env_img CHAR(1),
        vst_int    SMALLINT        

    DEFINE vnseguro CHAR(11)  --taa x curp

    DEFINE   gs_inval         SMALLINT    -- CPL1875 Mar 2015 
END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG('AFIC003.log')
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

    LET    gs_inval      =   0
END MAIN

FUNCTION inicio()
#i---------------

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
    WHERE  modulo_cod = 'afi'

    LET aceptar    = 0
    LET aceptar_f  = 0
    LET rechazar   = 0
    LET aclaracion = 0
    LET pendiente  = 0
    LET archb      = 0

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE safre_tmp:plano2

        CREATE TABLE safre_tmp:plano2
            --(n_registros CHAR(730))
            (n_registros CHAR(400))       --taa x curp

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
         tipo_trasp                   CHAR(02)     ,      --taa x curp
         paterno                      CHAR(40)     ,
         materno                      CHAR(40)     ,
         nombres                      CHAR(40)     ,
         clave_promotor               CHAR(10)     ,
         fec_recp_sol_afore           CHAR(08)     ,
         folio_solicitud              DECIMAL(10,0),
         cve_afore_ced                 CHAR(03)    ,
         folio_saftv                  CHAR(10)     ,      --CPL-1875 espacios
         cod_operacion                CHAR(02)     ,
         diag_proceso                 CHAR(15)     ,
         ident_lote_origen            CHAR(16)     ,
         cve_rend_neto                CHAR(01)     ,      --CPL-1875 espacios
         fec_emision_certif           CHAR(08)     ,
         fprimer_afil                 CHAR(08)     ,
         falta_actual                 CHAR(08)     ,
         cve_afore                    CHAR(03)     ,
         fecha_recep_sello            CHAR(08)     ,
         hora_recep_sello             CHAR(02)     ,
         minuto_recep_sello           CHAR(02)     ,
         seg_recep_sello              CHAR(02)     ,
         cseg_recep_sello             CHAR(02)     ,
         consecutivo_recep            CHAR(08)     ,
         periodo                      CHAR(06)     ,
         salario                      DECIMAL(7,2) ,
         fecha_fin_rend               CHAR(08)     ,
         folio_edo_cta                CHAR(20)     ,
         id_bono_issste               CHAR(01)     ,
         folio_traspaso               CHAR(20)     )      --CPL-1875 nuevo 

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

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIC0011" ATTRIBUTE(BORDER)

    DISPLAY " AFIC003  CARGA ARCHIVO CERTIFICACION PROCESAR (SOLIC. TRASP.)                 " AT 3,1 ATTRIBUTE(REVERSE)

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

        LET nom_arch = g_reg.generar CLIPPED
        LET longitud = LENGTH(g_reg.generar CLIPPED)

        FOR i = 1 TO longitud
            IF g_reg.generar[i] = "." THEN
                LET i  = i + 1
                LET i2 = i + 2

                IF g_reg.generar[i,i2] = 'taa' OR g_reg.generar[i,i2] = 'TAA' THEN
                    LET g_reg.generar = g_reg.generar[i,i2]
                    LET archb = 1
                    EXIT FOR
                ELSE
                    PROMPT 
                    "El archivo no es de -Certificacion por Traspaso-.",
                    " <ENTER> para continuar." FOR enter
                    LET archb = 0
                    NEXT FIELD generar 
                END IF
            END IF
        END FOR

        SELECT nombre_archivo
        INTO   varchivo
        FROM   afi_ctr_arh_tra
        WHERE  @nombre_archivo = vgenerar

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
            LET carga = g_paramgrales.ruta_rescate CLIPPED,"/",
            nom_arch CLIPPED

        WHENEVER ERROR CONTINUE
            LOAD FROM carga INSERT INTO safre_tmp:plano2
        WHENEVER ERROR STOP
DISPLAY carga SLEEP 1
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
        PROMPT "El archivo no es de -Certificacion por Traspaso-.",
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
        --carga_reg            CHAR(730)
        carga_reg            CHAR(400)      --taa x curp

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
        campo_06             CHAR(02),     --taa x curp
        campo_07             CHAR(40),
        campo_08             CHAR(40),
        campo_09             CHAR(40),
        campo_10             CHAR(10),
        campo_11             CHAR(08),
        campo_12             CHAR(10),
        campo_13             CHAR(03),
        campo_14             CHAR(10),
        campo_15             CHAR(02),
        campo_16             CHAR(15),
        campo_17             CHAR(16),
        campo_18             CHAR(01),
        campo_19             CHAR(08),
        campo_20             CHAR(08),
        campo_21             CHAR(08),
        campo_22             CHAR(03),
        campo_23             CHAR(08),
        campo_24             CHAR(02),
        campo_25             CHAR(02),
        campo_26             CHAR(02),
        campo_27             CHAR(02),
        campo_28             CHAR(08),
        campo_29             CHAR(06),
        campo_30             CHAR(07),
        campo_31             CHAR(08),
        campo_32             CHAR(20),
        campo_33             CHAR(01),
        campo_34             CHAR(20),                   -- CPL-1875 nuevo         

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
            LET campo_06 = carga_reg[044,045]     --taa x curp
            LET campo_07 = carga_reg[046,085]
            LET campo_08 = carga_reg[086,125]
            LET campo_09 = carga_reg[126,165]
            LET campo_10 = carga_reg[166,175]
            LET campo_11 = carga_reg[176,183]
            LET campo_12 = carga_reg[184,193]
            LET campo_13 = carga_reg[194,196]
            LET campo_14 = carga_reg[197,206]     --CPL-1875 espacios
            LET campo_15 = carga_reg[207,208]
            LET campo_16 = carga_reg[209,223]
            LET campo_17 = carga_reg[224,239]
            LET campo_18 = carga_reg[240,240]     --CPL-1875 espacios
            LET campo_19 = carga_reg[241,248]
            LET campo_20 = carga_reg[249,256]
            LET campo_21 = carga_reg[257,264]
            LET campo_22 = carga_reg[265,267]
            LET campo_23 = carga_reg[268,275]
            LET campo_24 = carga_reg[276,277]
            LET campo_25 = carga_reg[278,279]
            LET campo_26 = carga_reg[280,281]
            LET campo_27 = carga_reg[282,283]
            LET campo_28 = carga_reg[284,291]
            LET campo_29 = carga_reg[292,297]
            LET campo_30 = carga_reg[298,304]
            LET campo_31 = carga_reg[305,312]
            LET campo_32 = carga_reg[313,332]
            LET campo_33 = carga_reg[333,333]
            LET campo_34 = carga_reg[334,353]        ##--- CPL-1875             

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
                    campo_34 
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
        observacion CHAR(30),
        diagnostico CHAR(03),
        v_fecha2    CHAR(10)

    DEFINE 
        rechazo_lote	 CHAR(3) ,
        rechazo_deta	 CHAR(3) ,
        l_reg		 RECORD LIKE tab_rch_lote.* ,
        x_reg		 RECORD LIKE tab_rdeta.* ,
        aux_pausa	 CHAR(1)

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
        #diag CHAR(3),
        l_status_int SMALLINT

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
        DISPLAY "ERROR DE PROCESO, NO PUEDE CONTINUAR " 
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

{#24 dic 2010
        IF reg_det.fena_bd IS NOT NULL THEN 
            LET f3 = reg_det.fena_bd[5,6],"/",
                     reg_det.fena_bd[7,8],"/",
                     reg_det.fena_bd[1,4]
        ELSE
            LET f3 = NULL
        END IF 

        IF reg_det.fecha_nacimiento IS NOT NULL THEN 
            LET xfecha_nac  = reg_det.fecha_nacimiento[5,6],"/",
                              reg_det.fecha_nacimiento[7,8],"/",
                              reg_det.fecha_nacimiento[1,4]
        ELSE
            LET xfecha_nac = NULL
        END IF 
}
        LET f3 = NULL            --24 dic 2010
        LET xfecha_nac = NULL    --24 dic 2010

        LET falta      = f1
        LET fpafil     = f2
        LET fnbd       = f3
        LET xx_fecha   = x_fecha
        LET vfecha_nac = xfecha_nac

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

        IF reg_det.tipo_trasp = '01' THEN      --taa x curp
           UPDATE afi_solicitud 
           SET    fentcons       = xx_fecha,
                  status         = reg_det.cod_operacion,
                  status_captura = diag
           WHERE  n_seguro       = reg_det.nss_solicitud
           AND    n_folio        = reg_det.folio_solicitud
           AND    tipo_solicitud in(2,3,4)
           AND    status_interno in(30)
        ELSE
           SELECT n_seguro
           INTO   vnseguro
           FROM   afi_solicitud 
           WHERE  n_unico        = reg_det.curp_solicitud
           AND    n_folio        = reg_det.folio_solicitud
           AND    tipo_solicitud in(15)
           AND    status_interno in(30)

           LET reg_det.nss_solicitud = vnseguro

           UPDATE afi_solicitud 
           SET    fentcons       = xx_fecha,
                  status         = reg_det.cod_operacion,
                  status_captura = diag
           WHERE  n_unico        = reg_det.curp_solicitud
           AND    n_folio        = reg_det.folio_solicitud
           AND    tipo_solicitud in(15)
           AND    status_interno in(30)
        END IF

        LET sello = reg_det.fecha_recep_sello ,
                    reg_det.hora_recep_sello ,
                    reg_det.minuto_recep_sello ,
                    reg_det.seg_recep_sello ,
                    reg_det.cseg_recep_sello,
                    reg_det.consecutivo_recep 

        IF reg_det.tipo_trasp = '01' THEN      --taa x curp
           UPDATE afi_folio_edocta
           SET    fecha_respuesta = TODAY,
                  --grado_riesgo    = reg_det.grado_riesgo,   ---circ unica
                  --vector_riesgo   = reg_det.vector_riesgo,
                  --id_reg_env_verif= reg_det.id_reg_env_verif,
                  id_bono_iss     = reg_det.id_bono_issste,
                  cod_operacion   = reg_det.cod_operacion,
                  diag_proceso    = diag
           WHERE  nss             = reg_det.nss_solicitud
           AND    n_folio         = reg_det.folio_solicitud
           --AND    tipo_solicitud  = 2
           AND    tipo_solicitud  IN (2)
        ELSE
           UPDATE afi_folio_edocta
           SET    fecha_respuesta = TODAY,
                  --grado_riesgo    = reg_det.grado_riesgo,   ---circ unica
                  --vector_riesgo   = reg_det.vector_riesgo,
                  --id_reg_env_verif= reg_det.id_reg_env_verif,
                  id_bono_iss     = reg_det.id_bono_issste,
                  cod_operacion   = reg_det.cod_operacion,
                  diag_proceso    = diag
           WHERE  nss             = vnseguro
           AND    n_folio         = reg_det.folio_solicitud
           --AND    tipo_solicitud  = 2
           AND    tipo_solicitud  IN (15)             --taa x curp
        END IF
# 29-11-2004
        CASE reg_det.cod_operacion
            WHEN '06' #Aceptada
                CALL cod_op06()
            WHEN '05' #Proceso Val Firma
                --CALL cod_op05()
                CALL cod_op06()   #Aceptada Validacion CIRC UNICA
            WHEN '02' #Rechazo
                CALL cod_op02()
            WHEN '03' #Pendiente
                CALL cod_op03()
            OTHERWISE 
                LET  gs_inval             =   gs_inval    + 1   -- CPL1875 Mar 2015
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
            IF x_reg.tipo_rechazo = 'D' THEN
                IF reg_det.tipo_trasp = '01' THEN      --taa x curp
                   UPDATE afi_solicitud
                   SET    status_interno = 42
                   WHERE  n_seguro       = reg_det.nss_solicitud
                   AND    n_folio        = reg_det.folio_solicitud
                   AND    tipo_solicitud = 2
                   AND    status_interno = 40
                ELSE
                   UPDATE afi_solicitud
                   SET    status_interno = 42
                   WHERE  n_unico        = reg_det.curp_solicitud
                   AND    n_folio        = reg_det.folio_solicitud
                   AND    tipo_solicitud = 15
                   AND    status_interno = 40
                END IF
            END IF

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

FUNCTION cod_op06()
#co6---------------
    IF reg_det.tipo_trasp = '01' THEN      --taa x curp
       UPDATE afi_solicitud
       SET    status_interno     = 65,#cambia a 65 para que se aperturen mas tardar 2 días desp de la cert  
              fecha_actualiza_sa = f_periodo,
              sello_electronico  = sello ,
              fecha_1a_afil      = fpafil,
              salario_actual     = reg_det.salario,
              --n_rfc              = reg_det.rfc_trabajador,       --24 dic 2010
              --fena               = vfecha_nac,                   --24 dic 2010
              --estadon            = reg_det.entidad_nacimiento,   --24 dic 2010
              --tip_prob           = reg_det.tip_prob,             --24 dic 2010
              --fol_prob           = reg_det.fol_prob,             --24 dic 2010
              --doc_prob           = reg_det.doc_prob,             --24 dic 2010
              --nacionalidad       = reg_det.nacionalidad,         --24 dic 2010
              n_folio            = reg_det.folio_solicitud,
              --ind_infonavit      = reg_det.ind_infonavit,        --24 dic 2010
              n_unico            = reg_det.curp_solicitud,
              paterno            = reg_det.paterno,
              materno            = reg_det.materno,
              nombres            = reg_det.nombres
       WHERE  n_seguro           = reg_det.nss_solicitud
       AND    n_folio            = reg_det.folio_solicitud
       AND    tipo_solicitud    IN (2,3,4)
       AND    status_interno    IN (30)
    ELSE
       UPDATE afi_solicitud
       SET    status_interno     = 65,#cambia a 65 para que se aperturen mas tardar 2 días desp de la cert  
              fecha_actualiza_sa = f_periodo,
              sello_electronico  = sello ,
              fecha_1a_afil      = fpafil,
              salario_actual     = reg_det.salario,
              --n_rfc              = reg_det.rfc_trabajador,       --24 dic 2010
              --fena               = vfecha_nac,                   --24 dic 2010
              --estadon            = reg_det.entidad_nacimiento,   --24 dic 2010
              --tip_prob           = reg_det.tip_prob,             --24 dic 2010
              --fol_prob           = reg_det.fol_prob,             --24 dic 2010
              --doc_prob           = reg_det.doc_prob,             --24 dic 2010
              --nacionalidad       = reg_det.nacionalidad,         --24 dic 2010
              n_folio            = reg_det.folio_solicitud,
              --ind_infonavit      = reg_det.ind_infonavit,        --24 dic 2010
              n_unico            = reg_det.curp_solicitud,
              paterno            = reg_det.paterno,
              materno            = reg_det.materno,
              nombres            = reg_det.nombres
       WHERE  n_unico            = reg_det.curp_solicitud
       AND    n_folio            = reg_det.folio_solicitud
       AND    tipo_solicitud    IN (15)
       AND    status_interno    IN (30)
    END IF
    LET aceptar = aceptar + 1

    --CALL inserta_afi_ctr(70)
    CALL inserta_afi_ctr(65)

END FUNCTION

FUNCTION cod_op05()
#------------------

    IF reg_det.tipo_trasp = '01' THEN      --taa x curp
       UPDATE afi_solicitud
       SET    status_interno     = 65,
              fecha_actualiza_sa = f_periodo,
              sello_electronico  = sello ,
              fecha_1a_afil      = fpafil,
              salario_actual     = reg_det.salario,
              --n_rfc              = reg_det.rfc_trabajador,      --24 dic 2010
              --fena               = vfecha_nac,                  --24 dic 2010
              --estadon            = reg_det.entidad_nacimiento,  --24 dic 2010
              --tip_prob           = reg_det.tip_prob,            --24 dic 2010
              --fol_prob           = reg_det.fol_prob,            --24 dic 2010
              --doc_prob           = reg_det.doc_prob,            --24 dic 2010
              --nacionalidad       = reg_det.nacionalidad,        --24 dic 2010
              n_folio            = reg_det.folio_solicitud,
              --ind_infonavit      = reg_det.ind_infonavit,       --24 dic 2010
              n_unico            = reg_det.curp_solicitud,
              paterno            = reg_det.paterno,
              materno            = reg_det.materno,
              nombres            = reg_det.nombres
       WHERE  n_seguro           = reg_det.nss_solicitud
       AND    n_folio            = reg_det.folio_solicitud
       AND    tipo_solicitud    IN (2,3,4)
       AND    status_interno    IN (30)
    ELSE
       UPDATE afi_solicitud
       SET    status_interno     = 65,
              fecha_actualiza_sa = f_periodo,
              sello_electronico  = sello ,
              fecha_1a_afil      = fpafil,
              salario_actual     = reg_det.salario,
              --n_rfc              = reg_det.rfc_trabajador,      --24 dic 2010
              --fena               = vfecha_nac,                  --24 dic 2010
              --estadon            = reg_det.entidad_nacimiento,  --24 dic 2010
              --tip_prob           = reg_det.tip_prob,            --24 dic 2010
              --fol_prob           = reg_det.fol_prob,            --24 dic 2010
              --doc_prob           = reg_det.doc_prob,            --24 dic 2010
              --nacionalidad       = reg_det.nacionalidad,        --24 dic 2010
              n_folio            = reg_det.folio_solicitud,
              --ind_infonavit      = reg_det.ind_infonavit,       --24 dic 2010
              n_unico            = reg_det.curp_solicitud,
              paterno            = reg_det.paterno,
              materno            = reg_det.materno,
              nombres            = reg_det.nombres
       WHERE  n_unico            = reg_det.curp_solicitud
       AND    n_folio            = reg_det.folio_solicitud
       AND    tipo_solicitud    IN (15)
       AND    status_interno    IN (30)
    END IF

    LET aceptar_f = aceptar_f + 1

    CALL inserta_afi_ctr(65)

END FUNCTION

FUNCTION cod_op02()
#------------------

    CALL inserta_afi_ctr(40)
    IF reg_det.tipo_trasp = '01' THEN      --taa x curp
       UPDATE afi_solicitud 
       SET    status_interno  = 40,
              fecha_1a_afil   = fpafil
       WHERE  n_seguro = reg_det.nss_solicitud
       AND    n_folio  = reg_det.folio_solicitud
       AND    tipo_solicitud in(2,3,4)
       AND    status_interno in(30)
    ELSE
       UPDATE afi_solicitud 
       SET    status_interno  = 40,
              fecha_1a_afil   = fpafil
       WHERE  n_unico  = reg_det.curp_solicitud
       AND    n_folio  = reg_det.folio_solicitud
       AND    tipo_solicitud in(15)
       AND    status_interno in(30)
    END IF

    LET rechazar = rechazar + 1

    LET reg_carta.docto_cod = 30219
    LET reg_carta.opera_cod = 'P'

    IF (diag <> "970") OR 
       (diag <> "962")THEN
        CALL det_carta() #dc
    END IF

END FUNCTION

FUNCTION cod_op03()
#------------------

    CALL inserta_afi_ctr(50)

    IF reg_det.tipo_trasp = '01' THEN      --taa x curp
       UPDATE afi_solicitud
       SET    status_interno = 50,
              --fol_prob          = reg_det.fol_prob,   --24 dic 2010
              --doc_prob          = reg_det.doc_prob,   --24 dic 2010
              sello_electronico = sello,
              fecha_1a_afil      = fpafil
       WHERE  n_seguro = reg_det.nss_solicitud
       AND    n_folio = reg_det.folio_solicitud
       AND    tipo_solicitud in(2,3,4)
       AND    status_interno in(30)
    ELSE
       UPDATE afi_solicitud
       SET    status_interno = 50,
              --fol_prob          = reg_det.fol_prob,   --24 dic 2010
              --doc_prob          = reg_det.doc_prob,   --24 dic 2010
              sello_electronico = sello,
              fecha_1a_afil      = fpafil
       WHERE  n_unico = reg_det.curp_solicitud
       AND    n_folio = reg_det.folio_solicitud
       AND    tipo_solicitud in(15)
       AND    status_interno in(30)
    END IF

    LET pendiente = pendiente + 1

    LET diagnostico = reg_det.diag_proceso[1,3]

    SELECT p.pend_desc
      INTO observacion
      FROM tab_pendiente_taa p
     WHERE p.pend_cod = diagnostico 

    LET v_fecha2 = reg_det.fec_emision_certif[05,06],"/",
                   reg_det.fec_emision_certif[07,08],"/",
                   reg_det.fec_emision_certif[01,04]   

  IF reg_det.tipo_trasp = '01' THEN      --taa x curp
     INSERT INTO afi_rechaza_cert 
     VALUES (reg_det.folio_solicitud,
             2,
             reg_det.nss_solicitud,
             diagnostico,
             v_fecha2,
             "",
             "",
             observacion,
               '')                         --24 dic 2010
  ELSE
     INSERT INTO afi_rechaza_cert 
     VALUES (reg_det.folio_solicitud,
             15,
             vnseguro,
             diagnostico,
             v_fecha2,
             "",
             "",
             observacion,
               '')                         --24 dic 2010
  END IF

            LET reg_carta.docto_cod = 30227
            LET reg_carta.opera_cod = 'P'

            CALL det_carta() #dc

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
           
    IF reg_det.tipo_trasp = '01' THEN      --taa x curp
       SELECT MAX(p.ind_envio)
       INTO   ind_envio
       FROM   afi_ctr_solicitud p
       WHERE  p.n_seguro       = reg_det.nss_solicitud
       AND    p.n_folio        = reg_det.folio_solicitud
       AND    p.tipo_solicitud = 2

       SELECT p.fecha_envio, p.paterno, p.materno, p.nombres, p.agenc_cod
       INTO   fecha_envio, paterno, materno, nombres, vagenc_cod
       FROM   afi_solicitud p
       WHERE  p.n_seguro       = reg_det.nss_solicitud
       AND    p.n_folio        = reg_det.folio_solicitud
       AND    p.tipo_solicitud = 2
    ELSE
       SELECT MAX(p.ind_envio)
       INTO   ind_envio
       FROM   afi_ctr_solicitud p
       WHERE  p.n_seguro       = vnseguro
       AND    p.n_folio        = reg_det.folio_solicitud
       AND    p.tipo_solicitud = 15

       SELECT p.fecha_envio, p.paterno, p.materno, p.nombres, p.agenc_cod
       INTO   fecha_envio, paterno, materno, nombres, vagenc_cod
       FROM   afi_solicitud p
       WHERE  p.n_seguro       = vnseguro
       AND    p.n_folio        = reg_det.folio_solicitud
       AND    p.tipo_solicitud = 15
    END IF

    LET nom_comp               = paterno CLIPPED, "$",
		                 materno CLIPPED, "$",
		                 nombres CLIPPED

    --LET reg_det.nombre_pcanase = reg_det.nombre_pcanase CLIPPED   --24 dic 2010
    LET nom_comp               = nom_comp CLIPPED
{24 dic 2010
    IF nom_comp <> reg_det.nombre_pcanase THEN
       LET ind_nombre = 1
    END IF
}
    IF fecha_envio IS NULL THEN
       LET fecha_envio = today
    END IF

    IF reg_det.tipo_trasp = '01' THEN      --taa x curp
       INSERT INTO afi_ctr_solicitud
       VALUES(2                           ,
              reg_det.folio_solicitud     ,
              reg_det.nss_solicitud       ,
              nom_comp                    ,
              status_interno              ,
              fecha_envio                 ,
              today                       ,
              ""                          ,
              #reg_det.cve_afo_ced         ,
              #reg_det.ind_nss_modificado  ,
              ""                          ,
              x_fecha                     ,
              --reg_det.curp_oficial        ,   --24 dic 2010
              --reg_det.ind_curp_modif      ,   --24 dic 2010
              --reg_det.n_rfc_bd            ,   --24 dic 2010
              --reg_det.nombre_bd           ,   --24 dic 2010
              --reg_det.nombre_pcanase      ,   --24 dic 2010
              --fnbd                        ,   --24 dic 2010
              --reg_det.codven_bd           ,   --24 dic 2010
              --reg_det.sexo_bd             ,   --24 dic 2010
              --reg_det.estadon_bd          ,   --24 dic 2010
              '','','','','','','',0,''   ,     --24 dic 2010
              fpafil                      ,
              falta                       ,
              reg_det.cve_afore           ,
              --reg_det.nacionalidad_bd     ,   --24 dic 2010
              --reg_det.tip_prob_bd         ,   --24 dic 2010
              --reg_det.fol_prob_bd         ,   --24 dic 2010
              --reg_det.doc_prob_bd         ,   --24 dic 2010
              '','','',''                 ,     --24 dic 2010
              ind_envio                   ,
              ind_nombre                  ,
               reg_det.cod_operacion       ,
              reg_det.diag_proceso        ,
              vagenc_cod                  ,
              g_usuario                   ,
              today                       )
    ELSE
       INSERT INTO afi_ctr_solicitud
       VALUES(15                          ,
              reg_det.folio_solicitud     ,
              vnseguro                    ,
              nom_comp                    ,
              status_interno              ,
              fecha_envio                 ,
              today                       ,
              ""                          ,
              #reg_det.cve_afo_ced         ,
              #reg_det.ind_nss_modificado  ,
              ""                          ,
              x_fecha                     ,
              reg_det.curp_solicitud        ,   --24 dic 2010
              --reg_det.ind_curp_modif      ,   --24 dic 2010
              --reg_det.n_rfc_bd            ,   --24 dic 2010
              --reg_det.nombre_bd           ,   --24 dic 2010
              --reg_det.nombre_pcanase      ,   --24 dic 2010
              --fnbd                        ,   --24 dic 2010
              --reg_det.codven_bd           ,   --24 dic 2010
              --reg_det.sexo_bd             ,   --24 dic 2010
              --reg_det.estadon_bd          ,   --24 dic 2010
              '','','','','','',0,''   ,     --24 dic 2010
              fpafil                      ,
              falta                       ,
              reg_det.cve_afore           ,
              --reg_det.nacionalidad_bd     ,   --24 dic 2010
              --reg_det.tip_prob_bd         ,   --24 dic 2010
              --reg_det.fol_prob_bd         ,   --24 dic 2010
              --reg_det.doc_prob_bd         ,   --24 dic 2010
              '','','',''                 ,     --24 dic 2010
              ind_envio                   ,
              ind_nombre                  ,
               reg_det.cod_operacion       ,
              reg_det.diag_proceso        ,
              vagenc_cod                  ,
              g_usuario                   ,
              today                       )
    END IF

END FUNCTION

FUNCTION Inserta_datos_en_tabla_errores(reg_det,x_reg)
#-----------------------------------------------------

    DEFINE reg_det RECORD
        tipo_registro         CHAR(02)     ,
        contador_servicio     CHAR(10)     ,
        clave_operacion       CHAR(02)     ,
        nss_solicitud         CHAR(11)     ,
        curp_solicitud        CHAR(18)     ,
        tipo_trasp           CHAR(02)      ,    --taa x curp
        paterno               CHAR(40)     ,
        materno               CHAR(40)     ,
        nombres               CHAR(40)     ,
        clave_promotor        CHAR(10)     ,
        fec_recp_sol_afore    CHAR(08)     ,
        folio_solicitud       DECIMAL(16,0),
        cve_afore_ced         CHAR(03)     ,     --24 dic 2010 nueva
        folio_saftv           CHAR(10)     ,
        #cve_afo_ced           CHAR(03)     ,
        cod_operacion         CHAR(02)     ,
        diag_proceso          CHAR(15)     ,
        ident_lote_origen     CHAR(16)     ,
        cve_rend_neto         CHAR(01)     ,
        
        #ind_nss_modificado    CHAR(01)     ,
        
        fec_emision_certif    CHAR(08)     ,
        fprimer_afil          CHAR(08)     ,
        falta_actual          CHAR(08)     ,
        cve_afore             CHAR(03)     ,
        fecha_recep_sello     CHAR(08)     ,
        hora_recep_sello      CHAR(02)     ,
        minuto_recep_sello    CHAR(02)     ,
        seg_recep_sello       CHAR(02)     ,
        cseg_recep_sello      CHAR(02)     ,
        consecutivo_recep     CHAR(08)     ,
        periodo               CHAR(6)      ,
        salario               DECIMAL(9,2) ,
        fecha_fin_rend        CHAR(08)     ,
        folio_edo_cta         CHAR(20)     ,
        id_bono_issste        CHAR(01)     ,                ---<
        folio_traspaso        CHAR(20)              ##--  CPL-1875 Nuevo
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

    IF reg_det.tipo_trasp = '01' THEN      --taa x curp
       SELECT "X" 
       FROM   afi_rechaza_cert
       WHERE  n_seguro  = reg_det.nss_solicitud
       AND    n_folio   = reg_det.folio_solicitud
       AND    rdeta_cod = x_reg.rdeta_cod
       AND    f_rechazo = v_fecha
       GROUP BY 1
       IF STATUS = NOTFOUND THEN
           IF x_reg.rdeta_cod = 14 THEN
              INSERT INTO afi_rechaza_cert 
               VALUES (reg_det.folio_solicitud,
                       2,
                       reg_det.nss_solicitud,
                       x_reg.rdeta_cod,
                       v_fecha,
                       v_fecha1,
                       reg_det.cve_afore,
                       x_reg.rdeta_desc_c,
                       '')                         --24 dic 2010
           ELSE
               INSERT INTO afi_rechaza_cert 
               VALUES (reg_det.folio_solicitud,
                       2,
                       reg_det.nss_solicitud,
                       x_reg.rdeta_cod,
                       v_fecha,
                       " ",
                       " ",
                       x_reg.rdeta_desc_c,
                       '')                         --24 dic 2010
           END IF
       END IF
    ELSE
       SELECT "X" 
       FROM   afi_rechaza_cert
       WHERE  n_seguro  = vnseguro
       AND    n_folio   = reg_det.folio_solicitud
       AND    rdeta_cod = x_reg.rdeta_cod
       AND    f_rechazo = v_fecha
       GROUP BY 1
       IF STATUS = NOTFOUND THEN
           IF x_reg.rdeta_cod = 14 THEN
              INSERT INTO afi_rechaza_cert 
               VALUES (reg_det.folio_solicitud,
                       15,
                       vnseguro,
                       x_reg.rdeta_cod,
                       v_fecha,
                       v_fecha1,
                       reg_det.cve_afore,
                       x_reg.rdeta_desc_c,
                       '')                         --24 dic 2010
           ELSE
               INSERT INTO afi_rechaza_cert 
               VALUES (reg_det.folio_solicitud,
                       15,
                       vnseguro,
                       x_reg.rdeta_cod,
                       v_fecha,
                       " ",
                       " ",
                       x_reg.rdeta_desc_c,
                       '')                         --24 dic 2010
           END IF
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
      PRINT COLUMN 01,"CPL  AFORE",
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
           WHEN "06" 
               LET detalle2_1.descripcion = "VAL LLAM TEL"
           WHEN "05" 
               LET detalle2_1.descripcion = "VAL IMG Y/O VAL CONS TAA"
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
           WHEN "06" 
               LET detalle2_2.descripcion_1 = "VAL LLAM TEL"
           WHEN "05" 
               LET detalle2_2.descripcion_1 = "VAL IMG Y/O VAL CONS TAA"
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
    DEFINE acept_tot  SMALLINT

    LET total_resp = 0
    LET total_resp = aceptar + aceptar_f + rechazar + pendiente + aclaracion + gs_inval     --#  CPL1875 JGHM Mar 2015 
    LET acept_tot  = aceptar + aceptar_f

  IF bnd_proceso THEN
    DISPLAY "                 TOTAL REGISTROS RECIBIDOS                                     " 

    DISPLAY "Total de Registros del lote     : ",
            total_resp USING "#######&" 

    DISPLAY "Registros certificados  : ",
            aceptar USING "#######&" 

    DISPLAY "Reg val img y/o val cons: ",
            aceptar_f USING "#######&" 

    DISPLAY "Registros rechazados    : ",
            rechazar USING "#######&" 

    DISPLAY "Registros pendientes    : ",
            pendiente USING "#######&" 

    DISPLAY "Registros en aclaracion : ",
            aclaracion USING "#######&" 

    DISPLAY "Registros invalidos     : ",                                                    --# CPL1875 JGHM Mar 2015 
            gs_inval USING "#######&"                                                        --# CPL1875 JGHM Mar 2015 
  ELSE
    DISPLAY "                 TOTAL REGISTROS RECIBIDOS                                     " AT 10,1 ATTRIBUTE ( REVERSE )

    DISPLAY "Total de Registros del lote     : ",
            total_resp USING "#######&" AT 11,15 ATTRIBUTE ( BOLD )

    DISPLAY "Registros certificados  : ",
            aceptar    USING "#######&" AT 12,15 ATTRIBUTE ( BOLD )

    DISPLAY "Reg val img y/o val cons: ",
            aceptar_f  USING "#######&" AT 13,15 ATTRIBUTE ( BOLD )

    DISPLAY "Registros rechazados    : ",
            rechazar   USING "#######&" AT 14,15 ATTRIBUTE ( BOLD )

    DISPLAY "Registros pendientes    : ",
            pendiente  USING "#######&" AT 15,15 ATTRIBUTE ( BOLD )   

    DISPLAY "Registros en aclaracion : ",
            aclaracion USING "#######&" AT 16,15 ATTRIBUTE ( BOLD )

    DISPLAY "Registros invalidos     : ",                                                     --# CPL1875 JGHM Mar 2015
            gs_inval   USING "#######&" AT 16,15 ATTRIBUTE ( BOLD )                           --# CPL1875 JGHM Mar 2015

    PROMPT "Presione <enter> para continuar " FOR enter
  END IF

  INSERT INTO afi_ctr_arh_tra
  VALUES (vgenerar,
          acept_tot,
          rechazar,
          pendiente,
          hoy)

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

FUNCTION det_carta()
#dc-----------------

    IF reg_det.tipo_trasp = '01' THEN      --taa x curp
       SELECT @tipo_solicitud
         INTO reg_carta.tipo_solicitud
         FROM afi_solicitud
        WHERE @n_seguro = reg_det.nss_solicitud
          AND @n_folio  = reg_det.folio_solicitud
    ELSE
       SELECT @tipo_solicitud
         INTO reg_carta.tipo_solicitud
         FROM afi_solicitud
        WHERE @n_unico  = reg_det.curp_solicitud
          AND @n_folio  = reg_det.folio_solicitud
    END IF

    IF reg_det.tipo_trasp = '01' THEN      --taa x curp
       LET reg_carta.nss            = reg_det.nss_solicitud 
    ELSE
       LET reg_carta.nss            = vnseguro 
    END IF

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

