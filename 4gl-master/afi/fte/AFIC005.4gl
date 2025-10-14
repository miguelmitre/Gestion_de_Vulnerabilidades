#############################################################################
#Proyecto          => AFORES ( MEXICO )                                     #
#Propietario       => E.F.P.                                                #
#Programa AFIC005  => CARGA DE ARCHIVO DISPERSION CURP                      #
#Sistema           => AFI.                                                  #
#Autor             => MAURO MUNIZ CABALLERO                                 #
#Fecha             => 18 DE ENERO DE 2001                                   #
#Modificado por    => FERNANDO HERRERA HERNANDEZ.                           #
#Fecha actualiza.  => 06 DE AGOSTO DE 2003.                                 # 
#Actualizacion     => EDUARDO RESENDIZ MEDINA  15 MARZO 2006 listado_3      #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE generar          CHAR(20)
    DEFINE enter            CHAR(1)
    DEFINE varchivo         CHAR(14)
    DEFINE hoy              DATE
    DEFINE aux_pausa        CHAR(01)
    DEFINE ejecuta          CHAR(100)
    DEFINE corr             CHAR(100)
    DEFINE total_reg        INTEGER
    DEFINE carga            CHAR(50)
    DEFINE g_plano1         INTEGER
    DEFINE vn_seguro        CHAR(11)
    DEFINE vn_folio         CHAR(18)
    DEFINE vcurp            CHAR(18)
    DEFINE vf_rechazo       CHAR(10)
    DEFINE vcurp_solicitud  CHAR(18)
    DEFINE vfec_alta_curp   CHAR(10)
    DEFINE vhoy             DATE
    DEFINE vvcurp           CHAR(18)
    DEFINE vtotal           INTEGER
    DEFINE vaprobados       INTEGER
    DEFINE vrechazados      INTEGER
    DEFINE cont1            INTEGER
    DEFINE regs             INTEGER
    DEFINE vresp            CHAR(1)
    DEFINE vdiag_proceso    CHAR(8)
    DEFINE vcod_operacion   CHAR(2)
    DEFINE vcodigo_39       INTEGER
    DEFINE vdesc_cod_39     CHAR(30)
    DEFINE vstatus_ren      CHAR(02)
    DEFINE vent_asigna      CHAR(30)
    DEFINE cfecha_a8        CHAR(8)
    DEFINE cfecha_asig      CHAR(10)
    DEFINE vfecha_asig      DATE
    DEFINE g_usuario        CHAR(8)
    DEFINE vpaterno         CHAR(40)
    DEFINE vmaterno         CHAR(40)
    DEFINE vnombres         CHAR(40)
    DEFINE vpat_bd          CHAR(40)
    DEFINE vmat_bd          CHAR(40)
    DEFINE vnom_bd          CHAR(40)

    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE reg_bat RECORD
        pid            INTEGER,
        proceso_cod    INTEGER,
        opera_cod      INTEGER,
        nombre_archivo CHAR(25)
    END RECORD

    DEFINE 
        bnd_proceso     SMALLINT,
        marca           SMALLINT,
        xcodigo_marca   SMALLINT ,
        xcodigo_rechazo SMALLINT

    DEFINE reg_carta		RECORD LIKE int_ctr_carta.*
    DEFINE consulta_carta	CHAR(120) 
    DEFINE vfentcons		DATE
    DEFINE folio_solic   	DECIMAL(8,0)
    DEFINE tipo_solic    	SMALLINT
    DEFINE vcorrelativo         INTEGER
   
    DEFINE reg_afi_ren RECORD
        nss       CHAR(11),
        paterno   CHAR(40),
        materno   CHAR(40),
        nombres   CHAR(40),
        tip_prob  SMALLINT,
        fol_prob  CHAR(10),
        doc_prob  CHAR(16),
        fentcons  DATE,
        cod_oper  CHAR(2),
        diag_pro  CHAR(3),
        des_rech  CHAR(30),
        st_renapo CHAR(2),
        #caja      SMALLINT,
        posicion  SMALLINT,
        st_int    SMALLINT,
        nom_arch  CHAR(20)
    END RECORD

    DEFINE
        tipo_arh  SMALLINT,
        tipo_des  CHAR(20),
        G_LISTA   CHAR(200),
        G_LISTA1  CHAR(200),
        rpt_uno   CHAR(200),
        rpt_dos   CHAR(200),
        corre_rpt CHAR(200),
        veco      CHAR(200),
        vsalida   CHAR(200),
        vsal_ar   CHAR(200),
        f_proc    DATE

    DEFINE fecha_ini,
           fecha_fin   DATE,

           dispersa RECORD
                    n_seguro         LIKE afi_dispersa_curp.n_seguro,
                    status_renapo    LIKE afi_dispersa_curp.status_renapo,
                    fecha_asignacion LIKE afi_dispersa_curp.fecha_asignacion
                    END RECORD,

           mae      RECORD
                    nombres          LIKE afi_mae_afiliado.nombres,
                    paterno          LIKE afi_mae_afiliado.paterno,
                    materno          LIKE afi_mae_afiliado.materno,
                    n_folio          LIKE afi_mae_afiliado.n_folio
                    END RECORD,
           g_paramgrales RECORD      LIKE seg_modulo.*,

           usuario CHAR(08),
           GLISTA  CHAR(300),
           G_LISTA3 CHAR(300),
           G_IMPRE3 CHAR(300)

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG("AFIC005.log")
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

    CALL st_sal_ren19() #REPORTE STATUS CURP (SALIDA RENAPO 19)
    RETURNING regs

    #CALL llama_emision() #lle
END MAIN

FUNCTION inicio()
#i---------------

    LET reg_bat.pid         = ARG_VAL(1)
    LET reg_bat.proceso_cod = ARG_VAL(2)
    LET reg_bat.opera_cod   = ARG_VAL(3)

    LET bnd_proceso         = 0
    LET marca               = 605

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
    END IF 

    LET hoy = TODAY

    SELECT *, USER
    INTO   g_seg_modulo.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE safre_tmp:plano1_curp
    WHENEVER ERROR STOP

    CREATE TABLE safre_tmp:plano1_curp
        (n_registros CHAR(550))

    DATABASE safre_af

    LET ejecuta = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"

    INITIALIZE reg_carta.* TO NULL 

    LET G_LISTA  = g_seg_modulo.ruta_rescate CLIPPED,"/",
                   g_usuario CLIPPED,".RCH_REPROC_ST_19.",
                   HOY USING "ddmmyy" CLIPPED


    LET G_LISTA1 = g_seg_modulo.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED,".RCH_REPROC_ST_19_NULO.",
                   HOY USING "ddmmyy" CLIPPED


   LET G_LISTA3 = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                  ".REPORTE_DISPCURP." CLIPPED,
                  HOY USING "ddmmyy"


    LET cont1    = 0

END FUNCTION

FUNCTION crea_tablas() 
#ct-------------------

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE safre_tmp:encabezado1_curp
        DROP TABLE safre_tmp:detalle1_curp
        DROP TABLE safre_tmp:sumario1_curp
        DROP TABLE safre_tmp:afi_renapo
    WHENEVER ERROR STOP

    CREATE TABLE safre_tmp:encabezado1_curp
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
         campo22 CHAR(3)
        )

    CREATE TABLE safre_tmp:detalle1_curp
        (tipo_registro       CHAR(02),
         contador_servicio   CHAR(10),
         clave_operacion     CHAR(02),
         nss_solicitud       CHAR(11),
         curp_solicitud      CHAR(18),
         rfc_trabajador      CHAR(13),
         paterno             CHAR(40),
         materno             CHAR(40),
         nombres             CHAR(40),
         fecha_nacimiento    CHAR(08),
         clave_promotor      CHAR(10),
         fec_recp_sol_afore  CHAR(08),
         folio_solicitud     DECIMAL(16,0),
         sexo                SMALLINT,
         entidad_nacimiento  CHAR(02),
         ind_infonavit       CHAR(01),
         nacionalidad        CHAR(03),
         tip_aprob           CHAR(01),
         fol_aprob           CHAR(10),
         doc_aprob           CHAR(16),
         cod_operacion       CHAR(02),
         diag_proceso        CHAR(15),
         ident_lote_origen   CHAR(16),
         nss_oficial         CHAR(11),
         ind_nss_modificado  CHAR(01),
         fec_alta_curp       CHAR(08),
         curp_oficial        CHAR(18),
         ind_curp_modif      CHAR(01),
         fecha_recep_sello   CHAR(08),
         hora_recep_sello    CHAR(02),
         minuto_recep_sello  CHAR(02),
         seg_recep_sello     CHAR(02),
         cseg_recep_sello    CHAR(02),
         consecutivo_recep   CHAR(08),
         tipo_transaccion    CHAR(03),
         status_renapo       CHAR(02),
         ent_asigna_curp     INTEGER,
         archivo_alta        CHAR(6),
         archivo_modifica    CHAR(6),
         paterno_renapo      CHAR(40),
         materno_renapo      CHAR(40),
         nombres_renapo      CHAR(40),
         nacionalidad_ren    CHAR(03),
         tip_prob_ren        CHAR(01),
         fol_prob_ren        CHAR(10),
         doc_prob_ren        CHAR(16)
        )

    CREATE TABLE safre_tmp:sumario1_curp
        (campo1    CHAR(2),
         campo2    CHAR(2),
         campo3    CHAR(3),
         campo4    CHAR(8),
         campo5    CHAR(3),
         campo6    CHAR(2),
         campo7    CHAR(2),
         campo8    CHAR(9),
         campo9    CHAR(9)
        )

     CREATE TABLE safre_tmp:afi_renapo
    	(
     	 nss       CHAR(11),
      	 paterno   CHAR(40),
     	 materno   CHAR(40),
     	 nombres   CHAR(40),
     	 tip_prob  SMALLINT,
     	 fol_prob  CHAR(10),
     	 doc_prob  CHAR(16),
     	 fentcons  DATE,
     	 cod_oper  CHAR(2),
     	 diag_pro  CHAR(3),
     	 des_rech  CHAR(30),
     	 st_renapo CHAR(2),
     	 #caja      SMALLINT,
     	 posicion  SMALLINT,
     	 st_int    SMALLINT,
    	 nom_arch  CHAR(20)
     	)

    DATABASE safre_af

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIC0051" ATTRIBUTE(BORDER)
    DISPLAY " AFIC005      CARGA DE ARCHIVO DISPERSION CURP RENAPO                          " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
    DISPLAY g_seg_modulo.ruta_rescate AT 6,10
    DISPLAY g_seg_modulo.ruta_listados AT 11,10

    INPUT BY NAME generar, tipo_arh
        AFTER FIELD generar
        IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        ELSE
            NEXT FIELD tipo_arh
        END IF

        BEFORE FIELD tipo_arh
            LET tipo_arh = 1
            LET tipo_des = 'ARCHIVO RECURRENTE'

            DISPLAY BY NAME tipo_arh, tipo_des 

    AFTER FIELD tipo_arh
        IF tipo_arh IS NULL THEN
            ERROR "Campo tipo de archivo NO puede ser NULO"
            NEXT FIELD tipo_arh
        END IF

        IF tipo_arh = 2 THEN
            LET tipo_des = 'FECHAS ANTERIORES'
            DISPLAY BY NAME tipo_arh, tipo_des
        END IF

        IF tipo_arh <> 1 AND
           tipo_arh <> 2 THEN
            ERROR "Tipo de archivo solo es (1) Normal, (2) Fechas anteriores"
            NEXT FIELD tipo_arh
        END IF

        IF tipo_arh = 1 OR
           tipo_arh = 2 THEN
            WHILE TRUE
                PROMPT "¿Esta seguro de procesar el archivo [S/N]? "
                ATTRIBUTES(reverse)
                FOR enter
                IF enter MATCHES "[SsNn]" THEN
                    IF enter MATCHES "[Nn]" THEN
                        RETURN
                    ELSE
                        EXIT WHILE
                    END IF
                ELSE
                    DISPLAY "Solo debe presionar (S) Si o (N) No" AT 19,2
                END IF
            END WHILE
        END IF

        IF tipo_arh = 1 THEN
            SELECT @nombre_archivo
            INTO   varchivo
            FROM   afi_ctr_arh_curp
            WHERE  @nombre_archivo = generar
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                ERROR "ARCHIVO YA PROCESADO"
                INITIALIZE generar TO NULL
                CLEAR FORM
                NEXT FIELD generar
            END IF
        ELSE
            SELECT @fecha_proceso
            INTO   f_proc
            FROM   afi_ctr_arh_proc
            WHERE  nombre_archivo = generar

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR "ARCHIVO DEBER SER PROCESADO COMO TIPO (1)"
                INITIALIZE generar TO NULL
                CLEAR FORM
                NEXT FIELD generar
            END IF
        END IF

        LET carga = NULL
        LET carga = g_seg_modulo.ruta_rescate CLIPPED,"/",
                    generar CLIPPED

        WHENEVER ERROR CONTINUE
            LOAD FROM carga DELIMITER "," 
            INSERT INTO safre_tmp:plano1_curp
        WHENEVER ERROR STOP

        SELECT COUNT(*) 
        INTO   g_plano1 
        FROM   safre_tmp:plano1_curp

        IF g_plano1 IS NULL OR g_plano1 = 0 THEN
            ERROR "NOMBRE DE ARCHIVO INCORRECTO O VACIO, VERIFIQUE " 
            SLEEP 5
            EXIT PROGRAM
        END IF

        ERROR "Procesando Informacion"

        CALL rescata_valores() #

        EXIT INPUT

        ON KEY (control -b)
            CALL despliega_archivos()

    END INPUT

END FUNCTION

FUNCTION rescata_valores()
#rv-----------------------

    CALL crea_tablas()        #ct
    CALL actualiza_datos()    #ad
    CALL revisa_datos()       #rd
    CALL Actualiza_Maeafili() #am

END FUNCTION

FUNCTION actualiza_datos()
#ad-----------------------

    DEFINE 
        cont_reg      INTEGER
 
    DEFINE 
        carga_reg     CHAR(550)

    DEFINE   
        campo_011     CHAR(02),
        campo_012     CHAR(02),
        campo_013     CHAR(02),
        campo_014     CHAR(02),
        campo_015     CHAR(03),
        campo_016     CHAR(02),
        campo_017     CHAR(03),
        campo_018     CHAR(08),
        campo_019     CHAR(03),
        campo_110     CHAR(03),
        campo_111     CHAR(02),
        campo_112     CHAR(08),
        campo_113     CHAR(08),
        campo_114     CHAR(08),
        campo_115     CHAR(08),
        campo_116     CHAR(08),
        campo_117     CHAR(01),
        campo_118     CHAR(09),
        campo_119     CHAR(02),
        campo_210     CHAR(03),
        campo_211     CHAR(02),
        campo_212     CHAR(03),

        campo_01      CHAR(02),
        campo_02      CHAR(10),
        campo_03      CHAR(02),
        campo_04      CHAR(11),
        campo_05      CHAR(18),
        campo_06      CHAR(13),
        campo_07      CHAR(40),
        campo_08      CHAR(40),
        campo_09      CHAR(40),
        campo_10      CHAR(08),
        campo_11      CHAR(10),
        campo_12      CHAR(08),
        campo_13      CHAR(10),
        campo_14      CHAR(01),
        campo_15      CHAR(02),
        campo_16      CHAR(01),
        campo_17      CHAR(03),
        campo_18      CHAR(01),
        campo_19      CHAR(10),
        campo_20      CHAR(16),
        campo_21      CHAR(02),
        campo_22      CHAR(15),
        campo_23      CHAR(16),
        campo_24      CHAR(11),
        campo_25      CHAR(01),
        campo_26      CHAR(08),
        campo_27      CHAR(18),
        campo_28      CHAR(01),
        campo_29      CHAR(08),
        campo_30      CHAR(02),
        campo_31      CHAR(02),
        campo_32      CHAR(02),
        campo_33      CHAR(02),
        campo_34      CHAR(08),
        campo_35      CHAR(03),
        campo_36      CHAR(02),
        campo_37      CHAR(05),
        campo_38      CHAR(06),
        campo_39      CHAR(06),
        campo_40      CHAR(40),
        campo_41      CHAR(40),
        campo_42      CHAR(40),
        campo_43      CHAR(03),
        campo_44      CHAR(01),
        campo_45      CHAR(10),
        campo_46      CHAR(16),

        campo_201     CHAR(02),
        campo_202     CHAR(02),
        campo_203     CHAR(03),
        campo_204     CHAR(08),
        campo_205     CHAR(03),
        campo_206     CHAR(02),
        campo_207     CHAR(02),
        campo_208     CHAR(09),
        campo_209     CHAR(09)

    LET cont_reg = 0

    SELECT COUNT(*)
    INTO   total_reg
    FROM   safre_tmp:plano1_curp

    DECLARE cursor_1 CURSOR FOR 
    SELECT  * 
    FROM    safre_tmp:plano1_curp

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

            INSERT INTO safre_tmp:encabezado1_curp
            VALUES (campo_011 ,
                    campo_012,
                    campo_013,
                    campo_014,
                    campo_015,
                    campo_016,
                    campo_017,
                    campo_018,
                    campo_019,
                    campo_110,
                    campo_111,
                    campo_112,
                    campo_113,
                    campo_114,
                    campo_115,
                    campo_116,
                    campo_117,
                    campo_118,
                    campo_119,
                    campo_210,
                    campo_211,
                    campo_212 
                   )

            LET vf_rechazo = campo_018[5,6],"/",
                             campo_018[7,8],"/",
                             campo_018[1,4]
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
            LET campo_21 = carga_reg[247,248]
            LET campo_22 = carga_reg[249,263]
            LET campo_23 = carga_reg[264,279]
            LET campo_24 = carga_reg[280,290]
            LET campo_25 = carga_reg[291,291]
            LET campo_26 = carga_reg[292,299]
            LET campo_27 = carga_reg[300,317]
            LET campo_28 = carga_reg[318,318]
            LET campo_29 = carga_reg[319,326]
            LET campo_30 = carga_reg[327,328]
            LET campo_31 = carga_reg[329,330]
            LET campo_32 = carga_reg[331,332]
            LET campo_33 = carga_reg[333,334]
            LET campo_34 = carga_reg[335,342]
            LET campo_35 = carga_reg[343,345]
            LET campo_36 = carga_reg[346,347]
            LET campo_37 = carga_reg[348,352]
            LET campo_38 = carga_reg[353,358]
            LET campo_39 = carga_reg[359,364]
            LET campo_40 = carga_reg[365,404]
            LET campo_41 = carga_reg[405,444]
            LET campo_42 = carga_reg[445,484]
            LET campo_43 = carga_reg[485,487]
            LET campo_44 = carga_reg[488,488]
            LET campo_45 = carga_reg[489,498]
            LET campo_46 = carga_reg[499,514]

            INSERT INTO safre_tmp:detalle1_curp
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
                    campo_23,
                    campo_24,
                    campo_25,
                    campo_26,
                    campo_27,
                    campo_28,
                    campo_29,
                    campo_30,
                    campo_31,
                    campo_32,
                    campo_33,
                    campo_34,
                    campo_35,
                    campo_36,
                    campo_37,
                    campo_38,
                    campo_39,
                    campo_40,
                    campo_41,
                    campo_42,
                    campo_43,
                    campo_44,
                    campo_45,
                    campo_46
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

           INSERT INTO safre_tmp:sumario1_curp
           VALUES (campo_201,
                   campo_202,
                   campo_203,
                   campo_204,
                   campo_205,
                   campo_206,
                   campo_207,
                   campo_208,
                   campo_209
                  )
        END IF

    END FOREACH

END FUNCTION

FUNCTION revisa_datos()
#-----------------------

    DEFINE 
        rechazo_lote CHAR(3),
        rechazo_deta CHAR(3),
        l_reg RECORD LIKE tab_rch_lote.* ,
        x_reg RECORD LIKE tab_rdeta.* ,
        aux_pausa    CHAR(1)

    DEFINE reg_det RECORD
        tipo_registro       CHAR(02),
        contador_servicio   CHAR(10),
        clave_operacion     CHAR(02),
        nss_solicitud       CHAR(11),
        curp_solicitud      CHAR(18),
        rfc_trabajador      CHAR(13),
        paterno             CHAR(40),
        materno             CHAR(40),
        nombres             CHAR(40),
        fecha_nacimiento    CHAR(08),
        clave_promotor      CHAR(10),
        fec_recp_sol_afore  CHAR(08),
        folio_solicitud     DECIMAL(16,0),
        sexo                SMALLINT,
        entidad_nacimiento  CHAR(02),
        ind_infonavit       CHAR(01),
        nacionalidad        CHAR(03),
        tip_aprob           CHAR(01),
        fol_aprob           CHAR(10),
        doc_aprob           CHAR(16),
        cod_operacion       CHAR(02),
        diag_proceso        CHAR(15),
        ident_lote_origen   CHAR(16),
        nss_oficial         CHAR(11),
        ind_nss_modificado  CHAR(01),
        fec_alta_curp       CHAR(08),
        curp_oficial        CHAR(18),
        ind_curp_modif      CHAR(01),
        fecha_recep_sello   CHAR(08),
        hora_recep_sello    CHAR(02),
        minuto_recep_sello  CHAR(02),
        seg_recep_sello     CHAR(02),
        cseg_recep_sello    CHAR(02),
        consecutivo_recep   CHAR(08),
        tipo_transaccion    CHAR(03),
        status_renapo       CHAR(02),
        ent_asigna_curp     INTEGER,
        archivo_alta        CHAR(06),
        archivo_modifica    CHAR(06),
        paterno_renapo      CHAR(40),
        materno_renapo      CHAR(40),
        nombres_renapo      CHAR(40)
    END RECORD

    DEFINE 
        rechazo_09         CHAR(02),
        rechazo_001        CHAR(02),
        rechazo_002        CHAR(02),
        rechazo_003        CHAR(02),
        rechazo_019        CHAR(02),
        rechazo_020        CHAR(03),
        rechazo_021        CHAR(02)

    DEFINE 
        uno   CHAR(03),
        dos   CHAR(03),
        tre   CHAR(03),
        cua   CHAR(03),
        cin   CHAR(03),
        l_status_int  SMALLINT

    DEFINE 
        x_fecha   CHAR(10),
        xx_fecha  DATE

    # ENCABEZADO #
    SELECT campo1,
           campo2,
           campo3,
           campo18,
           campo19 
    INTO   rechazo_001,
           rechazo_002,
           rechazo_003,
           rechazo_lote,
           rechazo_019
    FROM safre_tmp:encabezado1_curp

    SELECT * 
    INTO   l_reg.* 
    FROM   tab_rch_lote
    WHERE  rlote_cod = rechazo_lote

    IF STATUS  <> NOTFOUND THEN
      IF bnd_proceso THEN
        DISPLAY "Program stopped, ERROR DE PROCESO NO PUEDE CONTINUAR" 
        DISPLAY l_reg.rlote_cod 
        DISPLAY l_reg.rlote_desc_c 
        EXIT PROGRAM
      ELSE
        DISPLAY l_reg.rlote_cod AT 10,1
        DISPLAY l_reg.rlote_desc_c AT 11,1
        PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
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
        PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
      END IF
    END IF

    IF rechazo_002 <> "07" THEN
      IF bnd_proceso THEN
        DISPLAY "Program stopped, Identificador de servicio de ser 07 en encabezado"
        EXIT PROGRAM
      ELSE
        CLEAR SCREEN
        DISPLAY "Identificador de Servicio debe ser 07 en ENCABEZADO" AT 10,1
        PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
      END IF
    END IF

    IF rechazo_003 <> "71" THEN
      IF bnd_proceso THEN
        DISPLAY "Program stopped, Identificador de operacion de ser 71 en encabezado"
        EXIT PROGRAM
      ELSE
        CLEAR SCREEN
        DISPLAY "Identificador de Operacion debe ser 71 en ENCABEZADO" AT 10,1
        PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
      END IF
    END IF

    IF rechazo_019 <> "03" THEN
      IF bnd_proceso THEN
        DISPLAY "Program stopped, Codigo de empresa operadora debe ser 03 en encabezado"
        EXIT PROGRAM
      ELSE
        CLEAR SCREEN
        DISPLAY "Codigo de Empresa Operadora debe ser 03 en ENCABEZADO" AT 10,1
        PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
      END IF
    END IF

    IF rechazo_020 <> "001" THEN
      IF bnd_proceso THEN
        DISPLAY "Program stopped, Clave entidad origen debe ser 001 en encabezado"
        EXIT PROGRAM
      ELSE
        CLEAR SCREEN
        DISPLAY "Clave Entidad Origen debe ser 001 en ENCABEZADO" AT 10,1
        PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
      END IF
    END IF

END FUNCTION

FUNCTION Actualiza_Maeafili()
#----------------------------

    DEFINE curp_reg      CHAR(18)
    DEFINE cdiag_proceso CHAR(3)
    DEFINE sdiag_proceso SMALLINT
    #DEFINE folio_solic   DECIMAL(8,0)
    #DEFINE tipo_solic    SMALLINT
    DEFINE long_curp     SMALLINT
    DEFINE marca_dom     SMALLINT
    DEFINE vtipo_prob    SMALLINT
    DEFINE vdcto_1       CHAR(1)

    IF NOT bnd_proceso THEN
        OPEN WINDOW w_curp AT 8,4 WITH FORM "AFIC0052"
        ATTRIBUTE (BORDER)
    END IF

    SELECT COUNT(*) INTO vtotal FROM safre_tmp:detalle1_curp

    START REPORT listado TO G_LISTA

        DECLARE c_curp CURSOR FOR 
        SELECT a.nss_solicitud, 
               a.diag_proceso,
               a.curp_solicitud,
               a.cod_operacion,
               b.tipo_solicitud,
               b.n_folio,
               a.status_renapo,
               a.ent_asigna_curp,
               a.fec_alta_curp,
               b.n_unico,
               a.paterno_renapo,
               a.materno_renapo,
               a.nombres_renapo,
               b.tip_prob,
               b.fol_prob,
               b.doc_prob,
               b.fentcons,
               b.status_interno,
               #b.indicador_b,
               b.ubicacion,
               b.documento_1,
               a.paterno,
               a.materno,
               a.nombres
        FROM   safre_tmp:detalle1_curp a, afi_mae_afiliado b
        WHERE  b.n_seguro = a.nss_solicitud 
        ORDER BY a.nss_solicitud

        fOREACH c_curp INTO vn_seguro,
                            vdiag_proceso,
                            vcurp_solicitud,
                            vcod_operacion,
                            tipo_solic,
                            folio_solic,
                            vstatus_ren,
                            vent_asigna,
                            cfecha_a8,
                            curp_reg,
                            vpaterno,
                            vmaterno,
                            vnombres,
                            vtipo_prob,
                            reg_afi_ren.fol_prob,
                            reg_afi_ren.doc_prob,
    			    vfentcons,
                            reg_afi_ren.st_int,
                            #reg_afi_ren.caja,
                            reg_afi_ren.posicion,
                            vdcto_1,
                            vpat_bd,
                            vmat_bd,
                            vnom_bd

        LET long_curp = LENGTH(curp_reg)

        LET cfecha_asig = cfecha_a8[5,6],'/',
                          cfecha_a8[7,8],'/',
                          cfecha_a8[1,4]

        LET vfecha_asig = cfecha_asig

        IF tipo_arh = 2 THEN
            SELECT "X"
              FROM afi_dispersa_curp
             WHERE @n_seguro         = vn_seguro
               AND @status_renapo    = '19'
               AND @fecha_asignacion = vfecha_asig

            IF SQLCA.SQLCODE <> 0 THEN
                LET cont1 = cont1 + 1
                OUTPUT TO REPORT listado(cont1      ,
                                         vn_seguro  ,
                                         folio_solic,
                                         vpat_bd    ,
                                         vmat_bd    ,
                                         vnom_bd    ,
                                         vfecha_asig,
                                         tipo_solic)
                IF tipo_solic <> 2 THEN
                   CONTINUE FOREACH
                END IF
            END IF
        END IF

        LET vvcurp = vcurp_solicitud 
        LET vcurp_solicitud = length(vcurp_solicitud)
        LET vhoy   = TODAY
        LET sdiag_proceso = vdiag_proceso[1,3]

        IF vcod_operacion = '01' THEN
            LET marca_dom = 1
        ELSE
            LET marca_dom = 0
        END IF

        ### Asignar datos para inserta en la tabla afi_renapo
        LET reg_afi_ren.nss         = vn_seguro
        LET reg_afi_ren.paterno     = vpaterno 
        LET reg_afi_ren.materno     = vmaterno
        LET reg_afi_ren.nombres     = vnombres
        LET reg_afi_ren.tip_prob    = vtipo_prob
        LET reg_afi_ren.cod_oper    = vcod_operacion
        LET reg_afi_ren.diag_pro    = vdiag_proceso
        LET reg_afi_ren.fentcons    = vfentcons

        SELECT @des_rechazo
        INTO   reg_afi_ren.des_rech
        FROM   tab_rch_curp
        WHERE  @curp_cod_rech       = reg_afi_ren.diag_pro
        IF SQLCA.SQLCODE = NOTFOUND THEN
           LET reg_afi_ren.des_rech = 'NO IDENTIFICADO'
        END IF

        LET reg_afi_ren.st_renapo    = vstatus_ren 
        LET reg_afi_ren.nom_arch     = generar 
        INSERT INTO safre_tmp:afi_renapo VALUES (reg_afi_ren.*)

        INSERT INTO afi_dispersa_curp
        VALUES (vn_seguro,
                vcod_operacion,
                vdiag_proceso,
                vhoy,
                generar,
                vstatus_ren,
                vent_asigna,
                vfecha_asig,
                marca_dom)

        SELECT "X"
        FROM   tab_status_renapo tsr
        WHERE  tsr.status_cod = vstatus_ren
        AND    tsr.marca = 'X'

        IF SQLCA.SQLCODE = 0 THEN
            INSERT INTO afi_status_tp5
            VALUES (vn_seguro,
                    vstatus_ren,
                    vpaterno,
                    vmaterno,
                    vnombres,
                    vf_rechazo,
                    vhoy)
        END IF

        SELECT @correlativo
          INTO vcorrelativo
          FROM cta_act_marca
         WHERE @nss       = vn_seguro
           AND @marca_cod = 605

        CALL desmarca_cuenta(vn_seguro, marca, g_usuario, vcorrelativo)

        IF vcod_operacion = "02" THEN
            IF vstatus_ren = 16 OR
               vstatus_ren = 17 OR
               vstatus_ren = 18 OR
               vstatus_ren = 19 OR
               vstatus_ren = 34 THEN
                LET vaprobados = vaprobados + 1

                UPDATE afi_mae_afiliado
                SET    n_unico = vvcurp,
                       status_interno = 200
                WHERE  n_seguro = vn_seguro

                UPDATE cta_afi_nip
                SET    curp = vvcurp
                WHERE  nss = vn_seguro
            ELSE
                LET vrechazados = vrechazados + 1

                CASE vstatus_ren
                    WHEN 20 
                        UPDATE afi_mae_afiliado
                        SET    status_interno = 140
                        WHERE  n_seguro = vn_seguro
                    WHEN 19
                        IF curp_reg IS NULL OR
                           curp_reg MATCHES " *" OR 
                           long_curp <> 18 THEN
                            UPDATE afi_mae_afiliado
                            SET    status_interno = 140
                            WHERE  n_seguro = vn_seguro
                        ELSE
                            UPDATE afi_mae_afiliado
                            SET    status_interno = 150
                            WHERE  n_seguro = vn_seguro
                        END IF
                    OTHERWISE
                        UPDATE afi_mae_afiliado
                        SET    status_interno = 150
                        WHERE  n_seguro = vn_seguro
                END CASE

               {
                IF vstatus_ren = 20 THEN
                    UPDATE afi_mae_afiliado
                    SET    status_interno = 140
                    WHERE  n_seguro = vn_seguro
                ELSE
                    UPDATE afi_mae_afiliado
                    SET    status_interno = 150
                    WHERE  n_seguro = vn_seguro
                END IF
               }
            END IF
        ELSE
            LET vaprobados = vaprobados + 1

            IF vtipo_prob = 5 THEN
                IF vdcto_1 = 0 AND vstatus_ren = '71' THEN
                    UPDATE afi_mae_afiliado
                    SET    n_unico        = vvcurp,
                           status_interno = 200,
                           documento_1    = 1
                    WHERE  n_seguro       = vn_seguro
                    AND    documento_1    = 0

                    UPDATE cta_afi_nip
                    SET    curp = vvcurp
                    WHERE  nss = vn_seguro
                ELSE
                    LET reg_carta.docto_cod = 30224
                    #CALL det_carta() #dc1
                    UPDATE afi_mae_afiliado
                    SET    n_unico        = vvcurp,
                           status_interno = 200
                    WHERE  n_seguro       = vn_seguro

                    UPDATE cta_afi_nip
                    SET    curp = vvcurp
                    WHERE  nss = vn_seguro
                END IF
            ELSE
                UPDATE afi_mae_afiliado
                SET    n_unico        = vvcurp,
                       status_interno = 200
                WHERE  n_seguro       = vn_seguro

                UPDATE cta_afi_nip
                SET    curp = vvcurp
                WHERE  nss = vn_seguro
            END IF
        END IF

    END FOREACH

    FINISH REPORT listado


    -- actualiza historico de archivos curp afi_ctr_arh_curp

    INSERT INTO afi_ctr_arh_curp
    VALUES (generar, vtotal, vaprobados, vrechazados, "", vhoy)

    IF NOT bnd_proceso THEN
        DISPLAY "                      DATOS A DISPERSAR                                        "
        AT 10,1 ATTRIBUTE ( REVERSE )

        DISPLAY "Total de Registros del lote     : ",
                 vtotal USING "#######&" AT 6,15 ATTRIBUTE ( CYAN )

        DISPLAY "Registros con CURP actualizados : ",
                 vaprobados USING "#######&" AT 8,15 ATTRIBUTE ( CYAN )

        DISPLAY "Registros con CURP rechazados   : ",
                 vrechazados USING "#######&" AT 10,15 ATTRIBUTE ( CYAN )

        PROMPT "Proceso Finalizado Satisfactoriamente  Presione <ENTER> Para Continuar          "
        ATTRIBUTE (REVERSE)
        FOR vresp
        ATTRIBUTE (REVERSE)

        CLOSE WINDOW w_curp
    ELSE
        DISPLAY "                      DATOS A DISPERSAR                                        "

        DISPLAY "Total de Registros del lote     : ",
                 vtotal USING "#######&" 

        DISPLAY "Registros con CURP actualizados : ",
                 vaprobados USING "#######&" 

        DISPLAY "Registros con CURP rechazados   : ",
                 vrechazados USING "#######&" 
    END IF

--->erm 15 Marzo 2006
   START REPORT listado_3 TO G_LISTA3
   OUTPUT TO REPORT listado_3(generar, vtotal, vaprobados, vrechazados)
   FINISH REPORT listado_3

   LET G_IMPRE3 = "lp ", G_LISTA3 CLIPPED

   RUN G_IMPRE3
---<erm 15 Marzo 2006

RETURN

END FUNCTION

REPORT listado(vcont1, vn_seguro, folio_solic, vpat_bd, vmat_bd, vnom_bd,
               vfecha_asig, vtipo_solic)
#l--------------------------------------------------------------------------

    DEFINE
        vcont1       INTEGER,
        vn_seguro    LIKE afi_mae_afiliado.n_seguro,
        folio_solic  LIKE afi_mae_afiliado.n_folio,
        vpat_bd      CHAR(13),
        vmat_bd      CHAR(13),
        vnom_bd      CHAR(16),
        vfecha_asig  CHAR(49),
        vtipo_solic  SMALLINT,
        vtip_soldes  CHAR(15)

    DEFINE l_estado  CHAR(16)

    DEFINE
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
            COLUMN 160,"====="
        PRINT
            COLUMN 001,razon_social                                         ,
            COLUMN 140,"FECHA   :",hoy USING "DD/MM/YY"
        PRINT
            COLUMN 001,"AFIC005"                                            ,
            COLUMN 035," ESTADO DE AFILIADOS RECHAZADOS DE ARCHIVO DE REPROCESO DE RENAPO (STATUS SALIDA 19)",
            COLUMN 140,"NUM.PAG.:",pageno USING "####,###"
            SKIP 1 LINE
        PRINT
            COLUMN 001,"----------------------------------------"           ,
            COLUMN 040,"----------------------------------------"           ,
            COLUMN 080,"----------------------------------------"           ,
            COLUMN 120,"----------------------------------------"           ,
            COLUMN 160,"-----"
       SKIP 1 LINES
       PRINT
           COLUMN 001, "Consec",
           COLUMN 011, "Numero de Seguro",
           COLUMN 040, "Numero de Folio",
           COLUMN 058, "Ap. Paterno",
           COLUMN 074, "Ap. Materno",
           COLUMN 090, "Nombres",
           COLUMN 108, "Fecha Asignacion",
           COLUMN 128, "Estado"
       PRINT
           COLUMN 001,"========================================"           ,
           COLUMN 040,"========================================"           ,
           COLUMN 080,"========================================"           ,
           COLUMN 120,"========================================"           ,
           COLUMN 160,"====="

    ON EVERY ROW
       IF vtip_soldes = 2 THEN
          LET vtip_soldes = 'TRASPASO'
       ELSE
          LET vtip_soldes = 'NO EXISTE'
       END IF

       PRINT
           COLUMN 001, vcont1 USING "######&",
           COLUMN 011, vn_seguro,
           COLUMN 040, folio_solic,
           COLUMN 058, vpat_bd,
           COLUMN 074, vmat_bd,
           COLUMN 090, vnom_bd,
           COLUMN 108, vfecha_asig,
           COLUMN 128, vtip_soldes

END REPORT

FUNCTION despliega_archivos()
#da--------------------------

    DEFINE aux_pausa CHAR(1)
    DEFINE HOY       DATE
    DEFINE SW_1      SMALLINT

    OPTIONS
        PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY control-o

        WHENEVER ERROR STOP

        LET HOY = TODAY
        OPEN WINDOW window_1 AT 2,2 WITH FORM "AFIM0231" ATTRIBUTE( BORDER)
        DISPLAY " AFIM030        CONSULTA ARCHIVOS DE DISPERSION DE CURP                        " AT 3,1 
        ATTRIBUTE(REVERSE) 
        DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 
        ATTRIBUTE(REVERSE)

        MENU "CONSULTA ARCHIVOS CURP PROCESADOS"
            COMMAND "Consulta" "Consultar Archivos Curp"
                CALL Consulta()
            COMMAND "Salir" "Salir del Programa"
                EXIT MENU
        END MENU

        CLOSE WINDOW window_1

END FUNCTION
 
FUNCTION Consulta()
#c-----------------

    DEFINE ga_record   ARRAY[3000] OF RECORD
        nombre_archivo LIKE afi_ctr_arh_curp.nombre_archivo,
        total_reg      LIKE afi_ctr_arh_curp.total_reg,
        total_aprob    LIKE afi_ctr_arh_curp.total_aprob,
        total_rech     LIKE afi_ctr_arh_curp.total_rech,
        fecha_proceso  LIKE afi_ctr_arh_curp.nombre_archivo
    END RECORD

    DEFINE
        vcount        INTEGER,
        vtotal        INTEGER,
        vaprobados    INTEGER,
        vrechazos     INTEGER,
        vtot_curp     INTEGER

    DEFINE pos        SMALLINT

    SELECT count(*) INTO vcount FROM afi_ctr_arh_curp
    SELECT sum(total_aprob) INTO vaprobados FROM afi_ctr_arh_curp
    SELECT sum(total_rech) INTO vrechazos FROM afi_ctr_arh_curp

    LET vtotal = vtot_curp

    DISPLAY "                                                                                  " AT 1,1
    DISPLAY "    CTRL-C cancela                                                                " AT 2,1
    DISPLAY " CONSULTA " AT 2,65

    DECLARE curp_12 CURSOR FOR
    SELECT *
    FROM afi_ctr_arh_curp
    ORDER BY 5

    LET pos = 1

    FOREACH curp_12 INTO ga_record[pos].*
        LET pos = pos + 1
    END FOREACH

    IF (pos-1) >= 1 THEN
        CALL  SET_COUNT(pos-1)
        DISPLAY BY NAME vcount
        DISPLAY BY NAME vtotal 
        DISPLAY BY NAME vaprobados 
        DISPLAY BY NAME vrechazos 

        DISPLAY ARRAY ga_record TO scr_1.*

        ON KEY (INTERRUPT)
            CLEAR FORM
            EXIT DISPLAY

        END DISPLAY
    ELSE
        ERROR "ARCHIVO DE CURP VACIO"
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

FUNCTION desmarca_cuenta(vnss, vmarca, vusuario, vcorrelativo)
#dc-----------------------

  DEFINE
     vnss           CHAR(11),
     vmarca         SMALLINT,
     vusuario       CHAR(8),
     vcorrelativo   INTEGER,
     pestado_marca  SMALLINT,
     pmarca_causa   SMALLINT

  LET pestado_marca = 0
  LET pmarca_causa  = 0

  PREPARE eje_desmarca FROM ejecuta
 
  EXECUTE eje_desmarca
  USING vnss,
        vmarca,
        vcorrelativo,
        pestado_marca,
        pmarca_causa,
        vusuario

END FUNCTION

{FUNCTION llama_emision()
#lle-------------------

    DEFINE llamar CHAR(200)

    LET llamar = 'fglgo /safre/int/exp/INTB0112 S'
 
    RUN llamar

END FUNCTION}


{###############################################################
FUNCTION det_carta() #dc1

   LET reg_carta.nss      	= vn_seguro
   LET reg_carta.n_folio 	= folio_solic
   LET reg_carta.tipo_solicitud	= tipo_solic
   LET reg_carta.fecha_registro	= vfentcons
   LET reg_carta.opera_cod 	= NULL
   LET reg_carta.edo_genera 	= 10
   LET reg_carta.fecha_genera	= TODAY
   LET reg_carta.hora_genera	= TIME
   LET reg_carta.lote_genera	= 0
   LET reg_carta.consecutivo	= 0
   LET reg_carta.id_sepomex	= 0

   LET consulta_carta = "EXECUTE PROCEDURE inserta_carta (?,?,?,?,?,?,", 
 			"?,?,?,?,?,?)"

   PREPARE exe_sql FROM consulta_carta
   EXECUTE exe_sql USING reg_carta.* 

   INITIALIZE reg_carta.* TO NULL

END FUNCTION
###############################################################}

FUNCTION st_sal_ren19() #REPORTE STATUS CURP (SALIDA RENAPO 19)

  CALL init()

  START REPORT afil023 TO GLISTA

  DECLARE cur_apt CURSOR FOR
  SELECT a.n_seguro, a.status_renapo, a.fecha_asignacion
    FROM afi_dispersa_curp a
   WHERE a.status_renapo = "19"
     AND a.fecha_actualiza = HOY
   ORDER BY a.fecha_asignacion

  FOREACH cur_apt INTO dispersa.*

     SELECT b.nombres, b.paterno, b.materno, b.n_folio
       INTO mae.*
       FROM afi_mae_afiliado b
      WHERE b.n_seguro = dispersa.n_seguro

     IF STATUS != NOTFOUND THEN
        LET regs = regs + 1

        OUTPUT TO REPORT afil023(dispersa.*, mae.*, regs, fecha_ini, fecha_fin)
     END IF

  END FOREACH
  FINISH REPORT afil023

  RETURN regs

END FUNCTION

FUNCTION init()

  INITIALIZE dispersa.*,   mae.*, g_paramgrales.* TO NULL
  INITIALIZE GLISTA, usuario TO NULL

  LET regs           = 0
  LET HOY            = TODAY
  LET fecha_ini      = HOY
  LET fecha_fin      = HOY

  SELECT *, USER
    INTO g_paramgrales.*, usuario
    FROM seg_modulo
   WHERE modulo_cod = "afi"

  LET GLISTA = g_paramgrales.ruta_listados CLIPPED,"/",usuario CLIPPED,
               ".ST_REN_19." CLIPPED,
               TODAY USING "ddmmyy"

END FUNCTION

REPORT afil023(dispersa, mae, regs, fecha_ini, fecha_fin)

  DEFINE dispersa         RECORD
         n_seguro         LIKE afi_dispersa_curp.n_seguro,
         status_renapo    LIKE afi_dispersa_curp.status_renapo,
         fecha_asignacion LIKE afi_dispersa_curp.fecha_asignacion
                          END RECORD,

         mae              RECORD
         nombres          LIKE afi_mae_afiliado.nombres,
         paterno          LIKE afi_mae_afiliado.paterno,
         materno          LIKE afi_mae_afiliado.materno,
         n_folio          LIKE afi_mae_afiliado.n_folio
                          END RECORD,

         fecha_ini        DATE,
         fecha_fin        DATE,
         fecha            DATE,
         usuario          CHAR(08),

         regs             INTEGER

  OUTPUT
     LEFT   MARGIN 0
     RIGHT  MARGIN 0
     TOP    MARGIN 0
     BOTTOM MARGIN 0
     PAGE   LENGTH 66

  FORMAT
     PAGE HEADER
     PRINT COLUMN 67, "REPORTE DE STATUS DE STATUS SALIDA RENAPO 19"
     PRINT COLUMN 77, fecha_ini USING "dd/mm/yyyy",
                      " A ",fecha_fin USING "dd/mm/yyyy"
     PRINT COLUMN 160, "Prog. : AFIL044"
     PRINT COLUMN 160, "Fecha : ",TODAY USING "dd/mm/yyyy"

     PRINT COLUMN 01, "==================================================",
           COLUMN 51, "==================================================",
           COLUMN 101, "==================================================",
           COLUMN 151, "===========================" ##177

     PRINT COLUMN 1, "CONSEC.",
           COLUMN 12, "F O L I O ",
           COLUMN 26, "N.  S.  S. ",
           COLUMN 41, "APELLIDO PATERNO",
           COLUMN 76, "APELLIDO MATERNO",
           COLUMN 111, "N O M B R E S ",
           COLUMN 146, "STATUS RENAPO",
           COLUMN 163, "FECHA ASIGNACION"

     PRINT COLUMN 01, "==================================================",
           COLUMN 51, "==================================================",
           COLUMN 101, "==================================================",
           COLUMN 151, "===========================" ##77

    ON EVERY ROW
     PRINT COLUMN 1, regs USING "######&",
           COLUMN 12, mae.n_folio,
           COLUMN 26, dispersa.n_seguro,
           COLUMN 41, mae.paterno CLIPPED,
           COLUMN 76, mae.materno CLIPPED,
           COLUMN 111, mae.nombres CLIPPED,
           COLUMN 151, dispersa.status_renapo,
           COLUMN 165, dispersa.fecha_asignacion

END REPORT


REPORT listado_3(lgenerar, ltotal, laprobados, lrechazados)    ---erm 13 Marzo 2006

  DEFINE
    cont_reg    INTEGER,
    l_sol       CHAR(12),
    g_nombre    CHAR(200),
    lgenerar    CHAR(30),
    ltotal      SMALLINT,
    laprobados  SMALLINT,
    lrechazados SMALLINT

  OUTPUT
    LEFT MARGIN   0
    RIGHT MARGIN  0
    TOP MARGIN    0
    BOTTOM MARGIN 0

  FORMAT
--    PAGE HEADER
--    ON EVERY ROW

    ON LAST ROW

      PRINT
        COLUMN 03,"========================================",
        COLUMN 40,"==================================================="
      PRINT
        COLUMN 15," ARCHIVO DE DISPERSION DE CURP "
      PRINT
        COLUMN 03,"----------------------------------------",
        COLUMN 40,"---------------------------------------------------"
      SKIP 2 LINE
      PRINT
        COLUMN 03,"Fecha                  : ", TODAY USING "dd-mm-yyyy"
      SKIP 1 LINE
      PRINT 
        COLUMN 03,"Clave Operador         : ", g_usuario CLIPPED
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Nombre Archivo Procesar: ","__________________________________"
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Nombre de Archivo Procesado: ",lgenerar CLIPPED
      SKIP 1 LINE
      PRINT 
        COLUMN 03,"Total Registros del Lote            : ", ltotal USING "#######&"
      SKIP 1 LINE
      PRINT
        COLUMN 03,"No. Registros con CURP actualizados : ", laprobados USING "#######&"
      SKIP 1 LINE
      PRINT
        COLUMN 03,"No. Registros con CURP rechazados   : ", lrechazados USING "#######&"
      SKIP 1 LINE
      PRINT
         COLUMN 03,"Nombre y Ruta Reporte a Detalle :"
      PRINT
        COLUMN 05,G_LISTA CLIPPED
      SKIP 2 LINE
      PRINT
        COLUMN 03,"========================================",
        COLUMN 40,"==================================================="

END REPORT
