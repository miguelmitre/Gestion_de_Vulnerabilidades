#############################################################################
#Proyecto          => AFORES ( MEXICO )                                     #
#Propietario       => E.F.P.                                                #
#Programa AFIC051  => CARGA DE ARCHIVO DISPERSION CURP FECHAS ANTERIORES    #
#Sistema           => AFI.                                                  #
#Autor             => MAURO MUNIZ CABALLERO                                 #
#Fecha             => 18 DE ENERO DE 2001                                   #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_reg RECORD
        generar CHAR(20)
    END RECORD

    DEFINE aux_pausa        CHAR(1)
    DEFINE vresp            CHAR(1)
    DEFINE vcod_operacion   CHAR(2)
    DEFINE vstatus_ren      CHAR(2)
    DEFINE vhora            CHAR(8)
    DEFINE vdiag_proceso    CHAR(8)
    DEFINE hora             CHAR(8)
    DEFINE cfecha_a8        CHAR(8)
    DEFINE g_usuario        CHAR(8)
    DEFINE cfecha_asig      CHAR(10)
    DEFINE vf_rechazo       CHAR(10)
    DEFINE vfec_alta_curp   CHAR(10)
    DEFINE vn_seguro        CHAR(11)
    DEFINE varchivo         CHAR(14)
    DEFINE vn_folio         CHAR(18)
    DEFINE vcurp            CHAR(18)
    DEFINE vcurp_solicitud  CHAR(18)
    DEFINE vvcurp           CHAR(18)
    DEFINE vdesc_cod_39     CHAR(30)
    DEFINE vent_asigna      CHAR(30)
    DEFINE vpaterno         CHAR(40)
    DEFINE vmaterno         CHAR(40)
    DEFINE vnombres         CHAR(40)
    DEFINE mpaterno         CHAR(40)
    DEFINE mmaterno         CHAR(40)
    DEFINE mnombres         CHAR(40)
    DEFINE carga            CHAR(50)
    DEFINE ejecuta          CHAR(100)
    DEFINE corr             CHAR(100)

    DEFINE vstat_int        SMALLINT
    DEFINE vst_int          SMALLINT

    DEFINE total_reg        INTEGER
    DEFINE g_plano1         INTEGER
    DEFINE vtotal           INTEGER
    DEFINE vaprobados       INTEGER
    DEFINE vrechazados      INTEGER
    DEFINE vduplicados      INTEGER
    DEFINE vcodigo_39       INTEGER

    DEFINE hoy              DATE
    DEFINE vhoy             DATE
    DEFINE vfecha_asig      DATE
    DEFINE f_modifica       DATE

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
        xcodigo_marca   SMALLINT,
        xcodigo_rechazo SMALLINT,
        marca_dom       SMALLINT

    DEFINE 
        cdiag_proceso CHAR(3)  ,
        curp_reg      CHAR(18) ,
        sdiag_proceso SMALLINT ,
        tipo_solic    SMALLINT ,
        long_curp     SMALLINT ,
        folio_solic   DECIMAL(8,0)

    DEFINE reg_disp_curp RECORD
        nss            CHAR(11),
        nombre         CHAR(50),
        n_unico        CHAR(18),
        curp_arh       CHAR(18),
        status_reg     SMALLINT,
        status_interno SMALLINT,
        desc_st_int    CHAR(25),
        desc_oper      CHAR(22)
    END RECORD

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG("AFIC051.log")
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

    LET bnd_proceso         = 0

    LET marca               = 605

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
    END IF 

    LET hoy = TODAY
    LET hora = TIME
    LET vhora = hora[1,2],hora[4,5],hora[7,8]

    LET g_reg.generar = "S"

    SELECT *, USER
    INTO   g_seg_modulo.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE plano2_curp
        DROP TABLE disp_curp_ant

        CREATE TABLE plano2_curp
            (n_registros CHAR(490))

        CREATE TABLE disp_curp_ant
            (nss            CHAR(11),
             nombre         CHAR(50),
             n_unico        CHAR(18),
             curp_arh       CHAR(18),
             status_reg     SMALLINT,
             status_interno SMALLINT,
             desc_st_int    CHAR(25),
             desc_oper      CHAR(25))

    WHENEVER ERROR STOP

    DATABASE safre_af

END FUNCTION

FUNCTION crea_tablas() 
#ct-------------------

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE safre_tmp:encabezado2_curp
        DROP TABLE safre_tmp:detalle2_curp
        DROP TABLE safre_tmp:sumario2_curp
    WHENEVER ERROR STOP

    CREATE TABLE safre_tmp:encabezado2_curp
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

    CREATE TABLE safre_tmp:detalle2_curp
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
         nombres_renapo      CHAR(40)
        )

    CREATE TABLE safre_tmp:sumario2_curp
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

    DATABASE safre_af

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIC0051" ATTRIBUTE(BORDER)
    DISPLAY " AFIC051      CARGA ARCHIVO DISPERSION CURP FECHA ANTERIOR                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
    DISPLAY g_seg_modulo.ruta_rescate AT 10,10

    INPUT BY NAME g_reg.generar
        AFTER FIELD generar
        IF g_reg.generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        SELECT nombre_archivo 
        INTO   varchivo 
        FROM   afi_ctr_arh_curp
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
        LET carga = g_seg_modulo.ruta_rescate CLIPPED,"/",
                    g_reg.generar CLIPPED

        WHENEVER ERROR CONTINUE
            LOAD FROM carga DELIMITER "," 
            INSERT INTO safre_tmp:plano2_curp
        WHENEVER ERROR STOP

        SELECT COUNT(*) 
        INTO   g_plano1 
        FROM   safre_tmp:plano2_curp

        IF g_plano1 IS NULL OR g_plano1 = 0 THEN
            ERROR "NOMBRE DE ARCHIVO INCORRECTO O VACIO, VERIFIQUE " 
            SLEEP 5
            EXIT PROGRAM
        END IF

        ERROR "Procesando Informacion"

        CALL rescata_valores() #

        EXIT PROGRAM

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
    CALL lista_err()          #le

END FUNCTION

FUNCTION actualiza_datos()
#ad-----------------------

    DEFINE 
        cont_reg      INTEGER
 
    DEFINE 
        carga_reg     CHAR(490)

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
    FROM   safre_tmp:plano2_curp

    DECLARE cursor_1 CURSOR FOR 
    SELECT  * 
    FROM    safre_tmp:plano2_curp

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

            INSERT INTO safre_tmp:encabezado2_curp
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

            INSERT INTO safre_tmp:detalle2_curp
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
                    campo_42
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

           INSERT INTO safre_tmp:sumario2_curp
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
        rechazo_09   CHAR(02),
        rechazo_001  CHAR(02),
        rechazo_002  CHAR(02),
        rechazo_003  CHAR(02),
        rechazo_019  CHAR(02),
        rechazo_020  CHAR(03),
        rechazo_021  CHAR(02)

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
    FROM safre_tmp:encabezado2_curp

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

    DEFINE sw_curp SMALLINT

    IF NOT bnd_proceso THEN
        OPEN WINDOW w_curp AT 8,4 WITH FORM "AFIC0052"
        ATTRIBUTE (BORDER)
    END IF

    SELECT COUNT(*)
    INTO   vtotal
    FROM   safre_tmp:detalle2_curp

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
           b.status_interno,
           b.paterno,
           b.materno,
           b.nombres
    FROM   safre_tmp:detalle2_curp a, afi_mae_afiliado b
    WHERE  b.n_seguro = a.nss_solicitud 
    ORDER BY a.nss_solicitud

    FOREACH c_curp INTO vn_seguro,
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
                        vstat_int,
                        mpaterno,
                        mmaterno,
                        mnombres

        LET vvcurp          = vcurp_solicitud
        LET vcurp_solicitud = LENGTH(vcurp_solicitud)
        LET vhoy            = TODAY
        LET sdiag_proceso   = vdiag_proceso[1,3]

        LET reg_disp_curp.nss      = vn_seguro
        LET reg_disp_curp.nombre   = mpaterno CLIPPED, " ",
                                     mmaterno CLIPPED, " ",
                                     mnombres CLIPPED
        LET reg_disp_curp.n_unico  = curp_reg
        LET reg_disp_curp.curp_arh = vvcurp

        LET reg_disp_curp.status_interno = vstat_int

        LET long_curp = LENGTH(curp_reg)

        IF long_curp < 18 OR
           curp_reg[1] = ' ' THEN
            LET sw_curp = 0
        ELSE
            LET sw_curp = 1
        END IF

        IF cfecha_a8 IS NULL OR
           cfecha_a8 MATCHES ' *'THEN
            LET vfecha_asig = '01/01/0001'
        ELSE
            LET cfecha_asig = cfecha_a8[5,6],'/',
                              cfecha_a8[7,8],'/',
                              cfecha_a8[1,4]

            LET vfecha_asig = cfecha_asig
        END IF

        IF vcod_operacion = '01' THEN
            IF vstatus_ren = '10' THEN
                LET marca_dom = 1
            ELSE
                LET marca_dom = 0
            END IF
        ELSE
            LET marca_dom = 0
        END IF

       {
        IF cfecha_asig IS NULL AND
           sw_curp = 1 THEN
            CONTINUE FOREACH
        END IF
       }

---- verifica si esta en proceso de modificacion

        IF vstat_int = 130 THEN
            LET reg_disp_curp.status_reg = 0
            LET reg_disp_curp.desc_oper  = 'PROCESO MODIF DATOS'
            CALL inserta_disp_ant()
            CONTINUE FOREACH
        END IF

        IF vcod_operacion = "02" THEN
            IF vstatus_ren = 16 OR
               vstatus_ren = 17 OR
               vstatus_ren = 18 OR
               vstatus_ren = 34 THEN
                LET vduplicados = vduplicados + 1

                IF curp_reg IS NULL OR
                   curp_reg MATCHES " *" OR 
                   long_curp <> 18 THEN
                    CALL sin_curp() #sc
                ELSE
                    CALL con_curp() #cc
                END IF
            ELSE
                LET vrechazados = vrechazados + 1

                SELECT max(fecha_asignacion)
                INTO   f_modifica
                FROM   afi_dispersa_curp adc
                WHERE  adc.n_seguro = vn_seguro
                AND   (adc.cod_operacion = '02'
                OR     adc.status_renapo NOT IN(16,17,18,34))

                IF f_modifica IS NULL THEN
                    LET f_modifica = '01/01/0001'
                END IF

                IF f_modifica < vfecha_asig THEN
                    CASE vstatus_ren
                        WHEN 20 
                            LET reg_disp_curp.status_interno = 140

                            UPDATE afi_mae_afiliado
                            SET    status_interno = reg_disp_curp.status_interno
                            WHERE  n_seguro = vn_seguro
                        WHEN 19
                            IF curp_reg IS NULL OR
                               curp_reg MATCHES " *" OR 
                               long_curp <> 18 THEN
                                LET reg_disp_curp.status_interno = 140

                                UPDATE afi_mae_afiliado
                                SET    status_interno = 
                                           reg_disp_curp.status_interno
                                WHERE  n_seguro = vn_seguro
                            ELSE
                                LET reg_disp_curp.status_interno = 150

                                UPDATE afi_mae_afiliado
                                SET    status_interno = 
                                           reg_disp_curp.status_interno
                                WHERE  n_seguro = vn_seguro
                            END IF
                        OTHERWISE
                            LET reg_disp_curp.status_interno = 150

                            UPDATE afi_mae_afiliado
                            SET    status_interno = reg_disp_curp.status_interno
                            WHERE  n_seguro = vn_seguro
                    END CASE
                    LET reg_disp_curp.desc_oper  = 'DISPERSION APLICADA'
                    LET reg_disp_curp.status_reg = 6
                ELSE
                    LET reg_disp_curp.desc_oper  = 'DISPERSION NO APLICADA'
                    LET reg_disp_curp.status_reg = 7
                END IF

                CALL inserta_disp_ant()

            END IF
        ELSE
            LET vaprobados = vaprobados + 1

            IF curp_reg IS NULL OR
               curp_reg MATCHES " *" OR 
               long_curp <> 18 THEN
                CALL sin_curp() #sc
            ELSE
                CALL con_curp() #cc
            END IF
        END IF

    END FOREACH

    -- actualiza historico de archivos curp afi_ctr_arh_curp

    INSERT INTO afi_ctr_arh_curp
    VALUES (g_reg.generar, vtotal, vaprobados, vrechazados, vduplicados, vhoy)

    IF NOT bnd_proceso THEN
        DISPLAY "                      DATOS A DISPERSAR"
        AT 10,1 ATTRIBUTE ( REVERSE )

        DISPLAY "Total de Registros del lote     : ",
                 vtotal USING "#######&" AT 6,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros con CURP actualizados : ",
                 vaprobados USING "#######&" AT 8,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros con CURP rechazados   : ",
                 vrechazados USING "#######&" AT 10,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros con CURP duplicada   : ",
                 vduplicados USING "#######&" AT 12,15 ATTRIBUTE ( BOLD )

        PROMPT "Proceso Finalizado Satisfactoriamente  Presione <ENTER> Para Continuar"
        ATTRIBUTE (REVERSE)
        FOR vresp
        ATTRIBUTE (REVERSE)

        CLOSE WINDOW w_curp
    ELSE
        DISPLAY "                      DATOS A DISPERSAR"

        DISPLAY "Total de Registros del lote     : ",
                 vtotal USING "#######&" 

        DISPLAY "Registros con CURP actualizados : ",
                 vaprobados USING "#######&" 

        DISPLAY "Registros con CURP rechazados   : ",
                 vrechazados USING "#######&" 

        DISPLAY "Registros con CURP duplicados   : ",
                 vduplicados USING "#######&" 
    END IF

    RETURN

END FUNCTION

FUNCTION sin_curp()
#sc----------------

    IF vstat_int = 160 THEN
        SELECT max(fecha_asignacion)
        INTO   f_modifica
        FROM   afi_dispersa_curp adc
        WHERE  adc.n_seguro = vn_seguro
        AND   (adc.cod_operacion = '01'
        OR     adc.status_renapo in(16,17,18,34))

        IF f_modifica IS NULL THEN
            LET f_modifica = '01/01/0001'
        END IF

        IF f_modifica < vfecha_asig THEN
            LET reg_disp_curp.status_interno = 200
            LET reg_disp_curp.status_reg     = 6
            LET reg_disp_curp.desc_oper = 'DISPERSION APLICADA'
            CALL act_curp1()
            CALL desmarca_cuenta()
            CALL inserta_disp_ant()
        ELSE
            LET reg_disp_curp.status_interno = 160
            LET reg_disp_curp.status_reg     = 6
            LET reg_disp_curp.desc_oper = 'DISPERSION APLICADA'
            CALL act_curp1()
            CALL inserta_disp_ant()
        END IF
    ELSE
        LET reg_disp_curp.status_interno = 200
        LET reg_disp_curp.status_reg     = 6
        LET reg_disp_curp.desc_oper = 'DISPERSION APLICADA'
        CALL act_curp1()
        CALL inserta_disp_ant()
    END IF

END FUNCTION

FUNCTION con_curp()
#cc----------------

    SELECT max(fecha_asignacion)
    INTO   f_modifica
    FROM   afi_dispersa_curp adc
    WHERE  adc.n_seguro = vn_seguro
    AND   (adc.cod_operacion = '01'
    OR     adc.status_renapo in(16,17,18,34))

    IF f_modifica IS NULL THEN
        LET f_modifica = '01/01/0001'
    END IF

    IF curp_reg <> vvcurp THEN
        IF f_modifica <>'01/01/0001' AND
           vfecha_asig <> '01/01/0001' THEN
            IF f_modifica < vfecha_asig THEN
                IF vstat_int = 160 THEN
                    CALL desmarca_cuenta()
                END IF

                LET reg_disp_curp.status_interno = 200
                LET reg_disp_curp.status_reg     = 1
                LET reg_disp_curp.desc_oper = 'DISPERSION APLICADA'
                CALL act_curp1()
            ELSE
                LET reg_disp_curp.status_interno = 160
                LET reg_disp_curp.status_reg     = 5
                LET reg_disp_curp.desc_oper = 'DISPERSION NO APLICADA'
            END IF
        ELSE
            IF f_modifica = '01/01/0001' THEN
                IF vfecha_asig <> '01/01/0001' THEN
                    LET reg_disp_curp.status_reg = 2
                    LET reg_disp_curp.desc_oper  = 'DISPERSION NO APLICADA'
                ELSE
                    LET reg_disp_curp.status_reg = 3
                    LET reg_disp_curp.desc_oper  = 'DISPERSION NO APLICADA'
                END IF
            ELSE
                IF vfecha_asig = '01/01/0001' THEN
                    LET reg_disp_curp.status_reg = 4
                    LET reg_disp_curp.desc_oper  = 'DISPERSION NO APLICADA'
                END IF
            END IF
        END IF
    ELSE
        LET reg_disp_curp.desc_oper  = 'DISPERSION NO APLICADA'
        LET reg_disp_curp.status_reg = 7
    END IF

    CALL inserta_disp_ant()

END FUNCTION

FUNCTION lista_err()
#-------------------

    DEFINE vdiag_proceso  CHAR(8)
    DEFINE vcont          INTEGER

    DEFINE gr_curp RECORD
        n_seguro        CHAR(11),
        nombres         CHAR(50),
        n_unico         CHAR(18),
        curp_arh        CHAR(18),
        status_reg      SMALLINT,
        status_interno  SMALLINT,
        desc_st_int     CHAR(25),
        desc_oper       CHAR(25)
    END RECORD

    DEFINE G_LISTA CHAR(200)
    DEFINE G_LIST1 CHAR(200)
    DEFINE G_IMPRE CHAR(200)
    DEFINE resp    CHAR(1)

    LET G_LISTA = g_seg_modulo.ruta_listados CLIPPED,"/",
                  g_usuario CLIPPED,".DISP_ANT.",
                  HOY USING "ddmmyy","-",vhora CLIPPED

    START REPORT listado TO G_LISTA

    DECLARE c_carga CURSOR FOR

    SELECT *
    FROM   safre_tmp:disp_curp_ant
    ORDER BY desc_oper, status_interno

    FOREACH c_carga INTO gr_curp.*
        OUTPUT TO REPORT listado(gr_curp.*)
    END FOREACH 

    FINISH REPORT listado

    ERROR"LISTADO GENERADO" SLEEP 2

    LET G_LIST1 = "chmod 777 ",g_seg_modulo.ruta_listados CLIPPED,"/",
                  g_usuario CLIPPED,".DISP_ANT.",
                  HOY USING "ddmmyy","-",vhora CLIPPED

    RUN G_LIST1

    LET G_IMPRE = "lp ", G_LISTA
    RUN G_IMPRE

END FUNCTION

REPORT listado(rpt)

    DEFINE rpt RECORD
        n_seguro        CHAR(11),
        nombre          CHAR(50),
        n_unico         CHAR(18),
        curp_arh        CHAR(18),
        status_reg      SMALLINT,
        status_interno  SMALLINT,
        desc_st_int     CHAR(25),
        desc_oper       CHAR(25)
    END RECORD

    DEFINE l_estado CHAR(16)

    DEFINE
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
            COLUMN 001,"========================================",
            COLUMN 040,"========================================",
            COLUMN 080,"========================================",
            COLUMN 120,"========================================",
            COLUMN 160,"=========="
        PRINT
            COLUMN 001,razon_social,
            COLUMN 140,"FECHA   :",hoy USING "DD/MM/YY",
            COLUMN 149,hora [1,5]
        PRINT
            COLUMN 001,"AFIC051",
            COLUMN 035," E S T A D O   D E   C U R P   D I S P E R S A D A S   F E C H A S   A N T E R I O R E S",
            COLUMN 140,"NUM.PAG.:",pageno USING "####,###"
            SKIP 1 LINE
        PRINT
            COLUMN 010,"Archivo procesado :  ", g_reg.generar
            SKIP 1 LINE
        PRINT
            COLUMN 001,"----------------------------------------",
            COLUMN 040,"----------------------------------------",
            COLUMN 080,"----------------------------------------",
            COLUMN 120,"----------------------------------------",
            COLUMN 160,"----------"
       SKIP 1 LINES
       PRINT
           COLUMN 001, "N.S.S.",
           COLUMN 014, "NOMBRE",
           COLUMN 066, "CURP BD",
           COLUMN 086, "CURP ARCHIVO",
           COLUMN 106, "ST REG",
           COLUMN 114, "ST INT",
           COLUMN 144, "OPERACION"
       PRINT
           COLUMN 001,"========================================",
           COLUMN 040,"========================================",
           COLUMN 080,"========================================",
           COLUMN 120,"========================================",
           COLUMN 160,"=========="

    ON EVERY ROW
       LET vcont = vcont + 1

       PRINT
           COLUMN 001, rpt.n_seguro,
           COLUMN 014, rpt.nombre,
           COLUMN 066, rpt.n_unico,
           COLUMN 086, rpt.curp_arh,
           COLUMN 106, rpt.status_reg,
           COLUMN 114, rpt.status_interno USING "&&&",
           COLUMN 118, rpt.desc_st_int,
           COLUMN 148, rpt.desc_oper

    #----------
    ON LAST ROW
    #----------

    SKIP 1 LINES 
        PRINT
           COLUMN 001,"========================================",
           COLUMN 040,"========================================",
           COLUMN 080,"========================================",
           COLUMN 120,"========================================",
           COLUMN 160,"=========="
        PRINT 
            "TOTAL DE CURP DISPERSADAS : ", vcont  USING "&&&&&&"

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

FUNCTION act_curp1()
#fac-----------------

    UPDATE afi_mae_afiliado
    SET    n_unico = vvcurp,
           status_interno = reg_disp_curp.status_interno
    WHERE  n_seguro = vn_seguro

    INSERT INTO afi_dispersa_curp
    VALUES (vn_seguro,
            vcod_operacion,
            vdiag_proceso,
            vhoy,
            g_reg.generar,
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

END FUNCTION

FUNCTION inserta_disp_ant()
#ida-----------------------

    SELECT estado_desc 
    INTO   reg_disp_curp.desc_st_int
    FROM   tab_status_afi
    WHERE  estado_cod = reg_disp_curp.status_interno

    INSERT INTO safre_tmp:disp_curp_ant VALUES(reg_disp_curp.*)

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

FUNCTION desmarca_cuenta()
#dc-----------------------

   LET ejecuta = "EXECUTE PROCEDURE desmarca_cuenta(","'",vn_seguro,"'",
                                                    ",",marca,",",
                                                    "'",g_usuario,"'",")"

    LET ejecuta = ejecuta CLIPPED

    PREPARE clausula_spl2 FROM ejecuta

    DECLARE cursor_desmarca CURSOR FOR clausula_spl2

    OPEN cursor_desmarca

    FETCH cursor_desmarca INTO xcodigo_marca, xcodigo_rechazo

    CLOSE cursor_desmarca

END FUNCTION

