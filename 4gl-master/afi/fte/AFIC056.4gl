############################################################################
#Proyecto          => AFORES ( MEXICO )                                    #
#Propietario       => E.F.P.                                               #
#Programa AFIC056  => CARGA DE ARCHIVO RESPUESTA (OP 19 BAJA CURP)         #
#Sistema           => AFI.                                                 #
#Autor             => Eduardo Joaquin Resendiz Medina                      #
#Fecha             => 30 DE ABRIL DE 2010                                  #
############################################################################
#Modificado        => Marisol Gonzalez                                      #
#Fecha             => 25/06/2014                                            #
#                  => Se adecua de acuerdo al INV-2777                      #
#############################################################################
#INV-2826          => Cambio de formato de entrada Mar 2015                 #
#############################################################################
DATABASE safre_af

GLOBALS
    DEFINE reg_mod       RECORD LIKE afi_mae_modifica.*
    DEFINE reg_ant       RECORD LIKE afi_mae_afiliado.*
    DEFINE g_paramgrales RECORD LIKE seg_modulo.*

    DEFINE
        vresp               CHAR(1),
        aux_pausa           CHAR(1),
        enter               CHAR(1),
        vcod_operacion      CHAR(2),
        vdiag_proceso       CHAR(3),
        vf_rechazo          CHAR(8),
        vfec_emision_certif CHAR(10),
        vn_seguro           CHAR(11),
        varchivo            CHAR(14),
        vcurp               CHAR(18),
        vcurp_solicitud     CHAR(18),
        vvcurp              CHAR(18),
        generar             CHAR(20),
        vdesc_cod_39        CHAR(30),
        operacion           CHAR(40),
        carga               CHAR(50),
        vnombre             CHAR(50),
        corr                CHAR(100),
        vn_folio            DECIMAL(10,0),	#0806 CIL Fol-10
        vn_folio_tram       INTEGER,
        vcircular_tmte      INTEGER  

    DEFINE
        vhoy ,
        HOY,
        xx_fecha  DATE

    DEFINE
        marca     ,
        contar_det  ,
        bnd_proceso SMALLINT

    DEFINE
        vcodigo_39       ,
        total_reg        ,
        g_plano_procesar ,
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
        opc             CHAR(1) ,
        vnss            CHAR(11) ,
        vmarca_entra    SMALLINT ,
        vmarca_estado   SMALLINT ,
        vcodigo_rechazo SMALLINT ,
        g_usuario       CHAR(8)  ,
        ejecuta         CHAR(300),
        xcodigo_marca   SMALLINT ,
        xcodigo_rechazo SMALLINT ,
        edo_proc        SMALLINT ,
        vnom_afore      CHAR(122) ,
        vnom_proce      CHAR(122) ,
        v_marca         CHAR(100) ,
        v_desmarca      CHAR(100)

    DEFINE
        reg_carta          RECORD LIKE safre_af:int_ctr_carta.*,
        consulta_carta     CHAR(120)

    DEFINE
      vcve_afo_nss_a  CHAR(3),
      vid_tip_trab    CHAR(1),
      vdiag_proceso15 CHAR(15),
      vcurp_oficial   CHAR(18),
      vfec_env_curp   DATE

END GLOBALS

MAIN
    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG(FGL_GETENV('USER')||'AFIC056.log')
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

    IF reg_bat.pid THEN
       DISPLAY "INICIANDO PROCESO ..."
       LET bnd_proceso = 1
    END IF

    LET HOY      = TODAY
    LET marca    = 600
    LET edo_proc = 605
    LET generar  = "S"

    LET vmarca_estado   = 0
    LET vcodigo_rechazo = 0

    SELECT *, USER
      INTO g_paramgrales.*, g_usuario
      FROM seg_modulo
     WHERE modulo_cod = 'afi'

    CREATE TEMP TABLE plano_procesar
        (n_registros CHAR(500))

    LET v_marca    = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? )"
    LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
END FUNCTION

FUNCTION crea_tablas()
#ct-------------------
    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp

        DROP TABLE cza_procesar
        DROP TABLE det_procesar
        DROP TABLE sum_procesar
    WHENEVER ERROR STOP

    CREATE TABLE cza_procesar
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

     CREATE TABLE det_procesar
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
         cadena_dif          CHAR(17),
         folio_solicitud     DECIMAL(10,0),
         sexo                SMALLINT,
         entidad_nacimiento  CHAR(02),
         ind_infonavit       CHAR(01),
         nacionalidad        CHAR(03),
         tip_prob            CHAR(01),
         fol_prob            CHAR(10),
         doc_prob            CHAR(16),
         cve_afore_ced       CHAR(3),
         id_tip_trab         CHAR(1),
         cod_operacion       CHAR(02),
         diag_proceso        CHAR(15),
         ident_lote_origen   CHAR(16),
         nss_oficial         CHAR(11),
         ind_nss_modificado  CHAR(01),
         fec_emision_certif  CHAR(08),
         curp_oficial        CHAR(18),
         ind_curp_modif      CHAR(01),
         rfc_bd              CHAR(13),
         nombre_imss         CHAR(50),
         nombre_procanase    CHAR(50),
         fena_bd             CHAR(08),
         cve_agente_prom     CHAR(10),
         sexo_bd             CHAR(01));

    CREATE TABLE sum_procesar
        (campo1 CHAR(2),
         campo2 CHAR(2),
         campo3 CHAR(3),
         campo4 CHAR(8),
         campo5 CHAR(3),
         campo6 CHAR(2),
         campo7 CHAR(2),
         campo8 CHAR(9),
         campo9 CHAR(9))

    CREATE INDEX det_procesar_1 ON det_procesar(nss_solicitud)

    DATABASE safre_af
END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------
    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIC0041" ATTRIBUTE(BORDER)
    DISPLAY " AFIC056     CARGA DE ARCHIVO RESPUESTA SOLICITUD BAJA CURP                        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                             Ctrl-C > Salir                                        " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
    DISPLAY g_paramgrales.ruta_rescate AT 6,10

    INPUT BY NAME generar
    AFTER FIELD generar
        IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        SELECT nombre_archivo
          INTO varchivo
          FROM afi_ctr_arh_proc
         WHERE nombre_archivo = generar

        IF STATUS <> NOTFOUND THEN
            ERROR "ARCHIVO YA PROCESADO"
            SLEEP 1
            ERROR " "
            INITIALIZE generar TO NULL
            CLEAR FORM
            NEXT FIELD generar
        END IF

        LET carga = NULL
        LET carga = g_paramgrales.ruta_rescate CLIPPED,"/", generar CLIPPED

        WHENEVER ERROR CONTINUE
            LOAD FROM carga DELIMITER ","
            INSERT INTO plano_procesar
        WHENEVER ERROR STOP

        SELECT COUNT(*)
          INTO g_plano_procesar
          FROM plano_procesar

        IF g_plano_procesar IS NULL OR
           g_plano_procesar = 0 THEN
            ERROR "NOMBRE DE ARCHIVO INCORRECTO O VACIO, VERIFIQUE"
            SLEEP 3
            EXIT PROGRAM
        END IF
        ERROR "Procesando Informacion"
        CALL rescata_valores()
        EXIT PROGRAM

    ON KEY (control-b)
        CALL despliega_archivos()

    END INPUT
END FUNCTION

FUNCTION rescata_valores()
#rd-----------------------
    CALL crea_tablas()        #ct
    CALL actualiza_datos()    #ad
    CALL revisa_datos()       #rd
    CALL Actualiza_Maeafili() #am
    CALL lista_err()          #le
END FUNCTION

FUNCTION actualiza_datos()
#-------------------------
    DEFINE
        cont_reg      INTEGER

    DEFINE
        carga_reg     CHAR(570)

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
        --campo_11      CHAR(18),
        campo_12      CHAR(10),
        campo_13      CHAR(01),
        campo_14      CHAR(02),
        campo_15      CHAR(01),
        campo_16      CHAR(03),
        campo_17      CHAR(01),
        campo_18      CHAR(10),
        campo_19      CHAR(16),
        campo_20      CHAR(01),
        --campo_21      CHAR(03),
        campo_22      CHAR(03),
        campo_23      CHAR(01),
        --campo_24      CHAR(07),
        campo_25      CHAR(02),
        campo_26      CHAR(15),
        campo_27      CHAR(16),
        campo_28      CHAR(11),
        campo_29      CHAR(01),
        campo_30      CHAR(08),
        campo_31      CHAR(18),
        campo_32      CHAR(01),
        campo_33      CHAR(13),
        campo_34      CHAR(50),
        campo_35      CHAR(50),
        campo_36      CHAR(08),
        campo_37      CHAR(10),
        campo_38      CHAR(01),
        campo_39      CHAR(02),
        campo_40      CHAR(08),
        campo_41      CHAR(08),
        campo_42      CHAR(03),
        campo_43      CHAR(03),
        campo_44      CHAR(01),
        campo_45      CHAR(10),
        campo_46      CHAR(16),
        campo_47      CHAR(01),
        campo_48      CHAR(08),
        campo_49      CHAR(08),
        campo_50      CHAR(08),
        campo_51      CHAR(06),
        campo_52      CHAR(07),
        campo_53      CHAR(08),

        campo_201     CHAR(2),
        campo_202     CHAR(2),
        campo_203     CHAR(3),
        campo_204     CHAR(8),
        campo_205     CHAR(3),
        campo_206     CHAR(2),
        campo_207     CHAR(2),
        campo_208     CHAR(9),
        campo_209     CHAR(9)

    LET cont_reg = 0

    SELECT COUNT(*)
      INTO total_reg
      FROM plano_procesar

    DECLARE cursor_1 CURSOR FOR
       SELECT  *
         FROM plano_procesar

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

            INSERT INTO safre_tmp:cza_procesar
            VALUES (campo_011,
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
                    campo_212)
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
            LET campo_12 = carga_reg[203,212]
            LET campo_13 = carga_reg[213,213]
            LET campo_14 = carga_reg[214,215]
            LET campo_15 = carga_reg[216,216]
            LET campo_16 = carga_reg[217,219]
            LET campo_17 = carga_reg[220,220]
            LET campo_18 = carga_reg[221,230]
            LET campo_19 = carga_reg[231,246]
            LET campo_20 = carga_reg[247,247]
            LET campo_22 = carga_reg[251,253]
            LET campo_23 = carga_reg[254,254]
            LET campo_25 = carga_reg[262,263]
            LET campo_26 = carga_reg[264,278]
            LET campo_27 = carga_reg[279,294]
            LET campo_28 = carga_reg[295,305]
            LET campo_29 = carga_reg[306,306]
            LET campo_30 = carga_reg[307,314]
            LET campo_31 = carga_reg[315,332]
            LET campo_32 = carga_reg[333,333]
            LET campo_33 = carga_reg[334,346]
            LET campo_34 = carga_reg[347,396]
            LET campo_35 = carga_reg[397,446]
            LET campo_36 = carga_reg[447,454]
            LET campo_37 = carga_reg[455,464]
            LET campo_38 = carga_reg[465,465]
            LET campo_39 = carga_reg[466,467]
            LET campo_40 = carga_reg[468,475]
            LET campo_41 = carga_reg[476,483]
            LET campo_42 = carga_reg[484,486]
            LET campo_43 = carga_reg[487,489]
            LET campo_44 = carga_reg[490,490]
            LET campo_45 = carga_reg[491,500]
            LET campo_46 = carga_reg[501,516]
            LET campo_47 = carga_reg[517,517]
            LET campo_48 = carga_reg[518,525]
            LET campo_49 = carga_reg[526,533]
            LET campo_50 = carga_reg[534,541]
            LET campo_51 = carga_reg[542,547]
            LET campo_52 = carga_reg[548,554]
            LET campo_53 = carga_reg[555,562]

        INSERT INTO safre_tmp:det_procesar
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
                NULL    ,
                campo_12,
                campo_13,
                campo_14,
                campo_15,
                campo_16,
                campo_17,
                campo_18,
                campo_19,
                campo_22,
                campo_23,
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
                campo_38)
        END IF

        IF cont_reg = total_reg        THEN
            LET campo_201 = carga_reg[001,002]
            LET campo_202 = carga_reg[003,004]
            LET campo_203 = carga_reg[005,007]
            LET campo_204 = carga_reg[008,015]
            LET campo_205 = carga_reg[016,018]
            LET campo_206 = carga_reg[019,020]
            LET campo_207 = carga_reg[021,022]
            LET campo_208 = carga_reg[023,031]
            LET campo_209 = carga_reg[032,040]

            INSERT INTO safre_tmp:sum_procesar
            VALUES (campo_201,
                    campo_202,
                    campo_203,
                    campo_204,
                    campo_205,
                    campo_206,
                    campo_207,
                    campo_208,
                    campo_209)
        END IF
    END FOREACH
END FUNCTION

FUNCTION revisa_datos()
#----------------------
    DEFINE
        rechazo_lote CHAR(2),
        rechazo_deta CHAR(3),
        l_reg        RECORD LIKE tab_rch_lote.*,
        x_reg        RECORD LIKE tab_rdeta.*,
        aux_pausa    CHAR(1)

    DEFINE
        rechazo_09  CHAR(02),
        rechazo_001 CHAR(02),
        rechazo_002 CHAR(02),
        rechazo_003 CHAR(02),
        rechazo_019 CHAR(03),
        rechazo_020 CHAR(03),
        rechazo_021 CHAR(02)

    DEFINE
        uno  CHAR(03),
        dos  CHAR(03),
        tre  CHAR(03),
        cua  CHAR(03),
        cin   CHAR(03),
        l_status_int SMALLINT

    LET contar_det = 1

    # ENCABEZADO #

    SELECT campo1,
           campo2,
           campo3,
           campo5,
           campo6
      INTO rechazo_001,
           rechazo_002,
           rechazo_003,
           rechazo_lote,
           rechazo_019
      FROM safre_tmp:cza_procesar

    SELECT *
      INTO l_reg.*
      FROM tab_rch_lote
     WHERE rlote_cod = rechazo_lote

    IF STATUS  <> NOTFOUND THEN
       IF bnd_proceso THEN
          DISPLAY "Program stopped, ERROR DE PROCESO,        NO PUEDE CONTINUAR "
          EXIT PROGRAM
       ELSE
          CLEAR SCREEN
          DISPLAY l_reg.rlote_cod AT 10,1
          DISPLAY l_reg.rlote_desc_c AT 11,1
          PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
          EXIT PROGRAM
       END IF
    END IF

    IF rechazo_001 <> "01" THEN
       IF bnd_proceso THEN
          DISPLAY "Program stopped, Tipo de registro debe ser 01 en encabezado"
          EXIT PROGRAM
       ELSE
          CLEAR SCREEN
          DISPLAY "Tipo de registro debe ser 01 en ENCABEZADO" AT 10,1
          PROMPT "ERROR DE PROCESO NO        PUEDE CONTINUAR" FOR aux_pausa
          EXIT PROGRAM
       END IF
    END IF

    IF rechazo_002 <> "01" THEN
       IF bnd_proceso THEN
          DISPLAY "Program stopped, Identificador de servicio debe ser 01 en encabezado"
          EXIT PROGRAM
       ELSE
          CLEAR SCREEN
          DISPLAY "Identificador de Servicio debe ser 01 en ENCABEZADO" AT 10,1
          PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
          EXIT PROGRAM
       END IF
    END IF

    IF rechazo_003 <> "19" THEN
       IF bnd_proceso THEN
          DISPLAY "Program stopped, Identificador de operacion debe ser 13 en encabezado"
          EXIT PROGRAM
       ELSE
          CLEAR SCREEN
          DISPLAY "Identificador de Operacion debe ser 13 en ENCABEZADO" AT 10,1
          PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
          EXIT PROGRAM
       END IF
    END IF

    IF rechazo_019 <> "03" THEN
       IF bnd_proceso THEN
          DISPLAY "Program stopped, Codigo empresa operadora debe ser 03 en encabezado"
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

     # SUMARIO #

    SELECT campo1
      INTO rechazo_09
      FROM safre_tmp:sum_procesar

    IF rechazo_09 <> "09" THEN
       IF bnd_proceso THEN
          DISPLAY "Program stopped, Tipo registro debe ser 09 en resumen"
          EXIT PROGRAM
       ELSE
          CLEAR SCREEN
          DISPLAY "Tipo de registro debe ser 09 en RESUMEN" AT 10,1
          PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
          EXIT PROGRAM
       END IF
    END IF
END FUNCTION

FUNCTION Actualiza_Maeafili()
#----------------------------
    OPEN WINDOW w_curp AT 8,4 WITH FORM "AFIC0082"
    ATTRIBUTE (BORDER)

    LET vhoy = TODAY

    SELECT COUNT(*)
      INTO vtotal
      FROM safre_tmp:det_procesar

    DECLARE c_curp CURSOR FOR
       SELECT a.nss_solicitud,
              a.diag_proceso[1,3],
              a.cod_operacion,
              b.n_unico,
              b.n_folio,
              a.nombre_imss,
              a.cve_afore_ced,
              a.id_tip_trab,
              a.diag_proceso,
              a.curp_oficial
         FROM safre_tmp:det_procesar a, afi_mae_modifica b
        WHERE a.nss_solicitud  = b.n_seguro
          AND b.status_interno = 250
          AND b.cod_operacion  = 0
          AND b.diag_proceso   = 0
       ORDER BY a.nss_solicitud

    FOREACH c_curp INTO vn_seguro,
                        vdiag_proceso,
                        vcod_operacion,
                        vcurp,
                        vn_folio,
                        vnombre,
                        vcve_afo_nss_a,
                        vid_tip_trab,
                        vdiag_proceso15,
                        vcurp_oficial

        IF vcurp_oficial IS NULL OR
           vcurp_oficial[1] = ' ' OR
           LENGTH(vcurp_oficial) < 18 THEN

           SELECT e.n_unico
           INTO vcurp_oficial
           FROM afi_mae_afiliado e
           WHERE e.n_seguro = vn_seguro         

        END IF

        LET marca = 600
        --CALL desmarca_cuenta(vn_seguro,marca,g_usuario,vn_folio)

        CASE vcod_operacion
            WHEN '02'
                LET vrechazados = vrechazados + 1

                   CALL actualiza_02()
                   CALL despliega_totales()

            WHEN '01'
                LET vaprobados = vaprobados + 1
                --CALL actualiza_03()
                CALL actualiza_01()
                CALL despliega_totales()

        END CASE
    END FOREACH

    INSERT INTO afi_ctr_arh_proc
       VALUES (generar,
               vtotal,
               vaprobados,
               vrechazados,
               vpendientes,
               vhoy)
{
    IF vaprobados > 0 THEN
       INSERT INTO afi_ctr_arh_mod
          VALUES (generar,
                  vtotal,
                  vaprobados,
                  0,
                  0,
                  vhoy)
    END IF
}
    CALL despliega_totales()

    IF NOT bnd_proceso THEN
        ERROR ""
        PROMPT "Proceso termino correcto, [Enter] Para Continuar "
        ATTRIBUTE (REVERSE)
        FOR vresp ATTRIBUTE (REVERSE)

        CLOSE WINDOW w_curp
    ELSE
        DISPLAY "Programa termino correcto"
    END IF
    RETURN
END FUNCTION

FUNCTION actualiza_02()
#a02-------------------
  DEFINE vrow     INTEGER

  INITIALIZE reg_mod.* TO NULL

  SELECT *, rowid
    INTO reg_mod.*, vrow
    FROM afi_mae_modifica
   WHERE n_seguro       = vn_seguro
     AND cod_operacion  = 0
     AND diag_proceso   = 0
     AND status_interno = 250

  SELECT fecha_envio
  INTO   vfec_env_curp
  FROM   afi_ctr_curp
  WHERE  nss             = vn_seguro
  AND    cve_operacion   = '19'
  AND    fecha_respuesta IS NULL

  IF vcurp_oficial IS NULL OR
     vcurp_oficial[1] = ' ' OR
     LENGTH(vcurp_oficial) < 18 THEN

     UPDATE afi_mae_afiliado
        SET status_interno  = 200
      WHERE n_seguro        = vn_seguro
        AND status_interno  in(250)

     UPDATE afi_mae_modifica
        SET cod_operacion   = vcod_operacion,
            diag_proceso    = vdiag_proceso,
            status_interno  = 270,
            nombre_pcanase  = vnombre
      WHERE n_seguro        = vn_seguro
        AND cod_operacion   = 0
        AND diag_proceso    = 0
  ELSE
     UPDATE afi_mae_afiliado
        SET status_interno  = 200
      WHERE n_seguro        = vn_seguro
        AND status_interno in(250)

     UPDATE afi_mae_modifica
        SET cod_operacion   = vcod_operacion,
            diag_proceso    = vdiag_proceso,
            status_interno  = 270,
            nombre_pcanase  = vnombre
      WHERE n_seguro        = vn_seguro
        AND cod_operacion   = 0
        AND diag_proceso    = 0
  END IF

  UPDATE afi_ctr_curp
  SET    cod_operacion      = vcod_operacion,
         diag_proceso       = vdiag_proceso15,
         fecha_respuesta    = TODAY
  WHERE  nss              = vn_seguro
  AND    cve_operacion    = '19'
  AND    fecha_respuesta IS NULL

  INSERT INTO afi_rechaza_proc
  VALUES (vn_seguro,
          vdiag_proceso,
          vhoy,
          generar)

   INSERT INTO afi_ctr_solicitud
      VALUES(reg_mod.tipo_solicitud,
             reg_mod.n_folio,
             reg_mod.n_seguro,
             '',                # nombre_trab_afo
             270,               # status_interno 
             vfec_env_curp,     # fecha_envio    
             TODAY,             # fecha_recepcion
             0,                 # cve_afore_ced  
             '',                # ind_nss_modif  
             '',                # fentcons       
             '',                # curp_oficial   
             '',                # ind_curp_modif 
             '',                # rfc_trab_bd    
             '',                # nombre_trab_bd 
             '',                # nombre_trab_pro
             '',                # fena_trab_bd   
             '',                # codven_bd      
             '',                # sexo_bd        
             '',                # estadon_bd     
             '',                # fecha_prim_afi 
             '',                # fecha_alta_act 
             '',                # cve_afore_afi  
             '',                # nacion_bd      
             '',                # tipo_prob      
             '',                # folio_prob     
             '',                # doc_prob       
             '',                # ind_envio      
             '',                # ind_nombre     
             vcod_operacion,    # cod_operacion  
             vdiag_proceso,     # diag_proceso   
             g_usuario,         # usuario        
             TODAY)             # factualiza     


END FUNCTION

FUNCTION actualiza_01()
#a01-------------------
    DEFINE marca2   SMALLINT
    DEFINE mrow     INTEGER
    DEFINE pat_proc CHAR(40)
    DEFINE mat_proc CHAR(40)
    DEFINE nom_proc CHAR(40)
    DEFINE vrow     INTEGER
    DEFINE f_recep  DATE
    DEFINE vnom_mod CHAR(120)
    DEFINE vnom_bd  CHAR(120)

    INITIALIZE reg_mod.* TO NULL

    LET vnom_afore = NULL
    LET vnom_proce = NULL

    LET marca2 = 610

    SELECT *, rowid
      INTO reg_mod.*, vrow
      FROM afi_mae_modifica
     WHERE n_seguro   = vn_seguro
       AND cod_operacion  = 0
       AND diag_proceso   = 0
       AND status_interno = 250

    SELECT a.paterno,
           a.materno,
           a.nombres,
           a.n_rfc,
           a.n_unico,
           a.sexo,
           a.edo_civil,
           a.fena,
           a.salario_base_comis,
           a.estadon,
           a.nacionalidad,
           a.tip_prob,
           a.fol_prob,
           a.doc_prob,
           a.const_curp,
           a.n_folio,
           a.fentcons,
           a.status_interno
      INTO reg_ant.paterno,
           reg_ant.materno,
           reg_ant.nombres,
           reg_ant.n_rfc,
           reg_ant.n_unico,
           reg_ant.sexo,
           reg_ant.edo_civil,
           reg_ant.fena,
           reg_ant.salario_base_comis,
           reg_ant.estadon,
           reg_ant.nacionalidad,
           reg_ant.tip_prob,
           reg_ant.fol_prob,
           reg_ant.doc_prob,
           reg_ant.const_curp,
           reg_ant.n_folio,
           reg_ant.fentcons,
           reg_ant.status_interno
      FROM afi_mae_afiliado a
     WHERE a.n_seguro = vn_seguro

     UPDATE afi_ctr_curp
     SET    cod_operacion      = vcod_operacion,
            diag_proceso       = vdiag_proceso15,
            fecha_respuesta    = TODAY
     WHERE  nss                = vn_seguro
     AND    n_folio            = reg_mod.n_folio
     AND    tipo_solicitud     = reg_mod.tipo_solicitud
     AND    cve_operacion      = '19'
     AND    fecha_respuesta IS NULL

    IF vcod_operacion = '01' THEN

      UPDATE afi_mae_afiliado
         SET status_interno  = 200
       WHERE n_seguro        = vn_seguro
         AND status_interno IN (250)
         AND tipo_solicitud IN (8,12,15,16,18)
         
       --INV-2777
       UPDATE afi_mae_afiliado
         SET status_interno  = 200,
             n_unico         = NULL
       WHERE n_seguro        = vn_seguro
         AND status_interno IN (250)
         AND tipo_solicitud NOT IN (8,12,15,16,18)
       --
       
      UPDATE afi_mae_modifica
         SET cod_operacion  = vcod_operacion,
             diag_proceso   = vdiag_proceso,
             status_interno = 260,
             nombre_pcanase = vnombre
       WHERE n_seguro       = vn_seguro
         AND cod_operacion  = 0
         AND diag_proceso   = 0


      INSERT INTO afi_ctr_solicitud
         VALUES(reg_mod.tipo_solicitud,
                reg_mod.n_folio,
                reg_mod.n_seguro,
                '',                # nombre_trab_afo
                260,               # status_interno 
                vfec_env_curp,     # fecha_envio    
                TODAY,             # fecha_recepcion
                0,                 # cve_afore_ced  
                '',                # ind_nss_modif  
                '',                # fentcons       
                '',                # curp_oficial   
                '',                # ind_curp_modif 
                '',                # rfc_trab_bd    
                '',                # nombre_trab_bd 
                '',                # nombre_trab_pro
                '',                # fena_trab_bd   
                '',                # codven_bd      
                '',                # sexo_bd        
                '',                # estadon_bd     
                '',                # fecha_prim_afi 
                '',                # fecha_alta_act 
                '',                # cve_afore_afi  
                '',                # nacion_bd      
                '',                # tipo_prob      
                '',                # folio_prob     
                '',                # doc_prob       
                '',                # ind_envio      
                '',                # ind_nombre     
                vcod_operacion,    # cod_operacion  
                vdiag_proceso,     # diag_proceso   
                g_usuario,         # usuario        
                TODAY)             # factualiza     

    END IF

    --CALL desmarca_cuenta(vn_seguro,marca2,g_usuario,vn_folio)

    #CALL valida_folio(vrow)
END FUNCTION

--->erm 01 Junio 2006
FUNCTION actualiza_03()
#a01-------------------
    DEFINE marca2   SMALLINT
    DEFINE mrow     INTEGER
    DEFINE pat_proc CHAR(40)
    DEFINE mat_proc CHAR(40)
    DEFINE nom_proc CHAR(40)
    DEFINE vrow     INTEGER
    DEFINE f_recep  DATE
    DEFINE vnom_mod CHAR(120)
    DEFINE vnom_bd  CHAR(120)
    DEFINE g_afi_mae_afil RECORD LIKE afi_mae_afiliado.*

    INITIALIZE reg_mod.* TO NULL
    INITIALIZE g_afi_mae_afil.* TO NULL

    SELECT *, rowid
      INTO reg_mod.*, vrow
      FROM afi_mae_modifica
     WHERE n_seguro       = vn_seguro
       AND cod_operacion  = 0
       AND diag_proceso   = 0
       AND status_interno = 130

    LET vnom_afore = NULL
    LET vnom_proce = NULL

    LET marca2 = 610

    SELECT *
      INTO g_afi_mae_afil.*
      FROM afi_mae_afiliado
     WHERE n_seguro   = vn_seguro

    INSERT INTO  afi_his_modifica
    VALUES(g_afi_mae_afil.tipo_solicitud,      --tipo_solicitud
           g_afi_mae_afil.n_folio,             --n_folio
           g_afi_mae_afil.fecha_elaboracion,   --fecha_elaboracion
           g_afi_mae_afil.folio_edo_cta,       --folio_edo_cuenta
           g_afi_mae_afil.cod_afore_ced,       --cod_afore_ced
           g_afi_mae_afil.femision,            --fecha_emision
           g_afi_mae_afil.frecafor,            --frecafor
           g_afi_mae_afil.paterno,             --paterno
           g_afi_mae_afil.materno,             --materno
           g_afi_mae_afil.nombres,             --nombres
           g_afi_mae_afil.n_seguro,            --n_seguro
           g_afi_mae_afil.n_rfc,               --n_rfc
           g_afi_mae_afil.n_unico,             --n_unico
           g_afi_mae_afil.sexo,                --sexo
           g_afi_mae_afil.edo_civil,           --edo_civil
           g_afi_mae_afil.fena,                --fena
           g_afi_mae_afil.salario_base_comis,  --salario_base_comis
           g_afi_mae_afil.estadon,             --estadon
           g_afi_mae_afil.nacionalidad,        --nacionalidad
           g_afi_mae_afil.tip_prob,            --tip_prob
           g_afi_mae_afil.fol_prob,            --fol_prob
           g_afi_mae_afil.doc_prob,            --doc_prob
           g_afi_mae_afil.ind_infonavit,       --ind_infonavit
           g_afi_mae_afil.cod_error_origen,    --cod_error_origen
           g_afi_mae_afil.const_curp,          --const_curp
           reg_mod.fecha_modifica,             --fecha_modifica de afi_mae_modifica
           reg_mod.usuario,                    --usuario        de afi_mae_modifica
           vcod_operacion,                     --cod_operacion  del archivo
           vdiag_proceso,                      --diag_proceso   del archivo
           g_afi_mae_afil.status_interno,      --status_interno
           g_afi_mae_afil.fecha_actualiza_sa,  --fecha_actualiza
           vnombre,                            --nombre_pcanase del archivo
           0,                                  --interfase
           0,                                  --folio_nvo
           0)                                  --folio_mod
 
END FUNCTION
---<  
  
FUNCTION despliega_totales()
#dt-------------------------
     IF NOT bnd_proceso THEN
        DISPLAY "                  DATOS A PROCESAR                 " AT 8,1 ATTRIBUTE ( REVERSE )
        DISPLAY "Total de Registros del lote : ",vtotal USING "#######&" AT 9,15 ATTRIBUTE ( BOLD )
        DISPLAY "Registros actualizados : ",vaprobados  USING "#######&" AT 10,15 ATTRIBUTE ( BOLD )
        DISPLAY "Registros rechazados   : ",vrechazados USING "#######&" AT 11,15 ATTRIBUTE ( BOLD )
        --DISPLAY "Registros pendientes   : ",vpendientes USING "#######&" AT 12,15 ATTRIBUTE ( BOLD )
    ELSE
        DISPLAY "                  DATOS A PROCESAR                 "
        DISPLAY "Total de Registros del lote  : ",vtotal USING "#######&"
        DISPLAY "Registros actualizados : ",vaprobados  USING "#######&"
        DISPLAY "Registros rechazados   : ",vrechazados USING "#######&"
        --DISPLAY "Registros pendientes   : ",vpendientes USING "#######&"
    END IF
END FUNCTION

FUNCTION lista_err()
#-------------------
    DEFINE hora          CHAR(8),
           vhora         CHAR(6)
    DEFINE HOY           DATE
    DEFINE vcod_rechazo  CHAR(8)
    DEFINE vcont         INTEGER

    DEFINE gr_curp RECORD
         n_seguro    LIKE afi_mae_afiliado.n_seguro,
         n_folio     LIKE afi_mae_afiliado.n_folio,
         tipo_solicitud        CHAR(1),
         paterno     CHAR(13),
         materno     CHAR(13),
         nombres     CHAR(16),
         des_rechazo CHAR(49),
         cod_rechazo LIKE afi_rechaza_proc.cod_rechazo
    END RECORD

    DEFINE G_LISTA     CHAR(200)
    DEFINE G_IMPRIME   CHAR(200)
    DEFINE resp        CHAR(1)

    LET hora = TIME
    LET HOY  = TODAY
    LET vhora   = HORA[1,2],HORA[4,5]

    LET G_LISTA = g_paramgrales.ruta_listados CLIPPED,"/",
                  g_usuario CLIPPED,".MOD_RECH_OP19.",
                  HOY USING "ddmmyy","_",vhora CLIPPED

    START REPORT listado TO G_LISTA

    DECLARE c_carga CURSOR FOR
        SELECT b.nss_solicitud,
               b.folio_solicitud,
               c.tipo_solicitud,
               b.paterno,
               b.materno,
               b.nombres,
               a.rdeta_desc_c,
               b.diag_proceso[1,3]
          FROM safre_af:tab_rdeta a,
               safre_tmp:det_procesar b,
               safre_af:afi_mae_afiliado c
         WHERE c.n_seguro = b.nss_solicitud
           AND b.diag_proceso[1,3] = a.rdeta_cod
           AND b.cod_operacion = "02"
           AND a.modulo_cod = 'afi'
        ORDER BY 3,1

    FOREACH c_carga INTO gr_curp.*
       OUTPUT TO REPORT listado(gr_curp.*)
    END FOREACH

    FINISH REPORT listado
    ERROR "LISTADO GENERADO" SLEEP 2

    LET G_LISTA = "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,"/",
                  g_usuario CLIPPED,".MOD_RECH_OP19.",
                  HOY USING "ddmmyy","_",vhora CLIPPED
    RUN G_LISTA

    LET G_IMPRIME = g_paramgrales.ruta_listados CLIPPED,"/",
                    g_usuario CLIPPED,".MOD_RECH_OP19.",
                    HOY USING "ddmmyy","_",vhora CLIPPED
    RUN G_IMPRIME
END FUNCTION

REPORT listado(rpt)
#l-----------------
    DEFINE rpt RECORD
        n_seguro    LIKE afi_mae_afiliado.n_seguro,
        n_folio     LIKE afi_mae_afiliado.n_folio,
        tipo_solicitud CHAR(1),
        paterno     CHAR(13),
        materno     CHAR(13),
        nombres     CHAR(16),
        des_rechazo CHAR(49),
        cod_rechazo LIKE afi_rechaza_proc.cod_rechazo
    END RECORD

    DEFINE
        l_estado     CHAR(16),
        aux_sexo     CHAR(10),
        razon_social CHAR(40),
        vcont      SMALLINT

    OUTPUT
        PAGE LENGTH 1  --90
        TOP  MARGIN 0
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 0

    FORMAT
    ON EVERY ROW
       PRINT COLUMN 1, rpt.*

END REPORT

FUNCTION despliega_archivos()
#da--------------------------
    DEFINE
        aux_pausa CHAR(1),
        HOY   DATE,
        SW_1   SMALLINT

    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY control-o
        WHENEVER ERROR STOP

    LET HOY = TODAY
    OPEN WINDOW window_1 AT 2,2 WITH FORM "AFIM0231" ATTRIBUTE(BORDER)
    DISPLAY " AFIM030                      CONSULTA REGISTROS PROCESADOS                    " AT 3,1
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
        nombre_archivo LIKE afi_ctr_arh_proc.nombre_archivo,
        total_reg      LIKE afi_ctr_arh_proc.total_reg,
        total_aprob    LIKE afi_ctr_arh_proc.total_aprob,
        total_rech     LIKE afi_ctr_arh_proc.total_rech,
        total_dup      LIKE afi_ctr_arh_proc.total_pend,
        fecha_proceso  LIKE afi_ctr_arh_proc.fecha_proceso
    END RECORD

    DEFINE
        vcount    ,
        vtotal    ,
        vaprobados ,
        vrechazos  ,
        vduplicados,
        vtot_curp  INTEGER

    DEFINE
        pos    SMALLINT

    SELECT COUNT(*)
    INTO   vcount
    FROM   afi_ctr_arh_proc

    SELECT SUM(@total_reg)
    INTO   vtotal
    FROM   afi_ctr_arh_proc

    SELECT SUM(@total_aprob)
    INTO   vaprobados
    FROM   afi_ctr_arh_proc

    SELECT SUM(@total_rech)
    INTO   vrechazos
    FROM   afi_ctr_arh_proc

    SELECT SUM(@total_pend)
    INTO   vduplicados
    FROM   afi_ctr_arh_proc

    DISPLAY "                                                                                    " AT 1,1
    DISPLAY "  CTRL-C cancela                                                                    " AT 2,1
    DISPLAY " CONSULTA " AT 2,65

    DECLARE curp_12 CURSOR FOR
    SELECT @nombre_archivo, @total_reg, @total_aprob, @total_rech, @total_pend,
           @fecha_proceso
      FROM afi_ctr_arh_proc
    ORDER BY 6 DESC

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
        DISPLAY BY NAME vduplicados
        DISPLAY ARRAY ga_record TO scr_1.*

        ON KEY (INTERRUPT)
            CLEAR FORM
            EXIT DISPLAY

        END DISPLAY
    ELSE
        ERROR "ARCHIVO DE PROCESAR VACIO"
    END IF
END FUNCTION

#######################################################################
FUNCTION desmarca_cuenta(vnss,vmarca,vusuario,vn_folio)
#dc----------------------------------------------------
    DEFINE
        vnss          CHAR(11),
        vmarca        SMALLINT,
        vusuario      CHAR(8),
        vn_folio      DECIMAL(10,0),	#0806 CIL Fol-10
        pestado_marca SMALLINT,
        pmarca_causa  SMALLINT,
        vcorrelativo  INTEGER

    LET pestado_marca = 0
    LET pmarca_causa  = 0

    DECLARE cmarca CURSOR FOR
       SELECT d.correlativo
         FROM cta_act_marca d
        WHERE d.nss = vnss
          AND d.marca_cod = vmarca

    FOREACH cmarca INTO vcorrelativo

      PREPARE eje_desmarca FROM v_desmarca
      EXECUTE eje_desmarca USING vnss,
                                 vmarca,
                                 vcorrelativo,
                                 pestado_marca,
                                 pmarca_causa,
                                 vusuario
    END FOREACH
END FUNCTION

#######################################################################
FUNCTION marca_cuenta(vnss,vmarca_entra,vmarca_edo,vcodigo_rech,vusuario,
		      vn_folio)
#mc-------------------
    DEFINE
        vnss         CHAR(11),
        vmarca_entra SMALLINT,
        vmarca_edo   SMALLINT,
        vcodigo_rech SMALLINT,
        vusuario     CHAR(08),
        vn_folio     DECIMAL(10,0)	#0806 CIL Fol-10

    DEFINE
        pmarca_causa SMALLINT,
        pfecha_causa DATE

    LET vmarca_entra = 605
    LET vmarca_edo   = 0
    LET vcodigo_rech = 0
    LET pmarca_causa = 0
    LET pfecha_causa = ""

    PREPARE eje_marca FROM v_marca
    DECLARE cur_marca CURSOR FOR eje_marca
       OPEN cur_marca USING vnss        ,
                            vmarca_entra,
                            vn_folio    ,
                            vmarca_edo  ,
                            vcodigo_rech,
                            pmarca_causa,
                            pfecha_causa,
                            vusuario

    FETCH cur_marca INTO xcodigo_marca, xcodigo_rechazo
    CLOSE cur_marca
    FREE cur_marca
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


