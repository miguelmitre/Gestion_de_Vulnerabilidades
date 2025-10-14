#############################################################################
#Proyecto          => AFORES ( MEXICO )                                     #
#Propietario       => E.F.P.                                                #
#Programa AFIC004  => CARGA DE ARCHIVO GENERADO POR OPERADORA BDNSAR        #
#Sistema           => AFI.                                                  #
#Autor             => MAURO MUNIZ CABALLERO                                 #
#Fecha             => 18 DE ENERO DE2001                                    #
#Autor             => MAURO MUNIZ CABALLERO (Proceso batch)                 #
#Modifico          => FERNANDO HERRERA HERNANDEZ                            #
#Fecha             => 07 DE ENERO DE 2009                                   #
#                  => Adecuaciones segun MPT (CURP)                         #
#                  => Eduardo Joaquin Resnendiz Medina CIRCULAR UNICA       #
#Modifico          => Eduardo Joaquin Resnendiz Medina CPL-1406             #
#Modifico          => Eduardo Joaquin Resnendiz Medina CPL-1634             #
#                                                                           #
#############################################################################
# CPL-1878         - CUO II Op 13+ EJRM JGHM  Feb 2015                      #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg_mod       RECORD LIKE afi_mae_modifica.*
    DEFINE reg_ant       RECORD LIKE afi_mae_modifica.*
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
        carga               CHAR(100),
        vnombre             CHAR(50),
        corr                CHAR(100),
        v_emision	    CHAR(08),
        vfecha_emision	    CHAR(10),
        vn_folio          DECIMAL(10,0)

    DEFINE
        vhoy ,
        HOY  ,
        f_emision DATE

    DEFINE
        marca       ,
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
        opc             CHAR(1)  ,
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
        vnom_proce      CHAR(122),
        v_marca         CHAR(300),
        v_desmarca      CHAR(300)

    DEFINE
        vcve_afo_nss_a  CHAR(3),
        vid_tip_trab    CHAR(1),
        vdiag_proceso15 CHAR(15),
        vcurp_oficial   CHAR(18)

    DEFINE v_sql_1      CHAR(50)
    DEFINE v_sql_2      CHAR(50)
    DEFINE vn_ss        CHAR(11),
           vvn_folio  DECIMAL(10,0),
           vtipsol   SMALLINT
    DEFINE r_mot_exp RECORD LIKE afi_motivo_expediente.*
    DEFINE fechacert CHAR(10) 
    DEFINE vfechacert DATE
    DEFINE MOD CHAR(30)
    DEFINE vmod CHAR(15)
    DEFINE vnss_asoc CHAR(11)
    DEFINE vdesc_motivo CHAR(60)
    DEFINE G_LISTA_13         CHAR(200)    
    DEFINE G_LISTA_ERR        CHAR(200)
    DEFINE G_LISTA_EXP        CHAR(200)   
    DEFINE G_LISTA_ACE        CHAR(200)
    

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG(FGL_GETENV('USER')||'.AFIC004.log')
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

    LET HOY      = TODAY
    LET marca    = 600
    LET edo_proc = 605
    LET generar  = "S"

    LET vmarca_estado   = 0
    LET vcodigo_rechazo = 0

    LET v_sql_1 = "EXECUTE PROCEDURE fn_fnacimiento(?,?)"
    LET v_sql_2 = "EXECUTE PROCEDURE fn_regimen_inv(?,?,?,?,?,?)"

    LET v_marca = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? )"

    LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"

    SELECT *, USER
    INTO   g_paramgrales.* , g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    CREATE TEMP TABLE plano_procesar
        (n_registros CHAR(576))            --CUO13+

END FUNCTION

FUNCTION crea_tablas() 
#ct-------------------

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp

        DROP TABLE cza_procesar
        DROP TABLE det_procesar
        DROP TABLE det_dom_part
        DROP TABLE det_dom_lab
        DROP TABLE det_ref
        DROP TABLE det_benef
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
         --cadena_dif          CHAR(17),   --CUO13+
         folio_solicitud     DECIMAL(10,0),
         sexo                SMALLINT,
         entidad_nacimiento  CHAR(02),
         --ind_uni_aut          CHAR(01)     ,
         tipo_movimiento     CHAR(01),     --CUO13+
         nacionalidad        CHAR(03),
         tip_prob            CHAR(01),
         fol_prob            CHAR(10),
         doc_prob            CHAR(16),
         ocupacion           CHAR(02),     --CUO13
         actividad           CHAR(02),     --CUO13
         nivel_estudios      CHAR(02),     --CUO13
         --cve_afore_ced       CHAR(3),
         cve_afo_nss_asoc    CHAR(03),     --CUO13  (guardar)
         id_tip_trab         CHAR(1),      --CUO13  (guardar)
         cod_operacion       CHAR(02),
         diag_proceso        CHAR(15),
         ident_lote_origen   CHAR(16),
         nss_oficial         CHAR(11),     --CUO13  (guardar es nss asociado)
         ind_nss_modificado  CHAR(01),
         fec_emision_certif  CHAR(08),
         curp_oficial        CHAR(18),
         ind_curp_modif      CHAR(01),
         rfc_bd              CHAR(13),
         nombre_imss         CHAR(50),
         nombre_procanase    CHAR(50),
         fena_bd             CHAR(08),
         cve_agente_prom     CHAR(10),
         sexo_bd             CHAR(01),
        estadon_bd           CHAR(02),         --->circ unica
        fprimer_afil         CHAR(08),
        falta_actual         CHAR(08),
        cve_afore            CHAR(03),
        nacionalidad_bd      CHAR(03),
        tip_aprob_bd         CHAR(01),
        fol_aprob_bd         CHAR(10),
        doc_aprob_bd         CHAR(16),
        ind_infonavit_bd     CHAR(01),
        fecha_recep_sello    CHAR(08),
        hora_recep_sello     CHAR(02),
        minuto_recep_sello   CHAR(02),
        seg_recep_sello      CHAR(02),
        cseg_recep_sello     CHAR(02),
        consecutivo_recep    CHAR(08),
        periodo              CHAR(06),
        salario              DECIMAL(9,2) ,
        num_fol_edo_cta      CHAR(8),
        exp_completo         CHAR(6));            --CUO13  (guardar)

    CREATE TABLE det_dom_part       #04
        (tipo_registro       CHAR(02),
         nss_solicitud       CHAR(11),
         curp_solicitud      CHAR(18),
         calle               CHAR(65),
         num_ext             CHAR(15),
         depto               CHAR(15),
         colonia             CHAR(65),
         delega              CHAR(65),
         cp                  CHAR(05),
         estado              CHAR(65),
         pais                CHAR(03),
         tipo_tel1           CHAR(03),
         telefono1           CHAR(10),
         ext1                CHAR(05),
         tipo_tel2           CHAR(03),
         telefono2           CHAR(10),
         ext2                CHAR(05),
         correoe             CHAR(50),
         f_actualiza         CHAR(08),
         hora_act            CHAR(08),
         cod_op              CHAR(02),
         diag_proc           CHAR(15));   
         
    CREATE TABLE det_dom_lab         #05
        (tipo_registro       CHAR(02),
         nss_solicitud       CHAR(11),
         curp_solicitud      CHAR(18),
         calle               CHAR(65),
         num_ext             CHAR(15),
         depto               CHAR(15),
         colonia             CHAR(65),
         delega              CHAR(65),
         cp                  CHAR(05),
         estado              CHAR(65),
         pais                CHAR(03),
         cod_op              CHAR(02),
         diag_proc           CHAR(15)); 

    CREATE TABLE det_ref            #06
        (tipo_registro       CHAR(02),
         nss_solicitud       CHAR(11),
         curp_solicitud      CHAR(18),
         pat1                CHAR(40),
         mat1                CHAR(40),
         nom1                CHAR(40),
         curp1               CHAR(18),
         telefono1           CHAR(10),
         paren1              CHAR(02),
         pat2                CHAR(40),
         mat2                CHAR(40),
         nom2                CHAR(40),
         curp2               CHAR(18),
         telefono2           CHAR(18),
         paren2              CHAR(02),
         cod_op              CHAR(02),
         diag_proc           CHAR(15)); 


    CREATE TABLE det_benef          #07
        (tipo_registro       CHAR(02),
         nss_solicitud       CHAR(11),
         curp_solicitud      CHAR(18),
         patb                CHAR(40),
         matb                CHAR(40),
         nomb                CHAR(40),
         curpb               CHAR(18),
         parenb              CHAR(02),
         porcentaje          CHAR(03),
         cod_op              CHAR(02),
         diag_proc           CHAR(15)); 
{
    CREATE TABLE afi_ctr_det_op13
    (nss       CHAR(11),
     n_folio        DECIMAL(10,0),
     tipo_solicitud SMALLINT,
     curp           CHAR(18),
     cod_op_det03   CHAR(02),
     dia_proc03     CHAR(15),
     cod_op_det04   CHAR(02),
     dia_proc04     CHAR(15),
     cod_op_det05   CHAR(02),
     dia_proc05     CHAR(15),
     cod_op_det06   CHAR(02),
     dia_proc06     CHAR(15),
     cod_op_det07   CHAR(02),
     dia_proc07     CHAR(15),
     tipo           SMALLINT,
     motivo         SMALLINT,
     tipo_id        CHAR(1) ,
     modulo         CHAR(15),
     fecha_cert     DATE,
     id_lote        CHAR(16),
     usuario        CHAR(8),
     factualiza     DATE);
}
    CREATE TABLE sum_procesar
        (campo1    CHAR(2),
         campo2    CHAR(2),
         campo3    CHAR(3),
         campo4    CHAR(8),
         campo5    CHAR(3),
         campo6    CHAR(2),
         campo7    CHAR(2),
         campo8    CHAR(9),
         campo9    CHAR(9),
         campo10   CHAR(9),     --CUO13
         campo11   CHAR(9),     --CUO13
         campo12   CHAR(9),     --CUO13
         campo13   CHAR(9),     --CUO13
         campo14   CHAR(9),     --CUO13
         campo15   CHAR(9),     --CUO13
         campo16   CHAR(9),     --CUO13
         campo17   CHAR(9))     --CUO13

    CREATE INDEX det_procesar_1 ON det_procesar(nss_solicitud)

    DATABASE safre_af

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIC0041" ATTRIBUTE(BORDER)
    DISPLAY " AFIC004      CARGA DE ARCHIVO RESPUESTA MODIFICACIONES                            " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           < Ctrl-C > Salir                                        " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

    DISPLAY g_paramgrales.ruta_rescate AT 6,10

    INPUT BY NAME generar

    AFTER FIELD generar
        IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        SELECT @nombre_archivo
        INTO   varchivo
        FROM   afi_ctr_arh_proc
        WHERE  @nombre_archivo = generar

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
            INSERT INTO plano_procesar
        WHENEVER ERROR STOP

        SELECT COUNT(*) 
        INTO   g_plano_procesar
        FROM   plano_procesar

        IF g_plano_procesar IS NULL OR
           g_plano_procesar = 0 THEN
            ERROR "NOMBRE DE ARCHIVO INCORRECTO O VACIO, VERIFIQUE " 
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
    CALL actualiza_dif_maeafili()
    CALL Actualiza_Maeafili() #am
    CALL lista_err()          #le
    CALL lista_err_exp()
    CALL lista_acep_exp()     
    CALL lista_op13()

END FUNCTION

FUNCTION actualiza_datos()
#-------------------------

    DEFINE 
        cont_reg      INTEGER
 
    DEFINE 
        carga_reg     CHAR(576)                 --CUO13+

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
        campo_120     CHAR(03),
        campo_121     CHAR(02),
        campo_122     CHAR(03),

#detalle 03 DATOS GRALES
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
        --campo_11      CHAR(17),  --CUO13+ 
        campo_13      CHAR(10),
        campo_14      CHAR(01),
        campo_15      CHAR(02),
        campo_16      CHAR(01),
        campo_17      CHAR(03),
        campo_18      CHAR(01),
        campo_19      CHAR(10),
        campo_20      CHAR(16),
        campo_21      CHAR(02),    --CUO13+  ocupacion
        campo_221     CHAR(02),    --CUO13+ 
        campo_222     CHAR(02),    --CUO13+ 

        campo_22      CHAR(03),
        campo_23      CHAR(1),
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

        campo_39  CHAR(02),             --->CIRC UNICA
        campo_40  CHAR(08),
        campo_41  CHAR(08),
        campo_42  CHAR(03),
        campo_43  CHAR(03),
        campo_44  CHAR(01),
        campo_45  CHAR(10),
        campo_46  CHAR(16),
        campo_47  CHAR(01),
        campo_48  CHAR(08),
        campo_49  CHAR(02),
        campo_491 CHAR(02),
        campo_492 CHAR(02),
        campo_493 CHAR(02),
        campo_50  CHAR(08),
        campo_51  CHAR(06),

        campo_52  CHAR(08),
        campo_53  CHAR(08),           ---<CIRC UNICA
        campo_54  CHAR(06),    --CUO13+ expediente completo

#detalle 04
        i4_tipo_registro       CHAR(02),
        i4_nss_solicitud       CHAR(11),
        i4_curp_solicitud      CHAR(18),
        i4_calle               CHAR(65),
        i4_num_ext             CHAR(15),
        i4_depto               CHAR(15),
        i4_colonia             CHAR(65),
        i4_delega              CHAR(65),
        i4_cp                  CHAR(05),
        i4_estado              CHAR(65),
        i4_pais                CHAR(03),
        i4_tipo_tel1           CHAR(03),
        i4_telefono1           CHAR(10),
        i4_ext1                CHAR(05),
        i4_tipo_tel2           CHAR(03),
        i4_telefono2           CHAR(10),
        i4_ext2                CHAR(05),
        i4_correoe             CHAR(50),
        i4_f_actualiza         CHAR(08),
        i4_hora_act            CHAR(08),
        i4_cod_op              CHAR(02),
        i4_diag_proc           CHAR(15),
         
#detalle 05
        i5_tipo_registro       CHAR(02),
        i5_nss_solicitud       CHAR(11),
        i5_curp_solicitud      CHAR(18),
        i5_calle               CHAR(65),
        i5_num_ext             CHAR(15),
        i5_depto               CHAR(15),
        i5_colonia             CHAR(65),
        i5_delega              CHAR(65),
        i5_cp                  CHAR(05),
        i5_estado              CHAR(65),
        i5_pais                CHAR(03),
        i5_cod_op              CHAR(02),
        i5_diag_proc           CHAR(15),


#detalle 06
        i6_tipo_registro       CHAR(02),
        i6_nss_solicitud       CHAR(11),
        i6_curp_solicitud      CHAR(18),
        i6_pat1                CHAR(40),
        i6_mat1                CHAR(40),
        i6_nom1                CHAR(40),
        i6_curp1               CHAR(18),
        i6_telefono1           CHAR(10),
        i6_paren1              CHAR(02),
        i6_pat2                CHAR(40),
        i6_mat2                CHAR(40),
        i6_nom2                CHAR(40),
        i6_curp2               CHAR(18),
        i6_telefono2           CHAR(18),
        i6_paren2              CHAR(02),
        i6_cod_op              CHAR(02),
        i6_diag_proc           CHAR(15),

#detalle 07
        i7_tipo_registro       CHAR(02),
        i7_nss_solicitud       CHAR(11),
        i7_curp_solicitud      CHAR(18),
        i7_patb                CHAR(40),
        i7_matb                CHAR(40),
        i7_nomb                CHAR(40),
        i7_curpb               CHAR(18),
        i7_parenb              CHAR(02),
        i7_porcentaje          CHAR(03),
        i7_cod_op              CHAR(02),
        i7_diag_proc           CHAR(15),

#sumario
        campo_201     CHAR(02),
        campo_202     CHAR(02),
        campo_203     CHAR(03),
        campo_204     CHAR(08),
        campo_205     CHAR(03),
        campo_206     CHAR(02),
        campo_207     CHAR(02),
        campo_208     CHAR(09),
        campo_209     CHAR(09),
        campo_210     CHAR(09),        --CUO13
        campo_211     CHAR(09),        --CUO13
        campo_212     CHAR(09),        --CUO13
        campo_213     CHAR(09),        --CUO13
        campo_214     CHAR(09),        --CUO13
        campo_215     CHAR(09),        --CUO13
        campo_216     CHAR(09),        --CUO13
        campo_217     CHAR(09)         --CUO13

    LET cont_reg = 0

    SELECT COUNT(*)
    INTO   total_reg
    FROM   plano_procesar

    DECLARE cursor_1 CURSOR FOR 
    SELECT  * 
    FROM    plano_procesar

    FOREACH cursor_1 INTO carga_reg
        LET cont_reg = cont_reg + 1
        CASE carga_reg[1,2]
          WHEN "01"       #---ENCABEZADO ---#

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
            LET campo_120 = carga_reg[085,087]
            LET campo_121 = carga_reg[088,089]
            LET campo_122 = carga_reg[090,092]

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
                    campo_120,
                    campo_121,
                    campo_122)

          WHEN "03"       #---DETALLE DATOS GRALES---#
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
            --LET campo_11 = carga_reg[185,201]
            LET campo_13 = carga_reg[203,212]
            LET campo_14 = carga_reg[213,213]
            LET campo_15 = carga_reg[214,215]
            LET campo_16 = carga_reg[216,216]
            LET campo_17 = carga_reg[217,219]
            LET campo_18 = carga_reg[220,220]
            LET campo_19 = carga_reg[221,230]
            LET campo_20 = carga_reg[231,246]
            LET campo_21 = carga_reg[247,248]   --CUO13
            LET campo_221= carga_reg[249,250]   --CUO13
            LET campo_222= carga_reg[251,252]
            LET campo_22 = carga_reg[257,259]
            LET campo_23 = carga_reg[260,260]
            LET campo_25 = carga_reg[268,269]
            LET campo_26 = carga_reg[270,284]
            LET campo_27 = carga_reg[285,300]
            LET campo_28 = carga_reg[301,311]
            LET campo_29 = carga_reg[312,312]
            LET campo_30 = carga_reg[313,320]
            LET campo_31 = carga_reg[321,338]
            LET campo_32 = carga_reg[339,339]
            LET campo_33 = carga_reg[340,352]
            LET campo_34 = carga_reg[353,402]
            LET campo_35 = carga_reg[403,452]
            LET campo_36 = carga_reg[453,460]
            LET campo_37 = carga_reg[461,470]
            LET campo_38 = carga_reg[471,471]

            LET campo_39 = carga_reg[472,473]                --->CIRC UNICA
            LET campo_40 = carga_reg[471,481]
            LET campo_41 = carga_reg[482,489]
            LET campo_42 = carga_reg[490,492]
            LET campo_43 = carga_reg[493,495]
            LET campo_44 = carga_reg[496,496]
            LET campo_45 = carga_reg[497,506]
            LET campo_46 = carga_reg[507,522]
            LET campo_47 = carga_reg[523,523]

            LET campo_48 = carga_reg[524,531]
            LET campo_49 = carga_reg[532,533]
            LET campo_491= carga_reg[534,535]
            LET campo_492= carga_reg[536,537]
            LET campo_493= carga_reg[538,539]
            LET campo_50 = carga_reg[540,547]
            LET campo_51 = carga_reg[548,553]
            LET campo_52 = carga_reg[554,560]
            --LET campo_512= carga_reg[
            LET campo_53 = carga_reg[561,568]                 --<CIRC UNICA
            LET campo_54 = carga_reg[569,570]       --CUO13  


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
                    --campo_11,  --CUO13
                    campo_13,
                    campo_14,
                    campo_15,
                    campo_16,
                    campo_17,
                    campo_18,
                    campo_19,
                    campo_20,
                    campo_21  ,
                    campo_221 ,
                    campo_222 ,
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
                    campo_38,
                    campo_39 ,       --->CIRC UNICA
                    campo_40 ,
                    campo_41 ,
                    campo_42 ,
                    campo_43 ,
                    campo_44 ,
                    campo_45 ,
                    campo_46 ,
                    campo_47 ,
                    campo_48 ,
                    campo_49 ,
                    campo_491,
                    campo_492,
                    campo_493,
                    campo_50 ,
                    campo_51 ,
                    campo_52 ,
                    campo_53 ,
                    campo_54)        ---<CIRC UNICA

          LET fechacert = campo_30[5,6],"/",campo_30[7,8],"/",campo_30[1,4]
          LET vfechacert = fechacert

          INITIALIZE vn_ss      TO NULL
          LET vvn_folio         = 0
          let vtipsol           = 0 

          IF campo_04 = '           ' THEN
              SELECT n_seguro,n_folio,tipo_solicitud
              INTO   vn_ss,vvn_folio,vtipsol
              FROM   afi_mae_afiliado
              WHERE    n_unico  = campo_05
              AND NOT  EXISTS   (SELECT nss
                                 FROM cta_act_marca
                                 WHERE nss       = n_seguro 
                                 AND   marca_cod IN(120,130,150)) #marcas de inhabilitacion
          ELSE 
              SELECT n_seguro,n_folio,tipo_solicitud
              INTO   vn_ss,vvn_folio,vtipsol
              FROM   afi_mae_afiliado
              WHERE  n_seguro = campo_04
              AND NOT  EXISTS   (SELECT nss
                                 FROM cta_act_marca
                                 WHERE nss       = n_seguro 
                                 AND   marca_cod IN(120,130,150)) #marcas de inhabilitacion
          END IF
            

          SELECT *
          INTO  r_mot_exp.*
          FROM  afi_motivo_expediente
          WHERE nss = vn_ss
          AND   n_folio = vvn_folio
          AND   tipo_solicitud = vtipsol

          IF SQLCA.SQLCODE = NOTFOUND THEN
            LET  r_mot_exp.tipo    = NULL
            LET  r_mot_exp.motivo  = NULL
            LET  r_mot_exp.tipo_id = NULL
            LET  vmod = "MODIFICACION"
          END IF

          INSERT INTO afi_ctr_det_op13
          VALUES(vn_ss,
                 vvn_folio,
                 vtipsol,
                 campo_05,  --curp
                 campo_25,
                 campo_26,
                 '',     --i4_cod_op,   
                 '',     --i4_diag_proc,
                 '',     --i5_cod_op   ,
                 '',     --i5_diag_proc,
                 '',     --i6_cod_op   ,
                 '',     --i6_diag_proc,
                 '',     --i7_cod_op   ,
                 '',     --i7_diag_proc,
                 r_mot_exp.tipo,
                 r_mot_exp.motivo,
                 r_mot_exp.tipo_id,
                 vmod,
                 vfechacert,
                 campo_27,      --id_lote
                 campo_54,      --ind_exp_comp
                 g_usuario,
                 HOY)

          WHEN "04"       #---DETALLE DE DOMICILIO PATRICULAR---# 
            LET i4_tipo_registro  = carga_reg[001,002]
            LET i4_nss_solicitud  = carga_reg[003,013]
            LET i4_curp_solicitud = carga_reg[014,031]
            LET i4_calle          = carga_reg[032,096]
            LET i4_num_ext        = carga_reg[097,111]
            LET i4_depto          = carga_reg[112,126]
            LET i4_colonia        = carga_reg[127,191]
            LET i4_delega         = carga_reg[192,256]
            LET i4_cp             = carga_reg[257,261]
            LET i4_estado         = carga_reg[262,326]
            LET i4_pais           = carga_reg[327,329]
            LET i4_tipo_tel1      = carga_reg[330,332]
            LET i4_telefono1      = carga_reg[333,342]
            LET i4_ext1           = carga_reg[343,347]
            LET i4_tipo_tel2      = carga_reg[348,350]
            LET i4_telefono2      = carga_reg[351,360]
            LET i4_ext2           = carga_reg[361,365]
            LET i4_correoe        = carga_reg[366,415]
            LET i4_f_actualiza    = carga_reg[416,423]
            LET i4_hora_act       = carga_reg[424,431]
            LET i4_cod_op         = carga_reg[432,433]
            LET i4_diag_proc      = carga_reg[434,448]

            UPDATE afi_ctr_det_op13 
            SET    cod_op_det04   = i4_cod_op,
                   dia_proc04     = i4_diag_proc
            WHERE  nss            = vn_ss
            AND    n_folio        = vvn_folio
            AND    tipo_solicitud = vtipsol
            AND    fecha_cert     = vfechacert
            AND    id_lote        = campo_27
{
           INSERT INTO safre_tmp:det_dom_part
           VALUES (i4_tipo_registro ,
                   i4_nss_solicitud ,
                   i4_curp_solicitud,
                   i4_calle         ,
                   i4_num_ext       ,
                   i4_depto         ,
                   i4_colonia       ,
                   i4_delega        ,
                   i4_cp            ,
                   i4_estado        ,
                   i4_pais          ,
                   i4_tipo_tel1     ,
                   i4_telefono1     ,
                   i4_ext1          ,
                   i4_tipo_tel2     ,
                   i4_telefono2     ,
                   i4_ext2          ,
                   i4_correoe       ,
                   i4_f_actualiza   ,
                   i4_hora_act      ,
                   i4_cod_op        ,
                   i4_diag_proc     )
   }

          WHEN "05"       #---DETALLE DE DOMICILIO LABORAL---# 
                LET i5_tipo_registro  = carga_reg[001,002]  
                LET i5_nss_solicitud  = carga_reg[003,013]
                LET i5_curp_solicitud = carga_reg[014,031]
                LET i5_calle          = carga_reg[032,096]
                LET i5_num_ext        = carga_reg[097,111]
                LET i5_depto          = carga_reg[112,126]
                LET i5_colonia        = carga_reg[127,191]
                LET i5_delega         = carga_reg[192,256]
                LET i5_cp             = carga_reg[257,261]
                LET i5_estado         = carga_reg[262,326]
                LET i5_pais           = carga_reg[327,329]
                LET i5_cod_op         = carga_reg[330,331]
                LET i5_diag_proc      = carga_reg[332,346]

            UPDATE afi_ctr_det_op13 
            SET    cod_op_det05   = i5_cod_op,
                   dia_proc05     = i5_diag_proc
            WHERE  nss            = vn_ss
            AND    n_folio        = vvn_folio
            AND    tipo_solicitud = vtipsol
            AND    fecha_cert     = vfechacert
            AND    id_lote        = campo_27
{
            INSERT INTO safre_tmp:det_dom_lab
            VALUES(i5_tipo_registro  ,   
                   i5_nss_solicitud  ,
                   i5_curp_solicitud ,
                   i5_calle          ,
                   i5_num_ext        ,
                   i5_depto          ,
                   i5_colonia        ,
                   i5_delega         ,
                   i5_cp             ,
                   i5_estado         ,
                   i5_pais           ,
                   i5_cod_op         ,
                   i5_diag_proc      )
}
          WHEN "06"       #---DETALLE DE REFERENCIAS---# 
                LET i6_tipo_registro  = carga_reg[001,002]
                LET i6_nss_solicitud  = carga_reg[003,013]
                LET i6_curp_solicitud = carga_reg[014,031]
                LET i6_pat1           = carga_reg[032,071]
                LET i6_mat1           = carga_reg[072,111]
                LET i6_nom1           = carga_reg[112,151]
                LET i6_curp1          = carga_reg[152,169]
                LET i6_telefono1      = carga_reg[170,179]
                LET i6_paren1         = carga_reg[180,181]
                LET i6_pat2           = carga_reg[182,221]
                LET i6_mat2           = carga_reg[222,261]
                LET i6_nom2           = carga_reg[262,301]
                LET i6_curp2          = carga_reg[302,319]
                LET i6_telefono2      = carga_reg[320,329]
                LET i6_paren2         = carga_reg[330,331]
                LET i6_cod_op         = carga_reg[332,333]
                LET i6_diag_proc      = carga_reg[334,348]

            UPDATE afi_ctr_det_op13 
            SET    cod_op_det06   = i6_cod_op,
                   dia_proc06     = i6_diag_proc
            WHERE  nss            = vn_ss
            AND    n_folio        = vvn_folio
            AND    tipo_solicitud = vtipsol
            AND    fecha_cert     = vfechacert
            AND    id_lote        = campo_27
{
            UPDATE afi_ctr_expediente 
            SET    cod_operacion  = i6_cod_op,
                   diag_proceso    = i6_diag_proc
            WHERE  nss            = vn_ss
            AND    n_folio        = vvn_folio
            AND    tipo_solicitud = vtipsol
            AND    cod_operacion  IS NULL
}
            INSERT INTO safre_tmp:det_ref
            VALUES(i6_tipo_registro  ,
                   i6_nss_solicitud  ,
                   i6_curp_solicitud ,
                   i6_pat1           ,
                   i6_mat1           ,
                   i6_nom1           ,
                   i6_curp1          ,
                   i6_telefono1      ,
                   i6_paren1         ,
                   i6_pat2           ,
                   i6_mat2           ,
                   i6_nom2           ,
                   i6_curp2          ,
                   i6_telefono2      ,
                   i6_paren2         ,
                   i6_cod_op         ,
                   i6_diag_proc      )

          
          WHEN "07"       #---DETALLE DE BENEFICIARIOS---# 
                LET i7_tipo_registro  = carga_reg[001,002]
                LET i7_nss_solicitud  = carga_reg[003,013]
                LET i7_curp_solicitud = carga_reg[014,031]
                LET i7_patb           = carga_reg[032,071]
                LET i7_matb           = carga_reg[072,111]
                LET i7_nomb           = carga_reg[112,151]
                LET i7_curpb          = carga_reg[152,169]
                LET i7_parenb         = carga_reg[170,171]
                LET i7_porcentaje     = carga_reg[172,174]
                LET i7_cod_op         = carga_reg[175,176]
                LET i7_diag_proc      = carga_reg[177,191]

            UPDATE afi_ctr_det_op13 
            SET    cod_op_det07   = i7_cod_op,
                   dia_proc07     = i7_diag_proc
            WHERE  nss            = vn_ss
            AND    n_folio        = vn_folio
            AND    tipo_solicitud = vtipsol
            AND    fecha_cert     = vfechacert
            AND    id_lote        = campo_27
{
            INSERT INTO safre_tmp:det_benef
            VALUES(i7_tipo_registro  ,
                   i7_nss_solicitud  ,
                   i7_curp_solicitud ,
                   i7_patb           ,
                   i7_matb           ,
                   i7_nomb           ,
                   i7_curpb          ,
                   i7_parenb         ,
                   i7_porcentaje     ,
                   i7_cod_op         ,
                   i7_diag_proc      )
}
          WHEN "09"       #---DETALLE DE DOMICILIO PATRICULAR---# 
            --IF cont_reg = total_reg THEN
                LET campo_201 = carga_reg[001,002]
                LET campo_202 = carga_reg[003,004]
                LET campo_203 = carga_reg[005,007]
                LET campo_204 = carga_reg[008,015]
                LET campo_205 = carga_reg[016,018]
                LET campo_206 = carga_reg[019,020]
                LET campo_207 = carga_reg[021,022]
                LET campo_208 = carga_reg[023,031]
                LET campo_209 = carga_reg[032,040]
                LET campo_210 = carga_reg[041,049]
                LET campo_211 = carga_reg[050,058]
                LET campo_212 = carga_reg[059,067]
                LET campo_213 = carga_reg[068,076]
                LET campo_214 = carga_reg[077,085]
                LET campo_215 = carga_reg[086,094]
                LET campo_216 = carga_reg[095,103] 
                LET campo_217 = carga_reg[104,112] 
          
                INSERT INTO safre_tmp:sum_procesar
                VALUES (campo_201,
                        campo_202,
                        campo_203,
                        campo_204,
                        campo_205,
                        campo_206,
                        campo_207,
                        campo_208,
                        campo_209,
                        campo_210,
                        campo_211,
                        campo_212,
                        campo_213,
                        campo_214,
                        campo_215,
                        campo_216,
                        campo_217 )

        END CASE   

    END FOREACH

END FUNCTION

FUNCTION revisa_datos()
#----------------------

    DEFINE
        rechazo_lote CHAR(3),
        rechazo_deta CHAR(3),
        l_reg        RECORD LIKE tab_rch_lote.* ,
        x_reg        RECORD LIKE tab_rdeta.* ,
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
        --cadena_dif          CHAR(17),   --CUO13
        folio_solicitud     DECIMAL(10,0),
        sexo                SMALLINT,
        entidad_nacimiento  CHAR(02),
        --ind_uni_aut          CHAR(01)     ,
        tipo_movimiento     CHAR(01),     --CUO13+
        nacionalidad        CHAR(03),
        tip_prob            CHAR(01),
        fol_prob            CHAR(10),
        doc_prob            CHAR(16),
        ocupacion           CHAR(02),     --CUO13
        actividad           CHAR(02),     --CUO13
        nivel_estudios      CHAR(02),     --CUO13
        --cve_afore_ced       CHAR(3),
        cve_afo_nss_asoc    CHAR(03),     --CUO13  (guardar)
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
        sexo_bd             CHAR(01),
        estadon_bd           CHAR(02),         --->circ unica
        fprimer_afil         CHAR(08),
        falta_actual         CHAR(08),
        cve_afore            CHAR(03),
        nacionalidad_bd      CHAR(03),
        tip_aprob_bd         CHAR(01),
        fol_aprob_bd         CHAR(10),
        doc_aprob_bd         CHAR(16),
        ind_infonavit_bd     CHAR(01),
        fecha_recep_sello    CHAR(08),
        hora_recep_sello     CHAR(02),
        minuto_recep_sello   CHAR(02),
        seg_recep_sello      CHAR(02),
        cseg_recep_sello     CHAR(02),
        consecutivo_recep    CHAR(08),
        periodo              CHAR(06),
        salario              DECIMAL(9,2) ,
        num_fol_edo_cta      CHAR(8),
        exp_completo         CHAR(6)            --CUO13  (guardar)
    END RECORD

    DEFINE reg_det_dom_part   RECORD      #04
         tipo_registro       CHAR(02),
         nss_solicitud       CHAR(11),
         curp_solicitud      CHAR(18),
         calle               CHAR(65),
         num_ext             CHAR(15),
         depto               CHAR(15),
         colonia             CHAR(65),
         delega              CHAR(65),
         cp                  CHAR(05),
         estado              CHAR(65),
         pais                CHAR(03),
         tipo_tel1           CHAR(03),
         telefono1           CHAR(10),
         ext1                CHAR(05),
         tipo_tel2           CHAR(03),
         telefono2           CHAR(10),
         ext2                CHAR(05),
         correoe             CHAR(50),
         f_actualiza         CHAR(08),
         hora_act            CHAR(08),
         cod_op              CHAR(02),
         diag_proc           CHAR(15)    
    END RECORD

    DEFINE reg_det_dom_lab RECORD        #05
         tipo_registro       CHAR(02),
         nss_solicitud       CHAR(11),
         curp_solicitud      CHAR(18),
         calle               CHAR(65),
         num_ext             CHAR(15),
         depto               CHAR(15),
         colonia             CHAR(65),
         delega              CHAR(65),
         cp                  CHAR(05),
         estado              CHAR(65),
         pais                CHAR(03),
         cod_op              CHAR(02),
         diag_proc           CHAR(15)  
    END RECORD

    DEFINE reg_det_ref   RECORD   #06
         tipo_registro       CHAR(02),
         nss_solicitud       CHAR(11),
         curp_solicitud      CHAR(18),
         pat1                CHAR(40),
         mat1                CHAR(40),
         nom1                CHAR(40),
         curp1               CHAR(18),
         telefono1           CHAR(10),
         paren1              CHAR(02),
         pat2                CHAR(40),
         mat2                CHAR(40),
         nom2                CHAR(40),
         curp2               CHAR(18),
         telefono2           CHAR(18),
         paren2              CHAR(02),
         cod_op              CHAR(02),
         diag_proc           CHAR(15)  
    END RECORD

    DEFINE reg_det_benef  RECORD  #07
         tipo_registro       CHAR(02),
         nss_solicitud       CHAR(11),
         curp_solicitud      CHAR(18),
         patb                CHAR(40),
         matb                CHAR(40),
         nomb                CHAR(40),
         curpb               CHAR(18),
         parenb              CHAR(02),
         porcentaje          CHAR(03),
         cod_op              CHAR(02),
         diag_proc           CHAR(15) 
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
        uno          CHAR(03),
        dos          CHAR(03),
        tre          CHAR(03),
        cua          CHAR(03),
        cin          CHAR(03),
        l_status_int SMALLINT

    DEFINE 
        x_fecha  CHAR(10),
        xx_fecha DATE

    LET contar_det = 1

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
    FROM safre_tmp:cza_procesar

    SELECT *
    INTO   l_reg.*
    FROM   tab_rch_lote
    WHERE  rlote_cod = rechazo_lote

    IF STATUS  <> NOTFOUND THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, ERROR DE PROCESO, NO PUEDE CONTINUAR "
            CALL borra_detalles()
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY l_reg.rlote_cod AT 10,1
            DISPLAY l_reg.rlote_desc_c AT 11,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR " FOR aux_pausa
            CALL borra_detalles()
            EXIT PROGRAM
        END IF
    END IF

    IF rechazo_001 <> "01" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Tipo de registro debe ser 01 en encabezado"
            CALL borra_detalles()
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Tipo de registro debe ser 01 en ENCABEZADO" AT 10,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR " FOR aux_pausa
            CALL borra_detalles()
            EXIT PROGRAM
        END IF
    END IF

    IF rechazo_002 <> "01" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Identificador de servicio debe ser 01 en encabezado"
            CALL borra_detalles()
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Identificador de Servicio debe ser 01 en ENCABEZADO" AT 10,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR " FOR aux_pausa
            CALL borra_detalles()
            EXIT PROGRAM
        END IF
    END IF

    IF rechazo_003 <> "13" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Identificador de operacion debe ser 13 en encabezado"
            CALL borra_detalles()
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Identificador de Operacion debe ser 13 en ENCABEZADO" AT 10,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR " FOR aux_pausa
            CALL borra_detalles()
            EXIT PROGRAM
        END IF
    END IF

    IF rechazo_019 <> "03" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Codigo empresa operadora debe ser 03 en encabezado"
            CALL borra_detalles()
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Codigo de Empresa Operadora debe ser 03 en ENCABEZADO" AT 10,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR " FOR aux_pausa
            CALL borra_detalles()
            EXIT PROGRAM
        END IF
    END IF
    
    IF rechazo_020 <> "001" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Clave entidad origen debe ser 001 en encabezado"
            CALL borra_detalles()
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Clave Entidad Origen debe ser 001 en ENCABEZADO" AT 10,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR " FOR aux_pausa
            CALL borra_detalles()
            EXIT PROGRAM
        END IF
    END IF

     # SUMARIO #

    SELECT campo1 
    INTO   rechazo_09 
    FROM   safre_tmp:sum_procesar

    IF rechazo_09 <> "09" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Tipo registro debe ser 09 en resumen"
            CALL borra_detalles()
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Tipo de registro debe ser 09 en RESUMEN" AT 10,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR " FOR aux_pausa
            CALL borra_detalles()
            EXIT PROGRAM
        END IF
    END IF

END FUNCTION

FUNCTION Actualiza_Maeafili()
#----------------------------
    DEFINE vcod_opera char(02)
    DEFINE vdiag_proc char(15)
    DEFINE vvtipsol SMALLINT

    OPEN WINDOW w_curp AT 8,4 WITH FORM "AFIC0082"
    ATTRIBUTE (BORDER)

    LET vhoy = TODAY
    LET MOD  = "MODIFICACION DE DATOS "
    LET vrechazados = 0
    LET vaprobados  = 0


    SELECT COUNT(*)
    INTO   vtotal
    FROM   safre_tmp:det_procesar a, afi_mae_modifica b
    WHERE  a.nss_solicitud  = b.n_seguro
    AND    b.status_interno = 130
    AND    b.cod_operacion  = 0
    AND    b.diag_proceso   = '0'
    
    DECLARE c_curp CURSOR FOR
    SELECT a.nss_solicitud,
           a.diag_proceso[1,3],
           a.cod_operacion,
           b.n_unico,
           b.n_folio,
           a.nombre_imss,
           a.fec_emision_certif,
           a.cve_afo_nss_asoc,
           a.nss_oficial,
           a.id_tip_trab,
           a.diag_proceso,
           a.curp_oficial,
           b.tipo_solicitud
    FROM   safre_tmp:det_procesar a, afi_mae_modifica b
    WHERE  a.nss_solicitud  = b.n_seguro
    AND    b.status_interno = 130
    AND    b.cod_operacion  = 0
    AND    b.diag_proceso   = '0'
    ORDER BY a.nss_solicitud

    FOREACH c_curp INTO vn_seguro,
                        vdiag_proceso,
                        vcod_operacion,
                        vcurp,
                        vn_folio,
                        vnombre,
                        v_emision,
                        vcve_afo_nss_a,
                        vnss_asoc,
                        vid_tip_trab,
                        vdiag_proceso15,
                        vcurp_oficial,
                        vvtipsol

        LET marca = 600

        CALL desmarca_cuenta(vn_seguro, marca, g_usuario, vn_folio)

        LET vfecha_emision = v_emision[5,6],"/",
                             v_emision[7,8],"/",
                             v_emision[1,4]
        LET f_emision      = vfecha_emision

        INITIALIZE vcod_opera TO NULL
        INITIALIZE vdiag_proc TO NULL
        SELECT a.cod_op,a.diag_proc
        INTO   vcod_opera,vdiag_proc
        FROM   safre_tmp:det_ref a
        WHERE  a.nss_solicitud = vn_seguro

        IF vcod_opera IS NOT NULL THEN
                UPDATE afi_ctr_expediente 
                SET    cod_operacion  = vcod_opera,
                       diag_proceso   = vdiag_proc
                WHERE  nss            = vn_seguro
                AND    n_folio        = vn_folio
                AND    tipo_solicitud = vvtipsol
                AND    cod_operacion  IS NULL
        END IF

        CASE vcod_operacion
            WHEN '02'
                LET vrechazados = vrechazados + 1

                {IF vdiag_proceso = '399' THEN
                    CALL actualiza_01()
                    CALL despliega_totales()
                ELSE}
                    CALL actualiza_02()
                    CALL despliega_totales(MOD)
                #END IF
            WHEN '01'
                LET vaprobados = vaprobados + 1

                CALL actualiza_01()
                CALL despliega_totales(MOD)

--------->CIRC UNICA
            WHEN '80'
                LET vaprobados = vaprobados + 1

                CALL actualiza_01()
                CALL despliega_totales(MOD)

            WHEN '83'
                LET vaprobados = vaprobados + 1

                CALL actualiza_01()
                CALL despliega_totales(MOD)
----------<CIRC UNICA

            {WHEN '03'
                LET vaprobados = vaprobados + 1

                CALL actualiza_01()

                UPDATE afi_mae_afiliado
                SET    status_interno = 160
                WHERE  n_seguro = vn_seguro

                UPDATE afi_mae_modifica
                SET    status_interno = 160,
                       cod_operacion  = 3,
                       nombre_pcanase = vnombre,
                       f_respuesta    = f_emision
                WHERE  n_seguro       = vn_seguro
                AND    cod_operacion  = 0
                AND    diag_proceso   = '0'

		CALL marca_cuenta( vn_seguro      ,
                                   edo_proc       ,
                                   vmarca_estado  ,
                                   vcodigo_rechazo,
                                   g_usuario      ,
                                   vn_folio )
                CALL despliega_totales(MOD)}
        END CASE
    END FOREACH

     -- actualiza historico de archivos procesar afi_ctr_arh_proc

    INSERT INTO afi_ctr_arh_proc 
    VALUES (generar, vtotal, vaprobados, vrechazados, vpendientes, vhoy)

    CALL despliega_totales(MOD)

    IF NOT bnd_proceso THEN
	ERROR ""
        PROMPT "Proceso finalizado satisfactoriamente, [Enter] Para Continuar "
        ATTRIBUTE (REVERSE)
        FOR vresp ATTRIBUTE (REVERSE)

        CLOSE WINDOW w_curp
    ELSE
        DISPLAY "Programa finalizado satisfactoriamente"
    END IF

    RETURN

END FUNCTION

FUNCTION actualiza_02()
#a02-------------------

    IF vdiag_proceso = '051' THEN
        UPDATE afi_mae_afiliado
        SET    status_interno = 160
        WHERE  n_seguro       = vn_seguro
        AND    status_interno in(130,240,300)

        UPDATE afi_mae_modifica
        SET    cod_operacion  = vcod_operacion,
               diag_proceso   = vdiag_proceso,
               status_interno = 160,
               nombre_pcanase = vnombre,
               f_respuesta    = f_emision
        WHERE  n_seguro       = vn_seguro
        AND    cod_operacion  = 0
        AND    diag_proceso   = "0"

        UPDATE afi_ctr_curp
        SET    cve_afo_nss_asoc = vcve_afo_nss_a,
               ident_tip_trab   = vid_tip_trab,
               cod_operacion    = vcod_operacion,
               diag_proceso     = vdiag_proceso15,
               curp_oficial     = vcurp_oficial
        WHERE  nss              = vn_seguro
        AND    cve_operacion    = '13'
        AND    fecha_respuesta IS NULL

    ELSE
        IF vcurp_oficial IS NULL OR
           vcurp_oficial[1] = ' ' OR
           LENGTH(vcurp_oficial) < 18 THEN
            UPDATE afi_mae_afiliado
            SET    status_interno = 140
            WHERE  n_seguro       = vn_seguro
            AND    status_interno in(130,240,300)

            UPDATE afi_mae_modifica
            SET    cod_operacion  = vcod_operacion,
                   diag_proceso   = vdiag_proceso,
                   status_interno = 140,
                   nombre_pcanase = vnombre,
                   f_respuesta    = f_emision
            WHERE  n_seguro       = vn_seguro
            AND    cod_operacion  = 0
            AND    diag_proceso   = "0"
        ELSE
            UPDATE afi_mae_afiliado
            SET    status_interno = 150
            WHERE  n_seguro       = vn_seguro
            AND    status_interno in(130,240,300)

            UPDATE afi_mae_modifica
            SET    cod_operacion  = vcod_operacion,
                   diag_proceso   = vdiag_proceso,
                   status_interno = 150,
                   nombre_pcanase = vnombre,
                   f_respuesta    = f_emision
            WHERE  n_seguro       = vn_seguro
            AND    cod_operacion  = 0
            AND    diag_proceso   = "0"
        END IF

        UPDATE afi_ctr_curp
        SET    cve_afo_nss_asoc = vcve_afo_nss_a,
               ident_tip_trab   = vid_tip_trab,
               cod_operacion    = vcod_operacion,
               diag_proceso     = vdiag_proceso15,
               curp_oficial     = vcurp_oficial,
               fecha_respuesta  = TODAY
        WHERE  nss              = vn_seguro
        AND    cve_operacion    = '13'
        AND    fecha_respuesta IS NULL

    END IF

    INSERT INTO afi_rechaza_proc 
    VALUES (vn_seguro, vdiag_proceso, vhoy, generar)

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

    DEFINE v_crea_fecha  DATE,
           v_existe      ,
           v_edad        ,
           v_criterio    ,
           v_ind_edad    SMALLINT,
           v_curp        CHAR(18),
           v_rfc         CHAR(13),
           v_fena        DATE,
           v_tipo_proc   ,
           v_tipo_trasp  ,
           v_medio       ,
           v_rechazo     ,
           v_folioatencion INTEGER

    DEFINE vn_folio_tes   INTEGER
    DEFINE vn_folio_tes2  INTEGER
    DEFINE vcuenta_tes    SMALLINT
    DEFINE vrowid         INTEGER
    DEFINE conta_tes      SMALLINT
    DEFINE ban_curp,
           ban_rfc,
           ban_regimen_edad SMALLINT
    DEFINE errmsg        CHAR(70)
    DEFINE vmarca_cod      ,
           vcorr           ,
           vvmarca_cod     CHAR(3),
           vmarca_cod23    CHAR(2),
           vcuenta_marca   INTEGER,
           vcontador_marca SMALLINT
    DEFINE vrowid2         INTEGER
    DEFINE vcondicion      CHAR(500)
    DEFINE vcuenta_asoc    SMALLINT

    INITIALIZE reg_mod.* TO NULL

    LET vnom_afore = NULL
    LET vnom_proce = NULL
    LET ban_curp   = 0
    LET ban_rfc    = 0
    LET ban_regimen_edad = 0  
    LET vmarca_cod       = 0
    INITIALIZE vvmarca_cod  TO NULL
    INITIALIZE vmarca_cod23 TO NULL
    LET vrowid2          = 0
    LET vcorr            = 0
    LET vcontador_marca  = 0
    INITIALIZE vcondicion to NULL

    PREPARE stmt1 FROM v_sql_1
    PREPARE stmt2 FROM v_sql_2

    LET marca2 = 610

    SELECT a.*, rowid
    INTO   reg_mod.*, vrow
    FROM   afi_mae_modifica a
    WHERE  a.n_seguro       = vn_seguro
    AND    a.cod_operacion  = 0
    AND    a.diag_proceso   = "0"
    AND    a.status_interno = 130

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
           a.usuario
    INTO   reg_ant.paterno,
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
           reg_ant.usuario
    FROM   afi_mae_afiliado a
    WHERE  a.n_seguro = vn_seguro

    IF reg_mod.edo_civil IS NULL OR
       reg_mod.edo_civil = " "   THEN
       LET reg_mod.edo_civil = 0
    END IF




    IF vcurp_oficial IS NULL OR
       vcurp_oficial = "                  " THEN
       UPDATE afi_mae_afiliado
       SET    paterno            = reg_mod.paterno            ,
              materno            = reg_mod.materno            ,
              nombres            = reg_mod.nombres            ,
              n_rfc              = reg_mod.n_rfc              ,
              sexo               = reg_mod.sexo               ,
              fena               = reg_mod.fena               ,
              salario_base_comis = reg_mod.salario_base_comis ,
              estadon            = reg_mod.estadon            ,
              nacionalidad       = reg_mod.nacionalidad       ,
              tip_prob           = reg_mod.tip_prob           ,
              fol_prob           = reg_mod.fol_prob           ,
              doc_prob           = reg_mod.doc_prob           ,
              const_curp         = reg_mod.const_curp         ,
              usuario            = reg_mod.usuario
       WHERE  n_seguro           = vn_seguro
       AND    n_folio            = reg_mod.n_folio
       AND    tipo_solicitud     = reg_mod.tipo_solicitud
       AND    status_interno     in(130,160)

    ELSE
       UPDATE afi_mae_afiliado
       SET    paterno            = reg_mod.paterno            ,
              materno            = reg_mod.materno            ,
              nombres            = reg_mod.nombres            ,
              --n_unico            = vcurp_oficial              ,
              n_unico            = reg_mod.n_unico            ,
              n_rfc              = reg_mod.n_rfc              ,
              sexo               = reg_mod.sexo               ,
              fena               = reg_mod.fena               ,
              salario_base_comis = reg_mod.salario_base_comis ,
              estadon            = reg_mod.estadon            ,
              nacionalidad       = reg_mod.nacionalidad       ,
              tip_prob           = reg_mod.tip_prob           ,
              fol_prob           = reg_mod.fol_prob           ,
              doc_prob           = reg_mod.doc_prob           ,
              const_curp         = reg_mod.const_curp         ,
              usuario            = reg_mod.usuario
       WHERE  n_seguro           = vn_seguro
       AND    n_folio            = reg_mod.n_folio
       AND    tipo_solicitud     = reg_mod.tipo_solicitud
       AND    status_interno     in(130,160)
    END IF

    UPDATE afi_mae_modifica
    SET    paterno            = reg_ant.paterno            ,
           materno            = reg_ant.materno            ,
           nombres            = reg_ant.nombres            ,
           n_unico            = reg_ant.n_unico            ,
           n_rfc              = reg_ant.n_rfc              ,
           sexo               = reg_ant.sexo               ,
           fena               = reg_ant.fena               ,
           salario_base_comis = reg_ant.salario_base_comis ,
           estadon            = reg_ant.estadon            ,
           nacionalidad       = reg_ant.nacionalidad       ,
           tip_prob           = reg_ant.tip_prob           ,
           fol_prob           = reg_ant.fol_prob           ,
           doc_prob           = reg_ant.doc_prob           ,
           const_curp         = reg_ant.const_curp         ,
           usuario            = reg_ant.usuario            ,
           f_respuesta        = f_emision
    WHERE  n_seguro           = vn_seguro
    AND    n_folio            = reg_mod.n_folio
    AND    tipo_solicitud     = reg_mod.tipo_solicitud
    AND    status_interno     = 130

    UPDATE afi_ctr_curp
    SET    cve_afo_nss_asoc   = vcve_afo_nss_a             ,
           ident_tip_trab     = vid_tip_trab               ,
           cod_operacion      = vcod_operacion             ,
           diag_proceso       = vdiag_proceso15            ,
           curp_oficial       = vcurp_oficial              ,
           fecha_respuesta    = TODAY
    WHERE  nss                = vn_seguro
    AND    n_folio            = reg_mod.n_folio
    AND    tipo_solicitud     = reg_mod.tipo_solicitud
    AND    cve_operacion      = '13'
    AND    fecha_respuesta IS NULL

--->actualiza regimen de inv
    --IF reg_ant.n_unico <> vcurp_oficial OR
    --   reg_ant.fena    <> reg_mod.fena THEN

--CUO13
    LET vcuenta_asoc = 0
    SELECT COUNT(*)
    INTO vcuenta_asoc
    FROM  afi_ctr_nss_asoc
    WHERE nss = vn_seguro
    AND   n_folio = reg_mod.n_folio
    AND   tipo_solicitud = reg_mod.tipo_solicitud

    IF vcuenta_asoc > 0 THEN
       UPDATE afi_ctr_nss_asoc
       SET   nss_asoc = vnss_asoc,
             cve_afo_nss_asoc = vcve_afo_nss_a,
             fecha_respuesta  = TODAY,
             usuario          = g_usuario
       WHERE nss            = vn_seguro
       AND   n_folio        = reg_mod.n_folio
       AND   tipo_solicitud = reg_mod.tipo_solicitud
    ELSE
      INSERT INTO afi_ctr_nss_asoc
      VALUES (vn_seguro              ,
              reg_mod.n_folio        ,
              reg_mod.tipo_solicitud ,
              vnss_asoc, 
              vcve_afo_nss_a,
              TODAY,
              g_usuario,
              TODAY)
    END IF
--<CUO13



    IF reg_ant.n_unico[5,10] <> vcurp_oficial[5,10] THEN

       LET v_crea_fecha = HOY
       LET v_tipo_trasp = 15    #MODIFICACION DATOS CURP
       LET v_tipo_proc  = 1
       LET v_medio      = 10

       DECLARE curs1 CURSOR FOR stmt1
       OPEN  curs1 USING vn_seguro, v_crea_fecha
       FETCH curs1 INTO v_existe, v_edad, v_criterio, v_ind_edad,
                        v_curp, v_rfc, v_fena
       CLOSE curs1
--->aplica validacion por normativa de edad del trabajador a partir de CPL-1634
       DISPLAY "Edad del trabajador: ",    v_edad
       #busca si regimen de trabajador de la subcuenta 1 este en la siefore 1 
       SELECT 'X'
       FROM   cta_regimen
       WHERE  nss            = vn_seguro
       AND    subcuenta      = 1
       AND    codigo_siefore = 1
 
       IF SQLCA.SQLCODE <> NOTFOUND THEN   #si es encontrado
           DISPLAY "Entra a validacion de edad por tener regimen sub 1, siefore 1"

           IF (v_edad  <= 55  OR
               v_edad  >= 60) THEN            #fuera de rango de 56 - 59
              DISPLAY "Cumple con una edad <= 56 o >= 59 se  llama a cambia regimen " 
                   --### Llama a funcion para cambio de regimen 
           
              LET ban_regimen_edad = 1
           END IF

       ELSE
          LET ban_regimen_edad = 2
       END IF
---<fin de validacion de edad del trabajador

       IF ban_regimen_edad = 1 OR   #banderas encendidas por regimen 1 fuera de rango de edad
          ban_regimen_edad = 2 THEN #banderas encendidas por no estar en regimen 1

          DECLARE curs2 CURSOR FOR stmt2
          OPEN curs2 USING vn_seguro,
                           v_ind_edad,
                           v_ind_edad,
                           v_tipo_proc,
                           v_tipo_trasp,
                           v_medio
--->cacha y despliega error 
          WHENEVER ERROR CONTINUE
          FETCH curs2 INTO v_existe, v_ind_edad, v_rechazo, v_folioatencion
             IF SQLCA.SQLCODE < 0 THEN              
                LET errmsg = err_get(SQLCA.SQLCODE)
                ERROR errmsg 
                PROMPT "No puede continuar, Enter para continuar " for enter
                EXIT PROGRAM
             END IF
          WHENEVER ERROR STOP
---<
          CLOSE curs2
   
          LET vcuenta_tes   = 0
          LET vn_folio_tes  = 0
          LET vn_folio_tes2 = 0
   
          SELECT COUNT(*)
          INTO vcuenta_tes
          FROM  tes_solicitud
          WHERE nss = vn_seguro
          AND   tipo_traspaso = 15
          AND   fecha_solicitud = TODAY
   
          IF vcuenta_tes > 1 THEN
             LET conta_tes = 0
   
             DECLARE curs_3 CURSOR FOR 
             SELECT folio_solicitud,ROWID
             FROM   tes_solicitud
             WHERE  nss             = vn_seguro
             AND    tipo_traspaso   = 15
             AND    fecha_solicitud = TODAY
   
             FOREACH curs_3 INTO vn_folio_tes,vrowid
   
                LET conta_tes = conta_tes + 1
   
                IF conta_tes = 2 THEN
                   IF   vn_folio_tes2     =  0   THEN 
                        LET vn_folio_tes2      = vn_folio_tes + 1
                   ELSE
                        LET vn_folio_tes2      = vn_folio_tes2 + 1
                   END IF
   
                   INSERT INTO cta_solicitud_regimen
                        VALUES ( vn_seguro              ,
                                 TODAY                  ,
                                 '0'                    , -- Folio Serial
                                 v_medio                ,
                                 CURRENT                ,
                                 CURRENT                ,
                                 v_rechazo              ,
                                 v_ind_edad             ,
                                 v_criterio             ,
                                 NULL                   ,
                                 v_tipo_trasp
                                )
 
                --****  Recupera el Folio Asignado ****
                   --LET vn_folio_tes2 = DBINFO('sqlca.sqlerrd1')
                   IF SQLCA.SQLCODE = 0 THEN
                      LET    vn_folio_tes2 = 0
                      SELECT  MAX(folio_solicitud)
                      INTO    vn_folio_tes2
                      FROM    cta_solicitud_regimen
                      WHERE   nss     = vn_seguro
                      AND     fecha_solicitud = TODAY
                      AND     tipo_proceso    = v_tipo_trasp
                   END IF
   
                   UPDATE tes_solicitud 
                   SET    folio_solicitud = vn_folio_tes2
                   WHERE  nss             = vn_seguro
                   AND    tipo_traspaso   = 15
                   AND    fecha_solicitud = TODAY
                   AND    ROWID           = vrowid
                END IF
   
                IF conta_tes > 2 THEN
                   IF   vn_folio_tes2     =  0   THEN 
                        LET vn_folio_tes2      = vn_folio_tes + 1
                   ELSE
                        LET vn_folio_tes2      = vn_folio_tes2 + 1
                   END IF
   
                   INSERT INTO cta_solicitud_regimen
                        VALUES ( vn_seguro              ,
                                 TODAY                  ,
                                 '0'                    , -- Folio Serial
                                 v_medio                ,
                                 CURRENT                ,
                                 CURRENT                ,
                                 v_rechazo              ,
                                 v_ind_edad             ,
                                 v_criterio             ,
                                 NULL                   ,
                                 v_tipo_trasp
                                )
   
                --****  Recupera el Folio Asignado ****
                   --LET vn_folio_tes2 = DBINFO('sqlca.sqlerrd1')
                   IF SQLCA.SQLCODE = 0 THEN
                      LET    vn_folio_tes2 = 0
                      SELECT  MAX(folio_solicitud)
                      INTO    vn_folio_tes2
                      FROM    cta_solicitud_regimen
                      WHERE   nss     = vn_seguro
                      AND     fecha_solicitud = TODAY
                      AND     tipo_proceso    = v_tipo_trasp
                   END IF
   
                   UPDATE tes_solicitud 
                   SET    folio_solicitud = vn_folio_tes2
                   WHERE  nss             = vn_seguro
                   AND    tipo_traspaso   = 15
                   AND    fecha_solicitud = TODAY
                   AND    ROWID           = vrowid
   
                END IF
   
                LET vcuenta_tes   = 0
                --LET vn_folio_tes  = 0
                --LET vn_folio_tes2 = 0
                LET vrowid        = 0
             END FOREACH
--->>>MLM-2433 y CPL-1634
                DECLARE cur_marca2 CURSOR FOR
                   SELECT marca_cod,correlativo,ROWID
                   FROM   cta_act_marca
                   WHERE  nss         =  vn_seguro
                   AND    marca_cod   IN (301,302,303,304,307,308,309,310,311)
                   AND    fecha_ini   =  TODAY
                   AND    correlativo =  vn_folio_tes
                   ORDER  by marca_cod

                   FOREACH cur_marca2 INTO   vmarca_cod,vcorr,vrowid2
                      LET vvmarca_cod = vmarca_cod
                      LET vmarca_cod23 = vvmarca_cod[2,3]
                      LET vcontador_marca = vcontador_marca + 1
   
                      UPDATE cta_act_marca 
                      SET    correlativo =  (SELECT folio_solicitud
                                             FROM   tes_solicitud t
                                             WHERE  nss             = vn_seguro
                                             AND    tipo_traspaso   = 15 
                                             AND    fecha_solicitud = TODAY
                                             AND    t.grupo_regimen = (SELECT n.grupo_regimen
                                                                       FROM   tab_grupo_regimen n
                                                                       WHERE  t.grupo_regimen = n.grupo_regimen
                                                                       AND    n.marca_cod     = cta_act_marca.marca_cod))
                      WHERE  nss         =  vn_seguro
                      AND    marca_cod   IN (301,302,303,304,307,308,309,310,311)
                      AND    fecha_ini   =  TODAY
                      AND    marca_cod   = vmarca_cod
                      AND    ROWID       = vrowid2
 
                      UPDATE cta_his_marca 
                      SET    correlativo =  (SELECT folio_solicitud
                                             FROM   tes_solicitud t
                                             WHERE  nss             = vn_seguro
                                             AND    tipo_traspaso   = 15 
                                             AND    fecha_solicitud = TODAY
                                             AND    t.grupo_regimen = (SELECT n.grupo_regimen
                                                                       FROM   tab_grupo_regimen n
                                                                       WHERE  t.grupo_regimen = n.grupo_regimen
                                                                       AND    n.marca_cod     = cta_his_marca.marca_cod))
                      WHERE  nss         =  vn_seguro
                      AND    marca_cod   IN (301,302,303,304,307,308,309,310,311)
                      AND    fecha_ini   =  TODAY        
                      AND    marca_cod   = vmarca_cod
                   END FOREACH
---<<<MLM-2433
          END IF
       END IF

       LET ban_curp   = 0  
       LET ban_rfc    = 0
       LET ban_regimen_edad = 0

       IF v_rechazo <> 0 THEN
         INSERT INTO safre_tmp:rch_apertura
         VALUES (vn_seguro,v_rechazo)
       END IF
    END IF
---<

    IF vcod_operacion = '01' OR
       vcod_operacion = '80' OR
       vcod_operacion = '83' THEN
    CASE vdiag_proceso
        WHEN '001'
            UPDATE afi_mae_afiliado
            SET    status_interno = 160
            WHERE  n_seguro = vn_seguro
            AND    status_interno in(130,240,300)

            UPDATE afi_mae_modifica
            SET    cod_operacion  = vcod_operacion,
                   diag_proceso   = vdiag_proceso,
                   status_interno = 160,
                   nombre_pcanase = vnombre,
                   f_respuesta    = f_emision
            WHERE  n_seguro       = vn_seguro
            AND    cod_operacion  = 0
            AND    diag_proceso   = "0"

            CALL marca_cuenta( vn_seguro       ,
                               edo_proc        ,
                               vmarca_estado   ,
                               vcodigo_rechazo ,
                               g_usuario       ,
                               reg_ant.n_folio 
                             )

       WHEN '002'
            IF vcurp_oficial IS NULL OR
               vcurp_oficial[1] = ' ' OR
               LENGTH(vcurp_oficial) < 18 THEN
                UPDATE afi_mae_afiliado
                SET    status_interno = 100
                WHERE  n_seguro       = vn_seguro
                AND    status_interno in(130,240,300)

                IF SQLCA.SQLCODE = 0 THEN
                    UPDATE afi_mae_modifica
                    SET    cod_operacion  = vcod_operacion,
                           diag_proceso   = vdiag_proceso,
                           status_interno = 100,
                           nombre_pcanase = vnombre,
                           f_respuesta    = f_emision
                    WHERE  n_seguro       = vn_seguro
                    AND    cod_operacion  = 0
                    AND    diag_proceso   = "0"
                ELSE
                    UPDATE afi_mae_modifica
                    SET    cod_operacion  = vcod_operacion,
                           diag_proceso   = vdiag_proceso,
                           status_interno = 160,
                           nombre_pcanase = vnombre,
                           f_respuesta    = f_emision
                    WHERE  n_seguro       = vn_seguro
                    AND    cod_operacion  = 0
                    AND    diag_proceso   = "0"
                END IF
            ELSE
                UPDATE afi_mae_afiliado
                SET    status_interno = 200
                WHERE  n_seguro       = vn_seguro
                AND    status_interno in(130,240,300)

                IF SQLCA.SQLCODE = 0 THEN
                    UPDATE afi_mae_modifica
                    SET    cod_operacion  = vcod_operacion,
                           diag_proceso   = vdiag_proceso,
                           status_interno = 200,
                           nombre_pcanase = vnombre,
                           f_respuesta    = f_emision
                    WHERE  n_seguro       = vn_seguro
                    AND    cod_operacion  = 0
                    AND    diag_proceso   = "0"
                ELSE
                    UPDATE afi_mae_modifica
                    SET    cod_operacion  = vcod_operacion,
                           diag_proceso   = vdiag_proceso,
                           status_interno = 160,
                           nombre_pcanase = vnombre,
                           f_respuesta    = f_emision
                    WHERE  n_seguro       = vn_seguro
                    AND    cod_operacion  = 0
                    AND    diag_proceso   = "0"
                END IF
            END IF
       WHEN '003'
            IF vcurp_oficial IS NULL OR
               vcurp_oficial[1] = ' ' OR
               LENGTH(vcurp_oficial) < 18 THEN
                UPDATE afi_mae_afiliado
                --SET    status_interno = 145
                SET    status_interno = 100
                WHERE  n_seguro       = vn_seguro
                AND    status_interno in(130,240,300)

                IF SQLCA.SQLCODE = 0 THEN
                    UPDATE afi_mae_modifica
                    SET    cod_operacion  = vcod_operacion,
                           diag_proceso   = vdiag_proceso,
                           --status_interno = 145,
                           status_interno = 100,
                           nombre_pcanase = vnombre,
                           f_respuesta    = f_emision
                    WHERE  n_seguro       = vn_seguro
                    AND    cod_operacion  = 0
                    AND    diag_proceso   = "0"
                END IF
            ELSE
                UPDATE afi_mae_afiliado
                --SET    status_interno = 155
                SET    status_interno = 200
                WHERE  n_seguro       = vn_seguro
                AND    status_interno in(130,240,300)

                IF SQLCA.SQLCODE = 0 THEN
                    UPDATE afi_mae_modifica
                    SET    cod_operacion  = vcod_operacion,
                           diag_proceso   = vdiag_proceso,
                           --status_interno = 155,
                           status_interno = 200,
                           nombre_pcanase = vnombre,
                           f_respuesta    = f_emision
                    WHERE  n_seguro       = vn_seguro
                    AND    cod_operacion  = 0
                    AND    diag_proceso   = "0"
                END IF
            END IF

       OTHERWISE
            UPDATE afi_mae_afiliado
            SET    status_interno = 200
            WHERE  n_seguro = vn_seguro
            AND    status_interno in(130,240,300)

            UPDATE afi_mae_modifica
            SET    cod_operacion  = vcod_operacion,
                   diag_proceso   = vdiag_proceso,
                   status_interno = 200,
                   nombre_pcanase = vnombre,
                   f_respuesta    = f_emision
            WHERE  n_seguro       = vn_seguro
            AND    cod_operacion  = 0
            AND    diag_proceso   = "0"
    END CASE
    END IF

            #CALL valida_folio(vrow)

	    LET pat_proc = NULL
	    LET mat_proc = NULL
	    LET nom_proc = NULL

            SELECT MAX(fecha_recepcion)
            INTO   f_recep
            FROM   afi_det_notifica
            WHERE  n_seguro = vn_seguro

            SELECT MAX(rowid)
            INTO   mrow
            FROM   afi_det_notifica
            WHERE  n_seguro = vn_seguro
	    AND    fecha_recepcion = f_recep

            IF f_recep IS NOT NULL THEN
                SELECT n.paterno_proc, n.materno_proc, n.nombres_proc
                INTO   pat_proc, mat_proc, nom_proc
                FROM   afi_det_notifica n
                WHERE  n.n_seguro = vn_seguro
	        AND    n.fecha_recepcion = f_recep
		AND    n.rowid = mrow
            ELSE
		SELECT MAX(nn.f_transf_lote)
                INTO   f_recep
                FROM   afi_det_notifica nn
                WHERE  nn.n_seguro = vn_seguro

                SELECT MAX(rowid)
                INTO   mrow
                FROM   afi_det_notifica
                WHERE  n_seguro = vn_seguro
	        AND    f_transf_lote = f_recep

                SELECT n.paterno_proc, n.materno_proc, n.nombres_proc
                INTO   pat_proc, mat_proc, nom_proc
                FROM   afi_det_notifica n
                WHERE  n.n_seguro = vn_seguro
	        AND    n.f_transf_lote = f_recep
		AND    n.rowid = mrow
            END IF

	    IF mat_proc MATCHES '  *' THEN
		LET mat_proc = NULL
            END IF

	    IF reg_mod.materno MATCHES '  *' THEN
		LET reg_mod.materno = NULL
            END IF

            LET vnom_afore = reg_mod.paterno," ",
                             reg_mod.materno," ",
                             reg_mod.nombres

            LET vnom_afore = vnom_afore CLIPPED

            LET vnom_proce = pat_proc," ",
                             mat_proc," ",
                             nom_proc

            LET vnom_proce = vnom_proce CLIPPED

            IF vnom_proce MATCHES vnom_afore OR
               vnom_proce LIKE    vnom_afore OR
               vnom_proce =       vnom_afore THEN
                CALL desmarca_cuenta(vn_seguro, marca2, g_usuario, 
                                     reg_ant.n_folio)
            END IF

END FUNCTION

FUNCTION despliega_totales(modulo)
#dt-------------------------

DEFINE modulo CHAR(30)

     IF NOT bnd_proceso THEN
        DISPLAY "  DATOS A PROCESAR  POR ",modulo
            AT 8,1 ATTRIBUTE ( REVERSE )

        DISPLAY "Total de Registros del lote     : ",
                 vtotal USING "#######&" AT 9,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros aceptados : ",          --CUO13
                 vaprobados USING "#######&" AT 10,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros rechazados   : ",
                 vrechazados USING "#######&" AT 11,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros pendientes   : ",
                 vpendientes USING "#######&" AT 12,15 ATTRIBUTE ( BOLD )

    ELSE
        DISPLAY " DATOS A PROCESAR  POR  ", modulo

        DISPLAY "Total de Registros del lote     : ",
                 vtotal USING "#######&" 

        DISPLAY "Registros aceptados : ",           --CUO13
                 vaprobados USING "#######&" 

        DISPLAY "Registros rechazados   : ",
                 vrechazados USING "#######&" 

        DISPLAY "Registros pendientes   : ",
                 vpendientes USING "#######&" 
    END IF

    {IF NOT bnd_proceso THEN
	PROMPT "Presione [Enter] para continuar" FOR enter
    END IF}

END FUNCTION

--CUO13
FUNCTION actualiza_dif_maeafili()
    OPEN WINDOW w_curp2 AT 8,4 WITH FORM "AFIC0082"
    ATTRIBUTE (BORDER)

    LET vhoy = TODAY
    LET MOD  = "NOTIFICACION DE EXPEDIENTE "
{
    DATABASE safre_tmp
    UPDATE STATISTICS FOR TABLE det_procesar
    DATABASE safre_af
}
    SELECT COUNT(*)
    INTO   vtotal
    FROM   safre_tmp:det_ref a, afi_mae_afiliado b
    WHERE  a.nss_solicitud  = b.n_seguro
    AND    NOT EXISTS (SELECT 1
                       FROM afi_mae_modifica c
                       WHERE c.n_seguro = b.n_seguro
                       AND   c.status_interno = 130
                       AND   c.cod_operacion  = 0
                       AND   c.diag_proceso   = '0')

    DECLARE c_nss CURSOR FOR
    SELECT a.nss_solicitud,
           a.diag_proc[1,3],
           a.cod_op,
           b.n_unico,
           b.n_folio,
           b.tipo_solicitud
    FROM   safre_tmp:det_ref a, afi_mae_afiliado b
    WHERE  a.nss_solicitud  = b.n_seguro
    AND    NOT EXISTS (SELECT 1
                       FROM afi_mae_modifica c
                       WHERE c.n_seguro = b.n_seguro
                       AND   c.status_interno = 130
                       AND   c.cod_operacion  = 0
                       AND   c.diag_proceso   = '0')
    ORDER BY a.nss_solicitud

    FOREACH c_nss  INTO vn_seguro,
                        vdiag_proceso,
                        vcod_operacion,
                        vcurp,
                        vn_folio,
                        vtipsol

        CASE vcod_operacion
            WHEN '02'
                LET vrechazados = vrechazados + 1

                UPDATE afi_ctr_expediente 
                SET    cod_operacion  = vcod_operacion,
                       diag_proceso   = vdiag_proceso
                WHERE  nss            = vn_seguro
                AND    n_folio        = vn_folio
                AND    tipo_solicitud = vtipsol
                AND    cod_operacion  IS NULL

                    --CALL actualiza_02()
                    CALL despliega_totales(MOD)
            WHEN '01'
                LET vaprobados = vaprobados + 1

                UPDATE afi_ctr_expediente 
                SET    cod_operacion  = vcod_operacion,
                       diag_proceso   = vdiag_proceso
                WHERE  nss            = vn_seguro
                AND    n_folio        = vn_folio
                AND    tipo_solicitud = vtipsol
                AND    cod_operacion  IS NULL

                --CALL actualiza_01()
                CALL despliega_totales(MOD)

            WHEN '80'
                LET vaprobados = vaprobados + 1

                --CALL actualiza_01()
                CALL despliega_totales(MOD)

            WHEN '83'
                LET vaprobados = vaprobados + 1

                --CALL actualiza_01()
                CALL despliega_totales(MOD)

        END CASE
    END FOREACH

    CALL despliega_totales(MOD)

    IF NOT bnd_proceso THEN
        ERROR ""
        PROMPT "Proceso de EXPEDIENTE finalizado ok, [Enter] Para Continuar "
        ATTRIBUTE (REVERSE)
        FOR vresp ATTRIBUTE (REVERSE)

        CLOSE WINDOW w_curp2
    ELSE
        DISPLAY "Programa finalizado satisfactoriamente"
    END IF

    RETURN

END FUNCTION 
--CUO13

FUNCTION lista_err()
#-------------------

     DEFINE hora          CHAR(8)
     DEFINE HOY           DATE
     DEFINE vcod_rechazo  CHAR(8)
     DEFINE vcont         INTEGER

     DEFINE gr_curp RECORD
         n_seguro    LIKE afi_mae_afiliado.n_seguro,
         n_folio     LIKE afi_mae_afiliado.n_folio,
         tipo_solicitud CHAR(1),
         paterno     CHAR(13),
         materno     CHAR(13),
         nombres     CHAR(16),
         des_rechazo CHAR(49),
         cod_rechazo LIKE afi_rechaza_proc.cod_rechazo
     END RECORD

     DEFINE G_LISTA         CHAR(200)
     DEFINE G_IMPRIME       CHAR(200)

    DEFINE resp   CHAR(1)

    LET hora = TIME
    LET HOY = TODAY

    LET G_LISTA_ERR =  g_paramgrales.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED,".RECH_MOD.",
                   HOY USING "ddmmyy","_",hora[1,2],
                   hora[4,5],hora[7,8] CLIPPED

    START REPORT listado TO G_LISTA_ERR

    DECLARE c_carga CURSOR FOR
        SELECT c.n_seguro, 
               c.n_folio, 
               c.tipo_solicitud,
               b.paterno, 
               b.materno, 
               b.nombres, 
               a.rdeta_desc_c,
               b.diag_proceso[1,3]
        FROM   safre_tmp:det_procesar b,
               safre_af:afi_mae_afiliado c,
               safre_af:afi_mae_modifica d,
        OUTER  (safre_af:tab_rdeta a)
        WHERE  c.n_seguro = b.nss_solicitud
        AND    d.n_seguro = c.n_seguro
        AND    d.n_seguro = b.nss_solicitud
        AND    b.diag_proceso[1,3] = a.rdeta_cod
        AND    b.cod_operacion = "02"
        AND    a.modulo_cod    = "afi"
        ORDER BY 3,1

    FOREACH c_carga INTO gr_curp.*
        OUTPUT TO REPORT listado(gr_curp.*)
    END FOREACH 

    FINISH REPORT listado

    --ERROR "LISTADO GENERADO" SLEEP 2
{
    LET G_LISTA_ERR =  "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED, ".RECH_MOD.",
                   HOY USING "ddmmyy","_",hora[1,2],
                   hora[4,5],hora[7,8] CLIPPED

    RUN G_LISTA_ERR

    LET G_IMPRIME = g_paramgrales.ruta_listados CLIPPED,"/",
                    g_usuario CLIPPED,".RECH_MOD.",
                    HOY USING "ddmmyy","_",hora[1,2],
                    hora[4,5],hora[7,8] CLIPPED

    RUN G_IMPRIME
}
END FUNCTION

--->CUO13
FUNCTION lista_err_exp()
#-------------------

     DEFINE hora          CHAR(8)
     DEFINE HOY           DATE
     DEFINE vcod_rechazo  CHAR(8)
     DEFINE vcont         INTEGER

     DEFINE gr_curp RECORD
         n_seguro    LIKE afi_mae_afiliado.n_seguro,
         n_folio     LIKE afi_mae_afiliado.n_folio,
         tipo_solicitud CHAR(1),
         paterno     CHAR(13),
         materno     CHAR(13),
         nombres     CHAR(16),
         des_rechazo CHAR(49),
         cod_rechazo LIKE afi_rechaza_proc.cod_rechazo
     END RECORD


     DEFINE G_IMPRIME       CHAR(200)

    DEFINE resp   CHAR(1)

    LET hora = TIME
    LET HOY = TODAY

    LET G_LISTA_EXP =  g_paramgrales.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED,".RECH_EXP.",
                   HOY USING "ddmmyy","_",hora[1,2],
                   hora[4,5],hora[7,8] CLIPPED

    START REPORT listado TO G_LISTA_EXP

    DECLARE c_carga_e CURSOR FOR
        SELECT c.n_seguro, 
               c.n_folio, 
               c.tipo_solicitud,
               c.paterno, 
               c.materno, 
               c.nombres, 
               a.rdeta_desc_c,
               b.diag_proc[1,3]
        FROM   safre_tmp:det_ref b,
               safre_af:afi_mae_afiliado c,
        OUTER  (safre_af:tab_rdeta a)
        WHERE  c.n_seguro = b.nss_solicitud
        --AND    c.n_unico  = b.curp_solicitud
        AND    b.diag_proc[1,3] = a.rdeta_cod
        AND    b.cod_op = "02"
        AND    a.modulo_cod    = "afi"
        ORDER BY 3,1

    FOREACH c_carga_e INTO gr_curp.*
        OUTPUT TO REPORT listado(gr_curp.*)
    END FOREACH 

    FINISH REPORT listado

    --ERROR "LISTADO GENERADO" SLEEP 2
{
    LET G_LISTA_EXP =  "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED, ".RECH_EXP.",
                   HOY USING "ddmmyy","_",hora[1,2],
                   hora[4,5],hora[7,8] CLIPPED

    RUN G_LISTA_EXP

    LET G_IMPRIME = g_paramgrales.ruta_listados CLIPPED,"/",
                    g_usuario CLIPPED,".RECH_MOD.",
                    HOY USING "ddmmyy","_",hora[1,2],
                    hora[4,5],hora[7,8] CLIPPED

    RUN G_IMPRIME
}
END FUNCTION


FUNCTION lista_op13()
#-------------------

     DEFINE hora          CHAR(8)
     DEFINE HOY           DATE
     DEFINE vcod_rechazo  CHAR(8)
     DEFINE vcont         INTEGER

     DEFINE gr_curp RECORD
         n_seguro    LIKE afi_mae_afiliado.n_seguro,
         n_folio     LIKE afi_mae_afiliado.n_folio,
         tipo_solicitud CHAR(1),
         paterno     CHAR(13),
         materno     CHAR(13),
         nombres     CHAR(16),
         --des_rechazo CHAR(49),
         cod_op_det03  LIKE afi_ctr_det_op13.cod_op_det03 ,
         dia_proc03    LIKE afi_ctr_det_op13.dia_proc03   , 
         cod_op_det04  LIKE afi_ctr_det_op13.cod_op_det04 ,     
         dia_proc04    LIKE afi_ctr_det_op13.dia_proc04   , 
         cod_op_det05  LIKE afi_ctr_det_op13.cod_op_det05 ,     
         dia_proc05    LIKE afi_ctr_det_op13.dia_proc05   , 
         cod_op_det06  LIKE afi_ctr_det_op13.cod_op_det06 ,     
         dia_proc06    LIKE afi_ctr_det_op13.dia_proc06   , 
         cod_op_det07  LIKE afi_ctr_det_op13.cod_op_det07 ,     
         dia_proc07    LIKE afi_ctr_det_op13.dia_proc07   , 
         tipo          LIKE afi_ctr_det_op13.tipo         ,     
         motivo        LIKE afi_ctr_det_op13.motivo       ,     
         tipo_id       LIKE afi_ctr_det_op13.tipo_id    
     END RECORD


     DEFINE G_IMPRIME       CHAR(200)

    DEFINE resp   CHAR(1)

    LET hora = TIME
    LET HOY = TODAY

    LET G_LISTA_13 =  g_paramgrales.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED,".LIST_OP13+.",
                   HOY USING "ddmmyy","_",hora[1,2],
                   hora[4,5],hora[7,8] CLIPPED

    START REPORT listado_op13 TO G_LISTA_13

    DECLARE c_carga_op13 CURSOR FOR
        SELECT b.nss , 
               c.n_folio        , 
               c.tipo_solicitud ,
               c.paterno        , 
               c.materno        , 
               c.nombres        ,
               b.cod_op_det03   ,
               b.dia_proc03[1,3],
               b.cod_op_det04   ,
               b.dia_proc04[1,3],
               b.cod_op_det05   ,
               b.dia_proc05[1,3],
               b.cod_op_det06   ,
               b.dia_proc06[1,3],
               b.cod_op_det07   ,
               b.dia_proc07[1,3],
               b.tipo           ,
               b.motivo         ,
               b.tipo_id         
               --c.rdeta_desc_c,
               --b.diag_proceso[1,3]
       --FROM   safre_af:tab_rdeta a, 
       FROM    afi_ctr_det_op13 b,
               safre_af:afi_mae_afiliado c
        WHERE  c.n_seguro = b.nss
        --AND    b.diag_proceso[1,3] = a.rdeta_cod
        --AND    b.cod_op_det06 = "02"
        --AND    a.modulo_cod    = "afi"
        ORDER BY 3,1

    FOREACH c_carga_op13 INTO gr_curp.*

        INITIALIZE vdesc_motivo TO NULL
        SELECT motivo_desc 
        INTO   vdesc_motivo
        FROM   rec_motivo
        WHERE  tipo_cod   = gr_curp.tipo
        AND    motivo_cod = gr_curp.motivo
        AND    tipo_id    = gr_curp.tipo_id
      
        OUTPUT TO REPORT listado_op13(gr_curp.*,vdesc_motivo)
    END FOREACH 

    FINISH REPORT listado_op13

    --ERROR "LISTADO GENERADO" SLEEP 2
    DISPLAY  "LISTADOS: ", G_LISTA_13  AT 10,1
    DISPLAY  "LISTADOS: ", G_LISTA_ACE AT 11,1
    DISPLAY  "LISTADOS: ", G_LISTA_ERR AT 12,1
    DISPLAY  "LISTADOS: ", G_LISTA_EXP AT 13,1

    PROMPT "Proceso de OP13 PLUS finalizado ok, [Enter] Para Continuar "
        ATTRIBUTE (REVERSE)
        FOR vresp ATTRIBUTE (REVERSE)
        EXIT PROGRAM
{
    LET G_LISTA_13 =  "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED, ".LIST_OP13+.",
                   HOY USING "ddmmyy","_",hora[1,2],
                   hora[4,5],hora[7,8] CLIPPED

    RUN G_LISTA_13

    LET G_IMPRIME = g_paramgrales.ruta_listados CLIPPED,"/",
                    g_usuario CLIPPED,".RECH_MOD.",
                    HOY USING "ddmmyy","_",hora[1,2],
                    hora[4,5],hora[7,8] CLIPPED

    RUN G_IMPRIME
}
END FUNCTION

REPORT listado_op13(rpt,rdesc_motivo)

    DEFINE rpt RECORD
         n_seguro       LIKE afi_mae_afiliado.n_seguro,
         n_folio        LIKE afi_mae_afiliado.n_folio,
         tipo_solicitud CHAR(1),
         paterno        CHAR(13),
         materno        CHAR(13),
         nombres        CHAR(16),
         --des_rechazo  CHAR(49),
         cod_op_det03   LIKE afi_ctr_det_op13.cod_op_det03 ,     
         dia_proc03     LIKE afi_ctr_det_op13.dia_proc03   , 
         cod_op_det04   LIKE afi_ctr_det_op13.cod_op_det04 ,     
         dia_proc04     LIKE afi_ctr_det_op13.dia_proc04   , 
         cod_op_det05   LIKE afi_ctr_det_op13.cod_op_det05 ,     
         dia_proc05     LIKE afi_ctr_det_op13.dia_proc05   , 
         cod_op_det06   LIKE afi_ctr_det_op13.cod_op_det06 ,     
         dia_proc06     LIKE afi_ctr_det_op13.dia_proc06   , 
         cod_op_det07   LIKE afi_ctr_det_op13.cod_op_det07 ,     
         dia_proc07     LIKE afi_ctr_det_op13.dia_proc07   , 
         tipo           LIKE afi_ctr_det_op13.tipo         ,     
         motivo         LIKE afi_ctr_det_op13.motivo       ,     
         tipo_id        LIKE afi_ctr_det_op13.tipo_id    
    END RECORD

    DEFINE rdesc_motivo char (60),
        vcont        SMALLINT

    OUTPUT
        PAGE LENGTH 99
        TOP  MARGIN 0
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 0

    FORMAT
    PAGE HEADER
    LET vcont = 0
        PRINT COLUMN 1,"n_seguro"       ,"|",
                       "n_folio "      ,"|",
                       "tipo_solicitud","|",
                       "paterno"       ,"|",
                       "materno"       ,"|",
                       "nombres"       ,"|",
                       "cod_op_det03"  ,"|",
                       "dia_proc03"    ,"|",
                       "cod_op_det04"  ,"|",
                       "dia_proc04"    ,"|",
                       "cod_op_det05"  ,"|",
                       "dia_proc05"    ,"|",
                       "cod_op_det06"  ,"|",
                       "dia_proc06"    ,"|",
                       "cod_op_det07"  ,"|",
                       "dia_proc07"    ,"|",
                       "tipo"          ,"|",
                       "motivo"        ,"|",
                       "tipo_id"       ,"|",
                       "desc_motivo"   ,"|"


    ON EVERY ROW
        LET vcont = vcont + 1
        PRINT COLUMN 1, rpt.n_seguro      ,"|",
                        rpt.n_folio       ,"|",
                        rpt.tipo_solicitud,"|",
                        rpt.paterno       ,"|",
                        rpt.materno       ,"|",
                        rpt.nombres       ,"|",
                        --des_rechazo ,"|",
                        rpt.cod_op_det03  ,"|",
                        rpt.dia_proc03    ,"|",
                        rpt.cod_op_det04  ,"|",
                        rpt.dia_proc04    ,"|",
                        rpt.cod_op_det05  ,"|",
                        rpt.dia_proc05    ,"|",
                        rpt.cod_op_det06  ,"|",
                        rpt.dia_proc06    ,"|",
                        rpt.cod_op_det07  ,"|",
                        rpt.dia_proc07    ,"|",
                        rpt.tipo          ,"|",
                        rpt.motivo        ,"|", 
                        rpt.tipo_id       ,"|",
                        rdesc_motivo      ,"|"

    ON LAST ROW 
       PRINT COLUMN 1,  "TOTAL REG", vcont

END REPORT

FUNCTION lista_acep_exp()
#-------------------

     DEFINE hora          CHAR(8)
     DEFINE HOY           DATE
     DEFINE vcod_rechazo  CHAR(8)
     DEFINE vcont         INTEGER

     DEFINE gr_curp RECORD
         n_seguro    LIKE afi_mae_afiliado.n_seguro,
         n_folio     LIKE afi_mae_afiliado.n_folio,
         tipo_solicitud CHAR(1),
         paterno     CHAR(13),
         materno     CHAR(13),
         nombres     CHAR(16),
         des_rechazo CHAR(49),
         cod_rechazo LIKE afi_rechaza_proc.cod_rechazo
     END RECORD


     DEFINE G_IMPRIME       CHAR(200)

    DEFINE resp   CHAR(1)

    LET hora = TIME
    LET HOY = TODAY

    LET G_LISTA_ACE =  g_paramgrales.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED,".ACEP_EXP.",
                   HOY USING "ddmmyy","_",hora[1,2],
                   hora[4,5],hora[7,8] CLIPPED

    START REPORT listado TO G_LISTA_ACE

    DECLARE c_cargae CURSOR FOR
        SELECT c.n_seguro, 
               c.n_folio, 
               c.tipo_solicitud,
               c.paterno, 
               c.materno, 
               c.nombres, 
               a.rdeta_desc_c,
               b.diag_proc[1,3]
        FROM   safre_tmp:det_ref b,
               safre_af:afi_mae_afiliado c,
        OUTER  (safre_af:tab_rdeta a)
        WHERE  c.n_seguro = b.nss_solicitud
        --AND    c.n_unico  = b.curp_solicitud
        AND    b.diag_proc[1,3] = a.rdeta_cod
        AND    b.cod_op = "01"
        AND    a.modulo_cod    = "afi"
        ORDER BY 3,1

    FOREACH c_cargae INTO gr_curp.*
        OUTPUT TO REPORT listado(gr_curp.*)
    END FOREACH 

    FINISH REPORT listado


{
    LET G_LISTA_ACE =  "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED, ".ACEP_EXP.",
                   HOY USING "ddmmyy","_",hora[1,2],
                   hora[4,5],hora[7,8] CLIPPED

    RUN G_LISTA_ACE

    LET G_IMPRIME = g_paramgrales.ruta_listados CLIPPED,"/",
                    g_usuario CLIPPED,".RECH_MOD.",
                    HOY USING "ddmmyy","_",hora[1,2],
                    hora[4,5],hora[7,8] CLIPPED

    RUN G_IMPRIME
}
END FUNCTION

---<CUO13
REPORT listado(rpt)

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
        vcont        SMALLINT

    OUTPUT
        PAGE LENGTH 90
        TOP  MARGIN 0
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 0

    FORMAT
    
    PAGE HEADER
    LET vcont = 0
        PRINT COLUMN 1, "n_seguro"       ,"|",
                        "n_folio"        ,"|",
                        "tipo_solicitud" ,"|",
                        "paterno"        ,"|",
                        "materno"        ,"|",
                        "nombres"        ,"|",
                        "des_rechazo"    ,"|",
                        "cod_rechazo"    ,"|"

    
    ON EVERY ROW
        LET vcont = vcont + 1
        PRINT COLUMN 1, rpt.n_seguro      ,"|",
                        rpt.n_folio       ,"|",
                        rpt.tipo_solicitud,"|",
                        rpt.paterno       ,"|",
                        rpt.materno       ,"|",
                        rpt.nombres       ,"|",
                        rpt.des_rechazo   ,"|",
                        rpt.cod_rechazo   ,"|"

    ON LAST ROW 
       PRINT COLUMN 1,  "TOTAL REG", vcont
    {
    ORDER EXTERNAL BY rpt.cod_rechazo

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
            COLUMN 140,"FECHA   :",HOY USING "DD/MM/YY"    
            --COLUMN 149,hora [1,5]
        PRINT
            COLUMN 001,"AFIL018"                                            ,
            COLUMN 035," E S T A D O   D E   M O D I F I C A D O S  R E C H A Z A D O S   P O R   P R O C E S A R",
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
            COLUMN 001, "Numero de Seguro",
            COLUMN 020, "Numero de Folio",
            COLUMN 038, "Ap. Paterno",
            COLUMN 054, "Ap. Materno",
            COLUMN 070, "Nombres",
            COLUMN 088, "Desc. Rechazo",
            COLUMN 148, "Codigo Rechazo"
       PRINT
            COLUMN 001,"========================================" ,
            COLUMN 040,"========================================" ,
            COLUMN 080,"========================================" ,
            COLUMN 120,"========================================" ,
            COLUMN 160,"====="

    ON EVERY ROW
       LET vcont = vcont + 1
       
       PRINT
            COLUMN 001, rpt.n_seguro,
            COLUMN 015, rpt.n_folio,
            COLUMN 038, rpt.paterno,
            COLUMN 054, rpt.materno,
            COLUMN 070, rpt.nombres,
            COLUMN 083, rpt.des_rechazo,
            COLUMN 157, rpt.cod_rechazo

    AFTER GROUP OF rpt.cod_rechazo
       SKIP 1 LINES 
       PRINT
            COLUMN 145, "================"
       PRINT
            COLUMN 125, "TOTAL POR CODIGO DE RECHAZO :", GROUP COUNT(*) USING "#####"
       SKIP 2 LINES

     #----------
     ON LAST ROW
     #----------

    SELECT count(*)
    INTO   vcodigo_39
    FROM   afi_rechaza_proc, tab_rdeta
    WHERE  afi_rechaza_proc.cod_rechazo[1,3] = tab_rdeta.rdeta_cod
    AND    tab_rdeta.modulo_cod = 'afi'

       SKIP 1 LINES 
       PRINT
            COLUMN 145, "================"
       PRINT
            COLUMN 125, "TOTAL GENERAL CODIGO DE RECHAZO :", vcont USING "#####"
       SKIP 2 LINES
       PRINT
            COLUMN 100, "TOTAL AFILIADOS CON FALTA DE DOC. PROBATORIO (39):",
                         vcodigo_39   USING "######"
    }

END REPORT

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
    DISPLAY " AFIC004                 CONSULTA REGISTROS PROCESADOS                        " AT 3,1
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
        fecha_proceso  LIKE afi_ctr_arh_proc.fecha_proceso
    END RECORD

    DEFINE
        vcount     ,
        vtotal     ,
        vaprobados ,
        vrechazos  ,
        vtot_curp  INTEGER

    DEFINE
        pos        SMALLINT

    SELECT COUNT(*)
    INTO   vcount
    FROM   afi_ctr_arh_proc

    SELECT SUM(@total_reg)
    INTO   vtotal
    FROM   afi_ctr_arh_proc

    SELECT SUM(total_aprob)
    INTO   vaprobados
    FROM   afi_ctr_arh_proc

    SELECT SUM(total_rech)
    INTO   vrechazos
    FROM   afi_ctr_arh_proc

    DISPLAY "                                                                               " AT 1,1
    DISPLAY "    CTRL-C cancela                                                             " AT 2,1
    DISPLAY " CONSULTA " AT 2,65

    DECLARE curp_12 CURSOR FOR
    SELECT @nombre_archivo, @total_reg, @total_aprob, @total_rech,
           @fecha_proceso
    FROM afi_ctr_arh_proc
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
        ERROR "ARCHIVO DE PROCESAR VACIO"
    END IF

END FUNCTION

FUNCTION desmarca_cuenta(vnss, vmarca, vusuario, vn_folio)
#dc----------------------------

   DEFINE vnss          CHAR(11),
          vmarca        SMALLINT,
          vusuario      CHAR(08),
          vn_folio      DECIMAL(10,0),
          pestado_marca SMALLINT,
          pmarca_causa  SMALLINT,
          vcorrelativo  INTEGER

    LET pestado_marca = 0
    LET pmarca_causa  = 0

    #Mod 24/jun/2004
    SELECT d.correlativo
    INTO   vcorrelativo
    FROM   cta_act_marca d
    WHERE  d.nss       = vnss
    AND    d.marca_cod = 600
    GROUP BY 1

    PREPARE eje_desmarca FROM v_desmarca

    EXECUTE eje_desmarca
    USING vnss,
          vmarca,
          vcorrelativo,
          pestado_marca,
          pmarca_causa,
          vusuario

END FUNCTION

FUNCTION marca_cuenta(vnss, vmarca_entra, vmarca_edo, vcodigo_rech, vusuario,
                      vn_folio)
#mc--------------------

   DEFINE vnss          CHAR(11),
          vmarca_entra  SMALLINT,
          vmarca_edo    SMALLINT,
          vcodigo_rech  SMALLINT,
          vusuario      CHAR(08),
          vn_folio      DECIMAL(10,0)

   DEFINE pmarca_causa  SMALLINT,
          pfecha_causa  DATE 

   LET vmarca_entra = 605
   LET vmarca_edo   = 0
   LET vcodigo_rech = 0
   LET pmarca_causa = 0
   LET pfecha_causa = ""

   PREPARE eje_marca FROM v_marca
   DECLARE cur_marca CURSOR FOR eje_marca

   OPEN cur_marca USING vnss         ,
                        vmarca_entra ,
                        vn_folio     ,
                        vmarca_edo   ,
                        vcodigo_rech ,
                        pmarca_causa ,
                        pfecha_causa ,
                        vusuario

   FETCH cur_marca INTO xcodigo_marca,
                        xcodigo_rechazo

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

{FUNCTION valida_folio(vrow)
#vf--------------------

    DEFINE 
        tipo_sol   SMALLINT,
        vrow       INTEGER ,
        vfolio_ant DECIMAL(8,0),
        vfolio_nvo DECIMAL(8,0)

    SELECT m.n_folio, m.folio_nvo, m.tipo_solicitud
    INTO   vfolio_ant, vfolio_nvo, tipo_sol
    FROM   afi_mae_modifica m
    WHERE  m.n_seguro = vn_seguro
    AND    m.fecha_modifica = reg_mod.fecha_modifica
    AND    m.rowid = vrow

    IF vfolio_ant <> vfolio_nvo THEN
        UPDATE afi_mae_afiliado
        SET    n_folio = vfolio_nvo
        WHERE  n_seguro = vn_seguro
        AND    n_folio  = vfolio_ant
        AND    tipo_solicitud = tipo_sol

        UPDATE afi_solicitud
        SET    n_folio = vfolio_nvo
        WHERE  n_seguro = vn_seguro
        AND    n_folio  = vfolio_ant
        AND    tipo_solicitud = tipo_sol

        UPDATE afi_patron
        SET    n_folio = vfolio_nvo
        WHERE  n_seguro = vn_seguro
        AND    n_folio  = vfolio_ant
        AND    tipo_solicitud = tipo_sol

        UPDATE afi_domicilio
        SET    n_folio = vfolio_nvo
        WHERE  nss     = vn_seguro
        AND    n_folio = vfolio_ant
        AND    tipo_solicitud = tipo_sol

        UPDATE afi_telefono
        SET    n_folio = vfolio_nvo
        WHERE  nss     = vn_seguro
        AND    n_folio = vfolio_ant
        AND    tipo_solicitud = tipo_sol

        UPDATE afi_icefa
        SET    n_folio = vfolio_nvo
        WHERE  n_seguro = vn_seguro
        AND    n_folio  = vfolio_ant
        AND    tipo_solicitud = tipo_sol

        UPDATE afi_beneficiario
        SET    n_folio = vfolio_nvo
        WHERE  n_seguro = vn_seguro
        AND    n_folio  = vfolio_ant
        AND    tipo_solicitud = tipo_sol

        UPDATE afi_mae_modifica
        SET    n_folio = vfolio_nvo,
               folio_nvo = vfolio_ant
        WHERE  n_seguro = vn_seguro
        AND    n_folio  = vfolio_ant
        AND    tipo_solicitud = tipo_sol

        WHENEVER ERROR CONTINUE

        UPDATE afi_recepcion
        SET    n_folio = vfolio_nvo
        WHERE  n_seguro = vn_seguro
        AND    n_folio  = vfolio_ant
        AND    tipo_solicitud = tipo_sol

        UPDATE afi_expediente
        SET    n_folio = vfolio_nvo
        WHERE  n_seguro = vn_seguro
        AND    n_folio  = vfolio_ant
        AND    tipo_solicitud = tipo_sol

        UPDATE afi_condicion_exp
        SET    n_folio = vfolio_nvo
        WHERE  n_folio  = vfolio_ant
        AND    tipo_solicitud = tipo_sol

        WHENEVER ERROR STOP
    END IF

END FUNCTION}

FUNCTION borra_detalles()
   DELETE FROM afi_ctr_det_op13
   WHERE fecha_cert = vfechacert
   AND id_lote = campo_27
   AND usuario = g_usuario
   AND factualiza = HOY
END FUNCTION

