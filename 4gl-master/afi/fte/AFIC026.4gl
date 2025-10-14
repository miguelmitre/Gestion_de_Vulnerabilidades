#############################################################################
#Proyecto          => AFORES ( MEXICO )                                     #
#Propietario       => E.F.P.                                                #
#Programa AFIC026  => CARGA DE ARCHIVO MODIFICACION DE DATOS                #
#Sistema           => AFI                                                   #
#Autor             => EDUARDO JOAQUIN RESENDIZ MEDINA                       #
#Fecha             => 10 DE ABRIL DE 2006                                   #
#Actualizacion     => EDUARDO JOAQUIN RESENDIZ MEDINA                       #
#Fecha             => 24 JUNIO 2009 NO AILIADOS ISSSTE                      #
#Modifico          => Eduardo Joaquin Resnendiz Medina CPL-1406             #
#Modifico          => Eduardo Joaquin Resnendiz Medina CPL-1634             #
#############################################################################
#COPPEL fn_regimen_inv
DATABASE safre_af

GLOBALS

    DEFINE reg_mod       RECORD LIKE afi_mae_modifica.*
    DEFINE reg_mod_op54  RECORD LIKE afi_ctr_det_op54.*
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
        vnseguro            CHAR(11),
        varchivo            CHAR(14),
        vcurp               CHAR(18),
        vcurp_solicitud     CHAR(18),
        vvcurp              CHAR(18),
        generar             CHAR(20),
        vdesc_cod_39        CHAR(30),
        operacion           CHAR(40),
        carga               CHAR(51),
        vnombre             CHAR(50),
        corr                CHAR(100),
        vn_folio          DECIMAL(10,0),
        vconsecutivo        SMALLINT

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
        vrechazados2     ,
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
        vnom_afore      CHAR(122),
        vnom_proce      CHAR(122),
        v_marca         CHAR(100),
        v_desmarca      CHAR(100)

    DEFINE
        reg_carta          RECORD LIKE safre_af:int_ctr_carta.*,
        consulta_carta     CHAR(120)

    DEFINE vafore  CHAR(3)
    DEFINE v_sql_1 CHAR(50)
    DEFINE v_sql_2 CHAR(50)
    DEFINE
        vdeleg_cod       INTEGER,
        v_estado         SMALLINT,
        v_ciudad         SMALLINT



END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG(FGL_GETENV('USER')||'.AFIC026.log')
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
    LET edo_proc = 605    ----borrar edo_proc
    LET generar  = "S"

    LET vmarca_estado   = 0
    LET vcodigo_rechazo = 0

    SELECT *, USER
    INTO   g_paramgrales.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    CREATE TEMP TABLE plano_procesar
        (n_registros CHAR(500))

    LET v_sql_1 = "EXECUTE PROCEDURE fn_fnacimiento(?,?)"
    LET v_sql_2 = "EXECUTE PROCEDURE fn_regimen_inv(?,?,?,?,?,?)"

    LET v_marca    = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? )"

    LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"

END FUNCTION

FUNCTION crea_tablas()
#ct-------------------

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp

        DROP TABLE cza_mod_ind
        DROP TABLE det_mod_ind
        DROP TABLE sum_mod_ind
    WHENEVER ERROR STOP

    CREATE TABLE cza_mod_ind
        (campo1  CHAR(2),
         campo2  CHAR(2),
         campo3  CHAR(2),
         campo4  CHAR(2),
         campo5  CHAR(3),
         campo6  CHAR(2),
         campo7  CHAR(3),
         campo8  CHAR(8),
         campo9  CHAR(3),
         campo10 CHAR(8),
         campo11 CHAR(1),
         campo12 CHAR(9))

     CREATE TABLE det_mod_ind
        (tipo_registro       CHAR(02),
         contador_servicio   CHAR(10),
         clave_operacion     CHAR(02),
         id_centro_pago      CHAR(07),          ---op53-54
         rfc_trabajador      CHAR(13),
         curp_solicitud      CHAR(18),          ---op53-54
         curp_nueva          CHAR(18),          ---op53-54
         nsi                 CHAR(11),          ---op53-54
         paterno             CHAR(40),          ---op53-54
         materno             CHAR(40),          ---op53-54
         nombres             CHAR(40),          ---op53-54
         fena                CHAR(08),          ---op53-54
         estadon             CHAR(02),          ---op53-54
         sexo                CHAR(01),          ---op53-54
         edocivil            CHAR(01),          ---op53-54
         status_afore        CHAR(02),          ---op53-54
         cod_resul_op        CHAR(02),
         diag_proc           CHAR(15),
         femision            CHAR(08),
         --rfc_bd              CHAR(13),        ---op53-54
         fecha_pri_reg       CHAR(08),
         fecha_alta_actua    CHAR(08),
         clave_afore_reg     CHAR(03),
         fecha_recep         CHAR(08),
         hora_recep          CHAR(08),
         consc               CHAR(08));

    CREATE TABLE sum_mod_ind
        (campo1 CHAR(2),
         campo2 CHAR(9),
         campo3 CHAR(9),
         campo4 CHAR(9),                    ---op53-54
         campo5 CHAR(9))                    ---op53-54

    DATABASE safre_af

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIC0041" ATTRIBUTE(BORDER)
    DISPLAY " AFIC026    CARGA DE ARCHIVO RESPUESTA MODIF NO AFILIADOS                          " AT 3,1 ATTRIBUTE(REVERSE)
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
        INTO   varchivo
        FROM   afi_ctr_arh_proc
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
        LET carga = g_paramgrales.ruta_rescate CLIPPED,"/", generar CLIPPED

        WHENEVER ERROR CONTINUE
            LOAD FROM carga DELIMITER ","
            INSERT INTO plano_procesar
        WHENEVER ERROR STOP

        SELECT COUNT(*)
        INTO   g_plano_procesar
        FROM   plano_procesar

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
    CALL Actualiza_op54()
    CALL Actualiza_Maeafili() #am
    CALL lista_err()          #le

END FUNCTION

FUNCTION actualiza_datos()
#-------------------------

    DEFINE
        cont_reg      INTEGER

    DEFINE
        carga_reg     CHAR(300)

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
        campo_110     CHAR(08),
        campo_111     CHAR(01),
        campo_112     CHAR(09),

        campo_01      CHAR(02),
        campo_02      CHAR(10),
        campo_03      CHAR(02),
        campo_04      CHAR(07),          ---op53-54
        campo_05      CHAR(13),
        campo_06      CHAR(18),          ---op53-54
        campo_07      CHAR(18),          ---op53-54
        campo_08      CHAR(11),          ---op53-54
        campo_09      CHAR(40),
        campo_10      CHAR(40),
        campo_11      CHAR(40),
        campo_12      CHAR(08),         ---op53-54
        campo_13      CHAR(02),         ---op53-54
        campo_14      CHAR(01),         ---op53-54
        campo_15      CHAR(01),         ---op53-54
        campo_16      CHAR(02),         ---op53-54
        campo_17      CHAR(02),
        campo_18      CHAR(15),
        campo_19      CHAR(08),
        campo_20      CHAR(08),
        campo_21      CHAR(08),
        campo_22      CHAR(03),
        campo_23      CHAR(08),
        campo_24      CHAR(08),
        campo_25      CHAR(08),

        campo_201     CHAR(2),
        campo_202     CHAR(9),
        campo_203     CHAR(9),
        campo_204     CHAR(9),
        campo_205     CHAR(9)


    LET cont_reg = 0

    SELECT COUNT(*)
    INTO   total_reg
    FROM   plano_procesar

    DECLARE cursor_1 CURSOR FOR
    SELECT  *
    FROM    plano_procesar

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
            LET campo_110 = carga_reg[028,035]
            LET campo_111 = carga_reg[036,036]
            LET campo_112 = carga_reg[037,045]

            INSERT INTO safre_tmp:cza_mod_ind
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
                    campo_112)
        END IF

        IF cont_reg <> total_reg AND cont_reg <> 1 THEN
            LET campo_01 = carga_reg[001,002]
            LET campo_02 = carga_reg[003,012]
            LET campo_03 = carga_reg[013,014]
            LET campo_04 = carga_reg[015,021]
            LET campo_05 = carga_reg[022,034]
            LET campo_06 = carga_reg[035,052]
            LET campo_07 = carga_reg[053,070]
            LET campo_08 = carga_reg[071,081]
            LET campo_09 = carga_reg[082,121]
            LET campo_10 = carga_reg[122,161]
            LET campo_11 = carga_reg[162,201]
            LET campo_12 = carga_reg[202,209]
            LET campo_13 = carga_reg[210,211]
            LET campo_14 = carga_reg[212,212]
            LET campo_15 = carga_reg[213,213]
            LET campo_16 = carga_reg[214,215]
            LET campo_17 = carga_reg[216,217]
            LET campo_18 = carga_reg[218,232]
            LET campo_19 = carga_reg[233,240]
            LET campo_20 = carga_reg[241,248]
            LET campo_21 = carga_reg[249,256]
            LET campo_22 = carga_reg[257,259]
            LET campo_23 = carga_reg[260,267]
            LET campo_24 = carga_reg[268,275]
            LET campo_25 = carga_reg[276,283]

        INSERT INTO safre_tmp:det_mod_ind
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
                campo_25)
        END IF

        IF cont_reg = total_reg        THEN
            LET campo_201 = carga_reg[001,002]
            LET campo_202 = carga_reg[003,011]
            LET campo_203 = carga_reg[012,020]
            LET campo_204 = carga_reg[021,029]
            LET campo_205 = carga_reg[030,038]

            INSERT INTO safre_tmp:sum_mod_ind
            VALUES (campo_201,
                    campo_202,
                    campo_203,
                    campo_204,
                    campo_205)
        END IF

    END FOREACH

END FUNCTION

FUNCTION revisa_datos()
#----------------------

    DEFINE
        rechazo_lote CHAR(9),
        rechazo_deta CHAR(3),
        l_reg        RECORD LIKE tab_rch_lote.*,
        x_reg        RECORD LIKE tab_rdeta.*,
        aux_pausa    CHAR(1)


    DEFINE
        rechazo_09  CHAR(02),
        rechazo_001 CHAR(02),
        rechazo_002 CHAR(02),
        rechazo_003 CHAR(02)

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
           campo12
    INTO   rechazo_001,
           rechazo_002,
           rechazo_003,
           rechazo_lote
    FROM safre_tmp:cza_mod_ind

    SELECT *
    INTO   l_reg.*
    FROM   tab_rch_lote
    WHERE  rlote_cod = rechazo_lote

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

    IF rechazo_003 <> "53" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Identificador de operacion debe ser 53 en encabezado"
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Identificador de Operacion debe ser 53 en ENCABEZADO" AT 10,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

     # SUMARIO #

    SELECT campo1
    INTO   rechazo_09
    FROM   safre_tmp:sum_mod_ind

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

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
FUNCTION Actualiza_op54()
#----------------------------

    OPEN WINDOW w_curp AT 8,4 WITH FORM "AFIC0082"
    ATTRIBUTE (BORDER)

    LET vhoy = TODAY

    SELECT COUNT(*)
    INTO   vtotal
    FROM   safre_tmp:det_mod_ind

    SELECT campo5
    INTO   vafore
    FROM safre_tmp:cza_mod_ind

    DECLARE c_curp2 CURSOR FOR
    SELECT b.n_seguro,
           b.curp_modificado,
           b.n_folio,
           b.consecutivo,
           a.cod_resul_op,
           a.diag_proc
    FROM   safre_tmp:det_mod_ind a, safre_af:afi_ctr_det_op54 b
    WHERE  a.curp_solicitud = b.curp_ant
    AND    b.status_interno IN  (130,131)
    --AND    a.cod_resul_op  = '01'
    ORDER BY a.curp_solicitud

    FOREACH c_curp2 INTO vn_seguro,
                        vcurp,
                        vn_folio,
                        vconsecutivo,
                        vcod_operacion,
                        vdiag_proceso

        IF vn_seguro IS NOT NULL OR
           vn_seguro[1,1] <> " " THEN

           LET marca = 600
           CALL desmarca_cuenta(vn_seguro,marca,g_usuario,vn_folio)
        END IF

        CASE vcod_operacion
            WHEN '02'
                LET vrechazados = vrechazados + 1
{
                IF vdiag_proceso = '399' THEN
                    CALL actualiza_001()
                    CALL despliega_totales()
                ELSE}
                    CALL actualiza_002()
                    CALL despliega_totales()
--                END IF
            WHEN '01'
                LET vaprobados = vaprobados + 1

                CALL actualiza_001()
                CALL despliega_totales()

            WHEN '03'
                LET vpendientes = vpendientes + 1

                CALL actualiza_001()
                CALL despliega_totales()

            WHEN '80'
                LET vaprobados = vaprobados + 1

                CALL actualiza_001()
                CALL despliega_totales()

            WHEN '83'
                LET vaprobados = vaprobados + 1

                CALL actualiza_001()
                CALL despliega_totales()
        END CASE
    END FOREACH

     -- actualiza historico de archivos procesar afi_ctr_arh_proc
{
    INSERT INTO afi_ctr_arh_proc
    VALUES (generar, vtotal, vaprobados, vrechazados, vpendientes, vhoy)

    CALL despliega_totales()

    IF NOT bnd_proceso THEN
        ERROR ""
        PROMPT "Proceso finalizado satisfactoriamente, [Enter] Para Continuar "
        ATTRIBUTE (REVERSE)
        FOR vresp ATTRIBUTE (REVERSE)
}
        CLOSE WINDOW w_curp
    --ELSE
        --DISPLAY "Programa finalizado satisfactoriamente"
    --END IF

    RETURN

END FUNCTION

FUNCTION actualiza_002()
#a02-------------------

    UPDATE afi_mae_afiliado
    SET    status_interno = 200
    WHERE  n_unico        = vcurp   ----por curp
    AND    status_interno IN(130,240,300)

    UPDATE afi_ctr_det_op54
    SET    cod_result_op    = vcod_operacion,
           motivo_rechazo1  = vdiag_proceso,
           status_interno   = 200
    WHERE  --n_seguro       = vn_seguro
           curp_modificado = vcurp
    AND    status_interno  IN (130,131)
    AND    consecutivo     = vconsecutivo
    --AND    motivo_rechazo1 = 000
{
    INSERT INTO afi_rechaza_proc
    VALUES (vn_seguro, vdiag_proceso, vhoy, generar)
}
END FUNCTION

FUNCTION actualiza_001()
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

    DEFINE vn_folio_tes   INTEGER
    DEFINE vn_folio_tes2  INTEGER
    DEFINE vcuenta_tes    SMALLINT
    DEFINE vrowid         INTEGER
    DEFINE conta_tes      SMALLINT

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
    INITIALIZE vnseguro TO NULL

    PREPARE stmt1 FROM v_sql_1
    PREPARE stmt2 FROM v_sql_2

    LET marca2 = 610

    SELECT *, rowid
    INTO   reg_mod_op54.*, vrow
    FROM   afi_ctr_det_op54
    WHERE  --n_seguro       = vn_seguro
           curp_modificado  = vcurp
    AND    status_interno   = 130
    AND    consecutivo      = vconsecutivo

    SELECT 'X'
    FROM   afi_mae_afiliado 
    WHERE  @n_unico        = reg_mod_op54.curp_ant
    AND    @n_folio        = reg_mod_op54.n_folio
    AND    @tipo_solicitud = reg_mod_op54.tipo_solicitud

    IF SQLCA.SQLCODE = 0 THEN

       SELECT a.fena,a.n_seguro
       INTO   reg_ant.fena,vnseguro
       FROM   afi_mae_afiliado a
       WHERE  a.n_unico        = reg_mod_op54.curp_ant
       AND    a.n_folio        = reg_mod_op54.n_folio
       AND    a.tipo_solicitud = reg_mod_op54.tipo_solicitud
   
   ---en base al lay-out a excepecion  de fechas
       UPDATE afi_mae_afiliado
       SET    paterno        = reg_mod_op54.paterno_modificado,
              materno        = reg_mod_op54.materno_modificado,
              nombres        = reg_mod_op54.nombres_modificado,
              n_unico        = reg_mod_OP54.curp_modificado,
              n_rfc          = reg_mod_op54.rfc_modificado    ,
              usuario        = reg_mod_op54.usuario           ,
              fena           = reg_mod_op54.fena_modificado   ,         ---op53-54
              estadon        = reg_mod_op54.estadon_modificado,         ---op53-54
              sexo           = reg_mod_op54.sexo_modificado   ,         ---op53-54
              edo_civil      = reg_mod_op54.edo_civil_modificado,       ---op53-54
              status_interno = 200                                      ---op53-54
       WHERE  --n_seguro       = vn_seguro
              n_unico        = reg_mod_op54.curp_ant
       AND    n_folio        = reg_mod_op54.n_folio
       AND    tipo_solicitud = reg_mod_op54.tipo_solicitud
       AND    status_interno in(130,160)

       UPDATE cta_ctr_reg_ind
       SET    curp       = reg_mod_op54.curp_modificado,
              nss_issste = reg_mod_op54.nsi_modificado
       WHERE  curp       = reg_mod_op54.curp_ant

    ELSE

       SELECT a.fena,a.n_seguro
       INTO   reg_ant.fena,vnseguro
       FROM   afi_mae_afiliado a
       WHERE  a.n_unico        = reg_mod_op54.curp_modificado
       AND    a.n_folio        = reg_mod_op54.n_folio
       AND    a.tipo_solicitud = reg_mod_op54.tipo_solicitud

   ---en base al lay-out a excepecion  de fechas
       UPDATE afi_mae_afiliado
       SET    paterno        = reg_mod_op54.paterno_modificado,
              materno        = reg_mod_op54.materno_modificado,
              nombres        = reg_mod_op54.nombres_modificado,
              n_unico        = reg_mod_OP54.curp_modificado,
              n_rfc          = reg_mod_op54.rfc_modificado    ,
              usuario        = reg_mod_op54.usuario           ,
              fena           = reg_mod_op54.fena_modificado   ,         ---op53-54
              estadon        = reg_mod_op54.estadon_modificado,         ---op53-54
              sexo           = reg_mod_op54.sexo_modificado   ,         ---op53-54
              edo_civil      = reg_mod_op54.edo_civil_modificado,       ---op53-54
              status_interno = 200                                      ---op53-54
       WHERE  --n_seguro       = vn_seguro
              n_unico        = reg_mod_op54.curp_modificado
       AND    n_folio        = reg_mod_op54.n_folio
       AND    tipo_solicitud = reg_mod_op54.tipo_solicitud
       AND    status_interno in(130,160)

       UPDATE cta_ctr_reg_ind
       SET    curp       = reg_mod_op54.curp_modificado,
              nss_issste = reg_mod_op54.nsi_modificado
       WHERE  curp       = reg_mod_op54.curp_modificado

    END IF

    UPDATE afi_mae_afiliado
    SET    status_interno = 200
    WHERE  --n_seguro        = vn_seguro
           n_unico        = reg_mod_op54.curp_modificado
    AND    status_interno in (130,240,300)

    UPDATE afi_ctr_det_op54
    SET    cod_result_op   = vcod_operacion,
           motivo_rechazo1 = vdiag_proceso,
           status_interno  = 200
    WHERE  --n_seguro       = vn_seguro
           curp_modificado = vcurp
    AND    status_interno  = 130
    AND    consecutivo     = vconsecutivo
    --AND    motivo_rechazo1 = 000

--->actualiza domicilio
{    SELECT estad_cod,deleg_cod,ciudad_cod
    INTO   v_estado,vdeleg_cod,v_ciudad
    FROM   tab_codpos
    WHERE  cpos_cod = reg_mod_op54.cpos_cod_modificado

    IF reg_mod_op54.domicilio_modificado IS NOT NULL OR
       reg_mod_op54.domicilio_modificado NOT MATCHES "[ *]" THEN
        UPDATE afi_domicilio
        SET    calle          = reg_mod_op54.domicilio_modificado,
               numero         = '',
               depto          = '',
               colonia        = reg_mod_op54.colonia_modificado,
               delega         = vdeleg_cod,
               codpos         = reg_mod_op54.cpos_cod_modificado,
               estado         = v_estado,
               ciudad         = v_ciudad,
               usuario        = g_usuario,
               factualiza     = TODAY
        WHERE  nss            = reg_mod_op54.n_seguro
        AND    n_folio        = reg_mod_op54.n_folio
        AND    tipo_solicitud = reg_mod_op54.tipo_solicitud
        AND    marca_envio    = 'X'
    END IF}
---<

--->actualiza regimen de inv
    IF reg_mod_op54.curp_ant[5,10] <> reg_mod_op54.curp_modificado[5,10] OR
       reg_ant.fena          <> reg_mod_op54.fena_modificado THEN

       LET v_crea_fecha = HOY
       LET v_tipo_trasp = 15    #MODIFICACION DATOS CURP
       LET v_tipo_proc  = 1
       LET v_medio      = 10

       DECLARE curs1 CURSOR FOR stmt1
       OPEN  curs1 USING reg_mod_op54.n_seguro, v_crea_fecha
       FETCH curs1 INTO v_existe, v_edad, v_criterio, v_ind_edad,
                        v_curp, v_rfc, v_fena
       CLOSE curs1
--->aplica validacion por normativa de edad del trabajador a partir de CPL-1634
       DISPLAY "Edad del trabajador: ",    v_edad
       #busca si regimen de trabajador de la subcuenta 1 este en la siefore 1 
       SELECT 'X'
       FROM   cta_regimen
       WHERE  nss            = reg_mod_op54.n_seguro
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

       IF ban_regimen_edad = 1 OR   #banderas encendidas por regimen 1 fuera de rango de edad
          ban_regimen_edad = 2 THEN #banderas encendidas por no estar en regimen 1
---<
           DECLARE curs2 CURSOR FOR stmt2
           OPEN curs2 USING reg_mod_op54.n_seguro,
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
           WHERE nss = reg_mod_op54.n_seguro
           AND   tipo_traspaso = 15
           AND   fecha_solicitud = TODAY

           IF vcuenta_tes > 1 THEN
              LET conta_tes = 0

              DECLARE curs_3 CURSOR FOR 
              SELECT folio_solicitud,ROWID
              FROM   tes_solicitud
              WHERE  nss             = reg_mod_op54.n_seguro
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
                         VALUES ( reg_mod_op54.n_seguro  ,
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
                       WHERE   nss     = reg_mod_op54.n_seguro
                       AND     fecha_solicitud = TODAY
                       AND     tipo_proceso    = v_tipo_trasp
                    END IF

                    UPDATE tes_solicitud 
                    SET    folio_solicitud = vn_folio_tes2
                    WHERE  nss             = reg_mod_op54.n_seguro
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
                         VALUES ( reg_mod_op54.n_seguro  ,
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
                       WHERE   nss     = reg_mod_op54.n_seguro
                       AND     fecha_solicitud = TODAY
                       AND     tipo_proceso    = v_tipo_trasp
                    END IF

                    UPDATE tes_solicitud 
                    SET    folio_solicitud = vn_folio_tes2
                    WHERE  nss             = reg_mod_op54.n_seguro
                    AND    tipo_traspaso   = 15
                    AND    fecha_solicitud = TODAY
                    AND    ROWID           = vrowid

                 END IF

                 LET vcuenta_tes   = 0
                 --LET vn_folio_tes  = 0
                 --LET vn_folio_tes2 = 0
                 LET vrowid        = 0
              END FOREACH
--->>>MLM-2433 Y CPL-1634
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
         VALUES (reg_mod_op54.n_seguro,v_rechazo)
       END IF
    END IF
---<

--->actualiza tablas PENISS
    IF vafore = '578' THEN
      UPDATE cta_bono_issste
      SET curp      = reg_mod_OP54.curp_modificado
      WHERE curp    = reg_mod_op54.curp_ant

      UPDATE afi_icefa_issste
      SET    curp           = reg_mod_OP54.curp_modificado   ,
             rfc            = reg_mod_op54.rfc_modificado    ,
             paterno        = reg_mod_op54.paterno_modificado,
             materno        = reg_mod_op54.materno_modificado,
             nombres        = reg_mod_op54.nombres_modificado,
             fnacimiento    = reg_mod_op54.fena_modificado   ,
             nss_issste     = reg_mod_op54.nsi_modificado    ,
             usuario        = reg_mod_op54.usuario           ,
             factualiza     = TODAY
       WHERE  nti            = vnseguro

         SELECT 'X'
         FROM   dis_hist_apor_bono
         WHERE  n_unico = reg_mod_op54.curp_ant
         GROUP BY 1

         IF SQLCA.SQLCODE <> NOTFOUND THEN
            UPDATE dis_hist_apor_bono 
            SET n_unico           = reg_mod_OP54.curp_modificado
            WHERE n_unico         = reg_mod_op54.curp_ant
         ELSE
            UPDATE dis_hist_apor_bono 
            SET n_unico           = reg_mod_OP54.curp_modificado
            WHERE n_unico         = reg_mod_op54.curp_modificado
         END IF

    END IF
---<actualiza tablas PENISS

    CALL desmarca_cuenta(reg_mod_op54.n_seguro,marca2,g_usuario,vn_folio)
    #END IF

END FUNCTION

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

FUNCTION Actualiza_Maeafili()
#----------------------------

    OPEN WINDOW w_curp AT 8,4 WITH FORM "AFIC0082"
    ATTRIBUTE (BORDER)

    LET vhoy = TODAY

    SELECT COUNT(*)
    INTO   vtotal
    FROM   safre_tmp:det_mod_ind
{
    IF vrechazados =  0 THEN
    ELSE
      LET vrechazados = vrechazados - 1
    END IF
}
    DECLARE c_curp CURSOR FOR
    SELECT b.n_seguro,
           b.n_unico,
           b.n_folio,
           a.diag_proc[1,3],
           a.cod_resul_op
    FROM   safre_tmp:det_mod_ind a, afi_mae_modifica b
    --WHERE  a.curp_solicitud = b.n_unico
    WHERE  a.curp_nueva     = b.n_unico
    AND    b.status_interno = 130
    AND    b.cod_operacion  = 0
    AND    b.diag_proceso   = 0
    ORDER BY a.curp_solicitud

    FOREACH c_curp INTO vn_seguro,
                        vcurp,
                        vn_folio,
                        vdiag_proceso,
                        vcod_operacion

        LET marca = 600

        CALL desmarca_cuenta(vn_seguro,marca,g_usuario,vn_folio)

        CASE vcod_operacion
            WHEN '02'
                LET vrechazados = vrechazados + 1
{
                IF vdiag_proceso = '399' THEN
                    CALL actualiza_01()
                    CALL despliega_totales()
                ELSE}
                    CALL actualiza_02()
                    CALL despliega_totales()
--                END IF
            WHEN '01'
                LET vaprobados = vaprobados + 1

                CALL actualiza_01()
                CALL despliega_totales()

            WHEN '03'
                LET vpendientes = vpendientes + 1

                CALL actualiza_01()
                CALL despliega_totales()
        END CASE
    END FOREACH

     -- actualiza historico de archivos procesar afi_ctr_arh_proc

    INSERT INTO afi_ctr_arh_proc
    VALUES (generar, vtotal, vaprobados, vrechazados, vpendientes, vhoy)

    UPDATE afi_ctr_cza_op54
    SET    status_interno = 160
    WHERE  status_interno = 130

    CALL despliega_totales()

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

        UPDATE afi_mae_afiliado
        SET    status_interno = 200
        WHERE  n_unico       = vcurp   ----por curp
        AND    status_interno in(130,240,300)

        UPDATE afi_mae_modifica
        SET    cod_operacion  = vcod_operacion,
               diag_proceso   = vdiag_proceso,
               status_interno = 200
        WHERE  n_unico       = vcurp   ---por curp
        AND    cod_operacion  = 0
        AND    diag_proceso   = 0

        UPDATE afi_ctr_curp
        SET    cod_operacion    = vcod_operacion,
               diag_proceso     = vdiag_proceso,
               fecha_respuesta  = TODAY
        WHERE  nss              = vn_seguro
        AND    cve_operacion    = '53'
        AND    fecha_respuesta IS NULL


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
    DEFINE vnom_mod CHAR(120)
    DEFINE vnom_bd  CHAR(120)

    DEFINE vn_folio_tes   INTEGER
    DEFINE vn_folio_tes2  INTEGER
    DEFINE vcuenta_tes    SMALLINT
    DEFINE vrowid         INTEGER
    DEFINE conta_tes      SMALLINT

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

    PREPARE stmt11 FROM v_sql_1
    PREPARE stmt21 FROM v_sql_2

    LET marca2 = 610

    SELECT *, rowid
    INTO   reg_mod.*, vrow
    FROM   afi_mae_modifica
    WHERE  n_seguro       = vn_seguro
           --n_unico        = vcurp
    AND    cod_operacion  = 0
    AND    diag_proceso   = 0
    AND    status_interno = 130


---modificar en base al lay out
    SELECT a.paterno,
           a.materno,
           a.nombres,
           a.n_rfc,
           a.n_unico,
           a.fena,                           ---op53-54
           a.estadon,                        ---op53-54
           a.sexo,                           ---op53-54
           a.edo_civil                       ---op53-54
    INTO   reg_ant.paterno,
           reg_ant.materno,
           reg_ant.nombres,
           reg_ant.n_rfc,
           reg_ant.n_unico,
           reg_ant.fena,                      ---op53-54
           reg_ant.estadon,                    ---op53-54
           reg_ant.sexo,                      ---op53-54
           reg_ant.edo_civil                  ---op53-54
    FROM   afi_mae_afiliado a
    WHERE  a.n_seguro = vn_seguro
           --a.n_unico  = vcurp

---en base al lay-out a excepecion  de fechas
    UPDATE afi_mae_afiliado
    SET    paterno            = reg_mod.paterno      ,
           materno            = reg_mod.materno      ,
           nombres            = reg_mod.nombres      ,
           n_unico            = reg_mod.n_unico      ,
           n_rfc              = reg_mod.n_rfc        ,
           usuario            = reg_mod.usuario      ,
           fena               = reg_mod.fena         ,         ---op53-54
           estadon            = reg_mod.estadon      ,         ---op53-54
           sexo               = reg_mod.sexo         ,         ---op53-54
           edo_civil          = reg_mod.edo_civil    ,         ---op53-54
           status_interno     = 200
    WHERE  n_seguro           = vn_seguro
           --n_unico            = vcurp
    AND    n_folio            = reg_mod.n_folio
    AND    tipo_solicitud     = reg_mod.tipo_solicitud
    AND    status_interno in(130,160)

    UPDATE afi_mae_modifica
    SET    paterno            = reg_ant.paterno  ,
           materno            = reg_ant.materno  ,
           nombres            = reg_ant.nombres  ,
           n_unico            = reg_ant.n_unico  ,
           n_rfc              = reg_ant.n_rfc    ,
           sexo               = reg_ant.sexo     ,
           fena               = reg_ant.fena     ,
           estadon            = reg_ant.estadon  ,
           edo_civil          = reg_ant.edo_civil,
           cod_operacion      = vcod_operacion   ,
           diag_proceso       = vdiag_proceso    ,
           status_interno     = 200
    WHERE  n_seguro       = vn_seguro
           --n_unico        = vcurp
    AND    cod_operacion  = 0
    AND    diag_proceso   = 0
    AND    status_interno = 130

    UPDATE afi_ctr_curp
    SET    cod_operacion    = vcod_operacion,
           diag_proceso     = vdiag_proceso,
           fecha_respuesta  = TODAY
    WHERE  nss              = vn_seguro
    AND    cve_operacion    = '53'
    AND    fecha_respuesta IS NULL

--->actualiza regimen de inv
    IF reg_ant.n_unico[5,10] <> reg_mod.n_unico[5,10] OR
       reg_ant.fena    <> reg_mod.fena    THEN

       LET v_crea_fecha = HOY
       LET v_tipo_trasp = 15    #MODIFICACION DATOS CURP
       LET v_tipo_proc  = 1
       LET v_medio      = 10

       DECLARE curs3 CURSOR FOR stmt11
       OPEN  curs3 USING vn_seguro, v_crea_fecha
       FETCH curs3 INTO v_existe, v_edad, v_criterio, v_ind_edad,
                        v_curp, v_rfc, v_fena
       CLOSE curs3
--->valida edad       DISPLAY "Edad del trabajador: ",    v_edad
       DISPLAY "Edad del trabajador: ",    v_edad
       #busca si regimen de trabajador de la subcuenta 1 este en la siefore 1 
       SELECT 'X'
       FROM   cta_regimen
       WHERE  nss            = reg_mod.n_seguro
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

       IF ban_regimen_edad = 1 OR   #banderas encendidas por regimen 1 fuera de rango de edad
          ban_regimen_edad = 2 THEN #banderas encendidas por no estar en regimen 1
--<
           DECLARE curs4 CURSOR FOR stmt21
           OPEN curs4 USING vn_seguro,
                            v_ind_edad,
                            v_ind_edad,
                            v_tipo_proc,
                            v_tipo_trasp,
                            v_medio
--->cacha y despliega error 
          WHENEVER ERROR CONTINUE
          FETCH curs4 INTO v_existe, v_ind_edad, v_rechazo, v_folioatencion
             IF SQLCA.SQLCODE < 0 THEN              
                LET errmsg = err_get(SQLCA.SQLCODE)
                ERROR errmsg 
                PROMPT "No puede continuar, Enter para continuar " for enter
                EXIT PROGRAM
             END IF
          WHENEVER ERROR STOP
---<
           CLOSE curs4
    
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

              DECLARE curs_31 CURSOR FOR 
              SELECT folio_solicitud,ROWID
              FROM   tes_solicitud
              WHERE  nss             = vn_seguro
              AND    tipo_traspaso   = 15
              AND    fecha_solicitud = TODAY

              FOREACH curs_31 INTO vn_folio_tes,vrowid

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
                    WHERE nss              = vn_seguro
                    AND tipo_traspaso      = 15
                    AND fecha_solicitud    = TODAY
                    AND ROWID              = vrowid

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
--->>>MLM-2433 Y CPL-1634
                DECLARE cur_marca3 CURSOR FOR
                   SELECT marca_cod,correlativo,ROWID
                   FROM   cta_act_marca
                   WHERE  nss         =  vn_seguro
                   AND    marca_cod   IN (301,302,303,304,307,308,309,310,311)
                   AND    fecha_ini   =  TODAY
                   AND    correlativo =  vn_folio_tes
                   ORDER  by marca_cod

                   FOREACH cur_marca3 INTO   vmarca_cod,vcorr,vrowid2
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

--->actualiza tablas PENISS
    IF vafore = '578' THEN
      UPDATE cta_bono_issste
      SET curp      = reg_mod.n_unico
      WHERE curp    = vcurp

      UPDATE afi_icefa_issste
      SET    curp           = reg_mod.n_unico,
             rfc            = reg_mod.n_rfc  ,
             paterno        = reg_mod.paterno,
             materno        = reg_mod.materno,
             nombres        = reg_mod.nombres,
             fnacimiento    = reg_mod.fena   ,
             usuario        = g_usuario      ,
             factualiza     = TODAY
      WHERE  curp           = vcurp
    END IF
---<actualiza tablas PENISS

    CALL desmarca_cuenta(vn_seguro,marca2,g_usuario,vn_folio)
    #END IF

END FUNCTION

FUNCTION despliega_totales()
#dt-------------------------

     IF NOT bnd_proceso THEN
        DISPLAY "                  DATOS A PROCESAR                 "
            AT 8,1 ATTRIBUTE ( REVERSE )

        DISPLAY "Total de Registros del lote : ",
                 vtotal USING "#######&" AT 9,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros actualizados      : ",
                 vaprobados USING "#######&" AT 10,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros rechazados        : ",
                 vrechazados USING "#######&" AT 11,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros pendientes        : ",
                 vpendientes USING "#######&" AT 12,15 ATTRIBUTE ( BOLD )

    ELSE
        DISPLAY "                  DATOS A PROCESAR                 "

        DISPLAY "Total de Registros del lote  : ",
                 vtotal USING "#######&"

        DISPLAY "Registros actualizados       : ",
                 vaprobados USING "#######&"

        DISPLAY "Registros rechazados         : ",
                 vrechazados USING "#######&"

        DISPLAY "Registros pendientes         : ",
                 vpendientes USING "#######&"
    END IF

    --IF NOT bnd_proceso THEN
        --PROMPT "Presione [Enter] para continuar" FOR enter
    --END IF

END FUNCTION

FUNCTION lista_err()
#-------------------

     DEFINE hora   CHAR(8)
     DEFINE HOY                  DATE
     DEFINE vcod_rechazo  CHAR(8)
     DEFINE vcont   INTEGER

     DEFINE gr_curp RECORD
         n_unico    LIKE afi_mae_afiliado.n_unico,
         --n_folio     LIKE afi_mae_afiliado.n_folio,
         tipo_solicitud        CHAR(1),
         paterno     CHAR(13),
         materno     CHAR(13),
         nombres     CHAR(16),
         des_rechazo CHAR(49),
         cod_rechazo LIKE afi_rechaza_proc.cod_rechazo
     END RECORD

     DEFINE G_LISTA     CHAR(200)
     DEFINE G_IMPRIME     CHAR(200)

    DEFINE resp   CHAR(1)

    LET hora = TIME
    LET HOY = TODAY

    LET G_LISTA =  g_paramgrales.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED,".RECHAZOS_PROC_IND.",
                   HOY USING "dd-mm-yy","_",hora CLIPPED

    START REPORT listado TO G_LISTA

    DECLARE c_carga CURSOR FOR
        SELECT --b.nss_solicitud,    -----curp
               b.curp_solicitud,
               c.tipo_solicitud,
               b.paterno,
               b.materno,
               b.nombres,
               a.rdeta_desc_c,
               b.diag_proc[1,3]
        FROM   safre_af:tab_rdeta a,
               safre_tmp:det_mod_ind b,
               safre_af:afi_mae_afiliado c
        WHERE  --c.n_seguro = b.nss_solicitud    ---curp
               c.n_unico           = b.curp_solicitud
        AND    b.diag_proc[1,3]    = a.rdeta_cod
        AND    b.cod_resul_op      = "02"
        AND    a.modulo_cod        = 'afi'
        ORDER BY 2,1

    FOREACH c_carga INTO gr_curp.*
        OUTPUT TO REPORT listado(gr_curp.*)
    END FOREACH

    FINISH REPORT listado

    ERROR "LISTADO GENERADO" SLEEP 2

    LET G_LISTA =  "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED, ".RECHAZOS_PROC_IND.",
                   HOY USING "dd-mm-yy","_",hora CLIPPED

    RUN G_LISTA
{
    LET G_IMPRIME = "lp ",g_paramgrales.ruta_listados CLIPPED,"/",
                    g_usuario CLIPPED,".RECHAZOS_PROC_IND.",
                    HOY USING "dd-mm-yy","_",hora CLIPPED

    RUN G_IMPRIME
}
END FUNCTION

REPORT listado(rpt)
#l-----------------

    DEFINE rpt RECORD
        n_curp      LIKE afi_mae_afiliado.n_unico,
        --n_folio     LIKE afi_mae_afiliado.n_folio,
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
        PAGE LENGTH 90
        TOP  MARGIN 0
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 0

    --------ADECUARLO PARA IMPRESION

{    FORMAT
    ON EVERY ROW

        PRINT COLUMN 1, rpt.*
--    ORDER EXTERNAL BY rpt.cod_rechazo
}
    FORMAT
    PAGE HEADER

    PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 001,"========================================"            ,
            COLUMN 040,"========================================"     ,
            COLUMN 080,"========================================"     ,
            COLUMN 120,"========================================"     ,
            COLUMN 160,"====="
        PRINT
            COLUMN 001,razon_social                                     ,
            COLUMN 140,"FECHA :",HOY USING "DD/MM/YYYY"
            --COLUMN 149,hora [1,5]
        PRINT
            COLUMN 001,"AFIC026"                                     ,
            COLUMN 035," E S T A D O   D E   M O D I F I C A D O S  R E C H A Z A D O S   P O R   P R O C E S A R",
            COLUMN 140,"NUM.PAG.:",pageno USING "####,###"
            SKIP 1 LINE
        PRINT
            COLUMN 001,"----------------------------------------"     ,
            COLUMN 040,"----------------------------------------"     ,
            COLUMN 080,"----------------------------------------"     ,
            COLUMN 120,"----------------------------------------"     ,
            COLUMN 160,"-----"
       SKIP 1 LINES
       PRINT
            COLUMN 001, "    Curp",
            COLUMN 020, "Tipo Solicitud",
            COLUMN 038, "Ap. Paterno",
            COLUMN 054, "Ap. Materno",
            COLUMN 070, "Nombres",
            COLUMN 088, "Desc. Rechazo",
            COLUMN 148,        "Codigo Rechazo"
       PRINT
            COLUMN 001,"========================================" ,
            COLUMN 040,"========================================" ,
            COLUMN 080,"========================================" ,
            COLUMN 120,"========================================" ,
            COLUMN 160,"====="

    ON EVERY ROW
       LET vcont = vcont + 1

       PRINT
            COLUMN 001, rpt.n_curp,
            COLUMN 025, rpt.tipo_solicitud,
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
            COLUMN 125, "TOTAL POR CODIGO DE RECHAZO     :", GROUP COUNT(*) USING "#####"
       SKIP 2 LINES


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
        vn_folio          DECIMAL(10,0),
        pestado_marca SMALLINT,
        pmarca_causa  SMALLINT,
        vcorrelativo  INTEGER

    LET pestado_marca = 0
    LET pmarca_causa  = 0

    SELECT d.correlativo
    INTO   vcorrelativo
    FROM   cta_act_marca d
    WHERE  d.nss = vnss
    AND    d.marca_cod = vmarca

    PREPARE eje_desmarca FROM v_desmarca

    EXECUTE eje_desmarca
    USING vnss,
          vmarca,
          vcorrelativo,
          pestado_marca,
          pmarca_causa,
          vusuario

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
        vn_folio          DECIMAL(10,0)

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

    OPEN cur_marca

    USING vnss        ,
          vmarca_entra,
          vn_folio    ,
          vmarca_edo  ,
          vcodigo_rech,
          pmarca_causa,
          pfecha_causa,
          vusuario

    FETCH cur_marca
    INTO  xcodigo_marca, xcodigo_rechazo

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

#################################################################
FUNCTION det_carta() #dc

   LET reg_carta.nss            = vn_seguro 
   LET reg_carta.n_folio        = vn_folio
   LET reg_carta.tipo_solicitud = 1
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

