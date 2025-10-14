#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa AFIM011  => MANTENIMIENTO DE AFILIADOS (SOLO DATOS DE PROCESAR)   #
#Fecha actualiz    => 19 DE MAYO DE 2000.                                   #
#Por               => MAURO MUNIZ CABALLERO, modificados(historico)         #
#Fecha modifica    => 04 de diciembre de 2001.                              #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ.                      #
#Sistema           => AFI.                                                  #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE
        sw_carta ,
        sw_cod   ,
        sw_1     ,
        digito   SMALLINT

    DEFINE
        c_pat     CHAR(1),
        c_mat     CHAR(1),
        c_nom     CHAR(1),
        c_fen     CHAR(1),
        c_doc     CHAR(1),
        enter     CHAR(1),
        aaa       ,
        mm        ,
        dd        CHAR(2),
        z_fecha   CHAR(10),
        xx_fecha  CHAR(10)

    DEFINE
        j_fecha   DATE,
        HOY       DATE

    DEFINE
        aux_pausa CHAR(1),
        ACCION    CHAR(1),
        pat       CHAR(40),
        mat       CHAR(40),
        nom       CHAR(40),
        x_fecha   CHAR(10),
        comando   CHAR(250),
        param     CHAR(193)

    DEFINE g_master RECORD
        codven             LIKE afi_mae_afiliado.codven,
        nip                LIKE pro_mae_promotor.nip,
        desc_codven        CHAR(60),
        agenc_cod          CHAR(10),
        agenc_desc         CHAR(60),
        tipo_solicitud     LIKE afi_mae_afiliado.tipo_solicitud,
        fecha_elaboracion  LIKE afi_mae_afiliado.fecha_elaboracion,
        folio_edo_cta      CHAR(8),
        cod_afore_ced      SMALLINT,
        desc_afore         CHAR(50),
        fecha_emision      DATE,
        n_folio            LIKE afi_mae_afiliado.n_folio,
        n_operac           INTEGER,
        frecafor           LIKE afi_mae_afiliado.frecafor,
        paterno            CHAR(40),
        materno            CHAR(40),
        nombres            CHAR(40),
        n_seguro           LIKE afi_mae_afiliado.n_seguro,
        n_rfc              LIKE afi_mae_afiliado.n_rfc,
        n_unico            LIKE afi_mae_afiliado.n_unico,
        sexo               LIKE afi_mae_afiliado.sexo,
        desc_sexo          CHAR(60),
        edo_civil          SMALLINT,
        desc_edo_civil     CHAR(60),
        fena               LIKE afi_mae_afiliado.fena,
        salario_base_comis LIKE afi_mae_afiliado.salario_base_comis,
        estadon            LIKE afi_mae_afiliado.estadon,
        desc_estadon       CHAR(60),
        nacionalidad       LIKE afi_mae_afiliado.nacionalidad,
        desc_nacionalidad  CHAR(60),
        tip_prob	   LIKE afi_mae_afiliado.tip_prob,
        fol_prob           LIKE afi_mae_afiliado.fol_prob,
        doc_prob           LIKE afi_mae_afiliado.doc_prob,
        docprob_desc       LIKE tab_doc_prob.docprob_desc,
        ind_infonavit      LIKE afi_mae_afiliado.ind_infonavit,
        cod_error_origen   LIKE afi_mae_afiliado.cod_error_origen,
        const_curp         SMALLINT
    END RECORD

    DEFINE g_master1 RECORD
        codven             LIKE afi_mae_afiliado.codven,
        nip                LIKE pro_mae_promotor.nip,
        desc_codven        CHAR(60),
        agenc_cod          CHAR(10),
        agenc_desc         CHAR(60),
        tipo_solicitud     LIKE afi_mae_afiliado.tipo_solicitud,
        fecha_elaboracion  LIKE afi_mae_afiliado.fecha_elaboracion,
        folio_edo_cta      CHAR(8),
        cod_afore_ced      SMALLINT,
        desc_afore         CHAR(50),
        fecha_emision      DATE,
        n_folio            LIKE afi_mae_afiliado.n_folio,
        n_operac           INTEGER,
        frecafor           LIKE afi_mae_afiliado.frecafor,
        paterno            CHAR(40),
        materno            CHAR(40),
        nombres            CHAR(40),
        n_seguro           LIKE afi_mae_afiliado.n_seguro,
        n_rfc              LIKE afi_mae_afiliado.n_rfc,
        n_unico            LIKE afi_mae_afiliado.n_unico,
        sexo               LIKE afi_mae_afiliado.sexo,
        desc_sexo          CHAR(60),
        edo_civil          SMALLINT,
        desc_edo_civil     CHAR(60),
        fena               LIKE afi_mae_afiliado.fena,
        salario_base_comis LIKE afi_mae_afiliado.salario_base_comis,
        estadon            LIKE afi_mae_afiliado.estadon,
        desc_estadon       CHAR(60),
        nacionalidad       LIKE afi_mae_afiliado.nacionalidad,
        desc_nacionalidad  CHAR(60),
        tip_prob	   LIKE afi_mae_afiliado.tip_prob,
        fol_prob           LIKE afi_mae_afiliado.fol_prob,
        doc_prob           LIKE afi_mae_afiliado.doc_prob,
        docprob_desc       LIKE tab_doc_prob.docprob_desc,
        ind_infonavit      LIKE afi_mae_afiliado.ind_infonavit,
        cod_error_origen   LIKE afi_mae_afiliado.cod_error_origen,
        const_curp         SMALLINT
    END RECORD

    DEFINE g_master2 ARRAY [100] OF RECORD 
        codven             LIKE afi_mae_afiliado.codven,
        nip                LIKE pro_mae_promotor.nip,
        desc_codven        CHAR(60),
        agenc_cod          CHAR(10),
        agenc_desc         CHAR(60),
        tipo_solicitud     LIKE afi_mae_afiliado.tipo_solicitud,
        fecha_modifica     DATE,
        fecha_elaboracion  LIKE afi_mae_afiliado.fecha_elaboracion,
        folio_edo_cta      CHAR(8),
        cod_afore_ced      SMALLINT,
        desc_afore         CHAR(50),
        fecha_emision      DATE,
        n_folio            LIKE afi_mae_afiliado.n_folio,
        n_operac           INTEGER,
        frecafor           LIKE afi_mae_afiliado.frecafor,
        paterno            CHAR(40),
        materno            CHAR(40),
        nombres            CHAR(40),
        n_seguro           LIKE afi_mae_afiliado.n_seguro,
        n_rfc              LIKE afi_mae_afiliado.n_rfc,
        n_unico            LIKE afi_mae_afiliado.n_unico,
        sexo               LIKE afi_mae_afiliado.sexo,
        desc_sexo          CHAR(60),
        edo_civil          SMALLINT,
        desc_edo_civil     CHAR(60),
        fena               LIKE afi_mae_afiliado.fena,
        salario_base_comis LIKE afi_mae_afiliado.salario_base_comis,
        estadon            LIKE afi_mae_afiliado.estadon,
        desc_estadon       CHAR(60),
        nacionalidad       LIKE afi_mae_afiliado.nacionalidad,
        desc_nacionalidad  CHAR(60),
        tip_prob           LIKE afi_mae_afiliado.tip_prob,
        docprob_desc       LIKE tab_doc_prob.docprob_desc,
        fol_prob           LIKE afi_mae_afiliado.fol_prob,
        doc_prob           LIKE afi_mae_afiliado.doc_prob,
        ind_infonavit      LIKE afi_mae_afiliado.ind_infonavit,
        cod_error_origen   LIKE afi_mae_afiliado.cod_error_origen,
        const_curp         SMALLINT
    END RECORD

    DEFINE g_afore RECORD LIKE tab_afore_local.*
    DEFINE r_cod   RECORD LIKE tab_supervisor.*

    DEFINE
        cod_error CHAR(4),
        g_usuario CHAR(8),
        g_hora    CHAR(8),
        COMMA     CHAR(200)

    DEFINE
        g_status_captura ,
        HAY_DATOS1       ,
        HAY_DATOS2       ,
        tot_error        ,
        paterno_2        ,
        materno_3        ,
        nombre_4         ,
        sexo_5           ,
        macim_6          ,
        entidad_7        ,
        nacion_8         ,
        cve_prob_9       ,
        doc_prob_10      ,
        fol_prob_11      ,
        cve_asig_12      ,
        su_estatus       ,
        reclamo          ,
        pos              ,
        vstatmod         ,
        vstatusint       SMALLINT

    DEFINE
        vcont INTEGER

    DEFINE
        opc             CHAR(1)  ,
        vnss            CHAR(11) ,
        vmarca_entra    SMALLINT ,
        vmarca_estado   SMALLINT ,
        vcodigo_rechazo SMALLINT ,
        ejecuta         CHAR(300),
        xcodigo_marca   SMALLINT ,
        xcodigo_rechazo SMALLINT ,
        edo_proc        SMALLINT ,
        su_st_int       SMALLINT ,
        operacion       CHAR(40)

END GLOBALS

MAIN

    LET g_master.n_seguro = ARG_VAL(1)
    LET g_master.n_folio  = ARG_VAL(2)
    LET reclamo           = ARG_VAL(3)

    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY CONTROL-I,
        COMMENT LINE LAST

    CALL STARTLOG("AFIM011.log")
    CALL inicio()
    CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE tmp_solo_afili
    WHENEVER ERROR STOP

    CREATE TABLE tmp_solo_afili
        (n_seguro CHAR(11),
         n_unico  CHAR(18),
         n_rfc    CHAR(13),
         paterno  CHAR(40),
         materno  CHAR(40),
         nombres  CHAR(40),
         fena     DATE,
         accion   CHAR(1))

    DATABASE safre_af

    SELECT *,USER 
    INTO   g_afore.*,g_usuario 
    FROM   tab_afore_local

    LET HOY = TODAY

    LET edo_proc        = 600
    LET vmarca_estado   = 0
    LET vcodigo_rechazo = 0

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    DEFINE bnd_mod CHAR(1)

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIM0111" ATTRIBUTE(BORDER)
    DISPLAY " AFIM011                 MANTENIMIENTO  AFILIADOS                              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                               " AT 6,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                               " AT 14,1 ATTRIBUTE(REVERSE)
    DISPLAY "               Nombre del Trabajador segun Documento Probatorio                " AT 10,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "AFILIADOS"
        COMMAND "Consulta" "Consulta de Afiliados"
            LET ACCION = "C"
            CALL Consulta()
            CALL Inicializa()
        COMMAND "Modifica" "Modificacion Datos"
            LET ACCION = "M"
            --CALL Ingresa_autoriza()
            --IF aux_pausa = "N" THEN
                WHILE TRUE
                PROMPT "(A) Certificables, (B) No certificables, (C) Salir : "
                ATTRIBUTES (REVERSE) FOR bnd_mod
                IF bnd_mod MATCHES'[AaBbCc]' THEN
                    IF bnd_mod MATCHES '[Aa]' THEN
                        CALL Modifica()
                        CALL Inicializa()
                    END IF
                    IF bnd_mod MATCHES '[Bb]' THEN
                        CALL Modifica_no_cert()
                        CALL Inicializa()
                    END IF
                    IF bnd_mod MATCHES '[Cc]' THEN
                        EXIT WHILE
                        RETURN
                    END IF
                ELSE
                    ERROR "Solo debe presionar (A) Certificables, (B) No certificables, (C) Salir "
                    SLEEP 1
                    ERROR ""
                END IF
                END WHILE
            --END IF
        COMMAND "Otros datos" "Otros datos de Afiliados"
            LET ACCION = "M"
            CALL Consulta()
            CALL Inicializa()
        COMMAND "Despliega" "Despliegue de Afiliados"
            LET ACCION = "D"
            CALL Inicializa()
            CALL Despliega() 
        COMMAND "Salir" "Salir de Programa"
            EXIT MENU
    END MENU

END FUNCTION

FUNCTION Inicializa()

    LET vstatmod = 0
    INITIALIZE g_master.* TO NULL
    DISPLAY "                                                                               " AT 6,1 ATTRIBUTE(REVERSE)

    CLEAR FORM

END FUNCTION

FUNCTION Numero_operacion_asignado()

    DEFINE x_numerii   INTEGER

    SELECT n_operac 
    INTO   x_numerii 
    FROM   afi_mae_afiliado
    WHERE  n_folio = g_master.n_folio

    DISPLAY "                                                                                                 " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                                                 " AT 2,1 ATTRIBUTE(REVERSE)
    PROMPT "Num. Operacion Asignado ",x_numerii USING "########"," comuniquelo al promotor y presione ENTER" ATTRIBUTE(REVERSE) FOR aux_pausa

END FUNCTION

FUNCTION Inserta_en_tabla_maeafili()
#itm--------------------------------

    LET g_hora = TIME

    SELECT "X"
    FROM   afi_mae_afiliado
    WHERE  n_seguro = g_master.n_seguro
    AND    n_folio  = g_master.n_folio
    AND    tipo_solicitud = g_master.tipo_solicitud

    IF STATUS = NOTFOUND THEN
        INSERT INTO afi_mae_afiliado
        VALUES(g_master.n_seguro,
               g_master.n_unico,
               g_master.n_rfc,
               g_master.paterno,
               g_master.materno,
               g_master.nombres,
               g_master.fena,
               g_master.n_folio,
               g_master.edo_civil,
               "",
               g_master.estadon,
               0,
               g_master.codven,
               g_master.sexo,
               "",
               "",
               "",
               "",
               "",
               "",
               "",
               "",
               "",
               g_master.n_operac,
               #g_master.entidad_r_civil,
               g_master.frecafor,
               "",
               "",
               "",
               "",
               0,
               g_master.agenc_cod,
               0,
               g_master.nacionalidad,
               g_master.tip_prob,
               g_master.fol_prob,
               g_master.doc_prob,
               g_master.ind_infonavit,
               "",
               "",
               "",
               "",
               "",
               "",
               "",
               "",
               "",
               g_usuario,
               g_hora,
               10,
               g_master.tipo_solicitud,
               g_master.fecha_elaboracion,
               0,
               "")
    ELSE
        UPDATE afi_mae_afiliado
        SET    paterno           = g_master.paterno           ,
               materno           = g_master.materno           ,
               nombres           = g_master.nombres           ,
               status_interno    = 120                        ,
               n_rfc             = g_master.n_rfc             ,
               sexo              = g_master.sexo              ,
               edo_civil         = g_master.edo_civil         ,
               fena              = g_master.fena              ,
               salario_base_comis= g_master.salario_base_comis,
               estadon           = g_master.estadon           ,
               nacionalidad      = g_master.nacionalidad      ,
               tip_prob          = g_master.tip_prob          ,
               fol_prob          = g_master.fol_prob          ,
               doc_prob          = g_master.doc_prob          ,
               ind_infonavit     = g_master.ind_infonavit     ,
               status_captura    = 20                         ,
               indicador_b       = g_master.const_curp        ,
               cod_error_origen  = g_master.cod_error_origen  ,
               usuario           = g_usuario
        WHERE  n_folio           = g_master.n_folio
        AND    tipo_solicitud    = g_master.tipo_solicitud
    END IF

END FUNCTION

FUNCTION Consulta()
#C-----------------

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " CONSULTA " AT 1,69 ATTRIBUTE(REVERSE)
    DISPLAY " CTRL : [b] Despliega [v] Otros datos [t] Modificaciones             " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY " CTRL : [o] Rechazos Procesar  [u] Dispersion Renapo  [c] Salir      " AT 2,1 ATTRIBUTE(BOLD)

    IF g_master.n_seguro IS NULL THEN
        CALL Inicializa()
        LET g_master.tipo_solicitud = NULL
    END IF

    INPUT BY NAME g_master.tipo_solicitud,
                  g_master.n_folio,
                  g_master.n_seguro WITHOUT DEFAULTS

    AFTER FIELD tipo_solicitud
        IF g_master.tipo_solicitud IS NULL OR
           g_master.tipo_solicitud < 1 OR
           g_master.tipo_solicitud > 5 THEN
            NEXT FIELD n_seguro
        END IF

    AFTER FIELD n_folio
        IF g_master.n_folio IS NULL THEN
            NEXT FIELD n_seguro
        ELSE
            IF NOT Rescata_datos("N",
                                 g_master.n_folio,
                                 g_master.tipo_solicitud) THEN
                ERROR "Afiliado NO existe"
                NEXT FIELD n_folio
            END IF

            DISPLAY BY NAME g_master.*
        END IF

    AFTER FIELD n_seguro
        IF g_master.n_seguro IS NULL THEN
            NEXT FIELD n_folio
        ELSE
            IF NOT Rescata_datos("C",g_master.n_seguro,0) THEN
                ERROR "Afiliado NO existe"
                NEXT FIELD n_seguro
            END IF

            DISPLAY BY NAME g_master.*
        END IF

    ON KEY ( CONTROL-O )
        CALL motivo_rechazo()

    ON KEY ( CONTROL-U )
        CALL motivo_renapo()

    ON KEY ( CONTROL-T )
        SELECT "X"
        FROM   afi_mae_modifica md
        WHERE  md.n_seguro = g_master.n_seguro
        AND    md.n_folio = g_master.n_folio
        AND    md.tipo_solicitud = g_master.tipo_solicitud
        GROUP BY 1

        IF SQLCA.SQLCODE = 0 THEN
            CALL verifica_datos_modificados()
        ELSE
            PROMPT "Registro sin modificaciones, [Enter] p/continuar " 
            ATTRIBUTES (REVERSE)
            FOR enter
        END IF

    ON KEY ( CONTROL-B )
        CALL Despliega()
        DISPLAY BY NAME g_master.n_folio

    ON KEY ( INTERRUPT )
        CALL Inicializa()
        DISPLAY "                                                                               " AT 6,1 ATTRIBUTE(REVERSE)
        DISPLAY "                                                                               " AT 14,1 ATTRIBUTE(REVERSE)
        DISPLAY "                  " AT 1,69 

        EXIT INPUT

    ON KEY ( CONTROL-V )
        IF g_master.n_seguro IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD n_seguro
        END IF

        LET COMMA = "fglgo AFIM012.4gi ",
                    g_master.n_folio,
                    " ",
                    ACCION,
                    " ",
                    vcont," ",
                    g_master.tipo_solicitud

        RUN COMMA 

    END INPUT

END FUNCTION

FUNCTION Rescata_datos(l_aux_val,x_valor,x_tipo)
#rd---------------------------------------------

    DEFINE l_aux_val CHAR(1)
    DEFINE x_valor   CHAR(11)
    DEFINE x_tipo    SMALLINT
    DEFINE xx_num    DECIMAL(11,0)

    CASE l_aux_val
        WHEN "C"
            SELECT cod_promotor,
                   agenc_cod,
                   n_folio,
                   n_operac,
                   frecafor,
                   paterno,
                   materno,
                   nombres,
                   n_seguro,
                   n_unico,
                   n_rfc,
                   sexo,
                   edo_civil,
                   fena,
                   salario_base_comis,
                   estadon,
                   status_interno,
                   tip_prob,
                   fol_prob,
                   doc_prob,
                   ind_infonavit,
                   nacionalidad,
                   tipo_solicitud,
                   fecha_elaboracion,
                   ' ', --cod_error_origen,
                   folio_edo_cta,
                   cod_afore_ced,
                   const_curp,
                   femision
            INTO   g_master.codven,
                   g_master.agenc_cod,
                   g_master.n_folio,
                   g_master.n_operac,
                   g_master.frecafor,
                   g_master.paterno,
                   g_master.materno,
                   g_master.nombres,
                   g_master.n_seguro,
                   g_master.n_unico,
                   g_master.n_rfc,
                   g_master.sexo,
                   g_master.edo_civil,
                   g_master.fena,
                   g_master.salario_base_comis,
                   g_master.estadon,
                   su_estatus,
                   g_master.tip_prob,
                   g_master.fol_prob,
                   g_master.doc_prob,
                   g_master.ind_infonavit,
                   g_master.nacionalidad,
                   g_master.tipo_solicitud,
                   g_master.fecha_elaboracion,
                   g_master.cod_error_origen,
                   g_master.folio_edo_cta,
                   g_master.cod_afore_ced,
                   g_master.const_curp,
                   g_master.fecha_emision
            FROM   afi_mae_afiliado
            WHERE  n_seguro =  x_valor

            IF SQLCA.SQLCODE <> 0 THEN
                RETURN FALSE
            END IF

            WHEN "N"
                LET xx_num = x_valor
                SELECT cod_promotor,
                       agenc_cod,
                       n_folio,
                       n_operac,
                       frecafor,
                       paterno,
                       materno,
                       nombres,
                       n_seguro,
                       n_unico,
                       n_rfc,
                       sexo,
                       edo_civil,
                       fena,
                       salario_base_comis,
                       estadon,
                       status_interno,
                       tip_prob,
                       fol_prob,
                       doc_prob,
                       ind_infonavit,
                       nacionalidad,
                       tipo_solicitud,
                       fecha_elaboracion,
                       ' ',  --cod_error_origen,
                       folio_edo_cta,
                       cod_afore_ced,
                       const_curp,
                       femision
                INTO   g_master.codven,
                       g_master.agenc_cod,
                       g_master.n_folio,
                       g_master.n_operac,
                       g_master.frecafor,
                       g_master.paterno,
                       g_master.materno,
                       g_master.nombres,
                       g_master.n_seguro,
                       g_master.n_unico,
                       g_master.n_rfc,
                       g_master.sexo,
                       g_master.edo_civil,
                       g_master.fena,
                       g_master.salario_base_comis,
                       g_master.estadon,
                       su_estatus,
                       g_master.tip_prob,
                       g_master.fol_prob,
                       g_master.doc_prob,
                       g_master.ind_infonavit,
                       g_master.nacionalidad,
                       g_master.tipo_solicitud,
                       g_master.fecha_elaboracion,
                       g_master.cod_error_origen,
                       g_master.folio_edo_cta,
                       g_master.cod_afore_ced,
                       g_master.const_curp,
                       g_master.fecha_emision
                FROM   afi_mae_afiliado
                WHERE  n_folio =  xx_num
                AND    tipo_solicitud = g_master.tipo_solicitud

                IF SQLCA.SQLCODE <> 0 THEN
                        RETURN FALSE
                END IF
        END CASE

        LET g_master1.* = g_master.*

        IF ACCION = 'M' THEN
            SELECT 'X'
            FROM   afi_mae_modifica
            WHERE  n_seguro       = g_master.n_seguro
            AND    cod_operacion  = 0
            AND    status_interno = 120

            IF SQLCA.SQLCODE = 0 THEN
                SELECT n_folio,
                       frecafor,
                       paterno,
                       materno,
                       nombres,
                       n_seguro,
                       n_unico,
                       n_rfc,
                       sexo,
                       edo_civil,
                       fena,
                       salario_base_comis,
                       estadon,
                       status_interno,
                       tip_prob,
                       fol_prob,
                       doc_prob,
                       ind_infonavit,
                       nacionalidad,
                       tipo_solicitud,
                       fecha_elaboracion,
                       cod_error_origen,
                       folio_edo_cta,
                       cod_afore_ced,
                       const_curp
                INTO   g_master.n_folio,
                       g_master.frecafor,
                       g_master.paterno,
                       g_master.materno,
                       g_master.nombres,
                       g_master.n_seguro,
                       g_master.n_unico,
                       g_master.n_rfc,
                       g_master.sexo,
                       g_master.edo_civil,
                       g_master.fena,
                       g_master.salario_base_comis,
                       g_master.estadon,
                       su_estatus,
                       g_master.tip_prob,
                       g_master.fol_prob,
                       g_master.doc_prob,
                       g_master.ind_infonavit,
                       g_master.nacionalidad,
                       g_master.tipo_solicitud,
                       g_master.fecha_elaboracion,
                       g_master.cod_error_origen,
                       g_master.folio_edo_cta,
                       g_master.cod_afore_ced,
                       g_master.const_curp
                FROM   afi_mae_modifica
                WHERE  n_seguro =  g_master.n_seguro
                AND    cod_operacion  = 0
                AND    status_interno = 120
            END IF
        END IF

        LET vstatusint = su_estatus

        IF g_master.const_curp IS NULL OR
           g_master.const_curp < 1 THEN
            LET g_master.const_curp = 1
        END IF

        IF g_master.ind_infonavit = '1' THEN
            LET g_master.ind_infonavit = 'S'

            UPDATE afi_mae_afiliado
            SET    ind_infonavit = g_master.ind_infonavit
            WHERE  n_seguro = g_master.n_seguro
        END IF

        IF g_master.ind_infonavit = '2' THEN
            LET g_master.ind_infonavit = 'N'

            UPDATE afi_mae_afiliado
            SET    ind_infonavit = g_master.ind_infonavit
            WHERE  n_seguro = g_master.n_seguro
        END IF

        SELECT paterno,
               materno,
               nombres,
               nip
        INTO   pat,
               mat,
               nom,
               g_master.nip
        FROM   pro_mae_promotor
        WHERE  codven = g_master.codven

        IF SQLCA.SQLCODE <> 0 THEN
            LET g_master.desc_codven = "NO EXISTE"
        ELSE
            LET g_master.desc_codven = pat CLIPPED," ",
                                       mat CLIPPED," ",
                                       nom CLIPPED
        END IF

        SELECT nombre_uni_n1 
        INTO   g_master.agenc_desc 
        FROM   com_nivel1
        WHERE  coduni_n1 = g_master.agenc_cod

        IF SQLCA.SQLCODE <> 0 THEN
            LET g_master.agenc_desc = "NO EXISTE"
        END IF

        SELECT ecivi_desc
        INTO   g_master.desc_edo_civil
        FROM   tab_edo_civil
        WHERE  ecivi_cod = g_master.edo_civil

        IF SQLCA.SQLCODE <> 0 THEN
            LET g_master.desc_edo_civil = "NO EXISTE"
        END IF

        SELECT sexo_desc
        INTO   g_master.desc_sexo
        FROM   tab_sexo
        WHERE  sexo_cod = g_master.sexo

        IF SQLCA.SQLCODE <> 0 THEN
            LET g_master.desc_sexo = "NO EXISTE"
        END IF

        SELECT ecivi_desc
        INTO   g_master.desc_edo_civil 
        FROM   tab_edo_civil
        WHERE  ecivi_cod = g_master.edo_civil

        IF SQLCA.SQLCODE <> 0 THEN
            LET g_master.desc_edo_civil = "NO EXISTE"
        END IF

        SELECT estad_desc
        INTO   g_master.desc_estadon
        FROM   tab_estado
        WHERE  estad_cod = g_master.estadon

       IF STATUS = NOTFOUND THEN
           LET g_master.desc_estadon = "NO EXISTE"
       END IF

       SELECT pais_desc 
       INTO   g_master.desc_nacionalidad
       FROM   tab_pais
       WHERE  pais_cod = g_master.nacionalidad

       IF STATUS = NOTFOUND THEN
           LET g_master.desc_nacionalidad = "NO EXISTE"
       END IF

       SELECT docprob_desc 
       INTO   g_master.docprob_desc 
       FROM   tab_doc_prob
       WHERE  docprob_cod = g_master.tip_prob

       IF SQLCA.SQLCODE <> 0 THEN
           LET g_master.docprob_desc = "NO EXISTE"
       END IF

       CALL rescata_status(su_estatus)

       RETURN TRUE

END FUNCTION

FUNCTION Modifica()
#M-----------------

    DEFINE xx_status     SMALLINT
    DEFINE rescato       SMALLINT
    DEFINE vfecha_modif  DATE

    LET vfecha_modif = TODAY
    LET sw_carta     = 0

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " MODIFICA CERT " AT 1,62 ATTRIBUTE(REVERSE)
    DISPLAY " [ Esc ] Modifica [ Ctrl-C ] Salir sin Modificar " AT 1,1 ATTRIBUTE(BOLD)

    DISPLAY " CONTROL : [v] Otros Datos  [t] Datos modificados  [b] Despliega               " AT 2,1 ATTRIBUTE(BOLD)

    IF g_master.n_seguro IS NULL THEN
        CALL Inicializa()
    END IF

    LET sw_1 = 0 

    LET g_master.cod_error_origen = NULL
    LET g_master.tipo_solicitud   = NULL
    LET g_master.cod_afore_ced    = NULL
    LET g_master.frecafor         = NULL
    LET g_master.fecha_elaboracion= NULL
    LET g_master.fena             = NULL
    LET g_master.sexo             = NULL
    LET g_master.fecha_emision    = NULL
    LET g_master.edo_civil        = NULL
    LET g_master.n_operac         = NULL
    LET g_master.const_curp       = NULL
    LET g_master.estadon          = NULL
    LET g_master.nip              = NULL

    INPUT BY NAME g_master.* WITHOUT DEFAULTS
        BEFORE FIELD codven
            NEXT FIELD tipo_solicitud

        AFTER FIELD tipo_solicitud
            IF g_master.tipo_solicitud IS NULL THEN
                NEXT FIELD n_seguro
            END IF

            IF g_master.tipo_solicitud < 1 OR
               g_master.tipo_solicitud > 3 THEN
                NEXT FIELD tipo_solicitud
            ELSE
                NEXT FIELD n_folio
            END IF

        AFTER FIELD n_folio
            IF g_master.n_folio IS NULL THEN
                NEXT FIELD n_folio
            ELSE
                IF NOT Rescata_datos("N",g_master.n_folio,
                                     g_master.tipo_solicitud) THEN
                    ERROR "Afiliado NO existe"
                    NEXT FIELD n_folio
                ELSE
                    --LET g_master1.* = g_master.*
                    IF vstatmod > 1 THEN
                        DISPLAY BY NAME g_master.*

                        PROMPT "REGISTRO NO SE PUEDE MODIFICAR, PRESIONE ENTER"
                        ATTRIBUTE(reverse) FOR enter
                        CALL Inicializa()
                        NEXT FIELD tipo_solicitud
                    END IF
                    IF vstatmod = 1 THEN
                        DISPLAY BY NAME g_master.*
                        WHILE TRUE
                        PROMPT "REGISTRO SOLO PUEDE MODIFICAR RFC, ¿DESEA MODIFICAR? (S/N) : "
                        ATTRIBUTE(reverse) FOR enter

                            IF enter MATCHES'[NnSs]' THEN
                                IF enter MATCHES '[Nn]' THEN
                                    CALL Inicializa()
                                    RETURN 
                                END IF
                                IF enter MATCHES '[Ss]' THEN
                                     NEXT FIELD n_rfc
                                     CALL Inicializa()
                                END IF
                            ELSE
                                ERROR "Solo debe presionar (S) Si modificar RFC, (N) No modificar RFC"
                                SLEEP 3
                                ERROR ""
                            END IF
                        END WHILE
                    END IF
                END IF
            END IF

            DISPLAY BY NAME g_master.*

            IF g_master.tip_prob = 5 THEN
                LET sw_cod = 1
            ELSE
                LET sw_cod = 0
            END IF

            NEXT FIELD paterno

        AFTER FIELD n_seguro
            IF g_master.n_seguro IS NULL THEN
                NEXT FIELD tipo_solicitud
            ELSE
                IF NOT Rescata_datos("C",g_master.n_seguro,0) THEN
                    ERROR "Afiliado NO existe"
                    NEXT FIELD n_seguro
                ELSE
                    LET g_master1.* = g_master.*

                    IF g_master.tipo_solicitud > 3 THEN
                        PROMPT "TIPO SOLICITUD NO SE PUEDE MODIFICAR, PRESIONE ENTER"
                        ATTRIBUTE(reverse) FOR enter
                        CALL Inicializa()
                        NEXT FIELD tipo_solicitud
                    END IF

                    IF vstatmod > 1 THEN
                        DISPLAY BY NAME g_master.*
                        PROMPT "REGISTRO NO SE PUEDE MODIFICAR, PRESIONE ENTER"
                        ATTRIBUTE(reverse) FOR enter
                        CALL Inicializa()
                        NEXT FIELD tipo_solicitud
                    END IF
                    IF vstatmod = 1 THEN
                        DISPLAY BY NAME g_master.*
                        WHILE TRUE
                        PROMPT "REGISTRO SOLO PUEDE MODIFICAR RFC, ¿DESEA MODIFICAR? (S/N) : "
                        ATTRIBUTE(reverse) FOR enter

                            IF enter MATCHES'[NnSs]' THEN
                                IF enter MATCHES '[Nn]' THEN
                                    CALL Inicializa()
                                    RETURN 
                                END IF
                                IF enter MATCHES '[Ss]' THEN
                                     NEXT FIELD n_rfc
                                     CALL Inicializa()
                                END IF
                            ELSE
                                ERROR "Solo debe presionar (S) Si modificar RFC, (N) No modificar RFC"
                                SLEEP 2
                                ERROR ""
                            END IF
                        END WHILE
                    END IF
                END IF
            END IF

            DISPLAY BY NAME g_master.*

            IF g_master.tip_prob = 5 THEN
                LET sw_cod = 1
            ELSE
                LET sw_cod = 0
            END IF

            NEXT FIELD paterno

        -- Oficio CONSAR CLAVE ME-10 sustituye 
        AFTER FIELD paterno
            IF g_master.paterno IS NULL THEN
                ERROR "APELLIDO PATERNO NO PUEDE SER NULO "
                ATTRIBUTE (REVERSE)
                SLEEP 2
                ERROR " " 
                NEXT FIELD paterno
            END IF

        { 
            IF sw_cod THEN
            WHILE TRUE
                PROMPT "A. Paterno tiene error de origen [S/N]? "
                ATTRIBUTES (REVERSE) FOR c_pat
                IF c_pat  MATCHES "[SsNn]" THEN
                    IF c_pat MATCHES "[Nn]" THEN
                        LET cod_origen = 0
                        NEXT FIELD materno
                    ELSE
                        LET cod_origen = 1
                        NEXT FIELD materno
                    END IF
                    EXIT WHILE
                ELSE
                    ERROR "Solo debe presionar (S)i o (N)o"
                    SLEEP 2
                    ERROR ""
                END IF
            END WHILE
	    }

            NEXT FIELD materno
 
        AFTER FIELD materno
            IF g_master.materno[1] = " " THEN
                ERROR "Ingrese Ap. Materno correcto o deje el campo nulo"
                LET g_master.materno = NULL
                DISPLAY BY NAME g_master.materno
                NEXT FIELD materno
            END IF

	    {
            IF g_master.materno IS NOT NULL THEN
                WHILE TRUE
                    PROMPT "A. Materno tiene error de origen [S/N]? "
                    ATTRIBUTES (REVERSE) FOR c_mat
                    IF c_mat  MATCHES "[SsNn]" THEN
                        IF c_mat MATCHES "[Nn]" THEN
                            NEXT FIELD nombres
                        ELSE
                            LET cod_origen = cod_origen + 2
                            NEXT FIELD nombres
                        END IF
                        EXIT WHILE
                    ELSE
                        ERROR "Solo debe presionar (S)i o (N)o"
                        SLEEP 2
                        ERROR ""
                    END IF
                END WHILE
            END IF
	    }

            NEXT FIELD nombres
 
        AFTER FIELD nombres
            IF g_master.nombres IS NULL THEN
                ERROR "EL NOMBRE NO PUEDE SER NULO "
                ATTRIBUTE (REVERSE)
                SLEEP 2
                ERROR " " 
                NEXT FIELD nombres
            END IF

	    {
            WHILE TRUE
                PROMPT "Nombres tiene error de origen [S/N]? "
                ATTRIBUTES (REVERSE) FOR c_nom
                IF c_nom  MATCHES "[SsNn]" THEN
                    IF c_nom MATCHES "[Nn]" THEN
                        NEXT FIELD n_rfc
                    ELSE
                        LET cod_origen = cod_origen + 4
                        NEXT FIELD n_rfc
                    END IF
                    EXIT WHILE
                ELSE
                    ERROR "Solo debe presionar (S)i o (N)o"
                    SLEEP 2
                    ERROR ""
                END IF
            END WHILE
	    }

            NEXT FIELD n_rfc
 
        -- Oficio CONSAR CLAVE ME-10 sustituye a version 1.6 
        AFTER FIELD n_rfc 
            IF g_master.n_rfc IS NULL THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD n_rfc
            END IF

            IF LENGTH(g_master.n_rfc) <> 10 AND
               LENGTH(g_master.n_rfc) <> 13 THEN
                ERROR "Debe ingresar R.F.C. completo"
                NEXT FIELD n_rfc
            END IF

            IF NOT valida_fecha_rfc(g_master.n_rfc[5,10]) THEN
                ERROR "Formato de RFC Incorrecto ( ----AAMMDD*** )"
                NEXT FIELD n_rfc
            ELSE
                WHENEVER ERROR CONTINUE

                LET aaa = g_master.n_rfc[5,6]
                LET mm = g_master.n_rfc[7,8]
                LET dd = g_master.n_rfc[9,10]
                LET z_fecha = mm,"/",dd,"/19",aaa
                LET j_fecha = z_fecha

                WHENEVER ERROR STOP

                IF j_fecha IS NULL THEN
                    ERROR "fecha Invalida en RFC"
                    NEXT FIELD n_rfc
                END IF
            END IF

            IF vstatmod = 1 THEN
                NEXT FIELD const_curp
            ELSE
                NEXT FIELD sexo
            END IF

        -- Oficio CONSAR CLAVE ME-10 sustituye a version 1.6
            AFTER FIELD sexo
                IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                    NEXT FIELD n_rfc
                END IF

                IF g_master.sexo IS NULL THEN
                    CALL Despliega_sexos() RETURNING g_master.sexo,
                                                     g_master.desc_sexo
                ELSE
                    SELECT sexo_desc
                    INTO g_master.desc_sexo
                    FROM tab_sexo
                    WHERE sexo_cod = g_master.sexo

                    IF SQLCA.SQLCODE <> 0 THEN
                        ERROR "Sexo Inexistente"
                        NEXT FIELD sexo
                    END IF
                END IF

                DISPLAY BY NAME g_master.sexo,g_master.desc_sexo

                NEXT FIELD edo_civil

            AFTER FIELD edo_civil
                IF g_master.edo_civil IS NULL THEN
                    CALL Despliega_estados_civiles() 
                    RETURNING g_master.edo_civil, g_master.desc_edo_civil
                ELSE
                    SELECT ecivi_desc
                    INTO g_master.desc_edo_civil
                    FROM tab_edo_civil
                    WHERE ecivi_cod = g_master.edo_civil

                    IF SQLCA.SQLCODE <> 0 THEN
                        ERROR "Estado Civil Inexistente"
                        NEXT FIELD edo_civil
                    END IF
                END IF

                DISPLAY BY NAME g_master.edo_civil,g_master.desc_edo_civil

                NEXT FIELD fena
 
        -- Oficio CONSAR CLAVE ME-10 sustituye a version 1.6 
            
            BEFORE FIELD fena
                LET x_fecha = g_master.n_rfc[7,8],"/",
                              g_master.n_rfc[9,10],"/",
                              "19",g_master.n_rfc[5,6]

                LET xx_fecha = x_fecha

            AFTER FIELD fena
                IF g_master.fena IS NULL THEN
                    ERROR "Campo NO puede ser NULO"
                    NEXT FIELD fena
                END IF

            IF xx_fecha <> g_master.fena THEN
                WHILE TRUE
                    PROMPT "Existen inconsistencias entre RFC/Fecha nacimiento, es correcto ¨[S/N]? "
                    FOR enter
                    IF enter MATCHES "[Ss/Nn]" THEN
                        IF enter MATCHES "[Ss]" THEN
                            NEXT FIELD salario_base_comis
                            EXIT WHILE
                        ELSE
                            NEXT FIELD n_rfc
                        END IF
                    ELSE
                        ERROR "Solo debe presionar (S)i o (N)o"
                        SLEEP 3
                        ERROR ""
                    END IF
                END WHILE
            END IF 

            AFTER FIELD salario_base_comis
                IF g_master.salario_base_comis IS NULL THEN
                    ERROR "Campo NO puede ser NULO"
                    NEXT FIELD salario_base_comis
                END IF

        -- Oficio CONSAR CLAVE ME-10 sustituye a version 1.6 
           AFTER FIELD estadon
               IF g_master.estadon IS NULL THEN
                   CALL Despliega_estados() RETURNING g_master.estadon,
                                                      g_master.desc_estadon

                   IF g_master.estadon = 0 THEN
                       NEXT FIELD estadon 
                   END IF
               ELSE
                   SELECT estad_desc 
                   INTO   g_master.desc_estadon 
                   FROM   tab_estado
                   WHERE  estad_cod = g_master.estadon

                   IF SQLCA.SQLCODE <> 0 THEN
                       ERROR "Entidad de Nacimiento Inexistente"
                       NEXT FIELD estadon
                   END IF
               END IF

               DISPLAY BY NAME g_master.estadon,g_master.desc_estadon

               NEXT FIELD nacionalidad

        -- Oficio CONSAR CLAVE ME-10 sustituye a version 1.6
           BEFORE FIELD nacionalidad
               IF g_master.nacionalidad IS NULL THEN
                   LET g_master.nacionalidad = "MEX"

                   DISPLAY BY NAME g_master.nacionalidad
               END IF

           AFTER FIELD nacionalidad
               IF g_master.nacionalidad IS NULL THEN
                   CALL Despliega_pais() RETURNING g_master.nacionalidad,
                                                   g_master.desc_nacionalidad
               ELSE
                   SELECT pais_desc
                   INTO   g_master.desc_nacionalidad
                   FROM   tab_pais
                   WHERE pais_cod = g_master.nacionalidad

                   IF SQLCA.SQLCODE <> 0 THEN
                       ERROR "Pais Inexistente"
                       NEXT FIELD nacionalidad
                   END IF
               END IF

               DISPLAY BY NAME g_master.nacionalidad, g_master.desc_nacionalidad

               NEXT FIELD tip_prob
 
        -- Se agrega validacion
           AFTER FIELD tip_prob
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                   NEXT FIELD nacionalidad
               END IF

               IF g_master.tip_prob IS NULL THEN
                   CALL Despliega_documento_probatorio()
                   RETURNING g_master.tip_prob,g_master.docprob_desc

                   IF g_master.tip_prob = 0 THEN
                       NEXT FIELD tip_prob 
                   END IF
               ELSE
                   SELECT docprob_desc
                   INTO   g_master.docprob_desc
                   FROM   tab_doc_prob
                   WHERE  docprob_cod = g_master.tip_prob

                   IF SQLCA.SQLCODE <> 0 THEN
                       ERROR "Tipo de Documento Probatorio Inexistente"
                       NEXT FIELD tip_prob
                   END IF
               END IF

               IF g_master.tip_prob = 5 THEN
                   IF g_master1.tip_prob = 6 THEN
                       NEXT FIELD n_unico
                   ELSE
                       ERROR "Cambio de Tipo de Documento Probatorio no valido"
                       LET g_master.tip_prob = g_master1.tip_prob
                       DISPLAY BY NAME g_master.tip_prob
                       NEXT FIELD tip_prob
                   END IF
               END IF

               DISPLAY BY NAME g_master.tip_prob,g_master.docprob_desc

               IF g_master.tip_prob = 5 THEN
                   NEXT FIELD n_unico
               ELSE
                   NEXT FIELD fol_prob
               END IF

           AFTER FIELD n_unico
               IF LENGTH(g_master.n_unico) <> 18 OR
                  g_master.n_unico[1] = " " OR
                  g_master.n_unico IS NULL THEN
                   ERROR "Debe ingresar CURP completa"

                   LET g_master.n_unico = g_master1.n_unico

                   NEXT FIELD n_unico
               END IF

               NEXT FIELD fol_prob

           AFTER FIELD fol_prob
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                   NEXT FIELD tip_prob
               END IF

               IF g_master.fol_prob IS NULL THEN
                  --g_master.tip_prob <> 5 THEN
                   ERROR "Numero de Folio NO puede ser NULO"
                   NEXT FIELD fol_prob
               END IF

           BEFORE FIELD doc_prob 
               IF g_master.tip_prob = "6" THEN
                   LET g_master.doc_prob = NULL

                   DISPLAY BY NAME g_master.doc_prob

                   NEXT FIELD ind_infonavit
               END IF

           AFTER FIELD doc_prob 
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                   NEXT FIELD fol_prob
               END IF

               IF NOT Verifica_documento_probatorio(g_master.tip_prob,
                                                    g_master.doc_prob) THEN
                   NEXT FIELD doc_prob
               END IF

           AFTER FIELD ind_infonavit
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                   NEXT FIELD doc_prob
               END IF

               IF g_master.ind_infonavit IS NULL THEN
                   ERROR "Se requiere la clave de prestamo del INFONAVIT"
                   NEXT FIELD ind_infonavit
               END IF

               IF g_master.ind_infonavit NOT MATCHES "[SsNn]" THEN
                   ERROR "Solo se acepta (s)i o (n)o"
                   NEXT FIELD ind_infonavit
               END IF
    ---   fin agrega ultima validacion

    ---   se agrega el campo de codigo de error de origen
           BEFORE FIELD cod_error_origen
               DISPLAY BY NAME g_master.cod_error_origen

               CALL verifica_cod_error_orig()

           AFTER FIELD cod_error_origen
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                   NEXT FIELD ind_infonavit
               END IF

               NEXT FIELD const_curp

           BEFORE FIELD const_curp

               IF g_master.const_curp IS NULL THEN
                   LET g_master.const_curp = 1
               END IF

               DISPLAY BY NAME g_master.const_curp

           AFTER FIELD const_curp
               IF g_master.const_curp IS NULL THEN
                   ERROR "Campo constancia curp NO debe ser nulo"
                   SLEEP 2
               END IF

               IF g_master.const_curp < 1 OR
                  g_master.const_curp > 2 THEN
                   ERROR "Solo puede ser valor (1) Sin const. curp / (2) Con const. curp "
                   LET g_master.const_curp = '1'
                   DISPLAY BY NAME g_master.const_curp 
                   NEXT FIELD const_curp
               END IF

           ON KEY ( CONTROL-T )
               SELECT "X"
               FROM   afi_mae_modifica md
               WHERE  md.n_seguro = g_master.n_seguro
               AND    md.n_folio = g_master.n_folio
               AND    md.tipo_solicitud = g_master.tipo_solicitud
               GROUP BY 1

               IF SQLCA.SQLCODE = 0 THEN
                   CALL verifica_datos_modificados()
               ELSE
                   PROMPT "Registro sin modificaciones, [Enter] p/continuar " 
                   ATTRIBUTES (REVERSE)
                   FOR enter
               END IF

           ON KEY ( CONTROL-B )
               CALL Despliega()
               DISPLAY BY NAME g_master.n_folio

           ON KEY ( INTERRUPT )
               CALL Inicializa()
               DISPLAY "                                                                               " AT 6,2  ATTRIBUTE(REVERSE)
               DISPLAY "                                                                               " AT 14,1 ATTRIBUTE(REVERSE)
               DISPLAY "                  " AT 1,62 
               EXIT INPUT

         ON KEY ( ESC )

    WHILE TRUE
        CALL Desea_modificar()

        IF aux_pausa MATCHES "[SsNn]" THEN
            EXIT WHILE
        END IF

    END WHILE

    IF aux_pausa MATCHES "[Nn]" THEN
        RETURN
    ELSE
        IF g_master.tip_prob = 5 THEN
            WHILE TRUE
                PROMPT "¿Desea Generar carta de notificacion [S/N]? "
                FOR aux_pausa

                IF aux_pausa MATCHES "[SsNn]" THEN
                    EXIT WHILE
                END IF

                IF aux_pausa MATCHES "[Ss]" THEN
                    UPDATE afi_mae_afiliado
                    SET    documento_1 = 0
                    WHERE  n_seguro    = g_master.n_seguro
                END IF
            END WHILE
        END IF

        LET g_master1.paterno      = g_master1.paterno      CLIPPED
        LET g_master.paterno       = g_master.paterno       CLIPPED
        LET g_master1.materno      = g_master1.materno      CLIPPED
        LET g_master.materno       = g_master.materno       CLIPPED
        LET g_master1.nombres      = g_master1.nombres      CLIPPED
        LET g_master.nombres       = g_master.nombres       CLIPPED
        LET g_master1.sexo         = g_master1.sexo         CLIPPED
        LET g_master.sexo          = g_master.sexo          CLIPPED
        LET g_master1.fena         = g_master1.fena         CLIPPED
        LET g_master.fena          = g_master.fena          CLIPPED
        LET g_master1.nacionalidad = g_master1.nacionalidad CLIPPED
        LET g_master.nacionalidad  = g_master.nacionalidad  CLIPPED
        LET g_master1.tip_prob     = g_master1.tip_prob     CLIPPED
        LET g_master.tip_prob      = g_master.tip_prob      CLIPPED
        LET g_master1.fol_prob     = g_master1.fol_prob     CLIPPED
        LET g_master.fol_prob      = g_master.fol_prob      CLIPPED
        LET g_master1.doc_prob     = g_master1.doc_prob     CLIPPED
        LET g_master.doc_prob      = g_master.doc_prob      CLIPPED
        LET g_master1.n_rfc        = g_master1.n_rfc        CLIPPED
        LET g_master.n_rfc         = g_master.n_rfc         CLIPPED

        IF g_master1.paterno      = g_master.paterno      AND
           g_master1.materno      = g_master.materno      AND
           g_master1.nombres      = g_master.nombres      AND
           g_master1.sexo         = g_master.sexo         AND
           g_master1.fena         = g_master.fena         AND
           g_master1.estadon      = g_master.estadon      AND
           g_master1.nacionalidad = g_master.nacionalidad AND
           g_master1.tip_prob     = g_master.tip_prob     AND
           g_master1.fol_prob     = g_master.fol_prob     AND
           g_master1.doc_prob     = g_master.doc_prob     AND
           g_master1.n_rfc        = g_master.n_rfc THEN
            IF g_master.cod_error_origen IS NOT NULL THEN
                CALL inserta_modificacion()    #im
            END IF

            IF g_master1.salario_base_comis <> g_master.salario_base_comis OR
               g_master1.ind_infonavit <> g_master.ind_infonavit OR
               g_master1.edo_civil <> g_master.edo_civil THEN

                UPDATE afi_mae_afiliado
                SET    salario_base_comis = g_master.salario_base_comis,
                       ind_infonavit      = g_master.ind_infonavit,
                       edo_civil          = g_master.edo_civil,
                       usuario            = g_usuario,
                       const_curp         = g_master.const_curp
                WHERE  n_seguro           = g_master.n_seguro
                AND    n_folio            = g_master.n_folio
                AND    tipo_solicitud     = g_master.tipo_solicitud

                IF SQLCA.SQLCODE = 0 THEN
                     INSERT INTO afi_mae_modifica
                     VALUES (g_master1.tipo_solicitud     ,
                             g_master1.n_folio            ,
                             g_master1.fecha_elaboracion  ,
                             g_master1.folio_edo_cta      ,
                             g_master1.cod_afore_ced      ,
                             g_master1.fecha_emision      ,
                             g_master1.frecafor           ,
                             g_master1.paterno            ,
                             g_master1.materno            ,
                             g_master1.nombres            ,
                             g_master1.n_seguro           ,
                             g_master1.n_rfc              ,
                             g_master1.n_unico            ,
                             g_master1.sexo               ,
                             g_master1.edo_civil          ,
                             g_master1.fena               ,
                             g_master1.salario_base_comis ,
                             g_master1.estadon            ,
                             g_master1.nacionalidad       ,
                             g_master1.tip_prob           ,
                             g_master1.fol_prob           ,
                             g_master1.doc_prob           ,
                             g_master1.ind_infonavit      ,
                             g_master1.cod_error_origen   ,
                             g_master1.const_curp         ,
                             HOY                          ,
                             g_usuario                    ,
                             1                            ,
                             532                          ,
                             vstatusint                   ,
                             ''                           ,
                             ''                           ,
                             '')
                END IF

                LET operacion = 'MODIF. AFIL. DATOS NO CERTIFICABLES'
                LET su_st_int = su_estatus

                CALL inserta_logico(su_st_int, operacion)
            ELSE
                IF g_master.cod_error_origen IS NULL THEN
                    ERROR "NO HUBO MODIFICACIONES, EL REGISTRO NO FUE ACTUALIZADO"
                    SLEEP 2
                    ERROR " "
                END IF
            END IF
        ELSE
            CALL inserta_modificacion()   #im
        END IF
    END IF

    CALL Inicializa()

    NEXT FIELD tipo_solicitud

    ON KEY ( CONTROL-V )
        LET COMMA = "fglgo AFIM012.4gi ",
                    g_master.n_folio,
                    " ",
                    ACCION,
                    " ",
                    vcont," ",
                    g_master.tipo_solicitud
        RUN COMMA 

     END INPUT

END FUNCTION

FUNCTION inserta_modificacion()
#im----------------------------

    DEFINE 
        marca_ant SMALLINT

    LET su_st_int = 120
    LET operacion = 'MODIF. AFILIADO DATOS CERTIFICABLES'

    SELECT "X"
    FROM   afi_mae_modifica a
    WHERE  a.n_seguro       = g_master.n_seguro
    AND    a.n_folio        = g_master.n_folio
    AND    a.tipo_solicitud = g_master.tipo_solicitud
    AND    a.cod_operacion  = 0
    GROUP BY 1

    IF SQLCA.SQLCODE = 0 THEN
        DELETE FROM afi_mae_modifica
        WHERE  n_seguro = g_master.n_seguro
        AND    n_folio  = g_master.n_folio
        AND    tipo_solicitud = g_master.tipo_solicitud
        AND    cod_operacion  = 0
    END IF

    UPDATE afi_mae_afiliado 
    SET    status_interno = 120
    WHERE  n_seguro       = g_master.n_seguro
    AND    n_folio        = g_master.n_folio
    AND    tipo_solicitud = g_master.tipo_solicitud
    AND    status_interno <> 160

    INSERT INTO afi_mae_modifica
    VALUES(g_master.tipo_solicitud     ,
           g_master.n_folio            ,
           g_master.fecha_elaboracion  ,
           g_master.folio_edo_cta      ,
           g_master.cod_afore_ced      ,
           g_master.fecha_emision      ,
           g_master.frecafor           ,
           g_master.paterno            ,
           g_master.materno            ,
           g_master.nombres            ,
           g_master.n_seguro           ,
           g_master.n_rfc              ,
           g_master.n_unico            ,
           g_master.sexo               ,
           g_master.edo_civil          ,
           g_master.fena               ,
           g_master.salario_base_comis ,
           g_master.estadon            ,
           g_master.nacionalidad       ,
           g_master.tip_prob           ,
           g_master.fol_prob           ,
           g_master.doc_prob           ,
           g_master.ind_infonavit      ,
           g_master.cod_error_origen   ,
           g_master.const_curp         ,
           HOY                         ,
           g_usuario                   ,
           0                           ,
           0                           ,
           120                         ,
           ''                          ,
           ''                          ,
           '')

    IF SQLCA.SQLCODE = 0 THEN
        SELECT marca_cod
        INTO   marca_ant
        FROM   cta_act_marca
        WHERE  nss = g_master.n_seguro
        AND    marca_cod = 600

        IF marca_ant <> 600 THEN
            CALL marca_cuenta()
        END IF

        CALL inserta_logico(su_st_int, operacion)
        ERROR "REGISTRO MODIFICADO" SLEEP 2 ERROR ""
    ELSE
        ERROR "REGISTRO CON DATOS ERRONEOS, NO SE HIZO MODIFICACION"
        SLEEP 2
        ERROR ""
    END IF

END FUNCTION

FUNCTION verifica_cod_error_orig()
#vceo-----------------------------

    IF g_master1.paterno <> g_master.paterno THEN
        LET paterno_2 = 1
    ELSE
        LET paterno_2 = 0
    END IF

    IF g_master1.materno <> g_master.materno THEN
        LET materno_3 = 2
    ELSE
        LET materno_3 = 0
    END IF

    IF g_master1.nombres <> g_master.nombres THEN
        LET nombre_4 = 4
    ELSE
        LET nombre_4 = 0
    END IF

    IF g_master1.sexo <> g_master.sexo THEN
        LET sexo_5 = 8
    ELSE
        LET sexo_5 = 0
    END IF

    IF g_master1.fena <> g_master.fena THEN
        LET macim_6 = 16
    ELSE
        LET macim_6 = 0
    END IF

    IF g_master1.estadon <> g_master.estadon THEN
        LET entidad_7 = 32
    ELSE
        LET entidad_7 = 0
    END IF

    IF g_master1.nacionalidad <> g_master.nacionalidad THEN
        LET nacion_8 = 64
    ELSE
        LET nacion_8 = 0
    END IF

    IF g_master1.tip_prob <> g_master.tip_prob THEN
        LET cve_prob_9 = 128
    ELSE
        LET cve_prob_9 = 0
    END IF

    IF g_master1.doc_prob <> g_master.doc_prob THEN
        LET doc_prob_10 = 256
    ELSE
        LET doc_prob_10 = 0
    END IF

    IF g_master1.fol_prob <> g_master.fol_prob THEN
        LET fol_prob_11 = 512
    ELSE
        LET fol_prob_11 = 0
    END IF

    LET tot_error = paterno_2   +
                    materno_3   +
                    nombre_4    +
                    sexo_5      +
                    macim_6     +
                    entidad_7   +
                    nacion_8    +
                    cve_prob_9  +
                    doc_prob_10 +
                    fol_prob_11 
  
    LET cod_error = tot_error USING "&&&&"

    IF cod_error IS NULL THEN
        LET cod_error = "0000"
    END IF

    IF cod_error <> "0000" THEN
        LET g_master.cod_error_origen = cod_error
        DISPLAY BY NAME g_master.cod_error_origen
    END IF

END FUNCTION

FUNCTION Desea_modificar()

    PROMPT "¿Desea Modificar la Informacion [S/N]? "
    FOR aux_pausa

END FUNCTION 

FUNCTION Despliega()

    DEFINE aux_pausa    CHAR(1)
    DEFINE txt          CHAR(300)
    DEFINE txt1         CHAR(300)
    DEFINE paterno      CHAR(50)
    DEFINE materno      CHAR(50)
    DEFINE nombres      CHAR(50)
    DEFINE cla_sel      CHAR(250)
    DEFINE n_busqueda   CHAR(100)

    DEFINE l_reg ARRAY[5000] OF RECORD
           n_seguro       CHAR(11),
           n_unico        CHAR(18),
           tipo_solicitud SMALLINT,
           n_folio         DECIMAL(10,0),
           nombre         CHAR(50)
    END RECORD

    DEFINE i           SMALLINT
    DEFINE HACER       SMALLINT
    DEFINE pat,mat,nom CHAR(50)

    OPEN WINDOW v1 AT  4,4 WITH FORM "AFIM0012" ATTRIBUTE(BORDER)
    DISPLAY "          USTED PUEDE UTILIZAR EL * (ASTERISCO) COMO COMODIN                " AT 1,1 ATTRIBUTE(REVERSE)

    LET HACER    = TRUE
    LET INT_FLAG = TRUE

    CONSTRUCT BY NAME cla_sel ON paterno,materno,nombres

    ON KEY ( INTERRUPT )
        LET HACER = FALSE
        EXIT CONSTRUCT

    ON KEY ( ESC )
        LET INT_FLAG=FALSE
        EXIT CONSTRUCT

    END CONSTRUCT

    LET txt = "SELECT n_seguro,n_unico,tipo_solicitud,n_folio,paterno,materno,",
              "nombres FROM afi_mae_afiliado WHERE ",cla_sel CLIPPED,
              " ORDER BY 5,6" CLIPPED

    IF HACER THEN
        ERROR "Buscando Informacion"

        PREPARE cur1 FROM txt
        DECLARE cursor_1 cursor FOR cur1
        LET i = 1

        FOREACH cursor_1 INTO l_reg[i].n_seguro,
                              l_reg[i].n_unico,
                              l_reg[i].tipo_solicitud,
                              l_reg[i].n_folio,
                              pat,
                              mat,
                              nom

        LET l_reg[i].nombre = pat CLIPPED," ",
                              mat CLIPPED," ",
                              nom CLIPPED
        LET i = i + 1

        IF i >= 32000  THEN
            ERROR "Sobrepaso Capacidad Maxima del Arreglo"
            EXIT FOREACH
        END IF

        END FOREACH

        FREE cursor_1

        IF (i-1) < 1 THEN
            ERROR "ARCHIVO AFILIADOS..... VACIO"
        END IF

        CALL SET_COUNT(i-1)
        DISPLAY ARRAY l_reg TO scr_1.*

        ON KEY ( CONTROL-M )
            LET i = ARR_CURR()
            LET g_master.n_seguro       = l_reg[i].n_seguro
            LET g_master.n_unico        = l_reg[i].n_unico
            LET g_master.n_folio        = l_reg[i].n_folio
            LET g_master.tipo_solicitud = l_reg[i].tipo_solicitud
            EXIT DISPLAY

        ON KEY ( INTERRUPT )
            LET g_master.n_seguro       = NULL
            LET g_master.n_unico        = NULL
            LET g_master.n_folio        = NULL
            LET g_master.tipo_solicitud = NULL
            EXIT DISPLAY

        END DISPLAY
    END IF

    CLOSE WINDOW v1

END FUNCTION

FUNCTION rescata_status(valor)
#rs---------------------------

    DEFINE edo_desc   CHAR(50)
    DEFINE edo_cta    SMALLINT
    DEFINE edo_proc1   SMALLINT
    DEFINE x_unico    LIKE afi_mae_afiliado.n_unico
    DEFINE valor      SMALLINT
    DEFINE l_estado   ,
           l_desc     CHAR(25)
    DEFINE x_fecha    DATE
    DEFINE x_inicio   DATE
    DEFINE x_asigna   DATE
    DEFINE finit      DATE
    DEFINE f_marca    DATE

    LET edo_desc = NULL
    LET l_estado = NULL
    LET l_desc   = NULL
    LET finit    = NULL
    LET x_inicio = NULL
    LET x_fecha  = NULL
    LET edo_cta  = 0
    LET edo_proc1 = 0
    LET vstatmod = 0

    SELECT tsa.estado_desc
    INTO   l_estado
    FROM   tab_status_afi tsa
    WHERE  tsa.estado_cod = valor

    LET l_desc = valor USING "&&&"

    SELECT cc.estado_cuenta, cc.marca_cod, cc.fecha_act_marca
    INTO   edo_cta, edo_proc1, f_marca
    FROM   cta_ctr_cuenta cc
    WHERE  cc.nss = g_master.n_seguro

    IF edo_cta = 5 THEN
        LET vstatmod = 2
    END IF

    CASE edo_proc1
        WHEN   0  LET vstatmod = 0
        WHEN   2  LET vstatmod = 0
        WHEN 230  LET vstatmod = 0
        WHEN 250  LET vstatmod = 0
        WHEN 270  LET vstatmod = 0
        WHEN 490  LET vstatmod = 0
        WHEN 540  LET vstatmod = 0
        WHEN 542  LET vstatmod = 0
        WHEN 600  LET vstatmod = 0
        OTHERWISE LET vstatmod = 2
    END CASE

    SELECT t.estado_descripcion
    INTO   edo_desc 
    FROM   tab_edo_cuenta t
    WHERE  t.estado_cuenta  = edo_cta
    AND    t.estado_proceso = edo_proc1

    IF vstatusint = 130 THEN
        LET vstatmod = 2
    END IF

    IF vstatusint = 160 THEN
        LET vstatmod = 1
    END IF

    DISPLAY l_estado CLIPPED," /",
            edo_desc CLIPPED AT 6,2 ATTRIBUTE(REVERSE)

    SELECT fentcons, finicta, finitmte
    INTO   x_fecha, x_inicio , finit
    FROM   afi_mae_afiliado
    WHERE  n_seguro = g_master.n_seguro

    IF finit IS NOT NULL THEN
        LET edo_desc = edo_desc CLIPPED, " ASIG MISMA AFORE"
    END IF

    IF g_master.tipo_solicitud <> 5 THEN
        DISPLAY "F.Certif. ",x_fecha USING "dd-mm-yyyy" 
        AT 14,57 ATTRIBUTE(REVERSE)
    ELSE
        DISPLAY "F.Asigna. ",x_fecha USING "dd-mm-yyyy" 
        AT 14,57 ATTRIBUTE(REVERSE)
    END IF

    DISPLAY "F.Apert.Cta. ",x_inicio USING "dd-mm-yyyy" 
    AT 14,29 ATTRIBUTE(REVERSE)

    IF finit IS NOT NULL THEN
        DISPLAY "F.Asigna. ",finit USING "dd-mm-yyyy"
        AT 14,5 ATTRIBUTE(REVERSE)
    END IF
 
END FUNCTION 

FUNCTION motivo_rechazo()
#mr----------------------

    DEFINE a ARRAY[100] OF RECORD
        codigo      SMALLINT,
        desc_cod    CHAR(10),
        descripcion CHAR(80),
        fecha_rechazo DATE
    END RECORD

    DEFINE i smallint

    OPEN WINDOW v34  at 6,3 WITH FORM "AFIM0112" attribute(border)
    DISPLAY " [ Ctrl_c ] Salir " AT 1,1
    DISPLAY "                    MOTIVOS DE RECHAZO PROCESAR                                " AT 3,1 ATTRIBUTE(REVERSE)

    DECLARE cursor_o CURSOR FOR
    SELECT m.diag_proceso, 'RECHAZADO', b.rdeta_desc_l, m.fecha_modifica
    FROM   afi_mae_modifica m, tab_rdeta b
    WHERE  m.n_seguro = g_master.n_seguro
    AND    m.cod_operacion = 2
    AND    m.diag_proceso = b.rdeta_cod

    LET i = 1

    FOREACH cursor_o INTO a[i].*
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)
    DISPLAY ARRAY a TO scr_1.*

    ON KEY ( INTERRUPT )
        EXIT DISPLAY

    END DISPLAY

    CLOSE WINDOW v34

END FUNCTION

FUNCTION Ingresa_autoriza()
#ia------------------------

    DEFINE cod  DECIMAL(10,0)
    DEFINE desc CHAR(60)

    DEFINE l_reg ARRAY[1000] OF RECORD
          codigo      CHAR(10),
          descripcion CHAR(50)
    END RECORD 

    DEFINE pos        SMALLINT
    DEFINE x_buscar   CHAR(60)
    DEFINE x_texto    CHAR(200)

    LET aux_pausa = "N"

    OPEN WINDOW ventanilla_super AT 8,4 WITH FORM "AFIM0015" ATTRIBUTE(BORDER)
    DISPLAY "                      OPCION RESERVADA PARA SUPERVISORES                       " AT 3,1 ATTRIBUTE ( REVERSE)

    INPUT BY NAME r_cod.super_cod,r_cod.super_desc,r_cod.nip 

    AFTER FIELD super_cod
        IF r_cod.super_cod IS NULL THEN
            CALL Despliega_supervisores()
            RETURNING r_cod.super_cod, r_cod.super_desc

            SELECT super_cod,super_desc 
            INTO   r_cod.super_cod,r_cod.super_desc
            FROM   tab_supervisor
            WHERE  super_cod = r_cod.super_cod

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR "No existe codigo ... "
                NEXT FIELD super_cod
            END IF
        ELSE
            SELECT area_cod,super_desc 
            INTO   r_cod.area_cod,r_cod.super_desc
            FROM   tab_supervisor
            WHERE  super_cod = r_cod.super_cod

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR "CLAVE DE SUPERVISOR INVALIDA... " SLEEP 2
                NEXT FIELD super_cod
            END IF

            DISPLAY BY NAME r_cod.super_desc

        END IF

        AFTER FIELD nip
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD super_cod 
            END IF

            IF r_cod.nip IS NULL THEN
                LET aux_pausa = "S"
                ERROR "El NIP NO ES CORRECTO"
                NEXT FIELD nip
            END IF

            SELECT "X" 
            FROM   tab_supervisor
            WHERE  nip = r_cod.nip
            AND    super_cod = r_cod.super_cod 

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR "Permiso denegado ... "
                LET aux_pausa = "S"
                NEXT FIELD nip
            ELSE
                LET aux_pausa = "N"
                EXIT INPUT
            END IF

        ON KEY ( INTERRUPT )
            LET aux_pausa = "S"
            EXIT INPUT

        END INPUT

        CLOSE WINDOW ventanilla_super

END FUNCTION

FUNCTION verifica_datos_modificados()
#vdm---------------------------------

   OPEN WINDOW ventana_dm AT 2,2 WITH FORM "AFIM0113" ATTRIBUTE(BORDER)

   DISPLAY " AFIM011                 MANTENIMIENTO  AFILIADOS                               " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "                                                                                " AT 6,1 ATTRIBUTE(REVERSE)
   DISPLAY "                                                                                " AT 14,1 ATTRIBUTE(REVERSE)
   DISPLAY "               Nombre del Trabajador segun Documento Probatorio                 " AT 10,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

   CALL Rescata_datos_modificados() #rdm

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      DISPLAY ARRAY g_master2 TO scr_1.*
         ON KEY (INTERRUPT)
            EXIT DISPLAY
      END DISPLAY
   END IF

   CLOSE WINDOW ventana_dm

END FUNCTION

FUNCTION Rescata_datos_modificados()
#rdm--------------------------------

   DECLARE cursor_2 CURSOR FOR
   SELECT a.codven,
          a.agenc_cod,
          b.n_folio,
          a.n_operac,
          b.frecafor,
          b.paterno,
          b.materno,
          b.nombres,
          b.n_seguro,
          b.n_unico,
          b.n_rfc,
          b.sexo,
          b.edo_civil,
          b.fena,
          b.salario_base_comis,
          b.estadon,
          b.tip_prob,
	  c.docprob_desc,
          b.fol_prob,
          b.doc_prob,
          b.ind_infonavit,
          b.nacionalidad,
          b.tipo_solicitud,
          b.fecha_modifica,
          b.fecha_elaboracion,
          b.cod_error_origen,
          a.folio_edo_cta,
          a.cod_afore_ced, 
          b.const_curp,
          a.femision
   FROM   afi_mae_afiliado a, afi_mae_modifica b, tab_doc_prob c
   WHERE  a.n_seguro = g_master.n_seguro
   AND    a.n_folio = g_master.n_folio
   AND    a.tipo_solicitud = g_master.tipo_solicitud
   AND    b.n_seguro = g_master.n_seguro
   AND    b.n_folio = g_master.n_folio
   AND    b.tipo_solicitud = g_master.tipo_solicitud
   AND    b.tip_prob = c.docprob_cod
   ORDER BY b.fecha_modifica DESC

   LET pos = 1

   FOREACH cursor_2 INTO g_master2[pos].codven,
                         g_master2[pos].agenc_cod,
                         g_master2[pos].n_folio,
                         g_master2[pos].n_operac,
                         g_master2[pos].frecafor,
                         g_master2[pos].paterno,
                         g_master2[pos].materno,
                         g_master2[pos].nombres,
                         g_master2[pos].n_seguro,
                         g_master2[pos].n_unico,
                         g_master2[pos].n_rfc,
                         g_master2[pos].sexo,
                         g_master2[pos].edo_civil,
                         g_master2[pos].fena,
                         g_master2[pos].salario_base_comis,
                         g_master2[pos].estadon,
                         g_master2[pos].tip_prob,
                         g_master2[pos].docprob_desc,
                         g_master2[pos].fol_prob,
                         g_master2[pos].doc_prob,
                         g_master2[pos].ind_infonavit,
                         g_master2[pos].nacionalidad,
                         g_master2[pos].tipo_solicitud,
                         g_master2[pos].fecha_modifica,
                         g_master2[pos].fecha_elaboracion,
                         g_master2[pos].cod_error_origen,
                         g_master2[pos].folio_edo_cta,
                         g_master2[pos].cod_afore_ced,
                         g_master2[pos].const_curp,
                         g_master2[pos].fecha_emision

      SELECT paterno,
             materno,
             nombres,
             nip 
      INTO   pat,
             mat,
             nom,
             g_master2[pos].nip
      FROM   pro_mae_promotor
      WHERE  codven = g_master2[pos].codven

      IF SQLCA.SQLCODE <> 0 THEN
         LET g_master2[pos].desc_codven = "NO EXISTE"
      ELSE
         LET g_master2[pos].desc_codven = pat CLIPPED," ",
                                     mat CLIPPED," ",
                                     nom CLIPPED
      END IF

      SELECT nombre_uni_n1 
      INTO   g_master2[pos].agenc_desc 
      FROM   com_nivel1
      WHERE  coduni_n1 = g_master2[pos].agenc_cod

      IF SQLCA.SQLCODE <> 0 THEN
         LET g_master2[pos].agenc_desc = "NO EXISTE"
      END IF

      SELECT ecivi_desc 
      INTO   g_master2[pos].desc_edo_civil 
      FROM   tab_edo_civil
      WHERE  ecivi_cod = g_master2[pos].edo_civil

      IF SQLCA.SQLCODE <> 0 THEN
         LET g_master2[pos].desc_edo_civil = "NO EXISTE"
      END IF

      SELECT sexo_desc 
      INTO   g_master2[pos].desc_sexo 
      FROM   tab_sexo
      WHERE  sexo_cod = g_master2[pos].sexo

      IF SQLCA.SQLCODE <> 0 THEN
         LET g_master2[pos].desc_sexo = "NO EXISTE"
      END IF

      SELECT ecivi_desc 
      INTO   g_master2[pos].desc_edo_civil 
      FROM   tab_edo_civil
      WHERE  ecivi_cod = g_master2[pos].edo_civil

      IF SQLCA.SQLCODE <> 0 THEN
         LET g_master2[pos].desc_edo_civil = "NO EXISTE"
      END IF

      SELECT estad_desc 
      INTO   g_master2[pos].desc_estadon
      FROM   tab_estado
      WHERE  estad_cod = g_master2[pos].estadon

      IF SQLCA.SQLCODE <> 0 THEN
         LET g_master2[pos].desc_estadon = "NO EXISTE"
      END IF

      SELECT pais_desc 
      INTO   g_master2[pos].desc_nacionalidad
      FROM   tab_pais
      WHERE  pais_cod = g_master2[pos].nacionalidad

      IF SQLCA.SQLCODE <> 0 THEN
         LET g_master2[pos].desc_nacionalidad = "NO EXISTE"
      END IF

      LET pos = pos +1
   END FOREACH

END FUNCTION

FUNCTION Modifica_no_cert()
#--------------------------

    DEFINE xx_status    SMALLINT
    DEFINE rescato      SMALLINT
    DEFINE vfecha_modif DATE

    LET vfecha_modif = TODAY

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " MODIFICA NO CERT " AT 1,59 ATTRIBUTE(REVERSE)
    DISPLAY " [ Esc ] Modifica [ Ctrl-C ] Salir sin Modificar " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY " CONTROL : [v] Otros Datos  [t] Datos modificados  [b] Despliega               " AT 2,1 ATTRIBUTE(BOLD)

    IF g_master.n_seguro IS NULL THEN
        CALL Inicializa()
    END IF

    LET sw_1 = 0 

    LET g_master.cod_error_origen = NULL

    INPUT BY NAME g_master.*
        BEFORE FIELD codven
        NEXT FIELD tipo_solicitud

        AFTER FIELD tipo_solicitud
            IF g_master.tipo_solicitud IS NULL THEN
                NEXT FIELD n_seguro
            END IF

            IF g_master.tipo_solicitud < 1 OR
               g_master.tipo_solicitud > 3 THEN
                NEXT FIELD tipo_solicitud
            ELSE
                NEXT FIELD n_folio
            END IF

        AFTER FIELD n_folio
            IF g_master.n_folio IS NULL THEN
                NEXT FIELD n_folio
            ELSE
                IF NOT Rescata_datos("N",g_master.n_folio,
                                     g_master.tipo_solicitud) THEN
                    ERROR "Afiliado NO existe"
                    NEXT FIELD n_folio
                ELSE
                    LET g_master1.* = g_master.*
                END IF
            END IF

            DISPLAY BY NAME g_master.*

            NEXT FIELD edo_civil

        AFTER FIELD n_seguro
            IF g_master.n_seguro IS NULL THEN
                NEXT FIELD tipo_solicitud
            ELSE
                IF NOT Rescata_datos("C",g_master.n_seguro,0) THEN
                    ERROR "Afiliado NO existe"
                    NEXT FIELD n_seguro
                ELSE
                    LET g_master1.* = g_master.*
                END IF
            END IF

            DISPLAY BY NAME g_master.*

            NEXT FIELD edo_civil

        AFTER FIELD edo_civil
            IF g_master.edo_civil IS NULL THEN
                CALL Despliega_estados_civiles()
                RETURNING g_master.edo_civil, g_master.desc_edo_civil
            ELSE
                SELECT ecivi_desc
                INTO   g_master.desc_edo_civil
                FROM   tab_edo_civil
                WHERE  ecivi_cod = g_master.edo_civil

                IF STATUS = NOTFOUND THEN
                    ERROR "Estado Civil Inexistente"
                    NEXT FIELD edo_civil
                END IF
            END IF

            DISPLAY BY NAME g_master.edo_civil,g_master.desc_edo_civil

            NEXT FIELD salario_base_comis

        AFTER FIELD salario_base_comis
            IF g_master.salario_base_comis IS NULL THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD salario_base_comis
            END IF

            NEXT FIELD ind_infonavit

        AFTER FIELD ind_infonavit
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD doc_prob
            END IF

            IF g_master.ind_infonavit IS NULL THEN
                ERROR "Se requiere la clave de prestamo del INFONAVIT"
                NEXT FIELD ind_infonavit
            END IF

            IF g_master.ind_infonavit NOT MATCHES "[SsNn]" THEN
                ERROR "Solo se acepta (s)i o (n)o"
                NEXT FIELD ind_infonavit
            END IF

    ---   fin agrega ultima validacion

        ON KEY ( CONTROL-T )
            SELECT "X"
            FROM   afi_mae_modifica md
            WHERE  md.n_seguro = g_master.n_seguro
            AND    md.n_folio = g_master.n_folio
            AND    md.tipo_solicitud = g_master.tipo_solicitud
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
                CALL verifica_datos_modificados()
            ELSE
                PROMPT "Registro sin modificaciones, [Enter] p/continuar " 
                ATTRIBUTES (REVERSE)
                FOR enter
            END IF

        ON KEY ( CONTROL-B )
            CALL Despliega()

            DISPLAY BY NAME g_master.n_folio

        ON KEY ( INTERRUPT )
            CALL Inicializa()
            DISPLAY "                                                                          " AT 6,2  ATTRIBUTE(REVERSE)
            DISPLAY "                                                                               " AT 14,1 ATTRIBUTE(REVERSE)
            DISPLAY "                  " AT 1,59 
            EXIT INPUT

        ON KEY ( ESC )
            WHILE TRUE
                CALL Desea_modificar()

                IF aux_pausa MATCHES "[SsNn]" THEN
                    EXIT WHILE
                END IF

            END WHILE

            IF aux_pausa MATCHES "[Nn]" THEN
                RETURN
            ELSE
                INSERT INTO afi_mae_modifica
                VALUES (g_master1.tipo_solicitud     ,
                        g_master1.n_folio            ,
                        g_master1.fecha_elaboracion  ,
                        g_master1.folio_edo_cta      ,
                        g_master1.cod_afore_ced      ,
                        g_master1.fecha_emision      ,
                        g_master1.frecafor           ,
                        g_master1.paterno            ,
                        g_master1.materno            ,
                        g_master1.nombres            ,
                        g_master1.n_seguro           ,
                        g_master1.n_rfc              ,
                        g_master1.n_unico            ,
                        g_master1.sexo               ,
                        g_master1.edo_civil          ,
                        g_master1.fena               ,
                        g_master1.salario_base_comis ,
                        g_master1.estadon            ,
                        g_master1.nacionalidad       ,
                        g_master1.tip_prob           ,
                        g_master1.fol_prob           ,
                        g_master1.doc_prob           ,
                        g_master1.ind_infonavit      ,
                        g_master1.cod_error_origen   ,
                        g_master1.const_curp         ,
                        HOY                          ,
                        g_usuario                    ,
                        1                            ,
                        532                          ,
                        vstatusint                   ,
                        ''                           ,
                        ''                           ,
                        '')

                UPDATE afi_mae_afiliado
                SET    salario_base_comis = g_master.salario_base_comis,
                       ind_infonavit      = g_master.ind_infonavit,
                       edo_civil          = g_master.edo_civil,
                       usuario            = g_usuario
                WHERE  n_seguro           = g_master.n_seguro
                AND    n_folio            = g_master.n_folio
                AND    tipo_solicitud     = g_master.tipo_solicitud
            END IF

        CALL Inicializa()
        NEXT FIELD tipo_solicitud

        ON KEY ( CONTROL-V )
            LET COMMA = "fglgo AFIM012.4gi ",
                        g_master.n_folio,
                        " ",
                        ACCION,
                        " ",
                        vcont," ",
                        g_master.tipo_solicitud
            RUN COMMA

    END INPUT

END FUNCTION

FUNCTION motivo_renapo()
#mr---------------------

   DEFINE a ARRAY[100] OF RECORD
       codigo           SMALLINT,
       descripcion      CHAR(80),
       fecha_asignacion DATE,
       fecha_rechazo    DATE,
       paterno          CHAR(40),
       materno          CHAR(40),
       nombres          CHAR(40)
   END RECORD

   DEFINE i SMALLINT

   OPEN WINDOW v35 AT 6,3 WITH FORM "AFIM0114" ATTRIBUTE(BORDER)
   DISPLAY " [ Ctrl_c ] Salir " AT 1,1
   DISPLAY "                    MOTIVOS DE RECHAZO RENAPO                                  " AT 3,1 ATTRIBUTE(REVERSE)

   DECLARE cursor_c CURSOR FOR
   SELECT m.status_renapo,
          d.status_desc,
          m.fecha_asignacion,
          n.fecha_rechazo,
          n.paterno,
          n.materno,
          n.nombres
   FROM   afi_dispersa_curp m, 
          tab_status_renapo d,
   OUTER  afi_status_tp5 n
   WHERE  m.n_seguro = g_master.n_seguro
   AND    m.n_seguro = n.n_seguro
   AND    m.status_renapo = n.status_renapo
   AND    m.fecha_actualiza = n.fecha_actualiza
   AND    d.status_cod = m.status_renapo 
   ORDER BY 4

   LET i = 1

   FOREACH cursor_c INTO a[i].*
      LET i = i + 1
   END FOREACH

   IF (i-1) >= 1 THEN
      CALL SET_COUNT(i-1)

      DISPLAY ARRAY a TO scr_1.*
         ON KEY (INTERRUPT)
            EXIT DISPLAY
      END DISPLAY
   ELSE
       PROMPT "Afiliado no tiene dispersiones de Renapo, [Enter] p/continuar"
       FOR enter 
       ATTRIBUTES(reverse) 
   END IF

   CLOSE WINDOW v35

END FUNCTION

FUNCTION marca_cuenta()
#mc--------------------

    LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
                  "'",g_master.n_seguro,"'",
                  ",",edo_proc,
                  ",",vmarca_estado,
                  ",",vcodigo_rechazo,",",
                  "'",g_usuario,"'",")"

    LET ejecuta = ejecuta CLIPPED

    PREPARE clausula_spl FROM ejecuta

    DECLARE cursor_marca CURSOR FOR clausula_spl

    OPEN cursor_marca

    FETCH cursor_marca INTO xcodigo_marca, xcodigo_rechazo

    CLOSE cursor_marca

END FUNCTION

FUNCTION inserta_logico(st_int, desc_oper)
#il---------------------------------------

    DEFINE
        st_int    SMALLINT,
        desc_oper CHAR(40)

    LET g_hora  = TIME

    INSERT INTO safre_af:afi_ctr_logico
    VALUES (g_master.n_folio,
            g_master.tipo_solicitud,
            g_master.n_seguro,
            st_int,
            g_usuario,
            HOY,
            g_hora,
            desc_oper)

END FUNCTION

