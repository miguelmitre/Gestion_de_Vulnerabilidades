############################################################################
#Proyecto          => Sistema de Afores. (MEXICO)                          #
#Propietario       => E.F.P                                                #
#Programa AFIM018  => MANTENIMIENTO DE SOLICITUDES DE AFILIACION           #
#Sistema           => AFI.                                                 #
#Autor             => MAURO MUNIZ CABALLERO                                #
#Fecha             => 28 de noviembre de 2000.                             #
#Modifico          => MAURO MUNIZ CABALLERO                                #
#Fecha             => 22 de julio de 2005                                  #
#                     Adecuaciones para trabajadores independientes        #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE
        des_promotor     CHAR(10),
        des_estatus      CHAR(12),
        des_nss          CHAR(11),
        c_doc            CHAR(01),
        des_folio        DECIMAL(8,0),
        bnd_dg           SMALLINT,
        st_int           SMALLINT,
        su_estatus       SMALLINT,
        su_st_nvo        SMALLINT,
        xx               SMALLINT,
        yy               SMALLINT,
        sw_1             SMALLINT,
        digito           SMALLINT,
        status_interno   SMALLINT,
        super            SMALLINT,
        g_status_captura SMALLINT,
        hay_datos1       SMALLINT,
        hay_datos2       SMALLINT,
        sw_sup           SMALLINT,
        vafore_f         SMALLINT,
        KEY              INTEGER

    DEFINE
        x_num_folio     INTEGER,
        vcont           INTEGER,
        gbene_rowid     INTEGER,
        gsiefo_rowid    INTEGER

    DEFINE
        vnss       DECIMAL(11,0),
        folio_prob DECIMAL(10,0)

    DEFINE
        enter      CHAR(1)  ,
        aux_pausa  CHAR(1)  ,
        aux_sup    CHAR(1)  ,
        ACCION     CHAR(1)  ,
        cod_err    CHAR(4)  ,
        g_usuario  CHAR(8)  ,
        g_hora     CHAR(8)  ,
        x_fecha    CHAR(10) ,
        xx_fecha   CHAR(10) ,
        desc_solic CHAR(15) ,
        valida_dg  CHAR(20) ,
        pat        CHAR(40) ,
        mat        CHAR(40) ,
        nom        CHAR(40) ,
        operacion  CHAR(40) ,
        comma      CHAR(200),
        comando    CHAR(250) 

    DEFINE
        HOY  DATE,
        x_fecha_cambio DATE

    DEFINE
        g_folio_edo_cta       CHAR(8)       ,
        g_edo_civil           SMALLINT      ,
        g_salario_base_comis  DECIMAL(12,2) ,
        g_n_operac            INTEGER       

    DEFINE r_afi RECORD
        tipo_solicitud      SMALLINT      ,
        desc_solicitud      CHAR(30)      ,
        n_folio         DECIMAL(10,0)       ,
        fecha_elaboracion   DATE          ,
        cod_afore_ced       SMALLINT      ,
        desc_afore          CHAR(50)      ,
        fecha_emision       DATE          ,
        frecafor            DATE          ,
        fecha_cap           DATE          ,
        n_seguro            CHAR(11)      ,
        n_unico             CHAR(18)      ,
        n_rfc               CHAR(13)      ,
        paterno             CHAR(40)      ,
        materno             CHAR(40)      ,
        nombres             CHAR(40)      ,
        estadon             SMALLINT      ,
        desc_estadon        CHAR(60)      ,
        nacionalidad        CHAR(3)       ,
        desc_nacionalidad   CHAR(60)      ,
        fena                DATE          ,
        sexo                SMALLINT      ,
        desc_sexo           CHAR(60)      ,
        ind_infonavit       CHAR(1)       ,
        cve_rend_neto       CHAR(1)       , ---28-18
        tip_prob            CHAR(1)       ,
        docprob_desc        CHAR(20)      ,
        fol_prob            DECIMAL(10,0) ,
        doc_prob            CHAR(16)      ,
        usuario1            CHAR(8)       ,
        cod_error_orig      SMALLINT      ,
        profesion_cod       SMALLINT      ,
        profesion_desc      CHAR(50)      ,
        actividad_cod       SMALLINT      ,
        actividad_desc      CHAR(50)      ,
        cod_promotor        CHAR(10)      ,
        nom_promotor        CHAR(60)      ,
        codven              CHAR(10)      ,
        desc_codven         CHAR(50)      ,
        agenc_cod           CHAR(10)      ,
        agenc_desc          CHAR(60)      

    END RECORD

    DEFINE g_afore RECORD LIKE tab_afore.*
    DEFINE r_cod   RECORD LIKE tab_supervisor.*

    DEFINE g_comision RECORD
        cod_esq_comision  SMALLINT,
        desc_esq_comision CHAR(50)
    END RECORD

      DEFINE reg  ARRAY[30000] OF RECORD
        usuario          CHAR(08) ,
        n_seguro         CHAR(11) ,
        n_folio         DECIMAL(10,0)  ,
        tipo_solicitud   SMALLINT ,
        n_unico          CHAR(18) ,
        cod_promotor     CHAR(10) ,
        status_interno   SMALLINT ,
        desc_status_int  CHAR(12)
    END RECORD

    DEFINE z_reg RECORD
        orden_1  SMALLINT ,
        orden_2  SMALLINT ,
        orden_3  SMALLINT ,
        orden_4  SMALLINT ,
        orden_5  SMALLINT
    END RECORD

    DEFINE
        z_reg2 ARRAY[6] OF SMALLINT

    DEFINE
       dcto_2        CHAR(1),
       dcto_3        CHAR(9),
       dcto_4        CHAR(7),
       dcto_7        CHAR(7)

    DEFINE
       r_nac         SMALLINT,
       a_nac         SMALLINT,
       b_nac         SMALLINT

    DEFINE xn_fena      CHAR(10)
    DEFINE vstatus_desc CHAR(40)

    DEFINE sexo_cur     CHAR(1)
    DEFINE desc_err     CHAR(60)
    DEFINE pasa_curp    SMALLINT
    DEFINE dig_curp     SMALLINT
    DEFINE bnd_fena     SMALLINT
    DEFINE tot_dias     INTEGER
    DEFINE cve_arma     CHAR(10)

    DEFINE max_ceo      SMALLINT

    DEFINE vtipo_administracion CHAR(02)
    DEFINE vdesc_tip_admon      CHAR(20)

END GLOBALS

MAIN

    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        --ACCEPT KEY CONTROL-I,
        COMMENT LINE LAST
        DEFER INTERRUPT

    CALL STARTLOG("AFIM018.log")
    CALL inicio()            #i
    CALL proceso_principal() #pp

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY = TODAY

    SELECT *, USER
    INTO   g_afore.*, g_usuario
    FROM   tab_afore
    WHERE  marca = 1

    LET aux_sup = 'S'
    LET sw_sup  = FALSE
    LET max_ceo = 2047

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIM0188" ATTRIBUTE(BORDER)
    DISPLAY " AFIM018             CONFIRMACION SOLICITUDES AFILIACION                      " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                              " AT 5,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "Confirmar"
        COMMAND "Confirma" "Confirmacion Solicitud "
            LET ACCION = "M"
                CALL elige_solicitud("C")
                CALL Inicializa()
        COMMAND "Modifica" "Modificacion Solicitud "
            LET ACCION = "M"
            IF sw_sup = FALSE THEN
                CALL Ingresa_autoriza()
            END IF
            IF aux_sup = "N" THEN
                CALL elige_solicitud("M")
                CALL Inicializa()
            END IF
        COMMAND "Baja" "Baja Solicitud "
            LET ACCION = "B"
                CALL elige_solicitud("B")
                CALL Inicializa()
        COMMAND "Alta" "Alta Solicitud "
            LET ACCION = "A"
                CALL elige_solicitud("A")
                CALL Inicializa()
        COMMAND "Folio" "Cambia Folio solicitud "
            LET ACCION = "F"
                CALL elige_solicitud("F")
                CALL Inicializa()
        COMMAND "NSS" "Cambia NSS solicitud "
            LET ACCION = "S"
                CALL elige_solicitud("S")
                CALL Inicializa()
        COMMAND "Promotor" "Cambia Cod. Promotor solicitud "
            LET ACCION = "P"
                CALL elige_solicitud("P")
                CALL Inicializa()
        COMMAND "Salir" "Salir de Programa"
            EXIT MENU
    END MENU

END FUNCTION

FUNCTION Inicializa()
#iz------------------

    DEFINE i SMALLINT

    INITIALIZE   g_folio_edo_cta  TO NULL
    INITIALIZE   g_edo_civil TO NULL

    LET g_edo_civil             = 0
    LET g_salario_base_comis    = 0
    LET g_n_operac              = 0

    INITIALIZE r_afi.* TO NULL
    INITIALIZE g_comision.* TO NULL
    INITIALIZE cod_err TO NULL
    INITIALIZE reg TO NULL

    DISPLAY "                                        " AT 5,56
    ATTRIBUTE(REVERSE)

    CLEAR FORM

END FUNCTION

FUNCTION Ingresa_autoriza()
#ia------------------------

    DEFINE cod      DECIMAL(10,0)
    DEFINE pos      SMALLINT
    DEFINE desc     CHAR(60)
    DEFINE x_buscar CHAR(60)
    DEFINE x_texto  CHAR(200)

    LET aux_sup = "S"

    OPEN WINDOW ventanilla_super AT 8,4 WITH FORM "AFIM0015" ATTRIBUTE(BORDER)
    DISPLAY "                      OPCION RESERVADA PARA SUPERVISORES                       " AT 3,1 ATTRIBUTE ( REVERSE)

    INPUT BY NAME r_cod.super_cod,r_cod.super_desc,r_cod.nip

    AFTER FIELD super_cod
        IF r_cod.super_cod IS NULL THEN
            CALL Despliega_supervisores()
                 RETURNING r_cod.super_cod, r_cod.super_desc

            SELECT area_cod, super_desc 
            INTO   r_cod.area_cod, r_cod.super_desc
            FROM   tab_supervisor 
            WHERE  super_cod = r_cod.super_cod

            IF STATUS = NOTFOUND THEN
                ERROR "No existe codigo"
                NEXT FIELD super_cod
            END IF
        ELSE
            SELECT area_cod, super_desc
            INTO   r_cod.area_cod, r_cod.super_desc
            FROM   tab_supervisor 
            WHERE  super_cod = r_cod.super_cod

            IF STATUS = NOTFOUND THEN
                ERROR "Clave de supervisor invalida "
                SLEEP 3
                NEXT FIELD super_cod
            END IF
        END IF

        DISPLAY BY NAME r_cod.super_cod, r_cod.super_desc

    AFTER FIELD nip
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD super_cod 
        END IF

        IF r_cod.nip IS NULL THEN
            LET aux_sup = "S"
            ERROR "El campo NIP no puede ser nulo"
            NEXT FIELD nip
        END IF

        SELECT "X" 
        FROM   tab_supervisor
        WHERE  nip = r_cod.nip
        AND    super_cod = r_cod.super_cod

        IF STATUS = NOTFOUND THEN
            ERROR "Permiso denegado, nip incorrecto"
            LET r_cod.nip = NULL
            LET aux_sup = "S"
            NEXT FIELD nip
        ELSE 
            LET aux_sup = "N"
            LET sw_sup = TRUE
            EXIT INPUT
        END IF

    ON KEY ( INTERRUPT )
        LET aux_sup = "S"
        EXIT INPUT

    END INPUT

    CLOSE WINDOW ventanilla_super

END FUNCTION

FUNCTION actualiza()
#ac-----------------

    UPDATE afi_solicitud
    SET    paterno            = r_afi.paterno           ,
           materno            = r_afi.materno           ,
           nombres            = r_afi.nombres           ,
           ---n_seguro           = r_afi.n_seguro       ,
           n_rfc              = r_afi.n_rfc             ,
           n_unico            = r_afi.n_unico           ,
           sexo               = r_afi.sexo              ,
           edo_civil          = g_edo_civil             ,
           fena               = r_afi.fena              ,
           estadon            = r_afi.estadon           ,
           nacionalidad       = r_afi.nacionalidad      ,
           tip_prob           = r_afi.tip_prob          ,
           fol_prob           = r_afi.fol_prob          ,
           doc_prob           = r_afi.doc_prob          ,
           ind_infonavit      = r_afi.ind_infonavit     ,
           salario_base_comis = g_salario_base_comis    ,
           status_captura     = 0                       ,
           cod_error_origen   = cod_err                 ,
           folio_edo_cta      = g_folio_edo_cta         ,
           cod_afore_ced      = r_afi.cod_afore_ced     ,
           femision           = r_afi.fecha_emision
    WHERE  n_folio            = r_afi.n_folio
    AND    tipo_solicitud     = r_afi.tipo_solicitud

    CALL inserta_afi_ctr_act(r_afi.n_seguro,r_afi.n_folio,r_afi.tipo_solicitud)

END FUNCTION

FUNCTION inserta_afi_ctr_act(vn_seguro, vn_folio, vtipo_solicitud)
#aca--------------------------------------------------------------

    DEFINE vn_seguro         CHAR(11)
    DEFINE vn_folio          DECIMAL(10,0)
    DEFINE vtipo_solicitud   SMALLINT

    SELECT 'X'
      FROM afi_ctr_actividad
     WHERE @nss            = r_afi.n_seguro
       AND @n_folio        = r_afi.n_folio
       AND @tipo_solicitud = r_afi.tipo_solicitud

    IF SQLCA.SQLCODE = 0 THEN
        UPDATE afi_ctr_actividad
           SET profesion_cod    = r_afi.profesion_cod,
               actividad_cod    = r_afi.actividad_cod,
               usuario          = g_usuario,
               factualiza       = HOY
         WHERE nss            = r_afi.n_seguro AND
               n_folio        = r_afi.n_folio  AND
               tipo_solicitud = r_afi.tipo_solicitud
    ELSE
        INSERT INTO afi_ctr_actividad
        VALUES(r_afi.n_seguro,
               r_afi.n_folio,
               r_afi.tipo_solicitud,
               r_afi.profesion_cod,
               r_afi.actividad_cod,
               g_usuario,
               hoy)
    END IF

END FUNCTION

FUNCTION Rescata_datos(l_aux_val,x_valor)
#rd--------------------------------------

    DEFINE l_aux_val CHAR(1)
    DEFINE x_valor   CHAR(11)
    DEFINE xx_num    DECIMAL(11,0)
    DEFINE tot_afil  SMALLINT

    CASE l_aux_val
      WHEN "C"
        SELECT COUNT(*) 
        INTO   tot_afil
        FROM   afi_solicitud a
        WHERE  a.n_seguro       = x_valor
        AND    a.tipo_solicitud = r_afi.tipo_solicitud

        IF tot_afil > 1 THEN
            PROMPT "Existe mas de un registro con tipo solic. ", 
                   r_afi.tipo_solicitud,", [Enter] p/continuar " 
                   ATTRIBUTES(REVERSE) FOR enter
            CALL Inicializa()
        ELSE
            SELECT a.codven,
                   a.agenc_cod,
                   a.cod_promotor,
                   a.n_folio,
                   a.frecafor,
                   a.paterno,
                   a.materno,
                   a.nombres,
                   a.n_seguro,
                   a.n_unico,
                   a.n_rfc,
                   a.sexo,
                   a.fena,
                   a.estadon,
                   a.status_interno,
                   a.tip_prob,
                   a.fol_prob,
                   a.doc_prob,
                   a.ind_infonavit,
                   a.nacionalidad,
                   a.tipo_solicitud,
                   a.fecha_elaboracion,
                   a.cod_afore_ced, 
                   a.cod_error_origen,
                   a.femision,
                   a.usuario
            INTO   r_afi.codven,
                   r_afi.agenc_cod,
                   r_afi.cod_promotor,
                   r_afi.n_folio,
                   r_afi.frecafor,
                   r_afi.paterno,
                   r_afi.materno,
                   r_afi.nombres,
                   r_afi.n_seguro,
                   r_afi.n_unico,
                   r_afi.n_rfc,
                   r_afi.sexo,
                   r_afi.fena,
                   r_afi.estadon,
                   su_estatus,
                   r_afi.tip_prob,
                   r_afi.fol_prob,
                   r_afi.doc_prob,
                   r_afi.ind_infonavit,
                   r_afi.nacionalidad,
                   r_afi.tipo_solicitud,
                   r_afi.fecha_elaboracion,
                   r_afi.cod_afore_ced,
                   r_afi.cod_error_orig,
                   r_afi.fecha_emision,
                   r_afi.usuario1     
            FROM   afi_solicitud a
            WHERE  a.n_seguro       = x_valor
            AND    a.tipo_solicitud = r_afi.tipo_solicitud

            IF STATUS = NOTFOUND THEN
                RETURN FALSE
            END IF

            IF r_afi.tipo_solicitud <> 2 THEN
                LET r_afi.fecha_emision = NULL 
                LET r_afi.cod_afore_ced = NULL 
            END IF
        END IF
      WHEN "N"
        LET xx_num = x_valor clipped

        SELECT a.codven,
               a.agenc_cod,
               a.cod_promotor,
               a.n_folio,
               a.frecafor,
               a.paterno,
               a.materno,
               a.nombres,
               a.n_seguro,
               a.n_unico,
               a.n_rfc,
               a.sexo,
               a.fena,
               a.estadon,
               a.status_interno,
               a.tip_prob,
               a.fol_prob,
               a.doc_prob,
               a.ind_infonavit,
               a.nacionalidad,
               a.tipo_solicitud,
               a.fecha_elaboracion,
               a.cod_afore_ced, 
               a.cod_error_origen,
               a.femision,      
               a.usuario     
        INTO   r_afi.codven,
               r_afi.agenc_cod,
               r_afi.cod_promotor,
               r_afi.n_folio,
               r_afi.frecafor,
               r_afi.paterno,
               r_afi.materno,
               r_afi.nombres,
               r_afi.n_seguro,
               r_afi.n_unico,
               r_afi.n_rfc,
               r_afi.sexo,
               r_afi.fena,
               r_afi.estadon,
               su_estatus,
               r_afi.tip_prob,
               r_afi.fol_prob,
               r_afi.doc_prob,
               r_afi.ind_infonavit,
               r_afi.nacionalidad,
               r_afi.tipo_solicitud,
               r_afi.fecha_elaboracion,
               r_afi.cod_afore_ced,
               r_afi.cod_error_orig,
               r_afi.fecha_emision,
               r_afi.usuario1     
        FROM   afi_solicitud a
        WHERE  a.n_folio = xx_num
        AND    a.tipo_solicitud = r_afi.tipo_solicitud

        IF STATUS = NOTFOUND THEN
            RETURN FALSE
        END IF

        IF r_afi.tipo_solicitud <> 2 THEN
            LET r_afi.fecha_emision = NULL 
            LET r_afi.cod_afore_ced = NULL 
        END IF
      WHEN "O"
        LET xx_num = x_valor clipped

        SELECT a.codven,
               a.agenc_cod,
               a.cod_promotor,
               a.n_folio,
               a.frecafor,
               a.paterno,
               a.materno,
               a.nombres,
               a.n_seguro,
               a.n_unico,
               a.n_rfc,
               a.sexo,
               a.fena,
               a.estadon,
               a.status_interno,
               a.tip_prob,
               a.fol_prob,
               a.doc_prob,
               a.ind_infonavit,
               a.nacionalidad,
               a.tipo_solicitud,
               a.fecha_elaboracion,
               a.cod_afore_ced, 
               a.cod_error_origen,
               a.femision,      
               a.usuario
        INTO   r_afi.codven,
               r_afi.agenc_cod,
               r_afi.cod_promotor,
               r_afi.n_folio,
               r_afi.frecafor,
               r_afi.paterno,
               r_afi.materno,
               r_afi.nombres,
               r_afi.n_seguro,
               r_afi.n_unico,
               r_afi.n_rfc,
               r_afi.sexo,
               r_afi.fena,
               r_afi.estadon,
               su_estatus,
               r_afi.tip_prob,
               r_afi.fol_prob,
               r_afi.doc_prob,
               r_afi.ind_infonavit,
               r_afi.nacionalidad,
               r_afi.tipo_solicitud,
               r_afi.fecha_elaboracion,
               r_afi.cod_afore_ced,
               r_afi.cod_error_orig,
               r_afi.usuario1
        FROM   afi_solicitud a
        WHERE  a.tipo_solicitud = r_afi.tipo_solicitud

        IF STATUS = NOTFOUND THEN
            RETURN FALSE
        END IF

        IF r_afi.tipo_solicitud <> 2 THEN
            LET r_afi.fecha_emision = NULL 
            LET r_afi.cod_afore_ced = NULL 
        END IF
    END CASE

    SELECT paterno,
           materno,
           nombres
    INTO   pat,
           mat,
           nom
    FROM   pro_mae_promotor
    WHERE  cod_promotor = r_afi.codven

    IF STATUS = NOTFOUND THEN
        LET r_afi.desc_codven = "NO EXISTE"
    ELSE
        LET r_afi.desc_codven = pat CLIPPED," ",
                                   mat CLIPPED," ",
                                   nom CLIPPED
    END IF

    SELECT paterno,
           materno,
           nombres
    INTO   pat,
           mat,
           nom
    FROM   pro_mae_promotor
    WHERE  cod_promotor = r_afi.cod_promotor

    IF STATUS = NOTFOUND THEN
        LET r_afi.nom_promotor = "NO EXISTE"
    ELSE
        LET r_afi.nom_promotor = pat CLIPPED," ",
                                    mat CLIPPED," ",
                                    nom CLIPPED
    END IF

    SELECT cod_esq_comision
    INTO   g_comision.cod_esq_comision
    FROM   afi_solicitud
    WHERE  n_folio = r_afi.n_folio
    AND    tipo_solicitud = r_afi.tipo_solicitud

    SELECT nombre_uni_n1 
    INTO   r_afi.agenc_desc 
    FROM   com_nivel1
    WHERE  coduni_n1 = r_afi.agenc_cod

    IF STATUS = NOTFOUND THEN
        LET r_afi.agenc_desc = "NO EXISTE"
    END IF

    SELECT sexo_desc 
    INTO   r_afi.desc_sexo 
    FROM   tab_sexo
    WHERE  sexo_cod = r_afi.sexo

    IF STATUS = NOTFOUND THEN
        LET r_afi.desc_sexo = "NO EXISTE"
    END IF

    SELECT estad_desc 
    INTO   r_afi.desc_estadon 
    FROM   tab_estado
    WHERE  estad_cod = r_afi.estadon

    IF STATUS = NOTFOUND THEN
        LET r_afi.desc_estadon = "NO EXISTE"
    END IF

    SELECT pais_desc 
    INTO   r_afi.desc_nacionalidad 
    FROM   tab_pais
    WHERE  pais_cod = r_afi.nacionalidad

    IF STATUS = NOTFOUND THEN
        LET r_afi.desc_nacionalidad = "NO EXISTE"
    END IF

    CALL rescata_status(su_estatus)

    SELECT docprob_desc 
    INTO   r_afi.docprob_desc 
    FROM   tab_doc_prob
    WHERE  docprob_cod = r_afi.tip_prob

    IF STATUS = NOTFOUND THEN
        LET r_afi.docprob_desc = "NO EXISTE"
    END IF

    IF r_afi.tipo_solicitud = 2 THEN
        SELECT afore_desc
        INTO   r_afi.desc_afore
        FROM   tab_afore
        WHERE  afore_cod = r_afi.cod_afore_ced

        IF STATUS = NOTFOUND THEN
            LET r_afi.desc_afore = " Cod. Afore NO EXISTE "
        END IF
    END IF

  # rescata fecha de captura
    LET r_afi.fecha_cap = r_afi.frecafor

    RETURN TRUE

END FUNCTION

FUNCTION modifica(vn_seguro, vn_folio, vtipo_solicitud,vtipo_oper)
#m-----------------------------------------------------------------

    DEFINE  vn_seguro        LIKE  afi_solicitud.n_seguro
    DEFINE  vn_folio         LIKE  afi_solicitud.n_folio
    DEFINE  vtipo_solicitud  LIKE  afi_solicitud.tipo_solicitud
    DEFINE  vtipo_oper       CHAR(1)
    DEFINE fecha_comprueba   DATE
    DEFINE xx_status         SMALLINT
    DEFINE rescato           SMALLINT
    DEFINE v_1               SMALLINT
    DEFINE a_yo_act          SMALLINT
    DEFINE a_yo_fena         SMALLINT
    DEFINE a_yo              SMALLINT
    DEFINE bla               SMALLINT
    DEFINE ban               SMALLINT
    DEFINE sino              SMALLINT
    DEFINE val_1             CHAR(80)
    DEFINE doc_prob_arma     CHAR(16)

    DEFINE vtip_prob_orig    SMALLINT
    DEFINE vdoc_prob_orig    CHAR(16)
    DEFINE vcurp_prob        CHAR(16)
    DEFINE vrfc_si           CHAR(4)
    DEFINE vrfc_orig         CHAR(4)

    OPEN WINDOW ventana_4 AT 2,2 WITH FORM "AFIM0011" ATTRIBUTE(BORDER)
    DISPLAY " AFIM018             CONFIRMACION SOLICITUDES AFILIACION                      " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                              " AT 5,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    DISPLAY "                                                                               " AT 13,1 ATTRIBUTE(REVERSE)
    DISPLAY "                          Estructura Comercial                                 "AT 18,1 ATTRIBUTE(REVERSE)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1

    IF vtipo_oper = 'C' THEN
        DISPLAY " CONFIRMA " AT 1,69 ATTRIBUTE(REVERSE)
    ELSE
        DISPLAY " MODIFICA " AT 1,69 ATTRIBUTE(REVERSE)
    END IF

    DISPLAY "[Esc] Graba [Ctrl-C] Salir sin Grabar [F1] Digit [F2] Cons.digit"
            AT 1,1
    DISPLAY "CTRL:[E]Dom [T]Tel [B]Benef [V]Patrones [W]Obser [Y]Iden [P]Icef [N] Tipo Adm" AT 2,1 ATTRIBUTE(BOLD) 

    LET sw_1 = 0
    LET rescato = TRUE
    LET int_flag = FALSE
    LET aux_pausa = 'N'

    INPUT BY NAME r_afi.* WITHOUT DEFAULTS

        BEFORE INPUT
            SELECT  a.cod_promotor,
                    p.nombres, 
                    p.paterno, 
                    p.materno,
                    a.codven,
                    a.agenc_cod,
                    nombre_uni_n1,
                    a.tipo_solicitud, 
                    a.n_folio, 
                    a.fecha_elaboracion, 
                    a.cod_afore_ced, 
                    afore_desc, 
                    a.femision,  
                    a.frecafor, 
                    a.paterno, 
                    a.materno, 
                    a.nombres,  
                    a.n_seguro, 
                    a.n_rfc, 
                    a.n_unico, 
                    a.sexo, 
                    sexo_desc, 
                    a.fena, 
                    a.ind_infonavit, 
                    a.estadon,
                    estad_desc, 
                    a.nacionalidad,
                    pais_desc, 
                    a.tip_prob,
                    docprob_desc, 
                    a.fol_prob, 
                    a.doc_prob, 
                    a.cod_error_origen,
                    a.status_interno,
                    a.usuario
              INTO
                    r_afi.cod_promotor, 
                    nom , 
                    pat,
                    mat,
                    r_afi.codven, 
                    r_afi.agenc_cod, 
                    r_afi.agenc_desc,
                    r_afi.tipo_solicitud, 
                    r_afi.n_folio, 
                    r_afi.fecha_elaboracion, 
                    r_afi.cod_afore_ced, 
                    r_afi.desc_afore, 
                    r_afi.fecha_emision, 
                    r_afi.frecafor, 
                    r_afi.paterno, 
                    r_afi.materno,
                    r_afi.nombres, 
                    r_afi.n_seguro, 
                    r_afi.n_rfc,
                    r_afi.n_unico, 
                    r_afi.sexo, 
                    r_afi.desc_sexo, 
                    r_afi.fena,
                    r_afi.ind_infonavit,
                    r_afi.estadon, 
                    r_afi.desc_estadon, 
                    r_afi.nacionalidad, 
                    r_afi.desc_nacionalidad,
                    r_afi.tip_prob, 
                    r_afi.docprob_desc,
                    r_afi.fol_prob, 
                    r_afi.doc_prob, 
                    r_afi.cod_error_orig,
                    su_estatus,
                    r_afi.usuario1 
              FROM  afi_solicitud a ,
                    pro_mae_promotor p,
                    tab_doc_prob td,
              OUTER com_nivel1 c,
              OUTER tab_afore tf ,
              OUTER tab_sexo  ts,
              OUTER tab_pais  tp,
              OUTER tab_estado tedo
              WHERE a.n_seguro       = vn_seguro
                AND a.n_folio        = vn_folio 
                AND a.tipo_solicitud = vtipo_solicitud
                AND a.cod_promotor   = p.cod_promotor
                AND a.agenc_cod      = c.coduni_n1
                AND a.cod_afore_ced  = tf.afore_cod
                AND a.sexo           = ts.sexo_cod
                AND a.nacionalidad   = tp.pais_cod
                AND a.estadon        = tedo.estad_cod
                AND a.tip_prob       = td.docprob_cod

         # obtiene fecha de captura 
                LET r_afi.fecha_cap = r_afi.frecafor

              SELECT  profesion_cod,actividad_cod
                 INTO  r_afi.profesion_cod,r_afi.actividad_cod
                 FROM  afi_ctr_actividad
                WHERE  nss = r_afi.n_seguro
                  AND  n_folio = r_afi.n_folio
                  AND  tipo_solicitud = r_afi.tipo_solicitud

            # obtiene descripciones de profesion y actividad
               SELECT  profesion_desc
                 INTO  r_afi.profesion_desc
                 FROM  tab_profesion
                WHERE  profesion_cod = r_afi.profesion_cod

               IF SQLCA.SQLCODE <> 0 THEN
                 INITIALIZE   r_afi.profesion_desc TO NULL
               END IF

               SELECT  actividad_desc
                 INTO  r_afi.actividad_desc
                 FROM  tab_actividad
                WHERE  actividad_cod = r_afi.actividad_cod

               IF SQLCA.SQLCODE <> 0 THEN
                 INITIALIZE   r_afi.actividad_desc TO NULL
               END IF


        LET r_afi.nom_promotor = pat CLIPPED, " ",
                                    mat CLIPPED, " ",
                                    nom CLIPPED

    DISPLAY BY NAME r_afi.*

--->28-18
    IF r_afi.tipo_solicitud = 2 THEN
       SELECT @cve_rend_neto
       INTO   r_afi.cve_rend_neto
       FROM   afi_folio_saftv
       WHERE  @n_folio        = r_afi.n_folio
       AND    @tipo_solicitud = r_afi.tipo_solicitud
    END IF

    DISPLAY BY NAME r_afi.cve_rend_neto
---<

# TIPO SOLICITUD

        AFTER FIELD tipo_solicitud
            IF r_afi.tipo_solicitud IS NULL THEN
                ERROR "Tipo de Registro  erroneo" 
                NEXT FIELD tipo_solicitud
            END IF

            IF r_afi.tipo_solicitud <> 1 AND
               r_afi.tipo_solicitud <> 2 AND 
               r_afi.tipo_solicitud <> 8 THEN
                ERROR "Tipo Solic. solo puede ser 1)Registro  2)Traspaso  8) Independiente"
                LET r_afi.tipo_solicitud = NULL
                DISPLAY BY NAME r_afi.tipo_solicitud
                NEXT FIELD tipo_solicitud
            END IF

            SELECT ts.desc_solicitud
            INTO   r_afi.desc_solicitud
            FROM   tab_tipo_solic ts
            WHERE  ts.tipo_solicitud = r_afi.tipo_solicitud

            IF SQLCA.SQLCODE = 0 THEN
                DISPLAY BY NAME r_afi.desc_solicitud
            ELSE
                ERROR "Tipo Solic. solo puede ser 1)Registro  2)Traspaso  8) Independiente"
                LET r_afi.tipo_solicitud = NULL
                DISPLAY BY NAME r_afi.tipo_solicitud
                NEXT FIELD tipo_solicitud
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                NEXT FIELD n_folio
            END IF

            NEXT FIELD n_folio

# FOLIO 

        AFTER FIELD n_folio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD tipo_solicitud
            END IF

            IF r_afi.n_folio IS NULL THEN
                NEXT FIELD n_folio
            ELSE
                IF rescato THEN
                    IF NOT Rescata_datos("N",r_afi.n_folio) THEN
                        ERROR "Afiliado NO existe"
                        NEXT FIELD n_folio
                    END IF

                    DISPLAY BY NAME r_afi.*
#OJOI
                    SELECT cod_esq_comision
                    INTO   g_comision.cod_esq_comision
                    FROM   afi_solicitud
                    WHERE  n_folio = r_afi.n_folio
                    AND    tipo_solicitud = r_afi.tipo_solicitud

                END IF
            END IF

            SELECT a.status_interno, b.estado_desc
            INTO   xx_status, vstatus_desc
            FROM   afi_solicitud a,
            OUTER  tab_status_afi b
            WHERE  a.n_seguro       = r_afi.n_seguro
            AND    a.n_folio        = r_afi.n_folio
            AND    a.tipo_solicitud = r_afi.tipo_solicitud
            AND    a.status_interno = b.estado_cod

            IF xx_status > 20 THEN
                ERROR "NO PUEDE MODIFICAR,", vstatus_desc
                NEXT FIELD tipo_solicitud
            END IF

        IF r_afi.tipo_solicitud <> 2 THEN
            LET r_afi.cod_afore_ced = ""
            LET r_afi.desc_afore    = ""

            DISPLAY BY NAME r_afi.cod_afore_ced,
                            r_afi.desc_afore

            NEXT FIELD paterno
        ELSE
            NEXT FIELD paterno
        END IF

        IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
           FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
           IF r_afi.tipo_solicitud <> 2 THEN
               LET r_afi.cod_afore_ced = ""
               LET r_afi.desc_afore    = ""

               DISPLAY BY NAME r_afi.cod_afore_ced,
                               r_afi.desc_afore

               NEXT FIELD paterno
           END IF
        END IF 

# PATERNO

        AFTER FIELD paterno
            IF r_afi.paterno IS NULL OR
               r_afi.paterno  =  " " THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD paterno
            END IF
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               IF r_afi.paterno IS NULL OR
                  r_afi.paterno = " "   THEN
                  ERROR "Campo Apellido Paterno NO puede ser nulo"
                  NEXT FIELD paterno
               ELSE
                  LET v_1 = 0
                  INITIALIZE val_1 TO NULL
                  CALL verifica_nombre(r_afi.paterno) RETURNING v_1,val_1 ##ve--
                  IF v_1 = 1 THEN
                     ERROR "A.Paterno ",val_1 CLIPPED
                     NEXT FIELD paterno
                  END IF
               END IF
               NEXT FIELD n_folio
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               IF r_afi.paterno IS NULL OR
                  r_afi.paterno = " "   THEN
                  ERROR "Campo Apellido Paterno NO puede ser nulo"
                  NEXT FIELD paterno
               ELSE
                  LET v_1 = 0
                  INITIALIZE val_1 TO NULL
                  CALL verifica_nombre(r_afi.paterno) RETURNING v_1,val_1 ##ve--
                  IF v_1 = 1 THEN
                     ERROR "A.Paterno ",val_1 CLIPPED
                     NEXT FIELD paterno
                  END IF
               END IF
                NEXT FIELD n_folio
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
               IF r_afi.paterno IS NULL OR
                  r_afi.paterno = " "   THEN
                  ERROR "Campo Apellido Paterno NO puede ser nulo"
                  NEXT FIELD paterno
               ELSE
                  LET v_1 = 0
                  INITIALIZE val_1 TO NULL
                  CALL verifica_nombre(r_afi.paterno) RETURNING v_1,val_1 ##ve--
                  IF v_1 = 1 THEN
                     ERROR "A.Paterno ",val_1 CLIPPED
                     NEXT FIELD paterno
                  END IF
               END IF
               NEXT FIELD materno
            END IF

            IF r_afi.paterno IS NOT NULL THEN
                  LET v_1 = 0
                  INITIALIZE val_1 TO NULL
                  CALL verifica_nombre(r_afi.paterno) RETURNING v_1,val_1 ##ve--
                  IF v_1 = 1 THEN
                     ERROR "A.Paterno ",val_1 CLIPPED
                     NEXT FIELD paterno
                  END IF
                  NEXT FIELD materno
            END IF

# MATERNO

        AFTER FIELD materno
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               NEXT FIELD paterno
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
               NEXT FIELD nombres
            END IF

            LET v_1 = 0
            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.materno) RETURNING v_1,val_1 ##ve--
            IF v_1 = 1 THEN
               ERROR "A.Materno ",val_1 CLIPPED
               NEXT FIELD materno
            END IF

# NOMBRE

        AFTER FIELD nombres
            IF r_afi.nombres IS NULL OR
               r_afi.nombres =  " "  THEN
                ERROR "Campo Nombre NO puede ser NULO"
                NEXT FIELD nombres
            ELSE
                LET v_1 = 0
                INITIALIZE val_1 TO NULL
                CALL verifica_nombre(r_afi.nombres) RETURNING v_1,val_1 
                IF v_1 = 1 THEN
                   ERROR "El Nombre ",val_1 CLIPPED
                   NEXT FIELD nombres
                END IF
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               IF r_afi.nombres IS NULL OR
                  r_afi.nombres = " "   THEN
                  ERROR "Campo Nombre NO puede ser NULO"
                  NEXT FIELD nombres
               ELSE
                  LET v_1 = 0
                  INITIALIZE val_1 TO NULL
                  CALL verifica_nombre(r_afi.nombres) RETURNING v_1,val_1 
                  IF v_1 = 1 THEN
                     ERROR "El Nombre ",val_1 CLIPPED
                     NEXT FIELD nombres
                  END IF
               END IF
                NEXT FIELD materno
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
               IF r_afi.nombres IS NULL OR
                  r_afi.nombres = " "   THEN
                  ERROR "Campo Nombre NO puede ser NULO"
                  NEXT FIELD nombres
               ELSE
                  LET v_1 = 0
                  INITIALIZE val_1 TO NULL
                  CALL verifica_nombre(r_afi.nombres) RETURNING v_1,val_1
                  IF v_1 = 1 THEN
                     ERROR "El Nombre ",val_1 CLIPPED
                     NEXT FIELD nombres
                  END IF
               END IF
               NEXT FIELD fena
            END IF
               NEXT FIELD fena

# FECHA DE NACIMIENTO

        BEFORE FIELD fena
            INITIALIZE x_fecha, xx_fecha TO NULL

            LET x_fecha = r_afi.n_rfc[7,8],"/",
                          r_afi.n_rfc[9,10],"/",
                          "19",r_afi.n_rfc[5,6]

        AFTER FIELD fena
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               NEXT FIELD nombres
            END IF

            IF r_afi.fena IS NULL THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD fena
            END IF

            LET xx_fecha = r_afi.fena

            LET a_yo_act  = 0 LET a_yo_fena = 0 LET a_yo = 0

            LET a_yo_act  = YEAR(TODAY) USING "&&&&"
            LET a_yo_fena = YEAR(r_afi.fena) USING "&&&&"
            LET a_yo      = a_yo_act - a_yo_fena
            IF a_yo > 120 THEN
               ERROR "Esta persona pasa del rango de 120 ayos, Verifique nuevamente"
               NEXT FIELD fena
            END IF

           IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
              FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
              NEXT FIELD sexo
           END IF

           IF x_fecha <> xx_fecha THEN
              ERROR "Existen diferencias de la fecha del RFC y F.Nacimiento"
              NEXT FIELD n_rfc
           END IF

# SEXO

        AFTER FIELD sexo
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               IF r_afi.sexo IS NULL OR r_afi.sexo = " " OR
                  r_afi.sexo = 0 THEN
                  ERROR "Digite correctamente el sexo"
                  NEXT FIELD sexo
               ELSE
                   NEXT FIELD fena
               END IF
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               IF r_afi.sexo IS NULL OR r_afi.sexo = " " OR
                  r_afi.sexo = 0 THEN
                  ERROR "Digite correctamente el sexo"
                  NEXT FIELD sexo
               ELSE
                   NEXT FIELD fena
               END IF
            END IF

            IF r_afi.sexo IS NULL OR r_afi.sexo = " " OR
               r_afi.sexo = 0 THEN
                CALL Despliega_sexos() RETURNING r_afi.sexo,
                                                 r_afi.desc_sexo
            ELSE
                SELECT sexo_desc 
                INTO   r_afi.desc_sexo 
                FROM   tab_sexo
                WHERE  sexo_cod = r_afi.sexo

                IF STATUS = NOTFOUND THEN
                    ERROR "Sexo Inexistente"
                    NEXT FIELD sexo
                END IF
            END IF

            DISPLAY BY NAME r_afi.sexo,r_afi.desc_sexo

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
               IF r_afi.sexo IS NULL OR r_afi.sexo = " " OR
                  r_afi.sexo = 0 THEN
                  ERROR "Digite correctamente el sexo"
                  NEXT FIELD sexo
               ELSE
                   NEXT FIELD n_unico
               END IF
            END IF

            NEXT FIELD n_rfc

# CURP 
        AFTER FIELD n_unico
            IF r_afi.tip_prob = 5 AND
              (r_afi.n_unico IS NULL OR
               r_afi.n_unico[1] = " ") THEN
                NEXT FIELD n_unico
            END IF

            IF r_afi.tipo_solicitud = 8 THEN
                IF (r_afi.n_unico IS NULL OR
                    r_afi.n_unico[1] = " ") THEN
                    ERROR "Campo CURP -NO PUEDE SER NULO-"
                    NEXT FIELD n_unico
                END IF
            END IF

            IF r_afi.n_unico IS NOT NULL OR r_afi.n_unico <> " " THEN 
               IF LENGTH(r_afi.n_unico) <> 18 OR
                  r_afi.n_unico[1] = " " OR
                  r_afi.n_unico IS NULL THEN
                  ERROR "Debe ingresar CURP completa"
                  NEXT FIELD n_unico
               END IF
            END IF 

            NEXT FIELD n_rfc

# NSS
        AFTER FIELD n_seguro
            CALL  digito_verif(r_afi.n_seguro[1,10],10) RETURNING digito

            IF digito = 32000 THEN 
                ERROR "N.S.S. solo contiene digitos" 
                NEXT FIELD n_seguro 
            END IF

            IF LENGTH(r_afi.n_seguro) = 11 AND 
               digito <> r_afi.n_seguro[11] THEN
                ERROR "Digito Verificador Invalido"
                NEXT FIELD n_seguro
            END IF

            IF r_afi.n_seguro[11] <> "1" AND 
               r_afi.n_seguro[11] <> "2" AND
               r_afi.n_seguro[11] <> "3" AND
               r_afi.n_seguro[11] <> "4" AND
               r_afi.n_seguro[11] <> "5" AND
               r_afi.n_seguro[11] <> "6" AND
               r_afi.n_seguro[11] <> "7" AND
               r_afi.n_seguro[11] <> "8" AND
               r_afi.n_seguro[11] <> "9" AND
               r_afi.n_seguro[11] <> "0" THEN
                ERROR "N.S.S. solo contiene digitos" 
                NEXT FIELD n_seguro 
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               NEXT FIELD n_unico
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
               NEXT FIELD n_rfc
            END IF

# RFC 
        AFTER FIELD n_rfc
            IF r_afi.n_rfc IS NULL THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD n_rfc
            ELSE
                LET v_1 = 0
                INITIALIZE val_1 TO NULL
                CALL verifica_rfc(r_afi.n_rfc[1,4])
                                     RETURNING v_1,val_1 ##ve--
                IF v_1 = 1 THEN
                   ERROR "R.F.C. ",val_1 CLIPPED
                   NEXT FIELD n_rfc
                END IF

                CALL valida_est_rfc(r_afi.n_rfc)
                RETURNING pasa_curp, desc_err

                IF pasa_curp = 1 THEN
                    ERROR "", desc_err
                    LET pasa_curp = 0
                    NEXT FIELD n_rfc
                END IF
            END IF

            IF LENGTH(r_afi.n_rfc) < 10 THEN
                ERROR "Debe ingresar R.F.C. completo"
                NEXT FIELD n_rfc
            END IF

            CALL arma_clave_rfc(r_afi.paterno,
                                r_afi.materno,
                                r_afi.nombres,
                                r_afi.fena) RETURNING cve_arma #rac

            IF cve_arma[1,4] != r_afi.n_rfc[1,4] THEN
                LET cve_arma = cve_arma[1,4]
                ERROR "El inicio del rfc debe ser: ", cve_arma
                WHILE TRUE
                    PROMPT "¿Dejar inicio de rfc encontrado [S/N]? " FOR enter
                    IF enter MATCHES "[Ss/Nn]" THEN
                        IF enter MATCHES "[Ss]" THEN
                            LET r_afi.n_rfc = cve_arma CLIPPED,r_afi.n_rfc[5,13]
                            DISPLAY BY NAME r_afi.n_rfc
                            EXIT WHILE
                        ELSE
                            LET vrfc_orig = r_afi.n_rfc[1,4]

                            SELECT @palabra_si
                              INTO vrfc_si
                              FROM afi_no_conviene
                             WHERE @palabra_no = vrfc_orig

                            IF SQLCA.SQLCODE = 0 THEN
                                LET r_afi.n_rfc = vrfc_si,r_afi.n_rfc[5,13]

                                DISPLAY BY NAME r_afi.n_rfc
                            END IF

                            EXIT WHILE
                        END IF
                    END IF
                END WHILE
            END IF

            INITIALIZE x_fecha, xx_fecha TO NULL

            LET x_fecha = r_afi.n_rfc[7,8],"/",
                          r_afi.n_rfc[9,10],"/",
                          "19",r_afi.n_rfc[5,6]

            LET xx_fecha = r_afi.fena

            IF x_fecha <> xx_fecha THEN
               WHILE TRUE
                  PROMPT "Existen inconsistencias en ",
                         "la Fecha nacimiento, es correcto ¿[S/N]? "
                  FOR enter
                  IF enter MATCHES "[Ss/Nn]" THEN
                     IF enter MATCHES "[Ss]" THEN
                        NEXT FIELD estadon
                        EXIT WHILE
                     ELSE
                        IF bnd_fena = 1 THEN
                           NEXT FIELD n_unico
                        ELSE
                           NEXT FIELD n_rfc
                        END IF
                        NEXT FIELD estadon
                     END IF
                  ELSE
                     ERROR "Solo debe presionar (S)i o (N)o"
                     SLEEP 3
                     ERROR ""
                  END IF
               END WHILE
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP")   OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") OR
               FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                LET v_1 = 0
                INITIALIZE val_1 TO NULL
                CALL verifica_rfc(r_afi.n_rfc[1,4]) RETURNING v_1,val_1 #ve
                IF v_1 = 1 THEN
                   ERROR "R.F.C. ",val_1 CLIPPED
                   NEXT FIELD n_rfc
                END IF

                IF r_afi.tipo_solicitud <> 8 THEN
                    NEXT FIELD fena
                END IF
            END IF

            IF r_afi.n_rfc IS NOT NULL THEN
               LET v_1 = 0
               INITIALIZE val_1 TO NULL
               CALL verifica_rfc(r_afi.n_rfc[1,4])
                                    RETURNING v_1,val_1 ##ve--
               IF v_1 = 1 THEN
                  ERROR "R.F.C. ",val_1 CLIPPED
                  NEXT FIELD n_rfc
               END IF
            END IF

            NEXT FIELD estadon

# ESTADON

        AFTER FIELD estadon
            IF r_afi.estadon IS NULL OR
               r_afi.estadon  =  0   THEN
                CALL Despliega_estados() RETURNING r_afi.estadon,
                                                   r_afi.desc_estadon
                IF r_afi.estadon = 0 THEN
                   NEXT FIELD estadon
                END IF
            ELSE
                SELECT estad_desc 
                INTO   r_afi.desc_estadon 
                FROM   tab_estado
                WHERE  estad_cod = r_afi.estadon

                IF STATUS = NOTFOUND THEN
                    ERROR "Entidad de Nacimiento Inexistente"
                    NEXT FIELD estadon
                END IF
            END IF

            DISPLAY BY NAME r_afi.estadon,r_afi.desc_estadon

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               NEXT FIELD n_rfc 
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                IF r_afi.tipo_solicitud <> 8 THEN
                    NEXT FIELD nacionalidad
                ELSE
                    NEXT FIELD fecha_emision
                END IF
            END IF

            IF r_afi.tipo_solicitud <> 8 THEN
                NEXT FIELD nacionalidad
            ELSE
                NEXT FIELD fecha_emision
            END IF

# NACIONALIDAD

       AFTER FIELD nacionalidad

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD estadon
            END IF

            IF r_afi.nacionalidad IS NULL OR
               r_afi.nacionalidad  = " "  THEN
                CALL Despliega_pais() RETURNING r_afi.nacionalidad,
                                                r_afi.desc_nacionalidad
            ELSE
                SELECT pais_desc
                INTO   r_afi.desc_nacionalidad
                FROM   tab_pais
                WHERE  pais_cod = r_afi.nacionalidad

                IF STATUS = NOTFOUND THEN
                    ERROR "Pais Inexistente"
                    NEXT FIELD nacionalidad
                END IF
            END IF

            DISPLAY BY NAME r_afi.nacionalidad, r_afi.desc_nacionalidad

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                NEXT FIELD profesion_cod
            END IF

            NEXT FIELD profesion_cod

# PROFESION U OCUPACION

       AFTER FIELD profesion_cod 
            IF r_afi.profesion_cod  IS NULL THEN

                CALL Despliega_profesion() RETURNING r_afi.profesion_cod,
                                                 r_afi.profesion_desc
            ELSE

                SELECT profesion_desc
                INTO   r_afi.profesion_desc
                FROM   tab_profesion
                WHERE  profesion_cod = r_afi.profesion_cod

                IF STATUS = NOTFOUND THEN
                    ERROR "Profesion NO existe"
                    SLEEP 1
                    NEXT FIELD profesion_cod
                END IF
            END IF

            DISPLAY BY NAME r_afi.profesion_cod, r_afi.profesion_desc

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN

                    NEXT FIELD actividad_cod
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                    NEXT FIELD nacionalidad
            END IF

# ACTIVIDAD O GIRO

        AFTER FIELD actividad_cod

            IF r_afi.actividad_cod  IS NULL THEN

                CALL Despliega_actividad() RETURNING r_afi.actividad_cod,
                                               r_afi.actividad_desc
            ELSE
                SELECT actividad_desc
                INTO   r_afi.actividad_desc
                FROM   tab_actividad
                WHERE  actividad_cod = r_afi.actividad_cod

                IF STATUS = NOTFOUND THEN
                    ERROR "Actividad NO existe"
                    SLEEP 1
                    NEXT FIELD actividad_cod
                END IF
            END IF

            DISPLAY BY NAME r_afi.actividad_cod, r_afi.actividad_desc

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                    NEXT FIELD ind_infonavit
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                    NEXT FIELD profesion_cod
            END IF

            NEXT FIELD ind_infonavit

# INDICADOR DE INFONAVIT

        AFTER FIELD ind_infonavit
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD actividad_cod
            END IF

            IF r_afi.ind_infonavit IS NULL THEN
                ERROR "Se requiere la clave de prestamo del INFONAVIT"
                NEXT FIELD ind_infonavit
            END IF

            IF r_afi.ind_infonavit NOT MATCHES "[0123]" THEN
                ERROR "Solo se acepta (s)i o (n)o"
                NEXT FIELD ind_infonavit
            END IF    

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                --NEXT FIELD fecha_emision
               IF r_afi.tipo_solicitud = 2 THEN     ---28-18
                   NEXT FIELD cve_rend_neto         ---28-18
               ELSE                                 ---28-18
                   NEXT FIELD fecha_emision
               END IF                               ---28-18
            END IF

{            IF r_afi.tipo_solicitud = 2 THEN
                NEXT FIELD fecha_emision
            ELSE
                NEXT FIELD tip_prob
            END IF
}
            IF r_afi.tipo_solicitud = 2 THEN     ---28-18
                NEXT FIELD cve_rend_neto         ---28-18
            ELSE                                 ---28-18
                NEXT FIELD fecha_emision
            END IF                               ---28-18

--->28-18
# CLAVE RENDIMIENTO NETO
     AFTER FIELD cve_rend_neto
           IF FGL_LASTKEY() = FGL_KEYVAL("UP")   OR
              FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               NEXT FIELD ind_infonavit
           END IF

           IF r_afi.tipo_solicitud  = 2 THEN
              IF r_afi.cve_rend_neto IS NULL OR 
                 r_afi.cve_rend_neto = " " THEN
                 ERROR "Clave Rendimiento Neto no puede ser nulo..."
                 SLEEP 2
                 ERROR ""
                 NEXT FIELD cve_rend_neto
              ELSE
                 SELECT 'X'
                 FROM tab_rango_edad
                 WHERE siefore = r_afi.cve_rend_neto
                 
                 IF SQLCA.SQLCODE = NOTFOUND THEN
                    ERROR "Clave rendimiento neto no existe..."
                    SLEEP 2
                    ERROR ""
                    NEXT FIELD cve_rend_neto
                 ELSE
                    NEXT FIELD fecha_emision
                 END IF
              END IF
           END IF
---<28-18

# FECHA VENCIMIENTO

        AFTER FIELD fecha_emision
            IF r_afi.fecha_emision IS NULL THEN
                 ERROR "Fecha Emision de dcto estrc comis NO puede ser NULO"
                 NEXT FIELD fecha_emision
            END IF

            IF r_afi.fecha_emision > r_afi.frecafor THEN
                ERROR "Fecha Emision Est. Com. es posterior a la Fecha de Recepcion"
                NEXT FIELD fecha_emision
            END IF

            IF r_afi.fecha_emision >= r_afi.fecha_elaboracion THEN
                ERROR "Fecha Emision Est. Com. es posterior a la Fecha de Firma"
                NEXT FIELD fecha_emision
            END IF

            LET fecha_comprueba = r_afi.fecha_emision + 30 units day

            IF r_afi.fecha_emision < fecha_comprueba THEN
                WHILE TRUE
                    PROMPT "Dcto estrc comis ya vencio, seguir la confirmacion [S/N] ? " 
                    ATTRIBUTES (REVERSE) FOR aux_pausa
                    IF aux_pausa  MATCHES "[SsNn]" THEN
                        IF aux_pausa MATCHES "[Nn]" THEN
                            RETURN
                        ELSE
                            EXIT WHILE
                        END IF
                    ELSE
                        ERROR  "Solo debe presionar (S)i o (N)o" 
                    END IF
                END WHILE
            END IF 

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                IF r_afi.tipo_solicitud <> 8 THEN
                    NEXT FIELD ind_infonavit
                ELSE
                    NEXT FIELD estadon
                END IF
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                NEXT FIELD frecafor
            END IF

            NEXT FIELD frecafor

# FRECAFOR

        BEFORE FIELD frecafor
            IF r_afi.tipo_solicitud = 8 THEN
                NEXT FIELD tipo_solicitud
            END IF

            DISPLAY BY NAME r_afi.frecafor

        AFTER FIELD frecafor
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                IF r_afi.tipo_solicitud = 2 THEN
                    NEXT FIELD cod_afore_ced
                ELSE
                    NEXT FIELD tip_prob
                END IF
            END IF

            IF r_afi.frecafor IS NULL THEN
                 ERROR "Fecha de Captura NO puede ser NULO"
                 NEXT FIELD frecafor
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                IF r_afi.tipo_solicitud = 2 THEN
                    NEXT FIELD cod_afore_ced
                ELSE
                    IF r_afi.tipo_solicitud = 8 THEN
                        NEXT FIELD tipo_solicitud
                    ELSE
                        NEXT FIELD tip_prob
                    END IF
                END IF
            END IF

            IF r_afi.tipo_solicitud = 1 THEN
                NEXT FIELD tip_prob
            ELSE
                NEXT FIELD cod_afore_ced
            END IF

# AFORE CEDENTE

        AFTER FIELD cod_afore_ced
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD fecha_cap
            END IF

            IF r_afi.cod_afore_ced IS NULL THEN
                CALL Despliega_afores()
                RETURNING r_afi.cod_afore_ced, r_afi.desc_afore

                IF r_afi.cod_afore_ced = 0 THEN
                    LET r_afi.cod_afore_ced = ''
                    DISPLAY BY NAME r_afi.cod_afore_ced
                    NEXT FIELD cod_afore_ced
                END IF

                SELECT @afore_desc
                INTO   r_afi.desc_afore
                FROM   safre_af:tab_afore
                WHERE  @afore_cod = r_afi.cod_afore_ced
                AND    @marca = 1

                IF SQLCA.SQLCODE <> 0 THEN
                    SELECT @afore_desc, @afore_fusion
                    INTO   r_afi.desc_afore, vafore_f
                    FROM   safre_af:tab_afore
                    WHERE  @afore_cod = r_afi.cod_afore_ced
                    AND    @marca = 0

                    IF SQLCA.SQLCODE = 0 THEN
                        IF vafore_f IS NOT NULL THEN
                            SELECT @afore_desc
                            INTO   r_afi.desc_afore
                            FROM   tab_afore
                            WHERE  @afore_cod = vafore_f

                            LET r_afi.cod_afore_ced = vafore_f
                        END IF

                        DISPLAY BY NAME r_afi.cod_afore_ced,
                                        r_afi.desc_afore
                    END IF
                ELSE
                    ERROR " Codigo Afore no existe o no permitido"
                    SLEEP 3
                    LET r_afi.cod_afore_ced = ''
                    DISPLAY BY NAME r_afi.desc_afore
                    NEXT FIELD cod_afore_ced
                END IF

                IF r_afi.desc_afore = 0 THEN
                    ERROR " Cod. Afore no existe "
                    NEXT FIELD cod_afore_ced
                END IF
            ELSE
                SELECT @afore_desc, @afore_fusion
                INTO   r_afi.desc_afore, vafore_f
                FROM   safre_af:tab_afore
                WHERE  @afore_cod = r_afi.cod_afore_ced
                AND    @marca = 0

                IF SQLCA.SQLCODE = 0 THEN
                    IF vafore_f IS NOT NULL THEN
                        SELECT @afore_desc
                        INTO   r_afi.desc_afore
                        FROM   tab_afore
                        WHERE  @afore_cod = vafore_f

                        LET r_afi.cod_afore_ced = vafore_f
                    END IF

                    DISPLAY BY NAME r_afi.cod_afore_ced, r_afi.desc_afore
                ELSE
                    ERROR " Clave de Afore no existe "

                    LET r_afi.cod_afore_ced = ''
                    DISPLAY BY NAME r_afi.desc_afore
                    NEXT FIELD cod_afore_ced
                END IF
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
               -- issa NEXT FIELD frecafor
               NEXT FIELD tip_prob
            END IF

        NEXT FIELD tip_prob 

# TIP_PROB

        BEFORE FIELD tip_prob
            DISPLAY BY NAME r_afi.tip_prob

        AFTER FIELD tip_prob
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                -- issa NEXT FIELD edo_civil
                NEXT FIELD cod_afore_ced
            END IF

            IF r_afi.tip_prob IS NULL OR
               r_afi.tip_prob  = " "  OR
               r_afi.tip_prob  = 0    THEN
                CALL Despliega_documento_probatorio() 
                     RETURNING r_afi.tip_prob,r_afi.docprob_desc

                IF r_afi.tip_prob = 0 THEN 
                    NEXT FIELD tip_prob 
                END IF
            ELSE
                SELECT docprob_desc 
                INTO   r_afi.docprob_desc 
                FROM   tab_doc_prob
                WHERE  docprob_cod = r_afi.tip_prob

                IF STATUS = NOTFOUND THEN
                    ERROR "Tipo de Documento Probatorio Inexistente"
                    NEXT FIELD tip_prob
                END IF
            END IF

            DISPLAY BY NAME r_afi.tip_prob,r_afi.docprob_desc

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                IF r_afi.tip_prob = 5 THEN
                    NEXT FIELD n_unico
                ELSE
                    NEXT FIELD fol_prob
                END IF
            END IF 

            IF r_afi.tip_prob = 5 THEN
                NEXT FIELD n_unico
            ELSE
                NEXT FIELD fol_prob
            END IF

# FOL_PROB

        AFTER FIELD fol_prob 
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR  
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD tip_prob 
            END IF

            IF r_afi.fol_prob IS NULL THEN
                ERROR "Se requiere el folio de documento probatorio"
                NEXT FIELD fol_prob
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
               NEXT FIELD doc_prob
            END IF

# DOCUMENTO PROBATORIO

        BEFORE FIELD doc_prob
            IF r_afi.tip_prob = 5 THEN
                LET r_afi.doc_prob = r_afi.n_unico[1,16]
                CALL arma_clave(r_afi.paterno,
                                r_afi.materno,
                                r_afi.nombres,
                                r_afi.fena,
                                r_afi.estadon,
                                r_afi.sexo) RETURNING doc_prob_arma #ac
            END IF

            DISPLAY BY NAME r_afi.doc_prob 

        AFTER FIELD doc_prob
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD fol_prob
            END IF

            CASE r_afi.tip_prob
                WHEN 1
                    IF LENGTH(r_afi.doc_prob) <> 16 THEN
                        ERROR "Documento probatorio erroneo"
                        SLEEP 2
                        LET r_afi.doc_prob = ''
                        DISPLAY BY NAME r_afi.doc_prob
                        NEXT FIELD doc_prob
                    END IF

                    CALL valida_anyo_nac() RETURNING sino

                    IF sino THEN
                        ERROR "Año de registro erroneo"
                        SLEEP 2
                        LET r_afi.doc_prob = ''
                        DISPLAY BY NAME r_afi.doc_prob
                        NEXT FIELD doc_prob
                    END IF
                WHEN 2
                    LET r_afi.doc_prob = r_afi.doc_prob[2,16]   

                    IF LENGTH(r_afi.doc_prob) <> 15 THEN
                        ERROR "Documento probatorio erroneo"
                        SLEEP 2
                        LET r_afi.doc_prob = ''
                        DISPLAY BY NAME r_afi.doc_prob
                        NEXT FIELD doc_prob
                    END IF
                    LET r_afi.doc_prob = dcto_2, r_afi.doc_prob

                    CALL valida_anyo_nac() RETURNING sino

                    IF sino THEN
                        ERROR "Año de registro erroneo"
                        SLEEP 2
                        LET r_afi.doc_prob = ''
                        DISPLAY BY NAME r_afi.doc_prob
                        NEXT FIELD doc_prob
                    END IF
                WHEN 3
                    LET r_afi.doc_prob = r_afi.doc_prob[10,16]

                    IF LENGTH(r_afi.doc_prob) <> 7 THEN
                        ERROR "Documento probatorio erroneo"
                        SLEEP 2
                        LET r_afi.doc_prob = ''
                        DISPLAY BY NAME r_afi.doc_prob
                        NEXT FIELD doc_prob
                    END IF
                    LET r_afi.doc_prob = dcto_3, r_afi.doc_prob
                WHEN 4
                    LET r_afi.doc_prob = r_afi.doc_prob[8,16]

                    IF LENGTH(r_afi.doc_prob) <> 9 THEN
                        ERROR "Documento probatorio erroneo"
                        SLEEP 2
                        LET r_afi.doc_prob = ''
                        DISPLAY BY NAME r_afi.doc_prob
                        NEXT FIELD doc_prob
                    END IF
                    LET r_afi.doc_prob = dcto_4, r_afi.doc_prob
                WHEN 5
                    IF r_afi.doc_prob NOT MATCHES doc_prob_arma THEN
                       WHILE TRUE
                           PROMPT "Existen inconsistencias entre doc prob/raiz curp, ",
                                  "es correcto ¿[S/N]? "
                           FOR enter
                           IF enter MATCHES "[Ss/Nn]" THEN
                               IF enter MATCHES "[Ss]" THEN
                                  WHILE TRUE
                                      PROMPT "Documento probatorio tiene error de origen [S/N]? "
                                      ATTRIBUTES (REVERSE) FOR c_doc
                                      IF c_doc  MATCHES "[SsNn]" THEN
                                          IF c_doc MATCHES "[Nn]" THEN
                                              NEXT FIELD cod_error_orig
                                          ELSE
                                              LET cod_err = cod_err + 256
                                              NEXT FIELD cod_error_orig
                                          END IF
                                          EXIT WHILE
                                      ELSE
                                          ERROR "Solo debe presionar (S)i o (N)o"
                                          SLEEP 2
                                          ERROR ""
                                      END IF
                                  END WHILE
#                                   NEXT FIELD cod_error_orig
                                   EXIT WHILE
                               ELSE
                                   NEXT FIELD n_unico
                               END IF
                           ELSE
                               ERROR "Solo debe presionar (S)i o (N)o"
                               SLEEP 3
                               ERROR ""
                           END IF
                       END WHILE 
                    END IF

                    IF LENGTH(r_afi.doc_prob) <> 16 THEN
                        ERROR "Documento probatorio erroneo"
                        SLEEP 2
                        LET r_afi.doc_prob = ''
                        DISPLAY BY NAME r_afi.doc_prob
                        NEXT FIELD doc_prob

                    END IF
                WHEN 7
                    LET r_afi.doc_prob = r_afi.doc_prob[8,16]

                    IF LENGTH(r_afi.doc_prob) <> 9 THEN
                        ERROR "Documento probatorio erroneo"
                        SLEEP 2
                        LET r_afi.doc_prob = ''
                        DISPLAY BY NAME r_afi.doc_prob
                        NEXT FIELD doc_prob
                    END IF
                    LET r_afi.doc_prob = dcto_7, r_afi.doc_prob
            END CASE

            DISPLAY BY NAME r_afi.doc_prob

            IF NOT Verifica_documento_probatorio(r_afi.tip_prob,
                                                 r_afi.doc_prob) THEN
                NEXT FIELD doc_prob
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
               NEXT FIELD cod_error_orig
            END IF

            NEXT FIELD cod_error_orig

# ERROR ORIGEN

        AFTER FIELD cod_error_orig 
            LET cod_err = r_afi.cod_error_orig  USING "&&&&"

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD doc_prob  
            END IF

           IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
               NEXT FIELD tipo_solicitud
           END IF 

           NEXT FIELD paterno

        ON KEY ( F1 )
           IF r_afi.tipo_solicitud = 2 THEN
               CALL carga_imagenes()
               CALL digital(r_afi.n_seguro, r_afi.tipo_solicitud,
                            r_afi.n_folio , r_afi.nom_promotor)
            ELSE
               ERROR "Para este tipo de solicitud no se califican imagenes"
               SLEEP 3
               ERROR ""
            END IF

        ON KEY ( F2 )
            IF r_afi.tipo_solicitud = 2 THEN
               CALL cons_digital(r_afi.n_seguro, r_afi.tipo_solicitud,
                                 r_afi.n_folio )
            ELSE
               ERROR "Para este tipo de solicitud no hay Consulta"
               SLEEP 3
               ERROR ""
            END IF

        ON KEY ( INTERRUPT )
            IF vtipo_oper = "C" THEN
               LET su_estatus=10
               LET des_estatus="CAPTURADA"
            ELSE
               LET su_estatus=15
               LET des_estatus="CONFIRMADA"
            END IF

            EXIT INPUT
            DISPLAY "                                     " AT 5,56
            ATTRIBUTE(REVERSE)
            EXIT INPUT

        ON KEY ( ACCEPT )
            IF r_afi.cod_promotor IS NULL THEN
                ERROR "Campo NO puede ser NULO"  
                NEXT FIELD cod_promotor
            END IF

            SELECT "X"
            FROM   pro_mae_promotor A
            WHERE  A.cod_promotor = r_afi.cod_promotor

            IF STATUS = NOTFOUND THEN
                ERROR "Promotor Inexistente"
                NEXT FIELD cod_promotor
            END IF

            IF r_afi.agenc_cod IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD agenc_cod
            END IF

            IF r_afi.n_folio IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD n_folio
            END IF

            IF r_afi.tipo_solicitud IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD tipo_solicitud
            END IF

---Nueva validacion para codigo de afore mmc 8-nov-2002

            IF r_afi.tipo_solicitud = 2 THEN
                IF r_afi.cod_afore_ced IS NULL OR
                   r_afi.cod_afore_ced = 0 THEN
                    ERROR "Campo Afore Cedente no es valido"
                    NEXT FIELD cod_afore_ced
                ELSE
                    SELECT "X"
                    FROM   tab_afore
                    WHERE  afore_cod = r_afi.cod_afore_ced

                    IF SQLCA.SQLCODE <> 0 THEN
                        ERROR "Campo Afore Cedente no es valido"
                        NEXT FIELD cod_afore_ced
                    END IF
                END IF
            END IF

            IF r_afi.frecafor IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD frecafor
            END IF

            IF r_afi.fecha_elaboracion IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD fecha_elaboracion
            END IF

            IF r_afi.fecha_emision IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD fecha_emision
            END IF

            IF r_afi.paterno IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD paterno
            END IF

            IF r_afi.nombres IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD nombres
            END IF

            IF r_afi.n_seguro IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD n_seguro
            END IF

            IF r_afi.n_rfc IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD n_rfc
            END IF

            IF r_afi.sexo IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD sexo
            END IF

            IF r_afi.fena IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD fena
            END IF

            IF r_afi.estadon IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD estadon
            END IF

            SELECT "X"
            FROM   afi_domicilio
            WHERE  n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            GROUP BY 1

            IF STATUS = NOTFOUND THEN
                ERROR "No puede dar de alta mientras no ingrese domicilio de la solicitud"
                NEXT FIELD tipo_solicitud
            END IF

            IF r_afi.tipo_solicitud <> 8 THEN
                IF r_afi.tip_prob IS NULL THEN
                    ERROR "Campo NO puede ser NULO" 
                    NEXT FIELD tip_prob
                END IF

                IF r_afi.fol_prob IS NULL AND
                   r_afi.tip_prob <> 5 THEN
                    ERROR "Campo NO puede ser NULO"
                    NEXT FIELD fol_prob
                END IF

                IF r_afi.doc_prob IS NULL AND
                   r_afi.tip_prob <> 5 THEN
                    ERROR "Campo NO puede ser NULO"
                    NEXT FIELD doc_prob
                END IF

                IF r_afi.ind_infonavit IS NULL THEN
                    ERROR "Campo NO puede ser NULO" 
                    NEXT FIELD ind_infonavit
                END IF

                IF r_afi.n_seguro[11] <> "1" AND
                   r_afi.n_seguro[11] <> "2" AND
                   r_afi.n_seguro[11] <> "3" AND
                   r_afi.n_seguro[11] <> "4" AND
                   r_afi.n_seguro[11] <> "5" AND
                   r_afi.n_seguro[11] <> "6" AND
                   r_afi.n_seguro[11] <> "7" AND
                   r_afi.n_seguro[11] <> "8" AND
                   r_afi.n_seguro[11] <> "9" AND
                   r_afi.n_seguro[11] <> "0" THEN
                    ERROR "N.S.S. solo contiene digitos"
                    NEXT FIELD n_seguro
                END IF
            END IF

        WHILE TRUE
            CALL Desea_modificar(vtipo_oper)
            IF aux_pausa MATCHES "[SsNn]" THEN
                EXIT WHILE
            END IF
        END WHILE

        IF aux_pausa MATCHES "[Nn]" THEN
            CASE su_estatus
                WHEN 10 LET des_estatus = "CAPTURADA"
                WHEN 12 LET des_estatus = "SIN DIGITALIZAR"
                WHEN 15 LET des_estatus = "CONFIRMADA"
                WHEN 20 LET des_estatus = "VALIDADA"
            END CASE
            EXIT INPUT
        ELSE
            IF vtipo_oper = "C" THEN
               IF r_afi.tipo_solicitud = 2 THEN
                   CALL valida_digital(su_estatus) RETURNING su_st_nvo
               ELSE
                   LET su_st_nvo = 15
               END IF

               CALL actualiza()

               UPDATE afi_solicitud 
               SET    afi_solicitud.status_interno  = su_st_nvo
               WHERE  afi_solicitud.n_folio         = r_afi.n_folio
               AND    afi_solicitud.tipo_solicitud  = r_afi.tipo_solicitud
               AND    afi_solicitud.n_seguro        = r_afi.n_seguro
               AND    afi_solicitud.status_interno  in(10,12)

               LET st_int = su_st_nvo

               IF st_int = 15 THEN
                   LET des_estatus = "CONFIRMADA"
               ELSE
                   LET des_estatus = "SIN DIGITALIZAR"
               END IF

               LET operacion   = 'CONFIRMACION'

               CALL inserta_logico(r_afi.n_folio,r_afi.tipo_solicitud,
                                   r_afi.n_seguro,st_int,g_usuario,operacion)
            ELSE
               CALL actualiza()

               LET st_int = su_estatus
               LET operacion = 'MODIFICACION A SOLIC CONFIRMADA'
               CALL inserta_logico(r_afi.n_folio,r_afi.tipo_solicitud,
                                   r_afi.n_seguro,st_int,g_usuario,operacion)
            END IF
        END IF

--->28-18
        UPDATE afi_folio_saftv
        SET    cve_rend_neto  = r_afi.cve_rend_neto
        WHERE  n_folio         = r_afi.n_folio
        AND    tipo_solicitud  = r_afi.tipo_solicitud
        AND    nss             = r_afi.n_seguro
---<28-18

        CASE vtipo_oper
             WHEN "C"
                  ERROR "REGISTRO CONFIRMADO" SLEEP 2 ERROR ""

                  SELECT afi_solicitud.status_interno
                  INTO   su_estatus
                  FROM   afi_solicitud 
                  WHERE  afi_solicitud.n_seguro       = r_afi.n_seguro
                  AND    afi_solicitud.n_folio        = r_afi.n_folio
                  AND    afi_solicitud.tipo_solicitud = r_afi.tipo_solicitud

                   IF su_estatus = 15 THEN
                       LET des_estatus = "CONFIRMADA"
                   ELSE
                       LET des_estatus = "SIN DIGITALIZAR"
                   END IF
             WHEN "M"
                  IF FIELD_TOUCHED(r_afi.*) THEN
                     ERROR "REGISTRO NO MODIFICADO" SLEEP 2 ERROR ""
                  ELSE
                     ERROR "REGISTRO MODIFICADO" SLEEP 2 ERROR ""
                  END IF
        END CASE

        EXIT INPUT

    ON KEY ( CONTROL-E,CONTROL-T,CONTROL-B,CONTROL-V,CONTROL-W,CONTROL-Y )
            SELECT a.status_interno, b.estado_desc
            INTO   xx_status, vstatus_desc
            FROM   afi_solicitud a,
            OUTER  tab_status_afi b
            WHERE  a.n_seguro       = r_afi.n_seguro
            AND    a.tipo_solicitud = r_afi.tipo_solicitud
            AND    a.n_folio        = r_afi.n_folio
            AND    a.status_interno = b.estado_cod

            IF xx_status > 20 THEN
                IF xx_status <> 40 THEN
                    ERROR "NO PUEDE MODIFICAR,", vstatus_desc
                ELSE
                    LET KEY = FGL_LASTKEY()

                    CASE KEY
                      WHEN 2
                          LET comma = "fglgo AFIM004 ",
                          r_afi.n_seguro," ",
                          ACCION CLIPPED," ",
                          r_afi.tipo_solicitud,
                          r_afi.n_folio
                      WHEN 5
                          LET comma = "fglgo AFIM002.4gi ",
                          r_afi.n_folio," ",
                          ACCION CLIPPED," ",vcont," ",
                          r_afi.tipo_solicitud
                      WHEN 16                                   
                          LET comma = "fglgo AFIM025 ",
                          r_afi.n_folio, " ",
                          r_afi.tipo_solicitud CLIPPED
                      WHEN 20
                          LET comma = "fglgo AFIM003 ",
                          r_afi.n_seguro," ",
                          ACCION CLIPPED," ",
                          r_afi.tipo_solicitud,
                          r_afi.n_folio
                      WHEN 22
                          LET comma = "fglgo AFIM005 ",
                          r_afi.n_seguro," ",
                          ACCION CLIPPED," ",
                          r_afi.tipo_solicitud,
                          r_afi.n_folio
                      WHEN 23
                          CALL observaciones(r_afi.n_seguro, r_afi.n_folio, r_afi.tipo_solicitud)
                      WHEN 25
                          CALL n_identif(r_afi.n_seguro, r_afi.n_folio, r_afi.tipo_solicitud)
                     END CASE
                RUN comma
                END IF
            END IF

        END INPUT

    CLOSE WINDOW ventana_4

END FUNCTION

FUNCTION Desea_reenviar()
#dr----------------------

    PROMPT "Desea Reenviar a Certificar la Solicitud [S/N]? " FOR aux_pausa

END FUNCTION

FUNCTION Desea_modificar(tipo_acc)
#dm-------------------------------

    DEFINE tipo_acc CHAR(1)

    IF tipo_acc = 'C' THEN
        PROMPT "Desea Confirmar la Informacion [S/N]? " FOR aux_pausa
    ELSE
        PROMPT "Desea Modificar la Informacion [S/N]? " FOR aux_pausa
    END IF

END FUNCTION

FUNCTION cons_digital(n_seguro, tipo_solicitud, n_folio)

    DEFINE n_seguro        CHAR(11)
    DEFINE tipo_solicitud  SMALLINT
    DEFINE n_folio         DECIMAL(10,0)
    DEFINE max_fec         DATE
    DEFINE max_hora        CHAR(08)
    DEFINE dig             RECORD LIKE afi_ctr_digital.*
    DEFINE de_s            RECORD
              n_seguro        CHAR(11),
              n_folio         DECIMAL(10,0),
              tipo_solicitud  SMALLINT,
              estado_doc1     SMALLINT,
              condic_doc1     SMALLINT,
              desc1           CHAR(30),
              estado_doc2     SMALLINT,
              condic_doc2     SMALLINT,
              desc2           CHAR(30),
              estado_doc3     SMALLINT,
              condic_doc3     SMALLINT,
              desc3           CHAR(30),
              estado_doc4     SMALLINT,
              condic_doc4     SMALLINT,
              desc4           CHAR(30),
              estado_doc5     SMALLINT,
              condic_doc5     SMALLINT,
              desc5           CHAR(30)
           END RECORD,

           descri             CHAR(20),
           ban                SMALLINT

    INITIALIZE max_fec, max_hora, dig.*, de_s.* TO NULL
    LET ban = 0

    SELECT MAX(b.fecha_actualiza) INTO max_fec FROM afi_ctr_digital b
    WHERE  b.n_seguro        = n_seguro
      AND  b.n_folio         = n_folio 
      AND  b.tipo_solicitud  = tipo_solicitud 

    IF max_fec = " "  OR
       max_fec IS NULL THEN
       ERROR "No existe informacion para este NSS"
       SLEEP 3
       ERROR ""
    END IF

    SELECT MAX(b.hora_actualiza) INTO max_hora FROM afi_ctr_digital b
    WHERE  b.n_seguro        = n_seguro
      AND  b.n_folio         = n_folio 
      AND  b.tipo_solicitud  = tipo_solicitud 
      AND  b.fecha_actualiza = max_fec

    DECLARE cur_cons_dig CURSOR FOR
       SELECT * FROM afi_ctr_digital b
       WHERE  b.n_seguro        = n_seguro
         AND  b.n_folio         = n_folio 
         AND  b.tipo_solicitud  = tipo_solicitud 
         AND  b.fecha_actualiza = max_fec
         AND  b.hora_actualiza  = max_hora

    FOREACH cur_cons_dig INTO dig.*

         IF ban = 0 THEN
            LET de_s.n_seguro          = dig.n_seguro
            LET de_s.n_folio           = dig.n_folio 
            LET de_s.tipo_solicitud    = dig.tipo_solicitud
            LET ban = 1
         END IF 

         CASE dig.condic_doc
                    WHEN 0   LET descri = "PROCEDENTE       "
                    WHEN 1   LET descri = "NO DIGITALIZADO  "
                    WHEN 2   LET descri = "ILEGIBLE         "
                    WHEN 3   LET descri = "NO COINCIDE      "
         END CASE

         CASE  dig.codigo_doc
             WHEN 1
                  LET de_s.estado_doc1 = dig.estado_doc
                  LET de_s.condic_doc1 = dig.condic_doc
                  LET de_s.desc1       = descri 
             WHEN 9
                  LET de_s.estado_doc2 = dig.estado_doc
                  LET de_s.condic_doc2 = dig.condic_doc
                  LET de_s.desc2       = descri 
             WHEN 2
                  LET de_s.estado_doc3 = dig.estado_doc
                  LET de_s.condic_doc3 = dig.condic_doc
                  LET de_s.desc3       = descri 
             WHEN 7
                  LET de_s.estado_doc4 = dig.estado_doc
                  LET de_s.condic_doc4 = dig.condic_doc
                  LET de_s.desc4       = descri 
             WHEN 0
                  LET de_s.estado_doc5 = dig.estado_doc
                  LET de_s.condic_doc5 = dig.condic_doc
                  LET de_s.desc5       = descri 
         END CASE

         INITIALIZE descri TO NULL
    END FOREACH

    OPEN WINDOW cons_digital_1 AT 5,3 WITH FORM "AFIM01811" ATTRIBUTE(BORDER)
         DISPLAY "                                                      ",
                 "                         " AT 1,1 ATTRIBUTE(REVERSE)
         DISPLAY "                          CONSULTA DIGITALIZACION    ",
                 "                         " AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY BY NAME de_s.*

         PROMPT "PRESIONE < ENTER > PARA CONTINUAR" FOR enter
    CLOSE WINDOW cons_digital_1

END FUNCTION

FUNCTION Despliega()
#d------------------

    DEFINE aux_pausa  CHAR(1)
    DEFINE paterno    CHAR(50)
    DEFINE materno    CHAR(50)
    DEFINE nombres    CHAR(50)
    DEFINE n_busqueda CHAR(100)
    DEFINE cla_sel    CHAR(250)
    DEFINE txt        CHAR(300)
    DEFINE i          SMALLINT
    DEFINE HACER      SMALLINT
    DEFINE pat,
           mat,
           nom        CHAR(50)

    DEFINE l_reg ARRAY[25000] OF RECORD
        n_seguro       CHAR(11),
        n_unico        CHAR(18),
        tipo_solicitud SMALLINT,
        n_folio         DECIMAL(10,0) ,
        nombre         CHAR(50)
    END RECORD

    OPEN WINDOW v1 AT  4,4 WITH FORM "AFIM0182" ATTRIBUTE(BORDER)

    DISPLAY "          USTED PUEDE UTILIZAR EL * (ASTERISCO) COMO COMODIN                " AT 1,1 ATTRIBUTE(REVERSE)

    LET HACER    = TRUE
    LET INT_FLAG = FALSE

    CONSTRUCT BY NAME cla_sel ON paterno,materno,nombres
        ON KEY ( INTERRUPT )
            LET INT_FLAG=FALSE
            LET HACER = FALSE
            EXIT CONSTRUCT

        ON KEY ( ESC )
            LET INT_FLAG=FALSE
            EXIT CONSTRUCT
    END CONSTRUCT

    LET txt = "SELECT n_seguro,n_unico,tipo_solicitud,n_folio,paterno,materno,",
              "nombres FROM afi_solicitud WHERE ",cla_sel CLIPPED,
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
                              pat,mat,nom

        LET l_reg[i].nombre = pat CLIPPED," ",
                              mat CLIPPED," ",
                              nom CLIPPED

        LET i = i + 1

        IF i >= 2500  THEN
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
            LET r_afi.n_seguro       = l_reg[i].n_seguro
            LET r_afi.n_unico        = l_reg[i].n_unico
            LET r_afi.n_folio        = l_reg[i].n_folio
            LET r_afi.tipo_solicitud = l_reg[i].tipo_solicitud
            EXIT DISPLAY

        ON KEY ( INTERRUPT )
            LET r_afi.n_seguro = NULL
            LET r_afi.n_unico = NULL
            LET r_afi.n_folio = NULL
            LET r_afi.tipo_solicitud = NULL
            EXIT DISPLAY
        END DISPLAY

    END IF

    CLOSE WINDOW v1

END FUNCTION

FUNCTION rescata_status(valor)
#rs---------------------------

    DEFINE valor    SMALLINT
    DEFINE l_estado CHAR(21)
    DEFINE x_fecha  DATE

    SELECT tsa.estado_desc
    INTO   l_estado
    FROM   tab_status_afi tsa
    WHERE  tsa.estado_cod = valor

    DISPLAY l_estado CLIPPED,"                            " AT 5,56 ATTRIBUTE(REVERSE)

    SELECT fentcons 
    INTO   x_fecha 
    FROM   afi_solicitud
    WHERE  n_seguro = r_afi.n_seguro
    AND    n_folio  = r_afi.n_folio
    AND    tipo_solicitud = r_afi.tipo_solicitud


    DISPLAY "F.Certif. ",x_fecha USING "dd-mm-yyyy" AT 13,57 ATTRIBUTE(REVERSE)

END FUNCTION

FUNCTION motivo_rechazo()
#mr----------------------

    DEFINE a ARRAY[100] OF RECORD
        codigo      SMALLINT,
        descripcion CHAR(80),
        cve_afore   SMALLINT,
        desc_afore  CHAR(14),
        fech_rech   DATE
    END RECORD

    DEFINE i         SMALLINT
    DEFINE j         SMALLINT
    DEFINE fech_rech DATE

    OPEN WINDOW v34  at 6,3 with form "AFIM0183" attribute(border)
    DISPLAY " [ Ctrl_c ] Salir " AT 1,1
    DISPLAY "                                MOTIVOS DE RECHAZO                                 " AT 3,1 ATTRIBUTE(REVERSE)

    DECLARE cursor_o CURSOR FOR 
        SELECT rdeta_cod, 
               observacion, 
               codigo_afore,
               "",  
               f_rechazo 
        FROM   afi_rechaza_cert
        WHERE  n_seguro = r_afi.n_seguro
        ORDER BY 3

        LET i = 1

        FOREACH cursor_o INTO a[i].* 
            SELECT afore_desc
            INTO   a[i].desc_afore
            FROM   tab_afore
            WHERE  afore_cod = a[i].cve_afore 

            LET i = i + 1 

        END FOREACH

        CALL SET_COUNT(i-1)

        DISPLAY ARRAY a TO scr_1.*

        ON KEY ( INTERRUPT )
            EXIT DISPLAY
            END DISPLAY

    CLOSE WINDOW v34

END FUNCTION

FUNCTION Despliega_esquema()
#de-------------------------

    DEFINE c ARRAY[100] OF RECORD
        codigo      SMALLINT,
        descripcion CHAR(50)
    END RECORD

    DEFINE i SMALLINT

    DECLARE cursor_1a1 CURSOR FOR 
        SELECT cod_esq_comision,
               desc_esq_comision 
        FROM   com_esq_comis
        ORDER BY 1

        LET i = 1

        FOREACH cursor_1a1 INTO c[i].*
            LET i = i + 1
        END FOREACH

        CALL SET_COUNT(i-1)

    OPEN WINDOW ventanilla_b AT 9,10 WITH FORM "AFIM0186" ATTRIBUTE(BORDER)

    DISPLAY "                   ESQUEMAS DE COMISION                 " AT 3,1 ATTRIBUTE ( REVERSE )

    DISPLAY ARRAY c TO scr_1.*

    ON KEY ( INTERRUPT )
        LET i = ARR_CURR()

    ON KEY ( CONTROL-M )
        LET i = ARR_CURR()
        LET g_comision.cod_esq_comision  = c[i].codigo
        LET g_comision.desc_esq_comision = c[i].descripcion

    EXIT DISPLAY

    END DISPLAY

    CLOSE WINDOW ventanilla_b

END FUNCTION

FUNCTION despliega_ing()
#di---------------------

    DEFINE rec_1 ARRAY[200] OF RECORD
           n_seguro       LIKE afi_solicitud.n_seguro,
           n_folio        LIKE afi_solicitud.n_folio,
           tipo_solicitud LIKE afi_solicitud.tipo_solicitud,
           status_interno CHAR(20),
           fentcons       LIKE afi_solicitud.fentcons
    END RECORD

    DEFINE
        idx        SMALLINT,
        seguro_cnt SMALLINT,
        array_sz   SMALLINT,
        over_size  SMALLINT

    LET array_sz = 200

    OPEN WINDOW AFIM0017 AT 7,4 WITH FORM "AFIM0187" ATTRIBUTE(BORDER)
    DISPLAY "                              [CTRL-C] Salir                                   " AT 1,1 ATTRIBUTE(REVERSE)

    LET over_size = FALSE
    LET seguro_cnt = 1

    DECLARE cur_err CURSOR FOR
    SELECT a.n_seguro,
           a.n_folio,
           a.tipo_solicitud,
           a.status_interno,
           a.fentcons
    FROM   afi_solicitud a
    WHERE  a.n_seguro = r_afi.n_seguro 

    FOREACH cur_err INTO rec_1[seguro_cnt].n_seguro,
                         rec_1[seguro_cnt].n_folio,
                         rec_1[seguro_cnt].tipo_solicitud,
                         status_interno,   
                         rec_1[seguro_cnt].fentcons

    SELECT tsa.estado_desc
    INTO   rec_1[seguro_cnt].status_interno
    FROM   tab_status_afi tsa
    WHERE  tsa.estado_cod = status_interno

    LET seguro_cnt = seguro_cnt + 1

    IF seguro_cnt > array_sz THEN
        LET over_size = TRUE
        EXIT FOREACH
    END IF
    END FOREACH

    IF (seguro_cnt = 1) THEN
        LET idx = 1
        LET rec_1[idx].n_seguro = NULL
    ELSE
        IF over_size THEN
        MESSAGE "Manuf array full: can only display ",array_sz USING "<<<<<<"
    END IF

    CALL SET_COUNT(seguro_cnt-1)

    LET int_flag = FALSE
    DISPLAY ARRAY rec_1 TO rec.*

    LET idx = ARR_CURR()
        IF int_flag THEN
            LET int_flag = FALSE
            LET rec_1[idx].n_seguro = NULL
        END IF
    END IF

END FUNCTION

FUNCTION elige_solicitud(tipo_oper)
#es--------------------------------

    DEFINE tipo_oper       CHAR(1),
           cla_where       CHAR(300),
           sel_where       CHAR(1000),
           mostrar         CHAR(1000),
           vstatus_interno SMALLINT,
           cont            SMALLINT,
           pos             INTEGER,
           L               INTEGER

    OPEN WINDOW ventana_2 AT 7,5 WITH FORM "AFIM0185" ATTRIBUTE( BORDER)

    DISPLAY "                         OPCIONES DE CONSULTA                                  " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY "[ Ctrl-C ] Salir" AT 1,1 --ATTRIBUTES(REVERSE)

    LET cont = 2

    IF (cont-1) >= 1 THEN
        CALL SET_COUNT(cont-1)

        LET int_flag = FALSE

        CONSTRUCT BY NAME cla_where ON usuario,
                                       n_seguro,
                                       n_folio, 
                                       tipo_solicitud,
                                       n_unico,
                                       cod_promotor,
                                       status_interno

        IF int_flag = TRUE THEN
            LET int_flag = FALSE
            ERROR "BUSQUEDA CANCELADA..."
            SLEEP 2
            ERROR ""
            CLOSE WINDOW ventana_2
            RETURN 
        END IF

        IF ordena() = TRUE THEN #o

        CLEAR FORM
        LET z_reg2[1] = z_reg.orden_1
        LET z_reg2[2] = z_reg.orden_2
        LET z_reg2[3] = z_reg.orden_3
        LET z_reg2[4] = z_reg.orden_4
        LET z_reg2[5] = z_reg.orden_5

        LET sel_where = " SELECT a.usuario, a.n_seguro, a.n_folio, ", 
                        " a.tipo_solicitud, a.n_unico, a.cod_promotor, ",
                        " a.status_interno, ",
              " CASE WHEN a.status_interno= 5 THEN ","'","BAJA","'",
                   " WHEN a.status_interno=10 THEN ","'","CAPTURADA","'",
                   " WHEN a.status_interno=12 THEN ","'","SIN DIGITALIZAR","'",
                   " WHEN a.status_interno=15 THEN ","'","CONFIRMADA","'",
                   " WHEN a.status_interno=20 THEN ","'","VALIDADA","'",
              " END CASE", " FROM afi_solicitud a " 

        CASE tipo_oper 
            WHEN "C" 
                LET sel_where = sel_where CLIPPED,
                                " WHERE a.status_interno in(10,12) "
            WHEN "M"
                LET sel_where = sel_where CLIPPED,
                                " WHERE a.status_interno IN (15,20) "
            WHEN "A"
                LET sel_where = sel_where CLIPPED, 
                                " WHERE a.status_interno = 5 "
            WHEN "B"
                LET sel_where = sel_where CLIPPED,
                                " WHERE a.status_interno in(10,12,15,20) "
            WHEN "F"
                LET sel_where = sel_where CLIPPED,
                                " WHERE a.status_interno <= 20 "
            WHEN "S"
                LET sel_where = sel_where CLIPPED,
                                " WHERE a.status_interno <= 20 "
            WHEN "P"
                LET sel_where = sel_where CLIPPED, 
                                " WHERE a.status_interno <= 20 "
        END CASE
     
        LET sel_where = sel_where CLIPPED,
                        " AND ", cla_where CLIPPED,
                        " ORDER BY ", z_reg.orden_1,",",z_reg.orden_2,",",
                                      z_reg.orden_3,",",z_reg.orden_4,",",
                                      z_reg.orden_5
  
        LET sel_where = sel_where CLIPPED

        ---LET mostrar = "echo ",sel_where clipped, " > x.sql"
        ---RUN mostrar

        PREPARE qry_consul FROM sel_where

        DECLARE cursor_c CURSOR FOR qry_consul

        LET pos = 1

        FOREACH cursor_c INTO reg[pos].usuario,
                              reg[pos].n_seguro,
                              reg[pos].n_folio,
                              reg[pos].tipo_solicitud,
                              reg[pos].n_unico,
                              reg[pos].cod_promotor,
                              reg[pos].status_interno,
                              reg[pos].desc_status_int

            LET pos = pos + 1
        END FOREACH

        CLOSE WINDOW ventana_2

        INITIALIZE reg[pos].* TO NULL

        IF (pos-1) >= 1 THEN
            CALL  SET_COUNT(pos-1)

            LET int_flag = FALSE

            DISPLAY ARRAY reg TO  scr_1.*
               ON KEY ( CONTROL-M )
                  LET pos = ARR_CURR()
                  LET L   = SCR_LINE()

                  IF tipo_oper = 'C' OR 
                     tipo_oper = 'M' THEN
                      CALL modifica ( reg[L].n_seguro,
                                      reg[L].n_folio,
                                      reg[L].tipo_solicitud,
                                      tipo_oper
                                    )
                  ELSE
                      IF tipo_oper = 'A' OR
                         tipo_oper = 'B' THEN
                          CALL cambia ( reg[L].n_seguro,
                                          reg[L].n_folio,
                                          reg[L].tipo_solicitud,
                                          tipo_oper
                                        )
                      ELSE
                          CALL modifica_fsp ( reg[L].n_seguro,
                                              reg[L].n_folio,
                                              reg[L].tipo_solicitud,
                                              tipo_oper
                                            )
                      END IF
                  END IF

                  DISPLAY su_estatus  TO scr_1[L].status_interno
                  DISPLAY des_estatus TO scr_1[L].desc_status_int

                  IF tipo_oper = 'F' THEN
                      DISPLAY des_folio TO scr_1[L].n_folio
                  END IF

                  IF tipo_oper = 'S' THEN
                      DISPLAY des_nss TO scr_1[L].n_seguro
                  END IF

                  IF tipo_oper = 'P' THEN
                      DISPLAY des_promotor TO scr_1[L].cod_promotor
                  END IF

                ON KEY ( INTERRUPT)
                    LET int_flag = FALSE
                    EXIT DISPLAY

                END DISPLAY
            ELSE
                CASE tipo_oper
                    WHEN 'C'
                        ERROR "ARCHIVO DE SOLICITUDES SIN CONFIRMAR VACIO"
                    WHEN 'M'
                        ERROR "ARCHIVO DE SOLICITUDES CONFIRMADAS/VALIDADAS VACIO"
                    WHEN 'F'
                        ERROR "ARCHIVO DE SOLICITUDES DE CAMBIO DE FOLIO VACIO"
                    WHEN 'S'
                        ERROR "ARCHIVO DE SOLICITUDES DE CAMBIO DE NSS VACIO"
                    WHEN 'P'
                        ERROR "ARCHIVO DE SOLICITUDES DE CAMBIO DE COD. PROMOTOR VACIO"
                END CASE

                SLEEP 2
                ERROR ""
                RETURN 
            END IF
        ELSE
            CLOSE WINDOW ventana_2
            RETURN 
        END IF
    END IF

END FUNCTION

FUNCTION ordena()
#o---------------
    DEFINE   resultado   SMALLINT

    LET resultado = TRUE

    OPEN WINDOW ventana_3 AT 7,5 WITH FORM "AFIM0184" ATTRIBUTE(BORDER)
    DISPLAY " [ Esc ] Grabar      [ Ctrl-C ] Salir"                                                 AT 1,1 ATTRIBUTE(BOLD) 
    DISPLAY "                      OPCIONES DE ORDENAMIENTO                                        " AT 2,1 ATTRIBUTE(REVERSE,BOLD)

    LET z_reg.orden_1 = 1
    LET z_reg.orden_2 = 2
    LET z_reg.orden_3 = 3
    LET z_reg.orden_4 = 4
    LET z_reg.orden_5 = 5

    DISPLAY BY NAME z_reg.*
       
    INPUT BY NAME z_reg.* WITHOUT DEFAULTS
      AFTER FIELD orden_1
         IF z_reg.orden_1 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_1
         ELSE
            IF z_reg.orden_1 < 1 OR z_reg.orden_1 > 5 THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_1
            END IF
         END IF

      AFTER FIELD orden_2
         IF z_reg.orden_2 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_2
         ELSE
            IF z_reg.orden_2 < 1 OR z_reg.orden_2 > 5  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_2
            END IF
            IF z_reg.orden_2 = z_reg.orden_1 THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_2
            END IF
         END IF

      AFTER FIELD orden_3
         IF z_reg.orden_3 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_3
         ELSE
            IF z_reg.orden_3 < 1 OR z_reg.orden_3 > 5 THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_3
            END IF
            IF (z_reg.orden_3 = z_reg.orden_1)
               OR (z_reg.orden_3 = z_reg.orden_2) THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_3
            END IF
         END IF

      AFTER FIELD orden_4
         IF z_reg.orden_4 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_4
         ELSE
            IF z_reg.orden_4 < 1 OR z_reg.orden_4 > 5  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_4
            END IF
            IF (z_reg.orden_4 = z_reg.orden_1)
               OR (z_reg.orden_4 = z_reg.orden_2)
               OR (z_reg.orden_4 = z_reg.orden_3) THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_4
            END IF
         END IF

      AFTER FIELD orden_5
         IF z_reg.orden_5 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_5
         ELSE
            IF z_reg.orden_5 < 1 OR z_reg.orden_5 > 5  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_5
            END IF
            IF (z_reg.orden_5 = z_reg.orden_1)
               OR (z_reg.orden_5 = z_reg.orden_2)
               OR (z_reg.orden_5 = z_reg.orden_3) 
               OR (z_reg.orden_5 = z_reg.orden_4) THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_5
            END IF
         END IF

      ON KEY ( ESC )
         IF z_reg.orden_1 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_1
         ELSE
            IF z_reg.orden_1 < 1 OR z_reg.orden_1 > 5  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_1
            END IF
         END IF

         IF z_reg.orden_2 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_2
         ELSE
            IF z_reg.orden_2 < 1 OR z_reg.orden_2 > 5  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_2
            END IF
            IF z_reg.orden_2 = z_reg.orden_1 THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_2
            END IF
         END IF

         IF z_reg.orden_3 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_3
         ELSE
            IF z_reg.orden_3 < 1 OR z_reg.orden_3 > 5  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_3
            END IF
            IF (z_reg.orden_3 = z_reg.orden_1)
               OR (z_reg.orden_3 = z_reg.orden_2) THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_3
            END IF
         END IF

         IF z_reg.orden_4 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_4
         ELSE
            IF z_reg.orden_4 < 1 OR z_reg.orden_4 > 5  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_4
            END IF
            IF (z_reg.orden_4 = z_reg.orden_1)
               OR (z_reg.orden_4 = z_reg.orden_2)
               OR (z_reg.orden_4 = z_reg.orden_3) THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_4
            END IF
         END IF

         IF z_reg.orden_5 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_5
         ELSE
            IF z_reg.orden_5 < 1 OR z_reg.orden_5 > 5  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_5
            END IF
            IF (z_reg.orden_5 = z_reg.orden_1)
               OR (z_reg.orden_5 = z_reg.orden_2)
               OR (z_reg.orden_5 = z_reg.orden_3) 
               OR (z_reg.orden_5 = z_reg.orden_4) THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_5
            END IF
         END IF

        ON KEY ( INTERRUPT )
            LET int_flag = FALSE
            LET resultado = FALSE
            ERROR "BUSQUEDA CANCELADA..."
            SLEEP 2
            ERROR ""
            EXIT INPUT
    END INPUT

    CLOSE WINDOW ventana_3

    RETURN resultado

END FUNCTION

FUNCTION cambia(vn_seguro, vn_folio, vtipo_solicitud,vtipo_oper)
#c--------------------------------------------------------------

    DEFINE  vn_seguro        LIKE afi_solicitud.n_seguro
    DEFINE  vn_folio         LIKE afi_solicitud.n_folio
    DEFINE  vtipo_solicitud  LIKE afi_solicitud.tipo_solicitud
    DEFINE  vtipo_oper       CHAR(1)
    DEFINE fecha_comprueba   DATE
    DEFINE xx_status         SMALLINT
    DEFINE rescato           SMALLINT

    OPEN WINDOW ventana_5 AT 2,2 WITH FORM "AFIM0011" ATTRIBUTE(BORDER)
    DISPLAY " AFIM018             CONFIRMACION SOLICITUDES AFILIACION                      " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                              " AT 5,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    DISPLAY "                                                                               " AT 13,1 ATTRIBUTE(REVERSE)
    DISPLAY "                          Estructura Comercial                                 "AT 18,1 ATTRIBUTE(REVERSE)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " CONFIRMA " AT 1,69 ATTRIBUTE(REVERSE)

    DISPLAY "[ Esc ] Graba  [ Ctrl-C ] Salir sin Grabar"
            AT 1,1
    DISPLAY "CTRL: [E] Domicilios [T] Tels [B] Benef [V] Patrones  [P] Icefas" AT 2,1 ATTRIBUTE(BOLD) 
    LET sw_1 = 0
    LET rescato = TRUE
    LET int_flag = FALSE

    INPUT BY NAME r_afi.tipo_solicitud,
                  r_afi.n_folio,
                  r_afi.n_seguro,
                  r_afi.cod_promotor WITHOUT DEFAULTS

        BEFORE INPUT
            SELECT  a.cod_promotor, 
                    p.nombres, 
                    p.paterno, 
                    p.materno,
                    a.codven, 
                    a.agenc_cod, 
                    nombre_uni_n1, 
                    a.tipo_solicitud, 
                    a.n_folio, 
                    a.fecha_elaboracion,  
                    a.cod_afore_ced, 
                    afore_desc, 
                    a.femision,  
                    a.frecafor, 
                    a.paterno, 
                    a.materno, 
                    a.nombres, 
                    a.n_seguro, 
                    a.n_rfc, 
                    a.n_unico, 
                    a.sexo, 
                    sexo_desc, 
                    a.fena, 
                    a.ind_infonavit, 
                    a.estadon, 
                    estad_desc, 
                    a.nacionalidad, 
                    pais_desc, 
                    a.tip_prob, 
                    docprob_desc, 
                    a.fol_prob, 
                    a.doc_prob, 
                    a.cod_error_origen,
                    a.status_interno,
                    a.usuario
              INTO
                    r_afi.cod_promotor, 
                    nom , 
                    pat, 
                    mat,
                    r_afi.codven, 
                    r_afi.agenc_cod, 
                    r_afi.agenc_desc,
                    r_afi.tipo_solicitud, 
                    r_afi.n_folio, 
                    r_afi.fecha_elaboracion, 
                    r_afi.cod_afore_ced, 
                    r_afi.desc_afore, 
                    r_afi.fecha_emision, 
                    r_afi.frecafor, 
                    r_afi.paterno, 
                    r_afi.materno,
                    r_afi.nombres, 
                    r_afi.n_seguro, 
                    r_afi.n_rfc,
                    r_afi.n_unico, 
                    r_afi.sexo, 
                    r_afi.desc_sexo, 
                    r_afi.fena,
                    r_afi.ind_infonavit,
                    r_afi.estadon, 
                    r_afi.desc_estadon, 
                    r_afi.nacionalidad, 
                    r_afi.desc_nacionalidad,
                    r_afi.tip_prob, 
                    r_afi.docprob_desc,
                    r_afi.fol_prob, 
                    r_afi.doc_prob, 
                    r_afi.cod_error_orig,
                    su_estatus,
                    r_afi.usuario1
              FROM  afi_solicitud a,
                    pro_mae_promotor p,
                    tab_doc_prob td,
              OUTER(com_nivel1 c,
                    tab_afore tf ,
                    tab_sexo  ts,
                    tab_pais  tp,
                    tab_estado tedo)
              WHERE a.n_seguro       = vn_seguro
                AND a.n_folio        = vn_folio 
                AND a.tipo_solicitud = vtipo_solicitud
                AND a.cod_promotor   = p.cod_promotor
                AND a.agenc_cod      = c.coduni_n1
                AND a.cod_afore_ced  = tf.afore_cod
                AND a.sexo           = ts.sexo_cod
                AND a.nacionalidad   = tp.pais_cod
                AND a.estadon        = tedo.estad_cod
                AND a.tip_prob       = td.docprob_cod
 

              SELECT  profesion_cod,actividad_cod
                 INTO  r_afi.profesion_cod,r_afi.actividad_cod
                 FROM  afi_ctr_actividad
                WHERE  nss = r_afi.n_seguro
                  AND  n_folio = r_afi.n_folio
                  AND  tipo_solicitud = r_afi.tipo_solicitud

            # obtiene descripciones de profesion y actividad
               SELECT  profesion_desc
                 INTO  r_afi.profesion_desc
                 FROM  tab_profesion
                WHERE  profesion_cod = r_afi.profesion_cod

               IF SQLCA.SQLCODE <> 0 THEN
                 INITIALIZE   r_afi.profesion_desc TO NULL
               END IF

               SELECT  actividad_desc
                 INTO  r_afi.actividad_desc
                 FROM  tab_actividad
                WHERE  actividad_cod = r_afi.actividad_cod

               IF SQLCA.SQLCODE <> 0 THEN
                 INITIALIZE   r_afi.actividad_desc TO NULL
               END IF

          LET r_afi.nom_promotor = pat CLIPPED, " ",
                                      mat CLIPPED, " ",
                                      nom CLIPPED

          CASE su_estatus
              WHEN  5 LET des_estatus = 'BAJA SOLIC'
              WHEN 10 LET des_estatus = 'CAPTURADA'
              WHEN 15 LET des_estatus = 'CONFIRMADA'
              WHEN 20 LET des_estatus = 'VALIDADA'
          END CASE

          DISPLAY BY NAME r_afi.*

          BEFORE FIELD tipo_solicitud
          -- issa BEFORE FIELD cod_promotor
          IF vtipo_oper = 'B' THEN
              IF su_estatus <= 20 OR
                 su_estatus > 5 THEN
                  WHILE TRUE
                  PROMPT "Esta seguro de dar de baja la solicitud [S/N] ? " 
                  ATTRIBUTES (REVERSE) FOR aux_pausa
                  IF aux_pausa  MATCHES "[SsNn]" THEN
                      IF aux_pausa MATCHES "[Nn]" THEN
                          EXIT INPUT
                          RETURN
                      ELSE
                          UPDATE afi_solicitud
                          SET    status_interno = 5
                          WHERE  n_folio = r_afi.n_folio
                          AND    tipo_solicitud = r_afi.tipo_solicitud

                          ERROR "Registro actualizado con status de baja"
                          SLEEP 3
                          ERROR  ""

                          DISPLAY " BAJA SOLCIITUD " AT 5,56 
                          ATTRIBUTE(REVERSE)

                          LET su_estatus  = 5
                          LET des_estatus = 'BAJA SOLICITUD'

                          CALL inserta_logico(r_afi.n_folio,
                                              r_afi.tipo_solicitud,
                                              r_afi.n_seguro,
                                              st_int,g_usuario,des_estatus)

                          EXIT WHILE
                      END IF
                  ELSE
                      ERROR "Solo debe presionar (S)i o (N)o" 
                  END IF
                  END WHILE
              ELSE
                  PROMPT "Status no puede modificarse, [Enter] p/continuar" 
                  FOR enter
                  EXIT INPUT
                  RETURN
              END IF
          ELSE
              IF su_estatus = 5 THEN
                  WHILE TRUE
                  PROMPT "Esta seguro de dar de alta la solicitud [S/N] ? " 
                  ATTRIBUTES (REVERSE) FOR aux_pausa
                  IF aux_pausa  MATCHES "[SsNn]" THEN
                      IF aux_pausa MATCHES "[Nn]" THEN
                          EXIT INPUT
                          RETURN
                      ELSE
                          UPDATE afi_solicitud
                          SET    status_interno = 10,
                                 frecafor = HOY
                          WHERE  n_folio = r_afi.n_folio
                          AND    tipo_solicitud = r_afi.tipo_solicitud

                          ERROR "Registro actualizado con status de alta"
                          SLEEP 3
                          ERROR  ""

                          DISPLAY " SOLICITUD CAPTURADA  " AT 5,56 
                          ATTRIBUTE(REVERSE)

                          LET su_estatus  = 10
                          LET des_estatus = 'ALTA SOLICITUD'

                          CALL inserta_logico(r_afi.n_folio,
                                              r_afi.tipo_solicitud,
                                              r_afi.n_seguro,
                                              st_int,g_usuario,des_estatus)

                          EXIT WHILE
                      END IF
                  ELSE
                      ERROR "Solo debe presionar (S)i o (N)o"
                  END IF
                  END WHILE
              ELSE
                  PROMPT "Status no puede modificarse, [Enter] p/continuar" 
                  FOR enter
                  EXIT INPUT
                  RETURN
              END IF
          END IF

        END INPUT

    CLOSE WINDOW ventana_5

END FUNCTION

FUNCTION modifica_fsp(vn_seguro, vn_folio, vtipo_solicitud,vtipo_oper)
#mfsp-----------------------------------------------------------------

    DEFINE vn_seguro       LIKE  afi_solicitud.n_seguro
    DEFINE vn_folio        LIKE  afi_solicitud.n_folio
    DEFINE vtipo_solicitud LIKE  afi_solicitud.tipo_solicitud
    DEFINE vtipo_oper      CHAR(1)
    DEFINE fecha_comprueba DATE
    DEFINE xx_status       SMALLINT
    DEFINE rescato         SMALLINT
    DEFINE recha           SMALLINT
    DEFINE folio_ant       DECIMAL(8,0)
    DEFINE folio_nvo       DECIMAL(8,0)
    DEFINE nss_ant         CHAR(11)
    DEFINE nss_nvo         CHAR(11)
    DEFINE pro_ant         CHAR(10)
    DEFINE pro_nvo         CHAR(10)

    OPEN WINDOW ventana_6 AT 2,2 WITH FORM "AFIM0011" ATTRIBUTE(BORDER)
    DISPLAY " AFIM018             CONFIRMACION SOLICITUDES AFILIACION                      " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                              " AT 5,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " CONFIRMA " AT 1,69 ATTRIBUTE(REVERSE)

    DISPLAY "[ Esc ] Graba  [ Ctrl-C ] Salir sin Grabar" AT 1,1
    DISPLAY "CTRL: [E] Domicilios [T] Tels [B] Benef [V] Patrones [P] Icefas" AT 2,1 ATTRIBUTE(BOLD) 

    LET sw_1     = 0
    LET rescato  = TRUE
    LET int_flag = FALSE

    CASE vtipo_oper
        WHEN 'F'
            INPUT BY NAME r_afi.cod_promotor,
                          r_afi.n_seguro, 
                          r_afi.n_folio,
                          r_afi.tipo_solicitud WITHOUT DEFAULTS

            BEFORE INPUT
                SELECT  a.cod_promotor, p.nombres, p.paterno, p.materno,
                        a.codven, a.agenc_cod, nombre_uni_n1, 
                        a.tipo_solicitud, a.n_folio, 
                        a.fecha_elaboracion,  
                        a.cod_afore_ced, afore_desc, 
                        a.femision,  
                        a.frecafor, a.paterno, a.materno, 
                        a.nombres, a.n_seguro, a.n_rfc, 
                        a.n_unico, a.sexo, sexo_desc, 
                        a.fena, 
                        a.ind_infonavit, 
                        a.estadon, estad_desc, 
                        a.nacionalidad, pais_desc, 
                        a.tip_prob, docprob_desc, 
                        a.fol_prob, a.doc_prob, 
                        a.cod_error_origen,
                        a.status_interno
                  INTO
                        r_afi.cod_promotor, nom , pat, mat,
                        r_afi.codven, r_afi.agenc_cod, 
                        r_afi.agenc_desc,
                        r_afi.tipo_solicitud, r_afi.n_folio, 
                        r_afi.fecha_elaboracion, 
                        r_afi.cod_afore_ced, r_afi.desc_afore, 
                        r_afi.fecha_emision, 
                        r_afi.frecafor, r_afi.paterno, r_afi.materno,
                        r_afi.nombres, r_afi.n_seguro, r_afi.n_rfc,
                        r_afi.n_unico, r_afi.sexo, r_afi.desc_sexo, 
                        r_afi.fena,
                        r_afi.ind_infonavit,
                        r_afi.estadon, r_afi.desc_estadon, 
                        r_afi.nacionalidad, r_afi.desc_nacionalidad,
                        r_afi.tip_prob, r_afi.docprob_desc,
                        r_afi.fol_prob, r_afi.doc_prob, 
                        r_afi.cod_error_orig,
                        su_estatus
                  FROM  afi_solicitud a,
                        pro_mae_promotor p,
                        tab_doc_prob td,
                  OUTER com_nivel1 c,
                  OUTER tab_afore tf,
                  OUTER tab_sexo  ts,
                  OUTER tab_pais  tp,
                  OUTER tab_estado tedo
                  WHERE a.n_seguro       = vn_seguro
                    AND a.n_folio        = vn_folio 
                    AND a.tipo_solicitud = vtipo_solicitud
                    AND a.cod_promotor   = p.cod_promotor
                    AND a.agenc_cod      = c.coduni_n1
                    AND a.cod_afore_ced  = tf.afore_cod
                    AND a.sexo           = ts.sexo_cod
                    AND a.nacionalidad   = tp.pais_cod
                    AND a.estadon        = tedo.estad_cod
                    AND a.tip_prob       = td.docprob_cod

              SELECT  profesion_cod,actividad_cod
                 INTO  r_afi.profesion_cod,r_afi.actividad_cod
                 FROM  afi_ctr_actividad
                WHERE  nss = r_afi.n_seguro
                  AND  n_folio = r_afi.n_folio
                  AND  tipo_solicitud = r_afi.tipo_solicitud

            # obtiene descripciones de profesion y actividad
               SELECT  profesion_desc
                 INTO  r_afi.profesion_desc
                 FROM  tab_profesion
                WHERE  profesion_cod = r_afi.profesion_cod

               IF SQLCA.SQLCODE <> 0 THEN
                 INITIALIZE   r_afi.profesion_desc TO NULL
               END IF

               SELECT  actividad_desc
                 INTO  r_afi.actividad_desc
                 FROM  tab_actividad
                WHERE  actividad_cod = r_afi.actividad_cod

               IF SQLCA.SQLCODE <> 0 THEN
                 INITIALIZE   r_afi.actividad_desc TO NULL
               END IF
    
            LET r_afi.nom_promotor = pat CLIPPED, " ",
                                        mat CLIPPED, " ",
                                        nom CLIPPED
            CASE su_estatus
                WHEN  5 LET des_estatus = 'BAJA SOLIC'
                WHEN 10 LET des_estatus = 'CAPTURADA'
                WHEN 15 LET des_estatus = 'CONFIRMADA'
                WHEN 20 LET des_estatus = 'VALIDADA'
            END CASE

            DISPLAY BY NAME r_afi.*

            LET folio_ant = r_afi.n_folio
            LET des_folio = r_afi.n_folio

            BEFORE FIELD cod_promotor 
                NEXT FIELD n_folio

            BEFORE FIELD n_folio

                ERROR "Ingrese el nuevo numero de folio "

            AFTER FIELD n_folio
                IF r_afi.n_folio IS NULL THEN
                    NEXT FIELD n_folio
                END IF

                LET folio_nvo        = r_afi.n_folio
                LET r_afi.n_folio = folio_ant

                SELECT "X"
                FROM   afi_mae_afiliado
                WHERE  n_folio = folio_nvo
                AND    tipo_solicitud = r_afi.tipo_solicitud

                IF STATUS = NOTFOUND THEN
                    SELECT "X"
                    FROM   afi_solicitud
                    WHERE  n_folio = folio_nvo
                    AND    tipo_solicitud = r_afi.tipo_solicitud

                    IF STATUS = NOTFOUND THEN
                        SELECT "X" 
                        FROM   afi_mae_modifica
                        WHERE  @folio_nvo      = folio_nvo
                        AND    @tipo_solicitud = r_afi.tipo_solicitud
                        AND    @cod_operacion  = 0
                        AND    @diag_proceso   = 0

                        IF STATUS = NOTFOUND THEN
                            ERROR "Actualizando registro "

                            UPDATE afi_solicitud
                            SET    n_folio = folio_nvo
                            WHERE  n_folio = r_afi.n_folio
                            AND    tipo_solicitud = r_afi.tipo_solicitud 
                            AND    n_seguro = r_afi.n_seguro

                            UPDATE afi_patron
                            SET    n_folio = folio_nvo
                            WHERE  n_folio = r_afi.n_folio
                            AND    tipo_solicitud = r_afi.tipo_solicitud 
                            AND    n_seguro = r_afi.n_seguro

                            UPDATE afi_domicilio
                            SET    n_folio = folio_nvo
                            WHERE  n_folio = r_afi.n_folio
                            AND    tipo_solicitud = r_afi.tipo_solicitud 
                            AND    nss = r_afi.n_seguro

                            UPDATE afi_telefono
                            SET    n_folio = folio_nvo
                            WHERE  n_folio = r_afi.n_folio
                            AND    tipo_solicitud = r_afi.tipo_solicitud 
                            AND    nss = r_afi.n_seguro

                            UPDATE afi_icefa
                            SET    n_folio = folio_nvo
                            WHERE  n_folio = r_afi.n_folio
                            AND    tipo_solicitud = r_afi.tipo_solicitud 
                            AND    n_seguro = r_afi.n_seguro

                            UPDATE afi_beneficiario
                            SET    n_folio = folio_nvo
                            WHERE  n_folio = r_afi.n_folio
                            AND    tipo_solicitud = r_afi.tipo_solicitud
                            AND    n_seguro = r_afi.n_seguro

                            WHENEVER ERROR CONTINUE

                            UPDATE afi_recepcion
                            SET    n_folio = folio_nvo
                            WHERE  n_folio = r_afi.n_folio
                            AND    tipo_solicitud = r_afi.tipo_solicitud
                            AND    n_seguro = r_afi.n_seguro

                            UPDATE afi_expediente
                            SET    n_folio = folio_nvo
                            WHERE  n_folio = r_afi.n_folio
                            AND    tipo_solicitud = r_afi.tipo_solicitud
                            AND    n_seguro = r_afi.n_seguro

                            UPDATE afi_condicion_exp
                            SET    n_folio = folio_nvo
                            WHERE  n_folio = r_afi.n_folio
                            AND    tipo_solicitud = r_afi.tipo_solicitud

                            WHENEVER ERROR STOP

                            LET r_afi.n_folio = folio_nvo
                            LET des_folio        = r_afi.n_folio

                            IF NOT Rescata_datos("N",r_afi.n_folio) THEN
                                ERROR "Afiliado NO existe"
                                NEXT FIELD n_folio
                            END IF

                            DISPLAY BY NAME r_afi.*

                            LET operacion = 'MODIFICA FOLIO'

                            CALL inserta_logico(r_afi.n_folio,
                                                r_afi.tipo_solicitud,
                                                r_afi.n_seguro,
                                                st_int,g_usuario,operacion)

                            PROMPT "Presione [Enter] para finalizar " FOR enter
                            EXIT INPUT
                        ELSE
                            ERROR "Folio ya ingresado anteriormente"
                            SLEEP 3
                            EXIT INPUT
                        END IF
                    ELSE
                        ERROR "Folio ya ingresado anteriormente"
                        SLEEP 3
                        EXIT INPUT
                    END IF
                ELSE
                    ERROR "Folio ya ingresado anteriormente "
                    SLEEP 3
                    EXIT INPUT
                END IF

            ON KEY (INTERRUPT)
                EXIT INPUT

            END INPUT
        WHEN 'S'
            IF r_afi.tipo_solicitud = 8 THEN
                ERROR "NO PUEDE MODIFICAR NTI"
                SLEEP 3
                ERROR ""

                RETURN
            END IF

            INPUT BY NAME r_afi.cod_promotor,
                          r_afi.n_seguro, 
                          r_afi.n_folio,
                          r_afi.tipo_solicitud WITHOUT DEFAULTS

            BEFORE INPUT
                SELECT  a.cod_promotor, p.nombres, p.paterno, p.materno,
                        a.codven, a.agenc_cod, nombre_uni_n1, 
                        a.tipo_solicitud, a.n_folio, 
                        a.fecha_elaboracion,  
                        a.cod_afore_ced, afore_desc, 
                        a.femision,  
                        a.frecafor, a.paterno, a.materno, 
                        a.nombres, a.n_seguro, a.n_rfc, 
                        a.n_unico, a.sexo, sexo_desc, 
                        a.fena, 
                        a.ind_infonavit, 
                        a.estadon, estad_desc, 
                        a.nacionalidad, pais_desc, 
                        a.tip_prob, docprob_desc, 
                        a.fol_prob, a.doc_prob, 
                        a.cod_error_origen,
                        a.status_interno
                  INTO
                        r_afi.cod_promotor, nom , pat, mat,
                        r_afi.codven, r_afi.agenc_cod, 
                        r_afi.agenc_desc,
                        r_afi.tipo_solicitud, r_afi.n_folio, 
                        r_afi.fecha_elaboracion, 
                        r_afi.cod_afore_ced, r_afi.desc_afore, 
                        r_afi.fecha_emision, 
                        r_afi.frecafor, r_afi.paterno, r_afi.materno,
                        r_afi.nombres, r_afi.n_seguro, r_afi.n_rfc,
                        r_afi.n_unico, r_afi.sexo, r_afi.desc_sexo, 
                        r_afi.fena,
                        r_afi.ind_infonavit,
                        r_afi.estadon, r_afi.desc_estadon, 
                        r_afi.nacionalidad, r_afi.desc_nacionalidad,
                        r_afi.tip_prob, r_afi.docprob_desc,
                        r_afi.fol_prob, r_afi.doc_prob, 
                        r_afi.cod_error_orig,
                        su_estatus
                  FROM  afi_solicitud a,
                        pro_mae_promotor p,
                        tab_doc_prob td,
                  OUTER com_nivel1 c,
                  OUTER tab_afore tf,
                  OUTER tab_sexo  ts,
                  OUTER tab_pais  tp,
                  OUTER tab_estado tedo
                  WHERE a.n_seguro       = vn_seguro
                    AND a.n_folio        = vn_folio 
                    AND a.tipo_solicitud = vtipo_solicitud
                    AND a.cod_promotor   = p.cod_promotor
                    AND a.agenc_cod      = c.coduni_n1
                    AND a.cod_afore_ced  = tf.afore_cod
                    AND a.sexo           = ts.sexo_cod
                    AND a.nacionalidad   = tp.pais_cod
                    AND a.estadon        = tedo.estad_cod
                    AND a.tip_prob       = td.docprob_cod

            LET r_afi.nom_promotor = pat CLIPPED, " ",
                                        mat CLIPPED, " ",
                                        nom CLIPPED
            CASE su_estatus
                WHEN  5 LET des_estatus = 'BAJA SOLIC'
                WHEN 10 LET des_estatus = 'CAPTURADA'
                WHEN 15 LET des_estatus = 'CONFIRMADA'
                WHEN 20 LET des_estatus = 'VALIDADA'
            END CASE

            DISPLAY BY NAME r_afi.*

            LET nss_ant = r_afi.n_seguro
            LET des_nss = r_afi.n_seguro

            BEFORE FIELD cod_promotor
                NEXT FIELD n_seguro

            BEFORE FIELD n_seguro

                ERROR "Ingrese el nuevo nss "

            AFTER FIELD n_seguro
                IF r_afi.n_seguro IS NULL THEN
                    NEXT FIELD n_seguro
                END IF

                LET nss_nvo           = r_afi.n_seguro

                CALL digito_verif(r_afi.n_seguro[1,10],10) RETURNING digito

                IF digito = 32000 THEN
                    ERROR "N.S.S. solo contiene digitos"
                    NEXT FIELD n_seguro
                END IF

                IF LENGTH(r_afi.n_seguro) = 11 AND
                   digito <> r_afi.n_seguro[11] THEN
                    ERROR "Digito Verificador Invalido, el digito debe ser:  ",
                    digito
                    SLEEP 3
                    NEXT FIELD n_seguro
                END IF

                IF r_afi.n_seguro[11] <> "1" AND
                   r_afi.n_seguro[11] <> "2" AND
                   r_afi.n_seguro[11] <> "3" AND
                   r_afi.n_seguro[11] <> "4" AND
                   r_afi.n_seguro[11] <> "5" AND
                   r_afi.n_seguro[11] <> "6" AND
                   r_afi.n_seguro[11] <> "7" AND
                   r_afi.n_seguro[11] <> "8" AND
                   r_afi.n_seguro[11] <> "9" AND
                   r_afi.n_seguro[11] <> "0" THEN
                    ERROR "N.S.S. solo contiene digitos"
                    NEXT FIELD n_seguro 
                END IF

                LET r_afi.n_seguro = nss_ant

                SELECT "X"
                FROM   afi_mae_afiliado
                WHERE  n_seguro = nss_nvo
                AND    tipo_solicitud = r_afi.tipo_solicitud

                IF STATUS = NOTFOUND THEN
                    SELECT "X"
                    FROM   afi_solicitud
                    WHERE  n_seguro = nss_nvo
                    AND    tipo_solicitud = r_afi.tipo_solicitud

                    IF STATUS = NOTFOUND THEN
                        ERROR "Actualizando registro "

                        UPDATE afi_solicitud
                        SET    n_seguro = nss_nvo
                        WHERE  n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud 
                        AND    n_seguro = r_afi.n_seguro

                        UPDATE afi_patron
                        SET    n_seguro = nss_nvo
                        WHERE  n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud 
                        AND    n_seguro = r_afi.n_seguro

                        UPDATE afi_domicilio
                        SET    nss = nss_nvo
                        WHERE  n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud 
                        AND    nss = r_afi.n_seguro

                        UPDATE afi_telefono
                        SET    nss = nss_nvo
                        WHERE  n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud 
                        AND    nss = r_afi.n_seguro

                        UPDATE afi_icefa
                        SET    n_seguro = nss_nvo
                        WHERE  n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud 
                        AND    n_seguro = r_afi.n_seguro

                        UPDATE afi_beneficiario
                        SET    n_seguro = nss_nvo
                        WHERE  n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud
                        AND    n_seguro = r_afi.n_seguro

                        WHENEVER ERROR CONTINUE

                        UPDATE afi_recepcion
                        SET    n_seguro = nss_nvo
                        WHERE  n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud
                        AND    n_seguro = r_afi.n_seguro

                        UPDATE afi_expediente
                        SET    n_seguro = nss_nvo
                        WHERE  n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud
                        AND    n_seguro = r_afi.n_seguro

                        UPDATE afi_condicion_exp
                        SET    n_seguro = nss_nvo
                        WHERE  n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud

                        WHENEVER ERROR STOP

                        LET r_afi.n_seguro = nss_nvo
                        LET des_nss           = r_afi.n_seguro

                        IF NOT Rescata_datos("N",r_afi.n_folio) THEN
                            ERROR "Afiliado NO existe"
                            NEXT FIELD n_seguro
                        END IF

                        DISPLAY BY NAME r_afi.*

                        LET operacion = 'MODIFICA NSS'

                        CALL inserta_logico(r_afi.n_folio,
                                            r_afi.tipo_solicitud,
                                            r_afi.n_seguro,
                                            st_int,g_usuario,operacion)

                        PROMPT "Presione [Enter] para finalizar " FOR enter
                        EXIT INPUT
                    ELSE
                        ERROR "NSS ya ingresado anteriormente"
                        SLEEP 3
                        EXIT INPUT
                    END IF
                ELSE
                    ERROR "NSS ya ingresado anteriormente "
                    SLEEP 3
                    EXIT INPUT
                END IF

            ON KEY (INTERRUPT)
                EXIT INPUT

            END INPUT
        WHEN 'P'
            INPUT BY NAME r_afi.cod_promotor,
                          r_afi.n_seguro, 
                          r_afi.n_folio,
                          r_afi.tipo_solicitud WITHOUT DEFAULTS

            BEFORE INPUT
                SELECT  a.cod_promotor, p.nombres, p.paterno, p.materno,
                        a.codven, a.agenc_cod, nombre_uni_n1, 
                        a.tipo_solicitud, a.n_folio, 
                        a.fecha_elaboracion,  
                        a.cod_afore_ced, afore_desc, 
                        a.femision,
                        a.frecafor, a.paterno, a.materno, 
                        a.nombres, a.n_seguro, a.n_rfc, 
                        a.n_unico, a.sexo, sexo_desc, 
                        a.fena, 
                        a.ind_infonavit, 
                        a.estadon, estad_desc, 
                        a.nacionalidad, pais_desc, 
                        a.tip_prob, docprob_desc, 
                        a.fol_prob, a.doc_prob, 
                        a.cod_error_origen,
                        a.status_interno
                  INTO
                        r_afi.cod_promotor, nom , pat, mat,
                        r_afi.codven, r_afi.agenc_cod, 
                        r_afi.agenc_desc,
                        r_afi.tipo_solicitud, r_afi.n_folio, 
                        r_afi.fecha_elaboracion, 
                        r_afi.cod_afore_ced, r_afi.desc_afore, 
                        r_afi.fecha_emision, 
                        r_afi.frecafor, r_afi.paterno, r_afi.materno,
                        r_afi.nombres, r_afi.n_seguro, r_afi.n_rfc,
                        r_afi.n_unico, r_afi.sexo, r_afi.desc_sexo, 
                        r_afi.fena,
                        r_afi.ind_infonavit,
                        r_afi.estadon, r_afi.desc_estadon, 
                        r_afi.nacionalidad, r_afi.desc_nacionalidad,
                        r_afi.tip_prob, r_afi.docprob_desc,
                        r_afi.fol_prob, r_afi.doc_prob, 
                        r_afi.cod_error_orig,
                        su_estatus
                  FROM  afi_solicitud a,
                        pro_mae_promotor p,
                        tab_doc_prob td,
                  OUTER com_nivel1 c,
                  OUTER tab_afore tf,
                  OUTER tab_sexo  ts,
                  OUTER tab_pais  tp,
                  OUTER tab_estado tedo
                  WHERE a.n_seguro       = vn_seguro
                    AND a.n_folio        = vn_folio 
                    AND a.tipo_solicitud = vtipo_solicitud
                    AND a.cod_promotor   = p.cod_promotor
                    AND a.agenc_cod      = c.coduni_n1
                    AND a.cod_afore_ced  = tf.afore_cod
                    AND a.sexo           = ts.sexo_cod
                    AND a.nacionalidad   = tp.pais_cod
                    AND a.estadon        = tedo.estad_cod
                    AND a.tip_prob       = td.docprob_cod

            LET r_afi.nom_promotor = pat CLIPPED, " ",
                                        mat CLIPPED, " ",
                                        nom CLIPPED
            CASE su_estatus
                WHEN  5 LET des_estatus = 'BAJA SOLIC'
                WHEN 10 LET des_estatus = 'CAPTURADA'
                WHEN 15 LET des_estatus = 'CONFIRMADA'
                WHEN 20 LET des_estatus = 'VALIDADA'
            END CASE

            DISPLAY BY NAME r_afi.*

            LET pro_ant      = r_afi.cod_promotor
            LET des_promotor = r_afi.cod_promotor

            BEFORE FIELD cod_promotor

                ERROR "Ingrese el nuevo cod. promotor "

            AFTER FIELD cod_promotor
                IF r_afi.cod_promotor IS NULL THEN
                    NEXT FIELD cod_promotor
                END IF

                LET pro_nvo        = r_afi.cod_promotor
                LET r_afi.cod_promotor = pro_ant

                SELECT p.status
                INTO   recha
                FROM   pro_mae_promotor p
                WHERE  p.cod_promotor = pro_nvo

                CASE recha 
                    WHEN 2 ERROR "PROMOTOR SUSPENDIDO" SLEEP 2
                        EXIT INPUT
                        RETURN
                    WHEN 3 ERROR "PROMOTOR CANCELADO" SLEEP 2 
                        EXIT INPUT
                        RETURN
                    OTHERWISE 
                        ERROR "PROMOTOR ACTIVO" SLEEP 1  
                END CASE 

                        ERROR "Actualizando registro "

                        UPDATE afi_solicitud
                        SET    cod_promotor = pro_nvo,
                               codven       = pro_nvo
                        WHERE  n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud 
                        AND    n_seguro = r_afi.n_seguro

                        WHENEVER ERROR CONTINUE

                        UPDATE afi_recepcion
                        SET    cod_promotor = pro_nvo
                        WHERE  n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud
                        AND    n_seguro = r_afi.n_seguro

                        WHENEVER ERROR STOP

                        LET r_afi.cod_promotor = pro_nvo
                        LET des_promotor          = r_afi.cod_promotor

                        IF NOT Rescata_datos("N",r_afi.n_folio) THEN
                            ERROR "Afiliado NO existe"
                            NEXT FIELD cod_promotor
                        END IF

                        DISPLAY BY NAME r_afi.*

                        LET operacion = 'MODIFICA PROMOTOR'

                        CALL inserta_logico(r_afi.n_folio,
                                            r_afi.tipo_solicitud,
                                            r_afi.n_seguro,
                                            st_int,g_usuario,operacion)

                        PROMPT "Presione [Enter] para finalizar " FOR enter
                        EXIT INPUT

            ON KEY (INTERRUPT)
                EXIT INPUT

            END INPUT
    END CASE

    CLOSE WINDOW ventana_6

END FUNCTION

FUNCTION digital(n_seguro, tipo_solicitud, n_folio, nombre)
    DEFINE  n_seguro       CHAR(11),
            tipo_solicitud SMALLINT,
            n_folio          DECIMAL(10,0),
            nombre         CHAR(40),
            codigo_doc1    SMALLINT,
            estado_doc1    SMALLINT,
            condic_doc1    SMALLINT,
            desc1          CHAR(30),
            codigo_doc2    SMALLINT,
            estado_doc2    SMALLINT,
            condic_doc2    SMALLINT,
            desc2          CHAR(30),
            codigo_doc3    SMALLINT,
            estado_doc3    SMALLINT,
            condic_doc3    SMALLINT,
            desc3          CHAR(30),
            codigo_doc4    SMALLINT,
            estado_doc4    SMALLINT,
            condic_doc4    SMALLINT,
            desc4          CHAR(30),
            codigo_doc5    SMALLINT,
            estado_doc5    SMALLINT,
            condic_doc5    SMALLINT,
            desc5          CHAR(30),
            sw, vv, band   SMALLINT,
            hora           CHAR(08)

    INITIALIZE desc1, desc2, desc3, desc4, desc5 TO NULL

    LET codigo_doc1 = 0   LET estado_doc1 = 0   LET condic_doc1 = 0
    LET codigo_doc2 = 0   LET estado_doc2 = 0   LET condic_doc2 = 0
    LET codigo_doc3 = 0   LET estado_doc3 = 0   LET condic_doc3 = 0
    LET codigo_doc4 = 0   LET estado_doc4 = 0   LET condic_doc4 = 0
    LET codigo_doc5 = 0   LET estado_doc5 = 0   LET condic_doc5 = 0
    LET sw          = 0   LET vv          = 0   LET band        = 0

    OPEN WINDOW digital_1 AT 5,3 WITH FORM "AFIM0189" ATTRIBUTE(BORDER)
    DISPLAY " < ESC > Aceptar                                      ",
            "< Ctrl-C > Cancelar      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "                          CONFIRMA DIGITALIZACION    ",
            "                         " AT 3,1 ATTRIBUTE(REVERSE)
        INPUT estado_doc1, condic_doc1,
              estado_doc2, condic_doc2,
              estado_doc3, condic_doc3,
              estado_doc4, condic_doc4,
              estado_doc5, condic_doc5
         FROM FORMONLY.estado_doc1, FORMONLY.condic_doc1,
              FORMONLY.estado_doc2, FORMONLY.condic_doc2,
              FORMONLY.estado_doc3, FORMONLY.condic_doc3,
              FORMONLY.estado_doc4, FORMONLY.condic_doc4,
              FORMONLY.estado_doc5, FORMONLY.condic_doc5

             BEFORE FIELD FORMONLY.estado_doc1
                    IF sw = 0 THEN
                       LET sw = 1
                       LET codigo_doc1 = 1
                       DISPLAY n_seguro       TO n_seguro 
                       DISPLAY nombre         TO nombre
                       DISPLAY n_folio        TO n_folio 
                       DISPLAY tipo_solicitud TO tipo_solicitud
                    END IF

                    LET valida_dg = 'i',n_seguro,'532011*' CLIPPED

                    CALL valida_imagen(valida_dg) RETURNING bnd_dg

                    IF bnd_dg THEN
                        LET estado_doc1 = bnd_dg
                        LET condic_doc1 = 1
                        LET desc1 = "NO DIGITALIZADO  "

                        DISPLAY estado_doc1 TO estado_doc1
                        DISPLAY condic_doc1 TO condic_doc1
                        DISPLAY desc1 TO desc1

                        NEXT FIELD FORMONLY.estado_doc2
                    END IF

             AFTER FIELD FORMONLY.estado_doc1
                   CASE estado_doc1
                      WHEN 0  NEXT FIELD FORMONLY.condic_doc1
                      WHEN 1  NEXT FIELD FORMONLY.condic_doc1
                      OTHERWISE
                         ERROR "Digite cero(0) o uno(1) "
                         NEXT FIELD FORMONLY.estado_doc1
                   END CASE

             BEFORE FIELD FORMONLY.condic_doc1
                   IF estado_doc1 = 0 THEN
                      LET condic_doc1 = 0 
                      LET desc1 = "PROCEDENTE       "
                   END IF

             AFTER FIELD FORMONLY.condic_doc1
                IF estado_doc1 = 1 THEN
                   CASE condic_doc1
                    WHEN 1   LET desc1 = "NO DIGITALIZADO  "
                    WHEN 2   LET desc1 = "ILEGIBLE         "
                    WHEN 3   LET desc1 = "NO COINCIDE      "
                    OTHERWISE
                      CALL dig_condic() RETURNING vv,condic_doc1, desc1  ##dig_c
                      IF vv = 0 THEN
                         DISPLAY condic_doc1   TO FORMONLY.condic_doc1
                         DISPLAY desc1         TO FORMONLY.desc1
                      ELSE
                         ERROR "Digite correctamente la Condicion del Docto."
                         NEXT FIELD FORMONLY.condic_doc1
                      END IF
                END CASE
                DISPLAY condic_doc1   TO FORMONLY.condic_doc1
                DISPLAY desc1         TO FORMONLY.desc1
             END IF 
             IF estado_doc1 = 0 THEN
                LET condic_doc1 = 0 
                LET desc1 = "PROCEDENTE       "
                DISPLAY condic_doc1   TO FORMONLY.condic_doc1
                DISPLAY desc1         TO FORMONLY.desc1
             END IF

             BEFORE FIELD FORMONLY.estado_doc2
                   LET codigo_doc2 = 9

                    LET valida_dg = 'i',n_seguro,'53209*' CLIPPED

                    CALL valida_imagen(valida_dg) RETURNING bnd_dg

                    IF bnd_dg THEN
                        LET estado_doc2 = bnd_dg
                        LET condic_doc2 = 1
                        LET desc2 = "NO DIGITALIZADO  "

                        DISPLAY estado_doc2 TO estado_doc2
                        DISPLAY condic_doc2 TO condic_doc2
                        DISPLAY desc2 TO desc2

                        NEXT FIELD FORMONLY.estado_doc3
                    END IF

             AFTER FIELD FORMONLY.estado_doc2
                   CASE estado_doc2
                      WHEN 0  NEXT FIELD FORMONLY.condic_doc2
                      WHEN 1  NEXT FIELD FORMONLY.condic_doc2
                      OTHERWISE
                         ERROR "Digite cero(0) o uno(1) "
                         NEXT FIELD FORMONLY.estado_doc2
                   END CASE

             BEFORE FIELD FORMONLY.condic_doc2
                   IF estado_doc2 = 0 THEN
                      LET condic_doc2 = 0 
                      LET desc2 = "PROCEDENTE       "
                   END IF

             AFTER FIELD FORMONLY.condic_doc2
                IF estado_doc2 = 1 THEN
                   CASE condic_doc2
                    WHEN 1   LET desc2 = "NO DIGITALIZADO  "
                    WHEN 2   LET desc2 = "ILEGIBLE         "
                    WHEN 3   LET desc2 = "NO COINCIDE      "
                    OTHERWISE
                      CALL dig_condic() RETURNING vv,condic_doc2, desc2  ##dig_c
                      IF vv = 0 THEN
                         DISPLAY condic_doc2   TO FORMONLY.condic_doc2
                         DISPLAY desc2         TO FORMONLY.desc2
                      ELSE
                         ERROR "Digite correctamente la Condicion del Docto."
                         NEXT FIELD FORMONLY.condic_doc2
                      END IF
                END CASE
                DISPLAY condic_doc2   TO FORMONLY.condic_doc2
                DISPLAY desc2         TO FORMONLY.desc2
             END IF 
             IF estado_doc2 = 0 THEN
                LET condic_doc2 = 0 
                LET desc2 = "PROCEDENTE       "
                DISPLAY condic_doc2   TO FORMONLY.condic_doc2
                DISPLAY desc2         TO FORMONLY.desc2
             END IF

             BEFORE FIELD FORMONLY.estado_doc3
                   LET codigo_doc3 = 2

                    LET valida_dg = 'i',n_seguro,'53202*' CLIPPED

                    CALL valida_imagen(valida_dg) RETURNING bnd_dg

                    IF bnd_dg THEN
                        LET estado_doc3 = bnd_dg
                        LET condic_doc3 = 1
                        LET desc3 = "NO DIGITALIZADO  "

                        DISPLAY estado_doc3 TO estado_doc3
                        DISPLAY condic_doc3 TO condic_doc3
                        DISPLAY desc3 TO desc3

                        NEXT FIELD FORMONLY.estado_doc4
                    END IF

             AFTER FIELD FORMONLY.estado_doc3
                   CASE estado_doc3
                      WHEN 0  NEXT FIELD FORMONLY.condic_doc3
                      WHEN 1  NEXT FIELD FORMONLY.condic_doc3
                      OTHERWISE
                         ERROR "Digite cero(0) o uno(1) "
                         NEXT FIELD FORMONLY.estado_doc3
                   END CASE

             BEFORE FIELD FORMONLY.condic_doc3
                   IF estado_doc3 = 0 THEN
                      LET condic_doc3 = 0 
                      LET desc3 = "PROCEDENTE       "
                   END IF

             AFTER FIELD FORMONLY.condic_doc3
                IF estado_doc3 = 1 THEN
                   CASE condic_doc3
                    WHEN 1   LET desc3 = "NO DIGITALIZADO  "
                    WHEN 2   LET desc3 = "ILEGIBLE         "
                    WHEN 3   LET desc3 = "NO COINCIDE      "
                    OTHERWISE
                      CALL dig_condic() RETURNING vv,condic_doc3, desc3  ##dig_c
                      IF vv = 0 THEN
                         DISPLAY condic_doc3   TO FORMONLY.condic_doc3
                         DISPLAY desc3         TO FORMONLY.desc3
                      ELSE
                         ERROR "Digite correctamente la Condicion del Docto."
                         NEXT FIELD FORMONLY.condic_doc3
                      END IF
                END CASE
                DISPLAY condic_doc3   TO FORMONLY.condic_doc3
                DISPLAY desc3         TO FORMONLY.desc3
             END IF 
             IF estado_doc3 = 0 THEN
                LET condic_doc3 = 0 
                LET desc3 = "PROCEDENTE       "
                DISPLAY condic_doc3   TO FORMONLY.condic_doc3
                DISPLAY desc3         TO FORMONLY.desc3
             END IF

             BEFORE FIELD FORMONLY.estado_doc4
                   LET codigo_doc4 = 7

                    LET valida_dg = 'i',n_seguro,'53207*' CLIPPED

                    CALL valida_imagen(valida_dg) RETURNING bnd_dg

                    IF bnd_dg THEN
                        LET estado_doc4 = bnd_dg
                        LET condic_doc4 = 1
                        LET desc4 = "NO DIGITALIZADO  "

                        DISPLAY estado_doc4 TO estado_doc4
                        DISPLAY condic_doc4 TO condic_doc4
                        DISPLAY desc4 TO desc4

                        EXIT INPUT
                    END IF

             AFTER FIELD FORMONLY.estado_doc4
                   CASE estado_doc4
                      WHEN 0  NEXT FIELD FORMONLY.condic_doc4
                      WHEN 1  NEXT FIELD FORMONLY.condic_doc4
                      OTHERWISE
                         ERROR "Digite cero(0) o uno(1) "
                         NEXT FIELD FORMONLY.estado_doc4
                   END CASE

             BEFORE FIELD FORMONLY.condic_doc4
                   IF estado_doc4 = 0 THEN
                      LET condic_doc4 = 0 
                      LET desc4 = "PROCEDENTE       "
                   END IF

             AFTER FIELD FORMONLY.condic_doc4
                IF estado_doc4 = 1 THEN
                   CASE condic_doc4
                    WHEN 1   LET desc4 = "NO DIGITALIZADO  "
                    WHEN 2   LET desc4 = "ILEGIBLE         "
                    WHEN 3   LET desc4 = "NO COINCIDE      "
                    OTHERWISE
                      CALL dig_condic() RETURNING vv,condic_doc4, desc4  ##dig_c
                      IF vv = 0 THEN
                         DISPLAY condic_doc4   TO FORMONLY.condic_doc4
                         DISPLAY desc4         TO FORMONLY.desc4
                      ELSE
                         ERROR "Digite correctamente la Condicion del Docto."
                         NEXT FIELD FORMONLY.condic_doc4
                      END IF
                END CASE
                DISPLAY condic_doc4   TO FORMONLY.condic_doc4
                DISPLAY desc4         TO FORMONLY.desc4
             END IF 
             IF estado_doc4 = 0 THEN
                LET condic_doc4 = 0 
                LET desc4 = "PROCEDENTE       "
                DISPLAY condic_doc4   TO FORMONLY.condic_doc4
                DISPLAY desc4         TO FORMONLY.desc4
             END IF

             BEFORE FIELD FORMONLY.estado_doc5
                    LET valida_dg = 'i',n_seguro,'532012*' CLIPPED

                    CALL valida_imagen(valida_dg) RETURNING bnd_dg

                    IF bnd_dg THEN
                        LET estado_doc5 = bnd_dg
                        LET condic_doc5 = 1
                        LET desc5 = "NO DIGITALIZADO  "

                        DISPLAY estado_doc5 TO estado_doc5
                        DISPLAY condic_doc5 TO condic_doc5
                        DISPLAY desc5 TO desc5

                        NEXT FIELD FORMONLY.estado_doc2
                    END IF

             AFTER FIELD FORMONLY.estado_doc5
                   CASE estado_doc5
                      WHEN 0  NEXT FIELD FORMONLY.condic_doc5
                      WHEN 1  NEXT FIELD FORMONLY.condic_doc5
                      OTHERWISE
                         ERROR "Digite cero(0) o uno(1) "
                         NEXT FIELD FORMONLY.estado_doc5
                   END CASE

             BEFORE FIELD FORMONLY.condic_doc5
                   IF estado_doc5 = 0 THEN
                      LET condic_doc5 = 0 
                      LET desc5 = "PROCEDENTE       "
                   END IF

             AFTER FIELD FORMONLY.condic_doc5
                IF estado_doc5 = 1 THEN
                   CASE condic_doc5
                    WHEN 1   LET desc5 = "NO DIGITALIZADO  "
                    WHEN 2   LET desc5 = "ILEGIBLE         "
                    WHEN 3   LET desc5 = "NO COINCIDE      "
                    OTHERWISE
                      CALL dig_condic() RETURNING vv,condic_doc5, desc5  ##dig_c
                      IF vv = 0 THEN
                         DISPLAY condic_doc5   TO FORMONLY.condic_doc5
                         DISPLAY desc5         TO FORMONLY.desc5
                      ELSE
                         ERROR "Digite correctamente la Condicion del Docto."
                         NEXT FIELD FORMONLY.condic_doc5
                      END IF
                END CASE
                DISPLAY condic_doc5   TO FORMONLY.condic_doc5
                DISPLAY desc5         TO FORMONLY.desc5
             END IF 
             IF estado_doc5 = 0 THEN
                LET condic_doc5 = 0 
                LET desc5 = "PROCEDENTE       "
                DISPLAY condic_doc5   TO FORMONLY.condic_doc5
                DISPLAY desc5         TO FORMONLY.desc5
             END IF
             LET band = 0
             EXIT INPUT

             ON KEY( CONTROL-C )
                LET band = 1
                PROMPT "PROCESO CANCELADO...<ENTER> PARA CONTINUAR"
                       FOR enter
                EXIT INPUT

             ON KEY( INTERRUPT )
                LET band = 1
                PROMPT "PROCESO CANCELADO...<ENTER> PARA CONTINUAR"
                       FOR enter
                EXIT INPUT

        END INPUT
        IF band = 0 THEN
                LET hora = TIME

                INSERT INTO afi_ctr_digital 
                       VALUES(n_seguro,
                              n_folio,
                              tipo_solicitud,
                              TODAY,  
                              hora,
                              codigo_doc1,
                              estado_doc1,
                              condic_doc1,
                              g_usuario)

                INSERT INTO afi_ctr_digital 
                       VALUES(n_seguro,
                              n_folio,
                              tipo_solicitud,
                              TODAY,  
                              hora,
                              codigo_doc2,
                              estado_doc2,
                              condic_doc2,
                              g_usuario)

                INSERT INTO afi_ctr_digital 
                       VALUES(n_seguro,
                              n_folio,
                              tipo_solicitud,
                              TODAY,  
                              hora,
                              codigo_doc3,
                              estado_doc3,
                              condic_doc3,
                              g_usuario)

                INSERT INTO afi_ctr_digital 
                       VALUES(n_seguro,
                              n_folio,
                              tipo_solicitud,
                              TODAY,  
                              hora,
                              codigo_doc4,
                              estado_doc4,
                              condic_doc4,
                              g_usuario)

                INSERT INTO afi_ctr_digital 
                       VALUES(n_seguro,
                              n_folio,
                              tipo_solicitud,
                              TODAY,  
                              hora,
                              codigo_doc5,
                              estado_doc5,
                              condic_doc5,
                              g_usuario)

                PROMPT "ALTA EFECTUADA...<ENTER> PARA CONTINUAR"
                       FOR enter

        END IF
    CLOSE WINDOW digital_1 
END FUNCTION

## Catalogo de Condiciones
FUNCTION dig_condic()
#dig_c

   DEFINE l_criterio   ARRAY[10] OF RECORD
              codigo   SMALLINT,          
              desc     CHAR(30)           
          END RECORD,                     

          desc         CHAR(30),          
          codigo       SMALLINT,          
          bandera      SMALLINT,          
          a_rr         SMALLINT           

   LET bandera   = 0     LET a_rr = 0   LET codigo = 0

   LET l_criterio[1].codigo = 1
   LET l_criterio[1].desc   = "NO DIGITALIZADO      "
   LET l_criterio[2].codigo = 2
   LET l_criterio[2].desc   = "ILEGIBLE             "
   LET l_criterio[3].codigo = 3
   LET l_criterio[3].desc   = "NO COINCIDE          "
   LET a_rr = 3

   CALL SET_COUNT(a_rr)
   OPEN WINDOW afim01810 AT 5,20 WITH FORM "AFIM01810" ATTRIBUTE( BORDER)
     DISPLAY " < ESC > Aceptar        < Ctrl-C > Cancelar "
             AT 1,1 ATTRIBUTE(REVERSE)
     DISPLAY "          SELECCIONA LA CONDICION           "
             AT 2,1 ATTRIBUTE(REVERSE)

     DISPLAY ARRAY l_criterio TO l_scr.*

           ON KEY(ESC)
              LET a_rr =  ARR_CURR()
              LET bandera = 0
              LET codigo  = l_criterio[a_rr].codigo
              LET desc    = l_criterio[a_rr].desc
              EXIT DISPLAY   

           ON KEY(CONTROL-C) 
              LET bandera = 1
              LET codigo  = ""
              LET desc    = ""
              EXIT DISPLAY   

           ON KEY(INTERRUPT) 
              LET bandera = 1
              LET codigo  = ""
              LET desc    = ""
              EXIT DISPLAY   
     END DISPLAY
     CLOSE WINDOW afim01810 

     RETURN bandera,codigo, desc

END FUNCTION

FUNCTION valida_digital(st_v)
#vd--------------------------

    DEFINE st_v  SMALLINT
    DEFINE st_r  SMALLINT
    DEFINE st    SMALLINT
    DEFINE i     SMALLINT
    DEFINE c     SMALLINT
    DEFINE d     SMALLINT
    DEFINE d_1   SMALLINT
    DEFINE d_2   SMALLINT
    DEFINE d_3   SMALLINT
    DEFINE d_4   SMALLINT
    DEFINE d_t   SMALLINT
    DEFINE f_max DATE
    DEFINE h_max CHAR(8)

    LET st = st_v

    FOR i = 1 TO 4
        CASE i
            WHEN 1 LET c = 1
            WHEN 2 LET c = 2
            WHEN 4 LET c = 7
            WHEN 9 LET c = 9
        END CASE

        SELECT MAX(acd.fecha_actualiza)
        INTO   f_max
        FROM   afi_ctr_digital acd
        WHERE  acd.n_seguro       = r_afi.n_seguro
        AND    acd.n_folio        = r_afi.n_folio
        AND    acd.tipo_solicitud = r_afi.tipo_solicitud
        AND    acd.codigo_doc     = c

        SELECT MAX(acd.hora_actualiza)
        INTO   h_max
        FROM   afi_ctr_digital acd
        WHERE  acd.n_seguro        = r_afi.n_seguro
        AND    acd.n_folio         = r_afi.n_folio
        AND    acd.tipo_solicitud  = r_afi.tipo_solicitud
        AND    acd.fecha_actualiza = f_max
        AND    acd.codigo_doc      = c

        SELECT estado_doc
        INTO   d
        FROM   afi_ctr_digital acd
        WHERE  acd.n_seguro        = r_afi.n_seguro
        AND    acd.n_folio         = r_afi.n_folio
        AND    acd.tipo_solicitud  = r_afi.tipo_solicitud
        AND    acd.fecha_actualiza = f_max
        AND    acd.hora_actualiza  = h_max
        AND    acd.codigo_doc      = c

        CASE i
            WHEN 1 LET d_1 = d
            WHEN 2 LET d_2 = d
            WHEN 7 LET d_3 = d
            WHEN 9 LET d_4 = d
        END CASE
    END FOR

    IF d_1 IS NULL THEN 
        LET d_1 = 1
    END IF

    IF d_2 IS NULL THEN 
        LET d_2 = 1
    END IF

    IF d_3 IS NULL THEN 
        LET d_3 = 1
    END IF

    IF d_3 IS NULL THEN 
        LET d_4 = 1
    END IF

    PROMPT "Presione [Enter] para continuar " ,d_1," ",d_2," ",d_3," ",d_4
    FOR enter

    IF NOT d_1 AND NOT d_2 AND NOT d_3 AND NOT d_4 THEN
        LET st_r = 15
    ELSE
        LET st_r = 12
    END IF

    RETURN st_r

END FUNCTION

FUNCTION carga_imagenes()
#ci----------------------

    DEFINE ejecuta CHAR(100)

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE tmp_imagen_c

        CREATE TABLE tmp_imagen_c
           (nombre_arch   CHAR(30))

        DATABASE safre_af
    WHENEVER ERROR STOP

    LET ejecuta = NULL

    LET ejecuta = "cd /archivos/imagenes/receptora/validar; ls *tif > plano " 
                  CLIPPED

    RUN ejecuta

    LET ejecuta = NULL

    LET ejecuta = "chmod 777 /archivos/imagenes/receptora/validar" CLIPPED
    RUN ejecuta

    LET ejecuta = NULL

    LET ejecuta = "/archivos/imagenes/receptora/validar/plano" CLIPPED

    LOAD FROM ejecuta INSERT INTO safre_tmp:tmp_imagen_c

END FUNCTION

FUNCTION valida_imagen(validar)
#vi----------------------------

    DEFINE validar CHAR(20)

    SELECT "X"
    FROM   safre_tmp:tmp_imagen_c
    WHERE  nombre_arch MATCHES valida_dg
    GROUP BY 1

    IF SQLCA.SQLCODE = 0 THEN
        RETURN 0
    ELSE
        RETURN 1
    END IF

END FUNCTION

FUNCTION valida_anyo_nac()
#van----------------------

    DEFINE sino SMALLINT

    CASE r_afi.tip_prob
        WHEN 1
            LET r_nac = r_afi.doc_prob[6,7]
        WHEN 2
            LET r_nac = r_afi.doc_prob[9,10]
    END CASE

    LET a_nac = xn_fena[9,10]

    IF r_nac < a_nac THEN
        WHILE TRUE
            PROMPT "Hay inconsistencias entre ",
                   "anyo reg./anyo nac., ",
                   "es correcto ¿[S/N]? "
            FOR enter

            IF enter MATCHES "[Ss]" THEN
                LET b_nac = 1
                LET sino  = 0
                EXIT WHILE
            ELSE
                IF enter NOT MATCHES "[nN]" THEN
                    ERROR "Solo debe presionar (S)i o (N)o"
                    SLEEP 3
                    ERROR ""
                ELSE
                    LET sino = 1
                    EXIT WHILE
                END IF
            END IF
        END WHILE 
    END IF

    RETURN sino

END FUNCTION

FUNCTION n_identif(vn_seguro, vn_folio, vtipo_solicitud)
#ni-----------------------------------------------------

    DEFINE
        vn_seguro       CHAR(11),
        vn_folio         DECIMAL(10,0),
        vtipo_solicitud SMALLINT

    DEFINE re_g RECORD
        clave       SMALLINT,
        descripcion CHAR(30),
        identifica  CHAR(30)
    END RECORD

    DEFINE
        clave        SMALLINT,
        descripcion  CHAR(30),
        identifica   CHAR(30),
        va           ,
        ban          SMALLINT

    LET va  = 0
    LET ban = 0
    SELECT a.clave_identif, b.des_tipo_identif, a.identifica
      INTO re_g.*
      FROM afi_ctr_identif a, tab_identificacion b
     WHERE a.n_seguro       = vn_seguro
       AND a.n_folio        = vn_folio
       AND a.tipo_solicitud = vtipo_solicitud
       AND b.tipo_identif   = a.clave_identif

    IF STATUS != NOTFOUND THEN
        LET va = 2
        LET clave       = re_g.clave
        LET descripcion = re_g.descripcion
        LET identifica  = re_g.identifica
    ELSE
        INITIALIZE re_g.* TO NULL
    END IF

    OPEN WINDOW v11 AT 6,6 WITH FORM "AFIM0019" ATTRIBUTE(BORDER)

    DISPLAY " [ Esc ] Grabar        [ Ctrl_C ] Salir                             " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY "                     I D E N T I F I C A C I O N                    " AT 2,1 ATTRIBUTE(REVERSE)

    IF va = 2 THEN
        DISPLAY BY NAME re_g.*
    END IF

    INPUT BY NAME re_g.*

    BEFORE FIELD clave
        IF ban = 0 THEN
            LET re_g.clave = clave
            LET re_g.descripcion = descripcion
            LET re_g.identifica = identifica


            DISPLAY BY NAME re_g.*
            LET ban = 1
        END IF

    AFTER FIELD clave
        IF re_g.clave IS NULL OR
           re_g.clave =  " "  THEN
            CALL clave_i() RETURNING re_g.clave,re_g.descripcion

            IF re_g.clave IS NULL OR re_g.clave = " " OR re_g.clave = 0 THEN
                NEXT FIELD clave
            END IF

            DISPLAY re_g.clave TO clave
            DISPLAY re_g.descripcion TO descripcion

            NEXT FIELD identifica
        ELSE
            SELECT b.des_tipo_identif
            INTO   re_g.descripcion
            FROM   tab_identificacion b
            WHERE  b.tipo_identif = re_g.clave

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR "Codigo Incorrecto, Verifique nuevamente"
                NEXT FIELD clave
            END IF

            DISPLAY re_g.descripcion TO descripcion
        END IF

    AFTER FIELD identifica
        IF re_g.identifica[1,1] = " " OR
                  re_g.identifica  IS NULL   THEN
                  ERROR "Digite la descripcion de la identificacion"
                  NEXT FIELD identifica
        ELSE
            SELECT "a.X"
              FROM afi_ctr_identif a
             WHERE a.n_seguro = vn_seguro
               AND a.n_folio  = vn_folio
               AND a.tipo_solicitud = vtipo_solicitud

            IF SQLCA.SQLCODE <> 0 THEN
                INSERT INTO afi_ctr_identif

                VALUES (vn_seguro,
                        vn_folio,
                        vtipo_solicitud,
                        re_g.clave,
                        re_g.identifica,
                        TODAY,
                        g_usuario)
            ELSE
                UPDATE afi_ctr_identif
                   SET afi_ctr_identif.clave_identif  = re_g.clave,
                       afi_ctr_identif.identifica     = re_g.identifica
                 WHERE afi_ctr_identif.n_seguro       = vn_seguro
                   AND afi_ctr_identif.n_folio        = vn_folio
                   AND afi_ctr_identif.tipo_solicitud = vtipo_solicitud
            END IF
        END IF

        PROMPT "ALTA EFECTUADA. [Enter] para continuar " FOR enter
        EXIT INPUT

    ON KEY(CONTROL-C)
        EXIT INPUT

    ON KEY(INTERRUPT, CONTROL - C, CONTROL - Z)
        EXIT INPUT

    END INPUT

    CLOSE WINDOW v11

END FUNCTION

FUNCTION clave_i()
#ci---------------

    DEFINE c ARRAY[50] OF RECORD
        codigo      SMALLINT,
        descripcion CHAR(50)
    END RECORD

    DEFINE i SMALLINT

    DECLARE cursor_cve_ii CURSOR FOR
        SELECT *
        FROM   tab_identificacion
        ORDER BY 1

        LET i = 1

        FOREACH cursor_cve_ii INTO c[i].*
            LET i = i + 1
        END FOREACH

        CALL SET_COUNT(i-1)

    OPEN WINDOW ven_clave AT 9,10 WITH FORM "AFIM00191" ATTRIBUTE(BORDER)

    DISPLAY "               CATALOGO DE IDENTIFICACIONES             " AT 3,1 ATTRIBUTE ( REVERSE )

    DISPLAY ARRAY c TO scr_1.*

    ON KEY ( INTERRUPT, CONTROL - C, CONTROL - Z )
        LET i = ARR_CURR()

    ON KEY ( CONTROL-M )
        LET i = ARR_CURR()
        EXIT DISPLAY


    END DISPLAY

    CLOSE WINDOW ven_clave

    RETURN c[i].codigo, c[i].descripcion

END FUNCTION

