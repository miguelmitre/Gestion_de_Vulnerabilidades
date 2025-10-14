###############################################################################
#Proyecto          => SAFRE ( MEXICO )                                        #
#Propietario       => E.F.P.                                                  # #Programa SEPL005  => CONSULTA DE SOLICITUDES DE SEPARACIÓN DE CUENTAS        #
#Sistema           => SEP                                                     #
#Fecha             => 20 DE FEBRERO  2004                                     #
#Modificado        => JOSÉ FRANCISCO LUGO CORNEJO                             #
############################################################################### 
DATABASE safre_af

GLOBALS
    DEFINE regresa_ejecuta SMALLINT
    DEFINE ejecuta      CHAR(1000)
    DEFINE
        su_estatus       SMALLINT,
        xx               SMALLINT,
        yy               SMALLINT,
        sw_1             SMALLINT,
        digito           SMALLINT,
        status_interno   SMALLINT,
        super            SMALLINT,
        g_status_captura SMALLINT,
        hay_datos1       SMALLINT,
        hay_datos2       SMALLINT,
        tot_sief         SMALLINT,
        KEY              INTEGER

    DEFINE
        x_num_folio      INTEGER,
        vcont            INTEGER,
        gbene_rowid      INTEGER,
        gsiefo_rowid     INTEGER 

    DEFINE
        vnss             DECIMAL(11,0),
        fol_prob         DECIMAL(10,0),
        doc_prob         DECIMAL(16,0),
        banr             ,
        bant             ,
        found_flag       SMALLINT

    DEFINE
        sexo_cur        CHAR(1)   ,
        c_pat           CHAR(1)   ,
        c_mat           CHAR(1)   ,
        c_nom           CHAR(1)   ,
        c_fen           CHAR(1)   ,
        c_doc           CHAR(1)   ,
        enter           CHAR(1)   ,
        g_enter           CHAR(1)   ,
        aux_pausa       CHAR(1)   ,
        ACCION          CHAR(1)   ,
        cod_err         CHAR(4)   ,
        g_usuario       CHAR(8)   ,
        g_hora          CHAR(8)   ,
        x_fecha         CHAR(10)  ,
        xn_fena         CHAR(10)  ,
        desc_solic      CHAR(15)  ,
        operacion       CHAR(40)  ,
        pat             CHAR(40)  ,
        mat             CHAR(40)  ,
        nom             CHAR(40)  ,
        COMMA           CHAR(200) ,
        comando         CHAR(250) 

    DEFINE
        contesta        CHAR(1)   ,
        nacim           CHAR(1)   ,
        anyo_nac        CHAR(2)   ,
        xx_fecha        CHAR(10)  ,
        param           CHAR(193) 

    DEFINE
        hoy             DATE,
        x_fecha_cambio  DATE,
        x_finicta       DATE,
        x_asigna        DATE,
        xn_fecha        DATE


    DEFINE r_afi RECORD
        tipo_solicitud      SMALLINT      ,
        desc_solicitud      CHAR(30)      ,
        n_folio             INTEGER       ,
        paterno             CHAR(40)      ,
        materno             CHAR(40)      ,
        nombres             CHAR(40)      ,
        fena                DATE          ,
        sexo                SMALLINT      ,
        desc_sexo           CHAR(60)      ,
        n_unico             CHAR(18)      ,
        n_seguro            CHAR(11)      ,
        n_rfc               CHAR(13)      ,
        estadon             SMALLINT      ,
        desc_estadon        CHAR(60)      ,
        nacionalidad        CHAR(3)       ,
        desc_nacionalidad   CHAR(60)      ,
        ind_infonavit       CHAR(1)       ,
        desc_ind_info       CHAR(25)      ,
        frecafor            DATE          ,
        cod_afore_ced       SMALLINT      ,
        desc_afore          CHAR(50)      ,
        tip_prob            CHAR(1)       ,
        docprob_desc        CHAR(20)      
    END RECORD
    DEFINE g_afore RECORD LIKE tab_afore.*
    DEFINE r_cod   RECORD LIKE tab_supervisor.*

    DEFINE g_comision RECORD
        cod_esq_comision  SMALLINT,
        desc_esq_comision CHAR(50)
    END RECORD

    DEFINE
        dcto_2 CHAR(1),
        dcto_3 CHAR(9),
        dcto_4 CHAR(7),
        dcto_7 CHAR(7)

    DEFINE
        a_nac    SMALLINT,
        r_nac    SMALLINT,
        b_nac    SMALLINT

    DEFINE
        v_an       SMALLINT,
        no_identif SMALLINT,
        n_iden_d   CHAR(30)

    DEFINE vsalario_actual     LIKE afi_solicitud.salario_actual
    DEFINE vcod_promotor       CHAR(10)
    DEFINE vcodven             CHAR(10)

    {DEFINE g_comision RECORD
        cod_esq_comision  SMALLINT,
        desc_esq_comision CHAR(50),
        indicador_b       CHAR(15),
        indicador_c       CHAR(15),
        indicador_d       CHAR(15),
        indicador_e       CHAR(15)
    END RECORD}

    DEFINE st_int           SMALLINT
    DEFINE val_pat          SMALLINT
    DEFINE v_1              SMALLINT
    DEFINE val_1            CHAR(80)
    DEFINE dig_curp         SMALLINT
    DEFINE pasa             SMALLINT
    DEFINE pasa_curp        SMALLINT
    DEFINE desc_err         CHAR(60)

    DEFINE llama_val        SMALLINT
    DEFINE vafore_f         SMALLINT
    DEFINE tipo_solic       SMALLINT

    DEFINE vnss_ant         CHAR(11)
    DEFINE bnd_fena         SMALLINT
    DEFINE ver_dig          SMALLINT

    DEFINE vg_cap2          CHAR(50)
    DEFINE gr_afictractividad RECORD LIKE afi_ctr_actividad.*

    DEFINE bnd_comma        SMALLINT

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY CONTROL-I,
        COMMENT LINE LAST

    CALL STARTLOG("SEPL005.log")
    CALL inicio()               #i
    CALL crea_tablas()          #ct
    CALL Proceso_principal()    #pp

END MAIN

FUNCTION inicio()
#i---------------

    LET llama_val  = ARG_VAL(1)
    LET tipo_solic = ARG_VAL(2)

    IF llama_val IS NULL THEN
        LET llama_val = 0
    END IF

    LET banr      = FALSE
    LET bant      = FALSE
    LET bnd_comma = FALSE

    LET hoy  = TODAY

    SELECT *, USER 
    INTO   g_afore.*, g_usuario 
    FROM   tab_afore
    WHERE  marca = 1

    CREATE TEMP TABLE tmp_mot_rch
       (codigo      SMALLINT,
        descripcion CHAR(80),
        cve_afore   SMALLINT,
        desc_afore  CHAR(14),
        fech_rech   DATE    ,
        n_pcanase   CHAR(50))

END FUNCTION

FUNCTION crea_tablas()
#ct-------------------

    CREATE TEMP TABLE temp_excepciones
        (campo CHAR(78))

END FUNCTION

FUNCTION Proceso_principal()
#pp-------------------------

      OPEN WINDOW ventana_menu AT 2,2 WITH 20 ROWS, 78 COLUMNS ATTRIBUTE(BORDER)
      DISPLAY " SEPL005           CONSULTA SOLICITUDES DE SEPARACIÓN DE CUENTAS         " AT  3,1 ATTRIBUTE(REVERSE)
      DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

    IF NOT llama_val THEN
        MENU "Sol reg"
         COMMAND KEY("O") "sOli Edo 2 a 21 " "Solicitudes en Estado Mayor a Cero"
               CALL despliega_nss("O",1)    #    SEPF005.4gl
         COMMAND KEY("T") "soli Edo 0 y 1" "Solicitudes Capturadas Estado Cero(0) y 21"
               CALL despliega_nss("T",0)    #    SEPF005.4gl

         COMMAND KEY("E") "Pend rEgistro" "Solicitud Pendientes de OP-33 Afiliacion x Registro"
               CALL despliega_nss("A",6)    #    SEPF005.4gl

         COMMAND KEY("L") "Pend traspaso" "Solicitud Pendientes de OP-34 Afiliacion x Traspaso"
               CALL despliega_nss("A",7)    #    SEPF005.4gl
         COMMAND KEY("R") "consulta registRo" "Consulta Solicitud en OP-33 Afiliacion x Registro"
               CALL despliega_nss("C",6)    #    SEPF005.4gl
         COMMAND KEY("P") "consulta trasPaso" "Consulta Solicitud en OP-34 Afiliacion x Traspaso"
               CALL despliega_nss("C",7)    #    SEPF005.4gl
            COMMAND "Salir" "Salir del Programa"
                EXIT MENU
            END MENU
    END IF
END FUNCTION

FUNCTION Ingresa_autoriza()
#ia------------------------

    DEFINE desc       CHAR(60)
    DEFINE x_buscar   CHAR(60)
    DEFINE x_texto    CHAR(200)
    DEFINE pos        SMALLINT
    DEFINE cod        DECIMAL(10,0)

    DEFINE l_reg ARRAY[1000] OF RECORD
        codigo      CHAR(10),
        descripcion CHAR(50)
    END RECORD

    LET aux_pausa = "N"

    OPEN WINDOW ventanilla_super AT 8,4 WITH FORM "SEPM0015" ATTRIBUTE(BORDER)
    DISPLAY "                      OPCION RESERVADA PARA SUPERVISORES                " AT 3,1 ATTRIBUTE ( REVERSE)

    INPUT BY NAME r_cod.super_cod,r_cod.super_desc,r_cod.nip
        AFTER FIELD super_cod
            IF r_cod.super_cod IS NULL THEN
                CALL Despliega_supervisores()
                RETURNING r_cod.super_cod, r_cod.super_desc
            END IF

            SELECT super_cod,
                   super_desc
            INTO   r_cod.area_cod,
                   r_cod.super_desc
            FROM   tab_supervisor 
            WHERE  super_cod = r_cod.super_cod

            IF STATUS = NOTFOUND THEN
                ERROR "CLAVE DE SUPERVISOR INVALIDA"
                SLEEP 2
                NEXT FIELD super_cod
            END IF

        AFTER FIELD nip
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD super_cod 
            END IF

            IF r_cod.nip IS NULL THEN
                LET aux_pausa = "S"
                ERROR "NIP NO PUEDE SER NULO"
                NEXT FIELD nip
            END IF

            SELECT "X" 
            FROM   tab_supervisor
            WHERE  nip = r_cod.nip

            IF STATUS = NOTFOUND THEN
                ERROR "NIP INCORRECTO, PERMISO DENEGADO"
                LET aux_pausa = "S"
                NEXT FIELD nip
            ELSE
                LET aux_pausa = "N"
                EXIT INPUT
            END IF

        ON KEY ( INTERRUPT, CONTROL-C, CONTROL-Z )
            LET aux_pausa = "S"
            EXIT INPUT

    END INPUT

    CLOSE WINDOW ventanilla_super

END FUNCTION

FUNCTION Inicializa()
#in------------------

    INITIALIZE r_afi.* TO NULL
    INITIALIZE g_comision.* TO NULL

    DISPLAY "                                                                               " AT  5,1 ATTRIBUTE(REVERSE)
    CLEAR FORM

END FUNCTION

FUNCTION Agrega(l_nss,l_tipo_sol,l_cve_ced)
#a----------------------------------------
    DEFINE l_cve_ced        CHAR(003)
    DEFINE l_nss            CHAR(011),
	   l_tipo_sol       smallint

    DEFINE recha            SMALLINT
    DEFINE recha_st         SMALLINT
    DEFINE l_nivel_prom     SMALLINT
    DEFINE dia_paso         SMALLINT
    DEFINE X_DAT            DECIMAL(10,0)
    DEFINE x_tip            DECIMAL(10,0)
    DEFINE fecha_comprueba  DATE
    DEFINE j_fecha          DATE
    DEFINE opc              CHAR(1)
    DEFINE vcontrol         CHAR(1)
    DEFINE aa,mm,dd         CHAR(2)
    DEFINE z_fecha          CHAR(10)
    DEFINE ll_calle         CHAR(30)
    DEFINE ll_num           LIKE afi_domicilio.numero
    DEFINE ll_cpos          LIKE afi_domicilio.codpos
    DEFINE r_nss            CHAR(11)
    DEFINE r_cta            SMALLINT
    DEFINE r_pro            SMALLINT

    ---DEFINE reg_preafili RECORD LIKE afore_xxi:afi_preafili.*
    --DEFINE reg_preafili RECORD LIKE safre_tmp:afi_preafili.*

    DEFINE
        sexo_cur        CHAR(1),
        aaa             CHAR(2),
        e_fecha         CHAR(10),
        codprom         CHAR(10),
        val_1           CHAR(80),
        doc_prob_arma   CHAR(16),
        rfc_arma        CHAR(10)

    DEFINE
        d_fecha         DATE

    DEFINE
        x_pro           DECIMAL(10,0),
        x_reg           CHAR(10)

    DEFINE
        estado_cuenta   SMALLINT,
        cod_origen      SMALLINT,
        v_1             SMALLINT,
        a_yo_act        SMALLINT,
        a_yo_fena       SMALLINT,
        a_yo            SMALLINT,
        bla             SMALLINT,
        ban             SMALLINT,
        sino            SMALLINT,
        fecha_curp      CHAR(10),
        estado_cur      CHAR(02),
        cve_edo_cur     SMALLINT,
        bnd_fena        SMALLINT,
        bnd_vto         SMALLINT


    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "SEPM0011" ATTRIBUTE(BORDER)

    DISPLAY " SEPL005           CONSULTA SOLICITUDES AFILIACION SEP                    " AT  3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

    DISPLAY "                                                                               " AT  5,1 ATTRIBUTE(REVERSE)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE)

    DISPLAY "  CTRL:[C ] Salir sin Grabar   [W] Observaciones" AT 1,1
    DISPLAY "CTRL:[E]Dom. [V]Patrones [Y] Ident" AT 2,1 --ATTRIBUTE(CYAN)

    CALL Inicializa()


    LET r_afi.n_seguro       = l_nss
    LET r_afi.cod_afore_ced  = l_cve_ced
    LET r_afi.tipo_solicitud  = 6

    SELECT a.afore_desc
    INTO   r_afi.desc_afore
    FROM   tab_afore a
    WHERE  a.afore_cod = r_afi.cod_afore_ced

    LET r_afi.tipo_solicitud = l_tipo_sol

    LET sw_1       = 0
    LET vcont      = 0
    LET vcontrol   = ""

    LET sexo_cur   = " "
    LET cod_origen = 0
    INITIALIZE doc_prob_arma TO NULL

    INPUT BY NAME r_afi.* WITHOUT DEFAULTS
        AFTER FIELD tipo_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD tipo_solicitud
            END IF

            IF r_afi.tipo_solicitud IS NULL THEN
                ERROR "Tipo de Solicitud NO puede ser NULO"
                NEXT FIELD tipo_solicitud
            END IF

            IF r_afi.tipo_solicitud < 6  OR
               r_afi.tipo_solicitud > 7 THEN
                ERROR "Tipo de Solicitud erroneo, ingrese tipo correcto"
                NEXT FIELD tipo_solicitud
            END IF

            SELECT ts.desc_solicitud
            INTO   r_afi.desc_solicitud
            FROM   tab_tipo_solic ts
            WHERE  ts.tipo_solicitud = r_afi.tipo_solicitud

            IF SQLCA.SQLCODE = 0 THEN
                DISPLAY BY NAME r_afi.desc_solicitud
            ELSE
                ERROR "Tipo Solic.: 6=>Registro, 7=>Traspaso"
                LET r_afi.tipo_solicitud = NULL

                DISPLAY BY NAME r_afi.tipo_solicitud
                NEXT FIELD tipo_solicitud
            END IF

        BEFORE FIELD n_folio
            IF vcontrol = "S" THEN
                NEXT FIELD paterno
            END IF

        AFTER FIELD n_folio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD tipo_solicitud
            END IF

            IF r_afi.n_folio IS NULL THEN
                ERROR "Campo Folio de la Solicitud NO puede ser NULO"
                NEXT FIELD n_folio
            ELSE
                SELECT "X" 
                FROM   afi_mae_afiliado 
                WHERE  n_folio        = r_afi.n_folio
                AND    tipo_solicitud = r_afi.tipo_solicitud
                GROUP BY 1

                IF SQLCA.SQLCODE = 0 THEN
                    PROMPT "Folio ya Existe en Maestro de Afiliados, [ENTER] p/salir"
                    FOR enter
                    NEXT FIELD n_folio 
                END IF

                SELECT "X" 
                FROM   afi_solicitud
                WHERE  n_folio        = r_afi.n_folio
                AND    tipo_solicitud = r_afi.tipo_solicitud
                GROUP BY 1

                IF SQLCA.SQLCODE = 0 THEN
                    PROMPT "Folio ya Existe en solicitudes de afiliacion, [Enter] p/salir"
                    FOR enter
                    NEXT FIELD n_folio 
                    RETURN
                END IF
            END IF

        BEFORE FIELD paterno
            LET val_pat = 0

        AFTER FIELD paterno
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                CALL valida_paterno()

                IF val_pat = 1 THEN
                    LET val_pat = 0
                    NEXT FIELD paterno
                END IF

                NEXT FIELD n_folio
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                CALL valida_paterno() 

                IF val_pat = 1 THEN
                    LET val_pat = 0
                    NEXT FIELD paterno
                END IF

                NEXT FIELD n_folio
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                CALL valida_paterno() 

                IF val_pat = 1 THEN
                    LET val_pat = 0
                    NEXT FIELD paterno
                END IF

                NEXT FIELD materno
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("RIGTH") THEN
                CALL valida_paterno() 

                IF val_pat = 1 THEN
                    LET val_pat = 0
                   NEXT FIELD paterno
               END IF

               NEXT FIELD materno
           END IF

           IF r_afi.paterno IS NULL  OR
              r_afi.paterno[1] = " " THEN
               ERROR "Campo Apellido Paterno NO puede ser NULO"
               NEXT FIELD paterno
           END IF

           LET v_1 = 0

           INITIALIZE val_1 TO NULL
           CALL verifica_nombre(r_afi.paterno) RETURNING v_1, val_1

           IF v_1 = 1 THEN
               ERROR "A. Paterno ", val_1 CLIPPED
               NEXT FIELD paterno 
           END IF

        AFTER FIELD materno
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD paterno
            END IF

            IF r_afi.materno[1] = " " THEN
                ERROR "Ingrese Apellido Materno correcto o deje campo nulo (teclee ctrl-d)"
                LET r_afi.materno = NULL
                DISPLAY BY NAME r_afi.materno
                NEXT FIELD materno
            END IF

            LET v_1 = 0

            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.materno) RETURNING v_1, val_1

            IF v_1 = 1 THEN
                ERROR "A. Materno ", val_1 CLIPPED
                NEXT FIELD materno
            END IF

        BEFORE FIELD nombres

        AFTER FIELD nombres
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD materno
            END IF

            IF r_afi.nombres IS NULL OR
               r_afi.nombres = " "   THEN
                ERROR "Campo nombre NO puede ser NULO"
                NEXT FIELD nombres
            END IF

            LET v_1 = 0

            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.nombres) RETURNING v_1, val_1

            IF v_1 = 1 THEN
                ERROR "EL Nombre ", val_1 CLIPPED
                NEXT FIELD nombres 
            END IF

            IF r_afi.nombres IS NULL  OR
               r_afi.nombres[1] = " " THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD nombres
            END IF

            NEXT FIELD fena

        BEFORE FIELD fena
            INITIALIZE x_fecha, xn_fecha, xn_fena TO NULL

            IF (r_afi.n_unico IS NOT NULL AND 
                r_afi.n_unico <> " ") THEN
                LET x_fecha = r_afi.n_unico[7,8], "/",
                              r_afi.n_unico[9,10],"/",
                              "19",
                              r_afi.n_unico[5,6]

                LET bnd_fena = 1
            ELSE
                IF (r_afi.n_rfc IS NOT NULL AND 
                    r_afi.n_rfc <> " ") THEN
                    LET x_fecha = r_afi.n_rfc[7,8], "/",
                                  r_afi.n_rfc[9,10],"/",
                                  "19",
                                  r_afi.n_rfc[5,6]

                    LET bnd_fena = 2
                ELSE
                    LET bnd_fena = 0
                END IF
            END IF

            IF bnd_fena THEN
                LET xn_fecha   = x_fecha
                LET xn_fena    = x_fecha
                LET r_afi.fena = x_fecha

                DISPLAY BY NAME r_afi.fena
            END IF

        AFTER FIELD fena
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD nombres
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                NEXT FIELD nacionalidad
            END IF

            IF r_afi.fena IS NULL THEN
                ERROR "Campo fecha de nacimiento NO puede ser NULO"
                NEXT FIELD fena
            END IF

            LET a_yo_act  = 0
            LET a_yo_fena = 0
            LET a_yo      = 0

            LET a_yo_act  = YEAR(TODAY)      USING "&&&&"
            LET a_yo_fena = YEAR(r_afi.fena) USING "&&&&"
            LET a_yo      = a_yo_act - a_yo_fena

            IF a_yo > 120 THEN
                ERROR "Esta persona pasa del rengo de 120 años, verifique nuevamente"
                NEXT FIELD fena
            END IF

            DISPLAY BY NAME r_afi.fena

            NEXT FIELD sexo

        BEFORE FIELD sexo

        AFTER FIELD sexo
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD nombres
            END IF

            IF r_afi.sexo IS NULL OR
               r_afi.sexo = " "   OR
               r_afi.sexo = 0     THEN
                CALL Despliega_sexos()
                     RETURNING r_afi.sexo, r_afi.desc_sexo
                IF r_afi.sexo IS NULL OR
                   r_afi.sexo = " "   OR
                   r_afi.sexo = 0     THEN
                    NEXT FIELD sexo
                END IF
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
            NEXT FIELD n_unico

        BEFORE FIELD n_unico
            LET ver_dig = ''

        AFTER FIELD n_unico
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD nombres
            END IF

            IF LENGTH(r_afi.n_unico) > 0  AND
               LENGTH(r_afi.n_unico) < 18 THEN
                ERROR "Debe ingresar CURP completa"
                NEXT FIELD n_unico
            ELSE
                IF r_afi.n_unico[1] <> " "   OR
                   r_afi.n_unico IS NOT NULL THEN
                    IF r_afi.n_unico[11] = "H" THEN
                        LET sexo_cur = "1"
                    ELSE
                        LET sexo_cur = "2"
                    END IF

                    CALL valida_est_curp(r_afi.n_unico)
                         RETURNING pasa_curp,desc_err

                    IF pasa_curp = 1 THEN
                        ERROR "", desc_err
                        LET pasa_curp = 0
                        NEXT FIELD n_unico
                    END IF

                    CALL var_dig_curp(r_afi.n_unico)
                         RETURNING pasa, dig_curp

                        LET ver_dig = r_afi.n_unico[18]

                        IF ver_dig <> dig_curp THEN
                            ERROR "Digito Verificador Invalido curp, el digito es : ",
                              dig_curp
                        END IF
                ELSE
                    LET sexo_cur = " "
                END IF
            END IF

            IF r_afi.n_unico[1] = " " THEN
                ERROR "Debe ingresar CURP correcta"
                NEXT FIELD n_unico
            END IF

            IF r_afi.n_unico IS NOT NULL OR
               r_afi.n_unico = ' '       THEN
                LET fecha_curp = r_afi.n_unico[07,08], "/",
                                 r_afi.n_unico[09,10], "/", 
                                 "19", r_afi.n_unico[05,06] CLIPPED

                IF r_afi.fena <> fecha_curp THEN
                    WHILE TRUE
                        PROMPT "Hay Inconsistencias F.Nac./F.Nac. de curp, es correcto [S/N] ? "
                        FOR enter

                        IF enter MATCHES "[sSnN]" THEN
                            IF enter MATCHES "[sS]" THEN
                                NEXT FIELD n_seguro
                                EXIT WHILE
                            ELSE
                                NEXT FIELD fena
                            END IF
                        END IF
                    END WHILE
                    ERROR ""
                ELSE
                    NEXT FIELD n_seguro
                END IF
             END IF

            IF r_afi.n_unico IS NOT NULL OR
               r_afi.n_unico[1] <> ' ' THEN
                IF r_afi.n_unico[11] = "H" THEN
                    LET sexo_cur = "1"
                ELSE
                    LET sexo_cur = "2"
                END IF

                IF sexo_cur <> r_afi.sexo THEN
                    WHILE TRUE
                        PROMPT "Hay Inconsistencias sexo/sexo de curp, es correcto [S/N] ? "
                        FOR enter

                        IF enter MATCHES "[sSnN]" THEN
                            IF enter MATCHES "[sS]" THEN
                                NEXT FIELD n_seguro
                                EXIT WHILE
                            ELSE
                                NEXT FIELD sexo
                            END IF
                        END IF
                    END WHILE
                    ERROR ""
                END IF
            END IF

            NEXT FIELD n_seguro

        BEFORE FIELD n_seguro
            IF vcontrol='S' THEN
                NEXT FIELD n_rfc
            END IF

        AFTER FIELD n_seguro
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                IF tipo_solic = 10 THEN
                    NEXT FIELD nombres
                ELSE
                    NEXT FIELD n_seguro
                END IF
            END IF

            IF r_afi.n_seguro IS NULL THEN
                ERROR "Campo Numero de Seguro Social NO puede ser NULO"
                NEXT FIELD n_seguro
            END IF

            IF LENGTH(r_afi.n_seguro) <> 11 THEN
                ERROR "Debe ingresar N.S.S. completo"
                NEXT FIELD n_seguro
            END IF

           --- Validación para verificar que no cambien nss del precapturado

            IF r_afi.n_seguro <> vnss_ant THEN
                ERROR "NSS no se puede cambiar del numero precapturado"
                SLEEP 3
                ERROR ""

                LET r_afi.n_seguro = vnss_ant
                DISPLAY BY NAME r_afi.n_seguro
                NEXT FIELD n_seguro
            END IF

            SELECT "X"
            FROM   afi_mae_afiliado
            WHERE  n_seguro = r_afi.n_seguro
            AND    tipo_solicitud <> 5

            IF SQLCA.SQLCODE = 0  THEN
                SELECT 'X'
                FROM   cta_act_marca a
                WHERE  a.nss = r_afi.n_seguro
                AND    a.marca_cod IN(SELECT b.marca_resulta
                                      FROM   tab_marca b
                                      WHERE  b.ind_habilita = 1)

                IF SQLCA.SQLCODE <> 0  THEN
                    ERROR "N.S.S. ya Ingresado en Maestro de Afiliados"
                    EXIT INPUT
                END IF
            END IF

            SELECT "X"
            FROM   afi_solicitud
            WHERE  n_seguro       = r_afi.n_seguro
            AND    n_folio        = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud

            IF STATUS <> NOTFOUND THEN
                ERROR "NSS ya Ingresado en solicitudes de afiliacion"
                NEXT FIELD n_seguro
            END IF

            SELECT COUNT(*)
            INTO   xx
            FROM   afi_solicitud
            WHERE  n_seguro = r_afi.n_seguro

            IF xx <> 0 THEN
                CALL despliega_ing()
                WHILE TRUE
                    PROMPT "DESEA SEGUIR LA CAPTURA S/N ? " FOR aux_pausa

                    IF aux_pausa  MATCHES "[SsNn]" THEN
                        IF aux_pausa MATCHES "[Nn]" THEN
			    CLOSE WINDOW ventana_1
                            RETURN
                        ELSE
                            EXIT WHILE
                        END IF
                    ELSE
                        DISPLAY "Solo debe presionar (S)i o (N)o" AT 18,2
                    END IF
                END WHILE
            END IF

            CALL digito_verif(r_afi.n_seguro[1,10],10) RETURNING digito

            IF digito = 32000 THEN 
                ERROR "N.S.S. solo contiene digitos" 
                NEXT FIELD n_seguro 
            END IF

            IF LENGTH(r_afi.n_seguro) = 11 AND
               digito <> r_afi.n_seguro[11] THEN
                ERROR "Digito Verificador Invalido, el digito debe ser: ",digito
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

            SELECT a.paterno,
                   a.materno,
                   a.nombres,
                   a.estadon,
                   a.sexo,
                   a.fentcons
              INTO r_afi.paterno,
                   r_afi.materno,
                   r_afi.nombres,
                   r_afi.estadon,
                   r_afi.sexo,
                   x_asigna
              FROM afi_mae_afiliado a
             WHERE a.n_seguro = r_afi.n_seguro
               AND a.tipo_solicitud = 5

            IF STATUS <>  NOTFOUND THEN
                DISPLAY BY NAME r_afi.n_seguro,
                                r_afi.paterno,
                                r_afi.materno,
                                r_afi.nombres,
                                r_afi.estadon,
                                r_afi.sexo

                DISPLAY "ASIG MISMA AFO" AT 13,14

                DISPLAY "F.Asigna. ",x_asigna USING "dd-mm-yyyy"
                AT 14,2 ATTRIBUTE(REVERSE)
            END IF

            NEXT FIELD n_rfc

        BEFORE FIELD n_rfc
            IF r_afi.n_seguro IS NULL OR
               r_afi.n_seguro = ' '   THEN
                ERROR "Campo Numero de Seguro Social NO puede ser NULO"
                NEXT FIELD n_seguro
            END IF

        AFTER FIELD n_rfc
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                LET v_1 = 0
                INITIALIZE val_1 TO NULL

                CALL verifica_rfc(r_afi.n_rfc[1,4]) RETURNING v_1, val_1
                IF v_1 = 1 THEN
                    ERROR "R.F.C. ", val_1 CLIPPED
                    NEXT FIELD n_rfc
                END IF

                NEXT FIELD fena
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                LET v_1 = 0
                INITIALIZE val_1 TO NULL

                CALL verifica_rfc(r_afi.n_rfc[1,4]) RETURNING v_1, val_1
                IF v_1 = 1 THEN
                    ERROR "R.F.C. ", val_1 CLIPPED
                    NEXT FIELD n_rfc
                END IF

                NEXT FIELD cod_esq_comision
            END IF

            IF r_afi.n_rfc IS NULL THEN
                ERROR "Campo R.F.C. NO puede ser NULO"
                NEXT FIELD n_rfc
            END IF

            IF LENGTH(r_afi.n_rfc) <> 10 AND
               LENGTH(r_afi.n_rfc) <> 13 THEN
                ERROR "Debe ingresar R.F.C. completo"
                NEXT FIELD n_rfc
            END IF

            IF NOT valida_fecha_rfc(r_afi.n_rfc[5,10]) THEN
                ERROR "Formato de RFC Incorrecto ( ----AAMMDD*** )"
                NEXT FIELD n_rfc
            ELSE
                WHENEVER ERROR CONTINUE
                LET aa = r_afi.n_rfc[5,6]
                LET mm = r_afi.n_rfc[7,8]
                LET dd = r_afi.n_rfc[9,10]
                LET z_fecha = mm,"/",dd,"/19",aa
                LET j_fecha = z_fecha

                IF r_afi.fena <> j_fecha THEN
                    WHILE TRUE
                    PROMPT
                    "Hay Inconsistencias RFC/Fecha nacim., es correcto [S/N] ? "
                    FOR enter

                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[sS]" THEN
                            NEXT FIELD estadon
                            EXIT WHILE
                        ELSE
                            NEXT FIELD fena
                        END IF
                    END IF

                    END WHILE
                END IF
                WHENEVER ERROR STOP

                IF j_fecha IS NULL THEN
                    ERROR "fecha Invalida en RFC"
                    NEXT FIELD n_rfc
                END IF
            END IF

            NEXT FIELD estadon

        AFTER FIELD estadon
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD sexo
            END IF

            IF r_afi.estadon IS NULL OR
               r_afi.estadon = 0     THEN
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

            CALL arma_clave_rfc(r_afi.paterno,
                                r_afi.materno,
                                r_afi.nombres,
                                r_afi.fena) RETURNING rfc_arma #rac

            IF rfc_arma != r_afi.n_rfc[1,10] THEN
                WHILE TRUE
                PROMPT "Hay Inconsistencias en el RFC, ¿es correcto [S/N]? "
                FOR enter

                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        NEXT FIELD nacionalidad
                        EXIT WHILE
                    ELSE
                        NEXT FIELD fena
                    END IF
                END IF

                END WHILE
            END IF

            IF r_afi.n_unico IS NOT NULL OR
               r_afi.n_unico[1] <> ' ' THEN
                LET estado_cur = r_afi.n_unico[12,13] 

                SELECT @estad_cod
                  INTO cve_edo_cur
                  FROM tab_edo_norma
                 WHERE @estad_pro = estado_cur

                IF cve_edo_cur <> r_afi.estadon THEN
                    WHILE TRUE
                    PROMPT "Hay Inconsistencias en CURP/Ent. nac., ¿es correcto [S/N]? "
                    FOR enter

                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[sS]" THEN
                            NEXT FIELD nacionalidad
                            EXIT WHILE
                        ELSE
                            NEXT FIELD estadon
                        END IF
                    END IF

                    END WHILE
                END IF
            END IF

            DISPLAY BY NAME r_afi.estadon,r_afi.desc_estadon

            NEXT FIELD nacionalidad

        BEFORE FIELD nacionalidad
            IF r_afi.nacionalidad IS NULL THEN
                LET r_afi.nacionalidad = "MEX"
                DISPLAY BY NAME r_afi.nacionalidad
            END IF

        AFTER FIELD nacionalidad
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD estadon
            END IF

            IF r_afi.nacionalidad IS NULL OR
               r_afi.nacionalidad = " "   THEN
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

            NEXT FIELD ind_infonavit

        BEFORE FIELD ind_infonavit
            LET r_afi.ind_infonavit = "0"

            SELECT @descripcion
              INTO r_afi.desc_ind_info
              FROM safre_af:tab_ind_cred
             WHERE @codigo = r_afi.ind_infonavit

            DISPLAY BY NAME r_afi.ind_infonavit, r_afi.desc_ind_info

        AFTER FIELD ind_infonavit
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD profesion_cod
            END IF

            IF r_afi.ind_infonavit IS NULL OR
               r_afi.ind_infonavit = " "   THEN
                CALL Despliega_ind_info() RETURNING r_afi.ind_infonavit,
                                                    r_afi.desc_ind_info

                IF r_afi.ind_infonavit IS NULL OR
                   r_afi.ind_infonavit = " "   THEN
                    ERROR "Se requiere el indicativo de credito de INFONAVIT"
                    NEXT FIELD ind_infonavit
                END IF
            END IF

            IF r_afi.ind_infonavit NOT MATCHES "[012]" THEN
                CALL Despliega_ind_info() RETURNING r_afi.ind_infonavit,
                                                    r_afi.desc_ind_info

                IF r_afi.ind_infonavit NOT MATCHES "[012]" THEN
                    ERROR "0) Sin credito, 1) Credito Infonavit, 2) Credito 43 bis"
                    NEXT FIELD ind_infonavit
                END IF
            ELSE
                SELECT @descripcion
                  INTO r_afi.desc_ind_info
                  FROM safre_af:tab_ind_cred
                 WHERE @codigo = r_afi.ind_infonavit

                DISPLAY BY NAME r_afi.desc_ind_info
            END IF

            LET r_afi.frecafor = TODAY
            DISPLAY BY NAME r_afi.frecafor

        AFTER FIELD cod_afore_ced
            IF r_afi.tipo_solicitud = 6 THEN 
	       CONTINUE INPUT 
	    END IF

            IF r_afi.cod_afore_ced IS NULL THEN
                ERROR "Codigo AFORE Cedente NO puede ser NULO"
                NEXT FIELD cod_afore_ced
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_emision
            END IF

            IF r_afi.cod_afore_ced IS NULL THEN
                CALL Despliega_afores()
                     RETURNING r_afi.cod_afore_ced, r_afi.desc_afore

                IF r_afi.cod_afore_ced = g_afore.afore_cod THEN
                     ERROR "Clave de Afore erronea"
                     LET r_afi.cod_afore_ced = ''
                     DISPLAY BY NAME r_afi.cod_afore_ced
                     NEXT FIELD cod_afore_ced
                END IF
            ELSE
                SELECT @afore_desc, @afore_fusion
                INTO   r_afi.desc_afore, vafore_f
                FROM   tab_afore
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
                    NEXT FIELD cod_afore_ced
                END IF
            END IF

        BEFORE FIELD tip_prob
            IF sw_1 = 0 THEN
                LET r_afi.tip_prob = "1"
                LET sw_1 = 1
            END IF

           CASE l_tipo_sol 
           WHEN 6

            IF r_afi.n_unico IS NOT NULL AND
               r_afi.n_unico <> "                  " AND
               LENGTH(r_afi.n_unico) = 18            THEN
                LET r_afi.tip_prob = "5"

                SELECT docprob_desc 
                INTO   r_afi.docprob_desc
                FROM   tab_doc_prob
                WHERE  docprob_cod = r_afi.tip_prob

            END IF

            DISPLAY BY NAME r_afi.tip_prob,r_afi.docprob_desc

            EXIT CASE
            WHEN 7
            IF r_afi.n_unico IS NOT NULL AND
               r_afi.n_unico <> "                  " AND
               LENGTH(r_afi.n_unico) = 18            THEN
                LET r_afi.tip_prob = "5"

                SELECT docprob_desc 
                INTO   r_afi.docprob_desc
                FROM   tab_doc_prob
                WHERE  docprob_cod = r_afi.tip_prob

                DISPLAY BY NAME r_afi.tip_prob,r_afi.docprob_desc
            ELSE
               LET r_afi.tip_prob = "6"
                SELECT docprob_desc 
                INTO   r_afi.docprob_desc
                FROM   tab_doc_prob
                WHERE  docprob_cod = r_afi.tip_prob

                DISPLAY BY NAME r_afi.tip_prob,r_afi.docprob_desc
               NEXT FIELD n_folio
            END IF

             EXIT CASE
            END CASE

        AFTER FIELD tip_prob

            WHENEVER ERROR CONTINUE
                LET x_tip = r_afi.tip_prob
            WHENEVER ERROR STOP

            IF x_tip IS NULL THEN
                ERROR "SOLO PUEDE SER NUMERO"
                NEXT FIELD tip_prob
            END IF

            IF r_afi.tip_prob IS NULL OR
               r_afi.tip_prob = " "   OR
               r_afi.tip_prob = 0     THEN
                CALL Despliega_documento_probatorio()
                     RETURNING r_afi.tip_prob, r_afi.docprob_desc

                IF r_afi.tip_prob = 0 THEN
                    NEXT FIELD tip_prob
                END IF

                IF r_afi.tip_prob = 6 THEN
                    ERROR "Tipo documento probatorio no permitido"
                    NEXT FIELD tip_prob
                END IF
            ELSE
                SELECT docprob_desc 
                INTO   r_afi.docprob_desc
                FROM   tab_doc_prob
                WHERE  docprob_cod = r_afi.tip_prob

                IF SQLCA.SQLCODE <> 0 THEN
                    ERROR "Tipo de Documento Probatorio Inexistente"
                    NEXT FIELD tip_prob
                END IF
            END IF

            IF r_afi.tip_prob = 6 THEN
                ERROR "Tipo documento probatorio no permitido"
                NEXT FIELD tip_prob
            END IF

            IF r_afi.tip_prob = 5 THEN
                IF r_afi.n_unico IS NULL OR
                   r_afi.n_unico MATCHES " *" THEN
                    ERROR "CURP no capturada, tipo dcto probatorio no puede ser 5"
                    SLEEP 3
                    ERROR ""

                    LET r_afi.tip_prob = ''
                    DISPLAY BY NAME r_afi.tip_prob
                    NEXT FIELD tip_prob
                END IF
            END IF

            DISPLAY BY NAME r_afi.tip_prob,r_afi.docprob_desc


        ON KEY ( INTERRUPT, CONTROL-C, CONTROL-Z )
            CALL Inicializa()
            EXIT INPUT

        ON KEY ( ESC )

            IF r_afi.n_folio IS NULL THEN
                ERROR "Campo folio de la solicitud NO puede ser NULO"
                NEXT FIELD n_folio
            END IF

            IF r_afi.tipo_solicitud IS NULL THEN
                 ERROR "Campo tipo de registro NO puede ser NULO" 
                 NEXT FIELD tipo_solicitud
            END IF

            IF r_afi.tipo_solicitud = 7 AND
               (r_afi.cod_afore_ced      IS NULL OR
                r_afi.cod_afore_ced = 0) THEN
                ERROR "Campo Afore Cedente no es valido"
                NEXT FIELD cod_afore_ced
            END IF

            IF r_afi.frecafor IS NULL THEN
                ERROR "Campo fecha firma NO puede ser NULO"
                NEXT FIELD frecafor
            END IF

            IF r_afi.paterno IS NULL THEN
                ERROR "Campo paterno NO puede ser NULO"
                NEXT FIELD paterno
            END IF

            LET v_1 = 0

            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.paterno) RETURNING v_1, val_1

            IF v_1 = 1 THEN
                ERROR "A. Paterno ", val_1 CLIPPED
                NEXT FIELD paterno
            END IF

            LET v_1 = 0
            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.materno) RETURNING v_1, val_1

            IF v_1 = 1 THEN
                ERROR "A. Materno ", val_1 CLIPPED
                NEXT FIELD materno
            END IF

            IF r_afi.nombres IS NULL THEN
                ERROR "Campo nombre NO puede ser NULO"
                NEXT FIELD nombres
            END IF

            LET v_1 = 0
            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.nombres) RETURNING v_1, val_1

            IF v_1 = 1 THEN
                ERROR "Nombres ", val_1 CLIPPED
                NEXT FIELD nombres
            END IF

            IF r_afi.n_seguro IS NULL THEN
                 ERROR "Campo N.S.S. NO puede ser NULO" 
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

            IF r_afi.n_rfc IS NULL THEN
                ERROR "Campo R.F.C. NO puede ser NULO"
                NEXT FIELD n_rfc
            END IF

            IF LENGTH(r_afi.n_rfc) < 10 THEN
                ERROR "Debe ingresar R.F.C. completo"
                NEXT FIELD n_rfc
            END IF

            IF r_afi.sexo IS NULL THEN
                ERROR "Campo sexo NO puede ser NULO"
                NEXT FIELD sexo
            END IF

            IF r_afi.fena IS NULL THEN
                ERROR "Campo fecha de nacimiento NO puede ser NULO" 
                NEXT FIELD fena
            END IF

            IF r_afi.estadon IS NULL THEN
                ERROR "Campo entidad de nacimiento NO puede ser NULO"
                NEXT FIELD estadon
            END IF

            IF r_afi.ind_infonavit IS NULL THEN
                 ERROR "Campo credito INFONAVIT NO puede ser NULO"
                 NEXT FIELD ind_infonavit
            END IF

            SELECT "X"
            FROM   afi_ctr_identif
            WHERE  n_folio        = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud 
            AND    n_seguro       = r_afi.n_seguro
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR "No puede dar de alta mientras no ingrese la identificacion"
                NEXT FIELD tipo_solicitud
            END IF

            SELECT @calle, @numero, @codpos
            INTO   ll_calle, ll_num, ll_cpos
            FROM   afi_domicilio
            WHERE  n_folio        = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            AND    nss            = r_afi.n_seguro
            AND    marca_envio    = 'X'

            IF STATUS = NOTFOUND THEN
                ERROR "No puede dar de alta mientras no ingrese domicilio de la solicitud"
                NEXT FIELD tipo_solicitud
            END IF

            IF ll_calle IS NULL OR ll_calle = ' '     OR
               ll_num   IS NULL OR ll_num   = ' '     OR
               ll_cpos  IS NULL OR ll_cpos  = '     ' THEN
                ERROR "No puede dar de alta mientras no ingrese domicilio de la solicitud"
                NEXT FIELD paterno
            END IF

            CALL inserta_solicitud()
            CALL inserta_logico()

            OPEN WINDOW vent_anilla AT 11,3 WITH 3 ROWS , 75 COLUMNS
            ATTRIBUTE(BORDER,REVERSE)

            --CALL Numero_operacion_asignado()
            CLOSE WINDOW vent_anilla

            DISPLAY "                                                     " AT 5,1
            ATTRIBUTE(REVERSE)

            INITIALIZE g_comision.* TO NULL
            ERROR "REGISTRO INGRESADO" SLEEP 3 ERROR ""

            CALL Inicializa()

            LET vcontrol = ""
            EXIT INPUT

        ON KEY (CONTROL-Y)
            LET v_an = 0
            CALL n_identif(r_afi.n_seguro, r_afi.n_folio, r_afi.tipo_solicitud)

        ON KEY (CONTROL-W)
            CALL observaciones(r_afi.n_seguro, r_afi.n_folio, r_afi.tipo_solicitud)

        ON KEY ( CONTROL-E, CONTROL-T,CONTROL-B,CONTROL-V)


            IF r_afi.n_folio IS NULL THEN
                ERROR "Campo folio de la solicitud NO puede ser NULO"
                NEXT FIELD n_folio
            END IF

            IF r_afi.tipo_solicitud IS NULL THEN
                 ERROR "Campo tipo de registro NO puede ser NULO" 
                 NEXT FIELD tipo_solicitud
             END IF

            IF r_afi.tipo_solicitud = 7  AND
               (r_afi.cod_afore_ced      IS NULL OR
                r_afi.cod_afore_ced = 0) THEN
                ERROR "Campo Afore Cedente no es valido"
                NEXT FIELD cod_afore_ced
            END IF

            IF r_afi.frecafor IS NULL THEN
                ERROR "Campo fecha firma NO puede ser NULO"
                NEXT FIELD frecafor
             END IF

            IF r_afi.paterno IS NULL THEN
                ERROR "Campo paterno NO puede ser NULO"
                NEXT FIELD paterno
            END IF

            LET v_1 = 0

            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.paterno) RETURNING v_1, val_1

            IF v_1 = 1 THEN
                ERROR "A. Paterno ", val_1 CLIPPED
                NEXT FIELD paterno
            END IF

            LET v_1 = 0
            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.materno) RETURNING v_1, val_1

            IF v_1 = 1 THEN
                ERROR "A. Materno ", val_1 CLIPPED
                NEXT FIELD materno
            END IF

            IF r_afi.nombres IS NULL THEN
                ERROR "Campo nombre NO puede ser NULO"
                NEXT FIELD nombres
            END IF

            LET v_1 = 0
            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.nombres) RETURNING v_1, val_1

            IF v_1 = 1 THEN
                ERROR "Nombres ", val_1 CLIPPED
                NEXT FIELD nombres
            END IF

            IF r_afi.n_seguro IS NULL THEN
                 ERROR "Campo N.S.S. NO puede ser NULO" 
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

            IF r_afi.n_rfc IS NULL THEN
                ERROR "Campo R.F.C. NO puede ser NULO"
                NEXT FIELD n_rfc
            END IF

            IF LENGTH(r_afi.n_rfc) < 10 THEN
                ERROR "Debe ingresar R.F.C. completo"
                NEXT FIELD n_rfc
           END IF

           IF r_afi.sexo IS NULL THEN
               ERROR "Campo sexo NO puede ser NULO"
               NEXT FIELD sexo
           END IF

            IF r_afi.fena IS NULL THEN
                ERROR "Campo fecha de nacimiento NO puede ser NULO" 
                NEXT FIELD fena
            END IF

            IF r_afi.estadon IS NULL THEN
                ERROR "Campo entidad de nacimiento NO puede ser NULO"
                NEXT FIELD estadon
            END IF

            IF r_afi.ind_infonavit IS NULL THEN
                 ERROR "Campo credito INFONAVIT NO puede ser NULO"
                 NEXT FIELD ind_infonavit
            END IF

            SELECT "X"
            FROM   afi_ctr_identif
            WHERE  n_folio        = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud 
            AND    n_seguro       = r_afi.n_seguro
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR "No puede dar de alta mientras no ingrese la identificacion"
                NEXT FIELD tipo_solicitud
            END IF

            SELECT "X"
              FROM afi_solicitud
             WHERE @n_seguro       = r_afi.n_seguro
               AND @n_folio        = r_afi.n_folio
               AND @tipo_solicitud = r_afi.tipo_solicitud

            IF SQLCA.SQLCODE <> 0 THEN
                CALL inserta_solicitud()
            END IF

            LET vcont = vcont + 1
            LET KEY = FGL_LASTKEY()

            CASE KEY
                WHEN 5
                    LET comma = "fglgo SEPL0052.4gi ", r_afi.n_folio," ",
                                ACCION CLIPPED,"  ",vcont, " ",
                                r_afi.tipo_solicitud

                    LET bnd_comma = TRUE
                WHEN 20
                    LET comma = "fglgo SEPL0053 ", r_afi.n_seguro," ",
                                ACCION CLIPPED," ", r_afi.tipo_solicitud,
                                r_afi.n_folio

                    LET bnd_comma = TRUE
                WHEN 22
                    LET comma = "fglgo SEPL0055 ", r_afi.n_seguro," ",
                                ACCION CLIPPED," ", r_afi.tipo_solicitud,
                                r_afi.n_folio

                    LET bnd_comma = TRUE
            END CASE

            IF bnd_comma  THEN
                RUN comma
            END IF

            LET vcontrol = "S"

            NEXT FIELD paterno

        ON KEY ( CONTROL-Z)
            EXIT INPUT

    END INPUT
    CALL Inicializa()
CLOSE WINDOW ventana_1  
END FUNCTION

FUNCTION Modifica()
#a-----------------
    DEFINE l_cve_ced        CHAR(003)
    DEFINE l_nss            CHAR(011),
	   l_tipo_sol       smallint

    DEFINE recha            SMALLINT
    DEFINE recha_st         SMALLINT
    DEFINE l_nivel_prom     SMALLINT
    DEFINE dia_paso         SMALLINT
    DEFINE X_DAT            DECIMAL(10,0)
    DEFINE x_tip            DECIMAL(10,0)
    DEFINE fecha_comprueba  DATE
    DEFINE j_fecha          DATE
    DEFINE opc              CHAR(1)
    DEFINE vcontrol         CHAR(1)
    DEFINE aa,mm,dd         CHAR(2)
    DEFINE z_fecha          CHAR(10)
    DEFINE ll_calle         CHAR(30)
    DEFINE ll_num           LIKE afi_domicilio.numero
    DEFINE ll_cpos          LIKE afi_domicilio.codpos
    DEFINE r_nss            CHAR(11)
    DEFINE r_cta            SMALLINT
    DEFINE r_pro            SMALLINT

    DEFINE
        sexo_cur        CHAR(1),
        aaa             CHAR(2),
        e_fecha         CHAR(10),
        codprom         CHAR(10),
        val_1           CHAR(80),
        doc_prob_arma   CHAR(16),
        rfc_arma        CHAR(10)

    DEFINE
        d_fecha         DATE

    DEFINE
        x_pro           DECIMAL(10,0),
        x_reg           CHAR(10)

    DEFINE
        estado_cuenta   SMALLINT,
        cod_origen      SMALLINT,
        v_1             SMALLINT,
        a_yo_act        SMALLINT,
        a_yo_fena       SMALLINT,
        a_yo            SMALLINT,
        bla             SMALLINT,
        ban             SMALLINT,
        sino            SMALLINT,
        fecha_curp      CHAR(10),
        estado_cur      CHAR(02),
        cve_edo_cur     SMALLINT,
        bnd_fena        SMALLINT,
        bnd_vto         SMALLINT


    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "SEPM0011" ATTRIBUTE(BORDER)

    DISPLAY " SEPL005           MODIFICACION  SOLICITUDES AFILIACION SEP                    " AT  3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

    DISPLAY "                                                                               " AT  5,1 ATTRIBUTE(REVERSE)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " MODIFICA " AT 1,69 ATTRIBUTE(REVERSE)

    DISPLAY "[ Esc ] Graba  CTRL:[C ] Salir sin Grabar   [W] Observaciones" AT 1,1
    DISPLAY "CTRL:[E]Dom. [V]Patrones [Y] Ident" AT 2,1 --ATTRIBUTE(CYAN)

    SELECT a.afore_desc
    INTO   r_afi.desc_afore
    FROM   tab_afore a
    WHERE  a.afore_cod = r_afi.cod_afore_ced
    LET ACCION     = "A"
    LET sw_1       = 0
    LET vcont      = 0
    LET vcontrol   = ""

    LET sexo_cur   = " "
    LET cod_origen = 0
    --INITIALIZE doc_prob_arma TO NULL
    INPUT BY NAME r_afi.* WITHOUT DEFAULTS
        AFTER FIELD tipo_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD tipo_solicitud
            END IF

            IF r_afi.tipo_solicitud IS NULL THEN
                ERROR "Tipo de Solicitud NO puede ser NULO"
                NEXT FIELD tipo_solicitud
            END IF

            IF r_afi.tipo_solicitud < 6  OR
               r_afi.tipo_solicitud > 7 THEN
                ERROR "Tipo de Solicitud erroneo, ingrese tipo correcto"
                NEXT FIELD tipo_solicitud
            END IF

            SELECT ts.desc_solicitud
            INTO   r_afi.desc_solicitud
            FROM   tab_tipo_solic ts
            WHERE  ts.tipo_solicitud = r_afi.tipo_solicitud

            IF SQLCA.SQLCODE = 0 THEN
                DISPLAY BY NAME r_afi.desc_solicitud
            ELSE
                ERROR "Tipo Solic.: 6=>Registro, 7=>Traspaso"
                LET r_afi.tipo_solicitud = NULL

                DISPLAY BY NAME r_afi.tipo_solicitud
                NEXT FIELD tipo_solicitud
            END IF

        BEFORE FIELD n_folio
            LET vcontrol = "S"
            IF vcontrol = "S" THEN
                NEXT FIELD paterno
            END IF
            

        AFTER FIELD n_folio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD tipo_solicitud
            END IF

            IF r_afi.n_folio IS NULL THEN
                ERROR "Campo Folio de la Solicitud NO puede ser NULO"
                NEXT FIELD n_folio
            ELSE
                SELECT "X" 
                FROM   afi_mae_afiliado 
                WHERE  n_folio        = r_afi.n_folio
                AND    tipo_solicitud = r_afi.tipo_solicitud
                GROUP BY 1

                IF SQLCA.SQLCODE = 0 THEN
                    PROMPT "Folio ya Existe en Maestro de Afiliados, [ENTER] p/salir"
                    FOR enter
                    NEXT FIELD n_folio 
                END IF

                SELECT "X" 
                FROM   afi_solicitud
                WHERE  n_folio        = r_afi.n_folio
                AND    tipo_solicitud = r_afi.tipo_solicitud
                GROUP BY 1

                IF SQLCA.SQLCODE = 0 THEN
                    PROMPT "Folio ya Existe en solicitudes de afiliacion, [Enter] p/salir"
                    FOR enter
                    NEXT FIELD n_folio 
                    RETURN
                END IF
            END IF

        BEFORE FIELD paterno
            LET val_pat = 0

        AFTER FIELD paterno
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                CALL valida_paterno()

                IF val_pat = 1 THEN
                    LET val_pat = 0
                    NEXT FIELD paterno
                END IF

                NEXT FIELD n_folio
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                CALL valida_paterno() 

                IF val_pat = 1 THEN
                    LET val_pat = 0
                    NEXT FIELD paterno
                END IF

                NEXT FIELD n_folio
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                CALL valida_paterno() 

                IF val_pat = 1 THEN
                    LET val_pat = 0
                    NEXT FIELD paterno
                END IF

                NEXT FIELD materno
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("RIGTH") THEN
                CALL valida_paterno() 

                IF val_pat = 1 THEN
                    LET val_pat = 0
                   NEXT FIELD paterno
               END IF

               NEXT FIELD materno
           END IF

           IF r_afi.paterno IS NULL  OR
              r_afi.paterno[1] = " " THEN
               ERROR "Campo Apellido Paterno NO puede ser NULO"
               NEXT FIELD paterno
           END IF

           LET v_1 = 0

           INITIALIZE val_1 TO NULL
           CALL verifica_nombre(r_afi.paterno) RETURNING v_1, val_1

           IF v_1 = 1 THEN
               ERROR "A. Paterno ", val_1 CLIPPED
               NEXT FIELD paterno 
           END IF

        AFTER FIELD materno
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD paterno
            END IF

            IF r_afi.materno[1] = " " THEN
                ERROR "Ingrese Apellido Materno correcto o deje campo nulo (teclee ctrl-d)"
                LET r_afi.materno = NULL
                DISPLAY BY NAME r_afi.materno
                NEXT FIELD materno
            END IF

            LET v_1 = 0

            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.materno) RETURNING v_1, val_1

            IF v_1 = 1 THEN
                ERROR "A. Materno ", val_1 CLIPPED
                NEXT FIELD materno
            END IF

        BEFORE FIELD nombres

        AFTER FIELD nombres
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD materno
            END IF

            IF r_afi.nombres IS NULL OR
               r_afi.nombres = " "   THEN
                ERROR "Campo nombre NO puede ser NULO"
                NEXT FIELD nombres
            END IF

            LET v_1 = 0

            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.nombres) RETURNING v_1, val_1

            IF v_1 = 1 THEN
                ERROR "EL Nombre ", val_1 CLIPPED
                NEXT FIELD nombres 
            END IF

            IF r_afi.nombres IS NULL  OR
               r_afi.nombres[1] = " " THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD nombres
            END IF

            NEXT FIELD fena

        BEFORE FIELD fena
            INITIALIZE x_fecha, xn_fecha, xn_fena TO NULL

            IF (r_afi.n_unico IS NOT NULL AND 
                r_afi.n_unico <> " ") THEN
                LET x_fecha = r_afi.n_unico[7,8], "/",
                              r_afi.n_unico[9,10],"/",
                              "19",
                              r_afi.n_unico[5,6]

                LET bnd_fena = 1
            ELSE
                IF (r_afi.n_rfc IS NOT NULL AND 
                    r_afi.n_rfc <> " ") THEN
                    LET x_fecha = r_afi.n_rfc[7,8], "/",
                                  r_afi.n_rfc[9,10],"/",
                                  "19",
                                  r_afi.n_rfc[5,6]

                    LET bnd_fena = 2
                ELSE
                    LET bnd_fena = 0
                END IF
            END IF

            IF bnd_fena THEN
                LET xn_fecha   = x_fecha
                LET xn_fena    = x_fecha
                LET r_afi.fena = x_fecha

                DISPLAY BY NAME r_afi.fena
            END IF

        AFTER FIELD fena
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD nombres
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                NEXT FIELD nacionalidad
            END IF

            IF r_afi.fena IS NULL THEN
                ERROR "Campo fecha de nacimiento NO puede ser NULO"
                NEXT FIELD fena
            END IF

            LET a_yo_act  = 0
            LET a_yo_fena = 0
            LET a_yo      = 0

            LET a_yo_act  = YEAR(TODAY)      USING "&&&&"
            LET a_yo_fena = YEAR(r_afi.fena) USING "&&&&"
            LET a_yo      = a_yo_act - a_yo_fena

            IF a_yo > 120 THEN
                ERROR "Esta persona pasa del rengo de 120 años, verifique nuevamente"
                NEXT FIELD fena
            END IF

            DISPLAY BY NAME r_afi.fena

            NEXT FIELD sexo

        BEFORE FIELD sexo

        AFTER FIELD sexo
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD nombres
            END IF

            IF r_afi.sexo IS NULL OR
               r_afi.sexo = " "   OR
               r_afi.sexo = 0     THEN
                CALL Despliega_sexos()
                     RETURNING r_afi.sexo, r_afi.desc_sexo
                IF r_afi.sexo IS NULL OR
                   r_afi.sexo = " "   OR
                   r_afi.sexo = 0     THEN
                    NEXT FIELD sexo
                END IF
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
            NEXT FIELD n_unico

        BEFORE FIELD n_unico
            LET ver_dig = ''

        AFTER FIELD n_unico
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD nombres
            END IF

            IF LENGTH(r_afi.n_unico) > 0  AND
               LENGTH(r_afi.n_unico) < 18 THEN
                ERROR "Debe ingresar CURP completa"
                NEXT FIELD n_unico
            ELSE
                IF r_afi.n_unico[1] <> " "   OR
                   r_afi.n_unico IS NOT NULL THEN
                    IF r_afi.n_unico[11] = "H" THEN
                        LET sexo_cur = "1"
                    ELSE
                        LET sexo_cur = "2"
                    END IF

                    CALL valida_est_curp(r_afi.n_unico)
                         RETURNING pasa_curp,desc_err

                    IF pasa_curp = 1 THEN
                        ERROR "", desc_err
                        LET pasa_curp = 0
                        NEXT FIELD n_unico
                    END IF

                    CALL var_dig_curp(r_afi.n_unico)
                         RETURNING pasa, dig_curp

                        LET ver_dig = r_afi.n_unico[18]

                        IF ver_dig <> dig_curp THEN
                            ERROR "Digito Verificador Invalido curp, el digito es : ",
                              dig_curp
                        END IF
                ELSE
                    LET sexo_cur = " "
                END IF
            END IF

            IF r_afi.n_unico[1] = " " THEN
                ERROR "Debe ingresar CURP correcta"
                NEXT FIELD n_unico
            END IF

            IF r_afi.n_unico IS NOT NULL OR
               r_afi.n_unico = ' '       THEN
                LET fecha_curp = r_afi.n_unico[07,08], "/",
                                 r_afi.n_unico[09,10], "/", 
                                 "19", r_afi.n_unico[05,06] CLIPPED

                IF r_afi.fena <> fecha_curp THEN
                    WHILE TRUE
                        PROMPT "Hay Inconsistencias F.Nac./F.Nac. de curp, es correcto [S/N] ? "
                        FOR enter

                        IF enter MATCHES "[sSnN]" THEN
                            IF enter MATCHES "[sS]" THEN
                                NEXT FIELD n_seguro
                                EXIT WHILE
                            ELSE
                                NEXT FIELD fena
                            END IF
                        END IF
                    END WHILE
                    ERROR ""
                ELSE
                    NEXT FIELD n_seguro
                END IF
             END IF

            IF r_afi.n_unico IS NOT NULL OR
               r_afi.n_unico[1] <> ' ' THEN
                IF r_afi.n_unico[11] = "H" THEN
                    LET sexo_cur = "1"
                ELSE
                    LET sexo_cur = "2"
                END IF

                IF sexo_cur <> r_afi.sexo THEN
                    WHILE TRUE
                        PROMPT "Hay Inconsistencias sexo/sexo de curp, es correcto [S/N] ? "
                        FOR enter

                        IF enter MATCHES "[sSnN]" THEN
                            IF enter MATCHES "[sS]" THEN
                                NEXT FIELD n_seguro
                                EXIT WHILE
                            ELSE
                                NEXT FIELD sexo
                            END IF
                        END IF
                    END WHILE
                    ERROR ""
                END IF
            END IF

            NEXT FIELD n_seguro

        BEFORE FIELD n_seguro
            IF vcontrol='S' THEN
                NEXT FIELD n_rfc
            END IF

        AFTER FIELD n_seguro
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                IF tipo_solic = 10 THEN
                    NEXT FIELD nombres
                ELSE
                    NEXT FIELD n_seguro
                END IF
            END IF

            IF r_afi.n_seguro IS NULL THEN
                ERROR "Campo Numero de Seguro Social NO puede ser NULO"
                NEXT FIELD n_seguro
            END IF

            IF LENGTH(r_afi.n_seguro) <> 11 THEN
                ERROR "Debe ingresar N.S.S. completo"
                NEXT FIELD n_seguro
            END IF

           --- Validación para verificar que no cambien nss del precapturado

            IF r_afi.n_seguro <> vnss_ant THEN
                ERROR "NSS no se puede cambiar del numero precapturado"
                SLEEP 3
                ERROR ""

                LET r_afi.n_seguro = vnss_ant
                DISPLAY BY NAME r_afi.n_seguro
                NEXT FIELD n_seguro
            END IF

            SELECT "X"
            FROM   afi_mae_afiliado
            WHERE  n_seguro = r_afi.n_seguro
            AND    tipo_solicitud <> 5

            IF SQLCA.SQLCODE = 0  THEN
                SELECT 'X'
                FROM   cta_act_marca a
                WHERE  a.nss = r_afi.n_seguro
                AND    a.marca_cod IN(SELECT b.marca_resulta
                                      FROM   tab_marca b
                                      WHERE  b.ind_habilita = 1)

                IF SQLCA.SQLCODE <> 0  THEN
                    ERROR "N.S.S. ya Ingresado en Maestro de Afiliados"
		    CLOSE WINDOW ventana_1
                    RETURN
                END IF
            END IF

            SELECT "X"
            FROM   afi_solicitud
            WHERE  n_seguro       = r_afi.n_seguro
            AND    n_folio        = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud

            IF STATUS <> NOTFOUND THEN
                ERROR "NSS ya Ingresado en solicitudes de afiliacion"
                NEXT FIELD n_seguro
            END IF

            SELECT COUNT(*)
            INTO   xx
            FROM   afi_solicitud
            WHERE  n_seguro = r_afi.n_seguro

            IF xx <> 0 THEN
                CALL despliega_ing()
                WHILE TRUE
                    PROMPT "DESEA SEGUIR LA CAPTURA S/N ? " FOR aux_pausa

                    IF aux_pausa  MATCHES "[SsNn]" THEN
                        IF aux_pausa MATCHES "[Nn]" THEN
			                 EXIT INPUT 
                            RETURN
                        ELSE
                            EXIT WHILE
                        END IF
                    ELSE
                        DISPLAY "Solo debe presionar (S)i o (N)o" AT 18,2
                    END IF
                END WHILE
            END IF

            CALL digito_verif(r_afi.n_seguro[1,10],10) RETURNING digito

            IF digito = 32000 THEN 
                ERROR "N.S.S. solo contiene digitos" 
                NEXT FIELD n_seguro 
            END IF

            IF LENGTH(r_afi.n_seguro) = 11 AND
               digito <> r_afi.n_seguro[11] THEN
                ERROR "Digito Verificador Invalido, el digito debe ser: ",digito
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

            SELECT a.paterno,
                   a.materno,
                   a.nombres,
                   a.estadon,
                   a.sexo,
                   a.fentcons
              INTO r_afi.paterno,
                   r_afi.materno,
                   r_afi.nombres,
                   r_afi.estadon,
                   r_afi.sexo,
                   x_asigna
              FROM afi_mae_afiliado a
             WHERE a.n_seguro = r_afi.n_seguro
               AND a.tipo_solicitud = 5

            IF STATUS <>  NOTFOUND THEN
                DISPLAY BY NAME r_afi.n_seguro,
                                r_afi.paterno,
                                r_afi.materno,
                                r_afi.nombres,
                                r_afi.estadon,
                                r_afi.sexo

                DISPLAY "ASIG MISMA AFO" AT 13,14

                DISPLAY "F.Asigna. ",x_asigna USING "dd-mm-yyyy"
                AT 14,2 ATTRIBUTE(REVERSE)
            END IF

            NEXT FIELD n_rfc

        BEFORE FIELD n_rfc
            IF r_afi.n_seguro IS NULL OR
               r_afi.n_seguro = ' '   THEN
                ERROR "Campo Numero de Seguro Social NO puede ser NULO"
                NEXT FIELD n_seguro
            END IF

        AFTER FIELD n_rfc
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                LET v_1 = 0
                INITIALIZE val_1 TO NULL

                CALL verifica_rfc(r_afi.n_rfc[1,4]) RETURNING v_1, val_1
                IF v_1 = 1 THEN
                    ERROR "R.F.C. ", val_1 CLIPPED
                    NEXT FIELD n_rfc
                END IF

                NEXT FIELD fena
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                LET v_1 = 0
                INITIALIZE val_1 TO NULL

                CALL verifica_rfc(r_afi.n_rfc[1,4]) RETURNING v_1, val_1
                IF v_1 = 1 THEN
                    ERROR "R.F.C. ", val_1 CLIPPED
                    NEXT FIELD n_rfc
                END IF

                NEXT FIELD cod_esq_comision
            END IF

            IF r_afi.n_rfc IS NULL THEN
                ERROR "Campo R.F.C. NO puede ser NULO"
                NEXT FIELD n_rfc
            END IF

            IF LENGTH(r_afi.n_rfc) <> 10 AND
               LENGTH(r_afi.n_rfc) <> 13 THEN
                ERROR "Debe ingresar R.F.C. completo"
                NEXT FIELD n_rfc
            END IF

            IF NOT valida_fecha_rfc(r_afi.n_rfc[5,10]) THEN
                ERROR "Formato de RFC Incorrecto ( ----AAMMDD*** )"
                NEXT FIELD n_rfc
            ELSE
                WHENEVER ERROR CONTINUE
                LET aa = r_afi.n_rfc[5,6]
                LET mm = r_afi.n_rfc[7,8]
                LET dd = r_afi.n_rfc[9,10]
                LET z_fecha = mm,"/",dd,"/19",aa
                LET j_fecha = z_fecha

                IF r_afi.fena <> j_fecha THEN
                    WHILE TRUE
                    PROMPT
                    "Hay Inconsistencias RFC/Fecha nacim., es correcto [S/N] ? "
                    FOR enter

                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[sS]" THEN
                            NEXT FIELD estadon
                            EXIT WHILE
                        ELSE
                            NEXT FIELD fena
                        END IF
                    END IF

                    END WHILE
                END IF
                WHENEVER ERROR STOP

                IF j_fecha IS NULL THEN
                    ERROR "fecha Invalida en RFC"
                    NEXT FIELD n_rfc
                END IF
            END IF

            NEXT FIELD estadon

        AFTER FIELD estadon
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD sexo
            END IF

            IF r_afi.estadon IS NULL OR
               r_afi.estadon = 0     THEN
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

            CALL arma_clave_rfc(r_afi.paterno,
                                r_afi.materno,
                                r_afi.nombres,
                                r_afi.fena) RETURNING rfc_arma #rac

            IF rfc_arma != r_afi.n_rfc[1,10] THEN
                WHILE TRUE
                PROMPT "Hay Inconsistencias en el RFC, ¿es correcto [S/N]? "
                FOR enter

                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        NEXT FIELD nacionalidad
                        EXIT WHILE
                    ELSE
                        NEXT FIELD fena
                    END IF
                END IF

                END WHILE
            END IF

            IF r_afi.n_unico IS NOT NULL OR
               r_afi.n_unico[1] <> ' ' THEN
                LET estado_cur = r_afi.n_unico[12,13] 

                SELECT @estad_cod
                  INTO cve_edo_cur
                  FROM tab_edo_norma
                 WHERE @estad_pro = estado_cur

                IF cve_edo_cur <> r_afi.estadon THEN
                    WHILE TRUE
                    PROMPT "Hay Inconsistencias en CURP/Ent. nac., ¿es correcto [S/N]? "
                    FOR enter

                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[sS]" THEN
                            NEXT FIELD nacionalidad
                            EXIT WHILE
                        ELSE
                            NEXT FIELD estadon
                        END IF
                    END IF

                    END WHILE
                END IF
            END IF

            DISPLAY BY NAME r_afi.estadon,r_afi.desc_estadon

            NEXT FIELD nacionalidad

        BEFORE FIELD nacionalidad
            IF r_afi.nacionalidad IS NULL THEN
                LET r_afi.nacionalidad = "MEX"
                DISPLAY BY NAME r_afi.nacionalidad
            END IF

        AFTER FIELD nacionalidad
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD estadon
            END IF

            IF r_afi.nacionalidad IS NULL OR
               r_afi.nacionalidad = " "   THEN
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

            NEXT FIELD ind_infonavit

        BEFORE FIELD ind_infonavit
            LET r_afi.ind_infonavit = "0"

            SELECT @descripcion
              INTO r_afi.desc_ind_info
              FROM safre_af:tab_ind_cred
             WHERE @codigo = r_afi.ind_infonavit

            DISPLAY BY NAME r_afi.ind_infonavit, r_afi.desc_ind_info

        AFTER FIELD ind_infonavit
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD profesion_cod
            END IF

            IF r_afi.ind_infonavit IS NULL OR
               r_afi.ind_infonavit = " "   THEN
                CALL Despliega_ind_info() RETURNING r_afi.ind_infonavit,
                                                    r_afi.desc_ind_info

                IF r_afi.ind_infonavit IS NULL OR
                   r_afi.ind_infonavit = " "   THEN
                    ERROR "Se requiere el indicativo de credito de INFONAVIT"
                    NEXT FIELD ind_infonavit
                END IF
            END IF

            IF r_afi.ind_infonavit NOT MATCHES "[012]" THEN
                CALL Despliega_ind_info() RETURNING r_afi.ind_infonavit,
                                                    r_afi.desc_ind_info

                IF r_afi.ind_infonavit NOT MATCHES "[012]" THEN
                    ERROR "0) Sin credito, 1) Credito Infonavit, 2) Credito 43 bis"
                    NEXT FIELD ind_infonavit
                END IF
            ELSE
                SELECT @descripcion
                  INTO r_afi.desc_ind_info
                  FROM safre_af:tab_ind_cred
                 WHERE @codigo = r_afi.ind_infonavit

                DISPLAY BY NAME r_afi.desc_ind_info
            END IF

            LET r_afi.frecafor = TODAY
            DISPLAY BY NAME r_afi.frecafor

        AFTER FIELD cod_afore_ced
            IF r_afi.tipo_solicitud = 6 THEN 
	       CONTINUE INPUT 
	    END IF

            IF r_afi.cod_afore_ced IS NULL THEN
                ERROR "Codigo AFORE Cedente NO puede ser NULO"
                NEXT FIELD cod_afore_ced
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_emision
            END IF

            IF r_afi.cod_afore_ced IS NULL THEN
                CALL Despliega_afores()
                     RETURNING r_afi.cod_afore_ced, r_afi.desc_afore

                IF r_afi.cod_afore_ced = g_afore.afore_cod THEN
                     ERROR "Clave de Afore erronea"
                     LET r_afi.cod_afore_ced = ''
                     DISPLAY BY NAME r_afi.cod_afore_ced
                     NEXT FIELD cod_afore_ced
                END IF
            ELSE
                SELECT @afore_desc, @afore_fusion
                INTO   r_afi.desc_afore, vafore_f
                FROM   tab_afore
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
                    NEXT FIELD cod_afore_ced
                END IF
            END IF

        BEFORE FIELD tip_prob
            IF sw_1 = 0 THEN
                LET r_afi.tip_prob = "1"
                LET sw_1 = 1
            END IF

           CASE l_tipo_sol 
           WHEN 6

            IF r_afi.n_unico IS NOT NULL AND
               r_afi.n_unico <> "                  " AND
               LENGTH(r_afi.n_unico) = 18            THEN
                LET r_afi.tip_prob = "5"

                SELECT docprob_desc 
                INTO   r_afi.docprob_desc
                FROM   tab_doc_prob
                WHERE  docprob_cod = r_afi.tip_prob

            END IF

            DISPLAY BY NAME r_afi.tip_prob,r_afi.docprob_desc

            EXIT CASE
            WHEN 7
            IF r_afi.n_unico IS NOT NULL AND
               r_afi.n_unico <> "                  " AND
               LENGTH(r_afi.n_unico) = 18            THEN
                LET r_afi.tip_prob = "5"

                SELECT docprob_desc 
                INTO   r_afi.docprob_desc
                FROM   tab_doc_prob
                WHERE  docprob_cod = r_afi.tip_prob

                DISPLAY BY NAME r_afi.tip_prob,r_afi.docprob_desc
            ELSE
               LET r_afi.tip_prob = "6"
                SELECT docprob_desc 
                INTO   r_afi.docprob_desc
                FROM   tab_doc_prob
                WHERE  docprob_cod = r_afi.tip_prob

                DISPLAY BY NAME r_afi.tip_prob,r_afi.docprob_desc
               NEXT FIELD n_folio
            END IF

             EXIT CASE
            END CASE

        AFTER FIELD tip_prob

            WHENEVER ERROR CONTINUE
                LET x_tip = r_afi.tip_prob
            WHENEVER ERROR STOP

            IF x_tip IS NULL THEN
                ERROR "SOLO PUEDE SER NUMERO"
                NEXT FIELD tip_prob
            END IF

            IF r_afi.tip_prob IS NULL OR
               r_afi.tip_prob = " "   OR
               r_afi.tip_prob = 0     THEN
                CALL Despliega_documento_probatorio()
                     RETURNING r_afi.tip_prob, r_afi.docprob_desc

                IF r_afi.tip_prob = 0 THEN
                    NEXT FIELD tip_prob
                END IF

                IF r_afi.tip_prob = 6 THEN
                    ERROR "Tipo documento probatorio no permitido"
                    NEXT FIELD tip_prob
                END IF
            ELSE
                SELECT docprob_desc 
                INTO   r_afi.docprob_desc
                FROM   tab_doc_prob
                WHERE  docprob_cod = r_afi.tip_prob

                IF SQLCA.SQLCODE <> 0 THEN
                    ERROR "Tipo de Documento Probatorio Inexistente"
                    NEXT FIELD tip_prob
                END IF
            END IF

            IF r_afi.tip_prob = 6 THEN
                ERROR "Tipo documento probatorio no permitido"
                NEXT FIELD tip_prob
            END IF

            IF r_afi.tip_prob = 5 THEN
                IF r_afi.n_unico IS NULL OR
                   r_afi.n_unico MATCHES " *" THEN
                    ERROR "CURP no capturada, tipo dcto probatorio no puede ser 5"
                    SLEEP 3
                    ERROR ""

                    LET r_afi.tip_prob = ''
                    DISPLAY BY NAME r_afi.tip_prob
                    NEXT FIELD tip_prob
                END IF
            END IF

            DISPLAY BY NAME r_afi.tip_prob,r_afi.docprob_desc


        ON KEY ( INTERRUPT, CONTROL-C, CONTROL-Z )
            CALL Inicializa()
            EXIT INPUT

        ON KEY ( ESC )

            IF r_afi.n_folio IS NULL THEN
                ERROR "Campo folio de la solicitud NO puede ser NULO"
                NEXT FIELD n_folio
            END IF

            IF r_afi.tipo_solicitud IS NULL THEN
                 ERROR "Campo tipo de registro NO puede ser NULO" 
                 NEXT FIELD tipo_solicitud
            END IF

            IF r_afi.tipo_solicitud = 7 AND
               (r_afi.cod_afore_ced      IS NULL OR
                r_afi.cod_afore_ced = 0) THEN
                ERROR "Campo Afore Cedente no es valido"
                NEXT FIELD cod_afore_ced
            END IF

            IF r_afi.frecafor IS NULL THEN
                ERROR "Campo fecha firma NO puede ser NULO"
                NEXT FIELD frecafor
            END IF

            IF r_afi.paterno IS NULL THEN
                ERROR "Campo paterno NO puede ser NULO"
                NEXT FIELD paterno
            END IF

            LET v_1 = 0

            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.paterno) RETURNING v_1, val_1

            IF v_1 = 1 THEN
                ERROR "A. Paterno ", val_1 CLIPPED
                NEXT FIELD paterno
            END IF

            LET v_1 = 0
            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.materno) RETURNING v_1, val_1

            IF v_1 = 1 THEN
                ERROR "A. Materno ", val_1 CLIPPED
                NEXT FIELD materno
            END IF

            IF r_afi.nombres IS NULL THEN
                ERROR "Campo nombre NO puede ser NULO"
                NEXT FIELD nombres
            END IF

            LET v_1 = 0
            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.nombres) RETURNING v_1, val_1

            IF v_1 = 1 THEN
                ERROR "Nombres ", val_1 CLIPPED
                NEXT FIELD nombres
            END IF

            IF r_afi.n_seguro IS NULL THEN
                 ERROR "Campo N.S.S. NO puede ser NULO" 
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

            IF r_afi.n_rfc IS NULL THEN
                ERROR "Campo R.F.C. NO puede ser NULO"
                NEXT FIELD n_rfc
            END IF

            IF LENGTH(r_afi.n_rfc) < 10 THEN
                ERROR "Debe ingresar R.F.C. completo"
                NEXT FIELD n_rfc
            END IF

            IF r_afi.sexo IS NULL THEN
                ERROR "Campo sexo NO puede ser NULO"
                NEXT FIELD sexo
            END IF

            IF r_afi.fena IS NULL THEN
                ERROR "Campo fecha de nacimiento NO puede ser NULO" 
                NEXT FIELD fena
            END IF

            IF r_afi.estadon IS NULL THEN
                ERROR "Campo entidad de nacimiento NO puede ser NULO"
                NEXT FIELD estadon
            END IF

            IF r_afi.ind_infonavit IS NULL THEN
                 ERROR "Campo credito INFONAVIT NO puede ser NULO"
                 NEXT FIELD ind_infonavit
            END IF

            SELECT "X"
            FROM   afi_ctr_identif
            WHERE  n_folio        = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud 
            AND    n_seguro       = r_afi.n_seguro
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR "No puede dar de alta mientras no ingrese la identificacion"
                NEXT FIELD tipo_solicitud
            END IF

            SELECT @calle, @numero, @codpos
            INTO   ll_calle, ll_num, ll_cpos
            FROM   afi_domicilio
            WHERE  n_folio        = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            AND    nss            = r_afi.n_seguro
            AND    marca_envio    = 'X'

            IF STATUS = NOTFOUND THEN
                ERROR "No puede dar de alta mientras no ingrese domicilio de la solicitud"
                NEXT FIELD tipo_solicitud
            END IF

            IF ll_calle IS NULL OR ll_calle = ' '     OR
               ll_num   IS NULL OR ll_num   = ' '     OR
               ll_cpos  IS NULL OR ll_cpos  = '     ' THEN
                ERROR "No puede dar de alta mientras no ingrese domicilio de la solicitud"
                NEXT FIELD paterno
            END IF
    
            CALL act_solicitud()
            CALL inserta_logico()

            OPEN WINDOW vent_anilla AT 11,3 WITH 3 ROWS , 75 COLUMNS
            ATTRIBUTE(BORDER,REVERSE)

            --CALL Numero_operacion_asignado()
            CLOSE WINDOW vent_anilla

            DISPLAY "                                                     " AT 5,1
            ATTRIBUTE(REVERSE)

            INITIALIZE g_comision.* TO NULL

            CALL Inicializa()

            LET vcontrol = ""
            EXIT INPUT

        ON KEY (CONTROL-Y)
            LET v_an = 0
            CALL n_identif(r_afi.n_seguro, r_afi.n_folio, r_afi.tipo_solicitud)

        ON KEY (CONTROL-W)
            CALL observaciones(r_afi.n_seguro, r_afi.n_folio, r_afi.tipo_solicitud)

        ON KEY ( CONTROL-E, CONTROL-T,CONTROL-B,CONTROL-V)


            IF r_afi.n_folio IS NULL THEN
                ERROR "Campo folio de la solicitud NO puede ser NULO"
                NEXT FIELD n_folio
            END IF

            IF r_afi.tipo_solicitud IS NULL THEN
                 ERROR "Campo tipo de registro NO puede ser NULO" 
                 NEXT FIELD tipo_solicitud
             END IF

            IF r_afi.tipo_solicitud = 7  AND
               (r_afi.cod_afore_ced      IS NULL OR
                r_afi.cod_afore_ced = 0) THEN
                ERROR "Campo Afore Cedente no es valido"
                NEXT FIELD cod_afore_ced
            END IF

            IF r_afi.frecafor IS NULL THEN
                ERROR "Campo fecha firma NO puede ser NULO"
                NEXT FIELD frecafor
             END IF

            IF r_afi.paterno IS NULL THEN
                ERROR "Campo paterno NO puede ser NULO"
                NEXT FIELD paterno
            END IF

            LET v_1 = 0

            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.paterno) RETURNING v_1, val_1

            IF v_1 = 1 THEN
                ERROR "A. Paterno ", val_1 CLIPPED
                NEXT FIELD paterno
            END IF

            LET v_1 = 0
            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.materno) RETURNING v_1, val_1

            IF v_1 = 1 THEN
                ERROR "A. Materno ", val_1 CLIPPED
                NEXT FIELD materno
            END IF

            IF r_afi.nombres IS NULL THEN
                ERROR "Campo nombre NO puede ser NULO"
                NEXT FIELD nombres
            END IF

            LET v_1 = 0
            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.nombres) RETURNING v_1, val_1

            IF v_1 = 1 THEN
                ERROR "Nombres ", val_1 CLIPPED
                NEXT FIELD nombres
            END IF

            IF r_afi.n_seguro IS NULL THEN
                 ERROR "Campo N.S.S. NO puede ser NULO" 
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

            IF r_afi.n_rfc IS NULL THEN
                ERROR "Campo R.F.C. NO puede ser NULO"
                NEXT FIELD n_rfc
            END IF

            IF LENGTH(r_afi.n_rfc) < 10 THEN
                ERROR "Debe ingresar R.F.C. completo"
                NEXT FIELD n_rfc
           END IF

           IF r_afi.sexo IS NULL THEN
               ERROR "Campo sexo NO puede ser NULO"
               NEXT FIELD sexo
           END IF

            IF r_afi.fena IS NULL THEN
                ERROR "Campo fecha de nacimiento NO puede ser NULO" 
                NEXT FIELD fena
            END IF

            IF r_afi.estadon IS NULL THEN
                ERROR "Campo entidad de nacimiento NO puede ser NULO"
                NEXT FIELD estadon
            END IF

            IF r_afi.ind_infonavit IS NULL THEN
                 ERROR "Campo credito INFONAVIT NO puede ser NULO"
                 NEXT FIELD ind_infonavit
            END IF

            SELECT "X"
            FROM   afi_ctr_identif
            WHERE  n_folio        = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud 
            AND    n_seguro       = r_afi.n_seguro
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR "No puede dar de alta mientras no ingrese la identificacion"
                NEXT FIELD tipo_solicitud
            END IF

            SELECT "X"
              FROM afi_solicitud
             WHERE @n_seguro       = r_afi.n_seguro
               AND @n_folio        = r_afi.n_folio
               AND @tipo_solicitud = r_afi.tipo_solicitud

            IF SQLCA.SQLCODE <> 0 THEN
                CALL inserta_solicitud()
            END IF

            LET vcont = vcont + 1
            LET KEY = FGL_LASTKEY()

            CASE KEY

{
                WHEN 2
                    LET comma = "fglgo AFIM004 ", r_afi.n_seguro," ",
                                ACCION CLIPPED," ", r_afi.tipo_solicitud,
                                r_afi.n_folio

                    LET bnd_comma = TRUE
}
                WHEN 5
                    LET comma = "fglgo SEPL0052.4gi ", r_afi.n_folio," ",
                                ACCION CLIPPED,"  ",vcont, " ",
                                r_afi.tipo_solicitud

                    LET bnd_comma = TRUE
{
                WHEN 16
                    LET comma = "fglgo AFIM025 ", r_afi.n_folio, " ",
                                r_afi.tipo_solicitud, " ",
                                ACCION CLIPPED

                    LET bnd_comma = TRUE
  }
                WHEN 20
                    LET comma = "fglgo SEPL0053 ", r_afi.n_seguro," ",
                                ACCION CLIPPED," ", r_afi.tipo_solicitud,
                                r_afi.n_folio

                    LET bnd_comma = TRUE
                WHEN 22
                    LET comma = "fglgo SEPL0055 ", r_afi.n_seguro," ",
                                ACCION CLIPPED," ", r_afi.tipo_solicitud,
                                r_afi.n_folio

                    LET bnd_comma = TRUE
            END CASE

            IF bnd_comma  THEN
                RUN comma
            END IF

            LET vcontrol = "S"

            NEXT FIELD paterno

        ON KEY ( CONTROL-Z)
            EXIT INPUT

    END INPUT
    CALL Inicializa()
CLOSE WINDOW ventana_1  
END FUNCTION





FUNCTION n_identif_c(nss, folio_sol, ts)
#nic------------------------------------

    DEFINE
        fecha     DATE,
        nss       CHAR(11),
        va        CHAR(1),
        folio_sol DECIMAL(8,0),
        ts        SMALLINT

    DEFINE re_g RECORD
        clave        SMALLINT,
        descripcion  CHAR(30),
        identifica   CHAR(30)
    END RECORD

    DEFINE l_modulo CHAR(003)

    INITIALIZE re_g, va, fecha TO NULL

    CASE ts
    WHEN 6  
      LET l_modulo = "AFI"
      EXIT CASE
    WHEN 7  
      LET l_modulo = "TAA"
      EXIT CASE
    OTHERWISE
      EXIT CASE
    END CASE

    SELECT MAX(a.fecha)
      INTO fecha
      FROM afi_ctr_identif a
     WHERE a.n_seguro = nss
       AND a.n_folio  = folio_sol
       AND a.tipo_solicitud = ts

    IF SQLCA.SQLCODE <> 0 THEN
        ERROR " NO EXISTE INFORMACION PARA ESTE NSS"
        SLEEP 3
        ERROR ""
	CLOSE WINDOW ventana_1
        RETURN
    ELSE
        SELECT a.clave_identif, b.des_tipo_identif, a.identifica
          INTO re_g.*
          FROM afi_ctr_identif a, tab_identificacion b
         WHERE a.n_seguro       = nss
           AND a.n_folio        = folio_sol
           AND a.tipo_solicitud = ts
           AND a.fecha          = fecha
           AND a.clave_identif  = b.tipo_identif
           AND a.modulo_cod     = b.modulo_cod
           AND a.modulo_cod     = l_modulo
    END IF

    OPEN WINDOW v12 AT 6,6 WITH FORM "SEPM0019" 
    ATTRIBUTE(BORDER)
    DISPLAY "                                                                               " AT 1,1
    ATTRIBUTE(REVERSE)
    DISPLAY "                     I D E N T I F I C A C I O N                               " AT 2,1 
    ATTRIBUTE(REVERSE)

    DISPLAY BY NAME re_g.*

    PROMPT "PRESIONE < ENTER > PARA CONTINUAR " FOR va
    CLOSE WINDOW v12

END FUNCTION

FUNCTION Numero_operacion_asignado()
#noa--------------------------------

    DEFINE x_numerii    INTEGER

    SELECT n_operac 
    INTO   x_numerii 
    FROM   afi_solicitud
    WHERE  n_folio = r_afi.n_folio
    AND    tipo_solicitud = r_afi.tipo_solicitud

    DISPLAY "                                                                                                 " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                                                 " AT 2,1 ATTRIBUTE(REVERSE)

    PROMPT "Alta realizada ........ " 
           ATTRIBUTE(REVERSE) FOR aux_pausa

END FUNCTION

FUNCTION inserta_solicitud()
#is-------------------------

    DEFINE x_coduni         CHAR(10)
    DEFINE estado_afiliado  SMALLINT
    DEFINE en_proceso       SMALLINT

    LET estado_afiliado = ""
    LET en_proceso      = ""
    LET g_hora          = TIME

    IF r_afi.tipo_solicitud = 2 THEN
        LET st_int = 15
    ELSE
        LET st_int = 20
    END IF

    SELECT "X"
    FROM   afi_solicitud
    WHERE  @n_seguro       = r_afi.n_seguro
    AND    @n_folio        = r_afi.n_folio
    AND    @tipo_solicitud = r_afi.tipo_solicitud
END FUNCTION

FUNCTION act_solicitud()
#is-------------------------

    DEFINE x_coduni         CHAR(10)
    DEFINE estado_afiliado  SMALLINT
    DEFINE en_proceso       SMALLINT

    LET estado_afiliado = ""
    LET en_proceso      = ""
    LET g_hora          = TIME

    IF r_afi.tipo_solicitud = 2 THEN
        LET st_int = 15
    ELSE
        LET st_int = 20
    END IF

    SELECT "X"
    FROM   safre_af:afi_solicitud a
    WHERE  a.n_seguro       = r_afi.n_seguro
    AND    a.n_folio        = r_afi.n_folio
    AND    a.tipo_solicitud = r_afi.tipo_solicitud
    AND    a.status_interno in (40,42)

END FUNCTION

FUNCTION Consulta(l_nss)
#c-----------------------
    DEFINE l_nss   CHAR(011)
    DEFINE desp    CHAR(40)
    DEFINE consul  CHAR(40)
    DEFINE desc_ts CHAR(12)
    DEFINE l_mod   SMALLINT

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "SEPM0011" ATTRIBUTE(BORDER)

    DISPLAY " SEPL005           CONSULTA SOLICITUD AFILIACION POR SEPARACION                " AT  3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

    DISPLAY "                                                                               " AT  5,1 ATTRIBUTE(REVERSE)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " CONSULTA " AT 1,69 ATTRIBUTE(REVERSE)

    --DISPLAY "CONTROL: [F] Despliega [O] Motivos Rech [W] Observaciones [C] Salir"
--    AT 1,1 ATTRIBUTE(CYAN)

    DISPLAY "CONTROL [E]Dom. [V]Patrones [Y]Identif   "
    AT 2,1 ATTRIBUTE(CYAN)

     CALL Inicializa()
    LET l_mod                = 0
    LET r_afi.n_seguro       = l_nss
    LET r_afi.tipo_solicitud = NULL
    LET r_afi.n_folio        = NULL

    INPUT BY NAME r_afi.tipo_solicitud, r_afi.n_folio, r_afi.n_seguro WITHOUT DEFAULTS

    AFTER FIELD tipo_solicitud
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD tipo_solicitud
        END IF

        IF r_afi.tipo_solicitud IS NULL THEN
            NEXT FIELD n_seguro
        END IF

        IF r_afi.tipo_solicitud < 6 OR
           r_afi.tipo_solicitud > 7 THEN
            ERROR "Tipo de Solicitud Erroneo"
            NEXT FIELD tipo_solicitud
       END IF

        SELECT tts.desc_solicitud
          INTO r_afi.desc_solicitud
          FROM tab_tipo_solic tts
         WHERE tts.tipo_solicitud = r_afi.tipo_solicitud

        IF SQLCA.SQLCODE = 0 THEN
            DISPLAY BY NAME r_afi.desc_solicitud
        ELSE
            ERROR "Tipo de Solicitud solo puede ser 6)Reg. 7)Trasp."
            LET r_afi.tipo_solicitud = NULL

            DISPLAY BY NAME r_afi.tipo_solicitud
            NEXT FIELD tipo_solicitud
        END IF

        IF r_afi.tipo_solicitud = 6 OR
           r_afi.tipo_solicitud = 7 THEN
           LET bant = TRUE
        END IF

        NEXT FIELD n_folio

    AFTER FIELD n_folio
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD tipo_solicitud
        END IF

        IF r_afi.n_folio IS NULL THEN
            NEXT FIELD n_seguro
        ELSE
            IF NOT Rescata_datos("N",r_afi.n_folio) THEN
                ERROR "Afiliado NO existe"
                NEXT FIELD n_folio
            END IF

            DISPLAY BY NAME r_afi.*
        END IF

    AFTER FIELD n_seguro
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            CALL Inicializa()
            NEXT FIELD tipo_solicitud
        END IF

        IF r_afi.n_seguro IS NULL THEN
            NEXT FIELD tipo_solicitud
        ELSE
            IF NOT Rescata_datos("C",r_afi.n_seguro) THEN
                ERROR "Afiliado NO existe"
                NEXT FIELD n_seguro
            END IF

            DISPLAY BY NAME r_afi.*
        END IF

    ON KEY ( CONTROL-G )
       -- IF r_afi.n_folio > 0 THEN
         --   CALL Ingresa_Esquema_Comision("C",r_afi.n_folio)
        --ELSE
            --ERROR
           --"No puede ver Esquema Comision sin antes especificar la solicitud"
         --END IF

    ON KEY ( CONTROL-F ) 
         LET l_mod = 1
         EXIT INPUT
    ON KEY ( CONTROL-O )
        CALL motivo_rechazo()

    --ON KEY ( CONTROL-F )
        --CALL Despliega()
        --DISPLAY BY NAME r_afi.tipo_solicitud, r_afi.n_folio

    ON KEY ( CONTROL-Y )
        LET v_an = 0
        CALL n_identif_c(r_afi.n_seguro, r_afi.n_folio, r_afi.tipo_solicitud)

    ON KEY (CONTROL-W)
        CALL observaciones_c(r_afi.n_seguro,r_afi.n_folio,r_afi.tipo_solicitud)

    ON KEY ( INTERRUPT, CONTROL-C, CONTROL-Z )
        CALL Inicializa()
        DISPLAY "                                     " AT 5,56
        ATTRIBUTE(REVERSE)
        DISPLAY "                                    " AT 13,57
        ATTRIBUTE(REVERSE)
        EXIT INPUT

    ON KEY (CONTROL-E,CONTROL-T,CONTROL-B,CONTROL-V,CONTROL-P)
        IF r_afi.n_seguro IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD n_seguro
        END IF

        LET KEY = FGL_LASTKEY()
        CASE KEY
				{
            WHEN 2
               LET comma = "fglgo AFIM004 ", r_afi.n_seguro," ",
                            ACCION CLIPPED," ", r_afi.tipo_solicitud,
                            r_afi.n_folio

               LET bnd_comma = 1
	       }
            WHEN 5
               LET comma = "fglgo SEPL0052.4gi ", r_afi.n_folio," ",
                            ACCION CLIPPED,"  ",vcont, " ",
                            r_afi.tipo_solicitud

               LET bnd_comma = 1
	       {
            WHEN 16
               LET comma = "fglgo AFIM025 ", r_afi.n_folio, " ",
                            r_afi.n_folio, " ", r_afi.tipo_solicitud," ",
                            ACCION CLIPPED

               LET bnd_comma = 1
	       }
            WHEN 20
               LET comma = "fglgo SEPL0053 ", r_afi.n_seguro," ",
                            ACCION CLIPPED," ", r_afi.tipo_solicitud,
                            r_afi.n_folio

               LET bnd_comma = 1
            WHEN 22
               LET comma = "fglgo SEPL0055 ", r_afi.n_seguro," ",
                            ACCION CLIPPED," ", r_afi.tipo_solicitud,
                            r_afi.n_folio

               LET bnd_comma = 1
        END CASE

        IF bnd_comma  THEN
            RUN comma
        END IF

    END INPUT
    IF l_mod = 1 THEN
       IF (su_estatus = 40 OR 
           su_estatus = 42 )THEN
              CLOSE WINDOW ventana_1
              CALL Modifica()
       ELSE 
              ERROR "SOLICITUD NO MODIFICABLE ...."
              LET l_mod = 0
       END IF 
    END IF
     
      IF l_mod <> 1 THEN
      CALL inicializa()
      CLOSE WINDOW ventana_1
      END IF
END FUNCTION

FUNCTION Pregunta(a)
#p------------------

   DEFINE a    SMALLINT

   CASE a
     WHEN 1
       PROMPT "Presione ENTER para Consultar otro Registro" FOR aux_pausa
     WHEN 2
       PROMPT "Esta seguro de Eliminar S/N ?" FOR aux_pausa
     WHEN 3
       PROMPT "Esta seguro de Cancelar S/N ?" FOR aux_pausa
   END CASE

END FUNCTION

FUNCTION Rescata_datos(l_aux_val,x_valor)
#rd--------------------------------------

    DEFINE l_aux_val  CHAR(1)
    DEFINE x_valor    CHAR(11)
    DEFINE xx_num     DECIMAL(11,0)
    DEFINE tot_afil   SMALLINT
    DEFINE aux        LIKE afi_solicitud.n_folio

    IF l_aux_val = 'C' THEN
        SELECT COUNT(*)
        INTO   tot_afil
        FROM   afi_solicitud a
        WHERE  a.n_seguro       = x_valor

        IF tot_afil > 1 THEN
            CALL despliega_ing()
            LET l_aux_val = 'N'
            LET x_valor   = r_afi.n_folio
        ELSE 
           SELECT a.tipo_solicitud 
           INTO r_afi.tipo_solicitud 
           FROM afi_solicitud a 
           WHERE a.n_seguro = x_valor
        END IF
    END IF

    CASE l_aux_val
        WHEN "C"
            SELECT MAX(n_folio)
              INTO aux
              FROM afi_solicitud
             WHERE n_seguro = x_valor

     IF r_afi.tipo_solicitud = 7 THEN
            SELECT a.n_folio,
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
                   a.ind_infonavit,
                   a.nacionalidad,
                   a.tipo_solicitud,
                   a.cod_afore_ced,
                   c.afore_desc   ,
                   a.finicta      ,
                   a.tip_prob     ,
                   b.docprob_desc   
            INTO   r_afi.n_folio,
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
                   r_afi.ind_infonavit,
                   r_afi.nacionalidad,
                   r_afi.tipo_solicitud,
                   r_afi.cod_afore_ced,
                   r_afi.desc_afore,
                   x_finicta ,
                   r_afi.tip_prob,
                   r_afi.docprob_desc
            FROM   afi_solicitud a,
                   tab_doc_prob b,
                   tab_afore    c
            WHERE  a.n_seguro       = x_valor
            AND    a.tipo_solicitud = r_afi.tipo_solicitud
            AND    a.n_folio        = aux
            AND    a.tip_prob       = b.docprob_cod
            AND    a.cod_afore_ced  = c.afore_cod

         ELSE

            SELECT a.n_folio,
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
                   a.ind_infonavit,
                   a.nacionalidad,
                   a.tipo_solicitud,
                   a.cod_afore_ced,
                   ""             ,
                   a.finicta      ,
                   a.tip_prob     ,
                   b.docprob_desc   
            INTO   r_afi.n_folio,
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
                   r_afi.ind_infonavit,
                   r_afi.nacionalidad,
                   r_afi.tipo_solicitud,
                   r_afi.cod_afore_ced,
                   r_afi.desc_afore,
                   x_finicta ,
                   r_afi.tip_prob,
                   r_afi.docprob_desc
            FROM   afi_solicitud a,
                   tab_doc_prob b
            WHERE  a.n_seguro       = x_valor
            AND    a.tipo_solicitud = r_afi.tipo_solicitud
            AND    a.n_folio        = aux
            AND    a.tip_prob       = b.docprob_cod

         END IF

        WHEN "N"
            LET xx_num = x_valor clipped
        IF r_afi.tipo_solicitud = 7 THEN
            SELECT a.n_folio,
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
                   a.ind_infonavit,
                   a.nacionalidad,
                   a.tipo_solicitud,
                   a.cod_afore_ced,
                   c.afore_desc   ,
                   a.finicta      ,
                   a.tip_prob     ,
                   b.docprob_desc
            INTO   r_afi.n_folio,
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
                   r_afi.ind_infonavit,
                   r_afi.nacionalidad,
                   r_afi.tipo_solicitud,
                   r_afi.cod_afore_ced,
                   r_afi.desc_afore   ,
                   x_finicta,
                   r_afi.tip_prob    ,
                   r_afi.docprob_desc
            FROM   afi_solicitud a ,
                   tab_doc_prob b  ,
                   tab_afore    c
            WHERE  a.n_folio        = xx_num
            AND    a.tipo_solicitud = r_afi.tipo_solicitud
            AND    a.tip_prob       = b.docprob_cod 
            AND    a.cod_afore_ced  = c.afore_cod
        ELSE 
            SELECT a.n_folio,
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
                   a.ind_infonavit,
                   a.nacionalidad,
                   a.tipo_solicitud,
                   a.cod_afore_ced,
                   " "           ,
                   a.finicta      ,
                   a.tip_prob     ,
                   b.docprob_desc
            INTO   r_afi.n_folio,
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
                   r_afi.ind_infonavit,
                   r_afi.nacionalidad,
                   r_afi.tipo_solicitud,
                   r_afi.cod_afore_ced,
                   r_afi.desc_afore   ,
                   x_finicta,
                   r_afi.tip_prob    ,
                   r_afi.docprob_desc
            FROM   afi_solicitud a ,
                   tab_doc_prob b  
            WHERE  a.n_folio        = xx_num
            AND    a.tipo_solicitud = r_afi.tipo_solicitud
            AND    a.tip_prob       = b.docprob_cod 

        END IF

    OTHERWISE 
            EXIT CASE
    END CASE

    SELECT sexo_desc 
    INTO   r_afi.desc_sexo 
    FROM   tab_sexo
    WHERE  sexo_cod = r_afi.sexo

    IF SQLCA.SQLCODE <> 0 THEN
        LET r_afi.desc_sexo = "NO EXISTE"
    END IF

    SELECT estad_desc 
    INTO   r_afi.desc_estadon
    FROM   tab_estado
    WHERE  estad_cod = r_afi.estadon

    IF SQLCA.SQLCODE <> 0 THEN
        LET r_afi.desc_estadon = "NO EXISTE"
    END IF

    SELECT pais_desc 
    INTO   r_afi.desc_nacionalidad 
    FROM   tab_pais
    WHERE  pais_cod = r_afi.nacionalidad

    IF SQLCA.SQLCODE <> 0 THEN
        LET r_afi.desc_nacionalidad = "NO EXISTE"
    END IF

    IF r_afi.tipo_solicitud = 7 THEN
        SELECT afore_desc
        INTO   r_afi.desc_afore
        FROM   tab_afore
        WHERE  afore_cod = r_afi.cod_afore_ced

        IF SQLCA.SQLCODE <> 0 THEN
            LET r_afi.desc_afore = " Cod. Afore NO EXISTE "
        END IF
    END IF


    SELECT @descripcion
      INTO r_afi.desc_ind_info
      FROM safre_af:tab_ind_cred
     WHERE @codigo = r_afi.ind_infonavit

    CALL rescata_status(su_estatus)

    RETURN TRUE

END FUNCTION


FUNCTION Desea_reenviar()
#dr----------------------

    PROMPT "¿Desea Enviar Nuevamente a Certificar esta Solicitud [S/N] ? "
    FOR aux_pausa

END FUNCTION

FUNCTION Desea_modificar()
#dm-----------------------

    PROMPT "¿Desea Modificar la Informacion [S/N] ? " FOR aux_pausa

END FUNCTION


FUNCTION Despliega()
#d------------------

    DEFINE aux_pausa    CHAR(1)
    DEFINE paterno      CHAR(50)
    DEFINE materno      CHAR(50)
    DEFINE nombres      CHAR(50)
    DEFINE n_busqueda   CHAR(100)
    DEFINE cla_sel      CHAR(250)
    DEFINE txt          CHAR(300)
    DEFINE i            SMALLINT
    DEFINE HACER        SMALLINT
    DEFINE pat,mat,nom  CHAR(50)
    DEFINE txt1         CHAR(300)

    DEFINE l_reg ARRAY[30000] OF RECORD
        n_seguro        CHAR(11),
        n_unico         CHAR(18),
        tipo_solicitud  SMALLINT,
        n_folio         INTEGER,
        nombre          CHAR(50)
    END RECORD


    LET cla_sel = NULL

    OPEN WINDOW v1 AT  4,4 WITH FORM "SEPM0012" ATTRIBUTE(BORDER)
    DISPLAY "          USTED PUEDE UTILIZAR EL * (ASTERISCO) COMO COMODIN                " AT 1,1 ATTRIBUTE(REVERSE)

    LET HACER = TRUE
    LET INT_FLAG=TRUE

    CONSTRUCT BY NAME cla_sel ON paterno, materno, nombres

    ON KEY ( INTERRUPT, CONTROL-C, CONTROL-Z )
        LET HACER = FALSE
        EXIT CONSTRUCT

    ON KEY ( ESC )
        LET INT_FLAG = FALSE
        EXIT CONSTRUCT
    END CONSTRUCT

    LET txt = "SELECT n_seguro,n_unico,tipo_solicitud,n_folio,paterno,materno,",
              " nombres FROM afi_solicitud WHERE ",cla_sel CLIPPED,
              " ORDER BY 5,6" CLIPPED

    IF HACER THEN
        ERROR "Buscando Informacion"
        PREPARE cur1 FROM txt
        DECLARE cursor_1 cursor FOR cur1

        LET i = 1

        FOREACH cursor_1 INTO l_reg[i].n_seguro, l_reg[i].n_unico,
                              l_reg[i].tipo_solicitud, l_reg[i].n_folio,
                              pat, mat, nom

            LET l_reg[i].nombre = pat CLIPPED," ",
                                  mat CLIPPED," ",
                                  nom CLIPPED

            LET i = i + 1

            IF i >= 30000 THEN
                ERROR "Sobrepaso Capacidad Maxima del Arreglo"
                EXIT FOREACH
            END IF
        END FOREACH

        FREE CURSOR_1

        IF (i-1) < 1 THEN
            ERROR "ARCHIVO AFILIADOS VACIO"
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

        ON KEY ( INTERRUPT, CONTROL-C, CONTROL-Z )
            LET r_afi.n_seguro       = NULL
            LET r_afi.n_unico        = NULL
            LET r_afi.n_folio        = NULL
            LET r_afi.tipo_solicitud = NULL
            EXIT DISPLAY

        END DISPLAY
    END IF

    CLOSE WINDOW v1

END FUNCTION

FUNCTION rescata_status(valor)
#rs---------------------------

    DEFINE valor          SMALLINT
    DEFINE l_estado       CHAR(21)
    DEFINE x_fecha        DATE
    DEFINE x_fecha_envio  DATE

    SELECT ea.estado_desc
    INTO   l_estado
    FROM   tab_status_afi ea
    WHERE  ea.estado_cod = valor

    DISPLAY l_estado CLIPPED,"                            " AT 5,56
    ATTRIBUTE(REVERSE)

    SELECT fentcons, finicta, fecha_envio
    INTO   x_fecha, x_finicta, x_fecha_envio
    FROM   afi_solicitud
    WHERE  n_seguro       = r_afi.n_seguro
    AND    n_folio        = r_afi.n_folio
    AND    tipo_solicitud = r_afi.tipo_solicitud

    IF x_finicta IS NULL OR
       x_finicta = "12/31/1899" THEN
        LET x_finicta = ''
    END IF

    DISPLAY "F.Envio ",x_fecha_envio USING "dd-mm-yyyy","  F.Certif. ",x_fecha USING "dd-mm-yyyy","  F.Apert.Cta. ",x_finicta USING "dd-mm-yyyy" ,"                   "
    AT 13,1 ATTRIBUTE(REVERSE)

END FUNCTION

FUNCTION motivo_rechazo()
#mr----------------------

    DEFINE a ARRAY[100] OF RECORD
        codigo      SMALLINT,
        descripcion CHAR(80),
        cve_afore   INTEGER ,
        desc_afore  CHAR(15),
        fech_afil   DATE    ,
        n_pcanase   CHAR(50)
    END RECORD

    DEFINE i         SMALLINT
    DEFINE j         SMALLINT
    DEFINE fech_rech DATE

    OPEN WINDOW v34  at 6,5 with form "SEPM0013" attribute(border)
    DISPLAY " [ Ctrl_c ] Salir " AT 1,1
    DISPLAY "                                MOTIVOS DE RECHAZO                                 " AT 3,1 ATTRIBUTE(REVERSE)
    DELETE FROM tmp_mot_rch

    INSERT INTO tmp_mot_rch
    SELECT rdeta_cod,
           observacion,
           codigo_afore,
           "",
           f_rechazo,
           nombre_pcanase
    FROM   afi_rechaza_cert
    WHERE  n_seguro = r_afi.n_seguro
    AND    n_folio  = r_afi.n_folio
    AND    tipo_solicitud = r_afi.tipo_solicitud

    INSERT INTO tmp_mot_rch
    SELECT motivo_rechazo,
           desc_rech,
           cve_ced_cuenta,
           '',
           fecha_presentacion,
           ''
    FROM   taa_det_devol,
    OUTER (tab_dev_taa)
    WHERE  n_seguro = r_afi.n_seguro
    AND    motivo_rechazo = cod_rech

    INSERT INTO tmp_mot_rch
    SELECT 14,
           'NO ATENDIDA',
           cve_ced_cuenta,
           '',
           fecha_presentacion,
           ''
    FROM   taa_det_no_aten
    WHERE  n_seguro = r_afi.n_seguro
    DECLARE cursor_o CURSOR FOR
        SELECT *
        FROM   tmp_mot_rch
        ORDER BY fech_rech DESC

        LET i = 1

        FOREACH cursor_o INTO a[i].*
            SELECT afore_desc
            INTO   a[i].desc_afore
            FROM   tab_afore
            WHERE  afore_cod = a[i].cve_afore

            IF a[i].descripcion MATCHES ' *' THEN
                IF r_afi.tipo_solicitud = 1 THEN
                    SELECT trd.rdeta_desc_c
                    INTO   a[i].descripcion
                    FROM   tab_rdeta trd
                    WHERE  trd.rdeta_cod = a[i].codigo
                    AND    trd.modulo_cod = 'afi'
                ELSE
                    SELECT trd.rdeta_desc_c
                    INTO   a[i].descripcion
                    FROM   tab_rdeta trd
                    WHERE  trd.rdeta_cod = a[i].codigo
                    AND    trd.modulo_cod = 'taa'
                END IF

                IF SQLCA.SQLCODE <> 0 THEN
                    LET a[i].descripcion = 'SIN DESCRIPCION'
                END IF
            END IF

            IF a[i].cve_afore = 0 THEN
                LET a[i].cve_afore = ''
            END IF

            LET i = i + 1

        END FOREACH

        CALL SET_COUNT(i-1)

        DISPLAY ARRAY a TO scr_1.*

        ON KEY ( INTERRUPT )
            EXIT DISPLAY
            END DISPLAY

    CLOSE WINDOW v34

END FUNCTION

FUNCTION Ingresa_Esquema_comision(aux_pausa,X_NUM_FOLIO)
#iec----------------------------------------------------

    DEFINE aux_pausa        CHAR(1)
    DEFINE X_NUM_FOLIO      INTEGER

    OPEN WINDOW ventanilla_a AT 9,10 WITH FORM "SEPM0014" ATTRIBUTE(BORDER)

    DISPLAY "                  E S Q U E M A    C O M I S I O N                " 
    AT 3,1 ATTRIBUTE(REVERSE)

    CASE aux_pausa
        WHEN "A"
            DISPLAY "" AT 1,1
            DISPLAY "" AT 2,1
            DISPLAY " AGREGA " AT 1,55 ATTRIBUTE(REVERSE)
            DISPLAY " [ Esc ] Grabar " AT 2,2

            INPUT BY NAME g_comision.* WITHOUT DEFAULTS
                AFTER FIELD cod_esq_comision
                    IF g_comision.cod_esq_comision IS NULL THEN
                        CALL Despliega_Esquema()
                    ELSE
                        SELECT desc_esq_comision
                        INTO   g_comision.desc_esq_comision
                        FROM   com_esq_comis
                        WHERE  cod_esq_comision = g_comision.cod_esq_comision

                        IF STATUS = NOTFOUND THEN
                            ERROR "Esquema Inexistente"
                            NEXT FIELD cod_esq_comision
                        END IF
                    END IF

                    DISPLAY BY NAME g_comision.cod_esq_comision,
                                    g_comision.desc_esq_comision

                ON KEY ( INTERRUPT, CONTROL - C, CONTROL - Z )
                    ERROR "Debe Capturar Esquema de Comision para la Solicitud"
                    SLEEP 2

                ON KEY ( ESC )
                    IF g_comision.cod_esq_comision IS NULL THEN
                        ERROR "Esquema Inexistente"
                        NEXT FIELD cod_esq_comision
                    END IF

                EXIT INPUT
            END INPUT

        WHEN "C"
            DISPLAY "" AT 1,1
            DISPLAY "" AT 2,1
            DISPLAY " CONSULTA " AT 1,55 ATTRIBUTE(REVERSE)
            DISPLAY " [ Ctrl-C ] Salir " AT 2,2

            SELECT cod_esq_comision, ""
            INTO   g_comision.cod_esq_comision, g_comision.desc_esq_comision
            FROM   afi_solicitud 
            WHERE  n_folio        = X_NUM_FOLIO
            AND    tipo_solicitud = r_afi.tipo_solicitud

            SELECT desc_esq_comision 
            INTO   g_comision.desc_esq_comision 
            FROM   com_esq_comis
            WHERE  cod_esq_comision = g_comision.cod_esq_comision

            DISPLAY BY NAME g_comision.*
            PROMPT "Presione < ENTER >para Continuar" FOR aux_pausa

        WHEN "M"
            DISPLAY "" AT 1,1
            DISPLAY "" AT 2,1
            DISPLAY " CONSULTA " AT 1,55 ATTRIBUTE(REVERSE)
            DISPLAY "        [ Ctrl-C ] Salir " AT 2,2

            SELECT cod_esq_comision, ""
            INTO   g_comision.cod_esq_comision, g_comision.desc_esq_comision
            FROM   afi_solicitud
            WHERE  n_folio = X_NUM_FOLIO
            AND    tipo_solicitud = r_afi.tipo_solicitud

            SELECT  desc_esq_comision 
            INTO    g_comision.desc_esq_comision 
            FROM    com_esq_comis
            WHERE   cod_esq_comision = g_comision.cod_esq_comision

            DISPLAY BY NAME g_comision.*

            INPUT BY NAME g_comision.* WITHOUT DEFAULTS
                AFTER FIELD cod_esq_comision
                IF g_comision.cod_esq_comision IS NULL THEN
                    CALL Despliega_Esquema()
                ELSE
                    SELECT desc_esq_comision
                    INTO   g_comision.desc_esq_comision
                    FROM   com_esq_comis
                    WHERE  cod_esq_comision = g_comision.cod_esq_comision

                    IF STATUS = NOTFOUND THEN
                        ERROR "Esquema Inexistente"
                        NEXT FIELD cod_esq_comision
                    END IF
                END IF

                DISPLAY BY NAME g_comision.cod_esq_comision,
                                g_comision.desc_esq_comision

            ON KEY ( INTERRUPT, CONTROL - C, CONTROL - Z )
                ERROR "Debe Capturar Esquema de Comision para la Solicitud"
                SLEEP 2
                EXIT INPUT

            ON KEY ( ESC )
                IF g_comision.cod_esq_comision IS NULL THEN
                    ERROR "Esquema Inexistente"
                    NEXT FIELD cod_esq_comision
                END IF

                EXIT INPUT

        END INPUT

    END CASE

    CLOSE WINDOW ventanilla_a

END FUNCTION

FUNCTION Despliega_esquema()
#de-------------------------

    DEFINE c ARRAY[100] OF RECORD
       codigo       SMALLINT,
       descripcion  CHAR(50)
    END RECORD

    DEFINE i SMALLINT

    DECLARE cursor_1a1 CURSOR FOR 
    SELECT  cod_esq_comision, desc_esq_comision
    FROM    com_esq_comis
    ORDER  BY 1

    LET i = 1

    FOREACH cursor_1a1 INTO c[i].*
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    OPEN WINDOW ventanilla_b AT 9,10 WITH FORM "SEPM0016" ATTRIBUTE(BORDER)
    DISPLAY "                   ESQUEMAS DE COMISION                 " AT 3,1 
    ATTRIBUTE ( REVERSE )

    DISPLAY ARRAY c TO scr_1.*

    ON KEY ( INTERRUPT, CONTROL - C, CONTROL - Z )
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
        status_interno CHAR(15),
        fentcons       LIKE afi_solicitud.fentcons
    END RECORD

    DEFINE
        idx        SMALLINT,
        seguro_cnt SMALLINT,
        array_sz   SMALLINT,
        over_size  SMALLINT,
        i          SMALLINT

    LET array_sz = 200

    OPEN WINDOW pantda AT 7,4 WITH FORM "SEPM0017" ATTRIBUTE(BORDER)
    DISPLAY "                              [CTRL-C] Salir                                   " AT 1,1 ATTRIBUTE(REVERSE)

    LET over_size = FALSE
    LET seguro_cnt = 1

    DECLARE cur_err CURSOR FOR
    SELECT a.n_seguro,
           a.n_folio,
           a.tipo_solicitud,
           a.status_interno,
           a.fentcons
      FROM afi_solicitud a
     WHERE a.n_seguro       = r_afi.n_seguro

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
            MESSAGE "Manuf array full: can only DISPLAY ",array_sz USING "<<<<"
        END IF
    END IF

    CALL SET_COUNT(seguro_cnt-1)

    LET int_flag = FALSE
    DISPLAY ARRAY rec_1 TO rec.*

    ON KEY ( CONTROL-M )
        LET i = ARR_CURR()
        IF ACCION = 'C' THEN
            LET r_afi.n_seguro       = rec_1[i].n_seguro
            LET r_afi.n_folio        = rec_1[i].n_folio
            LET r_afi.tipo_solicitud = rec_1[i].tipo_solicitud
        END IF

        EXIT DISPLAY

    END DISPLAY

    LET idx = ARR_CURR()
    IF int_flag THEN
        LET int_flag            = FALSE
        LET rec_1[idx].n_seguro = NULL
    END IF

    CLOSE WINDOW pantda
END FUNCTION

FUNCTION valida_paterno()
#vp----------------------
   ### OJO 12
   IF r_afi.paterno IS NULL OR
      r_afi.paterno = " "   THEN
      ERROR "Campo paterno NO puede ser NULO"
      LET val_pat = 1 
   ELSE
      LET v_1 = 0
      INITIALIZE val_1 TO NULL
      CALL verifica_nombre(r_afi.paterno) RETURNING v_1, val_1 #ve--
      IF v_1 = 1 THEN
         ERROR "A. Paterno ", val_1 CLIPPED
         LET val_pat = 1 
      END IF
   END IF
   ### OJO 12

END FUNCTION

FUNCTION n_identif_m(vn_seguro, vn_folio, vtipo_solicitud)
#nim------------------------------------------------------

    DEFINE vn_seguro       CHAR(11)
    DEFINE vn_folio        DECIMAL(8,0)
    DEFINE vtipo_solicitud SMALLINT
    DEFINE vfecha          DATE

    DEFINE re_g RECORD
        clave       SMALLINT,
        descripcion CHAR(30),
        identifica  CHAR(30)
    END RECORD

    DEFINE va          CHAR(1)
    DEFINE clave       SMALLINT
    DEFINE descripcion CHAR(30)
    DEFINE identifica  CHAR(30)
    DEFINE fe_hoy      DATE
    DEFINE ban         SMALLINT
    DEFINE l_modulo    CHAR(003)

    INITIALIZE re_g, va, vfecha TO NULL

    CASE vtipo_solicitud
     WHEN 6
      LET l_modulo = "AFI"
      EXIT CASE
     WHEN 7
      LET l_modulo = "TAA"
      EXIT CASE
     OTHERWISE 
      EXIT CASE
     END CASE

    LET fe_hoy = TODAY

    SELECT MAX(a.fecha)
    INTO   vfecha
    FROM   afi_ctr_identif a
    WHERE  a.n_seguro = vn_seguro
    AND    a.n_folio  = vn_folio
    AND    a.tipo_solicitud = vtipo_solicitud
    AND    a.modulo_cod = l_modulo

    IF STATUS = NOTFOUND THEN
        ERROR " NO EXISTE INFORMACION PARA ESTE NSS"
        SLEEP 3
        ERROR ""
	CLOSE WINDOW ventana_1 
        RETURN
    END IF

    LET ban = 0

    SELECT a.clave_identif, b.des_tipo_identif, a.identifica
    INTO   re_g.*
    FROM   afi_ctr_identif a, tab_identificacion b
    WHERE  a.n_seguro       = vn_seguro
    AND    a.n_folio        = vn_folio
    AND    a.tipo_solicitud = vtipo_solicitud
    AND    a.fecha          = vfecha
    AND    a.clave_identif  = b.tipo_identif
    AND    a.modulo_cod = b.modulo_cod
    AND    a.modulo_cod = l_modulo

    LET clave       = re_g.clave 
    LET descripcion = re_g.descripcion
    LET identifica  = re_g.identifica

    OPEN WINDOW vv_2 AT  6,6 WITH FORM "SEPM0019" ATTRIBUTE(BORDER)

    DISPLAY " [ Esc ] Salvar        [ Ctrl_C ] Salir                             "
    AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "                     I D E N T I F I C A C I O N                    "
    AT 2,1 ATTRIBUTE(REVERSE)

    DISPLAY BY NAME re_g.*

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
            IF re_g.clave IS NOT NULL AND
               re_g.descripcion IS NOT NULL AND
               re_g.identifica IS NOT NULL THEN
                DISPLAY BY NAME re_g.*
            END IF

            IF re_g.clave IS NULL OR re_g.clave = " " THEN
                CALL clave_i(vtipo_solicitud) 
                 RETURNING re_g.clave,re_g.descripcion

                IF re_g.clave IS NULL OR
                   re_g.clave = " "   OR
                   re_g.clave = 0 THEN
                    ERROR "Digite el Codigo Deseado, este campo no puede ser Nulo"
                    NEXT FIELD clave
                END IF
            ELSE
                SELECT b.des_tipo_identif
                INTO   re_g.descripcion
                FROM   tab_identificacion b
                WHERE  b.tipo_identif = re_g.clave
                AND    b.modulo_cod = l_modulo

                IF SQLCA.SQLCODE <> 0 THEN
                    ERROR "Codigo Incorrecto, Verifique nuevamente"
                    NEXT FIELD clave
                END IF

                DISPLAY re_g.clave       TO clave
                DISPLAY re_g.descripcion TO descripcion
            END IF
         ON KEY(ESC)
         ON KEY(CONTROL-C)
             EXIT INPUT

         ON KEY(INTERRUPT, CONTROL - C, CONTROL - Z)
             EXIT INPUT

    END INPUT

    CLOSE WINDOW vv_2 

END FUNCTION

FUNCTION n_identif(vn_seguro, vn_folio, vtipo_solicitud)
#ni-----------------------------------------------------

    DEFINE
        vn_seguro       CHAR(11),
        vn_folio        INTEGER,
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

     DEFINE l_modulo CHAR(003)

     CASE vtipo_solicitud
     WHEN 6 
      LET l_modulo = "AFI"
      EXIT CASE
     WHEN 7 
      LET l_modulo = "TAA"
      EXIT CASE
     OTHERWISE 
      EXIT CASE
     END CASE


    LET va  = 0
    LET ban = 0

    SELECT a.clave_identif, b.des_tipo_identif, a.identifica
      INTO re_g.*
      FROM afi_ctr_identif a, tab_identificacion b
     WHERE a.n_seguro       = vn_seguro
       AND a.n_folio        = vn_folio
       AND a.tipo_solicitud = vtipo_solicitud
       AND b.tipo_identif   = a.clave_identif
       AND a.modulo_cod = b.modulo_cod
       AND a.modulo_cod = l_modulo

    IF STATUS != NOTFOUND THEN
        LET va = 2
        LET clave       = re_g.clave
        LET descripcion = re_g.descripcion
        LET identifica  = re_g.identifica
    ELSE
        INITIALIZE re_g.* TO NULL
    END IF

    OPEN WINDOW v11 AT 6,6 WITH FORM "SEPM0019" ATTRIBUTE(BORDER)

    DISPLAY " [ Esc ] Grabar        [ Ctrl_C ] Salir                             "
            AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY "                     I D E N T I F I C A C I O N                    "
           AT 2,1 ATTRIBUTE(REVERSE)

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
            CALL clave_i(vtipo_solicitud) 
              RETURNING re_g.clave,re_g.descripcion

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
            AND    b.modulo_cod = l_modulo

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR "Codigo Incorrecto, Verifique nuevamente"
                NEXT FIELD clave
            END IF

            DISPLAY re_g.descripcion TO descripcion 
        END IF


    ON KEY(CONTROL-C)
        EXIT INPUT

    ON KEY(INTERRUPT, CONTROL - C, CONTROL - Z)
        EXIT INPUT

    END INPUT

    CLOSE WINDOW v11

END FUNCTION

FUNCTION clave_i(l_tipo_solicitud)
#ci---------------
    DEFINE l_tipo_solicitud SMALLINT
    DEFINE c ARRAY[50] OF RECORD
        codigo      SMALLINT,
        descripcion CHAR(50)
    END RECORD
   DEFINE l_modulo CHAR(003)
    DEFINE i SMALLINT

    CASE l_tipo_solicitud 
    WHEN 6
      LET l_modulo = "AFI"
     EXIT CASE
    WHEN 7
      LET l_modulo = "TAA"
     EXIT CASE
    OTHERWISE
     EXIT CASE
    END CASE

    DECLARE cursor_cve_i CURSOR FOR 
        SELECT a.*
        FROM   tab_identificacion a
        WHERE a.modulo_cod = l_modulo
      
        ORDER BY 1

        LET i = 1

        FOREACH cursor_cve_i INTO c[i].*
            LET i = i + 1
        END FOREACH

        CALL SET_COUNT(i-1)

    OPEN WINDOW ven_clave AT 9,10 WITH FORM "SEPM00191" ATTRIBUTE(BORDER)

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


FUNCTION inserta_logico()
#il----------------------

    LET g_hora  = TIME
END FUNCTION

FUNCTION observaciones(vn_seguro, vn_folio, vtipo_solicitud)
#o----------------------------------------------------------

    DEFINE
        vn_seguro       CHAR(11),
        vn_folio        INTEGER,
        vtipo_solicitud SMALLINT

    DEFINE re_g RECORD
        observacion CHAR(260)
    END RECORD

    DEFINE
        observacion CHAR(260),
        va          ,
        ban         SMALLINT

    LET va  = 0
    LET ban = 0

    SELECT a.observacion
      INTO observacion
      FROM afi_ctr_observa a
     WHERE a.nss            = vn_seguro
       AND a.n_folio        = vn_folio
       AND a.tipo_solicitud = vtipo_solicitud

    IF SQLCA.SQLCODE = 0 THEN
        LET va = 2
    END IF

    OPEN WINDOW v101 AT 6,5 WITH FORM "SEPM00101" ATTRIBUTE(BORDER)

    DISPLAY " [ Enter ] Grabar        [ Ctrl_C ] Salir                                      "
            AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY "                     O B S E R V A C I O N E S                                 "
           AT 2,1 ATTRIBUTE(REVERSE)

    IF va = 2 THEN
        DISPLAY BY NAME observacion
    END IF

    INPUT BY NAME observacion WITHOUT DEFAULTS

    AFTER FIELD observacion
        SELECT "a.X"
          FROM afi_ctr_observa a
         WHERE a.nss     = vn_seguro
           AND a.n_folio = vn_folio
           AND a.tipo_solicitud = vtipo_solicitud
    EXIT INPUT

    ON KEY(CONTROL-C)
        EXIT INPUT

    ON KEY(INTERRUPT, CONTROL - C, CONTROL - Z)
        EXIT INPUT

    END INPUT

    CLOSE WINDOW v101

END FUNCTION

FUNCTION observaciones_c(vn_seguro, vn_folio, vtipo_solicitud)
#oc-----------------------------------------------------------

    DEFINE
        vn_seguro       CHAR(11),
        vn_folio        INTEGER,
        vtipo_solicitud SMALLINT

    DEFINE re_g RECORD
        observacion CHAR(260)
    END RECORD

    DEFINE
        observacion CHAR(260),
        va          ,
        ban         SMALLINT

    LET va  = 0
    LET ban = 0

    SELECT a.observacion
      INTO observacion
      FROM afi_ctr_observa a
     WHERE a.nss            = vn_seguro
       AND a.n_folio        = vn_folio
       AND a.tipo_solicitud = vtipo_solicitud

    IF SQLCA.SQLCODE <> 0 THEN
        ERROR " NO EXISTE INFORMACION PARA ESTE NSS"
        SLEEP 3
        ERROR ""
	CLOSE WINDOW ventana_1
        RETURN
    END IF

    IF SQLCA.SQLCODE = 0 THEN
        LET va = 2
    END IF

    OPEN WINDOW v101 AT 6,5 WITH FORM "SEPM00101" ATTRIBUTE(BORDER)

    DISPLAY " [ Esc ] Grabar        [ Ctrl_C ] Salir                                        "
            AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY "                     O B S E R V A C I O N E S                                 "
           AT 2,1 ATTRIBUTE(REVERSE)

    DISPLAY BY NAME observacion

    PROMPT "PRESIONE < ENTER > PARA CONTINUAR " FOR va
    CLOSE WINDOW v101

END FUNCTION
