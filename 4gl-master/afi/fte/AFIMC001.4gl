############################################################################
#Proyecto          => Sistema de Afores. (MEXICO)                          #
#Propietario       => E.F.P                                                #
#Programa AFIM001  => MANTENIMIENTO DE SOLICITUDES DE AFILIACION           #
#Sistema           => AFI.                                                 #
#Autor             => MAURO MUNIZ CABALLERO                                #
#Fecha             => 28 de noviembre de 2000.                             #
#Modificado        => FERNANDO HERRERA HERNANDEZ                           #
#Fecha             => 23 de julio de 2004.                                 #
############################################################################

DATABASE safre_af

GLOBALS

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
        fol_prob   DECIMAL(10,0),
        doc_prob   DECIMAL(16,0)

    DEFINE
        sexo_cur   CHAR(1)  ,
        c_pat      CHAR(1)  ,
        c_mat      CHAR(1)  ,
        c_nom      CHAR(1)  ,
        c_fen      CHAR(1)  ,
        c_doc      CHAR(1)  ,
        enter      CHAR(1)  ,
        aux_pausa  CHAR(1)  ,
        aux_sup    CHAR(1)  ,
        ACCION     CHAR(1)  ,
        cod_err    CHAR(4)  ,
        g_usuario  CHAR(8)  ,
        g_hora     CHAR(8)  ,
        x_fecha    CHAR(10) ,
        xn_fena    CHAR(10) ,
        desc_solic CHAR(15) ,
        operacion  CHAR(40) ,
        pat        CHAR(40) ,
        mat        CHAR(40) ,
        nom        CHAR(40) ,
        comma      CHAR(200),
        comando    CHAR(250), 
 # issa --> No estan en la forma
        g_folio_edo_cta       CHAR(8)       ,
        g_edo_civil           SMALLINT      ,
        g_desc_edo_civil      CHAR(60)      ,
        g_salario_base_comis  DECIMAL(12,2) ,
        g_salario_actual      DECIMAL(12,2) ,
        g_n_operac            INTEGER       
 # <-----

    DEFINE
        hoy             DATE,
        fecha_comprueba DATE,
        x_fecha_cambio  DATE,
        x_finicta       DATE,
        x_asigna        DATE,
        xx_fecha        DATE,
        xn_fecha        DATE

    DEFINE r_afi            RECORD
        tipo_solicitud      SMALLINT      ,
        desc_solicitud      CHAR(30)      ,
        n_folio         DECIMAL(10,0)       ,
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
        profesion_cod       SMALLINT      ,
        profesion_desc      CHAR(50)      ,
        actividad_cod       SMALLINT      ,
        actividad_desc      CHAR(50)      ,
        ind_infonavit       CHAR(1)       ,
        desc_ind_info       CHAR(25)      ,
        fecha_emision       DATE          ,
        fecha_elaboracion   DATE          ,
        frecafor            DATE          ,
        fecha_cap           DATE          ,
        cod_afore_ced       SMALLINT      ,
        desc_afore          CHAR(50)      ,
        tip_prob            CHAR(1)       ,
        docprob_desc        CHAR(20)      ,
        fol_prob            CHAR(10)      ,
        doc_prob            CHAR(16)      ,
        cod_error_orig      SMALLINT      ,
        usuario1            CHAR(8)       , 
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

    DEFINE v_salario_actual	LIKE afi_solicitud.salario_actual
    DEFINE dig_curp		SMALLINT
    DEFINE pasa			SMALLINT
    DEFINE pasa_curp		SMALLINT
    DEFINE desc_err		CHAR(60)

END GLOBALS

MAIN

    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY CONTROL-I,
        COMMENT LINE LAST
        DEFER INTERRUPT

    CALL STARTLOG("AFIM001.log")
    CALL inicio()            #i
    CALL proceso_principal() #pp

END MAIN

FUNCTION inicio()
#i---------------
 
    LET hoy = TODAY

    SELECT *, USER 
    INTO   g_afore.*, g_usuario 
    FROM   tab_afore
    WHERE  marca = 1

    LET pasa       = 0
    LET pasa_curp  = 0

    CREATE TEMP TABLE tmp_mot_rch
       (codigo		SMALLINT,
        descripcion	CHAR(80),
	cve_afore	SMALLINT,
	desc_afore	CHAR(14),
	fech_rech	DATE,
	n_pcanase	CHAR(50))

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIM0011" ATTRIBUTE(BORDER)
    DISPLAY " AFIM001             MANTENIMIENTO SOLICITUDES AFILIACION                      " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                               " AT 5,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                               " AT 13,1 ATTRIBUTE(REVERSE)
    DISPLAY "                          Estructura Comercial                                 " AT 18,1 ATTRIBUTE(REVERSE)
    DISPLAY hoy USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "Sol"
        COMMAND "Consulta" "Consulta Solicitud "
            LET ACCION = "C"
            CALL Consulta()
            CALL Inicializa()
        COMMAND "Salir" "Salir de Programa"
            EXIT MENU
    END MENU

END FUNCTION

FUNCTION Inicializa()
#iz------------------

    INITIALIZE r_afi.* TO NULL
 # issa --> 
    INITIALIZE   g_folio_edo_cta  TO NULL    
    INITIALIZE   g_edo_civil TO NULL     
    LET g_edo_civil             = 0        
    LET g_salario_base_comis    = 0 
    LET g_salario_actual	    = 0  
    LET g_n_operac              = 0 
 # <-- 
    INITIALIZE g_comision.* TO NULL
    INITIALIZE cod_err TO NULL
    LET r_afi.usuario1              = g_usuario

    DISPLAY "                                        " AT 5,56 ATTRIBUTE(REVERSE)

    # issa DISPLAY "            " AT 8,15
    DISPLAY "                                                                               " AT 13,1 ATTRIBUTE(REVERSE)

    CLEAR FORM

END FUNCTION

FUNCTION Agrega()
#A---------------

    DEFINE
        sexo_cur CHAR(1),
        opc      CHAR(1),
        vcontrol CHAR(1),
        aaa      CHAR(2),
        mm       CHAR(2),
        dd       CHAR(2),
        z_fecha  CHAR(10),
        e_fecha  CHAR(10),
        codprom  CHAR(10),
        ll_calle CHAR(30),
        val_1    CHAR(80),
        doc_prob_arma   CHAR(16),
        rfc_arma   CHAR(10)

    DEFINE
        d_fecha         DATE,
        j_fecha         DATE,
        fecha_comprueba DATE

    DEFINE
        x_pro     DECIMAL(10,0),
        x_ref     CHAR(10)

    DEFINE
        l_nivel_prom  SMALLINT,
        recha         SMALLINT,
        estado_cuenta SMALLINT,
        cod_origen    SMALLINT,
        v_1           SMALLINT,
        a_yo_act      SMALLINT,
        a_yo_fena     SMALLINT,
        a_yo          SMALLINT,
        bla           SMALLINT,
        ban           SMALLINT,
        sino          SMALLINT,
        bnd_fena      SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    ATTRIBUTE(reverse)
    DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE)
    DISPLAY "[ Esc ] Graba  [ Ctrl-C ] Salir sin Grabar " AT 1,1

    DISPLAY "CTRL:[E]Dom. [T]Tels [B]Benef [V]Patrones [P]Icefas [W] Observa [Y] Ident      " 
            AT 2,1 ATTRIBUTE(BOLD)

    LET sw_1       = 0
    LET vcont      = 0
    LET vcontrol   = ""
    LET tot_sief   = 0
    LET sexo_cur   = " "
    LET cod_origen = 0
    LET bnd_fena   = 0


    INITIALIZE doc_prob_arma TO NULL

    INPUT BY NAME r_afi.*

-----> 
# TIPO SOLICITUD

      AFTER FIELD tipo_solicitud
      LET r_afi.usuario1              = g_usuario
      DISPLAY BY NAME r_afi.usuario1   

        IF r_afi.tipo_solicitud IS NULL THEN
            ERROR "Tipo de Solicitud NO puede ser NULO"
            NEXT FIELD tipo_solicitud
        END IF



 # issa tab_tipo_solic
      
        IF r_afi.tipo_solicitud > 2  OR 
           r_afi.tipo_solicitud < 1 THEN 
           ERROR "Tipo de Solicitud solo puede ser 1 o 2"
           NEXT FIELD tipo_solicitud
        END IF
       
        SELECT desc_solicitud 
        INTO  r_afi.desc_solicitud 
        FROM tab_tipo_solic
        WHERE tipo_solicitud = r_afi.tipo_solicitud
 
        DISPLAY BY NAME  r_afi.desc_solicitud
# <---


        IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
           FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
           NEXT FIELD n_folio
        END IF

        NEXT FIELD n_folio

        BEFORE FIELD n_folio
            IF vcontrol = "S" THEN
                NEXT FIELD paterno
            END IF 

# FOLIO SOLIC

        AFTER FIELD n_folio

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD tipo_solicitud 
            END IF

            IF r_afi.n_folio IS NULL THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD n_folio
            ELSE
                SELECT "X"
                FROM   afi_mae_afiliado
                WHERE  n_folio        = r_afi.n_folio
                AND    tipo_solicitud = r_afi.tipo_solicitud

                IF SQLCA.SQLCODE = 0 THEN
                    PROMPT "Folio ya Existe en Maestro de Afiliados, [Enter] p/s                    alir"
                    FOR enter
                    RETURN
                END IF

                SELECT "X"
                FROM   afi_solicitud
                WHERE  n_folio = r_afi.n_folio
                AND    tipo_solicitud = r_afi.tipo_solicitud

                IF SQLCA.SQLCODE = 0 THEN
                    PROMPT "Folio ya Existe en solicitudes de afiliacion, [Enter] p/salir"
                    FOR enter
                    RETURN
                END IF

                SELECT "X"
                FROM   afi_mae_modifica
                WHERE  folio_nvo      = r_afi.n_folio
                AND    tipo_solicitud = r_afi.tipo_solicitud
                AND    cod_operacion  = 0
                AND    diag_proceso   = 0
                IF SQLCA.SQLCODE = 0 THEN
                    PROMPT "Folio ya Existe en Maestro de Modificados, [Enter] p/salir"                           
                    FOR enter
                    RETURN
                END IF
            END IF
            #### Val_fol

        #Verifica existencia en control documental
            SELECT 'X'
            FROM   afi_recepcion arec
            WHERE  arec.n_folio        = r_afi.n_folio
            AND    arec.tipo_solicitud = r_afi.tipo_solicitud
            AND    arec.estado_sol     = 0
            AND    arec.estado_exp     = 0

            IF SQLCA.SQLCODE = 0 THEN
                SELECT afrec.n_seguro,
                       afrec.fecha_solicitud,
                       afrec.fecha_recepcion,
                       afrec.cod_promotor
                INTO   r_afi.n_seguro,
                       r_afi.fecha_elaboracion,
                       r_afi.frecafor,
                       r_afi.cod_promotor
                FROM   afi_recepcion afrec
                WHERE  afrec.n_folio        = r_afi.n_folio
                AND    afrec.tipo_solicitud = r_afi.tipo_solicitud

# issa mueve frecafor a fecha de captura 
                LET r_afi.fecha_cap = hoy 

                DISPLAY BY NAME r_afi.n_seguro,
                                r_afi.fecha_elaboracion,
                                r_afi.fecha_cap,
                                r_afi.frecafor

          SELECT A.paterno,
                 A.materno,
                 A.nombres
          INTO   pat,
                 mat,
                 nom
          FROM   pro_mae_promotor A
          WHERE  A.cod_promotor = r_afi.cod_promotor

          IF STATUS = NOTFOUND THEN
              ERROR "Promotor Inexistente"
              NEXT FIELD cod_promotor
          ELSE
              LET r_afi.nom_promotor = pat CLIPPED," ",
                                          mat CLIPPED," ",
                                          nom CLIPPED
          END IF

      DISPLAY BY NAME r_afi.cod_promotor,
                      r_afi.nom_promotor

      SELECT A.status
      INTO   recha
      FROM   pro_mae_promotor A
      WHERE  A.cod_promotor = r_afi.cod_promotor

      CASE recha
          WHEN 2 ERROR "PROMOTOR SUSPENDIDO" SLEEP 2
              NEXT FIELD cod_promotor
          WHEN 3 ERROR "PROMOTOR CANCELADO" SLEEP 2
              NEXT FIELD cod_promotor
          OTHERWISE
              ERROR "PROMOTOR ACTIVO" SLEEP 1
      END CASE

      LET r_afi.codven      = r_afi.cod_promotor
      LET r_afi.desc_codven = r_afi.nom_promotor

      DISPLAY BY NAME r_afi.codven, r_afi.desc_codven

              SELECT A.agenc_cod,
                     A.nivel
              INTO   r_afi.agenc_cod,
                     l_nivel_prom
              FROM   pro_mae_promotor A
                WHERE  A.cod_promotor = r_afi.cod_promotor

      SELECT nombre_uni_n1
      INTO   r_afi.agenc_desc
      FROM   com_nivel1
      WHERE  coduni_n1 = r_afi.agenc_cod

      SELECT indicador_comision, ""
      INTO   g_comision.cod_esq_comision, g_comision.desc_esq_comision
      FROM   com_tipo_promotor
      WHERE  cod_tipo_prom = l_nivel_prom

      IF SQLCA.SQLCODE = NOTFOUND THEN
          LET g_comision.cod_esq_comision = 1
      END IF

      DISPLAY BY NAME  r_afi.agenc_cod ,
                       r_afi.agenc_desc

                IF r_afi.tipo_solicitud = 1 THEN
                    NEXT FIELD paterno
                END IF
            ELSE
                PROMPT "Solicitud no capturada en control documental, ",
                       "[Enter] p/salir " ATTRIBUTES(REVERSE)
                FOR enter
                RETURN
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
               NEXT FIELD paterno
            END IF

--- PATERNO

        BEFORE FIELD paterno
            DISPLAY " PRIMER REGISTRO " AT 5,5 ATTRIBUTE(REVERSE)

           IF r_afi.cod_promotor <> 0 THEN
             DISPLAY " ELECCION DIRECTA        " AT 5,60 ATTRIBUTE(REVERSE)
           ELSE
             DISPLAY " ASIGNACION POR COMISION " AT 5,60 ATTRIBUTE(REVERSE)
           END IF

        AFTER FIELD paterno
	    {IF r_afi.cod_promotor[1,1] = "e" THEN
	       ERROR "Digite Correctamente la clave del promotor "
	       SLEEP 3
	       NEXT FIELD cod_promotor
	    END IF}

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                IF r_afi.paterno IS NULL OR
                   r_afi.paterno = " "   THEN
                   ERROR "No puede pasar al siguiente campo por no tener el A.Paterno"
                   NEXT FIELD paterno
                ELSE
                   LET v_1 = 0
                   INITIALIZE val_1 TO NULL
                   CALL verifica_nombre(r_afi.paterno) RETURNING v_1,val_1 #ve--

                   IF v_1 = 1 THEN
                      ERROR "A.Paterno ",val_1 CLIPPED
                      NEXT FIELD paterno
                   END IF

                   NEXT FIELD n_folio
                END IF
                
                NEXT FIELD n_folio
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                IF r_afi.paterno IS NULL OR
                   r_afi.paterno = " "   THEN
                   ERROR "No puede pasar al siguiente campo por no tener el A.Paterno"
                   NEXT FIELD paterno
                ELSE
                   LET v_1 = 0
                   INITIALIZE val_1 TO NULL
                   CALL verifica_nombre(r_afi.paterno) RETURNING v_1,val_1 #ve--
                   IF v_1 = 1 THEN
                      ERROR "A.Paterno ",val_1 CLIPPED
                      NEXT FIELD paterno
                   END IF
                   NEXT FIELD n_folio
                END IF
                NEXT FIELD n_folio
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                IF r_afi.paterno IS NULL OR
                   r_afi.paterno = " "   THEN
                   ERROR "No puede pasar al siguiente campo por no tener el A.Paterno"
                   NEXT FIELD paterno
                ELSE
                   LET v_1 = 0
                   INITIALIZE val_1 TO NULL
                   CALL verifica_nombre(r_afi.paterno) RETURNING v_1,val_1 #ve--

                   IF v_1 = 1 THEN
                      ERROR "A.Paterno ",val_1 CLIPPED
                      NEXT FIELD paterno
                   END IF
                   NEXT FIELD materno
                END IF
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                IF r_afi.paterno IS NULL OR
                   r_afi.paterno = " "   THEN
                   ERROR "No puede pasar al siguiente campo por no tener el A.Paterno"
                   NEXT FIELD paterno
                ELSE
                   LET v_1 = 0
                   INITIALIZE val_1 TO NULL
                   CALL verifica_nombre(r_afi.paterno) RETURNING v_1,val_1 #ve--

                   IF v_1 = 1 THEN
                      ERROR "A.Paterno ",val_1 CLIPPED
                      NEXT FIELD paterno
                   END IF
                   NEXT FIELD materno
                END IF
            END IF

            IF r_afi.paterno IS NULL OR
               r_afi.paterno[1] = " " THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD paterno
            END IF

            LET v_1 = 0
            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.paterno) RETURNING v_1, val_1  #ve--
            IF v_1 = 1 THEN
               ERROR "A.Paterno ",val_1 CLIPPED
               NEXT FIELD paterno
            END IF

 # MATERNO 
        AFTER FIELD materno
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD paterno
            END IF

            IF r_afi.materno[1] = " " THEN
                ERROR "Ingrese Ap. Materno correcto o deje el campo nulo"
                LET r_afi.materno = NULL
                DISPLAY BY NAME r_afi.materno
                NEXT FIELD materno
            END IF

            LET v_1 = 0
	    INITIALIZE val_1 TO NULL
	    CALL verifica_nombre(r_afi.materno) RETURNING v_1, val_1  #ve--

            IF v_1 = 1 THEN
               ERROR "A.Materno ",val_1 CLIPPED
               NEXT FIELD materno
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
               IF r_afi.materno IS NULL THEN
                  NEXT FIELD nombres
               END IF
            END IF

# NOMBRE 
        AFTER FIELD nombres
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                IF r_afi.nombres IS NULL OR
                   r_afi.nombres = " "   THEN
                   ERROR "No puede pasar al siguiente campo por no tener el Nombre"
                   NEXT FIELD nombres
                ELSE
                   LET v_1 = 0
                   INITIALIZE val_1 TO NULL
                   CALL verifica_nombre(r_afi.nombres) RETURNING v_1,val_1 #ve--

                   IF v_1 = 1 THEN
                      ERROR "El Nombre ",val_1 CLIPPED
                      NEXT FIELD nombres
                   END IF

                   IF cod_origen > 0 THEN
                       NEXT FIELD paterno
                   ELSE
                       NEXT FIELD materno
                   END IF
                END IF
            END IF

            LET v_1 = 0
            INITIALIZE val_1 TO NULL
            CALL verifica_nombre(r_afi.nombres) RETURNING v_1, val_1  #ve--

            IF v_1 = 1 THEN
               ERROR "El Nombre ",val_1 CLIPPED
               NEXT FIELD nombres
            END IF

            IF r_afi.nombres IS NULL OR
               r_afi.nombres[1] = " " THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD nombres
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                IF r_afi.nombres IS NULL OR
                   r_afi.nombres = " "   THEN
                   ERROR "No puede pasar al siguiente campo por no tener el Nombre"
                   NEXT FIELD nombres
                ELSE
                   LET v_1 = 0
                   INITIALIZE val_1 TO NULL
                   CALL verifica_nombre(r_afi.nombres) RETURNING v_1,val_1 #ve--

                   IF v_1 = 1 THEN
                      ERROR "El Nombre ",val_1 CLIPPED
                      NEXT FIELD nombres
                   END IF

                   NEXT FIELD fena
                 END IF
            END IF

# FECHA NACIMIENTO

        AFTER FIELD fena
            IF r_afi.fena IS NULL THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD fena
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                IF cod_origen > 0 THEN
                    NEXT FIELD paterno
                ELSE
                    NEXT FIELD nombres 
                END IF
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                IF cod_origen > 0 THEN
                    NEXT FIELD paterno
                ELSE
                    NEXT FIELD nombres
                END IF
            END IF


            LET a_yo_act  = 0 LET a_yo_fena = 0 LET a_yo      = 0

            LET xx_fecha = r_afi.fena

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


# SEXO
        BEFORE FIELD sexo

            CALL arma_clave_rfc(r_afi.paterno,
                                r_afi.materno,
                                r_afi.nombres,
                                r_afi.fena) RETURNING rfc_arma #rac

            IF rfc_arma != r_afi.n_rfc[1,10] THEN
               IF bnd_fena = 2 THEN
                   ERROR "Existe diferencia en las primeras 10 posic. del RFC" 
                   SLEEP 2
                   ERROR ""
                   NEXT FIELD n_rfc
               END IF
            END IF

        AFTER FIELD sexo
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               IF r_afi.sexo IS NULL OR r_afi.sexo = " " OR
                  r_afi.sexo = 0 THEN
                  ERROR "Digite correctamente el sexo antes de pasar a otro campo"
                  NEXT FIELD sexo
               ELSE
                  IF cod_origen > 0 THEN
                     NEXT FIELD paterno
                  ELSE
                     NEXT FIELD fena 
                  END IF
               END IF
            END IF


            IF r_afi.sexo IS NULL OR r_afi.sexo = " " OR
               r_afi.sexo = 0 THEN
                CALL Despliega_sexos() RETURNING r_afi.sexo,
                                                 r_afi.desc_sexo
                IF r_afi.sexo IS NULL OR r_afi.sexo = " " OR
                   r_afi.sexo = 0 THEN
                   ERROR "Digite correctamente el Sexo "
                   NEXT FIELD sexo
                END IF
            ELSE
                SELECT sexo_desc
                INTO   r_afi.desc_sexo
                FROM   safre_af:tab_sexo
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
                  ERROR "Digite correctamente el sexo antes de pasar a otro campo"
                  NEXT FIELD sexo
               ELSE
                  NEXT FIELD n_unico
               END IF
            END IF


# CURP
        AFTER FIELD n_unico
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") OR
               FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD sexo
            END IF

         IF r_afi.n_unico IS NOT NULL OR
            r_afi.n_unico <> " " THEN

            IF LENGTH(r_afi.n_unico) < 18 AND
               LENGTH(r_afi.n_unico) > 0  THEN
                ERROR "Debe ingresar CURP completa"
                NEXT FIELD n_unico 
            ELSE
                IF r_afi.n_unico[1] <> " " OR
                   r_afi.n_unico IS NOT NULL THEN
                    IF r_afi.n_unico[11] = "H" THEN
                        LET sexo_cur = "1"
                    ELSE
                        LET sexo_cur = "2"
                    END IF

                    CALL valida_est_curp(r_afi.n_unico)
                    RETURNING pasa_curp, desc_err
                    IF pasa_curp = 1 THEN
                       ERROR "", desc_err
                       LET pasa_curp = 0
                       NEXT FIELD n_unico
                    END IF

                    CALL var_dig_curp(r_afi.n_unico) RETURNING pasa, dig_curp
                    IF pasa = 0 THEN
                      ERROR "Digito Verificador Invalido curp, el digito es : ",
                      dig_curp
                      LET pasa = 0
                      NEXT FIELD n_unico
                    END IF
                ELSE
                    LET sexo_cur = " "
                END IF
            END IF

            IF r_afi.n_unico[1] = " " THEN
                ERROR "Debe ingresar CURP correcta"
                NEXT FIELD n_unico
            END IF

# agregue validacion del sexo

            IF sexo_cur = '1' OR sexo_cur = '2' THEN
              IF sexo_cur <> r_afi.sexo THEN
                WHILE TRUE
                    PROMPT "Sexo diferente en CURP y sexo, es correcto ¿[S/N]? "
                    FOR enter
                    IF enter MATCHES "[Ss/Nn]" THEN
                        IF enter MATCHES "[Ss]" THEN
                            NEXT FIELD ind_infonavit
                            EXIT WHILE
                        ELSE
                            NEXT FIELD sexo
                        END IF
                    ELSE
                        ERROR "Solo debe presionar (S) Si o (N) No"
                        SLEEP 3
                        ERROR ""
                    END IF
                END WHILE
              END IF
            END IF

         END IF

        IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
           FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
           NEXT FIELD n_rfc 
        END IF
# NSS
       BEFORE FIELD n_seguro
            IF vcontrol='S' THEN
                NEXT FIELD n_rfc
            END IF

        AFTER FIELD n_seguro
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD n_unico
            END IF


            IF r_afi.n_seguro IS NULL THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD n_seguro
            END IF

            IF LENGTH(r_afi.n_seguro) <> 11 THEN
                ERROR "Debe ingresar N.S.S. completo"
                NEXT FIELD n_seguro
            END IF

            SELECT "X"
            FROM   afi_mae_afiliado
            WHERE  n_seguro = r_afi.n_seguro
            AND    tipo_solicitud <> 5

            IF SQLCA.SQLCODE = 0 THEN
                SELECT 'X'
                FROM   cta_act_marca a
                WHERE  a.nss = r_afi.n_seguro
                AND    a.marca_cod IN(SELECT b.marca_resulta
                                      FROM   tab_marca b
                                      WHERE  b.ind_habilita = 1)

                IF SQLCA.SQLCODE <> 0 THEN
                    WHILE TRUE
                    PROMPT "NSS ya Ingresado en Maestro de Afiliados, ",
                           "DESEA SEGUIR LA CAPTURA ¿S/N? "
                    ATTRIBUTES(reverse)
                    FOR aux_pausa

                    IF aux_pausa MATCHES "[SsNn]" THEN
                        IF aux_pausa MATCHES "[Nn]" THEN
                            RETURN
                        ELSE
                            EXIT WHILE
                        END IF
                    ELSE
                        DISPLAY "Solo debe presionar (S) Si o (N) No" AT 19,2
                    END IF
                    END WHILE
                END IF
            END IF

            SELECT "X"
            FROM   afi_solicitud
            WHERE  n_seguro = r_afi.n_seguro
            AND    n_folio  = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud

            IF STATUS <> NOTFOUND THEN
                ERROR "NSS ya Ingresado en solicitudes de afiliacion"
                 NEXT FIELD n_seguro
            END IF

            SELECT count(*)
            INTO   xx
            FROM   afi_solicitud
            WHERE  n_seguro = r_afi.n_seguro

            IF xx IS NULL THEN
                LET xx = 0
            END IF

            IF xx <> 0 THEN
                CALL despliega_ing()

                WHILE TRUE
                    PROMPT "DESEA SEGUIR LA CAPTURA S/N ? "
                    FOR aux_pausa

                    IF aux_pausa MATCHES "[SsNn]" THEN
                        IF aux_pausa MATCHES "[Nn]" THEN
                            CLOSE WINDOW AFIM0017
                            RETURN
                        ELSE
                            CLOSE WINDOW AFIM0017
                            EXIT WHILE
                        END IF
                    ELSE
                        DISPLAY "Solo debe presionar (S) Si o (N) No" AT 19,2
                        END IF
                END WHILE
            END IF 
            LET r_afi.n_seguro = r_afi.n_seguro
          
            CALL  digito_verif(r_afi.n_seguro[1,10],10) RETURNING digito

            IF digito = 32000 THEN
                ERROR "N.S.S. solo contiene digitos"
                NEXT FIELD n_seguro
            END IF

            IF LENGTH(r_afi.n_seguro) = 11 AND
               digito <> r_afi.n_seguro[11] THEN
            ERROR "Digito Verificador Invalido, el digito debe ser:  ",digito
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
           LET r_afi.desc_solicitud = "AFIG MISMA AFO"
           # issa DISPLAY "ASIG MISMA AFO" AT 8,14 
           DISPLAY r_afi.desc_solicitud 

         # issa   DISPLAY "F.Asigna. ",x_asigna USING "dd-mm-yyyy" 
         #  AT 16,5 ATTRIBUTE(REVERSE)
       END IF   
                

        IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
           FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
           NEXT FIELD n_rfc 
        END IF

# RFC

        AFTER FIELD n_rfc
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               LET v_1 = 0
               INITIALIZE val_1 TO NULL
               CALL verifica_rfc(r_afi.n_rfc[1,4])
                                    RETURNING v_1,val_1 #ve--
               IF v_1 = 1 THEN
                  ERROR "R.F.C. ",val_1 CLIPPED
                  NEXT FIELD n_rfc
               END IF
               NEXT FIELD n_unico
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               LET v_1 = 0
               INITIALIZE val_1 TO NULL
               CALL verifica_rfc(r_afi.n_rfc[1,4])
                                    RETURNING v_1,val_1 #ve--
               IF v_1 = 1 THEN
                  ERROR "R.F.C. ",val_1 CLIPPED
                  NEXT FIELD n_rfc
               END IF
               NEXT FIELD n_unico
            END IF

            IF r_afi.n_rfc IS NULL THEN
                ERROR "Campo NO puede ser NULO"
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
                LET aaa = r_afi.n_rfc[5,6]
                LET mm = r_afi.n_rfc[7,8]
                LET dd = r_afi.n_rfc[9,10]
                LET z_fecha = mm,"/",dd,"/19",aaa
                LET j_fecha = z_fecha
                WHENEVER ERROR STOP

                IF j_fecha IS NULL THEN
                    ERROR "Fecha Invalida en RFC"
                    NEXT FIELD n_rfc
                END IF
            END IF

        IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
           FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
           LET v_1 = 0
           INITIALIZE val_1 TO NULL
           CALL verifica_rfc(r_afi.n_rfc[1,4])
                                RETURNING v_1,val_1 #ve--
           IF v_1 = 1 THEN
              ERROR "R.F.C. ",val_1 CLIPPED
              NEXT FIELD n_rfc
           END IF
           NEXT FIELD estadon   
        END IF

        IF r_afi.n_rfc IS NOT NULL THEN
           LET v_1 = 0
           INITIALIZE val_1 TO NULL
           CALL verifica_rfc(r_afi.n_rfc[1,4])
                                RETURNING v_1,val_1 #ve--
           IF v_1 = 1 THEN
              ERROR "R.F.C. ",val_1 CLIPPED
              NEXT FIELD n_rfc
           END IF

# issa agrego validacion de fena 

            INITIALIZE x_fecha, xn_fecha, xn_fena TO NULL

            IF r_afi.n_unico IS NOT NULL THEN
                 LET x_fecha = r_afi.n_unico[7,8], "/",
                               r_afi.n_unico[9,10],"/",
                               "19",
                               r_afi.n_unico[5,6]

                 LET bnd_fena = 1
             ELSE
                 LET x_fecha = r_afi.n_rfc[7,8], "/",
                               r_afi.n_rfc[9,10],"/",
                               "19",
                               r_afi.n_rfc[5,6]

                 LET bnd_fena = 2
            END IF
            LET xn_fecha      = x_fecha
            LET xn_fena       = x_fecha


            IF xn_fecha <> r_afi.fena THEN

                WHILE TRUE
                    PROMPT "Existen inconsistencias en Fecha nacimiento, ",
                           "es correcto ¿[S/N]? "
                    FOR enter
                    IF enter MATCHES "[Ss/Nn]" THEN
                        IF enter MATCHES "[Ss]" THEN
                            LET r_afi.fena = x_fecha
                            DISPLAY BY NAME r_afi.fena
                            NEXT FIELD estadon 
                            EXIT WHILE
                        ELSE
                            IF bnd_fena = 1 THEN
                                NEXT FIELD n_unico
                            ELSE
                                NEXT FIELD fena
                            END IF
                        END IF
                    ELSE
                        ERROR "Solo debe presionar (S)i o (N)o"
                        SLEEP 3
                        ERROR ""
                    END IF
                END WHILE
            END IF


           NEXT FIELD estadon 
         END IF
# ENT NAC 
        AFTER FIELD estadon
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                IF cod_origen > 0 THEN
                    NEXT FIELD paterno
                ELSE
                    NEXT FIELD n_rfc
                END IF
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

            DISPLAY BY NAME r_afi.estadon,r_afi.desc_estadon

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                NEXT FIELD nacionalidad
            END IF

# NACIONALIDAD

        BEFORE FIELD nacionalidad
            IF r_afi.nacionalidad IS NULL THEN
                LET r_afi.nacionalidad = "MEX"
                DISPLAY BY NAME r_afi.nacionalidad
            END IF

        AFTER FIELD nacionalidad
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                IF cod_origen > 0 THEN
                    NEXT FIELD paterno
                ELSE
                    NEXT FIELD estadon 
                END IF
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                IF cod_origen > 0 THEN
                    NEXT FIELD paterno
                ELSE
                    NEXT FIELD estadon
                END IF
            END IF

            IF r_afi.nacionalidad IS NULL OR
               r_afi.nacionalidad =  " "  THEN
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
                IF r_afi.nacionalidad IS NULL OR
                   r_afi.nacionalidad  = " "  THEN
                    NEXT FIELD nacionalidad
                ELSE
                    NEXT FIELD profesion_cod
                END IF
            END IF
----->
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

# INDICADOR DE CREDITO DE VIVIENDA

        BEFORE FIELD ind_infonavit
            LET r_afi.ind_infonavit = "0"
            SELECT @descripcion
              INTO r_afi.desc_ind_info
              FROM safre_af:tab_ind_cred
             WHERE @codigo = r_afi.ind_infonavit
            DISPLAY BY NAME r_afi.ind_infonavit, r_afi.desc_ind_info

        AFTER FIELD ind_infonavit
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                IF cod_origen > 0 THEN
                    NEXT FIELD paterno
                ELSE
                    NEXT FIELD actividad_cod 
                END IF
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                IF cod_origen > 0 THEN
                    NEXT FIELD paterno
                ELSE
                    NEXT FIELD actividad_cod
                END IF
            END IF

            IF r_afi.ind_infonavit IS NULL OR 
               r_afi.ind_infonavit = " "   THEN
               CALL Despliega_ind_info() RETURNING r_afi.ind_infonavit,
                                                   r_afi.desc_ind_info
                IF r_afi.ind_infonavit IS NULL OR
                   r_afi.ind_infonavit = " "   THEN
                   ERROR "Se requiere el indicativo de credito"
                   NEXT FIELD ind_infonavit
                END IF
            ELSE
                SELECT @descripcion
                  INTO r_afi.desc_ind_info
                  FROM safre_af:tab_ind_cred
                 WHERE @codigo = r_afi.ind_infonavit
                DISPLAY BY NAME r_afi.desc_ind_info
            END IF

            IF r_afi.ind_infonavit NOT MATCHES "[0123]" THEN
               CALL Despliega_ind_info() RETURNING r_afi.ind_infonavit,
                                                   r_afi.desc_ind_info
               IF r_afi.ind_infonavit NOT MATCHES "[0123]" THEN 
                  ERROR "(0)Sin cred (1)Con cred INFONAVIT (2)Con cred 43 BIS ",
                        "(3)Con cred FOVISSSTE"
                  NEXT FIELD ind_infonavit
               ELSE
                  SELECT @descripcion
                    INTO r_afi.desc_ind_info
                    FROM safre_af:tab_ind_cred
                   WHERE @codigo = r_afi.ind_infonavit
                  DISPLAY BY NAME r_afi.desc_ind_info
               END IF
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                NEXT FIELD fecha_elaboracion
            END IF

            IF r_afi.tipo_solicitud = 1 THEN
                IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
                   FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                    NEXT FIELD tip_prob
                END IF
            ELSE
                IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
                   FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                    NEXT FIELD fecha_emision
                END IF
            END IF

            IF r_afi.tipo_solicitud = 2 THEN
                NEXT FIELD fecha_emision
            ELSE
                NEXT FIELD tip_prob
            END IF

# FECHA VENCIMIENTO

    AFTER FIELD fecha_emision
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
           FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
            NEXT FIELD ind_infonavit
        END IF


        IF r_afi.fecha_emision IS NULL THEN
           ERROR "Fecha Vencimiento Estructura de Comision NO puede ser NULO."
            NEXT FIELD fecha_emision
        ELSE
       
        # Valida con fecha de Recepcion
        LET fecha_comprueba = r_afi.fecha_emision + 30 UNITS DAY
        # compara contra frecafor(hoy)
             {
               IF r_afi.fecha_emision < r_afi.frecafor THEN
                  ERROR "F. Ven. No puede ser Menor a F. Rec."
                  NEXT FIELD fecha_emision
               END IF
             }
               
               IF r_afi.frecafor      >  fecha_comprueba THEN
                  WHILE TRUE
                     PROMPT "Dcto estrc comis ya vencio, ",
                            "¿seguir la captura [S/N]? "
                     ATTRIBUTES (REVERSE) FOR aux_pausa
                      
                     IF aux_pausa MATCHES "[SsNn]" THEN  
                        IF aux_pausa MATCHES "[Nn]" THEN
                           RETURN
                        ELSE
                           EXIT WHILE
                        END IF
                     ELSE
                        ERROR "Solo debe presionar (S)i o (N)o"
                        SLEEP 2
                        ERROR ""
                     END IF
                  END WHILE
               END IF
 
        END IF

        IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
           FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
           NEXT FIELD fecha_elaboracion 
        END IF

        NEXT FIELD fecha_elaboracion 


# FECHA FIRMA 

        BEFORE FIELD fecha_elaboracion
           { issa pregunta a FER
            IF r_afi.fecha_elaboracion <> '' THEN
                IF r_afi.tipo_solicitud <> 2 THEN
                    NEXT FIELD fecha_emision
                ELSE
                  NEXT FIELD cod_afore_ced
                END IF
            END IF}

        AFTER FIELD fecha_elaboracion
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD fecha_emision
            END IF


            IF r_afi.fecha_elaboracion IS NULL THEN
                ERROR "Fecha Firma NO puede ser NULA"
                NEXT FIELD fecha_elaboracion
            END IF

            IF r_afi.fecha_elaboracion < "01/01/1997" OR
               r_afi.fecha_elaboracion > hoy THEN
                ERROR "Fecha Firma erronea"
                -- LET r_afi.fecha_elaboracion = NULL
                NEXT FIELD fecha_elaboracion
            END IF  


      IF r_afi.tipo_solicitud <> 2 THEN
          LET g_folio_edo_cta = NULL
          --NEXT FIELD fecha_emision
      ELSE
          NEXT FIELD cod_afore_ced
      END IF

      IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
          FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
          NEXT FIELD frecafor 
      END IF

          NEXT FIELD frecafor 

# FECHA F. REC

        BEFORE FIELD frecafor
           -- LET r_afi.frecafor = hoy
            DISPLAY BY NAME r_afi.frecafor


        AFTER FIELD frecafor

             IF FGL_LASTKEY() = FGL_KEYVAL("LEFT")  THEN
               --IF r_afi.tipo_solicitud = 2 THEN
               --    NEXT FIELD cod_afore_ced
               -- ELSE
                   NEXT FIELD fecha_elaboracion
               -- END IF
             END IF

             IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                 NEXT FIELD fecha_elaboracion
             END IF

            IF r_afi.fecha_elaboracion > r_afi.frecafor THEN
                ERROR "Fecha de firma o fecha recepcion erronea"
                SLEEP 2
               --LET r_afi.frecafor = NULL
               -- DISPLAY BY NAME r_afi.frecafor
                NEXT FIELD frecafor
                ERROR ""
            END IF

        IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
           FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
           NEXT FIELD fecha_cap
        END IF

           NEXT FIELD fecha_cap
-->
# FECHA CAPTURA 


        AFTER FIELD fecha_cap 
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") OR 
               FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD cod_afore_ced
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
               NEXT FIELD frecafor
            END IF

            NEXT FIELD cod_afore_ced 


# CODIGO AFORE CEDENTE

        AFTER FIELD cod_afore_ced
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") OR 
               FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_emision 
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
               NEXT FIELD tip_prob
            END IF


# TIPO DE DOCUMENTO PROBATORIO

        BEFORE FIELD tip_prob
            IF r_afi.n_unico IS NOT NULL AND
               r_afi.n_unico <> '                  ' THEN
                LET r_afi.tip_prob = "5"
            END IF

            DISPLAY BY NAME r_afi.tip_prob

        AFTER FIELD tip_prob
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                IF cod_origen > 0 THEN
                    NEXT FIELD paterno
                ELSE
                    -- issa NEXT FIELD edo_civil
                    NEXT FIELD cod_afore_ced
                END IF
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                IF cod_origen > 0 THEN
                    NEXT FIELD paterno
                ELSE
                    NEXT FIELD cod_afore_ced
                    -- issa NEXT FIELD salario_base_comis
                END IF
            END IF

            IF r_afi.tip_prob < 1 OR
               r_afi.tip_prob = 6 OR
               r_afi.tip_prob > 7 THEN
                ERROR "Tipo documento probatorio no valido"
                LET r_afi.tip_prob = ''

                DISPLAY BY NAME r_afi.tip_prob
                NEXT FIELD tip_prob
                ERROR ""
            END IF

            IF r_afi.tip_prob IS NULL OR r_afi.tip_prob = " " OR
               r_afi.tip_prob = 0 THEN
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
               NEXT FIELD fol_prob
            END IF
----> 
# FOLIO 

        BEFORE FIELD fol_prob
            DISPLAY BY NAME r_afi.tip_prob

            IF r_afi.tip_prob <> 6 THEN
                LET r_afi.fol_prob = r_afi.n_folio USING '&&&&&&&&&&'
            ELSE
                LET r_afi.fol_prob = NULL
            END IF

            DISPLAY BY NAME r_afi.fol_prob

        AFTER FIELD fol_prob
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                IF cod_origen > 0 THEN
                    NEXT FIELD paterno
                ELSE
                    NEXT FIELD cod_afore_ced 
                END IF
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                IF cod_origen > 0 THEN
                    NEXT FIELD paterno
                ELSE
                    NEXT FIELD tip_prob
                END IF
            END IF

            LET fol_prob          = r_afi.fol_prob 
            LET r_afi.fol_prob = fol_prob USING "&&&&&&&&&&"
           
            DISPLAY BY NAME fol_prob

            IF r_afi.fol_prob IS NULL AND
               r_afi.tip_prob <> 5 THEN
                ERROR "Numero de Folio NO puede ser NULO"
                NEXT FIELD fol_prob
            END IF

            IF fol_prob IS NULL THEN
                ERROR "Numero de Folio NO puede ser NULO"
                NEXT FIELD fol_prob
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR 
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN 
               NEXT FIELD doc_prob
            END IF

# DOCUMENTO PROBATORIO COD

        BEFORE FIELD doc_prob
            LET b_nac = 0

            IF r_afi.tip_prob = 5 THEN
                LET r_afi.doc_prob = r_afi.n_unico[1,16]
            END IF


            CALL arma_clave(r_afi.paterno,
                            r_afi.materno,
                            r_afi.nombres,
                            r_afi.fena,
                            r_afi.estadon,
                            r_afi.sexo) RETURNING doc_prob_arma #ac

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

            INITIALIZE x_fecha, xn_fecha, xn_fena TO NULL

            IF r_afi.n_unico IS NOT NULL THEN
                 LET x_fecha = r_afi.n_unico[7,8], "/",
                               r_afi.n_unico[9,10],"/",
                               "19",
                               r_afi.n_unico[5,6]

                 LET bnd_fena = 1
             ELSE
                 LET x_fecha = r_afi.n_rfc[7,8], "/",
                               r_afi.n_rfc[9,10],"/",
                               "19",
                               r_afi.n_rfc[5,6]

                 LET bnd_fena = 2
            END IF
            LET xn_fecha      = x_fecha
            LET xn_fena       = x_fecha


                    CALL valida_anyo_nac() RETURNING sino

                    IF sino THEN
                        ERROR "Año de registro erroneo"
                        SLEEP 2
                        LET r_afi.doc_prob = ''
                        DISPLAY BY NAME r_afi.doc_prob
                        NEXT FIELD doc_prob
                    END IF
                WHEN 2
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
                    IF LENGTH(r_afi.doc_prob) <> 7 THEN
                        ERROR "Documento probatorio erroneo"
                        SLEEP 2
                        LET r_afi.doc_prob = ''
                        DISPLAY BY NAME r_afi.doc_prob
                        NEXT FIELD doc_prob
                    END IF
                    LET r_afi.doc_prob = dcto_3, r_afi.doc_prob
                WHEN 4
                    IF LENGTH(r_afi.doc_prob) <> 9 THEN
                        ERROR "Documento probatorio erroneo"
                        SLEEP 2
                        LET r_afi.doc_prob = ''
                        DISPLAY BY NAME r_afi.doc_prob
                        NEXT FIELD doc_prob
                    END IF
                    LET r_afi.doc_prob = dcto_4, r_afi.doc_prob
                WHEN 5

                    IF LENGTH(r_afi.doc_prob) <> 16 THEN
                        ERROR "Documento probatorio erroneo"
                        SLEEP 2
                        LET r_afi.doc_prob = ''
                        DISPLAY BY NAME r_afi.doc_prob
                        NEXT FIELD doc_prob

                    END IF
                WHEN 7
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
                LET r_afi.doc_prob = ''
                DISPLAY BY NAME doc_prob
                NEXT FIELD doc_prob
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR 
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN 
               NEXT FIELD cod_error_orig
            END IF

            NEXT FIELD cod_error_orig            #ANEXO

# ERROR ORIGEN 

        BEFORE FIELD cod_error_orig
            IF cod_origen > 0 THEN
                LET r_afi.cod_error_orig = cod_origen
            ELSE
                LET r_afi.cod_error_orig = NULL
            END IF

        AFTER FIELD cod_error_orig 
            LET cod_err = r_afi.cod_error_orig USING "&&&&"

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                IF cod_origen > 0 THEN
                    NEXT FIELD paterno
                ELSE
                    NEXT FIELD cod_afore_ced
                END IF
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                IF cod_origen > 0 THEN
                    NEXT FIELD paterno
                ELSE
                    NEXT FIELD cod_afore_ced
                END IF
            END IF

           IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
               NEXT FIELD tipo_solicitud
            END IF
# USUARIO

# CODIGO PROMOTOR lo pongo como comentario

    AFTER FIELD cod_promotor
       IF r_afi.cod_promotor IS NULL THEN
           CALL Despliega_promotores() RETURNING r_afi.cod_promotor,
                                                 r_afi.nom_promotor
       ELSE
           WHENEVER ERROR CONTINUE
               LET x_pro = r_afi.cod_promotor
           WHENEVER ERROR STOP

          IF x_pro IS NULL THEN
              ERROR "Codigo Agente Promotor solo puede ser numero"
              NEXT FIELD cod_promotor
          END IF

          SELECT A.paterno,
                 A.materno,
                 A.nombres 
          INTO   pat,
                 mat,
                 nom
          FROM   pro_mae_promotor A
          WHERE  A.cod_promotor = r_afi.cod_promotor

          IF STATUS = NOTFOUND THEN
              ERROR "Promotor Inexistente"
              NEXT FIELD cod_promotor
          ELSE
              LET r_afi.nom_promotor = pat CLIPPED," ",
                                          mat CLIPPED," ",
                                          nom CLIPPED 
          END IF
      END IF

      SELECT A.agenc_cod,
             A.nivel  
      INTO   r_afi.agenc_cod,
             l_nivel_prom 
      FROM   pro_mae_promotor A
      WHERE  A.cod_promotor = r_afi.cod_promotor

      SELECT nombre_uni_n1
      INTO   r_afi.agenc_desc
      FROM   com_nivel1
      WHERE  coduni_n1 = r_afi.agenc_cod

      SELECT indicador_comision, ""
      INTO   g_comision.cod_esq_comision, g_comision.desc_esq_comision
      FROM   com_tipo_promotor
      WHERE  cod_tipo_prom = l_nivel_prom

      IF SQLCA.SQLCODE = NOTFOUND THEN
          LET g_comision.cod_esq_comision = 1
      END IF

      DISPLAY BY NAME  r_afi.agenc_cod ,
                       r_afi.agenc_desc   
      DISPLAY BY NAME r_afi.cod_promotor, 
                      r_afi.nom_promotor 

      SELECT A.status 
      INTO   recha 
      FROM   pro_mae_promotor A
      WHERE  A.cod_promotor = r_afi.cod_promotor

      CASE recha
          WHEN 2 ERROR "PROMOTOR SUSPENDIDO" SLEEP 2
              NEXT FIELD cod_promotor
          WHEN 3 ERROR "PROMOTOR CANCELADO" SLEEP 2
              NEXT FIELD cod_promotor
          OTHERWISE
              ERROR "PROMOTOR ACTIVO" SLEEP 1
      END CASE

      IF r_afi.cod_promotor <> 0 THEN
          DISPLAY " ELECCION DIRECTA        " AT 5,60 ATTRIBUTE(REVERSE)
      ELSE
          DISPLAY " ASIGNACION POR COMISION " AT 5,60 ATTRIBUTE(REVERSE)
      END IF


      LET r_afi.codven      = r_afi.cod_promotor
      LET r_afi.desc_codven = r_afi.nom_promotor

      DISPLAY BY NAME r_afi.codven, r_afi.desc_codven
      

      -- issa NEXT FIELD tipo_solicitud

      IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
         FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
	 IF r_afi.cod_promotor[1,1] = "e" THEN
	    ERROR "Digite Correctamente la clave del promotor "
	    SLEEP 3
	    NEXT FIELD cod_promotor
	 END IF
         NEXT FIELD tipo_solicitud
      END IF
--->  terminan modificaciones

        ON KEY ( INTERRUPT )
            DISPLAY "                 " AT 5,5 ATTRIBUTE(REVERSE)

            DELETE FROM afi_solicitud
            WHERE  n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            AND    n_seguro = r_afi.n_seguro

            # issa  

            DELETE FROM afi_ctr_actividad
            WHERE  nss     = r_afi.n_seguro
            AND    n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud

            DELETE FROM afi_ctr_observa
            WHERE n_folio        = r_afi.n_folio
            AND   tipo_solicitud = r_afi.tipo_solicitud
            AND   nss            = r_afi.n_seguro

            DELETE FROM afi_ctr_identif
            WHERE  n_seguro       = r_afi.n_seguro
            AND    n_folio        = r_afi.n_folio         
            AND    tipo_solicitud = r_afi.tipo_solicitud


            DELETE FROM afi_domicilio
            WHERE  n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            AND    nss     = r_afi.n_seguro

            DELETE FROM afi_telefono
            WHERE  n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            AND    nss     = r_afi.n_seguro

            DELETE FROM afi_beneficiario
            WHERE  n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            AND    n_seguro = r_afi.n_seguro

            DELETE FROM afi_patron
            WHERE  n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            AND    n_seguro = r_afi.n_seguro

            DELETE FROM afi_icefa
            WHERE  n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            AND    n_seguro = r_afi.n_seguro

            CALL Inicializa()

            EXIT INPUT

        ON KEY ( ESC )
            SELECT UNIQUE "X"
            FROM   pro_mae_promotor pmp
            WHERE  pmp.cod_promotor = r_afi.cod_promotor

            IF STATUS = NOTFOUND THEN
                ERROR "Campo Promotor erroneo"
                NEXT FIELD cod_promotor
            END IF

            IF r_afi.cod_promotor IS NULL THEN
                ERROR "Campo Promotor NO puede ser NULO"
                NEXT FIELD codven
            END IF

            IF r_afi.codven IS NULL THEN
                ERROR "Campo Referente NO puede ser NULO"
                NEXT FIELD codven
            END IF

            IF r_afi.agenc_cod IS NULL THEN
                ERROR "Campo Estructura Comercial NO puede ser NULO" 
                NEXT FIELD agenc_cod
            END IF

            IF r_afi.n_folio IS NULL THEN
                ERROR "Campo Folio Solicitud NO puede ser NULO" 
                NEXT FIELD n_folio
            END IF

            IF r_afi.tipo_solicitud IS NULL THEN
                ERROR "Campo Tipo Solicitud NO puede ser NULO" 
                NEXT FIELD tipo_solicitud
            END IF

            IF r_afi.tipo_solicitud = 2 AND
               r_afi.cod_afore_ced IS NULL OR
               r_afi.cod_afore_ced = 0 THEN
               ERROR "Campo Afore Cedente no es valido"
               NEXT FIELD cod_afore_ced
            END IF

            IF r_afi.frecafor IS NULL THEN
                ERROR "Campo Fecha Recepcion NO puede ser NULO" 
                NEXT FIELD frecafor
            END IF

            IF r_afi.fecha_elaboracion IS NULL THEN
                ERROR "Campo Fecha Firma NO puede ser NULO" 
                NEXT FIELD fecha_elaboracion
            END IF

            IF r_afi.paterno IS NULL THEN
                ERROR "Campo Paterno NO puede ser NULO" 
                NEXT FIELD paterno
            END IF

            IF r_afi.nombres IS NULL THEN
                ERROR "Campo Nombres NO puede ser NULO" 
                NEXT FIELD nombres
            END IF

            IF r_afi.n_seguro IS NULL THEN
                ERROR "Campo NSS NO puede ser NULO" 
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
                ERROR "Campo RFC NO puede ser NULO"
                NEXT FIELD n_rfc
            END IF

            IF LENGTH(r_afi.n_rfc) < 10 THEN
                ERROR "Debe ingresar R.F.C. completo"
                NEXT FIELD n_rfc
            END IF

            IF r_afi.sexo IS NULL THEN
                ERROR "Campo Sexo NO puede ser NULO" 
                NEXT FIELD sexo
            END IF

            IF r_afi.fena IS NULL THEN
                ERROR "Campo Fecha Nacimiento NO puede ser NULO" 
                NEXT FIELD fena
            END IF

            IF r_afi.estadon IS NULL THEN
                ERROR "Campo Estado Nacimiento NO puede ser NULO" 
                NEXT FIELD estadon
            END IF

            IF r_afi.tip_prob IS NULL THEN
                ERROR "Campo Tipo Docto. Prob. NO puede ser NULO" 
                NEXT FIELD tip_prob
            END IF

            IF r_afi.fol_prob IS NULL AND
               r_afi.tip_prob <> 5 THEN
                ERROR "Campo Folio Docto. Prob. NO puede ser NULO" 
                NEXT FIELD fol_prob
            END IF

            IF r_afi.doc_prob IS NULL AND
               r_afi.tip_prob <> 5 THEN
                ERROR "Campo Docto. Prob. NO puede ser NULO" 
                NEXT FIELD doc_prob
            END IF

            IF r_afi.ind_infonavit IS NULL THEN
                ERROR "Campo Indicador Credito Infonavit NO puede ser NULO" 
                NEXT FIELD ind_infonavit
            END IF

            SELECT "X"
            FROM   afi_domicilio
            WHERE  n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            AND    nss     = r_afi.n_seguro
            GROUP BY 1

            IF STATUS = NOTFOUND THEN
                ERROR "No puede dar de alta mientras no ingrese domicilio de la solicitud"
                NEXT FIELD tipo_solicitud
            END IF
       
            LET tot_sief = 0


            IF g_comision.cod_esq_comision IS NULL OR
               g_comision.cod_esq_comision < 0  THEN
               ERROR "No puede Agregar una Solicitud sin antes asignar ",
                     "Esquema de Comision"
                NEXT FIELD tipo_solicitud
            ELSE
                UPDATE afi_solicitud 
                SET    cod_esq_comision = g_comision.cod_esq_comision
                WHERE  n_folio          = r_afi.n_folio
                AND    tipo_solicitud   = r_afi.tipo_solicitud
            END IF
            CALL Inserta_en_tabla_afi_solicitud()


            LET su_estatus = 10
            LET operacion  = 'CAPTURA SOLICITUD'

            CALL inserta_logico()

        OPEN WINDOW vent_anilla AT 11,3 WITH 3 ROWS , 75 COLUMNS 
        ATTRIBUTE(BORDER,REVERSE)

        CALL Numero_operacion_asignado()

        CLOSE WINDOW vent_anilla

        DISPLAY "                                                     " AT 5,1 ATTRIBUTE(REVERSE)

        -- issa DISPLAY "              " AT 8,15 

            LET hay_datos1 = 0
            LET hay_datos2 = 0

            SELECT "1"
            INTO   hay_datos1
            FROM   afi_siefore
            WHERE  n_seguro = r_afi.n_seguro
            GROUP BY 1

            SELECT "1"
            INTO   hay_datos2
            FROM   afi_domicilio
            WHERE  nss = r_afi.n_seguro
            AND    n_folio        = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            GROUP BY 1

            IF (hay_datos1+hay_datos2) = 2 THEN
                UPDATE afi_solicitud 
                SET    status_captura = 0
                WHERE  n_seguro       = r_afi.n_seguro
                AND    n_folio        = r_afi.n_folio
                AND    tipo_solicitud = r_afi.tipo_solicitud
            END IF

            INITIALIZE g_comision.* TO NULL
            ERROR "REGISTRO INGRESADO" SLEEP 2 ERROR ""

            CALL Inicializa()

            LET vcontrol=""

            NEXT FIELD tipo_solicitud

        ON KEY (CONTROL-Y)
            CALL n_identif(r_afi.n_seguro, r_afi.n_folio, r_afi.tipo_solicitud)

        ON KEY (CONTROL-W)
           CALL observaciones(r_afi.n_seguro, r_afi.n_folio, r_afi.tipo_solicitud)
        ON KEY ( CONTROL-E,CONTROL-T,CONTROL-B,CONTROL-V,
                 CONTROL-P)   
         WHENEVER ERROR CONTINUE
	    IF r_afi.cod_promotor[1,1] = "e" THEN
	       ERROR "Digite Correctamente la clave del promotor "
	       SLEEP 3
	       NEXT FIELD cod_promotor
	    END IF
            #### Modi #### CHE#
            SELECT UNIQUE 'X'
            FROM safre_af:pro_mae_promotor a
            WHERE a.cod_promotor = r_afi.cod_promotor
            IF STATUS = NOTFOUND THEN
               ERROR "Promotor Inexistente"
               NEXT FIELD cod_promotor
            END IF

            IF r_afi.codven IS NULL THEN
                ERROR "Campo Referente NO puede ser NULO"
                NEXT FIELD codven
            END IF

            IF r_afi.cod_promotor IS NULL THEN
                ERROR "Campo Promotor NO puede ser NULO"
                NEXT FIELD cod_promotor
            END IF

            IF r_afi.agenc_cod IS NULL THEN
                ERROR "Campo Estructura Comercial NO puede ser NULO"
                NEXT FIELD agenc_cod
            END IF

            IF r_afi.n_folio IS NULL THEN
                ERROR "Campo Folio Solicitud NO puede ser NULO"
                NEXT FIELD n_folio
            END IF

            IF r_afi.tipo_solicitud IS NULL THEN
                ERROR "Campo Tipo Registro NO puede ser NULO"
                NEXT FIELD n_folio
            END IF

            LET g_n_operac = 0

            IF g_n_operac IS NULL THEN
                ERROR "Campo Numero de Operacion NO puede ser NULO"
                NEXT FIELD n_operac
            END IF

            IF r_afi.fecha_elaboracion IS NULL THEN
                ERROR "Campo Fecha Elaboracion NO puede ser NULO"
                NEXT FIELD frecafor
            END IF

            IF r_afi.frecafor IS NULL THEN
                ERROR "Campo Fecha Recepcion NO puede ser NULO"
                NEXT FIELD frecafor
            END IF

            IF r_afi.paterno IS NULL THEN
                ERROR "Campo Paterno NO puede ser NULO"
                NEXT FIELD paterno
            END IF

            IF r_afi.nombres IS NULL THEN
                ERROR "Campo Materno NO puede ser NULO"
                NEXT FIELD nombres
            END IF

            IF r_afi.n_seguro IS NULL THEN
                ERROR "Campo NSS NO puede ser NULO"
                NEXT FIELD n_seguro
            END IF

            IF LENGTH(r_afi.n_seguro) <> 11 THEN
                ERROR "Debe ingresar N.S.S. completo"
                NEXT FIELD n_seguro
            END IF

            IF vcontrol<>"S" THEN 
                SELECT "X"
                FROM   afi_solicitud
                WHERE  n_seguro       = r_afi.n_seguro
                AND    n_folio        = r_afi.n_folio
                AND    tipo_solicitud = r_afi.tipo_solicitud

                IF STATUS <> NOTFOUND THEN
                    ERROR "Numero de Seguro Social ya Ingresado"
                    NEXT FIELD n_seguro
                END IF

                SELECT "X" 
                FROM   afi_solicitud
                WHERE  n_seguro = r_afi.n_seguro

                IF STATUS <> NOTFOUND THEN
        ERROR "Numero de Seguro Social ya Ingresado en maestro de Afiliados"
                    NEXT FIELD n_seguro
                END IF
            END IF

            CALL digito_verif(r_afi.n_seguro[1,10],10) RETURNING digito

            IF digito = 32000 THEN 
                ERROR "N.S.S. solo contiene digitos" 
                NEXT FIELD n_seguro 
            END IF

            IF LENGTH(r_afi.n_seguro) = 11 AND 
               digito <> r_afi.n_seguro[11] THEN
                ERROR "Digito Verificador Invalido, el digito es : ", digito
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
                ERROR "Campo RFC NO puede ser NULO"
                NEXT FIELD n_rfc
            END IF

            IF LENGTH(r_afi.n_rfc) < 10 THEN
                ERROR "Debe ingresar R.F.C. completo"
                NEXT FIELD n_rfc
            END IF

            IF r_afi.sexo IS NULL THEN
                ERROR "Campo Sexo NO puede ser NULO"
                NEXT FIELD sexo
            END IF

            IF g_edo_civil IS NULL THEN
                ERROR "Campo Estado Civil NO puede ser NULO"
                NEXT FIELD edo_civil
            END IF

            IF r_afi.fena IS NULL THEN
                ERROR "Campo Fecha Nacimiento NO puede ser NULO"
                NEXT FIELD fena
            END IF

            IF g_salario_base_comis IS NULL THEN
                ERROR "Campo Salario NO puede ser NULO"
                NEXT FIELD salario_base_comis
            END IF

            IF r_afi.estadon IS NULL THEN
                ERROR "Campo Estado Nacimiento NO puede ser NULO"
                NEXT FIELD estadon
            END IF

            IF r_afi.tip_prob IS NULL THEN
                ERROR "Campo Tipo Docto Prob NO puede ser NULO" 
                NEXT FIELD tip_prob
            END IF

            IF r_afi.fol_prob IS NULL AND
               r_afi.tip_prob <> 5 THEN
                ERROR "Campo Folio Docto Prob NO puede ser NULO" 
                NEXT FIELD fol_prob
            END IF

            IF r_afi.doc_prob IS NULL THEN
                ERROR "Campo Docto Prob NO puede ser NULO" 
                NEXT FIELD doc_prob
            END IF

            IF r_afi.ind_infonavit IS NULL THEN
                ERROR "Campo Indicador Credito Infonavit NO puede ser NULO" 
                NEXT FIELD ind_infonavit
            END IF

----aqui cambiar por codigo linea 2580 segun programa de MAURO

            SELECT "X"
              FROM afi_solicitud
             WHERE @n_seguro       = r_afi.n_seguro
               AND @n_folio        = r_afi.n_folio
               AND @tipo_solicitud = r_afi.tipo_solicitud

            IF SQLCA.SQLCODE <> 0 THEN
                CALL Inserta_en_tabla_afi_solicitud()
            END IF


            LET vcont = vcont + 1

            LET KEY = FGL_LASTKEY()
 
            CASE KEY
                 WHEN 2
                          LET comma = "fglgo AFIM004 ",
                          r_afi.n_seguro," ",
                          ACCION CLIPPED," ",
                          r_afi.tipo_solicitud,
                          r_afi.n_folio
                 WHEN 5

                   LET comma = "fglgo AFIM002.4gi ", r_afi.n_folio," ",
                                ACCION CLIPPED,"  ",vcont, " ",
                                r_afi.tipo_solicitud

               {
                 WHEN 6
                          LET v_an = 0
                          CALL n_identiff() 
                               RETURNING v_an, no_identif, n_iden_d
               }
               {  WHEN 14
                          LET comma = "fglgo AFIM006 ",
                          r_afi.n_seguro," ",
                          ACCION CLIPPED," ",
                          r_afi.tipo_solicitud,
                          r_afi.n_folio}
                 WHEN 16
                          LET comma = "fglgo AFIM025 ",
                          r_afi.n_folio, " ",
                          r_afi.tipo_solicitud, " ",
                          ACCION CLIPPED
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
            END CASE

            IF KEY != 6 THEN
               RUN comma 
            END IF

            LET vcontrol="S"

         WHENEVER ERROR STOP
            NEXT FIELD paterno
            
    END INPUT

END FUNCTION

FUNCTION n_identiff_c(nss)
#c-------------------------
   DEFINE fecha       DATE,
          nss         CHAR(11),

          re_g      RECORD
               clave       SMALLINT,
               descri      CHAR(30),
               cve_identif CHAR(30)
          END RECORD,

          va          CHAR(1)

   INITIALIZE re_g, va, fecha TO NULL

   SELECT MAX(a.fecha) INTO fecha FROM afi_ctr_identif a
   WHERE a.n_seguro = nss
   IF STATUS = NOTFOUND THEN
      ERROR " NO EXISTE INFORMACION PARA ESTE NSS"
      SLEEP 3
      ERROR ""
      RETURN
   END IF

   SELECT a.clave_identif, b.descripcion, a.identifica  
          INTO  re_g.*
          FROM   afi_ctr_identif a
          WHERE  a.n_seguro      = nss
            AND  a.fecha         = fecha
            AND  a.clave_identif =  b.codigo

   OPEN WINDOW v1 AT  6,6 WITH FORM "AFIM0019" ATTRIBUTE(BORDER)

   DISPLAY "                                                                    " 
           AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "                     I D E N T I F I C A C I O N                    " 
           AT 2,1 ATTRIBUTE(REVERSE)

     DISPLAY BY NAME re_g.*

     PROMPT "PRESIONE < ENTER > PARA CONTINUAR " FOR va    
END FUNCTION
FUNCTION n_identiff_m(nss)
   DEFINE nss         CHAR(11),
          fecha       DATE,
          re_g      RECORD
               clave       SMALLINT,
               descri      CHAR(30),
               cve_identif CHAR(30)
          END RECORD,

          va               CHAR(1),
          clave            SMALLINT,
          descri           CHAR(30),
          cve_identif      CHAR(30)

   INITIALIZE re_g, va, fecha TO NULL

   SELECT MAX(a.fecha) INTO fecha FROM afi_ctr_identif a
   WHERE a.n_seguro = nss
   IF STATUS = NOTFOUND THEN
      ERROR " NO EXISTE INFORMACION PARA ESTE NSS"
      SLEEP 3
      ERROR ""
      RETURN
   END IF

   SELECT a.clave_identif, a.identifica  INTO clave, cve_identif
          FROM   afi_ctr_identif a
          WHERE  a.n_seguro = nss
            AND  a.fecha    = fecha

   OPEN WINDOW vv_2 AT  6,6 WITH FORM "AFIM0019" ATTRIBUTE(BORDER)

   DISPLAY " [ Esc ] Salvar        [ Ctrl_C ] Salir                             " 
           AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "                     I D E N T I F I C A C I O N                    " 
           AT 2,1 ATTRIBUTE(REVERSE)

     DISPLAY BY NAME re_g.*
     INPUT clave,cve_identif FROM FORMONLY.clave, FORMONLY.cve_identif

           AFTER FIELD clave
                 IF clave IS NULL OR clave = " " THEN
                    CALL clave_ii() RETURN clave,descri
                    IF clave IS NULL OR clave = " " OR clave = 0 THEN
                       ERROR "Digite el Codigo Deseado, este campo no puede ser Nulo"
                       NEXT FIELD clave
                    END IF
                 ELSE
                     SELECT b.descripcion INTO descri FROM afi_identifica b
                     WHERE  b.codigo = clave
                     IF STATUS = NOTFOUND THEN
                        ERROR "Codigo Incorrecto, Verifique nuevamente"
                        NEXT FIELD clave
                     END IF
                     DISPLAY descri TO descripcion
                 END IF

           AFTER FIELD cve_identif

           ON KEY(ESC)
               IF clave IS NOT NULL OR clave != " "  AND
                  cve_identif IS NULL THEN
                  ERROR "Digite correctamente la Descripcion de la Identificacion"
                  NEXT FIELD cve_identif
               END IF

               IF clave IS  NULL OR clave = " " OR clave = 0 AND
                  cve_identif IS NULL THEN
                  LET va = 1
               ELSE
                   LET va = 0
               END IF
               EXIT INPUT


           ON KEY(CONTROL-C)
               LET va = 1
               EXIT INPUT

           ON KEY(INTERRUPT)
               LET va = 1
               EXIT INPUT

     END INPUT

   CLOSE WINDOW vv_2 
   RETURN va, clave,cve_identif,fecha
END FUNCTION

FUNCTION n_identiff()
   DEFINE cve_identif CHAR(30),
          clave       SMALLINT,
          descri      CHAR(30),
          va          SMALLINT

   LET va  = 0

   OPEN WINDOW v1 AT  6,6 WITH FORM "AFIM0019" ATTRIBUTE(BORDER)

   DISPLAY " [ Esc ] Salvar        [ Ctrl_C ] Salir                             " 
           AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "                     I D E N T I F I C A C I O N                    " 
           AT 2,1 ATTRIBUTE(REVERSE)

     INPUT clave,cve_identif FROM FORMONLY.clave, FORMONLY.cve_identif

         AFTER FIELD clave
               IF clave IS NULL OR
                  clave =  " "  THEN
                  CALL clave_ii() RETURN clave,descri
                  IF clave IS NULL OR clave = " " OR clave = 0 THEN
                     NEXT FIELD clave
                  END IF
                  DISPLAY clave TO clave
                  DISPLAY descri TO descripcion
                  NEXT FIELD cve_identif
               ELSE
                     SELECT b.descripcion INTO descri FROM afi_identifica b
                     WHERE  b.codigo = clave
                     IF STATUS = NOTFOUND THEN
                        ERROR "Codigo Incorrecto, Verifique nuevamente"
                        NEXT FIELD clave
                     END IF
                     DISPLAY descri TO descripcion 
               END IF

         AFTER FIELD cve_identif

         ON KEY(ESC)
               IF clave IS NOT NULL OR clave != " "  AND
                  cve_identif IS NULL THEN
                  ERROR "Digite correctamente la Descripcion de la Identificacion"
                  NEXT FIELD cve_identif
               END IF

               IF clave IS  NULL OR clave = " " OR clave = 0 AND
                  cve_identif IS NULL THEN
                  LET va = 1
               ELSE
                   LET va = 0
               END IF 
               EXIT INPUT

         ON KEY(CONTROL-C)
               LET va = 1
               EXIT INPUT

         ON KEY(INTERRUPT)
               LET va = 1
               EXIT INPUT
     END INPUT
   CLOSE WINDOW v1
   RETURN va, clave,cve_identif
END FUNCTION

##Muestra catalogo de claves de Identificacion
FUNCTION clave_ii()
#de-------------------------

    DEFINE c ARRAY[50] OF RECORD
        codigo	    SMALLINT,
        descripcion CHAR(50)
    END RECORD

    DEFINE i SMALLINT

    DECLARE cursor_cve_i CURSOR FOR 
        SELECT *
        FROM   afi_identifica
	ORDER BY 1

	LET i = 1

	FOREACH cursor_cve_i INTO c[i].*
            LET i = i + 1
	END FOREACH

	CALL SET_COUNT(i-1)

    OPEN WINDOW ven_clave AT 9,10 WITH FORM "AFIM00191" ATTRIBUTE(BORDER)

    DISPLAY "               CATALOGO DE IDENTIFICACIONES             " AT 3,1 ATTRIBUTE ( REVERSE )

    DISPLAY ARRAY c TO scr_1.*

    ON KEY ( INTERRUPT )
        LET i = ARR_CURR()

    ON KEY ( CONTROL-M )
        LET i = ARR_CURR()
        LET g_comision.cod_esq_comision  = c[i].codigo
        LET g_comision.desc_esq_comision = c[i].descripcion
        
    EXIT DISPLAY

    END DISPLAY

    CLOSE WINDOW ven_clave

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

FUNCTION Numero_operacion_asignado()
#noa--------------------------------

    DEFINE x_numerii INTEGER

    SELECT n_operac 
    INTO   x_numerii 
    FROM   afi_solicitud
    WHERE  n_folio = r_afi.n_folio
    AND    tipo_solicitud = r_afi.tipo_solicitud

    DISPLAY "                                                                                                 " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                                                 " AT 2,1 ATTRIBUTE(REVERSE)

    #PROMPT "Numero de Operacion Asignado ",x_numerii USING "########",
    #       " comuniquelo al promotor y presione ENTER" 
    #       ATTRIBUTE(REVERSE) FOR aux_pausa

END FUNCTION

#-- aqui adecuacion a traspaso

FUNCTION Inserta_en_tabla_afi_solicitud()
#ita-------------------------------

    DEFINE x_coduni        CHAR(10)
    DEFINE estado_afiliado SMALLINT
    DEFINE en_proceso  	   SMALLINT

    LET estado_afiliado = ""
    LET en_proceso      = ""
    LET g_hora          = TIME

    #### Modi #### CHE#
    SELECT UNIQUE 'X'
    FROM safre_af:pro_mae_promotor a
    WHERE a.cod_promotor = r_afi.cod_promotor
    IF STATUS = NOTFOUND THEN
       ERROR "Promotor Inexistente"
       RETURN
    END IF

    SELECT UNIQUE "X" 
    FROM   afi_solicitud g
    WHERE  g.n_seguro = r_afi.n_seguro
    AND    g.n_folio  = r_afi.n_folio
    AND    g.tipo_solicitud = r_afi.tipo_solicitud

    IF STATUS = NOTFOUND THEN
        SELECT f.coduni_n1 
        INTO   x_coduni 
        FROM   com_nivel1 f
        WHERE  f.coduni_n1 = r_afi.agenc_cod

        INSERT INTO afi_solicitud 
        VALUES(r_afi.n_seguro,
               r_afi.n_unico,
               r_afi.n_rfc,
               r_afi.paterno,
               r_afi.materno,
               r_afi.nombres,
               r_afi.fena,
               r_afi.n_folio,
               g_edo_civil,
               "",
               r_afi.estadon,
               0,
               r_afi.cod_promotor,
               r_afi.sexo,
               g_n_operac,
               r_afi.frecafor,
               "",
               r_afi.fecha_emision,
               "",
               "",
               0,
               r_afi.agenc_cod,
               15, 
               r_afi.nacionalidad,
               r_afi.tip_prob,
               r_afi.fol_prob,
               r_afi.doc_prob,
               r_afi.ind_infonavit,
               "",
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
               0,      #status_captura
               r_afi.tipo_solicitud,
               r_afi.fecha_elaboracion,
               0,
               "",
               g_comision.cod_esq_comision,
               "",
               "",
               "",
               "",
               "",
               cod_err,
               g_folio_edo_cta,
               r_afi.cod_afore_ced,
               g_salario_base_comis,
               0,       #salario_actual
               "",      #fecha_actualiza_sa
               x_coduni,#coduni_n1
               0 ,      #indicador_comision
               r_afi.codven,
               "",
               "",
               "",
               ""
               )

          # issa 
          CALL Inserta_en_tabla_afi_ctr_actividad(r_afi.n_seguro, r_afi.n_folio, r_afi.tipo_solicitud)

    ELSE
           UPDATE afi_solicitud
           SET    paterno            = r_afi.paterno           ,
                  materno            = r_afi.materno           ,
                  nombres            = r_afi.nombres           ,
                  n_seguro           = r_afi.n_seguro          ,
                  n_rfc              = r_afi.n_rfc             ,
                  n_unico            = r_afi.n_unico           ,
                  sexo               = r_afi.sexo              ,
                  edo_civil          = g_edo_civil         ,
                  fena               = r_afi.fena              ,
                  estadon            = r_afi.estadon           ,
                  nacionalidad       = r_afi.nacionalidad      ,
                  tip_prob           = r_afi.tip_prob          ,
                  fol_prob           = r_afi.fol_prob          ,
                  doc_prob           = r_afi.doc_prob          ,
                  ind_infonavit      = r_afi.ind_infonavit     ,
                  salario_base_comis = g_salario_base_comis,
                  status_captura     = 0                          ,
                  cod_error_origen   = cod_err                    ,
                  folio_edo_cta      = g_folio_edo_cta     ,
                  cod_afore_ced      = r_afi.cod_afore_ced     ,
                  femision           = r_afi.fecha_emision
           WHERE  n_folio            = r_afi.n_folio
           AND    tipo_solicitud     = r_afi.tipo_solicitud

          # issa 
          CALL Inserta_en_tabla_afi_ctr_actividad(r_afi.n_seguro, r_afi.n_folio, r_afi.tipo_solicitud)

    END IF
    
END FUNCTION
FUNCTION Inserta_en_tabla_afi_ctr_actividad(vn_seguro, vn_folio, vtipo_solicitud)
#aca-----------------
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

FUNCTION Actualiza()
#ac-----------------

END FUNCTION

FUNCTION Consulta()
#c-----------------

    DEFINE desp    CHAR(40)
    DEFINE consul  CHAR(40)
    DEFINE desc_ts CHAR(12)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1

    DISPLAY " CONSULTA " AT 1,69 ATTRIBUTE(REVERSE)
    DISPLAY "CONTROL: [F] Despliega [G] Esq Comis [O] Motivos Rech [C] Salir" AT 1,1 ATTRIBUTE(BOLD)


    DISPLAY "[E] Dom. [T] Tels [B] Benef [V] Patrones [P] Icefas [W] Observ [Y] Ident" AT 2,1 ATTRIBUTE(BOLD)

    LET g_n_operac = NULL
    LET r_afi.n_folio  = NULL

    INPUT BY NAME r_afi.tipo_solicitud,
                  r_afi.n_folio,
                  r_afi.n_seguro WITHOUT DEFAULTS
                  -- issa g_n_operac WITHOUT DEFAULTS

        AFTER FIELD tipo_solicitud

            IF r_afi.tipo_solicitud IS NULL THEN
                ERROR "Tipo de Solicitud NO puede ser NULO"
                LET r_afi.tipo_solicitud = NULL
                DISPLAY BY NAME r_afi.tipo_solicitud
                NEXT FIELD tipo_solicitud
            END IF

            SELECT tts.desc_solicitud
            -- issa INTO   desc_ts
            INTO   r_afi.desc_solicitud 
            FROM   tab_tipo_solic tts
            WHERE  tts.tipo_solicitud = r_afi.tipo_solicitud

            IF SQLCA.SQLCODE = 0 THEN
                -- iss DISPLAY desc_ts AT 8,15
                DISPLAY r_afi.desc_solicitud
            ELSE
                ERROR "Tipo de Solicitud solo puede ser 1) reg. 2) trasp. 3) unif. 4) virtual 5) asig. "
                LET r_afi.tipo_solicitud = NULL
                DISPLAY BY NAME r_afi.tipo_solicitud
                NEXT FIELD tipo_solicitud
            END IF

            NEXT FIELD n_folio

        AFTER FIELD n_folio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                CALL Inicializa()
                NEXT FIELD tipo_solicitud
            END IF

            IF r_afi.n_folio IS NULL THEN
                NEXT FIELD n_seguro
            ELSE
                IF NOT Rescata_datos("N",r_afi.n_folio) THEN
                    ERROR "Afiliado NO existe"
                    NEXT FIELD n_folio
                END IF

                    LET r_afi.fecha_cap = r_afi.frecafor

                DISPLAY BY NAME r_afi.*
            END IF


        AFTER FIELD n_seguro
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                CALL Inicializa()
                NEXT FIELD tipo_solicitud
            END IF

            IF r_afi.n_seguro IS NULL THEN
                NEXT FIELD n_folio
            ELSE
                IF NOT Rescata_datos("C",r_afi.n_seguro) THEN
                    ERROR "Afiliado NO existe"
                    NEXT FIELD n_seguro
                END IF

                DISPLAY BY NAME r_afi.*
            END IF

        ON KEY ( CONTROL-O )
            CALL motivo_rechazo()

        ON KEY ( CONTROL-F )
            CALL Despliega()
            DISPLAY BY NAME r_afi.tipo_solicitud, r_afi.n_folio

        ON KEY ( CONTROL-Y )
            LET v_an = 0
            CALL n_identif_c(r_afi.n_seguro, r_afi.n_folio, r_afi.tipo_solicitud)

        ON KEY (CONTROL-W)
            CALL observaciones_c(r_afi.n_seguro,r_afi.n_folio,r_afi.tipo_solicitud)

        ON KEY ( INTERRUPT )
            CALL Inicializa()
            DISPLAY "                                     " AT 5,56  ATTRIBUTE(REVERSE)
            EXIT INPUT

        ON KEY ( CONTROL-E,CONTROL-T,CONTROL-B,CONTROL-V,
                 CONTROL-P,CONTROL-G,CONTROL-J )

            IF r_afi.n_seguro IS NULL THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD n_seguro
            END IF

            LET KEY = FGL_LASTKEY()
 
            CASE KEY
                 WHEN 2
                          LET comma = "fglgo AFIM004 ",
                          r_afi.n_seguro," ",
                          ACCION CLIPPED," ",
                          r_afi.tipo_solicitud,
                          r_afi.n_folio
                 WHEN 5

                    LET comma = "fglgo AFIM002.4gi ", r_afi.n_folio," ",
                                ACCION CLIPPED,"  ",vcont, " ",
                                r_afi.tipo_solicitud

		 {
                 WHEN 10
                          LET v_an = 0
                          CALL n_identiff_c(r_afi.n_seguro) 
		 }
                 {WHEN 14
                          LET comma = "fglgo AFIM006 ",
                          r_afi.n_seguro," ",
                          ACCION CLIPPED," ",
                          r_afi.tipo_solicitud,
                          r_afi.n_folio}
                 WHEN 16
                          LET comma = "fglgo AFIM025 ",
                          r_afi.n_folio, " ",
                          r_afi.tipo_solicitud," ",
                          ACCION CLIPPED
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
                 WHEN 7
                          IF r_afi.n_folio > 0 THEN
                             CALL Ingresa_Esquema_Comision("C",r_afi.n_folio)
                          ELSE
      ERROR "No puede ver Esquema Comision sin antes especificar la solicitud"
                          END IF
            END CASE

            IF KEY != 10 THEN
               RUN comma 
            END IF

    END INPUT

END FUNCTION

FUNCTION Pregunta(a)
#p------------------

    DEFINE a SMALLINT

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
                   -- issa a.n_operac,
                   a.frecafor,
                   a.paterno,
                   a.materno,
                   a.nombres,
                   a.n_seguro,
                   a.n_unico,
                   a.n_rfc,
                   a.sexo,
                   -- issa a.edo_civil,
                   a.fena,
                   a.frecafor,   
                   -- issa a.salario_base_comis,
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
                   -- issa  a.folio_edo_cta,
                   a.cod_error_origen,
                   a.femision,
                   a.finicta
            INTO   r_afi.codven,
                   r_afi.agenc_cod,
                   r_afi.cod_promotor,
                   r_afi.n_folio,
                   -- g_n_operac,
                   r_afi.frecafor,
                   r_afi.paterno,
                   r_afi.materno,
                   r_afi.nombres,
                   r_afi.n_seguro,
                   r_afi.n_unico,
                   r_afi.n_rfc,
                   r_afi.sexo,
                   -- issa g_edo_civil,
                   r_afi.fena,
                   r_afi.frecafor,
                   -- issa g_salario_base_comis,
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
                   -- issa g_folio_edo_cta,
                   r_afi.cod_error_orig,
                   r_afi.fecha_emision,
                   x_finicta
            FROM   afi_solicitud a
            WHERE  a.n_seguro       = x_valor
            AND    a.tipo_solicitud = r_afi.tipo_solicitud

            IF STATUS = NOTFOUND THEN
                RETURN FALSE
            END IF

            IF r_afi.tipo_solicitud <> 2 THEN
                #LET r_afi.fecha_emision = NULL 
                LET r_afi.cod_afore_ced = NULL 
                LET g_folio_edo_cta = NULL 
            END IF
        END IF
      WHEN "N"
        LET xx_num = x_valor clipped

        SELECT a.codven,
               a.agenc_cod,
               a.cod_promotor,
               a.n_folio,	
               -- issa a.n_operac,
               a.frecafor,
               a.paterno,
               a.materno,
               a.nombres,
               a.n_seguro,
               a.n_unico,
               a.n_rfc,
               a.sexo,
               -- issa a.edo_civil,
               a.fena,
               a.frecafor, 
               -- issa a.salario_base_comis,
               -- issa a.salario_actual,
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
               -- issa a.folio_edo_cta,
               a.cod_error_origen,
               a.femision,
               a.finicta
        INTO   r_afi.codven,
               r_afi.agenc_cod,
               r_afi.cod_promotor,
               r_afi.n_folio,
               -- issa g_n_operac,
               r_afi.frecafor,
               r_afi.paterno,
               r_afi.materno,
               r_afi.nombres,
               r_afi.n_seguro,
               r_afi.n_unico,
               r_afi.n_rfc,
               r_afi.sexo,
               -- issa g_edo_civil,
               r_afi.fena,
               r_afi.frecafor,
               -- issa g_salario_base_comis, 
               -- issa g_salario_actual ,
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
               -- issa g_folio_edo_cta,
               r_afi.cod_error_orig,
               r_afi.fecha_emision,
               x_finicta
        FROM   afi_solicitud a
        WHERE  a.n_folio = xx_num
        AND    a.tipo_solicitud = r_afi.tipo_solicitud

        IF STATUS = NOTFOUND THEN
            RETURN FALSE
        END IF

        IF r_afi.tipo_solicitud <> 2 THEN
            #LET r_afi.fecha_emision = NULL 
            LET r_afi.cod_afore_ced = NULL 
            LET g_folio_edo_cta = NULL 
        END IF
      WHEN "O"
        LET xx_num = x_valor clipped

        SELECT a.codven,
               a.agenc_cod,
               a.cod_promotor,
               a.n_folio,	
               -- issa a.n_operac,
               a.frecafor,
               a.paterno,
               a.materno,
               a.nombres,
               a.n_seguro,
               a.n_unico,
               a.n_rfc,
               a.sexo,
               -- issa a.edo_civil,
               a.fena,
               a.frecafor, 
               -- issa a.salario_base_comis,
               -- issa a.salario_actual,
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
               -- issa a.folio_edo_cta,
               a.cod_error_origen,
               a.femision,
               a.finicta
        INTO   r_afi.codven,
               r_afi.agenc_cod,
               r_afi.cod_promotor,
               r_afi.n_folio,
               -- issa g_n_operac,
               r_afi.frecafor,
               r_afi.paterno,
               r_afi.materno,
               r_afi.nombres,
               r_afi.n_seguro,
               r_afi.n_unico,
               r_afi.n_rfc,
               r_afi.sexo,
               -- issa g_edo_civil,
               r_afi.fena,
               r_afi.frecafor,
               -- issa g_salario_base_comis, 
               -- issa g_salario_actual,
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
               -- issa g_folio_edo_cta,
               r_afi.cod_error_orig,
               x_finicta
        FROM   afi_solicitud a
        WHERE  a.n_operac = xx_num
        AND    a.tipo_solicitud = r_afi.tipo_solicitud

        IF STATUS = NOTFOUND THEN
            RETURN FALSE
        END IF

        IF r_afi.tipo_solicitud <> 2 THEN
            #LET r_afi.fecha_emision = NULL 
            LET r_afi.cod_afore_ced = NULL 
            LET g_folio_edo_cta = NULL 
        END IF
    END CASE

    SELECT @descripcion
      INTO r_afi.desc_ind_info
      FROM safre_af:tab_ind_cred
     WHERE @codigo = r_afi.ind_infonavit
    DISPLAY BY NAME r_afi.desc_ind_info
 
    # issa 
    SELECT profesion_cod,actividad_cod
      INTO r_afi.profesion_cod,r_afi.actividad_cod
      FROM afi_ctr_actividad
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

    SELECT ecivi_desc 
    INTO   g_desc_edo_civil 
    FROM   tab_edo_civil
    WHERE  ecivi_cod = g_edo_civil

    IF STATUS = NOTFOUND THEN
        LET g_desc_edo_civil = "NO EXISTE"
    END IF

    SELECT sexo_desc 
    INTO   r_afi.desc_sexo 
    FROM   tab_sexo
    WHERE  sexo_cod = r_afi.sexo

    IF STATUS = NOTFOUND THEN
        LET r_afi.desc_sexo = "NO EXISTE"
    END IF

    SELECT ecivi_desc 
    INTO   g_desc_edo_civil 
    FROM   tab_edo_civil
    WHERE  ecivi_cod = g_edo_civil

    IF STATUS = NOTFOUND THEN
        LET g_desc_edo_civil = "NO EXISTE"
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

    RETURN TRUE

END FUNCTION

# Copiar a partir de aqui
FUNCTION Modifica()
#------------------

    DEFINE fecha_comprueba DATE
    DEFINE xx_status       SMALLINT
    DEFINE rescato         SMALLINT
    DEFINE recha           SMALLINT
    DEFINE v_1             SMALLINT
    DEFINE val_1           CHAR(80)
    DEFINE v_an            SMALLINT 
    DEFINE no_identif      SMALLINT
    DEFINE n_iden_d        CHAR(50)
    DEFINE fe_identif      DATE
    DEFINE vreenv          CHAR(1)
    DEFINE bnd_fena        SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY "MODIFICA " AT 1,70 ATTRIBUTE(REVERSE)

    DISPLAY "[Esc]Modif CTRL: [O]Mot.Rech [F]Despl [C]Salir" 
            AT 1,1 ATTRIBUTE(BOLD)

    DISPLAY " [E] Domic [T] Tels [B] Benef [V] Patrones [P] Icefas [W] Observ [Y] Ident" AT 2,1 ATTRIBUTE(BOLD)

    IF r_afi.n_seguro IS NULL THEN
        CALL Inicializa()

        LET r_afi.tipo_solicitud = NULL
        LET r_afi.n_folio        = NULL
    END IF

    LET sw_1     = 0
    LET rescato  = TRUE
    LET tot_sief = 0
    LET vreenv   = NULL

    INPUT BY NAME r_afi.* WITHOUT DEFAULTS

# TIPO_SOLICITUD 
        AFTER FIELD tipo_solicitud
            IF r_afi.tipo_solicitud IS NULL THEN
                ERROR "Tipo de Registro  erroneo" 
                NEXT FIELD tipo_solicitud
            END IF

            IF r_afi.tipo_solicitud <> 1 AND
               r_afi.tipo_solicitud <> 2 THEN
                ERROR "Tipo Solic. solo puede ser 1)Reg. Inicial  2)Traspaso  "
                LET r_afi.tipo_solicitud = NULL
                DISPLAY BY NAME r_afi.tipo_solicitud
                NEXT FIELD tipo_solicitud
            END IF

            SELECT ts.desc_solicitud
            INTO   r_afi.desc_solicitud
            FROM   tab_tipo_solic ts
            WHERE  ts.tipo_solicitud = r_afi.tipo_solicitud

            IF SQLCA.SQLCODE = 0 THEN
                DISPLAY r_afi.desc_solicitud 
            ELSE
                ERROR "Tipo Solic. solo puede ser 1)Reg. Inicial  2)Traspaso  "
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

                    LET r_afi.fecha_cap = r_afi.frecafor
                    DISPLAY BY NAME r_afi.*
#OJOI
                    SELECT cod_esq_comision
                    INTO   g_comision.cod_esq_comision
                    FROM   afi_solicitud
                    WHERE  n_folio = r_afi.n_folio
                    AND    tipo_solicitud = r_afi.tipo_solicitud
                END IF
            END IF

            SELECT a.status_interno
            INTO   xx_status
            FROM   afi_solicitud a
            WHERE  a.n_seguro = r_afi.n_seguro
            AND    a.n_folio = r_afi.n_folio
            AND    a.tipo_solicitud = r_afi.tipo_solicitud
                   
        CASE xx_status
            WHEN 15 ERROR "NO PUEDE MODIFICAR, SOLICITUD CONFIRMADA "
                NEXT FIELD tipo_solicitud
            WHEN 20 ERROR "NO PUEDE MODIFICAR, SOLICITUD VALIDADA "
                NEXT FIELD tipo_solicitud
            WHEN 30 ERROR "NO PUEDE MODIFICAR, ENVIADO A PROCESAR "
                NEXT FIELD tipo_solicitud
            WHEN 42 ERROR "NO PUEDE MODIFICAR, RECHAZO PROCESAR   "
                NEXT FIELD tipo_solicitud
            --WHEN 45 ERROR "NO PUEDE MODIFICAR, TRASPASO RECHAZADO "
                --NEXT FIELD tipo_solicitud
            WHEN 50 ERROR "NO PUEDE MODIFICAR, REGISTRO PENDIENTE "
                NEXT FIELD tipo_solicitud
            WHEN 55 ERROR "NO PUEDE MODIFICAR, REGISTRO EN ACLARACION "
                NEXT FIELD tipo_solicitud
            WHEN 60 ERROR "NO PUEDE MODIFICAR, REGISTRO APROBADO  "
                NEXT FIELD tipo_solicitud
            WHEN 70 ERROR "NO PUEDE MODIFICAR, REGISTRO CERTIF. NO LIQ. "
                NEXT FIELD tipo_solicitud
            WHEN 75 ERROR "NO PUEDE MODIFICAR, REGISTRO CERTIF. LIQ. "
                NEXT FIELD tipo_solicitud
            WHEN 90 ERROR "NO PUEDE MODIFICAR, REGISTRO PEND. ASIG.. "
                NEXT FIELD tipo_solicitud
            WHEN 100 ERROR "NO PUEDE MODIFICAR, REGISTRO CERTIFICADO  "
                NEXT FIELD tipo_solicitud
        END CASE  

        SELECT A.status 
        INTO   recha 
        FROM   pro_mae_promotor A
        WHERE  A.cod_promotor = r_afi.cod_promotor

     {
        IF recha IS NOT NULL THEN
            CASE recha
                WHEN 2 ERROR "PROMOTOR SUSPENDIDO" SLEEP 2
                    CLEAR FORM
                    NEXT FIELD tipo_solicitud
                WHEN 3 ERROR "PROMOTOR CANCELADO" SLEEP 2
                    CLEAR FORM
                    NEXT FIELD tipo_solicitud
                OTHERWISE
                    ERROR "PROMOTOR ACTIVO" SLEEP 1
            END CASE
        ELSE
            ERROR "PROMOTOR NO EXISTE" SLEEP 1
        END IF
     }

        IF r_afi.tipo_solicitud <> 2 THEN
            LET r_afi.cod_afore_ced = ""
            LET r_afi.desc_afore    = ""

            DISPLAY BY NAME r_afi.cod_afore_ced,
                            r_afi.desc_afore

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
           --ELSE
               --NEXT FIELD cod_afore_ced
           END IF
        END IF

# PATERNO

        AFTER FIELD paterno
            IF r_afi.paterno IS NULL OR
               r_afi.paterno =  " "  THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD paterno
            ELSE
                LET v_1 = 0
                INITIALIZE val_1 TO NULL
                CALL verifica_nombre(r_afi.paterno) RETURNING v_1,val_1 #ve--
                IF v_1 = 1 THEN
                   ERROR "A.Paterno ",val_1 CLIPPED
                   NEXT FIELD paterno
                END IF
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               IF r_afi.paterno IS NULL OR
                  r_afi.paterno =  " "  THEN
                   ERROR "Campo NO puede ser NULO"
                   NEXT FIELD paterno
               ELSE
                   LET v_1 = 0
                   INITIALIZE val_1 TO NULL
                   CALL verifica_nombre(r_afi.paterno) RETURNING v_1,val_1 #ve--
                   IF v_1 = 1 THEN
                      ERROR "A.Paterno ",val_1 CLIPPED
                      NEXT FIELD paterno
                   END IF
               END IF
               NEXT FIELD n_folio
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               IF r_afi.paterno IS NULL OR
                  r_afi.paterno =  " "  THEN
                   ERROR "Campo NO puede ser NULO"
                   NEXT FIELD paterno
               ELSE
                   LET v_1 = 0
                   INITIALIZE val_1 TO NULL
                   CALL verifica_nombre(r_afi.paterno) RETURNING v_1,val_1 #ve--
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
                  r_afi.paterno =  " "  THEN
                   ERROR "Campo NO puede ser NULO"
                   NEXT FIELD paterno
               ELSE
                   LET v_1 = 0
                   INITIALIZE val_1 TO NULL
                   CALL verifica_nombre(r_afi.paterno) RETURNING v_1,val_1 #ve--
                   IF v_1 = 1 THEN
                      ERROR "A.Paterno ",val_1 CLIPPED
                      NEXT FIELD paterno
                   END IF
               END IF
               NEXT FIELD materno
            END IF

# MATERNO
        AFTER FIELD materno
            IF r_afi.materno IS NOT NULL OR
               r_afi.materno !=  " "  THEN
               LET v_1 = 0
               INITIALIZE val_1 TO NULL
               CALL verifica_nombre(r_afi.materno) RETURNING v_1,val_1 #ve--
               IF v_1 = 1 THEN
                  ERROR "A.Materno ",val_1 CLIPPED
                  NEXT FIELD materno
               END IF
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD paterno
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
               NEXT FIELD nombres
            END IF

# NOMBRE

        AFTER FIELD nombres
            IF r_afi.nombres IS NULL OR r_afi.nombres = " " THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD nombres
            ELSE
                LET v_1 = 0
                INITIALIZE val_1 TO NULL
                CALL verifica_nombre(r_afi.nombres) RETURNING v_1,val_1 #ve--
                IF v_1 = 1 THEN
                   ERROR "El Nombre ",val_1 CLIPPED
                   NEXT FIELD nombres
                END IF
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                IF r_afi.nombres IS NULL OR
                   r_afi.nombres =  " "  THEN
                    ERROR "Campo NO puede ser NULO"
                    NEXT FIELD nombres
                ELSE
                    LET v_1 = 0
                    INITIALIZE val_1 TO NULL
                    CALL verifica_nombre(r_afi.nombres) RETURNING v_1,val_1 #ve--
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
                   r_afi.nombres =  " "  THEN
                    ERROR "Campo NO puede ser NULO"
                    NEXT FIELD nombres
                ELSE
                    LET v_1 = 0
                    INITIALIZE val_1 TO NULL
                    CALL verifica_nombre(r_afi.nombres) RETURNING v_1,val_1 #ve--
                    IF v_1 = 1 THEN
                       ERROR "El Nombre ",val_1 CLIPPED
                       NEXT FIELD nombres
                    END IF
                END IF
                NEXT FIELD fena
            END IF

# FECHA DE NAIMIENTO

        AFTER FIELD fena
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               NEXT FIELD nombres
            END IF

            IF r_afi.fena IS NULL THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD fena
            END IF


           IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
              FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
              NEXT FIELD sexo
           END IF                                              

# SEXO

        AFTER FIELD sexo
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
               FGL_LASTKEY() = FGL_KEYVAL("LEFT")  THEN
               IF r_afi.sexo IS NULL OR r_afi.sexo = " " OR
                  r_afi.sexo = 0 THEN
                  ERROR "Digite correctamente el sexo antes de pasar a otro campo"
                  NEXT FIELD sexo
               ELSE
                     NEXT FIELD fena
               END IF
            END IF                                           

            IF r_afi.sexo IS NULL OR r_afi.sexo = " " OR
               r_afi.sexo = 0 THEN
                CALL Despliega_sexos() RETURNING r_afi.sexo,
                                                 r_afi.desc_sexo
                IF r_afi.sexo IS NULL OR r_afi.sexo = " " OR
                   r_afi.sexo = 0 THEN
                   ERROR "Digite correctamente el sexo "
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

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
               IF r_afi.sexo IS NULL OR r_afi.sexo = " " OR
                  r_afi.sexo = 0 THEN
                  ERROR "Digite correctamente el sexo antes de pasar a otro campo"
                  NEXT FIELD sexo
               ELSE
                     NEXT FIELD n_unico
               END IF
            END IF

  
# CURP 

        AFTER FIELD n_unico
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") or 
               FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD sexo
            END IF

            IF r_afi.tip_prob = 5 THEN
                IF r_afi.n_unico IS NOT NULL OR
                   r_afi.n_unico <> " " THEN

                   IF LENGTH(r_afi.n_unico) < 18 AND
                      LENGTH(r_afi.n_unico) > 0  THEN
                       ERROR "Debe ingresar CURP completa"
                       NEXT FIELD n_unico 
                   ELSE
                       IF r_afi.n_unico[1] <> " " OR
                          r_afi.n_unico IS NOT NULL THEN
                           IF r_afi.n_unico[11] = "H" THEN
                               LET sexo_cur = "1"
                           ELSE
                               LET sexo_cur = "2"
                           END IF
                       END IF
                   END IF

                   IF r_afi.n_unico[1] = " " THEN
                       ERROR "Debe ingresar CURP correcta"
                       NEXT FIELD n_unico
                   END IF

                   CALL valida_est_curp(r_afi.n_unico) RETURNING pasa_curp, desc_err
                   IF pasa_curp = 1 THEN
                      ERROR "", desc_err
                      LET pasa_curp = 0
                      #NEXT FIELD n_unico
                   END IF

                   CALL var_dig_curp(r_afi.n_unico) RETURNING pasa, dig_curp

                   IF pasa = 0 THEN
                      ERROR "Digito Verificador Invalido curp, el digito es : ",
                      dig_curp
                      LET pasa = 0
                      #NEXT FIELD n_unico
                   END IF
                END IF
            END IF
 
        IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
           FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
           NEXT FIELD n_rfc 
        END IF

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
                IF r_afi.tipo_solicitud = 2 THEN
		    NEXT FIELD n_unico
                ELSE
		    NEXT FIELD n_folio
                END IF
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
		IF r_afi.tip_prob = 5 THEN
                    NEXT FIELD n_unico
                ELSE
                    NEXT FIELD n_rfc
                END IF
            END IF

# N_RFC
        AFTER FIELD n_rfc
            IF r_afi.n_rfc IS NULL THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD n_rfc
            END IF

            IF LENGTH(r_afi.n_rfc) <> 10 AND
               LENGTH(r_afi.n_rfc) <> 13 THEN
                ERROR "Debe ingresar R.F.C. completo"
                NEXT FIELD n_rfc
            END IF

            IF r_afi.n_unico IS NOT NULL THEN
                 LET x_fecha = r_afi.n_unico[7,8], "/",
                               r_afi.n_unico[9,10],"/",
                               "19",
                               r_afi.n_unico[5,6]

                 LET bnd_fena = 1
             ELSE
                 LET x_fecha = r_afi.n_rfc[7,8],"/",
                               r_afi.n_rfc[9,10],"/",
                               "19",r_afi.n_rfc[5,6]

                 LET bnd_fena = 2
            END IF

            LET xx_fecha = x_fecha

            IF xx_fecha <> r_afi.fena THEN
                WHILE TRUE
                    PROMPT "Existen inconsistencias en Fecha nacimiento, ",
                           "es correcto ¿[S/N]? "
                    FOR enter
                    IF enter MATCHES "[Ss/Nn]" THEN
                        IF enter MATCHES "[Ss]" THEN
                            LET r_afi.fena = xx_fecha
                            DISPLAY BY NAME r_afi.fena
                            NEXT FIELD estadon
                            EXIT WHILE
                        ELSE
                            IF bnd_fena = 1 THEN
                               NEXT FIELD n_unico
                            ELSE
                               NEXT FIELD n_rfc
                            END IF
                        END IF
                    ELSE
                        ERROR "Solo debe presionar (S)i o (N)o"
                        SLEEP 3
                        ERROR ""
                    END IF
                END WHILE 
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD sexo 
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
	       IF r_afi.tip_prob = 5 THEN
                   NEXT FIELD n_unico
               ELSE
                   NEXT FIELD sexo
               END IF
            END IF


            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                NEXT FIELD estadon
            END IF


# ESTADON 

        AFTER FIELD estadon
            IF r_afi.estadon IS NULL THEN
                CALL Despliega_estados() RETURNING r_afi.estadon,
                                                   r_afi.desc_estadon
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
               NEXT FIELD nacionalidad
            END IF

# NACIONALIDAD

        AFTER FIELD nacionalidad
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD estadon
            END IF

            IF r_afi.nacionalidad IS NULL THEN
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

# INFONAVIT

        AFTER FIELD ind_infonavit
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD actividad_cod
            END IF                                   

            IF r_afi.ind_infonavit IS NULL OR 
               r_afi.ind_infonavit = " "   THEN
               CALL Despliega_ind_info() RETURNING r_afi.ind_infonavit,
                                                   r_afi.desc_ind_info
               IF r_afi.ind_infonavit IS NULL OR
                  r_afi.ind_infonavit = " "   THEN
                  ERROR "Se requiere la clave del Indicador de Credito"
                  NEXT FIELD ind_infonavit
               END IF
            END IF

            IF r_afi.ind_infonavit NOT MATCHES "[0123]" THEN
               CALL Despliega_ind_info() RETURNING r_afi.ind_infonavit,
                                                   r_afi.desc_ind_info
               IF r_afi.ind_infonavit NOT MATCHES "[0123]" THEN
                  ERROR "(0)Sin cred (1)Con cred INFONAVIT (2)Con cred 43 Bis ",
                        "(3)Con cred FOVISSSTE"
                  NEXT FIELD ind_infonavit
               END IF
            ELSE
               SELECT @descripcion
                 INTO r_afi.desc_ind_info
                 FROM safre_af:tab_ind_cred
                WHERE @codigo = r_afi.ind_infonavit
               DISPLAY BY NAME r_afi.desc_ind_info 
            END IF    

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                NEXT FIELD fecha_elaboracion
            END IF       

            IF r_afi.tipo_solicitud = 2 THEN
                NEXT FIELD fecha_emision
            ELSE
                NEXT FIELD tip_prob
            END IF

# FECHA VENCIMIENTO 

        AFTER FIELD fecha_emision
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD ind_infonavit
            END IF

            IF r_afi.fecha_emision IS NULL THEN
                 ERROR "Fecha Emision de Estado de Cuenta NO puede ser NULO"
                 NEXT FIELD fecha_emision
            ELSE
        # Valida con fecha de Recepcion
        LET fecha_comprueba = r_afi.fecha_emision + 30 UNITS DAY
        # compara contra frecafor(hoy)
             {
               IF r_afi.fecha_emision < r_afi.frecafor THEN
                  ERROR "F. Ven. No puede ser Menor a F. Rec."
                  NEXT FIELD fecha_emision
               END IF
             }
               
               IF r_afi.frecafor      >  fecha_comprueba THEN
                  WHILE TRUE
                     PROMPT "Dcto estrc comis ya vencio, ",
                            "¿seguir la captura [S/N]? "
                     ATTRIBUTES (REVERSE) FOR aux_pausa
                      
                     IF aux_pausa MATCHES "[SsNn]" THEN  
                        IF aux_pausa MATCHES "[Nn]" THEN
                           RETURN
                        ELSE
                           EXIT WHILE
                        END IF
                     ELSE
                        ERROR "Solo debe presionar (S)i o (N)o"
                        SLEEP 2
                        ERROR ""
                     END IF
                  END WHILE
               END IF
 
            END IF


            IF (FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT")) THEN
                NEXT FIELD fecha_elaboracion
            END IF

# FRECAFOR --- F-REC

        AFTER FIELD frecafor

             IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") OR  
                FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                 NEXT FIELD fecha_elaboracion
             END IF


            IF (FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT")) AND
               r_afi.tip_prob = 5 THEN
                NEXT FIELD n_unico
            ELSE
                NEXT FIELD fecha_cap
            END IF

# FECHA CAPTURA

        AFTER FIELD fecha_cap
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") OR
               FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD cod_afore_ced
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
               NEXT FIELD frecafor
            END IF

            NEXT FIELD cod_afore_ced


# AFORE CEDENTE

        AFTER FIELD cod_afore_ced
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD fecha_emision 
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
                NEXT FIELD tip_prob
            END IF



# TIPO

        BEFORE FIELD tip_prob
            DISPLAY BY NAME r_afi.tip_prob

        AFTER FIELD tip_prob
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD cod_afore_ced
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               IF r_afi.tip_prob IS NULL OR
                  r_afi.tip_prob = " "   OR
                  r_afi.tip_prob =  0    THEN
                  NEXT FIELD tip_prob
               ELSE
                  NEXT FIELD cod_afore_ced
               END IF
            END IF

            IF r_afi.tip_prob IS NULL OR
               r_afi.tip_prob = " "   OR
               r_afi.tip_prob =  0    THEN
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
               IF r_afi.tip_prob IS NULL OR
                  r_afi.tip_prob = " "   OR
                  r_afi.tip_prob =  0    THEN
                  NEXT FIELD tip_prob
               ELSE
                  NEXT FIELD fol_prob
               END IF
            END IF  
# FOLIO
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

        AFTER FIELD doc_prob
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR 
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD fol_prob
            END IF

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

        ON KEY ( CONTROL-O )
            CALL motivo_rechazo()

        ON KEY ( CONTROL-F )
            CALL Despliega()
            DISPLAY BY NAME r_afi.n_folio

        ON KEY ( CONTROL-S )
          CASE r_afi.tipo_solicitud
              WHEN 1
                  IF su_estatus <= 20 OR
                     su_estatus > 5 THEN
                      WHILE TRUE
                      PROMPT "Esta seguro de rechazar solicitud [S/N] ? " 
                      ATTRIBUTES (REVERSE) FOR aux_pausa
                      IF aux_pausa  MATCHES "[SsNn]" THEN
                          IF aux_pausa MATCHES "[Nn]" THEN
                              RETURN
                          ELSE
                              UPDATE afi_solicitud
                              SET    status_interno = 5
                              WHERE  n_folio = r_afi.n_folio
                              AND    tipo_solicitud = r_afi.tipo_solicitud
                              ERROR "Registro actualizado con status de rechazo"
                              SLEEP 3
                              ERROR  ""
                              DISPLAY " SOLICITUD INCOMPLETA " AT 5,56 ATTRIBUTE(REVERSE)
                              EXIT WHILE
                          END IF
                      ELSE
                          DISPLAY "Solo debe presionar (S)i o (N)o" AT 19,2
                      END IF
                      END WHILE
                  ELSE
                      PROMPT "Status no puede modificarse, [Enter] p/continuar" FOR enter
                      RETURN
                  END IF
              WHEN 2
                  IF su_estatus <= 20 OR
                     su_estatus > 5 THEN
                      WHILE TRUE
                      PROMPT "Esta seguro de rechazar solicitud [S/N] ? " 
                      ATTRIBUTES (REVERSE) FOR aux_pausa
                      IF aux_pausa  MATCHES "[SsNn]" THEN
                          IF aux_pausa MATCHES "[Nn]" THEN
                              RETURN
                          ELSE
                              UPDATE afi_solicitud
                              SET    status_interno = 5
                              WHERE  n_folio = r_afi.n_folio
                              AND    tipo_solicitud = r_afi.tipo_solicitud
                              ERROR "Registro actualizado con status de rechazo"
                              SLEEP 3
                              ERROR  ""
                              DISPLAY " SOLICITUD INCOMPLETA " AT 5,56 ATTRIBUTE(REVERSE)
                              EXIT WHILE
                          END IF
                      ELSE
                          DISPLAY "Solo debe presionar (S)i o (N)o" AT 19,2
                      END IF
                      END WHILE
                  ELSE
                      PROMPT "Status no puede modificarse, [Enter] p/continuar" FOR enter
                      RETURN
                  END IF
              WHEN 3
                  PROMPT "Funcion no valida para este tipo de solicitud, [Enter] p/continuar" FOR enter
                  RETURN
              WHEN 4
                  PROMPT "Funcion no valida para este tipo de solicitud, [Enter] p/continuar" FOR enter
                  RETURN
              WHEN 5
                  PROMPT "Funcion no valida para este tipo de solicitud, [Enter] p/continuar" FOR enter
                  RETURN
              OTHERWISE
                  PROMPT "Tipo de solicitud invalido, [Enter] p/continuar" FOR enter
                  RETURN
          END CASE

          --ON KEY ( CONTROL-W )
          ON KEY ( CONTROL-Z )
              CASE r_afi.tipo_solicitud
                  WHEN 1
                      IF su_estatus = 5 THEN
                          WHILE TRUE
                          PROMPT "Esta seguro de aceptar solicitud [S/N] ? " 
                          ATTRIBUTES (REVERSE) FOR aux_pausa
                          IF aux_pausa  MATCHES "[SsNn]" THEN
                              IF aux_pausa MATCHES "[Nn]" THEN
                                  RETURN
                              ELSE
                                  UPDATE afi_solicitud
                                  SET    status_interno = 10
                                  WHERE  n_folio = r_afi.n_folio
                                  AND    tipo_solicitud = r_afi.tipo_solicitud
                                  ERROR "Registro actualizado con status de aceptada"
                                  SLEEP 3
                                  ERROR  ""
                                  DISPLAY " SOLICITUD COMPLETA   " AT 5,56 ATTRIBUTE(REVERSE)
                                  EXIT WHILE
                              END IF
                          ELSE
                              DISPLAY "Solo debe presionar (S)i o (N)o" AT 19,2
                          END IF
                          END WHILE
                      END IF
                  WHEN 2
                      IF su_estatus = 5 THEN
                          WHILE TRUE
                          PROMPT "Esta seguro de aceptar solicitud [S/N] ? " 
                          ATTRIBUTES (REVERSE) FOR aux_pausa
                          IF aux_pausa  MATCHES "[SsNn]" THEN
                              IF aux_pausa MATCHES "[Nn]" THEN
                                  RETURN
                              ELSE
                                  UPDATE afi_solicitud
                                  SET    status_interno = 10
                                  WHERE  n_folio = r_afi.n_folio
                                  AND    tipo_solicitud = r_afi.tipo_solicitud
                                  ERROR "Registro actualizado con status de aceptada"
                                  SLEEP 3
                                  ERROR  ""
                                  DISPLAY " SOLICITUD COMPLETA   " AT 5,56 ATTRIBUTE(REVERSE)
                                  EXIT WHILE
                              END IF
                          ELSE
                              DISPLAY "Solo debe presionar (S)i o (N)o" AT 19,2
                          END IF
                          END WHILE
                      END IF
                  WHEN 3
                      PROMPT "Funcion no valida para este tipo de solicitud, [Enter] p/continuar" FOR enter
                      RETURN
                  WHEN 4
                      PROMPT "Funcion no valida para este tipo de solicitud, [Enter] p/continuar" FOR enter
                      RETURN
                  WHEN 5
                      PROMPT "Funcion no valida para este tipo de solicitud, [Enter] p/continuar" FOR enter
                      RETURN
                  OTHERWISE
                      PROMPT "Tipo de solicitud invalido, [Enter] p/continuar" FOR enter
                      RETURN
              END CASE

        ON KEY ( INTERRUPT )
            CALL Inicializa()
            DISPLAY "                                     " AT 5,56
            ATTRIBUTE(REVERSE)

        EXIT INPUT

        ON KEY ( ESC )
            IF r_afi.codven IS NULL THEN
                ERROR "Campo NO puede ser NULO"  
                NEXT FIELD codven
            END IF

            SELECT "X"
            FROM   pro_mae_promotor A
            WHERE  A.cod_promotor = r_afi.cod_promotor

            IF STATUS = NOTFOUND THEN
                ERROR "Promotor Inexistente"
                NEXT FIELD cod_promotor
            END IF

            SELECT "X"
            FROM   pro_mae_promotor A
            WHERE  A.cod_promotor = r_afi.codven

            IF STATUS =  NOTFOUND THEN
                ERROR "Referente Inexistente"
                NEXT FIELD codven
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

            LET g_n_operac = 0

            IF g_n_operac IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD n_operac
            END IF

            IF r_afi.frecafor IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD frecafor
            END IF

            IF r_afi.Fecha_elaboracion IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD fecha_elaboracion
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

            IF g_edo_civil IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD edo_civil
            END IF

            IF r_afi.fena IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD fena
            END IF

            IF g_salario_base_comis IS NULL THEN
                ERROR "Campo NO puede ser NULO" 
                NEXT FIELD salario_base_comis
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
                ERROR "No puede dar de alta mientras no ingrese direcciones de la solicitud"
                NEXT FIELD tipo_solicitud
            END IF

            LET tot_sief = 0


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

    WHILE TRUE
        CALL Desea_modificar()
        IF aux_pausa MATCHES "[SsNn]" THEN
            EXIT WHILE
        END IF
    END WHILE

        IF aux_pausa MATCHES "[Nn]" THEN
            RETURN
        ELSE
          IF r_afi.tipo_solicitud = 2 AND
             su_estatus = 40 THEN
            WHILE TRUE
                PROMPT "MARCA REENVIO [0]Reenvio dev./[1]Error captura/[2]Confirma datos: " ATTRIBUTES (REVERSE) FOR vreenv

                IF vreenv  MATCHES "[012]" THEN
                    EXIT WHILE
                ELSE
                    ERROR "Solo debe presionar [0] , [1] o [2]"
                    SLEEP 3
                    ERROR " "
                END IF
            END WHILE
          END IF

            UPDATE afi_solicitud 
            SET    cod_promotor       = r_afi.cod_promotor,
                   agenc_cod          = r_afi.agenc_cod,
                   paterno            = r_afi.paterno,
                   materno            = r_afi.materno,
                   nombres            = r_afi.nombres,
                   n_seguro           = r_afi.n_seguro,
                   n_rfc              = r_afi.n_rfc,
                   n_unico            = r_afi.n_unico,
                   sexo               = r_afi.sexo,
                   edo_civil          = g_edo_civil,
                   fena               = r_afi.fena,
                   salario_base_comis = g_salario_base_comis,
                   estadon            = r_afi.estadon,
                   nacionalidad       = r_afi.nacionalidad,
                   tip_prob           = r_afi.tip_prob,
                   fol_prob           = r_afi.fol_prob,
                   doc_prob           = r_afi.doc_prob,
                   ind_infonavit      = r_afi.ind_infonavit,
                   cod_esq_comision   = g_comision.cod_esq_comision,
                   cod_error_origen   = cod_err,
                   tipo_solicitud     = r_afi.tipo_solicitud,
                   femision           = r_afi.fecha_emision,
                   folio_edo_cta      = g_folio_edo_cta,
                   cod_afore_ced      = r_afi.cod_afore_ced,
                   frecafor           = r_afi.frecafor,
                   documento_2        = vreenv
            WHERE  n_folio            = r_afi.n_folio
            AND    tipo_solicitud     = r_afi.tipo_solicitud

          # issa 
          CALL Inserta_en_tabla_afi_ctr_actividad(r_afi.n_seguro, r_afi.n_folio, r_afi.tipo_solicitud)

            LET operacion  = 'RECHAZO MODIFICADO'
            LET su_estatus = 40

            IF xx_status = 40 OR
               xx_status = 45 THEN
                CALL Desea_reenviar()
                IF aux_pausa MATCHES "[Ss]" THEN
                    LET operacion  = 'RECHAZO A REENVIAR'
                    LET su_estatus = 20

                    UPDATE afi_solicitud
                    SET    status_interno = su_estatus,
                           documento_1    = null      ,
                           documento_3    = null      ,
                           documento_4    = null      ,
                           documento_5    = null      ,
                           indicador_d    = null
                    WHERE  n_folio        = r_afi.n_folio
                    AND    tipo_solicitud = r_afi.tipo_solicitud
                END IF
            END IF

	    {
            IF v_an = 0 THEN
               UPDATE afi_identifica
                  SET codigo      = no_identif,
                      descripcion = n_iden_d,
                      fecha       = TODAY
                WHERE n_seguro = r_afi.n_seguro
                  AND fecha    = fe_identif
            END IF
	    }

            CALL inserta_logico()

        END IF
   
        ERROR "REGISTRO MODIFICADO" SLEEP 2 ERROR ""
        CALL Inicializa()
        NEXT FIELD tipo_solicitud 

        ON KEY ( CONTROL-E,CONTROL-T,CONTROL-B,CONTROL-V,
                 CONTROL-P,CONTROL-U )
            SELECT a.status_interno 
            INTO   xx_status 
            FROM   afi_solicitud a
            WHERE  a.n_seguro = r_afi.n_seguro
            AND    a.n_folio  = r_afi.n_folio
            AND    a.tipo_solicitud = r_afi.tipo_solicitud
 
            CASE xx_status 
                WHEN 30 ERROR "NO PUEDE MODIFICAR ENVIADO A PROCESAR" 
                    NEXT FIELD tipo_solicitud
                WHEN 42 ERROR "NO PUEDE MODIFICAR AFILIACION RECHAZADA" 
                    NEXT FIELD tipo_solicitud
                --WHEN 45 ERROR "NO PUEDE MODIFICAR TRASPASO RECHAZADO" 
                    --NEXT FIELD tipo_solicitud
                WHEN 50 ERROR "NO PUEDE MODIFICAR REGISTRO PENDIENTE" 
                    NEXT FIELD tipo_solicitud
                WHEN 55 ERROR "NO PUEDE MODIFICAR REGISTRO ACLARACION" 
                    NEXT FIELD tipo_solicitud
                WHEN 60 ERROR "NO PUEDE MODIFICAR REGISTRO APROBADO" 
                    NEXT FIELD tipo_solicitud
                WHEN 70 ERROR "NO PUEDE MODIFICAR REGISTRO CERTIF. NO LIQ."
                    NEXT FIELD tipo_solicitud
                WHEN 75 ERROR "NO PUEDE MODIFICAR REGISTRO CERTIF. LIQ."
                    NEXT FIELD tipo_solicitud   
                WHEN 90 ERROR "NO PUEDE MODIFICAR REGISTRO CANCELADO" 
                    NEXT FIELD tipo_solicitud
                WHEN 100 ERROR "NO PUEDE MODIFICAR REGISTRO CERTIFICADO" 
                    NEXT FIELD tipo_solicitud
                OTHERWISE
                    LET KEY = FGL_LASTKEY()
 
                    CASE KEY
                      WHEN 2
                          LET comma = "fglgo AFIM004 ",
                          r_afi.n_seguro," ",
                          ACCION CLIPPED," ",
                          r_afi.tipo_solicitud,
                          r_afi.n_folio
                      WHEN 5
                    LET comma = "fglgo AFIM002.4gi ", r_afi.n_folio," ",
                                ACCION CLIPPED,"  ",vcont, " ",
                                r_afi.tipo_solicitud

                      WHEN 14
                         { LET comma = "fglgo AFIM006 ",
                          r_afi.n_seguro," ",
                          ACCION CLIPPED," ",
                          r_afi.tipo_solicitud,
                          r_afi.n_folio}
                      WHEN 16
                          LET comma = "fglgo AFIM025 ",
                          r_afi.n_folio, " ",
                          r_afi.tipo_solicitud," ",
                          ACCION CLIPPED
                      WHEN 20
                          LET comma = "fglgo AFIM003 ",
                          r_afi.n_seguro," ",
                          ACCION CLIPPED," ",
                          r_afi.tipo_solicitud,
                          r_afi.n_folio
		      {
                      WHEN 21
                               LET v_an = 0
                               CALL n_identiff_m(r_afi.n_seguro) 
                               RETURNING v_an, no_identif,n_iden_d,fe_identif
                      }
                      WHEN 22
                          LET comma = "fglgo AFIM005 ",
                          r_afi.n_seguro," ",
                          ACCION CLIPPED," ",
                          r_afi.tipo_solicitud,
                          r_afi.n_folio
                      WHEN 7
                          IF r_afi.n_folio > 0 THEN
                             CALL Ingresa_Esquema_Comision("C",r_afi.n_folio)
                          ELSE
      ERROR "No puede ver Esquema Comision sin antes especificar la solicitud"
                          END IF
                     END CASE

                    IF KEY != 21 THEN
                       RUN comma
                    END IF
            END CASE

        ON KEY (CONTROL-Y)

            CALL n_identif_m(r_afi.n_seguro, r_afi.n_folio, r_afi.tipo_solicitud)


        ON KEY (CONTROL-W)
          CALL observaciones(r_afi.n_seguro, r_afi.n_folio, r_afi.tipo_solicitud)
        END INPUT

END FUNCTION

FUNCTION Modifica_folio()
#mf----------------------

    DEFINE desp      CHAR(40)
    DEFINE consul    CHAR(40)
    DEFINE folio_ant DECIMAL(10,0)
    DEFINE folio_nvo DECIMAL(10,0)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " MODIFICA FOLIO " AT 1,63 ATTRIBUTE(REVERSE)

    LET g_n_operac = NULL

    INPUT BY NAME r_afi.tipo_solicitud,
                  r_afi.n_folio,
                  r_afi.n_seguro WITHOUT DEFAULTS

    AFTER FIELD tipo_solicitud
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD tipo_solicitud
        END IF

        IF r_afi.tipo_solicitud IS NULL THEN
            ERROR "Campo Tipo de Registro NO puede ser nulo"
            NEXT FIELD tipo_solicitud
        END IF

        SELECT ts.desc_solicitud
        INTO   r_afi.desc_solicitud
        FROM   tab_tipo_solic ts
        WHERE  ts.tipo_solicitud = r_afi.tipo_solicitud

        IF SQLCA.SQLCODE = 0 THEN
            DISPLAY r_afi.desc_solicitud 
        ELSE
            ERROR "Tipo Solic. solo puede ser 1)Reg. 2)Trasp. 3)Unif. 4)Virtual 5)Asig "
            LET r_afi.tipo_solicitud = NULL
            NEXT FIELD tipo_solicitud
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
                LET folio_ant = r_afi.n_folio
            END IF

            IF su_estatus <= 20 OR
               su_estatus = 40 THEN
                PROMPT "Ingrese el nuevo numero de folio : "
                ATTRIBUTES (REVERSE) FOR folio_nvo

              {
                IF r_afi.tipo_solicitud = 1 AND
                   folio_nvo < 2900101 THEN
                    ERROR "Folio de solicitud está vencido"
                    SLEEP 3
                    ERROR ""
                    RETURN
                END IF

                IF r_afi.tipo_solicitud = 2 AND
                   folio_nvo < 205001 THEN
                   ERROR "Folio de solicitud está vencido"
                   SLEEP 3
                   ERROR ""
                   RETURN
                END IF 
              }

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

                        #### Val_fol

			SELECT "X"
			FROM    afi_mae_modifica 
			WHERE   @folio_nvo      = folio_nvo
			AND 	tipo_solicitud = r_afi.tipo_solicitud
			AND     cod_operacion  = 0
			AND     diag_proceso   = 0
			GROUP BY 1

			IF STATUS = NOTFOUND  THEN
                        #### Val_fol

             # isabel 
                    {    IF folio_nvo IS NULL THEN
                          RETURN
                        END IF}

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

                        LET operacion = 'MODIFICA FOLIO'
                        CALL inserta_logico()

                        IF NOT Rescata_datos("N",r_afi.n_folio) THEN
                            ERROR "Afiliado NO existe"
                            NEXT FIELD n_folio
                        END IF

                        DISPLAY BY NAME r_afi.*

                        PROMPT "Presione [Enter] para finalizar " FOR enter
                        EXIT INPUT

                        #### Val_fol
			ELSE 
                            ERROR "Folio ya ingresado anteriormente, mod"
                            SLEEP 3
                            EXIT INPUT
			END IF
                        #### Val_fol
                ELSE
                    ERROR "Folio ya ingresado anteriormente, sol"
                    SLEEP 3
                    EXIT INPUT
                END IF
            ELSE
                ERROR "Folio ya ingresado anteriormente, mae"
                SLEEP 3
                EXIT INPUT
            END IF
        ELSE
            PROMPT "No se puede modificar el numero de folio " FOR enter
            EXIT INPUT
        END IF

        AFTER FIELD n_seguro
	    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
	        NEXT FIELD tipo_solicitud
	    END IF

	    IF r_afi.n_seguro IS NULL THEN
	        NEXT FIELD n_folio
	    ELSE
	        IF NOT Rescata_datos("C",r_afi.n_seguro) THEN
		    ERROR "Afiliado NO existe"
		    NEXT FIELD n_seguro
	        END IF

	        DISPLAY BY NAME r_afi.*
                LET folio_ant = r_afi.n_folio
	    END IF

        ON KEY (INTERRUPT)
            CALL Inicializa()
            DISPLAY "                                     " AT 5,56 
	    ATTRIBUTE(REVERSE)
            # issa DISPLAY "                                                                               " AT 16,01 
	    # issa ATTRIBUTE(REVERSE)
            EXIT INPUT

        END INPUT 

END FUNCTION

FUNCTION Desea_reenviar()
#dr----------------------

    PROMPT "Desea Reenviar a Certificar la Solicitud [S/N]? " FOR aux_pausa

END FUNCTION

FUNCTION Desea_modificar()
#dm-----------------------

    PROMPT "Desea Modificar la Informacion [S/N]? " FOR aux_pausa

END FUNCTION

FUNCTION Despliega()
#d-----------------

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
        n_unico	       CHAR(18),
        tipo_solicitud SMALLINT,
        n_folio         DECIMAL(10,0),
        nombre         CHAR(50)
    END RECORD

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

    DEFINE valor    		SMALLINT
    DEFINE l_estado 		CHAR(21)
    DEFINE x_fecha  		DATE
    DEFINE x_fecha_envio 	DATE

	
    SELECT tsa.estado_desc
    INTO   l_estado
    FROM   tab_status_afi tsa
    WHERE  tsa.estado_cod = valor

    DISPLAY l_estado CLIPPED,"                            " 
    AT 5,56 ATTRIBUTE(REVERSE)


    LET x_fecha       = NULL
    LET x_finicta     = NULL
    LET x_fecha_envio = NULL

    SELECT fentcons , finicta, fecha_envio
    INTO   x_fecha, x_finicta, x_fecha_envio
    FROM   afi_solicitud
    WHERE  n_seguro = r_afi.n_seguro
    AND    n_folio  = r_afi.n_folio
    AND    tipo_solicitud = r_afi.tipo_solicitud
 

    DISPLAY "F. Envio. ",x_fecha_envio USING "dd-mm-yyyy" AT 13,2 
    ATTRIBUTE(REVERSE)
    DISPLAY "F.Certif. ",x_fecha USING "dd-mm-yyyy" AT 13,57 ATTRIBUTE(REVERSE)

    IF x_finicta IS NOT NULL THEN
        DISPLAY "F.Apert.Cta. ",x_finicta USING "dd-mm-yyyy" 
        AT 13,31 ATTRIBUTE(REVERSE)
    END IF  

END FUNCTION

FUNCTION motivo_rechazo()
#mr----------------------

    DEFINE a ARRAY[100] OF RECORD
        codigo      SMALLINT,
        descripcion CHAR(80),
        cve_afore   SMALLINT,
        desc_afore  CHAR(14),
        fech_rech   DATE    ,
        n_pcanase   CHAR(50)
    END RECORD

    DEFINE i         SMALLINT
    DEFINE j         SMALLINT
    DEFINE fech_rech DATE

    OPEN WINDOW v34  at 6,3 with form "AFIM0013" attribute(border)
    DISPLAY " [ Ctrl_c ] Salir " AT 1,1
    DISPLAY "                                MOTIVOS DE RECHAZO                                 " AT 3,1 ATTRIBUTE(REVERSE)

    DELETE FROM tmp_mot_rch

    INSERT INTO tmp_mot_rch
    SELECT rdeta_cod, observacion, codigo_afore, '', f_rechazo, nombre_pcanase
      FROM afi_rechaza_cert
     WHERE @n_seguro       = r_afi.n_seguro
       AND @n_folio        = r_afi.n_folio
       AND @tipo_solicitud = r_afi.tipo_solicitud

    INSERT INTO tmp_mot_rch
    SELECT motivo_rechazo, desc_rech, cve_ced_cuenta, '', fecha_presentacion,
           ''
      FROM taa_det_devol, 
            tab_dev_taa
     WHERE n_seguro       = r_afi.n_seguro
       AND motivo_rechazo = cod_rech


    INSERT INTO tmp_mot_rch
    SELECT 14, 'NO ATENDIDA', cve_ced_cuenta, '', fecha_presentacion, ''
      FROM taa_det_no_aten
     WHERE n_seguro = r_afi.n_seguro

    DECLARE cursor_o CURSOR FOR 
     SELECT *
       FROM tmp_mot_rch
      ORDER BY fech_rech DESC

        {SELECT rdeta_cod, 
               observacion, 
               codigo_afore,
               "",  
               f_rechazo,
               nombre_pcanase
        FROM   afi_rechaza_cert
        WHERE  n_seguro = r_afi.n_seguro
        AND    n_folio  = r_afi.n_folio
        AND    tipo_solicitud = r_afi.tipo_solicitud
        ORDER BY f_rechazo desc}

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
                   WHERE  trd.rdeta_cod  = a[i].codigo
                   AND    trd.modulo_cod = 'afi'
                ELSE
                   SELECT trd.rdeta_desc_c
		   INTO   a[i].descripcion
                   FROM   tab_rdeta trd
                   WHERE  trd.rdeta_cod  = a[i].codigo
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

FUNCTION Ingresa_Esquema_comision(aux_pausa,x_num_folio)
#iec----------------------------------------------------

    DEFINE aux_pausa   CHAR(1)
    DEFINE x_num_folio INTEGER

    OPEN WINDOW ventanilla_a AT 9,10 WITH FORM "AFIM0014" ATTRIBUTE(BORDER)

    DISPLAY "                  E S Q U E M A    C O M I S I O N                " AT 3,1 ATTRIBUTE(REVERSE)

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

            ON KEY ( INTERRUPT )
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
        WHEN "C"
            DISPLAY "" AT 1,1
            DISPLAY "" AT 2,1
            DISPLAY " CONSULTA " AT 1,55 ATTRIBUTE(REVERSE)
            DISPLAY " [ Ctrl-C ] Salir " AT 2,2

            SELECT cod_esq_comision,"" 
            INTO   g_comision.cod_esq_comision,g_comision.desc_esq_comision
            FROM   afi_solicitud
            WHERE  n_folio = x_num_folio
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
            DISPLAY " MODIFICA " AT 1,55 ATTRIBUTE(REVERSE)
            DISPLAY " [ Esc ] Modificar       [ Ctrl-C ] Salir " AT 2,2

            SELECT cod_esq_comision,""
            INTO   g_comision.cod_esq_comision,g_comision.desc_esq_comision
            FROM   afi_solicitud
            WHERE  n_folio = x_num_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud

	    SELECT desc_esq_comision 
            INTO   g_comision.desc_esq_comision 
	    FROM   com_esq_comis
	    WHERE  cod_esq_comision = g_comision.cod_esq_comision

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

                ON KEY ( INTERRUPT )
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
        codigo	    SMALLINT,
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

    OPEN WINDOW ventanilla_b AT 9,10 WITH FORM "AFIM0016" ATTRIBUTE(BORDER)

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

    OPEN WINDOW AFIM0017 AT 7,4 WITH FORM "AFIM0017" ATTRIBUTE(BORDER)
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
    END IF

    CALL SET_COUNT(seguro_cnt-1)

    LET int_flag = FALSE
    DISPLAY ARRAY rec_1 TO rec.*

    LET idx = ARR_CURR()
        IF int_flag THEN
            LET int_flag = FALSE
            LET rec_1[idx].n_seguro = NULL
        END IF

END FUNCTION

FUNCTION inserta_logico()
#il----------------------

    LET g_hora  = TIME

    INSERT INTO afi_ctr_logico
    VALUES (r_afi.n_folio,
            r_afi.tipo_solicitud,
            r_afi.n_seguro,
            su_estatus,
            g_usuario,
            hoy,
            g_hora,
            operacion)

END FUNCTION

FUNCTION Modifica_nss()
#mf----------------------

    DEFINE desp      CHAR(40)
    DEFINE consul    CHAR(40)
    DEFINE nss_ant   CHAR(11)
    DEFINE nss_nvo   CHAR(11)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " MODIFICA NSS " AT 1,63 ATTRIBUTE(REVERSE)


    INPUT BY NAME r_afi.tipo_solicitud,
                  r_afi.n_folio,
                  r_afi.n_seguro WITHOUT DEFAULTS

    AFTER FIELD tipo_solicitud
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD tipo_solicitud
        END IF

        IF r_afi.tipo_solicitud IS NULL THEN
            ERROR "Campo Tipo de Registro NO puede ser nulo"
            NEXT FIELD tipo_solicitud
        END IF

        SELECT ts.desc_solicitud
        INTO   r_afi.desc_solicitud
        FROM   tab_tipo_solic ts
        WHERE  ts.tipo_solicitud = r_afi.tipo_solicitud

        IF SQLCA.SQLCODE = 0 THEN
            DISPLAY r_afi.desc_solicitud 
        ELSE
            ERROR "Tipo Solic. solo puede ser 1)Reg. 2)Trasp. 3)Unif. 4)Virtual 5)Asig "
            LET r_afi.tipo_solicitud = NULL
            NEXT FIELD tipo_solicitud
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
                LET nss_ant = r_afi.n_folio
            END IF

            IF su_estatus <= 20 OR
               su_estatus = 40 THEN
                PROMPT "Ingrese el nuevo numero de seguridad social : "
                ATTRIBUTES (REVERSE) FOR nss_nvo

            IF nss_nvo IS NULL THEN
               NEXT FIELD n_folio
            END IF 

            CALL  digito_verif(nss_nvo[1,10],10) RETURNING digito

            IF digito = 32000 THEN
                ERROR "N.S.S. solo contiene digitos"
                NEXT FIELD n_seguro
            END IF

            IF LENGTH(nss_nvo) = 11 AND
               digito <> nss_nvo[11] THEN
            ERROR "Digito Verificador Invalido, el digito debe ser:  ",digito
                SLEEP 3
                NEXT FIELD n_seguro
            END IF

            IF nss_nvo[11] <> "1" AND
               nss_nvo[11] <> "2" AND
               nss_nvo[11] <> "3" AND
               nss_nvo[11] <> "4" AND
               nss_nvo[11] <> "5" AND
               nss_nvo[11] <> "6" AND
               nss_nvo[11] <> "7" AND
               nss_nvo[11] <> "8" AND
               nss_nvo[11] <> "9" AND
               nss_nvo[11] <> "0" THEN
                ERROR "N.S.S. solo contiene digitos"
                NEXT FIELD n_seguro 
            END IF

                SELECT "X"
                FROM   afi_mae_afiliado
                WHERE  n_seguro = nss_nvo

                IF STATUS = NOTFOUND THEN
                    SELECT "X"
                    FROM   afi_solicitud
                    WHERE  n_seguro = nss_nvo

                    IF STATUS = NOTFOUND THEN
                        ERROR "Actualizando registro "

                        UPDATE afi_solicitud
                        SET    n_seguro = nss_nvo
                        WHERE  n_seguro = r_afi.n_seguro
                        AND    n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud 

                        UPDATE afi_patron
                        SET    n_seguro = nss_nvo
                        WHERE  n_seguro = r_afi.n_seguro
                        AND    n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud 

                        UPDATE afi_domicilio
                        SET    nss = nss_nvo
                        WHERE  nss = r_afi.n_seguro
                        AND    n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud 

                        UPDATE afi_telefono
                        SET    nss = nss_nvo
                        WHERE  nss = r_afi.n_seguro
                        AND    n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud 

                        UPDATE afi_icefa
                        SET    n_seguro = nss_nvo
                        WHERE  n_seguro = r_afi.n_seguro
                        AND    n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud 

                        UPDATE afi_beneficiario
                        SET    n_seguro = nss_nvo
                        WHERE  n_seguro = r_afi.n_seguro
                        AND    n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud 

                        WHENEVER ERROR CONTINUE

                        UPDATE afi_recepcion
                        SET    n_seguro = nss_nvo
                        WHERE  n_seguro = r_afi.n_seguro
                        AND    n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud 

                        UPDATE afi_expediente
                        SET    n_seguro = nss_nvo
                        WHERE  n_seguro = r_afi.n_seguro
                        AND    n_folio = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud 

                        WHENEVER ERROR STOP

                        LET r_afi.n_seguro = nss_nvo

                        LET operacion = 'MODIFICA NSS'
                        CALL inserta_logico()

                        IF NOT Rescata_datos("N",r_afi.n_folio) THEN
                            ERROR "Afiliado NO existe"
                            NEXT FIELD n_folio
                        END IF

                        DISPLAY BY NAME r_afi.*

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
        ELSE
            PROMPT "No se puede modificar el NSS" FOR enter
            EXIT INPUT
        END IF

        AFTER FIELD n_seguro
	    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
	        NEXT FIELD tipo_solicitud
	    END IF

	    IF r_afi.n_seguro IS NULL THEN
	        NEXT FIELD n_folio
	    ELSE
	        IF NOT Rescata_datos("C",r_afi.n_seguro) THEN
		    ERROR "Afiliado NO existe"
		    NEXT FIELD n_seguro
	        END IF

	        DISPLAY BY NAME r_afi.*
                LET nss_ant = r_afi.n_seguro
	    END IF

        ON KEY (INTERRUPT)
            CALL Inicializa()
            DISPLAY "                                     " AT 5,56 
	    ATTRIBUTE(REVERSE)
            # issa DISPLAY "                                                                               " AT 16,01 
	    # ATTRIBUTE(REVERSE)
            EXIT INPUT

        END INPUT 

END FUNCTION

FUNCTION Modifica_pro()
#mf----------------------

    DEFINE desp      CHAR(40)
    DEFINE consul    CHAR(40)
    DEFINE pro_ant   CHAR(11)
    DEFINE pro_nvo   CHAR(11)
    DEFINE recha     SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " MODIFICA PROMOTOR " AT 1,58 ATTRIBUTE(REVERSE)

    LET g_n_operac = NULL

    INPUT BY NAME r_afi.tipo_solicitud,
                  r_afi.n_folio,
                  r_afi.n_seguro WITHOUT DEFAULTS

    AFTER FIELD tipo_solicitud
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD tipo_solicitud
        END IF

        IF r_afi.tipo_solicitud IS NULL THEN
            ERROR "Campo Tipo de Registro NO puede ser nulo"
            NEXT FIELD tipo_solicitud
        END IF

        SELECT ts.desc_solicitud
        INTO   r_afi.desc_solicitud
        FROM   tab_tipo_solic ts
        WHERE  ts.tipo_solicitud = r_afi.tipo_solicitud

        IF SQLCA.SQLCODE = 0 THEN
            DISPLAY r_afi.desc_solicitud
        ELSE
            ERROR "Tipo Solic. solo puede ser 1)Reg. 2)Trasp. 3)Unif. 4)Virtual 5)Asig "
            LET r_afi.tipo_solicitud = NULL
            NEXT FIELD tipo_solicitud
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
                LET pro_ant = r_afi.n_folio
            END IF

            IF su_estatus <= 20 OR
               su_estatus = 40 THEN
                PROMPT "Ingrese el nuevo codigo de promotor : "
                ATTRIBUTES (REVERSE) FOR pro_nvo

                SELECT A.status
                INTO   recha
                FROM   pro_mae_promotor A
                WHERE  A.cod_promotor = pro_nvo

                    CASE recha
                        WHEN 2 ERROR "PROMOTOR SUSPENDIDO" SLEEP 2
                            NEXT FIELD cod_promotor
                        WHEN 3 ERROR "PROMOTOR CANCELADO" SLEEP 2
                            NEXT FIELD cod_promotor
                        OTHERWISE
                        ERROR "PROMOTOR ACTIVO" SLEEP 1
                    END CASE

                    IF pro_nvo IS NULL THEN
                        NEXT FIELD tipo_solicitud
                    END IF 
                      
                        ERROR "Actualizando registro "

                        UPDATE afi_solicitud
                        SET    cod_promotor   = pro_nvo,
                               codven         = pro_nvo
                        WHERE  n_seguro       = r_afi.n_seguro
                        AND    n_folio        = r_afi.n_folio
                        AND    tipo_solicitud = r_afi.tipo_solicitud 

                        LET r_afi.cod_promotor = pro_nvo

                        LET operacion = 'MODIFICA PROMOTOR'
                        CALL inserta_logico()

                        IF NOT Rescata_datos("N",r_afi.n_folio) THEN
                            ERROR "Afiliado NO existe"
                            NEXT FIELD n_folio
                        END IF

                        DISPLAY BY NAME r_afi.*

                        PROMPT "Presione [Enter] para finalizar " FOR enter
                        EXIT INPUT
            ELSE
                PROMPT "No se puede modificar el Codigo de Promotor" FOR enter
                EXIT INPUT
            END IF

        AFTER FIELD n_seguro
	    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
	        NEXT FIELD tipo_solicitud
	    END IF

	    IF r_afi.n_seguro IS NULL THEN
	        NEXT FIELD n_folio
	    ELSE
	        IF NOT Rescata_datos("C",r_afi.n_seguro) THEN
		    ERROR "Afiliado NO existe"
		    NEXT FIELD n_seguro
	        END IF

	        DISPLAY BY NAME r_afi.*
                LET pro_ant = r_afi.n_seguro
	    END IF

        ON KEY (INTERRUPT)
            CALL Inicializa()
            DISPLAY "                                     " AT 5,56 
	    ATTRIBUTE(REVERSE)
           # issa  DISPLAY "                                                                               " AT 16,01 
	   # issa  ATTRIBUTE(REVERSE)
            EXIT INPUT

        END INPUT 

END FUNCTION

FUNCTION Elimina()
#e----------------

    DEFINE desp      CHAR(40)
    DEFINE consul    CHAR(40)
    DEFINE desc_ts   CHAR(12)
    DEFINE ee_status SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1

    DISPLAY " ELIMINA  " AT 1,69 ATTRIBUTE(REVERSE)
    DISPLAY "CONTROL:  [E] Elimina                                    [C] Salir" AT 1,1 ATTRIBUTE(BOLD)

    DISPLAY "                                                                               " AT 2,1 ATTRIBUTE(BOLD)

    LET g_n_operac = NULL
    LET r_afi.n_folio  = NULL

    INPUT BY NAME r_afi.tipo_solicitud,
                  r_afi.n_folio,
                  r_afi.n_seguro WITHOUT DEFAULTS

        AFTER FIELD tipo_solicitud

            IF r_afi.tipo_solicitud IS NULL THEN
                ERROR "Tipo de Solicitud NO puede ser NULO"
                LET r_afi.tipo_solicitud = NULL
                DISPLAY BY NAME r_afi.tipo_solicitud
                NEXT FIELD tipo_solicitud
            END IF

            SELECT tts.desc_solicitud
            INTO   r_afi.desc_solicitud
            FROM   tab_tipo_solic tts
            WHERE  tts.tipo_solicitud = r_afi.tipo_solicitud

            IF SQLCA.SQLCODE = 0 THEN
                DISPLAY r_afi.desc_solicitud
            ELSE
                ERROR "Tipo de Solicitud solo puede ser 1) reg. 2) trasp. 3) unif. 4) virtual 5) asig. "
                LET r_afi.tipo_solicitud = NULL
                DISPLAY BY NAME r_afi.tipo_solicitud
                NEXT FIELD tipo_solicitud
            END IF

            NEXT FIELD n_folio

        AFTER FIELD n_folio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                CALL Inicializa()
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
                NEXT FIELD n_folio
            ELSE
                IF NOT Rescata_datos("C",r_afi.n_seguro) THEN
                    ERROR "Afiliado NO existe"
                    NEXT FIELD n_seguro
                END IF

                DISPLAY BY NAME r_afi.*
            END IF

        ON KEY (CONTROL-E)

        SELECT a.status_interno
        INTO   ee_status
        FROM   afi_solicitud a
        WHERE  a.n_seguro = r_afi.n_seguro
        AND    a.n_folio = r_afi.n_folio
        AND    a.tipo_solicitud = r_afi.tipo_solicitud
                   
        CASE ee_status
            WHEN 30 ERROR "NO PUEDE MODIFICAR, ENVIADO A PROCESAR "
                NEXT FIELD tipo_solicitud
            WHEN 40 ERROR "NO PUEDE MODIFICAR, RECHAZO RECUPERABLE"
                NEXT FIELD tipo_solicitud
            WHEN 42 ERROR "NO PUEDE MODIFICAR, RECHAZO PROCESAR   "
                NEXT FIELD tipo_solicitud
            --WHEN 45 ERROR "NO PUEDE MODIFICAR, TRASPASO RECHAZADO "
                --NEXT FIELD tipo_solicitud
            WHEN 50 ERROR "NO PUEDE MODIFICAR, REGISTRO PENDIENTE "
                NEXT FIELD tipo_solicitud
            WHEN 55 ERROR "NO PUEDE MODIFICAR, REGISTRO EN ACLARACION "
                NEXT FIELD tipo_solicitud
            WHEN 60 ERROR "NO PUEDE MODIFICAR, REGISTRO APROBADO  "
                NEXT FIELD tipo_solicitud
            WHEN 70 ERROR "NO PUEDE MODIFICAR, REGISTRO CERTIF. NO LIQ. "
                NEXT FIELD tipo_solicitud
            WHEN 75 ERROR "NO PUEDE MODIFICAR, REGISTRO CERTIF. LIQ. "
                NEXT FIELD tipo_solicitud
            WHEN 90 ERROR "NO PUEDE MODIFICAR, REGISTRO PEND. ASIG.. "
                NEXT FIELD tipo_solicitud
            WHEN 100 ERROR "NO PUEDE MODIFICAR, REGISTRO CERTIFICADO  "
                NEXT FIELD tipo_solicitud
        END CASE  

            DELETE
            FROM   afi_icefa
            WHERE  n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            ;
            DELETE
            FROM   afi_beneficiario
            WHERE  n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            ;
            DELETE
            FROM   afi_telefono
            WHERE  n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            ;
            DELETE
            FROM   afi_domicilio
            WHERE  n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            ;
            DELETE 
            FROM   afi_condicion_exp 
            WHERE  n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            ;
            DELETE 
            FROM   afi_expediente
            WHERE  n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            ;
            DELETE 
            FROM   afi_recepcion
            WHERE  n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud
            ;
            DELETE
            FROM   afi_solicitud
            WHERE  n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud

            # issa 
            DELETE 
            FROM   afi_ctr_actividad
            WHERE  nss     = r_afi.n_seguro
            AND    n_folio = r_afi.n_folio
            AND    tipo_solicitud = r_afi.tipo_solicitud

            EXIT INPUT

        ON KEY ( INTERRUPT )
            CALL Inicializa()
            DISPLAY "                                     " AT 5,56  ATTRIBUTE(REVERSE)
            EXIT INPUT

    END INPUT

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
        LET sw_sup = FALSE 
        LET aux_sup = "S"
        EXIT INPUT

    END INPUT

    CLOSE WINDOW ventanilla_super

END FUNCTION

FUNCTION observaciones(vn_seguro, vn_folio, vtipo_solicitud)
#o----------------------------------------------------------

    DEFINE
        vn_seguro       CHAR(11),
        vn_folio         DECIMAL(10,0),
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
      INTO re_g.observacion
      FROM afi_ctr_observa a
     WHERE a.nss            = vn_seguro
       AND a.n_folio        = vn_folio
       AND a.tipo_solicitud = vtipo_solicitud

    IF SQLCA.SQLCODE = 0 THEN
        LET va = 2
    END IF

    OPEN WINDOW v101 AT 6,5 WITH FORM "AFIM00101" ATTRIBUTE(BORDER)

    DISPLAY " [ Enter ] Grabar        [ Ctrl_C ] Saliri                                     " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY "                     O B S E R V A C I O N E S                                 " AT 2,1 ATTRIBUTE(REVERSE)

    IF va = 2 THEN
        DISPLAY BY NAME observacion
    END IF

    INPUT BY NAME re_g.observacion WITHOUT DEFAULTS

    AFTER FIELD observacion
        SELECT "a.X"
          FROM afi_ctr_observa a
         WHERE a.nss     = vn_seguro
           AND a.n_folio = vn_folio
           AND a.tipo_solicitud = vtipo_solicitud

        IF SQLCA.SQLCODE <> 0 THEN
            INSERT INTO afi_ctr_observa
            VALUES (vn_seguro,
                    vn_folio,
                    vtipo_solicitud,
                    re_g.observacion,
                    g_usuario,
                    TODAY)
        ELSE
            UPDATE afi_ctr_observa
               SET afi_ctr_observa.observacion    = re_g.observacion
             WHERE afi_ctr_observa.nss            = vn_seguro
               AND afi_ctr_observa.n_folio        = vn_folio
               AND afi_ctr_observa.tipo_solicitud = vtipo_solicitud
        END IF

    PROMPT "ALTA EFECTUADA. [Enter] para continuar " FOR enter
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
        vn_folio         DECIMAL(10,0),
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
        RETURN
    END IF

    IF SQLCA.SQLCODE = 0 THEN
        LET va = 2
    END IF

    OPEN WINDOW v101 AT 6,5 WITH FORM "AFIM00101" ATTRIBUTE(BORDER)

    DISPLAY " [ Esc ] Grabar        [ Ctrl_C ] Salir                                        " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY "                     O B S E R V A C I O N E S                                 " AT 2,1 ATTRIBUTE(REVERSE)

    DISPLAY BY NAME observacion

    PROMPT "PRESIONE < ENTER > PARA CONTINUAR " FOR va
    CLOSE WINDOW v101

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

    INITIALIZE re_g, va, fecha TO NULL

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
    END IF

    OPEN WINDOW v12 AT 6,6 WITH FORM "AFIM0019"
    ATTRIBUTE(BORDER)
    DISPLAY "                                                                               " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "                     I D E N T I F I C A C I O N                               " AT 2,1 ATTRIBUTE(REVERSE)

    DISPLAY BY NAME re_g.*

    PROMPT "PRESIONE < ENTER > PARA CONTINUAR " FOR va
    CLOSE WINDOW v12

END FUNCTION
FUNCTION n_identif_m(vn_seguro, vn_folio, vtipo_solicitud)
#nim------------------------------------------------------

    DEFINE vn_seguro       CHAR(11)
    DEFINE vn_folio          DECIMAL(10,0)
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

    INITIALIZE re_g, va, vfecha TO NULL

    LET fe_hoy = TODAY

    SELECT MAX(a.fecha)
    INTO   vfecha
    FROM   afi_ctr_identif a
    WHERE  a.n_seguro = vn_seguro
    AND    a.n_folio  = vn_folio
    AND    a.tipo_solicitud = vtipo_solicitud

    IF STATUS = NOTFOUND THEN
        ERROR " NO EXISTE INFORMACION PARA ESTE NSS"
        SLEEP 3
        ERROR ""
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

    LET clave       = re_g.clave
    LET descripcion = re_g.descripcion
    LET identifica  = re_g.identifica

    OPEN WINDOW vv_2 AT  6,6 WITH FORM "AFIM0019" ATTRIBUTE(BORDER)

    DISPLAY " [ Esc ] Salvar        [ Ctrl_C ] Salir                             " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "                     I D E N T I F I C A C I O N                    " AT 2,1 ATTRIBUTE(REVERSE)

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
                CALL clave_i() RETURNING re_g.clave,re_g.descripcion

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

                IF SQLCA.SQLCODE <> 0 THEN
                    ERROR "Codigo Incorrecto, Verifique nuevamente"
                    NEXT FIELD clave
                END IF

                DISPLAY re_g.clave       TO clave
                DISPLAY re_g.descripcion TO descripcion
            END IF

        AFTER FIELD identifica
             IF re_g.identifica[1,1] = " " OR
                re_g.identifica IS NULL    THEN
                 ERROR "Digite correctamente la descripcion de la Identificacion"
                 NEXT FIELD identifica

             ELSE
                 UPDATE afi_ctr_identif
                    SET afi_ctr_identif.clave_identif  = re_g.clave,
                        afi_ctr_identif.identifica     = re_g.identifica,
                        afi_ctr_identif.fecha          = fe_hoy,
                        afi_ctr_identif.usuario        = g_usuario
                  WHERE afi_ctr_identif.n_seguro       = n_seguro
                    AND afi_ctr_identif.n_folio        = n_folio
                    AND afi_ctr_identif.tipo_solicitud = tipo_solicitud
             END IF

             PROMPT "MODIFICACION EFECTUADA. [Enter] para continuar"
             FOR enter
             EXIT INPUT

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


