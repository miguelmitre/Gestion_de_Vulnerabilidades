##############################################################################
#Programa PROM006  => MANTENIMIENTO DE SOLICITUDES DE EXAMEN A PROMOTORES     #
#Modulo            => PRO                                                     #
#Elaborado Por     => STEFANIE DANIELA VERA PIÑA                              #
#Fecha Elaboracion => 27-ABRIL-2007                                           #
#Modificacion      => ISABEL FONSECA FRIAS                                    #
#Fecha             => 30-Octubre-2008                                         #
#                  => Se efectuo cambios en Modificacion y Eliminacion        #
#                  => para que se considere por rfc y no por consecutivo      #
#                  => (v1)                                                    #
#Fecha             => 30-Septiembre-2008                                      #
#                  => Valida que el rfc no se repita con misma fecha          #
#                  => de examen                                               #
#                  => (v2)                                                    #
#Fecha             => 30-Septiembre-2008                                      #
#                  => La consulta de los catalogos se hara directa            #
#                  => (v3)                                                    #
#Fecha             => 04-Octubre-2008                                         #
#                  => Se agrega en el queri el filtro por numero de promotor  #
#                  => (v4)                                                    #
#Fecha             => 04-Octubre-2008                                         #
#                  => Las validacion para cuando la CURP y RFC son diferentes #
#                  => asi como las diferencias que existen en la construccion #
#                  => de la curp y rfc con respecto al que se catptura        #
#                  => que solo avise pero si permita la carga de la solicitud #
#                  => (v5)                                                    #
#Fecha             => 27-Octubre-2008                                         #
#                  => Se modificao el programa para que conserve los datos    #
#                  => que se envian en el 802 y los que se envia en el 803    #
#                  => los datos seria: Tipo de evento, fecha examen y         #
#                  => hora examen                                             #
#                  => (v6)                                                    #
###############################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg RECORD #glo #reg
        cod_promotor         LIKE pro_ctr_examen.cod_promotor,
        paterno              LIKE pro_ctr_examen.paterno,
        materno              LIKE pro_ctr_examen.materno,
        nombres              LIKE pro_ctr_examen.nombres,
        rfc                  LIKE pro_ctr_examen.rfc,
        curp                 LIKE pro_ctr_examen.curp,
        fecha_examen        LIKE pro_ctr_examen.fecha_examen,
        hora_examen         LIKE pro_ctr_examen.hora_examen,
        tpo_examen           LIKE pro_ctr_examen.tpo_examen,
        desc_tpo_examen      LIKE pro_tipo_examen.desc_tipo,
        evento           LIKE pro_ctr_examen.evento,
        desc_eve_802          LIKE pro_evento.desc_evento,
        sede                 LIKE pro_ctr_examen.sede,
        desc_sede            LIKE pro_sede_examen.desc_sede,
        edo_sede             LIKE pro_sede_examen.edo_sede,
        desc_edo_sede        LIKE tab_estado.estad_desc,
        num_vuelta           LIKE pro_ctr_examen.num_vuelta
    END RECORD

    DEFINE arr_1 ARRAY[1000] OF RECORD #glo #arr_1
        cod_promotor         LIKE pro_ctr_examen.cod_promotor,
        cve_solicitud        LIKE pro_ctr_examen.cve_solicitud,
        paterno              LIKE pro_ctr_examen.paterno,
        materno              LIKE pro_ctr_examen.materno,
        nombres              LIKE pro_ctr_examen.nombres,
        rfc                  LIKE pro_ctr_examen.rfc,
        curp                 LIKE pro_ctr_examen.curp,
        fecha_examen        LIKE pro_ctr_examen.fecha_examen,
        hora_examen          LIKE pro_ctr_examen.hora_examen,
        tpo_examen           LIKE pro_ctr_examen.tpo_examen,
        desc_tpo_examen      LIKE pro_tipo_examen.desc_tipo,
        evento           LIKE pro_ctr_examen.evento,
        desc_eve_802          LIKE pro_evento.desc_evento,
        sede                 LIKE pro_ctr_examen.sede,
        desc_sede            LIKE pro_sede_examen.desc_sede,
        edo_sede             LIKE pro_sede_examen.edo_sede,
        desc_edo_sede        LIKE tab_estado.estad_desc,
        num_vuelta           LIKE pro_ctr_examen.num_vuelta,
        evento_803           LIKE pro_ctr_examen.evento_803,
        desc_eve_803         LIKE pro_evento_803.desc_evento,
        fecha_exa_803        LIKE pro_ctr_examen.fecha_examen,
        hora_exa_803          LIKE pro_ctr_examen.hora_examen,
        calif                LIKE pro_ctr_examen.calif,
        cod_result           LIKE pro_result_examen.cod_result,
        desc_result          LIKE pro_result_examen.desc_result,
        consecutivo          LIKE pro_ctr_examen.consecutivo,
        estado_registro      LIKE pro_ctr_examen.estado_registro,
        desc_edo_reg         LIKE pro_status_interno.desc_status_corta
    END RECORD


    DEFINE #glo #date
        HOY                  DATE

    DEFINE #glo #char
        aaa                  CHAR(02),
        ayo_x                CHAR(10),
        dd                   CHAR(02),
        desc_err             CHAR(60),
        enter                CHAR(1),
        gerrflag             CHAR(30),
        mm                   CHAR(02),
        mcod_promo           CHAR(10),
        merror               CHAR(02),
        opc                  CHAR(1),
        rfc_arma             CHAR(10),
        v_rfc_arma           CHAR(4),
        sexo_cur             CHAR(01),
        txt_1                CHAR(3000),
        usuario              CHAR(10),
        val_1                CHAR(80),
        vestado_registro     CHAR(20),
        vrfc                 CHAR(13),
        x_busca              CHAR(1200),
        x_error              CHAR(500),
        v_status             SMALLINT,
        v_rfc                CHAR(13),                 --(v2)
        z_fecha              CHAR(10)

    DEFINE #glo #integer
        vconsecutivo         INTEGER


    DEFINE #glo #smallint
        ayo_s                ,
        ayo_1                ,
        ayo_2                ,
        dig_curp             ,
        i                    ,
        long                 ,
        mdigito_prom         ,
        pasa                 ,
        pasa_curp            ,
        sw_1                 ,
        total_reg            ,
        v_1                  ,
        ventro               ,
        wf_error             SMALLINT


    DEFINE
        cve_afore            LIKE tab_afore_local.codigo_afore
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG ("PROM006.log")
    CALL inicializa()

    LET HOY = TODAY
    INITIALIZE reg.* TO NULL

    OPEN WINDOW prom0061 AT 2,2 WITH FORM "PROM0061" ATTRIBUTE(BORDER)
    DISPLAY " PROM006                 SOLICITUD  DE  EXAMEN ",
            "                               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "SOLICITUDES "
        COMMAND KEY("A") "Agrega" "Agrega Solicitud de examen"
            CALL Agrega()
            CLEAR FORM

        COMMAND KEY("C") "Consulta" "Consulta Solicitud de examen"
            CALL Consulta()
            CLEAR FORM

        COMMAND KEY("M") "Modifica" "Modifica Solicitud de examen"
            CALL Modifica()
            CLEAR FORM

        COMMAND KEY("E") "Elimina" "Elimina Solicitud de examen"
            CALL elimina()
            CLEAR FORM

        COMMAND KEY("S") "Salir" "Salir del Programa"
            CLEAR FORM
            EXIT MENU
    END MENU

    CLOSE WINDOW prom0061
END MAIN


FUNCTION inicializa()
#-------------------
   INITIALIZE reg.*,cve_afore TO NULL

   SELECT USER
   INTO   usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "pro"

   SELECT codigo_afore
   INTO cve_afore
   FROM tab_afore_local
END FUNCTION


FUNCTION Agrega()
#----------------

    DEFINE #loc #date
        j_fecha        ,
        vfnaci         DATE

    INITIALIZE reg.* TO NULL

    IF int_flag THEN
        LET int_flag = FALSE
    END IF

    DISPLAY "                                               ",
            "                               " AT 3,1 ATTRIBUTE(NORMAL)
    DISPLAY "  Agrega             Ctrl-C: Salir                   "  AT 1,1 ATTRIBUTE(NORMAL)
    DISPLAY " ALTA " AT 1,69 ATTRIBUTE(REVERSE)
    DISPLAY " PROM006                 SOLICITUD  DE  EXAMEN ",
            "                               " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 2,65 ATTRIBUTE(REVERSE)
    DISPLAY "                         DATOS  DEL  PROMOTOR  ",
            "                               " AT 4,1 ATTRIBUTE(REVERSE)
    DISPLAY "                         DATOS  DEL  EXAMEN    ",
            "                               " AT 11,1 ATTRIBUTE(REVERSE)

    LET sw_1 = 0

    INPUT BY NAME reg.cod_promotor ,
                  reg.paterno      ,
                  reg.materno      ,
                  reg.nombres      ,
                  reg.rfc          ,
                  reg.curp         ,
                  reg.fecha_examen,
                  reg.hora_examen  ,
                  reg.tpo_examen   ,
                  reg.evento   ,
                  reg.sede         ,
                  reg.num_vuelta  WITHOUT DEFAULTS


        BEFORE FIELD cod_promotor
            IF sw_1 = 0 THEN


                 LET reg.num_vuelta = 1
                 LET vconsecutivo = 0


                 DISPLAY vconsecutivo TO consecutivo
                 DISPLAY reg.num_vuelta TO num_vuelta

                 LET sw_1 = 1
            END IF

        AFTER FIELD cod_promotor
            IF reg.cod_promotor IS NULL
            OR reg.cod_promotor = "0000000000" THEN
                LET reg.cod_promotor = "0000000000"
                DISPLAY reg.cod_promotor TO cod_promotor
                NEXT FIELD paterno
            ELSE
                SELECT b.paterno,
                       b.materno,
                       b.nombres,
                       b.rfc,
                       b.unico
                INTO   reg.paterno,
                       reg.materno,
                       reg.nombres,
                       reg.rfc    ,
                       reg.curp
                FROM   pro_mae_promotor b
                WHERE  b.cod_promotor = reg.cod_promotor

                IF SQLCA.SQLCODE = 0 THEN
                    DISPLAY reg.paterno TO paterno
                    DISPLAY reg.materno TO materno
                    DISPLAY reg.nombres TO nombres
                    DISPLAY reg.rfc     TO rfc
                    DISPLAY reg.curp    TO curp
                    NEXT FIELD paterno
                ELSE
                    SELECT b.paterno,
                           b.materno,
                           b.nombres,
                           b.rfc,
                           b.unico
                    INTO   reg.paterno,
                           reg.materno,
                           reg.nombres,
                           reg.rfc    ,
                           reg.curp
                    FROM   pro_solicitud b
                    WHERE  b.cod_promotor = reg.cod_promotor

                    IF SQLCA.SQLCODE = 0 THEN
                        DISPLAY reg.paterno TO paterno
                        DISPLAY reg.materno TO materno
                        DISPLAY reg.nombres TO nombres
                        DISPLAY reg.rfc     TO rfc
                        DISPLAY reg.curp    TO curp
                        NEXT FIELD paterno
                    ELSE
                        LET long = 0
                        IF reg.cod_promotor IS NOT NULL THEN
                            IF LENGTH(reg.cod_promotor) < 10 THEN
                                ERROR "  EL CODIGO DE PROMOTOR CONTIENE ",
                                      "MENOS DE 10 CARACTERES"
                                ATTRIBUTE(NORMAL)
                                NEXT FIELD cod_promotor
                            ELSE
                                INITIALIZE mcod_promo,merror TO NULL
                                LET mdigito_prom = 0
                                LET mcod_promo = reg.cod_promotor

                                FOR i = 1 TO 10
                                    IF mcod_promo[i] <> "0" AND
                                       mcod_promo[i] <> "1" AND
                                       mcod_promo[i] <> "2" AND
                                       mcod_promo[i] <> "3" AND
                                       mcod_promo[i] <> "4" AND
                                       mcod_promo[i] <> "5" AND
                                       mcod_promo[i] <> "6" AND
                                       mcod_promo[i] <> "7" AND
                                       mcod_promo[i] <> "8" AND
                                       mcod_promo[i] <> "9" THEN

                                          LET mdigito_prom = 1
                                          EXIT FOR
                                    END IF
                                END FOR

                                IF mdigito_prom = 1 THEN
                                    ERROR "  EL CODIGO DE PROMOTOR NO ES VALIDO "
                                    NEXT FIELD cod_promotor
                                END IF

                                INITIALIZE mcod_promo,merror TO NULL
                                LET mdigito_prom = 0

                                CALL Digito_prom(reg.cod_promotor CLIPPED)
                                    RETURNING mcod_promo, merror,mdigito_prom

                                IF merror NOT MATCHES "00" THEN
                                    ERROR "  DIGITO VERIFICADOR INVALIDO, EL DIGITO ",
                                          "DEBE SER : ",
                                          mdigito_prom ATTRIBUTE(NORMAL)
                                    WHILE TRUE
                                        PROMPT " PROMOTOR INCORRECTO DEBE DE SER :",
                                               mcod_promo,
                                               "... DESEA CONTINUAR  S/N ?"
                                        FOR CHAR enter

                                        IF enter MATCHES "[SsNn]" THEN
                                            EXIT WHILE
                                        END IF
                                    END WHILE

                                    IF enter MATCHES "[Ss]" THEN
                                        NEXT FIELD paterno
                                    ELSE
                                        NEXT FIELD cod_promotor
                                    END IF
                                ELSE
                                    NEXT FIELD paterno
                                END IF
                            END IF
                        END IF
                    END IF
                END IF
            END IF

        AFTER FIELD paterno
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF reg.paterno  IS NULL OR reg.paterno[1] = " " THEN
               ERROR "  EL APELLIDO PATERNO DEBE SER INGRESADO"
               ATTRIBUTE(NORMAL)
               NEXT FIELD paterno
            END IF

            IF reg.paterno[1] = "," OR
               reg.paterno[1] = ";" OR
               reg.paterno[1] = "!" OR
               reg.paterno[1] = '"' OR
               reg.paterno[1] = "#" OR
               reg.paterno[1] = "$" OR
               reg.paterno[1] = "%" OR
               reg.paterno[1] = "&" OR
               reg.paterno[1] = "/" OR
               reg.paterno[1] = "(" OR
               reg.paterno[1] = ")" OR
               reg.paterno[1] = "=" OR
               reg.paterno[1] = "?" OR
               reg.paterno[1] = "!" OR
               reg.paterno[1] = "'" OR
               reg.paterno[1] = "@" OR
               reg.paterno[1] = "'" OR
               reg.paterno[1] = ":" OR
               reg.paterno[1] = "^" OR
               reg.paterno[1] = "[" OR
               reg.paterno[1] = "]" OR
               reg.paterno[1] = "{" OR
               reg.paterno[1] = "}" OR
               reg.paterno[1] = "`" THEN
               ERROR "  EL APELLIDO PATERNO NO DEBE TENER CARACTERES ESPECIALES"
                     ATTRIBUTE(NORMAL)
               NEXT FIELD paterno
            END IF

        AFTER FIELD materno
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD PREVIOUS
            END IF

            IF reg.materno[1,2] = " ." OR
               reg.materno[1] = "."  OR reg.materno[1,2] = ".." OR
               reg.materno[1,2] = ".X"  THEN

                ERROR "  EL APELLIDO MATERNO NO PUEDE SER UN PUNTO o PUNTO  segido de  X "
                ATTRIBUTE(NORMAL)
                NEXT FIELD materno
            END IF

        AFTER FIELD nombres
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD PREVIOUS
            END IF

            IF reg.nombres  IS NULL OR reg.nombres[1] = " " THEN
                ERROR "  EL NOMBRE ES REQUERIDO" ATTRIBUTE(NORMAL)
                NEXT FIELD nombres
            END IF

        AFTER FIELD rfc
           IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
              NEXT FIELD PREVIOUS
           END IF

           IF reg.rfc IS NULL  OR  reg.rfc = " " THEN
               ERROR "  RFC NO puede ser NULO" ATTRIBUTE(NORMAL)
               NEXT FIELD rfc
           END IF

           IF LENGTH(reg.rfc CLIPPED) <> 10 AND
              LENGTH(reg.rfc CLIPPED) <> 13 THEN

              ERROR "  DEBE INGRESAR R.F.C. COMPLETO" ATTRIBUTE(NORMAL)
              NEXT FIELD rfc
           END IF

           IF NOT valida_fecha_rfc(reg.rfc[5,10]) THEN
               ERROR "Formato de RFC Incorrecto ( ----AAMMDD*** )"
               NEXT FIELD rfc
           ELSE
               WHENEVER ERROR CONTINUE
               LET aaa     = reg.rfc[5,6]
               LET mm      = reg.rfc[7,8]
               LET dd      = reg.rfc[9,10]
               LET z_fecha = mm,"/",dd,"/19",aaa
               LET j_fecha = z_fecha

               IF j_fecha IS NULL THEN
                   ERROR "  FECHA INVALIDA EN RFC" ATTRIBUTE(NORMAL)
                   NEXT FIELD rfc
               END IF
               WHENEVER ERROR STOP

              {IF vfnaci IS NULL OR vfnaci = " " THEN
                  INITIALIZE ayo_x TO NULL
                  LET ayo_s = 0
                  LET ayo_1 = 0
                  LET ayo_2 = 0

                  LET ayo_x = HOY
                  LET ayo_1 = ayo_x[7,10]
                  LET ayo_x = vfnaci
                  LET ayo_2 = ayo_x[7,10]
                  LET ayo_s = ayo_1 - ayo_2

                  IF ayo_s < 14 THEN
                     ERROR "  FECHA INVALIDA EN RFC TIENE MENOS DE 14 AQOS "
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD rfc
                  END IF

                  LET vfnaci = z_fecha
                  DISPLAY BY NAME vfnaci
              END IF}
           END IF

           IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
              FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
              LET v_1 = 0
              INITIALIZE val_1 TO NULL
              CALL verifica_rfc(reg.rfc[1,4])
                                RETURNING v_1,val_1 #ve--
              IF v_1 = 1 THEN
                 ERROR "R.F.C. ",val_1 CLIPPED
                 NEXT FIELD rfc
              END IF

              NEXT FIELD curp
           END IF

           IF LENGTH(reg.rfc) < 13 THEN
              ERROR " "
              ERROR "  DEBIO HABER INGRESADO EL RFC CON HOMOCLAVE"
                    ATTRIBUTE(NORMAL)
           END IF

           IF reg.rfc IS NOT NULL OR reg.rfc[1,2] <> "  " THEN
              LET v_1 = 0
              INITIALIZE val_1 TO NULL
              CALL verifica_rfc(reg.rfc[1,4])
                                RETURNING v_1,val_1 #ve--
              IF v_1 = 1 THEN
                 ERROR "R.F.C. ",val_1 CLIPPED
                 NEXT FIELD rfc
              END IF


--              NEXT FIELD curp

              IF NOT valida_fecha_rfc(reg.rfc[5,10]) THEN
                 ERROR "Formato de RFC Incorrecto ( ----AAMMDD*** )"
                 ATTRIBUTE(NORMAL)
                 NEXT FIELD rfc
              ELSE
                 WHENEVER ERROR CONTINUE
                   LET aaa     = reg.rfc[5,6]
                   LET mm      = reg.rfc[7,8]
                   LET dd      = reg.rfc[9,10]
                   LET z_fecha = mm,"/",dd,"/19",aaa
                   LET j_fecha = z_fecha

                   IF j_fecha IS NULL THEN
                       ERROR "  FECHA INVALIDA EN RFC" ATTRIBUTE(NORMAL)
                       NEXT FIELD rfc
                   END IF
                 WHENEVER ERROR STOP

                 {IF vfnaci IS NULL OR vfnaci = " " THEN
                     INITIALIZE ayo_x TO NULL
                     LET ayo_s = 0
                     LET ayo_1 = 0
                     LET ayo_2 = 0

                     LET ayo_x = HOY
                     LET ayo_1 = ayo_x[7,10]
                     LET ayo_x = vfnaci
                     LET ayo_2 = ayo_x[7,10]
                     LET ayo_s = ayo_1 - ayo_2

                     IF ayo_s < 14 THEN
                         ERROR "  FECHA INVALIDA EN RFC TIENE MENOS DE 14 AC.S "
                         ATTRIBUTE(NORMAL)
                         NEXT FIELD rfc
                     END IF

                     LET vfnaci = z_fecha
                 END IF }
              END IF
           END IF

           IF reg.rfc IS NOT NULL THEN
               LET vrfc = reg.rfc[1,10]
--               CALL rfc_promotor(vrfc)
           END IF

           INITIALIZE rfc_arma TO NULL
           INITIALIZE v_rfc_arma TO NULL

           CALL arma_clave_rfc(reg.paterno,
                               reg.materno,
                               reg.nombres,
                               vfnaci) RETURNING rfc_arma #rac

           IF reg.rfc[1,4] <> rfc_arma[1,4] THEN
              ERROR "  NO COINCIDE EL NOMBRE CON EL RFC " ATTRIBUTE(NORMAL)
              SLEEP 2
              ERROR "                                   "  ATTRIBUTE(NORMAL)

              WHILE TRUE

                  LET v_rfc_arma = rfc_arma[1,4]
-- v10
                  PROMPT " RFC INCORRECTO DEBE DE SER :",
                         v_rfc_arma,
                         "      DESEA CONTINUAR  S/N ?"
                  FOR CHAR enter

                  IF enter MATCHES "[SsNn]" THEN
                     EXIT WHILE
                  END IF
              END WHILE
            ELSE
              NEXT FIELD curp
            END IF

            IF enter MATCHES "[Ss]" THEN
               NEXT FIELD curp
            ELSE
               NEXT FIELD rfc
            END IF



        AFTER FIELD curp
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD PREVIOUS
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                IF reg.rfc[5,6] <> reg.curp[5,6]  THEN
                    ERROR "El AQO DEL RFC Y CURP SON DIFERENTES "
                    ATTRIBUTE(NORMAL)
                    SLEEP 3
                    ERROR ""
                    NEXT FIELD rfc
                END IF
            END IF

            IF reg.curp[1] = " " OR reg.curp IS NULL THEN
               ERROR "Debe ingresar CURP correcta"
               NEXT FIELD curp
            END IF


            IF LENGTH(reg.curp) < 18 AND
               LENGTH(reg.curp) > 0  THEN

                ERROR "Debe ingresar CURP completa"
                ATTRIBUTE(NORMAL)
                NEXT FIELD curp
            ELSE
                IF reg.rfc[1,10] <> reg.curp[1,10] THEN
                   WHILE TRUE
                      PROMPT "LOS 10 PRIMEROS CARACTERES SON DIFETENTESAL RFC ...DESEA CONTINUAR  S/N"                                                    --(v5)

                       FOR CHAR enter                                  --(v5)

                       IF enter MATCHES "[SsNn]" THEN                  --(v5)
                          EXIT WHILE                                   --(v5)
                       END IF                                          --(v5)
                   END WHILE                                           --(v5)
                 ELSE                                                  --(v5)
                   NEXT FIELD fecha_examen                             --(v5)
                 END IF                                                --(v5)

                   IF enter MATCHES "[Ss]" THEN                        --(v5)
                      NEXT FIELD fecha_examen                          --(v5)
                   ELSE                                                --(v5)
                      NEXT FIELD curp                                  --(v5)
                   END IF                                              --(v5)


                IF reg.curp[1] <> " " OR
                   reg.curp IS NOT NULL THEN
                    IF reg.curp[11] = "H" THEN
                        LET sexo_cur = "1"
                    ELSE
                        LET sexo_cur = "2"
                    END IF

                    CALL valida_est_curp(reg.curp)
                    RETURNING pasa_curp, desc_err
                    IF pasa_curp = 1 THEN
                        ERROR "", desc_err
                        LET pasa_curp = 0
                        NEXT FIELD curp
                    END IF

                    CALL var_dig_curp(reg.curp) RETURNING pasa, dig_curp
                    IF pasa = 0 THEN
                        ERROR "Digito Verificador Invalido curp, el digito es : ",
                        dig_curp
                        LET pasa = 0
                        NEXT FIELD curp
                    END IF
                ELSE
                    LET sexo_cur = " "
                END IF
            END IF


            IF reg.rfc[1,4] <> reg.curp[1,4] THEN
                ERROR "  LOS DATOS DEL RFC Y CURP SON DIFERENTES "
                ATTRIBUTE(NORMAL)
                SLEEP 3
                ERROR ""
                NEXT FIELD rfc
            END IF

        AFTER FIELD fecha_examen
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF reg.fecha_examen IS NULL OR
               reg.fecha_examen= '          ' THEN
                ERROR "La Fecha no puede ser Nula o Blancos "
                SLEEP 2
                NEXT FIELD fecha_examen
            END IF

            IF reg.fecha_examen < TODAY THEN
                ERROR "La Fecha no puede ser Menor al dia de HOY"
                SLEEP 2
                NEXT FIELD fecha_examen
            END IF

            -- Valida que el RFC no exista dupliccado con la misma --(v2)
            -- Fecha de examen                                     --(v2)

            INITIALIZE  v_rfc TO NULL                              --(v2)

            SELECT rfc                                             --(v2)
              INTO v_rfc                                           --(v2)
              FROM pro_ctr_examen                                  --(v2)
             WHERE fecha_examen     =  reg.fecha_examen            --(v2)
               AND rfc              =  reg.rfc                     --(v2)
               AND estado_registro  =  0                           --(v2)
               AND cod_promotor     =  reg.cod_promotor            --(v5)


            IF v_rfc IS NOT NULL THEN                              --(v2)
               ERROR  "El rfc ya existe con misma fecha de examen" --(v2)
                SLEEP 2                                            --(v2)
                NEXT FIELD fecha_examen                            --(v2)
            END IF                                                 --(v2)


        AFTER FIELD hora_examen
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD PREVIOUS
            END IF


            IF reg.hora_examen[1,2] > 24
            OR reg.hora_examen[1,2] < 00
            OR reg.hora_examen IS NULL THEN
               ERROR "Hora es menor a 00 o mayor 24 "
                SLEEP 2
                NEXT FIELD hora_examen
            END IF

            IF reg.hora_examen[1,2] = 24 THEN
               IF reg.hora_examen[3,4] <> 00 THEN
                  ERROR "Minutos de la Hora de examen deben ser 00 pora las 24 "
                  SLEEP 2
                  NEXT FIELD hora_examen
                END IF
            ELSE
               IF reg.hora_examen[3,4] > 59
               OR reg.hora_examen[3,4] < 00 THEN
                  ERROR "Minutos de la Hora de examen debe estar en el rango de 00 a 59"
                  SLEEP 2
                  NEXT FIELD hora_examen
                END IF
            END IF


        AFTER FIELD tpo_examen
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD PREVIOUS
            END IF

           IF reg.tpo_examen IS NULL THEN
               CALL llama_tpo_examen() RETURNING reg.tpo_examen
               IF reg.tpo_examen IS NULL THEN
                   NEXT FIELD tpo_examen
           ELSE

            -- Valida que examen le corresponda al promotor por su status

                LET v_status = null

                SELECT pro_mae_promotor.status
                  INTO v_status
                  FROM pro_mae_promotor
                 WHERE pro_mae_promotor.unico = reg.curp
                   AND pro_mae_promotor.cod_promotor = reg.cod_promotor --(v4)

                IF v_status = 1  THEN   -- Promotor Alta
                    IF reg.tpo_examen = 1 THEN  -- Postulacion
                      ERROR "Tipo de Examen no se permite a Promotor Activo 1"
                      SLEEP 2
                      NEXT FIELD tpo_examen
                    END IF
                END IF

                IF  cve_afore = "516" THEN        --- validacion solo para XXI
                   IF  reg.cod_promotor = "0000000000" THEN
                       IF reg.tpo_examen <> 1 THEN
                          ERROR "Tipo de examen no valido para el Promotor 2"
                          SLEEP 2
                          NEXT FIELD tpo_examen
                       END IF
                   ELSE
                       IF v_status = 1 and reg.tpo_examen = 1  THEN
                          ERROR "Tipo de examen no valido para el Promotor 3"
                          SLEEP 2
                          NEXT FIELD tpo_examen
                       END IF
                   END IF
                END IF


                SELECT desc_tipo
                INTO   reg.desc_tpo_examen
                FROM   pro_tipo_examen
                WHERE  cod_tipo = reg.tpo_examen

                DISPLAY reg.tpo_examen TO tpo_examen                                         --(v3)
                DISPLAY reg.desc_tpo_examen TO desc_tpo_examen
                NEXT FIELD evento
            END IF
        ELSE

               -- Valida que examen le corresponda al promotor por su status

                   LET v_status = null

                   SELECT pro_mae_promotor.status
                     INTO v_status
                     FROM pro_mae_promotor
                    WHERE pro_mae_promotor.unico = reg.curp
                      AND pro_mae_promotor.cod_promotor = reg.cod_promotor   --(v4)

                   IF v_status = 1  THEN   -- Promotor Alta
                      IF reg.tpo_examen = 1 THEN  -- Postulacion
                         ERROR "Tipo de Examen no se permite a Promotor Activo 1"
                         SLEEP 2
                         NEXT FIELD tpo_examen
                      END IF
                   END IF

                IF  cve_afore = "516" THEN        --- validacion solo para XXI
                   IF  reg.cod_promotor = "0000000000" THEN
                       IF reg.tpo_examen <> 1 THEN
                          ERROR "Tipo de examen no valido para el Promotor 2"
                          SLEEP 2
                          NEXT FIELD tpo_examen
                       END IF
                   ELSE
                       IF v_status = 1 and reg.tpo_examen = 1  THEN
                          ERROR "Tipo de examen no valido para el Promotor 3"
                          SLEEP 2
                          NEXT FIELD tpo_examen
                       END IF
                   END IF
                END IF


               SELECT desc_tipo
               INTO   reg.desc_tpo_examen
               FROM   pro_tipo_examen
               WHERE  cod_tipo = reg.tpo_examen

               IF SQLCA.SQLCODE <> 0 THEN
                   ERROR "   Tipo Examen no se encuentra en el Catalogo"
                   INITIALIZE reg.tpo_examen TO NULL
                   DISPLAY reg.tpo_examen TO tpo_examen
                   NEXT FIELD tpo_examen
               ELSE
                   DISPLAY reg.desc_tpo_examen TO desc_tpo_examen
                   NEXT FIELD evento
               END IF
           END IF

        AFTER FIELD evento
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD PREVIOUS
            END IF

           IF reg.evento IS NULL THEN
              CALL llama_evento() RETURNING reg.evento,
                                           reg.desc_eve_802

              IF reg.evento IS NULL THEN
                 NEXT FIELD tpo_examen
              ELSE
                 DISPLAY reg.evento TO evento
                 DISPLAY reg.desc_eve_802 TO desc_eve_802
              END IF
           ELSE
               SELECT desc_evento
               INTO   reg.desc_eve_802
               FROM pro_evento
               WHERE  cod_evento = reg.evento
               GROUP BY 1

               IF SQLCA.SQLCODE <> 0 THEN
                  ERROR "   El Evento no se encuentra en el Catalogo"
                  INITIALIZE reg.evento TO NULL
                  DISPLAY reg.evento TO evento
                  NEXT FIELD evento
               ELSE
                  DISPLAY reg.desc_eve_802 TO desc_eve_802
                  NEXT FIELD sede
               END IF
           END IF

        AFTER FIELD sede
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD PREVIOUS
            END IF

           IF reg.sede IS NULL THEN
              CALL llama_sede() RETURNING reg.sede, reg.desc_sede, reg.edo_sede

              IF reg.sede IS NULL OR reg.sede = 0 THEN
                 NEXT FIELD sede
              ELSE
                   SELECT estad_desc
                   INTO   reg.desc_edo_sede
                   FROM   tab_estado
                   WHERE  estad_cod = reg.edo_sede

                   DISPLAY reg.sede TO sede
                   DISPLAY reg.desc_sede TO desc_sede
                   DISPLAY reg.edo_sede TO edo_sede
                   DISPLAY reg.desc_edo_sede TO desc_edo_sede
                   NEXT FIELD num_vuelta
              END IF
           ELSE
               SELECT m.desc_sede, m.edo_sede
               INTO   reg.desc_sede, reg.edo_sede
               FROM pro_sede_examen m
               WHERE  m.cod_sede = reg.sede
               GROUP BY 1,2
               IF SQLCA.SQLCODE <> 0 THEN
                  ERROR "   No se encuentra el Codigo de la Sede en el Catalogo"
                  NEXT FIELD sede
               ELSE
                   SELECT estad_desc
                   INTO   reg.desc_edo_sede
                   FROM   tab_estado
                   WHERE  estad_cod = reg.edo_sede

                   DISPLAY reg.desc_sede TO desc_sede
                   DISPLAY reg.edo_sede TO edo_sede
                   DISPLAY reg.desc_edo_sede TO desc_edo_sede
                   DISPLAY reg.desc_edo_sede TO desc_edo_sede
                   NEXT FIELD num_vuelta
               END IF
           END IF

        AFTER FIELD num_vuelta
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF reg.num_vuelta IS NULL OR
               reg.num_vuelta = '          ' THEN
                ERROR "Numero de vuelta no puede ser Nulo "
                SLEEP 2
                NEXT FIELD num_vuelta
            END IF
            IF reg.num_vuelta > 2 or reg.num_vuelta < 0 THEN
               ERROR "Numero de vuelta debe ser o,1 o 2 "
               SLEEP 2
               NEXT FIELD num_vuelta
            END IF


--        ON KEY (ESC)

        PROMPT " ESTA SEGURO DE AGREGAR EL REGISTRO S/N " FOR CHAR enter
        IF enter MATCHES "[SsNn]" THEN
            IF enter MATCHES "[Ss]" THEN


                INSERT INTO pro_ctr_examen
                VALUES (0                     ,#folio_envio
                        0                     ,#folio_resexamen
                        0                     ,#folio_recep
                        reg.cod_promotor      ,
                        reg.paterno           ,
                        reg.materno           ,
                        reg.nombres           ,
                        reg.rfc               ,
                        reg.curp              ,
                        reg.fecha_examen     ,
                        ''                    ,#fecha_exa_803
                        reg.hora_examen      ,
                        ''                    ,#hora_exa_803
                        reg.tpo_examen        ,
                        reg.evento        ,
                        ''                    ,#evento_803
                        reg.sede              ,
                        reg.num_vuelta        ,
                        ''                    ,#cve_solicitud
                        ''                    ,#calificacion
                        ''                    ,#resultado
                        vconsecutivo          ,
                        0                     ,#estado_registro
                        ''                    ,#fecha_envio
                        ''                    ,#estado_reslexa
                        ''                    ,#fecha_recep
                        HOY                   ,#fecha_captura
                        usuario               ,#usuario_captura
                        ''                    ,#fecha_modifica
                        ''                     #usuario_modifica
                       )


                LET gerrflag = FALSE

                IF STATUS < 0 THEN
                    LET wf_error = STATUS
                    LET gerrflag = TRUE
                END IF

                IF NOT gerrflag THEN  #3
                    ERROR   "  REGISTRO AGREGADO" ATTRIBUTE(REVERSE) SLEEP 3
                    ERROR   ""
                    INITIALIZE reg.* TO NULL
                    CLEAR FORM
                    LET sw_1 = 0
                    NEXT FIELD cod_promotor
                ELSE
                    ERROR "  Error=",wf_error,", fallo la insercion ..."
                    ATTRIBUTE(NORMAL)
                    SLEEP 3
                    EXIT INPUT
                END IF  #3

            ELSE
                PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                   INITIALIZE reg.* TO NULL
                    CLEAR FORM
                    LET sw_1 = 0
                    NEXT FIELD cod_promotor
            END IF
        END IF

        ON KEY(CONTROL-C, INTERRUPT)
            DISPLAY " PROM006                 SOLICITUD  DE  EXAMEN ",
                    "                               " AT 3,1 ATTRIBUTE(REVERSE)
            DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
            DISPLAY "                                               ",
                    "                               " AT 4,1 ATTRIBUTE(NORMAL)
            DISPLAY "                                               ",
                    "                               " AT 11,1 ATTRIBUTE(NORMAL)

            CALL inicializa()
            CLEAR FORM


            LET int_flag = TRUE
            EXIT INPUT
    END INPUT
END FUNCTION


FUNCTION rfc_promotor(f_vrfc)
#----------------------------

DEFINE f_vrfc CHAR(010)
DEFINE pos  smallint
DEFINE arr_rfc ARRAY[100] OF RECORD #glo #arr_rfc
            cod_promotor   LIKE    pro_ctr_examen.cod_promotor
       END RECORD

       DECLARE cur_rfc CURSOR FOR
               SELECT  unique  cod_promotor
               FROM    pro_ctr_examen
               WHERE   rfc[1,10] = f_vrfc

               LET pos = 1
       FOREACH cur_rfc INTO arr_rfc[pos].*
               LET pos = pos + 1
       END FOREACH

       IF (pos-1) >= 1 THEN
          CALL SET_COUNT(pos-1)
          LET ventro = 1
          OPEN WINDOW Prom0063 AT 9,19 WITH FORM "PROM0063" ATTRIBUTE( BORDER)
          DISPLAY "             <Ctrl-c> Salir                     "
                  AT 1,1 ATTRIBUTE(REVERSE)

          DISPLAY ARRAY arr_rfc TO scr_1.*
                    ON KEY (CONTROL-C,INTERRUPT)
                       EXIT DISPLAY
          END DISPLAY
          CLOSE WINDOW Prom0063
       END IF

END FUNCTION


FUNCTION llama_tpo_examen()
#--------------------------
   DEFINE aux_val      SMALLINT,

          l_reg ARRAY[1000] OF RECORD
              codigo      CHAR(1),
              descripcion CHAR(40)
          END RECORD,

          x_x        CHAR(1000),
          x_buscar   CHAR(1200),
          pos        SMALLINT

   OPEN WINDOW vent_texamen AT 05,12 WITH FORM "PROM0062" ATTRIBUTE(BORDER)
   DISPLAY "                       TIPO DE EXAMEN                     "
                   AT 2,1 ATTRIBUTE(REVERSE)
--   INPUT BY NAME x_buscar                                    --(v3)
--         AFTER FIELD x_buscar                                --(v3)
--           IF x_buscar IS NULL THEN                          --(v3)
--              ERROR "Descripcion a Buscar NO puede ser nulo" --(v3)
--              NEXT FIELD x_buscar                            --(v3)
--           ELSE                                              --(v3)
--              EXIT INPUT                                     --(v3)
--           END IF                                            --(v3)
--   END INPUT                                                 --(v3)

   WHILE TRUE
         LET x_x = " SELECT cod_tipo,desc_tipo FROM pro_tipo_examen ",
--                   " WHERE cod_tipo MATCHES ",'"',x_buscar CLIPPED'"',    --(v3)
                   " ORDER BY 1 " CLIPPED
         PREPARE cur_texamen FROM x_x
         DECLARE cur_g_texamen CURSOR FOR cur_texamen
         LET pos = 1

         FOREACH cur_g_texamen INTO l_reg[pos].*
            LET pos = pos + 1
            IF pos >= 100 THEN
                ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
                EXIT FOREACH
            END IF
         END FOREACH

         IF (pos-1) < 1 THEN
            ERROR "SE ENCUENTRA VACIO EL CATALOGO"
         END IF

         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_reg TO scr_1.*
                 ON KEY ( INTERRUPT, CONTROL-C )
                    LET pos = 0
                    EXIT DISPLAY
                 ON KEY ( CONTROL-M )
                    LET pos = ARR_CURR()
                    EXIT DISPLAY
         END DISPLAY

         IF pos <> 0 THEN
             EXIT WHILE
         END IF
   END WHILE
   CLOSE WINDOW vent_texamen
   RETURN l_reg[pos].codigo
END FUNCTION

FUNCTION llama_evento()
#----------------------
   DEFINE aux_val      SMALLINT,

          l_reg ARRAY[1000] OF RECORD
              codigo      CHAR(2),
              descripcion CHAR(40)
          END RECORD,

          x_x        CHAR(100),
          x_buscar   CHAR(30),
          pos        SMALLINT

   OPEN WINDOW vent_evento AT 05,12 WITH FORM "PROM0063" ATTRIBUTE(BORDER)
   DISPLAY "                       TIPO DE EVENTO                     "
                   AT 2,1 ATTRIBUTE(REVERSE)
--   INPUT BY NAME x_buscar                                     --(v3)
--         AFTER FIELD x_buscar                                 --(v3)
--           IF x_buscar IS NULL THEN                           --(v3)
--              ERROR "Descripcion a Buscar NO puede ser nulo"  --(v3)
--              NEXT FIELD x_buscar                             --(v3)
--           ELSE                                               --(v3)
--              EXIT INPUT                                      --(v3)
--           END IF                                             --(v3)
--   END INPUT                                                  --(v3)

   WHILE TRUE
         LET x_x = " SELECT cod_evento,desc_evento FROM pro_evento ",
--                   " WHERE cod_evento MATCHES ",'"',x_buscar CLIPPED,'"',   --(v3)
                   " ORDER BY 1 " CLIPPED
         PREPARE cur_tevento FROM x_x
         DECLARE cur_g_tevento CURSOR FOR cur_tevento
         LET pos = 1
         FOREACH cur_g_tevento INTO l_reg[pos].*
            LET pos = pos + 1
            IF pos >= 100 THEN
                ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
                EXIT FOREACH
            END IF
         END FOREACH

         IF (pos-1) < 1 THEN
            ERROR "SE ENCUENTRA VACIO EL CATALOGO"
         END IF

         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_reg TO scr_1.*
                 ON KEY ( INTERRUPT, CONTROL-C )
                    LET pos = 0
                    EXIT DISPLAY
                 ON KEY ( CONTROL-M )
                    LET pos = ARR_CURR()
                    EXIT DISPLAY
         END DISPLAY

         IF pos <> 0 THEN
             EXIT WHILE
         END IF
   END WHILE
   CLOSE WINDOW vent_evento
   RETURN l_reg[pos].codigo, l_reg[pos].descripcion
END FUNCTION

FUNCTION llama_sede()
#--------------------
   DEFINE aux_val      SMALLINT,

          l_reg ARRAY[1000] OF RECORD
              cod_sede SMALLINT,
              desc_sede   CHAR(150),
              edo_sede    SMALLINT,
              desc_edo_s  CHAR(10)
          END RECORD,

          x_x        CHAR(1000),
          x_buscar   CHAR(30),
          m_m        SMALLINT,
          pos        SMALLINT

   LET m_m = 0
   OPEN WINDOW vent_evento AT 05,04 WITH FORM "PROM0064" ATTRIBUTE(BORDER)
   DISPLAY "                      SEDE PARA EXAMEN                    "
                   AT 2,1 ATTRIBUTE(REVERSE)

   WHILE TRUE
         LET x_x = " SELECT a.cod_sede, a.desc_sede[1,30],",
                   "  a.edo_sede, b.estad_desc[1,10] ",
                   " FROM pro_sede_examen a, tab_estado b ",
                   " WHERE   a.edo_sede = b.estad_cod ",
                   " ORDER BY 1 " CLIPPED
         PREPARE cur_tsede FROM x_x
         DECLARE cur_g_tsede CURSOR FOR cur_tsede
         LET pos = 1
         FOREACH cur_g_tsede INTO l_reg[pos].*
            LET pos = pos + 1
            IF pos >= 100 THEN
                ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
                EXIT FOREACH
            END IF
         END FOREACH

         IF (pos-1) < 1 THEN
            ERROR "SE ENCUENTRA VACIO EL CATALOGO"
         END IF

         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_reg TO scr_1.*
                 ON KEY ( INTERRUPT, CONTROL-C )
                    LET pos = 0
                    EXIT DISPLAY
                 ON KEY ( CONTROL-M )
                    LET pos = ARR_CURR()
                    EXIT DISPLAY
         END DISPLAY

         IF pos <> 0 THEN
             EXIT WHILE
         END IF
   END WHILE
   CLOSE WINDOW vent_evento
   RETURN l_reg[pos].cod_sede,
          l_reg[pos].desc_sede,
          l_reg[pos].edo_sede
END FUNCTION


FUNCTION Pregunta1()
#-------------------
    DEFINE responde CHAR(1)

    WHILE TRUE
             PROMPT "DESEA GRABAR EL REGISTRO S/N ? " FOR responde

             IF responde MATCHES "[SsNn]" THEN   #1
                EXIT WHILE
             END IF
    END WHILE
    RETURN responde
END FUNCTION


FUNCTION Consulta()
#------------------

    DEFINE #loc #integer
        pos                   INTEGER

    OPEN WINDOW prom0065 AT 2,2 WITH FORM "PROM0065" ATTRIBUTE (BORDER)
    DISPLAY " Ctrl-C: Salir                                           "  AT 1,1 ATTRIBUTE(NORMAL)
    DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE)
    DISPLAY " PROM006                 SOLICITUD  DE  EXAMEN ",
            "                               " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 2,65 ATTRIBUTE(REVERSE)
    DISPLAY "                         DATOS  DEL  PROMOTOR  ",
            "                               " AT 4,1 ATTRIBUTE(REVERSE)
    DISPLAY "                         DATOS  DEL  EXAMEN    ",
            "                               " AT 11,1 ATTRIBUTE(REVERSE)

    CLEAR FORM

    LET int_flag = FALSE

    CONSTRUCT BY NAME x_busca ON A.cod_promotor,
                                 A.cve_solicitud,
                                 A.paterno,
                                 A.materno,
                                 A.nombres,
                                 A.rfc,
                                 A.curp,
                                 A.fecha_examen,
                                 A.hora_examen,
                                 A.tpo_examen,
                                 A.evento,
                                 A.sede,
                                 A.result,
                                 A.consecutivo,
                                 A.estado_registro

        ON KEY (CONTROL-C)
                LET int_flag=TRUE
                EXIT CONSTRUCT

        ON KEY (INTERRUPT)
                LET int_flag=TRUE
                EXIT CONSTRUCT

        ON KEY (Esc)
            LET int_flag = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        CLOSE WINDOW prom0065
        RETURN
    END IF

    LET txt_1 = " SELECT A.cod_promotor    ,",
                "        A.cve_solicitud   ,",
                "        A.paterno         ,",
                "        A.materno         ,",
                "        A.nombres         ,",
                "        A.rfc             ,",
                "        A.curp            ,",
                "        A.fecha_examen    ,",
                "        A.hora_examen     ,",
                "        A.tpo_examen      ,",
                "        ''                ,",#desc_tpo_examen
                "        A.evento          ,",
                "        ''                ,",#desc_evento
                "        A.sede            ,",
                "        ''                ,",#desc_sede
                "        B.edo_sede        ,",
                "        ''                ,",#desc_edo_sede
                "        A.num_vuelta      ,",
                "        A.evento_803      ,",
                "        ''                ,",#desc_evento_803
                "        A.fecha_exa_803    ,",
                "        A.hora_exa_803     ,",
                "        A.calif           ,",
                "        A.result          ,",
                "        ''                ,",#desc_result
                "        A.consecutivo     ,",
                "        A.estado_registro ,",
                "        ''                 ",
                " FROM   pro_ctr_examen A, pro_sede_examen B ",
                " WHERE  A.sede = B.cod_sede ",
                " AND    ",x_busca CLIPPED

    PREPARE pre_1 FROM txt_1
    DECLARE cur_1 CURSOR FOR pre_1

    LET pos = 1
    ERROR "PROCESANDO INFORMACION"

    FOREACH cur_1 INTO arr_1[pos].*
        IF pos = 2000 THEN
            ERROR "ARREGLO LLENO"
            EXIT FOREACH
        END IF

        SELECT desc_tipo
        INTO   arr_1[pos].desc_tpo_examen
        FROM   pro_tipo_examen
        WHERE  cod_tipo = arr_1[pos].tpo_examen

        SELECT desc_evento
        INTO   arr_1[pos].desc_eve_802
        FROM   pro_evento
        WHERE  cod_evento = arr_1[pos].evento

        SELECT desc_evento
        INTO   arr_1[pos].desc_eve_803
        FROM   pro_evento_803
        WHERE  cod_evento = arr_1[pos].evento_803

        SELECT desc_sede
        INTO   arr_1[pos].desc_sede
        FROM   pro_sede_examen
        WHERE  cod_sede = arr_1[pos].sede

        SELECT estad_desc
        INTO   arr_1[pos].desc_edo_sede
        FROM   tab_estado
        WHERE  estad_cod = arr_1[pos].edo_sede

        SELECT A.desc_status_corta
        INTO   arr_1[pos].desc_edo_reg
        FROM   pro_status_interno A
        WHERE  A.status_interno = arr_1[pos].estado_registro

        -- Descripcion del resultado de examen
        LET arr_1[pos].desc_result = NULL
        SELECT A.desc_result
        INTO   arr_1[pos].desc_result
        FROM   pro_result_examen A
        WHERE  A.cod_result = arr_1[pos].cod_result

        LET pos = pos + 1
    END FOREACH

    ERROR ""
    IF pos = 1 THEN
        CLEAR FORM
        ERROR" NO HAY REGISTROS"
        CLOSE WINDOW prom0065
        RETURN
    END IF

    LET total_reg = pos -1
    DISPLAY "TOTAL REGISTROS: ",total_reg AT 22,02 ATTRIBUTE(REVERSE)
--isssa
    CALL SET_COUNT(pos-1)

    DISPLAY ARRAY arr_1 TO scr_1.*
        ON KEY ( CONTROL-C ,INTERRUPT )
            CALL inicializa()
            EXIT DISPLAY

        ON KEY ( INTERRUPT )
            CALL inicializa()
            EXIT DISPLAY

    END DISPLAY

    CLOSE WINDOW prom0065

END FUNCTION


FUNCTION Modifica()
#------------------
    DEFINE #loc #integer
        pos                   INTEGER

    OPEN WINDOW prom0065 AT 2,2 WITH FORM "PROM0065" ATTRIBUTE (BORDER)
    DISPLAY " ESC: Modifica           Ctrl-C: Salir                   "  AT 1,1 ATTRIBUTE(NORMAL)
    DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE)
    DISPLAY " PROM006                 SOLICITUD  DE  EXAMEN ",
            "                               " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 2,65 ATTRIBUTE(REVERSE)
    DISPLAY "                         DATOS  DEL  PROMOTOR  ",
            "                               " AT 4,1 ATTRIBUTE(REVERSE)
    DISPLAY "                         DATOS  DEL  EXAMEN    ",
            "                               " AT 11,1 ATTRIBUTE(REVERSE)

    LET int_flag = FALSE

    CONSTRUCT BY NAME x_busca ON A.cod_promotor,
                                 A.cve_solicitud,
                                 A.paterno,
                                 A.materno,
                                 A.nombres,
                                 A.rfc,
                                 A.curp,
                                 A.fecha_examen,
                                 A.hora_examen,
                                 A.tpo_examen,
                                 A.evento,
                                 A.sede,
                                 A.result,
                                 A.consecutivo,
                                 A.estado_registro

        ON KEY (CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT

        ON KEY (INTERRUPT)
            LET int_flag = TRUE
            EXIT CONSTRUCT

        ON KEY (Esc)
            LET int_flag = FALSE
            LET int_flag = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        CLOSE WINDOW prom0065
        RETURN
    END IF

    LET txt_1 = " SELECT A.cod_promotor    ,",
                "        A.cve_solicitud   ,",
                "        A.paterno         ,",
                "        A.materno         ,",
                "        A.nombres         ,",
                "        A.rfc             ,",
                "        A.curp            ,",
                "        A.fecha_examen    ,",
                "        A.hora_examen     ,",
                "        A.tpo_examen      ,",
                "        ''                ,",#desc_tpo_examen
                "        A.evento          ,",
                "        ''                ,",#desc_evento
                "        A.sede            ,",
                "        ''                ,",#desc_sede
                "        B.edo_sede        ,",
                "        ''                ,",#desc_edo_sede
                "        A.num_vuelta      ,",
                "        A.evento_803      ,",
                "        ''                ,",#desc_evento_803
                "        A.fecha_exa_803    ,",
                "        A.hora_exa_803     ,",
                "        A.calif           ,",
                "        A.result          ,",
                "        ''                ,",#desc_result
                "        A.consecutivo     ,",
                "        A.estado_registro ,",
                "        ''                 ",
                " FROM   pro_ctr_examen A, pro_sede_examen B ",
                " WHERE  A.sede = B.cod_sede ",
                " AND    A.estado_registro =  ", 0,
                " AND    ",x_busca CLIPPED


    PREPARE pre_2 FROM txt_1
    DECLARE cur_2 CURSOR FOR pre_2

    LET pos = 1
    ERROR "PROCESANDO INFORMACION"
    FOREACH cur_2 INTO arr_1[pos].*

        IF pos = 2000 THEN
            ERROR "ARREGLO LLENO"
            EXIT FOREACH
        END IF

        SELECT A.desc_status_corta
        INTO   arr_1[pos].desc_edo_reg
        FROM   pro_status_interno A
        WHERE  A.status_interno = arr_1[pos].estado_registro

        IF arr_1[pos].estado_registro > 0 THEN

            PROMPT "SOLICITUD DEL RFC <",arr_1[pos].rfc,
                   "> SE ENCUENTRA EN EL ESTADO : ",
                   arr_1[pos].desc_edo_reg CLIPPED,
                   " " ATTRIBUTE(REVERSE)
            FOR opc ATTRIBUTE(REVERSE)
            CONTINUE FOREACH
        END IF

        SELECT desc_tipo
        INTO   arr_1[pos].desc_tpo_examen
        FROM   pro_tipo_examen
        WHERE  cod_tipo = arr_1[pos].tpo_examen

        SELECT desc_evento
        INTO   arr_1[pos].desc_eve_802
        FROM   pro_evento
        WHERE  cod_evento = arr_1[pos].evento

        SELECT desc_evento
        INTO   arr_1[pos].desc_eve_803
        FROM   pro_evento
        WHERE  cod_evento = arr_1[pos].evento_803


        SELECT desc_sede
        INTO   arr_1[pos].desc_sede
        FROM   pro_sede_examen
        WHERE  cod_sede = arr_1[pos].sede

        SELECT estad_desc
        INTO   arr_1[pos].desc_edo_sede
        FROM   tab_estado
        WHERE  estad_cod = arr_1[pos].edo_sede

        -- Descripcion del resultado de examen

        SELECT A.desc_result
        INTO   arr_1[pos].desc_result
        FROM   pro_result_examen A
        WHERE  A.cod_result = arr_1[pos].cod_result

        LET pos = pos + 1
    END FOREACH

    ERROR ""
    IF (pos-1) >= 1 THEN
       CALL SET_COUNT(pos-1)

       DISPLAY ARRAY arr_1 TO scr_1.*
          ON KEY ( CONTROL-C )
             LET int_flag=TRUE
             EXIT DISPLAY

          ON KEY ( INTERRUPT )
             LET int_flag=TRUE
             EXIT DISPLAY

          ON KEY ( CONTROL-M )
             LET pos = ARR_CURR()
             EXIT DISPLAY
       END DISPLAY
    ELSE
        ERROR "NO EXISTE REGISTRO A MODIFICAR" ATTRIBUTE(NORMAL)
        CLOSE WINDOW prom0065
       RETURN
    END IF

    IF int_flag = TRUE THEN
        LET int_flag=FALSE
        CLOSE WINDOW prom0065
        RETURN
    END IF

    CALL construccion(arr_1[pos].*)

    CLOSE WINDOW prom0065
END FUNCTION


FUNCTION construccion(reg_mod)
#-----------------------------

    DEFINE reg_mod RECORD
        cod_promotor         LIKE pro_ctr_examen.cod_promotor,
        cve_solicitud        LIKE pro_ctr_examen.cve_solicitud,
        paterno              LIKE pro_ctr_examen.paterno,
        materno              LIKE pro_ctr_examen.materno,
        nombres              LIKE pro_ctr_examen.nombres,
        rfc                  LIKE pro_ctr_examen.rfc,
        curp                 LIKE pro_ctr_examen.curp,
        fecha_examen        LIKE pro_ctr_examen.fecha_examen,
        hora_examen         LIKE pro_ctr_examen.hora_examen,
        tpo_examen           LIKE pro_ctr_examen.tpo_examen,
        desc_tpo_examen      LIKE pro_tipo_examen.desc_tipo,
        evento           LIKE pro_ctr_examen.evento,
        desc_eve_802         LIKE pro_evento.desc_evento,
        sede                 LIKE pro_ctr_examen.sede,
        desc_sede            LIKE pro_sede_examen.desc_sede,
        edo_sede             LIKE pro_sede_examen.edo_sede,
        desc_edo_sede        LIKE tab_estado.estad_desc,
        num_vuelta           LIKE pro_ctr_examen.num_vuelta,
        evento_803           LIKE pro_ctr_examen.evento_803,
        desc_eve_803         LIKE pro_evento_803.desc_evento,
        fecha_exa_803        LIKE pro_ctr_examen.fecha_exa_803,
        hora_exa_803         LIKE pro_ctr_examen.hora_exa_803,
        calif                LIKE pro_ctr_examen.calif,
        cod_result           LIKE pro_result_examen.cod_result,
        desc_result          LIKE pro_result_examen.desc_result,
        consecutivo          LIKE pro_ctr_examen.consecutivo,
        estado_registro      LIKE pro_ctr_examen.estado_registro,
        desc_edo_reg         LIKE pro_status_interno.desc_status_corta
    END RECORD

    DEFINE #loc #smallint
        i                     SMALLINT

    DEFINE #loc #char
        opc                   CHAR(01)


    INPUT BY NAME reg_mod.cve_solicitud   ,
                  reg_mod.paterno         ,
                  reg_mod.materno         ,
                  reg_mod.nombres         ,
                  reg_mod.curp            ,
                  reg_mod.fecha_examen   ,
                  reg_mod.hora_examen     ,
                  reg_mod.tpo_examen      ,
                  reg_mod.evento          ,
                  reg_mod.sede            ,
                  reg_mod.num_vuelta  WITHOUT DEFAULTS


        BEFORE FIELD paterno
               NEXT FIELD fecha_examen


        AFTER FIELD paterno
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF reg_mod.paterno  IS NULL OR reg_mod.paterno[1] = " " THEN
               ERROR "  EL APELLIDO PATERNO DEBE SER INGRESADO"
               ATTRIBUTE(NORMAL)
               NEXT FIELD paterno
            END IF

            IF reg_mod.paterno[1] = "," OR
               reg_mod.paterno[1] = ";" OR
               reg_mod.paterno[1] = "!" OR
               reg_mod.paterno[1] = '"' OR
               reg_mod.paterno[1] = "#" OR
               reg_mod.paterno[1] = "$" OR
               reg_mod.paterno[1] = "%" OR
               reg_mod.paterno[1] = "&" OR
               reg_mod.paterno[1] = "/" OR
               reg_mod.paterno[1] = "(" OR
               reg_mod.paterno[1] = ")" OR
               reg_mod.paterno[1] = "=" OR
               reg_mod.paterno[1] = "?" OR
               reg_mod.paterno[1] = "!" OR
               reg_mod.paterno[1] = "'" OR
               reg_mod.paterno[1] = "@" OR
               reg_mod.paterno[1] = "'" OR
               reg_mod.paterno[1] = ":" OR
               reg_mod.paterno[1] = "^" OR
               reg_mod.paterno[1] = "[" OR
               reg_mod.paterno[1] = "]" OR
               reg_mod.paterno[1] = "{" OR
               reg_mod.paterno[1] = "}" OR
               reg_mod.paterno[1] = "`" THEN
               ERROR "  EL APELLIDO PATERNO NO DEBE TENER CARACTERES ESPECIALES"
                     ATTRIBUTE(NORMAL)
               NEXT FIELD paterno
            END IF

        AFTER FIELD materno
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD PREVIOUS
            END IF

            IF reg_mod.materno[1,2] = " ." OR
               reg_mod.materno[1] = "."  OR reg_mod.materno[1,2] = ".." OR
               reg_mod.materno[1] = "X"  OR reg_mod.materno[1,2] = " X" OR
               reg_mod.materno[1,2] = ".X"  THEN

                ERROR "  EL APELLIDO MATERNO NO PUEDE SER UN PUNTO o X "
                ATTRIBUTE(NORMAL)
                NEXT FIELD materno
            END IF

        AFTER FIELD nombres
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD PREVIOUS
            END IF

            IF reg_mod.nombres  IS NULL OR reg_mod.nombres[1] = " " THEN
                ERROR "  EL NOMBRE ES REQUERIDO" ATTRIBUTE(NORMAL)
                NEXT FIELD nombres
            END IF

        AFTER FIELD curp
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD PREVIOUS
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                IF reg_mod.rfc[5,6] <> reg_mod.curp[5,6]  THEN
                    ERROR "El AQO DEL RFC Y CURP SON DIFERENTES "
                    ATTRIBUTE(NORMAL)
                    SLEEP 3
                    ERROR ""
                    NEXT FIELD rfc
                END IF
            END IF

            IF LENGTH(reg_mod.curp) < 18 AND
               LENGTH(reg_mod.curp) > 0  THEN

                ERROR "Debe ingresar CURP completa"
                ATTRIBUTE(NORMAL)
                NEXT FIELD curp
            ELSE
                IF reg_mod.rfc[1,10] <> reg_mod.curp[1,10] THEN        --(v5)
                   WHILE TRUE
                      PROMPT "LOS 10 PRIMEROS CARACTERES SON DIFETENTESAL RFC ...DESEA CONTINUAR  S/N"
                            --(v5)
                       FOR CHAR enter                                  --(v5)

                       IF enter MATCHES "[SsNn]" THEN                  --(v5)
                          EXIT WHILE                                   --(v5)
                       END IF                                          --(v5)
                   END WHILE                                           --(v5)
                 ELSE                                                  --(v5)
                   NEXT FIELD fecha_examen                             --(v5)
                 END IF                                                --(v5)

                   IF enter MATCHES "[Ss]" THEN                        --(v5)
                      NEXT FIELD fecha_examen                         --(v5)
                   ELSE                                                --(v5)
                      NEXT FIELD curp                                  --(v5)
                   END IF                                              --(v5)


                IF reg_mod.curp[1] <> " " OR
                   reg_mod.curp IS NOT NULL THEN
                    IF reg_mod.curp[11] = "H" THEN
                        LET sexo_cur = "1"
                    ELSE
                        LET sexo_cur = "2"
                    END IF

                    CALL valida_est_curp(reg_mod.curp)
                    RETURNING pasa_curp, desc_err
                    IF pasa_curp = 1 THEN
                        ERROR "", desc_err
                        LET pasa_curp = 0
                        NEXT FIELD curp
                    END IF

                    CALL var_dig_curp(reg_mod.curp) RETURNING pasa, dig_curp
                    IF pasa = 0 THEN
                        ERROR "Digito Verificador Invalido curp, el digito es : ",
                        dig_curp
                        LET pasa = 0
                        NEXT FIELD curp
                    END IF
                ELSE
                    LET sexo_cur = " "
                END IF
            END IF

            IF reg_mod.curp[1] = " " OR reg_mod.curp IS NULL THEN
                ERROR "Debe ingresar CURP correcta"
                NEXT FIELD curp
            END IF

            IF reg_mod.rfc[1,4] <> reg_mod.curp[1,4] THEN
                ERROR "  LOS DATOS DEL RFC Y CURP SON DIFERENTES "
                ATTRIBUTE(NORMAL)
                SLEEP 3
                ERROR ""
                NEXT FIELD rfc
            END IF

        AFTER FIELD fecha_examen
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF reg_mod.fecha_examen IS NULL OR
               reg_mod.fecha_examen = '          ' THEN
                ERROR "La Fecha no puede ser Nula o Blancos "
                SLEEP 2
                NEXT FIELD fecha_examen
            END IF

            IF reg_mod.fecha_examen < TODAY THEN
                ERROR "La Fecha no puede ser Menor al dia de HOY"
                SLEEP 2
                NEXT FIELD fecha_examen
            END IF

        AFTER FIELD hora_examen
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD PREVIOUS
            END IF


            IF reg_mod.hora_examen[1,2] > 24
            OR reg_mod.hora_examen[1,2] < 00
            OR reg_mod.hora_examen IS NULL THEN
               ERROR "Hora es menor a 00 o mayor 24 "
                SLEEP 2
                NEXT FIELD hora_examen
            END IF

            IF reg_mod.hora_examen[1,2] = 24 THEN
               IF reg.hora_examen[3,4] <> 00 THEN
                  ERROR "Minutos de la Hora de examen deben ser 00 pora las 24 "
                  SLEEP 2
                  NEXT FIELD hora_examen
                END IF
            ELSE
               IF reg_mod.hora_examen[3,4] > 59
               OR reg_mod.hora_examen[3,4] < 00 THEN
                  ERROR "Minutos de la Hora de examen debe estar en el rango de 00 a 59"
                  SLEEP 2
                  NEXT FIELD hora_examen
                END IF
            END IF

        AFTER FIELD tpo_examen
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD PREVIOUS
            END IF

           IF reg_mod.tpo_examen IS NULL THEN
               CALL llama_tpo_examen() RETURNING reg_mod.tpo_examen
               IF reg_mod.tpo_examen IS NULL THEN
                   NEXT FIELD tpo_examen
               ELSE
               -- Valida que examen le corresponda al promotor por su status

                   LET v_status = null

                   SELECT pro_mae_promotor.status
                     INTO v_status
                     FROM pro_mae_promotor
                    WHERE pro_mae_promotor.unico = reg_mod.curp
                      AND pro_mae_promotor.cod_promotor = reg_mod.cod_promotor --(v4)

                   IF v_status = 1  THEN   -- Promotor Alta
                      IF reg_mod.tpo_examen = 1 THEN  -- Postulacion
                         ERROR "Tipo de Examen no se permite a Promotor Activo 4"
                         SLEEP 2
                         NEXT FIELD tpo_examen
                      END IF
                   END IF

                IF  cve_afore = "516" THEN        --- validacion solo para XXI
                   IF  reg_mod.cod_promotor = "0000000000" THEN
                       IF reg.tpo_examen <> 1 THEN
                          ERROR "Tipo de examen no valido para el Promotor 5"
                          SLEEP 2
                          NEXT FIELD tpo_examen
                       END IF
                   ELSE
                       IF v_status = 1 and reg_mod.tpo_examen = 1  THEN
                          ERROR "Tipo de examen no valido para el Promotor 6"
                          SLEEP 2
                          NEXT FIELD tpo_examen
                       END IF
                   END IF
                END IF


                   SELECT desc_tipo
                   INTO   reg_mod.desc_tpo_examen
                   FROM   pro_tipo_examen
                   WHERE  cod_tipo = reg_mod.tpo_examen

                   DISPLAY reg_mod.tpo_examen TO tpo_examen                          --(v3)
                   DISPLAY reg_mod.desc_tpo_examen TO desc_tpo_examen

                   NEXT FIELD evento
               END IF
           ELSE
               -- Valida que examen le corresponda al promotor por su status

                   LET v_status = null

                   SELECT pro_mae_promotor.status
                     INTO v_status
                     FROM pro_mae_promotor
                    WHERE pro_mae_promotor.unico = reg_mod.curp
                      AND pro_mae_promotor.cod_promotor = reg_mod.cod_promotor --(v4)

                   IF v_status = 1  THEN   -- Promotor Alta
                      IF reg_mod.tpo_examen = 1 THEN  -- Postulacion
                         ERROR "Tipo de Examen no se permite a Promotor Activo"
                         SLEEP 2
                         NEXT FIELD tpo_examen
                      END IF
                   END IF

                IF  cve_afore = "516" THEN        --- validacion solo para XXI
                   IF  reg_mod.cod_promotor = "0000000000" THEN
                       IF reg.tpo_examen <> 1 THEN
                          ERROR "Tipo de examen no valido para el Promotor 5"
                          SLEEP 2
                          NEXT FIELD tpo_examen
                          SLEEP 2
                          NEXT FIELD tpo_examen
                       END IF
                   ELSE
                       IF v_status = 1 and reg_mod.tpo_examen = 1  THEN
                          ERROR "Tipo de examen no valido para el Promotor 6"
                          SLEEP 2
                          NEXT FIELD tpo_examen
                       END IF
                   END IF
                END IF



               SELECT desc_tipo
               INTO   reg_mod.desc_tpo_examen
               FROM   pro_tipo_examen
               WHERE  cod_tipo = reg_mod.tpo_examen

               IF SQLCA.SQLCODE <> 0 THEN
                   ERROR "   Tipo Examen no se encuentra en el Catalogo"
                   INITIALIZE reg_mod.tpo_examen TO NULL
                   DISPLAY reg_mod.tpo_examen TO tpo_examen
                   NEXT FIELD tpo_examen
               ELSE
                   DISPLAY reg_mod.desc_tpo_examen TO desc_tpo_examen
                   NEXT FIELD evento
               END IF
           END IF

        AFTER FIELD evento
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD PREVIOUS
            END IF

           IF reg_mod.evento IS NULL THEN
              CALL llama_evento() RETURNING reg_mod.evento, reg_mod.desc_eve_802

              IF reg_mod.evento IS NULL THEN
                 NEXT FIELD tpo_examen
              ELSE
                 DISPLAY reg_mod.evento TO evento
                 DISPLAY reg_mod.desc_eve_802 TO desc_eve_802
              END IF
           ELSE
               SELECT desc_evento
               INTO   reg_mod.desc_eve_802
               FROM pro_evento
               WHERE  cod_evento= reg_mod.evento
               GROUP BY 1

               IF SQLCA.SQLCODE <> 0 THEN
                  ERROR "   El Evento no se encuentra en el Catalogo"
                  INITIALIZE reg_mod.evento TO NULL
                  DISPLAY reg_mod.evento TO evento
                  NEXT FIELD evento
               ELSE
                  DISPLAY reg_mod.evento TO evento                       --(v3)
                  DISPLAY reg_mod.desc_eve_802 TO desc_eve_802
                  NEXT FIELD sede
               END IF
           END IF

        AFTER FIELD sede
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD PREVIOUS
            END IF

           IF reg_mod.sede IS NULL THEN
              CALL llama_sede() RETURNING reg_mod.sede, reg_mod.desc_sede, reg_mod.edo_sede

              IF reg_mod.sede IS NULL OR reg_mod.sede = 0 THEN
                 NEXT FIELD sede
              ELSE
                   SELECT estad_desc
                   INTO   reg_mod.desc_edo_sede
                   FROM   tab_estado
                   WHERE  estad_cod = reg_mod.edo_sede

                   DISPLAY reg_mod.sede TO sede
                   DISPLAY reg_mod.desc_sede TO desc_sede
                   DISPLAY reg_mod.edo_sede TO edo_sede
                   DISPLAY reg_mod.desc_edo_sede TO desc_edo_sede
                   NEXT FIELD num_vuelta
              END IF
           ELSE
               SELECT m.desc_sede, m.edo_sede
               INTO   reg_mod.desc_sede, reg_mod.edo_sede
               FROM pro_sede_examen m
               WHERE  m.cod_sede = reg_mod.sede
               GROUP BY 1,2
               IF SQLCA.SQLCODE <> 0 THEN
                  ERROR "   No se encuentra el Codigo de la Sede en el Catalogo"
                  NEXT FIELD sede
               ELSE
                   SELECT estad_desc
                   INTO   reg_mod.desc_edo_sede
                   FROM   tab_estado
                   WHERE  estad_cod = reg_mod.edo_sede

                   DISPLAY reg_mod.desc_sede TO desc_sede
                   DISPLAY reg_mod.edo_sede TO edo_sede
                   DISPLAY reg_mod.desc_edo_sede TO desc_edo_sede
                   DISPLAY reg_mod.desc_edo_sede TO desc_edo_sede
                   NEXT FIELD num_vuelta
               END IF
           END IF

        AFTER FIELD num_vuelta
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF reg_mod.num_vuelta IS NULL OR
               reg_mod.num_vuelta = '          ' THEN
                ERROR "Numero de vuelta no puede ser Nulo "
                SLEEP 2
                NEXT FIELD num_vuelta
            END IF
            IF reg_mod.num_vuelta > 2 or reg.num_vuelta < 0 THEN
               ERROR "Numero de vuelta debe ser 0,1 o 2 "
               SLEEP 2
               NEXT FIELD num_vuelta
            END IF

        ON KEY (ESC)

            EXIT INPUT
    END INPUT

    WHILE TRUE
        PROMPT "  DESEAS ACTUALIZAR S/N ? " FOR opc
            IF opc NOT MATCHES "[SsNn]" THEN
                CONTINUE WHILE
            ELSE
                EXIT WHILE
            END IF
    END WHILE

    IF opc MATCHES "[Nn]" THEN
        DISPLAY "  MODIFICACION CANCELADA  "
                AT 22,02 ATTRIBUTE(REVERSE) SLEEP 3
        CALL inicializa()
        RETURN
    END IF

    WHENEVER ERROR CONTINUE

    UPDATE pro_ctr_examen
    SET    pro_ctr_examen.paterno      = reg_mod.paterno,
           pro_ctr_examen.materno      = reg_mod.materno,
           pro_ctr_examen.nombres      = reg_mod.nombres,
           pro_ctr_examen.curp         = reg_mod.curp,
           pro_ctr_examen.fecha_examen = reg_mod.fecha_examen,
           pro_ctr_examen.hora_examen  = reg_mod.hora_examen,
           pro_ctr_examen.tpo_examen   = reg_mod.tpo_examen,
           pro_ctr_examen.evento       = reg_mod.evento,
           pro_ctr_examen.sede         = reg_mod.sede,
           pro_ctr_examen.num_vuelta   = reg_mod.num_vuelta
    WHERE  pro_ctr_examen.rfc  = reg_mod.rfc                          --(v1)
    AND    pro_ctr_examen.estado_registro = 0
    AND    pro_ctr_examen.cod_promotor = reg_mod.cod_promotor         --(v5)
--    WHERE  pro_ctr_examen.consecutivo  = reg_mod.consecutivo        --(v1)
--    AND    pro_ctr_examen.fecha_examen   = reg_mod.fecha_examen   -5-(v1)

    IF SQLCA.SQLCODE < 0 THEN
        LET x_error = "UPDATE pro_ctr_examen:",
                      "rfc ",reg_mod.rfc,
                      "consecutivo ",reg_mod.consecutivo,
                      err_get(SQLCA.SQLCODE)

                      CALL errorlog(x_error CLIPPED)

                      PROMPT " ERROR de UPDATE pro_ctr_examen AVISE A SISTEMAS "
                      FOR enter
   END IF

   WHENEVER ERROR STOP
   ERROR   "  REGISTRO MODIFICADO" ATTRIBUTE(REVERSE) SLEEP 3
   ERROR   ""
   CALL inicializa() #i
   RETURN
END FUNCTION


FUNCTION elimina()
#-----------------

    DEFINE #loc #smallint
        arr                   ,
        cont_reg2             ,
        cont_reg              ,
        i                     ,
        pos                   ,
        src                   ,
        v_status              SMALLINT

    OPEN WINDOW prom0065 AT 2,2 WITH FORM "PROM0065" ATTRIBUTE (BORDER)
    DISPLAY " Enter: Elimina          Ctrl-C: Salir                   "  AT 1,1 ATTRIBUTE(NORMAL)
    DISPLAY " ELIMINA " AT 1,65 ATTRIBUTE(REVERSE)
    DISPLAY " PROM006                 SOLICITUD  DE  EXAMEN ",
            "                               " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 2,65 ATTRIBUTE(REVERSE)
    DISPLAY "                         DATOS  DEL  PROMOTOR  ",
            "                               " AT 4,1 ATTRIBUTE(REVERSE)
    DISPLAY "                         DATOS  DEL  EXAMEN    ",
            "                               " AT 11,1 ATTRIBUTE(REVERSE)

    CLEAR FORM

    LET int_flag = FALSE

    CONSTRUCT BY NAME x_busca ON A.cod_promotor,
                                 A.cve_solicitud,
                                 A.paterno,
                                 A.materno,
                                 A.nombres,
                                 A.rfc,
                                 A.curp,
                                 A.fecha_examen,
                                 A.hora_examen,
                                 A.tpo_examen,
                                 A.evento,
                                 A.sede,
                                 A.result,
                                 A.consecutivo,
                                 A.estado_registro

        ON KEY (CONTROL-C)
                LET int_flag=TRUE
                EXIT CONSTRUCT

        ON KEY (INTERRUPT)
                LET int_flag=TRUE
                EXIT CONSTRUCT

        ON KEY (Esc)
            LET int_flag = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    IF int_flag = TRUE THEN
            LET int_flag = FALSE
        CLOSE WINDOW prom0065
        RETURN
    END IF

    LET txt_1 = " SELECT A.cod_promotor    ,",
                "        A.cve_solicitud   ,",
                "        A.paterno         ,",
                "        A.materno         ,",
                "        A.nombres         ,",
                "        A.rfc             ,",
                "        A.curp            ,",
                "        A.fecha_examen    ,",
                "        A.hora_examen     ,",
                "        A.tpo_examen      ,",
                "        ''                ,",#desc_tpo_examen
                "        A.evento          ,",
                "        ''                ,",#desc_evento
                "        A.sede            ,",
                "        ''                ,",#desc_sede
                "        B.edo_sede        ,",
                "        ''                ,",#desc_edo_sede
                "        A.num_vuelta      ,",
                "        A.evento_803      ,",
                "        ''                ,",#desc_evento_803
                "        A.fecha_exa_803    ,",
                "        A.hora_exa_803     ,",
                "        A.calif           ,",
                "        A.result          ,",
                "        ''                ,",#desc_result
                "        A.consecutivo     ,",
                "        A.estado_registro ,",
                "        ''                 ",
                " FROM   pro_ctr_examen A, pro_sede_examen B ",
                " WHERE  A.sede = B.cod_sede ",
                " AND    A.estado_registro  = ", 0,
                " AND    ",x_busca CLIPPED

    PREPARE pre_3 FROM txt_1
    DECLARE cur_3 CURSOR FOR pre_3

    LET pos = 1
    ERROR "PROCESANDO INFORMACION"
    FOREACH cur_3 INTO arr_1[pos].*
        IF pos = 2000 THEN
            ERROR "ARREGLO LLENO"
            EXIT FOREACH
        END IF

        SELECT A.desc_status_corta
        INTO   arr_1[pos].desc_edo_reg
        FROM   pro_status_interno A
        WHERE  A.status_interno = arr_1[pos].estado_registro

        IF arr_1[pos].estado_registro > 0 THEN

            PROMPT "SOLICITUD DEL RFC <",arr_1[pos].rfc,
                   "> SE ENCUENTRA EN EL ESTADO : ",
                   arr_1[pos].desc_edo_reg CLIPPED,
                   " " ATTRIBUTE(REVERSE)
            FOR opc ATTRIBUTE(REVERSE)
            CONTINUE FOREACH
        END IF

        SELECT desc_tipo
        INTO   arr_1[pos].desc_tpo_examen
        FROM   pro_tipo_examen
        WHERE  cod_tipo = arr_1[pos].tpo_examen

        SELECT desc_evento
        INTO   arr_1[pos].desc_eve_802
        FROM   pro_evento
        WHERE  cod_evento = arr_1[pos].evento

        SELECT desc_evento
        INTO   arr_1[pos].desc_eve_803
        FROM   pro_evento
        WHERE  cod_evento = arr_1[pos].evento_803


        SELECT desc_sede
        INTO   arr_1[pos].desc_sede
        FROM   pro_sede_examen
        WHERE  cod_sede = arr_1[pos].sede

        SELECT estad_desc
        INTO   arr_1[pos].desc_edo_sede
        FROM   tab_estado
        WHERE  estad_cod = arr_1[pos].edo_sede

        -- Descripcion del resultado de examen

        SELECT A.desc_result
        INTO   arr_1[pos].desc_result
        FROM   pro_result_examen A
        WHERE  A.cod_result = arr_1[pos].cod_result

        LET pos = pos + 1
    END FOREACH


    ERROR ""
    IF pos = 1 THEN
        INITIALIZE reg.* TO NULL
        CLEAR FORM
        ERROR "NO EXISTE REGISTRO A ELIMINAR" ATTRIBUTE(NORMAL)
        CLOSE WINDOW prom0065
        RETURN
    END IF

    LET cont_reg = pos-1

--isss

    DISPLAY "  REGISTROS CARGADOS  ",cont_reg ,"" AT  22,02 ATTRIBUTE(REVERSE)

    CALL SET_COUNT(pos-1)
    DISPLAY ARRAY arr_1 TO scr_1.*

        ON KEY ( CONTROL-M )

            PROMPT "  DESEA ELIMAR EL REGISTRO S/N " FOR CHAR enter
            IF enter MATCHES "[Ss]" THEN

               LET pos = ARR_CURR()

               SELECT A.estado_registro
               INTO   v_status
               FROM   pro_ctr_examen A
               --WHERE  A.cod_promotor = arr_1[pos].cod_promotor   --(v1)
               --AND    A.consecutivo =  arr_1[pos].consecutivo    --(v1)
               WHERE  A.rfc          =  arr_1[pos].rfc             --(v1)
               AND    A.fecha_examen =  arr_1[pos].fecha_examen    --(v1)

               IF  v_status <> 0 THEN
                  PROMPT "  NO PUEDE SER ELIMINADO ESTE REGISTRO" FOR CHAR enter
                  EXIT DISPLAY
               ELSE
                  LET arr = ARR_CURR()
                  LET src = SCR_LINE()

                  DELETE
                  FROM   pro_ctr_examen
                  -- WHERE  cod_promotor = arr_1[arr].cod_promotor --(v1)
                  -- AND    consecutivo =  arr_1[arr].consecutivo  --(v1)
                  WHERE  rfc          = arr_1[arr].rfc             --(v1)
                  AND    fecha_examen =  arr_1[arr].fecha_examen  --(v1)
                  AND    cod_promotor =  arr_1[arr].cod_promotor   --(v5)

                  IF SQLCA.SQLCODE < 0 THEN
                     LET x_error = "DELETE pro_ctr_examen:",
                                   "rfc ",arr_1[arr].rfc,
                                   "consecutivo ",arr_1[arr].consecutivo,
                                   err_get(SQLCA.SQLCODE)

                     CALL errorlog(x_error CLIPPED)

                     PROMPT "   ERROR AL BORRAR EL REGISTRO AVISE A SISTEMAS "
                     FOR enter
                     EXIT PROGRAM
                  END IF
                  WHENEVER ERROR STOP

                  ERROR   "  REGISTRO ELIMINADO" ATTRIBUTE(REVERSE) SLEEP 3
                  ERROR   ""

                  EXIT DISPLAY
               END IF
            END IF
            EXIT DISPLAY

        ON KEY ( CONTROL-C )
            EXIT DISPLAY

        ON KEY ( INTERRUPT )
            EXIT DISPLAY
    END DISPLAY
CLOSE WINDOW  prom0065
END FUNCTION
