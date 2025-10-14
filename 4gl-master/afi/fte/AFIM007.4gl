##########################################################################
#Proyecto          => AFORE ( MEXICO )                                   #
#Propietario       => E.F.P.                                             #
#Programa AFIM007  => MANTENIMIENTO ARCHIVO RECEPCION DE SOLICITUDES     #
#Autor             => MAURO MUNIZ CABALLERO                              #
#Fecha             => 5 DE MARZO DE 2001                                 #
#Modifica          => MAURO MUNIZ CABALLERO                              #
#Fecha modifica    => 1 DE AGOSTO DE 2005                                #
#                     CAPTURA TRABAJADORES INDEPENDIENTES                #
#Sistema           => AFI.                                               #
#Modifico          => EDUARDO JOAQUIN RESENDIZ MEDINA                    #
#Fecha             => 27 DE ABRIL 20 2006 (Circ 7 - 12) VALIDA nss       #
##########################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg ARRAY[5000] OF RECORD
      n_folio          DECIMAL(10,0),
      curp             CHAR(18) ,
      n_seguro         CHAR(11) ,
      estado_sol       CHAR(4)  ,
      estado_exp       SMALLINT ,
      cod_promotor     CHAR(10) ,
      no_referencia    CHAR(10) ,
      fecha_solicitud  DATE     ,
      fecha_recepcion  DATE
    END RECORD

    DEFINE reg2 ARRAY[5000] OF RECORD
      tipo_solicitud   SMALLINT,
      n_folio          DECIMAL(10,0),
      cod_promotor     CHAR(10) ,
      fecha_solicitud  DATE     ,
      fecha_recepcion  DATE     ,
      n_seguro         CHAR(11) ,
      estado_sol       CHAR(4)  ,
      estado_exp       SMALLINT ,
      no_referencia    CHAR(10) ,
      curp             CHAR(18) ,
      lote_cap         CHAR(10) ,
      fecha_actualiza  DATE
    END RECORD

    DEFINE k_reg RECORD
      tipo_solicitud   SMALLINT ,
      n_folio          DECIMAL(10,0),
      cod_promotor     CHAR(10)
    END RECORD

    DEFINE z_reg RECORD
      orden_1  SMALLINT ,
      orden_2  SMALLINT ,
      orden_3  SMALLINT ,
      orden_4  SMALLINT ,
      orden_5  SMALLINT ,
      orden_6  SMALLINT ,
      orden_7  SMALLINT ,
      orden_8  SMALLINT ,
      orden_9  SMALLINT
    END RECORD

    DEFINE z_reg2 ARRAY[9] OF SMALLINT

    DEFINE
      g_afore       RECORD LIKE tab_afore_local.*,
      g_paramgrales RECORD LIKE seg_modulo.*

    DEFINE
      HOY          ,
      sig_fecha    DATE

    DEFINE
      accion               CHAR(1)   ,
      enter                CHAR(1)   ,
      imp_resumen          CHAR(1)   ,
      imprime_pantalla     CHAR(1)   ,
      pasa_curp            CHAR(1)   ,
      pasa                 CHAR(1)   ,
      opcion_menu          CHAR(7)   ,
      HHMMSS               CHAR(6)   ,
      HORA                 CHAR(8)   ,
      g_usuario            CHAR(8)   ,
      existe_cod_promotor  CHAR(10)  ,
      aux_cod_promotor     CHAR(10)  ,
      opcion_tipo_doc      CHAR(25)  ,
      desc_estado          CHAR(60)  ,
      desc_err             CHAR(60)  ,
      G_LISTA              CHAR(100) ,
      G_LISTA2             CHAR(100) ,
      lp                   CHAR(100) ,
      ejecuta              CHAR(300) ,
      txt_ger              CHAR(500) ,
      txt_1                CHAR(1000),
      k                    CHAR(1000),
      sel_where            CHAR(1000),
      cla_where            CHAR(1000)

    DEFINE
      i                 ,
      j                 ,
      sw_1              ,
      sw_2              ,
      sw_3              ,
      sw_4              ,
      digito            ,
      aux_status        ,
      cont_reg_pantalla ,
      s_status          ,
      cont_lineas       ,
      dig_curp          SMALLINT

    DEFINE
      contador_1        ,
      pos               INTEGER

    DEFINE
      usuario            CHAR(8) ,
      nom_analista       CHAR(30),
      tipo_solic         SMALLINT,
      desc_solic         CHAR(30),
      lote_cap           CHAR(10),
      estado_sol         CHAR(4) ,
      estado_exp         SMALLINT

    DEFINE cod_ret       CHAR(1)

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST ,
            INPUT WRAP

    CALL STARTLOG('AFIM007.log')
    CALL inicio()
    CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

    CREATE TEMP TABLE tmp_ctr_doc
      (n_folio         DECIMAL(10,0),
       n_seguro        CHAR(11) ,
       estado_sol      CHAR(4)  ,
       estado_exp      SMALLINT ,
       cod_promotor    CHAR(10) ,
       no_referencia   CHAR(10) ,
       fecha_solicitud DATE     ,
       fecha_recepcion DATE      )

    CREATE INDEX ind_10 ON tmp_ctr_doc (n_folio)

    LET HOY    = TODAY
    LET HORA   = TIME
    LET HHMMSS = HORA[1,2],HORA[4,5],HORA[7,8]
    LET accion = NULL

    SELECT * ,
           USER
    INTO   g_paramgrales.*,
           g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "AFIM00711" ATTRIBUTE( BORDER)
    WHILE TRUE
        LET opcion_tipo_doc = 'Recepcion de Solicitudes'
        DISPLAY " AFIM007                      CONTROL DOCUMENTAL                               " AT 3,1 ATTRIBUTE(REVERSE) 
        DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
        DISPLAY "                           RECEPCION DE SOLICITUDES                            " AT 7,1 ATTRIBUTE(REVERSE) 
        WHILE TRUE
            MENU opcion_tipo_doc
                COMMAND "Agrega" "Agrega Recepcion de Solicitudes"
                    LET accion = 'A'
                    CALL Inicializa_1()
                    CALL Agrega() #a
                    CALL Inicializa_1()
                COMMAND "Consulta" "Consulta Recepcion de Solicitudes"
                    CALL Inicializa_1()
                    DISPLAY "  " AT 5,1
                    LET sw_1 = 1
                    CALL busca_usuario()  #bu

                    IF  consulta() THEN#c
                        OPEN WINDOW ventana_5 AT 3,2 
                        WITH FORM "AFIM0075" ATTRIBUTE( BORDER)
                        DISPLAY "" AT 1,1
                        DISPLAY "" AT 2,1
                        DISPLAY " AFIM007                      CONTROL DOCUMENTAL                               " AT 3,1 ATTRIBUTE(REVERSE)
                        DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
                        DISPLAY "                           RECEPCION DE SOLICITUDES                            " AT 7,1 ATTRIBUTE(REVERSE)
                        DISPLAY "[ Ctrl-C ] Salir      [ Ctrl-I ] Imprimir" 
                                 AT 2,1 ATTRIBUTE(BOLD) 
                        DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
                        DISPLAY BY NAME usuario, nom_analista
                        DISPLAY ARRAY reg2 TO scr_2.*
                        ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")

                        ON KEY ( INTERRUPT )
                            EXIT DISPLAY
                            CLOSE WINDOW ventana_5
                        ON KEY ( CONTROL-I )
                            WHILE TRUE
                            PROMPT "Esta seguro de querer imprimir [S/N] "
                                FOR imprime_pantalla
                                    IF  imprime_pantalla MATCHES"[sSnN]" THEN
                                        LET G_LISTA = g_paramgrales.ruta_listados
                                                      CLIPPED, "/",g_usuario CLIPPED,
                                                      ".DCTOS_REC.",
                                                      HOY USING "DD-MM-YY","_",
                                                      HHMMSS CLIPPED
                                        IF  imprime_pantalla MATCHES "[nN]" THEN
                                            EXIT DISPLAY
                                        END IF
                                        IF  imprime_pantalla MATCHES "[sS]" THEN
                                            START REPORT listado_1 TO G_LISTA
                                              LET contador_1  = 0
                                              LET cont_lineas = 0
                                              ERROR "GENERANDO LISTADO"
                                              FOR j = 1 TO cont_reg_pantalla 
                                                IF  reg2[j].n_folio IS NULL THEN
                                                    EXIT FOR
                                                ELSE
                                                    OUTPUT to REPORT 
                                                    listado_1(reg2[j].*)
                                                END IF
                                              END FOR
                                            FINISH REPORT listado_1
                                            LET lp ="lp ",G_LISTA CLIPPED
                                            RUN lp
                                            LET G_LISTA2= "chmod 777 ", G_LISTA
                                            RUN G_LISTA2
                                            ERROR"LISTADO GENERANDO" SLEEP 2
                                        END IF
                                        EXIT WHILE
                                    END IF
                                END WHILE
                            END DISPLAY
                        CLOSE WINDOW ventana_5
                    ELSE
                        CALL Inicializa_1()
                    END IF
                    LET accion = NULL
                    CALL Inicializa_1()
                COMMAND "Modifica" "Modificación de las Solicitudes"
                    CALL Inicializa_1()
                    DISPLAY "  " AT 5,1
                    LET sw_1 = 1
                    CALL busca_usuario()  #bu
                    IF  Modifica() THEN#c
                        CALL Hace_modificaciones()  #hm
                    ELSE
                        CALL Inicializa_1()
                    END IF
                    LET accion = NULL
                    CALL Inicializa_1()
                COMMAND "Elimina" "Elimina Recepcion de Solicitudes"
                    CALL Inicializa_1()
                    LET sw_3 = 1
                    LET opcion_menu = "ELIMINA"
                    CALL busca_usuario()  #bu
                    CALL elimina() #e
                    LET accion = NULL
                    CALL Inicializa_1()
                COMMAND "Salir" "Regresar al Menu Anterior"
                    EXIT PROGRAM
            END MENU
            EXIT WHILE
        END WHILE
    END WHILE

    CLOSE WINDOW ventana_1

END FUNCTION

FUNCTION Agrega()
#a---------------

    DEFINE
      arr_c      ,
      scr_l      ,
      i          ,
      st_int     ,
      con_sales  ,
      con_input  SMALLINT

    LET con_sales = 0

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY "[ Esc ] Graba    [ Ctrl-I ] Imprime    [ Ctrl-C ] Salir " AT 2,1 ATTRIBUTE(BOLD)
    DISPLAY " AGREGA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)
    DISPLAY "Reg: ", con_sales USING "####&" AT 07,65 ATTRIBUTE (REVERSE)

    LET usuario    = g_usuario
    LET tipo_solic = ''
    LET desc_solic = ''
    LET aux_status = 0
    LET int_flag   = FALSE
    LET lote_cap   = NULL

    DISPLAY BY NAME lote_cap

    INPUT BY NAME usuario, tipo_solic, lote_cap WITHOUT DEFAULTS
      BEFORE FIELD usuario
        DISPLAY BY NAME usuario
        SELECT usuario_desc
        INTO   nom_analista
        FROM   seg_usuario
        WHERE  seg_usuario.usuario_cod = usuario

        DISPLAY BY NAME nom_analista
        NEXT FIELD tipo_solic

      AFTER FIELD tipo_solic
        IF tipo_solic IS NULL THEN
          CALL ven_solicitud() RETURNING tipo_solic, desc_solic
          IF tipo_solic IS NULL THEN
            ERROR "Tipo de solicitud NO puede ser nulo"
            SLEEP 2
            ERROR ""
            NEXT FIELD tipo_solic
          ELSE
            DISPLAY BY NAME tipo_solic, desc_solic
          END IF
        ELSE
          SELECT desc_solicitud
          INTO   desc_solic
          FROM   tab_tipo_solic
          WHERE  tipo_solicitud = tipo_solic

          IF SQLCA.SQLCODE <> 0 THEN
            ERROR "Tipo de solicitud no existe, ingrese tipo correcto"
            SLEEP 2
            ERROR ""
            NEXT FIELD tipo_solic
          ELSE
            DISPLAY BY NAME desc_solic
          END IF
        END IF

      BEFORE FIELD lote_cap
        LET lote_cap = NULL
        DISPLAY BY NAME lote_cap

      AFTER FIELD lote_cap
        IF lote_cap IS NULL THEN
           PROMPT "El -CAMPO LOTE- esta nulo, desea pasar al siguiente campo [S/N]: " FOR enter
           IF enter MATCHES"[Ss]" THEN
              EXIT INPUT
           ELSE
              NEXT FIELD lote_cap
           END IF 
        ELSE
          EXIT INPUT
        END IF

        IF int_flag THEN
          DISPLAY "               " AT 07,65
          ATTRIBUTE (REVERSE)
          LET int_flag = FALSE
          LET con_input = 1
        END IF
      END INPUT

      IF int_flag THEN
        DISPLAY "               " AT 07,65 ATTRIBUTE (REVERSE)
        LET int_flag = FALSE
        LET con_input = 1
      END IF

      INPUT ARRAY reg WITHOUT DEFAULTS FROM scr_1.*
        BEFORE INPUT
        IF con_input THEN
          EXIT INPUT
        END IF

        BEFORE FIELD n_folio
          LET arr_c = ARR_CURR()
          LET scr_l = SCR_LINE()
          LET con_sales = ARR_CURR()

        AFTER FIELD n_folio
          IF reg[arr_c].n_folio IS NOT NULL OR
             reg[arr_c].n_folio <> ' ' THEN
             DISPLAY "Reg: ", con_sales USING "####&" AT 07,65
             ATTRIBUTE (REVERSE)
          END IF

          IF reg[arr_c].n_folio IS NULL THEN
            ERROR "Numero de folio NO puede ser NULO"
            NEXT FIELD n_folio
          ELSE
            SELECT "X"
            FROM   afi_recepcion arec
            WHERE  arec.n_folio        = reg[arr_c].n_folio
            AND    arec.tipo_solicitud = tipo_solic

            IF STATUS <> NOTFOUND THEN
              ERROR "Folio ya ingresado en recepcion de solicitudes"
              NEXT FIELD n_folio
            END IF

            SELECT "X"
            FROM   afi_mae_afiliado M
            WHERE  M.n_folio        = reg[arr_c].n_folio
            AND    M.tipo_solicitud = tipo_solic

            IF STATUS <> NOTFOUND THEN
              ERROR "Folio ya ingresado en maestro de afiliados"
              NEXT FIELD n_folio
            END IF

            SELECT "X"
            FROM   afi_mae_modifica MM 
            WHERE  MM.folio_nvo      = reg[arr_c].n_folio
            AND    MM.tipo_solicitud = tipo_solic
            AND    MM.cod_operacion  = 0
            AND    MM.diag_proceso   = 0

            IF STATUS <> NOTFOUND THEN
              ERROR "Folio ya ingresado en el maestro de modificados"
              NEXT FIELD n_folio
            {
            ELSE
                SELECT status_interno
                INTO   st_int
                FROM   afi_solicitud sol
                WHERE  sol.n_folio = reg[arr_c].n_folio
                AND    sol.tipo_solicitud = tipo_solic

                IF SQLCA.SQLCODE = 0 THEN
                  IF st_int < 40 THEN
                      ERROR "Folio ya ingresado en captura de solicitudes"
                  ELSE
                      ERROR "Folio ya enviado a certificar"
                      NEXT FIELD n_folio
                  END IF
                END IF
             }
            END IF
          END IF

        BEFORE FIELD curp
          LET arr_c = ARR_CURR()
          LET scr_l = SCR_LINE()

        AFTER FIELD curp
          IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD n_folio
          END IF

          IF tipo_solic = 8 THEN
             IF reg[arr_c].curp IS NULL THEN
               ERROR "Campo CURP NO puede ser NULO"
               NEXT FIELD curp
             END IF

             SELECT n_seguro
             INTO   reg[arr_c].n_seguro
             FROM   afi_mae_afiliado M
             WHERE  M.n_unico         = reg[arr_c].curp
             AND    M.tipo_solicitud <> 5

             IF STATUS = NOTFOUND THEN
                SELECT "X"
                FROM   afi_recepcion
                WHERE  curp            = reg[arr_c].curp
                AND    fecha_actualiza = HOY

                IF STATUS <> NOTFOUND THEN
                   ERROR "CURP ya ingresada en esta fecha"
                   NEXT FIELD curp
                END IF
             ELSE
                SELECT "X"
                FROM   cta_act_marca cc
                WHERE  cc.nss        = reg[arr_c].n_seguro
                AND    cc.marca_cod IN (SELECT b.marca_resulta
                                        FROM   tab_marca b
                                        WHERE  b.ind_habilita = 1)
                GROUP BY 1

                IF SQLCA.SQLCODE <> 0 THEN
                   ERROR "CURP ya ingresada en maestro de afiliados"
                   NEXT FIELD curp
                END IF
             END IF
          ELSE
             IF reg[arr_c].curp IS NULL THEN
                NEXT FIELD n_seguro
             END IF
          END IF

          IF LENGTH(reg[arr_c].curp) < 18 AND
             LENGTH(reg[arr_c].curp) > 0  THEN
             ERROR "Debe ingresar CURP completa"
             NEXT FIELD curp
          ELSE
             IF reg[arr_c].curp[1] = " " THEN
                ERROR "Debe ingresar CURP correcta"
                NEXT FIELD curp
             END IF

             CALL valida_est_curp(reg[arr_c].curp) RETURNING pasa_curp, desc_err

             IF pasa_curp = 1 THEN
                ERROR "", desc_err
                LET pasa_curp = 0
                NEXT FIELD curp
             END IF

             CALL var_dig_curp(reg[arr_c].curp) RETURNING pasa, dig_curp

             IF pasa = 0 THEN
                ERROR "Digito Verificador Invalido curp, el digito es: ",
                dig_curp
                SLEEP 2
                LET pasa = 0
                NEXT FIELD curp
             END IF
          END IF

        BEFORE FIELD n_seguro
          LET arr_c = ARR_CURR()
          LET scr_l = SCR_LINE()

        AFTER FIELD n_seguro
          IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD n_folio
          END IF
--->erm 05 Abril 2006 permite captura  a solicitudes 8 y valida que NO exista en DB

        IF tipo_solic = 8 THEN
           IF reg[arr_c].n_seguro IS NOT NULL OR
              reg[arr_c].n_seguro <> " " THEN 
              LET pasa = 0
              CALL valida_g_nss(reg[arr_c].n_seguro) RETURNING pasa, desc_err, cod_ret

              IF pasa = 1 THEN
                 CASE cod_ret
                    WHEN "1"
                      ERROR "",desc_err
                      SLEEP  2
                      ERROR ""
                      NEXT FIELD n_seguro
                    WHEN "2"
                      ERROR "",desc_err
                      SLEEP  2
                      ERROR ""
                      NEXT FIELD n_seguro
                    WHEN "3"
                      ERROR "",desc_err
                      SLEEP  2
                      ERROR ""
                      NEXT FIELD n_seguro
                    WHEN "4"
                      ERROR "",desc_err
                      SLEEP  2
                      ERROR ""
                      NEXT FIELD n_seguro
                 END CASE
              END IF

              CALL valida_nss_db(reg[arr_c].n_seguro) RETURNING pasa, desc_err, cod_ret

              IF pasa = 1 THEN
                 CASE cod_ret
                    WHEN "1"
                      ERROR "", desc_err
                      SLEEP  2
                      ERROR ""
                      NEXT FIELD n_seguro
                    WHEN "2"
                      ERROR "",desc_err
                      SLEEP  2
                      ERROR ""
                      NEXT FIELD n_seguro
                    WHEN "3"
                      ERROR "",desc_err
                      SLEEP  2
                      ERROR ""
                      NEXT FIELD n_seguro
                    WHEN "4"
                      ERROR "",desc_err
                      SLEEP  2
                      ERROR ""
                      NEXT FIELD n_seguro
                 END CASE
              END IF

              CALL digito_verif(reg[arr_c].n_seguro[1,10],10) RETURNING digito

              IF digito = 32000 THEN
                 ERROR "N.S.S. solo contiene digitos"
                 NEXT FIELD n_seguro
              END IF

              IF LENGTH(reg[arr_c].n_seguro) = 11  AND
                 digito <> reg[arr_c].n_seguro[11] THEN
                 ERROR "Digito Verificador Invalido, el digito debe ser: ",
                       digito
                 SLEEP 2
                 NEXT FIELD n_seguro
              END IF

              IF reg[arr_c].n_seguro[11] <> "1" AND
                 reg[arr_c].n_seguro[11] <> "2" AND
                 reg[arr_c].n_seguro[11] <> "3" AND
                 reg[arr_c].n_seguro[11] <> "4" AND
                 reg[arr_c].n_seguro[11] <> "5" AND
                 reg[arr_c].n_seguro[11] <> "6" AND
                 reg[arr_c].n_seguro[11] <> "7" AND
                 reg[arr_c].n_seguro[11] <> "8" AND
                 reg[arr_c].n_seguro[11] <> "9" AND
                 reg[arr_c].n_seguro[11] <> "0" THEN
                 ERROR "N.S.S. solo contiene digitos"
                 NEXT FIELD n_seguro 
              END IF


           ELSE
---<erm
            --IF tipo_solic = 8 THEN       ---erm 05 Abril 2006
               LET ejecuta = "EXECUTE PROCEDURE fn_obtiene_nti(",
                             "'", reg[arr_c].curp, "')"
               LET ejecuta = ejecuta CLIPPED

               PREPARE eje_obtiene FROM ejecuta
               DECLARE cur_obtiene CURSOR FOR eje_obtiene
               OPEN    cur_obtiene
               FETCH   cur_obtiene INTO reg[arr_c].n_seguro
               CLOSE   cur_obtiene

               DISPLAY reg[arr_c].n_seguro TO scr_1[scr_l].n_seguro

               IF reg[arr_c].n_seguro IS NULL THEN
                  ERROR "Campo NO puede ser NULO"
                  NEXT FIELD n_seguro
               END IF

               NEXT FIELD cod_promotor
            --ELSE                        ---erm 05 Abril 2006
               IF reg[arr_c].n_seguro IS NULL THEN
                 ERROR "Campo NO puede ser NULO" 
                 NEXT FIELD n_seguro 
               END IF

               IF LENGTH(reg[arr_c].n_seguro) <> 11 THEN
                 ERROR "Debe ingresar N.S.S. completo"
                 NEXT FIELD n_seguro 
               END IF

           END IF                     ---erm 05 Abril 2006
        --END IF                         ---erm 05 Abril 2006
        ELSE

            CALL valida_g_nss(reg[arr_c].n_seguro) RETURNING pasa, desc_err, cod_ret

            IF pasa = 1 THEN
               CASE cod_ret
                  WHEN "1"
                    ERROR "",desc_err
                    SLEEP  2
                    ERROR ""
                    NEXT FIELD n_seguro
                  WHEN "2"
                    ERROR "",desc_err
                    SLEEP  2
                    ERROR ""
                    NEXT FIELD n_seguro
                  WHEN "3"
                    ERROR "",desc_err
                    SLEEP  2
                    ERROR ""
                    NEXT FIELD n_seguro
                  WHEN "4"
                    ERROR "",desc_err
                    SLEEP  2
                    ERROR ""
                    NEXT FIELD n_seguro
               END CASE
            END IF

            CALL valida_nss_db(reg[arr_c].n_seguro) RETURNING pasa, desc_err, cod_ret

            IF pasa = 1 THEN
               CASE cod_ret
                  WHEN "1"
                    ERROR "", desc_err
                    SLEEP  2
                    ERROR ""
                    NEXT FIELD n_seguro
                  WHEN "2"
                    ERROR "",desc_err
                    SLEEP  2
                    ERROR ""
                    NEXT FIELD n_seguro
                  WHEN "3"
                    ERROR "",desc_err
                    SLEEP  2
                    ERROR ""
                    NEXT FIELD n_seguro
                  WHEN "4"
                    ERROR "",desc_err
                    SLEEP  2
                    ERROR ""
                    NEXT FIELD n_seguro
               END CASE
            END IF
            CALL digito_verif(reg[arr_c].n_seguro[1,10],10) RETURNING digito

               IF digito = 32000 THEN
                 ERROR "N.S.S. solo contiene digitos"
                 NEXT FIELD n_seguro
               END IF

               IF LENGTH(reg[arr_c].n_seguro) = 11  AND
                  digito <> reg[arr_c].n_seguro[11] THEN
                 ERROR "Digito Verificador Invalido, el digito debe ser: ",
                        digito
                 SLEEP 2
                 NEXT FIELD n_seguro
               END IF

               IF reg[arr_c].n_seguro[11] <> "1" AND
                  reg[arr_c].n_seguro[11] <> "2" AND
                  reg[arr_c].n_seguro[11] <> "3" AND
                  reg[arr_c].n_seguro[11] <> "4" AND
                  reg[arr_c].n_seguro[11] <> "5" AND
                  reg[arr_c].n_seguro[11] <> "6" AND
                  reg[arr_c].n_seguro[11] <> "7" AND
                  reg[arr_c].n_seguro[11] <> "8" AND
                  reg[arr_c].n_seguro[11] <> "9" AND
                  reg[arr_c].n_seguro[11] <> "0" THEN
                  ERROR "N.S.S. solo contiene digitos"
                  NEXT FIELD n_seguro 
               END IF

        END IF

          BEFORE FIELD cod_promotor
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

          AFTER FIELD cod_promotor
            DISPLAY "                               " AT 1,1
            LET aux_status = 0

            IF reg[arr_c].cod_promotor IS NULL THEN
              CALL despliegue() #de
              LET reg[arr_c].cod_promotor = aux_cod_promotor
            END IF

            IF existe_cod_promotor IS NOT NULL THEN
              IF existe_cod_promotor <> reg[arr_c].cod_promotor THEN
                LET reg[arr_c].cod_promotor = existe_cod_promotor
                ERROR "No puede modificar el codigo del promotor"
                SLEEP 3
              END IF
            ELSE
              SELECT A.status ,
                     A.agenc_cod
              INTO   s_status
              FROM   pro_mae_promotor A
              WHERE  A.cod_promotor = reg[arr_c].cod_promotor

              IF STATUS = NOTFOUND THEN
                DISPLAY "Codigo de promotor no existe" AT 1,1
                ATTRIBUTES(REVERSE)
                SLEEP 3
                LET aux_status  = 1
                LET reg[arr_c].estado_sol = '2', reg[arr_c].estado_sol CLIPPED
              ELSE
                CASE s_status
                   WHEN 2
                     DISPLAY "PROMOTOR SUSPENDIDO" AT 1,1
                             ATTRIBUTES(REVERSE)
                     SLEEP 3
                     LET aux_status = 1
                     LET reg[arr_c].estado_sol = '2',
                                                reg[arr_c].estado_sol CLIPPED
                   WHEN 3
                     DISPLAY "PROMOTOR DADO DE BAJA" AT 1,1
                             ATTRIBUTES(REVERSE)
                     SLEEP 3
                     LET aux_status = 1
                     LET reg[arr_c].estado_sol = '2',
                                                reg[arr_c].estado_sol CLIPPED
                   OTHERWISE
                     DISPLAY "                               " AT 1,1
                     LET reg[arr_c].estado_sol = '0',
                                                reg[arr_c].estado_sol CLIPPED
                END CASE
              DISPLAY reg[arr_c].* TO scr_1[scr_l].*
              END IF
            END IF

            LET reg[arr_c].estado_exp = aux_status
            DISPLAY reg[arr_c].estado_sol TO scr_1[scr_l].estado_sol
            DISPLAY reg[arr_c].estado_exp TO scr_1[scr_l].estado_exp

        BEFORE FIELD no_referencia
          LET arr_c = ARR_CURR()
          LET scr_l = SCR_LINE()

        AFTER FIELD no_referencia
          IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD cod_promotor
          END IF

          IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD fecha_solicitud
          END IF

        BEFORE FIELD fecha_solicitud
          LET arr_c = ARR_CURR()
          LET scr_l = SCR_LINE()

        AFTER FIELD fecha_solicitud
          IF reg[arr_c].fecha_solicitud IS NULL THEN
             ERROR "Fecha de firma NO puede ser NULO"
             NEXT FIELD fecha_solicitud 
          END IF

          CALL cal_fecha_avant(reg[arr_c].fecha_solicitud, 20)
                RETURNING sig_fecha 

          IF HOY >= sig_fecha THEN
             ERROR "Fecha firma vencida"
             SLEEP 2
             LET reg[arr_c].fecha_solicitud = NULL
             NEXT FIELD fecha_solicitud
             --LET reg[arr_c].estado_sol = '8', reg[arr_c].estado_sol CLIPPED
          END IF

          DISPLAY reg[arr_c].estado_sol TO scr_1[scr_l].estado_sol
          DISPLAY reg[arr_c].estado_exp TO scr_1[scr_l].estado_exp

        BEFORE FIELD fecha_recepcion
          LET arr_c = ARR_CURR()
          LET scr_l = SCR_LINE()
          LET reg[arr_c].fecha_recepcion = HOY
          {IF reg[arr_c].fecha_recepcion >= sig_fecha THEN
             ERROR "Fecha firma vencida"
             SLEEP 2
             LET reg[arr_c].estado_sol = '8', reg[arr_c].estado_sol CLIPPED
           END IF}

        AFTER FIELD fecha_recepcion
          IF reg[arr_c].fecha_recepcion IS NULL THEN
             ERROR "Fecha de recepcion NO puede ser NULO"
             NEXT FIELD fecha_recepcion
          END IF
           {IF reg[arr_c].fecha_recepcion >= sig_fecha THEN
             ERROR "Fecha firma vencida"
             SLEEP 2
             LET reg[arr_c].estado_sol = '8', reg[arr_c].estado_sol CLIPPED
           END IF}

          DISPLAY reg[arr_c].estado_sol TO scr_1[scr_l].estado_sol
          DISPLAY reg[arr_c].estado_exp TO scr_1[scr_l].estado_exp

          INSERT INTO afi_recepcion
          VALUES(reg[arr_c].n_folio         , #n_folio
                 tipo_solic                 , #tipo_solicitud
                 reg[arr_c].n_seguro        , #n_seguro
                 reg[arr_c].curp            , #curp
                 reg[arr_c].fecha_recepcion , #fecha_recpecion
                 g_usuario                  , #usuario
                 reg[arr_c].cod_promotor    , #promotor
                 reg[arr_c].fecha_solicitud , #fecha_solicitud
                 reg[arr_c].estado_sol      , #estado_sol
                 reg[arr_c].no_referencia   , #sucursal
                 reg[arr_c].estado_exp      , #estado_exp
                 HOY                        , #fecha_actualiza
                 lote_cap                     #no_lote
                )

         ON KEY ( INTERRUPT )
           CALL Inicializa_1()
           DISPLAY "               " AT 07,65
           ATTRIBUTE (REVERSE)
           EXIT INPUT

         ON KEY (CONTROL-I)
           WHILE TRUE
             PROMPT "Esta seguro de querer imprimir [S/N] "
             FOR imprime_pantalla
             IF imprime_pantalla MATCHES"[sSnN]" THEN
               LET G_LISTA = g_paramgrales.ruta_listados CLIPPED ,"/",
                             g_usuario CLIPPED,".DCTOS_REC.",
                             HOY USING "DD-MM-YY","_",HHMMSS CLIPPED

               IF imprime_pantalla MATCHES "[sS]" THEN
                 START REPORT listado_1 TO G_LISTA
                   LET contador_1  = 0
                   LET cont_lineas = 0
                   ERROR"GENERANDO LISTADO"
                   FOR j= 1 TO 400 
                     IF reg2[j].n_folio IS NULL OR 
                        reg2[j].cod_promotor  IS NULL THEN
                       EXIT FOR
                     ELSE
                       OUTPUT to REPORT listado_1(reg2[j].*) #l1
                     END IF
                   END FOR
                 FINISH REPORT listado_1
                 LET lp ="lp ",G_LISTA CLIPPED
                 RUN lp
                 LET G_LISTA2 = "CHMOD 777 ", G_LISTA
                 RUN G_LISTA2
                 ERROR"LISTADO GENERANDO" SLEEP 2
               END IF

               LET G_LISTA = g_paramgrales.ruta_listados CLIPPED ,"/",
                             HOY USING "DD",HHMMSS CLIPPED
               START REPORT listado_3 TO G_LISTA
                 FOR j= 1 TO 400 
                   IF reg[j].n_folio IS NULL OR 
                      reg[j].cod_promotor  IS NULL THEN
                     EXIT FOR
                   ELSE
                     OUTPUT to REPORT listado_3(reg[j].*) #l3
                   END IF
                 END FOR
               FINISH REPORT listado_3
               LET G_LISTA = "CHMOD 777 ", G_LISTA
               RUN G_LISTA
               EXIT WHILE
             END IF
           END WHILE
           DISPLAY "               " AT 07,65
           ATTRIBUTE (REVERSE)

         ON KEY ( ESC )
           ERROR "REGISTROS AGREGADOS" SLEEP 2
           ERROR ""
           CALL Inicializa_1()
           DISPLAY "               " AT 07,65
           ATTRIBUTE (REVERSE)
           EXIT INPUT
         END INPUT

END FUNCTION

FUNCTION Modifica()
#m-----------------

    DEFINE
      folio_qq  INTEGER ,
      ts_qq     SMALLINT,
      si_qq     SMALLINT

    OPEN WINDOW ventana_2 AT 7,5 WITH FORM "AFIM0072" ATTRIBUTE( BORDER)
    DISPLAY "[ ESC ] Sig. pantalla     [ Ctrl-C ] Salir                                     " AT 1,1 ATTRIBUTES(REVERSE)
    DISPLAY "                         OPCIONES DE MODIFICACION                              " AT 2,1 ATTRIBUTE(REVERSE)

    LET pos = 2

    IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      LET int_flag = FALSE
      CONSTRUCT BY NAME cla_where ON
                tipo_solicitud,
                n_folio,
                cod_promotor,
                fecha_solicitud,
                fecha_recepcion,
                n_seguro,
                usuario,
                curp,
                fecha_actualiza

      IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLOSE WINDOW ventana_2  RETURN FALSE
      END IF

      CALL ordena() #o
      CLEAR FORM
      LET z_reg2[1] = z_reg.orden_1
      LET z_reg2[2] = z_reg.orden_2
      LET z_reg2[3] = z_reg.orden_3
      LET z_reg2[4] = z_reg.orden_4
      LET z_reg2[5] = z_reg.orden_5
      LET z_reg2[6] = z_reg.orden_6
      LET z_reg2[7] = z_reg.orden_7
      LET z_reg2[8] = z_reg.orden_8
      LET z_reg2[9] = z_reg.orden_9

      LET sel_where = " SELECT a.tipo_solicitud, a.n_folio,",
                      " a.cod_promotor, a.fecha_solicitud,",
                      " a.fecha_recepcion, a.n_seguro,",
                      " a.estado_sol, a.estado_exp, ",
                      " a.sucursal,",
                      " a.curp, a.fecha_actualiza ",
                      " FROM afi_recepcion a",
                      " WHERE ", cla_where CLIPPED ,
                      " ORDER BY ", z_reg.orden_1,",",z_reg.orden_2,",",
                                    z_reg.orden_3,",",z_reg.orden_4,",",
                                    z_reg.orden_5,",",z_reg.orden_6,",",
                                    z_reg.orden_7,",",z_reg.orden_8,",",
                                    z_reg.orden_9
      LET sel_where = sel_where CLIPPED

      PREPARE qry_consul FROM sel_where
      DECLARE cursor_c CURSOR FOR qry_consul
      LET pos = 1

      FOREACH cursor_c INTO reg2[pos].tipo_solicitud,
                            reg2[pos].n_folio,
                            reg2[pos].cod_promotor,
                            reg2[pos].fecha_solicitud,
                            reg2[pos].fecha_recepcion,
                            reg2[pos].n_seguro,
                            reg2[pos].estado_sol,
                            reg2[pos].estado_exp,
                            reg2[pos].no_referencia,
                            reg2[pos].curp,
                            reg2[pos].fecha_actualiza

        SELECT S.n_folio, S.tipo_solicitud, S.status_interno
        INTO   folio_qq, ts_qq, si_qq
        FROM   afi_solicitud S
        WHERE  S.n_folio = reg2[pos].n_folio
        AND    S.tipo_solicitud = reg2[pos].tipo_solicitud

        IF STATUS = NOTFOUND THEN
          LET pos = pos + 1
        ELSE
          IF si_qq < 21  THEN
            LET pos = pos + 1
          END IF
        END IF
      END FOREACH

      CLOSE WINDOW ventana_2
      INITIALIZE reg2[pos].* TO NULL

      IF (pos-1) >= 1 THEN
        CALL  SET_COUNT(pos-1)
        ERROR ""
        RETURN TRUE
      ELSE
        ERROR "ARCHIVO DE RECEP. SOLIC. VACIO"
        SLEEP 2
        ERROR ""
        RETURN FALSE
      END IF
    END IF

END FUNCTION

FUNCTION Hace_modificaciones()
#hm---------------------------

    DEFINE
      arr_c               ,
      scr_l               ,
      i                   SMALLINT,
      con_input           SMALLINT,
      tipo_solicitud_ant  SMALLINT,
      folio_ant           INTEGER ,
      promotor_paso       CHAR(10),
      n_seguro_ant        CHAR(11),
      curp_ant            CHAR(18)

    LET promotor_paso = ""

    OPEN WINDOW ventana_5 AT 3,2 WITH FORM "AFIM0075" ATTRIBUTE( BORDER)

    DISPLAY BY NAME usuario, nom_analista
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY "[ Ctrl-C ] Salir      [ Ctrl-I ] Imprimir" 
             AT 2,1 ATTRIBUTE(BOLD) 
    DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
    DISPLAY " AFIM007                      CONTROL DOCUMENTAL                               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY "                           RECEPCION DE SOLICITUDES                            " AT 7,1 ATTRIBUTE(REVERSE)

    INPUT ARRAY reg2 WITHOUT DEFAULTS FROM scr_2.*
    ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")

      BEFORE FIELD tipo_solicitud
        LET existe_cod_promotor = NULL
        LET arr_c               = ARR_CURR()
        LET scr_l               = SCR_LINE()
        LET folio_ant           = reg2[arr_c].n_folio
        LET tipo_solicitud_ant  = reg2[arr_c].tipo_solicitud
        LET n_seguro_ant        = reg2[arr_c].n_seguro
        LET curp_ant            = reg2[arr_c].curp

      AFTER FIELD n_folio
        IF reg2[arr_c].n_folio IS NULL THEN
          ERROR "Numero de folio NO puede ser NULO"
          NEXT FIELD n_folio
        END IF

        SELECT "X"
          FROM afi_mae_afiliado M
         WHERE M.n_folio        = reg2[arr_c].n_folio
           AND M.tipo_solicitud = reg2[arr_c].tipo_solicitud

        IF STATUS <> NOTFOUND THEN
           ERROR "Folio ya ingresado en maestro de afiliados"
           NEXT FIELD tipo_solicitud
        END IF

        #### Val fol ####
        SELECT 'X'
        FROM   afi_mae_modifica a
        WHERE  a.folio_nvo      = reg2[arr_c].n_folio
        AND    a.tipo_solicitud = tipo_solic
        AND    a.cod_operacion  = 0
        AND    a.diag_proceso   = 0

        IF STATUS <> NOTFOUND THEN
          ERROR "Folio existente en el maestro de modificados"
          SLEEP 3
          NEXT FIELD n_folio
        END IF

        SELECT "X"
          FROM  afi_solicitud S
         WHERE  S.n_folio        = reg2[arr_c].n_folio
           AND  S.tipo_solicitud = reg2[arr_c].tipo_solicitud

        IF SQLCA.SQLCODE = 0 THEN
            ERROR "Folio ya ingresado en solicitudes de afiliacion"
            #NEXT FIELD n_folio
            NEXT FIELD n_seguro
        END IF

        #### Val fol ####
      BEFORE FIELD n_seguro
        LET arr_c = ARR_CURR()
        LET scr_l = SCR_LINE()

      AFTER FIELD n_seguro
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
          NEXT FIELD n_folio
        END IF

        IF reg2[arr_c].n_seguro IS NULL THEN
          ERROR "Campo NO puede ser NULO"
          NEXT FIELD n_seguro 
        END IF

        IF LENGTH(reg2[arr_c].n_seguro) <> 11 THEN
          ERROR "Debe ingresar N.S.S. completo" 
          NEXT FIELD n_seguro 
        END IF

        CALL digito_verif(reg2[arr_c].n_seguro[1,10],10) RETURNING digito

        IF digito = 32000 THEN
          ERROR "N.S.S. solo contiene digitos"
          NEXT FIELD n_seguro
        END IF

        IF LENGTH(reg2[arr_c].n_seguro) = 11 AND
          digito <> reg2[arr_c].n_seguro[11] THEN
          ERROR "Digito Verificador Invalido, el digito debe ser: ",
                 digito
          SLEEP 2
          NEXT FIELD n_seguro
        END IF

        IF reg2[arr_c].n_seguro[11] <> "1" AND
           reg2[arr_c].n_seguro[11] <> "2" AND
           reg2[arr_c].n_seguro[11] <> "3" AND
           reg2[arr_c].n_seguro[11] <> "4" AND
           reg2[arr_c].n_seguro[11] <> "5" AND
           reg2[arr_c].n_seguro[11] <> "6" AND
           reg2[arr_c].n_seguro[11] <> "7" AND
           reg2[arr_c].n_seguro[11] <> "8" AND
           reg2[arr_c].n_seguro[11] <> "9" AND
           reg2[arr_c].n_seguro[11] <> "0" THEN
            ERROR "N.S.S. solo contiene digitos"
            NEXT FIELD n_seguro
        END IF

        SELECT "X"
        FROM   afi_mae_afiliado M
        WHERE  M.n_seguro        = reg2[arr_c].n_seguro
        AND    M.tipo_solicitud <> 5

        IF STATUS = NOTFOUND THEN
          #NEXT FIELD cod_promotor
          NEXT FIELD estado_sol
        ELSE
          ERROR"NSS ya ingresado en maestro de afiliados"
          NEXT FIELD tipo_solicitud
        END IF

      AFTER FIELD estado_sol
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
          NEXT FIELD n_seguro
        END IF

        IF reg2[arr_c].estado_sol IS NULL THEN
          ERROR "Campo estado solicitud NO puede ser NULO"
          NEXT FIELD estado_sol
        END IF

        NEXT FIELD estado_exp

      AFTER FIELD estado_exp
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
          NEXT FIELD estado_exp
        END IF

        IF reg2[arr_c].estado_exp IS NULL THEN 
          ERROR "Campo estado expediente NO puede ser NULO"
          NEXT FIELD estado_exp
        END IF

        NEXT FIELD cod_promotor

      BEFORE FIELD cod_promotor
        LET arr_c = ARR_CURR()
        LET scr_l = SCR_LINE()
        LET aux_status = 0
        DISPLAY "                               " AT 1,1
        LET promotor_paso = reg2[arr_c].cod_promotor

      AFTER FIELD cod_promotor
        IF reg2[arr_c].cod_promotor IS NULL THEN
          CALL despliegue() #de
          LET reg2[arr_c].cod_promotor = aux_cod_promotor
        END IF

        IF existe_cod_promotor IS NOT NULL THEN
          IF existe_cod_promotor <> reg2[arr_c].cod_promotor THEN
            LET reg2[arr_c].cod_promotor = existe_cod_promotor
            ERROR "No puede modificar el codigo del promotor"
          END IF
        ELSE
          SELECT A.status ,
                 A.agenc_cod
          INTO   s_status
          FROM   pro_mae_promotor A
          WHERE  A.cod_promotor = reg2[arr_c].cod_promotor

          IF STATUS = NOTFOUND THEN
            DISPLAY "Codigo de promotor no existe" AT 1,1 ATTRIBUTES(REVERSE)
            SLEEP 3
            LET aux_status = 1
            LET reg2[arr_c].estado_sol = reg2[arr_c].estado_sol CLIPPED, '2'
          ELSE
            CASE s_status
              WHEN 2 
                DISPLAY "PROMOTOR SUSPENDIDO" AT 1,1
                         ATTRIBUTES(REVERSE)
                SLEEP 3 
                LET aux_status = 1
                LET reg2[arr_c].estado_sol = reg2[arr_c].estado_sol CLIPPED, '2'
              WHEN 3 
                DISPLAY "PROMOTOR DADO DE BAJA" AT 1,1 ATTRIBUTES(REVERSE)
                SLEEP 3
                LET aux_status = 1
                LET reg2[arr_c].estado_sol = reg2[arr_c].estado_sol CLIPPED, '2'
              OTHERWISE
                DISPLAY "                               " AT 1,1
                LET reg2[arr_c].estado_sol = reg2[arr_c].estado_sol CLIPPED, '0'
            END CASE
          # DISPLAY reg[arr_c].* TO scr_1[scr_l].*
          END IF
        END IF

        IF aux_status > 0 THEN
          LET reg2[arr_c].cod_promotor = promotor_paso
          DISPLAY reg2[arr_c].cod_promotor TO scr_2[scr_l].cod_promotor
          DISPLAY "                               " AT 1,1
          NEXT FIELD n_folio
        END IF

        LET reg2[arr_c].estado_exp = 0
        DISPLAY reg2[arr_c].estado_sol TO scr_2[scr_l].estado_sol
        DISPLAY reg2[arr_c].estado_exp TO scr_2[scr_l].estado_exp

       BEFORE FIELD no_referencia
         LET arr_c = ARR_CURR()
         LET scr_l = SCR_LINE()

       AFTER FIELD no_referencia
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
          NEXT FIELD cod_promotor
        END IF

        IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
          NEXT FIELD fecha_solicitud
        END IF

        BEFORE FIELD fecha_solicitud
          LET arr_c = ARR_CURR()
          LET scr_l = SCR_LINE()

        AFTER FIELD fecha_solicitud
          IF reg2[arr_c].fecha_solicitud IS NULL THEN
            ERROR "Fecha de firma NO puede ser NULO"
            NEXT FIELD fecha_solicitud 
          END IF

        {
         IF reg2[arr_c].fecha_solicitud < HOY THEN
            LET reg2[arr_c].estado_sol = reg2[arr_c].estado_sol CLIPPED, '4'
         END IF
         DISPLAY reg2[arr_c].estado_sol TO scr_2[scr_l].estado_sol
         DISPLAY reg2[arr_c].estado_exp TO scr_2[scr_l].estado_exp
        }

        BEFORE FIELD fecha_recepcion
          LET arr_c = ARR_CURR()
          LET scr_l = SCR_LINE()

        AFTER FIELD fecha_recepcion
          IF reg2[arr_c].fecha_recepcion IS NULL THEN
            ERROR "Fecha de recepcion NO puede ser NULO"
            NEXT FIELD fecha_recepcion
          END IF

          NEXT FIELD curp

        {
         IF reg2[arr_c].fecha_recepcion < reg2[arr_c].fecha_solicitud THEN
            LET reg2[arr_c].estado_sol = reg2[arr_c].estado_sol CLIPPED, '8'
         END IF

         DISPLAY reg2[arr_c].estado_sol TO scr_2[scr_l].estado_sol
         DISPLAY reg2[arr_c].estado_exp TO scr_2[scr_l].estado_exp
        }

        BEFORE FIELD curp
          LET arr_c = ARR_CURR()
          LET scr_l = SCR_LINE()

        AFTER FIELD curp
          IF reg2[arr_c].tipo_solicitud = 8 THEN
            IF reg2[arr_c].curp IS NULL THEN
               PROMPT "El -CAMPO CURP- esta nulo, desea dejar asi el ",
                      "campo [S/N]: " FOR enter
               IF enter NOT MATCHES"[Ss]" THEN
                  NEXT FIELD curp
               END IF
            END IF
          END IF

          IF folio_ant          <> reg2[arr_c].n_folio OR
             tipo_solicitud_ant <> reg2[arr_c].tipo_solicitud THEN
              UPDATE afi_condicion_exp
                 SET n_folio        = reg2[arr_c].n_folio,
                     tipo_solicitud = reg2[arr_c].tipo_solicitud
               WHERE afi_condicion_exp.n_folio        = folio_ant
                 AND afi_condicion_exp.tipo_solicitud = tipo_solicitud_ant
          END IF

          IF folio_ant          <> reg2[arr_c].n_folio        OR
             tipo_solicitud_ant <> reg2[arr_c].tipo_solicitud OR
             n_seguro_ant       <> reg2[arr_c].n_seguro       THEN
              UPDATE afi_expediente
                 SET n_folio        = reg2[arr_c].n_folio       ,
                     tipo_solicitud = reg2[arr_c].tipo_solicitud,
                     n_seguro       = reg2[arr_c].n_seguro
               WHERE afi_expediente.n_folio        = folio_ant
                 AND afi_expediente.tipo_solicitud = tipo_solicitud_ant
                 AND afi_expediente.n_seguro       = n_seguro_ant
          END IF

          IF tipo_solic IS NULL THEN
             NEXT FIELD tipo_solicitud
          END IF

          UPDATE afi_recepcion
             SET n_folio          = reg2[arr_c].n_folio         ,
                 tipo_solicitud   = reg2[arr_c].tipo_solicitud  ,
                 n_seguro         = reg2[arr_c].n_seguro        ,
                 fecha_recepcion  = reg2[arr_c].fecha_recepcion ,
                 usuario          = usuario                     ,
                 cod_promotor     = reg2[arr_c].cod_promotor    ,
                 sucursal         = reg2[arr_c].no_referencia   ,
                 fecha_solicitud  = reg2[arr_c].fecha_solicitud ,
                 estado_sol       = reg2[arr_c].estado_sol      ,
                 estado_exp       = reg2[arr_c].estado_exp      ,
                 curp             = reg2[arr_c].curp            ,
                 fecha_actualiza  = HOY
           WHERE afi_recepcion.n_folio        = folio_ant
             AND afi_recepcion.tipo_solicitud = tipo_solicitud_ant
             AND afi_recepcion.n_seguro       = n_seguro_ant

        ON KEY ( INTERRUPT )
          CALL Inicializa_1()
          EXIT INPUT
          CLOSE WINDOW ventana_5

        END INPUT

    CLOSE WINDOW ventana_5

END FUNCTION

FUNCTION Inicializa_1()
#i1--------------------

    DEFINE i SMALLINT

    IF accion = 'A' THEN
       INITIALIZE reg TO NULL
       FOR i = 1 TO 5
        DISPLAY reg[i].* TO scr_1[i].* ATTRIBUTE(NORMAL)
       END FOR
       CLEAR FORM
    END IF

    IF accion = 'C' OR
       accion = 'M' OR
       accion = 'E' THEN
       INITIALIZE reg2 TO NULL

       FOR i = 1 TO 04
        DISPLAY reg2[i].* TO scr_2[i].* ATTRIBUTE(NORMAL)
       END FOR

       CLEAR FORM
       LET accion = NULL
    END IF

END FUNCTION

FUNCTION elimina()
#e----------------

    DEFINE
        arr_c ,
        scr_l ,
        i     SMALLINT

    DEFINE
        sale      CHAR(1)

    OPEN WINDOW ventana_5 AT 3,2 WITH FORM "AFIM0075" ATTRIBUTE( BORDER)
    DISPLAY BY NAME usuario, nom_analista
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY "[ Ctrl-C ] Salir    [ Ctrl-B ] Elimina" AT 2,1 ATTRIBUTE(BOLD) 
    DISPLAY " ELIMINA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
    DISPLAY " AFIM007                      CONTROL DOCUMENTAL                               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY "                           RECEPCION DE SOLICITUDES                            " AT 7,1 ATTRIBUTE(REVERSE)

    LET sw_1 = 1

    IF  consulta() = FALSE THEN
        LET sw_3 = 0
        LET accion = NULL
        CLOSE WINDOW ventana_5
        RETURN
    END IF

    WHILE TRUE
        LET sw_3 = 0
        DISPLAY ARRAY reg2 TO scr_2.*
        ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")
            ON KEY ( INTERRUPT )
                LET accion = NULL
                CALL Inicializa_1()
                LET sw_3 = 1
                EXIT DISPLAY
                CLOSE WINDOW ventana_5
            ON KEY ( CONTROL-B )
                LET aux_status = NULL
                LET i = ARR_CURR()

                SELECT A.estado_exp
                INTO   aux_status
                FROM   afi_recepcion A
                WHERE  A.n_folio        = reg2[i].n_folio
                AND    A.tipo_solicitud = reg2[i].tipo_solicitud

                IF aux_status = 0 OR
                   aux_status = 1 THEN
                   DELETE
                   FROM   afi_expediente
                   WHERE  afi_expediente.n_folio        = reg2[i].n_folio
                   AND    afi_expediente.tipo_solicitud = reg2[i].tipo_solicitud

                   DELETE
                   FROM   afi_condicion_exp
                   WHERE  afi_condicion_exp.n_folio        = reg2[i].n_folio
                   AND    afi_condicion_exp.tipo_solicitud = reg2[i].tipo_solicitud

                   DELETE
                   FROM   afi_recepcion
                   WHERE  afi_recepcion.n_folio        = reg2[i].n_folio
                   AND    afi_recepcion.tipo_solicitud = reg2[i].tipo_solicitud

                   PROMPT "Registro con condicion documental eliminado",
                   "; [Enter] para salir"
                   FOR enter
                ELSE
                   DELETE
                   FROM   afi_expediente
                   WHERE  afi_expediente.n_folio        = reg2[i].n_folio
                   AND    afi_expediente.tipo_solicitud = reg2[i].tipo_solicitud

                   DELETE
                   FROM   afi_condicion_exp
                   WHERE  afi_condicion_exp.n_folio        = reg2[i].n_folio
                   AND    afi_condicion_exp.tipo_solicitud = reg2[i].tipo_solicitud

                   DELETE
                   FROM   afi_recepcion
                   WHERE  afi_recepcion.n_folio        = reg2[i].n_folio
                   AND    afi_recepcion.tipo_solicitud = reg2[i].tipo_solicitud

                   PROMPT "Registro sin condicion documental eliminado",
                   "; [Enter] para salir"
                   FOR enter
                END IF
                EXIT DISPLAY
        END DISPLAY

        LET accion = NULL
        CALL Inicializa_1()

        IF  sw_3 = 1 THEN
          EXIT WHILE
        END IF
    END WHILE

    LET opcion_menu = ""
    CLOSE WINDOW ventana_5

END FUNCTION

FUNCTION consulta()
#c-----------------

    OPEN WINDOW ventana_2 AT 7,5 WITH FORM "AFIM0072" ATTRIBUTE( BORDER)
    DISPLAY "                         OPCIONES DE CONSULTA                                  " AT 2,1 ATTRIBUTE(REVERSE) 
    DISPLAY "[ ESC ] Sig. pantalla     [ Ctrl-C ] Salir                                     " AT 1,1 ATTRIBUTES(REVERSE)


    LET pos = 2
    IF  (pos-1) >= 1 THEN     
        CALL SET_COUNT(pos-1) 
        LET int_flag = FALSE

        CONSTRUCT BY NAME cla_where
        ON a.tipo_solicitud,
           a.n_folio,
           a.cod_promotor,
           a.fecha_solicitud,
           a.fecha_recepcion,
           a.n_seguro,
           a.usuario,
           a.curp,
           a.fecha_actualiza

        IF  int_flag = TRUE THEN
            LET int_flag = FALSE
            ERROR "BUSQUEDA CANCELADA..."
            SLEEP 2
            ERROR ""
            CLOSE WINDOW ventana_2
            RETURN FALSE
        END IF

        CALL ordena() #o

        CLEAR FORM

        LET z_reg2[1] = z_reg.orden_1
        LET z_reg2[2] = z_reg.orden_2
        LET z_reg2[3] = z_reg.orden_3
        LET z_reg2[4] = z_reg.orden_4
        LET z_reg2[5] = z_reg.orden_5
        LET z_reg2[6] = z_reg.orden_6
        LET z_reg2[7] = z_reg.orden_7
        LET z_reg2[8] = z_reg.orden_8
        LET z_reg2[9] = z_reg.orden_9

        LET sel_where = " SELECT a.tipo_solicitud, a.n_folio, ", 
                        " a.cod_promotor, a.fecha_solicitud, ",
                        " a.fecha_recepcion, a.n_seguro, a.sucursal, ",
                        " a.curp, a.fecha_actualiza, a.lote_cap, ",
                        " a.estado_sol, a.estado_exp ",
                        " FROM afi_recepcion a",
                        " WHERE ", cla_where CLIPPED ,
                        " ORDER BY ", z_reg.orden_1,",",z_reg.orden_2,",",
                                      z_reg.orden_3,",",z_reg.orden_4,",",
                                      z_reg.orden_5,",",z_reg.orden_6,",",
                                      z_reg.orden_7,",",z_reg.orden_8,",",
                                      z_reg.orden_9

        LET sel_where = sel_where CLIPPED

     #LET sel_where = "echo ",sel_where clipped, " > x.sql"  
     #RUN sel_where

        PREPARE qry_modif FROM sel_where
        DECLARE cursor_m CURSOR FOR qry_modif
        LET pos = 1

        FOREACH cursor_m INTO reg2[pos].tipo_solicitud,
                              reg2[pos].n_folio,
                              reg2[pos].cod_promotor,
                              reg2[pos].fecha_solicitud,
                              reg2[pos].fecha_recepcion,
                              reg2[pos].n_seguro,
                              reg2[pos].no_referencia,
                              reg2[pos].curp,
                              reg2[pos].fecha_actualiza,
                              reg2[pos].lote_cap,
                              reg2[pos].estado_sol,
                              reg2[pos].estado_exp
            LET pos = pos + 1
        END FOREACH

        CLOSE WINDOW ventana_2
        INITIALIZE reg2[pos].* TO NULL
        IF (pos-1) >= 1 THEN
           CALL  SET_COUNT(pos-1)
           ERROR ""
           RETURN TRUE
        ELSE
           ERROR "ARCHIVO DE RECEP. SOLIC. VACIO"
           SLEEP 2
           ERROR ""
           RETURN FALSE
        END IF
    END IF

END FUNCTION

FUNCTION ingreso_cod_promotor()
#icp---------------------------

    DEFINE
        arr_c  ,
        scr_l  ,
        i      SMALLINT

    INPUT BY NAME k_reg.cod_promotor WITHOUT DEFAULTS
        AFTER FIELD cod_promotor
            IF k_reg.cod_promotor IS NULL THEN
               ERROR "Codigo de promotor no puede ser NULO ...Reintente"
               NEXT FIELD cod_promotor
            ELSE
               RETURN
            END IF

        END INPUT

END FUNCTION

FUNCTION ordena()
#o---------------

    OPEN WINDOW ventana_3 AT 4,5 WITH FORM "AFIM0073" ATTRIBUTE(BORDER)
    DISPLAY " [ Esc ] Grabar      [ Ctrl-C ] Salir"
             AT 1,1 ATTRIBUTE(BOLD) 
    DISPLAY "                      OPCIONES DE ORDENAMIENTO                                        " AT 2,1 ATTRIBUTE(REVERSE,BOLD)

    LET z_reg.orden_1 = 1
    LET z_reg.orden_2 = 2
    LET z_reg.orden_3 = 3
    LET z_reg.orden_4 = 4
    LET z_reg.orden_5 = 5
    LET z_reg.orden_6 = 6
    LET z_reg.orden_7 = 7
    LET z_reg.orden_8 = 8
    LET z_reg.orden_9 = 9

    DISPLAY BY NAME z_reg.*

    INPUT BY NAME z_reg.* WITHOUT DEFAULTS
        AFTER FIELD orden_1
           IF z_reg.orden_1 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_1
           ELSE
               IF z_reg.orden_1 < 1 OR z_reg.orden_1 > 9 THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_1
               END IF
           END IF

        AFTER FIELD orden_2
            IF z_reg.orden_2 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_2
            ELSE
               IF z_reg.orden_2 < 1 OR z_reg.orden_2 > 9 THEN
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
              IF z_reg.orden_3 < 1 OR z_reg.orden_3 > 9 THEN
                 ERROR "La opcion de orden digitada no existe...Reingrese"
                 NEXT FIELD orden_3
              END IF

              IF (z_reg.orden_3 = z_reg.orden_1) OR
                 (z_reg.orden_3 = z_reg.orden_2) THEN
                  ERROR "Opcion ya digitada ...Reintente "
                  NEXT FIELD orden_3
              END IF
           END IF

        AFTER FIELD orden_4
           IF z_reg.orden_4 IS NULL THEN
              ERROR "Campo NO puede ser NULO"
              NEXT FIELD orden_4
           ELSE
              IF z_reg.orden_4 < 1 OR z_reg.orden_4 > 9  THEN
                 ERROR "La opcion de orden digitada no existe...Reingrese"
                 NEXT FIELD orden_4
              END IF

              IF (z_reg.orden_4 = z_reg.orden_1) OR
                 (z_reg.orden_4 = z_reg.orden_2) OR
                 (z_reg.orden_4 = z_reg.orden_3) THEN
                 ERROR "Opcion ya digitada ...Reintente "
                 NEXT FIELD orden_4
               END IF
           END IF

        AFTER FIELD orden_5
           IF z_reg.orden_5 IS NULL THEN
              ERROR "Campo NO puede ser NULO"
              NEXT FIELD orden_5
           ELSE
              IF z_reg.orden_5 < 1 OR z_reg.orden_5 > 9  THEN
                  ERROR "La opcion de orden digitada no existe...Reingrese"
                  NEXT FIELD orden_5
              END IF

              IF (z_reg.orden_5 = z_reg.orden_1) OR
                 (z_reg.orden_5 = z_reg.orden_2) OR
                 (z_reg.orden_5 = z_reg.orden_3) OR
                 (z_reg.orden_5 = z_reg.orden_4) THEN
                  ERROR "Opcion ya digitada ...Reintente "
                  NEXT FIELD orden_5
              END IF
           END IF

        AFTER FIELD orden_6
           IF z_reg.orden_6 IS NULL THEN
              ERROR "Campo NO puede ser NULO"
              NEXT FIELD orden_6
           ELSE
              IF z_reg.orden_6 < 1 OR z_reg.orden_6 > 9  THEN
                 ERROR "La opcion de orden digitada no existe...Reingrese"
                 NEXT FIELD orden_6
              END IF

              IF (z_reg.orden_6 = z_reg.orden_1) OR
                 (z_reg.orden_6 = z_reg.orden_2) OR
                 (z_reg.orden_6 = z_reg.orden_3) OR
                 (z_reg.orden_6 = z_reg.orden_4) OR
                 (z_reg.orden_6 = z_reg.orden_5) THEN
                 ERROR "Opcion ya digitada ...Reintente "
                 NEXT FIELD orden_6
              END IF
           END IF

        AFTER FIELD orden_7
           IF z_reg.orden_7 IS NULL THEN
              ERROR "Campo NO puede ser NULO"
              NEXT FIELD orden_7
           ELSE
              IF z_reg.orden_7 < 1 OR z_reg.orden_7 > 9  THEN
                 ERROR "La opcion de orden digitada no existe...Reingrese"
                 NEXT FIELD orden_7
              END IF
              IF (z_reg.orden_7 = z_reg.orden_1) OR
                 (z_reg.orden_7 = z_reg.orden_2) OR
                 (z_reg.orden_7 = z_reg.orden_3) OR
                 (z_reg.orden_7 = z_reg.orden_4) OR
                 (z_reg.orden_7 = z_reg.orden_5) OR
                 (z_reg.orden_7 = z_reg.orden_6) THEN
                 ERROR "Opcion ya digitada ...Reintente "
                 NEXT FIELD orden_7
              END IF
           END IF

        AFTER FIELD orden_8
           IF z_reg.orden_8 IS NULL THEN
              ERROR "Campo NO puede ser NULO"
              NEXT FIELD orden_8
           ELSE
              IF z_reg.orden_8 < 1 OR z_reg.orden_8 > 9  THEN
                 ERROR "La opcion de orden digitada no existe...Reingrese"
                 NEXT FIELD orden_8
              END IF

              IF (z_reg.orden_8 = z_reg.orden_1) OR
                 (z_reg.orden_8 = z_reg.orden_2) OR
                 (z_reg.orden_8 = z_reg.orden_3) OR
                 (z_reg.orden_8 = z_reg.orden_4) OR
                 (z_reg.orden_8 = z_reg.orden_5) OR
                 (z_reg.orden_8 = z_reg.orden_6) OR
                 (z_reg.orden_8 = z_reg.orden_7) THEN
                 ERROR "Opcion ya digitada ...Reintente "
                 NEXT FIELD orden_8
              END IF
           END IF

        AFTER FIELD orden_9
           IF z_reg.orden_9 IS NULL THEN
              ERROR "Campo NO puede ser NULO"
              NEXT FIELD orden_9
           ELSE
              IF z_reg.orden_9 < 1 OR z_reg.orden_9 > 9 THEN
                 ERROR "La opcion de orden digitada no existe...Reingrese"
                 NEXT FIELD orden_9
              END IF

              IF (z_reg.orden_9 = z_reg.orden_1) OR
                 (z_reg.orden_9 = z_reg.orden_2) OR
                 (z_reg.orden_9 = z_reg.orden_3) OR
                 (z_reg.orden_9 = z_reg.orden_4) OR
                 (z_reg.orden_9 = z_reg.orden_5) OR
                 (z_reg.orden_9 = z_reg.orden_6) OR
                 (z_reg.orden_9 = z_reg.orden_7) OR
                 (z_reg.orden_9 = z_reg.orden_8) THEN
                 ERROR "Opcion ya digitada ...Reintente "
                 NEXT FIELD orden_9
              END IF
           END IF

       ON KEY ( ESC )
           IF  z_reg.orden_1 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_1
           ELSE
               IF z_reg.orden_1 < 1 OR z_reg.orden_1 > 9  THEN
                  ERROR "La opcion de orden digitada no existe...Reingrese"
                  NEXT FIELD orden_1
               END IF
           END IF

           IF z_reg.orden_2 IS NULL THEN
              ERROR "Campo NO puede ser NULO"
              NEXT FIELD orden_2
           ELSE
              IF z_reg.orden_2 < 1 OR z_reg.orden_2 > 9  THEN
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
              IF z_reg.orden_3 < 1 OR z_reg.orden_3 > 9  THEN
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
              IF z_reg.orden_4 < 1 OR z_reg.orden_4 > 9  THEN
                 ERROR "La opcion de orden digitada no existe...Reingrese"
                 NEXT FIELD orden_4
              END IF

              IF (z_reg.orden_4 = z_reg.orden_1) OR
                 (z_reg.orden_4 = z_reg.orden_2) OR
                 (z_reg.orden_4 = z_reg.orden_3) THEN
                 ERROR "Opcion ya digitada ...Reintente "
                 NEXT FIELD orden_4
              END IF
           END IF

           IF z_reg.orden_5 IS NULL THEN
              ERROR "Campo NO puede ser NULO"
              NEXT FIELD orden_5
           ELSE
              IF z_reg.orden_5 < 1 OR z_reg.orden_5 > 9  THEN
                 ERROR "La opcion de orden digitada no existe...Reingrese"
                 NEXT FIELD orden_5
              END IF

              IF (z_reg.orden_5 = z_reg.orden_1) OR
                 (z_reg.orden_5 = z_reg.orden_2) OR
                 (z_reg.orden_5 = z_reg.orden_3) OR
                 (z_reg.orden_5 = z_reg.orden_4) THEN
                 ERROR "Opcion ya digitada ...Reintente "
                 NEXT FIELD orden_5
              END IF
           END IF

           IF z_reg.orden_6 IS NULL THEN
              ERROR "Campo NO puede ser NULO"
              NEXT FIELD orden_6
           ELSE
              IF z_reg.orden_6 < 1 OR z_reg.orden_6 > 9  THEN
                 ERROR "La opcion de orden digitada no existe...Reingrese"
                 NEXT FIELD orden_6
              END IF

              IF (z_reg.orden_6 = z_reg.orden_1) OR
                 (z_reg.orden_6 = z_reg.orden_2) OR
                 (z_reg.orden_6 = z_reg.orden_3) OR
                 (z_reg.orden_6 = z_reg.orden_4) OR
                 (z_reg.orden_6 = z_reg.orden_5) THEN
                 ERROR "Opcion ya digitada ...Reintente "
                 NEXT FIELD orden_6
               END IF
           END IF

           IF z_reg.orden_7 IS NULL THEN
              ERROR "Campo NO puede ser NULO"
              NEXT FIELD orden_7
           ELSE
              IF z_reg.orden_7 < 1 OR z_reg.orden_7 > 9  THEN
                 ERROR "La opcion de orden digitada no existe...Reingrese"
                 NEXT FIELD orden_7
              END IF

              IF (z_reg.orden_7 = z_reg.orden_1) OR
                 (z_reg.orden_7 = z_reg.orden_2) OR
                 (z_reg.orden_7 = z_reg.orden_3) OR
                 (z_reg.orden_7 = z_reg.orden_4) OR
                 (z_reg.orden_7 = z_reg.orden_5) OR
                 (z_reg.orden_7 = z_reg.orden_6) THEN
                 ERROR "Opcion ya digitada ...Reintente "
                 NEXT FIELD orden_7
               END IF
           END IF

           IF z_reg.orden_8 IS NULL THEN
              ERROR "Campo NO puede ser NULO"
              NEXT FIELD orden_8
           ELSE
              IF z_reg.orden_8 < 1 OR z_reg.orden_8 > 9  THEN
                 ERROR "La opcion de orden digitada no existe...Reingrese"
                 NEXT FIELD orden_8
              END IF

              IF (z_reg.orden_8 = z_reg.orden_1) OR
                 (z_reg.orden_8 = z_reg.orden_2) OR
                 (z_reg.orden_8 = z_reg.orden_3) OR
                 (z_reg.orden_8 = z_reg.orden_4) OR
                 (z_reg.orden_8 = z_reg.orden_5) OR
                 (z_reg.orden_8 = z_reg.orden_6) OR
                 (z_reg.orden_8 = z_reg.orden_7) THEN
                 ERROR "Opcion ya digitada ...Reintente "
                 NEXT FIELD orden_8
              END IF
           END IF

           IF z_reg.orden_9 IS NULL THEN
              ERROR "Campo NO puede ser NULO"
              NEXT FIELD orden_9
           ELSE
              IF z_reg.orden_9 < 1 OR z_reg.orden_9 > 9  THEN
                 ERROR "La opcion de orden digitada no existe...Reingrese"
                 NEXT FIELD orden_9
              END IF

              IF (z_reg.orden_9 = z_reg.orden_1) OR
                 (z_reg.orden_9 = z_reg.orden_2) OR
                 (z_reg.orden_9 = z_reg.orden_3) OR
                 (z_reg.orden_9 = z_reg.orden_4) OR
                 (z_reg.orden_9 = z_reg.orden_5) OR
                 (z_reg.orden_9 = z_reg.orden_6) OR
                 (z_reg.orden_9 = z_reg.orden_7) OR
                 (z_reg.orden_9 = z_reg.orden_8) THEN
                 ERROR "Opcion ya digitada ...Reintente "
                 NEXT FIELD orden_9
              END IF
           END IF

           CASE opcion_menu
               WHEN "ELIMINA"
                   LET z_reg2[1] = z_reg.orden_1
                   LET z_reg2[2] = z_reg.orden_2
                   LET z_reg2[3] = z_reg.orden_3
                   LET z_reg2[4] = z_reg.orden_4
                   LET z_reg2[5] = z_reg.orden_5
                   LET z_reg2[6] = z_reg.orden_6
                   LET z_reg2[7] = z_reg.orden_7
                   LET z_reg2[8] = z_reg.orden_8
                   LET z_reg2[9] = z_reg.orden_9
                   FOR i = 1 TO 9
                       CASE z_reg2[i]
                           WHEN 1
                               LET z_reg2[i] = 1
                           WHEN 2
                               LET z_reg2[i] = 4
                           WHEN 3
                               LET z_reg2[i] = 7
                           WHEN 4
                               LET z_reg2[i] = 6
                           WHEN 5
                               LET z_reg2[i] = 8
                       END CASE
                   END FOR
                   EXIT INPUT
               WHEN "IMPRIME"
                   EXIT INPUT
               OTHERWISE
                   EXIT INPUT
           END CASE
       ON KEY ( INTERRUPT )
           LET sw_4 = 0
           EXIT INPUT
    END INPUT

    CLOSE WINDOW ventana_3

    RETURN

END FUNCTION

REPORT listado_1(reg)
#l1------------------

    DEFINE reg RECORD
      tipo_solicitud   SMALLINT,
      n_folio         DECIMAL(10,0)  ,
      cod_promotor     CHAR(10) ,
      fecha_solicitud  DATE     ,
      fecha_recepcion  DATE     ,
      n_seguro         CHAR(11) ,
      estado_sol       CHAR(4)  ,
      estado_exp       SMALLINT ,
      no_referencia    CHAR(10) ,
      curp             CHAR(18) ,
      lote_cap         CHAR(10) ,
      fecha_actualiza  DATE
    END RECORD

    DEFINE g_nomb RECORD
        paterno   LIKE afi_mae_afiliado.paterno,
        materno   LIKE afi_mae_afiliado.materno,
        nombres   LIKE afi_mae_afiliado.nombres
    END RECORD

    DEFINE g_vend RECORD
        paterno       CHAR(40),
        materno       CHAR(40),
        nombres       CHAR(40)
    END RECORD
    DEFINE 
        razon_social  CHAR(040) ,
        nombres       CHAR(120) ,
        nombrev       CHAR(120) ,
        c85_titulo    CHAR(085)

    DEFINE
        nivel1        SMALLINT

    OUTPUT
        PAGE LENGTH 90
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
        LET c85_titulo = "      L I S T A D O   D E   R E C E P C I O N   D E   A F I L I A C I O N E S        "

        SELECT A.razon_social
        INTO   razon_social
        FROM   tab_afore_local A
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 001,"========================================",
            COLUMN 040,"========================================",
            COLUMN 080,"========================================",
            COLUMN 120,"========================================",
            COLUMN 160,"================="
        PRINT
            COLUMN 001,razon_social,
            COLUMN 150,"FECHA   :",hoy USING "DD/MM/YY"," ",HORA
        PRINT
            COLUMN 001,"AFIM0071",
            COLUMN 045,c85_titulo,
            COLUMN 150,"NUM.PAG.:",pageno USING "####,###"
            SKIP 1 LINE
        PRINT
            COLUMN 001,"----------------------------------------",
            COLUMN 040,"----------------------------------------",
            COLUMN 080,"----------------------------------------",
            COLUMN 120,"----------------------------------------",
            COLUMN 160,"-----------------"
        PRINT
            COLUMN   7,"FECHA DE" ,
            COLUMN  19,"TIPO DE " ,
            COLUMN  29,"NO. DE  " ,
            COLUMN  97,"CLAVE DE" 
        PRINT
            COLUMN 01,"No."       ,
            COLUMN 07,"RECEPCION" ,
            COLUMN 18,"SOLICITUD" ,
            COLUMN 29,"FOLIO "    ,
            COLUMN 39,"NOMBRE DEL AFILIADO",
            COLUMN 77,"N.S.S. "   ,
            COLUMN 91,"T.P. "     ,
            COLUMN 97,"PROMOTOR"  ,  
            COLUMN 110,"NOMBRE DEL PROMOTOR"  
        PRINT
            COLUMN 001,"========================================",
            COLUMN 040,"========================================",
            COLUMN 080,"========================================",
            COLUMN 120,"========================================",
            COLUMN 160,"================="
    ON EVERY ROW
       LET nombres        = ""
       LET g_nomb.paterno = ""
       LET g_nomb.materno = ""
       LET g_nomb.nombres = ""

    SELECT A.paterno ,
           A.materno ,
           A.nombres
    INTO   g_nomb.*
    FROM   afi_solicitud A
    --WHERE  cod_promotor  = reg.cod_promotor
    WHERE  n_folio = reg.n_folio
    AND    tipo_solicitud = tipo_solic

    LET nombres = g_nomb.paterno CLIPPED," ",
                  g_nomb.materno CLIPPED," ",
                  g_nomb.nombres CLIPPED

    SELECT c.nivel, c.paterno, c.materno, c.nombres
    INTO   nivel1, g_vend.*
    FROM   pro_mae_promotor c
    WHERE  c.cod_promotor = reg.cod_promotor

    LET nombrev = g_vend.paterno CLIPPED," ",
                  g_vend.materno CLIPPED," ",
                  g_vend.nombres CLIPPED

    LET contador_1 = contador_1 + 1

#           1         2         3         4         5         6         7
#  12345678901234567890123456789012345678901234567890123456789012345678901
#  ###  99/99/99 ########  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxxxxxxxxx
#  no.  f.recep  folio     nombre afiliado                    fecha_solic.

#          8         9         10        11        12        13        14
#  2345678901234567890123456789012345678901234567890123456789012345678901
#    xxxxxxxxxxxxxxx  xxx  xxxxxxxxxx  ----- ----- ######### ----- ------

#          15        16        17        18
#  2345678901234567890123456789012345678901234567890123456789012345678901
#  --------------- --------------------

   PRINT
   PRINT
      COLUMN 01,contador_1            USING "###"      ,
      COLUMN 07,reg.fecha_recepcion   USING "dd/mm/yy" ,
      COLUMN 21,tipo_solic            USING "#"        ,
      COLUMN 25,reg.n_folio           USING "########" ,
      COLUMN 35,nombres [1,35]                         ,
      COLUMN 78,reg.n_seguro                           ,
      COLUMN 90,nivel1                USING "###"      ,
      COLUMN 96,reg.cod_promotor      USING "&&&&&&&&&&",
      COLUMN 110,nombrev[1,40]
   PRINT

   ON LAST ROW
      skip 3 lines
   PRINT
      COLUMN 01,"Total solicitudes : ",contador_1  USING "###"
END REPORT

REPORT listado_3(reg)
#l3------------------

    DEFINE reg  RECORD
      n_folio         DECIMAL(10,0)  ,
      curp             CHAR(18) ,
      n_seguro         CHAR(11) ,
      estado_sol       CHAR(4)  ,
      estado_exp       SMALLINT ,
      cod_promotor     CHAR(10) ,
      no_referencia    CHAR(10) ,
      fecha_solicitud  DATE     ,
      fecha_recepcion  DATE
    END RECORD

    OUTPUT
        PAGE LENGTH 90
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT
            COLUMN 01,reg.fecha_recepcion	USING "dd/mm/yy" ,"|",
                      reg.n_folio USING "########" ,"|"

END REPORT

FUNCTION ven_solicitud()
#vs---------------------

    DEFINE arr_solicitud ARRAY[10] OF RECORD
        numero  SMALLINT,
        descri  CHAR(30)
    END RECORD

    DEFINE
        arr_ret ,
        cont    SMALLINT

    DECLARE apt_cur CURSOR FOR
    SELECT a.tipo_solicitud, a.desc_solicitud
    FROM   tab_tipo_solic a
    ORDER BY 1

    LET cont=1
    FOREACH apt_cur INTO arr_solicitud[cont].*
        LET cont = cont + 1
    END FOREACH

    OPEN WINDOW ventana_solicitud AT 7,10 WITH FORM "AFIM0079" ATTRIBUTE(BORDER)
        DISPLAY "TIPOS DE SOLICITUD" AT 1,14
        CALL SET_COUNT(cont-1)
        DISPLAY ARRAY arr_solicitud TO scr_solicitud.*
            ON KEY ( RETURN )
                LET cont = ARR_CURR()
                EXIT DISPLAY
            ON KEY ( INTERRUPT )
                EXIT DISPLAY
        END DISPLAY

    CLOSE WINDOW ventana_solicitud

    RETURN arr_solicitud[cont].numero, arr_solicitud[cont].descri 

END FUNCTION

FUNCTION despliegue()
#D-------------------

    DEFINE
        txt         CHAR(300) ,
        n_busqueda  CHAR(50)  ,
        pat         CHAR(50)  ,
        mat         CHAR(50)  ,
        nom         CHAR(50)

    DEFINE l_reg ARRAY[4000] OF RECORD
        cod_promotor	 CHAR(10) ,
        nombre  CHAR(50)
    END RECORD

    DEFINE
        num_registro ,
        HACER        SMALLINT

    DEFINE sx_reg ARRAY[4000] OF RECORD
        paterno CHAR(40) ,
        materno CHAR(40) ,
        nombres CHAR(40)
    END RECORD

    OPEN WINDOW ventana_4 AT  7,10 WITH FORM "AFIM0074" ATTRIBUTE(BORDER)
    DISPLAY "          PUEDE UTILIZAR EL * (ASTERISCO) COMO COMODIN                         " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "                PARA BUSCAR POR APELLIDO PATERNO                               " AT 2,1 ATTRIBUTE(REVERSE)

    LET HACER = TRUE

    INPUT BY NAME n_busqueda
        AFTER FIELD n_busqueda
        IF  n_busqueda IS NULL THEN
           NEXT FIELD n_busqueda
        END IF

        LET txt = NULL
        LET txt = "SELECT cod_promotor,",
                  "paterno,",
                  "materno,",
                  "nombres ",
                  "FROM pro_mae_promotor ",
                  "WHERE paterno MATCHES ",'"',n_busqueda CLIPPED,
                  '"' CLIPPED," ORDER BY 2,3"

        EXIT INPUT

        ON KEY ( INTERRUPT )
            DISPLAY "                         " AT 5,60
            LET HACER = FALSE
            EXIT INPUT

    END INPUT

    IF HACER THEN
       PREPARE cur1 FROM txt
       DECLARE cursor_1 CURSOR FOR cur1
       LET num_registro = 1

       FOREACH cursor_1 INTO l_reg[num_registro].cod_promotor ,
                             pat,
                             mat,
                             nom

           LET l_reg[num_registro].nombre   = pat CLIPPED," ",
                                              mat CLIPPED," ",
                                              nom CLIPPED

           LET sx_reg[num_registro].paterno = pat
           LET sx_reg[num_registro].materno = mat
           LET sx_reg[num_registro].nombres = nom

           IF num_registro = 4000 THEN
              ERROR "Capacidad del Arreglo fue Sobrepasada"
              EXIT FOREACH
           END IF

           LET num_registro = num_registro + 1
       END FOREACH

       CALL SET_COUNT(num_registro-1)

       DISPLAY ARRAY l_reg TO scr_1.*
           ON KEY ( CONTROL-M )
               LET num_registro = ARR_CURR()
               LET aux_cod_promotor = l_reg[num_registro].cod_promotor
               EXIT DISPLAY
           ON KEY ( INTERRUPT )
               DISPLAY "                         " AT 5,60
               EXIT DISPLAY
       END DISPLAY
   END IF

   CLOSE WINDOW ventana_4

END FUNCTION

FUNCTION busca_usuario()
#bu---------------------

    LET usuario = g_usuario

    SELECT usuario_desc
    INTO   nom_analista
    FROM   seg_usuario
    WHERE  seg_usuario.usuario_cod = usuario

    DISPLAY BY NAME usuario, nom_analista

END FUNCTION

FUNCTION cal_fecha_avant(x_fecha,ciclo)
#cf------------------------------------

    DEFINE cc         SMALLINT
    DEFINE dia_semana SMALLINT
    DEFINE ant_habil  SMALLINT
    DEFINE ciclo      SMALLINT

    DEFINE x_fecha    DATE
    DEFINE sig_fecha  DATE

    LET sig_fecha  = x_fecha
    LET cc = 1

    WHILE cc <= ciclo 
        LET sig_fecha  = sig_fecha + 1

        LET dia_semana = WEEKDAY(sig_fecha)

        IF dia_semana = 0 OR dia_semana = 6 THEN
           CONTINUE WHILE
        ELSE
           SELECT "x"
           FROM   tab_feriado
           WHERE  feria_fecha = sig_fecha

           IF STATUS <> NOTFOUND THEN
              CONTINUE WHILE
           ELSE
              LET cc = cc + 1
           END IF
        END IF
    END WHILE

    RETURN sig_fecha

END FUNCTION

############################################################################
FUNCTION busca_tipo_sol()
#bt----------------------

    SELECT desc_solicitud
    INTO   desc_solic
    FROM   tab_tipo_solic
    WHERE  tipo_solicitud = tipo_solic

    DISPLAY BY NAME tipo_solic, desc_solic

END FUNCTION
