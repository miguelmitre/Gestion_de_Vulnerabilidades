--************************************************************************
--***********
--***********
--***********
--***********
--***********
--***********
--************************************************************************

DATABASE safre_tmp

DEFINE g_mensaje1           ,
       g_mensaje2           ,
       g_mensaje3           CHAR(100),
       g_usuario            CHAR(8)  ,
       g_respuesta          CHAR(001)
DEFINE g_cancela           SMALLINT
DEFINE g_fecha_corte       DATE

MAIN
   DEFER INTERRUPT
   CALL fn_inicia_variables()
   CALL proceso_principal()
END MAIN

FUNCTION fn_inicia_variables()

SELECT USER
  INTO g_usuario
  FROM systables
 WHERE tabid = 1

END FUNCTION

FUNCTION fn_inicializa()

    DEFINE v_paso  SMALLINT

    LET v_paso = 1

    LET g_mensaje1 = "ESTE PROCESO ELIMINARA LA INFORMACION PARA LA GENERACION",
                     " DEL ULTIMO FORMATO GENERADO."
    LET g_mensaje2 = "INDIQUE LA FECHA DEL PERIODO:"
    LET g_mensaje3 = "ESTA SEGURO DE CONTINUAR EJECUCION S/N:"

    LET g_cancela = FALSE

    INPUT BY NAME g_fecha_corte,
                  g_respuesta

        BEFORE INPUT
            DISPLAY BY NAME g_mensaje1,
                            g_mensaje2

        BEFORE FIELD g_fecha_corte
            DISPLAY "" TO formonly.g_mensaje3
            DISPLAY "" TO formonly.g_respuesta

        AFTER FIELD g_fecha_corte
            IF g_fecha_corte IS NULL THEN
                ERROR "DEBE INDICAR FECHA DE CORTE" 
                SLEEP 3
                ERROR ""
                NEXT FIELD g_fecha_corte
            ELSE
                IF fn_valida_proceso(g_fecha_corte,v_paso) <> 0 THEN
                    NEXT FIELD g_fecha_corte
                END IF
            END IF

        BEFORE FIELD g_respuesta 
            DISPLAY BY NAME g_mensaje3

        AFTER FIELD g_respuesta 
            IF g_respuesta <> "S" AND
               g_respuesta <> "N" THEN
               ERROR "SOLO INDIQUE S o N "
               SLEEP 3
               ERROR ""
               NEXT FIELD g_respuesta
            ELSE
                IF g_respuesta = "N" THEN
                    ERROR "PROCESO CANCELADO."
                    SLEEP 3
                    ERROR ""
                    LET g_cancela = TRUE
                    EXIT INPUT
                ELSE
                    CALL fn_inserta_paso ( v_paso      ,
                                           g_fecha_corte
                                         )
                    IF fn_borra_informacion() = 0 THEN
                        CALL fn_finaliza_paso ( v_paso      ,
                                                g_fecha_corte
                                              )
                        ERROR "INICIALIZACION EFECTUADA."
                        SLEEP 3
                        ERROR ""
                        EXIT INPUT
                    ELSE
                        CALL fn_elimina_paso ( v_paso ,
                                               g_fecha_corte
                                             )
                        ERROR "NO SE PUDO INICIALIZAR, AVISE AL ADMINISTRADOR."
                        SLEEP 3
                        ERROR ""
                        EXIT INPUT
                    END IF
                END IF
            END IF

        ON KEY (CONTROL-C,INTERRUPT)
            ERROR "PROCESO CANCELADO."
            SLEEP 3
            ERROR ""
            LET g_cancela = TRUE
            LET int_flag  = FALSE
            EXIT INPUT
    END INPUT

END FUNCTION

FUNCTION proceso_principal()

    DEFINE hoy   DATE

    LET hoy = TODAY

    OPEN WINDOW  w_menu AT 4,4 WITH FORM "CTAMFMTOA1" ATTRIBUTES(BORDER)
    DISPLAY " CTAMFMTOA              GENERACION DE FORMATO A                                " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)

    MENU "Formato A"
         COMMAND "Inicializa" "Limpia estructura, elimina informacion anterior."
            CALL fn_inicializa()
            CLEAR FORM
         COMMAND "Identifica" "Identifica cuota en base a edad."
            CALL fn_identifica_nss()
            CLEAR FORM
         COMMAND "Obtiene saldos" "Ejecuta por Nohup Obtencion de saldos."
            CALL fn_genera_saldo()
            CLEAR FORM
         COMMAND "Genera formato" "Ejecuta por Nohup Formato-A "
            CALL fn_genera_formato()
            CLEAR FORM
         COMMAND "Revisa ejecucion" "Despliega la ejecucion de los procesos."
            CALL fn_revisa_proceso()
            CLEAR FORM
         COMMAND "Salir" "Salir del Programa"
            EXIT MENU
    END MENU
END FUNCTION

FUNCTION fn_borra_informacion()

    DEFINE v_error    INTEGER
    DEFINE v_comando  CHAR(150)

    LET v_error = 0

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_saldo_corte
        DROP TABLE cta_formato_nss
        DROP TABLE tmp_regpas1
    WHENEVER ERROR STOP

    SQL
        CREATE TABLE tmp_saldo_corte
            (
              nss                CHAR(11)     ,
              subcuenta          SMALLINT     ,
              siefore            SMALLINT     ,
              fecha_conversion   DATE         ,
              monto_en_acciones  DECIMAL(22,6),
              monto_en_pesos     DECIMAL(22,6) 
             )  in tmp_dbs1;
    END SQL

    SQL
        CREATE TABLE cta_formato_nss
            (
              nss              CHAR(11),
              tipo_solicitud   SMALLINT,
              ind_edad         SMALLINT,
              fecha_corte      DATE    ,
              fecha_proceso    DATE    ,
              usuario          CHAR(12) 
            )
    END SQL

    IF v_error <> 0 THEN
        LET v_error = -1
    END IF

    RETURN v_error
END FUNCTION

FUNCTION fn_valida_proceso ( p_fecha_corte, 
                             p_paso
                           )

    DEFINE p_paso         ,
           v_paso         ,
           v_paso_ant     ,
           v_status       SMALLINT
    DEFINE p_fecha_corte  ,
           v_fecha_fin    DATE

    LET v_paso      = NULL
    LET v_fecha_fin = NULL
    LET v_paso_ant  = p_paso - 1 
    LET v_status    = 0

    IF v_paso_ant <> 0 THEN --  Verifica ejecucion del paso anterior
       SELECT paso    ,
              fecha_fin
         INTO v_paso     ,
              v_fecha_fin 
         FROM cta_ctr_fmto_a
        WHERE fecha_corte = p_fecha_corte
          AND paso        = v_paso_ant

        IF v_paso IS NULL OR
           v_paso = 0 THEN
            ERROR "FALTA EJECUCION DEL PASO ANTERIOR. VERIFIQUE"
            SLEEP 3
            ERROR "" 
            LET v_status = 1
        ELSE
            IF v_fecha_fin IS NULL OR
               v_fecha_fin = "12/31/1899" THEN
                ERROR "PROCESO ANTERIOR AUN NO FINALIZA."
                SLEEP 3
                ERROR "" 
                LET v_status = 1
            END IF
        END IF
    END IF

    IF v_status = 0 THEN
        LET v_paso      = NULL
        LET v_fecha_fin = NULL

   ---  Verifica si ya se ha ejecutado el proceso anteriormente
        SELECT paso     ,
               fecha_fin 
          INTO v_paso     ,
               v_fecha_fin 
          FROM cta_ctr_fmto_a
         WHERE fecha_corte = p_fecha_corte
           AND paso        = p_paso

        IF v_paso = p_paso  THEN
            IF v_fecha_fin IS NULL OR
               v_fecha_fin = "12/31/1899" THEN
                ERROR "PROCESO YA EN EJECUCION, PARA ESTA FECHA."
                SLEEP 3
                ERROR "" 
                LET v_status = 1
            ELSE
                ERROR "PROCESO YA EJECUTADO, PARA ESTA FECHA."
                SLEEP 3
                ERROR "" 
                LET v_status = 1
            END IF
        END IF
    END IF

    RETURN v_status
END FUNCTION

FUNCTION fn_inserta_paso   ( p_paso, 
                             p_fecha_corte
                           )

    DEFINE p_paso        SMALLINT
    DEFINE p_fecha_corte DATE

    INSERT INTO cta_ctr_fmto_a  VALUES ( p_fecha_corte,
                                         p_paso       ,
                                         CURRENT      ,
                                         NULL         ,
                                         USER         ,
                                         TODAY        
                                       )
END FUNCTION

FUNCTION fn_finaliza_paso  ( p_paso, 
                             p_fecha_corte
                           )

    DEFINE p_paso        SMALLINT
    DEFINE p_fecha_corte DATE

    UPDATE cta_ctr_fmto_a 
       SET fecha_fin = CURRENT,
           usuario   = USER
     WHERE fecha_corte = p_fecha_corte
       AND paso        = p_paso

END FUNCTION

FUNCTION fn_elimina_paso  ( p_paso, 
                            p_fecha_corte
                           )

    DEFINE p_paso        SMALLINT
    DEFINE p_fecha_corte DATE

    DELETE
      FROM cta_ctr_fmto_a 
     WHERE fecha_corte = p_fecha_corte
       AND paso        = p_paso
END FUNCTION

FUNCTION fn_identifica_nss()

    DEFINE v_paso  SMALLINT

    LET v_paso = 2

    LET g_mensaje1 = "ESTE PROCESO IDENTFICA NSS POR SU EDAD "
    LET g_mensaje2 = "INDIQUE LA FECHA DEL PERIODO:"
    LET g_mensaje3 = "ESTA SEGURO DE CONTINUAR EJECUCION S/N:"

    LET g_cancela = FALSE

    INPUT BY NAME g_fecha_corte,
                  g_respuesta

        BEFORE INPUT 
            DISPLAY BY NAME g_mensaje1,
                            g_mensaje2

        BEFORE FIELD g_fecha_corte
            DISPLAY "" TO formonly.g_mensaje3
            DISPLAY "" TO formonly.g_respuesta

        AFTER FIELD g_fecha_corte
            IF g_fecha_corte IS NULL THEN
                ERROR "DEBE INDICAR FECHA DE CORTE" 
                SLEEP 3
                ERROR ""
                NEXT FIELD g_fecha_corte
            ELSE
                IF fn_valida_proceso(g_fecha_corte,v_paso) <> 0 THEN
                    NEXT FIELD g_fecha_corte
                END IF
            END IF

        BEFORE FIELD g_respuesta 
             DISPLAY BY NAME g_mensaje3

        AFTER FIELD g_respuesta 
            IF g_respuesta <> "S" AND
               g_respuesta <> "N" THEN
                ERROR "SOLO INDIQUE S o N "
                SLEEP 3
                ERROR ""
                NEXT FIELD g_respuesta
            ELSE
                IF g_respuesta = "N" THEN
                    ERROR "PROCESO CANCELADO."
                    SLEEP 3
                    ERROR ""
                    LET g_cancela = TRUE
                    EXIT INPUT
               ELSE
                  CALL fn_inserta_paso ( v_paso ,
                                         g_fecha_corte
                                       )
                  CALL fn_ejecuta(g_fecha_corte,1) 
                  ERROR "EJECUTANDO PROCESO POR NOHUP."
                  SLEEP 3
                  ERROR ""
                  EXIT INPUT
               END IF
            END IF

        ON KEY (CONTROL-C,INTERRUPT)
            ERROR "PROCESO CANCELADO."
            SLEEP 3
            ERROR ""
            LET g_cancela = TRUE
            LET int_flag  = FALSE
            EXIT INPUT
    END INPUT
END FUNCTION

FUNCTION fn_genera_saldo()

    DEFINE v_paso  SMALLINT

    LET v_paso = 3

    LET g_mensaje1 = "ESTE PROCESO OBTIENE LOS SALDOS, A LA ",
                     "FECHA DEL PERIODO "
    LET g_mensaje2 = "INDIQUE LA FECHA DEL PERIODO:"
    LET g_mensaje3 = "ESTA SEGURO DE CONTINUAR EJECUCION S/N:"

    LET g_cancela = FALSE

    INPUT BY NAME g_fecha_corte,
                  g_respuesta

        BEFORE INPUT 
            DISPLAY BY NAME g_mensaje1,
                            g_mensaje2

        BEFORE FIELD g_fecha_corte
            DISPLAY "" TO formonly.g_mensaje3
            DISPLAY "" TO formonly.g_respuesta

        AFTER FIELD g_fecha_corte
            IF g_fecha_corte IS NULL THEN
                ERROR "DEBE INDICAR FECHA DE CORTE" 
                SLEEP 3
                ERROR ""
                NEXT FIELD g_fecha_corte
            ELSE
                IF fn_valida_proceso(g_fecha_corte,v_paso) <> 0 THEN
                    NEXT FIELD g_fecha_corte
                END IF
            END IF

        BEFORE FIELD g_respuesta 
            DISPLAY BY NAME g_mensaje3

        AFTER FIELD g_respuesta 
            IF g_respuesta <> "S" AND
               g_respuesta <> "N" THEN
                ERROR "SOLO INDIQUE S o N "
                SLEEP 3
                ERROR ""
                NEXT FIELD g_respuesta
            ELSE
                IF g_respuesta = "N" THEN
                    ERROR "PROCESO CANCELADO."
                    SLEEP 3
                    ERROR ""
                    LET g_cancela = TRUE
                    EXIT INPUT
                ELSE
                    CALL fn_inserta_paso ( v_paso ,
                                           g_fecha_corte
                                          )
                    CALL fn_ejecuta(g_fecha_corte,2) 
                    ERROR "EJECUTANDO PROCESO POR NOHUP."
                    SLEEP 3
                    ERROR ""
                    EXIT INPUT
                END IF
            END IF

        ON KEY (CONTROL-C,INTERRUPT)
            ERROR "PROCESO CANCELADO."
            SLEEP 3
            ERROR ""
            LET g_cancela = TRUE
            LET int_flag  = FALSE
            EXIT INPUT

    END INPUT
END FUNCTION

FUNCTION fn_revisa_proceso()

    LET g_mensaje1 = "ESTA OPCION MUESTRA EL ESTADO DE EJECUCION DE LOS PASOS."
    LET g_mensaje2 = "INDIQUE LA FECHA DEL PERIODO:"

    LET g_cancela = FALSE

    INPUT BY NAME g_fecha_corte
        BEFORE INPUT 
            DISPLAY BY NAME g_mensaje1,
                            g_mensaje2

        BEFORE FIELD g_fecha_corte
            DISPLAY "" TO formonly.g_mensaje3
            DISPLAY "" TO formonly.g_respuesta

        AFTER FIELD g_fecha_corte
            IF g_fecha_corte IS NULL THEN
                ERROR "DEBE INDICAR FECHA DE CORTE" 
                SLEEP 3
                ERROR ""
                NEXT FIELD g_fecha_corte
            ELSE
                IF fn_despliega_pasos(g_fecha_corte) = 0 THEN
                    ERROR "NO EXISTE EJECUCION PARA LA FECHA."
                    SLEEP 3
                    ERROR ""
                    NEXT FIELD g_fecha_corte
                END IF
            END IF

        ON KEY (CONTROL-C,INTERRUPT)
            ERROR "PROCESO CANCELADO."
            SLEEP 3
            ERROR ""
            LET g_cancela = TRUE
            LET int_flag  = FALSE
            EXIT INPUT

    END INPUT
END FUNCTION

FUNCTION fn_despliega_pasos(l_fecha_corte)

    DEFINE l_fecha_corte   DATE
    DEFINE pos,v_status    SMALLINT

    DEFINE arr_fmtoa ARRAY[4] OF RECORD
           desc_paso        CHAR(12),
           fecha_ini        DATETIME YEAR TO SECOND,
           fecha_fin        DATETIME YEAR TO SECOND,
           usuario          CHAR(8)
       END RECORD

    OPEN WINDOW  w_submenu AT 6,4 WITH FORM "CTAMFMTOA2" ATTRIBUTES(BORDER)
    DISPLAY " CTAMFMTOA              GENERACION DE FORMATO A        < Ctrl-C > SALIR          " AT 1,1 ATTRIBUTE(REVERSE)

    DECLARE cur_fa CURSOR FOR
        SELECT DECODE(paso,1,"INICIALIZA  ",
                           2,"IDENTIFICA  ",
                           3,"SALDOS      ",
                           4,"GENERA FMTO.") desc_paso,
               fecha_ini,
               fecha_fin,
               usuario
          FROM cta_ctr_fmto_a
         WHERE fecha_corte = l_fecha_corte
         ORDER BY fecha_ini

    LET pos = 1

    FOREACH cur_fa INTO arr_fmtoa[pos].*
        LET pos = pos + 1

        IF pos >= 5 THEN
            EXIT FOREACH
        END IF
    END FOREACH

    CLOSE cur_fa
    FREE  cur_fa

    IF (pos-1) < 1 THEN
        LET v_status = 0
    ELSE
        LET v_status = 1
        CALL SET_COUNT(pos-1)

        DISPLAY l_fecha_corte TO fecha_corte
        DISPLAY ARRAY arr_fmtoa TO scr_1.*

            ON KEY ( INTERRUPT )
                LET pos = 0
                EXIT DISPLAY

        END DISPLAY
    END IF

    CLOSE WINDOW w_submenu

    RETURN v_status
END FUNCTION

FUNCTION fn_genera_formato()

    DEFINE v_paso  SMALLINT

    LET v_paso = 4

    LET g_mensaje1 = "ESTE PROCESO GENERA EL FORMATO A  EN BASE ",
                     " A LA INFORMACION OBTENIDA."
    LET g_mensaje2 = "INDIQUE LA FECHA DEL PERIODO:"
    LET g_mensaje3 = "ESTA SEGURO DE CONTINUAR EJECUCION S/N:"

    LET g_cancela = FALSE

    INPUT BY NAME g_fecha_corte,
                  g_respuesta

        BEFORE INPUT 
            DISPLAY BY NAME g_mensaje1,
                            g_mensaje2

        BEFORE FIELD g_fecha_corte
            DISPLAY "" TO formonly.g_mensaje3
            DISPLAY "" TO formonly.g_respuesta

        AFTER FIELD g_fecha_corte
            IF g_fecha_corte IS NULL THEN
                ERROR "DEBE INDICAR FECHA DE CORTE" 
                SLEEP 3
                ERROR ""
                NEXT FIELD g_fecha_corte
            ELSE
                IF fn_valida_proceso(g_fecha_corte,v_paso) <> 0 THEN
                   NEXT FIELD g_fecha_corte
                END IF
            END IF

        BEFORE FIELD g_respuesta 
            DISPLAY BY NAME g_mensaje3
        AFTER FIELD g_respuesta 
            IF g_respuesta <> "S" AND
               g_respuesta <> "N" THEN
                ERROR "SOLO INDIQUE S o N "
                SLEEP 3
                ERROR ""
                NEXT FIELD g_respuesta
            ELSE
                IF g_respuesta = "N" THEN
                    ERROR "PROCESO CANCELADO."
                    SLEEP 3
                    ERROR ""
                    LET g_cancela = TRUE
                    EXIT INPUT
                ELSE
                    CALL fn_inserta_paso ( v_paso ,
                                           g_fecha_corte
                                          )
                    CALL fn_ejecuta(g_fecha_corte,3) 
                    ERROR "EJECUTANDO PROCESO POR NOHUP."
                    SLEEP 3
                    ERROR ""
                    EXIT INPUT
                END IF
            END IF

        ON KEY (CONTROL-C,INTERRUPT)
            ERROR "PROCESO CANCELADO."
            SLEEP 3
            ERROR ""
            LET g_cancela = TRUE
            LET int_flag  = FALSE
            EXIT INPUT
    END INPUT
END FUNCTION

FUNCTION fn_ejecuta( p_fecha_corte, p_tipo )

    DEFINE p_fecha_corte DATE
    DEFINE p_tipo        SMALLINT
    DEFINE v_comando     CHAR(200)

    CASE p_tipo
        WHEN 1
              LET v_comando = "nohup time ./eje_cuota_edad.sh ",p_fecha_corte," ",
                               g_usuario CLIPPED," 1> ",g_usuario CLIPPED,".salida ",
                              "2> ",g_usuario CLIPPED,".error &"
        WHEN 2
              LET v_comando = "nohup time ./eje_saldo_corte.sh ",p_fecha_corte," ",
                               g_usuario CLIPPED," 1> ",g_usuario CLIPPED,".salida ",
                              "2> ",g_usuario CLIPPED,".error &"
        WHEN 3
              LET v_comando = "nohup time fglgo CTALFMTOA ",p_fecha_corte," ", 
                              " 1> ",g_usuario CLIPPED,".salida ",
                              "2> ",g_usuario CLIPPED,".error &"
    END CASE

    RUN v_comando
END FUNCTION 
