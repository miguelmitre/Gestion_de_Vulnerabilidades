{============================================================================}
{ Programa    : CTAB111.4gl                                                  }
{ Modulo      :                                                              }
{ Descripcion : Identificar afiliados mayores a 56 y cambiar de regimen      }
{ Elaboro     : Isai Jiméz Rojas Abril-2005                                }
{ Recibe      : Fecha con la que se realizara la evaluacion                  }
{ Regresa     : Codigo de ejecucion: 0 = Exito, -1 = error                   }
{ Ult Modific.: 03/Junio/2005                                                }
{             : Eliminacion de mensajes a pantalla                           }
{============================================================================}

DATABASE safre_af

MAIN

   DEFINE v_n_seguro       LIKE afi_mae_afiliado.n_seguro
   DEFINE v_fecha_corte    DATE
   DEFINE v_existencia     SMALLINT
   DEFINE v_edad           SMALLINT
   DEFINE v_criterio       SMALLINT
   DEFINE v_status         SMALLINT
   DEFINE v_mensaje        CHAR(80)

   DEFINE g_fecha_corte    DATE
   DEFINE v_decision       CHAR(1)
   DEFINE HOY              DATE
   DEFINE fecha_inicio     DATETIME HOUR TO SECOND
   DEFINE fecha_fin        DATETIME HOUR TO SECOND
   DEFINE fecha_fin2       DATE
   DEFINE instrucc         CHAR(110)
   DEFINE g_proceso_cod    SMALLINT
   DEFINE var_usr          CHAR(8)

   OPTIONS PROMPT LINE LAST -1

    --INICIALIZACION DE BITACORA

   CALL STARTLOG("CTAB111.log")

    RUN "chmod 777 CTAB111.log"

    CALL errorlog("============================================================")

   LET HOY           = TODAY
   LET fecha_inicio  = CURRENT
   LET fecha_fin     = NULL

   PREPARE ex1 FROM "EXECUTE FUNCTION fn_edad(?,?)"
   PREPARE ex2 FROM "EXECUTE PROCEDURE sp_guarda_regimen(?)"
   PREPARE ex3 FROM "EXECUTE FUNCTION fn_ind_transferencia(?,?,?)"
   PREPARE ex4 FROM "EXECUTE PROCEDURE sp_crea_regimen(?,?,?)"
   PREPARE ex5 FROM "INSERT INTO cta_his_nss_decimo VALUES(?,?,?,?,?)"

   SELECT USER
     INTO var_usr
     FROM systables
    WHERE tabid=1;

   --RECEPCION DEL PARAMETRO FECHA

   LET v_fecha_corte = ARG_VAL(1)

   IF v_fecha_corte IS NULL OR v_fecha_corte = "" THEN
      EXIT PROGRAM -1
   END IF

   { INTEGRAR VALIDACION CON cta_ctr_decimo }
---ermini VALIDA QUE NO EXISTA UN REGISTRO EN cta_ctr_decimo PARA ESTA FECHA
   LET instrucc = "SELECT fecha_corte,proceso_cod ",
                  "FROM cta_ctr_decimo ",
                  "WHERE proceso_cod = 1 ",
                  "AND fecha_corte = ","'",v_fecha_corte,"'"

   PREPARE segunda_ins FROM instrucc
   EXECUTE segunda_ins INTO g_fecha_corte,g_proceso_cod

   IF SQLCA.SQLCODE = 0 THEN
      DISPLAY "PROCESO CANCELADO, YA EXISTE UN PROCESO PARA ESTA FECHA, <ENTER> PARA SALIR: ", v_decision
      EXIT PROGRAM -1
   END IF

   INSERT INTO cta_ctr_decimo VALUES(v_fecha_corte,
                                     fecha_inicio,
                                     fecha_fin,
                                     NULL,
                                     var_usr,
                                     1)

    -----------------------------
   --SELECCION DE LOS AFILIADOS
    -----------------------------

   SELECT a.n_seguro
   FROM   afi_mae_afiliado a,
          cta_ctr_cuenta   c
   WHERE  n_seguro NOT IN( SELECT nss
                           FROM   cta_act_marca
                           WHERE  marca_cod in (120, 130) -- Inabilitados
                         )
   AND    a.tipo_solicitud != 5         -- No Asignado
   AND    a.n_seguro          = c. nss
   AND    c.ind_transferencia = 0
   AND    c.ind_edad          = 0
   INTO   TEMP tmp_afi_mae_afiliado

   ---------------------------------------
   -- PROCESAMIENTO PRINCIPAL DE CADA NSS
   ---------------------------------------

   DECLARE cur_afi CURSOR FOR
   SELECT  n_seguro
   FROM    tmp_afi_mae_afiliado
   ORDER BY 1

   FOREACH cur_afi INTO v_n_seguro

        -- IDENTIFICACION

      EXECUTE ex1 USING v_n_seguro, v_fecha_corte
                  INTO  v_existencia, v_edad, v_criterio

      IF v_existencia = 1 THEN

         IF v_edad = 1 THEN
               --MAYORES O IGUALES A 56

            LET v_mensaje = "NSS:",v_n_seguro, v_existencia,
                               v_edad, v_criterio

                -- CAMBIO DE REGIMEN

            EXECUTE ex2 USING v_n_seguro

            LET v_mensaje = v_mensaje CLIPPED," - ",sqlca.sqlcode

            EXECUTE ex4 USING v_n_seguro, "1", "0"

            LET v_mensaje = v_mensaje CLIPPED," - ",sqlca.sqlcode

            EXECUTE ex3 INTO v_status
                        USING v_n_seguro, "1", HOY

            LET v_mensaje = v_mensaje CLIPPED," - ",sqlca.sqlcode

            EXECUTE ex5 USING v_n_seguro, v_fecha_corte,
                              "1", HOY, var_usr


            LET v_mensaje = v_mensaje CLIPPED," - ",sqlca.sqlcode

              --ENVIA INFORMACION A LA BITACORA
            CALL ERRORLOG(v_mensaje)
         END IF
      END IF

   END FOREACH

----ermini ACTUALIZA REGISTRO EN cta_crt_decimo CON fecha_fin

   LET fecha_fin     = CURRENT

   UPDATE cta_ctr_decimo SET fecha_fin = fecha_fin
    WHERE fecha_corte    = v_fecha_corte
      AND fecha_ini      =  fecha_inicio

   EXIT PROGRAM 0

END MAIN

