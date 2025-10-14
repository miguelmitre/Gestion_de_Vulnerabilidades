--************************************************************************
--*********** CTAEDOCTA  :  Programa que genera  informacion
--***********            :  para estado de cuenta
--***********            :  Identifica cuota
--***********            :  Obtiene saldos
--***********            :  Obtiene datos generales de NSS
--***********
--************************************************************************

DATABASE safre_af

DEFINE g_mensaje1           ,
       g_mensaje2           ,
       g_mensaje3           CHAR(100),
       g_mensaje4           CHAR(100),
       g_usuario            CHAR(8)  ,
       g_respuesta          CHAR(001)

DEFINE g_cancela           SMALLINT

DEFINE g_fecha_corte       DATE

DEFINE g_ruta_uni_exp      CHAR(100)
#Cuatrimestre1 2009
DEFINE gi_mes SMALLINT

MAIN
   DEFER INTERRUPT
   CALL fn_inicia_variables()
   CALL proceso_principal()
END MAIN

FUNCTION fn_inicia_variables()

DEFINE v_tabla  CHAR(20)


SELECT USER
  INTO g_usuario
  FROM systables
 WHERE tabid = 1

SELECT ruta_exp
  INTO g_ruta_uni_exp
  FROM seg_modulo
 WHERE modulo_cod = "uni"

SELECT tabname
  INTO v_tabla
  FROM safre_tmp:systables
 WHERE tabname = "cta_ctr_edocta"

IF v_tabla IS NULL THEN
   DATABASE safre_tmp
   SQL
   create table cta_ctr_edocta
     (
       fecha_corte date,
       paso smallint,
       fecha_ini datetime year to second,
       fecha_fin datetime year to second,
       usuario char(8),
       fecha_proceso date,
       primary key (fecha_corte,paso)  constraint pk_edocta
     );
   END SQL
   DATABASE safre_af
END IF

END FUNCTION

FUNCTION fn_inicializa()

DEFINE v_paso  SMALLINT

LET v_paso = 1

LET g_mensaje1 = "ESTE PROCESO ELIMINARA LA INFORMACION DE LA GENERACION",
                 " DEL ULTIMO ESTADO DE CUENTA."

LET g_mensaje2 = "INDIQUE LA FECHA DEL PERIODO:"

LET g_mensaje3 = "ESTA SEGURO DE CONTINUAR EJECUCION S/N:"

LET g_cancela = FALSE

INPUT BY NAME g_fecha_corte,
              g_respuesta WITHOUT DEFAULTS

      BEFORE INPUT
             DISPLAY BY NAME g_mensaje1,
                             g_mensaje2

      BEFORE FIELD g_fecha_corte
             DISPLAY "" TO formonly.g_mensaje3
             DISPLAY "" TO formonly.g_respuesta

             #Cuatrimestre1 2009
             IF gi_mes <= 4 THEN
             	  LET g_fecha_corte = MDY (12,31, YEAR(TODAY) - 1)
             ELSE
                IF gi_mes > 4 AND gi_mes <= 8 THEN
                	  LET g_fecha_corte = MDY (4,30, YEAR(TODAY))
                ELSE
                	  IF gi_mes > 8 AND gi_mes <= 12 THEN
                	  	 LET g_fecha_corte = MDY (8,31, YEAR(TODAY))
                	  END IF
                END IF
             END IF

      AFTER FIELD g_fecha_corte
            IF g_fecha_corte IS NULL THEN
               ERROR "DEBE INDICAR FECHA DE CORTE"
               SLEEP 3
               ERROR ""
               NEXT FIELD g_fecha_corte
            ELSE
               IF fn_valida_proceso(0,g_fecha_corte,v_paso) <> 0 THEN
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
                  IF fn_borra_informacion(0,g_fecha_corte) = 0 THEN
                     CALL fn_finaliza_paso ( v_paso ,
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
#Cuatrimestre1 2009
LET gi_mes = MONTH(hoy)

    OPEN WINDOW  w_menu AT 4,4 WITH FORM "CTAEDOCTA1" ATTRIBUTES(BORDER)

    DISPLAY "CTAEDOCTA     GENERA INF. EDO DE CUENTA       < Ctrl-C > SALIR     " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)

    MENU "ESTADO DE CUENTA "
         COMMAND "Inicializa" "Limpia estructura, elimina informacion anterior."
            CALL fn_inicializa()
            CLEAR FORM
         COMMAND "Cuota" " Genera cuota de mercado ."
            CALL fn_lee_datos(2)
            CLEAR FORM
         COMMAND "Obtiene saldos" "Ejecuta por nohup obtencion de saldos."
            CALL fn_genera_saldo()
            CLEAR FORM
         COMMAND "Prepara NSS " "Identifica NSS"
            CALL fn_lee_datos(4)
            CLEAR FORM
         COMMAND "Genera datos" "Ejecuta obtencion de datos generales."
            CALL fn_lee_datos(5)
            CLEAR FORM

         --COMMAND "Insertar rendimientos" "Agrega los rendimientos del semestre."
            --CALL CTA_REND()
            --CLEAR FORM

         COMMAND "Revisa ejecucion" "Despliega la ejecucion de los procesos."
            CALL fn_revisa_proceso()
            CLEAR FORM
         COMMAND "Elimina ejecucion" "Elimina ejecucion de un paso."
            CALL fn_elimina_paso(0,today)
            CLEAR FORM
         COMMAND "Salir" "Salir del Programa"
            EXIT MENU
    END MENU

END FUNCTION

FUNCTION fn_borra_informacion(l_paso, l_fecha_corte)

DEFINE l_paso  SMALLINT
DEFINE l_fecha_corte DATE
DEFINE v_fecha_sem DATE

DEFINE v_error     INTEGER
DEFINE v_comando   CHAR(150)
DEFINE v_nom_tabla ,
       v_tabla     CHAR(15)

LET v_error = 0

#Cuatrimestre1 2009
IF MONTH(l_fecha_corte) = 4 THEN
   LET v_fecha_sem = MDY(12,31,YEAR(l_fecha_corte)-1)
ELSE
	 IF MONTH(l_fecha_corte) = 8 THEN
	    LET v_fecha_sem = MDY(4,30,YEAR(l_fecha_corte))
	 ELSE
	 	  LET v_fecha_sem = MDY(8,31,YEAR(l_fecha_corte))
	 END IF
END IF

LET v_nom_tabla = "saldos_",v_fecha_sem USING "MMYYYY"

LET v_comando = "RENAME TABLE tmp_saldo_edocta TO ",
                v_nom_tabla CLIPPED
PREPARE ren_tabla FROM v_comando

LET v_comando = "RENAME INDEX tmp_saldo_edocta1 TO ",
                v_nom_tabla CLIPPED,"1"

PREPARE ren_index FROM v_comando

IF l_paso = 0 THEN
WHENEVER ERROR CONTINUE
   EXECUTE ren_tabla
   EXECUTE ren_index
WHENEVER ERROR STOP

SELECT tabname
  INTO v_tabla
  FROM systables
 WHERE tabname = v_nom_tabla

IF v_tabla IS NOT NULL THEN
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_saldo_edocta
   WHENEVER ERROR STOP
END IF

SQL
create table tmp_saldo_edocta
(
  nss char(11),
  subcuenta smallint,
  siefore smallint,
  fecha_conversion date,
  monto_en_acciones decimal(22,6),
  monto_en_pesos decimal(22,6)
) in taa_dbs1;

END SQL
WHENEVER ERROR CONTINUE
   DROP TABLE cta_nss_edo_cta
   DROP TABLE cta_id_datos
WHENEVER ERROR STOP

#Cuatrimestre3 2009
--Agregar columna folio

#Cuatrimestre1 2010
--Agregar campos
--tipo_trab      (afil, no afil, asig)
--ind_actividad  (activo, inactivo)
--ind_bono       (sin bono, bono afore, bono traspaso)

SQL
create table cta_nss_edo_cta
  (
    folio_envio      INTEGER ,
    nss              CHAR(11),
    fecha_generacion DATE    ,
    consec_lote      INTEGER ,
    estado           SMALLINT,
    status           SMALLINT,
    ind_edad         SMALLINT,
    curp             CHAR(18),
    folio            CHAR(20),
    tipo_trab        SMALLINT,
    ind_actividad    SMALLINT,
    ind_bono         SMALLINT,
    correoe          CHAR(40),
    ind_afil         SMALLINT,
    ind_fmto         SMALLINT
  ) in taa_dbs1
END SQL

SQL
create table cta_id_datos
  (
    nss char(11),
    curp char(18),
    n_folio decimal(10,0),
    nombre char(120),
    rfc char(13),
    infonavit char(1),
    fentcons date,
    calle char(62),
    colonia char(60),
    delegacion char(40),
    estado char(40),
    ciudad char(40),
    postal_cod char(5),
    reparto_cod char(5)
  ) IN taa_dbs1
END SQL

ELSE
    CASE l_paso
         WHEN 3
               WHENEVER ERROR CONTINUE
                  DROP TABLE tmp_saldo_edocta
               WHENEVER ERROR STOP

                SQL
                create table tmp_saldo_edocta
                (
                  nss char(11),
                  subcuenta smallint,
                  siefore smallint,
                  fecha_conversion date,
                  monto_en_acciones decimal(22,6),
                  monto_en_pesos decimal(22,6)
                ) in taa_dbs1
                END SQL
         WHEN 4
               WHENEVER ERROR CONTINUE
                  DROP TABLE cta_nss_edo_cta
               WHENEVER ERROR STOP

               #Cuatrimestre3 2009
               --Agregar columna folio
               
               #Cuatrimestre1 2010
               --Agregar campos
               --tipo_trab      (afil, no afil, asig)
               --ind_actividad  (activo, inactivo)
               --ind_bono       (sin bono, bono afore, bono traspaso)

                SQL
                create table cta_nss_edo_cta
                (
                  folio_envio      INTEGER ,
                  nss              CHAR(11),
                  fecha_generacion DATE    ,
                  consec_lote      INTEGER ,
                  estado           SMALLINT,
                  status           SMALLINT,
                  ind_edad         SMALLINT,
                  curp             CHAR(18),
                  folio            CHAR(20),
                  tipo_trab        SMALLINT,
                  ind_actividad    SMALLINT,
                  ind_bono         SMALLINT,
                  correoe          CHAR(40),
                  ind_afil         SMALLINT,
                  ind_fmto         SMALLINT
                ) in taa_dbs1
                END SQL
         WHEN 5
               WHENEVER ERROR CONTINUE
                  DROP TABLE cta_id_datos
               WHENEVER ERROR STOP

               SQL
               create table cta_id_datos
                 (
                   nss char(11),
                   curp char(18),
                   n_folio decimal(10,0),
                   nombre char(120),
                   rfc char(13),
                   infonavit char(1),
                   fentcons date,
                   calle char(62),
                   colonia char(60),
                   delegacion char(40),
                   estado char(40),
                   ciudad char(40),
                   postal_cod char(5),
                   reparto_cod char(5)
                 ) IN taa_dbs1
               END SQL
    END CASE

END IF

IF v_error <> 0 THEN
   LET v_error = -1
END IF

RETURN v_error

END FUNCTION

FUNCTION fn_valida_proceso ( p_tipo,
                             p_fecha_corte,
                             p_paso
                           )

DEFINE p_paso         ,
       p_tipo         ,
       v_paso         ,
       v_paso_ant     ,
       v_status       SMALLINT

DEFINE p_fecha_corte  ,
       v_fecha_fin    DATE


   LET v_paso      = NULL
   LET v_fecha_fin = NULL

   IF p_paso = 4 THEN
      LET v_paso_ant  = 2
   ELSE
      LET v_paso_ant  = p_paso - 1
   END IF

   LET v_status = 0

IF p_tipo = 0 THEN

   IF v_paso_ant <> 0 THEN

       --  Verifica ejecucion del paso anterior

       SELECT paso     ,
              fecha_fin
         INTO v_paso   ,
              v_fecha_fin
         FROM safre_tmp:cta_ctr_edocta
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
         INTO v_paso   ,
              v_fecha_fin
         FROM safre_tmp:cta_ctr_edocta
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

ELSE
       SELECT paso     ,
              fecha_fin
         INTO v_paso   ,
              v_fecha_fin
         FROM safre_tmp:cta_ctr_edocta
        WHERE fecha_corte = p_fecha_corte
          AND paso        = p_paso

        IF v_paso IS NULL THEN
           ERROR "PROCESO NO EXISTE, PARA ESTA FECHA."
           SLEEP 3
           ERROR ""
           LET v_status = 1
        END IF
END IF

RETURN v_status

END FUNCTION

FUNCTION fn_inserta_paso   ( p_paso,
                             p_fecha_corte
                           )

DEFINE p_paso        SMALLINT
DEFINE p_fecha_corte DATE

INSERT INTO safre_tmp:cta_ctr_edocta  VALUES ( p_fecha_corte,
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

UPDATE safre_tmp:cta_ctr_edocta
   SET fecha_fin = CURRENT,
       usuario   = USER
 WHERE fecha_corte = p_fecha_corte
   AND paso        = p_paso


END FUNCTION

FUNCTION fn_elimina_paso  (p_paso,
                           p_fecha_corte)

DEFINE p_paso,
       g_paso  SMALLINT

DEFINE p_fecha_corte DATE,
       x_sw          SMALLINT


IF p_paso = 0 THEN
   LET g_mensaje1 = "ESTE PROCESO ELIMINA LA EJECUCION DE  ",
                    "UN PASO EN EL PERIODO."

   LET g_mensaje2 = "INDIQUE LA FECHA DEL PERIODO:"

   LET g_mensaje4 = "INDIQUE PASO :"

   LET g_mensaje3 = "ESTA SEGURO DE ELIMINAR EJECUCION S/N:"

   LET g_cancela = FALSE

   INPUT BY NAME g_fecha_corte,
                 g_paso       ,
                 g_respuesta

         BEFORE INPUT
                DISPLAY BY NAME g_mensaje1,
                                g_mensaje2,
                                g_mensaje4

         BEFORE FIELD g_fecha_corte
                DISPLAY "" TO formonly.g_mensaje3
                DISPLAY "" TO formonly.g_respuesta

                #Cuatrimestre1 2009
             IF gi_mes <= 4 THEN
             	  LET g_fecha_corte = MDY (12,31, YEAR(TODAY) - 1)
             ELSE
                IF gi_mes > 4 AND gi_mes <= 8 THEN
                	  LET g_fecha_corte = MDY (4,30, YEAR(TODAY))
                ELSE
                	  IF gi_mes > 8 AND gi_mes <= 12 THEN
                	  	 LET g_fecha_corte = MDY (8,31, YEAR(TODAY))
                	  END IF
                END IF
             END IF

         AFTER FIELD g_fecha_corte
               IF g_fecha_corte IS NULL THEN
                  ERROR "DEBE INDICAR FECHA DE CORTE"
                  SLEEP 3
                  ERROR ""
                  NEXT FIELD g_fecha_corte
               END IF

         AFTER FIELD g_paso
               IF g_paso IS NULL THEN
                  ERROR "DEBE INDICAR PASO A ELIMINAR"
                  SLEEP 3
                  ERROR ""
                  NEXT FIELD g_paso
               END IF

               IF fn_valida_proceso(1,g_fecha_corte,g_paso) <> 0 THEN
                  NEXT FIELD g_fecha_corte
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
                     DELETE
                       FROM safre_tmp:cta_ctr_edocta
                      WHERE fecha_corte = g_fecha_corte
                        AND paso        = g_paso

                     IF g_paso = 3 OR
                        g_paso = 4 OR
                        g_paso = 5 THEN
                        CALL fn_borra_informacion ( g_paso,g_fecha_corte )
                             RETURNING x_sw
                     END IF

                     ERROR "EJECUCION DE PASO ELIMINADA."
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
ELSE
   DELETE
     FROM safre_tmp:cta_ctr_edocta
    WHERE fecha_corte = g_fecha_corte
      AND paso        = g_paso
END IF

END FUNCTION

FUNCTION fn_lee_datos(l_paso)

DEFINE l_paso  SMALLINT

CASE l_paso
     WHEN 2
            LET g_mensaje1 = "ESTE PROCESO GENERA CUOTA DE MERCADO   ",
                             "PARA EL PERIODO.                       "
     WHEN 4
            LET g_mensaje1 = "ESTE PROCESO IDENTIFICA NSS PARA GENERAR",
                             " ESTADO DE CUENTA.                     "
     WHEN 5
            LET g_mensaje1 = "ESTE PROCESO OBTIENE DATOS GENERALES DE",
                             " NSS PARA ESTADO DE CUENTA.            "
END CASE

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
             #Cuatrimestre1 2009
             IF gi_mes <= 4 THEN
             	  LET g_fecha_corte = MDY (12,31, YEAR(TODAY) - 1)
             ELSE
                IF gi_mes > 4 AND gi_mes <= 8 THEN
                	  LET g_fecha_corte = MDY (4,30, YEAR(TODAY))
                ELSE
                	  IF gi_mes > 8 AND gi_mes <= 12 THEN
                	  	 LET g_fecha_corte = MDY (8,31, YEAR(TODAY))
                	  END IF
                END IF
             END IF

             {IF MONTH(TODAY) <= 6 THEN
                LET g_fecha_corte = MDY( 12,31,YEAR(TODAY)-1 ) ;
             ELSE
                LET g_fecha_corte = MDY( 6,30,YEAR(TODAY) ) ;
             END IF}

      AFTER FIELD g_fecha_corte
            IF g_fecha_corte IS NULL THEN
               ERROR "DEBE INDICAR FECHA DE CORTE"
               SLEEP 3
               ERROR ""
               NEXT FIELD g_fecha_corte
            ELSE
               IF fn_valida_proceso(0,g_fecha_corte,l_paso) <> 0 THEN
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
                  CALL fn_inserta_paso ( l_paso ,
                                         g_fecha_corte
                                       )
                  CALL fn_ejecuta_paso(g_fecha_corte,l_paso )
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

             #Cuatrimestre1 2009
             IF gi_mes <= 4 THEN
             	  LET g_fecha_corte = MDY (12,31, YEAR(TODAY) - 1)
             ELSE
                IF gi_mes > 4 AND gi_mes <= 8 THEN
                	  LET g_fecha_corte = MDY (4,30, YEAR(TODAY))
                ELSE
                	  IF gi_mes > 8 AND gi_mes <= 12 THEN
                	  	 LET g_fecha_corte = MDY (8,31, YEAR(TODAY))
                	  END IF
                END IF
             END IF

      AFTER FIELD g_fecha_corte
            IF g_fecha_corte IS NULL THEN
               ERROR "DEBE INDICAR FECHA DE CORTE"
               SLEEP 3
               ERROR ""
               NEXT FIELD g_fecha_corte
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
                  IF fn_valida_proceso(0,g_fecha_corte,v_paso) <> 0 THEN
                     NEXT FIELD g_fecha_corte
                  END IF
                  CALL fn_inserta_paso ( v_paso ,
                                         g_fecha_corte
                                       )
                  CALL fn_ejecuta_paso(g_fecha_corte,3)
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
################################################################################
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

             #Cuatrimestre1 2009
             IF gi_mes <= 4 THEN
             	  LET g_fecha_corte = MDY (12,31, YEAR(TODAY) - 1)
             ELSE
                IF gi_mes > 4 AND gi_mes <= 8 THEN
                	  LET g_fecha_corte = MDY (4,30, YEAR(TODAY))
                ELSE
                	  IF gi_mes > 8 AND gi_mes <= 12 THEN
                	  	 LET g_fecha_corte = MDY (8,31, YEAR(TODAY))
                	  END IF
                END IF
             END IF

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

DEFINE l_fecha_corte	DATE

DEFINE pos,v_status  SMALLINT

DEFINE arr_fmtoa ARRAY[8] OF RECORD
       paso             CHAR(1),
       desc_paso        CHAR(12),
       fecha_ini        DATETIME YEAR TO SECOND,
       fecha_fin        DATETIME YEAR TO SECOND,
       usuario          CHAR(8)
       END RECORD


OPEN WINDOW  w_submenu AT 6,4 WITH FORM "CTAEDOCTA2" ATTRIBUTES(BORDER)
DISPLAY "CTAEDOCTA     GENERA INF. EDO DE CUENTA       < Ctrl-C > SALIR     " AT 3,1 ATTRIBUTE(REVERSE)


DECLARE cur_fa CURSOR FOR
   SELECT paso,
          DECODE(paso,1,"INICIALIZA  ",
                      2,"CUOTA       ",
                      3,"SALDOS      ",
                      4,"IDENTIFICA  ",
                      5,"DATOS GRALES") desc_paso,
          fecha_ini,
          fecha_fin,
          usuario
     FROM safre_tmp:cta_ctr_edocta
    WHERE fecha_corte = l_fecha_corte
    ORDER BY fecha_ini,paso

   LET pos = 1
   FOREACH cur_fa INTO arr_fmtoa[pos].*
      LET pos = pos + 1
      IF pos >= 8 THEN
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

FUNCTION fn_ejecuta_paso( p_fecha_corte, p_paso )

DEFINE p_fecha_corte  DATE
DEFINE p_paso         SMALLINT
DEFINE v_fecha_inicio DATE

DEFINE v_comando  CHAR(200)

CASE p_paso
     WHEN 2
         DATABASE safre_tmp
         LET v_comando = "EXECUTE PROCEDURE amafore('",p_fecha_corte,
                         "')"
         PREPARE eje_cuota FROM v_comando
         ERROR "GENERANDO CUOTA DE MERCADO ..."
         EXECUTE eje_cuota
         ERROR "INFORMACION GENERADA"
         SLEEP 3
         ERROR ""
         DATABASE safre_af
         CALL fn_finaliza_paso ( p_paso , p_fecha_corte )

     WHEN 3
     	   #Cuatrimestre1 2009
     	   CALL get_cuatrimestre(p_fecha_corte) RETURNING v_fecha_inicio
         {IF MONTH(p_fecha_corte) = 6 THEN
            LET v_fecha_inicio = MDY(12,31,YEAR(p_fecha_corte)-1)
         ELSE
            LET v_fecha_inicio = MDY(6,30,YEAR(p_fecha_corte))
         END IF}
         LET v_comando = "nohup time eje_saldo_edocta.sh ",
                         "'",v_fecha_inicio,"'",
                         " ",
                         "'",p_fecha_corte,"'",
                         " ",
                         "'",g_usuario CLIPPED,"'",
                         " 1> ",g_usuario CLIPPED,".salida ",
                          "2> ",g_usuario CLIPPED,".error &"
         ERROR "VERIFIQUE ARCHIVOS: ",g_usuario CLIPPED,".salida y ",g_usuario CLIPPED,".error"
         SLEEP 5
         ERROR ""
     WHEN 4
{
         PREPARE eje_nss FROM "EXECUTE PROCEDURE sp_llena_edocta()"
         ERROR "GENERANDO INFORMACION ..."
         EXECUTE eje_nss
         ERROR "INFORMACION GENERADA"
         SLEEP 3
         ERROR ""
         CALL fn_finaliza_paso ( p_paso , p_fecha_corte )
}
     	   ERROR "EJECUTANDO PROCESO POR NOHUP."
         SLEEP 3
         ERROR ""

         LET v_comando = "nohup time ./eje_llena_edocta.sh ",
                         "'",p_fecha_corte,"'"," ",
                         "'",g_usuario CLIPPED,"'",
                         " 1> ",g_usuario CLIPPED,".out_llena_edocta ",
                          "2> ",g_usuario CLIPPED,".err_llena_edocta &"

         ERROR "VERIFIQUE ARCHIVOS: ",g_usuario CLIPPED,".out_llena_edocta y ",g_usuario CLIPPED,".err_llena_edocta"
         SLEEP 5
         ERROR ""

     WHEN 5
     	   ERROR "EJECUTANDO PROCESO POR NOHUP."
         SLEEP 3
         ERROR ""

     	   LET v_comando = "nohup time ./eje_gen_datos.sh ",p_fecha_corte," ",
                          g_usuario CLIPPED," 1> ",g_usuario CLIPPED,".out_domi ",
                          "2> ",g_usuario CLIPPED,".err_domi &"
         ERROR "VERIFIQUE ARCHIVOS: ",g_usuario CLIPPED,".out_domi y ",g_usuario CLIPPED,".err_domi"
         SLEEP 5
         ERROR ""

         {PREPARE eje_nss_dat FROM "EXECUTE PROCEDURE datos_trabajador()"
         ERROR "GENERANDO INFORMACION ..."
         EXECUTE eje_nss_dat
         CREATE INDEX cta_id_datosix ON cta_id_datos ( nss )
         UPDATE STATISTICS FOR TABLE cta_id_datos
         ERROR "INFORMACION GENERADA"
         SLEEP 3
         ERROR ""
         CALL fn_finaliza_paso ( p_paso , p_fecha_corte )}
END CASE

RUN v_comando

END FUNCTION
#############################################################################
FUNCTION get_cuatrimestre(ld_fecha_fin)
   DEFINE ld_fecha_fin,
          ld_fecha_ini DATE

   LET ld_fecha_ini = MDY (MONTH(ld_fecha_fin), 1, YEAR(ld_fecha_fin))

   LET ld_fecha_ini = ld_fecha_ini - 3 UNITS MONTH
   LET ld_fecha_ini = ld_fecha_ini - 1 UNITS DAY

   RETURN ld_fecha_ini
END FUNCTION
#############################################################################