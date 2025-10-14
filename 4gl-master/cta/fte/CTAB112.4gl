#******* PROGRAMA : CTAB112.4gl
#*******
#******* OBJETIVO :  Lanza por nohup congelacion de saldos
#*******
#*******
#*******
#******* PARTE 2  : LANZA CONGELACION DE SALDOS
#******* ELABORO  : Claudia U.
#******* FECHA    : 8-JUNIO-2005
#*******
#*************************************************************************


DATABASE safre_af


DEFINE g_fecha_corte      ,
       g_fecha_liquida    DATE

DEFINE g_proceso_cod      SMALLINT
DEFINE g_comando          CHAR(300)
DEFINE g_ruta_exp         CHAR(100)


MAIN

DEFER INTERRUPT

    SELECT ruta_exp
      INTO g_ruta_exp
      FROM seg_modulo
     WHERE modulo_cod = "cta"


    IF fn_ejecuta_proceso ()  THEN

       LET g_comando ="cd ",g_ruta_exp CLIPPED,";",
                      "nohup time fglgo CTAB1121 ",
                       g_fecha_corte   USING "MM/DD/YYYY"," &"

       ERROR "EJECUTANDO PROCESO CTAB1121 EN NOHUP "
       SLEEP 3
       ERROR ""
       RUN g_comando

    END IF

END MAIN


FUNCTION fn_ejecuta_proceso ()

DEFINE  v_status          SMALLINT

DEFINE  v_hoy             DATE

DEFINE  v_respuesta       CHAR(1)

LET v_status = TRUE

LET g_fecha_liquida    = TODAY
LET v_hoy              = TODAY

    IF MONTH(v_hoy) <= 6 THEN
        LET g_fecha_corte = MDY( 6,30,YEAR(v_hoy) )
    ELSE
        LET g_fecha_corte = MDY( 12,31,YEAR(v_hoy) )
    END IF



   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CTAB1121" ATTRIBUTE(BORDER)
   DISPLAY " CTAB112   CONGELACION DE SALDOS TRANSFERENCIA DE DECIMOS                      " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY v_hoy USING "dd-mm-yyyy" AT 3,62 ATTRIBUTE(REVERSE)

   INPUT BY NAME g_fecha_corte,
                 v_respuesta WITHOUT DEFAULTS

         BEFORE INPUT
                DISPLAY BY NAME g_fecha_corte

         AFTER FIELD g_fecha_corte
               IF g_fecha_corte IS NULL THEN
                  ERROR "Debe indicar Fecha de corte"
                  SLEEP 3
                  ERROR ""
                  NEXT FIELD g_fecha_corte
               ELSE
                  IF fn_valida_proceso(g_fecha_corte) THEN
                     NEXT FIELD v_respuesta
                  ELSE
                     NEXT FIELD g_fecha_corte
                  END IF
               END IF

         BEFORE FIELD v_respuesta
                DISPLAY "" TO formonly.v_respuesta

         AFTER FIELD v_respuesta
               IF v_respuesta NOT MATCHES "[SN]" THEN
                  CLEAR FORM
                  NEXT FIELD v_respuesta
               ELSE
                  IF v_respuesta = "N" THEN
                     LET v_status = FALSE
                     ERROR "Proceso Cancelado."
                     SLEEP 2
                     EXIT INPUT
                  ELSE
                     EXIT INPUT
                  END IF
               END IF

         ON KEY (INTERRUPT)
            LET v_status = FALSE
            LET int_flag = FALSE
            EXIT INPUT

         ON KEY (CONTROL-C)
            LET v_status = FALSE
            LET int_flag = FALSE
            EXIT INPUT

  END INPUT

RETURN v_status

END FUNCTION

FUNCTION fn_valida_proceso(l_fecha_corte)

DEFINE  l_fecha_corte     DATE
DEFINE  v_fecha_fin       DATE

DEFINE  v_status          SMALLINT

LET v_status = TRUE

---   Verifica que haya finalizado el proceso anterior
---   Identificacion de cuentas .

    SELECT proceso_cod,
           fecha_fin
      INTO g_proceso_cod,
           v_fecha_fin
      FROM cta_ctr_decimo
     WHERE fecha_corte = l_fecha_corte
       AND proceso_cod = 1

   IF g_proceso_cod IS NULL OR
      g_proceso_cod = 0 THEN
      ERROR " NO EXISTE EL PROCESO ANTERIOR PARA LA FECHA DE CORTE."
      SLEEP 3
      ERROR ""
      LET v_status = FALSE
   ELSE
      IF v_fecha_fin IS NULL OR
         v_fecha_fin = "12/31/1899" THEN
         IF g_proceso_cod = 1 THEN
            ERROR " PROCESO ANTERIOR AUN NO FINALIZA."
            SLEEP 3
            ERROR ""
            LET v_status = FALSE
         ELSE
            ERROR " NO EXISTE PROCESO ANTERIOR PARA LA FECHA."
            SLEEP 3
            ERROR ""
            LET v_status = FALSE
         END IF
      ELSE
         LET g_proceso_cod = NULL
         LET v_fecha_fin   = NULL

         SELECT c.proceso_cod,
                c.fecha_fin
           INTO g_proceso_cod,
                v_fecha_fin
           FROM cta_ctr_decimo c
          WHERE c.fecha_corte = l_fecha_corte
            AND c.proceso_cod = 2

         IF ( v_fecha_fin IS NULL OR
              v_fecha_fin = "12/31/1899" ) AND
              g_proceso_cod = 2 THEN
              ERROR "PROCESO YA EJECUTANDOSE"
              SLEEP 3
              ERROR ""
              LET v_status = FALSE
         ELSE
            IF ( v_fecha_fin IS NOT NULL AND
                 v_fecha_fin <> "12/31/1899" ) AND
                 g_proceso_cod = 2 THEN

                 ERROR "PROCESO YA EJECUTADO."
                 SLEEP 3
                 ERROR ""
                 LET v_status = FALSE
            ELSE
               LET g_proceso_cod = 2

---  Inserta inicio del proceso congelacion de saldos.
               INSERT INTO cta_ctr_decimo ( fecha_corte,
                                            proceso_cod,
                                            fecha_ini  ,
                                            usuario )
                                   VALUES ( l_fecha_corte,
                                            g_proceso_cod,
                                            CURRENT      ,
                                            USER
                                          )
            END IF
         END IF
      END IF
   END IF

RETURN v_status

END FUNCTION

