#*************************************************************************
#*******
#******* PROGRAMA : CTAMFMTOC.4gl
#*******
#******* OBJETIVO :  Lanza por nohup la generacion del Formato C 
#*******             Transferencia d edecimos 
#*******          
#*******
#******* PARTE 2  : LANZA GENERACION FORMATO C 
#******* ELABORO  : Claudia U.
#******* FECHA    : 22-JULIO-2005
#*******
#*************************************************************************


DATABASE safre_af


DEFINE g_fecha_corte      ,
       g_fecha_liquida    DATE,
       g_folio            INTEGER,
       g_usuario          CHAR(10)

DEFINE g_proceso_cod      SMALLINT
DEFINE g_comando          CHAR(300)
DEFINE g_ruta_exp         CHAR(100)

MAIN
DEFER INTERRUPT

    SELECT ruta_exp,
           USER
      INTO g_ruta_exp,
           g_usuario
      FROM seg_modulo
     WHERE modulo_cod = "cta"


    IF fn_ejecuta_proceso ()  THEN

       LET g_comando ="cd ",g_ruta_exp CLIPPED,";",
                      "nohup time fglgo CTALFMTOC ", g_folio, " ",
                       g_fecha_corte   USING "MM/DD/YYYY","  1> ",
                       g_usuario CLIPPED,".salida 2> ",g_usuario CLIPPED,
                       ".error &"

       ERROR "EJECUTANDO PROCESO CTALFMTOC EN NOHUP "
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



   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CTAMFMTOC1" ATTRIBUTE(BORDER)
   DISPLAY " CTAMFMTOC   ANEXO C  SALDO DE TRABAJADORES EN PROCESO DE DECIMOS              " AT 3,1 ATTRIBUTE(REVERSE)

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

    SELECT folio
      INTO g_folio
      FROM cta_ctr_decimo
     WHERE fecha_corte = l_fecha_corte
       AND proceso_cod = 4

   IF g_folio IS NULL OR
      g_folio = 0 THEN
      ERROR " NO HAY REGISTROS EN TRANSFERENCIA DE DECIMOS."
      SLEEP 3
      ERROR ""
      LET v_status = FALSE
   END IF

RETURN v_status

END FUNCTION

