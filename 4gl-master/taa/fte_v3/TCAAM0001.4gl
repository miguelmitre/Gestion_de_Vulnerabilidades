##########################################################################
#Proyecto          => AFORE ( MEXICO )                                   #
#Propietario       => E.F.P.                                             #
#Programa TCAAM0001  => CAPTURA FECHA VALUACIÓN PARA OP.09(NOTIFICACIÓN)  #
#Fecha             => 03 DE NOVIEMBRE  DE 2010                           #
#Autor             => JOSE FRANCISCO LUGO CORNEJO                        #
#Sistema           => TCAA(CEDENTE)                                      #
##########################################################################
DATABASE    safre_tmp
GLOBALS
   DEFINE
            g_fecha_valuacion                 ,
            g_today                           DATE
   DEFINE
            g_enter                         CHAR
END GLOBALS

MAIN
   OPTIONS 
            PROMPT    LINE LAST,
            INPUT     WRAP,
            ACCEPT    KEY      CONTROL-I
#           DEFER     INTERRUPT
   CALL     F_001_captura_fecha_valuacion()
END MAIN

FUNCTION    F_001_captura_fecha_valuacion()
   CALL     STARTLOG  ("TCAAM0001.log")
   LET      g_today                 =  TODAY
   OPEN     WINDOW    ventana_1      AT  2,2 
            WITH      FORM    "TCAAM0001"     ATTRIBUTE(BORDER)
   DISPLAY  "<TCAAM0001> CAPTURA FECHA VALUACIÓN PARA NOTIFICACIÓN DE SALDOS",
            "          "    AT  1,2   ATTRIBUTE(REVERSE)
   DISPLAY  g_today     USING   "DD-MM-YY"   AT   1,68   ATTRIBUTE(REVERSE)
   DISPLAY  "   <<Enter>> Continuar                  <<Ctrl-c>> Salir",
            "                         "      AT   3,2    ATTRIBUTE(REVERSE)
   SELECT   fecha_valuacion
     INTO   g_fecha_valuacion
     FROM   taa_cd_fecha_valuacion
   INPUT    BY  NAME    g_fecha_valuacion   WITHOUT DEFAULTS
            AFTER    FIELD   g_fecha_valuacion
                     IF      g_fecha_valuacion     IS    NULL    THEN
                             DELETE   FROM     taa_cd_fecha_valuacion;
                             ERROR " CON FECHA NULA LA VALUACION SERA LA DEL DIA    "	
                             PROMPT " TECLEE <Enter> Para Salir..." FOR g_enter
                     ELSE
                             ERROR  " ESTA ES LA FECHA PARA LA VALUACION...          "
                             PROMPT " TECLEE <Enter> Para Salir..." FOR g_enter
                             DELETE   FROM     taa_cd_fecha_valuacion;
                             INSERT   INTO     taa_cd_fecha_valuacion
                                    VALUES    (g_fecha_valuacion);
                     END IF
                     CLEAR   FORM
                     EXIT    INPUT
   END      INPUT
   CLOSE    WINDOW    ventana_1
   EXIT  PROGRAM
END FUNCTION

