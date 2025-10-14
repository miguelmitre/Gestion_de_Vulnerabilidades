################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#Owner             => E.F.P.                                                   #
#Programa COMC027  => Genera archivo  cifras detalle                           #
#Fecha             => 7-junio-2005                                             #
#By                => ISABEL FONSECA FRIAS.                                    #
#Sistema           => COM.                                                     #
################################################################################
DATABASE safre_af

GLOBALS 
  DEFINE G_ARCHI    CHAR(300)
  DEFINE hoy     DATE
  DEFINE g_param RECORD LIKE seg_modulo.*
  DEFINE usuario CHAR(08)

END GLOBALS

MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
   DEFER INTERRUPT

   SELECT *,
          USER
     INTO g_param.*,
          usuario
     FROM seg_modulo
     WHERE modulo_cod = "com"

   LET hoy = ARG_VAL(1)

   CALL Ejecuta_lectura()     

END MAIN

FUNCTION Ejecuta_lectura()

  DEFINE G_ARCHI    CHAR(300)

  DEFINE g_reg1   RECORD
         nss                  char(11),
         n_folio              integer,
         tipo_solicitud       smallint 
  END RECORD

  LET G_ARCHI = g_param.ruta_rescate CLIPPED,"/",usuario CLIPPED,
                              "_cif_det.COM_",hoy USING "DDMMYYYY"

  START REPORT rpt_cifras_arc TO G_ARCHI

       DECLARE c_2 CURSOR FOR 
       SELECT nss,
              n_folio,
              tipo_solicitud
       FROM   com_comis_detalle
       WHERE  fecha_corte = hoy 
       AND    nivel = 1

       FOREACH c_2 INTO g_reg1.*
  
          OUTPUT TO REPORT rpt_cifras_arc(g_reg1.*)

        END FOREACH

        FINISH REPORT rpt_cifras_arc
        ERROR "ARCHIVO GENERADO...."
        SLEEP 2
        ERROR ""


END FUNCTION

REPORT rpt_cifras_arc(g_reg1)

  DEFINE g_reg1   RECORD
         nss                  char(11),
         n_folio              integer,
         tipo_solicitud       smallint
  END RECORD

OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60

    FORMAT
 
      PAGE HEADER
      PRINT COLUMN 001,"nss        |",
            COLUMN 013,"folio      |",
            COLUMN 025,"tipo solicitud"

      ON EVERY ROW

      PRINT COLUMN 001,g_reg1.nss CLIPPED,
                       "|",g_reg1.n_folio CLIPPED, 
                       "|",g_reg1.tipo_solicitud CLIPPED
                       
  END REPORT
