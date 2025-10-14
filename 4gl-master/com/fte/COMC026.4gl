################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#Owner             => E.F.P.                                                   #
#Programa COMC026  => Genera detalle de calculo de comisiones                  #
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
         codven               char(10),
         n_folio              integer,
         nss                  char(11),
         tipo_solicitud       smallint,
         salario_base_comis   decimal(12,2),
         num_sm               decimal(6,2),
         cod_esq_comision     smallint,
         monto_comision       decimal(12,2),
         comis_pagada         decimal(12,2),
         estado_comision      smallint ,
         fecha_pago           date  
  END RECORD

  LET G_ARCHI = g_param.ruta_rescate CLIPPED,"/",usuario CLIPPED,
                              "_detalle.COM_",hoy USING "DDMMYYYY"

  START REPORT rpt_detalle_arc TO G_ARCHI

       DECLARE c_2 CURSOR FOR 

       SELECT  codven,
               n_folio,
               nss,
               tipo_solicitud,
               salario_base_comis,
               num_sm ,
               cod_esq_comision,
               monto_comision,
               comis_pagada,
               estado_comision,
               fecha_pago
       FROM    com_comis_detalle
       WHERE   nivel = 1
         AND   fecha_corte = hoy 
       ORDER   by 1,2

       FOREACH c_2 INTO g_reg1.*
  
          OUTPUT TO REPORT rpt_detalle_arc(g_reg1.*)

        END FOREACH

        FINISH REPORT rpt_detalle_arc
        ERROR "ARCHIVO GENERADO...."
        SLEEP 2
        ERROR ""


END FUNCTION

REPORT rpt_detalle_arc(g_reg1)

  DEFINE g_reg1   RECORD
         codven               char(10),
         n_folio              integer,
         nss                  char(11),
         tipo_solicitud       smallint,
         salario_base_comis   decimal(12,2),
         num_sm               decimal(6,2),
         cod_esq_comision     smallint,
         monto_comision       decimal(12,2),
         comis_pagada         decimal(12,2),
         estado_comision      smallint ,
         fecha_pago           date
  END RECORD

OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60

    FORMAT

      PAGE HEADER

      PRINT COLUMN 001,"num_int   |",
            COLUMN 012,"folio      |",
            COLUMN 024,"nss        |",
            COLUMN 036,"tip_so|",
            COLUMN 043,"sal_base      |",
            COLUMN 058,"num_sm  |",
            COLUMN 067,"esc_co|",
            COLUMN 074,"mont_comision |",
            COLUMN 089,"comis pagada  |",
            COLUMN 104,"edo   |",
            COLUMN 111,"fec_pago"

      ON EVERY ROW

      PRINT COLUMN 001,g_reg1.codven CLIPPED,
                       "|",g_reg1.n_folio CLIPPED, 
                       "|",g_reg1.nss CLIPPED, 
                       "|",g_reg1.tipo_solicitud CLIPPED, 
                       "|",g_reg1.salario_base_comis CLIPPED, 
                       "|",g_reg1.num_sm CLIPPED, 
                       "|",g_reg1.cod_esq_comision CLIPPED, 
                       "|",g_reg1.monto_comision CLIPPED, 
                       "|",g_reg1.comis_pagada CLIPPED, 
                       "|",g_reg1.estado_comision CLIPPED, 
                       "|",g_reg1.fecha_pago CLIPPED 
                       
  END REPORT
