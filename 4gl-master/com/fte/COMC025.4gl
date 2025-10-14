################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#Owner             => E.F.P.                                                   #
#Programa COMC025  => Genera archivo con sumatoria para subdirectpres          #
#                  => Gerentes y Ejecutivos                                    #
#Fecha             => 26-mayo-2005                                             #
#By                => ISABEL FONSECA FRIAS.                                    #
#Sistema           => COM.                                                     #
################################################################################
DATABASE safre_af

GLOBALS 
  DEFINE G_ARCHI    CHAR(300)
  DEFINE hoy     DATE
  DEFINE g_param RECORD LIKE seg_modulo.*
  DEFINE usuario CHAR(08)
  DEFINE opc     CHAR(01)
  DEFINE ejecuta CHAR(200)
  DEFINE opcx    CHAR(01)

  DEFINE g_reg   RECORD
         fecha_corte        DATE
  END RECORD

  DEFINE g_reg1   RECORD
         codven              CHAR(10),
         contador            SMALLINT,
         coduni_n1           CHAR(10), 
         salario_base_comis  DECIMAL(10,2), 
         num_sm              DECIMAL(10,2),
         monto_comision      DECIMAL(10,2),
         comis_pagada        DECIMAL(10,2)

  END RECORD

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

   LET hoy = TODAY
   LET opcx = NULL

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "COMC0251" ATTRIBUTE(BORDER)

   DISPLAY " COMB025                                                                       " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   DISPLAY "                          GENERA ARCHIVOS                                      " AT 6,1 ATTRIBUTE(REVERSE)

   MENU "Genera Archivos"
      COMMAND "(Cifras Detalle)"  "Genera archivo Cifras Detalle" 
         LET opcx = 'C' 
         CALL Carga()
      COMMAND "(Subdirectores)"  "Genera archivo Subdirector" 
         LET opcx = 'S' 
         CALL Carga()
      COMMAND "(Gerentes)"  "Genera archivo Gerentes"
         LET opcx = 'G' 
         CALL Carga()
      COMMAND "(Ejecutivos)"  "Genera archivo Ejecutivo" 
         LET opcx = 'E' 
         CALL Carga()
      COMMAND "(Detalle)"  "Genera archivo Detalle Comision" 
         LET opcx = 'D' 
         CALL Carga()
      COMMAND "Salida" "Salida"
         EXIT MENU
   END MENU


  CLOSE WINDOW ventana1
END MAIN

FUNCTION Carga()

   INPUT BY NAME g_reg.*
      AFTER FIELD fecha_corte
         IF g_reg.fecha_corte IS NULL THEN
            ERROR "Este campo no puede ser nulo"
         END IF

   EXIT INPUT

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT


   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      RETURN
   END IF


   PROMPT "Desea ejecutar el proceso [S/N] ..." FOR opc

   IF opc MATCHES '[Ss]' THEN
      ERROR "Procedando informacion ..."
      CASE opcx
         WHEN "C"
         LET ejecuta = " fglgo COMC027.4gi ",g_reg.fecha_corte," " 
         RUN ejecuta
         WHEN "G"
          CALL Ejecuta_Gerente()
         WHEN "S"
          CALL Ejecuta_Subdirector()
         WHEN "E"
          CALL Ejecuta_Ejecutivo()
         WHEN "D"
         LET ejecuta = " fglgo COMC026.4gi ",g_reg.fecha_corte," " 
         RUN ejecuta
     END CASE 

   END IF
   CLEAR FORM
   CLEAR SCREEN

END FUNCTION

FUNCTION Ejecuta_Gerente()

  DEFINE G_ARCHI    CHAR(300)

  DEFINE g_reg1   RECORD
         codven              CHAR(10),
         contador            SMALLINT,
         coduni_n1           CHAR(10), 
         salario_base_comis  DECIMAL(10,2),
         num_sm              DECIMAL(10,2),
         monto_comision      DECIMAL(10,2),
         comis_pagada        DECIMAL(10,2)

  END RECORD

    
  LET G_ARCHI = g_param.ruta_rescate CLIPPED,"/",usuario CLIPPED,
                              "_ade_ger.COM_",g_reg.fecha_corte USING "DDMMYYYY"

  -- PARA IMPRESION
  START REPORT rpt_cuenta_arc TO G_ARCHI

       DECLARE c_2 CURSOR FOR 
       SELECT codven,
              count(*),
              coduni_n1,  
              sum(salario_base_comis),
              sum(num_sm),
              sum(monto_comision),
              sum(comis_pagada)
       FROM   com_comis_detalle
       WHERE  nivel = 2
       AND    fecha_corte = g_reg.fecha_corte 
       GROUP  by 1,3


       FOREACH c_2 INTO g_reg1.*
  
          --PARA LA IMPRESION
          OUTPUT TO REPORT rpt_cuenta_arc(g_reg1.*)


        END FOREACH

        FINISH REPORT rpt_cuenta_arc
        ERROR "LISTADO 1 GENERADO...."
        SLEEP 2
        ERROR ""


END FUNCTION

REPORT rpt_cuenta_arc(g_reg1)

  DEFINE vcomis_pag100       DECIMAL(10,2) 
  DEFINE g_reg1   RECORD

         codven              CHAR(10),
         contador            SMALLINT,
         coduni_n1           CHAR(10),
         salario_base_comis  DECIMAL(10,2),
         num_sm              DECIMAL(10,2),
         monto_comision      DECIMAL(10,2),
         comis_pagada        DECIMAL(10,2)

  END RECORD

OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60

    FORMAT
      ON EVERY ROW

      LET vcomis_pag100 = 0
 
      LET vcomis_pag100 = (g_reg1.comis_pagada * 100) / 12

      PRINT COLUMN 001,g_reg1.codven CLIPPED,
                       "|",g_reg1.contador CLIPPED, 
                       "|",g_reg1.coduni_n1 CLIPPED,
                       "|",g_reg1.salario_base_comis CLIPPED,
                       "|",g_reg1.num_sm CLIPPED,
                       "|",g_reg1.monto_comision CLIPPED,
                       "|",g_reg1.comis_pagada CLIPPED,
                       "|",vcomis_pag100  CLIPPED
                       
  END REPORT

  FUNCTION Ejecuta_Subdirector()

  DEFINE G_ARCHI1    CHAR(300)

  DEFINE g_reg1   RECORD
         codven              CHAR(10),
         contador            SMALLINT,
         coduni_n1           CHAR(10),
         salario_base_comis  DECIMAL(10,2),
         num_sm              DECIMAL(10,2),
         monto_comision      DECIMAL(10,2),
         comis_pagada        DECIMAL(10,2)

  END RECORD


  LET G_ARCHI1 = g_param.ruta_rescate CLIPPED,"/",usuario CLIPPED,
                              "_ade_sub.COM_",g_reg.fecha_corte USING "DDMMYYYY"

  -- PARA IMPRESION

  START REPORT rpt_cuenta_arc1 TO G_ARCHI1

       DECLARE c_22 CURSOR FOR
       SELECT codven,
              count(*),
              coduni_n1,
              sum(salario_base_comis),
              sum(num_sm),
              sum(monto_comision),
              sum(comis_pagada)
       FROM   com_comis_detalle
       WHERE  nivel = 3
       AND    fecha_corte = g_reg.fecha_corte
       GROUP  by 1,3


       FOREACH c_22 INTO g_reg1.*

          --PARA LA IMPRESION
          OUTPUT TO REPORT rpt_cuenta_arc1(g_reg1.*)

        END FOREACH

        FINISH REPORT rpt_cuenta_arc1
        ERROR "LISTADO 1 GENERADO...."
        SLEEP 2
        ERROR ""


  END FUNCTION

REPORT rpt_cuenta_arc1(g_reg1)

  DEFINE vcomis_pag100       DECIMAL(10,2)
  DEFINE g_reg1   RECORD

         codven              CHAR(10),
         contador            SMALLINT,
         coduni_n1           CHAR(10),
         salario_base_comis  DECIMAL(10,2),
         num_sm              DECIMAL(10,2),
         monto_comision      DECIMAL(10,2),
         comis_pagada        DECIMAL(10,2)

  END RECORD

OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60

    FORMAT
      ON EVERY ROW

      LET vcomis_pag100 = 0

      LET vcomis_pag100 = (g_reg1.comis_pagada * 100) / 4

      PRINT COLUMN 001,g_reg1.codven CLIPPED,
                       "|",g_reg1.contador CLIPPED,
                       "|",g_reg1.coduni_n1 CLIPPED,
                       "|",g_reg1.salario_base_comis CLIPPED,
                       "|",g_reg1.num_sm CLIPPED,
                       "|",g_reg1.monto_comision CLIPPED,
                       "|",g_reg1.comis_pagada CLIPPED,
                       "|",vcomis_pag100  CLIPPED

  END REPORT

  FUNCTION Ejecuta_Ejecutivo()


  DEFINE G_ARCHI2    CHAR(300)

  DEFINE g_reg1   RECORD
         codven              CHAR(10),
         contador            SMALLINT,
         coduni_n1           CHAR(10),
         salario_base_comis  DECIMAL(10,2),
         num_sm              DECIMAL(10,2),
         monto_comision      DECIMAL(10,2),
         comis_pagada        DECIMAL(10,2)

  END RECORD


  LET G_ARCHI = g_param.ruta_rescate CLIPPED,"/",usuario CLIPPED,
                              "_ade_eje.COM_",g_reg.fecha_corte USING "DDMMYYYY"

  -- PARA IMPRESION
  START REPORT rpt_cuenta_arc2 TO G_ARCHI

       DECLARE c_23 CURSOR FOR
       SELECT codven,
              count(*),
              coduni_n1,
              sum(salario_base_comis),
              sum(num_sm),
              sum(monto_comision),
              sum(comis_pagada)
       FROM   com_comis_detalle
       WHERE  nivel = 1 
       AND    fecha_corte = g_reg.fecha_corte
       GROUP  by 1,3


       FOREACH c_23 INTO g_reg1.*

          --PARA LA IMPRESION
          OUTPUT TO REPORT rpt_cuenta_arc2(g_reg1.*)

        END FOREACH

        FINISH REPORT rpt_cuenta_arc2
        ERROR "LISTADO 1 GENERADO...."
        SLEEP 2
        ERROR ""


END FUNCTION

REPORT rpt_cuenta_arc2(g_reg1)

  DEFINE g_reg1   RECORD

         codven              CHAR(10),
         contador            SMALLINT,
         coduni_n1           CHAR(10),
         salario_base_comis  DECIMAL(10,2),
         num_sm              DECIMAL(10,2),
         monto_comision      DECIMAL(10,2),
         comis_pagada        DECIMAL(10,2)

  END RECORD

OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60

    FORMAT
      ON EVERY ROW

      PRINT COLUMN 001,g_reg1.codven CLIPPED,
                       "|",g_reg1.contador CLIPPED,
                       "|",g_reg1.coduni_n1 CLIPPED,
                       "|",g_reg1.salario_base_comis CLIPPED,
                       "|",g_reg1.num_sm CLIPPED,
                       "|",g_reg1.monto_comision CLIPPED,
                       "|",g_reg1.comis_pagada CLIPPED

  END REPORT
