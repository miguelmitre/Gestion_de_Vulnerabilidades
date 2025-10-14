################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#Owner             => E.F.P.                                                   #
#Programa COMB045  => Generacion de Interfaz de SAFRE-ADAM                     #
#Fecha             => 01 Feb   2005.                                           #
#By                => JOSE ALEJANDRO RAMIREZ.                                  #
#Sistema           => COM.                                                     #
################################################################################
DATABASE safre_af

GLOBALS 
  DEFINE hoy     DATE
  DEFINE g_param RECORD LIKE seg_modulo.*
  DEFINE usuario CHAR(08)
  DEFINE opc     CHAR(01)
  DEFINE g_reg   RECORD
         fecha_corte        DATE,
         fecha_pago         DATE
  END RECORD,
  vsecuencia     INTEGER

  DEFINE borra_lineas  CHAR(300)
  DEFINE num_reg   INTEGER
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

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "COMB0451" ATTRIBUTE(BORDER)

   DISPLAY " COMB045                INTERFAZ DE SAFRE-ADAM                                  " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   DISPLAY "                     GENERACION DE INTERFAZ AL CORTE                                " AT 6,1 ATTRIBUTE(REVERSE)

  CALL Carga()

  LET borra_lineas = g_param.ruta_rescate CLIPPED
  LET borra_lineas = "cd ",borra_lineas CLIPPED,";sed -e '/^$/d' ",
                  g_param.ruta_rescate CLIPPED,"/",
                  usuario CLIPPED,".INTERFAZ_",hoy USING "DDMMYYYY"," > ",
                  borra_lineas CLIPPED , "/p1";

  RUN borra_lineas

  LET borra_lineas = "mv ",g_param.ruta_rescate CLIPPED,"/p1 ",
                  g_param.ruta_rescate CLIPPED,"/",
                  usuario CLIPPED,".INTERFAZ_",hoy USING "DDMMYYYY";
  
  RUN borra_lineas

  CLOSE WINDOW ventana1
END MAIN

FUNCTION Carga()

   INPUT BY NAME g_reg.*
      AFTER FIELD fecha_corte
         IF g_reg.fecha_corte IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD fecha_corte
         END IF

   AFTER FIELD fecha_pago
           IF g_reg.fecha_pago IS NULL THEN
              ERROR "ESTE CAMPO NO PUDE SER NULO"
              NEXT FIELD fecha_pago
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

   SELECT nivel,fecha_corte,codven,total_comision
   FROM   com_comis_resumen
   WHERE  fecha_corte =g_reg.fecha_corte 
   INTO   TEMP tt_datos

   LET num_reg = 0

   SELECT count(*)
   INTO   num_reg
   FROM   tt_datos

   ERROR "Numero de reg:",num_reg

   PROMPT "Desea ejecutar el proceso [S/N] ..." FOR opc

   IF opc MATCHES '[Ss]' THEN
      ERROR "Procedando informacion ..."

      CALL Ejecuta_lectura()

   END IF
   CLEAR FORM
   CLEAR SCREEN

END FUNCTION

FUNCTION Ejecuta_lectura()

  DEFINE G_ARCHI    CHAR(300)
  DEFINE vdivide    CHAR(1)
  DEFINE nuevo   RECORD
         nivel      SMALLINT,
         fecha_corte DATE,
         codven      LIKE com_comis_resumen.codven,
         total_comision LIKE com_comis_resumen.total_comision
  END RECORD 


  LET G_ARCHI = g_param.ruta_rescate CLIPPED,"/",usuario CLIPPED,
                              ".INTERFAZ_",g_reg.fecha_corte USING "DDMMYYYY"
    
  -- PARA IMPRESION
  START REPORT rpt_cuenta_arc TO G_ARCHI


    IF num_reg IS NOT NULL AND num_reg > 0 THEN 


       LET vsecuencia = 1
       DECLARE c_2 CURSOR FOR 
         SELECT * 
         FROM   tt_datos
       FOREACH c_2 INTO nuevo.nivel,nuevo.fecha_corte,nuevo.codven,
                     nuevo.total_comision 
  

          --PARA LA IMPRESION
          OUTPUT TO REPORT rpt_cuenta_arc(nuevo.*)


        END FOREACH
        FINISH REPORT rpt_cuenta_arc
        ERROR "LISTADO 1 GENERADO...."
        SLEEP 2
        ERROR ""

    END IF

END FUNCTION

FUNCTION reformatea(long, cadena)

DEFINE long ,i SMALLINT,
       cadena CHAR(10),
       cadena_limpia CHAR(10)


    LET cadena_limpia=' '
    FOR i = 1 to 10-long
        LET cadena_limpia[i] = ' ' 
    END FOR
    LET cadena_limpia = cadena_limpia[1,i-1]||cadena CLIPPED
 
RETURN cadena_limpia

END FUNCTION



REPORT rpt_cuenta_arc(nuevo)

  DEFINE
    nuevo RECORD
      nivel             SMALLINT,
      fecha_corte       DATE,
      codven            CHAR(10), 
      total_comision     LIKE com_comis_resumen.total_comision
    END RECORD,
    nueva CHAR(10),
    vcompa CHAR(3)


OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60

    FORMAT
      ON EVERY ROW

      IF LENGTH(nuevo.codven) < 10 then
         CALL reformatea(LENGTH(nuevo.codven),nuevo.codven) 
              RETURNING nueva
      ELSE
         LET nueva = nuevo.codven
      END IF

      CASE nuevo.nivel 
         WHEN 1 OR 2
                LET vcompa = "AFV"
         WHEN 3 
                LET vcompa = "AFS"
         WHEN 4 
                LET vcompa = "AFO"
         OTHERWISE LET vcompa = "   "
      END CASE 

      PRINT COLUMN 001,"'",vcompa CLIPPED,"',",
                       "'",nueva CLIPPED,"',",
                       999 USING "###",",",                    --por ver
                       "'",nuevo.fecha_corte USING 'YYYYMMDD',"',",
                       vsecuencia USING "####",",", 
                       "'COMISION',",
                       "'NULL',",
                       "'NULL',",
                       "'NULL',",
                       nuevo.total_comision USING "#######&.&&",
                       ",'NULL',",
                       "'NULL',",
                       "'NULL',",
                       1 USING "#"
                       

      LET vsecuencia = vsecuencia + 1
               
  END REPORT
       --   COLUMN 127,nuevo.fecha_corte  USING 'DD/MM/YYYY',   "|",
{
------------------------------ VISTA ------------------------------------------------

create view hhh AS
  SELECT codven,
         nivel,
         cod_tipo_prom,
         coduni_n1,
         nss
  FROM com_comis_det
  WHERE nss in ('72836409192','68957511493')


drop view  hhh

}
