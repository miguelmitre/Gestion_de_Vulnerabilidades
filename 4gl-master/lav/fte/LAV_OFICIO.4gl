################################################################################
#Project           => SAFRE (Mexico)
#Owner             => E.F.P.
#Programa          => Oficio de lavado de dinero
#Fecha creacion    => 11 de diciembre de 2007
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ
#Sistema           => LAV
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE reg RECORD
          fecha_conversion DATE,
          nss              CHAR(11),
          monto_en_pesos   DECIMAL(16,6),
          tipo_movimiento  SMALLINT
   END RECORD

   DEFINE hoy              DATE

   DEFINE usuario         CHAR(20),
          vruta_envio     CHAR(50),
          g_impre         CHAR(100),
          txt_pdq         CHAR(100)
END GLOBALS
###############################################################################
MAIN
   CALL STARTLOG("LAV_OFICIO.log")

   LET hoy = TODAY

   CALL calculo()
END MAIN
###############################################################################
FUNCTION calculo ()

   DEFINE pos SMALLINT

   SELECT USER,
          ruta_envio
   INTO   usuario,
          vruta_envio
   FROM   seg_modulo
   WHERE  modulo_cod = "lav"

   LET g_impre = vruta_envio CLIPPED, "/",
                 usuario CLIPPED,".LAV_OFICIO",
                 ".",hoy USING "dd-mm-yyyy"

   LET txt_pdq = "SET PDQPRIORITY HIGH"
   PREPARE pdq_1 FROM txt_pdq

   EXECUTE pdq_1

   DISPLAY "PROCESANDO INFORMACION "
   START REPORT listado TO g_impre

   DECLARE cursor_1 CURSOR FOR
   SELECT a.fecha_conversion,
          a.nss,
          a.monto_en_pesos,
          a.tipo_movimiento
   FROM  dis_cuenta a
   WHERE a.fecha_conversion BETWEEN "01/01/2006" AND "11/30/2007"
   AND   a.subcuenta IN (3,10,11,12)
   AND   a.tipo_movimiento IN (1,490)
   AND   a.id_aportante[1,3] NOT IN ('MQ-','MC-','UM-','UQ-','MA-','MS-','AF-','AC-','UR-','US-','AA-','AS-','UY-','UZ-','COR')
   ORDER BY 2,1

   LET pos = 0
   FOREACH cursor_1 INTO reg.*
      LET pos = pos + 1
      OUTPUT TO REPORT listado(reg.*) 
   END FOREACH

   FINISH REPORT listado
   DISPLAY "ARCHIVO GENERADO...",g_impre
   DISPLAY "total registros:",pos
END FUNCTION
###############################################################################
REPORT listado(reg_1)
   DEFINE reg_1 RECORD
          fecha_conversion   DATE,
          nss                CHAR(11),
          monto_en_pesos     DECIMAL(16,6),
          tipo_movimiento    SMALLINT
   END RECORD

   DEFINE xtipo_movimiento CHAR(8),
          xtipo_cambio        DECIMAL(16,6)

   OUTPUT                         
      LEFT MARGIN   0             
      RIGHT MARGIN  0             
      TOP MARGIN    0             
      BOTTOM MARGIN 0             
      PAGE LENGTH   1  
                                  
   FORMAT                         
   ON EVERY ROW
      CASE reg_1.tipo_movimiento
         WHEN 1
            LET xtipo_movimiento = "DEPOSITO" 
         WHEN 490 
            LET xtipo_movimiento = "RETIRO"
      END CASE

      SELECT tipo_cambio
      INTO   xtipo_cambio
      FROM   tab_tipo_cambio
      WHERE  fecha_aplica = reg_1.fecha_conversion

      PRINT reg_1.fecha_conversion USING "YYYYMMDD", ASCII 009,
            reg_1.nss, ASCII 009,
            reg_1.monto_en_pesos USING "&&&&&&&&&&.&&&&&&", ASCII 009,
            xtipo_cambio        USING "&&&&&&&&&&.&&&&&&", ASCII 009,
            xtipo_movimiento
END REPORT





