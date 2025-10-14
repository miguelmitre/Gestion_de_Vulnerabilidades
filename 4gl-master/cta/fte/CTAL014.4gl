
#Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
#Owner             => E.F.P.                                                   #
#Programa PENC914  => Principal generación de saldos información por edad      #
#Fecha creacion    => 17 Ago 2009                                              #
#By                => JGHM                                                     #
#                  ->                                                          #
################################################################################
DATABASE safre_af 

GLOBALS   "CTAL014g.4gl"

MAIN

   DEFINE   ld_fecha_inicio           DATE
   DEFINE   ld_FechaSaldo             DATE
   DEFINE   ld_FechaHoy               DATE
   DEFINE   ld_fecha_paso             CHAR(10)
   DEFINE   lc_Resp                   CHAR(01)
   DEFINE   lc_Mens                   CHAR(70)
   DEFINE   li_ExisteTmp              INTEGER 

   CALL STARTLOG("CTAL014.log")

   DEFER INTERRUPT
   OPTIONS INPUT WRAP         ,
           PROMPT LINE LAST   ,
           ACCEPT KEY CONTROL-I

   ### Como criterio se establece la fecha de inicio es siempre 30 de junio del año
   LET  gd_fecha_corte     = "06/30/"||YEAR(TODAY) CLIPPED
   LET  ld_FechaHoy        = TODAY
   LET  gs_prepara         = 0
   LET  gc_RutaRep         = "/safre_prc/cta/envio/"

   OPEN WINDOW f_01 AT 2,2 WITH FORM "CTAL01401"                                                          ATTRIBUTE(BORDER)
   DISPLAY "CTAL01401                    SALDOS POR EDAD                                       " AT 2,3   ATTRIBUTE(NORMAL)
   DISPLAY ld_FechaHoy                                                        USING "DD/MM/YYYY" AT 2,67  ATTRIBUTE(REVERSE)

   MENU "EDAD"
      COMMAND "Genera Información " "Genera Información para Reporte de Identificación por Edad"
         LET  ld_FechaSaldo         =  f_VerFechaSaldo()     
         IF   ld_FechaSaldo         IS NOT  NULL   THEN 
              LET  lc_Mens               = "Existe Saldo con Fecha :     ", ld_FechaSaldo USING "dd/mm/yyyy"
              DISPLAY   lc_Mens                    TO    lc_Mens
              PROMPT  " Desea Actualizar la Fecha de Saldo (S/N)" FOR CHAR lc_Resp 
         END IF
         IF   lc_Resp           =  "S"
          OR  lc_Resp           =  "s" 
          OR  ld_FechaSaldo     IS NULL     THEN
              LET  lc_Mens               = "Ingrese la Fecha de Saldo (ddmmyyyy) "     
              DISPLAY   lc_Mens                    TO    lc_Mens
              INPUT BY NAME  ld_FechaSaldo
                      AFTER FIELD ld_FechaSaldo 
                          IF  ld_FechaSaldo   IS NULL THEN
                              ERROR " Capture una fecha valida .......  "
                              SLEEP 2
                          ELSE
                              EXIT INPUT
                          END IF

                      AFTER INPUT 
                          IF  ld_FechaSaldo   IS NULL THEN  
                              ERROR " Capture una fecha valida .......  " 
                              SLEEP 2
                          ELSE
                              EXIT INPUT 
                          END IF
              END INPUT

              CALL f_GeneraSaldo(ld_FechaSaldo)  
              LET  ld_FechaSaldo         =  f_VerFechaSaldo()
              LET  lc_Mens               = "Se Genero Información con Fecha : ", ld_FechaSaldo USING "dd/mm/yyyy"
              DISPLAY   lc_Mens                    TO    lc_Mens
         END IF
      COMMAND "SIEFORE x Edad         " "Generación de Información de Trabajadores por SIEFORE y por Edad    "
         ERROR " Espere un momento, procesando información ..... "
         LET  li_ExisteTmp               = f_VerExisteTmp() 
         IF   li_ExisteTmp               = 0  THEN 
              CALL   f_GeneraTmp() 
         END IF  
         ERROR " Proceso Terminado ..................               "
         CALL   f_GeneraTrSi()
         ERROR "                              "
      COMMAND "Acciones X SIEFORE     " "Generación de Información de Acciones por SIEFORE   "
         ERROR " Espere un momento, procesando información ..... "
         LET  li_ExisteTmp               = f_VerExisteTmp()
         IF   li_ExisteTmp               = 0  THEN
              CALL   f_GeneraTmp()
         END IF
         CALL   f_GeneraAcSi()
         ERROR "                              "
      COMMAND "Acciones X Subcuenta   " "Generación de Información de Acciones por SIEFORE y por Subcuenta  "
         ERROR " Espere un momento, procesando información ..... "
         LET  li_ExisteTmp               = f_VerExisteTmp()
         IF   li_ExisteTmp               = 0  THEN
              CALL   f_GeneraTmp()
         END IF
         CALL   f_GeneraAcSu()
         ERROR "                              "
      COMMAND "Detalle X Trabajadores " "Información a Detalle de Trabajadores                          "
         ERROR " Espere un momento, procesando información ..... "
         LET  li_ExisteTmp               = f_VerExisteTmp()
         IF   li_ExisteTmp               = 0  THEN
              CALL   f_GeneraTmp()
         END IF
         CALL   f_GeneraDtTr()
         ERROR " Proceso Terminado ..................               "
      COMMAND "Salida           " "Salida del programa"
         EXIT MENU
   END MENU
   CLOSE WINDOW f_01
END MAIN


### Función de primer prepare, procedimiento de creación de saldos
FUNCTION f_prepara01()
   DEFINE    lc_query                       CHAR(2000)

   LET lc_query  = " EXECUTE PROCEDURE sp_saldo_edad ( ? ) "
   PREPARE p_SdoEdad                FROM     lc_query

END FUNCTION


### Función de siguiente prepares, Query's de reportes  
FUNCTION f_prepara03()
  DEFINE    lc_query                       CHAR(2000)

  IF        gs_prepara  = 0 THEN  
    ### Obtener información de trabs x edad                     
    LET lc_query          =  " SELECT  '2', edad, COUNT(*)         ",
                             "   FROM  safre_tmp:xxedad            ",
                             "  WHERE  fecha_corte = ?             ",
                             "    AND  edad >= 56                  ",
                             "  GROUP  BY 1, 2                     ",
                             " UNION                               ",
                             " SELECT  '3', edad, COUNT(*)         ",
                             "   FROM  safre_tmp:xxedad            ",
                             "  WHERE  fecha_corte = ?             ",
                             "    AND  edad BETWEEN 46 AND 55      ",
                             "  GROUP  BY 1, 2                     ",
                             "  UNION                              ",
                             " SELECT  '4', edad, COUNT(*)         ",
                             "   FROM  safre_tmp:xxedad            ",
                             "  WHERE  fecha_corte = ?             ",
                             "    AND  edad BETWEEN 37 AND 45      ",
                             "  GROUP  BY 1, 2                     ",
                             "  UNION                              ",
                             " SELECT  '5', edad, COUNT(*)         ",
                             "   FROM  safre_tmp:xxedad            ",
                             "  WHERE  fecha_corte = ?             ",
                             "    AND  edad BETWEEN 27 AND 36      ",
                             "  GROUP  BY 1, 2                     ",
                             "  ORDER  BY 1, 2                     "
    PREPARE p_SelTrSi            FROM  lc_query
    DECLARE d_SelTrSi          CURSOR FOR  p_SelTrSi

    LET     lc_query     =   " SELECT  '5', SUM(monto_en_acciones)                ",
                             "   FROM  safre_tmp:tmp_saldo_edad                   ",
                             "  WHERE  nss IN ( SELECT  nss                       ",
                             "                    FROM  safre_tmp:xxedad          ",
                             "                   WHERE  fecha_corte = ?           ",
                             "                     AND  edad BETWEEN 27  AND 36 ) ",
                             "  UNION                                             ",
                             " SELECT  '4', SUM(monto_en_acciones)                ",
                             "   FROM  safre_tmp:tmp_saldo_edad                   ",
                             "  WHERE  nss IN ( SELECT  nss                       ",
                             "                    FROM  xxedad                    ",
                             "                   WHERE  fecha_corte = ?           ",
                             "                     AND  edad BETWEEN 37 AND  45 ) ",
                             "  UNION                                             ",
                             " SELECT  '3', SUM(monto_en_acciones)                ",
                             "   FROM  safre_tmp:tmp_saldo_edad                   ",
                             "  WHERE  nss IN ( SELECT  nss                       ",
                             "                    FROM  xxedad                    ",
                             "                   WHERE  fecha_corte = ?           ",
                             "                     AND  edad BETWEEN 46 AND  55 ) ",
                             "  UNION                                             ",
                             " SELECT  '2', SUM(monto_en_acciones)                ",
                             "   FROM  safre_tmp:tmp_saldo_edad                   ",
                             "  WHERE  nss IN ( SELECT  nss                       ",
                             "                    FROM  xxedad                    ",
                             "                   WHERE  fecha_corte = ?           ",
                             "                     AND  edad >= 56 )              "
    PREPARE p_SelAxSi            FROM  lc_query
    DECLARE d_SelAxSi          CURSOR FOR  p_SelAxSi

    LET     lc_query     =   " SELECT  '5', subcuenta, SUM(monto_en_acciones)     ",
                             "   FROM  safre_tmp:tmp_saldo_edad                   ",
                             "  WHERE  nss IN ( SELECT  nss                       ",
                             "                    FROM  safre_tmp:xxedad          ",
                             "                   WHERE  fecha_corte = ?           ",
                             "                     AND  edad BETWEEN 27  AND 36 ) ",
                             "  GROUP BY 1, 2                                     ",
                             "  UNION                                             ",
                             " SELECT  '4', subcuenta, SUM(monto_en_acciones)     ",
                             "   FROM  safre_tmp:tmp_saldo_edad                   ",
                             "  WHERE  nss IN ( SELECT  nss                       ",
                             "                    FROM  xxedad                    ",
                             "                   WHERE  fecha_corte = ?           ",
                             "                     AND  edad BETWEEN 37 AND  45 ) ",
                             "  GROUP BY 1, 2                                     ",
                             "  UNION                                             ",
                             " SELECT  '3', subcuenta, SUM(monto_en_acciones)     ",
                             "   FROM  safre_tmp:tmp_saldo_edad                   ",
                             "  WHERE  nss IN ( SELECT  nss                       ",
                             "                    FROM  xxedad                    ",
                             "                   WHERE  fecha_corte = ?           ",
                             "                     AND  edad BETWEEN 46 AND  55 ) ",
                             "  GROUP BY 1, 2                                     ",
                             "  UNION                                             ",
                             " SELECT  '2', subcuenta, SUM(monto_en_acciones)     ",
                             "   FROM  safre_tmp:tmp_saldo_edad                   ",
                             "  WHERE  nss IN ( SELECT  nss                       ",
                             "                    FROM  xxedad                    ",
                             "                   WHERE  fecha_corte = ?           ",
                             "                     AND  edad >= 56 )              ",
                             "  GROUP BY 1, 2                                     " 
    PREPARE p_SelAxSu            FROM  lc_query
    DECLARE d_SelAxSu          CURSOR FOR  p_SelAxSu

    LET     lc_query      =  "  SELECT  nss, SUM(monto_en_acciones)               ",
                             "    FROM  safre_tmp:tmp_saldo_edad                  ",
                             "   WHERE  nss IN( SELECT  nss                       ",
                             "                    FROM  xxedad                    ",
                             "                   WHERE  fecha_corte = ?           ",
                             "                     AND  edad between 27 AND 36 )  ",
                             "   GROUP  BY 1                                      ",
                             "   ORDER  BY 1                                      "
    PREPARE p_SelAxD5            FROM  lc_query
    DECLARE d_SelAxD5          CURSOR FOR  p_SelAxD5

    LET     lc_query      =  "  SELECT  nss, SUM(monto_en_acciones)               ",
                             "    FROM  safre_tmp:tmp_saldo_edad                  ",
                             "   WHERE  nss IN( SELECT  nss                       ",
                             "                    FROM  xxedad                    ",
                             "                   WHERE  fecha_corte = ?           ",
                             "                     AND  edad between 37 AND 45 )  ",
                             "   GROUP  BY 1                                      ",
                             "   ORDER  BY 1                                      "
    PREPARE p_SelAxD4            FROM  lc_query
    DECLARE d_SelAxD4          CURSOR FOR  p_SelAxD4

    LET     lc_query      =  "  SELECT  nss, SUM(monto_en_acciones)               ",
                             "    FROM  safre_tmp:tmp_saldo_edad                  ",
                             "   WHERE  nss IN( SELECT  nss                       ",
                             "                    FROM  xxedad                    ",
                             "                   WHERE  fecha_corte = ?           ",
                             "                     AND  edad between 46 AND 55 )  ",
                             "   GROUP  BY 1                                      ",
                             "   ORDER  BY 1                                      "
    PREPARE p_SelAxD3            FROM  lc_query
    DECLARE d_SelAxD3          CURSOR FOR  p_SelAxD3

    LET     lc_query      =  "  SELECT  nss, SUM(monto_en_acciones)               ",
                             "    FROM  safre_tmp:tmp_saldo_edad                  ",
                             "   WHERE  nss IN( SELECT  nss                       ",
                             "                    FROM  xxedad                    ",
                             "                   WHERE  fecha_corte = ?           ",
                             "                     AND  edad >= 56 )              ",
                             "   GROUP  BY 1                                      ",
                             "   ORDER  BY 1                                      "
    PREPARE p_SelAxD2            FROM  lc_query
    DECLARE d_SelAxD2          CURSOR FOR  p_SelAxD2

    LET     gs_prepara = 1
  END IF
END FUNCTION 


### Revisa la fecha que tiene tmp_saldo_edad
FUNCTION f_VerFechaSaldo()
  DEFINE  ld_fecha_conversion           DATE
  DEFINE  lc_query                      CHAR(500)

  WHENEVER ERROR CONTINUE 
  LET lc_query   =  " SELECT fecha_conversion             ",
                    "   FROM safre_tmp:tmp_saldo_edad     "
  PREPARE p_SelTmp                 FROM      lc_query
  DECLARE d_SelTmp               CURSOR FOR  p_SelTmp

  FOREACH d_SelTmp            INTO      ld_fecha_conversion
      EXIT FOREACH
  END FOREACH
  WHENEVER ERROR STOP

  IF       YEAR(ld_fecha_conversion) = 1899  THEN
      LET ld_fecha_conversion = ""
  END IF
 
  RETURN   ld_fecha_conversion
END FUNCTION
       
       
### Genera nueva fecha en tmp_saldo_edad a peticion 
FUNCTION f_GeneraSaldo(ld_FechaProceso)
  DEFINE ls_fin                      SMALLINT
  DEFINE ld_FechaProceso             DATE 
  DEFINE ls_resul                    SMALLINT
    
  ### Ejecuta el borrado y creación de la tabla donde se generan saldos 

  DATABASE  safre_tmp
  CALL f_prepara01()
  WHENEVER ERROR CONTINUE 
  DROP   TABLE tmp_saldo_edad
  CREATE TABLE tmp_saldo_edad
  (
    nss                              CHAR(11),
    subcuenta                        SMALLINT,
    siefore                          SMALLINT,
    fecha_conversion                 DATE,
    monto_en_acciones                DECIMAL(22,6),
    monto_en_pesos                   DECIMAL(22,6)
  )
  WHENEVER ERROR STOP 

  ### Ejecuta procedimiento para cargar la tabla de saldos edad

  EXECUTE  p_SdoEdad                 USING ld_FechaProceso
  DATABASE  safre_af 

END FUNCTION  


### Verifica que exista temporal para reportes
FUNCTION f_VerExisteTmp()
  DEFINE   li_existe                 INTEGER
  DEFINE   lc_query                  CHAR(2000)

  LET      li_existe   = 0 
  WHENEVER ERROR CONTINUE 
  LET  lc_query         =  " SELECT  COUNT(*)                    ",
                           "   FROM  safre_tmp:xxedad            "
  PREPARE p_SelTemp            FROM  lc_query
  EXECUTE  p_SelTemp                 INTO  li_existe
  WHENEVER ERROR STOP
   
  IF       li_existe   IS NULL   THEN 
      LET  li_existe   = 0 
  END IF 

  RETURN   li_existe  
END FUNCTION
 

### No existe temporal de reportes, Genera con algunas condiciones 
FUNCTION f_GeneraTmp()
  DEFINE   ld_FechaSaldo             DATE


  LET       ld_FechaSaldo          =  f_VerFechaSaldo()     
  DATABASE safre_tmp
  SELECT  *
    FROM  safre_af:cta_transf_edad
   WHERE  fecha_corte = gd_fecha_corte
    INTO  TEMP xxedad 
 
  DELETE FROM  xxedad
   WHERE  nss IN ( SELECT  nss 
                     FROM  safre_af:cta_det_sie_acep 
                    WHERE  diagnostico1 = '000' )
 
  DELETE FROM  xxedad 
   WHERE  nss IN ( SELECT  n_seguro
                     FROM  safre_af:taa_cd_det_cedido
                    WHERE  fecha_trasp BETWEEN gd_fecha_corte
                                           AND ld_FechaSaldo 
                      AND  estado = 103 ) 

  DELETE FROM  xxedad 
   WHERE  nss IN ( SELECT  nss
                     FROM  safre_af:cta_act_marca        
                    WHERE  marca_cod = 150 
                      AND  fecha_ini BETWEEN  gd_fecha_corte
                                         AND  ld_FechaSaldo )

  DELETE FROM  xxedad
   WHERE  nss IN ( SELECT  nss_cta1 
                     FROM  safre_af:uni_unificado 
                    WHERE  estado = 100
                      AND  fnotifica BETWEEN  gd_fecha_corte
                                         AND  ld_FechaSaldo )
  DATABASE safre_af

END FUNCTION


### Obtiene el reporte trabajadores por edad
FUNCTION f_GeneraTrSi()
  DEFINE      la_acum           ARRAY[999]  OF RECORD        
              siefore           SMALLINT,
              edad              SMALLINT,
              trabs             INTEGER
              END RECORD
  DEFINE      li_i              SMALLINT
  DEFINE      li_j              SMALLINT
  DEFINE      li_k              SMALLINT
  DEFINE      ld_FechaHoy       DATE
  DEFINE      ls_siefore        SMALLINT
  DEFINE      ln_subtotal       INTEGER
  DEFINE      ln_total          INTEGER
  DEFINE      li_cuantos        INTEGER
  DEFINE      lc_NomRep         CHAR(200)
  DEFINE      lc_titulo1        CHAR(130)
  DEFINE      lc_titulo2        CHAR(130)
  DEFINE      lc_titulo3        CHAR(130)
  DEFINE      lc_totales        CHAR(130)
  DEFINE      lc_linbla         CHAR(130)
  DEFINE      lc_linea          CHAR(130)
  DEFINE      lc_lineb          CHAR(130)
  DEFINE      ln_lon1           SMALLINT
  DEFINE      ln_lon2           SMALLINT
  DEFINE      ln_lon3           SMALLINT
  DEFINE      ln_lon4           SMALLINT

  CALL  f_prepara03() 
  FOR  li_i  = 1 TO 999 
      INITIALIZE  la_acum[li_i].*  TO NULL 
  END FOR 

  LET        ld_FechaHoy        =     TODAY
  LET        li_i               =     1
  LET        ln_total           =     0
  FOREACH    d_SelTrSi          USING gd_fecha_corte,
                                      gd_fecha_corte,
                                      gd_fecha_corte,
                                      gd_fecha_corte
                                INTO  la_acum[li_i].*
      IF     la_acum[li_i].trabs   IS NULL THEN
             LET  la_acum[li_i].trabs  = 0
      END IF
      LET    ln_total           =     ln_total + la_acum[li_i].trabs
      LET    li_i               =     li_i + 1
  END FOREACH

  CALL SET_COUNT(li_i - 1)
  LET  li_cuantos               =     li_i
  OPEN WINDOW f_02 AT 2,2 WITH FORM "CTAL01402"                                                          ATTRIBUTE(BORDER)
  DISPLAY "[Ctrl-B] Genera Reporte"                                                             AT 1,3   ATTRIBUTE(REVERSE)
  DISPLAY "[Ctrl-C] Salir"                                                                      AT 1,27  ATTRIBUTE(REVERSE)
  DISPLAY "CTAL01402                    TRABAJADORES POR EDAD                                 " AT 2,3   ATTRIBUTE(NORMAL)
  DISPLAY ld_FechaHoy                                                        USING "DD/MM/YYYY" AT 2,65  ATTRIBUTE(NORMAL)
  DISPLAY "SIEFORE  EDAD             TRABAJADORES"                                              AT 5,23  ATTRIBUTE(REVERSE)

  INPUT ARRAY  la_acum   WITHOUT DEFAULTS FROM l_reg.*
       BEFORE FIELD siefore 
          LET li_j          = ARR_CURR()
          LET ls_siefore    = la_acum[li_j].siefore
          LET ln_subtotal   = 0
          FOR li_k = 1 TO li_cuantos
              IF   la_acum[li_k].siefore = ls_siefore  THEN 
                   LET  ln_subtotal = ln_subtotal + la_acum[li_k].trabs 
              END IF
              IF   la_acum[li_k].siefore IS NULL THEN
                   EXIT FOR
              END IF
          END FOR
          DISPLAY ls_siefore          TO   subsie   
          DISPLAY ln_subtotal         TO   subtotal
          DISPLAY ln_total            TO   total

       ON KEY (CONTROL-B)  
          ERROR   "Espere un momento generando reporte .. " 
          LET      lc_NomRep  =  gc_RutaRep CLIPPED||'TraXEdad'||
                                 DAY(gd_fecha_corte) ||MONTH(gd_fecha_corte) ||YEAR(gd_fecha_corte)||'-'||
                                 DAY(ld_FechaHoy)  ||MONTH(ld_FechaHoy) ||YEAR(ld_FechaHoy)||'.rep'
          LET      lc_NomRep  =  lc_NomRep CLIPPED 
          LET      lc_titulo1 = "TRABAJADORES POR EDAD                                   FECHA DE IDENTIFICACION "||
                                 DAY(gd_fecha_corte)||'/'||MONTH(gd_fecha_corte)||'/'||YEAR(gd_fecha_corte)
          LET      lc_titulo2 = "SAFRE  "||CURRENT
          LET      lc_titulo3 = "            SIEFORE          EDAD                TRABAJADORES                    SUBTOTAL"
          LET      lc_linbla  = "  "

          START  REPORT f_Report TO lc_NomRep
 
             ### Armar titulo y enviarlo a reporte 
             OUTPUT TO REPORT f_Report(lc_titulo1)
             OUTPUT TO REPORT f_Report(lc_linbla)
             OUTPUT TO REPORT f_Report(lc_titulo3)
             LET       ln_subtotal  = 0
             FOR li_k = 1 TO li_cuantos

                 ### Armar detalle 
                 LET   ln_lon1    = LENGTH(la_acum[li_k].siefore)
                 LET   ln_lon2    = LENGTH(la_acum[li_k].edad)
                 LET   ln_lon3    = LENGTH(la_acum[li_k].trabs)
                 LET   ln_lon4    = LENGTH(ln_subtotal)
                 IF    la_acum[li_k + 1].siefore = la_acum[li_k].siefore THEN
                       LET   ln_subtotal  = ln_subtotal + la_acum[li_k].trabs
                       LET   lc_linea = lc_lineb[1, (10 - ln_lon1)], la_acum[li_k].siefore,
                                        lc_lineb[1, (10 - ln_lon2)], la_acum[li_k].edad,
                                        lc_lineb[1, (17 - ln_lon3)], la_acum[li_k].trabs
                 ELSE
                       LET   ln_subtotal  = ln_subtotal + la_acum[li_k].trabs
                       LET   lc_linea = lc_lineb[1, (10 - ln_lon1)], la_acum[li_k].siefore,
                                        lc_lineb[1, (10 - ln_lon2)], la_acum[li_k].edad,
                                        lc_lineb[1, (17 - ln_lon3)], la_acum[li_k].trabs,
                                        lc_lineb[1, (17 - ln_lon4)], ln_subtotal 
                       LET   ln_subtotal  = 0
                 END IF
                 OUTPUT TO REPORT f_Report(lc_linea)
             END FOR

             ### Armar totales 
             OUTPUT TO REPORT f_Report(lc_linbla)
             LET      ln_lon4    = LENGTH(ln_total)
             LET      lc_titulo3 = "                                                      TOTAL ",
                                   lc_lineb[1, (17 - ln_lon4)], ln_total
             OUTPUT TO REPORT f_Report(lc_titulo3)
             OUTPUT TO REPORT f_Report(lc_linbla)
             OUTPUT TO REPORT f_Report(lc_titulo2)
          FINISH REPORT f_Report 
          ERROR   "Terminado ", lc_NomRep

       ON KEY (CONTROL-C)
          EXIT INPUT
  END INPUT
  CLOSE WINDOW f_02
END FUNCTION


### Función general imprime una linea en el reporte que sea
REPORT f_Report(lc_DetReg)

  DEFINE   lc_DetReg                  CHAR(130)     --- Registro a tratar

  OUTPUT
     PAGE LENGTH 1
     LEFT MARGIN 0
     RIGHT MARGIN 0
     TOP MARGIN 0
     BOTTOM MARGIN 0

  FORMAT
      ON EVERY ROW
         PRINT COLUMN 001, lc_DetReg 

END REPORT

### Obtiene el reporte de acciones por siefore
FUNCTION f_GeneraAcSi()
  DEFINE      la_acu1           ARRAY[999]  OF RECORD
              siefore           SMALLINT,
              acciones          DECIMAL(22,6) 
              END RECORD
  DEFINE      li_i              SMALLINT
  DEFINE      li_j              SMALLINT
  DEFINE      li_k              SMALLINT
  DEFINE      ld_FechaHoy       DATE
  DEFINE      ls_siefore        SMALLINT
  DEFINE      ln_subtotal       DECIMAL(22,6)
  DEFINE      ln_total          DECIMAL(22,6)
  DEFINE      li_cuantos        INTEGER
  DEFINE      lc_NomRep         CHAR(200)
  DEFINE      lc_titulo1        CHAR(130)
  DEFINE      lc_titulo2        CHAR(130)
  DEFINE      lc_titulo3        CHAR(130)
  DEFINE      lc_totales        CHAR(130)
  DEFINE      lc_linbla         CHAR(130)
  DEFINE      lc_linea          CHAR(130)
  DEFINE      lc_lineb          CHAR(130)
  DEFINE      ln_lon1           SMALLINT
  DEFINE      ln_lon2           SMALLINT
  DEFINE      ln_lon3           SMALLINT
  DEFINE      ln_lon4           SMALLINT
  DEFINE      ld_FechaSaldo     DATE 

  CALL  f_prepara03()
  FOR  li_i  = 1 TO 999
      INITIALIZE  la_acu1[li_i].*  TO NULL
  END FOR

  LET        ld_FechaHoy        =     TODAY
  LET        li_i               =     1
  LET        ln_total           =     0
  FOREACH    d_SelAxSi          USING gd_fecha_corte,
                                      gd_fecha_corte,
                                      gd_fecha_corte,
                                      gd_fecha_corte
                                INTO  la_acu1[li_i].*
      IF     la_acu1[li_i].acciones   IS NULL THEN
             LET  la_acu1[li_i].acciones  = 0
      END IF
      LET    ln_total           =     ln_total + la_acu1[li_i].acciones
      LET    li_i               =     li_i + 1
  END FOREACH
  ERROR " Proceso Terminado ..................               "

  CALL SET_COUNT(li_i - 1)
  LET  li_cuantos               =     li_i
  OPEN WINDOW f_03 AT 2,2 WITH FORM "CTAL01403"                                                          ATTRIBUTE(BORDER)
  DISPLAY "[Ctrl-B] Genera Reporte"                                                             AT 1,3   ATTRIBUTE(REVERSE)
  DISPLAY "[Ctrl-C] Salir"                                                                      AT 1,27  ATTRIBUTE(REVERSE)
  DISPLAY "CTAL01403              VALOR DE ACCION POR SIEFORE                                 " AT 2,3   ATTRIBUTE(NORMAL)
  DISPLAY ld_FechaHoy                                                        USING "DD/MM/YYYY" AT 2,65  ATTRIBUTE(NORMAL)
  DISPLAY "SIEFORE                 ACCIONES"                                                    AT 5,23  ATTRIBUTE(REVERSE)

  INPUT ARRAY  la_acu1   WITHOUT DEFAULTS FROM l_reg1.*
       BEFORE FIELD siefore
          LET li_j          = ARR_CURR()
          LET ls_siefore    = la_acu1[li_j].siefore
          LET ln_subtotal   = 0
          FOR li_k = 1 TO li_cuantos
              IF   la_acu1[li_k].siefore = ls_siefore  THEN
                   LET  ln_subtotal = ln_subtotal + la_acu1[li_k].acciones
              END IF
              IF   la_acu1[li_k].siefore IS NULL THEN
                   EXIT FOR
              END IF
          END FOR
          DISPLAY ls_siefore          TO   subsie
          DISPLAY ln_subtotal         TO   subtotal
          DISPLAY ln_total            TO   total

       ON KEY (CONTROL-B)
          LET      ld_FechaSaldo         =  f_VerFechaSaldo()
          ERROR   "Espere un momento generando reporte .. "
          LET      lc_NomRep  =  gc_RutaRep CLIPPED||'AccXSief'||
                                 DAY(ld_FechaSaldo) ||MONTH(ld_FechaSaldo) ||YEAR(ld_FechaSaldo)||'-'||
                                 DAY(ld_FechaHoy)  ||MONTH(ld_FechaHoy) ||YEAR(ld_FechaHoy)||'.rep'
          LET      lc_NomRep  =  lc_NomRep CLIPPED
          LET      lc_titulo1 = "TOTAL DE ACCIONES POR SIEFORE                                    FECHA DE SALDO: "||
                                 DAY(ld_FechaSaldo)||'/'||MONTH(ld_FechaSaldo)||'/'||YEAR(ld_FechaSaldo)
          LET      lc_titulo2 = "SAFRE  "||CURRENT
          LET      lc_titulo3 = "            SIEFORE                       ACCIONES                           SUBTOTAL"
          LET      lc_linbla  = "  "

          START  REPORT f_Report TO lc_NomRep

             ### Armar titulo y enviarlo a reporte
             OUTPUT TO REPORT f_Report(lc_titulo1)
             OUTPUT TO REPORT f_Report(lc_linbla)
             OUTPUT TO REPORT f_Report(lc_titulo3)
             LET       ln_subtotal  = 0
             FOR li_k = 1 TO li_cuantos

                 ### Armar detalle
                 LET   ln_lon1    = LENGTH(la_acu1[li_k].siefore)
                 LET   ln_lon3    = LENGTH(la_acu1[li_k].acciones)
                 LET   ln_lon4    = LENGTH(ln_subtotal)
                 IF    la_acu1[li_k + 1].siefore = la_acu1[li_k].siefore THEN
                       LET   ln_subtotal  = ln_subtotal + la_acu1[li_k].acciones
                       LET   lc_linea = lc_lineb[1, (10 - ln_lon1)], la_acu1[li_k].siefore,
                                        lc_lineb[1, (10 - ln_lon3)], la_acu1[li_k].acciones
                 ELSE
                       LET   ln_subtotal  = ln_subtotal + la_acu1[li_k].acciones
                       LET   lc_linea = lc_lineb[1, (10 - ln_lon1)], la_acu1[li_k].siefore,
                                        lc_lineb[1, (10 - ln_lon3)], la_acu1[li_k].acciones,
                                        lc_lineb[1, (10 - ln_lon4)], ln_subtotal
                       LET   ln_subtotal  = 0
                 END IF
                 OUTPUT TO REPORT f_Report(lc_linea)
             END FOR

             ### Armar totales
             OUTPUT TO REPORT f_Report(lc_linbla)
             LET      ln_lon4    = LENGTH(ln_total)
             LET      lc_titulo3 = "                                       TOTAL ",
                                   lc_lineb[1, (17 - ln_lon4)], ln_total
             OUTPUT TO REPORT f_Report(lc_titulo3)
             OUTPUT TO REPORT f_Report(lc_linbla)
             OUTPUT TO REPORT f_Report(lc_titulo2)
          FINISH REPORT f_Report
          ERROR   "Terminado ", lc_NomRep

       ON KEY (CONTROL-C)
          EXIT INPUT
  END INPUT
  CLOSE WINDOW f_03

END FUNCTION 


### Obtiene el reporte de acciones por subcuenta 
FUNCTION f_GeneraAcSu()
  DEFINE      la_acu2           ARRAY[999]  OF RECORD
              siefore           SMALLINT,
              subcuenta         SMALLINT,
              acciones          DECIMAL(22,6)
              END RECORD
  DEFINE      li_i              SMALLINT
  DEFINE      li_j              SMALLINT
  DEFINE      li_k              SMALLINT
  DEFINE      ld_FechaHoy       DATE
  DEFINE      ls_siefore        SMALLINT
  DEFINE      ln_subtotal       DECIMAL(22,6)
  DEFINE      ln_total          DECIMAL(22,6)
  DEFINE      li_cuantos        INTEGER
  DEFINE      lc_NomRep         CHAR(200)
  DEFINE      lc_titulo1        CHAR(130)
  DEFINE      lc_titulo2        CHAR(130)
  DEFINE      lc_titulo3        CHAR(130)
  DEFINE      lc_totales        CHAR(130)
  DEFINE      lc_linbla         CHAR(130)
  DEFINE      lc_linea          CHAR(130)
  DEFINE      lc_lineb          CHAR(130)
  DEFINE      ln_lon1           SMALLINT
  DEFINE      ln_lon2           SMALLINT
  DEFINE      ln_lon3           SMALLINT
  DEFINE      ln_lon4           SMALLINT
  DEFINE      ld_FechaSaldo     DATE 

  CALL  f_prepara03()
  FOR  li_i  = 1 TO 999
      INITIALIZE  la_acu2[li_i].*  TO NULL
  END FOR

  LET        ld_FechaHoy        =     TODAY
  LET        li_i               =     1
  LET        ln_total           =     0
  FOREACH    d_SelAxSu          USING gd_fecha_corte,
                                      gd_fecha_corte,
                                      gd_fecha_corte,
                                      gd_fecha_corte
                                INTO  la_acu2[li_i].*
      IF     la_acu2[li_i].acciones   IS NULL THEN
             LET  la_acu2[li_i].acciones  = 0
      END IF
      LET    ln_total           =     ln_total + la_acu2[li_i].acciones
      LET    li_i               =     li_i + 1
  END FOREACH
  ERROR " Proceso Terminado ..................               "

  CALL SET_COUNT(li_i - 1)
  LET  li_cuantos               =     li_i
  OPEN WINDOW f_04 AT 2,2 WITH FORM "CTAL01404"                                                          ATTRIBUTE(BORDER)
  DISPLAY "[Ctrl-B] Genera Reporte"                                                             AT 1,3   ATTRIBUTE(REVERSE)
  DISPLAY "[Ctrl-C] Salir"                                                                      AT 1,27  ATTRIBUTE(REVERSE)
  DISPLAY "CTAL01404              VALOR DE ACCION POR SUBCUENTA                               " AT 2,3   ATTRIBUTE(NORMAL)
  DISPLAY ld_FechaHoy                                                        USING "DD/MM/YYYY" AT 2,65  ATTRIBUTE(NORMAL)
  DISPLAY "SIEFORE   SUBCUENTA           ACCIONES"                                              AT 5,23  ATTRIBUTE(REVERSE)

  INPUT ARRAY  la_acu2   WITHOUT DEFAULTS FROM l_reg2.*
       BEFORE FIELD siefore
          LET li_j          = ARR_CURR()
          LET ls_siefore    = la_acu2[li_j].siefore
          LET ln_subtotal   = 0
          FOR li_k = 1 TO li_cuantos
              IF   la_acu2[li_k].siefore = ls_siefore  THEN
                   LET  ln_subtotal = ln_subtotal + la_acu2[li_k].acciones
              END IF
              IF   la_acu2[li_k].siefore IS NULL THEN
                   EXIT FOR
              END IF
          END FOR
          DISPLAY ls_siefore          TO   subsie
          DISPLAY ln_subtotal         TO   subtotal
          DISPLAY ln_total            TO   total

       ON KEY (CONTROL-B)
          LET      ld_FechaSaldo         =  f_VerFechaSaldo()
          ERROR   "Espere un momento generando reporte .. "
          LET      lc_NomRep  =  gc_RutaRep CLIPPED||'AccXSbct'||
                                 DAY(ld_FechaSaldo) ||MONTH(ld_FechaSaldo) ||YEAR(ld_FechaSaldo)||'-'||
                                 DAY(ld_FechaHoy)  ||MONTH(ld_FechaHoy) ||YEAR(ld_FechaHoy)||'.rep'
          LET      lc_NomRep  =  lc_NomRep CLIPPED
          LET      lc_titulo1 = "TOTAL DE ACCIONES POR SIEFORE Y POR SUBCUENTA                    FECHA DE SALDO: "||
                                 DAY(ld_FechaSaldo)||'/'||MONTH(ld_FechaSaldo)||'/'||YEAR(ld_FechaSaldo)
          LET      lc_titulo2 = "SAFRE  "||CURRENT
          LET      lc_titulo3 = "            SIEFORE  SUBCUENTA                   ACCIONES                    SUBTOTAL"
          LET      lc_linbla  = "  "

          START  REPORT f_Report TO lc_NomRep

             ### Armar titulo y enviarlo a reporte
             OUTPUT TO REPORT f_Report(lc_titulo1)
             OUTPUT TO REPORT f_Report(lc_linbla)
             OUTPUT TO REPORT f_Report(lc_titulo3)
             LET       ln_subtotal  = 0
             FOR li_k = 1 TO li_cuantos

                 ### Armar detalle
                 LET   ln_lon1    = LENGTH(la_acu2[li_k].siefore)
                 LET   ln_lon2    = LENGTH(la_acu2[li_k].subcuenta)
                 LET   ln_lon3    = LENGTH(la_acu2[li_k].acciones)
                 LET   ln_lon4    = LENGTH(ln_subtotal)
                 IF    la_acu2[li_k + 1].siefore = la_acu2[li_k].siefore THEN
                       LET   ln_subtotal  = ln_subtotal + la_acu2[li_k].acciones
                       LET   lc_linea = lc_lineb[1, (10 - ln_lon1)], la_acu2[li_k].siefore,
                                        lc_lineb[1, ( 5 - ln_lon2)], la_acu2[li_k].subcuenta,
                                        lc_lineb[1, ( 5 - ln_lon3)], la_acu2[li_k].acciones
                 ELSE
                       LET   ln_subtotal  = ln_subtotal + la_acu2[li_k].acciones
                       LET   lc_linea = lc_lineb[1, (10 - ln_lon1)], la_acu2[li_k].siefore,
                                        lc_lineb[1, ( 5 - ln_lon2)], la_acu2[li_k].subcuenta,
                                        lc_lineb[1, ( 5 - ln_lon3)], la_acu2[li_k].acciones,
                                        lc_lineb[1, ( 5 - ln_lon4)], ln_subtotal
                       LET   ln_subtotal  = 0
                 END IF
                 OUTPUT TO REPORT f_Report(lc_linea)
             END FOR

             ### Armar totales
             OUTPUT TO REPORT f_Report(lc_linbla)
             LET      ln_lon4    = LENGTH(ln_total)
             LET      lc_titulo3 = "                                       TOTAL ",
                                   lc_lineb[1, (17 - ln_lon4)], ln_total
             OUTPUT TO REPORT f_Report(lc_titulo3)
             OUTPUT TO REPORT f_Report(lc_linbla)
             OUTPUT TO REPORT f_Report(lc_titulo2)
          FINISH REPORT f_Report
          ERROR   "Terminado ", lc_NomRep

       ON KEY (CONTROL-C)
          EXIT INPUT
  END INPUT
  CLOSE WINDOW f_04

END FUNCTION


FUNCTION f_GeneraDtTr()
  DEFINE      lr_reg            RECORD
              nss               CHAR(11), 
              acciones          DECIMAL(22,6)
              END RECORD
  DEFINE      li_i              SMALLINT
  DEFINE      li_j              SMALLINT
  DEFINE      li_k              SMALLINT
  DEFINE      ld_FechaHoy       DATE
  DEFINE      ls_siefore        SMALLINT
  DEFINE      ln_subtotal       DECIMAL(22,6)
  DEFINE      ln_total          DECIMAL(22,6)
  DEFINE      li_cuantos        INTEGER
  DEFINE      lc_NomRep         CHAR(200)
  DEFINE      lc_titulo1        CHAR(130)
  DEFINE      lc_titulo2        CHAR(130)
  DEFINE      lc_titulo3        CHAR(130)
  DEFINE      lc_totales        CHAR(130)
  DEFINE      lc_linbla         CHAR(130)
  DEFINE      lc_linea          CHAR(130)
  DEFINE      lc_lineb          CHAR(130)
  DEFINE      ln_lon1           SMALLINT
  DEFINE      ln_lon2           SMALLINT
  DEFINE      ln_lon3           SMALLINT
  DEFINE      ln_lon4           SMALLINT
  DEFINE      ld_FechaSaldo     DATE

  CALL  f_prepara03()
  INITIALIZE  lr_reg.*  TO NULL

  LET      ld_FechaSaldo         =  f_VerFechaSaldo()
  LET      ld_FechaHoy        =     TODAY
  LET      lc_NomRep          =  gc_RutaRep CLIPPED||'DetXAcci'||
                                 DAY(ld_FechaSaldo) ||MONTH(ld_FechaSaldo) ||YEAR(ld_FechaSaldo)||'-'||
                                 DAY(ld_FechaHoy)  ||MONTH(ld_FechaHoy) ||YEAR(ld_FechaHoy)||'.rep'
  LET      lc_NomRep          =  lc_NomRep CLIPPED
  ERROR   "Espere un momento generando reporte .. "

  START  REPORT f_Report TO lc_NomRep

     LET      lc_titulo1      = "      NSS                         ACCIONES   SIEFORE 5   FECHA DE SALDO: "||
                                DAY(ld_FechaSaldo)||'/'||MONTH(ld_FechaSaldo)||'/'||YEAR(ld_FechaSaldo)
     LET      lc_linbla       = "  "
     OUTPUT TO REPORT f_Report(lc_titulo1)
     OUTPUT TO REPORT f_Report(lc_linbla)
   
     FOREACH    d_SelAxD5          USING gd_fecha_corte
                                   INTO  lr_reg.*
         IF     lr_reg.acciones   IS NULL THEN
                LET  lr_reg.acciones  = 0
         END IF
         LET   ln_lon1    = LENGTH(lr_reg.nss)
         LET   ln_lon3    = LENGTH(lr_reg.acciones)

         LET   lc_linea = lc_lineb[1, (13 - ln_lon1)], lr_reg.nss,
                          lc_lineb[1, ( 5 - ln_lon3)], lr_reg.acciones
         OUTPUT TO REPORT f_Report(lc_linea)
     END FOREACH

     LET      lc_titulo1      = "      NSS                         ACCIONES   SIEFORE 4                   "
     OUTPUT TO REPORT f_Report(lc_titulo1)
     OUTPUT TO REPORT f_Report(lc_linbla)

     FOREACH    d_SelAxD4          USING gd_fecha_corte
                                   INTO  lr_reg.*
         IF     lr_reg.acciones   IS NULL THEN
                LET  lr_reg.acciones  = 0
         END IF
         LET   ln_lon1    = LENGTH(lr_reg.nss)
         LET   ln_lon3    = LENGTH(lr_reg.acciones)

         LET   lc_linea = lc_lineb[1, (13 - ln_lon1)], lr_reg.nss,
                          lc_lineb[1, ( 5 - ln_lon3)], lr_reg.acciones
         OUTPUT TO REPORT f_Report(lc_linea)
     END FOREACH

     LET      lc_titulo1      = "      NSS                         ACCIONES   SIEFORE 3                   "
     OUTPUT TO REPORT f_Report(lc_titulo1)
     OUTPUT TO REPORT f_Report(lc_linbla)

     FOREACH    d_SelAxD3          USING gd_fecha_corte
                                   INTO  lr_reg.*
         IF     lr_reg.acciones   IS NULL THEN
                LET  lr_reg.acciones  = 0
         END IF
         LET   ln_lon1    = LENGTH(lr_reg.nss)
         LET   ln_lon3    = LENGTH(lr_reg.acciones)

         LET   lc_linea = lc_lineb[1, (13 - ln_lon1)], lr_reg.nss,
                          lc_lineb[1, ( 5 - ln_lon3)], lr_reg.acciones
         OUTPUT TO REPORT f_Report(lc_linea)
     END FOREACH

     LET      lc_titulo1      = "      NSS                         ACCIONES   SIEFORE 2                   "
     OUTPUT TO REPORT f_Report(lc_titulo1)
     OUTPUT TO REPORT f_Report(lc_linbla)

     FOREACH    d_SelAxD2          USING gd_fecha_corte
                                   INTO  lr_reg.*
         IF     lr_reg.acciones   IS NULL THEN
                LET  lr_reg.acciones  = 0
         END IF
         LET   ln_lon1    = LENGTH(lr_reg.nss)
         LET   ln_lon3    = LENGTH(lr_reg.acciones)

         LET   lc_linea = lc_lineb[1, (13 - ln_lon1)], lr_reg.nss,
                          lc_lineb[1, ( 5 - ln_lon3)], lr_reg.acciones
         OUTPUT TO REPORT f_Report(lc_linea)
     END FOREACH

     FINISH REPORT f_Report
     ERROR   "Terminado ", lc_NomRep

END FUNCTION
 
