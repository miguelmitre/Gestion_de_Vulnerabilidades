################################################################################
#Project           => SAFRE (Mexico)
#Owner             => E.F.P.
#Programa VOLB001  => REPORTES DE TRANSACCIONES RECIBIDAS,SIEFORES, MEDIO
#Sistema           => VOL
#Fecha creacion    => 25 de Noviembre del 2004
#Por               => LAURA EUGENIA CORTES GUZMAN    
#Actualizado       => 25 de Noviembre del 2004
#Por               => LAURA EUGENIA CORTES GUZMAN         
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE ruta    LIKE seg_modulo.ruta_listados,
          HOY     DATE,
          ejecuta CHAR(500),
          g_user  CHAR(08),
          enter   CHAR(01)
          

END GLOBALS
################################################################################
MAIN
   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I

   DEFER INTERRUPT 

   CALL STARTLOG("VOLR001.log")

   INITIALIZE ruta, HOY, g_user TO NULL

   SELECT n.ruta_listados,USER INTO ruta, g_user FROM seg_modulo n
   WHERE  n.modulo_cod = "int"

   LET HOY = TODAY

   OPEN WINDOW v1 AT 3,2 WITH 20 ROWS,78 COLUMNS ATTRIBUTE(BORDER)
   DISPLAY " VOLB001            REPORTE DE TRANSACCIONES ",
           "RECIBIDAS                  " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd/mm/yyyy " AT 3,68 ATTRIBUTE(REVERSE)

   MENU "REPORTES"
      COMMAND "Transacciones Recibidas" 
              "Reporte de Transacciones Recibidas"
              CALL recibidas()
      COMMAND "Inversion de Siefores e Indicadores"
              "Reporte de Inversion de Siefores e Indicadores"
              CALL siefores()
      COMMAND "Recursos recibidos por medio y diario"
              "Reporte de Recursos recibidos por medio y diario"
              CALL medio_y_dia()
      COMMAND "Salir" "Salir del Programa"
              EXIT MENU
   END MENU

   CLOSE WINDOW v1

END MAIN
###############################################################################
FUNCTION ventana2(valor)
   DEFINE valor    SMALLINT

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1

   DISPLAY " [ESC] Total selec.                                           [",
           "Ctrl-c] Salir " AT 2,1
   OPEN WINDOW ventana AT 6,2 WITH FORM "VOLR0011"
   CASE valor
   WHEN 1  DISPLAY "Transacciones Recibidas" AT 1,27
   WHEN 2  DISPLAY "Inversion en Siefores" AT 1,28
   WHEN 3  DISPLAY "Recursos recibidos Medio y Diario" AT 1,22
   END CASE

END FUNCTION
#############################################################################
FUNCTION recibidas()

   DEFINE totret      INTEGER,
          totmto      DECIMAL(16,2),
          G_LISTA1    CHAR(200),
          fecha_ini   ,
          fecha_fin   DATE,

          sal_reci    RECORD
              nss             CHAR(11),
              monto           DECIMAL(16,2),
              fecha_captura   DATE,
              medio           SMALLINT,
              resul_operacion CHAR(02),
              estado          SMALLINT
          END RECORD

   INITIALIZE fecha_ini, fecha_fin, G_LISTA1, sal_reci.* TO NULL

   LET G_LISTA1 = ruta CLIPPED,"/VOL_RECIBIDAS_",TODAY USING "ddmmyy",".txt"
   CALL ventana2(1)
   
   LET int_flag = FALSE

   INPUT BY NAME fecha_ini, fecha_fin WITHOUT DEFAULTS
      AFTER FIELD fecha_ini
         IF fecha_ini IS NULL OR fecha_ini = "           " THEN
            ERROR "Fecha Inicial no puede ser NULA.."
            NEXT FIELD fecha_ini
         ELSE
            IF fecha_ini > TODAY THEN
               ERROR "La Fecha Inicial no puede ser Mayor al dia de HOY "
               NEXT FIELD fecha_ini
            END IF
         END IF

      AFTER FIELD fecha_fin
         IF fecha_fin IS NULL OR fecha_fin = "           " THEN
            ERROR "Fecha Inicial no puede ser NULA.."
            NEXT FIELD fecha_fin
         ELSE
            IF fecha_fin > TODAY THEN
               ERROR "La Fecha Final no puede ser Mayor a La HOY"
               NEXT FIELD fecha_fin
            END IF

            IF fecha_ini > fecha_fin THEN
               ERROR "La Fecha Inicial no puede ser Mayor a La Fecha Final"
               NEXT FIELD fecha_ini
            END IF
         END IF

         SELECT UNIQUE "m.K" 
         FROM   int_det_voluntaria m
         WHERE  m.fecha_captura BETWEEN fecha_ini AND fecha_fin
         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "NO EXISTE INFORMACION DEL PERIODO SOLICITADO "
            SLEEP 2
            ERROR ""
            NEXT FIELD fecha_ini
         END IF

         EXIT INPUT

      ON KEY (CONTROL-C, INTERRUPT)
         LET int_flag = TRUE
         EXIT INPUT
   END INPUT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..." 
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana
      RETURN
   END IF

   DISPLAY " PROCESANDO INFORMACION... ESPERE UN MOMENTO..." AT 14,5
   SLEEP 2

   START REPORT salida_uno TO G_LISTA1

   DECLARE cur_uno CURSOR FOR
       SELECT a.nss,
              a.monto_neto,
              a.fecha_captura,
              a.medio,
              a.resul_operacion,
              a.estado
       FROM   int_det_voluntaria a
       WHERE  a.resul_operacion = "01"
       AND    a.fecha_captura BETWEEN fecha_ini AND fecha_fin
##       AND    a.estado = 1
       ORDER BY 3,4

       LET totret = 0
       LET totmto = 0

   FOREACH cur_uno INTO sal_reci.*

      LET totret = totret + 1
      LET totmto = totmto + sal_reci.monto

      OUTPUT TO REPORT salida_uno(sal_reci.*,totret,totmto,fecha_ini,fecha_fin)
   END FOREACH

   FINISH REPORT salida_uno

   DISPLAY "" AT 14,1
   WHILE TRUE
     PROMPT "Desea (A)rchivo / (I)mpresion ? " FOR enter
     IF enter MATCHES "[aAIi]" THEN
        EXIT WHILE
     END IF
   END WHILE

   IF enter MATCHES "[aA]" THEN
      DISPLAY "" AT 16,1
      DISPLAY "Su Archivo se encuentra en : ", G_LISTA1 CLIPPED AT 16,1
   ELSE
      LET ejecuta = "lp ",G_LISTA1 CLIPPED
      RUN ejecuta
      SLEEP 2

      LET ejecuta = "rm ",G_LISTA1 CLIPPED
      RUN ejecuta
      ERROR "Su Impresion ya esta en la Impresora "
   END IF

   PROMPT "PROCESO FINALIZADO... <Enter> PARA CONTINUAR " FOR enter
   CLOSE WINDOW ventana
END FUNCTION
#######################################################################
REPORT salida_uno(sal_reci,totreg,totmto,fecha_ini,fecha_fin)
  DEFINE sal_reci    RECORD
              nss             CHAR(11),
              monto           DECIMAL(16,2),
              fecha_captura   DATE,
              medio           SMALLINT,
              resul_operacion CHAR(02),
              estado          SMALLINT
         END RECORD,

         totreg      INTEGER,
         totmto      DECIMAL(16,2),
         fecha_ini   ,
         fecha_fin   DATE,
         desc        CHAR(20),
         estado      CHAR(21)

  OUTPUT
     TOP MARGIN 0
     LEFT MARGIN 0
     BOTTOM MARGIN 0
     PAGE LENGTH 60

  FORMAT
     PAGE HEADER
        PRINT COLUMN 016,"REPORT DE TRANSACCIONES RECIBIDAS",
              COLUMN 076,TODAY USING "dd/mm/yyyy"
        PRINT COLUMN 028,"DEL ",fecha_ini USING "dd/mm/yyyy",
                         " AL ",fecha_fin USING "dd/mm/yyyy",
              COLUMN 079,"VOLR001"
        PRINT COLUMN 078,"PAG. ",PAGENO USING "##&"
        SKIP 2 LINES
        PRINT COLUMN 001,"==================================================",
                         "===================================" ##85
        PRINT COLUMN 001,"  N.S.S.   ",
              COLUMN 016,"   M O N T O    ",
              COLUMN 036,"F E C H A ",
              COLUMN 050,"     M E D I O      ",
              COLUMN 074,"LIQ/PEN/REC"
        PRINT COLUMN 001,"==================================================",
                         "===================================" ##85
        PRINT
    ON EVERY ROW
        INITIALIZE desc TO NULL
        SELECT a.descripcion INTO desc FROM tab_vol_medio a
        WHERE  a.cod_medio = sal_reci.medio
        IF SQLCA.SQLCODE <> 0 THEN
           LET desc = "NO EXISTE MEDIO "
        END IF
        IF sal_reci.resul_operacion = "01" THEN
           CASE sal_reci.estado 
                WHEN 1 LET estado = "PENDIENTE LIQUIDACION"
                WHEN 2 LET estado = "LIQUIDADO            "
           END CASE
        ELSE
                LET estado = "RECHAZADO POR EL NSS "
        END IF

        PRINT COLUMN 001,sal_reci.nss,
              COLUMN 016,sal_reci.monto USING "#####,###,###.&&",
              COLUMN 036,sal_reci.fecha_captura USING "dd/mm/yyyy",
              COLUMN 050,desc CLIPPED,
              COLUMN 074,estado CLIPPED

     ON LAST ROW
        PRINT
        PRINT COLUMN 001,"==================================================",
                         "===================================" ##85
        PRINT COLUMN 001,"TOTAL: ",totreg USING "######&",
              COLUMN 016,totmto USING "#####,###,###.&&"
END REPORT

FUNCTION siefores()

   DEFINE totreg      INTEGER,
          totmto      DECIMAL(22,6),
          totacc      DECIMAL(22,6),
          G_LISTA1    CHAR(200),
          txt         CHAR(300),
          fecha_ini   ,
          fecha_fin   DATE,
          siefore     SMALLINT,

          sal_sie    RECORD
              siefore         SMALLINT,
              fecha           DATE,
              monto_p         DECIMAL(22,6),
              monto_a         DECIMAL(22,6)
          END RECORD

   INITIALIZE fecha_ini, fecha_fin, G_LISTA1, sal_sie.* TO NULL

   LET siefore  = 0
   LET totmto   = 0
   LET totacc   = 0

   LET G_LISTA1 = ruta CLIPPED,"/VOL_SIEFORE_",TODAY USING "ddmmyy",".txt"
   CALL ventana2(2)

   LET int_flag = FALSE

   INPUT  fecha_ini, fecha_fin, siefore 
    FROM  FORMONLY.fecha_ini, FORMONLY.fecha_fin, FORMONLY.medio
      BEFORE FIELD fecha_ini
        DISPLAY "SIEFORE " AT 11,25

      AFTER FIELD fecha_ini
         IF fecha_ini IS NULL OR fecha_ini = "           " THEN
            ERROR "Fecha Inicial no puede ser NULA.."
            NEXT FIELD fecha_ini
         ELSE
            IF fecha_ini > TODAY THEN
               ERROR "La Fecha Inicial no puede ser Mayor al dia de HOY "
               NEXT FIELD fecha_ini
            END IF
         END IF

      AFTER FIELD fecha_fin
         IF fecha_fin IS NULL OR fecha_fin = "           " THEN
            ERROR "Fecha Inicial no puede ser NULA.."
            NEXT FIELD fecha_fin
         ELSE
            IF fecha_fin > TODAY THEN
               ERROR "La Fecha Final no puede ser Mayor a La HOY"
               NEXT FIELD fecha_fin
            END IF

            IF fecha_ini > fecha_fin THEN
               ERROR "La Fecha Inicial no puede ser Mayor a La Fecha Final"
               NEXT FIELD fecha_ini
            END IF
         END IF

      BEFORE FIELD medio
        DISPLAY "Siefore (1),(2) o si desea las dos (3)" AT 16,1

      AFTER FIELD medio
         IF siefore = 0 OR siefore IS NULL THEN
            ERROR "Debe seleccionar el tipo de siefore que desea "
            NEXT FIELD medio
         END IF

         CASE siefore
         WHEN 1
              SELECT UNIQUE "m.K" 
              FROM   dis_cuenta m
              WHERE  m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              AND    m.siefore = 1
         WHEN 2
              SELECT UNIQUE "m.K" 
              FROM   dis_cuenta m
              WHERE  m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              AND    m.siefore = 2
         WHEN 3
              SELECT UNIQUE "m.K" 
              FROM   dis_cuenta m
              WHERE  m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              AND    m.siefore IN(1,2)
         END CASE
         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "NO EXISTE INFORMACION DEL PERIODO SOLICITADO "
            SLEEP 2
            ERROR ""
            NEXT FIELD fecha_ini
         END IF

         EXIT INPUT

      ON KEY (CONTROL-C, INTERRUPT)
         LET int_flag = TRUE
         EXIT INPUT
   END INPUT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..." 
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana
      RETURN
   END IF

   DISPLAY "" AT 16,1
   DISPLAY " PROCESANDO INFORMACION... ESPERE UN MOMENTO..." AT 14,5
   SLEEP 2

   START REPORT salida_dos TO G_LISTA1

   CASE siefore
   WHEN 1
         LET txt = " SELECT m.siefore, m.fecha_conversion, ", 
                   " SUM(m.monto_en_pesos), SUM(m.monto_en_acciones) ",
                   " FROM   dis_cuenta m ",
                   " WHERE  m.fecha_conversion BETWEEN '",fecha_ini,"'",
                   " AND '",fecha_fin,"'",
                   " AND  m.siefore = 1 ",
                   " GROUP BY 1,2 ",
                   " ORDER BY 1,2 "
   WHEN 2
         LET txt = " SELECT m.siefore, m.fecha_conversion, ", 
                   " SUM(m.monto_en_pesos), SUM(m.monto_en_acciones) ",
                   " FROM   dis_cuenta m ",
                   " WHERE  m.fecha_conversion BETWEEN '",fecha_ini,"'",
                   " AND '",fecha_fin,"'",
                   " AND  m.siefore = 2 ",
                   " GROUP BY 1,2 ",
                   " ORDER BY 1,2 "
   WHEN 3
         LET txt = " SELECT m.siefore, m.fecha_conversion, ", 
                   " SUM(m.monto_en_pesos), SUM(m.monto_en_acciones) ",
                   " FROM   dis_cuenta m ",
                   " WHERE  m.fecha_conversion BETWEEN '",fecha_ini,"'",
                   " AND '",fecha_fin,"'",
                   " AND  m.siefore IN(1,2) ",
                   " GROUP BY 1,2 ",
                   " ORDER BY 1,2 "
   END CASE

    LET totmto = 0
    LET totacc = 0

   PREPARE cur_dos FROM txt 
   DECLARE cursor_dos CURSOR FOR cur_dos
   FOREACH cursor_dos INTO sal_sie.*

      LET totreg = totreg + 1
      LET totmto = totmto + sal_sie.monto_p
      LET totacc = totacc + sal_sie.monto_a

      OUTPUT TO REPORT 
             salida_dos(sal_sie.*,totreg,totmto,totacc,fecha_ini,fecha_fin)
   END FOREACH

   FINISH REPORT salida_dos

   DISPLAY "" AT 14,1
   WHILE TRUE
     PROMPT "Desea (A)rchivo / (I)mpresion ? " FOR enter
     IF enter MATCHES "[aAIi]" THEN
        EXIT WHILE
     END IF
   END WHILE

   IF enter MATCHES "[aA]" THEN
      DISPLAY "" AT 16,1
      DISPLAY "Su Archivo se encuentra en : ", G_LISTA1 CLIPPED AT 16,1
   ELSE
      LET ejecuta = "lp ",G_LISTA1 CLIPPED
      RUN ejecuta
      SLEEP 2

      LET ejecuta = "rm ",G_LISTA1 CLIPPED
      RUN ejecuta
      ERROR "Su Impresion ya esta en la Impresora "
   END IF

   PROMPT "PROCESO FINALIZADO... <Enter> PARA CONTINUAR " FOR enter
   ERROR ""
   CLOSE WINDOW ventana
END FUNCTION
####################################################################
REPORT salida_dos(sal_sie,totreg,totmto,totacc,fecha_ini,fecha_fin)
  DEFINE sal_sie    RECORD
              siefore         SMALLINT,
              fecha           DATE,
              monto_p         DECIMAL(22,6),
              monto_a         DECIMAL(22,6)
         END RECORD,

         ban         SMALLINT,
         totreg      INTEGER,
         totmto      DECIMAL(22,6),
         totacc      DECIMAL(22,6),
         monto_p     DECIMAL(22,6),
         monto_a     DECIMAL(22,6),
         fecha_ini   ,
         fecha_fin   DATE

  OUTPUT
     TOP MARGIN 3
     LEFT MARGIN 0
     BOTTOM MARGIN 0
     PAGE LENGTH 60

  FORMAT
     PAGE HEADER
        PRINT COLUMN 024,"REPORTE DE INVERSION EN SIEFORES",
              COLUMN 071,TODAY USING "dd/mm/yyyy"
        PRINT COLUMN 026,"DEL ",fecha_ini USING "dd/mm/yyyy",
                         " AL ",fecha_fin USING "dd/mm/yyyy",
              COLUMN 074,"VOLR001"
        PRINT COLUMN 073,"PAG. ",PAGENO USING "##&"
        PRINT
        PRINT

    BEFORE GROUP OF sal_sie.siefore
        LET monto_a = 0
        LET monto_p = 0

        SKIP 2 LINES
        PRINT COLUMN 001,"==================================================",
                         "==============================" ##80
        PRINT COLUMN 001,"  SIEFORE  ",
              COLUMN 016,"F E C H A ",
              COLUMN 030,"    MONTO EN PESOS    ",
              COLUMN 056,"    MONTO EN ACCIONES "
        PRINT COLUMN 001,"==================================================",
                         "==============================" ##80
        PRINT

    ON EVERY ROW
        PRINT COLUMN 006,sal_sie.siefore USING "&",
              COLUMN 016,sal_sie.fecha USING "dd/mm/yyyy",
              COLUMN 030,sal_sie.monto_p USING "###,###,###,##&.&&&&&&",
              COLUMN 056,sal_sie.monto_a USING "###,###,###,##&.&&&&&&"

              LET monto_p = monto_p + sal_sie.monto_p
              LET monto_a = monto_a + sal_sie.monto_a

    AFTER GROUP OF sal_sie.siefore
        PRINT
        PRINT COLUMN 001,"==================================================",
                         "==============================" ##80
        PRINT COLUMN 001,"TOTAL: ",GROUP COUNT(*) USING "#######&",
              COLUMN 030,monto_p USING "###,###,###,##&.&&&&&&",
              COLUMN 056,monto_a USING "###,###,###,##&.&&&&&&"

        LET monto_p = 0
        LET monto_a = 0
        SKIP 3 LINE

     ON LAST ROW
        SKIP 2 LINE
        PRINT COLUMN 001,"==================================================",
                         "==============================" ##80
        PRINT COLUMN 001,"GRAN TOTAL:",totreg USING "#######&",
              COLUMN 030,totmto USING "###,###,###,##&.&&&&&&",
              COLUMN 056,totacc USING "###,###,###,##&.&&&&&&"
END REPORT
###########################################################################
FUNCTION medio_y_dia()
   DEFINE totreg      INTEGER,
          totmto      DECIMAL(22,6),
          txt         CHAR(200),
          G_LISTA1    CHAR(200),
          descrip     CHAR(25),
          fecha_ini   ,
          fecha_fin   DATE,
          medio       SMALLINT,

          sal_medio    RECORD
              medio           SMALLINT,
              fecha           DATE,
              monto_p         DECIMAL(22,6)
          END RECORD

   INITIALIZE descrip,fecha_ini, fecha_fin, G_LISTA1, sal_medio.* TO NULL

   LET medio  = 0

   LET G_LISTA1 = ruta CLIPPED,"/VOL_MEDIO_",TODAY USING "ddmmyy",".txt"
   CALL ventana2(3)

   LET int_flag = FALSE

   INPUT BY NAME fecha_ini, fecha_fin, medio WITHOUT DEFAULTS
      BEFORE FIELD fecha_ini
         DISPLAY "MEDIO DE APORTACION" AT 11,15
         DISPLAY "" AT 16,1

      AFTER FIELD fecha_ini
         IF fecha_ini IS NULL OR fecha_ini = "           " THEN
            ERROR "Fecha Inicial no puede ser NULA.."
            NEXT FIELD fecha_ini
         ELSE
            IF fecha_ini > TODAY THEN
               ERROR "La Fecha Inicial no puede ser Mayor al dia de HOY "
               NEXT FIELD fecha_ini
            END IF
         END IF

      AFTER FIELD fecha_fin
         IF fecha_fin IS NULL OR fecha_fin = "           " THEN
            ERROR "Fecha Inicial no puede ser NULA.."
            NEXT FIELD fecha_fin
         ELSE
            IF fecha_fin > TODAY THEN
               ERROR "La Fecha Final no puede ser Mayor a La HOY"
               NEXT FIELD fecha_fin
            END IF

            IF fecha_ini > fecha_fin THEN
               ERROR "La Fecha Inicial no puede ser Mayor a La Fecha Final"
               NEXT FIELD fecha_ini
            END IF
         END IF

      BEFORE FIELD medio
         DISPLAY "Si selecciona cero(0) le dará todos los Medios de Aportacion"
                 AT 16,1

      AFTER FIELD medio
         IF medio IS NULL OR medio = " " THEN
            CALL cat_medio() RETURNING medio, descrip
            IF medio IS NULL THEN
               ERROR "DEBE SELECCIONAR ALGUN MEDIO DE APORTACION "
               NEXT FIELD medio
            ELSE
               IF medio = 0 THEN
                  DISPLAY "                        " AT 11,42
                  DISPLAY "TODOS LOS MEDIOS" AT 11,42 
               ELSE
                  SELECT m.descripcion INTO descrip FROM tab_vol_medio m
                  WHERE  m.cod_medio = medio
                  DISPLAY "                        " AT 11,42
                  DISPLAY descrip AT 11,42
               END IF
               DISPLAY medio TO medio
            END IF
         ELSE
            IF medio = 0 THEN
               DISPLAY "                        " AT 11,42
               DISPLAY "TODOS LOS MEDIOS" AT 11,42 
            ELSE
               SELECT a.descripcion INTO descrip FROM tab_vol_medio a
               WHERE  a.cod_medio = medio
               IF SQLCA.SQLCODE <> 0 THEN
                  ERROR "NO EXISTE CODIGO QUE DIGITO PARA EL ",
                        "MEDIO DE APORTACION"
                  NEXT FIELD medio
               END IF
               DISPLAY "                        " AT 11,42
               DISPLAY descrip AT 11,42
            END IF
         END IF

         WHILE TRUE
            PROMPT " DESEA EL MEDIO SELECCIONADO S/N ? " FOR enter
            IF enter MATCHES "[sSnN]" THEN
               EXIT WHILE
            END IF
         END WHILE

         IF enter MATCHES "[nN]" THEN
            NEXT FIELD medio
         END IF
         
         EXIT INPUT

      ON KEY (CONTROL-C, INTERRUPT)
         LET int_flag = TRUE
         EXIT INPUT
   END INPUT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..." 
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana
      RETURN
   END IF

   DISPLAY "" AT 16,1

   DISPLAY " PROCESANDO INFORMACION... ESPERE UN MOMENTO..." AT 14,5
   SLEEP 2

   START REPORT salida_tres TO G_LISTA1

   IF medio = 0 THEN
       LET txt = " SELECT a.medio, ",
                 " a.fecha_captura, ",
                 " SUM(a.monto_neto) ",
                 " FROM   int_det_voluntaria a ",
                 " WHERE  a.fecha_captura BETWEEN '",
                   fecha_ini,"'",
                 " AND '",fecha_fin,"'",
                 " GROUP BY 1,2 ",
                 " ORDER BY 1,2 "
   ELSE
       LET txt = " SELECT a.medio, ",
                 " a.fecha_captura, ",
                 " SUM(a.monto_neto) ",
                 " FROM   int_det_voluntaria a ",
                 " WHERE  a.fecha_captura BETWEEN '",
                   fecha_ini,"'",
                 " AND '",fecha_fin ,"'",
                 " AND    a.medio = ",medio,
                 " GROUP BY 1,2 "
   END IF

   LET totmto = 0

   PREPARE cur_tres FROM txt 
   DECLARE cursor_tres CURSOR FOR cur_tres
   FOREACH cursor_tres INTO sal_medio.*

      LET totreg = totreg + 1
      LET totmto = totmto + sal_medio.monto_p

      OUTPUT TO REPORT 
             salida_tres(sal_medio.*,totreg,totmto,fecha_ini,fecha_fin)
   END FOREACH

   FINISH REPORT salida_tres

   DISPLAY "" AT 14,1
   WHILE TRUE
     PROMPT "Desea (A)rchivo / (I)mpresion ? " FOR enter
     IF enter MATCHES "[aAIi]" THEN
        EXIT WHILE
     END IF
   END WHILE

   IF enter MATCHES "[aA]" THEN
      DISPLAY "" AT 16,1
      DISPLAY "Su Archivo se encuentra en : ", G_LISTA1 CLIPPED AT 16,1
   ELSE
      LET ejecuta = "lp ",G_LISTA1 CLIPPED
      RUN ejecuta
      SLEEP 2

      LET ejecuta = "rm ",G_LISTA1 CLIPPED
      RUN ejecuta
      ERROR "Su Impresion ya esta en la Impresora "
   END IF

   PROMPT "PROCESO FINALIZADO... <Enter> PARA CONTINUAR " FOR enter
   ERROR ""
   CLOSE WINDOW ventana
END FUNCTION

REPORT salida_tres(sal_medio,totreg,totmto,fecha_ini,fecha_fin)
  DEFINE sal_medio    RECORD
              medio          SMALLINT,
              fecha          DATE,
              monto_p        DECIMAL(22,6)
         END RECORD,

         desc        CHAR(20),
         totreg      INTEGER,
         totmto      DECIMAL(22,6),
         monto_p     DECIMAL(22,6),
         monto_t     DECIMAL(22,6),
         fecha_ini   ,
         fecha_fin   DATE

  OUTPUT
     TOP MARGIN 0
     LEFT MARGIN 0
     BOTTOM MARGIN 0
     PAGE LENGTH 60

  FORMAT
     PAGE HEADER
        PRINT COLUMN 016,"REPORTE DE RECURSOS RECIBIDOS POR MEDIO Y DIARIO",
              COLUMN 071,TODAY USING "dd/mm/yyyy"
        PRINT COLUMN 026,"DEL ",fecha_ini USING "dd/mm/yyyy",
                         " AL ",fecha_fin USING "dd/mm/yyyy",
              COLUMN 074,"VOLR001"
        PRINT COLUMN 073,"PAG. ",PAGENO USING "##&"

    BEFORE GROUP OF sal_medio.medio
        LET monto_p = 0
        INITIALIZE desc TO NULL

        SELECT a.descripcion[1,20] INTO desc FROM tab_vol_medio a
        WHERE  a.cod_medio = sal_medio.medio

        SKIP 2 LINES
        PRINT COLUMN 001,"==================================================",
                         "==============================" ##80
        PRINT COLUMN 001,"        M E D I O        ",
              COLUMN 030,"F E C H A ",
              COLUMN 044,"    MONTO EN PESOS    "
        PRINT COLUMN 001,"==================================================",
                         "==============================" ##80
        PRINT

    ON EVERY ROW
        PRINT COLUMN 001,sal_medio.medio USING "#&"," ",desc CLIPPED,
              COLUMN 030,sal_medio.fecha USING "dd/mm/yyyy",
              COLUMN 044,sal_medio.monto_p USING "###,###,###,##&.&&&&&&"

              LET monto_p = monto_p + sal_medio.monto_p
              LET monto_t = monto_t + sal_medio.monto_p

    AFTER GROUP OF sal_medio.medio
        PRINT
        PRINT COLUMN 001,"==================================================",
                         "==============================" ##80
        PRINT COLUMN 001,"TOTAL REG.: ",GROUP COUNT(*) USING "#######&",
              COLUMN 044,monto_p USING "###,###,###,##&.&&&&&&"

        LET monto_p = 0
        SKIP 3 LINE

     ON LAST ROW
        SKIP 2 LINE
        PRINT COLUMN 001,"==================================================",
                         "==============================" ##80
        PRINT COLUMN 001,"GRAN TOTAL: ",totreg USING "#######&",
              COLUMN 044,totmto USING "###,###,###,##&.&&&&&&"
END REPORT
###########################################################################
FUNCTION cat_medio()
    DEFINE catalogo ARRAY[100] OF RECORD
             cod_medio     SMALLINT,
             descripcion   CHAR(25)
           END RECORD

    DEFINE i            SMALLINT

    DECLARE cursor_1a1 CURSOR FOR
       SELECT  a.cod_medio, a.descripcion
       FROM   tab_vol_medio a
       ORDER  BY 1

       LET i = 1

    FOREACH cursor_1a1 INTO catalogo[i].*
       LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    OPEN WINDOW vent_medio AT 9,10 WITH FORM "VOLR0012" ATTRIBUTE(BORDER)
    DISPLAY "              MEDIOS DE APORTACION              " AT 2,1
    ATTRIBUTE ( REVERSE )

    DISPLAY ARRAY catalogo TO scr_1.*
       ON KEY ( INTERRUPT, CONTROL - C, CONTROL - Z )
          LET i = ARR_CURR()

       ON KEY ( CONTROL-M )
          LET i = ARR_CURR()
          EXIT DISPLAY

    END DISPLAY
    CLOSE WINDOW vent_medio
    RETURN catalogo[i].cod_medio, catalogo[i].descripcion

END FUNCTION
