###########################################################################
#Proyecto               => Sistema de Afores. (MEXICO)                    #
#Propietario            => E.F.P                                          #
#Programa     VOLL001   => Consulta y reporte de aportacion voluntaria    #
#                       => Liquidadas                                     #
#Fecha                  => 29 de agosto de 2001.                          #
#Por                    => LAURA EUGENIA CORTES GUZMAN                    #
#Fecha                  => 07 de Julio del 2006                           #
#Modificado por         => LAURA EUGENIA CORTES GUZMAN                    #
#Fecha Modif.           => 07 de Julio del 2006                           #
#Modulo                 => VOL.                                           #
###########################################################################
DATABASE safre_af
#####################################################################
MAIN

   DEFINE  reg   RECORD
               nss     LIKE dis_cuenta.nss,
               folio   LIKE dis_cuenta.folio,
               sie     LIKE dis_cuenta.siefore,
               mpesos  LIKE dis_cuenta.monto_en_pesos,
               maccion LIKE dis_cuenta.monto_en_acciones,
               paccion LIKE dis_cuenta.precio_accion,
               fliq    LIKE dis_cuenta.fecha_conversion,
               subcta  LIKE dis_cuenta.subcuenta
          END RECORD,

          vruta_listado  LIKE seg_modulo.ruta_listados,

          g_lista              CHAR(100),
          ejecuta              CHAR(100),
          vfecha_aplicacion    DATE,
          HOY                  DATE,
          ban                  SMALLINT,
          enter                CHAR(01),
          usuario              CHAR(08)

   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-O

   DEFER INTERRUPT
   LET HOY = TODAY
   LET ban = 0

   INITIALIZE g_lista, ejecuta TO NULL

   SELECT USER,
          ruta_listados
   INTO   usuario,
          vruta_listado
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = "vol"

   INITIALIZE reg.* TO NULL
   INITIALIZE vfecha_aplicacion TO NULL

   LET vfecha_aplicacion = NULL

   OPEN WINDOW v1 AT 2,2 WITH FORM "VOLR0021" ATTRIBUTE (BORDER)
   DISPLAY " <ESC> Ejecutar                                           <",
           "CTROL-C> Salir      " AT 1,1 ATTRIBUTE (REVERSE)
   DISPLAY " VOLL001            REPORTE DE LA LIQUIDACION",
           " POR FECHA                             " 
           AT 3,1 ATTRIBUTE (REVERSE)
   DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE (REVERSE)

      INPUT vfecha_aplicacion FROM vfecha_aplicacion
          AFTER FIELD vfecha_aplicacion
             IF vfecha_aplicacion IS NULL OR
                vfecha_aplicacion = "          " THEN
                ERROR "Digite Correctamente la Fecha de la Liquidacion"
                NEXT FIELD vfecha_aplicacion
             END IF

             SELECT UNIQUE "X" FROM int_det_voluntaria
             WHERE  fecha_liquida = vfecha_aplicacion
             IF SQLCA.SQLCODE <> 0 THEN
                ERROR "NO EXISTE INFORMACION CON ESA FECHA"
                NEXT FIELD vfecha_aplicacion
             END IF
             LET ban = 0
             EXIT INPUT

          ON KEY (ESC)
             IF vfecha_aplicacion IS NULL OR
                vfecha_aplicacion = "          " THEN
                ERROR "Digite Correctamente la Fecha de la Liquidacion"
                NEXT FIELD vfecha_aplicacion
             END IF

             SELECT UNIQUE (X) FROM int_det_voluntaria
             WHERE  fecha_liquida = vfecha_aplicacion
             IF SQLCA.SQLCODE <> 0 THEN
                ERROR "NO EXISTE INFORMACION CON ESA FECHA"
                NEXT FIELD vfecha_aplicacion
             END IF
             LET ban = 0
             EXIT INPUT

          ON KEY (INTERRUPT,CONTROL-C)
             LET ban = 1
             EXIT INPUT
      END INPUT

      IF ban = 0 THEN
          ERROR "PROCESANDO INFORMACION ..."

          LET g_lista = vruta_listado CLIPPED,"/",
                        usuario CLIPPED,"_LIQ_VOL_",
                        TODAY USING "ddmmyyyy" CLIPPED

          START REPORT sal_vol TO g_lista
          DECLARE cur_sor1 CURSOR FOR
               SELECT a.nss, a.folio, a.siefore, a.monto_en_pesos,
                      a.monto_en_acciones, a.precio_accion,
                      a.fecha_conversion, a.subcuenta
               FROM   dis_cuenta a
               WHERE  a.fecha_conversion = vfecha_aplicacion
               ORDER BY a.subcuenta, a.nss
          FOREACH cur_sor1 INTO reg.*
             OUTPUT TO REPORT sal_vol( reg.*)
          END FOREACH
          FINISH REPORT sal_vol
          ERROR ""
          ERROR "LISTADO GENERADO ..."
          SLEEP 2
          ERROR ""

--          LET ejecuta = " lp ",g_lista CLIPPED
--          RUN ejecuta
          PROMPT "PROCESO FINALIZADO...<ENTER> PARA CONTINUAR " FOR enter
      ELSE
          PROMPT "PROCESO CANCELADO...<ENTER> PARA CONTINUAR " FOR enter
      END IF
   CLOSE WINDOW v1
END MAIN
#####################################################################
REPORT sal_vol(reg)
   DEFINE  reg   RECORD
               nss     LIKE dis_cuenta.nss,
               folio   LIKE dis_cuenta.folio,
               sie     LIKE dis_cuenta.siefore,
               mpesos  LIKE dis_cuenta.monto_en_pesos,
               maccion LIKE dis_cuenta.monto_en_acciones,
               paccion LIKE dis_cuenta.precio_accion,
               fliq    LIKE dis_cuenta.fecha_conversion,
               subcta  LIKE dis_cuenta.subcuenta
          END RECORD,

          razon_social   CHAR(60),
          desc           CHAR(30),
          ban            SMALLINT,
          subcta         SMALLINT

   OUTPUT
   PAGE LENGTH 90
   LEFT MARGIN 0
   RIGHT MARGIN 0
   TOP MARGIN 0
   BOTTOM MARGIN 0

   FORMAT
   PAGE HEADER

     SELECT A.razon_social
     INTO   razon_social
     FROM   tab_afore_local A

##     PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
     PRINT '\033e\033(10U\033&l12\033(12H'

     LET ban = 0

     PRINT  COLUMN 001,"========================================",
            COLUMN 040,"========================================",
            COLUMN 080,"=============================="

     PRINT COLUMN 001,razon_social
     PRINT COLUMN 001,"FECHA : ",TODAY USING "DD/MM/YYYY"
     PRINT COLUMN 001,"PAGINA: ",PAGENO USING "##&"

     PRINT
     COLUMN 001,"VOLR002",
     COLUMN 024,"A P O R T A C I O N E S   L I Q U I D A D A S  E L  ",
                reg.fliq USING "DD/MM/YYYY"
     PRINT COLUMN 001,"----------------------------------------",
           COLUMN 041,"----------------------------------------",
           COLUMN 081,"------------------------------"

   ON EVERY ROW
      IF reg.subcta <> subcta THEN
        SKIP 2 LINES
        CASE reg.subcta
        WHEN 10 LET desc = "V O L U N T A R I A S        "
        WHEN 12 LET desc = "C O M P L E M E N T A R I A S"
        WHEN 16 LET desc = "L A R G O  P L A Z O         "
        END CASE

        PRINT COLUMN 45, desc CLIPPED
        SKIP 1 LINE

        PRINT
        COLUMN 001,"  N.S.S.  ",
        COLUMN 015,"     FOLIO",
        COLUMN 028,"SIEFORE",
        COLUMN 038,"        MONTO EN PESOS",
        COLUMN 063,"     MONTO EN ACCIONES",
        COLUMN 088,"      PRECIO DE ACCION"

        LET subcta = reg.subcta
      END IF

        PRINT
        COLUMN 001,reg.nss,
        COLUMN 015,reg.folio     USING "&&&&&&&&&&",
        COLUMN 031,reg.sie USING "&",
        COLUMN 038,reg.mpesos  USING "###,###,###,##&.&&&&&&",
        COLUMN 063,reg.maccion USING "###,###,###,##&.&&&&&&",
        COLUMN 088,reg.paccion USING "###,###,###,##&.&&&&&&"

   AFTER GROUP OF reg.subcta
        SKIP 2 LINES

        PRINT COLUMN 001,"----------------------------------------",
              COLUMN 041,"----------------------------------------",
              COLUMN 081,"------------------------------"
        PRINT
        COLUMN 1,"TOTAL DE REGISTROS : ",
                  GROUP COUNT(*) USING "####&",
        COLUMN 42 ,GROUP SUM(reg.mpesos) USING "###,###,###,##&.&&&&&&",
        COLUMN 68 ,GROUP SUM(reg.maccion) USING "###,###,###,##&.&&&&&&"

        SKIP 3 LINES

        PRINT COLUMN 001,"----------------------------------------",
              COLUMN 041,"----------------------------------------",
              COLUMN 081,"------------------------------"

   ON LAST ROW

     SKIP 2 LINES

     PRINT
     COLUMN 1,"TOTAL DE APORTACIONES : ",COUNT(*) USING "####&"

END REPORT
