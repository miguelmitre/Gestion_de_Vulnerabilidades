#################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )	        #
#Owner             => E.F.P.        			        #
#Programa DISB022  => REPORTE CONFAF                            #
#Fecha             => 27 DE SEPTIEMBRE 1999.    	        #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ. 	        #
#Sistema           => DIS. 				        #
#Modificado por    => GERARDO ALFONSO VEGA PAREDES              #
#Fecha             => 16-jul-2004.                              #
#Modificado por    => JOSE ALEJANDRO RAMIREZ                    #
#Fecha             => 13 Jul 2006  c22-11                       #
#################################################################
DATABASE safre_af

GLOBALS
    DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore 
    DEFINE HOY	          DATE

    DEFINE g_usuario      CHAR (08)
    DEFINE g_param_dis    RECORD LIKE seg_modulo.*
    DEFINE G_LISTA        CHAR(300)
    DEFINE G_IMPRE        CHAR(300)
    DEFINE G_IMPRE_VIV    CHAR(300)
    DEFINE G_IMPRE_REM    CHAR(300)
    DEFINE G_IMPRE_EXT    CHAR(300)
    DEFINE gimpresion     CHAR(300)
    DEFINE hora           CHAR(08)

    DEFINE v RECORD
              folio              INTEGER
    END RECORD

    DEFINE l_record   ARRAY[100] OF RECORD
            fech_liquidacion  DATE,
            ident_pago        CHAR(16),
            desc              CHAR(11),
            importe           DECIMAL(15,2),
            impt_aport_acept  DECIMAL(15,2),
            impt_aport_dev    DECIMAL(15,2),
            tipo_siefore      SMALLINT,       --c22-6
            fech_liquidacion2 DATE,
            ident_pago2       CHAR(16),
            desc2             CHAR(11),
            part_viv_env      DECIMAL(15,6),  --c22-11
            part_viv_acept    DECIMAL(15,6),  --c22-11
            part_viv_dev      DECIMAL(15,6),  --c22-11
            fech_liquidacion3 DATE,
            ident_pago3       CHAR(16),
            desc3             CHAR(11),
            apli_rem_viv_env  DECIMAL(15,6),  --c22-11
            apli_rem_viv_ace  DECIMAL(15,6),  --c22-11
            apli_rem_viv_dev  DECIMAL(15,6),  --c22-11
            fech_liquidacion4 DATE,
            ident_pago4       CHAR(16),
            desc4             CHAR(11),
            apli_ext_viv_env  DECIMAL(15,6),  --c22-11
            apli_ext_viv_ace  DECIMAL(15,6),  --c22-11
            apli_ext_viv_dev  DECIMAL(15,6)  --c22-11
    END RECORD

    DEFINE g    RECORD
            folio             INTEGER,
            fech_liquidacion  DATE,
            ident_pago        CHAR(16),
            desc              CHAR(11),
            importe           DECIMAL(15,2),
            impt_aport_acept  DECIMAL(15,2),
            impt_aport_dev    DECIMAL(15,2),
            part_viv_env      DECIMAL(16,6),
            part_viv_acept    DECIMAL(16,6),
            part_viv_dev      DECIMAL(16,6),
            apli_rem_viv_env  DECIMAL(16,6),  --c22-11
            apli_rem_viv_ace  DECIMAL(16,6),  --c22-11
            apli_rem_viv_dev  DECIMAL(16,6),  --c22-11
            apli_ext_viv_env  DECIMAL(16,6),  --c22-11
            apli_ext_viv_ace  DECIMAL(16,6),  --c22-11
            apli_ext_viv_dev  DECIMAL(16,6),  --c22-11
            tipo_siefore      SMALLINT,       --c22-6
            consec            INTEGER         --c22-115
    END RECORD

    DEFINE pos                     SMALLINT   
    DEFINE opc     CHAR(01),
	   cla_sel CHAR(500)

define
vfolio  integer,
vfecha  date,
vident  char(16),
vdesc   char(11),
vimporte decimal(15,2),
vimpt_acept decimal(15,2),
vimpt_dev   decimal(15,2) 

END GLOBALS

MAIN
    OPTIONS PROMPT LINE LAST,
    INPUT WRAP,
    ACCEPT KEY control-o

    CALL STARTLOG("DISB022.log");

    DEFER INTERRUPT

    SELECT  codigo_afore,USER
    INTO    w_codigo_afore,g_usuario
    FROM    tab_afore_local

    SELECT  ruta_listados
    INTO    g_param_dis.ruta_listados
    FROM    seg_modulo
    WHERE   modulo_cod ="dis"

    LET HOY = TODAY

    OPEN WINDOW ventana_1 AT 3,4 
    WITH 3 ROWS,72 COLUMNS
    ATTRIBUTE( BORDER)
    DISPLAY " DISB022 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE)
	
    MENU "MENU "
      COMMAND "Reporte CONFAF." 
        CALL Consulta()
      COMMAND "Salir" "Salir del Programa"
        EXIT MENU
    END MENU

    CLOSE WINDOW ventana_1

END MAIN

FUNCTION Consulta()
   DEFINE pos                     SMALLINT   

   CLEAR SCREEN

   OPEN WINDOW ventana_21 AT 10,4 WITH FORM "DISB0222" ATTRIBUTE( BORDER)
   DISPLAY "              (Ctrl-C) Salir          (ESC) Ejecutar                           " AT 1,1 ATTRIBUTE(REVERSE,green)

   LET int_flag= FALSE

   INPUT BY NAME v.folio
     ON KEY (ESC)
        LET int_flag =FALSE
        ERROR "PROCESANDO INFORMACION...."
        EXIT INPUT
     ON KEY (control-c)
        LET int_flag = TRUE
        EXIT INPUT
   END INPUT
   
   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      CLEAR FORM
      ERROR "Folio no aceptado..."
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana_21
      RETURN
   END IF

   LET cla_sel = "SELECT fech_liquidacion,",
                        "ident_pago[14,15],",
                        "' ',",
                        "importe,",
                        "impt_aport_acept,",
                        "impt_aport_dev, ",
                        "tipo_siefore,",      --c22-6
                        "fech_liquidacion,",
                        "ident_pago[14,15],",
                        "' ',",
                        "part_viv_env,",
                        "part_viv_acept,",
                        "part_viv_dev, ",
                        "fech_liquidacion,",
                        "ident_pago[14,15],",
                        "' ',",
                        "apli_rem_viv_env,",  --c22-11
                        "apli_rem_viv_ace,",  --c22-11
                        "apli_rem_viv_dev,",  --c22-11
                        "fech_liquidacion,",
                        "ident_pago[14,15],",
                        "' ',",
                        "apli_ext_viv_env,",  --c22-11
                        "apli_ext_viv_ace,",  --c22-11
                        "apli_ext_viv_dev ",  --c22-11
                 "FROM   dis_dep_aporte ",
                 "WHERE  folio = ",v.folio,
                " ORDER  BY 3" CLIPPED

   PREPARE claexe FROM cla_sel
   DECLARE cursor_1 CURSOR FOR claexe

   LET pos = 1

   FOREACH cursor_1 INTO l_record[pos].*

      IF l_record[pos].importe IS NULL THEN
         LET l_record[pos].importe = "0.0"
      END IF

      IF l_record[pos].impt_aport_acept IS NULL THEN
         LET l_record[pos].impt_aport_acept = "0.0"
      END IF

      IF l_record[pos].impt_aport_dev IS NULL THEN
         LET l_record[pos].impt_aport_dev = "0.0"
      END IF

      IF l_record[pos].part_viv_env IS NULL THEN
         LET l_record[pos].part_viv_env = "0.0"
      END IF

      IF l_record[pos].part_viv_acept IS NULL THEN
         LET l_record[pos].part_viv_acept = "0.0"
      END IF

      IF l_record[pos].part_viv_dev IS NULL THEN
         LET l_record[pos].part_viv_dev = "0.0"
      END IF

      IF l_record[pos].apli_rem_viv_env IS NULL THEN    --c22-11
         LET l_record[pos].apli_rem_viv_env = "0.0"     --c22-11
      END IF                                            --c22-11
      IF l_record[pos].apli_rem_viv_ace IS NULL THEN    --c22-11
         LET l_record[pos].apli_rem_viv_ace = "0.0"     --c22-11
      END IF                                            --c22-11
      IF l_record[pos].apli_rem_viv_dev IS NULL THEN    --c22-11
         LET l_record[pos].apli_rem_viv_dev = "0.0"     --c22-11
      END IF                                            --c22-11
      IF l_record[pos].apli_ext_viv_env IS NULL THEN    --c22-11
         LET l_record[pos].apli_ext_viv_env = "0.0"     --c22-11
      END IF                                            --c22-11
      IF l_record[pos].apli_ext_viv_ace IS NULL THEN    --c22-11
         LET l_record[pos].apli_ext_viv_ace = "0.0"     --c22-11
      END IF                                            --c22-11
      IF l_record[pos].apli_ext_viv_dev IS NULL THEN    --c22-11
         LET l_record[pos].apli_ext_viv_dev = "0.0"     --c22-11
      END IF                                            --c22-11

      CASE
         WHEN l_record[pos].ident_pago  = "11"
            LET l_record[pos].desc = "COM.RCV"
            LET l_record[pos].desc2 = "COM.RCV"
         WHEN l_record[pos].ident_pago  = "12"
            LET l_record[pos].desc = "COM.EST"
            LET l_record[pos].desc2 = "COM.EST"
         WHEN l_record[pos].ident_pago  = "15"      --c22-11
            LET l_record[pos].desc = "COM.VOL"      --c22-11
            LET l_record[pos].desc2 = "COM.VOL"     --c22-11
         WHEN l_record[pos].ident_pago  = "16"      --c22-11
            LET l_record[pos].desc = "COM.ACR"      --c22-11
            LET l_record[pos].desc2 = "COM.ACR"     --c22-11

         WHEN l_record[pos].ident_pago  = "17"      --c22-11
            LET l_record[pos].desc = "COM.REM"      --c22-11
            LET l_record[pos].desc3 = "COM.REM"     --c22-11

         WHEN l_record[pos].ident_pago  = "18"      --c22-11
            LET l_record[pos].desc = "COM.EXT"      --c22-11
            LET l_record[pos].desc4 = "COM.EXT"     --c22-11

         WHEN l_record[pos].ident_pago  = "21"
            LET l_record[pos].desc = "PRO.INT.RCV"
            LET l_record[pos].desc2 = "PRO.INT.RCV"
         WHEN l_record[pos].ident_pago  = "22"
            LET l_record[pos].desc = "PRO.INT.EST"
            LET l_record[pos].desc2 = "PRO.INT.EST"
         WHEN l_record[pos].ident_pago  = "23"
            LET l_record[pos].desc = "PRO.INT.VIV"
            LET l_record[pos].desc2 = "PRO.INT.VIV"
         WHEN l_record[pos].ident_pago  = "24"
            LET l_record[pos].desc = "PRO.INT.GAR"
            LET l_record[pos].desc2 = "PRO.INT.GAR"
         WHEN l_record[pos].ident_pago  = "25"       --c22-11
            LET l_record[pos].desc = "PRO.INT.VOL"   --c22-11
            LET l_record[pos].desc2 = "PRO.INT.VOL"  --c22-11
         WHEN l_record[pos].ident_pago  = "26"       --c22-11
            LET l_record[pos].desc = "PRO.INT.ACR"   --c22-11
            LET l_record[pos].desc2 = "PRO.INT.ACR"  --c22-11

         WHEN l_record[pos].ident_pago   = "27"       --c22-11
            LET l_record[pos].desc = "PRO.INT.REM"   --c22-11
            LET l_record[pos].desc3 = "PRO.INT.REM"  --c22-11
         WHEN l_record[pos].ident_pago   = "28"       --c22-11
            LET l_record[pos].desc = "PRO.INT.EXT"   --c22-11
            LET l_record[pos].desc4 = "PRO.INT.EXT"  --c22-11

         WHEN l_record[pos].ident_pago  = "31"
            LET l_record[pos].desc = "COM.INT.RCV"
            LET l_record[pos].desc2 = "COM.INT.RCV"
         WHEN l_record[pos].ident_pago  = "32"
            LET l_record[pos].desc = "COM.INT.EST"
            LET l_record[pos].desc2 = "COM.INT.EST"
         WHEN l_record[pos].ident_pago  = "35"        --c22-11 
            LET l_record[pos].desc = "COM.INT.VOL"    --c22-11 
            LET l_record[pos].desc2 = "COM.INT.VOL"   --c22-11 
         WHEN l_record[pos].ident_pago  = "36"        --c22-11 
            LET l_record[pos].desc = "COM.INT.ACR"    --c22-11 
            LET l_record[pos].desc2 = "COM.INT.ACR"   --c22-11 

         WHEN l_record[pos].ident_pago  = "37"        --c22-11 
            LET l_record[pos].desc = "COM.INT.REM"    --c22-11 
            LET l_record[pos].desc3 = "COM.INT.REM"   --c22-11 
         WHEN l_record[pos].ident_pago  = "38"        --c22-11 
            LET l_record[pos].desc = "COM.INT.EXT"    --c22-11 
            LET l_record[pos].desc4 = "COM.INT.EXT"   --c22-11 

         WHEN l_record[pos].ident_pago  = "41"
            LET l_record[pos].desc = "PRO.RCV"
            LET l_record[pos].desc2 = "PRO.RCV"
         WHEN l_record[pos].ident_pago  = "42"
            LET l_record[pos].desc = "PRO.EST"
            LET l_record[pos].desc2 = "PRO.EST"
         WHEN l_record[pos].ident_pago  = "43"
            LET l_record[pos].desc = "PRO.VIV"
            LET l_record[pos].desc2 = "PRO.VIV"
         WHEN l_record[pos].ident_pago  = "44"
            LET l_record[pos].desc = "PRO.GAR"
            LET l_record[pos].desc2 = "PRO.GAR"
         WHEN l_record[pos].ident_pago  = "45"   --c22-11
            LET l_record[pos].desc = "PRO.VOL"   --c22-11
            LET l_record[pos].desc2 = "PRO.VOL"  --c22-11
         WHEN l_record[pos].ident_pago  = "46"   --c22-11
            LET l_record[pos].desc = "PRO.ACR"   --c22-11
            LET l_record[pos].desc2 = "PRO.ACR"  --c22-11

         WHEN l_record[pos].ident_pago  = "47"   --c22-11
            LET l_record[pos].desc = "PRO.REM"   --c22-11
            LET l_record[pos].desc3 = "PRO.REM"  --c22-11
         WHEN l_record[pos].ident_pago  = "48"   --c22-11
            LET l_record[pos].desc = "PRO.EXT"   --c22-11
            LET l_record[pos].desc4 = "PRO.EXT"  --c22-11
         END CASE 

         LET pos = pos + 1
    END FOREACH

    INITIALIZE l_record[pos].* TO NULL

    CLOSE WINDOW ventana_21

    IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      ERROR ""
----      OPEN WINDOW ventana_2 AT 6,4 WITH FORM "DISB0221" ATTRIBUTE( BORDER)
      OPEN WINDOW ventana_2 AT 2,2 WITH FORM "DISB0221" ATTRIBUTE( BORDER)
      DISPLAY " CONSULTA " AT 1,50 ATTRIBUTE(REVERSE,green)
      DISPLAY " (Ctrl-C) Salir              (Ctrl-P) Imprimir            (Ctrl-F) Archivo     " AT 1,1 ATTRIBUTE(REVERSE,green)
      DISPLAY "                            LISTADO DE CONFAF                                  " AT 3,1 ATTRIBUTE(REVERSE,green) 
   	
      DISPLAY ARRAY l_record TO scr_1.*
         ON KEY (Control-p)
            ERROR "PROCESANDO IMPRESION...."
            CALL imprimir(pos)
         ON KEY (Control-f)
            ERROR "PROCESANDO LISTADO...."
            CALL archivo(pos)
         ON KEY (INTERRUPT)
            EXIT DISPLAY
      END DISPLAY

      CLEAR WINDOW ventana_2
      CLOSE WINDOW ventana_2
    ELSE
      ERROR "ARCHIVO ... VACIO."
      SLEEP 2
      ERROR ""
    END IF 

END FUNCTION

FUNCTION archivo(pos)

    DEFINE pos INTEGER

    LET hora = TIME
    LET G_LISTA = g_param_dis.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                                                       ".ARC_CONFAF",
                               HOY USING "DD-MM-YYYY","_",hora CLIPPED
    START REPORT rpt_cuenta_arc TO G_LISTA
    CALL cuentas_archi(pos)

END FUNCTION
 
FUNCTION cuentas_archi(pos)
    DEFINE i,pos INTEGER

    FOR i=1 TO (pos+1)
      LET g.folio              = v.folio
      LET g.fech_liquidacion   = l_record[i].fech_liquidacion
      LET g.ident_pago         = l_record[i].ident_pago
      LET g.desc               = l_record[i].desc
      LET g.importe            = l_record[i].importe
      LET g.impt_aport_acept   = l_record[i].impt_aport_acept
      LET g.impt_aport_dev     = l_record[i].impt_aport_dev
      LET g.tipo_siefore       = l_record[i].tipo_siefore      --c22-6
      LET g.part_viv_env       = l_record[i].part_viv_env
      LET g.part_viv_acept     = l_record[i].part_viv_acept
      LET g.part_viv_dev       = l_record[i].part_viv_dev
      LET g.apli_rem_viv_env   = l_record[i].apli_rem_viv_env  --c22-11
      LET g.apli_rem_viv_ace   = l_record[i].apli_rem_viv_ace  --c22-11
      LET g.apli_rem_viv_dev   = l_record[i].apli_rem_viv_dev  --c22-11
      LET g.apli_ext_viv_env   = l_record[i].apli_ext_viv_env  --c22-11
      LET g.apli_ext_viv_ace   = l_record[i].apli_ext_viv_ace  --c22-11
      LET g.apli_ext_viv_dev   = l_record[i].apli_ext_viv_dev  --c22-11
      LET g.consec             = i                             --c22-115

      IF g.folio IS NULL OR g.folio <> g.folio THEN
           EXIT FOR
      END IF

      OUTPUT TO REPORT rpt_cuenta_arc(g.*)
    END FOR

    FINISH REPORT rpt_cuenta_arc

    ERROR "ARCHIVO GENERADO...." 
    SLEEP 2
    ERROR ""

    LET G_LISTA = "chmod 777 ",g_param_dis.ruta_listados CLIPPED,
                       "/",g_usuario CLIPPED,".ARC_CONFAF",
                      HOY USING "DD-MM-YYYY","_",hora CLIPPED
    RUN G_LISTA

END FUNCTION

REPORT rpt_cuenta_arc(g)

    DEFINE g    RECORD
            folio             INTEGER,
            fech_liquidacion  DATE,
            ident_pago        CHAR(16),
            desc              CHAR(11),
            importe           DECIMAL(15,2),
            impt_aport_acept  DECIMAL(15,2),
            impt_aport_dev    DECIMAL(15,2),
            part_viv_env      DECIMAL(16,6),
            part_viv_acept    DECIMAL(16,6),
            part_viv_dev      DECIMAL(16,6),
            apli_rem_viv_env  DECIMAL(16,6),   --c22-11
            apli_rem_viv_ace  DECIMAL(16,6),   --c22-11
            apli_rem_viv_dev  DECIMAL(16,6),   --c22-11
            apli_ext_viv_env  DECIMAL(16,6),   --c22-11
            apli_ext_viv_ace  DECIMAL(16,6),   --c22-11
            apli_ext_viv_dev  DECIMAL(16,6),   --c22-11
            tipo_siefore      SMALLINT,        --c22-6
            consec            INTEGER          --c22-115
    END RECORD

    FORMAT
     PAGE HEADER
        PRINT COLUMN 02,"DISB022",
              COLUMN 30,"FOLIO: ",g.folio USING "#####",
              COLUMN 43,"FECHA LIQUIDA: ",g.fech_liquidacion USING "dd-mm-yyyy",
              COLUMN 70,"REPORTE CONFAF ",
              COLUMN 87, TODAY USING "mm/dd/yyyy"
        SKIP 2 LINE

       PRINT COLUMN 1,"ID",
             COLUMN 04,"DESCRIP.",
             COLUMN 19,"IMPORTE",
             COLUMN 31,"IMP.ACEPTADO",
             COLUMN 45,"IMP.RECHAZADO",
             COLUMN 74,"PART ENVIADO",
             COLUMN 59,"PART ACEPTADO",
             COLUMN 98,"PART RECHAZADO",

             COLUMN 110,"IMP. REM ENV",      --c22-11
             COLUMN 124,"IMP. REM ACE",      --c22-11
             COLUMN 137,"IMP. REM DEV",      --c22-11
             COLUMN 151,"IMP. EXT ENV",      --c22-11
             COLUMN 164,"IMP. EXT ACE",      --c22-11
             COLUMN 177,"IMP. EXT DEV",      --c22-11
             column 200,"SIEFORE"

       SKIP 1 LINE

     ON EVERY ROW
       PRINT COLUMN 1,g.ident_pago USING "##",
             COLUMN 07,g.desc,
             COLUMN 22,g.importe          USING "########&.&#",
             COLUMN 34,g.impt_aport_acept USING "########&.&#",
             COLUMN 47,g.impt_aport_dev   USING "########&.&#",
             COLUMN 60,g.part_viv_env     USING "########&.&&&&&&",
             COLUMN 77,g.part_viv_acept   USING "########&.&&&&&&",
             COLUMN 94,g.part_viv_dev     USING "########&.&&&&&&",

             COLUMN 112,g.apli_rem_viv_env USING "########&.&&&&&&", --c22-11
             COLUMN 129,g.apli_rem_viv_ace USING "########&.&&&&&&", --c22-11
             COLUMN 146,g.apli_rem_viv_dev USING "########&.&&&&&&", --c22-11
             COLUMN 163,g.apli_ext_viv_env USING "########&.&&&&&&", --c22-11
             COLUMN 180,g.apli_ext_viv_ace USING "########&.&&&&&&", --c22-11
             COLUMN 197,g.apli_ext_viv_dev USING "########&.&&&&&&", --c22-11
             COLUMN 214,g.tipo_siefore

END REPORT

FUNCTION imprimir(pos)

    DEFINE pos INTEGER

    LET hora = TIME

    LET G_IMPRE     = g_param_dis.ruta_listados CLIPPED,"/k10"
    LET G_IMPRE_VIV = g_param_dis.ruta_listados CLIPPED,"/k11"
    LET G_IMPRE_REM = g_param_dis.ruta_listados CLIPPED,"/k12"
    LET G_IMPRE_EXT = g_param_dis.ruta_listados CLIPPED,"/k13"

    START REPORT rpt_cuenta_imp TO G_IMPRE
    START REPORT rpt_cuenta_imp_viv TO G_IMPRE_VIV
    START REPORT rpt_cuenta_imp_rem TO G_IMPRE_REM
    START REPORT rpt_cuenta_imp_ext TO G_IMPRE_EXT

    CALL cuentas_impri(pos)

    LET cla_sel = "sed -e /^$/d ",G_IMPRE," > ",g_param_dis.ruta_listados CLIPPED,"/kt100";
    RUN cla_sel
    LET cla_sel = "sed -e /^$/d ",G_IMPRE_VIV," > ",g_param_dis.ruta_listados CLIPPED,"/kt110" 
    RUN cla_sel
    LET cla_sel = "sed -e /^$/d ",G_IMPRE_REM," > ",g_param_dis.ruta_listados CLIPPED,"/kt120" 
    RUN cla_sel
    LET cla_sel = "sed -e /^$/d ",G_IMPRE_EXT," > ",g_param_dis.ruta_listados CLIPPED,"/kt130" 
    RUN cla_sel

    LET cla_sel = "cat ",g_param_dis.ruta_listados CLIPPED,
                  "/kt100 ",g_param_dis.ruta_listados CLIPPED,
                  "/kt110 ",g_param_dis.ruta_listados CLIPPED,
                  "/kt120 ",g_param_dis.ruta_listados CLIPPED,
                  "/kt130 > ",g_param_dis.ruta_listados CLIPPED,
                  "/kt"
    RUN cla_sel

    LET cla_sel = "mv ",g_param_dis.ruta_listados CLIPPED,
                 "/kt ",g_param_dis.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                 ".LIS_CONFAF",HOY USING "DD-MM-YYYY",
                 "_",hora CLIPPED
   
    LET G_IMPRE = g_param_dis.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                  ".LIS_CONFAF",HOY USING "DD-MM-YYYY",
                  "_",hora CLIPPED
                   
    RUN cla_sel

    LET cla_sel = "rm ",g_param_dis.ruta_listados CLIPPED,"/k*"
    RUN cla_sel

    LET gimpresion = "lp ",G_IMPRE
    RUN gimpresion

END FUNCTION

FUNCTION cuentas_impri(pos)
    DEFINE i,pos INTEGER

    INITIALIZE g.* TO NULL

    FOR i=1 TO (pos+1)
      LET g.folio              = v.folio
      LET g.fech_liquidacion   = l_record[i].fech_liquidacion
      LET g.ident_pago         = l_record[i].ident_pago
      LET g.desc               = l_record[i].desc
      LET g.importe            = l_record[i].importe
      LET g.impt_aport_acept   = l_record[i].impt_aport_acept
      LET g.tipo_siefore       = l_record[i].tipo_siefore      --c22-6
      LET g.impt_aport_dev     = l_record[i].impt_aport_dev
      LET g.part_viv_env       = l_record[i].part_viv_env
      LET g.part_viv_acept     = l_record[i].part_viv_acept
      LET g.part_viv_dev       = l_record[i].part_viv_dev
      LET g.apli_rem_viv_env   = l_record[i].apli_rem_viv_env  --c22-11
      LET g.apli_rem_viv_ace   = l_record[i].apli_rem_viv_ace  --c22-11
      LET g.apli_rem_viv_dev   = l_record[i].apli_rem_viv_dev  --c22-11
      LET g.apli_ext_viv_env   = l_record[i].apli_ext_viv_env  --c22-11
      LET g.apli_ext_viv_ace   = l_record[i].apli_ext_viv_ace  --c22-11
      LET g.apli_ext_viv_dev   = l_record[i].apli_ext_viv_dev  --c22-11
      LET g.consec             = i                             --c22-115

      IF g.folio IS NULL OR g.folio <> g.folio THEN
           EXIT FOR
      END IF

      ---CAMPOS COMPLEMENTARIOS DE VIVIENDA
      IF g.ident_pago = '43' OR g.ident_pago = '44' THEN
         OUTPUT TO REPORT rpt_cuenta_imp_viv(g.*)
      END IF
      ---CAMPOS COMPLEMENTARIOS DE VIV REMANENTE
      IF g.ident_pago = '47' THEN
         OUTPUT TO REPORT rpt_cuenta_imp_rem(g.*)
      END IF
      ---CAMPOS COMPLEMENTARIOS DE VIV EXTEMPORA
      IF g.ident_pago = '48' THEN
         OUTPUT TO REPORT rpt_cuenta_imp_ext(g.*)
      END IF
    IF g.ident_pago is not null and g.ident_pago<>' ' then
      OUTPUT TO REPORT rpt_cuenta_imp(g.*)
    END IF
    END FOR

    FINISH REPORT rpt_cuenta_imp
    FINISH REPORT rpt_cuenta_imp_viv
    FINISH REPORT rpt_cuenta_imp_rem
    FINISH REPORT rpt_cuenta_imp_ext

    ERROR "LISTADO GENERADO...."
    SLEEP 2
    ERROR ""

END FUNCTION

REPORT rpt_cuenta_imp(g)

    DEFINE g    RECORD
            folio             INTEGER,
            fech_liquidacion  DATE,
            ident_pago        CHAR(16),
            desc              CHAR(11),
            importe           DECIMAL(15,2),
            impt_aport_acept  DECIMAL(15,2),
            impt_aport_dev    DECIMAL(15,2),
            part_viv_env      DECIMAL(16,6),
            part_viv_acept    DECIMAL(16,6),
            part_viv_dev      DECIMAL(16,6),
            apli_rem_viv_env  DECIMAL(16,6),  --c22-11
            apli_rem_viv_ace  DECIMAL(16,6),  --c22-11
            apli_rem_viv_dev  DECIMAL(16,6),  --c22-11
            apli_ext_viv_env  DECIMAL(16,6),  --c22-11
            apli_ext_viv_ace  DECIMAL(16,6),  --c22-11
            apli_ext_viv_dev  DECIMAL(16,6),  --c22-11
            tipo_siefore      SMALLINT,        --c22-6
            consec            INTEGER         --c22-115
    END RECORD

    OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60
      ORDER BY g.ident_pago,g.consec         --c22-115

    FORMAT
      PAGE HEADER
----         PRINT '\033e\033(s218T\033(s12H\033(s7B'

         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l10d\033(s12H'

        PRINT COLUMN 02,"DISB022",
              COLUMN 15,"FOLIO: ",g.folio USING "#####",
              COLUMN 55,"REPORTE CONFAF ",
              COLUMN 70, TODAY USING "mm/dd/yyyy"
       PRINT COLUMN 1," "

       PRINT COLUMN 1,"ID",
             COLUMN 08,"DESCRIP.",
             COLUMN 24,"IMPORTE",
             COLUMN 35,"IMP.ACEPTADO",
             COLUMN 49,"IMP.RECHAZADO",
             COLUMN 63,"SIEF",
             COLUMN 68,"FEC LIQUIDA"

       PRINT COLUMN 1," "

     ON EVERY ROW

       PRINT COLUMN 01,g.ident_pago USING "##",
             COLUMN 07,g.desc,
             COLUMN 22,g.importe          USING "########&.&#",
             COLUMN 34,g.impt_aport_acept USING "########&.&#",
             COLUMN 47,g.impt_aport_dev   USING "########&.&#",
             COLUMN 65,g.tipo_siefore     USING "##",
             COLUMN 68,g.fech_liquidacion USING "dd-mm-yyyy"
 
      ON LAST ROW 
        PRINT COLUMN 1," "
        PRINT COLUMN 1," "
        PRINT COLUMN 1,"____________________________________________________________"
              

END REPORT

REPORT rpt_cuenta_imp_viv(g)

    DEFINE g    RECORD
            folio             INTEGER,
            fech_liquidacion  DATE,
            ident_pago        CHAR(16),
            desc              CHAR(11),
            importe           DECIMAL(15,2),
            impt_aport_acept  DECIMAL(15,2),
            impt_aport_dev    DECIMAL(15,2),
            part_viv_env      DECIMAL(16,6),
            part_viv_acept    DECIMAL(16,6),
            part_viv_dev      DECIMAL(16,6),
            apli_rem_viv_env  DECIMAL(16,6),  --c22-11
            apli_rem_viv_ace  DECIMAL(16,6),  --c22-11
            apli_rem_viv_dev  DECIMAL(16,6),  --c22-11
            apli_ext_viv_env  DECIMAL(16,6),  --c22-11
            apli_ext_viv_ace  DECIMAL(16,6),  --c22-11
            apli_ext_viv_dev  DECIMAL(16,6),  --c22-11
            tipo_siefore      SMALLINT,        --c22-6
            consec            INTEGER         --c22-115
    END RECORD

    OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60
      ORDER BY g.ident_pago,g.consec         --c22-115

    FORMAT
      PAGE HEADER
       PRINT COLUMN 1,"DETALLE CAMPOS VIVIENDA"
       PRINT COLUMN 1," "
       PRINT COLUMN 1,"ID",
             COLUMN 12,"APLI VIV ENV",
             COLUMN 32,"APLI VIV ACEP",
             COLUMN 47,"APLI VIV DEVUE"

      SKIP 1 LINE

     ON EVERY ROW

      PRINT COLUMN 1,g.ident_pago        USING "##",
            COLUMN 07,g.part_viv_env     USING "########&.&&&&&&",
            COLUMN 24,g.part_viv_acept   USING "########&.&&&&&&",
            COLUMN 41,g.part_viv_dev     USING "########&.&&&&&&"

      ON LAST ROW 
        PRINT COLUMN 1," "
        PRINT COLUMN 1," "
        PRINT COLUMN 1,"____________________________________________________________"
END REPORT

REPORT rpt_cuenta_imp_rem(g)

    DEFINE g    RECORD
            folio             INTEGER,
            fech_liquidacion  DATE,
            ident_pago        CHAR(16),
            desc              CHAR(11),
            importe           DECIMAL(15,2),
            impt_aport_acept  DECIMAL(15,2),
            impt_aport_dev    DECIMAL(15,2),
            part_viv_env      DECIMAL(16,6),
            part_viv_acept    DECIMAL(16,6),
            part_viv_dev      DECIMAL(16,6),
            apli_rem_viv_env  DECIMAL(16,6),  --c22-11
            apli_rem_viv_ace  DECIMAL(16,6),  --c22-11
            apli_rem_viv_dev  DECIMAL(16,6),  --c22-11
            apli_ext_viv_env  DECIMAL(16,6),  --c22-11
            apli_ext_viv_ace  DECIMAL(16,6),  --c22-11
            apli_ext_viv_dev  DECIMAL(16,6),  --c22-11
            tipo_siefore      SMALLINT,        --c22-6
            consec            INTEGER         --c22-115
    END RECORD

    OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60
      ORDER BY g.ident_pago,g.consec         --c22-115

    FORMAT
      PAGE HEADER
       PRINT COLUMN 1,"DETALLE VIVIENDA REMANENTE"
       PRINT COLUMN 1," "

       PRINT COLUMN 1,"ID",
             COLUMN 12,"APLI REM ENV",
             COLUMN 31,"APLI REM ACEP",
             COLUMN 47,"APLI REM DEVUE"

      SKIP 1 LINE

     ON EVERY ROW

      PRINT COLUMN 01,g.ident_pago        USING "##",
            COLUMN 08,g.apli_rem_viv_env  USING "########&.&&&&&&", --c22-11
            COLUMN 25,g.apli_rem_viv_ace  USING "########&.&&&&&&", --c22-11
            COLUMN 42,g.apli_rem_viv_dev  USING "########&.&&&&&&"  --c22-11

      ON LAST ROW 
        PRINT COLUMN 1," "
        PRINT COLUMN 1," "
        PRINT COLUMN 1,"____________________________________________________________"

END REPORT

REPORT rpt_cuenta_imp_ext(g)

    DEFINE g    RECORD
            folio             INTEGER,
            fech_liquidacion  DATE,
            ident_pago        CHAR(16),
            desc              CHAR(11),
            importe           DECIMAL(15,2),
            impt_aport_acept  DECIMAL(15,2),
            impt_aport_dev    DECIMAL(15,2),
            part_viv_env      DECIMAL(16,6),
            part_viv_acept    DECIMAL(16,6),
            part_viv_dev      DECIMAL(16,6),
            apli_rem_viv_env  DECIMAL(16,6),  --c22-11
            apli_rem_viv_ace  DECIMAL(16,6),  --c22-11
            apli_rem_viv_dev  DECIMAL(16,6),  --c22-11
            apli_ext_viv_env  DECIMAL(16,6),  --c22-11
            apli_ext_viv_ace  DECIMAL(16,6),  --c22-11
            apli_ext_viv_dev  DECIMAL(16,6),  --c22-11
            tipo_siefore      SMALLINT,        --c22-6
            consec            INTEGER         --c22-115
    END RECORD

    OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60
      ORDER BY g.ident_pago,g.consec         --c22-115

    FORMAT
      PAGE HEADER
       PRINT COLUMN 1,"DETALLE VIVIENDA EXTEMPORANEA"
       SKIP 1 LINE
       PRINT COLUMN 1,"ID",
             COLUMN 12,"APLI EXT ENV",
             COLUMN 31,"APLI EXT ACEP",
             COLUMN 47,"APLI EXT DEVUE"

      SKIP 1 LINE

     ON EVERY ROW

      PRINT COLUMN 01,g.ident_pago        USING "##",
            COLUMN 08,g.apli_ext_viv_env  USING "########&.&&&&&&", --c22-11
            COLUMN 25,g.apli_ext_viv_ace  USING "########&.&&&&&&", --c22-11
            COLUMN 42,g.apli_ext_viv_dev  USING "########&.&&&&&&"  --c22-11

END REPORT
