################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			                   #
#Owner             => E.F.P.        					                            #
#Programa          => DISL015                                                  #
#Descripcion       => Consulta provision y liquidacion int.trans. EST          #
#Fecha             => 15 marzo 2002.    	       			                      #
#Por               => GERAROD ALFONSO VEGA PAREDES.    			                #
#Sistema           => DIS. 					                                     #
#Modifica          => Alejandro Ramirez (2003-10-07)                           #
#Descr. de modif   => Se cambio la forma de consulta, antes a traves del       #
#                  => folio, ahora a traves de la fecha del lote               #
#                                                                              #
--------------------------------------------------------------------------------
#Fecha modif       => 21 diciembre 2004                                        #
#Autor             => Alejandro Ramirez                                        #
#Descripcion       => Adecuación layout (032701) (032702) (032709)             #
#                     de acuerdo a circular 22-6 multi siefore                 #
#                     para intereses en transito                               #
################################################################################
DATABASE safre_af

GLOBALS
    DEFINE g RECORD LIKE dis_provision.*
    DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore 
    DEFINE HOY			DATE

    DEFINE g_usuario      CHAR (08)
    DEFINE g_param_dis    RECORD LIKE seg_modulo.*
    DEFINE G_LISTA                  CHAR(300)
    DEFINE G_IMPRE                  CHAR(300)
    DEFINE impresion               CHAR(300)
    DEFINE hora                     CHAR (08)

    DEFINE v RECORD
              folio              INTEGER
    END RECORD

    DEFINE l_record   ARRAY[300] OF RECORD
            siefore               SMALLINT,  --c22-6
            folio                 INTEGER,
            tipo_movimiento       INTEGER,
            subcuenta             INTEGER,
            subct                 CHAR(05),
            monto_en_pesos        DECIMAL(16,6),
            monto_en_acciones     DECIMAL(16,6)
    END RECORD

    DEFINE l_record_1 ARRAY[300] OF RECORD
            siefore               SMALLINT,  --c22-6
            folio                 INTEGER,
            tipo_movimiento       INTEGER,
            subcuenta             INTEGER,
            subct                 CHAR(05),
            monto_en_pesos        DECIMAL(16,6),
            monto_en_acciones     DECIMAL(16,6),
            registros             INTEGER
    END RECORD

    DEFINE opc CHAR(01),
           total_en_pesos        DECIMAL(16,6),
           total_en_acciones     DECIMAL(16,6),
           tot_registros         DECIMAL(16,6),
           enter                 CHAR(1)


    DEFINE cla_sel CHAR(400)
    DEFINE vfecha_lote  DATE
    DEFINE fecha_char1 CHAR(10)
    DEFINE fecha_char2 CHAR(08)
    DEFINE aux_folio   INTEGER

    DEFINE ban2                 SMALLINT,
           tmp_folio            INTEGER,
           fsuma_pesos          DECIMAL(16,6),
           fsuma_acciones       DECIMAL(16,6),
           fregistros           INTEGER

    DEFINE
	   folio_bus      INTEGER,
	   fec            DATE,
           fecha_char     CHAR(10),
	   fecha_bus      CHAR(08),
	   cons           CHAR(03)

END GLOBALS

MAIN
    OPTIONS PROMPT LINE LAST,
    INPUT WRAP,
    ACCEPT KEY control-o

    DEFER INTERRUPT

    SELECT  codigo_afore,USER
    INTO    w_codigo_afore,g_usuario
    FROM    tab_afore_local

    select ruta_listados 
    INTO g_param_dis.ruta_listados
    FROM seg_modulo
    WHERE modulo_cod='dis'


    LET HOY = TODAY

    OPEN WINDOW ventana_1 AT 3,4 
    WITH 3 ROWS,72 COLUMNS
    ATTRIBUTE( BORDER)
    DISPLAY " DISL015 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE)
	
    MENU "Transito EST"
      COMMAND "Provision"
        LET opc = "P"
        CALL Provision()
      COMMAND "Liquidacion"
        LET opc = "L"
        CALL Provision()
      COMMAND "Salir" "Salir del Programa"
        EXIT MENU
    END MENU

    CLOSE WINDOW ventana_1

END MAIN

FUNCTION Provision()
    DEFINE pos INTEGER   

    CLEAR SCREEN

    OPEN WINDOW ventana_21 AT 10,4 WITH FORM "DISL0152" ATTRIBUTE( BORDER)
    DISPLAY " (Ctrl-C) Salir                               (ENTER Y ESC) Ejecutar " AT 1,1 ATTRIBUTE(REVERSE,green)

    LET int_flag= FALSE
    CALL Crea_ttempo()

    INPUT BY NAME vfecha_lote
     AFTER FIELD vfecha_lote
       ON KEY (ESC)

          SELECT "x"
          FROM cta_det_transito
          WHERE fecha_creac_lote=vfecha_lote
	  GROUP BY 1
          --  AND fecha_creac_lote IN (SELECT fecha_creac_lote FROM
          --                   cta_cza_transito)
          --  GROUP BY 1

          IF STATUS <> NOTFOUND THEN

              LET int_flag =FALSE
              ERROR "PROCESANDO INFORMACION...."
              EXIT INPUT
          ELSE
              ERROR "LA FECHA SOLICITADA NO SE ENCUENTRA EN cta_det_transito "
          END IF 

       ON KEY (control-c)
           LET int_flag = TRUE
           EXIT INPUT
       END INPUT
   

       IF int_flag = TRUE THEN
          LET int_flag = FALSE
          CLEAR FORM
          ERROR "Proceso cancelado..."
          SLEEP 2
          ERROR ""
          CLEAR SCREEN
          CLOSE WINDOW ventana_21
          RETURN
       END IF

    LET tot_registros     = 0
    LET total_en_pesos    = 0
    LET total_en_acciones = 0

    LET fecha_char1 = vfecha_lote
    LET fecha_char2 = fecha_char1[7,10]||fecha_char1[1,2]||fecha_char1[4,5]
    LET aux_folio=NULL


    IF opc = "P" THEN

       -- AGREGAMOS CURSOR
       DECLARE c_8 CURSOR FOR
       SELECT fecha_gen_archivo,consec_reg_lote
       FROM cta_det_transito
       WHERE  fecha_creac_lote = vfecha_lote
	      AND    impt_liq_rcv     > 0
       GROUP BY 1,2
       FOREACH c_8 INTO fec,cons


       LET fecha_char = fec
       LET fecha_bus  = fecha_char[7,10],
                        fecha_char[1,2],
		        fecha_char[4,5]

       SELECT a.folio
       INTO   folio_bus 
       FROM   dis_cza_aporte a
       WHERE  a.fech_creac_lote = fecha_bus
       AND    a.lote_del_dia    = cons

       
                      INSERT INTO tt_transito 
                      SELECT a.siefore,a.folio,a.tipo_movimiento,     --c22-6
                             a.subcuenta, 
                             b.subct_corta, 
                             sum(a.monto_en_pesos) monto_en_pesos, 
                             0 monto_en_acciones,
                             count(*) registros
                      FROM   cta_interes_rcv a, tab_subcuenta b 
                      WHERE  a.folio = folio_bus
                      AND    a.subcuenta in (5,6,9) 
                      AND    a.tipo_movimiento = 3 
                      AND    a.subcuenta = b.subct_cod 
                      GROUP  BY 1,2,3,4,5                             --c22-6


      END FOREACH
      
    ELSE

       -- AGREGAMOS CURSOR
       DECLARE c_9 CURSOR FOR
       SELECT fecha_gen_archivo,consec_reg_lote 
       FROM cta_det_transito
       WHERE fecha_creac_lote = vfecha_lote
       AND    impt_liq_rcv     > 0
       GROUP BY 1,2
       FOREACH c_9 INTO fec,cons 

       LET fecha_char = fec
       LET fecha_bus  = fecha_char[7,10],
			fecha_char[1,2],
			fecha_char[4,5]

       SELECT a.folio
       INTO   folio_bus
       FROM   dis_cza_aporte a
       WHERE  a.fech_creac_lote = fecha_bus
       AND    a.lote_del_dia    = cons

                       INSERT INTO tt_transito 
                       SELECT a.siefore,a.folio,a.tipo_movimiento,    --c22-6
                             a.subcuenta, 
                             b.subct_corta, 
                             sum(a.monto_en_pesos) monto_en_pesos, 
                             sum(a.monto_en_acciones) monto_en_acciones,
                             count(*) registros
                      FROM   dis_cuenta a,tab_subcuenta b 
                      WHERE  a.folio = folio_bus
                      AND    a.subcuenta in (5,6,9) 
                      AND    a.tipo_movimiento = 3 
                      AND    a.subcuenta = b.subct_cod 
                      GROUP  BY 1,2,3,4,5                             --c22-6


      END FOREACH
    END IF



    DECLARE cursor_1 CURSOR FOR
    SELECT * FROM tt_transito 
    LET pos = 1

    FOREACH cursor_1 INTO l_record_1[pos].*



         LET total_en_pesos    = total_en_pesos    +
                                 l_record_1[pos].monto_en_pesos
         LET total_en_acciones = total_en_acciones +
                                 l_record_1[pos].monto_en_acciones
         LET tot_registros     = tot_registros + l_record_1[pos].registros

         LET l_record[pos].siefore          =l_record_1[pos].siefore  --c22-6
         LET l_record[pos].folio            =l_record_1[pos].folio
         LET l_record[pos].tipo_movimiento  =l_record_1[pos].tipo_movimiento
         LET l_record[pos].subcuenta        =l_record_1[pos].subcuenta
         LET l_record[pos].subct            =l_record_1[pos].subct
         LET l_record[pos].monto_en_pesos   =l_record_1[pos].monto_en_pesos
         LET l_record[pos].monto_en_acciones=l_record_1[pos].monto_en_acciones

         LET pos = pos + 1
    END FOREACH

    INITIALIZE l_record[pos].*, l_record_1[pos].* TO NULL

    CLOSE WINDOW ventana_21

    IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      ERROR ""
      OPEN WINDOW ventana_2 AT 6,4 WITH FORM "DISL0151" ATTRIBUTE( BORDER)
      DISPLAY " CONSULTA " AT 1,50 ATTRIBUTE(REVERSE,green)
      DISPLAY " (Ctrl-C) Salir          (Ctrl-P) Imprimir           (Ctrl-F) Archivo             " AT 1,1 ATTRIBUTE(REVERSE,green)
      DISPLAY " FECHA DEL LOTE : ",fecha_char2 AT 2,1
      DISPLAY " DISL015               Intereses en Transito EST                             " AT 3,1 ATTRIBUTE(REVERSE,green) 
 
      DISPLAY BY NAME total_en_pesos
      DISPLAY BY NAME total_en_acciones

      DISPLAY ARRAY l_record TO scr_1.*
         ON KEY (Control-p)
            ERROR "PROCESANDO IMPRESION...."
            CALL imprimir()
         ON KEY (Control-f)
            ERROR "PROCESANDO LISTADO...."
            CALL archivo()
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

#------------------------------------------------------------------------------
# Funcion para crear la tabla que acumula los registros
#------------------------------------------------------------------------------
FUNCTION Crea_ttempo()

 
 WHENEVER ERROR CONTINUE
 DROP TABLE tt_transito

 CREATE TEMP TABLE tt_transito
 (
   siefore               SMALLINT,           --c22-6
   folio                 INTEGER,
   tipo_movimiento       INTEGER,
   subcuenta             INTEGER,
   subct                 CHAR(05),
   monto_en_pesos        DECIMAL(16,6),
   monto_en_acciones     DECIMAL(16,6),
   registros             INTEGER 
 )

END FUNCTION


FUNCTION archivo()

    LET hora = TIME
    LET G_LISTA = g_param_dis.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                                                       ".INT_TRA_EST_",
                               HOY USING "DD-MM-YYYY","_",hora CLIPPED
    START REPORT rpt_cuenta_arc TO G_LISTA
    CALL cuentas_archi()

END FUNCTION
 
FUNCTION cuentas_archi()
    DEFINE i                 INTEGER,
           registros         INTEGER,
           ba                SMALLINT,
           bb                SMALLINT,
           sub_registros     INTEGER,
           sub_en_pesos,
           sub_en_acciones   DECIMAL(16,6),
           subct             CHAR(05)

    LET ba              = 0
    LET bb              = 0
    LET sub_registros   = 0
    LET sub_en_pesos    = 0
    LET sub_en_acciones = 0


    LET ban2 = 0
    LET tmp_folio = 0
    LET fsuma_pesos = 0
    LET fsuma_acciones = 0 
    LET fregistros     = 0


    FOR i=1 TO 300
      INITIALIZE subct TO NULL

      LET registros           = 0
      LET g.siefore           = l_record_1[i].siefore             --c22-6
      LET g.folio             = l_record_1[i].folio
      LET g.tipo_movimiento   = l_record_1[i].tipo_movimiento
      LET g.subcuenta         = l_record_1[i].subcuenta
      LET subct               = l_record_1[i].subct
      LET g.monto_en_pesos    = l_record_1[i].monto_en_pesos
      LET g.monto_en_acciones = l_record_1[i].monto_en_acciones
      LET registros           = l_record_1[i].registros


      IF g.tipo_movimiento IS NULL OR g.tipo_movimiento = 0 THEN
           EXIT FOR
      END IF

      IF ba = 0 THEN
         LET bb = g.subcuenta
         LET ba = 1
      END IF

      IF bb = g.subcuenta THEN
         LET sub_en_pesos    = sub_en_pesos    + g.monto_en_pesos
         LET sub_en_acciones = sub_en_acciones + g.monto_en_acciones
         LET sub_registros   = sub_registros   + registros

         OUTPUT TO REPORT rpt_cuenta_arc(g.*,sub_en_pesos,
                                         sub_en_acciones, registros,
                                         sub_registros, total_en_pesos,
                                         total_en_acciones, tot_registros,subct)
      ELSE
         LET bb              = g.subcuenta
         OUTPUT TO REPORT rpt_cuenta_arc(g.*,sub_en_pesos,
                                         sub_en_acciones, registros,
                                         sub_registros, total_en_pesos,
                                         total_en_acciones, tot_registros,subct)
         LET sub_en_pesos    = 0
         LET sub_en_acciones = 0
         LET sub_registros   = 0
         LET sub_en_pesos    = sub_en_pesos    + g.monto_en_pesos
         LET sub_en_acciones = sub_en_acciones + g.monto_en_acciones
         LET sub_registros   = sub_registros   + registros
      END IF
    END FOR
    LET g.subcuenta = null


   { DISPLAY "VALOR DEL FOLIO: ",l_record_1[i].folio
         sleep 5

                    ojo .........
    OUTPUT TO REPORT rpt_cuenta_arc(g.*,sub_en_pesos,
                                    sub_en_acciones, registros,
                                    sub_registros, total_en_pesos,
                                    total_en_acciones, tot_registros,subct)
   }

    FINISH REPORT rpt_cuenta_arc

    ERROR "ARCHIVO GENERADO...." 
    SLEEP 2
    ERROR ""

    LET G_LISTA = "chmod 777 ",g_param_dis.ruta_listados CLIPPED,
                       "/",g_usuario CLIPPED,".INT_TRA_EST_",
                      HOY USING "DD-MM-YYYY","_",hora CLIPPED
    RUN G_LISTA

END FUNCTION

REPORT rpt_cuenta_arc(g,sub_en_pesos,
                      sub_en_acciones, registros,
                      sub_registros, total_en_pesos,
                      total_en_acciones, tot_registros,subct)

    DEFINE g RECORD LIKE dis_provision.*,
           siefore              SMALLINT,                --c22-6
           folio                INTEGER,
           sub_en_pesos,
           sub_en_acciones      DECIMAL(16,6),
           registros            INTEGER,
           sub_registros        INTEGER,
           total_en_pesos,
           total_en_acciones    DECIMAL(16,6),
           tot_registros        INTEGER,
           subct                CHAR(05),
           ban                  SMALLINT,
           cta                  SMALLINT
         


    ORDER BY g.folio
    FORMAT
     PAGE HEADER
       PRINT COLUMN 02, "SIEFORE",                     --c22-6
             COLUMN 09, "FOLIO.",
             COLUMN 16, "MOV.",
             COLUMN 21, "SUB.",
             COLUMN 26, "MONTO EN PESOS",
             COLUMN 48, "MONTO EN ACCIONES",
             COLUMN 64, "No.REG."

       BEFORE GROUP OF  g.folio
       PRINT
       PRINT
       PRINT
           COLUMN 002,"FOLIO  :",g.folio 

       SKIP 1 LINE
       
     ON EVERY ROW

        --SE SUMARIZAN LOS TOTALES POR FOLIO
        IF ban2 = 0 THEN
           LET ban2 = 1
           LET tmp_folio = g.folio
        END IF

        IF tmp_folio = g.folio THEN
  
           LET fsuma_pesos=fsuma_pesos + g.monto_en_pesos
           LET fsuma_acciones=fsuma_acciones + g.monto_en_acciones
           LET fregistros=fregistros + registros

        ELSE
           LET tmp_folio = g.folio
           LET fsuma_pesos=  g.monto_en_pesos
           LET fsuma_acciones=  g.monto_en_acciones
           LET fregistros=registros
        END IF 


        IF ban = 0 THEN
           LET ban = 1
           LET cta = g.subcuenta
        END IF

        IF cta = g.subcuenta THEN
           PRINT COLUMN 01,g.siefore           USING "##",              --c22-6
                 COLUMN 04,g.folio             USING "######&",
                 COLUMN 12,g.tipo_movimiento   USING "##",
                 COLUMN 15,g.subcuenta         USING "##",
                 COLUMN 18,subct ,
                 COLUMN 24,g.monto_en_pesos    USING "###,###,###.######",
                 COLUMN 46,g.monto_en_acciones USING "###,###,###.######",
                 COLUMN 64,registros           USING "######&"
        ELSE
           LET cta = g.subcuenta

           PRINT COLUMN 01,g.siefore           USING "##",               --c22-6
                 COLUMN 04,g.folio             USING "######&",
                 COLUMN 12,g.tipo_movimiento   USING "##",
                 COLUMN 15,g.subcuenta         USING "##",
                 COLUMN 18,subct ,
                 COLUMN 24,g.monto_en_pesos    USING "###,###,###.######",
                 COLUMN 46,g.monto_en_acciones USING "###,###,###.######",
                 COLUMN 64,registros           USING "######&"
        END IF


--      AFTER GROUP OF g.folio
--         PRINT COLUMN 02,"SUBTOTAL : ",
--               COLUMN 24, fsuma_pesos USING "###,###,###.######",
--               COLUMN 46, fsuma_acciones USING "###,###,###.######",
--               COLUMN 64, fregistros     USING "#######"




      ON LAST ROW
           PRINT
           PRINT COLUMN 01,"_________________________________________________"
           PRINT COLUMN 02,"SUBTOTAL : ",
                 COLUMN 24, fsuma_pesos USING "###,###,###.######",
                 COLUMN 46, fsuma_acciones USING "###,###,###.######",
                 COLUMN 64, fregistros     USING "#######"
        SKIP 1 LINE            --- otro ojo
        PRINT COLUMN 2, "GRAN TOTAL :",
              COLUMN 24,total_en_pesos    USING "###,###,###.######",
              COLUMN 46,total_en_acciones USING "###,###,###.######",
              COLUMN 64,tot_registros     USING "######&"

END REPORT

FUNCTION imprimir()

    LET hora = TIME

    LET G_IMPRE = g_param_dis.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                              ".INT_TRA_EST_",HOY USING "DD-MM-YYYY",
                                "_",hora CLIPPED

    START REPORT rpt_cuenta_imp TO G_IMPRE

    CALL cuentas_impri()

    LET impresion = "lp ",G_IMPRE
    RUN impresion

END FUNCTION

FUNCTION cuentas_impri()
    DEFINE i          INTEGER,
           registros  INTEGER,
           ba                SMALLINT,
           bb                SMALLINT,
           sub_en_pesos,
           sub_en_acciones   DECIMAL(16,6),
           subct             CHAR(05),
           sub_registros     INTEGER,
           fecha_rcv         DATE   ,
           fecha_est         DATE   ,
           vsel              CHAR(70)

    LET ba              = 0
    LET bb              = 0
    LET registros       = 0
    LET sub_en_pesos    = 0
    LET sub_en_acciones = 0

    LET ban2 = 0
    LET tmp_folio = 0
    LET fsuma_pesos = 0
    LET fsuma_acciones = 0
    LET fregistros     = 0

    INITIALIZE vsel TO NULL

    LET vsel = " SELECT  d.fecha_conversion ",
               " FROM dis_cuenta d WHERE d.folio = ",v.folio,
               " AND d.subcuenta IN((5,6,9) AND tipo_movimiento = 1"

    PREPARE eje_vsel FROM vsel

    DECLARE c1 CURSOR FOR eje_vsel
    OPEN c1
    FETCH c1 INTO fecha_rcv
    CLOSE c1

    FOR i=1 TO 300
      INITIALIZE subct TO NULL

      LET registros           = 0
      LET g.siefore           = l_record_1[i].siefore               --c22-6
      LET g.folio             = l_record_1[i].folio
      LET g.tipo_movimiento   = l_record_1[i].tipo_movimiento
      LET g.subcuenta         = l_record_1[i].subcuenta
      LET subct               = l_record_1[i].subct
      LET g.monto_en_pesos    = l_record_1[i].monto_en_pesos
      LET g.monto_en_acciones = l_record_1[i].monto_en_acciones
      LET registros           = l_record_1[i].registros

      IF g.tipo_movimiento IS NULL OR g.tipo_movimiento = 0 THEN
            EXIT FOR
      END IF

      IF ba = 0 THEN
         LET bb = g.subcuenta
         LET ba = 1
      END IF
      IF bb = g.subcuenta THEN
         LET sub_en_pesos    = sub_en_pesos    + g.monto_en_pesos
         LET sub_en_acciones = sub_en_acciones + g.monto_en_acciones
         LET sub_registros   = sub_registros   + registros

         OUTPUT TO REPORT rpt_cuenta_imp(g.*, registros,
                                         sub_en_pesos, sub_en_acciones,
                                         sub_registros, tot_registros,
                                         total_en_pesos, total_en_acciones,
                                         fecha_rcv,subct)
      ELSE
         LET bb              = g.subcuenta

         OUTPUT TO REPORT rpt_cuenta_imp(g.*, registros,
                                         sub_en_pesos, sub_en_acciones,
                                         sub_registros, tot_registros,
                                         total_en_pesos, total_en_acciones,
                                         fecha_rcv,subct)
         LET sub_en_pesos    = 0
         LET sub_en_acciones = 0
         LET sub_registros   = 0
         LET sub_en_pesos    = sub_en_pesos    + g.monto_en_pesos
         LET sub_en_acciones = sub_en_acciones + g.monto_en_acciones
         LET sub_registros   = sub_registros   + registros

      END IF

    END FOR
    LET g.subcuenta = null

    OUTPUT TO REPORT rpt_cuenta_imp(g.*, registros,
                                    sub_en_pesos, sub_en_acciones,
                                    sub_registros, tot_registros,
                                    total_en_pesos, total_en_acciones,
                                         fecha_rcv,subct)

    FINISH REPORT rpt_cuenta_imp

    ERROR "LISTADO GENERADO...."
    SLEEP 2
    ERROR ""

END FUNCTION

REPORT rpt_cuenta_imp(g, registros,
                      sub_en_pesos, sub_en_acciones,
                      sub_registros, tot_registros,
                      total_en_pesos, total_en_acciones, fecha_rcv,subct)

    DEFINE g RECORD LIKE dis_provision.*,
           registros            INTEGER,
           sub_en_pesos,
           sub_en_acciones      DECIMAL(16,6),
           sub_registros        INTEGER,
           tot_registros        INTEGER,
           total_en_pesos,
           total_en_acciones    DECIMAL(16,6),
           fecha_rcv            DATE,
           fecha_est            DATE,
           subct                CHAR(05),
           precio               DECIMAL(11,6),
           ban, cta             SMALLINT

    OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60

    ORDER BY g.folio
    FORMAT
      PAGE HEADER
        IF ban = 0 THEN
           LET ban = 1
           LET cta = g.subcuenta
        END IF
        IF opc = "P" THEN
           PRINT COLUMN 02,"DISL015",
                 COLUMN 12,"PROVISION INTERESES EN TRANSITO EST",
                 COLUMN 65, TODAY USING "mm/dd/yyyy"
        ELSE
           PRINT COLUMN 02,"DISL015",
                 COLUMN 12,"LIQUIDACION INTERESES EN TRANSITO EST",
                 COLUMN 65, TODAY USING "mm/dd/yyyy"
        END IF
     -- PRINT COLUMN 02,"FOLIO : ",folio USING "######&"
        SKIP 2 LINE

        PRINT COLUMN 02, "SIEFORE.",                               --c22-6
              COLUMN 11, "FOLIO.",
              COLUMN 18, "MOV.",
              COLUMN 23, "SUB.",
              COLUMN 28, "MONTO EN PESOS",
              COLUMN 46, "MONTO EN ACCIONES",
              COLUMN 64, "No.REG."

     BEFORE GROUP OF  g.folio
        PRINT

        IF g.folio IS NULL OR g.folio= ' ' THEN
        ELSE
          PRINT
            COLUMN 002,"FOLIO  :",g.folio
        END IF
        SKIP 1 LINES


      ON EVERY ROW

        --SE SUMARIZAN LOS TOTALES POR FOLIO
        IF ban2 = 0 THEN
           LET ban2 = 1
           LET tmp_folio = g.folio
        END IF

        IF tmp_folio = g.folio THEN

           LET fsuma_pesos=fsuma_pesos + g.monto_en_pesos
           LET fsuma_acciones=fsuma_acciones + g.monto_en_acciones
           LET fregistros=fregistros + registros

        ELSE
           LET tmp_folio = g.folio
           LET fsuma_pesos=  g.monto_en_pesos
           LET fsuma_acciones=  g.monto_en_acciones
           LET fregistros=registros
        END IF




        IF cta = g.subcuenta THEN
           PRINT COLUMN 01,g.siefore             USING "##",         --c22-6
                 COLUMN 04,g.folio               USING "######&",
                 COLUMN 12,g.tipo_movimiento     USING "##",
                 COLUMN 15,g.subcuenta           USING "##",
                 COLUMN 18,subct ,
                 COLUMN 24,g.monto_en_pesos      USING "###,###,###.######",
                 COLUMN 46,g.monto_en_acciones   USING "###,###,###.######",
                 COLUMN 64,registros             USING "######&"
        ELSE
           SKIP 1 LINE
           LET cta = g.subcuenta

           PRINT COLUMN 01,g.siefore             USING "##",         --c22-6
                 COLUMN 04,g.folio               USING "######&",
                 COLUMN 12,g.tipo_movimiento     USING "##",
                 COLUMN 15,g.subcuenta           USING "##",
                 COLUMN 18,subct ,
                 COLUMN 24,g.monto_en_pesos      USING "###,###,###.######",
                 COLUMN 46,g.monto_en_acciones   USING "###,###,###.######",
                 COLUMN 64,registros             USING "######&"
        END IF


 ---    AFTER GROUP OF g.folio
 ---       PRINT COLUMN 02,"SUBTOTAL : ",
 ---             COLUMN 24, fsuma_pesos USING "###,###,###.######",
 ---             COLUMN 46, fsuma_acciones USING "###,###,###.######",
 ---             COLUMN 64, fregistros     USING "#######"

      PAGE TRAILER
        SKIP 2 LINES
        PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
        PAUSE "Presione enter para continuar...."

      ON LAST ROW
           PRINT COLUMN 01,"__________________________________________________________________"
           PRINT
           PRINT COLUMN 02,"SUBTOTAL : ",
                 COLUMN 24, fsuma_pesos USING "###,###,###.######",
                 COLUMN 46, fsuma_acciones USING "###,###,###.######",
                 COLUMN 64, fregistros     USING "#######"

        PRINT COLUMN 2, "GRAN TOTAL : ",
              COLUMN 24,total_en_pesos    USING "###,###,###.######",
              COLUMN 46,total_en_acciones USING "###,###,###.######",
              COLUMN 64,tot_registros     USING "######&"

        SKIP 1 LINES
{
        PRINT COLUMN 2, "Total de registros encontrados: ",
        COUNT(*)-1 USING "<<<<"
}
        IF opc = "P" THEN
        ELSE
           LET precio = 0
           SELECT a.precio_del_dia INTO precio FROM glo_valor_accion a
           WHERE  a.fecha_valuacion = fecha_rcv
           AND    a.codigo_siefore = 1              ----jerry 

           PRINT COLUMN 02,"Fecha Liquidacion : ",fecha_rcv USING "dd/mm/yyyy",
                 " Precio Accion : ",precio USING "###&.&&&&&&"
        END IF

END REPORT
