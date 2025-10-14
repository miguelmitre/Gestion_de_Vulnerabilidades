###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => EFP          				              #
#Programa          => REPORTE DE ESTADOS DE CUENTA EMITIDOS POR DIA           #
#Fecha             => 17 julio 2002                                           #
#Actualizado       => ARMANDO RODRIGUEZ CASTROPAREDES.                        #
###############################################################################
DATABASE safre_af
GLOBALS
   DEFINE enter char(001)

   DEFINE emi_dia  RECORD
         tipo        SMALLINT,
         fecha       DATE,
         total       SMALLINT
   END RECORD

   DEFINE ga_1   ARRAY[5000] OF RECORD
      tipo         SMALLINT,
      fecha        DATE,
      total        SMALLINT
   END RECORD

   DEFINE ga_2   ARRAY[5000] OF RECORD
      folio         INTEGER,
      nss           CHAR(11),
      nombre        CHAR(30),
      fecha_ini     DATE,
      fecha_fin     DATE
   END RECORD

   DEFINE liq_cta RECORD
          nss              CHAR(11),
          subcuenta        SMALLINT,
          pesos            DECIMAL(16,6),
          acciones         DECIMAL(16,6)
   END RECORD

   DEFINE tot_sub RECORD
         subcuenta        SMALLINT,
         pesos            DECIMAL(16,6),
         acciones         DECIMAL(16,6)
   END RECORD

   DEFINE total RECORD
      pesos            DECIMAL(16,6),
      acciones         DECIMAL(16,6)
   END RECORD

   DEFINE #glo #integer
         cont              ,
         cont1             ,
         i                 ,
         vfolio2           , 
         vfolio            , 
         tot_registros     INTEGER

   DEFINE #glo #decimal
         total_pesos       ,
         total_acciones    DECIMAL(16,6)

   DEFINE #glo #date
         vfecha            ,
         HOY               DATE

   DEFINE #glo #char
       G_LISTA	       	CHAR(500),
       G_LISTA1	       	CHAR(500),
       G_LISTA2	       	CHAR(100),
       G_LISTA3	       	CHAR(100),
       G_LISTA4	       	CHAR(100),
       imprime        	CHAR(100),
       borra          	CHAR(100),
       cat            	CHAR(500),
       aux_pausa       	CHAR(001),
       char            	CHAR(001),
       bold              CHAR(032),
       raya              CHAR(120),
       cla_sel           CHAR(250),
       nss_unifica       CHAR(011),
       cve_ent_uni       CHAR(003),
       cve_ent_cta       CHAR(003),
       vcodigo_afore     CHAR(003),
       vrazon_social     CHAR(050)

   DEFINE g_paramgrales   	RECORD LIKE seg_modulo.*
END GLOBALS

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG("CTAL002.log")

    CALL inicio()  #i
    CALL proceso_principal()  #pp
END MAIN

FUNCTION inicio()
#i---------------

    LET HOY = TODAY

    SELECT codigo_afore,
           razon_social
    INTO   vcodigo_afore,
           vrazon_social
    FROM   tab_afore_local

    LET bold  = '\033e\033(s218T\033(s12H\033(s7B'
    LET raya = '_____________________________________________',
               '_____________________________________________'

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CTAL0021" ATTRIBUTE(BORDER)
    DISPLAY " CTAL002       REPORTE DE ESTADOS DE CUENTA EMITIDOS POR DIA                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "      [ Esc ] Iniciar                                 [ Ctrl-C ] Salir         " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfecha

      AFTER FIELD vfecha
          SELECT "X"
          FROM   cta_ctr_proceso
          WHERE  factualiza = vfecha
          GROUP BY 1

          IF STATUS = NOTFOUND THEN
             ERROR " NO HAY ESTADOS DE CUENTA EMITIDOS EN EL DIA"
             SLEEP 3
	     EXIT INPUT
	     EXIT PROGRAM
          END IF

      ON KEY (ESC)
          SELECT "X"
          FROM   cta_ctr_proceso
          WHERE  factualiza = vfecha
          GROUP BY 1

          IF STATUS = NOTFOUND THEN
             ERROR " NO HAY ESTADOS DE CUENTA EMITIDOS EN EL DIA"
             SLEEP 3
	     EXIT INPUT
	     EXIT PROGRAM
          END IF
          ERROR " PROCESANDO INFORMACION " 

          DISPLAY "      [ Ctrl-V ] Cambia pantalla                      [ Ctrl-C ] Salir         " AT 1,1 ATTRIBUTE(REVERSE)

          CALL muestra_registros() #mr

      ON KEY (INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 2
         EXIT PROGRAM

    END INPUT

    CLEAR WINDOW ventana_1
    CLOSE WINDOW ventana_1

END FUNCTION

FUNCTION pregunta()
    ERROR ""
    PROMPT " DESEA GENERAR EL ARCHIVO  S/N  ?  " for CHAR aux_pausa
END FUNCTION                                                         

FUNCTION muestra_registros()
#mr

   DEFINE i    SMALLINT
   
   DEFINE sql_stat            INTEGER,
          item_row_cnt        SMALLINT,
          cont_inp            SMALLINT,
          cur_row             SMALLINT,
          scr_row             SMALLINT,
          p_cur_row           SMALLINT

   OPEN WINDOW ventana2 AT 6,2 WITH FORM "CTAL0022" ATTRIBUTES(BORDER)
   DISPLAY " [Ctrl-P] Imprime " AT 1,60

   DECLARE cur_reg CURSOR FOR
   SELECT tipo_informe,
          factualiza,
          COUNT(*)
   FROM   cta_ctr_proceso
   WHERE  factualiza = vfecha
   GROUP BY 1,2
   ORDER BY 1,2

   LET i = 1

   FOREACH cur_reg INTO ga_1[i].*
      LET i = i + 1
   END FOREACH

   LET i = i - 1

   IF (i) >= 1 THEN
      CALL SET_COUNT(i)
      LET cont_inp = TRUE

      DISPLAY " Tipo inf.   Fecha Emision   Total " AT 2,1 ATTRIBUTE(REVERSE)
      DISPLAY " Folio      NSS                 Nombre               F. inicio    F. final     " AT 9,1 ATTRIBUTE(REVERSE)

      WHILE (cont_inp = TRUE)
         INPUT ARRAY ga_1 WITHOUT DEFAULTS FROM scr_1.*
         ATTRIBUTES(MAXCOUNT = i,COUNT = i) 

            AFTER ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()

               DISPLAY ga_1[cur_row].* TO scr_1[scr_row].*

            BEFORE ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()

               IF (cur_row = i + 1) THEN
                  LET cont_inp = TRUE
                  #EXIT INPUT
               ELSE
                  LET scr_row = SCR_LINE()

                  DISPLAY ga_1[cur_row].* TO scr_1[scr_row].*
                  ATTRIBUTE (REVERSE)
 
                  LET cont_inp = FALSE

                  CALL proc_items(cur_row)
                       RETURNING sql_stat,
                                 item_row_cnt
               END IF

            ON KEY(CONTROL-V)
               CALL disp_array_items(item_row_cnt)

            ON KEY(CONTROL-P)
               CALL genera_reporte()    #gr

               ERROR " REPORTE GENERADO " 
               SLEEP 2
               ERROR ""
               EXIT INPUT

            ON KEY (CONTROL-C)
               EXIT INPUT
         END INPUT
      END WHILE
   ELSE
      ERROR "NO SE ENCONTRARON REGISTROS PARA MOSTRAR ..."
      SLEEP 2
      ERROR ""
   END IF

   CLOSE WINDOW ventana2   
END FUNCTION

FUNCTION disp_array_items(p_item_row_cnt)

   DEFINE p_item_row_cnt SMALLINT

   CALL SET_COUNT(p_item_row_cnt)

   DISPLAY ARRAY ga_2 TO scr_2.*
      ON KEY(ESC)
         EXIT DISPLAY
   END DISPLAY
END FUNCTION

FUNCTION proc_items(p_cur_row)

   DEFINE sql_stat      INTEGER,
          p_cur_row     SMALLINT,
          item_row_cnt  SMALLINT

   CALL sel_ord_item(p_cur_row)
        RETURNING sql_stat,
                  item_row_cnt
 
   CALL disp_four_items()

   RETURN sql_stat,
          item_row_cnt

END FUNCTION

FUNCTION sel_ord_item(p_cur_row)

   DEFINE sql_text   CHAR(650),
          sql_stat   INTEGER,
          p_cur_row  SMALLINT,
          row_cnt    SMALLINT

   DEFINE nom        CHAR(40),
          pat        CHAR(40),
          mat        CHAR(40)

   CALL null_items()

   LET sql_text = " SELECT folio,",
                          "nss,",
                          "'',",
                          "fecha_inicio,",
                          "fecha_fin",
                  " FROM  cta_ctr_proceso ",
                  " WHERE factualiza = ? ",
                  " AND   tipo_informe = ? ",
                  " ORDER BY  1,2,4,5 " CLIPPED

   PREPARE sel_item_stmt FROM sql_text
   DECLARE sel_item_curs CURSOR FOR sel_item_stmt

   WHENEVER ERROR CONTINUE

   OPEN sel_item_curs USING ga_1[p_cur_row].fecha,
                            ga_1[p_cur_row].tipo

   WHENEVER ERROR STOP
   LET sql_stat = SQLCA.SQLCODE
   LET row_cnt = 1

   WHILE ((NOT sql_stat) AND (row_cnt<= 100))
      LET nom = NULL
      LET pat = NULL
      LET mat = NULL
      WHENEVER ERROR CONTINUE
         FETCH sel_item_curs INTO ga_2[row_cnt].*

            SELECT paterno,
                   materno,
                   nombres
            INTO   pat,mat,nom
            FROM   afi_mae_afiliado
            WHERE  n_seguro = ga_2[row_cnt].nss
            group by 1,2,3

            LET ga_2[row_cnt].nombre = nom CLIPPED," ",
                                       pat CLIPPED," ",
                                       mat CLIPPED 
           

      WHENEVER ERROR STOP

      LET sql_stat = SQLCA.SQLCODE
      IF (NOT sql_stat) THEN
         LET row_cnt = row_cnt + 1
      END IF
   END WHILE

   IF (sql_stat = 100) THEN
      LET sql_stat = 0
   END IF

   RETURN sql_stat,
          row_cnt - 1
END FUNCTION

FUNCTION disp_four_items()

   DEFINE i SMALLINT

   FOR i = 1 TO 6
      DISPLAY ga_2[i].* TO scr_2[i].*
   END FOR
END FUNCTION

FUNCTION null_items()

   DEFINE i SMALLINT

   INITIALIZE ga_2[1].* TO NULL

   FOR i = 2 TO 6
      LET ga_2[i].* = ga_2[1].*
      INITIALIZE ga_2[i].* TO NULL
   END FOR
END FUNCTION

FUNCTION genera_reporte()
#gr----------------------
    DEFINE vtotal  INTEGER

    SELECT *
    INTO   g_paramgrales.*
    FROM   seg_modulo
    WHERE  modulo_cod = "cta"

    LET vtotal  = 0
    LET G_LISTA = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
      "emidia" CLIPPED

    INITIALIZE emi_dia  TO NULL 

    START REPORT listado_2 TO G_LISTA

       DECLARE cur_1 CURSOR FOR
       SELECT tipo_informe,
              factualiza,
              count(*)
       FROM   cta_ctr_proceso
       WHERE  factualiza = vfecha
       GROUP BY 1,2
       ORDER BY 1,2

       FOREACH cur_1 INTO emi_dia.*
           OUTPUT TO REPORT listado_2(emi_dia.*) #l2
           LET vtotal  = vtotal + emi_dia.total
       END FOREACH

       FINISH REPORT listado_2

       LET G_LISTA = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
                     "emidia1" CLIPPED

    START REPORT listado_5 TO G_LISTA
       OUTPUT TO REPORT listado_5(vtotal) #l5
    FINISH REPORT listado_5

    LET G_LISTA2 = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,"DIAEMI"
    LET G_LISTA3 = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,"emidia*"
    LET G_LISTA4 = g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,"EMI*"
    LET G_LISTA  = "cd ",g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
                   ";sed -e '/^$/d' ","emidia" CLIPPED," >", "EMI" CLIPPED
    RUN G_LISTA
    
    LET G_LISTA = "cd ",g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
                  ";sed -e '/^$/d' ","emidia1" CLIPPED," >", "EMI1" CLIPPED
    RUN G_LISTA

    LET cat = "cd ",g_paramgrales.ruta_rescate CLIPPED,"/" CLIPPED,
              ";cat EMI EMI1 > DIAEMI"
    RUN cat

    #LET imprime = "vi ",G_LISTA2
    LET imprime = "lp ",G_LISTA2
    RUN imprime
    #LET borra   = "rm ",G_LISTA2
    RUN borra
    LET borra   = "rm ",G_LISTA3
    RUN borra
    LET borra   = "rm ",G_LISTA4
    RUN borra

END FUNCTION
REPORT listado_2(reg_2)
#l2--------------------

    DEFINE reg_2 RECORD 
          tipo       SMALLINT,
          fecha      DATE,
          total      SMALLINT
    END RECORD

    DEFINE vdescripcion CHAR(30)

    OUTPUT
        PAGE LENGTH 60
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
       PRINT bold

       PRINT 
            COLUMN 004, vrazon_social,
            COLUMN 080, HOY USING "DD-MM-YYYY"
            
       PRINT 
            COLUMN 004,"CTAL002",
            COLUMN 029,"REPORTE DE ESTADOS DE CUENTA EMITIDOS POR DIA",
            COLUMN 080, "Pagina  ",pageno USING "##"

       PRINT 
            COLUMN 004,"Fecha Solicitada:  ",vfecha USING "DD-MM-YYYY"

       PRINT #peque,
            COLUMN 003,raya CLIPPED

       PRINT  
       PRINT  
            COLUMN 012,"Tipo ",
            COLUMN 023,"Descripcion ",
            COLUMN 052,"Fecha",
            COLUMN 078,"Total Emitidos"

       PRINT 
            COLUMN 003,raya CLIPPED

    ON EVERY ROW

    LET vdescripcion = ""

    SELECT descripcion
    INTO   vdescripcion
    FROM   tab_tipo_informe
    WHERE  tipo_informe = reg_2.tipo

    PRINT 
       COLUMN 008,reg_2.tipo,
       COLUMN 020,vdescripcion,
       COLUMN 041,reg_2.fecha,
       COLUMN 068,reg_2.total

END REPORT
REPORT listado_5(reg_5)
#l5--------------------

    DEFINE reg_5 RECORD
	total_global  INTEGER
    END RECORD

    OUTPUT
        PAGE LENGTH 3
	     LEFT MARGIN 0
	     RIGHT MARGIN 0
	     TOP MARGIN 0
	     BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW

        PRINT 
        PRINT 
            COLUMN 003,raya CLIPPED
        PRINT 
            COLUMN 005,"TOTAL DE EMISION EN EL DIA",
            COLUMN 068,reg_5.total_global

END REPORT

