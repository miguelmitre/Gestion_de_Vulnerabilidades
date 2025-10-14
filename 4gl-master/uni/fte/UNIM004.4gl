###############################################################################
#Proyecto          => AFORE (MEXICO)                                          #
#Propietario       => E.F.P  					              #
#Programa UNIM004  => CONSULTA DE PROCESO DE UNIFICACION DE CUENTAS           #
#Sistema           => UNI  					              #
#Autor             => Armando Rodriguez Castroparedes                         #
#Fecha             => 18 de diciembre 2003                                    #
#Modificado por    => OMAR SANDOVAL BADILLO                                   #
#Fecha modifica    => 18 de marzo de 2005                                     #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ                         #
#Fecha modifica    => 29 de marzo de 2005                                     #
###############################################################################
DATABASE safre_af
GLOBALS
    DEFINE seg_modulo  RECORD LIKE seg_modulo.*

    DEFINE enter                 CHAR(1),
           g_usuario             CHAR(8),
           aux_pausa             CHAR(01),
           xnss                  CHAR(11),
           HORA                  CHAR(08),
           HOY                   DATE,
           xfecha                DATE,
           xfolio                INTEGER,
           vfolio                INTEGER,
           rechazo               SMALLINT,
           g_afore               RECORD LIKE tab_afore_local.*,
	   ejecuta               CHAR(300)
         
   DEFINE arr_2  ARRAY[25000] OF RECORD
          nss_uni              CHAR(11),
          num_ctas_asoc        SMALLINT,
          cve_afo_recep        CHAR(03),
          fecha_envio          DATE,
          fecha_resp           DATE,
          folio                INTEGER,
          estado               SMALLINT,
          desc_estado          CHAR(20)
   END RECORD

   DEFINE reg_02 ARRAY[5000] OF RECORD
          nss_cta1           CHAR(11),
          nss_uni            CHAR(11)
   END RECORD

   DEFINE reg_03 ARRAY[5000] OF RECORD
          subcuenta          SMALLINT,
          siefore            SMALLINT,
          monto_acciones     DECIMAL(18,6),
          monto_pesos        DECIMAL(18,6)
   END RECORD

   DEFINE vsaldo         CHAR(150),
          detecta        CHAR(150)
   DEFINE total_monto    DECIMAL(16,6)

   DEFINE x_programa   CHAR(07)
END GLOBALS
#####################################################################
MAIN

   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      COMMENT LINE LAST

   DEFER INTERRUPT

   LET x_programa = "UNIM004"

   LET detecta = " SELECT a.nss_cta1,",
                         "a.nss_uni,",
                         "a.folio",
                 " FROM   uni_unificado a",
                 " WHERE  a.estado in(95,96,100)",
                 " GROUP BY 1,2,3",
                 " ORDER BY 3"
   PREPARE actualiza FROM detecta

   LET vsaldo      = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "
   PREPARE saldo_dia FROM vsaldo

   CALL STARTLOG("UNIM004.log")
   CALL inicio()
   CALL UNIC022(x_programa)
   #CALL actualiza_complementarias()
   CALL proceso_principal()

END MAIN
#####################################################################
FUNCTION actualiza_complementarias()

   DEFINE reg_01 ARRAY[9000] OF RECORD
          nss_cta1 CHAR(11),
          nss_uni  CHAR(11)
   --       folio    INTEGER
   END RECORD

   DEFINE i           INTEGER ,
          vsubcuenta  SMALLINT,
          vgrupo      SMALLINT

   DEFINE xsubcuenta  SMALLINT,
          xsiefore    SMALLINT,
          xacciones   DECIMAL(18,6),
          xpesos      DECIMAL(18,6)


   LET i = 1
   LET vsubcuenta = 0
   LET vgrupo     = 0

   DECLARE query_com CURSOR FOR actualiza
   FOREACH query_com INTO reg_01[i].*
      DECLARE s_saldo CURSOR FOR saldo_dia
      FOREACH s_saldo USING reg_01[i].nss_cta1,
                            vsubcuenta,
                            vgrupo,
                            HOY
                      INTO  xsubcuenta,
                            xsiefore,
                            xacciones,
                            xpesos

         IF xacciones > 0 THEN
            UPDATE uni_unificado
            SET    estado = 60
            WHERE  nss_cta1 = reg_01[i].nss_cta1
            AND    nss_uni  = reg_01[i].nss_uni
            #AND    folio    = reg_01[i].folio
         ELSE
            IF xsubcuenta = 14 AND xpesos > 0 THEN
               UPDATE uni_unificado
               SET    estado = 60
               WHERE  nss_cta1 = reg_01[i].nss_cta1
               AND    nss_uni  = reg_01[i].nss_uni
               #AND    folio    = reg_01[i].folio
            END IF
         END IF
      END FOREACH
      LET i = i + 1
   END FOREACH

END FUNCTION
#####################################################################
FUNCTION complementarias()

   DEFINE x_busca   CHAR(200)
   DEFINE vnss_cta1 CHAR(11)
   DEFINE sql_com   CHAR(500)
   DEFINE sql_com1  CHAR(500)
   DEFINE i         SMALLINT

   DEFINE sql_stat     INTEGER,
          item_row_cnt SMALLINT,
          row_cnt      SMALLINT,
          cont_inp     SMALLINT,
          cur_row      SMALLINT,
          scr_row      SMALLINT

   OPEN WINDOW ventana_com AT 6,2 WITH FORM "UNIM0045" ATTRIBUTE(BORDER)
   DISPLAY " UNIM004         UNIFICACION DE CUENTAS COMPLEMENTARIAS                        " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY " <Esc> Consulta            <Ctrl-F> Cambia pantalla " AT 1,3
   DISPLAY " UNIFICADO " AT 4,5 ATTRIBUTE(REVERSE)

   CONSTRUCT BY NAME x_busca ON nss_cta1

      ON KEY (INTERRUPT)
         LET  int_flag = TRUE
         EXIT CONSTRUCT

      ON KEY (ESC)
         LET int_flag = FALSE
         EXIT CONSTRUCT

   END CONSTRUCT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..."
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      RETURN
   END IF

   ERROR "PROCESANDO INFORMACION ..."
   LET sql_com = " SELECT a.nss_cta1,",
                         "a.nss_uni",
                 " FROM   uni_unificado a ",
                 " WHERE ",x_busca CLIPPED,
                 " AND    a.estado = 60 "

   PREPARE complementa FROM sql_com
   DECLARE query_com3 CURSOR FOR complementa

   LET i = 1

   FOREACH query_com3 INTO reg_02[i].*
      LET i = i + 1
   END FOREACH
   LET i = i - 1

   IF i >= 1 THEN
      CALL SET_COUNT(i)
      DISPLAY "     UNIFICADO       UNIFICADOR           TOTAL MONTO A TRASPASAR              " AT 4,1 ATTRIBUTE(REVERSE)
      DISPLAY "    SUBCUENTA  SIEFORE      MONTO EN ACCIONES           MONTO EN PESOS         " AT 11,1 ATTRIBUTE(REVERSE)

      LET cont_inp = TRUE
      WHILE (cont_inp = TRUE)
         INPUT ARRAY reg_02 WITHOUT DEFAULTS FROM scr_3.*
         ATTRIBUTES(MAXCOUNT=i,COUNT=i)

            AFTER ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()

               DISPLAY reg_02[cur_row].* TO scr_3[scr_row].*

            BEFORE ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()

               IF (cur_row = i + 1) THEN
                  LET cont_inp = TRUE
               ELSE
                  LET scr_row = SCR_LINE()

                  DISPLAY reg_02[cur_row].* TO scr_3[scr_row].*
                  ATTRIBUTE(REVERSE)

                  LET cont_inp = FALSE

                  CALL proc_items(cur_row)
                       RETURNING sql_stat,
                                 item_row_cnt
            END IF

            ON KEY(CONTROL-F)
               CALL disp_array_items(item_row_cnt)

            ON KEY (CONTROL-C)
               INITIALIZE sql_com TO NULL
               CLEAR FORM
               EXIT INPUT
         END INPUT
      END WHILE
   ELSE
      CLEAR FORM
      ERROR "NO EXISTEN REGISTROS ..." ATTRIBUTE(NORMAL)
   END IF

   CLOSE WINDOW ventana_com

END FUNCTION
#####################################################
FUNCTION proc_items(p_cur_row)

   DEFINE sql_status INTEGER,
          p_cur_row SMALLINT,
          item_row_cnt SMALLINT

   CALL sel_ord_item(p_cur_row)
        RETURNING sql_status,
                 item_row_cnt

   CALL disp_four_items()

   RETURN sql_status,
         item_row_cnt

END FUNCTION
#####################################################################
FUNCTION sel_ord_item(p_cur_row)

   DEFINE sql_status  INTEGER,
          p_cur_row   SMALLINT,
          row_cnt     SMALLINT,
          sql_txt     CHAR(250)

   DEFINE vsubcuenta SMALLINT,
          vgrupo     SMALLINT

   LET vsubcuenta = 0
   LET vgrupo     = 0
   LET total_monto = 0

   CALL null_items()

   WHENEVER ERROR CONTINUE
      DECLARE s_saldo1 CURSOR FOR saldo_dia
      OPEN s_saldo1 USING reg_02[p_cur_row].nss_cta1,
                         vsubcuenta,
                         vgrupo,
                         HOY
   WHENEVER ERROR STOP

   LET sql_status = SQLCA.SQLCODE
   LET row_cnt = 1

   WHILE ((NOT sql_status) AND (row_cnt<= 10))
      WHENEVER ERROR CONTINUE
         FETCH s_saldo1 INTO reg_03[row_cnt].*
         IF reg_03[row_cnt].monto_pesos IS NOT NULL THEN
           LET total_monto = total_monto + reg_03[row_cnt].monto_pesos
         END IF
      WHENEVER ERROR STOP
         LET sql_status = SQLCA.SQLCODE
         IF (NOT sql_status) THEN
            LET row_cnt = row_cnt + 1
         END IF
   END WHILE

   DISPLAY total_monto TO scr_5.*

   IF (sql_status = 100) THEN
      LET sql_status = 0
   END IF

   RETURN sql_status,
          row_cnt - 1
END FUNCTION
#####################################################
FUNCTION disp_four_items()
   DEFINE i SMALLINT

   FOR i = 1 TO 5
      DISPLAY reg_03[i].* TO scr_4[i].*
   END FOR

END FUNCTION
#####################################################
FUNCTION null_items()

   DEFINE i SMALLINT

   INITIALIZE reg_03[1].* TO NULL

   FOR i = 1 TO 100
      LET reg_03[i].* = reg_03[1].*
   END FOR
END FUNCTION
###############################################################
FUNCTION disp_array_items(p_item_row_cnt)

   DEFINE p_item_row_cnt SMALLINT

   CALL SET_COUNT(p_item_row_cnt)

   DISPLAY ARRAY reg_03 TO scr_4.*
      ON KEY(INTERRUPT)
         EXIT DISPLAY
   END DISPLAY
END FUNCTION
################################################################
FUNCTION proceso_principal()

   OPEN WINDOW ventana_1 AT 3,2 WITH FORM "UNIM0041" ATTRIBUTE(BORDER)
   DISPLAY " UNIM004            CONSULTA DE UNIFICACION DE CUENTAS                         " AT 4,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd-mm-yyyy" AT 4,67 ATTRIBUTE(REVERSE)
   DISPLAY "   [ Enter ] para iniciar                       [ Control-C ] para salir       " AT 5,1 ATTRIBUTE(REVERSE)

   MENU "CONSULTA "
      COMMAND "Lotes" "Consulta lotes de operacion 21"
         CALL consulta_lotes()
      COMMAND "Primer dia habil" "NSS Unificados a unificar el primer dia habil del mes siguiente"
         CALL proceso_2()
      COMMAND "Rechazos" "Rechazos de notificacion de operacion 22"
         CALL proceso()
      COMMAND "Complementarias" "Unificacion de cuentas complementarias"
         CALL complementarias()
      COMMAND "Salir" "Salir de Programa"
         EXIT MENU
   END MENU
END FUNCTION
#####################################################################
FUNCTION inicio()

   LET rechazo = 0
   LET HOY = TODAY

   LET HORA = TIME

   SELECT *,
          USER 
   INTO   g_afore.*,
          g_usuario 
   FROM   tab_afore_local

   SELECT *
   INTO   seg_modulo.*
   FROM   seg_modulo
   WHERE  modulo_cod = "uni"

END FUNCTION
#####################################################################
FUNCTION consulta_lotes()

   DEFINE arr_c,
          flag,
          tot_uni,
          tot_cta1,
          tot_cta11,
          i                     INTEGER

   DEFINE x_busca        CHAR(100)
   DEFINE txt_2          CHAR(500)

   DISPLAY " Folio del Lote :" AT 6,1
   DISPLAY " Fecha Recepcion:" AT 7,1
   DISPLAY " NSS Unificador: " AT 7,40
   DISPLAY "      nss      tot  afore    fecha    fecha                  estado           " AT 8,1 ATTRIBUTE(REVERSE)
   DISPLAY "  unificador   uni  recep.   envio   recepcion    Folio                       " AT 9,1 ATTRIBUTE(REVERSE)

   LET x_busca = ""
   LET txt_2 = ""

   FOR i = 1 TO 200 
      INITIALIZE arr_2[i] TO NULL
   END FOR

   CLEAR FORM

   CONSTRUCT  x_busca ON a.folio,
                         fecha_recepcion,
                         nss_uni
                    FROM xfolio,
                         xfecha,
                         xnss

      ON KEY (control-c) 
         LET int_flag=FALSE
         EXIT CONSTRUCT

      ON KEY (control-m)
         LET int_flag = FALSE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..."
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      RETURN
   END IF

   LET txt_2 =" SELECT a.nss_uni, ",
                      "a.num_ctas_asoc, ",
                      "a.cve_afo_recep, ",
                      "' ', ",   #fecha_envio
                      "' ', ",   #fecha_resp
                      "a.folio, ",
                      "a.estado, ",
                      "' ' ",   #desc_estado
              " FROM   uni_unificador a, uni_ctr_archivo b ",
              " WHERE  a.folio = b.folio ",
              " AND  ",x_busca CLIPPED,
              " GROUP BY 1,2,3,4,5,6,7,8 ",
              " ORDER BY 1,8 "

   LET txt_2 = txt_2 CLIPPED

   PREPARE pre_1 FROM txt_2
   DECLARE cur_1 CURSOR FOR pre_1

   LET i = 1
   LET tot_uni  = 0
   LET tot_cta1 = 0
   LET tot_cta11= 0

   FOREACH cur_1 INTO arr_2[i].*

      SELECT a.descripcion
      INTO   arr_2[i].desc_estado
      FROM   uni_status a
      WHERE  a.estado = arr_2[i].estado

      SELECT MAX(a.fecha_recepcion)
      INTO  arr_2[i].fecha_envio
      FROM   uni_ctr_archivo a
      WHERE  a.folio = arr_2[i].folio
      AND    a.nombre MATCHES "*uni8"

      SELECT MAX(a.fecha_recepcion)
      INTO   arr_2[i].fecha_resp
      FROM   uni_ctr_archivo a
      WHERE  a.folio = arr_2[i].folio
      AND    a.nombre MATCHES "*uni9"

      SELECT count(*)
      INTO   tot_cta11
      FROM   uni_unificado
      WHERE  folio = arr_2[i].folio
      AND    nss_uni = arr_2[i].nss_uni

      LET tot_cta1 = tot_cta1 + tot_cta11
      LET tot_uni = tot_uni + 1
      LET i = i + 1
   END FOREACH

   DISPLAY " Total unificadores:",tot_uni AT 17,1
   DISPLAY " Total unificados: ",tot_cta1 AT 17,40
   DISPLAY " <Ctrl-U> Detalle Unificador " AT 19,27 ATTRIBUTE(REVERSE)

   IF i = 1 THEN
      INITIALIZE arr_2[i].* TO NULL
      CLEAR FORM
      ERROR "    NO EXISTE REGISTRO "
      RETURN
   END IF

   CALL SET_COUNT(i-1)

   DISPLAY ARRAY arr_2 TO scr_2.*
      ON KEY ( INTERRUPT )
         CLEAR FORM
         EXIT DISPLAY
      ON KEY ( control-m )
         LET i = ARR_CURR()

         CALL detalle_cta1(arr_2[i].folio,
                           arr_2[i].nss_uni,
                           arr_2[i].num_ctas_asoc)
      ON KEY ( control-u )
         LET i = ARR_CURR()

         CALL consulta(arr_2[i].nss_uni,
                       arr_2[i].folio,
                       arr_2[i].estado)

      ON KEY ( control-p )
         CALL imprime(i)
   END DISPLAY

   DISPLAY "                 " AT 6,1
   DISPLAY "                 " AT 7,1
   DISPLAY "                 " AT 7,40
   DISPLAY "                                                                             " AT 8,1
   DISPLAY "                                                                             " AT 9,1
   DISPLAY "                                                                             " AT 17,1
   DISPLAY "                             " AT 19,27

END FUNCTION
#####################################################
FUNCTION imprime(pos)

   DEFINE pos                SMALLINT,
          i                  SMALLINT,
          g_impre            CHAR(300),
          g_lista            CHAR(300),
          vruta_listado      CHAR(300)

   DEFINE g_reg   RECORD
          nss_uni              CHAR(11),
          num_ctas_asoc        SMALLINT,
          cve_afo_recep        CHAR(03),
          fecha_envio          DATE,
          fecha_resp           DATE,
          folio                INTEGER,
          estado               SMALLINT,
          desc_estado          CHAR(20)
   END RECORD

   ERROR "PROCESANDO INFORMACION ..."

   SELECT ruta_listados
   INTO   vruta_listado
   FROM   seg_modulo
   WHERE  modulo_cod = "uni"

   LET g_impre = vruta_listado CLIPPED,"/",
                  "REPORTE_0"

   START REPORT impresion_0 TO g_impre
      FOR i = 1 TO (pos+1)
        LET g_reg.nss_uni       = arr_2[i].nss_uni  
        LET g_reg.num_ctas_asoc = arr_2[i].num_ctas_asoc
        LET g_reg.cve_afo_recep = arr_2[i].cve_afo_recep  
        LET g_reg.fecha_envio   = arr_2[i].fecha_envio
        LET g_reg.fecha_resp   = arr_2[i].fecha_resp
        LET g_reg.folio         = arr_2[i].folio
        LET g_reg.estado        = arr_2[i].estado
        LET g_reg.desc_estado   = " "

        IF g_reg.nss_uni IS NULL THEN
           EXIT FOR
        END IF

        OUTPUT TO REPORT impresion_0(g_reg.*)
     END FOR
   FINISH REPORT impresion_0
{
   ERROR "LISTADO GENERADO ..."
   SLEEP 2
   ERROR ""

   #LET g_lista = "lp ",g_impre
   #LET g_lista = "vi ",g_impre
   RUN g_lista
}   
END FUNCTION
###############################################################################
REPORT impresion_0(g_reg)

   DEFINE g_reg   RECORD
          nss_uni              CHAR(11),
          num_ctas_asoc        SMALLINT,
          cve_afo_recep        CHAR(03),
          fecha_envio          DATE,
          fecha_resp           DATE,
          folio                INTEGER,
          estado               SMALLINT,
          desc_estado          CHAR(20)
   END RECORD

   OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   2

   FORMAT
      ON EVERY ROW
         PRINT COLUMN 001,g_reg.nss_uni ,"|",
                          g_reg.num_ctas_asoc USING "###","|",
                          g_reg.cve_afo_recep,"|",
                          g_reg.fecha_envio USING "DD/MM/YYYY","|",
                          g_reg.fecha_resp USING "DD/MM/YYYY","|",
                          g_reg.folio USING "####","|",
                          g_reg.estado USING "###","|",
                          g_reg.desc_estado CLIPPED,"|"
END REPORT
###############################################################################
FUNCTION detalle_cta1(vfolio,vnss,vtotal)

   DEFINE l_reg ARRAY[2000] OF RECORD
          nss            CHAR(11),
          tipo_ent       CHAR(02),
          cve_ent        CHAR(3),
          nombre         CHAR(50),
          fnotifica      DATE,
          diag_unifica   CHAR(2),
          estado         SMALLINT,
          desc_estado    CHAR(30)
   END RECORD

   DEFINE vfolio         INTEGER
   DEFINE vnss           CHAR(11)
   DEFINE vtotal         INTEGER
   DEFINE vnombre_nss    CHAR(50)
   DEFINE vfoliol        INTEGER
   DEFINE xx             INTEGER
   DEFINE vpaterno       CHAR(40)
   DEFINE vmaterno       CHAR(40)
   DEFINE vnombre        CHAR(40)
   DEFINE vnombre_imss   CHAR(40)
   DEFINE i   	          SMALLINT

   OPEN WINDOW unim0042 AT 8,2 WITH FORM "UNIM0042"
   DISPLAY "                        DATOS DE NSS UNIFICADO                                " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY "    nss       afore     nombre unificado        fecha   diag      estado      " AT 4,1 ATTRIBUTE(REVERSE)
   DISPLAY " unificado   tipo/cve                         recepcion uni                   " AT 5,1 ATTRIBUTE(REVERSE)
   DISPLAY " <Ctrl-U> Detalle de unificado" AT 13,27 ATTRIBUTE(REVERSE)

   DISPLAY BY NAME vnss
   DISPLAY BY NAME vtotal

   SELECT paterno_uni,
          materno_uni,
          nombre_uni,
          nombre_imss_uni
   INTO   vpaterno,
          vmaterno,
          vnombre,
          vnombre_imss
   FROM   uni_unificador
   WHERE  nss_uni  = vnss
   AND    folio    = vfolio 

   LET vnombre_nss = vpaterno CLIPPED," ",
                     vmaterno CLIPPED," ",
                     vnombre CLIPPED

   IF (vnombre_nss IS NULL OR
       vnombre_nss MATCHES " *" ) THEN
      LET vnombre_nss = vnombre_imss
   END IF 

   LET vpaterno = ""
   LET vmaterno = ""
   LET vnombre  = ""
   LET vnombre_imss  = ""

   DISPLAY BY NAME vnombre_nss

   DECLARE det_1 CURSOR FOR
   SELECT nss_cta1,
          tipo_ent_cta1,
          cve_ent_cta1,
          " ",
          fnotifica,  --fnotifica
          diag_unifica,
          estado,
          ""          --desc estado
   FROM   uni_unificado
   WHERE  folio   = vfolio 
   AND    nss_uni = vnss
   ORDER BY 1

   LET i = 1
   FOREACH det_1 INTO l_reg[i].*
      SELECT a.descripcion
      INTO   l_reg[i].desc_estado
      FROM   uni_status a
      WHERE  a.estado = l_reg[i].estado

      SELECT paterno_cta1,
             materno_cta1,
             nombre_cta1,
             nombre_imss_cta1
      INTO   vpaterno,
             vmaterno,
             vnombre,
             vnombre_imss
      FROM   uni_unificado
      WHERE  nss_cta1 = l_reg[i].nss
      AND    folio   = vfolio 

      LET l_reg[i].nombre = vpaterno CLIPPED," ",
                            vmaterno CLIPPED," ",
                            vnombre CLIPPED

      IF (l_reg[i].nombre IS NULL OR
          l_reg[i].nombre MATCHES " *" ) THEN
         LET l_reg[i].nombre = vnombre_imss
      END IF 
      LET i = i + 1
   END FOREACH

   CALL SET_COUNT(i-1)

   DISPLAY ARRAY l_reg TO scr_6.*
      ON KEY ( control-m )
         INITIALIZE l_reg TO NULL
         FOR i = 1 TO 5
             DISPLAY l_reg[i].* TO scr_6[i].*
         END FOR
         EXIT DISPLAY

      ON KEY ( control-u )
         LET i = ARR_CURR()

         CALL unificados(l_reg[i].nss,
                         vfolio,
                         l_reg[i].estado)

   END DISPLAY
   CLOSE WINDOW unim0042
END FUNCTION 
##########################################################################
FUNCTION consulta(x_nss_uni,
                  x_folio,
                  x_estado)

   DEFINE arr_c,
          flag,
          i                  INTEGER,
          opc                CHAR(1)

   DEFINE x_busca1       CHAR(100)
   DEFINE txt_1          CHAR(500)

   DEFINE arr_1  ARRAY[3000] OF RECORD
          folio                INTEGER,
          folio_liquida        INTEGER,
          nss_uni              CHAR(11),
          rfc_uni              CHAR(13),
          paterno_uni          CHAR(40),
          materno_uni          CHAR(40),
          nombre_uni           CHAR(40),
          nombre_imss_uni      CHAR(50),
          tipo_ent_nss         CHAR(2),
          cve_ent_nss          CHAR(3),
          desc_afore           CHAR(25),
          curp_uni             CHAR(18),
          sexo_uni             CHAR(1),
          desc_sexo            CHAR(10),
          cve_afo_recep        CHAR(3),
          desc_recep           CHAR(25),
          num_ctas_asoc        SMALLINT,
          status_convoca       CHAR(1),
          ident_movimiento     CHAR(2),
          desc_movimiento      CHAR(25),
          cve_afo_aclara       CHAR(3),
          estado               SMALLINT,
          desc_estado          CHAR(25),
          fliquida             DATE,
          fnotifica            DATE
   END RECORD

   DEFINE x_nss_uni        CHAR(11),
          x_folio          INTEGER,
          x_estado         SMALLINT

   OPEN WINDOW unim0012 AT 2,2 WITH FORM "UNIM0012" ATTRIBUTE(BORDER)
   DISPLAY " UNIM001        SOLICITUD DE UNIFICACION DE CUENTAS                            " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   DISPLAY "CONSULTA SOLICITUD" AT 1,59 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-C]  Salir                                                        " AT 2,1 

   LET txt_1 =" SELECT a.folio, ",
                      "a.folio_liquida, ",
                      "a.nss_uni, ",
                      "a.rfc_uni, ",
                      "a.paterno_uni, ",
                      "a.materno_uni, ",
                      "a.nombre_uni, ",
                      "a.nombre_imss_uni, ",
                      "a.tipo_ent_nss, ",
                      "a.cve_ent_nss, ",
                      "' ', ",   #desc_afore
                      "a.curp_uni, ",
                      "a.sexo_uni, ",
                      "' ', ",   #desc_sexo
                      "a.cve_afo_recep, ",
                      "' ', ",   #desc_recep
                      "a.num_ctas_asoc, ",
                      "a.status_convoca, ",
                      "a.ident_movimiento, ",
                      "' ', ",   #desc_movimiento
                      "a.cve_afo_aclara, ",
                      "a.estado, ",
                      "' ', ",   #desc_estado
                      "a.fliquida, ",
                      "a.fnotifica ",
              " FROM   uni_unificador a ",
              " WHERE  a.folio = ",x_folio,
              " AND    a.nss_uni = ","'",x_nss_uni,"'",
              " AND    a.estado = ",x_estado 

   PREPARE pre_6 FROM txt_1
   DECLARE cur_6 CURSOR FOR pre_6

   LET i = 1
   FOREACH cur_6 INTO arr_1[i].*

      SELECT sexo_desc
      INTO   arr_1[i].desc_sexo
      FROM   tab_sexo
      WHERE  sexo_cod = arr_1[i].sexo_uni

      SELECT a.afore_desc
      INTO   arr_1[i].desc_afore
      FROM   tab_afore a
      WHERE  a.afore_cod = arr_1[i].cve_ent_nss

      SELECT a.afore_desc
      INTO   arr_1[i].desc_recep
      FROM   tab_afore a
      WHERE  a.afore_cod = arr_1[i].cve_afo_recep

      SELECT a.descripcion
      INTO   arr_1[i].desc_estado
      FROM   uni_status a
      WHERE  a.estado = arr_1[i].estado

      IF arr_1[i].ident_movimiento = "01" THEN
         LET arr_1[i].desc_movimiento = "INTRAFORE"
      ELSE
         LET arr_1[i].desc_movimiento = "EXTRAFORE"
      END IF

      LET i = i + 1
   END FOREACH

   IF i = 1 THEN
      CLEAR FORM
       ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
       CLOSE WINDOW unim0012
       RETURN
   END IF

   CALL SET_COUNT(i-1)

   DISPLAY ARRAY arr_1 TO scr_1.*

      ON KEY ( INTERRUPT )
         EXIT DISPLAY
   END DISPLAY
   CLOSE WINDOW unim0012
END FUNCTION
#################################################################################
FUNCTION unificados(x_nss_cta1,
                    x_folio,
                    x_estado)

   DEFINE arr_u,
          flag,
           i                     INTEGER

   DEFINE x_busca1       CHAR(100)
   DEFINE txt_1          CHAR(500)

   DEFINE arr_1  ARRAY[3000] OF RECORD
          folio                INTEGER,
          folio_liquida        INTEGER,
          nss_cta1             CHAR(11),
          rfc_cta1             CHAR(13),
          paterno_cta1         CHAR(40),
          materno_cta1         CHAR(40),
          nombre_cta1          CHAR(40),
          nombre_imss_cta1     CHAR(50),
          tipo_ent_cta1        CHAR(2),
          cve_ent_cta1         CHAR(3),
          desc_afore           CHAR(25),
          curp_cta1            CHAR(18),
          sexo_cta1            CHAR(1),
          desc_sexo            CHAR(10),
          diag_unifica         CHAR(02),
          status_convoca       CHAR(1),
          desc_movimiento      CHAR(25),
          estado               SMALLINT,
          desc_estado          CHAR(25),
          fliquida             DATE,
          fnotifica            DATE
   END RECORD

   DEFINE x_nss_cta1           CHAR(11),
          x_folio              INTEGER,
          x_estado             SMALLINT

   OPEN WINDOW unim0012 AT 2,2 WITH FORM "UNIM0014" ATTRIBUTE(BORDER)
   DISPLAY " UNIM001        SOLICITUD DE UNIFICACION DE CUENTAS                            " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   DISPLAY "CONSULTA SOLICITUD" AT 1,59 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-C]  Salir                                                          " AT 2,1 

   LET txt_1 =" SELECT a.folio, ",
                      "a.folio_liquida, ",
                      "a.nss_cta1, ",
                      "a.rfc_cta1, ",
                      "a.paterno_cta1, ",
                      "a.materno_cta1, ",
                      "a.nombre_cta1, ",
                      "a.nombre_imss_cta1, ",
                      "a.tipo_ent_cta1, ",
                      "a.cve_ent_cta1, ",
                      "' ', ",   #desc_afore
                      "a.curp_cta1, ",
                      "a.sexo_cta1, ",
                      "' ', ",   #desc_sexo
                      "a.diag_unifica, ",
                      "a.status_convoca, ",
                      "a.cve_afo_aclara, ",
                      "a.estado, ",
                      "' ', ",   #desc_estado
                      "a.fliquida, ",
                      "a.fnotifica ",
              " FROM   uni_unificado a ",
              " WHERE  a.folio = ",x_folio,
              " AND    a.nss_cta1 = ","'",x_nss_cta1,"'",
              " AND    a.estado = ",x_estado

   PREPARE pre_7 FROM txt_1
   DECLARE cur_7 CURSOR FOR pre_7

   LET i = 1
   FOREACH cur_7 INTO arr_1[i].*
      SELECT sexo_desc
      INTO   arr_1[i].desc_sexo
      FROM   tab_sexo
      WHERE  sexo_cod = arr_1[i].sexo_cta1

      SELECT a.afore_desc
      INTO   arr_1[i].desc_afore
      FROM   tab_afore a
      WHERE  a.afore_cod = arr_1[i].cve_ent_cta1

      SELECT a.descripcion
      INTO   arr_1[i].desc_estado
      FROM   uni_status a
      WHERE  a.estado = arr_1[i].estado

      LET i = i + 1
   END FOREACH

   IF i = 1 THEN
      CLEAR FORM
      ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
      CLOSE WINDOW unim0012
      RETURN
   END IF

   CALL SET_COUNT(i-1)

   DISPLAY ARRAY arr_1 TO scr_1.*
      ON KEY ( INTERRUPT )
         EXIT DISPLAY
   END DISPLAY
   CLOSE WINDOW unim0012
END FUNCTION
