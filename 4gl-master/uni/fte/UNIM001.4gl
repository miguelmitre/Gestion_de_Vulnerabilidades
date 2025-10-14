##############################################################################
#Proyecto          => AFORE (MEXICO)                                          #
#Propietario       => E.F.P  					              #
#Programa CTAM003  => CONSULTA DE SOLICITUDES DE UNIFICACION DE CUENTAS       #
#Sistema           => UNI  					              #
#Autor             => Armando Rodriguez Castroparedes                         #
#Fecha             => 28 de junio de 2001                                     #
#Fecha Actualiza   => 27 noviembre 2003                                       #
#Actualizado por   => MIGUEL ANGEL HERNANDEZ MARTINEZ                         #
#Fecha Actualiza   => 28 marzo 2005                                           #
#Actualizado por   => OMAR SANDOVAL BADILLO                                   #
#Fecha Actualiza   => 09 MAYO  2005                                           #
###############################################################################
DATABASE safre_af
GLOBALS

    DEFINE enter                 CHAR(1),
           g_usuario             CHAR(8),
           aux_pausa             CHAR(01),
           HORA                  CHAR(08),
           HOY                   DATE,
           vfolio                INTEGER,
           rechazo               SMALLINT,
           g_afore               RECORD LIKE tab_afore_local.*
         
   DEFINE l_reg_1 ARRAY[2000] OF RECORD
          x              CHAR(1),
          nss            CHAR(11),
          tipo_ent       CHAR(02),
          cve_ent        CHAR(3),
          nombre         CHAR(50),
          cve_receptora  CHAR(3)
   END RECORD

   DEFINE l_reg_2 ARRAY[2000] OF RECORD
          nss_cta1       CHAR(11),
          tipo_ent_cta1  CHAR(02),
          cve_ent_cta1   CHAR(3),
          y              CHAR(1)
   END RECORD

END GLOBALS
###############################################################################
MAIN

    OPTIONS
       PROMPT LINE LAST,
       INPUT WRAP,
       COMMENT LINE LAST

    DEFER INTERRUPT

    CALL STARTLOG("UNIM001.log")
    CALL inicio()
    CALL proceso_principal()

END MAIN
###############################################################################
FUNCTION proceso_principal()

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIM0011" ATTRIBUTE(BORDER)
    DISPLAY " UNIM001    CONSULTA DE SOLICITUDES DE UNIFICACION DE CUENTAS                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "CONSULTA "
        COMMAND "Folios" "Consulta Folios de ingreso "
           CALL revisa_folio()
           CLEAR FORM
        COMMAND "Por NSS" "Consulta de solicitudes por NSS"
           CALL revisa_nss()
           CLEAR FORM
        COMMAND "Salir" "Salir de Programa"
           EXIT MENU
    END MENU

END FUNCTION
###############################################################################
FUNCTION inicio()

    LET rechazo = 0
    LET HOY = TODAY

    LET HORA = TIME

    SELECT *,
           USER 
    INTO   g_afore.*,
           g_usuario 
    FROM   tab_afore_local

END FUNCTION
################################################################################
FUNCTION revisa_nss()

   DEFINE arr_c,
          flag,
          i              INTEGER

   DEFINE opc     CHAR(1)

   DEFINE x_busca        CHAR(100)
   DEFINE xx_busca       CHAR(20)
   DEFINE txt_2          CHAR(700)
   DEFINE txt_2a         CHAR(700)

   DEFINE nss_uni        CHAR(11),
          nss_cta1       CHAR(11)

   DEFINE tipo           CHAR(10)

   DEFINE nss_uni_x      CHAR(11),
          nss_cta1_x     CHAR(11)

   DEFINE parametro      SMALLINT

   DEFINE arr_2  ARRAY[500] OF RECORD
          folio                INTEGER,
          folio_liquida        INTEGER,
          total_fam            INTEGER,
          total_nss            INTEGER,
          estado               SMALLINT,
          desc_estado          CHAR(20),
          fliquida             DATE,
          fnotifica            DATE
   END RECORD

   INITIALIZE x_busca TO NULL
   INITIALIZE txt_2 TO NULL

   FOR i = 1 TO 200 
      INITIALIZE arr_2[i] TO NULL
   END FOR

   CLEAR FORM
   DISPLAY "UNIFICADOR                      UNIFICADO" AT 4,4
   LET int_flag = FALSE
   LET parametro = 0

   CONSTRUCT x_busca ON nss_uni,
                        nss_cta1
                   FROM nss_uni,
                        nss_cta1

      AFTER FIELD nss_uni
         LET nss_uni_x = GET_FLDBUF(nss_uni)
         IF nss_uni_x IS NOT NULL THEN
            LET tipo = "UNIFICADOR" 
         END IF

      AFTER FIELD nss_cta1
         LET nss_cta1_x = GET_FLDBUF(nss_cta1)
         IF nss_cta1_x IS NOT NULL THEN
            LET tipo = "UNIFICADO" 
         END IF

      AFTER CONSTRUCT
         IF (nss_uni_x IS NULL OR nss_uni_x = "") AND
            (nss_cta1_x  IS NULL OR nss_cta1_x = "") THEN
               ERROR " DEBE INGRESAR UN NSS ..."
               SLEEP 2
               ERROR ""
               NEXT FIELD nss_uni
         END IF

      ON KEY(INTERRUPT) 
         LET  int_flag = TRUE 
         DISPLAY "                                           " AT 4,4
         EXIT CONSTRUCT 

      ON KEY(ESC)
         LET int_flag = FALSE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
      RETURN
   END IF

   DISPLAY "     Utilice las flechas para navegar        [Enter] para ver detalle         " AT 5,1
   DISPLAY "     F O L I O     TOTAL FAM.   E S T A D O                 F E C H A         " AT 6,1 ATTRIBUTE(REVERSE)
   DISPLAY "  NOTIFICA LIQUIDA UNI   CTA  TIPO DESCRIPCION          LIQUIDA    NOTIFICA   " AT 7,1 ATTRIBUTE(REVERSE)

   IF nss_uni_x IS NOT NULL THEN
      LET txt_2a = " FROM   uni_unificador a,OUTER uni_status b ",
                   " WHERE  ",x_busca CLIPPED,
                   " AND    a.estado = b.estado ",
                   " GROUP BY 1,2,5,6,7,8 ",
                   " ORDER BY 1,5 "

   ELSE
      IF nss_cta1_x IS NOT NULL THEN
         LET txt_2a =  " FROM   uni_unificado a,OUTER uni_status b ",
                       " WHERE  ",x_busca CLIPPED,
                       " AND    a.estado = b.estado ",
                       " GROUP BY 1,2,5,6,7,8 ",
                       " ORDER BY 1,5 "
      END IF
   END IF

   LET txt_2 =" SELECT a.folio, ",
                      "a.folio_liquida, ",
                      "count(unique nss_uni), ",
                      "count(*), ",
                      "a.estado, ",
                      "b.descripcion, ",
                      "a.fliquida, ",
                      "a.fnotifica ",
                      txt_2a CLIPPED

   LET txt_2 = txt_2 CLIPPED

   PREPARE pre_1 FROM txt_2
   DECLARE cur_1_tmp CURSOR FOR pre_1

   LET i = 1
   FOREACH cur_1_tmp INTO arr_2[i].*
      LET i = i + 1
   END FOREACH

   IF i = 1 THEN
      INITIALIZE arr_2[i].* TO NULL
      CLEAR FORM
      ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
      RETURN
   END IF

   CASE tipo
      WHEN "UNIFICADOR"
           LET x_busca = nss_uni_x
      WHEN "UNIFICADO"
           LET x_busca = nss_cta1_x
   END CASE
 
   CALL SET_COUNT(i-1)
   DISPLAY ARRAY arr_2 TO scr_2.*

      ON KEY(INTERRUPT)
         DISPLAY "                                           " AT 4,4
         DISPLAY "                                                                               " AT 5,1
         DISPLAY "                                                                               " AT 6,1 
         DISPLAY "                                                                               " AT 7,1 
         EXIT DISPLAY

      ON KEY(CONTROL-M)
         LET i = ARR_CURR()
         IF x_busca = " " OR x_busca IS NULL THEN
            LET x_busca = NULL
         END IF

         CALL detalle_uni(arr_2[i].folio,
                          arr_2[i].estado,
                          arr_2[i].folio_liquida,x_busca)

   END DISPLAY
END FUNCTION
################################################################################
FUNCTION revisa_folio()

   DEFINE arr_c,
          flag,
          ii,
          iii,
          i              INTEGER

   DEFINE x_busca        CHAR(100)
   DEFINE txt_2          CHAR(900)

   DEFINE arr_x2  RECORD
          tipo                 CHAR(15),
          folio                INTEGER,
          folio_liquida        INTEGER,
          total                INTEGER,
          estado               SMALLINT,
          desc_estado          CHAR(20),
          fliquida             DATE,
          fnotifica            DATE
   END RECORD

   DEFINE arr_2  ARRAY[500] OF RECORD
          folio                INTEGER,
          folio_liquida        INTEGER,
          total_fam            INTEGER,
          total_nss            INTEGER,
          estado               SMALLINT,
          desc_estado          CHAR(20),
          fliquida             DATE,
          fnotifica            DATE
   END RECORD

   DEFINE arr_2x  ARRAY[500] OF RECORD
          folio                INTEGER,
          folio_liquida        INTEGER,
          total_fam            INTEGER,
          total_nss            INTEGER,
          estado               SMALLINT,
          desc_estado          CHAR(20),
          fliquida             DATE,
          fnotifica            DATE
   END RECORD

   DEFINE opc CHAR(1)
   DEFINE ban SMALLINT

   INITIALIZE x_busca TO NULL
   INITIALIZE txt_2 TO NULL

   FOR i = 1 TO 200 
      INITIALIZE arr_2[i] TO NULL
   END FOR

   CLEAR FORM

   DISPLAY "                                 " AT 4,4
   DISPLAY " Utilice las flechas para navegar  Detalle Control:[U]Unificador [N]Unificado  " AT 5,1
   DISPLAY "     F O L I O     TOTAL FAM.   E S T A D O                 F E C H A         " AT 6,1 ATTRIBUTE(REVERSE)
   DISPLAY "  NOTIFICA LIQUIDA UNI   CTA  TIPO DESCRIPCION          LIQUIDA    NOTIFICA   " AT 7,1 ATTRIBUTE(REVERSE)
   LET int_flag              = FALSE

   CONSTRUCT BY NAME x_busca ON folio,
                                folio_liquida,
                                estado,
                                fliquida,
                                fnotifica

      ON KEY (INTERRUPT) 
         LET  int_flag = TRUE 
         EXIT CONSTRUCT 

      ON KEY (Esc)
         LET int_flag = FALSE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
      DISPLAY "                                                                               " AT 5,1
      DISPLAY "                                                                               " AT 6,1 
      DISPLAY "                                                                               " AT 7,1 
      RETURN
   END IF

   LET txt_2 = "SELECT 'UNIFICADOR', ",
                      "folio, ",
                      "folio_liquida, ",
                      "count(* )  , ",
                      "estado, ",
                      "fliquida, ",
                      "fnotifica ",
               "FROM   uni_unificador  ",
               " WHERE  ",x_busca CLIPPED,
               " GROUP BY 1,2,3,5,6,7 ",
               "UNION ",
               "SELECT 'UNIFICADO', ",
                      "folio, ",
                      "folio_liquida, ",
                      "count(*)   , ",
                      "estado, ",
                      "fliquida, ",
                      "fnotifica ",
               "FROM   uni_unificado ",
               " WHERE  ",x_busca CLIPPED,
               " GROUP BY 1,2,3,5,6,7 ",
               "ORDER BY 5,1 "
   LET txt_2 = txt_2 CLIPPED

   PREPARE prep_1_tmp FROM txt_2
   DECLARE curs_1_tmp CURSOR FOR prep_1_tmp

   LET i   = 1
   LET ii  = 1
   LET iii = 1

   LET arr_2x[iii].estado = 0

   FOREACH curs_1_tmp INTO arr_x2.tipo,
                           arr_x2.folio,        
                           arr_x2.folio_liquida,
                           arr_x2.total,    
                           arr_x2.estado,       
                           arr_x2.fliquida,     
                           arr_x2.fnotifica     

      SELECT descripcion
      INTO   arr_x2.desc_estado
      FROM   uni_status
      WHERE  estado = arr_x2.estado

      CASE arr_x2.tipo
         WHEN "UNIFICADOR"
            LET arr_2x[iii].total_fam    = arr_x2.total

         WHEN "UNIFICADO"

            IF arr_2x[iii].estado = 0 THEN
               LET iii = 1
            ELSE
               IF arr_2x[iii].estado <> arr_x2.estado THEN
                  LET iii = iii + 1
               END IF
            END IF

            LET arr_2x[iii].folio         = arr_x2.folio
            LET arr_2x[iii].folio_liquida = arr_x2.folio_liquida
            LET arr_2x[iii].estado        = arr_x2.estado
            LET arr_2x[iii].desc_estado   = arr_x2.desc_estado
            LET arr_2x[iii].fliquida      = arr_x2.fliquida
            LET arr_2x[iii].fnotifica     = arr_x2.fnotifica
            LET arr_2x[iii].total_nss     = arr_x2.total

            CONTINUE FOREACH
      END CASE

   END FOREACH

   IF iii >= 1 THEN
      CALL SET_COUNT(iii)
      DISPLAY ARRAY arr_2x TO scr_2.*
         ON KEY ( INTERRUPT )
            DISPLAY "                                 " AT 4,4
            DISPLAY "                                                                               " AT 5,1
            DISPLAY "                                                                               " AT 6,1 
            DISPLAY "                                                                               " AT 7,1 
            EXIT DISPLAY
         ON KEY (CONTROL-N)
            LET iii = ARR_CURR()
            IF (arr_2x[iii].folio IS NULL  AND
                arr_2x[iii].estado IS NULL AND
                arr_2x[iii].folio_liquida IS NULL) THEN
            ELSE 
               CALL detalle_uni(arr_2x[iii].folio,
                                arr_2x[iii].estado,
                                arr_2x[iii].folio_liquida,"")
            END IF
         ON KEY (CONTROL-U)
            LET iii = ARR_CURR()
            IF (arr_2x[iii].folio IS NULL  AND
                arr_2x[iii].estado IS NULL AND
                arr_2x[iii].folio_liquida IS NULL) THEN
            ELSE 
               CALL detalle_unificador(arr_2x[iii].folio,
                                       arr_2x[iii].estado,
                                       arr_2x[iii].folio_liquida,"")
            END IF
      END DISPLAY
   ELSE
      INITIALIZE arr_2x[iii].* TO NULL
      CLEAR FORM
      ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
      RETURN
   END IF

END FUNCTION
################################################################################
FUNCTION detalle_uni(vfolio,
                     vestado,
                     vfoliol,
                     vnss)

   DEFINE l_reg ARRAY[2000] OF RECORD
          nss            CHAR(11),
          tipo_ent       CHAR(02),
          cve_ent        CHAR(3),
          nombre         CHAR(50),
          cve_receptora  CHAR(3),
          nss_cta1       CHAR(11),
          tipo_ent_cta1  CHAR(02),
          cve_ent_cta1   CHAR(3)
   END RECORD

   DEFINE vnss           CHAR(21)
   DEFINE vestado        INTEGER
   DEFINE vfolio         INTEGER
   DEFINE vfoliol        INTEGER
   DEFINE xx             INTEGER
   DEFINE vpaterno       CHAR(40)
   DEFINE vmaterno       CHAR(40)
   DEFINE vnombre        CHAR(40)
   DEFINE vnombre_imss   CHAR(40)
   DEFINE i              SMALLINT
   DEFINE opc            CHAR(1)
   DEFINE det_txt        CHAR(500),
          det_txt1       CHAR(150)

   OPEN WINDOW unim0016 AT 10,2 WITH FORM "UNIM0016" ATTRIBUTE(BORDER)
   DISPLAY "UNIFICADOR  TIPO/CVE        NOMBRE                 RECEP  UNIFICADO  TIPO/CVE " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "[Ctrl-U] Detalle unificador                        [Ctrl-N] Detalle unificado " AT 12,1 ATTRIBUTE(REVERSE)
  
   IF vnss IS NULL THEN 
      LET det_txt1 = " WHERE  folio   = ", vfolio CLIPPED, 
                     " AND    estado  = ", vestado CLIPPED,
                     " ORDER BY 6,7,1"
   ELSE
      SELECT "X"
      FROM   uni_unificado
      WHERE  nss_uni = vnss
      GROUP BY 1

      IF STATUS = NOTFOUND THEN
         LET det_txt1 = " WHERE  folio   = ",vfolio CLIPPED,
                        " AND    estado  = ",vestado CLIPPED,
                        " AND    nss_cta1 = ",vnss,
                        " ORDER BY 6,7,1"
      ELSE
         LET det_txt1 = " WHERE  folio   = ",vfolio CLIPPED,
                      --  " AND    estado  = ",vestado CLIPPED,
                        " AND    nss_uni = ",vnss,
                        " ORDER BY 6,7,1"
      END IF
   END IF

   LET det_txt = " SELECT nss_uni,",
                        " '',",  --tipo_ent_nss,
                        " '',",  --cve_ent_nss,
                        " ' ',",
                        " '',",  --cve_afo_recep,
                        " nss_cta1,",
                        " tipo_ent_cta1,",
                        " cve_ent_cta1",
                 " FROM   uni_unificado", det_txt1 CLIPPED

   PREPARE det_1 FROM det_txt
   DECLARE detalle CURSOR FOR det_1

   LET i = 1
   FOREACH detalle INTO l_reg[i].*
      LET xx = 0

      IF vestado = 100 THEN
         SELECT "X"
         FROM   uni_unificador
         WHERE  nss_uni = l_reg[i].nss
         AND    estado  = vestado
         AND    folio   = vfolio
         AND    folio_liquida = vfoliol
         GROUP BY 1

         IF STATUS <> NOTFOUND THEN
            LET xx = 1
         ELSE
            LET xx = 0
         END IF
      ELSE
         LET xx = 1
      END IF

       IF xx = 1 THEN
          SELECT paterno_uni,
                 materno_uni,
                 nombre_uni,
                 nombre_imss_uni,
                 tipo_ent_nss,
                 cve_ent_nss,
                 cve_afo_recep
          INTO   vpaterno,
                 vmaterno,
                 vnombre,
                 vnombre_imss,
                 l_reg[i].tipo_ent,
                 l_reg[i].cve_ent,
                 l_reg[i].cve_receptora
          FROM   uni_unificador
          WHERE  nss_uni = l_reg[i].nss
          AND    folio   = vfolio 

          LET l_reg[i].nombre = vpaterno CLIPPED," ",
                                vmaterno CLIPPED," ",
                                vnombre CLIPPED

          IF (l_reg[i].nombre IS NULL OR
              l_reg[i].nombre MATCHES " *" ) THEN
             LET l_reg[i].nombre = vnombre_imss
          END IF
          LET i = i + 1
       END IF
   END FOREACH

   ERROR ""
   CALL SET_COUNT(i-1)
   DISPLAY ARRAY l_reg TO scr_6.*
      ON KEY ( control-m )
         INITIALIZE l_reg TO NULL

         FOR i = 1 TO 8
             DISPLAY l_reg[i].* TO scr_6[i].*
         END FOR
         EXIT DISPLAY
      ON KEY ( control-u )
         LET i = ARR_CURR()

         CALL consulta(l_reg[i].nss,
                       vfolio)

      ON KEY ( control-n )
         LET i = ARR_CURR()

         CALL unificados(l_reg[i].nss_cta1,
                         vfolio)
   END DISPLAY

   CLOSE WINDOW unim0016
END FUNCTION #despliega_tipo
############################################################################
FUNCTION detalle_unificador(vfolio,
                            vestado,
                            vfoliol,
                            vnss)

   DEFINE vnss           CHAR(21)
   DEFINE vestado        INTEGER
   DEFINE vfolio         INTEGER
   DEFINE vfoliol        INTEGER
   DEFINE xx             INTEGER
   DEFINE vpaterno       CHAR(40)
   DEFINE vmaterno       CHAR(40)
   DEFINE vnombre        CHAR(40)
   DEFINE vnombre_imss   CHAR(40)
   DEFINE i              SMALLINT
   DEFINE opc            CHAR(1)
   DEFINE det_txt        CHAR(300),
          det_txt1       CHAR(150)

   DEFINE sql_stat     INTEGER,
          item_row_cnt SMALLINT,
          row_cnt      SMALLINT,
          cont_inp     SMALLINT,
          cur_row      SMALLINT,
          scr_row      SMALLINT

   OPEN WINDOW unim0016 AT 10,2 WITH FORM "UNIM0016" ATTRIBUTE(BORDER)
   DISPLAY "UNIFICADOR  TIPO/CVE        NOMBRE                 RECEP  UNIFICADO  TIPO/CVE " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "[Ctrl-U]Detalle unificador                          [Ctrl-N]Detalle unificado " AT 12,1 ATTRIBUTE(REVERSE)
   DISPLAY "[Ctrl-F]Cambia Pantalla " AT 12,28 ATTRIBUTE(REVERSE)
  
   IF vnss IS NULL THEN 
      LET det_txt1 = " WHERE  folio   = ", vfolio CLIPPED, 
                     " AND    estado  = ", vestado CLIPPED,
                     " ORDER BY 1"
   ELSE
      SELECT "X"
      FROM   uni_unificado
      WHERE  nss_uni = vnss
      GROUP BY 1

      IF STATUS = NOTFOUND THEN
         LET det_txt1 = " WHERE  folio   = ",vfolio CLIPPED,
                        " AND    estado  = ",vestado CLIPPED,
                        " AND    nss_cta1 = ",vnss,
                        " ORDER BY 1"
      ELSE
         LET det_txt1 = " WHERE  folio   = ",vfolio CLIPPED,
                        " AND    estado  = ",vestado CLIPPED,
                        " AND    nss_uni = ",vnss,
                        " ORDER BY 1"
      END IF
   END IF

   LET det_txt = " SELECT '',nss_uni,",
                        " '',",  --tipo_ent_nss,
                        " '',",  --cve_ent_nss,
                        " '',",  --nombre
                        " '' ",  --cve_afo_recep,
                 " FROM   uni_unificador", det_txt1 CLIPPED

   PREPARE det_1_a FROM det_txt
   DECLARE detalle_uni CURSOR FOR det_1_a

   LET i = 1
   FOREACH detalle_uni INTO l_reg_1[i].*
      LET xx = 0

      IF vestado = 100 THEN
         SELECT "X"
         FROM   uni_unificador
         WHERE  nss_uni = l_reg_1[i].nss
         AND    estado  = vestado
         AND    folio   = vfolio
         AND    folio_liquida = vfoliol
         GROUP BY 1

         IF STATUS <> NOTFOUND THEN
            LET xx = 1
         ELSE
            LET xx = 0
         END IF
      ELSE
         LET xx = 1
      END IF

       IF xx = 1 THEN
          SELECT paterno_uni,
                 materno_uni,
                 nombre_uni,
                 nombre_imss_uni,
                 tipo_ent_nss,
                 cve_ent_nss,
                 cve_afo_recep
          INTO   vpaterno,
                 vmaterno,
                 vnombre,
                 vnombre_imss,
                 l_reg_1[i].tipo_ent,
                 l_reg_1[i].cve_ent,
                 l_reg_1[i].cve_receptora
          FROM   uni_unificador
          WHERE  nss_uni = l_reg_1[i].nss
          AND    folio   = vfolio

          LET l_reg_1[i].nombre = vpaterno CLIPPED," ",
                                  vmaterno CLIPPED," ",
                                  vnombre CLIPPED

          IF (l_reg_1[i].nombre IS NULL OR
              l_reg_1[i].nombre MATCHES " *" ) THEN
             LET l_reg_1[i].nombre = vnombre_imss
          END IF
          LET i = i + 1
       END IF
   END FOREACH

   ERROR ""
   LET i = i - 1

   IF i >= 1 THEN
      CALL SET_COUNT(i)
      LET cont_inp = TRUE
    
      WHILE (cont_inp = TRUE)
         INPUT ARRAY l_reg_1 WITHOUT DEFAULTS FROM scr_6a.*
         ATTRIBUTES(MAXCOUNT=i,COUNT=i)

            AFTER ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()

               DISPLAY l_reg_1[cur_row].* TO scr_6a[scr_row].*

            BEFORE ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()

               IF (cur_row = i + 1) THEN
                  LET cont_inp = TRUE
               ELSE
                  LET scr_row = SCR_LINE()

                  DISPLAY l_reg_1[cur_row].* TO scr_6a[scr_row].*
                  ATTRIBUTE (REVERSE)

                  LET cont_inp = FALSE

                  CALL proc_items(cur_row)
                       RETURNING sql_stat,
                                 item_row_cnt
            END IF

            ON KEY(CONTROL-F)
               CALL disp_array_items(item_row_cnt,vfolio)

            ON KEY(CONTROL-U)
               LET i = ARR_CURR()
               CALL consulta(l_reg_1[i].nss,
                             vfolio)

             ON KEY (INTERRUPT)
                EXIT INPUT

         END INPUT
      END WHILE
   ELSE
      ERROR "NO SE ENCONTRARON REGISTROS PARA MOSTRAR ..."
   END IF

   CLOSE WINDOW unim0016
END FUNCTION
############################################################################
FUNCTION consulta(x_nss_uni,
                  x_folio
                  )

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
   DISPLAY " [Ctrl-C]  Salir                            [Ctrl-V] Para ver unificados" AT 2,1 

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
              " AND    a.nss_uni = ","'",x_nss_uni,"'"

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

      ON KEY ( CONTROL-V )
         LET i = ARR_CURR()
         CALL despliega_unificados(arr_1[i].folio,
                                   arr_1[i].nss_uni,
                                   arr_1[i].estado)

      ON KEY ( INTERRUPT )
         EXIT DISPLAY
   END DISPLAY
   CLOSE WINDOW unim0012
END FUNCTION
###############################################################################
FUNCTION despliega_unificados(vfolio,nss_u,vestado)

   DEFINE l_reg ARRAY[10] OF RECORD
          nss_cta1       CHAR(11),
          folio          INTEGER,
          folio_liquida  INTEGER,
          tipo_ent_cta1  CHAR(2),
          cve_ent_cta1   CHAR(3),
          nombre         CHAR(50),
          diag_unifica   CHAR(02),
          estado    	  SMALLINT
   END RECORD

   DEFINE vestado   INTEGER
   DEFINE vfolio    INTEGER
   DEFINE nss_u     CHAR(11)
   DEFINE vpaterno  CHAR(40)
   DEFINE vmaterno  CHAR(40)
   DEFINE vnombre   CHAR(40)
   DEFINE vnombre_imss   CHAR(40)
   DEFINE i   	          SMALLINT

   OPEN WINDOW unim0013 AT 9,2 WITH FORM "UNIM0013" ATTRIBUTE(BORDER)
   DISPLAY "      NSS          FOLIO       AFORE           NOMBRE                DIAG/EDO " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "  UNIFICADOS  NOTIFICA LIQUIDA                COMPLETO                         " AT 2,1 ATTRIBUTE(REVERSE)

   DECLARE cursor_1 CURSOR FOR
   SELECT nss_cta1,
          folio,
          folio_liquida,
          tipo_ent_cta1,
          cve_ent_cta1,
          " ",
          diag_unifica,
          estado
   FROM   uni_unificado
   WHERE  nss_uni = nss_u
   AND    folio   = vfolio 

   LET i = 1
   FOREACH cursor_1 INTO l_reg[i].*
      SELECT paterno_cta1,
              materno_cta1,
              nombre_cta1,
              nombre_imss_cta1
       INTO   vpaterno,
              vmaterno,
              vnombre,
               vnombre_imss
       FROM   uni_unificado
       WHERE  nss_cta1 = l_reg[i].nss_cta1
       AND    nss_uni = nss_u
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

   DISPLAY ARRAY l_reg TO scr_3.*
      ON KEY ( control-m )
         INITIALIZE l_reg TO NULL
         FOR i = 1 TO 3
             DISPLAY l_reg[i].* TO scr_3[i].*
         END FOR
         EXIT DISPLAY
   END DISPLAY
   CLOSE WINDOW unim0013
END FUNCTION #despliega_tipo
###############################################################################
FUNCTION unificados(x_nss_cta1,
                    x_folio
                    )

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
   DISPLAY " [Ctrl-C]  Salir                            [Ctrl-V] Para ver unificadores" AT 2,1 

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
              " AND    a.nss_cta1 = ","'",x_nss_cta1,"'"

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
      ON KEY ( CONTROL-V )
         LET i = ARR_CURR()
         CALL despliega_unificadores(arr_1[i].folio,
                                     arr_1[i].nss_cta1,
                                     arr_1[i].estado)

      ON KEY ( INTERRUPT )
         EXIT DISPLAY
   END DISPLAY
   CLOSE WINDOW unim0012
END FUNCTION
###############################################################################
FUNCTION despliega_unificadores(vfolio,
                                nss_u,
                                vestado)

   DEFINE l_reg ARRAY[10] OF RECORD
          nss_uni        CHAR(11),
          folio          INTEGER,
          folio_liquida  INTEGER,
          tipo_ent_nss   CHAR(2),
          cve_ent_nss    CHAR(3),
          nombre         CHAR(50),
          cve_afo_recep  CHAR(3),
          estado         SMALLINT
   END RECORD

   DEFINE vestado        INTEGER
   DEFINE vfolio         INTEGER
   DEFINE nss_u          CHAR(11)
   DEFINE nss_un         CHAR(11)
   DEFINE vpaterno       CHAR(40)
   DEFINE vmaterno       CHAR(40)
   DEFINE vnombre        CHAR(40)
   DEFINE vnombre_imss   CHAR(40)
   DEFINE i              SMALLINT

   OPEN WINDOW unim0013 AT 9,2 WITH FORM "UNIM0015" ATTRIBUTE(BORDER)
   DISPLAY "      NSS          FOLIO     TIPO/CVE          NOMBRE               RECEP/EDO " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "  UNIFICADOR  NOTIFICA LIQUIDA                COMPLETO                         " AT 2,1 ATTRIBUTE(REVERSE)

   SELECT UNIQUE nss_uni
   INTO   nss_un
   FROM   uni_unificado 
   WHERE  nss_cta1 = nss_u
   AND    estado   = vestado
   AND    folio    = vfolio 

   DECLARE cursor_2 CURSOR FOR
   SELECT nss_uni,
          folio,
          folio_liquida,
          tipo_ent_nss,
          cve_ent_nss,
          " ",
          cve_afo_recep,
          estado
   FROM   uni_unificador
   WHERE  nss_uni  = nss_un
   AND    folio   = vfolio 

   LET i = 1
   FOREACH cursor_2 INTO l_reg[i].*
      SELECT paterno_uni,
             materno_uni,
             nombre_uni,
             nombre_imss_uni
      INTO   vpaterno,
             vmaterno,
             vnombre,
             vnombre_imss
      FROM   uni_unificador
      WHERE  nss_uni = l_reg[i].nss_uni
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

   DISPLAY ARRAY l_reg TO scr_4.*
      ON KEY(CONTROL-M)
         INITIALIZE l_reg TO NULL
         FOR i = 1 TO 3
             DISPLAY l_reg[i].* TO scr_4[i].*
         END FOR
         EXIT DISPLAY
   END DISPLAY

   CLOSE WINDOW unim0013
END FUNCTION #despliega_tipo
################################################################################
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
##################################################################
FUNCTION sel_ord_item(p_cur_row)

   DEFINE sql_status  INTEGER,
          p_cur_row   SMALLINT,
          row_cnt     SMALLINT,
          sql_txt     CHAR(250)

   CALL null_items()

   LET sql_txt =  " SELECT nss_cta1,",
                          "tipo_ent_cta1,",
                          "cve_ent_cta1,",
                          "''",              --trap
                  " FROM   uni_unificado ",
                  " WHERE  nss_uni = ? ",
                  " GROUP BY 1,2,3,4 ",
                  " ORDER BY 1 "

   PREPARE sel_item_stmt FROM sql_txt
   DECLARE sel_item_curs CURSOR FOR sel_item_stmt

   WHENEVER ERROR CONTINUE
      OPEN sel_item_curs USING l_reg_1[p_cur_row].nss
   WHENEVER ERROR STOP

   LET sql_status = SQLCA.SQLCODE
   LET row_cnt = 1

   WHILE ((NOT sql_status) AND (row_cnt<= 10))
      WHENEVER ERROR CONTINUE
         FETCH sel_item_curs INTO l_reg_2[row_cnt].*
      WHENEVER ERROR STOP
         LET sql_status = SQLCA.SQLCODE
         IF (NOT sql_status) THEN
            LET row_cnt = row_cnt + 1
         END IF
   END WHILE

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
      DISPLAY l_reg_2[i].* TO scr_6b[i].*
   END FOR

END FUNCTION
#####################################################
FUNCTION null_items()

   DEFINE i SMALLINT

   INITIALIZE l_reg_2[1].* TO NULL

   FOR i = 2 TO 100
      LET l_reg_2[i].* = l_reg_2[1].*
   END FOR
END FUNCTION
###############################################################
FUNCTION disp_array_items(p_item_row_cnt,x_folio)

   DEFINE p_item_row_cnt SMALLINT

   DEFINE x_folio   INTEGER

   DEFINE sql_status INTEGER,
          item_row_cnt SMALLINT,
          row_cnt SMALLINT,
          cont_inp SMALLINT,
          cur_row SMALLINT,
          scr_row SMALLINT,
          sw      SMALLINT

   LET sw = 0
   CALL SET_COUNT(p_item_row_cnt)
   LET cont_inp = TRUE

   WHILE (cont_inp = TRUE)
      INPUT ARRAY l_reg_2 WITHOUT DEFAULTS FROM scr_6b.*
      ATTRIBUTES(MAXCOUNT=p_item_row_cnt,COUNT=p_item_row_cnt)

         AFTER ROW
            LET cur_row = ARR_CURR()
            LET scr_row = SCR_LINE()

            DISPLAY l_reg_2[cur_row].* TO scr_6b[scr_row].*

         BEFORE ROW
            LET cur_row = ARR_CURR()
            LET scr_row = SCR_LINE()

            IF (cur_row = p_item_row_cnt + 1) THEN
                LET cont_inp = TRUE
                EXIT INPUT
            ELSE
               LET scr_row = SCR_LINE()

               DISPLAY l_reg_2[cur_row].* TO scr_6b[scr_row].*
               ATTRIBUTE(REVERSE)

               LET cont_inp = FALSE
            END IF

         ON KEY(CONTROL-N)
            LET cur_row = ARR_CURR()
            CALL unificados(l_reg_2[cur_row].nss_cta1,
                            x_folio)

         ON KEY (INTERRUPT)
            EXIT INPUT
      END INPUT
   END WHILE
END FUNCTION
###########################################################################
{
FUNCTION disp_array_items(p_item_row_cnt)

   DEFINE p_item_row_cnt SMALLINT

   CALL SET_COUNT(p_item_row_cnt)

   DISPLAY ARRAY l_reg_2 TO scr_6b.*
      ON KEY(CONTROL-N)
         LET cur_row = ARR_CURR()
         CALL unificados(l_reg_2[cur_row].nss_cta1,
                         x_folio)
      ON KEY(INTERRUPT)
         EXIT DISPLAY
   END DISPLAY
END FUNCTION
}
#############################################################

