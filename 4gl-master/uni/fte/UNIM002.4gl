###############################################################################
#Proyecto          => AFORE (MEXICO)                                          #
#Propietario       => E.F.P  					              #
#Programa UNIM002  => CONSULTA DE SOLICITUDES DE TRASPASO                     #
#Sistema           => UNI  					              #
#Autor             => Armando Rodriguez Castroparedes                         #
#Fecha             => 29 agosto 2002                                          #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ                         #
#Fecha             => 28 marzo 2005                                           #
#Modificado por    => OMAR SANDOVAL BADILLO                                   #
#Fecha             => 02 AGOSTO 2005                                          #
###############################################################################
DATABASE safre_af
GLOBALS

    DEFINE enter                 CHAR(1),
           opc                   CHAR(1),
           g_usuario             CHAR(8),
           aux_pausa             CHAR(01),
           HORA                  CHAR(08),
           HOY                   DATE,
           vfolio                INTEGER,
           rechazo               SMALLINT,
           g_afore               RECORD LIKE tab_afore_local.*
         
END GLOBALS
########################################################################
MAIN

   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      COMMENT LINE LAST

   DEFER INTERRUPT

   CALL STARTLOG("UNIM002.log")
   CALL inicio()
   CALL proceso_principal()

END MAIN
########################################################################
FUNCTION proceso_principal()

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIM0022" ATTRIBUTE(BORDER)
   DISPLAY " UNIM002    CONSULTA  DE  SOLICITUDES  DE  TRASPASO  UNIFICACION              " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

   MENU "CONSULTA "
      COMMAND "Folios" "Consulta Folios de solicitudes de traspaso "
         CALL revisa_folio()
         CLEAR FORM
      COMMAND "Por NSS" "Consulta de solicitudes por NSS"
         CALL revisa_nss()
         CLEAR FORM
      COMMAND "Salir" "Salir de Programa"
         EXIT MENU
   END MENU

END FUNCTION
########################################################################
FUNCTION inicio()
    LET rechazo = 0
    LET HOY = TODAY

    LET HORA = TIME

    SELECT *, USER 
    INTO   g_afore.*, g_usuario 
    FROM   tab_afore_local

END FUNCTION
########################################################################
FUNCTION revisa_nss()

   DEFINE arr_c,
          flag,
          i               INTEGER

   DEFINE x_busca        CHAR(800)
   DEFINE txt_2          CHAR(500)

   DEFINE nss        CHAR(11)
   DEFINE nss_x        CHAR(11)

   DEFINE arr_2  ARRAY[200] OF RECORD
          folio                INTEGER,
          cve_operacion        CHAR(2),
          desc_operacion       CHAR(15),
          total_nss            INTEGER,
          estado               SMALLINT,
          desc_estado          CHAR(20)
   END RECORD

   INITIALIZE x_busca TO NULL
   INITIALIZE txt_2 TO NULL

   FOR i = 1 TO 200
      INITIALIZE arr_2[i] TO NULL
   END FOR

   CLEAR FORM
   DISPLAY "NSS :" AT 4,8
   LET int_flag = FALSE

   CONSTRUCT x_busca ON nss
                   FROM nss

      AFTER FIELD nss
         LET nss_x = GET_FLDBUF(nss)

         IF nss_x IS NULL THEN
            ERROR "EL CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD nss
         END IF

      ON KEY(INTERRUPT)
         LET int_flag = TRUE
         DISPLAY "                               " AT 4,8
         EXIT CONSTRUCT

      ON KEY(ESC)
         LET int_flag = FALSE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
      RETURN
   END IF

   DISPLAY "     Utilice las flechas para navegar        [Enter] para ver detalle         " AT 5,1
   DISPLAY "    FOLIO           OPERACION         TOTAL           E S T A D O            " AT 6,1 ATTRIBUTE(REVERSE)
   DISPLAY "    NOTIFICA    TIPO DESCRIPCION     CUENTAS       TIPO DESCRIPCION          " AT 7,1 ATTRIBUTE(REVERSE)

   LET txt_2 = " SELECT folio, ",
                      "cve_operacion, ",
                      "'CERTIFICACION', ",
                      "COUNT(*) total, ",
                      "estado, ",
                      "'' ",
               " FROM  uni_det_certifica  ",
               " WHERE ",x_busca CLIPPED,
               " GROUP BY 1,2,3,5,6 ",
               " UNION ",
               " SELECT folio, ",
                      "cve_operacion, ",
                      "'TRASPASO', ",
                      "COUNT(*) total, ",
                      "estado, ",
                      "'' ",
               " FROM  uni_det_traspaso ",
               " WHERE ",x_busca CLIPPED,
               " GROUP BY 1,2,3,5,6 ",
               " UNION ",
               " SELECT folio, ",
                      "cve_operacion, ",
                      "'ASIGNACION', ",
                      "COUNT(*) total, ",
                      "estado, ",
                      "'' ",
               " FROM  uni_det_asignado ",
               " WHERE ",x_busca CLIPPED,
               " GROUP BY 1,2,3,5,6 "

   LET txt_2 = txt_2 CLIPPED

   PREPARE x_pre_1 FROM txt_2
   DECLARE x_cur_1 CURSOR FOR x_pre_1

   LET i = 1
   FOREACH x_cur_1 INTO arr_2[i].*
      CASE arr_2[i].estado
         WHEN 10
            LET arr_2[i].desc_estado = "SOLICITUD GENERADA"
         WHEN 25
            LET arr_2[i].desc_estado = "SOLICITUD ACEPTADA"
         WHEN 40
            LET arr_2[i].desc_estado = "APERTURADA"
         WHEN 100
            LET arr_2[i].desc_estado = "TRASPASADA"
      END CASE
      LET i = i + 1
   END FOREACH

   IF i = 1 THEN
      INITIALIZE arr_2[i].* TO NULL
      CLEAR FORM
      ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
      RETURN
   END IF

   CALL SET_COUNT(i-1)
   DISPLAY ARRAY arr_2 TO scr_2.*
      ON KEY(INTERRUPT)
         DISPLAY "                                           " AT 4,8
         DISPLAY "                                                                               " AT 5,1
         DISPLAY "                                                                               " AT 6,1
         DISPLAY "                                                                               " AT 7,1
         EXIT DISPLAY
      ON KEY(CONTROL-M)
         LET i = ARR_CURR()
         CALL detalle_traspaso(arr_2[i].folio,
                               arr_2[i].cve_operacion,
                               arr_2[i].estado,
                               nss_x 
                               )
   END DISPLAY
END FUNCTION
########################################################################
FUNCTION revisa_folio()

   DEFINE arr_c,
          flag,
          i               INTEGER

   DEFINE x_busca        CHAR(100)
   DEFINE txt_2          CHAR(500)

   DEFINE arr_2  ARRAY[200] OF RECORD
          folio                INTEGER,
          cve_operacion        CHAR(2),
          desc_operacion       CHAR(15),
          total_nss            INTEGER,
          estado               SMALLINT,
          desc_estado          CHAR(20)
   END RECORD

   INITIALIZE x_busca TO NULL
   INITIALIZE txt_2 TO NULL
   FOR i = 1 TO 200 
      INITIALIZE arr_2[i] TO NULL
   END FOR

   CLEAR FORM

   DISPLAY "     Utilice las flechas para navegar        [Enter] para ver detalle         " AT 5,1
   DISPLAY "    FOLIO           OPERACION         TOTAL           E S T A D O            " AT 6,1 ATTRIBUTE(REVERSE)
   DISPLAY "    NOTIFICA    TIPO DESCRIPCION     CUENTAS       TIPO DESCRIPCION          " AT 7,1 ATTRIBUTE(REVERSE)

   LET int_flag              = FALSE

   CONSTRUCT BY NAME x_busca ON folio,
                                cve_operacion,
                                estado

      ON KEY (INTERRUPT) 
         IF int_flag = TRUE THEN    
            LET int_flag=FALSE     
            EXIT CONSTRUCT 
         END IF 

      ON KEY (Esc)
         LET int_flag = FALSE
         EXIT CONSTRUCT

   END CONSTRUCT

   LET txt_2 = "SELECT folio, ",
                      "cve_operacion, ",
                      "'CERTIFICACION', ",
                      "COUNT(*) total, ",
                      "estado, ",
                      "'' ",
               "FROM  uni_det_certifica ",
               "WHERE ",x_busca CLIPPED,
               " GROUP BY 1,2,3,5,6 ",
               "UNION ",
               "SELECT folio, ",
                      "cve_operacion, ",
                      "'TRASPASO', ",
                      "COUNT(*) total, ",
                      "estado, ",
                      "'' ",
               "FROM  uni_det_traspaso ",
               "WHERE ",x_busca CLIPPED,
               " GROUP BY 1,2,3,5,6 ",
               "UNION ",
               "SELECT folio, ",
                      "cve_operacion, ",
                      "'ASIGNACION', ",
                      "COUNT(*) total, ",
                      "estado, ",
                      "'' ",
               "FROM  uni_det_asignado ",
               "WHERE ",x_busca CLIPPED,
               " GROUP BY 1,2,3,5,6 "

   LET txt_2 = txt_2 CLIPPED

   PREPARE pre_1 FROM txt_2
   DECLARE cur_1 CURSOR FOR pre_1

   LET i = 1
   FOREACH cur_1 INTO arr_2[i].*
      CASE arr_2[i].estado
         WHEN 10
            LET arr_2[i].desc_estado = "SOLICITUD GENERADA"
         WHEN 25
            LET arr_2[i].desc_estado = "SOLICITUD ACEPTADA"
         WHEN 40
            LET arr_2[i].desc_estado = "APERTURADA"
         WHEN 100
            LET arr_2[i].desc_estado = "TRASPASADA"
      END CASE
      LET i = i + 1
   END FOREACH

   IF i = 1 THEN
      INITIALIZE arr_2[i].* TO NULL
      CLEAR FORM
      ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
      #CLOSE WINDOW unim0012
      RETURN
   END IF

   CALL SET_COUNT(i-1)

   DISPLAY ARRAY arr_2 TO scr_2.*
      ON KEY ( CONTROL-C )
         DISPLAY "                                           " AT 4,4
         DISPLAY "                                                                               " AT 5,1
         DISPLAY "                                                                               " AT 6,1
         DISPLAY "                                                                               " AT 7,1
         EXIT DISPLAY

      ON KEY ( CONTROL-M )
         LET i = ARR_CURR()
         CALL detalle_traspaso(arr_2[i].folio,
                               arr_2[i].cve_operacion,
                               arr_2[i].estado," ")
   END DISPLAY

END FUNCTION
################################################################################
FUNCTION detalle_traspaso(vfolio,
                          vcve_operacion,
                          vestado,
                          vnss_x)

   DEFINE l_reg ARRAY[3000] OF RECORD
          nss              CHAR(11),
          nombre           CHAR(50),
          cve_afo_ced      CHAR(03),
          fecha_certifica  CHAR(10)
   END RECORD

   DEFINE vestado        INTEGER
   DEFINE vcve_operacion CHAR(02)
   DEFINE x_tabla        CHAR(20)
   DEFINE vnss_x         CHAR(11)
   DEFINE txt_sel        CHAR(1000)
   DEFINE vfolio         INTEGER
   DEFINE vfoliol        INTEGER
   DEFINE xx             INTEGER
   DEFINE vpaterno       CHAR(40)
   DEFINE vmaterno       CHAR(40)
   DEFINE vnombre        CHAR(40)
   DEFINE vnombre_imss   CHAR(40)
   DEFINE i              SMALLINT
   DEFINE opc            CHAR(1)

   OPEN WINDOW unim0016 AT 9,2 WITH FORM "UNIM0023"
   DISPLAY "     NSS               NOMBRE                         CVE CED.  F. CERTIFICA " AT 1,1 ATTRIBUTE(REVERSE)

   CASE vcve_operacion
      WHEN "30" LET x_tabla = "uni_det_certifica a"
      WHEN "31" LET x_tabla = "uni_det_traspaso a"
      WHEN "32" LET x_tabla = "uni_det_asignado a"
   END CASE

   IF vnss_x IS NULL OR vnss_x = " " THEN
      LET txt_sel = "SELECT a.cve_afo_ced,",
                           "a.fecha_certifica,",
                           "a.nss,",
                    " TRIM(a.nombre)||' '||TRIM(a.paterno)||' '||TRIM(a.materno) nombre ",
                   " FROM   ",x_tabla CLIPPED,
                   " WHERE  a.folio   = ",vfolio, 
                   " AND    a.estado  = ",vestado,
                   " AND    a.cve_operacion = ",vcve_operacion,
                   " ORDER BY 1,2,3,4 "
   ELSE
      LET txt_sel = "SELECT a.cve_afo_ced,",
                           "a.fecha_certifica,",
                           "a.nss,",
                    " TRIM(a.nombre)||' '||TRIM(a.paterno)||' '||TRIM(a.materno) nombre ",
                   " FROM   ",x_tabla CLIPPED,
                   " WHERE  a.folio   = ",vfolio,
                   " AND    a.nss     = ","'",vnss_x,"'",
                   " AND    a.estado  = ",vestado,
                   " AND    a.cve_operacion = ",vcve_operacion,
                   " ORDER BY 1,2,3,4 "
   END IF

   LET txt_sel = txt_sel CLIPPED

   PREPARE pre_2 FROM txt_sel
   DECLARE det_1 CURSOR FOR pre_2

   LET i = 1

   FOREACH det_1 INTO l_reg[i].cve_afo_ced,
                      l_reg[i].fecha_certifica,
                      l_reg[i].nss,
                      l_reg[i].nombre           
      LET i = i + 1
   END FOREACH

   CALL SET_COUNT(i-1)

   DISPLAY ARRAY l_reg TO scr_6.*
      ON KEY ( CONTROL-C )
         INITIALIZE l_reg TO NULL

         FOR i = 1 TO 8
             DISPLAY l_reg[i].* TO scr_6[i].*
         END FOR
         EXIT DISPLAY

      ON KEY ( CONTROL-M )
         LET i = ARR_CURR()

         CALL consulta_traspaso(vfolio,
                                l_reg[i].nss,
                                vcve_operacion,
                                vestado)

   END DISPLAY

   CLOSE WINDOW unim0016
END FUNCTION #despliega_tipo
########################################################################
FUNCTION consulta_traspaso(x_folio,
                           x_nss,
                           x_cve_operacion,
                           x_estado)

   DEFINE x_folio          INTEGER,
          x_nss            CHAR(11),
          x_estado         SMALLINT

   DEFINE arr_c,
          flag,
          i                     INTEGER

   DEFINE x_busca1       CHAR(100)
   DEFINE txt_1          CHAR(600)

   DEFINE arr_1  ARRAY[1000] OF RECORD
          folio                INTEGER,
          nss                  CHAR(11),
          curp                 CHAR(18),
          rfc                  CHAR(13),
          paterno              CHAR(40),
          materno              CHAR(40),
          nombre               CHAR(40),
          fnacimiento          DATE,
          sexo                 CHAR(1),
          desc_sexo            CHAR(10),
          entidad              CHAR(3),
          desc_entidad         CHAR(25),
          credito              CHAR(1),
          nacionalidad         CHAR(3),
          doc_probatorio       CHAR(1),
          cve_afo_ced          CHAR(3),
          desc_afore           CHAR(25),
          fecha_certifica      DATE,
          estado               SMALLINT,
          apertura             CHAR(18),
          desc_estado          CHAR(25)
   END RECORD

   DEFINE x_cve_operacion    CHAR(2),
          x_tabla            CHAR(18)

   OPEN WINDOW unim0012 AT 2,2 WITH FORM "UNIM0021" ATTRIBUTE(BORDER)
   DISPLAY " UNIM002        SOLICITUD DE TRASPASO DE CUENTAS ASIGNADAS                     " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   DISPLAY "CONSULTA SOLICITUD" AT 1,59 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-C]  Salir                                                        " AT 2,1 

   CASE x_cve_operacion
      WHEN "30" LET x_tabla = "uni_det_certifica"
      WHEN "31" LET x_tabla = "uni_det_traspaso"
      WHEN "32" LET x_tabla = "uni_det_asignado"
   END CASE

   LET txt_1 =" SELECT a.folio, ",
                      "a.nss, ",
                      "a.curp, ",
                      "a.rfc, ",
                      "a.paterno, ",
                      "a.materno, ",
                      "a.nombre, ",
                      "a.fecha_nac, ",
                      "a.sexo, ",
                      "' ', ",   #desc_sexo
                      "a.ent_nac, ",
                      "' ', ",   #desc_ent
                      "a.ind_cred_info, ",
                      "a.nacionalidad, ",
                      "a.cve_doc_proba, ",
                      "a.cve_afo_ced, ",
                      "' ', ",   #desc_afore
                      "a.fecha_certifica, ",
                      "a.estado, ",
                      "' ' ",   #desc_estado
              " FROM   ",x_tabla CLIPPED," a",
              " WHERE  a.folio = ",x_folio CLIPPED,
              " AND    a.nss   = ","'",x_nss,"'",
              " AND    a.estado = ",x_estado CLIPPED

   PREPARE pre_6 FROM txt_1
   DECLARE cur_6 CURSOR FOR pre_6

   LET i = 1
   FOREACH cur_6 INTO arr_1[i].*
      SELECT sexo_desc
      INTO   arr_1[i].desc_sexo
      FROM   tab_sexo
      WHERE  sexo_cod = arr_1[i].sexo

      SELECT a.afore_desc
      INTO   arr_1[i].desc_afore
      FROM   tab_afore a
      WHERE  a.afore_cod = arr_1[i].cve_afo_ced

      SELECT a.estad_desc
      INTO   arr_1[i].desc_entidad
      FROM   tab_estado a
      WHERE  a.estad_cod = arr_1[i].entidad

      CASE arr_1[i].estado
         WHEN 10
            LET arr_1[i].desc_estado = "SOLICITUD GENERADA"
         WHEN 25
            LET arr_1[i].desc_estado = "SOLICITUD ACEPTADA"
         WHEN 40
            LET arr_1[i].desc_estado = "APERTURADA"
         WHEN 100
            LET arr_1[i].desc_estado = "TRASPASADA"
      END CASE

      SELECT "X"
      FROM   afi_solicitud
      WHERE  n_seguro = arr_1[i].nss
      AND    tipo_solicitud in(3,4)

      IF STATUS <> NOTFOUND THEN
         LET arr_1[i].apertura = "APERTURADO"
      ELSE
         SELECT "X"
         FROM   afi_mae_afiliado
         WHERE  n_seguro = arr_1[i].nss
         AND    tipo_solicitud in(3,4)

         IF STATUS = NOTFOUND THEN
            LET arr_1[i].apertura = "SIN APERTURA"
         ELSE
            LET arr_1[i].apertura = "APERTURADO"
         END IF
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
########################################################################
