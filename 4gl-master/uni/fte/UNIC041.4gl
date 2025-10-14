###############################################################################
#Proyecto          => AFORE (MEXICO)                                          #
#Propietario       => E.F.P  					              #
#Programa UNIC041  => CONSULTA DE NSS ACEPTADOS Y/O RECHAZADOS                #
#Sistema           => UNI  					              #
#Autor             => Omar Sandoval Badillo                                   #
#Fecha             => 11 de marzo de 2005                                     #
###############################################################################
DATABASE safre_af
GLOBALS
   DEFINE enter                 CHAR(1),
          opc                   CHAR(1),
          h                     SMALLINT,
          h1                     SMALLINT,
          g_usuario             CHAR(8),
          g_impre               CHAR(200),
          g_lista               CHAR(200),
          g_ruta_listados       CHAR(50),
          HOY                   DATE,
          g_afore               RECORD LIKE tab_afore_local.*

   DEFINE cla_where             CHAR(200),
          sel_where             CHAR(200)

   DEFINE vcodigo_afore     CHAR(03),
          vrazon_social     CHAR(50)

   DEFINE i              SMALLINT,
          xfolio         INTEGER,
          folio          INTEGER,
          xfecha_proceso DATE

   DEFINE gr_marcas    RECORD
          nss             CHAR(11),
          nombre          CHAR(40),
          fecha_ini       DATE,
          fecha_fin       DATE,
          rechazo_cod     SMALLINT,
          marca_causa     SMALLINT
   END RECORD

   DEFINE l_record ARRAY[8000] OF RECORD
          nss             CHAR(11),
          nombres         CHAR(40),
          fecha_ini       DATE,
          fecha_fin       DATE,
          rechazo_cod     SMALLINT,
          marca_causa     SMALLINT
   END RECORD

   DEFINE reg1  ARRAY[8000] OF RECORD 
          cuenta_reg       SMALLINT,   
          estado_marca     SMALLINT,
          descripcion      CHAR(15)
   END RECORD
END GLOBALS
#################################################
MAIN
   OPTIONS PROMPT LINE LAST,
   ACCEPT KEY CONTROL-I,
   INPUT WRAP,
   COMMENT LINE LAST
   DEFER INTERRUPT

   CALL STARTLOG("UNIC041.log")
   CALL inicio()   
   CALL proceso_principal()
END MAIN
#################################################
FUNCTION proceso_principal()

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIC0411" ATTRIBUTE(BORDER)
   DISPLAY " UNIC041         CONSULTA DE CUENTAS MARCADAS Y RECHAZADAS                     " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

   MENU "CONSULTA "
       COMMAND "Consulta" "Consulta de cuentas marcadas y/o rechazadas"
          CALL consulta()
       COMMAND "Salir" "Salir de Programa"
          EXIT MENU
   END MENU

END FUNCTION
#################################################
FUNCTION inicio()
   LET HOY = TODAY

   SELECT *, USER 
   INTO   g_afore.*, g_usuario 
   FROM   tab_afore_local

   SELECT ruta_listados
   INTO   g_ruta_listados
   FROM   seg_modulo
   WHERE  modulo_cod = "uni"

   SELECT codigo_afore,
          razon_social
   INTO   vcodigo_afore,
          vrazon_social
   FROM   tab_afore_local


END FUNCTION
#################################################
FUNCTION consulta()

   DEFINE arr_c     SMALLINT,
          arr_l     SMALLINT,
          arr_t     SMALLINT,
          cont_inp   CHAR(1)

   LET int_flag = FALSE
   LET folio = NULL

   DISPLAY "                                                                               " AT 1,1 
   DISPLAY " < ESC > Consultar                                         <Ctrl-P> Imprimir   " AT 2,1 

   INPUT BY NAME folio WITHOUT DEFAULTS

      AFTER FIELD folio
         IF folio IS NULL THEN
            ERROR "EL FOLIO NO PUEDE SER NULO"
            NEXT FIELD folio
         END IF

         SELECT 'X'
         FROM   uni_cza_notifica a
         WHERE  a.folio = folio

         IF STATUS = NOTFOUND THEN
            ERROR "NO SE ENCONTRARON REGISTROS CON EL FOLIO INDICADO"
            NEXT FIELD folio
         END IF

      ON KEY(ESC)
         IF folio IS NULL THEN
            ERROR "EL FOLIO NO PUEDE SER NULO"
            NEXT FIELD folio
         END IF

         SELECT 'X'
         FROM   uni_cza_notifica a
         WHERE  a.folio = folio

         IF STATUS = NOTFOUND THEN
            ERROR "NO SE ENCONTRARON REGISTROS CON EL FOLIO DADO"
            NEXT FIELD folio
         END IF

         LET int_flag = FALSE
         EXIT INPUT

      ON KEY(INTERRUPT)
         LET int_flag = TRUE
         EXIT INPUT
   END INPUT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      CLEAR FORM
      ERROR "PROCESO DE CONSULTA CANCELADO..."
      RETURN
   END IF

   ERROR "PROCESANDO INFORMACION..."

   SELECT a.folio,
          a.fecha_proceso
   INTO   xfolio,
          xfecha_proceso
   FROM   uni_cza_notifica a
   WHERE  a.folio = folio

   DECLARE cur1 CURSOR FOR 
   SELECT count(*),
          estado_marca
   FROM   cta_his_marca
   WHERE  fecha_ini = xfecha_proceso
   AND    marca_cod IN(241,242,243,244)
   GROUP BY 2
   UNION
   SELECT count(*),
          estado_marca
   FROM   cta_his_marca
   WHERE  fecha_fin = xfecha_proceso
   AND    marca_cod IN(241,242,243,244)
   GROUP BY 2
   ORDER BY 2

   LET i = 1
   FOREACH cur1 INTO reg1[i].cuenta_reg,
                     reg1[i].estado_marca

      CASE reg1[i].estado_marca
         WHEN 0  LET reg1[i].descripcion = "ACEPTADOS"
         WHEN 20 LET reg1[i].descripcion = "RECHAZADOS"
      END CASE

      LET i = i + 1
   END FOREACH

   LET i = i - 1

   IF i >= 1 THEN
      CALL SET_COUNT(i)

      DISPLAY "  Total  Estado  Descripcion  " AT 7,1 ATTRIBUTE(REVERSE)
      INPUT ARRAY reg1 WITHOUT DEFAULTS FROM scr_1.*
      ATTRIBUTES(MAXCOUNT = i,COUNT = i)

         AFTER ROW
            LET arr_c = ARR_CURR()
            LET arr_l = SCR_LINE()
            LET arr_t = ARR_COUNT()

            DISPLAY reg1[arr_c].* TO scr_1[arr_l].* 

            BEFORE ROW
               LET arr_c = ARR_CURR()
               LET arr_l = SCR_LINE()
               LET arr_t = ARR_COUNT()

               IF (arr_c = i+1) THEN
                  LET cont_inp = TRUE
               ELSE
                  LET arr_l = SCR_LINE()
                  DISPLAY reg1[arr_c].* TO scr_1[arr_l].* ATTRIBUTE(REVERSE)
                  LET cont_inp = TRUE
               END IF

         ON KEY (CONTROL-M)
            LET i = ARR_CURR()
            DISPLAY "    NSS                  NOMBRE              INICIO     FINAL  RECHAZO MARCA   " AT 11,1 ATTRIBUTE(REVERSE)
            CALL detalle_marca(reg1[i].estado_marca,xfecha_proceso)
         ON KEY (INTERRUPT)
            CLEAR FORM
            DISPLAY "                              " AT 7,1
            DISPLAY "                                                                               " AT 11,1
            ERROR "CONSULTA CANCELADA..."
            EXIT INPUT
      END INPUT
   ELSE
      ERROR "NO SE ENCONTRARON REGISTROS PARA MOSTRAR..."
      SLEEP 2
      ERROR ""
   END IF

END FUNCTION
#################################################
FUNCTION detalle_marca(reg1,xfecha_proceso)
   DEFINE reg1 RECORD
      estado_marca        SMALLINT
   END RECORD

   DEFINE xfecha_proceso  DATE,
          k               SMALLINT

   DEFINE afi_nombre   RECORD
          paterno         CHAR(40),
          materno         CHAR(40),
          nombres         CHAR(40)
   END RECORD

   DEFINE reg2  ARRAY[8000] OF RECORD
      nss           CHAR(11),
      fecha_ini     DATE,
      fecha_fin     DATE,
      rechazo_cod   SMALLINT,
      marca_causa   SMALLINT
   END RECORD

   DEFINE sel_where1   CHAR(600),
          sel_where2   CHAR(600)	

   IF reg1.estado_marca = 0 THEN
      LET sel_where1 = " WHERE  fecha_ini    = ","'",xfecha_proceso,"'",
                       " AND    estado_marca = ",reg1.estado_marca,
                       " AND    marca_cod in(241,242,243,244)",
                       " GROUP BY 1,2,3,4,5 ",
                       " ORDER BY 5 "
   ELSE
      LET sel_where1 = " WHERE  fecha_fin    = ","'",xfecha_proceso,"'",
                       " AND    estado_marca = ",reg1.estado_marca,
                       " AND    marca_cod in(241,242,243,244)",
                       " GROUP BY 1,2,3,4,5 ",
                       " ORDER BY 5 "   	
   END IF

   LET sel_where2 = " SELECT nss,",
                           "fecha_ini,",
                           "fecha_fin,",
                           "rechazo_cod,",
                           "marca_causa",
                   " FROM   cta_his_marca",sel_where1 CLIPPED

   PREPARE query_edo FROM sel_where2
   DECLARE cur2 CURSOR FOR query_edo

   LET k = 1
   FOREACH cur2 INTO reg2[k].*

      LET l_record[k].nss         = reg2[k].nss
      LET l_record[k].fecha_ini   = reg2[k].fecha_ini
      LET l_record[k].fecha_fin   = reg2[k].fecha_fin
      LET l_record[k].rechazo_cod = reg2[k].rechazo_cod
      LET l_record[k].marca_causa = reg2[k].marca_causa

      DECLARE cur3 CURSOR FOR
      SELECT  paterno,
              materno,
              nombres
      FROM    afi_mae_afiliado
      WHERE   n_seguro = reg2[k].nss

      OPEN cur3
      FETCH cur3 INTO afi_nombre.*
      CLOSE cur3
      
      LET l_record[k].nombres = afi_nombre.paterno CLIPPED," ",
                                afi_nombre.materno CLIPPED," ",
                                afi_nombre.nombres CLIPPED

      IF reg2[k].rechazo_cod = 0 THEN
         LET l_record[k].rechazo_cod = NULL
      END IF
      IF reg2[k].marca_causa = 0 THEN
         LET l_record[k].marca_causa = NULL
      END IF

      LET k = k + 1 
   END FOREACH

   LET k = k - 1

   IF k >= 1 THEN
      CALL SET_COUNT(k)
      DISPLAY ARRAY l_record TO scr_2.*
         ON KEY (CONTROL-P)
            CALL impresion(k)
         ON KEY (INTERRUPT)
            INITIALIZE l_record TO NULL
            FOR h = 1 TO 7
               DISPLAY l_record[h].* TO scr_2[h].*
            END FOR

            ERROR "CONSULTA CANCELADA..."
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "NO SE ENCONTRARON REGISTROS PARA MOSTRAR..."
      SLEEP 2
      ERROR ""
   END IF

END FUNCTION
#################################################
FUNCTION impresion(pos)

   DEFINE i, pos SMALLINT

   LET g_impre = g_ruta_listados CLIPPED,"/",
                 g_usuario CLIPPED,".IMP",
                 HOY USING "DD-MM-YYYY" CLIPPED

   START REPORT listados TO g_impre

   FOR i = 1 TO (pos+1)

       LET gr_marcas.nss         = l_record[i].nss
       LET gr_marcas.nombre      = l_record[i].nombres
       LET gr_marcas.fecha_ini   = l_record[i].fecha_ini
       LET gr_marcas.fecha_fin   = l_record[i].fecha_fin
       LET gr_marcas.rechazo_cod = l_record[i].rechazo_cod
       LET gr_marcas.marca_causa = l_record[i].marca_causa

       IF gr_marcas.nss IS NULL THEN
          EXIT FOR
       END IF

       OUTPUT TO REPORT listados(gr_marcas.*)
   END FOR

   FINISH REPORT listados

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   #LET g_lista = "vi ",g_impre
   RUN g_lista

END FUNCTION
#################################################
REPORT listados(gr_marcas)

   DEFINE gr_marcas    RECORD
          nss             CHAR(11),
          nombre          CHAR(40),
          fecha_ini       DATE,
          fecha_fin       DATE,
          rechazo_cod     SMALLINT,
          marca_causa     SMALLINT
   END RECORD

   DEFINE L1               CHAR(01),
          L2               CHAR(02),
          L3               CHAR(03),
          L4               CHAR(04),
          L5               CHAR(05),
          L6               CHAR(06),
          L7               CHAR(07),
          L8               CHAR(08),
          L9               CHAR(09),
          L10              CHAR(10)

   DEFINE tipo_marca       CHAR(30)

   OUTPUT
      TOP MARGIN    0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   80

   FORMAT
      PAGE HEADER

         LET  L1  = "\304"
         LET  L2  = "\304\304"
         LET  L3  = "\304\304\304"
         LET  L4  = "\304\304\304\304"
         LET  L5  = "\304\304\304\304\304"
         LET  L10 = "\304\304\304\304\304\304\304\304\304\304"

         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H','\033015'
         PRINT COLUMN 1,'\033e\033(s218T\033(s11H\033(s7B','\033015'

         PRINT COLUMN 003, vrazon_social,
               COLUMN 103, HOY USING "DD/MM/YYYY",'\033015'

         CASE reg1[i].estado_marca
            WHEN 0 LET tipo_marca = "ACEPTADOS"
            WHEN 20 LET tipo_marca = "RECHAZADOS"
         END CASE

         PRINT COLUMN 24," CONSULTA REGISTROS DE MARCAJE DE UNIFICACION DE CUENTAS ",tipo_marca CLIPPED
         PRINT COLUMN 002," UNIC041",
               COLUMN 103,"P\240gina  ",pageno USING "##",'\033015'
         SKIP 1 LINE

         PRINT COLUMN 003,"FOLIO : ",folio USING "#####",'\033015'
         SKIP 2 LINE

         PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s4B','\033015'
         PRINT COLUMN 2,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,'\033015'
         SKIP 1 LINE

         PRINT COLUMN 16," NSS ",
               COLUMN 36," NOMBRE",
               COLUMN 63," FECHA INICIO ",
               COLUMN 79," FECHA FIN ",
               COLUMN 88," RECHAZO ",
               COLUMN 95," MARCA "
         SKIP 1 LINE
         PRINT COLUMN 2,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,'\033015'
         SKIP 1 LINE

      ON EVERY ROW
         PRINT COLUMN 13,gr_marcas.nss,
               COLUMN 26,gr_marcas.nombre CLIPPED,
               COLUMN 65,gr_marcas.fecha_ini,
               COLUMN 80,gr_marcas.fecha_fin,
               COLUMN 88,gr_marcas.rechazo_cod,
               COLUMN 98,gr_marcas.marca_causa
         SKIP 2 LINE

      ON LAST ROW 
         SKIP 4 LINE
         PRINT COLUMN 03,"Total de registros : ",COUNT(*) USING "<<<"

END REPORT
#################################################
