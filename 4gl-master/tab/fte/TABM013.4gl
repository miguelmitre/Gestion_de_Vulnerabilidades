######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Owner             => E.F.P.                                         #
#Programa TABM0013 => CATALOGO ARCHIVO DE RECHAZOS.                  #
#Fecha             => 6 feb 1998.                                    #
#Fecha modifica    => 27 de noviembre de 2001.                       #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ                #
#Sistema           => TAB.                                           #
######################################################################
DATABASE safre_af

GLOBALS
    DEFINE g_reg  RECORD
           modulo_cod          CHAR(4),
           rdeta_cod           CHAR(15),
           rdeta_desc_c        CHAR(60),
           rdeta_desc_l        CHAR(120),
           tipo_rechazo        CHAR(1)
    END RECORD

    DEFINE g_param_dis  RECORD LIKE glo_parametro.*

    DEFINE l_record    ARRAY[3000] OF RECORD 
           modulo_cod          CHAR(4),
           rdeta_cod           CHAR(15),
           rdeta_desc_c        CHAR(60),
           rdeta_desc_l        CHAR(120),
           tipo_rechazo        CHAR(1)
    END RECORD

    DEFINE sw_1                SMALLINT,
           aux_pausa           CHAR(01),
           HOY                 DATE,
           aux_estad_desc      CHAR(40),
           seg_usuario         CHAR(08),
           pos                 SMALLINT,
           cla_where           CHAR(300),
           sel_where           CHAR(300),
           g_lista             CHAR(300),
           g_impre             CHAR(300)

END GLOBALS
##########################################################################
MAIN
    OPTIONS 
       PROMPT LINE LAST,
       INPUT WRAP,
       ACCEPT KEY control-o

    DEFER INTERRUPT

    CALL inicio()

    CALL proceso()
END MAIN
##########################################################################
FUNCTION inicio()
    SELECT USER,*
    INTO   seg_usuario
    FROM   glo_parametro

    SELECT ruta_spool
    INTO   g_param_dis.ruta_spool
    FROM   glo_parametro
END FUNCTION
##########################################################################
FUNCTION proceso()

    LET HOY = TODAY

    OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0131" ATTRIBUTE( BORDER)
    DISPLAY " TABM013           CATALOGO DE RECHAZOS DE AFILIADOS                               " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    MENU "CATALOGO RECHAZOS"
       COMMAND "Agrega" "Agrega Rechazos"
          CALL Agrega()
       COMMAND "Consulta" "Consulta Rechazos"
          CALL Consulta()
       COMMAND "Modifica" "Modifica Rechazos"
          CALL Modifica()
       COMMAND "Elimina" "Elimina Rechazos"
          CALL Elimina()
       COMMAND "Salir" "Salir del Programa"
          EXIT MENU
    END MENU

    CLOSE WINDOW ventana_1
END FUNCTION
#############################################################################
FUNCTION Inicializa()
   LET sw_1 = 0
   INITIALIZE g_reg.* TO NULL
   DISPLAY BY NAME g_reg.*
END FUNCTION
################################################################################
FUNCTION Agrega()
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( Esc ) Agrega                  (Ctrl-c) Salir " AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY " AGREGA " AT 1,70 ATTRIBUTE(REVERSE,BOLD)

   INPUT BY NAME  g_reg.*
      AFTER  FIELD modulo_cod
         IF g_reg.modulo_cod IS NULL THEN
            ERROR "El campo no puede ser nulo"
            NEXT FIELD modulo_cod
         ELSE
            SELECT "X"
            FROM seg_modulo 
            WHERE modulo_cod = g_reg.modulo_cod

            IF SQLCA.SQLCODE <> 0 THEN
               ERROR "Modulo no existe, verificar .."
               NEXT FIELD modulo_cod
            END IF
         END IF

      AFTER  FIELD rdeta_cod
         IF g_reg.rdeta_cod IS NULL THEN
            ERROR "El campo no puede ser nulo"
            NEXT FIELD rdeta_cod
         ELSE
            SELECT "X"
            FROM   tab_rdeta 
            WHERE  modulo_cod = g_reg.modulo_cod
	    AND    rdeta_cod = g_reg.rdeta_cod

            IF STATUS <> NOTFOUND THEN
               ERROR "Ya existe el rechazo"
               NEXT FIELD rdeta_cod
            END IF
         END IF

      AFTER  FIELD rdeta_desc_c
         IF g_reg.rdeta_desc_c IS NULL THEN
            ERROR "El campo no puede ser nulo"
            NEXT FIELD rdeta_desc_c 
         END IF

      AFTER  FIELD rdeta_desc_l
         IF g_reg.rdeta_desc_l IS NULL THEN
            ERROR "El campo no puede ser nulo"
            NEXT FIELD rdeta_desc_l
         END IF

      AFTER FIELD tipo_rechazo
         IF g_reg.tipo_rechazo IS NULL THEN
            ERROR "El campo no puede ser nulo"
            NEXT FIELD tipo_rechazo
         END IF

      ON KEY ( ESC )
         IF g_reg.modulo_cod IS NULL THEN
            ERROR "El campo no puede ser nulo"
            NEXT FIELD modulo_cod
         END IF

         IF g_reg.rdeta_cod IS NULL THEN
            ERROR "Codigo de Rechazo NO puede ser NULO"
            NEXT FIELD rdeta_cod
         END IF

         IF g_reg.rdeta_desc_c IS NULL THEN
            ERROR "Descripcion de Rechazo NO puede ser NULO"
            NEXT FIELD rdeta_desc_c
         END IF

         IF g_reg.rdeta_desc_l IS NULL THEN
            ERROR "Descripcion de Rechazo NO puede ser NULO"
            NEXT FIELD rdeta_desc_l
         END IF

         IF g_reg.tipo_rechazo IS NULL THEN
            ERROR "El campo no puede ser nulo"
            NEXT FIELD tipo_rechazo
         END IF

         INSERT INTO tab_rdeta
         VALUES ( g_reg.* )

         ERROR "REGISTRO INGRESADO"
         SLEEP 1
         ERROR ""

         CALL Inicializa()

         NEXT FIELD modulo_cod

      ON KEY (INTERRUPT)
         CALL Inicializa()
         EXIT INPUT
   END INPUT

   CALL Inicializa()

   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Consulta()
   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0132" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) Consulta              (Ctrl-p) Impresion             (Ctrl-C) Salir   " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "             Escoja con < ENTER > el rechazo para ver completo                 " AT 2,1 
      DISPLAY "                             RECHAZOS DE AFILIADOS                                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

      LET int_flag = FALSE

      CONSTRUCT cla_where ON modulo_cod,
                             rdeta_cod
                        FROM modulo_cod,
                             rdeta_cod
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT * FROM tab_rdeta WHERE ",
                       cla_where CLIPPED,
                      " ORDER BY 2 "

      PREPARE query FROM sel_where

      DECLARE cursor_1 CURSOR FOR query

      LET pos = 1
      FOREACH cursor_1 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)

         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.modulo_cod     = l_record[pos].modulo_cod
               LET g_reg.rdeta_cod      = l_record[pos].rdeta_cod
               LET g_reg.rdeta_desc_c   = l_record[pos].rdeta_desc_c
               LET g_reg.rdeta_desc_l   = l_record[pos].rdeta_desc_l
               LET g_reg.tipo_rechazo   = l_record[pos].tipo_rechazo
               EXIT DISPLAY
            ON KEY (control-p)
               ERROR "PROCESANDO IMPRESION..."
               CALL impresion(pos)
            ON KEY (INTERRUPT)
               EXIT DISPLAY
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE RECHAZOS DE afi_solicitud.... VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " CONSULTA" AT 1,67 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY BY NAME  g_reg.*
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION  Modifica()
   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0132" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) Consulta                                             (Ctrl-c) Salir   " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "             Escoja con < ENTER > el rechazo de afiliado a modificar           " AT 2,1 
      DISPLAY "                              RECHAZOS DE AFILIADOS                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON modulo_cod,
                             rdeta_cod
                        FROM modulo_cod,
                             rdeta_cod
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT * FROM tab_rdeta WHERE ",
                      cla_where CLIPPED,
                      " ORDER BY 2 "

      PREPARE query1 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query1

      LET pos = 1
      FOREACH cursor_2 INTO l_record[pos].*
         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)

         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.modulo_cod     = l_record[pos].modulo_cod
               LET g_reg.rdeta_cod      = l_record[pos].rdeta_cod
               LET g_reg.rdeta_desc_c   = l_record[pos].rdeta_desc_c
               LET g_reg.rdeta_desc_l   = l_record[pos].rdeta_desc_l
               LET g_reg.tipo_rechazo   = l_record[pos].tipo_rechazo
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE RECHAZOS DE AFILADOS.. VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " MODIFICA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

      INPUT BY NAME  g_reg.* WITHOUT DEFAULTS
         BEFORE FIELD modulo_cod
            NEXT FIELD rdeta_desc_c

         AFTER FIELD rdeta_desc_c
            IF g_reg.rdeta_desc_c IS NULL THEN
               ERROR "Descripcion de Rechazo de Afiliado corta NO puede ser nula"
               NEXT FIELD  rdeta_desc_c
            END IF 

         AFTER FIELD rdeta_desc_l
            IF g_reg.rdeta_desc_l IS NULL THEN
               ERROR "Descripcion de Rechazo de Afiliado larga NO puede ser nula"
               NEXT FIELD  rdeta_desc_l
            END IF 

    --- Nuevo campo a modificar mmc
         AFTER FIELD tipo_rechazo
            IF g_reg.tipo_rechazo IS NULL THEN
               ERROR "Tipo rechazo no puede ser nulo"
               NEXT FIELD  tipo_rechazo
            END IF 

            IF g_reg.tipo_rechazo <> 'C' AND
               g_reg.tipo_rechazo <> 'D' THEN
               ERROR "Tipo rechazo solo puede ser <C> corregible o <D> definitivo"
	       LET g_reg.tipo_rechazo = NULL
               NEXT FIELD  tipo_rechazo
            END IF 

            CALL Pregunta()

            IF aux_pausa MATCHES "[Ss]" THEN
               UPDATE tab_rdeta
               SET    rdeta_desc_c = g_reg.rdeta_desc_c,
                      rdeta_desc_l = g_reg.rdeta_desc_l,
                      tipo_rechazo = g_reg.tipo_rechazo
               WHERE  modulo_cod = g_reg.modulo_cod
               AND    rdeta_cod = g_reg.rdeta_cod

               ERROR "REGISTRO MODIFICADO" 
               SLEEP 2

               CALL Inicializa()
            ELSE
               ERROR "PROCESO DE MODIFICACION,CANCELADO"
               SLEEP 2
            END IF

            ERROR ""
            EXIT INPUT
         ON KEY ( INTERRUPT )
            CALL Inicializa()
            EXIT INPUT
      END INPUT
   ELSE
      ERROR "ARCHIVO DE RECHAZOS DE afi_solicitud.... VACIO"
   END IF

   CALL Inicializa()
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()
   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0132" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) Consulta                                             (Ctrl-c) Salir   " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "               Escoja con < ENTER > el rechazo de afiliado a eliminar          " AT 2,1 
      DISPLAY "                              RECHAZOS DE AFILIADOS                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON modulo_cod,
                             rdeta_cod
                        FROM modulo_cod,
                             rdeta_cod
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT * FROM tab_rdeta WHERE ",
                      cla_where CLIPPED,
                      " ORDER BY 2 "

      PREPARE query2 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query2

      LET pos = 1
      FOREACH cursor_3 INTO l_record[pos].*
         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1) 

         DISPLAY ARRAY l_record TO scr_1.* 
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.modulo_cod    = l_record[pos].modulo_cod
               LET g_reg.rdeta_cod     = l_record[pos].rdeta_cod
               LET g_reg.rdeta_desc_c  = l_record[pos].rdeta_desc_c
               LET g_reg.rdeta_desc_l  = l_record[pos].rdeta_desc_l
               LET g_reg.tipo_rechazo  = l_record[pos].tipo_rechazo
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE RECHAZOS DE afi_solicitud.... VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " ELIMINA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY BY NAME  g_reg.*

      CALL Pregunta()

      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE FROM tab_rdeta
         WHERE modulo_cod = g_reg.modulo_cod
         AND   rdeta_cod = g_reg.rdeta_cod

         ERROR "REGISTRO ELIMINADO" 
         SLEEP 2
      ELSE
         ERROR "ELIMINAR CANCELADO" 
         SLEEP 2
      END IF

      ERROR ""

      CALL Inicializa()
   ELSE
      ERROR "ARCHIVO DE RECHAZOS DE afi_solicitud.... VACIO"
   END IF

   CALL Inicializa()

   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Pregunta()
   PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION impresion(pos)
   DEFINE i,pos SMALLINT

   LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",seg_usuario CLIPPED,
                 ".IMPTABRDETA",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabrdeta TO g_impre

   FOR i=1 TO (pos+1)
      LET g_reg.modulo_cod    = l_record[i].modulo_cod
      LET g_reg.rdeta_cod     = l_record[i].rdeta_cod
      LET g_reg.rdeta_desc_c  = l_record[i].rdeta_desc_c
      LET g_reg.rdeta_desc_l  = l_record[i].rdeta_desc_l
      LET g_reg.tipo_rechazo  = l_record[i].tipo_rechazo

      IF g_reg.rdeta_cod IS NULL THEN
         EXIT FOR
      END IF

      OUTPUT TO REPORT rpt_tabrdeta(g_reg.*)
   END FOR

   FINISH REPORT rpt_tabrdeta

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabrdeta(g_reg)

   DEFINE g_reg  RECORD 
          modulo_cod          CHAR(3),
          rdeta_cod           CHAR(15),
          rdeta_desc_c        CHAR(60),
          rdeta_desc_l        CHAR(120),
          tipo_rechazo        CHAR(1)
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d' 
         PRINT COLUMN 01,"TABM013 ",
               COLUMN 65,"LISTADO DE CATALOGO DE RECHAZOS DE AFILIADOS     ",
               COLUMN 165,TODAY USING "dd-mm-yyyy"
         SKIP 3 LINE

         PRINT COLUMN 01,"MODULO",
               COLUMN 08,"CODIGO",
               COLUMN 20,"DESCRIPCION CORTA",
               COLUMN 110,"DESCRIPCION LARGA",
               COLUMN 174,"TIPO RECHAZO"
         SKIP 1 LINE
      ON EVERY ROW
         IF g_reg.rdeta_desc_l[101,120] IS NULL THEN
            PRINT COLUMN 1,g_reg.modulo_cod CLIPPED, 
                  --COLUMN 8,g_reg.rdeta_cod USING "#####",
                  COLUMN 8,g_reg.rdeta_cod USING "###",
                  COLUMN 15,g_reg.rdeta_desc_c,
                  COLUMN 73,g_reg.rdeta_desc_l[1,100],
                  COLUMN 177,g_reg.tipo_rechazo
         ELSE
            PRINT COLUMN 1,g_reg.modulo_cod CLIPPED, 
                  --COLUMN 8,g_reg.rdeta_cod USING "#####",
                  COLUMN 8,g_reg.rdeta_cod USING "###",
                  COLUMN 15,g_reg.rdeta_desc_c,
                  COLUMN 73,g_reg.rdeta_desc_l[1,100],
                  COLUMN 177,g_reg.tipo_rechazo
            PRINT COLUMN 73,g_reg.rdeta_desc_l[101,120]
         END IF
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
