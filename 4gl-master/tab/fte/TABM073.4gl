######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Owner             => E.F.P.                                         #
#Programa TABM0073 => CATALOGO DEVOLUCIONES.                         #
#Fecha             => 26 de marzo del 2003.                          #
#Fecha modifica    => 26 de marzo del 2003.                          #
#Modificado por    => LAURA EUGENIA CORTES GUZMAN                    #
#Sistema           => TAB.                                           #
#Modificado por    => FERNANDO HERRERA HERNANDEZ                     #
#Fecha modifica    => 01 de diciembre del 2008.                      #
#                     Agregar Tipo de Diagnosticos                   #
######################################################################
DATABASE safre_af

GLOBALS
    DEFINE g_reg  RECORD
           cod_rech           CHAR(3),
           desc_rech          CHAR(150),
           tipo               SMALLINT,
           desc_tipo          CHAR(40),
           tipo_diag          SMALLINT,
           desc_diag          CHAR(40)
    END RECORD

    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE l_record    ARRAY[3000] OF RECORD 
           cod_rech           CHAR(3),
           desc_rech          CHAR(150),
           tipo               SMALLINT,
           tipo_diag          SMALLINT
    END RECORD

    DEFINE sw_1                SMALLINT,
           aux_pausa           CHAR(01),
           HOY                 DATE,
           fecha               DATE,
           aux_estad_desc      CHAR(40),
           g_usuario           CHAR(08),
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

    SELECT *, USER   
    INTO   g_seg_modulo.*, g_usuario    
    FROM   seg_modulo   
    WHERE  modulo_cod = "tab"

    INITIALIZE g_reg.*, fecha TO NULL
END FUNCTION
##########################################################################
FUNCTION proceso()

    LET HOY = TODAY

    OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0731" ATTRIBUTE( BORDER)
    DISPLAY " TABM073             CATALOGO DE DIAG VERIF IMAG                               " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    MENU "CATALOGO DIAGNOSTICOS VERIF IMG"
       COMMAND "Agrega" "Agrega Diagnosticos"
          CALL Agrega()
       COMMAND "Consulta" "Consulta Diagnosticos"
          CALL Consulta()
       COMMAND "Modifica" "Modifica Diagnosticos"
          CALL Modifica()
       COMMAND "Elimina" "Elimina Diagnosticos"
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
   DISPLAY " ( Esc ) Agrega                  (Ctrl-c) Salir " 
   AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY " AGREGA " AT 1,70 ATTRIBUTE(REVERSE,BOLD)

   INPUT BY NAME  g_reg.*

      AFTER  FIELD cod_rech
         IF g_reg.cod_rech IS NULL THEN
            ERROR "El campo no puede ser nulo"
            NEXT FIELD cod_rech
         END IF

      AFTER FIELD tipo
         IF g_reg.tipo IS NULL OR
            g_reg.tipo = " "   THEN
            ERROR "El campo no puede ser nulo o blanco"
            NEXT FIELD tipo
         END IF

         IF g_reg.tipo <> 1 AND
            g_reg.tipo <> 2 AND
	    g_reg.tipo <> 3 THEN
            ERROR "Tipo Devolucion solo puede ser <1> Operativo o ",
                  "<2> Validacion o <3> Validacion a reenviar"
            LET g_reg.tipo = NULL
            NEXT FIELD tipo
         END IF 

         CASE g_reg.tipo     
           WHEN 1
             LET g_reg.desc_tipo = 'DEVOLUCION OPERATIVO'
           WHEN 2
             LET g_reg.desc_tipo = 'VALIDACION'
           WHEN 3
             LET g_reg.desc_tipo = 'VALIDACION A REENVIAR'
           OTHERWISE
             LET g_reg.desc_tipo = ''
         END CASE
         DISPLAY BY NAME g_reg.desc_tipo

      AFTER FIELD desc_rech
         IF g_reg.desc_rech IS NULL    OR
            g_reg.desc_rech[1,1] = " " THEN
            ERROR "El campo no puede ser nulo o blanco"
            NEXT FIELD desc_rech
         END IF

      AFTER FIELD tipo_diag
        IF g_reg.tipo_diag <> 1 AND
           g_reg.tipo_diag <> 2 AND
           g_reg.tipo_diag <> 3 THEN
           ERROR "Tipo Diagnostico solo puede ser <1> Result Llamada",
                 "<2> Val Img o <3> Cancelación Folio"
           LET g_reg.tipo_diag = NULL
           NEXT FIELD tipo_diag
        ELSE
           SELECT "a.X"
           FROM   tab_dev_taa a
	   WHERE  a.cod_rech  = g_reg.cod_rech
           AND    a.tipo_diag = g_reg.tipo_diag
           IF STATUS <> NOTFOUND THEN
              ERROR "Ya existe el Codigo de diagnostico"
              NEXT FIELD cod_rech
           ELSE
             IF g_reg.tipo_diag = 2 THEN
                LET g_reg.cod_rech = g_reg.cod_rech USING "&&"
             ELSE
                LET g_reg.cod_rech = g_reg.cod_rech USING "&&&"
             END IF

             CASE g_reg.tipo_diag
               WHEN 1
                 LET g_reg.desc_diag = 'RESULT LLAMADA VERIFICACION TRASPASO'
               WHEN 2
                 LET g_reg.desc_diag = 'VALIDACION DE IMAGENES'
               WHEN 3
                 LET g_reg.desc_diag = 'CANCELACION DE FOLIO'
               OTHERWISE
                 LET g_reg.desc_diag = ''
             END CASE
             DISPLAY BY NAME g_reg.desc_diag
           END IF
        END IF

      ON KEY ( ESC )
         IF g_reg.cod_rech IS NULL THEN
            ERROR "El campo no puede ser nulo"
            NEXT FIELD cod_rech
         ELSE
            SELECT "a.X"
            FROM   tab_dev_taa a
	    WHERE  a.cod_rech  = g_reg.cod_rech
            AND    a.tipo_diag = g_reg.tipo_diag
            IF STATUS <> NOTFOUND THEN
               ERROR "Ya existe el Codigo de diagnostico"
               NEXT FIELD cod_rech
            END IF
         END IF

         IF g_reg.tipo IS NULL OR
            g_reg.tipo = " "   THEN
            ERROR "El campo no puede ser nulo o blanco"
            NEXT FIELD tipo
         END IF

         IF g_reg.tipo_diag IS NULL OR
            g_reg.tipo_diag = " "   THEN
            ERROR "El campo no puede ser nulo o blanco"
            NEXT FIELD tipo_diag
         END IF

         IF g_reg.desc_rech IS NULL    OR
            g_reg.desc_rech[1,1] = " " THEN
            ERROR "El campo no puede ser nulo"
            NEXT FIELD desc_rech
         END IF

         LET fecha = TODAY
         INSERT INTO tab_dev_taa
         VALUES ( g_reg.cod_rech, 
                  g_reg.desc_rech, 
                  g_usuario,
                  fecha, 
                  g_reg.tipo,
                  g_reg.tipo_diag )

         ERROR "REGISTRO INGRESADO"
         SLEEP 1
         ERROR ""

         CALL Inicializa()

         NEXT FIELD cod_rech

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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0732" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) Consulta              (Ctrl-p) Impresion             (Ctrl-C) Salir   " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "          Seleccione con < ENTER > el Diagnostico para ver completo            " AT 2,1 
      DISPLAY "                             D I A G N O S T I C O S                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

      LET int_flag = FALSE

      CONSTRUCT cla_where ON cod_rech,
                             tipo,
                             tipo_diag
                        FROM cod_rech,
                             tipo,
                             tipo_diag
         ON KEY (CONTROL-M)
            LET int_flag = FALSE
            EXIT CONSTRUCT

         ON KEY (CONTROL-C)
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

      LET sel_where = "SELECT cod_rech, desc_rech, tipo, tipo_diag ",
		      "  FROM tab_dev_taa WHERE ",
                       cla_where CLIPPED,
                      " ORDER BY 4,1 "

      PREPARE query FROM sel_where
      DECLARE cursor_1 CURSOR FOR query

      LET pos = 1
      FOREACH cursor_1 INTO l_record[pos].*

         IF l_record[pos].tipo_diag = 2 THEN
            LET l_record[pos].cod_rech = l_record[pos].cod_rech USING "&&"
         ELSE
            LET l_record[pos].cod_rech = l_record[pos].cod_rech USING "&&&"
         END IF

         LET pos = pos + 1

      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)

         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.cod_rech       = l_record[pos].cod_rech
               LET g_reg.desc_rech      = l_record[pos].desc_rech
               LET g_reg.tipo           = l_record[pos].tipo     
               LET g_reg.tipo_diag      = l_record[pos].tipo_diag

               IF l_record[pos].tipo_diag = 2 THEN
                  LET g_reg.cod_rech = g_reg.cod_rech USING "&&"
               ELSE
                  LET g_reg.cod_rech = g_reg.cod_rech USING "&&&"
               END IF
               EXIT DISPLAY

            ON KEY (control-p)
               ERROR "PROCESANDO IMPRESION..."
               CALL impresion(pos)

            ON KEY (INTERRUPT)
               EXIT DISPLAY
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE DIAGNOSTICOS.... VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " CONSULTA" AT 1,67 ATTRIBUTE(REVERSE,BOLD)

      CASE g_reg.tipo     
        WHEN 1
          LET g_reg.desc_tipo = 'DEVOLUCION OPERATIVO'
        WHEN 2
          LET g_reg.desc_tipo = 'VALIDACION'
        WHEN 3
          LET g_reg.desc_tipo = 'VALIDACION A REENVIAR'
        OTHERWISE
          LET g_reg.desc_tipo = ''
      END CASE
      DISPLAY BY NAME g_reg.desc_tipo

      CASE g_reg.tipo_diag
        WHEN 1
          LET g_reg.desc_diag = 'RESULT LLAMADA VERIFICACION TRASPASO'
        WHEN 2
          LET g_reg.desc_diag = 'VALIDACION DE IMAGENES'
        WHEN 3
          LET g_reg.desc_diag = 'CANCELACION DE FOLIO'
        OTHERWISE
          LET g_reg.desc_diag = ''
      END CASE

      DISPLAY BY NAME  g_reg.*
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Modifica()
   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0732" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) Consulta                                             (Ctrl-c) Salir   " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "               Seleccione con < ENTER > el Diagnostico a modificar             " AT 2,1 
      DISPLAY "                              D I A G N O S T I C O S                              " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON cod_rech, 
                             tipo,
                             tipo_diag
                        FROM cod_rech,
                             tipo,
                             tipo_diag
         ON KEY (CONTROL-M)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (CONTROL-C)
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

      LET sel_where = "SELECT cod_rech, desc_rech, tipo, tipo_diag ",
		      "  FROM tab_dev_taa WHERE ",
                      cla_where CLIPPED,
                      " ORDER BY 4,1"

      PREPARE query1 FROM sel_where
      DECLARE cursor_2 CURSOR FOR query1

      LET pos = 1
      FOREACH cursor_2 INTO l_record[pos].*
         IF l_record[pos].tipo_diag = 2 THEN
            LET l_record[pos].cod_rech = l_record[pos].cod_rech USING "&&"
         ELSE
            LET l_record[pos].cod_rech = l_record[pos].cod_rech USING "&&&"
         END IF

         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)

         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.cod_rech      = l_record[pos].cod_rech
               LET g_reg.desc_rech     = l_record[pos].desc_rech
               LET g_reg.tipo          = l_record[pos].tipo     
               LET g_reg.tipo_diag     = l_record[pos].tipo_diag

               IF l_record[pos].tipo_diag = 2 THEN
                  LET g_reg.cod_rech = g_reg.cod_rech USING "&&"
               ELSE
                  LET g_reg.cod_rech = g_reg.cod_rech USING "&&&"
               END IF

               EXIT DISPLAY

            ON KEY (INTERRUPT)
               ERROR "Deberá seleccionar un registro a modificar"
               LET pos = ARR_CURR()
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE DIAGNOSTICO DE IMAGENES.. VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " MODIFICA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

      CASE g_reg.tipo     
        WHEN 1
          LET g_reg.desc_tipo = 'DEVOLUCION OPERATIVO'
        WHEN 2
          LET g_reg.desc_tipo = 'VALIDACION'
        WHEN 3
          LET g_reg.desc_tipo = 'VALIDACION A REENVIAR'
        OTHERWISE
          LET g_reg.desc_tipo = ''
      END CASE
      DISPLAY BY NAME g_reg.desc_tipo

      CASE g_reg.tipo_diag
        WHEN 1
          LET g_reg.desc_diag = 'RESULT LLAMADA VERIFICACION TRASPASO'
        WHEN 2
          LET g_reg.desc_diag = 'VALIDACION DE IMAGENES'
        WHEN 3
          LET g_reg.desc_diag = 'CANCELACION DE FOLIO'
        OTHERWISE
          LET g_reg.desc_diag = ''
      END CASE
      DISPLAY BY NAME g_reg.desc_diag

      INPUT BY NAME  g_reg.* WITHOUT DEFAULTS
         BEFORE FIELD cod_rech
            NEXT FIELD desc_rech

         AFTER FIELD desc_rech
            IF g_reg.desc_rech IS NULL    OR
               g_reg.desc_rech[1,1] = " " THEN
               ERROR "LA descripcion del Diagnostico NO puede ser nulo"
               NEXT FIELD  desc_rech
            END IF 

         AFTER FIELD tipo
            IF g_reg.tipo IS NULL OR
               g_reg.tipo  =  " " THEN
               ERROR "Tipo de devolucion No puede ser nulo o blanco"
               NEXT FIELD tipo
            END IF 

            IF g_reg.tipo <> 1 AND
               g_reg.tipo <> 2 AND 
	       g_reg.tipo <> 3 THEN
               ERROR "Tipo Devolucion solo puede ser <1> Operativo o ",
                     "<2> Validacion o <3> Validacion a reenviar "
	       LET g_reg.tipo = NULL
               NEXT FIELD tipo
            ELSE
              CASE g_reg.tipo     
                WHEN 1
                  LET g_reg.desc_tipo = 'DEVOLUCION OPERATIVO'
                WHEN 2
                  LET g_reg.desc_tipo = 'VALIDACION'
                WHEN 3
                  LET g_reg.desc_tipo = 'VALIDACION A REENVIAR'
                OTHERWISE
                  LET g_reg.desc_tipo = ''
              END CASE
              DISPLAY BY NAME g_reg.desc_tipo
            END IF 

         AFTER FIELD tipo_diag
           IF g_reg.tipo_diag <> 1 AND
              g_reg.tipo_diag <> 2 AND
              g_reg.tipo_diag <> 3 THEN
              ERROR "Tipo Diagnostico solo puede ser <1> Result Llamada",
                    "<2> Val Img o <3> Cancelación Folio"
              LET g_reg.tipo_diag = NULL
              NEXT FIELD tipo_diag
           ELSE
              {SELECT "a.X"
              FROM   tab_dev_taa a
	      WHERE  a.cod_rech  = g_reg.cod_rech
              AND    a.tipo      = g_reg.tipo
              AND    a.tipo_diag = g_reg.tipo_diag
              IF STATUS <> NOTFOUND THEN
                 ERROR "Ya existe el Codigo de diagnostico"
                 NEXT FIELD cod_rech
              ELSE}
                CASE g_reg.tipo_diag
                  WHEN 1
                    LET g_reg.desc_diag = 'RESULT LLAMADA VERIFICACION TRASPASO'
                  WHEN 2
                    LET g_reg.desc_diag = 'VALIDACION DE IMAGENES'
                  WHEN 3
                    LET g_reg.desc_diag = 'CANCELACION DE FOLIO'
                  OTHERWISE
                    LET g_reg.desc_diag = ''
                END CASE
                DISPLAY BY NAME g_reg.desc_diag
              #END IF
           END IF

           CALL Pregunta()

           IF aux_pausa MATCHES "[Ss]" THEN
              UPDATE tab_dev_taa
              SET    desc_rech = g_reg.desc_rech,
                     tipo      = g_reg.tipo
              WHERE  cod_rech  = g_reg.cod_rech
              AND    tipo_diag = g_reg.tipo_diag

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
      ERROR "ARCHIVO DE DIAGNOSTICOS SE ENCUENTRA.... VACIO"
   END IF

   CALL Inicializa()
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()
   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0732" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) Consulta                                             (Ctrl-c) Salir   " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                Seleccione con < ENTER > el Diagnostico a eliminar             " AT 2,1 
      DISPLAY "                              D E V O L U C I O N E S                          " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON cod_rech,
                             tipo,
                             tipo_diag
                        FROM cod_rech,
                             tipo,
                             tipo_diag
         ON KEY (CONTROL-M)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (CONTROL-C)
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

      LET sel_where = "SELECT cod_rech, desc_rech, tipo, tipo_diag ",
		      "  FROM tab_dev_taa WHERE ",
                      cla_where CLIPPED,
                      " ORDER BY 4,1 "

      PREPARE query2 FROM sel_where
      DECLARE cursor_3 CURSOR FOR query2

      LET pos = 1
      FOREACH cursor_3 INTO l_record[pos].*
         IF l_record[pos].tipo_diag = 2 THEN
            LET l_record[pos].cod_rech = l_record[pos].cod_rech USING "&&"
         ELSE
            LET l_record[pos].cod_rech = l_record[pos].cod_rech USING "&&&"
         END IF

         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1) 

         DISPLAY ARRAY l_record TO scr_1.* 
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.cod_rech     = l_record[pos].cod_rech
               LET g_reg.desc_rech    = l_record[pos].desc_rech
               LET g_reg.tipo         = l_record[pos].tipo
               LET g_reg.tipo_diag    = l_record[pos].tipo_diag

               IF l_record[pos].tipo_diag = 2 THEN
                  LET g_reg.cod_rech = g_reg.cod_rech USING "&&"
               ELSE
                  LET g_reg.cod_rech = g_reg.cod_rech USING "&&&"
               END IF

               EXIT DISPLAY

            ON KEY (INTERRUPT)
               ERROR "Debera seleccionar un registro"
               LET pos = ARR_CURR()
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE DIAGNOSTICOS DE IMAGENES.... VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " ELIMINA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

      CASE g_reg.tipo     
        WHEN 1
          LET g_reg.desc_tipo = 'DEVOLUCION OPERATIVO'
        WHEN 2
          LET g_reg.desc_tipo = 'VALIDACION'
        WHEN 3
          LET g_reg.desc_tipo = 'VALIDACION A REENVIAR'
        OTHERWISE
          LET g_reg.desc_tipo = ''
      END CASE

      CASE g_reg.tipo_diag
        WHEN 1
          LET g_reg.desc_diag = 'RESULT LLAMADA VERIFICACION TRASPASO'
        WHEN 2
          LET g_reg.desc_diag = 'VALIDACION DE IMAGENES'
        WHEN 3
          LET g_reg.desc_diag = 'CANCELACION DE FOLIO'
        OTHERWISE
          LET g_reg.desc_diag = ''
      END CASE

      DISPLAY BY NAME g_reg.*

      CALL Pregunta()

      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE FROM tab_dev_taa
         WHERE cod_rech  = g_reg.cod_rech
         AND   tipo      = g_reg.tipo
         AND   tipo_diag = g_reg.tipo_diag

         ERROR "REGISTRO ELIMINADO" 
         SLEEP 2
      ELSE
         ERROR "ELIMINAR CANCELADO" 
         SLEEP 2
      END IF

      ERROR ""

      CALL Inicializa()
   ELSE
      ERROR "ARCHIVO DE DIAGNOSTICOS SE ENCUENTRA.... VACIO"
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

   LET g_impre = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                 ".IMPTABDEVOL",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabdevol TO g_impre

   FOR i=1 TO (pos+1)
      LET g_reg.cod_rech     = l_record[i].cod_rech
      LET g_reg.desc_rech    = l_record[i].desc_rech
      LET g_reg.tipo         = l_record[i].tipo     
      LET g_reg.tipo_diag    = l_record[i].tipo_diag

      IF g_reg.cod_rech IS NULL THEN
         EXIT FOR
      END IF

      OUTPUT TO REPORT rpt_tabdevol(g_reg.*)
   END FOR

   FINISH REPORT rpt_tabdevol

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   #LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   #RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabdevol(g_reg)

   DEFINE g_reg  RECORD 
          cod_rech           CHAR(3),
          desc_rech          CHAR(150),
          tipo               SMALLINT,
          desc_tipo          CHAR(40),
          tipo_diag          SMALLINT,
          desc_diag          CHAR(40)
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
         PRINT COLUMN 01,"TABM073 ",
               COLUMN 70,"LISTADO DE CATALOGO DE DIAG IMAGENES    ",
               COLUMN 165,TODAY USING "dd-mm-yyyy"
         SKIP 3 LINE

         PRINT COLUMN 01,"CODIGO",
               COLUMN 10,"DESCRIPCION LARGA",
               COLUMN 134,"TIPO DIAG"
         SKIP 1 LINE

      ON EVERY ROW

            IF g_reg.tipo_diag = 2 THEN
               LET g_reg.cod_rech = g_reg.cod_rech USING "&&"
            ELSE
               LET g_reg.cod_rech = g_reg.cod_rech USING "&&&"
            END IF

            PRINT COLUMN 1,g_reg.cod_rech,
                  COLUMN 10,g_reg.desc_rech CLIPPED,
                  COLUMN 135,g_reg.tipo_diag
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
