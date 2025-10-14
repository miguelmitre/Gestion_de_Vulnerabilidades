######################################################################
#Proyecto          => Sistema de AFORE.( MEXICO )                    #
#Propietario       => E.F.P.                                         #
#Programa          => COM
#Descripcion       => CATALOGO DE CONTROL PROCESO.                   #
#Fecha             => 3 mayo 2002.                                   #
#Por               => GERARDO ALFONSO VEGA PAREDES                   #
#Sistema           => COM                                            #
######################################################################
DATABASE safre_af

GLOBALS
   DEFINE g_param_dev  RECORD LIKE seg_modulo.*

   DEFINE g_reg  RECORD LIKE com_ctrl_proceso.* 

   DEFINE hoy           DATE,
          usuario       CHAR(08),
          cla_where     CHAR(300),
          sel_where     CHAR(300),
          g_lista       CHAR(300),
          g_impre       CHAR(300),
          hora          CHAR(08)

END GLOBALS
#####################################################################
MAIN
   OPTIONS PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT

   CALL inicio()
   CALL proceso()
END MAIN
#####################################################################
FUNCTION inicio()
   SELECT ruta_listados,
          USER
   INTO   g_param_dev.ruta_listados,
          usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "com"

   SELECT USER
   INTO   usuario
   FROM   com_parametro
END FUNCTION
#####################################################################
FUNCTION proceso()
   LET HOY = TODAY

   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "COMC0021" ATTRIBUTE( BORDER)
   DISPLAY " COMC002              CATALOGO DE CONTROL PROCESO                              " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "CATALOGO CONTROL PROCESO"
      COMMAND "Consulta" "Consulta Control proceso"
         CALL Consulta()
      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1
END FUNCTION
#####################################################################
FUNCTION Inicializa()
   DISPLAY BY NAME g_reg.*
END FUNCTION
#####################################################################
FUNCTION Consulta()

   OPEN WINDOW ventana_2 AT 3,3 WITH FORM "COMC0021" ATTRIBUTE(BORDER) 
   DISPLAY " (Enter) Consulta " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " COMC002              CATALOGO DE CONTROL PROCESO                              " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

   LET int_flag = FALSE

   CONSTRUCT cla_where ON fecha_proceso,
                          proceso_cod,
                          etapa_cod 
                     FROM fecha_proceso,
                          proceso_cod,
                          etapa_cod
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

   LET sel_where = "SELECT * FROM com_ctrl_proceso WHERE ",
                    cla_where CLIPPED,
                   " ORDER BY 1,2,3,4 "

   PREPARE query FROM sel_where

   DECLARE cursor_1 SCROLL CURSOR  WITH HOLD FOR query

   OPEN cursor_1

   FETCH cursor_1 INTO g_reg.*

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "Registro no encontrado."
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana_2
      RETURN
   ELSE
      CALL Inicializa()

      CALL Botones()

      RETURN
   END IF

   CLEAR SCREEN
END FUNCTION
#####################################################################
FUNCTION Botones()

   MENU "Ingreso a botones."
      COMMAND "Primero" "Primer registro de la tabla."
         CALL Primero()
      COMMAND "Siguiente" "Siguiente registro de la tabla."
         CALL Siguiente()
      COMMAND "Anterior" "Anterior registro de la tabla."
         CALL Anterior()
      COMMAND "Ultimo" "Ultimo registro de la tabla."
         CALL Ultimo()
      COMMAND "Impresion" "Impresion de los registros."
         CALL Impresion()
      COMMAND KEY("L") "saLir" "Salir de los botones."
         EXIT MENU
   END MENU

   CLEAR SCREEN
   CLOSE WINDOW ventana_2

END FUNCTION
#######################################################################
FUNCTION Primero()
   FETCH FIRST cursor_1 INTO g_reg.*

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "Registro no encontrado."
      RETURN
   ELSE
      CALL Inicializa()
      RETURN
   END IF
END FUNCTION
#####################################################################
FUNCTION Siguiente()
   FETCH NEXT cursor_1 INTO g_reg.*

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "Registro no encontrado."
      RETURN
   ELSE
      CALL Inicializa()
      RETURN
   END IF
END FUNCTION
#####################################################################
FUNCTION Anterior()
   FETCH PREVIOUS cursor_1 INTO g_reg.*

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "Registro no encontrado."
      RETURN
   ELSE
      CALL Inicializa()
      RETURN
   END IF
END FUNCTION
#####################################################################
FUNCTION Ultimo()
   FETCH LAST cursor_1 INTO g_reg.*

   IF SQLCA.SQLCODE = NOTFOUND THEN
      ERROR "Registro no encontrado."
      RETURN
   ELSE
      CALL Inicializa()
      RETURN
   END IF
END FUNCTION
#####################################################################
FUNCTION Impresion()
   DEFINE i INTEGER

   LET hora = TIME

   LET g_impre = g_param_dev.ruta_listados CLIPPED,"/",usuario CLIPPED,
                 ".IMPCONPROCESO",hoy USING "dd-mm-yyyy","_",hora CLIPPED

   START REPORT rpt_tabcity TO g_impre

      FOREACH cursor_1 INTO g_reg.*
         OUTPUT TO REPORT rpt_tabcity(g_reg.*)
      END FOREACH

      INITIALIZE g_reg.* TO NULL

   FINISH REPORT rpt_tabcity

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabcity(g_reg)

   DEFINE g_reg RECORD  LIKE com_ctrl_proceso.*

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN 0 
      RIGHT MARGIN 0
      PAGE LENGTH 60

   FORMAT

   PAGE HEADER

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
      PRINT COLUMN 02,"COMC002 ",
            COLUMN 70," LISTADO DE CATALOGOS DE CONTROL PROCESO ",
            COLUMN 165, TODAY USING "dd-mm-yyyy"
      SKIP 3 LINE 

      PRINT COLUMN 01,"F. Proceso",
            COLUMN 12,"Cod.Pro.",
            COLUMN 21,"Etapa",
            COLUMN 27,"Hora Ini.",
            COLUMN 37,"Hora fin",
            COLUMN 46,"Parametro 1",
            COLUMN 60,"Parametro 2",
            COLUMN 75,"Parametro 3",
            COLUMN 90,"Parametro 4",
            COLUMN 105,"Parametro 5",
            COLUMN 120,"Usuario",
            COLUMN 150,"Resultado"

      PRINT COLUMN 01,"______________________________________________________________________________________________________________________________________________________________________________"
      SKIP 1 LINE

   ON EVERY ROW
      PRINT COLUMN 01,g_reg.fecha_proceso USING "DD/MM/YYYY",
            COLUMN 14,g_reg.proceso_cod ,
            COLUMN 21,g_reg.etapa_cod,
            COLUMN 27,g_reg.hora_inicial,
            COLUMN 37,g_reg.hora_final,
            COLUMN 46,g_reg.parametro1,
            COLUMN 60,g_reg.parametro2,
            COLUMN 75,g_reg.parametro3,
            COLUMN 90,g_reg.parametro4,
            COLUMN 105,g_reg.parametro5,
            COLUMN 120,g_reg.usuario,
            COLUMN 130,g_reg.resultado

   PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

   ON LAST ROW
      SKIP 2 LINE
      PRINT COLUMN 02,"Total de registros : ",COUNT(*) USING "<<<<<"
END REPORT
#####################################################################
