######################################################################
#Proyecto          => Sistema de AFORE.( MEXICO )                    #
#Propietario       => E.F.P.                                         #
#Programa          => EXCC004                                        #
#Descripcion       => CATALOGO DE CONTROL PROCESO.                   #
#Por               => OMAR SANDOVAL BADILLO                          #
#Fecha             => 29 JULIO 2005                                  #
#Sistema           => EXC                                            #
######################################################################
DATABASE safre_af

GLOBALS
   DEFINE g_param_dev  RECORD LIKE seg_modulo.*

   DEFINE g_reg  ARRAY[10000] OF RECORD LIKE dis_ctrl_proceso.* 

   DEFINE hoy           DATE,
          usuario       CHAR(08),
          opc           CHAR(01),
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

   CALL STARTLOG("EXCC003.log")
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
   WHERE  modulo_cod = "exc"
END FUNCTION
#####################################################################
FUNCTION proceso()
   LET HOY = TODAY

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "EXCC0041" ATTRIBUTE(BORDER)
   DISPLAY " EXCC004              CONSULTA DE CONTROL PROCESO                              " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "CONTROL PROCESO"
      COMMAND "Consulta" "Consulta Control proceso"
         CALL Consulta()
      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1
END FUNCTION
#####################################################################
FUNCTION Consulta()

   DEFINE l_reg  ARRAY[10000] OF RECORD
          fecha_proceso        DATE,        
          proceso_cod          CHAR(10),    
          folio                INTEGER,     
          etapa_cod            DECIMAL(2,0),
          hora_inicial         CHAR(8),     
          hora_final           CHAR(8)     
   END RECORD

   DEFINE l_reg2  ARRAY[10000] OF RECORD
          resultado            CHAR(50),    
          parametro1           CHAR(15),    
          parametro2           CHAR(15),    
          parametro3           CHAR(15),    
          parametro4           CHAR(15),    
          parametro5           CHAR(15)    
   END RECORD

   DEFINE pos      SMALLINT,
          arr_c    SMALLINT,
          arr_l    SMALLINT,
          arr_t    SMALLINT,
          cont_inp SMALLINT,
          i        SMALLINT

   OPEN WINDOW ventana_2 AT 2,2 WITH FORM "EXCC0041" ATTRIBUTE(BORDER) 
   DISPLAY " (Enter) Consulta                                                              " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY " EXCC004              CONSULTA DE CONTROL PROCESO                              " AT 3,1 ##ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ##ATTRIBUTE(REVERSE)
   DISPLAY " FECHA PROCESO   PROCESO       FOLIO       ETAPA                               " AT 4,1 ATTRIBUTE(REVERSE)

   LET int_flag = FALSE

   CONSTRUCT cla_where ON fecha_proceso,
                          proceso_cod,
                          folio,
                          etapa_cod
                     FROM fecha_proceso,
                          proceso_cod,
                          folio,
                          etapa_cod

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

   LET sel_where = " SELECT *",
                   " FROM dis_ctrl_proceso ",
                   " WHERE ",cla_where CLIPPED,
                   " ORDER BY 1,3 "

   PREPARE query FROM sel_where
   DECLARE cursor_1 SCROLL CURSOR  WITH HOLD FOR query

   LET pos = 1
   FOREACH cursor_1 INTO g_reg[pos].*

      LET l_reg[pos].fecha_proceso   = g_reg[pos].fecha_proceso
      LET l_reg[pos].proceso_cod     = g_reg[pos].proceso_cod
      LET l_reg[pos].folio           = g_reg[pos].folio
      LET l_reg[pos].etapa_cod       = g_reg[pos].etapa_cod
      LET l_reg[pos].hora_inicial    = g_reg[pos].hora_inicial
      LET l_reg[pos].hora_final      = g_reg[pos].hora_final
      
      LET l_reg2[pos].resultado      = g_reg[pos].resultado
      LET l_reg2[pos].parametro1     = g_reg[pos].parametro1
      LET l_reg2[pos].parametro2     = g_reg[pos].parametro2
      LET l_reg2[pos].parametro3     = g_reg[pos].parametro3
      LET l_reg2[pos].parametro4     = g_reg[pos].parametro4

      LET pos = pos + 1
   END FOREACH

   LET pos = pos - 1

   IF pos >= 1 THEN
      CALL SET_COUNT(pos)

      DISPLAY " FECHA PROCESO   PROCESO       FOLIO       ETAPA    HORA INICIAL  HORA FINAL" AT 4,1 ATTRIBUTE(REVERSE)
   DISPLAY " (Enter) Consulta              [Ctrl-I] Impresion             [Ctrl-C] Salir   " AT 2,1 ATTRIBUTE(REVERSE)
      DISPLAY "               RESULTADO                                       PARAMETROS      " AT 14,1 ATTRIBUTE(REVERSE)
      DISPLAY "1.:" AT 16,57 
      DISPLAY "2.:" AT 17,57 
      DISPLAY "3.:" AT 18,57 
      DISPLAY "4.:" AT 19,57 
      DISPLAY "5.:" AT 20,57

      INPUT ARRAY l_reg WITHOUT DEFAULTS FROM scr_1.*
      ATTRIBUTES(MAXCOUNT = pos,COUNT = pos)

         AFTER ROW
            LET arr_c = ARR_CURR()
            LET arr_l = SCR_LINE()
            LET arr_t = ARR_COUNT()

            DISPLAY l_reg2[arr_c].* TO scr_2[1].*

            BEFORE ROW
               LET arr_c = ARR_CURR()
               LET arr_l = SCR_LINE()
               LET arr_t = ARR_COUNT()

               IF (arr_c = pos + 1) THEN
                  LET cont_inp = TRUE
               ELSE
                  LET arr_l = SCR_LINE()
                  DISPLAY l_reg2[arr_c].* TO scr_2[1].*
                  LET cont_inp = FALSE
               END IF

         ON KEY(CONTROL-I)
            CALL impresion(l_reg[pos].*,l_reg2[pos].*)
            #EXIT INPUT

         ON KEY(INTERRUPT)
            ERROR "CONSULTA CANCELADA ..."
            EXIT INPUT
      END INPUT
      CLEAR FORM
   ELSE
      ERROR "NO SE ENCONTRARON REGISTROS ..."
      SLEEP 2
      ERROR ""
   END IF

   CLOSE WINDOW ventana_2

END FUNCTION
#####################################################################
FUNCTION Impresion(l_reg,l_reg2)

   DEFINE l_reg  RECORD
          fecha_proceso        DATE,
          proceso_cod          CHAR(10),
          folio                INTEGER,
          etapa_cod            DECIMAL(2,0),
          hora_inicial         CHAR(8),
          hora_final           CHAR(8)
   END RECORD

   DEFINE l_reg2 RECORD
          resultado            CHAR(50),
          parametro1           CHAR(15),
          parametro2           CHAR(15),
          parametro3           CHAR(15),
          parametro4           CHAR(15),
          parametro5           CHAR(15)
   END RECORD

   DEFINE i INTEGER

   LET hora = TIME
   LET g_impre = g_param_dev.ruta_listados CLIPPED,"/",usuario CLIPPED,
                 ".IMPCONPROCESO",hoy USING "DD-MM-YYYY","_",hora CLIPPED

   START REPORT rpt_tabcity TO g_impre

      LET i = 1
      FOREACH cursor_1 INTO g_reg[i].*
         OUTPUT TO REPORT rpt_tabcity(g_reg[i].*)
      END FOREACH

      INITIALIZE g_reg[i].* TO NULL

   FINISH REPORT rpt_tabcity

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabcity(g_reg)

   DEFINE g_reg RECORD  LIKE dis_ctrl_proceso.*

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN 0 
      RIGHT MARGIN 0
      PAGE LENGTH 60

   FORMAT

   PAGE HEADER

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
      PRINT COLUMN 02,"EXCC001 ",
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
