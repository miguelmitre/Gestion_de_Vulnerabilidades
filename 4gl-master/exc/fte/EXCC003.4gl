###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.                                                  #
#Programa  EXCC003 => CONSULTA DE ACCIONES
#Sistema           => EXC                                                     #
#Autor             => Omar Sandoval Badillo                                   #
#Fecha             => 19 de enero de 2004.                                    #
###############################################################################
DATABASE safre_af
GLOBALS
   DEFINE g_param    RECORD LIKE seg_modulo.*
 
   DEFINE aux_pausa      CHAR(1),
          enter          SMALLINT,
          opc            SMALLINT,
          pos            SMALLINT,
          pos_1          SMALLINT,
          usuario        CHAR(8),
          g_impre        CHAR(300),
          g_arch         CHAR(300),
          g_lista        CHAR(300),
          sel_where      CHAR(300),
          cla_where      CHAR(300),
          hora           CHAR(8),
          hoy            DATE

   DEFINE reg            RECORD
          folio               DECIMAL(10,0),
          nss                 CHAR(11),
          precio_accion_ini   DECIMAL(16,6),  
          monto_comi_ret      DECIMAL(16,6),
          monto_comi_ces_vej  DECIMAL(16,6),
          subcuenta           SMALLINT,        
          tipo_movimiento     SMALLINT,
          monto_en_acciones   DECIMAL(16,6),
          monto_en_pesos      DECIMAL(16,6)   
   END RECORD   

   DEFINE l_record   ARRAY[9000] OF RECORD
          folio               DECIMAL(10,0),    
          nss                 CHAR(11),
          precio_accion_ini   DECIMAL(16,6)
   END RECORD

   DEFINE l_record_2 ARRAY[9000] OF RECORD
          folio               DECIMAL(10,0),    
          nss                 CHAR(11),
          monto_comi_ret      DECIMAL(16,6),
          monto_comi_ces_vej  DECIMAL(16,6)
   END RECORD

   DEFINE l_record_3 ARRAY[9000] OF RECORD
          folio               DECIMAL(10,0),    
          nss                 CHAR(11),
          subcuenta           SMALLINT,        
          tipo_movimiento     SMALLINT,
          monto_en_acciones   DECIMAL(16,6),
          monto_en_pesos      DECIMAL(16,6)   
   END RECORD

   DEFINE folio_archivo       INTEGER
   
END GLOBALS
------------------------------------------------
MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I

   DEFER INTERRUPT

   LET hoy = TODAY

   SELECT USER,
          ruta_listados
   INTO   usuario,
          g_param.ruta_listados
   FROM   seg_modulo
   WHERE  modulo_cod = "exc"

   CALL inicio()
END MAIN
------------------------------------------------
FUNCTION inicio()
   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "EXCC0031" ATTRIBUTE(BORDER)

   DISPLAY "                                                                               " AT 5,1
   DISPLAY " EXCC003                   CONSULTA DE ACCIONES                                " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "CONSULTAS"
      COMMAND "Inicial" "Consulta de precio inicial"
         CALL inicial(0)
      COMMAND "Comision" "Consulta de cobro de comision"
         CALL inicial(1)
      COMMAND "Acciones" "Consulta de venta de acciones"
         CALL inicial(2)
      COMMAND "Salir" "Sale del programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1

END FUNCTION
------------------------------------------------
FUNCTION inicial(x_param)

   DEFINE x_param         SMALLINT

   IF x_param = 0 THEN
      OPEN WINDOW ventana_2 AT 3,3 WITH FORM "EXCC0031" ATTRIBUTE(BORDER)
      DISPLAY "PRECIO INICIAL" AT 2,62
   ELSE
      IF x_param = 1 THEN
         OPEN WINDOW ventana_3 AT 3,3 WITH FORM "EXCC0032" ATTRIBUTE(BORDER)
         DISPLAY "COBRO DE COMISION" AT 2,59
      ELSE
         IF x_param = 2 THEN
            OPEN WINDOW ventana_4 AT 3,3 WITH FORM "EXCC0033" ATTRIBUTE(BORDER)
            DISPLAY "VENTA DE ACCIONES" AT 2,59
         END IF
      END IF
   END IF
 
   DISPLAY "[Ctrl-C] Salir" AT 2,2
   DISPLAY "[Ctrl-P] Impresion" AT 2,19
   DISPLAY "[Ctrl-W] Archivo" AT 2,41

   DISPLAY " EXCC003                   CONSULTA DE ACCIONES                                " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   LET int_flag = FALSE

   CONSTRUCT cla_where ON   folio
                       FROM folio

      ON KEY(CONTROL-M)
         ERROR " PROCESANDO INFORMACION..."
         SLEEP 1
         ERROR ""
         LET int_flag = FALSE
         EXIT CONSTRUCT
      ON KEY(CONTROL-C)
         LET int_flag = TRUE
         CLEAR FORM
         EXIT CONSTRUCT

   END CONSTRUCT

   IF int_flag = TRUE THEN
      ERROR " CONSULTA CANCELADA..."
      IF x_param = 0 THEN
         CLOSE WINDOW ventana_2
      ELSE
         IF x_param = 1 THEN
            CLOSE WINDOW ventana_3
         ELSE
            IF x_param = 2 THEN
               CLOSE WINDOW ventana_4
            END IF
         END IF
      END IF
      RETURN 
   END IF      

   CASE
      WHEN x_param = 0 
         LET sel_where = " SELECT folio,",
                                 "nss,",
                                 "precio_accion_ini",
                         " FROM   exc_exceso_plu_min",
                         " WHERE ",cla_where CLIPPED
      WHEN x_param = 1
         LET sel_where = " SELECT folio,",
                                 "nss,",
                                 "monto_comi_ret,",
                                 "monto_comi_ces_vej",
                         " FROM   exc_exceso_comis",
                         " WHERE ", cla_where CLIPPED,
		         " AND    monto_comi_ret < 0",
			 " AND    monto_comi_ces_vej < 0"  
      WHEN x_param = 2
         LET sel_where = " SELECT folio,",
                                 "nss,",
                                 "subcuenta,",
                                 "tipo_movimiento,",
                                 "monto_en_acciones,",
                                 "monto_en_pesos",      
                         " FROM   dis_provision",
                         " WHERE ", cla_where CLIPPED,
                         " AND    tipo_movimiento BETWEEN 540 AND 555",
                         " ORDER BY 1,2,3"
   END CASE 

   PREPARE query1 FROM sel_where
   DECLARE cursor1 CURSOR FOR query1

   LET pos = 1

   CASE 
      WHEN x_param = 0
         FOREACH cursor1 INTO l_record[pos].* 
            LET folio_archivo = l_record[pos].folio
            IF l_record[pos].precio_accion_ini <= 0 THEN
               CONTINUE FOREACH
            END IF
            LET pos = pos + 1
         END FOREACH

         INITIALIZE l_record[pos].* TO NULL
      WHEN x_param = 1
         FOREACH cursor1 INTO l_record_2[pos].* 
            LET folio_archivo = l_record[pos].folio
            LET pos = pos + 1
         END FOREACH

         INITIALIZE l_record_2[pos].* TO NULL
      WHEN x_param = 2
         FOREACH cursor1 INTO l_record_3[pos].* 
            LET folio_archivo = l_record[pos].folio
            LET pos = pos + 1
         END FOREACH

         INITIALIZE l_record_3[pos].* TO NULL
   END CASE

   IF (pos - 1) >= 1 THEN
      CALL SET_COUNT(pos - 1)
      ERROR ""

      CASE
         WHEN x_param = 0
            DISPLAY ARRAY l_record TO scr_1.*
               ON KEY (CONTROL-P)
                  CALL impresion(pos,0,0)
               ON KEY (CONTROL-W)
                  CALL impresion(pos,0,1)
               ON KEY (INTERRUPT)
                  ERROR  " CONSULTA CANCELADA..."
                  EXIT DISPLAY
            END DISPLAY 
         WHEN x_param = 1
            DISPLAY ARRAY l_record_2 TO scr_2.*
               ON KEY (CONTROL-P)
                  CALL impresion(pos,1,0)
               ON KEY (CONTROL-W)
                  CALL impresion(pos,1,1)
               ON KEY (INTERRUPT)
                  ERROR  " CONSULTA CANCELADA..."
                  EXIT DISPLAY
            END DISPLAY 
         WHEN x_param = 2
            DISPLAY ARRAY l_record_3 TO scr_3.*
               ON KEY (CONTROL-P)
                  CALL impresion(pos,2,0)
               ON KEY (CONTROL-W)
                  CALL impresion(pos,2,1)
               ON KEY (INTERRUPT)
                  ERROR  " CONSULTA CANCELADA..."
                  EXIT DISPLAY
            END DISPLAY
      END CASE 
   ELSE
      ERROR " ARCHIVO VACIO..."
   END IF

   IF x_param = 0 THEN
      CLOSE WINDOW ventana_2
   ELSE
      IF x_param = 1 THEN
         CLOSE WINDOW ventana_3
      ELSE 
         IF x_param = 2 THEN
            CLOSE WINDOW ventana_4
         END IF
      END IF
   END IF

END FUNCTION
------------------------------------------------
FUNCTION impresion(pos,x_param,salida)

   DEFINE salida    SMALLINT,
          x_param   SMALLINT,
          xx_nombre CHAR(8),
          xxx_nombre CHAR(8)

   DEFINE i        ,
          pos      SMALLINT

   LET hora = TIME

   IF x_param = 0 THEN
      LET xx_nombre = ".IMPINI"
      LET xxx_nombre = ".INI_"
   END IF
   IF x_param = 1 THEN
      LET xx_nombre = ".IMPCOM"
      LET xxx_nombre = ".COM_"
   END IF
   IF x_param = 2 THEN
      LET xx_nombre = ".IMPACC"
      LET xxx_nombre = ".ACC_"
   END IF

   LET g_impre = g_param.ruta_listados CLIPPED,"/",usuario CLIPPED,
                 xx_nombre CLIPPED,hoy USING "dd-mm-yyyy",
                 "_",hora CLIPPED 

   LET g_arch  = "/LISTADOS/",usuario CLIPPED,xxx_nombre CLIPPED,
                 folio_archivo USING "<<<<<"

   IF x_param = 0 AND salida = 1 THEN
	   START REPORT rpt_consulta_a TO g_arch   #"rpt_precio_ini.out"
   ELSE
	   IF x_param = 0 AND salida = 0 THEN
		   START REPORT rpt_consulta TO g_impre
		END IF
	END IF	

   IF x_param = 1 AND salida = 1 THEN
	   START REPORT rpt_consulta1_a TO g_arch   #"rpt_comision.out"
   ELSE
	   IF x_param = 1 AND salida = 0 THEN
		   START REPORT rpt_consulta1 TO g_impre
		END IF
	END IF	

   IF x_param = 2 AND salida = 1 THEN
	  START REPORT rpt_consulta2_a TO g_arch   #"rpt_acciones.out"
   ELSE
	   IF x_param = 2 AND salida = 0 THEN
		   START REPORT rpt_consulta2 TO g_impre
		END IF
	END IF	

      IF x_param = 0 THEN
         FOR i = 1 TO (pos + 1)
            LET reg.folio             = l_record[i].folio   
            LET reg.nss               = l_record[i].nss
            LET reg.precio_accion_ini = l_record[i].precio_accion_ini

            IF reg.folio IS NULL THEN
               EXIT FOR
            END IF 

            IF salida = 1 THEN
               OUTPUT TO REPORT rpt_consulta_a(reg.*,x_param)
            ELSE
               OUTPUT TO REPORT rpt_consulta(reg.*,x_param)
            END IF
         END FOR

         IF salida = 1 THEN
            FINISH REPORT rpt_consulta_a
         ELSE
            FINISH REPORT rpt_consulta
         END IF
      END IF

      IF x_param = 1 THEN
         FOR i = 1 TO (pos + 1)
            LET reg.folio              = l_record_2[i].folio   
            LET reg.nss                = l_record_2[i].nss
            LET reg.monto_comi_ret     = l_record_2[i].monto_comi_ret
            LET reg.monto_comi_ces_vej = l_record_2[i].monto_comi_ces_vej

            IF reg.folio IS NULL THEN
               EXIT FOR
            END IF 

            IF salida = 1 THEN
               OUTPUT TO REPORT rpt_consulta1_a(reg.*,x_param)
            ELSE
               OUTPUT TO REPORT rpt_consulta1(reg.*,x_param)
            END IF
         END FOR

         IF salida = 1 THEN
            FINISH REPORT rpt_consulta1_a
         ELSE
            FINISH REPORT rpt_consulta1
         END IF
      END IF

      IF x_param = 2 THEN
         FOR i = 1 TO (pos + 1)
            LET reg.folio              = l_record_3[i].folio   
            LET reg.nss                = l_record_3[i].nss
            LET reg.subcuenta          = l_record_3[i].subcuenta
            LET reg.tipo_movimiento    = l_record_3[i].tipo_movimiento
            LET reg.monto_en_acciones  = l_record_3[i].monto_en_acciones
            LET reg.monto_en_pesos     = l_record_3[i].monto_en_pesos

            IF reg.folio IS NULL THEN
               EXIT FOR
            END IF 

            IF salida = 1 THEN
               OUTPUT TO REPORT rpt_consulta2_a(reg.*,x_param)
            ELSE
               OUTPUT TO REPORT rpt_consulta2(reg.*,x_param)
            END IF
         END FOR

         IF salida = 1 THEN
            FINISH REPORT rpt_consulta2_a
         ELSE
            FINISH REPORT rpt_consulta2
         END IF
      END IF

   IF salida = 0 THEN
      ERROR "LISTADO GENERADO..."
      #DISPLAY g_impre AT 19,2
      SLEEP 2
      ERROR ""
   ELSE
      ERROR "ARCHIVO GENERADO..."
      DISPLAY g_arch AT 19,2
      SLEEP 2
      ERROR ""
   END IF
 
   IF salida = 0 THEN
      LET g_lista = "lp ",g_impre
      RUN g_lista
   END IF
   
END FUNCTION
------------------------------------------------
REPORT rpt_consulta(reg,x_param)

   DEFINE x_param  SMALLINT,
          salida   SMALLINT

   DEFINE reg           RECORD
          folio               DECIMAL(10,0),
          nss                 CHAR(11),
          precio_accion_ini   DECIMAL(16,6),
          monto_comi_ret      DECIMAL(16,6),
          monto_comi_ces_vej  DECIMAL(16,6),
          subcuenta           SMALLINT,
          tipo_movimiento     SMALLINT,
          monto_en_acciones   DECIMAL(16,6),
          monto_en_pesos      DECIMAL(16,6)   
   END RECORD   

   OUTPUT
      TOP MARGIN     1
      BOTTOM MARGIN  0
      LEFT MARGIN    0
      RIGHT MARGIN   0
      PAGE LENGTH    90
   FORMAT
      PAGE HEADER
         PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
         PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s7B'
         PRINT COLUMN 37,"REPORTE DE PRECIO INICIAL"
         SKIP 3 LINE
         PRINT COLUMN 01,'\033e\033(s218T\033(s11H\033(s7B'
         PRINT COLUMN 02,"EXCC003",
               COLUMN 106,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE
         PRINT COLUMN 01,'\033e\033(s218T\033(s12H\033(s7B'
         PRINT COLUMN 25,"FOLIO",
               COLUMN 59,"NSS",
               COLUMN 86,"PRECIO ACCION INICIAL"
         SKIP 1 LINE

      ON EVERY ROW
         PRINT COLUMN 01,'\033e\033(s218T\033(s13H\033(s7B'
         PRINT COLUMN 20,reg.folio,
               COLUMN 60,reg.nss,
               COLUMN 90,reg.precio_accion_ini 

      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01,"Total de Registros : ",COUNT(*) USING "<<<<"

      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 125," Pagina : ",PAGENO USING "<<<<<"

END REPORT
------------------------------------------------
REPORT rpt_consulta_a(reg,x_param)

   DEFINE x_param  SMALLINT,
          salida   SMALLINT

   DEFINE reg           RECORD
          folio               INTEGER,
          nss                 CHAR(11),
          precio_accion_ini   DECIMAL(16,6),
          monto_comi_ret      DECIMAL(16,6),
          monto_comi_ces_vej  DECIMAL(16,6),
          subcuenta           SMALLINT,
          tipo_movimiento     SMALLINT,
          monto_en_acciones   DECIMAL(16,6),
          monto_en_pesos      DECIMAL(16,6)
   END RECORD

   OUTPUT
      TOP MARGIN     1
      BOTTOM MARGIN  0
      LEFT MARGIN    0
      RIGHT MARGIN   0
      PAGE LENGTH    90

   FORMAT
      PAGE HEADER
         PRINT COLUMN 01,"folio",
               COLUMN 12,"nss",
               COLUMN 21,"precio_accion_ini"
         PRINT

      ON EVERY ROW
         PRINT COLUMN 01,reg.folio USING "<<<<<<",
               COLUMN 08,reg.nss,
               COLUMN 23,reg.precio_accion_ini USING "######&.&&&&&&"

END REPORT
------------------------------------------------
REPORT rpt_consulta1(reg,x_param)

   DEFINE x_param  SMALLINT,
          salida   SMALLINT

   DEFINE reg           RECORD
          folio               DECIMAL(10,0),
          nss                 CHAR(11),
          precio_accion_ini   DECIMAL(16,6),
          monto_comi_ret      DECIMAL(16,6),
          monto_comi_ces_vej  DECIMAL(16,6),
          subcuenta           SMALLINT,
          tipo_movimiento     SMALLINT,
          monto_en_acciones   DECIMAL(16,6),
          monto_en_pesos      DECIMAL(16,6)   
   END RECORD   

   OUTPUT
      TOP MARGIN     1
      BOTTOM MARGIN  0
      LEFT MARGIN    0
      RIGHT MARGIN   0
      PAGE LENGTH    90
   FORMAT
      PAGE HEADER
         PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
         PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s7B'
         PRINT COLUMN 37,"REPORTE DE COBRO COMISION"
         SKIP 3 LINE
         PRINT COLUMN 01,'\033e\033(s218T\033(s11H\033(s7B'
         PRINT COLUMN 02,"EXCC003",
               COLUMN 104,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE
         PRINT COLUMN 01,'\033e\033(s218T\033(s12H\033(s7B'
         PRINT COLUMN 25,"FOLIO",
               COLUMN 47,"NSS",
               COLUMN 65,"MONTO COMISION",
               COLUMN 86,"MONTO COMISION"
         SKIP 1 LINE
         PRINT COLUMN 69,"RETIRO",
               COLUMN 85,"CESANTIA Y VEJES"
         SKIP 1 LINE

      ON EVERY ROW
         PRINT COLUMN 01,'\033e\033(s218T\033(s13H\033(s7B'
         PRINT COLUMN 20,reg.folio,
               COLUMN 47,reg.nss,
               COLUMN 65,reg.monto_comi_ret,
               COLUMN 88,reg.monto_comi_ces_vej

      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01,"Total de Registros : ",COUNT(*) USING "<<<<"

      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 125," Pagina : ",PAGENO USING "<<<<<"

END REPORT
------------------------------------------------
REPORT rpt_consulta1_a(reg,x_param)

   DEFINE x_param  SMALLINT,
          salida   SMALLINT

   DEFINE reg           RECORD
          folio               INTEGER,
          nss                 CHAR(11),
          precio_accion_ini   DECIMAL(16,6),
          monto_comi_ret      DECIMAL(16,6),
          monto_comi_ces_vej  DECIMAL(16,6),
          subcuenta           SMALLINT,
          tipo_movimiento     SMALLINT,
          monto_en_acciones   DECIMAL(16,6),
          monto_en_pesos      DECIMAL(16,6)
   END RECORD

   OUTPUT
      TOP MARGIN     1
      BOTTOM MARGIN  0
      LEFT MARGIN    0
      RIGHT MARGIN   0
      PAGE LENGTH    90
   FORMAT
      PAGE HEADER
         PRINT COLUMN 01,"folio",
               COLUMN 12,"nss",
               COLUMN 21,"monto_comision_ret",
               COLUMN 42,"monto_comision_cv"
         PRINT

      ON EVERY ROW
         PRINT COLUMN 01,reg.folio USING "<<<<<<",
               COLUMN 08,reg.nss,
               COLUMN 25,reg.monto_comi_ret USING "------&.&&&&&&",
               COLUMN 38,reg.monto_comi_ces_vej USING "------&.&&&&&&"

END REPORT
------------------------------------------------
REPORT rpt_consulta2(reg,x_param)

   DEFINE x_param  SMALLINT,
          salida   SMALLINT

   DEFINE reg           RECORD
          folio               DECIMAL(10,0),
          nss                 CHAR(11),
          precio_accion_ini   DECIMAL(16,6),
          monto_comi_ret      DECIMAL(16,6),
          monto_comi_ces_vej  DECIMAL(16,6),
          subcuenta           SMALLINT,
          tipo_movimiento     SMALLINT,
          monto_en_acciones   DECIMAL(16,6),
          monto_en_pesos      DECIMAL(16,6)   
   END RECORD   

   OUTPUT
      TOP MARGIN     1
      BOTTOM MARGIN  0
      LEFT MARGIN    0
      RIGHT MARGIN   0
      PAGE LENGTH    90
   FORMAT
      PAGE HEADER
         PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
         PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s7B'
         PRINT COLUMN 37,"REPORTE DE VENTA ACCIONES"
         SKIP 3 LINE
         PRINT COLUMN 01,'\033e\033(s218T\033(s11H\033(s7B'
         PRINT COLUMN 02,"EXCC003",
               COLUMN 104,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE
         PRINT COLUMN 15,"FOLIO",
               COLUMN 31,"NSS",
               COLUMN 43,"SUBCUENTA",
               COLUMN 57,"TIPO",
               COLUMN 71,"MONTO EN",
               COLUMN 88,"MONTO EN"
         SKIP 1 LINE
         PRINT COLUMN 55,"MOVIMIENTO",
               COLUMN 71,"ACCIONES",
               COLUMN 90,"PESOS"
         SKIP 1 LINE

   ON EVERY ROW
         PRINT COLUMN 01,'\033e\033(s218T\033(s13H\033(s7B'
         PRINT COLUMN 10,reg.folio,
               COLUMN 33,reg.nss,
               COLUMN 50,reg.subcuenta,
               COLUMN 65,reg.tipo_movimiento,
               COLUMN 75,reg.monto_en_acciones,
               COLUMN 95,reg.monto_en_pesos

      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01,"Total de Registros : ",COUNT(*) USING "<<<<"

      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 125," Pagina : ",PAGENO USING "<<<<<"

END REPORT
------------------------------------------------
REPORT rpt_consulta2_a(reg,x_param)

   DEFINE x_param  SMALLINT,
          salida   SMALLINT

   DEFINE reg           RECORD
          folio               INTEGER,
          nss                 CHAR(11),
          precio_accion_ini   DECIMAL(16,6),
          monto_comi_ret      DECIMAL(16,6),
          monto_comi_ces_vej  DECIMAL(16,6),
          subcuenta           SMALLINT,
          tipo_movimiento     SMALLINT,
          monto_en_acciones   DECIMAL(16,6),
          monto_en_pesos      DECIMAL(16,6)
   END RECORD

   OUTPUT
      TOP MARGIN     1
      BOTTOM MARGIN  0
      LEFT MARGIN    0
      RIGHT MARGIN   0
      PAGE LENGTH    90

   FORMAT
      PAGE HEADER
         PRINT COLUMN 01,"folio",
               COLUMN 12,"nss",
               COLUMN 21,"subcuenta",
               COLUMN 32,"tipo_movimiento",
               COLUMN 48,"monto_en_acciones",
               COLUMN 68,"monto_en_pesos"
         PRINT

   ON EVERY ROW
         PRINT COLUMN 01,reg.folio USING "<<<<<<",
               COLUMN 08,reg.nss,
               COLUMN 25,reg.subcuenta,
               COLUMN 38,reg.tipo_movimiento,
               COLUMN 50,reg.monto_en_acciones USING "------&.&&&&&&",
               COLUMN 71,reg.monto_en_pesos USING "------&.&&&&&&"

END REPORT
