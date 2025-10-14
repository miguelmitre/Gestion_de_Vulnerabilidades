##############################################################################
#Project             => SAFRE (Mexico)                                       #
#Owner               => E.F.P.                                               #
#Programa AFIL028    =>                                                      #
#Fecha actualiz.     => 23 ENERO 2003                                        #
#Actualizacion       => FERNANDO HERRERA HERNANDEZ                           #
#Fecha actualiz.     =>                                                      #
#Sistema             => AFI                                                  #
##############################################################################
DATABASE safre_af
GLOBALS
    DEFINE
           HOY                     DATE,
	   HORA			   CHAR(8),
           f_transf_lote	   DATE,
	   vf_transf_lote	   DATE,
           usuario                 CHAR(08),
           g_paramgrales RECORD    LIKE seg_modulo.*,
           respuesta		   CHAR(1),
           ruta			   CHAR(200),
           reg			   RECORD 
                                   n_seguro		CHAR(11),
                                   paterno		CHAR(40),
                                   materno		CHAR(40),
                                   nombres		CHAR(40)
                                   END RECORD,
	   contador		   INTEGER,
           si_hay		   INTEGER,
           cla_where	 	   CHAR(500),
	   sel_where 	 	   CHAR(500),
           vmaterno	  	   CHAR(40),
    	   vpaterno 		   CHAR(40),
           vnombres	 	   CHAR(40),
	   vnomcomp		   CHAR(120),
	   anomcomp		   CHAR(120),
	   vn_seguro		   CHAR(11)

END GLOBALS
############################################################################
MAIN
    DEFER INTERRUPT
    OPTIONS
    PROMPT LINE LAST

    CALL STARTLOG('AFIL028.log')

    WHENEVER ERROR CONTINUE

    CALL principal()
    CLOSE WINDOW ventana_1

END MAIN

############################################################################

FUNCTION principal()

  CALL init()
  CALL ventana()
  CALL datos()
  CALL pregunta()
  CALL genera()
  CALL preg_sal()

END FUNCTION

############################################################################

FUNCTION init()

    SELECT *, USER
      INTO g_paramgrales.* , usuario        
      FROM seg_modulo
     WHERE @modulo_cod = 'afi'

    LET HORA = TIME
    LET HORA = HORA[1,2] CLIPPED, HORA[4,5] CLIPPED, HORA[7,8] CLIPPED

    LET ruta           = g_paramgrales.ruta_listados CLIPPED, "/", 
			 usuario CLIPPED, ".not_nom_" CLIPPED,
                         TODAY USING "DDMMYY", "." CLIPPED, HORA CLIPPED
    LET HOY            = TODAY
    LET f_transf_lote  = HOY 
    LET contador       = 0

END FUNCTION

############################################################################

FUNCTION ventana()

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIL028" ATTRIBUTE(BORDER)
    DISPLAY " AFIL028                     A F I L I A C I O N                               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "                   REPORTE DE NOMBRE DE REGISTROS NOTIFICADOS                  " AT 6,1 ATTRIBUTE(REVERSE)

END FUNCTION

############################################################################

FUNCTION datos()

   CONSTRUCT BY NAME cla_where ON f_transf_lote

      BEFORE CONSTRUCT 
         DISPLAY BY NAME f_transf_lote 

   END CONSTRUCT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..."
      SLEEP 2
      EXIT PROGRAM
      ERROR ""
    END IF

    LET sel_where = " SELECT count(*)",
                    " FROM afi_det_notifica",
                    " WHERE ", cla_where CLIPPED 
        
    LET sel_where = sel_where CLIPPED

    PREPARE qry_modif FROM sel_where
    DECLARE cursor_m CURSOR FOR qry_modif
    FOREACH cursor_m INTO si_hay
      IF si_hay = 0 THEN
         ERROR "No existe informacion en este periodo."
         SLEEP 2
         ERROR " "
         EXIT PROGRAM
      END IF
    END FOREACH

END FUNCTION

############################################################################

FUNCTION pregunta()

   PROMPT "Desea generar el proceso [S/N]: " FOR respuesta
   IF respuesta NOT MATCHES "[Ss]" THEN
      ERROR "PROCESO CANCELADO. . ."
      SLEEP 2 
      ERROR " "
      EXIT PROGRAM
   END IF

END FUNCTION

############################################################################

FUNCTION genera()

   ERROR "GENERANDO PROCESO. . ."

   START REPORT sale TO ruta 
   LET sel_where = " SELECT unique b.n_seguro,",
		   " a.paterno, a.materno, a.nombres",
                   " FROM afi_mae_afiliado a, afi_det_notifica b",
                   " WHERE ", cla_where CLIPPED ,
                   " AND a.n_seguro = b.n_seguro",
		   " ORDER BY 1"
        
   LET sel_where = sel_where CLIPPED

   PREPARE exe_sql FROM sel_where
   DECLARE c_0 CURSOR FOR exe_sql
   FOREACH c_0 INTO reg.*


      LET reg.paterno         = reg.paterno CLIPPED
      LET reg.materno         = reg.materno CLIPPED 
      LET reg.nombres         = reg.nombres CLIPPED
      LET anomcomp            = reg.nombres CLIPPED, " ", 
                                reg.paterno CLIPPED, " ",
                                reg.materno CLIPPED

      SELECT MAX(@f_transf_lote)
        INTO vf_transf_lote
        FROM afi_det_notifica
       WHERE @n_seguro            = reg.n_seguro

      SELECT unique @n_seguro, @paterno, @materno, @nombres
        INTO vn_seguro, vpaterno, vmaterno, vnombres
        FROM afi_det_notifica
       WHERE @n_seguro            = reg.n_seguro
         AND @f_transf_lote       = vf_transf_lote

      IF STATUS = NOTFOUND THEN

         LET vnomcomp = NULL  
         LET vpaterno = NULL
         LET vmaterno = NULL
         LET vnombres = NULL
      ELSE
         LET vpaterno = vpaterno CLIPPED
         LET vmaterno = vmaterno CLIPPED
         LET vnombres = vnombres CLIPPED
         LET vnomcomp = vnombres CLIPPED, " ", 
                        vpaterno CLIPPED, " ",
                        vmaterno CLIPPED
      END IF 


      IF vnomcomp   MATCHES anomcomp OR
         vnomcomp   LIKE    anomcomp OR
         vnomcomp   =       anomcomp THEN
         CONTINUE FOREACH
      ELSE
         LET contador = contador + 1
         DISPLAY "Total de registros (Reporte): ", contador AT 15,08
         OUTPUT TO REPORT sale (reg.n_seguro, vnomcomp, anomcomp, 
				vf_transf_lote)
      END IF 

   END FOREACH
   FINISH REPORT sale

   ERROR "PROCESO FINALIZADO. . ."
   SLEEP 2
   ERROR " "

END FUNCTION

############################################################################

FUNCTION preg_sal()

   PROMPT "PRESIONE [ ENTER ] PARA SALIR." FOR respuesta


END FUNCTION

############################################################################

REPORT sale (rn_seguro, rvnomcomp, ranomcomp, rf_transf_lote)

   DEFINE rn_seguro		   CHAR(11),
	  rvnomcomp		   CHAR(120),
	  ranomcomp		   CHAR(120),
          rf_transf_lote	   DATE

   OUTPUT  
      PAGE      LENGTH 90
      LEFT 	MARGIN 0
      TOP	MARGIN 0
      BOTTOM	MARGIN 0
      RIGHT	MARGIN 0

   FORMAT 
      FIRST PAGE HEADER 
         PRINT COLUMN 001, "REPORTE COMPARATIVO DE NOMBRES."
         SKIP 1 LINES
         {PRINT COLUMN 001, "|NSS",
               COLUMN 013, "|NOMBRE COMPLETO AFORE",
               COLUMN 065, "|NOMBRE COMPLETO PROCESAR",
               COLUMN 120, "|FECHA NOTIFICACION"}

         PRINT COLUMN 001, "NSS|",
                           "NOMBRE COMPLETO AFORE|",
                           "NOMBRE COMPLETO PROCESAR|",
                           "FECHA NOTIFICACION|"
         SKIP 1 LINES

      ON EVERY ROW

         {PRINT COLUMN 001, "|", rn_seguro      	,
               COLUMN 013, "|", rvnomcomp   	CLIPPED,
               COLUMN 065, "|", ranomcomp       CLIPPED,
               COLUMN 120, "|", rf_transf_lote} 

         PRINT COLUMN 001,      rn_seguro      	,        "|",
                                rvnomcomp   	CLIPPED, "|",
                                ranomcomp       CLIPPED, "|",
                                rf_transf_lote
END REPORT

############################################################################
