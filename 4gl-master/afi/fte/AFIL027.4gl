##############################################################################
#Proyecto            => SAFRE (Mexico)                                       #
#Propietario         => E.F.P.                                               #
#Programa AFIL027    => Reporte registros modificados con notificacion.      #
#Fecha actualiz.     => 21 ENERO 2003                                        #
#Actualizacion       => FERNANDO HERRERA HERNANDEZ                           #
#Fecha actualiz.     =>                                                      #
#Sistema             => AFI                                                  #
##############################################################################
DATABASE safre_af
GLOBALS
    DEFINE
           HOY                     DATE,
	   HORA			   CHAR(8),
           fecha_modifica	   DATE,
           usuario                 CHAR(08),
           g_paramgrales RECORD    LIKE seg_modulo.*,
           respuesta		   CHAR(1),
           ruta			   CHAR(200),
           reg			   RECORD 
                                   mn_seguro		CHAR(11),
                                   mfena		DATE,
                                   msexo		SMALLINT,
                                   mnacionalidad	CHAR(3),
                                   mestadon		SMALLINT,
                                   mtip_prob		SMALLINT,
                                   mfol_prob		CHAR(10),
                                   mdoc_prob		CHAR(16),
              
				   apaterno		CHAR(40),
				   amaterno		CHAR(40),
				   anombres		CHAR(40),
                                   afena		DATE,
                                   asexo		SMALLINT,
                                   anacionalidad	CHAR(3),
                                   aestadon		SMALLINT,
                                   atip_prob		CHAR(1),
                                   afol_prob		CHAR(10),
                                   adoc_prob		CHAR(16),
                                   musuario		CHAR(8),
                                   mfecha_modifica	DATE
                                   END RECORD,
           cuantos		   SMALLINT,
           cuantos_610		   SMALLINT,
	   contador		   INTEGER,
           contador1		   INTEGER,
           si_hay		   INTEGER,
           cla_where	 	   CHAR(500),
	   sel_where 	 	   CHAR(500),
           enter                   CHAR(1)

END GLOBALS
############################################################################
MAIN
    DEFER INTERRUPT
    OPTIONS
    PROMPT LINE LAST

    CALL STARTLOG('AFIL027.log')

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

    LET HORA	       = TIME
    LET HORA	       = HORA[1,2], HORA[4,5], HORA[7,8] CLIPPED
    LET ruta           = g_paramgrales.ruta_listados CLIPPED, 
			 "/", usuario CLIPPED, ".",
			 "mod_not" CLIPPED,
                         TODAY USING "DDMMYY", "." CLIPPED,
			 HORA

    LET HOY            = TODAY
    LET fecha_modifica = HOY 
    LET contador       = 0
    LET contador1      = 0

END FUNCTION

############################################################################

FUNCTION ventana()

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIL027" ATTRIBUTE(BORDER)
    DISPLAY " AFIL027                     A F I L I A C I O N                               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "                   REPORTE DE REGISTOS MODIFICADOS CON NOTIFICACION            " AT 6,1 ATTRIBUTE(REVERSE)

END FUNCTION

############################################################################

FUNCTION datos()

   CONSTRUCT BY NAME cla_where ON b.fecha_modifica

      BEFORE CONSTRUCT 
         DISPLAY BY NAME fecha_modifica 

   END CONSTRUCT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..."
      SLEEP 2
      EXIT PROGRAM
      ERROR ""
    END IF

    LET sel_where = " SELECT count(*)",
                    " FROM afi_mae_afiliado a, afi_mae_modifica b",
                    " WHERE ", cla_where CLIPPED ,
                    " AND a.status_interno IN (120,160,130)",#STATUS MODIFICADOS
                    " AND a.n_seguro = b.n_seguro",
                    " AND a.n_folio = b.n_folio",
                    " AND a.tipo_solicitud = b.tipo_solicitud",
                    " AND b.cod_operacion = 0"
        
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
   LET sel_where = " SELECT b.n_seguro, b.fena, b.sexo, b.nacionalidad,",
                   " b.estadon, b.tip_prob, b.fol_prob, b.doc_prob,",
		   " a.paterno, a.materno, a.nombres,",
                   " a.fena, a.sexo, a.nacionalidad, a.estadon,",
                   " a.tip_prob, a.fol_prob, a.doc_prob, b.usuario,",
                   " b.fecha_modifica",
                   " FROM afi_mae_afiliado a, afi_mae_modifica b",
                   " WHERE ", cla_where CLIPPED ,
                   " AND a.status_interno IN (120,160,130)",#STATUS MODIFICADOS
                   " AND a.n_seguro = b.n_seguro",
                   " AND a.n_folio = b.n_folio",
                   " AND a.tipo_solicitud = b.tipo_solicitud",
                   " AND b.cod_operacion = 0"
        
   LET sel_where = sel_where CLIPPED

   PREPARE exe_sql FROM sel_where
   DECLARE c_0 CURSOR FOR exe_sql
   FOREACH c_0 INTO reg.*

      LET contador1 = contador1 + 1
      SELECT COUNT(*)
        INTO cuantos_610
        FROM cta_act_marca
       WHERE @nss       = reg.mn_seguro
         AND @marca_cod = 610 
      IF cuantos_610 >= 1 THEN
         SELECT COUNT(*)
           INTO cuantos
           FROM afi_det_notifica
          WHERE @n_seguro = reg.mn_seguro
         IF cuantos >= 1 THEN
            LET reg.mfena         = reg.mfena         CLIPPED
            LET reg.afena         = reg.afena         CLIPPED 
            LET reg.msexo         = reg.msexo         CLIPPED
            LET reg.asexo         = reg.asexo         CLIPPED
            LET reg.mnacionalidad = reg.mnacionalidad CLIPPED
            LET reg.anacionalidad = reg.anacionalidad CLIPPED
            LET reg.mestadon      = reg.mestadon      CLIPPED
            LET reg.aestadon      = reg.aestadon      CLIPPED
            LET reg.mtip_prob     = reg.mtip_prob     CLIPPED
            LET reg.atip_prob     = reg.atip_prob     CLIPPED
            LET reg.mfol_prob     = reg.mfol_prob     CLIPPED
            LET reg.afol_prob     = reg.afol_prob     CLIPPED
            LET reg.mdoc_prob     = reg.mdoc_prob     CLIPPED
            LET reg.adoc_prob     = reg.adoc_prob     CLIPPED

            IF reg.mfena         MATCHES reg.afena         OR
               reg.mfena         LIKE    reg.afena	   OR
               reg.mfena         =       reg.afena         THEN
               LET reg.mfena     = NULL
            END IF 

            IF reg.msexo         MATCHES reg.asexo         OR
	       reg.msexo         LIKE    reg.asexo	   OR
               reg.msexo         =       reg.asexo         THEN
               LET reg.msexo     = NULL
            END IF

	    IF reg.mnacionalidad MATCHES reg.anacionalidad OR
               reg.mnacionalidad LIKE    reg.anacionalidad OR
               reg.mnacionalidad =       reg.anacionalidad THEN
               LET reg.mnacionalidad = NULL
    	    END IF

            IF reg.mestadon      MATCHES reg.aestadon      OR
               reg.mestadon      LIKE    reg.aestadon      OR
               reg.mestadon      =       reg.aestadon      THEN
               LET reg.mestadon  = NULL
	    END IF

      	    IF reg.mtip_prob     MATCHES reg.atip_prob   OR
               reg.mtip_prob     LIKE    reg.atip_prob   OR 
               reg.mtip_prob     =       reg.atip_prob   THEN
               LET reg.mtip_prob  = NULL
	    END IF

            IF reg.mfol_prob     MATCHES reg.afol_prob     OR 
               reg.mfol_prob     LIKE    reg.afol_prob     OR
               reg.mfol_prob     =       reg.afol_prob     THEN
               LET reg.mfol_prob = NULL
 	    END IF

            IF reg.mdoc_prob     MATCHES reg.adoc_prob     OR
               reg.mdoc_prob     LIKE    reg.adoc_prob     OR
               reg.mdoc_prob     =       reg.adoc_prob     THEN
               LET reg.mdoc_prob = NULL
	    END IF

            IF reg.mfena         IS NOT NULL OR 
               reg.msexo         IS NOT NULL OR 
               reg.mnacionalidad IS NOT NULL OR
               reg.mestadon      IS NOT NULL OR
               reg.mtip_prob     IS NOT NULL OR
               reg.mfol_prob     IS NOT NULL OR
               reg.mdoc_prob     IS NOT NULL THEN 
               LET contador          = contador + 1
               DISPLAY "Total de registros (Reporte): ", contador AT 15,08
               OUTPUT TO REPORT sale (reg.*)
            END IF
        END IF

      END IF
      DISPLAY "Total de registros (Encontrados): ", contador1 AT 14,08

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

REPORT sale (rep)

   DEFINE rep			   RECORD 
                                   mn_seguro		CHAR(11),
                                   mfena		DATE,
                                   msexo		SMALLINT,
                                   mnacionalidad	CHAR(3),
                                   mestadon		SMALLINT,
                                   mtip_prob		SMALLINT,
                                   mfol_prob		CHAR(10),
                                   mdoc_prob		CHAR(16),
              
				   apaterno		CHAR(40),
				   amaterno		CHAR(40),
				   anombres		CHAR(40),
                                   afena		DATE,
                                   asexo		SMALLINT,
                                   anacionalidad	CHAR(3),
                                   aestadon		SMALLINT,
                                   atip_prob		CHAR(1),
                                   afol_prob		CHAR(10),
                                   adoc_prob		CHAR(16),
                                   musuario		CHAR(8),
                                   mfecha_modifica	DATE
                                   END RECORD,
    	  vpaterno		   			CHAR(40),
          vmaterno					CHAR(40),
          vnombres		   			CHAR(40),
	  vnomcomp					CHAR(80),
    	  apaterno		   			CHAR(40),
          amaterno					CHAR(40),
          anombres		   			CHAR(40),
	  anomcomp					CHAR(80),
	  v_seguro					CHAR(11),
	  vf_transf_lote				DATE 

   OUTPUT  
      PAGE      LENGTH 90
      LEFT 	MARGIN 0
      TOP	MARGIN 0
      BOTTOM	MARGIN 0
      RIGHT	MARGIN 0

   FORMAT 
      FIRST PAGE HEADER 
         PRINT COLUMN 001, "REPORTE DE REGISTROS MODIFICADOS CON NOTIFICACION."
         SKIP 1 LINES
         PRINT COLUMN 001, "NSS|",
                           "NOMBRE COMPLETO|",
                           "NOMBRE COMPLETO PROC|",
                           "FECHA MODIFICACION|",
                           "FECHA NACIMIENTO|",
                           "SEXO|",
                           "NACIONALIDAD|",
                           "ESTADO NAC.|",
                           "TIP. PROB.|",
                           "FOL. PROB.|",
                           "DOC. PROB.|",
                           "USUARIO" 
	 {
         PRINT COLUMN 001, "|NSS",
               COLUMN 013, "|NOMBRE COMPLETO",
               COLUMN 055, "|NOMBRE COMPLETO PROC",
               COLUMN 110, "|FECHA MODIFICACION",
               COLUMN 115, "|FECHA NACIMIENTO",
               COLUMN 120, "|SEXO",
               COLUMN 125, "|NACIONALIDAD",
               COLUMN 140, "|ESTADO NAC.",
               COLUMN 160, "|TIP. PROB.",
               COLUMN 163, "|FOL. PROB.",
               COLUMN 174, "|DOC. PROB.",
               COLUMN 192, "|USUARIO" 
         }
         SKIP 1 LINES

      ON EVERY ROW

         LET anomcomp = rep.anombres CLIPPED, " ", 
                        rep.apaterno CLIPPED, " ",
                        rep.amaterno CLIPPED
         LET apaterno = NULL
         LET amaterno = NULL
         LET anombres = NULL

         SELECT MAX(@f_transf_lote)
           INTO vf_transf_lote
           FROM afi_det_notifica
          WHERE @n_seguro            = rep.mn_seguro

         SELECT unique @n_seguro, @paterno_proc, @materno_proc, @nombres_proc
           INTO v_seguro, vpaterno, vmaterno, vnombres
           FROM afi_det_notifica
          WHERE @n_seguro            = rep.mn_seguro
            AND @f_transf_lote       = vf_transf_lote

         IF STATUS = NOTFOUND THEN

            LET vnomcomp = NULL  
            LET vpaterno = NULL
            LET vmaterno = NULL
            LET vnombres = NULL
         ELSE
            LET vnomcomp = vnombres CLIPPED, " ", 
                           vpaterno CLIPPED, " ",
                           vmaterno CLIPPED
            LET vpaterno = NULL
            LET vmaterno = NULL
            LET vnombres = NULL
         END IF 

         PRINT COLUMN 001,  rep.mn_seguro        ,     "|", 
                            anomcomp   	     CLIPPED,  "|",
                            vnomcomp   	     CLIPPED,  "|",
                            rep.mfecha_modifica  ,     "|", 
                            rep.mfena            ,     "|",
                            rep.msexo            ,     "|",
                            rep.mnacionalidad    ,     "|",
                            rep.mestadon         ,     "|",
                            rep.mtip_prob        ,     "|",
                            rep.mfol_prob        ,     "|",
                            rep.mdoc_prob        ,     "|",
                            rep.musuario         


{
         PRINT COLUMN 001, "|", rep.mn_seguro        , 
               COLUMN 013, "|", anomcomp   	     CLIPPED,
               COLUMN 055, "|", vnomcomp   	     CLIPPED,
               COLUMN 110, "|", rep.mfecha_modifica  ,
               COLUMN 115, "|", rep.mfena            ,
               COLUMN 120, "|", rep.msexo            ,
               COLUMN 125, "|", rep.mnacionalidad    ,
               COLUMN 140, "|", rep.mestadon         ,
               COLUMN 160, "|", rep.mtip_prob        ,
               COLUMN 163, "|", rep.mfol_prob        ,
               COLUMN 174, "|", rep.mdoc_prob        ,
               COLUMN 192, "|", rep.musuario         
}

END REPORT

############################################################################
