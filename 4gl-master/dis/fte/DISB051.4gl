###############################################################################
#Proyecto          => safre_af                                                #
#Owner             => E.F.P                                                   #
#Programa DISB051  => EJECUTA VIA BATCH PROCESO DE CALCULO INTERESES RCV EN   #
#                  => TRANSITO PROVISION                                      #
#Created           => 19/AGOSTO/2003.                                         #
#By                => FRANCISCO JAVIER LARIOS HERNANDEZ.                      #
#Sistema           => DIS                                                     #
#                  => VERSION PRODUCTIVA.                                     #
#Modify            => JOSE ALEJANDRO RAMIREZ                                  #
#Descr.            => Se agrego al filtro del query principal un identi       #
#                  => que unicamente toma los registros para rcv evitando     #
#                  => que pasaran folios repetidos                            #
###############################################################################

DATABASE safre_af

GLOBALS

   DEFINE
      HOY               DATE,
      ejecuta           CHAR(150)

   DEFINE g_reg      RECORD
      fecha_aplica   DATE,
      nom_archivo    CHAR(20),
      folios         CHAR(50)
   END RECORD

   DEFINE g_reg2     RECORD
      fecha_aplica   DATE,
      fecha_accion   DATE,
      precio_accion  DECIMAL(16,6),
      folios         CHAR(50)
   END RECORD

   DEFINE
      x_precio_accion   FLOAT,
      opc               CHAR(1),
      resul             CHAR(50),
      usuario           CHAR(8),
      fec               CHAR(8),
      cons              CHAR(03),
      contador          INTEGER,
      tipo_reg          CHAR(02),
      bandera           SMALLINT,
      idx_arreglo       SMALLINT,
      max_pos           SMALLINT,
      ind               SMALLINT,
      folio_int         INTEGER,
      folio_bus         INTEGER,
      folio_char        CHAR(07)

      DEFINE arr_folio ARRAY[15] OF RECORD
	  folio          INTEGER
      END RECORD
      DEFINE  g_param_dis RECORD LIKE seg_modulo.*,
	 comando        CHAR(200),
	 folio_pro      INTEGER

END GLOBALS

MAIN
   OPTIONS
      PROMPT LINE last,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
      DEFER INTERRUPT

   CALL STARTLOG("DISB051.log")

   LET HOY=TODAY

   SELECT USER
   INTO   usuario
   FROM   glo_parametro

   SELECT *
   INTO g_param_dis.*
   FROM seg_modulo
   WHERE modulo_cod = 'dis'

   INITIALIZE arr_folio[1].* TO NULL
   LET idx_arreglo = 1


   OPEN WINDOW ventana_11 AT 3,4
   WITH 3 ROWS,72 COLUMNS
   ATTRIBUTE( BORDER)
   DISPLAY " DISB051 " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE)

   MENU "MENU RCV "
     COMMAND "Provision" "Calculo intereses RCV en transito Provision"
       CALL   Procesa(1)
     COMMAND "Liquidacion" "Liquidacion intereses RCV en transito"
       CALL   Procesa(2)
     COMMAND "Consultas" "Consulta de intereses RCV en transito"
       LET ejecuta = "fglgo DISL014.4gi "
       RUN ejecuta
     COMMAND "Reverso" "Reverso de intereses RCV en transito"
       LET ejecuta = "fglgo DISB040.4gi "
       RUN ejecuta

     COMMAND "Salir" "Salir del Programa"
       EXIT MENU
   END MENU

   CLOSE WINDOW ventana_11


END MAIN  # dispersa_interes

FUNCTION Procesa(paso)

 DEFINE paso SMALLINT,
	extension   CHAR(04)

 CLEAR SCREEN

 IF paso = 1 THEN
    
    ----------------------PARA LA PROVISION---------------------------------
    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "DISB0511" ATTRIBUTE(BORDER)
    DISPLAY " DISB011       PROVISION INTERESES RCV, RECURSOS EN TRANSITO                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                     Intereses recibidos de Operadora                                      " AT 6,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "Fecha Proceso : ",hoy USING "dd-mm-yyyy" AT 5,03

    LET int_flag= FALSE

    INPUT BY NAME g_reg.*
       BEFORE FIELD fecha_aplica
      	LET g_reg.fecha_aplica = hoy
	DISPLAY BY NAME g_reg.fecha_aplica

	LET x_precio_accion = 1

    AFTER FIELD nom_archivo
	IF g_reg.nom_archivo IS NULL THEN
	    ERROR "Este campo no puede ser nulo"
	    NEXT FIELD nom_archivo
        END IF

	SELECT 'X'
        FROM   dis_ctrl_proceso
        WHERE  proceso_cod = 'DISB011'
        AND    parametro2  = g_reg.nom_archivo
        AND    resultado   = 'FINALIZADO'

        IF STATUS = NOTFOUND THEN
        ELSE
	   ERROR  "PROVISION DE ESTE ARCHIVO, REALIZADA ANTERIORMENTE"
           NEXT FIELD nom_archivo
	END IF

	LET bandera = valida_archivo()
	IF bandera = 2 THEN
           ERROR "ESTE ARCHIVO NO EXISTE"
	   NEXT FIELD nom_archivo
        END IF

        LET tipo_reg = null
        LET fec      = null
        LET cons     = null
        LET contador = 0

	DECLARE cursor_tmp CURSOR FOR
	  SELECT n_registros[1,2],n_registros[24,31],n_registros[21,23]
	  FROM   safre_tmp:tmp_pla_rcv
	  WHERE  n_registros[93,93] in ('1','2')
	  AND    n_registros[18] = '5'
          GROUP  BY 1,2,3

        FOREACH cursor_tmp INTO tipo_reg,fec,cons

	  LET contador = contador + 1

          LET folio_pro = null
	  CALL Busca_folio() RETURNING folio_pro
	  LET folio_char = folio_pro
	  LET contador = 0
	  LET resul    = resul CLIPPED,' ',
			 folio_char CLIPPED
        END FOREACH

	LET g_reg.folios = resul
        DISPLAY BY NAME g_reg.folios
	WHENEVER ERROR STOP

    EXIT INPUT

    ON KEY(INTERRUPT)
       LET INT_FLAG = TRUE
       EXIT INPUT

    END INPUT

    IF INT_FLAG THEN
        LET INT_FLAG = FALSE
	CLEAR FORM
        CLEAR SCREEN
	CLOSE WINDOW ventana_1   ----ojo
	RETURN
    END IF

    PROMPT "Desea dispersar intereses [S/N] ? " FOR opc
    IF opc MATCHES "[Ss]" THEN
       ERROR "Procesando Informacion ..."
       CALL Ejecuta_batch2()
       ERROR ""
       CLEAR FORM
       CLEAR SCREEN
       LET resul = ' '
       CLOSE WINDOW ventana_1
       RETURN
    ELSE
       LET resul = ' '
       CLOSE WINDOW ventana_1
       RETURN
    END IF


 ELSE
    ---------------------- PARA LIQUIDACION ----------------------------
    OPEN WINDOW ventana_3 AT 2,2 WITH FORM "DISB0521" ATTRIBUTE(BORDER)
    DISPLAY " DISB012       LIQUIDACION INTERESES RCV, RECURSOS EN TRANSITO                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                     Intereses recibidos de Operadora                                        " AT 6,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "Fecha Proceso : ",hoy USING "dd-mm-yyyy" AT 5,03

    LET INT_FLAG= FALSE

    INPUT BY NAME g_reg2.*
       BEFORE FIELD fecha_aplica
      	LET g_reg2.fecha_aplica = hoy
	DISPLAY BY NAME g_reg2.fecha_aplica

    AFTER FIELD fecha_aplica

	IF g_reg2.fecha_aplica IS NULL THEN
	    ERROR "La fecha no puede ser nula..."
	    NEXT FIELD fecha_aplica
        END IF

        AFTER FIELD fecha_accion
	  
	   SELECT precio_del_dia
	   INTO   x_precio_accion
	   FROM   glo_valor_accion
	   WHERE  fecha_valuacion = g_reg2.fecha_accion
	   AND    codigo_siefore  = 2                ----jerry   --c22-6.7

	   IF STATUS = NOTFOUND THEN
	      LET x_precio_accion = 2                            --c22-6.7
	      ERROR "PRECIO DE ACCION INEXISTENTE A LA FECHA DE APLICACION"
	      NEXT FIELD fecha_accion
           ELSE
	      LET g_reg2.precio_accion = x_precio_accion
	      DISPLAY BY NAME g_reg2.precio_accion
           END IF


	   -- Valida si ya hay provision
	   LET bandera = null
           LET bandera = valida_si_hay_provision()

           LET bandera = 0 --ojo    quitar  alex

           IF bandera = 1 THEN
	      ERROR "LA PROVISION NO HA SIDO PROCESADA AUN"
	      SLEEP 3
   	      NEXT FIELD fecha_aplica
	   END IF



           -- Valida si ya hay liquidacion
	   LET bandera = null
	   LET bandera = valida_si_hay_liquidacion()

           IF bandera = 0 THEN
	      ERROR "LA LIQUIDACION YA HA SIDO PROCESADA CON ANTERIORIDAD"
	      SLEEP 3
   	      NEXT FIELD fecha_aplica
	   END IF
           

	   LET bandera = 0
           LET tipo_reg = null
	   LET fec      = null
	   LET cons     = null
	   LET contador = 0

           DECLARE cursor_tmm CURSOR FOR
 	    SELECT n_registros[1,2],n_registros[24,31],n_registros[21,23]
	    FROM   safre_tmp:tmp_pla_rcv
	    WHERE  n_registros[93,93] in ('1','2')
	    AND    n_registros[18] = '5'
            GROUP  BY 1,2,3
	   FOREACH cursor_tmm INTO tipo_reg,fec,cons

             LET contador = contador + 1

             LET folio_bus = null
	     SELECT a.folio
	     INTO   folio_bus
	     FROM   dis_cza_aporte a
	     WHERE  a.fech_creac_lote = fec
	     AND    a.lote_del_dia    = cons

	     LET arr_folio[idx_arreglo].folio = folio_bus
	     LET idx_arreglo = idx_arreglo + 1

	     SELECT 'X'
	     FROM   dis_ctrl_proceso
	     WHERE  proceso_cod = 'DISB011'
	     AND    folio       = folio_bus
             GROUP BY 1               --ojo quitar alex

	     IF STATUS = NOTFOUND THEN
		LET bandera = 1
		EXIT FOREACH
             END IF

	   END FOREACH

           IF bandera = 1 THEN
	      ERROR "PROVISION NO APLICADA O AUN NO TERMINA"
	      NEXT FIELD fecha_aplica
           END IF

           IF Busca_Liq() THEN
   	      ERROR "LIQUIDACION YA REALIZADA CON ANTERIORIDAD"
	      NEXT FIELD fecha_aplica
	   END IF

           LET max_pos = idx_arreglo - 1

	   FOR ind = 1 TO max_pos
	       LET folio_int = arr_folio[ind].folio

	       SELECT "M"
	       FROM   dis_cuenta
	       WHERE  folio = folio_int
	       AND    subcuenta in (1,2,3)
	       AND    tipo_movimiento = 3
	       GROUP BY 1

	       IF STATUS <> NOTFOUND THEN
		  ERROR "Intereses ya aplicados a la fecha: ",
			 g_reg2.fecha_aplica USING "dd-mm-yyyy"
                  NEXT FIELD fecha_aplica
               END IF

	       LET folio_char    = folio_int
	       LET g_reg2.folios = g_reg2.folios CLIPPED,' ',
			   	   folio_char CLIPPED

           END FOR

	   DISPLAY BY NAME g_reg2.folios

	   EXIT INPUT

	   ON KEY(INTERRUPT)
	      LET INT_FLAG = TRUE
	      EXIT INPUT

           END INPUT

    IF INT_FLAG THEN
        LET INT_FLAG = FALSE
	CLEAR FORM
        CLEAR SCREEN
	CLOSE WINDOW ventana_3
	RETURN
    END IF

    PROMPT "Desea dispersar intereses [S/N] ? " FOR opc
    IF opc MATCHES "[Ss]" THEN
       ERROR "Procesando Informacion ..."
       CALL Ejecuta_batch()
       ERROR ""
       CLEAR FORM
       CLEAR SCREEN
       LET g_reg2.folios = ' '
       CLOSE WINDOW ventana_3
       RETURN
    ELSE
       LET g_reg2.folios = ' '
       CLOSE WINDOW ventana_3
       RETURN
    END IF
END IF

END FUNCTION

FUNCTION Busca_Liq()   --solo en la liquidacion

 DEFINE
    fecha_proc   CHAR(10),
    mes_proc     SMALLINT,
    mes_actual   SMALLINT,
    fecha_actual CHAR(10)

  LET fecha_actual = TODAY
  LET mes_actual   = fecha_actual[1,2]

  SELECT MAX(fecha_proceso)
  INTO   fecha_proc
  FROM   dis_ctrl_proceso
  WHERE  proceso_cod = 'DISB012'

  LET mes_proc = fecha_proc[1,2]

  IF mes_proc = mes_actual THEN
     RETURN TRUE
  ELSE
     RETURN FALSE
  END IF

END FUNCTION


FUNCTION valida_archivo()
   DEFINE vfecha DATE
   DEFINE vfecha_ini DATE
   DEFINE vfecha_fin DATE
   DEFINE com1 CHAR(250)
   DEFINE com2 CHAR(250)
   DEFINE comando2 CHAR(70)
   DEFINE nuevo CHAR(250)

      WHENEVER ERROR CONTINUE
      DROP TABLE tt_archivos
      CREATE TEMP TABLE tt_archivos
      (
	lista CHAR(60)
      )

      WHENEVER ERROR STOP

      LET comando = "cd ",g_param_dis.ruta_rescate CLIPPED,"; ls > arc_tmp"
      RUN comando

      LET comando = g_param_dis.ruta_rescate CLIPPED,"/arc_tmp" CLIPPED
      LOAD FROM comando INSERT INTO tt_archivos

      SELECT "x" 
      FROM   tt_archivos
      WHERE  lista = g_reg.nom_archivo
      
      IF STATUS = NOTFOUND THEN
         RETURN 2             ---no existe archivo
      ELSE
	 RETURN 0             ---si existe archivo
      END IF


END FUNCTION


FUNCTION Busca_folio()

 DEFINE
    vvfolio  INTEGER

    LET vvfolio = 0

    SELECT folio
    INTO   vvfolio
    FROM   dis_cza_aporte
    WHERE  fech_creac_lote = fec
    AND    lote_del_dia    = cons

    RETURN vvfolio

END FUNCTION

FUNCTION valida_si_hay_provision()

  DEFINE  fec_char CHAR(10)
  DEFINE  fec_ini DATE
  DEFINE  fec_fin DATE
  
  LET fec_char = g_reg2.fecha_aplica

  IF fec_char[1,2] < 12 THEN
     LET fec_ini  = MDY(fec_char[1,2],'01',fec_char[7,10])
     LET fec_fin  = MDY(fec_char[1,2] + 1 ,'01',fec_char[7,10])
  ELSE
     LET fec_ini  = MDY(fec_char[1,2],'01',fec_char[7,10])
     LET fec_fin  = MDY('01','01',fec_char[7,10] + 1)
  END IF

  SELECT 'x' FROM dis_ctrl_proceso
  WHERE  proceso_cod='DISB011'
  AND    fecha_proceso between fec_ini and fec_fin
  AND    resultado like 'FINALIZADO%'
  AND    parametro2 <> ' '
  GROUP BY 1

  IF STATUS = NOTFOUND THEN
     RETURN 1
  ELSE
     RETURN 0   
  END IF

END FUNCTION





FUNCTION valida_si_hay_liquidacion()

  DEFINE  fec_char CHAR(10)
  DEFINE  fec_ini DATE
  DEFINE  fec_fin DATE
  
--  LET fec_char = g_reg2.fecha_aplica
  LET fec_char = g_reg2.fecha_accion

  IF fec_char[1,2] < 12 THEN
     LET fec_ini  = MDY(fec_char[1,2],'01',fec_char[7,10])
     LET fec_fin  = MDY(fec_char[1,2] + 1 ,'01',fec_char[7,10])
  ELSE
     LET fec_ini  = MDY(fec_char[1,2],'01',fec_char[7,10])
     LET fec_fin  = MDY('01','01',fec_char[7,10] + 1)
  END IF

  SELECT 'x' FROM dis_ctrl_proceso
  WHERE  proceso_cod='DISB012'
  AND    fecha_proceso between fec_ini and fec_fin
  AND    resultado='FINALIZADO' 
  GROUP BY 1

  IF STATUS = NOTFOUND THEN
     RETURN 1
  ELSE
     RETURN 0   
  END IF

END FUNCTION




#-----------------------------------------------------------------------------
# Para la provision
#-----------------------------------------------------------------------------
FUNCTION Ejecuta_batch2()
  DEFINE l_reg RECORD LIKE cta_interes_rcv.*

  DEFINE l_cta RECORD
     nss       CHAR(11),
     subcuenta SMALLINT,
     folio     INTEGER
  END RECORD

  DEFINE l_mov RECORD
     nss              CHAR(11),
     consecutivo_lote INTEGER,
     folio            INTEGER,
     subcuenta        SMALLINT,
     fecha_valor      DATE,
     fecha_conversion DATE,
     monto_en_pesos   DECIMAL(16,6)
  END RECORD

DEFINE
     interes         FLOAT,
     x_aporte_pesos  FLOAT,
     x_tasa_mes      FLOAT,
     x_fecha_calculo DATE,
     x_fecha_final   DATE,
     vfecha_final    DATE,
     x_fecha_limite  DATE,
     x_fecha_tasa    DATE,
     x_fecha_dias    DATE,
     cont            INTEGER,
     hora_inicial    CHAR(08),
     hora_final      CHAR(08),
     ejecuta         CHAR(200)

  LET hora_inicial = TIME
  LET hora_final   = NULL

  INSERT INTO dis_ctrl_proceso VALUES
     (TODAY,                   -- fecha_proceso
      "DISB011",               -- proceso_cod
      1,                       -- etapa_cod  -- CALCULO INTERESES EN TRAN.
      hora_inicial,            -- hora_inicial
      hora_final,              -- hora_final
      g_reg.fecha_aplica,      -- parametro1
      g_reg.nom_archivo,       -- parametro2
      NULL,                    -- parametro3
      NULL,                    -- parametro4
      NULL,                    -- parametro5
      NULL,                    -- folio
      "PROCESANDO",            -- resultado
      usuario,                 -- usuario
      0)                       -- consecutivo
  IF STATUS < 0 THEN
     ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Intereses ",STATUS
     SLEEP 4
     EXIT PROGRAM
  END IF

  ERROR "Ejecutando Calculo de Interes por nohup ..."
  SLEEP 3

  LET ejecuta = "nohup time fglgo DISB011.4gi ",
 			                       g_reg.fecha_aplica," ",
			                       g_reg.nom_archivo,"  & "
  RUN ejecuta

  ERROR "El proceso se ejecuto satisfactoriamente por nohup"
  SLEEP 3
  ERROR ""

END FUNCTION


#-----------------------------------------------------------------------------
# Para la Liquidacion
#-----------------------------------------------------------------------------

FUNCTION Ejecuta_batch()

   DEFINE l_reg RECORD LIKE cta_interes_rcv.*

   DEFINE l_cta RECORD
      nss       CHAR(11),
      subcuenta SMALLINT,
      folio     INTEGER
   END RECORD

   DEFINE l_mov RECORD
      nss              CHAR(11),
      consecutivo_lote INTEGER,
      folio            INTEGER,
      subcuenta        SMALLINT,
      fecha_valor      DATE,
      fecha_conversion DATE,
      monto_en_pesos   DECIMAL(16,6)
   END RECORD

   DEFINE
      interes         FLOAT,
      x_aporte_pesos  FLOAT,
      x_tasa_mes      FLOAT,
      x_fecha_calculo DATE,
      x_fecha_final   DATE,
      vfecha_final    DATE,
      x_fecha_limite  DATE,
      x_fecha_tasa    DATE,
      x_fecha_dias    DATE,
      cont            INTEGER,
      hora_inicial    CHAR(08),
      hora_final      CHAR(08),
      ejecuta         CHAR(200)

      LET hora_inicial = TIME
      LET hora_final   = NULL

  INSERT INTO dis_ctrl_proceso VALUES
	(TODAY,                   -- fecha_proceso
         "DISB012",               -- proceso_cod
         1,                       -- etapa_cod -- LIQUIDACION INTERESES EN TRAN.
	 hora_inicial,            -- hora_inicial
	 hora_final,              -- hora_final
	 g_reg2.fecha_aplica,      -- parametro1
	 g_reg2.fecha_accion,      -- parametro2
	 g_reg2.precio_accion,     -- parametro3
 	 NULL,                     -- parametro4
	 NULL,                    -- parametro5
	 NULL,                    -- folio
	 "PROCESANDO",            -- resultado
	 usuario,                 -- usuario
	 0)                       -- consecutivo
    IF STATUS < 0 THEN
       ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Intereses ",STATUS
       SLEEP 4
       EXIT PROGRAM
    END IF

    ERROR "Ejecutando Calculo de Interes por nohup ..."
    SLEEP 3
    LET ejecuta = "nohup time fglgo DISB012.4gi ",
						 g_reg2.fecha_aplica," ",
						 g_reg2.fecha_accion," ",
						 g_reg2.precio_accion,"  & "
    RUN ejecuta
    ERROR "El proceso se ejecuto satisfactoriamente por nohup"
    SLEEP 3

END FUNCTION  # dispersa_interes

