################################################################################
#Owner             => E.F.P                                                    #
#Programa SEPB108  => RECIBE RESPUESTA OP 29                                   #
#Fecha creacion    => 22 DE JUNIO 2011
#Por               => JESUS YAÑEZ MORENO                                       #
#Sistema           => SEP                                                      #
################################################################################
DATABASE safre_af
GLOBALS

    DEFINE g_param_sep        RECORD  LIKE seg_modulo.* 
    DEFINE g_idSolicitudSeparacion INTEGER
    DEFINE g_fecha_proceso    DATETIME YEAR TO SECOND
    DEFINE reg_cza_rch_op29   RECORD  LIKE sep_cza_rch_op29.* 
    DEFINE reg_det02_rch_op29 RECORD  LIKE sep_det02_rch_op29.*
    DEFINE reg_det05_rch_op29 RECORD  LIKE sep_det05_rch_op29.*
    DEFINE reg_det03_rch_op29 RECORD  LIKE sep_det03_rch_op29.*
    DEFINE reg_det06_rch_op29 RECORD  LIKE sep_det06_rch_op29.*
    DEFINE reg_sum_rch_op29   RECORD  LIKE sep_sum_rch_op29.* 

    DEFINE 
        HOY                   DATE
     
    DEFINE #char
        carga_reg             CHAR(330),
        enter    	      CHAR(001),
        generar               CHAR(020),
        nombre_archivo        CHAR(020),
        archivo_separa        CHAR(200)

    DEFINE #smallint
        s_notificado               ,
        s_notificado_rechazado     ,
        s_notificado_aceptado      ,
        cuantos                    ,
        cont                  SMALLINT

    DEFINE #integer
        ultimo_folio          INTEGER
END GLOBALS

MAIN
    OPTIONS
        PROMPT LINE LAST
        DEFER INTERRUPT


    CALL STARTLOG("SEPB108.log")

    WHENEVER ERROR CONTINUE
        DROP TABLE sep_plano

        CREATE TEMP TABLE sep_plano
        (
         n_registros          CHAR(300)
        )
    WHENEVER ERROR STOP

        CALL init()
	OPEN WINDOW sepc0011 AT 2,2 WITH FORM "SEPB1081" ATTRIBUTE(BORDER)
        DISPLAY "       [Esc]  Iniciar                                    [Ctrl-C]  Salir       " AT 1,1 ATTRIBUTE(REVERSE)  

        DISPLAY " SEPB108                 RECIBE RESPUESTA  OP29 SEPARACION                     " AT 3,1 ATTRIBUTE(REVERSE)


 	DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

	INPUT BY NAME generar WITHOUT DEFAULTS
            BEFORE FIELD generar
               LET generar = NULL
               CLEAR FORM

	    AFTER FIELD generar
	       IF generar IS NULL THEN
	          ERROR "Campo NO puede ser NULO"
	          NEXT FIELD generar
	       END IF

               SELECT nombre
               INTO   nombre_archivo
               FROM   sep_ctr_archivo
               WHERE  nombre = generar
                
               IF STATUS <> NOTFOUND THEN
                  ERROR " Este archivo ya se recibio "
	    NEXT FIELD generar
               END IF

               WHENEVER ERROR CONTINUE
                  SELECT *
                  INTO   g_param_sep.*
                  FROM   seg_modulo
                  WHERE  modulo_cod = "sep"

                  LET archivo_separa = g_param_sep.ruta_rescate CLIPPED,"/",
                                       generar CLIPPED

                  LOAD FROM archivo_separa DELIMITER "+"
                  INSERT INTO sep_plano
                      
                  SELECT count(*)
                  INTO   cuantos
                  FROM   sep_plano
                    
                  IF cuantos = 0 THEN
                     DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
                     AT 19,2 ATTRIBUTE(REVERSE)
                     SLEEP 2
                     NEXT FIELD generar
                  ELSE
                     EXIT INPUT
                  END IF
               WHENEVER ERROR STOP

	    ON KEY (INTERRUPT)
               ERROR " PROCESO CANCELADO "
               SLEEP 2
               EXIT PROGRAM
	END INPUT


WHILE  TRUE
PROMPT "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? " FOR enter
IF enter  MATCHES "[sSnN]" THEN
   IF enter  MATCHES "[sS]" THEN
      EXIT WHILE
   ELSE
      ERROR  "PROCESO CANCELADO ..." ATTRIBUTE(REVERSE)
      SLEEP  1
      EXIT PROGRAM
   END IF
END IF
END WHILE

        CALL lee_archivo_plano() #lap

        PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR "
        FOR CHAR enter

        CLOSE WINDOW sepc0011
END MAIN

FUNCTION init()
#i-------------
    LET HOY  = TODAY
    LET g_fecha_proceso = CURRENT

    LET s_notificado            = 12
    LET s_notificado_rechazado  = 13
    LET s_notificado_aceptado   = 14

END FUNCTION

FUNCTION lee_archivo_plano()
#lap------------------------

    DECLARE cur_1 CURSOR FOR
    SELECT  a.* 
    FROM     sep_plano a
    ORDER BY a.n_registros[1,2] 
   
    LET cont            = 0
    FOREACH cur_1 INTO carga_reg
        DISPLAY " TOTAL REGISTROS PROCESADOS  : ",cont     AT 12,8


              #---Encabezado por lote ---#

        IF carga_reg[1,2] = "01" THEN
            CALL carga_cza() #cc
        END IF

              #---Detalle de nss---#

        IF carga_reg[1,2] = "02" THEN
            LET cont = cont + 1
	        CALL carga_det02()
        END IF
        IF carga_reg[1,2] = "05" THEN
            LET cont = cont + 1
	        CALL carga_det05()
        END IF

        IF carga_reg[1,2] = "03" THEN
            LET cont = cont + 1
            CALL carga_det03()  #cdn        
        END IF

        IF carga_reg[1,2] = "06" THEN
            LET cont = cont + 1
            CALL carga_det06()  #cdn        
        END IF

        IF carga_reg[1,2] = "09" THEN 
           CALL carga_sum()
        END IF 

    END FOREACH

END FUNCTION

FUNCTION carga_cza() #cc
#cc---------------------

            LET reg_cza_rch_op29.fechaTransferenciaLote = carga_reg[17,24]
            LET reg_cza_rch_op29.fecha_proceso          = g_fecha_proceso  

            INSERT INTO sep_cza_rch_op29 VALUES (reg_cza_rch_op29.*)

            LET ultimo_folio = 0

        INSERT INTO sep_ctr_archivo VALUES(generar           ,
                                    ultimo_folio             ,
                                    reg_cza_rch_op29.fechaTransferenciaLote ,
                                    "29")

END FUNCTION

FUNCTION carga_det02() 
#cdn-----------------

  DEFINE v_corr              INTEGER,
         v_folio             INTEGER


       LET reg_det02_rch_op29.fecha_proceso         = g_fecha_proceso
       LET reg_det02_rch_op29.contadorServicio      = carga_reg[03,12]
       LET reg_det02_rch_op29.invadido              = carga_reg[13,23]
       LET reg_det02_rch_op29.resultadoOperacion    = carga_reg[24,25]
       LET reg_det02_rch_op29.motivoRechazo         = carga_reg[26,34]

       SELECT a.folio,a.correlativo
       INTO   v_folio,v_corr
       FROM  sep_det_reg_sol_reclamante a
       WHERE a.n_seguro  = reg_det02_rch_op29.invadido
       AND   a.estado    = s_notificado

       LET g_idSolicitudSeparacion = v_corr 

       LET reg_det02_rch_op29.idSolicitudSeparacion = g_idSolicitudSeparacion

       IF reg_det02_rch_op29.resultadoOperacion = "02" THEN

               INSERT INTO sep_det02_rch_op29 VALUES (reg_det02_rch_op29.*)

               UPDATE sep_det_reg_sol_reclamante
	       SET    estado       = s_notificado_rechazado
               WHERE  n_seguro     = reg_det02_rch_op29.invadido
               AND    correlativo  = v_corr
               AND    estado       = s_notificado
       ELSE
           UPDATE   sep_det_reg_sol_reclamante
              SET   estado      = s_notificado_aceptado
            WHERE   n_seguro    = reg_det02_rch_op29.invadido
              AND   correlativo = v_corr
              AND   estado      = s_notificado
       END IF		      

END FUNCTION

FUNCTION carga_det05() 
#cdn-----------------


       LET reg_det05_rch_op29.fecha_proceso         = g_fecha_proceso
       LET reg_det05_rch_op29.idSolicitudSeparacion = g_idSolicitudSeparacion
       LET reg_det05_rch_op29.contadorServicio      = carga_reg[03,12]
       LET reg_det05_rch_op29.nrp                   = carga_reg[13,23]
       LET reg_det05_rch_op29.resultadoOperacion    = carga_reg[24,25]
       LET reg_det05_rch_op29.motivoRechazo         = carga_reg[26,34]

       INSERT INTO sep_det05_rch_op29 VALUES (reg_det05_rch_op29.*)

END FUNCTION

FUNCTION carga_det03() 
#cdn------------------

       LET reg_det03_rch_op29.fecha_proceso         = g_fecha_proceso
       LET reg_det03_rch_op29.idSolicitudSeparacion = g_idsolicitudSeparacion
       LET reg_det03_rch_op29.contadorServicio      = carga_reg[03,12]
       LET reg_det03_rch_op29.asociado              = carga_reg[13,23]
       LET reg_det03_rch_op29.resultadoOperacion    = carga_reg[24,25]
       LET reg_det03_rch_op29.motivoRechazo         = carga_reg[26,34]

       INSERT INTO sep_det03_rch_op29 VALUES (reg_det03_rch_op29.*)

END FUNCTION

FUNCTION carga_det06() 
#cdn-----------------

       LET reg_det06_rch_op29.fecha_proceso         = g_fecha_proceso
       LET reg_det06_rch_op29.idSolicitudSeparacion = g_idSolicitudSeparacion
       LET reg_det06_rch_op29.contadorServicio      = carga_reg[03,12]
       LET reg_det06_rch_op29.nrp                   = carga_reg[13,23]
       LET reg_det06_rch_op29.resultadoOperacion    = carga_reg[24,25]
       LET reg_det06_rch_op29.motivoRechazo         = carga_reg[26,34]

       INSERT INTO sep_det06_rch_op29 VALUES (reg_det06_rch_op29.*)

END FUNCTION

FUNCTION carga_sum()
#csum---------------

    LET reg_sum_rch_op29.fecha_proceso = g_fecha_proceso

    INSERT INTO sep_sum_rch_op29 VALUES(reg_sum_rch_op29.*)

END FUNCTION
