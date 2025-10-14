###############################################################################
# Proyecto           => SYSTEMA DE AFORES ( MEXICO )                          #
# Owner              => E.F.P.                                                #
# Programa RETM806   => Generacion de Operacion 06  VERSION 2                 #
# Sistema            => RETIRO                                                #
# Fecha Creacion     => 24 - Agosto - 2004                                    #
# Autor              => Isai Jimenez Rojas                                    #
# Modificacion       => 25/Oct/2004 Isai Jimenez Rojas                        #
#                    => Cambiar la forma de manipulacion, de por tipo de      #
#                    => retiro a global por lote (solicitandose el nombre del #
#                    => lote.                                                 #
###############################################################################

DATABASE safre_af

DEFINE HOY             DATE
DEFINE g_codigo_afore  SMALLINT
DEFINE g_tot_registros INTEGER
DEFINE g_seg_modulo    RECORD LIKE seg_modulo.*
DEFINE g_cmd           CHAR(500)

DEFINE ga_detalle     ARRAY[100] OF RECORD     --Detalles recibidos del archivo
       tipo_retiro    LIKE ret_solicitud_tx.tipo_retiro,
       nss            LIKE ret_solicitud_tx.nss,
       consecutivo    LIKE ret_solicitud_tx.consecutivo,
       diag_reg       CHAR(3),
       diag_inf       CHAR(1)
       END RECORD

DEFINE i              SMALLINT

#=========================================================================#
#                                                                         #
#=========================================================================#
MAIN

   DEFINE lr_reg         RECORD
          tipo_retiro    LIKE tab_retiro.tipo_retiro,
          desc_retiro    LIKE tab_retiro.descripcion
          END RECORD

   DEFINE v_pos_act      SMALLINT   -- posicion actual dentro del arreglo
   DEFINE v_elem         SMALLINT   -- No de Elementos (detalles) en el arreglo
   
   DEFINE v_status       SMALLINT   -- status de ejecucion
   DEFINE v_num_regs     SMALLINT   -- Numero de regs del archivo

   DEFINE v_tecla        CHAR(1)    -- para el prompt
   DEFINE v_nom_archivo  CHAR(30)   -- Nombre del archivo (lote) a procesar
   
   DEFER INTERRUPT
   
   OPTIONS MESSAGE LINE LAST
    
   CALL STARTLOG("RETC612.log")
  
   CALL init()
   
   --------------------------------------------------------
   -- PRIMERO SE SOLICITA EL NOMBRE DEL ARCHIVO A PROCESAR
   --------------------------------------------------------
   
   OPEN WINDOW w1 AT 2,2 WITH FORM "RETC6121" ATTRIBUTE(BORDER)
   DISPLAY "                      GENERACION DE OPERACION (06)                 "
           AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "RETC612" AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
   DISPLAY "ESC = Aceptar, CTRL_C = Cancelar" AT 2,1
   
   -- Captura del Nombre del Archivo 
   
   INPUT v_nom_archivo FROM nom_archivo
      AFTER INPUT
            IF existe_archivo(v_nom_archivo) = FALSE THEN
               ERROR "ARCHIVO INEXISTENTE"
               NEXT FIELD nom_archivo
            END IF
      ON KEY (INTERRUPT)
            EXIT INPUT
   END INPUT
      
   CLOSE WINDOW w1

   IF INT_FLAG THEN
      EXIT PROGRAM
   END IF
   
   ----------------------------------------
   -- CARGA DE LOS REGISTROS DEL ARCHIVO
   ----------------------------------------
   
   MESSAGE "PROCESANDO, ESPERE UN MOMENTO..." ATTRIBUTE(REVERSE)
   
   -- CARGA LOS DETALLES DEL ARCHIVO EN LA TABLA TEMPORAL (tmp_detalles05)
   
   CALL carga_archivo(v_nom_archivo) RETURNING v_status, v_num_regs
   
   MESSAGE ""
   
   DISPLAY "SE CARGARON ",v_num_regs USING "<<<,<<<"," REGISTROS" 
           AT 21,1 ATTRIBUTE(REVERSE)
           
   CALL carga_detalles() RETURNING v_elem

   CALL SET_COUNT(v_elem)

   ----------------------------------------------------
   -- CAPTURA DE LOS DETALLES DE DIAGNOSTICO A GENERAR
   ----------------------------------------------------
   
   OPEN WINDOW w2 AT 2,2 WITH FORM "RETC6122" ATTRIBUTE(BORDER)
   DISPLAY "                      GENERACION DE OPERACION (06)                 "
           AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "RETC612" AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY"  AT 3,68 ATTRIBUTE(REVERSE)
   DISPLAY "ESC = Aceptar, CTRL_C = Cancelar" AT 2,1

   INPUT ARRAY ga_detalle WITHOUT DEFAULTS FROM sa_reg2.*
             
         AFTER ROW
         -------------
             LET v_pos_act = ARR_CURR()
             IF ga_detalle[v_pos_act].diag_reg IS NULL OR ga_detalle[v_pos_act].diag_reg =" " THEN
                ERROR "EL DIAGNOSTICO DE REGISTRO NO PUEDE SER NULO"
                NEXT FIELD diag_reg
             END IF
             IF ga_detalle[v_pos_act].diag_inf IS NULL OR ga_detalle[v_pos_act].diag_inf =" " THEN
                ERROR "EL DIAGNOSTICO DE VIVIENDA NO PUEDE SER NULO"
                NEXT FIELD diag_inf
             END IF

             IF v_pos_act = v_elem THEN   -- Controla que no se agreguen nuevos elementos
                IF FGL_LASTKEY() != FGL_KEYVAL("UP") AND FGL_LASTKEY() != FGL_KEYVAL("LEFT") THEN
                   NEXT FIELD diag_inf
                END IF
             END IF

         AFTER INPUT 
         -------------
             ERROR "AFTER INPUT"
             SLEEP 2
             ERROR ""
                
         ON KEY (INTERRUPT)
             ERROR "SALIDA POR INTERRUPCION"
             SLEEP 2
             ERROR ""
             EXIT INPUT

         ON KEY (CONTROL-C)
             ERROR "SALIDA POR INTERRUPCION CON CONTROL-C"
             SLEEP 2
             ERROR ""
             EXIT INPUT

         ON KEY (ACCEPT)
             -- Si se esta abandonando la captura 
             -- se validan todas las ocurrencias
             FOR i = 1 TO v_elem
                 IF ga_detalle[i].diag_reg IS NULL OR ga_detalle[i].diag_reg =" " OR
                    ga_detalle[i].diag_inf IS NULL OR ga_detalle[i].diag_inf =" " THEN
                    ERROR "HAY CAMPOS DE DIAGNOSTICO VACIOS, CORRIJALOS"
                    CONTINUE INPUT
                    --NEXT FIELD diag_inf
                 END IF
             END FOR
             EXIT INPUT

   END INPUT

   IF INT_FLAG THEN
      EXIT PROGRAM
   END IF

   -- ACTUALIZA CADA UNO DE LOS DETALLES 
   
   FOR i = 1 TO v_elem
     
      UPDATE tmp_detalles05
         SET linea[05,06]   = "06",
             linea[364,366] = ga_detalle[i].diag_reg,
             linea[367,367] = ga_detalle[i].diag_inf
       WHERE linea[007,017] = ga_detalle[i].nss
         AND linea[339,349] = ga_detalle[i].consecutivo
         
   END FOR

   -------------------------------------------------------
   --GENERACION DE CADA UNO DE LOS DETALLES SELECCIONADOS
   -------------------------------------------------------

   CALL genera_reporte06(lr_reg.tipo_retiro, v_elem)

   OPTIONS PROMPT LINE 21
   PROMPT "PRESIONE RETURN PARA CONTINUAR " ATTRIBUTE(REVERSE) 
          FOR CHAR v_tecla

END MAIN

#=========================================================================#
#                                                                         #
#=========================================================================#

FUNCTION init()

    LET HOY  = TODAY  

    SELECT codigo_afore
    INTO   g_codigo_afore
    FROM   tab_afore_local

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"
    
END FUNCTION

#=========================================================================#
# Verifica si el archivo proporcionado existe en la ruta de envio         #
#=========================================================================#
FUNCTION existe_archivo(v_nom_archivo)

   DEFINE v_nom_archivo  CHAR(100)
   DEFINE v_comando      CHAR(200)
   DEFINE v_status       SMALLINT
   
   --Arma el Nombre de Archivo
   LET v_nom_archivo = g_seg_modulo.ruta_envio CLIPPED,"/",v_nom_archivo

   MESSAGE "verificando: ",v_nom_archivo CLIPPED
   SLEEP 2
   MESSAGE ""
                      
   LET v_comando = "ls ",v_nom_archivo CLIPPED," > /dev/null "
                   
   RUN v_comando RETURNING v_status
   
   IF v_status != 0 THEN
      RETURN FALSE
   ELSE
      RETURN TRUE
   END IF
   
END FUNCTION

#=========================================================================#
#                                                                         #
#=========================================================================#
FUNCTION carga_archivo(v_nom_archivo)

   DEFINE v_nom_archivo  CHAR(100)   -- NOmbre del archivo a procesar
   DEFINE v_status       SMALLINT    -- Status de ejecucion de la carga
   DEFINE v_num_regs     SMALLINT    -- Numero de registros cargados
  
   -- TAMBLA TEMPORAL PARA CARGA DE LAS LINEAS DEL ARCHIVO
   
   CREATE TEMP TABLE tmp_detalles05
   (
      linea CHAR(390)
   )
   
   --Arma el Nombre de Archivo
   LET v_nom_archivo = g_seg_modulo.ruta_envio CLIPPED,"/",v_nom_archivo

   WHENEVER ERROR CONTINUE
      
   LOAD FROM v_nom_archivo INSERT INTO tmp_detalles05
   
   WHENEVER ERROR STOP
   
   LET v_status   = SQLCA.SQLCODE
   LET v_num_regs = SQLCA.SQLERRD[3]
    
   RETURN v_status, v_num_regs
   
END FUNCTION
 
#=========================================================================#
#                                                                         #
#=========================================================================#
FUNCTION carga_detalles() 
   DEFINE v_elem         SMALLINT
   DEFINE v_detalle05    CHAR(390)  -- para leer cada registro
   
   -- CARGA DATOS DE LOS DETALLES EN EL ARREGLO PARA ANEXAR EL DIAGNOSTICO

   DECLARE cur1 CURSOR FOR 
    SELECT * 
      FROM tmp_detalles05

   LET v_elem = 1

   FOREACH cur1 INTO v_detalle05

      --De cada Registro procesado Eliminar Encabezado y Detalle
      IF v_detalle05[1,2] != "01"  AND    -- Encabezado
         v_detalle05[1,2] != "09"  THEN   -- Sumario
            
         IF v_detalle05[1,2] = "03"  THEN    -- Detalle
            LET ga_detalle[v_elem].tipo_retiro = v_detalle05[158,158]
            LET ga_detalle[v_elem].nss         = v_detalle05[007,017]
            LET ga_detalle[v_elem].consecutivo = v_detalle05[339,349]
            LET v_elem = v_elem +1
         ELSE
            ERROR "EL ARCHIVO NO CONTIENE EL FORMATO REQUERIDO, PROCESO CANCELADO"
            SLEEP 3
            EXIT PROGRAM
         END IF
            
      END IF
   
   END FOREACH

   LET v_elem = v_elem - 1

   RETURN v_elem

END FUNCTION

 
 
#=========================================================================#
# Controla la generacion del reporte segun el tipo de retiro              #
#=========================================================================#

FUNCTION genera_reporte06(v_tipo_retiro, v_elem)
   DEFINE v_tipo_retiro       LIKE ret_solicitud_tx.tipo_retiro
   DEFINE v_elem              SMALLINT   -- # de elementos en el arreglo global
   DEFINE i                   SMALLINT
   DEFINE v_nom_arch          CHAR(200)
   DEFINE v_detalle           CHAR(390)
   DEFINE v_mensaje           CHAR(80)
   DEFINE v_status            SMALLINT  -- Estatus de ejecucion del Run     
   ----------------------------------------------------------
   -- GENERA EL ENCABEZADO DEL REPORTE (En archivo de paso)
   ----------------------------------------------------------
      
      LET v_nom_arch = g_seg_modulo.ruta_rescate CLIPPED,"/ENC06"
            
      START REPORT rep_encabezado TO v_nom_arch
      OUTPUT TO REPORT rep_encabezado()
      FINISH REPORT rep_encabezado

   ----------------------------------------------------------
   -- GENERA EL DETALLE DEL REPORTE DE OPERACIONES 06 (En archivo de paso)
   ----------------------------------------------------------

      LET v_nom_arch = g_seg_modulo.ruta_rescate CLIPPED,"/DET06"
         
      START REPORT rep_detalle TO v_nom_arch
          
      DECLARE cur_det_tmp CURSOR FOR 
         SELECT * FROM tmp_detalles05
          WHERE linea[1,2] NOT IN ("01" ,"09")
          
      FOREACH cur_det_tmp INTO v_detalle
          OUTPUT TO REPORT rep_detalle(v_detalle)
      END FOREACH 
      
      FINISH REPORT rep_detalle
      
   -- GENERA EL SUMARIO DEL REPORTE

      LET v_nom_arch = g_seg_modulo.ruta_rescate CLIPPED,"/SUM06"
      
      START REPORT rep_sumario TO v_nom_arch
      OUTPUT TO REPORT rep_sumario(v_elem)
      FINISH REPORT rep_sumario
   
   --- CONCATENA LOS ARCHIVOS EN EL ARCHIVO FINAL
   
      LET g_cmd = "cat ",g_seg_modulo.ruta_rescate CLIPPED,"/ENC06", " ",
                         g_seg_modulo.ruta_rescate CLIPPED,"/DET06", " ",
                         g_seg_modulo.ruta_rescate CLIPPED,"/SUM06", " > ",
                         g_seg_modulo.ruta_rescate CLIPPED,"/OP06-",TODAY USING "DDMMYYYY"
      RUN g_cmd
      
   -- ELIMINA LOS ARCHIVOS DE PASO
   
      LET g_cmd = "rm -f ",g_seg_modulo.ruta_rescate CLIPPED,"/ENC06", " ",
                           g_seg_modulo.ruta_rescate CLIPPED,"/DET06", " ",
                           g_seg_modulo.ruta_rescate CLIPPED,"/SUM06"
      RUN g_cmd RETURNING v_status
      
      IF v_status != 0 THEN
         ERROR "NO PUDO GENERARSE EL ARCHIVO, VERIFIQUE PERMISOS"
      ELSE
         LET v_mensaje = "EL ARCHIVO OP06-",TODAY USING "DDMMYYYY"," HA SIDO GENERADO"
         ERROR v_mensaje
      END IF
         
      SLEEP 3
      ERROR ""

END FUNCTION 

#=========================================================================#
#                                                                         #
#=========================================================================#

REPORT rep_encabezado()

   DEFINE f_transferencia  DATE
   
    OUTPUT
        PAGE LENGTH   1
	LEFT MARGIN   0
	RIGHT MARGIN  0
	TOP MARGIN    0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW 
            CALL habil_siguiente(HOY,3)
            RETURNING f_transferencia
        PRINT
            COLUMN 001,"01"                            ,
            COLUMN 003,"04"                            ,
            COLUMN 005,"01"                            ,
            COLUMN 007,g_codigo_afore USING"&&&"       ,
            COLUMN 010,"03"                            ,
            COLUMN 012,"001"                           ,
            COLUMN 015,HOY USING"YYYYMMDD"             ,
            COLUMN 023,f_transferencia USING"YYYYMMDD" ,
            COLUMN 031, 340 SPACES
END REPORT

#=========================================================================#
#                                                                         #
#=========================================================================#

REPORT rep_detalle(v_detalle)
   DEFINE v_detalle  CHAR(390)
 
   OUTPUT
        PAGE LENGTH   1
	LEFT MARGIN   0
	RIGHT MARGIN  0
	TOP MARGIN    0
	BOTTOM MARGIN 0

   FORMAT
      ON EVERY ROW 

         PRINT COLUMN 001,v_detalle

END REPORT

#=========================================================================#
#                                                                         #
#=========================================================================#

REPORT rep_sumario(v_elem)
#st--------------
    DEFINE v_elem SMALLINT  -- Numero re registros del arreglo (procesados)
    
    OUTPUT
        PAGE LENGTH   1
	LEFT MARGIN   0
	RIGHT MARGIN  0
	TOP MARGIN    0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW 
        LET g_tot_registros = 0

        { -- Despues me lo explicara Franco
        SELECT SUM(A.total_registros)
        INTO   g_tot_registros
        FROM   ret_ctr_envio_lote A, tab_tramite_retiro B, tab_retiro C
        WHERE  A.tipo_retiro = B.tipo_retiro
        AND    A.estado      = 2
        AND    B.cod_tramite = 2
        AND    A.tipo_retiro = C.tipo_retiro
        }
        LET g_tot_registros = v_elem   -- Provisional

        PRINT
            COLUMN 001,"09"                          ,
            COLUMN 003,"04"                          ,
            COLUMN 005,"01"                          ,
            COLUMN 007,g_codigo_afore USING"&&&"     ,
            COLUMN 010,"03"                          ,
            COLUMN 012,"001"                         ,
            COLUMN 015,HOY USING "YYYYMMDD"          ,
            COLUMN 023,g_tot_registros USING"&&&&&&" ,
            COLUMN 025,346 SPACES
END REPORT
