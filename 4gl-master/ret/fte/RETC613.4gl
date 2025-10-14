###############################################################################
# Proyecto           => SYSTEMA DE AFORES ( MEXICO )                          #
# Owner              => E.F.P.                                                #
# Programa RETM806   => Generacion de Operacion 13                            #
# Sistema            => RETIRO                                                #
# Fecha Creacion     => 01 - septiembre - 2004                                #
# Autor              => Isai Jimenez Rojas                                    #
###############################################################################

DATABASE safre_af

DEFINE HOY             DATE
DEFINE g_codigo_afore  SMALLINT
DEFINE g_tot_registros INTEGER
DEFINE g_seg_modulo    RECORD LIKE seg_modulo.*
DEFINE g_cmd           CHAR(500)

DEFINE ga_reg2        ARRAY[100] OF RECORD
   
       nss            LIKE ret_solicitud_tx.nss,
       consecutivo    LIKE ret_solicitud_tx.consecutivo,
       diag_reg       CHAR(3)
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

   DEFINE v_pos_act      SMALLINT        -- posicion actual dentro del arreglo
   DEFINE v_elem         SMALLINT   -- # Elementos (detalles) en el arreglo
   
   DEFINE v_status       SMALLINT   -- status de ejecucion
   DEFINE v_num_regs     SMALLINT   -- Numero de regs del archivo
   DEFINE v_detalle05    CHAR(390)  -- para leer cada registro
   DEFINE v_tecla        CHAR(1)    -- para el prompt
   
   DEFER INTERRUPT
    
   CALL STARTLOG("RETC613.log")
  
   CALL init()
   
   OPEN WINDOW w1 AT 2,2 WITH FORM "RETC6131" ATTRIBUTE(BORDER)
   DISPLAY "                      GENERACION DE OPERACION (13)                "
           AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "RETC613" AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
   DISPLAY "ESC = Aceptar, CTRL_C = Cancelar" AT 2,1
  
   INPUT BY NAME lr_reg.* WITHOUT DEFAULTS
      AFTER FIELD tipo_retiro 
            IF lr_reg.tipo_retiro IS NULL THEN
               ERROR "TIPO DE RETIRO NO PUEDE SER NULO"
               NEXT FIELD tipo_retiro
            END IF
            IF lr_reg.tipo_retiro NOT MATCHES "[iI]" THEN
               ERROR "POR EL MOMENTO SOLO APLICA PARA TIPO DE RETIRO PARCIAL(I)"
               NEXT FIELD tipo_retiro
            END IF
           
            SELECT descripcion
              INTO lr_reg.desc_retiro
              FROM tab_retiro
             WHERE tipo_retiro = lr_reg.tipo_retiro

            IF SQLCA.SQLCODE = NOTFOUND THEN
               ERROR "TIPO DE RETIRO NO EXISTE EN CATALOGO"
               NEXT FIELD tipo_retiro
            ELSE
               DISPLAY BY NAME lr_reg.desc_retiro
            END IF
      AFTER INPUT
            IF existe_archivo(lr_reg.tipo_retiro) = FALSE THEN
               ERROR "NO EXISTE ARCHIVO PARA EL TIPO DE RETIRO SELECCIONADO"
               NEXT FIELD tipo_retiro
            END IF
      ON KEY (INTERRUPT)
            EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      EXIT PROGRAM
   END IF
      
      
   DISPLAY "PROCESANDO, ESPERE UN MOMENTO..." AT 21,1 ATTRIBUTE(REVERSE)
   
   -- CARGA LOS DETALLES DEL ARCHIVO EN LA TABLA TEMPORAL (tmp_detalles12)
   
   CALL carga_archivo(lr_reg.tipo_retiro) RETURNING v_status, v_num_regs
   
   DISPLAY "SE CARGARON ",v_num_regs USING "<<<,<<<"," REGISTROS" 
           AT 21,1 ATTRIBUTE(REVERSE)
           
  
   -- CARGA DATOS DE LOS DETALLES EN EL ARREGLO PARA ANEXAR EL DIAGNOSTICO

   DECLARE cur1 CURSOR FOR 
    SELECT * 
      FROM tmp_detalles12

   LET v_elem = 1

   FOREACH cur1 INTO v_detalle05

      LET ga_reg2[v_elem].nss         = v_detalle05[007,017]
      LET ga_reg2[v_elem].consecutivo = v_detalle05[339,349]
      LET v_elem = v_elem +1
   
   END FOREACH

   LET v_elem = v_elem - 1

   CALL SET_COUNT(v_elem)

   ----------------------------------------------------
   -- CAPTURA DE LOS DETALLES DE DIAGNOSTICO A GENERAR
   ----------------------------------------------------

   INPUT ARRAY ga_reg2 WITHOUT DEFAULTS FROM sa_reg2.*
             
         AFTER ROW

             LET v_pos_act = ARR_CURR()
             IF ga_reg2[v_pos_act].diag_reg IS NULL OR ga_reg2[v_pos_act].diag_reg =" " THEN
                ERROR "EL DIAGNOSTICO DE REGISTRO NO PUEDE SER NULO"
                NEXT FIELD diag_reg
             END IF

             IF v_pos_act = v_elem THEN   -- Controla que no se agreguen nuevos elementos
                IF FGL_LASTKEY()!=FGL_KEYVAL("UP") AND 
                   FGL_LASTKEY()!=FGL_KEYVAL("LEFT") THEN
                   NEXT FIELD diag_reg
                END IF
             END IF

         AFTER INPUT 
             error "AFTER INPUT"
             sleep 2
             error ""
                
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
                 IF ga_reg2[i].diag_reg IS NULL OR ga_reg2[i].diag_reg =" " THEN
                    ERROR "HAY CAMPOS DE DIAGNOSTICO VACIOS, CORRIJALOS"
                    CONTINUE INPUT
                 END IF
             END FOR
             EXIT INPUT

   END INPUT

   IF INT_FLAG THEN
      EXIT PROGRAM
   END IF

   -- ACTUALIZA CADA UNO DE LOS DETALLES 
   
   FOR i = 1 TO v_elem
     
      UPDATE tmp_detalles12
         SET linea[05,06]   = "13",
             linea[264,266] = ga_reg2[i].diag_reg
       WHERE linea[007,017] = ga_reg2[i].nss
         
   END FOR

   -------------------------------------------------------
   --GENERACION DE CADA UNO DE LOS DETALLES SELECCIONADOS
   -------------------------------------------------------

   CALL genera_reporte13(lr_reg.tipo_retiro, v_elem)

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
#                                                                         #
#=========================================================================#
FUNCTION existe_archivo(v_tipo_retiro)

   DEFINE v_tipo_retiro  CHAR(1)
   DEFINE v_comando      CHAR(200)
   DEFINE v_status       SMALLINT
   
   LET v_comando = "ls ",g_seg_modulo.ruta_envio CLIPPED,"/DETAV12",
                   " > /dev/null "
                   
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
FUNCTION carga_archivo(v_tipo_retiro)

   DEFINE v_tipo_retiro  CHAR(1)
   DEFINE v_nomarch      CHAR(200)
   DEFINE v_status       SMALLINT
   DEFINE v_num_regs     SMALLINT --Numero de regs cargados
  
   CREATE TEMP TABLE tmp_detalles12
   (
      linea CHAR(360)
   )
   
   LET v_nomarch = g_seg_modulo.ruta_envio CLIPPED,"/DETAV12"

   LOAD FROM v_nomarch INSERT INTO tmp_detalles12
   
   LET v_status   = SQLCA.SQLCODE
   LET v_num_regs = SQLCA.SQLERRD[3]
    
   RETURN v_status, v_num_regs
   
END FUNCTION
 
#=========================================================================#
# Controla la generacion del reporte segun el tipo de retiro              #
#=========================================================================#

FUNCTION genera_reporte13(v_tipo_retiro, v_elem)
   DEFINE v_tipo_retiro       LIKE ret_solicitud_tx.tipo_retiro
   DEFINE v_elem              SMALLINT   -- # de elementos en el arreglo global
   DEFINE i                   SMALLINT
   DEFINE v_nom_arch          CHAR(200)
   DEFINE v_detalle           CHAR(390)
     
   -- GENERA EL ENCABEZADO DE LOTE DE TRANSACCIONES (En archivo de paso)
      
      LET v_nom_arch = g_seg_modulo.ruta_rescate CLIPPED,"/ENCL13"
            
      START REPORT rep_encabezado_tra TO v_nom_arch
      OUTPUT TO REPORT rep_encabezado_tra()
      FINISH REPORT rep_encabezado_tra

   -- GENERA EL ENCABEZADO POR TIPO DE OPERACION (En archivo de paso)
      
      LET v_nom_arch = g_seg_modulo.ruta_rescate CLIPPED,"/ENCO13"
            
      START REPORT rep_encabezado_ope TO v_nom_arch
      OUTPUT TO REPORT rep_encabezado_ope()
      FINISH REPORT rep_encabezado_ope

   -- GENERA EL DETALLE DEL REPORTE DE OPERACIONES 13 (En archivo de paso)

      LET v_nom_arch = g_seg_modulo.ruta_rescate CLIPPED,"/DET13"
         
      START REPORT rep_detalle TO v_nom_arch
          
      DECLARE cur_det_tmp CURSOR FOR 
       SELECT * FROM tmp_detalles12
         
      FOREACH cur_det_tmp INTO v_detalle
          OUTPUT TO REPORT rep_detalle(v_detalle)
      END FOREACH 
      
      FINISH REPORT rep_detalle
      
   -- GENERA EL SUMARIO POR TIPO DE OPERACION 

      LET v_nom_arch = g_seg_modulo.ruta_rescate CLIPPED,"/SUMO13"
      
      START REPORT rep_sumario_ope TO v_nom_arch
      OUTPUT TO REPORT rep_sumario_ope(v_elem)
      FINISH REPORT rep_sumario_ope
   
   -- GENERA EL SUMARIO DE LOTE DE TRANSACCIONES

      LET v_nom_arch = g_seg_modulo.ruta_rescate CLIPPED,"/SUML13"
      
      START REPORT rep_sumario_tra TO v_nom_arch
      OUTPUT TO REPORT rep_sumario_tra(v_elem)
      FINISH REPORT rep_sumario_tra
   
   --- CONCATENA LOS ARCHIVOS EN EL ARCHIVO FINAL
   
      LET g_cmd = "cat ",g_seg_modulo.ruta_rescate CLIPPED,"/ENCL13", " ",
                         g_seg_modulo.ruta_rescate CLIPPED,"/ENCO13", " ",
                         g_seg_modulo.ruta_rescate CLIPPED,"/DET13" , " ",
                         g_seg_modulo.ruta_rescate CLIPPED,"/SUMO13", " ",
                         g_seg_modulo.ruta_rescate CLIPPED,"/SUML13", " > ",
                         g_seg_modulo.ruta_rescate CLIPPED,"/OP13-",v_tipo_retiro
      RUN g_cmd
      
   -- ELIMINA LOS ARCHIVOS DE PASO
   
      LET g_cmd = "rm -f ",g_seg_modulo.ruta_rescate CLIPPED,"/ENCL13", " ",
                           g_seg_modulo.ruta_rescate CLIPPED,"/ENCO13", " ",
                           g_seg_modulo.ruta_rescate CLIPPED,"/DET13" , " ",
                           g_seg_modulo.ruta_rescate CLIPPED,"/SUMO13", " ",
                           g_seg_modulo.ruta_rescate CLIPPED,"/SUML13"
      RUN g_cmd      
   
      ERROR "EL ARCHIVO OP13-",v_tipo_retiro," HA SIDO GENERADO"
      SLEEP 3
      ERROR ""

END FUNCTION 

#=========================================================================#
#  REPORTES DE ENCABEZADO ( DE LOTE Y DE OPERACION )                      #
#=========================================================================#

REPORT rep_encabezado_tra()

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
            COLUMN 005,"03"                            ,
            COLUMN 007,"003"                           ,
            COLUMN 010,"01"                            ,
            COLUMN 012,g_codigo_afore USING"&&&"       ,
            COLUMN 015,HOY USING"YYYYMMDD"             ,
            COLUMN 023,"  "                            ,
            COLUMN 025,336 SPACES
END REPORT

REPORT rep_encabezado_ope()

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
            COLUMN 001,"02"                            ,
            COLUMN 003,"04"                            ,
            COLUMN 005,"13"                            ,
            COLUMN 007,"03"                            ,
            COLUMN 009,"001"                           ,
            COLUMN 012,"01"                            ,
            COLUMN 014,g_codigo_afore USING"&&&"       ,
            COLUMN 017,HOY USING"YYYYMMDD"             ,
            COLUMN 025,"     "                         ,
            COLUMN 028,"13",
            COLUMN 030,331 SPACES
END REPORT
#=========================================================================#
#                                                                         #
#=========================================================================#

REPORT rep_detalle(v_detalle)
   DEFINE v_detalle  CHAR(360)
 
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

REPORT rep_sumario_ope(v_elem)
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

        LET g_tot_registros = v_elem   -- Provisional

        PRINT
            COLUMN 001,"04"                          ,
            COLUMN 003,"04"                          ,
            COLUMN 005,"13"                          ,
            COLUMN 007,"03"                          ,
            COLUMN 009,"001"                         ,
            COLUMN 010,"03"                          ,
            COLUMN 012,"001"                         ,
            COLUMN 015,HOY USING "YYYYMMDD"          ,
            COLUMN 023,g_tot_registros USING"&&&&&&" ,
            COLUMN 025,346 SPACES
END REPORT

REPORT rep_sumario_tra(v_elem)
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

        LET g_tot_registros = v_elem   -- Provisional

        PRINT
            COLUMN 001,"09"                          ,
            COLUMN 003,"04"                          ,
            COLUMN 005,"03"                          ,
            COLUMN 007,"001"                         ,
            COLUMN 010,"01"                          ,
            COLUMN 012,g_codigo_afore USING"&&&"     ,
            COLUMN 015,HOY USING "YYYYMMDD"          ,
            COLUMN 023,g_tot_registros USING"&&"     ,
            COLUMN 025,336 SPACES
END REPORT
