#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC704  => NOHUP DE EXTRACCION RETIRO POR DESEMPLEO MENSUAL          #
#Fecha creacion    => 24-Jul-2016                                               #
#By                => CRISTINA ABASOLO TAPIA                                    #
#Sistema           => RET                                                       #
#################################################################################

DATABASE safre_af

DEFINE mr_rango     RECORD
       fecha_ini      DATE,
       fecha_fin      DATE
END RECORD

DEFINE mr_edo RECORD
       capturado           SMALLINT    ,
       enviado_op67        SMALLINT    ,
       recibido_op68       SMALLINT    ,
       confirmado          SMALLINT    ,
       liquidado           SMALLINT    ,
       enviado_op16        SMALLINT    ,
       recibido_op16       SMALLINT    ,
       suspendido          SMALLINT    ,
       rechazado           SMALLINT    ,
       reinv_cajas         SMALLINT
END RECORD

DEFINE
       m_usuario      VARCHAR(10),
       m_log          VARCHAR(30),
       m_ruta         CHAR(300),
       m_cuantos      INTEGER,
       m_enter        CHAR(01),
       mr_param       RECORD LIKE safre_af:seg_modulo.*,
       m_hoy          DATE
   
MAIN
   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I
           DEFER INTERRUPT

WHENEVER ERROR STOP

CALL f_lib_abre_log("RETL857")
CALL init()
CALL f_proceso()  #proceso principal

END MAIN

#################################################################################
FUNCTION init()

   --Parametros globales y usuario que ejecuta
   SELECT a.*,USER 
   INTO mr_param.*, m_usuario
   FROM safre_af:seg_modulo a
   WHERE a.modulo_cod = "ret"
   
   INITIALIZE mr_rango.* TO NULL
   
   LET mr_rango.fecha_ini = TODAY
   LET mr_rango.fecha_fin = TODAY
   LET m_hoy              = TODAY
   
   ----- ESTADOS DE SOLICITUD -----
   SELECT A.estado_solicitud 
   INTO   mr_edo.capturado
   FROM   ret_estado A
   WHERE  A.descripcion = "CAPTURADO"
   
   SELECT A.estado_solicitud 
   INTO   mr_edo.enviado_op67
   FROM   ret_estado A
   WHERE  A.descripcion = "ENVIADO OP67"

   SELECT A.estado_solicitud 
   INTO   mr_edo.recibido_op68
   FROM   ret_estado A
   WHERE  A.descripcion = "RECIBIDO OP68"

   SELECT A.estado_solicitud 
   INTO   mr_edo.confirmado
   FROM   ret_estado A
   WHERE  A.descripcion = "CONFIRMADO"

   SELECT A.estado_solicitud 
   INTO   mr_edo.liquidado
   FROM   ret_estado A
   WHERE  A.descripcion = "LIQUIDADO"
   
   SELECT A.estado_solicitud 
   INTO   mr_edo.enviado_op16
   FROM   ret_estado A
   WHERE  A.descripcion = "ENVIADO OP16"

   SELECT A.estado_solicitud 
   INTO   mr_edo.recibido_op16
   FROM   ret_estado A
   WHERE  A.descripcion = "RECIBIDO OP16"

   SELECT A.estado_solicitud 
   INTO   mr_edo.suspendido
   FROM   ret_estado A
   WHERE  A.descripcion = "SUSPENDIDO"

   SELECT A.estado_solicitud 
   INTO   mr_edo.rechazado
   FROM   ret_estado A
   WHERE  A.descripcion = "RECHAZADO"

   SELECT A.estado_solicitud 
   INTO   mr_edo.reinv_cajas
   FROM   ret_estado A
   WHERE  A.descripcion = "PAGOS REINTEGRADOS CAJAS COPPEL"

   
END FUNCTION

#################################################################################
FUNCTION f_proceso()

CALL f_ventana()
CALL f_tablas()

    INPUT BY NAME mr_rango.* WITHOUT DEFAULTS
    
    AFTER FIELD fecha_ini 
           IF  mr_rango.fecha_ini IS NULL THEN
                   ERROR "DEBE INGRESAR LA FECHA INICIAL DE LA BUSQUEDA"
                   NEXT FIELD fecha_ini
           END IF
           
    AFTER FIELD fecha_fin 
           IF  mr_rango.fecha_fin IS NULL THEN
                   ERROR "DEBE INGRESAR LA FECHA FIN DE LA BUSQUEDA"
                   NEXT FIELD fecha_fin
           END IF
           
    ON KEY (ESC)
           IF  mr_rango.fecha_ini IS NULL THEN
                    ERROR "DEBE INGRESAR LA FECHA INICIAL DE LA BUSQUEDA"
                    NEXT FIELD fecha_ini
            END IF
            
            IF  mr_rango.fecha_fin IS NULL THEN
                    ERROR "DEBE INGRESAR LA FECHA FIN DE LA BUSQUEDA"
                    NEXT FIELD fecha_fin
            END IF
           
           ERROR ""
           PROMPT "¿DESEA EXTRAER LA INFORMACION ASOCIADA AL RANGO DE FECHAS? (S/N)" FOR CHAR m_enter
     
           IF m_enter MATCHES "[SsNn]" THEN
              IF m_enter MATCHES "[Ss]" THEN
                 IF f_carga(mr_rango.fecha_ini, mr_rango.fecha_fin) = 0 THEN 
                    CALL f_genera()
                 END IF
              ELSE #Si respuesta Nn
                 ERROR "PROCESO CANCELADO" SLEEP 2
                 ERROR ""
                 EXIT PROGRAM
              END IF   
           ELSE
              ERROR "SOLO PRESIONE S o N"
           END IF  
           EXIT INPUT
     
        ON KEY (CONTROL-C, INTERRUPT)
           ERROR "PROCESO CANCELADO" SLEEP 2
           ERROR ""
           EXIT PROGRAM
     
   END INPUT
  
   PROMPT   "PROCESO FINALIZADO...<Enter> PARA SALIR" ATTRIBUTE (REVERSE) FOR  CHAR  m_enter

CLOSE WINDOW win1

END FUNCTION

#################################################################################
FUNCTION f_tablas()

 DATABASE safre_tmp

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_carga
        IF SQLCA.SQLCODE < 0 AND SQLCA.SQLCODE <> -206  THEN
           PROMPT   "SE ESTA GENERANDO UN REPORTE ACTUALMENTE...<Enter> PARA SALIR" ATTRIBUTE (REVERSE) FOR  CHAR  m_enter
           EXIT PROGRAM
        END IF
        
        DROP TABLE tmp_carga_parcialidades
        IF SQLCA.SQLCODE < 0 AND SQLCA.SQLCODE <> -206  THEN
           PROMPT   "SE ESTA GENERANDO UN REPORTE ACTUALMENTE...<Enter> PARA SALIR" ATTRIBUTE (REVERSE) FOR  CHAR  m_enter
           EXIT PROGRAM
        END IF
        
        DROP TABLE tmp_reinv
        IF SQLCA.SQLCODE < 0 AND SQLCA.SQLCODE <> -206  THEN
           PROMPT   "SE ESTA GENERANDO UN REPORTE ACTUALMENTE...<Enter> PARA SALIR" ATTRIBUTE (REVERSE) FOR  CHAR  m_enter
           EXIT PROGRAM
        END IF
        
        DROP TABLE tmp_rechazos
        IF SQLCA.SQLCODE < 0 AND SQLCA.SQLCODE <> -206  THEN
           PROMPT   "SE ESTA GENERANDO UN REPORTE ACTUALMENTE...<Enter> PARA SALIR" ATTRIBUTE (REVERSE) FOR  CHAR  m_enter
           EXIT PROGRAM
        END IF
    WHENEVER ERROR STOP

    CREATE TABLE tmp_carga(
    nss                   CHAR(11),
    consecutivo           INTEGER,
    folio                 DECIMAL(11,0),
    tipo_desempleo        CHAR(1),
    tipo_pago             SMALLINT,
    pago_desempleo        DECIMAL(22,6),
    num_resolucion        INTEGER,
    fecha                 DATE
    ) 
    
    GRANT ALL ON tmp_carga TO PUBLIC
    
    CREATE TABLE tmp_carga_parcialidades(
    nss                   CHAR(11),
    curp                  CHAR(18),
    consecutivo           INTEGER,
    folio                 DECIMAL(11,0),
    fecha                 DATE
    ) 
    
    GRANT ALL ON tmp_carga_parcialidades TO PUBLIC
    
    CREATE TABLE tmp_reinv(
    nss                   CHAR(11),
    consecutivo           INTEGER,
    folio                 DECIMAL(11,0),
    tip_mov               SMALLINT,
    fecha                 DATE,
    monto                 DECIMAL(22,6)
    ) 
    
    GRANT ALL ON tmp_reinv TO PUBLIC

    CREATE TABLE tmp_rechazos(
    nss                   CHAR(11),
    consecutivo           INTEGER,
    tipo_desempleo        CHAR(1),
    fecha                 DATE,
    diag_resol            CHAR(03),
    diag_retiro           CHAR(03)
    ) 
    
    GRANT ALL ON tmp_rechazos TO PUBLIC
    

  DATABASE safre_af

END FUNCTION 

#################################################################################
FUNCTION f_ventana()

OPEN WINDOW win1 AT 2,2 WITH FORM "RETL8571" ATTRIBUTE(BORDER)

 DISPLAY " [Esc] Iniciar                                                      [Ctrl-C] Salir   " AT 1,1 ATTRIBUTE(REVERSE)  
 DISPLAY " RETL857           EXTRACCION INFORMACION PARCIALES             " ,m_hoy USING"DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)

END FUNCTION

#################################################################################
FUNCTION f_carga(ld_fecha_ini, ld_fecha_fin)

DEFINE    ld_fecha_ini,
          ld_fecha_fin   DATE
          
DEFINE
          lc_query            CHAR(700),
          li_tot_soli         INTEGER,
          li_tot_par          INTEGER,
          li_tot_reinv        INTEGER,
          li_tot_rech         INTEGER,
          li_total            INTEGER

LET li_tot_soli  = 0
LET li_tot_par   = 0
LET li_tot_reinv = 0
LET li_tot_rech  = 0
LET li_total     = 0

DISPLAY "PROCESANDO INFORMACION..."  AT 18,3

CALL f_genera_tmp_cuenta(1)

LET lc_query = ""
LET lc_query = "INSERT INTO safre_tmp:tmp_carga ",
               "SELECT UNIQUE nss, consecutivo, ",
               "       folio, tipo_desempleo, tipo_pago, pago_desempleo, num_resolucion, fecha_envio ",
               "FROM ret_parcial ",
               "WHERE nss IN (SELECT UNIQUE nss FROM tmp_dis_cuenta_reinv)",
               "AND consecutivo IN (SELECT UNIQUE consecutivo_lote FROM tmp_dis_cuenta_reinv)",
               "AND tipo_prestacion = 6 ",
               "AND estado_solicitud IN (",mr_edo.liquidado,",",mr_edo.enviado_op16,",",mr_edo.recibido_op16,")"
LET lc_query = lc_query CLIPPED
#DISPLAY lc_query

PREPARE pre_des FROM lc_query
EXECUTE pre_des

LET lc_query = ""
LET lc_query = "INSERT INTO safre_tmp:tmp_carga_parcialidades ",
               "SELECT UNIQUE nss, curp, consecutivo, folio, ",
               "       fecha_liquidacion ",
               "FROM ret_parcialidad_des ",
               "WHERE fecha_liquidacion BETWEEN '", ld_fecha_ini, "' AND '", ld_fecha_fin, "' ",
               "AND consec_pago > 1 ",
               "AND estado = ", mr_edo.liquidado
LET lc_query = lc_query CLIPPED
#DISPLAY lc_query

PREPARE pre_parc FROM lc_query
EXECUTE pre_parc

--CALL f_genera_tmp_cuenta(2)

LET lc_query = ""
LET lc_query = "INSERT INTO safre_tmp:tmp_reinv ",
               "SELECT UNIQUE a.nss, a.consecutivo, 0, 0, DATE(b.fecha_carga), SUM(a.monto_en_pesos) ",
               "FROM ret_parcialidad_des a, ",
               "     ret_parcial_op70 b ",
               "WHERE a.nss = b.nss ",
               "AND   a.consecutivo = b.consecutivo ",
               "AND   a.consec_pago > 1 ",
               "AND   a.estado IN (",mr_edo.capturado,",", mr_edo.suspendido , "," , mr_edo.reinv_cajas,") ",
               "AND   DATE(b.fecha_carga) BETWEEN '", ld_fecha_ini, "' AND '", ld_fecha_fin, "' ",
               "AND  (b.resultado_op = '01' OR (b.resultado_op = '02' AND (b.diagnostico = '036' OR b.diagnostico = '049')))",
               "GROUP BY 1,2,3,4,5 "
LET lc_query = lc_query CLIPPED
#DISPLAY lc_query

PREPARE pre_reinv FROM lc_query
EXECUTE pre_reinv

--Se agrega el tipo_movimiento 1343 de reintegro de recursos no cobrados
LET lc_query = ""
LET lc_query = "INSERT INTO safre_tmp:tmp_reinv ",
               "SELECT UNIQUE nss, consecutivo_lote, folio, tipo_movimiento, fecha_conversion, SUM(monto_en_pesos) ",
               "FROM dis_cuenta ",
               "WHERE fecha_conversion BETWEEN '", ld_fecha_ini, "' AND '", ld_fecha_fin, "' ",
               "AND tipo_movimiento = 1343 ",
               "GROUP BY 1,2,3,4,5 "
LET lc_query = lc_query CLIPPED
#DISPLAY lc_query

PREPARE pre_reinv2 FROM lc_query
EXECUTE pre_reinv2

LET lc_query = ""
LET lc_query = "INSERT INTO safre_tmp:tmp_rechazos ",
               "SELECT UNIQUE a.nss, a.consecutivo, a.tipo_desempleo, a.fecha_envio, b.diag_procesar, a.diag_cuenta_ind ",
               "FROM ret_parcial a, ret_parcial_resol b ",
               "WHERE a.nss = b.nss ",
               "AND a.num_resolucion = b.num_resolucion ",
               "AND a.estado_solicitud = ", mr_edo.rechazado,
               "AND a.diag_cuenta_ind IS NOT NULL ",
               "AND a.tipo_prestacion = 6 ",
               "AND a.fecha_envio BETWEEN '", ld_fecha_ini, "' AND '", ld_fecha_fin, "' "
LET lc_query = lc_query CLIPPED
#DISPLAY lc_query

PREPARE pre_rech FROM lc_query
EXECUTE pre_rech

SELECT COUNT(UNIQUE nss)
INTO   li_tot_soli
FROM   safre_tmp:tmp_carga

SELECT COUNT(UNIQUE nss)
INTO   li_tot_par
FROM   safre_tmp:tmp_carga_parcialidades

SELECT COUNT(UNIQUE nss)
INTO   li_tot_reinv
FROM   safre_tmp:tmp_reinv

SELECT COUNT(UNIQUE nss)
INTO   li_tot_rech
FROM   safre_tmp:tmp_rechazos

LET li_total = li_tot_soli + li_tot_par + li_tot_reinv + li_tot_rech

IF li_total <= 0 THEN
   ERROR "NO SE ENCUENTRAN REGISTROS EN EL RANGO DE FECHAS INSERTADO"
   RETURN 1
END IF

   DISPLAY "NSS A EVALUAR DETALLE 1: ", li_tot_soli USING "<<<<<<<<<<" AT 11,13   
   DISPLAY "NSS A EVALUAR DETALLE 2: ", li_tot_par USING "<<<<<<<<<<" AT 12,13
   DISPLAY "NSS A EVALUAR DETALLE 3: ", li_tot_reinv USING "<<<<<<<<<<" AT 13,13
   DISPLAY "NSS A EVALUAR DETALLE 4: ", li_tot_rech USING "<<<<<<<<<<" AT 14,13

   DISPLAY "                               "  AT 18,3
RETURN 0 
END FUNCTION

#################################################################################
FUNCTION f_genera()

DEFINE ls_ejecuta CHAR (500),
       lc_fecha CHAR(08) 
       
LET lc_fecha = mr_rango.fecha_fin USING "DDMMYYYY"

LET ls_ejecuta = "nohup time fglgo RETP857.4gi ",
              mr_rango.fecha_ini USING "DDMMYYYY"," ",
              mr_rango.fecha_fin USING "DDMMYYYY"," ",
              "1> ", "RETP857.salida ",
              "2> ", "RETP857.error &" 

LET ls_ejecuta = ls_ejecuta CLIPPED
RUN ls_ejecuta

LET m_ruta = mr_param.ruta_rescate CLIPPED, "/",m_usuario CLIPPED,".D00_220_1513_",lc_fecha[03,04] CLIPPED, 
             "_",lc_fecha[05,08] CLIPPED,"_568.unl" CLIPPED
  
LET m_ruta = m_ruta CLIPPED

DISPLAY "EL PROCESO SE EJECUTO SATISFACTORIAMENTE POR NOHUP ..."  AT 16,3
DISPLAY "AL FINAL DEL PROCESO SE GENERARA EL ARCHIVO: " AT 17,3 ATTRIBUTE(REVERSE)
DISPLAY m_ruta AT 18,3

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_tmp_cuenta : Genera una tabla temporal con los movimientos       #
#                       historicos de el nss dado                           #
#---------------------------------------------------------------------------#
FUNCTION f_genera_tmp_cuenta(ps_ind)

    DEFINE
        ps_ind                  SMALLINT,
        ls_tpo_mov              CHAR(15),
        lc_cond_sie             CHAR(150),
        lc_query_his            CHAR(5000)  ,
        lc_nombre_tabla         CHAR(0020)
    
    DEFINE
        li_numero       DECIMAL(15,0)

    -- -----------------------------------------------------------------------------

    LET lc_query_his    = " "
    
    IF ps_ind = 1 THEN  #Detalle 1
          LET ls_tpo_mov = "877,876,875"
          LET lc_cond_sie = " AND folio IN (SELECT UNIQUE folio FROM ret_parcial WHERE estado_solicitud IN (",mr_edo.liquidado,",",mr_edo.enviado_op16,",",mr_edo.recibido_op16,"))"
    ELSE
          LET ls_tpo_mov = "930"
          LET lc_cond_sie = " AND siefore NOT IN (10) "
    END IF

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_dis_cuenta_reinv
    WHENEVER ERROR STOP

    DECLARE cur_his CURSOR FOR
        SELECT tabname
        FROM   SYSTABLES
        WHERE  tabname MATCHES "dis_cuenta??"

    FOREACH cur_his INTO lc_nombre_tabla
        LET lc_query_his = lc_query_his CLIPPED ,
                           " SELECT * "                                 ,
                           " FROM ", lc_nombre_tabla                    ,
                           " WHERE fecha_conversion BETWEEN '", mr_rango.fecha_ini, "' AND '", mr_rango.fecha_fin, "' ",
                           " AND   tipo_movimiento IN (", ls_tpo_mov CLIPPED,")",
                           lc_cond_sie CLIPPED,
                           " UNION ALL "
    END FOREACH

    CLOSE cur_his

    LET lc_query_his = lc_query_his CLIPPED,
                       " SELECT * "                                 ,
                       " FROM  dis_cuenta "                        ,
                       " WHERE fecha_conversion BETWEEN '", mr_rango.fecha_ini, "' AND '", mr_rango.fecha_fin, "' ",
                       " AND   tipo_movimiento IN (", ls_tpo_mov CLIPPED,")",
                       lc_cond_sie CLIPPED,
                       " INTO TEMP tmp_dis_cuenta_reinv "
    #DISPLAY lc_query_his

    PREPARE eje_sel_his FROM lc_query_his
    EXECUTE eje_sel_his

    CREATE INDEX tmp_dis_cuenta_reinv1 ON tmp_dis_cuenta_reinv(nss, tipo_movimiento)
    UPDATE STATISTICS FOR TABLE tmp_dis_cuenta_reinv

END FUNCTION