#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC704  => EXTRACCION RETIRO POR DESEMPLEO MENSUAL                   #
#Fecha creacion    => 24-Jul-2016                                               #
#By                => CRISTINA ABASOLO TAPIA                                    #
#Sistema           => RET                                                       #
#################################################################################

DATABASE safre_af

DEFINE mr_rango     RECORD
       fecha_ini      CHAR(08),
       fecha_fin      CHAR(08)
END RECORD

DEFINE mr_elemento RECORD
       id             CHAR(02),
       nss            CHAR(11),
       curp           CHAR(18),
       fecha_envio    DATE,
       tpo_derecho    CHAR(01),
       fec_sol_der    DATE,
       fec_cer_der    DATE,
       fec_pago       DATE,
       tpo_desem      CHAR(01),
       tpo_des_obs    CHAR(02),
       tpo_pago       CHAR(01),
       num_pagos      CHAR(01),
       monto_rcv_ant  DECIMAL(22,6),
       pago_desempleo DECIMAL(22,6),
       fec_liq        DATE,
       primer_pago    DECIMAL(22,6)
END RECORD

DEFINE mr_parcialidad RECORD
       id             CHAR(02),
       nss            CHAR(11),
       curp           CHAR(18),
       fecha_confirma DATE,
       fecha_envio    DATE,
       fecha_recibe   DATE,
       respuesta      CHAR(01),
       fecha_liquida  DATE,
       num_pagos      CHAR(01),
       num_pago_folio CHAR(01),
       monto_pago     DECIMAL(22,6)
END RECORD

DEFINE mr_reinv        RECORD
       id              CHAR(02),
       nss             CHAR(11),
       curp            CHAR(18),
       motivo_reinv    CHAR(1),
       num_no_pag      CHAR(1),
       monto_reinv     DECIMAL(22,6),
       fecha_ult_pago  DATE,
       fecha_reinv     DATE,
       fecha_reintegro DATE
END RECORD

DEFINE mr_rechazos     RECORD
       id              CHAR(02),
       nss             CHAR(11),
       curp            CHAR(18),
       fecha_envio     DATE,
       tipo_derecho    CHAR(1),
       diag_resol      CHAR(03),
       diag_retiro     CHAR(03)
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

LET mr_rango.fecha_ini  = ARG_VAL(1)
LET mr_rango.fecha_fin  = ARG_VAL(2)

DISPLAY "Inicia la extraccion de informacion del rande de fechas: "
DISPLAY mr_rango.fecha_ini, "-", mr_rango.fecha_fin

CALL f_lib_abre_log("RETP857")
CALL init()
CALL f_genera()

END MAIN

#################################################################################
FUNCTION init()

   --Parametros globales y usuario que ejecuta
   SELECT a.*,USER 
   INTO mr_param.*, m_usuario
   FROM safre_af:seg_modulo a
   WHERE a.modulo_cod = "ret"
      
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
FUNCTION f_genera()

DEFINE lc_ejecuta CHAR(5000)

  LET m_cuantos = 0
  
  LET m_ruta = mr_param.ruta_listados CLIPPED, "/",m_usuario CLIPPED,".DETALLE1.",
               mr_rango.fecha_fin CLIPPED
  
  LET m_ruta = m_ruta CLIPPED
  START REPORT r_detalle_1 TO m_ruta

   CALL f_genera_detalle_1()
   
  FINISH REPORT r_detalle_1
  
  LET m_ruta = ""
  LET m_ruta = mr_param.ruta_listados CLIPPED, "/",m_usuario CLIPPED,".DETALLE2.",
               mr_rango.fecha_fin CLIPPED
  
  LET m_ruta = m_ruta CLIPPED
  START REPORT r_detalle_2 TO m_ruta

   CALL f_genera_detalle_2()
   
  FINISH REPORT r_detalle_2
  
  LET m_ruta = ""
  LET m_ruta = mr_param.ruta_listados CLIPPED, "/",m_usuario CLIPPED,".DETALLE3.",
               mr_rango.fecha_fin CLIPPED
  
  LET m_ruta = m_ruta CLIPPED
  START REPORT r_detalle_3 TO m_ruta

   CALL f_genera_detalle_3()
   
  FINISH REPORT r_detalle_3
  
  LET m_ruta = ""
  LET m_ruta = mr_param.ruta_listados CLIPPED, "/",m_usuario CLIPPED,".DETALLE4.",
               mr_rango.fecha_fin CLIPPED
  
  LET m_ruta = m_ruta CLIPPED
  START REPORT r_detalle_4 TO m_ruta

   CALL f_genera_detalle_4()
   
  FINISH REPORT r_detalle_4
  

LET lc_ejecuta = "cat ",mr_param.ruta_listados CLIPPED, "/",m_usuario CLIPPED,".DETALLE1.",mr_rango.fecha_fin CLIPPED, " ",
                 mr_param.ruta_listados CLIPPED, "/",m_usuario CLIPPED,".DETALLE2.",mr_rango.fecha_fin CLIPPED, " ",
                 mr_param.ruta_listados CLIPPED, "/",m_usuario CLIPPED,".DETALLE3.",mr_rango.fecha_fin CLIPPED, " ",
                 mr_param.ruta_listados CLIPPED, "/",m_usuario CLIPPED,".DETALLE4.",mr_rango.fecha_fin CLIPPED, " >",
                 mr_param.ruta_rescate CLIPPED, "/",m_usuario CLIPPED,".D00_220_1513_",mr_rango.fecha_fin[03,04], 
                 "_",mr_rango.fecha_fin[05,08],"_568.unl" CLIPPED

RUN lc_ejecuta

DISPLAY "ARCHIVO GENERADO " AT 17,3 ATTRIBUTE(REVERSE)

ERROR "FINALIZA PROCESO" SLEEP 3
ERROR " "

END FUNCTION

######################################################################################################
FUNCTION f_ini_record (pr_elemento)

DEFINE pr_elemento RECORD
       id             CHAR(02),
       nss            CHAR(11),
       curp           CHAR(18),
       fecha_envio    DATE,
       tpo_derecho    CHAR(01),
       fec_sol_der    DATE,
       fec_cer_der    DATE,
       fec_pago       DATE,
       tpo_desem      CHAR(01),
       tpo_des_obs    CHAR(02),
       tpo_pago       CHAR(01),
       num_pagos      CHAR(01),
       monto_rcv_ant  DECIMAL(22,6),
       pago_desempleo DECIMAL(22,6),
       fec_liq        DATE,
       primer_pago    DECIMAL(22,6)
END RECORD

INITIALIZE pr_elemento.* TO NULL

LET pr_elemento.id             = "01"
LET pr_elemento.nss            = ""
LET pr_elemento.curp           = ""
LET pr_elemento.fecha_envio    = ""
LET pr_elemento.tpo_derecho    = ""
LET pr_elemento.fec_sol_der    = ""
LET pr_elemento.fec_cer_der    = ""
LET pr_elemento.fec_pago       = ""
LET pr_elemento.tpo_desem      = ""
LET pr_elemento.tpo_des_obs    = "  "
LET pr_elemento.tpo_pago       = ""
LET pr_elemento.num_pagos      = ""
LET pr_elemento.monto_rcv_ant  = 0
LET pr_elemento.pago_desempleo = 0
LET pr_elemento.fec_liq        = ""
LET pr_elemento.primer_pago    = 0

RETURN pr_elemento.*
       
END FUNCTION

######################################################################################################
FUNCTION f_ini_record_parc (pr_parcialidades)

DEFINE pr_parcialidades RECORD
       id             CHAR (02),
       nss            CHAR(11),
       curp           CHAR(18),
       fecha_confirma DATE,
       fecha_envio    DATE,
       fecha_recibe   DATE,
       respuesta      CHAR(1),
       fecha_liquida  DATE,
       num_pagos      CHAR(1),
       num_pago_folio CHAR(1),
       monto_pago     DECIMAL(22,6)
END RECORD

INITIALIZE pr_parcialidades.* TO NULL

LET pr_parcialidades.id             = "02"
LET pr_parcialidades.nss            = ""
LET pr_parcialidades.curp           = ""
LET pr_parcialidades.fecha_confirma = NULL
LET pr_parcialidades.fecha_envio    = NULL
LET pr_parcialidades.fecha_recibe   = NULL
LET pr_parcialidades.respuesta      = ""
LET pr_parcialidades.fecha_liquida  = NULL
LET pr_parcialidades.num_pagos      = ""
LET pr_parcialidades.num_pago_folio = ""
LET pr_parcialidades.monto_pago     = 0

RETURN pr_parcialidades.*
       
END FUNCTION

######################################################################################################
FUNCTION f_ini_record_reinv (pr_reinv)

DEFINE pr_reinv        RECORD
       id              CHAR(02),
       nss             CHAR(11),
       curp            CHAR(18),
       motivo_reinv    CHAR(1),
       num_no_pag      CHAR(1),
       monto_reinv     DECIMAL(22,6),
       fecha_ult_pago  DATE,
       fecha_reinv     DATE,
       fecha_reintegro DATE
END RECORD

INITIALIZE pr_reinv.* TO NULL

LET pr_reinv.id              = "03"
LET pr_reinv.nss             = ""
LET pr_reinv.curp            = ""
LET pr_reinv.motivo_reinv    = ""
LET pr_reinv.num_no_pag      = ""
LET pr_reinv.monto_reinv     = 0
LET pr_reinv.fecha_ult_pago  = NULL
LET pr_reinv.fecha_reinv     = NULL
LET pr_reinv.fecha_reintegro = NULL

RETURN pr_reinv.*
       
END FUNCTION

######################################################################################################
FUNCTION f_ini_record_rech (pr_rechazos)

DEFINE pr_rechazos    RECORD
       id              CHAR(02),
       nss             CHAR(11),
       curp            CHAR(18),
       fecha_envio     DATE,
       tipo_derecho    CHAR(1),
       diag_resol      CHAR(03),
       diag_retiro     CHAR(03)
END RECORD

INITIALIZE pr_rechazos.* TO NULL

LET pr_rechazos.id           = "04"
LET pr_rechazos.nss          = ""
LET pr_rechazos.curp         = ""
LET pr_rechazos.fecha_envio  = NULL
LET pr_rechazos.tipo_derecho = ""
LET pr_rechazos.diag_resol   = ""
LET pr_rechazos.diag_retiro  = ""

RETURN pr_rechazos.*
       
END FUNCTION


#################################################################################
FUNCTION f_genera_detalle_1()

DEFINE lr_elementos  RECORD
       nss                   CHAR(11),
       consecutivo           INTEGER,
       folio                 DECIMAL(11,0),
       tipo_desempleo        CHAR(1),
       tipo_pago             SMALLINT,
       pago_desempleo        DECIMAL(22,6),
       num_resolucion        INTEGER,
       fecha                 DATE
END RECORD

DEFINE lr_resolucion RECORD
       fecha_fall_mat_des DATE,
       salario_base_a     DECIMAL(22,6),
       salario_base_b     DECIMAL(22,6)
END RECORD

DEFINE lr_op66 RECORD
       clave_mensaje_procesar_a SMALLINT,
       clave_mensaje_procesar_b SMALLINT
END RECORD

DEFINE li_numero   INTEGER,
       ld_monto    DECIMAL(22,6),
       ld_precio   DECIMAL(22,6),
       ld_calculo  DECIMAL(22,6)

DEFINE lc_nom_tabla    CHAR(12),
       ls_year         SMALLINT,
       lc_year         CHAR(04),
       lc_cadena       CHAR(1500)

LET li_numero = 0

DISPLAY "Inicia generacion Detalle 1 ..."

DECLARE cur_nss CURSOR FOR
SELECT * FROM safre_tmp:tmp_carga
ORDER BY fecha
   
FOREACH cur_nss INTO lr_elementos.*

   #DISPLAY "NSS a evaluar: ",lr_elementos.nss
   INITIALIZE mr_elemento.* TO NULL
   CALL f_ini_record(mr_elemento.*) RETURNING mr_elemento.*;

    LET mr_elemento.id = "01"
    LET mr_elemento.nss = lr_elementos.nss
    
    SELECT UNIQUE n_unico
    INTO mr_elemento.curp
    FROM afi_mae_afiliado
    WHERE n_seguro = lr_elementos.nss
    
    LET mr_elemento.fecha_envio = lr_elementos.fecha
            
    SELECT UNIQUE fecha_fall_mat_des, salario_base_a, salario_base_b
    INTO lr_resolucion.*
    FROM ret_parcial_resol
    WHERE nss = lr_elementos.nss
    AND num_resolucion = lr_elementos.num_resolucion
    AND diag_procesar = 400
    AND fecha_carga_afore = (SELECT MAX(fecha_carga_afore) FROM ret_parcial_resol
                             WHERE nss = lr_elementos.nss
                             AND num_resolucion = lr_elementos.num_resolucion
                             AND diag_procesar = 400)

    IF lr_elementos.fecha >= "06/29/2015" THEN
       LET mr_elemento.fec_sol_der = lr_resolucion.fecha_fall_mat_des
    ELSE
       LET mr_elemento.fec_sol_der = ""
    END IF

    LET mr_elemento.fec_cer_der = lr_resolucion.fecha_fall_mat_des
    
    IF lr_elementos.tipo_desempleo = "A" THEN
       LET mr_elemento.tpo_derecho = "0"
       LET mr_elemento.tpo_pago = "1"
       LET mr_elemento.num_pagos = "1"
    ELSE
       LET mr_elemento.tpo_derecho = "1"

       IF lr_elementos.tipo_pago = "5" THEN
          LET mr_elemento.num_pagos = "2"
       ELSE
          IF lr_elementos.tipo_pago = "4" OR lr_elementos.tipo_pago = "6" THEN
             LET mr_elemento.num_pagos = "6"
          ELSE
             LET mr_elemento.num_pagos = "1"
          END IF
       END IF
       
       LET ld_monto = lr_resolucion.salario_base_b * 90
       
       IF ld_monto = lr_elementos.pago_desempleo THEN
          LET mr_elemento.tpo_pago = "3"
       ELSE
          LET mr_elemento.tpo_pago = "2"
       END IF
    END IF

    
    SELECT UNIQUE cve_msj_procesara, cve_msj_procesarb
    INTO lr_op66.*
    FROM ret_parcial_op66
    WHERE nss = lr_elementos.nss
    AND num_resolucion = lr_elementos.num_resolucion
    AND diag_procesar = "400"
    AND tipo_prestacion = "06"
    AND fecha_fall_mat_des = (SELECT MAX(fecha_fall_mat_des)
                                      FROM ret_parcial_op66
                                      WHERE nss = lr_elementos.nss
                                      AND num_resolucion = lr_elementos.num_resolucion
                                      AND diag_procesar = "400"
                                      AND tipo_prestacion = "06")
    
    IF lr_op66.clave_mensaje_procesar_a = "21" AND lr_op66.clave_mensaje_procesar_b = "22" THEN
       LET mr_elemento.tpo_desem = "2"
       LET mr_elemento.tpo_des_obs = "  "
    ELSE
       IF lr_op66.clave_mensaje_procesar_a = "21" AND lr_op66.clave_mensaje_procesar_b = "13" THEN
          LET mr_elemento.tpo_desem = "3"
          LET mr_elemento.tpo_des_obs = " 1"
       ELSE
           IF lr_op66.clave_mensaje_procesar_a = "12" AND lr_op66.clave_mensaje_procesar_b = "22" THEN
              LET mr_elemento.tpo_desem = "4"
              LET mr_elemento.tpo_des_obs = " 2"
           END IF
       END IF
    END IF
    
    CALL f_genera_tmp_cuenta(lr_elementos.nss)
    
    SELECT UNIQUE SUM(monto_en_pesos), precio_accion, fecha_conversion 
    INTO mr_elemento.primer_pago, ld_precio, mr_elemento.fec_liq
    FROM tmp_dis_cuenta_nss
    WHERE nss = lr_elementos.nss 
    AND folio = lr_elementos.folio
    AND consecutivo_lote = lr_elementos.consecutivo
    AND tipo_movimiento IN (877,876)
    GROUP BY 2,3
    
    IF mr_elemento.primer_pago IS NOT NULL THEN
       LET mr_elemento.primer_pago = mr_elemento.primer_pago * -1
    ELSE
       LET mr_elemento.primer_pago = 0
    END IF
    
    LET mr_elemento.fec_pago = mr_elemento.fec_liq
    
    {LET lc_nom_tabla = " "
    LET ls_year = YEAR(mr_elemento.fec_liq) 
    LET lc_year = ls_year
    LET lc_nom_tabla = "dis_cuenta", lc_year[3,4]
    
    LET lc_cadena = " "
    LET lc_cadena = "SELECT tabname FROM SYSTABLES \n ",
                   " WHERE  tabname = '", lc_nom_tabla CLIPPED, "'"
    LET lc_cadena = lc_cadena CLIPPED

    PREPARE pre_reg FROM lc_cadena
    EXECUTE pre_reg
    
    IF SQLCA.SQLCODE = 0 THEN   #existe
       LET lc_cadena = " "
       LET lc_cadena = " SELECT SUM(monto_en_acciones)                         \n ",
                       " FROM   ",lc_nom_tabla CLIPPED,"                       \n ",
                       " WHERE  nss              =  '",lr_elementos.nss,"'     \n ",
                       " AND    folio            <> ",lr_elementos.folio,"     \n ",
                       " AND    fecha_conversion <= '",mr_elemento.fec_liq,"'  \n ",
                       " AND    subcuenta IN (1,2,5,6,9)" 
            
                           
       LET lc_cadena = lc_cadena CLIPPED
               
       PREPARE pre_monto_ant_ret_ant2 FROM lc_cadena
       EXECUTE pre_monto_ant_ret_ant2 INTO ld_calculo  
    ELSE
        SELECT SUM(monto_en_acciones)
        INTO ld_calculo
        FROM dis_cuenta
        WHERE nss = lr_elementos.nss
        AND folio <> lr_elementos.folio
        AND fecha_conversion <= mr_elemento.fec_liq
        AND subcuenta IN (1,2,5,6,9)
    END IF
    
    LET mr_elemento.monto_rcv_ant = ld_calculo * ld_precio
    }
    
    SELECT saldocuentaindividualantesretiro
    INTO mr_elemento.monto_rcv_ant
    FROM ret_bus_diag16
    WHERE nss = lr_elementos.nss
    AND folio = lr_elementos.folio

    SELECT SUM(monto_en_pesos)
    INTO mr_elemento.pago_desempleo
    FROM tmp_dis_cuenta_nss 
    WHERE nss = lr_elementos.nss 
    AND folio = lr_elementos.folio
    AND consecutivo_lote = lr_elementos.consecutivo
    AND tipo_movimiento IN (877,876,927)
   
    LET mr_elemento.pago_desempleo = mr_elemento.pago_desempleo * -1
    
    OUTPUT TO REPORT  r_detalle_1(mr_elemento.*)
    
    CALL f_ini_record(mr_elemento.*) RETURNING mr_elemento.*;
      
END FOREACH

END FUNCTION

#################################################################################
FUNCTION f_genera_detalle_2()

DEFINE lr_elementos  RECORD
nss                   CHAR(11),
curp                  CHAR(18),
consecutivo           INTEGER,
folio                 DECIMAL(11,0),
fecha                 DATE
END RECORD

DEFINE 
    fecha_confirma  ,
    fecha_envio     ,
    fecha_recibe    DATE

DEFINE monto        DECIMAL(22,6),
       fecha_liq    DATE

DEFINE li_numero   INTEGER,
       ld_monto    DECIMAL(11,6),
       ld_precio   DECIMAL(11,6),
       ld_calculo  DECIMAL(22,6)

DEFINE lc_fec_ini  CHAR(10),
       lc_fec_fin  CHAR(10),
       ld_inicio   DATE,
       ld_fin      DATE

LET li_numero = 0

DISPLAY "Inicia generacion Detalle 2 ..."

LET lc_fec_ini = mr_rango.fecha_ini[03,04],"/",mr_rango.fecha_ini[01,02],"/",mr_rango.fecha_ini[05,08]
LET lc_fec_fin = mr_rango.fecha_fin[03,04],"/",mr_rango.fecha_fin[01,02],"/",mr_rango.fecha_fin[05,08]
LET ld_inicio = lc_fec_ini
LET ld_fin = lc_fec_fin

DECLARE cur_nss_par CURSOR FOR
SELECT * FROM safre_tmp:tmp_carga_parcialidades
ORDER BY fecha
   
FOREACH cur_nss_par INTO lr_elementos.*
   
   #DISPLAY "NSS a evaluar: ",lr_elementos.nss
   INITIALIZE mr_parcialidad.* TO NULL
   CALL f_ini_record_parc(mr_parcialidad.*) RETURNING mr_parcialidad.*;

    LET mr_parcialidad.id = "02"
    LET mr_parcialidad.nss = lr_elementos.nss
    LET mr_parcialidad.curp = lr_elementos.curp
    LET mr_parcialidad.respuesta = "1"
    
    CALL f_gen_tmp_his_parc(lr_elementos.nss)
    
    {SELECT MAX(DATE(fecha_ins))
    INTO mr_parcialidad.fecha_confirma
    FROM tmp_his_parc_nss 
    WHERE nss = lr_elementos.nss
    AND consecutivo = lr_elementos.consecutivo
    AND estado = mr_edo.confirmado 
    AND consec_pago IN (SELECT consec_pago
                        FROM tmp_his_parc_nss
                        WHERE nss = lr_elementos.nss
                        AND consecutivo = lr_elementos.consecutivo
                        AND folio = lr_elementos.folio)}
                        
    SELECT MAX(DATE(fecha_ins))
    INTO mr_parcialidad.fecha_envio
    FROM tmp_his_parc_nss
    WHERE nss = lr_elementos.nss 
    AND consecutivo = lr_elementos.consecutivo
    AND estado = mr_edo.enviado_op67
    AND consec_pago IN (SELECT consec_pago
                        FROM tmp_his_parc_nss
                        WHERE nss = lr_elementos.nss
                        AND consecutivo = lr_elementos.consecutivo
                        AND folio = lr_elementos.folio)
                        
    LET mr_parcialidad.fecha_confirma = mr_parcialidad.fecha_envio
                        
    SELECT MAX(DATE(fecha_ins))
    INTO mr_parcialidad.fecha_recibe
    FROM tmp_his_parc_nss
    WHERE nss = lr_elementos.nss 
    AND consecutivo = lr_elementos.consecutivo
    AND estado = mr_edo.recibido_op68
    AND consec_pago IN (SELECT consec_pago
                        FROM tmp_his_parc_nss
                        WHERE nss = lr_elementos.nss
                        AND consecutivo = lr_elementos.consecutivo
                        AND folio = lr_elementos.folio)
     
    #SELECT COUNT(*) 
    SELECT MAX(consec_pago)
    INTO mr_parcialidad.num_pagos
    FROM ret_parcialidad_des
    WHERE nss = lr_elementos.nss
    AND folio = lr_elementos.folio
    AND consecutivo = lr_elementos.consecutivo
    AND estado = mr_edo.liquidado
    
    CALL f_genera_tmp_cuenta(lr_elementos.nss)
    
    SELECT UNIQUE SUM(monto_en_pesos), fecha_conversion 
    INTO mr_parcialidad.monto_pago, mr_parcialidad.fecha_liquida
    FROM tmp_dis_cuenta_nss
    WHERE nss = lr_elementos.nss 
    AND folio = lr_elementos.folio
    AND tipo_movimiento IN (877)
    GROUP BY 2
    
    SELECT COUNT(*) 
    INTO mr_parcialidad.num_pago_folio
    FROM ret_parcialidad_des
    WHERE nss = lr_elementos.nss
    AND consecutivo = lr_elementos.consecutivo
    #AND folio = lr_elementos.folio
    #AND fecha_liquidacion = lr_elementos.fecha
    AND fecha_liquidacion >= ld_inicio
    AND fecha_liquidacion <= ld_fin
    
    AND estado = mr_edo.liquidado
   
    LET mr_parcialidad.monto_pago = mr_parcialidad.monto_pago * -1
    
    OUTPUT TO REPORT  r_detalle_2(mr_parcialidad.*)
    
    CALL f_ini_record_parc(mr_parcialidad.*) RETURNING mr_parcialidad.*;
      
END FOREACH

END FUNCTION

#################################################################################
FUNCTION f_genera_detalle_3()

DEFINE lr_elementos  RECORD
    nss                   CHAR(11),
    consecutivo           INTEGER,
    folio                 DECIMAL(11,0),
    tip_mov               SMALLINT,
    fecha                 DATE,
    monto                 DECIMAL(22,6)
END RECORD

DISPLAY "Inicia generacion Detalle 3 ..."

DECLARE cur_nss_reinv CURSOR FOR
SELECT * FROM safre_tmp:tmp_reinv
ORDER BY fecha, nss
   
FOREACH cur_nss_reinv INTO lr_elementos.*
   
   #DISPLAY "NSS a evaluar: ",lr_elementos.nss
   INITIALIZE mr_reinv.* TO NULL
   CALL f_ini_record_reinv(mr_reinv.*) RETURNING mr_reinv.*;
   
   LET mr_reinv.id = "03"
   LET mr_reinv.nss = lr_elementos.nss
   
   SELECT UNIQUE n_unico
   INTO mr_reinv.curp
   FROM afi_mae_afiliado
   WHERE n_seguro = lr_elementos.nss
   
   IF lr_elementos.tip_mov = 1343 THEN
       LET mr_reinv.motivo_reinv    = 1
       LET mr_reinv.num_no_pag      = 0
       LET mr_reinv.fecha_reintegro = lr_elementos.fecha
   ELSE
       SELECT ind_suspende
       INTO mr_reinv.motivo_reinv
       FROM ret_parcial_sub
       WHERE nss = lr_elementos.nss
       AND consecutivo = lr_elementos.consecutivo
       
       IF mr_reinv.motivo_reinv IS NULL OR mr_reinv.motivo_reinv = "" THEN
           LET mr_reinv.motivo_reinv = "2"
       END IF
       
       SELECT COUNT(*) 
       INTO mr_reinv.num_no_pag
       FROM ret_parcialidad_des
       WHERE nss = lr_elementos.nss
       AND consecutivo = lr_elementos.consecutivo
       AND estado IN (mr_edo.capturado, mr_edo.suspendido, mr_edo.reinv_cajas)

       SELECT MAX(fecha_reintegro)
       INTO mr_reinv.fecha_reintegro
       FROM ret_parcial_op70
       WHERE nss = lr_elementos.nss 
       AND consecutivo = lr_elementos.consecutivo
       AND (resultado_op = '01' OR (resultado_op = '02' AND (diagnostico = '036' OR diagnostico = '049')))
   END IF
    
   LET mr_reinv.monto_reinv = lr_elementos.monto
   LET mr_reinv.fecha_reinv = lr_elementos.fecha
   
   SELECT MAX(fecha_liquidacion)
   INTO mr_reinv.fecha_ult_pago
   FROM ret_parcialidad_des 
   WHERE nss = lr_elementos.nss 
   AND consecutivo = lr_elementos.consecutivo 
   AND estado = mr_edo.liquidado

   OUTPUT TO REPORT  r_detalle_3(mr_reinv.*)
       
   CALL f_ini_record_reinv(mr_reinv.*) RETURNING mr_reinv.*;
      
END FOREACH

END FUNCTION

#################################################################################
FUNCTION f_genera_detalle_4()

DEFINE lr_elementos  RECORD
    nss                   CHAR(11),
    consecutivo           INTEGER,
    tipo_desempleo        CHAR(1),
    fecha                 DATE,
    diag_resol            CHAR(03),
    diag_retiro           CHAR(03)
END RECORD

DISPLAY "Inicia generacion Detalle 4 ..."

DECLARE cur_nss_rech CURSOR FOR
SELECT * FROM safre_tmp:tmp_rechazos
ORDER BY fecha
   
FOREACH cur_nss_rech INTO lr_elementos.*
   
   #DISPLAY "NSS a evaluar: ",lr_elementos.nss
   INITIALIZE mr_rechazos.* TO NULL
   CALL f_ini_record_rech(mr_rechazos.*) RETURNING mr_rechazos.*;
   
   LET mr_rechazos.id           = "04"
   LET mr_rechazos.nss          = lr_elementos.nss
   
   SELECT UNIQUE n_unico
   INTO mr_rechazos.curp
   FROM afi_mae_afiliado
   WHERE n_seguro = lr_elementos.nss
   
   LET mr_rechazos.fecha_envio  = lr_elementos.fecha
   
   IF lr_elementos.tipo_desempleo = "A" THEN
       LET mr_rechazos.tipo_derecho = "0"
    ELSE
       LET mr_rechazos.tipo_derecho = "1"
    END IF
   
   LET mr_rechazos.diag_resol   = lr_elementos.diag_resol
   LET mr_rechazos.diag_retiro  = lr_elementos.diag_retiro

   OUTPUT TO REPORT  r_detalle_4(mr_rechazos.*)
       
   CALL f_ini_record_rech(mr_rechazos.*) RETURNING mr_rechazos.*;
      
END FOREACH

END FUNCTION

###############################################################################################
REPORT r_detalle_1(lr_elemento)

DEFINE lr_elemento RECORD
       id             CHAR(02),
       nss            CHAR(11),
       curp           CHAR(18),
       fecha_envio    DATE,
       tpo_derecho    CHAR(01),
       fec_sol_der    DATE,
       fec_cer_der    DATE,
       fec_pago       DATE,
       tpo_desem      CHAR(01),
       tpo_des_obs    CHAR(02),
       tpo_pago       CHAR(01),
       num_pagos      CHAR(01),	
       monto_rcv_ant  DECIMAL(22,6),
       pago_desempleo DECIMAL(22,6),
       fec_liq        DATE,
       primer_pago    DECIMAL(22,6)
END RECORD

DEFINE lc_monto_rcv   CHAR(14),
       lc_pago        CHAR(14),
       lc_primer_pago CHAR(14)

   OUTPUT
            PAGE      LENGTH   1
            LEFT      MARGIN   0
            RIGHT     MARGIN   0
            TOP       MARGIN   0
            BOTTOM    MARGIN   0
   FORMAT

          ON EVERY ROW
            
            LET lc_monto_rcv = lr_elemento.monto_rcv_ant  USING "&&&&&&&&&&&.&&"
            LET lc_pago = lr_elemento.pago_desempleo USING "&&&&&&&&&&&.&&"
            LET lc_primer_pago = lr_elemento.primer_pago    USING "&&&&&&&&&&&.&&"

            PRINT COLUMN 1,
                 lr_elemento.id             ,                                  ",",
                 lr_elemento.nss            ,                                  ",",
                 lr_elemento.curp           ,                                  ",",
                 lr_elemento.fecha_envio    USING "YYYYMMDD",                  ",",
                 lr_elemento.tpo_derecho    CLIPPED,                           ",",
                 lr_elemento.fec_sol_der    USING "YYYYMMDD",                  ",",
                 lr_elemento.fec_cer_der    USING "YYYYMMDD",                  ",",
                 lr_elemento.fec_pago       USING "YYYYMMDD",                  ",",
                 lr_elemento.tpo_desem      CLIPPED,                           ",",
                 lr_elemento.tpo_des_obs    ,                           ",",
                 lr_elemento.tpo_pago       CLIPPED,                           ",",
                 lr_elemento.num_pagos      CLIPPED,                           ",", 
                 lc_monto_rcv[01,11]   CLIPPED, lc_monto_rcv[13,14]   CLIPPED, ",",
                 lc_pago[01,11]        CLIPPED, lc_pago[13,14]        CLIPPED, ",",
                 lr_elemento.fec_liq        USING "YYYYMMDD",                   ",",
                 lc_primer_pago[01,11] CLIPPED, lc_primer_pago[13,14] CLIPPED

END REPORT

###############################################################################################
REPORT r_detalle_2(lr_elemento)
DEFINE lr_elemento RECORD
       id             CHAR (02),
       nss            CHAR(11),
       curp           CHAR(18),
       fecha_confirma DATE,
       fecha_envio    DATE,
       fecha_recibe   DATE,
       respuesta      CHAR(1),
       fecha_liquida  DATE,
       num_pagos      CHAR(1),
       num_pago_folio CHAR(1),
       monto_pago     DECIMAL(22,6)
END RECORD

DEFINE lc_pago        CHAR(14)

   OUTPUT
            PAGE      LENGTH   1
            LEFT      MARGIN   0
            RIGHT     MARGIN   0
            TOP       MARGIN   0
            BOTTOM    MARGIN   0
   FORMAT

          ON EVERY ROW
            
            LET lc_pago = lr_elemento.monto_pago USING "&&&&&&&&&&&.&&"

            PRINT COLUMN 1,
                 lr_elemento.id              ,                      ",",
                 lr_elemento.nss             ,                      ",",
                 lr_elemento.curp            ,                      ",",
                 lr_elemento.fecha_confirma  USING "YYYYMMDD",      ",",
                 lr_elemento.fecha_envio     USING "YYYYMMDD",      ",",
                 lr_elemento.fecha_recibe    USING "YYYYMMDD",      ",",
                 lr_elemento.respuesta       CLIPPED,               ",",
                 lr_elemento.fecha_liquida   USING "YYYYMMDD",      ",",
                 lr_elemento.num_pagos       CLIPPED,               ",",
                 lr_elemento.num_pago_folio  CLIPPED,               ",", 
                 lc_pago[01,11] CLIPPED, lc_pago[13,14] CLIPPED

END REPORT

###############################################################################################
REPORT r_detalle_3(lr_elemento)
DEFINE lr_elemento RECORD
       id              CHAR(02),
       nss             CHAR(11),
       curp            CHAR(18),
       motivo_reinv    CHAR(1),
       num_no_pag      CHAR(1),
       monto_reinv     DECIMAL(22,6),
       fecha_ult_pago  DATE,
       fecha_reinv     DATE,
       fecha_reintegro DATE
END RECORD

DEFINE lc_pago        CHAR(14)

   OUTPUT
            PAGE      LENGTH   1
            LEFT      MARGIN   0
            RIGHT     MARGIN   0
            TOP       MARGIN   0
            BOTTOM    MARGIN   0
   FORMAT

          ON EVERY ROW

            LET lc_pago = lr_elemento.monto_reinv USING "&&&&&&&&&&&.&&"

            PRINT COLUMN 1,
                 lr_elemento.id               ,                      ",",
                 lr_elemento.nss              ,                      ",",
                 lr_elemento.curp             ,                      ",",
                 lr_elemento.motivo_reinv     ,                      ",",
                 lr_elemento.num_no_pag       ,                      ",",
                 lc_pago[01,11] CLIPPED, lc_pago[13,14] CLIPPED,     ",",
                 lr_elemento.fecha_ult_pago   USING "YYYYMMDD",      ",",
                 lr_elemento.fecha_reinv      USING "YYYYMMDD",      ",",
                 lr_elemento.fecha_reintegro  USING "YYYYMMDD"

END REPORT

###############################################################################################
REPORT r_detalle_4(lr_elemento)
DEFINE lr_elemento RECORD
       id              CHAR(02),
       nss             CHAR(11),
       curp            CHAR(18),
       fecha_envio     DATE,
       tipo_derecho    CHAR(1),
       diag_resol      CHAR(03),
       diag_retiro     CHAR(03)
END RECORD

   OUTPUT
            PAGE      LENGTH   1
            LEFT      MARGIN   0
            RIGHT     MARGIN   0
            TOP       MARGIN   0
            BOTTOM    MARGIN   0
   FORMAT

          ON EVERY ROW

            PRINT COLUMN 1,
                 lr_elemento.id              ,                      ",",
                 lr_elemento.nss             ,                      ",",
                 lr_elemento.curp            ,                      ",",
                 lr_elemento.fecha_envio     USING "YYYYMMDD",      ",",
                 lr_elemento.tipo_derecho    CLIPPED,               ",",
                 lr_elemento.diag_resol      CLIPPED,               ",",
                 lr_elemento.diag_retiro     CLIPPED
                 
END REPORT


#---------------------------------------------------------------------------#
# f_genera_tmp_cuenta : Genera una tabla temporal con los movimientos       #
#                       historicos de el nss dado                           #
#---------------------------------------------------------------------------#
FUNCTION f_genera_tmp_cuenta(pc_nss)

    DEFINE
        pc_nss                  CHAR(0011)

    DEFINE
        lc_query_his            CHAR(3000)  ,
        lc_nombre_tabla         CHAR(0020)
    
    DEFINE
        li_numero       DECIMAL(15,0)

    -- -----------------------------------------------------------------------------

    LET lc_query_his    = " "

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_dis_cuenta_nss
    WHENEVER ERROR STOP

    DECLARE cur_his CURSOR FOR
        SELECT tabname
        FROM   SYSTABLES
        WHERE  tabname MATCHES "dis_cuenta??"

    FOREACH cur_his INTO lc_nombre_tabla
        LET lc_query_his = lc_query_his CLIPPED ,
                           " SELECT * "                                 ,
                           " FROM ", lc_nombre_tabla                    ,
                           " WHERE nss = ", "'", pc_nss, "'"            ,
                           " AND   tipo_movimiento IN (876,877,927,928,929,930) ",
                           " UNION ALL "
    END FOREACH

    CLOSE cur_his

    LET lc_query_his = lc_query_his CLIPPED,
                       " SELECT * "                                 ,
                       " FROM   dis_cuenta "                        ,
                       " WHERE  nss = ", "'", pc_nss, "'"           ,
                       " AND   tipo_movimiento IN (876,877,927,928,929,930) ",
                       " INTO TEMP tmp_dis_cuenta_nss "

    PREPARE eje_sel_his FROM lc_query_his
    EXECUTE eje_sel_his

    CREATE INDEX tmp_dis_cuenta_nss1 ON tmp_dis_cuenta_nss(nss, tipo_movimiento)
    UPDATE STATISTICS FOR TABLE tmp_dis_cuenta_nss

END FUNCTION

######################################################################################################
FUNCTION f_gen_tmp_his_parc(pc_nss)

    DEFINE
        pc_nss                  CHAR(0011)

    DEFINE
        lc_query_his            CHAR(2000)  
    -- -----------------------------------------------------------------------------

    LET lc_query_his    = " "

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_his_parc_nss
    WHENEVER ERROR STOP
    
    LET lc_query_his = " SELECT * "                                 ,
                       " FROM   ret_parcialidad_des_trg "           ,
                       " WHERE  nss = '", pc_nss, "'"           ,
                       " INTO TEMP tmp_his_parc_nss "

    PREPARE eje_sel_his_par FROM lc_query_his
    EXECUTE eje_sel_his_par

    UPDATE STATISTICS FOR TABLE tmp_his_parc_nss

END FUNCTION


