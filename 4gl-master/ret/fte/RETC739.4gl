#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC739  => Pantalla de ejecucion de preliquidacion Derecho Otorgado  #
#Fecha creacion    => 09 DE  Diciembre DE 2011                                  #
#By                => ALEJANDRO CHAGOYA SALAZAR                                 #
#Fecha actualiz.   => 3 DE MAYO DE 2012                                         #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                     Adecuaciones para incluir las modificaciones a la Ley del #
#                     INFONAVIT 73                                              #
#Sistema           => ret                                                       #
#################################################################################
DATABASE safre_af
DEFINE 
   m_usuario     VARCHAR(10),
   m_hoy         DATE,
   m_hora        CHAR(8),
   mr_param      RECORD LIKE safre_af:seg_modulo.*,
   m_enter       CHAR(1),
   m_cuantos,
   m_log         VARCHAR(30),
   m_char        CHAR(500),
   m_folio       INTEGER,
   
   gs_codigo_afore,
   gs_invercap    ,
   gs_mov_viv97  SMALLINT,
   
   mr_edo RECORD
        rechazado   ,
        preliquidado,
        provisionado    LIKE ret_estado.estado_solicitud
   END RECORD

MAIN
   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I
           DEFER INTERRUPT

WHENEVER ERROR STOP

CALL f_inicio()

LET m_log = m_usuario CLIPPED, ".RETC739.log"
CALL STARTLOG(m_log CLIPPED)

  CALL f_principal()

END MAIN

#################################################################################
FUNCTION f_inicio()

    DEFINE
        lc_prepare      CHAR(100)
        
--inicializa variables
LET m_hoy   = TODAY
LET m_log   = " "
LET m_enter = ""
LET m_cuantos = 0
LET m_char = ""

INITIALIZE mr_param.*, m_usuario, mr_edo.* TO NULL

    -- MOVIMIENTO DE TRANSFERENCIA LEY INFONAVIT 73
    SELECT codigo
    INTO   gs_mov_viv97
    FROM   tab_movimiento
    WHERE  descripcion  = "RETIRO TRANSFERENCIA VIVIENDA 97 REGIMEN 73"

    SELECT codigo_afore
    INTO   gs_codigo_afore
    FROM   tab_afore_local

    SELECT afore_cod
    INTO   gs_invercap
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*INVERCAP*"

--Parametros globales y usuario que ejecuta
   SELECT a.*,USER INTO mr_param.*, m_usuario
   FROM safre_af:seg_modulo a
   WHERE a.modulo_cod = "ret"

--Estados
    SELECT A.estado_solicitud
    INTO   mr_edo.rechazado
    FROM   ret_estado A
    WHERE  A.descripcion = "DERECHO NO OTORGADO"   #112

    SELECT A.estado_solicitud
    INTO   mr_edo.provisionado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROVISIONADO DERECHO"   #114

    SELECT A.estado_solicitud
    INTO   mr_edo.preliquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "PRELIQUIDADO DERECHO"   #116

    ----- DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM lc_prepare

END FUNCTION

#################################################################################
FUNCTION f_principal()

CALL f_ventana()

INPUT m_folio  WITHOUT DEFAULTS FROM folio

   AFTER FIELD folio
      IF m_folio IS NULL OR m_folio = 0 THEN
            ERROR "ERROR, INGRESE UN FOLIO VALIDO"
            NEXT FIELD folio
      END IF
      IF f_cuenta() = 0 THEN
          ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
          NEXT FIELD folio
      END IF

   AFTER INPUT
      IF m_folio IS NULL OR m_folio = 0 THEN
            ERROR "ERROR, INGRESE UN FOLIO VALIDO"
            NEXT FIELD folio
      END IF
      IF f_cuenta() = 0 THEN
          ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
          NEXT FIELD folio
      END IF

   ON KEY (ESC)

      IF m_folio IS NULL OR m_folio = 0 THEN
            ERROR "ERROR, INGRESE UN FOLIO VALIDO"
            NEXT FIELD folio
      END IF
      IF f_cuenta() = 0 THEN
          ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
          NEXT FIELD folio
      END IF
      EXIT INPUT

   ON KEY (CONTROL-C, INTERRUPT)
      ERROR "PROCESO CANCELADO" SLEEP 2
      ERROR ""
      EXIT PROGRAM

END INPUT

    WHILE TRUE
      PROMPT "SE PRELIQUIDARAN ", m_cuantos, " CUENTAS. ¿ESTA SEGURO? (S/N)  " FOR CHAR m_enter
      IF m_enter MATCHES "[SsNn]" THEN
        IF m_enter MATCHES "[Ss]" THEN
           CALL f_preliquida()
           CALL f_desmarca_rechazos(m_folio)
           EXIT WHILE
        ELSE
           ERROR "PROCESO CANCELADO" SLEEP 2
           ERROR ""
           CLOSE WINDOW win1
           EXIT PROGRAM
        END IF   
      ELSE
        ERROR "SOLO PRESIONE S o N"
      END IF  
    END WHILE

CLOSE WINDOW win1
END FUNCTION

#################################################################################
FUNCTION f_ventana()

    OPEN WINDOW win1 AT 2,2 WITH FORM "RETC7151" ATTRIBUTE(BORDER)
    DISPLAY " RETC739     PRELIQUIDACION  DERECHO OTORGADO V. UNICA 2.5     ",m_hoy USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " [Esc]Ejecuta Preliquidacion                           [Ctrl-C]Salir     " AT 1,1 ATTRIBUTE(REVERSE)  

END FUNCTION

#################################################################################
FUNCTION f_cuenta()

LET m_cuantos = 0

  SELECT COUNT(*) INTO m_cuantos
  FROM  ret_det_datamart a
  WHERE a.folio = m_folio
  AND   a.estado = mr_edo.provisionado
  AND   a.tipo_retiro IN ("A", "B", "C", "U")

RETURN m_cuantos

END FUNCTION

#################################################################################
FUNCTION f_preliquida()

DEFINE r_pre RECORD LIKE safre_af:ret_preliquida.*
DEFINE r_det_dat RECORD LIKE safre_af:ret_det_datamart.*

DEFINE 
    ldt_fecha_ini_mes DATE

DEFINE l_cuanto SMALLINT

DEFINE ld_precio_viv LIKE glo_valor_Accion.precio_del_dia
    
    
    DEFINE
        ls_estado_act               ,
        ls_ind_recep_trans          SMALLINT


LET ls_ind_recep_trans  = 0

#LET ldt_fecha_ini_mes =  MDY(MONTH(m_hoy),1,YEAR(m_hoy))


#SELECT precio_del_dia
#INTO   ld_precio_viv
#FROM   glo_valor_Accion
#WHERE  fecha_valuacion  = ldt_fecha_ini_mes
#AND    codigo_siefore   = 11


ERROR "PROCESANDO INFORMACION"

# CAT MLM-1590 Se modifica el query que obtiene las solicitudes para preliquidar, contemplando las
#              que no presentan movimientos en ret_provision

DECLARE cur_det CURSOR FOR
   SELECT * FROM  ret_det_datamart a
   WHERE a.folio = m_folio
   AND   a.estado = mr_edo.provisionado
   AND   a.tipo_retiro IN ("A", "B", "C", "U")

INITIALIZE r_det_dat.* TO NULL
FOREACH cur_det INTO r_det_dat.*

     SELECT count(*) INTO l_cuanto
     FROM   dis_provision d
     WHERE  d.folio           = m_folio
     AND    d.tipo_movimiento IN (800,810,815,805)
     AND    d.nss             = r_det_dat.nss
   
     IF l_cuanto > 0 THEN
     
         DECLARE cur_pre CURSOR FOR
         SELECT * FROM dis_provision d
         WHERE  d.folio           = m_folio
         AND    d.tipo_movimiento IN (800,810,815,805)
         AND    d.nss             = r_det_dat.nss
         
         INITIALIZE r_pre.* TO NULL
         
          FOREACH cur_pre INTO r_pre.*
                  
                  IF r_pre.tipo_movimiento <> gs_mov_viv97 THEN
                      LET r_pre.fecha_conversion  = m_hoy
                      LET r_pre.fecha_pago        = m_hoy
                      IF r_pre.siefore <> 11 THEN
                          LET r_pre.fecha_valor = m_hoy
                      END IF
         
                      INSERT INTO safre_af:ret_preliquida VALUES(r_pre.*)
         
                      IF SQLCA.SQLERRD[3] = 0 THEN  #Si no inserto NADA
                         LET m_char = ""
                         LET m_char = "ERROR AL INSERTAR EN ret_preliquida",r_pre.*
                         CALL ERRORLOG(m_char CLIPPED)
                      #ELSE
                      #   IF r_pre.siefore = 11 THEN
                      #       UPDATE safre_af:ret_preliquida
                      #       SET    precio_accion    = ld_precio_viv     ,
                      #              fecha_valor      = ldt_fecha_ini_mes
                      #       WHERE  nss              = r_pre.nss
                      #       AND    folio            = r_pre.folio
                      #       AND    consecutivo_lote = r_pre.consecutivo_lote
                      #       AND    siefore          = 11
         
                      #   END IF -- Siefore 11
                      END IF -- No inserto nada
                  END IF -- Movimiento Viv97

              INITIALIZE r_pre.* TO NULL
          END FOREACH 
     END IF
     
     UPDATE safre_af:ret_trans_imss SET estado_solicitud = mr_edo.preliquidado
     WHERE nss  = r_det_dat.nss
     AND folio  = m_folio
     AND estado_solicitud = mr_edo.provisionado
               
     UPDATE safre_af:ret_det_datamart SET estado = mr_edo.preliquidado
     WHERE nss  = r_det_dat.nss
     AND folio  = m_folio
     AND estado = mr_edo.provisionado

END FOREACH

# CAT MLM-1590    

PROMPT " PROCESO TERMINADO SATISFACTORIAMENTE ... <ENTER> PARA SALIR " FOR CHAR m_enter

END FUNCTION

#################################################################################
FUNCTION f_desmarca_rechazos(ps_folio)
    DEFINE
        li_desm             ,
        ps_folio            INTEGER

    DEFINE  lr_desmarca       RECORD
            nss               LIKE  cta_his_marca.nss          ,
            movimiento        LIKE  cta_his_marca.marca_cod    ,
            consec            LIKE  cta_his_marca.correlativo  ,
            edo_causa         LIKE  cta_his_marca.estado_marca ,
            marca_causa       LIKE  cta_his_marca.marca_causa
    END RECORD

    INITIALIZE lr_desmarca.* TO NULL 
    LET  li_desm = 0

    DECLARE cur_desm CURSOR FOR
    SELECT A.nss                ,
           B.marca_cod          ,
           B.correlativo        ,
           0                    ,
           0
    FROM   ret_trans_imss A ,
           cta_act_marca B
    WHERE  A.nss                = B.nss
    AND    B.marca_cod         IN (800,810,815,805)
    AND    A.folio              = ps_folio
    AND    A.estado_solicitud   = mr_edo.rechazado        

    FOREACH cur_desm INTO lr_desmarca.*
        
        EXECUTE eje_desmarca        USING  lr_desmarca.*, 
                                           m_usuario
        LET   li_desm = li_desm + 1

    END FOREACH
END FUNCTION
#################################################################################
