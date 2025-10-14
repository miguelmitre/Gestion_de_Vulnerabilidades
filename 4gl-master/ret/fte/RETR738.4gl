#################################################################################
#Propietario       => E.F.P.                                                    #
#Programa          => RETR738 PROGRAMA DE REVERSOS (DERECHO OTORGADO) VU 2.5    #
#Fecha             => 22 de Octubre del 2012                                    #
#Autor             => CRISTINA ABASOLO TAPIA                                    #
#Sistema           => RET                                                       #
#Requerimiento     => MLMVUA-3 (RF003)                                          #
#------------------------------MODIFICACIONES-----------------------------------#
#Requerimiento     => MLM-1603                                                  #
#Fecha y Autor     => 29-Ene-2013 Cristina Abasolo Tapia                        #
#Descripcion       => Se implementan los metodos de reverso de la marca y       #
#                  => desmarca y se controla el orden de eliminacion en el      #
#                  => reverso                                                   #
#################################################################################
DATABASE safre_af

DEFINE
  m_usuario       VARCHAR(50),
  m_hoy           DATE,
  mr_param        RECORD LIKE safre_af:seg_modulo.*,
  m_enter         CHAR(1),
  m_cuantos       INT,
  m_proceso,
  m_error         SMALLINT,
  m_marca_921,
  m_marca_922,
  m_marca_140,
  m_marca_800,
  m_marca_805,
  m_marca_806,  #CPL-2324
  m_marca_810,
  m_marca_815    SMALLINT,
  m_zero         SMALLINT,
  m_log           VARCHAR(30),
  m_char          CHAR(300),
  m_accion        VARCHAR(20),
  m_folio         DEC(11,0),
  mr_edo RECORD
        derecho_otorgado,
        provisionado    ,
        preliquidado    ,
        liquidado       ,
        rechazado_derecho,
        liquidado_derecho,
        sol_reinversion,
        liq_saldo       LIKE ret_estado.estado_solicitud
  END RECORD,
  m_estado_old    ,
  m_estado_new    LIKE ret_estado.estado_solicitud,
  m_tabla         ,
  m_tipos         VARCHAR(50),
  m_folios        VARCHAR(50),
  m_query         CHAR(7000)


########################################################################################
MAIN

   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I
           DEFER INTERRUPT

WHENEVER ERROR STOP

CALL f_inicio()

LET m_log = m_usuario CLIPPED, ".RETR738.log"
CALL STARTLOG(m_log CLIPPED)

  CALL f_ventana_main()
  CALL f_menu()

  CLOSE WINDOW win_main

END MAIN

########################################################################################
FUNCTION f_inicio()
--inicializa variables
LET m_hoy     = TODAY
LET m_log     = ""    LET m_char = " "
LET m_error   = 0     LET m_proceso = 0
LET m_cuantos = 0     LET m_query   = ""
LET m_zero = 0

INITIALIZE mr_param.*, m_usuario, m_enter, mr_edo.*, m_folio, m_marca_921,m_marca_922, m_marca_140,
           m_marca_800, m_marca_805, m_marca_810, m_marca_815,m_marca_806 TO NULL

CREATE TEMP TABLE nss_reversa_status
    (nss CHAR(11)) WITH NO LOG

#Parametros globales y usuario que ejecuta
SELECT a.*,USER INTO mr_param.*, m_usuario
FROM safre_af:seg_modulo a
WHERE a.modulo_cod = "ret"

#Selecciona estados
SELECT A.estado_solicitud INTO mr_edo.derecho_otorgado     #108
FROM   ret_estado A
WHERE  A.descripcion = "DERECHO OTORGADO"

SELECT A.estado_solicitud INTO mr_edo.provisionado         #114
FROM   ret_estado A
WHERE  A.descripcion = "PROVISIONADO DERECHO"

SELECT A.estado_solicitud INTO mr_edo.preliquidado         #116
FROM   ret_estado A
WHERE  A.descripcion = "PRELIQUIDADO DERECHO"  

SELECT A.estado_solicitud INTO mr_edo.liquidado            #118
FROM   ret_estado A
WHERE  A.descripcion = "LIQUIDADO DERECHO"

SELECT A.estado_solicitud INTO mr_edo.rechazado_derecho
FROM   ret_estado A 
WHERE  A.descripcion = "DERECHO NO OTORGADO"

SELECT A.estado_solicitud INTO mr_edo.liquidado_derecho    #119
FROM   ret_estado A
WHERE A.descripcion = "LIQUIDADO TOTAL DERECHO"

SELECT A.estado_solicitud INTO mr_edo.sol_reinversion      #140
FROM   ret_estado A
WHERE A.descripcion = "SOLICITUD REINVERSION"

SELECT A.estado_solicitud INTO mr_edo.liq_saldo      #106
FROM   ret_estado A
WHERE A.descripcion = "LIQUIDADO SALDO"

#Selecciona Marca
SELECT marca_cod INTO m_marca_921
FROM tab_marca
WHERE marca_desc ="CONSULTA SALDO PREVIO"

SELECT marca_cod INTO m_marca_922
FROM tab_marca
WHERE marca_desc ="MARCA DE REINVERSION"

SELECT marca_cod INTO m_marca_140
FROM tab_marca
WHERE marca_desc ="INHABILITACION POR RETIRO"

SELECT marca_cod INTO m_marca_800
FROM tab_marca
WHERE marca_desc ="TRANS.IMSS RENTA VITALICI"

SELECT marca_cod INTO m_marca_810
FROM tab_marca
WHERE marca_desc ="TRANS.GOB.FED.PEN. IMSS"

SELECT marca_cod INTO m_marca_815
FROM tab_marca
WHERE marca_desc ="TRANFERENCIA RECURSOS AL"

SELECT marca_cod INTO m_marca_805
FROM tab_marca
WHERE marca_desc ="TRANS POR PRESCRIPCION DE DERECHOS"

SELECT marca_cod INTO m_marca_806   #CPL-2324
FROM tab_marca
WHERE marca_desc ="TRANS REGIMEN DE JUBILACIONES Y PENSIONES (B)"

#Prepare sentencia para actualizar ret_det_datamart
LET m_char = "UPDATE ret_det_datamart ",
             "SET    estado = ? ", 
             "WHERE  folio = ? ",
             "AND    estado = ? ",
             "AND nss IN (SELECT nss FROM nss_reversa_status)"

LET m_char = m_char CLIPPED
PREPARE p_update1 FROM m_char

#Prepare sentencia para actualizar ret_det_datamart para retiros tipo D y S
LET m_char = "UPDATE ret_det_datamart ",
             "SET    estado = ?, folio = null ", 
             "WHERE  folio = ? ",
             "AND    estado = ? ",
             "AND    tipo_retiro IN ('D','S')",
             "AND    diagnostico = 501 "

LET m_char = m_char CLIPPED
PREPARE p_updateDS FROM m_char

#Prepare borra montos ventanillas
LET m_char = ""
LET m_char = "DELETE FROM safre_af:ret_monto_ventanilla " ,
             " WHERE folio = ? ",
             " AND nss IN (SELECT nss FROM nss_reversa_status)"

LET m_char = m_char CLIPPED
PREPARE p_del_mto FROM m_char

#Prepare borra montos ventanillas
LET m_char = ""
LET m_char = "DELETE FROM safre_af:ret_trans_imss " ,
             " WHERE folio = ? ",
             " AND nss IN (SELECT nss FROM nss_reversa_status)"

LET m_char = m_char CLIPPED
PREPARE p_del_trans FROM m_char

#Prepare borra folio de datamart
LET m_char = ""
LET m_char = "UPDATE safre_af:ret_det_datamart SET " ,
             " folio = null ",
             " WHERE folio = ? ",
             " AND nss IN (SELECT nss FROM nss_reversa_status)"

LET m_char = m_char CLIPPED
PREPARE p_del_dtmrt FROM m_char

#Prepare borra folio de rechazos
LET m_char = ""
LET m_char = "UPDATE safre_af:ret_rech_datamart SET " ,
             " folio = null ",
             " WHERE folio = ? ",
             " AND nss IN (SELECT nss FROM nss_reversa_status)"

LET m_char = m_char CLIPPED
PREPARE p_del_rech FROM m_char


#Prepare actualiza montos ventanillas
LET m_char = ""
LET m_char = "UPDATE safre_af:ret_trans_imss SET " ,
             " estado_solicitud = ? ",
             " WHERE folio = ? ",
             " AND estado_solicitud = ? ",
             " AND nss IN (SELECT nss FROM nss_reversa_status)"

LET m_char = m_char CLIPPED
PREPARE p_upd_trans FROM m_char


#Prepare sentencia para actualizar Liquidadas con derecho total
LET m_char = ""
LET m_char = "UPDATE safre_af:ret_solicitud_saldo SET " ,
             " estado_solicitud = ? ",
             " WHERE id_solicitud_saldo = ?",
             " AND estado_solicitud = ? ",
             " AND nss IN (SELECT nss FROM nss_reversa_status)"

LET m_char = m_char CLIPPED
PREPARE p_upd_solicitud FROM m_char

#Prepare sentencia para actualizar Liquidadas sin Derecho Total
#Prepare borra ret_reinversion
LET m_char = ""
LET m_char = "DELETE FROM safre_af:ret_reinversion " ,
             " WHERE id_solicitud_saldo = ?"

LET m_char = m_char CLIPPED
PREPARE p_del_reinversion FROM m_char

END FUNCTION

########################################################################################
FUNCTION f_ventana_main()

    OPEN WINDOW win_main AT 2,2 WITH 22 ROWS, 73 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETR738         REVERSOS VENTANILLA UNICA 2.5               ",m_hoy USING "DD-MM-YYYY" AT 5,1 ATTRIBUTE(REVERSE)

END FUNCTION

########################################################################################
FUNCTION f_menu()

MENU "REVERSOS"

     COMMAND "Provision" "Reversa Provision Derecho Otorgado"
        LET m_proceso = 1
        CALL f_principal()
        CLOSE WINDOW win1
     COMMAND "Preliquidacion" "Reversa Preliquidacion Derecho Otorgado"
        LET m_proceso = 2
        CALL f_principal()
        CLOSE WINDOW win1
     COMMAND "Liquidacion" "Reversa Liquidacion Derecho Otorgado"
        LET m_proceso = 3
        CALL f_principal()
        CLOSE WINDOW win1
     COMMAND "Salir" "Salir del Programa"
        EXIT PROGRAM

END MENU

END FUNCTION

########################################################################################
FUNCTION f_principal()

CALL f_variables()   #Asigna variables dependiendo del proceso
CALL f_ventana()     #Abre ventana para captura de folio

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

      PROMPT "¿DESEA EJECUTAR EL REVERSO DE ", m_accion," DE DERECHO OTORGADO? (S/N)" FOR CHAR m_enter
      IF m_enter MATCHES "[SsNn]" THEN
         IF m_enter MATCHES "[Ss]" THEN
            CALL f_reversa()
            EXIT INPUT
         ELSE
            ERROR "PROCESO CANCELADO" 
            NEXT FIELD folio
         END IF   
      ELSE
         ERROR "SOLO PRESIONE S o N"
         NEXT FIELD folio
      END IF  

   ON KEY (CONTROL-C, INTERRUPT)
      ERROR "PROCESO CANCELADO" SLEEP 2
      ERROR ""
      EXIT INPUT

END INPUT

END FUNCTION

########################################################################################
FUNCTION f_ventana()

    OPEN WINDOW win1 AT 2,2 WITH FORM "RETR7381" ATTRIBUTE(BORDER)
    DISPLAY " RETR7381     REVERSO   ",m_accion,  "   DERECHO OTORGADO  VU. 2.5       ",m_hoy USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
    --DISPLAY " RETR7381     REVERSO   PRELIQUIDACION   DERECHO OTORGADO  VU. 2.5       ",m_hoy USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
    --DISPLAY " RETR738       ENVIO DERECHO OTORGADO VENTANILLA UNICA 2.5       ",m_hoy USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "       [Esc]Reversa                                   [Ctrl-C]Salir     " AT 1,1 ATTRIBUTE(REVERSE)  

END FUNCTION

########################################################################################
FUNCTION f_variables()

   LET m_accion =""  LET m_tabla  =""  
   LET m_folio =""   LET m_tipos = ""

  INITIALIZE m_estado_old, m_estado_new TO NULL

    CASE m_proceso
       WHEN 1
          LET m_accion = "   PROVISION  "
          LET m_tabla  = "dis_provision"
          LET m_tipos  = "800,805,810,815,816,806"  #CPL-2324
          LET m_estado_old = mr_edo.provisionado
          LET m_estado_new = mr_edo.derecho_otorgado
       WHEN 2
          LET m_accion = "PRELIQUIDACION"
          LET m_tabla  = "ret_preliquida"
          LET m_tipos  = "800,805,810,815,816,806"  #CPL-2324
          LET m_estado_old = mr_edo.preliquidado
          LET m_estado_new = mr_edo.provisionado
       WHEN 3
          LET m_accion = "  LIQUIDACION "
          LET m_tabla  = "dis_cuenta"
          LET m_tipos  = "800,805,810,815,816,806"  #CPL-2324
          LET m_estado_old = mr_edo.liquidado
          LET m_estado_new = mr_edo.preliquidado
    END CASE

END FUNCTION

########################################################################################
FUNCTION f_cuenta()
    
    DEFINE l_query CHAR (500)
    
    LET m_cuantos = 0
    LET l_query = ""
    
 SELECT COUNT(*) INTO m_cuantos
  FROM  ret_det_datamart a
  WHERE a.folio = m_folio
  AND   a.estado = m_estado_old
  OR   (a.estado = mr_edo.rechazado_derecho
  AND   a.diagnostico IN (502,505,509))
  
  IF m_cuantos > 0 THEN
    
          DELETE FROM nss_reversa_status WHERE 1 = 1   
          LET l_query = "INSERT INTO nss_reversa_status            \n ", 
                        "      SELECT DISTINCT(det.nss)            \n ",
                        "      FROM ret_det_datamart det           \n ",
                        "      WHERE det.folio = ",m_folio,"      \n",  
                        "      AND   det.estado = ",m_estado_old,"  \n",
                        "      OR (det.estado = ",mr_edo.rechazado_derecho, "\n",
                        "      AND det.diagnostico IN (502,505,509)) \n"
          LET l_query = l_query CLIPPED 
                    
          PREPARE pre_inserta FROM l_query    
          EXECUTE pre_inserta
          
          LET l_query = ""
          LET l_query = "INSERT INTO nss_reversa_status            \n ", 
                        "      SELECT DISTINCT(det.nss)            \n ",
                        "      FROM ret_rech_datamart det          \n ",
                        "      WHERE det.folio = ",m_folio,"       \n"  
          LET l_query = l_query CLIPPED 
                    
          PREPARE pre_inserta1 FROM l_query    
          EXECUTE pre_inserta1

    END IF

RETURN m_cuantos

END FUNCTION

########################################################################################
FUNCTION f_reversa()

IF f_delete() = 1 THEN
   RETURN
END IF 
CALL f_update()

PROMPT "PROCESO FINALIZADO <ENTER> PARA SALIR...." FOR CHAR m_enter

END FUNCTION

########################################################################################
FUNCTION f_update()

   EXECUTE p_update1 USING m_estado_new, m_folio, m_estado_old
   DISPLAY "REGISTROS ACTUALIZADOS:   ", SQLCA.SQLERRD[3] USING "<<<<<<<" AT 13,1
   
   IF m_proceso = 1 THEN      #provisiona
   
        # CPL-1702 Se verifica si hubo cambios en el historico de diagnosticos - CAT
        CALL f_upt_his_diagnostico(m_folio) 
        
        EXECUTE p_updateDS USING mr_edo.derecho_otorgado, m_folio, mr_edo.liquidado
        EXECUTE p_del_dtmrt USING m_folio
        EXECUTE p_del_rech USING m_folio
        
   END IF 

END FUNCTION

########################################################################################
FUNCTION f_delete()

DISPLAY "FOLIO(S) ACTUALIZADO(S): 1 " AT 14,1
   
IF m_proceso = 1 THEN      #provisiona
   CALL f_reversa_provision();
END IF
IF m_proceso = 2 THEN      #preliquida
   CALL f_reversa_preliq(); 
END IF
IF m_proceso = 3 THEN      #liquida
   CALL f_reversa_liquida();
END IF

#Prepare sentencia para borrar 
LET m_char = ""
LET m_char = "DELETE FROM safre_af:",m_tabla,
             " WHERE folio =",m_folio,
             " AND tipo_movimiento IN (",m_tipos,") ",
             " AND nss IN (SELECT nss FROM nss_reversa_status)"
             
LET m_char = m_char CLIPPED
PREPARE p_delete FROM m_char

#Borra registros
EXECUTE p_delete 
DISPLAY "REGISTROS BORRADOS DE TABLA ", m_tabla,": ", SQLCA.SQLERRD[3] USING "<<<<<<<" AT 15,1

RETURN 0

END FUNCTION

#########################################################################
FUNCTION f_reversa_provision()

    DEFINE l_estado_reinversion SMALLINT

    DEFINE m_condition RECORD
            id_saldo          INTEGER,
            nss               CHAR(11),
            estado_solicitud  SMALLINT,
            id_registro       DECIMAL(16,0),
            tipo_retiro       CHAR
    END  RECORD

    LET m_query = "SELECT rss.id_solicitud_saldo, \n",
                  "       rss.nss, \n", 
                  "       rss.estado_solicitud, \n",
                  "       rdd.id_registro, \n",
                  "       rdd.tipo_retiro \n",
                  " FROM ret_solicitud_saldo rss, ret_det_datamart rdd \n",
                  " WHERE rss.nss = rdd.nss \n",
                  " AND rdd.folio = ",m_folio ,
                  " AND rdd.estado = ", mr_edo.rechazado_derecho,
                  " AND rss.nss IN (SELECT nss FROM nss_reversa_status)"

    LET m_query = m_query CLIPPED 

    PREPARE pre_saldo_prov FROM m_query
    DECLARE cur_saldo_prov CURSOR FOR pre_saldo_prov
    INITIALIZE m_condition.* TO NULL
    ERROR ""  LET m_error = 0
      
    FOREACH cur_saldo_prov INTO m_condition.*

        SELECT estado_solicitud
          INTO l_estado_reinversion
          FROM ret_reinversion
         WHERE id_solicitud_saldo = m_condition.id_saldo
    
        IF l_estado_reinversion = mr_edo.sol_reinversion THEN    #solicitud_reinvesion 140
             EXECUTE p_del_reinversion USING m_condition.id_saldo
             CALL f_reversa_marca (m_condition.nss, m_marca_922, m_condition.id_saldo);
        END IF

        CASE m_condition.tipo_retiro

            WHEN 'A'
                CALL f_reversa_desmarca (m_condition.nss, m_marca_800, m_condition.id_registro)

            WHEN 'B'
                SELECT "OK"
                  FROM cta_his_marca
                 WHERE nss = m_condition.nss
                   AND correlativo = m_condition.id_registro
                   AND marca_cod = m_marca_806
                 GROUP BY 1

                IF (SQLCA.SQLCODE = 0) THEN
                    CALL f_reversa_desmarca (m_condition.nss, m_marca_806, m_condition.id_registro)
                ELSE
                    CALL f_reversa_desmarca (m_condition.nss, m_marca_810, m_condition.id_registro)
                END IF

            WHEN 'C'
                CALL f_reversa_desmarca (m_condition.nss, m_marca_815, m_condition.id_registro);

            WHEN 'U'
                CALL f_reversa_desmarca (m_condition.nss, m_marca_805, m_condition.id_registro);

        END CASE
    END FOREACH

    EXECUTE p_del_mto USING m_folio
    DISPLAY "REGISTROS BORRADOS DE TABLA ret_monto_ventanilla: ", SQLCA.SQLERRD[3] USING "<<<<<<<" AT 16,1
    
    EXECUTE p_del_trans USING m_folio
    DISPLAY "REGISTROS BORRADOS DE TABLA ret_trans_imss: ", SQLCA.SQLERRD[3] USING "<<<<<<<" AT 17,1
    
END FUNCTION

##########################################################################
FUNCTION f_reversa_preliq()
    
    EXECUTE p_upd_trans USING m_estado_new, m_folio, m_estado_old
    DISPLAY "REGISTROS ACTUALIZADOS DE TABLA ret_trans_imss: ", SQLCA.SQLERRD[3] USING "<<<<<<<" AT 16,1

END FUNCTION

##########################################################################
FUNCTION f_reversa_liquida()
    DEFINE l_estado_reinversion SMALLINT,
           l_num_error SMALLINT,
           l_unload    CHAR(500),
           l_marca,
           l_cod_rech  SMALLINT
             
    DEFINE l_cuanto SMALLINT
    
    DEFINE m_condition RECORD
            id_saldo          INTEGER,
            nss               CHAR(11),
            estado_solicitud  SMALLINT,
            id_registro       DECIMAL(16,0),
            tipo_retiro       CHAR
    END  RECORD
 
    LET l_cuanto = 0

WHENEVER ERROR CONTINUE
   DROP TABLE rev_error_nss
WHENEVER ERROR STOP

CREATE TEMP TABLE rev_error_nss
    (nss CHAR(11),
    estado_reinversion SMALLINT) WITH NO LOG
     
      LET m_query = "SELECT rss.id_solicitud_saldo, \n",
                    "       rss.nss, \n", 
                    "       rss.estado_solicitud, \n",
                    "       rdd.id_registro, \n",
                    "       rdd.tipo_retiro \n",
                    " FROM ret_solicitud_saldo rss, ret_det_datamart rdd \n",
                    " WHERE rss.nss = rdd.nss \n",
                    " AND rdd.folio = ",m_folio ,
                    " AND rss.estado_solicitud IN(",mr_edo.liq_saldo, ",", mr_edo.liquidado_derecho,")",
                    " AND rdd.estado = ", mr_edo.liquidado,
                    " AND rss.nss IN (SELECT nss FROM nss_reversa_status)"
      
      LET m_query = m_query CLIPPED 
      
        PREPARE pre_saldo FROM m_query
        DECLARE cur_saldo CURSOR FOR pre_saldo
        INITIALIZE m_condition.* TO NULL
        ERROR ""  LET m_error = 0
      
      FOREACH cur_saldo INTO m_condition.*
         #si es una transferencia total
         IF m_condition.estado_solicitud = mr_edo.liquidado_derecho THEN   #119
             EXECUTE p_upd_solicitud USING mr_edo.liq_saldo, m_condition.id_saldo, mr_edo.liquidado_derecho
             #CALL f_reversa_desmarca (m_condition.nss, m_marca_921, m_condition.id_saldo);
         ELSE 
             SELECT estado_solicitud INTO l_estado_reinversion FROM ret_reinversion
             WHERE id_solicitud_saldo = m_condition.id_saldo
    
             IF l_estado_reinversion = mr_edo.sol_reinversion THEN    #solicitud_reinvesion 140
                  EXECUTE p_del_reinversion USING m_condition.id_saldo
                  CALL f_reversa_marca (m_condition.nss, m_marca_922, m_condition.id_saldo);
             ELSE
                IF l_estado_reinversion != 0 THEN
                   DELETE FROM nss_reversa_status WHERE nss = m_condition.nss;
                   INSERT INTO rev_error_nss VALUES (m_condition.nss, l_estado_reinversion);
                   #DISPLAY "inserta: ",m_condition.nss
                   CONTINUE FOREACH
                END IF
             END IF
         END IF

         CALL f_reversa_marca (m_condition.nss,m_marca_140, m_condition.id_registro)

#Se agrega id_registro de datamart para la marca del retiro - ACS 17/12/2012
         CASE m_condition.tipo_retiro

             WHEN 'A'
                CALL f_reversa_desmarca (m_condition.nss, m_marca_800, m_condition.id_registro)

             WHEN 'B'
#CPL-2324 INI
                SELECT "OK"
                FROM cta_his_marca
                WHERE nss = m_condition.nss
                AND correlativo = m_condition.id_registro
                AND marca_cod = m_marca_806
                GROUP BY 1

                IF (SQLCA.SQLCODE = 0) THEN
                    CALL f_reversa_desmarca (m_condition.nss, m_marca_806, m_condition.id_registro)
                ELSE
                    CALL f_reversa_desmarca (m_condition.nss, m_marca_810, m_condition.id_registro)
                END IF
#CPL-2324 FIN
             WHEN 'C'
                CALL f_reversa_desmarca (m_condition.nss, m_marca_815, m_condition.id_registro);

             WHEN 'U'
                CALL f_reversa_desmarca (m_condition.nss, m_marca_805, m_condition.id_registro);

         END CASE
      END FOREACH

      EXECUTE p_upd_trans USING m_estado_new, m_folio, m_estado_old
      DISPLAY "REGISTROS ACTUALIZADOS DE TABLA ret_trans_imss: ", SQLCA.SQLERRD[3] USING "<<<<<<<" AT 16,1

      SELECT count(*) INTO l_num_error FROM rev_error_nss;
      IF l_num_error > 0 THEN
          LET l_unload = ""
          LET l_unload = mr_param.ruta_listados CLIPPED , "/",m_usuario CLIPPED,
                         ".reversoLiq_",m_folio USING "<<<<<<<<",
                         ".",m_hoy USING "DDYYMM"
          LET l_unload = l_unload CLIPPED
          #DISPLAY l_unload
          UNLOAD TO l_unload
          SELECT " El NSS ", ret_re.nss, " tiene una solicitud de reinversion en estado ", ret_re.estado_reinversion,
                 " por lo cual no se le puede aplicar el reverso de forma correcta "
          FROM rev_error_nss ret_re

          DISPLAY "LIQUIDACION CON ERRORES, REVISAR DETALLE EN: " AT 18,1
          DISPLAY l_unload AT 19,1 ATTRIBUTE (REVERSE)
      END IF

END FUNCTION

##########################################################################
FUNCTION f_reversa_marca(pnss, pmarca, pcorrelativo)
   DEFINE 
      pnss              CHAR(11),
      pmarca            SMALLINT,
      pcorrelativo      INTEGER
   DEFINE    
      v_desmarca        VARCHAR(100)
      
   SELECT "X"
   FROM   cta_act_marca
   WHERE  nss = pnss
   AND    marca_cod = pmarca
   AND    correlativo = pcorrelativo
   
   # -- Si la cuenta está marcada, se procede a realizar el reverso de la marca
   IF SQLCA.SQLCODE = 0 THEN
      LET v_desmarca    = " EXECUTE PROCEDURE reversa_marca ( ?,?,?)"

      PREPARE eje_desmarca FROM v_desmarca

      EXECUTE eje_desmarca
      USING pnss,
            pmarca,
            pcorrelativo
   END IF
   
END FUNCTION


##########################################################################
FUNCTION f_reversa_desmarca(pnss, pmarca, pcorrelativo)

    DEFINE 
       pnss              CHAR(11),
       pmarca            SMALLINT,
       pcorrelativo      INTEGER
    DEFINE    
       v_desmarca        VARCHAR(100),
       ld_fecha_ini      DATE

    INITIALIZE ld_fecha_ini TO NULL

    # -- Obteniendo la fecha de la marca, se obtiene el registro más viejo
    DECLARE cur_rev_marca CURSOR FOR 
        SELECT c.fecha_ini
        FROM   cta_his_marca c
        WHERE  c.nss         = pnss
        AND    c.marca_cod   = pmarca
        AND    c.correlativo = pcorrelativo
        AND    c.fecha_fin   IS NOT NULL
        ORDER BY 1 DESC

    FOREACH cur_rev_marca INTO ld_fecha_ini
       EXIT FOREACH
    END FOREACH

    # -- Si la cuenta está marcada, se procede a realizar el reverso de la desmarca
    IF ld_fecha_ini IS NOT NULL THEN

        LET v_desmarca = " EXECUTE PROCEDURE reversa_desmarca (?,?,?,?)"
        PREPARE eje_rev_desmarca FROM v_desmarca

       EXECUTE eje_rev_desmarca
       USING pnss,
             pmarca,
             pcorrelativo,
             ld_fecha_ini
    END IF

END FUNCTION


#############################################################################################

FUNCTION f_upt_his_diagnostico (l_folio)

   DEFINE l_folio             INTEGER
   DEFINE lr_his_diagnostico  RECORD LIKE ret_his_diagnostico.*
   DEFINE lc_error            CHAR(1000)
   
   DECLARE cur_his_diag CURSOR FOR 
      SELECT a.*
      FROM   ret_his_diagnostico a, ret_det_datamart b
      WHERE  a.consecutivo = b.id_registro
      AND    a.nss = b.nss
      AND    b.folio = l_folio
      
   FOREACH cur_his_diag INTO lr_his_diagnostico.*
         
         UPDATE ret_det_datamart
         SET    diagnostico = lr_his_diagnostico.diagnostico,
                estado      = lr_his_diagnostico.estado
         WHERE  id_registro = lr_his_diagnostico.consecutivo
         AND    nss         = lr_his_diagnostico.nss
         AND    folio       = l_folio
         
         
         IF SQLCA.SQLERRD[3] = 0 THEN
           LET lc_error = "NO SE ACTUALIZO EL DIAGNOSTICO DEL HISTORICO, CONSECUTIVO: ", lr_his_diagnostico.consecutivo CLIPPED," NSS : ", lr_his_diagnostico.nss CLIPPED
           CALL ERRORLOG(lc_error CLIPPED)  
           RETURN
         ELSE 
           SET LOCK MODE TO WAIT 2;   # 2 segundos de espera
           LOCK TABLE ret_his_diagnostico IN EXCLUSIVE MODE;
              DELETE FROM ret_his_diagnostico
              WHERE nss         = lr_his_diagnostico.nss
              AND   consecutivo = lr_his_diagnostico.consecutivo
              AND   diagnostico = lr_his_diagnostico.diagnostico
              AND   estado      = lr_his_diagnostico.estado
           
           IF SQLCA.SQLERRD[3] = 0 THEN
              # Se envía el error de inserción al Log de errores
              LET lc_error = "NO SE ELIMINO EL HISTORICO DEL DIAGNOSTICO, CONSECUTIVO: ", lr_his_diagnostico.consecutivo CLIPPED," NSS : ", lr_his_diagnostico.nss CLIPPED
              CALL ERRORLOG(lc_error CLIPPED)
           END IF
           SET LOCK MODE TO NOT WAIT;
           UNLOCK TABLE ret_his_diagnostico;
         END IF
         
   END FOREACH
   
END FUNCTION 