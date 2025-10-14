#################################################################################
#Propietario       => E.F.P.                                                    #
#Programa          => RETR704 PROGRAMA DE REVERSOS VU 2.5                       #
#Fecha             => 7 de Octubre del 2012                                     #
#Autor             => CRISTINA ABASOLO TAPIA                                    #
#Sistema           => RET                                                       #
#Requerimiento     => MLMVUA-2 (RF002)                                          #
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
  m_log           VARCHAR(30),
  m_char          CHAR(300),
  m_accion        VARCHAR(20),
  m_folio         DEC(11,0),
  mr_edo RECORD
        sol_saldo      ,
        provisionado   ,
        preliquidado   ,
        liquidado      LIKE ret_estado.estado_solicitud
  END RECORD,
  m_estado_old    ,
  m_estado_new    LIKE ret_estado.estado_solicitud,
  m_tabla         ,
  m_tipos         VARCHAR(14),
  m_folios        VARCHAR(50)


########################################################################################
MAIN

   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I
           DEFER INTERRUPT

WHENEVER ERROR STOP

CALL f_inicio()

LET m_log = m_usuario CLIPPED, ".RETR7041.log"
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
LET m_cuantos = 0

INITIALIZE mr_param.*, m_usuario, m_enter, mr_edo.*, m_folio TO NULL

#Parametros globales y usuario que ejecuta
SELECT a.*,USER INTO mr_param.*, m_usuario
FROM safre_af:seg_modulo a
WHERE a.modulo_cod = "ret"

#Selecciona estados
SELECT A.estado_solicitud INTO mr_edo.sol_saldo       #100
FROM   ret_estado A
WHERE  A.descripcion = "SOLICITUD SALDO"

SELECT A.estado_solicitud INTO mr_edo.provisionado     #102
FROM   ret_estado A
WHERE  A.descripcion = "PROVISIONADO SALDO"

SELECT A.estado_solicitud INTO mr_edo.preliquidado     #104
FROM   ret_estado A
WHERE  A.descripcion = "PRELIQUIDADO SALDO"  

SELECT A.estado_solicitud INTO mr_edo.liquidado        #106
FROM   ret_estado A
WHERE  A.descripcion = "LIQUIDADO SALDO"

#Prepare sentencia para actualizar provision
LET m_char = "UPDATE safre_af:ret_solicitud_saldo SET " ,
             " estado_solicitud = ? ", ",",
             " folio_cargo = 0,",
             " diag_envio_procesar = 0 ",
             " WHERE folio_cargo = ?",
             " AND estado_solicitud = ?"

LET m_char = m_char CLIPPED
PREPARE p_update1 FROM m_char

#Prepare sentencia para actualizar pre y liquidacion
LET m_char = ""
LET m_char = "UPDATE safre_af:ret_solicitud_saldo SET " ,
             " estado_solicitud = ? ",
             " WHERE folio_cargo = ?",
             " AND estado_solicitud = ?"

LET m_char = m_char CLIPPED
PREPARE p_update2 FROM m_char

#Prepare sentencia para actualizar folio
LET m_char = ""
LET m_char = "UPDATE safre_af:ret_folio SET ",
             " estado_folio = ? ",
             " WHERE folio_cargo = ?",
             " AND estado_folio = ?"

LET m_char = m_char CLIPPED
PREPARE p_upd_fol FROM m_char

#Prepare sentencia para borrar folio
LET m_char = ""
LET m_char = "DELETE FROM safre_af:ret_folio " ,
             " WHERE folio_cargo = ?",
             " AND estado_folio = ?"

LET m_char = m_char CLIPPED
PREPARE p_del_fol FROM m_char

#Prepare borra montos fip
LET m_char = ""
LET m_char = "DELETE FROM safre_af:ret_mto_solicitud_saldo " ,
             " WHERE folio_cargo = ?"

LET m_char = m_char CLIPPED
PREPARE p_del_fip FROM m_char

END FUNCTION

########################################################################################
FUNCTION f_ventana_main()

    OPEN WINDOW win_main AT 2,2 WITH 22 ROWS, 73 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETR704         REVERSOS VENTANILLA UNICA 2.5               ",m_hoy USING "DD-MM-YYYY" AT 5,1 ATTRIBUTE(REVERSE)

END FUNCTION

########################################################################################
FUNCTION f_menu()

MENU "REVERSOS"

     COMMAND "Provision" "Reversa Provision Solicitud de Saldo"
        LET m_proceso = 1
        CALL f_principal()
        CLOSE WINDOW win1
     COMMAND "Preliquidacion" "Reversa Preliquidacion Solicitud de Saldo"
        LET m_proceso = 2
        CALL f_principal()
        CLOSE WINDOW win1
     COMMAND "Liquidacion" "Reversa Liquidacion Solicitud de Saldo"
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

      PROMPT "¿DESEA EJECUTAR EL REVERSO DE ", m_accion," DE SALDOS? (S/N)" FOR CHAR m_enter
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

    OPEN WINDOW win1 AT 2,2 WITH FORM "RETR7041" ATTRIBUTE(BORDER)
    DISPLAY " RETR7041     REVERSO   ",m_accion,  "   SALDOS  VU. 2.5       ",m_hoy USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
    --DISPLAY " RETR7041     REVERSO   PRELIQUIDACION   SALDOS  VU. 2.5       ",m_hoy USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
    --DISPLAY " RETR715       ENVIO SALDOS PREVIOS VENTANILLA UNICA 2.5       ",m_hoy USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
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
          LET m_tipos  = "921"
          LET m_estado_old = mr_edo.provisionado
          LET m_estado_new = mr_edo.sol_saldo
       WHEN 2
          LET m_accion = "PRELIQUIDACION"
          LET m_tabla  = "ret_preliquida"
          LET m_tipos  = "921,922"
          LET m_estado_old = mr_edo.preliquidado
          LET m_estado_new = mr_edo.provisionado
       WHEN 3
          LET m_accion = "  LIQUIDACION "
          LET m_tabla  = "dis_cuenta"
          LET m_tipos  = "921,922"
          LET m_estado_old = mr_edo.liquidado
          LET m_estado_new = mr_edo.preliquidado
    END CASE

END FUNCTION

########################################################################################
FUNCTION f_cuenta()

LET m_cuantos = 0

 SELECT COUNT(*) INTO m_cuantos
  FROM  ret_solicitud_saldo a
  WHERE a.folio_cargo = m_folio
  AND   a.estado_solicitud = m_estado_old

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

IF m_proceso = 1 THEN    #Provision
   EXECUTE p_update1 USING m_estado_new, m_folio, m_estado_old
ELSE
   EXECUTE p_update2 USING m_estado_new, m_folio, m_estado_old
END IF

   DISPLAY "REGISTROS ACTUALIZADOS:   ", SQLCA.SQLERRD[3] USING "<<<<<<<" AT 13,1

END FUNCTION

########################################################################################
FUNCTION f_delete()
DEFINE l_folio_abono DEC(11,0)

INITIALIZE l_folio_abono, m_folios TO NULL

#Busca folio abono
   SELECT folio_abono INTO l_folio_abono 
    FROM ret_folio
   WHERE folio_cargo = m_folio
   AND estado_folio = m_estado_old

IF STATUS = NOTFOUND AND m_proceso != 1 THEN  #si NO encuentra y es diferente de provision
   ERROR "NO EXISTE FOLIO ABONO" SLEEP 2
   RETURN 1
END IF

IF m_proceso = 1 THEN      #Provision
   LET m_folios = m_folio USING "<<<<<<<<"
ELSE
   IF l_folio_abono IS NOT NULL THEN
      LET m_folios = m_folio USING "<<<<<<<<", ",", l_folio_abono USING "<<<<<<<<"
   ELSE
      LET m_folios = m_folio USING "<<<<<<<<"
   END IF
END IF

#Prepare sentencia para borrar tabla segun el reverso(prov, preliq, liq)
LET m_char = ""
LET m_char = "DELETE FROM safre_af:",m_tabla,
             " WHERE folio IN (",m_folios,")",
             " AND tipo_movimiento IN (",m_tipos,")"

LET m_char = m_char CLIPPED
PREPARE p_delete FROM m_char

#Borra o actualiza folio
IF m_proceso = 1 THEN      #Provision
   EXECUTE p_del_fol USING m_folio, m_estado_old
   DISPLAY "FOLIO(S) BORRADO(S): ", SQLCA.SQLERRD[3] USING "<<<<<<<" AT 14,1
ELSE
   EXECUTE p_upd_fol USING  m_estado_new, m_folio, m_estado_old
   DISPLAY "FOLIO(S) ACTUALIZADO(S): ", SQLCA.SQLERRD[3] USING "<<<<<<<" AT 14,1
END IF

#Borra registros
IF m_proceso = 2 THEN      #preliquida
   EXECUTE p_del_fip USING m_folio
   DISPLAY "REGISTROS BORRADOS DE TABLA ret_mto_solicitud_saldo: ", SQLCA.SQLERRD[3] USING "<<<<<<<" AT 16,1
END IF

   EXECUTE p_delete
   DISPLAY "REGISTROS BORRADOS DE TABLA ", m_tabla,": ", SQLCA.SQLERRD[3] USING "<<<<<<<" AT 15,1

RETURN 0

END FUNCTION
