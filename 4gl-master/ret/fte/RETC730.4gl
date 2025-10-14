#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC730  => Pantalla de ejecucion de preliquidacion de Disposicion    #
#Fecha creacion    => 09 DE  Diciembre DE 2011                                  #
#By                => ALEJANDRO CHAGOYA SALAZAR                                 #
#Fecha actualiz.   =>                                                           #
#Actualizacion     =>                                                           #
#                  =>                                                           #
#Sistema           => ret                                                       #
#################################################################################
DATABASE safre_af
DEFINE 
   m_usuario     VARCHAR(10),
   m_hoy         DATE,
   mr_param      RECORD LIKE safre_af:seg_modulo.*,
   m_enter       CHAR(1),
   m_cuantos,
   m_log         VARCHAR(30),
   m_char        CHAR(500),
   m_folio       INTEGER,
   mr_edo RECORD
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

LET m_log = m_usuario CLIPPED, ".RETC730.log"
CALL STARTLOG(m_log CLIPPED)

  CALL f_principal()

END MAIN

#################################################################################
FUNCTION f_inicio()
--inicializa variables
LET m_hoy   = TODAY
LET m_log   = " "
LET m_enter = ""
LET m_cuantos = 0
LET m_char = ""

INITIALIZE mr_param.*, m_usuario, mr_edo.* TO NULL

--Parametros globales y usuario que ejecuta
   SELECT a.*,USER INTO mr_param.*, m_usuario
   FROM safre_af:seg_modulo a
   WHERE a.modulo_cod = "ret"

--Estados
    SELECT A.estado_solicitud
    INTO   mr_edo.provisionado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROVISIONADO DISPOSICION"   #130

    SELECT A.estado_solicitud
    INTO   mr_edo.preliquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "PRELIQUIDADO DISPOSICION"   #132

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
    DISPLAY " RETC730    PRELIQUIDACION DISPOSICION VENTANILLA UNICA 2.5    ",m_hoy USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " [Esc]Ejecuta Preliquidacion                           [Ctrl-C]Salir     " AT 1,1 ATTRIBUTE(REVERSE)  

END FUNCTION

#################################################################################
FUNCTION f_cuenta()

LET m_cuantos = 0

  SELECT COUNT(*) INTO m_cuantos
  FROM  safre_af:ret_solicitud_tx a
  WHERE a.folio = m_folio
  AND   a.estado_solicitud = mr_edo.provisionado

RETURN m_cuantos

END FUNCTION

#################################################################################
FUNCTION f_preliquida()
DEFINE r_pre RECORD LIKE safre_af:ret_preliquida.*

ERROR "PROCESANDO INFORMACION"

 DECLARE cur_pre CURSOR FOR
   SELECT * FROM dis_provision d
   WHERE d.folio = m_folio

INITIALIZE r_pre.* TO NULL
   FOREACH cur_pre INTO r_pre.*
     INSERT INTO safre_af:ret_preliquida VALUES(r_pre.*)

     IF SQLCA.SQLERRD[3] = 0 THEN  #Si no inserto NADA
        LET m_char = ""
        LET m_char = "ERROR AL INSERTAR EN ret_preliquida",r_pre.*
        CALL ERRORLOG(m_char CLIPPED)
     END IF

     UPDATE safre_af:ret_solicitud_tx SET estado_solicitud = mr_edo.preliquidado
     WHERE nss  = r_pre.nss
     AND folio  = m_folio
     AND estado_solicitud = mr_edo.provisionado

    INITIALIZE r_pre.* TO NULL
   END FOREACH 

PROMPT " PROCESO TERMINADO SATISFACTORIAMENTE ... <ENTER> PARA SALIR " FOR CHAR m_enter

END FUNCTION

#################################################################################
