#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC734  => Cliente WS para envio de confirmacion de saldos           #
#Fecha creacion    => 30 DE  Noviembre DE 2011                                  #
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
   m_hora        CHAR(8),
   mr_param      RECORD LIKE safre_af:seg_modulo.*,
   m_enter       CHAR(1),
   m_cuantos,
   m_error       SMALLINT,
   m_log         VARCHAR(30),
   m_char        CHAR(1000)

MAIN
   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I
           DEFER INTERRUPT

WHENEVER ERROR STOP

CALL f_inicio()

LET m_log = m_usuario CLIPPED, ".RETC734.log"
CALL STARTLOG(m_log CLIPPED)

  CALL f_ventana()
  CALL f_menu()

   CLOSE WINDOW win1
END MAIN

#################################################################################
FUNCTION f_inicio()
--inicializa variables
LET m_hoy   = TODAY   LET m_hora  = TIME
LET m_log   = " "     LET m_enter = ""
LET m_cuantos = 0     LET m_error = 0
LET m_char    = " "   LET m_cuantos = 0

INITIALIZE mr_param.*, m_usuario TO NULL

--Parametros globales y usuario que ejecuta
SELECT a.*,USER INTO mr_param.*, m_usuario
FROM safre_af:seg_modulo a
WHERE a.modulo_cod = "ret"

END FUNCTION

#################################################################################
FUNCTION f_menu()

MENU "EXTEMPORANEAS"

     COMMAND "Enviar" "Enviar Extemporaneas y Especiales"
        CALL f_principal()
     COMMAND "Salir" "Salir del Programa"
        EXIT PROGRAM

END MENU

END FUNCTION

#################################################################################
FUNCTION f_principal()

    CLEAR WINDOW win1
    CALL f_cuenta()
    WHILE TRUE
      PROMPT "¿DESEA EJECUTAR EL ENVIO? (S/N)" FOR CHAR m_enter
      IF m_enter MATCHES "[SsNn]" THEN
        IF m_enter MATCHES "[Ss]" THEN
           CALL f_WS()
           EXIT WHILE
        ELSE
           ERROR "PROCESO CANCELADO" SLEEP 2
           CLEAR WINDOW win1
           ERROR ""
           DISPLAY "                                                                         " AT 3,1
           RETURN
        END IF   
      ELSE
        ERROR "SOLO PRESIONE S o N"
      END IF  
    END WHILE

END FUNCTION

#################################################################################
FUNCTION f_ventana()

    OPEN WINDOW win1 AT 2,2 WITH 24 ROWS, 73 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETC734      ENVIO EXTEMPORANEAS VENTANILLA UNICA 2.5        ",m_hoy USING "DD-MM-YYYY" AT 5,1 ATTRIBUTE(REVERSE)
    DISPLAY "       [Esc]Enviar                                    [Ctrl-C]Salir     " AT 4,1 ATTRIBUTE(REVERSE)  

END FUNCTION

#################################################################################
FUNCTION f_cuenta()

LET m_cuantos  = 0

    SELECT COUNT(*) INTO m_cuantos
    FROM ret_extemporanea a
    WHERE (a.diag_envio_procesar IS NULL OR a.diag_envio_procesar = 0)

    IF m_cuantos = 0 THEN
       ERROR "NO EXISTEN REGISTROS A ENVIAR"
       SLEEP 2
       ERROR ""
       EXIT PROGRAM
    END IF

    DISPLAY "REGISTROS A ENVIAR: ",m_cuantos AT 13,2

END FUNCTION

#################################################################################
FUNCTION f_WS()
DEFINE r_ext      RECORD LIKE safre_af:ret_extemporanea.*,
       l_cuenta   INTEGER

     ERROR "PROCESANDO INFORMACION"

WHENEVER ERROR CONTINUE
   LET m_char = "touch axis.log; rm -f axis.log; rm -f ",m_usuario CLIPPED,".axis.log;",
                "touch ",m_usuario CLIPPED,".ws3_vu_out.log;rm -f ",m_usuario CLIPPED,".ws3_vu_out.log;" ,
                "touch ",m_usuario CLIPPED,".ws3_vu_err.log;rm -f ",m_usuario CLIPPED,".ws3_vu_err.log "
   LET m_char = m_char CLIPPED
   RUN m_char
WHENEVER ERROR STOP

   LET l_cuenta = 0
   DECLARE cur1 CURSOR FOR
    SELECT * FROM safre_af:ret_extemporanea a
    WHERE (a.diag_envio_procesar IS NULL OR a.diag_envio_procesar = 0)

     INITIALIZE r_ext.* TO NULL
     FOREACH cur1 INTO r_ext.*
      IF r_ext.sec_pension IS NULL OR r_ext.sec_pension = "" OR
         r_ext.sec_pension = " " THEN
         LET r_ext.sec_pension = 0
      END IF

      LET l_cuenta = l_cuenta + 1
           LET m_char = ""
           LET m_char = "java -jar /safre/java/ClientExtemporaneousContributions.jar ",
                        r_ext.nss                , " ",
                        r_ext.sec_pension        , " ",
                        r_ext.periodo_pago       , " ",
                        r_ext.id_tramite         , " ",
                        r_ext.consecutivo_lote   , " ",
                        " 1> ws3_vu_out.log 2> ws3_vu_err.log"

         --actualiza indicador a enviado
           UPDATE safre_af:ret_extemporanea SET diag_envio_procesar = 0, --enviado
                                                fecha_envio = TODAY,
                                                estado_registro = 4      --enviado
           WHERE nss = r_ext.nss
           AND consecutivo_lote = r_ext.consecutivo_lote 
           AND (diag_envio_procesar IS NULL OR diag_envio_procesar = 0)

        DISPLAY "REGISTROS ENVIADOS: ",l_cuenta AT 14,2

        WHENEVER ERROR CONTINUE
           LET m_char = m_char CLIPPED
           DISPLAY "RUN: ", m_char  sleep 2
           RUN m_char

           LET m_char = "touch axis.log;cat axis.log >>",m_usuario CLIPPED,".axis.log; rm -f axis.log"
           LET m_char = m_char CLIPPED
           --DISPLAY "RUN1: ", m_char  sleep 2
           RUN m_char

           LET m_char = "touch ws3_vu_out.log;cat ws3_vu_out.log >>",m_usuario CLIPPED,".ws3_vu_out.log; ",
                        " touch ws3_vu_err.log; cat ws3_vu_err.log >>",m_usuario CLIPPED,".ws3_vu_err.log" 
           LET m_char = m_char CLIPPED
           --DISPLAY "RUN2: ", m_char  sleep 2
           RUN m_char
        WHENEVER ERROR STOP

         INITIALIZE r_ext.* TO NULL
     END FOREACH 

ERROR "FINALIZA PROCESO" SLEEP 2
ERROR " "

END FUNCTION

#################################################################################
