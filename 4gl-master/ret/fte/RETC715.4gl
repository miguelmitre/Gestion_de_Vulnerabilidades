#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC715  => Cliente WS para envio de saldos previos                   #
#Fecha creacion    => 03 DE  Noviembre DE 2011                                  #
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
   m_char        CHAR(1000),
   m_folio       INTEGER,
   m_query       CHAR(7000)
   
MAIN
   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I
           DEFER INTERRUPT

WHENEVER ERROR STOP

CALL f_inicio()

LET m_log = m_usuario CLIPPED, ".RETC715.log"
CALL STARTLOG(m_log CLIPPED)

  CALL f_principal()

END MAIN

#################################################################################
FUNCTION f_inicio()
--inicializa variables
LET m_hoy   = TODAY
LET m_hora  = TIME
LET m_log   = " "
LET m_enter = ""
LET m_cuantos = 0
LET m_char = " "
LET m_error = 0
LET m_cuantos = 0
LET m_query = " "

INITIALIZE mr_param.*, m_usuario TO NULL

--Parametros globales y usuario que ejecuta
SELECT a.*,USER INTO mr_param.*, m_usuario
FROM safre_af:seg_modulo a
WHERE a.modulo_cod = "ret"

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
      PROMPT "¿DESEA EJECUTAR EL ENVIO DE SALDOS? (S/N)" FOR CHAR m_enter
      IF m_enter MATCHES "[SsNn]" THEN
        IF m_enter MATCHES "[Ss]" THEN
           CALL f_WS()
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
    DISPLAY " RETC715       ENVIO SALDOS PREVIOS VENTANILLA UNICA 2.5       ",m_hoy USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "       [Esc]Consulta                                   [Ctrl-C]Salir     " AT 1,1 ATTRIBUTE(REVERSE)  

END FUNCTION

#################################################################################
FUNCTION f_cuenta()

LET m_cuantos = 0

  SELECT COUNT(*) INTO m_cuantos
  FROM ret_solicitud_saldo a
  WHERE a.folio_cargo = m_folio
  AND a.estado_solicitud IN (104,106)     --preliquidado, liquidado SALDO
  AND (a.ind_recep_procesar IS NULL OR a.ind_recep_procesar = 0)

RETURN m_cuantos

END FUNCTION

#################################################################################
FUNCTION f_WS()
DEFINE saldo RECORD 
           nss   CHAR(11),        #nss             
           curp  CHAR(18),        #curp
           diag  SMALLINT,        #diagnostico
           cmp1,                  #retiro97        
           cmp2,                  #cuota_social    
           cmp3,                  #cv_esp_est      
           cmp4,                  #viv97           
           cmp5,                  #voluntarias     
           cmp6,                  #retiro92        
           cmp7,                  #viv92           
           cmp8,                  #ret_iss_bnx     
           cmp9,                  #ret92_iss       
           cmp10,                 #aport_compl     
           cmp11,                 #viv_fov92       
           cmp12,                 #aport_lp        
           cmp13,                 #fovi08          
           cmp14,                 #ret_iss08       
           cmp15,                 #cv_iss          
           cmp16,                 #ahor_sol        
           cmp17,                 #cs_iss          
           cmp18,                 #retiro97-fip    
           cmp19,                 #cuota_social-fip
           cmp20,                 #cv_esp_est-fip  
           cmp21,                 #viv97-fip       
           cmp22,                 #voluntarias-fip 
           cmp23,                 #retiro92-fip    
           cmp24,                 #ret_iss_bnx-fip 
           cmp25,                 #aport_compl-fip 
           cmp26,                 #viv_fov92-fip   
           cmp27,                 #aport_lp-fip    
           cmp28,                 #fovi08-fip      
           cmp29,                 #ret_iss08-fip   
           cmp30,                 #cv_iss-fip      
           cmp31,                 #ahor_sol-fip    
           cmp32 DECIMAL (13,2)   #cs_iss-fip
       END RECORD

ERROR "PROCESANDO INFORMACION"

WHENEVER ERROR CONTINUE
   LET m_char = "touch axis.log; rm -f axis.log; rm -f ",m_usuario CLIPPED,".axis.log; ",
                "touch ",m_usuario CLIPPED,".ws1_vu_out.log;rm -f ",m_usuario CLIPPED,".ws1_vu_out.log; ",
                "touch ",m_usuario CLIPPED,".ws1_vu_err.log;rm -f ",m_usuario CLIPPED,".ws1_vu_err.log"

   LET m_char = m_char CLIPPED
   RUN m_char
WHENEVER ERROR STOP

LET m_query = "SELECT a.nss, curp, a.diag_envio_procesar,",
               f_query(1,"1") CLIPPED,                 ",",   #retiro97        
               f_query(1,"5") CLIPPED,                 ",",   #cuota_social    
               f_query(1,"2,6,9") CLIPPED,             ",",   #cv_esp_est      
               f_query(1,"4") CLIPPED,                 ",",   #viv97           
               f_query(1,"3,10,23") CLIPPED,           ",",   #voluntarias     
               f_query(1,"7") CLIPPED,                 ",",   #retiro92        
               f_query(1,"8") CLIPPED,                 ",",   #viv92           
               f_query(1,"19") CLIPPED,                ",",   #ret_iss_bnx     
               f_query(1,"13") CLIPPED,                ",",   #ret92_iss       
               f_query(1,"11,12") CLIPPED,             ",",   #aport_compl     
               f_query(1,"14") CLIPPED,                ",",   #viv_fov92       
               f_query(1,"15,16,26,27,28,29") CLIPPED, ",",   #aport_lp        
               f_query(1,"35") CLIPPED,                ",",   #fovi08          
               f_query(1,"30") CLIPPED,                ",",   #ret_iss08       
               f_query(1,"31,39") CLIPPED,             ",",   #cv_iss          
               f_query(1,"33,34") CLIPPED,             ",",   #ahor_sol        
               f_query(1,"32") CLIPPED,                ",",   #cs_iss          
               f_query(2,"1") CLIPPED,                 ",",   #retiro97-fip    
               f_query(2,"5") CLIPPED,                 ",",   #cuota_social-fip
               f_query(2,"2,6,9") CLIPPED,             ",",   #cv_esp_est-fip  
               f_query(2,"4") CLIPPED,                 ",",   #viv97-fip       
               f_query(2,"3,10,23") CLIPPED,           ",",   #voluntarias-fip 
               f_query(2,"7") CLIPPED,                 ",",   #retiro92-fip    
               f_query(2,"19") CLIPPED,                ",",   #ret_iss_bnx-fip 
               f_query(2,"11,12") CLIPPED,             ",",   #aport_compl-fip 
               f_query(2,"14") CLIPPED,                ",",   #viv_fov92-fip   
               f_query(2,"15,16,26,27,28,29") CLIPPED, ",",   #aport_lp-fip    
               f_query(2,"35") CLIPPED,                ",",   #fovi08-fip      
               f_query(2,"30") CLIPPED,                ",",   #ret_iss08-fip   
               f_query(2,"31,39") CLIPPED,             ",",   #cv_iss-fip      
               f_query(2,"33,34") CLIPPED,             ",",   #ahor_sol-fip    
               f_query(2,"32") CLIPPED,                       #cs_iss-fip      
              " FROM ret_solicitud_saldo a ",
              " WHERE a.folio_cargo = ",m_folio,
              " AND (a.ind_recep_procesar IS NULL OR a.ind_recep_procesar = 0) "

     LET m_query = m_query CLIPPED 
     --DISPLAY m_query SLEEP 2
     PREPARE p_saldo FROM m_query
     DECLARE cur1 CURSOR FOR p_saldo
      INITIALIZE saldo.* TO NULL
      FOREACH cur1 INTO saldo.*

        IF saldo.curp = "" OR saldo.curp = " " OR saldo.curp IS NULL
           OR  LENGTH(saldo.curp) != 18 THEN
           LET saldo.curp = "0"
        END IF

         LET m_char = "java -jar /safre/java/ClientPreviousBalance.jar ",
                      saldo.nss,   " ", saldo.curp,  " ",
                      saldo.cmp1,  " ", saldo.cmp2,  " ",
                      saldo.cmp3,  " ", saldo.cmp4,  " ",
                      saldo.cmp5,  " ", saldo.cmp6,  " ",
                      saldo.cmp7,  " ", saldo.cmp8,  " ",
                      saldo.cmp9,  " ", saldo.cmp10, " ",
                      saldo.cmp11, " ", saldo.cmp12, " ",
                      saldo.cmp13, " ", saldo.cmp14, " ",
                      saldo.cmp15, " ", saldo.cmp16, " ",
                      saldo.cmp17, " ", saldo.cmp18, " ",
                      saldo.cmp19, " ", saldo.cmp20, " ",
                      saldo.cmp21, " ", saldo.cmp22, " ",
                      saldo.cmp23, " ", saldo.cmp24, " ",
                      saldo.cmp25, " ", saldo.cmp26, " ",
                      saldo.cmp27, " ", saldo.cmp28, " ",
                      saldo.cmp29, " ", saldo.cmp30, " ",
                      saldo.cmp31, " ", saldo.cmp32, " ",
                      saldo.diag,  " 1> ws1_vu_out.log 2> ws1_vu_err.log"

       --actualiza indicador a enviado
         UPDATE safre_af:ret_solicitud_saldo
            SET ind_recep_procesar = 0       --enviado
           WHERE nss = saldo.nss
            AND  folio_cargo = m_folio
            AND (ind_recep_procesar IS NULL OR ind_recep_procesar = 0)

      WHENEVER ERROR CONTINUE
         LET m_char = m_char CLIPPED
         --DISPLAY "RUN: ", m_char -- sleep 2
         RUN m_char

         LET m_char = "touch axis.log;cat axis.log >>",m_usuario CLIPPED,".axis.log; rm -f axis.log"
         LET m_char = m_char CLIPPED
         --DISPLAY "RUN1: ", m_char  sleep 2
         RUN m_char

         LET m_char = "touch ws1_vu_out.log;cat ws1_vu_out.log >>",m_usuario CLIPPED,".ws1_vu_out.log; ",
                      "touch ws1_vu_err.log; cat ws1_vu_err.log >>",m_usuario CLIPPED,".ws1_vu_err.log" 
         LET m_char = m_char CLIPPED
         --DISPLAY "RUN2: ", m_char  sleep 2
         RUN m_char
      WHENEVER ERROR STOP

       INITIALIZE saldo.* TO NULL
END FOREACH 

ERROR "FINALIZA PROCESO" SLEEP 2
ERROR " "

END FUNCTION

#################################################################################
FUNCTION f_query(p_ind,p_subcta)
DEFINE   p_ind       SMALLINT,
         p_subcta    VARCHAR(20),
         l_cadena    CHAR(500)
       
LET l_cadena = ""
#Se modifica para cambiar de signo los saldos, deben ser positivos.
IF p_ind = 1 THEN --saldo actual
   LET l_cadena = " (SELECT NVL(SUM(-b.monto_en_pesos),0) ",
                  " FROM ret_preliquida b",
                  " WHERE b.folio = a.folio_cargo ",
                  " AND b.consecutivo_lote = a.id_solicitud_saldo ",
                  " AND b.nss = a.nss",
                  " AND b.subcuenta IN (",p_subcta,")) "
ELSE              --saldo fip
   LET l_cadena = " (SELECT NVL(SUM(c.mto_en_pesos),0) ",
                  " FROM ret_mto_solicitud_saldo c",
                  " WHERE c.folio_cargo = a.folio_cargo",
                  " AND c.id_solicitud_saldo = a.id_solicitud_saldo",
                  " AND c.nss = a.nss",
                  " AND c.subcuenta IN (",p_subcta,")) "
END IF

LET l_cadena = l_cadena CLIPPED

RETURN l_cadena

END FUNCTION

