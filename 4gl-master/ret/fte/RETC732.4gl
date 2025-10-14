#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC732  => Cliente WS para envio de confirmacion de saldos           #
#Fecha creacion    => 15 DE  Noviembre DE 2011                                  #
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
   m_oper        CHAR(1),        --tipo de operacion transfer/disposicion
   m_estados ,                   --estados de datamart (provisionado o liquidado)
   m_tipos_ret   ,               --tipos de retiro 
   m_tipos_mov,                  --tipos de movimiento
   m_fld_tipo    VARCHAR (50),   --campo indicador
   m_query       CHAR(4500)
   
MAIN
   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I
           DEFER INTERRUPT

WHENEVER ERROR STOP

CALL f_inicio()

LET m_log = m_usuario CLIPPED, ".RETC732.log"
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
LET m_oper    = ""    LET m_estados  = "" 
LET m_tipos_ret   = ""    LET m_fld_tipo = ""
LET m_query   = ""    LET m_tipos_mov = ""  

INITIALIZE mr_param.*, m_usuario TO NULL

--Parametros globales y usuario que ejecuta
SELECT a.*,USER INTO mr_param.*, m_usuario
FROM safre_af:seg_modulo a
WHERE a.modulo_cod = "ret"

END FUNCTION

#################################################################################
FUNCTION f_menu()

MENU "CONFIRMACION"

     COMMAND "Transferencia" "Enviar Saldos de Transferencia"
        LET m_oper = "T"
        CALL f_principal()
     COMMAND "Disposicion" "Enviar Saldos de Disposicion"
        LET m_oper = "D"
        CALL f_principal() 
     COMMAND "Salir" "Salir del Programa"

   EXIT PROGRAM

END MENU


END FUNCTION

#################################################################################
FUNCTION f_principal()
DEFINE l_desc VARCHAR (20)

  IF m_oper = "T" THEN
  	 LET l_desc = "TRANSFERENCIA"
  ELSE
     LET l_desc = "DISPOSICION  "
  END IF	 

DISPLAY "       [Esc]Envio ", l_desc,"                  [Ctrl-C]Salir            " AT 3,1 ATTRIBUTE(REVERSE)  

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
      DISPLAY "                                                                         " AT 3,1
      RETURN

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
           DISPLAY "                                                                         " AT 3,1
           RETURN
        END IF   
      ELSE
        ERROR "SOLO PRESIONE S o N"
      END IF  
    END WHILE

DISPLAY "                                                                         " AT 3,1

END FUNCTION

#################################################################################
FUNCTION f_ventana()

    OPEN WINDOW win1 AT 2,2 WITH FORM "RETC7151" ATTRIBUTE(BORDER)
    DISPLAY "                                                                         " AT 3,1
    DISPLAY " RETC732        ENVIO CONFIRMACION VENTANILLA UNICA 2.5        ",m_hoy USING "DD-MM-YYYY" AT 5,1 ATTRIBUTE(REVERSE)
    --DISPLAY "       [Esc]Consulta                                   [Ctrl-C]Salir     " AT 1,1 ATTRIBUTE(REVERSE)  

END FUNCTION

#################################################################################
FUNCTION f_cuenta()

LET m_char = " "     LET m_cuantos  = 0

CALL f_constantes()

LET m_char =" SELECT COUNT(*)",
            " FROM ret_det_datamart a, dis_provision b",
            " WHERE b.folio = ",m_folio,
            " AND a.nss = b.nss",
            " AND a.id_registro = b.consecutivo_lote ",
            " AND b.tipo_movimiento  IN ",m_tipos_mov,
            --" AND a.estado IN ",m_estados,
            " AND a.tipo_retiro IN ",m_tipos_ret,
            " AND (",m_fld_tipo," IS NULL OR ",m_fld_tipo," = 0)"

 LET m_char = m_char CLIPPED
 --DISPLAY "CUENTA ",m_char

 PREPARE p_count FROM m_char
 EXECUTE p_count INTO m_cuantos

RETURN m_cuantos

END FUNCTION

#################################################################################
FUNCTION f_WS()
DEFINE confirma RECORD
	         id             INTEGER,
           nss            CHAR(11),        #nss             
           curp           CHAR(18),        #curp            
           sec_pension    CHAR(2),         #secuencia de pension
           cmp1,                  #Retiro 97 Trans
           cmp2,                  #Cuota Social Trans
           cmp3,                  #CV, C. Esp y Est Trans
           cmp4,                  #Viv97 Trans  
           cmp5,                  #Voluntarias Trans
           cmp6,                  #Ret ISSSTE BANXICO Trans
           cmp7,                  #Ret92 ISSSTE Trans
           cmp8,                  #Complementarias de Retiro Trans
           cmp9,                  #Viv FOVISSSTE 92 Trans
           cmp10,                 #Largo Plazo Trans
           cmp11,                 #FOVISSSTE 2008 Trans
           cmp12,                 #Ret ISSSTE 2008 Trans
           cmp13,                 #CV ISSSTE Trans
           cmp14,                 #Ahorro Solidario Trans
           cmp15,                 #Cuota Social ISSSTE Trans
           cmp16,                 #Retiro 97 Disp
           cmp17,                 #Cuota Social Disp
           cmp18,                 #Cesantía y Vejez, Cuota Especial y Estatal Disp
           cmp19,                 #Vivienda 97 Disp
           cmp20,                 #Aportaciones Voluntarias Disp
           cmp21,                 #Retiro 92 IMSS Disp
           cmp22,                 #Vivienda 92 IMSS Disp
           cmp23,                 #Ahorro para el retiro ISSSTE BANXICO Disp
           cmp24,                 #Retiro 92 ISSSTE Disp
           cmp25,                 #Aportaciones Complementarias de Retiro Disp
           cmp26,                 #Vivienda FOVISSSTE 92 Disp
           cmp27,                 #Aportaciones a Largo Plazo Disp
           cmp28,                 #FOVISSSTE 2008 Disp
           cmp29,                 #Retiro ISSSTE 2008 Disp
           cmp30,                 #CV ISSSTE Disp
           cmp31,                 #Ahorro Solidario Disp
           cmp32 DECIMAL (13,2)   #Cuota Social ISSSTE Disp
       END RECORD,
           l_subcta VARCHAR(100),
           l_campo  VARCHAR(20),
           l_long   SMALLINT

     LET l_subcta = ""   LET m_char = " "
     ERROR "PROCESANDO INFORMACION"

WHENEVER ERROR CONTINUE
   LET m_char = "touch axis.log; rm -f axis.log; rm -f ",m_usuario CLIPPED,".axis.log"
   LET m_char = m_char CLIPPED
   RUN m_char
WHENEVER ERROR STOP
    LET m_char = " "  LET m_query = " "
    CALL f_constantes()
    #arma query llamando a la funcion por cada subcuenta y lo concatena para el prepare
     LET m_query = 
      " SELECT a.id_registro, a.nss, a.curp, a.sec_pension,",
        f_query("1",1),                  ",",  #retiro 97 trans
        f_query("5",1),                  ",",  #Cuota Social Trans
        f_query("2,6,9",1),              ",",  #CV, C. Esp y Est Trans                         
        f_query("4",1),                  ",",  #Viv97 Trans       
        f_query("3,10,23",1),            ",",  #Voluntarias Trans 
        f_query("19",1),                 ",",  #Ret ISSSTE BANXICO Trans                       
        f_query("13",1),                 ",",  #Ret92 ISSSTE Trans
        f_query("11,12",1),              ",",  #Complementarias de Retiro Trans                
        f_query("14",1),                 ",",  #Viv FOVISSSTE 92 Trans                         
        f_query("15,16,26,27,28,29",1),  ",",  #Largo Plazo Trans 
        f_query("35",1),                 ",",  #FOVISSSTE 2008 Trans                           
        f_query("30",1),                 ",",  #Ret ISSSTE 2008 Trans                          
        f_query("31,39",1),              ",",  #CV ISSSTE Trans   
        f_query("33,34",1),              ",",  #Ahorro Solidario Trans                         
        f_query("32",1),                 ",",  #Cuota Social ISSSTE Trans                      
        f_query("1",2),                  ",",  #Retiro 97 Disp    
        f_query("5",2),                  ",",  #Cuota Social Disp 
        f_query("2,6,9",2),              ",",  #Cesantía y Vejez, Cuota Especial y Estatal Disp
        f_query("4",2),                  ",",  #Vivienda 97 Disp  
        f_query("3,10,23",2),            ",",  #Aportaciones Voluntarias Disp                  
        f_query("7",2),                  ",",  #Retiro 92 IMSS Disp                            
        f_query("8",2),                  ",",  #Vivienda 92 IMSS Disp                          
        f_query("19",2),                 ",",  #Ahorro para el retiro ISSSTE BANXICO Disp      
        f_query("13",2),                 ",",  #Retiro 92 ISSSTE Disp                          
        f_query("11,12",2),              ",",  #Aportaciones Complementarias de Retiro Disp    
        f_query("14",2),                 ",",  #Vivienda FOVISSSTE 92 Disp                     
        f_query("15,16,26,27,28,29",2),  ",",  #Aportaciones a Largo Plazo Disp                
        f_query("35",2),                 ",",  #FOVISSSTE 2008 Disp                            
        f_query("30",2),                 ",",  #Retiro ISSSTE 2008 Disp                        
        f_query("31,39",2),              ",",  #CV ISSSTE Disp    
        f_query("33,34",2),              ",",  #Ahorro Solidario Disp                          
        f_query("32",2)                    ,   #Cuota Social ISSSTE Disp                       
        " FROM ret_det_datamart a , dis_provision b",
        " WHERE (",m_fld_tipo," IS NULL OR ",m_fld_tipo," = 0)",
        " AND b.folio = ",m_folio,
        " AND a.nss = b.nss",
        " AND a.id_registro = b.consecutivo_lote ",
        " AND b.tipo_movimiento  IN ",m_tipos_mov,
        " AND a.tipo_retiro IN ",m_tipos_ret
        
        LET m_query = m_query CLIPPED
        --DISPLAY "s_query "  DISPLAY m_query SLEEP 3

   PREPARE p_query FROM m_query
   DECLARE cur1 CURSOR FOR p_query
     INITIALIZE confirma.* TO NULL
      FOREACH cur1 INTO confirma.*
        LET l_campo = " "   LET l_long = 0
        IF confirma.curp = "" OR confirma.curp = " " OR confirma.curp IS NULL
           OR  LENGTH(confirma.curp) != 18 THEN
           LET confirma.curp = "0"
        END IF
         LET l_long = LENGTH(m_fld_tipo CLIPPED)
         LET l_campo = m_fld_tipo[3,l_long]   -- se kita el 'a.'
         LET m_char = "java -jar /safre/java/ClientConfirmationRequest.jar ",
                      confirma.nss,   " ", confirma.curp,  " ",
                      confirma.sec_pension, " ",
                      confirma.cmp1,  " ", confirma.cmp2,  " ",
                      confirma.cmp3,  " ", confirma.cmp4,  " ",
                      confirma.cmp5,  " ", confirma.cmp6,  " ",
                      confirma.cmp7,  " ", confirma.cmp8,  " ",
                      confirma.cmp9,  " ", confirma.cmp10, " ",
                      confirma.cmp11, " ", confirma.cmp12, " ",
                      confirma.cmp13, " ", confirma.cmp14, " ",
                      confirma.cmp15, " ", confirma.cmp16, " ",
                      confirma.cmp17, " ", confirma.cmp18, " ",
                      confirma.cmp19, " ", confirma.cmp20, " ",
                      confirma.cmp21, " ", confirma.cmp22, " ",
                      confirma.cmp23, " ", confirma.cmp24, " ",
                      confirma.cmp25, " ", confirma.cmp26, " ",
                      confirma.cmp27, " ", confirma.cmp28, " ",
                      confirma.cmp29, " ", confirma.cmp30, " ",
                      confirma.cmp31, " ", confirma.cmp32, " 0", --clabe
                      confirma.id,    " ", l_campo,
                      " 1> ws2_vu_out.log 2> ws2_vu_err.log"

       --actualiza indicador a enviado
         LET m_query = " "
         LET m_query = 
         " UPDATE safre_af:ret_det_datamart SET ",l_campo," = 0", --enviado
         " WHERE id_registro = ",confirma.id,
         " AND (",l_campo," IS NULL OR ",l_campo," = 0)"

         LET m_query = m_query CLIPPED
         PREPARE p_upd_dtm FROM m_query
         EXECUTE p_upd_dtm

      WHENEVER ERROR CONTINUE
         LET m_char = m_char CLIPPED
         --DISPLAY "RUN: ", m_char  --sleep 2
         RUN m_char

         LET m_char = "touch axis.log;cat axis.log >>",m_usuario CLIPPED,".axis.log; rm -f axis.log"
         LET m_char = m_char CLIPPED
         --DISPLAY "RUN1: ", m_char  sleep 2
         RUN m_char

         LET m_char = "touch ws2_vu_out.log;cat ws2_vu_out.log >>",m_usuario CLIPPED,".ws2_vu_out.log; ",
                      " touch ws2_vu_err.log; cat ws2_vu_err.log >>",m_usuario CLIPPED,".ws2_vu_err.log" 
         LET m_char = m_char CLIPPED
         --DISPLAY "RUN2: ", m_char  sleep 2
         RUN m_char
      WHENEVER ERROR STOP

       INITIALIZE confirma.* TO NULL
END FOREACH 

ERROR "FINALIZA PROCESO" SLEEP 2
ERROR " "

END FUNCTION

#################################################################################
FUNCTION f_constantes()

   LET m_estados  = " " LET m_tipos_ret = " "
   LET m_fld_tipo = " " LET m_tipos_mov = " "

   IF m_oper = "T" THEN -- transferencia
      LET m_estados   = "(116,118)"       --preliquidado, liquidado DERECHO
      LET m_tipos_mov = "(800,810,815)"
      LET m_fld_tipo  = "a.ind_env_recep_trans"
   ELSE                 -- disposicion
      LET m_estados  = "(132,134)"       --preliquidado, liquidado DISPOSICION
      LET m_fld_tipo = "a.ind_env_recep_disp"
      LET m_tipos_mov = "(820,830)"
   END IF

      LET m_tipos_ret = "('A','B','C')"
END FUNCTION

#################################################################################
# l_ind_oper = 1 --> transferencia, l_ind_oper = 2 --> disposicion
#################################################################################
FUNCTION f_query(l_subcta, l_ind_oper)
DEFINE    l_ind_oper    SMALLINT,
          l_subcta      VARCHAR(100),
          l_cadena      VARCHAR(255)

 LET l_cadena = " "

  IF m_oper = "T" AND l_ind_oper = 2 THEN
  	 LET l_cadena = " (0) "
     RETURN l_cadena
  END IF

  IF m_oper = "D" AND l_ind_oper = 1 THEN
  	 LET l_cadena = " (0) "
     RETURN l_cadena
  END IF

 LET l_cadena = " (SELECT NVL(SUM(b.monto_en_pesos),0)",
                " FROM dis_provision b ",
                " WHERE b.folio = ",m_folio,
                " AND b.consecutivo_lote = a.id_registro ",
                " AND b.nss = a.nss ",
                " AND b.subcuenta IN (",l_subcta,")",
                " AND b.tipo_movimiento IN ",m_tipos_mov,") "

RETURN l_cadena

END FUNCTION
