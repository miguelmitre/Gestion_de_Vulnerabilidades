#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC716  => BITACORA VENTANILLA UNICA 2.5                             #
#Fecha creacion    => 19 DE DICIEMBRE DE 2011                                   #
#By                => ALEJANDRO CHAGOYA SALAZAR                                 #
#Fecha actualiz.   =>                                                           #
#Actualizacion     =>                                                           #
#                  =>                                                           #
#Sistema           => ret                                                       #
#################################################################################
DATABASE safre_af

 DEFINE
   m_usuario     VARCHAR(10),
   m_xtype       SMALLINT,
   m_afore       SMALLINT,
   m_raz_soc     CHAR(50),
   m_hoy         DATE,
   m_hora        CHAR(8),
   mr_param      RECORD LIKE safre_af:seg_modulo.*,
   m_enter       CHAR(1),
   m_impre       CHAR(300),
   m_cuantos,
   m_error       SMALLINT,
   m_log         VARCHAR(30),
   mr_busca      RECORD            #Variables de Filtro
     nss          CHAR(11),
     curp         CHAR(18),
     fec_ini      DATE,
     fec_fin      DATE
  END RECORD,
  mr_busca_sp RECORD               #Variables de Filtro para el WS
     nss         CHAR(11),
     curp        CHAR(18),
     fec_ini     CHAR(10),
     fec_fin     CHAR(10)
  END RECORD,
  m_filtro       SMALLINT,        #1=filtro1, 2=filtro2
  m_char         CHAR(500)

  ##Variables de consulta 2            
DEFINE m_registros   INTEGER,              #count de la tabla 
       m_paginas     SMALLINT,             #total de paginas
       m_pag_act     SMALLINT,             #pagina actual
       m_posarray    INTEGER,              #posision del arreglo
       mr_datos2    ARRAY[10] OF RECORD    #Arreglo para el display
       	  id             DECIMAL(16,0),
          nss            CHAR(11),
          fec_sol        DATETIME YEAR TO SECOND,
          diag_afore     CHAR(3),
          diag_procesar  CHAR(3)
       END RECORD,
       m_p_row      SMALLINT,
       m_tabla      VARCHAR(30),
       m_ventana    SMALLINT

MAIN
   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I
           DEFER INTERRUPT

WHENEVER ERROR STOP

CALL f_inicio()

LET m_log = m_usuario CLIPPED, ".RETC716.log"
CALL STARTLOG(m_log CLIPPED)

  CALL f_ventana(1)
  CALL f_menu()
    
  CLOSE WINDOW win1
END MAIN

#################################################################################
FUNCTION f_inicio()
--inicializa variables
LET m_hoy   = TODAY
LET m_hora  = TIME
LET m_log   = " "
LET m_enter = ""
LET m_cuantos = 0
LET m_xtype = 0
LET m_char = " "
LET m_p_row = 0
LET m_error = 0

INITIALIZE mr_param.*, m_usuario, mr_busca.* TO NULL

--Parametros globales y usuario que ejecuta
SELECT a.*,USER INTO mr_param.*, m_usuario
FROM safre_af:seg_modulo a
WHERE a.modulo_cod = "ret"

END FUNCTION

#################################################################################
FUNCTION f_menu()

MENU "BITACORA WS 2.5"

     COMMAND "Solicitud" "Solicitud de Saldo Previo"
              LET m_xtype = 2
              CALL f_input()
     COMMAND "Notificacion" "Notificacion de Saldos"
              LET m_xtype = 3
              CALL f_input()
     COMMAND "Datamart" "Recepcion Datamart"
              LET m_xtype = 4
              CALL f_input()
     COMMAND "Confirmacion" "Transferencia y Disposicion"
              LET m_xtype = 5
              CALL f_input()
     COMMAND KEY ("A") "cAncelacion" "Solicitudes de Cancelacion"
              LET m_xtype = 6
              CALL f_input()
     COMMAND "Extemporaneas" "Extemporaneas y especiales"
              LET m_xtype = 7
              CALL f_input()
     COMMAND KEY ("R")"saliR" "Salir del Programa"
              EXIT PROGRAM

END MENU

END FUNCTION

#################################################################################
FUNCTION f_input()

   CALL f_tabla()   --selecciona tabla
   CALL f_delete()  --borra registros de la tabla seleecionada

INITIALIZE mr_busca.* TO NULL
INPUT mr_busca.* FROM scr_1.* -- WITHOUT DEFAULTS

   AFTER FIELD nss
     LET  m_filtro = 1
     IF mr_busca.nss IS NOT NULL THEN
        IF LENGTH(mr_busca.nss) != 11 THEN
           ERROR "LA LONGITUD DEBE SER DE 11 POSICIONES"
           NEXT FIELD nss
        ELSE
           LET  m_filtro = 1
           EXIT INPUT
        END IF 
     END IF    --NSS NO NULL

    AFTER FIELD curp
     LET  m_filtro = 1
     IF mr_busca.curp IS NOT NULL THEN
        IF LENGTH(mr_busca.curp) != 18 THEN
           ERROR "LA LONGITUD DEBE SER DE 18 POSICIONES"
           NEXT FIELD curp
        ELSE
           LET  m_filtro = 1
           EXIT INPUT   
        END IF 
     END IF    --CURP NO NULL

     AFTER FIELD fec_ini
      LET m_filtro = 2
      IF mr_busca.fec_ini IS NOT NULL THEN
      	 IF mr_busca.fec_ini > TODAY THEN
           ERROR "LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA ACTUAL"
           NEXT FIELD fec_ini
        END IF
         LET m_filtro = 2
         NEXT FIELD fec_fin	
      END IF

     AFTER FIELD fec_fin
      LET m_filtro = 2
      IF mr_busca.fec_ini IS NOT NULL AND mr_busca.fec_fin IS NULL THEN
         ERROR "DEBE INGRESAR LA FECHA FINAL"
         NEXT FIELD fec_fin
      END IF

      IF mr_busca.fec_fin IS NOT NULL AND mr_busca.fec_ini IS NULL THEN
         ERROR "DEBE INGRESAR LA FECHA INICIAL"
         NEXT FIELD fec_ini
      END IF

     IF mr_busca.fec_fin IS NOT NULL THEN 
        IF mr_busca.fec_fin > TODAY THEN
           ERROR "LA FECHA FINAL NO PUEDE SER MAYOR A LA FECHA ACTUAL"
           NEXT FIELD fec_fin
        END IF

        IF mr_busca.fec_fin < mr_busca.fec_ini THEN
           ERROR "LA FECHA FINAL NO PUEDE SER MENOR A LA FECHA INICIAL"
           NEXT FIELD fec_ini
        END IF	 
     END IF

     IF mr_busca.fec_fin IS NULL AND mr_busca.fec_fin IS NULL THEN 
     	 NEXT FIELD nss 
  	 END IF

      NEXT FIELD fec_ini

    AFTER INPUT
      IF NOT FIELD_TOUCHED(scr_1.*) THEN
         ERROR "DEBE INGRESAR ALGUN CRITERIO DE BUSQUEDA"
         NEXT FIELD nss
      END IF

   ON KEY (ESC)
   	--DISPLAY "datos1: ",mr_busca.* SLEEP 4
    --DISPLAY "datos2: ",mr_busca.* SLEEP 4
      IF LENGTH(mr_busca.nss CLIPPED) != 0 THEN
         IF LENGTH(mr_busca.nss) != 11 THEN
            ERROR "LA LONGITUD DEBE SER DE 11 POSICIONES"
            NEXT FIELD nss
         ELSE
            LET  m_filtro = 1
            EXIT INPUT
         END IF 
      END IF    --NSS NO NULL

     IF LENGTH(mr_busca.curp) != 0 THEN
        IF LENGTH(mr_busca.curp) != 18 THEN
           ERROR "LA LONGITUD DEBE SER DE 18 POSICIONES"
           NEXT FIELD curp
        ELSE
           LET  m_filtro = 1
           EXIT INPUT   
        END IF 
     END IF    --NSS NO NULL
     
     	  IF mr_busca.fec_ini > TODAY THEN
           ERROR "LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA ACTUAL"
           NEXT FIELD fec_ini
        END IF

      IF mr_busca.fec_fin IS NULL AND mr_busca.fec_ini IS NOT NULL THEN
         ERROR "DEBE INGRESAR LA FECHA FINAL"
         NEXT FIELD fec_fin
      END IF
      
      IF mr_busca.fec_fin IS NOT NULL AND mr_busca.fec_ini IS NULL THEN
         ERROR "DEBE INGRESAR LA FECHA INICIAL"
         NEXT FIELD fec_ini
      END IF

     IF mr_busca.fec_fin IS NOT NULL AND mr_busca.fec_ini IS NOT NULL THEN 
        IF mr_busca.fec_fin > TODAY THEN
           ERROR "LA FECHA FINAL NO PUEDE SER MAYOR A LA FECHA ACTUAL"
           NEXT FIELD fec_fin
        END IF
        
        IF mr_busca.fec_fin < mr_busca.fec_ini THEN
           ERROR "LA FECHA FINAL NO PUEDE SER MENOR A LA FECHA INICIAL"
           NEXT FIELD fec_ini
        END IF	 
     END IF

      IF NOT FIELD_TOUCHED(scr_1.*) THEN
         ERROR "DEBE INGRESAR ALGUN CRITERIO DE BUSQUEDA"
         NEXT FIELD nss
      END IF

      IF (LENGTH(mr_busca.nss)=0) AND (LENGTH(mr_busca.curp)=0) AND
      	 (mr_busca.fec_ini IS NULL) AND mr_busca.fec_fin IS NULL  THEN
            ERROR "DEBE INGRESAR ALGUN CRITERIO DE BUSQUEDA"
            NEXT FIELD nss
      END IF

      EXIT INPUT

   ON KEY (CONTROL-C, INTERRUPT)
      ERROR "PROCESO CANCELADO" SLEEP 2
      ERROR ""
      CALL f_delete()
      RETURN --EXIT PROGRAM

END INPUT

    WHILE TRUE
      PROMPT "¿DESEA EJECUTAR LA CONSULTA? (S/N)" FOR CHAR m_enter
      
      IF m_enter MATCHES "[SsNn]" THEN
        IF m_enter MATCHES "[Ss]" THEN
           CALL f_WS()
           EXIT WHILE
        ELSE
           ERROR "PROCESO CANCELADO" SLEEP 2
           ERROR ""
           RETURN--EXIT PROGRAM
        END IF   
      ELSE
        ERROR "SOLO PRESIONE S o N"
      END IF  
    END WHILE

END FUNCTION

#################################################################################
FUNCTION f_ventana(p)
DEFINE p SMALLINT

--DISPLAY "entra vent ",p SLEEP 2
CASE p
  WHEN 1
    OPEN WINDOW win1 AT 2,2 WITH FORM "RETC7161" ATTRIBUTE(BORDER)
    DISPLAY " RETC716           BITACORA VENTANILLA UNICA 2.5               ",m_hoy USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "       [Esc]Consulta                                   [Ctrl-C]Salir     " AT 4,1 ATTRIBUTE(REVERSE)  
  WHEN 2
  	OPEN WINDOW w_filtro1 AT 2,2 WITH FORM "RETC7162" ATTRIBUTE(BORDER)
    DISPLAY "  [Esc]Consulta  Detalle                               [Ctrl-C]Salir     " AT 1,1 ATTRIBUTE(REVERSE)  
--    DISPLAY "   <CTRL-P> Archivo Plano                          <CTRL-I> Imprimir     " AT 2,1 ATTRIBUTE(REVERSE)  
    DISPLAY " RETC716     CUENTAS BITACORA VENTANILLA UNICA 2.5             ",m_hoy USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "   <CTRL-B> PAGINA ANTERIOR                   <CTRL-N>PAGINA SIGUIENTE   " AT 20,1 ATTRIBUTE(REVERSE)  
  WHEN 3
    OPEN WINDOW w_filtro3 AT 2,2 WITH FORM "RETC7163" ATTRIBUTE(BORDER)
    DISPLAY " RETC716      DETALLE SOL. SALDO BITACORA VU 2.5               ",m_hoy USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " [Ctrl-C]Salir                                                           " AT 1,1 ATTRIBUTE(REVERSE)
  WHEN 4
    OPEN WINDOW w_filtro4 AT 2,2 WITH FORM "RETC7164" ATTRIBUTE(BORDER)
    DISPLAY " RETC716       DETALLE NOTIFICACION BITACORA VU 2.5            ",m_hoy USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " [Ctrl-C]Salir       [Ctrl-T]SALDO    [Ctrl-F]SALDO FIP                  " AT 1,1 ATTRIBUTE(REVERSE)
  WHEN 5
    OPEN WINDOW w_filtro5 AT 2,2 WITH FORM "RETC7165" ATTRIBUTE(BORDER)
    DISPLAY " RETC716        DETALLE DATAMART BITACORA VU 2.5               ",m_hoy USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " [Ctrl-C]Salir                                                           " AT 1,1 ATTRIBUTE(REVERSE)
  WHEN 6
    OPEN WINDOW w_filtro6 AT 2,2 WITH FORM "RETC7166" ATTRIBUTE(BORDER)
    DISPLAY " RETC716       DETALLE CONFIRMACION BITACORA VU 2.5            ",m_hoy USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " [Ctrl-C]Salir       [Ctrl-T]TRANSFERENCIA       [Ctrl-F]DISPOSICION     " AT 1,1 ATTRIBUTE(REVERSE)
  WHEN 7
    OPEN WINDOW w_filtro7 AT 2,2 WITH FORM "RETC7167" ATTRIBUTE(BORDER)
    DISPLAY " RETC716       DETALLE CANCELACION BITACORA VU 2.5             ",m_hoy USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " [Ctrl-C]Salir                                                           " AT 1,1 ATTRIBUTE(REVERSE)
  WHEN 8
    OPEN WINDOW w_filtro8 AT 2,2 WITH FORM "RETC7168" ATTRIBUTE(BORDER)
    DISPLAY " RETC716       DETALLE EXTEMPORANEAS BITACORA VU 2.5           ",m_hoy USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " [Ctrl-C]Salir                                                           " AT 1,1 ATTRIBUTE(REVERSE)
END CASE


END FUNCTION

#################################################################################
FUNCTION f_delete()

     LET m_char = ""
     LET m_char = "DELETE  FROM safre_af:", m_tabla,
                  " WHERE 1 = 1"

     PREPARE p_del FROM m_char
     
     WHENEVER ERROR CONTINUE 
     EXECUTE p_del
     WHENEVER ERROR STOP

END FUNCTION

#################################################################################
FUNCTION f_WS()

ERROR "PROCESANDO INFORMACION" SLEEP 1
CALL f_params()

WHENEVER ERROR CONTINUE
   LET m_char =""
   LET m_char = "touch axis.log; rm -f axis.log;",
                "rm -f ",m_usuario CLIPPED, ".log_vu2-5.log ",
                         m_usuario CLIPPED ,".log_vu2-5.err"
   LET m_char = m_char CLIPPED
   RUN m_char
WHENEVER ERROR STOP

LET m_char = "java -jar /safre/java/VU_log_2-5.jar ", 
                                       m_xtype,                         " ",  
                                       mr_busca_sp.nss CLIPPED,         " ",       
                                       mr_busca_sp.curp CLIPPED,        " ",      
                                       mr_busca_sp.fec_ini CLIPPED,     " ",
                                       mr_busca_sp.fec_fin  CLIPPED,
                                       " 1>",m_usuario CLIPPED ,
                                       ".log_vu2-5.log 2>",m_usuario CLIPPED ,".log_vu2-5.err"

LET m_char = m_char CLIPPED
--DISPLAY "RUN: ", m_char  sleep 2

RUN m_char

   LET m_char =""
   LET m_char = "UPDATE STATISTICS FOR TABLE ",m_tabla
   PREPARE p_stat FROM m_char
   EXECUTE p_stat

ERROR "FINALIZA EJECUCION DEL WEB SERVICE" SLEEP 2
ERROR " "

 WHENEVER ERROR CONTINUE
   LET m_char =""
   LET m_char = "chmod 777 ",m_usuario CLIPPED,".log_vu2-5.log ", m_usuario CLIPPED,".log_vu2-5.err"
   LET m_char = m_char CLIPPED
   RUN m_char

   LET m_char = "touch axis.log; mv axis.log ", m_usuario CLIPPED,".axis.log"
   LET m_char = m_char CLIPPED
   RUN m_char
 WHENEVER ERROR STOP

CALL f_consulta()

--DISPLAY " sale de ws " SLEEP 2

END FUNCTION

#################################################################################
FUNCTION f_params()

INITIALIZE mr_busca_sp.* TO NULL
--DISPLAY "filtro: ", m_filtro SLEEP 1
    IF m_filtro = 1 THEN  --NSS/CURP
       IF mr_busca.nss IS NULL THEN
          LET mr_busca_sp.nss ="0"
          LET mr_busca_sp.curp = mr_busca.curp 
       ELSE
           LET mr_busca_sp.nss = mr_busca.nss
           LET mr_busca_sp.curp = "0"
       END IF
    
       IF mr_busca.curp IS NULL THEN
          LET mr_busca_sp.curp = "0"
          LET mr_busca_sp.nss = mr_busca.nss
       ELSE
          LET mr_busca_sp.curp = mr_busca.curp 
          LET mr_busca_sp.nss ="0"
       END IF

       LET mr_busca_sp.fec_ini = "0"
       LET mr_busca_sp.fec_fin = "0"
    END IF
    
    IF m_filtro = 2 THEN    --rango de fechas
       LET mr_busca_sp.fec_ini = mr_busca.fec_ini USING "YYYY-MM-DD"
       LET mr_busca_sp.fec_fin = mr_busca.fec_fin USING "YYYY-MM-DD"
       LET mr_busca_sp.nss  ="0"
       LET mr_busca_sp.curp = "0"
    END IF

--DISPLAY "params: ", mr_busca_sp.* SLEEP 4

END FUNCTION

#################################################################################
FUNCTION f_consulta()

LET m_paginas   = 0
LET m_registros = 0
LET m_posarray  = 1
LET m_char = " "

   CALL f_ventana(2)    #abre ventana de consulta

   DISPLAY mr_busca.fec_ini  TO fec_ini
   DISPLAY mr_busca.fec_fin  TO fec_fin

--PREPARE total registros consulta principal
LET m_char   = "SELECT COUNT(*)",
              " FROM safre_af: ", m_tabla

LET m_char = m_char CLIPPED

PREPARE p_cuenta FROM m_char
EXECUTE p_cuenta INTO m_registros

IF m_registros = 0 THEN
      ERROR "NO HAY DATOS PARA MOSTRAR" SLEEP 2
      ERROR ""
      CLOSE WINDOW w_filtro1
      CALL f_delete()
      RETURN --EXIT PROGRAM
END IF

--PREPARE selecion de consulta principal
LET m_char = " "
LET m_char = "SELECT id,nss, fec_rec_proces, diag_afore, diag_procesar ",
              " FROM safre_af:" ,m_tabla,
              " ORDER BY 2 desc"

LET m_char = m_char CLIPPED
PREPARE p_consul FROM m_char

   LET m_pag_act = 1
   CALL f_llena_arreglo()

CLOSE WINDOW w_filtro1
END FUNCTION


################################################################################
# Seleccion de informacion para llenar el arreglo que se despliega en pantalla
################################################################################
FUNCTION f_llena_arreglo()
DEFINE l_RegIni   SMALLINT,         #registro inicial
       l_RegFin   SMALLINT,         #registro final
       x          SMALLINT          #indice 

   LET m_paginas = m_registros / 10
   IF (m_paginas < (m_registros / 10)) THEN
       LET m_paginas = m_paginas + 1
   END IF

   DECLARE cur1 SCROLL CURSOR FOR p_consul
    OPEN cur1
     FETCH cur1 
     IF STATUS != NOTFOUND THEN --cuando hay registros
        WHILE TRUE
          CALL f_ini_arreglo()     # Inicializa arreglo de pantalla
             WHILE  m_pag_act <= m_paginas 
                LET l_RegIni = 1 + ((m_pag_act - 1) * 10) 
                LET l_RegFin = m_pag_act * 10 
                IF (l_RegFin > m_registros) THEN
                    LET l_RegFin = m_registros
                END IF
                 FOR x = l_RegIni TO l_RegFin
                       LET m_posarray = m_posarray + 1 
                       FETCH ABSOLUTE x cur1 
                       INTO mr_datos2[m_posarray].*
                 END FOR 
                CALL SET_COUNT(m_posarray)
                CALL f_dArreglo()                  #Despliega arreglo
                IF m_error = 1 THEN
                   LET m_error = 0
                   EXIT WHILE
                END IF
             END WHILE
          CALL SET_COUNT(m_registros)
        END WHILE
     ELSE --NO hay registros
        CLOSE cur1
        FREE cur1
     END IF
        CLOSE cur1
        FREE cur1

END FUNCTION

#####################################################################
FUNCTION f_ini_arreglo()
DEFINE x SMALLINT

--inicializa ARRAY
FOR x = 1 TO 10
  INITIALIZE mr_datos2[x].* TO NULL
  CLEAR scr_d2[x].*               --limpia arreglo de pantalla
END FOR 

LET m_posarray = 0

END FUNCTION

#################################################################################
FUNCTION F_dArreglo() 

       DISPLAY ARRAY mr_datos2 TO scr_d2.* ATTRIBUTE (CURRENT ROW DISPLAY = "REVERSE")
            ON KEY (INTERRUPT, CONTROL-C)
               ERROR "PROCESO CANCELADO" SLEEP 2
               ERROR ""
               EXIT PROGRAM

            ON KEY(CONTROL-B)                                  #Pagina Anterior
               IF (m_pag_act = 1) THEN
                  ERROR "ES LA PRIMERA PAGINA, NO HAY MAS DATOS"
               ELSE
                 CALL f_ini_arreglo()
                 LET m_pag_act = m_pag_act - 1 
                 EXIT DISPLAY
               END IF
            ON KEY(CONTROL-N)                                 #siguiente pagina 
               IF (m_pag_act = m_paginas) THEN    #si es la ultima pag
                  ERROR "ES LA ULTIMA PAGINA, NO HAY MAS DATOS"
               ELSE
                  CALL f_ini_arreglo()     
                  LET m_pag_act = m_pag_act + 1
                  EXIT DISPLAY
               END IF

            ON KEY(CONTROL-I)       #Imprimir
            	 --CALL f_reporte(1)
               SLEEP 2
       	    
            ON KEY(CONTROL-P)       #Genera archivo plano
            	 --CALL f_reporte(2)
               SLEEP 2

            ON KEY (ESC)
            	 LET m_p_row = ARR_CURR()
               CALL f_decide_consulta()
             IF m_error = 1 THEN
             --   LET m_error = 0
                EXIT DISPLAY
             END IF

        END DISPLAY

END FUNCTION

#################################################################################
FUNCTION f_decide_consulta()

   CALL f_ventana(m_ventana)

    CASE m_xtype
      WHEN 2        #solicitud de saldo
      CALL consulta3()
      WHEN 3        #notificacion de saldo
       CALL consulta4()
      WHEN 4        #Datamart
       CALL consulta5()
      WHEN 5        #Confirmacion /transferencia y disposicion
       CALL consulta6()
      WHEN 6        #Cancelacion
       CALL consulta7()
      WHEN 7        #extemporaneas
       CALL consulta8()
    END CASE

END FUNCTION 

#################################################################################
#solicitud de saldo WS01
FUNCTION consulta3()
DEFINE r_datos3 ARRAY [1] OF RECORD
    nss                 CHAR(11),
    curp                CHAR(18),
    nombre,
    paterno,
    materno             VARCHAR(40),
    fecha_sol_trab,
    fip                 DATETIME YEAR TO SECOND ,
    ind_sol_fip         SMALLINT,
    cve_pension         CHAR(3),
    ind_portabilidad,
    ind_trasp_post_fip  SMALLINT,
    ret97_post,
    cs_post,
    cv_post,
    viv97_post,
    saldo_viv97         DECIMAL(13,2),
    estado_vivienda     SMALLINT,
    fecha_vencimiento   DATETIME YEAR  TO  SECOND,
    diag_afore          SMALLINT,
    fec_rec_proces,
    fec_respud_safre,
    fec_envioa_safre,
    fec_sola_procesar   DATETIME YEAR TO SECOND
END RECORD

INITIALIZE r_datos3[1].* TO NULL
   LET m_char= ""
   LET m_char = "SELECT nss, curp, nombre, paterno, ",
                " materno, fecha_sol_trab, fip, ind_sol_fip, ", 
                " cve_pension, ind_portabilidad, ind_trasp_post_fip, ",
                " ret97_post, cs_post , cv_post, viv97_post, ",
                " saldo_viv97, estado_vivienda, fecha_vencimiento, ", 
                " diag_afore, fec_rec_proces, fec_respud_safre ,",
                " fec_envioa_safre, fec_sola_procesar " ,
                " FROM safre_af:", m_tabla,
                " WHERE id = ",mr_datos2[m_p_row].id
   LET m_char = m_char CLIPPED
   --DISPLAY "query 1 ",m_char SLEEP 2
   PREPARE p_consulta3 FROM m_char
   EXECUTE p_consulta3 INTO r_datos3[1].*

   IF STATUS = NOTFOUND THEN
      ERROR "NO HAY DATOS PARA MOSTRAR" SLEEP 2
      ERROR ""

      CLOSE WINDOW w_filtro3
      CALL f_delete()
      EXIT PROGRAM

   END IF

    CALL SET_COUNT(1)
    DISPLAY ARRAY r_datos3 TO src_d3.*
    
       ON KEY (INTERRUPT, CONTROL-C)
          ERROR "CONSULTA  FINALIZADA" SLEEP 2
          ERROR ""
          --CALL f_ini_arreglo()
          LET m_error = 1
          EXIT DISPLAY

    END DISPLAY

CLOSE WINDOW w_filtro3

END FUNCTION

#################################################################################
#Notificacion de Saldo WS02
FUNCTION consulta4()
DEFINE r_datos4 ARRAY [1] OF RECORD
          nss                 CHAR(11),
          curp                CHAR(18),
          diag_afore,
          diag_procesar       SMALLINT,
          fec_rec_proces,
          fec_respud_safre,
          fec_envioa_safre,
          fec_sola_procesar   DATETIME YEAR TO SECOND
      END RECORD,
      r_datos4_1  RECORD
          ret97,
          cs,
          cv,
          viv97,
          aport_vol,
          ret92,
          viv92,
          ret_iss_bxico,
          ret92_iss,
          aport_comp,
          viv_fov92,
          aport_lp,
          fov_08,
          ret_iss_08,
          cv_iss,
          ahorro_sol,
          cs_iss              DECIMAL(13,2)
      END RECORD

INITIALIZE r_datos4[1].*,r_datos4_1.* TO NULL
   LET m_char= ""
   LET m_char = "SELECT nss, curp, diag_afore, diag_procesar,",
                " fec_rec_proces, fec_respud_safre ,",
                " fec_envioa_safre, fec_sola_procesar " ,
                " FROM safre_af:", m_tabla,
                " WHERE id = ",mr_datos2[m_p_row].id
   LET m_char = m_char CLIPPED
   PREPARE p_consulta4 FROM m_char
   EXECUTE p_consulta4 INTO r_datos4[1].*

   IF STATUS = NOTFOUND THEN
      ERROR "NO HAY DATOS PARA MOSTRAR" SLEEP 2
      ERROR ""

      CLOSE WINDOW w_filtro4
      CALL f_delete()
      EXIT PROGRAM

   END IF

--Consulta SALDO
   LET m_char= ""
   LET m_char = "SELECT ret97,cs,cv,viv97,aport_vol, ",
                " ret92,viv92,ret_iss_bxico,ret92_iss, ",
                " aport_comp,viv_fov92,aport_lp,fov_08, ",
                " ret_iss_08,cv_iss,ahorro_sol,cs_iss ",
                " FROM safre_af:", m_tabla,
                " WHERE id = ",mr_datos2[m_p_row].id
   LET m_char = m_char CLIPPED
   PREPARE p_consulta4_1 FROM m_char

--Consulta saldo a FECHA INICIO DE PENSION
   LET m_char= ""
   LET m_char = "SELECT ret97_fip, cs_fip, cv_fip, viv97_fip, ",
                " aport_vol_fip, ' ', ' ', ret_iss_bxico_fip, ",
                " ret92_iss_fip, aport_comp_fip, viv_fov92_fip, ",
                " aport_lp_fip, fov_08_fip, ret_iss_08_fip, ",
                " cv_iss_fip, ahorro_sol_fip, cs_iss_fip ",
                " FROM safre_af:", m_tabla,
                " WHERE id = ",mr_datos2[m_p_row].id

   LET m_char = m_char CLIPPED
   PREPARE p_consulta4_2 FROM m_char

    CALL SET_COUNT(1)
    DISPLAY ARRAY r_datos4 TO src_d4.*
    
      ON KEY (INTERRUPT, CONTROL-C)
          ERROR "CONSULTA  FINALIZADA" SLEEP 2
          ERROR ""
          LET m_error = 1
          EXIT DISPLAY

      ON KEY (CONTROL-T)
        DISPLAY "                             SALDO ACTUAL                                 " AT 7,1 ATTRIBUTE(REVERSE)
        CLEAR src_d4_1.*               --limpia arreglo de pantalla
        EXECUTE p_consulta4_1 INTO r_datos4_1.*
        DISPLAY  r_datos4_1.* TO src_d4_1.*

      ON KEY (CONTROL-F)
        DISPLAY "                      SALDO A FECHA INICIO DE PENSION                     " AT 7,1 ATTRIBUTE(REVERSE)
        CLEAR src_d4_1.*
        EXECUTE p_consulta4_2 INTO r_datos4_1.*
        DISPLAY  r_datos4_1.* TO src_d4_1.*
       

    END DISPLAY

CLOSE WINDOW w_filtro4

END FUNCTION

#################################################################################
#Datamart WS03
FUNCTION consulta5()
DEFINE r_datos5 ARRAY [1] OF RECORD
           nss                 CHAR(11) ,
           curp                CHAR(18),
           sec_pension         CHAR(2),
           nombre_datamart     VARCHAR(50),
           tipo_movimiento     SMALLINT,
           regimen             CHAR(2),
           tipo_retiro         CHAR(1),
           tipo_seguro         CHAR(2),
           tipo_pension        CHAR(2),
           tipo_prestacion     SMALLINT,
           art_negativa        CHAR(3),
           frac_negativa,
           num_considerando    CHAR(2),
           porcentaje_val      DECIMAL(5,2),
           clave_aseguradora   CHAR(3),
           semanas_cotizadas   SMALLINT,
           fec_carga_datamart  DATETIME YEAR TO SECOND,
           monto_sol_imss      DECIMAL(13,2),
           transf_previa       CHAR(1),
           diag_afore,
           diag_procesar       SMALLINT,
           int_viv_97,
           int_viv_92,
           imp_viv_97,
           imp_viv_92          DECIMAL(14,6),
           fec_rec_proces,
           fec_respud_safre,
           fec_envioa_safre,
           fec_sola_procesar   DATETIME YEAR TO SECOND
     END RECORD


INITIALIZE r_datos5[1].* TO NULL
   LET m_char= ""
   LET m_char = "SELECT nss, curp, sec_pension, nombre_datamart, ",
                " tipo_movimiento, regimen, tipo_retiro, ",
                " tipo_seguro, tipo_pension, tipo_prestacion, ",
                " art_negativa, frac_negativa, num_considerando, ",
                " porcentaje_val, clave_aseguradora, semanas_cotizadas, ",
                " fec_carga_datamart, monto_sol_imss, transf_previa, ",
                " diag_afore, diag_procesar, int_viv_97, ",
                " int_viv_92, imp_viv_97, imp_viv_92, ",
                " fec_rec_proces, fec_respud_safre, fec_envioa_safre, ",
                " fec_sola_procesar ",
                " FROM safre_af:", m_tabla,
                " WHERE id = ",mr_datos2[m_p_row].id
   LET m_char = m_char CLIPPED
   --DISPLAY "query 1 ",m_char SLEEP 2
   PREPARE p_consulta5 FROM m_char
   EXECUTE p_consulta5 INTO r_datos5[1].*

   IF STATUS = NOTFOUND THEN
      ERROR "NO HAY DATOS PARA MOSTRAR" SLEEP 2
      ERROR ""

      CLOSE WINDOW w_filtro5
      CALL f_delete()
      EXIT PROGRAM

   END IF

    CALL SET_COUNT(1)
    DISPLAY ARRAY r_datos5 TO src_d5.*
    
       ON KEY (INTERRUPT, CONTROL-C)
          ERROR "CONSULTA  FINALIZADA" SLEEP 2
          ERROR ""
          LET m_error = 1
          EXIT DISPLAY

    END DISPLAY

CLOSE WINDOW w_filtro5

END FUNCTION

#################################################################################
#Confirmacion /transferencia y disposicion WS04
FUNCTION consulta6()
DEFINE r_datos6 ARRAY [1] OF RECORD
          nss                 CHAR(11),
          curp                CHAR(18),
          sec_pension         CHAR(2),
          diag_procesar       SMALLINT,
          CLABE               DECIMAL(13,2),
          fec_rec_proces,
          fec_respud_safre,
          fec_envioa_safre,
          fec_sola_procesar   DATETIME YEAR TO SECOND
      END RECORD,
      r_datos6_1  RECORD
          ret97,
          cs,
          cv,
          viv97,
          aport_vol,
          ret92,
          viv92,
          ret_iss_bxico,
          ret92_iss,
          aport_comp,
          viv_fov92,
          aport_lp,
          fov_08,
          ret_iss_08,
          cv_iss,
          ahorro_sol,
          cs_iss              DECIMAL(13,2)
      END RECORD

INITIALIZE r_datos6[1].*,r_datos6_1.* TO NULL
   LET m_char= ""
   LET m_char = "SELECT nss, curp, sec_pension, diag_procesar, clabe, ",
                " fec_rec_proces, fec_respud_safre ,",
                " fec_envioa_safre, fec_sola_procesar " ,
                " FROM safre_af:", m_tabla,
                " WHERE id = ",mr_datos2[m_p_row].id
   LET m_char = m_char CLIPPED
   PREPARE p_consulta6 FROM m_char
   EXECUTE p_consulta6 INTO r_datos6[1].*

   IF STATUS = NOTFOUND THEN
      ERROR "NO HAY DATOS PARA MOSTRAR" SLEEP 2
      ERROR ""

      CLOSE WINDOW w_filtro6
      CALL f_delete()
      EXIT PROGRAM

   END IF

--Consulta TRANSFERENCIA
   LET m_char= ""
   LET m_char = " SELECT ret97_trans, cs_trans, cv_trans, viv97_trans,",
                " aport_vol_trans, '', '', ret_iss_bnx_trans, ",
                " ret92_iss_trans, aport_comp_trans, fov_92_trans, ",
                " aport_lp_trans, fov_08_trans, ret_iss08_trans, ",
                " cv_iss_trans, ahorro_sol_trans, cs_iss_trans",
                " FROM safre_af:", m_tabla,
                " WHERE id = ",mr_datos2[m_p_row].id
   LET m_char = m_char CLIPPED
   PREPARE p_consulta6_1 FROM m_char

--Consulta DISPOSICION
   LET m_char= ""
   LET m_char = " SELECT ret_97_disp, cs_disp, cv_disp, viv97_disp, ",
                " aport_vol_disp, ret92_disp, viv92_disp, ret_iss_bnx_disp, ",
                " ret92_iss_disp, aport_comp_disp, fov_92_disp, ",
                " aport_lp_disp, fov_08_disp, ret_iss08_disp, ",
                " cv_iss_disp, ahorro_sol_disp, cs_iss_disp ",
                " FROM safre_af:", m_tabla,
                " WHERE id = ",mr_datos2[m_p_row].id

   LET m_char = m_char CLIPPED
   PREPARE p_consulta6_2 FROM m_char

    CALL SET_COUNT(1)
    DISPLAY ARRAY r_datos6 TO src_d6.*

      ON KEY (INTERRUPT, CONTROL-C)
          ERROR "CONSULTA  FINALIZADA" SLEEP 2
          ERROR ""
          LET m_error = 1
          EXIT DISPLAY

      ON KEY (CONTROL-T)   --TRANSFERENCIA
        DISPLAY "                              TRANSFERENCIA                             " AT 7,1 ATTRIBUTE(REVERSE)
        CLEAR src_d6_1.*               --limpia arreglo de pantalla
        EXECUTE p_consulta6_1 INTO r_datos6_1.*
        DISPLAY  r_datos6_1.* TO src_d6_1.*

      ON KEY (CONTROL-F)   --DISPOSICION
        DISPLAY "                               DISPOSICION                              " AT 7,1 ATTRIBUTE(REVERSE)
        CLEAR src_d6_1.*
        EXECUTE p_consulta6_2 INTO r_datos6_1.*
        DISPLAY  r_datos6_1.* TO src_d6_1.*

    END DISPLAY

CLOSE WINDOW w_filtro6

END FUNCTION

#################################################################################
#Cancelacion WS05
FUNCTION consulta7() 
DEFINE r_datos7 ARRAY[1] OF RECORD
        nss                 CHAR(11),
        curp                CHAR(18),
        nombre, 
        paterno,
        materno             VARCHAR(40),
        diaf_afore          SMALLINT,
        fec_rec_proces,
        fec_respud_safre,
        fec_envioa_safre,
        fec_sola_procesar   DATETIME YEAR TO SECOND
END RECORD

INITIALIZE r_datos7[1].* TO NULL
   LET m_char= ""
   LET m_char = "SELECT nss, curp, nombre, ",
                " paterno, materno, diag_afore, ",
                " fec_rec_proces, fec_respud_safre ,",
                " fec_envioa_safre, fec_sola_procesar " ,
                " FROM safre_af:", m_tabla,
                " WHERE id = ",mr_datos2[m_p_row].id
   LET m_char = m_char CLIPPED
   PREPARE p_consulta7 FROM m_char
   EXECUTE p_consulta7 INTO r_datos7[1].*

   IF STATUS = NOTFOUND THEN
      ERROR "NO HAY DATOS PARA MOSTRAR" SLEEP 2
      ERROR ""

      CLOSE WINDOW w_filtro7
      CALL f_delete()
      EXIT PROGRAM

   END IF

    CALL SET_COUNT(1)
    DISPLAY ARRAY r_datos7 TO src_d7.*
    
       ON KEY (INTERRUPT, CONTROL-C)
          ERROR "CONSULTA  FINALIZADA" SLEEP 2
          ERROR ""
          LET m_error = 1
          EXIT DISPLAY

    END DISPLAY

CLOSE WINDOW w_filtro7

END FUNCTION

#################################################################################
#Extemporaneas WS06
FUNCTION consulta8() 
DEFINE r_datos8 ARRAY[1] OF RECORD
        nss                 CHAR(11),
        sec_pension         CHAR(2),
        periodo_pago        CHAR(6), 
        id_tramite,
        diag_procesar       SMALLINT,
        fec_rec_proces,
        fec_respud_safre,
        fec_envioa_safre,
        fec_sola_procesar   DATETIME YEAR TO SECOND
END RECORD

INITIALIZE r_datos8[1].* TO NULL
   LET m_char= ""
   LET m_char = "SELECT nss, sec_pension, periodo_pago, ",
                " id_tramite, diag_procesar, ",
                " fec_rec_proces, fec_respud_safre ,",
                " fec_envioa_safre, fec_sola_procesar " ,
                " FROM safre_af:", m_tabla,
                " WHERE id = ",mr_datos2[m_p_row].id
   LET m_char = m_char CLIPPED
   PREPARE p_consulta8 FROM m_char
   EXECUTE p_consulta8 INTO r_datos8[1].*

   IF STATUS = NOTFOUND THEN
      ERROR "NO HAY DATOS PARA MOSTRAR" SLEEP 2
      ERROR ""

      CLOSE WINDOW w_filtro8
      CALL f_delete()
      EXIT PROGRAM

   END IF

    CALL SET_COUNT(1)
    DISPLAY ARRAY r_datos8 TO src_d8.*
    
       ON KEY (INTERRUPT, CONTROL-C)
          ERROR "CONSULTA  FINALIZADA" SLEEP 2
          ERROR ""
          --CALL f_ini_arreglo()
          LET m_error = 1
          EXIT DISPLAY

    END DISPLAY

CLOSE WINDOW w_filtro8

END FUNCTION

#################################################################################
FUNCTION f_reporte(p_ind)
DEFINE p_ind     SMALLINT,
lr_datos   RECORD LIKE safre_af:ret_bitacora_ws.*

LET m_hora  = TIME
---------------LET mr_param.ruta_listados = "." --ruta de pruebas
LET m_impre = mr_param.ruta_listados CLIPPED ,"/",m_usuario CLIPPED,
             ".RPT_BITACORA_VU_WS.",m_hoy USING "YYMMDD", ".", m_hora[1,2],
             m_hora[4,5], m_hora[7,8]
LET m_impre = m_impre CLIPPED
START REPORT rpt_ws TO m_impre

 DECLARE cur_rpt CURSOR FOR
   SELECT * FROM safre_af:ret_bitacora_ws
   
   FOREACH cur_rpt INTO lr_datos.*
      OUTPUT TO REPORT rpt_ws(lr_datos.*)	
   END FOREACH

FINISH REPORT rpt_ws
    
    IF p_ind = 1 THEN --imprimir

       LET m_char = " "
       LET m_char ="lp ",m_impre CLIPPED
       LET m_char = m_char CLIPPED
       RUN m_char RETURNING m_error
     
       IF m_error = 0 THEN
           ERROR " EL ARCHIVO SE HA ENVIADO A LA IMPRESORA PREDETERMINADA"
           SLEEP 2
           ERROR ""
       END IF
    END IF
    
    IF p_ind = 2 THEN       --archivo plano
    	 DISPLAY "ARCHIVO: ", m_impre AT 22,2 ATTRIBUTE (REVERSE)
    END IF

END FUNCTION

#################################################################################
REPORT rpt_ws(p)
DEFINE p RECORD LIKE safre_af:ret_bitacora_ws.*

  OUTPUT
       LEFT MARGIN 1
       RIGHT MARGIN 1
       TOP MARGIN 1
       BOTTOM MARGIN 1
       PAGE LENGTH 4


FORMAT 


FIRST PAGE HEADER

PRINT COLUMN 1, 
                "FECHA/HORA RECEPCION SOLICITUD PROCESAR,",
                "FECHA/HORA ENVÍO SOLICITUD A SAFRE,",
                "FECHA/HORA RECEPCION RESPUESTA DE SAFRE,",
                "FECHA/HORA RESPUESTA SOLICITUD A PROCESAR,",
                "NSS,",
                "CURP,",
                "SELLO ELECTRONICO,",
                "SIEFORE RCV,",
                "PRECIO DE LA ACCION,",
                "RETIRO 97 EN ACCIONES,",
                "RETIRO 97 EN PESOS,",
                "CV EN ACCIONES,",
                "CV EN PESOS,",
                "CUOTA SOCIAL EN ACCIONES,",
                "CUOTA SOCIAL EN PESOS,",
                "SIEFORE VIVIENDA,",
                "PRECIO DE LA ACCION,",
                "VIVIENDA 97 EN ACCIONES,",
                "VIVIENDA 97 EN PESOS,",
                "DIAGNOSTICO DE LA SOLICITUD,",
                "FOLIO DE LA SOLICITUD,",
                "ID DE ERROR,",
                "NUMERO DE ERROR,",
                "DESCRIPCION DE ERROR,",
                "TIEMPO RESPUESTA,",
                "FECHA,"


ON EVERY ROW

   PRINT COLUMN 1, p.fec_rec_proces     ,",", 
                   p.fec_envioa_safre   ,",",
                   p.fec_respud_safre   ,",",
                   p.fec_sola_procesar  ,",",                 
                   p.nss                ,",",
                   p.curp               ,",",
                   p.sello              ,",",
                   p.siefore_rcv        ,",",
                   p.precio_rcv         ,",",
                   p.acc_ret97          ,",",
                   p.peso_ret97         ,",",
                   p.acc_cv             ,",",
                   p.pes_cv             ,",",
                   p.acc_cs             ,",",
                   p.pes_cs             ,",",
                   p.siefore_viv        ,",",
                   p.precio_viv         ,",",
                   p.acc_viv            ,",",
                   p.pes_viv            ,",",
                   p.diagnostico        ,",",
                   p.sello              ,",",
                   p.error_id           ,",",
                   p.error_cod          ,",",
                   p.error_desc         ,",",
                   p.tiempo_resp        ,",",
                   m_hoy                ,","

END REPORT

#################################################################################
FUNCTION f_tabla()

LET m_tabla = ""
LET m_ventana = 0

    CASE m_xtype
      WHEN 2        #solicitud de saldo
       LET m_tabla = "ret_wslog_solicitud"
       LET m_ventana = 3
      WHEN 3        #notificacion de saldo
       LET m_tabla = "ret_wslog_notificacion"
       LET m_ventana = 4
      WHEN 4        #Datamart
       LET m_tabla = "ret_wslog_datamart"
       LET m_ventana = 5
      WHEN 5        #Confirmacion /transferencia y disposicion
       LET m_tabla = "ret_wslog_confirmacion"
       LET m_ventana = 6
      WHEN 6        #Cancelacion
       LET m_tabla = "ret_wslog_cancelacion"
       LET m_ventana = 7
      WHEN 7        #extemporaneas
       LET m_tabla = "ret_wslog_extemporaneas"
       LET m_ventana = 8
    END CASE

END FUNCTION