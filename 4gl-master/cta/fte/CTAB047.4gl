# ********************************************************************************#
# Modulo    : CTAB047                                                             #
# Objetivo  : PROCESO DE FUSION DE LA SIEFORE 5 A LA SIEFORE 4                    #
# Funciones : Lista de nombres de las funciones que contiene el modulo            #
# SCCS Id   : VERSION 1.0                                                         # 
# Autor     : STEFANIE DANIELA VERA PIÑA                                          #
# Fecha     : 02 DE NOVIEMBRE DEL 2012                                            #
# Lugar     : /safre/cta/fte                                                      #
# Argumentos:                                                                     #             
# Formas    :                                                                     #
# ********************************************************************************#
DATABASE  safre_af

GLOBALS

   DEFINE
      gd_hoy             ,
      gd_fecha_liquida   DATE
      
   DEFINE
      gc_enter           CHAR(1),
      gc_ruta_listados   CHAR(40),
      gc_lanza_proceso   CHAR(200),
      lc_permisos        CHAR(27),      
      gc_usuario         CHAR(8)
      
   DEFINE 
      gs_ban             ,
      gs_opcion          SMALLINT 
   
      
END GLOBALS


MAIN
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I

   DEFER INTERRUPT

   CALL sel_inicio()

   CALL STARTLOG("CTAB047.log")
   
   CALL mnu_menu()

END MAIN   


FUNCTION mnu_menu()
#------------------

   OPEN WINDOW v1 AT 2,3 WITH 3 ROWS , 76 COLUMNS
   ATTRIBUTE(BORDER)

   MENU "MENU"
      COMMAND "Identifica Traspasos" "Identifica cuentas con inversion en SB5 y con marca de TAA Cedente"
         CALL fn_identifica(1)
      COMMAND "Identifica Cuentas" "Identifica cuentas con perfil de inversion en la SB5"
         --CALL fn_identifica(2)
      COMMAND "Identifica Traspasos Complementarios" "Identifica cuentas con inversion en SB5 y con marca de TAA Complementarios"
         CALL fn_identifica(7)         
      COMMAND "Reporte de Identificacion" "Genera Reporte de Identificacion"
           CALL fn_rpt_identifica(5)
      COMMAND "Reporte Identificacion Complementarios" "Genera Reporte de Identificacion TAA Complementarios"
           CALL fn_rpt_identifica(8)
      COMMAND "Cambia Regimen" "Cambio de Regimen de Inversion de SB5 a SB4"     
         CALL fn_cambio_regimen()   
      COMMAND "Reporte Cambio de Regimen" "Genera Reporte de Cambio de Regimen"
         CALL fn_rpt_cambio_regimen()         
      COMMAND "Liquida" "Transferencia de Recursos de SB5 a SB4"    
         CALL fn_liquida(4)
      COMMAND "Reporte Liquidacion" "Genera Reporte de Transferencia de Recursos"
         CALL fn_rpt_liquida()         
      COMMAND "Salir" "Salir "
         EXIT MENU
   END MENU
   CLOSE WINDOW v1

END FUNCTION


FUNCTION sel_inicio()
#--------------------

   LET gd_hoy  = TODAY

   SELECT ruta_listados,
          USER
   INTO   gc_ruta_listados,
          gc_usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"

END FUNCTION


FUNCTION fn_identifica(ls_opcion)
#---------------------------------
  
   DEFINE
      li_folio             INTEGER
      
   DEFINE
      ls_opcion            SMALLINT      
      
   LET gs_ban = 0   

   IF ls_opcion = 1 THEN
      OPEN WINDOW v2 AT 5,3 WITH FORM "CTAB0471" ATTRIBUTE( BORDER )
      DISPLAY " <ESC> Ejecutar                                             <Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY " CTAB047       IDENTIFICA CUENTAS PARA FUSION DE SB5 A SB4                  " AT 3,1 ATTRIBUTE(REVERSE)
      DISPLAY gd_hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE (REVERSE)
      
      INPUT BY NAME li_folio WITHOUT DEFAULTS
         AFTER FIELD li_folio
            IF li_folio IS NULL
            OR li_folio = 0 THEN
                ERROR " FOLIO INCORRECTO "
                NEXT FIELD li_folio
            END  IF
      
         ON KEY (ESC)
      
            IF li_folio IS NULL
            OR li_folio = 0 THEN
               ERROR " FOLIO INCORRECTO "
               NEXT FIELD li_folio
            END IF
      
            SELECT "a.x"
            FROM   taa_cd_det_cedido a
            WHERE  a.folio = li_folio
            AND    a.estado = 101
            GROUP BY 1
      
            IF SQLCA.SQLCODE <> 0 THEN
               ERROR " NO EXISTE INFORMACION DEL FOLIO "
               NEXT FIELD li_folio
            END IF
      
            EXIT INPUT
      
         ON KEY( INTERRUPT, CONTROL-C )
      
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
            FOR gc_enter
            LET gs_ban = 1
            EXIT INPUT
      END INPUT
      
      IF gs_ban = 1 THEN
         CLOSE WINDOW v2
         RETURN
      END IF
      
      PROMPT " DESEA EJECUTAR EL PROCESO S/N ? "
      FOR gc_enter
      
      IF gc_enter MATCHES "[Ss]" THEN
      
         DISPLAY " PROCESANDO INFORMACION " AT 17,1 ATTRIBUTE(REVERSE)

         LET gc_lanza_proceso = "nohup fglgo CTAB047_1 ",li_folio,ls_opcion," '",gd_hoy,"'"
      
	       LET gc_lanza_proceso  = gc_lanza_proceso CLIPPED, " 1>"," ctab047_1" CLIPPED," 2> ctab047_1.err &"
	       
         RUN gc_lanza_proceso
      
         LET lc_permisos = "chmod 777 ","ctab047_1"
	       RUN lc_permisos
      
         DISPLAY "                   " AT 17,1
         PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR gc_enter
         
         LET gs_ban = 1
      
      ELSE
         PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
         FOR gc_enter
         LET gs_ban = 1
      END IF
      
      IF gs_ban = 1 THEN
        CLOSE WINDOW v2
        RETURN
      END IF
   ELSE
      OPEN WINDOW v2 AT 5,3 WITH FORM "CTAB0472" ATTRIBUTE( BORDER )
      DISPLAY " <ESC> Ejecutar                                             <Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY " CTAB047       IDENTIFICA CUENTAS PARA FUSION DE SB5 A SB4                  " AT 3,1 ATTRIBUTE(REVERSE)
      DISPLAY gd_hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE (REVERSE)   	 

      PROMPT " DESEA EJECUTAR EL PROCESO S/N ? "
      FOR gc_enter
      
      IF gc_enter MATCHES "[Ss]" THEN
      
         DISPLAY " PROCESANDO INFORMACION " AT 17,1 ATTRIBUTE(REVERSE)
      
         LET gc_lanza_proceso = "nohup fglgo CTAB047_1 ",li_folio,ls_opcion," '",gd_hoy,"'"
      
	       LET gc_lanza_proceso  = gc_lanza_proceso CLIPPED, " 1>"," ctab047_1" CLIPPED," 2> ctab047_1.err &"
         RUN gc_lanza_proceso
      
         LET lc_permisos = "chmod 777 ","ctab047_1"
	       RUN lc_permisos
      
         DISPLAY "                   " AT 17,1
         PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR gc_enter
         
         LET gs_ban = 1
      
      ELSE
         PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
         FOR gc_enter
         LET gs_ban = 1
      END IF
      
      IF gs_ban = 1 THEN
        CLOSE WINDOW v2
        RETURN
      END IF
   END IF

END FUNCTION


FUNCTION fn_rpt_identifica(ls_opcion)
#------------------------------------

   DEFINE
      lc_lanza_proceso     CHAR(200),
      lc_permisos          CHAR(27)
      
   DEFINE
      li_folio             INTEGER
      
   DEFINE
      ls_opcion            SMALLINT      

   OPEN WINDOW v5 AT 5,3 WITH FORM "CTAB0472" ATTRIBUTE( BORDER )
   DISPLAY " <ESC> Ejecutar                                             <Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " CTAB047  REPORTE IDENTIFICACION DE CUENTAS PARA FUSION DE SB5 A SB4        " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY gd_hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE (REVERSE)


   PROMPT " DESEA EJECUTAR EL PROCESO S/N ? "
   FOR gc_enter

   IF gc_enter MATCHES "[Ss]" THEN
      DISPLAY " PROCESANDO INFORMACION " AT 17,1 ATTRIBUTE(REVERSE)
            
      LET li_folio = 0
      
      LET lc_lanza_proceso = "nohup fglgo CTAB047_1 ",li_folio,ls_opcion," '",gd_hoy,"'"
      
      LET lc_lanza_proceso  = lc_lanza_proceso CLIPPED, " 1>"," ctab047_1" CLIPPED," 2> ctab047_1.err &"
      RUN lc_lanza_proceso
       
      LET lc_permisos = "chmod 777 ","ctab047_1"
      RUN lc_permisos
      
      DISPLAY "REPORTE GENERADO EN LA RUTA: ",gc_ruta_listados CLIPPED AT 11,10
      DISPLAY "CON EL NOMBRE : ","CTAB047_",gd_hoy  USING "YYYYMMDD",".txt" AT 13,10

      DISPLAY "                   " AT 17,1
      PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR gc_enter
      
      LET gs_ban = 1
      
   ELSE
      PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
      FOR gc_enter
      LET gs_ban = 1
   END IF

   IF gs_ban = 1 THEN
     CLOSE WINDOW v5
     RETURN
   END IF


END FUNCTION


FUNCTION fn_cambio_regimen()
#---------------------------------
  
   DEFINE
      lc_lanza_proceso     CHAR(200),
      lc_permisos          CHAR(27)
      
   DEFINE
      li_folio             INTEGER
      
   DEFINE
      ls_opcion            SMALLINT      

   OPEN WINDOW v3 AT 5,3 WITH FORM "CTAB0472" ATTRIBUTE( BORDER )
   DISPLAY " <ESC> Ejecutar                                             <Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " CTAB047  CAMBIO DE REGIMEN DE CUENTAS PARA FUSION DE SB5 A SB4             " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY gd_hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE (REVERSE)


   PROMPT " DESEA EJECUTAR EL PROCESO S/N ? "
   FOR gc_enter

   IF gc_enter MATCHES "[Ss]" THEN
      DISPLAY " PROCESANDO INFORMACION " AT 17,1 ATTRIBUTE(REVERSE)
            
      LET li_folio = 0
      LET ls_opcion = 3
      
      LET lc_lanza_proceso = "nohup fglgo CTAB047_1 ",li_folio,ls_opcion," '",gd_hoy,"'"
      
      LET lc_lanza_proceso  = lc_lanza_proceso CLIPPED, " 1>"," ctab047_1" CLIPPED," 2> ctab047_1.err &"
      RUN lc_lanza_proceso
       
      LET lc_permisos = "chmod 777 ","ctab047_1"
      RUN lc_permisos

      DISPLAY "                   " AT 17,1
      PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR gc_enter
      
      LET gs_ban = 1
      
   ELSE
      PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
      FOR gc_enter
      LET gs_ban = 1
   END IF

   IF gs_ban = 1 THEN
     CLOSE WINDOW v3
     RETURN
   END IF

END FUNCTION


FUNCTION fn_rpt_cambio_regimen()
#-------------------------------
   DEFINE
      lc_lanza_proceso     CHAR(200),
      lc_permisos          CHAR(27)
      
   DEFINE
      li_folio             INTEGER
      
   DEFINE
      ls_opcion            SMALLINT      

   OPEN WINDOW v6 AT 5,3 WITH FORM "CTAB0472" ATTRIBUTE( BORDER )
   DISPLAY " <ESC> Ejecutar                                             <Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " CTAB047  REPORTE CAMBIO DE REGIMEN DE CUENTAS PARA FUSION DE SB5 A SB4     " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY gd_hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE (REVERSE)


   PROMPT " DESEA EJECUTAR EL PROCESO S/N ? "
   FOR gc_enter

   IF gc_enter MATCHES "[Ss]" THEN
      DISPLAY " PROCESANDO INFORMACION " AT 17,1 ATTRIBUTE(REVERSE)
            
      LET li_folio = 0
      LET ls_opcion = 6
      
      LET lc_lanza_proceso = "nohup fglgo CTAB047_1 ",li_folio,ls_opcion," '",gd_hoy,"'"
      
      LET lc_lanza_proceso  = lc_lanza_proceso CLIPPED, " 1>"," ctab047_1" CLIPPED," 2> ctab047_1.err &"
      RUN lc_lanza_proceso
       
      LET lc_permisos = "chmod 777 ","ctab047_1"
      RUN lc_permisos

      DISPLAY "REPORTE GENERADO EN LA RUTA: ",gc_ruta_listados CLIPPED AT 11,10
      DISPLAY "CON EL NOMBRE : ","CTAB047_REG_",gd_hoy  USING "YYYYMMDD",".txt" AT 13,10

      DISPLAY "                   " AT 17,1
      PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR gc_enter
      
      LET gs_ban = 1
      
   ELSE
      PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
      FOR gc_enter
      LET gs_ban = 1
   END IF

   IF gs_ban = 1 THEN
     CLOSE WINDOW v6
     RETURN
   END IF


END FUNCTION


FUNCTION fn_liquida(ls_opcion)
#------------------------------

   DEFINE
      ls_opcion         SMALLINT

   OPEN WINDOW v4 AT 5,3 WITH FORM "CTAB0474" ATTRIBUTE( BORDER )
   DISPLAY " <ESC> Ejecutar                                             <Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " CTAB047     LIQUIDACION DE CUENTAS PARA FUSION DE SB5 A SB4                " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY gd_hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE (REVERSE)
   
   INPUT BY NAME gd_fecha_liquida
         AFTER FIELD gd_fecha_liquida
            IF gd_fecha_liquida IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD gd_fecha_liquida
            END IF
         
         ON KEY (ESC)

            IF gd_fecha_liquida IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD gd_fecha_liquida
            END IF
            
            PROMPT " DESEA EJECUTAR EL PROCESO S/N ? "
               FOR gc_enter
         
            IF gc_enter MATCHES "[Ss]" THEN
         
               DISPLAY " PROCESANDO INFORMACION " AT 17,1 ATTRIBUTE(REVERSE)

               LET gc_lanza_proceso = "nohup fglgo CTAB047_1 ",0,4," '",gd_fecha_liquida,"'"
         
         	    LET gc_lanza_proceso  = gc_lanza_proceso CLIPPED, " 1>"," ctab047_1" CLIPPED," 2> ctab047_1.err &"
               RUN gc_lanza_proceso
          
               LET lc_permisos = "chmod 777 ","ctab047_1"
         	    RUN lc_permisos
         
               DISPLAY "                   " AT 17,1
               PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR gc_enter
               
               LET gs_ban = 1
         
            ELSE
               PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
               FOR gc_enter
               LET gs_ban = 1
            END IF
         
          ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA CONTINUAR "
               FOR gc_enter
            EXIT INPUT
   END INPUT

   CLEAR FORM
   
   IF gs_ban = 1 THEN
      CLOSE WINDOW v4
      RETURN
   END IF

END FUNCTION


FUNCTION fn_rpt_liquida()
#-------------------------

   DEFINE
      lc_lanza_proceso     CHAR(200),
      lc_permisos          CHAR(27)
      
   DEFINE
      li_folio             INTEGER
      
   DEFINE
      ls_opcion            SMALLINT      

   OPEN WINDOW v7 AT 5,3 WITH FORM "CTAB0473" ATTRIBUTE( BORDER )
   DISPLAY " <ESC> Ejecutar                                             <Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " CTAB047    REPORTE DE TRANSFERENCIA DE RECURSOS DE SB5 A SB4               " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY gd_hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE (REVERSE)

   INPUT BY NAME li_folio WITHOUT DEFAULTS
      AFTER FIELD li_folio
         IF li_folio IS NULL
         OR li_folio = 0 THEN
             ERROR " FOLIO INCORRECTO "
             NEXT FIELD li_folio
         END  IF
   
      ON KEY (ESC)
   
         IF li_folio IS NULL
         OR li_folio = 0 THEN
            ERROR " FOLIO INCORRECTO "
            NEXT FIELD li_folio
         END IF
   
         SELECT "a.x"
         FROM   dis_cuenta a
         WHERE  a.folio = li_folio
         GROUP BY 1
   
         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NO EXISTE INFORMACION DEL FOLIO "
            NEXT FIELD li_folio
         END IF
   
         EXIT INPUT
   
      ON KEY( INTERRUPT, CONTROL-C )
   
         PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
         FOR gc_enter
         LET gs_ban = 1
         EXIT INPUT
   END INPUT
   
   IF gs_ban = 1 THEN
      CLOSE WINDOW v7
      RETURN
   END IF

   PROMPT " DESEA EJECUTAR EL PROCESO S/N ? "
   FOR gc_enter

   IF gc_enter MATCHES "[Ss]" THEN
      DISPLAY " PROCESANDO INFORMACION " AT 17,1 ATTRIBUTE(REVERSE)
            
      LET ls_opcion = 9
      
      LET lc_lanza_proceso = "nohup fglgo CTAB047_1 ",li_folio,ls_opcion," '",gd_fecha_liquida,"'"
      
      LET lc_lanza_proceso  = lc_lanza_proceso CLIPPED, " 1>"," ctab047_1" CLIPPED," 2> ctab047_1.err &"
      RUN lc_lanza_proceso
       
      LET lc_permisos = "chmod 777 ","ctab047_1"
      RUN lc_permisos

      DISPLAY "REPORTE GENERADO EN LA RUTA: ",gc_ruta_listados CLIPPED AT 11,10
      DISPLAY "CON EL NOMBRE : ","CTAB047_LIQ_",gd_hoy  USING "YYYYMMDD",".txt" AT 13,10

      DISPLAY "                   " AT 17,1
      PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR gc_enter
      
      LET gs_ban = 1
      
   ELSE
      PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
      FOR gc_enter
      LET gs_ban = 1
   END IF

   IF gs_ban = 1 THEN
     CLOSE WINDOW v7
     RETURN
   END IF

END FUNCTION