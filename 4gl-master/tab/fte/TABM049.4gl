#
# -------------------------------------------------------------------
# Programa : TABM049.4gl
# Objetivo : Catalogo de errores
# Modulo   : Interfases
# Fecha    : 17 de Enero de 2001
# Modifico : Patricia Martinez Sierra
# --------------------------------------------------------------------
#
DATABASE         safre_af
GLOBALS
DEFINE 
    p_error RECORD LIKE tab_error.*,
    v_arch CHAR(60),
    g_opc  CHAR(1),
    cla_where CHAR (150),

    l_record   ARRAY[300] OF RECORD
         error_cod	LIKE tab_error.error_cod ,
         error_desc   	LIKE tab_error.error_desc
    END RECORD,

    p_int RECORD LIKE  tab_error.*, 
    pos, i    SMALLINT,
    hoy       DATE

END GLOBALS
#
# ---------------------------------------------------------------------
# main 
# ---------------------------------------------------------------------
#
MAIN 
   OPTIONS                    
      INPUT WRAP,
      PROMPT LINE LAST, 
      ACCEPT KEY CONTROL-I 
      DEFER INTERRUPT
      #decision
      CALL menu_deci()

END MAIN
#
#-----------------------------------------------
# obtiene  datos con el seg_usuario
# -----------------------------------------
#
FUNCTION principal()
DEFINE vpausa CHAR(1),
       ejecuta CHAR(150),
       v_ban SMALLINT,
       hoy DATE
INITIALIZE  p_error.*  TO NULL
LET hoy = TODAY
LET v_ban = FALSE
OPEN WINDOW INTB049 AT 4,4 WITH FORM "TABM0491" ATTRIBUTE(BORDER)      
DISPLAY " <ESC> Procesar                                            < Ctrl-C > Salir " AT 1,1 ATTRIBUTE(REVERSE)                                         
                                                                         
DISPLAY " TABMO49       CATALOGO DE ERRORES DEL USUARIO                    " AT 3,1 ATTRIBUTE(REVERSE)
DISPLAY hoy  USING"DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)                
  INPUT BY NAME p_error.* WITHOUT DEFAULTS                                  
     BEFORE FIELD error_cod 
           IF g_opc = "a"  AND v_ban = FALSE THEN
              LET v_ban = TRUE
              SELECT MAX(error_cod) + 1 INTO p_error.error_cod 
              FROM tab_error
              DISPLAY BY NAME p_error.error_cod attribute(yellow)
              NEXT FIELD error_desc 
           END IF 
     AFTER FIELD error_cod                                                
           IF p_error.error_cod IS NULL THEN
              ERROR "Codigo de error no puede ser nulos"
              NEXT FIELD error_cod
           END IF
           IF g_opc = "t" or g_opc = "b" THEN
              SELECT error_desc INTO p_error.error_desc
              FROM tab_error 
              WHERE error_cod = p_error.error_cod    
              DISPLAY BY NAME p_error.error_desc attribute(yellow)
              IF SQLCA.SQLCODE = NOTFOUND THEN
                 ERROR " Registro Inexistente ..."
                 NEXT FIELD error_cod 
              END IF 
           END IF 
     AFTER FIELD error_desc                           
           IF p_error.error_desc IS NULL THEN
              ERROR "La descripcion no puede ir nulos "
              NEXT FIELD error_desc
           END IF                      
     ON KEY (ESC)                                                  
           IF p_error.error_cod IS NULL OR 
              p_error.error_desc IS NULL THEN
                  ERROR "Datos nulos ..." 
                  NEXT FIELD error_cod 
           END IF 
           IF g_opc = "t" or g_opc = "b"  THEN
               SELECT "OK"
               FROM tab_error
               WHERE error_cod =  p_error.error_cod 
               IF SQLCA.SQLCODE = NOTFOUND THEN
                   ERROR "Registro Inexistente ..." 
                   NEXT FIELD error_cod
               END IF 
           END IF 
      	   DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE) 
           CASE g_opc
                WHEN "a"
                      CALL alta_err()   
                WHEN "b"
                      CALL baja_err()
                WHEN "t"
                      CALL actualiza_err()
           END CASE
      	   DISPLAY " PROCESO TERMINADO ... " AT 19,1 ATTRIBUTE(REVERSE) 
           LET v_ban = FALSE
           SLEEP 3
      	   DISPLAY "                         " AT 19,1 ATTRIBUTE(YELLOW) 
           CLEAR FORM      
           EXIT INPUT
      ON KEY (INTERRUPT)                                                  
           DISPLAY " PROCESO CANCELADO " AT 19,2 ATTRIBUTE(REVERSE) SLEEP 2
           EXIT INPUT
  END INPUT                                                               
  CLOSE WINDOW INTB049	
END FUNCTION
#
# -------------------------------------------------------------
#  Alta del registro
# -------------------------------------------------------------
#
FUNCTION alta_err()
    INSERT INTO tab_error VALUES (p_error.*)
END FUNCTION
#
# ------------------------------------------------------------
# Baja del registro 
# -------------------------------------------------------------
#
FUNCTION baja_err()
      DELETE FROM tab_error WHERE error_cod  = p_error.error_cod
END FUNCTION
#
# ------------------------------------------------------------
# Actualizacion del registro 
# ------------------------------------------------------------
#
FUNCTION actualiza_err() 
      UPDATE tab_error SET error_desc = p_error.error_desc
      WHERE error_cod  =  p_error.error_cod
END FUNCTION
#
# ------------------------------------------------------------
# Consulta de registros 
# ------------------------------------------------------------
#
FUNCTION consulta_err()
DEFINE v_ban smallint
LET hoy = TODAY
   CALL  SET_COUNT(11)
   OPEN WINDOW ventana_1 AT 4,2 WITH FORM "TABM0494" ATTRIBUTE( BORDER)
   DISPLAY " TABM049       CONSULTA DE CATALOGOS DE ERRORES DEL USUARIO                    " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING " dd-mm-yyyy " AT 3,67 ATTRIBUTE(REVERSE)
   DISPLAY " (Ctrl-C) Salir                                                (ESC) Ejecutar  " AT 1,1 ATTRIBUTE(REVERSE,green)
   	
   CONSTRUCT cla_where ON error_cod ,      #base
                          error_desc 
                     FROM error_cod,
                          error_desc 
       ON KEY (esc)     
          LET v_ban =   TRUE
          EXIT CONSTRUCT
       ON KEY (control-c)
          EXIT CONSTRUCT
   END CONSTRUCT
   ERROR "PROCESANDO INFORMACION ..."
   IF v_ban THEN
      LET v_ban = FALSE
      CALL despliega_consulta()
   END IF
   CLOSE WINDOW ventana_1
END FUNCTION


FUNCTION despliega_consulta()
DEFINE v_ent, v_ban SMALLINT,
       v_sel CHAR(500),
       v_cuantos INTEGER 
    INITIALIZE l_record[300].* TO NULL
    LET i = 0
    LET v_sel = "SELECT  * ",
                " FROM  tab_error   "
    LET v_sel  =  v_sel CLIPPED , " WHERE ",
                cla_where CLIPPED
    LET v_sel =  v_sel CLIPPED, "  ORDER BY 1   "
   
    LET pos = FALSE 
    PREPARE i_cur FROM v_sel
    DECLARE cur_1 CURSOR FOR i_cur  
    FOREACH cur_1 INTO p_int.*
        LET pos = TRUE
        LET i = i + 1
        LET l_record[i].error_cod  = p_int.error_cod
        LET l_record[i].error_desc = p_int.error_desc
    END FOREACH

   IF  pos =  TRUE THEN
      ERROR " "
      CALL  SET_COUNT(i)
      OPEN WINDOW ventana_3 AT 4,2 WITH FORM "TABM0493" ATTRIBUTE( BORDER)
      DISPLAY " TABM049       CONSULTA DE CATALOGOS DE ERRORES DEL USUARIO                    " AT 3,1 ATTRIBUTE(REVERSE)
      DISPLAY HOY USING " dd-mm-yyyy " AT 3,67 ATTRIBUTE(REVERSE)
      DISPLAY " (Ctrl-C) Salir                                                                " AT 1,1 ATTRIBUTE(REVERSE,green)
   	
      DISPLAY  ARRAY l_record TO  scr_1.*

         ON KEY (control-c)
                EXIT DISPLAY 
      END DISPLAY

      CLOSE WINDOW ventana_3
    ELSE
            ERROR "No se encontro informacion ..."
            SLEEP 3
    END IF

END FUNCTION
#
# -----------------------------------------------------------
# funcion menu 
# -----------------------------------------------------------
#
FUNCTION menu_deci()
    OPEN WINDOW w_tr_1 at 2,2 WITH 2 ROWS, 77 COLUMNS
    ATTRIBUTE (BORDER) 
    MENU "ERROR "
         COMMAND "Alta"
             LET g_opc = "a"
             CALL principal()
         COMMAND  "Baja"
             LET g_opc = "b" 
             CALL principal()
         COMMAND  "acTualiza"
             LET g_opc  =  "t"
             CALL principal()
         COMMAND "Consulta"
             LET g_opc ="c"
             CALL consulta_err()
         COMMAND "Salir"
             LET g_opc = "s"
             EXIT MENU
    END MENU
    CLOSE WINDOW w_tr_1
END FUNCTION
#
