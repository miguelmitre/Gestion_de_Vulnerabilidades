#
# -------------------------------------------------------------------
# Programa : TABM051.4gl
# Objetivo : Catalogo de Leyendas
# Modulo   : Interfases
# Fecha    : 13 de Marzo de 2001
# --------------------------------------------------------------------
#
DATABASE         safre_af
GLOBALS
DEFINE 
    g_leyenda RECORD LIKE tab_leyenda.*,
    u_leyenda RECORD LIKE tab_leyenda.*,
    n_leyenda RECORD LIKE tab_leyenda.*,
	 g_layout  ARRAY[80] OF RECORD 
								  layout_cod LIKE tab_layout.layout_cod,
								  layout_des LIKE tab_layout.layout_des 
              END RECORD,

    v_arch           CHAR(60),
    g_opc            CHAR(1),
    limpia           CHAR (150),

    pos, i,v_cuantos SMALLINT,
    hoy              DATE,

	 d_ley,d_lay,
	 d_des          CHAR(15)

END GLOBALS
#
# ---------------------------------------------------------------------
# main 
# ---------------------------------------------------------------------
#
MAIN 
   DEFER INTERRUPT
   OPTIONS                    
      INPUT WRAP,
      PROMPT LINE LAST 
		CALL init_text()
		CALL init_del()
		CALL init_lay()
      CALL menu_leyenda()

END MAIN


FUNCTION init_text()

DEFINE text_i 		SMALLINT,
		 v_sel2     CHAR(350)

LOCATE g_leyenda.leyenda IN MEMORY
LOCATE u_leyenda.leyenda IN MEMORY
LOCATE n_leyenda.leyenda IN MEMORY

INITIALIZE  limpia TO NULL
INITIALIZE  u_leyenda.leyenda_cod TO NULL
INITIALIZE  n_leyenda.layout_cod TO NULL

LET v_sel2 = "SELECT leyenda_cod, layout_cod FROM tab_leyenda WHERE ",
				 " leyenda_cod = ? AND layout_cod = ? "

PREPARE eje_sel2 FROM v_sel2
END FUNCTION 

FUNCTION init_del()

  DECLARE del_cur CURSOR FOR
		 SELECT * FROM tab_leyenda
		  WHERE tab_leyenda.leyenda_cod = g_leyenda.leyenda_cod
			 AND tab_leyenda.layout_cod = g_leyenda.layout_cod
  FOR UPDATE
END FUNCTION


#
#-----------------------------------------------
# obtiene  datos con el seg_usuario
# -----------------------------------------
#
FUNCTION alta_leyenda()
DEFINE vpausa CHAR(1),
       ejecuta CHAR(150),
       hoy DATE

INITIALIZE  g_leyenda.*  TO NULL

LET hoy = TODAY

LET d_ley = "CODIGO  LEYENDA"
LET d_lay = "CODIGO  LAYOUT"
LET d_des = "DESCRIPCION "

CLEAR FORM

DISPLAY hoy  USING"DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)                
DISPLAY " TABM051         CATALOGO DE LEYENDAS PARA LAYOUT                " 
			 AT 3,1 ATTRIBUTE(REVERSE)
DISPLAY " (Ctrl-C) Salir                                          (ESC) Ejecutar  " AT 4,1 #ATTRIBUTE(REVERSE,green)

DISPLAY BY NAME d_ley
DISPLAY BY NAME d_lay
DISPLAY BY NAME d_des

LET int_flag = FALSE

INPUT BY NAME g_leyenda.layout_cod,
				  g_leyenda.leyenda_cod,
				  g_leyenda.leyenda

     AFTER FIELD layout_cod                                                
				  IF NOT valida_layout(g_leyenda.layout_cod) THEN
					  CALL layout_ventana()  RETURNING g_leyenda.layout_cod,
																  int_flag

                 IF int_flag THEN
						  LET int_flag = FALSE
						  LET g_leyenda.layout_cod = NULL
                    ERROR "Codigo de layout no puede ser nulo"
						  NEXT FIELD layout_cod
                 ELSE
						  DISPLAY BY NAME g_leyenda.layout_cod
					  END IF
              END IF

     BEFORE FIELD leyenda_cod

         SELECT MAX(leyenda_cod) + 1 INTO g_leyenda.leyenda_cod 
           FROM tab_leyenda
          WHERE layout_cod = g_leyenda.layout_cod

         IF g_leyenda.leyenda_cod IS NULL THEN
				LET g_leyenda.leyenda_cod = 1
         END IF

         DISPLAY BY NAME g_leyenda.leyenda_cod attribute(yellow)
         NEXT FIELD leyenda 

     AFTER FIELD leyenda                           
           IF g_leyenda.leyenda IS NULL THEN
              ERROR "La leyenda no puede ser nula "
              NEXT FIELD leyenda
           END IF                      

    ON KEY (INTERRUPT ) 
	    LET g_leyenda.leyenda_cod = n_leyenda.leyenda_cod
	    LET g_leyenda.layout_cod = n_leyenda.layout_cod
	    CALL cancela()
	    EXIT INPUT

END INPUT                                                               

IF int_flag THEN 
	LET int_flag = FALSE
	LET g_leyenda.leyenda_cod = n_leyenda.leyenda_cod
	LET g_leyenda.layout_cod = n_leyenda.layout_cod
	CALL cancela()
	RETURN
END IF

WHENEVER ERROR CONTINUE

INSERT INTO tab_leyenda
		 VALUES ( g_leyenda.* )

IF SQLCA.SQLCODE = NOTFOUND THEN
   ERROR " No se pudo insertar el Registro."
	SLEEP 2
	CALL limpia_men()
ELSE
   ERROR " Registro Insertado."
	SLEEP 2
	CALL limpia_men()
END IF 

WHENEVER ERROR STOP

END FUNCTION


FUNCTION borra_leyenda()

WHENEVER ERROR CONTINUE

OPEN del_cur

FETCH del_cur INTO g_leyenda.*

WHENEVER ERROR STOP

IF SQLCA.SQLCODE = NOTFOUND THEN

	ERROR "El Registro ya fue borrado."
	SLEEP 2
	CALL limpia_men()
ELSE

	IF SQLCA.SQLCODE < 0 THEN
		ERROR "El Registro no puede ser borrado."
	   SLEEP 2
	   CALL limpia_men()
   ELSE
		WHENEVER ERROR CONTINUE
		DELETE FROM tab_leyenda WHERE CURRENT OF del_cur
		WHENEVER ERROR STOP

		IF SQLCA.SQLCODE < 0 THEN
			ERROR "Ocurrio el error ", SQLCA.SQLCODE USING "-<<<<" 
      ELSE
			ERROR "Registro borrado."
	      SLEEP 2
	      CALL limpia_men()
      END IF
   END IF

END IF

END FUNCTION


FUNCTION actualiza_leyenda() 

LET hoy = TODAY

LET d_ley = "CODIGO  LEYENDA"
LET d_lay = "CODIGO  LAYOUT"
LET d_des = "DESCRIPCION "

CLEAR FORM
DISPLAY " TABM051         CATALOGO DE LEYENDAS PARA LAYOUT                " 
			 AT 3,1 ATTRIBUTE(REVERSE)
DISPLAY HOY USING " dd-mm-yyyy " AT 3,66 ATTRIBUTE(REVERSE)
DISPLAY " (Ctrl-C) Salir                                          (ESC) Ejecutar  " AT 4,1 #ATTRIBUTE(REVERSE,green)

DISPLAY BY NAME d_ley
DISPLAY BY NAME d_lay
DISPLAY BY NAME d_des


LET int_flag = FALSE

OPEN del_cur

WHENEVER ERROR CONTINUE
FETCH del_cur INTO g_leyenda.leyenda_cod, g_leyenda.layout_cod
WHENEVER ERROR STOP

IF SQLCA.SQLCODE < 0 OR SQLCA.SQLCODE = NOTFOUND THEN
	ERROR "No se puede bloquear el registro para actualizar."
ELSE
	LET u_leyenda.leyenda_cod = g_leyenda.leyenda_cod
	LET u_leyenda.layout_cod = g_leyenda.layout_cod

	SELECT leyenda
	  INTO u_leyenda.leyenda
	  FROM tab_leyenda
    WHERE leyenda_cod = g_leyenda.leyenda_cod
	   AND layout_cod  = g_leyenda.layout_cod

   LET int_flag = FALSE
	INPUT g_leyenda.leyenda
	WITHOUT DEFAULTS
	FROM leyenda

	AFTER FIELD leyenda

    ON KEY (INTERRUPT ) 
   IF int_flag THEN 
	    LET g_leyenda.leyenda_cod = n_leyenda.leyenda_cod
	    LET g_leyenda.layout_cod = n_leyenda.layout_cod
	    CALL cancela()
	    EXIT INPUT

   {IF int_flag THEN 
   	LET int_flag = FALSE
   	LET g_leyenda.leyenda_cod = u_leyenda.leyenda_cod
   	LET g_leyenda.layout_cod = u_leyenda.layout_cod

	   SELECT leyenda
	     INTO g_leyenda.leyenda
	     FROM tab_leyenda
       WHERE leyenda_cod = u_leyenda.leyenda_cod
	      AND layout_cod  = u_leyenda.layout_cod

      CLEAR FORM
		
      CALL cancela() }

   ELSE

		WHENEVER ERROR CONTINUE

      UPDATE tab_leyenda 
			SET leyenda = g_leyenda.leyenda
       WHERE leyenda_cod  =  g_leyenda.leyenda_cod
         AND layout_cod   =  g_leyenda.layout_cod

      WHENEVER ERROR STOP

		IF SQLCA.SQLCODE < 0 THEN
			ERROR "Ocurrio el error ", SQLCA.SQLCODE USING "-<<<<" 
   	   LET g_leyenda.leyenda_cod = u_leyenda.leyenda_cod
   	   LET g_leyenda.layout_cod = u_leyenda.layout_cod

	      SELECT leyenda
	        INTO g_leyenda.leyenda
	        FROM tab_leyenda
          WHERE leyenda_cod = u_leyenda.leyenda_cod
	         AND layout_cod  = u_leyenda.layout_cod

        DISPLAY  BY NAME g_leyenda.* 

      ELSE

			ERROR "Registro Actualizado."
	      SLEEP 2
	      CALL limpia_men()

      END IF

   END IF

	END INPUT

END IF


END FUNCTION

#
# ------------------------------------------------------------
# Consulta de registros 
# ------------------------------------------------------------
#
FUNCTION consulta_ley()

DEFINE   v_ent,  i            SMALLINT, 
         v_sel,v_cnt          CHAR(500),
		     cla_where          CHAR(250)

LET i = 0
LET v_cuantos = 0
LET v_sel = limpia
LET v_cnt = limpia

LET hoy = TODAY

LET d_ley = "CODIGO  LEYENDA"
LET d_lay = "CODIGO  LAYOUT"
LET d_des = "DESCRIPCION "

CLEAR FORM
DISPLAY " TABM051         CATALOGO DE LEYENDAS PARA LAYOUT                " 
			 AT 3,1 ATTRIBUTE(REVERSE)
DISPLAY HOY USING " dd-mm-yyyy " AT 3,66 ATTRIBUTE(REVERSE)
DISPLAY " (Ctrl-C) Salir                                          (ESC) Ejecutar  " AT 4,1 #ATTRIBUTE(REVERSE,green)

DISPLAY BY NAME d_ley
DISPLAY BY NAME d_lay
DISPLAY BY NAME d_des


LET int_flag = FALSE

CONSTRUCT BY NAME cla_where ON layout_cod, 
										 leyenda_cod 

IF int_flag = TRUE THEN
   LET int_flag = FALSE
   CALL cancela() 
	RETURN FALSE
END IF

LET v_sel = "SELECT  leyenda_cod, layout_cod FROM  tab_leyenda  WHERE ",
					  cla_where CLIPPED

LET v_sel = v_sel CLIPPED
   
LET v_cnt = "SELECT  COUNT(*)  FROM tab_leyenda WHERE ",
					  cla_where CLIPPED

LET v_cnt = v_cnt CLIPPED

PREPARE s_cnt FROM v_cnt

PREPARE i_cur FROM v_sel

DECLARE cur_1 SCROLL CURSOR FOR i_cur  

OPEN cur_1

FETCH FIRST cur_1 INTO g_leyenda.leyenda_cod, g_leyenda.layout_cod

     IF SQLCA.SQLCODE = NOTFOUND THEN
		  ERROR "No existen Registros !"
		  CLOSE cur_1
		  SLEEP 2
		  CALL limpia_men()
		  RETURN FALSE
    ELSE

		 DECLARE ley_todas CURSOR FOR eje_sel2

		 OPEN ley_todas USING g_leyenda.leyenda_cod,
									 g_leyenda.layout_cod

       FETCH ley_todas INTO g_leyenda.leyenda_cod,
									 g_leyenda.layout_cod

       IF SQLCA.SQLCODE = NOTFOUND THEN
			 ERROR "No existe registro(s)."
		    RETURN FALSE
       ELSE
          DECLARE cur_cnt CURSOR FOR s_cnt  
	       OPEN cur_cnt
	       FETCH cur_cnt INTO v_cuantos
		    CLOSE cur_cnt
		    ERROR "Existen ", v_cuantos USING "<<<<", " Registros."
		    SLEEP 3
		    ERROR ""

		    SELECT leyenda
		      INTO g_leyenda.leyenda
		      FROM tab_leyenda
           WHERE leyenda_cod = g_leyenda.leyenda_cod
		       AND layout_cod  = g_leyenda.layout_cod
           ERROR " "
           DISPLAY  BY NAME g_leyenda.* 
		     RETURN TRUE
      END IF

    END IF

END FUNCTION

FUNCTION cambia_reg ( direccion )

  DEFINE direccion SMALLINT

  FETCH RELATIVE direccion cur_1 INTO g_leyenda.leyenda_cod,
												  g_leyenda.layout_cod

  IF SQLCA.SQLCODE = NOTFOUND THEN

	  IF direccion = 1 THEN
		  ERROR "Esta en el inicio de la lista."
     ELSE
		  ERROR "Esta en el final de la lista."
     END IF

  ELSE
	  OPEN ley_todas USING g_leyenda.leyenda_cod,
								  g_leyenda.layout_cod

     FETCH ley_todas INTO g_leyenda.leyenda_cod,
								  g_leyenda.layout_cod

     IF SQLCA.SQLCODE = NOTFOUND THEN

		  CLEAR FORM
		  DISPLAY BY NAME g_leyenda.*
		  ERROR " El registro ya no existe."
     ELSE

	    SELECT leyenda
	      INTO g_leyenda.leyenda
	      FROM tab_leyenda
        WHERE leyenda_cod = g_leyenda.leyenda_cod
	       AND layout_cod  = g_leyenda.layout_cod

		  DISPLAY BY NAME g_leyenda.*

     END IF

	  CLOSE ley_todas

  END IF
		  

END FUNCTION
								 
FUNCTION menu_leyenda()

DEFINE tipo_menu       SMALLINT

LET tipo_menu = 0

OPEN WINDOW ventana_1 AT 2,2 WITH FORM "TABM0511" ATTRIBUTE( BORDER)

    MENU "LEYENDA "
	 BEFORE MENU
			HIDE OPTION ALL
			SHOW OPTION "Consulta", "Alta","Salir"

         COMMAND "Consulta"
				 CLEAR FORM
			    HIDE OPTION ALL
			    SHOW OPTION "Consulta", "Alta","Salir"
             IF consulta_ley() THEN
					 IF v_cuantos = 1 THEN
					    HIDE OPTION ALL
					    SHOW OPTION "Consulta", "Baja", "acTualiza","Salir"
						 LET tipo_menu = 1
                ELSE
					    HIDE OPTION ALL
					    SHOW OPTION "Consulta", "Baja", "acTualiza","siGuiente", "aNterior","Salir"
						 LET tipo_menu = 1
					 END IF
             ELSE
					 LET tipo_menu = 0
             END IF
         COMMAND "Alta"
             CALL alta_leyenda()
				 LET tipo_menu = 0
			    HIDE OPTION ALL
			    SHOW OPTION "Consulta", "Alta","Salir"
         COMMAND  "Baja"
             CALL borra_leyenda()
				 LET tipo_menu = 0
	          CALL limpia_men()
			    HIDE OPTION ALL
			    SHOW OPTION "Consulta", "Alta","Salir"
         COMMAND  KEY (T) "acTualiza"
             CALL actualiza_leyenda()
				 LET tipo_menu = 0
			    HIDE OPTION ALL
			    SHOW OPTION "Consulta", "Alta","Salir"
         COMMAND KEY (G) "siGuiente"
				 CALL cambia_reg (1)
				 LET tipo_menu = 0
         COMMAND KEY (N) "aNterior"
				 CALL cambia_reg (-1)
				 LET tipo_menu = 0
         COMMAND "Salir"
				 IF tipo_menu = 1 THEN
				    LET tipo_menu = 0
	             LET g_leyenda.leyenda_cod = n_leyenda.leyenda_cod
	             LET g_leyenda.layout_cod = n_leyenda.layout_cod
	             CALL limpia_men()
			       HIDE OPTION ALL
			       SHOW OPTION "Consulta", "Alta","Salir"
				 ELSE
                EXIT MENU
				 END IF

    END MENU
    CLOSE WINDOW ventana_1
END FUNCTION


FUNCTION limpia_men()
	CLEAR FORM
   DISPLAY "" AT 3,66 
   DISPLAY "" AT 3,1 
   DISPLAY "" AT 4,1 
END FUNCTION

FUNCTION cancela()
	CLEAR FORM
   ERROR " PROCESO CANCELADO " 
   DISPLAY "" AT 3,66 
   DISPLAY "" AT 3,1 
   DISPLAY "" AT 4,1 
END FUNCTION

FUNCTION valida_layout(v_cod_layout)

DEFINE v_cod_layout SMALLINT

SELECT "k"
  FROM tab_layout
 WHERE layout_cod = v_cod_layout

IF SQLCA.SQLCODE = NOTFOUND THEN
	ERROR "No existe codigo de layout."
	RETURN FALSE
ELSE
	RETURN TRUE
END IF

END FUNCTION

FUNCTION layout_ventana()

DEFINE lay_element       SMALLINT


OPEN WINDOW w_layout AT 3,20
	  WITH FORM "TABM0512"
     ATTRIBUTE (BORDER)

#DISPLAY " (Ctrl-C) Salir                                          (ESC) Ejecutar  " AT 4,1 #ATTRIBUTE(REVERSE,green)

CALL set_count(10)

LET int_flag = FALSE

DISPLAY ARRAY g_layout TO f_layout.*

CLOSE WINDOW w_layout

LET lay_element = arr_curr()

RETURN g_layout[lay_element].layout_cod, int_flag

END FUNCTION



FUNCTION init_lay()

  DEFINE i         SMALLINT


  DECLARE lay_ptr CURSOR FOR
	 SELECT layout_cod, layout_des     
		FROM tab_layout
		ORDER BY 1


  LET i = 1

  FOREACH lay_ptr INTO g_layout[i].*

	  LET i = i + 1

	  IF i > 60 THEN
		  EXIT FOREACH
     END IF

  END FOREACH

END FUNCTION
#
