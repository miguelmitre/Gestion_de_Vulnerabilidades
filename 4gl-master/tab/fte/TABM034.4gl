################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#Propietario       => E.F.P.        					       #
#Programa TABM034  => CONSULTADOR DE VALOR DE ACCIONES. 
#Fecha             => 29 Julio 1997.   					       #
#Autor             => JUAN COLIN M.                			       #
#Fecha actualiza   => 12 de ENERO 2000.                                        #
#Actualizado por   => MIGUEL ANGEL HERNANDEZ MARTINEZ.                         #
#Fecha actualiza   => 15 de DICIEMBRE 2004.                                    #
#Actualizado por   => MARIA ISABEL FONSECA FRIAS                               #
#Sistema           => TAB. 					               #
################################################################################
DATABASE safre_af
GLOBALS

    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE g   RECORD
        codigo_siefore        SMALLINT,
        fecha_valuacion       DATE,
        precio_del_dia        DECIMAL(19,14)
    END RECORD

    DEFINE g_reg11 RECORD
        generar CHAR(20)
    END RECORD

    DEFINE g_regg RECORD
        siefore             SMALLINT,
        fecha_valuacion     DATE,
        precio_del_dia      DECIMAL(19,14)
    END RECORD

    DEFINE g_reg RECORD 
	codigo_siefore LIKE tab_siefore_local.codigo_siefore,
	razon_social   LIKE tab_siefore_local.razon_social,
        desde          DATE,
        hasta          DATE
    END RECORD

       DEFINE g_regi  ARRAY[3000] OF RECORD 
        codigo_siefore       SMALLINT,   
        fecha_valuacion      DATE,   
        precio_del_dia       DECIMAL(19,14) 
       END RECORD

       DEFINE g_reg1  ARRAY[1000] OF RECORD 
              fecha_valuacion          LIKE glo_valor_accion.fecha_valuacion,
              codigo_siefore           LIKE glo_valor_accion.codigo_siefore,
              razon_social             LIKE tab_siefore_local.razon_social,
              precio_del_dia           CHAR(20), 
            #  monto_comis_saldos       LIKE glo_valor_accion.monto_comis_saldos,
              rowid                    INTEGER 
       END RECORD

       DEFINE g_reg2  RECORD 
	      acc_circ_cap_fijo        LIKE glo_valor_accion.acc_circ_cap_fijo,
	      acc_circ_res_esp         LIKE glo_valor_accion.acc_circ_res_esp,
	      acc_circ_inv_perm        LIKE glo_valor_accion.acc_circ_inv_perm,
	      acc_circ_traba           LIKE glo_valor_accion.acc_circ_traba,
	      acc_circ_traba_vol       LIKE glo_valor_accion.acc_circ_traba_vol,
	      acc_circ_inv_afore       LIKE glo_valor_accion.acc_circ_inv_afore,
  	      acc_circ_totales         LIKE glo_valor_accion.acc_circ_totales,
              rowid                    INTEGER 
       END RECORD

       DEFINE g_reg3  RECORD 
              codigo_siefore           LIKE glo_valor_accion.codigo_siefore,
              razon_social             LIKE tab_siefore_local.razon_social,
              precio_del_dia           LIKE glo_valor_accion.precio_del_dia, 
              fecha_valuacion          LIKE glo_valor_accion.fecha_valuacion,
              rowid                    INTEGER 
       END RECORD

       DEFINE l_record   ARRAY[1000] OF RECORD
              codigo_siefore    LIKE tab_siefore_local.codigo_siefore,
              razon_social      LIKE tab_siefore_local.razon_social
       END RECORD

       DEFINE accion		       CHAR(1),
              ok,
              enter		       CHAR(1),
              i			       INTEGER,
              aux_pausa		       CHAR(1),
              HOY		       DATE,
              vquery1                  CHAR(300),
              vsiefo_cod               CHAR(5),
              vquery                   CHAR(300),
              vflag                    CHAR(1),
              resp                     CHAR(1),
              modo                     CHAR(1),
              modod                    CHAR(10),
              mododd                   CHAR(26),
              modeli                   CHAR(1),
              modmod                   CHAR(1),
              rsocial                  CHAR(50),
              pos                      SMALLINT,
              pos1                     SMALLINT,
              pos2                     SMALLINT,
              g_usuario                CHAR(08),
              cargaa                   CHAR(60),
              g_vpd                    SMALLINT,
              vprecio_del_dia          CHAR(20),
              vprecio_del_dia1         CHAR(20),
              v_codigo_siefore         SMALLINT,
              v_fecha_valuacion        DATE,
              v_precio_del_dia         DECIMAL(19,14),
              cont_reg                 SMALLINT
    DEFINE 
              gimpresion               CHAR(300),
              G_LISTA                  CHAR(300),
              G_IMPRE                  CHAR(300)


    DEFINE g_paramgrales RECORD LIKE seg_modulo.*



END GLOBALS
##########################################################################
MAIN

        CALL inicio () #i
        CALL STARTLOG("TABM034.log")
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT
        CALL proceso()
END MAIN
##########################################################################
FUNCTION inicio()
#----------------
    SELECT *, USER
    INTO   g_paramgrales.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'tab'

    WHENEVER ERROR CONTINUE
        DROP TABLE tem_val
    WHENEVER ERROR STOP

        CREATE TEMP TABLE tem_val
        (campo_sie SMALLINT,
         campo1    DATE,
         campo2    DECIMAL(19,14))

    SELECT  *
    INTO    g_seg_modulo.*
    FROM    seg_modulo
    WHERE   modulo_cod = 'tab'

    LET HOY = TODAY

END FUNCTION

FUNCTION proceso()
#----------------
        
	OPEN WINDOW ventana_1 AT 3,2 WITH FORM "TABM0341" ATTRIBUTE( BORDER)
	DISPLAY " TABM034                  POSICION TOTAL DE ACC/PART                           " AT 3,1 ATTRIBUTE(REVERSE)
	DISPLAY "                                                                               " AT 9,1 ATTRIBUTE(REVERSE)
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
	INITIALIZE modo TO NULL 
	INITIALIZE modod TO NULL 
	INITIALIZE mododd TO NULL 
	MENU "MENU ACC/PART"
           COMMAND "Agrega" "Agrega acciones / participaciones"
              LET modo = "1"
              LET modod = " Agrega"
              LET mododd = " [Enter] Agrega registro                         "
	      CALL Carga()
           COMMAND "Modifica" "Modifica acciones / participaciones"
              LET modo = "2"
              LET modod = " Modifica"
              LET mododd = " [Enter] Modifica registro                       "
	      CALL Carga()
           COMMAND "Elimina" "Elimina acciones / participaciones"
              LET modo = "3"
              LET modod = " Elimina "
              LET mododd = " [Enter] Elimina registro                        "
	      CALL Carga()
           COMMAND "Consulta" "Consulta acciones / participaciones"
              LET modo = "4"
              LET modod = " Consulta "
              LET mododd = "Consulta registro                                "
	      CALL Carga()
           COMMAND KEY ("R") "aRchivo" "Procesa Archivo                      "
	      CALL procesa_archivo()
           COMMAND KEY ( "S" )"Salir" "Salir del Programa"
	      EXIT MENU
	END MENU
        CLOSE WINDOW ventana_1
END FUNCTION
################################################################################
FUNCTION Inicializa()
#----------------
	INITIALIZE g_reg.* TO NULL
	DISPLAY BY NAME g_reg.*
	INITIALIZE g_reg1 TO NULL
	CLEAR FORM
	FOR i=1 TO 50
	  LET g_reg1[i].fecha_valuacion    = NULL 
	  # LET g_reg1[i].monto_comis_saldos = NULL
	END FOR
END FUNCTION
################################################################################
FUNCTION Carga()
#----------------
         DEFINE HAY_REGISTROS		SMALLINT	

         DISPLAY "" AT 1,1
	 DISPLAY "" AT 2,1
	 DISPLAY modod AT 2,1 ATTRIBUTE(REVERSE)
	 DISPLAY " ( Esc ) Carga                                              (Ctrl-c) Salir " AT 1,1 ATTRIBUTE(BOLD)

	 INPUT BY NAME g_reg.*
            BEFORE FIELD codigo_siefore
               IF modo = "1" OR modo = "2" OR modo = "3" THEN
                  --LET g_reg.codigo_siefore = 11
		  --DISPLAY BY NAME g_reg.codigo_siefore
               END IF
	    AFTER FIELD codigo_siefore
	       IF g_reg.codigo_siefore IS NULL THEN
	          CALL Muestra() 
		  IF ok = "N" THEN 
		     NEXT FIELD codigo_siefore
	          END IF
	       ELSE   
                  SELECT codigo_siefore,
                         razon_social 
		  INTO   g_reg.codigo_siefore, 
                         g_reg.razon_social 
                  FROM   tab_siefore_local
                  WHERE  codigo_siefore = g_reg.codigo_siefore

	          IF STATUS = NOTFOUND THEN
		     ERROR "NO EXISTE SIEFORE EN CATALOGO"
	             NEXT FIELD codigo_siefore
	          END IF  
	          INITIALIZE rsocial TO NULL
                  LET rsocial = g_reg.razon_social
		  DISPLAY BY NAME g_reg.razon_social
	       END IF
 # se modifico issa 06
	       IF (g_reg.codigo_siefore = 11 OR
	          g_reg.codigo_siefore = 12) THEN 
                 IF modo = "1" THEN
                  CALL despliega_a11()
                  EXIT INPUT
                 ELSE
                  NEXT FIELD desde
                 END IF  
               ELSE
                IF modo = "1" OR modo = "2" OR modo = "3" THEN
                 ERROR "Opcion no valida"
	       NEXT FIELD codigo_siefore
                END IF
               END IF 
	       NEXT FIELD desde
	    AFTER FIELD desde
	       IF g_reg.desde IS NULL THEN
	          ERROR "Debe Ingresar una Fecha."
		  NEXT FIELD desde
	       END IF

	    AFTER FIELD hasta
	       IF g_reg.hasta IS NULL THEN
		  ERROR "Debe Ingresar una Fecha."
		  NEXT FIELD hasta
	       END IF
	           
	    ON KEY (INTERRUPT)
	       LET accion = "1"
	       EXIT INPUT
            ON KEY (ESC)   
	       LET accion = "2"

               SELECT "X" 
               FROM   glo_valor_accion
               WHERE fecha_valuacion BETWEEN g_reg.desde AND g_reg.hasta 
               GROUP BY 1

	       IF STATUS = NOTFOUND THEN
	          ERROR "NO EXISTEN DATOS PARA ESA FECHA."
	          CALL Inicializa()
	          NEXT FIELD codigo_siefore
	       END IF
	       EXIT INPUT
	 END INPUT

      IF (g_reg.codigo_siefore = 11 OR
         g_reg.codigo_siefore = 12) AND modo = "1" THEN
	    CALL INICIALIZA()
      ELSE
         CALL Carga1()
      END IF
END FUNCTION
#############################################################################
FUNCTION Carga1() 
#----------------
         DEFINE HAY_REGISTROS		SMALLINT	
         DISPLAY "" AT 1,1
	 DISPLAY "" AT 2,1
	 DISPLAY mododd AT 2,1 ATTRIBUTE(REVERSE)
         IF modo = '4' THEN
	     DISPLAY " (Ctrl-p) Mas Datos                                          (Ctrl-c) Salir   " AT 1,1 ATTRIBUTE(BOLD)
         ELSE
	    DISPLAY "                                                             (Ctrl-c) Salir   " AT 1,1 ATTRIBUTE(BOLD)
         END IF
	 IF accion = "1" THEN
	    CALL INICIALIZA()
	    RETURN
	 END IF

         IF g_reg.codigo_siefore = 11 THEN
             DISPLAY "Part Viv" AT 10,19
         END IF

         IF g_reg.codigo_siefore = 12 THEN
             DISPLAY "Part Fov" AT 10,19
         END IF

         IF g_reg.codigo_siefore = 1 OR
            g_reg.codigo_siefore = 2 OR
            g_reg.codigo_siefore = 3 OR
            g_reg.codigo_siefore = 4 OR
            g_reg.codigo_siefore = 5 THEN
             DISPLAY "SIEFORE " AT 10,19
         END IF
         
         DECLARE cur_1 CURSOR FOR
	 SELECT a.fecha_valuacion,
                a.codigo_siefore,
                b.razon_social, 
                a.precio_del_dia,
              #  a.monto_comis_saldos,
                a.rowid 
         FROM   glo_valor_accion a, tab_siefore_local b
 	 WHERE a.codigo_siefore = g_reg.codigo_siefore  
           AND a.codigo_siefore = b.codigo_siefore
           AND fecha_valuacion BETWEEN g_reg.desde AND g_reg.hasta
	 ORDER BY 1

         LET pos = 1
         LET HAY_REGISTROS = FALSE

	 FOREACH cur_1 INTO g_reg1[pos].*
            LET HAY_REGISTROS = TRUE
            IF g_reg1[pos].codigo_siefore = 1 OR 
               g_reg1[pos].codigo_siefore = 2 THEN 
               LET vprecio_del_dia = g_reg1[pos].precio_del_dia
                                                USING "###.######" 
               LET g_reg1[pos].precio_del_dia = vprecio_del_dia
            END IF
            LET pos = pos + 1
         END FOREACH

         CALL SET_COUNT(pos-1)

         DISPLAY ARRAY g_reg1 TO scr_1.*
            ON KEY ( CONTROL-M )
              CASE modo
              WHEN  "2"  # Modifica
                CALL despliega_m11()
               LET pos = 0
	       CALL INICIALIZA()
               EXIT DISPLAY
              WHEN  "3"  # Elimina	
                CALL despliega_e11()
               LET pos = 0
	       CALL INICIALIZA()
               EXIT DISPLAY
              END CASE
            ON KEY (INTERRUPT)
               LET pos = 0
	       CALL INICIALIZA()
               EXIT DISPLAY
            ON KEY ( CONTROL-P )
               IF modo = '4' THEN 
                  CALL despliega_acc()
               END IF
	 END DISPLAY
END FUNCTION
################################################################################
FUNCTION Muestra()
#----------------
	DEFINE HAY_REGISTROS			SMALLINT

	OPEN WINDOW ventana_2 AT 6,2 WITH FORM "TABM0342" ATTRIBUTE( BORDER)
	DISPLAY " CONSULTA " AT 1,67
	DISPLAY " (Ctrl-C) Salir " AT 1,1 
	DISPLAY "                             S I E F O R E S                                   " AT 2,1 ATTRIBUTE(REVERSE,BOLD) 
   	
        DECLARE prog_1 cursor FOR
        SELECT codigo_siefore, 
               razon_social 
        FROM   tab_siefore_local
        ORDER BY 1

        LET pos = 1
        LET HAY_REGISTROS = FALSE
        FOREACH prog_1 INTO l_record[pos].*
           LET HAY_REGISTROS = TRUE
           LET pos = pos + 1
        END FOREACH

	LET ok = "S"

        IF NOT HAY_REGISTROS THEN
           ERROR "NO SE ENCONTRARON REGISTROS CON LAS CONDICIONES"
           ATTRIBUTE (REVERSE)
           SLEEP 1
           ERROR " "
           INITIALIZE vquery TO NULL
           CLOSE WINDOW ventana_2
	   LET ok = "N"
           RETURN 
        END IF 

        CALL SET_COUNT(pos-1)
        DISPLAY ARRAY l_record TO scr_1.*
           ON KEY ( INTERRUPT)
              LET pos = 0 
	      LET ok = "N"
              EXIT DISPLAY
           ON KEY ( CONTROL-M )
              LET pos = ARR_CURR()
              LET g_reg.codigo_siefore = l_record[pos].codigo_siefore
	      LET g_reg.razon_social = l_record[pos].razon_social
              EXIT DISPLAY
        END DISPLAY

        CLOSE WINDOW ventana_2 
        DISPLAY BY NAME g_reg.codigo_siefore, g_reg.razon_social
END FUNCTION
################################################################################
FUNCTION Pregunta()
#----------------
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION

################################################################################
FUNCTION despliega_acc()
#----------------
	OPEN WINDOW ventana_x AT 3,2 WITH FORM "TABM0343" ATTRIBUTE( BORDER)
	DISPLAY " TABM034                 POSICION TOTAL DE ACCIONES                                  " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

        CALL SET_COUNT(pos-1)

        LET pos = ARR_CURR()
        LET g_reg2.rowid = g_reg1[pos].rowid

        SELECT acc_circ_cap_fijo,
               acc_circ_res_esp,
               acc_circ_inv_perm,
               acc_circ_traba,
               acc_circ_traba_vol,
               acc_circ_inv_afore,
               acc_circ_totales,
               rowid 
        INTO   g_reg2.*
        FROM   glo_valor_accion
        WHERE  rowid = g_reg2.rowid 

   LET g_reg2.acc_circ_cap_fijo  = g_reg2.acc_circ_cap_fijo  USING "######.##"
   LET g_reg2.acc_circ_res_esp   = g_reg2.acc_circ_res_esp   USING "######.##"
   LET g_reg2.acc_circ_inv_perm  = g_reg2.acc_circ_inv_perm  USING "######.##"
   LET g_reg2.acc_circ_traba     = g_reg2.acc_circ_traba     USING "######.##"
   LET g_reg2.acc_circ_traba_vol = g_reg2.acc_circ_traba_vol USING "######.##"
   LET g_reg2.acc_circ_inv_afore = g_reg2.acc_circ_inv_afore USING "######.##"
   LET g_reg2.acc_circ_totales   = g_reg2.acc_circ_totales   USING "######.##"

        DISPLAY BY NAME  g_reg2.acc_circ_cap_fijo,
                          g_reg2.acc_circ_res_esp,
                          g_reg2.acc_circ_inv_perm,
                          g_reg2.acc_circ_traba,
                          g_reg2.acc_circ_traba_vol,
                          g_reg2.acc_circ_inv_afore,
                          g_reg2.acc_circ_totales,
                          g_reg2.rowid
        PROMPT "Presiones <ENTER> para continuar..." FOR CHAR enter 
        CLOSE WINDOW ventana_x
END FUNCTION
################################################################################
FUNCTION despliega_e11()
#----------------
	OPEN WINDOW ventana_y AT 3,2 WITH FORM "TABM0344" ATTRIBUTE( BORDER)
	DISPLAY " TABM034                       POSICION TOTAL DE ACCIONES                               " AT 3,1 ATTRIBUTE(REVERSE) 
        DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
       
        CALL SET_COUNT(pos-1)
    
	LET modeli = 'S'
        LET pos = ARR_CURR()
        LET g_reg3.rowid = g_reg1[pos].rowid
        LET g_reg3.codigo_siefore = g_reg1[pos].codigo_siefore
        LET g_reg3.fecha_valuacion = g_reg1[pos].fecha_valuacion
        LET g_reg3.precio_del_dia = g_reg1[pos].precio_del_dia
        


        DISPLAY BY NAME g_reg3.*
        CALL Pregunta()	
        IF aux_pausa MATCHES "[Ss]" THEN
   # valida en dis_cuenta
               SELECT "X" 
               FROM   dis_cuenta
               WHERE fecha_conversion = g_reg3.fecha_valuacion 
                         AND siefore = g_reg3.codigo_siefore 
               GROUP BY 1
	       IF SQLCA.SQLCODE = 0 THEN
                  LET modeli = 'N'
	          ERROR "YA EXISTE registro provisionado y/o liquidado"
                  SLEEP 3
                  ERROR ""
	       ELSE 
   # valida en dis_procision 
                  SELECT "X" 
                  FROM   dis_provision
                  WHERE fecha_conversion = g_reg3.fecha_valuacion 
                            AND siefore = g_reg3.codigo_siefore 
                  GROUP BY 1

	          IF SQLCA.SQLCODE = 0 THEN
                     LET modeli = 'N'
	             ERROR "YA EXISTE registro provisionado y/o liquidado"
                     SLEEP 3
                     ERROR ""

                  END IF
               END IF
               IF modeli = 'S' THEN
                  DELETE FROM glo_valor_accion      
                  WHERE codigo_siefore=g_reg3.codigo_siefore AND 
                        fecha_valuacion=g_reg3.fecha_valuacion
                  ERROR "Registro Eliminado"
                  SLEEP 1
                  ERROR ""
               END IF
        ELSE 
          ERROR " PROCESO CANCELADO"
          SLEEP 1
          ERROR ""
        END IF
        
       CLOSE WINDOW ventana_y
END FUNCTION
###############################################################################
FUNCTION despliega_m11()
#----------------
	OPEN WINDOW ventana_y AT 3,2 WITH FORM "TABM0344" ATTRIBUTE( BORDER)
	DISPLAY mododd AT 2,1 ATTRIBUTE(REVERSE)

	DISPLAY " TABM034                         POSICION TOTAL DE ACCIONES                          " AT 3,1 ATTRIBUTE(REVERSE) 
        DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
       
        CALL SET_COUNT(pos-1)
   
        LET modmod = 'S'  
        LET pos = ARR_CURR()
        LET g_reg3.rowid = g_reg1[pos].rowid
        LET g_reg3.codigo_siefore = g_reg1[pos].codigo_siefore
        LET g_reg3.fecha_valuacion = g_reg1[pos].fecha_valuacion
        LET g_reg3.precio_del_dia = g_reg1[pos].precio_del_dia
        
        INPUT BY NAME g_reg3.* WITHOUT DEFAULTS
          BEFORE FIELD codigo_siefore
           NEXT FIELD precio_del_dia
          AFTER FIELD precio_del_dia
            IF g_reg3.precio_del_dia IS NULL THEN
              ERROR "Precio del dia NO puede ser nulo"
              NEXT FIELD precio_del_dia
            ELSE
            LET vprecio_del_dia1      = g_reg3.precio_del_dia
                                        USING "####.##############" 
            LET g_reg3.precio_del_dia = vprecio_del_dia1
            CALL Pregunta()

              IF aux_pausa MATCHES "[Ss]" THEN
                 #  valida en dis_cuenta
                 SELECT "X" 
                 FROM   dis_cuenta
                 WHERE fecha_conversion = g_reg3.fecha_valuacion 
                           AND siefore = g_reg3.codigo_siefore 
                 GROUP BY 1
	         IF SQLCA.SQLCODE = 0 THEN
                    LET modmod = 'N'
	            ERROR "YA EXISTE registro provisionado y/o liquidado"
                    SLEEP 3
                    ERROR ""
                 ELSE
                 # valida en dis_procision 
                    SELECT "X" 
                    FROM   dis_provision
                    WHERE fecha_conversion = g_reg3.fecha_valuacion 
                              AND siefore = g_reg3.codigo_siefore 
                    GROUP BY 1
	          IF SQLCA.SQLCODE = 0 THEN
                     LET modmod = 'N'
	             ERROR "YA EXISTE registro provisionado y/o liquidado"
                     SLEEP 3
                     ERROR ""

                  END IF
               END IF
                 IF modmod = 'S' THEN
                    UPDATE glo_valor_accion SET
                    precio_del_dia=g_reg3.precio_del_dia,
                    fecha_valuacion=g_reg3.fecha_valuacion
                    WHERE codigo_siefore=g_reg3.codigo_siefore
                          AND fecha_valuacion = g_reg3.fecha_valuacion
                    ERROR "Registro Agregado"
                    SLEEP 1
                    ERROR ""
                    EXIT INPUT #CALL Inicializa()
                 ELSE
                    EXIT INPUT #CALL Inicializa()
                 END IF
              ELSE
                 ERROR "Proceso Cancelado"
                 SLEEP 1
                 ERROR ""
                 EXIT INPUT #CALL Inicializa()
            END IF	
          END IF	
        END INPUT 
                
        CLOSE WINDOW ventana_y
END FUNCTION
################################################################################
FUNCTION despliega_a11()
#----------------
	OPEN WINDOW ventana_y AT 3,2 WITH FORM "TABM0344" ATTRIBUTE( BORDER)
	DISPLAY " TABM034                       POSICION TOTAL DE ACCIONES                            " AT 3,1 ATTRIBUTE(REVERSE) 
        DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
       
	INITIALIZE g_reg3.* TO NULL
        LET g_reg3.codigo_siefore = g_reg.codigo_siefore
        LET g_reg3.razon_social = rsocial

        INPUT BY NAME g_reg3.* WITHOUT DEFAULTS
          BEFORE FIELD codigo_siefore
           NEXT FIELD precio_del_dia
          AFTER FIELD precio_del_dia
            IF g_reg3.precio_del_dia IS NULL THEN
              ERROR "Precio del dia NO puede ser nulo"
              NEXT FIELD precio_del_dia
            ELSE
              NEXT FIELD fecha_valuacion
            END IF
          AFTER FIELD fecha_valuacion
            IF g_reg3.fecha_valuacion IS NULL THEN
	          ERROR "Debe Ingresar una Fecha."
		  NEXT FIELD fecha_valuacion
            ELSE
               SELECT "X" 
               FROM   glo_valor_accion
               WHERE fecha_valuacion = g_reg3.fecha_valuacion 
                         AND codigo_siefore = g_reg3.codigo_siefore 
               GROUP BY 1

	       IF SQLCA.SQLCODE = 0 THEN
	          ERROR "SIEFORE YA INGRESADA, con fecha de valuacion"
		  NEXT FIELD fecha_valuacion
	       ELSE
                 CALL Pregunta()
                 IF aux_pausa MATCHES "[Ss]" THEN
# issa
                     INSERT INTO glo_valor_accion 
                            VALUES(g_reg3.codigo_siefore,
                            g_reg3.precio_del_dia,
                            HOY,g_reg3.fecha_valuacion,0,0,0,0,0,0,0,0)

                     ERROR "Registro Agregado"
                     SLEEP 1
                     ERROR ""
                     EXIT INPUT #CALL Inicializa()
                 ELSE
                     ERROR "Proceso Cancelado"
                     SLEEP 1
                     ERROR ""
                 END IF	
              END IF	
          END IF	
        END INPUT 
                
        CLOSE WINDOW ventana_y

END FUNCTION

FUNCTION Modifica ()   
#----------------
          INPUT BY NAME g_reg2.acc_circ_cap_fijo,  g_reg2.acc_circ_res_esp,
                        g_reg2.acc_circ_inv_perm,  g_reg2.acc_circ_traba,
                        g_reg2.acc_circ_traba_vol, g_reg2.acc_circ_traba_vol,
                        g_reg2.acc_circ_inv_afore, g_reg2.acc_circ_totales
                        WITHOUT DEFAULTS

              AFTER FIELD acc_circ_cap_fijo
              IF g_reg2.acc_circ_cap_fijo IS NULL THEN 
                ERROR "Acciones Circulante de Capitarl Fijo No puede ser NULO"
                NEXT FIELD acc_circ_cap_fijo
              END IF

              AFTER FIELD acc_circ_res_esp
               IF g_reg2.acc_circ_res_esp IS NULL THEN 
                ERROR "Acciones Circulante de Res. Espec. No puede ser NULO"
                NEXT FIELD acc_circ_res_esp
               END IF

              AFTER FIELD acc_circ_inv_perm
               IF g_reg2.acc_circ_inv_perm IS NULL THEN 
                ERROR "Acciones Circulante de Inv. Perma. No puede ser NULO"
                NEXT FIELD acc_circ_inv_perm
               END IF

              AFTER FIELD acc_circ_traba
               IF g_reg2.acc_circ_traba IS NULL THEN 
                ERROR "Acciones Circulante de Trabajadores No puede ser NULO"
                NEXT FIELD acc_circ_traba
               END IF

              AFTER FIELD acc_circ_traba_vol
               IF g_reg2.acc_circ_traba_vol IS NULL THEN 
                ERROR "Acciones Circulante de Traba. Volu. No puede ser NULO"
                NEXT FIELD acc_circ_traba_vol
               END IF

              AFTER FIELD acc_circ_inv_afore
               IF g_reg2.acc_circ_inv_afore IS NULL THEN 
                ERROR "Acciones Circulante de Inv. Afore No puede ser NULO"
                NEXT FIELD acc_circ_inv_afore
               ELSE  
              
                 LET g_reg2.acc_circ_totales = g_reg2.acc_circ_cap_fijo +
                                               g_reg2.acc_circ_res_esp + 
                                               g_reg2.acc_circ_inv_perm +
                                               g_reg2.acc_circ_traba +
                                               g_reg2.acc_circ_traba_vol +
                                               g_reg2.acc_circ_inv_afore
                DISPLAY BY NAME  g_reg2.acc_circ_totales
               END IF
	 
              CALL Pregunta ()
               IF aux_pausa MATCHES "[Ss]" THEN
                 ERROR "Se actualizara el registro"
                 SLEEP 1
                 ERROR ""
               ELSE
                 ERROR "Accion cancelada"
                 SLEEP 1
                 ERROR ""
               END IF
              EXIT INPUT
          END INPUT   
 
END FUNCTION 
FUNCTION procesa_archivo()
#pp-------------------------

    OPEN WINDOW ventana_11 AT 3,2 WITH FORM "TABM0345" ATTRIBUTE(BORDER)
    DISPLAY " TABM034                  CARGA ARCHIVO                                        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)    
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    DISPLAY g_paramgrales.ruta_rescate AT 6,35

    INPUT BY NAME g_reg11.generar
        AFTER FIELD generar
        IF g_reg11.generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        LET cargaa = NULL
        LET cargaa = g_paramgrales.ruta_rescate CLIPPED,"/",
                     g_reg11.generar CLIPPED

        WHENEVER ERROR CONTINUE
           LOAD FROM cargaa DELIMITER ',' INSERT INTO tem_val
        WHENEVER ERROR STOP

        SELECT COUNT(*)
        INTO   g_vpd
        FROM   tem_val

        IF g_vpd IS NULL OR g_vpd = 0 THEN
            ERROR "NOMBRE DE ARCHIVO INCORRECTO O VACIO, VERIFIQUE..."
            SLEEP 5
            EXIT INPUT
        END IF

        ERROR "Procesando Informacion"
        SLEEP 1

        CALL carga_tabla()


    FINISH REPORT rpt_val_part

    LET gimpresion = "lp ",G_IMPRE
    RUN gimpresion

    ERROR "LISTADO IMPRESO"
    SLEEP 2
    ERROR ""

        EXIT  INPUT

    END INPUT

        CLOSE WINDOW ventana_11
END FUNCTION

FUNCTION carga_tabla()
#-------------------------
    DEFINE ii,
           i SMALLINT
    DEFINE hora               CHAR(8)

    LET hora        = TIME
    LET hora = hora[1,2],hora[4,5],hora[7,8]
    LET G_IMPRE = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                  ".LIS_VAL_PART.",HOY USING "DDMMYY", "_",hora CLIPPED

DECLARE cur_12 CURSOR FOR

    SELECT  *
    FROM    tem_val

    LET pos1 = 0
    LET pos2 = 0
    START REPORT rpt_val_part TO G_IMPRE

    FOREACH cur_12 INTO g_regg.*
         
         SELECT 'X'
           FROM glo_valor_accion
          WHERE codigo_siefore  = g_regg.siefore
            AND fecha_valuacion = g_regg.fecha_valuacion
          GROUP BY 1
         IF STATUS = NOTFOUND THEN

            INSERT INTO glo_valor_accion 
            VALUES(g_regg.siefore, g_regg.precio_del_dia, g_regg.fecha_valuacion,
                   g_regg.fecha_valuacion,0,0,0,0,0,0,0,0)

            LET pos1 = pos1 + 1

            DISPLAY "TOTAL DE REGISTROS: ", pos1 AT 14,04
         ELSE
            LET pos2 = pos2 + 1
            CALL imprimir(pos2)
         END IF

    END FOREACH

    IF pos1 = 0 THEN
       DISPLAY "ARCHIVO YA INGRESADO O VACIO" AT 14,04
    END IF

    PROMPT "Proceso finalizado, presione <ENTER> para salir." FOR enter

END FUNCTION

FUNCTION imprimir(i)
#impr--------------

    DEFINE ii,
           i SMALLINT

          LET g.codigo_siefore       = g_regg.siefore
          LET g.fecha_valuacion      = g_regg.fecha_valuacion
          LET g.precio_del_dia       = g_regg.precio_del_dia

             OUTPUT TO REPORT rpt_val_part(g.*)


    --LET gimpresion = "lp ",G_IMPRE
    --RUN gimpresion}

END FUNCTION

REPORT rpt_val_part(g)
#rci--------------------

    DEFINE g   RECORD
        codigo_siefore        SMALLINT,
        fecha_valuacion       DATE,
        precio_del_dia        DECIMAL(19,14)
    END RECORD

    OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60
    FORMAT
      PAGE HEADER
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 2,  "TABM034",
              COLUMN 25, "REGISTROS NO ASIGNADOS",
              COLUMN 55, TODAY USING "mm/dd/yyyy"
        SKIP 2 LINE
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'


     	PRINT COLUMN  2, "SIEFORE",
     	      COLUMN 23, "FECHA VALUACION",
              COLUMN 44, "      AIV'S  DEL DIA"

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

     ON EVERY ROW

        PRINT COLUMN  1 ,g.codigo_siefore,
              COLUMN  25,g.fecha_valuacion,
              COLUMN  43,g.precio_del_dia

       ON LAST ROW
         SKIP 4 LINES
         PRINT COLUMN 2, "Total Registro : ",
               COLUMN 15, pos2
END REPORT

