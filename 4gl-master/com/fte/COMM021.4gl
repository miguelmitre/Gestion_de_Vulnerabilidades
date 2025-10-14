################################################################
#Proyecto          => Sistema de Afore. ( MEXICO )	            #
#Propietario       => E.F.P.                                   #
#Programa          => COMM021                                  #
#Descripcion       => CATALOGO DE FACTOR PERDIDA.              #
#Sistema           => COM. 				                        #
#Por               => MIGUEL ANGEL HERNAND MARTINEZ.	         #
#Fecha             => 24 Enero de 2001.                        #
#Cambio por        => GERARDO ALFONSO VEGA PAREDES. 	         #
#Fecha             => 16 marzo de 2001.                        #
################################################################
DATABASE safre_af
GLOBALS
   DEFINE 
      g_param_dis RECORD LIKE com_parametro.*

   DEFINE 
      aux_pausa CHAR(1),
      vusuario	CHAR(8),
      vcuantos  SMALLINT,
      hoy	DATE

   DEFINE g_reg RECORD LIKE com_parametro.*

END GLOBALS

MAIN
   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o
   DEFER INTERRUPT

   CALL inicio()

   CALL proceso()

END MAIN

FUNCTION inicio()
   SELECT USER,*
     INTO vusuario
     FROM glo_parametro

   SELECT ruta_spool
     INTO g_param_dis.ruta_spool
     FROM com_parametro
END FUNCTION

FUNCTION proceso()
   LET HOY = TODAY
   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "COMM0211" ATTRIBUTE( BORDER)
   DISPLAY " COMM021                NOMBRES DE NIVELES                                     " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
   MENU "CATALOGO"
      COMMAND "Agrega"    "Agrega metas "
	 CALL Agrega()
      COMMAND "Consulta"  "Consulta metas"
	 CALL Consulta()
      COMMAND "Modifica"  "Modifica metas "
         CALL Modifica()
      COMMAND "Elimina"   "Elimina metas "
         CALL Elimina()
      COMMAND "Salir"     "Salir del Programa"
	 EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1
END FUNCTION

FUNCTION Inicializa()
   INITIALIZE g_reg.* TO NULL
   DISPLAY BY NAME g_reg.*
END FUNCTION

FUNCTION Agrega()
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( Esc ) Agrega             (Ctrl-c) Salir " AT 1,1 
   DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

   INPUT BY NAME g_reg.*
       
      BEFORE FIELD nivel_5
         LET g_reg.factualiza = TODAY
         LET g_reg.usuario = vusuario
         
         DISPLAY BY NAME g_reg.factualiza,g_reg.usuario

         SELECT COUNT(*)
           INTO vcuantos
           FROM com_parametro
           IF vcuantos > 0 THEN
              ERROR "No puede tener mas de un registro"
              EXIT INPUT
           END IF

      AFTER FIELD nivel_5
         IF g_reg.nivel_5 IS NULL THEN
	    ERROR "Nivel 5 NO puede ser nulo"
	    NEXT FIELD  nivel_5	
	 END IF
      AFTER FIELD meta_factor_n5	
let g_reg.meta_factor_n5 = 0
--	 IF g_reg.meta_factor_n5 IS NULL THEN
--	    ERROR "Factor perdida nivel 5 NO puede ser nulo"
--	    NEXT FIELD  meta_factor_n5	
--	 END IF
                
         IF g_reg.meta_factor_n5 > 100 THEN              
            ERROR "Sobre pasa el porcentaje..."     
            NEXT FIELD meta_factor_n5               
         END IF                                      

      AFTER FIELD nivel_4	
	 IF g_reg.nivel_4 IS NULL THEN
	    ERROR "Nivel 4 NO puede ser nulo"
	    NEXT FIELD  nivel_4	
	 END IF
      AFTER FIELD meta_factor_n4	
let g_reg.meta_factor_n4 = 0
--	 IF g_reg.meta_factor_n4 IS NULL THEN
--	    ERROR "Factor perdida nivel 4 NO puede ser nulo"
--	    NEXT FIELD  meta_factor_n4	
--	 END IF
                  
         IF g_reg.meta_factor_n4 > 100 THEN              
            ERROR "Sobre pasa el porcentaje..."     
            NEXT FIELD meta_factor_n4               
         END IF                                      

      AFTER FIELD nivel_3	
	 IF g_reg.nivel_3 IS NULL THEN
	    ERROR "Nivel 3 NO puede ser nulo"
	    NEXT FIELD  nivel_3	
	 END IF
      AFTER FIELD meta_factor_n3	
let g_reg.meta_factor_n3 = 0
--	 IF g_reg.meta_factor_n3 IS NULL THEN
--	    ERROR "Factor perdida nivel 3 NO puede ser nulo"
--	    NEXT FIELD  meta_factor_n3	
--	 END IF
                  
         IF g_reg.meta_factor_n3 > 100 THEN              
            ERROR "Sobre pasa el porcentaje..."     
            NEXT FIELD meta_factor_n3               
         END IF                                      

      AFTER FIELD nivel_2	
	 IF g_reg.nivel_2 IS NULL THEN
	    ERROR "Nivel 2 NO puede ser nulo"
	    NEXT FIELD  nivel_2	
	 END IF
      AFTER FIELD meta_factor_n2	
let g_reg.meta_factor_n2 = 0
--	 IF g_reg.meta_factor_n2 IS NULL THEN
--	    ERROR "Factor perdida nivel 2 NO puede ser nulo"
--	    NEXT FIELD  meta_factor_n2	
--	 END IF
                  
         IF g_reg.meta_factor_n2 > 100 THEN              
            ERROR "Sobre pasa el porcentaje..."     
            NEXT FIELD meta_factor_n2               
         END IF                                      

      AFTER FIELD nivel_1	
	 IF g_reg.nivel_1 IS NULL THEN
	    ERROR "Nivel 1 NO puede ser nulo"
	    NEXT FIELD  nivel_1	
	 END IF
      AFTER FIELD meta_factor_n1	
let g_reg.meta_factor_n1 = 0
--         IF g_reg.meta_factor_n1 IS NULL THEN
--	    ERROR "Factor perdida nivel 1 NO puede ser nulo"
--	    NEXT FIELD  meta_factor_n1	
--	 END IF
                  
         IF g_reg.meta_factor_n1 > 100 THEN              
            ERROR "Sobre pasa el porcentaje..."     
            NEXT FIELD meta_factor_n1               
         END IF                                      

      AFTER FIELD ruta_spool	
         IF g_reg.ruta_spool IS NULL THEN
	    ERROR "Ruta de listados NO puede ser nulo"
	    NEXT FIELD  ruta_spool	
	 END IF
      AFTER FIELD ruta_ejecuta_com	
         IF g_reg.ruta_ejecuta_com IS NULL THEN
	    ERROR "Ruta de ejecutables NO puede ser nulo"
	    NEXT FIELD  ruta_ejecuta_com	
	 END IF

      ON KEY ( ESC )

	 IF g_reg.nivel_5 IS NULL THEN
	    ERROR "Nivel 5 NO puede ser nulo"
	    NEXT FIELD  nivel_5	
	 END IF
	     
--	 IF g_reg.meta_factor_n5 IS NULL THEN
--let g_reg.meta_factor_n5 = 0
--	    ERROR "Factor perdida nivel 5 NO puede ser nulo"
--	    NEXT FIELD  meta_factor_n5	
--	 END IF
                  
         IF g_reg.meta_factor_n5 > 100 THEN              
            ERROR "Sobre pasa el porcentaje..."     
            NEXT FIELD meta_factor_n5               
         END IF                                      

	 IF g_reg.nivel_4 IS NULL THEN
	    ERROR "Nivel 4 NO puede ser nulo"
	     NEXT FIELD  nivel_4	
	 END IF
	    
--	 IF g_reg.meta_factor_n4 IS NULL THEN
--let g_reg.meta_factor_n4 = 0
--	    ERROR "Factor perdida nivel 4 NO puede ser nulo"
--	     NEXT FIELD  meta_factor_n4	
--	 END IF
                  
         IF g_reg.meta_factor_n4 > 100 THEN              
            ERROR "Sobre pasa el porcentaje..."     
            NEXT FIELD meta_factor_n4               
         END IF                                      

	 IF g_reg.nivel_3 IS NULL THEN
	    ERROR "Nivel 3 NO puede ser nulo"
	    NEXT FIELD  nivel_3	
	 END IF
	    
--	 IF g_reg.meta_factor_n3 IS NULL THEN
--let g_reg.meta_factor_n3 = 0
--	    ERROR "Factor perdida nivel 3 NO puede ser nulo"
--	    NEXT FIELD  meta_factor_n3	
--	 END IF
                  
         IF g_reg.meta_factor_n3 > 100 THEN              
            ERROR "Sobre pasa el porcentaje..."     
            NEXT FIELD meta_factor_n3               
         END IF                                      

	 IF g_reg.nivel_2 IS NULL THEN
	    ERROR "Nivel 2 NO puede ser nulo"
	    NEXT FIELD  nivel_2	
	 END IF
	    
--	 IF g_reg.meta_factor_n2 IS NULL THEN
--let g_reg.meta_factor_n2 = 0
--	    ERROR "Factor perdida nivel 2 NO puede ser nulo"
--	    NEXT FIELD  meta_factor_n2	
--	 END IF
                  
         IF g_reg.meta_factor_n2 > 100 THEN              
            ERROR "Sobre pasa el porcentaje..."     
            NEXT FIELD meta_factor_n2               
         END IF                                      

	 IF g_reg.nivel_1 IS NULL THEN
	    ERROR "Nivel 1 NO puede ser nulo"
	    NEXT FIELD  nivel_1	
	 END IF
	   
--	 IF g_reg.meta_factor_n1 IS NULL THEN
--let g_reg.meta_factor_n1 = 0
--	    ERROR "Factor perdida nivel 1 NO puede ser nulo"
--	    NEXT FIELD  meta_factor_n1	
--	 END IF
                  
         IF g_reg.meta_factor_n1 > 100 THEN              
            ERROR "Sobre pasa el porcentaje..."     
            NEXT FIELD meta_factor_n1               
         END IF                                      

	 IF g_reg.ruta_spool IS NULL THEN
	    ERROR "Ruta de listados NO puede ser nulo"
	    NEXT FIELD  ruta_spool	
	 END IF
	    
	 IF g_reg.ruta_ejecuta_com IS NULL THEN
	    ERROR "Ruta de ejecutables NO puede ser nulo"
	    NEXT FIELD  ruta_ejecuta_com	
	 END IF

         INSERT INTO com_parametro 
            VALUES (g_reg.*)
         ERROR "REGISTRO INGRESADO" 
         SLEEP 2
         ERROR ""

         CALL Inicializa()
         EXIT INPUT
      ON KEY (INTERRUPT)
         CALL Inicializa()
         EXIT INPUT
   END INPUT
END FUNCTION

FUNCTION Consulta()
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( Enter ) Salir  " AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

   SELECT * 
     INTO g_reg.*
     FROM com_parametro 
     IF SQLCA.SQLCODE = 0 THEN
        DISPLAY BY NAME g_reg.*
        PROMPT "Oprima <ENTER> para salir de la consulta" ATTRIBUTE(REVERSE)
        FOR CHAR aux_pausa ATTRIBUTE(REVERSE)
     ELSE
	ERROR "ARCHIVO ... VACIO"
        SLEEP 2
	ERROR ""
     END IF
   CALL Inicializa()
END FUNCTION

FUNCTION  Modifica()
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( Esc ) Modifica                 (Ctrl-c) Salir " AT 1,1 
   DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

   SELECT * 
     INTO g_reg.*
     FROM com_parametro 
     IF SQLCA.SQLCODE = 0 THEN
        INPUT BY NAME g_reg.* WITHOUT DEFAULTS 
	   AFTER FIELD nivel_5	
	      IF g_reg.nivel_5 IS NULL THEN
	         ERROR "Nivel 5 NO puede ser nulo"
	         NEXT FIELD  nivel_5	
	      END IF
	   AFTER FIELD meta_factor_n5	
let g_reg.meta_factor_n5 = 0
--	      IF g_reg.meta_factor_n5 IS NULL THEN
--	         ERROR "Factor perdida nivel 5 NO puede ser nulo"
--		 NEXT FIELD  meta_factor_n5	
--	      END IF
                    
              IF g_reg.meta_factor_n5 > 100 THEN              
                 ERROR "Sobre pasa el porcentaje..."     
                 NEXT FIELD meta_factor_n5               
              END IF                                      

	   AFTER FIELD nivel_4	
	      IF g_reg.nivel_4 IS NULL THEN
	         ERROR "Nivel 4 NO puede ser nulo"
	         NEXT FIELD  nivel_4	
	      END IF
	   AFTER FIELD meta_factor_n4	
let g_reg.meta_factor_n4 = 0
--	      IF g_reg.meta_factor_n4 IS NULL THEN
--	         ERROR "Factor perdida nivel 4 NO puede ser nulo"
--	         NEXT FIELD  meta_factor_n4	
--	      END IF
                  
              IF g_reg.meta_factor_n4 > 100 THEN              
                 ERROR "Sobre pasa el porcentaje..."     
                 NEXT FIELD meta_factor_n4               
              END IF                                      

	   AFTER FIELD nivel_3	
	      IF g_reg.nivel_3 IS NULL THEN
	         ERROR "Nivel 3 NO puede ser nulo"
	         NEXT FIELD  nivel_3	
	      END IF
	   AFTER FIELD meta_factor_n3	
let g_reg.meta_factor_n3 = 0
--	      IF g_reg.meta_factor_n3 IS NULL THEN
--	         ERROR "Factor perdida nivel 3 NO puede ser nulo"
--	         NEXT FIELD  meta_factor_n3	
--	      END IF
                  
              IF g_reg.meta_factor_n3 > 100 THEN              
                 ERROR "Sobre pasa el porcentaje..."     
                 NEXT FIELD meta_factor_n3               
              END IF                                      

	   AFTER FIELD nivel_2	
	      IF g_reg.nivel_2 IS NULL THEN
	         ERROR "Nivel 2 NO puede ser nulo"
	         NEXT FIELD  nivel_2	
	      END IF
	   AFTER FIELD meta_factor_n2	
let g_reg.meta_factor_n2 = 0
--	      IF g_reg.meta_factor_n2 IS NULL THEN
--	         ERROR "Factor perdida nivel 2 NO puede ser nulo"
--	         NEXT FIELD  meta_factor_n2	
--	      END IF
                  
              IF g_reg.meta_factor_n2 > 100 THEN              
                 ERROR "Sobre pasa el porcentaje..."     
                 NEXT FIELD meta_factor_n2               
              END IF                                      

	   AFTER FIELD nivel_1	
	      IF g_reg.nivel_1 IS NULL THEN
	         ERROR "Nivel 1 NO puede ser nulo"
	         NEXT FIELD  nivel_1	
	      END IF
	   AFTER FIELD meta_factor_n1	
let g_reg.meta_factor_n1 = 0
--	      IF g_reg.meta_factor_n1 IS NULL THEN
--	         ERROR "Factor perdida nivel 1 NO puede ser nulo"
--	         NEXT FIELD  meta_factor_n1	
--	      END IF
                  
              IF g_reg.meta_factor_n1 > 100 THEN              
                 ERROR "Sobre pasa el porcentaje..."     
                 NEXT FIELD meta_factor_n1               
              END IF                                      

	   AFTER FIELD ruta_spool	
	      IF g_reg.ruta_spool IS NULL THEN
	         ERROR "Ruta de listados NO puede ser nulo"
	         NEXT FIELD  ruta_spool	
	      END IF

	   AFTER FIELD ruta_ejecuta_com	
       	      IF g_reg.ruta_ejecuta_com IS NULL THEN
		 ERROR "Ruta de ejecutables NO puede ser nulo"
		 NEXT FIELD  ruta_ejecuta_com	
   	      END IF

	   ON KEY ( ESC )
   	      IF g_reg.nivel_5 IS NULL THEN
		 ERROR "Nivel 5 NO puede ser nulo"
		 NEXT FIELD  nivel_5	
	      END IF
	     
--	      IF g_reg.meta_factor_n5 IS NULL THEN
--let g_reg.meta_factor_n5 = 0
--	         ERROR "Factor perdida nivel 5 NO puede ser nulo"
--	         NEXT FIELD  meta_factor_n5	
--	      END IF
                  
              IF g_reg.meta_factor_n5 > 100 THEN              
                 ERROR "Sobre pasa el porcentaje..."     
                 NEXT FIELD meta_factor_n5               
              END IF                                      

	      IF g_reg.nivel_4 IS NULL THEN
	         ERROR "Nivel 4 NO puede ser nulo"
	         NEXT FIELD  nivel_4	
	      END IF
	    
--	      IF g_reg.meta_factor_n4 IS NULL THEN
--let g_reg.meta_factor_n4 = 0
--	         ERROR "Factor perdida nivel 4 NO puede ser nulo"
--	         NEXT FIELD  meta_factor_n4	
--	      END IF
                  
              IF g_reg.meta_factor_n4 > 100 THEN              
                 ERROR "Sobre pasa el porcentaje..."     
                 NEXT FIELD meta_factor_n4               
              END IF                                      

	      IF g_reg.nivel_3 IS NULL THEN
	         ERROR "Nivel 3 NO puede ser nulo"
	         NEXT FIELD  nivel_3	
	      END IF
	    
--	      IF g_reg.meta_factor_n3 IS NULL THEN
--let g_reg.meta_factor_n3 = 0
--	         ERROR "Factor perdida nivel 3 NO puede ser nulo"
--	         NEXT FIELD  meta_factor_n3	
--	      END IF
                  
              IF g_reg.meta_factor_n3 > 100 THEN              
                 ERROR "Sobre pasa el porcentaje..."     
                 NEXT FIELD meta_factor_n3               
              END IF                                      

	      IF g_reg.nivel_2 IS NULL THEN
	         ERROR "Nivel 2 NO puede ser nulo"
	         NEXT FIELD  nivel_2	
	      END IF
	    
--	      IF g_reg.meta_factor_n2 IS NULL THEN
--let g_reg.meta_factor_n2 = 0
--	         ERROR "Factor perdida nivel 2 NO puede ser nulo"
--	         NEXT FIELD  meta_factor_n2	
--	      END IF
                  
              IF g_reg.meta_factor_n2 > 100 THEN              
                 ERROR "Sobre pasa el porcentaje..."     
                 NEXT FIELD meta_factor_n2               
              END IF                                      

	      IF g_reg.nivel_1 IS NULL THEN
	         ERROR "Nivel 1 NO puede ser nulo"
	         NEXT FIELD  nivel_1	
	      END IF
	  
--	      IF g_reg.meta_factor_n1 IS NULL THEN
--let g_reg.meta_factor_n1 = 0
--	         ERROR "Factor perdida nivel 1 NO puede ser nulo"
--	         NEXT FIELD  meta_factor_n1	
--	      END IF
                  
              IF g_reg.meta_factor_n1 > 100 THEN              
                 ERROR "Sobre pasa el porcentaje..."     
                 NEXT FIELD meta_factor_n1               
              END IF                                      

	      IF g_reg.ruta_spool IS NULL THEN
	         ERROR "Ruta de listados NO puede ser nulo"
	         NEXT FIELD  ruta_spool	
	      END IF
	    
	      IF g_reg.ruta_ejecuta_com IS NULL THEN
	         ERROR "Ruta de ejecutables NO puede ser nulo"
	         NEXT FIELD  ruta_ejecuta_com	
	      END IF
      
              UPDATE com_parametro
                 SET cod_nivel_5      = g_reg.cod_nivel_5,
                     cod_nivel_4      = g_reg.cod_nivel_4,
                     cod_nivel_3      = g_reg.cod_nivel_3,
                     cod_nivel_2      = g_reg.cod_nivel_2,
                     cod_nivel_1      = g_reg.cod_nivel_1,
                     nivel_5          = g_reg.nivel_5, 
                     nivel_4          = g_reg.nivel_4,
                     nivel_3          = g_reg.nivel_3,
                     nivel_2          = g_reg.nivel_2,
                     nivel_1          = g_reg.nivel_1,
                     indicador_pago5  = g_reg.indicador_pago5,
                     indicador_pago4  = g_reg.indicador_pago4,
                     indicador_pago3  = g_reg.indicador_pago3,
                     indicador_pago2  = g_reg.indicador_pago2,
                     indicador_pago1  = g_reg.indicador_pago1,
                     meta_factor_n5   = g_reg.meta_factor_n5,
                     meta_factor_n4   = g_reg.meta_factor_n4,
                     meta_factor_n3   = g_reg.meta_factor_n3,
                     meta_factor_n2   = g_reg.meta_factor_n2,
                     meta_factor_n1   = g_reg.meta_factor_n1,
                     ruta_spool       = g_reg.ruta_spool,
                     ruta_ejecuta_com = g_reg.ruta_ejecuta_com,
                     factualiza       = HOY,
                     usuario          = vusuario 

   	      ERROR "REGISTRO MODIFICADO" 
              SLEEP 2
              ERROR ""

              CALL Inicializa()
              EXIT INPUT
           ON KEY (INTERRUPT)
              CALL Inicializa()
              EXIT INPUT
        END INPUT
     ELSE
	   ERROR "ARCHIVO  ... VACIO"
           SLEEP 2
	   ERROR ""
     END IF

   CALL Inicializa()
END FUNCTION
################################################################################
FUNCTION Elimina()
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " (Ctrl-c) Salir " AT 1,1 ATTRIBUTE(BOLD)  
   DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

   SELECT * 
     INTO g_reg.*
     FROM com_parametro 
     IF SQLCA.SQLCODE = 0 THEN
        DISPLAY BY NAME g_reg.*

        CALL Pregunta()

        IF aux_pausa MATCHES "[Ss]" THEN
	   DELETE FROM com_parametro
           ERROR "REGISTRO ELIMINADO."
	   SLEEP 2
	   ERROR ""
        ELSE
	   ERROR "PROCESO CANCELADO."
	   SLEEP 2
	   ERROR ""
	END IF

	CALL Inicializa()
     ELSE
	ERROR "ARCHIVO  ... VACIO."
     END IF
   CALL Inicializa()
END FUNCTION
################################################################################
FUNCTION Pregunta()
   PROMPT "Esta seguro S/N ? " ATTRIBUTE (REVERSE)
      FOR CHAR aux_pausa ATTRIBUTE (REVERSE)
END FUNCTION
################################################################################
