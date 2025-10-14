###########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                       #
#Propietario       => E.F.P.                                              #
#Programa TABM070  => CATALOGO DE PATRONES                                #
#Fecha             => 17 DE JULIO DE 2002                                 #
#Por               => STEFANIE DANIELA VERA PIÑA                          #
#Sistema           => TAB                                                 #
###########################################################################
DATABASE safre_af

GLOBALS

   DEFINE g_param_dis   RECORD LIKE glo_parametro.*

   DEFINE aux_pausa     CHAR(1)

   DEFINE g_reg         RECORD LIKE tab_patron.*

   DEFINE l_record ARRAY[32000] OF RECORD
          reg_patronal         char(11),
          reg_fed_contrib      char(13), 
          razon_social         char(50), 
	  tipo_patron          char(1),
          calle                char(40),
          numero               char(20),  
          codpos               char(5), 
          delega               integer,  
          ciudad               smallint, 
          colonia              char(60),     
          estado               smallint,  
          telefono             char(15)
   END RECORD

   DEFINE desdeleg             CHAR(20),
          desciudad            CHAR(20),
          desestad             CHAR(20)  

   DEFINE pos                  SMALLINT,
          usuario              CHAR(8)

   DEFINE
        bandera               ,
        sw_1                  SMALLINT

   DEFINE 
          HOY                  DATE,
          cla_where            CHAR(200),
          sel_where            CHAR(300),
          g_lista              CHAR(300),
          g_impre              CHAR(300),
          hora                 CHAR(08)
END GLOBALS
################################################################################
MAIN
   OPTIONS PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT
   CALL inic()

   LET HOY = TODAY
   LET bandera = FALSE
                                                                   
   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0701" ATTRIBUTE( BORDER)
   DISPLAY " TABM070                    CATALOGO DE PATRONES                                         " AT 3,1 ATTRIBUTE(REVERSE)    
   DISPLAY HOY USING "DD/MM/YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "PATRON "
      COMMAND "Agrega" "Agrega PATRON"
         CALL agrega() #a
      COMMAND "Consulta" "Consulta PATRON"
         CALL consulta() #c
      COMMAND "Modifica" "Modifica PATRON"
         CALL modifica() #m
--      COMMAND "Elimina" "Elimina PATRON"
--         CALL elimina() #e
      COMMAND KEY(S) "Salir" "Salir del Programa"

      EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1
END MAIN
################################################################################
FUNCTION inic()
#--------------
   SELECT USER,*
      INTO   usuario
      FROM   glo_parametro  

   SELECT ruta_spool
      INTO   g_param_dis.ruta_spool
      FROM   glo_parametro
END FUNCTION
################################################################################
FUNCTION Inicializa()
#i-------------------
   LET sw_1 = 0
   INITIALIZE g_reg.* TO NULL
END FUNCTION
################################################################################
FUNCTION agrega()
#a---------------
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( Esc ) Agrega                   (Ctrl-c) Salir                               " AT 1,1 ATTRIBUTE(BOLD)        
                                                                             
   DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

--   LET g_reg.*   = NULL

   LET sw_1 = 0

   INPUT BY NAME  g_reg.reg_patronal, 
                  g_reg.reg_fed_contrib,      
                  g_reg.razon_social,        
		  g_reg.tipo_patron,
                  g_reg.calle,               
                  g_reg.numero,             
                  g_reg.codpos,         
                  g_reg.colonia,     
                  g_reg.delega,           
                  g_reg.ciudad,          
                  g_reg.estado,         
                  g_reg.telefono  WITHOUT DEFAULTS         

      AFTER FIELD reg_patronal
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD telefono       
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD reg_fed_contrib
         END IF                                                                

         IF g_reg.reg_patronal IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD reg_patronal
         ELSE
            SELECT "X"
            FROM    tab_patron      
            WHERE   reg_patronal = g_reg.reg_patronal  

            IF STATUS <> NOTFOUND THEN
               ERROR "   REGISTRO PATRONAL YA EXISTENTE   "
               NEXT FIELD reg_patronal    
            END IF                                 
         END IF

      AFTER FIELD  reg_fed_contrib
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD reg_patronal
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD razon_social  
         END IF                                                                

         IF g_reg.reg_fed_contrib  IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO  "
            NEXT FIELD  reg_fed_contrib
         END IF

      AFTER FIELD razon_social   
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD  reg_fed_contrib
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD tipo_patron  
         END IF                                                                

         IF g_reg.razon_social IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD razon_social   
         END IF  

      AFTER FIELD tipo_patron
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD  razon_social    
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD calle        
         END IF                                                                

         IF g_reg.tipo_patron IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD tipo_patron    
         END IF  

      AFTER FIELD calle
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD tipo_patron      
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD numero 
         END IF

         IF g_reg.calle IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD calle       
         END IF                   

       AFTER FIELD numero      
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD  calle          
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD codpos
         END IF

         IF g_reg.numero  IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD numero      
         END IF                                    

       AFTER FIELD codpos      
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD numero          
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD colonia
         END IF

         IF g_reg.codpos IS NULL THEN
            CALL despliega_codigo_postal() #dcp
            RETURNING g_reg.codpos  ,
                      g_reg.colonia ,
                      g_reg.delega  ,
                      desdeleg      ,
                      g_reg.ciudad  ,
                      desciudad     ,
                      g_reg.estado  ,
                      desestad

            IF g_reg.codpos IS NULL THEN
               NEXT FIELD codpos
	    END IF  

         ELSE
            SELECT "X"
            FROM   tab_codpos
            WHERE  cpos_cod = g_reg.codpos

            IF STATUS = NOTFOUND THEN
               ERROR "CODIGO POSTAL INEXISTENTE"
               NEXT FIELD codpos
            END IF                                                                                        
            CALL despliega_colonias(g_reg.codpos)
            RETURNING g_reg.colonia ,
                      g_reg.delega  ,
                      desdeleg      ,
                      g_reg.ciudad  ,
                      desciudad      ,
                      g_reg.estado  ,
                      desestad               
         END IF

         DISPLAY BY NAME g_reg.colonia ,
                         g_reg.delega  ,
                         g_reg.ciudad  ,
                         g_reg.estado

            DISPLAY desdeleg, desciudad, desestad
                 TO desdeleg, desciudad, desestad        

            NEXT FIELD telefono
           
      AFTER FIELD colonia
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD  codpos 
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD delega
         END IF

         IF g_reg.colonia IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD colonia
         END IF                                       

      AFTER FIELD delega   
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD  colonia
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD ciudad       
         END IF               
     
        IF g_reg.delega  IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD delegaa
         END IF                                               

      AFTER FIELD ciudad   
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD  delega 
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD estado      
         END IF               
     
        IF g_reg.ciudad IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD ciudad
         END IF                                               

      AFTER FIELD estado   
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD  ciudad 
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD telefono    
         END IF                    

        IF g_reg.estado IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD estado
         END IF                                               

      AFTER FIELD telefono
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD  estado
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD reg_patronal
         END IF

         IF g_reg.telefono IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD telefono
         END IF                  
         
      ON KEY ( ESC )

         IF g_reg.reg_patronal IS NULL THEN
             ERROR "   CAMPO NO PUEDE SER NULO   "
             NEXT FIELD reg_patronal
         ELSE
            SELECT "X"
            FROM    tab_patron
            WHERE   reg_patronal = g_reg.reg_patronal

            IF STATUS <> NOTFOUND THEN
               ERROR "   REGISTRO PATRONAL YA EXISTENTE   "      
              NEXT FIELD reg_patronal
            END IF
         END IF                     

         IF g_reg.reg_fed_contrib  IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO  "
            NEXT FIELD  reg_fed_contrib
         END IF

         IF g_reg.razon_social IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD razon_social
         END IF

         IF g_reg.tipo_patron IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD tipo_patron 
         END IF
	
         IF g_reg.calle IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD calle
         END IF

         IF g_reg.numero  IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD numero
         END IF

         IF g_reg.codpos IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD numero
         END IF

         IF g_reg.colonia IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD colonia
         END IF                                               

         IF g_reg.delega  IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD delegaa
         END IF

         IF g_reg.ciudad IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD ciudad
         END IF                  

         IF g_reg.estado IS NULL THEN
           ERROR "   CAMPO NO PUEDE SER NULO   "
           NEXT FIELD estado
         END IF

         IF g_reg.telefono IS NULL THEN
           ERROR "   CAMPO NO PUEDE SER NULO   "
           NEXT FIELD telefono
         END IF                                                 

         INSERT INTO tab_patron  VALUES (g_reg.reg_patronal,
                                         g_reg.reg_fed_contrib,
                                         "",
                                         "",
                                         g_reg.razon_social,
                                         g_reg.calle,
                                         g_reg.numero,
                                         g_reg.colonia,
                                         g_reg.delega,
                                         g_reg.ciudad,
                                         g_reg.estado,
                                         g_reg.codpos,
                                         g_reg.telefono,
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         g_reg.tipo_patron,
                                         USER)
                                             

         ERROR "   REGISTRO INGRESADO   " SLEEP 1
         ERROR ""
         CLEAR FORM
         CALL Inicializa()

         NEXT FIELD reg_patronal
         ON KEY (INTERRUPT)
            CLEAR FORM
            CALL Inicializa()
            EXIT INPUT
         ON KEY (control-c)
            CLEAR FORM
            CALL Inicializa()
            EXIT INPUT
   END INPUT
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION despliega_codigo_postal()
#ff
    DEFINE aux_val          SMALLINT

    DEFINE l_reg ARRAY[32767] OF RECORD
           cpos_cod         char(05),
           colon_desc       char(25)
    END RECORD

    DEFINE reg RECORD
       codpos  smallint,
       colonia char(40),
       deleg   smallint,
       ciudad  smallint,
       estado  smallint
    END RECORD    

    DEFINE
      desdeleg char(40),
      desciuda char(40),
      desestad char(40),
      vestad smallint

    DEFINE pos              SMALLINT

        OPEN WINDOW vent_1 AT 6,3 WITH FORM "TABM0704" ATTRIBUTE(BORDER)
        DISPLAY "                            CODIGOS POSTALES                                       " AT 2,1 ATTRIBUTE(REVERSE) 

            LET int_flag = FALSE  
            CONSTRUCT BY NAME cla_where ON cpos_cod  
               ON KEY (control-m)
                  LET int_flag = FALSE
                  EXIT CONSTRUCT
               ON KEY (control-c)
                  IF int_flag = TRUE THEN
                  EXIT CONSTRUCT
END IF	
            END CONSTRUCT                

            IF int_flag = TRUE THEN
               LET int_flag = FALSE
    	       LET g_reg.codpos  = NULL
               LET g_reg.colonia = NULL
               LET reg.deleg     = NULL
               LET desdeleg      = NULL
               LET reg.ciudad    = NULL
               LET desciuda      = NULL
               LET reg.estado    = NULL
               LET desestad      = NULL
               ERROR "BUSQUEDA CANCELADA..."
               SLEEP 2
               ERROR ""
               CLEAR SCREEN
               CLOSE WINDOW vent_1
               RETURN
                   g_reg.codpos,
                   g_reg.colonia,
                   reg.deleg,
                   desdeleg,                                
                   reg.ciudad,
                   desciuda,
                   reg.estado,
                   desestad
            END IF                          

        WHILE TRUE
            LET sel_where = "SELECT ",
                            " cpos_cod,",
                            " colon_desc",
                            " FROM tab_colonia WHERE ",
                             cla_where CLIPPED ,
                            " ORDER BY 1,2 "                

              LET sel_where = sel_where CLIPPED                                                                    
              PREPARE query FROM sel_where

              DECLARE cur_1 CURSOR FOR query 

              LET pos = 1
              FOREACH cur_1 INTO l_reg[pos].cpos_cod,
                                 l_reg[pos].colon_desc
                 IF pos >= 32000 THEN
                    EXIT FOREACH
                 ELSE
                    LET pos = pos+1
                 END IF                      
              END FOREACH

              CALL SET_COUNT(pos-1)
              DISPLAY ARRAY l_reg TO scr_1.*
                      ON KEY ( INTERRUPT )
                         LET pos = 0
                         LET pos = ARR_CURR()                 
			 LET l_reg[pos].cpos_cod   = NULL
                         LET l_reg[pos].colon_desc = NULL
                         LET reg.deleg = NULL
                         LET desdeleg  = NULL
                         LET reg.ciudad= NULL
                         LET desciuda  = NULL
                         LET reg.estado= NULL
                         LET desestad  = NULL
                         EXIT DISPLAY
                      ON KEY ( CONTROL-M )
                         LET pos = ARR_CURR()

                         SELECT deleg_cod,ciudad_cod,estad_cod
                         INTO   reg.deleg,reg.ciudad,reg.estado
                         FROM   tab_codpos
                         WHERE  cpos_cod = l_reg[pos].cpos_cod

                         SELECT deleg_desc
                         INTO desdeleg
                         FROM safre_af:tab_delegacion
                         WHERE deleg_cod = reg.deleg                 

                         SELECT ciudad_desc INTO desciuda
                         FROM safre_af:tab_ciudad WHERE
                         ciudad_cod = reg.ciudad

                         SELECT estad_desc INTO desestad
                         FROM safre_af:tab_estado WHERE
                         estad_cod = reg.estado

                         EXIT DISPLAY
              END DISPLAY
              IF pos <> 0 THEN
                 EXIT WHILE
              END IF
        END WHILE

        CLOSE WINDOW vent_1
      RETURN
         l_reg[pos].cpos_cod,
         l_reg[pos].colon_desc,
         reg.deleg,
         desdeleg,                                                     
         reg.ciudad,
         desciuda,
         reg.estado,
         desestad
END FUNCTION                            
###############################################################################
FUNCTION despliega_colonias(xcpos_cod)
   DEFINE
      xcpos_cod         CHAR(05),
      aux_val           SMALLINT,
      x_x               CHAR(300),
      x_buscar          CHAR(30),
      pos               SMALLINT,

      reg record
         cod_colon smallint,
         colonia char(40),
         deleg   smallint,
         ciudad  smallint,
         estado  smallint
      end record,

      desdeleg char(40),
      desciuda char(40),
      desestad char(40),

      l_reg ARRAY[1000] OF RECORD
         cod            CHAR(05),
         codigo         INTEGER,
         descripcion    CHAR(40)

      END RECORD                                          

ERROR "BUSCANDO INFORMACION ..."

      DECLARE cur_cp CURSOR FOR

      SELECT  cpos_cod,colon_cod,colon_desc
      FROM    tab_colonia
      WHERE   cpos_cod = xcpos_cod

      LET pos = 1
      FOREACH cur_cp INTO l_reg[pos].*
         LET pos = pos + 1
      END FOREACH

ERROR ""
      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         OPEN WINDOW ventana_cp AT 6,3 WITH FORM "TABM0703" ATTRIBUTE (BORDER)
         DISPLAY " (ENTER) Elegir                                            (Ctrl - C) Salir " AT 1,1 ATTRIBUTE(BOLD)
         DISPLAY "                           C  O  L  O  N  I  A  S                                   " AT 2,1 ATTRIBUTE (REVERSE,BOLD) 
                                                                   
         DISPLAY ARRAY l_reg to scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()

               SELECT deleg_cod,ciudad_cod,estad_cod
               INTO   reg.deleg,reg.ciudad,reg.estado
               FROM   tab_codpos
               WHERE  cpos_cod = l_reg[pos].cod

               SELECT deleg_desc
               INTO   desdeleg
               FROM   tab_delegacion
               WHERE  deleg_cod = reg.deleg

               SELECT ciudad_desc INTO desciuda
               FROM   tab_ciudad 
               WHERE  ciudad_cod = reg.ciudad

               SELECT estad_desc INTO desestad
               FROM   tab_estado
               WHERE  estad_cod = reg.estado 
                                                                 
               EXIT DISPLAY

            ON KEY(INTERRUPT)
               LET pos = ARR_CURR()
               LET l_reg[pos].descripcion = NULL
               LET reg.deleg = NULL
               LET desdeleg  = NULL
               LET reg.ciudad= NULL
               LET desciuda  = NULL
               LET reg.estado= NULL
               LET desestad  = NULL
               EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW ventana_cp
      ELSE
         ERROR "ARCHIVO DE COLONIAS ..... VACIO"
      END IF
      RETURN
         l_reg[pos].descripcion,
         reg.deleg,
         desdeleg,
         reg.ciudad,                                   
         desciuda,
         reg.estado,
         desestad

END FUNCTION              
###############################################################################
FUNCTION consulta()
#c-----------------
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0702" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
                                                                         
      DISPLAY "                                 PATRONES                                     " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON reg_patronal,razon_social FROM reg_patronal,
					  		    razon_social
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "   BUSQUEDA CANCELADA...   "
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT reg_patronal ,",
                      "reg_fed_contrib    ,",
                      "razon_social       ,",
                      "tipo_patron        ,",
                      "calle              ,",
                      "numero             ,",
                      "codpos             ,",
                      "delega             ,",
                      "ciudad             ,",
                      "colonia            ,",
                      "estado             ,",
                      "telefono            ",
                      "FROM tab_patron     ",
                      "WHERE ",
                       cla_where CLIPPED ,
                      "ORDER BY 1 "

      PREPARE query2 FROM sel_where

      DECLARE cur_2 CURSOR FOR query2

      LET pos = 1

      FOREACH cur_2 INTO l_record[pos].*
         LET pos = pos + 1
             IF pos >= 32000 THEN
	        ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
	        EXIT FOREACH
	     END IF
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.*
--            ON KEY (control-p)
--               ERROR "   PROCESANDO IMPRESION...   "
--               CALL Impresion(pos)
            ON KEY (INTERRUPT)
               EXIT DISPLAY
            ON KEY (control-c)

               EXIT DISPLAY
          END DISPLAY
          CLOSE WINDOW ventana_2
       ELSE
          ERROR "   REGISTRO DE PATRON .... NO EXISTE   "
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
       END IF
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION modifica()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)                             
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0702" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Modifica                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE) 
      DISPLAY "                 Escoja con < ENTER > el patron a modificar                     " AT 2,1     
      DISPLAY "                             PATRON                                    " AT 3,1 ATTRIBUTE(REVERSE) 

      LET int_flag = FALSE

           CONSTRUCT cla_where ON reg_patronal,razon_social FROM reg_patronal,  
								 razon_social
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE

            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "   BUSQUEDA CANCELADA...   "
                                                                       
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT reg_patronal,",
                      "reg_fed_contrib    ,",
                      "razon_social       ,",
                      "tipo_patron        ,",
                      "calle              ,",
                      "numero             ,",
                      "codpos             ,",
                      "delega             ,",
                      "ciudad             ,",
                      "colonia            ,",
                      "estado             ,",
                      "telefono            ",
                      "FROM tab_patron     ",
                      "WHERE ",
                       cla_where CLIPPED ,
                      "ORDER BY 1 "

      PREPARE query3 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query3

      LET pos = 1
      FOREACH cursor_3 INTO l_record[pos].*
         LET pos = pos + 1
             IF pos >= 32000 THEN
	        ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
	        EXIT FOREACH
	     END IF
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
                                                                    
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.reg_patronal    =  l_record[pos].reg_patronal
               LET g_reg.reg_fed_contrib =  l_record[pos].reg_fed_contrib 
               LET g_reg.razon_social    =  l_record[pos].razon_social   
               LET g_reg.tipo_patron     =  l_record[pos].tipo_patron 
               LET g_reg.calle           =  l_record[pos].calle          
               LET g_reg.numero          =  l_record[pos].numero    
               LET g_reg.codpos          =  l_record[pos].codpos      
               LET g_reg.colonia         =  l_record[pos].colonia
               LET g_reg.delega          =  l_record[pos].delega    
               LET g_reg.ciudad          =  l_record[pos].ciudad    
               LET g_reg.estado          =  l_record[pos].estado    
               LET g_reg.telefono        =  l_record[pos].telefono
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "   USTED DEBE ESCOJER UN REGISTRO   "
               LET pos = ARR_CURR()
            ON KEY (control-c)
               ERROR "   USTED DEBE ESCOJER UN REGISTRO   "
               LET pos = ARR_CURR()
          END DISPLAY
          CLOSE WINDOW ventana_2
      ELSE
          ERROR "   REGISTRO DE PATRON .... NO EXISTE   "
                                                                          
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
          RETURN

      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)

      DISPLAY BY NAME g_reg.reg_patronal, 
                      g_reg.reg_fed_contrib,      
                      g_reg.razon_social,        
                      g_reg.tipo_patron,        
                      g_reg.calle,               
                      g_reg.numero,             
                      g_reg.codpos,         
                      g_reg.colonia,     
                      g_reg.delega,           
                      g_reg.ciudad,          
                      g_reg.estado,         
                      g_reg.telefono  

      INPUT BY NAME g_reg.reg_fed_contrib,      
                    g_reg.razon_social,        
                    g_reg.tipo_patron,        
                    g_reg.calle,               
                    g_reg.numero,             
                    g_reg.codpos,         
                    g_reg.colonia,     
                    g_reg.delega,           
                    g_reg.ciudad,          
                    g_reg.estado,         
                    g_reg.telefono   WITHOUT DEFAULTS


      AFTER FIELD  reg_fed_contrib
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD telefono     
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD razon_social  
         END IF                                                                

         IF g_reg.reg_fed_contrib  IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO  "
            NEXT FIELD  reg_fed_contrib
         END IF

      AFTER FIELD razon_social   
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD  reg_fed_contrib
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD tipo_patron  
         END IF                                                                

         IF g_reg.razon_social IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD razon_social   
         END IF  

      AFTER FIELD tipo_patron
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD  razon_social    
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD calle        
         END IF                                                                

         IF g_reg.tipo_patron IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD tipo_patron    
         END IF  

      AFTER FIELD calle       
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD tipo_patron
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD numero 
         END IF

         IF g_reg.calle IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD calle       
         END IF                   

       AFTER FIELD numero      
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD  calle          
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD codpos
         END IF

         IF g_reg.numero  IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD numero      
         END IF                                    

       AFTER FIELD codpos      
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD numero          
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD colonia
         END IF

         IF g_reg.codpos IS NULL THEN
            CALL despliega_codigo_postal() #dcp
            RETURNING g_reg.codpos  ,
                      g_reg.colonia ,
                      g_reg.delega  ,
                      desdeleg      ,
                      g_reg.ciudad  ,
                      desciudad     ,
                      g_reg.estado  ,
                      desestad

         ELSE
            SELECT "X"
            FROM   tab_codpos
            WHERE  cpos_cod = g_reg.codpos

            IF STATUS = NOTFOUND THEN
               ERROR "CODIGO POSTAL INEXISTENTE"
               NEXT FIELD codpos
            END IF                                                                                        
            CALL despliega_colonias(g_reg.codpos)
            RETURNING g_reg.colonia ,
                      g_reg.delega  ,
                      desdeleg      ,
                      g_reg.ciudad  ,
                      desciudad      ,
                      g_reg.estado  ,
                      desestad               
         END IF

         DISPLAY BY NAME g_reg.colonia ,
                         g_reg.delega  ,
                         g_reg.ciudad  ,
                         g_reg.estado

            DISPLAY desdeleg, desciudad, desestad
                 TO desdeleg, desciudad, desestad        

            NEXT FIELD telefono
           
      AFTER FIELD colonia
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD  codpos 
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD delega
         END IF

         IF g_reg.colonia IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD colonia
         END IF                                       

      AFTER FIELD delega   
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD  colonia
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD ciudad       
         END IF               
     
        IF g_reg.delega  IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD delegaa
         END IF                                               

      AFTER FIELD ciudad   
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD  delega 
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD estado      
         END IF               
     
        IF g_reg.ciudad IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD ciudad
         END IF                                               

      AFTER FIELD estado   
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD  ciudad 
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD telefono    
         END IF                    

        IF g_reg.estado IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD estado
         END IF                                               

      AFTER FIELD telefono
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD  estado
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD reg_fed_contrib
         END IF

         IF g_reg.telefono IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO   "
            NEXT FIELD telefono
         END IF                  
         
         WHILE TRUE
             PROMPT "Esta seguro S/N ? "
             FOR CHAR aux_pausa
                                                                     
             IF aux_pausa MATCHES "[sS]" THEN
                 UPDATE tab_patron      
                 SET reg_patronal    = g_reg.reg_patronal, 
                     reg_fed_contrib = g_reg.reg_fed_contrib,      
                     razon_social    = g_reg.razon_social,        
                     tipo_patron     = g_reg.tipo_patron,        
                     calle           = g_reg.calle,               
                     numero          = g_reg.numero,             
                     codpos          = g_reg.codpos,         
                     colonia         = g_reg.colonia,     
                     delega          = g_reg.delega,           
                     ciudad          = g_reg.ciudad,          
                     estado          = g_reg.estado,         
                     telefono        = g_reg.telefono  
                WHERE reg_patronal   = g_reg.reg_patronal

                IF SQLCA.SQLCODE != 0 then
                   ERROR "   ERROR EN LA ACTUALIZACION DE PATRONES   "
                   ATTRIBUTE (reverse)
                   SLEEP 2
                   ERROR " "
                ELSE
                   ERROR "   REGISTRO MODIFICADO   "
                   SLEEP 1
                   ERROR ""
                   CLEAR FORM
                                                                         
                   EXIT WHILE
                END IF
             ELSE
                ERROR "   PROCESO DE MODIFICAR,CANCELADO   "
                SLEEP 2
                ERROR " "
                INITIALIZE g_reg.* TO NULL
                CLEAR FORM
                EXIT WHILE
             END IF

             CALL Inicializa()
             END WHILE
             EXIT INPUT
             ON KEY ( INTERRUPT )
                CALL Inicializa()
                CLEAR FORM
                EXIT INPUT
             ON KEY (control-c )
                CALL Inicializa()
                CLEAR FORM
                                                                       
                EXIT INPUT
      END INPUT
   ELSE
      ERROR "   ARCHIVO DE PATRONES VACIO   "
   END IF
   CLEAR SCREEN
END FUNCTION
