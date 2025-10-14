########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                    #
#Owner             => EFP                                              #
#Programa TABM052  => CATALOGO DE IDENTIFICADOR APORTANTES
#Fecha             => 08 DE JUNIO 2001.                                #
#Por               => STEFANIE DANIELA VERA PIÑA
#Sistema           => TAB.                                             #
########################################################################
DATABASE safre_af
GLOBALS
   DEFINE g_param_dis          RECORD LIKE glo_parametro.*

   DEFINE gr_regis        RECORD
          modulo          CHAR(3),
          id_aportante    CHAR(11),
          longitud        SMALLINT,
          origen          CHAR(15),
          descripcion     CHAR(50)
   END RECORD

   DEFINE l_record1   ARRAY[3000] OF RECORD
          modulo           CHAR(3),
          id_aportante     CHAR(11),
          descripcion      CHAR(50)
   END RECORD                         

   DEFINE l_record2  ARRAY[3000] OF RECORD
          
          modulo           CHAR(3),
          id_aportante     CHAR(11),
          longitud         SMALLINT,
          origen           CHAR(15),
          descripcion      CHAR(50)
                    
   END RECORD                                                               

   DEFINE hoy                  DATE,
          aux_pausa            CHAR(01),
          sw_1                 SMALLINT,
          usuario              CHAR(08),
          siono                CHAR(01),
          pos                  INTEGER,
          cla_where            CHAR(200),
          sel_where            CHAR(200),
          g_lista              CHAR(300),
          g_impre              CHAR(300),
          hora                 CHAR(08)

END GLOBALS 
#############################################################################
MAIN
   OPTIONS 
     PROMPT LINE LAST,
     INPUT WRAP
   
     WHENEVER ERROR CONTINUE

     DEFER INTERRUPT

     CALL inic()
     CALL menu_inic()
END MAIN          
############################################################################
FUNCTION inic()
  SELECT USER,*
    INTO   usuario
    FROM   glo_parametro

    SELECT ruta_spool
      INTO   g_param_dis.ruta_spool
      FROM   glo_parametro
END FUNCTION                                                
###########################################################################
FUNCTION menu_inic()
   LET hoy = TODAY
   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0521" ATTRIBUTE( BORDER)
   DISPLAY " TABM052         CATALOGO DE IDENTIFICADOR DE APORTANTES                       " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "IDENTIFICADOR APORTANTES"
     COMMAND "Agrega" "Agrega id.aportante"
       CALL Agrega()
     COMMAND "Consulta" "Consulta id.aportante"
       CALL Consulta()
     COMMAND "Modifica" "Modifica id.aportante"
       CALL Modifica()
     COMMAND "Elimina" "Elimina id.aportante"
       CALL Elimina()
     COMMAND "Salir" "Salir del Programa"
     EXIT MENU
   END MENU
   CLOSE WINDOW ventana_1
END FUNCTION                             
############################################################################
FUNCTION Inicializa()
  LET sw_1 = 0
  INITIALIZE gr_regis.* TO NULL
  DISPLAY BY NAME gr_regis.*

END FUNCTION
############################################################################
FUNCTION Agrega()

  DEFINE  var     SMALLINT 
   
  LET var = 0
  DISPLAY "" AT 1,1
  DISPLAY "" AT 2,1
  DISPLAY " ( Esc ) Agrega                 (Ctrl-c) Salir                                 " AT 1,1
  DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE)
        
  LET gr_regis.id_aportante= NULL
  LET sw_1 = 0

  INPUT BY NAME  gr_regis.*

    AFTER FIELD modulo
      IF gr_regis.modulo IS NULL THEN
        ERROR "El modulo NO puede ser nulo"
        SLEEP 2
        ERROR " "
        NEXT FIELD modulo
      END IF

    AFTER FIELD id_aportante
      IF gr_regis.id_aportante IS NULL THEN
        ERROR "id.aportanten NO puede ser nulo"
        NEXT FIELD  id_aportante
      END IF

      SELECT a.id_aportante
      FROM  tab_id_aportante a
      WHERE  a.id_aportante= gr_regis.id_aportante

      IF STATUS <> NOTFOUND THEN
        ERROR "id.aportante YA INGRESADO VERIFIQUE ..."
        ATTRIBUTE (REVERSE)
        SLEEP 2
        ERROR " "
        INITIALIZE gr_regis.id_aportante TO NULL
        NEXT FIELD id_aportante
      ELSE
          NEXT FIELD longitud
      END IF

      AFTER FIELD longitud
        IF gr_regis.longitud IS NULL THEN
          ERROR "La longitud NO puede ser nula"
          NEXT FIELD longitud
        END IF                                                       

      AFTER FIELD origen
      IF gr_regis.origen IS NULL THEN
        ERROR "El origen NO puede ser nulo"
        NEXT FIELD origen
      END IF

      AFTER FIELD descripcion
         IF gr_regis.descripcion IS NULL THEN
           ERROR "La descripcion NO puede ser nula"
           NEXT FIELD descripcion
         END IF
                                                      
      ON KEY (ESC)
         IF gr_regis.modulo IS NULL THEN
           ERROR "El modulo NO puede ser nulo"
           SLEEP 2
           ERROR " "
           NEXT FIELD modulo
         END IF

         IF gr_regis.id_aportante IS NULL THEN
           ERROR "id.aportanten NO puede ser nulo"
           NEXT FIELD  id_aportante
         END IF

         IF gr_regis.origen IS NULL THEN
           ERROR "El origen NO puede ser nulo"
           NEXT FIELD origen
         END IF

         IF gr_regis.longitud IS NULL THEN
           ERROR "La longitud NO puede ser nula"
           NEXT FIELD longitud
         END IF                                                       

         IF gr_regis.descripcion IS NULL AND
            gr_regis.descripcion = " "   THEN
           ERROR "La descripcion NO puede ser nula"
           NEXT FIELD descripcion
         END IF
                                                      
      ON KEY (INTERRUPT)                                            
         CALL Inicializa()
         LET var = 1
         EXIT INPUT                                                   

      ON KEY (CONTROL-C)
         CALL Inicializa()
         LET var = 1
         EXIT INPUT

 END INPUT

  IF var = 0 THEN
    INSERT INTO tab_id_aportante VALUES ( gr_regis.modulo,      
                                         gr_regis.id_aportante,
                                         gr_regis.longitud,
                                         gr_regis.origen,
                                         gr_regis.descripcion,  
                                         usuario,
                                         TODAY )              

     ERROR "REGISTRO INGRESADO"
     SLEEP 2
     ERROR ""

     CALL Inicializa()
  ELSE
     ERROR "REGISTRO CANCELADO"
     SLEEP 2
     ERROR ""

     CALL Inicializa()
  END IF
     CLEAR FORM
  CLEAR SCREEN
END FUNCTION                             
############################################################################
FUNCTION Consulta()
  LET pos = 2

  IF (pos-1) >= 1 THEN
    CALL  SET_COUNT(pos-1)
    OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0522" ATTRIBUTE( BORDER)
    DISPLAY " (Enter) Consulta              (Ctrol-p) Impresion           (Ctrl-c)  Salir   " AT 1,1 ATTRIBUTE(REVERSE) 
    DISPLAY "            Escoja con < ENTER > el identificador de aportante                 " AT 2,1
    DISPLAY "                         IDENTIFICADOR APORTANTES                              " AT 3,1 ATTRIBUTE(REVERSE)
    LET int_flag = FALSE

    CONSTRUCT cla_where ON modulo,id_aportante  FROM modulo,id_aportante
      ON KEY (control-m)
      LET int_flag = FALSE
      EXIT CONSTRUCT
      ON KEY (control-c)
      LET int_flag = TRUE
      EXIT CONSTRUCT
    END CONSTRUCT               

    IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..."
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana_2
      RETURN
    END IF

    LET sel_where = "SELECT modulo,id_aportante,descripcion FROM tab_id_aportante WHERE ",
                       cla_where CLIPPED ,"ORDER BY 1 "

    PREPARE query FROM sel_where

    DECLARE cursor_1 CURSOR FOR query

    LET pos = 1
    FOREACH cursor_1 INTO l_record1[pos].*
      LET pos = pos + 1
    END FOREACH

    INITIALIZE l_record1[pos].* TO NULL

      IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)
        DISPLAY ARRAY l_record1 TO scr_1.*
        ON KEY (control-p)
          ERROR "PROCESANDO IMPRESION..."
          CALL Impresion(pos)
        ON KEY (CONTROL-M)
          LET pos = ARR_CURR()
          LET gr_regis.modulo = l_record1[pos].modulo        
          LET gr_regis.id_aportante = l_record1[pos].id_aportante
          LET gr_regis.descripcion = l_record1[pos].descripcion
          EXIT DISPLAY
        ON KEY (INTERRUPT)
          ERROR "Usted debe escojer un registro"
          LET pos = ARR_CURR()              

        ON KEY (control-c)
          ERROR "Usted debe escojer un registro"
          LET pos = ARR_CURR()                                         
        END DISPLAY
        CLOSE WINDOW ventana_2
      ELSE
        ERROR "IDENTIFICADOR DE APORTANTE  .... NO EXISTE"
        SLEEP 2
        ERROR ""
        CLOSE WINDOW ventana_2
        RETURN
      END IF                                

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " (Ctrl-C) Salir " AT 1,1
   DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(REVERSE)

   SELECT modulo,   
          id_aportante,
          descripcion,
          longitud,
          origen
     INTO   gr_regis.modulo,   
            gr_regis.id_aportante,
            gr_regis.descripcion,
            gr_regis.longitud,
            gr_regis.origen
     FROM   tab_id_aportante
     WHERE  id_aportante = gr_regis.id_aportante

   DISPLAY BY NAME gr_regis.modulo,
                   gr_regis.id_aportante,
                   gr_regis.descripcion,
                   gr_regis.longitud,
                   gr_regis.origen

   PROMPT " Oprima ...<ENTER>... Para salir " ATTRIBUTE (REVERSE)
     FOR CHAR aux_pausa ATTRIBUTE (REVERSE)
     CALL Inicializa()
   ELSE
     ERROR "ARCHIVO  VACIO"
   END IF
   ERROR ""
END FUNCTION                                    
############################################################################
FUNCTION  Modifica()
  LET pos=2
    IF (pos-1) >= 1  THEN
      CALL SET_COUNT(pos-1) 
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0522" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "        Escoja con < ENTER > el identificador aportante a modificar                                                                                            " AT 2,1
      DISPLAY "                         IDENTIFICADOR APORTANTES                              " AT 3,1 ATTRIBUTE(REVERSE) 

      LET int_flag = FALSE
      CONSTRUCT cla_where ON modulo,id_aportante  FROM modulo,id_aportante
        ON KEY (control-m)
          LET int_flag = FALSE
          EXIT CONSTRUCT
        ON KEY (control-c)
           LET int_flag = TRUE
           EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        CLOSE WINDOW ventana_2
        RETURN
      END IF

      LET sel_where = "SELECT modulo,id_aportante,descripcion FROM tab_id_aportante WHERE ",
                       cla_where CLIPPED ,"ORDER BY 1 "

      PREPARE query_2 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query_2

      LET pos = 1
      FOREACH cursor_2 INTO l_record1[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record1[pos].* TO NULL

      IF (pos-1) >= 1 THEN
--    	LET sw=0
         CALL SET_COUNT(pos-1)

         DISPLAY ARRAY l_record1 TO scr_1.*
           ON KEY (CONTROL-m)
              LET pos = ARR_CURR()
              LET gr_regis.modulo = l_record1[pos].modulo
              LET gr_regis.id_aportante = l_record1[pos].id_aportante
              LET gr_regis.descripcion = l_record1[pos].descripcion
              EXIT DISPLAY
           ON KEY (INTERRUPT)
              ERROR "Usted debe escojer un registro"
              LET pos = ARR_CURR()

           ON KEY (control-c)
              ERROR "Usted debe escojer un registro"
              LET pos = ARR_CURR()                                    
        END DISPLAY                  
          CLOSE WINDOW ventana_2
       ELSE
         --LET sw=1
         ERROR "REGISTRO DE id.aportante NO EXISTE"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF
     
      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Enter) Modifica                 (Ctrl-C) Salir " AT 1,1 
       DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)

       SELECT a.longitud,
              a.origen,
              a.descripcion
         INTO gr_regis.longitud,
              gr_regis.origen,
              gr_regis.descripcion  
         FROM tab_id_aportante  a 
        WHERE a.id_aportante=gr_regis.id_aportante

    IF STATUS = NOTFOUND THEN
       ERROR  "No trae informacion"
       SLEEP 2
     END IF

     DISPLAY BY NAME gr_regis.modulo,
                     gr_regis.id_aportante,
                     gr_regis.longitud,
                     gr_regis.origen,
                     gr_regis.descripcion

       INPUT BY NAME gr_regis.modulo,
                     gr_regis.longitud,
                     gr_regis.origen,
                     gr_regis.descripcion
                     WITHOUT DEFAULTS

          AFTER FIELD modulo
             IF gr_regis.modulo IS NULL THEN
                ERROR "EL modulo NO puede ser nulo"
                NEXT FIELD  modulo
             END IF

          AFTER FIELD longitud
             IF gr_regis.longitud IS NULL THEN
                ERROR "La longitud NO puede ser nula"
                NEXT FIELD longitud
             END IF
                                        
          AFTER FIELD origen
             IF gr_regis.origen IS NULL THEN
                ERROR "El origen NO puede ser nulo"
                NEXT FIELD origen
             END IF

          AFTER FIELD descripcion
             IF gr_regis.descripcion IS NULL THEN
                ERROR "La descripcion NO puede ser nula"
                NEXT FIELD descripcion
             END IF                   

          PROMPT "Desea Actualizar este Registro S/N ? "
          ATTRIBUTE (REVERSE) FOR aux_pausa

          IF aux_pausa MATCHES "[sS]" THEN

             UPDATE tab_id_aportante 
             SET    modulo       = gr_regis.modulo,
                    longitud     = gr_regis.longitud,
                    origen       = gr_regis.origen,
                    descripcion  = gr_regis.descripcion,
                    usuario     = usuario,
                    factualiza   = HOY
             WHERE  id_aportante = gr_regis.id_aportante

                ERROR "REGISTRO MODIFICADO"
                SLEEP 1
                ERROR ""

                CLEAR FORM 
          ELSE
             ERROR "Operacion Cancelada"
             ATTRIBUTE (REVERSE)
             SLEEP 2
             ERROR " "

             INITIALIZE gr_regis.* TO NULL              
             CLEAR FORM
          END IF

          CALL Inicializa()
          EXIT INPUT
       ON KEY ( INTERRUPT )
          CALL Inicializa()
          EXIT INPUT
      END INPUT
    ELSE
       ERROR "Archivo de Identificadores  Aportantes vacio" 
    END IF
    CLEAR SCREEN
END FUNCTION                       
############################################################################
FUNCTION Elimina()
   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0522" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "        Escoja con < ENTER > el identificador aportante a eliminar             " AT 2,1
      DISPLAY "                         IDENTIFICADOR APORTANTES                              " AT 3,1 ATTRIBUTE(REVERSE)  
      LET int_flag = FALSE

      CONSTRUCT cla_where ON modulo,id_aportante
                        FROM modulo,id_aportante
         ON KEY (control-m)
            LET int_flag = FALSE                
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF
      LET sel_where = "SELECT modulo,id_aportante,descripcion FROM tab_id_aportante WHERE ",
                       cla_where CLIPPED ,
                       " ORDER BY 1,2 "

      PREPARE query2 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query2

      LET pos = 1
      FOREACH cursor_3 INTO l_record1[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record1[pos].* TO NULL       
      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record1 TO scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET gr_regis.modulo   = l_record1[pos].modulo
               LET gr_regis.id_aportante = l_record1[pos].id_aportante
               LET gr_regis.descripcion = l_record1[pos].descripcion
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()

            ON KEY (control-c)
               ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()                  
         END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "NOMBRE INCORRECTO "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " ( Esc ) Elimina                  (Ctrl-C) Salir                               " AT 1,1 
      DISPLAY " ELIMINA " AT 1,67 ATTRIBUTE(REVERSE)

       SELECT a.longitud,
              a.origen,
              a.descripcion
         INTO gr_regis.longitud,
              gr_regis.origen,
              gr_regis.descripcion
         FROM tab_id_aportante  a
        WHERE a.id_aportante=gr_regis.id_aportante                             

     DISPLAY BY NAME gr_regis.modulo,
                     gr_regis.id_aportante,
                     gr_regis.longitud,
                     gr_regis.origen,
                     gr_regis.descripcion

      CALL Pregunta()

      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE
         FROM tab_id_aportante
         WHERE modulo   = gr_regis.modulo
         AND   id_aportante = gr_regis.id_aportante 

         IF SQLCA.SQLCODE != 0 THEN
            ERROR "Error en la Actualizacion "
            ATTRIBUTE (reverse)
         ELSE
            ERROR "REGISTRO ELIMINADO"
            SLEEP 2
            CLEAR FORM
         END IF
      ELSE
         ERROR "OPERACION CANCELADA ..."
         SLEEP 2
         CLEAR FORM
      END IF

      ERROR ""
      CALL Inicializa()
   ELSE
       ERROR "EL REGISTRO NO EXISTE"
   END IF
   ERROR ""
END FUNCTION                               
############################################################################
 FUNCTION Pregunta()
        PROMPT "Esta seguro S/N ? " ATTRIBUTE (REVERSE)
        FOR CHAR aux_pausa
        ATTRIBUTE (REVERSE)
END FUNCTION                                                     
############################################################################
FUNCTION Impresion(pos)
    DEFINE i,pos INTEGER

    LET hora = TIME

    LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",usuario CLIPPED,
                  ".IMPUSUARIO",hoy USING "dd-mm-yyyy","_",hora CLIPPED

    START REPORT rpt_id_aportante TO g_impre

    FOR i=1 TO (pos+1)
        LET gr_regis.modulo          = l_record1[i].modulo
        LET gr_regis.id_aportante    = l_record1[i].id_aportante
        LET gr_regis.descripcion     = l_record1[i].descripcion
 
   SELECT longitud,origen
   INTO gr_regis.longitud,gr_regis.origen
   FROM tab_id_aportante
   WHERE modulo=gr_regis.modulo AND
   id_aportante=gr_regis.id_aportante

        IF gr_regis.id_aportante IS NULL THEN
           EXIT FOR
        END IF
       OUTPUT TO REPORT rpt_id_aportante(gr_regis.*)
    END FOR

    FINISH REPORT rpt_id_aportante

    ERROR "LISTADO GENERADO..."
    SLEEP 2
    ERROR ""

    LET g_lista = "lp ",g_impre
    RUN g_lista
END FUNCTION                         
############################################################################
REPORT rpt_id_aportante(gr_regis)

    DEFINE gr_regis        RECORD
           modulo          CHAR(3),
           id_aportante    CHAR(11),
           longitud        SMALLINT,
           origen          CHAR(15),
           descripcion     CHAR(50)  
    END RECORD              
   OUTPUT
       TOP MARGIN 1
       BOTTOM MARGIN 0
       LEFT MARGIN 0
       RIGHT MARGIN 0
       PAGE LENGTH 60
    FORMAT
    PAGE HEADER
       PRINT COLUMN 02," TABM052 ",
             COLUMN 19," LISTADO DE IDENTIFICADOR DE APORTANTES",
             COLUMN 67, TODAY  USING "dd/mm/yyyy"
       SKIP 2 LINE

       PRINT COLUMN 01,"Mod",
             COLUMN 05,"id.aportante",
             COLUMN 18,"Long",                                   
             COLUMN 24,"Origen",                                   
             COLUMN 44,"Descripcion"
       SKIP 1 LINE

    ON EVERY ROW                       
       PRINT COLUMN 01,gr_regis.modulo,
             COLUMN 05,gr_regis.id_aportante,
             COLUMN 19,gr_regis.longitud USING "##",                             
             COLUMN 23,gr_regis.origen CLIPPED,                             
             COLUMN 36,gr_regis.descripcion CLIPPED

    PAGE TRAILER
       SKIP 2 LINE
       PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

    ON LAST ROW
       SKIP 2 LINE
       PRINT COLUMN 02,"Total de registros : ",COUNT(*) USING "<<<<<"
END REPORT



                                                                                                                                    







                                                                  

                                                            
                                
                                                                


                                                   
                                           
                                   
                                                                             

                                                                





                                                                   


                                              
                                                                                                           



















                                                
