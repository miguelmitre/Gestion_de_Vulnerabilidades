###########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                       #
#Propietario       => E.F.P.                                              #
#Programa TABM0601 => CATALOGO DE CONSULTA DE MARCAS ACTIVAS C/DESCRIPCION#
#Fecha             => 06 DE MARZO DE 2002                                 #
#Por               => STEFANIE DANIELA VERA PIÑA                          #
#Sistema           => TAB    .                                            #
###########################################################################

DATABASE safre_af

GLOBALS

   DEFINE g_param_dis          RECORD LIKE glo_parametro.*

   DEFINE l_record ARRAY[32767] OF RECORD
          nss                   CHAR(11),		
          marca_cod             SMALLINT,
          fecha_ini             DATE,
          hora_ini              DATETIME HOUR TO SECOND,
          estado_marca          SMALLINT,
          marca_causa           SMALLINT,
          fecha_causa           DATE    ,
          descri                CHAR(50)
   END RECORD

   DEFINE l_record2   ARRAY[400] OF RECORD
          nss           LIKE cta_his_marca.nss,
          marca_cod     LIKE cta_his_marca.marca_cod,
          fecha_ini     LIKE cta_his_marca.fecha_ini,
          fecha_fin     LIKE cta_his_marca.fecha_fin,
          hora_ini      LIKE cta_his_marca.hora_ini,
          estado_marca  LIKE cta_his_marca.estado_marca,
          rechazo_cod   LIKE cta_his_marca.rechazo_cod,
          marca_causa   LIKE cta_his_marca.marca_causa,
          fecha_causa   LIKE cta_his_marca.fecha_causa
   END RECORD    
   
   DEFINE g_repor  RECORD
          nss                   CHAR(11),
          marca_cod             SMALLINT,
          fecha_ini             DATE,
          hora_ini              DATETIME HOUR TO SECOND,
          estado_marca          SMALLINT,
          marca_causa           SMALLINT,
          fecha_causa           DATE,
          nss2                  LIKE cta_his_marca.nss,
          marca_cod2            LIKE cta_his_marca.marca_cod,
          fecha_ini2            LIKE cta_his_marca.fecha_ini,
          fecha_fin2            LIKE cta_his_marca.fecha_fin,
          hora_ini2             LIKE cta_his_marca.hora_ini,
          estado_marca2         LIKE cta_his_marca.estado_marca,
          rechazo_cod2          LIKE cta_his_marca.rechazo_cod,
          marca_causa2          LIKE cta_his_marca.marca_causa,
          fecha_causa2          LIKE cta_his_marca.fecha_causa,
          descri                CHAR(50)
   END RECORD                                                

   DEFINE g_marca  RECORD LIKE cta_act_marca.*

   DEFINE vnss  CHAR (11)
   DEFINE enter CHAR (1)

   DEFINE
        no_docto              ,
        banderac              ,
        banderah              , 
        sw_1                  SMALLINT
 
   DEFINE usuario              CHAR(08),
          pos                  SMALLINT,
          HOY                  DATE, 
          cla_where            CHAR(200),
          sel_where            CHAR(400),
          g_lista              CHAR(300),
          g_impre              CHAR(300),        
          hora                 CHAR(08)
 
   DEFINE param_1              CHAR(11)
END GLOBALS
############################################################################
MAIN
   OPTIONS PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT
   CALL STARTLOG("TABM0601.log")
   CALL inicio()
   CALL proceso() 
END MAIN
#############################################################################
FUNCTION inicio()
   INITIALIZE param_1 TO NULL
   LET param_1 = ARG_VAL(1)    ##N_SEGURO

   SELECT USER,*
      INTO   usuario
      FROM   glo_parametro

   SELECT ruta_spool
      INTO   g_param_dis.ruta_spool
      FROM   glo_parametro
END FUNCTION          
#############################################################################
FUNCTION proceso()
   LET HOY=today
   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM06011" ATTRIBUTE( BORDER)
   DISPLAY " TABM0601                   MARCAS ACTIVAS                                                     " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD/MM/YYYY" AT 3,67 ATTRIBUTE(REVERSE)     
   MENU " MARCAS ACTIVAS "
      COMMAND "Consulta" "Consulta Marcas Activas"
         CALL Consulta() 
      COMMAND KEY(S) "Salir" "Salir del Programa"
         EXIT MENU
   END MENU
   CLOSE WINDOW ventana_1
END FUNCTION
################################################################################
FUNCTION Consulta()
#c-----------------
   DEFINE ban  SMALLINT

   LET ban = 0

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
##   DISPLAY " (Ctrl-B) Historico             (Ctrl-p) Imprimir             (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY " (Ctrl-B) Historico                                           (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY " Ingrese el NSS a consultar                                   "  AT 2,1
   LET int_flag = FALSE

   IF param_1 IS NULL OR
      param_1 = " "   THEN
      LET ban = 1
   END IF

   CONSTRUCT cla_where ON a.nss FROM nss

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
      RETURN
   END IF

   IF ban = 0 THEN
      LET cla_where = "nss='",param_1 CLIPPED,"'"
   END IF

   LET sel_where = "SELECT a.nss, a.marca_cod, a.fecha_ini, a.hora_ini, ",
		   " a.estado_marca, a.marca_causa, a.fecha_causa, ",
		   " b.marca_desc ",
		   " FROM cta_act_marca a, tab_marca b  ",
		   " WHERE ", cla_where CLIPPED ,
		   " AND   a.marca_cod = b.marca_cod  ",
		   " ORDER BY 1 "

   PREPARE query FROM sel_where

   DECLARE cursor_1 CURSOR FOR query

   LET pos = 1
   FOREACH cursor_1 INTO l_record[pos].*
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
         ON KEY (control-p)
              LET pos = ARR_CURR()     
              LET g_marca.nss           = l_record[pos].nss
              LET g_marca.marca_cod     = l_record[pos].marca_cod
              LET g_marca.fecha_ini     = l_record[pos].fecha_ini
              LET g_marca.hora_ini      = l_record[pos].hora_ini
              LET g_marca.estado_marca  = l_record[pos].estado_marca
              LET g_marca.marca_causa   = l_record[pos].marca_causa
              LET g_marca.fecha_causa   = l_record[pos].fecha_causa
              LET g_repor.nss           = l_record[pos].nss
              LET g_repor.marca_cod     = l_record[pos].marca_cod
              LET g_repor.fecha_ini     = l_record[pos].fecha_ini
              LET g_repor.hora_ini      = l_record[pos].hora_ini
              LET g_repor.estado_marca  = l_record[pos].estado_marca
              LET g_repor.marca_causa   = l_record[pos].marca_causa
              LET g_repor.fecha_causa   = l_record[pos].fecha_causa    
              LET g_repor.descri        = l_record[pos].descri    
              ERROR "PROCESANDO IMPRESION..."
              CALL Impresion2(pos)

         ON KEY (control-b)
            LET pos = ARR_CURR()      
            LET  vnss = l_record[pos].nss
            CALL Historico()   
         ON KEY (control-c)
            EXIT DISPLAY
         ON KEY (INTERRUPT)
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "REGISTRO INEXISTENTE"
      SLEEP 2
      ERROR ""
      CLEAR FORM
   END IF
   CLEAR FORM
END FUNCTION               
################################################################################
FUNCTION Historico()
   LET banderac = 0
   LET banderah = 0
   OPEN WINDOW ventana_2 AT 10,3 WITH FORM "TABM06021" ATTRIBUTE (BORDER)
   DISPLAY " (Ctrl-p) Imprimir             HISTORICO                      (Ctrl-c) Salir     " AT 2,1 ATTRIBUTE(REVERSE) 
   LET int_flag = FALSE

   DECLARE cursor_2 CURSOR FOR 
      SELECT a.nss,
             a.marca_cod,
             a.fecha_ini,
             a.fecha_fin,
             a.hora_ini,
             a.estado_marca,
             a.rechazo_cod,
             a.marca_causa,
             a.fecha_causa
      FROM cta_his_marca a,cta_act_marca
      WHERE a.fecha_fin is not null
      AND a.nss = cta_act_marca.nss
      AND a.nss=vnss
      ORDER BY 1                     

   LET pos = 1
   FOREACH cursor_2 INTO l_record2[pos].*
      LET pos = pos + 1
   END FOREACH                        

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_record2 TO scr_1.*
         ON KEY (control-p)
            ERROR "PROCESANDO IMPRESION..."         
            LET banderah = 1
            CALL Impresion(pos)
         ON KEY (INTERRUPT)
            EXIT DISPLAY
         ON KEY (control-c)
            EXIT DISPLAY
      END DISPLAY
      CLOSE WINDOW ventana_2
      RETURN
   ELSE
      ERROR "REGISTRO INEXISTENTE"
      SLEEP 2
      ERROR ""
      CLOSE WINDOW ventana_2
      RETURN
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Impresion(pos)
#----------------------
   DEFINE i,pos INTEGER

   LET hora = TIME

   LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",usuario CLIPPED, 
                  ".IMPMARCACT",hoy USING "dd-mm-yyyy","_",hora CLIPPED

   START REPORT rpt_marca_activa TO g_impre

      FOR i=1 TO (pos+1)
          LET g_repor.nss2          = l_record2[i].nss
          LET g_repor.marca_cod2    = l_record2[i].marca_cod
          LET g_repor.fecha_ini2    = l_record2[i].fecha_ini
          LET g_repor.fecha_fin2    = l_record2[i].fecha_ini
          LET g_repor.hora_ini2     = l_record2[i].hora_ini
          LET g_repor.estado_marca2 = l_record2[i].estado_marca
          LET g_repor.rechazo_cod2  = l_record2[i].rechazo_cod
          LET g_repor.marca_causa2  = l_record2[i].marca_causa
          LET g_repor.fecha_causa2  = l_record2[i].fecha_causa    

          IF g_repor.nss IS NULL THEN
             EXIT FOR
          END IF

          IF g_repor.nss2 IS NULL THEN
             EXIT FOR                  
          END IF

          OUTPUT TO REPORT rpt_marca_activa (g_repor.*)
      END FOR                      

   FINISH REPORT rpt_marca_activa

   ERROR "LISTADO GENERADO..."
   SLEEP 2                                           
   ERROR ""

   LET g_lista = " lp ",g_impre
   RUN g_lista
END FUNCTION
###############################################################################
REPORT rpt_marca_activa (g_repor)

   DEFINE cont_h INTEGER

   DEFINE g_repor  RECORD
          nss                   CHAR(11),
          marca_cod             SMALLINT,
          fecha_ini             DATE,
          hora_ini              DATETIME HOUR TO SECOND,
          estado_marca          SMALLINT,
          marca_causa           SMALLINT,
          fecha_causa           DATE,
          nss2                  LIKE cta_his_marca.nss,
          marca_cod2            LIKE cta_his_marca.marca_cod,
          fecha_ini2            LIKE cta_his_marca.fecha_ini,
          fecha_fin2            LIKE cta_his_marca.fecha_fin,
          hora_ini2             LIKE cta_his_marca.hora_ini,
          estado_marca2         LIKE cta_his_marca.estado_marca,
          rechazo_cod2          LIKE cta_his_marca.rechazo_cod,
          marca_causa2          LIKE cta_his_marca.marca_causa,
          fecha_causa2          LIKE cta_his_marca.fecha_causa,
          descri                CHAR(50)
   END RECORD
  
   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN 0
      RIGHT MARGIN 0
      PAGE LENGTH 60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
         PRINT COLUMN 01,'\033(s13H\033(s7B'  
         PRINT COLUMN 02," TABM0601"
         PRINT COLUMN 128, TODAY  USING "dd/mm/yyyy"
                                                                 
         PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s7B'

         PRINT COLUMN 28," LISTADO DE CATALOGO DE MARCAS ACTIVAS "

         BEFORE GROUP OF g_repor.nss
            PRINT COLUMN 01,'\033e\033(s218T\033(s13H\033(s7B'
 
            SKIP 2 LINE              
            PRINT COLUMN 05,"NSS",
                  COLUMN 23,"Marca",
                  COLUMN 35,"Fecha Inicial",
                  COLUMN 56,"Hora Inicial",
                  COLUMN 75,"Estado",
                  COLUMN 87,"Marca Causa",
                  COLUMN 103,"Fecha Causa" 
            SKIP 1 LINE
      
            PRINT COLUMN 01,'\033e\033(s218T\033(s13H\033(s7B'
            PRINT COLUMN 05,g_repor.nss                ,
                  COLUMN 21,g_repor.marca_cod          ,
                  COLUMN 37,g_repor.fecha_ini          ,                   
                  COLUMN 58,g_repor.hora_ini           ,
                  COLUMN 73,g_repor.estado_marca       ,
                  COLUMN 87,g_repor.marca_causa        ,
                  COLUMN 108,g_repor.fecha_causa

            SKIP 4 LINE
            PRINT COLUMN 02,"Total de registros : ",COUNT(*) USING "<<<<<"     
            SKIP 5 LINES

            LET cont_h = 0 

            ON EVERY ROW
               LET cont_h = cont_h +1
               IF banderah = 1 THEN
                  IF cont_h = 1 THEN   
                     PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s7B'  
                     PRINT COLUMN 28," LISTADO DE HISTORICO DE MARCAS ACTIVAS "   
                     PRINT COLUMN 01,'\033e\033(s218T\033(s13H\033(s7B'

                     SKIP 2 LINE

                     PRINT COLUMN 05,"NSS"           ,
                           COLUMN 19,"Marca"         ,
                           COLUMN 27,"Fecha Inicial" ,
                           COLUMN 43,"Fecha Final"   ,
                           COLUMN 56,"Hora Inicial"  ,
                           COLUMN 70,"Estado"        ,
                           COLUMN 78,"Codigo Rechazo",
                           COLUMN 95,"Marca Causa"   ,
                           COLUMN 109,"Fecha Causa"
  
                     SKIP 1 LINE
     
                     PRINT COLUMN 01,'\033e\033(s218T\033(s13H\033(s7B'
                     PRINT COLUMN 05,g_repor.nss2               ,
                           COLUMN 17,g_repor.marca_cod2         ,
                           COLUMN 29,g_repor.fecha_ini2         ,   
                           COLUMN 44,g_repor.fecha_fin2         ,
                           COLUMN 58,g_repor.hora_ini2          ,
                           COLUMN 69,g_repor.estado_marca2      ,
                           COLUMN 82,g_repor.rechazo_cod2       ,
                           COLUMN 96,g_repor.marca_causa2       ,
                           COLUMN 110,g_repor.fecha_causa2                    

                  ELSE  
                     PRINT COLUMN 01,'\033e\033(s218T\033(s13H\033(s7B'
                     PRINT COLUMN 05,g_repor.nss2               ,
                           COLUMN 17,g_repor.marca_cod2         ,
                           COLUMN 29,g_repor.fecha_ini2         ,
                           COLUMN 44,g_repor.fecha_fin2         ,
                           COLUMN 58,g_repor.hora_ini2          ,
                           COLUMN 69,g_repor.estado_marca2      ,
                           COLUMN 82,g_repor.rechazo_cod2       ,
                           COLUMN 96,g_repor.marca_causa2       ,
                           COLUMN 110,g_repor.fecha_causa2        
                  END IF
              END IF

         AFTER GROUP OF  g_repor.nss
               SKIP 3 LINE
                     PRINT COLUMN 04,"Total de registros : ",COUNT(*) USING "<<<<<"
                PAGE TRAILER
                    SKIP 2 LINE
                     PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"     
END REPORT                                                          
###############################################################################
FUNCTION Impresion2(pos)
#----------------------
   DEFINE i,pos INTEGER
   LET hora = TIME

   LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",usuario CLIPPED,
                  ".MARCACTIVA",hoy USING "dd-mm-yyyy","_",hora CLIPPED

   START REPORT rpt_cta_act_marca TO g_impre

   OUTPUT TO REPORT rpt_cta_act_marca (g_marca.*)

   FINISH REPORT rpt_cta_act_marca

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = " lp ",g_impre
   RUN g_lista
END FUNCTION
###############################################################################
REPORT rpt_cta_act_marca (g_marca)

   DEFINE g_marca RECORD LIKE cta_act_marca.*

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN 0
      RIGHT MARGIN 0
      PAGE LENGTH 60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
         PRINT COLUMN 01,'\033(s13H\033(s7B'
         PRINT COLUMN 02," TABM0601"
         PRINT COLUMN 128, TODAY  USING "dd/mm/yyyy"

         PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s7B'
 
         PRINT COLUMN 28," LISTADO DE CATALOGO DE MARCAS ACTIVAS "
  
         PRINT COLUMN 01,'\033e\033(s218T\033(s13H\033(s7B'

         SKIP 2 LINE

          PRINT COLUMN 05,"NSS",
                COLUMN 23,"Marca",
                COLUMN 35,"Fecha Inicial",
                COLUMN 56,"Hora Inicial", 
                COLUMN 75,"Estado",
                COLUMN 87,"Marca Causa",
                COLUMN 103,"Fecha Causa"

         SKIP 1 LINE

      ON EVERY ROW
         PRINT COLUMN 01,'\033e\033(s218T\033(s13H\033(s7B'
         PRINT COLUMN 05,g_marca.nss                ,
               COLUMN 21,g_marca.marca_cod          ,
               COLUMN 37,g_marca.fecha_ini          ,
               COLUMN 58,g_marca.hora_ini           ,
               COLUMN 73,g_marca.estado_marca       ,
               COLUMN 87,g_marca.marca_causa        ,
               COLUMN 108,g_marca.fecha_causa

      PAGE TRAILER
         SKIP 3 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW                                         
         SKIP 2 LINE
         PRINT COLUMN 02,"Total de registros : ",COUNT(*) USING "<<<<<"
END REPORT                        
