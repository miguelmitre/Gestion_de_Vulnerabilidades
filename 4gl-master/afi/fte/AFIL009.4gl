###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.                                                  #
#Programa AFIL009  => LISTADO DE AFILIADOS POR USUARIO (REGISTRO o TRASPASO)  #
#Por               => JUAN COLIN M.                                           #
#Fecha             => 25 Abril 1997.      				      #
#Actualizacion     => RICARDO ARELLANO M.                                     #
#Fecha             => 19 Enero 1999.      				      #
#Sistema           => AFI. 					              #
###############################################################################
DATABASE safre_af
GLOBALS
   DEFINE g_reg          RECORD
          fecha_ini      DATE,
          fecha_top      DATE,
          usuario        CHAR(08),
          ord            SMALLINT,
          opcion         SMALLINT
   END RECORD

   DEFINE w_aux          RECORD
          n_folio        LIKE afi_solicitud.n_folio,
          tipo_solicitud LIKE afi_solicitud.tipo_solicitud,
          n_seguro       LIKE afi_solicitud.n_seguro,
          n_rfc          LIKE afi_solicitud.n_rfc,
          paterno        LIKE afi_solicitud.paterno,
          materno        LIKE afi_solicitud.materno,
          nombres        LIKE afi_solicitud.nombres,
          cod_promotor         LIKE pro_mae_promotor.cod_promotor,
          ppaterno       LIKE pro_mae_promotor.paterno,
          pmaterno       LIKE pro_mae_promotor.materno,
          pnombres       LIKE pro_mae_promotor.nombres,
          agenc_cod      LIKE pro_mae_promotor.agenc_cod,
          fecha_envio    LIKE afi_solicitud.fecha_envio,
          frecafor       LIKE afi_solicitud.frecafor,
          hora           LIKE afi_solicitud.hora,
          status_captura LIKE afi_solicitud.status_captura,
          status_interno LIKE afi_solicitud.status_interno,
          usuario        LIKE afi_solicitud.usuario
   END RECORD

   DEFINE w_cap          RECORD
          bas,
          comp,
          tras           SMALLINT
   END RECORD

   DEFINE w_edo          RECORD
          cap,
          incomp,
          compl,
          envia,
          recha,
          tr_rech,
          pend,
          aclara,
          aprob,
          certif,
          liquida,
          canc,
          regis          SMALLINT
   END RECORD                       

   DEFINE g_afore        RECORD LIKE tab_afore_local.*
   DEFINE g_paramgral    RECORD LIKE glo_parametro.*
   DEFINE parametrox     CHAR(05)
   DEFINE hoy            DATE
   DEFINE hora           CHAR(05)
   DEFINE estrc          CHAR(16)
   DEFINE aux_pausa	 CHAR(01)
   DEFINE l_estado       CHAR(16)
   DEFINE l_captura      CHAR(16)
   DEFINE g_usuario      CHAR(08)
   DEFINE razon_social   CHAR(40)
   DEFINE tipo_salida    CHAR(19)
   DEFINE tipo_reporte   CHAR(19)
   DEFINE G_LISTA        CHAR(100)
   DEFINE txt1           CHAR(600)

END GLOBALS

MAIN

   OPTIONS  
   PROMPT LINE LAST,
   ACCEPT KEY CONTROL-I,

   FORM LINE 3
   DEFER INTERRUPT

   LET hora = TIME
   LET hoy  = TODAY

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIL0091" ATTRIBUTE(BORDER)
   DISPLAY "AFIL009    LISTADO DE AFILIADOS POR USUARIO (REGISTRO o TRASPASO)              " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING "DD-MM-YYYY" AT 3,63 ATTRIBUTE(REVERSE)

   LET g_reg.fecha_ini = TODAY
   LET g_reg.fecha_top = TODAY
   LET g_reg.ord       = 1
   LET w_edo.cap       = 0
   LET w_cap.bas       = 0
   LET w_cap.comp      = 0
   LET w_cap.tras      = 0
   LET w_edo.incomp    = 0
   LET w_edo.compl     = 0
   LET w_edo.envia     = 0
   LET w_edo.recha     = 0
   LET w_edo.pend      = 0
   LET w_edo.aprob     = 0
   LET w_edo.canc      = 0
   LET w_edo.regis     = 0  

   INPUT BY NAME g_reg.* WITHOUT DEFAULTS 

     AFTER FIELD fecha_ini
     IF g_reg.fecha_ini IS NULL THEN
        ERROR "LA FECHA NO PUEDE SER NULO" SLEEP 2    
        NEXT FIELD fecha_ini
     END IF

     AFTER FIELD fecha_top
     IF fgl_lastkey() = fgl_keyval("UP") THEN
        NEXT FIELD fecha_ini
     END IF

     IF g_reg.fecha_top IS NULL THEN
        ERROR "LA FECHA NO PUEDE SER NULO" SLEEP 2
	NEXT FIELD solic_ini
     END IF

     IF g_reg.fecha_top < g_reg.fecha_ini THEN
        ERROR "FECHA NO PUEDE SER MENOR A LA FECHA INICIAL" SLEEP 2
        NEXT FIELD fecha_top
     END IF

     NEXT FIELD usuario
     IF g_reg.usuario IS NULL THEN
        ERROR "CON USUARIO EN NULO, SE PROCESARAN TODOS LOS USUARIOS" SLEEP 2
     END IF

     AFTER FIELD ord                                            
     IF g_reg.ord IS NULL THEN                                  
        ERROR "EL CRITERIO DE ORDENAMIENTO NO PUEDE SER NULO"   
        NEXT FIELD ord                                          
     END IF                                                     
                                                           
     IF g_reg.ord > 3 OR g_reg.ord = 0 THEN                     
        ERROR "OPCION INCORRECTA ... FAVOR DE VERIFICAR" SLEEP 2
        NEXT FIELD ord                                          
     END IF                                                     
                                                           
     AFTER FIELD opcion                                         
     IF g_reg.opcion IS NULL THEN                               
        ERROR "LA OPCION NO PUEDE SER NULO" SLEEP 2             
        NEXT FIELD opcion                                       
     ELSE                                                       
        IF g_reg.opcion = 0 OR g_reg.opcion > 2 THEN            
           ERROR "LA OPCION SOLO PUEDE SER 1 o 2" SLEEP 2       
           NEXT FIELD opcion                                    
        END IF                                                  
     END IF                                                     

     EXIT INPUT
   END INPUT

   WHILE TRUE                                                   
     PROMPT "DESEA EMITIR INFORME [S/N] " FOR aux_pausa  
     IF aux_pausa MATCHES "[SsNn]" THEN                         
        EXIT WHILE                                              
     END IF                                                     
   END WHILE                                                    
                                                             
   IF aux_pausa MATCHES "[Nn]" THEN                             
      ERROR "PROCESO CANCELADO" SLEEP 2                         
   ELSE                                                         
      ERROR "PROCESANDO INFORMACION ... ESPERE UN MOMENTO"      
                                                             
      SELECT *, USER                                            
      INTO   g_afore.*, g_usuario                               
      FROM   tab_afore_local                                           
                                                             
      SELECT *                                                  
      INTO   g_paramgral.*                                      
      FROM   glo_parametro                                        
                                                             
      IF g_reg.opcion = 1 THEN                                            
         LET tipo_salida  = ".REGISTRO."                         
         LET tipo_reporte = "   R E G I S T R O "                         
      ELSE                                                                
         LET tipo_salida  = ".TRASPASO."                         
         LET tipo_reporte = "   T R A S P A S O "                         
      END IF                                                              
                                                                     
      LET G_LISTA = g_paramgral.ruta_spool CLIPPED,"/",g_usuario CLIPPED,
                    tipo_salida CLIPPED, HOY USING "DDMMYY","_",HORA CLIPPED
                                                                     
      CASE g_reg.ord

          WHEN 1 
          START REPORT listado1 TO G_LISTA

          LET txt1 = " SELECT a.n_folio,",
                     " a.tipo_solicitud,",
                     " a.n_seguro      ,",
                     " a.n_rfc         ,",
                     " a.paterno       ,",
                     " a.materno       ,",
                     " a.nombres       ,",
                     " b.cod_promotor        ,",
                     " b.paterno       ,",
                     " b.materno       ,",
                     " b.nombres       ,",
                     " b.agenc_cod     ,",
                     " a.fecha_envio   ,",
                     " a.frecafor      ,",
                     " a.hora          ,",
                     " a.status_captura,",
                     " a.status_interno,",
                     " a.usuario        ",
                     " FROM  afi_solicitud a, pro_mae_promotor b ",
                     " WHERE a.frecafor BETWEEN '",g_reg.fecha_ini,"'",
                                        " AND '",g_reg.fecha_top,"'",  
                     " AND   a.cod_promotor = b.cod_promotor",
                     " AND   a.tipo_solicitud = '",g_reg.opcion,"'" 

          IF g_reg.usuario IS NOT NULL THEN
             LET txt1 = txt1 CLIPPED," AND  a.usuario = '",g_reg.usuario,"'",
             " ORDER BY a.usuario, a.frecafor, a.hora"
          ELSE
             LET txt1 = txt1 CLIPPED," ORDER BY a.usuario,a.frecafor,a.hora"
          END IF

          PREPARE hhh FROM txt1

          DECLARE cur_1 CURSOR for hhh
            FOREACH cur_1 INTO w_aux.*
              OUTPUT TO REPORT listado1(w_aux.*)       
            END FOREACH 

          FINISH REPORT listado1
          PROMPT "LISTADO GENERADO ",G_LISTA CLIPPED," <enter>" FOR aux_pausa

          WHEN 2
          START REPORT listado2 TO G_LISTA
          LET txt1 = " SELECT a.n_folio,",
                     " a.tipo_solicitud,",
                     " a.n_seguro      ,",
                     " a.n_rfc         ,",
                     " a.paterno       ,",
                     " a.materno       ,",
                     " a.nombres       ,",
                     " b.cod_promotor        ,",
                     " b.paterno       ,",
                     " b.materno       ,",
                     " b.nombres       ,",
                     " b.agenc_cod     ,",
                     " a.fecha_envio   ,",
                     " a.frecafor      ,",
                     " a.hora          ,",
                     " a.status_captura,",
                     " a.status_interno,",
                     " a.usuario        ",
                     " FROM  afi_solicitud a, pro_mae_promotor b ",
                     " WHERE a.frecafor BETWEEN '",g_reg.fecha_ini,"'",
                                        " AND '",g_reg.fecha_top,"'",  
                     " AND   a.cod_promotor = b.cod_promotor",                     
                     " AND   a.tipo_solicitud = '",g_reg.opcion,"'"    

          IF g_reg.usuario IS NOT NULL THEN
             LET txt1 = txt1 CLIPPED," AND a.usuario = '",g_reg.usuario,"'",
             " ORDER BY b.cod_promotor,a.frecafor,a.hora"
          ELSE
             LET txt1 = txt1 CLIPPED," ORDER BY b.cod_promotor,a.frecafor,a.hora"
          END IF

          PREPARE hhh1 FROM txt1

          DECLARE cur_2a CURSOR for hhh1
            FOREACH cur_2a INTO w_aux.*
              OUTPUT TO REPORT listado2(w_aux.*)       
            END FOREACH 

          FINISH REPORT listado2
          PROMPT "LISTADO GENERADO ",G_LISTA CLIPPED," <enter>" FOR aux_pausa

          WHEN 3
          START REPORT listado3 TO G_LISTA

          LET txt1 = " SELECT a.n_folio,",
                     " a.tipo_solicitud,",
                     " a.n_seguro      ,",
                     " a.n_rfc         ,",
                     " a.paterno       ,",
                     " a.materno       ,",
                     " a.nombres       ,",
                     " b.cod_promotor        ,",
                     " b.paterno       ,",
                     " b.materno       ,",
                     " b.nombres       ,",
                     " b.agenc_cod     ,",
                     " a.fecha_envio   ,",
                     " a.frecafor      ,",
                     " a.hora          ,",
                     " a.status_captura,",
                     " a.status_interno,",
                     " a.usuario        ",
                     " FROM  afi_solicitud a, pro_mae_promotor b ",
                     " WHERE a.frecafor BETWEEN '",g_reg.fecha_ini,"'",
                                        " AND '",g_reg.fecha_top,"'",  
                     " AND   a.cod_promotor = b.cod_promotor",                     
                     " AND   a.tipo_solicitud = '",g_reg.opcion,"'"    

          IF g_reg.usuario IS NOT NULL THEN
             LET txt1 = txt1 CLIPPED," AND a.usuario = '",g_reg.usuario,"'",
             " ORDER BY b.agenc_cod,a.frecafor,a.hora"
          ELSE
             LET txt1 = txt1 CLIPPED," ORDER BY b.agenc_cod,a.frecafor,a.hora"
          END IF

          PREPARE hhh2 FROM txt1

          DECLARE cur_3 CURSOR for hhh2
            FOREACH cur_3 INTO w_aux.*
              OUTPUT TO REPORT listado3(w_aux.*)       
            END FOREACH 

          FINISH REPORT listado3
          PROMPT "LISTADO GENERADO ",G_LISTA CLIPPED," <enter>" FOR aux_pausa

      END CASE
   END IF

END MAIN

REPORT listado1(w_aux)
#l1-------------------

   DEFINE w_aux          RECORD
          n_folio        LIKE afi_solicitud.n_folio,
          tipo_solicitud LIKE afi_solicitud.tipo_solicitud,
          n_seguro       LIKE afi_solicitud.n_seguro,
          n_rfc          LIKE afi_solicitud.n_rfc,
          paterno        LIKE afi_solicitud.paterno,
          materno        LIKE afi_solicitud.materno,
          nombres        LIKE afi_solicitud.nombres,
          cod_promotor         LIKE pro_mae_promotor.cod_promotor,
          ppaterno       LIKE pro_mae_promotor.paterno,
          pmaterno       LIKE pro_mae_promotor.materno,
          pnombres       LIKE pro_mae_promotor.nombres,
          agenc_cod      LIKE pro_mae_promotor.agenc_cod,
          fecha_envio    LIKE afi_solicitud.fecha_envio,
          frecafor       LIKE afi_solicitud.frecafor,
          hora           LIKE afi_solicitud.hora,
          status_captura LIKE afi_solicitud.status_captura,
          status_interno LIKE afi_solicitud.status_interno,
          usuario        LIKE afi_solicitud.usuario
   END RECORD

   OUTPUT
   PAGE LENGTH 90
   LEFT MARGIN 0
   RIGHT MARGIN 0
   TOP MARGIN 0
   BOTTOM MARGIN 0
  
   FORMAT
   PAGE HEADER

     SELECT A.razon_social
     INTO   razon_social
     FROM   tab_afore_local A

     PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

     PRINT
     COLUMN 001,"========================================",
     COLUMN 040,"========================================",
     COLUMN 080,"========================================",
     COLUMN 120,"========================================",
     COLUMN 160,"========"

     PRINT
     COLUMN 001,razon_social,                                                  
     COLUMN 149,"FECHA : ",hoy USING "DD/MM/YYYY"                             
                                                                          
     PRINT                                                                     
     COLUMN 001,"AFIL009",                                                     
     COLUMN 035,"C A P T U R A    D E    A F I L I A D O S    P O R ",
                 tipo_reporte,"   (N I V E L    U S U A R I O)",COLUMN 149,hora         
                                                                          
     PRINT                                                                     
     PRINT                                                                     
     COLUMN 67,"Del : ", g_reg.fecha_ini USING "DD/MM/YYYY",                   
               "  Al : ",g_reg.fecha_top USING "DD/MM/YYYY",                   
     COLUMN 149,"P\240gina : ",pageno USING "####"                             

     PRINT
     COLUMN 001,"----------------------------------------",
     COLUMN 040,"----------------------------------------",
     COLUMN 080,"----------------------------------------",
     COLUMN 120,"----------------------------------------",
     COLUMN 160,"--------"

     PRINT
     COLUMN 001,"Solicitud",
     COLUMN 013,"N.S.S.",
     COLUMN 025,"R.F.C.",
     COLUMN 040,"Nombre Del Afiliado",
     COLUMN 075,"No.Promotor",
     COLUMN 087,"Nombre Del Promotor",
     COLUMN 107,"Estructura",
     COLUMN 124,"F. Captura",
     COLUMN 135,"F. Envio",
     COLUMN 147,"Hora",
     COLUMN 153,"S.C.",
     COLUMN 156,"S.I.",
     COLUMN 160,"Usuario"

     PRINT
     COLUMN 040,"========================================",
     COLUMN 080,"========================================",
     COLUMN 120,"========================================",
     COLUMN 160,"========"

   ON EVERY ROW

     LET estrc = NULL

     SELECT nombre_uni_n1 
     INTO   estrc 
     FROM   com_nivel1
     WHERE  coduni_n1 = w_aux.agenc_cod

     LET l_estado = NULL

     CASE w_aux.status_interno

         WHEN   0 LET l_estado = "BAJA"
                  LET w_edo.cap  = w_edo.cap + 1

         WHEN  10 LET l_estado = "CAPTURADO"
                  LET w_edo.incomp = w_edo.incomp + 1

         WHEN  15 LET l_estado = "CONFIRMADO"
                  LET w_edo.incomp = w_edo.incomp + 1

         WHEN  20 LET l_estado = "VALIDADO"
                  LET w_edo.compl  = w_edo.compl + 1

         WHEN  30 LET l_estado = "ENVIADO"
                  LET w_edo.envia  = w_edo.envia + 1

         WHEN  40 LET l_estado = "RECHAZADO"
                  LET w_edo.recha  = w_edo.recha + 1

         WHEN  42 LET l_estado = "RECHAZO DEFINITIVO"
                  LET w_edo.recha  = w_edo.recha + 1

         WHEN  45 LET l_estado = "TRASPASO RECHAZADO"
                  LET w_edo.tr_rech  = w_edo.tr_rech + 1

         WHEN  50 LET l_estado = "PENDIENTE"
                  LET w_edo.pend  = w_edo.pend + 1

         WHEN  55 LET l_estado = "ACLARACION"
                  LET w_edo.aclara  = w_edo.aclara + 1

         WHEN  60 LET l_estado = "APROBADO"
                  LET w_edo.aprob  = w_edo.aprob + 1

         WHEN  70 LET l_estado = "CERT. NO LIQUIDADO"
                  LET w_edo.certif  = w_edo.certif + 1

         WHEN  75 LET l_estado = "CERT. LIQUIDADO"
                  LET w_edo.liquida  = w_edo.liquida + 1  

         WHEN  90 LET l_estado = "PENDIENTE ASIG."
                  LET w_edo.canc  = w_edo.canc + 1

         WHEN 100 LET l_estado = "REGISTRADO"
                  LET w_edo.regis  = w_edo.regis + 1
     END CASE                           

     LET l_captura = NULL

    {
     CASE w_aux.status_captura

         WHEN   10 LET l_captura = "BASICO"
                   LET w_cap.bas = w_cap.bas + 1

         WHEN   20 LET l_captura = "COMPLETO"
                   LET w_cap.comp = w_cap.comp + 1

         WHEN  100 LET l_captura = "TRASPASADO"
                   LET w_cap.tras = w_cap.tras + 1

     END CASE
   }

     PRINT
     COLUMN 001,w_aux.tipo_solicitud USING "&", 
     COLUMN 003,w_aux.n_folio     USING "&&&&&&&&",
     COLUMN 013,w_aux.n_seguro,
     COLUMN 025,w_aux.n_rfc,
     COLUMN 040,w_aux.paterno     CLIPPED,
                " ",w_aux.materno CLIPPED,
                " ",w_aux.nombres [1,9],
     COLUMN 075,w_aux.cod_promotor      USING "&&&&&&&&&&",
     COLUMN 087,w_aux.ppaterno    CLIPPED,", ",w_aux.pnombres [1,9],
     COLUMN 107,estrc [1,16],
     COLUMN 124,w_aux.frecafor    USING "DD-MM-YYYY",
     COLUMN 135,w_aux.fecha_envio USING "DD-MM-YYYY",
     COLUMN 147,w_aux.hora [1,5],
     COLUMN 153,l_captura  [1,3],
     COLUMN 157,l_estado   [1,3],
     COLUMN 161,w_aux.usuario

   AFTER GROUP OF w_aux.usuario

     SKIP 2 LINES

     PRINT 
     COLUMN 1,"SOLICITUDES CAPTURADAS POR  ",w_aux.usuario," : ",
               GROUP COUNT(*) USING "&&&&&"

     SKIP 2 LINES

   ON LAST ROW

     SKIP 2 LINES

     PRINT 
     COLUMN 1,"TOTAL  DE  SOLICITUDES    : ",COUNT(*) USING "&&&&&&&"

     SKIP 2 LINES

END REPORT

REPORT listado2(w_aux)
#l2-------------------

   DEFINE w_aux          RECORD
          n_folio        LIKE afi_solicitud.n_folio,
          tipo_solicitud LIKE afi_solicitud.tipo_solicitud ,
          n_seguro       LIKE afi_solicitud.n_seguro,
          n_rfc          LIKE afi_solicitud.n_rfc,
          paterno        LIKE afi_solicitud.paterno,
          materno        LIKE afi_solicitud.materno,
          nombres        LIKE afi_solicitud.nombres,
          cod_promotor         LIKE pro_mae_promotor.cod_promotor,
          ppaterno       LIKE pro_mae_promotor.paterno,
          pmaterno       LIKE pro_mae_promotor.materno,
          pnombres       LIKE pro_mae_promotor.nombres,
          agenc_cod      LIKE pro_mae_promotor.agenc_cod,
          fecha_envio    LIKE afi_solicitud.fecha_envio,
          frecafor       LIKE afi_solicitud.frecafor,
          hora           LIKE afi_solicitud.hora,
          status_captura LIKE afi_solicitud.status_captura,
          status_interno LIKE afi_solicitud.status_interno,
          usuario        LIKE afi_solicitud.usuario
   END RECORD

   OUTPUT
     PAGE LENGTH 90
     LEFT MARGIN 0
     RIGHT MARGIN 0
     TOP MARGIN 0
     BOTTOM MARGIN 0
  
   FORMAT
   PAGE HEADER

     SELECT A.razon_social
     INTO   razon_social
     FROM   tab_afore_local A

     PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

     PRINT
     COLUMN 001,"========================================",
     COLUMN 040,"========================================",
     COLUMN 080,"========================================",
     COLUMN 120,"========================================",
     COLUMN 160,"========"

     PRINT                                                                     
     COLUMN 001,razon_social,                                                  
     COLUMN 149,"FECHA : ",hoy USING "DD/MM/YYYY"                              
                                                                          
     PRINT                                                                     
     COLUMN 001,"AFIL009",                                                     
     COLUMN 035,"C A P T U R A    D E    A F I L I A D O S    P O R ",         
                 tipo_reporte,"   (N I V E L    U S U A R I O)",COLUMN 149,hora
                                                                          
                                                                          
     PRINT                                                                     
     PRINT                                                                     
     COLUMN 67,"Del : ", g_reg.fecha_ini USING "DD/MM/YYYY",                   
               "  Al : ",g_reg.fecha_top USING "DD/MM/YYYY",                   
     COLUMN 149,"P\240gina : ",pageno USING "####"                             
                                                                          
     PRINT
     COLUMN 001,"----------------------------------------",
     COLUMN 040,"----------------------------------------",
     COLUMN 080,"----------------------------------------",
     COLUMN 120,"----------------------------------------",
     COLUMN 160,"--------"

     PRINT
     COLUMN 001,"Solicitud",
     COLUMN 013,"N.S.S.",
     COLUMN 025,"R.F.C.",
     COLUMN 040,"Nombre Del Afiliado",
     COLUMN 075,"No.Promotor",
     COLUMN 087,"Nombre Del Promotor",
     COLUMN 107,"Estructura",
     COLUMN 124,"F. Captura",
     COLUMN 135,"F. Envio",
     COLUMN 147,"Hora",
     COLUMN 153,"S.C.",
     COLUMN 156,"S.I.",
     COLUMN 160,"Usuario"

     PRINT
     COLUMN 001,"========================================",
     COLUMN 040,"========================================",
     COLUMN 080,"========================================",
     COLUMN 120,"========================================",
     COLUMN 160,"========"

   ON EVERY ROW

     LET estrc = NULL

     SELECT nombre_uni_n1
     INTO   estrc
     FROM   com_nivel1
     WHERE  coduni_n1 = w_aux.agenc_cod

     LET l_captura = NULL

   {
     CASE w_aux.status_captura

         WHEN   10 LET l_captura = "BASICO"
                   LET w_cap.bas = w_cap.bas + 1

         WHEN   20 LET l_captura = "COMPLETO"
                   LET w_cap.comp = w_cap.comp + 1

         WHEN  100 LET l_captura = "TRASPASADO"
                   LET w_cap.tras = w_cap.tras + 1
     END CASE
   }

     LET l_estado = NULL

     CASE w_aux.status_interno

         WHEN   0 LET l_estado = "BAJA"
                  LET w_edo.cap  = w_edo.cap + 1

         WHEN  10 LET l_estado = "CAPTURADO"
                  LET w_edo.incomp = w_edo.incomp + 1

         WHEN  15 LET l_estado = "CONFIRMADO"
                  LET w_edo.incomp = w_edo.incomp + 1

         WHEN  20 LET l_estado = "VALIDADO"
                  LET w_edo.compl  = w_edo.compl + 1

         WHEN  30 LET l_estado = "ENVIADO"
                  LET w_edo.envia  = w_edo.envia + 1

         WHEN  40 LET l_estado = "RECHAZADO"
                  LET w_edo.recha  = w_edo.recha + 1

         WHEN  42 LET l_estado = "RECHAZO DEFINITIVO"
                  LET w_edo.recha  = w_edo.recha + 1

         WHEN  45 LET l_estado = "TRASPASO RECHAZADO"
                  LET w_edo.tr_rech  = w_edo.tr_rech + 1

         WHEN  50 LET l_estado = "PENDIENTE"
                  LET w_edo.pend  = w_edo.pend + 1

         WHEN  55 LET l_estado = "ACLARACION"
                  LET w_edo.aclara  = w_edo.aclara + 1

         WHEN  60 LET l_estado = "APROBADO"
                  LET w_edo.aprob  = w_edo.aprob + 1

         WHEN  70 LET l_estado = "CERTIFICADO"
                  LET w_edo.certif  = w_edo.certif + 1

         WHEN  75 LET l_estado = "CERT. LIQUIDADO"
                  LET w_edo.liquida  = w_edo.liquida + 1  

         WHEN  90 LET l_estado = "PENDIENTE ASIG."
                  LET w_edo.canc  = w_edo.canc + 1

         WHEN 100 LET l_estado = "REGISTRADO"
                  LET w_edo.regis  = w_edo.regis + 1

     END CASE                           

     PRINT
     COLUMN 001,w_aux.tipo_solicitud USING "&",
     COLUMN 003,w_aux.n_folio     USING "&&&&&&&&",
     COLUMN 013,w_aux.n_seguro,
     COLUMN 025,w_aux.n_rfc,
     COLUMN 040,w_aux.paterno     CLIPPED," ",
                w_aux.materno     CLIPPED," ",
                w_aux.nombres     [1,9],
     COLUMN 075,w_aux.cod_promotor      USING "&&&&&&&&&&",
     COLUMN 087,w_aux.ppaterno    CLIPPED,", ",w_aux.pnombres [1,9],
     COLUMN 107,estrc [1,16],
     COLUMN 124,w_aux.frecafor    USING "DD-MM-YYYY",
     COLUMN 135,w_aux.fecha_envio USING "dd-mm-yyyy",
     COLUMN 147,w_aux.hora [1,5],
     COLUMN 153,l_captura  [1,3],
     COLUMN 157,l_estado   [1,3],
     COLUMN 161,w_aux.usuario

   AFTER GROUP OF w_aux.cod_promotor

     SKIP 2 LINES

     PRINT 
     COLUMN 1,"SOLICITUDES REALIZADAS POR EL PROMOTOR   ",w_aux.cod_promotor," : ",
               GROUP COUNT(*) USING "&&&&&"

     SKIP 2 LINES

   ON LAST ROW

     SKIP 2 LINES

     PRINT 
     COLUMN 1," TOTAL SOLICITUDES     : ",COUNT(*) USING "&&&&&&&"

     SKIP 2 LINES

END REPORT


REPORT listado3(w_aux)
#l3-------------------
   DEFINE w_aux          RECORD
          n_folio        LIKE afi_solicitud.n_folio,
          tipo_solicitud LIKE afi_solicitud.tipo_solicitud,
          n_seguro       LIKE afi_solicitud.n_seguro,
          n_rfc          LIKE afi_solicitud.n_rfc,
          paterno        LIKE afi_solicitud.paterno,
          materno        LIKE afi_solicitud.materno,
          nombres        LIKE afi_solicitud.nombres,
          cod_promotor         LIKE pro_mae_promotor.cod_promotor,
          ppaterno       LIKE pro_mae_promotor.paterno,
          pmaterno       LIKE pro_mae_promotor.materno,
          pnombres       LIKE pro_mae_promotor.nombres,
          agenc_cod      LIKE pro_mae_promotor.agenc_cod,
          fecha_envio    LIKE afi_solicitud.fecha_envio,
          frecafor       LIKE afi_solicitud.frecafor,
          hora           LIKE afi_solicitud.hora,
          status_captura LIKE afi_solicitud.status_captura,
          status_interno LIKE afi_solicitud.status_interno,
          usuario        LIKE afi_solicitud.usuario
   END RECORD

   OUTPUT
     PAGE LENGTH 90
     LEFT MARGIN 0
     RIGHT MARGIN 0
     TOP MARGIN 0
     BOTTOM MARGIN 0
  
   FORMAT
   PAGE HEADER

     SELECT A.razon_social
     INTO   razon_social
     FROM   tab_afore_local A

     PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

     PRINT
     COLUMN 001,"========================================", 
     COLUMN 040,"========================================",
     COLUMN 080,"========================================",
     COLUMN 120,"========================================",
     COLUMN 160,"========"

     PRINT                                                                     
     COLUMN 001,razon_social,                                                  
     COLUMN 149,"FECHA : ",hoy USING "DD/MM/YYYY"                              
                                                                          
     PRINT                                                                     
     COLUMN 001,"AFIL009",                                                     
     COLUMN 035,"C A P T U R A    D E    A F I L I A D O S    P O R ",         
                 tipo_reporte,"   (N I V E L    U S U A R I O)",COLUMN 149,hora
                                                                          
     PRINT                                                                     
     PRINT                                                                     
     COLUMN 67,"Del : ", g_reg.fecha_ini USING "DD/MM/YYYY",                   
               "  Al : ",g_reg.fecha_top USING "DD/MM/YYYY",                   
     COLUMN 149,"P\240gina : ",pageno USING "####"                             
                                                                          
     PRINT
     COLUMN 001,"----------------------------------------",
     COLUMN 040,"----------------------------------------",
     COLUMN 080,"----------------------------------------",
     COLUMN 120,"----------------------------------------",
     COLUMN 160,"--------"

     PRINT
     COLUMN 001,"Solicitud",
     COLUMN 013,"N.S.S.",
     COLUMN 025,"R.F.C.",
     COLUMN 040,"Nombre Del Afiliado",
     COLUMN 075,"No.Promotor",
     COLUMN 087,"Nombre Del Promotor",
     COLUMN 107,"Estructura",
     COLUMN 124,"F. Captura",
     COLUMN 135,"F. Envio",
     COLUMN 147,"Hora",
     COLUMN 153,"S.C.",
     COLUMN 156,"S.I.",
     COLUMN 160,"Usuario"

     PRINT
     COLUMN 001,"========================================",
     COLUMN 040,"========================================",
     COLUMN 080,"========================================",
     COLUMN 120,"========================================", 
     COLUMN 160,"========"

   ON EVERY ROW

     LET estrc = NULL

     SELECT nombre_uni_n1
     INTO   estrc
     FROM   com_nivel1
     where  coduni_n1 = w_aux.agenc_cod

     LET l_captura = NULL

    {
     CASE w_aux.status_captura

         WHEN  10 LET l_captura = "BASICO"
                  LET w_cap.bas = w_cap.bas + 1

         WHEN  20 LET l_captura = "COMPLETO"
                  LET w_cap.comp = w_cap.comp + 1

         WHEN 100 LET l_captura = "SUBIDO AL MTO."
                  LET w_cap.tras = w_cap.tras + 1
     END CASE
    }

     LET l_estado = NULL

     CASE w_aux.status_interno

         WHEN   0 LET l_estado = "BAJA"
                  LET w_edo.cap  = w_edo.cap + 1

         WHEN  10 LET l_estado = "CAPTURADO"
                  LET w_edo.incomp = w_edo.incomp + 1

         WHEN  15 LET l_estado = "CONFIRMADO"
                  LET w_edo.incomp = w_edo.incomp + 1

         WHEN  20 LET l_estado = "VALIDADO"
                  LET w_edo.compl  = w_edo.compl + 1

         WHEN  30 LET l_estado = "ENVIADO"
                  LET w_edo.envia  = w_edo.envia + 1

         WHEN  40 LET l_estado = "RECHAZADO"
                  LET w_edo.recha  = w_edo.recha + 1

         WHEN  42 LET l_estado = "RECHAZO DEFINITIVO"
                  LET w_edo.recha  = w_edo.recha + 1

         WHEN  45 LET l_estado = "TRASP. RECH."
                  LET w_edo.recha  = w_edo.recha + 1

         WHEN  50 LET l_estado = "PENDIENTE"
                  LET w_edo.pend  = w_edo.pend + 1

         WHEN  55 LET l_estado = "ACLARACION"
                  LET w_edo.pend  = w_edo.pend + 1

         WHEN  60 LET l_estado = "APROBADO"
                  LET w_edo.aprob  = w_edo.aprob + 1

         WHEN  70 LET l_estado = "CERTIFICADA"
                  LET w_edo.aprob  = w_edo.aprob + 1

         WHEN  75 LET l_estado = "CERT. LIQ."
                  LET w_edo.pend  = w_edo.pend + 1

         WHEN  90 LET l_estado = "PENDIENTE ASIG."
                  LET w_edo.canc  = w_edo.canc + 1

         WHEN 100 LET l_estado = "REGISTRADO"
                  LET w_edo.regis  = w_edo.regis + 1

     END CASE                           

     PRINT
     COLUMN 001,w_aux.tipo_solicitud USING "&",
     COLUMN 003,w_aux.n_folio     USING "&&&&&&&&",
     COLUMN 013,w_aux.n_seguro,
     COLUMN 025,w_aux.n_rfc,
     COLUMN 040,w_aux.paterno     CLIPPED," ",
                w_aux.materno     CLIPPED," ",
                w_aux.nombres     [1,9],
     COLUMN 075,w_aux.cod_promotor      USING "&&&&&&&&&&",
     COLUMN 087,w_aux.ppaterno    CLIPPED,", ",w_aux.pnombres [1,9],
     COLUMN 107,estrc [1,16],
     COLUMN 124,w_aux.frecafor    USING "DD-MM-YYYY",
     COLUMN 135,w_aux.fecha_envio USING "DD-MM-YYYY",
     COLUMN 147,w_aux.hora [1,5],
     COLUMN 153,l_captura  [1,3],
     COLUMN 157,l_estado   [1,3],
     COLUMN 161,w_aux.usuario

   AFTER GROUP OF w_aux.agenc_cod

     SKIP 2 LINES

     PRINT 
     COLUMN 1,"SOLICITUDES POR ESTRUCTURA   ",w_aux.agenc_cod," : ",
               GROUP COUNT(*) USING "&&&&&"

     SKIP 2 LINES

   ON LAST ROW

     SKIP 2 LINES

     PRINT 
     COLUMN 1,"TOTAL  DE  SOLICITUDES   : ",COUNT(*) USING "&&&&&&&"

     SKIP 2 LINES

END REPORT
