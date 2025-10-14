#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa AFIL031  => ESTADOS DEL AFILIADO (RECHAZADOS)                     #
#Por               => JUAN COLIN M.                                         #
#Fecha             => 14 Abril 1997.      				    #
#Actualizacion     => RICARDO ARELLANO M.                                   #
#Fecha             => 15 Diciembre 1998   				    #
#Actualizacion     => JESUS DAVID YANEZ.                                    #
#Fecha             => 21 ABRIL DE 1999.   				    #
#Actualizacion     => FERNANDO HERRERA HERNANDEZ.                           #
#Fecha  	   => 05 AGOSTRO DE 2003. VERSION 2                         #
#Sistema           => AFI. 					            #
#############################################################################
DATABASE safre_af
GLOBALS

   DEFINE g_reg        RECORD
          fecha_ini    DATE,
          fecha_top    DATE,
          estruc       CHAR(05),
          promotor     CHAR(12),
          motivo       CHAR(05), 
          ord          SMALLINT,
          opcion       SMALLINT                        
   END RECORD

   DEFINE var_aux RECORD
          var_stat LIKE afi_solicitud.tipo_solicitud
   END RECORD

   DEFINE w_aux        RECORD
          lote         LIKE afi_solicitud.lote,
          agenc_cod    LIKE afi_solicitud.agenc_cod,
          #num_solic    LIKE pro_mae_promotor.nro_solicitud,
          num_solic    DECIMAL(10,0),
          ppaterno     LIKE pro_mae_promotor.paterno,
          mmaterno     LIKE pro_mae_promotor.materno,
          nnombres     LIKE pro_mae_promotor.nombres,
          n_folio      LIKE afi_rechaza_cert.n_folio,
          frecafor     LIKE afi_solicitud.frecafor,
          fecrec       LIKE afi_rechaza_cert.f_rechazo,
          rdeta_cod    LIKE afi_rechaza_cert.rdeta_cod,
          observacion  LIKE afi_rechaza_cert.observacion,
          paterno      LIKE afi_solicitud.paterno,
          materno      LIKE afi_solicitud.materno,
          nombres      LIKE afi_solicitud.nombres,
          n_seguro     LIKE afi_solicitud.n_seguro,
          n_rfc        LIKE afi_solicitud.n_rfc, 
          usuario      LIKE afi_solicitud.usuario   
   END RECORD

   DEFINE band         SMALLINT
   DEFINE g_paramgral  RECORD LIKE seg_modulo.*
   DEFINE g_afore      RECORD LIKE tab_afore.*
   DEFINE hoy          DATE
   DEFINE hora         CHAR(05)
   DEFINE estrc        CHAR(16)
   DEFINE aux_pausa    CHAR(01)
   DEFINE g_usuario    CHAR(08)
   DEFINE parametrox   CHAR(05)
   DEFINE tipo_salida  CHAR(20)
   DEFINE tipo_reporte CHAR(20)
   DEFINE G_LISTA      CHAR(100)
   DEFINE txt1         CHAR(800)
   DEFINE vimpresion   CHAR(100)

END GLOBALS

MAIN

   OPTIONS 
   PROMPT LINE LAST,
   ACCEPT KEY CONTROL-I,
   FORM LINE 3

   CALL STARTLOG("AFIL031.log")

   LET hora = TIME
   LET hoy  = TODAY

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIL0311" ATTRIBUTE(BORDER)
   DISPLAY "AFIL031  LISTADO DE AFILIADOS RECHAZADOS (REGISTRO o TRASPASO)                 " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   LET g_reg.fecha_ini = TODAY
   LET g_reg.fecha_top = TODAY
   LET g_reg.estruc    = NULL
   LET g_reg.promotor  = NULL
   LET g_reg.motivo    = NULL
   LET g_reg.ord       = 1

   INPUT BY NAME g_reg.* WITHOUT DEFAULTS 

     AFTER FIELD fecha_ini
     IF g_reg.fecha_ini IS NULL THEN
        ERROR "LA FECHA INICIAL NO PUEDE SER NULA"
        NEXT FIELD fecha_ini
     END IF

     AFTER FIELD fecha_top
     IF fgl_lastkey() = fgl_keyval("UP") THEN
        NEXT FIELD fecha_ini
     END IF

     IF g_reg.fecha_top IS NULL THEN
        ERROR "LA FECHA FINAL NO PUEDE SER NULA"
        NEXT FIELD fecha_top
     ELSE
        NEXT FIELD motivo
     END IF

     IF g_reg.fecha_top < g_reg.fecha_ini THEN
        ERROR "FECHA NO PUEDE SER MENOR A LA FECHA INICIAL"
        NEXT FIELD fecha_top
     END IF

     BEFORE FIELD motivo
     LET g_reg.motivo = "TODOS"

     AFTER FIELD motivo
     IF fgl_lastkey() = fgl_keyval("UP") THEN
        LET g_reg.estruc = NULL
        DISPLAY g_reg.estruc
        LET g_reg.promotor = NULL
        DISPLAY g_reg.promotor
        NEXT FIELD promotor
     END IF

     IF g_reg.motivo IS NULL THEN
        NEXT FIELD estruc
     ELSE 
        NEXT FIELD ord
     END IF

     BEFORE FIELD estruc
     LET g_reg.estruc = "TODOS"

     AFTER FIELD estruc   
     IF fgl_lastkey() = fgl_keyval("UP") THEN
        LET g_reg.promotor = NULL
        DISPLAY g_reg.promotor
        LET g_reg.motivo = NULL
        DISPLAY g_reg.motivo
        NEXT FIELD fecha_ini
     END IF

     IF g_reg.estruc IS NULL THEN
        NEXT FIELD promotor
     ELSE 
        NEXT FIELD ord
     END IF

     BEFORE FIELD promotor
     LET g_reg.promotor = "TODOS"

     AFTER FIELD promotor
     IF fgl_lastkey() = fgl_keyval("UP") THEN
        LET g_reg.promotor = NULL
        LET g_reg.estruc = NULL
        DISPLAY g_reg.estruc
        LET g_reg.motivo = NULL
        DISPLAY g_reg.motivo
        DISPLAY g_reg.promotor
        NEXT FIELD estruc
     END IF

     IF g_reg.promotor IS NULL THEN
        NEXT FIELD motivo
     ELSE 
        NEXT FIELD ord
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

   END INPUT

   WHILE TRUE
     PROMPT "DESEA EMITIR INFORME S/N ==> " FOR aux_pausa
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
      FROM   tab_afore
      WHERE  @marca = 1

      SELECT *
      INTO   g_paramgral.*
      FROM   seg_modulo    
      WHERE  modulo_cod = 'afi'
 
      IF g_reg.opcion = 1 THEN
         LET tipo_salida  = ".RECH_REG."
         LET tipo_reporte = "    R E G I S T R O "
      ELSE
         LET tipo_salida  = ".RECH_TRA."
         LET tipo_reporte = "    T R A S P A S O "
      END IF

      LET G_LISTA = g_paramgral.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                    tipo_salida CLIPPED, HOY USING "DDMMYY","_",
                    HORA[1,2],HORA[4,5] CLIPPED
            
      CASE g_reg.ord

          WHEN 1
	  START REPORT listado TO G_LISTA
           
	  LET txt1 = " SELECT          ",
                     " a.lote         ,",
                     " a.agenc_cod    ,",
                     " c.nro_solicitud,",
                     " c.paterno      ,",
                     " c.materno      ,",
                     " c.nombres      ,",
                     " a.n_folio      ,",
                     " a.frecafor     ,",
                     " b.f_rechazo    ,",
                     " b.rdeta_cod    ,",
                     " b.observacion  ,",
                     " a.paterno      ,",
                     " a.materno      ,",
                     " a.nombres      ,",
                     " a.n_seguro     ,",
                     " a.n_rfc        ,",
                     " a.usuario       ",
                     " FROM  afi_solicitud a, afi_rechaza_cert b,",
                     " pro_mae_promotor c",
                     " WHERE b.f_rechazo BETWEEN '",g_reg.fecha_ini,"'",
                                         " AND '",g_reg.fecha_top,"'",
                     " AND   a.tipo_solicitud = '",g_reg.opcion,"'"

          IF g_reg.estruc IS NOT NULL THEN
             LET txt1 = txt1 CLIPPED," AND a.status_interno IN (40,42)",
                                     " AND a.n_folio = b.n_folio",
                               " AND   b.tipo_solicitud = '",g_reg.opcion,"'",
                                     " AND a.cod_promotor  = c.cod_promotor"

             IF g_reg.estruc != "TODOS" THEN
               LET txt1 = txt1 CLIPPED," AND a.agenc_cod = '",g_reg.estruc,"'",
                " ORDER BY a.lote, a.n_folio, a.n_seguro, a.agenc_cod "
             ELSE
                LET txt1 = txt1 CLIPPED,
                " ORDER BY a.lote, a.n_folio, a.n_seguro, a.agenc_cod "
             END IF
          END IF

          IF g_reg.promotor IS NOT NULL THEN
             LET txt1 = txt1 CLIPPED," AND  a.status_interno IN (40,42)",
                                     " AND  a.n_folio = b.n_folio",
                                     " AND  a.cod_promotor  = c.cod_promotor"

             IF g_reg.promotor != "TODOS" THEN
                LET txt1 = txt1 CLIPPED,
                " AND c.nro_solicitud = '",g_reg.promotor,"'",
                " ORDER BY a.lote, a.n_folio, a.n_seguro, a.agenc_cod "
             ELSE
                LET txt1 = txt1 CLIPPED,
                " ORDER BY a.lote, a.n_folio, a.n_seguro, a.agenc_cod "
             END IF
          END IF

          IF g_reg.motivo IS NOT NULL THEN
             LET txt1 = txt1 CLIPPED," AND a.status_interno IN (40,42)",
                                     " AND a.n_folio = b.n_folio",
                               " AND   b.tipo_solicitud = '",g_reg.opcion,"'",
                                     " AND a.cod_promotor  = c.cod_promotor"

             IF g_reg.motivo != "TODOS" THEN
                LET txt1 = txt1 CLIPPED,
                " AND b.rdeta_cod = '",g_reg.motivo,"'",
                " ORDER BY a.lote, a.n_folio, a.n_seguro, b.rdeta_cod "
             ELSE
                LET txt1 = txt1 CLIPPED,
                " ORDER BY a.n_folio, a.lote, a.n_seguro, b.rdeta_cod "
             END IF
          END IF

          PREPARE hhh FROM txt1

          DECLARE cur_1 CURSOR for hhh
            FOREACH cur_1 INTO w_aux.*

        # DISPLAY w_aux.n_folio
          {LET var_aux.var_stat = 0
          IF g_reg.opcion = 1 THEN 
              LET band = 2 
          END IF

          IF g_reg.opcion = 2 THEN 
              LET band = 1 
          END IF

          SELECT x.tipo_solicitud 
          INTO   var_aux.* 
          FROM   afi_solicitud x 
          WHERE  x.n_folio = w_aux.n_folio 
          AND    x.tipo_solicitud = band

          IF var_aux.var_stat = band THEN
             CONTINUE FOREACH
          END IF}

              OUTPUT TO REPORT listado(w_aux.*)       
            END FOREACH 

          FINISH REPORT listado
          LET vimpresion = "lp ", G_LISTA
          RUN G_LISTA
          PROMPT "LISTADO GENERADO => ",G_LISTA CLIPPED," <enter>" FOR aux_pausa


          WHEN 2
	  START REPORT listado1 TO G_LISTA

          LET txt1 = " SELECT          ",
                     " a.lote         ,",
                     " a.agenc_cod    ,",
                     " c.nro_solicitud,",
                     " c.paterno      ,",
                     " c.materno      ,",
                     " c.nombres      ,",
                     " a.n_folio      ,",
                     " a.frecafor     ,",
                     " b.f_rechazo    ,",
                     " b.rdeta_cod    ,",
                     " b.observacion  ,",
                     " a.paterno      ,",
                     " a.materno      ,",
                     " a.nombres      ,",
                     " a.n_seguro     ,",
                     " a.n_rfc        ,",
                     " a.usuario       ",
                     " FROM  afi_solicitud a, afi_rechaza_cert b, ",
                     " pro_mae_promotor c",
                     " WHERE b.f_rechazo BETWEEN '",g_reg.fecha_ini,"'",
                                         " AND '",g_reg.fecha_top,"'",
                     " AND   a.tipo_solicitud = '",g_reg.opcion,"'" 
  
          IF g_reg.estruc IS NOT NULL THEN
             LET txt1 = txt1 CLIPPED," AND a.status_interno IN (40,42)",
                                     " AND a.n_folio = b.n_folio",
                               " AND   b.tipo_solicitud = '",g_reg.opcion,"'",
                                     " AND a.cod_promotor  = c.cod_promotor"
  
             IF g_reg.estruc != "TODOS" THEN
                LET txt1 = txt1 CLIPPED," AND a.agenc_cod = '",g_reg.estruc,"'",
                " ORDER BY a.lote, a.n_folio, a.n_seguro, c.nro_solicitud "
             ELSE
                LET txt1 = txt1 CLIPPED,
                " ORDER BY a.lote, a.n_folio, a.n_seguro, c.nro_solicitud "
             END IF
          END IF

          IF g_reg.promotor IS NOT NULL THEN
             LET txt1 = txt1 CLIPPED," AND a.status_interno IN (40,42)",
                                     " AND a.n_folio = b.n_folio",
                               " AND   b.tipo_solicitud = '",g_reg.opcion,"'",
                                     " AND a.cod_promotor  = c.cod_promotor"
  
             IF g_reg.promotor != "TODOS" THEN
                LET txt1 = txt1 CLIPPED,
                " AND c.nro_solicitud = '",g_reg.promotor,"'",
                " ORDER BY a.lote, a.n_folio, a.n_seguro, c.nro_solicitud "
             ELSE
                LET txt1 = txt1 CLIPPED,
                " ORDER BY a.lote, a.n_folio, a.n_seguro, c.nro_solicitud "
             END IF
          END IF

          IF g_reg.motivo IS NOT NULL THEN
             LET txt1 = txt1 CLIPPED," AND a.status_interno IN (40,42)",
                                     " AND a.n_folio = b.n_folio",
                                     " AND a.cod_promotor  = c.cod_promotor"  

#  " AND a.nro_solicitud = c.nro_solicitud"

             IF g_reg.motivo != "TODOS" THEN
                LET txt1 = txt1 CLIPPED,
                " AND b.rdeta_cod = '",g_reg.motivo,"'",
                " ORDER BY a.lote, a.n_folio, a.n_seguro, c.nro_solicitud "
             ELSE
                LET txt1 = txt1 CLIPPED,
                " ORDER BY a.lote, a.n_folio, a.n_seguro, c.nro_solicitud "
             END IF
          END IF
  
          PREPARE hhh1 FROM txt1
  
          DECLARE cur_2 CURSOR for hhh1
            FOREACH cur_2 INTO w_aux.*

# DISPLAY w_aux.n_folio

          {LET var_aux.var_stat = 0
          IF g_reg.opcion = 1 THEN 
              LET band = 2 
          END IF

          IF g_reg.opcion = 2 THEN 
              LET band = 1 
          END IF

          SELECT x.tipo_solicitud 
          INTO   var_aux.* 
          FROM   afi_solicitud x 
          WHERE  x.n_folio = w_aux.n_folio 
          AND    x.tipo_solicitud = band

          IF var_aux.var_stat = band THEN
             CONTINUE FOREACH
          END IF}

              OUTPUT TO REPORT listado1(w_aux.*)       
            END FOREACH 

          FINISH REPORT listado1
          LET vimpresion = "lp ", G_LISTA
          RUN G_LISTA
          PROMPT "LISTADO GENERADO => ",G_LISTA CLIPPED," <enter>" FOR aux_pausa


          WHEN 3
          START REPORT listado2 TO G_LISTA
           
          LET txt1 = "SELECT ",
                     "a.lote,",
                     "a.agenc_cod,",
                     "c.nro_solicitud,",
                     "c.paterno,",
                     "c.materno,",
                     "c.nombres,",
                     "a.n_folio,",
                     "a.frecafor,",
                     "b.f_rechazo,",
                     "b.rdeta_cod,",
                     "b.observacion,",
                     "a.paterno,",
                     "a.materno,",
                     "a.nombres,",
                     "a.n_seguro,",
                     "a.n_rfc,",
                     "a.usuario",
                     " FROM afi_solicitud a, afi_rechaza_cert b, ",
                     " pro_mae_promotor c",
                     " WHERE b.f_rechazo BETWEEN '",g_reg.fecha_ini,"'",
                                         " AND '",g_reg.fecha_top,"'",
                     " AND a.tipo_solicitud = '",g_reg.opcion,"'"
  
          IF g_reg.motivo IS NOT NULL THEN
            LET txt1 = txt1 CLIPPED," AND a.status_interno IN (40,42)",
            " AND a.n_folio = b.n_folio",
            " AND b.tipo_solicitud = '",g_reg.opcion,"'",
            " AND a.cod_promotor = c.cod_promotor"
  
             IF g_reg.motivo != "TODOS" THEN
                LET txt1 = txt1 CLIPPED," AND b.rdeta_cod = '",g_reg.motivo,"'",
                " ORDER BY b.rdeta_cod, a.lote, a.n_folio, a.n_seguro "
             ELSE
                LET txt1 = txt1 CLIPPED,
                " ORDER BY a.lote, b.rdeta_cod, a.n_folio, a.n_seguro "
             END IF
          END IF
  
          IF g_reg.estruc IS NOT NULL THEN
            LET txt1 = txt1 CLIPPED," AND a.status_interno IN (40,42)",
            " AND a.n_folio = b.n_folio",
            " AND b.tipo_solicitud = '",g_reg.opcion,"'",
            " AND a.cod_promotor  = c.cod_promotor"
  
             IF g_reg.estruc != "TODOS" THEN
                LET txt1 = txt1 CLIPPED," AND a.agenc_cod = '",g_reg.estruc,"'",
                " ORDER BY b.rdeta_cod, a.lote, a.n_folio, a.n_seguro "
             ELSE
                LET txt1 = txt1 CLIPPED,
                " ORDER BY b.rdeta_cod, a.lote, a.n_folio, a.n_seguro "
             END IF
          END IF

          IF g_reg.promotor IS NOT NULL THEN
            LET txt1 = txt1 CLIPPED," AND a.status_interno IN (40,42)",
            " AND a.n_folio = b.n_folio",
            " AND a.cod_promotor  = c.cod_promotor"
  
             IF g_reg.promotor != "TODOS" THEN
                LET txt1 = txt1 CLIPPED,
                " AND a.nro_solicitud = '",g_reg.promotor,"'",
                " ORDER BY b.rdeta_cod, a.lote, a.n_folio, a.n_seguro "
             ELSE
                LET txt1 = txt1 CLIPPED,
                " ORDER BY b.rdeta_cod, a.lote, a.n_folio, a.n_seguro "
             END IF
          END IF

          PREPARE hhh2 FROM txt1
  
          DECLARE cur_3 CURSOR for hhh2
            FOREACH cur_3 INTO w_aux.*

              # DISPLAY w_aux.n_folio
              {LET var_aux.var_stat = 0
              IF g_reg.opcion = 1 THEN 
                 LET band = 2 
              END IF
              IF g_reg.opcion = 2 THEN 
                 LET band = 1 
              END IF

              SELECT x.tipo_solicitud 
                INTO var_aux.* 
                FROM afi_solicitud x 
               WHERE x.n_folio        = w_aux.n_folio 
                 AND x.tipo_solicitud = band
              IF var_aux.var_stat = band THEN
                 # DISPLAY var_aux.var_stat,w_aux.n_folio
                 CONTINUE FOREACH
              END IF}

              OUTPUT TO REPORT listado2(w_aux.*)       
            END FOREACH 
  
          DECLARE cur_24 CURSOR for 
            SELECT 
                  a.lote       ,
                  a.agenc_cod  ,
                  "  ",
                  "  ",
                  "  ",
                  "  ",
                  a.n_folio    ,
                  a.frecafor   ,
                  b.f_rechazo  ,
                  b.rdeta_cod  ,
                  b.observacion,
                  a.paterno    ,
                  a.materno    ,
                  a.nombres    ,
                  a.n_seguro   ,
                  a.n_rfc      ,
                  a.usuario        
            FROM  afi_solicitud a, afi_rechaza_cert b
            WHERE b.f_rechazo BETWEEN g_reg.fecha_ini AND g_reg.fecha_top
            AND   b.rdeta_cod = 24
            AND   a.n_folio = b.n_folio
            AND   b.tipo_solicitud = g_reg.opcion
  
            FOREACH cur_24 INTO w_aux.*
              OUTPUT TO REPORT listado2(w_aux.*)       
            END FOREACH 
  
          FINISH REPORT listado2
          LET vimpresion = "lp ", G_LISTA
          RUN G_LISTA
          PROMPT "LISTADO GENERADO => ",G_LISTA CLIPPED," <enter>" FOR aux_pausa
  
      END CASE
   END IF

   EXIT PROGRAM

END MAIN


REPORT listado(w_aux)
#l-------------------
   DEFINE w_aux        RECORD
          lote         LIKE afi_solicitud.lote,
          agenc_cod    LIKE afi_solicitud.agenc_cod,
          num_solic    LIKE pro_mae_promotor.nro_solicitud,
          ppaterno     LIKE pro_mae_promotor.paterno,
          mmaterno     LIKE pro_mae_promotor.materno,
          nnombres     LIKE pro_mae_promotor.nombres,
          n_folio      LIKE afi_solicitud.n_folio,
          frecafor     LIKE afi_solicitud.frecafor,
          fecrec       LIKE afi_rechaza_cert.f_rechazo,
          rdeta_cod    LIKE afi_rechaza_cert.rdeta_cod,
          observacion  LIKE afi_rechaza_cert.observacion,
          paterno      LIKE afi_solicitud.paterno ,
          materno      LIKE afi_solicitud.materno ,
        --nombres      LIKE afi_solicitud.nombres ,
          nombres      CHAR(9),
          n_seguro     LIKE afi_solicitud.n_seguro,
          n_rfc        LIKE afi_solicitud.n_rfc,   
          usuario      LIKE afi_solicitud.usuario
   END RECORD

   DEFINE razon_social CHAR(40)

   OUTPUT
   PAGE LENGTH  90
   LEFT MARGIN   0
   RIGHT MARGIN  0
   TOP MARGIN    0
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
     COLUMN 160,"========="

     PRINT
     COLUMN 001,razon_social,
     COLUMN 149,"FECHA : ",hoy USING "DD/MM/YYYY"

     PRINT
     COLUMN 001,"AFIL031",
     COLUMN 035,"S O L I C I T U D E S    D E    A F I L I A C I O N    P O R",
                 tipo_reporte,"   R E C H A Z A D A S", COLUMN 149,hora 

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
     COLUMN 160,"---------"

     PRINT
     COLUMN 001,"Lote",
     COLUMN 006,"Estr",
     COLUMN 013,"N. Promo.",
     COLUMN 024,"Nombre Promotor",
     COLUMN 048,"Folio",
     COLUMN 060,"F. captura",
     COLUMN 072,"F. rechazo",
     COLUMN 084,"Motivo de Rechazo",
     COLUMN 114,"Nombre",
     COLUMN 144,"N.S.S.",
     COLUMN 160,"R.F.C."             

     PRINT
     COLUMN 001,"========================================",
     COLUMN 040,"========================================",
     COLUMN 080,"========================================",
     COLUMN 120,"========================================",
     COLUMN 160,"========="

   ON EVERY ROW

     LET estrc = NULL

     SELECT nombre_uni_n1 
     INTO   estrc
     FROM   com_nivel1
     WHERE  coduni_n1 = w_aux.agenc_cod

     PRINT
     COLUMN 001,w_aux.lote USING "##",
   --COLUMN 006,estrc [1,6] ,
     COLUMN 010,w_aux.num_solic,
     COLUMN 024,w_aux.ppaterno [1,9]," ",w_aux.nnombres [1,13],
     COLUMN 048,w_aux.n_folio  USING "&&&&&&&&&",
     COLUMN 060,w_aux.frecafor USING "DD/MM/YYYY", 
     COLUMN 072,w_aux.fecrec   USING "DD/MM/YYYY",
     COLUMN 084,w_aux.observacion [1,29],
     COLUMN 114,w_aux.paterno  CLIPPED," ",
                w_aux.materno  CLIPPED," ",w_aux.nombres [1,9],
     COLUMN 144,w_aux.n_seguro USING "&&&&&&&&&&&&",
     COLUMN 160,w_aux.n_rfc                                           

   AFTER GROUP OF w_aux.agenc_cod

     PRINT
     PRINT
     COLUMN 01,"SOLICITUDES RECHAZADAS POR : " ,w_aux.agenc_cod,
               " : ",GROUP COUNT(*) USING "&&&&&&"
     PRINT

   AFTER GROUP  OF w_aux.lote
     PRINT
     PRINT
     COLUMN 01,"SOLICITUDES RECHAZADAS EN LOTE ",w_aux.lote,
               " : ",w_aux.usuario," : ", GROUP COUNT(*) USING "&&&&&&"
     PRINT

   ON LAST ROW

     PRINT 
     COLUMN 01,"TOTAL DE SOLICITUDES    :  ",COUNT(*) USING "&&&&&&&"

     SKIP 2 LINES

END REPORT


REPORT listado1(w_aux)
#--------------------
   DEFINE w_aux        RECORD
          lote         LIKE afi_solicitud.lote,
          agenc_cod    LIKE afi_solicitud.agenc_cod,
          num_solic    LIKE pro_mae_promotor.nro_solicitud ,
          ppaterno     LIKE pro_mae_promotor.paterno,
          mmaterno     LIKE pro_mae_promotor.materno,
          nnombres     LIKE pro_mae_promotor.nombres,
          n_folio      LIKE afi_solicitud.n_folio,
          frecafor     LIKE afi_solicitud.frecafor,
          fecrec       LIKE afi_rechaza_cert.f_rechazo,
          rdeta_cod    LIKE afi_rechaza_cert.rdeta_cod,
          observacion  LIKE afi_rechaza_cert.observacion,
          paterno      LIKE afi_solicitud.paterno ,
          materno      LIKE afi_solicitud.materno ,
        --nombres      LIKE afi_solicitud.nombres ,
          nombres      CHAR(9),
          n_seguro     LIKE afi_solicitud.n_seguro,
          n_rfc        LIKE afi_solicitud.n_rfc , 
          usuario      LIKE afi_solicitud.usuario 

   END RECORD

   DEFINE razon_social CHAR(40)

   OUTPUT
   PAGE LENGTH  90
   LEFT MARGIN   0
   RIGHT MARGIN  0
   TOP MARGIN    0
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
     COLUMN 160,"========="

     PRINT
     COLUMN 001,razon_social,
     COLUMN 149,"FECHA : ",hoy USING "DD/MM/YYYY"

     PRINT
     COLUMN 001,"AFIL031",
     COLUMN 035,"S O L I C I T U D E S    D E    A F I L I A C I O N    P O R",
                 tipo_reporte,"   R E C H A Z A D A S", COLUMN 149,hora 

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
     COLUMN 160,"---------"

     PRINT
     COLUMN 001,"Lote",
     COLUMN 006,"Estr",
     COLUMN 013,"N. Promo.",
     COLUMN 024,"Nombre Promotor",
     COLUMN 048,"Folio",
     COLUMN 060,"F. captura",
     COLUMN 072,"F. rechazo",
     COLUMN 084,"Motivo de Rechazo",
     COLUMN 114,"Nombre",
     COLUMN 144,"N.S.S.",
     COLUMN 160,"R.F.C."             

     PRINT
     COLUMN 001,"========================================",
     COLUMN 040,"========================================",
     COLUMN 080,"========================================",
     COLUMN 120,"========================================",
     COLUMN 160,"========="

   ON EVERY ROW

     LET estrc = NULL

     SELECT nombre_uni_n1 
     INTO   estrc
     FROM   com_nivel1
     WHERE coduni_n1 = w_aux.agenc_cod

     PRINT
     COLUMN 001,w_aux.lote USING "##" ,
   --COLUMN 006,estrc [1,6] ,
     COLUMN 010,w_aux.num_solic,
     COLUMN 024,w_aux.ppaterno [1,9]," ",w_aux.nnombres [1,13] ,
     COLUMN 048,w_aux.n_folio  USING "&&&&&&&&&",
     COLUMN 060,w_aux.frecafor USING "DD/MM/YYYY", 
     COLUMN 072,w_aux.fecrec   USING "DD/MM/YYYY",
     COLUMN 084,w_aux.observacion [1,29],
     COLUMN 114,w_aux.paterno  CLIPPED," ",
                w_aux.materno  CLIPPED," ",w_aux.nombres [1,9],
     COLUMN 144,w_aux.n_seguro USING "&&&&&&&&&&&&",
     COLUMN 160,w_aux.n_rfc                                           

   AFTER GROUP  OF w_aux.num_solic

     PRINT
     PRINT
     COLUMN 01,"SOLICITUDES RECHAZADAS POR : ",w_aux.num_solic,
               " : ", GROUP COUNT(*) USING "&&&&&&"
     PRINT

   AFTER GROUP  OF w_aux.lote

     PRINT
     PRINT
     COLUMN 01,"SOLICITUDES RECHAZADAS EN LOTE ",w_aux.lote,
               " : ",w_aux.usuario," : ", GROUP COUNT(*) USING "&&&&&&"
     PRINT

   ON LAST ROW

     PRINT 
     COLUMN 01,"TOTAL DE SOLICITUDES    :  ",COUNT(*) USING "&&&&&&&"

     SKIP 2 LINES

END REPORT


REPORT listado2(w_aux)
#--------------------
   DEFINE w_aux        RECORD
          lote         LIKE afi_solicitud.lote,
          agenc_cod    LIKE afi_solicitud.agenc_cod,
          num_solic    LIKE pro_mae_promotor.nro_solicitud ,
          ppaterno     LIKE pro_mae_promotor.paterno,
          mmaterno     LIKE pro_mae_promotor.materno,
          nnombres     LIKE pro_mae_promotor.nombres,
          n_folio      LIKE afi_solicitud.n_folio,
          frecafor     LIKE afi_solicitud.frecafor,
          fecrec       LIKE afi_rechaza_cert.f_rechazo,
          rdeta_cod    LIKE afi_rechaza_cert.rdeta_cod,
          observacion  LIKE afi_rechaza_cert.observacion,
          paterno      LIKE afi_solicitud.paterno ,
          materno      LIKE afi_solicitud.materno ,
        --nombres      LIKE afi_solicitud.nombres ,
          nombres      CHAR(9),
          n_seguro     LIKE afi_solicitud.n_seguro,
          n_rfc        LIKE afi_solicitud.n_rfc,  
          usuario      LIKE afi_solicitud.usuario    
   END RECORD

   DEFINE razon_social CHAR(40)

   OUTPUT
   PAGE LENGTH  90
   LEFT MARGIN   0
   RIGHT MARGIN  0
   TOP MARGIN    0
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
     COLUMN 160,"========="

     PRINT
     COLUMN 001,razon_social,
     COLUMN 149,"FECHA : ",hoy USING "DD/MM/YYYY"

     PRINT
     COLUMN 001,"AFIL031",
     COLUMN 035,"S O L I C I T U D E S    D E    A F I L I A C I O N    P O R",
                 tipo_reporte,"   R E C H A Z A D A S", COLUMN 149,hora 

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
     COLUMN 160,"---------"

     PRINT
     COLUMN 001,"Lote",
     COLUMN 006,"Estr",
     COLUMN 013,"N. Promo.",
     COLUMN 024,"Nombre Promotor",
     COLUMN 048,"Folio",
     COLUMN 060,"F. captura",
     COLUMN 072,"F. rechazo",
     COLUMN 086,"Motivo de Rechazo",
     COLUMN 114,"Nombre",
     COLUMN 144,"N.S.S.",
     COLUMN 160,"R.F.C."             

     PRINT
     COLUMN 001,"========================================",
     COLUMN 040,"========================================",
     COLUMN 080,"========================================",
     COLUMN 120,"========================================",
     COLUMN 160,"========="

   ON EVERY ROW

     LET estrc = NULL

     SELECT nombre_uni_n1 
     INTO   estrc
     FROM   com_nivel1
     WHERE  coduni_n1 = w_aux.agenc_cod

     PRINT
     COLUMN 001,w_aux.lote USING "##",
  -- COLUMN 006,estrc [1,6],
     COLUMN 010,w_aux.num_solic USING "#######",
     COLUMN 024,w_aux.ppaterno [1,9]," ",w_aux.nnombres [1,13],
     COLUMN 048,w_aux.n_folio  USING "&&&&&&&&&",
     COLUMN 060,w_aux.frecafor USING "DD/MM/YYYY", 
     COLUMN 072,w_aux.fecrec   USING "DD/MM/YYYY",
     COLUMN 084,w_aux.observacion [1,29],
     COLUMN 116,w_aux.paterno  CLIPPED," ",
                w_aux.materno  CLIPPED," ",w_aux.nombres [1,6],
     COLUMN 144,w_aux.n_seguro USING "&&&&&&&&&&&&",
     COLUMN 160,w_aux.n_rfc                                           

   AFTER GROUP  OF w_aux.lote

     PRINT
     PRINT
     COLUMN 01,"SOLICITUDES RECHAZADAS EN LOTE ",w_aux.lote,
               " : ",w_aux.usuario," : ", GROUP COUNT(*) USING "&&&&&&"

     SKIP TO TOP OF PAGE   

   AFTER GROUP  OF w_aux.rdeta_cod

     PRINT
     PRINT
     COLUMN 01,"SOLICITUDES RECHAZADAS POR : ",w_aux.rdeta_cod, 
               " : ", GROUP COUNT(*) USING "&&&&&&"
     PRINT

   ON LAST ROW

     PRINT 
     COLUMN 01,"TOTAL DE SOLICITUDES    :  ",COUNT(*) USING "&&&&&&&"

     SKIP 2 LINES

END REPORT
