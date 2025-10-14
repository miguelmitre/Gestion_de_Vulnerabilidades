#################################################################################
#Proyecto           => AFORE ( MEXICO )                                         #
#Propietario        => E.F.P.                                                   #
#Programa TCAAL020  => CONSTANCIA DE LIQUIDACION DE TRASPASO AFORE CEDENTE      #
#Autor              => JOSE FRANCISCO LUGO CORNEJO                              #
#Fecha              => 03 SEPTIEMBRE  2009                                      #
#Sistema            => TCAA                                                     #
#------------------------------MODIFICACIONES-----------------------------------#
#Requerimiento     => CPL-1191                                                  #
#Fecha y Autor     => 11-Abril-2013   -  Alejandro Chagoya S.                   #
#Descripcion       => Se modifica layout para agregar al final de cada registro #
#                  => el promedio del rendimiento neto, de las administradoras  #
#-------------------------------------------------------------------------------#
#Requerimiento     => CPL-1279                                                  #
#Fecha y Autor     => 29-Mayo-2013   -  Alejandro Chagoya S.                    #
#Descripcion       => Se optimizan queries de afi_mae_afiliado ya que son los   #
#                  => presentan un mayor costo hascia el motor de la DB         #
#CPL-1479          => FSR se obtiene el NTI por medio de la curp en caso de no  #
#                  => tener aperturado por medio de NSS                         #
#################################################################################
DATABASE   safre_af
GLOBALS
   DEFINE b_cancela SMALLINT
   DEFINE
      g_lastkey                          ,
      g_arr_menu                         ,
      g_arr_curr                         ,
      g_scr_line                         ,
      g_arr_count                        ,
      g_liquidada                        ,  
      g_long                             ,  
      i                                  ,  
      g_grupo                            ,
      g_tipo_proceso                     SMALLINT
   DEFINE
      g_enter                            CHAR(001),
      g_tipo_traspaso                    CHAR(002),
      g_desc_tipo_traspaso               CHAR(023),
      g_afore_cedente                    CHAR(030),
      g_ejecuta                          CHAR(300)
   DEFINE 
      reg_taa_cd_ctr_folio            RECORD  LIKE  taa_cd_ctr_folio.*
   DEFINE 
      g_num_registros                    INTEGER
   DEFINE 
      g_today                            DATE,
      g_usuario                          CHAR(008),
      g_lista                            CHAR(500),
      g_archivo                          CHAR(600),
      g_archivo2                         CHAR(600),
      g_archivo3                         CHAR(600),
      g_hora                             CHAR(008)
   DEFINE  
      g_seg_modulo                    RECORD LIKE safre_af:seg_modulo.*
   DEFINE 
      cza                             RECORD
          tipo_registro                  CHAR(02),
          tipo_carta                     CHAR(05),
          leyenda_cza                    CHAR(120),
          codigo_afore                   CHAR(03),
          fecha_generacion               CHAR(10),
          consec_lote                    CHAR(08),
          num_registros                  CHAR(08)
                                      END RECORD
   DEFINE
      det                             RECORD
          tipo_registro                  CHAR(02),
          tipo_carta                     CHAR(05),
          nombre                         CHAR(40),
          paterno                        CHAR(40),
          materno                        CHAR(40),
          calle                          CHAR(40),
          numero                         CHAR(10),
          num_interno                    CHAR(10),
          colonia                        CHAR(60),
          delegacion                     CHAR(40),
          ciudad                         CHAR(40),
          estado                         CHAR(40),
          codigo_postal                  CHAR(05),
          centro_reparto                 CHAR(05),
          lada                           CHAR(05),
          telefono                       CHAR(10),
          fecha_generacion               CHAR(10),
          nss                            CHAR(11),
          sexo                           CHAR(01),
          curp                           CHAR(18),
          id_20a21                       CHAR(20),
          n_folio                        CHAR(10),
          id_23a24                       CHAR(04),
          afore_cedente                  CHAR(30),
          id_26a32                       CHAR(79),
          siefore_1                      CHAR(08),
          siefore_7                      CHAR(08),
          siefore_10                     CHAR(08),
          siefore_11                     CHAR(08),
          siefore_13                     CHAR(08),
          siefore_15                     CHAR(08),
          id_39a42                       CHAR(46),
          rfc                            CHAR(13),
          id_44                          CHAR(10),
          fecha_recep_recur              CHAR(10),
          id_46                          CHAR(10),
          nacionalidad                   CHAR(20),
          id_48                          CHAR(40),
          rendimientos                   CHAR(1014),
          id_153                         CHAR(02),
          saldos_1a4                     CHAR(52),
          afo_recep                      CHAR(03),
          nom_afo_recep                  CHAR(30),
          id_160                         CHAR(40),
          udis_bono                      CHAR(13),
          fecha_redencion                CHAR(10),
          codigo_barras                  CHAR(28),
          saldos_issste                  CHAR(26),
          promedio_rendi                 CHAR(7)       #CPL-1191
                                      END RECORD
END GLOBALS

MAIN
   OPTIONS
            PROMPT   LINE   LAST,
            ACCEPT   KEY    CONTROL-I
            DEFER    INTERRUPT
   CALL STARTLOG (FGL_GETENV("USER")||".TCAAL020.log") #ACS
   LET b_cancela = 0
   CALL     F_100_inicio()
   CALL     fn_elige_folio()
   --CALL     F_500_proceso()
END MAIN

FUNCTION    F_100_inicio()
   LET      g_today                      =  TODAY 
   SELECT   *,user
     INTO   g_seg_modulo.*,g_usuario
     FROM   seg_modulo
    WHERE   modulo_cod                   =  'taa';
   LET      g_liquidada                  =  103
   INITIALIZE      cza.*,  det.*        TO    NULL
END FUNCTION

#--------------------------
FUNCTION fn_elige_folio()
#--------------------------

   DEFINE l_txt CHAR(2000)
   DEFINE pos   integer
   DEFINE band  smallint
   DEFINE cad_construct char(100)
   DEFINE cont_1 INTEGER

   DEFINE arr_captura_filtro ARRAY[100] OF RECORD
          fecha_liquidacion date ,
          folio             dec(10,0),
          tipo_traspaso     smallint ,
          desc_tipo_traspaso char(20),
          tot_folio          INTEGER 
   END RECORD

   OPEN WINDOW  tcaal0200   AT 2,2  WITH  FORM  "TCAAL0200"  ATTRIBUTE(BORDER)
   DISPLAY  "TCAAL020  GENERA CONSTANCIAS DE LIQUIDACION DE TRASPASOS (A-A CEDENTE) ",
            "                  "  AT  3,1  ATTRIBUTE(REVERSE)
   DISPLAY  " <Esc> Continuar            < CTRL-C > Salir                     ",
            "                         "   AT  1,1   ATTRIBUTE(REVERSE)
   DISPLAY  g_today USING "DD-MM-YYYY" AT 1,64 ATTRIBUTE(REVERSE)

   CONSTRUCT  cad_construct ON a.fecha_liquidacion
                          FROM fecha_liquidacion

     ON KEY(ESC)
          LET int_flag = FALSE
          EXIT CONSTRUCT
    ON KEY(INTERRUPT)
       LET band = 0
       EXIT CONSTRUCT
   END CONSTRUCT

   IF NOT int_flag THEN
      LET l_txt = "SELECT FIRST 50 a.fecha_liquidacion , ",
                        " a.folio             , ",
                        " a.tipo_traspaso     , ", 
                        " CASE WHEN a.tipo_traspaso = 1 THEN 'NORMAL' ",
                        "      WHEN a.tipo_traspaso = 2 THEN 'COMPLEMENTARIO' ",
                        "      WHEN a.tipo_traspaso = 3 THEN 'INTERNET' ",
                        "      WHEN a.tipo_traspaso = 4 THEN 'PROCESOS' ",
                        " END desc_tipo_traspaso, ",
                        " COUNT(*) tot_folio ",
                   "FROM taa_cd_ctr_folio a , ",
                       " taa_cd_det_cedido b ",
                   "WHERE a.estado = 103  ",
                   "AND   a.folio = b.folio ", 
                   "AND   b.estado in (103,12,99,98) ",
                   "GROUP BY 1,2,3 ",
                   "ORDER BY 1 desc,3,2 "
                         
            
           WHILE TRUE

           PREPARE qry_filtro FROM l_txt
           DECLARE cur_filtro CURSOR FOR qry_filtro

           LET cont_1 = 1

           FOREACH cur_filtro INTO arr_captura_filtro[cont_1].*
                   LET cont_1 = cont_1 + 1
           END FOREACH

           IF cont_1 = 1 THEN
              ERROR ""
              CLEAR FORM
              PROMPT " No Existen Registros...<ENTER> para Salir "
              ATTRIBUTE(REVERSE) FOR CHAR g_enter
              CLOSE WINDOW tcaal0200
              EXIT WHILE

           ELSE 

             CALL SET_COUNT(cont_1-1)
             DISPLAY " Ctrl-i Elige Folio                                            Ctrl-c Salir  " AT 1,1
             DISPLAY ARRAY arr_captura_filtro TO  scr_folios.*

               ON KEY (CONTROL-i)    -- elige folio

                   LET  pos = ARR_CURR()

                   CALL F_500_proceso(arr_captura_filtro[pos].folio)

                   LET band = 1
                   EXIT DISPLAY

                ON KEY (INTERRUPT)
                   LET band = 0
                   EXIT DISPLAY
             END DISPLAY

             IF band = 0 THEN
                PROMPT " PROCESO FINALIZADO <ENTER> PARA SALIR..." FOR CHAR g_enter
                CLOSE WINDOW tcaal0200
                EXIT WHILE
             END IF
           END IF

       END WHILE

      ELSE
              CLOSE WINDOW tcaal0200
      END IF

END FUNCTION

#----------------------------------
FUNCTION    F_500_proceso(v_folio)
#----------------------------------

   DEFINE   v_folio       DEC(10,0)

   DEFINE   l_registros   INTEGER,
            l_day         DATE
            
   OPEN WINDOW  TCAAL020   AT 2,2  WITH  FORM  "TCAAL020"  ATTRIBUTE(BORDER)

   DISPLAY  "TCAAL020  GENERA CONSTANCIAS DE LIQUIDACIÓN DE TRASPASOS (A-A CEDENTE) ",
            "                  "  AT  3,1  ATTRIBUTE(REVERSE)
   DISPLAY  " <Esc> Continuar            < CTRL-C > Salir                     ",
            "                         "   AT  1,1   ATTRIBUTE(REVERSE)
   DISPLAY  g_today USING "DD-MM-YYYY" AT 1,64 ATTRIBUTE(REVERSE)

   SELECT   MAX(fecha_liquidacion)
     INTO   l_day
     FROM   taa_cd_ctr_folio a
    WHERE   folio = v_folio 
      AND   estado                   =   103
      AND   tipo_traspaso           IN(1,3,4)

   DECLARE  cur_folios         CURSOR   FOR
   SELECT   a.*
     FROM   safre_af:taa_cd_ctr_folio a
    WHERE   a.folio = v_folio 
      AND   a.estado                 =  103
      AND   tipo_traspaso          IN(1,3,4)
      AND   a.fecha_liquidacion      =  l_day
    ORDER   BY  tipo_traspaso  desc ; 

   FOREACH  cur_folios         INTO   reg_taa_cd_ctr_folio.*

            SELECT   COUNT(*)
              INTO   l_registros
              FROM   safre_af:taa_cd_det_cedido
             WHERE   folio                     =  reg_taa_cd_ctr_folio.folio
               --AND   estado                    =  g_liquidada; --CPL-3820
               AND   estado                    IN (12,103,99,98);
            LET      g_tipo_proceso   =  reg_taa_cd_ctr_folio.tipo_traspaso
            IF       g_tipo_proceso            =  1     THEN
                     LET      g_desc_tipo_traspaso      =  'PROMOTOR'
            ELSE
            IF       g_tipo_proceso            =  3     THEN
                     LET      g_desc_tipo_traspaso      =  'INTERNET'
            ELSE
            IF       g_tipo_proceso            =  4     THEN
                     LET      g_desc_tipo_traspaso      =  'DIVERSOS'
            END IF
            END IF
            END IF
            DISPLAY  reg_taa_cd_ctr_folio.folio     TO  FORMONLY.folio
            DISPLAY  g_desc_tipo_traspaso           TO  FORMONLY.desc_tipo
            DISPLAY  reg_taa_cd_ctr_folio.fecha_presentacion    TO
                     FORMONLY.fecha_presentacion
            DISPLAY  reg_taa_cd_ctr_folio.fecha_envio_saldos    TO
                     FORMONLY.fecha_envio_saldos
            DISPLAY  reg_taa_cd_ctr_folio.fecha_liquidacion     TO
                     FORMONLY.fecha_liquidacion
            DISPLAY  l_registros                TO  FORMONLY.l_registros
            WHILE    TRUE
                     PROMPT    " ES EL FOLIO DESEADO TECLEE [S/N] ?..."
                               FOR      g_enter
                     IF        g_enter      MATCHES   "[sSnN]"       THEN
                               IF     g_enter      MATCHES   "[sS]"    THEN
                                      EXIT FOREACH
                               ELSE
                                      LET       reg_taa_cd_ctr_folio.folio  =  0
                                      CONTINUE  FOREACH
                               END IF
                     END  IF
            END      WHILE
   END FOREACH
   IF       reg_taa_cd_ctr_folio.folio      IS  NULL     OR
            reg_taa_cd_ctr_folio.folio       =  0        THEN
            --PROMPT   "  TECLEE ENTER PARA SALIR..."  FOR  g_enter
            --EXIT     PROGRAM --CPL-3820
            CLOSE WINDOW TCAAL020
            RETURN 
   END IF
   DISPLAY  "  PROCESANDO INFORMACION ......                                "
               AT   18,1   --ATTRIBUTE(REVERSE)
   CALL     F_520_genera_archivo()
   IF b_cancela = 1 THEN 
      LET b_cancela = 0
      CLOSE    WINDOW   TCAAL020
      RETURN
   END IF
   DISPLAY  "                                                                  "
               AT   18,1
   PROMPT  "  PROCESO FINALIZADO <Enter> Para Salir..." FOR g_enter
   CLOSE    WINDOW   TCAAL020
END FUNCTION

-------------------------------------------------------------------------------

FUNCTION F_520_genera_archivo()

  DEFINE l_registros  INTEGER
  DEFINE v_nss_tmp    CHAR(11)    -- CPL-3851
  DEFINE v_cadena     CHAR(1000)  -- CPL-3851

  LET g_hora          =  TIME
  LET g_num_registros =  0
  LET g_archivo       =
            g_seg_modulo.ruta_envio   CLIPPED,"/",g_usuario   CLIPPED,
            ".TCAAL020.",g_today      USING   "YYMMDD", "_",g_hora CLIPPED

  START REPORT R_590_imprime_archivo TO g_archivo

  DISPLAY "      PROCESANDO  INFORMACION ..............         "  AT  16,1
  DISPLAY "ARCHIVO:",g_archivo," "    AT  16,1   -- ATTRIBUTE(REVERSE)

  CALL F_530_selecciona_tipo_traspaso()
  
  IF b_cancela THEN 
     RETURN
  END IF

  LET cza.tipo_registro    =  "01"
  LET cza.tipo_carta       =  "30225"
  LET cza.leyenda_cza      =  "Constancia de Liquidación de Traspaso"
  LET cza.fecha_generacion =  g_today   USING  "DD/MM/YYYY"
  LET cza.consec_lote      =  g_tipo_proceso  USING '&&&&&&&&'

  SELECT codigo_afore,razon_social
    INTO cza.codigo_afore,g_afore_cedente
    FROM safre_af:tab_afore_local;

  IF g_tipo_proceso = 1 THEN
     SELECT 1
       FROM tmp_tipo_traspaso
      WHERE tipo_traspaso = '12'
      
     IF status <> NOTFOUND THEN
        LET cza.consec_lote = 2 USING  '&&&&&&&&'
     END IF
  END IF

  SELECT COUNT(UNIQUE  n_seguro) INTO l_registros
    FROM safre_af:taa_cd_det_cedido  cd
   WHERE cd.folio          =  reg_taa_cd_ctr_folio.folio
     AND cd.tipo_traspaso IN (SELECT tipo_traspaso
                                FROM tmp_tipo_traspaso)
   --AND cd.estado         = 103; --CPL-3820
     AND cd.estado        IN (12,103,99,98);

  DISPLAY l_registros TO FORMONLY.l_registros

  DECLARE cur_ced CURSOR FOR
   SELECT n_seguro, ident_lote_solici[3,5]
     FROM safre_af:taa_cd_det_cedido  cd
    WHERE cd.folio          =  reg_taa_cd_ctr_folio.folio
      AND cd.tipo_traspaso IN (SELECT  tipo_traspaso FROM tmp_tipo_traspaso)
    --AND cd.estado         =  103; --CPL-3820
      AND cd.estado        IN (12,103,99,98);
      
  FOREACH cur_ced INTO det.nss, det.afo_recep
    ##CPL-1479  
    # Verificamos si esta aperturada la cuenta por medio del NSS 
    
    SELECT UNIQUE "OK"
      FROM afi_mae_afiliado
     WHERE n_seguro = det.nss

    IF SQLCA.SQLCODE <> 0 THEN 
    	
    	 LET v_nss_tmp = det.nss  -- CPL-3851
     	
     	LET v_cadena = '',
     	               '\n SELECT FIRST 1 n_seguro                                 ', -- CPL-3851 ESTE QUERY FUE EL REPORTE
                     '\n   FROM afi_mae_afiliado                                 ',
                     '\n  WHERE n_unico IN ( SELECT UNIQUE n_unico               ',
   	                 '\n                       FROM safre_af:taa_cd_det_cedido   ',
   	                 '\n                      WHERE n_seguro  =  "',v_nss_tmp,'" ', -- CPL-3851
   	                 '\n                    )                                    ',
   	                 '\n    AND n_seguro = "',v_nss_tmp,'"                       '  -- CPL-3851
   	  PREPARE v_cons FROM v_cadena
   	  EXECUTE v_cons INTO det.nss
   	
   	   IF SQLCA.SQLCODE <> 0 THEN 
   		    -- DISPLAY "EL NSS: ", det.nss, "NO SE ENCUENTRA APERTURADO"
   		    DISPLAY "EL NSS: ", v_nss_tmp, " NO SE ENCUENTRA APERTURADO"
   		    SLEEP 1
   	   END IF  
   	 
    END IF 

    ##CPL-1479    	
   	
    LET g_num_registros = g_num_registros + 1
    DISPLAY g_num_registros TO l_procesados 
    
    CALL F_550_arma_datos_generales()
    CALL F_550_trae_saldos()
    OUTPUT TO REPORT R_590_imprime_archivo()
    
    INITIALIZE det.* TO  NULL
    #if g_num_registros  = 100  then exit  foreach  end if
     
  END FOREACH --cur_ced

  FINISH REPORT R_590_imprime_archivo

  LET g_lista = "chmod   777 ",g_archivo CLIPPED
  RUN g_lista
  ERROR   "                                                         "

END FUNCTION

-------------------------------------------------------------------------------



FUNCTION    F_530_selecciona_tipo_traspaso()
   DEFINE   l_cur                           ,
            l_arr_count                     ,
            l_hay_proceso                   ,
            l_scr_line                      ,
            l_arr_curr                      SMALLINT,
            l_registros                     INTEGER
   DEFINE   l_tipo_tra_desc                 CHAR(50),
            l_tipo_traspaso                 CHAR(02)
   DEFINE   l_sql                           CHAR(1000)
   DEFINE   l_construct                     CHAR(1000)

   DEFINE   l_t_trasp                    ARRAY[100]  OF   RECORD
            cursor                          CHAR(01),
            tipo_traspaso                   CHAR(02),
            tipo_tra_desc                   CHAR(40),
            registros                       INTEGER  
                                         END   RECORD

   LET      l_hay_proceso              =  0

   WHENEVER ERROR CONTINUE
     DROP TABLE tmp_tipo_traspaso
   WHENEVER ERROR STOP

   CREATE   TEMP   TABLE   tmp_tipo_traspaso
           (tipo_traspaso        CHAR(02));

   OPEN     WINDOW   TCAAL0201        AT  2,2 WITH     FORM    "TCAAL0201"   ATTRIBUTE(BORDER)

   DISPLAY  "TCAAL0201   <<  ORIGEN DE TRASPASOS A GENERAR MARCADOS CON * >>                                   "    AT  1,1   ATTRIBUTE    (REVERSE)

   FOR      i                        =  1        TO  100
            INITIALIZE     l_t_trasp[i].*      TO  NULL
   END FOR

   LET      i                        =  1
   LET      l_registros              =  0
   LET      l_tipo_traspaso          =  NULL
   LET      l_tipo_tra_desc          =  NULL

   CONSTRUCT  l_construct    ON  a.tipo_traspaso   FROM  tipo_traspaso_sol
            ON KEY  ( RETURN )
                     LET      INT_FLAG        =  FALSE
                     EXIT     CONSTRUCT
            ON KEY  ( INTERRUPT )
                     EXIT     CONSTRUCT
   END CONSTRUCT

   IF       INT_FLAG                 =  FALSE     THEN
            LET       l_sql                   =
                      " SELECT   UNIQUE  '*',a.tipo_traspaso,  ",
                      "          t.descripcion ,COUNT(n_seguro)  ",
                      "   FROM   safre_af:taa_cd_det_cedido   a ,",
                      "          safre_af:taa_cd_tipo_traspaso  t ",
                      "  WHERE  ",l_construct   CLIPPED,
                      "    AND   a.folio      =   ",reg_taa_cd_ctr_folio.folio,
                      "    AND   a.tipo_traspaso   =  t.tipo_traspaso ",
                      "    AND   a.estado          in (12,103,99,98)        ",
                      "  GROUP   BY  1,2,3                            ",
                      "  ORDER   BY  1,2,3                            "
            LET       l_sql                =  l_sql    CLIPPED
            DISPLAY  "  EXTRAE ORIGEN DE TRASPASO Y NUMERO DE REGISTROS........      "
               AT   18,1   ATTRIBUTE(REVERSE)
            PREPARE   qry_consul         FROM    l_sql
            DECLARE   cursor_c     CURSOR FOR    qry_consul
            FOREACH   cursor_c           INTO    l_t_trasp[i].*
                      LET      i           =  i   +  1
            END FOREACH
   END IF

   DISPLAY  " <Esc>Generar Constancia  <Enter> Omite Origen          <Control-c>Cancelar  " AT  3,1  ATTRIBUTE  (REVERSE)

   IF      (i  -  1)          >=  1    THEN
            CALL     SET_COUNT(i  - 1)
            INPUT    ARRAY    l_t_trasp  WITHOUT DEFAULTS  FROM  scr_tipo_tra.*
                     BEFORE   ROW
                              LET      l_arr_curr         =  ARR_CURR()
                              LET      l_arr_count        =  ARR_COUNT()
                              LET      l_scr_line         =  SCR_LINE()
                              IF       l_arr_curr         >  l_arr_count    THEN
                                       EXIT INPUT
                              END IF
                              DISPLAY  l_t_trasp[l_arr_curr].*   TO
                                       scr_tipo_tra[l_scr_line].*
                                       ATTRIBUTE(REVERSE)
                                       DISPLAY  "  GENERA UNICAMENTE ORIGEN DE TRASPASO",
                                       " MARCADO CON *                       "
                                       AT   18,1   ATTRIBUTE(REVERSE)

                     AFTER    ROW
                              LET      l_arr_curr         =  ARR_CURR()
                              LET      l_scr_line         =  SCR_LINE()
                              DISPLAY  l_t_trasp[l_arr_curr].*   TO
                                       scr_tipo_tra[l_scr_line].*
                     AFTER    FIELD    cursor
                              IF       l_arr_curr     >=  (l_arr_count)  THEN
                                       LET      g_lastkey     =   FGL_LASTKEY()
                                       IF     ((g_lastkey     =
                                                FGL_KEYVAL("down"))      OR
                                               (g_lastkey     =
                                                FGL_KEYVAL("return"))    OR
                                               (g_lastkey     = 
                                                FGL_KEYVAL("tab"))       OR
                                               (g_lastkey     =
                                                FGL_KEYVAL("right"))) THEN
                                                ERROR    "    NO HAY MAS OPCIONES EN ESA DIRECCION ..."
                                                NEXT    FIELD     cursor
                                       END IF
                              END IF

                     ON KEY   ( RETURN )
                              INITIALIZE  l_t_trasp[l_arr_curr].*    TO  NULL
                              DISPLAY  l_t_trasp[l_arr_curr].*   TO
                                       scr_tipo_tra[l_scr_line].*
                                       ATTRIBUTE(REVERSE)

                     ON KEY  ( INTERRUPT )
                              FOR   i= 1 TO 100
                                    INITIALIZE l_t_trasp[i].* TO NULL
                              END FOR
                              LET l_hay_proceso = 2
                              EXIT INPUT

                     ON KEY  ( ESC )
                              EXIT INPUT
            END INPUT
   END IF

   FOR i = 1 TO l_arr_count

            IF l_t_trasp[i].cursor = "*" THEN

               LET l_hay_proceso =  1
               LET l_registros   =  l_registros + l_t_trasp[i].registros
               INSERT INTO tmp_tipo_traspaso VALUES ( l_t_trasp[i].tipo_traspaso)

            END IF

   END FOR

   IF l_hay_proceso = 0  THEN
            PROMPT   " CANCELANDO GENERACION DE CONSTANCIA <Enter> para Salir..." FOR g_enter
            LET b_cancela = 1
            CLOSE    WINDOW  TCAAL0201
            RETURN
   END IF

   IF l_hay_proceso = 2  THEN
            PROMPT   " CANCELANDO GENERACION DE CONSTANCIA <Enter> para Salir..." FOR g_enter
            LET b_cancela = 1
            CLOSE    WINDOW  TCAAL0201
            RETURN
   END IF

   LET      cza.num_registros           =  l_registros   USING "&&&&&&&&"
   LET      INT_FLAG                    =  FALSE
   CLEAR    FORM 
   CLOSE    WINDOW  TCAAL0201
END FUNCTION

FUNCTION    F_550_arma_datos_generales()
   DEFINE   l_fecha              CHAR(08),
            l_fecha_reden        DATE,
            l_nac                CHAR(3)

            LET l_nac = ""

   LET      det.tipo_registro           =  '02'
   LET      det.tipo_carta              =  '30225'
   LET      det.fecha_generacion        =  g_today   USING "DD/MM/YYYY"
   LET      cza.fecha_generacion        =  g_today   USING "DD/MM/YYYY"

   SELECT   afi.n_unico  ,  afi.n_rfc   ,  afi.n_folio,
            afi.paterno  ,  afi.materno ,  afi.nombres,
            afi.nacionalidad
     INTO   det.curp     ,  det.rfc     ,  det.n_folio,
            det.paterno  ,  det.materno ,  det.nombre,
            l_nac
     FROM   safre_af:afi_mae_afiliado  afi
    WHERE   afi.n_seguro                 =  det.nss;

   SELECT nac.nacionalidad INTO det.nacionalidad
    FROM tab_nacionalidad nac
   WHERE nac.codigo_pais = l_nac

   CALL     F_522_arma_siefores()
   CALL     F_523_trae_telefono()
   CALL     F_570_trae_domicilio()
   
   SELECT   MAX(fecha_reden)
     INTO   l_fecha
     FROM   safre_af:dis_det_bono
    WHERE   n_unico                      =  det.curp;
   LET      det.fecha_recep_recur        = 
            reg_taa_cd_ctr_folio.fecha_liquidacion    USING "DD/MM/YYYY"
   IF       l_fecha      IS   NOT   NULL  THEN
            LET    l_fecha_reden         =
                   MDY(l_fecha[5,6],l_fecha[7,8],l_fecha[1,4])
   END IF
   CALL     F_580_arma_tabla_comisiones()
   IF       l_fecha_reden                IS    NULL          OR
            l_fecha_reden                 <   "01/01/2009"   THEN
            SELECT   MAX(fecha_red_bono)
              INTO   l_fecha_reden        
              FROM   safre_af:taa_viv_recepcion
             WHERE   nss                  =  det.nss
   END IF
   IF       l_fecha_reden                IS  NOT  NULL       AND
            l_fecha_reden                <> "01/01/0001"     AND 
            l_fecha_reden                <> "12/31/1899"     THEN
            LET      det.fecha_redencion    = l_fecha_reden USING "DD/MM/YYYY"
   ELSE
            LET      det.fecha_redencion    =  "          "  
   END IF
########    No_carta  700 ,  ref  21  ,  000 ,  nss
   LET      det.codigo_barras          =  "C70021000",det.nss,
            reg_taa_cd_ctr_folio.fecha_liquidacion     USING   "YYYYMMDD"
   LET      det.afore_cedente          =  g_afore_cedente
   SELECT   afore_desc
     INTO   det.nom_afo_recep
     FROM   tab_afore
    WHERE   afore_cod                    =  det.afo_recep
   IF       det.nom_afo_recep      IS  NULL      THEN
            LET      det.nom_afo_recep       =  " "
   END IF 
END FUNCTION

FUNCTION  F_522_arma_siefores()
   DEFINE   l_siefore                         CHAR(08),
            l_subcuenta                       SMALLINT

   DECLARE  cur_sie         CURSOR   FOR
   SELECT   reg.subcuenta,sie.razon_social[1,8]
     FROM   cta_regimen  reg,  tab_siefore_local  sie
    WHERE   reg.nss                      =  det.nss
      AND   reg.subcuenta               IN(1,7,10,11,13,15)
      AND   reg.codigo_siefore           =  sie.codigo_siefore;

   FOREACH  cur_sie         INTO  l_subcuenta,l_siefore
            CASE     l_subcuenta
                      WHEN    1     LET     det.siefore_1       =  l_siefore 
                      WHEN    7     LET     det.siefore_7       =  l_siefore 
                      WHEN    10    LET     det.siefore_10      =  l_siefore 
                      WHEN    11    LET     det.siefore_11      =  l_siefore 
                      WHEN    13    LET     det.siefore_13      =  l_siefore 
                      WHEN    15    LET     det.siefore_15      =  l_siefore 
            END CASE
   END   FOREACH

END FUNCTION

FUNCTION  F_523_trae_telefono()
   DEFINE   l_telefono           CHAR(15)

  DECLARE  c_tel           CURSOR     FOR
   SELECT   tel.cve_lada, tel.telefono
     INTO   det.lada,   det.telefono
     FROM   safre_af:afi_mae_afiliado afi, safre_af:afi_telefono  tel
    WHERE   tel.nss                    =  det.nss
      AND   tel.nss                    =  afi.n_seguro
      AND   tel.n_folio                =  afi.n_folio
      AND   tel.tipo_solicitud         =  afi.tipo_solicitud
      AND   tel.telefono     IS       NOT   NULL
      AND   tel.tel_cod                <   7
    ORDER   BY  tel.factualiza     DESC

   FOREACH  c_tel               INTO  det.lada,det.telefono
            EXIT  FOREACH
   END FOREACH

END FUNCTION

FUNCTION  F_550_trae_saldos()
   DEFINE    l_pesos                      ,
             l_acciones                   DEC(16,6)

   DEFINE lc_query                        CHAR(3000),
          v_anio_consulta,
          v_anio_actual                   CHAR(4),
          v_tabla                         CHAR(13),
          v_existe_dis_ctaxx              INTEGER

   INITIALIZE lc_query, v_anio_consulta, v_anio_actual, v_tabla, v_existe_dis_ctaxx TO NULL
   
   LET v_anio_consulta = YEAR(reg_taa_cd_ctr_folio.fecha_liquidacion)
   LET v_anio_actual = YEAR(TODAY)

   DECLARE  cur_grupo       CURSOR  FOR
    SELECT  UNIQUE     (grupo)
      FROM  safre_af:taa_cd_inf_grupo
     ORDER  BY  grupo;
   FOREACH  cur_grupo       INTO    g_grupo

   IF v_anio_consulta = v_anio_actual THEN
   
      LET lc_query = "",
                     "\n SELECT   SUM(monto_en_pesos * -100), SUM(monto_en_acciones * -100)",
                     "\n   FROM   safre_af:dis_cuenta     dc, ",
                     "\n          safre_af:taa_cd_inf_grupo gp ",
                     "\n  WHERE   dc.nss = ", det.nss, "",
                     "\n    AND   dc.folio = ", reg_taa_cd_ctr_folio.folio,
                     "\n    AND   gp.grupo = ", g_grupo,
                     "\n    AND   dc.subcuenta = gp.subcuenta "

   ELSE
      LET v_tabla = 'dis_cuenta'||v_anio_consulta[3,4]

      --Se valida que exista la tabla
      SELECT count(*)
        INTO v_existe_dis_ctaxx
       FROM systables
      WHERE tabname MATCHES v_tabla

      IF v_existe_dis_ctaxx > 0 THEN
      
      LET lc_query = "",
                     "\n SELECT   SUM(monto_en_pesos * -100), SUM(monto_en_acciones * -100)",
                     "\n   FROM   safre_af:dis_cuenta", v_anio_consulta[3,4], " dc, ",
                     "\n          safre_af:taa_cd_inf_grupo  gp",
                     "\n  WHERE   dc.nss = ", det.nss,
                     "\n    AND   dc.folio = ", reg_taa_cd_ctr_folio.folio,
                     "\n    AND   gp.grupo = ", g_grupo,
                     "\n    AND   dc.subcuenta = gp.subcuenta "
         ELSE
            LET lc_query = "",
                           "\n SELECT   SUM(monto_en_pesos * -100), SUM(monto_en_acciones * -100)",
                           "\n   FROM   safre_af:dis_cuenta  dc, ",
                           "\n          safre_af:taa_cd_inf_grupo  gp",
                           "\n  WHERE   dc.nss = ", det.nss,
                           "\n    AND   dc.folio = ", reg_taa_cd_ctr_folio.folio,
                           "\n    AND   gp.grupo = ", g_grupo,
                           "\n    AND   dc.subcuenta = gp.subcuenta "
         END IF
      END IF

         {SELECT   SUM(monto_en_pesos * -100),SUM(monto_en_acciones * -100)
           INTO   l_pesos,l_acciones
           FROM   safre_af:dis_cuenta     dc ,
                  safre_af:taa_cd_inf_grupo      gp
          WHERE   dc.nss                  =  det.nss
            AND   dc.folio                =  reg_taa_cd_ctr_folio.folio
            AND   gp.grupo                =  g_grupo
            AND   dc.subcuenta            =  gp.subcuenta}
         PREPARE prp_montos FROM lc_query
         EXECUTE prp_montos INTO l_pesos,l_acciones

         IF       l_pesos                IS   NULL    THEN
                  LET      l_pesos        =  0
         END IF
         IF       l_pesos                IS   NULL    THEN
                  LET      l_acciones     =  0
         END IF
         IF       g_grupo                <=  4  THEN 
                  LET      det.saldos_1a4    =  det.saldos_1a4  CLIPPED,
                              l_pesos  USING  "&&&&&&&&&&&&&"  CLIPPED
         END IF
         IF       g_grupo                 =  5       THEN
                  LET      det.udis_bono  =  l_acciones USING "&&&&&&&&&&&&&"
         END IF
         IF       g_grupo                >=  6  THEN 
                  LET      det.saldos_issste    =  det.saldos_issste CLIPPED,
                           l_pesos     USING  "&&&&&&&&&&&&&"  CLIPPED
         END IF
   END  FOREACH
END FUNCTION

FUNCTION    F_570_trae_domicilio()
    DEFINE    domi        INTEGER
    DEFINE    l_delegacion         LIKE   afi_domicilio.delega,
              l_estado             LIKE   afi_domicilio.estado,
              l_ciudad             LIKE   afi_domicilio.ciudad
   SELECT   MIN(s.ROWID)
     INTO   domi
     FROM   safre_af:afi_mae_afiliado  afi, safre_af:afi_domicilio s
    WHERE   s.nss                    =  det.nss
      AND   s.nss                    =  afi.n_seguro
      AND   s.n_folio                =  afi.n_folio
      AND   s.tipo_solicitud         =  afi.tipo_solicitud
      AND   s.marca_envio            = "X";
   IF       SQLCA.SQLCODE           <>  0    THEN
            RETURN
   END IF
   SELECT   o.calle, o.numero, o.depto, o.colonia, o.codpos,
            o.delega, o.estado, o.ciudad
     INTO   det.calle, det.numero, det.num_interno,  det.colonia,
            det.codigo_postal, l_delegacion, l_estado , l_ciudad  
     FROM   afi_domicilio o
    WHERE   o.rowid                  =  domi;
   SELECT   am.centro_reparto
     INTO   det.centro_reparto
     FROM   tab_reparto am
    WHERE   am.codigo_postal         =  det.codigo_postal;
   SELECT   p.estad_desc
     INTO   det.estado
     FROM   tab_estado p
    WHERE   p.estad_cod              =  l_estado;
   SELECT   q.deleg_desc
     INTO   det.delegacion
     FROM   tab_delegacion q
    WHERE   q.deleg_cod              =  l_delegacion;
   SELECT   q.ciudad_desc
     INTO   det.ciudad
     FROM   tab_ciudad q
    WHERE   q.ciudad_cod             =  l_ciudad;
END  FUNCTION

FUNCTION F_580_arma_tabla_comisiones()
  DEFINE   cat            RECORD
           rendim_afo                            DECIMAL(4,2),
           comis_afo                             DECIMAL(5,2),
           rendim_neto                           DECIMAL(4,2)
                          END RECORD,
           l_rendi                               CHAR(1014),
           afore                                 CHAR(20),
           cuantos                               SMALLINT,
           cuantos2                              SMALLINT,
           vsiefore                              SMALLINT,
           l_avg_simple                          DECIMAL(6,2)  #promedio simple de los rendimientos

   SELECT   c.codigo_siefore  INTO   vsiefore
     FROM   cta_nss_regimen c
    WHERE   c.nss                    =  det.nss
      AND   grupo_regimen            =  1;

   LET      cuantos                  =  0
   
   LET      cuantos2                 =  0
   
   SELECT   COUNT(*) 
     INTO   cuantos
     FROM   tab_rendimiento_neto a
    WHERE   reg_taa_cd_ctr_folio.fecha_liquidacion BETWEEN a.fecha_ini AND a.fecha_fin
      AND   a.siefore_cod            =  vsiefore;
   
   SELECT tot_afore
   INTO cuantos2
   FROM tab_comp_afore
   WHERE cod_compara = 1
   
   -- CPL-2804. se busca
   IF cuantos < cuantos2 THEN 
   	  IF vsiefore = 90 THEN                                  -- CPL-2243
   	     -- NO ALERTAR DADO QUE NO SE TIENEN VALORES         -- CPL-2243
      ELSE                                                   -- CPL-2243  
         PROMPT "No estan completas las Afores en el Comparativo, <Enter> " FOR  g_enter
      END IF                                                 -- CPL-2243
   END IF
 
   LET l_rendi            = ' '
   LET det.promedio_rendi = ""

  DECLARE cur_tab_comi CURSOR FOR
   SELECT m.afore_desc, a.rendimiento, a.comision, a.rendimiento_neto
     FROM tab_rendimiento_neto a,  tab_afore m
    WHERE reg_taa_cd_ctr_folio.fecha_liquidacion BETWEEN a.fecha_ini AND a.fecha_fin
      AND a.siefore_cod =   vsiefore
      AND a.afore_cod   =   m.afore_cod
      AND a.afore_cod   NOT IN(999) 
    ORDER BY  a.rendimiento_neto DESC;
    
  FOREACH cur_tab_comi INTO afore,cat.*
  	
    FOR i = 1 TO 20
      IF afore[i] IS NULL OR afore[i] = ' ' THEN
         LET afore[i] = "?"
      END  IF
    END FOR
    
    LET l_rendi = l_rendi CLIPPED, afore CLIPPED
    LET l_rendi = l_rendi CLIPPED, cat.rendim_afo  USING  "#&.&&","%" CLIPPED
    LET l_rendi = l_rendi CLIPPED, cat.comis_afo   USING "##&.&&","%" CLIPPED
    LET l_rendi = l_rendi CLIPPED, cat.rendim_neto USING  "#&.&&","%" CLIPPED
    
  END FOREACH
  
  -- CPL-2243
  IF (vsiefore = 90) AND (l_rendi = ' ') THEN
  	 LET l_rendi = ""
  	 FOR cuantos = 1 TO 11                    -- NUMERO DE AFORES 
  	 	
  	     LET afore = " "     
  	     FOR i = 1 TO 20
           IF afore[i] IS NULL OR afore[i] = ' ' THEN
              LET afore[i] = "?"
           END  IF
         END FOR	     
  	     
  	     LET cat.rendim_afo  = 0.0
  	     LET cat.comis_afo   = 0.0
  	     LET cat.rendim_neto = 0.0
  	   
         LET l_rendi = l_rendi CLIPPED, afore CLIPPED
         LET l_rendi = l_rendi CLIPPED, cat.rendim_afo  USING  "#&.&&","%" CLIPPED
         LET l_rendi = l_rendi CLIPPED, cat.comis_afo   USING "##&.&&","%" CLIPPED
         LET l_rendi = l_rendi CLIPPED, cat.rendim_neto USING  "#&.&&","%" CLIPPED
        
  	 END FOR 
  END IF 
  -- CPL-2243 
  
  FOR i = 1 TO 1014
    IF l_rendi[i] = '?' THEN
       LET l_rendi[i] = ' '
    END IF
  END FOR
  
  LET det.rendimientos =  l_rendi

  #CPL-1191 INI
  ## Se calcula el promedio simple
  LET l_avg_simple = 0

  SELECT ROUND(AVG(a.rendimiento_neto),2) INTO l_avg_simple
    FROM tab_rendimiento_neto a,  tab_afore m
   WHERE reg_taa_cd_ctr_folio.fecha_liquidacion BETWEEN a.fecha_ini AND a.fecha_fin
     AND a.siefore_cod =  vsiefore
     AND a.afore_cod   =  m.afore_cod
     AND a.afore_cod   NOT   IN(999)
     
  -- CPL-2243
  IF (vsiefore = 90) AND (l_avg_simple IS NULL) THEN  
     LET l_avg_simple = 0  	 	
  END IF 
  -- CPL-2243     

  LET det.promedio_rendi    = l_avg_simple USING "##&.&&","%"
  #CPL-1191 FIN

END FUNCTION

REPORT      R_590_imprime_archivo()
   OUTPUT
       TOP      MARGIN   0
       BOTTOM   MARGIN   0
       LEFT     MARGIN   0
       RIGHT    MARGIN   0
       PAGE     LENGTH   2
   FORMAT
        FIRST   PAGE   HEADER
                PRINT    COLUMN   01,cza.*
        ON EVERY ROW
                PRINT    COLUMN   01,det.*
END REPORT
