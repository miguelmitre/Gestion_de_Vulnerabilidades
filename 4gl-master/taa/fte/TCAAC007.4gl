#####################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => TCAAC007                                      #
#Descripcion       => RECEPCION DE RECHAZOS DE SALDOS               #
#Version           => V 1.1  del  22 de Noviembre  2010.            #
#Por               => JOSE FRANCISCO LUGO CORNEJO                   #
#Sistema           => TCAA (CEDENTE)                                #
#####################################################################
DATABASE  safre_af    
GLOBALS
   DEFINE
            g_rechazo_saldo                        ,
            g_provisionado                         ,
            t_cza                                  ,
            t_det                                  ,
            g_cero                                 ,
            g_scta                                 ,
            g_cargado                              SMALLINT
    DEFINE 
            g_fecha_presentacion                   ,
            g_hoy                                  DATE
    DEFINE  
            g_ejecuta                              CHAR(500) ,
            g_enter                                CHAR(001) ,
            nom_archivo                            CHAR(060) ,
            g_usuario                              CHAR(008) ,
            g_lista                                CHAR(200) ,
            g_operacion_desc                       CHAR(030) ,
            g_reporte                              CHAR(300) ,
            g_hora                                 CHAR(008) ,
            g_operacion                            CHAR(002)
    DEFINE  
            reg_folio RECORD  LIKE  safre_af:taa_cd_ctr_folio.*,
            p_tablayout       RECORD  LIKE  safre_af:tab_layout.*,
            g_seg_modulo      RECORD  LIKE  safre_af:seg_modulo.*,
            p_tabcampo        RECORD  LIKE  safre_af:tab_campo.*,
            reg_02_rch        RECORD  LIKE  safre_tmp:taa_cd_det_02_rch_sal.*,
            reg_05_rch        RECORD  LIKE  safre_tmp:taa_cd_det_05_rch_sal.*
    DEFINE
            g                                      RECORD
            afo_recep                              CHAR(03),
            tipo_traspaso                          CHAR(02),
            subcuenta                              CHAR(03),
            m_acciones                             DECIMAL(16,6),
            m_pesos                                DECIMAL(16,6)
                                               END RECORD

    DEFINE  afo_local                              RECORD
            afore_cod                              CHAR(03),
            raz_social                             CHAR(50)
                                               END RECORD
    DEFINE  g_monto_en_pesos                       DEC(12,2)
END GLOBALS

MAIN 
   OPTIONS 
            HELP     FILE    "rch_sal_status.o",
            INPUT    WRAP          ,
            PROMPT   LINE  LAST ,   
            ACCEPT   KEY   CONTROL-I
            DEFER    INTERRUPT
   CALL     f_010_inicio()
   CALL     f_100_proceso()
   CALL     f_300_fin()
END MAIN

FUNCTION    f_010_inicio()
   DEFINE   cadena                      CHAR(200)
   CALL     STARTLOG("TCAAC007.log")
   LET      g_hora                      =  TIME
   LET      t_cza                       =  1 
   LET      t_det                       =  2 
   LET      g_cero                      =  0
   LET      g_hoy                       =  TODAY
   DELETE   FROM     safre_tmp:taa_cd_cza_rch_sal;
   DELETE   FROM     safre_tmp:taa_cd_det_02_rch_sal;
   DELETE   FROM     safre_tmp:taa_cd_det_05_rch_sal;
   SELECT   user
     INTO   g_usuario
     FROM   safre_af:tab_afore_local ;
   SELECT   a.* 
     INTO   g_seg_modulo.*
     FROM   safre_af:seg_modulo a
    WHERE   a.modulo_cod             =  "taa";
   SELECT   b.*
     INTO   p_tablayout.*
     FROM   safre_af:tab_layout b
    WHERE   b.layout_cod             =  31000
      AND   b.modulo_cod             =  "taa";
   SELECT   a.estado
     INTO   g_provisionado
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion            = 'PROVISIONADA'
      AND   a.tipo                   =  3;
   SELECT   a.estado
     INTO   g_rechazo_saldo
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion            = 'RECHAZO SALDO'
      AND   a.tipo                   =  3;
   CREATE   TEMP  TABLE  taa_cd_arch_rch_sal ( registro   CHAR(100))
    LET     g_ejecuta                =
            "ls ",g_seg_modulo.ruta_rescate CLIPPED,"/",
            " > ",g_seg_modulo.ruta_rescate CLIPPED,"/rch_sal"
   RUN      g_ejecuta
   LET      g_ejecuta                =  g_seg_modulo.ruta_rescate CLIPPED,
                                        "/", "rch_sal"
   LOAD     FROM   g_ejecuta  INSERT  INTO    taa_cd_arch_rch_sal
END FUNCTION

FUNCTION    f_100_proceso()
   OPEN     WINDOW  taac0071  AT  2,2  WITH  FORM  "TCAAC007" ATTRIBUTE(BORDER)
   DISPLAY  " <Esc>Ejecutar      <Ctrl-c>Salir                    ",
            "                            "   AT   1,1   ATTRIBUTE(REVERSE)
   DISPLAY  "<TCAAC007>     CARGA ARCHIVO DE RECHAZOS DE SALDOS AF-AF CEDENTE",
            "                            "   AT   3,1   ATTRIBUTE(REVERSE)
   INPUT  BY  NAME      nom_archivo    WITHOUT DEFAULTS
          AFTER FIELD    nom_archivo
          IF       nom_archivo      IS  NULL  THEN 
                   ERROR    "ARCHIVO NO EXISTE... TECLEAR",
                                    " NUEVAMENTE NOMBRE DEL ARCHIVO ..."
                   NEXT FIELD    nom_archivo
          END  IF
          SELECT   "OK"
            FROM   safre_tmp:taa_cd_arch_rch_sal 
           WHERE   registro         =  nom_archivo
          IF       STATUS           =  NOTFOUND     THEN
                   ERROR     "ARCHIVO NO EXISTE... TECLEAR",
                                     " NUEVAMENTE NOMBRE DEL ARCHIVO ......   "
                   NEXT FIELD    nom_archivo
          END IF
        ON KEY (ESC)
                  IF       nom_archivo      IS  NULL    THEN
                           ERROR "ARCHIVO  NO  EXISTE... CORRIJA  NOMBRE  " 
                           NEXT FIELD    nom_archivo
                  END IF
                  SELECT   "OK"
                    FROM   safre_tmp:taa_cd_arch_rch_sal
                   WHERE   registro              =  nom_archivo
                  IF       STATUS                =  NOTFOUND     THEN
                           ERROR    "ARCHIVO NO EXISTE... TECLEAR NOMBRE",
                                    " DE ARCHIVO VALIDO ......."
                           NEXT FIELD      nom_archivo
                  END IF
                  DISPLAY  "    Procesando Informacion.....                 ",
                           "                                     "    AT   19,1
                           ATTRIBUTE(REVERSE)
                          
                  LET      nom_archivo          =  nom_archivo CLIPPED
                  CALL     f_110_recepciona_archivo()
                  CALL     f_120_dbload_cza_det()
                  CALL     f_130_valida_archivo()
                  IF       NOT      g_cargado         THEN
                           CALL     f_140_trata_folios()
                           EXIT INPUT
                  ELSE 
                  DISPLAY  "    ARCHIVO CARGADO ANTERIORMENTE .....",
                           "......                                "   AT  19,1
                           ATTRIBUTE(REVERSE)
                           PROMPT  "TECLEE  <Enter>  Para Salir ......? "
                                    FOR     g_enter
                           EXIT PROGRAM
                  END IF
                  LET      nom_archivo          =  NULL
                  CLEAR    FORM       
                  EXIT     INPUT
        ON KEY(CONTROL-C) 
                  PROMPT   " PROCESO CANCELADO...<ENTER> PARA SALIR ?" 
                           FOR     g_enter
                  EXIT PROGRAM
        ON KEY(INTERRUPT) 
                  PROMPT   " PROCESO CANCELADO...<ENTER> PARA SALIR ?"
                           FOR     g_enter
                  EXIT PROGRAM
   END INPUT
   DISPLAY   "    FIN DE REPORTES   ................                    ",
             "                                               "   AT   19,1
   PROMPT   "PROCESO FINALIZADO...TECLEE <ENTER> PARA  SALIR ?"   FOR  g_enter
END FUNCTION

FUNCTION    f_110_recepciona_archivo()
   DEFINE   g_ejecuta                    CHAR(200)
   LET      g_ejecuta                   =
            "sed -e '/^01/!d' ",g_seg_modulo.ruta_rescate    CLIPPED,
            "/",nom_archivo CLIPPED," >",g_seg_modulo.ruta_rescate   CLIPPED,
            "/",p_tablayout.arch_cza
   RUN      g_ejecuta  
   LET      g_ejecuta                   =
            "sed -e '/^02/!d' ", g_seg_modulo.ruta_rescate CLIPPED   ,
            "/",nom_archivo CLIPPED," >",g_seg_modulo.ruta_rescate  CLIPPED ,
            "/",p_tablayout.arch_det
   RUN      g_ejecuta  
   LET      p_tablayout.arch_det        =  "rch_sal_05"
   LET      g_ejecuta                   =
            "sed -e '/^05/!d' ",g_seg_modulo.ruta_rescate   CLIPPED,
            "/",nom_archivo  CLIPPED," >", g_seg_modulo.ruta_rescate  CLIPPED,
            "/",p_tablayout.arch_det
   RUN      g_ejecuta  
END FUNCTION

FUNCTION    f_120_dbload_cza_det()
   CALL     f_125_salida(t_cza,p_tablayout.arch_cza,p_tablayout.tab_cza)
   LET      g_ejecuta                  =
            "cd ",g_seg_modulo.ruta_rescate    CLIPPED,
            "/;dbload -d safre_tmp -c ",p_tablayout.nom_arch,
            " -l " CLIPPED," ",p_tablayout.arch_cza CLIPPED,
            ".err" CLIPPED, " -e 100 -n 1000 -k  > rch_sal_status.log"
   RUN      g_ejecuta
   LET      p_tablayout.arch_det        =  "rch_sal_02"
   CALL     f_125_salida(t_det,p_tablayout.arch_det,p_tablayout.tab_det)
   LET      g_ejecuta                  =
            "cd ",g_seg_modulo.ruta_rescate    CLIPPED,               
            "/;dbload -d safre_tmp -c ",p_tablayout.nom_arch,
            " -l " CLIPPED," ",p_tablayout.arch_det CLIPPED,
            ".err" CLIPPED," -e 100 -n 1000 -k >> rch_sal_status.log  "
   RUN      g_ejecuta
   LET      t_det                      =  5
   LET      p_tablayout.arch_det       =  "rch_sal_05"
   LET      p_tablayout.tab_det        =  "taa_cd_det_05_rch_sal"
   CALL     f_125_salida(t_det,p_tablayout.arch_det,p_tablayout.tab_det)
   LET      g_ejecuta                  =
            "cd ",g_seg_modulo.ruta_rescate    CLIPPED,               
            "/;dbload -d safre_tmp -c ",p_tablayout.nom_arch,
            " -l " CLIPPED," ",p_tablayout.arch_det   CLIPPED,
            ".err" CLIPPED," -e 100 -n 1000 -k >> rch_sal_status.log  "
   RUN      g_ejecuta
END FUNCTION

FUNCTION    f_125_salida(v_tipo_reg,v_nom_arch,v_tabla)
   DEFINE   v_tabla                        CHAR(30),
            v_tipo_reg                     SMALLINT,
            l_nom_archivo_dat              ,  
            l_nom_archivo_cmd              ,
            v_nom_arch                     CHAR(60)
   LET      l_nom_archivo_cmd              =
            g_seg_modulo.ruta_rescate   CLIPPED, "/",  p_tablayout.nom_arch
   LET      l_nom_archivo_dat              =
            g_seg_modulo.ruta_rescate   CLIPPED, "/",  v_nom_arch
    START REPORT    r_report_db         TO       l_nom_archivo_cmd
    DISPLAY   "    VALIDANDO  ARCHIVO  .................."  AT  16,5
    OUTPUT    TO   REPORT  r_report_db(l_nom_archivo_dat , 
                                       p_tablayout.layout_cod,
                                       v_tipo_reg,        #cza, det, sum
                                       v_tabla            #tabla a insertar
                                       )
    FINISH  REPORT   r_report_db
END FUNCTION

FUNCTION    f_130_valida_archivo()
   DEFINE   l_reg_cza                     CHAR(500)
   DEFINE   l_consec_lote_dia             SMALLINT,
            l_folio                       INTEGER,
            l_nss                         CHAR(11),
            l_curp                        CHAR(18)
   CREATE   TEMP  TABLE    taa_cd_load_rch_sal (l_reg_cza       CHAR(500))
   LET      g_ejecuta                =
            g_seg_modulo.ruta_rescate CLIPPED, "/", p_tablayout.arch_cza
   LOAD     FROM     g_ejecuta    INSERT  INTO  taa_cd_load_rch_sal
   SELECT   *
     INTO   l_reg_cza
     FROM   taa_cd_load_rch_sal
   IF       l_reg_cza[03,04]            <> "02"     THEN
            ERROR    "  IDENTIFICADOR  DE  SERVICIO  INVALIDO     "
            SLEEP    4
            EXIT     PROGRAM
   END IF
   LET      g_operacion                  =  l_reg_cza[05,06]
   IF       g_operacion                 <>  "09"   AND
            g_operacion                 <>  "12"   THEN
            PROMPT   "   OPERACION  INVALIDA  TECLEE <Enter> PARA SAIR "
                     FOR     g_enter
            EXIT     PROGRAM
   END IF
   LET      g_fecha_presentacion            = 
            MDY(l_reg_cza[24,25],l_reg_cza[26,27],l_reg_cza[20,23])
   LET      l_consec_lote_dia               =  l_reg_cza[28,30]
   IF       g_operacion               =  "09"    THEN
              SELECT   MAX(nss)      INTO   l_nss
                FROM   safre_tmp:taa_cd_det_02_rch_sal;
              SELECT  MAX( folio )        INTO  l_folio
                FROM   safre_af:taa_cd_det_cedido 
               WHERE   n_seguro         =  l_nss
                 AND   fecha_trasp      >  TODAY;
             IF        l_folio         IS  NULL   OR
                       l_folio          =  0      THEN
                       SELECT   MAX(curp)     INTO  l_curp
                         FROM   safre_tmp:taa_cd_det_02_rch_sal;
                       SELECT   MAX( folio )         INTO  l_folio
                         FROM   safre_af:taa_cd_det_cedido
                        WHERE   n_unico          =  l_curp
                          AND   fecha_trasp      >  TODAY;
             END  IF
   ELSE
   IF       g_operacion               =  "12"    THEN
            SELECT   MAX(folio)
              INTO   l_folio
              FROM   taa_cd_ctr_folio
             WHERE   tipo_traspaso           =  2
               AND   fecha_envio_saldos      =  g_fecha_presentacion;
   END IF
   END IF
   SELECT   *
     INTO   reg_folio.*
     FROM   taa_cd_ctr_folio
    WHERE   estado                          =  102
      AND   folio                           =  l_folio;
   IF       reg_folio.folio      =  0   THEN
            ERROR    " NO ENCUENTRA FOLIO PARA ESTE ARCHIVO.........."
            PROMPT   " TECLEE  ENTER  PARA  SALIR  ..........."  FOR  g_enter
            EXIT  PROGRAM
   END IF
   LET      g_cargado                       =  0
   SELECT   COUNT(*)     INTO   g_cargado
     FROM   safre_af:taa_cd_cza_rch_sal A
    WHERE   A.folio                         =  reg_folio.folio
   IF       g_cargado                       >  0   THEN
            ERROR    " ARCHIVO CARGADO ANTERIORMENTE ...          "
            PROMPT   " TECLEE  ENTER  PARA  SALIR  ..........."  FOR  g_enter
            EXIT  PROGRAM
   END IF
   DROP     TABLE   taa_cd_load_rch_sal 
END FUNCTION

FUNCTION    f_140_trata_folios()
   DEFINE   l_tipo_traspaso                 ,
            l_ha_cargado                    ,
            l_num_rechazos                  SMALLINT
   LET      l_ha_cargado                    =  0
   SELECT   COUNT(*)        INTO  l_num_rechazos
     FROM   safre_tmp:taa_cd_det_02_rch_sal
   DISPLAY  g_hoy    USING   "DD-MM-YYYY"  AT  3,65 ATTRIBUTE(REVERSE)
   IF       reg_folio.tipo_traspaso   =  1   THEN
                     DISPLAY  "PROMOTOR "     TO   FORMONLY.tipo_traspaso
   ELSE
   IF       reg_folio.tipo_traspaso   =  2   THEN
                     DISPLAY  "COMPLEMENTARIO"   TO   FORMONLY.tipo_traspaso
   ELSE
   IF       reg_folio.tipo_traspaso   =  3   THEN
                     DISPLAY  "INTERNET"   TO   FORMONLY.tipo_traspaso
   ELSE
   IF       reg_folio.tipo_traspaso   =  4   THEN
                     DISPLAY  "DIVERSOS"   TO   FORMONLY.tipo_traspaso
   END IF
   END IF
   END IF
   END IF
   DISPLAY  BY  NAME  reg_folio.fecha_presentacion
   DISPLAY  BY  NAME  reg_folio.fecha_envio_saldos
   DISPLAY  BY  NAME  reg_folio.fecha_liquidacion
   DISPLAY  BY  NAME  reg_folio.folio 
   DISPLAY   "               REGISTROS A PROCESAR  :",
             l_num_rechazos   USING  "##,###", "       " AT  16,5
   DISPLAY  "                         PROCESADOS  :",
                     "                                          " AT  17,5
   WHILE  TRUE
          PROMPT    "  VERIFIQUE FOLIO,DESEA CONTINUAR CON LA CARGA DE RECHAZOS",
                               "  <S/N> ?   "   FOR   g_enter
          IF        g_enter  MATCHES      "[sSnN]"       THEN
                    IF       g_enter        MATCHES  "[sS]"    THEN
                             LET      l_ha_cargado       =  1
                             EXIT     WHILE
                    ELSE
                    IF       NOT      l_ha_cargado    THEN
                             EXIT     PROGRAM
                    ELSE
                             EXIT     WHILE
                    END IF
                    END  IF
            END  IF
    END  WHILE
    ERROR    "  PROCESANDO  INFORMACION ...... "
    IF       l_num_rechazos         >  0      THEN
             CALL     f_150_sube_historico()
             CALL     f_160_imprime_reportes()
             LET      l_num_rechazos          =  0
    END IF
END FUNCTION

FUNCTION    f_150_sube_historico()
   DEFINE   l_sql                            CHAR(1500)
   DEFINE   l_rechazados                     SMALLINT,
            l_folio                          INTEGER
   LET      l_rechazados                     =  0
   LET      l_sql                            =
           'INSERT   INTO    safre_af:taa_cd_cza_rch_sal '   ,   
           'SELECT  ',reg_folio.folio ,  ', ' ,
                    'A.tipo_registro  ,     ' ,
                    'A.ident_servicio ,     ' ,
                    'A.ident_operacion ,    ' ,
                    'A.tipo_ent_origen ,    ' ,
                    'A.cve_ent_origen ,    ' ,
                    'A.tipo_ent_destino ,   ' ,
                    'A.cve_ent_destino ,    ' ,
                    'A.ent_fed_envio_lote,  ' ,
                    'MDY(A.fecha_presentacion[5,6],' ,
                    '    A.fecha_presentacion[7,8],' ,
                    '    A.fecha_presentacion[1,4]) , ',
                    'A.consec_lote_dia,     ' ,
                    'A.cve_mod_recepcion,   ' ,
                    'A.cod_result_operac,   ' ,
                    'A.motivo_rechazo       ' ,
           'FROM     safre_tmp:taa_cd_cza_rch_sal A '    
   LET      l_sql                 =  l_sql   CLIPPED
   PREPARE  qry1     FROM     l_sql
   EXECUTE  qry1
   DECLARE  cur_reg_02   CURSOR  FOR 
   SELECT   *
     FROM   safre_tmp:taa_cd_det_02_rch_sal  b
    ORDER   BY  b.nss
   FOREACH  cur_reg_02     INTO  reg_02_rch.* 
      IF     reg_02_rch.tipo_traspaso        =  "73"    OR
             reg_02_rch.tipo_traspaso        =  "83"    OR
             reg_02_rch.tipo_traspaso        =  "84"    OR
             reg_02_rch.tipo_traspaso        =  "85"    THEN
             IF    reg_folio.tipo_traspaso         =  1    OR
                   reg_folio.tipo_traspaso         =  4    THEN
                   SELECT   n_seguro
                     INTO   reg_02_rch.nss
                     FROM   safre_af:taa_cd_det_cedido
                    WHERE   folio                  =  reg_folio.folio
                      AND   n_unico                =  reg_02_rch.curp
                      AND   estado                 =  102;
             ELSE
                   SELECT   n_seguro
                     INTO   reg_02_rch.nss
                     FROM   safre_af:taa_cd_det_cedido
                    WHERE   estado                 =  12 
                      AND   n_unico                =  reg_02_rch.curp;
             END IF
      END IF
            
      INSERT   INTO     safre_af:taa_cd_det_02_rch_sal
               VALUES   (reg_folio.folio,
                         reg_02_rch.tipo_registro  ,
                         reg_02_rch.cont_servicio  ,
                         reg_02_rch.tipo_recep_cuenta,
                         reg_02_rch.cve_recep_cuenta,
                         reg_02_rch.tipo_ced_cuenta , 
                         reg_02_rch.cve_ced_cuenta  , 
                         reg_02_rch.tipo_traspaso   , 
                         g_hoy                     ,
                         reg_02_rch.curp             ,
                         reg_02_rch.nss              ,
                         NULL                        ,
                         reg_02_rch.paterno          ,
                         reg_02_rch.materno          ,
                         reg_02_rch.nombre           ,
                         NULL                        ,
                         NULL                        ,
                         NULL                        ,
                         NULL                        ,
                         NULL                        ,
                         NULL                        ,
                         NULL                        ,
                         NULL                        ,
                         NULL                        ,
                         NULL                        ,
                         NULL                        ,
                         NULL                        ,
                         reg_02_rch.cod_result_operac,
                         reg_02_rch.diag_proceso     ,
                         NULL                        ,
                         NULL                        );
      IF       reg_folio.tipo_traspaso     =  1    OR  
               reg_folio.tipo_traspaso     =  4    THEN
               CALL     f_152_desmarcar()
               UPDATE   safre_af:taa_cd_det_cedido
                  SET   estado              =  g_rechazo_saldo
                WHERE   n_seguro            =  reg_02_rch.nss
                  AND   estado              =  102
      ELSE
               UPDATE   safre_af:taa_cd_det_cedido
                  SET   estado              =  103
                WHERE   n_seguro            =  reg_02_rch.nss
                  AND   estado              =  12;
               UPDATE   safre_af:taa_cd_det_comple
                  SET   estado              =  106
                WHERE   folio               =  reg_folio.folio
                  AND   n_seguro            =  reg_02_rch.nss
      END IF
      LET      l_rechazados                =  l_rechazados  +  1
            DISPLAY  "                         PROCESADOS  :",
                     l_rechazados      USING  "##,###", "       "   AT  17,5
   END FOREACH
   DECLARE  cur_det_05        CURSOR  FOR
   SELECT   *
     FROM   safre_tmp:taa_cd_det_05_rch_sal
    ORDER   BY  nss
   FOREACH  cur_det_05          INTO  reg_05_rch.* 
      IF     reg_05_rch.tipo_traspaso        =  "73"    OR
             reg_05_rch.tipo_traspaso        =  "83"    OR
             reg_05_rch.tipo_traspaso        =  "84"    OR
             reg_05_rch.tipo_traspaso        =  "85"    THEN
             IF    reg_folio.tipo_traspaso         =  1    OR
                   reg_folio.tipo_traspaso         =  4    THEN
                   SELECT   n_seguro
                     INTO   reg_05_rch.nss
                     FROM   safre_af:taa_cd_det_cedido
                    WHERE   folio                  =  reg_folio.folio
                      AND   n_unico                =  reg_05_rch.curp
                      AND   estado                IN(102)
             ELSE
                   SELECT   n_seguro
                     INTO   reg_05_rch.nss
                     FROM   safre_af:taa_cd_det_cedido
                    WHERE   estado                IN(12)
                      AND   n_unico                =  reg_05_rch.curp
             END IF
      END IF
      INSERT   INTO     safre_af:taa_cd_det_05_rch_sal
               VALUES   (reg_folio.folio,
                         reg_05_rch.tipo_registro  ,
                         reg_05_rch.cont_servicio  ,
                         reg_05_rch.tipo_recep_cuenta,
                         reg_05_rch.cve_recep_cuenta,
                         reg_05_rch.tipo_ced_cuenta , 
                         reg_05_rch.cve_ced_cuenta  , 
                         reg_05_rch.tipo_traspaso   , 
                         NULL                        ,
                         NULL                        ,
                         reg_05_rch.curp             ,
                         reg_05_rch.nss              ,
                         NULL                        ,
                         reg_05_rch.cod_result_operac,
                         reg_05_rch.diag_proceso     ,
                         NULL                        ,
                         NULL                        
                         );
   END FOREACH
   UPDATE   safre_af:dis_provision
      SET   estado             =  106
     WHERE  folio              =  reg_folio.folio
       AND  nss       IN(SELECT  nss  FROM  safre_tmp:taa_cd_det_05_rch_sal)
END FUNCTION 

FUNCTION    f_152_desmarcar()        
   DEFINE   l_marca_entra                      ,
            l_estado_marca                     SMALLINT,
            l_folio                            INTEGER
   LET      l_estado_marca           =  30  #Desmarca y Habilita con HIstoria
   SELECT   a.marca_cod
     INTO   l_marca_entra
     FROM   safre_af:taa_cd_tipo_traspaso a
    WHERE   a.tipo_traspaso          =  reg_02_rch.tipo_traspaso;

   SELECT   MAX(f.folio)
     INTO   l_folio
    FROM    safre_af:taa_cd_det_cedido  f
    WHERE   f.n_seguro               =  reg_02_rch.nss
      AND   f.estado                 =  102
   LET      g_ejecuta                =
              "EXECUTE PROCEDURE safre_af:desmarca_cuenta(?,?,?,?,?,?)"
   LET      g_ejecuta                =  g_ejecuta    CLIPPED
   PREPARE  des_rch_sal  FROM   g_ejecuta
   EXECUTE  des_rch_sal  USING  reg_02_rch.nss          ,
                                l_marca_entra           ,
                                l_folio                 ,
                                l_estado_marca          , #Desmarca y Habilita 
                                l_marca_entra           ,
                                g_usuario
END FUNCTION

FUNCTION    f_160_imprime_reportes()
   LET      g_hora                       =  TIME
   IF       reg_folio.tipo_traspaso          =  1  THEN
            LET      g_operacion_desc     =  "PROMOTOR  "
   ELSE
   IF       reg_folio.tipo_traspaso          =  2  THEN
            LET      g_operacion_desc     =  "COMPLEMENTARIO"
   ELSE
   IF       reg_folio.tipo_traspaso          =  4  THEN
            LET      g_operacion_desc     =  "DIVERSOS"
   END IF
   END IF
   END IF
   LET      g_reporte                  =
            g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
            ".RCH_SAL_CED.",g_hoy   USING  "DDMMYY", "_",g_hora CLIPPED
   START REPORT  r_162_imprime_reporte_1     TO  g_reporte
   DISPLAY   "    GENERANDO  REPORTES .................."  AT  18,5
   CALL     f_162_genera_reporte_1()
   LET      g_lista                    =  "chmod 777 ",g_reporte CLIPPED
   RUN      g_lista
   LET      g_lista                    =  "lp ",g_reporte
   RUN      g_lista
   LET      g_reporte                  = 
            g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
            ".RCH_SAL_CED2.",g_hoy USING "DDMMYY", "_",g_hora CLIPPED
   START REPORT  r_164_imprime_reporte_2    TO  g_reporte
   DISPLAY   "     GENERANDO  REPORTES .................."  AT  18,5
   CALL     f_164_genera_reporte_2()
   LET      g_lista                    =  "chmod 777 ",g_reporte CLIPPED
   RUN      g_lista
   LET      g_lista                    =  "lp ",g_reporte
   RUN      g_lista
   LET      g_reporte                  =
            g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
            ".RCH_SAL_CED3.",g_hoy USING "DDMMYY", "_",g_hora CLIPPED
   START REPORT    r_166_imprime_reporte_3  TO g_reporte
   DISPLAY   "     GENERANDO  REPORTES .................."  AT  18,5
   CALL     f_166_genera_reporte_3()
   LET      g_lista                    =  "chmod 777 ",g_reporte CLIPPED
   RUN      g_lista
   LET      g_lista                    =  "lp ",g_reporte
   RUN      g_lista
END FUNCTION

FUNCTION    f_162_genera_reporte_1()
   DECLARE  cur_afo       CURSOR  FOR
   SELECT   a.cve_recep_cuenta,a.tipo_traspaso,
            b.subcuenta,SUM(b.monto_en_acciones),SUM(b.monto_en_pesos)
     FROM   safre_af:taa_cd_det_02_rch_sal  a , safre_af:dis_provision  b
    WHERE   a.folio                =  reg_folio.folio
      AND   a.folio                =  b.folio
      AND   a.nss                  =  b.nss
      AND   b.tipo_movimiento         BETWEEN  200   AND  299
    GROUP   BY   1,2,3   ORDER    BY  1,2,3;
   FOREACH  cur_afo             INTO  g.*
            OUTPUT           TO   REPORT    r_162_imprime_reporte_1(g.*)
   END FOREACH
   FINISH  REPORT   r_162_imprime_reporte_1
   ERROR   "GENERANDO LISTADO 1 ..."             
END FUNCTION

REPORT      r_162_imprime_reporte_1(rpte)
   DEFINE   rpte                         RECORD
            afo_recep                    CHAR(03),
            tipo_traspaso                CHAR(02),
            subcuenta                    CHAR(02),
            m_acciones                   DECIMAL(16,6),
            m_pesos                      DECIMAL(16,6)
                                    END RECORD
   DEFINE   l_nom_afore                  CHAR(20)
   DEFINE
            l_tot_acciones               ,
            l_tot_pesos                  DECIMAL(16,6)
   DEFINE   l_num_regs                   ,
            l_tipo_trasp                 ,
            l_tot_regs                   SMALLINT
   OUTPUT
      TOP      MARGIN   1
      BOTTOM   MARGIN   0
      LEFT     MARGIN   0
      RIGHT    MARGIN   0
      PAGE     LENGTH   60
    FORMAT
      PAGE HEADER
        PRINT    COLUMN   69,"Pagina:",PAGENO USING "<<<<"
        PRINT    COLUMN   02,"TCAAC007",
                 COLUMN   14,"RECHAZOS DE SALDOS DE TRASPASOS",
                          " AFORE(CEDENTE) FECHA:",
                 COLUMN   67, g_hoy USING "mm-dd-yyyy"
        PRINT    COLUMN   02,"________________________________________________",
                             "_______________________________________"
        PRINT    COLUMN   02,afo_local.afore_cod,"     ",afo_local.raz_social
        PRINT    COLUMN   02,"FOLIO",reg_folio.folio ,
                          "  ",g_operacion_desc
        PRINT    COLUMN   02,"________________________________________________",
                             "_______________________________________"
        PRINT    COLUMN   02,"AFO_RECEP",
                 COLUMN   12,"TIPO_TRA",
                 COLUMN   22,"SUBCUENTA",
                 COLUMN   37,"MONTO EN ACCIONES",
                 COLUMN   64,"MONTO EN PESOS"
        PRINT    COLUMN   02,"________________________________________________",
                             "_______________________________________"
   ON EVERY ROW
        PRINT    COLUMN    2,rpte.afo_recep         ,
                 COLUMN   12,rpte.tipo_traspaso     ,
                 COLUMN   21,rpte.subcuenta         USING  "##",
                 COLUMN   36,rpte.m_acciones        USING  "---,---,--&.######",
                 COLUMN   62,rpte.m_pesos           USING  "---,---,--&.######"
        IF       lineno                  >  57    THEN
                 SKIP   TO   TOP   OF  PAGE
        END IF
        AFTER    GROUP  OF  rpte.afo_recep
        SELECT   afore_desc
          INTO   l_nom_afore
          FROM   safre_af:tab_afore
         WHERE   afore_cod         =  rpte.afo_recep
        PRINT    COLUMN   02,"________________________________________________",
                             "_______________________________________"
        PRINT    COLUMN   2,"TOTAL ",
                 COLUMN   8,l_nom_afore,
                 COLUMN   29,"==>",
                 COLUMN   36,GROUP  SUM(rpte.m_acciones)
                          USING     "---,---,--&.######",
                 COLUMN   62,GROUP  SUM(rpte.m_pesos)
                          USING     "---,---,--&.######"
        PRINT    COLUMN   02,"________________________________________________",
                             "_______________________________________"
        PRINT
        AFTER GROUP  OF rpte.tipo_traspaso
        PRINT    COLUMN   02,"________________________________________________",
                             "_______________________________________"
        PRINT    COLUMN   02,"TOTAL POR TIPO DE TRASPASO ==>",
                 COLUMN   36,GROUP SUM(rpte.m_acciones)
                           USING   "---,---,--&.######",
                 COLUMN   62,GROUP SUM(rpte.m_pesos) 
                           USING   "---,---,--&.######"
        PRINT
   ON  LAST  ROW
        PRINT
        PRINT    COLUMN   02,"________________________________________________",
                             "_______________________________________"
        PRINT    COLUMN 02,"TOTALES  POR  SUBCUENTA" 
        LET      l_tot_acciones           =  0
        LET      l_tot_pesos              =  0
        DECLARE  cur_cta       CURSOR  FOR
        SELECT   b.subcuenta,SUM(b.monto_en_acciones),SUM(b.monto_en_pesos)
          FROM   safre_af:taa_cd_det_05_rch_sal a , safre_af:dis_provision  b
         WHERE   a.folio                  =  reg_folio.folio
           AND   a.folio                  =  b.folio
           AND   a.nss                    =  b.nss
           AND   b.tipo_movimiento           BETWEEN   200   AND   299
         GROUP   BY    1        ORDER  BY  1;
       FOREACH   cur_cta        INTO  g.subcuenta,g.m_acciones,g.m_pesos
                 PRINT     COLUMN 20,g.subcuenta    USING  "##",
                           COLUMN 36,g.m_acciones   USING  "---,---,--&.######",
                           COLUMN 62,g.m_pesos      USING  "---,---,--&.######"
                 LET       l_tot_acciones           =
                           l_tot_acciones           +  g.m_acciones
                 LET       l_tot_pesos              =
                           l_tot_pesos              +  g.m_pesos
       END FOREACH
        PRINT    COLUMN 02,"_________________________________________________",
                             "_______________________________________"
        PRINT    COLUMN 02,"TOTALES  POR  TIPO DE TRASPASO Y SUBCUENTA" 
        DECLARE  cur_tip_sub      CURSOR  FOR
         SELECT  a.tipo_traspaso,b.subcuenta,
                 SUM(b.monto_en_acciones),SUM(b.monto_en_pesos)
           FROM  safre_af:taa_cd_det_05_rch_sal a , safre_af:dis_provision b
          WHERE  a.folio                  =  reg_folio.folio
            AND  a.folio                  =  b.folio
            AND  a.nss                    =  b.nss
            AND  b.tipo_movimiento      BETWEEN   200  AND  299
          GROUP  BY   1,2   ORDER  BY  1,2;
        FOREACH  cur_tip_sub    INTO  l_tipo_trasp,g.subcuenta,
                                      g.m_acciones,g.m_pesos
                 IF       g.m_acciones        IS  NULL   THEN
                          LET      g.m_acciones           =  0  
                          LET      g.m_pesos              =  0  
                 END IF
                 PRINT    COLUMN   12,l_tipo_trasp  ,
                          COLUMN   21,g.subcuenta   USING  "##",
                          COLUMN   36,g.m_acciones  USING  "---,---,--&.######",
                          COLUMN   62,g.m_pesos     USING  "---,---,--&.######"
       END FOREACH
        PRINT    COLUMN   02,"_______________________________________________",
                             "_______________________________________"
        LET      l_num_regs                  = 0 
        LET      l_tot_regs                  = 0 
        PRINT    COLUMN   02,"TOTALES  POR  TIPO DE TRASPASO" 
        DECLARE  cur_tipo      CURSOR  FOR
        SELECT   a.tipo_traspaso,COUNT(UNIQUE a.nss),
                 SUM(b.monto_en_acciones),SUM(b.monto_en_pesos)
          FROM   safre_af:taa_cd_det_05_rch_sal a ,
                 OUTER   safre_af:dis_provision b
         WHERE   a.folio                =  reg_folio.folio
           AND   a.folio                =  b.folio
           AND   a.nss                  =  b.nss
           AND   b.tipo_movimiento      BETWEEN  200    AND  299
         GROUP   BY   1   ORDER  BY  1;
       FOREACH   cur_tipo     INTO  l_tipo_trasp,l_num_regs,
                                    g.m_acciones,g.m_pesos
                 IF       g.m_acciones             IS  NULL   THEN
                          LET       g.m_acciones          =  0  
                          LET       g.m_pesos             =  0  
                 END IF
                 PRINT    COLUMN    20,l_tipo_trasp          ,
                          COLUMN    24,l_num_regs    USING "##### REGS.",
                          COLUMN    36,g.m_acciones  USING "---,---,--&.######",
                          COLUMN    62,g.m_pesos     USING "---,---,--&.######"
                 LET      l_tot_regs          =  l_tot_regs   +  l_num_regs
      END FOREACH
       PRINT    COLUMN   02,"________________________________________________",
                            "_______________________________________"
       PRINT    COLUMN   02,"TOTAL GLOBAL " ,
                COLUMN   15,"==>",
                COLUMN   19,l_tot_regs            USING  "##### REGS.",
                COLUMN   36,l_tot_acciones        USING  "---,---,--&.######",
                COLUMN   62,l_tot_pesos           USING  "---,---,--&.######"
END REPORT

FUNCTION    f_164_genera_reporte_2()
    DEFINE
            glocal                               RECORD
            afo_recep                              CHAR(03),
            tipo_traspaso                          CHAR(02),
            motivo_rechazo                         CHAR(03),
            m_acciones                             DECIMAL(16,6),
            m_pesos                                DECIMAL(16,6)
                                               END RECORD
   DECLARE  cur_afo2                CURSOR  FOR
   SELECT   a.cve_recep_cuenta,a.tipo_traspaso,
            a.diag_proceso[1,3],SUM(b.monto_en_acciones),SUM(b.monto_en_pesos)
     FROM   safre_af:taa_cd_det_02_rch_sal a , safre_af:dis_provision b
    WHERE   a.folio             =  reg_folio.folio
      AND   a.folio             =  b.folio
      AND   a.nss               =  b.nss
      AND   b.tipo_movimiento      BETWEEN   200   AND   299
    GROUP   BY   1,2,3      ORDER  BY  1,2,3;
   FOREACH  cur_afo2         INTO  glocal.*
            OUTPUT     TO  REPORT  r_164_imprime_reporte_2(glocal.*)
   END FOREACH
   FINISH   REPORT  r_164_imprime_reporte_2
   ERROR    "GENERANDO LISTADO 2..."             
   SLEEP    2
   ERROR    ""
END FUNCTION

REPORT      r_164_imprime_reporte_2(rpte)
   DEFINE   rpte                              RECORD
            afo_recep                       CHAR(03),
            tipo_traspaso                   CHAR(02),
            motivo_rechazo                  CHAR(03),
            m_acciones                      DECIMAL(16,6),
            m_pesos                         DECIMAL(16,6)
                                          END RECORD
   DEFINE   l_nom_afore                     CHAR(20)
   DEFINE   l_tot_acciones                  ,
            l_tot_pesos                     DECIMAL(16,6)
   DEFINE   l_motivo_rechazo                CHAR(03)
   DEFINE   l_num_regs                      ,
            l_tipo_trasp                    ,
            l_tot_regs                      SMALLINT
   OUTPUT
      TOP      MARGIN   1
      BOTTOM   MARGIN   0
      LEFT     MARGIN   0
      RIGHT    MARGIN   0
      PAGE     LENGTH   60
   FORMAT
     PAGE HEADER
        PRINT    COLUMN   69,"Pagina:",PAGENO USING "<<<<"
        PRINT    COLUMN   02,"TCAAC007",
                 COLUMN   14,"RECHAZOS DE SALDOS DE TRASPASOS AFORE(CEDENTE)",
                          " FECHA:",
                 COLUMN   67, g_hoy USING "mm-dd-yyyy"
        PRINT    COLUMN   02,"_______________________________________________",
                            "_______________________________________"
        PRINT    COLUMN   02,afo_local.afore_cod,"     ",afo_local.raz_social
        PRINT    COLUMN   02,"FOLIO",reg_folio.folio ,"  ",
                          g_operacion_desc
        PRINT    COLUMN   02,"_______________________________________________",
                            "_______________________________________"
        PRINT    COLUMN   02,"AFO_RECEP",
                 COLUMN   12,"TIPO_TRA",
                 COLUMN   22,"MOTI_RECHAZO",
                 COLUMN   37,"MONTO EN ACCIONES",
                 COLUMN   64,"MONTO EN PESOS"
        PRINT    COLUMN   02,"_______________________________________________",
                            "_______________________________________"
   ON EVERY ROW
        PRINT    COLUMN    2,rpte.afo_recep          ,
                 COLUMN   12,rpte.tipo_traspaso      ,
                 COLUMN   22,rpte.motivo_rechazo     ,
                 COLUMN   36,rpte.m_acciones         USING "---,---,--&.######",
                 COLUMN   62,rpte.m_pesos            USING "---,---,--&.######"
        IF       lineno                    >  57     THEN
                 SKIP   TO  TOP  OF  PAGE
        END IF
        AFTER    GROUP  OF    rpte.afo_recep
                 SELECT   afore_desc
                   INTO   l_nom_afore
                   FROM   safre_af:tab_afore
                  WHERE   afore_cod           =  rpte.afo_recep
        PRINT    COLUMN   02,"_______________________________________________",
                            "_______________________________________"
        PRINT    COLUMN    2,"TOTAL ",
                 COLUMN    8,l_nom_afore,
                 COLUMN   29,"==>",
                 COLUMN   36,GROUP SUM(rpte.m_acciones)
                          USING "---,---,--&.######",
                 COLUMN   62,GROUP SUM(rpte.m_pesos)
                          USING "---,---,--&.######"
        PRINT    COLUMN   02,"_______________________________________________",
                            "_______________________________________"
        PRINT
        AFTER    GROUP  OF rpte.tipo_traspaso
        PRINT    COLUMN   02,"_______________________________________________",
                            "_______________________________________"
        PRINT    COLUMN    2,"TOTAL POR TIPO DE TRASPASO ==>",
                 COLUMN   36,GROUP SUM(rpte.m_acciones)
                             USING   "---,---,--&.######",
                 COLUMN   62,GROUP SUM(rpte.m_pesos)   
                             USING   "---,---,--&.######"
        PRINT
  ON  LAST  ROW
        PRINT
        PRINT    COLUMN   02,"_______________________________________________",
                            "_______________________________________"
        PRINT    COLUMN   02,"TOTALES  POR  MOTIVO DE RECHAZO" 
        LET      l_tot_acciones          =  0
        LET      l_tot_pesos             =  0
        DECLARE  cur_cta2      CURSOR  FOR
        SELECT   a.diag_proceso[1,3],SUM(b.monto_en_acciones),
                 SUM(b.monto_en_pesos)
          FROM   safre_af:taa_cd_det_05_rch_sal a , safre_af:dis_provision  b
         WHERE   a.folio                 =  reg_folio.folio
           AND   a.folio                 =  b.folio
           AND   a.nss                   =  b.nss
           AND   b.tipo_movimiento    BETWEEN  200   AND  299
         GROUP   BY   1   ORDER  BY  1;
        FOREACH  cur_cta2     INTO  l_motivo_rechazo,g.m_acciones,g.m_pesos
                 PRINT    COLUMN   20,l_motivo_rechazo,
                          COLUMN   36,g.m_acciones   USING "---,---,--&.######",
                          COLUMN   62,g.m_pesos      USING "---,---,--&.######"
                 LET      l_tot_acciones     =  l_tot_acciones  +  g.m_acciones
                 LET      l_tot_pesos        =  l_tot_pesos     +  g.m_pesos
        END FOREACH
        PRINT    COLUMN   02,"_______________________________________________",
                             "_______________________________________"
        PRINT    COLUMN   02,"TOTALES POR TIPO DE TRASPASO Y MOTIVO DE RECHAZO" 
        DECLARE  cur_tip_sub2     CURSOR  FOR
        SELECT   a.tipo_traspaso,a.diag_proceso[1,3],COUNT(unique a.nss),
                 SUM(b.monto_en_acciones),SUM(b.monto_en_pesos)
          FROM   safre_af:taa_cd_det_05_rch_sal  a ,
                 OUTER   safre_af:dis_provision  b
         WHERE   a.folio               =  reg_folio.folio
           AND   a.folio               =  b.folio
           AND   a.nss                 =  b.nss
           AND   b.tipo_movimiento     BETWEEN  200   AND  299
         GROUP   BY   1,2       ORDER  BY  1,2;
        FOREACH  cur_tip_sub2    INTO  l_tipo_trasp,l_motivo_rechazo,
                                       l_num_regs,g.m_acciones,g.m_pesos
                 IF       g.m_acciones        IS    NULL     THEN
                          LET      g.m_acciones              =  0  
                          LET      g.m_pesos                 =  0  
                 END IF
                 PRINT    COLUMN   12,l_tipo_trasp USING  "##",
                          COLUMN   20,l_motivo_rechazo, 
                          COLUMN   25,l_num_regs   USING "##### REGS.",
                          COLUMN   36,g.m_acciones USING "---,---,--&.######",
                          COLUMN   62,g.m_pesos    USING "---,---,--&.######"
        END FOREACH
        PRINT    COLUMN   02,"_______________________________________________",
                             "_______________________________________"
        LET      l_num_regs                 =  0 
        LET      l_tot_regs                 =  0 
        PRINT    COLUMN   02,"TOTALES  POR  TIPO DE TRASPASO" 
        DECLARE  cur_tipo2     CURSOR  FOR
        SELECT   a.tipo_traspaso,COUNT(UNIQUE  a.nss),
                 SUM(b.monto_en_acciones),SUM(b.monto_en_pesos)
          FROM   safre_af:taa_cd_det_05_rch_sal  a ,
                 OUTER   safre_af:dis_provision  b
         WHERE   b.folio                    =  reg_folio.folio
           AND   a.folio                    =  b.folio
           AND   a.nss                      =  b.nss
           AND   b.tipo_movimiento  BETWEEN  200   AND  299
         GROUP   BY   1   ORDER  BY  1;
        FOREACH  cur_tipo2  INTO l_tipo_trasp,l_num_regs,g.m_acciones,g.m_pesos
                 IF       g.m_acciones      IS  NULL   THEN
                          LET      g.m_acciones          =  0  
                          LET      g.m_pesos             =  0  
                 END IF
                 PRINT    COLUMN   20,l_tipo_trasp  USING "##",
                          COLUMN   24,l_num_regs    USING "##### REGS.",
                          COLUMN   36,g.m_acciones  USING "---,---,--&.######",
                          COLUMN   62,g.m_pesos     USING "---,---,--&.######"
                 LET      l_tot_regs       =  l_tot_regs    +  l_num_regs
        END FOREACH
        PRINT    COLUMN   02,"_______________________________________________",
                             "________________________________________"
        PRINT
                 COLUMN   02,"TOTAL GLOBAL " ,
                 COLUMN   15,"==>",
                 COLUMN   19,l_tot_regs       USING  "##### REGS.",
                 COLUMN   36,l_tot_acciones   USING  "---,---,--&.######",
                 COLUMN   62,l_tot_pesos      USING  "---,---,--&.######"
END REPORT

FUNCTION    f_166_genera_reporte_3()
   DEFINE 
            l_reg                         RECORD
            motivo_rechazo                CHAR(03),
            nss                           CHAR(11)
                                      END RECORD
   DECLARE  cur_afo3       CURSOR  FOR
    SELECT  a.diag_proceso[1,3],a.nss     
      FROM  safre_af:taa_cd_det_05_rch_sal  a
     WHERE  a.folio                    =  reg_folio.folio
   FOREACH  cur_afo3       INTO  l_reg.*
            OUTPUT     TO   REPORT   r_166_imprime_reporte_3(l_reg.*)
   END FOREACH
   FINISH   REPORT  r_166_imprime_reporte_3
   ERROR    "GENERANDO LISTADO 3..."             
   SLEEP    2
   ERROR    ""
END FUNCTION

REPORT      r_166_imprime_reporte_3(rpte)
   DEFINE   rpte                            RECORD
            motivo_rechazo                  CHAR(03),
            nss                             CHAR(11)    
                                        END RECORD
   DEFINE   l_desc_motivo_rechazo           CHAR(60)
   DEFINE   l_scta                          SMALLINT
   DEFINE   l_acciones                      ,
            l_acciones_tot                  ,
            l_pesos                         ,
            l_pesos_tot                     DEC(16,6)
   OUTPUT
      TOP     MARGIN   1
      BOTTOM  MARGIN   0
      LEFT    MARGIN   0
      RIGHT   MARGIN   0
      PAGE    LENGTH   60
      ORDER   BY  rpte.motivo_rechazo,rpte.nss
   FORMAT
      PAGE HEADER
        PRINT    COLUMN   69,"Pagina:",PAGENO USING "<<<<"
        PRINT    COLUMN   02,"TCAAC007",
                 COLUMN   14,"RECHAZOS DE SALDOS DE TRASPASOS AFORE(CEDENTE)",
                           " FECHA:",
                 COLUMN   67, g_hoy USING "mm-dd-yyyy"
        PRINT    COLUMN   02,"_______________________________________________",
                             "_______________________________________"

        PRINT    COLUMN   02,afo_local.afore_cod,"     ",afo_local.raz_social
        PRINT    COLUMN   02,"FOLIO",reg_folio.folio ,
                 "  ",g_operacion_desc
        PRINT    COLUMN   02,"_______________________________________________",
                             "_______________________________________"
   BEFORE  GROUP  OF   rpte.motivo_rechazo
        LET      l_desc_motivo_rechazo     =  NULL
        SELECT   rdeta_desc_c
          INTO   l_desc_motivo_rechazo
          FROM   safre_af:tab_rdeta
         WHERE   modulo_cod                =  "taa"
           AND   rdeta_cod                 =  rpte.motivo_rechazo   
           AND   tipo_rechazo              =  "C"
        IF       l_desc_motivo_rechazo    IS  NULL    THEN
                 LET     l_desc_motivo_rechazo    =  " NO  EXISTE  MOTIVO  "
        END IF  
        PRINT    COLUMN   02,"MOTIVO DE RECHAZO : "," ",
                 rpte.motivo_rechazo      CLIPPED ,"  ",
                 l_desc_motivo_rechazo    CLIPPED
        PRINT    COLUMN   02,"_______________________________________________",
                             "_______________________________________"
        PRINT    COLUMN   02,"NSS" 
   AFTER GROUP OF  rpte.nss      
        PRINT COLUMN  2,rpte.nss
   AFTER GROUP OF  rpte.motivo_rechazo      
        PRINT    COLUMN   02,"_______________________________________________",
                             "_______________________________________"
        PRINT    COLUMN   02,"SUB-TOTAL MOTIVO DE RECHAZO : ",
                 rpte.motivo_rechazo      USING  "##&"," ",
                 l_desc_motivo_rechazo    CLIPPED,
                 GROUP COUNT(*)  USING   "##,##& REGS."
        PRINT    COLUMN   02,"_______________________________________________",
                             "_______________________________________"
        IF       lineno        >  57  THEN
                 SKIP TO TOP OF PAGE
        END IF
   ON  LAST  ROW
        PRINT
        PRINT    COLUMN   02,"_______________________________________________",
                             "_______________________________________"
        PRINT    COLUMN   02,"TOTAL  RECHAZADOS  " ,
                 COLUMN   15,"==>",
                 COLUMN   19, COUNT(*) USING "##,### REGS."
        PRINT    COLUMN   02,"SCTA.                     ACCIONES         ",
                             "             PESOS "
        LET      l_acciones_tot               =  0
        LET      l_pesos_tot                  =  0
        DECLARE  c_rech_sal     CURSOR   FOR  
        SELECT   b.subcuenta,SUM(b.monto_en_acciones),SUM(b.monto_en_pesos)
          FROM   safre_af:taa_cd_det_05_rch_sal a , safre_af:dis_provision  b
         WHERE   a.folio                  =  reg_folio.folio
           AND   a.folio                  =  b.folio
           AND   a.nss                    =  b.nss
           AND   b.tipo_movimiento           BETWEEN   200   AND   299
         GROUP   BY    1        ORDER  BY  1;
       FOREACH  c_rech_sal        INTO   l_scta,l_acciones,l_pesos 
                PRINT    COLUMN   02,l_scta      USING   "##",
                         COLUMN   17,l_acciones  USING   "####,###,###.######",
                         COLUMN   44,l_pesos     USING   "####,###,###.######" 
                LET      l_acciones_tot    =  l_acciones_tot   +  l_acciones  
                LET      l_pesos_tot       =  l_pesos_tot      +  l_pesos
       END FOREACH
       PRINT    COLUMN   02,"________________________________________________",
                             "_______________________________________"
       PRINT    COLUMN   02,"TOTAL :" ,
                COLUMN   17,l_acciones_tot       USING   "####,###,###.######",
                COLUMN   44,l_pesos_tot          USING   "####,###,###.######" 
END REPORT


FUNCTION    f_300_fin()
   LET      g_ejecuta              =
            "echo \'chmod  777 ",
            "rch_sal_*", "\' > arch_paso;" , "sh arch_paso 1>>err_paso 2>&1"
   RUN      g_ejecuta
   LET      g_ejecuta              =  "rm  arch_paso "
   RUN      g_ejecuta
   LET      g_ejecuta              =  "rm  err_paso "
   RUN      g_ejecuta 
END FUNCTION
