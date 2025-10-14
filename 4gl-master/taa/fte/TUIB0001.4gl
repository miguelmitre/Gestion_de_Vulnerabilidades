#####################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => TUIB0001                                      #
#Descripcion       => RECEPCION DE OPERACIÓN 09 UNIFICACIÓN         #
#Autor             => JOSE FRANCISCO LUGO CORNEJO                   #
#Sistema           => TUI( TRASPASO POR UNIFICACIÓN ISSSTE IMSSS    #
#Fecha Creacion    => 20  OCTUBRE  2010        .                    #
#REQ:     709      => Juan de la cruz P.V.     22/06/2011.          #
#####################################################################
DATABASE  safre_af    
GLOBALS
   DEFINE
            g_estado                               ,
            ind                                    ,
            ini                                    ,
            fin                                    ,
            g_cargado                              SMALLINT
    DEFINE 
            g_fecha_presentacion                   ,
            g_today                                DATE

    DEFINE  
            g_ejecuta                              CHAR(500) ,
            g_enter                                CHAR(001) ,
            nom_archivo                            CHAR(060) ,
            g_usuario                              CHAR(008) ,
            g_lista                                CHAR(200) ,
            g_reporte                              CHAR(050) ,
            g_hora                                 CHAR(008),
            g_tui_cza_uni_issste                         ,
            g_tui_sdo_uni_issste                          ,
            g_tui_int_uni_issste                          ,
            g_tui_sum_uni_issste                  CHAR(50)
    DEFINE  
            reg_tui_ctr_folio    RECORD  LIKE  safre_af:tui_ctr_folio.*,
            g_seg_modulo         RECORD  LIKE  safre_af:seg_modulo.*,
            p_tabcampo           RECORD  LIKE  safre_af:tab_campo.*

    DEFINE  rep                  RECORD
            estado                                 SMALLINT,
            tipo_diagnostico                       SMALLINT,
            nss                                    CHAR(11),
            int_viv92                              ,
            int_viv08                              ,
            sar92                                  ,
            viv92                                  ,
            int_sar92                              ,
            viv08                                  DECIMAL(16,6)
                                 END RECORD

    DEFINE  g_folio                                ,
            g_insertados                           INTEGER,

            g_tot           ARRAY[5,50]   OF    RECORD
            pesos                               DEC(16,6)
                                          END   RECORD

END GLOBALS

MAIN 
   OPTIONS 
            HELP     FILE    "tui.o",
            INPUT    WRAP          ,
            PROMPT   LINE  LAST ,   
            ACCEPT   KEY   CONTROL-I
#           DEFER    INTERRUPT
   CALL     f_010_inicio()
   CALL     f_100_proceso()
   CALL     f_300_fin()
END MAIN

FUNCTION    f_010_inicio()
   CALL     STARTLOG("TUIB0001.log")
   LET      g_hora                       =  TIME
   LET      g_today                      =  TODAY
   LET      g_tui_cza_uni_issste         = 'tui_cza_uni_issste'
   LET      g_tui_sdo_uni_issste         = 'tui_sdo_uni_issste'
   LET      g_tui_int_uni_issste         = 'tui_int_uni_issste'
   LET      g_tui_sum_uni_issste         = 'tui_sum_uni_issste'
   DELETE   FROM     safre_tmp:tui_cza_uni_issste;
   DELETE   FROM     safre_tmp:tui_sdo_uni_issste;
   DELETE   FROM     safre_tmp:tui_int_uni_issste;
   DELETE   FROM     safre_tmp:tui_sum_uni_issste;
   SELECT   a.* ,user
     INTO   g_seg_modulo.*,g_usuario
     FROM   safre_af:seg_modulo a
    WHERE   a.modulo_cod             =  "taa";
   CREATE   TEMP  TABLE  tui_arch_uni_issste ( registro   CHAR(730))
   LET      g_ejecuta                =
            "ls ",g_seg_modulo.ruta_rescate CLIPPED,"/",
            " > ",g_seg_modulo.ruta_rescate CLIPPED,"/ls_tui"
   RUN      g_ejecuta
   LET      g_ejecuta                =  g_seg_modulo.ruta_rescate CLIPPED,
                                        "/", "ls_tui"
   LOAD     FROM   g_ejecuta  INSERT  INTO    tui_arch_uni_issste
END FUNCTION

FUNCTION    f_100_proceso()
   OPEN     WINDOW  tuib001  AT  2,2  WITH  FORM  "TUIB0001" ATTRIBUTE(BORDER)
   DISPLAY  " <Esc>Ejecutar      <Ctrl-c>Salir                    ",
            "                            "   AT   1,1   ATTRIBUTE(REVERSE)
   DISPLAY  "<<TUIB0001>>     TRASPASO  POR  UNIFICACIÓN  DE  CUENTAS  ISSSTE-IMSS              ",
            "                            "   AT   3,1   ATTRIBUTE(REVERSE)
   DISPLAY  "                        RECEPCION  DE  ARCHIVO  OPERACIÓN  09          ",
            "                            "   AT   4,1   ATTRIBUTE(REVERSE)
   INPUT  BY  NAME      nom_archivo    WITHOUT DEFAULTS
         AFTER FIELD    nom_archivo
                  IF       nom_archivo      IS  NULL  THEN 
                           ERROR    "Archivo no Valido... Teclear",
                                    " Nuevamente Nombre del Archivo ..."
                           NEXT FIELD    nom_archivo
                  END  IF
                  SELECT   "OK"
                    FROM   tui_arch_uni_issste 
                   WHERE   registro         =  nom_archivo
                  IF       STATUS           =  NOTFOUND     THEN
                           ERROR     "Archivo no Valido... Teclear",
                                     " Nuevamente Nombre del Archivo ......   "
                           NEXT FIELD    nom_archivo
                  END IF
        ON KEY (ESC)
                  IF       nom_archivo      IS  NULL    THEN
                           ERROR "Archivo  no  Existe... Corrija  Nombre  " 
                           NEXT FIELD    nom_archivo
                  END IF
                  SELECT   "OK"
                    FROM   safre_tmp:tui_arch_uni_issste
                   WHERE   registro              =  nom_archivo
                  IF       STATUS                =  NOTFOUND     THEN
                           ERROR    "Archivo no Valido... Teclear Nombre",
                                    " de Archivo Valido ......."
                           NEXT FIELD      nom_archivo
                  END IF
                  LET      nom_archivo          =  nom_archivo CLIPPED
                  CALL     f_110_recibe_archivo()
                  CALL     f_120_carga_tablas_temporales()
                  CALL     f_130_valida_archivo()
                  CALL     f_150_arma_historicos()
                  CALL     f_170_imprime_reportes()
                  EXIT  INPUT
        ON KEY(CONTROL-C) 
                  PROMPT   " Proceso Cancelado...<Enter> para Salir ?" 
                           FOR     g_enter
                  EXIT PROGRAM
        ON KEY(INTERRUPT) 
                  PROMPT   " Proceso Cancelado...<Enter> para Salir ?"
                           FOR     g_enter
                  EXIT PROGRAM
   END INPUT
   DISPLAY  "                                                ",
            "                                        "    AT   19,1
END FUNCTION

FUNCTION    f_110_recibe_archivo()
   LET      g_ejecuta                   =
            "sed -e '/^01/!d' ",g_seg_modulo.ruta_rescate    CLIPPED,
            "/",nom_archivo CLIPPED," >",g_seg_modulo.ruta_rescate   CLIPPED,
            "/",g_tui_cza_uni_issste
   RUN      g_ejecuta  
   LET      g_ejecuta                   =
            "sed -e '/^02/!d' ", g_seg_modulo.ruta_rescate CLIPPED   ,
            "/",nom_archivo CLIPPED," >",g_seg_modulo.ruta_rescate  CLIPPED ,
            "/",g_tui_sdo_uni_issste
   RUN      g_ejecuta  
   LET      g_ejecuta                   =
            "sed -e '/^03/!d' ",g_seg_modulo.ruta_rescate   CLIPPED,
            "/",nom_archivo  CLIPPED," >", g_seg_modulo.ruta_rescate  CLIPPED,
            "/",g_tui_int_uni_issste
   RUN      g_ejecuta  
   LET      g_ejecuta                   =
            "sed -e '/^09/!d' ",g_seg_modulo.ruta_rescate   CLIPPED,
            "/",nom_archivo  CLIPPED," >", g_seg_modulo.ruta_rescate  CLIPPED,
            "/",g_tui_sum_uni_issste
   RUN      g_ejecuta  
   ERROR   "    Procesando Informacion... " 
END FUNCTION

FUNCTION    f_120_carga_tablas_temporales()
   CALL     f_125_salida("1",g_tui_cza_uni_issste,g_tui_cza_uni_issste)
   LET      g_ejecuta                  =
            "cd ",g_seg_modulo.ruta_rescate    CLIPPED,
            "/;dbload -d safre_tmp -c ","insert"  CLIPPED,
            " -l " CLIPPED," ",g_tui_cza_uni_issste  CLIPPED,
            ".err" CLIPPED, " -e 100 -n 1000 -k  > tui_uni_issste.log"
   RUN      g_ejecuta

   CALL     f_125_salida("2",g_tui_sdo_uni_issste,g_tui_sdo_uni_issste)
   LET      g_ejecuta                  =
            "cd ",g_seg_modulo.ruta_rescate    CLIPPED,               
            "/;dbload -d safre_tmp -c ","insert"  CLIPPED,
            " -l " CLIPPED," ",g_tui_sdo_uni_issste   CLIPPED,
            ".err" CLIPPED," -e 100 -n 1000 -k >> tui_uni_issste.log  "
   RUN      g_ejecuta

   CALL     f_125_salida("3",g_tui_int_uni_issste,g_tui_int_uni_issste)
   LET      g_ejecuta                  =
            "cd ",g_seg_modulo.ruta_rescate    CLIPPED,               
            "/;dbload -d safre_tmp -c ","insert"  CLIPPED,
            " -l " CLIPPED," ",g_tui_int_uni_issste   CLIPPED,
            ".err" CLIPPED," -e 100 -n 1000 -k >> tui_uni_issste.log  "
   RUN      g_ejecuta

   CALL     f_125_salida("9",g_tui_sum_uni_issste,g_tui_sum_uni_issste)
   LET      g_ejecuta                  =
            "cd ",g_seg_modulo.ruta_rescate    CLIPPED,               
            "/;dbload -d safre_tmp -c ","insert"  CLIPPED,
            " -l " CLIPPED," ",g_tui_sum_uni_issste  CLIPPED,
            ".err" CLIPPED," -e 100 -n 1000 -k >> tui_uni_issste.log  "
   RUN      g_ejecuta
END FUNCTION

FUNCTION    f_125_salida(v_tipo_reg,v_nom_arch,v_tabla)
   DEFINE   v_tabla                        CHAR(30),
            v_tipo_reg                     SMALLINT,
            l_nom_archivo_dat              ,  
            l_nom_archivo_cmd              ,
            v_nom_arch                     CHAR(60),
            l_layout_cod                   SMALLINT
   LET      l_layout_cod                   =  48 
   LET      l_nom_archivo_cmd              =
            g_seg_modulo.ruta_rescate   CLIPPED, "/insert"
   LET      l_nom_archivo_dat              =
            g_seg_modulo.ruta_rescate   CLIPPED, "/",  v_nom_arch
    START   REPORT    r_report_db         TO       l_nom_archivo_cmd
    OUTPUT    TO   REPORT  r_report_db(l_nom_archivo_dat , 
                                       l_layout_cod,
                                       v_tipo_reg,        #cza, det, sum
                                       v_tabla            #tabla a insertar
                                       )
    FINISH  REPORT   r_report_db
END FUNCTION

REPORT   r_report_db(v_arch, v_codigo, v_reg, v_tabla )
   DEFINE   v_arch, v_tabla,  v_campo     CHAR(110),
            v_reg,  v_codigo, v_ban,  i,j SMALLINT
   DEFINE   p_tabcampo        RECORD LIKE   safre_af:tab_campo.*
     OUTPUT
        PAGE   LENGTH 1
        LEFT   MARGIN 0
        RIGHT  MARGIN 0
        TOP    MARGIN 0
        BOTTOM MARGIN 0

     FORMAT
        ON EVERY ROW

           PRINT COLUMN 1, "FILE ", "\"",v_arch CLIPPED , "\"  ("

           DECLARE  cur_db   CURSOR  FOR
                   SELECT  a.*  FROM  safre_af:tab_campo a
                    WHERE  a.layout_cod           =  v_codigo
                      AND  a.tipo_reg             =  v_reg
                    ORDER  BY 3
                   LET     v_ban                  =  false
           FOREACH  cur_db   INTO  p_tabcampo.*
                   IF      v_ban      THEN
                           PRINT   COLUMN   32, ","
                   END IF
                   PRINT COLUMN 1,  p_tabcampo.campo_desc CLIPPED;
                   PRINT COLUMN 20, p_tabcampo.pos_ini CLIPPED,
                                      "-" CLIPPED,
                                    p_tabcampo.pos_fin USING "&&&&" CLIPPED ;
                    LET     v_ban     =  TRUE
           END FOREACH
           PRINT COLUMN 32 , ");";
           PRINT
           PRINT
           PRINT COLUMN 1,"INSERT INTO  ", v_tabla  CLIPPED, ";"
END REPORT

FUNCTION    f_130_valida_archivo()
   DEFINE   l_ha_cargado                     ,
            l_hay_error                      ,
            l_consec_lote_dia                ,
            l_count                          ,
            l_num_reg                        DEC(10)
   DEFINE   l_curp                           CHAR(18)
   DEFINE   l_reg_cza                        CHAR(500)
   DEFINE   l_ident_operacion                CHAR(02)
   CREATE   TEMP  TABLE    tui_load_uni_issste (l_reg_cza       CHAR(500))
   LET      g_ejecuta                =
            g_seg_modulo.ruta_rescate CLIPPED, "/", g_tui_cza_uni_issste
   LOAD     FROM     g_ejecuta    INSERT  INTO  tui_load_uni_issste
   SELECT   *
     INTO   l_reg_cza
     FROM   tui_load_uni_issste
   IF       l_reg_cza[03,04]            <> "02"     THEN
            PROMPT   "Archivo Invalido Favor de Verificarlo  <Enter Sair> "
                     FOR     g_enter
            EXIT     PROGRAM
   END IF
   IF       l_reg_cza[05,06]            <> "09"     THEN
            PROMPT   "Archivo Invalido Favor de Verificarlo  <Enter Sair> "
                     FOR     g_enter
            EXIT     PROGRAM
   END IF
   LET      g_fecha_presentacion            = 
            MDY(l_reg_cza[24,25],l_reg_cza[26,27],l_reg_cza[20,23])
   DISPLAY  g_fecha_presentacion           TO  fecha_presentacion
   LET      l_consec_lote_dia               =  l_reg_cza[28,30]
   LET      g_cargado                       =  0
   SELECT   COUNT(*)     INTO   g_cargado
     FROM   safre_af:tui_cza_uni_issste  A
    WHERE   A.fecha_presentacion            =  g_fecha_presentacion
      AND   A.consecutivo_lote              =  l_consec_lote_dia
   IF       g_cargado            THEN
            DISPLAY  "  Archivo Cargado Anteriormente Favor de Verificar...",
                     "                               "   AT  19,1
                     ATTRIBUTE(REVERSE)
                     PROMPT  "Teclee  <Enter>  Para Salir ......? "
                             FOR     g_enter
                     EXIT PROGRAM
   END IF
   LET      l_hay_error                     =  0
   LET      l_num_reg                       =  0
   SELECT   COUNT(*)
     INTO   l_count    
     FROM   safre_tmp:tui_sdo_uni_issste;
   LET      l_num_reg                       =  l_count
   SELECT   COUNT(*)
     INTO   l_count
     FROM   safre_tmp:tui_int_uni_issste;
   LET      l_num_reg                       =  l_num_reg   +  l_count
   DISPLAY  g_today    USING   "DD-MM-YYYY"  AT  1,65 ATTRIBUTE(REVERSE)
   DISPLAY  "    Registros de Detalle a Procesar     :",
            l_num_reg       USING  "####,##&", "       " AT  13,3
   IF       NOT       l_num_reg             THEN
            ERROR   "  NO  Hay  Registros  a  Procesar   Teclee <Enter> para Salir  ......    "
            PROMPT  "  Teclee  <Enter>  para  Salir   ......  ?                                      "
            FOR     g_enter
            EXIT    PROGRAM
   END IF
   WHILE  TRUE
            PROMPT    "  Desea Continuar con la Carga de Uinificación Op. 09",
                      "  <S/N> ?   "   FOR   g_enter
                     IF        g_enter  MATCHES      "[sSnN]"       THEN
                               IF       g_enter        MATCHES  "[sS]"    THEN
                                        EXIT     WHILE
                               ELSE
                                        EXIT     PROGRAM
                               END  IF
                     END  IF
   END  WHILE
   INSERT   INTO     safre_af:glo_folio   VALUES(0);
   SELECT   MAX(folio)
     INTO   g_folio
     FROM   safre_af:glo_folio;
   IF       g_folio       IS  NULL   THEN
            LET      g_folio         =  0
   END IF
   DISPLAY  g_folio           TO  folio
   DROP     TABLE    tui_load_uni_issste ;
END FUNCTION

FUNCTION    f_150_arma_historicos()
   DATABASE       safre_tmp
   UPDATE   safre_tmp:tui_sdo_uni_issste
      SET   fecha_recep_soli    =  "00010101"
    WHERE   fecha_recep_soli    =  "        ";
   UPDATE   STATISTICS     FOR  TABLE   tui_sdo_uni_issste;
   UPDATE   STATISTICS     FOR  TABLE   tui_int_uni_issste;
   DATABASE       safre_af;
   CALL     f_155_arma_tui_ctr_folio()
   CALL     f_160_arma_tui_cza_uni_issste()
   CALL     f_170_arma_tui_sdo_uni_issste()
   CALL     f_180_arma_tui_int_uni_issste()
   CALL     f_210_arma_tui_sum_uni_issste()
   UPDATE   STATISTICS     FOR  TABLE   tui_sdo_uni_issste;
   UPDATE   STATISTICS     FOR  TABLE   tui_int_uni_issste;
END FUNCTION

FUNCTION  f_155_arma_tui_ctr_folio()
    DEFINE  l_dia_var                   ,
            l_dos_dias_uno              ,
            l_mes_envio                 ,
            l_mes_liqui                 ,
            l_dia_semana                SMALLINT
    DEFINE
            l_fecha_var                 DATE
    LET     l_dia_var           =  0
    LET     l_fecha_var         =   g_fecha_presentacion  + 1  UNITS  MONTH
    LET     l_fecha_var         =  MDY( MONTH (l_fecha_var),
                                   "01",YEAR(l_fecha_var))
    
    WHILE   TRUE
            LET      l_dia_semana           =  WEEKDAY(l_fecha_var)
            IF       l_dia_semana           >  0  AND
                     l_dia_semana           <  6  THEN
                 SELECT  "ok"
                   FROM   safre_af:tab_feriado
                  WHERE   feria_fecha          =  l_fecha_var
                 IF       STATUS               =  NOTFOUND     THEN
                            IF       l_dia_var        =  1     THEN
                                     EXIT  WHILE
                            END IF
                            LET      l_dia_var        =  l_dia_var  +  1
                 END IF
            END IF
            LET      l_fecha_var        =  l_fecha_var     +  1
    END WHILE
    LET     reg_tui_ctr_folio.folio               =  g_folio
    LET     reg_tui_ctr_folio.archivo             =  nom_archivo
    LET     reg_tui_ctr_folio.fecha_presentacion  =  g_fecha_presentacion
    LET     reg_tui_ctr_folio.fecha_provision     =  l_fecha_var
    LET     reg_tui_ctr_folio.fecha_liquidacion   =  l_fecha_var
    LET     reg_tui_ctr_folio.estado              =  101
    LET     reg_tui_ctr_folio.usuario             =  g_usuario
    INSERT  INTO  safre_af:tui_ctr_folio
            VALUES (reg_tui_ctr_folio.*)
END FUNCTION

FUNCTION    f_160_arma_tui_cza_uni_issste()
   DEFINE   l_sql                            CHAR(1500)
   ERROR    "  PROCESANDO  INFORMACION ...... "
   LET      g_insertados                     =  0
   LET      l_sql                            =
           'INSERT   INTO    safre_af:tui_cza_uni_issste  ',   
           'SELECT  ',g_folio           ,',',
                    'A.tipo_registro                   ,',
                    'A.ident_servicio                  ,',
                    'A.ident_operacion                 ,',
                    'A.tipo_ent_origen                 ,',
                    'A.tipo_ent_destino                ,',
                    'A.cve_ent_destino                 ,',
                    'MDY(A.fecha_presentacion[5,6]     ,',
                    '    a.fecha_presentacion[7,8]     ,',
                    '    a.fecha_presentacion[1,4])    ,', 
                    'A.consecutivo_lote                ,',
                    'A.modalidad_recep                  ',
           'FROM     safre_tmp:tui_cza_uni_issste  A  '    
   LET      l_sql                 =  l_sql   CLIPPED
   PREPARE  qry_cza            FROM  l_sql
   EXECUTE  qry_cza
END FUNCTION

FUNCTION    f_170_arma_tui_sdo_uni_issste()
   DEFINE   l_sql                            CHAR(1500)
   DEFINE   l_fecha                          DATE
   DEFINE   l_con_error                      INTEGER
   DEFINE   l_reg             RECORD   LIKE    safre_af:tui_sdo_uni_issste.*
   SELECT   COUNT(*)
     FROM   safre_tmp:tui_sdo_uni_issste
    WHERE  (fecha_recep_soli       IS  NULL
      OR    fecha_recep_soli        =  "          ")
      OR   (fecha_presentacion     IS  NULL
      OR    fecha_presentacion      =  "          ")
      OR   (fecha_banxico          IS  NULL
      OR    fecha_banxico           =  "          ")
   IF       l_con_error           THEN
            ERROR   "  VERIFIQUE  ARCHIVO CONTIENE REGISTROS CON FECHA ERRONEA..."
            PROMPT  "  TECLEE  <<Enter>>  para Salir...      "  for  g_enter
            DELETE    FROM  tui_ctr_folio
             WHERE    folio               =  g_folio;
            DELETE    FROM  tui_cza_uni_issste
             WHERE    folio               =  g_folio;
            EXIT    PROGRAM
   END IF
   LET      g_insertados               =  0
   DISPLAY  "    Total Registros Grabados de Detalle :"
                     AT 15,03  ATTRIBUTE(REVERSE)
   LET      l_sql                      =
           'SELECT  ',g_folio           ,',',
                    'B.tipo_registro                 ,',
                    'B.cont_servicio                 ,',
                    'B.tipo_ent_recep_cta            ,',
                    'B.cve_ent_recep_cta             ,',
                    'B.tipo_ent_ced_cta              ,',
                    'B.cve_ent_ced_cta               ,',
                    'B.tipo_diagnostico              ,',
                    'MDY(B.fecha_presentacion[05,06]  ,',
                    '    B.fecha_presentacion[07,08]  ,',
                    '    B.fecha_presentacion[01,04]) ,',
                    'MDY(B.fecha_banxico[05,06]       ,',
                    '    B.fecha_banxico[07,08]       ,',
                    '    B.fecha_banxico[01,04])      ,',
                    'B.curp                          ,',
                    'B.nss                           ,',
                    'B.paterno                       ,',
                    'B.materno                       ,',
                    'B.nombres                       ,',
                    'B.tipo_unificacion              ,',
                    'MDY(B.fecha_recep_soli[05,06]  ,',
                    '    B.fecha_recep_soli[07,08]  ,',
                    '    B.fecha_recep_soli[01,04]) ,',
                    'B.int_viv92     /  1000000      ,',
                    'B.int_viv08     /  1000000      ,',
                    'B.sar92         /  100          ,',
                    'B.viv92         /  100          ,',
                    'B.viv08         /  100          ,',
                    '101                              ',
             'FROM     safre_tmp:tui_sdo_uni_issste  B  '
   LET      l_sql                 =  l_sql   CLIPPED
   PREPARE  qry_saldo         FROM     l_sql
   DECLARE  cur_saldos       CURSOR FOR    qry_saldo
   FOREACH  cur_saldos         INTO     l_reg.*
            INSERT   INTO      tui_sdo_uni_issste
                     VALUES  (l_reg.*)
            LET      g_insertados           =  g_insertados  +  1
            DISPLAY  g_insertados      USING  "###,###"     AT 15,45
   END FOREACH
   UPDATE   tui_sdo_uni_issste
      SET   estado            =  114
     WHERE  folio             =  g_folio
       AND  nss          NOT IN(SELECT   n_seguro  FROM  afi_mae_afiliado);
END FUNCTION 

FUNCTION    f_180_arma_tui_int_uni_issste()
   DEFINE   l_sql                            CHAR(1500)
   DEFINE   l_fecha                          DATE
   DEFINE   l_i                              SMALLINT
   DEFINE   l_reg             RECORD   LIKE    safre_af:tui_int_uni_issste.*
   LET      l_sql                      =
            'SELECT  ',g_folio           ,',',
                     'C.tipo_registro                     ,',
                     'C.cont_servicio                     ,',
                     'C.tipo_ent_recep_cta                ,',
                     'C.cve_ent_recep_cta                 ,',
                     'C.tipo_ent_ced_cta                  ,',
                     'C.cve_ent_ced_cta                   ,',
                     'C.tipo_diagnostico                  ,',
                     'MDY(C.fecha_presentacion[05,06]     ,',
                     '    C.fecha_presentacion[07,08]     ,',
                     '    C.fecha_presentacion[01,04])    ,',
                     'MDY(C.fecha_banxico[05,06]          ,',
                     '    C.fecha_banxico[07,08]          ,',
                     '    C.fecha_banxico[01,04])         ,',
                     'C.curp                              ,',
                     'C.nss                               ,',
                     'C.paterno                           ,',
                     'C.materno                           ,',
                     'C.nombres                           ,',
                     'C.int_sar92      /  100             ,',
                     '101                                  ',
             'FROM     safre_tmp:tui_int_uni_issste   C    '
   LET      l_sql                 =  l_sql   CLIPPED
   PREPARE  qry_interes       FROM     l_sql
   DECLARE  cur_interes      CURSOR FOR qry_interes
   FOREACH  cur_interes        INTO     l_reg.*
            INSERT   INTO      tui_int_uni_issste
                     VALUES  (l_reg.*)
            LET      g_insertados           =  g_insertados  +  1
            DISPLAY  g_insertados      USING  "###,###"     AT 15,45
   END FOREACH
   UPDATE   tui_int_uni_issste
      SET   estado            =  106
     WHERE  folio             =  g_folio
       AND  nss          NOT IN(SELECT   n_seguro  FROM  afi_mae_afiliado);
END FUNCTION 

FUNCTION    f_210_arma_tui_sum_uni_issste()
   DEFINE   l_sql                            CHAR(1500)
   LET      l_sql                            =
           'INSERT   INTO    safre_af:tui_sum_uni_issste  ',   
           'SELECT  ',g_folio           ,',',
                    'E.tipo_registro     ,',
                    'E.num_registros     ,',
                    'E.sar92_80              /  100          ,',
                    'E.viv92_80              /  100          ,',
                    'E.viv08_80              /  100          ,',
                    'E.int_viv92_80          /  1000000      ,',
                    'E.int_viv08_80          /  1000000      ,',
                    --'E.int_sar08_80          /  1000000      ,',   #709
                    '0                                       ,',     #709
                    'E.sar92_81              /  100          ,',
                    'E.viv92_81              /  100          ,',
                    'E.viv08_81              /  100          ,',
                    'E.int_viv92_81          /  1000000      ,',
                    'E.int_viv08_81          /  1000000      ,',
                    'E.int_sar08_81          /  1000000      ,',
                    --'E.sar92_82              /  100          ,',   #709
                    '0                                       ,',     #709
                    'E.viv92_82              /  100          ,',
                    'E.viv08_82              /  100          ,',
                    'E.int_viv92_82          /  1000000      ,',
                    --'E.int_sar08_82          /  100          ,',   #709
                    '0                                       ,',     #709
                    'E.int_viv08_82          /  1000000      ,',
                    'E.sar92_91              /  100          ,',     #709
                    'E.viv92_91              /  100          ,',     #709
                    'E.viv08_91              /  100          ,',     #709
                    'E.intviv92_91           /  1000000      ,',     #709
                    'E.intviv08_91           /  1000000       ',     #709
           'FROM     safre_tmp:tui_sum_uni_issste         E  '    
   LET      l_sql                 =  l_sql   CLIPPED
   PREPARE  qry_sumario     FROM     l_sql
   EXECUTE  qry_sumario
END FUNCTION 

FUNCTION    f_170_imprime_reportes()
   DEFINE   l_rfc_patron                     CHAR(13),
            l_subcuenta                      SMALLINT,
            l_des_estado                     CHAR(10),
            l_pesos                          DEC(16,6)

   LET      g_reporte                    =            
            g_seg_modulo.ruta_listados   CLIPPED,"/",g_usuario   CLIPPED,
            ".TUIB0001.",g_folio    USING   "&&&&&&"
   FOR      ind              =  1         TO  5
            FOR      l_subcuenta      =  1         TO  50
                     LET      g_tot[ind,l_subcuenta].pesos         =  0
                     LET      g_tot[ind,l_subcuenta].pesos         =  0
            END  FOR
   END  FOR
   DISPLAY  "Reporte:",g_reporte         AT  17,5
   DISPLAY  "      Generando  Reportes ..................         "  AT  19,5
   START    REPORT     r_190_imprime_reporte_1     TO  g_reporte
   DECLARE  c_reporte        CURSOR   FOR
   SELECT   A.estado, A.tipo_diagnostico, A.nss, SUM(A.int_viv92),
            SUM(A.int_viv08), SUM(A.sar92), SUM(A.viv92), SUM(A.viv08),
            SUM(B.int_sar92)
     FROM   tui_sdo_uni_issste   A,tui_int_uni_issste   B
    WHERE   A.folio               =  g_folio
      AND   B.folio               =  g_folio
      AND   A.nss                 =  B.nss
      AND   A.tipo_diagnostico    =  B.tipo_diagnostico
    GROUP   BY   1,2,3
    ORDER   BY   1,2,3
   FOREACH   c_reporte    INTO  rep.*
           OUTPUT   TO   REPORT    r_190_imprime_reporte_1(rep.*)
   END FOREACH
   FINISH  REPORT   r_190_imprime_reporte_1
   LET      g_lista                    =  "chmod   777 ",g_reporte CLIPPED
   RUN      g_lista
   LET      g_lista                    =  "lp ",g_reporte
#  RUN      g_lista
   ERROR   "                                                         "
END FUNCTION
  
REPORT      r_190_imprime_reporte_1(s)
 DEFINE  s                  RECORD
            estado                                 SMALLINT,
            tipo_diagnostico                       SMALLINT,
            nss                                    CHAR(11),
            int_viv92                              ,
            int_viv08                              ,
            sar92                                  ,
            viv92                                  ,
            int_sar92                              ,
            viv08                                  DECIMAL(16,6)
                            END    RECORD
   
   DEFINE     l_estado                    CHAR(11)
   OUTPUT
      TOP      MARGIN   1
      BOTTOM   MARGIN   0
      LEFT     MARGIN   0
      RIGHT    MARGIN   0
      PAGE     LENGTH   60

    ORDER BY   s.estado
    FORMAT
      PAGE HEADER
    LET       l_estado          =  "Procedentes"
    IF       s.estado           =  106     THEN
             LET       l_estado          =  "Rechazos"    
    END IF
        PRINT    COLUMN   100,"PAGINA:",PAGENO USING "<<<<"
        PRINT    COLUMN   02,"<<TUIB0001>>",
                 COLUMN   24,"TRASPASOS UNIFICACIÓN ISSSTE-IMSS CARGA OPERACIÓN  09",
                 COLUMN   100,"FECHA:", g_today USING   "dd-mm-yyyy"
        PRINT    COLUMN   02,"________________________________________________",
"____________________________________________________________________________________"

        PRINT    COLUMN   02,"FOLIO:",g_folio USING  "########"  CLIPPED,"  Estado:",s.estado   USING "###"," ",l_estado,"    IMPORTES EN PESOS, ARCHIVO : ",g_reporte
        PRINT    COLUMN   02,"_________________________________________________",
"___________________________________________________________________________________"
        PRINT    COLUMN   02,"DIAG. Nss             Interes_Viv_92     Interes_Viv_98       Saldo Sar_92        Saldo Viv_92      Saldo_ Viv_08     Interes Sar_92"
        PRINT    COLUMN   02,"________________________________________________",
"____________________________________________________________________________________"


   ON EVERY ROW
        PRINT    COLUMN    2,s.tipo_diagnostico   USING  "##",       "    ",
                             s.nss,                                  
                             s.int_viv92          USING  "####,###,##&.######",
                             s.int_viv08          USING  "####,###,##&.######",
                             s.sar92              USING  "####,###,##&.######",
                             s.viv92              USING  "####,###,##&.######",
                             s.viv08              USING  "####,###,##&.######",
                             s.int_sar92          USING  "####,###,##&.######" 
 
       AFTER GROUP  OF s.estado
        PRINT    COLUMN   02,"________________________________________________",
"____________________________________________________________________________________"

    PRINT  COLUMN  2,"TOTAL ESTADO:",s.estado     USING  "#&&&",
                      GROUP SUM(rep.int_viv92)    USING  "####,###,##&.######",
                      GROUP SUM(rep.int_viv08)    USING  "####,###,##&.######",
                      GROUP SUM(rep.sar92)        USING  "####,###,##&.######",
                      GROUP SUM(rep.viv92)        USING  "####,###,##&.######",
                      GROUP SUM(rep.viv08)        USING  "####,###,##&.######",
                      GROUP SUM(rep.int_sar92)    USING  "####,###,##&.######"
         SKIP 60 LINES
END REPORT
 
FUNCTION    f_300_fin()
   LET      g_ejecuta              =
            "cd ",g_seg_modulo.ruta_rescate    CLIPPED,
            "/;echo \'chmod  777 ",
            "tui*", "\' > arch_paso;" , "sh arch_paso 1>>err_paso 2>&1"
   RUN      g_ejecuta
   LET      g_ejecuta              = 
            "cd ",g_seg_modulo.ruta_rescate    CLIPPED,
            "/;rm  arch_paso "
   RUN      g_ejecuta
   LET      g_ejecuta              =
            "cd ",g_seg_modulo.ruta_rescate    CLIPPED,
            "/;rm  err_paso "
   RUN      g_ejecuta 
   PROMPT   "Proceso Finalizado...Teclee <Enter> para  Salir ?..."  FOR  g_enter
   EXIT     PROGRAM
END FUNCTION
