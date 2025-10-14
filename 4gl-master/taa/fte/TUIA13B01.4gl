###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.                                                  #
#Autor             => JOSE FRANCISCO LUGO CORNEJO                             #
#Sistema           => Unif. Intra-Afore Cutas ACTIVAS  Diferente CURP         #
#                     Traspasos de saldos Oper.13                             #
#Programa          => TUIA13B01 (CARGA  OP.13)                                #
#Fecha Creacion    => 25/Ago/2011                                             #
#REQUERIMIENTO:    => PST-455                 25/Ago/2011                     #
###############################################################################
#Modificacion      => PST-613 3-Mayo-12 Se agrega Actualizacion del tipo de   #
#                  => Trabajador en afi_cta_peisste   --> Alejandro Chagoya S #
###############################################################################
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
            g_n_seguro                             CHAR(11),
            g_tui13_cza_noti_fin_uni                         ,
            g_tui13_det_noti_fin_uni                          ,
            g_tui13_sum_noti_fin_uni                  CHAR(50)
    DEFINE
            reg_tui13_ctr_folio    RECORD  LIKE  safre_af:tui13_ctr_folio.*,
            g_seg_modulo         RECORD  LIKE  safre_af:seg_modulo.*,
            p_tabcampo           RECORD  LIKE  safre_af:tab_campo.*
    DEFINE  g_folio                                ,
            g_insertados                           INTEGER
   DEFINE   g_det_noti    RECORD   LIKE    safre_af:tui13_det_noti_fin_uni.*
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
   CALL     STARTLOG("TUIA13B01.log")
   LET      g_hora                       =  TIME
   LET      g_today                      =  TODAY
   LET      g_tui13_cza_noti_fin_uni         = 'tui13_cza_noti_fin_uni'
   LET      g_tui13_det_noti_fin_uni         = 'tui13_det_noti_fin_uni'
   LET      g_tui13_sum_noti_fin_uni         = 'tui13_sum_noti_fin_uni'
   DELETE   FROM     safre_tmp:tui13_cza_noti_fin_uni;
   DELETE   FROM     safre_tmp:tui13_det_noti_fin_uni;
   DELETE   FROM     safre_tmp:tui13_sum_noti_fin_uni;
   SELECT   a.* ,user
     INTO   g_seg_modulo.*,g_usuario
     FROM   safre_af:seg_modulo a
    WHERE   a.modulo_cod             =  "taa";
   CREATE   TEMP  TABLE  tui13_arch_uni_issste ( registro   CHAR(730))

   #verifica que archivos existe en la ruta
   LET      g_ejecuta                =
            "ls ",g_seg_modulo.ruta_rescate CLIPPED,"/",
            " > ",g_seg_modulo.ruta_rescate CLIPPED,"/ls_tui"
   RUN      g_ejecuta
   LET      g_ejecuta                =  g_seg_modulo.ruta_rescate CLIPPED,
                                        "/", "ls_tui"
   LOAD     FROM   g_ejecuta  INSERT  INTO    tui13_arch_uni_issste
END FUNCTION

FUNCTION    f_100_proceso()
   OPEN     WINDOW  tuib001  AT  2,2  WITH  FORM  "TUIA13B01" ATTRIBUTE(BORDER)
   DISPLAY  " <Esc>Ejecutar      <Ctrl-c>Salir                    ",
            "                            "   AT   1,1   ATTRIBUTE(REVERSE)
   DISPLAY  "<<TUIA13B01>>     TRASPASO  POR  UNIFICACIÓN  DE  CUENTAS  ISSSTE-IMSS              ",
            "                            "   AT   3,1   ATTRIBUTE(REVERSE)
   DISPLAY  "                        RECEPCION  DE  ARCHIVO  OPERACIÓN  13          ",
            "                            "   AT   4,1   ATTRIBUTE(REVERSE)
   INPUT  BY  NAME      nom_archivo    WITHOUT DEFAULTS
         AFTER FIELD    nom_archivo
                  IF       nom_archivo      IS  NULL  THEN
                           ERROR    "Archivo no Valido... Teclear",
                                    " Nuevamente Nombre del Archivo ..."
                           NEXT FIELD    nom_archivo
                  END  IF
                  SELECT   "OK"
                    FROM   tui13_arch_uni_issste
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
                    FROM   safre_tmp:tui13_arch_uni_issste
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
   #Separa cabeza
   LET      g_ejecuta                   =
            "sed -e '/^01/!d' ",g_seg_modulo.ruta_rescate    CLIPPED,
            "/",nom_archivo CLIPPED," >",g_seg_modulo.ruta_rescate   CLIPPED,
            "/",g_tui13_cza_noti_fin_uni
   RUN      g_ejecuta

   #Separa detalle
   LET      g_ejecuta                   =
            "sed -e '/^02/!d' ", g_seg_modulo.ruta_rescate CLIPPED   ,
            "/",nom_archivo CLIPPED," >",g_seg_modulo.ruta_rescate  CLIPPED ,
            "/",g_tui13_det_noti_fin_uni
   RUN      g_ejecuta
   RUN      g_ejecuta

   #Separa sumario
   LET      g_ejecuta                   =
            "sed -e '/^09/!d' ",g_seg_modulo.ruta_rescate   CLIPPED,
            "/",nom_archivo  CLIPPED," >", g_seg_modulo.ruta_rescate  CLIPPED,
            "/",g_tui13_sum_noti_fin_uni
   RUN      g_ejecuta
   ERROR   "    Procesando Informacion... "
END FUNCTION

FUNCTION    f_120_carga_tablas_temporales()
   #Carga cabeza
   CALL     f_125_salida("1",g_tui13_cza_noti_fin_uni,g_tui13_cza_noti_fin_uni)
   LET      g_ejecuta                  =
            "cd ",g_seg_modulo.ruta_rescate    CLIPPED,
            "/;dbload -d safre_tmp -c ","insert"  CLIPPED,
            " -l " CLIPPED," ",g_tui13_cza_noti_fin_uni  CLIPPED,
            ".err" CLIPPED, " -e 100 -n 1000 -k  > tui13_uni_issste.log"
   RUN      g_ejecuta

   #Carga detalle
   CALL     f_125_salida("2",g_tui13_det_noti_fin_uni,g_tui13_det_noti_fin_uni)
   LET      g_ejecuta                  =
            "cd ",g_seg_modulo.ruta_rescate    CLIPPED,
            "/;dbload -d safre_tmp -c ","insert"  CLIPPED,
            " -l " CLIPPED," ",g_tui13_det_noti_fin_uni   CLIPPED,
            ".err" CLIPPED," -e 100 -n 1000 -k >> tui13_uni_issste.log  "
   RUN      g_ejecuta

   #Carga sumario
   CALL     f_125_salida("9",g_tui13_sum_noti_fin_uni,g_tui13_sum_noti_fin_uni)
   LET      g_ejecuta                  =
            "cd ",g_seg_modulo.ruta_rescate    CLIPPED,
            "/;dbload -d safre_tmp -c ","insert"  CLIPPED,
            " -l " CLIPPED," ",g_tui13_sum_noti_fin_uni  CLIPPED,
            ".err" CLIPPED," -e 100 -n 1000 -k >> tui13_uni_issste.log  "
   RUN      g_ejecuta
END FUNCTION

FUNCTION    f_125_salida(v_tipo_reg,v_nom_arch,v_tabla)
   DEFINE   v_tabla                        CHAR(30),
            v_tipo_reg                     SMALLINT,
            l_nom_archivo_dat              ,
            l_nom_archivo_cmd              ,
            v_nom_arch                     CHAR(60),
            l_layout_cod                   SMALLINT
   LET      l_layout_cod                   =  50
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
   CREATE   TEMP  TABLE    tui13_load_uni_issste (l_reg_cza       CHAR(500))
   LET      g_ejecuta                =
            g_seg_modulo.ruta_rescate CLIPPED, "/", g_tui13_cza_noti_fin_uni
   LOAD     FROM     g_ejecuta    INSERT  INTO  tui13_load_uni_issste

   #Valida cabeza
   SELECT   *
     INTO   l_reg_cza
     FROM   tui13_load_uni_issste
   IF       l_reg_cza[03,04]            <> "02"     THEN
            PROMPT   "Archivo Invalido Favor de Verificarlo  <Enter Sair> "
                     FOR     g_enter
            EXIT     PROGRAM
   END IF
   IF       l_reg_cza[05,06]            <> "13"     THEN
            PROMPT   "Archivo Invalido Favor de Verificarlo  <Enter Sair> "
                     FOR     g_enter
            EXIT     PROGRAM
   END IF

   #Despliega datos
   LET      g_fecha_presentacion            =
            MDY(l_reg_cza[24,25],l_reg_cza[26,27],l_reg_cza[20,23])
   DISPLAY  g_fecha_presentacion           TO  fecha_presentacion

   #Verifica si ya se cargó
   LET      l_consec_lote_dia               =  l_reg_cza[28,30]
   LET      g_cargado                       =  0
   SELECT   COUNT(*)     INTO   g_cargado
     FROM   safre_af:tui13_cza_noti_fin_uni  A
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
     FROM   safre_tmp:tui13_det_noti_fin_uni;
   LET      l_num_reg                       =  l_count
   DISPLAY  g_today    USING   "DD-MM-YYYY"  AT  1,65 ATTRIBUTE(REVERSE)
   DISPLAY  "    Registros de Detalle a Procesar     :",
            l_num_reg       USING  "####,##&", "       " AT  13,3
   IF       NOT       l_num_reg             THEN
            ERROR   "  NO  Hay  Registros  a  Procesar   Teclee <Enter> para Salir  ......    "
            PROMPT  "  Teclee  <Enter>  para  Salir   ......  ?                                      "
            FOR     g_enter
            EXIT    PROGRAM
   END IF

   #verifica continuar
   WHILE  TRUE
            PROMPT    "  Desea Continuar con la Carga de Uinificación Op. 13",
                      "  <S/N> ?   "   FOR   g_enter
                     IF        g_enter  MATCHES      "[sSnN]"       THEN
                               IF       g_enter        MATCHES  "[sS]"    THEN
                                        EXIT     WHILE
                               ELSE
                                        EXIT     PROGRAM
                               END  IF
                     END  IF
   END  WHILE

   #Asigna folio
   INSERT   INTO     safre_af:glo_folio   VALUES(0);
   SELECT   MAX(folio)
     INTO   g_folio
     FROM   safre_af:glo_folio;
   IF       g_folio       IS  NULL   THEN
            LET      g_folio         =  0
   END IF
   DISPLAY  g_folio           TO  folio
   DROP     TABLE    tui13_load_uni_issste ;
END FUNCTION

FUNCTION    f_150_arma_historicos()
   CALL     f_155_arma_tui13_ctr_folio()
   CALL     f_160_arma_tui13_cza_noti_fin_uni()
   CALL     f_170_arma_tui13_det_noti_fin_uni()
   CALL     f_210_arma_tui13_sum_noti_fin_uni()
#  UPDATE   STATISTICS     FOR  TABLE   tui13_det_noti_fin_uni;
END FUNCTION

FUNCTION  f_155_arma_tui13_ctr_folio()
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

    #primer dia habil mes siguiente a fecha presentación
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
    LET     reg_tui13_ctr_folio.folio               =  g_folio
    LET     reg_tui13_ctr_folio.archivo             =  nom_archivo
    LET     reg_tui13_ctr_folio.fecha_presentacion  =  g_fecha_presentacion
    LET     reg_tui13_ctr_folio.fecha_provision     =  l_fecha_var
    LET     reg_tui13_ctr_folio.fecha_liquidacion   =  l_fecha_var
    LET     reg_tui13_ctr_folio.estado              =  101
    LET     reg_tui13_ctr_folio.usuario             =  g_usuario
    INSERT  INTO  safre_af:tui13_ctr_folio
            VALUES (reg_tui13_ctr_folio.*)
END FUNCTION

FUNCTION    f_160_arma_tui13_cza_noti_fin_uni()
   DEFINE   l_sql                            CHAR(1500)
   ERROR    "  PROCESANDO  INFORMACION ...... "
   LET      g_insertados                     =  0
   LET      l_sql                            =
           'INSERT   INTO    safre_af:tui13_cza_noti_fin_uni  ',
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
           'FROM     safre_tmp:tui13_cza_noti_fin_uni  A  '
   LET      l_sql                 =  l_sql   CLIPPED
   PREPARE  qry_cza            FROM  l_sql
   EXECUTE  qry_cza
END FUNCTION

FUNCTION    f_170_arma_tui13_det_noti_fin_uni()
   DEFINE   l_sql                            CHAR(1500)
   DEFINE   l_fecha                          DATE
   DEFINE   l_con_error                      INTEGER
   SELECT   COUNT(*)
     FROM   safre_tmp:tui13_det_noti_fin_uni
    WHERE  (fecha_movto            IS  NULL
      OR    fecha_movto             =  "          ")
      OR   (fecha_presentacion     IS  NULL
      OR    fecha_presentacion      =  "          ")
   IF       l_con_error           THEN
            ERROR   "  VERIFIQUE  ARCHIVO CONTIENE REGISTROS CON FECHA ERRONEA..."
            PROMPT  "  TECLEE  <<Enter>>  para Salir...      "  for  g_enter
            DELETE    FROM  tui13_ctr_folio
             WHERE    folio               =  g_folio;
            DELETE    FROM  tui13_cza_noti_fin_uni
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
                    'B.tipo_unificacion              ,',
                    'B.clasificacion_uni             ,',
                    'B.tipo_ent_ced_cta              ,',
                    'B.cve_ent_ced_cta               ,',
                    'B.tipo_diagnostico              ,',
                    'MDY(B.fecha_presentacion[05,06]  ,',
                    '    B.fecha_presentacion[07,08]  ,',
                    '    B.fecha_presentacion[01,04]) ,',
                    'MDY(B.fecha_movto[05,06]       ,',
                    '    B.fecha_movto[07,08]       ,',
                    '    B.fecha_movto[01,04])      ,',
                    'B.curp_unificador               ,',
                    'B.nss_unificador                ,',
                    'B.paterno_unificador            ,',
                    'B.materno_unificador            ,',
                    'B.nombres_unificador            ,',
                    'B.id_regimen_pension            ,',
                    'B.curp_unificada                ,',
                    'B.paterno_unificada             ,',
                    'B.materno_unificada             ,',
                    'B.nombres_unificada             ,',
                    'B.bimestres_cot_unificada       ,',
                    '101                              ',
             'FROM     safre_tmp:tui13_det_noti_fin_uni  B  '
   LET      l_sql                 =  l_sql   CLIPPED
   PREPARE  qry_saldo         FROM     l_sql
   DECLARE  cur_saldos       CURSOR FOR    qry_saldo
   FOREACH  cur_saldos         INTO     g_det_noti.*
            SELECT   MAX(n_seguro)
              INTO   g_det_noti.nss
              FROM   afi_mae_afiliado
             WHERE   n_unico              =  g_det_noti.curp
               --AND   tipo_solicitud       =  8;
            IF       g_det_noti.nss       IS  NULL  THEN
                     LET      g_det_noti.estado     =  114
            END IF
            INSERT   INTO      tui13_det_noti_fin_uni
                     VALUES  (g_det_noti.*)
            --CALL     f_180_act_bimestres()
            LET      g_insertados           =  g_insertados  +  1
            DISPLAY  g_insertados      USING  "###,###"     AT 15,45
   END FOREACH
END FUNCTION


FUNCTION  f_180_act_bimestres()
   DEFINE   l_num_bim_acum             ,
            l_marca_cod                SMALLINT,
            l_bimestres                CHAR(3)
   CALL     F_225_libera_unificadora()
   CALL     F_230_activa_unificadora()
   SELECT   n_seguro
     INTO   g_n_seguro
     FROM   afi_mae_afiliado
    WHERE   n_unico                =  g_det_noti.curp_unificada
      --AND   tipo_solicitud         =  8;

   UPDATE   afi_icefa_issste
      SET   num_bim_acum           =  0 #es=a g_det_noti.bimestres_cot_unificada
    WHERE   nti                    =  g_n_seguro;
########   Trae marca para actualizar la cuenta unificada
   SELECT   marca_cod
     INTO   l_marca_cod
     FROM   taa_cd_tab_mov_op03
    WHERE   tipo_diagnostico      =  g_det_noti.tipo_diagnostico;
   UPDATE   afi_cta_peissste
      SET   estatus               =  l_marca_cod
    WHERE   nti                   =  g_n_seguro;

   IF       g_det_noti.tipo_diagnostico       <  90    THEN
            RETURN
   END IF
   SELECT   num_bim_acum
     INTO   l_num_bim_acum
     FROM   afi_icefa_issste
    WHERE   nti               =  g_det_noti.nss;
   LET      l_num_bim_acum    =
            l_num_bim_acum    +  g_det_noti.bimestres_cot_unificada
   LET      l_bimestres       =  l_num_bim_acum   USING "&&&"
   UPDATE   afi_icefa_issste
      SET   num_bim_acum      =  l_bimestres
    WHERE   nti               =  g_det_noti.nss;
   CALL     f_180_act_historico()
END FUNCTION

#########################################################################
FUNCTION  f_180_act_historico()
   DEFINE   l_num_bim_acum             ,
            l_marca_cod                ,
            l_tipo_cta                 SMALLINT,
            l_tipo_trabajador          CHAR(01)
   --DEFINE   l_reg        RECORD      LIKE   afi_cta_peissste.*

INITIALIZE  l_tipo_cta, l_tipo_trabajador TO NULL

   SELECT   tipo_cta
     INTO   l_tipo_cta
     FROM   tui13_herencia_marcas
    WHERE   regimen_pension       =  g_det_noti.marca_regimen_pension;

#####      Actualiza  cuenta  unificadora
#Se agrega actualizacion al tipo de trabajador
   UPDATE   afi_cta_peissste
      SET   tipo_cta              =  l_tipo_cta
    WHERE   nti                   =  g_det_noti.nss;

   SELECT   tipo_trabajador
     INTO   l_tipo_trabajador
     FROM   afi_cta_peissste
    WHERE   nti                   =  g_det_noti.nss;

#Se agrega actualizacion al tipo de trabajador ACS-05/may/12
#REQ PST-613
    IF (l_tipo_cta = 5 OR l_tipo_cta = 6 OR l_tipo_cta = 11)
        AND (l_tipo_trabajador ="N") THEN
        LET l_tipo_trabajador ="P"

        UPDATE afi_cta_peissste SET tipo_trabajador = l_tipo_trabajador
        WHERE   nti                   =  g_det_noti.nss;
    END IF

   INSERT   INTO     afi_his_marcaje_peis
            VALUES  (g_det_noti.nss,
                     g_det_noti.curp,
                     TODAY,
                     l_tipo_cta,
                     l_tipo_trabajador);
END FUNCTION

FUNCTION    F_225_libera_unificadora()
   DEFINE   l_estado_marca              ,
            l_marca_cod                SMALLINT,
            l_correlativo              INTEGER
   LET      l_estado_marca           =  40  #Desmarca y Habilita con HIstoria
   SELECT   marca_cod
     INTO   l_marca_cod
     FROM   taa_cd_tab_mov_op03
    WHERE   tipo_diagnostico      =  g_det_noti.tipo_diagnostico;

   SELECT   correlativo
     INTO   l_correlativo
     FROM   cta_act_marca
    WHERE   nss                   =  g_det_noti.nss
      AND   marca_cod             =  l_marca_cod;

   LET      g_ejecuta               =
           "EXECUTE  PROCEDURE  desmarca_cuenta(?,?,?,?,?,?)"
   LET      g_ejecuta               =  g_ejecuta   CLIPPED
   PREPARE  clausula_spl1        FROM  g_ejecuta
   EXECUTE  clausula_spl1       USING  g_det_noti.nss,
                                       l_marca_cod,
                                       l_correlativo,
                                       l_estado_marca,
                                       l_marca_cod,
                                       g_usuario
END FUNCTION

FUNCTION    F_230_activa_unificadora()
   DEFINE   l_estado_marca              ,
            l_marca_cod                SMALLINT,
            l_correlativo              INTEGER
   LET      l_estado_marca           =  40  #Desmarca y Habilita con HIstoria
   LET      l_marca_cod              =  140
   SELECT   correlativo
     INTO   l_correlativo
     FROM   cta_act_marca
    WHERE   nss                   =  g_det_noti.nss
      AND   marca_cod             =  140;

   LET      g_ejecuta               =
           "EXECUTE  PROCEDURE  desmarca_cuenta(?,?,?,?,?,?)"
   LET      g_ejecuta               =  g_ejecuta   CLIPPED
   PREPARE  spl_activa           FROM  g_ejecuta
   EXECUTE  spl_activa          USING  g_det_noti.nss,
                                       l_marca_cod,
                                       l_correlativo,
                                       l_estado_marca,
                                       l_marca_cod,
                                       g_usuario
END FUNCTION

FUNCTION    f_210_arma_tui13_sum_noti_fin_uni()
   DEFINE   l_sql                            CHAR(1500)
   LET      l_sql                            =
           'INSERT   INTO    safre_af:tui13_sum_noti_fin_uni  ',
           'SELECT  ',g_folio            ,',',
                    'E.tipo_registro     ,',
                    'E.num_registros     ,',
                    'E.regs_diag_81      ,',
                    'E.regs_diag_82      ,',
                    'E.regs_diag_83      ,',
                    'E.regs_diag_84      ,',
                    'E.regs_diag_85      ,',
                    'E.regs_diag_90      ,',
                    'E.regs_diag_91      ,',
                    'E.regs_diag_92      ',
           'FROM     safre_tmp:tui13_sum_noti_fin_uni         E  '
   LET      l_sql                 =  l_sql   CLIPPED
   PREPARE  qry_sumario     FROM     l_sql
   EXECUTE  qry_sumario
END FUNCTION


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
