#####################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                 
#Propietario       => E.F.P.                                      
#Programa          => TCAAC006                                    
#Descripcion       => CARGA Y VALIDACION SOLICITUDES CEDENTE    
#Fecha             => 24 FEBRERO 2003        .                
#Por               => JESUS DAVID YAÒEZ MORENO               
#Sistema           => TCAA (CEDENTE) 
#####################################################################
#Modificacion      => Se Genera reporte de cuentas con mas de 175
#                  => salarios minimos  REQ_XXI-823
#                  => Alejandro Chagoya S.       02-Sep-2011
#####################################################################
# 10-Mayo-2012     => Se agrega informe de 175 Salarios minimos al 
# CPL-843          => layout --> ACS 
#####################################################################
# 04-Julio-2013    => Se excluye la validación de los 175 SM para los
#                  => Tipos de Traspaso 38 y 55 
# CPL-1333         => JLNJ
#####################################################################
# 15-Noviembre-2013=> Se incluye el tipo de solicitud 17 para la
#                     busqueda de la cuenta individual
# CPL-1419         => JLNJ
#########################################################################
# 15-Noviembre-2013=> Se modifica la forma en la que se busca la 
#                     cuenta individual para las solicitudes con origenes
#                     01, 02, 12, 20, 21, 24, 25, 38, 51, 55 y 57 para 
#                     buscarlas por NSS y/o CURP
# CPL-1442         => JLNJ
#########################################################################

DATABASE safre_af    

GLOBALS
    DEFINE tot_registros              INTEGER
    DEFINE xcodigo_marca              ,
           xcodigo_rechazo            ,
           vmarca_causa               , 
           vcodigo_rechazo            ,
           vestado_marca              ,
           marca_entra                SMALLINT,
           cuenta                     ,
           g_pid                      ,
           g_proceso_cod              ,
           g_opera_cod                INTEGER,
           g_tipo_traspaso            ,
           periodo_carga              SMALLINT

DEFINE     t_cza                      ,
           t_det                      ,
           t_sum                      ,
           uno                        ,
           cero                       ,
           fusion                     ,
           procedente                 SMALLINT

    DEFINE v_folio                    ,
           recepcion                  ,
           tot_total                  INTEGER

    DEFINE 
           vfecha_presentacion        ,
           vfecha_causa               ,
           g_hoy                      DATE

    DEFINE  
        enter                         CHAR(001) ,
        nom_archivo                   CHAR(060) ,
        g_opc                         CHAR(001) ,
        v_diag                        CHAR(200) 

    DEFINE v_rechazo                  CHAR(200) ,
        v_operacion                   CHAR(013) ,
        comm                          CHAR(100) ,
        hora                          CHAR(008) ,
        g_lista                       CHAR(100) ,
        g_lista2                      CHAR(100) , --ACS
        vusuario                      CHAR(010) ,
        rfolio                        CHAR(010) ,
        muestra_log                   CHAR(1)   ,
        g_registro                    CHAR(300) ,
        g_ejecuta                     CHAR(200) 

    DEFINE   g_salario_diario            DEC(10,2)
    DEFINE  
        reg_ctr_folio        RECORD  LIKE  safre_af:taa_cd_ctr_folio.*,
        p_tablayout          RECORD  LIKE  safre_af:tab_layout.*,
        p_param_modulo       RECORD  LIKE  safre_af:seg_modulo.*,
        p_tabcampo           RECORD  LIKE  safre_af:tab_campo.*,
        p_puestos            RECORD  LIKE  safre_af:tab_puesto.*

   DEFINE  reg_soli          RECORD  LIKE  safre_af:taa_cd_det_cedido.*,
           g_parametros      RECORD  LIKE  safre_af:seg_modulo.*
END GLOBALS

MAIN 
    DEFINE
            vpausa                CHAR(1),
            ejecuta               CHAR(150),
            enter                 CHAR(1)

    OPTIONS 
        HELP  FILE  "/safre_prc/taa/rescate/soli_status.o",
        INPUT WRAP          ,
        PROMPT LINE LAST ,   
        ACCEPT KEY CONTROL-I
#       DEFER   INTERRUPT

    CALL STARTLOG (FGL_GETENV("USER")||".TCAAC006.log")

    LET     g_pid                       =  ARG_VAL(1)
    LET     g_proceso_cod               =  ARG_VAL(2)
    LET     g_opera_cod                 =  ARG_VAL(3)
    LET     nom_archivo                 =  ARG_VAL(4)  CLIPPED
#   LET     nom_archivo                 =  "arch_ced"
    IF      g_pid        THEN
            DISPLAY  " "
            DISPLAY  ".1"
    END IF
    CALL inicio() #i
    
    CREATE TEMP TABLE taa_cd_archivo_cedente
        (
         registro   CHAR(100)
        )

    LET ejecuta = "ls ",p_param_modulo.ruta_rescate CLIPPED,"/",
                      " > ",p_param_modulo.ruta_rescate CLIPPED,"/arch_ced"
    RUN ejecuta

    LET ejecuta = p_param_modulo.ruta_rescate CLIPPED,"/", "arch_ced"

    LOAD FROM ejecuta INSERT INTO taa_cd_archivo_cedente

    IF  g_pid IS  NULL  THEN

        OPEN WINDOW taac0061 AT 2,2 WITH FORM "TCAAC0061" ATTRIBUTE(BORDER)      
        DISPLAY " <Esc>Ejecutar      <Ctrl-c>Salir                                             " AT 1,1 ATTRIBUTE(REVERSE)
        DISPLAY g_hoy  USING"DD-MM-YYYY" AT 1,65 ATTRIBUTE(REVERSE) 
        DISPLAY "<TCAAC006>  RECEPCION DE ARCHIVO DE SOLICITUDES TRASPASO AFORE-AFORE CEDENTE              " AT 2,1 ATTRIBUTE(REVERSE)


        INPUT BY NAME periodo_carga, nom_archivo  WITHOUT DEFAULTS                                  
            AFTER FIELD nom_archivo
            IF nom_archivo IS NULL THEN 
               ERROR "ARCHIVO NO VALIDO... TECLEAR NOMBRE DE ARCHIVO VALIDO ..."
                NEXT FIELD nom_archivo
            END  IF

            SELECT "OK"
            FROM   taa_cd_archivo_cedente 
            WHERE  registro = nom_archivo

            IF STATUS = NOTFOUND THEN
               ERROR "ARCHIVO NO VALIDO... TECLEAR NOMBRE DE ARCHIVO VALIDO ..."
               NEXT FIELD nom_archivo
            END IF
            
            #CPL-1377 Se agrega el indicador del periodo a ejecutar para saber si es Mensual o Bimestral
            AFTER FIELD periodo_carga
            IF periodo_carga  IS  NULL   THEN
               ERROR     " Periodo de carga : 1=Mensual,2=Bimestral"
               NEXT FIELD periodo_carga
            END IF 

        ON KEY (ESC)
            IF nom_archivo IS NULL THEN
               ERROR "ARCHIVO NO VALIDO... TECLEAR NOMBRE DE ARCHIVO VALIDO ..."
               NEXT FIELD nom_archivo
            END IF

            SELECT "OK"
            FROM   taa_cd_archivo_cedente
            WHERE  registro = nom_archivo

            IF STATUS = NOTFOUND THEN
               ERROR "ARCHIVO NO VALIDO... TECLEAR NOMBRE DE ARCHIVO VALIDO ..."
               NEXT FIELD nom_archivo
            END IF

            DISPLAY "                                     " AT 19,1 
            ERROR " PROCESANDO INFORMACION...                 "

            #valida e inserta en las tablas correspondientes

            LET   nom_archivo           =  nom_archivo CLIPPED
            IF    recepciona_archivo() != FALSE THEN 
                  CALL trata_det_sum() 
                  CALL sube_historico()
                  CALL imprime_reporte()
                 -- CALL llama_showhelp()
                  EXIT INPUT
            ELSE 
            DISPLAY "                                     " AT 19,1 
                  ERROR  "  LOTE DUPLICADO ...<ENTER> PARA SALIR "
                  PROMPT  " "
                      FOR  enter
                 EXIT PROGRAM
            END IF

            ERROR " PROCESANDO INFORMACION...                 "
            LET nom_archivo = NULL
                    CLEAR FORM       
                    EXIT INPUT

        ON KEY(CONTROL-C) 

           PROMPT " PROCESO CANCELADO...<ENTER> PARA CONTINUAR ?" FOR enter
            EXIT PROGRAM

        ON KEY(INTERRUPT) 
           PROMPT " PROCESO CANCELADO...<ENTER> PARA CONTINUAR ?" FOR enter
            EXIT PROGRAM
      END INPUT

        SELECT COUNT(*)
        INTO   tot_registros
        FROM   taa_cd_det_cedido  a
        WHERE  a.folio       =  v_folio;
        DISPLAY "                                                  "  AT 19,1
        ERROR "                                                    "
        DISPLAY "TOTAL REGISTROS CARGADOS: ",tot_registros    AT 15,1
        DISPLAY "REPORTE GENERADO EN RUTA: ",g_lista  AT 16,1 ATTRIBUTE(REVERSE)
        DISPLAY "REPORTE SALARIOS MINIMOS: ",g_lista2  AT 17,1 ATTRIBUTE(REVERSE)
        PROMPT  "PROCESO FINALIZADO TECLEE <ENTER> PARA  SALIR?..." 
        FOR enter

        LET comm = "lp ",g_lista
        RUN comm
        
        LET comm = "lp ",g_lista2 --Se imprime reporte detalle --ACS
        RUN comm

        CLOSE   WINDOW  taac0061
    ELSE
        DISPLAY " TCAAC006  RECEPCION DE ARCHIVO DE SOLICITUDES AFO-AFO (CEDENTE)            "
        DISPLAY "IDENTIFICADOR DEL PROCESO  :",g_pid          USING  "######"
        DISPLAY "CODIGO DEL PROCESO         :",g_proceso_cod  USING  "######"
        DISPLAY "CODIGO DE OPERACION        :",g_opera_cod    USING  "######"
        DISPLAY "NOMBRE DEL ARCHIVO         : ",nom_archivo
    END IF

    IF      g_pid     THEN

            LET nom_archivo  = nom_archivo CLIPPED

            SELECT "OK"
            FROM   taa_cd_archivo_cedente 
            WHERE  registro = nom_archivo

            IF STATUS = NOTFOUND THEN
              DISPLAY "Program stopped"
              DISPLAY "ERROR ARCHIVO NO VALIDO... TECLEAR NOMBRE DE ARCHIVO VALIDO ..."
              EXIT PROGRAM
            END IF

            IF    recepciona_archivo() != FALSE THEN 
                  CALL trata_det_sum() 
                  CALL sube_historico()
                  CALL imprime_reporte()
            ELSE 
                 DISPLAY "Program stopped"
                 DISPLAY "ERROR LOTE DUPLICADO ..."
                 EXIT  PROGRAM
            END IF
    END IF

    UPDATE   safre_af:taa_cd_ctr_folio
       SET   estado      =  procedente
     WHERE   folio       =  v_folio


   IF g_pid IS  NOT  NULL     THEN
      SELECT COUNT(*) 
      INTO   cuenta
      FROM   safre_af:taa_cd_det_cedido
      WHERE  folio = v_folio
      DISPLAY "TOTAL REGISTROS CARGADOS   : ",cuenta USING"##########&"
      DISPLAY "FIN DE PROGRAMA  TCAAC006  : "

       CALL actualiza_operacion(g_pid,g_proceso_cod,g_opera_cod,reg_ctr_folio.folio)

   END IF
-----------------   INICIA  ASIGNACION  DE  PERMISOS   ----------
   LET    g_ejecuta  = "echo \'chmod  777 ",p_param_modulo.ruta_rescate CLIPPED,"/cza_status", "\' > arch_paso;" , "sh arch_paso 1>>err_paso 2>&1"
   RUN  g_ejecuta
   LET    g_ejecuta  = "echo \'chmod  777 ",p_param_modulo.ruta_rescate CLIPPED,"/soli_*", "\' > arch_paso;" , "sh arch_paso 1>>err_paso 2>&1"
   RUN  g_ejecuta
   LET    g_ejecuta  = "echo \'chmod  777 ",p_param_modulo.ruta_rescate CLIPPED,"/arch_*", "\' > arch_paso;" , "sh arch_paso 1>>err_paso 2>&1"
   RUN  g_ejecuta
   LET    g_ejecuta  = "echo \'chmod  777 ",p_param_modulo.ruta_rescate CLIPPED,"/taa", "\' > arch_paso;" , "sh arch_paso 1>>err_paso 2>&1"
   RUN  g_ejecuta
   LET    g_ejecuta  = "rm  arch_paso "
   RUN  g_ejecuta
   LET    g_ejecuta  = "rm  err_paso "
   RUN  g_ejecuta
-----------------   TERMINA ASIGNACION  DE  PERMISOS   ----------

END MAIN

FUNCTION  llama_showhelp()
#llsh---------------------

#### Arma  Parametros  para  SHOWHELP

    LET    g_ejecuta  = "echo .1",
                        " >",p_param_modulo.ruta_rescate CLIPPED,"/cza_status"
    RUN    g_ejecuta

   LET g_ejecuta = "cat ",p_param_modulo.ruta_rescate CLIPPED,"/cza_status ",
                          p_param_modulo.ruta_rescate CLIPPED,"/soli_status.log >",
                          p_param_modulo.ruta_rescate CLIPPED,"/soli_status"

    RUN g_ejecuta

    LET    g_ejecuta  = "mkmessage ",
                      p_param_modulo.ruta_rescate CLIPPED,"/soli_status"," ",
                      p_param_modulo.ruta_rescate CLIPPED,"/soli_status.o"
    RUN g_ejecuta

     DISPLAY "   <Ctrl-B> Ver Resultado de la Carga                <Ctrl-C>  Continuar " AT 19,1     ATTRIBUTE(REVERSE)
     INPUT BY NAME  muestra_log WITHOUT DEFAULTS
            ON KEY(CONTROL-B)
        	CALL SHOWHELP(1)
            ON KEY(CONTROL-C)
                EXIT PROGRAM
     END INPUT
#####   Termina    Estructura  del SHOWHELP

     DISPLAY "                                                                         " AT 19,1    
        PROMPT  "PROCESO FINALIZADO...<ENTER> PARA  SALIR ?" FOR enter
        LET comm = "lp ",g_lista
        RUN comm
END FUNCTION

FUNCTION recepciona_archivo()
#ra--------------------------

    DEFINE #loc #char
        ejecuta               CHAR(200)


    LET ejecuta = "sed -e '/^01/!d' ",
                  p_param_modulo.ruta_rescate CLIPPED,
                  "/",nom_archivo CLIPPED," >",
                  p_param_modulo.ruta_rescate CLIPPED,
                  "/",p_tablayout.arch_cza
    RUN ejecuta  

    IF trata_cza() != FALSE THEN
       LET ejecuta = "sed -e '/^02/!d' "                ,
                     p_param_modulo.ruta_rescate CLIPPED   ,
                     "/",nom_archivo CLIPPED," >"          ,
                     p_param_modulo.ruta_rescate CLIPPED       ,
                     "/",p_tablayout.arch_det
       RUN ejecuta  
 

       LET ejecuta = "sed -e '/^09/!d' ",p_param_modulo.ruta_rescate CLIPPED,
                     "/",nom_archivo CLIPPED," >",
                     p_param_modulo.ruta_rescate CLIPPED,
                     "/",p_tablayout.arch_sum
       RUN ejecuta  
       RETURN TRUE
    ELSE
       RETURN FALSE
    END IF

            DISPLAY "                                     " AT 19,1 
            ERROR " PROCESANDO INFORMACION...                 "
END FUNCTION

FUNCTION trata_cza()
#tc-----------------
    DEFINE  ejecuta          ,      
            v_plano                    CHAR(500)
    DEFINE  l_consec_lote_dia          ,
            l_cargado                  SMALLINT

    CREATE TEMP TABLE taa_cd_cza_cedente ( v_plano CHAR(500))

    LET ejecuta = p_param_modulo.ruta_rescate CLIPPED, "/",
                  p_tablayout.arch_cza
    LOAD FROM ejecuta INSERT INTO taa_cd_cza_cedente

    SELECT * INTO v_plano FROM  taa_cd_cza_cedente

    IF     v_plano[03,04]            <>  "02" THEN
           IF         g_pid  IS      NULL  THEN
                ERROR"IDENTIFICADOR DE SERVICIO INVALIDO"
                SLEEP 4
                EXIT PROGRAM
           ELSE
               DISPLAY "Program stopped" 
               DISPLAY "ERROR IDENTIFICADOR DE SERVICIO INVALIDO..."
               EXIT PROGRAM
          END IF

    END IF
    IF    v_plano[05,06]      <>  "01" THEN
          IF     g_pid        IS   NULL THEN
                 ERROR"IDENTIFICADOR DE OPERACION INVALIDO"
                 SLEEP 4
                 EXIT PROGRAM
          ELSE
               DISPLAY "Program stopped" 
               DISPLAY "ERROR IDENTIFICADOR DE OPERACION INVALIDO"
               EXIT PROGRAM
          END IF
    END IF

    LET    vfecha_presentacion     =  MDY(v_plano  [21,22],
                                      v_plano      [23,24],
                                      v_plano      [17,20])
    LET    l_consec_lote_dia       =  v_plano[25,27]

    SELECT   "1"
      INTO   l_cargado
      FROM   safre_af:taa_cd_cza_cedido A
     WHERE   A.fecha_presentacion   =  vfecha_presentacion
       AND   A.consec_lote_dia      =  l_consec_lote_dia
    IF       l_cargado      THEN
             ERROR   " ARCHIVO CARGADO ANTERIORMENTE....  "
             PROMPT  " TECLEE   <ENTER> PARA  SALIR?... " FOR enter
             EXIT  PROGRAM
    END IF

# armo archivo de comando cabecera

    CALL salida(t_cza,p_tablayout.arch_cza,p_tablayout.tab_cza)

    LET ejecuta = "cd ",p_param_modulo.ruta_rescate CLIPPED,               
                  "/;dbload -d safre_tmp -c ",p_tablayout.nom_arch,
                  " -l " CLIPPED," ",p_tablayout.arch_cza CLIPPED,
          #       ".err" CLIPPED, " -e 100 -n 1000 -k  >> soli_status.log"
                  ".err" CLIPPED, " -e 100 -n 1000 -k  > soli_status.log"

    RUN      ejecuta

    RETURN TRUE
END FUNCTION

FUNCTION  arma_taa_cd_ctr_folio()
#atcf------------------------

    DEFINE  l_dia_var                   ,
            l_dos_dias_uno              ,
            l_mes_envio                 ,
            l_mes_liqui                 ,
            l_dia_semana                SMALLINT
    DEFINE  
            l_fecha_var                 DATE
    LET     l_dia_var                   =  0
    LET     l_fecha_var                 =  vfecha_presentacion
    WHILE   TRUE
       LET      l_dia_semana       =  WEEKDAY(l_fecha_var)
       IF       l_dia_semana       >  0  AND
                l_dia_semana       <  6  THEN
            SELECT  "ok"
              FROM   safre_af:tab_feriado
             WHERE   feria_fecha        =  l_fecha_var  
            IF       STATUS             =  NOTFOUND     THEN
                  IF   g_tipo_traspaso    =  1      OR
                       g_tipo_traspaso    =  4      THEN
                       IF       l_dia_var        =  7     THEN
                                LET   reg_ctr_folio.fecha_envio_saldos
                                                 =  l_fecha_var 
                       END IF  
                       IF       l_dia_var        =  10    THEN
                                EXIT  WHILE
                       END IF
                       LET      l_dia_var        =  l_dia_var  +  1
                  ELSE
                       IF       l_dia_var        =  2     THEN
                                LET   reg_ctr_folio.fecha_envio_saldos
                                                 =  l_fecha_var 
                       END IF  
                       IF       l_dia_var        =  5     THEN
                                EXIT  WHILE
                       END IF
                       LET      l_dia_var        =  l_dia_var  +  1
                  END IF
            END IF
       END IF
       LET      l_fecha_var        =  l_fecha_var     +  1
    END WHILE
    LET     reg_ctr_folio.folio               =  v_folio
    LET     reg_ctr_folio.tipo_traspaso       =  g_tipo_traspaso
    LET     reg_ctr_folio.fecha_presentacion  =  vfecha_presentacion
    LET     reg_ctr_folio.fecha_liquidacion   =  l_fecha_var
    LET     reg_ctr_folio.estado              =  recepcion
    LET     reg_ctr_folio.usuario             =  vusuario
    LET     l_mes_envio     =  reg_ctr_folio.fecha_envio_saldos
            USING  "MM"
   
    LET     l_mes_liqui     =  reg_ctr_folio.fecha_liquidacion
            USING  "MM"

    INSERT  INTO  safre_af:taa_cd_ctr_folio 
            VALUES (reg_ctr_folio.*)
    IF      g_pid        IS  NULL     THEN
            DISPLAY  BY  NAME  reg_ctr_folio.folio
            DISPLAY  BY  NAME  reg_ctr_folio.fecha_presentacion
            DISPLAY  BY  NAME  reg_ctr_folio.fecha_envio_saldos
            DISPLAY  BY  NAME  reg_ctr_folio.fecha_liquidacion
    ELSE 
            DISPLAY  "FOLIO DE OPERACION         : ",reg_ctr_folio.folio
 USING  "######"
            DISPLAY  "FECHA DE PRESENTACION      : ",reg_ctr_folio.fecha_presentacion
            DISPLAY  "FECHA DE ENVIO SALDO       : ",reg_ctr_folio.fecha_envio_saldos
            DISPLAY  "FECHA DE LIQUIDACION       : ",reg_ctr_folio.fecha_liquidacion
   END IF

END FUNCTION

FUNCTION trata_det_sum()
#td----------------------

    DEFINE   ejecuta                 CHAR(500)
   DEFINE   l_reg_procesar             INTEGER

# armo archivo de comando de detalle

    CALL salida(t_det,p_tablayout.arch_det,p_tablayout.tab_det)

    LET ejecuta = "cd ",p_param_modulo.ruta_rescate CLIPPED,               
                  "/;dbload -d safre_tmp -c ",p_tablayout.nom_arch,
                  " -l " CLIPPED," ",p_tablayout.arch_det CLIPPED,
                  ".err" CLIPPED," -e 100 -n 1000 -k >> soli_status.log  "
    RUN      ejecuta

    SELECT   MIN(a.tipo_traspaso_proc)
      INTO   g_tipo_traspaso
      FROM   safre_af:taa_cd_tipo_traspaso a
    WHERE  a.tipo_traspaso  in (SELECT b.tipo_traspaso 
                                FROM safre_tmp:taa_cd_det_sol_tra_ind b)

    CALL  arma_taa_cd_ctr_folio()

# armo archivo de sumario

    CALL salida(t_sum,p_tablayout.arch_sum,p_tablayout.tab_sum)

    LET ejecuta = "cd ",p_param_modulo.ruta_rescate CLIPPED,               
                  "/;dbload -d safre_tmp -c ",p_tablayout.nom_arch,
                  " -l " CLIPPED," ",p_tablayout.arch_sum CLIPPED,
                  ".err" CLIPPED," -e 100 -n 1000 -k >> soli_status.log   "
    RUN ejecuta                                                        
END FUNCTION


FUNCTION salida(v_tipo_reg,v_nom_arch,v_tabla)
#s--------------------------------------------

    DEFINE #loc #char
        v_tabla               CHAR(30),
        v_tipo_reg            SMALLINT,
        nom_archivo_dat       CHAR(60),
        nom_archivo_cmd       CHAR(60),
        v_nom_arch            CHAR(60)

    LET nom_archivo_cmd = p_param_modulo.ruta_rescate CLIPPED, "/",
                          p_tablayout.nom_arch
    LET nom_archivo_dat = p_param_modulo.ruta_rescate CLIPPED, "/",
                          v_nom_arch

    START REPORT r_report_db TO nom_archivo_cmd
        OUTPUT TO REPORT r_report_db(nom_archivo_dat , 
                                     p_tablayout.layout_cod,
                                     v_tipo_reg, #cza, det, sum
                                     v_tabla     #nombre de la tabla a insertar
                                    )
    FINISH REPORT r_report_db

END FUNCTION

FUNCTION inicio()
#i---------------

    DEFINE  cadena CHAR(200),
            l_desde_sm                    DATE,
            l_sql                         CHAR(400)
    CALL    STARTLOG("TCAAC006.log") 
    LET hora  = TIME
    LET t_cza = "1" 
    LET t_det = "2" 
    LET t_sum = "9" 
    LET uno   = 1
    LET cero  = 0

   LET  g_hoy      =  TODAY
   --LET  g_hoy      =  "05/18/2011"
 
    SELECT user
    INTO vusuario
    FROM safre_af:tab_afore_local 
 
    SELECT a.* 
    INTO   p_param_modulo.*
    FROM   safre_af:seg_modulo a
    WHERE  a.modulo_cod = "taa"

    SELECT b.*
    INTO   p_tablayout.*
    FROM   safre_af:tab_layout b
    WHERE  b.layout_cod = 30000
      AND  b.modulo_cod = "taa"  

    SELECT a.estado
    INTO   recepcion
    FROM   safre_af:taa_cd_edo_cedente a
    WHERE  a.descripcion = 'RECIBIDO'
    AND    a.tipo      = 3

    SELECT a.estado
    INTO   procedente
    FROM   safre_af:taa_cd_edo_cedente  a
    WHERE  a.descripcion = 'PROCEDENTE'
    AND    a.tipo      = 2


    SELECT a.estado
    INTO   fusion
    FROM   safre_af:taa_cd_edo_cedente a
    WHERE  a.descripcion matches '*FUSION*'
    AND    a.tipo = 1

    INSERT INTO safre_af:glo_folio VALUES(0)
  
    SELECT MAX(folio) 
    INTO   v_folio
    FROM   safre_af:glo_folio

    DELETE FROM safre_tmp:taa_cd_cza_sol_tra_ind
    DELETE FROM safre_tmp:taa_cd_det_sol_tra_ind
    DELETE FROM safre_tmp:taa_cd_sum_sol_tra_ind

   SELECT   MAX(fecha_desde_sm)
     INTO   l_desde_sm
     FROM   tabsalario_minimo2
    WHERE   zona_cod               = "A";

   SELECT   monto_sm
     INTO   g_salario_diario
     FROM   tabsalario_minimo2
    WHERE   zona_cod               = "A"
      AND   fecha_desde_sm         =  l_desde_sm;

   LET      l_sql   =
       ' SELECT  siefore, SUM(monto_en_acciones * precio_del_dia)  ',
       '   FROM  dis_cuenta  ,glo_valor_accion            ',
       '  WHERE  nss    =  ? ',
       '    AND  subcuenta  IN(3,10,11,12,15,16,17,18,20,21,22, ',
       '                       23,24,25,26,27,28,29,37) ',
       '    AND  fecha_valuacion   =  ',"'",g_hoy,"'" CLIPPED,' ' ,
       '    AND  codigo_siefore    =  siefore  ',
       '  GROUP  BY  1  ; '   CLIPPED
   PREPARE  sql_salarios         FROM  l_sql

END FUNCTION

FUNCTION sube_historico()
#vl----------------------
   DEFINE   l_tiene_correo             SMALLINT
   DEFINE   l_existe                   SMALLINT

   DEFINE   l_correo_clip              ,
            l_correo_soli_tra          CHAR(40),
            l_n_folio                  LIKE  afi_mae_afiliado.n_folio,
            l_tipo_solicitud           LIKE  afi_mae_afiliado.tipo_solicitud

   DEFINE   txt                        CHAR(1000),
            ejecuta                    CHAR(0200)

   DEFINE   b_fusion                   SMALLINT
   DEFINE   l_ind_cierre_marca         SMALLINT

   DEFINE   v_fecha_trasp              DATE,
            i                          SMALLINT
   DEFINE   l_origen                   CHAR(02),
            l_registros                INTEGER
   DEFINE   ltp             ARRAY[12]   OF    RECORD --subcuenta,siefore
            origen                              CHAR(02),
            registros                           INTEGER
                                          END   RECORD
   SELECT   *
     INTO   g_parametros.*
     FROM   safre_af:seg_modulo
    WHERE   modulo_cod    =  'taa';
   LET      ejecuta        =  v_folio   USING  '&&&&&&&&'   CLIPPED
   FOR       i     =  1   TO  8
             IF       ejecuta[i]    <>  '0'    THEN
                      EXIT  FOR
             END IF
   END FOR
   LET      rfolio       =  ejecuta[i ,8]    CLIPPED
   LET      ejecuta       =   ejecuta[i ,8]    CLIPPED
   LET      g_lista       =  g_parametros.ruta_listados CLIPPED, "/",
            vusuario CLIPPED,".TCAAC006.DETALLE.", ejecuta    CLIPPED
   START    REPORT     f_250_imprime_detalle       TO  g_lista
   DISPLAY "         REGISTROS A PROCESAR POR ORIGEN DE TRASPASO                     " AT 11,1 ATTRIBUTE(REVERSE)
   DISPLAY  "REPORTE DETALLE: ",g_lista   AT  18,1   ATTRIBUTE  (REVERSE)
    FOR      i       =  1   TO  12
             LET      ltp[i].origen          =   " "
             LET      ltp[i].registros       =  0
    END  FOR
    LET      i                      =  0
    DECLARE  c_tipo_trasp     CURSOR   FOR  
    SELECT   tipo_traspaso,COUNT(*)
      FROM   safre_tmp:taa_cd_det_sol_tra_ind
      GROUP  BY   1
      ORDER  BY   1;
    FOREACH   c_tipo_trasp     INTO   l_origen,l_registros
        LET   i                      =  i        +  1
        LET   ltp[i].origen          =  l_origen
        LET   ltp[i].registros       =  l_registros
    END  FOREACH
    IF      ltp[1].registros     >  0   THEN
      DISPLAY "Origen ",ltp[1].origen,":",ltp[1].registros USING "####&" AT 12,1
    END IF
    IF      ltp[2].registros     >  0   THEN
      DISPLAY "Origen ",ltp[2].origen,":",ltp[2].registros USING "####&" AT 13,1
    END IF
    IF      ltp[3].registros     >  0   THEN
      DISPLAY "Origen ",ltp[3].origen,":",ltp[3].registros USING "####&" AT 14,1
    END IF
    IF      ltp[4].registros     >  0   THEN
      DISPLAY "Origen ",ltp[4].origen,":",ltp[4].registros USING "####&" AT 12,20
    END IF
    IF      ltp[5].registros     >  0   THEN
      DISPLAY "Origen ",ltp[5].origen,":",ltp[5].registros USING "####&" AT 13,20
    END IF
    IF      ltp[6].registros     >  0   THEN
      DISPLAY "Origen ",ltp[6].origen,":",ltp[6].registros USING "####&" AT 14,20
    END IF
    IF      ltp[7].registros     >  0   THEN
      DISPLAY "Origen ",ltp[7].origen,":",ltp[7].registros USING "####&" AT 12,40
    END IF
    IF      ltp[8].registros     >  0   THEN
      DISPLAY "Origen ",ltp[8].origen,":",ltp[8].registros USING "####&" AT 13,40
    END IF
    IF      ltp[9].registros     >  0   THEN
      DISPLAY "Origen ",ltp[9].origen,":",ltp[9].registros USING "####&" AT 14,40
    END IF
    IF      ltp[10].registros     >  0   THEN
      DISPLAY "Origen ",ltp[10].origen,":",ltp[10].registros USING "####&" AT 12,60
    END IF
    IF      ltp[11].registros     >  0   THEN
      DISPLAY "Origen ",ltp[11].origen,":",ltp[11].registros USING "####&" AT 13,60
    END IF
    IF      ltp[12].registros     >  0   THEN
      DISPLAY "Origen ",ltp[12].origen,":",ltp[12].registros USING "####&" AT 14,60
    END IF
    LET      v_fecha_trasp   =  g_hoy
    LET      ejecuta     =  "EXECUTE PROCEDURE safre_af:marca_cuenta(",
                                    "?,?,?,?,?,?,?,?)"
    LET       ejecuta     =  ejecuta   CLIPPED
    PREPARE   spl_marca_cuenta     FROM  ejecuta
    LET       ejecuta      = 
             "EXECUTE  PROCEDURE sp_cierra_marca(","?,?)"
    LET       ejecuta      =  ejecuta  CLIPPED
    PREPARE   desmara_unificacion        FROM   ejecuta
    LET       txt  =  'INSERT INTO safre_af:taa_cd_cza_cedido '    ,   
                  'SELECT ',v_folio ,  ', '                 ,
                  'A.tipo_registro  ,     ' ,
                  'A.ident_servicio ,     ' ,
                  'A.ident_operacion ,    ' ,
                  'A.tipo_ent_origen ,    ' ,
                  'A.tipo_ent_destino ,   ' ,
                  'A.cve_ent_destino ,    ' ,
                  'MDY(A.fecha_presentacion[5,6],' ,
                  '    A.fecha_presentacion[7,8],' ,
                  '    A.fecha_presentacion[1,4]) , ',
                  'A.consec_lote_dia,     ' ,
                  'A.cve_mod_recepcion    ' ,
                  'FROM   safre_tmp:taa_cd_cza_sol_tra_ind A '    
    LET txt = txt CLIPPED
    PREPARE qry1 FROM txt
    EXECUTE qry1

    LET   txt   =  'SELECT ',v_folio     ,  ' ,',
                  'A.tipo_registro           ,',
                  'A.cont_servicio           ,',
                  'A.tipo_recep_cuenta       ,',
                  'A.cve_recep_cuenta        ,',
                  'A.tipo_ced_cuenta         ,',
                  'A.cve_ced_cuenta          ,',
                  'A.tipo_traspaso           ,',
                  'MDY(A.fecha_presentacion[5,6] ,',
                  '    A.fecha_presentacion[7,8] ,',
                  '    A.fecha_presentacion[1,4]),', 
                  'A.n_unico                 ,',
                  'A.n_seguro                ,',
                  '"n"                       ,',
                  'A.paterno                 ,',
                  'A.materno                 ,',
                  'A.nombre                  ,',
                  '"1"                       ,',
                  'MDY(A.fecha_recep_solic[5,6] , ',
                  '    A.fecha_recep_solic[7,8] , ',
                  '    A.fecha_recep_solic[1,4]), ',
                  'A.ident_lote_solici       ,',
                  'A.id_regimen_pension      ,',
                  'A.id_credito_fovissste    ,',
                  'A.id_credito_infonavit    ,',
                  'A.ind_175_smvdf            ',       #ACS --> 10-May-2012
                  'FROM   safre_tmp:taa_cd_det_sol_tra_ind A '  ,
                  'order by n_unico '

    LET txt = txt CLIPPED
    PREPARE qry2 FROM txt
    DECLARE cur_prueba CURSOR FOR qry2

    FOREACH cur_prueba INTO reg_soli.*
         LET      reg_soli.estado             =  101
         LET      reg_soli.fecha_trasp        =  reg_ctr_folio.fecha_liquidacion
         LET      reg_soli.cve_sector         =  0
         LET      reg_soli.rfc                =  " "
         
         #CPL-1377 Se valida que para la carga Mensual (periodo_carga = 1) solo se reciban solicitudes 12, 20, 24, 25, 51, 57, 83, 84 
         #en caso de ser Bimestral (periodo_carga = 2) se recibiran las diferentes a 12, 20, 24, 25, 51, 57, 83, 84  
         IF periodo_carga = 1 THEN
            IF reg_soli.tipo_traspaso     !=  "12"    AND
               reg_soli.tipo_traspaso     !=  "20"    AND
               reg_soli.tipo_traspaso     !=  "24"    AND
               reg_soli.tipo_traspaso     !=  "25"    AND
               reg_soli.tipo_traspaso     !=  "51"    AND
               reg_soli.tipo_traspaso     !=  "57"    AND
               reg_soli.tipo_traspaso     !=  "83"    AND
               reg_soli.tipo_traspaso     !=  "84"    AND
               reg_soli.tipo_traspaso     !=  "85"    THEN
                  
                  LET   reg_soli.estado    = 115
            END IF
         ELSE 
            IF reg_soli.tipo_traspaso      =  "12"    OR
               reg_soli.tipo_traspaso      =  "20"    OR
               reg_soli.tipo_traspaso      =  "24"    OR
               reg_soli.tipo_traspaso      =  "25"    OR
               reg_soli.tipo_traspaso      =  "51"    OR
               reg_soli.tipo_traspaso      =  "57"    OR
               reg_soli.tipo_traspaso      =  "83"    OR
               reg_soli.tipo_traspaso      =  "84"    AND
               reg_soli.tipo_traspaso      =  "85"    THEN
                  
                  LET   reg_soli.estado    = 115  
            END IF
         END IF
         
         IF    reg_soli.estado    != 115 THEN
            ################################################################################################
            IF       reg_soli.tipo_traspaso      =  "71"    OR
                     reg_soli.tipo_traspaso      =  "72"    OR
                     reg_soli.tipo_traspaso      =  "73"    OR
                     reg_soli.tipo_traspaso      =  "74"    OR
                     reg_soli.tipo_traspaso      =  "83"    OR
                     reg_soli.tipo_traspaso      =  "84"    OR
                     reg_soli.tipo_traspaso      =  "85"    THEN
                     LET      reg_soli.n_seguro_cedente    =  reg_soli.n_seguro
                     SELECT   COUNT(*)
                       INTO   l_existe
                       FROM   afi_mae_afiliado
                      WHERE   n_unico            =  reg_soli.n_unico
                        AND   tipo_solicitud     IN(1,8,12,15,16,17,18)
                        AND   n_seguro NOT IN (SELECT nss 
                                               FROM   cta_act_marca 
                                               WHERE  marca_cod IN (120,130,140,150)
                                               AND    nss IN (SELECT n_seguro 
                                                              FROM   afi_mae_afiliado
                                                              WHERE  n_unico =  reg_soli.n_unico)); #CPL-1419 Se agrega el tipo de solicitud 17
                    IF        l_existe           =  1     THEN
                              SELECT   MAX(n_seguro)
                                INTO   reg_soli.n_seguro
                                FROM   afi_mae_afiliado
                               WHERE   n_unico            =  reg_soli.n_unico
                                 AND   tipo_solicitud     IN(1,8,12,15,16,17,18)
                                 AND   n_seguro not IN (SELECT nss 
                                                        FROM cta_act_marca 
                                                        WHERE marca_cod IN (120,130,140,150) 
                                                        AND   nss IN (SELECT n_seguro 
                                                                      FROM   afi_mae_afiliado
                                                                      WHERE   n_unico =  reg_soli.n_unico));
                    ELSE
                             LET   reg_soli.estado    = 114
                             LET   reg_soli.n_seguro  = reg_soli.n_seguro_cedente
                    END IF
            END IF 
            
            ## Se busca el afiliado por nss y/o curp
            IF    reg_soli.tipo_traspaso      =  "01"    OR
                  reg_soli.tipo_traspaso      =  "02"    OR
                  reg_soli.tipo_traspaso      =  "12"    OR
                  reg_soli.tipo_traspaso      =  "20"    OR
                  reg_soli.tipo_traspaso      =  "21"    OR
                  reg_soli.tipo_traspaso      =  "24"    OR
                  reg_soli.tipo_traspaso      =  "25"    OR
                  reg_soli.tipo_traspaso      =  "38"    OR
                  reg_soli.tipo_traspaso      =  "51"    OR
                  reg_soli.tipo_traspaso      =  "55"    OR
                  reg_soli.tipo_traspaso      =  "57"    THEN
                
               IF reg_soli.n_seguro IS NULL OR  # MLM-1442 Se modifica la forma en la que se busca la cuenta individual
                  reg_soli.n_seguro = "           " THEN
                     SELECT COUNT(*)                                    
                      INTO  l_existe                                      
                      FROM  afi_mae_afiliado                              
                     WHERE  n_unico           =  reg_soli.n_unico; 
               ELSE
                   SELECT COUNT(*)                                    
                     INTO  l_existe                                      
                     FROM  afi_mae_afiliado                              
                    WHERE  n_seguro          =  reg_soli.n_seguro;
               END IF       
               
               
               IF        l_existe          <>  1     THEN
                  LET   reg_soli.estado    = 114
               END IF   
            END IF    
                  
            IF   reg_soli.tipo_traspaso         =  "38"    OR     
                 reg_soli.tipo_traspaso         =  "57"    THEN     
                 INSERT INTO  safre_af:taa_cd_correo_elect
                        VALUES (  reg_soli.folio,
                                  reg_soli.n_seguro,
                                  l_correo_clip,
                                  l_correo_soli_tra);
                 SELECT  n_folio,tipo_solicitud 
                   INTO  l_n_folio,l_tipo_solicitud
                   FROM  safre_af:afi_mae_afiliado
                  WHERE  n_seguro       =  reg_soli.n_seguro;
                 SELECT  UNIQUE  "1"         INTO    l_tiene_correo
                   FROM  safre_af:afi_correo_elect
                  WHERE  nss              =  reg_soli.n_seguro
                    AND  cod_correo_e    IN(1,2);
                 IF      l_tiene_correo          THEN
                         UPDATE   safre_af:afi_correo_elect
                            SET   correo_elect          =  l_correo_clip,
                                  factualiza            =  g_hoy,
                                  usuario               =  vusuario
                          WHERE   nss                   =  reg_soli.n_seguro
                            AND   cod_correo_e         IN(1,2);
                 ELSE
   
                         INSERT  INTO  safre_af:afi_correo_elect
                                 VALUES ( 
                                  reg_soli.n_seguro,
                                  l_n_folio,
                                  l_tipo_solicitud,
                                  "1",
                                  l_correo_clip,
                                  "X",
                                  reg_soli.folio,
                                  g_hoy,
                                  vusuario);
                        INSERT  INTO  safre_af:afi_correo_elect
                                VALUES ( 
                                         reg_soli.n_seguro,
                                         l_n_folio,
                                         l_tipo_solicitud,
                                         "2",
                                         l_correo_soli_tra,
                                         "X",
                                         reg_soli.folio,
                                         g_hoy,
                                         vusuario);
                 END IF
            END IF
            INITIALIZE marca_entra TO NULL
            LET      l_ind_cierre_marca      =  0
            SELECT  A.marca_cod,A.ind_cierre_marca 
              INTO  marca_entra,l_ind_cierre_marca
              FROM  safre_af:taa_cd_tipo_traspaso A
             WHERE  A.tipo_traspaso = reg_soli.tipo_traspaso
            IF      marca_entra IS NULL THEN 
                    IF    g_pid IS NULL THEN
                          PROMPT   "TIPO TRASPASO : ",reg_soli.tipo_traspaso, 
                                   " NO EXISTE EN CATALOGO <Enter> PARA CONTINUAR" 
                                    FOR  CHAR  enter 
                          PROMPT   "CONTACTAR CON SISTEMAS...<Enter> PARA SALIR" 
                                    FOR  CHAR  enter
                   ELSE
                          DISPLAY  "Program stopped" 
                          DISPLAY  "ERROR TIPO TRASPASO ",reg_soli.tipo_traspaso,
                                   " NO EXISTE EN CATALOGO..."
                   END IF
            END IF
            CALL      detecta_fusion(reg_soli.*)
            RETURNING  b_fusion
            IF       b_fusion   =  uno   THEN
                     CONTINUE FOREACH
            END IF
         END IF #  reg_soli.estado    = 115

         LET      vmarca_causa      =  cero
          LET      vestado_marca     =  cero
          LET      vcodigo_rechazo   =  cero
          LET      vcodigo_rechazo   =  cero
          LET      vfecha_causa      =  NULL
          IF       reg_soli.estado        =  101      THEN
                   
                   IF       l_ind_cierre_marca     =  1  THEN
                            EXECUTE   desmara_unificacion       USING
                                      reg_soli.n_seguro,
                                      reg_soli.tipo_traspaso
                   END IF
                   IF       reg_soli.ident_lote_solici  [3,5]  =
                            reg_soli.cve_ced_cuenta     THEN
                            LET       reg_soli.estado     =  107
                   END IF
                   IF       reg_soli.estado        =  101      THEN
                           DECLARE  cur_marca_cuenta  CURSOR  FOR spl_marca_cuenta
                           OPEN     cur_marca_cuenta     USING   
                                    reg_soli.n_seguro,
                                    marca_entra,     
                                    v_folio   ,    # envia folio como correlativo
                                    vestado_marca, # regresa(0 ok. o 30 rechazo )
                                    vcodigo_rechazo, # envia cod_rech en 0           
                                    vmarca_causa,  # envia 0
                                    vfecha_causa,  # envia  NULLA
                                    vusuario       # envia  usuario de sesion
                           FETCH    cur_marca_cuenta 
                                    INTO  xcodigo_marca,xcodigo_rechazo
                                    IF    xcodigo_rechazo       <>  0    THEN
                                          LET  reg_soli.estado = xcodigo_rechazo
                                          INSERT   INTO   taa_cd_rch_convivencia
                                                 VALUES(reg_soli.folio         ,
                                                        reg_soli.n_seguro      ,
                                                        reg_soli.n_unico       ,
                                                        reg_soli.cont_servicio ,
                                                        xcodigo_rechazo) 
                                    #ELSE
                                    #	   CALL     F_520_checa_salarios_minimos()  # CPL-1889 Se omite la validación de los SM
                                    END IF
                           CLOSE     cur_marca_cuenta
                   END IF
            END IF
            INSERT    INTO     safre_af:taa_cd_det_cedido
                      VALUES (reg_soli.*)
            LET       g_registro     =  rfolio  CLIPPED, '|'  CLIPPED,
                                        reg_soli.n_unico, '|',
                                        reg_soli.n_seguro,'|',
                                        reg_soli.tipo_traspaso, '|',
                                        reg_soli.paterno   CLIPPED,'|',
                                        reg_soli.materno   CLIPPED,'|',
                                        reg_soli.nombre    CLIPPED
            OUTPUT    TO  REPORT  f_250_imprime_detalle() 
    END FOREACH
    LET    txt    =   'INSERT  INTO   safre_af:taa_cd_sum_cedido '    ,      
                      'SELECT ',v_folio ,', A.* '               ,
                        'FROM   safre_tmp:taa_cd_sum_sol_tra_ind A '
    LET    txt    =  txt CLIPPED
    PREPARE   qry3    FROM  txt
    EXECUTE   qry3
    FINISH    REPORT    f_250_imprime_detalle
END FUNCTION 

######################################################################
#Se suman todos los montos de todas las subcuentas.
FUNCTION    F_520_checa_salarios_minimos()
   DEFINE   l_num_salarios                INTEGER,
            l_siefore                     ,
            l_scta                        SMALLINT,
            l_pesos                       ,
            l_tot_pesos                   DEC(16,6)
            
   # CPL-1333 Se agrega la validación solo para los tipos de Traspaso 01,51 y 71         
   IF       reg_soli.tipo_traspaso        =  "01"      OR
            reg_soli.tipo_traspaso        =  "51"      OR
            reg_soli.tipo_traspaso        =  "71"      THEN
      
      
      LET      l_num_salarios = 0
      LET      l_tot_pesos = 0
      DECLARE  c_salarios   CURSOR  FOR  sql_salarios
      FOREACH  c_salarios    USING  reg_soli.n_seguro
                              INTO  l_siefore, l_pesos
               LET l_num_salarios       =  l_pesos     /  g_salario_diario
               LET l_tot_pesos = l_tot_pesos + l_pesos
                        
      END FOREACH
   
       LET l_num_salarios       =  l_tot_pesos     /  g_salario_diario
       LET l_siefore = 99 LET l_scta = 99
       
       IF l_num_salarios > 175 THEN
             INSERT   INTO    safre_tmp:taa_cd_num_salarios
                    VALUES  (v_folio,
                             reg_soli.n_seguro,
                             l_siefore,
                             l_scta,
                             l_tot_pesos,
                             l_num_salarios);
   
          LET      reg_soli.estado       =  105
       END IF
    END IF
   
END FUNCTION

FUNCTION imprime_reporte()
#ir-----------------------

   DEFINE  datos           RECORD
           cve_recep                   CHAR(003),
           des_afore                   CHAR(025),
           tipo_trasp                  CHAR(002),
           tot_parcial                 SMALLINT
   END RECORD

   DEFINE  aux_pausa                   CHAR(001)
   DEFINE  tot                         INTEGER 
   DEFINE  g_afore           RECORD  LIKE  safre_af:tab_afore_local.*
   DEFINE  r_det RECORD
            nss          CHAR(11),
            afore        VARCHAR(30),
            nombre       VARCHAR(120),
            fecha        DATE,
            pesos        DEC(16,6)
   END RECORD,
            l_query      CHAR(500)

    LET l_query = " "   --acs
   SELECT  COUNT(*)
   INTO    tot
   FROM    safre_af:taa_cd_det_cedido A
   WHERE   A.folio = v_folio

   IF      tot       =  0    THEN
           ERROR  "NO EXISTEN DATOS PARA EL FOLIO <Enter> Salir .."
           PROMPT  " "  FOR  enter
           EXIT PROGRAM 
   END IF 
   INITIALIZE  datos.*        TO  NULL
   SELECT  *
     INTO  g_afore.*
     FROM  safre_af:tab_afore_local

   LET     g_lista       =  g_parametros.ruta_listados CLIPPED, "/",
           vusuario CLIPPED,"SolTrasp.",g_hoy USING "DDMMYY",
           hora[1,2],hora[4,5],hora[7,8]
   START REPORT listado TO g_lista
       DECLARE cur1 CURSOR FOR

       SELECT  A.ident_lote_solici[3,5]  ,   
               B.afore_desc        ,     
               A.tipo_traspaso     , 
               COUNT(*)
         FROM  safre_af:taa_cd_det_cedido A, 
               OUTER    safre_af:tab_afore B
        WHERE  A.folio                     =  v_folio
        AND    A.ident_lote_solici[3,5]    =  B.afore_cod
        GROUP  BY 1,2,3
        ORDER  BY 1,3

        IF     STATUS             =  NOTFOUND  THEN 
               IF     g_pid IS NULL THEN
                      ERROR "NO HAY DATOS ..." SLEEP 2
               ELSE
                      DISPLAY "Program stopped" 
                      DISPLAY "ERROR NO SE IMPRIME REPORTE" SLEEP 2
               END IF
        ELSE  
               FOREACH  cur1      INTO   datos.*
                      LET     tot_total    =  tot_total   +   datos.tot_parcial
                      OUTPUT    TO  REPORT  listado(datos.*)  #l
               END FOREACH 
        END IF

   FINISH REPORT listado

--Se agrega reporte para registros con + de 175 salarios minimos
--ACS_Sep/2011

   LET     g_lista2       =  g_parametros.ruta_listados CLIPPED, "/",
           vusuario CLIPPED,".DET_SolTrasp.",g_hoy USING "DDMMYY",
           hora[1,2],hora[4,5],hora[7,8]

   START REPORT rpt_detalle TO g_lista2

       LET l_query =
       " SELECT t.n_seguro, TRIM(a.afore_desc),",
       " TRIM(t.paterno) || ' ' || TRIM(t.materno) || ' ' || TRIM(t.nombre),",
       " t.fecha_recep_solic, SUM(s.pesos)",
       " FROM  safre_af:taa_cd_det_cedido t, safre_tmp:taa_cd_num_salarios s,",
       " safre_af:tab_afore a ",
       " WHERE  t.folio = s.folio",
       " AND t.n_seguro = s.nss",
       " AND t.ident_lote_solici[3,5] = a.afore_cod",
       " AND t.folio = ",v_folio ,
       " GROUP BY 1,2,3,4"


--DISPLAY "l_q ", l_query SLEEP 2

       PREPARE p_det FROM l_query
       DECLARE cur_det CURSOR FOR p_det

    INITIALIZE r_det.* TO NULL
    FOREACH cur_det INTO r_det.*
       OUTPUT TO REPORT rpt_detalle(r_det.*)
    END FOREACH

  FINISH REPORT rpt_detalle


END FUNCTION

REPORT listado(datos)
#l-------------------
    
   DEFINE  datos       RECORD
           cve_recep                       CHAR(3),
           des_afore                       CHAR(25),
           tipo_trasp                      CHAR(2),
           tot_parcial                     SMALLINT
   END RECORD

   DEFINE  l_total_tipo_trasp              INTEGER
   DEFINE  l_total_folio                   INTEGER
   DEFINE  l_tipo_traspaso                 CHAR(02),
           l_estado                        SMALLINT,
           l_desc_edo                      CHAR(25)

   DEFINE  campo       RECORD
           afore_cod                       SMALLINT,
           raz_social                      CHAR(50) 
    END RECORD

    OUTPUT
    PAGE LENGTH 60
    LEFT MARGIN 0
    RIGHT MARGIN 132
    TOP MARGIN 0
    BOTTOM MARGIN 0

    FORMAT
	PAGE HEADER
	  --PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
	    PRINT  COLUMN 67,"Pagina:",PAGENO USING "<<<<"
	    PRINT  COLUMN 2,"TCAAC006",
		   COLUMN 10,"SOLICITUD DE TRASPASO  AFORE / AFORE (CEDENTE)",
                   COLUMN 58,"FECHA:",
		   COLUMN 65, g_hoy USING "dd-mm-yyyy"
	    PRINT  COLUMN 02,"___________________________________________________________________________________________________________"

            SELECT  A.codigo_afore,
                    A.razon_social
              INTO  campo.afore_cod,
                    campo.raz_social
              FROM  safre_af:tab_afore_local A

            PRINT COLUMN 2,campo.afore_cod USING "&&&","     ",campo.raz_social
            PRINT COLUMN 2,"NOMBRE DEL ARCHIVO : ",nom_archivo
            PRINT COLUMN 2,"FECHA DE PRESENTACION : ",vfecha_presentacion USING"DD-MM-YYYY"
	    PRINT COLUMN 02,"FOLIO:",v_folio
	    PRINT COLUMN 02,"___________________________________________________________________________________________________________"

    PRINT COLUMN 2,"CLAVE ",COLUMN 8,COLUMN 11,"RAZON SOCIAL",COLUMN 43,COLUMN 45, "ORIGEN",COLUMN 52,COLUMN 54,"REGISTROS SOLICITADOS",COLUMN 76

    PRINT COLUMN 02,"___________________________________________________________________________________________________________"
ON EVERY ROW

    PRINT COLUMN 2,datos.cve_recep USING "&&&",
          COLUMN 11,datos.des_afore,
          COLUMN 45,datos.tipo_trasp,
          COLUMN 54,datos.tot_parcial USING "#####&"

    IF lineno > 57 THEN
       SKIP TO TOP OF PAGE
    END IF

AFTER GROUP OF datos.cve_recep 
    PRINT COLUMN 02,"___________________________________________________________________________________________________________"
    PRINT COLUMN 2,"TOTAL ",datos.des_afore,
          COLUMN 54,GROUP SUM(datos.tot_parcial) USING "#####&"
    PRINT COLUMN 02,"___________________________________________________________________________________________________________"

    PRINT  "    <<   TOTALES  POR  TIPO  DE  TRASPASO    >> "
    ON  LAST ROW
          LET      l_total_folio       =  0
          DECLARE  tot_tipo_trasp    CURSOR  FOR
          SELECT   a.tipo_traspaso,a.estado,b.descripcion,COUNT(*)
          FROM     safre_af:taa_cd_det_cedido  a, OUTER  taa_cd_edo_cedente b
          WHERE    a.folio             =  v_folio
           AND     a.estado            =  b.estado
         GROUP     BY 1,2,3    ORDER  BY   1,2,3
         FOREACH   tot_tipo_trasp   INTO   l_tipo_traspaso,l_estado,
                                           l_desc_edo,l_total_tipo_trasp
               IF      l_desc_edo     IS  NULL    THEN
                       SELECT  marca_desc
                         INTO  l_desc_edo
                         FROM  tab_marca
                        WHERE  marca_cod         =  l_estado
               END IF
               PRINT   COLUMN  2,"ORIGEN_TRASPASO: ",l_tipo_traspaso,
                       COLUMN  25,"EDO: ",l_estado  USING  "####","  ",l_desc_edo,
                       COLUMN  54,l_total_tipo_trasp     USING  "#####&"
               LET     l_total_folio   =  l_total_folio  +  l_total_tipo_trasp
               PRINT   COLUMN 02,"___________________________________________________________________________________________________________"
          END FOREACH
          PRINT  COLUMN   2,"TOTAL REGISTROS CARGADOS==> ",
                 COLUMN  61,l_total_folio   USING   "#####&"

END REPORT

FUNCTION  detecta_fusion(reg_fusion)
#f---------------------------------

   DEFINE  reg_fusion RECORD LIKE safre_af:taa_cd_det_cedido.*
   DEFINE  cve_act                   CHAR(003)
   DEFINE  band                      INTEGER
   DEFINE  l_cve_afo_recep           CHAR(03)

   LET     l_cve_afo_recep          =  reg_fusion.ident_lote_solici[3,5]

   SELECT  a.cve_ced_cuenta_act
   INTO    cve_act
   FROM    safre_af:taa_cd_fusion_afore a
   WHERE   a.cve_ced_cuenta_org     =  l_cve_afo_recep
   IF      cve_act                 <>  l_cve_afo_recep      THEN

           LET     reg_fusion.estado      =  fusion

           INSERT  INTO  safre_af:taa_cd_det_cedido
                 VALUES (reg_fusion.*)

           INSERT  INTO  safre_af:taa_cd_rch_convivencia    
                 VALUES (reg_fusion.folio         ,
                         reg_fusion.n_seguro      ,
                         reg_fusion.n_unico       ,
                         reg_fusion.cont_servicio ,
                         fusion) 

          LET     band                    =  uno
          RETURN  band
   ELSE
          LET     band                    =  cero
          RETURN  band 
   END IF
END FUNCTION

REPORT   f_250_imprime_detalle()
    OUTPUT
        TOP MARGIN    0
        BOTTOM MARGIN 0
        LEFT MARGIN   0
    FORMAT
    ON EVERY ROW
        PRINT COLUMN  001, g_registro
END REPORT

############################################################
REPORT rpt_detalle (p_det)
   DEFINE  p_det RECORD
            nss          CHAR(11),
            afore        VARCHAR(30),
            nombre       VARCHAR(120),
            fecha        DATE,
            pesos        DEC(16,6)
   END RECORD
      DEFINE  campo       RECORD
           afore_cod                       SMALLINT,
           raz_social                      CHAR(50) 
    END RECORD

    OUTPUT
    PAGE LENGTH 60
    LEFT MARGIN 0
    RIGHT MARGIN 0
    TOP MARGIN 0
    BOTTOM MARGIN 0

 FORMAT
  PAGE HEADER
      PRINT  COLUMN 67,"Pagina:",PAGENO USING "<<<<"
      PRINT  COLUMN 2,"TCAAC006",
       COLUMN 12,"SOLICITUD DE TRASPASO  AFORE / AFORE (CEDENTE)",
       COLUMN 60,"FECHA: ",g_hoy USING "dd-mm-yyyy"
      PRINT  COLUMN 2, "DETALLE DE REGISTROS CON MAS DE 175 SALARIOS MINIMOS"
      PRINT  COLUMN 02,"___________________________________________________________________________________________________________"

            SELECT  A.codigo_afore, A.razon_social
              INTO  campo.afore_cod, campo.raz_social
              FROM  safre_af:tab_afore_local A

            PRINT COLUMN 2,campo.afore_cod USING "&&&","     ",campo.raz_social
	    PRINT COLUMN 02,"___________________________________________________________________________________________________________"

    PRINT COLUMN   2, "NSS",
          COLUMN  15, "AFORE SOL.",
          COLUMN  35, "NOMBRE",
          COLUMN  72, "FECHA SOL.",
          COLUMN  87, "MONTO_PESOS"

    PRINT COLUMN 02,"___________________________________________________________________________________________________________"
ON EVERY ROW
    
     PRINT COLUMN  2, p_det.nss,
           COLUMN 15, p_det.afore[1,19],
           COLUMN 35, p_det.nombre[1,36],
           COLUMN 72, p_det.fecha      USING "dd-mm-yyyy",
           COLUMN 87, p_det.pesos      USING "#########&.#####&"

END REPORT 
