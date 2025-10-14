#####################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                 
#Propietario       => E.F.P.                                      
#Programa          => TCREABOP01                                    
#Descripcion       => CARGA Y VALIDACION SOLICITUDES REASIGNACION REQ- CPL-693
#Fecha             => 24 FEBRERO 2003        .                
#Por               => JESUS DAVID YAÒEZ MORENO               
#Sistema           => TCAA (CEDENTE) 
#####################################################################

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
           g_tipo_traspaso            SMALLINT

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

   DEFINE  reg_soli          RECORD  LIKE  safre_af:taa_cd_soli_rea.*,
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

    CALL STARTLOG("TCREABOP01.log")

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

        OPEN WINDOW taac0061 AT 2,2 WITH FORM "TCREABOP01" ATTRIBUTE(BORDER)      
        DISPLAY " <Esc>Ejecutar      <Ctrl-c>Salir                                             " AT 1,1 ATTRIBUTE(REVERSE)
        DISPLAY g_hoy  USING"DD-MM-YYYY" AT 1,65 ATTRIBUTE(REVERSE) 
        DISPLAY "<TCREABOP01>  RECEPCION DE ARCHIVO DE SOLICITUDES TRASPASO AFORE-AFORE CEDENTE              " AT 2,1 ATTRIBUTE(REVERSE)


        INPUT BY NAME nom_archivo WITHOUT DEFAULTS                                  
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

        ON KEY (ESC)
                  CALL sube_historico()  #borra
 
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
        FROM   taa_cd_soli_rea  a
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
        DISPLAY " TCREABOP01  RECEPCION DE ARCHIVO DE SOLICITUDES AFO-AFO (CEDENTE)            "
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
      FROM   safre_af:taa_cd_soli_rea
      WHERE  folio = v_folio
      DISPLAY "TOTAL REGISTROS CARGADOS   : ",cuenta USING"##########&"
      DISPLAY "FIN DE PROGRAMA  TCREABOP01  : "

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
    CALL    STARTLOG("TCREABOP01.log") 
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
   DEFINE   l_sal_vol                     DEC(16,6)
   DEFINE   l_per_pago                    CHAR(06)

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
   DISPLAY "         REGISTROS A PROCESAR POR ORIGEN DE TRASPASO                     " AT 11,1 ATTRIBUTE(REVERSE)
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
                  'A.id_credito_infonavit     ',
                  'FROM   safre_tmp:taa_cd_det_sol_tra_ind A '  ,
                  'order by n_unico '

    LET txt = txt CLIPPED
    PREPARE qry2 FROM txt

   LET       txt         =  'SELECT   SUM(monto_en_acciones)          ',
                             '   FROM   safre_tmp:tmp_saldo_anexo71     ',
                             '  WHERE   nss            =  ?  ',
                    '    AND  subcuenta  IN(3,10,11,12,15,16,20,21,22, ',
                             '   23,24,25,26,27,28,29,37); '

   PREPARE   sal_vol         FROM  txt

   LET       txt         =  'SELECT   MAX(periodo_pago)          ',
                             '   FROM   dis_det_aporte     ',
                             '  WHERE   n_seguro            =  ?  '
   PREPARE   per_pago    FROM  txt

   DECLARE cur_prueba CURSOR FOR qry2

   FOREACH cur_prueba INTO reg_soli.*
       LET      reg_soli.estado             =  101
       LET      reg_soli.fecha_trasp        =  reg_ctr_folio.fecha_liquidacion
       LET      reg_soli.cve_sector         =  0
       LET      reg_soli.rfc                =  " "
       INITIALIZE marca_entra TO NULL
       LET      l_ind_cierre_marca      =  0
       LET      marca_entra       =  "290"
       LET      vmarca_causa      =  cero
       LET      vestado_marca     =  cero
       LET      vcodigo_rechazo   =  cero
       LET      vcodigo_rechazo   =  cero
       LET      vfecha_causa      =  NULL
 
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
                END IF
       CLOSE     cur_marca_cuenta
 

       IF       reg_soli.estado         =  101     THEN
                LET      l_per_pago       =  NULL
                EXECUTE  per_pago  USING  reg_soli.n_seguro
                                    INTO  l_per_pago
                IF       l_per_pago        IS   NULL    OR
                         l_per_pago        =  "      "  OR
                         l_per_pago        =  ""        THEN
                         LET      l_per_pago       =  "200101"
                END IF
                IF       l_per_pago        >  "201106"      THEN
                         LET      reg_soli.estado     =  107
                END IF
                IF       reg_soli.estado         =  101     THEN
                         LET      l_sal_vol        =  0
                         EXECUTE  sal_vol      USING  reg_soli.n_seguro
                                                INTO  l_sal_vol
                         IF       l_sal_vol         IS   NULL    THEN
                                  LET      l_sal_vol      =  0
                         END IF
                         IF       l_sal_vol        >  0    THEN
                                  LET      reg_soli.estado     =  105
                         END IF
                END IF
       END IF

       INSERT    INTO     safre_af:taa_cd_soli_rea
                   VALUES (reg_soli.*)
{
display 'nss=',reg_soli.n_seguro, '  l_estado=', reg_soli.estado,' l_per_pago=',l_per_pago  ,  '    l_sal_vol=',l_sal_vol
prompt '  '  for  enter
}

    END FOREACH
    LET    txt    =   'INSERT  INTO   safre_af:taa_cd_sum_cedido '    ,      
                      'SELECT ',v_folio ,', A.* '               ,
                        'FROM   safre_tmp:taa_cd_sum_sol_tra_ind A '
    LET    txt    =  txt CLIPPED
    PREPARE   qry3    FROM  txt
    EXECUTE   qry3
END FUNCTION 


