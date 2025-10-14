#####################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => TCAAC011                                      #
#Descripcion       => CARGA MASIVA DE PRECIOS DE ACCION             #
#Fecha             => 10  julio  07.                                #
#Autor             => JOSE FRANCISCO LUGO CORNEJO                   #
#Sistema           => TAAI  (TRASPASOS AFORE CEDENTE)               #
#####################################################################
DATABASE  safre_af    
GLOBALS
    DEFINE 
            g_fecha_valuacion                      ,
            g_today                                DATE
    DEFINE  
            g_ejecuta                              CHAR(500) ,
            g_enter                                CHAR(001) ,
            nom_archivo                            CHAR(060) ,
            g_usuario                              CHAR(008) ,
            g_hora                                 CHAR(008)
    DEFINE  
            g_seg_modulo                   RECORD  LIKE  safre_af:seg_modulo.*
    DEFINE  afo_local                              RECORD
            afore_cod                              CHAR(03),
            raz_social                             CHAR(50)
                                               END RECORD
    DEFINE  
            g_num_afore                            ,
            g_afore                                ,
            g_siefore                              SMALLINT,
            g_precio_accion                        DEC(10,6)

   DEFINE   g_total_archivo                        ,
            g_total_grabados                       ,
            g_total_procesados                     DECIMAL(16)
END GLOBALS

MAIN 
   OPTIONS 
            INPUT    WRAP          ,
            PROMPT   LINE  LAST ,   
            ACCEPT   KEY   CONTROL-I
#           DEFER    INTERRUPT
   CALL     f_010_inicio()
   CALL     f_100_proceso()
END MAIN

FUNCTION    f_010_inicio()
   CALL     STARTLOG("TCAAC011.log")
   LET      g_hora                      =  TIME
   LET      g_today                     =  TODAY
   SELECT   user
     INTO   g_usuario
     FROM   safre_af:tab_afore_local ;
   SELECT   a.*
     INTO   g_seg_modulo.*
     FROM   safre_af:seg_modulo a
    WHERE   a.modulo_cod                =  "taa";
   CREATE   TEMP  TABLE  arch_valuacion ( registro   CHAR(1000))
   LET      g_ejecuta        =  "ls ",g_seg_modulo.ruta_rescate CLIPPED,"/",
            " > ",g_seg_modulo.ruta_rescate   CLIPPED,"/arch_valuacion"
   RUN      g_ejecuta
   LET      g_ejecuta        =  g_seg_modulo.ruta_rescate CLIPPED,"/",
                               "arch_valuacion"
   LOAD     FROM   g_ejecuta  INSERT  INTO    arch_valuacion;
END FUNCTION

FUNCTION    f_100_proceso()
   OPEN     WINDOW  taac001  AT  2,2  WITH  FORM  "TCAAC011" ATTRIBUTE(BORDER)
   DISPLAY  " <Esc>Ejecutar      <Ctrl-c>Salir                    ",
            "                            "   AT   1,1   ATTRIBUTE(REVERSE)
   DISPLAY  " TCAAC011        CARGA PRECIOS DE ACCION DE TODAS LAS AFORES  ",
            "                            "   AT   3,1   ATTRIBUTE(REVERSE)
   DISPLAY  "              PARA CALCULO DE RENDIMIENTOS POR TRASPASO INDEBIDO",
            "                            "   AT   4,1   ATTRIBUTE(REVERSE)
   DISPLAY  " El Archivo lo toma de /safre_prc/taa/rescate           " AT 6,1
   INPUT  BY  NAME      nom_archivo    WITHOUT DEFAULTS
         AFTER FIELD    nom_archivo
                  IF       nom_archivo      IS  NULL  THEN 
                           ERROR    "ARCHIVO NO VALIDO... TECLEAR",
                                    " NUEVAMENTE NOMBRE DEL ARCHIVO ..."
                           NEXT FIELD    nom_archivo
                  END  IF
                  SELECT   "OK"
                    FROM   safre_tmp:arch_valuacion 
                   WHERE   registro         =  nom_archivo
                  IF       STATUS           =  NOTFOUND     THEN
                           ERROR     "ARCHIVO  INVALIDO......  TECLEAR",
                                     " NUEVAMENTE NOMBRE DEL ARCHIVO ......   "
                           NEXT FIELD    nom_archivo
                  END IF
         ON KEY (ESC)
                  IF       nom_archivo      IS  NULL    THEN
                           ERROR "ARCHIVO  NO  VALIDO... CORRIJA  NOMBRE  " 
                           NEXT FIELD    nom_archivo
                  END IF
                  SELECT   "OK"
                    FROM   safre_tmp:arch_valuacion
                   WHERE   registro              =  nom_archivo
                  IF       STATUS                =  NOTFOUND     THEN
                           ERROR    "ARCHIVO NO VALIDO... TECLEAR NOMBRE",
                                    " DE ARCHIVO VALIDO ......."
                           NEXT FIELD      nom_archivo
                  END IF
                  DISPLAY  "    Procesando Informacion.....                 ",
                           "                                     "    AT   19,1
                           ATTRIBUTE(REVERSE)
                  LET      nom_archivo           =  nom_archivo CLIPPED
                  CALL     f_110_carga_precios()
                  EXIT  INPUT
        ON KEY(CONTROL-C) 
                  PROMPT   " PROCESO CANCELADO...<ENTER> PARA SALIR ?" 
                           FOR     g_enter
                  EXIT PROGRAM
        ON KEY(INTERRUPT) 
                  PROMPT   " PROCESO CANCELADO...<ENTER> PARA SALIR ?"
                           FOR     g_enter
                  EXIT PROGRAM
   END INPUT
   ERROR    "                                                 "
   PROMPT   "PROCESO FINALIZADO...TECLEE <ENTER> PARA  SALIR ?"   FOR  g_enter
END FUNCTION

FUNCTION    f_110_carga_precios()
   DEFINE   l_acc                        CHAR(30)
   DEFINE   l_ini                        ,
            l_fin                        ,
            l_ind                        SMALLINT  
   DEFINE   l_reg_valuacion              CHAR(1000)
   LET      g_total_archivo              =  0
   LET      g_total_procesados           =  0
   LET      g_total_grabados             =  0
   CREATE   TEMP  TABLE       valuacion ( registro       CHAR(1000) );
   LET      g_ejecuta                    =  g_seg_modulo.ruta_rescate  CLIPPED,
                                           "/",nom_archivo             CLIPPED
   LOAD     FROM   g_ejecuta  INSERT  INTO    valuacion;
   SELECT   COUNT(*) 
     INTO   g_total_archivo 
     FROM   valuacion;
   DISPLAY "   FECHAS A PROCESAR          :",
            g_total_archivo  USING  "###,###,###"   AT  12,2
   DECLARE  cur_valuacion     CURSOR  FOR
   SELECT   *
     FROM   valuacion;
   FOREACH  cur_valuacion     INTO    l_reg_valuacion
        LET      g_total_procesados          =  g_total_procesados    +  1
        DISPLAY  "   FECHAS  PROCESADAS         :",
                  g_total_procesados     USING  "###,###,###"   AT   13,2
        IF       l_reg_valuacion[13]        <>  ","    THEN
                 ERROR   "  ERROR  DE ESTRUCTURA EN REGISTRO  ",
                            g_total_procesados
                 CONTINUE  FOREACH
        END IF
        LET      g_num_afore                 =  0
        LET      l_ini                       =  14
        LET      l_fin                       =  14
        LET      g_fecha_valuacion           =  l_reg_valuacion[1,10]
        LET      g_siefore                   =  l_reg_valuacion[12]
        LET      g_precio_accion             =  0
        FOR      l_fin                       =  l_fin       TO   1000
                 IF      l_reg_valuacion[l_fin]           =  " "    OR
                         l_reg_valuacion[l_fin]           =  ","    THEN
                         IF       l_reg_valuacion[l_fin - 1]      = ","   THEN
                                  LET      g_precio_accion        =  0 
                         ELSE 
                                  LET      g_precio_accion        =
                                          l_reg_valuacion[l_ini, l_fin - 1] 
                         END IF
                         LET      l_ini                    =  l_fin        +  1
                         LET      g_num_afore              =  g_num_afore  +  1
                         CALL     f_120_insert_precios_accion()
                         LET      g_total_grabados         =
                                  g_total_grabados         +  1
                         IF       l_reg_valuacion[l_ini  +  3]   =  "    "  THEN
                                  CONTINUE  FOREACH
                         END IF
                 ELSE 
                         CONTINUE FOR
                 END IF
        END FOR 
   END FOREACH
   DISPLAY "   PRECIOS DE ACCION GRABADOS :",
            g_total_grabados    USING  "###,###,##&"   AT  14,2
END FUNCTION

FUNCTION    f_120_insert_precios_accion()
   DEFINE   l_afore_cod                   SMALLINT
   LET      l_afore_cod               =  0
   SELECT   afore_cod
     INTO   g_afore
     FROM   safre_af:taa_cd_afores_inde
    WHERE   num_afore                 =  g_num_afore;
   DELETE   FROM   safre_af:tab_siefore_sistema
    WHERE   fecha_valuacion           =  g_fecha_valuacion
      AND   afore_cod                 =  g_afore
      AND   siefore_cod               =  g_siefore;
   INSERT   INTO     safre_af:tab_siefore_sistema
            VALUES  (g_fecha_valuacion,
                     g_afore,
                     g_siefore,
                     g_precio_accion
                    );
END FUNCTION
