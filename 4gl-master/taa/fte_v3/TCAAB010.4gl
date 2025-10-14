###########################################################################
#Proyecto           => AFORE ( MEXICO )                                   #
#Propietario        => E.F.P.                                             #
#Programa TCAAB010  => LIQUIDACION AFORE - AFORE CEDENTE                  #
#Autor              => JOSE FRANCISCO LUGO CORNEJO                        #
#Fecha              => 13 JUNIO  2004                                     #
#Sistema            => TCAA                                               #
###########################################################################

DATABASE   safre_af

GLOBALS
   DEFINE
           g_provisionado                  ,  
           g_liquidado                     ,  
           g_cero                          ,
           g_siefore_max                   SMALLINT
   DEFINE
           g_procesados                    INTEGER
   DEFINE
           g_nom_tab_taa_cd_det            CHAR(040),  
           g_nss                           CHAR(011),
           g_enter                         CHAR(001),
           g_ejecuta                       CHAR(300)

   DEFINE  taa_cd_ctr_folio_ind            RECORD  LIKE  taa_cd_ctr_folio_ind.*,
           g_precio_acc_b1                 LIKE glo_valor_accion.precio_del_dia,
           g_precio_acc_b2                 LIKE glo_valor_accion.precio_del_dia,
           g_precio_acc_b3                 LIKE glo_valor_accion.precio_del_dia,
           g_precio_acc_b4                 LIKE glo_valor_accion.precio_del_dia,
           #g_precio_acc_b5                 LIKE glo_valor_accion.precio_del_dia,# CPL-1195
           g_precio_acc_b6                 LIKE glo_valor_accion.precio_del_dia,
           g_precio_parti                  LIKE glo_valor_accion.precio_del_dia
END GLOBALS

GLOBALS "GLOB_REPS.4gl"

MAIN
   OPTIONS
            PROMPT   LINE   LAST,
            ACCEPT   KEY    CONTROL-I
            DEFER    INTERRUPT
   CALL     STARTLOG('TCAAB010.log')
   CALL     F_001_inicio()
   CALL     F_500_proceso()
   CALL     F_900_fin()
END MAIN

FUNCTION    F_001_inicio()
   LET      hoy                      =  today 
   LET      g_cero                   =  0
   SELECT   *,user
     INTO   g_seg_modulo.*,g_usuario
     FROM   seg_modulo
    WHERE   modulo_cod               =  'taa';
   SELECT   a.estado
     INTO   g_provisionado
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion            =  'PROVISIONADA'
      AND   a.tipo                   =  3;
   SELECT   a.estado
     INTO   g_liquidado
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion            = 'LIQUIDADA'
      AND   a.tipo                   =  3;
   SELECT   MAX(codigo_siefore)   INTO  g_siefore_max  
     FROM   safre_af:tab_siefore_local
    WHERE   codigo_siefore    NOT IN(0,11);    
END FUNCTION

FUNCTION    F_500_proceso()
   DEFINE   l_registros                   INTEGER
   DEFINE   l_texto_query                 CHAR(500)
   OPEN WINDOW  TCAAB010   AT 2,2  WITH  FORM  "TCAAB010"  ATTRIBUTE(BORDER)
   DISPLAY  "TCAAB010   LIQUIDA DEVOLUCION DE COMISIONES POR TRASPASO INDEBIDO ",
            "                  "  AT  3,1  ATTRIBUTE(REVERSE)
   DISPLAY  " <Esc> Continuar                       < CTRL-C > Salir  ",
            "                         "   AT  1,1   ATTRIBUTE(REVERSE)
   DISPLAY  hoy USING "DD-MM-YYYY" AT 1,61 ATTRIBUTE(REVERSE)
   SELECT   a.*     INTO  taa_cd_ctr_folio_ind.*
     FROM   safre_af:taa_cd_ctr_folio_ind  a 
    --WHERE   a.fecha_liquidacion       =  hoy
    --  AND   a.estado                  =  g_provisionado
      WHERE   a.estado                  =  g_provisionado

   LET taa_cd_ctr_folio_ind.fecha_liquidacion = hoy

   IF       NOT    taa_cd_ctr_folio_ind.folio    THEN
            PROMPT   "  No existe folio pendiente de liquidar para hoy Teclee",
                     " <<Enter para Salir>>"  FOR  g_enter
            EXIT PROGRAM
   END IF
   SELECT   COUNT(nss)      INTO  l_registros
     FROM   safre_af:taa_cd_indebidos
    WHERE   folio                   =  taa_cd_ctr_folio_ind.folio
      AND   estado                  =  g_provisionado;

   CALL     F_510_trae_precio_accion()
   DISPLAY  taa_cd_ctr_folio_ind.folio                TO  FORMONLY.folio_ind
   DISPLAY  taa_cd_ctr_folio_ind.fecha_liquidacion    TO
            FORMONLY.fecha_liquidacion 
   DISPLAY  g_precio_acc_b1                TO  FORMONLY.precio_accion_b1
   DISPLAY  g_precio_acc_b2                TO  FORMONLY.precio_accion_b2
   DISPLAY  g_precio_acc_b3                TO  FORMONLY.precio_accion_b3
   DISPLAY  g_precio_acc_b4                TO  FORMONLY.precio_accion_b4
   #DISPLAY  g_precio_acc_b5                TO  FORMONLY.precio_accion_b5 # CPL-1195
   DISPLAY  g_precio_acc_b6                TO  FORMONLY.precio_accion_b6
   --DISPLAY  g_precio_parti                 TO  FORMONLY.precio_parti
   DISPLAY  l_registros                    TO  FORMONLY.l_registros
   WHILE    TRUE
            PROMPT   "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? "  FOR  g_enter
            IF       g_enter    MATCHES    "[sS]"      THEN
                     EXIT     WHILE
            ELSE
                     ERROR    "PROCESO CANCELADO" ATTRIBUTE(REVERSE) 
                     SLEEP    2
                     EXIT     PROGRAM
            END IF
   END WHILE
   DISPLAY  "  PROCESANDO INFORMACION ......                                "
               AT   18,1   ATTRIBUTE(REVERSE)
   CALL     F_520_liquida()
   CALL     F_530_arma_globales_para_reporte()
   CALL     genera_reporte()
   DISPLAY  "                                                                  "
               AT   18,1
   UPDATE   taa_cd_ctr_folio_ind
      SET   estado                     =  g_liquidado,
            fecha_liquidacion          =  hoy     
    WHERE   folio                      =  taa_cd_ctr_folio_ind.folio
      AND   estado                     =  g_provisionado;
   PROMPT  "  PROCESO FINALIZADO, TECLEE  <Enter>  Para Salir         "
            ATTRIBUTE(REVERSE)     FOR    g_enter
   CLOSE  WINDOW   TCAAB010
END FUNCTION

FUNCTION    F_510_trae_precio_accion()
   LET      g_precio_acc_b1             =  0
   LET      g_precio_acc_b2             =  0
   LET      g_precio_acc_b3             =  0
   LET      g_precio_acc_b4             =  0
   #LET      g_precio_acc_b5             =  0 # CPL-1195
   LET      g_precio_acc_b6             =  0
   LET      g_precio_parti              =  0

   SELECT   a.precio_del_dia
     INTO   g_precio_acc_b1
     FROM   safre_af:glo_valor_accion a
    WHERE   a.fecha_valuacion           =  hoy
     AND    a.codigo_siefore            =  1;
   IF       g_precio_acc_b1             =  0   THEN
            ERROR   "NO HAY PRECIO DE ACCION DE SIEFORE 1 DE HOY ...... "
            PROMPT  "TECLEE ENTER PARA SALIR..."   FOR  g_enter
            EXIT    PROGRAM
   END IF
   SELECT   a.precio_del_dia
     INTO   g_precio_acc_b2
     FROM   safre_af:glo_valor_accion a
    WHERE   a.fecha_valuacion           =  hoy
      AND   a.codigo_siefore            =  2;
   IF       g_precio_acc_b2             =  0    THEN
            ERROR   "NO HAY PRECIO DE ACCION DE SIEFORE 2 DE HOY ...... "
            PROMPT  "   TECLEE ENTER PARA SALIR........ "   FOR  g_enter
            EXIT    PROGRAM
   END IF
   SELECT   a.precio_del_dia
     INTO   g_precio_acc_b3
     FROM   safre_af:glo_valor_accion a
    WHERE   a.fecha_valuacion           =  hoy
      AND   a.codigo_siefore            =  3;
   IF       g_precio_acc_b3             =  0    THEN
            ERROR   "NO HAY PRECIO DE ACCION DE SIEFORE 3 DE HOY ...... "
            PROMPT  "   TECLEE ENTER PARA SALIR........ "   FOR  g_enter
            EXIT    PROGRAM
   END IF

   SELECT   a.precio_del_dia
     INTO   g_precio_acc_b4
     FROM   safre_af:glo_valor_accion a
    WHERE   a.fecha_valuacion           =  hoy
      AND   a.codigo_siefore            =  4;
   IF       g_precio_acc_b4             =  0    THEN
            ERROR   "NO HAY PRECIO DE ACCION DE SIEFORE 4 DE HOY ...... "
            PROMPT  "   TECLEE ENTER PARA SALIR........ "   FOR  g_enter
            EXIT    PROGRAM
   END IF
   
{# CPL-1195
   SELECT   a.precio_del_dia
     INTO   g_precio_acc_b5
     FROM   safre_af:glo_valor_accion a
    WHERE   a.fecha_valuacion           =  hoy
      AND   a.codigo_siefore            =  5;
   IF       g_precio_acc_b5             =  0    THEN
            ERROR   "NO HAY PRECIO DE ACCION DE SIEFORE 5 DE HOY ...... "
            PROMPT  "   TECLEE ENTER PARA SALIR........ "   FOR  g_enter
            EXIT    PROGRAM
   END IF
}

 SELECT "OK" 
 FROM  tab_siefore_local 
 WHERE codigo_siefore = 6
 GROUP BY 1
IF STATUS <> NOTFOUND THEN 
   SELECT   a.precio_del_dia
     INTO   g_precio_acc_b6
     FROM   safre_af:glo_valor_accion a
    WHERE   a.fecha_valuacion           =  hoy
      AND   a.codigo_siefore            =  6;
   IF       g_precio_acc_b6             =  0    THEN
            ERROR   "NO HAY PRECIO DE ACCION DE SIEFORE 6 DE HOY ...... "
            PROMPT  "   TECLEE ENTER PARA SALIR........ "   FOR  g_enter
            EXIT    PROGRAM
   END IF
 END IF

   LET      g_fecha_parti               =  MDY(MONTH(hoy),"01",YEAR(hoy))

--   SELECT   a.precio_del_dia
     --INTO   g_precio_parti
     --FROM   safre_af:glo_valor_accion a
    --WHERE   a.fecha_valuacion           =  g_fecha_parti
      --AND   a.codigo_siefore            =  11;
   --IF       g_precio_parti              =  0   THEN
            --ERROR   "NO HAY PRECIO DE PARTICIPACIN PARA VIVIENDA .... "
            --PROMPT  "TECLEE ENTER PARA SALIR..."  FOR  g_enter
            --EXIT  PROGRAM
   --END IF
END FUNCTION

FUNCTION    F_520_liquida()
   DEFINE   l_fecha_prov                  DATE
   DEFINE   l_texto_query                 CHAR(500)
   DEFINE   l_texto_liq                   CHAR(500)
   DEFINE   l_scta                        ,
            l_scta_fin                    ,
            verifica_liq                  SMALLINT
   SELECT   MAX(subcuenta_safre)      INTO   l_scta_fin
     FROM   taa_cd_subcuentas;
   LET      l_texto_liq           =  ' EXECUTE FUNCTION fn_liquida(?,?,?,?,?) '
   PREPARE  qry_liq            FROM  l_texto_liq
   LET      g_procesados          =  0
   DECLARE  cur_ced_ind       CURSOR  FOR
   SELECT   nss
     FROM   safre_af:taa_cd_indebidos
    WHERE   folio                 =  taa_cd_ctr_folio_ind.folio
      AND   estado                =  g_provisionado;
   FOREACH  cur_ced_ind        INTO  g_nss
            LET      g_procesados         =  g_procesados     +  1
            DISPLAY  g_procesados        TO  FORMONLY.l_procesados
            UPDATE   dis_provision
               SET   estado               =  6
             WHERE   folio                =  taa_cd_ctr_folio_ind.folio
               AND   nss                  =  g_nss
               AND   estado               =  5;
            FOR      l_scta               =  1     TO  l_scta_fin 
                     DECLARE  cur_liq      CURSOR  FOR  qry_liq
                     FOREACH  cur_liq
                              USING    taa_cd_ctr_folio_ind.folio,
                                       g_nss       ,
                                       l_scta      ,
                                       hoy         ,
                                       hoy
                                 INTO  verifica_liq
                     END FOREACH
            END FOR
            UPDATE   safre_af:taa_cd_indebidos
               SET   estado                =  g_liquidado,
                     fecha_liquidacion     =  taa_cd_ctr_folio_ind.fecha_liquidacion
             WHERE   folio                 =  taa_cd_ctr_folio_ind.folio
               AND   nss                   =  g_nss
               AND   estado                =  g_provisionado;
   END FOREACH
END FUNCTION

FUNCTION    F_530_arma_globales_para_reporte()
   DEFINE   l_tipo_desc                  CHAR(30)
   LET      g_tabname               = 'dis_cuenta    b '
   LET      g_folio                 =  taa_cd_ctr_folio_ind.folio
   SELECT   COUNT(UNIQUE   nss)
     INTO   g_total_cuentas
     FROM   safre_af:taa_cd_indebidos
    WHERE   folio                   =  g_folio
      AND   estado                  =  103;
   IF      (STATUS IS NULL)         THEN
            LET       g_total_cuentas          =  0
   END IF
   LET      g_fecha_accion          =  taa_cd_ctr_folio_ind.fecha_liquidacion
   LET      g_tipo_desc1                = 'TCAAB010'," ",l_tipo_desc CLIPPED,
            " ","POR TIPO DE TRASPASO"
   LET      g_tipo_desc2                = 'TCAAB010' ," ",l_tipo_desc CLIPPED,
            " ","POR SUBCUENTA"
   LET      g_nombre_programa           =  "TCAAB010"
   LET      g_tip_rep                   =  "LIQ" 
END FUNCTION

FUNCTION    F_900_fin()
   DEFINE   ejecuta                       CHAR(300)
   LET      ejecuta          =  "cd ",g_seg_modulo.ruta_exp CLIPPED,
                                "; fglgo    TCAAL009 ","2",g_folio
   RUN      ejecuta
   PROMPT   " Proceso Finalizado Teclee  <<Enter>> para Salir "  FOR  g_enter
END FUNCTION

