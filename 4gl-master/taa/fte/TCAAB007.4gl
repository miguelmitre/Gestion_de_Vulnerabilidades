###########################################################################
#Proyecto           => AFORE ( MEXICO )                                   #
#Propietario        => E.F.P.                                             #
#Programa TCAAB007  => LIQUIDACION AFORE - AFORE CEDENTE                  #
#Autor              => JOSE FRANCISCO LUGO CORNEJO                        #
#Fecha              => 13 JUNIO  2004                                     #
#Sistema            => TCAA                                               #
###########################################################################

DATABASE   safre_af

GLOBALS
   DEFINE
           g_fecha_liquidacion             DATE
   DEFINE
           g_provisionado                  ,  
           g_liquidada                     ,  
           g_cero                          ,
           g_siefore_max                   ,
           g_tipo_traspaso                 SMALLINT
   DEFINE
           g_procesados                    INTEGER
   DEFINE
           g_nom_tab_taa_cd_det            CHAR(040),  
           g_nss                           CHAR(011),
           g_enter                         CHAR(001),
           g_desc_tipo_traspaso            CHAR(023),
           g_ejecuta                       CHAR(300)

   DEFINE  reg_taa_cd_ctr_folio            RECORD  LIKE  taa_cd_ctr_folio.*,
           g_precio_acc_b1                 LIKE glo_valor_accion.precio_del_dia,
           g_precio_acc_b2                 LIKE glo_valor_accion.precio_del_dia,
           g_precio_acc_b3                 LIKE glo_valor_accion.precio_del_dia,
           g_precio_acc_b4                 LIKE glo_valor_accion.precio_del_dia,
           g_precio_acc_b5                 LIKE glo_valor_accion.precio_del_dia,
           g_precio_acc_b6                 LIKE glo_valor_accion.precio_del_dia,
           g_precio_parti                  LIKE glo_valor_accion.precio_del_dia
END GLOBALS

GLOBALS "GLOB_REPS.4gl"

MAIN
   OPTIONS
            PROMPT   LINE   LAST,
            ACCEPT   KEY    CONTROL-I
            DEFER    INTERRUPT
   CALL     STARTLOG('TCAAB007.log')
   CALL     F_001_inicio()
   CALL     F_500_proceso()
   CALL     F_900_fin()
END MAIN

FUNCTION    F_001_inicio()
   LET      hoy                      =  TODAY
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
     INTO   g_liquidada
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion            = 'LIQUIDADA'
      AND   a.tipo                   =  3;
   SELECT   MAX(codigo_siefore)   INTO  g_siefore_max  
     FROM   safre_af:tab_siefore_local
    WHERE   codigo_siefore    NOT  IN(0,11);    
END FUNCTION

FUNCTION    F_500_proceso()
   DEFINE   l_registros                   INTEGER
   DEFINE   l_texto_query                 CHAR(500)
   OPEN WINDOW  TCAAB0071   AT 2,2  WITH  FORM  "TCAAB0071"  ATTRIBUTE(BORDER)
   DISPLAY  "TCAAB007    LIQUIDACION  DE  SALDOS  TRASPASOS(CEDENTE) ",
            "                  "  AT  3,1  ATTRIBUTE(REVERSE)
   DISPLAY  " <Esc> Continuar                       < CTRL-C > Salir  ",
            "                         "   AT  1,1   ATTRIBUTE(REVERSE)
   DISPLAY  hoy USING "DD-MM-YYYY" AT 3,61 ATTRIBUTE(REVERSE)
   FOR      g_tipo_traspaso     =  1     TO  3
            SELECT   a.*     INTO   reg_taa_cd_ctr_folio.*
              FROM   taa_cd_ctr_folio a
             WHERE   a.tipo_traspaso           =  g_tipo_traspaso
               AND   a.fecha_liquidacion       =  hoy
               AND   a.estado                  =  g_provisionado
            IF       STATUS                   <>  NOTFOUND THEN
                     EXIT FOR
            END IF
   END FOR
   IF       STATUS                =  NOTFOUND    THEN
            PROMPT   "  No hay folio para liquidar hoy Teclee",
                     " <<Enter para Salir>>"  FOR  g_enter
            EXIT PROGRAM
   END IF
   IF       g_tipo_traspaso            =  1     OR
            g_tipo_traspaso            =  3     THEN
            LET      g_nom_tab_taa_cd_det      =  "taa_cd_det_cedido"
   ELSE
            LET      g_nom_tab_taa_cd_det      =  "taa_cd_det_comple"
   END IF
   LET      l_texto_query              = 
            ' SELECT   COUNT(*) ',
            '   FROM   ', g_nom_tab_taa_cd_det CLIPPED ,' ',
            '  WHERE   folio        = ',reg_taa_cd_ctr_folio.folio  CLIPPED,' ',
            '    AND   estado          =  ',g_provisionado
   PREPARE  query_count   FROM  l_texto_query
   EXECUTE  query_count   INTO  l_registros
   CALL     F_510_trae_precio_accion()
   IF       g_tipo_traspaso             =  1     THEN  
            LET      g_desc_tipo_traspaso          =  'NORMAL'
   ELSE
   IF       g_tipo_traspaso             =  3     THEN  
            LET      g_desc_tipo_traspaso          =  'INTERNET'
   ELSE
            LET      g_desc_tipo_traspaso          =  'COMPLEMENTARIO'
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
   DISPLAY  g_precio_acc_b1            TO  FORMONLY.precio_accion_b1
   DISPLAY  g_precio_acc_b2            TO  FORMONLY.precio_accion_b2
   DISPLAY  g_precio_acc_b3            TO  FORMONLY.precio_accion_b3
   DISPLAY  g_precio_acc_b4            TO  FORMONLY.precio_accion_b4
   DISPLAY  g_precio_acc_b5            TO  FORMONLY.precio_accion_b5
   DISPLAY  g_precio_acc_b6            TO  FORMONLY.precio_accion_b6
   DISPLAY  g_precio_parti             TO  FORMONLY.precio_parti
   DISPLAY  l_registros                TO  FORMONLY.l_registros
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
   UPDATE   taa_cd_ctr_folio
      SET   estado                     =  g_liquidada
    WHERE   folio                      =  reg_taa_cd_ctr_folio.folio
      AND   estado                     =  g_provisionado;
   IF       g_tipo_traspaso            =  1    OR
            g_tipo_traspaso            =  3    THEN
            UPDATE   taa_cd_det_cedido
               SET   estado            =  g_liquidada
             WHERE   folio             =  reg_taa_cd_ctr_folio.folio
               AND   estado            =  g_provisionado;
   ELSE
            UPDATE   taa_cd_det_comple
               SET   estado            =  g_liquidada
             WHERE   folio             =  reg_taa_cd_ctr_folio.folio
               AND   estado            =  g_provisionado;

            UPDATE   taa_cd_det_cedido
               SET   estado            =  g_liquidada
             WHERE   estado            =  12;
   END IF
   PROMPT  "  PROCESO FINALIZADO, TECLEE  <Enter>  Para Salir         "
            ATTRIBUTE(REVERSE)     FOR    g_enter
   CLOSE  WINDOW   TCAAB0071
END FUNCTION

FUNCTION    F_510_trae_precio_accion()
   LET      g_precio_acc_b1             =  0
   LET      g_precio_acc_b2             =  0
   LET      g_precio_acc_b3             =  0
   LET      g_precio_acc_b4             =  0
   LET      g_precio_acc_b5             =  0
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

   LET      g_fecha_parti               =  MDY(MONTH(hoy),"01",YEAR(hoy))
   SELECT   a.precio_del_dia
     INTO   g_precio_parti
     FROM   safre_af:glo_valor_accion a
    WHERE   a.fecha_valuacion           =  g_fecha_parti
      AND   a.codigo_siefore            =  11;

   IF       g_precio_parti              =  0   THEN
            ERROR   "NO HAY PRECIO DE PARTICIPACIN PARA VIVIENDA .... "
            PROMPT  "TECLEE ENTER PARA SALIR..."  FOR  g_enter
            EXIT  PROGRAM
   END IF
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
   LET      l_texto_query         =
            ' SELECT  n_seguro ',
            ' FROM    ', g_nom_tab_taa_cd_det CLIPPED ,
            ' WHERE   folio       = ',reg_taa_cd_ctr_folio.folio   CLIPPED,
            ' AND     estado      = ',g_provisionado
   PREPARE  query_ced            FROM  l_texto_query
   DECLARE  cur_ced       CURSOR  FOR  query_ced
   FOREACH  cur_ced         INTO  g_nss
            LET      g_procesados         =  g_procesados     +  1
            DISPLAY  g_procesados        TO  FORMONLY.l_procesados
            IF       g_tipo_traspaso            =  1  OR
                     g_tipo_traspaso            =  3  THEN
                     CALL     F_225_desmarcar()
            END IF
            UPDATE   dis_provision
               SET   estado              =  6
             WHERE   folio               =  reg_taa_cd_ctr_folio.folio
               AND   nss                 =  g_nss
               AND   estado              =  5;
            FOR      l_scta              =  1     TO  l_scta_fin 
                     DECLARE  cur_liq      CURSOR  FOR  qry_liq
                     FOREACH  cur_liq
                              USING    reg_taa_cd_ctr_folio.folio,
                                       g_nss       ,
                                       l_scta      ,
                                       hoy         ,
                                       hoy
                                 INTO  verifica_liq
                     END FOREACH
            END FOR
            SELECT   UNIQUE(a.fecha_valor)
              INTO   l_fecha_prov
              FROM   dis_provision  a
             WHERE   a.folio                  =  reg_taa_cd_ctr_folio.folio
               AND   a.nss                    =  g_nss
               AND   a.subcuenta             IN(3,10)
               AND   a.tipo_movimiento        BETWEEN   200   AND  299;
            IF       l_fecha_prov            >=  '01/01/1997'    AND
                     l_fecha_prov            <=  hoy             THEN
                     UPDATE   cta_saldo_vol
                        SET   saldo_acciones        =  0,
                              fecha_saldo           =  hoy,
                              usuario               =  g_usuario
                      WHERE   nss                   =  g_nss
                        AND   subcuenta           IN(3,10)
                        AND   fecha_saldo          <=  l_fecha_prov;
	    END IF
   END FOREACH
END FUNCTION

FUNCTION    F_225_desmarcar()        
   DEFINE   l_marca_entra                      ,
            l_ind_transferencia                ,
            l_ind_cod_result                   ,
            l_tipo_informe                     SMALLINT
   DEFINE   l_tipo_traspaso                    CHAR(2)  
   LET      l_ind_transferencia     =  6    # Transferencia concluida
                                            # por traspaso afore cedente
   SELECT   a.tipo_traspaso,b.marca_cod,b.tipo_informe
     INTO   l_tipo_traspaso,l_marca_entra,l_tipo_informe
     FROM   safre_af:taa_cd_det_cedido  a,safre_af:taa_cd_tipo_traspaso  b
    WHERE   a.folio                 =  reg_taa_cd_ctr_folio.folio
      AND   a.n_seguro              =  g_nss
      AND   a.estado                =  g_provisionado
      AND   a.tipo_traspaso         =  b.tipo_traspaso;
   IF       l_tipo_informe         THEN
            UPDATE   cta_ctr_cuenta
               SET   tipo_informe              =  l_tipo_informe ,
                     fecha_informe             =  hoy
             WHERE   nss                       =  g_nss;
   END IF
   LET      g_ejecuta            = 
           "EXECUTE  PROCEDURE  desmarca_cuenta(?,?,?,?,?,?)"
   LET      g_ejecuta            =  g_ejecuta   CLIPPED
   PREPARE  clausula_spl1         FROM  g_ejecuta
   EXECUTE  clausula_spl1        USING  g_nss,
                                        l_marca_entra,
                                        reg_taa_cd_ctr_folio.folio,
                                        g_cero,
                                        l_marca_entra,
                                        g_usuario
   LET      g_ejecuta            = 
           "EXECUTE  PROCEDURE  fn_ind_transferencia(?,?,?)"
   LET      g_ejecuta            =  g_ejecuta   CLIPPED
   PREPARE  spl_ind_trans        FROM   g_ejecuta
   EXECUTE  spl_ind_trans        USING  g_nss,
                                        l_ind_transferencia,
                                        reg_taa_cd_ctr_folio.fecha_liquidacion
                                 INTO   l_ind_cod_result
END FUNCTION

FUNCTION    F_530_arma_globales_para_reporte()
   DEFINE   l_tipo_desc                  CHAR(30)
   LET      g_tabname               = 'dis_cuenta    b '
   LET      g_folio                 =  reg_taa_cd_ctr_folio.folio
   SELECT   COUNT(UNIQUE n_seguro)
     INTO   g_total_cuentas
     FROM   safre_af:taa_cd_det_cedido
    WHERE   folio                   =  g_folio
      AND   estado  IN(12,102);
   IF      (STATUS IS NULL)         THEN
            LET       g_total_cuentas          =  0
   END IF
   LET      g_fecha_accion          =  reg_taa_cd_ctr_folio.fecha_liquidacion
   IF       g_tipo_traspaso             =  1  THEN
            LET        l_tipo_desc      =  "NORMAL"
   ELSE 
   IF       g_tipo_traspaso             =  3  THEN
            LET        l_tipo_desc      =  "INTERNET"
   ELSE 
            LET        l_tipo_desc      =  "COMPLEMENTARIO"
   END IF
   END IF
   LET      g_tipo_desc1                = 'TCAAB007'," ",l_tipo_desc CLIPPED,
            " ","POR TIPO DE TRASPASO"
   LET      g_tipo_desc2                = 'TCAAB007' ," ",l_tipo_desc CLIPPED,
            " ","POR SUBCUENTA"
   LET      g_nombre_programa           =  "TCAAB007"
   LET      g_tip_rep                   =  "LIQ" 
END FUNCTION

FUNCTION    F_900_fin()
   DELETE   FROM    safre_tmp:taa_cd_saldos_arch
    WHERE   folio               =  reg_taa_cd_ctr_folio.folio;
   DELETE   FROM    safre_tmp:taa_cd_sum_arch
    WHERE   folio               =  reg_taa_cd_ctr_folio.folio;
END FUNCTION
