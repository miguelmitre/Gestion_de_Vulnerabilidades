################################################################################
#Proyecto          => AFORE ( MEXICO )                                         #
#Propietario       => E.F.P.                                                   #
#Programa TCAAC010 => PROVISION DE TRASPASOS INDEBIDOS.                        #
#Sistema           => TCAA                                                     #
#Fecha             => 7/Feb/2007                                               #
#Autor             => JOSE  FRANCISCO  LUGO  CORNEJO                           #
#-----------------------------MODIFICACIONES-----------------------------------#
#Requerimiento     => CPL-1650    04-Junio-2014  Alejandro Chagoya Salazar     #
#Actualizacion     => Se cambia la forma de extrar la comision, se cambia tabla#
#                  => taa_cd_factor_comi por tab_rendimiento_neto              #
################################################################################
DATABASE    safre_af
GLOBALS

   DEFINE 
            g_fecha_recep_cta                        ,
            g_fecha_trasp_cta                        ,
            g_fecha_recep_incre                      ,
            g_fecha_liq_parti                        ,
            g_fecha_valor                            ,
            g_today                              DATE
   DEFINE 
            g_sie                                ,
            g_scta                               ,
            g_tipo_mov                           ,
            g_tipo_registro                      ,
            g_tipo_rendimiento                   ,
            g_scta_ini                           ,
            g_scta_fin                           ,
            g_sie_ini                            ,
            g_sie_fin                            ,
            g_procedente                         ,
            g_provisionado                       ,
            g_liquidado                          ,
            g_titulos                            ,
            g_tipo_inconsistencia                ,
            g_max_rendi_siefore                  ,
            g_tot_bloques_rcv                    SMALLINT
   DEFINE
            g_precio_parti                       DEC(25,14),
            g_max_rendimiento                    DEC(15,15),
            g_acciones                           DEC(16,6),
            g_pesos                              DEC(16,2), #CPL-1554
            g_pesos_traspaso                     DEC(16,2), #CPL-1554
            g_pesos_rendi_dia                    DEC(20,2), #CPL-1554
            g_cont_servicio                      DEC(10,0),
            g_factor_comision                    DEC(16,10),
            g_factor_porcentaje                  DEC(16,10),
            g_monto_minimo                       DEC(15,2)
   DEFINE
            g_n_unico                            CHAR(018),
            g_nss                                CHAR(11),
            g_tit_subcuenta                      CHAR(170),
            g_det_flujo                          CHAR(360),
            g_det_saldo                          CHAR(360),
            g_det_interes                        CHAR(360),
            g_det_nss                            CHAR(360),
            g_reg_total                          CHAR(360),
            g_id_aportante                       CHAR(008),
            g_enter2                              CHAR(020),
            g_hora                               CHAR(005), 
            g_comando                            CHAR(100),
            g_afore_local                        CHAR(003),
            g_max_rendi_afore                    CHAR(003),
            g_afo_recupera_cta                   CHAR(003),
            g_raz_social                         CHAR(050)

   DEFINE   g_valor_acc       ARRAY[50]     OF    RECORD
     	    nom_siefore                          CHAR(008),
            precio_accion                        DEC(11,6)
   END   RECORD
   DEFINE   g_c_flujo      ARRAY[50,50]     OF    RECORD
            acciones                             DEC(16,6),
            pesos                                DEC(16,2)
   END   RECORD
   DEFINE   g_c_saldo      ARRAY[50,50]     OF    RECORD
            acciones                             DEC(16,6),
            pesos                                DEC(16,2)
   END   RECORD
   DEFINE   g_resarcir      ARRAY[50,50]     OF    RECORD
            acciones                             DEC(16,6),
            pesos                                DEC(16,2)
   END   RECORD
   DEFINE   g_t_nss        ARRAY[50,50]     OF    RECORD
            acciones                             DEC(16,6),
            pesos                                DEC(16,2)
   END   RECORD
   DEFINE   g_t_rep        ARRAY[50,50]     OF    RECORD
            acciones                             DEC(16,6),
            pesos                                DEC(16,2)
   END   RECORD
   DEFINE   reg_taa_cd_ctr_folio   RECORD  LIKE  safre_af:taa_cd_ctr_folio.*
   DEFINE   taa_cd_ctr_folio_ind   RECORD  LIKE  safre_af:taa_cd_ctr_folio_ind.*
   DEFINE   g_dis_cuenta           RECORD  LIKE  safre_af:dis_cuenta.*

   DEFINE   m_comision    DECIMAL(16,10)   # CPL-1650
END GLOBALS

GLOBALS "GLOB_REPS.4gl"

MAIN
   OPTIONS  INPUT    WRAP,
            PROMPT   LINE   LAST,
            ACCEPT   KEY    CONTROL-I
#           DEFER    INTERRUPT
   CALL STARTLOG(FGL_GETENV("USER")||".TCAAC010.log")

   CALL     F_010_inicio()
   CALL     F_100_proceso()
   CALL     F_900_fin()
END MAIN
 
FUNCTION    F_010_inicio()
   CALL     STARTLOG(FGL_GETENV("USER")||".TCAAC010.log")
   LET      g_today                    =  TODAY
   LET      hoy                        =  g_today
   LET      g_hora                     =  TIME
   LET      g_scta_ini                 =  1
   LET      g_sie_ini                  =  1
   LET      g_titulos                  =  1
   LET      g_id_aportante             = "DevIndeb"
   SELECT   a.estado
     INTO   g_procedente
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion              =  "PROCEDENTE" 
      AND   a.tipo                     =  2 ;
   SELECT   a.estado
     INTO   g_provisionado
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion              =  "PROVISIONADA"
      AND   a.tipo                     =  3 ;
   SELECT   a.estado
     INTO   g_liquidado
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion            = 'LIQUIDADA'
      AND   a.tipo                   =  3;
   SELECT   codigo_afore, razon_social, user
     INTO   g_afore_local,g_raz_social,g_usuario
     FROM   tab_afore_local ;

   SELECT   MAX(subct_cod)                  
     INTO   g_scta_fin
     FROM   safre_af:tab_subcuenta;

   SELECT   MAX(codigo_siefore)                 
     INTO   g_sie_fin
     FROM   safre_af:tab_siefore_local          
     WHERE  codigo_siefore     NOT IN (0,11,12);


WHENEVER ERROR CONTINUE 
  DROP TABLE taa_cd_cambio_sie
WHENEVER ERROR STOP


   CALL     F_020_busca_folio_pendiente()
   CALL     F_030_trae_parametros()
   FOR      g_sie                    =  g_sie_ini     TO  g_sie_fin
            FOR      g_scta          =  g_scta_ini    TO  g_scta_fin
                     LET      g_t_rep[g_sie,g_scta].acciones     =  0
                     LET      g_t_rep[g_sie,g_scta].pesos        =  0
            END FOR
   END FOR
#   LET      g_tipo_registro         =  1
   CALL     F_200_arma_registro()
END FUNCTION

FUNCTION   F_010_1_arma_fechas_folio()
   DEFINE  l_folio_existente           INTEGER


{

   SELECT  fecha_envio_saldos,fecha_envio_saldos
     INTO  reg_taa_cd_ctr_folio.fecha_envio_saldos,
           taa_cd_ctr_folio_ind.fecha_liquidacion
    FROM   safre_af:taa_cd_ctr_folio
   WHERE   fecha_envio_saldos       BETWEEN  g_today     AND  g_today  +  5
     AND   tipo_traspaso              =  1;
   IF      STATUS                     =  NOTFOUND     THEN
           ERROR "  NO EXISTE  FOLIO PROCEDENTE PARA TOMAR LAS FECHAS PARA EL PROCESO  "
           PROMPT "  TECLEE ENTER PARA SALIR DEL PROGRAMA  ? "  FOR  g_enter2
           EXIT PROGRAM
   END IF
}
   LET reg_taa_cd_ctr_Folio.fecha_envio_saldos = g_today
   LET taa_cd_ctr_folio_ind.fecha_liquidacion  = g_today

   SELECT  folio
     INTO  l_folio_existente
     FROM  safre_af:taa_cd_ctr_folio_ind
    WHERE  estado                     = 102 
   IF      l_folio_existente          THEN
           ERROR "  YA  EXISTE FOLIO PROVISIONADO PARA RESARCIMIENTO..."
           PROMPT "  TECLEE ENTER PARA SALIR DEL PROGRAMA  ? "  FOR  g_enter2
           EXIT PROGRAM
   END IF
END FUNCTION

FUNCTION    F_020_busca_folio_pendiente()
   DEFINE   l_verifica_prov                     SMALLINT
   DEFINE   l_tot_registros                     INTEGER
   OPEN     WINDOW    TCAAC010       AT  2,2 
            WITH      FORM     "TCAAC010"  ATTRIBUTE(BORDER)
   DISPLAY  "TCAAC010            TRASPASOS  INDEBIDOS   (CEDENTE)       ",
            "                   "    AT  2,2   ATTRIBUTE(REVERSE)   
   DISPLAY  "     PROVISIONA DEVOLUCION DE COMISIONES POR TRASPASOS INDEBIDOS               ",
            "                   "    AT  4,2   ATTRIBUTE(REVERSE)   
   CALL     F_010_1_arma_fechas_folio()
   DISPLAY  g_today     USING  "DD-MM-YYYY"   AT  2,60  ATTRIBUTE(REVERSE)
   DISPLAY  BY  NAME  reg_taa_cd_ctr_folio.fecha_envio_saldos
   --DISPLAY  BY  NAME  taa_cd_ctr_folio_ind.fecha_liquidacion
   DISPLAY  taa_cd_ctr_folio_ind.folio        TO  folio_ind
   CALL     F_022_precio_acc_parti()
   LET      int_flag                   =  FALSE

   SELECT   COUNT(*)
     INTO   l_tot_registros
     FROM   safre_af:taa_cd_indebidos a
    WHERE   estado = 101

   DISPLAY  l_tot_registros            TO  l_registros
   WHILE    TRUE
            PROMPT   "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? "  FOR  g_enter2
            IF       g_enter2      MATCHES   "[sSnN]"       THEN
                     IF       g_enter2       MATCHES   "[sS]"       THEN
                              IF       l_tot_registros     =  0    THEN
                                       EXIT  PROGRAM
                              END IF
                              EXIT WHILE
                     ELSE
                              EXIT     PROGRAM
                     END IF
            END  IF
   END      WHILE
   INSERT   INTO     safre_af:glo_folio 
            VALUES  (0)
   SELECT   MAX(folio)
     INTO   taa_cd_ctr_folio_ind.folio
     FROM   safre_af:glo_folio;
   DISPLAY  taa_cd_ctr_folio_ind.folio       TO  folio_ind
   ERROR    "  PROCESANDO  INFORMACION .........    "
END FUNCTION

FUNCTION    F_022_precio_acc_parti()
   DEFINE   l_siefore_rcv                     ,
            l_siefore_viv                     SMALLINT
   DEFINE   l_nom_siefore                     CHAR(008),
            l_precio_accion                   DEC(11,6)
   FOR      g_sie             =  g_sie_ini      TO  g_sie_fin
            LET      g_valor_acc[g_sie].nom_siefore     =  "        "
            LET      g_valor_acc[g_sie].precio_accion   =  0
   END FOR

   DECLARE  cur_sie        CURSOR    FOR
   SELECT   a.codigo_siefore,b.razon_social,a.precio_del_dia 
     FROM   glo_valor_accion a, tab_siefore_local b
    WHERE   a.fecha_valuacion         =  g_today
      AND   a.codigo_siefore       NOT IN (0,5,11,12) #CPL-1195
      AND   b.codigo_afore           =  g_afore_local
      AND   a.codigo_siefore          =  b.codigo_siefore;

   FOREACH  cur_sie       INTO    g_sie,l_nom_siefore,l_precio_accion
            LET      g_valor_acc[g_sie].nom_siefore     =  l_nom_siefore
            LET      g_valor_acc[g_sie].precio_accion   =  l_precio_accion
   END FOREACH
   FOR      g_sie             =  g_sie_ini      TO  g_sie_fin
            IF       g_valor_acc[g_sie].nom_siefore       IS  NULL   OR
                     g_valor_acc[g_sie].nom_siefore[1,3]   =  "   "   THEN
                     CONTINUE  FOR
            END IF
            IF       g_valor_acc[g_sie].precio_accion       IS  NULL   OR
                     g_valor_acc[g_sie].precio_accion        =  0   THEN
                     ERROR    "  NO HAY PRECIO DE ACCION SIEFORE RCV BASICA ",
                               g_sie   USING   "##","                     "

                     PROMPT   "TECLEE ENTER PARA SALIR..."     FOR  g_enter2
                     EXIT     PROGRAM
            END IF
   END FOR
   DISPLAY  g_valor_acc[1].precio_accion       TO  g_precio_accion_b1
   DISPLAY  g_valor_acc[2].precio_accion       TO  g_precio_accion_b2
   DISPLAY  g_valor_acc[3].precio_accion       TO  g_precio_accion_b3
   DISPLAY  g_valor_acc[4].precio_accion       TO  g_precio_accion_b4
  # DISPLAY  g_valor_acc[5].precio_accion       TO  g_precio_accion_b5 # CPL-1195
   DISPLAY  g_valor_acc[6].precio_accion       TO  g_precio_accion_b6
END FUNCTION

FUNCTION    F_030_trae_parametros()
   DEFINE   l_taa_cd_subcuentas      RECORD  LIKE safre_af:taa_cd_subcuentas.*
   SELECT   *
     INTO   g_seg_modulo.*
     FROM   seg_modulo
    WHERE   modulo_cod                  =  "taa" ; 
END FUNCTION

FUNCTION    F_100_proceso()
   DEFINE   l_leidos                            INTEGER
   DEFINE   l_fecha_mov_banxico                 DATE
   LET      l_leidos                    =  0
   CALL     F_105_inserta_folio_indebidos()
   LET      g_tipo_registro             =  2
   LET      g_fecha_valor               =  g_today
   LET      g_comando                   = 
            g_seg_modulo.ruta_listados           CLIPPED, "/",
            g_usuario    CLIPPED ,".DEV_INDEBIDOS.",
           "12",".", g_today    USING    "DDMMYY"
   START    REPORT     R_220_rep_saldos_tot      TO  g_comando

   DECLARE  cur_nss_ced         CURSOR   FOR
   SELECT   nss,tipo_rendi
     FROM   safre_af:taa_cd_indebidos 
    WHERE   estado                   =  101

    ORDER   BY  nss ;
   FOREACH  cur_nss_ced       INTO  g_nss,g_tipo_rendimiento
            SELECT   n_unico,cont_servicio
              INTO   g_n_unico,g_cont_servicio
              FROM   safre_af:taa_cd_det_cedido
             WHERE   n_seguro                     =  g_nss
               AND   tipo_traspaso                IN ("21","73") # CPL-1285
               AND   estado                       =  103; 
            LET      g_fecha_recep_cta            =  NULL

            SELECT   MAX(A.fecha_mov_banxico)
              INTO   l_fecha_mov_banxico
              FROM   safre_af:taa_rcv_recepcion  A
             WHERE   A.nss                        =  g_nss
               AND   A.ident_operacion            = "09";

            SELECT   cve_ced_cuenta
              INTO   g_afo_recupera_cta
              FROM   safre_af:taa_rcv_recepcion  A
             WHERE   A.nss                        =  g_nss
               AND   A.ident_operacion            = "09"
               AND   A.fecha_mov_banxico          =  l_fecha_mov_banxico;

            SELECT   MAX(A.fecha_calculo_rendi)
              INTO   g_fecha_recep_cta
              FROM   safre_af:taa_cd_his_rendi_dia  A
             WHERE   A.nss                        =  g_nss
            IF       g_fecha_recep_cta                IS  NULL             OR
                     g_fecha_recep_cta                 <  "12/31/1998"     THEN
                     LET      g_fecha_recep_cta        =  l_fecha_mov_banxico
                     SELECT   MAX(a.fecha_trasp)
                       INTO   g_fecha_trasp_cta
                       FROM   taa_cd_det_cedido a
                      WHERE   a.n_seguro               =  g_nss
                        AND   a.estado                 =  103;
            END IF
 
            CALL     F_110_inicializa_nss()
#           CALL     F_120_comision_por_flujo() #Habilitar para actinver 
#           CALL     F_140_comision_por_saldo()
            CALL     F_150_resarcimiento()
            CALL     F_200_arma_registro()
                     ######    Arma  Registros  de  Detalle   #########
            OUTPUT     TO    REPORT  R_220_rep_saldos_tot()
            LET      l_leidos                 =  l_leidos       +  1
            DISPLAY  "<< REGISTROS  PROCESADOS >>:     ", l_leidos
                      USING     "[######]"      AT  14,23

            UPDATE   safre_af:taa_cd_indebidos
               SET   estado           =  102
             WHERE   folio            =  taa_cd_ctr_folio_ind.folio
               AND   nss              =  g_nss
               AND   estado           =  101; 
   END FOREACH
   IF       l_leidos                  =  0  THEN
            ERROR    "  NO SE ENCONTRARON REGISTROS PARA SER TRASPASADOS"
                       ATTRIBUTE(REVERSE)
            PROMPT   "  TECLEE ENTER PARA SALIR..." FOR g_enter2
            EXIT     PROGRAM
   END IF
   FINISH   REPORT     R_220_rep_saldos_tot
   UPDATE   safre_af:taa_cd_indebidos
      SET   folio                     =  taa_cd_ctr_folio_ind.folio,
            estado                    =  102
    WHERE   estado                    =  101; 
   CALL     F_500_arma_globales_para_reporte()
   CALL     genera_reporte()
   DISPLAY  "                                                                  "
               AT   18,1
END FUNCTION

FUNCTION    F_105_inserta_folio_indebidos()
   INSERT   INTO     safre_af:taa_cd_ctr_folio_ind
            VALUES  (
                     taa_cd_ctr_folio_ind.folio,
                    "2" ,
                     taa_cd_ctr_folio_ind.folio,
                     g_today,
                     --taa_cd_ctr_folio_ind.fecha_liquidacion,
                     " ",
                     g_provisionado,
                     g_usuario          
                    )
END FUNCTION

FUNCTION    F_110_inicializa_nss()
   FOR      g_sie                     =  g_sie_ini     TO  g_sie_fin
            FOR      g_scta           =  g_scta_ini    TO  30
                     LET      g_c_flujo[g_sie,g_scta].acciones      =  0
                     LET      g_c_flujo[g_sie,g_scta].pesos         =  0
                     LET      g_c_saldo[g_sie,g_scta].acciones      =  0
                     LET      g_c_saldo[g_sie,g_scta].pesos         =  0
                     LET      g_resarcir[g_sie,g_scta].acciones      =  0
                     LET      g_resarcir[g_sie,g_scta].pesos         =  0
                     LET      g_t_nss  [g_sie,g_scta].acciones      =  0
                     LET      g_t_nss  [g_sie,g_scta].pesos         =  0
            END FOR
   END FOR
   LET      g_acciones                   =  0
   LET      g_pesos                      =  0
   LET      g_det_flujo                  =  NULL
   LET      g_det_saldo                  =  NULL
   LET      g_det_interes                =  NULL
   LET      g_det_nss                    =  NULL
   LET      g_max_rendi_afore            =  0
   LET      g_max_rendi_siefore          =  0
   LET      g_max_rendimiento            =  0
END FUNCTION

FUNCTION    F_120_comision_por_flujo()
   LET      g_tipo_mov                   =  777
   DECLARE  cur_comi_flujo       CURSOR  FOR
   SELECT   a.subcuenta,a.siefore,SUM(a.monto_en_acciones  *  -1)
     FROM   safre_af:dis_cuenta05 a
    WHERE   a.nss                          =  g_nss
      AND   a.tipo_movimiento             IN (101,102,103,104,105,106,107,
                                              108,109,111,112)
      AND   (a.fecha_conversion           >=  g_fecha_recep_cta
      AND    a.fecha_conversion            <  g_today)
      GROUP  BY 1,2
   UNION  ALL
   SELECT   b.subcuenta,b.siefore,SUM(b.monto_en_acciones    *  -1)
     FROM   safre_af:dis_cuenta b
    WHERE   b.nss                          =  g_nss
      AND   b.tipo_movimiento             IN (101,102,103,104,105,106,107,
                                              108,109,111,112)
      AND  (b.fecha_conversion            >=  g_fecha_recep_cta
      AND   b.fecha_conversion             <  g_today)
    GROUP   BY  1,2

   FOREACH  cur_comi_flujo            INTO  g_scta,g_sie,g_acciones
            LET      g_pesos             =
                     g_acciones          *  g_valor_acc[g_sie].precio_accion
            IF       g_acciones         IS  NULL      THEN
                     LET     g_acciones               =  0
            END IF
            IF       g_pesos            IS  NULL      THEN
                     LET     g_pesos                  =  0
            END IF
            LET      g_c_flujo[g_sie,g_scta].acciones     =
                     g_c_flujo[g_sie,g_scta].acciones     +  g_acciones
            LET      g_c_flujo[g_sie,g_scta].pesos        =
                     g_c_flujo[g_sie,g_scta].pesos        +  g_pesos
            CALL     F_130_acumula_nss_total()
            CALL     F_160_inserta_dis_provision()
   END FOREACH
END FUNCTION

FUNCTION    F_130_acumula_nss_total()
   LET      g_t_nss  [g_sie,g_scta].acciones         =
            g_t_nss  [g_sie,g_scta].acciones         +  g_acciones
   LET      g_t_nss  [g_sie,g_scta].pesos            =
            g_t_nss  [g_sie,g_scta].pesos            +  g_pesos
   LET      g_t_rep  [g_sie,g_scta].acciones         =
            g_t_rep  [g_sie,g_scta].acciones         +  g_acciones
   LET      g_t_rep  [g_sie,g_scta].pesos            =
            g_t_rep  [g_sie,g_scta].pesos            +  g_pesos
END FUNCTION

FUNCTION    F_140_comision_por_saldo()
   LET      g_tipo_mov                   =  778
   DECLARE  cur_comi_saldo          CURSOR  FOR
   SELECT   siefore,subcuenta,SUM(monto_en_pesos  *  -1)
     FROM   safre_af:dis_cuenta05
    WHERE   nss                          =  g_nss
      AND   tipo_movimiento              =  110
      AND   fecha_conversion            >=  g_fecha_recep_cta
      AND   fecha_conversion             <  g_today
     GROUP  BY 1,2
   UNION    ALL
   SELECT   siefore,subcuenta,SUM(monto_en_pesos  *  -1)
     FROM   safre_af:dis_cuenta
    WHERE   nss                          =  g_nss
      AND   tipo_movimiento              =  110
      AND   fecha_conversion            >=  g_fecha_recep_cta
      AND   fecha_conversion             <  g_today
    GROUP   BY  1,2;
   FOREACH  cur_comi_saldo            INTO  g_sie,g_scta,g_pesos
            LET      g_acciones          =
                     g_pesos             /  g_valor_acc[g_sie].precio_accion
            IF       g_acciones         IS  NULL      THEN
                     LET     g_acciones               =  0
            END IF
            IF       g_pesos            IS  NULL      THEN
                     LET     g_pesos                  =  0
            END IF
            LET      g_c_saldo[g_sie,g_scta].acciones       =
                     g_c_saldo[g_sie,g_scta].acciones       +  g_acciones
            LET      g_c_saldo[g_sie,g_scta].pesos          =
                     g_c_saldo[g_sie,g_scta].pesos          +  g_pesos
            CALL     F_130_acumula_nss_total()
            CALL     F_160_inserta_dis_provision()
   END FOREACH
END FUNCTION 

FUNCTION    F_150_resarcimiento()
   DEFINE   lc_saldo_acumulado           DEC(20,10)
   DEFINE   lc_saldo_acumulado_6d        DEC(16,6)
   DEFINE   lc_saldo_dia                 DEC(20,10)
   DEFINE   anio,
            l_mes                        SMALLINT
   DEFINE   dias_ano                     SMALLINT
   DEFINE   fecha1 ,fecha2      DATE

   LET      g_tipo_mov                   =  779
   LET      lc_saldo_acumulado           =  0
   CALL     genera_tmp_cuenta           (g_nss,
                                         g_fecha_recep_cta,
                                         g_today
                                        )
   DECLARE  cur_scta CURSOR FOR 
   SELECT   UNIQUE a.subcuenta
     FROM   tmp_dis_cuenta a 
    WHERE   nss                     =  g_nss
      AND   subcuenta         NOT  IN (4,8,14,0);
   FOREACH  cur_scta         INTO  g_scta 
         SELECT   SUM(monto_en_pesos      *  -1)
           INTO   g_pesos_traspaso
           FROM   tmp_dis_cuenta
          WHERE   nss                     =  g_nss
            AND   subcuenta               =  g_scta
            AND   fecha_conversion        =  g_fecha_trasp_cta
            AND   tipo_movimiento        IN(SELECT  b.marca_cod
                                              FROM  taa_cd_tipo_traspaso  b);
         IF       g_pesos_traspaso       IS  NULL  THEN
                  LET      g_pesos_traspaso     =  0
         END IF
         LET   g_fecha_recep_incre     =  g_fecha_recep_cta
         LET   lc_saldo_acumulado      =  0
         WHILE   g_fecha_recep_incre      <=  g_fecha_trasp_cta

               CALL    F_155_max_siefore()
               LET g_factor_comision   = 0 
               LET g_factor_porcentaje = 0 

               LET dias_ano = 0
               LET anio    = YEAR(g_fecha_recep_incre)
               LET l_mes   = MONTH(g_fecha_recep_incre)
               LET fecha1  = MDY("12","31",anio)
               LET fecha2  = MDY("01","01",anio)
               LET dias_ano =  fecha1 - fecha2 + 1

#CPL-1650 INI
               LET m_comision = 0

               SELECT UNIQUE (a.comision/100) INTO m_comision
               FROM tab_rendimiento_neto a
               WHERE a.afore_cod = g_afore_local
               AND a.fecha_ini = (SELECT MAX(b.fecha_ini) FROM tab_rendimiento_neto b
                                  WHERE b.afore_cod = g_afore_local
                                  AND MONTH(b.fecha_ini) = l_mes
                                  AND YEAR(b.fecha_ini) = anio)

               IF SQLCA.SQLCODE = 100 THEN   #Si no encuentra comision con la fecha
                  SELECT UNIQUE (a.comision/100) INTO m_comision
                  FROM tab_rendimiento_neto a
                  WHERE a.afore_cod = g_afore_local
                  AND a.fecha_ini = (SELECT MIN(b.fecha_ini) FROM tab_rendimiento_neto b
                                     WHERE b.afore_cod = g_afore_local
                                     AND b.fecha_ini > g_fecha_recep_incre )
               END IF

               LET g_factor_comision   = 1 + (m_comision / dias_ano)
               LET g_factor_porcentaje = m_comision
#CPL-1650 FIN
               LET     lc_saldo_dia           =  0
               LET     lc_saldo_acumulado     = 
                       lc_saldo_acumulado     *  g_factor_comision
               IF      lc_saldo_acumulado    IS  NULL    THEN 
                       LET     lc_saldo_acumulado    =  0
               END IF

               SELECT   ROUND(SUM(monto_en_pesos)      *
                        (1  +  g_max_rendimiento ),6)  *  g_factor_comision
                 INTO    lc_saldo_dia
                 FROM    tmp_dis_cuenta a 
                WHERE    a.nss                  =  g_nss 
                  AND    a.subcuenta            =  g_scta
                  AND    a.fecha_conversion     =  g_fecha_recep_incre
                  AND    a.tipo_movimiento NOT IN (110,100,101,102,103,104,105,
                                                106,107,108,109,111,112,113,114)
                  AND    a.tipo_movimiento NOT IN (select b.marca_cod 
                                                FROM   taa_cd_tipo_traspaso b)
               IF        lc_saldo_dia   IS  NULL   THEN 
                         LET     lc_saldo_dia    =  0
               END IF
               LET       lc_saldo_acumulado      =
                         lc_saldo_acumulado      +  lc_saldo_dia
               LET       lc_saldo_acumulado_6d   =  lc_saldo_acumulado 
               LET       g_pesos_rendi_dia       =  lc_saldo_acumulado_6d
           --    IF        g_pesos_rendi_dia      <>  0   THEN 
                         CALL      F_165_inserta_historico()
           --    END IF
               LET       g_fecha_recep_incre      =  g_fecha_recep_incre   +  1
           END WHILE
           SELECT   UNIQUE a.codigo_siefore 
             INTO   g_sie
             FROM   cta_regimen  a 
            WHERE   a.nss                   =  g_nss 
              and   a.subcuenta             =  g_scta;
           LET      g_pesos                 =
                    lc_saldo_acumulado_6d   -  g_pesos_traspaso
           IF       g_pesos                <=  0   THEN
                    CONTINUE  FOREACH
           END IF

           SELECT a.precio_del_dia 
           INTO   g_valor_acc[g_sie].precio_accion 
           FROM   glo_valor_accion a
           WHERE  a.fecha_valuacion  = g_fecha_trasp_cta
           AND    a.codigo_siefore   = g_sie

           LET      g_acciones              =
                    g_pesos                 /  g_valor_acc[g_sie].precio_accion
           LET      g_resarcir[g_sie,g_scta].acciones    =
                    g_resarcir[g_sie,g_scta].acciones    +  g_acciones
           LET      g_resarcir[g_sie,g_scta].pesos       =
                    g_resarcir[g_sie,g_scta].pesos       +  g_pesos
           CALL     F_130_acumula_nss_total()
           CALL     F_160_inserta_dis_provision()

    END FOREACH
END FUNCTION

FUNCTION    F_155_max_siefore()
   DEFINE   l_afo_recupera_cta         CHAR(003)
   DEFINE   l_fecha_recep_incre        DATE
   DEFINE   l_siefore                  SMALLINT
   DEFINE   l_factualiza               DATE
   DEFINE   l_txt_cur                  CHAR(300)
   DEFINE   reg_inicio      RECORD     LIKE  tab_siefore_sistema.*  
   DEFINE   reg_fin         RECORD     LIKE  tab_siefore_sistema.*
   DEFINE   l_rendimiento_afo          DEC(16,15)
   DEFINE   l_cont SMALLINT
   LET      l_cont                    =  0
   LET      g_max_rendimiento         =  0
   LET      l_fecha_recep_incre       =  g_fecha_recep_incre 
   WHILE    TRUE
            SELECT   "ok" 
              FROM   tab_siefore_sistema a
             WHERE   a.fecha_valuacion             =  l_fecha_recep_incre
             GROUP   BY  1
            IF       STATUS    <>    NOTFOUND THEN 
                     EXIT WHILE
            ELSE 
                     LET    l_fecha_recep_incre   =  l_fecha_recep_incre  -  1 
            END IF 
   END WHILE

 LET l_afo_recupera_cta = g_afo_recupera_cta

   IF g_afo_recupera_cta = 540 THEN
       LET l_afo_recupera_cta = 544
   END IF

   IF g_afo_recupera_cta = 558 THEN
       LET l_afo_recupera_cta = 564
   END IF

   LET     l_txt_cur    =  " SELECT  a.* ",
                           " FROM    tab_siefore_sistema a ",
                           " WHERE   a.fecha_valuacion   = ? "

   CASE    g_tipo_rendimiento
           WHEN     1
                 LET l_siefore = 0
                 DECLARE cur_orden_fecha CURSOR FOR 
                    SELECT a.factualiza,a.codigo_siefore 
                    FROM   cta_his_regimen a
                    WHERE  a.nss        = g_nss
                    AND    a.subcuenta  = g_scta
                    UNION ALL 
                    SELECT today,b.codigo_siefore
                    FROM   cta_regimen b 
                    WHERE  b.nss = g_nss
                    AND    b.subcuenta = g_scta
                    ORDER BY 1
                 FOREACH cur_orden_fecha INTO l_factualiza,l_siefore
                      IF l_fecha_recep_incre < l_factualiza THEN 
                         EXIT FOREACH 
                      END IF
                 END FOREACH

                    LET      l_txt_cur      =  l_txt_cur  CLIPPED,
                             "  AND (a.afore_cod   =  ",g_afore_local,
                             "  OR a.afore_cod     =  ",l_afo_recupera_cta,")" ,
                             "  AND a.siefore_cod = ",l_siefore
      
                    EXIT CASE
           OTHERWISE
                    EXIT CASE
   END CASE
   
   #Selecciona informacion de reg_inicio
   INITIALIZE reg_inicio.* TO NULL
            
   PREPARE lqry_mr1       FROM  l_txt_cur
   DECLARE lcur_mr1     CURSOR  FOR lqry_mr1
   FOREACH lcur_mr1      USING  l_fecha_recep_incre 
                          INTO  reg_inicio.*
         IF      ( reg_inicio.precio_accion IS NULL OR 
                  reg_inicio.precio_accion = 0 ) THEN
                  CONTINUE  FOREACH
         END IF

         LET      l_cont              = l_cont + 1 
         LET      l_rendimiento_afo   = 0
         #Selecciona informacion de reg_inicio
         INITIALIZE reg_fin.* TO NULL
   
         SELECT   a.* 
           INTO   reg_fin.*
           FROM   tab_siefore_sistema a 
          WHERE   a.fecha_valuacion       =  g_fecha_trasp_cta
            AND   a.afore_cod             =  reg_inicio.afore_cod
            AND   a.siefore_cod           =  reg_inicio.siefore_cod

         IF (reg_fin.precio_accion IS NULL 
             OR reg_fin.precio_accion = 0) THEN 
             CONTINUE FOREACH 
         END IF  

         LET      l_rendimiento_afo       =
                 (reg_fin.precio_accion   /  reg_inicio.precio_accion)   - 1 

         IF l_cont = 1 THEN 
            LET g_max_rendimiento = l_rendimiento_afo
         END IF

         IF      (l_rendimiento_afo       >  g_max_rendimiento)       THEN 
                  LET      g_max_rendi_afore        =  reg_fin.afore_cod
                  LET      g_max_rendi_siefore      =  reg_inicio.siefore_cod
                  LET      g_max_rendimiento        =  l_rendimiento_afo
         END IF
   END FOREACH
END FUNCTION

FUNCTION    F_160_inserta_dis_provision()
   DEFINE   l_folio_sua                        INTEGER
   LET      l_folio_sua                  =  9999
   INSERT   INTO     safre_af:dis_provision       VALUES
           (
            g_tipo_mov                             ,  #tipo_movimiento
            g_scta                                 ,  #subcuenta
            g_sie                                  ,  #siefore
            taa_cd_ctr_folio_ind.folio             ,  #folio
            g_cont_servicio                        ,  #consecutivo
            g_nss                                  ,  #nss
            g_n_unico                              ,  #nss
            l_folio_sua                            ,  #folio_sua
            g_fecha_trasp_cta                      ,  #fecha pago
            g_fecha_trasp_cta                      ,  #fecha pago
            g_fecha_trasp_cta                      ,  #fecha pago
            g_pesos                                ,  #monto_en_pesos
            g_acciones                             ,  #monto_en_acciones  
            g_valor_acc[g_sie].precio_accion       ,
            "0"                                    ,  #dias_cotizados
            "1"                                    ,  #sucursal
            g_id_aportante                         ,  #id_aportante
            "6"                                    ,  #estado
            g_today                                ,  #fecha proceso
            g_usuario                              ,
            g_today                                ,  #fecha archivo
            "1"                                       # etiqueta
            )
END FUNCTION

FUNCTION    F_165_inserta_historico()
   INSERT   INTO     safre_af:taa_cd_his_rendi_dia       VALUES
           (
            taa_cd_ctr_folio_ind.folio             ,  #folio
            g_nss                                  ,  #nss
            g_max_rendi_afore                      ,
            g_max_rendi_siefore                    ,
            g_scta                                 ,  #subcuenta
            g_factor_comision                      ,
            g_factor_porcentaje                    ,  #porcentaje comis
            g_max_rendimiento                      ,
            g_pesos_rendi_dia                      ,  #saldo diario
            g_fecha_recep_incre                    ,  #fecha rendi diario
            g_usuario
            )
END FUNCTION

FUNCTION    F_200_arma_registro()
   FOR      g_sie          =  g_sie_ini    TO  g_sie_fin
            FOR      g_scta              =  g_scta_ini     TO  g_scta_fin
                     IF       g_scta                        =  4     OR
                              g_scta                        =  8     OR
                              g_scta                        =  14    THEN
                              CONTINUE  FOR
                     END IF
                     IF       g_sie                        <>  6        THEN
                              IF       g_scta               =  3        OR
                                       g_scta               =  10       OR
                                       g_scta               =  11       OR
                                       g_scta               =  12       OR
                                       g_scta               =  13       OR
                                       g_scta               =  15       THEN
                                       CONTINUE  FOR
                              END IF
                     END IF
                     IF       g_sie                         =  6        THEN
                              IF       g_scta              <>  3        AND
                                       g_scta              <>  10       AND
                                       g_scta              <>  11       AND
                                       g_scta              <>  12       AND
                                       g_scta              <>  13       AND
                                       g_scta              <>  15       THEN
                                       CONTINUE  FOR
                              END IF
                     END IF
                     CASE     g_tipo_registro
                              WHEN     1
                                       CALL      F_220_arma_titulos()
                              WHEN     2
                                       CALL      F_240_arma_detalles()
                              WHEN     3
                                       CALL      F_260_arma_totales()
                     END CASE
            END FOR
   END FOR
END FUNCTION

FUNCTION    F_220_arma_titulos()
   LET      g_tit_subcuenta           =  g_tit_subcuenta              CLIPPED,
            g_scta        USING   "&&           ","_"                 CLIPPED
END FUNCTION

FUNCTION    F_240_arma_detalles()
   LET      g_det_flujo               =  g_det_flujo                  CLIPPED,
            g_sie    USING    "&&"    CLIPPED, g_scta    USING  "&&"  CLIPPED,
            g_c_flujo[g_sie,g_scta].acciones  USING  "&&&&&&.&&&&&&"  CLIPPED,
            g_c_flujo[g_sie,g_scta].pesos     USING  "&&&&&&.&&&&&&"  CLIPPED

   LET      g_det_saldo               =  g_det_saldo                  CLIPPED,
            g_sie    USING    "&&"    CLIPPED, g_scta    USING  "&&"  CLIPPED,
            g_c_saldo[g_sie,g_scta].acciones  USING  "&&&&&&.&&&&&&"  CLIPPED,
            g_c_saldo[g_sie,g_scta].pesos     USING  "&&&&&&.&&&&&&"  CLIPPED

   LET      g_det_interes             =  g_det_interes                CLIPPED,
            g_sie    USING    "&&"    CLIPPED, g_scta    USING  "&&"  CLIPPED,
            g_resarcir[g_sie,g_scta].acciones  USING  "&&&&&&.&&&&&&"  CLIPPED,
            g_resarcir[g_sie,g_scta].pesos     USING  "&&&&&&.&&&&&&"  CLIPPED

   LET      g_det_nss                 =  g_det_nss                    CLIPPED,
            g_sie    USING    "&&"    CLIPPED, g_scta    USING  "&&"  CLIPPED,
            g_t_nss[g_sie,g_scta].acciones    USING  "&&&&&&.&&&&&&"  CLIPPED,
            g_t_nss[g_sie,g_scta].pesos       USING  "&&&&&&.&&&&&&"  CLIPPED
END FUNCTION

FUNCTION    F_260_arma_totales()
   LET      g_reg_total               =  g_reg_total                  CLIPPED,
            g_sie    USING    "&&", g_scta    USING  "&&",
            g_t_rep[g_sie,g_scta].acciones    USING  "&&&&&&.&&&&&&"  CLIPPED,
            g_t_rep[g_sie,g_scta].pesos       USING  "&&&&&&.&&&&&&"  CLIPPED
END FUNCTION

REPORT    R_220_rep_saldos_tot()
   OUTPUT
            PAGE     LENGTH   1
            LEFT     MARGIN   0
            RIGHT    MARGIN   0
            TOP      MARGIN   0
            BOTTOM   MARGIN   0
   FORMAT

   ON  EVERY  ROW

       IF       g_titulos          THEN
                PRINT    COLUMN   1,"01",g_today    USING  "DD/MM/YYYY"
                LET      g_titulos             =  0
       END IF

       PRINT    COLUMN   1,"02",g_nss,g_det_flujo
       PRINT    COLUMN   1,"03",g_nss,g_det_saldo
       PRINT    COLUMN   1,"04",g_nss,g_det_interes
       PRINT    COLUMN   1,"05",g_nss,g_det_nss

   ON  LAST  ROW
       LET      g_tipo_registro              =  3
       CALL     F_200_arma_registro()
       PRINT    COLUMN   1,"09",g_reg_total 
END REPORT

FUNCTION    F_500_arma_globales_para_reporte()
   DEFINE   l_tipo_desc                  CHAR(30)
   LET      g_tabname               = 'dis_provision  b '
   LET      g_folio                 =  taa_cd_ctr_folio_ind.folio
   SELECT   COUNT(UNIQUE   nss)
     INTO   g_total_cuentas
     FROM   safre_af:taa_cd_indebidos
    WHERE   folio                   =  g_folio
      AND   estado                  =  102;

   IF      (STATUS IS NULL)         THEN
            LET       g_total_cuentas          =  0
   END IF
   LET      g_fecha_accion              =   g_today

   LET      g_tipo_desc1                = 'TCAAC010'," ",l_tipo_desc CLIPPED,
            " ","POR TIPO DE TRASPASO"
   LET      g_tipo_desc2                = 'TCAAC010' ," ",l_tipo_desc CLIPPED,
            " ","POR SUBCUENTA"
   LET      g_nombre_programa           = "TCAAC010"
   LET      g_tip_rep                   = "PRO"

END FUNCTION

FUNCTION    F_900_fin()
   DEFINE   ejecuta                       CHAR(300)
   LET      ejecuta          =  "cd ",g_seg_modulo.ruta_exp CLIPPED,
                                "; fglgo    TCAAL009 ","1",g_folio
   RUN      ejecuta
   PROMPT   " Proceso Finalizado Teclee  <<Enter>> para Salir "  FOR  g_enter2
END FUNCTION

