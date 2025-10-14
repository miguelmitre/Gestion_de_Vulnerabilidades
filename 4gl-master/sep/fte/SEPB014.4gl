DATABASE safre_af

GLOBALS

DEFINE g_ind_edad_separado            ,
       g_ind_edad_separador           ,
       g_ind_trans_separado           ,
       g_ind_trans_separador          ,
       g_sie_nss_separado             ,
       g_sie_nss_separador            ,
       g_ind_trans_nss_separado       ,
       g_ind_trans_nss_separador      ,
       g_caso_total                   ,     
       g_ind_transferencia            ,
       g_cero                         ,
       g_cargo_mov_separacion         ,
       caso_separacion	              ,
       g_abono_mov_separacion         ,
       g_abono_mov_act_separacion     ,
       g_abono_mov_rend_separacion    ,
       g_cargo_mov_trans_separacion   ,
       g_abono_mov_trans_separacion   SMALLINT

DEFINE g_folio                        INTEGER

DEFINE g_rutas                        RECORD LIKE seg_modulo.*
DEFINE g_reg_mov_para_separar         RECORD LIKE dis_cuenta.*
DEFINE reg_dis_cuenta                 RECORD LIKE dis_cuenta.*

DEFINE g_plano_no_liga                CHAR(1000),
       g_plano_dis                    CHAR(1000),
       g_reg_pat_a_separar            CHAR(0011),
       g_enter                        CHAR(0001),
       g_nss_separado                 CHAR(0011),
       g_nss_separador                CHAR(0011),
       g_n_unico_separador            CHAR(0018),
       g_usuario                      CHAR(0010),
       g_diagnostico                  CHAR(0002),
       g_clasificacion                CHAR(0001),
       g_ejecuta                      CHAR(1000)

DEFINE g_fecha_viv                    ,
       g_today                        ,
       g_primer_habil                 ,
       g_primer_natural               DATE

DEFINE reg_dis_det_aporte             RECORD 
       rfc_patron                     CHAR(013),
       periodo_pago                   CHAR(006)
END RECORD

END  GLOBALS 

GLOBALS "SEPB015S.4gl" 

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

CALL STARTLOG("SEPB014.log")

# entrada de parametros externos

LET g_folio             = ARG_VAL(1) 
LET g_nss_separado      = ARG_VAL(2) 
LET g_nss_separador     = ARG_VAL(3) 
LET g_diagnostico       = ARG_VAL(4) 
LET g_clasificacion     = ARG_VAL(5) 

# IF de 01 C
IF (g_diagnostico   = '01' AND     # si los recursos pertenecen a 
    g_clasificacion = 'C' ) THEN   # mas de un solo trabajador

   LET g_ejecuta = "fglgo SEPM020",  # pantalla para elegir registro
                   g_folio       ,   # patronal
                   " "           ,
                   g_nss_separado,
                   " "           ,
                   g_nss_separador
   RUN g_ejecuta 

   SELECT "ok" 
   FROM   sep_reg_patro_separador a
   WHERE  a.folio         = g_folio 
   AND    a.nss_separado  = g_nss_separado
   AND    a.nss_separador = "ESC"
   GROUP BY 1

   IF STATUS <> NOTFOUND THEN            # cuando es modificacion 
      UPDATE sep_det_reg_sol_reclamante  # por separacion no se 
      SET    estado = 8 ,                # transfieren recursos
             fecha_proceso = g_primer_habil     # solo se actualiza a 
      WHERE  folio = g_folio             # liquidado
      AND    n_seguro = g_nss_separado 
      AND    nss      = g_nss_separador
      AND    estado   = 5

      DELETE FROM sep_reg_patro_separador 
      WHERE   folio         = g_folio
      AND     nss_separado  = g_nss_separado
      AND     nss_separador = "ESC"
   
      EXIT PROGRAM
   END IF
   
   SELECT "ok" 
   FROM   sep_reg_patro_separador a
   WHERE  a.folio         = g_folio 
   AND    a.nss_separado  = g_nss_separado
   AND    a.nss_separador = "INT"
   GROUP BY 1

   IF STATUS <> NOTFOUND THEN                #cuando se cancela
                                             #la pantalla de 
      DELETE FROM sep_reg_patro_separador    #eleccion de registro
      WHERE   folio = g_folio                #patronal
      AND     nss_separado = g_nss_separado
      AND     nss_separador = "INT"
   
      EXIT PROGRAM
   END IF

   ERROR ""

    OPEN   WINDOW  SEPM020  AT  2,2  WITH FORM "SEPM020" ATTRIBUTE(BORDER)
    DISPLAY  "                                                                                  "   AT  1,1  ATTRIBUTE  (REVERSE)
    DISPLAY  "SEPB014              SEPARACION DE REGISTROS PATRONALES                           "    AT  3,1 ATTRIBUTE  (REVERSE)

ELSE  #SI NO ES 01 C

    ERROR ""
    OPEN   WINDOW  SEPM020  AT  2,2  WITH FORM "SEPM020" ATTRIBUTE(BORDER)
    DISPLAY  "                                                                                  "   AT  1,1  ATTRIBUTE  (REVERSE)
    DISPLAY  "SEPB014              SEPARACION DE REGISTROS PATRONALES                           "    AT  3,1 ATTRIBUTE  (REVERSE)

    WHILE  TRUE
      PROMPT "   DESEA REALIZAR LA SEPARACION DE RECURSOS [S/N] ? " FOR g_enter
           IF    g_enter  MATCHES "[sSnN]" THEN
              IF   g_enter  MATCHES "[sS]" THEN
                     EXIT WHILE
                ELSE
                     EXIT PROGRAM
              END IF
           END IF
    END WHILE
END IF

DISPLAY "PROCESANDO INFORMACION..." AT 20,2

CALL inicio() 
CALL prepara_querys_SEPB015()
CALL trae_generales_separador()
CALL ejecuta_iniciales()
CALL clasifica_nss()

    IF g_caso_total <> 3 THEN
      CALL movimientos_a_separar_rcv() # apartar movimientos rcv
                                       # de manera automatica

      LET g_plano_no_liga = g_rutas.ruta_listados CLIPPED,"/",
                           g_usuario CLIPPED,".no_liga.",
                           g_folio USING"&&&&&&",".",
                           g_nss_separado CLIPPED,"-",
                           g_nss_separador

      SELECT "OK" FROM no_encontrados
      GROUP BY 1
     
      IF STATUS <> NOTFOUND THEN

         UNLOAD TO g_plano_no_liga 
         SELECT a.* 
         FROM   safre_af:dis_det_aporte a   ,
                no_encontrados          b
         WHERE  a.folio           = b.folio 
         AND    a.n_seguro        = b.nss
         AND    a.consec_reg_lote = b.consec_reg_lote

      END IF

--      CALL movimientos_a_separar_viv() # apartar movimientos viv 97
                                       # de manera automatica

      PREPARE sql_27 FROM g_sql_27         # inserta los movimientos elegidos
      EXECUTE sql_27 USING g_nss_separado  # manualmente 

      CALL provisiona_caso1()      
--      CALL provisiona_caso1_viv()      

      LET g_ejecuta = "fglgo SEPL014 ",g_folio," ",g_nss_separado," ",
                      g_nss_separador," ",g_diagnostico," ",g_clasificacion
      RUN g_ejecuta 
      ERROR ""

    ELSE 

    END IF

--LET g_plano_dis = g_rutas.ruta_listados CLIPPED,"/plano_dis.",
--                g_nss_separado CLIPPED,"_",g_nss_separador CLIPPED
          
--UNLOAD TO g_plano_dis
--SELECT * 
--FROM dis_cuenta
--WHERE folio = g_folio
--AND   nss  IN (g_nss_separado,g_nss_separador)

DELETE FROM dis_provision where folio = g_folio
                           and  nss in (g_nss_separado,g_nss_separador)

DELETE FROM  dis_cuenta where folio = g_folio
                           and  nss in (g_nss_separado,g_nss_separador)

DELETE FROM sep_reg_patro_separador 
WHERE folio         = g_folio 
AND   nss_separado  = g_nss_separado
AND   nss_separador = g_nss_separador


    PROMPT "REVISION CONCLUIDA...<Enter> PARA CONCLUIR" FOR char g_enter

    CLOSE WINDOW SEPM020

END MAIN

FUNCTION inicio()
#funcion para asignaciones iniciales
------------------------------------

WHENEVER ERROR CONTINUE
  DROP TABLE mov_para_separar
  DROP TABLE movimientos_separados
  DROP TABLE movimientos_pendientes
  DROP TABLE no_encontrados
  DROP TABLE tmp_viv_280
WHENEVER ERROR STOP

LET g_cero = 0
{
CREATE TABLE no_encontrados (
--CREATE TEMP TABLE no_encontrados (
     folio integer ,
     nss   char(011),
     consec_reg_lote integer)

CREATE   TABLE mov_para_separar(
--CREATE TEMP  TABLE mov_para_separar(
    tipo_movimiento   smallint     ,
    subcuenta         smallint     ,
    siefore           smallint     ,
    folio             integer      ,
    consecutivo_lote  integer      ,
    nss               char(11)     ,
    curp              char(18)     ,
    folio_sua         char(6)      ,
    fecha_pago        date         ,
    fecha_valor       date         ,
    fecha_conversion  date         ,
    monto_en_pesos    decimal(22,6),
    monto_en_acciones decimal(22,6),
    precio_accion     decimal(22,6),
    dias_cotizados    integer    ,
    sucursal          char(10)     ,
    id_aportante      char(11)     ,
    estado            smallint     ,
    fecha_proceso     date         ,
    usuario           char(8)      ,
    fecha_archivo     date         ,
    etiqueta          smallint)

CREATE  TABLE tmp_viv_280(
--CREATE TEMP TABLE tmp_viv_280(
    tipo_movimiento   smallint     ,
    subcuenta         smallint     ,
    siefore           smallint     ,
    folio             integer      ,
    consecutivo_lote  integer      ,
    nss               char(11)     ,
    curp              char(18)     ,
    folio_sua         char(6)      ,
    fecha_pago        date         ,
    fecha_valor       date         ,
    fecha_conversion  date         ,
    monto_en_pesos    decimal(22,6),
    monto_en_acciones decimal(22,6),
    precio_accion     decimal(22,6),
    dias_cotizados    integer    ,
    sucursal          char(10)     ,
    id_aportante      char(11)     ,
    estado            smallint     ,
    fecha_proceso     date         ,
    usuario           char(8)      ,
    fecha_archivo     date         ,
    etiqueta          smallint)

CREATE  TABLE movimientos_separados(
--CREATE TEMP TABLE movimientos_separados(
    tipo_movimiento   smallint     ,
    subcuenta         smallint     ,
    siefore           smallint     ,
    folio             integer      ,
    consecutivo_lote  integer      ,
    nss               char(11)     ,
    curp              char(18)     ,
    folio_sua         char(6)      ,
    fecha_pago        date         ,
    fecha_valor       date         ,
    fecha_conversion  date         ,
    monto_en_pesos    decimal(22,6),
    monto_en_acciones decimal(22,6),
    precio_accion     decimal(22,6),
    dias_cotizados    integer    ,
    sucursal          char(10)     ,
    id_aportante      char(11)     ,
    estado            smallint     ,
    fecha_proceso     date         ,
    usuario           char(8)      ,
    fecha_archivo     date         ,
    etiqueta          smallint)

CREATE  TABLE movimientos_pendientes(
--CREATE TEMP TABLE movimientos_pendientes(
    tipo_movimiento   smallint     ,
    subcuenta         smallint     ,
    siefore           smallint     ,
    folio             integer      ,
    consecutivo_lote  integer      ,
    nss               char(11)     ,
    curp              char(18)     ,
    folio_sua         char(6)      ,
    fecha_pago        date         ,
    fecha_valor       date         ,
    fecha_conversion  date         ,
    monto_en_pesos    decimal(22,6),
    monto_en_acciones decimal(22,6),
    precio_accion     decimal(22,6),
    dias_cotizados    integer      ,
    sucursal          char(10)     ,
    id_aportante      char(11)     ,
    estado            smallint     ,
    fecha_proceso     date         ,
    usuario           char(8)      ,
    fecha_archivo     date         ,
    etiqueta          smallint     ,
    indicador         smallint     )
}
 LET g_cargo_mov_separacion       = 280
 LET g_abono_mov_separacion       = 590
 LET g_abono_mov_act_separacion   = 595
 LET g_abono_mov_rend_separacion  = 596
 LET g_cargo_mov_trans_separacion = 210
 LET g_abono_mov_trans_separacion = 1
 LET g_today                      = TODAY
 LET g_fecha_viv      = MDY(MONTH(g_today),'01',YEAR(g_today))
 LET g_primer_natural = MDY(MONTH(g_today),'01',YEAR(g_today))
 CALL cal_fecha_avant(g_primer_natural,1) RETURNING g_primer_habil
END FUNCTION

FUNCTION prepara_querys_SEPB015()
#funcion para preparar querys de proceso

   CALL define_querys_SEPB015()

   PREPARE sql_01 FROM g_sql_01
   DECLARE cur_01 CURSOR FOR sql_01

   PREPARE sql_03 FROM g_sql_03 
   DECLARE cur_03 CURSOR FOR sql_03

   PREPARE sql_03_1 FROM g_sql_03_1
   DECLARE cur_03_1 CURSOR FOR sql_03_1

   PREPARE sql_04 FROM g_sql_04
   DECLARE cur_04 CURSOR FOR sql_04

   PREPARE sql_05 FROM g_sql_05
   DECLARE cur_05 CURSOR FOR sql_05

   PREPARE sql_08 FROM g_sql_08

   PREPARE sql_09 FROM g_sql_09

   PREPARE sql_07 FROM g_sql_07

   PREPARE sql_10 FROM g_sql_10

   PREPARE sql_11 FROM g_sql_11
   DECLARE cur_11 CURSOR FOR sql_11

   PREPARE sql_11_1 FROM g_sql_11_1
   DECLARE cur_11_1 CURSOR FOR sql_11_1

   PREPARE sql_12 FROM g_sql_12

   PREPARE sql_13 FROM g_sql_13
   DECLARE cur_13 CURSOR FOR sql_13

   PREPARE sql_13_x FROM g_sql_13_x
   DECLARE cur_13_x CURSOR FOR sql_13_x

   PREPARE sql_13_b FROM g_sql_13_b
   DECLARE cur_13_b CURSOR FOR sql_13_b

   PREPARE sql_13_c FROM g_sql_13_c

   PREPARE sql_14 FROM g_sql_14
   DECLARE cur_14 CURSOR FOR sql_14

   PREPARE sql_15 FROM g_sql_15

   PREPARE sql_16 FROM g_sql_16

   PREPARE sql_18 FROM g_sql_18
   DECLARE cur_18 CURSOR FOR sql_18

   PREPARE sql_18_1 FROM g_sql_18_1
   DECLARE cur_18_1 CURSOR FOR sql_18_1

   PREPARE sql_18_sar FROM g_sql_18_sar
   DECLARE cur_18_sar CURSOR FOR sql_18_sar

   PREPARE sql_17_1 FROM g_sql_17_1

   PREPARE sql_17_2 FROM g_sql_17_2

   PREPARE sql_19 FROM g_sql_19
   DECLARE cur_19 CURSOR FOR sql_19

   PREPARE sql_19_1 FROM g_sql_19_1
   DECLARE cur_19_1 CURSOR FOR sql_19_1

   PREPARE sql_19_sar FROM g_sql_19_sar
   DECLARE cur_19_sar CURSOR FOR sql_19_sar

   PREPARE sql_21 FROM g_sql_21
   DECLARE cur_21 CURSOR FOR sql_21

   PREPARE sql_22 FROM g_sql_22
   PREPARE sql_22_1 FROM g_sql_22_1
   PREPARE sql_22_2 FROM g_sql_22_2
   PREPARE sql_22_3 FROM g_sql_22_3

   PREPARE sql_23 FROM g_sql_23
   DECLARE cur_23 CURSOR FOR sql_23

   PREPARE sql_24 FROM g_sql_24

   PREPARE sql_25 FROM g_sql_25

   PREPARE sql_26 FROM g_sql_26

   PREPARE sql_28 FROM g_sql_28
   DECLARE cur_28 CURSOR FOR sql_28

   PREPARE sql_29 FROM g_sql_29
   DECLARE cur_29 CURSOR FOR sql_29

   PREPARE sql_30 FROM g_sql_30

   PREPARE sql_31 FROM g_sql_31

   PREPARE sql_32 FROM g_sql_32

   PREPARE sql_33 FROM g_sql_33

   PREPARE sql_34 FROM g_sql_34

END FUNCTION

#############################################################
# funcion para clasificar la manera de liquidar la separacion
#############################################################

FUNCTION clasifica_nss ()
#cnss-------------------

  LET g_ind_edad_separado   = 0     # ind_edad cta_ctr_cuenta
  LET g_ind_edad_separador  = 0

  LET g_ind_trans_separado  = 0     # ind_transferencia cta_ctr_cuenta
  LET g_ind_trans_separador = 0

  LET g_sie_nss_separado    = 0     # siefore_rcv cta_nss_regimen
  LET g_sie_nss_separador   = 0

  EXECUTE sql_08 USING g_nss_separado
                 INTO  g_ind_edad_separado,
                       g_ind_trans_separado

  EXECUTE sql_08 USING g_nss_separador
                 INTO  g_ind_edad_separador ,
                       g_ind_trans_separador

  EXECUTE sql_34 USING g_nss_separado
                 INTO  g_sie_nss_separado

  EXECUTE sql_34 USING g_nss_separador
                 INTO  g_sie_nss_separador

IF (g_diagnostico   = '01' AND 
    g_clasificacion = 'B' ) THEN
   LET g_caso_total = 3
ELSE 
   LET g_caso_total = 0
END IF

END FUNCTION

####################################################################
# trae generales de separador
####################################################################
FUNCTION trae_generales_separador()

EXECUTE sql_10 USING g_nss_separador       #trae la curp 
               INTO  g_n_unico_separador,  #del nss separador
                     g_usuario

END FUNCTION
####################################################################
# provisiona el cargo a la cuenta separada caso 1
####################################################################
FUNCTION prov_cargo(l_reg_prov_cargo)

DEFINE l_reg_prov_cargo RECORD 
       folio                 INTEGER    ,
       folio_sua             SMALLINT   ,
       nss                   CHAR(011)  ,
       subcuenta             SMALLINT   ,
       tipo_movimiento       SMALLINT   ,
       consecutivo_lote      INTEGER    ,
       siefore               SMALLINT   ,
       tot_acc_210           DEC(16,6)  ,
       tot_pesos_210         DEC(16,6)  ,
       id_aportante          char(010)  ,
       fecha_conversion      DATE
END RECORD
DEFINE l_verifica_provision SMALLINT

         FOREACH cur_04 USING l_reg_prov_cargo.*
                         INTO l_verifica_provision 
         END FOREACH

END FUNCTION
##############################################################
# liquida caso 1 
##############################################################
FUNCTION liquida_caso1()
DEFINE l_verifica_provision SMALLINT
DEFINE g_resp         SMALLINT 
DEFINE l_precio_part  DEC(10,6)
DEFINE l_part         DEC(16,2)
DEFINE l_reg_ajuste_prov RECORD 
       folio           INTEGER      ,
       folio_sua       CHAR(006)    ,
       nss             CHAR(011)    ,
       subcuenta       SMALLINT     ,
       tipo_movimiento SMALLINT     ,
       consecutivo     INTEGER      ,
       monto_accion    DECIMAL(16,2),
       monto_pesos     DECIMAL(16,2),
       id_aportante    CHAR(20)     ,
       fecha_proceso   DATE
END RECORD
DEFINE l_reg_ajuste_cargo RECORD 
       folio           INTEGER      ,
       folio_sua       CHAR(006)    ,
       nss             CHAR(011)    ,
       subcuenta       SMALLINT     ,
       tipo_movimiento SMALLINT     ,
       consecutivo     INTEGER      ,
       siefore         SMALLINT     ,
       monto_accion    DECIMAL(16,2),
       monto_pesos     DECIMAL(16,2),
       id_aportante    CHAR(20)     ,
       fecha_proceso   DATE
END RECORD

DEFINE l_saldo_viv        DEC(16,2)
DEFINE l_saldo_separa_viv DEC(16,2)
DEFINE l_ajuste           DEC(16,2)

LET g_resp = 5
LET l_part = 11
LET l_precio_part = 0
{   ##### ajuste operativo
IF g_caso_total <> 3 THEN

SELECT ABS(sum(a.monto_en_acciones))
INTO   l_saldo_viv
FROM   dis_provision a
WHERE  a.folio = g_folio 
AND    a.nss = g_nss_separado 
AND    a.tipo_movimiento = 280 
AND    a.subcuenta = 4

SELECT ABS(sum(a.monto_en_acciones))
INTO   l_saldo_separa_viv
FROM   dis_provision a
WHERE  a.folio     = g_folio 
AND    a.subcuenta = 4
AND    a.tipo_movimiento in (590,595,596,597) 

IF l_saldo_viv  <> l_saldo_separa_viv THEN
   IF l_saldo_viv > l_saldo_separa_viv THEN     # abono diferencia 620
       LET l_ajuste = l_saldo_viv - l_saldo_separa_viv

    LET l_reg_ajuste_prov.folio             = g_folio        
    LET l_reg_ajuste_prov.folio_sua         = "0"
    LET l_reg_ajuste_prov.nss               = g_nss_separado
    LET l_reg_ajuste_prov.subcuenta         = 4
    LET l_reg_ajuste_prov.tipo_movimiento   = 620
    LET l_reg_ajuste_prov.consecutivo       = 0
    LET l_reg_ajuste_prov.monto_accion      = l_ajuste
    
    EXECUTE sql_15 USING g_fecha_viv ,
                         l_part   
                   INTO  l_precio_part                 

    LET l_reg_ajuste_prov.monto_pesos       = l_reg_ajuste_prov.monto_accion * 
                                              l_precio_part
    
    LET l_reg_ajuste_prov.id_aportante      = 'AJUSTE SEP'
    LET l_reg_ajuste_prov.fecha_proceso     = g_today

    FOREACH cur_05 USING l_reg_ajuste_prov.*
                    INTO l_verifica_provision 
    END FOREACH
   END IF
   IF l_saldo_viv  < l_saldo_separa_viv THEN # cargo diferencia 620
       LET l_ajuste = l_saldo_separa_viv - l_saldo_viv

       LET l_reg_ajuste_cargo.folio           = g_folio        
       LET l_reg_ajuste_cargo.folio_sua       = "0"
       LET l_reg_ajuste_cargo.nss             = g_nss_separado
       LET l_reg_ajuste_cargo.subcuenta       = 4
       LET l_reg_ajuste_cargo.tipo_movimiento = 610
       LET l_reg_ajuste_cargo.consecutivo     = 0
       LET l_reg_ajuste_cargo.siefore         = 11
       LET l_reg_ajuste_cargo.monto_accion    = -l_ajuste
       
       EXECUTE sql_15 USING g_fecha_viv ,
                            l_part   
                      INTO  l_precio_part                 
   
       LET l_reg_ajuste_cargo.monto_pesos = -(l_reg_ajuste_prov.monto_accion * 
                                              l_precio_part)
       
       LET l_reg_ajuste_cargo.id_aportante  = 'CARGO AJUSTE SEP'
       LET l_reg_ajuste_cargo.fecha_proceso = g_today
   
       FOREACH cur_04 USING l_reg_ajuste_cargo.*
                       INTO l_verifica_provision 
       END FOREACH

     END IF
  END IF
END IF
}
EXECUTE sql_16 USING g_folio 
EXECUTE sql_24 USING g_folio
EXECUTE sql_25 USING g_primer_habil  ,
                     g_nss_separado  ,
                     g_nss_separador ,
                     g_resp
END FUNCTION

#################################################################
# ejecuta querys iniciales 
################################################################
FUNCTION ejecuta_iniciales()

  EXECUTE sql_32 USING "sep" INTO g_rutas.*  #rutas operativas

  FOREACH cur_01 INTO g_tabname        # lista las dis_cuenta
   CALL define_querys_SEPB015()        # existentes en taa_cd_tab_cuenta
   PREPARE sql_17 FROM g_sql_17        # e inserta todos los movimientos
   EXECUTE sql_17 USING g_nss_separado # del nss separado en mov_para_separar
  END FOREACH

   EXECUTE sql_17_1      # actualiza dias_cotizados a 0

   EXECUTE sql_17_2      # inserta en movimientos_pendientes
                         # todos los movimientos de mov_para_separar
END FUNCTION

#################################################################
# busca aportes 
#################################################################
FUNCTION provisiona_caso1()
#pc1-----------------------

DEFINE l_fecha_conver_210  DATE 

DEFINE l_precio_accion_210 ,
       l_precio_accion     ,
       l_acciones_210      ,
       l_acciones_210_c    ,
       l_pesos_210         DEC(16,6) 

DEFINE l_scta              ,
       l_verifica          ,
       l_sie_scta          SMALLINT

DEFINE l_folio_sua         CHAR(01),
       l_id_aportante      CHAR(010)

LET l_folio_sua    = " "
LET l_id_aportante = "CORTE"

      FOREACH cur_03_1 INTO  g_reg_mov_para_separar.*
              CALL prov_det_abo_caso1(g_reg_mov_para_separar.*) 
      END FOREACH

    IF g_ind_trans_separado = 4 THEN 

      EXECUTE sql_12 USING g_nss_separado
                     INTO  l_fecha_conver_210,
                           l_precio_accion_210 

         FOREACH cur_13  USING g_folio              ,
                               g_nss_separador      ,
                               l_fecha_conver_210
                          INTO l_scta               ,
                               l_acciones_210 

             EXECUTE sql_13_c USING g_folio            ,
                                    g_nss_separador    , 
                                    l_scta
                              INTO  l_acciones_210_c 
      
              IF l_acciones_210 <= 0 THEN 
                 CONTINUE FOREACH
              END IF 

              LET l_acciones_210   = l_acciones_210   * -1
              LET l_acciones_210_c = l_acciones_210_c * -1
              LET l_pesos_210      = l_acciones_210   * l_precio_accion_210

              ## SE PROVISIONA EL CARGO 210 CORTE TRANSVERSAL P/SEPARADOR
              ## UNICAMENTE LA PARTE SEPARADA DE ACCIONES HISTORICAS

              CALL prov_cargo( g_folio                      ,#folio
                               l_folio_sua                  ,#folio sua
                               g_nss_separador              ,#separador
                               l_scta                       ,#subcuenta
                               g_cargo_mov_trans_separacion ,#tipo movimiento(210)
                               0                            ,#consecutivo
                               2                            ,#siefore
                               l_acciones_210               ,#acciones
                               l_pesos_210                  ,#pesos
                               "CORTE"                      ,#id aportante
                               l_fecha_conver_210           )#fecha conversion

          LET l_pesos_210 = l_pesos_210 * -1

              ## SE PROVISIONA EL ABONO 1 CORTE TRANSVERSAL PARA EL SEPARADOR
              ## UNICAMENTE LA PARTE SEPARADA DE ACCIONES HISTORICAS
 
          FOREACH cur_14 USING g_folio                      ,#folio
                               l_folio_sua                  ,#folio sua
                               g_nss_separador              ,#separador
                               l_scta                       ,#subcuenta
                               g_abono_mov_trans_separacion ,#tipo movimiento(1)
                               g_cero                       ,#consecutivo
                               g_cero                       ,#acciones
                               l_pesos_210                  ,#pesos
                               l_id_aportante               ,#id aportante
                               l_fecha_conver_210            #fecha conversion
          INTO l_verifica

       END FOREACH                     

          ## SE PROVISIONA CARGO EN EL SEPARADO POR EL TOTAL DE ACCIONES PARA
          ## LA SUBCUENTA QUE SE ESTA TRABAJANDO

             LET l_precio_accion = 0

             EXECUTE sql_15 USING g_today            ,
                                  g_sie_nss_separado
                            INTO  l_precio_accion

             LET l_pesos_210 = l_acciones_210_c * l_precio_accion

             CALL prov_cargo( g_folio                        ,#folio separacion
                              l_folio_sua                    ,#folio sua
                              g_nss_separado                 ,#nss separado
                              l_scta                         ,#subcuenta
                              g_cargo_mov_separacion         ,#tipo_movimiento(290)
                              0                              ,#consecutivo 
                              g_sie_nss_separado             ,#siefore actual separado
                              l_acciones_210_c               ,#acciones
                              l_pesos_210                    ,#pesos
                              "SEPARACION"                   ,#id aportante
                              g_today                        )#fecha conversion 
        END FOREACH

      ELSE

         LET l_acciones_210  = 0

         FOREACH cur_13_b  USING g_folio        ,
                               g_nss_separador
                          INTO l_sie_scta       ,
                               l_scta           ,
                               l_acciones_210

           LET l_precio_accion = 0

           EXECUTE sql_15 USING g_today ,
                                l_sie_scta
                           INTO l_precio_accion

             LET l_pesos_210     = 0

             LET l_acciones_210 = l_acciones_210 * -1
             LET l_pesos_210    = l_acciones_210 * l_precio_accion

             CALL prov_cargo( g_folio                ,#folio separacion
                              l_folio_sua            ,#folio sua
                              g_nss_separado         ,#nss separado
                              l_scta                 ,#subcuenta
                              g_cargo_mov_separacion ,#mov 280
                              0                      ,#consecutivo 
                              l_sie_scta             ,#siefore act separado
                              l_acciones_210         ,#acciones
                              l_pesos_210            ,#pesos
                              "SEPARACION"           ,#id aportante
                              g_today                )#fecha conversion             
         END FOREACH
      END IF
END FUNCTION

#################################################################
# provisiona caso1  viv
#################################################################
FUNCTION provisiona_caso1_viv_sar()

DEFINE l_dis_folio INTEGER

DEFINE l_consec_reg_lote INTEGER
DEFINE l_fecha_conver_210 DATE ,
       l_precio_accion_210 DEC(16,6) ,
       l_precio_accion_sb1 DEC(16,6) ,
       l_scta   SMALLINT ,
       l_acciones   DEC(16,6), 
       l_pesos_210 DEC(16,6) ,
       l_fecha_avis DATE 
DEFINE l_verifica SMALLINT

DEFINE l_folio_sua char(01)
DEFINE l_id_aportante CHAR(010)
DEFINE l_sb1  SMALLINT
DEFINE l_sb2  SMALLINT
DEFINE l_movimiento SMALLINT

LET l_sb1 = 1
LET l_sb2 = 2
LET l_folio_sua = " "
LET l_id_aportante = "CORTE"

LET l_fecha_avis = '08/01/2004'

FOREACH cur_18_sar INTO g_reg_mov_para_separar.*

 CALL prov_det_cargo_caso1_viv(g_reg_mov_para_separar.*)

END FOREACH

      FOREACH cur_19_sar USING g_nss_separado    
                         INTO  g_reg_mov_para_separar.*
                   
      CASE g_reg_mov_para_separar.tipo_movimiento 
      WHEN 1 
         LET l_movimiento = g_abono_mov_separacion
      EXIT CASE
      WHEN 4 
         LET l_movimiento = g_abono_mov_separacion
      EXIT CASE
      WHEN 2
         LET l_movimiento = g_abono_mov_act_separacion
      EXIT CASE 
      WHEN 3 LET l_movimiento = g_abono_mov_rend_separacion 
      EXIT CASE
      END CASE

      IF (g_reg_mov_para_separar.fecha_valor <= l_fecha_avis AND 
          g_reg_mov_para_separar.tipo_movimiento <> 999      AND 
          g_reg_mov_para_separar.tipo_movimiento <> 990      AND 
          g_reg_mov_para_separar.tipo_movimiento <> 991      AND 
          g_reg_mov_para_separar.id_aportante <> 'REMANENTE'   ) THEN 

IF g_reg_mov_para_separar.fecha_valor < '07/01/1997' THEN 
     LET g_reg_mov_para_separar.fecha_valor = 
         MDY(MONTH(g_reg_mov_para_separar.fecha_proceso),'01',
             YEAR(g_reg_mov_para_separar.fecha_proceso))
END IF
             FOREACH cur_23 USING g_reg_mov_para_separar.monto_en_pesos,
                                  g_reg_mov_para_separar.fecha_valor   ,
                                  l_fecha_avis
                            INTO  l_acciones
             END FOREACH
           
           IF (l_acciones < 0 AND 
               g_reg_mov_para_separar.tipo_movimiento <> 610 AND
               g_reg_mov_para_separar.tipo_movimiento <> 270 ) THEN
             ERROR"1 REGISTRO CON DISPOSICIÓN DE RECURSOS...NO PUEDE SEPARARSE",
              "..REGRESANDO"

             DELETE FROM dis_provision
             WHERE  folio = g_folio
             AND    nss   in (g_nss_separado, g_nss_separador)
 
             DELETE FROM sep_reg_patro_separador
             WHERE  folio = g_folio
             AND    nss_separado = g_nss_separado
             AND    nss_separador = g_nss_separador
        
             SLEEP 5
           END IF 

IF g_reg_mov_para_separar.fecha_valor < '07/01/1997' THEN 
     LET g_reg_mov_para_separar.fecha_valor = 
         MDY(MONTH(g_reg_mov_para_separar.fecha_proceso),'01',
             YEAR(g_reg_mov_para_separar.fecha_proceso))
END IF
 
             FOREACH cur_21  USING g_folio                          ,
                                   g_reg_mov_para_separar.folio_sua ,
                                   g_nss_separador                  ,
                                   g_reg_mov_para_separar.subcuenta ,
                                   l_movimiento                     ,
                                   g_reg_mov_para_separar.consecutivo_lote ,
                                   l_acciones                              ,
                                   g_reg_mov_para_separar.monto_en_pesos   ,
                                   g_reg_mov_para_separar.id_aportante     ,
                                   g_reg_mov_para_separar.fecha_pago       ,
                                   g_reg_mov_para_separar.fecha_valor      ,
                                   g_reg_mov_para_separar.fecha_conversion 
             INTO l_verifica
                EXECUTE sql_22 USING g_reg_mov_para_separar.tipo_movimiento    ,
                             g_reg_mov_para_separar.subcuenta          ,
                             g_reg_mov_para_separar.siefore            ,
                             g_reg_mov_para_separar.folio              ,
                             g_reg_mov_para_separar.consecutivo_lote   ,
                             g_reg_mov_para_separar.nss                ,
                         --    g_reg_mov_para_separar.folio_sua          ,
                             g_reg_mov_para_separar.fecha_pago        ,
                             g_reg_mov_para_separar.fecha_valor       ,
                             g_reg_mov_para_separar.fecha_conversion  ,
                             g_reg_mov_para_separar.monto_en_pesos    ,
                             g_reg_mov_para_separar.monto_en_acciones ,
                             g_reg_mov_para_separar.precio_accion     ,
                             g_reg_mov_para_separar.dias_cotizados    ,
                             g_reg_mov_para_separar.id_aportante      ,
                             g_reg_mov_para_separar.estado            ,
                             g_reg_mov_para_separar.fecha_proceso     ,
                             g_reg_mov_para_separar.usuario           ,
                             g_reg_mov_para_separar.etiqueta       
             END FOREACH
      END IF
      IF (g_reg_mov_para_separar.fecha_valor > l_fecha_avis AND 
          g_reg_mov_para_separar.tipo_movimiento <> 999      AND 
          g_reg_mov_para_separar.tipo_movimiento <> 990      AND 
          g_reg_mov_para_separar.tipo_movimiento <> 991   ) THEN 

             FOREACH cur_21  USING g_folio                          ,
                                   g_reg_mov_para_separar.folio_sua ,
                                   g_nss_separador                  ,
                                   g_reg_mov_para_separar.subcuenta ,
                                   l_movimiento                     ,
                                   g_reg_mov_para_separar.consecutivo_lote ,
                                   g_reg_mov_para_separar.monto_en_acciones,
                                   g_reg_mov_para_separar.monto_en_pesos   ,
                                   g_reg_mov_para_separar.id_aportante     ,
                                   g_reg_mov_para_separar.fecha_pago       ,
                                   g_reg_mov_para_separar.fecha_valor      ,
                                   g_reg_mov_para_separar.fecha_conversion
             INTO l_verifica
                EXECUTE sql_22 USING g_reg_mov_para_separar.tipo_movimiento    ,
                             g_reg_mov_para_separar.subcuenta          ,
                             g_reg_mov_para_separar.siefore            ,
                             g_reg_mov_para_separar.folio              ,
                             g_reg_mov_para_separar.consecutivo_lote   ,
                             g_reg_mov_para_separar.nss                ,
                            -- g_reg_mov_para_separar.folio_sua          ,
                             g_reg_mov_para_separar.fecha_pago        ,
                             g_reg_mov_para_separar.fecha_valor       ,
                             g_reg_mov_para_separar.fecha_conversion  ,
                             g_reg_mov_para_separar.monto_en_pesos    ,
                             g_reg_mov_para_separar.monto_en_acciones ,
                             g_reg_mov_para_separar.precio_accion     ,
                             g_reg_mov_para_separar.dias_cotizados    ,
                             g_reg_mov_para_separar.id_aportante      ,
                             g_reg_mov_para_separar.estado            ,
                             g_reg_mov_para_separar.fecha_proceso     ,
                             g_reg_mov_para_separar.usuario           ,
                             g_reg_mov_para_separar.etiqueta

             END FOREACH
      END IF
    END FOREACH

    FOREACH cur_18_sar INTO g_reg_mov_para_separar.*

      CASE g_reg_mov_para_separar.tipo_movimiento 
      WHEN 1 
         LET l_movimiento = g_abono_mov_separacion
      EXIT CASE
      WHEN 4 
         LET l_movimiento = g_abono_mov_separacion
      EXIT CASE
      WHEN 2
         LET l_movimiento = g_abono_mov_act_separacion
      EXIT CASE 
      WHEN 3 LET l_movimiento = g_abono_mov_rend_separacion 
      EXIT CASE
      END CASE

      IF g_reg_mov_para_separar.tipo_movimiento = 3 THEN 
         CONTINUE FOREACH
      END IF

      IF (g_reg_mov_para_separar.fecha_valor <= l_fecha_avis AND 
          g_reg_mov_para_separar.tipo_movimiento <> 999      AND 
          g_reg_mov_para_separar.tipo_movimiento <> 990      AND 
          g_reg_mov_para_separar.tipo_movimiento <> 991      AND 
          g_reg_mov_para_separar.id_aportante <> 'REMANENTE' ) THEN 

IF g_reg_mov_para_separar.fecha_valor < '07/01/1997' THEN 
     LET g_reg_mov_para_separar.fecha_valor = 
         MDY(MONTH(g_reg_mov_para_separar.fecha_proceso),'01',
             YEAR(g_reg_mov_para_separar.fecha_proceso))
END IF
             FOREACH cur_23 USING g_reg_mov_para_separar.monto_en_pesos,
                                  g_reg_mov_para_separar.fecha_valor   ,
                                  l_fecha_avis
                            INTO  l_acciones
             END FOREACH
           
           IF (l_acciones < 0 AND
               g_reg_mov_para_separar.tipo_movimiento <> 610 AND 
               g_reg_mov_para_separar.tipo_movimiento <> 270) THEN
             ERROR
             "2 REGISTRO CON DISPOSICIÓN DE RECURSOS...NO PUEDE SEPARARSE",
              "..REGRESANDO"

             DELETE FROM dis_provision
             WHERE  folio = g_folio
             AND    nss   in (g_nss_separado, g_nss_separador)
 
             DELETE FROM sep_reg_patro_separador
             WHERE  folio = g_folio
             AND    nss_separado = g_nss_separado
             AND    nss_separador = g_nss_separador
        
             SLEEP 5
           END IF 

IF g_reg_mov_para_separar.fecha_valor < '07/01/1997' THEN 
     LET g_reg_mov_para_separar.fecha_valor = 
         MDY(MONTH(g_reg_mov_para_separar.fecha_proceso),'01',
             YEAR(g_reg_mov_para_separar.fecha_proceso))
END IF
             FOREACH cur_21  USING g_folio                          ,
                                   g_reg_mov_para_separar.folio_sua ,
                                   g_nss_separado                   ,
                                   g_reg_mov_para_separar.subcuenta ,
                                   l_movimiento                     ,
                                   g_reg_mov_para_separar.consecutivo_lote ,
                                   l_acciones                              ,
                                   g_reg_mov_para_separar.monto_en_pesos   ,
                                   g_reg_mov_para_separar.id_aportante     ,
                                   g_reg_mov_para_separar.fecha_pago       ,
                                   g_reg_mov_para_separar.fecha_valor      ,
                                   g_reg_mov_para_separar.fecha_conversion 
             INTO l_verifica
         --       EXECUTE sql_22 USING g_reg_mov_para_separar.*
             END FOREACH
      END IF
      IF (g_reg_mov_para_separar.fecha_valor > l_fecha_avis AND 
          g_reg_mov_para_separar.tipo_movimiento <> 999      AND 
          g_reg_mov_para_separar.tipo_movimiento <> 990      AND 
          g_reg_mov_para_separar.tipo_movimiento <> 991   ) THEN 

             FOREACH cur_21  USING g_folio                          ,
                                   g_reg_mov_para_separar.folio_sua ,
                                   g_nss_separado                   ,
                                   g_reg_mov_para_separar.subcuenta ,
                                   l_movimiento                     ,
                                   g_reg_mov_para_separar.consecutivo_lote ,
                                   g_reg_mov_para_separar.monto_en_acciones,
                                   g_reg_mov_para_separar.monto_en_pesos   ,
                                   g_reg_mov_para_separar.id_aportante     ,
                                   g_reg_mov_para_separar.fecha_pago       ,
                                   g_reg_mov_para_separar.fecha_valor      ,
                                   g_reg_mov_para_separar.fecha_conversion
             INTO l_verifica
          --      EXECUTE sql_22 USING g_reg_mov_para_separar.*
             END FOREACH
      END IF
    END FOREACH
END FUNCTION

FUNCTION provisiona_caso1_viv()
#pcv------------------------------

DEFINE l_dis_folio INTEGER

DEFINE l_consec_reg_lote INTEGER
DEFINE l_fecha_conver_210 DATE ,
       l_precio_accion_210 DEC(16,6) ,
       l_precio_accion_sb1 DEC(16,6) ,
       l_scta   SMALLINT ,
       l_acciones   DEC(16,6), 
       l_pesos_210 DEC(16,6) ,
       l_fecha_avis DATE 
DEFINE l_verifica SMALLINT
DEFINE l_fecha_valor_ajuste DATE
DEFINE l_folio_sua char(01)
DEFINE l_id_aportante CHAR(010)
DEFINE l_sb1  SMALLINT
DEFINE l_sb2  SMALLINT
DEFINE l_movimiento SMALLINT

LET l_sb1 = 1
LET l_sb2 = 2
LET l_folio_sua = " "
LET l_id_aportante = "CORTE"

LET l_fecha_avis = '08/01/2004'

FOREACH cur_18 INTO g_reg_mov_para_separar.*

 CALL prov_det_cargo_caso1_viv(g_reg_mov_para_separar.*)

END FOREACH

FOREACH cur_19_1 INTO  g_reg_mov_para_separar.*
                   
      CASE g_reg_mov_para_separar.tipo_movimiento 
      WHEN 1 
         LET l_movimiento = g_abono_mov_separacion
      EXIT CASE
      WHEN 2
         LET l_movimiento = g_abono_mov_act_separacion
      EXIT CASE 
      WHEN 3 LET l_movimiento = g_abono_mov_rend_separacion 
      OTHERWISE  LET l_movimiento = 597
        EXIT CASE
      EXIT CASE
      END CASE

      IF (g_reg_mov_para_separar.fecha_valor <= l_fecha_avis AND 
          g_reg_mov_para_separar.tipo_movimiento <> 999      AND 
          g_reg_mov_para_separar.tipo_movimiento <> 990      AND 
          g_reg_mov_para_separar.tipo_movimiento <> 991   ) THEN 

          IF g_reg_mov_para_separar.fecha_valor < '07/01/1997' THEN 
             LET l_fecha_valor_ajuste = g_reg_mov_para_separar.fecha_valor
             LET g_reg_mov_para_separar.fecha_valor = 
                 MDY(MONTH(g_reg_mov_para_separar.fecha_proceso),'01',
                     YEAR(g_reg_mov_para_separar.fecha_proceso))
          END IF

             FOREACH cur_23 USING g_reg_mov_para_separar.monto_en_pesos,
                                  g_reg_mov_para_separar.fecha_valor   ,
                                  l_fecha_avis
                            INTO  l_acciones
             END FOREACH
           
           IF (l_acciones < 0 AND
               g_reg_mov_para_separar.tipo_movimiento <> 610 AND
               g_reg_mov_para_separar.tipo_movimiento <> 270 ) THEN
             ERROR"3 REGISTRO CON DISPOSICIÓN DE RECURSOS...NO PUEDE SEPARARSE",
              "..REGRESANDO"

             DELETE FROM dis_provision
             WHERE  folio = g_folio
             AND    nss   in (g_nss_separado, g_nss_separador)
 
             DELETE FROM sep_reg_patro_separador
             WHERE  folio = g_folio
             AND    nss_separado = g_nss_separado
             AND    nss_separador = g_nss_separador
        
             SLEEP 5
           END IF 

           IF g_reg_mov_para_separar.fecha_valor < '07/01/1997' THEN 
             LET l_fecha_valor_ajuste = g_reg_mov_para_separar.fecha_valor
               LET g_reg_mov_para_separar.fecha_valor = 
                   MDY(MONTH(g_reg_mov_para_separar.fecha_proceso),'01',
                   YEAR(g_reg_mov_para_separar.fecha_proceso))
           END IF
 
             FOREACH cur_21  USING g_folio                          ,
                                   g_reg_mov_para_separar.folio_sua ,
                                   g_nss_separador                  ,
                                   g_reg_mov_para_separar.subcuenta ,
                                   l_movimiento                     ,
                                   g_reg_mov_para_separar.consecutivo_lote ,
                                   l_acciones                              ,
                                   g_reg_mov_para_separar.monto_en_pesos   ,
                                   g_reg_mov_para_separar.id_aportante     ,
                                   g_reg_mov_para_separar.fecha_pago       ,
                                   g_reg_mov_para_separar.fecha_valor      ,
                                   g_reg_mov_para_separar.fecha_conversion 
             INTO l_verifica
                IF g_reg_mov_para_separar.tipo_movimiento =  640 THEN
                   Let g_reg_mov_para_separar.fecha_valor = 
                   l_fecha_valor_ajuste
                END IF
                EXECUTE sql_22 USING 
                             g_reg_mov_para_separar.tipo_movimiento    ,
                             g_reg_mov_para_separar.subcuenta          ,
                             g_reg_mov_para_separar.siefore            ,
                             g_reg_mov_para_separar.folio              ,
                             g_reg_mov_para_separar.consecutivo_lote   ,
                             g_reg_mov_para_separar.nss                ,
                       --      g_reg_mov_para_separar.folio_sua          ,
                             g_reg_mov_para_separar.fecha_pago        ,
                             g_reg_mov_para_separar.fecha_valor       ,
                             g_reg_mov_para_separar.fecha_conversion  ,
                             g_reg_mov_para_separar.monto_en_pesos    ,
                             g_reg_mov_para_separar.monto_en_acciones ,
                             g_reg_mov_para_separar.precio_accion     ,
                             g_reg_mov_para_separar.dias_cotizados    ,
                             g_reg_mov_para_separar.id_aportante      ,
                             g_reg_mov_para_separar.estado            ,
                             g_reg_mov_para_separar.fecha_proceso     ,
                             g_reg_mov_para_separar.usuario           ,
                             g_reg_mov_para_separar.etiqueta

             END FOREACH
      END IF
      IF (g_reg_mov_para_separar.fecha_valor > l_fecha_avis AND 
          g_reg_mov_para_separar.tipo_movimiento <> 999      AND 
          g_reg_mov_para_separar.tipo_movimiento <> 990      AND 
          g_reg_mov_para_separar.tipo_movimiento <> 991   ) THEN 

             FOREACH cur_21  USING g_folio                          ,
                                   g_reg_mov_para_separar.folio_sua ,
                                   g_nss_separador                  ,
                                   g_reg_mov_para_separar.subcuenta ,
                                   l_movimiento                     ,
                                   g_reg_mov_para_separar.consecutivo_lote ,
                                   g_reg_mov_para_separar.monto_en_acciones,
                                   g_reg_mov_para_separar.monto_en_pesos   ,
                                   g_reg_mov_para_separar.id_aportante     ,
                                   g_reg_mov_para_separar.fecha_pago       ,
                                   g_reg_mov_para_separar.fecha_valor      ,
                                   g_reg_mov_para_separar.fecha_conversion
             INTO l_verifica
                EXECUTE sql_22 USING g_reg_mov_para_separar.tipo_movimiento    ,
                             g_reg_mov_para_separar.subcuenta          ,
                             g_reg_mov_para_separar.siefore            ,
                             g_reg_mov_para_separar.folio              ,
                             g_reg_mov_para_separar.consecutivo_lote   ,
                             g_reg_mov_para_separar.nss                ,
                         --    g_reg_mov_para_separar.folio_sua          ,
                             g_reg_mov_para_separar.fecha_pago        ,
                             g_reg_mov_para_separar.fecha_valor       ,
                             g_reg_mov_para_separar.fecha_conversion  ,
                             g_reg_mov_para_separar.monto_en_pesos    ,
                             g_reg_mov_para_separar.monto_en_acciones ,
                             g_reg_mov_para_separar.precio_accion     ,
                             g_reg_mov_para_separar.dias_cotizados    ,
                             g_reg_mov_para_separar.id_aportante      ,
                             g_reg_mov_para_separar.estado            ,
                             g_reg_mov_para_separar.fecha_proceso     ,
                             g_reg_mov_para_separar.usuario           ,
                             g_reg_mov_para_separar.etiqueta

             END FOREACH
      END IF
END FOREACH

    FOREACH cur_18 INTO g_reg_mov_para_separar.*

      CASE g_reg_mov_para_separar.tipo_movimiento 
      WHEN 1 
         LET l_movimiento = g_abono_mov_separacion
      EXIT CASE
      WHEN 2
         LET l_movimiento = g_abono_mov_act_separacion
      EXIT CASE 
      WHEN 3 LET l_movimiento = 3
      EXIT CASE
      OTHERWISE LET l_movimiento = 597
      END CASE

      IF l_movimiento  = 3 THEN 
         CONTINUE FOREACH
      END IF

      IF (g_reg_mov_para_separar.fecha_valor <= l_fecha_avis AND 
          g_reg_mov_para_separar.tipo_movimiento <> 999      AND 
          g_reg_mov_para_separar.tipo_movimiento <> 990      AND 
          g_reg_mov_para_separar.tipo_movimiento <> 991   ) THEN 

IF g_reg_mov_para_separar.fecha_valor < '07/01/1997' THEN 
     LET l_fecha_valor_ajuste = g_reg_mov_para_separar.fecha_valor
     LET g_reg_mov_para_separar.fecha_valor = 
         MDY(MONTH(g_reg_mov_para_separar.fecha_proceso),'01',
             YEAR(g_reg_mov_para_separar.fecha_proceso))
END IF



             FOREACH cur_23 USING g_reg_mov_para_separar.monto_en_pesos,
                                  g_reg_mov_para_separar.fecha_valor   ,
                                  l_fecha_avis
                            INTO  l_acciones
             END FOREACH
           
           IF (l_acciones < 0 AND
               g_reg_mov_para_separar.tipo_movimiento <> 610 AND
               g_reg_mov_para_separar.tipo_movimiento <> 270 ) THEN
             ERROR
             "4 REGISTRO CON DISPOSICIÓN DE RECURSOS...NO PUEDE SEPARARSE",
              "..REGRESANDO"
DISPLAY g_reg_mov_para_separar.folio       
DISPLAY g_reg_mov_para_separar.fecha_conversion
DISPLAY g_reg_mov_para_separar.subcuenta
DISPLAY g_reg_mov_para_separar.monto_en_pesos
PROMPT g_reg_mov_para_separar.tipo_movimiento for char g_enter
             DELETE FROM dis_provision
             WHERE  folio = g_folio
             AND    nss   in (g_nss_separado, g_nss_separador)
 
             DELETE FROM sep_reg_patro_separador
             WHERE  folio = g_folio
             AND    nss_separado = g_nss_separado
             AND    nss_separador = g_nss_separador
        
             SLEEP 5
           END IF 

IF g_reg_mov_para_separar.fecha_valor < '07/01/1997' THEN 
     LET l_fecha_valor_ajuste = g_reg_mov_para_separar.fecha_valor
     LET g_reg_mov_para_separar.fecha_valor = 
         MDY(MONTH(g_reg_mov_para_separar.fecha_proceso),'01',
             YEAR(g_reg_mov_para_separar.fecha_proceso))
END IF

             FOREACH cur_21  USING g_folio                          ,
                                   g_reg_mov_para_separar.folio_sua ,
                                   g_nss_separado                   ,
                                   g_reg_mov_para_separar.subcuenta ,
                                   l_movimiento                     ,
                                   g_reg_mov_para_separar.consecutivo_lote ,
                                   l_acciones                              ,
                                   g_reg_mov_para_separar.monto_en_pesos   ,
                                   g_reg_mov_para_separar.id_aportante     ,
                                   g_reg_mov_para_separar.fecha_pago       ,
                                   g_reg_mov_para_separar.fecha_valor      ,
                                   g_reg_mov_para_separar.fecha_conversion 
             INTO l_verifica
         --       EXECUTE sql_22 USING g_reg_mov_para_separar.*
             END FOREACH
      END IF
      IF (g_reg_mov_para_separar.fecha_valor > l_fecha_avis AND 
          g_reg_mov_para_separar.tipo_movimiento <> 999      AND 
          g_reg_mov_para_separar.tipo_movimiento <> 990      AND 
          g_reg_mov_para_separar.tipo_movimiento <> 991   ) THEN 

             FOREACH cur_21  USING g_folio                          ,
                                   g_reg_mov_para_separar.folio_sua ,
                                   g_nss_separado                   ,
                                   g_reg_mov_para_separar.subcuenta ,
                                   l_movimiento                     ,
                                   g_reg_mov_para_separar.consecutivo_lote ,
                                   g_reg_mov_para_separar.monto_en_acciones,
                                   g_reg_mov_para_separar.monto_en_pesos   ,
                                   g_reg_mov_para_separar.id_aportante     ,
                                   g_reg_mov_para_separar.fecha_pago       ,
                                   g_reg_mov_para_separar.fecha_valor      ,
                                   g_reg_mov_para_separar.fecha_conversion
             INTO l_verifica
          --      EXECUTE sql_22 USING g_reg_mov_para_separar.*
             END FOREACH
      END IF
    END FOREACH
END FUNCTION

#########################################################################
# provision de los abonos del caso1
#########################################################################
FUNCTION prov_det_abo_caso1(l_abono)

DEFINE l_abono RECORD LIKE dis_cuenta.*
DEFINE l_movimiento SMALLINT

CASE l_abono.tipo_movimiento 
WHEN 1 
   LET l_movimiento = g_abono_mov_separacion
   EXIT CASE
WHEN 2
   LET l_movimiento = g_abono_mov_act_separacion
   EXIT CASE
WHEN 3 
   LET l_movimiento = g_abono_mov_rend_separacion
   EXIT CASE
WHEN 4 
   LET l_movimiento = g_abono_mov_rend_separacion
   EXIT CASE
WHEN 5 
   LET l_movimiento = g_abono_mov_rend_separacion
   EXIT CASE
WHEN 100 
   LET l_movimiento = l_abono.tipo_movimiento
   EXIT CASE
WHEN 101 
   LET l_movimiento = l_abono.tipo_movimiento
   EXIT CASE
WHEN 102 
   LET l_movimiento = l_abono.tipo_movimiento
   EXIT CASE
WHEN 103 
   LET l_movimiento = l_abono.tipo_movimiento
   EXIT CASE
WHEN 104 
   LET l_movimiento = l_abono.tipo_movimiento
   EXIT CASE
WHEN 105 
   LET l_movimiento = l_abono.tipo_movimiento
   EXIT CASE
WHEN 106 
   LET l_movimiento = l_abono.tipo_movimiento
   EXIT CASE
WHEN 107 
   LET l_movimiento = l_abono.tipo_movimiento
   EXIT CASE
WHEN 108 
   LET l_movimiento = l_abono.tipo_movimiento
   EXIT CASE
WHEN 109 
   LET l_movimiento = l_abono.tipo_movimiento
   EXIT CASE
WHEN 110
   LET l_movimiento = 110
   EXIT CASE
OTHERWISE 
   LET l_movimiento = l_abono.tipo_movimiento
   EXIT CASE
END CASE

 CASE l_abono.tipo_movimiento 
 WHEN 990 
   LET l_movimiento = l_abono.tipo_movimiento
   EXIT CASE
 WHEN 991 
   LET l_movimiento = l_abono.tipo_movimiento
   EXIT CASE
 END CASE


EXECUTE sql_09 USING  l_movimiento             ,
                      l_abono.subcuenta        ,
                      l_abono.siefore          ,
                      g_folio                  ,
                      l_abono.consecutivo_lote ,
                      g_nss_separador          ,
                      g_n_unico_separador      ,
                      l_abono.folio_sua        ,
                      l_abono.fecha_pago       ,
                      l_abono.fecha_valor      ,
                      l_abono.fecha_conversion ,
                      l_abono.monto_en_pesos   ,
                      l_abono.monto_en_acciones,
                      l_abono.precio_accion    ,
                      l_abono.dias_cotizados   ,
                      l_abono.sucursal         ,
                      l_abono.id_aportante     ,
                      l_abono.estado           ,
                      g_today                  ,
                      g_usuario                ,
                      l_abono.fecha_archivo    ,
                      l_abono.etiqueta    

END FUNCTION

##########################################################
# funcion para realizar el cargo de viv en el separado
##########################################################
FUNCTION prov_det_cargo_caso1_viv(l_cargo)

DEFINE l_cargo RECORD LIKE dis_cuenta.*
DEFINE l_movimiento SMALLINT
DEFINE l_fecha_valor  DATE

LET  l_fecha_valor = MDY(MONTH(g_today),"01",YEAR(g_today))

LET l_movimiento = 280

LET l_cargo.monto_en_pesos = l_cargo.monto_en_pesos * -1
LET l_cargo.monto_en_acciones = l_cargo.monto_en_acciones * -1


IF (l_cargo.fecha_valor     <= '08/01/2004' AND 
    l_cargo.tipo_movimiento <> 999          AND
    l_cargo.tipo_movimiento <> 990          AND
    l_cargo.tipo_movimiento <> 991          AND 
    l_cargo.fecha_conversion <= '08/01/2004'   ) THEN
   

EXECUTE sql_09 USING  l_movimiento             ,
                      l_cargo.subcuenta        ,
                      l_cargo.siefore          ,
                      g_folio                  ,
                      l_cargo.consecutivo_lote ,
                      g_nss_separado           ,
                      l_cargo.curp             , 
                      l_cargo.folio_sua        ,
                      l_cargo.fecha_pago       ,
                      l_fecha_valor            ,
                      g_today                  ,
                      l_cargo.monto_en_pesos   ,
                      l_cargo.monto_en_pesos   ,
                      l_cargo.precio_accion    ,
                      l_cargo.dias_cotizados   ,
                      l_cargo.sucursal         ,
                      l_cargo.id_aportante     ,
                      l_cargo.estado           ,
                      g_today                  ,
                      g_usuario                ,
                      l_cargo.fecha_archivo    ,
                      l_cargo.etiqueta    

EXECUTE sql_07 USING  l_movimiento             ,
                      l_cargo.subcuenta        ,
                      l_cargo.siefore          ,
                      g_folio                  ,
                      l_cargo.consecutivo_lote ,
                      g_nss_separado           ,
                      l_cargo.curp             , 
                      l_cargo.folio_sua        ,
                      l_cargo.fecha_pago       ,
                      l_cargo.fecha_valor      ,
                      l_cargo.fecha_conversion ,
                      l_cargo.monto_en_pesos   ,
                      l_cargo.monto_en_pesos   ,
                      l_cargo.precio_accion    ,
                      l_cargo.dias_cotizados   ,
                      l_cargo.sucursal         ,
                      l_cargo.id_aportante     ,
                      l_cargo.estado           ,
                      g_today                  ,
                      g_usuario                ,
                      l_cargo.fecha_archivo    ,
                      l_cargo.etiqueta    
END IF

IF ((l_cargo.fecha_valor     > '08/01/2004' OR 
     l_cargo.fecha_valor     <= '08/01/2004' ) AND 
    l_cargo.tipo_movimiento <> 999         AND
    l_cargo.tipo_movimiento <> 990         AND
    l_cargo.tipo_movimiento <> 991         AND 
    l_cargo.fecha_conversion > '08/01/2004'  ) THEN

EXECUTE sql_09 USING  l_movimiento             ,
                      l_cargo.subcuenta        ,
                      l_cargo.siefore          ,
                      g_folio                  ,
                      l_cargo.consecutivo_lote ,
                      g_nss_separado           ,
                      l_cargo.curp           , 
                      l_cargo.folio_sua        ,
                      l_cargo.fecha_pago       ,
                      l_fecha_valor            ,
                      g_today                  ,
                      l_cargo.monto_en_pesos   ,
                      l_cargo.monto_en_acciones,
                      l_cargo.precio_accion    ,
                      l_cargo.dias_cotizados   ,
                      l_cargo.sucursal         ,
                      l_cargo.id_aportante     ,
                      l_cargo.estado           ,
                      g_today                  ,
                      g_usuario                ,
                      l_cargo.fecha_archivo    ,
                      l_cargo.etiqueta    

EXECUTE sql_07 USING  l_movimiento             ,
                      l_cargo.subcuenta        ,
                      l_cargo.siefore          ,
                      g_folio                  ,
                      l_cargo.consecutivo_lote ,
                      g_nss_separado           ,
                      l_cargo.curp             , 
                      l_cargo.folio_sua        ,
                      l_cargo.fecha_pago       ,
                      l_cargo.fecha_valor      ,
                      l_cargo.fecha_conversion ,
                      l_cargo.monto_en_pesos   ,
                      l_cargo.monto_en_acciones,
                      l_cargo.precio_accion    ,
                      l_cargo.dias_cotizados   ,
                      l_cargo.sucursal         ,
                      l_cargo.id_aportante     ,
                      l_cargo.estado           ,
                      g_today                  ,
                      g_usuario                ,
                      l_cargo.fecha_archivo    ,
                      l_cargo.etiqueta    
END IF

END FUNCTION

###################################################################################
# provisiona caso3 transferencia total de recursos 
###################################################################################
FUNCTION provisiona_caso3()

DEFINE l_dis_folio INTEGER

DEFINE l_consec_reg_lote INTEGER
DEFINE l_fecha_conver_210 DATE ,
       l_precio_accion_210 DEC(22,14) ,
       l_precio_accion_sb1 DEC(22,14) ,
       l_precio_accion DEC(22,14) ,
       l_scta   SMALLINT ,
       l_acciones_210 DEC(16,6), 
       l_pesos_210 DEC(16,6) 
DEFINE l_siefore_caso SMALLINT
DEFINE l_verifica SMALLINT
DEFINE l_folio_sua char(01)
DEFINE l_id_aportante CHAR(010)
DEFINE l_scta_siefore SMALLINT

LET l_folio_sua = " "
LET l_id_aportante = "CORTE"


      FOREACH cur_18_1 INTO  g_reg_mov_para_separar.*
        IF g_reg_mov_para_separar.tipo_movimiento <> 999 THEN
              CALL prov_det_abo_caso1(g_reg_mov_para_separar.*) 
        END IF
      END FOREACH
         FOREACH cur_13_x  USING g_folio        ,
                               g_nss_separador
                          INTO l_scta         ,
                               l_acciones_210

          EXECUTE sql_33 USING g_nss_separado,
                               l_scta
                         INTO  l_scta_siefore

         EXECUTE sql_15 USING g_today ,
                              l_scta_siefore
                         INTO l_precio_accion

             LET l_acciones_210 = l_acciones_210 * -1
             LET l_pesos_210 = l_acciones_210 * l_precio_accion

             IF l_acciones_210 = 0 THEN
                CONTINUE FOREACH
             END IF

             CALL prov_cargo( g_folio                        ,
                              " "                            ,
                              g_nss_separado                 ,
                              l_scta                         ,
                              g_cargo_mov_separacion         ,
                              0                              , 
                              l_scta_siefore                 ,#menor o mayor 56
                              l_acciones_210                 ,
                              l_pesos_210                    ,#pesos
                              "SEPARACION"                   ,
                              g_today         )

         END FOREACH
END FUNCTION

FUNCTION movimientos_a_separar_rcv()
#mps -------------------------------

DEFINE l_no_graba_consec        SMALLINT      ,
       l_dis_folio                            ,
       l_consec_reg_lote        INTEGER


FOREACH cur_11_1 USING g_folio                ,
                       g_nss_separado         ,
                       g_nss_separador
                  INTO g_reg_pat_a_separar

     FOREACH cur_11 USING g_nss_separado      ,
                          g_reg_pat_a_separar
                     INTO l_dis_folio         ,
                          l_consec_reg_lote

                      LET l_no_graba_consec = 0
          FOREACH cur_03 USING l_dis_folio       ,
                               g_nss_separado    ,
                               l_consec_reg_lote 
                         INTO  g_reg_mov_para_separar.*
                    
                      EXECUTE sql_26   USING g_reg_mov_para_separar.*
                EXECUTE sql_22 USING g_reg_mov_para_separar.tipo_movimiento    ,
                             g_reg_mov_para_separar.subcuenta          ,
                             g_reg_mov_para_separar.siefore            ,
                             g_reg_mov_para_separar.folio              ,
                             g_reg_mov_para_separar.consecutivo_lote   ,
                             g_reg_mov_para_separar.nss                ,
                --             g_reg_mov_para_separar.folio_sua          ,
                             g_reg_mov_para_separar.fecha_pago        ,
                             g_reg_mov_para_separar.fecha_valor       ,
                             g_reg_mov_para_separar.fecha_conversion  ,
                             g_reg_mov_para_separar.monto_en_pesos    ,
                             g_reg_mov_para_separar.monto_en_acciones ,
                             g_reg_mov_para_separar.precio_accion     ,
                             g_reg_mov_para_separar.dias_cotizados    ,
                             g_reg_mov_para_separar.id_aportante      ,
                             g_reg_mov_para_separar.estado            ,
                             g_reg_mov_para_separar.fecha_proceso     ,
                             g_reg_mov_para_separar.usuario           ,
                             g_reg_mov_para_separar.etiqueta

                      EXECUTE sql_22_1 USING 
                    g_reg_mov_para_separar.tipo_movimiento    ,
                    g_reg_mov_para_separar.subcuenta          ,
                    g_reg_mov_para_separar.siefore            ,
                    g_reg_mov_para_separar.folio              ,
                    g_reg_mov_para_separar.consecutivo_lote   ,
                    g_reg_mov_para_separar.nss                ,
                  --  g_reg_mov_para_separar.folio_sua          ,
                    g_reg_mov_para_separar.fecha_pago        ,
                    g_reg_mov_para_separar.fecha_valor       ,
                    g_reg_mov_para_separar.fecha_conversion  ,
                    g_reg_mov_para_separar.monto_en_pesos    ,
                    g_reg_mov_para_separar.monto_en_acciones ,
                    g_reg_mov_para_separar.precio_accion     ,
                    g_reg_mov_para_separar.dias_cotizados    ,
                    g_reg_mov_para_separar.id_aportante      ,
                    g_reg_mov_para_separar.estado            ,
                    g_reg_mov_para_separar.fecha_proceso     ,
                    g_reg_mov_para_separar.usuario           ,
                    g_reg_mov_para_separar.etiqueta       

                      LET l_no_graba_consec = 1
          END FOREACH
          IF l_no_graba_consec = 0 THEN 
             
             EXECUTE sql_31 USING l_dis_folio      ,
                                  g_nss_separado   ,
                                  l_consec_reg_lote             
          END IF


     END FOREACH

END FOREACH

      EXECUTE sql_22_3 

END FUNCTION


FUNCTION movimientos_a_separar_viv()
#masv-------------------------------

DEFINE l_dis_folio INTEGER

DEFINE l_consec_reg_lote INTEGER

FOREACH cur_11_1 USING g_folio           ,
                       g_nss_separado    ,
                       g_nss_separador 
                 INTO  g_reg_pat_a_separar

     FOREACH cur_11 USING g_nss_separado      ,
                          g_reg_pat_a_separar
                    INTO  l_dis_folio         ,
                          l_consec_reg_lote

          FOREACH cur_19 USING l_dis_folio       ,
                               g_nss_separado    ,
                               l_consec_reg_lote 
                         INTO  g_reg_mov_para_separar.*
           
                 EXECUTE sql_26   USING g_reg_mov_para_separar.*
                      EXECUTE sql_22_1 USING
                    g_reg_mov_para_separar.tipo_movimiento    ,
                    g_reg_mov_para_separar.subcuenta          ,
                    g_reg_mov_para_separar.siefore            ,
                    g_reg_mov_para_separar.folio              ,
                    g_reg_mov_para_separar.consecutivo_lote   ,
                    g_reg_mov_para_separar.nss                ,
             --       g_reg_mov_para_separar.folio_sua          ,
                    g_reg_mov_para_separar.fecha_pago        ,
                    g_reg_mov_para_separar.fecha_valor       ,
                    g_reg_mov_para_separar.fecha_conversion  ,
                    g_reg_mov_para_separar.monto_en_pesos    ,
                    g_reg_mov_para_separar.monto_en_acciones ,
                    g_reg_mov_para_separar.precio_accion     ,
                    g_reg_mov_para_separar.dias_cotizados    ,
                    g_reg_mov_para_separar.id_aportante      ,
                    g_reg_mov_para_separar.estado            ,
                    g_reg_mov_para_separar.fecha_proceso     ,
                    g_reg_mov_para_separar.usuario           ,
                    g_reg_mov_para_separar.etiqueta


          END FOREACH  

    END FOREACH

END FOREACH

      EXECUTE sql_22_2 

END FUNCTION

FUNCTION mov_ajuste()
#ma-----------------

DEFINE reg_ajuste RECORD LIKE dis_provision.*

DEFINE scta_ajuste SMALLINT
DEFINE nss_ajuste CHAR(011)
DEFINE acc_6dec DEC(16,6),
       acc_2dec DEC(16,2),
       pesos_2dec DEC(16,2)

DEFINE fecha_ajuste  DATE
DEFINE l_sie_viv     SMALLINT
DEFINE precio_ajuste LIKE glo_valor_accion.precio_del_dia

LET fecha_ajuste = "09/01/2005"
LET l_sie_viv    = 11

EXECUTE sql_15 USING fecha_ajuste,
                     l_sie_viv
               INTO  precio_ajuste 

FOREACH cur_28 USING g_folio        ,
                     g_nss_separado ,
                     fecha_ajuste    
               INTO  scta_ajuste    ,
                     acc_6dec       ,
                     acc_2dec

     LET reg_ajuste.tipo_movimiento  = 990
     LET reg_ajuste.subcuenta        = scta_ajuste
     LET reg_ajuste.siefore          = l_sie_viv
     LET reg_ajuste.folio            = g_folio
     LET reg_ajuste.consecutivo_lote = 0 
     LET reg_ajuste.nss              = g_nss_separado
     LET reg_ajuste.curp             = " "
     LET reg_ajuste.folio_sua        = " "
     LET reg_ajuste.fecha_pago       = fecha_ajuste
     LET reg_ajuste.fecha_valor      = fecha_ajuste
     LET reg_ajuste.fecha_conversion = fecha_ajuste
     LET reg_ajuste.monto_en_pesos   = (acc_6dec * precio_ajuste) * -1
     LET reg_ajuste.monto_en_acciones= (acc_6dec)* -1
     LET reg_ajuste.precio_accion    = precio_ajuste
     LET reg_ajuste.dias_cotizados   = 0
     LET reg_ajuste.sucursal         = " "
     LET reg_ajuste.id_aportante     = "AJUSTE"
     LET reg_ajuste.estado           = 5
     LET reg_ajuste.fecha_proceso    = fecha_ajuste
     LET reg_ajuste.usuario          = g_usuario
     LET reg_ajuste.fecha_archivo    = fecha_ajuste
     LET reg_ajuste.etiqueta         = 1

     EXECUTE sql_09 USING  reg_ajuste.*  #cargo a 6 decimales

     LET reg_ajuste.tipo_movimiento  = 991
     LET reg_ajuste.subcuenta        = scta_ajuste
     LET reg_ajuste.siefore          = l_sie_viv
     LET reg_ajuste.folio            = g_folio
     LET reg_ajuste.consecutivo_lote = 0 
     LET reg_ajuste.nss              = g_nss_separado
     LET reg_ajuste.curp             = " "
     LET reg_ajuste.folio_sua        = " "
     LET reg_ajuste.fecha_pago       = fecha_ajuste
     LET reg_ajuste.fecha_valor      = fecha_ajuste
     LET reg_ajuste.fecha_conversion = fecha_ajuste
     LET pesos_2dec                  = acc_2dec * precio_ajuste
     LET reg_ajuste.monto_en_pesos   = pesos_2dec
     LET reg_ajuste.monto_en_acciones= acc_2dec
     LET reg_ajuste.precio_accion    = precio_ajuste
     LET reg_ajuste.dias_cotizados   = 0
     LET reg_ajuste.sucursal         = " "
     LET reg_ajuste.id_aportante     = "SALDO"
     LET reg_ajuste.estado           = 5
     LET reg_ajuste.fecha_proceso    = fecha_ajuste
     LET reg_ajuste.usuario          = g_usuario
     LET reg_ajuste.fecha_archivo    = fecha_ajuste
     LET reg_ajuste.etiqueta         = 1
 
     EXECUTE sql_09 USING  reg_ajuste.*  #abono saldo a 2 decimales

    END FOREACH

FOREACH cur_29 USING g_folio,
                     g_nss_separado  ,
                     g_nss_separador ,
                     fecha_ajuste
               INTO  nss_ajuste      ,
                     scta_ajuste     ,
                     acc_6dec        ,
                     acc_2dec

     LET reg_ajuste.tipo_movimiento  = 990
     LET reg_ajuste.subcuenta        = scta_ajuste
     LET reg_ajuste.siefore          = l_sie_viv
     LET reg_ajuste.folio            = g_folio
     LET reg_ajuste.consecutivo_lote = 0 
     LET reg_ajuste.nss              = nss_ajuste
     LET reg_ajuste.curp             = " "
     LET reg_ajuste.folio_sua        = " "
     LET reg_ajuste.fecha_pago       = fecha_ajuste
     LET reg_ajuste.fecha_valor      = fecha_ajuste
     LET reg_ajuste.fecha_conversion = fecha_ajuste
     LET reg_ajuste.monto_en_pesos   = (acc_6dec * precio_ajuste) * -1
     LET reg_ajuste.monto_en_acciones= (acc_6dec)* -1
     LET reg_ajuste.precio_accion    = precio_ajuste
     LET reg_ajuste.dias_cotizados   = 0
     LET reg_ajuste.sucursal         = " "
     LET reg_ajuste.id_aportante     = "AJUSTE"
     LET reg_ajuste.estado           = 5
     LET reg_ajuste.fecha_proceso    = fecha_ajuste
     LET reg_ajuste.usuario          = g_usuario
     LET reg_ajuste.fecha_archivo    = fecha_ajuste
     LET reg_ajuste.etiqueta         = 1
 
     EXECUTE sql_09 USING  reg_ajuste.*  #cargo a 6 decimales

     LET reg_ajuste.tipo_movimiento  = 991
     LET reg_ajuste.subcuenta        = scta_ajuste
     LET reg_ajuste.siefore          = l_sie_viv
     LET reg_ajuste.folio            = g_folio
     LET reg_ajuste.consecutivo_lote = 0 
     LET reg_ajuste.nss              = nss_ajuste
     LET reg_ajuste.curp             = " "
     LET reg_ajuste.folio_sua        = " "
     LET reg_ajuste.fecha_pago       = fecha_ajuste
     LET reg_ajuste.fecha_valor      = fecha_ajuste
     LET reg_ajuste.fecha_conversion = fecha_ajuste
     LET pesos_2dec                  = acc_2dec * precio_ajuste
     LET reg_ajuste.monto_en_pesos   = pesos_2dec
     LET reg_ajuste.monto_en_acciones= acc_2dec
     LET reg_ajuste.precio_accion    = precio_ajuste
     LET reg_ajuste.dias_cotizados   = 0
     LET reg_ajuste.sucursal         = " "
     LET reg_ajuste.id_aportante     = "SALDO"
     LET reg_ajuste.estado           = 5
     LET reg_ajuste.fecha_proceso    = fecha_ajuste
     LET reg_ajuste.usuario          = g_usuario
     LET reg_ajuste.fecha_archivo    = fecha_ajuste
     LET reg_ajuste.etiqueta         = 1
 
     EXECUTE sql_09 USING  reg_ajuste.*  #abono saldo a 2 decimales

    END FOREACH

END FUNCTION

FUNCTION actualiza_sie_viv()
#asv-----------------------

   EXECUTE sql_30 USING g_folio

END FUNCTION

FUNCTION cal_fecha_avant(x_fecha,ciclo)
#cf-------------------------------

    DEFINE cc         SMALLINT
    DEFINE x_fecha    DATE
    DEFINE ciclo      SMALLINT

    DEFINE sig_fecha    DATE
    DEFINE dia_semana SMALLINT
    DEFINE ant_habil  SMALLINT

    LET sig_fecha  = x_fecha
    LET cc = 1

    WHILE cc <= ciclo
        LET dia_semana = WEEKDAY(x_fecha)
        IF dia_semana = 0 OR dia_semana = 6 THEN
        LET x_fecha  = x_fecha + 1
           CONTINUE WHILE
        ELSE
           SELECT "ok"
           FROM   tab_feriado
           WHERE  feria_fecha = x_fecha
           IF STATUS <> NOTFOUND THEN
              LET x_fecha  = x_fecha + 1
              CONTINUE WHILE
           ELSE
         LET cc = cc + 1
           END IF
        END IF
    END WHILE
    RETURN x_fecha
END FUNCTION
