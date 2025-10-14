##########################################################
#Programa:             => TAAB002C                       #
#Autor:                => JCPV  01/03/2012               #
#Autor                 => MAURO MUÑIZ CABALLERO          #
#Fecha                 => 13 DE SEPTIEMBRE DE 2019       #
#          Adecuación para fondos gneracionales          #
##########################################################

DATABASE safre_af

GLOBALS

   DEFINE vfolio                    INTEGER
   DEFINE gregs                     INTEGER
   DEFINE reg_graba2                INTEGER

   DEFINE i                         SMALLINT
   DEFINE j                         SMALLINT
   DEFINE k                         SMALLINT
   DEFINE l                         SMALLINT
   DEFINE g_opcion                  SMALLINT
   DEFINE l_tot_sief_loc            SMALLINT

   DEFINE vsaldo_subc               DECIMAL(18,6)
   DEFINE vno_tot_acc               DECIMAL(15,2)

   DEFINE dia_liq                   DATE
   DEFINE dia_liq_viv               DATE

   DEFINE g_usuario                 CHAR(8)
   DEFINE ghora                     CHAR(10)
   DEFINE G_IMPRE                   CHAR(150)
   DEFINE plano_prov                CHAR(250)
   DEFINE cifras_prov               CHAR(250)
   DEFINE v_rep                     CHAR(250)
   DEFINE sel_txt                   CHAR(1000)

   DEFINE reg_recep_rcv RECORD
       folio                        INTEGER,
       cont_servicio                DECIMAL(10,0),
       ident_operacion              CHAR(2),
       cve_ced_cuenta               CHAR(3),
       tipo_traspaso                SMALLINT,
       fecha_mov_banxico            DATE,
       fecha_presentacion           DATE,
       nss                          CHAR(11),
       curp                         CHAR(18),
       cve_subcta                   CHAR(2),
       prctj_subc                   DECIMAL(16,6),
       saldo_subc                   DECIMAL(15,2),
       no_tot_acc                   DECIMAL(22,6),
       siefore                      CHAR(8),
       precio_acc                   DECIMAL(15,6),
       subcuenta                    SMALLINT,
       codigo_siefore               SMALLINT,
       porcentaje                   DECIMAL (5,2),
       precio_ced                   DECIMAL(11,6),
       precio_loc                   DECIMAL(11,6),
       monto_pesos                  DECIMAL(16,6),
       monto_accs                   DECIMAL(16,6)
    END RECORD
    
    DEFINE reg_recep_viv RECORD
       folio                        INTEGER,
       cont_servicio                DECIMAL(10,0),
       ident_operacion              CHAR(02),
       cve_ced_cuenta               CHAR(03),
       tipo_traspaso                SMALLINT,
       fecha_mov_banxico            DATE,
       fecha_presentacion           DATE,
       nss                          CHAR(11),
       curp                         CHAR(18),
       subcuenta                    SMALLINT,
       codigo_siefore               SMALLINT,
       precio_loc                   DECIMAL(11,6),
       monto_pesos                  DECIMAL(16,6),
       monto_accs                   DECIMAL(16,6),
       aivs_issste_08               DECIMAL(18,6),
       saldo_issste_08              DECIMAL(15,2),
       partic_viv97                 DECIMAL(18,6),
       saldo_viv97                  DECIMAL(15,2),
       aivs_issste                  DECIMAL(18,6),
       saldo_issste                 DECIMAL(15,2),
       partic_viv92                 DECIMAL(18,6),
       saldo_viv92                  DECIMAL(15,2),
       importe_bono                 DECIMAL(18,4)
    END RECORD

    DEFINE arr_precios_loc ARRAY[100] OF RECORD
       precio                       DECIMAL(19,14),
       razon_social                 CHAR(20)
    END RECORD

    DEFINE cif_ctl ARRAY[100,50] OF RECORD
       siefore                      SMALLINT,
       subcuenta                    SMALLINT,
       pesos                        DECIMAL(18,6),
       acciones                     DECIMAL(18,6) 
   END RECORD

   DEFINE cif_ctl_tot_sief ARRAY[100] OF RECORD
      pesos                         DECIMAL(18,6),
      acciones                      DECIMAL(18,6)
   END RECORD

   DEFINE reg_prov RECORD LIKE dis_provision.*

   DEFINE reg_tas RECORD
      codigo_afore                  SMALLINT,
      desc_siefore                  CHAR(08),
      codigo_siefore                SMALLINT,
      precio_del_dia                DECIMAL(11,6)
   END RECORD

END GLOBALS

MAIN

   LET vfolio = ARG_VAL(1)

   CALL inicio()
   CALL fn_cambia_regimen()   #CPL-6043
   CALL det_rcv()
   CALL det_viv()
   CALL cifras_control()

   DISPLAY "Cifras control generadas"

   SELECT count(*)
     INTO reg_graba2
     FROM safre_tmp:recep

   DISPLAY "Total de registros procesados  ", reg_graba2  USING "########&&&&"
   DISPLAY ""
   DISPLAY ""
   DISPLAY "Proceso finalizado"

END MAIN

FUNCTION det_rcv()

   FOR i = 1 TO 8
      LET gregs = 0
      LET ghora = TIME

      DISPLAY "En proceso RCV subcuenta ", i USING '#', ' ', ghora

      LET sel_txt = " SELECT ",
                    " folio, ",
                    " cont_servicio, ",
                    " ident_operacion, ",
                    " cve_ced_cuenta, ",
                    " tipo_traspaso,  ",
                    " fecha_mov_banxico, ",
                    " ' ',            ",
                    " trr.nss,        ",
                    " ' ',            ",
                    " cve_subcta_",i USING '#',',', 
                    " prctj_subc_",i USING '#',',',
                    " saldo_subc_",i USING '#',',',
                    " no_tot_acc_",i USING '#',',',
                    " siefore_",i    USING '#',',',
                    " precio_acc_",i USING '#',',',
                    " tst.subcuenta, ",
                    " cr.codigo_siefore, ",
                    " cr.porcentaje,    ",
                    " tas.precio_del_dia, ",
                    " '0', ",
                    " '0', ",
                    " '0' ",
                    " FROM taa_rcv_recepcion trr, ",
                          " cta_regimen cr, ",
                          " tab_subcta_taa tst, ",
                          " taa_accion_siefore tas ",
                    " WHERE trr.folio        = ",vfolio,
                    "  AND trr.cve_subcta_",i USING '#', " IS NOT NULL ",
                    "  AND trr.cve_subcta_",i USING '#', " = tst.sub_taa ",
                    "  AND trr.nss          = cr.nss ",
                    "  AND cr.codigo_siefore > 0 ",
                    "  AND cr.subcuenta     =  tst.subcuenta ",
                    "  AND tas.fecha_valuacion = ","'",dia_liq,"'", 
                    "  AND tas.desc_siefore    = trr.siefore_",i USING '#',
                    "  AND trr.no_tot_acc_",i using '#',' > 0 ',
                    "  AND trr.no_tot_acc_",i using '#', " IS NOT NULL"  

      PREPARE qry_txt1 FROM sel_txt

      DECLARE csr1 CURSOR FOR qry_txt1
      FOREACH csr1 INTO reg_recep_rcv.*
         LET k = reg_recep_rcv.codigo_siefore
         LET l = reg_recep_rcv.subcuenta

         IF k = 90 THEN
            LET k = 23
         END IF

         LET reg_recep_rcv.precio_loc = arr_precios_loc[k].precio

         IF reg_recep_rcv.cve_ced_cuenta <> 531 THEN
            LET  reg_recep_rcv.monto_pesos = reg_recep_rcv.no_tot_acc 
                                           * reg_recep_rcv.precio_ced
                                           * reg_recep_rcv.porcentaje
                                           / 100
         ELSE
            LET  reg_recep_rcv.monto_pesos = reg_recep_rcv.saldo_subc
                                           * reg_recep_rcv.precio_ced
                                           * reg_recep_rcv.porcentaje
                                           / 100
         END IF

         LET reg_recep_rcv.monto_accs   = reg_recep_rcv.monto_pesos
                                        / reg_recep_rcv.precio_loc

         LET cif_ctl[k,l].siefore            =  reg_recep_rcv.codigo_siefore
         LET cif_ctl[k,l].subcuenta          =  reg_recep_rcv.subcuenta
         LET cif_ctl[k,l].pesos              =  cif_ctl[k,l].pesos
                                             +  reg_recep_rcv.monto_pesos
         LET cif_ctl[k,l].acciones           =  cif_ctl[k,l].acciones
                                             +  reg_recep_rcv.monto_accs

         INSERT INTO safre_tmp:recep VALUES(reg_recep_rcv.*)

         LET gregs = gregs + 1

         IF gregs  MOD 10000 = 0 THEN
            LET ghora = TIME

            DISPLAY "Procesados: ", gregs,  ' ', ghora
         END IF
      END FOREACH
   END FOR

   SELECT count(*)
     INTO gregs
     FROM safre_tmp:recep

   DISPLAY "Total de registros de RCV      ", gregs

END FUNCTION       #det_rcv()

FUNCTION det_viv()

   LET gregs = 0
   LET ghora = TIME

   DISPLAY "En proceso VIV ", ' ', ghora

   LET sel_txt =  " SELECT ",
                  " folio, ",
                  " cont_servicio, ",
                  " ident_operacion, ",
                  " cve_ced_cuenta, ",
                  " tipo_traspaso, ",
                  " fecha_mov_banxico, ",
                  " fecha_presentacion, ",
                  " tvr.nss,  ",
                  " curp, ",
                  " 0, ",
                  " 0, ",
                  " 0, ",
                  " 0, ",
                  " 0, ",
                  " NVL(aivs_issste_08,0), ",
                  " NVL(saldo_issste_08,0), ",
                  " NVL(partic_viv97,0), ",
                  " NVL(saldo_viv97,0), ",
                  " NVL(aivs_issste,0), ",
                  " NVL(saldo_issste,0), ",
                  " NVL(partic_viv92,0), ",
                  " NVL(saldo_viv92,0), ",
                  " NVL(importe_bono,0) ",
                  " FROM taa_viv_recepcion tvr ",
                  " WHERE tvr.folio = ",vfolio

   PREPARE qry_txt2 FROM sel_txt

   DECLARE csr2 CURSOR FOR qry_txt2

   FOREACH csr2 INTO reg_recep_viv.*
      LET gregs = gregs + 1

      IF gregs  MOD 10000 = 0 THEN
         LET ghora = TIME

         DISPLAY "Procesados: ", gregs,  ' ', ghora
      END IF

      --DISPLAY "reg_recep_viv.partic_viv97: ", reg_recep_viv.partic_viv97
      IF reg_recep_viv.partic_viv97 > 0 THEN
         SELECT codigo_siefore
           INTO reg_recep_viv.codigo_siefore
           FROM cta_regimen
          WHERE nss = reg_recep_viv.nss
            AND subcuenta = 4

         LET k = reg_recep_viv.codigo_siefore
         LET l = 4
         LET reg_recep_viv.subcuenta = l
         --DISPLAY "l: ", l
         --DISPLAY "k: ", k
         LET reg_recep_viv.precio_loc  = arr_precios_loc[k].precio
         LET reg_recep_viv.monto_pesos = reg_recep_viv.partic_viv97
                                         * reg_recep_viv.precio_loc
         LET reg_recep_viv.monto_accs = reg_recep_viv.partic_viv97

         LET cif_ctl[k,l].siefore   = reg_recep_viv.codigo_siefore
         LET cif_ctl[k,l].subcuenta = reg_recep_viv.subcuenta
         LET cif_ctl[k,l].pesos     = cif_ctl[k,l].pesos
                                    + reg_recep_viv.monto_pesos
         LET cif_ctl[k,l].acciones  = cif_ctl[k,l].acciones
                                    + reg_recep_viv.monto_accs

         LET  vsaldo_subc = reg_recep_viv.partic_viv97
         LET  vno_tot_acc = reg_recep_viv.saldo_viv97

         CALL arma_rcv()

         INSERT INTO safre_tmp:recep VALUES(reg_recep_rcv.*)
      END IF

      --DISPLAY "reg_recep_viv.partic_viv92: ", reg_recep_viv.partic_viv92
      IF reg_recep_viv.partic_viv92 > 0 THEN
         SELECT codigo_siefore
           INTO reg_recep_viv.codigo_siefore
           FROM cta_regimen
          WHERE nss = reg_recep_viv.nss
            AND subcuenta = 8

         LET k = reg_recep_viv.codigo_siefore
         LET l = 8
         --DISPLAY "l: ", l
         --DISPLAY "k: ", k
         LET reg_recep_viv.subcuenta = l
         LET reg_recep_viv.precio_loc = arr_precios_loc[k].precio
         LET reg_recep_viv.monto_pesos = reg_recep_viv.partic_viv92
                                       * reg_recep_viv.precio_loc
         LET reg_recep_viv.monto_accs = reg_recep_viv.partic_viv92 

         LET cif_ctl[k,l].siefore   = reg_recep_viv.codigo_siefore
         LET cif_ctl[k,l].subcuenta = reg_recep_viv.subcuenta
         LET cif_ctl[k,l].pesos     = cif_ctl[k,l].pesos
                                    + reg_recep_viv.monto_pesos
         LET cif_ctl[k,l].acciones  = cif_ctl[k,l].acciones
                                    + reg_recep_viv.monto_accs

         LET  vsaldo_subc = reg_recep_viv.partic_viv92
         LET  vno_tot_acc = reg_recep_viv.saldo_viv92

         CALL arma_rcv()

         INSERT INTO safre_tmp:recep VALUES(reg_recep_rcv.*)
      END IF 

      --DISPLAY "reg_recep_viv.aivs_issste: ", reg_recep_viv.aivs_issste
      IF reg_recep_viv.aivs_issste > 0 THEN
         SELECT codigo_siefore
           INTO reg_recep_viv.codigo_siefore
           FROM cta_regimen
          WHERE nss = reg_recep_viv.nss
            AND subcuenta = 14

         LET k = reg_recep_viv.codigo_siefore
         LET l = 14
         --DISPLAY "l: ", l
         --DISPLAY "k: ", k
         LET reg_recep_viv.subcuenta = l
         LET reg_recep_viv.precio_loc = arr_precios_loc[k].precio
         LET reg_recep_viv.monto_pesos = reg_recep_viv.aivs_issste
                                       * reg_recep_viv.precio_loc
         LET reg_recep_viv.monto_accs = reg_recep_viv.aivs_issste

         LET cif_ctl[k,l].siefore   = reg_recep_viv.codigo_siefore
         LET cif_ctl[k,l].subcuenta = reg_recep_viv.subcuenta
         LET cif_ctl[k,l].pesos     = cif_ctl[k,l].pesos
                                    + reg_recep_viv.monto_pesos
         LET cif_ctl[k,l].acciones  = cif_ctl[k,l].acciones
                                    + reg_recep_viv.monto_accs

         LET  vsaldo_subc = reg_recep_viv.aivs_issste
         LET  vno_tot_acc = reg_recep_viv.saldo_issste

         CALL arma_rcv()

         INSERT INTO safre_tmp:recep VALUES(reg_recep_rcv.*)
      END IF

      --DISPLAY "reg_recep_viv.aivs_issste_08: ", reg_recep_viv.aivs_issste_08
      IF reg_recep_viv.aivs_issste_08 > 0 THEN
         SELECT codigo_siefore
           INTO reg_recep_viv.codigo_siefore
           FROM cta_regimen
          WHERE nss = reg_recep_viv.nss
            AND subcuenta = 35

         LET k = reg_recep_viv.codigo_siefore
         LET l = 35
        -- DISPLAY "l: ", l
         --DISPLAY "k: ", k
         LET reg_recep_viv.subcuenta = l
         LET reg_recep_viv.precio_loc = arr_precios_loc[k].precio
         LET reg_recep_viv.monto_pesos = reg_recep_viv.aivs_issste_08
                                      * reg_recep_viv.precio_loc
         LET reg_recep_viv.monto_accs = reg_recep_viv.aivs_issste_08

         LET cif_ctl[k,l].siefore   = reg_recep_viv.codigo_siefore
         LET cif_ctl[k,l].subcuenta = reg_recep_viv.subcuenta
         LET cif_ctl[k,l].pesos     = cif_ctl[k,l].pesos         
                                    + reg_recep_viv.monto_pesos
         LET cif_ctl[k,l].acciones  = cif_ctl[k,l].acciones     
                                    + reg_recep_viv.monto_accs

         LET  vsaldo_subc = reg_recep_viv.aivs_issste_08
         LET  vno_tot_acc = reg_recep_viv.saldo_issste_08

         CALL arma_rcv()

         INSERT INTO safre_tmp:recep VALUES(reg_recep_rcv.*)
      END IF

      --DISPLAY "reg_recep_viv.importe_bono: ", reg_recep_viv.importe_bono
      IF reg_recep_viv.importe_bono  >  0  THEN 
         LET reg_recep_viv.codigo_siefore = 13
         LET k = reg_recep_viv.codigo_siefore
         LET l = 36
         LET reg_recep_viv.subcuenta = l
         LET reg_recep_viv.precio_loc = arr_precios_loc[k].precio
         LET reg_recep_viv.monto_pesos = reg_recep_viv.importe_bono
                                         * reg_recep_viv.precio_loc
         LET reg_recep_viv.monto_accs = reg_recep_viv.importe_bono

         LET cif_ctl[k,l].siefore   = reg_recep_viv.codigo_siefore
         LET cif_ctl[k,l].subcuenta = reg_recep_viv.subcuenta
         LET cif_ctl[k,l].pesos     = cif_ctl[k,l].pesos         
                                    + reg_recep_viv.monto_pesos
         LET cif_ctl[k,l].acciones  = cif_ctl[k,l].acciones     
                                    + reg_recep_viv.monto_accs

         LET  vsaldo_subc = reg_recep_viv.importe_bono
         LET  vno_tot_acc = 0

         CALL arma_rcv()

         INSERT INTO safre_tmp:recep VALUES(reg_recep_rcv.*)
      END IF

   END FOREACH

   SELECT count(*)
     INTO gregs
     FROM safre_tmp:recep

   DISPLAY "Total de registros de vivienda ", gregs

END FUNCTION       #det_viv()

FUNCTION inicio()
 DEFINE v_90            SMALLINT

 
   SELECT UNIQUE(fecha_mov_banxico)
     INTO dia_liq
     FROM taa_viv_recepcion
    WHERE folio = vfolio

   LET dia_liq_viv = MDY(MONTH(dia_liq),1,YEAR(dia_liq))
   LET ghora = TIME
   LET v_90  = 90

   DISPLAY "Inicia proceso cifras control", ghora
   DISPLAY "Folio ", vfolio
   DISPLAY "Fecha Mov Banxico ", dia_liq USING "DD/MM/YYYY"

   SELECT COUNT(*)
     INTO l_tot_sief_loc
     FROM tab_siefore_local
    WHERE codigo_siefore > 10

   LET l_tot_sief_loc = l_tot_sief_loc + 10

   LET gregs = 0

   FOR i = 11 TO l_tot_sief_loc
      LET arr_precios_loc[i].precio = 0
      LET arr_precios_loc[i].razon_social = ""
   END FOR

   FOR i = 11 TO l_tot_sief_loc
      FOR j = 1 TO 50
         LET cif_ctl[i,j].siefore   = 0
         LET cif_ctl[i,j].subcuenta = 0
         LET cif_ctl[i,j].pesos     = 0
         LET cif_ctl[i,j].acciones  = 0
      END FOR
   END FOR

   FOR i = 11 TO l_tot_sief_loc
      LET cif_ctl_tot_sief[i].pesos    = 0
      LET cif_ctl_tot_sief[i].acciones = 0
   END FOR

   FOR i = 11 TO l_tot_sief_loc
      IF i = 11 OR i = 12 THEN
         SELECT g.precio_del_dia, c.razon_social
           INTO arr_precios_loc[i].precio, arr_precios_loc[i].razon_social
           FROM glo_valor_accion g, tab_siefore_local c
          WHERE g.fecha_valuacion = dia_liq_viv
            AND g.codigo_siefore  = i
            AND g.codigo_siefore  = c.codigo_siefore

         DISPLAY "Precio siefore ", i, " ",arr_precios_loc[i].razon_social, " ", arr_precios_loc[i].precio
      ELSE
         IF i = 23 THEN
            SELECT g.precio_del_dia, c.razon_social
              INTO arr_precios_loc[i].precio, arr_precios_loc[i].razon_social
              FROM glo_valor_accion g, tab_siefore_local c
             WHERE g.fecha_valuacion = dia_liq
               AND g.codigo_siefore  = v_90
               AND g.codigo_siefore  = c.codigo_siefore

            DISPLAY "Precio siefore ", v_90, " ",arr_precios_loc[i].razon_social, " ", arr_precios_loc[i].precio
         ELSE
            SELECT g.precio_del_dia, c.razon_social
              INTO arr_precios_loc[i].precio, arr_precios_loc[i].razon_social
              FROM glo_valor_accion g, tab_siefore_local c
             WHERE g.fecha_valuacion = dia_liq
               AND g.codigo_siefore  = i
               AND g.codigo_siefore  = c.codigo_siefore

            DISPLAY "Precio siefore ", i, " ",arr_precios_loc[i].razon_social, " ", arr_precios_loc[i].precio
         END IF
      END IF
   END FOR

   DISPLAY ""
   DISPLAY "Precios Afores Cedentes "

   DECLARE csr_tas1 CURSOR FOR 
      SELECT codigo_afore,desc_siefore,codigo_siefore,precio_del_dia
      FROM   taa_accion_siefore
      WHERE  fecha_valuacion = dia_liq
      ORDER BY 1,3,2

   FOREACH csr_tas1 INTO reg_tas.*
      DISPLAY  "Precio cedente ", ' ',reg_tas.codigo_afore, ' ',
                                      reg_tas.desc_siefore, ' ',
                                      reg_tas.codigo_siefore,' ',
                                      reg_tas.precio_del_dia
   END FOREACH

   DATABASE safre_tmp

   WHENEVER  ERROR CONTINUE
      DROP TABLE recep

      CREATE TABLE recep
         (folio                     INTEGER,
          cont_servicio             DECIMAL(10,0),
          ident_operacion           CHAR(2),
          cve_ced_cuenta            CHAR(3),
          tipo_traspaso             SMALLINT,
          fecha_mov_banxico         DATE,
          fecha_presentacion        DATE,
          nss                       CHAR(11),
          curp                      CHAR(18),
          cve_subcta                CHAR(2),
          prctj_subc                DECIMAL(16,6),
          saldo_subc                DECIMAL(16,6),
          no_tot_acc                DECIMAL(16,6),
          siefore                   CHAR(8),
          precio_acc                DECIMAL(16,6),
          subcuenta                 SMALLINT,
          codigo_siefore            SMALLINT,
          porcentaje                DECIMAL (16,6),
          precio_ced                DECIMAL(16,6),
          precio_loc                DECIMAL(16,6),
          monto_pesos               DECIMAL(18,6),
          monto_accs                DECIMAL(18,6))

      DATABASE safre_af
   WHENEVER ERROR STOP

    SELECT USER, ruta_rescate, ruta_listados
      INTO g_usuario, plano_prov, cifras_prov
      FROM seg_modulo 
     WHERE modulo_cod = 'taa'

   LET G_IMPRE = cifras_prov CLIPPED,"/cifras_control_recep_aa-",vfolio USING "&&&&&&",".txt"
   LET G_IMPRE = G_IMPRE CLIPPED

   DISPLAY ""
   DISPLAY ""

   DISPLAY G_IMPRE

   START REPORT cifras TO G_IMPRE

   LET reg_graba2 = 0

END FUNCTION       #inicio()

FUNCTION arma_rcv()

   LET reg_recep_rcv.folio              = reg_recep_viv.folio
   LET reg_recep_rcv.ident_operacion    = reg_recep_viv.ident_operacion
   LET reg_recep_rcv.cont_servicio      = reg_recep_viv.cont_servicio
   LET reg_recep_rcv.cve_ced_cuenta     = reg_recep_viv.cve_ced_cuenta
   LET reg_recep_rcv.tipo_traspaso      = reg_recep_viv.tipo_traspaso
   LET reg_recep_rcv.fecha_mov_banxico  = reg_recep_viv.fecha_mov_banxico
   LET reg_recep_rcv.fecha_presentacion = reg_recep_viv.fecha_presentacion
   LET reg_recep_rcv.nss                = reg_recep_viv.nss
   LET reg_recep_rcv.curp               = reg_recep_viv.curp
   LET reg_recep_rcv.prctj_subc         = 0
   LET reg_recep_rcv.saldo_subc         = 0
   LET reg_recep_rcv.saldo_subc         = vsaldo_subc
   LET reg_recep_rcv.no_tot_acc         = vno_tot_acc
   LET reg_recep_rcv.siefore            = 0
   LET reg_recep_rcv.precio_acc         = 0
   LET reg_recep_rcv.subcuenta          = reg_recep_viv.subcuenta
   LET reg_recep_rcv.codigo_siefore     = reg_recep_viv.codigo_siefore
   LET reg_recep_rcv.porcentaje         = 0
   LET reg_recep_rcv.precio_ced         = 0
   LET reg_recep_rcv.precio_loc         = reg_recep_viv.precio_loc
   LET reg_recep_rcv.monto_pesos        = reg_recep_viv.monto_pesos
   LET reg_recep_rcv.monto_accs         = reg_recep_viv.monto_accs

END FUNCTION     #arma_rcv

FUNCTION cifras_control()

   FOR i = 1 TO 20
     FOR j = 1 TO 50
       IF cif_ctl[i,j].siefore > 0  THEN
         OUTPUT TO REPORT cifras()
       END IF
     END FOR
   END FOR

   FINISH REPORT cifras

END FUNCTION       #cifras_control()

REPORT cifras()

   DEFINE m                         SMALLINT

   OUTPUT
       PAGE LENGTH 66
       LEFT MARGIN 0
       RIGHT MARGIN 0
       TOP MARGIN 0
       BOTTOM MARGIN 0

   FORMAT
   PAGE HEADER

   PRINT COLUMN 5,"CIFRAS DE CONTROL PREVIO A LA PROVISION TAA"
   PRINT
   PRINT COLUMN 5,"FOLIO         SIEF   SUBCTA      PESOS           ACCIONES"
   PRINT                          

   ON EVERY ROW

   LET m = i 

   IF m = 23 THEN
      LET m = 90
   END IF

   PRINT COLUMN  5, vfolio USING "&&&&&&&",
         COLUMN 20, m USING "&&",
         COLUMN 30, j USING "&&",
         COLUMN 35, cif_ctl[i,j].pesos USING "#########&.&&&&&&",
         COLUMN 55, cif_ctl[i,j].acciones USING "#########&.&&&&&&"

         LET cif_ctl_tot_sief[i].pesos = cif_ctl_tot_sief[i].pesos + cif_ctl[i,j].pesos

         LET cif_ctl_tot_sief[i].acciones = cif_ctl_tot_sief[i].acciones + cif_ctl[i,j].acciones
    ON LAST ROW

    PRINT
    PRINT COLUMN 05, "TOTALES POR SIEFORE "
    PRINT

    FOR i = 1 TO l_tot_sief_loc
       IF cif_ctl_tot_sief[i].pesos <> 0 THEN
          LET m = i

          IF m = 23 THEN 
            LET m = 90
          END IF

          PRINT COLUMN 20, m USING "&&", 
                COLUMN 35, cif_ctl_tot_sief[i].pesos USING "#########&.&&&&&&",
                COLUMN 55, cif_ctl_tot_sief[i].acciones USING "#########&.&&&&&&"
       END IF
    END FOR

END REPORT     #cifras()

REPORT graba_prov()

    OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT

    ON EVERY ROW
       PRINT COLUMN 001, v_rep

END REPORT       #graba_prov()


#CPL-6043
FUNCTION fn_cambia_regimen()
 DEFINE v_query                CHAR(1000)

 DEFINE v_crea_fecha           DATE
 DEFINE v_nss                  CHAR(11)
 DEFINE v_curp                 CHAR(18) 
 DEFINE v_rfc                  CHAR(13) 
 DEFINE v_fnacimiento          DATE 
 DEFINE v_tipo_traspaso        CHAR(2)
 DEFINE v_siefore_actual       INTEGER
 DEFINE v_siefore_ced          VARCHAR(20)
 DEFINE v_siefore_oss          INTEGER
 DEFINE v_f_oss                DATE

 DEFINE v_tipo_proc            SMALLINT
 DEFINE v_medio                SMALLINT

#Salida del cambio de regimen
 DEFINE v_existe              SMALLINT
 DEFINE v_edad                SMALLINT
 DEFINE v_criterio            SMALLINT
 DEFINE v_ind_edad            SMALLINT
 DEFINE v_rechazo             SMALLINT
 DEFINE v_folioaten           INTEGER
 
 DEFINE v_ls_query            CHAR(100) #CPL-6043 

   LET v_tipo_proc = 2
   LET v_medio     = 10   

   DISPLAY "Se esta ejecutando el cambio de regimen..."

   #CPL-4063
   INITIALIZE v_ls_query TO NULL
   LET v_ls_query   = " EXECUTE FUNCTION fn_valida_edad_sol (?,?) "
   PREPARE prp_fecha_sol FROM v_ls_query

   INITIALIZE v_ls_query TO NULL
   LET v_ls_query = "EXECUTE PROCEDURE fn_fnacimiento ( ?, ?)"
   PREPARE prp_f_nacimiento FROM v_ls_query

   INITIALIZE v_ls_query TO NULL
   LET v_ls_query   = " EXECUTE PROCEDURE fn_regimen_inv (?,?,?,?,?,?)"
   PREPARE prp_act_regimen FROM v_ls_query
   #CPL-4063

   {
   INITIALIZE v_query TO NULL
   LET v_query =  "\n SELECT cli.nss,                                                  ",
                  "\n        viv.tpo_traspaso,                                         ",
                  "\n        op.perfil_actual,                                         ",
                  "\n        op.cve_siefore_desc,                                      ",
                  "\n        op.cve_siefore_oss,                                       ",
                  "\n        op.fecha_oss                                              ",
                  "\n   FROM trc_ctr_traspaso                  trc                     ",
                  "\n        INNER JOIN trc_detalle_viv        viv                     ",
                  "\n           ON viv.id_traspaso = trc.id_traspaso                   ",
                  "\n        INNER JOIN taa_indicadores_op09   op                      ",
                  "\n           ON (op.nss = viv.nss AND op.nom_archivo = trc.archivo) ",
                  "\n        INNER JOIN cli_afiliado           cli                     ", 
                  "\n           ON cli.nss = viv.nss                                   ",
                  "\n  WHERE trc.folio          = ?                                    ",
                  "\n    AND op.cve_siefore_oss > 0                                    "
   }

   INITIALIZE v_query TO NULL
   LET v_query =  "\n SELECT afi.n_seguro,                                 ",
                  "\n        viv.tipo_traspaso,                            ",
                  "\n        op.perfil_actual,                             ",
                  "\n        op.cve_siefore_desc,                          ",
                  "\n        op.cve_siefore_oss,                           ",
                  "\n        op.fecha_oss                                  ",
                  "\n   FROM taa_ctr_traspaso                  taa         ",
                  "\n        INNER JOIN taa_viv_recepcion      viv         ",
                  "\n           ON viv.folio       = taa.folio             ",
                  "\n        INNER JOIN taa_indicadores_op09   op          ",
                  "\n           ON (op.nss         = viv.nss AND           ",
                  "\n               op.nom_archivo = taa.nombre_archivo)   ",
                  "\n        INNER JOIN afi_mae_afiliado       afi         ", 
                  "\n           ON afi.n_seguro    = viv.nss               ",
                  "\n  WHERE taa.folio             = ?                     ",
                  "\n    AND op.cve_siefore_oss    > 0                     "
                  
   PREPARE exe_cunsulta_oss FROM v_query
   DECLARE cur_consulta_oss CURSOR FOR exe_cunsulta_oss

   FOREACH cur_consulta_oss USING vfolio
                             INTO v_nss,
                                  v_tipo_traspaso,
                                  v_siefore_actual,
                                  v_siefore_ced,
                                  v_siefore_oss,
                                  v_f_oss
                                    
      IF v_nss IS NOT NULL AND v_nss[1] <> ' ' THEN
         DISPLAY " "
         DISPLAY "Solicitando cambio de siefore por OSS..."
         DISPLAY "nss: ", v_nss
         DISPLAY "tipo de traspaso: ", v_tipo_traspaso
         DISPLAY "Siefore por OSS: ", v_siefore_ced
         DISPLAY "Siefore local OSS: ", v_siefore_oss

         EXECUTE prp_fecha_sol USING   v_nss, 
                                       v_tipo_traspaso
                              INTO     v_crea_fecha

         EXECUTE prp_f_nacimiento USING v_nss,
                                        v_crea_fecha
                                   INTO v_existe, 
                                        v_edad, 
                                        v_criterio, 
                                        v_ind_edad,
                                        v_curp, 
                                        v_rfc, 
                                        v_fnacimiento

         EXECUTE prp_act_regimen USING v_nss,
                                       v_ind_edad,
                                       v_siefore_oss,
                                       v_tipo_proc,
                                       v_tipo_traspaso,
                                       v_medio
                                  INTO v_existe, 
                                       v_edad, 
                                       v_rechazo, 
                                       v_folioaten
      END IF
   END FOREACH
   
END FUNCTION
#CPL-6043
