################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#                  => E.F.P.                                                   #
#Programa DISB071  => RESPUESTA A PROCESAR INTERESES EN TRANSITO ERROR PROCESAR#
#Fecha             => 14 Mayo 2004.                                            #
#By                => JOSE ALEJANDRO RAMIREZ.                                  #
#Sistema           => DIS.  			                               #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE
      vident_operacion CHAR(2),
      g_param_dis     RECORD LIKE seg_modulo.* ,
      vcodigo_afore   CHAR(03)

   DEFINE reg_cza RECORD
      tipo_registro    CHAR(02),
      ident_servicio   CHAR(02),
      ident_operacion  CHAR(02),
      tipo_ent_origen  CHAR(02),
      cve_ent_origen   CHAR(03),
      tipo_ent_destino CHAR(02),
      cve_ent_destino  CHAR(03),
      fecha_creac_lote CHAR(08),
      result_operacion CHAR(02),
      diag1            CHAR(03),
      diag2            CHAR(03),
      diag3            CHAR(03)
   END RECORD


   DEFINE reg_det RECORD
      tipo_registro    CHAR(02),
      ident_servicio   CHAR(02),
      nss              CHAR(11),
      rfc              CHAR(13),
      nombre           CHAR(50),
      periodo_pago     CHAR(06),
      fecha_pago       CHAR(08),
      fecha_valor_rcv  CHAR(08),
      fecha_valor_viv  CHAR(08),
      folio_sua        CHAR(06),
      nrp_patron       CHAR(11),
      impt_total_rcv   CHAR(07),    
      impt_total_vol   CHAR(07), 
      impt_total_viv   CHAR(07), 
      impt_total_gub   CHAR(07), 
      impt_comision    CHAR(07), 
-------      impt_com_saldo   CHAR(07),
      fecha_pago_gub   CHAR(08),
      ident_viv_gara   CHAR(01),
      mov_afect_cta    CHAR(02),
      fecha_mov        CHAR(08),
      afore_disper     CHAR(03),
      afore_admin      CHAR(03),
      siefore1         CHAR(08),
      num_accion1      CHAR(16),
      valor_accion1    CHAR(11),
      fecha_accion1    CHAR(08),
      siefore2         CHAR(08),
      num_accion2      CHAR(16),
      valor_accion2    CHAR(11),
      fecha_accion2    CHAR(08),
      resultado        CHAR(01),
      diag1            CHAR(03),
      diag2            CHAR(03),
      diag3            CHAR(03)
   END RECORD

   DEFINE reg_sum RECORD
      tipo_registro    CHAR(02),
      ident_servicio   CHAR(02),
      ident_operacion  CHAR(02),
      tipo_ent_origen  CHAR(02),
      cve_ent_origen   CHAR(03),
      tipo_ent_destino CHAR(02),
      cve_ent_destino  CHAR(03),
      fecha_creac_lote CHAR(08),
      impt_total_rcv   CHAR(15),
      impt_total_viv   CHAR(15),
      num_registros    INTEGER
   END RECORD

   DEFINE 
      hoy       DATE,
      opc       CHAR(01),
      vcomando  CHAR(01),
      generar   CHAR(15),
      comando   CHAR(200),
      carga_reg CHAR(295)
     
   DEFINE
      vfecha_lote        DATE,
      vfolio             INTEGER,
      cla_sel            CHAR(300),
      cla_sel2           CHAR(300),
      cla_sel3           CHAR(300),
      cla_sel4           CHAR(300),
      cla_sel5           CHAR(300),
      vsalida            CHAR(100),
      vtot_int           DECIMAL(15,2),
      vfec_creac_lote    CHAR(08),
      vtotal_rcv         DECIMAL(15,2),
      vsuma_rcv          DECIMAL(15,2),
      vsuma_vol          DECIMAL(15,2),
      vsuma_gub          DECIMAL(15,2),
      vtotal_viv         DECIMAL(15,2),
      vchar_totrcv       CHAR(15),
      vchar_totviv       CHAR(15),
      cla                CHAR(2500),
      vresult_operacion  CHAR(01),
      vnum_acc1          DECIMAL(22,6),
      vvalor_acc1        DECIMAL(22,6),
      y1                 CHAR(17),
      y2                 CHAR(12),
      vconsec            INTEGER,
      vfec_valorviv      CHAR(08),
      vsdo_subc1         DECIMAL(22,6),
      vsdo_subc2         DECIMAL(22,6),
      vsdo_subc3         DECIMAL(22,6),
      vsdo_subc5         DECIMAL(22,6),
      vsdo_subc6         DECIMAL(22,6),
      vsdo_subc9         DECIMAL(22,6),
      vmonto_subc1       DECIMAL(22,6),
      vmonto_subc2       DECIMAL(22,6),
      vmonto_subc3       DECIMAL(22,6),
      vmonto_subc5       DECIMAL(22,6),
      vmonto_subc6       DECIMAL(22,6),
      vmonto_subc9       DECIMAL(22,6),
      vmto_viv           DECIMAL(22,6),
      vinteres           DECIMAL(16,6),
      vinteres_rcv       DECIMAL(16,6),
      g_grep_00          CHAR(1500), 
      g_grep_01          CHAR(1500)

   DEFINE vfecha_valor DATE,
          vmes         SMALLINT,
          vdia         SMALLINT,
          vano         SMALLINT,
          vcontador    INTEGER
      
   DEFINE vimpt_com_saldo CHAR(07)

   DEFINE viv decimal(15,2)

   DEFINE vcom_saldo        DECIMAL(15,2),
          vfecha_conversion DATE

   DEFINE g_corte_cambio,g_fecha_cambio DATE,
          g_tasa_1,g_tasa_2,g_precio_ant,g_sel_saldo  DECIMAL(16,6)
    DEFINE reg_1 RECORD #loc #reg_1
        nss                   CHAR(11)      ,
        subcuenta             SMALLINT      ,
        fecha_conversion      DATE          ,
        monto_en_acciones     DECIMAL(16,6) ,
        tipo  CHAR(01)
    END RECORD

   DEFINE promedio char(100)

   DEFINE COMSALDO decimal(15,2)

   DEFINE vconsecutivo INTEGER
   DEFINE vusuario     CHAR(08)

   DEFINE vacc_sub1    DECIMAL(22,6)
   DEFINE vacc_sub2    DECIMAL(22,6)
   DEFINE vacc_sub3    DECIMAL(22,6)
   DEFINE vacc_sub5    DECIMAL(22,6)
   DEFINE vacc_sub6    DECIMAL(22,6)
   DEFINE vacc_sub9    DECIMAL(22,6)
   DEFINE vint1        DECIMAL(22,6)
   DEFINE vint2        DECIMAL(22,6)
   DEFINE vint3        DECIMAL(22,6)
   DEFINE vint5        DECIMAL(22,6)
   DEFINE vint6        DECIMAL(22,6)
   DEFINE vint9        DECIMAL(22,6)
   DEFINE vsal1        DECIMAL(22,6)
   DEFINE vsal3        DECIMAL(22,6)

   DEFINE vfolio_glo     INTEGER
     
END GLOBALS

MAIN
   OPTIONS 
      INPUT WRAP,
      PROMPT LINE LAST  ,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT
        
   CALL init()
   CALL captura()

END MAIN


FUNCTION init()
#--------------
   INITIALIZE reg_cza.*  TO NULL
   INITIALIZE reg_det.*  TO NULL
   INITIALIZE reg_sum.*  TO NULL
   LET hoy               = TODAY

   SELECT codigo_afore
     INTO vcodigo_afore
     FROM tab_afore_local

   SELECT *
   INTO   g_param_dis.*
   FROM   seg_modulo
   WHERE modulo_cod = "dis"

   SELECT USER
   INTO   vusuario
   FROM   seg_modulo
   GROUP BY 1


      LET Vtotal_rcv = 0
      LET Vtotal_viv = 0
  # AFORE ACTIVER
    LET g_tasa_1         = 0.24
    LET g_tasa_2         = 0.24
    LET g_corte_cambio   = "01/01/2004"
    LET g_fecha_cambio   = "12/30/2003"

    LET promedio = " SELECT AVG(precio_del_dia),COUNT(*) ",
         " FROM glo_valor_accion ",
         " WHERE fecha_valuacion BETWEEN ? ",
         " AND ? "

    SELECT MAX(folio) + 1
    INTO   vfolio_glo
    FROM   glo_folio
    INSERT INTO glo_folio
    VALUES(vfolio_glo)
END FUNCTION

FUNCTION captura()
   OPEN WINDOW ven1 AT 3,2 WITH FORM "DISB0711" ATTRIBUTE(BORDER)
   DISPLAY " DISB071    APORTACIONES INCORRECTAS PROCESAR                      " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "dd-mm-yyyy" AT 2,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME vfecha_lote,vfolio
      AFTER FIELD vfecha_lote
         IF vfecha_lote IS NULL THEN
            ERROR "Fecha NO puede ser NULA"
            NEXT FIELD vfecha_lote
         END IF
 
      AFTER FIELD vfolio
         IF vfolio IS NULL THEN
            ERROR "EL folio NO puede ser NULO"
            NEXT FIELD vfolio
         END IF

      ON KEY (ESC)
         IF vfecha_lote IS NULL THEN
            ERROR "Fecha NO puede ser NULA"
            NEXT FIELD vfecha_lote
         END IF
         IF vfolio IS NULL THEN
            ERROR "El folio NO puede ser NULO"
            NEXT FIELD vfolio
         END IF

         PROMPT "Deseas lanzar el proceso [S/N]..." FOR opc
         IF opc MATCHES "[Ss]" THEN
            ERROR "Procesando Informacion"
-----            CALL genera_temporales()
-----            CALL genera_cza()
            CALL genera_det()
-----            CALL genera_sum()
-----            CALL genera_archivo()

            ERROR "PROCESO TERMINADO"
            sleep 2

         ELSE
            ERROR "Proceso Cancelado"
            SLEEP 2
         END IF
         EXIT INPUT

         ON KEY (INTERRUPT)
         ERROR "Proceso Cancelado"
         SLEEP 2
         EXIT INPUT
   END INPUT

END FUNCTION

FUNCTION genera_temporales()

 DATABASE safre_tmp
 WHENEVER ERROR CONTINUE
   DROP TABLE tmp_discuenta 

 WHENEVER ERROR STOP
 
   CREATE TABLE tmp_discuenta
  (
    tipo_movimiento smallint not null ,
    subcuenta smallint not null ,
    siefore smallint,
    folio integer not null ,
    consecutivo_lote integer,
    nss char(11) not null ,
    curp char(18),
    folio_sua char(6),
    fecha_pago date,
    fecha_valor date,
    fecha_conversion date,
    monto_en_pesos decimal(22,6),
    monto_en_acciones decimal(22,6),
    precio_accion decimal(22,6),
    dias_cotizados smallint,
    sucursal char(10),
    id_aportante char(11),
    estado smallint,
    fecha_proceso date,
    usuario char(8),
    fecha_archivo date,
    etiqueta smallint
  ); 

  CREATE INDEX ix_disapo ON tmp_discuenta(nss,folio_sua,id_aportante,subcuenta,tipo_movimiento,consecutivo_lote)

  INSERT INTO tmp_discuenta
  SELECT * 
  FROM   safre_af:dis_cuenta
  WHERE  nss IN (SELECT nss FROM safre_tmp:tmp_det_aporte)

  UPDATE STATISTICS FOR TABLE tmp_discuenta
 
 DATABASE safre_af

END FUNCTION



FUNCTION genera_cza()

   SELECT * 
   INTO   reg_cza.*
   FROM   safre_tmp:tmp_cza_aporte 

   LET vsalida = g_param_dis.ruta_envio CLIPPED,"/cza_apo"
   START REPORT rep_cza TO vsalida

    LET reg_cza.tipo_registro     = "01"
    LET reg_cza.ident_servicio    = "03"
    LET reg_cza.ident_operacion   = "60"
    LET reg_cza.tipo_ent_origen   = "01"
    LET reg_cza.cve_ent_origen    = vcodigo_afore
    LET reg_cza.tipo_ent_destino  = "03"
    LET reg_cza.cve_ent_destino   = "001"
    LET reg_cza.result_operacion  = ' '
    LET reg_cza.diag1             = ' '
    LET reg_cza.diag2             = ' '
    LET reg_cza.diag3             = ' '

OUTPUT TO REPORT rep_cza(reg_cza.*)
   FINISH REPORT rep_cza

END FUNCTION

REPORT rep_cza(reg_cza)

DEFINE reg_cza RECORD
      tipo_registro    CHAR(02),
      ident_servicio   CHAR(02),
      ident_operacion  CHAR(02),
      tipo_ent_origen  CHAR(02),
      cve_ent_origen   CHAR(03),
      tipo_ent_destino CHAR(02),
      cve_ent_destino  CHAR(03),
      fecha_creac_lote CHAR(08),
      result_operacion CHAR(02),
      diag1            CHAR(03),
      diag2            CHAR(03),
      diag3            CHAR(03)
   END RECORD

   OUTPUT
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1
   FORMAT
      ON EVERY ROW

         PRINT COLUMN 1, reg_cza.tipo_registro,
                         reg_cza.ident_servicio,
                         reg_cza.ident_operacion,
                         reg_cza.tipo_ent_origen,
                         reg_cza.cve_ent_origen,
                         reg_cza.tipo_ent_destino,
                         reg_cza.cve_ent_destino,
                         reg_cza.fecha_creac_lote,
                         reg_cza.result_operacion,
                         reg_cza.diag1 USING '###',
                         reg_cza.diag2 USING '###',
                         reg_cza.diag3 USING '###',
                         255 SPACES

END REPORT


##-----------------------------------------------------------------------
##Realiza el calculo de intereses
##-----------------------------------------------------------------------
FUNCTION calcula_interes_viv(gp1_monto_pesos,gp1_fecha_valor)

   DEFINE gp1_fecha_valor   DATE
   DEFINE vmes_calculo      SMALLINT 
   DEFINE vtasa_opera       DECIMAL(11,6)
   DEFINE vmonto_acum       DECIMAL(16,6)
   DEFINE vint_aplica       DECIMAL(16,6)
   DEFINE gp1_monto_pesos   DECIMAL(16,6)
   DEFINE vband             CHAR(01)
   DEFINE registros         SMALLINT
   DEFINE vfecha_calculo    DATE
   DEFINE gr_tasa_oper      DECIMAL(10,6)
   DEFINE vfecha_masunmes   DATE
   DEFINE vfecha_hasta      DATE


      LET vfecha_hasta = "06/01/2004"
      LET vint_aplica = 0
      LET vmonto_acum = 0

      LET vmes_calculo = month(gp1_fecha_valor) + 1

      IF vmes_calculo = 13 THEN
         LET vfecha_calculo = MDY(1,1,YEAR(gp1_fecha_valor)+1)
      ELSE
         LET vfecha_calculo = MDY(vmes_calculo,1,YEAR(gp1_fecha_valor))
      END IF

      LET vband = "S" 


      WHILE TRUE

         SELECT tasa_valor
         INTO   vtasa_opera
         FROM   tab_tasa_remanente
         WHERE  tasa_fecha = vfecha_calculo
         AND    tasa_origen = "VIV" 

         IF STATUS = NOTFOUND THEN
            LET vtasa_opera = gr_tasa_oper 
         END IF

         IF vband = "S" THEN

            --------------------  I N T E R E S  ---------------------
            LET vint_aplica = gp1_monto_pesos *
                              (vfecha_calculo - gp1_fecha_valor) *
                             (vtasa_opera / 36000) 

            LET vmonto_acum = vmonto_acum +
                              vint_aplica +    --intereses
                              gp1_monto_pesos  ---aporte
            ----------------------------------------------------------
            LET vband = "N"

         ELSE
            LET vfecha_masunmes = vfecha_calculo-1 UNITS MONTH 

            --------------------  I N T E R E S  ---------------------
            LET vint_aplica = vmonto_acum *
                              (vfecha_calculo - vfecha_masunmes) *
                              (vtasa_opera / 36000) 

            LET vmonto_acum = vmonto_acum + vint_aplica
            ----------------------------------------------------------
         END IF

         IF vfecha_calculo >= vfecha_hasta THEN
            EXIT WHILE 
         END IF

         LET vfecha_calculo = vfecha_calculo + 1 UNITS MONTH

      END WHILE

  RETURN vmonto_acum

END FUNCTION

FUNCTION prepara()

    let cla_sel = "SELECT SUM(monto_en_acciones) ",
                  "FROM  safre_tmp:tmp_discuenta ",
                  "WHERE nss = ? ",
                  " AND  folio_sua = ? ",
                  " AND  id_aportante = ? ",
                  " AND  subcuenta = ? ",
                  " AND  tipo_movimiento in (1,2,3,100,101,102,103,104) "
     PREPARE claexe1 FROM cla_sel

   let cla_sel2 = "SELECT SUM(monto_en_acciones) ",
                  "FROM  safre_tmp:tmp_discuenta ",
                  "WHERE nss = ? ",
                  " AND   subcuenta = ? ",
                  " AND   tipo_movimiento = 3 ",
                  " AND   consecutivo_lote = ? "
   PREPARE claexe2 FROM cla_sel2

   LET cla_sel3 = "SELECT SUM(monto_en_pesos) ",
                  "FROM  safre_tmp:tmp_discuenta ",
                  "WHERE nss              = ? ",
                  " AND   folio_sua        = ? ",
                  " AND   id_aportante     = ? ",
                  " AND   subcuenta        = ? ",
                  " AND   tipo_movimiento  in (100,101,102,103,104) "
   PREPARE claexe3 FROM cla_sel3

   LET cla_sel4 = "SELECT SUM(monto_en_acciones),fecha_conversion ",
                  "FROM  safre_tmp:tmp_discuenta ",
                  "WHERE nss = ? ",
                  " AND  folio_sua = ? ",
                  " AND  id_aportante = ? ",
                  " AND  subcuenta in (1,2,5,6,9) ",
                  " GROUP BY 2 "
   PREPARE claexe4 FROM cla_sel4

   LET cla_sel5 = "SELECT SUM(monto_en_acciones),fecha_conversion ",
                  "FROM  safre_tmp:tmp_discuenta ",
                  "WHERE nss = ? ",
                  " AND  folio_sua = ? ",
                  " AND  id_aportante = ? ",
                  " AND  subcuenta = 3 ",
                  " GROUP BY 2 "
   PREPARE claexe5 FROM cla_sel5

END FUNCTION

FUNCTION genera_det()
   LET vtot_int          = 0
   LET vconsec           = 0
   LET vfec_valorviv     = ' '
   LET vmto_viv          = 0.0
   LET vinteres          = 0.0
   LET vcontador = 0
   LET viv = 0
   LET vsuma_rcv = 0
   LET vsuma_vol = 0
   LET vsuma_gub = 0
   LET vacc_sub1 = 0 
   LET vacc_sub2 = 0
   LET vacc_sub3 = 0
   LET vacc_sub5 = 0
   LET vacc_sub6 = 0
   LET vacc_sub9 = 0
   LET vint1     = 0
   LET vint2     = 0
   LET vint3     = 0
   LET vint5     = 0
   LET vint6     = 0
   LET vint9     = 0
   LET vsal1     = 0
   LET vsal3     = 0

   LET vsalida = g_param_dis.ruta_envio CLIPPED,"/det_apo"

   LET vtotal_rcv = 0
   LET vtotal_viv = 0

   SELECT precio_del_dia
   INTO   vvalor_acc1
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = "06/01/2004"

   IF vvalor_acc1 IS NULL THEN
      LET vvalor_acc1 = 0.0
   END IF


    CALL prepara()

 let cla = "select * from safre_tmp:tmp_det_aporte " 
-- let cla = "select * from safre_tmp:tmp_det_aporte WHERE nss='51803300212' " 
   PREPARE cust_stmt FROM cla
   DECLARE curdet CURSOR FOR cust_stmt

   LET reg_det.tipo_registro  = "02"
   LET reg_det.ident_servicio = "03"

   FOREACH curdet INTO reg_det.*

   LET vacc_sub1 = 0 
   LET vacc_sub2 = 0
   LET vacc_sub3 = 0
   LET vacc_sub5 = 0
   LET vacc_sub6 = 0
   LET vacc_sub9 = 0
   LET vint1     = 0
   LET vint2     = 0
   LET vint3     = 0
   LET vint5     = 0
   LET vint6     = 0
   LET vint9     = 0
   LET vsal1     = 0
   LET vsal3     = 0

         LET vconsec = 0 

         SELECT consec_reg_lote,
                fech_valor_viv
         INTO   vconsec,
                vfec_valorviv
         FROM   dis_det_aporte
         WHERE  folio             = vfolio
         AND    n_seguro          = reg_det.nss
         AND    periodo_pago      = reg_det.periodo_pago
         AND    folio_pago_sua    = reg_det.folio_sua
         AND    reg_patronal_imss = reg_det.nrp_patron
         AND    result_operacion  = '02'
         GROUP BY 1,2

         IF STATUS <> NOTFOUND THEN

            LET vcontador = vcontador + 1


               EXECUTE claexe1 USING reg_det.nss,
                                     reg_det.folio_sua,
                                     reg_det.nrp_patron,
                                     "1"
               INTO vacc_sub1
               IF vacc_sub1 IS NULL  THEN
                  LET vacc_sub1 = 0
               END IF

------------------------------------------------------------------
               EXECUTE claexe1 USING reg_det.nss,
                                     reg_det.folio_sua,
                                     reg_det.nrp_patron,
                                     "2"
               INTO vacc_sub2
               IF vacc_sub2 IS NULL  THEN
                  LET vacc_sub2 = 0
               END IF

------------------------------------------------------------------
               EXECUTE claexe1 USING reg_det.nss,
                                     reg_det.folio_sua,
                                     reg_det.nrp_patron,
                                     "3"
               INTO vacc_sub3
               IF vacc_sub3 IS NULL  THEN
                  LET vacc_sub3 = 0
               END IF

------------------------------------------------------------------
               EXECUTE claexe1 USING reg_det.nss,
                                     reg_det.folio_sua,
                                     reg_det.nrp_patron,
                                     "5"
               INTO vacc_sub5
               IF vacc_sub5 IS NULL  THEN
                  LET vacc_sub5 = 0
               END IF
------------------------------------------------------------------
               EXECUTE claexe1 USING reg_det.nss,
                                     reg_det.folio_sua,
                                     reg_det.nrp_patron,
                                     "6"
               INTO vacc_sub6
               IF vacc_sub6 IS NULL  THEN
                  LET vacc_sub6 = 0
               END IF
------------------------------------------------------------------

               EXECUTE claexe1 USING reg_det.nss,
                                     reg_det.folio_sua,
                                     reg_det.nrp_patron,
                                     "9"
               INTO vacc_sub9
               IF vacc_sub9 IS NULL  THEN
                  LET vacc_sub9 = 0
               END IF
------------------------------------------------------------------

               SELECT unique consecutivo_lote
               INTO   vconsecutivo
               FROM  safre_tmp:tmp_discuenta
               WHERE nss              = reg_det.nss
               AND   folio_sua        = reg_det.folio_sua
               AND   id_aportante     = reg_det.nrp_patron
               AND   subcuenta        in (1,2,3,5,6,9)
               AND   tipo_movimiento  = 1

               EXECUTE claexe2 USING reg_det.nss,
                                     "1",
                                     vconsecutivo
               INTO vint1
               IF vint1 IS NULL THEN
                  LET vint1 = 0
               END IF
------------------------------------------------------------------
               EXECUTE claexe2 USING reg_det.nss,
                                     "2",
                                     vconsecutivo
               INTO vint2
               IF vint2 IS NULL THEN
                  LET vint2 = 0
               END IF
------------------------------------------------------------------
               EXECUTE claexe2 USING reg_det.nss,
                                     "3",
                                     vconsecutivo
               INTO vint3
               IF vint3 IS NULL THEN
                  LET vint3 = 0
               END IF
------------------------------------------------------------------
               EXECUTE claexe2 USING reg_det.nss,
                                     "5",
                                     vconsecutivo
               INTO vint5
               IF vint5 IS NULL THEN
                  LET vint5 = 0
               END IF
------------------------------------------------------------------
               EXECUTE claexe2 USING reg_det.nss,
                                     "6",
                                     vconsecutivo
               INTO vint6
               IF vint6 IS NULL THEN
                  LET vint6 = 0
               END IF
------------------------------------------------------------------
               EXECUTE claexe2 USING reg_det.nss,
                                     "9",
                                     vconsecutivo
               INTO vint9
               IF vint9 IS NULL THEN
                  LET vint9 = 0
               END IF
------------------------------------------------------------------


              LET vacc_sub1 = vacc_sub1 + vint1
              CALL Agrega_a_dis_cuenta(reg_det.nss,vacc_sub1,1,"N","A")

              LET vacc_sub2 = vacc_sub2 + vint2
              CALL Agrega_a_dis_cuenta(reg_det.nss,vacc_sub2,2,"N","A")

              LET vacc_sub3 = vacc_sub3 + vint3
              CALL Agrega_a_dis_cuenta(reg_det.nss,vacc_sub3,3,"N","A")

              LET vacc_sub5 = vacc_sub5 + vint5
              CALL Agrega_a_dis_cuenta(reg_det.nss,vacc_sub5,5,"N","A")

              LET vacc_sub6 = vacc_sub6 + vint6
              CALL Agrega_a_dis_cuenta(reg_det.nss,vacc_sub6,6,"N","A")

              LET vacc_sub9 = vacc_sub9 + vint9
              CALL Agrega_a_dis_cuenta(reg_det.nss,vacc_sub9,9,"N","A")



------------------------------------------------------------------
               EXECUTE claexe4 USING reg_det.nss,
                                     reg_det.folio_sua,
                                     reg_det.nrp_patron
               INTO vcom_saldo,vfecha_conversion
               IF vcom_saldo IS NULL THEN
                  LET vcom_saldo = 0
               END IF
               CALL Calcula_comision_aporte(vcom_saldo,vfecha_conversion)
               RETURNING vsal1

              CALL Agrega_a_dis_cuenta(reg_det.nss,vsal1,1,"P","C")
------------------------------------------------------------------
               EXECUTE claexe5 USING reg_det.nss,
                                     reg_det.folio_sua,
                                     reg_det.nrp_patron
               INTO vcom_saldo,vfecha_conversion
               IF vcom_saldo IS NULL THEN
                  LET vcom_saldo = 0
               END IF

               CALL Calcula_comision_aporte(vcom_saldo,vfecha_conversion)
               RETURNING vsal3
              CALL Agrega_a_dis_cuenta(reg_det.nss,vsal3,3,"P","C")
------------------------------------------------------------------
               --Para el importe total de la vivienda---------
               SELECT SUM(monto_en_pesos)
               INTO  vmto_viv 
               FROM  safre_tmp:tmp_discuenta
               WHERE nss              = reg_det.nss
               AND   folio_sua        = reg_det.folio_sua
               AND   id_aportante     = reg_det.nrp_patron
               AND   subcuenta        = 4
               AND   tipo_movimiento  = 1

               IF vmto_viv IS NULL THEN 
                  LET vmto_viv = 0
               END IF

               LET vmes = vfec_valorviv[5,6]
               LET vdia = vfec_valorviv[7,8]
               LET vano = vfec_valorviv[1,4]
               LET vfecha_valor = mdy(vmes,vdia,vano)

               --Buscamos el interes hasta abril
               CALL calcula_interes_viv(vmto_viv,vfecha_valor)
               RETURNING vinteres 

               IF vinteres IS NULL THEN
                  LET vinteres = 0
               END IF
              CALL Agrega_a_dis_cuenta(reg_det.nss,vinteres,4,"N","A")

{
display ""
display "vacc_sub1 ",vacc_sub1
display "vacc_sub2 ",vacc_sub2
display "vacc_sub3 ",vacc_sub3
display "vacc_sub5 ",vacc_sub5
display "vacc_sub6 ",vacc_sub6
display "vacc_sub9 ",vacc_sub9
display "vinteres  ",vinteres
display "nss       ",reg_det.nss
display "folio sua ",reg_det.folio_sua
display "nr pat    ",reg_det.nrp_patron
exit program
}

         END IF

   END FOREACH
 
END FUNCTION


FUNCTION Calcula_comision_aporte(vimpt_saldo,fecha_conver)
   DEFINE vimpt_saldo DECIMAL(15,2),
          fecha_conver DATE,
          x_fecha_ini_prov DATE,
          x_fecha_fin_prov DATE

    DEFINE reg_1 RECORD #loc #reg_1
        nss                   CHAR(11)      ,
        subcuenta             SMALLINT      ,
        fecha_conversion      DATE          ,
        monto_en_acciones     DECIMAL(16,6) ,
        tipo  CHAR(01)
    END RECORD

    PREPARE cla_promedio FROM promedio

   LET reg_1.nss = reg_det.nss
   LET reg_1.subcuenta = 1
   LET reg_1.fecha_conversion = fecha_conver
   LET reg_1.monto_en_acciones = vimpt_saldo
   LET x_fecha_ini_prov = f_calculo(fecha_conver) + 1
   LET x_fecha_fin_prov = "06/01/2004"

   CALL calcula_comision(reg_1.*,x_fecha_ini_prov,x_fecha_fin_prov)

  RETURN comsaldo

END FUNCTION

FUNCTION f_calculo (x_fecha)

   DEFINE
      diaSemana      SMALLINT,
      feriado     SMALLINT,
      finSemana      SMALLINT,
      x_fecha     DATE,
      x_fecha_calculo   DATE

      LET diaSemana = NULL
      LET feriado   = NULL
      LET finSemana = NULL

      LET diaSemana = WEEKDAY(x_fecha + 1)

      IF diaSemana = 0 OR diaSemana = 6 THEN
          LET finSemana = 1
      END IF

      SELECT *
      FROM   tab_feriado
      WHERE  feria_fecha = (x_fecha + 1)

      IF STATUS <> NOTFOUND THEN
          LET feriado = 1
      END IF

      IF feriado = 1 OR finSemana = 1 THEN
          CALL habil_siguiente(x_fecha + 1)
               RETURNING x_fecha_calculo
          LET x_fecha_calculo = x_fecha_calculo - 1
      ELSE
          LET x_fecha_calculo = x_fecha
      END IF

      RETURN x_fecha_calculo

END FUNCTION #fc

FUNCTION calcula_comision (x_reg, x_fecha_ini_prov, x_fecha_fin_prov)
#cc------------------------------------------------------------------

    DEFINE x_reg RECORD #loc #x_reg
        nss                   CHAR(11)      ,
        subcuenta             SMALLINT      ,
        fecha_conversion      DATE          ,
        monto_en_acciones     DECIMAL(16,6) ,
        tipo  CHAR(01)
    END RECORD

    DEFINE reg_com RECORD #loc #reg_com
        nss                   CHAR(11)      ,
        subcuenta             SMALLINT      ,
        fecha_aplica          DATE          ,
        monto_provision       DECIMAL(16,6) ,
        total_acciones        DECIMAL(16,6)
    END RECORD

    DEFINE #loc #date
        x_fecha_ini_prov      DATE ,
        x_fecha_fin_prov      DATE ,
        x_fecha_cambio        DATE

    DEFINE #loc #smallint
        s_dias1               SMALLINT ,
        s_dias2               SMALLINT ,
        s_dias                SMALLINT

    DEFINE #loc #decimal
        d_comis_saldo         DECIMAL(16,6) ,
        d_comis_saldo_pre1    DECIMAL(16,6) ,
        d_comis_saldo_pre2    DECIMAL(16,6) ,
        d_precio_del_dia1     DECIMAL(16,6) ,
        d_precio_del_dia2     DECIMAL(16,6) ,
        d_precio_del_dia      DECIMAL(16,6) ,
        x_tasa                DECIMAL(16,6)


    LET s_dias1 = 0
    LET s_dias2 = 0
    LET s_dias = 0
    LET d_comis_saldo = 0
    LET d_comis_saldo_pre1 = 0
    LET d_comis_saldo_pre2 = 0
    LET d_precio_del_dia1 = 0
    LET d_precio_del_dia2 = 0
    LET d_precio_del_dia = 0
    LET x_tasa = 0

    IF x_fecha_fin_prov <> g_corte_cambio THEN

   CALL precio_del_dia ( x_fecha_ini_prov, x_fecha_fin_prov )
        RETURNING d_precio_del_dia, s_dias


        IF x_fecha_fin_prov < g_fecha_cambio THEN
            LET x_tasa = g_tasa_1
        ELSE
            LET x_tasa = g_tasa_2
        END IF

        LET d_comis_saldo = x_reg.monto_en_acciones *
                            s_dias                  *
                            d_precio_del_dia        *
                            x_tasa/36500

        IF d_comis_saldo IS NULL THEN
            LET d_comis_saldo = 0
        END IF
    ELSE
        LET x_fecha_cambio = g_fecha_cambio

   CALL precio_del_dia ( x_fecha_ini_prov, x_fecha_cambio )
        RETURNING d_precio_del_dia1, s_dias1

        IF s_dias1 IS NULL THEN
            LET s_dias1 = 0
            LET d_precio_del_dia1 = 0
        END IF

        LET d_comis_saldo_pre1 = x_reg.monto_en_acciones *
                                 s_dias1                 *
                                 d_precio_del_dia1       *
                                 g_tasa_1/36500

        IF d_comis_saldo_pre1 IS NULL THEN
            LET d_comis_saldo_pre1 = 0
        END IF

        IF x_fecha_ini_prov > g_fecha_cambio THEN
            LET x_fecha_cambio = x_fecha_ini_prov - 1
        END IF

   CALL precio_del_dia ( x_fecha_cambio + 1, x_fecha_fin_prov )
        RETURNING d_precio_del_dia2, s_dias2

        LET d_comis_saldo_pre2 = x_reg.monto_en_acciones *
                                 s_dias2                 *
                                 d_precio_del_dia2       *
                                 g_tasa_2/36500

        IF d_comis_saldo_pre2 IS NULL THEN
            LET d_comis_saldo_pre2 = 0
        END IF

        LET d_comis_saldo = d_comis_saldo_pre1 +
                            d_comis_saldo_pre2
    END IF

    IF x_reg.subcuenta <> 7 AND x_reg.subcuenta <> 3 THEN
        LET x_reg.subcuenta = 1
    END IF

    LET reg_com.nss             = x_reg.nss
    LET reg_com.subcuenta       = x_reg.subcuenta
    LET reg_com.fecha_aplica    = x_fecha_fin_prov + 1
    LET reg_com.monto_provision = d_comis_saldo
    LET reg_com.total_acciones  = x_reg.monto_en_acciones

    LET COMSALDO = d_comis_saldo

END FUNCTION #cc

FUNCTION habil_siguiente(diaActual)
#hs--------------------------------

   DEFINE diaTmp  DATE,
        contador  SMALLINT,
     diaActual DATE

   DEFINE diaHabilSig   DATE,
     diaSemana SMALLINT,
     feriado   SMALLINT,
     finSemana SMALLINT

   LET diaHabilSig = diaActual

   WHILE TRUE
          LET feriado   = 0
          LET finSemana = 0
          LET diaSemana = WEEKDAY(diaHabilSig)

          IF diaSemana = 0 OR diaSemana = 6 THEN
                 LET finSemana = 1
          END IF

          SELECT *
          FROM   tab_feriado
          WHERE  feria_fecha = diaHabilSig

          IF STATUS <> NOTFOUND THEN
                 LET feriado = 1
          END IF

          IF feriado = 1 OR finSemana = 1 THEN
                 LET diaHabilSig = diaHabilSig + 1 UNITS DAY
          ELSE
                 EXIT WHILE
          END IF
   END WHILE

   RETURN diaHabilSig

END FUNCTION #hs

FUNCTION precio_del_dia ( fecha_ini, fecha_fin )
#pd

DEFINE  fecha_ini,
   fecha_fin     DATE

DEFINE  precio        DECIMAL(16,6) ,
   dias          SMALLINT


DECLARE cur_pre CURSOR FOR cla_promedio

OPEN cur_pre USING fecha_ini, fecha_fin

FETCH cur_pre INTO precio, dias

CLOSE cur_pre

FREE cur_pre

RETURN precio,dias

END FUNCTION


##------------------------------------------------------------------------
## Agrega infor en dis_cuenta
##------------------------------------------------------------------------
FUNCTION Agrega_a_dis_cuenta(vn_seguro,
                             vmonto,
                             vsubc,
                             vsigno,
                             vtipo)


  DEFINE vn_seguro      CHAR(11)
  DEFINE vmonto         DECIMAL(22,6)
  DEFINE vsubc          SMALLINT
  DEFINE vsigno         CHAR(01)
  DEFINE vtipo          CHAR(01)
  DEFINE vtotal_viv2    DECIMAL(15,2)
  DEFINE vvalor_acc     DECIMAL(22,6)
  DEFINE reg_dis RECORD LIKE dis_cuenta.*
  DEFINE fconver        DATE
  DEFINE x_fecha_ini_prov DATE
  DEFINE x_fecha_fin_prov DATE
  DEFINE d_precio_del_dia  DECIMAL(16,6)
  
  IF vsubc = 4 AND vtipo = "A"  THEN
     LET reg_dis.subcuenta         =   4
     LET reg_dis.monto_en_pesos    =   vmonto * (-1)
     LET reg_dis.monto_en_acciones =   0.0
     LET reg_dis.siefore           =   0      
     LET reg_dis.precio_accion     =   0
     LET reg_dis.tipo_movimiento   =   610
  END IF

  IF vtipo = "A" AND vsubc <> 4 THEN
     LET reg_dis.subcuenta         =   vsubc
     LET reg_dis.monto_en_pesos    =   vvalor_acc1 * vmonto *(-1)
     LET reg_dis.monto_en_acciones =   vmonto * -1
     LET reg_dis.siefore           =   1
     LET reg_dis.precio_accion     =   vvalor_acc1
     LET reg_dis.tipo_movimiento   =   610
  END IF

  IF vtipo = "C" AND vsubc <> 4 THEN
     LET reg_dis.subcuenta         =   vsubc
     LET reg_dis.monto_en_pesos    =   vmonto
     LET reg_dis.monto_en_acciones =   0
     LET reg_dis.siefore           =   1
     LET reg_dis.precio_accion     =   0
     LET reg_dis.tipo_movimiento   =   620
  END IF

    LET reg_dis.folio                =   vfolio_glo
    LET reg_dis.consecutivo_lote     =   0      --?
    LET reg_dis.nss                  =   reg_det.nss
    LET reg_dis.curp                 =   ' '     --?
    LET reg_dis.folio_sua            =   ' '
    LET reg_dis.fecha_pago           =   today
    LET reg_dis.fecha_valor          =   today
    LET reg_dis.fecha_conversion     =   today
    LET reg_dis.dias_cotizados = 0
    LET reg_dis.sucursal             =   ' '     --?
    LET reg_dis.id_aportante         =   "AC-APO-INDE"
    LET reg_dis.estado               =   0
    LET reg_dis.fecha_proceso        =   today
    LET reg_dis.usuario              =   vusuario 
    LET reg_dis.fecha_archivo        =   today
    LET reg_dis.etiqueta             =   0

    INSERT INTO safre_tmp:dis_cuenta_tmp
    VALUES(reg_dis.*)

END FUNCTION
