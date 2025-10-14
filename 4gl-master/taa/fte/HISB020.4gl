##########################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                        #
#Propietario       => E.F.P.                                             #
#Actualizacion     =>                                                    #
#Programa HIST002  => INFORMACION HISTORICA TRASPASOS AFORE - AFOR  P2/3 # 
#Por               => JESUS DAVID YANEZ MORENO                           #
#Fecha creacion    => 14 AGOSTO 2000                                     #
#Fecha actualiz.   => 28 octubre 2000                                    #
#Sistema           => HIST                                               #
##########################################################################

DATABASE safre_af

GLOBALS 
define s_cve_afore  SMALLINT
define c_cve_afore  CHAR(003)
define lanza_uni CHAR(100)
define v_conver smallint
define tt char(050)
define aa1 smallint
define aa2 smallint
define vv_siefore char(007)
define lanza char(100)
define vv_fecha_ini DATE
define vv_fecha_fin DATE
define xx char(002)
define v_fecha_traspaso ,
       vv_fecha_traspaso DATE,
       v_fecha_apertura DATE
define v_cuantos   smallint
define v_comis     decimal(12,2)

     DEFINE l_fech_pago       CHAR(008) 
  
     DEFINE v_id              CHAR(003)

     DEFINE reg_paso1         RECORD 
            subcuenta         SMALLINT      ,
            fecha_conversion  DATE          ,
            monto_en_pesos    DECIMAL(13,2) ,
            monto_en_acciones DECIMAL(13,2) ,
            precio_accion     LIKE safre_af:dis_cuenta.precio_accion
     END RECORD

     DEFINE reg_taa_rcv_recepcion  RECORD LIKE safre_af:taa_rcv_recepcion.*
     DEFINE reg_taa_viv_recepcion  RECORD LIKE safre_af:taa_viv_recepcion.*

     DEFINE reg_220          RECORD 
            tipo_recep_cuenta  CHAR(002)                             ,
            cve_recep_cuenta   CHAR(003)                             ,
            tipo_ced_cuenta    CHAR(002)                             ,
            cve_ced_cuenta     CHAR(003)                             ,
            fecha_conversion   DATE                                  ,
            nor_comp           smallint                              
     END RECORD 


     DEFINE reg_m1dis        RECORD  LIKE safre_af:dis_det_aporte.*
     DEFINE reg_m2dis        RECORD  LIKE safre_af:dis_det_aporte.*

    DEFINE  reg_230          RECORD 
            fecha_trasp       LIKE safre_af:acr_det_cedido.fecha_trasp  , 
            tipo_recep_cuenta LIKE safre_af:acr_det_cedido.tipo_recep_cuenta  ,
            cve_recep_cuenta  LIKE safre_af:acr_det_cedido.cve_recep_cuenta   ,
            tipo_ced_cuenta   LIKE safre_af:acr_det_cedido.tipo_ced_cuenta    ,
            cve_ced_cuenta    LIKE safre_af:acr_det_cedido.cve_ced_cuenta
    END RECORD  

     DEFINE enter             CHAR(001) ,
            v_ident_operacion CHAR(002)

     DEFINE v_tipo_movimiento ,
            v_folio           INTEGER	

     DEFINE v_id_aportante    CHAR(012),
	    v_folio_sua       CHAR(006)

     DEFINE v_fecha_pago      DATE

     DEFINE reg_input         RECORD 
            fecha_trasp       DATE
     END RECORD

     DEFINE reg_cza_hist_trab RECORD LIKE safre_tmp:cza_hist_trab.*

     DEFINE reg_det_mov_trab  RECORD LIKE safre_tmp:det_mov_trab.*
     DEFINE reg_beta  RECORD
    tipo_movimiento integer     ,
    subcuenta integer        ,
    siefore integer          ,
    folio integer  ,
    consecutivo_lote integer,
    nss char(11)  ,
    curp char(18),
    folio_sua char(6),
    fecha_pago date,
    fecha_valor date,
    fecha_conversion date,
    monto_en_pesos decimal(22,6),
    monto_en_acciones decimal(22,6),
    precio_accion decimal(22,6),
    dias_cotizados integer,
    sucursal char(10),
    id_aportante char(11),
    estado integer,
    fecha_proceso date,
    usuario char(8),
    fecha_archivo date,
    etiqueta integer
   END RECORD

     DEFINE reg_cuentas       RECORD LIKE safre_af:dis_cuenta.*
     DEFINE reg_taa_det_recep_ps   RECORD LIKE taa_det_recep_ps.*
END GLOBALS

MAIN
CALL STARTLOG ("HISB020.log")

WHENEVER ERROR CONTINUE

SELECT a.codigo_afore 
INTO   s_cve_afore
FROM   safre_af:tab_afore_local a

LET c_cve_afore = s_cve_afore USING"&&&"

LET vv_fecha_ini = ARG_VAL(1)
LET vv_fecha_fin = ARG_VAL(2)

DROP TABLE taa_cd_max_fechatrasp

CREATE TEMP TABLE taa_cd_max_fechatrasp
(
fecha_trasp date
)

DROP TABLE taa_his_comis

CREATE TEMP TABLE taa_his_comis
  (
    tipo_movimiento integer,
    subcuenta integer ,
    siefore smallint,
    folio integer  ,
    consecutivo_lote integer,
    nss char(11)  ,
    curp char(18),
    folio_sua char(6),
    fecha_pago date,
    fecha_valor date,
    fecha_conversion date,
    monto_en_pesos decimal(22,6),
    monto_en_acciones decimal(22,6),
    precio_accion decimal(22,6),
    dias_cotizados integer,
    sucursal char(10),
    id_aportante char(11),
    estado integer,
    fecha_proceso date,
    usuario char(8),
    fecha_archivo date,
    etiqueta integer
  )

CREATE TEMP TABLE e_fecha (fecha DATE)

     DECLARE cur_1 CURSOR FOR
     SELECT *        
     FROM   safre_tmp:cza_hist_trab
     ORDER BY cont_serv

   FOREACH cur_1 INTO reg_cza_hist_trab.* 
                
    LET reg_det_mov_trab.consecutivo = 0

    LET vv_fecha_traspaso = MDY(reg_cza_hist_trab.fecha_trasp[5,6],
 			    reg_cza_hist_trab.fecha_trasp[7,8],
			    reg_cza_hist_trab.fecha_trasp[1,4])

    DELETE FROM taa_cd_max_fechatrasp

          SELECT  A.fentcons
          INTO    v_fecha_apertura
          FROM    safre_af:afi_mae_afiliado    A         
          WHERE   A.n_seguro = reg_cza_hist_trab.nss 

	  INSERT INTO taa_cd_max_fechatrasp						 
          SELECT max(A.fecha_envio_saldos)
          FROM   taa_cd_ctr_folio  A ,
                 taa_cd_det_cedido B
          WHERE  A.folio = B.folio
          AND    B.n_seguro = reg_cza_hist_trab.nss  
          AND    A.estado  in (103,12,99)

          INSERT INTO taa_cd_max_fechatrasp 
          SELECT max(a.fecha_envio_saldos)
          FROM   taa_cd_ctr_folio a ,
                 taa_cd_det_comple b
          WHERE  a.folio = b.folio  
          AND    b.n_seguro = reg_cza_hist_trab.nss
          AND    b.estado   = 103
          AND    b.fecha_liquida < vv_fecha_traspaso

          SELECT max(a.fecha_trasp)
          INTO   v_fecha_traspaso
          FROM   taa_cd_max_fechatrasp a

          SELECT "ok"
          FROM safre_af:dis_cuenta A
          WHERE  A.nss =  reg_cza_hist_trab.nss
          AND    A.tipo_movimiento = 230
          GROUP BY 1

          IF status = NOTFOUND THEN 
             LET xx = "02"      
          ELSE 
             LET xx = "01"
          END IF

    DECLARE cur_3 CURSOR FOR

    SELECT A.*
    FROM   safre_af:dis_cuenta A
    WHERE  A.nss     = reg_cza_hist_trab.nss
    ORDER BY 11

    FOREACH cur_3 INTO  reg_beta.*

    --IF reg_beta.fecha_conversion <= v_fecha_traspaso THEN
    IF reg_beta.fecha_conversion < v_fecha_traspaso THEN
       CONTINUE FOREACH
    END IF
    IF reg_beta.fecha_conversion >= vv_fecha_traspaso THEN
       CONTINUE FOREACH
    END IF




    SELECT "OK" 
    FROM   safre_af:taa_cd_tipo_traspaso a
    WHERE  a.marca_cod = reg_beta.tipo_movimiento

    IF STATUS <> NOTFOUND THEN 
       CONTINUE FOREACH
    END IF

    SELECT a.razon_social
    INTO vv_siefore 
    FROM tab_siefore_local a
    WHERE a.codigo_siefore = reg_beta.siefore

                  CASE reg_beta.tipo_movimiento 

                  WHEN  1

                     IF LENGTH(reg_beta.id_aportante) = 11 THEN  ####dis####

                        CALL tipo_mov1dis() 

                     ELSE    #### else (tra) ####

                        LET v_id[1,3] = reg_beta.id_aportante[1,3]

                        CASE v_id     #### traspasos tipo mov 1  #####

                        WHEN "AF-" #### afore afore (receptora   ####
                             CALL tipo_mov1AF()
                             EXIT CASE 
                        WHEN "AC-" #### afore afore (receptora   ####
                             CALL tipo_mov1AF()
                             EXIT CASE 
                        WHEN "UR-" #### afore afore (receptora   ####
                             CALL tipo_mov1AF()
                             EXIT CASE 
                        WHEN "US-" #### afore afore (receptora   ####
                             CALL tipo_mov1AF()
                             EXIT CASE 
                        WHEN "AA-" #### afore afore (receptora   ####
                             CALL tipo_mov1AF()
                             EXIT CASE 
                        WHEN "AS-" #### afore afore (receptora   ####
                             CALL tipo_mov1AF()
                             EXIT CASE 
                        WHEN "UY-" #### afore afore (receptora   ####
                             CALL tipo_mov1AF()
                             EXIT CASE 
                        WHEN "UZ-" #### afore afore (receptora   ####
                             CALL tipo_mov1AF()
                             EXIT CASE 

                        WHEN "MQ-" #### ps afore como (afore) ####
                             CALL tipo_mov1MQ()
                             EXIT CASE
                        WHEN "MC-" #### ps afore como (afore) ####
                             CALL tipo_mov1MQ()
                             EXIT CASE
                        WHEN "UM-" #### ps afore como (afore) ####
                             CALL tipo_mov1MQ()
                             EXIT CASE

                        WHEN "UQ-" #### ps afore como (afore) ####
                             CALL tipo_mov1MQ()
                             EXIT CASE

                        WHEN "MA-" #### ps afore como (afore) ####
                             CALL tipo_mov1MQ()
                             EXIT CASE
                        WHEN "MS-" #### ps afore como (afore) ####
                             CALL tipo_mov1MQ()
                             EXIT CASE
                        WHEN "TI-"
                             CALL tipo_mov1IC()
			     EXIT CASE
                        WHEN "VE-"
			     CALL tipo_mov1VEN()
			     EXIT CASE
                        WHEN "UN-"
			     CALL tipo_unif()
			     EXIT CASE
                        WHEN "UC-"
			     CALL tipo_unif()
			     EXIT CASE
                        OTHERWISE
			     EXIT CASE
                        END CASE  ##### traspasos tipo mov 1 ######

                     END IF
                     IF LENGTH(reg_beta.id_aportante) = 7 THEN  ####dis####
			IF reg_beta.id_aportante = 'DEV-INF' THEN
                           CALL tipo_movSDX() 
                        END IF
                     END IF
                     IF LENGTH(reg_beta.id_aportante) = 9 THEN  ####dis####
			IF reg_beta.id_aportante = 'DEV. INF.' THEN
                           CALL tipo_movSDX() 
                        END IF
                     END IF
                     EXIT CASE

                  WHEN  2
                        CALL tipo_mov1dis() 
                        EXIT CASE

                  WHEN  3 CASE 
                   reg_beta.id_aportante
                     WHEN "BANXICO"
                        CALL tipo_mov3rcv() 
		        EXIT CASE

                     WHEN "INFONAVIT"  
		        CALL tipo_mov3viv() 
                        EXIT CASE 

                     WHEN "REMANENTE"  
                        CALL tipo_mov3viv()
                        EXIT CASE

                     OTHERWISE

                       IF LENGTH(reg_beta.id_aportante)=11 THEN
                          CALL tipo_mov1dis()
                       ELSE 
                         CASE reg_beta.id_aportante[1,3] 
                         WHEN "MQ-" 
                           CALL tipo_mov1MQ()
                           EXIT CASE
                         OTHERWISE
                           CALL tipo_mov1IC()
                           EXIT CASE
                         END CASE
                        END IF
                       EXIT CASE
                      END CASE
                  EXIT CASE

                  WHEN  4
          
                     IF reg_beta.id_aportante = "REMANENTE" THEN
			CALL tipo_mov3viv()
			EXIT CASE
		     END IF

                     IF reg_beta.id_aportante = "BANXICO" THEN

                        CALL tipo_mov4dis()
			EXIT CASE

                     ELSE 

		        LET v_id = reg_beta.id_aportante[1,3]

                        CASE v_id 

		        WHEN "MA-" 
                             CALL tipo_mov1MQ()
                              EXIT CASE
		        WHEN "MC-" 
                             CALL tipo_mov1MQ()
                              EXIT CASE
		        WHEN "MQ-" 
                             CALL tipo_mov1MQ()
                              EXIT CASE
		        WHEN "MS-" 
                             CALL tipo_mov1MQ()
                              EXIT CASE
		        WHEN "TI-"
                              CALL tipo_mov1IC()
                              EXIT CASE
                        WHEN "UN-"
			     CALL tipo_unif()
			     EXIT CASE
                        WHEN "UC-"
			     CALL tipo_unif()
			     EXIT CASE
			OTHERWISE
			      EXIT CASE
		        END CASE

		     END IF

			IF reg_beta.id_aportante = 'DEV-INF' THEN
                           CALL tipo_movSDX() 
                        END IF
			IF reg_beta.id_aportante = 'DEV. INF.' THEN
                           CALL tipo_movSDX() 
                        END IF

                  EXIT CASE

                  WHEN  5
                       IF LENGTH(reg_beta.id_aportante) = 11 THEN
                          CALL tipo_mov1dis()
                       ELSE
                        CALL tipo_mov1IC()
                       END IF
                        EXIT CASE

                  WHEN  220
                     #CALL tipo_mov1AF() 
                  EXIT CASE

                  WHEN  230
                     CALL tipo_mov1AF()
                  EXIT CASE

                  WHEN  235
                     CALL tipo_mov1AF()
                  EXIT CASE

                  WHEN  270
                     CALL tipo_mov1AF()
                  EXIT CASE
                  WHEN  290
                    # CALL tipo_mov1AF()
                  EXIT CASE
                  WHEN  520
                    # CALL tipo_mov1AF()
                  EXIT CASE

                  WHEN 540
		    CALL tipo_movDEV_EX()
		    EXIT CASE
                  WHEN 545
		    CALL tipo_movDEV_EX()
		    EXIT CASE
                  WHEN 550
		    CALL tipo_movDEV_EX()
		    EXIT CASE
                  WHEN 555
		    CALL tipo_movDEV_EX()
		    EXIT CASE
		  WHEN 630
		    CALL ajuste()
		    EXIT CASE
		  WHEN 640
		    CALL ajuste()
		    EXIT CASE
		  OTHERWISE

                     IF (reg_beta.tipo_movimiento >=  401 AND
                         reg_beta.tipo_movimiento <= 412 ) THEN
                         CALL tipo_mov400IVRT()
                     END IF
                     IF (reg_beta.tipo_movimiento >=  457 AND
                         reg_beta.tipo_movimiento <= 487 ) THEN
                         IF (reg_beta.tipo_movimiento =  486 OR
                             reg_beta.tipo_movimiento = 487 ) THEN
                             CALL tipo_mov400PARCIAL() 
                         ELSE
                             CALL tipo_mov400TOTAL()
                         END IF
                     END IF
                     IF (reg_beta.tipo_movimiento >=  415 AND
                         reg_beta.tipo_movimiento <= 454 ) THEN
                         CALL tipo_mov400SAR92()
                     END IF
                     IF reg_beta.tipo_movimiento =  490 THEN
                         CALL tipo_mov400VOL()
                     END IF

		  EXIT CASE

                  END CASE

          END FOREACH    ####### END FOREACH TIPO MOVIMIENTOS ##########

       END FOREACH      ####### END FOREACH TRABAJADORES     ##########

LET lanza = "nohup fglgo HISL010 ",vv_fecha_ini CLIPPED," ",
	    vv_fecha_fin CLIPPED, " &"
RUN lanza


END MAIN


FUNCTION  tipo_mov1dis()
#idmDIS-------------------------


     DEFINE v_paso_fecha DATE

    DEFINE reg_cur1_tipo_mov1dis RECORD
            fecha_pago            LIKE safre_af:dis_cuenta.fecha_pago      ,
            fecha_valor           LIKE safre_af:dis_cuenta.fecha_valor     ,
            folio_sua             LIKE safre_af:dis_cuenta.folio_sua       ,
            fecha_conversion      LIKE safre_af:dis_cuenta.fecha_conversion
     END RECORD

      DEFINE reg_cur2_tipo_mov1dis RECORD 
	     fech_valor_rcv     LIKE dis_det_aporte.fech_valor_rcv    ,
	     fech_valor_viv     LIKE dis_det_aporte.fech_valor_viv    ,
	     fech_pago          LIKE dis_det_aporte.fech_pago         ,
	     periodo_pago       LIKE dis_det_aporte.periodo_pago      ,
	     folio_pago_sua     LIKE dis_det_aporte.folio_pago_sua    ,
	     cve_ent_receptora  LIKE dis_det_aporte.cve_ent_receptora ,
	     reg_patronal_imss  LIKE dis_det_aporte.reg_patronal_imss ,
	     dias_cotz_bimestre LIKE dis_det_aporte.dias_cotz_bimestre,
             dias_incap_bimest  LIKE dis_det_aporte.dias_incap_bimest ,
             dias_ausent_bimest LIKE dis_det_aporte.dias_ausent_bimest,
             fecha_estado       LIKE dis_det_aporte.fecha_estado,
             rfc_patron         LIKE dis_det_aporte.rfc_patron  , 
             ult_salario_diario LIKE dis_det_aporte.ult_salario_diario
     END RECORD

     DEFINE reg_cur3_tipo_mov1dis  RECORD 
            subcuenta         SMALLINT      ,
            fecha_conversion  DATE          ,
            monto_en_pesos    DECIMAL(13,2) ,
            monto_en_acciones DECIMAL(13,2) ,
            precio_accion     LIKE safre_af:dis_cuenta.precio_accion
     END RECORD

     DEFINE v_fecha_pago          CHAR(008),
            v_fecha_valor         CHAR(008)

          LET reg_det_mov_trab.iden_mov = " "

          LET v_fecha_pago  = reg_cur1_tipo_mov1dis.fecha_pago  USING"YYYYMMDD"
          LET v_fecha_valor = reg_cur1_tipo_mov1dis.fecha_valor USING"YYYYMMDD"

          DECLARE cur_mov1 CURSOR FOR

          SELECT A.fech_valor_rcv        ,
	         A.fech_valor_viv        ,
	         A.fech_pago             ,
	         A.periodo_pago          ,
	         A.folio_pago_sua        ,
	         A.cve_ent_receptora     ,
	         A.reg_patronal_imss     ,
	         A.dias_cotz_bimestre    ,
                 A.dias_incap_bimest     ,
                 A.dias_ausent_bimest    ,
                 A.fecha_estado          ,
                 A.rfc_patron            ,
                 A.ult_salario_diario 
          FROM   dis_det_aporte A 
          WHERE  A.folio             = reg_beta.folio 
          AND    A.n_seguro          = reg_cza_hist_trab.nss
	  AND    A.folio_pago_sua    = reg_beta.folio_sua
          AND    A.reg_patronal_imss = reg_beta.id_aportante
	  --AND    A.fech_pago         = v_fecha_pago
	  --AND    A.fech_valor_rcv    = v_fecha_valor
          --AND    A.dias_cotz_bimestre = reg_beta.dias_cotizados

          INITIALIZE reg_cur2_tipo_mov1dis.* TO NULL

          FOREACH cur_mov1 INTO reg_cur2_tipo_mov1dis.*
          END FOREACH


               LET reg_det_mov_trab.cont_serv     = 
                   reg_cza_hist_trab.cont_serv

               LET reg_det_mov_trab.curp = 
		   reg_cza_hist_trab.n_unico

               LET reg_det_mov_trab.nss_afore = 
		   reg_cza_hist_trab.nss

               LET reg_det_mov_trab.iden_proc = "01"

               LET reg_det_mov_trab.consecutivo   =
                   reg_det_mov_trab.consecutivo + 1
                    
               LET reg_det_mov_trab.iden_mov[1,8]   = 
                   reg_cur2_tipo_mov1dis.fech_valor_rcv
 
               LET reg_det_mov_trab.iden_mov[9,16]  = 
                   reg_beta.fecha_conversion USING"YYYYMMDD"

               LET reg_det_mov_trab.iden_mov[17,24]   = 
                   reg_cur2_tipo_mov1dis.fech_valor_viv

               LET reg_det_mov_trab.iden_mov[25,32]  = 
                   reg_cur2_tipo_mov1dis.fech_pago

               LET reg_det_mov_trab.iden_mov[33,38] =
                   reg_cur2_tipo_mov1dis.periodo_pago
 
               LET reg_det_mov_trab.iden_mov[39,44] = 
                   reg_cur2_tipo_mov1dis.folio_pago_sua USING"&&&&&&"

               LET reg_det_mov_trab.iden_mov[45,47] = 
                   reg_cur2_tipo_mov1dis.cve_ent_receptora USING"&&&"

               LET reg_det_mov_trab.iden_mov[48,58] = 
                   reg_cur2_tipo_mov1dis.reg_patronal_imss

               LET reg_det_mov_trab.iden_mov[59,71] = 
                   reg_cur2_tipo_mov1dis.rfc_patron

               LET reg_det_mov_trab.iden_mov[72,73] = 
                   reg_cur2_tipo_mov1dis.dias_cotz_bimestre USING"&&"

               LET reg_det_mov_trab.iden_mov[74,75] = 
                   reg_cur2_tipo_mov1dis.dias_incap_bimest USING"&&"

               LET reg_det_mov_trab.iden_mov[76,77] = 
                   reg_cur2_tipo_mov1dis.dias_ausent_bimest USING"&&"

               LET reg_det_mov_trab.iden_mov[78,85] =
                   reg_cur2_tipo_mov1dis.fecha_estado USING "YYYYMMDD"

               LET reg_det_mov_trab.siefore =  vv_siefore

               LET reg_det_mov_trab.fecha_valuacion = 
                   reg_beta.fecha_conversion USING "YYYYMMDD"

               LET reg_det_mov_trab.acciones = 
                   reg_beta.monto_en_acciones
#prompt reg_beta.precio_accion for char enter
               LET reg_det_mov_trab.precio_accion = 
                   reg_beta.precio_accion
#prompt reg_det_mov_trab.precio_accion for char enter

              LET v_cuantos = 0

              SELECT count(*) 
              INTO   v_cuantos
              FROM   safre_af:dis_dep_aporte A
              WHERE  A.folio = reg_beta.folio
   
              IF v_cuantos = 5 THEN
               LET reg_det_mov_trab.iden_mov[86,87] = "02"
              END IF
              IF v_cuantos = 6 THEN
               LET reg_det_mov_trab.iden_mov[86,87] = "02"
              END IF
              IF v_cuantos = 10 THEN 
               LET reg_det_mov_trab.iden_mov[86,87] = "01"
              END IF 
              IF v_cuantos = 12 THEN 
               LET reg_det_mov_trab.iden_mov[86,87] = "01"
              END IF 

              LET reg_det_mov_trab.iden_mov[88,94] = 
                  reg_cur2_tipo_mov1dis.ult_salario_diario 
                  USING"&&&&&&&"


             LET v_comis = 0

LET v_conver = YEAR(reg_beta.fecha_conversion) USING"&&&&"



DELETE FROM taa_his_comis

              INSERT INTO taa_his_comis
              SELECT A.*
              FROM   safre_af:dis_cuenta A
              WHERE  A.nss              = reg_beta.nss
             
              IF v_comis IS NULL THEN
                 LET v_comis = 0
              END IF

              LET v_comis = 0

              SELECT A.monto_en_pesos
              INTO   v_comis
              FROM   taa_his_comis A
              WHERE  A.nss              = reg_beta.nss
              AND    A.subcuenta        = reg_beta.subcuenta
              AND    A.tipo_movimiento  in (100,101,102,103,104,105,106,107,
                                            108,109)
              AND    A.fecha_conversion = reg_beta.fecha_conversion
              AND    A.folio            = reg_beta.folio
              AND    A.folio_sua        = reg_beta.folio_sua

              IF v_comis IS NULL THEN
                 LET v_comis = 0
              END IF

              LET v_comis = v_comis * 100

              LET reg_det_mov_trab.iden_mov[95,108] = 
              v_comis  USING"&&&&&&&&&&&&&&"

              IF reg_beta.monto_en_pesos < 0 THEN
                 LET reg_det_mov_trab.tipo_mov = "C" 
              ELSE 
                 LET reg_det_mov_trab.tipo_mov = "A"
              END IF

              CASE reg_beta.subcuenta

               WHEN 1
                  CASE reg_beta.tipo_movimiento 
                   WHEN 1
                  LET reg_det_mov_trab.subcuenta = "01" EXIT CASE
                   WHEN 2
                  LET reg_det_mov_trab.subcuenta = "25" EXIT CASE
                   WHEN 3
                  LET reg_det_mov_trab.subcuenta = "18" EXIT CASE
                   OTHERWISE
                  LET reg_det_mov_trab.subcuenta = "01" EXIT CASE
                  END CASE
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 2 

                  CASE reg_beta.tipo_movimiento 
                   WHEN 1
                  LET reg_det_mov_trab.subcuenta = "02" EXIT CASE
                   WHEN 2
                  LET reg_det_mov_trab.subcuenta = "27" EXIT CASE
                   WHEN 3
                  LET reg_det_mov_trab.subcuenta = "20" EXIT CASE
                   OTHERWISE
                  LET reg_det_mov_trab.subcuenta = "27" EXIT CASE
                  END CASE
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 3 

                  LET reg_det_mov_trab.subcuenta = "13"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 4 

                  LET reg_det_mov_trab.subcuenta = "04"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 5 
                 CASE reg_beta.tipo_movimiento 
                 WHEN 3
                  LET reg_det_mov_trab.subcuenta = "19"
                  EXIT CASE
                 OTHERWISE
                  LET reg_det_mov_trab.subcuenta = "03"
                  EXIT CASE
                 END CASE

                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 6 

                  LET reg_det_mov_trab.subcuenta = "06"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 9 

                  LET reg_det_mov_trab.subcuenta = "07"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos  
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

                OTHERWISE 
                           EXIT CASE
                END CASE
{
        ELSE 

              CASE reg_beta.subcuenta

               WHEN 1
                  LET reg_det_mov_trab.subcuenta = "25"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 2 

                  LET reg_det_mov_trab.subcuenta = "27"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 5 

                  LET reg_det_mov_trab.subcuenta = "29"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 6 

                  LET reg_det_mov_trab.subcuenta = "31"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 9 

                  LET reg_det_mov_trab.subcuenta = "33"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos  
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

                OTHERWISE 
                           EXIT CASE
                END CASE

        END IF 
}

END FUNCTION

FUNCTION tipo_mov1MQ() 
#tm1MQ------------------
          DEFINE ps_apor   CHAR(003)

          LET ps_apor  = reg_beta.id_aportante[4,6]

	  LET reg_det_mov_trab.iden_mov = " "


          LET reg_det_mov_trab.cont_serv     = 
              reg_cza_hist_trab.cont_serv


          LET reg_det_mov_trab.curp = 
	      reg_cza_hist_trab.n_unico

          LET reg_det_mov_trab.nss_afore = 
	      reg_cza_hist_trab.nss


          LET reg_det_mov_trab.iden_proc = "03"

                      LET reg_det_mov_trab.consecutivo   =
                          reg_det_mov_trab.consecutivo + 1


                      LET reg_det_mov_trab.iden_mov[1,8]   = 
                          reg_beta.fecha_conversion USING"YYYYMMDD"
                  

                    CASE reg_beta.id_aportante[1,2] 
                     WHEN "MQ" 
                        LET reg_det_mov_trab.iden_mov[9,10] = "12"
                        LET reg_det_mov_trab.iden_mov[11,12]= "01"
                        LET reg_det_mov_trab.iden_mov[13,15]= c_cve_afore 
                        LET reg_det_mov_trab.iden_mov[16,17]= "08"
                        EXIT CASE
                      WHEN "MC"
                        LET reg_det_mov_trab.iden_mov[9,10] = "12"
                        LET reg_det_mov_trab.iden_mov[11,12]= "01"
                        LET reg_det_mov_trab.iden_mov[13,15]= c_cve_afore
                        LET reg_det_mov_trab.iden_mov[16,17]= "08"
                        EXIT CASE
                      WHEN "MA" 
                        LET reg_det_mov_trab.iden_mov[9,10] = "15"
                        LET reg_det_mov_trab.iden_mov[11,12]= "01"
                        LET reg_det_mov_trab.iden_mov[13,15]= c_cve_afore
                        LET reg_det_mov_trab.iden_mov[16,17]= "08"
                        EXIT CASE
                      WHEN "MS" 
                        LET reg_det_mov_trab.iden_mov[9,10] = "15"
                        LET reg_det_mov_trab.iden_mov[11,12]= "01"
                        LET reg_det_mov_trab.iden_mov[13,15]= c_cve_afore
                        LET reg_det_mov_trab.iden_mov[16,17]= "08"
                        EXIT CASE
                      WHEN "UN"
                        LET reg_det_mov_trab.iden_mov[9,10] = "14"
                        LET reg_det_mov_trab.iden_mov[11,12]= "01"
                        LET reg_det_mov_trab.iden_mov[13,15]= c_cve_afore
                        LET reg_det_mov_trab.iden_mov[16,17]= "08"
                        EXIT CASE
                      WHEN "UQ"
                        LET reg_det_mov_trab.iden_mov[9,10] = "14"
                        LET reg_det_mov_trab.iden_mov[11,12]= "01"
                        LET reg_det_mov_trab.iden_mov[13,15]= c_cve_afore
                        LET reg_det_mov_trab.iden_mov[16,17]= "08"
                        EXIT CASE
                      OTHERWISE
                        EXIT CASE
                      END CASE
 
                      LET reg_det_mov_trab.iden_mov[18,20] = 
                          reg_beta.id_aportante[4,6]
     

                      IF (reg_beta.id_aportante[1,2] = "MQ" OR 
                          reg_beta.id_aportante[1,2] = "UN" OR 
                          reg_beta.id_aportante[1,2] = "MA" )THEN 
                         LET reg_det_mov_trab.iden_mov[21,22] = "01"
                      ELSE
                         LET reg_det_mov_trab.iden_mov[21,22] = "02"
                      END IF
           
                      IF reg_beta.monto_en_pesos >= 0 THEN 
                         LET reg_det_mov_trab.tipo_mov = "A"
                      ELSE 
                         LET reg_det_mov_trab.tipo_mov = "C"
                      END IF

                      IF reg_beta.id_aportante[1,3] = "MA-" OR
			 reg_beta.id_aportante[1,3] = "MS-" OR
			 reg_beta.id_aportante[1,3] = "MQ-" OR
			 reg_beta.id_aportante[1,3] = "MC-" THEN

                         SELECT sum(dias_pag_cuo_soc) ,
			 	sum(no_apor_viv) 
			 INTO aa1 ,
			      aa2 
			 FROM taa_det_recep_ps
			 WHERE nss_afo_recep = reg_beta.nss

                       LET reg_det_mov_trab.iden_mov[23,27] = aa1 USING"&&&&&"
                       LET reg_det_mov_trab.iden_mov[28,32] = aa2 USING"&&&&&"

	              END IF

                      LET reg_det_mov_trab.siefore =  vv_siefore
                         
                      LET reg_det_mov_trab.fecha_valuacion  = 
                      reg_beta.fecha_conversion USING"YYYYMMDD"

                      LET reg_det_mov_trab.acciones = 
                          reg_beta.monto_en_acciones

                      LET reg_det_mov_trab.precio_accion = 
                          reg_beta.precio_accion

                      LET reg_det_mov_trab.fecha_proceso = 
                          TODAY 

              CASE reg_beta.subcuenta

               WHEN 1
                  IF reg_beta.tipo_movimiento = 4 THEN
                     LET reg_det_mov_trab.subcuenta = "18"
                  ELSE 
                     LET reg_det_mov_trab.subcuenta = "01"
                  END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                  LET reg_det_mov_trab.fecha_conversion = 
                      reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 2 
                  IF reg_beta.tipo_movimiento = 4 THEN
                     LET reg_det_mov_trab.subcuenta = "20"
                  ELSE 
                     LET reg_det_mov_trab.subcuenta = "02"
                  END IF
                 
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 3 
                 IF reg_beta.tipo_movimiento = 4 THEN
                  LET reg_det_mov_trab.subcuenta = "23"
                 ELSE 
                  LET reg_det_mov_trab.subcuenta = "13"
                 END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 4 
                  IF reg_beta.tipo_movimiento = 4 THEN
                      LET reg_det_mov_trab.subcuenta = "12"
                  ELSE 
                      LET reg_det_mov_trab.subcuenta = "04"
                  END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 5 
                  IF reg_beta.tipo_movimiento = 4 THEN
                     LET reg_det_mov_trab.subcuenta = "19"
                  ELSE 
                     LET reg_det_mov_trab.subcuenta = "03"
                  END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 7 

                  LET reg_det_mov_trab.subcuenta = "08"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE


               WHEN 8 

                  LET reg_det_mov_trab.subcuenta = "09"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

                OTHERWISE 
                  EXIT CASE
             END CASE

END FUNCTION

FUNCTION tipo_mov1AF()
#tm1AF--------------- 
DEFINE mov_tipo_trasp SMALLINT
DEFINE vorig CHAR(002)

               LET reg_det_mov_trab.iden_mov = " "

               LET reg_det_mov_trab.cont_serv     = 
                   reg_cza_hist_trab.cont_serv


               LET reg_det_mov_trab.curp = 
		   reg_cza_hist_trab.n_unico

               LET reg_det_mov_trab.nss_afore = 
		   reg_cza_hist_trab.nss

               LET reg_det_mov_trab.iden_proc = "03"

               LET reg_det_mov_trab.consecutivo   =
                   reg_det_mov_trab.consecutivo + 1


               LET reg_det_mov_trab.iden_mov[1,8]   = 
                   reg_beta.fecha_conversion USING"YYYYMMDD"
                 
                      CASE reg_beta.id_aportante[1,3]
                      WHEN "AF-"
                        SELECT a.tipo_traspaso
			INTO   mov_tipo_trasp 
			FROM   safre_af:taa_rcv_recepcion a
			WHERE  nss_afo_recep = reg_beta.nss  
			GROUP BY 1

			CASE mov_tipo_trasp
                        WHEN 51 
                          LET reg_det_mov_trab.iden_mov[9,10]  = "11"
                          LET reg_det_mov_trab.iden_mov[11,12] = "01"
                          LET reg_det_mov_trab.iden_mov[13,15] = c_cve_afore
                          LET reg_det_mov_trab.iden_mov[16,17] = "01"
                          LET reg_det_mov_trab.iden_mov[18,20] = 
                              reg_beta.id_aportante[4,6]
			      EXIT CASE
                        WHEN 12 
                          LET reg_det_mov_trab.iden_mov[9,10]  = "10"
                          LET reg_det_mov_trab.iden_mov[11,12] = "01"
                          LET reg_det_mov_trab.iden_mov[13,15] = c_cve_afore
                          LET reg_det_mov_trab.iden_mov[16,17] = "01"
                          LET reg_det_mov_trab.iden_mov[18,20] = 
                              reg_beta.id_aportante[4,6]
                             EXIT CASE
                        OTHERWISE 
                          LET reg_det_mov_trab.iden_mov[9,10]  = "06"
                          LET reg_det_mov_trab.iden_mov[11,12] = "01"
                          LET reg_det_mov_trab.iden_mov[13,15] = c_cve_afore
                          LET reg_det_mov_trab.iden_mov[16,17] = "01"
                          LET reg_det_mov_trab.iden_mov[18,20] = 
                              reg_beta.id_aportante[4,6]
                             EXIT CASE
                          END CASE 
		         EXIT CASE
                      WHEN "AC-" 
                          LET reg_det_mov_trab.iden_mov[9,10]  = "06"
                          LET reg_det_mov_trab.iden_mov[11,12] = "01"
                          LET reg_det_mov_trab.iden_mov[13,15] = c_cve_afore
                          LET reg_det_mov_trab.iden_mov[16,17] = "01"
                          LET reg_det_mov_trab.iden_mov[18,20] = 
                              reg_beta.id_aportante[4,6]
                          EXIT CASE
                      WHEN "UR-" 
                          LET reg_det_mov_trab.iden_mov[9,10]  = "10"
                          LET reg_det_mov_trab.iden_mov[11,12] = "01"
                          LET reg_det_mov_trab.iden_mov[13,15] = c_cve_afore
                          LET reg_det_mov_trab.iden_mov[16,17] = "01"
                          LET reg_det_mov_trab.iden_mov[18,20] = 
                              reg_beta.id_aportante[4,6]
                          EXIT CASE
                      WHEN "US-" 
                          LET reg_det_mov_trab.iden_mov[9,10]  = "10"
                          LET reg_det_mov_trab.iden_mov[11,12] = "01"
                          LET reg_det_mov_trab.iden_mov[13,15] = c_cve_afore
                          LET reg_det_mov_trab.iden_mov[16,17] = "01"
                          LET reg_det_mov_trab.iden_mov[18,20] = 
                              reg_beta.id_aportante[4,6]
                          EXIT CASE
                      WHEN "AA-" 
                          LET reg_det_mov_trab.iden_mov[9,10]  = "11"
                          LET reg_det_mov_trab.iden_mov[11,12] = "01"
                          LET reg_det_mov_trab.iden_mov[13,15] = c_cve_afore
                          LET reg_det_mov_trab.iden_mov[16,17] = "01"
                          LET reg_det_mov_trab.iden_mov[18,20] = 
                              reg_beta.id_aportante[4,6]
                          EXIT CASE
                      WHEN "AS-" 
                          LET reg_det_mov_trab.iden_mov[9,10]  = "11"
                          LET reg_det_mov_trab.iden_mov[11,12] = "01"
                          LET reg_det_mov_trab.iden_mov[13,15] = c_cve_afore
                          LET reg_det_mov_trab.iden_mov[16,17] = "01"
                          LET reg_det_mov_trab.iden_mov[18,20] = 
                              reg_beta.id_aportante[4,6]
                          EXIT CASE
                      WHEN "TR-"
                          LET reg_det_mov_trab.iden_mov[9,10]  = "08"
                          LET reg_det_mov_trab.iden_mov[11,12] = "01"
                          LET reg_det_mov_trab.iden_mov[13,15] = 
                              reg_beta.id_aportante[4,6]
                          LET reg_det_mov_trab.iden_mov[16,17] = "01"
                          LET reg_det_mov_trab.iden_mov[18,20] = c_cve_afore
                          EXIT CASE
                      WHEN "TC-" 
                          LET reg_det_mov_trab.iden_mov[9,10]  = "08"
                          LET reg_det_mov_trab.iden_mov[11,12] = "01"
                          LET reg_det_mov_trab.iden_mov[13,15] = 
                              reg_beta.id_aportante[4,6]
                          LET reg_det_mov_trab.iden_mov[16,17] = "01"
                          LET reg_det_mov_trab.iden_mov[18,20]  = c_cve_afore
                          EXIT CASE
                      WHEN "ACR" 
                          LET reg_det_mov_trab.iden_mov[9,10]  = "05"
                          LET reg_det_mov_trab.iden_mov[11,12] = "04"
                          LET reg_det_mov_trab.iden_mov[13,15] = "002"
                          LET reg_det_mov_trab.iden_mov[16,17] = "01"
                          LET reg_det_mov_trab.iden_mov[18,20]  = c_cve_afore
                          EXIT CASE
                      WHEN "UT-"
                          LET reg_det_mov_trab.iden_mov[9,10]  = "10"
                          LET reg_det_mov_trab.iden_mov[11,12] = "01"
                          LET reg_det_mov_trab.iden_mov[13,15] = 
                              reg_beta.id_aportante[4,6]
                          LET reg_det_mov_trab.iden_mov[16,17] = "01"
                          LET reg_det_mov_trab.iden_mov[18,20] = c_cve_afore
                          EXIT CASE
                      WHEN "UC-"
                          LET reg_det_mov_trab.iden_mov[9,10]  = "10"
                          LET reg_det_mov_trab.iden_mov[11,12] = "01"
                          LET reg_det_mov_trab.iden_mov[13,15] = 
                              reg_beta.id_aportante[4,6]
                          LET reg_det_mov_trab.iden_mov[16,17] = "01"
                          LET reg_det_mov_trab.iden_mov[18,20] = c_cve_afore
                          EXIT CASE
                      WHEN "DI-"
                          LET reg_det_mov_trab.iden_mov[9,10]  = "02"
                          LET reg_det_mov_trab.iden_mov[11,12] = "01"
                          LET reg_det_mov_trab.iden_mov[13,15] = c_cve_afore
                          LET reg_det_mov_trab.iden_mov[16,17] = "07"
                          LET reg_det_mov_trab.iden_mov[18,20] = 
                              reg_beta.id_aportante[4,6]
                          EXIT CASE
                      WHEN "DC-"
                          LET reg_det_mov_trab.iden_mov[9,10]  = "02"
                          LET reg_det_mov_trab.iden_mov[11,12] = "01"
                          LET reg_det_mov_trab.iden_mov[13,15] = c_cve_afore
                          LET reg_det_mov_trab.iden_mov[16,17] = "07"
                          LET reg_det_mov_trab.iden_mov[18,20] = 
                              reg_beta.id_aportante[4,6]
                          EXIT CASE
                      WHEN "UY-"
                          LET reg_det_mov_trab.iden_mov[9,10]  = "10"
                          LET reg_det_mov_trab.iden_mov[11,12] = "01"
                          LET reg_det_mov_trab.iden_mov[13,15] = c_cve_afore
                          LET reg_det_mov_trab.iden_mov[16,17] = "01"
                          LET reg_det_mov_trab.iden_mov[18,20] = 
                              reg_beta.id_aportante[4,6]
                          EXIT CASE
                      WHEN "UZ-"
                          LET reg_det_mov_trab.iden_mov[9,10]  = "10"
                          LET reg_det_mov_trab.iden_mov[11,12] = "01"
                          LET reg_det_mov_trab.iden_mov[13,15] = c_cve_afore
                          LET reg_det_mov_trab.iden_mov[16,17] = "01"
                          LET reg_det_mov_trab.iden_mov[18,20] = 
                              reg_beta.id_aportante[4,6]
                          EXIT CASE

                      OTHERWISE 
                          EXIT CASE
                      END CASE

                      IF (reg_beta.id_aportante[1,2]= "AF" OR 
                          reg_beta.id_aportante[1,2]= "UR" OR
                          reg_beta.id_aportante[1,2]= "AA" OR
                          reg_beta.id_aportante[1,2]= "TR" OR
                          reg_beta.id_aportante[1,2]= "UT" OR 
			  reg_beta.id_aportante[1,2]= "UY" ) THEN
                         LET reg_det_mov_trab.iden_mov[21,22] = "01"
                      ELSE
                         LET reg_det_mov_trab.iden_mov[21,22] = "02"
                      END IF

                       IF reg_beta.id_aportante[1,3] = "ACR" THEN
                         LET reg_det_mov_trab.iden_mov[21,22] = "  "
                       END IF
                         
                      IF (reg_beta.id_aportante[1,3] = "DI-" ) THEN
			CASE reg_beta.subcuenta
			 WHEN 7

                         SELECT a.orig_tipo_trasp
                         INTO   vorig
                         FROM   dev_det_normal a
                         WHERE  a.folio_devol = reg_beta.folio
                         AND    a.n_seguro    = reg_beta.nss
                         AND    a.saldo_sar_92 = -(reg_beta.monto_en_pesos)
                         AND    a.status = 1

                         CASE  vorig
                         WHEN "08"
                            LET reg_det_mov_trab.iden_mov[21,22]="05"
                         EXIT CASE
                         WHEN "09"
                            LET reg_det_mov_trab.iden_mov[21,22]="04"
                         EXIT CASE
                         WHEN "06"
	                    LET reg_det_mov_trab.iden_mov[21,22]="06"
	                 EXIT CASE
	                 WHEN "07"
	                    LET reg_det_mov_trab.iden_mov[21,22]="07"
	                 EXIT CASE
	                 OTHERWISE
                            LET reg_det_mov_trab.iden_mov[21,22]="05"
			 EXIT CASE
                        END CASE
			 EXIT CASE

			 WHEN  8

                         SELECT a.orig_tipo_trasp
                         INTO   vorig
                         FROM   dev_det_normal a
                         WHERE  a.folio_devol = reg_beta.folio
                         AND    a.n_seguro    = reg_beta.nss
                         AND    a.saldo_viv_92 = -(reg_beta.monto_en_pesos)
                         AND    a.status = 1

                         CASE  vorig
                         WHEN "08"
                            LET reg_det_mov_trab.iden_mov[21,22]="05"
                         EXIT CASE
                         WHEN "09"
                            LET reg_det_mov_trab.iden_mov[21,22]="04"
                         EXIT CASE
                         WHEN "06"
	                    LET reg_det_mov_trab.iden_mov[21,22]="06"
	                 EXIT CASE
	                 WHEN "07"
	                    LET reg_det_mov_trab.iden_mov[21,22]="07"
	                 EXIT CASE
	                 OTHERWISE
                            LET reg_det_mov_trab.iden_mov[21,22]="05"
			 EXIT CASE
                        END CASE
                           EXIT CASE
                         OTHERWISE 
			   EXIT CASE
			   END CASE
	              END IF

                      IF (reg_beta.id_aportante[1,3] = "DC-") THEN
			 LET reg_det_mov_trab.iden_mov[21,22]= "06"
                      END IF


                      IF reg_beta.monto_en_pesos >= 0 THEN 
                         LET reg_det_mov_trab.tipo_mov = "A"
                      ELSE 
                         LET reg_det_mov_trab.tipo_mov = "C"
                      END IF

                      LET reg_det_mov_trab.siefore =  vv_siefore
         
                      LET reg_det_mov_trab.iden_mov[23,27] = "00000"
                      LET reg_det_mov_trab.iden_mov[28,32] = "00000"

                      LET reg_det_mov_trab.fecha_valuacion  = 
                      reg_beta.fecha_conversion USING"YYYYMMDD"

                      LET reg_det_mov_trab.acciones = 
                          reg_beta.monto_en_acciones

                      LET reg_det_mov_trab.precio_accion = 
                          reg_beta.precio_accion

                      LET reg_det_mov_trab.fecha_proceso = 
                          TODAY 

              CASE reg_beta.subcuenta

               WHEN 1
                  LET reg_det_mov_trab.subcuenta = "01"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                  LET reg_det_mov_trab.fecha_conversion = 
                      reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 2 

                  LET reg_det_mov_trab.subcuenta = "02"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 3 

                  LET reg_det_mov_trab.subcuenta = "13"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 4 
               IF reg_beta.tipo_movimiento = 235 THEN 
                  LET reg_det_mov_trab.subcuenta = "12"
               ELSE 
                  LET reg_det_mov_trab.subcuenta = "04"
               END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 5 

                  LET reg_det_mov_trab.subcuenta = "03"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 6 

                  LET reg_det_mov_trab.subcuenta = "06"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
               WHEN 7 

                  LET reg_det_mov_trab.subcuenta = "08"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
               WHEN 8 
              IF reg_beta.tipo_movimiento = 235 THEN
                  LET reg_det_mov_trab.subcuenta = "14"
              ELSE 
                  LET reg_det_mov_trab.subcuenta = "09"
              END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 9 

                  LET reg_det_mov_trab.subcuenta = "07"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos  
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

                OTHERWISE 
                  EXIT CASE
             END CASE

END FUNCTION

FUNCTION tipo_movSDX()
#tmSDX---------------- 

               LET reg_det_mov_trab.iden_mov = " "

               LET reg_det_mov_trab.cont_serv     = 
                   reg_cza_hist_trab.cont_serv


               LET reg_det_mov_trab.curp = 
		   reg_cza_hist_trab.n_unico

               LET reg_det_mov_trab.nss_afore = 
		   reg_cza_hist_trab.nss

               LET reg_det_mov_trab.iden_proc = "03"

               LET reg_det_mov_trab.consecutivo   =
                   reg_det_mov_trab.consecutivo + 1


               LET reg_det_mov_trab.iden_mov[1,8]   = 
                   reg_beta.fecha_conversion USING"YYYYMMDD"
                 
                          LET reg_det_mov_trab.iden_mov[9,10]  = "04"
                          LET reg_det_mov_trab.iden_mov[11,12] = "01"
                          LET reg_det_mov_trab.iden_mov[13,15] = c_cve_afore
                          LET reg_det_mov_trab.iden_mov[16,17] = "04"
                          LET reg_det_mov_trab.iden_mov[18,20] = "002"

                      IF reg_beta.monto_en_pesos >= 0 THEN 
                         LET reg_det_mov_trab.tipo_mov = "A"
                      ELSE 
                         LET reg_det_mov_trab.tipo_mov = "C"
                      END IF

                      LET reg_det_mov_trab.siefore =  vv_siefore
         
                      LET reg_det_mov_trab.iden_mov[23,27] = "00000"
                      LET reg_det_mov_trab.iden_mov[28,32] = "00000"

                      LET reg_det_mov_trab.fecha_valuacion  = 
                      reg_beta.fecha_conversion USING"YYYYMMDD"

                      LET reg_det_mov_trab.acciones = 
                          reg_beta.monto_en_acciones

                      LET reg_det_mov_trab.precio_accion = 
                          reg_beta.precio_accion

                      LET reg_det_mov_trab.fecha_proceso = 
                          TODAY 

              CASE reg_beta.subcuenta

               WHEN 4 
               IF reg_beta.tipo_movimiento = 4 THEN 
                  LET reg_det_mov_trab.subcuenta = "12"
               ELSE 
                  LET reg_det_mov_trab.subcuenta = "04"
               END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 8 
              IF reg_beta.tipo_movimiento = 4 THEN
                  LET reg_det_mov_trab.subcuenta = "14"
              ELSE 
                  LET reg_det_mov_trab.subcuenta = "09"
              END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

                OTHERWISE 
                  EXIT CASE
             END CASE

END FUNCTION

FUNCTION tipo_mov1IC()
#idm1IC------------------


               LET reg_det_mov_trab.iden_mov = " "

               LET reg_det_mov_trab.cont_serv     = 
                   reg_cza_hist_trab.cont_serv


               LET reg_det_mov_trab.curp = 
		   reg_cza_hist_trab.n_unico

               LET reg_det_mov_trab.nss_afore = 
		   reg_cza_hist_trab.nss

               LET reg_det_mov_trab.iden_proc = "03"

               LET reg_det_mov_trab.consecutivo   =
                   reg_det_mov_trab.consecutivo + 1


                   LET reg_det_mov_trab.iden_mov[1,8] = 
                       reg_beta.fecha_conversion USING"YYYYMMDD"

                   LET reg_det_mov_trab.iden_mov[9,10]  = "01"

                   LET reg_det_mov_trab.iden_mov[11,12] = "01"
                   LET reg_det_mov_trab.iden_mov[13,15] = c_cve_afore
                   LET reg_det_mov_trab.iden_mov[16,17] = "07"

                   LET reg_det_mov_trab.iden_mov[18,20] =
                       reg_beta.id_aportante[4,6]
                
                   CASE reg_beta.id_aportante[1,2] 
                   WHEN "TI"  
                       IF (reg_beta.tipo_movimiento = 3 OR 
                          reg_beta.tipo_movimiento = 5 )THEN
                           LET reg_det_mov_trab.iden_mov[21,22] = "03"
                       ELSE
                           LET reg_det_mov_trab.iden_mov[21,22] = "01"
                       END IF
                       EXIT CASE 
                   WHEN "IC" 
                       IF (reg_beta.tipo_movimiento = 3 OR 
                          reg_beta.tipo_movimiento = 5 )THEN
                           LET reg_det_mov_trab.iden_mov[21,22] = "03"
                       ELSE
                       LET reg_det_mov_trab.iden_mov[21,22] = "02"
                       END IF
                       EXIT CASE
                   OTHERWISE 
                       EXIT CASE
                   END CASE
       
                   LET reg_det_mov_trab.iden_mov[23,27] = "00000"

                   LET reg_det_mov_trab.iden_mov[28,32] = "00000"


                   IF reg_beta.monto_en_pesos >= 0 THEN 
                      LET reg_det_mov_trab.tipo_mov = "A"
                   ELSE 
                      LET reg_det_mov_trab.tipo_mov = "C"
                   END IF 

              CASE reg_beta.subcuenta

               WHEN 7
                 CASE reg_beta.tipo_movimiento 
                 WHEN 1
                  LET reg_det_mov_trab.subcuenta = "08"
                  EXIT CASE
                 WHEN 4
                  LET reg_det_mov_trab.subcuenta = "24"
                  EXIT CASE
                 WHEN 3
                  LET reg_det_mov_trab.subcuenta = "08"
                  EXIT CASE
                 WHEN 5
                  LET reg_det_mov_trab.subcuenta = "08"
                  EXIT CASE
                 OTHERWISE 
                  EXIT CASE
                 END CASE
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                  LET reg_det_mov_trab.siefore = vv_siefore
                  LET reg_det_mov_trab.fecha_valuacion = 
                      reg_beta.fecha_conversion USING"YYYYMMDD"
                  LET reg_det_mov_trab.acciones = 
                      reg_beta.monto_en_acciones
                  LET reg_det_mov_trab.precio_accion = 
                      reg_beta.precio_accion
                  LET reg_det_mov_trab.fecha_proceso = TODAY
                  LET reg_det_mov_trab.fecha_conversion = 
                      reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 8
                  CASE reg_beta.tipo_movimiento 
                  WHEN 1
                  LET reg_det_mov_trab.subcuenta = "09"
                  EXIT CASE
                  WHEN 4
                  LET reg_det_mov_trab.subcuenta = "14"
                  EXIT CASE
                  OTHERWISE 
                  EXIT CASE
                  END CASE
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                  LET reg_det_mov_trab.siefore = vv_siefore
                  LET reg_det_mov_trab.fecha_valuacion = 
                      reg_beta.fecha_conversion USING"YYYYMMDD"
                  LET reg_det_mov_trab.acciones = 
                      reg_beta.monto_en_acciones
                  LET reg_det_mov_trab.precio_accion = 
                      reg_beta.precio_accion
                  LET reg_det_mov_trab.fecha_proceso = TODAY
                  LET reg_det_mov_trab.fecha_conversion = 
                      reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
              END CASE

END FUNCTION

FUNCTION  tipo_mov3rcv()
#tm3rcv-----------------

               LET reg_det_mov_trab.iden_mov = " "

               LET reg_det_mov_trab.cont_serv     = 
                   reg_cza_hist_trab.cont_serv

               LET reg_det_mov_trab.curp = 
		   reg_cza_hist_trab.n_unico

               LET reg_det_mov_trab.nss_afore = 
		   reg_cza_hist_trab.nss

               LET reg_det_mov_trab.iden_proc = "01"

               LET reg_det_mov_trab.consecutivo   =
                   reg_det_mov_trab.consecutivo + 1

               LET reg_det_mov_trab.iden_mov[1,8] = 
		   reg_beta.fecha_valor USING"YYYYMMDD"

               LET reg_det_mov_trab.iden_mov[9,16] = 
		   reg_beta.fecha_conversion USING"YYYYMMDD"

               LET reg_det_mov_trab.iden_mov[25,32] = 
		   reg_beta.fecha_pago USING"YYYYMMDD"

               LET reg_det_mov_trab.iden_mov[45,47] = c_cve_afore

               LET reg_det_mov_trab.iden_mov[72,73] = "00"
               LET reg_det_mov_trab.iden_mov[74,75] = "00"
               LET reg_det_mov_trab.iden_mov[76,77] = "00"
               LET reg_det_mov_trab.iden_mov[86,87] = "03"
               LET reg_det_mov_trab.iden_mov[88,108] = 
		"000000000000000000000"
		  
               IF reg_beta.monto_en_pesos >= 0 THEN
                  LET reg_det_mov_trab.tipo_mov = "A"
               ELSE    
                  LET reg_det_mov_trab.tipo_mov = "C"
               END IF

               LET reg_det_mov_trab.importe = 
                   reg_beta.monto_en_pesos
 
               LET reg_det_mov_trab.siefore = vv_siefore
        
               LET reg_det_mov_trab.fecha_valuacion = 
                   reg_beta.fecha_conversion USING"YYYYMMDD"

               LET reg_det_mov_trab.acciones = 
                   reg_beta.monto_en_acciones
     
               LET reg_det_mov_trab.precio_accion = 
                   reg_beta.precio_accion

               LET reg_det_mov_trab.fecha_proceso = 
                   TODAY

               LET reg_det_mov_trab.fecha_conversion = 
                   reg_beta.fecha_conversion

              CASE reg_beta.subcuenta

               WHEN 1
                  LET reg_det_mov_trab.subcuenta = "01"
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 2 
                  LET reg_det_mov_trab.subcuenta = "02"
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 3 
                  LET reg_det_mov_trab.subcuenta = "23"
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

                OTHERWISE 
                  EXIT CASE
                END CASE

END FUNCTION

FUNCTION tipo_mov3viv()
#---------------------
                LET reg_det_mov_trab.iden_mov = " "


                LET reg_det_mov_trab.cont_serv     = 
                    reg_cza_hist_trab.cont_serv


               LET reg_det_mov_trab.curp = 
		   reg_cza_hist_trab.n_unico

               LET reg_det_mov_trab.nss_afore = 
		   reg_cza_hist_trab.nss

                    
                LET reg_det_mov_trab.iden_proc = "06"

                LET reg_det_mov_trab.consecutivo   =
                    reg_det_mov_trab.consecutivo + 1
 

                IF reg_beta.id_aportante = "INFONAVIT" THEN
                    LET reg_det_mov_trab.iden_mov[1,2] = '01'
		ELSE 
	            LET reg_det_mov_trab.iden_mov[1,2] = '02'
                END IF

               IF reg_beta.monto_en_pesos >= 0 THEN
                  LET reg_det_mov_trab.tipo_mov = "A"
               ELSE    
                  LET reg_det_mov_trab.tipo_mov = "C"
               END IF


               LET reg_det_mov_trab.importe = 
                   reg_beta.monto_en_pesos 
  
               LET reg_det_mov_trab.siefore = vv_siefore
       
               LET reg_det_mov_trab.fecha_valuacion = 
                   reg_beta.fecha_conversion USING"YYYYMMDD"  

              LET reg_det_mov_trab.acciones  = 
                  reg_beta.monto_en_acciones

              LET reg_det_mov_trab.precio_accion =
                  reg_beta.precio_accion

              LET reg_det_mov_trab.fecha_proceso = 
                  TODAY

              LET reg_det_mov_trab.fecha_conversion = 
                  reg_beta.fecha_conversion 

              CASE reg_beta.subcuenta

               WHEN "4"
                  IF reg_beta.id_aportante = "INFONAVIT" THEN
                  LET reg_det_mov_trab.subcuenta = "12"
                  ELSE
                  LET reg_det_mov_trab.subcuenta = "15"
                  END IF 
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 8
                  IF reg_beta.id_aportante = "INFONAVIT" THEN
                  LET reg_det_mov_trab.subcuenta = "14"
                  ELSE
                  LET reg_det_mov_trab.subcuenta = "16"
                  END IF 
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               OTHERWISE 
                  EXIT CASE
               END CASE
END FUNCTION

FUNCTION  tipo_mov4dis()
#tm4dis-----------------

     DEFINE v_paso_fecha DATE

     DEFINE reg_cur1_tipo_mov4dis RECORD
            fecha_pago            LIKE safre_af:dis_cuenta.fecha_pago      ,
            fecha_valor           LIKE safre_af:dis_cuenta.fecha_valor     ,
            folio_sua             LIKE safre_af:dis_cuenta.folio_sua       ,
            fecha_conversion      LIKE safre_af:dis_cuenta.fecha_conversion
     END RECORD

      DEFINE reg_cur2_tipo_mov4dis RECORD 
	     fech_valor_rcv     LIKE safre_af:dis_det_aporte.fech_valor_rcv    ,
	     fech_valor_viv     LIKE safre_af:dis_det_aporte.fech_valor_viv    ,
	     fech_pago          LIKE safre_af:dis_det_aporte.fech_pago         ,
	     periodo_pago       LIKE safre_af:dis_det_aporte.periodo_pago      ,
	     folio_pago_sua     LIKE safre_af:dis_det_aporte.folio_pago_sua    ,
	     cve_ent_receptora  LIKE safre_af:dis_det_aporte.cve_ent_receptora ,
	     reg_patronal_imss  LIKE safre_af:dis_det_aporte.reg_patronal_imss ,
	     dias_cotz_bimestre LIKE safre_af:dis_det_aporte.dias_cotz_bimestre,
             dias_incap_bimest  LIKE safre_af:dis_det_aporte.dias_incap_bimest ,
             dias_ausent_bimest LIKE safre_af:dis_det_aporte.dias_ausent_bimest,
             fecha_estado       LIKE safre_af:dis_det_aporte.fecha_estado,
             rfc_patron         LIKE safre_af:dis_det_aporte.rfc_patron,
             ult_salario_diario LIKE safre_af:dis_det_aporte.ult_salario_diario
     END RECORD

     DEFINE reg_cur3_tipo_mov4dis  RECORD 
            subcuenta         SMALLINT      ,
            fecha_conversion  DATE          ,
            monto_en_pesos    DECIMAL(13,2) ,
            monto_en_acciones DECIMAL(13,2) ,
            precio_accion     LIKE safre_af:dis_cuenta.precio_accion
     END RECORD

     DEFINE v_fecha_pago          CHAR(008),
            v_fecha_valor         CHAR(008)

          LET reg_det_mov_trab.iden_mov = " "

          DECLARE cur_mov4 CURSOR FOR

          SELECT A.fech_valor_rcv        ,
	         A.fech_valor_viv        ,
	         A.fech_pago             ,
	         A.periodo_pago          ,
	         A.folio_pago_sua        ,
	         A.cve_ent_receptora     ,
	         A.reg_patronal_imss     ,
	         A.dias_cotz_bimestre    ,
                 A.dias_incap_bimest     ,
                 A.dias_ausent_bimest    ,
                 A.fecha_estado          ,
                 A.rfc_patron            ,
                 A.ult_salario_diario
          FROM   safre_af:dis_det_interes A 
          WHERE  A.folio             = reg_beta.folio
          AND    A.n_seguro          = reg_cza_hist_trab.nss 
          AND    A.folio_pago_sua    = reg_beta.folio_sua
          --AND    A.reg_patronal_imss = reg_beta.id_aportante
	  --AND    A.fech_pago         = v_fecha_pago
	  --AND    A.fech_valor_rcv    = v_fecha_valor
	  --AND    A.dias_cotz_bimestre  = reg_beta.dias_cotizados


          INITIALIZE reg_cur2_tipo_mov4dis.* TO NULL

          FOREACH cur_mov4 INTO reg_cur2_tipo_mov4dis.*
          END FOREACH

               LET reg_det_mov_trab.cont_serv     = 
                   reg_cza_hist_trab.cont_serv


               LET reg_det_mov_trab.curp = 
		   reg_cza_hist_trab.n_unico

               LET reg_det_mov_trab.nss_afore = 
		   reg_cza_hist_trab.nss

               LET reg_det_mov_trab.iden_proc = "01"
                    
               LET reg_det_mov_trab.consecutivo   =
                   reg_det_mov_trab.consecutivo + 1
 
               LET reg_det_mov_trab.iden_mov[1,8]   = 
                   reg_cur2_tipo_mov4dis.fech_valor_rcv 
 
               LET reg_det_mov_trab.iden_mov[9,16]  = 
                   reg_beta.fecha_conversion USING"YYYYMMDD"

               LET reg_det_mov_trab.iden_mov[17,24] = 
                   reg_cur2_tipo_mov4dis.fech_valor_viv

               LET reg_det_mov_trab.iden_mov[25,32]  =
                   reg_cur2_tipo_mov4dis.fech_pago          

               LET reg_det_mov_trab.iden_mov[33,38] = 
                   reg_cur2_tipo_mov4dis.periodo_pago

               LET reg_det_mov_trab.iden_mov[39,44] = 
                   reg_cur2_tipo_mov4dis.folio_pago_sua

               LET reg_det_mov_trab.iden_mov[45,47] = 
                   reg_cur2_tipo_mov4dis.cve_ent_receptora USING"&&&" 

               LET reg_det_mov_trab.iden_mov[48,58] = 
                   reg_cur2_tipo_mov4dis.reg_patronal_imss

               LET reg_det_mov_trab.iden_mov[59,71] =
                   reg_cur2_tipo_mov4dis.rfc_patron   

               LET reg_det_mov_trab.iden_mov[72,73] = 
                   reg_cur2_tipo_mov4dis.dias_cotz_bimestre USING"&&"

               LET reg_det_mov_trab.iden_mov[74,75] = 
                   reg_cur2_tipo_mov4dis.dias_incap_bimest USING"&&"

               LET reg_det_mov_trab.iden_mov[76,77] = 
                   reg_cur2_tipo_mov4dis.dias_ausent_bimest USING"&&"

               LET reg_det_mov_trab.iden_mov[78,85] =
                   reg_cur2_tipo_mov4dis.fecha_estado USING "YYYYMMDD"    


               LET reg_det_mov_trab.siefore =  vv_siefore

               LET reg_det_mov_trab.fecha_valuacion = 
                   reg_beta.fecha_conversion USING "YYYYMMDD"

               LET reg_det_mov_trab.acciones = 
                   reg_beta.monto_en_acciones

               LET reg_det_mov_trab.precio_accion = 
                   reg_beta.precio_accion

              LET v_cuantos = 0

              SELECT count(*) 
              INTO   v_cuantos
              FROM   safre_af:dis_dep_aporte A
              WHERE  A.folio = reg_beta.folio
   
              IF v_cuantos = 5 THEN
               LET reg_det_mov_trab.iden_mov[86,87] = "02"
              END IF
              IF v_cuantos = 6 THEN
               LET reg_det_mov_trab.iden_mov[86,87] = "02"
              END IF
              IF v_cuantos = 10 THEN 
               LET reg_det_mov_trab.iden_mov[86,87] = "01"
              END IF 
              IF v_cuantos = 12 THEN 
               LET reg_det_mov_trab.iden_mov[86,87] = "01"
              END IF 

              LET reg_det_mov_trab.iden_mov[88,94] = 
                  reg_cur2_tipo_mov4dis.ult_salario_diario 
		  USING "&&&&&&&"

              LET v_comis = 0

              LET reg_det_mov_trab.iden_mov[95,108] = 
              v_comis  USING"&&&&&&&&&&&&&&"

              IF reg_beta.monto_en_pesos < 0 THEN
                 LET reg_det_mov_trab.tipo_mov = "C" 
              ELSE 
                 LET reg_det_mov_trab.tipo_mov = "A"
              END IF

              CASE reg_beta.subcuenta

               WHEN 1
                  LET reg_det_mov_trab.subcuenta = "18"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 2 

                  LET reg_det_mov_trab.subcuenta = "20"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 3 

                  LET reg_det_mov_trab.subcuenta = "23"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 4 

                  LET reg_det_mov_trab.subcuenta = "12"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 5 

                  LET reg_det_mov_trab.subcuenta = "19"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 6 

                  LET reg_det_mov_trab.subcuenta = "10"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 9 

                  LET reg_det_mov_trab.subcuenta = "11"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos  
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

                OTHERWISE 
                           EXIT CASE
                END CASE

END FUNCTION

FUNCTION tipo_mov400IVRT()
#tm1AF-------------------- 

                       LET reg_det_mov_trab.iden_mov = " "

                       LET reg_det_mov_trab.cont_serv     = 
                          reg_cza_hist_trab.cont_serv


               LET reg_det_mov_trab.curp = 
		   reg_cza_hist_trab.n_unico

               LET reg_det_mov_trab.nss_afore = 
		   reg_cza_hist_trab.nss

                      LET reg_det_mov_trab.iden_proc = "02"

                      LET reg_det_mov_trab.consecutivo   =
                          reg_det_mov_trab.consecutivo + 1

                      LET reg_det_mov_trab.iden_mov[1,8]   = 
                          "00010101"

                      LET reg_det_mov_trab.iden_mov[9,16]   = 
                          reg_beta.fecha_conversion USING"YYYYMMDD"
                
                    
                      CASE reg_beta.tipo_movimiento 
                      WHEN 401 
                          LET reg_det_mov_trab.iden_mov[17,18]="IM"
                          LET reg_det_mov_trab.iden_mov[19,20]="IN"
                        EXIT CASE
                      WHEN 402 
                          LET reg_det_mov_trab.iden_mov[17,18]="IM"
                          LET reg_det_mov_trab.iden_mov[19,20]="VI"
                        EXIT CASE
                      WHEN 403 
                          LET reg_det_mov_trab.iden_mov[17,18]="IM"
                          LET reg_det_mov_trab.iden_mov[19,20]="VO"
                        EXIT CASE
                      WHEN 404 
                          LET reg_det_mov_trab.iden_mov[17,18]="IM"
                          LET reg_det_mov_trab.iden_mov[19,20]="OR"
                        EXIT CASE
                      WHEN 405 
                          LET reg_det_mov_trab.iden_mov[17,18]="IM"
                          LET reg_det_mov_trab.iden_mov[19,20]="AS"
                        EXIT CASE
                      WHEN 408 
                          LET reg_det_mov_trab.iden_mov[17,18]="RT"
                          LET reg_det_mov_trab.iden_mov[19,20]="IP"
                        EXIT CASE
                      WHEN 409 
                          LET reg_det_mov_trab.iden_mov[17,18]="RT"
                          LET reg_det_mov_trab.iden_mov[19,20]="VI"
                        EXIT CASE
                      WHEN 410
                          LET reg_det_mov_trab.iden_mov[17,18]="RT"
                          LET reg_det_mov_trab.iden_mov[19,20]="VO"
                        EXIT CASE
                      WHEN 411
                          LET reg_det_mov_trab.iden_mov[17,18]="RT"
                          LET reg_det_mov_trab.iden_mov[19,20]="OR"
                        EXIT CASE
                      WHEN 412
                          LET reg_det_mov_trab.iden_mov[17,18]="RT"
                          LET reg_det_mov_trab.iden_mov[19,20]="AS"
                        EXIT CASE

                      OTHERWISE
                         EXIT CASE
                     END CASE 
                          
                       LET reg_det_mov_trab.iden_mov[21,22] = "01"
                       LET reg_det_mov_trab.iden_mov[46,47] = "01"
 
                      IF reg_beta.monto_en_pesos >= 0 THEN 
                         LET reg_det_mov_trab.tipo_mov = "A"
                      ELSE 
                         LET reg_det_mov_trab.tipo_mov = "C"
                      END IF

                      LET reg_det_mov_trab.siefore =  vv_siefore
                         
                      LET reg_det_mov_trab.fecha_valuacion  = 
                      reg_beta.fecha_conversion USING"YYYYMMDD"

                      LET reg_det_mov_trab.acciones = 
                          reg_beta.monto_en_acciones

                      LET reg_det_mov_trab.precio_accion = 
                          reg_beta.precio_accion

                      LET reg_det_mov_trab.fecha_proceso = 
                          TODAY 

              CASE reg_beta.subcuenta

               WHEN 1
                  LET reg_det_mov_trab.subcuenta = "01"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                  LET reg_det_mov_trab.fecha_conversion = 
                      reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 2 

                  LET reg_det_mov_trab.subcuenta = "02"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE


               WHEN 4 
                  LET reg_det_mov_trab.subcuenta = "04"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 5 

                  LET reg_det_mov_trab.subcuenta = "03"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 6 

                  LET reg_det_mov_trab.subcuenta = "06"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 9

                  LET reg_det_mov_trab.subcuenta = "07"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

                OTHERWISE 
                  EXIT CASE
             END CASE

END FUNCTION

FUNCTION tipo_mov400PARCIAL()
#tm1AF---------------------- 

                       LET reg_det_mov_trab.iden_mov = " "

                       LET reg_det_mov_trab.cont_serv     = 
                          reg_cza_hist_trab.cont_serv


               LET reg_det_mov_trab.curp = 
		   reg_cza_hist_trab.n_unico

               LET reg_det_mov_trab.nss_afore = 
		   reg_cza_hist_trab.nss

                      LET reg_det_mov_trab.iden_proc = "02"

                      LET reg_det_mov_trab.consecutivo   =
                          reg_det_mov_trab.consecutivo + 1

                      LET reg_det_mov_trab.iden_mov[1,8]   = 
                          "00010101"

                      LET reg_det_mov_trab.iden_mov[9,16]   = 
                          reg_beta.fecha_conversion USING"YYYYMMDD"
                
                      IF reg_beta.tipo_movimiento = 486 THEN           
                       LET reg_det_mov_trab.iden_mov[21,22] = "06"
                      ELSE
                       LET reg_det_mov_trab.iden_mov[21,22] = "07"
                      END IF

                       LET reg_det_mov_trab.iden_mov[46,47] = "02"
 
                      IF reg_beta.monto_en_pesos >= 0 THEN 
                         LET reg_det_mov_trab.tipo_mov = "A"
                      ELSE 
                         LET reg_det_mov_trab.tipo_mov = "C"
                      END IF

                      LET reg_det_mov_trab.siefore =  vv_siefore
                         
                      LET reg_det_mov_trab.fecha_valuacion  = 
                      reg_beta.fecha_conversion USING"YYYYMMDD"

                      LET reg_det_mov_trab.acciones = 
                          reg_beta.monto_en_acciones

                      LET reg_det_mov_trab.precio_accion = 
                          reg_beta.precio_accion

                      LET reg_det_mov_trab.fecha_proceso = 
                          TODAY 

              CASE reg_beta.subcuenta

               WHEN 1
                  LET reg_det_mov_trab.subcuenta = "01"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                  LET reg_det_mov_trab.fecha_conversion = 
                      reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 2 

                  LET reg_det_mov_trab.subcuenta = "02"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 4 

                  LET reg_det_mov_trab.subcuenta = "04"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 5 

                  LET reg_det_mov_trab.subcuenta = "03"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 6 

                  LET reg_det_mov_trab.subcuenta = "06"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
               WHEN 9 

                  LET reg_det_mov_trab.subcuenta = "07"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
                OTHERWISE 
                  EXIT CASE
             END CASE

END FUNCTION
FUNCTION tipo_mov400TOTAL()
#tm1AF-------------------- 

                       LET reg_det_mov_trab.iden_mov = " "

                       LET reg_det_mov_trab.cont_serv     = 
                          reg_cza_hist_trab.cont_serv


               LET reg_det_mov_trab.curp = 
		   reg_cza_hist_trab.n_unico

               LET reg_det_mov_trab.nss_afore = 
		   reg_cza_hist_trab.nss

                      LET reg_det_mov_trab.iden_proc = "02"

                      LET reg_det_mov_trab.consecutivo   =
                          reg_det_mov_trab.consecutivo + 1

                      LET reg_det_mov_trab.iden_mov[1,8]   = 
                          "00010101"

                      LET reg_det_mov_trab.iden_mov[9,16]   = 
                          reg_beta.fecha_conversion USING"YYYYMMDD"
                
                    
                      CASE reg_beta.tipo_movimiento 
                      WHEN 457
                          LET reg_det_mov_trab.iden_mov[17,18]="IM"
                          LET reg_det_mov_trab.iden_mov[19,20]="IN"
                        EXIT CASE
                      WHEN 458
                          LET reg_det_mov_trab.iden_mov[17,18]="IM"
                          LET reg_det_mov_trab.iden_mov[19,20]="VI"
                        EXIT CASE
                      WHEN 459 
                          LET reg_det_mov_trab.iden_mov[17,18]="IM"
                          LET reg_det_mov_trab.iden_mov[19,20]="VO"
                        EXIT CASE
                      WHEN 460
                          LET reg_det_mov_trab.iden_mov[17,18]="IM"
                          LET reg_det_mov_trab.iden_mov[19,20]="OR"
                        EXIT CASE
                      WHEN 461 
                          LET reg_det_mov_trab.iden_mov[17,18]="IM"
                          LET reg_det_mov_trab.iden_mov[19,20]="AS"
                        EXIT CASE
                      WHEN 464 
                          LET reg_det_mov_trab.iden_mov[17,18]="RT"
                          LET reg_det_mov_trab.iden_mov[19,20]="IP"
                        EXIT CASE
                      WHEN 465
                          LET reg_det_mov_trab.iden_mov[17,18]="RT"
                          LET reg_det_mov_trab.iden_mov[19,20]="VI"
                        EXIT CASE
                      WHEN 466
                          LET reg_det_mov_trab.iden_mov[17,18]="RT"
                          LET reg_det_mov_trab.iden_mov[19,20]="VO"
                        EXIT CASE
                      WHEN 467
                          LET reg_det_mov_trab.iden_mov[17,18]="RT"
                          LET reg_det_mov_trab.iden_mov[19,20]="OR"
                        EXIT CASE
                      WHEN 468
                          LET reg_det_mov_trab.iden_mov[17,18]="RT"
                          LET reg_det_mov_trab.iden_mov[19,20]="AS"
                        EXIT CASE
                      WHEN 471
                          LET reg_det_mov_trab.iden_mov[17,18]="IV"
                          LET reg_det_mov_trab.iden_mov[19,20]="VE"
                        EXIT CASE
                      WHEN 472
                          LET reg_det_mov_trab.iden_mov[17,18]="IV"
                          LET reg_det_mov_trab.iden_mov[19,20]="CE"
                        EXIT CASE
                      WHEN 475
                          LET reg_det_mov_trab.iden_mov[17,18]="TJ"
                          LET reg_det_mov_trab.iden_mov[19,20]="IP"
                        EXIT CASE
                      WHEN 476
                          LET reg_det_mov_trab.iden_mov[17,18]="TJ"
                          LET reg_det_mov_trab.iden_mov[19,20]="IN"
                        EXIT CASE

                      WHEN 477
                          LET reg_det_mov_trab.iden_mov[17,18]="TJ"
                          LET reg_det_mov_trab.iden_mov[19,20]="VI"
                        EXIT CASE
                      WHEN 478
                          LET reg_det_mov_trab.iden_mov[17,18]="TJ"
                          LET reg_det_mov_trab.iden_mov[19,20]="VO"
                        EXIT CASE
                      WHEN 479
                          LET reg_det_mov_trab.iden_mov[17,18]="TJ"
                          LET reg_det_mov_trab.iden_mov[19,20]="OR"
                        EXIT CASE
                      WHEN 480
                          LET reg_det_mov_trab.iden_mov[17,18]="TJ"
                          LET reg_det_mov_trab.iden_mov[19,20]="AS"
                        EXIT CASE
                      WHEN 481
                          LET reg_det_mov_trab.iden_mov[17,18]="TJ"
                          LET reg_det_mov_trab.iden_mov[19,20]="RE"
                        EXIT CASE
                      WHEN 482
                          LET reg_det_mov_trab.iden_mov[17,18]="TJ"
                          LET reg_det_mov_trab.iden_mov[19,20]="VE"
                        EXIT CASE
                      WHEN 483
                          LET reg_det_mov_trab.iden_mov[17,18]="TJ"
                          LET reg_det_mov_trab.iden_mov[19,20]="CE"
                        EXIT CASE
                      OTHERWISE
                         EXIT CASE
                     END CASE 
                          
                       LET reg_det_mov_trab.iden_mov[21,22] = "03"
                       LET reg_det_mov_trab.iden_mov[46,47] = "03"
 
                      IF reg_beta.monto_en_pesos >= 0 THEN 
                         LET reg_det_mov_trab.tipo_mov = "A"
                      ELSE 
                         LET reg_det_mov_trab.tipo_mov = "C"
                      END IF

                      LET reg_det_mov_trab.siefore =  vv_siefore
                         
                      LET reg_det_mov_trab.fecha_valuacion  = 
                      reg_beta.fecha_conversion USING"YYYYMMDD"

                      LET reg_det_mov_trab.acciones = 
                          reg_beta.monto_en_acciones

                      LET reg_det_mov_trab.precio_accion = 
                          reg_beta.precio_accion

                      LET reg_det_mov_trab.fecha_proceso = 
                          TODAY 

              CASE reg_beta.subcuenta

               WHEN 1
                  LET reg_det_mov_trab.subcuenta = "01"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                  LET reg_det_mov_trab.fecha_conversion = 
                      reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 2 

                  LET reg_det_mov_trab.subcuenta = "02"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE


               WHEN 4 
                  LET reg_det_mov_trab.subcuenta = "04"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 5 

                  LET reg_det_mov_trab.subcuenta = "03"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 6 

                  LET reg_det_mov_trab.subcuenta = "06"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
               WHEN 7

                  LET reg_det_mov_trab.subcuenta = "08"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
               WHEN 8 

                  LET reg_det_mov_trab.subcuenta = "09"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
               WHEN 9 

                  LET reg_det_mov_trab.subcuenta = "07"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
                OTHERWISE 
                  EXIT CASE
             END CASE

END FUNCTION
FUNCTION tipo_mov400SAR92()
#tm1AF-------------------- 

                       LET reg_det_mov_trab.iden_mov = " "

                       LET reg_det_mov_trab.cont_serv     = 
                          reg_cza_hist_trab.cont_serv


               LET reg_det_mov_trab.curp = 
		   reg_cza_hist_trab.n_unico

               LET reg_det_mov_trab.nss_afore = 
		   reg_cza_hist_trab.nss

                      LET reg_det_mov_trab.iden_proc = "02"

                      LET reg_det_mov_trab.consecutivo   =
                          reg_det_mov_trab.consecutivo + 1

                      LET reg_det_mov_trab.iden_mov[1,8]   = 
                          "00010101"

                      LET reg_det_mov_trab.iden_mov[9,16]   = 
                          reg_beta.fecha_conversion USING"YYYYMMDD"
                
                    
                      CASE reg_beta.tipo_movimiento 
                      WHEN 415 
                          LET reg_det_mov_trab.iden_mov[17,18]="IM"
                          LET reg_det_mov_trab.iden_mov[19,20]="IN"
                        EXIT CASE
                      WHEN 416
                          LET reg_det_mov_trab.iden_mov[17,18]="IM"
                          LET reg_det_mov_trab.iden_mov[19,20]="VI"
                        EXIT CASE
                      WHEN 417 
                          LET reg_det_mov_trab.iden_mov[17,18]="IM"
                          LET reg_det_mov_trab.iden_mov[19,20]="VO"
                        EXIT CASE
                      WHEN 418
                          LET reg_det_mov_trab.iden_mov[17,18]="IM"
                          LET reg_det_mov_trab.iden_mov[19,20]="OR"
                        EXIT CASE
                      WHEN 419 
                          LET reg_det_mov_trab.iden_mov[17,18]="IM"
                          LET reg_det_mov_trab.iden_mov[19,20]="AS"
                        EXIT CASE
                      WHEN 422
                          LET reg_det_mov_trab.iden_mov[17,18]="RT"
                          LET reg_det_mov_trab.iden_mov[19,20]="IP"
                        EXIT CASE
                      WHEN 423 
                          LET reg_det_mov_trab.iden_mov[17,18]="RT"
                          LET reg_det_mov_trab.iden_mov[19,20]="VI"
                        EXIT CASE
                      WHEN 424
                          LET reg_det_mov_trab.iden_mov[17,18]="RT"
                          LET reg_det_mov_trab.iden_mov[19,20]="VO"
                        EXIT CASE
                      WHEN 425
                          LET reg_det_mov_trab.iden_mov[17,18]="RT"
                          LET reg_det_mov_trab.iden_mov[19,20]="OR"
                        EXIT CASE
                      WHEN 426
                          LET reg_det_mov_trab.iden_mov[17,18]="RT"
                          LET reg_det_mov_trab.iden_mov[19,20]="AS"
                        EXIT CASE
                      WHEN 429
                          LET reg_det_mov_trab.iden_mov[17,18]="IV"
                          LET reg_det_mov_trab.iden_mov[19,20]="VE"
                        EXIT CASE
                      WHEN 430
                          LET reg_det_mov_trab.iden_mov[17,18]="IV"
                          LET reg_det_mov_trab.iden_mov[19,20]="CE"
                        EXIT CASE
                      WHEN 433
                          LET reg_det_mov_trab.iden_mov[17,18]="PP"
                          LET reg_det_mov_trab.iden_mov[19,20]="  " 
                        EXIT CASE 
                      WHEN 436
                          LET reg_det_mov_trab.iden_mov[17,18]="IV"
                          LET reg_det_mov_trab.iden_mov[19,20]="IP"
                        EXIT CASE
                      WHEN 437
                          LET reg_det_mov_trab.iden_mov[17,18]="IV"
                          LET reg_det_mov_trab.iden_mov[19,20]="IN"
                        EXIT CASE
                      WHEN 438
                          LET reg_det_mov_trab.iden_mov[17,18]="IV"
                          LET reg_det_mov_trab.iden_mov[19,20]="VI"
                        EXIT CASE
                      WHEN 439
                          LET reg_det_mov_trab.iden_mov[17,18]="IV"
                          LET reg_det_mov_trab.iden_mov[19,20]="VO"
                        EXIT CASE

                      WHEN 440
                          LET reg_det_mov_trab.iden_mov[17,18]="IV"
                          LET reg_det_mov_trab.iden_mov[19,20]="OR"
                        EXIT CASE
                      WHEN 441
                          LET reg_det_mov_trab.iden_mov[17,18]="IV"
                          LET reg_det_mov_trab.iden_mov[19,20]="AS"
                        EXIT CASE
                      WHEN 442
                          LET reg_det_mov_trab.iden_mov[17,18]="CV"
                          LET reg_det_mov_trab.iden_mov[19,20]="VE"
                        EXIT CASE
                      WHEN 443
                          LET reg_det_mov_trab.iden_mov[17,18]="CV"
                          LET reg_det_mov_trab.iden_mov[19,20]="CE"
                        EXIT CASE
                      WHEN 446
                          LET reg_det_mov_trab.iden_mov[17,18]="TJ"
                          LET reg_det_mov_trab.iden_mov[19,20]="IP"
                        EXIT CASE
                      WHEN 447
                          LET reg_det_mov_trab.iden_mov[17,18]="TJ"
                          LET reg_det_mov_trab.iden_mov[19,20]="IN"
                        EXIT CASE
                      WHEN 448
                          LET reg_det_mov_trab.iden_mov[17,18]="TJ"
                          LET reg_det_mov_trab.iden_mov[19,20]="VI"
                        EXIT CASE
                      WHEN 449
                          LET reg_det_mov_trab.iden_mov[17,18]="TJ"
                          LET reg_det_mov_trab.iden_mov[19,20]="VO"
                        EXIT CASE
                      WHEN 450
                          LET reg_det_mov_trab.iden_mov[17,18]="TJ"
                          LET reg_det_mov_trab.iden_mov[19,20]="OR"
                        EXIT CASE
                      WHEN 451
                          LET reg_det_mov_trab.iden_mov[17,18]="TJ"
                          LET reg_det_mov_trab.iden_mov[19,20]="AS"
                        EXIT CASE
                      WHEN 453
                          LET reg_det_mov_trab.iden_mov[17,18]="TJ"
                          LET reg_det_mov_trab.iden_mov[19,20]="RE"
                        EXIT CASE
                      WHEN 454
                          LET reg_det_mov_trab.iden_mov[17,18]="TJ"
                          LET reg_det_mov_trab.iden_mov[19,20]="VE"
                        EXIT CASE
                      OTHERWISE
                         EXIT CASE
                     END CASE 
                       IF  (reg_beta.tipo_movimiento > 433 AND 
                            reg_beta.tipo_movimiento < 446 ) THEN
                       LET reg_det_mov_trab.iden_mov[21,22] = "04"
                       ELSE
                       LET reg_det_mov_trab.iden_mov[21,22] = "05"
                       END IF
                       LET reg_det_mov_trab.iden_mov[46,47] = "04"
 
                      IF reg_beta.monto_en_pesos >= 0 THEN 
                         LET reg_det_mov_trab.tipo_mov = "A"
                      ELSE 
                         LET reg_det_mov_trab.tipo_mov = "C"
                      END IF

                      LET reg_det_mov_trab.siefore =  vv_siefore
                         
                      LET reg_det_mov_trab.fecha_valuacion  = 
                      reg_beta.fecha_conversion USING"YYYYMMDD"

                      LET reg_det_mov_trab.acciones = 
                          reg_beta.monto_en_acciones

                      LET reg_det_mov_trab.precio_accion = 
                          reg_beta.precio_accion

                      LET reg_det_mov_trab.fecha_proceso = 
                          TODAY 

              CASE reg_beta.subcuenta

               WHEN 7
                  LET reg_det_mov_trab.subcuenta = "08"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                  LET reg_det_mov_trab.fecha_conversion = 
                      reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 8 

                  LET reg_det_mov_trab.subcuenta = "09"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

                OTHERWISE 
                  EXIT CASE
             END CASE

END FUNCTION
FUNCTION tipo_mov400VOL()
#tm1AF-------------------- 

                       LET reg_det_mov_trab.iden_mov = " "

                       LET reg_det_mov_trab.cont_serv     = 
                          reg_cza_hist_trab.cont_serv


               LET reg_det_mov_trab.curp = 
		   reg_cza_hist_trab.n_unico

               LET reg_det_mov_trab.nss_afore = 
		   reg_cza_hist_trab.nss

                      LET reg_det_mov_trab.iden_proc = "02"

                      LET reg_det_mov_trab.consecutivo   =
                          reg_det_mov_trab.consecutivo + 1

                      LET reg_det_mov_trab.iden_mov[1,8]   = 
                          "00010101"

                      LET reg_det_mov_trab.iden_mov[9,16]   = 
                          reg_beta.fecha_conversion USING"YYYYMMDD"
                
                    IF reg_beta.id_aportante = "PATRON" THEN 
                       LET reg_det_mov_trab.iden_mov[46,47] = "07"
                    ELSE 
                       LET reg_det_mov_trab.iden_mov[46,47] = "08"
                    END IF
                      
 
                      IF reg_beta.monto_en_pesos >= 0 THEN 
                         LET reg_det_mov_trab.tipo_mov = "A"
                      ELSE 
                         LET reg_det_mov_trab.tipo_mov = "C"
                      END IF

                      LET reg_det_mov_trab.siefore =  vv_siefore
                         
                      LET reg_det_mov_trab.fecha_valuacion  = 
                      reg_beta.fecha_conversion USING"YYYYMMDD"

                      LET reg_det_mov_trab.acciones = 
                          reg_beta.monto_en_acciones

                      LET reg_det_mov_trab.precio_accion = 
                          reg_beta.precio_accion

                      LET reg_det_mov_trab.fecha_proceso = 
                          TODAY 

              CASE reg_beta.subcuenta

               WHEN 3
                   IF reg_beta.id_aportante = "PATRON" THEN   
                      LET reg_det_mov_trab.subcuenta = "13"
                    ELSE
                      LET reg_det_mov_trab.subcuenta = "05"
                   END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                  LET reg_det_mov_trab.fecha_conversion = 
                      reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
                 OTHERWISE
                 EXIT CASE
             END CASE

END FUNCTION

FUNCTION tipo_mov1VEN()
#tmven -----------------

        LET reg_det_mov_trab.iden_mov = " "

        LET reg_det_mov_trab.cont_serv     = 
            reg_cza_hist_trab.cont_serv


        LET reg_det_mov_trab.curp = 
            reg_cza_hist_trab.n_unico

        LET reg_det_mov_trab.nss_afore = 
	    reg_cza_hist_trab.nss

        LET reg_det_mov_trab.iden_proc = "09"

        LET reg_det_mov_trab.consecutivo   =
            reg_det_mov_trab.consecutivo + 1

        LET reg_det_mov_trab.iden_mov[1,8]   = 
            reg_beta.fecha_conversion USING"YYYYMMDD"

                      IF reg_beta.monto_en_pesos >= 0 THEN 
                         LET reg_det_mov_trab.tipo_mov = "A"
                      ELSE 
                         LET reg_det_mov_trab.tipo_mov = "C"
                      END IF

                      LET reg_det_mov_trab.siefore =  vv_siefore
                         
                      LET reg_det_mov_trab.fecha_valuacion  = 
                      reg_beta.fecha_conversion USING"YYYYMMDD"

                      LET reg_det_mov_trab.acciones = 
                          reg_beta.monto_en_acciones

                      LET reg_det_mov_trab.precio_accion = 
                          reg_beta.precio_accion

                      LET reg_det_mov_trab.fecha_proceso = 
                          TODAY 

              CASE reg_beta.subcuenta

               WHEN 10
                      LET reg_det_mov_trab.subcuenta = "05"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                  LET reg_det_mov_trab.fecha_conversion = 
                      reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
                 OTHERWISE
                 EXIT CASE
             END CASE

END FUNCTION

FUNCTION cal_fecha_retro(x_fecha,ciclo)
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

        LET sig_fecha  = sig_fecha - 1

        LET dia_semana = WEEKDAY(sig_fecha)
        IF dia_semana = 0 OR dia_semana = 6 THEN
           CONTINUE WHILE
        ELSE  
           SELECT "ok" 
           FROM   tab_feriado
           WHERE  feria_fecha = sig_fecha
       
           IF STATUS <> NOTFOUND THEN
              CONTINUE WHILE
           ELSE 
	      LET cc = cc + 1
           END IF	
        END IF
    END WHILE
    RETURN sig_fecha
END FUNCTION

FUNCTION  tipo_movDEV_EX()
#idmDIS-------------------------

    DEFINE v_paso_fecha DATE

    DEFINE reg_cur1_tipo_movex RECORD
            fecha_pago            LIKE safre_af:dis_cuenta.fecha_pago      ,
            fecha_valor           LIKE safre_af:dis_cuenta.fecha_valor     ,
            folio_sua             LIKE safre_af:dis_cuenta.folio_sua       ,
            fecha_conversion      LIKE safre_af:dis_cuenta.fecha_conversion
     END RECORD

      DEFINE reg_cur2_tipo_movex RECORD  LIKE exc_det_exceso.*
      DEFINE reg_cur2_tipo_movex1 RECORD  LIKE dis_det_aporte.*

      DEFINE ex_fecha_conversion DATE,
             ex_fecha_creac DATE

     DEFINE reg_cur3_tipo_movex  RECORD
            subcuenta         SMALLINT      ,
            fecha_conversion  DATE          ,
            monto_en_pesos    DECIMAL(13,2) ,
            monto_en_acciones DECIMAL(13,2) ,
            precio_accion     LIKE safre_af:dis_cuenta.precio_accion
     END RECORD
     DEFINE c_f_pago              CHAR(008),
            c_f_rcv               CHAR(008),
            c_f_viv               CHAR(008),
            c_rfc                 CHAR(010)
     DEFINE v_fecha_pago          CHAR(008),
            v_fecha_valor         CHAR(008)

          LET reg_det_mov_trab.iden_mov = " "
          DECLARE cur_movex1 CURSOR FOR

          SELECT A.*
          FROM   exc_det_exceso A
          WHERE  A.folio             = reg_beta.folio
          AND    A.nss               = reg_cza_hist_trab.nss
          AND    A.folio_pago_sua    = reg_beta.folio_sua

          INITIALIZE reg_cur2_tipo_movex.* TO NULL

          FOREACH cur_movex1 INTO reg_cur2_tipo_movex.*
          END FOREACH

          LET c_f_pago = reg_cur2_tipo_movex.fecha_pago USING"YYYYMMDD"
          LET c_f_rcv = reg_cur2_tipo_movex.fecha_valor_rcv USING"YYYYMMDD"
          LET c_f_viv = reg_cur2_tipo_movex.fecha_valor_viv USING"YYYYMMDD"
          LET c_rfc = reg_cur2_tipo_movex.rfc[1,10]

          DECLARE cur_movex2 CURSOR FOR

           SELECT A.*
           FROM   dis_det_aporte a
           WHERE a.n_seguro        = reg_cur2_tipo_movex.nss
           AND a.n_rfc[1,10]       = c_rfc
           AND a.periodo_pago      = reg_cur2_tipo_movex.periodo_pago
           AND a.fech_pago         = c_f_pago
           AND a.fech_valor_rcv      = c_f_rcv
           AND a.fech_valor_viv      = c_f_viv
           AND a.folio_pago_sua      = reg_cur2_tipo_movex.folio_pago_sua
           AND a.cve_ent_receptora   = reg_cur2_tipo_movex.clave_ent_recep
           AND a.reg_patronal_imss   = reg_cur2_tipo_movex.reg_patronal_imss

           FOREACH cur_movex2 INTO reg_cur2_tipo_movex1.*

           END FOREACH

           DELETE FROM e_fecha

           INSERT INTO e_fecha
           SELECT max(a.fecha_conversion)
           FROM   dis_cuenta97 a
           WHERE  a.folio     = reg_cur2_tipo_movex1.folio
           AND    a.nss       = reg_beta.nss

           INSERT INTO e_fecha
           SELECT max(a.fecha_conversion)
           FROM   dis_cuenta98 a
           WHERE  a.folio     = reg_cur2_tipo_movex1.folio
           AND    a.nss       = reg_beta.nss

           INSERT INTO e_fecha
           SELECT max(a.fecha_conversion)
           FROM   dis_cuenta a
           WHERE  a.folio     = reg_cur2_tipo_movex1.folio
           AND    a.nss       = reg_beta.nss

          SELECT MAX(a.fecha)
	  INTO  ex_fecha_conversion 
	  FROM  e_fecha  a

           SELECT a.fecha_creac_lote
           INTO   ex_fecha_creac
           FROM   exc_cza_exceso a
           WHERE  a.folio = reg_cur2_tipo_movex.folio

               LET reg_det_mov_trab.cont_serv     =
                   reg_cza_hist_trab.cont_serv

               LET reg_det_mov_trab.curp =
                   reg_cza_hist_trab.n_unico

               LET reg_det_mov_trab.nss_afore =
                   reg_cza_hist_trab.nss

               LET reg_det_mov_trab.iden_proc = "14"

               LET reg_det_mov_trab.consecutivo   =
                   reg_det_mov_trab.consecutivo + 1

               LET reg_det_mov_trab.iden_mov[1,11]   =
                   reg_cur2_tipo_movex.reg_patronal_imss

               LET reg_det_mov_trab.iden_mov[12,22] =
                   reg_beta.nss

               LET reg_det_mov_trab.iden_mov[23,28] =
                   reg_cur2_tipo_movex.periodo_pago

               LET reg_det_mov_trab.iden_mov[29,34] =
                   reg_cur2_tipo_movex.folio_pago_sua USING"&&&&&&"

               LET reg_det_mov_trab.iden_mov[35,42]  =
                   reg_cur2_tipo_movex.fecha_pago USING "YYYYMMDD"

               LET reg_det_mov_trab.iden_mov[43,50]  =
                   reg_cur2_tipo_movex.fecha_valor_viv USING"YYYYMMDD"

               LET reg_det_mov_trab.iden_mov[51,58]  =
                   reg_cur2_tipo_movex.fecha_valor_rcv USING"YYYYMMDD"

               LET reg_det_mov_trab.iden_mov[59,66]  =
                   ex_fecha_conversion USING"YYYYMMDD"

               LET reg_det_mov_trab.iden_mov[67,74]  =
                   ex_fecha_conversion USING"YYYYMMDD"

               LET reg_det_mov_trab.iden_mov[75,77] =
                   reg_cur2_tipo_movex.clave_ent_recep USING"&&&"

               LET reg_det_mov_trab.iden_mov[78,79] =
                   reg_cur2_tipo_movex.dias_cotz_bimestre USING"&&"

               LET reg_det_mov_trab.iden_mov[80,87] =
                   ex_fecha_creac USING"YYYYMMDD"

               LET reg_det_mov_trab.iden_mov[88,92] =
                   reg_cur2_tipo_movex.consec_reg_lote USING"&&&&&"

               LET reg_det_mov_trab.iden_mov[93,100] =
                   reg_beta.fecha_conversion USING"YYYYMMDD"

               LET reg_det_mov_trab.iden_mov[101,108] =
                   reg_beta.fecha_conversion  USING"YYYYMMDD"

               LET reg_det_mov_trab.siefore =  vv_siefore

               LET reg_det_mov_trab.fecha_valuacion =
                   reg_beta.fecha_conversion USING "YYYYMMDD"

               LET reg_det_mov_trab.acciones =
                   reg_beta.monto_en_acciones

               LET reg_det_mov_trab.precio_accion =
                   reg_beta.precio_accion

              IF reg_beta.monto_en_pesos < 0 THEN
                 LET reg_det_mov_trab.tipo_mov = "C"
              ELSE
                 LET reg_det_mov_trab.tipo_mov = "A"
              END IF

              CASE reg_beta.subcuenta

               WHEN 1
                  LET reg_det_mov_trab.subcuenta = "01"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion =
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 2

                  LET reg_det_mov_trab.subcuenta = "02"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion =
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 3

                  LET reg_det_mov_trab.subcuenta = "13"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion =
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 4

                  LET reg_det_mov_trab.subcuenta = "04"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion =
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 5
                  LET reg_det_mov_trab.subcuenta = "03"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion =
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 6

                  LET reg_det_mov_trab.subcuenta = "06"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion =
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 9

                  LET reg_det_mov_trab.subcuenta = "07"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion =
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

                OTHERWISE
                           EXIT CASE
                END CASE
END FUNCTION

FUNCTION tipo_unif()
#tmunif------------- 
DEFINE primer_dia DATE
DEFINE vma smallint
DEFINE vnss_cta1 char(011)
DEFINE vcve char(003)

               LET reg_det_mov_trab.iden_mov = " "

               LET reg_det_mov_trab.cont_serv     = 
                   reg_cza_hist_trab.cont_serv

               LET reg_det_mov_trab.curp = 
		   reg_cza_hist_trab.n_unico

               LET reg_det_mov_trab.nss_afore = 
		   reg_cza_hist_trab.nss

               LET reg_det_mov_trab.iden_proc = "08"

               LET reg_det_mov_trab.consecutivo   =
                   reg_det_mov_trab.consecutivo + 1

           IF (reg_beta.subcuenta = 4 OR
	      reg_beta.subcuenta = 8) THEN

               SELECT a.nss_cta1,a.cve_ent_cta1
	       INTO vnss_cta1,vcve
	       FROM   safre_af:uni_unificado a 
	       WHERE  a.nss_uni  = reg_beta.nss
	       AND    YEAR(a.fliquida)= YEAR(reg_beta.fecha_conversion)
	       AND    MONTH(a.fliquida)= MONTH(reg_beta.fecha_conversion)
               AND    a.estado = 100

            ELSE

               SELECT a.nss_cta1,a.cve_ent_cta1
	       INTO   vnss_cta1,vcve
	       FROM   safre_af:uni_unificado a 
	       WHERE  a.nss_uni  = reg_beta.nss
	       AND    a.fliquida = reg_beta.fecha_conversion
               AND    a.estado = 100

	    END IF

               SELECT a.marca_causa
	       INTO   vma
	       FROM   safre_af:cta_act_marca a
	       WHERE  a.nss = vnss_cta1
	       AND    a.marca_cod = 130
	       AND    a.marca_causa in (241,242,243,244)


               IF (vma = 241 OR vma = 242) THEN
                  LET reg_det_mov_trab.iden_mov[1,2]   = '01'
	       END IF 
               IF (vma = 243 OR vma = 244) THEN
                  LET reg_det_mov_trab.iden_mov[1,2]   = '02'
	       END IF 


               LET reg_det_mov_trab.iden_mov[3,10]   = reg_beta.fecha_conversion
               USING "YYYYMMDD"

               LET reg_det_mov_trab.iden_mov[11,13]   = c_cve_afore
               LET reg_det_mov_trab.iden_mov[14,16]   = c_cve_afore
	       IF (vma = 241 OR vma = 242) THEN
                  LET reg_det_mov_trab.iden_mov[17,19]   = c_cve_afore
	       END IF
	       IF (vma = 243 OR vma = 244) THEN
                  LET reg_det_mov_trab.iden_mov[17,19]   = vcve
	       END IF
                      IF reg_beta.monto_en_pesos >= 0 THEN 
                         LET reg_det_mov_trab.tipo_mov = "A"
                      ELSE 
                         LET reg_det_mov_trab.tipo_mov = "C"
                      END IF

                      LET reg_det_mov_trab.siefore =  vv_siefore
         
                      LET reg_det_mov_trab.fecha_valuacion  = 
                      reg_beta.fecha_conversion USING"YYYYMMDD"

                      LET reg_det_mov_trab.acciones = 
                          reg_beta.monto_en_acciones

                      LET reg_det_mov_trab.precio_accion = 
                          reg_beta.precio_accion

                      LET reg_det_mov_trab.fecha_proceso = 
                          TODAY 

              CASE reg_beta.subcuenta

               WHEN 1 
		 IF reg_beta.tipo_movimiento = 1 THEN
                  LET reg_det_mov_trab.subcuenta = "01"
		 END IF 
		 IF reg_beta.tipo_movimiento = 4 THEN
                  LET reg_det_mov_trab.subcuenta = "18"
		 END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 2 
		 IF reg_beta.tipo_movimiento = 1 THEN
                  LET reg_det_mov_trab.subcuenta = "02"
		 END IF 
		 IF reg_beta.tipo_movimiento = 4 THEN
                  LET reg_det_mov_trab.subcuenta = "20"
		 END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 3 
		 IF reg_beta.tipo_movimiento = 1 THEN
                  LET reg_det_mov_trab.subcuenta = "13"
		 END IF 
		 IF reg_beta.tipo_movimiento = 4 THEN
                  LET reg_det_mov_trab.subcuenta = "23"
		 END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 4 
		 IF reg_beta.tipo_movimiento = 1 THEN
                  LET reg_det_mov_trab.subcuenta = "04"
		 END IF 
		 IF reg_beta.tipo_movimiento = 4 THEN
                  LET reg_det_mov_trab.subcuenta = "12"
		 END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 5 
		 IF reg_beta.tipo_movimiento = 1 THEN
                  LET reg_det_mov_trab.subcuenta = "03"
		 END IF 
		 IF reg_beta.tipo_movimiento = 4 THEN
                  LET reg_det_mov_trab.subcuenta = "19"
		 END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
               WHEN 6 
		 IF reg_beta.tipo_movimiento = 1 THEN
                  LET reg_det_mov_trab.subcuenta = "06"
		 END IF 
		 IF reg_beta.tipo_movimiento = 4 THEN
                  LET reg_det_mov_trab.subcuenta = "10"
		 END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
               WHEN 7
		 IF reg_beta.tipo_movimiento = 1 THEN
                  LET reg_det_mov_trab.subcuenta = "08"
		 END IF 
		 IF reg_beta.tipo_movimiento = 4 THEN
                  LET reg_det_mov_trab.subcuenta = "24"
		 END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
               WHEN 8 
		 IF reg_beta.tipo_movimiento = 1 THEN
                  LET reg_det_mov_trab.subcuenta = "09"
		 END IF 
		 IF reg_beta.tipo_movimiento = 4 THEN
                  LET reg_det_mov_trab.subcuenta = "14"
		 END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
               WHEN 9 
		 IF reg_beta.tipo_movimiento = 1 THEN
                  LET reg_det_mov_trab.subcuenta = "07"
		 END IF 
		 IF reg_beta.tipo_movimiento = 4 THEN
                  LET reg_det_mov_trab.subcuenta = "11"
		 END IF
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
               WHEN 10
                  LET reg_det_mov_trab.subcuenta = "05"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
                OTHERWISE 
                  EXIT CASE
             END CASE

END FUNCTION
FUNCTION ajuste()
#tmunif---------- 

               LET reg_det_mov_trab.iden_mov = " "

               LET reg_det_mov_trab.cont_serv     = 
                   reg_cza_hist_trab.cont_serv

               LET reg_det_mov_trab.curp = 
		   reg_cza_hist_trab.n_unico

               LET reg_det_mov_trab.nss_afore = 
		   reg_cza_hist_trab.nss

               LET reg_det_mov_trab.iden_proc = "13"

               LET reg_det_mov_trab.consecutivo   =
                   reg_det_mov_trab.consecutivo + 1


                  LET reg_det_mov_trab.iden_mov[1,8]   = 
		      reg_beta.fecha_conversion USING"YYYYMMDD"

               CASE reg_beta.id_aportante[1,2]
	       WHEN "UN"
                 LET reg_det_mov_trab.iden_mov[9,10]   = '08' 
		 EXIT CASE
	       WHEN "UC"
                 LET reg_det_mov_trab.iden_mov[9,10]   = '08' 
		 EXIT CASE
	       OTHERWISE
                 LET reg_det_mov_trab.iden_mov[9,10]   = '08' 
		 EXIT CASE
	       END CASE

                      IF reg_beta.monto_en_pesos >= 0 THEN 
                         LET reg_det_mov_trab.tipo_mov = "A"
                      ELSE 
                         LET reg_det_mov_trab.tipo_mov = "C"
                      END IF

                      LET reg_det_mov_trab.siefore =  vv_siefore
         
                      LET reg_det_mov_trab.fecha_valuacion  = 
                      reg_beta.fecha_conversion USING"YYYYMMDD"

                      LET reg_det_mov_trab.acciones = 
                          reg_beta.monto_en_acciones

                      LET reg_det_mov_trab.precio_accion = 
                          reg_beta.precio_accion

                      LET reg_det_mov_trab.fecha_proceso = 
                          TODAY 

              CASE reg_beta.subcuenta

               WHEN 1 
                  LET reg_det_mov_trab.subcuenta = "01"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 2 
                  LET reg_det_mov_trab.subcuenta = "02"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 3 
                  LET reg_det_mov_trab.subcuenta = "13"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 4 
                  LET reg_det_mov_trab.subcuenta = "04"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE

               WHEN 5 
                  LET reg_det_mov_trab.subcuenta = "03"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
               WHEN 6 
                  LET reg_det_mov_trab.subcuenta = "06"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
               WHEN 7
                  LET reg_det_mov_trab.subcuenta = "08"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
               WHEN 8 
                  LET reg_det_mov_trab.subcuenta = "09"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
               WHEN 9 
                  LET reg_det_mov_trab.subcuenta = "07"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
               WHEN 10
                  LET reg_det_mov_trab.subcuenta = "05"
                  LET reg_det_mov_trab.importe =
                      reg_beta.monto_en_pesos 
                       LET reg_det_mov_trab.fecha_conversion = 
                           reg_beta.fecha_conversion
                  INSERT INTO safre_tmp:det_mov_trab values (reg_det_mov_trab.*)
                  EXIT CASE
                OTHERWISE 
                  EXIT CASE
             END CASE

END FUNCTION
