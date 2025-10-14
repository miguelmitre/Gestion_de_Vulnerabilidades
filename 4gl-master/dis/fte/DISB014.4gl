################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#                  => E.F.P.                                                   #
#Programa DISB014  => RESPUESTA A PROCESAR INTERESES EN TRANSITO               #
#Fecha             => 15 diciembre 1998.                                       #
#By                => GERARDO ALFONSO VEGA PAREDES.                            #
#Sistema           => DIS.     	                                               #
#Modifico          => FRANCISCO JAVIER LARIOS HERNANDEZ.                       #
#Fecha             => 03/JUNIO/2003.                                           #
#                  => SE INTEGRO LOS INTERESES EN TRANSITO PARA GOBIERNO.      #
#Modifico          => FRANCISCO JAVIER LARIOS HERNANDEZ.                       #
#Fecha             => 25/AGOSTO/2003.                                          #
#                  => SE MODIFICO POR CAMBIO DE LAYOUT POR ELIMINACION DE RE-  #
#                  => GISTRO 03.                                               #
--------------------------------------------------------------------------------
#Fecha modif       => 02 diciembre 2004                                        #
#Autor             => Alejandro Ramirez                                        #
#Descripcion       => Adecuación layout (032701) (032702) (032709)             #
#                     de acuerdo a circular 22-6 multi siefore,                #
#                     utilizacion de preparados para utilizar multisiefore     #
#                     para intereses en transito                               #
--------------------------------------------------------------------------------
#FEcha modifi      => 13 diciembre 2004                                        #
#Autor             => GERARDO ALFONSO VEGA PAREDES                             #
#Descripcion       => Generar archivo con multiples siefore                    #
--------------------------------------------------------------------------------
#FEcha modifi      => 17 May 2005                                              #
#Autor             => Alejandro Ramirez                                        #
#Descripcion       => (v2) Se agrego al query que obtiene los folios el conse. #
#                  => del lote para evitar duplicidad en la busqueda.          #
--------------------------------------------------------------------------------
#FEcha modifi      => 6 de Jul 2005                                            #
#Autor             => Alejandro Ramirez                                        #
#Descripcion       => (v3) Trunco las cantidades en el sumario por dif 1 cent. #
--------------------------------------------------------------------------------
#FEcha modifi      =>18 de Jul 2005                                            #
#Autor             => Alejandro Ramirez                                        #
#Descripcion       => (v4) Como hay descuadre del detall vs el sumario obtengo #
#                  => la suma del detalle                                      #
#                  => (v5) Si se corre mas de una vez no llama a funcion de    #
#                  => Genera_reg_nvo()                                         #
################################################################################
DATABASE safre_af

GLOBALS

   DEFINE vident_operacion CHAR(2),
          g_param_dis      RECORD LIKE seg_modulo.* ,
          vcodigo_afore    LIKE tab_afore_local.codigo_afore 

   DEFINE reg_cza RECORD
      tipo_registro     CHAR(02),
      ident_servicio    CHAR(02),
      ident_operacion   CHAR(02),
      tipo_ent_origen   CHAR(02),
      clave_ent_origen  CHAR(03),
      tipo_ent_destino  CHAR(02),
      clave_ent_destino CHAR(03),
      fecha_creac_lote  DATE,
      lote_del_dia      CHAR(03),
      mod_recp_envio    CHAR(02),
      fecha_limite_resp DATE,
      fecha_ini_notifi  DATE,
      fecha_fin_notifi  DATE,
      fecha_liq_interes DATE,
      result_operacion  CHAR(02),
      diag1             CHAR(03),
      diag2             CHAR(03),
      diag3             CHAR(03) 
   END RECORD

   DEFINE regh RECORD LIKE cta_det_transito.*   --c22-113
   DEFINE vsiefore2     CHAR(08)                --c22-113

   DEFINE reg_det RECORD
      fecha_creac_lote  DATE,
      tipo_registro     CHAR(02),
      ident_servicio    CHAR(02),
      ident_pago        CHAR(16),
      consec_reg_lote   CHAR(03),
      fecha_gen_archivo DATE,
      impt_liq_rcv      DECIMAL(15,2),
      num_aport_envia   CHAR(08),
      fecha_pago        DATE,
      impt_int_cal_pro  DECIMAL(15,2),
   -- impt_int_cal_afo  DECIMAL(15,2),
      impt_int_cal_afo  FLOAT,          --bueno 
      ident_int         CHAR(01),
      ident_siefore     CHAR(08),   --c22-6
      tipo_siefore      SMALLINT,   --c22-6
      result_operacion  CHAR(02),  
      diag1             CHAR(03),
      diag2             CHAR(03),
      diag3             CHAR(03)
   END RECORD

   DEFINE reg_sum RECORD
      tipo_registro      CHAR(02),
      ident_servicio     CHAR(02),
      ident_operacion    CHAR(02),
      tipo_ent_origen    CHAR(02),
      clave_ent_origen   CHAR(03),
      tipo_ent_destino   CHAR(02),
      clave_ent_destino  CHAR(03),
      fecha_creac_lote   DATE,
      lote_del_dia       CHAR(03),
      num_reg_ctas_xpag  CHAR(06),
      impt_liq_rcv       DECIMAL(15,2),
      impt_total_int_rcv DECIMAL(15,2),
      impt_total_com_rcv DECIMAL(15,2),
      impt_liq_rcv_acl   DECIMAL(15,2),
      impt_int_rcv_acl   DECIMAL(15,2),
      impt_liq_vol       DECIMAL(15,2),  --c22-6
      impt_int_vol       DECIMAL(15,2),  --c22-6
      impt_liq_vol_acl   DECIMAL(15,2),  --c22-6
      impt_int_vol_acl   DECIMAL(15,2),  --c22-6
      impt_liq_com       DECIMAL(15,2),  --c22-6
      impt_int_com       DECIMAL(15,2),  --c22-6
      impt_liq_com_acl   DECIMAL(15,2),  --c22-6
      impt_int_com_acl   DECIMAL(15,2),  --c22-6
      impt_liq_lplazo    DECIMAL(15,2),  --c22-11
      impt_int_lplazo    DECIMAL(15,2),  --c22-11
      impt_liq_lpzo_acl  DECIMAL(15,2),  --c22-11
      impt_int_lpzo_acl  DECIMAL(15,2),  --c22-11
      impt_liq_subadi    DECIMAL(15,2),  --c22-11
      impt_int_subadi    DECIMAL(15,2),  --c22-11
      impt_liq_suba_acl  DECIMAL(15,2),  --c22-11
      impt_int_suba_acl  DECIMAL(15,2),  --c22-11
      impt_liq_gob       DECIMAL(15,2),
      impt_total_int_gob DECIMAL(15,2),
      impt_total_xpag    DECIMAL(15,2),
      impt_total_vol     DECIMAL(15,2),  --c22-6
      impt_total_com     DECIMAL(15,2),  --c22-6
      impt_total_lplazo  DECIMAL(15,2),  --c22-11
      impt_total_subadi  DECIMAL(15,2),  --c22-11
      impt_tot_xpag_gob  DECIMAL(15,2)
   END RECORD

   DEFINE reg_int RECORD
      folio   INTEGER,
      interes DECIMAL(15,2)
   END RECORD

   DEFINE 
      hoy       DATE,
      opc       CHAR(01),
      vcomando  CHAR(01),
      generar   CHAR(20),
      comando   CHAR(200),
      carga_reg CHAR(360),  --c22-6  antes 295
      cla_where CHAR(200),
      cla_sel   CHAR(300)
     
   DEFINE
      vfecha_lote        DATE,
      vfecha_aux_lote    CHAR(10),
      vfecha_aux_lote2   CHAR(08),
      vfecha_aux         CHAR(08),
      vfecha_creac_lote  CHAR(10),
      vfecha_limite_resp CHAR(10),
      vfecha_ini_notifi  CHAR(10),
      vfecha_fin_notifi  CHAR(10),
      vfecha_liq_interes CHAR(10),
      vfecha_gen_archivo CHAR(10),
      vfecha_pago        CHAR(10),
      folios             INTEGER,
      vsalida            CHAR(100),
      vfec_creac_lote    CHAR(08),
      vfec_limite_resp   CHAR(08),
      vfec_ini_notifi    CHAR(08),
      vfec_fin_notifi    CHAR(08),
      vfec_liq_interes   CHAR(08),
      vfec_gen_archivo   CHAR(08),
      vfec_pago          CHAR(08),
      vimpt_liq_rcv      CHAR(15),
      vimpt_liq_rcv_acl  CHAR(15),
      vimpt_liq_gob      CHAR(15),
      vimpt_int_cal_pro  CHAR(15),
      vimpt_int_cal_afo  CHAR(15),
      vtot_int_rcv       DECIMAL(15,2),
      vtot_int_rcv_acl   DECIMAL(15,2),
      vtot_int_vol       DECIMAL(15,2),              --c22-6
      vtot_int_vol_acl   DECIMAL(15,2),              --c22-6
      vtot_int_com       DECIMAL(15,2),              --c22-6
      vtot_int_com_acl   DECIMAL(15,2),              --c22-6
      vtot_int_lpzo      DECIMAL(15,2),              --c22-11
      vtot_int_lpzo_acl  DECIMAL(15,2),              --c22-11
      vtot_int_sadi      DECIMAL(15,2),              --c22-11
      vtot_int_sadi_acl  DECIMAL(15,2),              --c22-11

      vtot_gob           DECIMAL(15,2),
      vimpt_total_int_rcv CHAR(15),
      vimpt_total_com_rcv CHAR(15),

      vimpt_int_rcv_acl   CHAR(15),
      vimpt_liq_vol       CHAR(15),       --c22-6
      vimpt_int_vol       CHAR(15),       --c22-6
      vimpt_liq_vol_acl   CHAR(15),       --c22-6
      vimpt_int_vol_acl   CHAR(15),       --c22-6
      vimpt_liq_com       CHAR(15),       --c22-6
      vimpt_int_com       CHAR(15),       --c22-6
      vimpt_liq_com_acl   CHAR(15),       --c22-6
      vimpt_int_com_acl   CHAR(15),       --c22-6
      vimpt_total_int_gob CHAR(15),
      vimpt_total_xpag    CHAR(15),
      vimpt_total_apor_vol CHAR(15),      --v4
      vimpt_total_vol     CHAR(15),       --c22-6
      vimpt_total_com     CHAR(15),       --c22-6
      vimpt_total_lplazo  CHAR(15),       --c22-11
      vimpt_total_subadi  CHAR(15),       --c22-11
                         
      vimpt_tot_xpag_gob  CHAR(15),
      vfolio              INTEGER,
      tipo_ident          SMALLINT

   DEFINE vident_pago CHAR(02)

   DEFINE vsuma DECIMAL(16,6),      --c22-6.2
          vpor  DECIMAL(16,8),      --c22-6.2
          vsie  SMALLINT,           --c22-6.2
          vsub  SMALLINT,           --c22-6.2
          vsubd CHAR(02)            --c22-6.2

   DEFINE vident_siefore CHAR(08),
          vtipo_siefore  SMALLINT,
          vcontador_det  SMALLINT

    DEFINE enter CHAR(1)

END GLOBALS

MAIN
   OPTIONS 
      INPUT WRAP,
      PROMPT LINE LAST  ,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT

   CALL STARTLOG("DISB014.log")   --v2
   CALL init()
   CALL captura()

   ERROR "GENERANDO PROCESO"
   CALL genera_cza()
   CALL genera_det()
   CALL genera_sum()
   CALL genera_archivo()

   ERROR "PROCESO TERMINADO"
END MAIN

FUNCTION init()
   INITIALIZE reg_cza.*  TO NULL
   INITIALIZE reg_det.*  TO NULL
   INITIALIZE reg_sum.*  TO NULL
   LET hoy               = TODAY

   SELECT codigo_afore
   INTO   vcodigo_afore
   FROM   tab_afore_local

   SELECT * 
   INTO   g_param_dis.*
   FROM   seg_modulo
   WHERE   modulo_cod = 'dis'

END FUNCTION

FUNCTION captura()
   OPEN WINDOW ven1 AT 3,2 WITH FORM "DISB0141" ATTRIBUTE(BORDER) 
   DISPLAY " DISB014           RESPUESTA INTERESES RCV EN TRANSITO                         " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "dd-mm-yyyy" AT 2,65 ATTRIBUTE(REVERSE)            

   INPUT BY NAME vfecha_lote
      AFTER FIELD vfecha_lote
         PROMPT "Es correcta la fecha [S/N] ... " FOR opc
         IF opc MATCHES '[Ss]' THEN
            LET vcomando = 2
            EXIT INPUT
         ELSE
            NEXT FIELD vfecha_lote
         END IF
      ON KEY (INTERRUPT)                   
         LET vcomando =  1                    
         LET INT_FLAG = FALSE
         EXIT INPUT                    
   END INPUT

   IF vcomando = 1 THEN         
      ERROR "Operacion abortada"
      EXIT PROGRAM              
   END IF                       
END FUNCTION

FUNCTION genera_cza()
   SELECT * 
   INTO   reg_cza.*
   FROM   cta_cza_transito
   WHERE  fecha_creac_lote = vfecha_lote

   LET reg_cza.tipo_registro     = "01"
   LET reg_cza.ident_servicio    = "03"
   LET reg_cza.ident_operacion   = "27"
   LET reg_cza.tipo_ent_origen   = "01"
   LET reg_cza.clave_ent_origen  = vcodigo_afore CLIPPED --c22-6
   LET reg_cza.tipo_ent_destino  = "03"
   LET reg_cza.clave_ent_destino = "001"
   LET reg_cza.mod_recp_envio    = "02"
   LET vsalida = g_param_dis.ruta_envio CLIPPED,"/cza_rcv"

   START REPORT rep_cza TO vsalida
      LET vfecha_creac_lote  = reg_cza.fecha_creac_lote
      LET vfecha_limite_resp = reg_cza.fecha_limite_resp
      LET vfecha_ini_notifi  = reg_cza.fecha_ini_notifi
      LET vfecha_fin_notifi  = reg_cza.fecha_fin_notifi
      LET vfecha_liq_interes = reg_cza.fecha_liq_interes
      LET vfec_creac_lote  = vfecha_creac_lote[7,10],
                             vfecha_creac_lote[1,2],
                             vfecha_creac_lote[4,5]
      LET vfec_limite_resp = vfecha_limite_resp[7,10],
                             vfecha_limite_resp[1,2],
                             vfecha_limite_resp[4,5]
      LET vfec_ini_notifi  = vfecha_ini_notifi[7,10],
                             vfecha_ini_notifi[1,2],
                             vfecha_ini_notifi[4,5]
      LET vfec_fin_notifi  = vfecha_fin_notifi[7,10],
                             vfecha_fin_notifi[1,2],
                             vfecha_fin_notifi[4,5]
      LET vfec_liq_interes = vfecha_liq_interes[7,10],
                             vfecha_liq_interes[1,2],
                             vfecha_liq_interes[4,5]
      OUTPUT TO REPORT rep_cza(reg_cza.*)
   FINISH REPORT rep_cza

END FUNCTION

REPORT rep_cza(reg_cza)
   DEFINE reg_cza RECORD
      tipo_registro     CHAR(02),
      ident_servicio    CHAR(02),
      ident_operacion   CHAR(02),
      tipo_ent_origen   CHAR(02),
      clave_ent_origen  CHAR(03),
      tipo_ent_destino  CHAR(02),
      clave_ent_destino CHAR(03),
      fecha_creac_lote  DATE,
      lote_del_dia      CHAR(03),
      mod_recp_envio    CHAR(02),
      fecha_limite_resp DATE,
      fecha_ini_notifi  DATE,
      fecha_fin_notifi  DATE,
      fecha_liq_interes DATE,
      result_operacion  CHAR(02),
      diag1             CHAR(03),
      diag2             CHAR(03),
      diag3             CHAR(03) 
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
                         reg_cza.clave_ent_origen,  
                         reg_cza.tipo_ent_destino, 
                         reg_cza.clave_ent_destino, 
                         vfec_creac_lote,
                         reg_cza.lote_del_dia,      
                         reg_cza.mod_recp_envio,    
                         vfec_limite_resp,
                         vfec_ini_notifi,
                         vfec_fin_notifi,
                         vfec_liq_interes,
                         reg_cza.result_operacion,  
                         reg_cza.diag1,             
                         reg_cza.diag2,             
                         reg_cza.diag3,
                         408 SPACES   --c22-11
END REPORT

FUNCTION genera_det()

   LET vtot_int_vol     = 0
   LET vtot_int_vol_acl = 0  
   LET vtot_int_com     = 0  
   LET vtot_int_com_acl = 0  
   LET vtot_int_lpzo    = 0            --c22-11
   LET vtot_int_lpzo_acl= 0            --c22-11
   LET vtot_int_sadi    = 0            --c22-11
   LET vtot_int_sadi_acl= 0            --c22-11

   LET vtot_int_rcv = 0
   LET vtot_int_rcv_acl = 0
   LET vtot_gob = 0

   WHENEVER ERROR CONTINUE                                              --v5
   DROP TABLE archivos_dis_bat2                                         --v5
   CREATE TEMP TABLE archivos_dis_bat2                                  --v5
   (campo  CHAR(40))                                                    --v5
   WHENEVER ERROR STOP                                                  --v5

   LET comando = "cd ",g_param_dis.ruta_envio CLIPPED,"/","; ls ",vfec_creac_lote,".TRAN > checa_arch" CLIPPED                                          --v5

   RUN comando                                                          --v5
   LET  comando = g_param_dis.ruta_envio CLIPPED,"/checa_arch" CLIPPED  --v5
   LOAD FROM comando INSERT INTO archivos_dis_bat2                      --v5

   SELECT "X"                                                           --v5
   FROM archivos_dis_bat2                                               --v5
   IF STATUS = NOTFOUND THEN                                            --v5
      CALL Genera_reg_nvo()                                             --v5
   END IF                                                               --v5

   LET vsalida = g_param_dis.ruta_envio CLIPPED,"/det_rcv"
   LET vcontador_det = 0

   ---INGRESO LA UN REGISTRO EN BLANCO DE SIEFORE 2 PARA
   ---LOS REGISTROS DE COMISIONES POR INTERESES (AUNQUE NO SE CONSIDEREN)

   SELECT razon_social                                                --c22-113
   INTO   vsiefore2                                                   --c22-113
   FROM   tab_siefore_local                                           --c22-113
   WHERE  codigo_siefore = 2                                          --c22-113

   select unique("x")                                          #060616
   from cta_det_transito                                       #060616
   where ident_pago[14,15] in ('61','62','66')                 #060616
   and   fecha_creac_lote = vfecha_lote                        #060616
   and   tipo_siefore = 2                                      #060616
   if  STATUS = NOTFOUND then                                  #060616
     SELECT * FROM cta_det_transito                            --c22-113
     WHERE ident_pago[14,15] in ('61','62','66')               --c22-113
     AND   fecha_creac_lote = vfecha_lote                      --c22-113
     INTO  TEMP tmp_int_comision                               --c22-113


     DECLARE c23 CURSOR FOR                                    --c22-113
     SELECT * FROM tmp_int_comision                            --c22-113
     FOREACH c23 INTO  regh.*                                  --c22-113
        let regh.impt_liq_rcv        = 0.00                     #060616
        let regh.num_aport_envia     = "00000000"               #060616
        let regh.impt_int_cal_pro    = 0.00                     #060616
        let regh.impt_int_cal_afo    = 0.00                     #060616

        LET regh.tipo_siefore = 2                             --c22-113
        LET regh.ident_siefore = vsiefore2                    --c22-113

        INSERT INTO cta_det_transito                          --c22-113
        VALUES(regh.*)                                        --c22-113

     END FOREACH                                               --c22-113
   end if


   DECLARE curdet CURSOR FOR
   SELECT *
   INTO   reg_det.*
   FROM   cta_det_transito
   WHERE  fecha_creac_lote = vfecha_lote

   START REPORT rep_rcv TO vsalida
   FOREACH curdet INTO reg_det.*

      LET vcontador_det = vcontador_det + 1
     
      LET tipo_ident = reg_det.ident_int

      CASE tipo_ident
       WHEN  '1'                                -- RCV Ordinario  
          OUTPUT TO REPORT rep_rcv(reg_det.*,1)
       WHEN '2'                                 -- RCV Aclaracion Ordinario
          OUTPUT TO REPORT rep_rcv(reg_det.*,2)
       WHEN '3'                                 -- Gubernamentales 
          OUTPUT TO REPORT rep_rcv(reg_det.*,3) 
       OTHERWISE 
          OUTPUT TO REPORT rep_rcv(reg_det.*,0)
      END CASE
       
   END FOREACH
   FINISH REPORT rep_rcv

END FUNCTION

FUNCTION Genera_reg_nvo()
   DEFINE r_det RECORD
      fecha_creac_lote  DATE,
      tipo_registro     CHAR(02),
      ident_servicio    CHAR(02),
      ident_pago        CHAR(16),
      consec_reg_lote   CHAR(03),
      fecha_gen_archivo DATE,
      impt_liq_rcv      DECIMAL(15,2),
      num_aport_envia   CHAR(08),
      fecha_pago        DATE,
      impt_int_cal_pro  DECIMAL(15,2),
    --impt_int_cal_afo  DECIMAL(15,2),
      impt_int_cal_afo  FLOAT,          --bueno
      ident_int         CHAR(01),
      ident_siefore     CHAR(08),   --c22-6
      tipo_siefore      SMALLINT,   --c22-6
      result_operacion  CHAR(02),
      diag1             CHAR(03),
      diag2             CHAR(03),
      diag3             CHAR(03)
   END RECORD
   
   DEFINE xfecha_gen_archivo CHAR(10),
          xfec_gen_archivo   CHAR(08)

   DEFINE xfolio           INTEGER

   DEFINE xsie      SMALLINT,
          xid_pago  CHAR(16),
          xid_pagoc CHAR(14),
       -- vinteres  DECIMAL(15,2)
          vinteres  FLOAT         --bueno 

   DEFINE vcontador_sie SMALLINT

   DECLARE cdet CURSOR FOR
   SELECT *
   FROM   cta_det_transito
   WHERE  fecha_creac_lote = vfecha_lote
   AND    ident_pago[14] = "5"

   FOREACH cdet INTO r_det.*
      LET xfecha_gen_archivo = r_det.fecha_gen_archivo
      LET xfec_gen_archivo = xfecha_gen_archivo[7,10],
                             xfecha_gen_archivo[1,2],
                             xfecha_gen_archivo[4,5]

      LET xfolio = 0  --c22-113

      SELECT folio
      INTO   xfolio
      FROM   dis_cza_aporte
      WHERE  fech_creac_lote = xfec_gen_archivo
      AND    lote_del_dia    = r_det.consec_reg_lote   --v2 
      IF xfolio IS NULL THEN
         LET xfolio = 0
      END IF

      IF r_det.ident_pago[15] = "1" THEN   --- INTERES RCV
         LET cla_sel = "SELECT idp,",
                              "sie,",
                              "ROUND(sum(mto),2) ", --v3ojo
                       "FROM   safre_tmp:tmp_interes_sub  ",  
                       "WHERE  fol = ",xfolio,
                      " AND   sub in (1,2,17,15) ",  --c22-11
                      " GROUP  BY 1,2 ",
                      " ORDER  BY 2,1 "
      END IF
 
      IF r_det.ident_pago[15] = "2" THEN    ---INTERES EST 
         LET cla_sel = "SELECT idp,",
                              "sie,",
                              "ROUND(sum(mto),2) ",
                       "FROM   safre_tmp:tmp_interes_sub  ",
                       "WHERE  fol = ",xfolio,
                      " AND    sub in (5,6,9) ",
                      " GROUP  BY 1,2 ",
                      " ORDER  BY 2,1 "
      END IF

      IF r_det.ident_pago[15] = "6" THEN    --- INTERES ACR    --c22-112
         LET cla_sel = "SELECT idp,",
                              "sie,",
                              "ROUND(sum(mto),2) ",
                       "FROM   safre_tmp:tmp_interes_sub  ",
                       "WHERE  fol = ",xfolio,
                      " AND    sub = 11 ",
                      " GROUP  BY 1,2 ",
                      " ORDER  BY 2,1 "
      END IF

      IF r_det.ident_pago[15] = "5" THEN    --- INTERES VOL
         LET cla_sel = "SELECT idp,",
                              "sie,",
                              "ROUND(sum(mto),2) ",
                       "FROM   safre_tmp:tmp_interes_sub  ",
                       "WHERE  fol = ",xfolio,
                      " AND    sub IN (3)    ",
                      " GROUP  BY 1,2 ",
                      " ORDER  BY 2,1 "
      END IF

      LET vcontador_sie = 0
 
      PREPARE claexe2 FROM cla_sel
      DECLARE cdet2 CURSOR FOR claexe2
 
      FOREACH cdet2 INTO xid_pago,xsie,vinteres
 
         LET vcontador_sie = vcontador_sie + 1
 
         SELECT razon_social
         INTO   r_det.ident_siefore
         FROM   tab_siefore_local
         WHERE  codigo_siefore = xsie
 
         LET r_det.tipo_siefore = xsie
  
         IF vcontador_sie = 1 THEN
 
            UPDATE cta_det_transito
            SET    impt_int_cal_afo = vinteres,
                   ident_siefore    = r_det.ident_siefore,
                   tipo_siefore     = r_det.tipo_siefore
            WHERE  fecha_creac_lote  = vfecha_lote
            AND    fecha_gen_archivo = r_det.fecha_gen_archivo
            AND    tipo_siefore      = 0
            AND    ident_pago        = xid_pago

            LET xid_pagoc = xid_pago[1,13],"6" CLIPPED

            UPDATE cta_det_transito
            SET    ident_siefore = r_det.ident_siefore,
                   tipo_siefore  = r_det.tipo_siefore
            WHERE  ident_pago[1,14] = xid_pagoc
            AND    impt_int_cal_pro = 0
            AND    tipo_siefore     = 0
 
         ELSE
           
            INSERT INTO cta_det_transito VALUES
               (
               r_det.fecha_creac_lote,  
               r_det.tipo_registro,     
               r_det.ident_servicio,    
               r_det.ident_pago,        
               r_det.consec_reg_lote,   
               r_det.fecha_gen_archivo, 
               r_det.impt_liq_rcv,      
               r_det.num_aport_envia,   
               r_det.fecha_pago,        
               r_det.impt_int_cal_pro,  
               vinteres,
               r_det.ident_int,         
               r_det.ident_siefore,
               r_det.tipo_siefore,      
               r_det.result_operacion,
               r_det.diag1,
               r_det.diag2,
               r_det.diag3
               )

         END IF

      END FOREACH

   END FOREACH

END FUNCTION

REPORT rep_rcv(reg_det,tipo_int)
   DEFINE reg_det RECORD
      fecha_creac_lote  DATE,
      tipo_registro     CHAR(02),
      ident_servicio    CHAR(02),
      ident_pago        CHAR(16),
      consec_reg_lote   CHAR(03),
      fecha_gen_archivo DATE,
      impt_liq_rcv      DECIMAL(15,2),
      num_aport_envia   CHAR(08),
      fecha_pago        DATE,
      impt_int_cal_pro  DECIMAL(15,2),
   -- impt_int_cal_afo  DECIMAL(15,2),
      impt_int_cal_afo  FLOAT,        --bueno 
      ident_int         CHAR(01),	
      ident_siefore     CHAR(08),  --c22-6
      tipo_siefore      SMALLINT,  --c22-6
      result_operacion  CHAR(02),  
      diag1             CHAR(03),
      diag2             CHAR(03),
      diag3             CHAR(03)
   END RECORD,

   tipo_int          SMALLINT,
   vimpt_liq_rcv_det DECIMAL(17,2)

   DEFINE reg_int RECORD
      folio    INTEGER,
      interes  DECIMAL(15,2)
   END RECORD

   DEFINE vrazon CHAR(08)          --c22-6 
   DEFINE vcod_siefore SMALLINT    --c22-6

   DEFINE vimpt_liq_rcv DECIMAL(16,6),         --c22-6.2
          vint_cal_pro  DECIMAL(16,6),         --c22-6.2
          vcontador     SMALLINT,              --c22-6.2
          vsub_ant      ARRAY[50] OF SMALLINT, --c22-6.2
          i             SMALLINT               --c22-6.2

   OUTPUT            
      LEFT MARGIN 0  
      RIGHT MARGIN 0 
      TOP MARGIN 0   
      BOTTOM MARGIN 0
      PAGE LENGTH 1  
   FORMAT            
      ON EVERY ROW   
         LET reg_det.tipo_registro  = "02"
         LET reg_det.ident_servicio = "03"
         LET vfecha_gen_archivo     = reg_det.fecha_gen_archivo
         LET vfec_gen_archivo       = vfecha_gen_archivo[7,10],
                                      vfecha_gen_archivo[1,2],
                                      vfecha_gen_archivo[4,5]
         LET vfecha_pago            = reg_det.fecha_pago
         LET vfec_pago              = vfecha_pago[7,10],
                                      vfecha_pago[1,2],
                                      vfecha_pago[4,5]


         LET reg_det.impt_liq_rcv     = reg_det.impt_liq_rcv * 100    --c22-6.2
         LET reg_det.impt_int_cal_pro = reg_det.impt_int_cal_pro * 100--c22-6.2
         LET reg_det.impt_int_cal_afo = reg_det.impt_int_cal_afo * 100--c22-6.2

         PRINT COLUMN 1, reg_det.tipo_registro,
                         reg_det.ident_servicio,    
                         reg_det.ident_pago,        
                         reg_det.consec_reg_lote,   
                         vfec_gen_archivo, 
                         reg_det.impt_liq_rcv     USING '&&&&&&&&&&&&&&&',
                         reg_det.num_aport_envia,   
                         vfec_pago,        
                         reg_det.impt_int_cal_pro USING '&&&&&&&&&&&&&&&',
                         reg_det.impt_int_cal_afo USING '&&&&&&&&&&&&&&&',
                         reg_det.ident_int,
                         reg_det.ident_siefore,
                         reg_det.tipo_siefore     USING '&&&',   --c22-6
                         376 SPACES                              --c22-11

END REPORT

FUNCTION genera_sum()

   SELECT * 
   INTO   reg_sum.*
   FROM   cta_sum_transito
   WHERE  fecha_creac_lote = vfecha_lote

   LET reg_sum.tipo_registro     = "09"
   LET reg_sum.ident_servicio    = "03"
   LET reg_sum.ident_operacion   = "27"
   LET reg_sum.tipo_ent_origen   = "01"
   LET reg_sum.clave_ent_origen  = vcodigo_afore CLIPPED  --c22-6 
   LET reg_sum.tipo_ent_destino  = "03"
   LET reg_sum.clave_ent_destino = "001"

   --------------------- PARA EL SUMARIO -----------------------------
         -- INTERES RCV ORDINARIO

         SELECT ROUND(SUM(mto),2)   --v3
         INTO   vtot_int_rcv
         FROM   safre_tmp:tmp_interes_sub
         WHERE  ide = 1
         AND    sub in (1,2)
         IF vtot_int_rcv IS NULL THEN
            LET vtot_int_rcv = 0
         END IF

         SELECT ROUND(SUM(mto),2)   --v3
         INTO   vtot_int_vol
         FROM   safre_tmp:tmp_interes_sub
         WHERE  ide = 1
         AND    sub = 3
         IF vtot_int_vol IS NULL THEN
            LET vtot_int_vol = 0
         END IF
  
{                                   --c22-114
         --(id21)
         SELECT ROUND(SUM(mto),2)   --v3
         INTO   vtot_int_com
         FROM   safre_tmp:tmp_interes_sub
         WHERE  ide = 1
         AND    sub = 11
}

select sum(impt_int_cal_afo)        --c22-114
INTO    vtot_int_com
from    cta_det_transito
where   fecha_creac_lote = vfecha_lote
and ident_pago[14,15]='56'
and ident_int=1

         IF vtot_int_com IS NULL THEN
            LET vtot_int_com = 0
         END IF




         --(id25)
         SELECT ROUND(SUM(mto),2)          --c22-11
         INTO   vtot_int_lpzo              --c22-11
         FROM   safre_tmp:tmp_interes_sub  --c22-11
         WHERE  ide = 1                    --c22-11
         AND    sub = 15                   --c22-11
         IF vtot_int_lpzo IS NULL THEN     --c22-11
            LET vtot_int_lpzo = 0          --c22-11
         END IF                            --c22-11

         --(id29)
         SELECT ROUND(SUM(mto),2)          --c22-11
         INTO   vtot_int_sadi              --c22-11
         FROM   safre_tmp:tmp_interes_sub  --c22-11
         WHERE  ide = 1                    --c22-11
         AND    sub = 17                   --c22-11
         IF vtot_int_sadi IS NULL THEN     --c22-11
            LET vtot_int_sadi = 0          --c22-11
         END IF                            --c22-11


        -- INTERES RCV ACLARACION ORDINARIA

         SELECT ROUND(SUM(mto),2)   --v3
         INTO   vtot_int_rcv_acl
         FROM   safre_tmp:tmp_interes_sub
         WHERE  ide = 2
         AND    sub in (1,2)
         IF vtot_int_rcv_acl IS NULL THEN
            LET vtot_int_rcv_acl = 0
         END IF

         SELECT ROUND(SUM(mto),2)  --v3
         INTO   vtot_int_vol_acl
         FROM   safre_tmp:tmp_interes_sub
         WHERE  ide = 2
         AND    sub = 3
         IF vtot_int_vol_acl IS NULL THEN
            LET vtot_int_vol_acl = 0
         END IF
   
         --(id23)
{                                   --c22-115
         SELECT ROUND(SUM(mto),2)   --v3
         INTO   vtot_int_com_acl
         FROM   safre_tmp:tmp_interes_sub
         WHERE  ide = 2
         AND    sub = 11
}

select sum(impt_int_cal_afo)        --c22-115
INTO    vtot_int_com_acl
from    cta_det_transito
where   fecha_creac_lote = vfecha_lote
and ident_pago[14,15]='56'
and ident_int=2

         IF vtot_int_com_acl IS NULL THEN
            LET vtot_int_com_acl = 0
         END IF

         --(id27)
         -- SUB LARGO PLAZO
         SELECT ROUND(SUM(mto),2)          --c22-11
         INTO   vtot_int_lpzo_acl          --c22-11
         FROM   safre_tmp:tmp_interes_sub  --c22-11
         WHERE  ide = 3                    --c22-11
         AND    sub = 15                   --c22-11
         IF vtot_int_lpzo_acl IS NULL THEN --c22-11
            LET vtot_int_lpzo_acl = 0      --c22-11
         END IF                            --c22-11

         --(id31)
         -- SUB ADICIONAL  
         SELECT ROUND(SUM(mto),2)          --c22-11
         INTO   vtot_int_sadi_acl          --c22-11
         FROM   safre_tmp:tmp_interes_sub  --c22-11
         WHERE  ide = 3                    --c22-11
         AND    sub = 17                   --c22-11
         IF vtot_int_sadi_acl IS NULL THEN --c22-11
            LET vtot_int_sadi_acl = 0      --c22-11
         END IF                            --c22-11

         --(id33)
         -- INTERES GOBIERNO
         SELECT TRUNC(SUM(mto),2)   --v3
         INTO   vtot_gob
         FROM   safre_tmp:tmp_interes_sub
         WHERE  ide = 3
         AND    sub in (5,6,9)
         IF vtot_gob IS NULL THEN
            LET vtot_gob = 0
         END IF

-------------------------------------------------------------------------------
   LET vimpt_liq_rcv       = reg_sum.impt_liq_rcv       USING "&&&&&&&&&&&&.&&"
   LET vimpt_total_int_rcv = vtot_int_rcv               USING "&&&&&&&&&&&&.&&"

   
   --(id12) --------------------------------------------------------        --v4
   SELECT SUM(impt_int_cal_afo)                                             --v4
   INTO   vimpt_total_int_rcv                                               --v4
   FROM   cta_det_transito                                                  --v4
   WHERE  fecha_creac_lote=vfecha_lote                                      --v4
   AND    ident_pago[14,15]='51'                                            --v6
   AND    ident_int = 1                                                     --v4
   IF vimpt_total_int_rcv is null THEN                                      --v6
      LET vimpt_total_int_rcv=0                                             --v6
   END IF                                                                   --v6
   LET vimpt_total_int_rcv = vimpt_total_int_rcv USING "&&&&&&&&&&&&.&&"    --v4
  
   --(id13)
   LET vimpt_total_com_rcv = reg_sum.impt_total_com_rcv USING "&&&&&&&&&&&&.&&"

   LET vimpt_liq_rcv_acl   = reg_sum.impt_liq_rcv_acl   USING "&&&&&&&&&&&&.&&"
   LET vimpt_int_rcv_acl   = vtot_int_rcv_acl           USING "&&&&&&&&&&&&.&&"
 
   --(id15) --------------------------------------------------------        --v4
   SELECT SUM(impt_int_cal_afo)                                             --v4
   INTO   vimpt_int_rcv_acl                                                 --v4
   FROM   cta_det_transito                                                  --v4
   WHERE  fecha_creac_lote=vfecha_lote                                      --v4
   AND    ident_pago[14,15]='51'                                            --v6
   AND    ident_int = 2                                                     --v4
   IF vimpt_int_rcv_acl is null THEN                                        --v6
      LET vimpt_int_rcv_acl=0                                               --v6
   END IF                                                                   --v6

   LET vimpt_int_rcv_acl   = vimpt_int_rcv_acl   USING "&&&&&&&&&&&&.&&"    --v4
 

   LET reg_sum.impt_liq_vol     = reg_sum.impt_liq_vol * 100      --c22-6.2
   LET reg_sum.impt_int_vol     = vtot_int_vol         * 100      --c22-6.2
   LET reg_sum.impt_liq_vol_acl = reg_sum.impt_liq_vol_acl * 100  --c22-6.2
   LET reg_sum.impt_int_vol_acl = vtot_int_vol_acl         * 100  --c22-6.2

   --(id17) --------------------------------------------------------        --v6
   SELECT SUM(impt_int_cal_afo)                                             --v6
   INTO   vimpt_int_vol                                                     --v6
   FROM   safre_af:cta_det_transito                                         --v6
   WHERE  fecha_creac_lote=vfecha_lote                                      --v6
   AND    ident_pago[14,15]='55'                                            --v6
   AND    ident_int = 1                                                     --v6
   IF vimpt_int_vol is null THEN                                            --v6
      LET vimpt_int_vol = 0                                                 --v6
   END IF                                                                   --v6
   LET vimpt_int_vol       = vimpt_int_vol       USING "&&&&&&&&&&&&.&&"    --v6
          
   --(id19) --------------------------------------------------------        --v6
   SELECT SUM(impt_int_cal_afo)                                             --v6
   INTO   vimpt_int_vol_acl                                                 --v6
   FROM   safre_af:cta_det_transito                                         --v6
   WHERE  fecha_creac_lote=vfecha_lote                                      --v6
   AND    ident_pago[14,15]='55'                                            --v6
   AND    ident_int = 2                                                     --v6
   IF vimpt_int_vol_acl is null THEN                                        --v6
      LET vimpt_int_vol_acl = 0                                             --v6
   END IF                                                                   --v6
   LET vimpt_int_vol_acl   = vimpt_int_vol_acl   USING "&&&&&&&&&&&&.&&"    --v6

   LET reg_sum.impt_liq_com     = reg_sum.impt_liq_com     * 100  --c22-6.2
   LET reg_sum.impt_int_com     = vtot_int_com             * 100  --c22-114
-- LET reg_sum.impt_int_com     = reg_sum.impt_int_com     * 100  --c22-112

   LET reg_sum.impt_liq_com_acl = reg_sum.impt_liq_com_acl * 100  --c22-6.2
   LET reg_sum.impt_int_com_acl = vtot_int_com_acl         * 100  --c22-114

   LET reg_sum.impt_liq_lplazo   = reg_sum.impt_liq_lplazo   * 100 --c22-11
   LET reg_sum.impt_int_lplazo   = vtot_int_lpzo             * 100 --c22-114 
   LET reg_sum.impt_liq_lpzo_acl = reg_sum.impt_liq_lpzo_acl * 100 --c22-11 
   LET reg_sum.impt_int_lpzo_acl = vtot_int_lpzo_acl         * 100 --c22-114
   LET reg_sum.impt_liq_subadi   = reg_sum.impt_liq_subadi   * 100 --c22-11
   LET reg_sum.impt_int_subadi   = vtot_int_sadi             * 100 --c22-114
   LET reg_sum.impt_liq_suba_acl = reg_sum.impt_liq_suba_acl * 100 --c22-11
   LET reg_sum.impt_int_suba_acl = vtot_int_sadi_acl        * 100  --c22-114


   LET vimpt_liq_gob       = reg_sum.impt_liq_gob       USING "&&&&&&&&&&&&.&&"
   LET vimpt_total_int_gob = vtot_gob                   USING "&&&&&&&&&&&&.&&"

 
   --(id26) --------------------------------------------------------        --v4
   SELECT SUM(impt_int_cal_afo)                                             --v4
   INTO   vimpt_total_int_gob                                               --v4
   FROM   safre_af:cta_det_transito                                         --v4
   WHERE  fecha_creac_lote=vfecha_lote                                     --v4
   AND    ident_pago[14,15]='52'                                            --v4
   AND    ident_int=3                                                       --v4
   IF vimpt_total_int_gob is null THEN                                      --v6
      LET vimpt_total_int_gob = 0                                           --v6
   END IF                                                                   --v6
   
   LET    vimpt_total_int_gob = vimpt_total_int_gob USING "&&&&&&&&&&&&.&&" --v4
-------------------------------------------------------------------------------
 

   -- suma campos 12, 13 y 15 de layout sumario --
   LET vimpt_total_xpag    = vtot_int_rcv           +
                             reg_sum.impt_total_com_rcv + 
                             vtot_int_rcv_acl USING "&&&&&&&&&&&&.&&"
 
   --(id28) --------------------------------------------------------        --v4
   SELECT SUM(impt_int_cal_afo)                                             --v4
   INTO   vimpt_total_xpag                                                  --v4
   FROM   safre_af:cta_det_transito                                         --v4
   WHERE  fecha_creac_lote=vfecha_lote                                     --v4
   AND    ident_pago[14]='5'                                                --v4
   AND    ident_pago[15]='1'                                                --v4
   IF vimpt_total_xpag is null THEN                                         --v6
      LET vimpt_total_xpag = 0                                              --v6
   END IF                                                                   --v6
   LET    vimpt_total_xpag = vimpt_total_xpag USING "&&&&&&&&&&&&.&&"       --v4

 
   --(id29) --------------------------------------------------------        --v4
   SELECT SUM(impt_int_cal_afo)                                             --v4
   INTO   vimpt_total_apor_vol                                              --v4
   FROM   safre_af:cta_det_transito                                         --v4
   WHERE  fecha_creac_lote=vfecha_lote                                     --v4
   AND    ident_pago[14,15]='55'                                            --v4
   IF vimpt_total_apor_vol is null THEN                                     --v6
      LET vimpt_total_apor_vol = 0                                          --v6
   END IF                                                                   --v6
   LET vimpt_total_apor_vol = vimpt_total_apor_vol USING "&&&&&&&&&&&&.&&"  --v4
 

   LET reg_sum.impt_total_vol = reg_sum.impt_int_vol +           --c22-6
                                reg_sum.impt_int_vol_acl         --c22-6

   LET reg_sum.impt_total_com = reg_sum.impt_int_com +            --c22-6
                                reg_sum.impt_int_com_acl          --c22-6

   LET reg_sum.impt_total_lplazo = reg_sum.impt_int_lplazo +      --c22-11
                                reg_sum.impt_int_lpzo_acl         --c22-11

   LET reg_sum.impt_total_subadi= reg_sum.impt_int_subadi +       --c22-11
                                reg_sum.impt_int_suba_acl         --c22-11

   LET vimpt_tot_xpag_gob  = vtot_gob USING "&&&&&&&&&&&&.&&"
   LET vimpt_tot_xpag_gob  = vimpt_total_int_gob     --v4

   LET vimpt_liq_rcv       ="0",vimpt_liq_rcv[1,12],
                            vimpt_liq_rcv[14,15]

   LET vimpt_liq_rcv_acl   ="0",vimpt_liq_rcv_acl[1,12],
                            vimpt_liq_rcv_acl[14,15]

   LET vimpt_int_vol       ="0",vimpt_int_vol[1,12],      --v6
                            vimpt_int_vol[14,15]          --v6

   LET vimpt_int_vol_acl   ="0",vimpt_int_vol_acl[1,12],      --v6
                            vimpt_int_vol_acl[14,15]          --v6

   LET vimpt_liq_gob       ="0",vimpt_liq_gob[1,12],
                            vimpt_liq_gob[14,15]

   LET vimpt_total_int_rcv ="0",vimpt_total_int_rcv[1,12],
                            vimpt_total_int_rcv[14,15]

   LET vimpt_total_com_rcv ="0",vimpt_total_com_rcv[1,12],
                            vimpt_total_com_rcv[14,15]

   LET vimpt_int_rcv_acl   ="0",vimpt_int_rcv_acl[1,12], 
                             vimpt_int_rcv_acl[14,15]


   LET vimpt_total_int_gob ="0",vimpt_total_int_gob[1,12], 
                             vimpt_total_int_gob[14,15]

   LET vimpt_total_xpag    ="0",vimpt_total_xpag[1,12],
                            vimpt_total_xpag[14,15]

 
   LET vimpt_total_apor_vol="0",vimpt_total_apor_vol[1,12],    --v4
                            vimpt_total_apor_vol[14,15]        --v4
 

   LET vimpt_tot_xpag_gob  ="0",vimpt_tot_xpag_gob[1,12],
                            vimpt_tot_xpag_gob[14,15]

   LET vsalida = g_param_dis.ruta_envio CLIPPED,"/sum_rcv"

   START REPORT rep_sum TO vsalida
      LET vfecha_creac_lote  = reg_sum.fecha_creac_lote
      LET vfec_creac_lote  = vfecha_creac_lote[7,10],
                             vfecha_creac_lote[1,2],
                             vfecha_creac_lote[4,5]
      OUTPUT TO REPORT rep_sum(reg_sum.*)
   FINISH REPORT rep_sum

END FUNCTION

REPORT rep_sum(reg_sum)
   DEFINE reg_sum RECORD
      tipo_registro      CHAR(02),
      ident_servicio     CHAR(02),
      ident_operacion    CHAR(02),
      tipo_ent_origen    CHAR(02),
      clave_ent_origen   CHAR(03),
      tipo_ent_destino   CHAR(02),
      clave_ent_destino  CHAR(03),
      fecha_creac_lote   DATE,
      lote_del_dia       CHAR(03),
      num_reg_ctas_xpag  CHAR(06),
      impt_liq_rcv       DECIMAL(15,2),
      impt_total_int_rcv DECIMAL(15,2),
      impt_total_com_rcv DECIMAL(15,2),
      impt_liq_rcv_acl   DECIMAL(15,2),
      impt_int_rcv_acl   DECIMAL(15,2),
      impt_liq_vol       DECIMAL(15,2),  --c22-6
      impt_int_vol       DECIMAL(15,2),  --c22-6
      impt_liq_vol_acl   DECIMAL(15,2),  --c22-6
      impt_int_vol_acl   DECIMAL(15,2),  --c22-6
      impt_liq_com       DECIMAL(15,2),  --c22-6
      impt_int_com       DECIMAL(15,2),  --c22-6
      impt_liq_com_acl   DECIMAL(15,2),  --c22-6
      impt_int_com_acl   DECIMAL(15,2),  --c22-6
      impt_liq_lplazo    DECIMAL(15,2),  --c22-11
      impt_int_lplazo    DECIMAL(15,2),  --c22-11
      impt_liq_lpzo_acl  DECIMAL(15,2),  --c22-11
      impt_int_lpzo_acl  DECIMAL(15,2),  --c22-11
      impt_liq_subadi    DECIMAL(15,2),  --c22-11
      impt_int_subadi    DECIMAL(15,2),  --c22-11
      impt_liq_suba_acl  DECIMAL(15,2),  --c22-11
      impt_int_suba_acl  DECIMAL(15,2),  --c22-11
      impt_liq_gob       DECIMAL(15,2),
      impt_total_int_gob DECIMAL(15,2),
      impt_total_xpag    DECIMAL(15,2),
      impt_total_vol     DECIMAL(15,2),  --c22-6
      impt_total_com     DECIMAL(15,2),  --c22-6
      impt_total_lplazo  DECIMAL(15,2),  --c22-11
      impt_total_subadi  DECIMAL(15,2),  --c22-11
      impt_tot_xpag_gob  DECIMAL(15,2)
   END RECORD

   OUTPUT             
      LEFT MARGIN 0  
      RIGHT MARGIN 0 
      TOP MARGIN 0   
      BOTTOM MARGIN 0
      PAGE LENGTH 1  
   FORMAT             
      ON EVERY ROW       
                                
         PRINT COLUMN 1, reg_sum.tipo_registro,
                         reg_sum.ident_servicio,    
                         reg_sum.ident_operacion,   
                         reg_sum.tipo_ent_origen,   
                         reg_sum.clave_ent_origen,  
                         reg_sum.tipo_ent_destino, 
                         reg_sum.clave_ent_destino, 
                         vfec_creac_lote,
                         reg_sum.lote_del_dia,      
                         vcontador_det      USING '&&&&&&',
----                         reg_sum.num_reg_ctas_xpag,  --c22-6.3
                         vimpt_liq_rcv,
                         vimpt_total_int_rcv,
                         vimpt_total_com_rcv,
                         vimpt_liq_rcv_acl,   
                         vimpt_int_rcv_acl,
                     reg_sum.impt_liq_vol     USING '&&&&&&&&&&&&&&&', --c22-6.2
                  -- reg_sum.impt_int_vol     USING '&&&&&&&&&&&&&&&', --c22-6.2
                         vimpt_int_vol,       --v6
                     reg_sum.impt_liq_vol_acl USING '&&&&&&&&&&&&&&&', --c22-6.2
                  -- reg_sum.impt_int_vol_acl USING '&&&&&&&&&&&&&&&', --c22-6.2
                         vimpt_int_vol_acl,   --v6
                     reg_sum.impt_liq_com     USING '&&&&&&&&&&&&&&&', --c22-6.2
                     reg_sum.impt_int_com     USING '&&&&&&&&&&&&&&&', --c22-6.2
                     reg_sum.impt_liq_com_acl USING '&&&&&&&&&&&&&&&', --c22-6.2
                     reg_sum.impt_int_com_acl USING '&&&&&&&&&&&&&&&', --c22-6.2
                     reg_sum.impt_liq_lplazo   USING '&&&&&&&&&&&&&&&',--c22-11
                     reg_sum.impt_int_lplazo   USING '&&&&&&&&&&&&&&&',--c22-11
                     reg_sum.impt_liq_lpzo_acl USING '&&&&&&&&&&&&&&&',--c22-11
                     reg_sum.impt_int_lpzo_acl USING '&&&&&&&&&&&&&&&',--c22-11

                     reg_sum.impt_liq_subadi  USING '&&&&&&&&&&&&&&&', --c22-11 
                     reg_sum.impt_int_subadi  USING '&&&&&&&&&&&&&&&', --c22-11 
                     reg_sum.impt_liq_suba_acl USING '&&&&&&&&&&&&&&&',--c22-11 
                     reg_sum.impt_int_suba_acl USING '&&&&&&&&&&&&&&&',--c22-11
                     --  15 SPACES,                                    --c22-11
                         vimpt_liq_gob,     
                         vimpt_total_int_gob, 
                     --  15 SPACES,                                    --c22-11
                         vimpt_total_xpag,   
                  -- reg_sum.impt_total_vol USING '&&&&&&&&&&&&&&&', --c22-6--v4
                         vimpt_total_apor_vol, --v6
                         reg_sum.impt_total_com USING '&&&&&&&&&&&&&&&', --c22-6
                     reg_sum.impt_total_lplazo USING '&&&&&&&&&&&&&&&', --c22-11
                     reg_sum.impt_total_subadi USING '&&&&&&&&&&&&&&&', --c22-11
                     vimpt_tot_xpag_gob,                               --c22-113
                   --vimpt_liq_gob,                                    --c22-113
                         12 SPACES           --c22-6  antes 97
END REPORT

FUNCTION genera_archivo()
   DEFINE 
      ejecuta CHAR(200)

  LET ejecuta="cat ",g_param_dis.ruta_envio CLIPPED,"/cza_rcv ",
                     g_param_dis.ruta_envio CLIPPED,"/det_rcv ",
                     g_param_dis.ruta_envio CLIPPED,"/sum_rcv > ",
                     g_param_dis.ruta_envio CLIPPED,"/",vfec_creac_lote,".TRAN"
                              
   DISPLAY ejecuta
   PROMPT 'OPRIMA [RETURN] P/TERMINAR ... ' for opc
   RUN ejecuta 
                      
END FUNCTION
