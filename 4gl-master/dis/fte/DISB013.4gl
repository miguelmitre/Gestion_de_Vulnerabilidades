################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#                  => E.F.P.                                                   #
#Programa DISB013  => CARGA DE ARCHIVO tmp_pla INTERESES EN TRANSITO           #
#Fecha             => 14 diciembre 1998.                                       #
#By                => GERARDO ALFONSO VEGA PAREDES.                            #
#Sistema           => DIS.  		            	                               #
#Modifico          => FRANCISCO JAVIER LARIOS HERNANDEZ.                       #
#Fecha             => 14/MAYO/2003.                                            #
#                  => SE MODIFICO ERROR EN LECTURA DE ARCHIVO.                 #
#Modifico          => FRANCISCO JAVIER LARIOS HERNANDEZ.                       #
#Fecha             => 03/JUNIO/2003.                                           #
#                  => SE INTEGRO LOS INTERESES DE GOBIERNO.                    #
#Modifico          => FRANCISCO JAVIER LARIOS HERNANDEZ.                       #
#Fecha             => 25/AGOSTO/2003.                                          #
#                  => SE MODIFICO POR CAMBIO EN LAYOUT POR CALCULO DE INTERE-  #
#                  => SES DE GOBIERNO.                                         #
--------------------------------------------------------------------------------
#Fecha modif       => 02 diciembre 2004                                        #
#Autor             => Alejandro Ramirez                                        #
#Descripcion       => Adecuación layout (032701) (032702) (032709)             #
#                     de acuerdo a circular 22-6 multi siefore,                #
#                     utilizacion de preparados para utilizar multisiefore     #
#                     para intereses en transito                               #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE
      vident_operacion CHAR(2),
    --  g_param_dis     RECORD LIKE dis_parametro.* ,
      g_param_dis     RECORD LIKE seg_modulo.* ,
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
      impt_int_cal_afo  DECIMAL(15,2),
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

   DEFINE 
      hoy       DATE,
      opc       CHAR(01),
      vcomando  CHAR(01),
      generar   CHAR(20),
      comando   CHAR(300),
      carga_reg CHAR(360)  --c22-6  antes 295
     
   DEFINE
      vfecha_creac_lote  DATE,
      vfecha_limite_resp DATE,
      vfecha_ini_notifi  DATE,
      vfecha_fin_notifi  DATE,
      vfecha_liq_interes DATE,
      vfecha_gen_archivo DATE,
      vfecha_pago        DATE,
      ren                SMALLINT,
      col                SMALLINT
END GLOBALS

MAIN
   OPTIONS 
      INPUT WRAP,
      PROMPT LINE LAST  ,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT
        
   CALL STARTLOG("DISB013.log")
   let hoy = TODAY

   OPEN WINDOW ventana_1 AT 4,4 WITH FORM "DISB0131" ATTRIBUTE(BORDER)
   DISPLAY " DISB013          CARGA ARCHIVO INTERESES DE TRANSITO                                          " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                               " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)
   CALL init()
   LET vcomando = "N"

   INPUT BY NAME generar
      AFTER FIELD generar
         IF generar IS NULL THEN
	    ERROR "Campo NO puede ser NULO"
	    NEXT FIELD generar
	 END IF

--  ON KEY (INTERRUPT)
--      EXIT PROGRAM

         SELECT *
         INTO g_param_dis.*
         FROM seg_modulo
         WHERE modulo_cod = 'dis'

         CALL valida_archivo()
         CALL crea_tabla()

         LET comando = g_param_dis.ruta_rescate CLIPPED,"/",generar

         LOAD FROM comando INSERT INTO safre_tmp:tmp_pla_rcv
         IF STATUS = NOTFOUND THEN
            DISPLAY  "NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO"
            EXIT PROGRAM
         ELSE
            EXIT INPUT
         END IF

      ON KEY (INTERRUPT) 
         LET vcomando = "S"
         EXIT INPUT
   END INPUT

   IF vcomando ="S" THEN
      ERROR "PROCESO CANCELADO"
      SLEEP 2
      EXIT PROGRAM
   END IF

   PROMPT "DESEA GENERAR PROCESO [S/N] ..." FOR opc
   IF opc MATCHES '[Ss]' THEN
      ERROR "PROCESANDO INFORMACION "
      CALL proceso_principal()
   ELSE
      ERROR "PROCESO CANCELADO"
      SLEEP 2
      EXIT PROGRAM
   END IF
END MAIN

FUNCTION init()
   INITIALIZE reg_cza.*  TO NULL
   INITIALIZE reg_det.*  TO NULL
   INITIALIZE reg_sum.*  TO NULL
   LET hoy               = TODAY

   LET reg_det.impt_int_cal_pro  = 0
   LET reg_det.impt_int_cal_afo  = 0

   SELECT codigo_admin
     INTO vcodigo_afore
     FROM tab_admin
END FUNCTION

FUNCTION crea_tabla()

   WHENEVER ERROR CONTINUE
   DATABASE safre_tmp
   DROP TABLE tmp_pla_rcv
   CREATE TABLE tmp_pla_rcv
      (
       n_registros CHAR(480)  --c22-11 antes 360
      )
     CREATE index plano_rcv_1 ON plano_rcv(n_registros)
   DATABASE safre_af
   WHENEVER ERROR STOP

END FUNCTION

FUNCTION proceso_principal()
   DEFINE 
      cont_reg,
      total_reg INTEGER 
   DEFINE
      suma_temp DECIMAL(15,2)
 
   DEFINE 
      carga_reg CHAR(480)   --c22-11  antes 360

   DEFINE
      tipo_pago SMALLINT

   LET cont_reg = 0
   LET ren = 11
   LET col = 3
   LET total_reg = 0

   SELECT COUNT(*)
     INTO total_reg
     FROM safre_tmp:tmp_pla_rcv

   IF total_reg < 2 THEN
      DISPLAY "SE RECHAZA EL LOTE. ",
              "NUMERO DE REGISTROS NO SATISFACE EL MINIMO (minimo = 3) "
      EXIT PROGRAM
   END IF
        
   UPDATE STATISTICS FOR TABLE cta_det_transito

   DECLARE cur_3 CURSOR FOR 
   SELECT *,rowid 
     FROM safre_tmp:tmp_pla_rcv
   ORDER BY rowid 

   FOREACH cur_3 INTO carga_reg
      LET cont_reg = cont_reg + 1
      IF cont_reg = 1 THEN
         LET reg_cza.tipo_registro     = carga_reg[001,002] 
         LET reg_cza.ident_servicio    = carga_reg[003,004]
         LET reg_cza.ident_operacion   = carga_reg[005,006]
         LET reg_cza.tipo_ent_origen   = carga_reg[007,008]
         LET reg_cza.clave_ent_origen  = carga_reg[009,011]
         LET reg_cza.tipo_ent_destino  = carga_reg[012,013]
         LET reg_cza.clave_ent_destino = carga_reg[014,016]
         LET reg_cza.fecha_creac_lote  = mdy(carga_reg[21,22],
                                             carga_reg[23,24],
                                             carga_reg[17,20])
         LET reg_cza.lote_del_dia      = carga_reg[025,027]
         LET reg_cza.mod_recp_envio    = carga_reg[028,029]
         LET reg_cza.fecha_limite_resp = mdy(carga_reg[34,35],
                                             carga_reg[36,37],
                                             carga_reg[30,33])
         LET reg_cza.fecha_ini_notifi  = mdy(carga_reg[42,43],
                                             carga_reg[44,45],
                                             carga_reg[38,41])
         LET reg_cza.fecha_fin_notifi  = mdy(carga_reg[50,51],
                                             carga_reg[52,53],
                                             carga_reg[46,49])
         LET reg_cza.fecha_liq_interes = mdy(carga_reg[58,59],
                                             carga_reg[60,61],
                                             carga_reg[54,57])
         LET reg_cza.result_operacion  = carga_reg[062,063]
         LET reg_cza.diag1             = carga_reg[064,066]
         LET reg_cza.diag2             = carga_reg[067,069]
         LET reg_cza.diag3             = carga_reg[070,072]

         INSERT INTO cta_cza_transito VALUES(reg_cza.*)

         DISPLAY "Fec lim resp: ",reg_cza.fecha_limite_resp 
         USING "DD-MM-YYYY","     ",  
         "Fec ini noti: ",reg_cza.fecha_ini_notifi 
         USING "DD-MM-YYYY"  AT 07,07

         DISPLAY "Fec fin noti: ",reg_cza.fecha_fin_notifi 
         USING "DD-MM-YYYY","     ",
         "Fec liq:      ",reg_cza.fecha_liq_interes 
         USING "DD-MM-YYYY"  AT 08,07

      ELSE
         IF cont_reg < total_reg THEN


            LET reg_det.fecha_creac_lote = reg_cza.fecha_creac_lote
            LET reg_det.tipo_registro     = carga_reg[001,002]
            LET reg_det.ident_servicio    = carga_reg[003,004]
            LET reg_det.ident_pago        = carga_reg[005,020]
            LET reg_det.consec_reg_lote   = carga_reg[021,023]
            LET reg_det.fecha_gen_archivo = mdy(carga_reg[28,29], 
                                                carga_reg[30,31],
                                                carga_reg[24,27])

            LET reg_det.impt_liq_rcv      = carga_reg[032,046]
            LET reg_det.impt_liq_rcv      = reg_det.impt_liq_rcv / 100
            LET reg_det.num_aport_envia   = carga_reg[047,054]
            LET reg_det.fecha_pago        = mdy(carga_reg[59,60],
                                                carga_reg[61,62],
                                                carga_reg[55,58])

            LET reg_det.impt_int_cal_pro  = carga_reg[063,077]/100
            LET suma_temp                 = carga_reg[063,077]
            LET reg_det.impt_int_cal_pro  = suma_temp /100

            LET reg_det.impt_int_cal_afo  = carga_reg[078,092]/100
            LET reg_det.ident_int         = carga_reg[93,93]
            LET reg_det.ident_siefore     = carga_reg[094,101]   --c22-6
            LET reg_det.tipo_siefore      = carga_reg[102,104]   --c22-6
            LET reg_det.result_operacion  = carga_reg[105,106]   --c22-6
            LET reg_det.diag1             = carga_reg[107,109]   --c22-6
            LET reg_det.diag2             = carga_reg[110,112]   --c22-6
            LET reg_det.diag3             = carga_reg[113,115]   --c22-6

      	   INSERT INTO cta_det_transito VALUES(reg_det.*)

            LET tipo_pago = reg_det.ident_pago[14,15]
 
        --    IF tipo_pago = '51' OR
        --       tipo_pago = '61' THEN
            IF reg_det.ident_int = '1' OR
                 reg_det.ident_int = '2' THEN
 
               DISPLAY "Fec dis ",reg_det.fecha_gen_archivo 
                      USING "DD-MM-YY",
                     " Int RCV",reg_det.impt_int_cal_pro 
                      USING "--------.--"  AT ren,col
            END IF

           {
            IF tipo_pago = '52' OR
               tipo_pago = '62' THEN
           }

            IF reg_det.ident_int = '3' THEN
 
               DISPLAY "Fec dis ",reg_det.fecha_gen_archivo 
                      USING "DD-MM-YY",
                     " Int GOB",reg_det.impt_int_cal_pro 
                      USING "--------.--"  AT ren,col
            END IF
                 
            LET ren = ren + 1
            IF ren = 19 THEN
               LET ren = 11
               LET col = 40
            END IF
         ELSE
          LET reg_sum.tipo_registro     = carga_reg[001,002] 
          LET reg_sum.ident_servicio    = carga_reg[003,004] 
          LET reg_sum.ident_operacion   = carga_reg[005,006] 
          LET reg_sum.tipo_ent_origen   = carga_reg[007,008] 
          LET reg_sum.clave_ent_origen  = carga_reg[009,011] 
          LET reg_sum.tipo_ent_destino  = carga_reg[012,013] 
          LET reg_sum.clave_ent_destino = carga_reg[014,016] 
          LET reg_sum.fecha_creac_lote   = mdy(carga_reg[21,22], 
                                               carga_reg[23,24],
                                               carga_reg[17,20])
          LET reg_sum.lote_del_dia      = carga_reg[025,027] 
          LET reg_sum.num_reg_ctas_xpag = carga_reg[028,033]
          LET reg_sum.impt_liq_rcv      = carga_reg[034,048]
          LET reg_sum.impt_liq_rcv      = reg_sum.impt_liq_rcv/100
          LET reg_sum.impt_total_int_rcv= carga_reg[049,063]
          LET reg_sum.impt_total_int_rcv= reg_sum.impt_total_int_rcv /100
          LET reg_sum.impt_total_com_rcv= carga_reg[064,078]
          LET reg_sum.impt_total_com_rcv= reg_sum.impt_total_com_rcv/100
          LET reg_sum.impt_liq_rcv_acl  = carga_reg[079,093]
          LET reg_sum.impt_liq_rcv_acl  = reg_sum.impt_liq_rcv_acl/100
          LET reg_sum.impt_int_rcv_acl  = carga_reg[094,108]
          LET reg_sum.impt_int_rcv_acl  = reg_sum.impt_int_rcv_acl/100
          LET reg_sum.impt_liq_vol      = carga_reg[109,123]           --c22-6
          LET reg_sum.impt_liq_vol      = reg_sum.impt_liq_vol/100     --c22-6
          LET reg_sum.impt_int_vol      = carga_reg[124,138]           --c22-6
          LET reg_sum.impt_int_vol      = reg_sum.impt_int_vol/100     --c22-6
          LET reg_sum.impt_liq_vol_acl  = carga_reg[139,153]           --c22-6
          LET reg_sum.impt_liq_vol_acl  = reg_sum.impt_liq_vol_acl/100 --c22-6
          LET reg_sum.impt_int_vol_acl  = carga_reg[154,168]           --c22-6
          LET reg_sum.impt_int_vol_acl  = reg_sum.impt_int_vol_acl/100 --c22-6
          LET reg_sum.impt_liq_com      = carga_reg[169,183]           --c22-6
          LET reg_sum.impt_liq_com      = reg_sum.impt_liq_com/100     --c22-6
          LET reg_sum.impt_int_com      = carga_reg[184,198]           --c22-6
          LET reg_sum.impt_int_com      = reg_sum.impt_int_com/100     --c22-6
          LET reg_sum.impt_liq_com_acl  = carga_reg[199,213]           --c22-6
          LET reg_sum.impt_liq_com_acl  = reg_sum.impt_liq_com_acl/100 --c22-6
          LET reg_sum.impt_int_com_acl  = carga_reg[214,228]           --c22-6
          LET reg_sum.impt_int_com_acl  = reg_sum.impt_int_com_acl/100 --c22-6
          
          LET reg_sum.impt_liq_lplazo   = carga_reg[229,243]            --c22-11
          LET reg_sum.impt_liq_lplazo   = reg_sum.impt_liq_lplazo/100   --c22-11
          LET reg_sum.impt_int_lplazo   = carga_reg[244,258]            --c22-11
          LET reg_sum.impt_int_lplazo   = reg_sum.impt_int_lplazo/100   --c22-11
          LET reg_sum.impt_liq_lpzo_acl = carga_reg[259,273]            --c22-11
          LET reg_sum.impt_liq_lpzo_acl = reg_sum.impt_liq_lpzo_acl/100 --c22-11
          LET reg_sum.impt_int_lpzo_acl = carga_reg[274,288]            --c22-11
          LET reg_sum.impt_int_lpzo_acl = reg_sum.impt_int_lpzo_acl/100 --c22-11
          LET reg_sum.impt_liq_subadi   = carga_reg[289,303]            --c22-11
          LET reg_sum.impt_liq_subadi   = reg_sum.impt_liq_subadi/100   --c22-11
          LET reg_sum.impt_int_subadi   = carga_reg[304,318]            --c22-11
          LET reg_sum.impt_int_subadi   = reg_sum.impt_int_subadi/100   --c22-11
          LET reg_sum.impt_liq_suba_acl = carga_reg[319,333]            --c22-11
          LET reg_sum.impt_liq_suba_acl = reg_sum.impt_liq_suba_acl/100 --c22-11
          LET reg_sum.impt_int_suba_acl = carga_reg[334,348]            --c22-11
          LET reg_sum.impt_int_suba_acl = reg_sum.impt_int_suba_acl/100 --c22-11
   
          LET reg_sum.impt_liq_gob      = carga_reg[349,363]
          LET reg_sum.impt_liq_gob      = reg_sum.impt_liq_gob/100
          LET reg_sum.impt_total_int_gob= carga_reg[364,378]
          LET reg_sum.impt_total_int_gob= reg_sum.impt_total_int_gob/100
          LET reg_sum.impt_total_xpag   = carga_reg[379,393]
          LET reg_sum.impt_total_xpag   = reg_sum.impt_total_xpag/100
          LET reg_sum.impt_total_vol    = carga_reg[394,408]           --c22-6
          LET reg_sum.impt_total_vol    = reg_sum.impt_total_vol/100   --c22-6
          LET reg_sum.impt_total_com    = carga_reg[409,423]           --c22-6
          LET reg_sum.impt_total_com    = reg_sum.impt_total_com/100   --c22-6

          LET reg_sum.impt_total_lplazo = carga_reg[424,438]            --c22-11
          LET reg_sum.impt_total_lplazo = reg_sum.impt_total_lplazo/100 --c22-11
          LET reg_sum.impt_total_subadi = carga_reg[439,453]            --c22-11
          LET reg_sum.impt_total_subadi = reg_sum.impt_total_subadi/100 --c22-11

          LET reg_sum.impt_tot_xpag_gob = carga_reg[454,468]
          LET reg_sum.impt_tot_xpag_gob = reg_sum.impt_tot_xpag_gob/100

      	 INSERT INTO cta_sum_transito VALUES(reg_sum.*)

         END IF
      END IF
   END FOREACH
   ERROR ""
   PROMPT "PROCESO TERMINADO OPRIMA [ENTER] P/SALIR " FOR opc

END FUNCTION

FUNCTION valida_archivo()  --c22-11
   DEFINE vfecha DATE
   DEFINE vfecha2 CHAR(8)

   DATABASE safre_tmp 

   WHENEVER ERROR CONTINUE
   DROP TABLE tmp_fechalote

   CREATE TABLE tmp_fechalote
   (
     arc_fecha  char(08)
   )
   WHENEVER ERROR STOP


  LET comando = "head -n 1 ",g_param_dis.ruta_rescate CLIPPED,"/",
          generar CLIPPED," > ",g_param_dis.ruta_rescate CLIPPED,"/cza2"
  RUN comando

  LET comando = "cut -c17-24 ",g_param_dis.ruta_rescate CLIPPED,"/cza2 > ",
          g_param_dis.ruta_rescate CLIPPED,"/fechalote"
  RUN comando

   LET comando = g_param_dis.ruta_rescate CLIPPED,"/fechalote"

   LOAD FROM comando INSERT INTO tmp_fechalote

   LET vfecha2=''
   SELECT arc_fecha
   INTO vfecha2
   FROM tmp_fechalote

   DATABASE safre_af 

   IF vfecha2 IS NULL THEN
      DISPLAY  "NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO"
   ELSE

      LET vfecha = MDY(vfecha2[5,6],vfecha2[7,8],vfecha2[1,4])

      SELECT 'x' FROM cta_cza_transito
      WHERE fecha_creac_lote = vfecha

      IF STATUS <> NOTFOUND THEN
         ERROR "ESTE ARCHIVO YA FUE PROCESADO       "
         SLEEP 2
         EXIT PROGRAM
      END IF
      
   END IF
   
{ --c22-11
   LET vfecha = MDY(generar[5,6],generar[7,8],generar[1,4])

   SELECT *    
     FROM cta_cza_transito
    WHERE fecha_creac_lote = vfecha
}

END FUNCTION
