##########################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                        #
#Propietario       => E.F.P.                                             #
#Programa HISB001  => INFORMACION HISTORICA TRASPASOS  AFO-AFO
#Por               => JESUS DAVID YANEZ MORENO                           #
#Fecha creacion    => 28 AGOSTO 2006                                     #
#Actualizacion     =>                                                    #
#Fecha actualiz.   =>                                                    #
#Sistema           => HIST COMPLEMENTARIO                                #
##########################################################################

DATABASE safre_af
GLOBALS
DEFINE g_nom_dis_cuenta LIKE taa_cd_tab_cuenta.nombre_tabla
DEFINE g_scta SMALLINT	
DEFINE g_tipo_mov SMALLINT
DEFINE v_ind char(001)
define tt char(050)
DEFINE lanza char(100)
       DEFINE enter            CHAR(001)
       DEFINE sw_1             SMALLINT
       DEFINE v_fecha_pat DATE
       DEFINE v_fecha_ven DATE
       DEFINE HOY              DATE

       DEFINE v_rowid_afi_mae_afiliado INTEGER 

       DEFINE reg_input  RECORD 
              folio_normal           ,
              folio_comp             INTEGER
       END    RECORD	

       DEFINE reg_taa_cd_det_cedido RECORD LIKE safre_af:taa_cd_det_cedido.* 
       DEFINE reg_taa_cd_det_comple RECORD LIKE safre_af:taa_cd_det_comple.* 

       DEFINE reg_afi_mae_afiliado    RECORD LIKE safre_af:afi_mae_afiliado.*

       DEFINE reg_cza_hist_trab RECORD LIKE safre_tmp:cza_hist_trab.*
    
       DEFINE cont       INTEGER

       DEFINE v_nss      CHAR(011)
      
       DEFINE vv_fecha_ini DATE
       DEFINE vv_fecha_fin DATE
       DEFINE g_sql_dis_cuentas CHAR(100) 
       DEFINE g_sql_vol_ret  CHAR(200) 
       DEFINE g_sql_vol_abo  CHAR(200) 
END GLOBALS

MAIN 
CALL STARTLOG("HISB010.log")
LET g_sql_dis_cuentas =
  ' SELECT a.nombre_tabla  ' ,
  ' FROM   taa_cd_tab_cuenta a '

PREPARE qry_dis_cuentas FROM g_sql_dis_cuentas
DECLARE cur_dis_cuentas CURSOR FOR qry_dis_cuentas

LET tt = "set pdqpriority 100"
PREPARE qr FROM tt
EXECUTE qr
    LET vv_fecha_ini = ARG_VAL(1)
    LET vv_fecha_fin = ARG_VAL(2)

    LET reg_cza_hist_trab.cont_serv = 0
    LET HOY = TODAY
WHENEVER ERROR CONTINUE 
 DROP TABLE temp_t1
 DROP TABLE temp_t2
 DROP TABLE temp_t3
 DROP TABLE temp_t4

 CREATE TEMP TABLE temp_t1 (fecha_conversion date)
 CREATE TEMP TABLE temp_t2 (fecha_conversion date)
 CREATE TEMP TABLE temp_t3 (fecha_conversion date)
 CREATE TEMP TABLE temp_t4 (fecha_conversion date)

WHENEVER ERROR  STOP


      DECLARE cur_1 CURSOR FOR

	 SELECT A.*
	 FROM   safre_af:taa_cd_det_comple A
	 WHERE  fecha_liquida BETWEEN vv_fecha_ini AND vv_fecha_fin
         AND    estado = 103
         ORDER BY A.fecha_trasp,A.n_seguro

      FOREACH cur_1 INTO reg_taa_cd_det_comple.*

         SELECT A.*
	 INTO   reg_taa_cd_det_cedido.*
	 FROM   safre_af:taa_cd_det_cedido A
	 WHERE  A.n_seguro =  reg_taa_cd_det_comple.n_seguro
         AND    A.fecha_trasp = reg_taa_cd_det_comple.fecha_trasp
	 AND    A.estado IN (103,12)


    LET reg_cza_hist_trab.origen = reg_taa_cd_det_cedido.tipo_traspaso


         SELECT A.ind_infonavit
         INTO v_ind
         FROM afi_mae_afiliado  A
         WHERE A.n_seguro = reg_taa_cd_det_cedido.n_seguro 

        IF v_ind = "S" THEN
             LET reg_cza_hist_trab.ind_viv = "1"
         ELSE  
             LET reg_cza_hist_trab.ind_viv = "2"
         END IF  

         SELECT a.rowid,a.*
         INTO   v_rowid_afi_mae_afiliado,reg_afi_mae_afiliado.*
         FROM   safre_af:afi_mae_afiliado a
         WHERE  a.n_seguro = reg_taa_cd_det_cedido.n_seguro 
      
         LET reg_cza_hist_trab.fecha_afi_pri = reg_afi_mae_afiliado.fentcons
	     USING "YYYYMMDD"

         LET reg_cza_hist_trab.fecha_trasp = reg_taa_cd_det_comple.fecha_liquida
             USING "YYYYMMDD"
 LET v_fecha_pat = NULL
#########
delete from temp_t1 
delete from temp_t2 
delete from temp_t3 
delete from temp_t4 


              FOREACH cur_dis_cuentas INTO g_nom_dis_cuenta 
                LET g_sql_vol_ret =
                    ' INSERT INTO temp_t1 ' ,
                    ' SELECT max(a.fecha_conversion) fecha_conversion ',
                    ' FROM   ',g_nom_dis_cuenta CLIPPED,' a ',
                    ' WHERE  a.nss =  ? ',
                    ' AND    a.subcuenta  = ? ',
                    ' AND    a.tipo_movimiento = ? '
                 PREPARE qry_vol_pat_ret FROM g_sql_vol_ret
                  LET g_scta = 3   
                  LET g_tipo_mov = 490 
                 EXECUTE qry_vol_pat_ret USING reg_taa_cd_det_cedido.n_seguro,
                                           g_scta                       ,
                                           g_tipo_mov     
                                       
              END FOREACH

              SELECT max(a.fecha_conversion)
              INTO  v_fecha_pat 
              FROM temp_t1 a

              IF v_fecha_pat IS NULL THEN
                 LET v_fecha_pat = NULL
                 FOREACH cur_dis_cuentas INTO g_nom_dis_cuenta 
                     LET g_sql_vol_abo =
                     ' INSERT INTO temp_t2 ' ,
                     ' SELECT min(a.fecha_conversion) fecha_conversion ',
                     ' FROM   ',g_nom_dis_cuenta CLIPPED,' a ',
                     ' WHERE  a.nss =  ? ',
                     ' AND    a.subcuenta  = ? ',
                     ' AND    a.tipo_movimiento = ? '
                     PREPARE qry_vol_pat_abo FROM g_sql_vol_abo

                     LET g_scta = 3   
                     LET g_tipo_mov = 1 
                     EXECUTE qry_vol_pat_abo USING reg_taa_cd_det_cedido.n_seguro,
                                               g_scta                       ,
                                               g_tipo_mov     
                                          
                  END FOREACH

                  SELECT min(a.fecha_conversion)
                  INTO v_fecha_pat
                  FROM temp_t2  a

                  IF v_fecha_pat IS NULL THEN
                    LET reg_cza_hist_trab.fecha_pat_pri = "00010101"
                  ELSE
                     LET reg_cza_hist_trab.fecha_pat_pri = 
                     v_fecha_pat USING "YYYYMMDD"
                  END IF
              ELSE
                  LET reg_cza_hist_trab.fecha_pat_pri = 
                      v_fecha_pat USING "YYYYMMDD"
              END IF

              LET v_fecha_ven = NULL

              FOREACH cur_dis_cuentas INTO g_nom_dis_cuenta 
                LET g_sql_vol_ret =
                    ' INSERT INTO temp_t3 ' ,
                    ' SELECT max(a.fecha_conversion) fecha_conversion ',
                    ' FROM   ',g_nom_dis_cuenta CLIPPED,' a ',
                    ' WHERE  a.nss =  ? ',
                    ' AND    a.subcuenta  = ? ',
                    ' AND    a.tipo_movimiento = ? '
                 PREPARE qry_vol_ret FROM g_sql_vol_ret
                  LET g_scta = 10   
                  LET g_tipo_mov = 490 
                 EXECUTE qry_vol_ret USING reg_taa_cd_det_cedido.n_seguro,
                                           g_scta                       ,
                                           g_tipo_mov     
                                       
              END FOREACH

              SELECT max(a.fecha_conversion)
              INTO v_fecha_ven
              FROM temp_t3 a

          IF v_fecha_ven IS NULL THEN

             LET v_fecha_ven = NULL
                 FOREACH cur_dis_cuentas INTO g_nom_dis_cuenta 
                     LET g_sql_vol_abo =
                     ' INSERT INTO temp_t2 ' ,
                     ' SELECT min(a.fecha_conversion) fecha_conversion ',
                     ' FROM   ',g_nom_dis_cuenta CLIPPED,' a ',
                     ' WHERE  a.nss =  ? ',
                     ' AND    a.subcuenta  = ? ',
                     ' AND    a.tipo_movimiento = ? '
                     PREPARE qry_vol_abo FROM g_sql_vol_abo

                     LET g_scta = 10   
                     LET g_tipo_mov = 1 
                     EXECUTE qry_vol_abo USING reg_taa_cd_det_cedido.n_seguro,
                                               g_scta                       ,
                                               g_tipo_mov     
                                          
                  END FOREACH

             SELECT min(a.fecha_conversion)
             INTO v_fecha_ven
             FROM   temp_t4 a

            IF v_fecha_ven IS NULL THEN
	       LET reg_cza_hist_trab.fecha_ven_pri = "00010101"
            ELSE
               LET reg_cza_hist_trab.fecha_ven_pri =
	  	   v_fecha_ven USING "YYYYMMDD"
            END IF                                  
  ELSE
	      LET reg_cza_hist_trab.fecha_ven_pri =
                  v_fecha_ven USING "YYYYMMDD"
  END IF                       


   LET reg_cza_hist_trab.cont_serv         = reg_cza_hist_trab.cont_serv + 1
   LET reg_cza_hist_trab.fecha_proceso     = HOY
   LET reg_cza_hist_trab.tipo_registro     = "02"
   LET reg_cza_hist_trab.tipo_recep_cuenta= reg_taa_cd_det_cedido.tipo_recep_cuenta
   LET reg_cza_hist_trab.cve_recep_cuenta  = reg_taa_cd_det_cedido.cve_recep_cuenta
   LET reg_cza_hist_trab.tipo_ced_cuenta   = reg_taa_cd_det_cedido.tipo_ced_cuenta
   LET reg_cza_hist_trab.cve_ced_cuenta    = reg_taa_cd_det_cedido.cve_ced_cuenta
   LET reg_cza_hist_trab.orig_tipo_trasp   = reg_taa_cd_det_cedido.tipo_traspaso
   LET reg_cza_hist_trab.n_unico           = reg_taa_cd_det_cedido.n_unico
   LET reg_cza_hist_trab.nss               = reg_taa_cd_det_cedido.n_seguro

   SELECT count(unique a.nss_cta1) 
   INTO   reg_cza_hist_trab.nro_nss_asoc 
   FROM   uni_unificado a
   WHERE  a.nss_uni = reg_cza_hist_trab.nss
   AND    a.estado  = 100

   IF reg_cza_hist_trab.nro_nss_asoc IS NULL THEN
      LET reg_cza_hist_trab.nro_nss_asoc = 0
   END IF

           INSERT INTO safre_tmp:cza_hist_trab 
           VALUES (reg_cza_hist_trab.*)
     
     END FOREACH

LET lanza = "fglgo HISB020 ",vv_fecha_ini CLIPPED," ",vv_fecha_fin CLIPPED
RUN lanza 
END MAIN
             
