DATABASE safre_tmp

GLOBALS
define lanza char(100)
define cve_afore smallint
define razon_social char(015)
define ll char(100)
DEFINE HOY DATE
DEFINE ruta CHAR(100)
define tt char(050)
DEFINE reg_cza_hist_trab RECORD LIKE safre_tmp:cza_hist_trab.*
DEFINE reg_ruta RECORD LIKE safre_af:seg_modulo.*
DEFINE txt1 CHAR(400)
DEFINE vv_fecha_ini DATE
DEFINE vv_fecha_fin DATE
DEFINE fecha_envio DATE 
       ,f1 DATE
END GLOBALS

MAIN
CALL STARTLOG("HISL010.log")
{
LET tt = "set pdqpriority 100"
PREPARE qr FROM tt
EXECUTE qr
}

select a.codigo_afore ,
       a.razon_social 
INTO   cve_afore ,
       razon_social 
FROM   safre_af:tab_afore_local a


UPDATE safre_tmp:det_mov_trab
SET siefore = " ",
    acciones = 0,
    precio_accion = 0
WHERE subcuenta  IN ("04","09","12","14","15","16")
AND   acciones = 0
UPDATE safre_tmp:det_mov_trab_uni
SET siefore = " ",
    acciones = 0,
    precio_accion = 0
WHERE subcuenta  IN ("04","09","12","14","15","16")
AND   acciones = 0

SELECT a.* 
INTO reg_ruta.*
FROM safre_af:seg_modulo a
WHERE a.modulo_cod = "est"

LET HOY = TODAY
LET vv_fecha_ini = ARG_VAL(1)
LET vv_fecha_fin = ARG_VAL(2)

--LET fecha_envio = MDY(MONTH(HOY),"01",YEAR(HOY))

CALL cal_fecha_avant(vv_fecha_fin,10)RETURNING f1
{
WHILE  (MONTH(f1) <> MONTH(fecha_envio))
   CALL cal_fecha_retro(f1,1) RETURNING f1
END WHILE
}

    UPDATE STATISTICS FOR TABLE safre_tmp:cza_hist_trab 
    UPDATE STATISTICS FOR TABLE safre_tmp:det_mov_trab  
    UPDATE STATISTICS FOR TABLE safre_tmp:det_mov_trab_uni

    LET ruta =  reg_ruta.ruta_envio CLIPPED,"/PSO_COMP.",
                vv_fecha_ini USING"YYYYMMDD","-",vv_fecha_fin USING"YYYYMMDD"   

       START REPORT rpt_1  TO ruta
       OUTPUT TO REPORT rpt_1()
       FINISH REPORT rpt_1

LET txt1 = "cd ",reg_ruta.ruta_envio CLIPPED,";",
           "sed -e /^$/d ",ruta ," > ",
           reg_ruta.ruta_envio CLIPPED,"/PHISTORICO_COMP.",vv_fecha_ini
           USING"YYYYMMDD","-",vv_fecha_fin USING"YYYYMMDD"
RUN txt1

LET txt1 = "rm ",ruta CLIPPED
RUN txt1 

LET lanza = "fglgo HISL002 ",vv_fecha_ini , " ",vv_fecha_fin
RUN lanza
END MAIN




REPORT rpt_1()
#r1-----------
DEFINE uno char(003),
       dos char(016),
       tres char(011)

DEFINE reg_cza_hist_trab RECORD LIKE safre_tmp:cza_hist_trab.*
DEFINE reg_det_mov_trab  RECORD LIKE safre_tmp:det_mov_trab.*

  DEFINE reg_det_mov_trab_uni RECORD 
    cont_serv      decimal(10),
    nss_unif       char(11),
    curp_unif  	   char(18),
    nss_afore      char(11),
    curp           char(18),
    iden_proc      char(2),
    consecutivo    smallint,
    iden_mov       char(108),
    tipo_mov       char(1),
    subcuenta      char(2),
    importe        decimal(15,2),
    siefore        char(8),
    fecha_valuacion char(8),
    acciones       decimal(16,6),
    precio_accion  decimal(12,6),
    consec_asoc    smallint,
    fecha_proceso  date,
    fecha_conversion date
END RECORD    

DEFINE v_tot_det_acc     INTEGER         
DEFINE tot_det_03        INTEGER         
DEFINE tot_det_04        INTEGER         
DEFINE tot_trab          INTEGER         
DEFINE tot_viv           DECIMAL(13,2)         
DEFINE tot_ret           DECIMAL(13,2)       
DEFINE total_vivienda    DECIMAL(13,2)         
DEFINE total_retiro      DECIMAL(13,2)       
OUTPUT                                            
       
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

FORMAT                                            

FIRST PAGE HEADER
  PRINT COLUMN 001,"01"            ,
        COLUMN 003,"02"            ,
        COLUMN 005,"07"            ,
        COLUMN 007,"01"            ,
        COLUMN 009,cve_afore USING "&&&" ,
        COLUMN 012,"01"            ,
        COLUMN 014,"   "           ,
        COLUMN 017,f1  USING "YYYYMMDD"  ,
        COLUMN 025,"001"           ,
        COLUMN 028,243 SPACES 

ON EVERY ROW   

    DECLARE cur_1 CURSOR FOR 
         SELECT A.*
         FROM safre_tmp:cza_hist_trab A
	 ORDER BY cont_serv
 
    FOREACH cur_1 INTO reg_cza_hist_trab.*         

        PRINT COLUMN 001,"02"            ,
              COLUMN 003,reg_cza_hist_trab.cont_serv   USING"&&&&&&&&&&"  ,
              COLUMN 013,"01"                                             ,
              COLUMN 015,reg_cza_hist_trab.cve_recep_cuenta USING"###"    ,
              COLUMN 018,"01"                                             ,
              COLUMN 020,reg_cza_hist_trab.cve_ced_cuenta USING"&&&"      ,
              COLUMN 023,reg_cza_hist_trab.origen                         ,
              COLUMN 041,reg_cza_hist_trab.n_unico                        ,
              COLUMN 059,reg_cza_hist_trab.nss                            ,
              COLUMN 070,reg_cza_hist_trab.fecha_afi_pri  , 
              COLUMN 078,reg_cza_hist_trab.fecha_trasp    ,
              COLUMN 086,reg_cza_hist_trab.fecha_pat_pri  ,
              COLUMN 094,reg_cza_hist_trab.fecha_ven_pri  ,
              COLUMN 102,reg_cza_hist_trab.ind_viv    USING"&",
              COLUMN 103,reg_cza_hist_trab.nro_nss_asoc USING "&&",
              COLUMN 105,166  SPACES       

         DECLARE cur_2 CURSOR FOR 
              SELECT A.* 
              FROM   safre_tmp:det_mov_trab A
              WHERE  A.cont_serv = reg_cza_hist_trab.cont_serv
	      ORDER BY  A.consecutivo
         FOREACH cur_2 INTO reg_det_mov_trab.*

         PRINT COLUMN 001,"03"                                          ,
               COLUMN 003,reg_det_mov_trab.cont_serv  USING "&&&&&&&&&&",
               COLUMN 013,reg_det_mov_trab.curp                         ,
               COLUMN 031,reg_det_mov_trab.nss_afore                    ,
               COLUMN 042,reg_det_mov_trab.iden_proc USING"&&"          ,
               COLUMN 044,reg_det_mov_trab.consecutivo  USING"&&&"      ,
               COLUMN 047,reg_det_mov_trab.iden_mov                     ,
               COLUMN 155,reg_det_mov_trab.tipo_mov                     ,
               COLUMN 156,reg_det_mov_trab.subcuenta                    ,
               COLUMN 158,reg_det_mov_trab.importe  * 100 
			  USING"&&&&&&&&&&&&&&&",
	       COLUMN 173,reg_det_mov_trab.siefore                      ,
	       COLUMN 181,reg_det_mov_trab.fecha_valuacion              ,
	       COLUMN 189,reg_det_mov_trab.acciones * 1000000 
			  USING"&&&&&&&&&&&&&&&&"                       ,
               COLUMN 205,reg_det_mov_trab.precio_accion * 1000000
			  USING"&&&&&&&&&&&"                            ,
               COLUMN 216,55 SPACES

        END FOREACH
###

         DECLARE cur_2_uni CURSOR FOR 
              SELECT A.* 
              FROM   safre_tmp:det_mov_trab_uni A
              WHERE  A.cont_serv = reg_cza_hist_trab.cont_serv
	      ORDER BY A.consec_asoc, A.consecutivo
         FOREACH cur_2_uni INTO reg_det_mov_trab_uni.*

         PRINT COLUMN 001,"04"                                          ,
               COLUMN 003,reg_det_mov_trab_uni.cont_serv  USING "&&&&&&&&&&",
               COLUMN 013,reg_det_mov_trab_uni.nss_unif                     ,
               COLUMN 024,reg_det_mov_trab_uni.curp_unif                    ,
               COLUMN 042,reg_det_mov_trab_uni.nss_afore                    ,
               COLUMN 053,reg_det_mov_trab_uni.curp                         ,
               COLUMN 071,reg_det_mov_trab_uni.iden_proc USING"&&"          ,
               COLUMN 073,reg_det_mov_trab_uni.consecutivo  USING"&&&"      ,
               COLUMN 076,reg_det_mov_trab_uni.iden_mov                     ,
               COLUMN 184,reg_det_mov_trab_uni.tipo_mov                     ,
               COLUMN 185,reg_det_mov_trab_uni.subcuenta                    ,
               COLUMN 187,reg_det_mov_trab_uni.importe  * 100 
			  USING"&&&&&&&&&&&&&&&",
	       COLUMN 202,reg_det_mov_trab_uni.siefore                      ,
	       COLUMN 210,reg_det_mov_trab_uni.fecha_valuacion              ,
	       COLUMN 218,reg_det_mov_trab_uni.acciones * 1000000 
			  USING"&&&&&&&&&&&&&&&&"                           ,
               COLUMN 234,reg_det_mov_trab_uni.precio_accion * 1000000
			  USING"&&&&&&&&&&&"                                ,
               COLUMN 245,reg_det_mov_trab_uni.consec_asoc   USING"&&"      ,
               COLUMN 247,23 SPACES

        END FOREACH


####
                SELECT COUNT(*) 
                INTO tot_det_03   
                FROM safre_tmp:det_mov_trab A
                WHERE A.cont_serv = reg_cza_hist_trab.cont_serv 

IF tot_det_03 IS NULL THEN
   LET tot_det_03 = 0
END IF
                SELECT COUNT(*) 
                INTO tot_det_04   
                FROM safre_tmp:det_mov_trab_uni A
                WHERE A.cont_serv = reg_cza_hist_trab.cont_serv 

IF tot_det_04 IS NULL THEN
   LET tot_det_04 = 0
END IF

                LET tot_det_03 = tot_det_03 + tot_det_04


                SELECT SUM(ABS(B.importe))
                INTO tot_viv
                FROM  safre_tmp:det_mov_trab B 
                WHERE B.cont_serv = reg_cza_hist_trab.cont_serv
                AND   B.subcuenta IN ("04","09","12","14","15","16")

IF tot_viv IS NULL THEN
   LET tot_viv = 0
END IF
                SELECT SUM(ABS(B.importe))
                INTO tot_ret 
                FROM  safre_tmp:det_mov_trab B
                WHERE B.cont_serv = reg_cza_hist_trab.cont_serv
                AND   B.subcuenta NOT IN ("04","09","12","14","15","16")
IF tot_ret IS NULL THEN
   LET tot_ret = 0
END IF

                PRINT COLUMN 001,"05"                                  ,
                      COLUMN 003,reg_cza_hist_trab.nss                 ,
                      COLUMN 014,reg_cza_hist_trab.n_unico             ,
                      COLUMN 032,tot_det_03 USING"&&&&&&&&&"           ,
                      COLUMN 041,tot_viv  * 100 USING"&&&&&&&&&&&&&&&" ,
                      COLUMN 056,tot_ret  * 100 USING"&&&&&&&&&&&&&&&" ,
                      COLUMN 071,200 SPACES

      END FOREACH
                SELECT SUM(ABS(B.importe)) 
                INTO total_vivienda 
                FROM  safre_tmp:det_mov_trab B 
                WHERE B.subcuenta IN ("04","09","12","14","15","16")
IF total_vivienda IS NULL
THEN LET total_vivienda = 0
END IF

                SELECT COUNT(*) 
		INTO tot_trab
		FROM safre_tmp:cza_hist_trab

IF tot_trab IS NULL
THEN LET tot_trab = 0
END IF
                SELECT SUM(ABS(B.importe))
                INTO total_retiro
                FROM  safre_tmp:det_mov_trab B
                WHERE B.subcuenta NOT IN ("04","09","12","14","15","16")

IF total_retiro IS NULL
THEN LET total_retiro = 0
END IF
             PRINT COLUMN 001,"09"                   ,
		   COLUMN 003,tot_trab USING "&&&&&&&&&"  ,
		   COLUMN 012,total_vivienda * 100 USING"&&&&&&&&&&&&&&&" ,
		   COLUMN 027,total_retiro * 100 USING"&&&&&&&&&&&&&&&" ,
		   COLUMN 042,229 SPACES
END REPORT

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

        LET sig_fecha  = sig_fecha + 1

        LET dia_semana = WEEKDAY(sig_fecha)
        IF dia_semana = 0 OR dia_semana = 6 THEN
           CONTINUE WHILE
        ELSE  
           SELECT "ok" 
           FROM   safre_af:tab_feriado
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
           FROM   safre_af:tab_feriado
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

