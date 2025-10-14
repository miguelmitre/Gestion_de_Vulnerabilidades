DATABASE safre_af

GLOBALS
define reg_afore record like safre_af:tab_afore_local.*
define vv_fecha_ini date
define vv_fecha_fin date
define lanza char(100)
DEFINE vv DATE
DEFINE HORA CHAR(008)
DEFINE i SMALLINT
DEFINE HOY DATE

DEFINE reg_2 RECORD
       normal               , 
       unificacion          ,
       asignacion   DECIMAL(13,2) 
END RECORD 

DEFINE reg_1 RECORD 
       cve_recep_cuenta     LIKE safre_tmp:cza_hist_trab.cve_recep_cuenta , 
       subtotal             INTEGER 
END RECORD

DEFINE v_orig_tipo_trasp CHAR(002)
DEFINE ruta CHAR(100)
DEFINE reg_cza_hist_trab RECORD LIKE safre_tmp:cza_hist_trab.*
END GLOBALS

MAIN

LET vv_fecha_ini = ARG_VAL(1) 
LET vv_fecha_fin = ARG_VAL(2) 

LET HOY = TODAY
LET HORA = TIME

SELECT a.*
INTO   reg_afore.*
FROM   safre_af:tab_afore_local a


       LET ruta =  "/safre_prc/est/envio/REPH.",vv_fecha_ini USING"YYYYMMDD" ,
		   "-",vv_fecha_fin USING"YYYYMMDD"

       START REPORT rpt_1  TO ruta


       LET vv = MDY(MONTH(HOY),"01",YEAR(HOY)) 

       DECLARE cur_1 CURSOR FOR
       SELECT A.cve_recep_cuenta ,
              COUNT(*) 
       FROM   safre_tmp:cza_hist_trab A
       GROUP BY 1
       ORDER BY 1

       FOREACH cur_1 INTO reg_1.*
LET reg_2.normal      = 0
LET reg_2.unificacion = 0
LET reg_2.asignacion  = 0
          FOR i = 1 TO 9
          CASE i 
          WHEN 1
             SELECT SUM(B.importe)
             INTO   reg_2.normal
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "01"
             AND    B.subcuenta in ("01","18","26","25")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv
 
             SELECT SUM(B.importe)
             INTO   reg_2.unificacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "12"
             AND    B.subcuenta in ("01","18","26","25")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv

             SELECT SUM(B.importe)
             INTO   reg_2.asignacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "51"
             AND    B.subcuenta in ("01","18","26","25")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv

             IF reg_2.normal IS NULL THEN 
                 LET reg_2.normal = 0
             END IF
             IF reg_2.unificacion IS NULL THEN 
                 LET reg_2.unificacion = 0
             END IF
             IF reg_2.asignacion IS NULL THEN 
                 LET reg_2.asignacion = 0
             END IF

             OUTPUT TO REPORT rpt_1(reg_1.*,reg_2.*,i)
             EXIT CASE
          WHEN 2
             SELECT SUM(B.importe)
             INTO   reg_2.normal
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "01"
             AND    B.subcuenta in ("02","27","28","20")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv
           
             SELECT SUM(B.importe)
             INTO   reg_2.unificacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "12"
             AND    B.subcuenta in ("02","27","28","20")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv

             SELECT SUM(B.importe)
             INTO   reg_2.asignacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "51"
             AND    B.subcuenta in ("02","27","28","20")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv
 
             IF reg_2.normal IS NULL THEN 
                              LET reg_2.normal = 0
             END IF
             IF reg_2.unificacion IS NULL THEN 
                 LET reg_2.unificacion = 0
             END IF
             IF reg_2.asignacion IS NULL THEN 
                 LET reg_2.asignacion = 0
             END IF
             OUTPUT TO REPORT rpt_1(reg_1.*,reg_2.*,i)
             EXIT CASE

          WHEN 3
             SELECT SUM(B.importe)
             INTO   reg_2.normal
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "01"
             AND    B.subcuenta in ("13","05","23")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv
           
             SELECT SUM(B.importe)
             INTO   reg_2.unificacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "12"
             AND    B.subcuenta in ("13","05","23")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv

             SELECT SUM(B.importe)
             INTO   reg_2.asignacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "51"
             AND    B.subcuenta in ("13","05","23")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv
 
             IF reg_2.normal IS NULL THEN 
                 LET reg_2.normal = 0
             END IF
             IF reg_2.unificacion IS NULL THEN 
                 LET reg_2.unificacion = 0
             END IF
             IF reg_2.asignacion IS NULL THEN 
                 LET reg_2.asignacion = 0
             END IF
             OUTPUT TO REPORT rpt_1(reg_1.*,reg_2.*,i)
             EXIT CASE

          WHEN 4
             SELECT SUM(B.importe)
             INTO   reg_2.normal
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "01"
             AND    B.subcuenta in ("04","12","15")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv
          
 
             SELECT SUM(B.importe)
             INTO   reg_2.unificacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "12"
             AND    B.subcuenta in ("04","12","15")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv


             SELECT SUM(B.importe)
             INTO   reg_2.asignacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "51"
             AND    B.subcuenta in ("04","12","15")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv
 
             IF reg_2.normal IS NULL THEN 
                 LET reg_2.normal = 0
             END IF
             IF reg_2.unificacion IS NULL THEN 
                 LET reg_2.unificacion = 0
             END IF
             IF reg_2.asignacion IS NULL THEN 
                 LET reg_2.asignacion = 0
             END IF

             OUTPUT TO REPORT rpt_1(reg_1.*,reg_2.*,i)
             EXIT CASE

          WHEN 5
             SELECT SUM(B.importe)
             INTO   reg_2.normal
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "01"
             AND    B.subcuenta in ("03","19","29","30")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv
           
             SELECT SUM(B.importe)
             INTO   reg_2.unificacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "12"
             AND    B.subcuenta in ("03","19","29","30")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv

             SELECT SUM(B.importe)
             INTO   reg_2.asignacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "51"
             AND    B.subcuenta in ("03","19","29","30")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv
 
             IF reg_2.normal IS NULL THEN 
                 LET reg_2.normal = 0
             END IF
             IF reg_2.unificacion IS NULL THEN 
                 LET reg_2.unificacion = 0
             END IF
             IF reg_2.asignacion IS NULL THEN 
                 LET reg_2.asignacion = 0
             END IF
             OUTPUT TO REPORT rpt_1(reg_1.*,reg_2.*,i)
             EXIT CASE


          WHEN 6
             SELECT SUM(B.importe)
             INTO   reg_2.normal
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "01"
             AND    B.subcuenta in ("06","31","32")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv
           
             SELECT SUM(B.importe)
             INTO   reg_2.unificacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "12"
             AND    B.subcuenta in ("06","31","32")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv

             SELECT SUM(B.importe)
             INTO   reg_2.asignacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "51"
             AND    B.subcuenta in ("06","31","32")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv
 
             IF reg_2.normal IS NULL THEN 
                 LET reg_2.normal = 0
             END IF
             IF reg_2.unificacion IS NULL THEN 
                 LET reg_2.unificacion = 0
             END IF
             IF reg_2.asignacion IS NULL THEN 
                 LET reg_2.asignacion = 0
             END IF
             OUTPUT TO REPORT rpt_1(reg_1.*,reg_2.*,i)
             EXIT CASE
          WHEN 7
             SELECT SUM(B.importe)
             INTO   reg_2.normal
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "01"
             AND    B.subcuenta in ("08","24")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv
           
             SELECT SUM(B.importe)
             INTO   reg_2.unificacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "12"
             AND    B.subcuenta in ("08","24")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv

             SELECT SUM(B.importe)
             INTO   reg_2.asignacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "51"
             AND    B.subcuenta in ("08","24")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv
 
             IF reg_2.normal IS NULL THEN 
                 LET reg_2.normal = 0
             END IF
             IF reg_2.unificacion IS NULL THEN 
                 LET reg_2.unificacion = 0
             END IF
             IF reg_2.asignacion IS NULL THEN 
                 LET reg_2.asignacion = 0
             END IF
             OUTPUT TO REPORT rpt_1(reg_1.*,reg_2.*,i)
             EXIT CASE
          WHEN 8
             SELECT SUM(B.importe)
             INTO   reg_2.normal
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "01"
             AND    B.subcuenta in ("09","14","16")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv
           
             SELECT SUM(B.importe)
             INTO   reg_2.unificacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "12"
             AND    B.subcuenta in ("09","14","16")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv

             SELECT SUM(B.importe)
             INTO   reg_2.asignacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "51"
             AND    B.subcuenta in ("09","14","16")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv
 
             IF reg_2.normal IS NULL THEN 
                 LET reg_2.normal = 0
             END IF
             IF reg_2.unificacion IS NULL THEN 
                 LET reg_2.unificacion = 0
             END IF
             IF reg_2.asignacion IS NULL THEN 
                 LET reg_2.asignacion = 0
             END IF
             OUTPUT TO REPORT rpt_1(reg_1.*,reg_2.*,i)
             EXIT CASE
          WHEN 9
             SELECT SUM(B.importe)
             INTO   reg_2.normal
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "01"
             AND    B.subcuenta in ("07","33","34")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv
           
             SELECT SUM(B.importe)
             INTO   reg_2.unificacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "12"
             AND    B.subcuenta in ("07","33","34")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv

             SELECT SUM(B.importe)
             INTO   reg_2.asignacion
             FROM   safre_tmp:cza_hist_trab A,
                    safre_tmp:det_mov_trab B 
             WHERE  A.cont_serv = B.cont_serv
             AND    A.orig_tipo_trasp = "51"
             AND    B.subcuenta in ("07","33","34")
             AND    A.cve_recep_cuenta = reg_1.cve_recep_cuenta
             #AND    B.fecha_conversion < vv
 
             IF reg_2.normal IS NULL THEN 
                 LET reg_2.normal = 0
             END IF
             IF reg_2.unificacion IS NULL THEN 
                 LET reg_2.unificacion = 0
             END IF
             IF reg_2.asignacion IS NULL THEN 
                 LET reg_2.asignacion = 0
             END IF
             OUTPUT TO REPORT rpt_1(reg_1.*,reg_2.*,i)
             EXIT CASE

      END CASE
 END FOR
END FOREACH

       FINISH REPORT rpt_1
UPDATE safre_tmp:tra_ctr_historico
SET estado = "FIN"

END MAIN

REPORT rpt_1(reg_1,reg_2,sub)
#r1--------------------------

DEFINE reg_1 RECORD 
       cve_recep_cuenta CHAR(003),
       subtotal  INTEGER
END RECORD

DEFINE reg_2 RECORD 
       normal        ,
       unificacion   ,
       asignacion    DECIMAL(13,2)
END RECORD

DEFINE sub SMALLINT

OUTPUT
     PAGE LENGTH 90

     FORMAT
     PAGE HEADER    
     PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT 
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "======="
       SKIP 1 LINE 
        PRINT
            COLUMN 001,HOY USING"DD-MM-YYYY" ,
            COLUMN 155,"HISL002" 
	    PRINT
            COLUMN 001,HORA,
            COLUMN 050,"            REPORTE DE INFORMACION HISTORICA",
            COLUMN 153,reg_afore.razon_social CLIPPED," AFORE "
        PRINT
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "======="     
         PRINT
            COLUMN 001,"SUBCUENTA"                        ,
            COLUMN 022,"NORMAL"                        ,
            COLUMN 054,"UNIFICACION"                        ,
            COLUMN 085,"ASIGNACION"                            
        PRINT
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "======="                              
BEFORE GROUP OF reg_1.cve_recep_cuenta

SKIP 1 LINE
       PRINT "Afore     : ",reg_1.cve_recep_cuenta
       PRINT "Registros : ",reg_1.subtotal
       PRINT

ON EVERY ROW

PRINT
COLUMN 001,sub                                            ,
COLUMN 013,reg_2.normal     USING "-############&.&&"       ,
COLUMN 050,reg_2.unificacion  USING "-############&.&&"       ,
COLUMN 080,reg_2.asignacion  USING "-############&.&&"

AFTER GROUP OF reg_1.cve_recep_cuenta
SKIP 1 LINES
        PRINT
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "======="                              
PRINT
COLUMN 013,GROUP SUM(reg_2.normal) USING "-############&.&&",
COLUMN 050,GROUP SUM(reg_2.unificacion) USING "-#############&.&&",
COLUMN 080,GROUP SUM(reg_2.asignacion) USING "-############&.&&"
SKIP 1 LINE  
        PRINT
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "======="                              
END REPORT                             
