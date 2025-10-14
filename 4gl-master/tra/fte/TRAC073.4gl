##############################################################################
#Owner             => E.F.P.
#Programa TRAC073  => CONFRONTA AUTOMATICA TRASPASOS I-A ISSSTE
#Fecha creacion    => 13 OCTUBRE 2003    
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 04 DE MARZO DEL 2005
#Ultima Mod        => ENERO DEL 2006
#Sistema           => TRA-ICE-ISSSTE
##############################################################################
DATABASE safre_af 

GLOBALS
DEFINE  txt_upd         CHAR(100)
DEFINE  tot_nss         SMALLINT
DEFINE  enter           CHAR(001),
        result_compara  CHAR(001),
        c_validacion    CHAR(008),
        n_seguro_10     CHAR(011),
        v_nombre_afore  CHAR(120)

DEFINE  v_folio_interno INTEGER,
        v_tipo          SMALLINT

DEFINE  reg_tra_det_atm_issste RECORD LIKE tra_det_atm_issste.*

DEFINE  HOY             DATE
DEFINE folito           INTEGER
DEFINE reg_a_proc       INTEGER
DEFINE reg_proc         INTEGER
DEFINE reg_acep         INTEGER 
DEFINE reg_acep_dup     INTEGER
DEFINE reg_rech         INTEGER
DEFINE reg_rech_dup     INTEGER
DEFINE g_param_taa    RECORD LIKE seg_modulo.* 
DEFINE g_listita        CHAR(200)
DEFINE permisos         CHAR(100)
DEFINE usuario          CHAR(08) 
DEFINE cod_afore        LIKE tab_afore_local.codigo_afore
DEFINE raz_social       LIKE tab_afore_local.razon_social
DEFINE criterio         LIKE tra_aut_criterio.criterio_cod 
DEFINE desc_crite       LIKE tra_aut_criterio.criterio_desc
END GLOBALS

MAIN 

DEFINE l_cve CHAR(015),
       l_tipo_criterio SMALLINT ,
       l_criterio_desc CHAR(015)

LET v_folio_interno = ARG_VAL(1) 

CALL init()
CALL valida_folio() 

UPDATE tra_ctr_automatico
SET estado = "FIN"

DECLARE cur_ctr  CURSOR FOR
  SELECT b.icefa_desc,a.tipo_criterio,c.descripcion
  INTO   l_cve,l_tipo_criterio,l_criterio_desc 
  FROM   tra_det_atm_issste a , 
         tab_icefa          b ,
         tra_tab_valcri     c 
  WHERE  a.cve_ced_cuenta = b.icefa_cod 
  AND    a.tipo_criterio  = c.criterio_cod
  AND    a.folio_interno  = v_folio_interno
  GROUP BY 1,2,3

  FOREACH cur_ctr INTO   l_cve,l_tipo_criterio,l_criterio_desc 
   INSERT INTO tra_ctr_folio 
   VALUES (v_folio_interno ,
           l_cve           ,
           l_tipo_criterio ,
           l_criterio_desc )
   END FOREACH
CALL inf_reporte() 

END MAIN

FUNCTION init()
#i-------------
LET HOY = TODAY

SELECT USER
   INTO usuario
   FROM  safre_af:tab_afore_local

SELECT *
   INTO g_param_taa.*
   FROM seg_modulo
WHERE  modulo_cod = 'tra' 

CREATE TEMP TABLE tmp_nss_issste(nss char(011))
DATABASE safre_tmp

SELECT "X"
   FROM systables
WHERE tabname =  "tmp_rfc_10"

IF ( STATUS = NOTFOUND ) THEN
   CREATE TABLE tmp_rfc_10(n_seguro char(011),n_rfc_13 char(013),n_rfc_10 char(010))
  
ELSE 
   DROP TABLE tmp_rfc_10
   CREATE TABLE tmp_rfc_10(n_seguro char(011),n_rfc_13 char(013),n_rfc_10 char(010))
END IF

CREATE INDEX i_tmp_rfc13 ON tmp_rfc_10(n_rfc_13)
CREATE INDEX i_tmp_rfc10 ON tmp_rfc_10(n_rfc_10)
DATABASE safre_af

END FUNCTION

FUNCTION valida_folio()
#b--------------------
    DEFINE l_n_seguro             CHAR(011)    
    DEFINE charac                 CHAR(001)    , 
           charac1                CHAR(001)    ,
           v_rfc_ent_10           CHAR(010)    ,
           v_cadena               CHAR(120)    ,
           v_cadena1              CHAR(120)    ,
           cadena_compacta_afore  CHAR(120)    ,
           cadena_compacta_icefa  CHAR(120)    ,
           paterno                CHAR(040)    ,  
           materno                CHAR(040)    ,
           nombre                 CHAR(040)

    DEFINE m                                   ,
           i                                   ,
           cuenta                 SMALLINT

    UPDATE STATISTICS FOR TABLE tra_det_atm_issste

    INSERT INTO safre_tmp:tmp_rfc_10 
    SELECT a.n_seguro,a.n_rfc,a.n_rfc[1,10] 
    FROM   afi_mae_afiliado a

    DATABASE safre_tmp
     UPDATE STATISTICS FOR TABLE tmp_rfc_10 
    DATABASE safre_af
    DECLARE cur_1 CURSOR FOR 

    SELECT A.* 
    FROM   tra_det_atm_issste A
    WHERE  A.folio_interno    = v_folio_interno
    AND    A.estado           = 0

    FOREACH cur_1 INTO reg_tra_det_atm_issste.*

    LET c_validacion = "00000000"

    SELECT "OK" 
    FROM   tab_icefa A
    WHERE  A.icefa_cod = reg_tra_det_atm_issste.cve_ced_cuenta
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
       LET c_validacion = "222" 
       UPDATE tra_det_atm_issste 
       SET    tra_det_atm_issste.cad_valida = c_validacion ,
              tra_det_atm_issste.usuario    = usuario        
       WHERE  tra_det_atm_issste.correlativo = 
	      reg_tra_det_atm_issste.correlativo
    END IF      


    IF c_validacion = "00000000" THEN

    FOR i = 1 TO 2

        CASE i 
    
         WHEN 1

             LET v_rfc_ent_10 = reg_tra_det_atm_issste.rfc_ent[1,10],"*"

             DELETE FROM tmp_nss_issste

             INSERT INTO tmp_nss_issste
             SELECT a.n_seguro                       ## rfc a 13
             FROM   safre_tmp:tmp_rfc_10 a
             WHERE  a.n_rfc_13 = reg_tra_det_atm_issste.rfc_ent
             
             SELECT "ok" 
             FROM   tmp_nss_issste a
             GROUP BY 1

             IF STATUS <> NOTFOUND THEN
                LET c_validacion[1,2] = "10"
             ELSE 
               INSERT INTO tmp_nss_issste
               SELECT a.n_seguro                    ## rfc a 10
               FROM   safre_tmp:tmp_rfc_10 a
               WHERE  a.n_rfc_10 = v_rfc_ent_10

                 SELECT "ok" 
                 FROM   tmp_nss_issste a
                 GROUP BY 1
          
                  IF STATUS <> NOTFOUND THEN 
                     LET c_validacion[1,2] = "01"
                  END IF 
              END IF 

              IF c_validacion[1,2] = "00" THEN 
                 EXIT FOR       ## si no a 10 ni a 13 se sale de validacion
              END IF

         EXIT CASE

         WHEN 2 
             LET tot_nss = 0
             SELECT COUNT(UNIQUE nss)
             INTO tot_nss
             FROM tmp_nss_issste a

             IF tot_nss = 1 THEN


             SELECT UNIQUE a.nss 
             INTO   l_n_seguro 
             FROM   tmp_nss_issste a

             SELECT A.paterno   ,
                    A.materno   ,
                    A.nombres
             INTO   paterno ,
                    materno ,
                    nombre
             FROM   safre_af:afi_mae_afiliado A
             WHERE  A.n_seguro = l_n_seguro

             LET v_cadena[001,040] = paterno
             LET v_cadena[041,080] = materno
             LET v_cadena[081,120] = nombre

          CALL compacta_cadena(v_cadena)
          RETURNING cadena_compacta_afore

          LET result_compara = "0"
 
          LET v_cadena1[001,040] = reg_tra_det_atm_issste.nombre_ent[1,40]
          LET v_cadena1[041,080] = reg_tra_det_atm_issste.nombre_ent[41,80]
          LET v_cadena1[081,120] = reg_tra_det_atm_issste.nombre_ent[81,120]

            CALL compacta_cadena(v_cadena1)
            RETURNING cadena_compacta_icefa

            IF cadena_compacta_afore = cadena_compacta_icefa THEN
               LET c_validacion[3,8] = "111111"
            ELSE
               CALL compara_cadena(cadena_compacta_afore,cadena_compacta_icefa)
               RETURNING result_compara
            END IF

            IF result_compara = "1" THEN
               LET c_validacion[3,8] = "111110"
               EXIT FOR 
            END IF 
           END IF
          EXIT CASE
          OTHERWISE
          EXIT CASE
   END CASE
 END FOR
END IF

        SELECT a.criterio_cod,a.tipo 
        INTO criterio,v_tipo
        FROM tra_tab_valcri a
        WHERE a.vector = c_validacion

        CASE v_tipo
        WHEN 1

               SELECT "OK"
               FROM   tra_det_atm_issste  A
               WHERE  A.n_seguro       = reg_tra_det_atm_issste.n_seguro 
               AND    A.n_seguro_ent   = reg_tra_det_atm_issste.n_seguro_ent
               AND    A.rfc_ent        = reg_tra_det_atm_issste.rfc_ent
               AND    A.cve_ced_cuenta = reg_tra_det_atm_issste.cve_ced_cuenta 
               AND    A.nro_ctrl_icefa = reg_tra_det_atm_issste.nro_ctrl_icefa
               AND    A.nombre_ent     = reg_tra_det_atm_issste.nombre_ent
               AND    A.correlativo    <> reg_tra_det_atm_issste.correlativo
               AND    A.estado         <> 0
	            GROUP BY 1

             IF STATUS =  NOTFOUND THEN

                UPDATE tra_det_atm_issste
                SET    tra_det_atm_issste.estado        = 110          ,
                       tra_det_atm_issste.cad_valida    = c_validacion ,
                       tra_det_atm_issste.tipo_criterio = criterio     ,
                       tra_det_atm_issste.fecha_edo     = TODAY        ,
                       tra_det_atm_issste.usuario       = usuario      ,
                       tra_det_atm_issste.n_seguro      = l_n_seguro 
             WHERE     tra_det_atm_issste.folio_interno = v_folio_interno
               AND     tra_det_atm_issste.correlativo   =          
                       reg_tra_det_atm_issste.correlativo

              ELSE
                SELECT "OK"
                FROM   tra_det_atm_issste A
                WHERE  A.n_seguro       = reg_tra_det_atm_issste.n_seguro 
                AND    A.n_seguro_ent   = reg_tra_det_atm_issste.n_seguro_ent
                AND    A.rfc_ent        = reg_tra_det_atm_issste.rfc_ent
                AND    A.cve_ced_cuenta = reg_tra_det_atm_issste.cve_ced_cuenta 
                AND    A.nro_ctrl_icefa = reg_tra_det_atm_issste.nro_ctrl_icefa
                AND    A.nombre_ent     = reg_tra_det_atm_issste.nombre_ent
                AND    A.correlativo  <> reg_tra_det_atm_issste.correlativo
                AND    (A.estado       IN (110,160,161,163) OR
                        A.estado       IN (SELECT B.estado FROM tra_status B))
                                         
                GROUP BY 1
       
                IF STATUS <> NOTFOUND THEN

                UPDATE tra_det_atm_issste
                SET    tra_det_atm_issste.estado        = 111          ,
                       tra_det_atm_issste.cad_valida    = c_validacion ,
                       tra_det_atm_issste.tipo_criterio      = criterio,
                       tra_det_atm_issste.fecha_edo     = TODAY,
                       tra_det_atm_issste.usuario       = usuario,
                       tra_det_atm_issste.n_seguro      = l_n_seguro 
                WHERE  tra_det_atm_issste.folio_interno = v_folio_interno
                AND    tra_det_atm_issste.correlativo   = 
                       reg_tra_det_atm_issste.correlativo

                ELSE 

                UPDATE tra_det_atm_issste
                SET    tra_det_atm_issste.estado        = 110          ,
                       tra_det_atm_issste.cad_valida    = c_validacion,
                       tra_det_atm_issste.tipo_criterio = criterio,
                       tra_det_atm_issste.fecha_edo     = TODAY,
                       tra_det_atm_issste.usuario       = usuario ,
                       tra_det_atm_issste.n_seguro      = l_n_seguro 
                WHERE  tra_det_atm_issste.folio_interno = v_folio_interno
                AND    tra_det_atm_issste.correlativo   = 
                       reg_tra_det_atm_issste.correlativo
 
                END IF 
                END IF  
                EXIT CASE

          WHEN 2

               SELECT "OK"
               FROM   tra_det_atm_issste  A
               WHERE  A.n_seguro       = reg_tra_det_atm_issste.n_seguro 
               AND    A.n_seguro_ent   = reg_tra_det_atm_issste.n_seguro_ent
               AND    A.rfc_ent        = reg_tra_det_atm_issste.rfc_ent
               AND    A.cve_ced_cuenta = reg_tra_det_atm_issste.cve_ced_cuenta 
               AND    A.nro_ctrl_icefa = reg_tra_det_atm_issste.nro_ctrl_icefa
               AND    A.nombre_ent     = reg_tra_det_atm_issste.nombre_ent
               AND    A.correlativo   <> reg_tra_det_atm_issste.correlativo
               AND    A.estado        <> 0
	       GROUP BY 1

             IF STATUS = NOTFOUND THEN
                 UPDATE tra_det_atm_issste
                 SET    tra_det_atm_issste.estado        = 112          ,
                        tra_det_atm_issste.cad_valida    = c_validacion,
                        tra_det_atm_issste.tipo_criterio      = criterio,
                        tra_det_atm_issste.fecha_edo     = TODAY ,
                        tra_det_atm_issste.usuario       = usuario 
                 WHERE  tra_det_atm_issste.folio_interno = v_folio_interno
                 AND    tra_det_atm_issste.correlativo   =          
                        reg_tra_det_atm_issste.correlativo

              ELSE
                SELECT "OK"
                FROM   tra_det_atm_issste A
                WHERE  A.n_seguro       = reg_tra_det_atm_issste.n_seguro 
                AND    A.n_seguro_ent   = reg_tra_det_atm_issste.n_seguro_ent
                AND    A.rfc_ent        = reg_tra_det_atm_issste.rfc_ent
                AND    A.cve_ced_cuenta = reg_tra_det_atm_issste.cve_ced_cuenta 
                AND    A.nro_ctrl_icefa = reg_tra_det_atm_issste.nro_ctrl_icefa
                AND    A.nombre_ent     = reg_tra_det_atm_issste.nombre_ent
                AND    A.estado         IN (112,113,116,117)
                AND    A.correlativo    <> reg_tra_det_atm_issste.correlativo
                GROUP BY 1
       
                IF STATUS <> NOTFOUND THEN

                UPDATE tra_det_atm_issste
                SET    tra_det_atm_issste.estado        = 113          ,
                       tra_det_atm_issste.cad_valida    = c_validacion,
                       tra_det_atm_issste.tipo_criterio      = criterio,
                       tra_det_atm_issste.fecha_edo     = TODAY,
                       tra_det_atm_issste.usuario       = usuario
                WHERE  tra_det_atm_issste.folio_interno = v_folio_interno
                AND    tra_det_atm_issste.correlativo   = 
                       reg_tra_det_atm_issste.correlativo

                ELSE 

                UPDATE tra_det_atm_issste
                SET    tra_det_atm_issste.estado        = 112          ,
                       tra_det_atm_issste.cad_valida    = c_validacion,
                       tra_det_atm_issste.tipo_criterio      = criterio,
                       tra_det_atm_issste.fecha_edo     = TODAY ,
                       tra_det_atm_issste.usuario       = usuario
                WHERE  tra_det_atm_issste.folio_interno = v_folio_interno
                AND    tra_det_atm_issste.correlativo   = 
                       reg_tra_det_atm_issste.correlativo
 
                END IF 
                END IF 
                EXIT CASE

               OTHERWISE
               EXIT CASE
               END CASE
     END FOREACH

END FUNCTION

FUNCTION compacta_cadena(fv_cadena)
#cc-------------------------------

DEFINE fv_cadena  CHAR(120)
DEFINE fv_cadena1 CHAR(120)
DEFINE charac     CHAR(001)
DEFINE k,l        SMALLINT
LET l = 1
                 FOR k=1 TO 120
                      LET charac = fv_cadena[k]
                      IF charac <> " " THEN
                         LET fv_cadena1[l] =  charac
                         LET l=l+1
                      END IF
                 END FOR 

RETURN fv_cadena1

END FUNCTION

FUNCTION compara_cadena(cadena_afore,cadena_icefa)
#cc-----------------------------------------------

DEFINE o SMALLINT
DEFINE cadena_afore CHAR(120)
DEFINE cadena_icefa CHAR(120)
DEFINE result_band  CHAR(001)
DEFINE cuenta SMALLINT
LET cuenta = 0
                 FOR o = 1 TO 120  
                     IF cadena_afore[o] = cadena_icefa[o] THEN
                     ELSE
                        LET cuenta = cuenta + 1
                        IF cuenta > 2 THEN
                           EXIT FOR
                        END IF
                     END IF
                 END FOR
                        
                 IF cuenta <= 2 THEN
                    LET result_band = "1"
                 ELSE 
                    LET result_band = "0"
                 END IF
RETURN result_band
END FUNCTION
###############################################################################
FUNCTION  inf_reporte() 

   LET g_listita =  g_param_taa.ruta_listados CLIPPED,"/",
       usuario CLIPPED,".",
       "TRAC071",".",
       HOY USING "YYYYMMDD",".F",v_folio_interno USING"&&&&&"

   START REPORT r_confauto TO g_listita 

  WHENEVER ERROR CONTINUE 
   LET permisos  ='chmod 777 ',g_listita
   RUN permisos
  WHENEVER ERROR STOP

      SELECT a.folio_interno,a.total_reg 
      INTO folito,reg_a_proc 
      FROM tra_ctr_automatico a
      WHERE a.folio_interno = v_folio_interno
     
      SELECT count(*)
      INTO reg_proc
      FROM  tra_det_atm_issste a
      WHERE a.folio_interno = v_folio_interno 
      AND a.tipo_criterio in (1,2)
      AND a.estado not in (0,400);

      SELECT count(*)
         INTO  reg_acep
         FROM  tra_det_atm_issste a
      WHERE a.folio_interno = v_folio_interno 
	AND a.tipo_criterio in (1,2)
        AND a.estado = 110;

      SELECT count(*)
         INTO  reg_acep_dup
         FROM  tra_det_atm_issste a
      WHERE a.folio_interno = v_folio_interno 
	AND a.tipo_criterio in (1,2)
        AND a.estado = 111;

      SELECT count(*)
         INTO  reg_rech
         FROM  tra_det_atm_issste a
      WHERE a.folio_interno = v_folio_interno 
	AND a.tipo_criterio in (1,2)
        AND a.estado = 112;

      SELECT count(*)
         INTO  reg_rech_dup
         FROM  tra_det_atm_issste a
      WHERE a.folio_interno = v_folio_interno 
	AND a.tipo_criterio in (1,2)
        AND a.estado = 113;

      OUTPUT TO REPORT r_confauto ( folito,
                                    reg_a_proc,
                                    reg_proc,
                                    reg_acep,
                                    reg_acep_dup,
                                    reg_rech,
                                    reg_rech_dup, 
                                    criterio,
                                    desc_crite )

   FINISH REPORT r_confauto 

END FUNCTION 
##############################################################################
REPORT r_confauto( r_folito,
                     r_reg_a_proc,
                     r_reg_proc,
                     r_reg_acep,
                     r_reg_acep_dup,
                     r_reg_rech,
                     r_reg_rech_dup, 
                     r_criterio,
                     r_desc_crite )

DEFINE r_folito           INTEGER
DEFINE r_reg_a_proc       INTEGER
DEFINE r_reg_proc         INTEGER
DEFINE r_reg_acep         INTEGER 
DEFINE r_reg_acep_dup     INTEGER
DEFINE r_reg_rech         INTEGER
DEFINE r_reg_rech_dup     INTEGER
DEFINE r_criterio         LIKE tra_aut_criterio.criterio_cod 
DEFINE r_desc_crite       LIKE tra_aut_criterio.criterio_desc

OUTPUT
   TOP MARGIN 1
   BOTTOM MARGIN 0
   LEFT MARGIN 0
   RIGHT MARGIN 0
   PAGE LENGTH 60
   ORDER BY r_folito 

   FORMAT
        PAGE HEADER
        SELECT codigo_afore,razon_social
        INTO cod_afore,raz_social
        FROM tab_afore_local

        PRINT COLUMN 70,"Pagina:",PAGENO USING "<<<<"
        PRINT COLUMN 01, "_______________________________________________________________________________"
        PRINT COLUMN 01,"TRAC071",
              COLUMN 10,"REPORTE CONFRONTA AUTOMATICA DE TRASPASOS (CRUCES SAR92)",
              COLUMN 68,TODAY USING "dd-mm-yyyy"
        PRINT COLUMN 24,"         TRASPASOS ICEFA-AFORE ISSSTE          "
        PRINT COLUMN 01,cod_afore USING "&&&","  ",raz_social CLIPPED
        PRINT COLUMN 01, "_______________________________________________________________________________"

        PRINT 

       BEFORE GROUP OF r_folito 

          PRINT COLUMN 01, "_______________________________________________________________________________"

          PRINT COLUMN 01,"FOLIO INTERNO:",folito USING "#####"
             --   COLUMN 34,"CRITERIO:"," ",r_criterio USING "#"," ","DESCRIPCION:",r_desc_crite CLIPPED

          PRINT COLUMN 01, "_______________________________________________________________________________"

       ON EVERY ROW

          PRINT 
          PRINT 
          PRINT 
          PRINT 
          PRINT 
          PRINT 
          PRINT 
          PRINT 
          PRINT 
          PRINT 
          PRINT 
          PRINT 
          PRINT 
          PRINT 
          PRINT 
          PRINT COLUMN 20,"REGISTROS A PROCESAR           :",r_reg_a_proc USING "###,##&"
          PRINT
          PRINT COLUMN 20,"REGISTROS PROCESADOS           :",r_reg_proc USING "###,##&"
          PRINT
          PRINT COLUMN 20,"REGISTROS ACEPTADOS            :",r_reg_acep USING "###,##&"
          PRINT 
          PRINT COLUMN 20,"REGISTROS ACEPTADOS DUPLICADOS :",r_reg_acep_dup USING "###,##&"
          PRINT
          PRINT COLUMN 20,"REGISTROS RECHAZADOS           :",r_reg_rech USING "###,##&"
          PRINT
          PRINT COLUMN 20,"REGISTROS RECHAZADOS DUPLICADOS:",r_reg_rech_dup USING "###,##&"

          PRINT
          PRINT
          PRINT
          PRINT
          PRINT
          PRINT
          PRINT
          PRINT
          PRINT
          PRINT
          PRINT
          PRINT
          PRINT
          PRINT
          PRINT
          PRINT
          PRINT
          PRINT
          PRINT
          PRINT COLUMN 01,"_______________________________________________________________________________"


       PRINT 

    PAGE TRAILER
        SKIP 2 LINE
        PAUSE "Presione enter para continuar...."

END REPORT                      

####################################################################
{--- LETRA PEQUEÑA PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'      
--- NEGRITA Y GRANDE PRINT '\033e\033(s218T\033(s9H\033(s7B'
--- PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'      
}
