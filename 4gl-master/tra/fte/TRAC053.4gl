##############################################################################
#Owner             => E.F.P.
#Programa TRAC053  => CONFRONTA AUTOMATICA DE TRASPASOS ICE-AFO (CRUCE SAR92)
#Fecha creacion    => 07 ENERO 2002      
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 09 DE FEBRERO DEL 2005
#Sistema           => TRA-ICE-IMSS
##############################################################################
DATABASE safre_af 

GLOBALS

DEFINE  enter           CHAR(001),
        result_compara  CHAR(001),
        c_validacion    CHAR(003),
        n_seguro_10     CHAR(011),
        v_nombre_afore  CHAR(120)

DEFINE  v_correlativo   INTEGER,
        v_folio_interno INTEGER,
        v_tipo          SMALLINT

DEFINE  reg_tra_det_automatico RECORD LIKE safre_tmp:tra_det_automatico.*

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
DEFINE criterio         LIKE safre_af:tra_aut_criterio.criterio_cod 
DEFINE desc_crite       LIKE safre_af:tra_aut_criterio.criterio_desc
END GLOBALS

MAIN 

LET v_folio_interno = ARG_VAL(1) 
{
WHENEVER ERROR CONTINUE
DATABASE safre_tmp
DROP TABLE  tmp_afi_mae_afiliado
CREATE TABLE tmp_afi_mae_afiliado(n_seguro char(10),n_rfc(10))
WHENEVER ERROR STOP

INSERT INTO safre_tmp:tmp_afi_mae_afiliado 
SELECT a.n_seguro[1,10],b.n_rfc[1,10]
FROM   safre_af:afi_mae_afiliado a

create index tmp_ama1 ON tmp_afi_mae_afiliado(n_seguro)
create index tmp_ama2 ON tmp_afi_mae_afiliado(n_rfc)
UPDATE statistics for table tmp_afi_mae_afiliado

DATABASE safre_af

}
CALL init()
CALL valida_folio() 

UPDATE safre_tmp:tra_ctr_automatico
SET estado = "FIN"

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


END FUNCTION

FUNCTION valida_folio()
#b--------------------
    DEFINE charac                 CHAR(001)    , 
           charac1                CHAR(001)    ,
           v_rfc_ent              CHAR(010)    ,
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
{
    DATABASE safre_tmp
    UPDATE STATISTICS FOR TABLE tra_det_automatico
    DATABASE safre_af
}
    DECLARE cur_1 CURSOR FOR 

    SELECT A.* 
    FROM   safre_tmp:tra_det_automatico A
    WHERE  A.folio_interno    = v_folio_interno
    AND    A.estado           = 0

    FOREACH cur_1 INTO reg_tra_det_automatico.*

    LET c_validacion = "000"

    SELECT "OK" 
    FROM   tab_icefa A
    WHERE  A.icefa_cod = reg_tra_det_automatico.cve_ced_cuenta
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
       LET c_validacion = "222" 
       UPDATE safre_tmp:tra_det_automatico 
       SET    safre_tmp:tra_det_automatico.cad_valida = c_validacion
       WHERE  safre_tmp:tra_det_automatico.correlativo = 
	      reg_tra_det_automatico.correlativo
    END IF      

    SELECT "OK" 
    FROM   safre_af:afi_mae_afiliado A
    WHERE  A.n_seguro = reg_tra_det_automatico.n_seguro
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
       LET c_validacion = "333" 
       UPDATE safre_tmp:tra_det_automatico 
       SET    safre_tmp:tra_det_automatico.cad_valida = c_validacion
       WHERE  safre_tmp:tra_det_automatico.correlativo = 
	      reg_tra_det_automatico.correlativo
    END IF      

    IF c_validacion = "000" THEN

        FOR i = 1 TO 4

        CASE i 
    
         WHEN 1
             LET n_seguro_10 = reg_tra_det_automatico.n_seguro_ent[1,10],"*"

             SELECT "OK"
             FROM   safre_af:afi_mae_afiliado A
             WHERE  A.n_seguro matches n_seguro_10
             GROUP BY 1

             IF STATUS <> NOTFOUND THEN
                LET c_validacion[1] = "1"
             END IF
         EXIT CASE

         WHEN 2

             LET  v_rfc_ent = reg_tra_det_automatico.rfc_ent[1,10]

             SELECT "OK"
             FROM   safre_af:afi_mae_afiliado A
             WHERE  A.n_seguro = reg_tra_det_automatico.n_seguro
	     AND    A.n_rfc[1,10]  = v_rfc_ent
             GROUP BY 1

             IF STATUS <> NOTFOUND THEN
                LET c_validacion[2] = "1"
             END IF
         EXIT CASE

         WHEN 3 

             SELECT A.paterno   ,
                    A.materno   ,
                    A.nombres
             INTO   paterno ,
                    materno ,
                    nombre
             FROM   safre_af:afi_mae_afiliado A
             WHERE  A.n_seguro = reg_tra_det_automatico.n_seguro

             IF  STATUS <> NOTFOUND THEN

             LET v_cadena[001,040] = paterno
             LET v_cadena[041,080] = materno
             LET v_cadena[081,120] = nombre

          CALL compacta_cadena(v_cadena)
          RETURNING cadena_compacta_afore

          FOR m = 1 TO 5 

            LET result_compara = "0"
 
           CASE m 

           WHEN 1 
            LET v_cadena1[001,040] = reg_tra_det_automatico.nombre_ent[1,40]
            LET v_cadena1[041,080] = reg_tra_det_automatico.nombre_ent[41,80]
            LET v_cadena1[081,120] = reg_tra_det_automatico.nombre_ent[81,120]

            CALL compacta_cadena(v_cadena1)
            RETURNING cadena_compacta_icefa

            IF cadena_compacta_afore = cadena_compacta_icefa THEN
               LET result_compara = "1"
            ELSE
               CALL compara_cadena(cadena_compacta_afore,cadena_compacta_icefa)
               RETURNING result_compara
            END IF

            IF result_compara = "1" THEN
               LET c_validacion[3] = "1"
               EXIT FOR 
            END IF 
            EXIT CASE
           WHEN  2
            LET v_cadena1[001,040] = reg_tra_det_automatico.nombre_ent[41,80]
            LET v_cadena1[041,080] = reg_tra_det_automatico.nombre_ent[81,120]
            LET v_cadena1[081,120] = reg_tra_det_automatico.nombre_ent[1,40]

            CALL compacta_cadena(v_cadena1)
            RETURNING cadena_compacta_icefa

            IF cadena_compacta_afore = cadena_compacta_icefa THEN
               LET result_compara = "1"
            ELSE
               CALL compara_cadena(cadena_compacta_afore,cadena_compacta_icefa)
               RETURNING result_compara
            END IF
            IF result_compara = "1" THEN
               LET c_validacion[3] = "1"
               EXIT FOR
            END IF
            EXIT CASE

           WHEN  3
            LET v_cadena1[001,040] = reg_tra_det_automatico.nombre_ent[81,120]
            LET v_cadena1[041,080] = reg_tra_det_automatico.nombre_ent[1,40]
            LET v_cadena1[081,120] = reg_tra_det_automatico.nombre_ent[41,80]

            CALL compacta_cadena(v_cadena1)
            RETURNING cadena_compacta_icefa

            IF cadena_compacta_afore = cadena_compacta_icefa THEN
               LET result_compara = "1"
            ELSE
               CALL compara_cadena(cadena_compacta_afore,cadena_compacta_icefa)
               RETURNING result_compara
            END IF
            IF result_compara = "1" THEN
               LET c_validacion[3] = "1"
               EXIT FOR
            END IF
            EXIT CASE
            EXIT CASE

           WHEN  4
            LET v_cadena1[001,040] = reg_tra_det_automatico.nombre_ent[41,80]
            LET v_cadena1[041,080] = reg_tra_det_automatico.nombre_ent[1,41]
            LET v_cadena1[081,120] = reg_tra_det_automatico.nombre_ent[81,120]

            CALL compacta_cadena(v_cadena1)
            RETURNING cadena_compacta_icefa

            IF cadena_compacta_afore = cadena_compacta_icefa THEN
               LET result_compara = "1"
            ELSE
               CALL compara_cadena(cadena_compacta_afore,cadena_compacta_icefa)
               RETURNING result_compara
            END IF
            IF result_compara = "1" THEN
               LET c_validacion[3] = "1"
               EXIT FOR
            END IF
            EXIT CASE

           WHEN  5

            CALL compacta_cadena(reg_tra_det_automatico.nombre_ent)
            RETURNING cadena_compacta_icefa

            IF cadena_compacta_afore = cadena_compacta_icefa THEN
               LET result_compara = "1"
            ELSE
               CALL compara_cadena(cadena_compacta_afore,cadena_compacta_icefa)
               RETURNING result_compara
            END IF
            IF result_compara = "1" THEN
               LET c_validacion[3] = "1"
               EXIT FOR
            END IF
            EXIT CASE

           OTHERWISE 
            EXIT CASE

           END CASE
           END FOR  
          END IF
         EXIT CASE

         WHEN 4 

             SELECT A.paterno   ,
                    A.materno   ,
                    A.nombres
             INTO   paterno ,
                    materno ,
                    nombre
             FROM   safre_af:afi_mae_afiliado A
             WHERE  A.n_seguro = reg_tra_det_automatico.n_seguro

             IF  STATUS <> NOTFOUND THEN

             LET v_cadena1[001,040] = reg_tra_det_automatico.nombre_ent[1,40]
             LET v_cadena1[041,080] = reg_tra_det_automatico.nombre_ent[41,80]
             LET v_cadena1[081,120] = reg_tra_det_automatico.nombre_ent[81,120]

          CALL compacta_cadena(v_cadena1)
          RETURNING cadena_compacta_icefa

          FOR m = 1 TO 5 

            LET result_compara = "0"
 
           CASE m 

           WHEN 1 
            LET v_cadena[001,040] = paterno
            LET v_cadena[041,080] = materno
            LET v_cadena[081,120] = nombre

            CALL compacta_cadena(v_cadena)
            RETURNING cadena_compacta_afore

            IF cadena_compacta_icefa = cadena_compacta_afore THEN
               LET result_compara = "1"
            ELSE
               CALL compara_cadena(cadena_compacta_icefa,cadena_compacta_afore)
               RETURNING result_compara
            END IF

            IF result_compara = "1" THEN
               LET c_validacion[3] = "1"
               EXIT FOR 
            END IF 
            EXIT CASE
           WHEN  2
            LET v_cadena[001,040] = materno
            LET v_cadena[041,080] = nombre
            LET v_cadena[081,120] = paterno

            CALL compacta_cadena(v_cadena)
            RETURNING cadena_compacta_afore

            IF cadena_compacta_icefa = cadena_compacta_afore THEN
               LET result_compara = "1"
            ELSE
               CALL compara_cadena(cadena_compacta_icefa,cadena_compacta_afore)
               RETURNING result_compara
            END IF
            IF result_compara = "1" THEN
               LET c_validacion[3] = "1"
               EXIT FOR
            END IF
            EXIT CASE

           WHEN  3
            LET v_cadena[001,040] = nombre
            LET v_cadena[041,080] = paterno
            LET v_cadena[081,120] = materno

            CALL compacta_cadena(v_cadena)
            RETURNING cadena_compacta_afore

            IF cadena_compacta_icefa = cadena_compacta_afore THEN
               LET result_compara = "1"
            ELSE
               CALL compara_cadena(cadena_compacta_icefa,cadena_compacta_afore)
               RETURNING result_compara
            END IF
            IF result_compara = "1" THEN
               LET c_validacion[3] = "1"
               EXIT FOR
            END IF
            EXIT CASE
            EXIT CASE

           WHEN  4
            LET v_cadena[001,040] = materno
            LET v_cadena[041,080] = paterno
            LET v_cadena[081,120] = nombre

            CALL compacta_cadena(v_cadena)
            RETURNING cadena_compacta_afore

            IF cadena_compacta_icefa = cadena_compacta_afore THEN
               LET result_compara = "1"
            ELSE
               CALL compara_cadena(cadena_compacta_icefa,cadena_compacta_afore)
               RETURNING result_compara
            END IF
            IF result_compara = "1" THEN
               LET c_validacion[3] = "1"
               EXIT FOR
            END IF
            EXIT CASE

           WHEN  5
            LET v_nombre_afore = paterno,materno,nombre
            CALL compacta_cadena(v_nombre_afore)
            RETURNING cadena_compacta_afore

            IF cadena_compacta_icefa = cadena_compacta_afore THEN
               LET result_compara = "1"
            ELSE
               CALL compara_cadena(cadena_compacta_icefa,cadena_compacta_afore)
               RETURNING result_compara
            END IF
            IF result_compara = "1" THEN
               LET c_validacion[3] = "1"
               EXIT FOR
            END IF
            EXIT CASE

           OTHERWISE 
            EXIT CASE

           END CASE
           END FOR  
          END IF
         EXIT CASE
         OTHERWISE 
         EXIT CASE
        END CASE
        END FOR
    END IF

        SELECT a.tipo 
        INTO v_tipo
        FROM safre_af:tra_tab_validacion a
        WHERE a.c_validacion = c_validacion

        CASE v_tipo
        WHEN 1

             INITIALIZE v_correlativo TO NULL

               SELECT "OK"
               FROM   safre_tmp:tra_det_automatico  A
               WHERE  A.n_seguro       = reg_tra_det_automatico.n_seguro 
               AND    A.n_seguro_ent   = reg_tra_det_automatico.n_seguro_ent
               AND    A.rfc_ent        = reg_tra_det_automatico.rfc_ent
               AND    A.cve_ced_cuenta = reg_tra_det_automatico.cve_ced_cuenta 
               AND    A.nro_ctrl_icefa = reg_tra_det_automatico.nro_ctrl_icefa
               AND    A.correlativo  <> reg_tra_det_automatico.correlativo
               AND    A.estado         <> 0
	       GROUP BY 1

             IF STATUS =  NOTFOUND THEN

                UPDATE safre_tmp:tra_det_automatico
                SET    safre_tmp:tra_det_automatico.estado      = 10          ,
                       safre_tmp:tra_det_automatico.cad_valida  = c_validacion,
                       safre_tmp:tra_det_automatico.fecha_edo   = TODAY
             WHERE  safre_tmp:tra_det_automatico.folio_interno = v_folio_interno
               AND    safre_tmp:tra_det_automatico.correlativo =          
                                    reg_tra_det_automatico.correlativo

              ELSE
                SELECT "OK"
                FROM   safre_tmp:tra_det_automatico A
                WHERE  A.n_seguro       = reg_tra_det_automatico.n_seguro 
                AND    A.n_seguro_ent   = reg_tra_det_automatico.n_seguro_ent
                AND    A.rfc_ent        = reg_tra_det_automatico.rfc_ent
                AND    A.cve_ced_cuenta = reg_tra_det_automatico.cve_ced_cuenta 
                AND    A.nro_ctrl_icefa = reg_tra_det_automatico.nro_ctrl_icefa
                AND    A.correlativo  <> reg_tra_det_automatico.correlativo
                AND    A.estado         IN (10,11,14,15,30,32,34,36,50)
                GROUP BY 1
       
                IF STATUS <> NOTFOUND THEN

                UPDATE safre_tmp:tra_det_automatico
                SET    safre_tmp:tra_det_automatico.estado = 11,
                       safre_tmp:tra_det_automatico.cad_valida =c_validacion 
            WHERE  safre_tmp:tra_det_automatico.folio_interno = v_folio_interno
                AND    safre_tmp:tra_det_automatico.correlativo = 
                       reg_tra_det_automatico.correlativo

                ELSE 

                UPDATE safre_tmp:tra_det_automatico
                SET    safre_tmp:tra_det_automatico.estado = 10,
                       safre_tmp:tra_det_automatico.cad_valida =c_validacion
            WHERE  safre_tmp:tra_det_automatico.folio_interno = v_folio_interno
                AND    safre_tmp:tra_det_automatico.correlativo = 
                       reg_tra_det_automatico.correlativo
 
                END IF 
                END IF  
                EXIT CASE

          WHEN 2

             INITIALIZE v_correlativo TO NULL

               SELECT "OK"
               FROM   safre_tmp:tra_det_automatico  A
               WHERE  A.n_seguro       = reg_tra_det_automatico.n_seguro 
               AND    A.n_seguro_ent   = reg_tra_det_automatico.n_seguro_ent
               AND    A.rfc_ent        = reg_tra_det_automatico.rfc_ent
               AND    A.cve_ced_cuenta = reg_tra_det_automatico.cve_ced_cuenta 
               AND    A.nro_ctrl_icefa = reg_tra_det_automatico.nro_ctrl_icefa
               AND    A.correlativo  <> reg_tra_det_automatico.correlativo
               AND    A.estado         <> 0
	       GROUP BY 1

             IF STATUS = NOTFOUND THEN
                 UPDATE safre_tmp:tra_det_automatico
                 SET    safre_tmp:tra_det_automatico.estado      = 12          ,
                        safre_tmp:tra_det_automatico.cad_valida  = c_validacion,
                        safre_tmp:tra_det_automatico.fecha_edo   = TODAY
             WHERE  safre_tmp:tra_det_automatico.folio_interno = v_folio_interno
                 AND    safre_tmp:tra_det_automatico.correlativo =          
                        reg_tra_det_automatico.correlativo

              ELSE
                SELECT "OK"
                FROM   safre_tmp:tra_det_automatico A
                WHERE  A.n_seguro       = reg_tra_det_automatico.n_seguro 
                AND    A.n_seguro_ent   = reg_tra_det_automatico.n_seguro_ent
                AND    A.rfc_ent        = reg_tra_det_automatico.rfc_ent
                AND    A.cve_ced_cuenta = reg_tra_det_automatico.cve_ced_cuenta 
                AND    A.nro_ctrl_icefa = reg_tra_det_automatico.nro_ctrl_icefa
                AND    A.estado         IN (12,13,16,17)
                AND    A.correlativo  <> reg_tra_det_automatico.correlativo
                GROUP BY 1
       
                IF STATUS <> NOTFOUND THEN

                UPDATE safre_tmp:tra_det_automatico
                SET    safre_tmp:tra_det_automatico.estado = 13,
                       safre_tmp:tra_det_automatico.cad_valida  = c_validacion
            WHERE  safre_tmp:tra_det_automatico.folio_interno = v_folio_interno
                AND    safre_tmp:tra_det_automatico.correlativo = 
                       reg_tra_det_automatico.correlativo

                ELSE 

                UPDATE safre_tmp:tra_det_automatico
                SET    safre_tmp:tra_det_automatico.estado = 12,
                       safre_tmp:tra_det_automatico.cad_valida  = c_validacion
            WHERE  safre_tmp:tra_det_automatico.folio_interno = v_folio_interno
                AND    safre_tmp:tra_det_automatico.correlativo = 
                       reg_tra_det_automatico.correlativo
 
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
       "CAUTO",".",
       HOY USING "YYYYMMDD",".F",v_folio_interno USING"&&&&&"

   LET permisos  ='chmod 777 ',g_listita
   RUN permisos

   START REPORT r_confauto TO g_listita 

      SELECT a.folio_interno,a.total_reg 
      INTO folito,reg_a_proc 
      FROM safre_tmp:tra_ctr_automatico a
      WHERE a.folio_interno = v_folio_interno
     
      SELECT count(*)
  INTO reg_proc
      FROM  safre_tmp:tra_det_automatico a
      WHERE a.folio_interno = v_folio_interno 
      AND a.estado <> 0;

      SELECT count(*)
         INTO  reg_acep
         FROM  safre_tmp:tra_det_automatico a
      WHERE a.folio_interno = v_folio_interno 
        AND a.estado = 10;

      SELECT count(*)
         INTO  reg_acep_dup
         FROM  safre_tmp:tra_det_automatico a
      WHERE a.folio_interno = v_folio_interno 
        AND a.estado = 11;

      SELECT count(*)
         INTO  reg_rech
         FROM  safre_tmp:tra_det_automatico a
      WHERE a.folio_interno = v_folio_interno 
        AND a.estado = 12;

      SELECT count(*)
         INTO  reg_rech_dup
         FROM  safre_tmp:tra_det_automatico a
      WHERE a.folio_interno = v_folio_interno 
        AND a.estado = 13;

      SELECT a.tipo_criterio
         INTO criterio
         FROM safre_tmp:tra_det_automatico a
      WHERE a.folio_interno = v_folio_interno 
      GROUP BY 1

      SELECT a.criterio_desc
         INTO desc_crite
         FROM safre_af:tra_aut_criterio a
      WHERE a.criterio_cod = criterio 


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
DEFINE r_criterio         LIKE safre_af:tra_aut_criterio.criterio_cod 
DEFINE r_desc_crite       LIKE safre_af:tra_aut_criterio.criterio_desc

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
        PRINT COLUMN 01,"TRAC051",
              COLUMN 10,"REPORTE CONFRONTA AUTOMATICA DE TRASPASOS (CRUCES SAR92)",
              COLUMN 68,TODAY USING "dd-mm-yyyy"
        PRINT COLUMN 24,"         TRASPASOS ICEFA-AFORE IMSS             "
        PRINT COLUMN 01,cod_afore USING "&&&","  ",raz_social CLIPPED
        PRINT COLUMN 01, "_______________________________________________________________________________"

        PRINT 

       BEFORE GROUP OF r_folito 

          PRINT COLUMN 01, "_______________________________________________________________________________"

          PRINT COLUMN 01,"FOLIO INTERNO:",folito USING "#####",
                COLUMN 34,"CRITERIO:"," ",r_criterio USING "#"," ","DESCRIPCION:",r_desc_crite CLIPPED

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
