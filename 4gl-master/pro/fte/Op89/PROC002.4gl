################################################################################
#PROYECTO          => SISTEMA DE safre_af ( MEXICO )                           #
#SISTEMA           => PRO                                                      #
#PROGRAMA PROC002  => RECEPCION DE ARCHIVOS ENVIADOS POR LA CONSAR             #
#FECHA             => 05 DE FEBRERO DEL 2000                                   #
#BY                => FRANCO ESTEBAN ULLOA VIDELA                              #
#FECHA ACTUALIZ.   => 29 DE MARZO DEL 2004                                     #
#ACTUALIZACION     => LAURA EUGENIA CORTES GUZMAN                              #
#Modificado Por    => Isabel Fonseca Frias                                     #
#Fecha             => 26 de Febrero del 2008                                   #
#Observacion       => Se agregaron modificaciones de acuerdo a MPT version     #
#                  => 3.0   (v1)                                               #
#Modificado        => Isabel Fonseca Frias                                     #
#Fecha             => 23 Septiembre del 2008                                   #
#Observacion       => El estado = 3 se modifica por estado 2 ya que es el      #
#                  => estado con el que queda cuando se genera el archivo      #
#                  => pro_capacitacion                                         #
#                  => (v2)                                                     #
#Modificado Por    => Isabel Fonseca Frias                                     #
#Fecha             => 22-12-2008                                               #
#Observacion       => Se actualiza el estado en pro_capacitacon con cualquier  #
#                  => diagnostico (v5)                                         #
#Modificado Por    => Isabel Fonseca Frias                                     #
#Fecha             => 28 de septiembre del 2009                                #
#Observacion       => Se agregaron modificaciones de acuerdo a MPT             #
#                  => con fecha 29/07/2009                                     #
#                  => (v10)                                                    #
#SISTEMA           => PRO                                                      #
################################################################################
DATABASE safre_af
GLOBALS

    DEFINE arr_1 ARRAY[10] OF RECORD #glo #arr_1
                descripcion           CHAR(30) ,
                nro_registros_ok      SMALLINT
           END RECORD,

           parametro         RECORD LIKE seg_modulo.*,

           reg_1 RECORD 
                nom_archivo      CHAR(20)
           END RECORD,

           HOY                   ,   
           vfecha_proceso        DATE,

           wnom_archivo          CHAR(100) ,
           enter                 CHAR(001) ,
           G_LISTA               CHAR(100) ,
           cve_scb_303           CHAR(2)   ,     --(v100)
           carga_fecha           CHAR(375) ,
           vfecha                CHAR(010) ,

           cuantos               ,  
           s_status_interno      SMALLINT,

           ultimo_folio          ,    
           ultimo_lote           ,    
           cont_1                INTEGER 
END GLOBALS

MAIN
    OPTIONS
        INPUT WRAP          ,
        PROMPT LINE LAST    ,
        ACCEPT KEY CONTROL-I

    DEFER INTERRUPT

    CALL STARTLOG("PROC002.log")
    WHENEVER ERROR CONTINUE
        CREATE TEMP TABLE pro_plano_carga
        (
         n_registros              CHAR(450)
        )

        CREATE TEMP TABLE pro_resumen
        (
         descripcion              CHAR(50) ,
         nro_registros            SMALLINT
        )

    WHENEVER ERROR STOP

    CALL STARTLOG("PROC002.log")

    CALL init() #i
    OPEN WINDOW proc0021 AT 4,4 WITH FORM "PROC0021" ATTRIBUTE(BORDER)
    DISPLAY "                           < Ctrl-C > Sa",
            "lir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PROC002        CARGA DE ARCHIVO GENERADO POR LA ",
            "CONSAR                        " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    DISPLAY " ANTES DE EJECUTAR ESTE PROCESO DEBE VERIFICAR QUE EL ARCHIVO "
            AT 5,8
    DISPLAY " DEVUELTO   POR  CONSAR   ESTE  UBICADO   EN   EL  DIRECTORIO "
            AT 6,8
    DISPLAY parametro.ruta_rescate CLIPPED AT 7,9

    INPUT BY NAME reg_1.nom_archivo WITHOUT DEFAULTS
        AFTER FIELD nom_archivo
            IF reg_1.nom_archivo IS NULL OR
               reg_1.nom_archivo =  "  " THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD nom_archivo
            ELSE
               SELECT  "B.OK"
               FROM    pro_cza_agte B
               WHERE   B.nom_archivo = reg_1.nom_archivo

               IF STATUS <> NOTFOUND THEN
                   ERROR "   ARCHIVO YA PROCESADO ANTERIORMENTE"
                         ATTRIBUTE(NORMAL)
                   NEXT FIELD nom_archivo
               END IF
            END IF

        ON KEY (ESC)
            IF reg_1.nom_archivo IS NULL OR
               reg_1.nom_archivo =  "  " THEN
               ERROR " CAMPO NO PUEDE SER NULO "
               NEXT FIELD nom_archivo
           ELSE
              SELECT  "B.OK"
              FROM    pro_cza_agte B
              WHERE   B.nom_archivo = reg_1.nom_archivo

              IF STATUS <> NOTFOUND THEN
                  ERROR "   ARCHIVO YA PROCESADO ANTERIORMENTE"
                  ATTRIBUTE(NORMAL)
                  NEXT FIELD nom_archivo
              END IF
           END IF

           WHENEVER ERROR CONTINUE

           LET wnom_archivo = parametro.ruta_rescate CLIPPED,"/",
                              reg_1.nom_archivo

           LOAD FROM wnom_archivo INSERT INTO pro_plano_carga

           SELECT count(*)
           INTO   cuantos
           FROM   pro_plano_carga

           IF cuantos = 0 THEN
              ERROR "   NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO"
              ATTRIBUTE(NORMAL)
              NEXT FIELD nom_archivo
           ELSE
              EXIT INPUT
           END IF    

           WHENEVER ERROR STOP


        ON KEY (INTERRUPT,CONTROL-C)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
    END INPUT
    
    CALL primer_paso()  #pp
    CALL segundo_paso() #sp
    CALL tercer_paso()  #tp

    PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
END MAIN

FUNCTION primer_paso()
#pp-------------------
    SELECT MAX(folio)
    INTO   ultimo_folio
    FROM   pro_folio

    LET ultimo_folio = ultimo_folio + 1

    IF ultimo_folio IS NULL THEN
        LET ultimo_folio = 1
    END IF

    LET ultimo_lote = ultimo_folio - 1

    INSERT INTO pro_folio VALUES(ultimo_folio)
    DISPLAY " FOLIO ",ultimo_folio AT 18,1
END FUNCTION

FUNCTION segundo_paso()
#sp-------------------
DEFINE reg_cza RECORD 
           nro_registro          INTEGER  ,
           fecha_proceso         DATE     ,
           lote_de_consar        INTEGER  ,
           nro_reg_301           LIKE pro_cza_agte.nro_reg_301,
           nro_reg_302           LIKE pro_cza_agte.nro_reg_302,
           nro_reg_303           LIKE pro_cza_agte.nro_reg_303,
           nro_reg_304           LIKE pro_cza_agte.nro_reg_304,
           nro_reg_306           LIKE pro_cza_agte.nro_reg_306,
           nro_reg_307           LIKE pro_cza_agte.nro_reg_307,
           nro_reg_308           LIKE pro_cza_agte.nro_reg_308,   --(v1)
           ind_resp              LIKE pro_cza_agte.ind_resp,
           ind_rechazo           LIKE pro_cza_agte.ind_rechazo
END RECORD,
        
       reg_det RECORD
           nombre                CHAR(40) ,
           paterno               CHAR(40) ,
           materno               CHAR(40) ,
           rfc_letras            CHAR(04) ,
           rfc_numeros           INTEGER  ,
           rfc_homonimia         CHAR(03) ,
           n_unico               CHAR(18) ,             
           fecha_solicitud       DATE     ,
           fecha_registro        DATE     ,
           cod_promotor          CHAR(10) ,
           codven                CHAR(10) ,
           diag_proceso          CHAR(02) ,
           fecha_proceso         DATE     ,
           lote_afore            INTEGER  ,
           cve_afore_consar      SMALLINT ,
           nombre_consar         CHAR(40) ,
           paterno_consar        CHAR(40) ,
           materno_consar        CHAR(40) ,
           rfc_letras_consar     CHAR(04) ,
           rfc_numeros_consar    INTEGER  ,
           rfc_homo_consar       CHAR(03) ,
           nss_consar            CHAR(11) ,
           curp_consar           CHAR(18) ,
           fnaci_consar          DATE     ,
           resuelva              SMALLINT ,
           fecha_vence           DATE     ,
           cod_res_op            SMALLINT                     --(v10)

END RECORD,
-- (v10)   inicia
       reg_2G RECORD                                           

           codven                LIKE pro_mae_promotor.codven,
           seguro                LIKE pro_mae_promotor.seguro,
           nip                   LIKE pro_mae_promotor.nip,
           agenc_cod             LIKE pro_mae_promotor.agenc_cod,
           unico                 LIKE pro_mae_promotor.unico,
           rfc                   LIKE pro_mae_promotor.rfc,
           paterno               LIKE pro_mae_promotor.paterno,
           materno               LIKE pro_mae_promotor.materno,
           nombres               LIKE pro_mae_promotor.nombres,
           fingre                LIKE pro_mae_promotor.fingre,
           fenvio                LIKE pro_mae_promotor.fenvio,
           fecha_registro        LIKE pro_mae_promotor.fecha_registro,
           fecha_baja            LIKE pro_mae_promotor.fecha_baja,
           calle                 LIKE pro_mae_promotor.calle,
           numero                LIKE pro_mae_promotor.numero,
           dpto                  LIKE pro_mae_promotor.dpto,
           colonia               LIKE pro_mae_promotor.colonia,
           deleg                 LIKE pro_mae_promotor.deleg,
           ciudad                LIKE pro_mae_promotor.ciudad,
           estado                LIKE pro_mae_promotor.estado,
           codpos                LIKE pro_mae_promotor.codpos,
           fono                  LIKE pro_mae_promotor.fono,
           sup                   LIKE pro_mae_promotor.sup,
           nivel                 LIKE pro_mae_promotor.nivel,
           resuelva              LIKE pro_mae_promotor.resuelva,
           fnaci                 LIKE pro_mae_promotor.fnaci,
           diag_proceso          LIKE pro_mae_promotor.diag_proceso,
           fautoriz              LIKE pro_mae_promotor.fautoriz,
           status                LIKE pro_mae_promotor.status,
           nro_solicitud         LIKE pro_mae_promotor.nro_solicitud,
           status_interno        LIKE pro_mae_promotor.status_interno,
           fecha_certifi         LIKE pro_mae_promotor.fecha_certifi,
           motivo_suspende       LIKE pro_mae_promotor.motivo_suspende,
           fecha_suspende        LIKE pro_mae_promotor.fecha_suspende,
           fech_credencial       LIKE pro_mae_promotor.fech_credencial,
           cod_promotor          LIKE pro_mae_promotor.cod_promotor,
           tipo_recibo           LIKE pro_mae_promotor.tipo_recibo,
           escolar               LIKE pro_mae_promotor.escolar

END RECORD,
-- (v10) fin
       reg_det_reval RECORD
           cod_promotor          LIKE pro_det_revalida.cod_promotor  ,
           fecha_reval           LIKE pro_det_revalida.fecha_reval   ,
           diag_reval            LIKE pro_det_revalida.diag_reval    ,
           fecha_proceso         LIKE pro_det_revalida.fecha_proceso ,
           lote_afore            LIKE pro_det_revalida.lote_afore    ,
           fecha_emision         LIKE pro_det_revalida.fecha_emision ,
           fecha_vence           LIKE pro_det_revalida.fecha_vence
END RECORD,

       reg_recepcion_scb RECORD
           cod_promotor          CHAR(10) ,
           fecha_scb             DATE     ,
           cve_scb               CHAR(2)  ,
           fecha_proceso         DATE     ,
           lote_afore            INTEGER  ,
           ind_orig_scb          LIKE pro_recep_scb.ind_orig_scb,  
           cod_res_op            SMALLINT                      --(v10)
END RECORD,

       reg_resul_mod RECORD
           cod_promotor          CHAR(10) ,
           diag_proceso          CHAR(2)  ,
           fecha_proceso         DATE     ,
           lote_afore            INTEGER  ,
           nombre                CHAR(40) ,
           paterno               CHAR(40) ,
           materno               CHAR(40) ,
           rfc_letras            CHAR(04) ,
           rfc_numeros           CHAR(06) ,
           rfc_homonimia         CHAR(3)  ,
           nss                   CHAR(11) ,
           curp                  CHAR(18) ,
           fnaci                 DATE     ,
           ind_orig_mod          LIKE pro_resul_mod.ind_orig_mod,
           calle_num_mod         LIKE pro_resul_mod.calle_num_mod,
           colonia_mod           LIKE pro_resul_mod.colonia_mod,
           deleg_mod             LIKE pro_resul_mod.deleg_mod,
           ciudad_mod            LIKE pro_resul_mod.ciudad_mod,
           estado_mod            LIKE pro_resul_mod.estado_mod,
           codpos_mod            LIKE pro_resul_mod.codpos_mod,
           cod_res_op            LIKE pro_resul_mod.cod_res_op,      --(v10)
           sexo                  SMALLINT,
           ef_naci               CHAR(02),
           telefono              CHAR(100),
           grado_escol           CHAR(01),
           tipo_mod              CHAR(08) 
END RECORD,

       reg_resul_examen RECORD
           cod_promotor          CHAR(10) ,
           calificacion          SMALLINT ,
           diag_proceso          CHAR(2)  ,
           fecha_proceso         DATE     ,
           nro_lote              INTEGER
END RECORD,

       reg_aviso_examen RECORD
           cod_promotor          CHAR(10) ,
           nombres               CHAR(40) ,
           paterno               CHAR(40) ,
           materno               CHAR(40) ,
           fecha_examen          DATE     ,
           horario_examen        CHAR(5)  ,
           lugar_examen          LIKE pro_aviso_examen.lugar_examen,
           fecha_proceso         DATE     ,
           lote_afore            INTEGER,
           diag_proceso          CHAR(02)                         --(v1)
END RECORD,
 
       reg_3 RECORD
           diag_proceso          CHAR(2)  ,
           status                SMALLINT ,
           status_interno        SMALLINT ,
           motivo_suspende       CHAR(2)  ,
           fecha_baja            DATE     ,
           fecha_suspende        DATE
END RECORD,

       c13_rfc               CHAR(013) ,
       c10_rfc               CHAR(010) ,
       c10_cod_promotor      CHAR(010) ,
       c10_fecha             CHAR(010) ,
       carga_reg             CHAR(450) ,
       vrfc                  CHAR(013) ,
       vrfc_2                CHAR(013) ,
       vpaterno              CHAR(40)  ,
       vmaterno              CHAR(40)  ,
       vnombres              CHAR(40)   ,

       diferencia            ,
       cont_difer_ok         ,
       cont_aviso_examen     ,
       cont_resul_examen     ,
       cont_agentes          ,
       cont_rechazos_ok      ,
       cont_reactiva_ok      ,
       cont_reactiva_ok2     ,                          --(v1)
       cont_altas_ok         ,
       cont_reval            ,
       cont_scb              ,
       cont_mod              ,
       cont_scb_2            ,
       cont_1                INTEGER

  DEFINE     lc_tipo_mod     CHAR(01)

DECLARE cur_1 CURSOR FOR

     SELECT *
     FROM   pro_plano_carga

     LET cont_aviso_examen = 0
     LET cont_resul_examen = 0
     LET cont_1            = 0
     LET cont_agentes      = 0
     LET cont_reactiva_ok  = 0
     LET cont_rechazos_ok  = 0
     LET cont_altas_ok     = 0
     LET cont_reval        = 0
     LET cont_scb          = 0
     LET cont_mod          = 0
     LET cont_scb_2        = 0
     LET cont_difer_ok     = 0
     LET diferencia        = 0
  
    FOREACH cur_1 INTO carga_reg
      LET cont_1 = cont_1 + 1
      LET diferencia        = 0

      IF carga_reg[1,3] = "301" THEN
          LET cont_agentes = cont_agentes + 1

          LET reg_det.nombre             = carga_reg[004,043] 
          LET reg_det.paterno            = carga_reg[044,083] 
          LET reg_det.materno            = carga_reg[084,123] 
          LET reg_det.rfc_letras         = carga_reg[124,127] 
          LET reg_det.rfc_numeros        = carga_reg[128,133] 
          LET reg_det.rfc_homonimia      = carga_reg[134,136] 
          LET reg_det.n_unico            = carga_reg[137,154] 

          LET c10_fecha                  = carga_reg[159,160],"/",
                                           carga_reg[161,162],"/",
                                           carga_reg[155,158]
          LET reg_det.fecha_solicitud    = c10_fecha

          LET c10_fecha                  = carga_reg[167,168],"/",
                                           carga_reg[169,170],"/",
                                           carga_reg[163,166]
          LET reg_det.fecha_registro     = c10_fecha

          LET reg_det.cod_promotor       = carga_reg[171,180] 

--(v10)  SE CAMBIARON LAS POSICIONES POR cod_res_op Y SE QUITO LA
--(v10)  FECHA DE PROCESO

          LET reg_det.cod_res_op         = carga_reg[181,182] --(v10)


--(v10)          LET c10_fecha                  = carga_reg[187,188],"/",
--(v10)                                           carga_reg[189,190],"/",
--(v10)                                           carga_reg[183,186]
--(v10)          LET reg_det.fecha_proceso      = c10_fecha

          LET reg_det.fecha_proceso      = HOY                 --(v10)
     
          LET reg_det.diag_proceso       = carga_reg[183,184]
          LET reg_det.lote_afore         = carga_reg[185,193]
          LET reg_det.cve_afore_consar   = carga_reg[194,196]

          LET reg_det.nombre_consar      = carga_reg[197,236]
          LET reg_det.paterno_consar     = carga_reg[237,276]
          LET reg_det.materno_consar     = carga_reg[277,316]
          LET reg_det.rfc_letras_consar  = carga_reg[317,320] 
          LET reg_det.rfc_numeros_consar = carga_reg[321,326] 
          LET reg_det.rfc_homo_consar    = carga_reg[327,329] 
          LET reg_det.nss_consar         = carga_reg[330,340] 
          LET reg_det.curp_consar        = carga_reg[341,358] 

          LET c10_fecha                  = carga_reg[363,364],"/",
                                           carga_reg[365,366],"/",
                                           carga_reg[359,362]
          LET reg_det.fnaci_consar       = c10_fecha

          LET reg_det.resuelva           = carga_reg[367,369]   --(v10)
          
--(v10)          LET c10_fecha                  = carga_reg[379,380],"/",
--(v10)                                           carga_reg[381,382],"/",
--(v10)                                           carga_reg[375,378]
--(v10)          LET reg_det.fecha_vence       = c10_fecha
          LET reg_det.fecha_vence       = "          "          --(v10) 

         
   
          LET c10_rfc = reg_det.rfc_letras    CLIPPED       ,
                        reg_det.rfc_numeros   USING"&&&&&&" 

           LET reg_det.cve_afore_consar = "301"

          SELECT UNIQUE A.status_interno ,
                 A.cod_promotor   ,
                 A.rfc            ,
                 A.paterno        ,
                 A.materno        ,
                 A.nombres
          INTO   s_status_interno ,
                 c10_cod_promotor ,
                 vrfc             ,
                 vpaterno         ,
                 vmaterno         ,
                 vnombres    
          FROM   pro_solicitud A
          WHERE  A.rfc[1,10]      = c10_rfc
          AND    A.status_interno IN(3,8,21)  -- 3  = Lote Generado
                                              -- 8  = Enviado Reactivacion
                                              -- 21 = Enviado Rechazado
 
          LET vrfc_2 = reg_det.rfc_letras    CLIPPED       ,
                       reg_det.rfc_numeros   USING"&&&&&&" ,
                       reg_det.rfc_homonimia CLIPPED       

          IF vrfc_2 <> vrfc  THEN
             LET  diferencia = 1
          END IF

          IF vpaterno <> reg_det.paterno THEN
             LET  diferencia = 1
          END IF

          IF vmaterno <> reg_det.materno  THEN
             LET  diferencia = 1
          END IF

          IF vnombres <> reg_det.nombre  THEN
             LET  diferencia = 1
          END IF


          IF  reg_det.diag_proceso <> "1A" 
          AND reg_det.diag_proceso <> "1R" 
          AND reg_det.diag_proceso <> "1K" THEN
             LET diferencia= 0
          END IF

          IF SQLCA.SQLCODE = 0  THEN

              CASE s_status_interno
              WHEN 3               --LOTE GENERADO 
                LET cont_altas_ok = cont_altas_ok + 1

                IF  reg_det.diag_proceso <> "1A" 
                AND reg_det.diag_proceso <> "1R" 
                AND reg_det.diag_proceso <> "1K" THEN  
                    LET reg_det.cod_promotor = c10_cod_promotor
                END IF

                IF diferencia = 0 THEN
                   UPDATE pro_solicitud 
                   SET  pro_solicitud.folio         = ultimo_folio          ,
                        pro_solicitud.status        = 1                     ,
                        pro_solicitud.status_interno= 4                     ,
                        pro_solicitud.cod_promotor  = reg_det.cod_promotor  ,
                        pro_solicitud.diag_proceso  = reg_det.diag_proceso  ,
                        pro_solicitud.fecha_registro= reg_det.fecha_registro,
                        pro_solicitud.fecha_proceso = reg_det.fecha_proceso
                  WHERE pro_solicitud.rfc[1,10]     = c10_rfc
                  AND   pro_solicitud.status_interno= 3


                   INSERT INTO pro_det_agte 
                          VALUES(ultimo_folio ,
                                 reg_det.*    ,
                                 4)
                ELSE  
                    LET cont_difer_ok = cont_difer_ok + 1

                    INSERT INTO pro_det_agte 
                           VALUES(ultimo_folio , reg_det.*, 6)
                END IF

             WHEN 8                 -- Enviado Reactivacion   
                 LET cont_reactiva_ok = cont_reactiva_ok + 1


                 IF  reg_det.diag_proceso <> "1A" 
                 AND reg_det.diag_proceso <> "1R" 
                 AND reg_det.diag_proceso <> "1K" THEN
                     LET reg_det.cod_promotor = c10_cod_promotor
                 END IF
						  
                 IF diferencia = 0 THEN
                       
                    UPDATE pro_solicitud 
                    SET pro_solicitud.folio         = ultimo_folio          ,
                        pro_solicitud.status        = 1                     ,
                        pro_solicitud.status_interno= 40                    ,
                        pro_solicitud.cod_promotor  = reg_det.cod_promotor  ,
                        pro_solicitud.diag_proceso  = reg_det.diag_proceso  ,
                        pro_solicitud.fecha_registro= reg_det.fecha_registro,
                        pro_solicitud.fecha_proceso = reg_det.fecha_proceso
                    WHERE  pro_solicitud.rfc[1,10]     = c10_rfc
                    AND    pro_solicitud.status_interno= 8
		    AND    pro_solicitud.cod_promotor NOT IN 
                           (SELECT cod_promotor                     --(v1) 
			    FROM   pro_mae_promotor                 --(v1)
--			   WHERE  diag_proceso IN ("7E","7T"))      --(v1)
			   WHERE  diag_proceso IN ("7X"))           --(v10)

                          INSERT INTO pro_det_agte 
                                VALUES(ultimo_folio ,
                                       reg_det.*    ,
                                       40)
                                    
                 ELSE  
                    LET cont_difer_ok = cont_difer_ok + 1
                    INSERT INTO pro_det_agte 
                           VALUES(ultimo_folio, reg_det.*, 6)
                 END IF
                 
              WHEN 21                 -- Enviado Rechazado

                 LET cont_rechazos_ok = cont_rechazos_ok + 1

                 IF  reg_det.diag_proceso <> "1A" 
                 AND reg_det.diag_proceso <> "1R" 
                 AND reg_det.diag_proceso <> "1K" THEN
                     LET reg_det.cod_promotor = c10_cod_promotor
                 END IF

                 IF diferencia = 0 THEN

                    UPDATE pro_solicitud 
                       SET pro_solicitud.folio         = ultimo_folio          ,
                           pro_solicitud.status        = 1                     ,
                           pro_solicitud.status_interno= 41                    ,
                           pro_solicitud.cod_promotor  = reg_det.cod_promotor  ,
                           pro_solicitud.diag_proceso  = reg_det.diag_proceso  ,
                           pro_solicitud.fecha_registro= reg_det.fecha_registro,
                           pro_solicitud.fecha_proceso = reg_det.fecha_proceso
                       WHERE  pro_solicitud.rfc[1,10]     = c10_rfc
                       AND    pro_solicitud.status_interno= 21

                       INSERT INTO pro_det_agte 
                              VALUES(ultimo_folio, reg_det.*, 41)
                    ELSE  
                       LET cont_difer_ok = cont_difer_ok + 1

                       INSERT INTO pro_det_agte 
                             VALUES(ultimo_folio, reg_det.*, 6)
                    END IF

              END CASE

          ELSE
              LET cont_altas_ok = cont_altas_ok + 1
              LET cont_difer_ok = cont_difer_ok + 1
              INSERT INTO pro_det_agte 
                     VALUES(ultimo_folio , reg_det.*, 6)

          END IF
          CONTINUE FOREACH
      END IF

      IF carga_reg[1,3] = "308" THEN                                   --(v1)
          LET cont_agentes = cont_agentes + 1

          LET reg_det.cod_promotor       = carga_reg[004,013]          
          LET reg_det.nombre             = carga_reg[014,053]          
          LET reg_det.paterno            = carga_reg[054,093]          
          LET reg_det.materno            = carga_reg[094,133]           
          LET reg_det.rfc_letras         = carga_reg[134,137]           
          LET reg_det.rfc_numeros        = carga_reg[138,143]           
          LET reg_det.rfc_homonimia      = carga_reg[144,146]           
          LET reg_det.n_unico            = carga_reg[147,164]          

          LET c10_fecha                  = carga_reg[169,170],"/",     
                                           carga_reg[171,172],"/",    
                                           carga_reg[165,168]         
          LET reg_det.fecha_solicitud    = c10_fecha                   

          LET c10_fecha                  = carga_reg[177,178],"/",     
                                           carga_reg[179,180],"/",     
                                           carga_reg[173,176]          
          LET reg_det.fecha_registro     = c10_fecha                   

         
          LET reg_det.cod_res_op         = carga_reg[181,182]          --(v10

-- (v10) Se recorren las posiciones

          LET reg_det.diag_proceso       = carga_reg[183,184]          


--(v10)   LET c10_fecha                  = carga_reg[187,188],"/",     
--(v10)                                    carga_reg[189,190],"/",     
--(v10)                                    carga_reg[183,186]          
--(v10)   LET reg_det.fecha_proceso      = c10_fecha     

          LET reg_det.fecha_proceso      = HOY                         --(v10) 

          LET reg_det.lote_afore         = carga_reg[185,193]          
          LET reg_det.cve_afore_consar   = carga_reg[194,196]          
          LET reg_det.nombre_consar      = carga_reg[197,236]          
          LET reg_det.paterno_consar     = carga_reg[237,276]          
          LET reg_det.materno_consar     = carga_reg[277,316]          
          LET reg_det.rfc_letras_consar  = carga_reg[317,320]         
          LET reg_det.rfc_numeros_consar = carga_reg[321,326]          
          LET reg_det.rfc_homo_consar    = carga_reg[327,329]          
          LET reg_det.nss_consar         = carga_reg[330,340]          
          LET reg_det.curp_consar        = carga_reg[341,358]         

          LET c10_fecha                  = carga_reg[363,364],"/",   
                                           carga_reg[365,366],"/",    
                                           carga_reg[359,362]          
          LET reg_det.fnaci_consar       = c10_fecha                  

          LET reg_det.resuelva           = carga_reg[367,369]          
          
--(v10)   LET c10_fecha                  = carga_reg[379,380],"/",     
--(v10)                                    carga_reg[381,382],"/",     
--(v10)                                    carga_reg[375,378]          
--(v10)          LET reg_det.fecha_vence       = c10_fecha     
          LET reg_det.fecha_vence       = "          "              --(v10)     
          
         
   
          LET c10_rfc = reg_det.rfc_letras    CLIPPED       ,
                        reg_det.rfc_numeros   USING"&&&&&&" 


          LET reg_det.cve_afore_consar = "308"
          
          LET vrfc_2        = null
          LET vrfc          = null
          LET vpaterno      = null
          LET vmaterno      = null
          LET vnombres      = null
          LET diferencia    = 0
         

          SELECT UNIQUE A.status_interno ,
                 A.cod_promotor   ,
                 A.rfc            ,
                 A.paterno        ,
                 A.materno        ,
                 A.nombres
          INTO   s_status_interno ,
                 c10_cod_promotor ,
                 vrfc             ,
                 vpaterno         ,
                 vmaterno         ,
                 vnombres    
          FROM   pro_solicitud A, pro_mae_promotor B         --(v1)
          WHERE  A.rfc[1,10]      = c10_rfc
          AND    A.status_interno in (8,62)   -- ENVIADO REACTIVACION   --(v10)
          AND    B.cod_promotor   = A.cod_promotor           --(v1)
--          AND    B.diag_proceso  in ("7E", "7T")           --(v1)
          AND    B.diag_proceso  in ("7X")                   --(v10)

          LET vrfc_2 = reg_det.rfc_letras    CLIPPED       ,
                       reg_det.rfc_numeros   USING"&&&&&&" ,
                       reg_det.rfc_homonimia CLIPPED       


display "c10_rfc           :",c10_rfc
display "vrfc_2            :",vrfc_2
display "vrfc              :",vrfc
display "vpaterno          :",vpaterno
display "reg_det.paterno   :",reg_det.paterno
display "vmaterno          :",vmaterno
display "reg_det.materno   :",reg_det.materno
display "vnombres          :",vnombres
display "reg_det.nombre    :",reg_det.nombre
sleep 6


          IF vrfc_2 <> vrfc  THEN
             LET  diferencia = 1
          END IF

          IF vpaterno <> reg_det.paterno THEN
             LET  diferencia = 1
          END IF

          IF vmaterno <> reg_det.materno  THEN
             LET  diferencia = 1
          END IF

          IF vnombres <> reg_det.nombre  THEN
             LET  diferencia = 1
          END IF

          IF  reg_det.diag_proceso <> "1R"  THEN
             LET diferencia= 0
          END IF


          IF SQLCA.SQLCODE = 0  THEN   -- si lo encontro

--  (v10)             CASE s_status_interno
--  (v10)            WHEN 8           --   ENVIADO-REACTIVACION

              IF s_status_interno = 8 OR                 --(v10)
                 s_status_interno = 62 THEN              --(v10)
                    LET cont_reactiva_ok2 = cont_reactiva_ok2 + 1

        
-- v10 Inicia
                    IF reg_det.diag_proceso = "9C" --Incon.datos.Renapo
                    OR reg_det.diag_proceso = "1I" THEN --Info.incompleta 


                       UPDATE pro_solicitud
                       SET pro_solicitud.folio         = ultimo_folio          ,
                           pro_solicitud.status_interno= 62                    ,
                           pro_solicitud.diag_proceso  = reg_det.diag_proceso  ,
                           pro_solicitud.fecha_registro= reg_det.fecha_registro,
                           pro_solicitud.fecha_proceso = reg_det.fecha_proceso
                       WHERE  pro_solicitud.rfc[1,10]     = c10_rfc
                       AND    pro_solicitud.status_interno in (8,62)  --(v10)

                       INSERT INTO pro_det_agte
                             VALUES(ultimo_folio ,
                                    reg_det.*    ,
                                    62)
                    END IF 

-- v10 Finaliza


                    IF reg_det.diag_proceso = "1R" THEN 
                                                 
                       IF reg_det.diag_proceso <> "1R"  THEN
                            LET reg_det.cod_promotor = c10_cod_promotor
                       END IF

                       IF diferencia = 0 THEN

                         UPDATE pro_solicitud 
                         SET pro_solicitud.folio         = ultimo_folio     ,
                            pro_solicitud.status         = 1                ,
                            pro_solicitud.status_interno = 40               ,
                            pro_solicitud.cod_promotor   = reg_det.cod_promotor,
                            pro_solicitud.diag_proceso   = reg_det.diag_proceso,
                            pro_solicitud.fecha_registro=reg_det.fecha_registro,
                            pro_solicitud.fecha_proceso  = reg_det.fecha_proceso
                          WHERE  pro_solicitud.rfc[1,10]      = c10_rfc
                          AND    pro_solicitud.status_interno in (8,62) --(v10)

                          INSERT INTO pro_det_agte 
                                VALUES(ultimo_folio ,
                                       reg_det.*    ,
                                       40)
                       ELSE  
                          LET cont_difer_ok = cont_difer_ok + 1
                       
                          INSERT INTO pro_det_agte 
                          VALUES(ultimo_folio, reg_det.*, 61)
                       END IF
                    END IF 
--  (v10)             END CASE
              END IF 

          ELSE
              LET cont_reactiva_ok2 = cont_reactiva_ok2 + 1         --(v1)
              LET cont_difer_ok = cont_difer_ok + 1

              INSERT INTO pro_det_agte 
              VALUES(ultimo_folio , reg_det.*, 61)

          END IF
          CONTINUE FOREACH
      END IF

-- V10 este detalle se cansela segun MPT 19-07-2009
{
      IF carga_reg[1,3] = "302" THEN
         LET cont_reval = cont_reval + 1

         LET reg_det_reval.cod_promotor  = carga_reg[04,13]

         LET c10_fecha                   = carga_reg[18,19],"/",
                                           carga_reg[20,21],"/",
                                           carga_reg[14,17]
         LET reg_det_reval.fecha_reval   = c10_fecha

         LET reg_det_reval.diag_reval    = carga_reg[22,23]

         LET c10_fecha                   = carga_reg[28,29],"/",
                                           carga_reg[30,31],"/",
                                           carga_reg[24,27]
         LET reg_det_reval.fecha_proceso = c10_fecha

         LET reg_det_reval.lote_afore    = carga_reg[32,40]
         
         LET c10_fecha                   = carga_reg[45,46],"/",
                                           carga_reg[47,48],"/",
                                           carga_reg[41,44]
         LET reg_det_reval.fecha_emision = c10_fecha         

         LET c10_fecha                   = carga_reg[53,54],"/",
                                           carga_reg[55,56],"/",
                                           carga_reg[49,52]
         LET reg_det_reval.fecha_vence   = c10_fecha       
         
         INSERT INTO pro_det_revalida 
                VALUES(ultimo_folio ,
                       reg_det_reval.*)

         CALL actualiza_revalidacion(reg_det_reval.cod_promotor ,
                                     reg_det_reval.fecha_reval  ,
                                     reg_det_reval.diag_reval   ,
                                     reg_det_reval.fecha_proceso,
                                     reg_det_reval.lote_afore) #ar

         CONTINUE FOREACH
      END IF
}--(v10)

      IF carga_reg[1,3] = "303" THEN
         LET cont_scb = cont_scb + 1

         LET reg_recepcion_scb.cod_promotor  = carga_reg[04,13]

         LET c10_fecha                       = carga_reg[18,19],"/",
                                               carga_reg[20,21],"/",
                                               carga_reg[14,17]
         LET reg_recepcion_scb.fecha_scb     = c10_fecha

         LET cve_scb_303                     = carga_reg[22,23]    --(v100)


--(v10)          LET c10_fecha                       = carga_reg[28,29],"/",
--(v10)                                                carga_reg[30,31],"/",
--(v10)                                                carga_reg[24,27]
--(v10)          LET reg_recepcion_scb.fecha_proceso = c10_fecha

         LET reg_recepcion_scb.fecha_proceso = HOY       --(v10)

         LET reg_recepcion_scb.lote_afore    = carga_reg[24,32]

         LET reg_recepcion_scb.ind_orig_scb  = carga_reg[33,33]
         LET reg_recepcion_scb.cod_res_op    = carga_reg[34,35]
         LET reg_recepcion_scb.cve_scb       = carga_reg[36,37]    --(v100)

--(v10) Nota: en el layout en la posicion 36,37 se menciona el diagnostico 
--            del proceso, este queda pendiente porque al parecer es el 
--            mismo que el id 4 (Clave de motivo de la bajaj o suspension) 


-- Bajas Rechazadas (2H,2I,2J)


         IF  reg_recepcion_scb.cve_scb <> "2C"       --(v1)
         AND reg_recepcion_scb.cve_scb <> "2P"       --(v10)
         AND reg_recepcion_scb.cve_scb <> "2R"       --(v1)
         AND reg_recepcion_scb.cve_scb <> "2T"       --(v1)
         AND reg_recepcion_scb.cve_scb <> "3C"       --(v1)
         AND reg_recepcion_scb.cve_scb <> "3E"       --(v1)
         AND reg_recepcion_scb.cve_scb <> "3D"       --(v10)
         AND reg_recepcion_scb.cve_scb <> "3T"       --(v1)
         AND reg_recepcion_scb.cve_scb <> "6A"       --(v1)
         AND reg_recepcion_scb.cve_scb <> "6B"       --(v1)
         AND reg_recepcion_scb.cve_scb <> "6C"       --(v1)
         AND reg_recepcion_scb.cve_scb <> "8B"       --(v1)
         AND reg_recepcion_scb.cve_scb <> "8C" THEN  --(v1)

            SELECT "m.OK"
            FROM   pro_envio_scb m
            WHERE  m.cod_promotor   = reg_recepcion_scb.cod_promotor
            AND    m.status_interno = 1
            GROUP BY 1

            IF STATUS = NOTFOUND THEN   --Cuando no existen

                                   #### BAJAS RECHAZADAS ####           --(v1)

               INSERT INTO pro_recep_scb
                      VALUES(ultimo_folio        ,
                             reg_recepcion_scb.* ,
                             6)

               UPDATE pro_mae_promotor
--             SET    pro_mae_promotor.diag_proceso    = reg_recepcion_scb.cve_scb   ,  --(v100)
               SET    pro_mae_promotor.diag_proceso    = cve_scb_303  ,                 --)v100) 
                      pro_mae_promotor.motivo_suspende = reg_recepcion_scb.cve_scb
               WHERE  pro_mae_promotor.cod_promotor    = reg_recepcion_scb.cod_promotor


            ELSE
                                    #### BAJAS DE LA AFORE ####

                LET cont_scb_2 = cont_scb_2 + 1

                UPDATE pro_envio_scb
                SET    pro_envio_scb.status_interno = 5,--------ACTUALIZADO
                       pro_envio_scb.folio_recepcion = ultimo_folio
                WHERE  pro_envio_scb.cod_promotor=reg_recepcion_scb.cod_promotor
                AND    pro_envio_scb.status_interno = 1

-- (v1) Inicia
                IF reg_recepcion_scb.cve_scb = "2J"                
                OR reg_recepcion_scb.cve_scb = "2H"             
                OR reg_recepcion_scb.cve_scb = "2M"             
                OR reg_recepcion_scb.cve_scb = "2I" THEN           

                   UPDATE pro_mae_promotor     
                   SET    pro_mae_promotor.status_interno = 5--ACTUALIZADO 
                   WHERE  pro_mae_promotor.cod_promotor   = reg_recepcion_scb.cod_promotor  
                   AND    pro_mae_promotor.status_interno = 3--ENVIADO #aqui tenia 1 
                ELSE                                                 
                    UPDATE pro_mae_promotor                 
                    SET    pro_mae_promotor.fecha_suspende  = reg_recepcion_scb.fecha_scb , 
                           pro_mae_promotor.motivo_suspende = reg_recepcion_scb.cve_scb   ,
                           pro_mae_promotor.status_interno  = 5---------ACTUALIZADO       
                    WHERE  pro_mae_promotor.cod_promotor   = reg_recepcion_scb.cod_promotor 
                    --AND    status_interno = 3---------ENVIADO #aqui tenia 1  
                END IF



                INSERT INTO pro_recep_scb
                       VALUES(ultimo_folio        ,
                              reg_recepcion_scb.* ,
                              5)
            END IF
            CONTINUE FOREACH
         ELSE
                                    #### BAJAS DE PROCESAR ####

             LET cont_scb_2 = cont_scb_2 + 1
-- (v1) Inicia
            IF  reg_recepcion_scb.cve_scb = "8B" THEN              

               select *                                               
               from pro_mae_promotor                                 
               where cod_promotor = reg_recepcion_scb.cod_promotor  
                 and diag_proceso  = "8A"                          

               IF STATUS = NOTFOUND THEN                         


                  PROMPT "El promotor  ",reg_recepcion_scb.cod_promotor, " no tiene diag. 8A"  FOR CHAR enter  

                 ELSE                                      
                    UPDATE pro_mae_promotor       
                    SET    pro_mae_promotor.fecha_suspende  = reg_recepcion_scb.fecha_scb , 
                           pro_mae_promotor.motivo_suspende = reg_recepcion_scb.cve_scb   , 
                           pro_mae_promotor.status_interno  = 5,---------ACTUALIZADO
                           pro_mae_promotor.status          = 3                              
                    WHERE  pro_mae_promotor.cod_promotor = reg_recepcion_scb.cod_promotor  
                 END IF                                
            END IF                                 

            IF  reg_recepcion_scb.cve_scb = "8C" THEN    

               select *                          
               from pro_mae_promotor       
               where cod_promotor = reg_recepcion_scb.cod_promotor 
                 and diag_proceso  = "8B"
                 IF status = notfound THEN 

                    PROMPT "El promotor  ",reg_recepcion_scb.cod_promotor, " no tiene diag. 8B"  FOR CHAR enter 
                 ELSE    
                    UPDATE pro_mae_promotor   
                    SET    pro_mae_promotor.fecha_suspende  = reg_recepcion_scb.fecha_scb ,  
                           pro_mae_promotor.motivo_suspende = reg_recepcion_scb.cve_scb   , 
                           pro_mae_promotor.status_interno  = 5,---------ACTUALIZADO   
                           pro_mae_promotor.status          = 3          
                    WHERE  pro_mae_promotor.cod_promotor = reg_recepcion_scb.cod_promotor     
                 END IF                         
            END IF                    

            IF  reg_recepcion_scb.cve_scb <> "8C"     
            AND  reg_recepcion_scb.cve_scb <> "8B" THEN   

                 UPDATE pro_mae_promotor               
                    SET    pro_mae_promotor.fecha_suspende  = reg_recepcion_scb.fecha_scb ,  
                           pro_mae_promotor.motivo_suspende = reg_recepcion_scb.cve_scb   ,
                           pro_mae_promotor.status_interno  = 5,---------ACTUALIZADO   
                           pro_mae_promotor.status          = 3            
                    WHERE  pro_mae_promotor.cod_promotor = reg_recepcion_scb.cod_promotor  
            END IF

             INSERT INTO pro_recep_scb
                    VALUES(ultimo_folio, reg_recepcion_scb.*, 5)
             CONTINUE FOREACH
         END IF

      END IF

      IF carga_reg[1,3] = "304" THEN
         LET cont_mod = cont_mod + 1

--(v10) se recorrieron las posiciones segun MPT de l 29-07-09
--(v10)         LET c10_fecha                   = carga_reg[020,021],"/",
--(v10)                                           carga_reg[022,023],"/",
--(v10)                                           carga_reg[016,019]
--(v10)         LET reg_resul_mod.fecha_proceso = c10_fecha

         LET reg_resul_mod.fecha_proceso = HOY                  --(v10)     
--       LET reg_resul_mod.ind_orig_mod  = carga_reg[197,197]   -- no viene 0ct 2010
         LET reg_resul_mod.ind_orig_mod  = ''                                         

         --- Nuevo layout 304 Oct 2010 
         LET reg_resul_mod.cod_promotor  = carga_reg[004,013]
         LET reg_resul_mod.nombre        = carga_reg[014,053]
         LET reg_resul_mod.paterno       = carga_reg[054,093]
         LET reg_resul_mod.materno       = carga_reg[094,133]
         LET reg_resul_mod.rfc_letras    = carga_reg[134,137]
         LET reg_resul_mod.rfc_numeros   = carga_reg[138,143]
         LET reg_resul_mod.rfc_homonimia = carga_reg[144,146]
         LET reg_resul_mod.nss           = carga_reg[147,157]
         LET reg_resul_mod.curp          = carga_reg[158,175]

         LET c10_fecha                   = carga_reg[180,181],"/",
                                           carga_reg[182,183],"/",
                                           carga_reg[176,179]
         LET reg_resul_mod.fnaci         = c10_fecha

         LET reg_resul_mod.sexo          = carga_reg[184,184]
         LET reg_resul_mod.ef_naci       = carga_reg[185,186]
                                            
         LET reg_resul_mod.calle_num_mod = carga_reg[187,226]
         LET reg_resul_mod.colonia_mod   = carga_reg[227,256]
         LET reg_resul_mod.deleg_mod     = carga_reg[257,286]
         LET reg_resul_mod.ciudad_mod    = carga_reg[287,316]
         LET reg_resul_mod.estado_mod    = carga_reg[317,318]
         LET reg_resul_mod.codpos_mod    = carga_reg[319,323]

         LET reg_resul_mod.telefono      = carga_reg[324,423]
         LET reg_resul_mod.grado_escol   = carga_reg[424,424]
         LET lc_tipo_mod                 = carga_reg[425,425]    -- Nuevo
         IF  lc_tipo_mod                 =   '1'  THEN 
             LET  reg_resul_mod.tipo_mod = 'INACTIVO'
         ELSE
             LET  reg_resul_mod.tipo_mod = 'ACTIVO'
         END IF
         LET reg_resul_mod.cod_res_op    = carga_reg[426,427]    --(v10)
         LET reg_resul_mod.diag_proceso  = carga_reg[428,429]
         LET reg_resul_mod.lote_afore    = carga_reg[430,438]

         INSERT INTO pro_resul_mod
         VALUES (ultimo_folio,reg_resul_mod.*)

--(v10) inicia
         IF reg_resul_mod.diag_proceso = "2G" THEN

            INITIALIZE reg_2G.* TO NULL

            SELECT a.*
            INTO   reg_2G.*
            FROM   pro_mae_promotor a
            WHERE  a.cod_promotor =  reg_resul_mod.cod_promotor

            IF STATUS <> NOTFOUND THEN   -- si existe
               INSERT INTO pro_his_baja
               VALUES  (ultimo_folio  ,
                        reg_2G.*      )
            END IF

            UPDATE pro_mae_promotor
            SET    pro_mae_promotor.motivo_suspende = reg_resul_mod.diag_proceso  ,
                   pro_mae_promotor.diag_proceso    = reg_resul_mod.diag_proceso  ,
                   pro_mae_promotor.status          = 2                           ,
                   pro_mae_promotor.status_interno  = 5                           ,
                   pro_mae_promotor.fecha_baja      = HOY                         ,
                   pro_mae_promotor.fecha_suspende  = HOY 
            WHERE  pro_mae_promotor.cod_promotor = reg_resul_mod.cod_promotor

            INSERT INTO pro_recep_scb
            VALUES(ultimo_folio        ,
                   reg_resul_mod.cod_promotor ,
                   HOY,
                   reg_resul_mod.diag_proceso,
                   HOY,
                   reg_resul_mod.lote_afore,
                   reg_resul_mod.ind_orig_mod,
                   reg_resul_mod.cod_res_op,
                   5)
         END IF

         IF reg_resul_mod.diag_proceso = "1I"  THEN

            INITIALIZE reg_2G.* TO NULL
            SELECT a.*
            INTO   reg_2G.*
            FROM   pro_mae_promotor a  
            WHERE  a.cod_promotor =  reg_resul_mod.cod_promotor

            IF STATUS <> NOTFOUND THEN   -- si existe
               INSERT INTO pro_his_baja
               VALUES  (ultimo_folio  ,
                        reg_2G.*      )
            END IF

            UPDATE pro_mae_promotor
            SET    pro_mae_promotor.diag_proceso = reg_resul_mod.diag_proceso
            WHERE  pro_mae_promotor.cod_promotor = reg_resul_mod.cod_promotor
         END IF
 
--(v10) finaliza

         IF reg_resul_mod.diag_proceso = "7A" OR
            reg_resul_mod.diag_proceso = "7M" THEN

            LET c13_rfc = reg_resul_mod.rfc_letras  CLIPPED ,
                          reg_resul_mod.rfc_numeros CLIPPED ,
                          reg_resul_mod.rfc_homonimia

            IF reg_resul_mod.fnaci IS NULL OR
               reg_resul_mod.fnaci = "          " THEN

               UPDATE pro_mae_promotor
               SET pro_mae_promotor.diag_proceso = reg_resul_mod.diag_proceso ,
                   pro_mae_promotor.nombres      = reg_resul_mod.nombre       ,
                   pro_mae_promotor.paterno      = reg_resul_mod.paterno      ,
                   pro_mae_promotor.materno      = reg_resul_mod.materno      ,
                   pro_mae_promotor.rfc          = c13_rfc                    ,
                   pro_mae_promotor.unico        = reg_resul_mod.curp         ,
                   pro_mae_promotor.status_interno = 4 --Proceso Modificado
               WHERE  pro_mae_promotor.cod_promotor = reg_resul_mod.cod_promotor
            ELSE
               UPDATE pro_mae_promotor
               SET pro_mae_promotor.diag_proceso = reg_resul_mod.diag_proceso ,
                   pro_mae_promotor.nombres      = reg_resul_mod.nombre       ,
                   pro_mae_promotor.paterno      = reg_resul_mod.paterno      ,
                   pro_mae_promotor.materno      = reg_resul_mod.materno      ,
                   pro_mae_promotor.rfc          = c13_rfc                    ,
                   pro_mae_promotor.unico        = reg_resul_mod.curp         ,
                   pro_mae_promotor.fnaci        = reg_resul_mod.fnaci        ,
                   pro_mae_promotor.status_interno = 4 --Proceso Modificado
               WHERE  pro_mae_promotor.cod_promotor = reg_resul_mod.cod_promotor
            END IF
         END IF

         IF reg_resul_mod.diag_proceso = "7E"                     --(v1)
         OR reg_resul_mod.diag_proceso = "7X" THEN                --(v10)

            SELECT *                                   --(v1)
            FROM   pro_mae_promotor                      --(v1)
            WHERE  cod_promotor = reg_resul_mod.cod_promotor    --(v1)
            AND    motivo_suspende in ("3C", "3D", "3E", "3T")  --(v10)

              IF STATUS = NOTFOUND THEN                 --(v1)
                 PROMPT "El promotor  ",reg_resul_mod.cod_promotor, " no tiene diag. 3C, 3D, 3E, 3T" FOR CHAR enter  --(v10)
              ELSE                                           --(v1)
                 UPDATE pro_mae_promotor                           --(v1)
                 SET    pro_mae_promotor.diag_proceso = reg_resul_mod.diag_proceso          --(v1)
                 WHERE  pro_mae_promotor.cod_promotor = reg_resul_mod.cod_promotor          --(v1)
              END IF                                          --(v1)
         END IF                                               --(v1)

         IF reg_resul_mod.diag_proceso = "8E" THEN           --(v1)

            SELECT *                                            --(v1)
            FROM pro_mae_promotor                             --(v1)
            WHERE cod_promotor = reg_resul_mod.cod_promotor   --(v1)
            AND motivo_suspende in  ("8B", "8C")            --(v1)

            IF STATUS = NOTFOUND THEN                       --(v1)
               PROMPT "El promotor  ",reg_resul_mod.cod_promotor, " no tiene diag. 8B o 8C" FOR CHAR enter    --(v1)
            ELSE                                           --(v1)
               UPDATE pro_mae_promotor                      --(v1)
               SET    pro_mae_promotor.diag_proceso = reg_resul_mod.diag_proceso          --(v1)
               WHERE  pro_mae_promotor.cod_promotor = reg_resul_mod.cod_promotor          --(v1)
            END IF                                         --(v1)
         END IF                                            --(v1)

         IF reg_resul_mod.diag_proceso = "7L"               --(v1)
         OR reg_resul_mod.diag_proceso = "8A" THEN           --(v4)

            UPDATE pro_mae_promotor                         --(v1)
            SET    pro_mae_promotor.diag_proceso = reg_resul_mod.diag_proceso             --(v1)
            WHERE  pro_mae_promotor.cod_promotor = reg_resul_mod.cod_promotor             --(v1)
         END IF
      END IF

      IF carga_reg[1,3] = "306" THEN
            LET cont_aviso_examen = cont_aviso_examen + 1

            LET reg_aviso_examen.cod_promotor   = carga_reg[004,013]
            LET reg_aviso_examen.nombres        = carga_reg[014,053]
            LET reg_aviso_examen.paterno        = carga_reg[054,093]
            LET reg_aviso_examen.materno        = carga_reg[094,133]

--(v10) se recorrieron las posiciones segun MPT 19-07-2009
            LET c10_fecha                       = carga_reg[141,142],"/",
                                                  carga_reg[143,144],"/",
                                                  carga_reg[137,140]
            LET reg_aviso_examen.fecha_examen   = c10_fecha

            LET reg_aviso_examen.horario_examen = carga_reg[145,149]
            
            LET reg_aviso_examen.lugar_examen   = carga_reg[150,249]

--(v10)            LET c10_fecha                       = carga_reg[251,252],"/",
--(v10)                                                  carga_reg[253,254],"/",
--(v10)                                                  carga_reg[247,250]
--(v10)            LET reg_aviso_examen.fecha_proceso  = c10_fecha

            LET reg_aviso_examen.fecha_proceso  = HOY            --(v10)

            LET reg_aviso_examen.lote_afore     = 000000000      --(v10)

            LET reg_aviso_examen.diag_proceso   = "5B"           --(v10)



         UPDATE pro_mae_promotor                                         --(v2)
           SET    pro_mae_promotor.diag_proceso = reg_aviso_examen.diag_proceso
         WHERE  pro_mae_promotor.cod_promotor = reg_aviso_examen.cod_promotor 

         INSERT INTO pro_aviso_examen 
                     VALUES(ultimo_folio       ,
                            reg_aviso_examen.* ,
                            1                  #estado                   --(v1)
                            )

      END IF

      IF carga_reg[1,3] = "307" THEN
         LET cont_resul_examen = cont_resul_examen + 1

         LET reg_resul_examen.cod_promotor  = carga_reg[04,13]


         LET reg_resul_examen.calificacion  = carga_reg[137,139]
         LET reg_resul_examen.diag_proceso  = carga_reg[148,149]   --(v100)

--(v10)         LET c10_fecha                      = carga_reg[22,23],"/",
--(v10)                                  carga_reg[24,25],"/",
--(v10)                                  carga_reg[18,21]
--(v10)         LET reg_resul_examen.fecha_proceso = c10_fecha

         LET reg_resul_examen.fecha_proceso = c10_fecha       --(v10)

         LET reg_resul_examen.nro_lote      = 000000000       --(v10)

         INSERT INTO pro_resul_examen 
                VALUES(ultimo_folio, reg_resul_examen.*, HOY) 

         SELECT A.diag_proceso    ,
                A.status          ,
                A.status_interno  ,
                A.motivo_suspende ,
                A.fecha_baja      ,
                A.fecha_suspende
         INTO   reg_3.*
         FROM   pro_mae_promotor A
         WHERE  A.cod_promotor = reg_resul_examen.cod_promotor

         INSERT INTO pro_his_examen
               VALUES(ultimo_folio                  ,#folio
                      reg_resul_examen.cod_promotor ,
                      reg_3.diag_proceso            ,#diag_proceso
                      reg_3.status                  ,
                      reg_3.status_interno          ,
                      reg_3.motivo_suspende         ,
                      reg_3.fecha_baja              ,
                      reg_3.fecha_suspende          ,
                      HOY                            #fecha_insercion
                     )

-- (v10) inicia 
         IF reg_resul_examen.diag_proceso = "5A" OR
            reg_resul_examen.diag_proceso = "5C" OR
            reg_resul_examen.diag_proceso = "5G" OR
            reg_resul_examen.diag_proceso = "5N" OR
            reg_resul_examen.diag_proceso = "5K" THEN

            UPDATE pro_mae_promotor
            SET    pro_mae_promotor.diag_proceso    =  reg_resul_examen.diag_proceso 
            WHERE  pro_mae_promotor.cod_promotor    =  reg_resul_examen.cod_promotor

         END IF

-- (v10) finaliza     
-- (v10) Se cansela por el momento ya que en el MPT del 27-09-2009 solo
-- se recibiran los diagnostivos 5A y 5K 

{         CASE carga_reg[16,16]
         WHEN "2"
            UPDATE pro_mae_promotor
            SET    pro_mae_promotor.diag_proceso    =  reg_resul_examen.diag_proceso ,
                   pro_mae_promotor.status          = 2                     ,
                   pro_mae_promotor.status_interno  = 5                     ,
                   pro_mae_promotor.motivo_suspende =  reg_resul_examen.diag_proceso ,
                   pro_mae_promotor.fecha_suspende  =  reg_resul_examen.fecha_proceso
            WHERE  pro_mae_promotor.cod_promotor    =  reg_resul_examen.cod_promotor

         WHEN "5"
            UPDATE pro_mae_promotor
            SET    pro_mae_promotor.diag_proceso    = reg_resul_examen.diag_proceso ,
                   pro_mae_promotor.status          = 1                     ,
                   pro_mae_promotor.status_interno  = 5                     ,
                   pro_mae_promotor.motivo_suspende = ""                    ,
                   pro_mae_promotor.fecha_baja      = ""                    ,
                   pro_mae_promotor.fecha_suspende  = ""
            WHERE  pro_mae_promotor.cod_promotor = reg_resul_examen.cod_promotor
         WHEN "0"  
            UPDATE pro_mae_promotor
            SET    pro_mae_promotor.diag_proceso    =  reg_resul_examen.diag_proceso ,
                   pro_mae_promotor.status          = 1                     ,
                   pro_mae_promotor.status_interno  = 5                     ,
                   pro_mae_promotor.motivo_suspende = ""                    ,
                   pro_mae_promotor.fecha_baja      = ""                    ,
                   pro_mae_promotor.fecha_suspende  = ""
            WHERE  pro_mae_promotor.cod_promotor = reg_resul_examen.cod_promotor
         WHEN "6"
            UPDATE pro_mae_promotor
            SET    pro_mae_promotor.diag_proceso    = 
                                              reg_resul_examen.diag_proceso ,
                   pro_mae_promotor.status          = 2                     ,
                   pro_mae_promotor.status_interno  = 5                     ,
                   pro_mae_promotor.motivo_suspende = 
                                              reg_resul_examen.diag_proceso ,
                   pro_mae_promotor.fecha_suspende  = 
                                              reg_resul_examen.fecha_proceso
            WHERE  pro_mae_promotor.cod_promotor = reg_resul_examen.cod_promotor
         END CASE

         CASE carga_reg[16,17]
         WHEN "10"
            UPDATE pro_mae_promotor
            SET    pro_mae_promotor.diag_proceso    =
                                              reg_resul_examen.diag_proceso ,
                   pro_mae_promotor.status          = 1                     ,
                   pro_mae_promotor.status_interno  = 5                     ,
                   pro_mae_promotor.motivo_suspende = ""                    ,
                   pro_mae_promotor.fecha_baja      = ""                    ,
                   pro_mae_promotor.fecha_suspende  = ""
            WHERE  pro_mae_promotor.cod_promotor = reg_resul_examen.cod_promotor
         END CASE }

--(v10) se deja como comentario

         UPDATE pro_aviso_examen                 --(v3)
         SET    pro_aviso_examen.estado = 2
         WHERE  pro_aviso_examen.cod_promotor = reg_resul_examen.cod_promotor  
      END IF

      IF carga_reg[1,2] = "01" THEN

         LET c10_fecha               = carga_reg[23,24],"/",
                                       carga_reg[25,26],"/",
                                       carga_reg[19,22]
         LET reg_cza.fecha_proceso  = c10_fecha
                
         LET reg_cza.lote_de_consar = carga_reg[27,29]
         
         LET reg_cza.nro_reg_301    = carga_reg[30,34]
         
         LET reg_cza.nro_reg_302    = carga_reg[35,39]
         
         LET reg_cza.nro_reg_303    = carga_reg[40,44]
         
         LET reg_cza.nro_reg_304    = carga_reg[45,49]
         
         LET reg_cza.nro_reg_306    = carga_reg[50,54]
         
         LET reg_cza.nro_reg_307    = carga_reg[55,59]

         LET reg_cza.nro_reg_308    = carga_reg[60,64]          --(v1)
         
         LET reg_cza.ind_resp       = carga_reg[65,66]          --(v1)
         
         -- LET reg_cza.ind_rechazo    = carga_reg[62,70]       --(v1) 
         
         LET reg_cza.nro_registro   = reg_cza.nro_reg_301 +
                                      reg_cza.nro_reg_302 +
                                      reg_cza.nro_reg_303 +
                                      reg_cza.nro_reg_304 +
                                      reg_cza.nro_reg_306 +
                                      reg_cza.nro_reg_307 +   
                                      reg_cza.nro_reg_308       --(v1) 

         INSERT INTO pro_cza_agte 
                VALUES(ultimo_folio,
                       reg_cza.*   ,
                       reg_1.nom_archivo,
                       HOY)

         CONTINUE FOREACH
      END IF             
    END FOREACH

    IF cont_altas_ok > 0 THEN
       INSERT INTO pro_recepcion 
               VALUES(ultimo_folio          ,#nro_lote
                      reg_cza.fecha_proceso ,#fecha_genera
                      "301"                 ,#tipo_operacion
                      cont_altas_ok)

       INSERT INTO pro_resumen 
               VALUES("ALTAS"         ,
                      cont_altas_ok)
    END IF

    IF cont_reactiva_ok2 > 0 THEN                            --(v1)
       INSERT INTO pro_recepcion                             --(v1)
               VALUES(ultimo_folio          ,#nro_lote       --(v1)
                      reg_cza.fecha_proceso ,#fecha_genera   --(v1)
                      "308"                 ,#tipo_operacion --(v1)
                      cont_reactiva_ok2)                     --(v1)

       INSERT INTO pro_resumen                               --(v1)
               VALUES("REAC. 308" ,                          --(v1)
                      cont_reactiva_ok2)                     --(v1)
    END IF

    IF cont_reactiva_ok > 0 THEN
       INSERT INTO pro_recepcion 
               VALUES(ultimo_folio          ,#nro_lote
                      reg_cza.fecha_proceso ,#fecha_genera
                      "801"                 ,#tipo_operacion
                      cont_reactiva_ok)
        
       INSERT INTO pro_resumen 
               VALUES("REAC. 301" ,
                      cont_reactiva_ok)
    END IF
    

    IF cont_rechazos_ok > 0 THEN
       INSERT INTO pro_recepcion 
               VALUES(ultimo_folio          ,#nro_lote
                      reg_cza.fecha_proceso ,#fecha_genera
                      "401"                 ,#tipo_operacion
                      cont_rechazos_ok)

       INSERT INTO pro_resumen 
               VALUES("RECHAZOS REENVIADOS", cont_rechazos_ok)
    END IF

    IF cont_difer_ok > 0 THEN
       INSERT INTO pro_resumen 
              VALUES("CON DIFERENCIAS", cont_difer_ok)
    END IF

    IF cont_reval > 0 THEN
        INSERT INTO pro_recepcion 
               VALUES(ultimo_folio          ,#nro_lote
                      reg_cza.fecha_proceso ,#fecha_genera
                      "302"                 ,#tipo_operacion
                      cont_reval
                     )

        INSERT INTO pro_resumen 
               VALUES("REVALIDACION", cont_reval)
    END IF

    IF cont_scb > 0 THEN
        INSERT INTO pro_recepcion 
               VALUES(ultimo_folio          ,#nro_lote
                      reg_cza.fecha_proceso ,#fecha_genera
                      "303"                 ,#tipo_operacion
                      cont_scb
                     )

        INSERT INTO pro_resumen 
               VALUES("BAJAS", cont_scb)
    END IF

    IF cont_mod > 0 THEN
        INSERT INTO pro_recepcion 
               VALUES(ultimo_folio          ,#nro_lote
                      reg_cza.fecha_proceso ,#fecha_genera
                      "304"                 ,#tipo_operacion
                      cont_mod
                     )

        INSERT INTO pro_resumen 
               VALUES("MODIFICACIONES", cont_mod)
    END IF

    IF cont_aviso_examen > 0 THEN
        INSERT INTO pro_recepcion 
               VALUES(ultimo_folio          ,#nro_lote
                      reg_cza.fecha_proceso ,#fecha_genera
                      "306"                 ,#tipo_operacion
                      cont_aviso_examen
                     )

        INSERT INTO pro_resumen 
               VALUES("AVISO EXAMEN", cont_aviso_examen)
    END IF

    IF cont_resul_examen > 0 THEN
        INSERT INTO pro_recepcion 
               VALUES(ultimo_folio          ,#nro_lote
                      reg_cza.fecha_proceso ,#fecha_genera
                      "307"                 ,#tipo_operacion
                      cont_resul_examen
                     )

        INSERT INTO pro_resumen 
               VALUES("RESUL.EXAMEN", cont_resul_examen)
    END IF

    INSERT INTO pro_ctr_lote 
               VALUES (ultimo_folio      ,#nro_lote
                       ""                    ,#fecha_envio
                       reg_cza.fecha_proceso ,#fecha_recepcion
                       cont_1                 #nro_de_registros
                      )

END FUNCTION

FUNCTION init()
#-------------
    LET HOY = TODAY

    SELECT * 
    INTO   parametro.* 
    FROM   seg_modulo
    WHERE  modulo_cod = "pro"

END FUNCTION

FUNCTION tercer_paso()
#tp-------------------
    DEFINE #loc #smallint
        j                     ,
        i                     SMALLINT,
        vn                    INTEGER

    DECLARE cur_2 CURSOR FOR
        SELECT *
        FROM   pro_resumen
        ORDER BY descripcion

        LET i = 1
    FOREACH cur_2 INTO arr_1[i].*
        LET j = 12 + i
        DISPLAY arr_1[i].descripcion      AT j,12
        DISPLAY arr_1[i].nro_registros_ok AT j,50
        LET i = i + 1
    END FOREACH
END FUNCTION

FUNCTION actualiza_revalidacion(reg_det_reval)
#ar-------------------------------------------
    DEFINE #loc #reg_2
        reg_2                 RECORD LIKE pro_mae_promotor.*

    DEFINE reg_det_reval RECORD #loc #reg_det_reval
        cod_promotor          LIKE pro_det_revalida.cod_promotor  ,
        fecha_reval           LIKE pro_det_revalida.fecha_reval   ,
        diag_reval            LIKE pro_det_revalida.diag_reval    ,
        fecha_proceso         LIKE pro_det_revalida.fecha_proceso ,
        lote_afore            LIKE pro_det_revalida.lote_afore
    END RECORD

    SELECT *
    INTO   reg_2.*
    FROM   pro_mae_promotor
    WHERE  cod_promotor = reg_det_reval.cod_promotor
           
    IF STATUS <> NOTFOUND THEN
    	
        SELECT "m.X" 
        FROM   pro_his_revalida m
        WHERE  m.cod_promotor = reg_det_reval.cod_promotor
        
        IF STATUS = NOTFOUND THEN
           INSERT INTO pro_his_revalida VALUES (reg_2.*)
        ELSE
           DELETE 
           FROM  pro_his_revalida
           WHERE pro_his_revalida.cod_promotor = reg_det_reval.cod_promotor

           INSERT INTO pro_his_revalida VALUES (reg_2.*)
        END IF
        
      IF reg_det_reval.diag_reval = "4A" THEN
        UPDATE pro_mae_promotor
        SET    pro_mae_promotor.fecha_registro  = reg_det_reval.fecha_reval   ,
               pro_mae_promotor.diag_proceso    = reg_det_reval.diag_reval    ,
               pro_mae_promotor.fenvio          = reg_det_reval.fecha_proceso ,
               pro_mae_promotor.status          = 1                           ,
               pro_mae_promotor.status_interno  = 5                           ,
               pro_mae_promotor.motivo_suspende = ""                          ,
               pro_mae_promotor.fecha_suspende  = ""
        WHERE  pro_mae_promotor.cod_promotor    = reg_det_reval.cod_promotor

          --UPDATE pro_capacitacion                                    --(v5)
          --SET    pro_capacitacion.estado       = 5                   --(v5)
          --WHERE  pro_capacitacion.cod_promotor = reg_det_reval.cod_promotor 
          -- AND    pro_capacitacion.estado       = 3            --(v2)--(v5)
          --AND    pro_capacitacion.estado       = 2             --(v2)--(v5) 
          
      --ELSE                                                           --(v5)
      --    UPDATE pro_capacitacion                                    --(v5)
      --    SET    pro_capacitacion.estado       = 3                   --(v5) 
      --    WHERE  pro_capacitacion.cod_promotor = reg_det_reval.cod_promotor
      --    -- AND    pro_capacitacion.estado       = 3       --(v2)   --(v5)
      --    AND    pro_capacitacion.estado       = 2          --(v2)   --(v5)

      END IF        

      UPDATE pro_capacitacion                                      --(v5)
      SET    pro_capacitacion.estado       = 5                     --(v5)
      WHERE  pro_capacitacion.cod_promotor = reg_det_reval.cod_promotor  --(v5)
      AND    pro_capacitacion.estado       = 3                     --(v5)


    END IF
END FUNCTION
#############################################
