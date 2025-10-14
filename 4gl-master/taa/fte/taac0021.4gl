#############################################################################
#Proyecto          => AFORE ( MEXICO )                                      #
#Propietario       => E.F.P.                                                #
#Programa TAAC002  => RECIBE DEVOLUCION SOLICITUDES, AFORE RECEPTORA        #
#Por               => MAURO MUNIZ CABALLERO                                 #
#Fecha creacion    => 31 DE ENERO DE 2001                                   #
#Sistema           => TAA                                                   #
#Modificado        => EDUARDO JOAQUIN RESENDIZ MEDINA (TOTAL DE REGISTROS)  #
#Fecha Modificacion=> 06 DE JULIO DE 2005                                   #
#Modificado        => MAURO MU¥IZ CABALLERO                                 #
#Fecha Modificacion=> 9 DE MAYO DE 2007                                     #
#              Verificaci¢n resultado de llamadas telefonicas               #
#Modificado        => JOSU LISANDRO HUERTA SIERRA                          #
#Fecha Modificacion=> 17 DE AGOSTO DE 2007                                  #
#              Actualizaci¢n de acuerdo a circular 69-1                     #
#############################################################################
#Modificacion         29 Mar 07 LCI, Ajustes Reporte solicitado x Cir 28-16 #
#                     03 Ago 07 BMC, Oficio CONSAR se incluye promotor      #
#                     08 Jul 08 CIL, Cambio de Tama¤o de Folio a 10 Pos     #
#                     30 Jul 08 BMC, Se incluye cambios Cir 28-18           #
#############################################################################
DATABASE safre_af

GLOBALS
    DEFINE reg_taa_cza_devol RECORD LIKE taa_cza_devol.*
    DEFINE reg_taa_det_devol RECORD LIKE taa_det_devol.*
    DEFINE reg_taa_sum_devol RECORD LIKE taa_sum_devol.*
    DEFINE g_seg_modulo      RECORD LIKE seg_modulo.* 

    DEFINE enter      CHAR(1)
    DEFINE g_usuario  CHAR(8)
    DEFINE generar    CHAR(20) 
    DEFINE archivo    CHAR(150) 
    DEFINE G_SALIDA_R CHAR(300)
    DEFINE G_SAL_D    CHAR(300)
    DEFINE G_SAL_T    CHAR(300)
    DEFINE G_PAS_D    CHAR(300)
    DEFINE G_PAS_T    CHAR(300)
    DEFINE HOY        DATE         
    DEFINE emite      DATE         
    DEFINE fecha_ver  DATE         
    DEFINE fecha_pre  DATE         
    DEFINE cont_reg   SMALLINT
    DEFINE cuantos    SMALLINT
    DEFINE caduco     SMALLINT
    DEFINE actual     SMALLINT
    DEFINE nro_reg    INTEGER
    DEFINE tot_acep  INTEGER
    DEFINE tot_rech  INTEGER
    DEFINE tot_reg   INTEGER
    DEFINE ejecuta      CHAR(300)
    DEFINE borra_lineas CHAR(300)
    DEFINE ch           CHAR(300)

    DEFINE g RECORD
        nss            CHAR(11),
        afore          CHAR(3),
        cod_operac     CHAR(2),
        motivo_rechazo CHAR(2),
        diag_proc      CHAR(3),
        descripcion    CHAR(30),
        cod_edo        SMALLINT,
        causa          CHAR(2),
        agte           CHAR(10),
        estado         CHAR(20)
    END RECORD

    DEFINE i               RECORD LIKE tab_afore_local.*
    DEFINE reg_carta       RECORD LIKE int_ctr_carta.*
    DEFINE consulta_carta  CHAR(120)
    DEFINE tipo_solic      SMALLINT
END GLOBALS

MAIN
    LET generar = ARG_VAL(1)

    CALL STARTLOG('TAAC0021.log')
    CALL inicio()                   #i
    CALL proceso_principal()        #pp
    CALL impresion_reporte()        #ir
    DISPLAY  "PROCESO FINALIZADO"

END MAIN

FUNCTION inicio()
#i---------------
    LET HOY      = TODAY
    LET cont_reg = 1
    LET caduco   = 1
    LET actual   = 1
    LET nro_reg  = 0

    SELECT *, user
    INTO   g_seg_modulo.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'taa'
 
    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp

        DROP TABLE plano_devueltas
    WHENEVER ERROR STOP

    CREATE TABLE plano_devueltas
        (n_registros          CHAR(730)) 
         
    DATABASE safre_af

    INITIALIZE reg_carta.* TO NULL

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------
    LET G_SALIDA_R = g_seg_modulo.ruta_rescate CLIPPED,"/Afi_rgr_Devo",
                     hoy USING "DDMMYYYY","_01.txt" CLIPPED

    LET G_SAL_D = g_seg_modulo.ruta_rescate CLIPPED,"/Afi_Devo_det"
    LET G_SAL_T = g_seg_modulo.ruta_rescate CLIPPED,"/Afi_Devo_tot"
    LET G_PAS_D = g_seg_modulo.ruta_rescate CLIPPED,"/Afi_det2"
    LET G_PAS_T = g_seg_modulo.ruta_rescate CLIPPED,"/Afi_tot2"

    LET archivo = g_seg_modulo.ruta_rescate CLIPPED,"/", generar CLIPPED

    LOAD FROM archivo INSERT INTO safre_tmp:plano_devueltas

    DISPLAY "PROCESANDO INFORMACION"
    CALL validacion_previa() #vp

    START REPORT arch_riesgo_tot TO G_SAL_T

    START REPORT arch_riesgo_det TO G_SAL_D
       CALL lee_archivo_plano() #lap
    FINISH REPORT arch_riesgo_det
    FINISH REPORT arch_riesgo_tot

    LET borra_lineas = "sed -e '/^$/d' ",G_SAL_D CLIPPED," > ",G_PAS_D CLIPPED
    RUN borra_lineas
    LET borra_lineas = "sed -e '/^$/d' ",G_SAL_T CLIPPED," > ",G_PAS_T CLIPPED
    RUN borra_lineas 
    LET ejecuta = "cat ", G_PAS_D CLIPPED, " ", G_PAS_T CLIPPED, " > ", 
                          G_SALIDA_R CLIPPED
    RUN ejecuta
    LET ejecuta = "rm ", G_SAL_D CLIPPED," ", G_SAL_T CLIPPED," ",
                         G_PAS_D CLIPPED," ", G_PAS_T CLIPPED
    RUN ejecuta
    LET ch = "chmod 644 ", G_SALIDA_R CLIPPED
    RUN ch

END FUNCTION

FUNCTION lee_archivo_plano()
#lap------------------------
    DEFINE 
        cont      SMALLINT,
        cont_fct  SMALLINT,
        vfolio    DECIMAL(10,0),
        srowid    INTEGER

    DEFINE
        vfentcons    DATE,
        vfecha_envio DATE
 
    DEFINE 
        ident_operacion CHAR(2),
        cfecha_8        CHAR(8),
        cfecha_10       CHAR(10),
        carga_reg       CHAR(730), 
        l_tipo_rechazo  CHAR(01)

    LET cont                          = 0
    LET ident_operacion               = ""

    LET reg_taa_det_devol.f_actualiza = HOY

    DECLARE cur_1 CURSOR FOR
    SELECT * 
      FROM safre_tmp:plano_devueltas

    LET cont = 0
    LET ident_operacion = ""

    FOREACH cur_1 INTO carga_reg
        LET cont = cont + 1

                   #---ENCABEZADO SOLICITUD TRASPASO INDIVIDUAL---#

        IF carga_reg[1,2] = "01" AND carga_reg[5,6] = "06" THEN
            LET ident_operacion = "01"
            LET reg_taa_cza_devol.tipo_registro     = carga_reg[001,002] 
            LET reg_taa_cza_devol.ident_servicio    = carga_reg[003,004]
            LET reg_taa_cza_devol.ident_operacion   = carga_reg[005,006]
            LET cfecha_8                            = carga_reg[020,027]

            LET cfecha_10 = cfecha_8[5,6],"/",cfecha_8[7,8],"/",cfecha_8[1,4]
            LET reg_taa_cza_devol.fecha_presentacion = cfecha_10

            SELECT "X"
              FROM taa_cza_devol
             WHERE fecha_presentacion = reg_taa_cza_devol.fecha_presentacion

            IF STATUS = NOTFOUND THEN
                INSERT INTO taa_cza_devol VALUES(reg_taa_cza_devol.*)
            END IF
        END IF

        IF carga_reg[1,2] = "01" AND carga_reg[5,6] <> "06" THEN
            LET ident_operacion = "02"
            DISPLAY "EL ARCHIVO NO ES DE DEVOLUCION DE SOLICITUDES"
            EXIT PROGRAM
        END IF

                   #---DETALLE SOLICITUD TRASPASO INDIVIDUAL---#

        IF carga_reg[1,2] = "02" AND ident_operacion = "01" THEN
            LET reg_taa_det_devol.tipo_registro      = carga_reg[001,002]
            LET reg_taa_det_devol.cont_servicio      = carga_reg[003,012]
            LET reg_taa_det_devol.tipo_recep_cuenta  = carga_reg[013,014]
            LET reg_taa_det_devol.tipo_ced_cuenta    = carga_reg[018,019]
            LET reg_taa_det_devol.cve_ced_cuenta     = carga_reg[020,022]
            LET reg_taa_det_devol.tipo_traspaso      = carga_reg[023,024]
            LET cfecha_8                             = carga_reg[025,032]
            LET reg_taa_det_devol.curp               = carga_reg[041,058]
            LET reg_taa_det_devol.n_seguro           = carga_reg[059,069]
            LET reg_taa_det_devol.rfc                = carga_reg[085,097]
            LET reg_taa_det_devol.paterno            = carga_reg[098,137]
            LET reg_taa_det_devol.materno            = carga_reg[138,177]
            LET reg_taa_det_devol.nombres            = carga_reg[178,217]
            LET reg_taa_det_devol.cve_sector         = carga_reg[221,221]
            LET reg_taa_det_devol.f_recep_sol        = carga_reg[232,239]
            LET reg_taa_det_devol.ident_lote_solic   = carga_reg[240,255]
            LET reg_taa_det_devol.nss_cedente        = carga_reg[271,281]
            LET reg_taa_det_devol.rfc_cedente        = carga_reg[282,282]
            LET reg_taa_det_devol.paterno_ced        = carga_reg[325,363]
            LET reg_taa_det_devol.materno_ced        = carga_reg[365,404]
            LET reg_taa_det_devol.nombres_ced        = carga_reg[405,444]
            LET reg_taa_det_devol.id_reg_reenv       = carga_reg[581,581]
            LET reg_taa_det_devol.cod_result_operac  = carga_reg[583,584]
            LET reg_taa_det_devol.diag_proceso       = carga_reg[585,587]
            LET reg_taa_det_devol.no_llamadas        = carga_reg[588,589]
            LET reg_taa_det_devol.causa_origen       = carga_reg[590,591]  #LCI
            LET reg_taa_det_devol.cod_promotor       = carga_reg[592,601]  #BMC
            LET reg_taa_det_devol.motivo_rechazo     = carga_reg[703,704]
            LET reg_taa_det_devol.diag_imagen        = carga_reg[705,706]

            LET cfecha_10 = cfecha_8[5,6],"/",cfecha_8[7,8],"/",cfecha_8[1,4]
            LET reg_taa_det_devol.fecha_presentacion = cfecha_10

            SELECT "X"
              FROM taa_det_devol
             WHERE n_seguro = reg_taa_det_devol.n_seguro
               AND fecha_presentacion = reg_taa_det_devol.fecha_presentacion

            IF STATUS = NOTFOUND  THEN
               LET reg_taa_det_devol.estado = 0

               SELECT t.tipo 
                 INTO reg_taa_det_devol.estado
                 FROM tab_dev_taa t
                WHERE t.cod_rech = reg_taa_det_devol.motivo_rechazo

                ## Verificacion de tipo de Rechazo de la Validacion Telefonica
               IF STATUS = NOTFOUND  OR
                  reg_taa_det_devol.motivo_rechazo = "  "  THEN
 
                  SELECT f.tipo_rechazo
                    INTO l_tipo_rechazo
                    FROM tab_rdeta f
                   WHERE f.rdeta_cod  = reg_taa_det_devol.diag_proceso 
                     AND f.modulo_cod = "tel"
 
                  IF STATUS = NOTFOUND  OR
                     reg_taa_det_devol.diag_proceso = "   " THEN

                     IF reg_taa_det_devol.diag_imagen IS NULL OR
                           reg_taa_det_devol.diag_imagen = '  ' THEN
                           LET reg_taa_det_devol.estado = 0
                     ELSE
                         SELECT t.tipo 
                           INTO reg_taa_det_devol.estado
                           FROM tab_dev_taa t
                          WHERE t.cod_rech = reg_taa_det_devol.diag_imagen
                     END IF
                  ELSE 
                     IF l_tipo_rechazo = 'R' THEN
                        LET reg_taa_det_devol.estado = 2
                     ELSE 
                        LET reg_taa_det_devol.estado = 0
                     END IF
                  END IF 
               END IF 

               SELECT MAX(@fentcons)
                 INTO vfentcons
                 FROM afi_solicitud
                WHERE @n_seguro       = reg_taa_det_devol.n_seguro
                  AND @status_interno IN(50,65,66,70)
                  AND @tipo_solicitud = 2

               SELECT COUNT(*)
                 INTO cont_fct  
                 FROM afi_solicitud
                WHERE @n_seguro       = reg_taa_det_devol.n_seguro
                  AND @fentcons       = vfentcons
                  AND @status_interno IN(50,65,66,70)
                  AND @tipo_solicitud = 2

               IF cont_fct > 1  THEN
                  SELECT MAX(rowid)
                    INTO srowid
                    FROM afi_solicitud
                   WHERE @n_seguro       = reg_taa_det_devol.n_seguro
                     AND @fentcons       = vfentcons
                     AND @status_interno IN(50,65,66,70)
                     AND @tipo_solicitud = 2

                  SELECT @n_folio, @fecha_envio
                    INTO vfolio, vfecha_envio
                    FROM afi_solicitud
                   WHERE @n_seguro = reg_taa_det_devol.n_seguro
                     AND @rowid    = srowid
               ELSE
                  SELECT @n_folio, @fecha_envio
                    INTO vfolio, vfecha_envio
                    FROM afi_solicitud
                   WHERE @n_seguro       = reg_taa_det_devol.n_seguro
                     AND @fentcons       = vfentcons
                     AND @status_interno IN(50,65,66,70)
                     AND @tipo_solicitud = 2
               END IF
               LET reg_taa_det_devol.n_folio = vfolio

               IF reg_taa_det_devol.cod_result_operac = '01' THEN
                  UPDATE afi_solicitud
                     SET status_interno  = 70
                   WHERE @n_seguro       = reg_taa_det_devol.n_seguro
                     AND @status_interno IN(50,65,66,70)
                     AND @fentcons       = vfentcons
                     AND @n_folio        = vfolio
                     AND @tipo_solicitud = 2

                  CALL actualiza_afi_ctr(vfolio, vfecha_envio,
                                         reg_taa_det_devol.n_seguro,70,
                                         reg_taa_det_devol.cod_result_operac,
                                         reg_taa_det_devol.fecha_presentacion)
               ELSE
                  CASE reg_taa_det_devol.estado 
                       WHEN 1    
                            LET reg_carta.docto_cod = 30227
                            LET reg_carta.opera_cod = 'D'
                            LET reg_carta.edo_genera = 10
                       WHEN 2 
                            UPDATE afi_solicitud
                               SET afi_solicitud.status_interno = 45
                             WHERE afi_solicitud.n_seguro =
                                   reg_taa_det_devol.n_seguro
                               AND afi_solicitud.status_interno in(50,65,66,70)
                               AND afi_solicitud.n_folio = vfolio

                            LET reg_carta.docto_cod  = 30219
                            LET reg_carta.opera_cod  = ' '
                            LET reg_carta.edo_genera = 10

                            CALL actualiza_afi_ctr(vfolio, vfecha_envio,
                                 reg_taa_det_devol.n_seguro,45,
                                 reg_taa_det_devol.cod_result_operac,
                                 reg_taa_det_devol.fecha_presentacion)
                       WHEN 3
                            UPDATE afi_solicitud
                               SET afi_solicitud.status_interno = 40
                             WHERE afi_solicitud.n_seguro =
                                   reg_taa_det_devol.n_seguro
                               AND afi_solicitud.status_interno in(50,65,66,70)
                               AND afi_solicitud.n_folio = vfolio

                            LET reg_carta.docto_cod  = 30219
                            LET reg_carta.opera_cod  = ' '
                            LET reg_carta.edo_genera = 10

                            CALL actualiza_afi_ctr(vfolio, vfecha_envio,
                                 reg_taa_det_devol.n_seguro,40,
                                 reg_taa_det_devol.cod_result_operac,
                                 reg_taa_det_devol.fecha_presentacion)
                   END CASE

                   OUTPUT TO REPORT arch_riesgo_det("01",
                                                    vfolio,
                                                    2,
                                                    reg_taa_det_devol.cod_result_operac)
                END IF

                INSERT INTO taa_det_devol VALUES(reg_taa_det_devol.*)

                LET reg_carta.fecha_registro = 
                    reg_taa_det_devol.fecha_presentacion 

                CASE reg_taa_det_devol.tipo_traspaso
                    WHEN '01' LET tipo_solic = 2
                              CALL det_carta(tipo_solic, vfolio) #dc
                    WHEN '02' LET tipo_solic = 2
                              CALL det_carta(tipo_solic, vfolio) #dc
                    WHEN '12' LET tipo_solic = 4
                    WHEN '51' LET tipo_solic = 1
                    WHEN '20' LET tipo_solic = 4
                END CASE
            END IF
        END IF

                   #---SUMARIO SOLICITUD TRASPASO INDIVIDUAL---#

        IF carga_reg[1,2] = "09" AND ident_operacion = "01" THEN
            LET ident_operacion = ""
            LET reg_taa_sum_devol.tipo_registro      = carga_reg[001,002]
            LET reg_taa_sum_devol.cantidad_reg_det   = carga_reg[003,011]
            LET reg_taa_sum_devol.fecha_presentacion =
                reg_taa_cza_devol.fecha_presentacion

            SELECT "X"
              FROM taa_sum_devol
             WHERE fecha_presentacion = reg_taa_sum_devol.fecha_presentacion

            IF STATUS = NOTFOUND THEN
                INSERT INTO taa_sum_devol VALUES(reg_taa_sum_devol.*)
            END IF
        END IF

        LET cfecha_8  = null
        LET cfecha_10 = null
    END FOREACH
    
    SELECT MAX(d.f_actualiza)
    INTO   fecha_ver
    FROM   taa_det_devol d

    SELECT COUNT(*)
    INTO   tot_rech
    FROM   taa_det_devol
    WHERE  f_actualiza = fecha_ver
    AND    cod_result_operac = '02'

    SELECT COUNT(*)
    INTO   tot_acep
    FROM   taa_det_devol
    WHERE  f_actualiza = fecha_ver
    AND    cod_result_operac = '01'

    LET tot_reg = tot_rech+tot_acep
    INSERT INTO taa_ctr_arh VALUES (generar,tot_reg,tot_acep,tot_rech,0,today,g_usuario)

    OUTPUT TO REPORT arch_riesgo_tot("02",
                                     reg_taa_cza_devol.fecha_presentacion,
                                     reg_taa_sum_devol.cantidad_reg_det,
                                     reg_taa_det_devol.f_recep_sol,
                                     reg_taa_sum_devol.cantidad_reg_det,
                                     tot_rech)
END FUNCTION

FUNCTION validacion_previa()
#vp------------------------
    DEFINE
        c2_tipo_registro      CHAR(2)

    DEFINE
        sw_1                  ,
        sw_2                  ,
        sw_9                  SMALLINT

    DECLARE cur_2 CURSOR FOR
    SELECT UNIQUE(n_registros[1,2])
      FROM safre_tmp:plano_devueltas

    LET sw_1 = 0
    LET sw_2 = 0
    LET sw_9 = 0

    FOREACH cur_2 INTO c2_tipo_registro
        CASE c2_tipo_registro
            WHEN "01"  LET sw_1 = 1
            WHEN "02"  LET sw_2 = 1
            WHEN "09"  LET sw_9 = 1
        END CASE
    END FOREACH

    IF sw_1 = 0 THEN
        DISPLAY "SE RECHAZA EL LOTE. NO EXISTE ENCABEZADO" 
        EXIT PROGRAM
    END IF

    IF sw_2 = 0 THEN
        DISPLAY "SE RECHAZA EL LOTE. NO EXISTEN REGISTROS DE DETALLE" 
        EXIT PROGRAM
    END IF

    IF sw_9 = 0 THEN
        DISPLAY "SE RECHAZA EL LOTE. NO EXISTE SUMARIO"
        EXIT PROGRAM
    END IF

END FUNCTION

REPORT arch_riesgo_det(tipo_reg,n_folio,tipo_solicitud,mot_rechazo)
#------------------------------------------------------------------
  DEFINE   tipo_reg              CHAR(02),
           n_folio               DECIMAL(10,0),
           tipo_solicitud        CHAR(1),
           mot_rechazo           CHAR(3),
           desc_mot_rechazo      CHAR(40)

  OUTPUT
    TOP MARGIN 1
    BOTTOM MARGIN 0
    LEFT MARGIN 0
    RIGHT MARGIN 0
    PAGE LENGTH 60

  FORMAT
    ON EVERY ROW
       SELECT @rdeta_desc_c
         INTO desc_mot_rechazo
         FROM tab_rdeta
        WHERE rdeta_cod = mot_rechazo
          AND modulo_cod = 'tel'

       PRINT COLUMN 01,tipo_reg,"|",
                       n_folio          CLIPPED  USING "&&&&&&&&&&","|",
                       tipo_solicitud   CLIPPED,"|",
                       mot_rechazo      CLIPPED,"|",
                       desc_mot_rechazo CLIPPED,"|"
END REPORT

REPORT arch_riesgo_tot(tipo_reg,fecha_env,num_reg_env,fec_resp,num_reg_res,num_rechazo)
#-------------------------------------------------------------------------------
  DEFINE   tipo_reg            CHAR(2),
           fecha_env           DATE,
           num_reg_env         SMALLINT,
           fec_resp            CHAR(8),
           num_reg_res         SMALLINT,
           num_rechazo         SMALLINT,
           vfecha              CHAR(10),
           fecha_resp          DATE,
           paso                CHAR(6),
           paso1               CHAR(6),
           paso2               CHAR(6)

  OUTPUT
    TOP MARGIN 1
    BOTTOM MARGIN 0
    LEFT MARGIN 0
    RIGHT MARGIN 0
    PAGE LENGTH 60

  FORMAT
    ON EVERY ROW
       LET vfecha = fec_resp[5,6],"/",
                    fec_resp[7,8],"/",
                    fec_resp[1,4]
       LET fecha_resp = vfecha

       LET paso  = num_reg_env
       LET paso1 = num_reg_res
       LET paso2 = num_rechazo

       PRINT COLUMN 01,tipo_reg,"|",
                       fecha_env,"|",
                       paso  CLIPPED,"|",
                       fecha_resp,"|",
                       paso1 CLIPPED,"|",
                       paso2 CLIPPED,"|"
END REPORT

FUNCTION impresion_reporte()
#ir-------------------------
    DEFINE w_codigo_afore LIKE safre_af:tab_afore_local.codigo_afore

    DEFINE hora       CHAR(8)
    DEFINE G_IMPRE    CHAR(300)
    DEFINE gimpresion CHAR(300)

    SELECT codigo_afore
      INTO w_codigo_afore
      FROM tab_afore_local

    SELECT MAX(d.f_actualiza)
      INTO fecha_ver
      FROM taa_det_devol d

    SELECT s.fecha_presentacion
      INTO fecha_pre
      FROM taa_det_devol s
     WHERE s.f_actualiza = fecha_ver
    GROUP BY 1

    LET hora = TIME
    LET hora = hora[1,2],hora[4,5],hora[7,8]

    LET G_IMPRE = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                ".DEV_SOL.",hoy USING "DDMMYY", "_",hora CLIPPED

    START REPORT det_dev_sol TO  G_IMPRE
        OUTPUT TO REPORT det_dev_sol(g.*)
    FINISH REPORT det_dev_sol

    DISPLAY 'REPORTE GENERADO:',G_IMPRE

END FUNCTION

REPORT det_dev_sol(g)
#dvsi----------------
    DEFINE g RECORD
        nss            CHAR(11),
        afore          CHAR(3),
        cod_operac     CHAR(2),
        motivo_rechazo CHAR(2),
        diag_proc      CHAR(3),
        descripcion    CHAR(100),
        cod_edo        SMALLINT,
        causa          CHAR(2),
        agte           CHAR(10),
        estado         CHAR(20)
    END RECORD
                          #0703 29-LCI, Definicion de nuevos campos del Reporte
    DEFINE lr_afi_ctr_solicitud RECORD LIKE afi_ctr_solicitud.* 
          ,lr_taa_det_devol     RECORD LIKE taa_det_devol.* 
          ,lr_pro_mae_promotor  RECORD LIKE pro_mae_promotor.* 
          ,lr_afi_solicitud     RECORD LIKE afi_solicitud.* 
          ,trab_nombre  
          ,prom_nombre    CHAR(40)
          ,l_rowid        INTEGER
          ,desc_est_prom  CHAR(10)
          ,l_cod_error    CHAR(06)
          ,l_desc_causa_llamada
          ,l_desc_val_img CHAR(60) 
          ,tl_fentcons    DATE

    #DEFINE des_causa LIKE tab_causa_llamada.descripcion
    DEFINE l_cif       RECORD
           vapuntador  SMALLINT
    END RECORD

    OUTPUT
        TOP MARGIN 0
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 0
        PAGE LENGTH 60
    FORMAT
        FIRST PAGE HEADER
        SELECT razon_social
          INTO i.razon_social
          FROM safre_af:tab_afore_local

        PRINT COLUMN 01,i.razon_social,
              COLUMN 69,TODAY USING "dd-mm-yyyy"
        PRINT COLUMN 08,"                     TOTAL DE SOLICITUDES DEVUELTAS"
        PRINT COLUMN 08,"                  TRASPASOS AFORE - AFORE  (RECEPTORA)"
        PRINT COLUMN 08,"ARCHIVO : ",generar,
              COLUMN 48,"FECHA PRESENTACION : ", fecha_pre
        PRINT COLUMN 02,
  "------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
        PRINT COLUMN 000,"FOLIO","|",
                         "AFORE","|",
                         "N.S.S","|",
                         "NOMBRE AFILIADO","|",
                         "CVE. PROM.","|",
                         "CVE EST PROM","|",
                         "DESC EST PROM","|",
                         "PROMOTOR","|",
                         "ESTADO","|",
                         "COD. VAL. TELEFONIA","|",
                         "DIAGNOSTICO VAL. TELEFONICA","|",
                         "COD. VAL. IMAGENES","|",
                         "DIAGNOSTICO VAL. IMAGENES"
                         ,"|AGTE. PROCESAR"
                         ,"|CVE. CAUSA LLAMADA"
                         ,"|DESC. CAUSA LLAMADA"

    ON EVERY ROW
               #0703 29-LCI, Recuperacion de la info completa de taa_det_devol.
    DECLARE cursor CURSOR FOR
        SELECT *
          FROM taa_det_devol dv
         WHERE dv.f_actualiza = fecha_ver
         ORDER BY 2,1

    FOREACH cursor INTO lr_taa_det_devol.*
      #0703 29-LCI, Asignacion de la Info recuparda a los campos ya utilizados.
        LET g.nss            = lr_taa_det_devol.n_seguro 
        LET g.afore          = lr_taa_det_devol.cve_ced_cuenta 
        LET g.motivo_rechazo = lr_taa_det_devol.motivo_rechazo 
        LET g.cod_edo        = lr_taa_det_devol.estado
        LET g.causa          = lr_taa_det_devol.causa_origen
        LET g.agte           = lr_taa_det_devol.cod_promotor

        LET g.descripcion = " "
        LET l_cod_error = ''
        LET l_cod_error = lr_taa_det_devol.diag_proceso

         SELECT f.rdeta_desc_l
           INTO g.descripcion
           FROM tab_rdeta f
          WHERE f.rdeta_cod  = lr_taa_det_devol.diag_proceso
            AND f.modulo_cod = "tel"

         IF STATUS = NOTFOUND OR
            lr_taa_det_devol.diag_proceso = " "  OR
            lr_taa_det_devol.diag_proceso is null THEN
            LET g.descripcion = " "
         END IF 

        LET l_desc_val_img = ''
        IF lr_taa_det_devol.cod_result_operac != '01' THEN
           SELECT tr.desc_rech
             INTO l_desc_val_img
             FROM tab_dev_taa tr
            WHERE tr.cod_rech = g.motivo_rechazo
        END IF 
 
       CASE g.cod_edo
            WHEN 0
                        ## 070502-LCI Validacion x resultado de la Operacion
                 IF lr_taa_det_devol.cod_result_operac = '01' THEN
                    LET g.estado = "PROCEDENTE"
                 ELSE
                    LET g.estado = "RECHAZADA"
                 END IF
            WHEN 1  LET g.estado = "REENVIO PROCESAR"
            WHEN 2  LET g.estado = "RECHAZO DEFINITIVO"
            WHEN 3  LET g.estado = "RECHAZADA A REENVIAR"
        END CASE

        LET nro_reg = nro_reg + 1
                        ## 070329-LCI Recupera info de la solicitud y promotor 
        LET trab_nombre = lr_taa_det_devol.nombres CLIPPED," ",
                          lr_taa_det_devol.paterno CLIPPED," ",
                          lr_taa_det_devol.materno CLIPPED
 
        INITIALIZE lr_afi_ctr_solicitud.* TO NULL
        INITIALIZE lr_afi_solicitud.* TO NULL
        LET l_rowid = NULL
        LET tl_fentcons = NULL
        SELECT MAX(fentcons)
          INTO tl_fentcons
          FROM afi_solicitud a
         WHERE a.n_seguro       = lr_taa_det_devol.n_seguro
           AND a.tipo_solicitud = 2
           AND a.status_interno > 30

        SELECT s.*
          INTO lr_afi_solicitud.*
          FROM afi_solicitud s
         WHERE s.n_seguro       = lr_taa_det_devol.n_seguro
           AND s.tipo_solicitud = 2
           AND s.status_interno > 30
           AND fentcons         = tl_fentcons

        INITIALIZE lr_pro_mae_promotor.* TO NULL
        SELECT p.*
          INTO lr_pro_mae_promotor.*
          FROM pro_mae_promotor p
         WHERE p.cod_promotor = lr_afi_solicitud.cod_promotor

        LET prom_nombre = lr_pro_mae_promotor.nombres CLIPPED," ",
                          lr_pro_mae_promotor.paterno CLIPPED," ",
                          lr_pro_mae_promotor.materno CLIPPED

        CASE lr_pro_mae_promotor.status
           WHEN 1  LET desc_est_prom = "ACTIVO"
           WHEN 2  LET desc_est_prom = "BAJA"
           WHEN 3  LET desc_est_prom = "SUSPENDIDO"
           OTHERWISE LET desc_est_prom = ""
        END CASE

         SELECT f.rdeta_desc_l
           INTO l_desc_causa_llamada 
           FROM tab_rdeta f
          WHERE f.rdeta_cod  = lr_taa_det_devol.causa_origen
            AND f.modulo_cod = "clt"

         IF STATUS = NOTFOUND OR
            lr_taa_det_devol.causa_origen = " "  OR
            lr_taa_det_devol.causa_origen is null THEN
            LET l_desc_causa_llamada = " "
         END IF 

    #0703 29-LCI, Reporte con los Datos Complementarios solicitados.
        PRINT COLUMN 000,lr_afi_solicitud.n_folio,
                         "|",g.afore,
                         "|",g.nss,
                         "|",trab_nombre,
                         "|",lr_afi_solicitud.cod_promotor,
                         "|",lr_pro_mae_promotor.status,
                         "|",desc_est_prom,
                         "|",prom_nombre,
                         "|",g.estado,
                         "|",l_cod_error,
                         "|",g.descripcion,
                         "|",g.motivo_rechazo,
                         "|",l_desc_val_img
                        ,"|",g.agte
                        ,"|",lr_taa_det_devol.causa_origen
                        ,"|",l_desc_causa_llamada
    END FOREACH 

    ON LAST ROW
       PRINT COLUMN 2, "Total de registros encontrados: ",nro_reg USING "<<<<"

END REPORT                      

FUNCTION det_carta(tipo_sol, cfolio)
#dc---------------------------------
   DEFINE vfentc    DATE
   DEFINE vfrec     DATE
   DEFINE tipo_sol  SMALLINT
   DEFINE vtot_fent SMALLINT
   DEFINE vrowid    INTEGER
   DEFINE cfolio    DECIMAL(10,0)       #0807 CIL Fol-10

   LET reg_carta.tipo_solicitud = tipo_sol
   LET reg_carta.n_folio        = cfolio
   LET reg_carta.nss            = reg_taa_det_devol.n_seguro
   LET reg_carta.fecha_genera   = TODAY 
   LET reg_carta.hora_genera    = TIME
   LET reg_carta.lote_genera    = 0
   LET reg_carta.consecutivo    = 0
   LET reg_carta.id_sepomex     = 0

   SELECT 'X'
     FROM int_ctr_carta
    WHERE @nss            = reg_taa_det_devol.n_seguro
      AND @n_folio        = reg_carta.n_folio
      AND @tipo_solicitud = tipo_sol
      AND @fecha_genera   = today
    GROUP BY 1
 
   IF STATUS = NOTFOUND THEN
     LET consulta_carta = "EXECUTE PROCEDURE inserta_carta (?,?,?,?,?,?,?,?,?,",
                          "?,?,?)"
     PREPARE sql_exe FROM consulta_carta
     EXECUTE sql_exe USING reg_carta.*

     INITIALIZE reg_carta.* TO NULL
   END IF

END FUNCTION

FUNCTION actualiza_afi_ctr(vfolio, vfecha_envio, vnss, st_int, cod_op, vfact)
#aac------------------------------------------------------------------
    DEFINE
        vfolio       DECIMAL(10,0),     #0807 CIL Fol-10
        vfecha_envio DATE,
        vnss         CHAR(11),
        st_int       SMALLINT,
        cod_op       SMALLINT,
        maxregid     INTEGER,
        vfact        DATE

    DEFINE reg_ctr RECORD LIKE afi_ctr_solicitud.*

    SELECT MAX(@rowid)
      INTO maxregid
      FROM afi_ctr_solicitud
     WHERE @n_seguro       = vnss
       AND @n_folio        = vfolio
       AND @tipo_solicitud = 2
       AND @fecha_envio    = vfecha_envio

    SELECT *
      INTO reg_ctr.*
      FROM afi_ctr_solicitud
     WHERE @n_seguro       = vnss
       AND @n_folio        = vfolio
       AND @tipo_solicitud = 2
       AND @fecha_envio    = vfecha_envio
       AND @rowid          = maxregid

    LET reg_ctr.fecha_recepcion = TODAY
    LET reg_ctr.cod_operacion   = cod_op
    LET reg_ctr.status_interno  = st_int
    LET reg_ctr.n_folio         = vfolio
    LET reg_ctr.fecha_envio     = vfecha_envio
    LET reg_ctr.tipo_solicitud  = 2
    LET reg_ctr.factualiza      = vfact

    INSERT INTO afi_ctr_solicitud VALUES(reg_ctr.*)

END FUNCTION
