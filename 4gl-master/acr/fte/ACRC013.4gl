############################################################################
#Propietario       => E.F.P.                                               #
#Programa ACRC013  => RECIBE ARCHIVOS RECHAZOS DEDEVOLUCIONES A PROCESAR   #
#Fecha actualiz.   => 31 DE AGOSTO DE 2002                                 #
#Actualizacion.    => MIGUEL ANGEL HERNANDEZ MARTINEZ                      #
#Sistema           => ACR (TRA)                                            #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg_cza_cred_rdev RECORD
        tipo_registro         CHAR(02) ,
        ident_servicio        CHAR(02) ,
        ident_operacion       CHAR(02) ,
        tipo_ent_origen       CHAR(02) ,
        cve_ent_origen        CHAR(03) ,
        tipo_ent_destino      CHAR(02) ,
        cve_ent_destino       CHAR(03) ,
        ent_fed_envio_lote    CHAR(03) ,
        fecha_presentacion    DATE     ,
        consec_lote_dia       SMALLINT ,
        cve_mod_recepcion     CHAR(02) ,
        cod_result_operac     CHAR(02) ,
        rechazo               CHAR(09)
    END RECORD

    DEFINE reg_det_cred_rdev RECORD
        tipo_registro         CHAR(002)     ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(002)     ,
        cve_recep_cuenta      CHAR(003)     ,
        tipo_ced_cuenta       CHAR(002)     ,
        cve_ced_cuenta        CHAR(003)     ,
        tipo_transferencia    CHAR(002)     ,
        fecha_presentacion    DATE          ,
        n_unico_infonavit     CHAR(018)     ,
        nss_infonavit         CHAR(011)     ,
        rfc_infonavit         CHAR(013)     ,
        paterno_infonavit     CHAR(040)     ,
        materno_infonavit     CHAR(040)     ,
        nombres_infonavit     CHAR(040)     ,
        ident_lote_devol      CHAR(016)     ,
        nss_afore             CHAR(011)     ,  
        rfc_afore             CHAR(013)     ,
        paterno_afore         CHAR(040)     ,
        materno_afore         CHAR(040)     ,
        nombres_afore         CHAR(040)     ,
        ult_apor_viv_97       DECIMAL(15,02),
        cod_result_operac     CHAR(002)     ,
        diag_proceso          CHAR(015)     ,
        nombre_imss           CHAR(050)     ,
        num_cred_infonavit    DECIMAL(10,0) ,
        motivo_devolucion     CHAR(2)       ,
        periodo_pago          CHAR(6)
    END RECORD

    DEFINE reg_sum_cred_rdev RECORD
         tipo_registro        CHAR(02)      ,
         cant_reg_det         DECIMAL(9,0)  ,
         sum_ult_apor_viv97   DECIMAL(15,2)
    END RECORD

    DEFINE g_param_taa RECORD LIKE seg_modulo.*

    DEFINE
        HOY                   ,
        d_fecha_cza           ,
        d_fecha_det           DATE

    DEFINE
        enter                 CHAR(001) ,
        generar               CHAR(020) ,
        cat                   CHAR(300) ,
        archivo_traspaso      CHAR(200) ,
        lote_rechazado        CHAR(002) ,
        c_paso_cza            CHAR(8)   ,
        c_paso_det            CHAR(8)   ,
        c_fecha_cza           CHAR(10)  ,
        c_fecha_det           CHAR(10)

    DEFINE 
        cuantos               ,
        s_codigo_afore        ,
        cont                  SMALLINT

    DEFINE rzo_lote RECORD
        result_operacion      CHAR(02) ,
        motivo_rechazo1       CHAR(03) ,
        motivo_rechazo2       CHAR(03) ,
        motivo_rechazo3       CHAR(03)
    END RECORD

    DEFINE g        RECORD
        total                 INTEGER,
        diag_proceso          CHAR(3)
    END RECORD

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS 
        INPUT WRAP,
        PROMPT LINE LAST  ,
        ACCEPT KEY CONTROL-I

   CALL inicio() #i
   CALL proceso_principal() #pp

END MAIN

FUNCTION proceso_principal()
#pp-------------------------

   OPEN WINDOW ventana_1 AT 4,4 WITH FORM "ACRC0011" ATTRIBUTE(BORDER)
   DISPLAY " ACRC013  RECIBE ARCHIVO RECHAZOS DEVOLUCIONES CRED GARANTIA                   " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME generar
       AFTER FIELD generar
	   IF generar IS NULL THEN
	       ERROR "Campo NO puede ser NULO"
	       NEXT FIELD generar
	   END IF

           WHENEVER ERROR CONTINUE

               LET archivo_traspaso = g_param_taa.ruta_rescate CLIPPED,
                                      "/",generar CLIPPED

               LOAD FROM archivo_traspaso INSERT INTO safre_tmp:pla_rch_acred
                      
               SELECT count(*)
               INTO   cuantos
               FROM   safre_tmp:pla_rch_acred
                    
               IF cuantos = 0 THEN
                   DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
                   AT 19,2 ATTRIBUTE(REVERSE)
                   SLEEP 3
                   NEXT FIELD generar
               ELSE
                   EXIT INPUT
               END IF
           WHENEVER ERROR STOP

       ON KEY (INTERRUPT)
           DISPLAY "PROCESO CANCELADO" AT 19,1 ATTRIBUTE(REVERSE)
           SLEEP 2
           EXIT PROGRAM
   END INPUT

   ERROR "PROCESANDO INFORMACION "
   
   CALL crea_tablas()       #ct
   CALL validacion_previa() #vp
   CALL lee_archivo_plano() #lap
   CALL impresion_reporte()

   PROMPT "Proceso finalizado normalmente, [Enter] p/salir" FOR enter

END FUNCTION

FUNCTION inicio()
#i-------------

    LET HOY = TODAY

    SELECT codigo_afore
    INTO   s_codigo_afore
    FROM   tab_afore_local             

    SELECT * 
    INTO g_param_taa.*
    FROM seg_modulo
    WHERE modulo_cod = 'acr'

    DATABASE safre_tmp

    WHENEVER ERROR CONTINUE
        DROP TABLE pla_rch_acred
    WHENEVER ERROR STOP

    CREATE TABLE pla_rch_acred
        (
         n_registros           CHAR(730)
        )

    DATABASE safre_af

END FUNCTION

FUNCTION crea_tablas()
#ct-------------------

    DATABASE safre_tmp

    WHENEVER ERROR CONTINUE
        DROP TABLE cza_cred_rdev
        DROP TABLE det_cred_rdev
        DROP TABLE sum_cred_rdev
    WHENEVER ERROR STOP

    CREATE TABLE cza_cred_rdev
       (
        tipo_registro         CHAR(02) ,
        ident_servicio        CHAR(02) ,
        ident_operacion       CHAR(02) ,
        tipo_ent_origen       CHAR(02) ,
        cve_ent_origen        CHAR(03) ,
        tipo_ent_destino      CHAR(02) ,
        cve_ent_destino       CHAR(03) ,
        ent_fed_envio_lote    CHAR(03) ,
        fecha_presentacion    DATE     ,
        consec_lote_dia       SMALLINT ,
        cve_mod_recepcion     CHAR(02) ,
        cod_result_operac     CHAR(02) ,
        rechazo               CHAR(09)
       )

    CREATE TABLE det_cred_rdev
       (
        tipo_registro         CHAR(002)     ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(002)     ,
        cve_recep_cuenta      CHAR(003)     ,
        tipo_ced_cuenta       CHAR(002)     ,
        cve_ced_cuenta        CHAR(003)     ,
        tipo_transferencia    CHAR(002)     ,
        fecha_presentacion    DATE          ,
        n_unico_infonavit     CHAR(018)     ,
        nss_infonavit         CHAR(011)     ,
        rfc_infonavit         CHAR(013)     ,
        paterno_infonavit     CHAR(040)     ,
        materno_infonavit     CHAR(040)     ,
        nombres_infonavit     CHAR(040)     ,
        ident_lote_devol      CHAR(016)     ,
        nss_afore             CHAR(011)     ,  
        rfc_afore             CHAR(013)     ,
        paterno_afore         CHAR(040)     ,
        materno_afore         CHAR(040)     ,
        nombres_afore         CHAR(040)     ,
        ult_apor_viv_97       DECIMAL(15,02),
        cod_result_operac     CHAR(002)     ,
        diag_proceso          CHAR(015)     ,
        nombre_imss           CHAR(050)     ,
        num_cred_infonavit    DECIMAL(10,0) ,
        motivo_devolucion     CHAR(2)       ,
        periodo_pago          CHAR(6)
    )

   CREATE TABLE sum_cred_rdev
       (
         tipo_registro        CHAR(02)      ,
         cant_reg_det         DECIMAL(9,0)  ,
         sum_ult_apor_viv97   DECIMAL(15,2)
       )

    DATABASE safre_af

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
    FROM   safre_tmp:pla_rch_acred

    LET sw_1 = 0
    LET sw_2 = 0
    LET sw_9 = 0

    FOREACH cur_2 INTO c2_tipo_registro
        CASE c2_tipo_registro
            WHEN "01"
                LET sw_1 = 1
            WHEN "02"
                LET sw_2 = 1
            WHEN "09"
                LET sw_9 = 1
        END CASE
    END FOREACH

    IF sw_1 = 0 THEN
        PROMPT "SE RECHAZA EL LOTE. NO EXISTE ENCABEZADO" FOR enter
        EXIT PROGRAM
    END IF

    IF sw_2 = 0 THEN
        PROMPT "SE RECHAZA EL LOTE. NO EXISTEN REGISTROS DE DETALLE" FOR enter
        EXIT PROGRAM
    END IF

    IF sw_9 = 0 THEN
        PROMPT "SE RECHAZA EL LOTE. NO EXISTE SUMARIO" FOR enter
        EXIT PROGRAM
    END IF

END FUNCTION

FUNCTION lee_archivo_plano()
#lap------------------------

    DEFINE 
        cont                 SMALLINT
 
    DEFINE 
        carga_reg            CHAR(730) ,
        c2_ident_operacion   CHAR(002)

    DECLARE cur_1 CURSOR FOR
    SELECT  * 
    FROM    safre_tmp:pla_rch_acred
   
    LET cont = 0
    LET c2_ident_operacion = ""
    FOREACH cur_1 INTO carga_reg
        LET cont = cont + 1

               #---ENCABEZADO SOLICITUD DE TRANSFERENCIA---#
        IF carga_reg[5,6] = "06" THEN
            LET c2_ident_operacion = "01"
            LET reg_cza_cred_rdev.tipo_registro     = carga_reg[001,002] 
            LET reg_cza_cred_rdev.ident_servicio    = carga_reg[003,004]
            LET reg_cza_cred_rdev.ident_operacion   = carga_reg[005,006]
            LET reg_cza_cred_rdev.tipo_ent_origen   = carga_reg[007,008]
            LET reg_cza_cred_rdev.cve_ent_origen    = carga_reg[009,011]
            LET reg_cza_cred_rdev.tipo_ent_destino  = carga_reg[012,013]
            LET reg_cza_cred_rdev.cve_ent_destino   = carga_reg[014,016]
            LET reg_cza_cred_rdev.ent_fed_envio_lote= carga_reg[017,019]
            LET c_paso_cza                          = carga_reg[020,027]
            LET reg_cza_cred_rdev.consec_lote_dia   = carga_reg[028,030]
            LET reg_cza_cred_rdev.cve_mod_recepcion = carga_reg[031,032]
            LET reg_cza_cred_rdev.cod_result_operac = carga_reg[033,034]
            LET reg_cza_cred_rdev.rechazo           = carga_reg[035,043]

            LET c_fecha_cza = c_paso_cza[5,6],"/",
                              c_paso_cza[7,8],"/",
                              c_paso_cza[1,4]

            LET reg_cza_cred_rdev.fecha_presentacion = c_fecha_cza

            INSERT INTO safre_tmp:cza_cred_rdev VALUES(reg_cza_cred_rdev.*)
        END IF

                #---DETALLE DE SOLICITUD DE TRANSFERENCIA---#
        IF carga_reg[1,2] = "02" AND c2_ident_operacion = "01" THEN
            LET reg_det_cred_rdev.tipo_registro      = carga_reg[001,002]
            LET reg_det_cred_rdev.cont_servicio      = carga_reg[003,012]
            LET reg_det_cred_rdev.tipo_recep_cuenta  = carga_reg[013,014]
            LET reg_det_cred_rdev.cve_recep_cuenta   = carga_reg[015,017]
            LET reg_det_cred_rdev.tipo_ced_cuenta    = carga_reg[018,019]
            LET reg_det_cred_rdev.cve_ced_cuenta     = carga_reg[020,022]
            LET reg_det_cred_rdev.tipo_transferencia = carga_reg[023,024]
            LET c_paso_det                         = carga_reg[025,032]
            LET reg_det_cred_rdev.n_unico_infonavit  = carga_reg[041,058]
            LET reg_det_cred_rdev.nss_infonavit      = carga_reg[059,069]
            LET reg_det_cred_rdev.rfc_infonavit      = carga_reg[085,097]
            LET reg_det_cred_rdev.paterno_infonavit  = carga_reg[098,137]
            LET reg_det_cred_rdev.materno_infonavit  = carga_reg[138,177]
            LET reg_det_cred_rdev.nombres_infonavit  = carga_reg[178,217]
            LET reg_det_cred_rdev.ident_lote_devol   = carga_reg[240,255]
            LET reg_det_cred_rdev.nss_afore          = carga_reg[271,281]
            LET reg_det_cred_rdev.rfc_afore          = carga_reg[282,294]
            LET reg_det_cred_rdev.paterno_afore      = carga_reg[325,364]
            LET reg_det_cred_rdev.materno_afore      = carga_reg[365,404]
            LET reg_det_cred_rdev.nombres_afore      = carga_reg[405,444]
            LET reg_det_cred_rdev.ult_apor_viv_97    = carga_reg[490,504]
            LET reg_det_cred_rdev.cod_result_operac  = carga_reg[583,584]
            LET reg_det_cred_rdev.diag_proceso       = carga_reg[585,599]
            LET reg_det_cred_rdev.nombre_imss        = carga_reg[600,649]
            LET reg_det_cred_rdev.num_cred_infonavit = carga_reg[650,659]
            LET reg_det_cred_rdev.motivo_devolucion  = carga_reg[703,704]
            LET reg_det_cred_rdev.periodo_pago       = carga_reg[713,718]

            LET c_fecha_det = c_paso_det[5,6],"/",
                              c_paso_det[7,8],"/",
                              c_paso_det[1,4]
            LET reg_det_cred_rdev.fecha_presentacion  = c_fecha_det

            INSERT INTO safre_tmp:det_cred_rdev VALUES(reg_det_cred_rdev.*)

            UPDATE acr_det_tra_cred
            SET    cod_result_operac = reg_det_cred_rdev.cod_result_operac,
                   diag_proceso = reg_det_cred_rdev.diag_proceso
            WHERE  nss_afore = reg_det_cred_rdev.nss_afore
            AND    acr_det_tra_cred.fecha_presentacion =
                   reg_det_cred_rdev.fecha_presentacion
        END IF

                #---SUMARIO DE SOLICITUD DE TRANSFERENCIA---#
        IF carga_reg[1,2] = "09" AND c2_ident_operacion = "01" THEN
            LET c2_ident_operacion = ""
            LET reg_sum_cred_rdev.tipo_registro       = carga_reg[001,002]
            LET reg_sum_cred_rdev.cant_reg_det        = carga_reg[003,011]
            LET reg_sum_cred_rdev.sum_ult_apor_viv97  = carga_reg[057,071]

            INSERT INTO safre_tmp:sum_cred_rdev VALUES(reg_sum_cred_rdev.*)
        END IF

    END FOREACH

END FUNCTION

FUNCTION impresion_reporte()

  DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore

  DEFINE g_usuario      CHAR (08)
  DEFINE G_IMPRE                  CHAR(300)
  DEFINE gimpresion               CHAR(300)

  SELECT  codigo_afore,USER
  INTO    w_codigo_afore,g_usuario
  FROM    tab_afore_local

  LET G_IMPRE = g_param_taa.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
              ".RECH_DEV.",HOY USING "DD-MM-YYYY"

  START REPORT det_cred_rdev_imp TO  G_IMPRE

  OUTPUT TO REPORT det_cred_rdev_imp(g.*)

  FINISH REPORT det_cred_rdev_imp

  LET gimpresion = "lp ",G_IMPRE
  RUN gimpresion
END FUNCTION

REPORT det_cred_rdev_imp(g)
   DEFINE g     RECORD
     total         INTEGER,
     diag_proceso  CHAR(3)
   END RECORD

   DEFINE i      RECORD LIKE tab_afore_local.*

   OUTPUT
     TOP MARGIN 1
     BOTTOM MARGIN 0
     LEFT MARGIN 0
     RIGHT MARGIN 0
     PAGE LENGTH 60
   FORMAT
     PAGE HEADER
       SELECT razon_social
       INTO i.razon_social
       FROM tab_afore_local

       PRINT COLUMN 01,i.razon_social,
             COLUMN 68,TODAY USING "dd-mm-yyyy"
       SKIP 2 LINE
       PRINT COLUMN 10,"TOTAL DE DEVOLUCIONES RECHAZADAS DE PROCESAR."
       PRINT COLUMN 16,"PROCESO DE CREDITOS EN GARANTIA."
       PRINT COLUMN 05,"------------------------------------------"
       PRINT COLUMN 05,"TOTAL DE REGISTROS",
             COLUMN 30,"MOTIVO DE RECHAZO"
       PRINT COLUMN 05,"------------------------------------------"
   
     ON EVERY ROW
 
       DECLARE cursor  CURSOR FOR

       SELECT COUNT(*),
              diag_proceso[1,3]
       FROM safre_tmp:det_cred_rdev
       GROUP BY 2
       ORDER BY 2

       FOREACH cursor INTO g.*
         PRINT COLUMN 12,g.total,
               COLUMN 40,g.diag_proceso
       END FOREACH

     ON LAST ROW
       SKIP 4 LINE
       PRINT COLUMN 5,"Total de registros encontrados: ",
       COUNT (*) USING "<<<<<"

     PAGE TRAILER
       SKIP 2 LINE
       PRINT COLUMN 60,"Pagina: ",PAGENO USING "<<<<"
END REPORT
