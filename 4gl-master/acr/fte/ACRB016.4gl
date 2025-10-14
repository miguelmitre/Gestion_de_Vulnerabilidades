############################################################################
#Propietario       => E.F.P.                                               #
#Programa ACRB016  => RECIBE ARCHIVO CREDITO EN GARANTIA, SOLICITUD MARCA  #
#Fecha creacion    => 1 DE JULIO DE 2002                                   #
#Por               => MAURO MUNIZ CABALLERO                                #
#Sistema           => ACR (TRA)                                            #
#Modificado        => EDUARDO JOAQUIN RESENDIZ MEDINA                      #
#Fecha             => 22 FEBRERO DE 2005                                   #
#Modificado        => DMR 18 junio 2013  MLM-1981                          #
#Fecha             => Se agrego la rutina limpieza para cambiar la \ por   #
#                  => otro caracter Ñ para que no truene el programa       #
#                  => en el marcaje y desmarcaje de las cuentas            #
#Modificado        => DMR 08 julio 2013  MLM-1977                          #
#Modificacion      => Se cambio el llamado de la rutina limpieza la cual se#
#                  => adiciono en el ACR_GLOB.4gl para los demas programas.#
#Modificado        => DMR 21 Agosto 2013 MLM-2087 NO archivo RESPALDO y no #
#                  => se pide confirmacion al usuario para sobreescribir   #
############################################################################
DATABASE safre_af

GLOBALS
    DEFINE reg_cza_tra_cred RECORD
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
        cod_result_operac     CHAR(02) ,
        rechazo               CHAR(09)
    END RECORD

    DEFINE reg_det_tra_cred RECORD
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
        partic_v97            DECIMAL(22,06),
        sdo_viv_97            DECIMAL(15,02),
        sdo_viv_92            DECIMAL(15,02),
        cod_result_operac     CHAR(002)     ,
        diag_proceso          CHAR(015)     ,
        nombre_imss           CHAR(050)     ,
        num_cred_infonavit    DECIMAL(10,0) ,
        int_viv_97            DECIMAL(15,02),
        int_viv_92            DECIMAL(15,02),
        periodo_pago          CHAR(6)
    END RECORD

    DEFINE reg_sum_tra_cred RECORD
        tipo_registro        CHAR(02)      ,
        cant_reg_det         DECIMAL(9,0)  ,
        sum_partic_v97       DECIMAL(22,6) ,
        sum_sdo_viv97        DECIMAL(15,2) ,
        sum_sdo_viv92        DECIMAL(15,2) ,
        sum_int_viv97        DECIMAL(15,2) ,
        sum_int_viv92        DECIMAL(15,2)
    END RECORD

    DEFINE g_param_taa RECORD LIKE seg_modulo.*

    DEFINE
        HOY                   ,
        d_fecha_cza           ,
        d_fecha_det           DATE

    DEFINE
        enter                 CHAR(1),
        lote_rechazado        CHAR(2),
        c_paso_cza            CHAR(8),
        c_paso_det            CHAR(8),
        c_fecha_cza           CHAR(10),
        c_fecha_det           CHAR(10),
        generar               CHAR(20),
        archivo_traspaso      CHAR(200),
        cat                   CHAR(300),
        ejecuta               CHAR(300)

    DEFINE
        bnd_proc              ,
        edo_proc              SMALLINT,
        cuantos               ,
        s_codigo_afore        ,
        cont                  INTEGER

    DEFINE
        opc             CHAR(1)  ,
        vnss            CHAR(11) ,
        vmarca_entra    SMALLINT ,
        vmarca_estado   SMALLINT ,
        vcodigo_rechazo SMALLINT ,
        g_usuario       CHAR(8)  ,
        xcodigo_marca   SMALLINT ,
        xcodigo_rechazo SMALLINT  

    DEFINE g RECORD
        total                 INTEGER,
        tipo_transferencia    CHAR(2),
        descripcion           CHAR(20),
        marca                 SMALLINT,
        marca_desc            CHAR(20)
    END RECORD

    DEFINE 
      v_marca                 CHAR(300)
END GLOBALS


MAIN
   DEFER INTERRUPT
   OPTIONS 
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I

   CALL STARTLOG("ACRB016.log")
   CALL inicio() #i
   CALL proceso_principal() #pp
   CALL impresion_reporte()
   CALL impresion_reporte_fechpres()
   DISPLAY "PROCESO TERMINADO" AT 19,1 ATTRIBUTE(REVERSE)
   SLEEP 2
END MAIN


FUNCTION proceso_principal()
#pp-------------------------

   OPEN WINDOW ventana_1 AT 4,4 WITH FORM "ACRC0011" ATTRIBUTE(BORDER)

   DISPLAY " ACRB016   RECIBE ARCHIVO CREDITO EN GARANTIA, SOLICITUD MARCA             " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME generar
      AFTER FIELD generar
         IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
         END IF

         SELECT "X"
         FROM   acr_ctr_arh a
         WHERE  a.nombre_archivo = generar

         IF SQLCA.SQLCODE = 0 THEN
            PROMPT "ARCHIVO YA PROCESADO,[Enter] p/salir" FOR enter
            EXIT PROGRAM
         END IF

         CALL limpieza(g_param_taa.ruta_rescate,generar)
      
         WHENEVER ERROR CONTINUE

         LET archivo_traspaso = g_param_taa.ruta_rescate CLIPPED,
                                "/",generar CLIPPED

         LOAD FROM archivo_traspaso INSERT INTO safre_tmp:tmp_pla_acr

         SELECT count(*)
         INTO   cuantos
         FROM   safre_tmp:tmp_pla_acr

         IF cuantos = 0 THEN
            DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO " AT 19,2 ATTRIBUTE(REVERSE)
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

   ERROR "PROCESANDO INFORMACION"

   CALL validacion_previa() #vp
   CALL lee_archivo_plano() #lap
END FUNCTION


FUNCTION inicio()
#i---------------
   LET HOY = TODAY

   LET bnd_proc = 0
   LET edo_proc = 237

   SELECT codigo_afore, USER
   INTO   s_codigo_afore, g_usuario
   FROM   tab_afore_local

   SELECT *
   INTO   g_param_taa.*
   FROM   seg_modulo
   WHERE  modulo_cod = 'acr'

   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp

      DROP TABLE tmp_pla_acr

      CREATE TABLE tmp_pla_acr
      (n_registros  CHAR(730))

      DATABASE safre_af
   WHENEVER ERROR STOP

   LET v_marca    = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? )"
END FUNCTION


FUNCTION validacion_previa()
#vp------------------------
    DEFINE
        c2_tipo_registro  CHAR(2)

    DEFINE
        sw_1  ,
        sw_2  ,
        sw_9  SMALLINT

    DECLARE cur_2 CURSOR FOR
    SELECT UNIQUE(n_registros[1,2])
    FROM   safre_tmp:tmp_pla_acr

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
        cont    INTEGER
 
    DEFINE
        mot_dev SMALLINT
 
    DEFINE
        sdo_v97 DECIMAL(18,6),
        sdo_v92 DECIMAL(18,6)
 
    DEFINE 
        carga_reg            CHAR(730),
        c2_ident_operacion   CHAR(2)

    DEFINE 
        fecha_conver DATE

    LET fecha_conver = MDY(MONTH(HOY),1,YEAR(HOY))

    LET bnd_proc = 0

    DECLARE cur_1 CURSOR FOR
    SELECT  * 
    FROM    safre_tmp:tmp_pla_acr

    LET cont = 0
    LET c2_ident_operacion = ""

    FOREACH cur_1 INTO carga_reg

               #---ENCABEZADO SOLICITUD DE TRANSFERENCIA---#
        IF carga_reg[5,6] = "01" THEN
            LET c2_ident_operacion = "01"
            LET reg_cza_tra_cred.tipo_registro     = carga_reg[001,002]
            LET reg_cza_tra_cred.ident_servicio    = carga_reg[003,004]
            LET reg_cza_tra_cred.ident_operacion   = carga_reg[005,006]
            LET reg_cza_tra_cred.tipo_ent_origen   = carga_reg[007,008]
            LET reg_cza_tra_cred.cve_ent_origen    = carga_reg[009,011]
            LET reg_cza_tra_cred.tipo_ent_destino  = carga_reg[012,013]
            LET reg_cza_tra_cred.cve_ent_destino   = carga_reg[014,016]
            LET reg_cza_tra_cred.ent_fed_envio_lote= carga_reg[017,019]
            LET c_paso_cza                         = carga_reg[020,027]
            LET reg_cza_tra_cred.consec_lote_dia   = carga_reg[028,030]
            LET reg_cza_tra_cred.cod_result_operac = carga_reg[033,034]
            LET reg_cza_tra_cred.rechazo           = carga_reg[035,043]

            LET c_fecha_cza = c_paso_cza[5,6],"/",
                              c_paso_cza[7,8],"/",
                              c_paso_cza[1,4]

            LET reg_cza_tra_cred.fecha_presentacion = c_fecha_cza

            SELECT 'X'
            FROM   acr_cza_tra_cred a
            WHERE  a.fecha_presentacion = reg_cza_tra_cred.fecha_presentacion

            IF SQLCA.SQLCODE THEN
                INSERT INTO acr_cza_tra_cred VALUES(reg_cza_tra_cred.*)
            END IF
        END IF

                #---DETALLE DE SOLICITUD DE TRANSFERENCIA---#
        IF carga_reg[1,2] = "02" AND c2_ident_operacion = "01" THEN
            LET cont = cont + 1
            LET reg_det_tra_cred.tipo_registro      = carga_reg[001,002]
            LET reg_det_tra_cred.cont_servicio      = carga_reg[003,012]
            LET reg_det_tra_cred.tipo_recep_cuenta  = carga_reg[013,014]
            LET reg_det_tra_cred.cve_recep_cuenta   = carga_reg[015,017]
            LET reg_det_tra_cred.tipo_ced_cuenta    = carga_reg[018,019]
            LET reg_det_tra_cred.cve_ced_cuenta     = carga_reg[020,022]
            LET reg_det_tra_cred.tipo_transferencia = carga_reg[023,024]
            LET c_paso_det                          = carga_reg[025,032]
            LET reg_det_tra_cred.n_unico_infonavit  = carga_reg[041,058]
            LET reg_det_tra_cred.nss_infonavit      = carga_reg[059,069]
            LET reg_det_tra_cred.rfc_infonavit      = carga_reg[085,097]
            LET reg_det_tra_cred.paterno_infonavit  = carga_reg[098,137]
            LET reg_det_tra_cred.materno_infonavit  = carga_reg[138,177]
            LET reg_det_tra_cred.nombres_infonavit  = carga_reg[178,217]
            LET reg_det_tra_cred.ident_lote_devol   = carga_reg[240,255]
            LET reg_det_tra_cred.nss_afore          = carga_reg[271,281]
            LET reg_det_tra_cred.rfc_afore          = carga_reg[282,294]
            LET reg_det_tra_cred.paterno_afore      = carga_reg[325,364]
            LET reg_det_tra_cred.materno_afore      = carga_reg[365,404]
            LET reg_det_tra_cred.nombres_afore      = carga_reg[405,444]
            LET reg_det_tra_cred.sdo_viv_97         = carga_reg[490,504]
            LET reg_det_tra_cred.sdo_viv_92         = carga_reg[565,579]
            LET reg_det_tra_cred.cod_result_operac  = carga_reg[583,584]
            LET reg_det_tra_cred.diag_proceso       = carga_reg[585,599]
            LET reg_det_tra_cred.nombre_imss        = carga_reg[600,649]
            LET reg_det_tra_cred.num_cred_infonavit = carga_reg[650,659]
            LET reg_det_tra_cred.int_viv_97         = carga_reg[660,674]
            LET reg_det_tra_cred.int_viv_92         = carga_reg[675,689]
            LET reg_det_tra_cred.periodo_pago       = carga_reg[713,718]

            LET c_fecha_det = c_paso_det[5,6],"/",
                              c_paso_det[7,8],"/",
                              c_paso_det[1,4]
            LET reg_det_tra_cred.fecha_presentacion  = c_fecha_det

            LET vmarca_estado   = 0
            LET vcodigo_rechazo = 0

            INSERT INTO acr_det_tra_cred 
            VALUES(reg_det_tra_cred.*,xcodigo_rechazo)

            SELECT SUM(monto_en_pesos)
            INTO   sdo_v97
            FROM   dis_cuenta
            WHERE  nss = reg_det_tra_cred.nss_afore
            AND    subcuenta = 4
            AND    tipo_movimiento  <> 888
            AND    fecha_valor <= fecha_conver

            IF sdo_v97 < 0 OR sdo_v97 IS NULL THEN
                --sveraLET bnd_proc = 1
                --sveraLET mot_dev  = 8

                --sveraLET vmarca_estado   = 30
                --sveraLET vcodigo_rechazo = 900

            END IF

            SELECT "X"
            FROM   cta_act_marca
            WHERE  marca_cod = edo_proc
            AND    nss       = reg_det_tra_cred.nss_afore

            IF STATUS = NOTFOUND THEN
                CALL marca_cuenta (reg_det_tra_cred.nss_afore,
                                   edo_proc, vmarca_estado,
                                   vcodigo_rechazo, g_usuario,
                                   reg_det_tra_cred.cont_servicio) 
            END IF

            LET bnd_proc = 0
            LET mot_dev  = 0
        END IF

                #---SUMARIO DE SOLICITUD DE TRANSFERENCIA---#
        IF carga_reg[1,2] = "09" AND c2_ident_operacion = "01" THEN
            LET c2_ident_operacion = ""
            LET reg_sum_tra_cred.tipo_registro  = carga_reg[001,002]
            LET reg_sum_tra_cred.cant_reg_det   = carga_reg[003,011]
            LET reg_sum_tra_cred.sum_sdo_viv97  = carga_reg[057,071]
            LET reg_sum_tra_cred.sum_sdo_viv92  = carga_reg[132,146]
            LET reg_sum_tra_cred.sum_int_viv97  = carga_reg[147,161]
            LET reg_sum_tra_cred.sum_int_viv92  = carga_reg[162,176]

            INSERT INTO acr_sum_tra_cred 
            VALUES(reg_sum_tra_cred.*,reg_cza_tra_cred.fecha_presentacion)
        END IF

    END FOREACH

    INSERT INTO acr_ctr_arh VALUES(generar,cont,0,0,0,hoy,g_usuario)
END FUNCTION


FUNCTION impresion_reporte()
#ir-------------------------
   DEFINE G_IMPRE        CHAR(300)
   DEFINE c_impre        CHAR(300)
   DEFINE gimpresion     CHAR(300)
   DEFINE hora           CHAR (08)

    LET G_IMPRE = g_param_taa.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                  ".CRED_GARANTIA.",hoy USING "DD-MM-YYYY"

    START REPORT det_tra_cred_imp TO  G_IMPRE

       OUTPUT TO REPORT det_tra_cred_imp(g.*)

    FINISH REPORT det_tra_cred_imp

    LET gimpresion = "lp ",G_IMPRE
    RUN gimpresion

    LET c_impre = ' chmod 777 ', G_IMPRE CLIPPED
    RUN c_impre
END FUNCTION


REPORT det_tra_cred_imp(g)
#dtai---------------------
    DEFINE g RECORD
        nss                 CHAR(11),
        tipo_transferencia  CHAR(2),
        descripcion         CHAR(20),
        marca               SMALLINT,
        marca_desc          CHAR(20)
    END RECORD

    DEFINE fecha_max DATE

    OUTPUT
        TOP MARGIN 1
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 0
        PAGE LENGTH 70

    FORMAT
        PAGE HEADER

        PRINT COLUMN 01,s_codigo_afore,
            COLUMN 68,TODAY USING "dd-mm-yyyy"
        SKIP 2 LINE
        PRINT COLUMN 08,"   REGISTROS SOLICITADOS PARA MARCA DE CREDITO EN GARANTIA   "
        SKIP 1 LINE 
        PRINT COLUMN 01,"---------------------------------------------------------------------"
        PRINT COLUMN 02,"NSS",
              COLUMN 15,"TIPO DE TRANSFERENCIA",
              COLUMN 43,"MARCA"
        PRINT COLUMN 01,"---------------------------------------------------------------------"
        SKIP 1 LINE 
    ON EVERY ROW

      SELECT MAX(fecha_presentacion)
      INTO   fecha_max
      FROM   acr_cza_tra_cred

      DECLARE cursor CURSOR FOR
      SELECT a.nss_afore,
             a.tipo_transferencia
      FROM   acr_det_tra_cred a
      WHERE  a.fecha_presentacion = fecha_max
      ORDER BY 2

      FOREACH cursor INTO g.nss,
                          g.tipo_transferencia

          SELECT NVL(c.marca_cod,0)
          INTO   g.marca
          FROM   cta_act_marca c
          WHERE  c.nss       = g.nss
          AND    c.marca_cod = 237
          GROUP BY 1

          IF g.marca = 237 THEN
              SELECT c.marca_desc
              INTO   g.marca_desc
              FROM   tab_marca c
              WHERE  c.marca_cod = g.marca
          ELSE
              LET g.marca = ' '
              LET g.marca_desc = 'SIN MARCA 43-BIS'
          END IF

          CASE g.tipo_transferencia
              WHEN "03" LET g.descripcion = "TRANSFERENCIA TOTAL"
              WHEN "04" LET g.descripcion = "ULTIMA APORTACION"
              WHEN "16" LET g.descripcion = "CREDITO EN GARANTIA"
              WHEN "18" LET g.descripcion = "USO DE CREDITO"
              WHEN "70" LET g.descripcion = "TRANSFERENCIA TOTAL"
              WHEN "71" LET g.descripcion = "ULTIMA APORTACION"
          END CASE
       
        PRINT COLUMN 02,g.nss,
              COLUMN 15,g.tipo_transferencia,
              COLUMN 18,g.descripcion,
              COLUMN 45,g.marca,
              COLUMN 50,g.marca_desc
      END FOREACH

     --ON LAST ROW
        --SKIP 4 LINES

     PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
      PAUSE "Presione enter para continuar."
END REPORT


#######################################################################
FUNCTION marca_cuenta(vnss, vmarca_entra, vmarca_edo, vcodigo_rech,
                      vusuario, vcorrelativo)
#mc------------------

   DEFINE
      vnss               CHAR(11),
      vmarca_entra       SMALLINT,
      vmarca_edo         SMALLINT,
      vcodigo_rech       SMALLINT,
      vusuario           CHAR(08),
      vcorrelativo       INTEGER,
      vmarca_causa       SMALLINT,
      vfecha_causa       DATE
 
   LET vmarca_causa = 0
   LET vfecha_causa = ""

   PREPARE eje_marca FROM v_marca
  
   DECLARE cur_marca CURSOR FOR eje_marca

   OPEN cur_marca

   USING vnss        ,
         vmarca_entra,
         vcorrelativo,
         vmarca_edo  ,
         vcodigo_rech,
         vmarca_causa,
         vfecha_causa,
         vusuario

   FETCH cur_marca
   INTO  xcodigo_marca, xcodigo_rechazo
   
   IF vcodigo_rech = 900 THEN
       LET xcodigo_rechazo = 8
   END IF

   UPDATE acr_det_tra_cred
      SET estado    = xcodigo_rechazo
    WHERE nss_afore = vnss
      AND fecha_presentacion = reg_det_tra_cred.fecha_presentacion 

   CLOSE cur_marca
   FREE  cur_marca
END FUNCTION


FUNCTION impresion_reporte_fechpres()
#irf-------------------------
  DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore

  --DEFINE g_usuario      CHAR (08)
  DEFINE G_IMPRE        CHAR(300)
  DEFINE c_impre        CHAR(300)
  DEFINE gimpresion     CHAR(300)
  DEFINE hora           CHAR (08)

  DEFINE h RECORD 
      nss       CHAR(11),
      fech_pres DATE,
      cod_edo   SMALLINT,
      desc_edo  CHAR(30)      
  END RECORD

  DEFINE g_seg_modulo RECORD LIKE seg_modulo.*

  SELECT  codigo_afore
  INTO    w_codigo_afore
  FROM    tab_afore_local

  LET hora = TIME

  LET G_IMPRE = g_param_taa.ruta_listados CLIPPED,"/",
                g_usuario CLIPPED,
                ".CRED_GTIA_FECHPRES.",hoy USING "DD-MM-YYYY",
                "_",hora CLIPPED

  START REPORT det_garantia_imp_fechpres TO  G_IMPRE
  
  DECLARE cur_report2 CURSOR FOR
     SELECT nss_afore,fecha_presentacion,estado
     FROM   acr_det_tra_cred
     WHERE  estado <> 0
     ORDER BY 1
     
  FOREACH cur_report2 INTO h.*     

     SELECT rechazo_desc INTO h.desc_edo
     FROM tab_rch_marca
     WHERE rechazo_cod = h.cod_edo
     
     OUTPUT TO REPORT det_garantia_imp_fechpres(h.*)
  
  END FOREACH

  FINISH REPORT det_garantia_imp_fechpres

  LET gimpresion = "lp ",G_IMPRE
  RUN gimpresion

  LET c_impre = ' chmod 777 ', G_IMPRE CLIPPED
  RUN c_impre
END FUNCTION


REPORT det_garantia_imp_fechpres(h)
#dtai--------------------
  DEFINE h     RECORD
    nss        CHAR(11),
    fecha_pres DATE,
    cod_edo    SMALLINT,
    desc_edo   CHAR(30)
  END RECORD

  DEFINE tot_reg INTEGER

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

      SELECT COUNT(*)
      INTO   tot_reg
      FROM   acr_det_tra_cred
      WHERE estado <> 0

      PRINT COLUMN 01,i.razon_social,
            COLUMN 68,TODAY USING "dd-mm-yyyy"
      SKIP 2 LINE
      PRINT COLUMN 08,"DEVOLUCION DE REGISTROS DE MARCA DE GARANTIA"
      SKIP 2 LINE
      PRINT COLUMN 08,"NOMBRE ARCHIVO RECIBIDO : ", generar
      SKIP 1 LINE
      PRINT COLUMN 01,"--------------------------------------------------------------------------------------"
      PRINT COLUMN 05,"N. SEGURO",
            COLUMN 20,"FECHA PRESENTACION",
            COLUMN 40,"ESTADO",
            COLUMN 50,"DESC. ESTADO"
      PRINT COLUMN 01,"--------------------------------------------------------------------------------------"
      SKIP 1 LINE
 ON EVERY ROW
     
        PRINT --COLUMN 10,g.total,
              COLUMN 05,h.nss,
              COLUMN 25,h.fecha_pres USING "dd-mm-yyyy",
              COLUMN 37,h.cod_edo,
              COLUMN 50,h.desc_edo

     ON LAST ROW
        SKIP 4 LINES
        PRINT COLUMN 2, "Total registros solicitados : ",
        tot_reg USING "<<<<"

     PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
      PAUSE "Presione enter para continuar...."
END REPORT

