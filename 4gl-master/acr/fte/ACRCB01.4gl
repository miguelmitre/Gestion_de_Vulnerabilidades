############################################################################
#Propietario       => E.F.P.                                               #
#Programa ACRCB01  => RECIBE ARCHIVOS DE TRANSFERENCIA DE ACREDITADOS      #
#Fecha creacion    => 30 DE ENERO DE 1998                                  #
#Por               => MAURO MUNIZ CABALLERO                                #
#Fecha actualiz.   => 24 DE MARZO DE 1999                                  #
#Actualizacion .   => MAURO MUNIZ CABALLERO                                #
#Fecha actualiz.   => 17 DE JUNIO 1999.                                    #
#Actualizacion     => MIGUEL ANGEL HERNANDEZ MARTINEZ                      #
#Solucion          => INTEGRACION DE LA FUNCION DEL REPORTE                #
#Sistema           => ACR (TAA) / BATCH                                    #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg_cza_tra_acr RECORD
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

    DEFINE reg_det_tra_acr RECORD
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
        periodo_pago           CHAR(6)
    END RECORD

    DEFINE reg_sum_tra_acr RECORD
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
        generar               CHAR(012) ,
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

    DEFINE g        RECORD
        total                 INTEGER,
        tipo_transferencia    CHAR(2),
        descripcion           CHAR(20)
    END RECORD

    DEFINE reg_bat RECORD
		  pid            INTEGER  ,
		  proceso_cod    INTEGER  ,
		  opera_cod      INTEGER  ,
		  nombre_archivo CHAR(025) 
    END RECORD

END GLOBALS

MAIN
   
DISPLAY " "
DISPLAY ".1"

   CALL inicio() #i
   CALL proceso_principal() #pp
   CALL actualiza_operacion() #pp

END MAIN

FUNCTION proceso_principal()
#pp-------------------------

    LET archivo_traspaso = g_param_taa.ruta_rescate CLIPPED,"/",
									reg_bat.nombre_archivo CLIPPED

    LOAD FROM archivo_traspaso INSERT INTO safre_tmp:tmp_pla_acr

    SELECT count(*)
    INTO   cuantos
    FROM   safre_tmp:tmp_pla_acr
                    
    IF cuantos = 0 THEN
        DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
    END IF

    CALL crea_tablas() #ct
    CALL validacion_previa() #vp
    CALL lee_archivo_plano() #lap
    CALL impresion_reporte() 

END FUNCTION

FUNCTION inicio()
#i-------------



    LET reg_bat.pid            = ARG_VAL(1)
    LET reg_bat.proceso_cod    = ARG_VAL(2)
    LET reg_bat.opera_cod      = ARG_VAL(3)
    LET reg_bat.nombre_archivo = ARG_VAL(4) 

    DISPLAY "INICIANDO PROCESO ...."

    SELECT codigo_afore
    INTO   s_codigo_afore
    FROM   tab_afore_local             

    SELECT * 
    INTO   g_param_taa.*
    FROM   seg_modulo
	 WHERE  modulo_cod = 'acr'

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp

        DROP TABLE tmp_pla_acr

        CREATE TABLE tmp_pla_acr
          (
           n_registros           CHAR(730)
          )

        DATABASE safre_af

    WHENEVER ERROR STOP

END FUNCTION

FUNCTION crea_tablas()
#ct-------------------

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp

        DROP TABLE safre_tmp:cza_tra_acr
        DROP TABLE safre_tmp:det_tra_acr
        DROP TABLE safre_tmp:sum_tra_acr
    WHENEVER ERROR STOP

    CREATE TABLE "safre".cza_tra_acr
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
       );

    CREATE TABLE "safre".det_tra_acr
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
        periodo_pago          CHAR(6)       ,
        estado                SMALLINT
       )

   CREATE TABLE "safre".sum_tra_acr
       (
         tipo_registro        CHAR(02)      ,
         cant_reg_det         DECIMAL(9,0)  ,
         sum_ult_apor_viv97   DECIMAL(15,2)
       );

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
        DISPLAY "Program stopped SE RECHAZA EL LOTE. NO EXISTE ENCABEZADO" 
        EXIT PROGRAM
    END IF

    IF sw_2 = 0 THEN
        DISPLAY "Program stopped SE RECHAZA EL LOTE. NO EXISTEN REGISTROS DE DETALLE" 
        EXIT PROGRAM
    END IF

    IF sw_9 = 0 THEN
        DISPLAY "Program stopped SE RECHAZA EL LOTE. NO EXISTE SUMARIO" 
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
    FROM    safre_tmp:tmp_pla_acr
   
    LET cont = 0
    LET c2_ident_operacion = ""
    FOREACH cur_1 INTO carga_reg
        LET cont = cont + 1

               #---ENCABEZADO SOLICITUD DE TRANSFERENCIA---#
        IF carga_reg[5,6] = "01" THEN
            LET c2_ident_operacion = "01"
            LET reg_cza_tra_acr.tipo_registro     = carga_reg[001,002] 
            LET reg_cza_tra_acr.ident_servicio    = carga_reg[003,004]
            LET reg_cza_tra_acr.ident_operacion   = carga_reg[005,006]
            LET reg_cza_tra_acr.tipo_ent_origen   = carga_reg[007,008]
            LET reg_cza_tra_acr.cve_ent_origen    = carga_reg[009,011]
            LET reg_cza_tra_acr.tipo_ent_destino  = carga_reg[012,013]
            LET reg_cza_tra_acr.cve_ent_destino   = carga_reg[014,016]
            LET reg_cza_tra_acr.ent_fed_envio_lote= carga_reg[017,019]
            LET c_paso_cza                        = carga_reg[020,027]
            LET reg_cza_tra_acr.consec_lote_dia   = carga_reg[028,030]
            LET reg_cza_tra_acr.cve_mod_recepcion = carga_reg[031,032]
            LET reg_cza_tra_acr.cod_result_operac = carga_reg[033,034]
            LET reg_cza_tra_acr.rechazo           = carga_reg[035,043]

            LET c_fecha_cza = c_paso_cza[5,6],"/",
                              c_paso_cza[7,8],"/",
                              c_paso_cza[1,4]

            LET reg_cza_tra_acr.fecha_presentacion = c_fecha_cza

            INSERT INTO safre_tmp:cza_tra_acr VALUES(reg_cza_tra_acr.*)
        END IF

                #---DETALLE DE SOLICITUD DE TRANSFERENCIA---#
        IF carga_reg[1,2] = "02" AND c2_ident_operacion = "01" THEN
            LET reg_det_tra_acr.tipo_registro      = carga_reg[001,002]
            LET reg_det_tra_acr.cont_servicio      = carga_reg[003,012]
            LET reg_det_tra_acr.tipo_recep_cuenta  = carga_reg[013,014]
            LET reg_det_tra_acr.cve_recep_cuenta   = carga_reg[015,017]
            LET reg_det_tra_acr.tipo_ced_cuenta    = carga_reg[018,019]
            LET reg_det_tra_acr.cve_ced_cuenta     = carga_reg[020,022]
            LET reg_det_tra_acr.tipo_transferencia = carga_reg[023,024]
            LET c_paso_det                         = carga_reg[025,032]
            LET reg_det_tra_acr.n_unico_infonavit  = carga_reg[041,058]
            LET reg_det_tra_acr.nss_infonavit      = carga_reg[059,069]
            LET reg_det_tra_acr.rfc_infonavit      = carga_reg[085,097]
            LET reg_det_tra_acr.paterno_infonavit  = carga_reg[098,137]
            LET reg_det_tra_acr.materno_infonavit  = carga_reg[138,177]
            LET reg_det_tra_acr.nombres_infonavit  = carga_reg[178,217]
            LET reg_det_tra_acr.ident_lote_devol   = carga_reg[240,255]
            LET reg_det_tra_acr.nss_afore          = carga_reg[271,281]
            LET reg_det_tra_acr.rfc_afore          = carga_reg[282,294]
            LET reg_det_tra_acr.paterno_afore      = carga_reg[325,364]
            LET reg_det_tra_acr.materno_afore      = carga_reg[365,404]
            LET reg_det_tra_acr.nombres_afore      = carga_reg[405,444]
            LET reg_det_tra_acr.ult_apor_viv_97    = carga_reg[490,504]
            LET reg_det_tra_acr.cod_result_operac  = carga_reg[583,584]
            LET reg_det_tra_acr.diag_proceso       = carga_reg[585,599]
            LET reg_det_tra_acr.nombre_imss        = carga_reg[600,649]
            LET reg_det_tra_acr.num_cred_infonavit = carga_reg[650,659]

            LET c_fecha_det = c_paso_det[5,6],"/",
                              c_paso_det[7,8],"/",
                              c_paso_det[1,4]
            LET reg_det_tra_acr.fecha_presentacion  = c_fecha_det
 
            INSERT INTO safre_tmp:det_tra_acr VALUES(reg_det_tra_acr.*,0)

            UPDATE cta_ctr_cuenta
            SET    estado_proceso = 230 ,
                   fecha_edo_proceso = today
            WHERE  nss = reg_det_tra_acr.nss_afore 

        END IF

                #---SUMARIO DE SOLICITUD DE TRANSFERENCIA---#
        IF carga_reg[1,2] = "09" AND c2_ident_operacion = "01" THEN
            LET c2_ident_operacion = ""
            LET reg_sum_tra_acr.tipo_registro       = carga_reg[001,002]
            LET reg_sum_tra_acr.cant_reg_det        = carga_reg[003,011]
            LET reg_sum_tra_acr.sum_ult_apor_viv97  = carga_reg[057,071]

            INSERT INTO safre_tmp:sum_tra_acr VALUES(reg_sum_tra_acr.*)
        END IF

    END FOREACH

END FUNCTION

FUNCTION impresion_reporte()
#ir-------------------------

  DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore
  DEFINE g_param_dis    RECORD LIKE dis_parametro.*

  DEFINE g_usuario      CHAR (08)
  DEFINE G_IMPRE        CHAR(300)
  DEFINE c_impre        CHAR(300)
  DEFINE gimpresion     CHAR(300)
  DEFINE hora           CHAR (08)
  DEFINE hoy            DATE

  SELECT  codigo_afore,USER
  INTO    w_codigo_afore,g_usuario
  FROM    tab_afore_local

  SELECT  ruta_spool
  INTO    g_param_dis.ruta_spool
  FROM    dis_parametro

  LET hoy = today

  LET G_IMPRE = g_param_dis.ruta_spool CLIPPED,"/",g_usuario CLIPPED,
              ".SOL_ACR.",hoy USING "DD-MM-YYYY",
               "_",hora CLIPPED

  START REPORT det_tra_acr_imp TO  G_IMPRE

  OUTPUT TO REPORT det_tra_acr_imp(g.*)

  FINISH REPORT det_tra_acr_imp

  --LET gimpresion = "lp ",G_IMPRE
  --RUN gimpresion

  LET c_impre = ' chmod 777 ', G_IMPRE CLIPPED
  RUN c_impre

END FUNCTION

REPORT det_tra_acr_imp(g)
#dtai--------------------

  DEFINE g              RECORD
    total               INTEGER,
    tipo_transferencia  CHAR(2),
    descripcion         CHAR(20)
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
      PRINT COLUMN 08,"TOTAL DE REGISTROS SOLICITADOS PARA TRANSFERENCIA DE ACREDITADOS"
      SKIP 1 LINE 
      PRINT COLUMN 01,"-------------------------------------------------------------"
      PRINT COLUMN 01,"TOTAL DE REGISTROS",
            COLUMN 22,"TIPO DE TRANSFERENCIA",
            COLUMN 50,"DESCRIPCION"
      PRINT COLUMN 01,"-------------------------------------------------------------"
      SKIP 1 LINE 
    ON EVERY ROW

      DECLARE cursor CURSOR FOR
       
      SELECT COUNT(*),
             tipo_transferencia
      FROM safre_tmp:det_tra_acr
      GROUP BY 2
      ORDER BY 2
  
      FOREACH cursor INTO g.total,
                          g.tipo_transferencia
        IF g.tipo_transferencia = "03" THEN
           LET g.descripcion = "TRANSFERENCIA TOTAL"
        ELSE
           LET g.descripcion = "ULTIMA APORTACION"
        END IF
       
        PRINT COLUMN 10,g.total,
              COLUMN 32,g.tipo_transferencia,
              COLUMN 50,g.descripcion
      END FOREACH

     ON LAST ROW
        SKIP 4 LINES
        PRINT COLUMN 2, "Total de tipos de transferencia encontrados: ",
        COUNT(*) USING "<<<<"

     PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
      PAUSE "Presione enter para continuar...."
END REPORT

FUNCTION actualiza_operacion()
#ao---------------------------

    UPDATE bat_ctr_operacion
    SET    estado_operacion = 4,
           fecha_fin        = CURRENT
    WHERE  pid         = reg_bat.pid
    AND    proceso_cod = reg_bat.proceso_cod
    AND    opera_cod   = reg_bat.opera_cod

   {
    UPDATE bat_ctr_proceso 
	 SET    estado_proceso = 4,
			  fecha_fin       = CURRENT
    WHERE  pid         = reg_bat.pid
    AND    proceso_cod = reg_bat.proceso_cod
   }

END FUNCTION 

