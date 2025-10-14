############################################################################
#Propietario       => E.F.P.                                               #
#Programa ACRB030  => RECIBE ARCHIVO CREDITO GARANTIA, SOLICITUD DESMARCA  #
#Fecha creacion    => 1 DE JULIO DE 2002                                   #
#Por               => MAURO MUNIZ CABALLERO                                #
#Sistema           => ACR                                                  #
#Modificacion      => Se adecuo anexandole cifras de totales               #
#Fecha Modificacion=> 6 DE FEBRERO DE 2003                                 #
#Modificado por    => FERNANDO HERRERA HERNANDEZ                           #
#Modificacion      => SE ADECUA CON DESMARCA DE ANUALIDADES GARANTIZADAS   #
#Fecha Modificacion=> 13 DE ENERO DE 2010                                  #
#Modificado por    => MAURO MUÑIZ CABALLERO                                #
#Fecha Modificacion=> 09 DE JUNIO DE 2011                                  #
#Modificado por    => ALEJANDRO CHAGOYA SALAZAR.                           #
#                     Se adecua reporte para agregar cuentas marcadas      #
#                     antes y despues de procesar el archivo               #
#Modificado        => DMR 18 junio 2013  MLM-1981                          #
#Fecha             => Se agrego la rutina limpieza para cambiar la \ por   #
#                  => otro caracter Ñ para que no truene el programa       #
#                  => en el marcaje y desmarcaje de las cuentas            #
#Modificado        => DMR 08 julio 2013  MLM-1977                          #
#Modificacion      => Se cambio el llamado de la rutina limpieza la cual se#
#                  => adiciono en el ACR_GLOB.4gl para los demas programas.#
#Modificado        => DMR 21 Agosto 2013 MLM-2087 NO archivo RESPALDO y no #
#                  => se pide confirmacion al usuario para sobreescribir   #
#Modificado        => DMR 26 Marzo 2014 CPL-1476 STARTLOG personalizado    #
#                  => ademas desmarcar 230 y 234  o 234 y 230 para los nss #
#                  => que llegan para desmarca por inconsistencia en Tras--#
#                  => pasos afore cedente - receptora del indicador 2      #
#Modificado        => DMR 21/Abr/2014 CPL-1605 Error dedo edo_proc en lugar#
#                     de edo_proc2                                         #
############################################################################
DATABASE safre_af

GLOBALS
    DEFINE reg_cza_des_cred RECORD
        tipo_registro         CHAR(02),
        ident_servicio        CHAR(02),
        ident_operacion       CHAR(02),
        tipo_ent_origen       CHAR(02),
        cve_ent_origen        CHAR(03),
        tipo_ent_destino      CHAR(02),
        cve_ent_destino       CHAR(03),
        ent_fed_envio_lote    CHAR(03),
        fecha_presentacion    DATE,
        consec_lote_dia       SMALLINT,
        cod_result_operac     CHAR(02),
        rechazo               CHAR(09)
    END RECORD

    DEFINE reg_det_des_cred RECORD
        tipo_registro         CHAR(002),
        cont_servicio         DECIMAL(10,0),
        tipo_recep_cuenta     CHAR(002),
        cve_recep_cuenta      CHAR(003),
        tipo_ced_cuenta       CHAR(002),
        cve_ced_cuenta        CHAR(003),
        tipo_transferencia    CHAR(002),
        fecha_presentacion    DATE,
        n_unico_infonavit     CHAR(018),
        nss_infonavit         CHAR(011),
        rfc_infonavit         CHAR(013),
        paterno_infonavit     CHAR(040),
        materno_infonavit     CHAR(040),
        nombres_infonavit     CHAR(040),
        ident_lote_devol      CHAR(016),
        nss_afore             CHAR(011),
        rfc_afore             CHAR(013),
        paterno_afore         CHAR(040),
        materno_afore         CHAR(040),
        nombres_afore         CHAR(040),
        sdo_viv_97            DECIMAL(15,02),
        sdo_viv_92            DECIMAL(15,02),
        cod_result_operac     CHAR(002),
        diag_proceso          CHAR(015),
        nombre_imss           CHAR(050),
        num_cred_infonavit    DECIMAL(10,0),
        int_viv_97            DECIMAL(15,02),
        int_viv_92            DECIMAL(15,02),
        periodo_pago          CHAR(6)
    END RECORD

    DEFINE reg_sum_des_cred RECORD
        tipo_registro        CHAR(02),
        cant_reg_det         DECIMAL(9,0) 
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
        g_usuario             CHAR(8),
        c_fecha_cza           CHAR(10),
        c_fecha_det           CHAR(10),
        generar               CHAR(25),
        archivo_traspaso      CHAR(200),
        cat                   CHAR(300),
        ejecuta               CHAR(300)

    DEFINE
        xcodigo_marca         ,
        xcodigo_rechazo       ,
        marca                 ,
        bnd_proc              ,
        edo_proc2             ,
        edo_proc              SMALLINT,
        cuantos               ,
        s_codigo_afore        ,
        cont                  INTEGER

    DEFINE g RECORD
        total                 INTEGER,
        tipo_transferencia    CHAR(2),
        marca                 SMALLINT,
        descripcion           CHAR(25)
    END RECORD

    DEFINE tot_arch           INTEGER
    DEFINE tot_desm           INTEGER
    DEFINE tot_ndes           INTEGER

    DEFINE v_afore            CHAR(30)
    DEFINE v_desmarca         CHAR(300)
    DEFINE vcorrelativo       INTEGER

    DEFINE upd_upd            CHAR(6)
    DEFINE upd_tab            CHAR(20)
    DEFINE upd_where          CHAR(90)
    DEFINE upd_hist           CHAR(120)
END GLOBALS


MAIN
   DEFER INTERRUPT
   OPTIONS 
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I

   CALL STARTLOG(FGL_GETENV("USER")||".ACRB030.log")
   CALL inicio() #i
   CALL proceso_principal() #pp
   CALL impresion_reporte() 

   DISPLAY "PROCESO TERMINADO" AT 19,1 ATTRIBUTE(REVERSE)
   SLEEP 2
END MAIN


FUNCTION proceso_principal()
#pp-------------------------
   OPEN WINDOW ventana_1 AT 4,4 WITH FORM "ACRC0011" ATTRIBUTE(BORDER)

   DISPLAY " ACRB030  RECIBE ARCHIVO SOLICITUD DESMARCA CREDITOS VIVIENDA                  " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)

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
    LET HOY        = TODAY
    LET tot_arch   = 0
    LET tot_desm   = 0
    LET tot_ndes   = 0

    SELECT codigo_afore, razon_social, USER
    INTO   s_codigo_afore, v_afore, g_usuario
    FROM   tab_afore_local

    SELECT *
    INTO   g_param_taa.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'acr'

    WHENEVER ERROR CONTINUE
       DATABASE safre_tmp
       DROP TABLE tmp_pla_acr

       CREATE TABLE tmp_pla_acr (n_registros  CHAR(730))
       DATABASE safre_af
    WHENEVER ERROR STOP

    LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
    
    ## Se crea tabla temporal para guardar marcas activas     
    ## para los NSS del archivo   ACS-Jun2011

    CREATE TEMP TABLE tmp_cta_act
    (
      nss        CHAR(11),
      marca      SMALLINT,
      descr      CHAR(50)
    )
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
      cont               INTEGER

   DEFINE
      mot_dev            SMALLINT

   DEFINE
      sdo_v97            DECIMAL(18,6),
      sdo_v92            DECIMAL(18,6)

   DEFINE
      carga_reg          CHAR(730),
      c2_ident_operacion CHAR(2)

   DEFINE
      fecha_conver       DATE

   DEFINE
      x_marca            ,
      bnd_desmarca       SMALLINT

   LET fecha_conver = MDY(MONTH(HOY),1,YEAR(HOY))

   LET bnd_proc = 0

   LET upd_upd   = 'UPDATE'

   DECLARE cur_1 CURSOR FOR
   SELECT  * 
   FROM    safre_tmp:tmp_pla_acr

   LET cont = 0
   LET c2_ident_operacion = ""

   FOREACH cur_1 INTO carga_reg
      LET cont = cont + 1

               #---ENCABEZADO SOLICITUD DE TRANSFERENCIA---#
      IF carga_reg[5,6] = "30" THEN
         LET c2_ident_operacion = "01"
         LET reg_cza_des_cred.tipo_registro     = carga_reg[001,002]
         LET reg_cza_des_cred.ident_servicio    = carga_reg[003,004]
         LET reg_cza_des_cred.ident_operacion   = carga_reg[005,006]
         LET reg_cza_des_cred.tipo_ent_origen   = carga_reg[007,008]
         LET reg_cza_des_cred.cve_ent_origen    = carga_reg[009,011]
         LET reg_cza_des_cred.tipo_ent_destino  = carga_reg[012,013]
         LET reg_cza_des_cred.cve_ent_destino   = carga_reg[014,016]
         LET reg_cza_des_cred.ent_fed_envio_lote= carga_reg[017,019]
         LET c_paso_cza                         = carga_reg[020,027]
         LET reg_cza_des_cred.consec_lote_dia   = carga_reg[028,030]
         LET reg_cza_des_cred.cod_result_operac = carga_reg[033,034]
         LET reg_cza_des_cred.rechazo           = carga_reg[035,043]

         LET c_fecha_cza = c_paso_cza[5,6],"/",
                           c_paso_cza[7,8],"/",
                           c_paso_cza[1,4]

         LET reg_cza_des_cred.fecha_presentacion = c_fecha_cza

         SELECT 'X'
         FROM   acr_cza_des_cred a
         WHERE  a.fecha_presentacion = reg_cza_des_cred.fecha_presentacion

         IF SQLCA.SQLCODE <> 0 THEN
            INSERT INTO acr_cza_des_cred VALUES(reg_cza_des_cred.*)
         END IF
      END IF

                #---DETALLE DE SOLICITUD DE TRANSFERENCIA---#
      IF carga_reg[1,2] = "02" AND c2_ident_operacion = "01" THEN
         LET reg_det_des_cred.tipo_registro      = carga_reg[001,002]
         LET reg_det_des_cred.cont_servicio      = carga_reg[003,012]
         LET reg_det_des_cred.tipo_recep_cuenta  = carga_reg[013,014]
         LET reg_det_des_cred.cve_recep_cuenta   = carga_reg[015,017]
         LET reg_det_des_cred.tipo_ced_cuenta    = carga_reg[018,019]
         LET reg_det_des_cred.cve_ced_cuenta     = carga_reg[020,022]
         LET reg_det_des_cred.tipo_transferencia = carga_reg[023,024]
         LET c_paso_det                          = carga_reg[025,032]
         LET reg_det_des_cred.n_unico_infonavit  = carga_reg[041,058]
         LET reg_det_des_cred.nss_infonavit      = carga_reg[059,069]
         LET reg_det_des_cred.rfc_infonavit      = carga_reg[085,097]
         LET reg_det_des_cred.paterno_infonavit  = carga_reg[098,137]
         LET reg_det_des_cred.materno_infonavit  = carga_reg[138,177]
         LET reg_det_des_cred.nombres_infonavit  = carga_reg[178,217]
         LET reg_det_des_cred.ident_lote_devol   = carga_reg[240,255]
         LET reg_det_des_cred.nss_afore          = carga_reg[271,281]
         LET reg_det_des_cred.rfc_afore          = carga_reg[282,294]
         LET reg_det_des_cred.paterno_afore      = carga_reg[325,364]
         LET reg_det_des_cred.materno_afore      = carga_reg[365,404]
         LET reg_det_des_cred.nombres_afore      = carga_reg[405,444]
         LET reg_det_des_cred.sdo_viv_97         = carga_reg[490,504]
         LET reg_det_des_cred.sdo_viv_92         = carga_reg[565,579]
         LET reg_det_des_cred.cod_result_operac  = carga_reg[583,584]
         LET reg_det_des_cred.diag_proceso       = carga_reg[585,599]
         LET reg_det_des_cred.nombre_imss        = carga_reg[600,649]
         LET reg_det_des_cred.num_cred_infonavit = carga_reg[650,659]
         LET reg_det_des_cred.int_viv_97         = carga_reg[660,674]
         LET reg_det_des_cred.int_viv_92         = carga_reg[675,689]
         LET reg_det_des_cred.periodo_pago       = carga_reg[713,718]
    
         ## Se llama a funcion que valida las marcas activas por cada 
         ## NSS del archivo ACS-Jun2011

         CALL f_marca_act(reg_det_des_cred.nss_afore)  

         LET c_fecha_det = c_paso_det[5,6],"/",
                           c_paso_det[7,8],"/",
                           c_paso_det[1,4]
         LET reg_det_des_cred.fecha_presentacion  = c_fecha_det

         SELECT 'X'
         FROM   acr_det_des_cred c
         WHERE  c.nss_afore = reg_det_des_cred.nss_afore
         AND    c.fecha_presentacion = reg_det_des_cred.fecha_presentacion

         IF SQLCA.SQLCODE <> 0 THEN
            INSERT INTO acr_det_des_cred VALUES(reg_det_des_cred.*,mot_dev)
         END IF

         LET upd_where = 'SET estado = 9 WHERE nss_afore ="', reg_det_des_cred.nss_afore, '" AND (estado = 1 OR estado = 0)'

         CASE reg_det_des_cred.tipo_transferencia      #CPL-1476
            WHEN '01'
               LET edo_proc  = 230
               LET edo_proc2 = 234
            WHEN '02'
               LET edo_proc  = 237
               INITIALIZE edo_proc2 TO NULL            #CPL-1605
            WHEN '04'
               LET edo_proc  = 234
               LET edo_proc2 = 230
         END CASE                                      #CPL-1476

         LET bnd_desmarca = 0
         LET x_marca      = 0

         DECLARE cur_desmarca CURSOR FOR
         SELECT c.marca_cod, c.correlativo
         FROM   cta_act_marca c
         WHERE  c.nss = reg_det_des_cred.nss_afore
         AND    (c.marca_cod = edo_proc OR c.marca_cod = edo_proc2)

         FOREACH cur_desmarca INTO x_marca, vcorrelativo
            CALL desmarca_cuenta(reg_det_des_cred.nss_afore, x_marca, g_usuario,
                                 vcorrelativo)

            LET bnd_desmarca = 1
         END FOREACH

         IF bnd_desmarca THEN
            LET tot_desm = tot_desm + 1
            DISPLAY "Total de registros desmarcados   : ", tot_desm AT 14,4
         END IF

         CASE reg_det_des_cred.tipo_transferencia
            WHEN '01' LET upd_tab = ' acr_det_cedido '
            WHEN '02' LET upd_tab = ' acr_det_tra_cred '
            WHEN '04' LET upd_tab = ' acr_det_ced_ag '
         END CASE

         LET upd_hist  = upd_upd, upd_tab, upd_where CLIPPED

         PREPARE ex_upd FROM upd_hist
         EXECUTE ex_upd

         LET tot_arch = tot_arch + 1
         DISPLAY "Total de registros en el archivo : ", tot_arch AT 13,4 
         LET tot_ndes = tot_arch - tot_desm
         DISPLAY "Total de registros no desmarcados: ", tot_ndes AT 15,4
      END IF

                #---SUMARIO DE SOLICITUD DE TRANSFERENCIA---#
      IF carga_reg[1,2] = "09" AND c2_ident_operacion = "01" THEN
         LET c2_ident_operacion = ""
         LET reg_sum_des_cred.tipo_registro  = carga_reg[001,002]
         LET reg_sum_des_cred.cant_reg_det   = carga_reg[003,011]

         SELECT 'X'
         FROM    acr_sum_des_cred s
         WHERE   s.fecha_presentacion = reg_cza_des_cred.fecha_presentacion

         IF SQLCA.SQLCODE <> 0 THEN
            INSERT INTO acr_sum_des_cred 
            VALUES(reg_sum_des_cred.*,reg_cza_des_cred.fecha_presentacion)
         END IF
      END IF
   END FOREACH

   INSERT INTO acr_ctr_arh
   VALUES(generar,tot_arch,tot_desm,tot_ndes,0,today,g_usuario)

   ERROR ""
   PROMPT "Presione [Enter] p/continuar" FOR enter
END FUNCTION


FUNCTION impresion_reporte()
#ir-------------------------
   DEFINE
      hora           CHAR(8),
      G_IMPRE        CHAR(300),
      c_impre        CHAR(300),
      gimpresion     CHAR(300)

   LET G_IMPRE = g_param_taa.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                 ".DESMARCA_CREDITOS.",hoy USING "DDMMYY"

   START REPORT det_des_cred_imp TO  G_IMPRE

      OUTPUT TO REPORT det_des_cred_imp(g.*)

   FINISH REPORT det_des_cred_imp

   --LET gimpresion = "lp ",G_IMPRE
   --RUN gimpresion

   LET c_impre = ' chmod 775 ', G_IMPRE CLIPPED
   RUN c_impre
END FUNCTION


REPORT det_des_cred_imp(g)
#dtai---------------------
   DEFINE
      fecha_max  DATE

   DEFINE
      g          RECORD
                    nss                 CHAR(11),
                    tipo_transferencia  CHAR(2),
                    marca               SMALLINT,
                    descripcion         CHAR(25)
                 END RECORD,

      r_act      RECORD 
                    nss                 CHAR(11),
                    marca               SMALLINT,
                    mar_des             CHAR(50)
                 END RECORD   

   OUTPUT
      TOP MARGIN    1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  70

   FORMAT

      PAGE HEADER  
	 PRINT COLUMN 02,s_codigo_afore USING "&&&",
               COLUMN 07,v_afore,
               COLUMN 60,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE		
	
         SELECT MAX(fecha_presentacion)
         INTO   fecha_max
         FROM   acr_cza_des_cred		

   ON EVERY ROW
    
      -- Se agregan  registros de marcas activas antes de la desmarca
      -- ACS-jun2011
  
      INITIALIZE r_act.* TO NULL

      PRINT COLUMN 02,"REGISTROS DEL ARCHIVO CON MARCA ACTIVA - ANTES DE LA DESMARCA "
      SKIP 1 LINE 
      PRINT COLUMN 01,"---------------------------------------------------------------------"
      PRINT COLUMN 02,"NSS",
            COLUMN 17,"MARCA",
            COLUMN 28,"DESCRIPCION"
      PRINT COLUMN 01,"---------------------------------------------------------------------"

      DECLARE cur_act CURSOR FOR
      SELECT * FROM tmp_cta_act
      ORDER BY nss, marca

      FOREACH cur_act INTO r_act.*
         PRINT COLUMN 02, r_act.nss,
               COLUMN 17, r_act.marca USING "<<<<",
               COLUMN 28, r_act.mar_des CLIPPED

      END FOREACH    
      SKIP 2 LINE	    
    
      PRINT COLUMN 02,"REGISTROS SOLICITADOS PARA DESMARCA DE CREDITOS VIVIENDA             "
      SKIP 1 LINE 
      PRINT COLUMN 01,"---------------------------------------------------------------------"
      PRINT COLUMN 02,"NSS",
            COLUMN 15,"TIPO DE TRANSFERENCIA",
            COLUMN 43,"MARCA"
      PRINT COLUMN 01,"---------------------------------------------------------------------"
      SKIP 1 LINE 

      DECLARE cursor CURSOR FOR
      SELECT a.nss_afore,
             a.tipo_transferencia,
             DECODE(a.tipo_transferencia,'01',230,'02',237,'04',234),
             DECODE(a.tipo_transferencia,'01','ACREDITADOS','02','CREDITOS 43 BIS','04','ANUALIDADES GARANTIZADAS')
      FROM   acr_det_des_cred a
      WHERE  a.fecha_presentacion = fecha_max
      ORDER BY 2

      FOREACH cursor INTO g.*
         PRINT COLUMN 02,g.nss,
               COLUMN 15,g.tipo_transferencia,
               COLUMN 40,g.marca USING "&&&", 
               COLUMN 45,g.descripcion
      END FOREACH
      SKIP 2 LINE	      
      
      -- Se agregan registros que quedaron con marcas activas despues de la desmarca
      -- ACS-Jun2011

   ON LAST ROW 
      INITIALIZE r_act.* TO NULL
      
      PRINT COLUMN 02,"REGISTROS DEL ARCHIVO CON MARCA ACTIVA - DESPUES DE LA DESMARCA "
      SKIP 1 LINE 
      PRINT COLUMN 01,"---------------------------------------------------------------------"
      PRINT COLUMN 02,"NSS",
            COLUMN 17,"MARCA",
            COLUMN 28,"DESCRIPCION"
      PRINT COLUMN 01,"---------------------------------------------------------------------"
 
      DECLARE cur_act2 CURSOR FOR
      SELECT a.nss_afore, b.marca_cod, c.marca_desc
      FROM   acr_det_des_cred a , cta_act_marca b, tab_marca c
      WHERE  a.fecha_presentacion = fecha_max
      AND    a.nss_afore = b.nss
      AND    b.marca_cod = c.marca_cod
      ORDER BY 1,2

      FOREACH cur_act2 INTO r_act.*
         PRINT COLUMN 02, r_act.nss,
               COLUMN 17, r_act.marca USING "<<<<",
               COLUMN 28, r_act.mar_des CLIPPED	 
      END FOREACH

   PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
END REPORT


FUNCTION desmarca_cuenta(vnss,vmarca,vusuario,vcorrelativo)
#dc--------------------------------------------------------
   DEFINE
      vnss          CHAR(11),
      vmarca        SMALLINT,
      vusuario      CHAR(8),
      vcorrelativo  INTEGER,
      pestado_marca SMALLINT,
      pmarca_causa  SMALLINT

   LET pestado_marca = 0
   LET pmarca_causa  = 0

   PREPARE eje_desmarca FROM v_desmarca

   EXECUTE eje_desmarca
   USING vnss,
         vmarca,
         vcorrelativo,
         pestado_marca,
         pmarca_causa,
         vusuario

END FUNCTION


###########################################################################
#Funcion que valida si existen marcas activas e inserta en tabla
#temporal para reporte    --ACS- Jun2011
###########################################################################
FUNCTION f_marca_act(p_nss)
DEFINE
   p_nss    CHAR(11),
   l_marca  SMALLINT,
   l_desc   CHAR(50)
	  
   LET l_marca = -1 
   LET l_desc = " "

   DECLARE cur_marca CURSOR FOR
   SELECT  a.marca_cod, TRIM(b.marca_desc)
   FROM  cta_act_marca a, tab_marca b
   WHERE a.nss = p_nss
   AND   a.marca_cod = b.marca_cod
    
   FOREACH cur_marca INTO l_marca, l_desc
      INSERT INTO tmp_cta_act VALUES(p_nss,l_marca,l_desc)
   END FOREACH
END FUNCTION

