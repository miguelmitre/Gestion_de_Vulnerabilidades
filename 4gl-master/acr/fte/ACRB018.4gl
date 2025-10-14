############################################################################
#Propietario       => E.F.P.                                               #
#Sistema           => AFORE (Mexico)                                       #
#Programa ACRB018  => RECIBE ARCHIVOS SOLICITUDES USO GARANTIA             #
#Fecha creacion    => 30 DE SEPTIEMBRE DE 2002                             #
#Autor             => MAURO MUNIZ CABALLERO                                #
#Fecha actualiza   => 4 DE AGOSTO DE 2004                                  #
#Actualizacion     => MAURO MUNIZ CABALLERO                                #
#               Se adecuo por proceso de participaciones                   #
#Proceso           => ACR                                                  #
#Modificacion      => EDUARDO JOAQUIN RESENDIZ MEDINA                      #
#Fecha Modifica.   => 18 DE FEBRERO DE 2005                                #
#Modifico          => DMR 30/JULIO/2014 MLM-2683 STARTLOG PERSONALIZADO,   #
#                  => Se modifico validacion para no permitir que se provi-#
#                     sione y liquide monto mayor al saldo es decir, evitar#
#                     quebrantos verificando saldo y montos comprometidos  #
#Modifico          => PST-1596 confirmacion de impresion de listados       #
############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      HOY               ,
      d_fecha_cza       ,
      d_fecha_det       ,
      d_fecha_mov       ,
      vfecha_ini        ,
      vfecha_fin        DATE

   DEFINE
      enter             CHAR(1),
      lote_rechazado    CHAR(2),
      g_usuario         CHAR(8),
      c_paso_cza        CHAR(8),
      c_paso_det        CHAR(8),
      c_paso_mov        CHAR(8),
      c_fecha_cza       CHAR(10),
      c_fecha_det       CHAR(10),
      c_fecha_mov       CHAR(10),
      generar           CHAR(20),
      archivo_traspaso  CHAR(200),
      cat               CHAR(300)

   DEFINE 
      sdo_sal           DECIMAL(18,6),
      sdo_v97           DECIMAL(18,6),
      mot_dev           ,
      bnd_proc          ,
      vdias             ,
      edo_proc          SMALLINT,
      cuantos           ,
      s_codigo_afore    ,
      cont_acep         ,
      cont_rech         ,
      cont_tot          ,
      cont              INTEGER

   DEFINE
      opc               CHAR(1),
      vnss              CHAR(11),
      vmarca_entra      SMALLINT,
      vmarca_estado     SMALLINT,
      vcodigo_rechazo   SMALLINT,
      xcodigo_marca     SMALLINT,
      xcodigo_rechazo   SMALLINT,
      ejecuta           CHAR(300),
      v_marca           CHAR(100)

   DEFINE
      reg_cza_garantia  RECORD
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

   DEFINE
      reg_det_garantia  RECORD
                           tipo_registro         CHAR(002),
                           cont_servicio         INTEGER,
                           tipo_recep_cuenta     CHAR(002),
                           cve_recep_cuenta      CHAR(003),
                           tipo_ced_cuenta       CHAR(002),
                           cve_ced_cuenta        CHAR(003),
                           tipo_transferencia    CHAR(002),
                           fecha_presentacion    DATE,
                           fecha_movimiento      DATE,
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
                           partic_v97            DECIMAL(22,06),
                           saldo_viv_97          DECIMAL(15,02),
                           saldo_viv_92          DECIMAL(15,02),
                           cod_result_operac     CHAR(002),
                           diag_proceso          CHAR(015),
                           nombre_imss           CHAR(050),
                           num_cred_infonavit    DECIMAL(10,0),
                           ints_viv_97           DECIMAL(15,02),
                           ints_viv_92           DECIMAL(15,02),
                           periodo_pago          CHAR(6)
                        END RECORD

   DEFINE
      reg_sum_garantia  RECORD
                           tipo_registro         CHAR(2),
                           cant_reg_det          DECIMAL(9,0),
                           sum_partic_v97        DECIMAL(22,6),
                           sum_sdo_viv97         DECIMAL(15,2),
                           sum_int_viv97         DECIMAL(15,2),
                           sum_int_viv92         DECIMAL(15,2)
                        END RECORD

   DEFINE
      g_param_taa       RECORD LIKE seg_modulo.*

   DEFINE
      g                 RECORD
                           total                 INTEGER,
                           tipo_transferencia    CHAR(2),
                           descripcion           CHAR(20)
                        END RECORD

END GLOBALS


MAIN
   DEFER INTERRUPT
   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I

   CALL STARTLOG(FGL_GETENV("USER")||".ACRB018.log")        #MLM-2683
   CALL inicio() #i
   CALL proceso_principal() #pp
   CALL impresion_reporte() 
   CALL impresion_reporte_fechpres()  

   SELECT count(*)                                          #MLM-2683
   INTO  cont_acep
   FROM  safre_tmp:det_garantia
   WHERE estado = 0

   SELECT count(*)
   INTO  cont_rech
   FROM  safre_tmp:det_garantia
   WHERE estado <> 0

   DISPLAY " REGISTROS ACEPTADOS   : ", cont_acep AT 13,4
   DISPLAY " REGISTROS RECHAZADOS  : ", cont_rech AT 15,4
   ERROR ""

   LET cont_tot = cont_acep + cont_rech

   INSERT INTO acr_ctr_arh
   VALUES(generar,cont_tot,0,0,0,hoy,g_usuario)

   PROMPT " CARGA DE ARCHIVO TERMINADA ...                 Pulse <ENTER> para Salir " FOR CHAR opc
END MAIN


FUNCTION proceso_principal()
#pp-------------------------
   OPEN WINDOW ventana_1 AT 4,4 WITH FORM "ACRC0011" ATTRIBUTE(BORDER)
   DISPLAY " ACRB018    RECIBE SOLICITUDES DE USO DE CREDITO EN GARANTIA               " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME generar
      AFTER FIELD generar
         IF generar IS NULL THEN
            ERROR "Nombre del Archivo NO puede ser NULO"
            NEXT FIELD generar
         ELSE
            SELECT "X"
            FROM   acr_ctr_arh a
            WHERE  a.nombre_archivo = generar
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
               ERROR "ARCHIVO YA PROCESADO !!!"
               NEXT FIELD generar
            END IF
         END IF

         WHENEVER ERROR CONTINUE
            LET archivo_traspaso = g_param_taa.ruta_rescate CLIPPED,
                                   "/",generar CLIPPED

            LOAD FROM archivo_traspaso INSERT INTO safre_tmp:tmp_pla_acr

            SELECT count(*)
            INTO   cuantos
            FROM   safre_tmp:tmp_pla_acr

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
END FUNCTION


FUNCTION inicio()
#i---------------
   LET hoy     = TODAY
   LET sdo_sal = 0

   LET vmarca_estado   = 0
   LET vcodigo_rechazo = 0

   LET edo_proc = 236
   LET bnd_proc = 0
   LET mot_dev  = 0

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

   LET v_marca = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? ) "
END FUNCTION


FUNCTION crea_tablas()
#ct-------------------
   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp

      DROP TABLE cza_garantia
      DROP TABLE det_garantia
      DROP TABLE sum_garantia
   WHENEVER ERROR STOP

   CREATE TABLE cza_garantia
      (tipo_registro         CHAR(02),
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
      );

   CREATE TABLE det_garantia
      (tipo_registro         CHAR(002),
       cont_servicio         INTEGER,
       tipo_recep_cuenta     CHAR(002),
       cve_recep_cuenta      CHAR(003),
       tipo_ced_cuenta       CHAR(002),
       cve_ced_cuenta        CHAR(003),
       tipo_transferencia    CHAR(002),
       fecha_presentacion    DATE,
       fecha_movimiento      DATE,
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
       partic_v97            DECIMAL(22,06),
       saldo_viv_97          DECIMAL(15,02),
       saldo_viv_92          DECIMAL(15,02),
       cod_result_operac     CHAR(002),
       diag_proceso          CHAR(015),
       nombre_imss           CHAR(050),
       num_cred_infonavit    DECIMAL(10,0),
       ints_viv_97           DECIMAL(15,02),
       ints_viv_92           DECIMAL(15,02),
       periodo_pago          CHAR(6),
       estado                SMALLINT
      )

   CREATE TABLE sum_garantia
      (tipo_registro         CHAR(02),
       cant_reg_det          DECIMAL(9,0),
       sum_partic_v97        DECIMAL(22,6),
       sum_sdo_viv97         DECIMAL(15,2),
       sum_int_viv97         DECIMAL(15,2),
       sum_int_viv92         DECIMAL(15,2)
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
      cont                INTEGER
 
   DEFINE 
      c2_ident_operacion  CHAR(2),
      band_gar            CHAR(1),
      carga_reg           CHAR(730)

   DEFINE
      fecha_conver        DATE

   DEFINE
      vmonto_en_acciones  DECIMAL(16,6)

   LET fecha_conver       = MDY(MONTH(HOY),1,YEAR(HOY))
   LET cont               = 0
   LET c2_ident_operacion = ""
   LET sdo_v97            = 0

   SELECT nss, sum(monto_en_pesos) pesos, sum(monto_en_acciones) acciones
   FROM   safre_af:dis_cuenta
   WHERE  nss IN(SELECT UNIQUE(n_registros[59,69])
                 FROM   safre_tmp:tmp_pla_acr
                 WHERE  n_registros[1,2] = '02')
   AND    subcuenta = 4
   GROUP BY 1
   INTO TEMP mto_nss

   DECLARE cur_1 CURSOR FOR
   SELECT  * 
   FROM    safre_tmp:tmp_pla_acr
   
   FOREACH cur_1 INTO carga_reg
      LET cont = cont + 1

              #---ENCABEZADO SOLICITUD DE TRANSFERENCIA---#
       IF carga_reg[5,6] = "01" THEN
          LET c2_ident_operacion = "01"
          LET reg_cza_garantia.tipo_registro     = carga_reg[001,002]
          LET reg_cza_garantia.ident_servicio    = carga_reg[003,004]
          LET reg_cza_garantia.ident_operacion   = carga_reg[005,006]
          LET reg_cza_garantia.tipo_ent_origen   = carga_reg[007,008]
          LET reg_cza_garantia.cve_ent_origen    = carga_reg[009,011]
          LET reg_cza_garantia.tipo_ent_destino  = carga_reg[012,013]
          LET reg_cza_garantia.cve_ent_destino   = carga_reg[014,016]
          LET reg_cza_garantia.ent_fed_envio_lote= carga_reg[017,019]
          LET c_paso_cza                         = carga_reg[020,027]
          LET reg_cza_garantia.consec_lote_dia   = carga_reg[028,030]
          LET reg_cza_garantia.cod_result_operac = carga_reg[033,034]
          LET reg_cza_garantia.rechazo           = carga_reg[035,043]

          LET c_fecha_cza = c_paso_cza[5,6],"/",
                            c_paso_cza[7,8],"/",
                            c_paso_cza[1,4]

          LET reg_cza_garantia.fecha_presentacion = c_fecha_cza

          INSERT INTO safre_tmp:cza_garantia
          VALUES(reg_cza_garantia.*)
       END IF

               #---DETALLE DE SOLICITUD DE TRANSFERENCIA---#
       IF carga_reg[1,2] = "02" AND c2_ident_operacion = "01" THEN
          LET reg_det_garantia.tipo_registro      = carga_reg[001,002]
          LET reg_det_garantia.cont_servicio      = carga_reg[003,012]
          LET reg_det_garantia.tipo_recep_cuenta  = carga_reg[013,014]
          LET reg_det_garantia.cve_recep_cuenta   = carga_reg[015,017]
          LET reg_det_garantia.tipo_ced_cuenta    = carga_reg[018,019]
          LET reg_det_garantia.cve_ced_cuenta     = carga_reg[020,022]
          LET reg_det_garantia.tipo_transferencia = carga_reg[023,024]
          LET c_paso_det                          = carga_reg[025,032]
          LET c_paso_mov                          = carga_reg[033,040]
          LET reg_det_garantia.n_unico_infonavit  = carga_reg[041,058]
          LET reg_det_garantia.nss_infonavit      = carga_reg[059,069]
          LET reg_det_garantia.rfc_infonavit      = carga_reg[085,097]
          LET reg_det_garantia.paterno_infonavit  = carga_reg[098,137]
          LET reg_det_garantia.materno_infonavit  = carga_reg[138,177]
          LET reg_det_garantia.nombres_infonavit  = carga_reg[178,217]
          LET reg_det_garantia.ident_lote_devol   = carga_reg[240,255]
          LET reg_det_garantia.nss_afore          = carga_reg[271,281]
          LET reg_det_garantia.rfc_afore          = carga_reg[282,294]
          LET reg_det_garantia.paterno_afore      = carga_reg[325,364]
          LET reg_det_garantia.materno_afore      = carga_reg[365,404]
          LET reg_det_garantia.nombres_afore      = carga_reg[405,444]
          LET reg_det_garantia.partic_v97         = carga_reg[475,489]
          LET reg_det_garantia.saldo_viv_97       = carga_reg[490,504]
          LET reg_det_garantia.saldo_viv_92       = carga_reg[565,579]
          LET reg_det_garantia.cod_result_operac  = carga_reg[583,584]
          LET reg_det_garantia.diag_proceso       = carga_reg[585,599]
          LET reg_det_garantia.nombre_imss        = carga_reg[600,649]
          LET reg_det_garantia.num_cred_infonavit = carga_reg[650,659]
          LET reg_det_garantia.periodo_pago       = carga_reg[713,718]

          LET reg_det_garantia.ints_viv_97 = 0
          LET reg_det_garantia.ints_viv_92 = 0

          LET c_fecha_det = c_paso_det[5,6],"/",
                            c_paso_det[7,8],"/",
                            c_paso_det[1,4]

          LET reg_det_garantia.fecha_presentacion  = c_fecha_det

          LET c_fecha_mov = c_paso_mov[5,6],"/",
                            c_paso_mov[7,8],"/",
                            c_paso_mov[1,4]

          LET reg_det_garantia.fecha_movimiento = c_fecha_mov

          LET reg_det_garantia.partic_v97   = reg_det_garantia.partic_v97
                                            / 1000000
          LET reg_det_garantia.saldo_viv_97 = reg_det_garantia.saldo_viv_97 
	                                    / 100


          #-- VALIDA SI EL NSS ESTA EN PROCESO DE SUBSECUENTES --  #MLM-2683
          
          LET vfecha_ini = MDY(MONTH(HOY),1,YEAR(HOY))
          
          --LET vfecha_ini = vfecha_ini - 1 UNITS MONTH
          
          CALL fin_mes(HOY)
          RETURNING vfecha_fin,vdias
          
          LET band_gar = 0 
          SELECT NVL(SUM(monto_en_acciones),0) * -1
          INTO   vmonto_en_acciones
          FROM   dis_provision
          WHERE  nss = reg_det_garantia.nss_afore
          AND    subcuenta = 4
          AND    tipo_movimiento = 7 
          AND    fecha_proceso BETWEEN vfecha_ini AND vfecha_fin        

          IF SQLCA.SQLCODE = 0 THEN                  #Si existe movimiento entra.
             IF vmonto_en_acciones >= reg_det_garantia.partic_v97 THEN
                LET reg_det_garantia.partic_v97 = 0  
                LET reg_det_garantia.saldo_viv_97  = 0  
                LET band_gar = 1
             ELSE
                LET reg_det_garantia.partic_v97 = reg_det_garantia.partic_v97 -
                                                  vmonto_en_acciones
             END IF
          END IF                                                #MLM-2683
 
          #-- VALIDA EL SALDO --#
 
          SELECT acciones
          INTO   sdo_v97
          FROM   mto_nss
          WHERE  nss = reg_det_garantia.nss_afore
 
          IF SQLCA.SQLCODE <> 0 THEN
             LET bnd_proc = 1
             LET mot_dev  = 9
          END IF

          IF sdo_v97 < 0.01 OR sdo_v97 IS NULL OR band_gar = 1 THEN #Rechazo Saldo
             LET bnd_proc = 1                                       #menor a GAR 
             LET mot_dev  = 10                                      #o igual cero

             LET reg_det_garantia.partic_v97    = 0
	     LET reg_det_garantia.saldo_viv_97  = 0
             LET sdo_v97  = 0
          ELSE
             IF vmonto_en_acciones >= sdo_v97 THEN
                LET sdo_v97 = 0 
                LET reg_det_garantia.partic_v97 = 0 
                LET reg_det_garantia.saldo_viv_97  = 0  
                LET band_gar = 1

                LET bnd_proc = 1
                LET mot_dev  = 10  
             ELSE
                LET sdo_v97 = sdo_v97 - vmonto_en_acciones
             END IF

             IF reg_det_garantia.partic_v97 > sdo_v97 THEN
                LET reg_det_garantia.partic_v97 = sdo_v97
             END IF
 
             UPDATE mto_nss
             SET    acciones = acciones - reg_det_garantia.partic_v97
             WHERE  nss      = reg_det_garantia.nss_afore
          END IF

          #-- VALIDA MARCA PREVIA 237 --#
           
          SELECT "X"
          FROM   acr_det_tra_cred a
          WHERE  a.nss_afore = reg_det_garantia.nss_afore
          AND    a.estado = 1
          GROUP BY 1

          IF SQLCA.SQLCODE <> 0 THEN
             SELECT "X"
             FROM   cta_act_marca
             WHERE  @nss = reg_det_garantia.nss_afore
             AND    @marca_cod = 237
             GROUP BY 1
           
             IF SQLCA.SQLCODE = 100 THEN
                LET bnd_proc = 1
                LET mot_dev  = 8
             END IF
          END IF

          IF bnd_proc = 1 THEN
             LET vmarca_estado   = 30
             LET vcodigo_rechazo = 900
          ELSE
             LET vmarca_estado   = 0
             LET vcodigo_rechazo = 0
          END IF

          #-- MARCA LA CUENTA CON LA 236 --#
           
          CALL marca_cuenta (reg_det_garantia.nss_afore,
                             edo_proc, vmarca_estado,
                             vcodigo_rechazo, g_usuario,
                             reg_det_garantia.cont_servicio)

          IF mot_dev <> 0 THEN
             LET xcodigo_rechazo = mot_dev
          END IF

          INSERT INTO safre_tmp:det_garantia 
          VALUES(reg_det_garantia.*,xcodigo_rechazo)

          LET bnd_proc = 0
          LET mot_dev  = 0
          LET sdo_sal  = 0
          LET sdo_v97  = 0

          LET vmarca_estado   = 0
          LET vcodigo_rechazo = 0
       END IF

                #---SUMARIO DE SOLICITUD DE TRANSFERENCIA---#
       IF carga_reg[1,2] = "09" AND c2_ident_operacion = "01" THEN
          LET c2_ident_operacion = ""
          LET reg_sum_garantia.tipo_registro  = carga_reg[001,002]
          LET reg_sum_garantia.cant_reg_det   = carga_reg[003,011]
          LET reg_sum_garantia.sum_partic_v97 = carga_reg[042,056]
          LET reg_sum_garantia.sum_sdo_viv97  = carga_reg[057,071]

          LET reg_sum_garantia.sum_int_viv97 = 0
          LET reg_sum_garantia.sum_int_viv92 = 0

          LET reg_sum_garantia.sum_partic_v97 =
              reg_sum_garantia.sum_partic_v97 / 1000000

          LET reg_sum_garantia.sum_sdo_viv97 = 
              reg_sum_garantia.sum_sdo_viv97 / 100

          INSERT INTO safre_tmp:sum_garantia
          VALUES(reg_sum_garantia.*)
       END IF
   END FOREACH
END FUNCTION


FUNCTION impresion_reporte()
#ir-------------------------
   DEFINE
      w_codigo_afore LIKE tab_afore_local.codigo_afore
      
   DEFINE
      hora           CHAR(8)
      
   DEFINE
      G_IMPRE        CHAR(300),
      c_impre        CHAR(300),
      gimpresion     CHAR(300)

   SELECT codigo_afore
   INTO   w_codigo_afore
   FROM   tab_afore_local

   LET hora = TIME

   LET G_IMPRE = g_param_taa.ruta_listados CLIPPED,"/",
                 g_usuario CLIPPED,
                 ".USO_CRED.",hoy USING "DD-MM-YYYY",
                 "_",hora CLIPPED

   START REPORT det_garantia_imp TO G_IMPRE

      OUTPUT TO REPORT det_garantia_imp(g.*)

   FINISH REPORT det_garantia_imp

   PROMPT  "DESEA IMPRIMIR LISTADO S/N ? " FOR enter      #PST-1596
   IF enter = "S"  or enter = "s" THEN
      LET gimpresion = "lp ",G_IMPRE
      RUN gimpresion
   END IF

   LET c_impre = ' chmod 777 ', G_IMPRE CLIPPED
   RUN c_impre
END FUNCTION


REPORT det_garantia_imp(g)
#dtai---------------------
   DEFINE
      g          RECORD
                    total               INTEGER,
                    tipo_transferencia  CHAR(2),
                    descripcion         CHAR(20)
                 END RECORD

   DEFINE
      i          RECORD LIKE tab_afore_local.*

   OUTPUT
      TOP MARGIN    1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60

   FORMAT
      PAGE HEADER
      
      SELECT razon_social
      INTO i.razon_social
      FROM tab_afore_local

      PRINT COLUMN 01,i.razon_social,
            COLUMN 68,TODAY USING "dd-mm-yyyy"
      SKIP 2 LINE
      PRINT COLUMN 08,"TOTAL DE REGISTROS SOLICITADOS PARA USO DEL CREDITO EN GARANTIA"
      SKIP 1 LINE 
      PRINT COLUMN 01,"-------------------------------------------------------------"
      PRINT COLUMN 01,"TOTAL DE REGISTROS",
            COLUMN 22,"TIPO DE TRANSFERENCIA",
            COLUMN 50,"DESCRIPCION"
      PRINT COLUMN 01,"-------------------------------------------------------------"
      SKIP 1 LINE 
      
   ON EVERY ROW
      DECLARE cursor CURSOR FOR
      SELECT COUNT(*), tipo_transferencia
      FROM safre_tmp:det_garantia
      GROUP BY 2
      ORDER BY 2
      
      FOREACH cursor INTO g.total,
                          g.tipo_transferencia
      
         CASE g.tipo_transferencia
            WHEN "18" LET g.descripcion = "USO CREDITO GARANTIA"
            WHEN "03" LET g.descripcion = "TRANSFERENCIA TOTAL"
            WHEN "04" LET g.descripcion = "ULTIMA APORTACION"
            WHEN "70" LET g.descripcion = "TRANSFERENCIA TOTAL"
            WHEN "71" LET g.descripcion = "ULTIMA APORTACION"
         END CASE
      
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


#####################################################################
FUNCTION marca_cuenta(vnss, vmarca_entra, vmarca_edo, vcodigo_rech,
                      vusuario, vcorrelativo)
#mc------------------
   DEFINE
      vnss              CHAR(11),
      vmarca_entra      SMALLINT,
      vmarca_edo        SMALLINT,
      vcodigo_rech      SMALLINT,
      vusuario	        CHAR(08),
      vcorrelativo      INTEGER,
      vmarca_causa      SMALLINT,
      vfecha_causa      DATE

   LET vmarca_causa = 0
   LET vfecha_causa = ""

   PREPARE eje_marca FROM v_marca
   DECLARE cur_marca CURSOR FOR eje_marca

   OPEN cur_marca
   USING vnss	      ,
         vmarca_entra,
         vcorrelativo,
         vmarca_edo  ,
         vcodigo_rech,
         vmarca_causa,
         vfecha_causa,
         vusuario

   FETCH cur_marca
   INTO  xcodigo_marca, xcodigo_rechazo

   CLOSE cur_marca
   FREE  cur_marca 
END FUNCTION


FUNCTION impresion_reporte_fechpres()
#irf-------------------------
   DEFINE
      w_codigo_afore     LIKE tab_afore_local.codigo_afore
   
   --DEFINE g_usuario    CHAR (08)
   
   DEFINE
      G_IMPRE            CHAR(300),
      c_impre            CHAR(300),
      gimpresion         CHAR(300),
      hora               CHAR (08)
   
   DEFINE
      h                  RECORD 
                            nss       CHAR(11),
                            fech_pres DATE,
                            cod_edo   SMALLINT,
                            desc_edo  CHAR(30)      
                         END RECORD
   
   DEFINE
      g_seg_modulo       RECORD LIKE seg_modulo.*
   
   SELECT  codigo_afore
   INTO    w_codigo_afore
   FROM    tab_afore_local
   
   LET hora = TIME
   
   LET G_IMPRE = g_param_taa.ruta_listados CLIPPED,"/",
                 g_usuario CLIPPED,
                 ".USO_CRED_FECHPRES.",hoy USING "DD-MM-YYYY",
                 "_",hora CLIPPED
   
   START REPORT det_garantia_imp_fechpres TO  G_IMPRE
   
      DECLARE cur_report2 CURSOR FOR
      SELECT nss_afore,fecha_presentacion,estado
      FROM   safre_tmp:det_garantia
      WHERE  estado <> 0
      
      FOREACH cur_report2 INTO h.*     
         SELECT rechazo_desc INTO h.desc_edo
         FROM tab_rch_marca
         WHERE rechazo_cod = h.cod_edo
      
         OUTPUT TO REPORT det_garantia_imp_fechpres(h.*)
      END FOREACH
   
   FINISH REPORT det_garantia_imp_fechpres
  
   IF enter = "S"  or enter = "s" THEN                    #PST-1596
      LET gimpresion = "lp ",G_IMPRE
      RUN gimpresion
   END IF 
   
   LET c_impre = ' chmod 777 ', G_IMPRE CLIPPED
   RUN c_impre
END FUNCTION


REPORT det_garantia_imp_fechpres(h)
#dtai--------------------
   DEFINE
      tot_reg      INTEGER
   
   DEFINE
      i            RECORD LIKE tab_afore_local.*

   DEFINE
      h            RECORD
                      nss           CHAR(11),
                      fecha_pres    DATE,
                      cod_edo       SMALLINT,
                      desc_edo      CHAR(30)
                   END RECORD
   
   OUTPUT
      TOP MARGIN    1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
      
   FORMAT
      PAGE HEADER
         SELECT razon_social
         INTO i.razon_social
         FROM tab_afore_local
         
         SELECT COUNT(*)
         INTO   tot_reg
         FROM   safre_tmp:det_garantia
         WHERE estado <> 0
         
         PRINT COLUMN 01,i.razon_social,
               COLUMN 68,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE
         PRINT COLUMN 08,"DEVOLUCION DE REGISTROS DE USO DE GARANTIA"
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


FUNCTION fin_mes(fecha)
#-----------------------
   DEFINE
      mes          SMALLINT,
      fecha        DATE,
      bisiesto     ,
      ano          ,
      v_ban        ,
      ult_dia      SMALLINT,
      v_fecha      CHAR(12),
      v_ano        ,
      v_mes        ,
      v_ult_dia    CHAR(5)

   LET v_ban = FALSE
   LET ano = YEAR(fecha)
   LET bisiesto =  ano MOD 4

   IF bisiesto = 0 THEN
      LET v_ban = TRUE
   END IF

   LET mes = MONTH(fecha)

   IF mes = 4 OR mes = 6 OR mes = 9 OR mes = 11 THEN
      LET ult_dia = 30
   ELSE
      IF mes = 2 THEN
         IF v_ban THEN
            LET ult_dia = 29
         ELSE
            LET ult_dia = 28
         END IF
      ELSE
         LET ult_dia = 31
      END IF
   END IF

   LET v_mes  = mes
   LET v_ano = ano
   LET v_ult_dia = ult_dia
   LET v_fecha = v_mes CLIPPED,"/",v_ult_dia CLIPPED, "/", v_ano
   LET fecha = v_fecha

   RETURN fecha,ult_dia
END FUNCTION

