##############################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                            #
#Propietario       => E.F.P.                                                 #
#Programa ACRB021  => GENERA ARCHIVO DE SOLICITUDES RECHAZADAS (USO CREDITO) #
#Por               => MAURO MUNIZ CABALLERO                                  #
#Fecha creacion    => 1 DE AGOSTO DE 2002                                    #
#Sistema           => ACR                                                    #
#Modifico          => DMR 30/JULIO/2014 MLM-2683 STARTLOG PERSONALIZADO,     #
#                  => Se modifico validacion para no permitir que se provi-  #
#                     sione y liquide monto mayor al saldo es decir, evitar  #
#                     quebrantos verificando saldo y montos comprometidos    #
#Modifico          => PST-1596 confirmacion de impresion de listados         #
##############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      fecha_conver          ,
      fecha_envio           ,
      HOY                   ,
      vfecha_presentacion   DATE

   DEFINE 
      enter                 CHAR(1),
      c2_motivo_devol       CHAR(2),
      HORA                  CHAR(5),
      c5_HORA               CHAR(5),
      periodo_1             CHAR(6),
      c8_presenta           CHAR(8),
      g_usuario             CHAR(8),
      g_cza                 CHAR(100),
      g_det                 CHAR(100),
      g_sum                 CHAR(100),
      cat                   CHAR(300)

   DEFINE 
      existe                ,
      bandera               ,
      dev                   ,
      sw_1                  ,
      s_lotes_num           ,
      s_lotes_correlativo   ,
      s_codigo_afore        SMALLINT,
      contar_devol          ,
      s_contador            INTEGER

   DEFINE 
      ult_aport_v97         DECIMAL(15,6),
      cont_reg              DECIMAL(10,0),
      vsum_ult_ap_viv97     DECIMAL(15,2)

   DEFINE
      reg_cza_devol_sol     RECORD
                               tipo_registro        CHAR(02),
                               ident_servicio       CHAR(02),
                               ident_operacion      CHAR(02),
                               tipo_ent_origen      CHAR(02),
                               cve_ent_origen       CHAR(03),
                               tipo_ent_destino     CHAR(02),
                               cve_ent_destino      CHAR(03),
                               ent_fed_envio_lote   CHAR(03),
                               fecha_presentacion   DATE,
                               consec_lote_dia      SMALLINT,
                               cve_mod_recepcion    CHAR(02),
                               cod_result_operac    CHAR(02),
                               mot_rechazo_lote     CHAR(09) 
                            END RECORD
 
   DEFINE
      reg_det_devol_sol     RECORD 
                               tipo_registro        CHAR(002),
                               cont_servicio        DECIMAL(10,0),
                               tipo_recep_cuenta    CHAR(002),
                               cve_recep_cuenta     CHAR(003),
                               tipo_ced_cuenta      CHAR(002),
                               cve_ced_cuenta       CHAR(003),
                               tipo_transferencia   CHAR(002),
                               fecha_presentacion   DATE,
                               n_unico_infonavit    CHAR(018),
                               nss_infonavit        CHAR(011),
                               rfc_infonavit        CHAR(013),
                               paterno_infonavit    CHAR(040),
                               materno_infonavit    CHAR(040),
                               nombres_infonavit    CHAR(040),
                               ident_lote_devol     CHAR(016),
                               nss_afore            CHAR(011),
                               rfc_afore            CHAR(013),
                               paterno_afore        CHAR(040),
                               materno_afore        CHAR(040),
                               nombres_afore        CHAR(040),
                               ult_aport_viv_97     DECIMAL(15,2),
                               cod_result_operac    CHAR(002),
                               diag_proceso         CHAR(015),
                               nombre_imss          CHAR(050),
                               num_cred_infonavit   DECIMAL(10,0),
                               motivo_devolucion    CHAR(002),
                               periodo_pago         CHAR(006)
                            END RECORD

   DEFINE
      reg_sum_devol_sol     RECORD
                               tipo_registro        CHAR(02),
                               cantidad_reg_det     DECIMAL(9,0),
                               sum_ult_apor_viv97   DECIMAL(15,2)
                            END RECORD

   DEFINE
      g                     RECORD
                               total                INTEGER,
                               motivo_devolucion    CHAR(2),
                               descripcion          CHAR(30)
                            END RECORD

   DEFINE
      g_param_taa           RECORD LIKE seg_modulo.*

END GLOBALS


MAIN
   OPTIONS INPUT WRAP,
           PROMPT  LINE LAST,
           ACCEPT KEY CONTROL-I
           DEFER INTERRUPT

   CALL STARTLOG(FGL_GETENV("USER")||".ACRB021.log")      #MLM-2683
   CALL inicio() #i
   CALL proceso_principal() #pp
END MAIN


FUNCTION proceso_principal()
#pp-------------------------
   INITIALIZE vfecha_presentacion TO NULL
 
   OPEN WINDOW v1 AT 4,4 WITH FORM "ACRB0011" ATTRIBUTE(BORDER)
   DISPLAY " ACRB021 DEVOLUCION DE SOLICITUDES PARA USO DE LA GARANTIA                     " AT 3,1 ATTRIBUTE(REVERSE)   
   DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)

   INPUT BY NAME vfecha_presentacion WITHOUT DEFAULTS

      AFTER FIELD vfecha_presentacion
         IF vfecha_presentacion IS NULL THEN
            ERROR " FECHA NO PUEDE SER NULA "
            NEXT FIELD vfecha_presentacion
         END IF
         
      ON KEY (ESC)
         IF vfecha_presentacion IS NULL THEN
            ERROR " FECHA NO PUEDE SER NULA "
            NEXT FIELD vfecha_presentacion
         END IF
   
         SELECT COUNT(*)                  
         INTO   dev                       
         FROM   safre_tmp:det_devol_uso   
   
         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NO EXISTEN SOLICITUDES A DEVOLVER "
            NEXT FIELD vfecha_presentacion
         END IF
   
         ERROR " PROCESANDO INFORMACION ..."
         SLEEP 3
         ERROR ""

         LET c8_presenta  = vfecha_presentacion USING "YYYYMMDD"
         LET fecha_conver = MDY(MONTH(vfecha_presentacion),
                                1,YEAR(vfecha_presentacion))
         
         CALL crea_tabla()              #ct
         CALL verifica_devolucion()     #vd

         CALL genera_cza_devol_sol()    #gcds
         CALL genera_det_devol_sol()    #gdds
         CALL genera_sum_devol_sol()    #gsds
        
         LET cat = "cat ",g_param_taa.ruta_envio CLIPPED,"/CDU ",
                          g_param_taa.ruta_envio CLIPPED,"/DDU ",
                          g_param_taa.ruta_envio CLIPPED,"/SDU > ",
                          g_param_taa.ruta_envio CLIPPED,"/",
                          "DEV_USO.",HOY USING "YYMMDD"
        
         RUN cat

         CALL impresion_reporte()

         DISPLAY "                        " AT 19,1 
         
         DISPLAY "ARCHIVO GENERADO EN LA RUTA: ",g_param_taa.ruta_envio CLIPPED AT 13,12
         DISPLAY "CON EL NOMBRE : ", "DEV_USO.",HOY USING "YYMMDD" AT 14,12
         
         DISPLAY "REPORTE GENERADO EN LA RUTA: ",g_param_taa.ruta_listados CLIPPED AT 16,12
         DISPLAY "CON EL NOMBRE : ", g_usuario CLIPPED,
                                     ".DEV_USO.",HOY USING "DD-MM-YYYY" AT 17,12

         PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR enter
         EXIT INPUT

      ON KEY (INTERRUPT, CONTROL-C)
         PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR enter
         EXIT INPUT
   END INPUT

   CLOSE WINDOW v1
END FUNCTION


FUNCTION inicio()
#i---------------
   LET HOY     = TODAY
   LET HORA    = TIME
   LET c5_HORA = HORA

   SELECT codigo_afore, USER
   INTO   s_codigo_afore, g_usuario
   FROM   tab_afore_local

   SELECT *
   INTO   g_param_taa.*
   FROM   seg_modulo
   WHERE  modulo_cod = 'acr'
END FUNCTION


FUNCTION crea_tabla()
#ct------------------
   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp
      DROP TABLE det_devol_uso

      CREATE TABLE det_devol_uso
         (
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
          ult_aport_viv_97      DECIMAL(15,02),
          cod_result_operac     CHAR(002),
          diag_proceso          CHAR(015),
          nombre_imss           CHAR(050),
          num_cred_infonavit    DECIMAL(10,0),
          motivo_devolucion     SMALLINT, 
          periodo_pago 	        CHAR(006)
         )
      
      DATABASE safre_af
   WHENEVER ERROR STOP
END FUNCTION


FUNCTION verifica_devolucion()
#vd---------------------------
   DEFINE
      suma_v97     DECIMAL(18,6),
      suma_v92     DECIMAL(18,6),
      suma_tot     DECIMAL(18,6)
      
   DEFINE
      edo_proc     SMALLINT,
      bnd_proc     SMALLINT
      
   DEFINE
      fecha_pr     DATE

   LET bandera      = 0
   LET suma_v97     = 0
   LET suma_v92     = 0
   LET suma_tot     = 0
   LET s_contador   = 1
   LET contar_devol = 1

   DECLARE cursor_1 CURSOR FOR
   SELECT tipo_registro,
          cont_servicio,
          tipo_recep_cuenta,
          cve_recep_cuenta,
          tipo_ced_cuenta,
          cve_ced_cuenta,
          tipo_transferencia,
          fecha_presentacion,
          n_unico_infonavit,
          nss_infonavit,
          rfc_infonavit,
          paterno_infonavit,
          materno_infonavit,
          nombres_infonavit,
          ident_lote_devol,
          nss_afore,
          rfc_afore,
          paterno_afore,
          materno_afore,
          nombres_afore,
          saldo_viv_97,
          cod_result_operac,
          diag_proceso,
          nombre_imss,
          num_cred_infonavit,
          estado, 
          periodo_pago
   FROM   safre_tmp:det_garantia
   WHERE  estado <> 0

   FOREACH cursor_1 INTO reg_det_devol_sol.*
      LET reg_det_devol_sol.cont_servicio = contar_devol

      INSERT INTO safre_tmp:det_devol_uso
      VALUES (reg_det_devol_sol.*)

      LET contar_devol = contar_devol + 1
   END FOREACH
END FUNCTION


FUNCTION genera_cza_devol_sol()
#gcds--------------------------
   LET reg_cza_devol_sol.tipo_registro      = "01"
   LET reg_cza_devol_sol.ident_servicio     = "02"
   LET reg_cza_devol_sol.ident_operacion    = "06"
   LET reg_cza_devol_sol.tipo_ent_origen    = "01"
   LET reg_cza_devol_sol.cve_ent_origen     = s_codigo_afore
   LET reg_cza_devol_sol.tipo_ent_destino   = "04"
   LET reg_cza_devol_sol.cve_ent_destino    = "002" 
   LET reg_cza_devol_sol.ent_fed_envio_lote = "009"
   LET reg_cza_devol_sol.fecha_presentacion = c8_presenta
   LET reg_cza_devol_sol.consec_lote_dia    = 6
   LET reg_cza_devol_sol.cve_mod_recepcion  = NULL
   LET reg_cza_devol_sol.cod_result_operac  = NULL
   LET reg_cza_devol_sol.mot_rechazo_lote   = NULL

   LET g_cza = g_param_taa.ruta_envio CLIPPED, "/CDU "

   START REPORT listado_1 TO g_cza
      OUTPUT TO REPORT listado_1(reg_cza_devol_sol.*) #1
   FINISH REPORT listado_1
END FUNCTION


REPORT listado_1(reg_cza_devol_sol)
#1---------------------------------
   DEFINE
      reg_cza_devol_sol RECORD
                           tipo_registro        CHAR(02),
                           ident_servicio       CHAR(02),
                           ident_operacion      CHAR(02),
                           tipo_ent_origen      CHAR(02),
                           cve_ent_origen       CHAR(03),
                           tipo_ent_destino     CHAR(02),
                           cve_ent_destino      CHAR(03),
                           ent_fed_envio_lote   CHAR(03),
                           fecha_presentacion   DATE,
                           consec_lote_dia      SMALLINT,
                           cve_mod_recepcion    CHAR(02),
                           cod_result_operac    CHAR(02),
                           mot_rechazo_lote     CHAR(09) 
                        END RECORD

   OUTPUT
      PAGE LENGTH   1
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

   FORMAT
      ON EVERY ROW 
         PRINT
            COLUMN 01,reg_cza_devol_sol.tipo_registro,
                      reg_cza_devol_sol.ident_servicio,
                      reg_cza_devol_sol.ident_operacion,
                      reg_cza_devol_sol.tipo_ent_origen,
                      reg_cza_devol_sol.cve_ent_origen,
                      reg_cza_devol_sol.tipo_ent_destino,
                      reg_cza_devol_sol.cve_ent_destino,
                      reg_cza_devol_sol.ent_fed_envio_lote,
                      c8_presenta,
                      reg_cza_devol_sol.consec_lote_dia    USING "&&&",
                      reg_cza_devol_sol.cve_mod_recepcion,
                      reg_cza_devol_sol.cod_result_operac,
                      reg_cza_devol_sol.mot_rechazo_lote,
                      687 spaces 
END REPORT


FUNCTION genera_det_devol_sol()
#gdds--------------------------
   LET vsum_ult_ap_viv97 = 0 

   DECLARE cur_1 CURSOR FOR
   SELECT tipo_transferencia,
          n_unico_infonavit,
          nss_infonavit,
          rfc_infonavit,
          paterno_infonavit,
          materno_infonavit,
          nombres_infonavit,
          ident_lote_devol,
          nss_afore,
          rfc_afore,
          paterno_afore,
          materno_afore,
          nombres_afore,
          ult_aport_viv_97,
          cod_result_operac,
          diag_proceso,
          nombre_imss,
          num_cred_infonavit,
          motivo_devolucion, 
          periodo_pago
   FROM   safre_tmp:det_devol_uso A
   ORDER BY 1
 
   LET reg_det_devol_sol.tipo_registro      = "02"
   LET reg_det_devol_sol.cont_servicio      = cont_reg
   LET reg_det_devol_sol.tipo_recep_cuenta  = "04"
   LET reg_det_devol_sol.cve_recep_cuenta   = "002"
   LET reg_det_devol_sol.tipo_ced_cuenta    = "01"
   LET reg_det_devol_sol.cve_ced_cuenta     = s_codigo_afore
   LET reg_det_devol_sol.fecha_presentacion = c8_presenta

   LET sw_1     = 0
   LET cont_reg = 1

   LET g_det = g_param_taa.ruta_envio CLIPPED,"/DDU "

   START REPORT listado_2 TO g_det
      FOREACH cur_1 INTO reg_det_devol_sol.tipo_transferencia,
                         reg_det_devol_sol.n_unico_infonavit,
                         reg_det_devol_sol.nss_infonavit,
                         reg_det_devol_sol.rfc_infonavit,
                         reg_det_devol_sol.paterno_infonavit,
                         reg_det_devol_sol.materno_infonavit,
                         reg_det_devol_sol.nombres_infonavit,
                         reg_det_devol_sol.ident_lote_devol,
                         reg_det_devol_sol.nss_afore,
                         reg_det_devol_sol.rfc_afore,
                         reg_det_devol_sol.paterno_afore,
                         reg_det_devol_sol.materno_afore,
                         reg_det_devol_sol.nombres_afore,
                         reg_det_devol_sol.ult_aport_viv_97,
                         reg_det_devol_sol.cod_result_operac,
                         reg_det_devol_sol.diag_proceso,
                         reg_det_devol_sol.nombre_imss,
                         reg_det_devol_sol.num_cred_infonavit,
                         reg_det_devol_sol.motivo_devolucion, 
                         reg_det_devol_sol.periodo_pago

         LET vsum_ult_ap_viv97 = vsum_ult_ap_viv97 +
                                 reg_det_devol_sol.ult_aport_viv_97
         LET sw_1 = 1

         OUTPUT TO REPORT listado_2(reg_det_devol_sol.*)      #2

         DISPLAY "TOTAL DE REGISTROS : ",cont_reg AT 11,20
         
         LET cont_reg = cont_reg + 1  
      END FOREACH

      IF sw_1 = 0 THEN
         DISPLAY "NO SE ENCONTRARON REGISTROS " AT 19,1 ATTRIBUTE(REVERSE)
         SLEEP 3
         EXIT PROGRAM
      END IF
      
      LET cont_reg = cont_reg - 1  
   FINISH REPORT listado_2
END FUNCTION


REPORT listado_2(reg_det_devol_sol)
#2---------------------------------
   DEFINE
      vsdo_viv97        DECIMAL(15,2)
      
   DEFINE
      reg_det_devol_sol RECORD
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
                           ult_aport_viv_97      DECIMAL(15,2),
                           cod_result_operac     CHAR(002),
                           diag_proceso          CHAR(015),
                           nombre_imss           CHAR(050),
                           num_cred_infonavit    DECIMAL(10,0),
                           motivo_devolucion     SMALLINT, 
                           periodo_pago          CHAR(006)
                        END RECORD

   OUTPUT
      PAGE LENGTH   1
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

   FORMAT
      ON EVERY ROW 
         LET vsdo_viv97 = 0
         LET vsdo_viv97 = reg_det_devol_sol.ult_aport_viv_97 * 100
         PRINT
            COLUMN 01,reg_det_devol_sol.tipo_registro,
                      cont_reg USING "&&&&&&&&&&",
                      reg_det_devol_sol.tipo_recep_cuenta,
                      reg_det_devol_sol.cve_recep_cuenta,
                      reg_det_devol_sol.tipo_ced_cuenta,
                      reg_det_devol_sol.cve_ced_cuenta,
                      reg_det_devol_sol.tipo_transferencia,
                      c8_presenta,
                      8 spaces,
                      reg_det_devol_sol.n_unico_infonavit,
                      reg_det_devol_sol.nss_infonavit,
                      15 spaces,
                      reg_det_devol_sol.rfc_infonavit,
                      reg_det_devol_sol.paterno_infonavit,
                      reg_det_devol_sol.materno_infonavit,
                      reg_det_devol_sol.nombres_infonavit,
                      22 spaces,
                      reg_det_devol_sol.ident_lote_devol,
                      15 spaces,
                      reg_det_devol_sol.nss_afore,
                      reg_det_devol_sol.rfc_afore,
                      30 spaces,
                      reg_det_devol_sol.paterno_afore,
                      reg_det_devol_sol.materno_afore,
                      reg_det_devol_sol.nombres_afore,
                      45 spaces,
                      vsdo_viv97                        USING "&&&&&&&&&&&&&&&",
                      78 spaces,
                      reg_det_devol_sol.cod_result_operac,
                      reg_det_devol_sol.diag_proceso,
                      reg_det_devol_sol.nombre_imss,
                      reg_det_devol_sol.num_cred_infonavit USING "&&&&&&&&&&",
                      43 spaces,
                      reg_det_devol_sol.motivo_devolucion  USING "&&",
                      8 spaces,
                      reg_det_devol_sol.periodo_pago,
                      12 spaces
END REPORT


FUNCTION genera_sum_devol_sol()
#gstc--------------------------
   LET reg_sum_devol_sol.tipo_registro      = "09"
   LET reg_sum_devol_sol.cantidad_reg_det   = cont_reg
   LET reg_sum_devol_sol.sum_ult_apor_viv97 = vsum_ult_ap_viv97 * 100

   LET g_sum = g_param_taa.ruta_envio CLIPPED,"/SDU "

   START REPORT listado_3 TO g_sum
      OUTPUT TO REPORT listado_3(reg_sum_devol_sol.*) #3
   FINISH REPORT listado_3
END FUNCTION


REPORT listado_3(reg_sum_devol_sol)
#3---------------------------------
   DEFINE
      reg_sum_devol_sol  RECORD
                            tipo_registro        CHAR(02),
                            cantidad_reg_det     DECIMAL(9,0),
                            sum_ult_apor_viv97   DECIMAL(15,2)
                         END RECORD 

   OUTPUT
      PAGE LENGTH   1
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

   FORMAT
      ON EVERY ROW 
         PRINT
            COLUMN 01,reg_sum_devol_sol.tipo_registro      ,
                      reg_sum_devol_sol.cantidad_reg_det   USING "&&&&&&&&&",
                      45 spaces                            ,
                      reg_sum_devol_sol.sum_ult_apor_viv97 USING "&&&&&&&&&&&&&&&",
                      659 spaces
END REPORT


FUNCTION impresion_reporte()
#ir-------------------------
   DEFINE
      w_codigo_afore LIKE tab_afore_local.codigo_afore

   DEFINE
      G_IMPRE        CHAR(300),
      gimpresion     CHAR(300),
      hora           CHAR (08)

   SELECT codigo_afore
   INTO   w_codigo_afore
   FROM   tab_afore_local
   
   LET G_IMPRE = g_param_taa.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                 ".DEV_USO.",HOY USING "DD-MM-YYYY"
   
   START REPORT _sol_imp TO  G_IMPRE
   
      OUTPUT TO REPORT _sol_imp(g.*)
   
   FINISH REPORT _sol_imp

   PROMPT  "DESEA IMPRIMIR LISTADO S/N ? " FOR enter      #PST-1596
   IF enter = "S"  or enter = "s" THEN
      LET gimpresion = "lp ",G_IMPRE
      RUN gimpresion
   END IF
END FUNCTION


REPORT _sol_imp(g)
#dvsi-------------
   DEFINE
      g            RECORD
                      total               INTEGER,
                      motivo_devolucion   CHAR(2),
                      descripcion         CHAR(30)
                   END RECORD
   
   DEFINE
      i            RECORD LIKE tab_afore_local.*
    
   OUTPUT
      TOP MARGIN     1
      BOTTOM MARGIN  0
      LEFT MARGIN    0
      RIGHT MARGIN   0
      PAGE LENGTH   60
   
   FORMAT
      PAGE HEADER
         SELECT razon_social
         INTO i.razon_social
         FROM tab_afore_local
      
         PRINT COLUMN 01,"ACRB021",
               COLUMN 10,"DEVOLUCIONES DE SOLICITUD PARA USO DE LA GARANTIA (OPERACION 06)",
               COLUMN 75,TODAY USING "dd-mm-yyyy"
               
         SKIP 2 LINE      
      
         PRINT COLUMN 01,"NOMBRE DE ARCHIVO A ENVIAR : ",g_param_taa.ruta_envio CLIPPED,"/",
                                                         "DEV_USO.",HOY USING "YYMMDD"
         
         SKIP 1 LINE
   
         PRINT COLUMN 01,"---------------------------------------------------------------------------------"
         PRINT COLUMN 04,"TOTAL DE REGISTROS",
               COLUMN 25,"MOTIVO DE DEVOLUCION",
               COLUMN 50,"DESCRIPCION"
         PRINT COLUMN 01,"---------------------------------------------------------------------------------"
         
         SKIP 1 LINE
   
      ON EVERY ROW
         DECLARE cur_2 CURSOR FOR
         SELECT COUNT(*), dd.motivo_devolucion
         FROM   safre_tmp:det_devol_uso dd
         GROUP BY 2
         ORDER BY 2
   
         FOREACH cur_2 INTO g.total, g.motivo_devolucion
            CASE g.motivo_devolucion 
               WHEN 8
                  LET g.descripcion = "SALDO CERO"
               WHEN 4
                  LET g.descripcion = "PROCESO DE RETIRO"
               WHEN 11
                  LET g.descripcion = "PROCESO DE TRASPASO"
               WHEN 13
                  LET g.descripcion = "SALDO PREVIAMENTE TRANSFERIDO"
               WHEN 14
                  LET g.descripcion = "MONTOS NO COINCIDEN"
               WHEN 19
                  LET g.descripcion = "PROCESO SEPARACION CUENTAS"
               WHEN 20
                  LET g.descripcion = "PROCESO DEV PAGOS EXCESO"
            END CASE 
         
            PRINT COLUMN 08,g.total,
                  COLUMN 35,g.motivo_devolucion,
                  COLUMN 50,g.descripcion
         END FOREACH                   

      ON LAST ROW
         SKIP 2 LINE 
         
         PRINT COLUMN 01,"---------------------------------------------------------------------------------"
         
         PRINT COLUMN 1, "TOTALES : ", g.total USING "<<<<" 
   
      PAGE TRAILER
         SKIP 2 LINE
         
         PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
         PAUSE "Presione enter para continuar...."
END REPORT


FUNCTION periodo()
#p----------------
   DEFINE 
      s_anyo    SMALLINT,
      c_anyo    CHAR(4),
      mes       CHAR(2),
      c_periodo CHAR(6)

   LET s_anyo = YEAR(fecha_envio)

   CASE MONTH(fecha_envio)
      WHEN 1
         LET mes = "06"
         LET s_anyo = s_anyo - 1
      WHEN 2
         LET mes = "06"
         LET s_anyo = s_anyo - 1
      WHEN 3
         LET mes = "01"
      WHEN 4
         LET mes = "01"
      WHEN 5
         LET mes = "02"
      WHEN 6 
         LET mes = "02"
      WHEN 7
         LET mes = "03"
      WHEN 8
         LET mes = "03"
      WHEN 9
         LET mes = "04"
      WHEN 10
         LET mes = "04"
      WHEN 11
         LET mes = "05"
      WHEN 12
         LET mes = "05"
   END CASE

   LET c_anyo    = s_anyo
   LET c_periodo = c_anyo,mes

   RETURN c_periodo
END FUNCTION

