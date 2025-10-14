######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Propietario       => EFP                                            #
#Programa          => ACRB051                                        #
#Descripcion       => GENERA ARCHIVO OPER.06 DEVOLUCIONES DE         #
#                     ANUALIDADES GARANTIZADAS                       #
#Sistema           => ACR                                            #
#Fecha creacion    => 17 NOVIEMBRE 2009                              #
#Por               => STEFANIE DANIELA VERA PINA                     #
#Modificado        => DMR 05 SEPTIEMBRE 2013                         #
#                  => Estandarizacion de Programas de Anual. Garanti.#
#                  => y adicionalmente montos en sumario DEC(18,6)   #
#                  => MPT 13/09/2012 version 2.0.1    MLM-2163       #
#Modificado        => DMR 31/oct/2013 STARTLOG Personalizado         #
#Modificado        => DMR 05/Mar/2014 MLM-2475 trabajadores con saldo#
#                  => en cero en las subcuentas 4 y 8 son rechazados #
#Modificado        => DMR 1/Dic/2014 CPL-1801 El motivo de rechazo   #
#                  => debe colocarse con formato "&&" segun layout   #
######################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      f_valor              ,
      fecha_conver         ,
      fecha_envio          ,
      HOY                  ,
      vfecha_presentacion  DATE

   DEFINE 
      v_desmarca           CHAR(100),
      g_usuario            CHAR(8),
      periodo_1            CHAR(6),
      c2_motivo_devol      CHAR(2),
      enter                CHAR(1),
      cat                  CHAR(300),
      c8_presenta          CHAR(8),
      g_cza                CHAR(100),
      g_det                CHAR(100), 
      g_sum                CHAR(100),
      nom_archivo          CHAR(50)

   DEFINE 
      pestado_rechazo      , 
      pcodigo_rechazo      ,
      existe               ,
      bandera              ,
      dev                  ,
      sw_1                 ,
      s_lotes_num          ,
      s_lotes_correlativo  ,
      s_codigo_afore       SMALLINT,
      contar_devol         ,
      s_contador           ,
      vcorrelativo         INTEGER

   DEFINE 
      ult_aport_v97        DECIMAL(15,6),
      cont_reg             DECIMAL(10,0),
      vsum_ult_ap_viv97    DECIMAL(15,2),
      vsum_partic_v97      DECIMAL(22,6)
   
   DEFINE
      reg_cza_devol_ag     RECORD 
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
                              cod_result_operac    CHAR(02),
                              mot_rechazo_lote     CHAR(09) 
                           END RECORD

   DEFINE
      reg_det_devol_ag     RECORD 
                              tipo_registro        CHAR(002),
                              cont_servicio        DECIMAL(10,0),
                              tipo_recep_cuenta    CHAR(002),
                              cve_recep_cuenta     CHAR(003),
                              tipo_ced_cuenta      CHAR(002),
                              cve_ced_cuenta       CHAR(003),
                              tipo_transferencia   CHAR(002),
                              fecha_presentacion   DATE,
                              curp_infonavit       CHAR(018),
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
                              cod_result_operac    CHAR(002),
                              diag_proceso         CHAR(015),
                              nombre_imss          CHAR(050),
                              num_cred_infonavit   DECIMAL(10,0),
                              motivo_devolucion    CHAR(002)
                           END RECORD

   DEFINE
      reg_sum_devol_ag     RECORD
                              tipo_registro        CHAR(02),
                              cantidad_reg_det     DECIMAL(9,0)
                           END RECORD

   DEFINE
      g                    RECORD
                              total                INTEGER,
                              motivo_devolucion    CHAR(2),
                              descripcion          CHAR(30)
                           END RECORD

   DEFINE
      g_param_taa          RECORD LIKE seg_modulo.*

END GLOBALS


MAIN
   OPTIONS INPUT WRAP,
   PROMPT LINE LAST,
          ACCEPT KEY CONTROL-I
          DEFER INTERRUPT

   CALL STARTLOG(FGL_GETENV("USER")||".ACRB051.log")
   CALL inicio() #i
   CALL proceso_principal() #pp
END MAIN


FUNCTION proceso_principal()
#pp-------------------------
   OPEN WINDOW ACRB0511 AT 4,4 WITH FORM "ACRB0511" ATTRIBUTE(BORDER)
   DISPLAY " ACRB051  GENERA ARCHIVO OPER. 06 ANUALIDADES GARANTIZADAS                     " AT 3,1 ATTRIBUTE(REVERSE)   
   DISPLAY "<ESC> Ejecutar                                          <Ctrl-C> Salir " AT 1,1   
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)

   LET vfecha_presentacion = HOY

   INPUT BY NAME vfecha_presentacion WITHOUT DEFAULTS

      AFTER FIELD vfecha_presentacion
         IF vfecha_presentacion IS NULL THEN
            ERROR " FECHA CONTESTACION RECHAZOS NO PUEDE SER NULA "
            NEXT FIELD vfecha_presentacion
         END IF

      ON KEY (ESC)
         ERROR " PROCESANDO INFORMACION ..."
         SLEEP 2

         LET c8_presenta  = vfecha_presentacion USING "YYYYMMDD"
         LET f_valor = MDY(MONTH(vfecha_presentacion),
                       1,YEAR(vfecha_presentacion))

         CALL habil_siguiente(f_valor) 
         RETURNING fecha_conver

         CALL crea_tabla() #ct
         CALL verifica_devolucion() #vd

         SELECT COUNT(*)
         INTO   dev
         FROM   safre_tmp:det_devol_ag

         IF dev > 0 THEN
            CALL genera_cza_devol_ag()    #gcda
            CALL genera_det_devol_ag()    #gdda
            CALL genera_sum_devol_ag()    #gsda

            LET cat = "cat ",g_param_taa.ruta_envio CLIPPED,"/CDAG ",
                             g_param_taa.ruta_envio CLIPPED,"/DDAG ",
                             g_param_taa.ruta_envio CLIPPED,"/SDAG > ",
                             g_param_taa.ruta_envio CLIPPED,"/",
                             "DEV_ANU_GAR.",HOY USING "YYMMDD"

            RUN cat
         ELSE
            PROMPT " NO HAY REGISTROS A DEVOLVER...<ENTER> PARA SALIR" FOR enter
         END IF                      

         CALL impresion_reporte()

         LET nom_archivo = "DEV_ANU_GAR.",HOY USING "YYMMDD"

         DISPLAY " EL ARCHIVO HA SIDO GENERADO EN LA RUTA : " AT 12,11
         DISPLAY g_param_taa.ruta_envio CLIPPED AT 13,12
         DISPLAY " CON EL NOMBRE : ", nom_archivo AT 15,11

         ERROR ""
         PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter

         EXIT INPUT

      ON KEY (INTERRUPT)
         PROMPT " PROCESO CANCELADO...<ENTER> PARA CONTINUAR " FOR enter
         EXIT INPUT

   END INPUT
END FUNCTION


FUNCTION inicio()
#i-------------
   LET HOY     = TODAY

   LET pestado_rechazo = 40
   LET pcodigo_rechazo = 900

   SELECT codigo_afore,
          USER
   INTO   s_codigo_afore,
          g_usuario
   FROM   tab_afore_local

   SELECT *
   INTO   g_param_taa.*
   FROM   seg_modulo
   WHERE  modulo_cod = 'acr'

   LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
END FUNCTION


FUNCTION crea_tabla()
#ct------------------
   WHENEVER ERROR CONTINUE
   DATABASE safre_tmp
      DROP TABLE det_devol_ag

      CREATE TABLE det_devol_ag
      (
       tipo_registro        CHAR(002),
       cont_servicio        DECIMAL(10,0),
       tipo_recep_cuenta    CHAR(002),
       cve_recep_cuenta     CHAR(003),
       tipo_ced_cuenta      CHAR(002),
       cve_ced_cuenta       CHAR(003),
       tipo_transferencia   CHAR(002),
       fecha_presentacion   DATE,
       curp_infonavit       CHAR(018),
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
       cod_result_operac    CHAR(002),
       diag_proceso         CHAR(015),
       nombre_imss          CHAR(050),
       num_cred_infonavit   DECIMAL(10,0),
       motivo_devolucion    CHAR(002)
      )

   DATABASE safre_af
   WHENEVER ERROR STOP
END FUNCTION


FUNCTION verifica_devolucion()
#vd---------------------------
   DEFINE 
      suma_v97    DECIMAL(18,6),
      suma_v92    DECIMAL(18,6),
      suma_tot    DECIMAL(18,6),
      l_aivs_v92  DECIMAL(18,6),
      l_aivs_v97  DECIMAL(18,6) 
		  
   DEFINE
      edo_proc ,
      bnd_proc SMALLINT

   LET bandera  = 0
   LET suma_v97 = 0
   LET suma_v92 = 0
   LET suma_tot = 0
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
          curp_infonavit,      
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
          "",
          "",
          nombre_imss,      
          num_cred_infonavit,
          estado,
          aivs_v92,
          aivs_v97
   FROM  safre_tmp:det_acr_ag

   FOREACH cursor_1 INTO reg_det_devol_ag.*, l_aivs_v92, l_aivs_v97

      IF reg_det_devol_ag.motivo_devolucion <> 0 THEN
         LET bandera = 1
      ELSE
         #--- VERIFICA SALDO CERO ---#    

         SELECT NVL(SUM(monto_en_acciones),0)
         INTO   suma_v97
         FROM   dis_cuenta
         WHERE  nss = reg_det_devol_ag.nss_afore
         AND    subcuenta = 4
         AND    tipo_movimiento <> 888
         AND    fecha_valor <= f_valor

         IF suma_v97 IS NULL OR suma_v97 < 0.01 THEN
            LET suma_v97 = 0
         END IF

         SELECT NVL(SUM(monto_en_acciones),0)
         INTO   suma_v92
         FROM   dis_cuenta
         WHERE  nss = reg_det_devol_ag.nss_afore
         AND    subcuenta = 8
         AND    tipo_movimiento <> 888
         AND    fecha_valor <= f_valor

         IF suma_v92 IS NULL OR suma_v92 < 0.01 THEN
            LET suma_v92 = 0
         END IF

         IF (suma_v92 > 0 AND l_aivs_v92 > 0)OR(suma_v97 > 0 AND l_aivs_v97 > 0)
         THEN
            #NSS aceptados
         ELSE
            #NSS rechazados                                #MLM-2475
            LET c2_motivo_devol = "08"                     #MLM-2475
            LET bandera = 1                                #MLM-2475
         END IF

         LET suma_tot = suma_v97 + suma_v92

         IF suma_tot = 0 THEN
            IF suma_tot = 0 AND c2_motivo_devol = "13" THEN
               LET bandera = 1
            ELSE
               LET c2_motivo_devol = "08"
               LET bandera = 1
            END IF
         END IF

         IF suma_tot > 0 AND c2_motivo_devol = "13" THEN 
            LET bandera = 0
         END IF
      END IF

      IF bandera THEN
         LET vcorrelativo = reg_det_devol_ag.cont_servicio

         LET reg_det_devol_ag.cont_servicio = contar_devol 

         IF reg_det_devol_ag.motivo_devolucion = 0 THEN

            LET reg_det_devol_ag.motivo_devolucion = c2_motivo_devol

            UPDATE safre_tmp:det_acr_ag
            SET    estado    = c2_motivo_devol
            WHERE  nss_afore = reg_det_devol_ag.nss_afore
         END IF

         IF reg_det_devol_ag.motivo_devolucion = '8' THEN
            LET reg_det_devol_ag.motivo_devolucion = '08'
         END IF

         SELECT @correlativo
         INTO   vcorrelativo
         FROM   cta_act_marca
         WHERE  @nss = reg_det_devol_ag.nss_afore
         AND    @marca_cod = 235

         CALL desmarca_cuenta(reg_det_devol_ag.nss_afore,
                              235,
                              pestado_rechazo,
                              pcodigo_rechazo,
			      g_usuario,
			      vcorrelativo)

         INSERT INTO safre_tmp:det_devol_ag
         VALUES (reg_det_devol_ag.*)

         LET contar_devol = contar_devol + 1
         LET bandera    = 0
         LET suma_v97   = 0
         LET suma_v92   = 0
         LET suma_tot   = 0
         LET c2_motivo_devol = ''
      ELSE
         LET bandera    = 0
         LET suma_v97   = 0
         LET suma_v92   = 0
         LET suma_tot   = 0
         LET c2_motivo_devol = ''
      END IF

      LET s_contador = s_contador + 1

   END FOREACH
END FUNCTION


FUNCTION genera_cza_devol_ag()
#gcda-------------------------
   LET reg_cza_devol_ag.tipo_registro      = "01"
   LET reg_cza_devol_ag.ident_servicio     = "02"
   LET reg_cza_devol_ag.ident_operacion    = "06"
   LET reg_cza_devol_ag.tipo_ent_origen    = "01"
   LET reg_cza_devol_ag.cve_ent_origen     = s_codigo_afore
   LET reg_cza_devol_ag.tipo_ent_destino   = "04"
   LET reg_cza_devol_ag.cve_ent_destino    = "002" 
   LET reg_cza_devol_ag.ent_fed_envio_lote = "009"
   LET reg_cza_devol_ag.fecha_presentacion = c8_presenta
   LET reg_cza_devol_ag.consec_lote_dia    = 5
   LET reg_cza_devol_ag.cod_result_operac  = NULL
   LET reg_cza_devol_ag.mot_rechazo_lote   = NULL

   LET g_cza = g_param_taa.ruta_envio CLIPPED, "/CDAG "

   START REPORT listado_1 TO g_cza
      OUTPUT TO REPORT listado_1(reg_cza_devol_ag.*) #1
   FINISH REPORT listado_1
END FUNCTION


REPORT listado_1(reg_cza_devol_ag)
#1---------------------------------
   DEFINE
      reg_cza_devol_ag RECORD
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
            COLUMN 01,reg_cza_devol_ag.tipo_registro,
                      reg_cza_devol_ag.ident_servicio,
                      reg_cza_devol_ag.ident_operacion,
                      reg_cza_devol_ag.tipo_ent_origen,
                      reg_cza_devol_ag.cve_ent_origen,
                      reg_cza_devol_ag.tipo_ent_destino,
                      reg_cza_devol_ag.cve_ent_destino,
                      reg_cza_devol_ag.ent_fed_envio_lote,
                      c8_presenta,
                      reg_cza_devol_ag.consec_lote_dia  USING "&&&",
                      2 spaces,
                      reg_cza_devol_ag.cod_result_operac,
                      reg_cza_devol_ag.mot_rechazo_lote,
                      687 spaces
END REPORT


FUNCTION genera_det_devol_ag()
#gdda-------------------------
   LET vsum_ult_ap_viv97 = 0 
   LET vsum_partic_v97   = 0 

   DECLARE cur_1 CURSOR FOR
   SELECT tipo_transferencia,   
          curp_infonavit,     
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
          cod_result_operac,    
          diag_proceso,        
          nombre_imss,        
          num_cred_infonavit, 
          motivo_devolucion 
   FROM   safre_tmp:det_devol_ag A
   ORDER BY 1
 
   LET reg_det_devol_ag.tipo_registro      = "02"
   LET reg_det_devol_ag.cont_servicio      = cont_reg 
   LET reg_det_devol_ag.tipo_recep_cuenta  = "04"
   LET reg_det_devol_ag.cve_recep_cuenta   = "002"
   LET reg_det_devol_ag.tipo_ced_cuenta    = "01"
   LET reg_det_devol_ag.cve_ced_cuenta     = s_codigo_afore
   LET reg_det_devol_ag.fecha_presentacion = c8_presenta

   LET sw_1     = 0
   LET cont_reg = 1

   LET g_det = g_param_taa.ruta_envio CLIPPED,"/DDAG "

   START REPORT listado_2 TO g_det
      FOREACH cur_1 INTO reg_det_devol_ag.tipo_transferencia,
                         reg_det_devol_ag.curp_infonavit,
                         reg_det_devol_ag.nss_infonavit,
                         reg_det_devol_ag.rfc_infonavit,
                         reg_det_devol_ag.paterno_infonavit,
                         reg_det_devol_ag.materno_infonavit,
                         reg_det_devol_ag.nombres_infonavit,
                         reg_det_devol_ag.ident_lote_devol,
                         reg_det_devol_ag.nss_afore,
                         reg_det_devol_ag.rfc_afore,
                         reg_det_devol_ag.paterno_afore,
                         reg_det_devol_ag.materno_afore,
                         reg_det_devol_ag.nombres_afore,
                         reg_det_devol_ag.cod_result_operac,
                         reg_det_devol_ag.diag_proceso,
                         reg_det_devol_ag.nombre_imss,
                         reg_det_devol_ag.num_cred_infonavit,
                         reg_det_devol_ag.motivo_devolucion

         LET sw_1 = 1

         OUTPUT TO REPORT listado_2(reg_det_devol_ag.*) #2

         DISPLAY "TOTAL DE REGISTROS : ", cont_reg AT 10,12

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


REPORT listado_2(reg_det_devol_ag)
#2---------------------------------
   DEFINE
      reg_det_devol_ag RECORD #loc #reg_det_devol_ag
                          tipo_registro        CHAR(002),
                          cont_servicio        DECIMAL(10,0),
                          tipo_recep_cuenta    CHAR(002),
                          cve_recep_cuenta     CHAR(003),
                          tipo_ced_cuenta      CHAR(002),
                          cve_ced_cuenta       CHAR(003),
                          tipo_transferencia   CHAR(002),
                          fecha_presentacion   DATE,
                          curp_infonavit       CHAR(018),
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
                          cod_result_operac    CHAR(002),
                          diag_proceso         CHAR(015),
                          nombre_imss          CHAR(050),
                          num_cred_infonavit   DECIMAL(10,0),
                          motivo_devolucion    CHAR(002)
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
            COLUMN 01,reg_det_devol_ag.tipo_registro,
                     cont_reg USING "&&&&&&&&&&",
                     reg_det_devol_ag.tipo_recep_cuenta,
                     reg_det_devol_ag.cve_recep_cuenta,
                     reg_det_devol_ag.tipo_ced_cuenta,
                     reg_det_devol_ag.cve_ced_cuenta,
                     reg_det_devol_ag.tipo_transferencia,
                     c8_presenta,
                     8 spaces,
                     reg_det_devol_ag.curp_infonavit,
                     reg_det_devol_ag.nss_infonavit,
                     15 spaces,
                     reg_det_devol_ag.rfc_infonavit,
                     reg_det_devol_ag.paterno_infonavit,
                     reg_det_devol_ag.materno_infonavit,
                     reg_det_devol_ag.nombres_infonavit,
                     22 spaces,
                     reg_det_devol_ag.ident_lote_devol,
                     15 spaces,
                     reg_det_devol_ag.nss_afore,
                     reg_det_devol_ag.rfc_afore,
                     30 spaces,
                     reg_det_devol_ag.paterno_afore,
                     reg_det_devol_ag.materno_afore,
                     reg_det_devol_ag.nombres_afore,
                     138 spaces,
                     reg_det_devol_ag.cod_result_operac,
                     reg_det_devol_ag.diag_proceso,
                     reg_det_devol_ag.nombre_imss,
                     reg_det_devol_ag.num_cred_infonavit USING "&&&&&&&&&&",
                     43 spaces,
                     reg_det_devol_ag.motivo_devolucion USING "&&",    #CPL-1801
                     26 spaces
END REPORT


FUNCTION genera_sum_devol_ag()
#gsta-------------------------
   LET reg_sum_devol_ag.tipo_registro      = "09"
   LET reg_sum_devol_ag.cantidad_reg_det   = cont_reg

   LET g_sum = g_param_taa.ruta_envio CLIPPED,"/SDAG "

   START REPORT listado_3 TO g_sum
      OUTPUT TO REPORT listado_3(reg_sum_devol_ag.*) #3
   FINISH REPORT listado_3
END FUNCTION


REPORT listado_3(reg_sum_devol_ag)
#3---------------------------------
   DEFINE
      reg_sum_devol_ag RECORD
                          tipo_registro        CHAR(02),
                          cantidad_reg_det     DECIMAL(9,0)  
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
            COLUMN 01,reg_sum_devol_ag.tipo_registro,
                      reg_sum_devol_ag.cantidad_reg_det   USING "&&&&&&&&&",
                      719 spaces                            
END REPORT


FUNCTION impresion_reporte()
#ir-------------------------
   DEFINE
      G_IMPRE         CHAR(300),
      gimpresion      CHAR(300),
      hora            CHAR (08)
   
   DEFINE
      w_codigo_afore         LIKE tab_afore_local.codigo_afore,
      g_param_dis     RECORD LIKE dis_parametro.*

   SELECT codigo_afore
   INTO   w_codigo_afore
   FROM   tab_afore_local

   LET G_IMPRE = g_param_taa.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                 ".DEV_ANU_GAR.",HOY USING "DD-MM-YYYY"

   START REPORT det_sol_imp TO  G_IMPRE

      OUTPUT TO REPORT det_sol_imp(g.*)

   FINISH REPORT det_sol_imp

   IF w_codigo_afore = 578 THEN           #  578  PEISSSTE
      -- LET gimpresion = "lp ",G_IMPRE
      -- LET gimpresion = "vi ",G_IMPRE
      -- RUN gimpresion
   ELSE
      LET gimpresion = "lp ",G_IMPRE
      -- LET gimpresion = "vi ",G_IMPRE
      RUN gimpresion
   END IF
END FUNCTION


REPORT det_sol_imp(g)
#dvsi-----------------
   DEFINE
      g        RECORD
                  total             INTEGER,
                  motivo_devolucion CHAR(2),
                  descripcion       CHAR(30)
               END RECORD

   DEFINE
      i        RECORD LIKE tab_afore_local.*
  
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
      PRINT COLUMN 08,"TOTAL DE DEVOLUCIONES DE AFORE."
      PRINT COLUMN 08,"ANUALIDADES GARANTIZADAS."
      PRINT COLUMN 04,"----------------------------------------------------------------------"
      PRINT COLUMN 04,"TOTAL DE REGISTROS",
            COLUMN 25,"MOTIVO DE DEVOLUCION",
            COLUMN 50,"DESCRIPCION"
     PRINT COLUMN 04,"----------------------------------------------------------------------"

  ON EVERY ROW
     DECLARE cur_2 CURSOR FOR

     SELECT COUNT(*), dd.motivo_devolucion
     FROM safre_tmp:det_devol_ag dd
     GROUP BY 2
     ORDER BY 2

     FOREACH cur_2 INTO g.total, g.motivo_devolucion
        CASE g.motivo_devolucion 
           WHEN "04"
              LET g.descripcion = "PROCESO DE RETIRO"
           WHEN "08"
              LET g.descripcion = "SALDO CERO"
           WHEN "11"
              LET g.descripcion = "PROCESO DE TRASPASO"
           WHEN "16"
              LET g.descripcion = "PROCESO DE UNIFICACION DE CUENTAS"
           WHEN "19"
              LET g.descripcion = "PROCESO DE SEPARACION DE CUENTAS"
           WHEN "20"
              LET g.descripcion = "PROCESO DE DEVOLUCION DE PAGOS SIN JUSTIFICACION LEGAL"
        END CASE 

        PRINT COLUMN 08,g.total,
              COLUMN 35,g.motivo_devolucion,
              COLUMN 50,g.descripcion
     END FOREACH 

     ON LAST ROW
        SKIP 4 LINES
        PRINT COLUMN 2, "Total de registros encontrados: ", g.total USING "<<<<"

     PAGE TRAILER
        SKIP 2 LINE
        PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
        PAUSE "Presione enter para continuar...."
END REPORT                      


FUNCTION periodo()
#p----------------
   DEFINE 
      s_anyo     SMALLINT,
      c_anyo     CHAR(4),
      mes        CHAR(2),
      c_periodo  CHAR(6)

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

   LET c_anyo = s_anyo
   LET c_periodo = c_anyo,mes

   RETURN c_periodo
END FUNCTION


FUNCTION habil_siguiente(diaActual)
#hs--------------------------------
   DEFINE
      diaTmp        ,
      diaActual     ,
      diaHabilSig   DATE

   DEFINE
      contador      ,
      diaSemana     ,
      feriado       ,
      finSemana     SMALLINT

   LET diaHabilSig = diaActual

   WHILE TRUE
      LET feriado   = 0
      LET finSemana = 0
      LET diaSemana = WEEKDAY(diaHabilSig)  

      IF diaSemana = 0 OR diaSemana = 6 THEN
         LET finSemana = 1
      END IF
             
      SELECT *
      FROM   safre_af:tab_feriado
      WHERE  feria_fecha = diaHabilSig
       
      IF STATUS <> NOTFOUND THEN
         LET feriado = 1
      END IF 
                
      IF feriado = 1 OR finSemana = 1 THEN
         LET diaHabilSig = diaHabilSig + 1 UNITS DAY
      ELSE
         EXIT WHILE
      END IF
   END WHILE

   RETURN diaHabilSig
END FUNCTION


FUNCTION desmarca_cuenta(vnss, vmarca, vmarca_edo, vcodigo_rech,
                      vusuario, vcorrelativo)
#dc-------------------------------------------------------------
   DEFINE
      vnss          CHAR(11),
      vmarca        SMALLINT,
      vmarca_edo    SMALLINT,
      vcodigo_rech  SMALLINT,
      vusuario      CHAR(08),
      vcorrelativo  INTEGER,
      vmarca_causa  SMALLINT,
      vfecha_causa  DATE

   LET vmarca_causa = 0
   LET vfecha_causa = ""

   PREPARE eje_desmarca FROM v_desmarca

   EXECUTE eje_desmarca
   USING vnss,
         vmarca,
         vcorrelativo,
         vmarca_edo  ,
         vmarca_causa,
         vusuario
END FUNCTION

