# ********************************************************************************#
# Modulo    : CTAB047_1                                                           #
# Objetivo  : IDENTIFICACION,MARCAJE,CAMBIO DE REGIMEN Y LIQUIDACION              #
# Funciones : Lista de nombres de las funciones que contiene el modulo            #
# SCCS Id   : VERSION 1.0                                                         # 
# Autor     : STEFANIE DANIELA VERA PIÑA                                          #
# Fecha     : 02 DE NOVIEMBRE DEL 2012                                            #
# Lugar     : /safre/cta/fte                                                      #
# Argumentos:                                                                     #             
# Formas    :                                                                     #
# ********************************************************************************#
DATABASE  safre_af

GLOBALS

   DEFINE
      gd_hoy                  DATE 
      
   DEFINE
      gc_elproc               CHAR(1000),
      gc_comando              CHAR(1000),
      gc_ruta_listado         CHAR(40),
      gc_usuario              CHAR(8),
      gc_operacion            CHAR(2)
       
   DEFINE 
      gi_folio                INTEGER
   
   DEFINE
      gs_codigo_afore         ,
      gs_opcion               SMALLINT,
      gd_fecha_liquida        DATE

END GLOBALS


MAIN

   LET gi_folio = ARG_VAL(1)
   LET gs_opcion = ARG_VAL(2)
   LET gd_fecha_liquida = ARG_VAL(3)
   
   LET gd_hoy = TODAY
   
   SELECT USER,
          ruta_listados
   INTO   gc_usuario,
          gc_ruta_listado
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"   
      
   SELECT codigo_afore
   INTO   gs_codigo_afore
   FROM   tab_afore_local
 
   IF gs_opcion = 1 #--IDENTIFICA CUENTAS EN SB5 Y EN TAA CED. --#
   OR gs_opcion = 2 #--IDENTIFICA CUENTAS EN SB5 --#
   OR gs_opcion = 7 THEN #-- IDENTIFICA CUENTAS EN SB5 Y EN TAA COMPLEMENTARIOS --#
      CALL ins_identifica()
   END IF
   
   IF gs_opcion = 5  #-- REPORTE DE IDENTIFICACION --#
   OR gs_opcion = 8 THEN #--REPORTE DE INDENTIFICACION DE TAA COMPLEMENTARIOS --#
      CALL rpt_identificacion()
   END IF
   
   IF gs_opcion = 3 THEN #-- CAMBIO DE REGIMEN --#
      CALL fn_cambio_regimen()
   END IF

   IF gs_opcion = 6 THEN #-- REPORTE DE CAMBIO DE REGIMEN --#
      CALL fn_genera_reporte_cambio_regimen()
   END IF
               
   IF gs_opcion = 4 THEN#-- LIQUIDA --#
   	CALL fn_liquida()
   END IF

   IF gs_opcion = 9 THEN#-- REPORTE DE LIQUIDACION --#
   	CALL fn_genera_reporte_liquidacion(gi_folio)
   END IF
         
END MAIN


FUNCTION ins_identifica()
#------------------------

   DEFINE lr_reg1 RECORD 
      nss                 CHAR(11),    
      curp                CHAR(18),    
      tipo_solicitud      SMALLINT,    
      siefore_actual      SMALLINT,    
      siefore_cambio      SMALLINT,    
      subcuenta           SMALLINT,    
      monto_en_acciones   DECIMAL(16,6),
      monto_en_pesos      DECIMAL(16,6),
      estado              SMALLINT
   END RECORD

   DEFINE
      lde_precio_del_dia   DECIMAL(16,6)

   LET lr_reg1.siefore_actual = 5
   LET lr_reg1.siefore_cambio  = 4
   LET lr_reg1.estado = 0 

   SELECT precio_del_dia
   INTO   lde_precio_del_dia
   FROM   glo_valor_accion
   WHERE  codigo_siefore = lr_reg1.siefore_actual
   AND    fecha_valuacion = gd_hoy
         
   IF gs_opcion = 1 THEN 
   	
   	  #-- IDENTIFICACION DE CUENTAS EN PROCESO DE TRAPASOS AFORE-AFORE CEDENTE --#
   	  
      SELECT n_seguro
      FROM   taa_cd_det_cedido
      WHERE  folio = gi_folio
      --AND    estado = 101    
      INTO TEMP tmp_ctab047_1   	

    
      #-- IDENTIFICACION DE CUENTAS CON REGIMEN DE INVERSION EN SB5 --#
      
      SELECT UNIQUE nss
      FROM   cta_nss_regimen
      WHERE  nss IN (SELECT n_seguro 
                     FROM   tmp_ctab047_1)
      AND    codigo_siefore = lr_reg1.siefore_actual
      INTO TEMP  tmp_ctab047_2    
      
   END IF  
   
   IF gs_opcion = 7 THEN
   	
      SELECT nss n_seguro     #-- TRASPASOS COMPLEMENTARIOS --#
      FROM   safre_tmp:taa_cd_op_12
      WHERE  estado = 101
      INTO TEMP tmp_ctab047_1    

      #-- IDENTIFICACION DE CUENTAS CON REGIMEN DE INVERSION EN SB5 --#
      
      SELECT UNIQUE nss
      FROM   cta_nss_regimen
      WHERE  nss IN (SELECT n_seguro 
                     FROM   tmp_ctab047_1)
      AND    codigo_siefore = lr_reg1.siefore_actual
      INTO TEMP  tmp_ctab047_2    
                  	
   END IF
   
   IF gs_opcion = 2 THEN 
      #-- IDENTIFICACION DE CUENTAS CON REGIMEN DE INVERSION EN SB5 --#
      
      SELECT UNIQUE nss
      FROM   cta_nss_regimen
      WHERE  codigo_siefore = lr_reg1.siefore_actual
      AND    nss IN ("01008401158","01008501007")
      INTO TEMP  tmp_ctab047_2    
         	
   END IF
      
   #-- LLENADO DE TABLAS DE LA IDENTIFICACION --#
   
   DECLARE cur_1 CURSOR FOR
   SELECT nss
   FROM   tmp_ctab047_2
   
   FOREACH cur_1 INTO lr_reg1.nss
      
      SELECT n_unico,
             tipo_solicitud
      INTO   lr_reg1.curp,
             lr_reg1.tipo_solicitud
      FROM   afi_mae_afiliado
      WHERE  n_seguro = lr_reg1.nss
      
      DECLARE cur_2 CURSOR FOR     
      SELECT subcuenta
      FROM   cta_regimen
      WHERE  nss = lr_reg1.nss
      AND    codigo_siefore = lr_reg1.siefore_actual
      
      FOREACH cur_2 INTO lr_reg1.subcuenta


         SELECT NVL(SUM(monto_en_acciones),0)
         INTO   lr_reg1.monto_en_acciones
         FROM   dis_cuenta
         WHERE  nss = lr_reg1.nss
         AND    subcuenta = lr_reg1.subcuenta
         AND    siefore = lr_reg1.siefore_actual
         
         LET lr_reg1.monto_en_pesos = lr_reg1.monto_en_acciones * lde_precio_del_dia
         
         IF lr_reg1.monto_en_acciones > 0 THEN 
         	  
            INSERT INTO safre_tmp:tmp_saldos_fusion
            VALUES (lr_reg1.nss,              
                    lr_reg1.subcuenta,        
                    lr_reg1.monto_en_acciones,
                    lr_reg1.monto_en_pesos )
         END IF
	
      END FOREACH

      CASE gs_opcion
      	 WHEN 1
      	 	   LET gc_operacion = "01"
      	 WHEN 2
      	 	   LET gc_operacion = "  "
      	 WHEN 7
      	 	   LET gc_operacion = "12"
      END CASE
      	
      	
      INSERT INTO safre_tmp:tmp_fusion_siefores
      VALUES(lr_reg1.nss,           
             lr_reg1.curp,          
             lr_reg1.tipo_solicitud,
             lr_reg1.siefore_actual,
             lr_reg1.siefore_cambio,
             "",                #-- marca_cod --# 
             lr_reg1.estado,    #-- IDENTIFICADO --#
             gc_operacion)                    
      
   END FOREACH      

   DATABASE safre_tmp
   
   SQL
   UPDATE STATISTICS HIGH FOR TABLE tmp_saldos_fusion;
   END SQL
   
   SQL
   UPDATE STATISTICS HIGH FOR TABLE tmp_fusion_siefores;
   END SQL  
   
   DATABASE safre_af              

END FUNCTION


FUNCTION rpt_identificacion()
#-----------------------------
   
   DEFINE reg_fusion_siefores RECORD
      nss                  CHAR(11),
      curp                 CHAR(18),
      tipo_solicitud       CHAR(30),
      siefore_actual       SMALLINT,
      siefore_cambio       SMALLINT,
      estado               SMALLINT
   END RECORD
   
   DEFINE reg_saldos_fusion RECORD  
      subcuenta            SMALLINT     ,   
      monto_en_acciones    DECIMAL(16,6),   
      monto_en_pesos       DECIMAL(16,6) 
   END RECORD

   DEFINE reg_repor RECORD
      fecha_calculo         DATE,
      nss                   CHAR(11),
      curp                  CHAR(18),
      tipo_cta              CHAR(30),
      siefore_ant           SMALLINT,
      siefore               SMALLINT,
      estado                CHAR(25), 
      desc_estado           CHAR(150),
      operacion             CHAR(2),
      monto_subcta13        DECIMAL(16,6),
      monto_subcta30        DECIMAL(16,6),
      monto_subcta31        DECIMAL(16,6),
      monto_subcta32        DECIMAL(16,6),
      monto_subcta33        DECIMAL(16,6),
      monto_subcta34        DECIMAL(16,6),
      monto_subcta36        DECIMAL(16,6),
      monto_subcta11        DECIMAL(16,6),
      monto_subcta12        DECIMAL(16,6),
      monto_subcta40        DECIMAL(16,6),
      monto_subcta41        DECIMAL(16,6),
      monto_subcta1         DECIMAL(16,6),
      monto_subcta2         DECIMAL(16,6),
      monto_subcta5         DECIMAL(16,6),
      monto_subcta6         DECIMAL(16,6),
      monto_subcta9         DECIMAL(16,6),
      monto_subcta17        DECIMAL(16,6),
      monto_subcta18        DECIMAL(16,6),
      monto_subcta24        DECIMAL(16,6),
      monto_subcta25        DECIMAL(16,6),
      monto_subcta15        DECIMAL(16,6),
      monto_subcta16        DECIMAL(16,6),
      monto_subcta26        DECIMAL(16,6),
      monto_subcta27        DECIMAL(16,6),
      monto_subcta37        DECIMAL(16,6),
      monto_subcta7         DECIMAL(16,6),
      monto_subcta20        DECIMAL(16,6),
      monto_subcta21        DECIMAL(16,6),
      monto_subcta28        DECIMAL(16,6),
      monto_subcta29        DECIMAL(16,6), 
      monto_acc_subcta13    DECIMAL(16,6),
      monto_acc_subcta30    DECIMAL(16,6),
      monto_acc_subcta31    DECIMAL(16,6),
      monto_acc_subcta32    DECIMAL(16,6),
      monto_acc_subcta33    DECIMAL(16,6),
      monto_acc_subcta34    DECIMAL(16,6),
      monto_acc_subcta36    DECIMAL(16,6),
      monto_acc_subcta11    DECIMAL(16,6),
      monto_acc_subcta12    DECIMAL(16,6),
      monto_acc_subcta40    DECIMAL(16,6),
      monto_acc_subcta41    DECIMAL(16,6),
      monto_acc_subcta1     DECIMAL(16,6),
      monto_acc_subcta2     DECIMAL(16,6),
      monto_acc_subcta5     DECIMAL(16,6),
      monto_acc_subcta6     DECIMAL(16,6),
      monto_acc_subcta9     DECIMAL(16,6),
      monto_acc_subcta17    DECIMAL(16,6),
      monto_acc_subcta18    DECIMAL(16,6),
      monto_acc_subcta24    DECIMAL(16,6),
      monto_acc_subcta25    DECIMAL(16,6),
      monto_acc_subcta15    DECIMAL(16,6),
      monto_acc_subcta16    DECIMAL(16,6),
      monto_acc_subcta26    DECIMAL(16,6),
      monto_acc_subcta27    DECIMAL(16,6),
      monto_acc_subcta37    DECIMAL(16,6),
      monto_acc_subcta7     DECIMAL(16,6),
      monto_acc_subcta20    DECIMAL(16,6),
      monto_acc_subcta21    DECIMAL(16,6),
      monto_acc_subcta28    DECIMAL(16,6),
      monto_acc_subcta29    DECIMAL(16,6)
   END RECORD

   DEFINE 
      G_LISTA               CHAR(100),
      GUSER                 CHAR(08),
      permisos              CHAR(100),
      v_comando             CHAR(1000),
      vsp_transf_edad       CHAR(100),
      lc_operacion          CHAR(2),
      vfecha_corte          ,
      vfecha_ini            DATE,
      maxfecha              DATE, 
      marca                 SMALLINT,
      ls_tipo_solicitud     SMALLINT,
      lc_tipo_trab_ind      CHAR(2)
      
      
   LET G_LISTA = gc_ruta_listado CLIPPED,"/","CTAB047_",gd_hoy  USING "YYYYMMDD",".txt"

   LET gc_operacion = "00"
   
   LET reg_saldos_fusion.monto_en_acciones = 0
   LET reg_saldos_fusion.monto_en_pesos    = 0

   LET reg_repor.monto_subcta13      = 0
   LET reg_repor.monto_subcta30      = 0
   LET reg_repor.monto_subcta31      = 0
   LET reg_repor.monto_subcta32      = 0
   LET reg_repor.monto_subcta33      = 0
   LET reg_repor.monto_subcta34      = 0
   LET reg_repor.monto_subcta36      = 0
   LET reg_repor.monto_subcta11      = 0
   LET reg_repor.monto_subcta12      = 0
   LET reg_repor.monto_subcta40      = 0
   LET reg_repor.monto_subcta41      = 0
   LET reg_repor.monto_subcta1       = 0
   LET reg_repor.monto_subcta2       = 0
   LET reg_repor.monto_subcta5       = 0
   LET reg_repor.monto_subcta6       = 0
   LET reg_repor.monto_subcta9       = 0
   LET reg_repor.monto_subcta17      = 0
   LET reg_repor.monto_subcta18      = 0
   LET reg_repor.monto_subcta24      = 0
   LET reg_repor.monto_subcta25      = 0
   LET reg_repor.monto_subcta15      = 0
   LET reg_repor.monto_subcta16      = 0
   LET reg_repor.monto_subcta26      = 0
   LET reg_repor.monto_subcta27      = 0
   LET reg_repor.monto_subcta37      = 0
   LET reg_repor.monto_subcta7       = 0
   LET reg_repor.monto_subcta20      = 0
   LET reg_repor.monto_subcta21      = 0
   LET reg_repor.monto_subcta28      = 0
   LET reg_repor.monto_subcta29      = 0
   LET reg_repor.monto_acc_subcta13  = 0
   LET reg_repor.monto_acc_subcta30  = 0
   LET reg_repor.monto_acc_subcta31  = 0
   LET reg_repor.monto_acc_subcta32  = 0
   LET reg_repor.monto_acc_subcta33  = 0
   LET reg_repor.monto_acc_subcta34  = 0
   LET reg_repor.monto_acc_subcta36  = 0
   LET reg_repor.monto_acc_subcta11  = 0
   LET reg_repor.monto_acc_subcta12  = 0
   LET reg_repor.monto_acc_subcta40  = 0
   LET reg_repor.monto_acc_subcta41  = 0
   LET reg_repor.monto_acc_subcta1   = 0
   LET reg_repor.monto_acc_subcta2   = 0
   LET reg_repor.monto_acc_subcta5   = 0
   LET reg_repor.monto_acc_subcta6   = 0
   LET reg_repor.monto_acc_subcta9   = 0
   LET reg_repor.monto_acc_subcta17  = 0
   LET reg_repor.monto_acc_subcta18  = 0
   LET reg_repor.monto_acc_subcta24  = 0
   LET reg_repor.monto_acc_subcta25  = 0
   LET reg_repor.monto_acc_subcta15  = 0
   LET reg_repor.monto_acc_subcta16  = 0
   LET reg_repor.monto_acc_subcta26  = 0
   LET reg_repor.monto_acc_subcta27  = 0
   LET reg_repor.monto_acc_subcta37  = 0
   LET reg_repor.monto_acc_subcta7   = 0
   LET reg_repor.monto_acc_subcta20  = 0
   LET reg_repor.monto_acc_subcta21  = 0
   LET reg_repor.monto_acc_subcta28  = 0
   LET reg_repor.monto_acc_subcta29  = 0
 
   CASE gs_opcion 
   	  WHEN 5
   	  	 LET lc_operacion = "01"
   	  WHEN 8
   	  	 LET lc_operacion = "12"
   END CASE
   
   DATABASE safre_tmp
   
   START REPORT genera_reporte TO G_LISTA
   # -- Se buscan los NSS's para buscar el detalle de los saldos para cada uno de estos
   DECLARE cur_fus_sie CURSOR FOR
   SELECT fusion.nss, 
          fusion.curp, 
          " ", 
          fusion.siefore_actual, 
          fusion.siefore_cambio,
          "",
          fusion.tipo_solicitud,
          fusion.operacion 
   FROM   tmp_fusion_siefores fusion
   WHERE  operacion = lc_operacion

   FOREACH cur_fus_sie INTO reg_fusion_siefores.*,
   	                        ls_tipo_solicitud,
   	                        gc_operacion
   	
   	  #-- Inicializa variables 
   	  
   	  LET reg_saldos_fusion.monto_en_acciones = 0
   	  LET reg_saldos_fusion.monto_en_pesos = 0
      LET reg_repor.monto_acc_subcta13 = 0
      LET reg_repor.monto_subcta13     = 0                                                                   
      LET reg_repor.monto_acc_subcta30 = 0     
      LET reg_repor.monto_subcta30     = 0                                                                 
      LET reg_repor.monto_acc_subcta31 = 0     
      LET reg_repor.monto_subcta31     = 0                                                                   
      LET reg_repor.monto_acc_subcta32 = 0     
      LET reg_repor.monto_subcta32     = 0                                                                   
      LET reg_repor.monto_acc_subcta33 = 0     
      LET reg_repor.monto_subcta33     = 0                                                                
      LET reg_repor.monto_acc_subcta34 = 0    
      LET reg_repor.monto_subcta34     = 0                                                                    
      LET reg_repor.monto_acc_subcta36 = 0 
      LET reg_repor.monto_subcta36     = 0                                                                     
      LET reg_repor.monto_acc_subcta11 = 0     
      LET reg_repor.monto_subcta11     = 0                                                                  
      LET reg_repor.monto_acc_subcta12 = 0     
      LET reg_repor.monto_subcta12     = 0                                                                  
      LET reg_repor.monto_acc_subcta40 = 0    
      LET reg_repor.monto_subcta40     = 0                                                                     
      LET reg_repor.monto_acc_subcta41 = 0  
      LET reg_repor.monto_subcta41     = 0                                                                    
      LET reg_repor.monto_acc_subcta1  = 0     
      LET reg_repor.monto_subcta1      = 0                                                                  
      LET reg_repor.monto_acc_subcta2  = 0     
      LET reg_repor.monto_subcta2      = 0                                                                    
      LET reg_repor.monto_acc_subcta5  = 0     
      LET reg_repor.monto_subcta5      = 0                                                                  
      LET reg_repor.monto_acc_subcta6  = 0     
      LET reg_repor.monto_subcta6      = 0                                                                    
      LET reg_repor.monto_acc_subcta9  = 0     
      LET reg_repor.monto_subcta9      = 0                                                                 
      LET reg_repor.monto_acc_subcta17 = 0     
      LET reg_repor.monto_subcta17     = 0                                                                    
      LET reg_repor.monto_acc_subcta18 = 0    
      LET reg_repor.monto_subcta18     = 0                                                                   
      LET reg_repor.monto_acc_subcta24 = 0     
      LET reg_repor.monto_subcta24     = 0                                                            
      LET reg_repor.monto_acc_subcta25 = 0     
      LET reg_repor.monto_subcta25     = 0                                        
      LET reg_repor.monto_acc_subcta15 = 0     
      LET reg_repor.monto_subcta15     = 0                                                              
      LET reg_repor.monto_acc_subcta16 = 0     
      LET reg_repor.monto_subcta16     = 0                                                             
      LET reg_repor.monto_acc_subcta26 = 0     
      LET reg_repor.monto_subcta26     = 0                                                            
      LET reg_repor.monto_acc_subcta27 = 0     
      LET reg_repor.monto_subcta27     = 0                                                              
      LET reg_repor.monto_acc_subcta37 = 0     
      LET reg_repor.monto_subcta37     = 0                                                               
      LET reg_repor.monto_acc_subcta7  = 0     
      LET reg_repor.monto_subcta7      = 0                                                              
      LET reg_repor.monto_acc_subcta20 = 0     
      LET reg_repor.monto_subcta20     = 0                                                             
      LET reg_repor.monto_acc_subcta21 = 0     
      LET reg_repor.monto_subcta21     = 0                                                             
      LET reg_repor.monto_acc_subcta28 = 0     
      LET reg_repor.monto_subcta28     = 0                                                           
      LET reg_repor.monto_acc_subcta29 = 0     
      LET reg_repor.monto_subcta29     = 0
   	
   	  #-- Se asigna el tipo de trabajador
   	  
   	  LET reg_fusion_siefores.tipo_solicitud = "IMSS"
   	  
   	  IF ls_tipo_solicitud = 5 THEN
   	  	 LET reg_fusion_siefores.tipo_solicitud = "ASIGNADO"
   	  END IF
   	  
   	  IF ls_tipo_solicitud = 8 THEN
   	  	 SELECT tipo_trab_ind
   	  	 INTO   lc_tipo_trab_ind
   	  	 FROM   cta_ctr_reg_ind
   	  	 WHERE  nti = reg_fusion_siefores.nss
   	  	 
   	  	 IF lc_tipo_trab_ind = 1 THEN
   	  	 	  LET reg_fusion_siefores.tipo_solicitud = "IDENPENDIENTE"
   	  	 END IF

   	  	 IF lc_tipo_trab_ind = 2 THEN
   	  	 	  LET reg_fusion_siefores.tipo_solicitud = "ISSSTE"
   	  	 END IF
   	  	    	  	
   	  END IF
   	  
      # -- Llenando la información del registro a enviar al reporte 
      LET reg_repor.nss         = reg_fusion_siefores.nss
      LET reg_repor.curp        = reg_fusion_siefores.curp
      LET reg_repor.tipo_cta    = reg_fusion_siefores.tipo_solicitud
      LET reg_repor.siefore_ant = reg_fusion_siefores.siefore_actual
      LET reg_repor.siefore     = reg_fusion_siefores.siefore_cambio
      LET reg_repor.operacion   = gc_operacion
      
      # -- Se busca si el nss está marcado, de ser así, se obtine el código de la ultima marca registrada
      SELECT MAX(fecha_ini) 
      INTO   maxfecha
      FROM   safre_af:cta_act_marca
      WHERE  nss = reg_fusion_siefores.nss 
      
      SELECT marca_act.marca_cod, marca_tab.marca_desc
      INTO   marca, reg_repor.estado
      FROM   safre_af:cta_act_marca marca_act,
             safre_af:tab_marca marca_tab
      WHERE  marca_act.nss = reg_fusion_siefores.nss 
      AND    marca_act.fecha_ini = maxfecha
      AND    marca_act.marca_cod = marca_tab.marca_cod 
         
      # -- Se actualiza el campo estado con la marca obtenida
      UPDATE tmp_fusion_siefores SET estado = marca WHERE nss = reg_fusion_siefores.nss 
      
      IF marca > 0 THEN
         LET reg_repor.desc_estado = "CON MARCA"
      ELSE
         LET reg_repor.desc_estado = "SIN MARCA"
      END IF
      
      LET reg_repor.desc_estado = reg_repor.desc_estado CLIPPED
      
      # -- Se inicializa el registro a NULL
      INITIALIZE reg_saldos_fusion.* TO NULL
      
      # -- Para cada NSS, se busca los saldos de las subcuetas
      DECLARE cur_saldos CURSOR FOR
      SELECT saldos.subcuenta,
             saldos.monto_en_acciones,
             saldos.monto_en_pesos
      FROM  tmp_saldos_fusion saldos
      WHERE saldos.nss = reg_fusion_siefores.nss
            
      FOREACH cur_saldos INTO reg_saldos_fusion.*

         CASE reg_saldos_fusion.subcuenta
            WHEN 13                                                                                                                             
               LET reg_repor.monto_acc_subcta13 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta13 = reg_saldos_fusion.monto_en_pesos 
            WHEN 30                                                                    
               LET reg_repor.monto_acc_subcta30 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta30 = reg_saldos_fusion.monto_en_pesos 
            WHEN 31                                                                    
               LET reg_repor.monto_acc_subcta31 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta31 = reg_saldos_fusion.monto_en_pesos 
            WHEN 32                                                                    
               LET reg_repor.monto_acc_subcta32 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta32 = reg_saldos_fusion.monto_en_pesos 
            WHEN 33                                                                    
               LET reg_repor.monto_acc_subcta33 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta33 = reg_saldos_fusion.monto_en_pesos 
            WHEN 34                                                                    
               LET reg_repor.monto_acc_subcta34 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta34 = reg_saldos_fusion.monto_en_pesos 
            WHEN 36                                                                    
               LET reg_repor.monto_acc_subcta36 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta36 = reg_saldos_fusion.monto_en_pesos 
            WHEN 11                                                                    
               LET reg_repor.monto_acc_subcta11 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta11 = reg_saldos_fusion.monto_en_pesos 
            WHEN 12                                                                    
               LET reg_repor.monto_acc_subcta12 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta12 = reg_saldos_fusion.monto_en_pesos 
            WHEN 40                                                                    
               LET reg_repor.monto_acc_subcta40 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta40 = reg_saldos_fusion.monto_en_pesos 
            WHEN 41                                                                    
               LET reg_repor.monto_acc_subcta41 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta41 = reg_saldos_fusion.monto_en_pesos 
            WHEN 1                                                                     
               LET reg_repor.monto_acc_subcta1  = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta1  = reg_saldos_fusion.monto_en_pesos 
            WHEN 2                                                                     
               LET reg_repor.monto_acc_subcta2  = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta2  = reg_saldos_fusion.monto_en_pesos 
            WHEN 5                                                                     
               LET reg_repor.monto_acc_subcta5  = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta5  = reg_saldos_fusion.monto_en_pesos 
            WHEN 6                                                                     
               LET reg_repor.monto_acc_subcta6  = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta6  = reg_saldos_fusion.monto_en_pesos 
            WHEN 9                                                                     
               LET reg_repor.monto_acc_subcta9  = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta9  = reg_saldos_fusion.monto_en_pesos 
            WHEN 17                                                                    
               LET reg_repor.monto_acc_subcta17 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta17 = reg_saldos_fusion.monto_en_pesos 
            WHEN 18                                                                    
               LET reg_repor.monto_acc_subcta18 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta18 = reg_saldos_fusion.monto_en_pesos 
            WHEN 24                                                                    
               LET reg_repor.monto_acc_subcta24 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta24 = reg_saldos_fusion.monto_en_pesos 
            WHEN 25                                                                    
               LET reg_repor.monto_acc_subcta25 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta25 = reg_saldos_fusion.monto_en_pesos 
            WHEN 15                                                                    
               LET reg_repor.monto_acc_subcta15 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta15 = reg_saldos_fusion.monto_en_pesos 
            WHEN 16                                                                    
               LET reg_repor.monto_acc_subcta16 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta16 = reg_saldos_fusion.monto_en_pesos 
            WHEN 26                                                                    
               LET reg_repor.monto_acc_subcta26 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta26 = reg_saldos_fusion.monto_en_pesos 
            WHEN 27                                                                    
               LET reg_repor.monto_acc_subcta27 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta27 = reg_saldos_fusion.monto_en_pesos 
            WHEN 37                                                                    
               LET reg_repor.monto_acc_subcta37 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta37 = reg_saldos_fusion.monto_en_pesos 
            WHEN 7                                                                     
               LET reg_repor.monto_acc_subcta7  = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta7  = reg_saldos_fusion.monto_en_pesos 
            WHEN 20                                                                    
               LET reg_repor.monto_acc_subcta20 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta20 = reg_saldos_fusion.monto_en_pesos 
            WHEN 21                                                                    
               LET reg_repor.monto_acc_subcta21 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta21 = reg_saldos_fusion.monto_en_pesos 
            WHEN 28                                                                    
               LET reg_repor.monto_acc_subcta28 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta28 = reg_saldos_fusion.monto_en_pesos 
            WHEN 29                                                                    
               LET reg_repor.monto_acc_subcta29 = reg_saldos_fusion.monto_en_acciones     
               LET reg_repor.monto_subcta29 = reg_saldos_fusion.monto_en_pesos 
         END CASE
         
         LET reg_repor.fecha_calculo  = gd_hoy
         
      END FOREACH
      
      OUTPUT TO REPORT genera_reporte(reg_repor.*)
   END FOREACH

   FINISH REPORT genera_reporte
  
   LET permisos = " chmod 777 ",G_LISTA CLIPPED
   RUN permisos 

END FUNCTION


REPORT genera_reporte(reg_repor)
#----------------------------------

   DEFINE reg_repor RECORD
      fecha_calculo         DATE,
      nss                   CHAR(11),
      curp                  CHAR(18),
      tipo_cta              CHAR(33),
      siefore_ant           SMALLINT,
      siefore               SMALLINT,
      estado                CHAR(25),
      desc_estado           CHAR(150),
      operacion             CHAR(2),
      monto_subcta13        DECIMAL(16,6),
      monto_subcta30        DECIMAL(16,6),
      monto_subcta31        DECIMAL(16,6),
      monto_subcta32        DECIMAL(16,6),
      monto_subcta33        DECIMAL(16,6),
      monto_subcta34        DECIMAL(16,6),
      monto_subcta36        DECIMAL(16,6),
      monto_subcta11        DECIMAL(16,6),
      monto_subcta12        DECIMAL(16,6),
      monto_subcta40        DECIMAL(16,6),
      monto_subcta41        DECIMAL(16,6),
      monto_subcta1         DECIMAL(16,6),
      monto_subcta2         DECIMAL(16,6),
      monto_subcta5         DECIMAL(16,6),
      monto_subcta6         DECIMAL(16,6),
      monto_subcta9         DECIMAL(16,6),
      monto_subcta17        DECIMAL(16,6),
      monto_subcta18        DECIMAL(16,6),
      monto_subcta24        DECIMAL(16,6),
      monto_subcta25        DECIMAL(16,6),
      monto_subcta15        DECIMAL(16,6),
      monto_subcta16        DECIMAL(16,6),
      monto_subcta26        DECIMAL(16,6),
      monto_subcta27        DECIMAL(16,6),
      monto_subcta37        DECIMAL(16,6),
      monto_subcta7         DECIMAL(16,6),
      monto_subcta20        DECIMAL(16,6),
      monto_subcta21        DECIMAL(16,6),
      monto_subcta28        DECIMAL(16,6),
      monto_subcta29        DECIMAL(16,6),
      monto_acc_subcta13    DECIMAL(16,6), 
      monto_acc_subcta30    DECIMAL(16,6), 
      monto_acc_subcta31    DECIMAL(16,6), 
      monto_acc_subcta32    DECIMAL(16,6), 
      monto_acc_subcta33    DECIMAL(16,6), 
      monto_acc_subcta34    DECIMAL(16,6), 
      monto_acc_subcta36    DECIMAL(16,6), 
      monto_acc_subcta11    DECIMAL(16,6), 
      monto_acc_subcta12    DECIMAL(16,6), 
      monto_acc_subcta40    DECIMAL(16,6), 
      monto_acc_subcta41    DECIMAL(16,6), 
      monto_acc_subcta1     DECIMAL(16,6), 
      monto_acc_subcta2     DECIMAL(16,6), 
      monto_acc_subcta5     DECIMAL(16,6), 
      monto_acc_subcta6     DECIMAL(16,6), 
      monto_acc_subcta9     DECIMAL(16,6), 
      monto_acc_subcta17    DECIMAL(16,6), 
      monto_acc_subcta18    DECIMAL(16,6), 
      monto_acc_subcta24    DECIMAL(16,6), 
      monto_acc_subcta25    DECIMAL(16,6), 
      monto_acc_subcta15    DECIMAL(16,6), 
      monto_acc_subcta16    DECIMAL(16,6), 
      monto_acc_subcta26    DECIMAL(16,6), 
      monto_acc_subcta27    DECIMAL(16,6), 
      monto_acc_subcta37    DECIMAL(16,6), 
      monto_acc_subcta7     DECIMAL(16,6), 
      monto_acc_subcta20    DECIMAL(16,6), 
      monto_acc_subcta21    DECIMAL(16,6), 
      monto_acc_subcta28    DECIMAL(16,6), 
      monto_acc_subcta29    DECIMAL(16,6)
   END RECORD

   DEFINE
      voperacion            CHAR(2)

    OUTPUT
        PAGE LENGTH   45
        LEFT MARGIN    0
        RIGHT MARGIN   0
        TOP MARGIN     0
        BOTTOM MARGIN  0

    FORMAT

        FIRST PAGE HEADER

           PRINT
              COLUMN 001,"IDENTIFICACION DE CUENTAS A TRANSFERIRSE DE SB5 A SB4 "
           
           SKIP 2 LINES
              
           PRINT
              COLUMN 003,"FECHA",
              COLUMN 018,"NTI",
              COLUMN 032,"CURP",
              COLUMN 057,"TIPO DE",
              COLUMN 078,"SIEFORE",
              COLUMN 087,"SIEFORE",
              COLUMN 105,"AHORRO PARA EL RETIRO",
              COLUMN 137,"RETIRO ISSSTE 2008",
              COLUMN 170,"CV ISSSTE",
              COLUMN 200,"CS ISSSTE",
              COLUMN 229,"AHORRO SOLIDARIO PATRON",
              COLUMN 261,"AHORRO SOLIDARIO TRABAJADOR",
              COLUMN 296,"BONO DE PENSION",
              COLUMN 330,"ACR PATRONAL",
              COLUMN 359,"ACR VENTANILLA",
              COLUMN 387,"70% REMANENTE DE OPERACION",
              COLUMN 420,"20% REMANENTE DE OPERACION",
              COLUMN 458,"RETIRO IMSS",
              COLUMN 492,"CV IMSS",
              COLUMN 522,"CS IMSS",
              COLUMN 557,"ESTATAL",
              COLUMN 586,"CUOTA ESPECIAL",
              COLUMN 617,"ADICIONAL SUA",    
              COLUMN 647,"ADICIONAL VENTANILLA", 
              COLUMN 685,"ACR PATRON",  
              COLUMN 714,"ACR VENTANILLA",
              COLUMN 744,"AHORRO LARGO PLAZO SUA",
              COLUMN 771,"AHORRO LARGO PLAZO VENTANILLA",
              COLUMN 803,"AHORRO LARGO PLAZO VENTANILLA CON BF",
              COLUMN 843,"SAR IMSS CON BF",
              COLUMN 867,"PERSPECTIVA A LARGO PLAZO ART. 218",
              COLUMN 904,"PERSPECTIVA A LARGO PLAZO",
              COLUMN 935,"PERSPECTIVA A LARGO PLAZO PATRON",
              COLUMN 970,"PERSPECTIVA A LARGO PLAZO VENTANILLA",
              COLUMN 1009,"PERSPECTIVA A LARGO PLAZO PATRON CON BF",              
              COLUMN 1052,"PERSPECTIVA A LARGO PLAZO VENTANILLA CON BF",
              COLUMN 1100,"OPERACION",
              COLUMN 1112,"ESTATUS",
              COLUMN 1125,"PROCESO"
                                         
           PRINT
              COLUMN 002,"CALCULO",
              COLUMN 054,"ADMINISTRACION",
              COLUMN 078,"ACTUAL",
              COLUMN 087,"CAMBIO",
              COLUMN 100,"ACCIONES          PESOS",
              COLUMN 133,"ACCIONES          PESOS",
              COLUMN 165,"ACCIONES          PESOS",
              COLUMN 197,"ACCIONES          PESOS",
              COLUMN 229,"ACCIONES          PESOS",
              COLUMN 262,"ACCIONES          PESOS",
              COLUMN 293,"ACCIONES          PESOS",
              COLUMN 325,"ACCIONES          PESOS",
              COLUMN 357,"ACCIONES          PESOS",
              COLUMN 389,"ACCIONES          PESOS",
              COLUMN 421,"ACCIONES          PESOS",
              COLUMN 454,"ACCIONES          PESOS",
              COLUMN 485,"ACCIONES          PESOS",
              COLUMN 518,"ACCIONES          PESOS",
              COLUMN 549,"ACCIONES          PESOS",
              COLUMN 581,"ACCIONES          PESOS",
              COLUMN 613,"ACCIONES          PESOS",
              COLUMN 646,"ACCIONES          PESOS",
              COLUMN 677,"ACCIONES          PESOS",
              COLUMN 710,"ACCIONES          PESOS",
              COLUMN 741,"ACCIONES          PESOS",
              COLUMN 773,"ACCIONES          PESOS",
              COLUMN 805,"ACCIONES          PESOS",
              COLUMN 839,"ACCIONES          PESOS",
              COLUMN 870,"ACCIONES          PESOS",
              COLUMN 902,"ACCIONES          PESOS",
              COLUMN 936,"ACCIONES          PESOS",
              COLUMN 970,"ACCIONES          PESOS",
              COLUMN 1014,"ACCIONES          PESOS",
              COLUMN 1058,"ACCIONES          PESOS"      

               
           PRINT
              COLUMN 001,"------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------",
                         "------------------------------------------------------------"



        ON EVERY ROW

              PRINT
                 COLUMN 001,reg_repor.fecha_calculo USING "DD/MM/YYYY",
                 COLUMN 014,reg_repor.nss,
                 COLUMN 027,reg_repor.curp ,
                 COLUMN 047,reg_repor.tipo_cta,
                 COLUMN 081,reg_repor.siefore_ant USING "#",
                 COLUMN 090,reg_repor.siefore  USING "#",
                 COLUMN 093,reg_repor.monto_acc_subcta13 USING "#########.######", 
                 COLUMN 111,reg_repor.monto_subcta13     USING "#########.######",
                 COLUMN 123,reg_repor.monto_acc_subcta30 USING "#########.######", 
                 COLUMN 141,reg_repor.monto_subcta30     USING "#########.######",
                 COLUMN 152,reg_repor.monto_acc_subcta31 USING "#########.######", 
                 COLUMN 170,reg_repor.monto_subcta31     USING "#########.######",
                 COLUMN 181,reg_repor.monto_acc_subcta32 USING "#########.######", 
                 COLUMN 199,reg_repor.monto_subcta32     USING "#########.######",
                 COLUMN 212,reg_repor.monto_acc_subcta33 USING "#########.######", 
                 COLUMN 230,reg_repor.monto_subcta33     USING "#########.######",
                 COLUMN 246,reg_repor.monto_acc_subcta34 USING "#########.######", 
                 COLUMN 264,reg_repor.monto_subcta34     USING "#########.######",
                 COLUMN 281,reg_repor.monto_acc_subcta36 USING "#########.######", 
                 COLUMN 299,reg_repor.monto_subcta36     USING "#########.######",
                 COLUMN 313,reg_repor.monto_acc_subcta11 USING "#########.######", 
                 COLUMN 331,reg_repor.monto_subcta11     USING "#########.######",
                 COLUMN 343,reg_repor.monto_acc_subcta12 USING "#########.######", 
                 COLUMN 361,reg_repor.monto_subcta12     USING "#########.######",
                 COLUMN 374,reg_repor.monto_acc_subcta40 USING "#########.######", 
                 COLUMN 392,reg_repor.monto_subcta40     USING "#########.######",
                 COLUMN 409,reg_repor.monto_acc_subcta41 USING "#########.######", 
                 COLUMN 427,reg_repor.monto_subcta41     USING "#########.######",
                 COLUMN 441,reg_repor.monto_acc_subcta1  USING "#########.######", 
                 COLUMN 459,reg_repor.monto_subcta1      USING "#########.######", 
                 COLUMN 470,reg_repor.monto_acc_subcta2  USING "#########.######", 
                 COLUMN 488,reg_repor.monto_subcta2      USING "#########.######", 
                 COLUMN 501,reg_repor.monto_acc_subcta5  USING "#########.######", 
                 COLUMN 519,reg_repor.monto_subcta5      USING "#########.######",
                 COLUMN 533,reg_repor.monto_acc_subcta6  USING "#########.######", 
                 COLUMN 551,reg_repor.monto_subcta6      USING "#########.######",
                 COLUMN 565,reg_repor.monto_acc_subcta9  USING "#########.######", 
                 COLUMN 583,reg_repor.monto_subcta9      USING "#########.######",
                 COLUMN 595,reg_repor.monto_acc_subcta17 USING "#########.######", 
                 COLUMN 613,reg_repor.monto_subcta17     USING "#########.######",
                 COLUMN 627,reg_repor.monto_acc_subcta18 USING "#########.######", 
                 COLUMN 645,reg_repor.monto_subcta18     USING "#########.######",
                 COLUMN 658,reg_repor.monto_acc_subcta24 USING "#########.######", 
                 COLUMN 676,reg_repor.monto_subcta24     USING "#########.######",
                 COLUMN 689,reg_repor.monto_acc_subcta25 USING "#########.######", 
                 COLUMN 707,reg_repor.monto_subcta25     USING "#########.######",
                 COLUMN 720,reg_repor.monto_acc_subcta15 USING "#########.######", 
                 COLUMN 738,reg_repor.monto_subcta15     USING "#########.######",
                 COLUMN 753,reg_repor.monto_acc_subcta16 USING "#########.######", 
                 COLUMN 771,reg_repor.monto_subcta16     USING "#########.######",
                 COLUMN 793,reg_repor.monto_acc_subcta26 USING "#########.######", 
                 COLUMN 811,reg_repor.monto_subcta26     USING "#########.######",
                 COLUMN 827,reg_repor.monto_acc_subcta27 USING "#########.######", 
                 COLUMN 845,reg_repor.monto_subcta27     USING "#########.######",
                 COLUMN 860,reg_repor.monto_acc_subcta37 USING "#########.######", 
                 COLUMN 878,reg_repor.monto_subcta37     USING "#########.######",
                 COLUMN 895,reg_repor.monto_acc_subcta7  USING "#########.######", 
                 COLUMN 913,reg_repor.monto_subcta7      USING "#########.######",
                 COLUMN 928,reg_repor.monto_acc_subcta20 USING "#########.######", 
                 COLUMN 946,reg_repor.monto_subcta20     USING "#########.######",
                 COLUMN 964,reg_repor.monto_acc_subcta21 USING "#########.######", 
                 COLUMN 982,reg_repor.monto_subcta21     USING "#########.######",
                 COLUMN 1002,reg_repor.monto_acc_subcta28 USING "#########.######", 
                 COLUMN 1028,reg_repor.monto_subcta28     USING "#########.######",
                 COLUMN 1046,reg_repor.monto_acc_subcta29 USING "#########.######", 
                 COLUMN 1064,reg_repor.monto_subcta29     USING "#########.######",        
                 COLUMN 1103,reg_repor.operacion,         
                 COLUMN 1112,reg_repor.desc_estado CLIPPED,
                 COLUMN 1125,reg_repor.estado CLIPPED

END REPORT


FUNCTION fn_cambio_regimen()
#---------------------------

   DEFINE lc_nss           CHAR(11),
          ejecuta          CHAR(300)
   
   DEFINE ls_grupo         ,
          ls_marca_cod     ,
          ls_subcta        SMALLINT
   
   DEFINE
	      vcodigo_rechazo   , 
	      vmarca_estado     SMALLINT
	
	DEFINE li_consec        INTEGER
	
	DEFINE lb_saldo         ,
	       xcodigo_marca    ,
		    xcodigo_rechazo  SMALLINT
   
   # -- Se obtienen los nss que fueron identificados en la tabla de la fusión
   DATABASE safre_af
   
   LET li_consec = 0
   
   DECLARE cur_nss_fusion CURSOR FOR
      SELECT nss FROM safre_tmp:tmp_fusion_siefores
   
   FOREACH cur_nss_fusion INTO lc_nss
      
      # -- Buscando el grupo del régimen para cada nss encontrado y así obtener el código de marca
      DECLARE cur_reg_marca CURSOR FOR
      SELECT regimen.grupo_regimen,  
             tab_regimen.marca_cod
      FROM   cta_nss_regimen regimen,
             tab_grupo_regimen tab_regimen
      WHERE  regimen.nss = lc_nss
      AND    regimen.codigo_siefore = 5
      AND    tab_regimen.grupo_regimen = regimen.grupo_regimen
            
         FOREACH cur_reg_marca INTO ls_grupo, ls_marca_cod
            LET li_consec = li_consec + 1
            LET lb_saldo = FALSE
            
            # -- Se marcará la cuenta con cada marca de cada grupo_regimen obtenido
            SELECT "X"
            FROM   cta_act_marca
            WHERE  nss = lc_nss
            AND    marca_cod = ls_marca_cod
         
            IF SQLCA.SQLCODE <> 0 THEN
         
                LET vmarca_estado   = 0
                LET vcodigo_rechazo = 0
         
                LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
                              "'",lc_nss,"'",
                              ",",ls_marca_cod,
                              ",",li_consec,
                              ",",vmarca_estado,
                              ",",vcodigo_rechazo,
                              ",",0,
                              ",","'","'",",",
                              "'",gc_usuario,"'",")"
         
                LET ejecuta = ejecuta CLIPPED
         
                PREPARE clausula_spl FROM ejecuta
                DECLARE cursor_marca CURSOR FOR clausula_spl
         
                OPEN cursor_marca
         
                FETCH cursor_marca INTO xcodigo_marca, xcodigo_rechazo
         
                CLOSE cursor_marca
         
            END IF

            # -- Se buscan las subcuentas asociadas al grupo
            DECLARE cur_subcta CURSOR FOR
               SELECT subcuenta
               FROM   tab_agrupa_subcta_regimen
               WHERE  grupo_regimen = ls_grupo
            
            FOREACH cur_subcta INTO ls_subcta            	
               # -- Se actualiza el código de la siefore a 4
               UPDATE cta_regimen
               SET    codigo_siefore = 4
               WHERE  nss = lc_nss
               AND    subcuenta = ls_subcta 
            END FOREACH

            # -- En el caso de que el marcaje haya sido correcto, se realiza el cambio de régimen para el NSS
            UPDATE cta_nss_regimen
            SET    codigo_siefore = 4
            WHERE  nss = lc_nss
            AND    grupo_regimen = ls_grupo                                      
              
         END FOREACH
         
         UPDATE safre_tmp:tmp_fusion_siefores
         SET    estado = 1 #--CAMBIO DE REGIMEN --#
         WHERE  nss = lc_nss
   END FOREACH
END FUNCTION


FUNCTION fn_genera_reporte_cambio_regimen()
# ----------------------------------------
   DEFINE lc_nss        CHAR(11)
   
   DEFINE lr_consulta RECORD 
      nss               CHAR(11) ,
      subcuenta         SMALLINT , 
      codigo_siefore    SMALLINT  
   END RECORD
   
   DEFINE   reg_reporte RECORD
      nss               CHAR(11) ,
      siefore_subcta1   SMALLINT, 
      siefore_subcta2   SMALLINT,
      siefore_subcta3   SMALLINT,
      siefore_subcta5   SMALLINT,
      siefore_subcta6   SMALLINT,
      siefore_subcta7   SMALLINT,
      siefore_subcta9   SMALLINT,
      siefore_subcta10  SMALLINT,
      siefore_subcta11  SMALLINT,
      siefore_subcta12  SMALLINT,
      siefore_subcta13  SMALLINT,
      siefore_subcta15  SMALLINT,
      siefore_subcta16  SMALLINT,
      siefore_subcta17  SMALLINT,
      siefore_subcta18  SMALLINT,
      siefore_subcta19  SMALLINT,
      siefore_subcta20  SMALLINT,
      siefore_subcta21  SMALLINT,
      siefore_subcta22  SMALLINT,
      siefore_subcta23  SMALLINT,
      siefore_subcta24  SMALLINT,
      siefore_subcta25  SMALLINT,
      siefore_subcta26  SMALLINT,
      siefore_subcta27  SMALLINT,
      siefore_subcta28  SMALLINT,
      siefore_subcta29  SMALLINT,
      siefore_subcta30  SMALLINT,
      siefore_subcta31  SMALLINT,
      siefore_subcta32  SMALLINT,
      siefore_subcta33  SMALLINT,
      siefore_subcta34  SMALLINT,
      siefore_subcta36  SMALLINT,
      siefore_subcta37  SMALLINT,
      siefore_subcta39  SMALLINT,
      siefore_subcta40  SMALLINT,
      siefore_subcta41  SMALLINT
   END RECORD
   
   DEFINE 
      vruta_rescate     CHAR(40) ,
      G_LISTA           CHAR(100),
      GUSER             CHAR(08) ,
      permisos          CHAR(100),
      HOY               DATE
   
   LET HOY = TODAY
   
    
   SELECT ruta_rescate, USER
   INTO   vruta_rescate, GUSER
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"

   LET G_LISTA = gc_ruta_listado CLIPPED,"/","CTAB047_REG_",HOY  USING "YYYYMMDD",".txt"
   
   START REPORT genera_archivo_cambio_regimen TO G_LISTA
   
   # -- Se obtienen todos los nss de la tabla de fuisión
   DECLARE cur_nss_regimen_fus CURSOR FOR
      SELECT DISTINCT
             fusion.nss
      FROM   safre_tmp:tmp_fusion_siefores fusion
      ORDER BY 1
      
   FOREACH cur_nss_regimen_fus INTO lc_nss
      # -- Se busca la siefore por subcuenta para ese nss
      LET reg_reporte.nss = lc_nss
      
      DECLARE cur_regimen_fus CURSOR FOR
         SELECT DISTINCT
                regimen.nss, 
                regimen.subcuenta, 
                regimen.codigo_siefore
         FROM   cta_regimen regimen,
                safre_tmp:tmp_fusion_siefores fusion
         WHERE  regimen.nss = fusion.nss
         AND    regimen.nss = lc_nss
         ORDER BY 1,2
      
      FOREACH cur_regimen_fus INTO lr_consulta.*
         # -- Se arma el registro que se enviará al reporte         
         CASE lr_consulta.subcuenta
            WHEN 1             
               LET reg_reporte.siefore_subcta1  = lr_consulta.codigo_siefore
            WHEN 2                          
               LET reg_reporte.siefore_subcta2  = lr_consulta.codigo_siefore
            WHEN 3                    
               LET reg_reporte.siefore_subcta3  = lr_consulta.codigo_siefore
            WHEN 5                          
               LET reg_reporte.siefore_subcta5  = lr_consulta.codigo_siefore
            WHEN 6                       
               LET reg_reporte.siefore_subcta6  = lr_consulta.codigo_siefore
            WHEN 7                          
               LET reg_reporte.siefore_subcta7  = lr_consulta.codigo_siefore
            WHEN 9                        
               LET reg_reporte.siefore_subcta9  = lr_consulta.codigo_siefore
            WHEN 10                         
               LET reg_reporte.siefore_subcta10 = lr_consulta.codigo_siefore
            WHEN 11                      
               LET reg_reporte.siefore_subcta11 = lr_consulta.codigo_siefore
            WHEN 12                         
               LET reg_reporte.siefore_subcta12 = lr_consulta.codigo_siefore
            WHEN 13                       
               LET reg_reporte.siefore_subcta13 = lr_consulta.codigo_siefore
            WHEN 15                        
               LET reg_reporte.siefore_subcta15 = lr_consulta.codigo_siefore
            WHEN 16                      
               LET reg_reporte.siefore_subcta16 = lr_consulta.codigo_siefore
            WHEN 17                       
               LET reg_reporte.siefore_subcta17 = lr_consulta.codigo_siefore
            WHEN 18                        
               LET reg_reporte.siefore_subcta18 = lr_consulta.codigo_siefore
            WHEN 19                         
               LET reg_reporte.siefore_subcta19 = lr_consulta.codigo_siefore
            WHEN 20                        
               LET reg_reporte.siefore_subcta20 = lr_consulta.codigo_siefore
            WHEN 21                      
               LET reg_reporte.siefore_subcta21 = lr_consulta.codigo_siefore
            WHEN 22                         
               LET reg_reporte.siefore_subcta22 = lr_consulta.codigo_siefore
            WHEN 23                       
               LET reg_reporte.siefore_subcta23 = lr_consulta.codigo_siefore
            WHEN 24                        
               LET reg_reporte.siefore_subcta24 = lr_consulta.codigo_siefore
            WHEN 25                        
               LET reg_reporte.siefore_subcta25 = lr_consulta.codigo_siefore
            WHEN 26                        
               LET reg_reporte.siefore_subcta26 = lr_consulta.codigo_siefore
            WHEN 27                         
               LET reg_reporte.siefore_subcta27 = lr_consulta.codigo_siefore
            WHEN 28                        
               LET reg_reporte.siefore_subcta28 = lr_consulta.codigo_siefore
            WHEN 29                         
               LET reg_reporte.siefore_subcta29 = lr_consulta.codigo_siefore
            WHEN 30                        
               LET reg_reporte.siefore_subcta30 = lr_consulta.codigo_siefore
            WHEN 31                       
               LET reg_reporte.siefore_subcta31 = lr_consulta.codigo_siefore
            WHEN 32                        
               LET reg_reporte.siefore_subcta32 = lr_consulta.codigo_siefore
            WHEN 33                      
               LET reg_reporte.siefore_subcta33 = lr_consulta.codigo_siefore
            WHEN 34                        
               LET reg_reporte.siefore_subcta34 = lr_consulta.codigo_siefore
            WHEN 36                        
               LET reg_reporte.siefore_subcta36 = lr_consulta.codigo_siefore
            WHEN 37                        
               LET reg_reporte.siefore_subcta37 = lr_consulta.codigo_siefore
            WHEN 39                       
               LET reg_reporte.siefore_subcta39 = lr_consulta.codigo_siefore
            WHEN 40                        
               LET reg_reporte.siefore_subcta40 = lr_consulta.codigo_siefore
            WHEN 41                         
               LET reg_reporte.siefore_subcta41 = lr_consulta.codigo_siefore
         END CASE 
      END FOREACH
      OUTPUT TO REPORT genera_archivo_cambio_regimen(reg_reporte.*)
   END FOREACH

   FINISH REPORT genera_archivo_cambio_regimen
  
   LET permisos = " chmod 777 ",G_LISTA CLIPPED
   RUN permisos 
   
   DISPLAY "REPORTE GENERADO: ", G_LISTA
END FUNCTION


REPORT genera_archivo_cambio_regimen(reg_repor)
#--------------------------------

   DEFINE reg_repor RECORD
      nss               CHAR(11) ,
      siefore_subcta1   SMALLINT, 
      siefore_subcta2   SMALLINT,
      siefore_subcta3   SMALLINT,
      siefore_subcta5   SMALLINT,
      siefore_subcta6   SMALLINT,
      siefore_subcta7   SMALLINT,
      siefore_subcta9   SMALLINT,
      siefore_subcta10  SMALLINT,
      siefore_subcta11  SMALLINT,
      siefore_subcta12  SMALLINT,
      siefore_subcta13  SMALLINT,
      siefore_subcta15  SMALLINT,
      siefore_subcta16  SMALLINT,
      siefore_subcta17  SMALLINT,
      siefore_subcta18  SMALLINT,
      siefore_subcta19  SMALLINT,
      siefore_subcta20  SMALLINT,
      siefore_subcta21  SMALLINT,
      siefore_subcta22  SMALLINT,
      siefore_subcta23  SMALLINT,
      siefore_subcta24  SMALLINT,
      siefore_subcta25  SMALLINT,
      siefore_subcta26  SMALLINT,
      siefore_subcta27  SMALLINT,
      siefore_subcta28  SMALLINT,
      siefore_subcta29  SMALLINT,
      siefore_subcta30  SMALLINT,
      siefore_subcta31  SMALLINT,
      siefore_subcta32  SMALLINT,
      siefore_subcta33  SMALLINT,
      siefore_subcta34  SMALLINT,
      siefore_subcta36  SMALLINT,
      siefore_subcta37  SMALLINT,
      siefore_subcta39  SMALLINT,
      siefore_subcta40  SMALLINT,
      siefore_subcta41  SMALLINT
   END RECORD


    OUTPUT
        PAGE LENGTH   45
        LEFT MARGIN    0
        RIGHT MARGIN   0
        TOP MARGIN     0
        BOTTOM MARGIN  0

    FORMAT
        FIRST PAGE HEADER
           PRINT 
              COLUMN 001, "CAMBIO DE REGIMEN DE FUSIÓN DE SIEFORE BÁSICA 5 A SIEFORE BÁSICA 4"

           SKIP 2 LINES
           
           PRINT
              COLUMN 005,"NSS",
              COLUMN 018,"SIEFORE",
              COLUMN 029,"SIEFORE",
              COLUMN 040,"SIEFORE",
              COLUMN 051,"SIEFORE",
              COLUMN 062,"SIEFORE",
              COLUMN 073,"SIEFORE",
              COLUMN 084,"SIEFORE",
              COLUMN 095,"SIEFORE",
              COLUMN 106,"SIEFORE",
              COLUMN 117,"SIEFORE",
              COLUMN 128,"SIEFORE",
              COLUMN 139,"SIEFORE",
              COLUMN 150,"SIEFORE",
              COLUMN 161,"SIEFORE",
              COLUMN 172,"SIEFORE",
              COLUMN 183,"SIEFORE",
              COLUMN 194,"SIEFORE",
              COLUMN 205,"SIEFORE",
              COLUMN 216,"SIEFORE",
              COLUMN 227,"SIEFORE",
              COLUMN 238,"SIEFORE",
              COLUMN 249,"SIEFORE",
              COLUMN 260,"SIEFORE",
              COLUMN 271,"SIEFORE",
              COLUMN 282,"SIEFORE",
              COLUMN 293,"SIEFORE",
              COLUMN 304,"SIEFORE",
              COLUMN 315,"SIEFORE",
              COLUMN 326,"SIEFORE",
              COLUMN 337,"SIEFORE",
              COLUMN 348,"SIEFORE",
              COLUMN 359,"SIEFORE",
              COLUMN 370,"SIEFORE",
              COLUMN 381,"SIEFORE",
              COLUMN 392,"SIEFORE",
              COLUMN 403,"SIEFORE"
                                       
           PRINT
              COLUMN 018,"SUBCTA 1",
              COLUMN 029,"SUBCTA 2",
              COLUMN 040,"SUBCTA 3",
              COLUMN 051,"SUBCTA 5",
              COLUMN 062,"SUBCTA 6",
              COLUMN 073,"SUBCTA 7",
              COLUMN 084,"SUBCTA 9",
              COLUMN 095,"SUBCTA 10",
              COLUMN 106,"SUBCTA 11",
              COLUMN 117,"SUBCTA 12",
              COLUMN 128,"SUBCTA 13",
              COLUMN 139,"SUBCTA 15",
              COLUMN 150,"SUBCTA 16",
              COLUMN 161,"SUBCTA 17",
              COLUMN 172,"SUBCTA 18",
              COLUMN 183,"SUBCTA 19",
              COLUMN 194,"SUBCTA 20",
              COLUMN 205,"SUBCTA 21",
              COLUMN 216,"SUBCTA 22",
              COLUMN 227,"SUBCTA 23",
              COLUMN 238,"SUBCTA 24",
              COLUMN 249,"SUBCTA 25",
              COLUMN 260,"SUBCTA 26",
              COLUMN 271,"SUBCTA 27",
              COLUMN 282,"SUBCTA 28",
              COLUMN 293,"SUBCTA 29",
              COLUMN 304,"SUBCTA 30",
              COLUMN 315,"SUBCTA 31",
              COLUMN 326,"SUBCTA 32",
              COLUMN 337,"SUBCTA 33",
              COLUMN 348,"SUBCTA 34",
              COLUMN 359,"SUBCTA 36",
              COLUMN 370,"SUBCTA 37",
              COLUMN 381,"SUBCTA 39",
              COLUMN 392,"SUBCTA 40",
              COLUMN 403,"SUBCTA 41"
                    
           PRINT
              COLUMN 001,"-------------------------------------------------------------------------------",
                         "-------------------------------------------------------------------------------",
                         "-------------------------------------------------------------------------------",
                         "-------------------------------------------------------------------------------",
                         "-------------------------------------------------------------------------------",
                         "------------------"

        ON EVERY ROW

              PRINT
                 COLUMN 002,reg_repor.nss CLIPPED    ,
                 COLUMN 021,reg_repor.siefore_subcta1, 
                 COLUMN 032,reg_repor.siefore_subcta2, 
                 COLUMN 043,reg_repor.siefore_subcta3, 
                 COLUMN 054,reg_repor.siefore_subcta5, 
                 COLUMN 065,reg_repor.siefore_subcta6, 
                 COLUMN 076,reg_repor.siefore_subcta7, 
                 COLUMN 087,reg_repor.siefore_subcta9, 
                 COLUMN 098,reg_repor.siefore_subcta10,
                 COLUMN 109,reg_repor.siefore_subcta11,
                 COLUMN 120,reg_repor.siefore_subcta12,
                 COLUMN 131,reg_repor.siefore_subcta13,
                 COLUMN 142,reg_repor.siefore_subcta15,
                 COLUMN 153,reg_repor.siefore_subcta16,
                 COLUMN 164,reg_repor.siefore_subcta17,
                 COLUMN 175,reg_repor.siefore_subcta18,
                 COLUMN 186,reg_repor.siefore_subcta19,
                 COLUMN 197,reg_repor.siefore_subcta20,
                 COLUMN 208,reg_repor.siefore_subcta21,
                 COLUMN 219,reg_repor.siefore_subcta22,
                 COLUMN 230,reg_repor.siefore_subcta23,
                 COLUMN 241,reg_repor.siefore_subcta24,
                 COLUMN 252,reg_repor.siefore_subcta25,
                 COLUMN 263,reg_repor.siefore_subcta26,
                 COLUMN 274,reg_repor.siefore_subcta27,
                 COLUMN 285,reg_repor.siefore_subcta28,
                 COLUMN 296,reg_repor.siefore_subcta29,
                 COLUMN 307,reg_repor.siefore_subcta30,
                 COLUMN 318,reg_repor.siefore_subcta31,
                 COLUMN 329,reg_repor.siefore_subcta32,
                 COLUMN 340,reg_repor.siefore_subcta33,
                 COLUMN 351,reg_repor.siefore_subcta34,
                 COLUMN 362,reg_repor.siefore_subcta36,
                 COLUMN 373,reg_repor.siefore_subcta37,
                 COLUMN 384,reg_repor.siefore_subcta39,
                 COLUMN 395,reg_repor.siefore_subcta40,
                 COLUMN 406,reg_repor.siefore_subcta41      

END REPORT


FUNCTION fn_liquida()
#--------------------

   DEFINE lc_nss           CHAR(300),
          v_desmarca       CHAR(100)
   
   DEFINE ld_acciones      ,
          ld_pesos         DECIMAL(16,6)
   
   DEFINE ld_precio_SB4    ,
          ld_precio_SB5    DECIMAL(19,14) 
          
   DEFINE ls_subcta        ,
          ls_marca_cod     ,
          pmarca_causa     ,
          pestado_marca    SMALLINT
   
   DEFINE li_consecutivo   ,
          vcorrelativo     INTEGER
   
   # -- Obteniendo los precios de accion para la SIEFORE Basica 4
   SELECT precio_del_dia
   INTO ld_precio_SB4
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = gd_fecha_liquida
   AND    codigo_siefore = 4
   
   # -- Obteniendo los precios de accion para la SIEFORE Basica 5
   SELECT precio_del_dia
   INTO ld_precio_SB5
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = gd_fecha_liquida
   AND    codigo_siefore = 5
   
   # -- Obteniendo un folio de glo_folio
   SELECT MAX(folio)+1
   INTO gi_folio
   FROM glo_folio
   
   INSERT INTO glo_folio
   VALUES(gi_folio)
   
   LET li_consecutivo = 0
   
   LET pmarca_causa = 0
   LET pestado_marca = 0
   
   # -- Se obtienen los nss de la fusión
   DECLARE cur_nss_estado CURSOR FOR
      SELECT nss 
      FROM safre_tmp:tmp_fusion_siefores
      WHERE estado = 1
    
   FOREACH cur_nss_estado INTO lc_nss
      # -- Para cada nss, se obtiene las subcuentas con saldo de safre_tmp:tmp_saldos_fusion
      DECLARE cur_subctas CURSOR FOR
         SELECT subcuenta
         FROM safre_tmp:tmp_saldos_fusion
         WHERE nss = lc_nss
      
      FOREACH cur_subctas INTO ls_subcta
         # -- Calculando el saldo para la subcta
         SELECT NVL(SUM(monto_en_acciones),0)
         INTO   ld_acciones
         FROM   dis_cuenta
         WHERE  nss = lc_nss
         AND    subcuenta = ls_subcta
         AND    siefore = 5
         
         # -- En caso de que cuente con saldo, se da de alta el movimiento 925 y 926 en dis_cuenta
         IF ld_acciones > 0 THEN
            LET li_consecutivo = li_consecutivo + 1
            
            # -- Calculando el monto en pesos para SIEFORE Basica 5
            LET ld_pesos    = ld_acciones * ld_precio_SB5
            LET ld_pesos    = ld_pesos * -1
            LET ld_acciones = ld_acciones * -1
            
            INSERT INTO dis_cuenta  VALUES
                   (925,                       #tipo_movimiento
                   ls_subcta,                  #subcuenta
                   5,                          #siefore
                   gi_folio,                   #folio
                   li_consecutivo,             #consecutivo_lote
                   lc_nss,                     #nss
                   "",                         #curp
                   "",                         #folio_sua
                   gd_fecha_liquida,           #fecha_pago
                   gd_fecha_liquida,           #fecha_valor
                   gd_fecha_liquida,           #fecha_conversion
                   ld_pesos,                   #monto_en_pesos
                   ld_acciones,                #monto_en_acciones
                   ld_precio_SB5,              #precio_accion
                   0,                          #dias_cotizados
                   "",                         #sucursal
                   "FUSIONSB5",                #id_aportante
                   6,                          #estado
                   gd_hoy,                     #fecha_proceso
                   gc_usuario,                 #usuario
                   gd_hoy,                     #fecha_archivo
                   1)                          #etiqueta
                   
            # -- Calculando el monto en pesos para SIEFORE Basica 4
            LET ld_pesos    = ld_pesos * -1
            LET ld_acciones = ld_pesos / ld_precio_SB4
            
            INSERT INTO dis_cuenta  VALUES
                   (926,                       #tipo_movimiento
                   ls_subcta,                  #subcuenta
                   4,                          #siefore
                   gi_folio,                   #folio
                   li_consecutivo,             #consecutivo_lote
                   lc_nss,                     #nss
                   "",                         #curp
                   "",                         #folio_sua
                   gd_fecha_liquida,           #fecha_pago
                   gd_fecha_liquida,           #fecha_valor
                   gd_fecha_liquida,           #fecha_conversion
                   ld_pesos,                   #monto_en_pesos
                   ld_acciones,                #monto_en_acciones
                   ld_precio_SB4,              #precio_accion
                   0,                          #dias_cotizados
                   "",                         #sucursal
                   "FUSIONSB5",                #id_aportante
                   6,                          #estado
                   gd_hoy,                     #fecha_proceso
                   gc_usuario,                 #usuario
                   gd_hoy,                     #fecha_archivo
                   1)                          #etiqueta
                   
            # -- Se desmarca la cuenta para las marcas del proceso
            DECLARE cur_marcas CURSOR FOR
               SELECT marca_cod
               FROM   tab_marca
               WHERE  marca_cod BETWEEN 300 AND 315
            
            FOREACH cur_marcas INTO ls_marca_cod
               
               SELECT c.correlativo
               INTO   vcorrelativo
               FROM   cta_act_marca c
               WHERE  c.nss = lc_nss 
               AND    c.marca_cod = ls_marca_cod
               
               IF SQLCA.SQLCODE = 0 THEN
                  LET v_desmarca    = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"

display "v_desmarca :",v_desmarca
display "lc_nss :",lc_nss
display "ls_marca_cod :",ls_marca_cod
display "vcorrelativo :",vcorrelativo
display "pestado_marca :",pestado_marca
display "pmarca_causa :",pmarca_causa
display "gc_usuario :",gc_usuario
         
                  PREPARE eje_desmarca FROM v_desmarca
            
                  EXECUTE eje_desmarca
                  USING lc_nss,
                        ls_marca_cod,
                        vcorrelativo,
                        pestado_marca,
                        pmarca_causa,
                        gc_usuario
               END IF
            END FOREACH
            
            UPDATE safre_tmp:tmp_fusion_siefores
            SET    estado = 2  #-- LIQUIDADO --#
            WHERE  nss = lc_nss
            AND    estado = 1  #-- CAMBIO DE REGIMEN --#
            
         END IF
      END FOREACH
   END FOREACH
END FUNCTION


FUNCTION fn_genera_reporte_liquidacion(li_folio)
# ----------------------------------------------

   DEFINE   reg_reporte RECORD
      nss                  CHAR(11)     ,
      tipo_movimiento      SMALLINT     ,
      siefore              SMALLINT     ,
      subcuenta            SMALLINT     , 
      monto_en_pesos       DECIMAL(22,6),
      monto_en_acciones    DECIMAL(22,6) 
   END RECORD
   
   DEFINE 
      vruta_rescate     CHAR(40) ,
      G_LISTA           CHAR(100),
      GUSER             CHAR(08) ,
      permisos          CHAR(100),
      HOY               DATE
   
   DEFINE 
      li_folio          INTEGER
   
   LET HOY = TODAY
   
    
   SELECT ruta_rescate, USER
   INTO   vruta_rescate, GUSER
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"

   LET G_LISTA = gc_ruta_listado CLIPPED,"/","CTAB047_LIQ_",HOY  USING "YYYYMMDD",".txt"
   
   START REPORT genera_archivo_liquidacion TO G_LISTA
   
   DECLARE cur_liquida_rpt CURSOR FOR
      SELECT nss, tipo_movimiento, siefore, subcuenta, 
             monto_en_pesos, monto_en_acciones
      FROM   dis_cuenta
      WHERE  folio = li_folio
      ORDER  by 1,4,2


   FOREACH cur_liquida_rpt INTO reg_reporte.*
   	
      OUTPUT TO REPORT genera_archivo_liquidacion(li_folio, reg_reporte.*)
   END FOREACH

   FINISH REPORT genera_archivo_liquidacion
  
   LET permisos = " chmod 777 ",G_LISTA CLIPPED
   RUN permisos 
   
   DISPLAY "REPORTE GENERADO: ", G_LISTA
END FUNCTION


REPORT genera_archivo_liquidacion(li_folio, reg_repor)
#-----------------------------------------------------

   DEFINE reg_repor RECORD
      nss                  CHAR(11)     ,
      tipo_movimiento      SMALLINT     ,
      siefore              SMALLINT     ,
      subcuenta            SMALLINT     , 
      monto_en_pesos       DECIMAL(22,6),
      monto_en_acciones    DECIMAL(22,6) 
   END RECORD
   
   DEFINE li_folio         INTEGER


    OUTPUT
        PAGE LENGTH   45
        LEFT MARGIN    0
        RIGHT MARGIN   0
        TOP MARGIN     0
        BOTTOM MARGIN  0

    FORMAT
        FIRST PAGE HEADER
           PRINT 
              COLUMN 013, "LIQUIDACIÓN DE CUENTAS DE SIEFORE BÁSICA 5 A SIEFORE BÁSICA 4"

           SKIP 2 LINES
           
           PRINT
              COLUMN 005,"NSS",
              COLUMN 015,"TIPO MOVIMIENTO",
              COLUMN 033,"SIEFORE",
              COLUMN 042,"SUBCUENTA",
              COLUMN 057,"MONTO PESOS",
              COLUMN 076,"MONTO ACCIONES"
                                             
           PRINT
              COLUMN 001,"------------------------------------------------------------------------------------------"

        ON EVERY ROW

              PRINT
                 COLUMN 002,reg_repor.nss CLIPPED      ,
                 COLUMN 021,reg_repor.tipo_movimiento  ,
                 COLUMN 036,reg_repor.siefore          ,
                 COLUMN 046,reg_repor.subcuenta        ,
                 COLUMN 055,reg_repor.monto_en_pesos   ,
                 COLUMN 076,reg_repor.monto_en_acciones
                 
END REPORT