##############################################################################
#Owner             => E.F.P.
#Programa TRABIU01 => GENERA SOLIC DE TRASPASO POR UNIFICACION SAR - ISSSSTE
#Fecha creacion    => 24  DE JUNIO  DEL 2009.
#By                => MARCO ANTONIO GONZALEZ ROJAS é ISABEL FONSECA FRIAS
#Fecha de Mod      => 24 DE JUNIO DEL 2009.
#Ultima Mod.       => 22 DE FEBRERO DEL 2010.
#...               => 22 DE ABRIL   DEL 2010.
#...               => MODIFICACIONES CUO FEBRERO 2015. 
#...               => Unica Version CPL-INV-MLM                 
#...               => MLM-3028
#Sistema           => TRA-ICE-UNI-SAR-ISSSTE
#Fecha de emision  => Fecha de emision 22-06-2009 Version: 2.0 sustituye a:
#                     1.0 ( 26-03-2007 )
#Fecha de emision  => 01/12/2009   Version:1.0  Sustituye a ninguna
##############################################################################
DATABASE safre_af

GLOBALS
DEFINE g_desc_exp       CHAR(014)#MLM-3028 Exp  
DEFINE g_eje_fne        CHAR(200)#MLM-3028 Exp   
DEFINE g_ruta_y_arch            ,#MLM-3028
       g_lista          CHAR(100)             
DEFINE v_tipo_sol                     LIKE afi_mae_afiliado.tipo_solicitud
DEFINE vmarca_causa                   SMALLINT
DEFINE ejecuta                        CHAR(300)
DEFINE xcodigo_marca                          ,
       xcodigo_rechazo                SMALLINT
DEFINE vnss             CHAR(11),             
       vmarca_entra             ,             
       vmarca_estado            ,             
       vcodigo_rechazo          ,             
       g_ls_afi_ctr             ,#MLM-3028 Exp       
       g_ls_afi_ref     SMALLINT,#MLM-3028 Exp
       vusuario         ,
       g_hora           CHAR(08) #MLM-3028 

DEFINE reg_cza_sol_trasp RECORD #glo #reg_cza_sol_trasp
       tipo_registro                  ,
       ident_servicio                 ,
       ident_operacion                ,
       tipo_ent_origen       CHAR(02) ,
       cve_ent_origen        CHAR(03) ,
       tipo_ent_destino      CHAR(02) ,
       ent_fed_envio_lote    CHAR(03) ,
       fech_presentacion     CHAR(08) ,
       consec_lote_dia       SMALLINT ,
       cve_mod_recepcion     CHAR(02)
                         END RECORD

DEFINE reg_det_sol_trasp RECORD #glo #reg_det_sol_trasp
       tipo_registro         CHAR(002)     ,
       cont_servicio         DECIMAL(10,0) ,
       tipo_recep_cuenta     CHAR(002)     ,
       cve_recep_cuenta      CHAR(003)     ,
       tipo_ced_cuenta       CHAR(002)     ,
       cve_ced_cuenta        SMALLINT      ,
       orig_tipo_trasp       CHAR(002)     ,
       fech_presentacion     CHAR(008)     ,
       n_unico               CHAR(018)     ,
       n_seguro              CHAR(011)     ,
       rfc                   CHAR(013)     ,
       paterno                             ,
       materno                             ,
       nombres               CHAR(040)     ,
       cve_sector            CHAR(001)     ,
       fech_recep_solic      CHAR(008)     ,
       ident_lote_solic      CHAR(016)     ,
       n_seguro_ent          CHAR(011)     ,
       rfc_ent               CHAR(013)     ,
       id_procesar           CHAR(08)      ,
       num_ctrl_int_icefa    CHAR(030)     ,
       nombre_trab_icefa     CHAR(120)
                         END RECORD

DEFINE reg_sum_sol_trasp RECORD #glo #reg_sum_sol_trasp
       tipo_registro         CHAR(02) ,
       cantidad_reg_det      INTEGER
END RECORD

DEFINE reg_1 RECORD #glo #reg_1
       capturada                      ,
       enviada                        ,
       reenviada                      ,
       rech_conv                      ,
       pendiente                      ,
       pendiente_docto       SMALLINT 
END RECORD

DEFINE #glo #date
       f_fech_recep_solic    ,
       f_fentcons            ,
       f_fecha_solic_tra     ,
       HOY                   DATE
     
DEFINE #glo #char
       x_n_seguro            CHAR(011) ,
       x_n_folio             LIKE safre_af:tra_mae_icefa_issste.n_folio,
       x_tipo_solicitud      LIKE safre_af:tra_mae_icefa_issste.tipo_solicitud,
       g_fol                 LIKE safre_af:tra_mae_icefa_issste.n_folio, 
       g_ts                  LIKE safre_af:tra_mae_icefa_issste.tipo_solicitud,
       RUTA                            ,
       G_LISTA_1                       ,
       G_LISTA_2                       ,
       G_LISTA_3             CHAR(100) ,
       usuario               CHAR(008) ,
       c10_fecha_solic_tra             ,
       c10_fentcons          CHAR(010) ,
       c40_paterno                     ,
       c40_materno                     ,
       c40_nombres           CHAR(040) ,
       enter    	     CHAR(001) ,
       cat                   CHAR(300) ,
       c8_HOY                CHAR(008) ,
       HORA                  CHAR(005)

DEFINE #glo #smallint
       sw_1                  ,
       s_lotes_num           ,
       s_lotes_correlativo   ,
       g_exp_tipo_solic      ,#MLM-3028 Exp
       s_codigo_afore        SMALLINT

DEFINE #glo #integer                         
       i_correlativo         ,               
       g_numsExp             INTEGER#MLM-3028

DEFINE #glo #decimal         #MLM-3028
       cont_reg                          ,
       cont_reg_AcepExp                  ,
       g_exp_n_folio                     ,#MLM-3028 Exp
       cont_reg_RechExp      DECIMAL(10,0)

DEFINE folio_val             LIKE tra_det_aut_issste.folio_interno
DEFINE i                     INTEGER    
DEFINE reg                   RECORD LIKE tra_mae_icefa_issste.* 

DEFINE env_sol    RECORD
       tipo_criterio         LIKE tra_det_aut_issste.tipo_criterio,
       folio_interno         LIKE tra_det_aut_issste.folio_interno,
       cve_ced_cuenta        LIKE tra_det_aut_issste.cve_ced_cuenta,
       estado                LIKE tra_det_aut_issste.estado
                  END RECORD 

DEFINE est_acept      CHAR(020) 
DEFINE g_param_taa    RECORD LIKE seg_modulo.* 

DEFINE rep     RECORD 
       cve_ced_cuenta LIKE tra_det_aut_issste.cve_ced_cuenta,
       tipo_criterio  LIKE tra_det_aut_issste.tipo_criterio,
       total          INTEGER
               END RECORD

DEFINE raz_social     LIKE tab_afore_local.razon_social
DEFINE cod_afore      LIKE tab_afore_local.codigo_afore
DEFINE g_listita       ,      
       permisos        CHAR(100)
DEFINE proc_edo_cod    CHAR(020)
DEFINE g_borra_archs   CHAR(1000)
DEFINE g_codigo_afore  CHAR(003)
DEFINE g_fecha_cap     DATE    
DEFINE g_fuente        CHAR(01)
DEFINE g_curp          CHAR(18)
DEFINE g_exp_n_seguro  CHAR(011)     #MLM-3028 Exp
DEFINE g_pasa                       ,#MLM-3028
       g_cod_result_operac  CHAR(02)          
DEFINE g_stat               INTEGER           

END GLOBALS

MAIN
   OPTIONS INPUT WRAP  ,
   PROMPT LINE LAST    ,
   ACCEPT KEY CONTROL-I
   DEFER INTERRUPT

   CALL STARTLOG(FGL_GETENV("USER")||".TRABIU01.log")#INV-2495   

   WHENEVER ERROR CONTINUE 
      DATABASE safre_tmp

         DROP TABLE res_env_sol

         CREATE  TABLE res_env_sol( tipo_criterio  SMALLINT   ,
                                    folio_interno  INTEGER    ,
                                    cve_ced_cuenta CHAR(03)   ,
                                    estado         SMALLINT)

         CREATE INDEX rrr  ON res_env_sol(folio_interno,tipo_criterio)
         CREATE INDEX rrr1 ON res_env_sol(folio_interno)
				  
      WHENEVER ERROR STOP

      DATABASE safre_af

     CALL init()

     OPEN WINDOW TRABU0011 AT 4,4 WITH FORM "TRABU0011" ATTRIBUTE(BORDER)
     DISPLAY" TRABIU01    GENERA SOLICITUD DE TRASPASO UNI-SAR-ISSSTE                       " AT 3,1 ATTRIBUTE(REVERSE)   

     DISPLAY "                          < CTRL-C> Salir                                  " AT 1,1 ATTRIBUTE(REVERSE)

     DISPLAY HOY USING"DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)

     WHILE TRUE
         PROMPT "ESTA SEGURO S/N ? " FOR CHAR enter
         IF enter MATCHES "[sSnN]" THEN
             IF enter MATCHES "[sS]" THEN
                 EXIT WHILE
             ELSE
                 DISPLAY"PROCESO CANCELADO" AT 19,1 ATTRIBUTE(REVERSE) SLEEP 2
                 EXIT PROGRAM
             END IF
         END IF
     END WHILE

     DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

     CALL genera_cza_sol_trasp()   #gcst
     
           IF  g_codigo_afore <> 564 THEN #AFORE METLIFE ( CON SIEFORE ADICIONAL ) ENTONCES INV-COPPEL       	
              CALL gen_det_sol_traspINVCOPPEL()   #gdst MLM-3028
           ELSE#564 AFORE M E T L I F E ( CON SIEFORE ADICIONAL )
           	  CALL gen_det_sol_traspMET()   #gdst MLM-3028 
           END IF
     
     CALL genera_sum_sol_trasp()   #gsst MLM-3028
    #CALL envio_sar92() 

     INITIALIZE cat TO NULL
     LET cat =    RUTA CLIPPED,"/SOLISSSTE",
                  HOY USING"YYYYMMDD","-", HORA[1,2],HORA[4,5]
     LET cat = cat CLIPPED

     DISPLAY "OP.01 A ENVIAR" AT 15,2
     DISPLAY "ARCHIVO:  ",cat AT 16,2  ATTRIBUTE(REVERSE)

     #--MLM-3028  --#                                                        
     DISPLAY "Rep. Det. Aceptados, Rechazados Expedientes" AT 11,2           
     DISPLAY "ARCHIVO:",g_lista at 12,2                                      
     #--FIN DE LA MODIFICACION  --#                                          
     
     PROMPT "PROCESO FINALIZADO...PRESIONE <ENTER> PARA GENERAR ARCHIVO"
     FOR CHAR enter

     INITIALIZE cat TO NULL
     LET cat = "cat ",G_LISTA_1 CLIPPED," ",G_LISTA_2 CLIPPED," ",
                      G_LISTA_3 CLIPPED," > ",RUTA CLIPPED,"/SOLISSSTE",
                      HOY USING"YYYYMMDD","-", HORA[1,2],HORA[4,5]

     RUN cat

     UPDATE tab_lote
     SET    lotes_num   = s_lotes_num
     WHERE  lotes_fecha = HOY
     AND    lotes_cod   = 6

    #---BORRA ARCHIVOS QUE DEJA DE PASO PARA DEPURAR LA RUTA    INV-2917
    LET g_borra_archs = 'rm ',G_LISTA_1
    RUN g_borra_archs

    INITIALIZE g_borra_archs TO NULL

    LET g_borra_archs = 'rm ',G_LISTA_2
    RUN g_borra_archs

    INITIALIZE g_borra_archs TO NULL

    LET g_borra_archs = 'rm ',G_LISTA_3
    RUN g_borra_archs
    #---
    
    #Se da Permisos al Archivo Generado    
    INITIALIZE cat  TO NULL                
    LET cat = "chmod 777 ",cat   
    RUN cat                                                                           
    #---
    
     CLOSE WINDOW TRABU0011

END MAIN

FUNCTION init()
#--------------

   LET HOY                                   = TODAY
   LET HORA                                  = TIME
   LET c8_HOY                                = HOY USING"YYYYMMDD"
   
   SELECT A.ruta_envio
   INTO   RUTA
   FROM   seg_modulo A
   WHERE  A.modulo_cod = "tra"
   
   SELECT lotes_num          ,
          lotes_correlativo 
   INTO   s_lotes_num        ,
          s_lotes_correlativo
   FROM   tab_lote
   WHERE  lotes_fecha = HOY
   AND    lotes_cod   = 6
   
   IF STATUS = NOTFOUND THEN
      LET s_lotes_num = 1
         INSERT INTO tab_lote VALUES(HOY,6,"TRASPASOS",0,1)
   ELSE
      LET s_lotes_num = s_lotes_num + 1
   END IF
   
   SELECT codigo_afore   ,
          USER
   INTO   s_codigo_afore ,
          usuario
   FROM   tab_afore_local
   
   SELECT  estado
   INTO    reg_1.capturada
   FROM    tra_status
   WHERE   des_estado = "CONFIRMADA"
   
   SELECT  estado
   INTO    reg_1.rech_conv
   FROM    tra_status
   WHERE   des_estado = "RECH_CONV"
   
   SELECT  estado
   INTO    reg_1.reenviada
   FROM    tra_status
   WHERE   des_estado = "REENVIADA"
   
   SELECT  estado
   INTO    reg_1.enviada
   FROM    tra_status
   WHERE   des_estado = "ENVIADA"
   
   SELECT  estado
   INTO    reg_1.pendiente
   FROM    tra_status
   WHERE   des_estado = "PENDIENTE ASIG"
   
   SELECT  estado
   INTO    reg_1.pendiente_docto
   FROM    tra_status
   WHERE   des_estado = "PENDIENTE POR DOCTOS"
   
   SELECT *
   INTO g_param_taa.*
   FROM seg_modulo
   WHERE  modulo_cod = 'tra' 
   
   SELECT estado_cod
   INTO   proc_edo_cod 
   FROM   tra_aut_estado
   WHERE  estado_descripcion = "ENVIADA AL MAESTRO DE ICEFAS " 
   
   LET proc_edo_cod = 30
   
   LET g_cod_result_operac   =    NULL #MLM-3028 
   LET g_pasa                =   "00"  #MLM-3028     
   
   LET g_exp_n_folio         =    NULL #MLM-3028 Exp
   LET g_exp_n_seguro        =    NULL #MLM-3028 Exp   
   LET g_exp_tipo_solic      =    NULL #MLM-3028 Exp   
   
   LET g_ls_afi_ctr          =    0    #MLM-3028 Exp    
   LET g_ls_afi_ref          =    0    #MLM-3028 Exp     
  
   LET g_eje_fne = "EXECUTE FUNCTION fn_verifica_expediente(?,?,?)" #MLM-3028 Exp

END FUNCTION

FUNCTION genera_cza_sol_trasp()
#gcst--------------------------
LET reg_cza_sol_trasp.tipo_registro      = "01"
LET reg_cza_sol_trasp.ident_servicio     = "02"
LET reg_cza_sol_trasp.ident_operacion    = "01"
LET reg_cza_sol_trasp.tipo_ent_origen    = "01"
LET reg_cza_sol_trasp.cve_ent_origen     = s_codigo_afore 
LET reg_cza_sol_trasp.tipo_ent_destino   = "03"
LET reg_cza_sol_trasp.ent_fed_envio_lote = "009"
LET reg_cza_sol_trasp.fech_presentacion  = c8_HOY
LET reg_cza_sol_trasp.consec_lote_dia    = s_lotes_num
LET reg_cza_sol_trasp.cve_mod_recepcion  = "02"

LET G_LISTA_1 = RUTA CLIPPED,"/CST"

START REPORT listado_1 TO G_LISTA_1

   OUTPUT TO REPORT listado_1(reg_cza_sol_trasp.*) #1

FINISH REPORT listado_1

END FUNCTION

FUNCTION gen_det_sol_traspINVCOPPEL()
#gdst--------------------------

DEFINE ee         CHAR(050)
     
    #-- Inicaliza Variables  --##
 
    LET    x_n_seguro                                 =           NULL
    LET    x_n_folio                                  =           NULL
    LET    x_tipo_solicitud                           =           NULL
    LET    reg_det_sol_trasp.cve_ced_cuenta           =           NULL
    LET    reg_det_sol_trasp.n_seguro_ent             =           NULL
    LET    reg_det_sol_trasp.rfc_ent                  =           NULL
    LET    reg_det_sol_trasp.id_procesar              =           NULL
    LET    reg_det_sol_trasp.num_ctrl_int_icefa       =           NULL
    LET    c40_paterno                                =           NULL
    LET    c40_materno                                =           NULL
    LET    c40_nombres                                =           NULL
    LET    f_fecha_solic_tra                          =           NULL
    LET    i_correlativo                              =           0   
    LET    reg_det_sol_trasp.orig_tipo_trasp          =           NULL
    LET    g_fecha_cap                                =           NULL
    LET    g_fuente                                   =           NULL
       
    # C O D I G O     E X C L U S I V O   INV-COPPEL
    #JIRA INV-2495 Se Adiciona Status RECH_CONV (21)	        
    DECLARE cur_1 CURSOR FOR 
    	
       SELECT  A.n_seguro        ,
               A.n_folio         ,
               A.tipo_solicitud  ,
               A.icefa_cod       ,#cve_ced_cuenta
               A.nss             ,#n_seguro_ent
               A.rfc             ,#rfc_ent
               A.id_procesar     ,#id_procesar              #Campo Nuevo
               A.nro_int_cta     ,#num_ctrl_int_icefa
               A.paterno         ,
               A.materno         ,
               A.nombres         , #nombre_trab_icefa
               A.fecha_solic_tra ,
               A.correlativo     ,
               A.origen_traspaso ,#origen_traspaso
               A.fecha_captura   ,#fecha_captura
               A.fuente          ,#fuente                   
               A.status           #MLM-3028  
       FROM    safre_af:tra_mae_icefa_issste A 
       WHERE   A.status         in (reg_1.capturada,      #CONFIRMADA     (1)
                                    reg_1.reenviada,      #REENVIADA      (9)
                                    reg_1.rech_conv )     #RECH_CONV      (21)	
                                   
                                   #JIRA INV-2071 Septiembre 2013 Se Añade Clave Icefa 199
                                   #Cuentas Inactivas Canceladas Peis
                                   #reg_1.rech_conv,      #RECH_CONV      (21)
		    		                       #reg_1.pendiente,      PENDIENTE ASIG (30)
                                   #reg_1.pendiente_docto,#NO EXI EN EL CAT 
 	                                 #reg_1.reenviada )     #REENVIADA       (9)
 	                                 
         AND   A.fecha_captura  >=  "02/01/2010"  # Reg. que entran por arch. 
                                                  # por Uni SAR # ISSSTE  098301
         AND   A.n_seguro IS NOT NULL
         AND   A.n_seguro <> ""
         AND   A.n_seguro <> " "
       
       ORDER BY A.n_seguro
       
       
       LET reg_det_sol_trasp.tipo_registro     = "02"
       LET reg_det_sol_trasp.tipo_recep_cuenta = "01"
       LET reg_det_sol_trasp.cve_recep_cuenta  = s_codigo_afore
       LET reg_det_sol_trasp.tipo_ced_cuenta   = "07"
       LET reg_det_sol_trasp.fech_presentacion = c8_HOY
       LET reg_det_sol_trasp.cve_sector        = "2"
       
       LET reg_det_sol_trasp.ident_lote_solic  = "01",
                                                 s_codigo_afore USING"&&&",
                                                 c8_HOY,
                                                 s_lotes_num   USING"&&&"
       
       #--MLM-3028  --#                
       LET cont_reg          = 0       
       LET cont_reg_AcepExp  = 0      
       LET cont_reg_RechExp  = 0      
       
       LET G_LISTA_2 = RUTA CLIPPED,"/DST"
       
       START REPORT listado_2 TO G_LISTA_2
       
       LET sw_1 = 0
    
       #--MLM-3028  --#                                                   
       LET   g_hora   =   TIME                                     
       LET   g_lista  =   RUTA CLIPPED,"/",                        
                          usuario CLIPPED,".EXPED-SARISSS_",       
                          TODAY USING "YYYYMMDD","_",g_hora CLIPPED
                                                                   
       LET   g_lista  =      g_lista  CLIPPED                      
       START REPORT rpt_2 TO g_lista                               
       #FIN MOD MLM-3028                                           
    
    FOREACH cur_1 INTO x_n_seguro                           ,
                       x_n_folio                            ,
                       x_tipo_solicitud                     ,
                       reg_det_sol_trasp.cve_ced_cuenta     ,
                       reg_det_sol_trasp.n_seguro_ent       ,
                       reg_det_sol_trasp.rfc_ent            ,
                       reg_det_sol_trasp.id_procesar        ,
                       reg_det_sol_trasp.num_ctrl_int_icefa ,
                       c40_paterno                          ,
                       c40_materno                          ,
                       c40_nombres                          ,
                       f_fecha_solic_tra                    ,
                       i_correlativo                        ,
                       reg_det_sol_trasp.orig_tipo_trasp    ,
                       g_stat                               #MLM-3028
         
       #-- MLM-3028 Aqui tendria que Venir  La Validación de si EXISTE ó                            
       #--          No Existe su EXPEDIENTE de IDENTIFICACION DEL TRABAJADOR                        
       
       LET g_exp_n_seguro        =    NULL             
       LET g_exp_tipo_solic      =    NULL              
       LET g_exp_n_folio         =    NULL            
                                                      
       SELECT A.n_seguro       ,                      
              A.n_folio        ,                       
              A.tipo_solicitud                        
       INTO   g_exp_n_seguro   ,                      
              g_exp_n_folio    ,                      
              g_exp_tipo_solic                        
       FROM   safre_af:afi_mae_afiliado A             
       WHERE  A.n_seguro  =  x_n_seguro               
                                                      
                                                      
       IF ( g_exp_n_seguro IS NULL  OR                
       	    g_exp_n_seguro = ""     OR                
       	    g_exp_n_seguro = " "  ) THEN              
       
           #-- No esta el NSS en Maestro de Afiliacion    --#
        	 LET g_exp_n_seguro = "ABCDEFGHIJK"         
        	   
       END IF                                         
       
       LET  g_ls_afi_ctr  =     0                      
       LET  g_ls_afi_ref  =     0                      
       LET  g_pasa        =    "00"                   
       
       IF g_exp_n_seguro <> "ABCDEFGHIJK"  THEN       #MLM-3028 Exp  Si es Afiliado de la Afore
         
          PREPARE ver_exp FROM g_eje_fne                              
          DECLARE cur_exp CURSOR FOR ver_exp                         
                                                                        
          OPEN cur_exp USING g_exp_n_seguro   ,       #MLM-3028 Exp VARIABLES Q' SE ENVIAN 
                             g_exp_n_folio    ,       
                             g_exp_tipo_solic         
             FETCH cur_exp INTO g_ls_afi_ctr  ,       #MLM-3028 Exp VARIABLES RECIBIDAS    
                                g_ls_afi_ref          
                                
                                                        
          CLOSE cur_exp                                             
           
          IF g_ls_afi_ctr IS NULL THEN 
             LET g_ls_afi_ctr = 0      
          END IF                       
                                                                        
          #Apartir de Aqui vendria el Codigo del Trato de Variables.                                           
          IF g_ls_afi_ctr = 1  THEN                   #MLM-3028 Exp Si tiene Expediente
          	
          	 LET g_pasa       = "01" #ACEPTADO 
          	 LET g_desc_exp   = "CON EXPEDIENTE"  
        
          ELSE#NO TIENE EXPEDIENTE
          	
          	 LET g_desc_exp     = "SIN EXPEDIENTE" 
          	 LET g_stat         = 77
          	 
          	 UPDATE tra_mae_icefa     
             SET    status      = 77  #RECH. POR EXP. IDENT
             WHERE  correlativo = i_correlativo
          	
          END IF                                      
                                                      
       END IF         
       #-- HASTA AQUI MLM-3028 Exp  HASTA AQUI MLM-3028 Exp HASTA AQUI MLM-3028 Exp--#
       
              
       
       IF g_pasa   = "01" THEN#ACEPTADO < SI Existe EXPEDIENTE de IDENTIFICACION DEL TRABAJADOR >       	  
       	 
       	  LET cont_reg_AcepExp = cont_reg_AcepExp + 1                               
       	  DISPLAY "Num. de Casos Aceptados(Expediente de Ident.): ","  ",cont_reg_AcepExp at 07,12
       	  
       	  OUTPUT TO REPORT rpt_2 ( g_exp_n_seguro      , #MLM-3028Bis Se Ingresa para que Muestre el Reporte
       	                           g_exp_n_folio       , #MLM-3028 Exp Las Solic. Aceptadas.
       	                           g_exp_tipo_solic    , #MLM-3028 Exp Las Solic. Aceptadas.
                                   g_ls_afi_ctr        , #MLM-3028 Exp Las Solic. Aceptadas. 
                                   g_desc_exp          , #MLM-3028 Exp Las Solic. Aceptadas.                                  
                                   g_stat              ) #MLM-3028 Exp       	  
       	  
       END IF
       	  
       IF g_pasa   = "00"  THEN#< NO > Existe EXPEDIENTE de IDENTIFICACION DEL TRABAJADOR
       	  LET cont_reg_RechExp = cont_reg_RechExp + 1                                 
       	  DISPLAY "Num. de Casos Rechazados(Expediente de Ident.):","  ",cont_reg_RechExp at 08,12 
          
          OUTPUT TO REPORT rpt_2 ( g_exp_n_seguro      , #MLM-3028Bis Se Ingresa para que Muestre el Reporte
       	                           g_exp_n_folio       , #MLM-3028 Exp Las Solic. Aceptadas.
       	                           g_exp_tipo_solic    , #MLM-3028 Exp Las Solic. Aceptadas.
                                   g_ls_afi_ctr        , #MLM-3028 Exp Las Solic. Aceptadas. 
                                   g_desc_exp          , #MLM-3028 Exp Las Solic. Aceptadas.                                  
                                   g_stat              ) #MLM-3028 Exp 
       	  
       	  #--CONTINUE FOREACH
       END IF                
       #-- FIN VALIDACION  MLM-3028
    
       #--CHECA QUE EL n_folio NO VENGA NULO---#
       IF g_pasa   = "01" THEN#ACEPTADO < SI Existe EXPEDIENTE de IDENTIFICACION DEL TRABAJADOR >                                                                                                                            
        
          IF x_n_folio IS NULL OR x_n_folio = "" OR x_n_folio = " " THEN
          
             LET g_fol        =     NULL
             LET g_ts         =     NULL
             
             SELECT a.n_folio,a.tipo_solicitud
             INTO   g_fol,g_ts
             FROM   afi_mae_afiliado a
             WHERE  a.n_seguro   =   x_n_seguro
             
             UPDATE tra_mae_icefa_issste
             SET    tra_mae_icefa_issste.n_folio         = g_fol              ,
                    tra_mae_icefa_issste.tipo_solicitud  = g_ts               
             WHERE  tra_mae_icefa_issste.correlativo     = i_correlativo
          
          END IF
          
          SELECT A.tipo_solicitud
          INTO   v_tipo_sol 
          FROM   afi_mae_afiliado A
          WHERE  A.n_seguro = x_n_seguro
          
          IF v_tipo_sol = 5 THEN
          
              UPDATE tra_mae_icefa_issste
              SET    tra_mae_icefa_issste.status = reg_1.pendiente
              WHERE  correlativo = i_correlativo
          
             CONTINUE FOREACH
          
          END IF
          
          SELECT "OK"
          FROM  cta_act_marca A
          WHERE nss = x_n_seguro 
          AND   A.marca_cod = 5
          AND   A.marca_causa  between 200 and 299
          GROUP BY 1
          
          IF STATUS <> NOTFOUND THEN
          
             UPDATE tra_mae_icefa_issste 
             SET    tra_mae_icefa_issste.status = reg_1.rech_conv
             WHERE  correlativo = i_correlativo
             
             CONTINUE FOREACH
          
          END IF
          
          LET vnss               =    x_n_seguro
          LET vmarca_entra       =    260
          LET vmarca_estado      =    0
          LET vcodigo_rechazo    =    0
          
          SELECT user
          INTO   vusuario 
          FROM   tab_afore_local
          
          
          LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(","'",vnss    ,"'"   ,",",
          	  				                                  vmarca_entra   ,",",
          				                                      i_correlativo  ,",",
                                   				            vmarca_estado  ,",",
               		                                    vcodigo_rechazo,",",
          		                                          vmarca_causa,",",
          			                                        "'',",
          			                                        "'",vusuario,"')"
          
          
          LET ejecuta = ejecuta CLIPPED
          
          PREPARE clausula_spl FROM ejecuta
          
          DECLARE cursor_marca CURSOR FOR clausula_spl
          OPEN    cursor_marca
          FETCH   cursor_marca INTO xcodigo_marca, xcodigo_rechazo
          CLOSE   cursor_marca
          
          IF xcodigo_rechazo <> 0 THEN
          
             UPDATE tra_mae_icefa_issste 
             SET    tra_mae_icefa_issste.status = reg_1.rech_conv
             WHERE  correlativo = i_correlativo
          
             CONTINUE FOREACH
          
          END IF
          
          LET cont_reg = cont_reg + 1  
          
          IF cont_reg > 10000 THEN
              EXIT FOREACH
          END IF
          
          DISPLAY "NRO. DE REGISTROS PROCESADOS ",cont_reg at 05,8
          
          SELECT  B.n_unico  ,
                  B.n_seguro ,
                  B.n_rfc    ,
                  B.paterno  ,
                  B.materno  ,
                  B.nombres  ,
                  B.fentcons
          INTO
                  reg_det_sol_trasp.n_unico  ,
                  reg_det_sol_trasp.n_seguro ,
                  reg_det_sol_trasp.rfc      ,
                  reg_det_sol_trasp.paterno  ,
                  reg_det_sol_trasp.materno  ,
                  reg_det_sol_trasp.nombres  ,
                  f_fentcons
          FROM    afi_mae_afiliado B
          WHERE   B.n_seguro = x_n_seguro
          
          LET reg_det_sol_trasp.nombre_trab_icefa[01,40]  = c40_paterno 
          LET reg_det_sol_trasp.nombre_trab_icefa[41,80]  = c40_materno
          LET reg_det_sol_trasp.nombre_trab_icefa[81,120] = c40_nombres
          
          IF f_fecha_solic_tra IS NULL THEN
             LET c10_fentcons = f_fentcons
             LET reg_det_sol_trasp.fech_recep_solic =  c10_fentcons[07,10],
                                                       c10_fentcons[01,02],
                                                       c10_fentcons[04,05]
             LET f_fech_recep_solic = f_fentcons
          ELSE
             LET c10_fecha_solic_tra = f_fecha_solic_tra
             LET reg_det_sol_trasp.fech_recep_solic =c10_fecha_solic_tra[07,10],
                                                  c10_fecha_solic_tra[01,02],
                                                  c10_fecha_solic_tra[04,05]
             LET f_fech_recep_solic = f_fecha_solic_tra
          END IF
          
          LET sw_1 = 1
          
          #MOD. MLM-1314 Agosto 2012                                                                                             
          #VALIDACION campos llaves si VIENEN NULOS se Guardaran con ""                                                          
                                                                                                                                 
          IF x_n_seguro IS NULL THEN                                                                                             
                                                                                                                                 
             LET     x_n_seguro  =  ""                                                                                            
                                                                                                                                  
             UPDATE  tra_mae_icefa_issste                                                                                         
             SET     tra_mae_icefa_issste.n_seguro        = x_n_seguro                                                            
             WHERE   tra_mae_icefa_issste.correlativo     = i_correlativo                                                         
                                                                                                                                 
          END IF                                                                                                                 
                                                                                                                                 
          IF reg_det_sol_trasp.n_seguro_ent IS NULL THEN                                                                         
                                                                                                                                 
             LET  reg_det_sol_trasp.n_seguro_ent  =  ""                                                                           
                                                                                                                                  
             UPDATE  tra_mae_icefa_issste                                                                                         
             SET     tra_mae_icefa_issste.nss             = reg_det_sol_trasp.n_seguro_ent                                        
             WHERE   tra_mae_icefa_issste.correlativo     = i_correlativo                                                         
                                                                                                                                  
          END IF                                                                                                                 
                                                                                                                                        
          IF reg_det_sol_trasp.rfc_ent IS NULL THEN                                                                              
                                                                                                                                 
             LET  reg_det_sol_trasp.rfc_ent  =  ""                                                                                
                                                                                                                                  
             UPDATE  tra_mae_icefa_issste                                                                                         
             SET     tra_mae_icefa_issste.rfc             = reg_det_sol_trasp.rfc_ent                                             
             WHERE   tra_mae_icefa_issste.correlativo     = i_correlativo                                                         
                                                                                                                                  
          END IF                                                                                                                 
                                                                                                                                        
          IF reg_det_sol_trasp.id_procesar IS NULL THEN                                                                          
                                                                                                                                 
            LET  reg_det_sol_trasp.id_procesar  =  ""                                                                            
                                                                                                                                 
            UPDATE  tra_mae_icefa_issste                                                                                         
            SET     tra_mae_icefa_issste.id_procesar     = reg_det_sol_trasp.id_procesar                                         
            WHERE   tra_mae_icefa_issste.correlativo     = i_correlativo                                                         
                                                                                                                                 
          END IF                                                                                                                 
                                                                                                                                        
          IF reg_det_sol_trasp.num_ctrl_int_icefa IS NULL THEN                                                                   
                                                                                                                                 
             LET  reg_det_sol_trasp.num_ctrl_int_icefa  =  ""                                                                     
                                                                                                                                  
             UPDATE  tra_mae_icefa_issste                                                                                         
             SET     tra_mae_icefa_issste.nro_int_cta     = reg_det_sol_trasp.num_ctrl_int_icefa                                  
             WHERE   tra_mae_icefa_issste.correlativo     = i_correlativo                                                         
                                                                                                                                  
          END IF                                                                                                                 
                                                                                                                                 
          #FIN VALIDACION campos llaves si VIENEN NULOS se Guardaran con ""
          
          OUTPUT TO REPORT listado_2(reg_det_sol_trasp.*) #2
          
          
          UPDATE tra_mae_icefa_issste
          SET    tra_mae_icefa_issste.status             =     reg_1.enviada      ,
                 tra_mae_icefa_issste.lote_genera        =     s_lotes_num        ,
                 tra_mae_icefa_issste.fecha_genera       =     TODAY              ,
                 tra_mae_icefa_issste.fecha_proceso      =     TODAY              ,
                 tra_mae_icefa_issste.fecha_solic_tra    =     f_fech_recep_solic ,
                 tra_mae_icefa_issste.n_envios           =     tra_mae_icefa_issste.n_envios + 1
          WHERE   tra_mae_icefa_issste.correlativo        =     i_correlativo
            
          IF cont_reg = 10000 THEN
             EXIT FOREACH
          END IF
     
       END IF #ACEPTADO < SI Existe EXPEDIENTE de IDENTIFICACION DEL TRABAJADOR >                                                                                                                                    
 
          LET g_cod_result_operac =   NULL  #MLM-3028
          LET g_pasa              =   "00"  #MLM-3028          
          
          LET g_exp_n_folio       =   NULL  #MLM-3028 Exp
          LET g_exp_n_seguro      =   NULL  #MLM-3028 Exp   
          LET g_exp_tipo_solic    =   NULL  #MLM-3028 Exp 
          
          LET g_ls_afi_ctr        =   0     #MLM-3028 Exp 
          LET g_ls_afi_ref        =   0     #MLM-3028 Exp           
              
    END FOREACH
    
    #-- Codigo Común para todos los Clientes.  --#
    IF sw_1 = 0 THEN #MLM-3028Bis  NO EXISTEN REGISTROS A ENVIAR OP 01
    	
      #--#MLM-3028Bis--#                                                       
       DISPLAY "Rep. Det. Aceptados, Rechazados Expedientes" AT 11,2           
       DISPLAY "ARCHIVO:",g_lista at 12,2                                      
    	 
    	 INITIALIZE  cat  TO NULL
          	 
       LET g_borra_archs = 'rm ',G_LISTA_1 
       RUN g_borra_archs                  
                                          
    	 INITIALIZE g_borra_archs TO NULL    
    	                                     
    	 LET g_borra_archs = 'rm ',G_LISTA_2 
    	 RUN g_borra_archs                   
    	                                     
    	 INITIALIZE g_borra_archs TO NULL    
    	                                     
    	 LET g_borra_archs = 'rm ',G_LISTA_3 
    	 RUN g_borra_archs                   
    	 #--FIN DE LA MODIFICACION  MLM-3028Bis--#    	 
    	 
       DISPLAY "NO SE ENCONTRARON REGISTROS A ENVIAR OP01" AT 19,1 ATTRIBUTE(REVERSE)
       SLEEP 3
       #--EXIT PROGRAM  MLM-3028Bis--

    END IF
    
    IF sw_1 = 0 THEN #MLM-3028Bis  NO EXISTEN REGISTROS A ENVIAR OP 01
       FINISH REPORT rpt_2
       EXIT PROGRAM	
    END IF 	   
    
    FINISH REPORT listado_2
    FINISH REPORT rpt_2     

END FUNCTION

FUNCTION gen_det_sol_traspMET()
#gdst--------------------------

DEFINE ee         CHAR(050)
     
    #-- Inicaliza Variables  --##
 
    LET    x_n_seguro                                 =           NULL
    LET    x_n_folio                                  =           NULL
    LET    x_tipo_solicitud                           =           NULL
    LET    reg_det_sol_trasp.cve_ced_cuenta           =           NULL
    LET    reg_det_sol_trasp.n_seguro_ent             =           NULL
    LET    reg_det_sol_trasp.rfc_ent                  =           NULL
    LET    reg_det_sol_trasp.id_procesar              =           NULL
    LET    reg_det_sol_trasp.num_ctrl_int_icefa       =           NULL
    LET    c40_paterno                                =           NULL
    LET    c40_materno                                =           NULL
    LET    c40_nombres                                =           NULL
    LET    f_fecha_solic_tra                          =           NULL
    LET    i_correlativo                              =           0   
    LET    reg_det_sol_trasp.orig_tipo_trasp          =           NULL
    LET    g_fecha_cap                                =           NULL
    LET    g_fuente                                   =           NULL
       
 
    DECLARE cur_21 CURSOR FOR                                                                                                        
       SELECT  A.n_seguro        ,                                                                                                  
               A.n_folio         ,                                                                                                  
               A.tipo_solicitud  ,                                                                                                  
               A.icefa_cod       ,#cve_ced_cuenta                                                                                   
               A.nss             ,#n_seguro_ent                                                                                     
               A.rfc             ,#rfc_ent                                                                                          
               A.id_procesar     ,#id_procesar              #Campo Nuevo                                                            
               A.nro_int_cta     ,#num_ctrl_int_icefa                                                                               
               A.paterno         ,                                                                                                  
               A.materno         ,                                                                                                  
               A.nombres         ,#nombre_trab_icefa                                                                                
               A.fecha_solic_tra ,                                                                                                  
               A.correlativo     ,                                                                                                  
               A.origen_traspaso ,#origen_traspaso                                                                                  
               A.fecha_captura   ,#fecha_captura                                                                                    
               A.fuente          ,#fuente                                                                                           
               A.status           #MLM-3028 
       FROM    safre_af:tra_mae_icefa_issste A                                                                                      
       WHERE   A.status         in (reg_1.capturada      ,#CONFIRMADA     (1)                                                       
                                    reg_1.reenviada      ,#REENVIADA      (9)                                                       
                                    reg_1.rech_conv )     #RECH_CONV                                                                
                                                                                                                                    
                                   #JIRA MLM-1999 Julio 2013 Se Añade Clave Icefa 199                                               
                                   #Cuentas Inactivas Canceladas Peis                                                               
                                   #Se Omite para que solo tome Solicitudes Recientes                                               
                                   #para el Caso de la Icefa Cod 178 y 199.                                                         
                                                                                                                                    
                                   #reg_1.rech_conv      ,#RECH_CONV      (21)                                                      
          	                       #reg_1.pendiente      ,#PENDIENTE ASIG (30)                                                       
                                   #reg_1.pendiente_docto,#NO EXI EN EL CAT                                                          
                                                                                                                                    
                                                                                                                                    
       AND   A.fecha_captura  >=  "02/01/2010"  # Reg. que entran por arch.                                                         
                                                # por Uni SAR # ISSSTE  098301                                                      
       AND   A.n_seguro IS NOT NULL                                                                                                 
       AND   A.n_seguro <> ""                                                                                                       
       AND   A.n_seguro <> " "                                                                                                      
       AND   A.fuente   <>  4 #CAPT POR USUARIO <NO POR ARCHIVO DE BDSARISSSTE>                                                         	                                                                                                                                 
       UNION                                                                                                                               	                                                                                                                                 
       SELECT  A.n_seguro        ,                                                                                                  
               A.n_folio         ,                                                                                                  
               A.tipo_solicitud  ,                                                                                                  
               A.icefa_cod       ,#cve_ced_cuenta                                                                                   
               A.nss             ,#n_seguro_ent                                                                                     
               A.rfc             ,#rfc_ent                                                                                          
               A.id_procesar     ,#id_procesar              #Campo Nuevo                                                            
               A.nro_int_cta     ,#num_ctrl_int_icefa                                                                               
               A.paterno         ,                                                                                                  
               A.materno         ,                                                                                                  
               A.nombres         ,#nombre_trab_icefa                                                                               
               A.fecha_solic_tra ,                                                                                                 
               A.correlativo     ,                                                                                                 
               A.origen_traspaso ,#origen_traspaso                                                                                 
               A.fecha_captura   ,#fecha_captura                                                                                   
               A.fuente          ,#fuente                                                                                          
               A.status           #MLM-3028 
       FROM    safre_af:tra_mae_icefa_issste A                                                                                      
       WHERE   A.status         in (reg_1.capturada      ,#CONFIRMADA                                                               
                                    reg_1.reenviada      ,#REENVIADA                                                                
                                    reg_1.rech_conv )                                                                               
                                                                                                                                    
                                    #JIRA MLM-1999 Julio 2013 Se Añade Clave Icefa 199                                              
                                    #Cuentas Inactivas Canceladas Peis                                                              
                                    #Se Omite para que solo tome Solicitudes Recientes                                              
                                    #para el Caso de la Icefa Cod 178 y 199.                                                        
                                                                                                                                    
                                    #reg_1.rech_conv      ,#RECH_CONV      (21)                                                     
                                    #reg_1.pendiente      ,#PENDIENTE ASIG (30)                                                    
                                    #reg_1.pendiente_docto,#NO EXI EN EL CAT                                                        
       AND    A.fecha_captura  >=  "02/01/2010"  # Reg. que entran por arch.                                                        
                                                 # por Uni SAR # ISSSTE  098301                                                     
                                                                                                                                    
       AND    A.fuente          =    4 #CAPTURA POR ARCHIVO DE BDSARISSSTE                                                          
       AND    A.n_seguro IS NOT NULL                                                                                                
       AND    A.n_seguro <> ""                                                                                                      
       AND    A.n_seguro <> " "                                                                                                     
       AND    A.tipo_solicitud  <>  8                                                                                               
       UNION                                                                                                                          
       SELECT  A.n_seguro        ,                                                                                                 
               A.n_folio         ,                                                                                                 
               A.tipo_solicitud  ,                                                                                                 
               A.icefa_cod       ,#cve_ced_cuenta                                                                                  
               A.nss             ,#n_seguro_ent                                                                                    
               A.rfc             ,#rfc_ent                                                                                         
               A.id_procesar     ,#id_procesar              #Campo Nuevo                                                           
               A.nro_int_cta     ,#num_ctrl_int_icefa                                                                              
               A.paterno         ,                                                                                                 
               A.materno         ,                                                                                                 
               A.nombres         ,#nombre_trab_icefa                                                                              
               A.fecha_solic_tra ,                                                                                                
               A.correlativo     ,                                                                                                
               A.origen_traspaso ,#origen_traspaso                                                                                
               A.fecha_captura   ,#fecha_captura                                                                                  
               A.fuente          ,#fuente                                                                                         
               A.status           #MLM-3028 
       FROM    safre_af:tra_mae_icefa_issste A                                                                                     
       WHERE   A.status         in (reg_1.capturada      ,#CONFIRMADA                                                              
                                    reg_1.reenviada      ,#REENVIADA                                                               
                                    reg_1.rech_conv )     #RECH_CONV                                                               
                                                                                                                                   
                                   #JIRA MLM-1999 Julio 2013 Se Añade Clave Icefa 199                                              
                                   #Cuentas Inactivas Canceladas Peis                                                              
                                   #Se Omite para que solo tome Solicitudes Recientes                                              
                                   #para el Caso de la Icefa Cod 178 y 199.                                                        
                                                                                                                                   
                                   #reg_1.rech_conv      ,#RECH_CONV      (21)                                                     
          	                       #reg_1.pendiente      ,#PENDIENTE ASIG (30)                                                    
                                   #reg_1.pendiente_docto,#NO EXI EN EL CAT                                                        
                                                                                                                                   
       AND    A.fecha_captura  >=  "02/01/2010"  # Reg. que entran por arch.                                                       
                                                 # por Uni SAR # ISSSTE  098301                                                    
                                                                                                                                   
       AND    A.fuente          =    4 #CAPTURA POR ARCHIVO DE BDSARISSSTE                                                         
       AND    A.tipo_solicitud  =    8 #Los que pueden TRAER NSS = NULOS "o"                                                       
                                       #NSS = "" 'o' NSS = " "   
                                                                                                                                        
       LET reg_det_sol_trasp.tipo_registro     = "02"                                                                                  
       LET reg_det_sol_trasp.tipo_recep_cuenta = "01"                                                                                  
       LET reg_det_sol_trasp.cve_recep_cuenta  = s_codigo_afore                                                                        
       LET reg_det_sol_trasp.tipo_ced_cuenta   = "07"                                                                                  
       LET reg_det_sol_trasp.fech_presentacion = c8_HOY                                                                                
       LET reg_det_sol_trasp.cve_sector        = "2"                                                                                   
                                                                                                                                       
       LET reg_det_sol_trasp.ident_lote_solic  = "01",                                                                                 
                                                 s_codigo_afore USING"&&&",                                                            
                                                 c8_HOY,                                                                               
                                                 s_lotes_num   USING"&&&"                                                              
                                                                                                                                       
       #--MLM-3028  --# 
       LET cont_reg          = 0
       LET cont_reg_AcepExp  = 0
       LET cont_reg_RechExp  = 0                                                                                                                      
                                                                                                                                       
       LET G_LISTA_2 = RUTA CLIPPED,"/DST"                                                                                             
                                                                                                                                       
       START REPORT listado_2 TO G_LISTA_2                                                                                             
                                                                                                                                       
       LET sw_1 = 0                                                                                                                    
        
      #--MLM-3028  --# 
       LET   g_hora   =   TIME                                                                               
       LET   g_lista  =   RUTA CLIPPED,"/",                                                     
                          usuario CLIPPED,".EXPED-SARISSS_",                             
                          TODAY USING "YYYYMMDD","_",g_hora CLIPPED                                          
      
       LET   g_lista  =      g_lista  CLIPPED                                
       START REPORT rpt_2 TO g_lista                                  
       #FIN MOD MLM-3028
                                                                                                                                        
    FOREACH cur_21 INTO x_n_seguro                           ,                                                                   
                        x_n_folio                            ,                                                                   
                        x_tipo_solicitud                     ,                                                                   
                        reg_det_sol_trasp.cve_ced_cuenta     ,                                                                   
                        reg_det_sol_trasp.n_seguro_ent       ,                                                                   
                        reg_det_sol_trasp.rfc_ent            ,                                                                   
                        reg_det_sol_trasp.id_procesar        ,                                                                   
                        reg_det_sol_trasp.num_ctrl_int_icefa ,                                                                   
                        c40_paterno                          ,                                                                   
                        c40_materno                          ,                                                                   
                        c40_nombres                          ,                                                                   
                        f_fecha_solic_tra                    ,                                                                   
                        i_correlativo                        ,                                                                   
                        reg_det_sol_trasp.orig_tipo_trasp    ,                                                                   
                        g_fecha_cap                          ,#Nvas Variables                                                    
                        g_fuente                             ,
                        g_stat                               #MLM-3028                                                                    
                                  
                                  
       #-- MLM-3028 Aqui tendria que Venir  La Validación de si EXISTE ó
       #--          No Existe su EXPEDIENTE de IDENTIFICACION DEL TRABAJADOR
      
       LET g_exp_n_seguro        =    NULL             
       LET g_exp_tipo_solic      =    NULL              
       LET g_exp_n_folio         =    NULL            
                                                      
       SELECT A.n_seguro       ,                      
              A.n_folio        ,                       
              A.tipo_solicitud                        
       INTO   g_exp_n_seguro   ,                      
              g_exp_n_folio    ,                      
              g_exp_tipo_solic                        
       FROM   safre_af:afi_mae_afiliado A             
       WHERE  A.n_seguro  =  x_n_seguro               
                                                      
                                                      
       IF ( g_exp_n_seguro IS NULL  OR                
       	    g_exp_n_seguro = ""     OR                
       	    g_exp_n_seguro = " "  ) THEN              
       
           #-- No esta el NSS en Maestro de Afiliacion    --#
        	 LET g_exp_n_seguro = "ABCDEFGHIJK"         
        	   
       END IF                                         
       
       LET  g_ls_afi_ctr  =    0                      
       LET  g_ls_afi_ref  =    0                      
       LET  g_pasa        =    "00"                   
       
       IF g_exp_n_seguro <> "ABCDEFGHIJK"  THEN       #MLM-3028 Exp  Si es Afiliado de la Afore
         
          PREPARE metver_exp FROM g_eje_fne                              
          DECLARE metcur_exp CURSOR FOR metver_exp                         
                                                                        
          OPEN metcur_exp USING g_exp_n_seguro   ,       #MLM-3028 Exp VARIABLES Q' SE ENVIAN 
                                g_exp_n_folio    ,       
                                g_exp_tipo_solic         
             FETCH metcur_exp INTO g_ls_afi_ctr  ,       #MLM-3028 Exp VARIABLES RECIBIDAS    
                                   g_ls_afi_ref          
                                
                                                        
          CLOSE metcur_exp  
          
          IF g_ls_afi_ctr IS NULL THEN
             LET g_ls_afi_ctr = 0     
          END IF                                                                 
                                                                        
          #Apartir de Aqui vendria el Codigo del Trato de Variables.                                           
          IF g_ls_afi_ctr = 1  THEN                   #MLM-3028 Exp Si tiene Expediente
          	
          	 LET g_pasa       = "01" #ACEPTADO 
          	 LET g_desc_exp   = "CON EXPEDIENTE"  
        
          ELSE#NO TIENE EXPEDIENTE
          	
          	 LET g_desc_exp     = "SIN EXPEDIENTE" 
          	 LET g_stat         = 77
          	 
          	 UPDATE tra_mae_icefa     
             SET    status      = 77  #RECH. POR EXP. IDENT
             WHERE  correlativo = i_correlativo
          	
          END IF                                      
                                                      
       END IF         
       #-- HASTA AQUI MLM-3028 Exp  HASTA AQUI MLM-3028 Exp HASTA AQUI MLM-3028 Exp--#
       
              
       
       IF g_pasa   = "01" THEN#ACEPTADO < SI Existe EXPEDIENTE de IDENTIFICACION DEL TRABAJADOR >       	  
       	 
       	  LET cont_reg_AcepExp = cont_reg_AcepExp + 1                               
       	  DISPLAY "Num. de Casos Aceptados(Expediente de Ident.): ","  ",cont_reg_AcepExp at 07,12
       	  
       	  OUTPUT TO REPORT rpt_2 ( g_exp_n_seguro      , #MLM-3028Bis Se Ingresa para que Muestre el Reporte
       	                           g_exp_n_folio       , #MLM-3028 Exp Las Solic. Aceptadas.
       	                           g_exp_tipo_solic    , #MLM-3028 Exp Las Solic. Aceptadas.
                                   g_ls_afi_ctr        , #MLM-3028 Exp Las Solic. Aceptadas. 
                                   g_desc_exp          , #MLM-3028 Exp Las Solic. Aceptadas.                                  
                                   g_stat              ) #MLM-3028 Exp       	  
       	  
       END IF
       	  
       IF g_pasa   = "00"  THEN#< NO > Existe EXPEDIENTE de IDENTIFICACION DEL TRABAJADOR
       	  LET cont_reg_RechExp = cont_reg_RechExp + 1                                 
       	  DISPLAY "Num. de Casos Rechazados(Expediente de Ident.):","  ",cont_reg_RechExp at 08,12 
          
          OUTPUT TO REPORT rpt_2 ( g_exp_n_seguro      , #MLM-3028Bis Se Ingresa para que Muestre el Reporte
       	                           g_exp_n_folio       , #MLM-3028 Exp Las Solic. Aceptadas.
       	                           g_exp_tipo_solic    , #MLM-3028 Exp Las Solic. Aceptadas.
                                   g_ls_afi_ctr        , #MLM-3028 Exp Las Solic. Aceptadas. 
                                   g_desc_exp          , #MLM-3028 Exp Las Solic. Aceptadas.                                  
                                   g_stat              ) #MLM-3028 Exp 
       	  
       	  #--CONTINUE FOREACH
       END IF                
       #-- FIN VALIDACION  MLM-3028
                                                                                                                                
       #--REGISTROS CON NSS = NULOS ò NSS = "" ò NSS = " " ---#                                                                     
       #--LOS CUALES VIENEN DE ARCHIVO #                                                                                            
                                                                                                                                    
        
       IF g_pasa   = "01" THEN#ACEPTADO < SI Existe EXPEDIENTE de IDENTIFICACION DEL TRABAJADOR >                                                                                                                            
       
          IF (  x_tipo_solicitud =  8   AND   g_fuente    =  4 ) THEN                                                                  
                                                                                                                                       
             LET    g_curp                 =        NULL                                                                               
                                                                                                                                       
             SELECT A.curp                                                                                                             
             INTO   g_curp                                                                                                             
             FROM   tra_issste_unifica A                                                                                               
             WHERE  A.id_procesar          =        reg_det_sol_trasp.id_procesar                                                      
             AND    A.f_actualiza          =        g_fecha_cap                                                                        
             AND    A.nss_icefa            =        reg_det_sol_trasp.n_seguro_ent                                                     
             AND    A.rfc_icefa            =        reg_det_sol_trasp.rfc_ent                                                          
             GROUP  BY 1                                                                                                               
                                                                                                                                       
             IF ( STATUS =  NOTFOUND ) THEN                                                                                            
                                                                                                                                       
                CONTINUE FOREACH                                                                                                       
                                                                                                                                       
             ELSE                                                                                                                      
                                                                                                                                       
                SELECT  B.n_seguro                                                                                                     
                INTO    x_n_seguro                                                                                                     
                FROM    afi_mae_afiliado B                                                                                             
                WHERE   B.n_unico             =        g_curp                                                                          
                AND     B.tipo_solicitud      =        x_tipo_solicitud                                                                
                AND     B.n_folio             =        x_n_folio                                                                       
                                                                                                                                       
                IF ( x_n_seguro IS NULL OR x_n_seguro = "" OR x_n_seguro = " " ) THEN                                                  
                                                                                                                                       
                   CONTINUE FOREACH                                                                                                    
                                                                                                                                       
                ELSE                                                                                                                   
                                                                                                                                       
                   UPDATE tra_mae_icefa_issste                                                                                         
                   SET    tra_mae_icefa_issste.n_seguro        = x_n_seguro                                                            
                   WHERE  tra_mae_icefa_issste.correlativo     = i_correlativo                                                         
                                                                                                                                       
                END IF                                                                                                                 
                                                                                                                                       
             END IF                                                                                                                    
                                                                                                                                       
          END IF                                                                                                                       
                                                                                                                                        
          #--FIN  DE REGISTROS CON NSS = NULOS ò NSS = "" ò NSS = " " ---#                                                                 	                                                                                                                                        	                                                                                                                                    
          
          #--CHECA QUE EL n_folio NO VENGA NULO---#                                                                                           
                                                                                                                                          
          IF x_n_folio IS NULL OR x_n_folio = "" OR x_n_folio = " " THEN                                                                      
                                                                                                                                             
              LET g_fol        =     NULL                                                                                                     
              LET g_ts         =     NULL                                                                                                     
                                                                                                                                             
              SELECT a.n_folio,a.tipo_solicitud                                                                                               
              INTO   g_fol,g_ts                                                                                                               
              FROM   afi_mae_afiliado a                                                                                                       
              WHERE  a.n_seguro   =   x_n_seguro                                                                                              
                                                                                                                                             
              UPDATE tra_mae_icefa_issste                                                                                                     
              SET    tra_mae_icefa_issste.n_folio         = g_fol              ,                                                              
                     tra_mae_icefa_issste.tipo_solicitud  = g_ts                                                                              
              WHERE  tra_mae_icefa_issste.correlativo     = i_correlativo                                                                     
                                                                                                                                             
          END IF                                                                                                                              
          #---                                                                                                                                
                                                                                                                                             
          SELECT A.tipo_solicitud                                                                                                             
          INTO   v_tipo_sol                                                                                                                   
          FROM   afi_mae_afiliado A                                                                                                           
          WHERE  A.n_seguro = x_n_seguro                                                                                                      
                                                                                                                                             
          IF v_tipo_sol = 5 THEN                                                                                                              
                                                                                                                                             
              UPDATE tra_mae_icefa_issste                                                                                                     
              SET    tra_mae_icefa_issste.status = reg_1.pendiente                                                                            
              WHERE  correlativo = i_correlativo                                                                                              
                                                                                                                                             
              CONTINUE FOREACH                                                                                                                
                                                                                                                                             
          END IF                                                                                                                              
                                                                                                                                             
          SELECT "OK"                                                                                                                         
          FROM  cta_act_marca A                                                                                                               
          WHERE nss = x_n_seguro                                                                                                              
          AND   A.marca_cod = 5                                                                                                               
          AND   A.marca_causa  between 200 and 299                                                                                            
          GROUP BY 1                                                                                                                          
                                                                                                                                             
          IF STATUS <> NOTFOUND THEN                                                                                                          
                                                                                                                                             
           UPDATE tra_mae_icefa_issste                                                                                                        
           SET    tra_mae_icefa_issste.status = reg_1.rech_conv                                                                               
           WHERE  correlativo = i_correlativo                                                                                                 
                                                                                                                                             
           CONTINUE FOREACH                                                                                                                   
                                                                                                                                             
          END IF                                                                                                                              
                                                                                                                                             
          LET vnss            = x_n_seguro                                                                                                    
          LET vmarca_entra    = 260                                                                                                           
          LET vmarca_estado   = 0                                                                                                             
          LET vcodigo_rechazo = 0                                                                                                             
                                                                                                                                             
          SELECT user                                                                                                                         
          INTO   vusuario                                                                                                                     
          FROM   tab_afore_local                                                                                                              
                                                                                                                                             
                                                                                                                                             
          LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(","'",vnss    ,"'"   ,",",                                                            
          	  				                                        vmarca_entra   ,",",                                                                                               
          				                                            i_correlativo  ,",",                                                                                           
          				                                            vmarca_estado  ,",",                                                                                           
               		                                            vcodigo_rechazo,",",                                                                         
          		                                                vmarca_causa,",",                                                                                  
          			                                              "'',",                                                                                                   
          			                                              "'",vusuario,"')"                                                                                       
                                                                                                                                             
                                                                                                                                             
          LET ejecuta = ejecuta CLIPPED                                                                                                       
                                                                                                                                             
          PREPARE clausula_spl21 FROM ejecuta                                                                                                   
                                                                                                                                             
          DECLARE cursor_marca21 CURSOR FOR clausula_spl21                                                                                        
          OPEN    cursor_marca21                                                                                                                   
          FETCH   cursor_marca21 INTO xcodigo_marca, xcodigo_rechazo                                                                              
          CLOSE   cursor_marca21                                                                                                                  
                                                                                                                                          
             IF xcodigo_rechazo <> 0 THEN                                                                                                        
                                                                                                                                                
                UPDATE tra_mae_icefa_issste                                                                                                      
                SET    tra_mae_icefa_issste.status = reg_1.rech_conv                                                                             
                WHERE  correlativo = i_correlativo                                                                                               
                                                                                                                                                
                CONTINUE FOREACH                                                                                                                 
                                                                                                                                                
             END IF                                                                                                                              
                                                                                                                                          
             LET cont_reg = cont_reg + 1                                                                                            
                                                                                                                                 
             IF cont_reg > 10000 THEN                                                                                             
                EXIT FOREACH                                                                                                     
             END IF                                                                                                               
                                                                                                                                 
             DISPLAY "NRO. DE REG. A ENVIAR EN ARCHIVO OP. 01: ",cont_reg at 5,8                                                               
                                                                                                                                   
             SELECT  B.n_unico  ,                                                                                                   
                     B.n_seguro ,                                                                                                   
                     B.n_rfc    ,                                                                                                   
                     B.paterno  ,                                                                                                   
                     B.materno  ,                                                                                                   
                     B.nombres  ,                                                                                                   
                     B.fentcons                                                                                                     
             INTO                                                                                                                   
                     reg_det_sol_trasp.n_unico  ,                                                                                   
                     reg_det_sol_trasp.n_seguro ,                                                                                   
                     reg_det_sol_trasp.rfc      ,                                                                                   
                     reg_det_sol_trasp.paterno  ,                                                                                   
                     reg_det_sol_trasp.materno  ,                                                                                   
                     reg_det_sol_trasp.nombres  ,                                                                                   
                     f_fentcons                                                                                                     
             FROM    afi_mae_afiliado B                                                                                             
             WHERE   B.n_seguro = x_n_seguro                                                                                        
                                                                                                                                   
             LET reg_det_sol_trasp.nombre_trab_icefa[01,40]  = c40_paterno                                                          
             LET reg_det_sol_trasp.nombre_trab_icefa[41,80]  = c40_materno                                                          
             LET reg_det_sol_trasp.nombre_trab_icefa[81,120] = c40_nombres                                                          
                                                                                                                                   
             IF f_fecha_solic_tra IS NULL THEN                                                                                      
                LET c10_fentcons = f_fentcons                                                                                       
                LET reg_det_sol_trasp.fech_recep_solic =  c10_fentcons[07,10],                                                      
                                                          c10_fentcons[01,02],                                                      
                                                          c10_fentcons[04,05]                                                       
                LET f_fech_recep_solic = f_fentcons                                                                                 
             ELSE                                                                                                                   
                LET c10_fecha_solic_tra = f_fecha_solic_tra                                                                         
                LET reg_det_sol_trasp.fech_recep_solic =c10_fecha_solic_tra[07,10],                                                 
                                                     c10_fecha_solic_tra[01,02],                                                    
                                                     c10_fecha_solic_tra[04,05]                                                     
                LET f_fech_recep_solic = f_fecha_solic_tra                                                                          
             END IF                                                                                                                 
                                                                                                                                   
             LET sw_1 = 1                                                                                                           
                                                                                                                                          
             #MOD. MLM-1314 Agosto 2012                                                                                             
             #VALIDACION campos llaves si VIENEN NULOS se Guardaran con ""                                                          
                                                                                                                                    
             IF x_n_seguro IS NULL THEN                                                                                             
                                                                                                                                    
                LET     x_n_seguro  =  ""                                                                                            
                                                                                                                                     
                UPDATE  tra_mae_icefa_issste                                                                                         
                SET     tra_mae_icefa_issste.n_seguro        = x_n_seguro                                                            
                WHERE   tra_mae_icefa_issste.correlativo     = i_correlativo                                                         
                                                                                                                                    
             END IF                                                                                                                 
                                                                                                                                    
             IF reg_det_sol_trasp.n_seguro_ent IS NULL THEN                                                                         
                                                                                                                                    
                LET  reg_det_sol_trasp.n_seguro_ent  =  ""                                                                           
                                                                                                                                     
                UPDATE  tra_mae_icefa_issste                                                                                         
                SET     tra_mae_icefa_issste.nss             = reg_det_sol_trasp.n_seguro_ent                                        
                WHERE   tra_mae_icefa_issste.correlativo     = i_correlativo                                                         
                                                                                                                                     
             END IF                                                                                                                 
                                                                                                                                           
             IF reg_det_sol_trasp.rfc_ent IS NULL THEN                                                                              
                                                                                                                                    
                LET  reg_det_sol_trasp.rfc_ent  =  ""                                                                                
                                                                                                                                     
                UPDATE  tra_mae_icefa_issste                                                                                         
                SET     tra_mae_icefa_issste.rfc             = reg_det_sol_trasp.rfc_ent                                             
                WHERE   tra_mae_icefa_issste.correlativo     = i_correlativo                                                         
                                                                                                                                     
             END IF                                                                                                                 
                                                                                                                                           
             IF reg_det_sol_trasp.id_procesar IS NULL THEN                                                                          
                                                                                                                                    
               LET  reg_det_sol_trasp.id_procesar  =  ""                                                                            
                                                                                                                                    
               UPDATE  tra_mae_icefa_issste                                                                                         
               SET     tra_mae_icefa_issste.id_procesar     = reg_det_sol_trasp.id_procesar                                         
               WHERE   tra_mae_icefa_issste.correlativo     = i_correlativo                                                         
                                                                                                                                    
             END IF                                                                                                                 
                                                                                                                                           
             IF reg_det_sol_trasp.num_ctrl_int_icefa IS NULL THEN                                                                   
                                                                                                                                    
                LET  reg_det_sol_trasp.num_ctrl_int_icefa  =  ""                                                                     
                                                                                                                                     
                UPDATE  tra_mae_icefa_issste                                                                                         
                SET     tra_mae_icefa_issste.nro_int_cta     = reg_det_sol_trasp.num_ctrl_int_icefa                                  
                WHERE   tra_mae_icefa_issste.correlativo     = i_correlativo                                                         
                                                                                                                                     
             END IF                                                                                                                 
                                                                                                                                    
             #FIN VALIDACION campos llaves si VIENEN NULOS se Guardaran con ""                                                      
                                                                                                                                    
             OUTPUT TO REPORT listado_2(reg_det_sol_trasp.*) #2                                                                     
                                                                                                                                          
                                                                                                                                          
             UPDATE tra_mae_icefa_issste                                                                                                 
             SET    tra_mae_icefa_issste.status          = reg_1.enviada      ,                                                          
                    tra_mae_icefa_issste.lote_genera     = s_lotes_num        ,                                                          
                    tra_mae_icefa_issste.fecha_genera    = TODAY              ,                                                          
                    tra_mae_icefa_issste.fecha_proceso   = TODAY              ,                                                          
                    tra_mae_icefa_issste.fecha_solic_tra = f_fech_recep_solic ,                                                          
                    tra_mae_icefa_issste.n_envios = tra_mae_icefa_issste.n_envios + 1                                                    
             WHERE  tra_mae_icefa_issste.correlativo = i_correlativo                                                                     
                                                                                                                                   
             IF cont_reg = 10000 THEN                                                                                               
                 EXIT FOREACH                                                                                                       
             END IF                                                                                                                 
       
       END IF #ACEPTADO < SI Existe EXPEDIENTE de IDENTIFICACION DEL TRABAJADOR >                                                                                                                                  
          
          LET g_cod_result_operac =   NULL  #MLM-3028
          LET g_pasa              =   "00"  #MLM-3028          
          
          LET g_exp_n_folio       =   NULL  #MLM-3028 Exp
          LET g_exp_n_seguro      =   NULL  #MLM-3028 Exp   
          LET g_exp_tipo_solic    =   NULL  #MLM-3028 Exp 
          
          LET g_ls_afi_ctr        =   0     #MLM-3028 Exp 
          LET g_ls_afi_ref        =   0     #MLM-3028 Exp
                                                                                                                                     
    END FOREACH 
    
    #-- Codigo Común para todos los Clientes.  --#
    IF sw_1 = 0 THEN #MLM-3028Bis  NO EXISTEN REGISTROS A ENVIAR OP 01
    	
      #--#MLM-3028Bis--#                                                       
       DISPLAY "Rep. Det. Aceptados, Rechazados Expedientes" AT 11,2           
       DISPLAY "ARCHIVO:",g_lista at 12,2                                      
    	 
    	 INITIALIZE  cat  TO NULL
          	 
       LET g_borra_archs = 'rm ',G_LISTA_1 
       RUN g_borra_archs                  
                                          
    	 INITIALIZE g_borra_archs TO NULL    
    	                                     
    	 LET g_borra_archs = 'rm ',G_LISTA_2 
    	 RUN g_borra_archs                   
    	                                     
    	 INITIALIZE g_borra_archs TO NULL    
    	                                     
    	 LET g_borra_archs = 'rm ',G_LISTA_3 
    	 RUN g_borra_archs                   
    	 #--FIN DE LA MODIFICACION  MLM-3028Bis--#    	 
    	 
       DISPLAY "NO SE ENCONTRARON REGISTROS A ENVIAR OP01" AT 19,1 ATTRIBUTE(REVERSE)
       SLEEP 3
       #--EXIT PROGRAM  MLM-3028Bis--

    END IF
    
    IF sw_1 = 0 THEN #MLM-3028Bis  NO EXISTEN REGISTROS A ENVIAR OP 01
       FINISH REPORT rpt_2
       EXIT PROGRAM	
    END IF 	   
    
    FINISH REPORT listado_2
    FINISH REPORT rpt_2 
    
END FUNCTION

FUNCTION genera_sum_sol_trasp()
#gsst--------------------------

LET reg_sum_sol_trasp.tipo_registro     = "09"
LET reg_sum_sol_trasp.cantidad_reg_det  = cont_reg

LET G_LISTA_3 = RUTA CLIPPED,"/SST"

START REPORT listado_3 TO G_LISTA_3
   OUTPUT TO REPORT listado_3(reg_sum_sol_trasp.*) #3
FINISH REPORT listado_3

END FUNCTION

REPORT listado_1(reg_cza_sol_trasp)
#1---------------------------------
    DEFINE reg_cza_sol_trasp RECORD
        tipo_registro         CHAR(02) ,
        ident_servicio        CHAR(02) ,
        ident_operacion       CHAR(02) ,
        tipo_ent_origen       CHAR(02) ,
        cve_ent_origen        CHAR(03) ,
        tipo_ent_destino      CHAR(02) ,
        ent_fed_envio_lote    CHAR(03) ,
        fech_presentacion     CHAR(08) ,
        consec_lote_dia       SMALLINT ,
        cve_mod_recepcion     CHAR(02)
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
            COLUMN 001,reg_cza_sol_trasp.tipo_registro      ,
            COLUMN 003,reg_cza_sol_trasp.ident_servicio     ,
            COLUMN 005,reg_cza_sol_trasp.ident_operacion    ,
            COLUMN 007,reg_cza_sol_trasp.tipo_ent_origen    ,
            COLUMN 009,reg_cza_sol_trasp.cve_ent_origen     ,
            COLUMN 012,reg_cza_sol_trasp.tipo_ent_destino   ,
            COLUMN 017,reg_cza_sol_trasp.ent_fed_envio_lote ,
            COLUMN 020,reg_cza_sol_trasp.fech_presentacion  ,
            COLUMN 028,reg_cza_sol_trasp.consec_lote_dia    USING"&&&",
            COLUMN 031,reg_cza_sol_trasp.cve_mod_recepcion  ,
            COLUMN 033,2   SPACES                           ,#--Nva Version
            COLUMN 035,9   SPACES                           ,#--Nva Version
            COLUMN 044,687 SPACES                            #--Nva Version

END REPORT

REPORT listado_2(reg_det_sol_trasp)
#2---------------- -----------------
    DEFINE reg_det_sol_trasp RECORD #loc #reg_det_sol_trasp
        tipo_registro         CHAR(002)     ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(002)     ,
        cve_recep_cuenta      CHAR(003)     ,
        tipo_ced_cuenta       CHAR(002)     ,
        cve_ced_cuenta        SMALLINT      ,
        orig_tipo_trasp       CHAR(002)     ,
        fech_presentacion     CHAR(008)     ,
        n_unico               CHAR(018)     ,
        n_seguro              CHAR(011)     ,
        rfc                   CHAR(013)     ,
        paterno               CHAR(040)     ,
        materno               CHAR(040)     ,
        nombres               CHAR(040)     ,
        cve_sector            CHAR(001)     ,
        fech_recep_solic      CHAR(008)     ,
        ident_lote_solic      CHAR(016)     ,
        n_seguro_ent          CHAR(011)     ,
        rfc_ent               CHAR(013)     ,
        id_procesar           CHAR(08)      ,
        num_ctrl_int_icefa    CHAR(030)     ,
        nombre_trab_icefa     CHAR(120)
    END RECORD

    DEFINE tipo_sol           SMALLINT

    OUTPUT
        PAGE LENGTH   1
	      LEFT MARGIN   0
	      RIGHT MARGIN  0
	      TOP MARGIN    0
	      BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW 
    
        #JIRA INV-2071 Septiembre 2013 Se Añade Clave Icefa 199
        #Cuentas Inactivas Canceladas Peis                          
                                                                    
        #Layout 020102 Detalle de Solicitud                         
        #id 28 Clave de la ICEFA en BDSARISSSTE Posic 445 - 447     
        #Alfanúmerico  Deberá ser siempre 

        LET   tipo_sol        =           NULL

        SELECT A.tipo_solicitud
        INTO tipo_sol
        FROM afi_mae_afiliado A
        WHERE A.n_seguro = reg_det_sol_trasp.n_seguro        
        	
        #Empieza Mod. Jira CPL-1607                         	
        #Se Añade Mas tipos de Solicitud,aparte de los T.S 8	
        
        IF (tipo_sol = 8)  OR   # 8   NO AFILIADO                             
           (tipo_sol = 12) OR   #12   REGISTRO INTERNET NO AFIL        
           (tipo_sol = 15) OR   #15   TRASPASO POR CURP                	
           (tipo_sol = 16) OR   #16   TRASP POR CURP INTRNET ORIG 72   	
           (tipo_sol = 18) THEN #18   TRASP POR CURP INTRNET ORIG 74   	
           	
           LET reg_det_sol_trasp.orig_tipo_trasp    =  63
           LET reg_det_sol_trasp.n_seguro           =  "           "
           LET reg_det_sol_trasp.cve_sector         =  2 #Sector PRIVADO
           
        ELSE #<> T,S 8,12,15,16,18
        	
           LET reg_det_sol_trasp.orig_tipo_trasp    =  61
           LET reg_det_sol_trasp.cve_sector         =  1 #Sector PUBLICO
        END IF #Fin Si es  8,12,15,16,18 o No es 

        PRINT
            COLUMN 001,reg_det_sol_trasp.tipo_registro      ,
            COLUMN 003,cont_reg          USING"&&&&&&&&&&"  ,#cont_servicio
            COLUMN 013,reg_det_sol_trasp.tipo_recep_cuenta  ,
            COLUMN 015,reg_det_sol_trasp.cve_recep_cuenta   ,
            COLUMN 018,"03"                                 ,
            COLUMN 020,"001"                                ,
            COLUMN 023,reg_det_sol_trasp.orig_tipo_trasp USING "&&" ,
            COLUMN 025,reg_det_sol_trasp.fech_presentacion  ,
            COLUMN 033,8 SPACES                             ,
            COLUMN 041,reg_det_sol_trasp.n_unico            ,
            COLUMN 059,reg_det_sol_trasp.n_seguro           ,
            COLUMN 070,15 SPACES                            ,
            COLUMN 085,reg_det_sol_trasp.rfc                ,
            COLUMN 098,reg_det_sol_trasp.paterno            ,
            COLUMN 138,reg_det_sol_trasp.materno            ,
            COLUMN 178,reg_det_sol_trasp.nombres            ,
            COLUMN 218,3 SPACES                             ,          
            COLUMN 221,reg_det_sol_trasp.cve_sector         ,
            COLUMN 222,10 SPACES                            ,
            COLUMN 232,reg_det_sol_trasp.fech_recep_solic   ,
            COLUMN 240,reg_det_sol_trasp.ident_lote_solic   ,
            COLUMN 256,reg_det_sol_trasp.id_procesar        ,
            COLUMN 264,7 SPACES                             ,
            COLUMN 271,reg_det_sol_trasp.n_seguro_ent       ,
            COLUMN 282,reg_det_sol_trasp.rfc_ent            ,
            COLUMN 295,reg_det_sol_trasp.num_ctrl_int_icefa ,
            COLUMN 325,reg_det_sol_trasp.nombre_trab_icefa  ,
            COLUMN 445,reg_det_sol_trasp.cve_ced_cuenta USING"&&&",#JIRA INV-2071 Septiembre 2013 Se Añade Clave Icefa 199 Cuentas Inactivas Canceladas Peis
            COLUMN 448,135 SPACES                           ,      #              por lo cual tendría que viajar la Icefa Cod                
            COLUMN 583,2   SPACES                           ,      #Icefa 199 Ctas. Ind. Inactivas -Peis
            COLUMN 585,15  SPACES                           ,      #Icefa 178 PENSIONISSSTE
            COLUMN 600,131 SPACES                           

END REPORT

REPORT listado_3(reg_sum_sol_trasp)
#3---------------------------------
    DEFINE reg_sum_sol_trasp RECORD #loc #reg_sum_sol_trasp
        tipo_registro        CHAR(02) ,
        cantidad_reg_det     INTEGER
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
            COLUMN 001,reg_sum_sol_trasp.tipo_registro    ,
            COLUMN 003,reg_sum_sol_trasp.cantidad_reg_det USING"&&&&&&&&&" ,
            COLUMN 012,719 SPACES
END REPORT

REPORT rpt_2  ( r_exp_n_seguro,r_exp_n_folio,r_exp_tipo_solic,r_ls_afi_ctr,r_desc_exp,r_stat) #MLM-3028 Exp
#r1-----------------------------------------      
DEFINE  r_exp_n_seguro       CHAR(011)       #MLM-3028 Exp                         
DEFINE  r_exp_n_folio        DECIMAL(10,0)   #MLM-3028 Exp
DEFINE  r_exp_tipo_solic                  ,  #MLM-3028 Exp
        r_ls_afi_ctr         SMALLINT        #MLM-3028 Exp
DEFINE  r_desc_exp           CHAR(014)       #MLM-3028 Exp
DEFINE  r_stat               INTEGER         #MLM-3028 Exp
                                                  
OUTPUT                                            
   TOP MARGIN     0                                   
   BOTTOM MARGIN  0                                
   LEFT MARGIN    0                                  
   RIGHT MARGIN   0                                 
   ORDER BY  r_ls_afi_ctr,r_stat,r_exp_n_seguro
                                                  
FORMAT                                            
PAGE HEADER                                       
                                                  
PRINT                                             
    COLUMN  001,"NSS"                ,                             
    COLUMN  018,"N_FOLIO"            ,
    COLUMN  029,"TIPO SOLICITUD"     ,
    COLUMN  045,"COD. EXP."          ,
    COLUMN  057,"STATUS EXP."        ,
    COLUMN  070,"STATUS"        
    
          
                                                  
ON EVERY ROW                                      
                                                  
   PRINT    r_exp_n_seguro   CLIPPED,"|",                
            r_exp_n_folio    CLIPPED,"|",       
            r_exp_tipo_solic CLIPPED,"|",      
            r_ls_afi_ctr     CLIPPED,"|",
            r_desc_exp       CLIPPED,"|",
            r_stat           CLIPPED,"|"
                                                  
END REPORT             