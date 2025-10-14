##############################################################################
#Owner             => E.F.P.
#Programa TRAB001  => GENERA SOLICITUD DE TRASPASO DE ICEFAS
#Fecha creacion    => 20 DE AGOSTO DE 1997
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => MODIFICACIONES CUO FEBRERO 2015.
#                  => Unica Versión CPL-INV-MLM
#                     MLM-3028
#                     FEB-2015 Implementacion CUO para revisar si la Solcitud
#                     que se está enviando tiene Expediente Aceptado ó No ?.
#                     Solo se Enviarán los Aceptados.
#Sistema           => TRA-ICE-IMSS
##############################################################################
DATABASE safre_af
GLOBALS
DEFINE g_desc_exp       CHAR(014)#MLM-3028 Exp  
DEFINE g_eje_fne        CHAR(200)#MLM-3028 Exp  
DEFINE g_ruta_y_arch            ,#MLM-3028
       g_lista          CHAR(100)
DEFINE gnom_arch_gen    CHAR(100)
DEFINE v_tipo_sol       LIKE afi_mae_afiliado.tipo_solicitud
DEFINE vmarca_causa     SMALLINT
DEFINE ejecuta          CHAR(300)
DEFINE xcodigo_marca            ,
       xcodigo_rechazo  SMALLINT
DEFINE vnss             CHAR(11),
       vmarca_entra             ,
       vmarca_estado            ,
       vcodigo_rechazo          ,
       g_ls_afi_ctr             ,#MLM-3028 Exp
       g_ls_afi_ref     SMALLINT,#MLM-3028 Exp
       vusuario                 ,
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
       paterno               CHAR(040)     ,
       materno               CHAR(040)     ,
       nombres               CHAR(040)     ,
       cve_sector            CHAR(001)     ,
       fech_recep_solic      CHAR(008)     ,
       ident_lote_solic      CHAR(016)     ,
       n_seguro_ent          CHAR(011)     ,
       rfc_ent               CHAR(013)     ,
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
       pendiente             SMALLINT
             END RECORD

DEFINE #glo #date
       f_fech_recep_solic    ,
       f_fentcons            ,
       f_fecha_solic_tra     ,
       HOY                   DATE
 
DEFINE #glo #char
       x_n_seguro            CHAR(011) ,
       RUTA                            ,
       G_LISTA_1                       ,
       G_LISTA_2                       ,
       G_LISTA_3                       ,
       G_LISTA_4             CHAR(100) ,
       usuario               CHAR(008) ,
       c10_fecha_solic_tra   CHAR(010) ,
       c10_fentcons          CHAR(010) ,
       c40_paterno                     ,
       c40_materno                     ,
       c40_nombres           CHAR(040) ,
       enter    	           CHAR(001) ,
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
       i_correlativo         INTEGER     
       
DEFINE #glo #decimal         #MLM-3028
       cont_reg                          ,
       cont_reg_AcepExp                  ,
       g_exp_n_folio                     ,#MLM-3028 Exp
       cont_reg_RechExp      DECIMAL(10,0)
### anexe variables
DEFINE folio_val LIKE safre_tmp:tra_det_automatico.folio_interno
DEFINE i         INTEGER    
DEFINE reg RECORD LIKE tra_mae_icefa.* 
	
DEFINE env_sol RECORD
       tipo_criterio  LIKE safre_tmp:tra_det_automatico.tipo_criterio,
       folio_interno  LIKE safre_tmp:tra_det_automatico.folio_interno,
       cve_ced_cuenta LIKE safre_tmp:tra_det_automatico.cve_ced_cuenta,
       estado         LIKE safre_tmp:tra_det_automatico.estado
              END RECORD 
              
DEFINE est_acept      CHAR(020) 
DEFINE g_param_taa    RECORD LIKE seg_modulo.* 

DEFINE rep RECORD 
       cve_ced_cuenta LIKE safre_tmp:tra_det_automatico.cve_ced_cuenta,
       tipo_criterio  LIKE safre_tmp:tra_det_automatico.tipo_criterio,
       total          INTEGER
           END RECORD
           
DEFINE raz_social   LIKE tab_afore_local.razon_social
DEFINE cod_afore    LIKE tab_afore_local.codigo_afore
DEFINE g_listita                 , 
       permisos             CHAR(100) 
DEFINE proc_edo_cod         CHAR(20)
DEFINE g_borra_archs        CHAR(1000) 
DEFINE g_exp_n_seguro       CHAR(011)#MLM-3028 Exp
DEFINE g_pasa                       ,#MLM-3028
       g_cod_result_operac  CHAR(02) 
DEFINE g_stat               INTEGER  #MLM-3028
END GLOBALS

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST  ,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT
    
    CALL STARTLOG(FGL_GETENV("USER")||".TRAB001.log")#CPL-1848 
    
    WHENEVER ERROR CONTINUE 
    
       DATABASE safre_tmp
       
           DROP TABLE res_env_sol
           CREATE  TABLE res_env_sol(    tipo_criterio  SMALLINT  ,
                                         folio_interno  INTEGER   ,
                                         cve_ced_cuenta CHAR(03)  ,
                                         estado         SMALLINT)
           CREATE INDEX rrr ON res_env_sol(folio_interno,tipo_criterio)
           CREATE INDEX rrr1 ON res_env_sol(folio_interno)
				  
    WHENEVER ERROR STOP


DATABASE safre_af
    CALL init()
    OPEN WINDOW trab0011 AT 4,4 WITH FORM "TRAB0011" ATTRIBUTE(BORDER)
    DISPLAY" TRAB001   GENERA SOLICITUD DE TRASPASO ICEFA-AFORE IMSS                       " AT 3,1 ATTRIBUTE(REVERSE)   

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
    
    #-- CPL-1848 ENERO 2015                                                      ====#    
    #---BORRA ARCHIVOS QUE DEJA DE PASO PARA DEPURAR LA RUTA  --#
    
    INITIALIZE g_borra_archs  TO NULL                                       
    LET g_borra_archs = 'rm -f ',G_LISTA_1 CLIPPED,' ',G_LISTA_2 CLIPPED,   
                        ' ',G_LISTA_3 CLIPPED,' ',G_LISTA_4 CLIPPED                              
                                                                            
    LET g_borra_archs =   g_borra_archs CLIPPED                             
                                                                            
    RUN g_borra_archs                                                       
    
    #--                                                                          ====#
    
    #pp
    CALL genera_cza_sol_trasp()   #gcst
    CALL genera_det_sol_trasp()   #gdst
    CALL genera_sum_sol_trasp()   #gsst
    #CALL envio_sar92() 

    LET gnom_arch_gen        = RUTA CLIPPED,"/SOL",
                               HOY USING"YYYYMMDD","-", HORA[1,2],HORA[4,5]

    LET gnom_arch_gen        = gnom_arch_gen CLIPPED

    DISPLAY "OP.01 A ENVIAR" AT 15,2
    DISPLAY "ARCHIVO:  ",gnom_arch_gen at 16,2
    
    #--MLM-3028  --#    
    DISPLAY "Rep. Det. Aceptados, Rechazados Expedientes" AT 11,2           
    DISPLAY "ARCHIVO:",g_lista at 12,2      
    #--FIN DE LA MODIFICACION  --#
    
    PROMPT "PROCESO FINALIZADO...PRESIONE <ENTER> PARA GENERAR ARCHIVO"
            FOR CHAR enter
    
    INITIALIZE cat  TO NULL
    LET cat = "cat ",G_LISTA_1 CLIPPED," ",G_LISTA_2 CLIPPED," ",
                     G_LISTA_3 CLIPPED," > ",RUTA CLIPPED,"/SOL",
                     HOY USING"YYYYMMDD","-", HORA[1,2],HORA[4,5]

    RUN cat
    
    #-- CPL-1848 ENERO 2015                                                      ====# 
    #---BORRA ARCHIVOS QUE DEJA DE PASO PARA DEPURAR LA RUTA  --#                      
     
    INITIALIZE  cat  TO NULL         
                                                                          
    INITIALIZE g_borra_archs  TO NULL                                     
    LET g_borra_archs = 'rm -f ',G_LISTA_1 CLIPPED,' ',G_LISTA_2 CLIPPED, 
                        ' ',G_LISTA_3 CLIPPED,' ',G_LISTA_4 CLIPPED                             
                                                                          
    LET g_borra_archs =   g_borra_archs CLIPPED                          
                                                                         
    RUN g_borra_archs                                                    
                                                                         
    #Se da Permisos al Archivo Generado                                  
    INITIALIZE cat  TO NULL                                              
    LET cat = "chmod 777 ",gnom_arch_gen   
    RUN cat                              
    
    #---                                                                         ----#
    
    UPDATE tab_lote
    SET    lotes_num   = s_lotes_num
    WHERE  lotes_fecha = HOY
    AND    lotes_cod   = 6 

    CLOSE WINDOW trab0011
END MAIN

FUNCTION init()
#--------------
    DEFINE #loc #char
        c10_HOY               CHAR(010)

    LET HOY     = TODAY
    LET c10_HOY = TODAY
    LET HORA    = TIME
    LET c8_HOY  = c10_HOY[07,10],c10_HOY[01,02],c10_HOY[4,5]

    SELECT  A.ruta_envio
    INTO    RUTA
    FROM    seg_modulo A
    WHERE   A.modulo_cod = "tra"
    
    #-- JIRA MLM-1990 JULIO 2013 ---#  
                                       
    LET G_LISTA_1 = RUTA CLIPPED,"/CST"
    LET G_LISTA_1 = G_LISTA_1 CLIPPED  
                                       
    LET G_LISTA_2 = RUTA CLIPPED,"/DST"
    LET G_LISTA_2 = G_LISTA_2 CLIPPED  
                                       
    LET G_LISTA_3 = RUTA CLIPPED,"/SST"
    LET G_LISTA_3 = G_LISTA_3 CLIPPED  

    LET G_LISTA_4 = RUTA CLIPPED,"/A"
    LET G_LISTA_4 = G_LISTA_4 CLIPPED           
                                       
    #--                           --#
    
    SELECT lotes_num          ,
           lotes_correlativo 
    INTO   s_lotes_num ,
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

    SELECT  codigo_afore   ,
            USER
    INTO    s_codigo_afore ,
            usuario
    FROM    tab_afore_local

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

    SELECT  *
    INTO    g_param_taa.*
    FROM    seg_modulo
    WHERE   modulo_cod = 'tra' 
            
    SELECT  estado_cod
    INTO    proc_edo_cod 
    FROM    tra_aut_estado
    WHERE   estado_descripcion = "ENVIADA AL MAESTRO DE ICEFAS " 
    LET     proc_edo_cod = 30
    
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
    LET reg_cza_sol_trasp.tipo_ent_destino   = "07"
    LET reg_cza_sol_trasp.ent_fed_envio_lote = "009"
    LET reg_cza_sol_trasp.fech_presentacion  = c8_HOY
    LET reg_cza_sol_trasp.consec_lote_dia    = s_lotes_num
    LET reg_cza_sol_trasp.cve_mod_recepcion  = "02"

    LET G_LISTA_1 = RUTA CLIPPED,"/CST"
    START REPORT listado_1 TO G_LISTA_1
        OUTPUT TO REPORT listado_1(reg_cza_sol_trasp.*) #1
    FINISH REPORT listado_1
    
END FUNCTION

FUNCTION genera_det_sol_trasp()
#gdst--------------------------

DEFINE ee char(050)

    DECLARE cur_1 CURSOR FOR
    	
       SELECT  A.n_seguro        ,
               A.icefa_cod       ,#cve_ced_cuenta
               A.nss             ,#n_seguro_ent
               A.rfc             ,#rfc_ent
               A.nro_int_cta     ,#num_ctrl_int_icefa
               A.paterno         ,
               A.materno         ,
               A.nombres         ,#nombre_trab_icefa
               A.fecha_captura   , #se sustituye por fech_solic_tra allianz
               A.correlativo     ,
               A.status           #MLM-3028  
       FROM    tra_mae_icefa A 
       WHERE   A.status  in (reg_1.capturada, #STATUS = 1  'CONFIRMADA'
                             reg_1.rech_conv, #STATUS = 21 'RECH_CONV'				                         
                             reg_1.reenviada) #STATUS = 9  'REENVIADA'
       
       ORDER BY A.n_seguro
       
       LET reg_det_sol_trasp.tipo_registro     = "02"
       LET reg_det_sol_trasp.tipo_recep_cuenta = "01"
       LET reg_det_sol_trasp.cve_recep_cuenta  = s_codigo_afore
       LET reg_det_sol_trasp.tipo_ced_cuenta   = "07"
       LET reg_det_sol_trasp.orig_tipo_trasp   = "01"
       LET reg_det_sol_trasp.fech_presentacion = c8_HOY
       LET reg_det_sol_trasp.cve_sector        = "1"
       
       LET reg_det_sol_trasp.ident_lote_solic  = "01",
                                                 s_codigo_afore USING"&&&",
                                                 c8_HOY,
                                                 s_lotes_num    USING"&&&"
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
                          usuario CLIPPED,".EXPED-SARIMSS_",                             
                          TODAY USING "YYYYMMDD","_",g_hora CLIPPED                                          
      
       LET   g_lista  =      g_lista  CLIPPED                                
       START REPORT rpt_2 TO g_lista                                  
       #FIN MOD MLM-3028
       
    FOREACH cur_1 INTO x_n_seguro                           ,
                       reg_det_sol_trasp.cve_ced_cuenta     ,
                       reg_det_sol_trasp.n_seguro_ent       ,
                       reg_det_sol_trasp.rfc_ent            ,
                       reg_det_sol_trasp.num_ctrl_int_icefa ,
                       c40_paterno                          ,
                       c40_materno                          ,
                       c40_nombres                          ,#nombre_trab_icefa
                       f_fecha_solic_tra                    ,
                       i_correlativo                        ,
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
       
       IF g_pasa   = "01" THEN#ACEPTADO < SI Existe EXPEDIENTE de IDENTIFICACION DEL TRABAJADOR >                                                                                                                            
       	
          SELECT A.tipo_solicitud
          INTO   v_tipo_sol 
          FROM   afi_mae_afiliado A
          WHERE  A.n_seguro = x_n_seguro
          
          IF v_tipo_sol = 5 THEN
          
             UPDATE tra_mae_icefa
             SET    status = 30
             WHERE  correlativo = i_correlativo
          
             CONTINUE FOREACH
          END IF
          
          SELECT "OK"
          FROM  cta_act_marca A
          WHERE nss = x_n_seguro 
          AND   A.marca_cod = 120
          GROUP BY 1
          
          IF STATUS <> NOTFOUND THEN
          
             UPDATE tra_mae_icefa 
             SET    status = 21
             WHERE  correlativo = i_correlativo
             
             CONTINUE FOREACH
          
          END IF
          
          LET vnss            = x_n_seguro
          LET vmarca_entra    = 250
          LET vmarca_estado   = 0
          LET vcodigo_rechazo = 0
          
          SELECT USER
          INTO   vusuario 
          FROM   tab_afore_local
          GROUP BY 1
          
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
          OPEN cursor_marca
          FETCH cursor_marca INTO xcodigo_marca, xcodigo_rechazo
          CLOSE cursor_marca
          
          IF xcodigo_rechazo <> 0 THEN
          
             UPDATE tra_mae_icefa 
             SET    status = 21
             WHERE  correlativo = i_correlativo
             
             CONTINUE FOREACH
          
          END IF
          
          LET cont_reg = cont_reg + 1  
          DISPLAY "NRO. DE REG. A ENVIAR EN ARCHIVO OP. 01: ",cont_reg at 05,8
          
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
             LET reg_det_sol_trasp.fech_recep_solic =c10_fentcons[07,10],
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
          #---                                                                       --#             
          #MOD. EFPS-208  Agosto 2012                                    
          #VALIDACION campos llaves si VIENEN NULOS se Guardaran con "" 
          
          IF reg_det_sol_trasp.n_seguro IS NULL THEN                                     
                                                                         
             LET     reg_det_sol_trasp.n_seguro  =  ""                                    
                                                                          
             UPDATE  tra_mae_icefa                                
             SET     tra_mae_icefa.n_seguro        =       reg_det_sol_trasp.n_seguro    
             WHERE   tra_mae_icefa.correlativo     =       i_correlativo 
                                                                         
          END IF                                                         
          
          IF reg_det_sol_trasp.cve_ced_cuenta IS NULL THEN                                     
                                                                         
             LET     reg_det_sol_trasp.cve_ced_cuenta  =  ""                                    
                                                                          
             UPDATE  tra_mae_icefa                                
             SET     tra_mae_icefa.icefa_cod       =       reg_det_sol_trasp.cve_ced_cuenta    
             WHERE   tra_mae_icefa.correlativo     =       i_correlativo 
                                                                    
          END IF                                        
              
          IF reg_det_sol_trasp.n_seguro_ent IS NULL THEN                                   
                                                                                               
             LET     reg_det_sol_trasp.n_seguro_ent  =  ""                                      
                                                                                                  
             UPDATE  tra_mae_icefa                                                                
             SET     tra_mae_icefa.nss             =       reg_det_sol_trasp.n_seguro_ent       
             WHERE   tra_mae_icefa.correlativo     =       i_correlativo                        
                                                                                               
          END IF                                                                               
              
          IF reg_det_sol_trasp.rfc_ent IS NULL THEN                                      
                                                                                              
             LET     reg_det_sol_trasp.rfc_ent  =  ""                                     
                                                                                               
             UPDATE  tra_mae_icefa                                                             
             SET     tra_mae_icefa.rfc             =       reg_det_sol_trasp.rfc_ent      
             WHERE   tra_mae_icefa.correlativo     =       i_correlativo                       
                                                                                               
          END IF   
          
          IF reg_det_sol_trasp.num_ctrl_int_icefa IS NULL THEN                                 
                                                                                   
             LET     reg_det_sol_trasp.num_ctrl_int_icefa  =  ""                               
                                                                                    
             UPDATE  tra_mae_icefa                                                                                                                             
             SET     tra_mae_icefa.nro_int_cta     =       reg_det_sol_trasp.num_ctrl_int_icefa   
             WHERE   tra_mae_icefa.correlativo     =       i_correlativo                
                                                                                   
          END IF
          
          #---                                                                        --#
          
          OUTPUT TO REPORT listado_2(reg_det_sol_trasp.*) #2
          
          INSERT INTO est_0961 VALUES (reg_det_sol_trasp.cve_ced_cuenta,
                                       reg_det_sol_trasp.fech_presentacion,
                                       reg_det_sol_trasp.fech_presentacion)
          
          UPDATE tra_mae_icefa
          SET    tra_mae_icefa.status            =      reg_1.enviada      ,
                 tra_mae_icefa.lote_genera       =      s_lotes_num        ,
                 tra_mae_icefa.fecha_genera      =      TODAY              ,
                 tra_mae_icefa.fecha_proceso     =      TODAY              ,
                 tra_mae_icefa.fecha_solic_tra   =      f_fech_recep_solic ,
                 tra_mae_icefa.n_envios          =      tra_mae_icefa.n_envios + 1
          WHERE  tra_mae_icefa.correlativo       =      i_correlativo
          
          
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

    IF sw_1 = 0 THEN #MLM-3028Bis  NO EXISTEN REGISTROS A ENVIAR OP 01
    	  
    	  #--#MLM-3028Bis--#    
        DISPLAY "Rep. Det. Aceptados, Rechazados Expedientes" AT 11,2           
        DISPLAY "ARCHIVO:",g_lista at 12,2      
        
        INITIALIZE  cat  TO NULL         
                                                                          
        INITIALIZE g_borra_archs  TO NULL                                     
        LET g_borra_archs = 'rm -f ',G_LISTA_1 CLIPPED,' ',G_LISTA_2 CLIPPED, 
                            ' ',G_LISTA_3 CLIPPED,' ',G_LISTA_4 CLIPPED                             
                                                                              
        LET g_borra_archs =   g_borra_archs CLIPPED                          
                                                                             
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
            COLUMN 033,698 SPACES
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
        num_ctrl_int_icefa    CHAR(030)     ,
        nombre_trab_icefa     CHAR(120)
    END RECORD

    OUTPUT
        PAGE LENGTH   1
	      LEFT MARGIN   0
	      RIGHT MARGIN  0
	      TOP MARGIN    0
	      BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW  
       
        #Modificacion Jira INV - 2445                                              
        #ID  7    Origen /  tipo del Traspaso   Pocision  023 ,024                 
        #si  el registro a Enviar empieza en su primer Carácter con "I" Entonces   
        #se enviará con  Clave 42 de lo Contrario se enviaría con 01 Afiliado.     
                                                                                   
        #ID 11  NSS del trabajador según AFORE receptora Pocision 059,069          
        #si el NSS está registrado en BDNSAR ( Solo trabajadores Afiliados )       
        #se enviariá el NSS , si el NSS pertenece a un Trabajador NO AFILIADO      
        #no se enviará datos se enviarían 11 Espacios.                             
                                                                                   
                                                                                   
        IF ( reg_det_sol_trasp.n_seguro[1] = "I" )  THEN  # NSS NO AFILIADO        
                                                                                   
           LET reg_det_sol_trasp.orig_tipo_trasp   = "42"                          
           LET reg_det_sol_trasp.n_seguro          =  11 SPACES                    
                                                                                   
        END IF    
    
        PRINT
            COLUMN 001,reg_det_sol_trasp.tipo_registro      ,
            COLUMN 003,cont_reg          USING"&&&&&&&&&&"  ,#cont_servicio
            COLUMN 013,reg_det_sol_trasp.tipo_recep_cuenta  ,
            COLUMN 015,reg_det_sol_trasp.cve_recep_cuenta   ,
            COLUMN 018,reg_det_sol_trasp.tipo_ced_cuenta    ,
            COLUMN 020,reg_det_sol_trasp.cve_ced_cuenta USING"&&&" ,
            COLUMN 023,reg_det_sol_trasp.orig_tipo_trasp    ,
            COLUMN 025,reg_det_sol_trasp.fech_presentacion  ,
            COLUMN 041,reg_det_sol_trasp.n_unico            ,
            COLUMN 059,reg_det_sol_trasp.n_seguro           ,
            COLUMN 085,reg_det_sol_trasp.rfc                ,
            COLUMN 098,reg_det_sol_trasp.paterno            ,
            COLUMN 138,reg_det_sol_trasp.materno            ,
            COLUMN 178,reg_det_sol_trasp.nombres            ,
            COLUMN 221,reg_det_sol_trasp.cve_sector         ,
            COLUMN 232,reg_det_sol_trasp.fech_recep_solic   ,
            COLUMN 240,reg_det_sol_trasp.ident_lote_solic   ,
            COLUMN 271,reg_det_sol_trasp.n_seguro_ent       ,
            COLUMN 282,reg_det_sol_trasp.rfc_ent            ,
            COLUMN 295,reg_det_sol_trasp.num_ctrl_int_icefa ,
            COLUMN 325,reg_det_sol_trasp.nombre_trab_icefa  ,
            COLUMN 445,286 SPACES
            
            #INV - 2445
            #INICIALIZACION DE VARIABLES                            
                                                                    
            INITIALIZE   reg_det_sol_trasp.orig_tipo_trasp  TO NULL 
            INITIALIZE   reg_det_sol_trasp.n_seguro         TO NULL
                                                                    
            #FIN DE   INICIALIZACION DE VARIABLES             
            
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
            COLUMN 011,719 SPACES
END REPORT

##############################################################################
REPORT rep_env_sol(rep,folio_val)
DEFINE folio_val LIKE safre_tmp:tra_det_automatico.folio_interno
DEFINE rep RECORD 
       cve_ced_cuenta LIKE safre_tmp:tra_det_automatico.cve_ced_cuenta,
       tipo_criterio  LIKE safre_tmp:tra_det_automatico.tipo_criterio,
       total          INTEGER
           END RECORD
DEFINE icefa LIKE safre_tmp:tra_det_automatico.cve_ced_cuenta

OUTPUT
   TOP MARGIN    1
   BOTTOM MARGIN 0
   LEFT MARGIN   0
   RIGHT MARGIN  0
   PAGE LENGTH  60

   FORMAT
        PAGE HEADER
        SELECT codigo_afore,razon_social
        INTO   cod_afore,raz_social
        FROM   tab_afore_local

        PRINT COLUMN 70,"Pagina:",PAGENO USING "<<<<"
        PRINT COLUMN 01, "_______________________________________________________________________________"
        PRINT COLUMN 01,"TRAB001",
              COLUMN 23,"REPORTE GENERAL DE ENVIO DE SOLICITUDES",
              COLUMN 68,TODAY USING "dd-mm-yyyy"
        PRINT COLUMN 24,"         TRASPASO ICEFA-AFORE IMSS            "
        PRINT COLUMN 01,cod_afore USING "&&&","  ",raz_social CLIPPED
        PRINT COLUMN 01, "_______________________________________________________________________________"

        PRINT 

        BEFORE GROUP OF folio_val

           SELECT UNIQUE (cve_ced_cuenta)
           INTO   icefa   
           FROM   safre_tmp:res_env_sol
           WHERE  folio_interno = folio_val

           PRINT COLUMN 01, "_______________________________________________________________________________"

--- LETRA PEQUEÑA PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'      
--- NEGRITA Y GRANDE PRINT '\033e\033(s218T\033(s9H\033(s7B'
--- PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'      

           PRINT COLUMN 01,"FOLIO: ",folio_val USING "#####",
                 COLUMN 08,"    ","ICEFA: ",icefa CLIPPED 
           PRINT COLUMN 01, "_______________________________________________________________________________"

           PRINT COLUMN 25,"CRITERIO",
                 COLUMN 50,"TOTAL"
           PRINT 

       ON EVERY ROW

          PRINT COLUMN 22,rep.tipo_criterio,
                COLUMN 42,rep.total
          PRINT

       AFTER GROUP OF folio_val

           PRINT COLUMN 01, "_______________________________________________________________________________"

           PRINT COLUMN 22,"TOTAL FOLIO: ",folio_val USING "#####",
                 COLUMN 46,GROUP SUM(rep.total) USING "###,###"
       PRINT  
       PRINT 

    ON LAST ROW
        SKIP 4 LINES
        PRINT COLUMN 2, "TOTAL DE REGISTROS ENVIADOS: ",
        SUM(rep.total) USING "<<<<"

    PAGE TRAILER
        SKIP 2 LINE
        PAUSE "Presione enter para continuar...."

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