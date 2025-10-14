#################################################################################
#Proyecto:     => SISTEMA DE AFORES.( MEXICO )                                  #
#Programa:     => RETC762 - PROGRAMA BATCH PARA IDENTIFICACION DE REGISTROS A   #
#                           REINVERTIR                                          #
#Autor:        => Daniel Buendia, UPGENIA                                       #
#Fecha:        => Diciembre 07, 2011                                            #
#Sistema:      => RET                                                           #
#------------------------------MODIFICACIONES-----------------------------------#
#Requerimiento     => MLM-1241                                                  #
#Fecha y Autor     => 07-Ago-2012 Alejandro Chagoya Salazar                     #
#Descripcion       => Se agrega validacion para excluir cuentas con marca de    #
#                  => Trasferencia                                              #
#################################################################################
#Modificacion      => Se modifica query y subquery hacia ret_solicitud_saldo    #
#Autor             => Carlos Trejo Meixueiro                                    #
#Fecha             => 20 Junio 2013                                             #
#Requerimiento     => MLM-1975                                                  #
#################################################################################
#Modificacion      => La generacion del log debido problemas en producción      #
#Autor             => Carlos Trejo Meixueiro                                    #
#Fecha             => 24 Julio 2013                                             #
#Requerimiento     => MLM-1975                                                  #
#################################################################################
#Modificacion      => Se agrega la nueva marca para Reinversion 922 y se        #
#                  => desmarca la 921, cuando se inserta la solicitud           #
#Autor             => Cristina Abasolo Tapia                                    #
#Fecha             => 27 Noviembre 2013                                         #
#Requerimiento     => MLM-2179                                                  #
#################################################################################
DATABASE safre_af

{ ==========================================================================
Clave: 
Nombre: MAIN
Fecha creacion: Diciembre 07, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Proceso que identifica las solicitudes de desinversion a ser reinvertidas

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================
}
MAIN
DEFINE
 li_cont_reg         INTEGER, -- contador de registros
 lr_ret_sol_sdo      RECORD LIKE ret_solicitud_saldo.*, -- registro de solicitud de saldo
 lr_ret_reinversion  RECORD LIKE ret_reinversion.*, -- registro de ret reinversion
 lb_es_laudo         SMALLINT, -- bandera que indica si el nss es marca en proceso de laudo o no
 lb_es_fec_venc_val  SMALLINT, -- bandera que indica si la fecha de vencimiento es valida
 ldt_fec_vencim_aux  DATE, -- fecha de vencimiento calculada
 lb_status_proc      SMALLINT, -- status del proceso
 ls_tipo_origen      SMALLINT, -- tipo de origen a insertar
 lc_qryTxt           CHAR(500), -- se asigna sentencia sql a ejecutar
 m_log               CHAR(30) #-- MLM-1975 --# 24 Julio 2013
 
 DEFINE ls_marca        SMALLINT,
        ls_cod_rechazo  SMALLINT,
        gc_prepare      CHAR(300),
        gc_error        CHAR(1000),
        CERO            SMALLINT
        
 DEFINE  HOY                   DATE
 
 -- se asume que no ocurrira error durante el proceso
 LET lb_status_proc = 0
 
 -- se genera el achivo LOG
 
 #-- MLM-1975 INI --# 24 Julio 2013
 LET m_log = ""
 LET CERO = 0
 LET HOY = TODAY

 SELECT USER 
   INTO m_log
   FROM tab_afore_local
   
 # PREPARE para marcar_la cuenta
 LET gc_prepare = ""
 LET gc_prepare = "EXECUTE PROCEDURE safre_af:marca_cuenta(?,?,?,?,?,?,?,?)"
 LET gc_prepare = gc_prepare CLIPPED
 PREPARE p_marca FROM gc_prepare

 
 LET m_log = m_log CLIPPED,".RETC762.log"
 
 CALL STARTLOG(m_log CLIPPED)
 #-- MLM-1975 FIN --# 24 Julio 2013

 CALL ERRORLOG("Inicio del proceso")
 
 -- se invoca la funcion que registra el inicio del proceso en la tabla bitacora
 --CALL f_graba_proceso("inicio")
 
-- se crea sentencia sql que selecciona las solicitudes de saldo liquidadas con
-- estado 106 (Liquidado saldo). Las cuales no esten previamente registradas

#MLM-1241 INI
 LET lc_qryTxt = " SELECT a.*\n",
                 " FROM ret_solicitud_saldo a\n",
                 " WHERE a.estado_solicitud = 106\n",
                 " AND NOT EXISTS (SELECT 1 FROM ret_reinversion b \n", #--     MLM-1975     --#
                 "                  WHERE a.id_solicitud_saldo = b.id_solicitud_saldo ) \n", #--     MLM-1975     --#
                 " AND a.nss NOT IN (SELECT nss FROM cta_act_marca WHERE marca_cod IN(800,810,815))"
#MLM-1241 FIN

 PREPARE prp_slct_retSolSdo FROM lc_qryTxt
 DECLARE cur_slct_retSolSdo CURSOR FOR prp_slct_retSolSdo
 
 FOREACH cur_slct_retSolSdo INTO lr_ret_sol_sdo.*
   -- se invoca la funcion que verifica si el nss es marca en proceso de Laudo (marca 590)
   CALL f_nss_marcaLaudo(lr_ret_sol_sdo.nss) RETURNING lb_es_laudo
   
   -- se verifica si es laudo el nss
   IF lb_es_laudo THEN
     LET ls_tipo_origen = 2
   ELSE
     -- se verifica si la fecha de vencimiento no es nula
     IF lr_ret_sol_sdo.fecha_vencimiento IS NOT NULL THEN
       -- se asigna la fecha de vencimiento (DATETIME) en la variable auxiliar para cumplir
       -- con el tipo de dato requerido en la funcion (DATE)
       LET ldt_fec_vencim_aux = lr_ret_sol_sdo.fecha_vencimiento
       
       -- se invoca la funcion que verifica si la fecha vencimiento ya esta excedida
       CALL f_val_fecVencimiento(ldt_fec_vencim_aux) RETURNING lb_es_fec_venc_val
     ELSE
       -- se verifica si la fecha de recepcion procesar no es nula
       IF lr_ret_sol_sdo.f_recep_procesar IS NOT NULL THEN
         -- se invoca la funcion que calcula la fecha vencimiento a partir de la fecha de envio procesar
         CALL f_calc_fecVencimiento(lr_ret_sol_sdo.f_recep_procesar) RETURNING ldt_fec_vencim_aux
         
         -- se invoca la funcion que verifica si la fecha vencimiento calculada ya esta excedida
         CALL f_val_fecVencimiento(ldt_fec_vencim_aux) RETURNING lb_es_fec_venc_val
       ELSE
         -- continua con el siguiente registro
         CONTINUE FOREACH
       END IF
     END IF
     
     -- se verifica el status de de la fecha de vencimiento valida
     IF lb_es_fec_venc_val THEN
       LET ls_tipo_origen = 3
     ELSE
       -- continua con el siguiente registro
       CONTINUE FOREACH
     END IF
   END IF
   
   -- se inserta el registro en ret reinversion, con estado 140-Solicitud reinversion
   --LET lr_ret_reinversion.id_reinversion = 1
   LET lr_ret_reinversion.id_solicitud_saldo = lr_ret_sol_sdo.id_solicitud_saldo
   LET lr_ret_reinversion.fecha_registro = CURRENT YEAR TO SECOND
   LET lr_ret_reinversion.id_tipo_origen_reinv  = ls_tipo_origen
   LET lr_ret_reinversion.estado_solicitud = 140
   
   -- se inserta el registo en ret reinversion
   INSERT INTO ret_reinversion VALUES (lr_ret_reinversion.*)
   
   IF SQLCA.sqlcode < 0 THEN
     LET lb_status_proc = -1
     
     EXIT FOREACH
   END IF
   
   # INI MLM-2179 Quitando la marca 921-CONSULTA SALDO PREVIO
   LET ls_marca   = 921
   LET gc_prepare = "EXECUTE  PROCEDURE  desmarca_cuenta(?,?,?,?,?,?)"  
   LET gc_prepare =  gc_prepare   CLIPPED     
   PREPARE  p_desmarca       FROM  gc_prepare               
   EXECUTE  p_desmarca       USING  lr_ret_sol_sdo.nss,                  
                                    ls_marca,          
                                    lr_ret_sol_sdo.id_solicitud_saldo, 
                                    CERO,                   
                                    ls_marca,          
                                    m_log

   # Marcando la cuenta con 922-REINVERTIDOS 
   LET ls_marca  = 922
   DECLARE cur_marca CURSOR FOR p_marca
   OPEN cur_marca USING 
       lr_ret_sol_sdo.nss,                       #pnss           
       ls_marca,                                 #pmarca_entra   
       lr_ret_sol_sdo.id_solicitud_saldo,        #pcorrelativo   
       CERO,                                     #pestado_marca  
       CERO,                                     #pcodigo_rechazo
       ls_marca,                                 #pmarca_causa   
       HOY,                                      #pfecha_causa   
       m_log                                     #pusuario       

   FETCH cur_marca INTO ls_marca, ls_cod_rechazo
   IF ls_cod_rechazo != 0 THEN
      LET gc_error = "NO SE MARCÓ 922-REINVERTIDOS, NSS : ", lr_ret_sol_sdo.nss CLIPPED
      CALL ERRORLOG(gc_error CLIPPED)  
   END IF
   
   CLOSE cur_marca 
   
   # FIN MLM-2179
   
 END FOREACH
 
 -- se invoca la funcion que registra el final del proceso en la tabla bitacora
 --CALL f_graba_proceso("fin")
 
 CALL ERRORLOG("Fin del proceso")
 
 EXIT PROGRAM lb_status_proc
 
END MAIN

{ ==========================================================================
Clave: 
Nombre: f_nss_marcaLaudo
Fecha creacion: Diciembre 07, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que verifica si el nss que entra como parametro pertenece a marca
en proceso de laudo.

Regresa:
TRUE  - si el nss pertenece a marca en proceso de laudo
FALSE - si el nss NO pertenece a marca en proceso de laudo

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================
}
FUNCTION f_nss_marcaLaudo(lc_nss)
DEFINE
 lc_nss       LIKE ret_solicitud_saldo.nss, -- numero de seguridad social
 li_count_reg INTEGER, -- contador de registros
 lc_qryTxt    CHAR(500) -- se asigna sentencia sql a ejecutar
 
 -- se crea la sentencia sql que verifica si el nss es marca en proceso de laudo
 LET lc_qryTxt = " SELECT COUNT(*)\n",
                 " FROM cta_act_marca\n",
                 " WHERE nss = '",lc_nss CLIPPED,"'\n",
                 " AND marca_cod = 590"
 
 PREPARE prp_count_ctaActMarca FROM lc_qryTxt
 EXECUTE prp_count_ctaActMarca INTO li_count_reg
 
 -- se verica la respuesta del query para asignar el valor de retorno
 IF li_count_reg > 0 THEN
   RETURN TRUE
 ELSE
   RETURN FALSE
 END IF
 
END FUNCTION

{ ==========================================================================
Clave: 
Nombre: f_val_fecVencimiento
Fecha creacion: Diciembre 07, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que verifica si la fecha de vencimiento (parametro de entrada)
es valida para insertar en ret reinversion, es decir que la fecha de
entrada sea menor a la fecha actual.

Regresa:
TRUE  - si la fecha de vencimiento es valida
FALSE - si la fecha de vencimiento NO es valida

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================
}
FUNCTION f_val_fecVencimiento(ldt_fec_vencimiento)
DEFINE
 ldt_fec_vencimiento DATE -- fecha de vencimiento
 
 -- se verica la respuesta del query para asignar el valor de retorno
 IF ldt_fec_vencimiento < TODAY THEN
   RETURN TRUE
 ELSE
   RETURN FALSE
 END IF
 
END FUNCTION

{ ==========================================================================
Clave: 
Nombre: f_calc_fecVencimiento
Fecha creacion: Diciembre 07, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que calcula la fecha de vencimiento para la fecha de recepcion
de procesar (entra como parametro). La fecha de vencimiento sera 30
dias habiles mayor a la fecha de recepcion de procesar. Se utliza la
funcion fn_habil_siguiente para calcular dicha fecha.

Regresa:
Fecha de vencimiento

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================
}
FUNCTION f_calc_fecVencimiento(ldt_fec_recep_procesar)
DEFINE
 ldt_fec_recep_procesar LIKE ret_solicitud_saldo.f_recep_procesar, -- fecha recepcion procesar
 ldt_fec_rcp_prcsar_aux DATE, -- fecha de recepcion procesar auxiliar
 ldt_fec_vencimiento    DATE, -- fecha de vencimiento
 lc_qryTxt              CHAR(500) -- se asigna sentencia sql a ejecutar
 
 -- se asigna la fecha en la variable auxiliar
 LET ldt_fec_rcp_prcsar_aux = ldt_fec_recep_procesar
 
 -- se crear la sentencia que invoca "funcion habil siguiente" que le suma 30 dias habiles
 LET lc_qryTxt = " EXECUTE PROCEDURE fn_habil_siguiente('",ldt_fec_rcp_prcsar_aux,"',30)"
 
 PREPARE prp_fn_habil_siguiente FROM lc_qryTxt
 EXECUTE prp_fn_habil_siguiente INTO ldt_fec_vencimiento
 
 -- regresa la fecha de vencimiento
 RETURN ldt_fec_vencimiento
 
END FUNCTION

