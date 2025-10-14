DROP procedure sp_umx_op01;
CREATE PROCEDURE sp_umx_op01(entidadOrigen                    CHAR(3),
														 nssSolicitud                     CHAR(11),
														 curpSolicitud                    CHAR(18),
														 rfc                              CHAR(13),
														 apellidoPaterno                  CHAR(40),
														 apellidoMaterno                  CHAR(40),
														 nombreTrabajador                 CHAR(40),
														 fechaDeNacimiento                CHAR(8),
														 folioDeLaSolicitud               CHAR(10),
														 sexo                             CHAR(1),
														 entidadFederativaDeNacimiento    CHAR(2),
														 indicadorDeUnificacionAutorizada CHAR(1),
														 nacionalidad                     CHAR(3),
														 claveDeTipoDeDocumentoProbatorio CHAR(1),
														 documentoProbatorio              CHAR(16),
														 folioDeDocumentoProbatorio       CHAR(10),
														 tipoMovimiento                   CHAR(1),
														 selloUnicoVerificacionBiometrica CHAR(14),
														 curpEmpleado                     CHAR(18),
														 tipoSolicitante                  CHAR(2),
														 curpSolicitante                  CHAR(18),
                             lc_usuario                       CHAR(20)
)
RETURNING INTEGER;

-- variables de la captura


   DEFINE ls_id                INTEGER;
   DEFINE ls_zero              SMALLINT;
   DEFINE lf_fecha_carga       DATETIME YEAR TO SECOND;
   DEFINE lc_fec_carga         CHAR(20);
   DEFINE lc_today             CHAR(08);
   DEFINE lc_fecha_tmp         CHAR(10);
   DEFINE lc_fecha_sol         CHAR(10);
   DEFINE lc_fecha_nac         CHAR(10);
   DEFINE ls_estado            SMALLINT;
   DEFINE lc_ok                CHAR(2);
   DEFINE lc_hora              CHAR(19);

   --Variables de exception
   DEFINE li_sql_err    INT      ;
   DEFINE li_isam_err   INT      ;
   DEFINE lc_error_info CHAR(100);

  --variable para ejecucion del sistema
   DEFINE lc_txt        CHAR(9000);
   DEFINE lc_comilla    CHAR(1);

   DEFINE lc_err_tec_desc VARCHAR(255);

   --Manejo de excepciones
   --ON EXCEPTION
   --   SET li_sql_err, li_isam_err, lc_error_info
    --  LET lc_err_tec_desc = li_sql_err||" "||li_isam_err||" "||lc_error_info;

      --# no se borra el registro en excepciones

   --   EXECUTE PROCEDURE error (-746,lc_err_tec_desc);
   --END EXCEPTION;
   
    SELECT FIRST 1 substr(current,1,10)||"_"||substr(current,12,8)
    INTO lc_hora
    FROM tab_afore_local
    ;

    SET debug file TO "/safre_lst/sp_umx_op01.txt";
    --TRACE ON;
     
     LET lc_txt  = "";
     LET lc_txt  = "rm -f /safre_lst/paso_op01.log";
     LET lc_txt = TRIM(lc_txt);
     SYSTEM lc_txt;
     
     LET lc_txt  = "";
     LET lc_txt  = "rm -f /safre_lst/"||TRIM(lc_usuario)||".resp_op01.log";
     LET lc_txt = TRIM(lc_txt);
     SYSTEM lc_txt;

     LET lc_txt  = "";
     LET lc_txt  = "java -jar /safre/umx/exp/EnvioOp01.jar  '"||TRIM(entidadOrigen)||"' "||                   
																												   "'"||TRIM(nssSolicitud)||"' "||                    
																												   "'"||TRIM(curpSolicitud)||"' "||                   
																												   "'"||TRIM(rfc)||"' "||                             
																												   "'"||TRIM(apellidoPaterno)||"' "||                 
																												   "'"||TRIM(apellidoMaterno)||"' "||                 
																												   "'"||TRIM(nombreTrabajador)||"' "||                
																												   "'"||TRIM(fechaDeNacimiento)||"' "||               
																												   "'"||TRIM(folioDeLaSolicitud)||"' "||              
																												   "'"||TRIM(sexo)||"' "||                            
																												   "'"||TRIM(entidadFederativaDeNacimiento)||"' "||   
																												   "'"||TRIM(indicadorDeUnificacionAutorizada)||"' "||
																												   "'"||TRIM(nacionalidad)||"' "||                    
																												   "'"||TRIM(claveDeTipoDeDocumentoProbatorio)||"' "||
																												   "'"||TRIM(documentoProbatorio)||"' "||             
																												   "'"||TRIM(folioDeDocumentoProbatorio)||"' "||      
																												   "'"||TRIM(tipoMovimiento)||"' "||                  
																												   "'"||TRIM(selloUnicoVerificacionBiometrica)||"' "||
																												   "'"||TRIM(curpEmpleado)||"' "||                    
																												   "'"||TRIM(tipoSolicitante)||"' "||                 
																												   "'"||TRIM(curpSolicitante)||"'  1 >> /safre_lst/paso_op01.log "||" 2>> /safre_lst/"||TRIM(lc_usuario)||".resp_op01.err";
     LET lc_txt = TRIM(lc_txt);
     SYSTEM lc_txt;
     
     LET lc_txt  = "";  
     LET lc_txt  = "cp /safre_lst/paso_op01.log /safre_lst/EnvioOp01_"||curpSolicitud||"_"||lc_hora||".rqrp";
     LET lc_txt = TRIM(lc_txt);
     SYSTEM lc_txt;

     LET lc_txt  = "";  
     LET lc_txt  = "tail -1 /safre_lst/paso_op01.log > /safre_lst/"||TRIM(lc_usuario)||".resp_op01.log";
     LET lc_txt = TRIM(lc_txt);
     SYSTEM lc_txt;    

     LET lc_txt  = "";
     LET lc_txt  = "chmod 777 /safre_lst/"||TRIM(lc_usuario)||".resp_op01.log";
     LET lc_txt = TRIM(lc_txt);
     SYSTEM lc_txt;
     
     RETURN 0 ;
   
--TRACE OFF;
END PROCEDURE;
