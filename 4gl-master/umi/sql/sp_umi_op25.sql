DROP procedure sp_umi_op25;
CREATE PROCEDURE sp_umi_op25(origenSolicitud             CHAR(2), 
                             claveAforeResponsable   CHAR(3), 
                             nss                     CHAR(11),
                             curp                    CHAR(18),
                             tipoConfronta           CHAR(1),
                             identificadorUniverso   CHAR(1),
                             indicadorProcedencia    CHAR(2),
                             lc_usuario              CHAR(20),
                             lc_hora                 CHAR(19)
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
   --DEFINE lc_hora              CHAR(19);

   --Variables de exception
   DEFINE li_sql_err    INT      ;
   DEFINE li_isam_err   INT      ;
   DEFINE lc_error_info CHAR(100);

  --variable para ejecucion del sistema
   DEFINE lc_txt        CHAR(8000);
   DEFINE lc_comilla    CHAR(1);

   DEFINE lc_err_tec_desc VARCHAR(255);

   --Manejo de excepciones
   --ON EXCEPTION
   --   SET li_sql_err, li_isam_err, lc_error_info
    --  LET lc_err_tec_desc = li_sql_err||" "||li_isam_err||" "||lc_error_info;

      --# no se borra el registro en excepciones

   --   EXECUTE PROCEDURE error (-746,lc_err_tec_desc);
   --END EXCEPTION;
   
    --SELECT FIRST 1 substr(current,1,10)||"_"||substr(current,12,8)
    --INTO lc_hora
    --FROM umi_det_solicitud
    --;
    
    SET debug file TO "/safre_lst/sp_umi_op25.txt";
    --TRACE ON;
     
     LET lc_txt  = "";
     LET lc_txt  = "rm -f /safre_lst/paso_op25.log";
     LET lc_txt = TRIM(lc_txt);
     SYSTEM lc_txt;
     
     LET lc_txt  = "";
     LET lc_txt  = "rm -f /safre_lst/"||TRIM(lc_usuario)||".resp_op25.log";
     LET lc_txt = TRIM(lc_txt);
     SYSTEM lc_txt;
     
     LET lc_txt  = "";
     LET lc_txt  = "java -jar /safre/umi/exp/EnvioOp25.jar '"||TRIM(origenSolicitud)||"' '"||TRIM(claveAforeResponsable)||"' '"||TRIM(nss)||"' "||
                   "'"||TRIM(curp)||"' '"||TRIM(tipoConfronta)||"' '"||TRIM(identificadorUniverso)||"' '"||TRIM(indicadorProcedencia)||"'  1 >> /safre_lst/paso_op25.log "||" 2>> /safre_lst/"||TRIM(lc_usuario)||".resp_op25.err";
     LET lc_txt = TRIM(lc_txt);
     SYSTEM lc_txt;
     
     LET lc_txt  = "";  
     LET lc_txt  = "cp /safre_lst/paso_op25.log /safre_lst/EnvioOp25_"||nss||"_"||lc_hora||".rqrp";
     LET lc_txt = TRIM(lc_txt);
     SYSTEM lc_txt;

     LET lc_txt  = "";  
     LET lc_txt  = "tail -1 /safre_lst/paso_op25.log > /safre_lst/"||TRIM(lc_usuario)||".resp_op25.log";
     LET lc_txt = TRIM(lc_txt);
     SYSTEM lc_txt;    

     LET lc_txt  = "";
     LET lc_txt  = "chmod 777 /safre_lst/"||TRIM(lc_usuario)||".resp_op25.log";
     LET lc_txt = TRIM(lc_txt);
     SYSTEM lc_txt;
     
     RETURN 0 ;
   
--TRACE OFF;
END PROCEDURE;
