DROP PROCEDURE sp_act_status_movil;
CREATE PROCEDURE sp_act_status_movil(ps_cod_rech SMALLINT, pd_folio_pre DECIMAL(10,0), pc_motivo_rechazo CHAR(100))
   
  IF ps_cod_rech = 0 THEN
     LET pc_motivo_rechazo = "";
     
     SET LOCK MODE TO WAIT;

     UPDATE afi_solicitud_movil
      SET status         = '01', -- ACEPTADO
          motivo_rechazo = pc_motivo_rechazo      
     WHERE id_folio_movil = pd_folio_pre;

     SET LOCK MODE TO NOT WAIT;
  ELSE   
     SET LOCK MODE TO WAIT;

     UPDATE afi_solicitud_movil
      SET status         = '02', -- RECHAZADO
          motivo_rechazo = pc_motivo_rechazo      
     WHERE id_folio_movil = pd_folio_pre;

     SET LOCK MODE TO NOT WAIT;
  END IF 

END PROCEDURE;


