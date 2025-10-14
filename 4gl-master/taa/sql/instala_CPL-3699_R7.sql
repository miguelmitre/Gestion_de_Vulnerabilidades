delete from dis_provision where folio = 96869
;
update taa_cd_det_cedido 
set estado = 103
where n_seguro in (select b.n_seguro 
                   from taa_cd_det_comple b 
                   where b.folio = 96869)
and n_seguro not in (select b.n_seguro 
                     from taa_cd_det_comple b
                     where b.folio > 96869
                     and estado in (101,102))
and estado = 12
;
delete from taa_cd_det_comple
where folio = 96869
;
delete from taa_cd_ctr_folio
where folio = 96869
;