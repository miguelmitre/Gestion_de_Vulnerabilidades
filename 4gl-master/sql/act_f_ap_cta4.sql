unload to sel_f_ap_cta4.unl
select n_seguro, n_folio, tipo_solicitud, finicta
from afi_mae_afiliado
where n_seguro in(
select nss
from taa_viv_recepcion
where folio in(
109622,
109619))
;

unload to sel_f_ap_cta5.unl
select n_seguro, n_folio, tipo_solicitud, finicta
from afi_mae_afiliado
where n_seguro in(
select nss
from taa_viv_recepcion
where folio in(
109906,
109882))
;


unload to sel_f_ap_cta6.unl
select n_seguro, n_folio, tipo_solicitud, finicta
from afi_mae_afiliado
where n_seguro in(
select nss
from taa_viv_recepcion
where folio in(
110238,
110325))
;


update
     afi_mae_afiliado
set finicta = "09/15/2023"
where n_seguro in(
select nss
from taa_viv_recepcion
where folio in(
109622,
109619))
;


update
     afi_mae_afiliado
set finicta = "09/22/2023"
where n_seguro in(
select nss
from taa_viv_recepcion
where folio in(
109906,
109882))
;


update
     afi_mae_afiliado
set finicta = "09/29/2023"
where n_seguro in(
select nss
from taa_viv_recepcion
where folio in(
110238,
110325))
;
