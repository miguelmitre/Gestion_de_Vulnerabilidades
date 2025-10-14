unload to sel_f_ap_cta.unl
select n_seguro, n_folio, tipo_solicitud
from afi_mae_afiliado
where n_seguro in(
select nss
from taa_viv_recepcion
where folio in(
108671,
108678))
;
