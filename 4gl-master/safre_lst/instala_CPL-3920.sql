update afi_solicitud 
set finicta = "11/28/2022" 
where n_seguro = "34856401012"
and tipo_solicitud = 6;

update afi_mae_afiliado
set finicta = "11/28/2022" 
where n_seguro = "34856401012"
and tipo_solicitud = 6;
