unload to tab_campo_resp_408.unl
select *
from tab_campo
where layout_cod = 408
and tipo_reg = 2
order by campo_cod asc;

delete
from tab_campo
where layout_cod = 408
and tipo_reg = 2;

load from tab_campo_1802.unl
insert into tab_campo;