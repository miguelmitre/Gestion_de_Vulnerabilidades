unload to CPL-3996.resp 
select * from cta_act_bono
where nss in ( "02196780270", "15916829896",
               "24927606244", "33018015801",
               "45927614714", "51027700627",
               "53987210191", "61997804564",
               "62897007928", "67117301829",
               "71967831547", "I0177400459",
               "I0187901128", "I0197203397")
;

insert into cta_his_bono(nss, curp,fecha_redencion,
                         fecha_registro, udis,
                         pesos, proceso, fecha_baja,
                         proceso_baja, fecha_reingreso, usuario)
select nss, curp, fecha_redencion, fecha_registro,
       udis, pesos, proceso,today,"TAC","","CPL-3996"
from cta_act_bono
where nss in ( "02196780270", "15916829896",
               "24927606244", "33018015801",
               "45927614714", "51027700627",
               "53987210191", "61997804564",
               "62897007928", "67117301829",
               "71967831547", "I0177400459",
               "I0187901128", "I0197203397")
;

delete from cta_act_bono
where nss in ( "02196780270", "15916829896",
               "24927606244", "33018015801",
               "45927614714", "51027700627",
               "53987210191", "61997804564",
               "62897007928", "67117301829",
               "71967831547", "I0177400459",
               "I0187901128", "I0197203397")
;
