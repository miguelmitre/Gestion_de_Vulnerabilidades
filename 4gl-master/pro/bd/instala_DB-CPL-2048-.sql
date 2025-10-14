
create table pro_his_capacita
  (
    cod_promotor char(10),
    calificacion smallint,
    horas smallint,
    estado smallint,
    fecha_genera date,
    ind_vigencia char(01),
    factualiza   date
  );

insert into pro_his_capacita
select *," ", today 
from pro_capacitacion;

delete 
from pro_capacitacion;

ALTER TABLE pro_capacitacion
 ADD ind_vigencia CHAR(01);