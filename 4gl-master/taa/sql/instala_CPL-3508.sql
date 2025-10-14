delete from taa_cd_tipo_traspaso where tipo_traspaso in (86);
insert into taa_cd_tipo_traspaso values (86,223,"TCAPP-",0,"TRASP CURP U ACT ACT EXTRA AFORE",4,0);

delete from tab_movimiento where codigo in (223);
insert into tab_movimiento values (223,"TRASP CURP U ACT ACT EXTRA AFORE",-1);

delete from tab_marca where marca_cod in (223);
insert into tab_marca values (223,"TRASP CURP U ACT ACT EXTRA AFOR",120,1,0,today,"safre");

delete from cta_convivencia where marca_entra  in (223);
delete from cta_convivencia where marca_activa in (223);

--drop table if exists tmp_cta_convivencia ;
--drop table if exists tmp_cta_convivencia1 ;

create temp table tmp_cta_convivencia 
  (
    marca_entra smallint not null ,
    marca_activa smallint not null ,
    convive_cod smallint not null ,
    rechazo_cod smallint not null ,
    fecha_actualiza date not null ,
    usuario char(8) not null 
  );
create temp table tmp_cta_convivencia1
  (
    marca_entra smallint not null ,
    marca_activa smallint not null ,
    convive_cod smallint not null ,
    rechazo_cod smallint not null ,
    fecha_actualiza date not null ,
    usuario char(8) not null 
  );


--223

insert into tmp_cta_convivencia 
select unique * from cta_convivencia
where marca_entra = 220 
   OR marca_activa = 220
;

update tmp_cta_convivencia
set marca_entra = 223
where marca_entra = 220
;
update tmp_cta_convivencia
set marca_activa = 223
where marca_activa = 220
;
insert into tmp_cta_convivencia values (220,223,20,11,today,"safre")
;
insert into tmp_cta_convivencia values (223,220,20,11,today,"safre")
;

INSERT INTO cta_convivencia
SELECT * FROM tmp_cta_convivencia
;

DELETE FROM tmp_cta_convivencia
;

