create temp table precios_cpl_3975
(
siefore         smallint  ,
precio_del_dia  dec(16,6)
);

insert into precios_cpl_3975 values (14,2.538157);
insert into precios_cpl_3975 values (15,2.776310);
insert into precios_cpl_3975 values (16,2.723011);
insert into precios_cpl_3975 values (17,11.376006);
insert into precios_cpl_3975 values (18,2.866006);
insert into precios_cpl_3975 values (19,11.686117);
insert into precios_cpl_3975 values (20,2.991476);
insert into precios_cpl_3975 values (21,2.966839);
insert into precios_cpl_3975 values (22,2.924104);
insert into precios_cpl_3975 values (90,2.428769);

create temp table dis_cuenta_cpl_3975_620_1
  (
    tipo_movimiento smallint not null ,
    subcuenta smallint not null ,
    siefore smallint,
    folio integer not null ,
    consecutivo_lote integer,
    nss char(11) not null ,
    curp char(18),
    folio_sua char(6),
    fecha_pago date,
    fecha_valor date,
    fecha_conversion date,
    monto_en_pesos decimal(22,6),
    monto_en_acciones decimal(22,6),
    precio_accion decimal(22,6),
    dias_cotizados smallint,
    sucursal char(10),
    id_aportante char(11),
    estado integer,
    fecha_proceso date,
    usuario char(8),
    fecha_archivo date,
    etiqueta integer
  );

load from diferencial_cpl_3975.unl insert into dis_cuenta_cpl_3975_620_1;

create procedure cpl_3975()
returning char(50) as salida;
          

define v_siefore smallint ; 
define v_precio dec(16,6);
define v_salida char(50);
define v_folio integer;

   INSERT INTO safre_af:glo_folio VALUES(0);

   SELECT MAX(folio)
   INTO   v_folio
   FROM   safre_af:glo_folio;


foreach select siefore ,
               precio_del_dia 
        into   v_siefore ,
               v_precio 
        from precios_cpl_3975
        order by 1

        update dis_cuenta_cpl_3975_620_1
        set    monto_en_pesos = monto_en_acciones * v_precio  ,
               precio_accion = v_precio ,
               fecha_conversion = today ,
               fecha_valor = today ,
               fecha_pago = today  ,
               fecha_proceso = today ,
               fecha_archivo = today ,
               folio         = v_folio
        where siefore = v_siefore;

        LET v_salida = "Siefore: "||v_siefore||" Actualizada con precio: "||" "||v_precio;

        RETURN v_salida WITH RESUME;

end foreach;

end procedure ;

execute procedure cpl_3975();

insert into dis_cuenta 
select * from dis_cuenta_cpl_3975_620_1 ;

SELECT folio,siefore,subcuenta,sum(monto_en_acciones) acciones,sum(monto_en_pesos)  pesos
from dis_cuenta_cpl_3975_620_1
group by 1,2,3;

drop procedure cpl_3975;
