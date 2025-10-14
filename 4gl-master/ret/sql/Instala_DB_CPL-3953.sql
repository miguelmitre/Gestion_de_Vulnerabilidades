--- CREACION DE LA TABLA HISTORICA 
CREATE table tab_isr_anual_his
  (
    id smallint not null ,
    limite_inferior decimal(14,2) not null ,
    limite_superior decimal(14,2),
    cuota_fija decimal(14,2) not null ,
    porc_excedente decimal(6,4) not null ,
    fecha_inserta datetime year to second,
    usuario char(8), 
    factualiza date
  );

INSERT INTO tab_isr_anual_his
SELECT *, TODAY 
FROM tab_isr_anual;

UNLOAD TO tab_isr_anual_resp.unl
SELECT *
FROM tab_isr_anual;

update tab_isr_anual
 set limite_superior =8952.49, 
     fecha_inserta = current 
where id= 1;


update tab_isr_anual
 set limite_inferior = 8952.50, 
     limite_superior = 75984.55, 
     cuota_fija = 171.88, 
     fecha_inserta = current 
where id= 2;

update tab_isr_anual
 set limite_inferior = 75984.56, 
     limite_superior = 133536.07, 
     cuota_fija = 4461.94, 
     fecha_inserta = current 
where id= 3;

update tab_isr_anual
 set limite_inferior = 133536.08, 
     limite_superior = 155229.80, 
     cuota_fija = 10723.55, 
     fecha_inserta = current 
where id= 4;

update tab_isr_anual
 set limite_inferior = 155229.81, 
     limite_superior = 185852.57, 
     cuota_fija = 14194.54, 
     fecha_inserta = current 
where id= 5;

update tab_isr_anual
 set limite_inferior = 185852.58, 
     limite_superior = 374837.88, 
     cuota_fija = 19682.13, 
     fecha_inserta = current 
where id= 6;

update tab_isr_anual
 set limite_inferior = 374837.89, 
     limite_superior = 590795.99, 
     cuota_fija = 60049.40, 
     fecha_inserta = current 
where id= 7;

update tab_isr_anual
 set limite_inferior = 590796.00, 
     limite_superior = 1127926.84, 
     cuota_fija = 110842.74, 
     fecha_inserta = current 
where id= 8;

update tab_isr_anual
 set limite_inferior = 1127926.85, 
     limite_superior = 1503902.46, 
     cuota_fija = 271981.99, 
     fecha_inserta = current 
where id= 9;

update tab_isr_anual
 set limite_inferior = 1503902.47, 
     limite_superior = 4511707.37, 
     cuota_fija = 392294.17, 
     fecha_inserta = current 
where id= 10;

update tab_isr_anual
 set limite_inferior = 4511707.38, 
     cuota_fija = 1414947.85, 
     fecha_inserta = current 
where id= 11;




