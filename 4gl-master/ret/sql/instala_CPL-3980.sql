
create temp table base (nss char(11), ind_edad smallint);


insert into base
select a.n_seguro, c.id_rango_nacimiento
from afi_mae_afiliado a, cat_rango_nacimiento c
where a.n_seguro in(
"13796211897",
"01096100035",
"13806000785",
"82987922257",
"23057427397",
"12129393984",
"38179879374",
"92018417243",
"48026301050",
"04927421133",
"84907032753",
"92917406115",
"17846506776")
and year(a.fena) between c.anio_ini and c.anio_fin
;


create procedure crea_reg_CPL_3980()
returning char(11) as nss,
          smallint as gpo_regimen,
          smallint as resultado;


define v_nss char(11);
define v_grupo_regimen smallint;
define v_siefore_regimen smallint;
define v_marca_regimen smallint;
define v_monto_acciones decimal(16,6);
define v_siefore_ant smallint;
define v_f_solicitud date;
define v_criterio smallint;
define v_estado_sol smallint;
define v_marca smallint;
define v_marca_causa smallint;
define v_edo_marca smallint;
define v_rechazo_marca smallint;

define v_tipo_proceso smallint;
define v_tipo_traspaso smallint;
define v_medio char(2);

define r_ind_edad smallint;
define r_folio integer;
define r_rechazo smallint;

let v_f_solicitud = today;
let v_tipo_proceso = 2;
let v_tipo_traspaso = 14;
let v_medio = 10;
let v_criterio = 1;
let v_estado_sol = 100;
let v_edo_marca = 0;
let v_rechazo_marca = 0;

   FOREACH select *
           into v_nss, r_ind_edad
           from base

        LET v_siefore_regimen = NULL;
        LET v_siefore_ant = NULL;
        LET v_monto_acciones = 0;
        LET v_marca_regimen = NULL;

        FOREACH
           SELECT c.grupo_regimen  ,
                  c.codigo_siefore ,
                  t.marca_cod
             INTO v_grupo_regimen,
                  v_siefore_regimen,
                  v_marca_regimen
             FROM cta_nss_regimen c ,
                  tab_grupo_regimen t
            WHERE c.nss = v_nss
              AND t.grupo_regimen = c.grupo_regimen
              AND t.ind_actualiza = 1

           LET v_marca_causa = v_marca_regimen;

           IF EXISTS ( SELECT rechazo_cod
                         FROM  cta_convivencia c, cta_act_marca a
                        WHERE  a.nss = v_nss
                          AND  a.marca_cod    = c.marca_activa
                          AND  c.marca_entra  = v_marca_regimen
                          AND  c.rechazo_cod  > 0
                     ) THEN
              LET r_rechazo = 2 ;
           ELSE
              SELECT siefore, sum(monto_en_acciones) monto_en_acciones
                INTO v_siefore_ant, v_monto_acciones
                FROM dis_cuenta
               WHERE nss = v_nss
                 AND subcuenta IN ( SELECT subcuenta
                                      FROM tab_agrupa_subcta_regimen
                                     WHERE grupo_regimen = v_grupo_regimen )
                 AND siefore <> v_siefore_regimen
               GROUP BY 1
               HAVING sum(monto_en_acciones) > 0 ;

               IF v_monto_acciones > 0 THEN
                  LET r_folio    = 0;

                  INSERT INTO  cta_solicitud_regimen
                       VALUES  ( v_nss         ,
                                 v_f_solicitud ,
                                 '0'           , -- Folio Serial
                                 v_medio       ,
                                 CURRENT       ,
                                 NULL          ,
                                 NULL          ,
                                 r_ind_edad    ,
                                 v_criterio    ,
                                 NULL          ,
                                 v_tipo_traspaso
                               );

                  --****  Recupera el Folio Asignado ****
                  LET r_folio = DBINFO('sqlca.sqlerrd1');

                  LET v_marca, r_rechazo = marca_cuenta ( v_nss              ,
                                                          v_marca_regimen    ,
                                                          r_folio            ,
                                                          v_edo_marca        ,
                                                          v_rechazo_marca    ,
                                                          v_marca_causa      ,
                                                          NULL               ,
                                                          USER
                                                          );
                  IF r_rechazo <> 0 THEN
                     --***  No se pudo marcar la cuenta
                     IF r_rechazo <> v_marca_regimen THEN
                        LET r_rechazo = 2 ; -- No se pudo marcar con convivencia
                     ELSE
                        --**   No se pudo marcar porque existe otra solicitud
                        --**   de transferencia entre siefores
                        LET r_rechazo = 6 ;
                     END IF
                  ELSE
                     INSERT INTO tes_solicitud VALUES (v_nss            ,
                                                       r_folio          ,
                                                       v_f_solicitud    ,
                                                       v_medio          ,
                                                       v_tipo_traspaso  ,
                                                       v_grupo_regimen  ,
                                                       v_siefore_ant    ,
                                                       v_siefore_regimen,
                                                       v_estado_sol     ,
                                                       NULL             ,
                                                       NULL             ,
                                                       NULL
                                                     );
                  END IF --- Fin de marcaje
               END IF
           END IF

           LET v_siefore_regimen = NULL;
           LET v_siefore_ant = NULL;
           LET v_monto_acciones = 0;
           LET v_marca_regimen = NULL;

        END FOREACH;

   END FOREACH;

return v_nss, v_grupo_regimen, r_rechazo with resume;

END PROCEDURE;

execute procedure crea_reg_CPL_3980();
drop procedure crea_reg_CPL_3980;
drop table base;
