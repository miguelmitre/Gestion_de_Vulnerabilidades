database safre_af
globals

    define dis_prov record like safre_af:dis_provision.*
    define rec_tra_rcv record like safre_tmp:rec_tra_rcv.*
    define vprecio_dia decimal (18,6)
    define tot_reg smallint

end globals

main

    let tot_reg = 0
    select precio_del_dia
      into vprecio_dia
      from glo_valor_accion
     where fecha_valuacion  = '01292009'
       and codigo_siefore = 1

    declare cur_1 cursor for
    select *
    from safre_tmp:rec_tra_rcv
    where folio = 1

    foreach cur_1 into rec_tra_rcv.*
        let tot_reg = tot_reg + 1

        update dis_provision
           set siefore = 1,
               precio_accion = vprecio_dia,
               monto_en_acciones = monto_en_pesos / vprecio_dia
         where folio = 7469
           and nss = rec_tra_rcv.n_seguro
           and subcuenta = rec_tra_rcv.cve_subcta

        display tot_reg

    end foreach

end main

