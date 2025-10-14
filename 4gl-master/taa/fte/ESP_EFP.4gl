DATABASE safre_tmp

globals

define grecep_rcv record like recep_rcv.*
define grec_tra_rcv record like rec_tra_rcv.*

end globals

main

  declare c_1 cursor for
  select *
  from   rec_tra_rcv
  foreach c_1 into grec_tra_rcv.*

     select 'x'
     from   recep_rcv
     where  nss = grec_tra_rcv.n_seguro
     and cve_subcta = grec_tra_rcv.cve_subcta
     and cve_ced_cuenta = grec_tra_rcv.cve_ced_cuenta
     and siefore = grec_tra_rcv.siefore
     group by 1
     if status = notfound then

        update rec_tra_rcv set folio = 1
        where n_seguro =  grec_tra_rcv.n_seguro
        and cve_subcta = grec_tra_rcv.cve_subcta
        and cve_ced_cuenta = grec_tra_rcv.cve_ced_cuenta
        and siefore = grec_tra_rcv.siefore
     end if

  end foreach

end main
