update
     det_tra_viv
set estado_reg = 1
where nom_archivo = "S230927.O09AA.C037"
and estado_reg <> 1
;

update
     det_tra_rcv
set estado_reg = 1
where nom_archivo = "S230927.O09AA.C037"
and estado_reg <> 1
;
