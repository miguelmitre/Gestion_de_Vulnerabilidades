#Actualiza afi_mae_afiliado                                         #
#Autor: Juan de la cruz P.V.                                        #

DATABASE safre_tmp

GLOBALS

 define hora      char(10)
 define lei2      integer
 define graba2    integer
 define inserta2  integer
 define cod_ret   integer
 define noexiste  integer
 define nom_arch  char(65)
 define cuantos  integer
 define opcion   char(1)
 define mes      char(02)
 define dia      char(02)
 define anio     char(4)
 define fp       char(10)
 define  cr      char(30)
 define  vnum_acciones decimal(22,6)
 define  vsaldo        decimal(15,2)
 define reg_trr  RECORD LIKE safre_af:taa_rcv_recepcion.* 
 define reg_tvr  RECORD LIKE safre_af:taa_viv_recepcion.* 
 define rcod   smallint
 define vfolio integer

END GLOBALS

MAIN
   let vfolio   = ARG_VAL(1)
   let opcion   = ARG_VAL(2)
   let hora = TIME
   display "Inicia programa BOMB00I " , hora
   display "Folio   recibido ", vfolio,   ' Opcion ', opcion
   if opcion = 1  then
     display "Opcion Actualizar "
    else
     display "Opcion DUMMY"
   end if 
   call inicio()
   call principal_rcv()
   call inicio()
   call principal_viv()

   let hora = TIME
   display "Termino programa BOMB00I " , hora

END MAIN

FUNCTION inicio()

let cuantos = 0
let lei2    = 0
let graba2  = 0
let inserta2= 0
let noexiste= 0


END FUNCTION #inicio()

FUNCTION principal_rcv()

select count(*)
into cuantos
from   safre_af:taa_rcv_recepcion
where folio = vfolio
display " RCV "
display "Registros que contiene el archivo trr ", cuantos USING "####,##&"

IF opcion = 1  THEN
 display "Inicia Actualizacion RCV ", hora
 declare c2 cursor for
 select *
 from safre_af:taa_rcv_recepcion
 where folio = vfolio
 foreach c2 into reg_trr.*
  let lei2 = lei2 + 1
  INITIALIZE  cod_ret  TO NULL             
  update safre_af:dis_cuenta         
   set consecutivo_lote = reg_trr.cont_servicio
  where folio           = vfolio    
   and  nss             = reg_trr.nss
   and  subcuenta       not in(4,8,14,35,36)
  let cod_ret = SQLCA.SQLERRD[3]
  if  cod_ret = 0  then
     --display "NO ACTUALIZO ", reg_trr.folio, ' ', reg_trr.nss
   else
       -- display "actualizo ",cod_ret     ,' registros', ' folio ',
       --      reg_trr.folio USING "######", ' ', ' nss ',
       --      reg_trr.nss
        let  graba2 = graba2 + cod_ret
  end if
  if lei2 MOD 1000 = 0 THEN
    let hora = TIME
    display "Procesados RCV: ", lei2, ' ','De: ', cuantos, ' ', hora
    if cod_ret > 0 then
      display "ultimo actualizado ", reg_trr.nss    
    end if
  end if

 end foreach
end if

   display "Registros actualizados RCV ", graba2      USING "####,###"
   display "Registros leidos trr   ", lei2        USING "####,###"
END FUNCTION       #principal_rcv()


FUNCTION principal_viv()

select count(*)
into cuantos
from   safre_af:taa_viv_recepcion
where folio = vfolio
display " VIV "
display "Registros que contiene el archivo tvr ", cuantos USING "####,##&"

IF opcion = 1  THEN
 display "Inicia Actualizacion VIV ", hora
 declare c3 cursor for
 select *
 from safre_af:taa_viv_recepcion
 where folio = vfolio
 foreach c3 into reg_tvr.*
  let lei2 = lei2 + 1
  INITIALIZE  cod_ret  TO NULL             
  update safre_af:dis_cuenta         
   set consecutivo_lote = reg_tvr.cont_servicio
  where folio           = vfolio    
   and  nss             = reg_tvr.nss
   and  subcuenta       in(4,8,14,35,36)
  let cod_ret = SQLCA.SQLERRD[3]
  if  cod_ret = 0  then
     --display "NO ACTUALIZO ", reg_tvr.folio, ' ', reg_tvr.nss
   else
        -- display "actualizo ",cod_ret     ,' registros', ' folio ',
        --     reg_tvr.folio USING "######", ' ', ' nss ',
        --     reg_tvr.nss
        let  graba2 = graba2 + cod_ret
  end if
  if lei2 MOD 1000 = 0 THEN
    let hora = TIME
    display "Procesados VIV: ", lei2, ' ','De: ', cuantos, ' ', hora
    if cod_ret > 0 then
      display "ultimo actualizado ", reg_tvr.nss    
    end if
  end if

 end foreach
end if

   display "Registros actualizados VIV ", graba2      USING "####,###"
   display "Registros leidos tvr   ", lei2        USING "####,###"
END FUNCTION       #principal_viv()

