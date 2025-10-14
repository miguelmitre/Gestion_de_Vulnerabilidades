######################################################################
#MODULO        : SEP
#PROGRAMA      : SEPB016
#DESCRIPCION   : Provisiona las parejas de Separacion de Cuentas que se
#                encuentran en estado 54 (proyectadas)
#AUTOR         : JESUS YAÑEZ MORENO
#FECHA         : 3 abr 2010
######################################################################
DATABASE safre_af

GLOBALS
define  g_folio          integer
define  g_total     integer

define  g_enter          smallint,
        g_confirma       smallint

define  g_today          ,
        g_fecha_max      ,
        g_fecha_habil    ,
        g_primer_natural ,
        g_primer_habil   date 

END GLOBALS

MAIN

 OPTIONS INPUT WRAP,
 PROMPT LINE LAST,
 ACCEPT KEY CONTROL-I
 DEFER INTERRUPT

 CALL STARTLOG("SEPB016.log")

 let g_today = TODAY

 open window v_provisiona at 2,2 with form "SEPB016" attribute(border) 
    DISPLAY "                                                                               " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY " SEPB016              PROVISION DE SEPARACION DE CUENTAS                       " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY g_today USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

-- se validan las fechas de ejecucion del proceso

 call valida_fecha()

-- se muestran datos iniciales informativos

 select count(*) 
 into   g_total
 from   sep_det_reg_sol_reclamante a 
 where  a.estado = 54 -- proyectadas
 
 display by name g_total,g_primer_habil,g_today

-- se pide confirmacion de provision

 call pregunta("Confirmar Provision [s/n]...")
 returning g_confirma

-- si no se confirma se regresa a la pantalla de lista

 if not g_confirma then 
    error"Cancelando Provision..." 
    sleep 2
    exit program
 else 
    error"Procesando Provision ..."
 end if 
 

--
-- provision confirmada
--

 -- se toma folio para provisionar el lote de parejas de separacion

 select max(folio) + 1
 into   g_folio 
 from   glo_folio

 insert into glo_folio values (g_folio)

 call insertar_detalle_mov_separacion()

 call provisionar()

 display "Folio de Provision : ", g_folio at 14,14 attribute(reverse) 

 prompt "Provision Concluida <enter> para Continuar..." for char g_enter

END MAIN

FUNCTION cal_fecha_avant(xfecha,ndias)
#calcula el dia habil dado en ciclo
--------------------------------------

define xfecha,fhabil     DATE

define ndias                    ,
       cuenta                   ,
       dia_semana      SMALLINT

let fhabil = xfecha
let cuenta = 1

 while cuenta <= ndias
     let dia_semana = weekday(fhabil)
     if  dia_semana = 0 or dia_semana = 6 then 
         let fhabil = fhabil + 1
         continue while
     else 
         select "ok"
         from   tab_feriado 
         where  feria_fecha = fhabil
         if STATUS <> NOTFOUND then 
            let fhabil = fhabil + 1
            continue while
         else 
            let cuenta = cuenta + 1 
         end if
     end if
 end while 

 return fhabil

END FUNCTION


FUNCTION valida_fecha()
# valida que la fecha sea valida para provisionar
-------------------------------------------------

 call cal_fecha_avant(g_today,1)
 returning g_fecha_habil

 if g_today <> g_fecha_habil then 
    prompt "Dia inhabil, no se puede provisionar...<enter>" for char g_enter
    exit program
 end if

 let g_primer_natural = mdy(month(g_today),"01",year(g_today))

 call cal_fecha_avant(g_primer_natural,8)
 returning g_fecha_max

 if g_today > g_fecha_max then
    prompt "Fecha fuera de tiempo para provisionar...<enter>" for char g_enter
    exit program
 end if

 call cal_fecha_avant(g_primer_natural,1)
 returning g_primer_habil

END FUNCTION

FUNCTION pregunta(largumento)
#ciclo while para preguntas tipo s/n
-------------------------------------

define largumento          char(60) ,
       lrespuesta          smallint ,
       lenter              char(001)

let lrespuesta = 0

  while true 
    prompt largumento clipped for char lenter
      if lenter matches "[SsNn]" then 
        if lenter matches "[Ss]" then 
           let lrespuesta = 1
           exit while
        else 
           let lrespuesta = 0 
           exit while
        end if
      end if
  end while
  return lrespuesta
END FUNCTION

FUNCTION insertar_detalle_mov_separacion()
# llena la tabla historica de movimientos de separacion de cuentas
------------------------------------------------------------------


insert into sep_his_dis_cuenta 
select a.*
from   sep_separacion_preliminar  a,
       sep_det_reg_sol_reclamante b
where  b.estado                 = 54  -- proyectada
and    a.idSolicitudSeparacion  = b.correlativo

END FUNCTION


FUNCTION provisionar()
# resume los movimientos involucrados en la separacion de cuentas
-----------------------------------------------------------------

define lfecha_valor           ,
       lfecha_conversion      date

define l_reg_provisiona record 
       nss           char(011),
       siefore       smallint ,
       subcuenta     smallint ,
       monto_en_acciones dec(16,6)
end record

define l_qry         char(300)

define l_proyectadas     record 
       idSolicitudSeparacion integer   ,
       invadido              char(011) ,
       asociado              char(011)
end record

declare cur_proyectadas cursor for

 select a.correlativo ,
        a.n_seguro    ,
        a.nss  
 from   sep_det_reg_sol_reclamante a
 where  a.estado = 54 -- proyectadas
 order by a.correlativo

 let l_qry =
 " select a.nss      , ",
       " a.siefore   , ",
       " a.subcuenta , ",
       " sum(a.monto_en_acciones) ",
 " from   sep_his_dis_cuenta a    ",
 " where  a.idSolicitudSeparacion = ? " ,
 " group by 1,2,3 "

 prepare qry from l_qry
 declare cur_provision cursor for qry

 foreach cur_proyectadas into l_proyectadas.*

   foreach cur_provision using l_proyectadas.idSolicitudSeparacion
                         into  l_reg_provisiona.* 

      case l_reg_provisiona.subcuenta
       when 4 
            let lfecha_valor = g_primer_natural
            let lfecha_conversion = g_primer_habil
            let l_reg_provisiona.siefore = 11
            exit case 
       when 8 
            let lfecha_valor = g_primer_natural
            let lfecha_conversion = g_primer_habil
            let l_reg_provisiona.siefore = 11
            exit case 
      otherwise 
            let lfecha_valor      = g_primer_habil
            let lfecha_conversion = g_primer_habil
            exit case 
      end case 

    if l_reg_provisiona.nss = l_proyectadas.invadido then

      -- inserta el movimiento de cargo por siefore 
      -- y por subcuenta para el invadido

      call inserta_dis_provision(280,
                                l_reg_provisiona.subcuenta ,
                                l_reg_provisiona.siefore   ,
                                l_proyectadas.invadido     ,
                                lfecha_valor               ,
                                lfecha_conversion          , 
                                l_reg_provisiona.monto_en_acciones,
                                l_proyectadas.idSolicitudSeparacion)
    else 
      -- inserta el movimiento de abono por siefore 
      -- y por subcuenta para el asociado

      call inserta_dis_provision(590,
                                l_reg_provisiona.subcuenta ,
                                l_reg_provisiona.siefore   ,
                                l_proyectadas.asociado     ,
                                lfecha_valor               ,
                                lfecha_conversion          , 
                                l_reg_provisiona.monto_en_acciones,
                                l_proyectadas.idSolicitudSeparacion)
    end if

    end foreach  -- fin provision 

    --
    -- se actualiza el estado de la solicitud de separacion como provisionada 
    --
    update sep_det_reg_sol_reclamante 
    set    estado = 55       ,
           folio  = g_folio  ,
           fecha_proceso = g_today
    where  correlativo = l_proyectadas.idSolicitudSeparacion

    update sep_det_solicitud 
    set    folio = g_folio 
    where  idSolicitudSeparacion = l_proyectadas.idSolicitudSeparacion

    update sep_det03_solicitud 
    set    folio = g_folio 
    where  idSolicitudSeparacion = l_proyectadas.idSolicitudSeparacion
    
   end foreach  -- fin proyectadas
END FUNCTION


FUNCTION inserta_dis_provision(ltipmov     ,
                               lsubcuenta  ,
                               lsiefore    ,
                               lnss        ,
                               lfval       ,
                               lfconv      ,
                               lmtoacc     ,
                               lidSolicitudSeparacion )
#inserta en dis_provision
---------------------------------------------

define ltipmov                    ,
       lsubcuenta                 ,
       lsiefore           smallint,
       lnss               char(011),
       lfval              date     ,
       lfconv             date     ,
       lmtoacc            dec(16,6),
       lmtopesos          dec(16,6),
       lprecio            dec(10,6),
       lidSolicitudSeparacion integer


   let lprecio = 0

   if lsiefore = 11 then
       select a.precio_del_dia
       into   lprecio 
       from   glo_valor_accion a
       where  a.fecha_valuacion = lfval
       and    a.codigo_siefore  = lsiefore
   else 
       select a.precio_del_dia
       into   lprecio 
       from   glo_valor_accion a
       where  a.fecha_valuacion = lfconv
       and    a.codigo_siefore  = lsiefore
   end if 

   let lmtopesos = lmtoacc * lprecio

   INSERT INTO dis_provision
               VALUES( ltipmov    ,
                       lsubcuenta , 
                       lsiefore   ,
                       g_folio    ,
                       lidSolicitudSeparacion ,
                       lnss      ,
                       ''        ,--curp
                       ''        ,--sua
                       lfval     ,--fp
                       lfval     ,--fv
                       lfconv    ,--fc
                       lmtopesos ,--mp
                       lmtoacc   ,--ma
                       lprecio   ,--pv
                       '0'       ,--dias cot
                       ''        ,--suc
                       'SEPARACION' ,--id_apo
                       '6'     ,--est
                       g_today ,--fp
                       USER    ,--u
                       g_today ,--fa
                       '1'     )--eti
END FUNCTION
