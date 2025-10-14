#########################################################################
# SEPB015         : Separacion Preliminar de Cuentas
# AUTOR           : Jesus David Yañez Moreno
# Creacion        : 20 de Marzo 2010
# Comentarios     : Reestructura del Modulo de Separacion de Cuentas
#################################################################################
#Actualizacion     => Se valida el regimen por subcuenta de el invadido         #
#                  => para hacer el cargo y el abono correspondiente            #
#Autor             => Carlos Trejo Meixueiro                                    #
#Fecha             => 15 Julio 2013                                             #
#Requerimiento     => CPL-1349                                                  #
#################################################################################
################################################################################
# fecha Actuali   => CPL-1908 02/03/2015                                       #
# Autor           => Cristian  Morales Roblero                                 #
# Actualizacion   => se corrigue el query para que regrese regrese un solo     #
#                    renglon y se agrega delete para en caso de encontrar regis-
#                     tros los borre y vuelva a insertar                        #
#################################################################################


DATABASE safre_af

GLOBALS

define            g_idSolicitudSeparacion integer

define            g_invadido               char(011),
                  g_asociado               char(011),
                  g_reg_patronal           char(011),
                  g_curp_asociado          char(018),
                  g_diagnostico            char(002),
                  g_clasificacion          char(001),
                  g_usuario                char(015),
                  g_nombre_tabla           char(050),
                  g_enter                  char(001)

define            g_ejecuta                          ,
                  sql_cambia_aporte                  ,
                  sql_precio_del_dia                 ,
                  sql_prov_cargo                     ,
                  sql_prov_abono             char(300)

define            g_cero                    ,
                  g_cargo_sep               ,
                  g_abono_sep               ,
                  g_abono_actualizacion_sep ,
                  g_abono_rendimiento_sep   smallint


define            g_today                   ,
                  g_primer_habil            ,
                  g_primer_natural          ,
                  g_fecha_cedido            ,
                  g_fecha_reingreso         date


define     g_rutas                    RECORD LIKE seg_modulo.* ,
           g_sep_movimientos_invadido RECORD LIKE sep_movimientos_invadido.*

END GLOBALS

MAIN
 OPTIONS INPUT WRAP,
 PROMPT LINE LAST,
 ACCEPT KEY CONTROL-I
 DEFER INTERRUPT

	 CALL STARTLOG(FGL_GETENV("USER")||".SEPB015.log")

	 -- entrada de parametros externos

         let g_idSolicitudSeparacion = arg_val(1)
	 let g_invadido              = arg_val(2)
	 let g_asociado              = arg_val(3)
	 let g_diagnostico           = arg_val(4)
	 let g_clasificacion         = arg_val(5)

	 -- si es de tipo 01 C se llama la pantalla para elegir
	 -- registros patronales a ser separados

	 if (g_diagnostico   = '01' and
	     g_clasificacion = 'C'     ) then

	    let g_ejecuta = 'fglgo SEPM020'         ,
			    g_idSolicitudSeparacion ,
			    ' '                     ,
			    g_invadido              ,
			    ' '                     ,
			    g_asociado

	    run g_ejecuta

	    select "ok"
	    from sep_reg_patro_separador a
	    where a.idSolicitudSeparacion = g_idSolicitudSeparacion
	    and   a.nss_separado          = g_invadido
	    and   a.nss_separador         = "INT"
	    group by 1

	    if STATUS <> NOTFOUND then

	       -- cuand se cancela la eleccion de registro patronal

	       delete from sep_reg_patro_separador
	       where  idSolicitudSeparacion = g_idSolicitudSeparacion
	       and    nss_separado  = g_invadido
	       and    nss_separador = "INT"

	       exit program
	    end if

	    error ""

	    open window SEPM020  at 2,2 with form "SEPM020" attribute(border)
	    display "                                                                               "    at 1,1 attribute(reverse)
	    display "SEPB015                SEPARACION DE REGISTROS PATRONALES                      "    at 3,1 attribute(reverse)

	 else -- si no es 01 C

	   error ""

	    open window SEPM020  at 2,2 with form "SEPM020" attribute(border)
	    display "                                                                               "    at 1,1 attribute(reverse)
	    display "SEPB401                SEPARACION DE REGISTROS PATRONALES                      "    at 3,1 attribute(reverse)

	    while true
	      prompt " Desea Realizar la Separacion de Recursos [s/n] ? " for g_enter
	       if g_enter matches "[SsNn]" then
		 if g_enter matches "[Ss]" then
		    exit while
		 else
		    exit program
		 end if
	       end if
	     end while
	 end if -- fin de if si es 01 C

	 display "PROCESANDO SEPARACION SEGUN REGISTROS PATRONALES ..." at 20,2

	 call inicio()  -- asignacion de valores iniciales
         call datos_globales() -- ejecuta querys iniciales necesarios

 if g_clasificacion = 'C' then


    -- se ligan los movimientos de dis_det_aporte con los movimientos
    -- del invadido

    call identifica_mov_a_separar()

    -- se abre pantalla para captura manual de movimientos restantes
    -- a incluir en la separacion

    let g_ejecuta = "fglgo SEPL010 ",g_idSolicitudSeparacion," ",
                    g_invadido," ",
                    g_asociado," ",g_diagnostico," ",g_clasificacion
    run g_ejecuta
    error ""

    call separacion_tipo_c()

    -- para verificar los saldos de la separacion

    call verifica_saldos_separacion()

 else    -- fin de if de clasificacion tipo C

    call separacion_tipo_b()

 end if  -- fin de la clasificacion tipo b

-- se actualiza el estado de la solicitud de separacion de cuentas como
-- proyectada

update sep_det_reg_sol_reclamante
set    estado = 54 ,
       fecha_proceso = g_today
where  correlativo = g_idSolicitudSeparacion

prompt " Separacion de Cuentas Proyectada <enter> para Continuar..."
for char g_enter

END MAIN

FUNCTION inicio()
#asigancion de valores iniciales
--------------------------------

 let g_cero                    = 0
 let g_cargo_sep               = 280
 let g_abono_sep               = 590
 let g_abono_actualizacion_sep = 595
 let g_abono_rendimiento_sep   = 596
 let g_today                   = TODAY

 let g_primer_natural    = MDY(MONTH(g_today),'01',YEAR(g_today))

 call cal_fecha_avant(g_primer_natural,1) returning g_primer_habil

-- funcion para actualizar aporte de vivienda de pesos al valor de aivs

 let sql_cambia_aporte =
     "execute function fn_cambia_aporte(?,?,?) "
 prepare pre_cambia_aporte from sql_cambia_aporte
 declare cur_cambia_aporte cursor for pre_cambia_aporte

-- funcion para provisionar cargo por ajuste operativo

 let sql_prov_cargo =
     "execute function fn_prov_cargo(?,?,?,?,?,?,?,?,?,?,?) "
 prepare pre_prov_cargo from sql_prov_cargo
 declare cur_prov_cargo cursor for pre_prov_cargo

-- funcion para provisionar abono por ajuste operativo

 let sql_prov_abono =
     "execute function fn_prov_abono(?,?,?,?,?,?,?,?,?,?) "
 prepare pre_prov_abono from sql_prov_abono
 declare cur_prov_abono cursor for pre_prov_abono

-- funcion para extraer el precio del dia
 let sql_precio_del_dia =
     " SELECT a.precio_del_dia    ",
     " FROM   glo_valor_accion a  ",
     " WHERE  fecha_valuacion = ? ",
     " AND    codigo_siefore  = ? "
  prepare pre_precio_del_dia from sql_precio_del_dia

END FUNCTION


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

FUNCTION datos_globales()
#trae los datos globales para el proceso
----------------------------------------

define ltxt char(300)

-- datos del invadido

select a.n_unico , USER
into   g_curp_asociado,g_usuario
from   afi_mae_afiliado a
where  a.n_seguro = g_asociado

select "0k"
from   taa_cd_det_cedido a
where  a.n_seguro = g_invadido
and    a.estado   = 99
group by 1

if STATUS <> NOTFOUND then

   select max(a.fecha_trasp)
   into   g_fecha_cedido
   from   taa_cd_det_cedido a
   where  a.n_seguro = g_invadido
   and    a.estado   = 99

   select max(a.fecha_mov_banxico)
   into   g_fecha_reingreso
   from   taa_rcv_recepcion a
   where  a.nss               = g_invadido
   and    a.ident_operacion   = '09'
   and    a.fecha_mov_banxico > g_fecha_cedido
else
   let g_fecha_reingreso = '01/01/1997' -- fecha minima
end if

-- datos globales

select a.*
into   g_rutas.*
from   seg_modulo a
where  a.modulo_cod = "sep"


SELECT "ok"
FROM   sep_movimientos_invadido a
WHERE  a.nss = g_invadido
GROUP BY 1

if status = notfound then

 declare cur_01 cursor for
 select a.nombre_tabla
 from   taa_cd_tab_cuenta a

  foreach cur_01 into g_nombre_tabla
  let ltxt =
  " insert into sep_movimientos_invadido ",
  " select '0',a.*,'0' ",
  " from ",g_nombre_tabla clipped, " a ",
  " where  nss = ? ",
  " and    fecha_conversion >= ? "

  prepare qry from ltxt
  execute qry using g_invadido , g_fecha_reingreso

  end FOREACH
ELSE
   --cpl_1908 se agrega delete para borrar y volver a insertar

   DELETE FROM sep_movimientos_invadido
   WHERE  nss = g_invadido

   declare cur_del cursor for
   select a.nombre_tabla
   from   taa_cd_tab_cuenta a

   foreach cur_del into g_nombre_tabla
   let ltxt =
   " insert into sep_movimientos_invadido ",
    " select '0',a.*,'0' ",
   " from ",g_nombre_tabla clipped, " a ",
   " where  nss = ? ",
   " and    fecha_conversion >= ? "

  prepare qrydel from ltxt
  execute qrydel using g_invadido , g_fecha_reingreso
END FOREACH
END IF

END FUNCTION

FUNCTION identifica_mov_a_separar()
#identifica los movimientos segun registros patronales
------------------------------------------------------

define l_dis_folio               ,
       l_consec_reg_lote         integer


declare cur_02 cursor for
select a.reg_patronal_imss
from   sep_reg_patro_separador a
where  a.idSolicitudSeparacion = g_idSolicitudSeparacion

declare cur_03 cursor for
select a.folio            ,
       a.consec_reg_lote
from   dis_det_aporte    a
where  a.n_seguro          = g_invadido
and    a.reg_patronal_imss = g_reg_patronal


 foreach cur_02 into g_reg_patronal

     foreach cur_03 into l_dis_folio, l_consec_reg_lote

        -- se encuentran los correspondientes a rcv
        -- por registro patronal

        update sep_movimientos_invadido
        set    ind_mov_asociado  = 1
        where  folio            = l_dis_folio
        and    nss              = g_invadido
        and    consecutivo_lote = l_consec_reg_lote
        and    subcuenta        not in (4,8,14)
        and    ind_mov_asociado = 0

        -- se encuentran los correspondientes a viv97
        -- por registro patronal

        update sep_movimientos_invadido
        set    ind_mov_asociado = 1
        where  folio            = l_dis_folio
        and    nss              = g_invadido
        and    consecutivo_lote = l_consec_reg_lote
        and    subcuenta        = 4
        and    tipo_movimiento <> 3

     end foreach
 end foreach

END FUNCTION


FUNCTION separacion_tipo_c()
#provisiona separacion tipo c
------------------------------

--
-- separacion de rcv
--

declare cur_04 cursor for
select a.*
from   sep_movimientos_invadido a
where  a.nss       = g_invadido
and    a.subcuenta not in (4,8,14)
and    a.ind_mov_asociado not in (0,5)


 foreach cur_04 into g_sep_movimientos_invadido.*
     call actualiza_movimiento_separado(g_sep_movimientos_invadido.*)
 end foreach -- fin cur_04

 ---
 --- separacion de viv
 ---

declare cur_05 cursor for
select a.*
from   sep_movimientos_invadido a
where  a.nss       = g_invadido
and    a.subcuenta in (4,8)
and    a.ind_mov_asociado not in (0,5)
--     se utilizan solamente los movimientos de aporte
and    a.tipo_movimiento not in (3,999,990,991,270,610,640)

foreach cur_05 into  g_sep_movimientos_invadido.*
     call actualiza_movimiento_separado_viv(g_sep_movimientos_invadido.*)
end foreach  --fin cur 05

-- se llama la ruta para provisionar los saldos de la separacion

END FUNCTION


FUNCTION actualiza_movimiento_separado(l_reg_mov_separado)
#actualiza el movimiento que se va a separar
#incluyendo traspasos entre siefores
----------------------------------------------------------

DEFINE l_reg_mov_separado         RECORD LIKE sep_movimientos_invadido.*
DEFINE l_reg_dis_cuenta_cargo     RECORD LIKE sep_movimientos_invadido.*
DEFINE l_reg_dis_cuenta_abono     RECORD LIKE sep_movimientos_invadido.*
DEFINE l_reg_tes_solicitud        RECORD LIKE safre_af:tes_solicitud.*
DEFINE l_sep_aporte_receptora     RECORD LIKE safre_af:sep_aporte_receptora.*

DEFINE i                          ,
       lno_decimo                 ,
       l_movimiento               ,
       l_existe                   ,
       l_ind_tes                  ,
       l_ind_folio_origen         SMALLINT

DEFINE l_dis_cuenta               CHAR(015)


DEFINE lmto_acc_decimo      ,
       lmto_acciones        ,
       lmto_pesos           ,
       l_resto              ,
       lsaldo_sie           ,
       lsaldo_sie_0         ,
       lsaldo_sie_1         DEC(16,6)

DEFINE l_fecha_decimo       DATE

DEFINE l_reg_sep_transferencias RECORD
       folio           INTEGER   ,
       nss             CHAR(011) ,
       fecha           DATE      ,
       tipo            SMALLINT
END RECORD


DEFINE arr_verifica_neg  ARRAY[4] OF RECORD
       nss          char(011) ,
       siefore      smallint  ,
       monto        dec(16,6)
END RECORD

DEFINE r_verifica_neg RECORD
       nss          char(011) ,
       siefore      smallint  ,
       monto        dec(16,6)
END RECORD


#############################################################
##### SECCION DE CARGO Y ABONO MOVIMIENTO ORIGINAL  #########
#############################################################

CASE l_reg_mov_separado.tipo_movimiento
WHEN 1
   LET l_movimiento = g_abono_sep
   EXIT CASE
WHEN 2
   LET l_movimiento = g_abono_actualizacion_sep
   EXIT CASE
WHEN 3
   LET l_movimiento = g_abono_rendimiento_sep
   EXIT CASE
WHEN 4
   LET l_movimiento = g_abono_rendimiento_sep
   EXIT CASE
WHEN 5
   LET l_movimiento = g_abono_rendimiento_sep
   EXIT CASE
OTHERWISE
   LET l_movimiento = l_reg_mov_separado.tipo_movimiento
   EXIT CASE
END CASE
#
# abono del aporte al separador
#


---
--- en caso de ser movimiento de traspaso receptora se separa solo la parte
--- del asociado
---
select "ok"
from   sep_aporte_receptora a
where  a.idSolicitudSeparacion = g_idSolicitudSeparacion
and    a.idSepMovimientosInvadido = l_reg_mov_separado.idSepMovimientosInvadido
and    a.subcuenta = l_reg_mov_separado.subcuenta
and    a.siefore   = l_reg_mov_separado.siefore
group by 1

if status <> notfound then
   select a.*
   into   l_sep_aporte_receptora.*
   from   sep_aporte_receptora a
   where  a.idSolicitudSeparacion = g_idSolicitudSeparacion
   and  a.idSepMovimientosInvadido = l_reg_mov_separado.idSepMovimientosInvadido
   and  a.subcuenta = l_reg_mov_separado.subcuenta
   and  a.siefore   = l_reg_mov_separado.siefore

   let l_reg_mov_separado.monto_en_acciones =
       l_sep_aporte_receptora.acciones_asociado
   let l_reg_mov_separado.monto_en_pesos    =
       l_sep_aporte_receptora.acciones_asociado *
               l_reg_mov_separado.precio_accion
end if

LET lmto_acciones = l_reg_mov_separado.monto_en_acciones
LET lmto_pesos    = l_reg_mov_separado.monto_en_pesos

CALL inserta_en_historico(l_movimiento         ,
                          g_asociado           ,
                          g_curp_asociado      ,
                          l_reg_mov_separado.* ,
                          lmto_acciones        ,
                          lmto_pesos           )

#
# cargo por el monto del aporte al separado
#

LET l_movimiento  = 280
LET lmto_acciones = l_reg_mov_separado.monto_en_acciones * -1
LET lmto_pesos    = l_reg_mov_separado.monto_en_pesos    * -1

CALL inserta_en_historico(l_movimiento            ,
                          g_invadido              ,
                          l_reg_mov_separado.curp ,
                          l_reg_mov_separado.*    ,
                          lmto_acciones           ,
                          lmto_pesos              )

#############################################################
##### FIN CARGO Y ABONO MOVIMIENTO ORIGINAL         #########
#############################################################


#############################################################
##### VERIFICAR MOVIMIENTOS DE TES                      #####
#############################################################

WHENEVER ERROR CONTINUE
  DROP TABLE sep_transferencias
  DROP TABLE sep_siefore_monto
WHENEVER ERROR STOP

CREATE  TEMP TABLE sep_transferencias(folio integer  ,
                                     nss   char(011) ,
                                     fecha date      ,
                                     tipo  smallint  )

CREATE TEMP TABLE sep_siefore_monto (siefore smallint ,
                                     monto dec(16,6))

#saldo inicial
insert into sep_siefore_monto values (l_reg_mov_separado.siefore ,
                                      l_reg_mov_separado.monto_en_acciones)

#
# ocurrencias de tes_solicitud
#
INSERT INTO sep_transferencias
SELECT a.folio           ,
       a.nss             ,
       a.fecha_traspaso  ,
       a.tipo_traspaso
FROM   safre_af:tes_solicitud a
WHERE  a.nss            = g_invadido
AND    a.fecha_traspaso > l_reg_mov_separado.fecha_conversion
AND    a.estado = 103

#
# ocurrencias de decimos
#

INITIALIZE l_fecha_decimo TO NULL

SELECT a.fecha_conversion
INTO   l_fecha_decimo
FROM   cta_his_decimo a
WHERE  a.nss              = g_invadido
AND    a.no_transferencia = 1
AND    a.subcuenta        = l_reg_mov_separado.subcuenta
GROUP BY 1

IF STATUS = NOTFOUND THEN

   SELECT a.fecha_conversion
   INTO   l_fecha_decimo
   FROM   cta_his_decimo a
   WHERE  a.nss              = g_invadido
   AND    a.no_transferencia = 10
   AND    a.subcuenta        = l_reg_mov_separado.subcuenta
   GROUP BY 1

END IF


IF l_fecha_decimo > l_reg_mov_separado.fecha_conversion THEN

    INSERT INTO sep_transferencias
    SELECT a.folio            ,
           a.nss              ,
           a.fecha_conversion ,
           "99"
    FROM   cta_his_decimo a
    WHERE  a.nss       = g_invadido
    AND    a.subcuenta = l_reg_mov_separado.subcuenta
    GROUP BY 1,2,3

END IF

LET lno_decimo  = 0

DECLARE cur_transferencias CURSOR FOR
 SELECT a.*
 FROM   sep_transferencias a
 ORDER BY a.fecha

 FOREACH cur_transferencias INTO l_reg_sep_transferencias.*

     CASE l_reg_sep_transferencias.tipo
         WHEN 99
             LET lno_decimo = lno_decimo + 1

             CALL verifica_existe_transf(l_reg_sep_transferencias.folio  ,
                                         l_reg_mov_separado.subcuenta    ,
                                         l_reg_sep_transferencias.fecha  ,
                                         "99"
                                         )
             RETURNING l_existe,l_reg_dis_cuenta_cargo.*,
                       l_reg_dis_cuenta_abono.*

             IF lno_decimo = 1 THEN

                LET lsaldo_sie = 0

                SELECT sum(a.monto)
                INTO   lsaldo_sie
                FROM   sep_siefore_monto a
                WHERE  a.siefore = 2

                IF  lsaldo_sie <> 0 THEN

                    SELECT TRUNC((lsaldo_sie/10),6)
                    INTO lmto_acc_decimo
                    FROM tab_afore_local a

                END IF
             END IF

             IF l_existe AND lmto_acc_decimo <> 0  THEN

                   # verifica monto de ultima transferencia #

                   IF l_reg_sep_transferencias.fecha = '01/31/2008' THEN

                      SELECT trunc(sum(a.monto),6)
                      INTO lmto_acc_decimo
                      FROM  sep_siefore_monto  a
                      WHERE a.siefore   = 2

                   END IF

                      # cargo en el separador en la siefore de cargo

                      LET l_movimiento = 215

                      LET lmto_acciones = lmto_acc_decimo * -1
                      LET lmto_pesos    = lmto_acciones   *
                                          l_reg_dis_cuenta_cargo.precio_accion

                      # el decimo se resta del saldo
                      INSERT INTO sep_siefore_monto
                      VALUES (l_reg_dis_cuenta_cargo.siefore, lmto_acciones)

                      CALL inserta_en_historico(l_movimiento             ,
                                                g_asociado               ,
                                                g_curp_asociado          ,
                                                l_reg_dis_cuenta_cargo.* ,
                                                lmto_acciones            ,
                                                lmto_pesos               )

                      # abono en el separado en la siefore de cargo

                      LET l_movimiento  = 15
                      LET lmto_acciones = lmto_acc_decimo
                      LET lmto_pesos    = lmto_acciones  *
                                          l_reg_dis_cuenta_cargo.precio_accion

                      CALL inserta_en_historico(l_movimiento               ,
                                                g_invadido                 ,
                                                l_reg_dis_cuenta_cargo.curp,
                                                l_reg_dis_cuenta_cargo.*   ,
                                                lmto_acciones              ,
                                                lmto_pesos                 )

                      # abono en el separador en la siefore de abono

                      LET l_movimiento = 15

                      LET lmto_pesos    = lmto_acc_decimo * l_reg_dis_cuenta_cargo.precio_accion
                      LET lmto_acciones = lmto_pesos    / l_reg_dis_cuenta_abono.precio_accion

                      INSERT INTO sep_siefore_monto values (l_reg_dis_cuenta_abono.siefore ,
                                                            lmto_acciones)

                      CALL inserta_en_historico(l_movimiento                ,
                                                g_asociado                  ,
                                                g_curp_asociado             ,
                                                l_reg_dis_cuenta_abono.*    ,
                                                lmto_acciones               ,
                                                lmto_pesos                  )

                      # cargo en el separado en la siefore de abono

                      LET l_movimiento = 215

                      LET lmto_pesos    = lmto_acc_decimo *
                                          l_reg_dis_cuenta_cargo.precio_accion
                                          * - 1

                      LET lmto_acciones = lmto_pesos /
                                          l_reg_dis_cuenta_abono.precio_accion

                      CALL inserta_en_historico(l_movimiento                   ,
                                                g_invadido                     ,
                                                l_reg_dis_cuenta_abono.curp ,
                                                l_reg_dis_cuenta_abono.*       ,
                                                lmto_acciones                  ,
                                                lmto_pesos                     )

                 END IF
         EXIT CASE
         OTHERWISE
            DECLARE cur_sol_trans CURSOR FOR
             SELECT a.*
             FROM   tes_solicitud a
             WHERE  a.folio          = l_reg_sep_transferencias.folio
             AND    a.nss            = l_reg_sep_transferencias.nss
             AND    a.fecha_traspaso = l_reg_sep_transferencias.fecha
             AND    a.tipo_traspaso  = l_reg_sep_transferencias.tipo
             GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12

             FOREACH cur_sol_trans INTO   l_reg_tes_solicitud.*
             END FOREACH

             LET l_ind_folio_origen = 0

             SELECT a.ind_folio_origen
             INTO   l_ind_folio_origen
             FROM   tes_tipo_id_aportante a
             WHERE  a.tipo_traspaso = l_reg_sep_transferencias.tipo

             CASE l_ind_folio_origen
               WHEN 0

                     IF l_reg_tes_solicitud.tipo_traspaso = 10 THEN
                        LET l_ind_tes = 2
                     ELSE
                        LET l_ind_tes = 1
                     END IF

                     CALL verifica_existe_transf(l_reg_tes_solicitud.folio          ,
                                                 l_reg_mov_separado.subcuenta       ,
                                                 l_reg_tes_solicitud.fecha_traspaso ,
                                                 l_ind_tes                          )
                     RETURNING l_existe,l_reg_dis_cuenta_cargo.*,l_reg_dis_cuenta_abono.*

                     LET lsaldo_sie_0 = 0

                     SELECT SUM(a.monto)
                     INTO   lsaldo_sie_0
                     FROM   sep_siefore_monto a
                     WHERE  a.siefore = l_reg_dis_cuenta_cargo.siefore

                     IF lsaldo_sie_0 IS NULL THEN LET lsaldo_sie_0 = 0 END IF

                     IF (l_existe AND  lsaldo_sie_0 <> 0) THEN

                     LET l_resto = lsaldo_sie_0

                      # cargo en el separador en la siefore de cargo

                      LET l_movimiento = 210
                      LET lmto_acciones = l_resto  * - 1
                      LET lmto_pesos    = lmto_acciones *
                                          l_reg_dis_cuenta_cargo.precio_accion

                      # se inserta el movimiento de cargo
                      INSERT INTO sep_siefore_monto
                      values (l_reg_dis_cuenta_cargo.siefore, lmto_acciones)

                      CALL inserta_en_historico(l_movimiento              ,
                                                g_asociado                ,
                                                g_curp_asociado           ,
                                                l_reg_dis_cuenta_cargo.*  ,
                                                lmto_acciones             ,
                                                lmto_pesos                )

                      # abono en el separado en la siefore de cargo

                      LET l_movimiento = 1
                      LET lmto_acciones = l_resto
                      LET lmto_pesos    = lmto_acciones *
                                          l_reg_dis_cuenta_cargo.precio_accion

                      CALL inserta_en_historico(l_movimiento                ,
                                                g_invadido                  ,
                                                l_reg_dis_cuenta_cargo.curp ,
                                                l_reg_dis_cuenta_cargo.*    ,
                                                lmto_acciones               ,
                                                lmto_pesos                  )

                      # abono en el separador en la siefore de abono

                      LET l_movimiento = 1
                      LET lmto_pesos    = l_resto * l_reg_dis_cuenta_cargo.precio_accion
                      LET lmto_acciones = lmto_pesos / l_reg_dis_cuenta_abono.precio_accion

                      # se insert el movimiento de abono
                      INSERT INTO sep_siefore_monto values (l_reg_dis_cuenta_abono.siefore  ,
                                                            lmto_acciones)

                      CALL inserta_en_historico(l_movimiento             ,
                                                g_asociado               ,
                                                g_curp_asociado          ,
                                                l_reg_dis_cuenta_abono.* ,
                                                lmto_acciones            ,
                                                lmto_pesos               )

                      # cargo en el separado en la siefore de abono

                      LET l_movimiento = 210
                      LET lmto_pesos    = l_resto * l_reg_dis_cuenta_cargo.precio_Accion * - 1
                      LET lmto_acciones = lmto_pesos / l_reg_dis_cuenta_abono.precio_accion

                      CALL inserta_en_historico(l_movimiento                ,
                                                g_invadido                  ,
                                                l_reg_dis_cuenta_abono.curp ,
                                                l_reg_dis_cuenta_abono.*    ,
                                                lmto_acciones               ,
                                                lmto_pesos                  )

                 END IF
               EXIT CASE
               WHEN 1
                  IF (l_reg_tes_solicitud.folio_origen = l_reg_mov_separado.folio AND
                      l_reg_tes_solicitud.fecha_solicitud = l_reg_mov_separado.fecha_conversion)
                  THEN

                     CALL verifica_existe_transf(l_reg_tes_solicitud.folio          ,
                                                 l_reg_mov_separado.subcuenta       ,
                                                 l_reg_tes_solicitud.fecha_traspaso ,
                                                 "1"                                )

                     RETURNING l_existe,l_reg_dis_cuenta_cargo.*,l_reg_dis_cuenta_abono.*

                     LET lsaldo_sie_1 = 0

                     SELECT sum(a.monto)
                     INTO   lsaldo_sie_1
                     FROM   sep_siefore_monto a
                     WHERE  a.siefore = l_reg_dis_cuenta_cargo.siefore

                     IF lsaldo_sie_1 IS NULL THEN LET lsaldo_sie_1 = 0 END IF

                     IF (l_existe AND
                         lsaldo_sie_1 <> 0 ) THEN

                      # cargo en el separador en la siefore de cargo

                      LET l_movimiento  = 210
                      LET lmto_acciones = lsaldo_sie_1 * -1
                      LET lmto_pesos    = lmto_acciones * l_reg_dis_cuenta_cargo.precio_accion

                      INSERT INTO sep_siefore_monto values (l_reg_dis_cuenta_cargo.siefore ,
                                                            lmto_acciones)

                      CALL inserta_en_historico(l_movimiento             ,
                                                g_asociado               ,
                                                g_curp_asociado          ,
                                                l_reg_dis_cuenta_cargo.* ,
                                                lmto_acciones            ,
                                                lmto_pesos               )

                      # abono en el separado en la siefore de cargo

                      LET l_movimiento  = 1
                      LET lmto_acciones = lsaldo_sie_1
                      LET lmto_pesos    = lmto_acciones * l_reg_dis_cuenta_cargo.precio_accion

                      CALL inserta_en_historico(l_movimiento                ,
                                                g_invadido                  ,
                                                l_reg_dis_cuenta_cargo.curp ,
                                                l_reg_dis_cuenta_cargo.*    ,
                                                lmto_acciones               ,
                                                lmto_pesos                  )

                      # abono en el separador en la siefore de abono

                      LET l_movimiento  = 1
                      LET lmto_pesos    = lsaldo_sie_1 * l_reg_dis_cuenta_cargo.precio_accion
                      LET lmto_acciones = lmto_pesos / l_reg_dis_cuenta_abono.precio_accion

                      INSERT INTO sep_siefore_monto values (l_reg_dis_cuenta_abono.siefore ,
                                                            lmto_acciones)

                      CALL inserta_en_historico(l_movimiento             ,
                                                g_asociado               ,
                                                g_curp_asociado          ,
                                                l_reg_dis_cuenta_abono.* ,
                                                lmto_acciones            ,
                                                lmto_pesos               )

                      # cargo en el separado en la siefore de abono

                      LET l_movimiento  = 210
                      LET lmto_pesos    = lsaldo_sie_1 *
                                          l_reg_dis_cuenta_cargo.precio_accion
                                          * - 1
                      LET lmto_acciones = lmto_pesos /
                                          l_reg_dis_cuenta_abono.precio_accion

                      CALL inserta_en_historico(l_movimiento                ,
                                                g_invadido                  ,
                                                l_reg_dis_cuenta_abono.curp ,
                                                l_reg_dis_cuenta_abono.*    ,
                                                lmto_acciones               ,
                                                lmto_pesos                  )

                     END IF
                  END IF
               EXIT CASE
             END CASE
         EXIT CASE
     END CASE
 END FOREACH

END FUNCTION

FUNCTION actualiza_movimiento_separado_viv(l_reg_mov_separado)
# registra el detalle de los montos a separar de vivienda
# ademas de la correspondiente actualizacion para el caso de
# aportes anteriores a la conversion de aivs
-------------------------------------------------------------

define l_reg_mov_separado     record like sep_movimientos_invadido.*
define l_sep_aporte_receptora record like sep_aporte_receptora.*

define l_movimiento          smallint

define l_fecha_aivs          date

define l_precio                dec(16,6),
       l_acciones                      ,
       l_pesos                         ,
       lmto_pesos                      ,
       lmto_acciones           dec(16,2)

let    l_fecha_aivs =       '08/01/2004'

case l_reg_mov_separado.tipo_movimiento
 when 1
      let l_movimiento = g_abono_sep
      exit case
 when 2
      let l_movimiento = g_abono_actualizacion_sep
      exit case
 when 3
      let l_movimiento = g_abono_rendimiento_sep
      exit case
 otherwise
      let l_movimiento = 597
      exit case
end case
-- si el movimiento es anterior a la conversion
-- de aivs se debe hacer la conversion de pesos
-- a aivs de acuerdo a la fecha valor

if l_reg_mov_separado.fecha_valor <= l_fecha_aivs then
   foreach cur_cambia_aporte using l_reg_mov_separado.monto_en_pesos  ,
                                   l_reg_mov_separado.fecha_valor ,
                                   l_fecha_aivs
                             into  l_acciones
          let l_reg_mov_separado.monto_en_acciones = l_acciones
   end foreach
end if

---
--- para tomar solo el movimiento a separar en caso de ser traspaso receptora
---

select "ok"
from   sep_aporte_receptora a
where  a.idSolicitudSeparacion = g_idSolicitudSeparacion
and    a.idSepMovimientosInvadido = l_reg_mov_separado.idSepMovimientosInvadido
and    a.subcuenta = l_reg_mov_separado.subcuenta
and    a.siefore   = l_reg_mov_separado.siefore
group by 1

if status <> notfound then
   select a.*
   into   l_sep_aporte_receptora.*
   from   sep_aporte_receptora a
   where  a.idSolicitudSeparacion = g_idSolicitudSeparacion
   and  a.idSepMovimientosInvadido = l_reg_mov_separado.idSepMovimientosInvadido
   and  a.subcuenta = l_reg_mov_separado.subcuenta
   and  a.siefore   = l_reg_mov_separado.siefore

   let l_reg_mov_separado.monto_en_acciones =
       l_sep_aporte_receptora.acciones_asociado

   let l_precio = 0

   select a.precio_del_dia
   into   l_precio
   from   glo_valor_accion a
   where  a.codigo_siefore = 11
   and    a.fecha_valuacion = l_reg_mov_separado.fecha_valor

   let l_reg_mov_separado.monto_en_pesos    =
       l_sep_aporte_receptora.acciones_asociado * l_precio

end if

let l_reg_mov_separado.siefore = 11 -- siefore de vivienda

let lmto_acciones = l_reg_mov_separado.monto_en_acciones
let lmto_pesos    = l_reg_mov_separado.monto_en_pesos

 -- se inserta el abono correspondiente al asociado

CALL inserta_en_historico(l_movimiento         ,
                          g_asociado           ,
                          g_curp_asociado      ,
                          l_reg_mov_separado.* ,
                          lmto_acciones        ,
                          lmto_pesos           )

 -- se inserta el corgo correspondiente al invadido

LET l_movimiento  = 280
LET lmto_acciones = l_reg_mov_separado.monto_en_acciones * -1
LET lmto_pesos    = l_reg_mov_separado.monto_en_pesos    * -1

CALL inserta_en_historico(l_movimiento            ,
                          g_invadido              ,
                          l_reg_mov_separado.curp ,
                          l_reg_mov_separado.*    ,
                          lmto_acciones           ,
                          lmto_pesos              )
END FUNCTION


FUNCTION verifica_existe_transf(lfolio,lsubcuenta,lfechatrasp,lindicador)
#ld----------------------------------------------------------------------

DEFINE ltxt CHAR(300)
DEFINE lexiste SMALLINT
DEFINE lsubcuenta          SMALLINT
DEFINE lindicador          SMALLINT
DEFINE lfolio              INTEGER
DEFINE lfechatrasp         DATE
DEFINE lcargo_discuenta    RECORD LIKE sep_movimientos_invadido.*
DEFINE labono_discuenta    RECORD LIKE sep_movimientos_invadido.*
DEFINE lmov      SMALLINT

LET lexiste     = 0

   LET ltxt = " SELECT a.* "                    ,
              " FROM  sep_movimientos_invadido a ",
              " WHERE a.folio = ? "             ,
              " AND   a.nss   = ? "             ,
              " AND   a.tipo_movimiento = ?   " ,
              " AND   a.subcuenta = ?         " ,
              " AND   a.fecha_conversion = ?  "

   PREPARE qry_verifica FROM ltxt
   DECLARE cur_verifica CURSOR FOR qry_verifica


  CASE lindicador
  WHEN 1
    LET lmov = 210
    EXIT CASE
  WHEN 2
    LET lmov = 950
    EXIT CASE
  OTHERWISE
    LET lmov = 215
    EXIT CASE
  END CASE

   FOREACH cur_verifica USING lfolio                   ,
                              g_invadido               ,
                              lmov                     ,
                              lsubcuenta               ,
                              lfechatrasp
                        INTO  lcargo_discuenta.*
           LET lexiste = 1
   END FOREACH

  CASE lindicador
  WHEN 1
    LET lmov = 1
    EXIT CASE
  WHEN 2
    LET lmov = 951
    EXIT CASE
  OTHERWISE
    LET lmov = 15
    EXIT CASE
  END CASE

   FOREACH cur_verifica USING lfolio                      ,
                              g_invadido                  ,
                              lmov                        ,
                              lsubcuenta                  ,
                              lfechatrasp
                        INTO  labono_discuenta.*
           LET lexiste = 1
   END FOREACH

   RETURN lexiste,lcargo_discuenta.*,labono_discuenta.*

END FUNCTION

FUNCTION separacion_tipo_b()
# inserta el detalle de los movimientos de tipo b asi como registra
# la provision de los saldos de dicha separacion
-------------------------------------------------------------------

define l_reg_mov_separado record like sep_movimientos_invadido.*

define l_movimiento       smallint

define lmto_acciones      dec(16,6) ,
       lmto_pesos         dec(16,6)


update sep_movimientos_invadido
set    ind_mov_asociado   = 1
where  nss = g_invadido
and    tipo_movimiento <> 999

declare cur_tipo_b cursor for
select a.*
from   sep_movimientos_invadido a
where  a.nss = g_invadido
and    a.ind_mov_asociado = 1

foreach cur_tipo_b into l_reg_mov_separado.*

  case l_reg_mov_separado.tipo_movimiento
    when 1
         let l_movimiento = g_abono_sep
         exit case
    when 2
         let l_movimiento = g_abono_actualizacion_sep
         exit case
    when 3
         let l_movimiento = g_abono_rendimiento_sep
         exit case
    when 4
         let l_movimiento = g_abono_rendimiento_sep
         exit case
    when 5
         let l_movimiento = g_abono_rendimiento_sep
         exit case
    otherwise
         let l_movimiento = g_abono_rendimiento_sep
         exit case
   end case

let lmto_acciones = l_reg_mov_separado.monto_en_acciones
let lmto_pesos    = l_reg_mov_separado.monto_en_pesos

-- se inserta el movimiento de abono en el asociado

call inserta_en_historico(l_movimiento         ,
                          g_asociado           ,
                          g_curp_asociado      ,
                          l_reg_mov_separado.* ,
                          lmto_acciones        ,
                          lmto_pesos           )

-- se inserta el movimiento de cargo en el invadido

let lmto_acciones = l_reg_mov_separado.monto_en_acciones * -1
let lmto_pesos    = l_reg_mov_separado.monto_en_pesos    * -1

call inserta_en_historico(280                     ,
                          g_invadido              ,
                          l_reg_mov_separado.curp ,
                          l_reg_mov_separado.*    ,
                          lmto_acciones           ,
                          lmto_pesos              )

end foreach

END FUNCTION


FUNCTION inserta_en_historico(lmovimiento,lnss,lcurp      ,
                              l_sep_separacion_preliminar ,
                              mtoacciones,mtopesos        )
# inserta en el historico de detalle los movimientos a ser considerados
# en el saldo de la separacion de cuentas
-----------------------------------------------------------------------

DEFINE lmovimiento          SMALLINT
DEFINE l_sep_separacion_preliminar RECORD LIKE sep_movimientos_invadido.*
DEFINE lnss                 CHAR(011),
       lcurp                CHAR(018),
       mtoacciones          DEC(16,6),
       mtopesos             DEC(16,6)

 insert into sep_separacion_preliminar values (
            g_idSolicitudSeparacion                              ,
            l_sep_separacion_preliminar.idSepMovimientosInvadido ,
            l_sep_separacion_preliminar.tipo_movimiento          ,
            l_sep_separacion_preliminar.subcuenta                ,
            l_sep_separacion_preliminar.siefore                  ,
            l_sep_separacion_preliminar.folio                    ,
            l_sep_separacion_preliminar.consecutivo_lote         ,
            lnss                                                 ,
            lcurp                                                ,
            l_sep_separacion_preliminar.folio_sua                ,
            l_sep_separacion_preliminar.fecha_pago               ,
            l_sep_separacion_preliminar.fecha_valor              ,
            l_sep_separacion_preliminar.fecha_conversion         ,
            mtopesos                                             ,
            mtoacciones                                          ,
            l_sep_separacion_preliminar.precio_accion            ,
            l_sep_separacion_preliminar.dias_cotizados           ,
            l_sep_separacion_preliminar.sucursal                 ,
            l_sep_separacion_preliminar.id_aportante             ,
            l_sep_separacion_preliminar.estado                   ,
            l_sep_separacion_preliminar.fecha_proceso            ,
            l_sep_separacion_preliminar.usuario                  ,
            l_sep_separacion_preliminar.fecha_archivo            ,
            l_sep_separacion_preliminar.etiqueta                 )

END FUNCTION

FUNCTION verifica_saldos_separacion()
# verifica que la suma de las entradas y salidas por subcuenta y siefore
# de la separacion coincida con el saldo origen de la cuenta invadida
# antes de la separacion
------------------------------------------------------------------------
DEFINE l_siefore        SMALLINT      #-- CPL-1349 --#

define i,j,k                 ,
       l_verifica_provision  smallint

define lfecha_precio   date

define resultado        ,
       resultado_abs    ,
       lprecio          dec(16,6),
       limite_dif       dec(4,2)

-- arreglo dimensiones nss,subcuenta,siefore

define arr_verifica array[2,30,20] of record
       monto        dec(16,6)
end record

define reg_verifica record
       nss          char(011) ,
       subcuenta    smallint  ,
       siefore      smallint  ,
       monto        dec(16,6)
end record

define l_reg_ajuste  record like sep_movimientos_invadido.*

-- maxima diferencia admitida en la separacion por subcuenta-siefore
let limite_dif      = 0.01
LET l_siefore = 0     #-- CPL-1349 --#

 -- se inicializa en cero todo el arreglo en las tres dimensiones

for i = 1 to 2
  for j = 1 to 30
    for k = 1 to 20
        let arr_verifica[i,j,k].monto = 0
    end for
  end for
end for

--- verificacion 1
--- se verifica las entradas vs salidas en invadido y asociado
---

declare cur_1_verifica cursor for
select  a.nss                 ,
        a.subcuenta           ,
        a.siefore             ,
        abs(sum(monto_en_acciones))
from    sep_separacion_preliminar  a
where   a.idSolicitudSeparacion = g_idSolicitudSeparacion
and     a.nss   in (g_invadido,g_asociado)
group by 1,2,3
order by 1,2,3

foreach cur_1_verifica into reg_verifica.*

  if reg_verifica.nss = g_invadido then
     let i = 1
  else
     let i = 2
  end if

  let j = reg_verifica.subcuenta
  let k = reg_verifica.siefore

  let arr_verifica[i,j,k].monto =
      reg_verifica.monto

end foreach

--- se comparan los valores de invadido y el asociado
let resultado = 0
for j = 1 to 30
    for k = 1 to 20

        let resultado = arr_verifica[1,j,k].monto - arr_verifica[2,j,k].monto

        if resultado <> 0 then
           error"Separacion no procede por inconsistencia en datos..."
           update sep_det_reg_sol_reclamante
           set    estado                = 20
           where  correlativo= g_idSolicitudSeparacion
        end if
    end for
end for -- fin verificacion 1

 -- se inicializa a cero todo el arreglo
for i = 1 to 2
  for j = 1 to 30
    for k = 1 to 20
        let arr_verifica[i,j,k].monto = 0
    end for
  end for
end for
---
--- verificacion 2 suma de los montos de separacion de cuentas
--- vs montos originales en el invadido
---

declare cur_2_verifica cursor for
 -- se seleccionan los montos de la separacion
select "SEPARACION"               ,
       a.subcuenta                ,
       a.siefore                  ,
       sum(a.monto_en_acciones)
from   sep_separacion_preliminar a
where  a.idSolicitudSeparacion = g_idSolicitudSeparacion
and    a.nss        in (g_invadido)
group by 1,2,3
order by 1,2,3

foreach cur_2_verifica into reg_verifica.*

   let j = reg_verifica.subcuenta
   let k = reg_verifica.siefore

   -- se asignan los montos al arreglo 1

   let arr_verifica[1,j,k].monto =
       reg_verifica.monto

   -- se asigna el saldo del invadido al arreglo 2

#-- CPL-1349 INI --#

    SELECT codigo_siefore INTO l_siefore
     FROM cta_regimen
    WHERE nss = g_invadido
    AND subcuenta = reg_verifica.subcuenta

    IF SQLCA.SQLCODE < 0 THEN
       ERROR "NO EXISTE SIEFORE PARA EL NSS: ",g_invadido, " SUBCUENTA: ",reg_verifica.subcuenta
       SLEEP 3
       EXIT PROGRAM
    END IF

#-- CPL-1349 FIN --#

   select abs(sum(monto_en_acciones))
   into   arr_verifica[2,j,k].monto
   from   dis_cuenta a
   where  a.nss       = g_invadido
   and    a.subcuenta = reg_verifica.subcuenta
   and    a.siefore   = l_siefore          #-- CPL-1349 --#

end foreach

let resultado = 0
let resultado_abs = 0
-- se comparan los montos por subcuenta y siefore entre el saldo
-- del invadido y el saldo de la separacion incluyendo tanto al
-- asociado como al invadido

for j = 1 to 30
    for k = 1 to 20
      -- se obtiene diferencia

      let resultado = (arr_verifica[1,j,k].monto + arr_verifica[2,j,k].monto)

        if resultado > limite_dif then
           continue for
        end if

        if resultado < 0 then
           -- convierte en diferencia en  positivo
           let resultado_abs = (resultado * -1)
        else
           let resultado_abs = resultado
        end if

        if resultado_abs > limite_dif then

           -- si el valor absoluto de la diferencia es mayor
           -- al limite: 0.01 la ejecucion termina con error

           error"Separacion no procede por inconsistencia en datos..."

           update sep_det_reg_sol_reclamante
           set    estado                = 20
           where  correlativo = g_idSolicitudSeparacion
           sleep 10
           exit program

        end if

        -- si la diferencia es igual o menor al valor limite
        -- se genera movimiento de ajuste para la subcuenta y
        -- siefore respectivos

        if resultado <> 0 then -- si el saldo origen es distinto dentro del
                               -- limite

           initialize l_reg_ajuste.* to null

           let l_reg_ajuste.idSepMovimientosInvadido = 0

           let l_reg_ajuste.subcuenta           = j
           let l_reg_ajuste.siefore             = k
           let l_reg_ajuste.folio               = 0
           let l_reg_ajuste.consecutivo_lote    = 0
           let l_reg_ajuste.folio_sua           = "0"

           if k = 11 then
              let lfecha_precio = g_primer_natural
           else
              let lfecha_precio = g_primer_habil
           end if
           let l_reg_ajuste.fecha_pago   = lfecha_precio
           let l_reg_ajuste.fecha_valor  = lfecha_precio
           let l_reg_ajuste.fecha_conversion = g_primer_habil

           let lprecio = 0
           -- extrae el precio para la siefore k
           execute pre_precio_del_dia using lfecha_precio ,
                                            k
                                      into  lprecio

           let l_reg_ajuste.precio_accion = lprecio
           let l_reg_ajuste.id_aportante  = 'AJUSTE SEP'
           let l_reg_ajuste.fecha_proceso = g_today
           let l_reg_ajuste.usuario       = g_usuario

           let l_reg_ajuste.monto_en_acciones = resultado --* -1
           let l_reg_ajuste.monto_en_pesos =
                                  l_reg_ajuste.monto_en_acciones *
                                  lprecio

           let l_reg_ajuste.tipo_movimiento = 620

            call inserta_en_historico(620 ,
                                      g_asociado                        ,
                                      g_curp_asociado                   ,
                                      l_reg_ajuste.*                    ,
                                      l_reg_ajuste.monto_en_acciones    ,
                                      l_reg_ajuste.monto_en_pesos       )

           let l_reg_ajuste.tipo_movimiento = 610

            call inserta_en_historico(610 ,
                                      g_invadido                        ,
                                      " "                  ,
                                      l_reg_ajuste.*                    ,
                                      -l_reg_ajuste.monto_en_acciones    ,
                                      -l_reg_ajuste.monto_en_pesos       )

         end if  -- fin if de insertar movimiento de cargo - abono por ajuste
    end for
end for -- fin verificacion 2

END FUNCTION

