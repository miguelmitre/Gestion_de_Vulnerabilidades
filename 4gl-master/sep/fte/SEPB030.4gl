######################################################################### 
#MODULO       : SEP
#PROGRAMA     : SEPB030
#DESCRIPCION  : Realiza verficaciones de errores comunes en la historia
#             : de los movimientos involucrados en la separacion de cuentas
#CREACION     : 02 MAYO 2008
#AUTOR        : JESUS YAÑEZ MORENO
#MODIFICACION : Resstructura del Modulo de Separacion
#FECHA MOD    : 3 abr 2010
#POR          : JESUS YAÑEZ MORENO
#########################################################################

DATABASE safre_af

GLOBALS

-- parametros globales

define g_idSolicitudSeparacion  integer   ,
       g_invadido               char(011) ,
       g_asociado               char(011) ,
       g_diagnostico            char(002) ,
       g_clasificacion          char(001)

define g_today                  ,
       g_fecha_cedido           ,
       g_fecha_reingreso        date 

define g_rutas                  RECORD LIKE seg_modulo.* 

define g_n_unico_separador      char(0018),
       g_n_unico_separado       char(0018),
       g_ejecuta                char(1000),
       g_enter                  char(0001),
       g_txt                    char(0050),
       g_nombre_tabla           char(0050)

define dda_folio                 integer   ,
       dda_consec                integer   ,
       g_folio_transf            integer   ,
       ts_fecha_trasp            date      ,
       g_nombre_dis_cuenta       char(012) ,
       g_txt_dis_cuenta          char(150) 

END  GLOBALS 

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

CALL STARTLOG("SEPB030.log")

# entrada de parametros externos

let g_idSolicitudSeparacion = arg_val(1) 
let g_invadido              = arg_val(2)
LET g_asociado              = arg_val(3) 
LET g_diagnostico           = arg_val(4) 
LET g_clasificacion         = arg_val(5) 

OPEN   WINDOW  SEPM020  AT  2,2  WITH FORM "SEPM020" ATTRIBUTE(BORDER)
    DISPLAY  "                                                                               "   AT  1,1  ATTRIBUTE  (REVERSE)
    DISPLAY  "SEPB030              VERIFICACION DE INCONSISTENCIAS                           "    AT  3,1 ATTRIBUTE  (REVERSE)

DISPLAY "Procesando Informacion..." AT 20,2

 call inicio()
 call datos_globales()

--
-- verificacion dis_cuenta05 
--

SELECT "ok"
FROM   sep_movimientos_invadido_v a
WHERE  a.nss       = g_invadido
AND    a.subcuenta in (4,8)
AND    a.tipo_movimiento = 3
AND    a.monto_en_acciones <> 0 
GROUP BY 1

IF STATUS <> NOTFOUND THEN

  update sep_movimientos_invadido_v 
  set    ind_mov_asociado = 1
  WHERE  nss = g_invadido
  AND    subcuenta in (4,8)
  AND    tipo_movimiento = 3
  AND    monto_en_acciones <> 0

  CALL inserta_descripcion("TIPO DE MOVIMIENTO = 3 INVALIDO",1)
END IF

--- fin verificacion viv dis_cuenta05 

---
--- verificacion de movimientos de retiro 
---

SELECT "OK"
FROM   sep_movimientos_invadido_v a
WHERE  a.nss = g_invadido
AND    a.subcuenta in (1,2,4,5,6,9)
AND    a.tipo_movimiento not in (3,100,101,102,103,104,105,106,107,
                                 108,109,110,111,112,999,210,215,
                                 950,960,990,961,991)
AND    a.monto_en_pesos < 0
GROUP BY 1

IF STATUS <> NOTFOUND THEN

update sep_movimientos_invadido_v
set    ind_mov_asociado = 2
WHERE  nss = g_invadido
AND    subcuenta in (1,2,4,5,6,9)
AND    tipo_movimiento not in (3,100,101,102,103,104,105,106,107,
                               108,109,110,111,112,999,210,215,
                               950,960,990,961,991)
AND    monto_en_pesos < 0

  CALL inserta_descripcion("DETECCION DE MOVIMIENTOS DE RETIRO",2)

END IF

--- fin de verificacion de movimientos de retiro 

---
--- verifica inconsistencias en dis_cuenta 
---

SELECT "OK"
FROM  sep_movimientos_invadido_v a
WHERE  a.nss = g_invadido 
AND   (a.fecha_valor IN (" ","") OR
       a.fecha_valor IS NULL)
GROUP BY 1
IF STATUS <> NOTFOUND THEN 

  update sep_movimientos_invadido_v 
  set    ind_mov_asociado = 3
  where  nss = g_invadido
  and    (fecha_valor IN (" ","") OR
          fecha_valor IS NULL)

  CALL inserta_descripcion("FECHA VALOR NULA",3)

END IF

--- fin verifica inconsistencias en dis_cuenta 


---
--- verifica dis_det_aporte 
---

DECLARE cur_incons2 CURSOR FOR

select a.folio, a.consec_reg_lote , COUNT(*)
from   dis_det_aporte a 
where  a.n_seguro = g_invadido
group by 1,2 having count(*) > 1

FOREACH cur_incons2 INTO dda_folio,dda_consec

   insert into sep_mov_tipo_inconsistencia
   values ("dis_det_aporte",
           dda_folio        ,
           "FOLIO DUPLICADO EN dis_det_aporte") 

END FOREACH

--- fin verifica dis_det_aporte 

---
--- verifica liquidaciones de tes_solicitud 
---

   select "ok"
   from   tes_solicitud a
   where  a.nss = g_invadido
   and    estado = 103
   group by 1

  IF STATUS <> NOTFOUND THEN

    DECLARE cur_incons3 CURSOR FOR 

     select a.folio,a.fecha_traspaso 
     from   tes_solicitud a
     where  a.nss    = g_invadido
     and    a.estado = 103
     group by 1,2

     FOREACH cur_incons3  INTO dda_folio,ts_fecha_trasp 

       CALL extrae_nombre_discuenta(ts_fecha_trasp)
       RETURNING g_nombre_dis_cuenta
 
                                   
       LET g_txt_dis_cuenta = " SELECT a.folio FROM ",g_nombre_dis_cuenta ," a",
                              " WHERE  a.folio = ? ",
                              " AND    a.nss   = ? ",
                              " AND    a.fecha_conversion   = ? ",
                              " GROUP BY 1 "

       PREPARE g_sql_cuenta FROM g_txt_dis_cuenta 
       
       EXECUTE g_sql_cuenta USING dda_folio,g_invadido,ts_fecha_trasp 
                            INTO g_folio_transf

       IF g_folio_transf IS NULL THEN LET g_folio_transf = 0 END IF
       IF g_folio_transf = 0 THEN 
           LET g_txt = "TES NO ENCONTRADO EN ",g_nombre_dis_cuenta,"FC ",
                        ts_fecha_trasp USING "dd-mm-yyyy"

          INSERT INTO sep_mov_tipo_inconsistencia VALUES 
          ("tes_solicitud" ,
           dda_folio        , 
           g_txt) 
       END IF
    END FOREACH
  END IF

--- fin verifica liquidaciones de tes_solicitud 

  CALL despliega_inconsistencia()

  CLOSE WINDOW SEPM020

END MAIN

FUNCTION datos_globales()
#trae los datos globales para el proceso
----------------------------------------

define ltxt char(300)

-- datos del invadido

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

declare cur_01 cursor for 
select a.nombre_tabla
from   taa_cd_tab_cuenta a 

foreach cur_01 into g_nombre_tabla

 let ltxt  = 
 " insert into sep_movimientos_invadido_v  ",
 " select '0',a.*,'0' ",
 " from ",g_nombre_tabla clipped ," a ",
 " where a.nss = ? " ,
 " and   a.fecha_conversion >= ? "

 prepare qry from ltxt 

 execute qry using g_invadido ,
                   g_fecha_reingreso 

end foreach

END FUNCTION

FUNCTION inserta_descripcion(l_descripcion,l_tipo)
# inserta dis_cuenta,folio y descripcion de inconsistencia
----------------------------------------------------------

define l_folio_incons integer   ,
       l_ano          smallint  ,
       l_tipo         smallint  ,
       l_descripcion  char(080) ,
       l_ano_c        char(004) ,
       l_dis_cuenta   char(012)

 declare cur_incons CURSOR FOR
 select a.folio                 ,
        YEAR(a.fecha_conversion)
 from   sep_movimientos_invadido_v a
 where  a.ind_mov_asociado = l_tipo
 group by 1,2

   FOREACH cur_incons INTO l_folio_incons, l_ano 

     let l_ano_c      = l_ano
     let l_dis_cuenta = "dis_cuenta",l_ano_c[3,4]

     select "ok"
     from taa_cd_tab_cuenta a
     where a.nombre_tabla = l_dis_cuenta

     IF STATUS = NOTFOUND THEN
         let l_dis_cuenta = "dis_cuenta"
     END IF

     insert into sep_mov_tipo_inconsistencia
     values (l_dis_cuenta     ,
             l_folio_incons   ,
             l_descripcion    )

   END FOREACH

END FUNCTION


FUNCTION despliega_inconsistencia()
# muestra la informacion de las inconsistencias encontradas
-----------------------------------------------------------

  DEFINE arr_d ARRAY[400] OF RECORD
         arc_1            smallint   ,
         tabla            char(015)  ,
         folio            integer    ,
         descripcion      char(101)
  END RECORD

  DEFINE l_total          integer   ,
         arcd_1           integer   ,
         contd_1          integer   ,
         totald_pa        integer   ,  
         lband             smallint  ,
         lmodificar       smallint

let lmodificar = 0

    OPEN WINDOW ventana_nss_30 AT 2,2 WITH FORM "SEPB0301" ATTRIBUTE(BORDER)

    DISPLAY " [Ctrl-I] Ver Detalle [Ctrl-V] Aceptar [ Ctrl-C ] Salir                        " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY " SEPB030             INCONSISTENCIAS DETECTADAS                                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY  "Fecha:",g_today USING "DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)

    DISPLAY  "NSS INVADIDO  :",g_invadido AT  5,1
    DISPLAY  "NSS ASOCIADO  :",g_asociado AT  6,1

  DECLARE cur_ver_inconsistencia CURSOR FOR
  SELECT " ",a.* 
  FROM   sep_mov_tipo_inconsistencia a
  ORDER BY 1  

LET contd_1 = 1

FOREACH cur_ver_inconsistencia INTO arr_d[contd_1].*

        LET arr_d[contd_1].arc_1 = contd_1
        LET contd_1              = contd_1 + 1

END FOREACH

           IF contd_1 = 1 THEN
              ERROR ""
              CLEAR FORM
              PROMPT " Sin Inconsistencia Detectada...<enter>... "
              ATTRIBUTE(REVERSE)
              FOR CHAR g_enter

              call pregunta("Apartar para provision [s/n] ?")
              returning lband   -- si es 1 se actualiza el edo de solicitud
           ELSE

             SELECT COUNT(*) 
             INTO   l_total
             FROM   sep_mov_tipo_inconsistencia a
                   
             LET totald_pa = contd_1 - 1

             DISPLAY BY NAME totald_pa
             DISPLAY BY NAME l_total

             CALL SET_COUNT(contd_1-1)

             LET arcd_1 = 0 

             DISPLAY ARRAY arr_d TO scr_1.* 

             ON KEY (CONTROL-I)

                LET arcd_1 = ARR_CURR()

                IF (arr_d[arcd_1].tabla = "dis_det_aporte" OR 
                    arr_d[arcd_1].tabla = "tes_solicitud" ) THEN 
                 ERROR "SIN MAS DETALLE DE INCONSISTENCIA"
 
                ELSE 
 
                  LET arcd_1 = ARR_CURR()
                  LET g_ejecuta = "fglgo SEPL030 ",arr_d[arcd_1].folio ," ",
                                                 g_invadido ," ",
                                                 g_asociado ," ",
                                                 g_diagnostico   ," ",
                                                 g_clasificacion

                  RUN g_ejecuta
                END IF 

             ON KEY (CONTROL-V)
                LET arcd_1 = ARR_CURR()


                select "ok" 
                from   sep_mov_tipo_inconsistencia a
                where  a.descripcion not matches '*RETIRO*' 
                group by 1 

                if status <> notfound then 

                   prompt "Inconsistencias no resueltas detectadas <enter>..."
                   for char g_enter

                else
                  prompt "Invadido con Disposicion de Recursos <enter>.."
                   for char g_enter
                  prompt "Es posible que aportes anteriores al ultimo <enter>"
                   for char g_enter
                  prompt "Retiro no sean considerados en la separacion <enter>"
                   for char g_enter

                   call pregunta("Apartar para Separacion Preliminar [s/n] ?")
                   returning lband -- si es 1 se actuaiza edo para separacion
                   exit display
                                   -- preliminar        
                end if

             ON KEY (INTERRUPT)
                LET lband = 0
                EXIT DISPLAY
             END DISPLAY

  END IF

  IF lband = 0 THEN 
     PROMPT "Consulta Finalizada...<enter> para salir "
     ATTRIBUTE (REVERSE)
     FOR CHAR g_enter
     CLOSE WINDOW ventana_nss_30
  ELSE

     update sep_det_reg_sol_reclamante
     set    estado = 53 -- verificada
     where  correlativo = g_idSolicitudSeparacion

     prompt "Pareja Apartada para Separacion Preliminar <enter>.."
     attribute (reverse)
     for char g_enter 
     close window ventana_nss_30

  END IF


END FUNCTION

FUNCTION extrae_nombre_discuenta(lfecha_conversion)

DEFINE lano               CHAR(004)
DEFINE lnomdis            CHAR(15)
DEFINE lfecha_conversion  DATE

LET lano    = YEAR(lfecha_conversion) 
LET lnomdis = "dis_cuenta" , lano[3,4]

SELECT "OK" 
FROM   taa_cd_tab_cuenta a
WHERE  a.nombre_tabla = lnomdis

IF STATUS = NOTFOUND THEN 
   LET lnomdis = "dis_cuenta"
END IF

RETURN lnomdis

END FUNCTION

FUNCTION inicio()

-- borrado inicial de tablas de trabajo

   delete from sep_mov_tipo_inconsistencia
   delete from sep_movimientos_invadido_v

   let g_today      = 0

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
