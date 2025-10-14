###############################################################################
#Proyecto          => Safre                                                   #
#Propietario       => E.F.P.                                                  #
#Modulo            => DIS.                                                    #
#Programa          => DISB094                                                 #
#Descripcion       => Genara Saldos de la cuentas individual(OFICIO consar272)#
#Fecha Inicio      => 30-agosto-2006                                          #
#Fecha Termino     =>                                                         #
#Autor             => JOSE ALEJANDRO RAMIREZ LARA                             #
###############################################################################

DATABASE safre_tmp

GLOBALS
 DEFINE
   gparam_dis    RECORD LIKE safre_af:seg_modulo.*,
   gusuario      CHAR(08),
   vcod_afore    LIKE safre_af:tab_afore_local.codigo_afore,
   v_tsol        SMALLINT,
   v_curp         CHAR(18),
   v_interes     DECIMAL(15,2),
   v_total_nss   INTEGER,
   vfecha_corte  ,
   vfec_fovisss  ,
   f_ini         ,
   f_fin         DATE,
   v_listado     CHAR(200),
   v_saldo_cero  ,
   v_total_saldos SMALLINT

DEFINE r_saldos RECORD
       nss       CHAR(11),
       subcuenta SMALLINT,
       pesos     DECIMAL(15,2),
       fecha     DATE
       END RECORD

 DEFINE nom_spl char(250)
 DEFINE ejecuta char(250)
 DEFINE hora_inicial  CHAR(08)
 DEFINE hora_final    CHAR(08)
 DEFINE HOY     DATE

END GLOBALS

MAIN
  DEFINE ltime CHAR(10)
  DEFINE vsal  CHAR(50)

  LET ltime = time
  CALL STARTLOG("DISB094.log")
  LET vfecha_corte = ARG_VAL(1)

  CALL Inicializa()

  --Inicia lanzando los querys inciales
  display "Ejecutando la agrupacion preliminar por subcuenta"

  CALL Ingresa_ctrl_proceso(2,"Agrupa los sdos")
  CALL querys_preliminares()
  CALL Actualiza_ctrl_proceso(2,"FINALIZA Agrupa sdos")

  CALL Ingresa_ctrl_proceso(3,"Genera archivo")
  CALL fn_arma_saldos()
  CALL Actualiza_ctrl_proceso(3,"FINALIZA Genera archivo")

  LET vsal="FINALIZA /safre_prc/dis/envio/EST_SDO.",vfecha_corte USING "YYYYMM"
  CALL Actualiza_ctrl_proceso(1,vsal)

  CALL limpia_nulos()

END MAIN

FUNCTION Inicializa()

   LET HOY = today
   LET v_total_nss = 0

   SELECT *,USER
   INTO   gparam_dis.*,gusuario
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = "dis"

   SELECT codigo_afore
   INTO vcod_afore
   FROM safre_af:tab_afore_local

   LET f_ini = MDY(MONTH(vfecha_corte),1,YEAR(vfecha_corte))
   LET f_fin = MDY(MONTH(vfecha_corte)+1,1,YEAR(vfecha_corte))-1

   LET vfec_fovisss = MDY(MONTH(vfecha_corte)+1,1,YEAR(vfecha_corte))

   LET v_listado = gparam_dis.ruta_envio CLIPPED,"/EST_SDO.",vfecha_corte USING "YYYYMM"

END FUNCTION

FUNCTION Ingresa_ctrl_proceso(vetapa,vproceso)

   DEFINE vetapa INTEGER
   DEFINE vproceso CHAR(15)

   --INSERTA REGISTRO EN dis_ctrl_proceso
   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO safre_af:dis_ctrl_proceso VALUES
      (HOY,                     -- fecha_proceso
       "DISB093",               -- proceso_cod
       vetapa,                  -- etapa_cod   -- LECTURA
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       vproceso,                -- parametro1
       vfecha_corte,            -- parametro2
       f_ini,                   -- parametro3
       f_fin,                   -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "PROCESANDO",            -- resultado
       gusuario,
       0)                       -- usuario
   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Lectura Archivo",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

END FUNCTION
FUNCTION Actualiza_ctrl_proceso(vetapa,vresultado)
   DEFINE vetapa     integer
   DEFINE vresultado char(50)

   LET hora_final = TIME

   LET ejecuta = " UPDATE safre_af:dis_ctrl_proceso ",
                 " SET hora_final    = '",hora_final,"',",
                     " resultado     = '",vresultado CLIPPED,"'",
                 " WHERE proceso_cod   = 'DISB093' ",
                 " AND etapa_cod       = ",vetapa CLIPPED,
                 " AND parametro2   = '",vfecha_corte CLIPPED,"'"

   PREPARE claexe10 FROM ejecuta
   EXECUTE claexe10

END FUNCTION

FUNCTION querys_preliminares()

WHENEVER ERROR CONTINUE
DROP TABLE tmp_saldos_prc

create table tmp_saldos_prc
  (
    nss char(11),
    subcta_prc char(2),
    pesos decimal(22,6),
    fecha date
  );

create index tmp_saldos_prc1 on tmp_saldos_prc
    (nss,subcta_prc);

display "Inicia llenado de tmp_saldos_prc"

INSERT INTO tmp_saldos_prc
SELECT c.n_seguro,
       t.subct_prc,
       SUM(s.monto_en_pesos ),
       "01/01/0001"
      FROM safre_tmp:cuota c,
          safre_tmp:tmp_saldo_corte s ,
           safre_af:tab_subcuenta t
     WHERE s.nss = c.n_seguro
       AND s.subcuenta <= 19
       AND s.subcuenta = t.subct_cod
       AND s.monto_en_pesos NOT BETWEEN -0.0099999 AND 0.009999
       GROUP BY 1,2

select nss,subcuenta,tipo_movimiento,monto_en_pesos,monto_en_acciones,
       fecha_conversion
from   safre_af:dis_cuenta
where  subcuenta in(3,10,11,12,15,16)
and    fecha_conversion between f_ini and f_fin
into temp tt_mov_vol

create index idx_nss_vol on tt_mov_vol(nss)

update statistics for table tt_mov_vol   ---alex

delete from tt_mov_vol
where  nss not in (select n_seguro from safre_tmp:cuota)

insert into tmp_saldos_prc
select nss, "31",monto_en_pesos,fecha_conversion
from  tt_mov_vol
where subcuenta in(10)
and   tipo_movimiento in(1,3)

insert into tmp_saldos_prc
select nss, "32",monto_en_pesos,fecha_conversion
from  tt_mov_vol
where subcuenta in(3)
and   tipo_movimiento in(1,3)

insert into tmp_saldos_prc
select nss, "33",sum(monto_en_pesos),fecha_conversion
from  tt_mov_vol
where subcuenta in(3,10)
and   tipo_movimiento in(10,490)
group by 1,4

insert into tmp_saldos_prc
select nss, "41",monto_en_pesos,fecha_conversion
from  tt_mov_vol
where subcuenta in(12)
and   tipo_movimiento in(1,3)

insert into tmp_saldos_prc
select nss, "42",monto_en_pesos,fecha_conversion
from  tt_mov_vol
where subcuenta in(11)
and   tipo_movimiento in(1,3)

insert into tmp_saldos_prc
select nss, "51",monto_en_pesos,fecha_conversion
from  tt_mov_vol
where subcuenta in(15,16)
and   tipo_movimiento in(1,3)

insert into tmp_saldos_prc
select nss, "52",monto_en_pesos,fecha_conversion
from  tt_mov_vol
where subcuenta in(15,16)
and   tipo_movimiento between 800 and 899

----- Agrega nss de cuota que no se encuentren en tmp_saldos_prc

select n_seguro,"01" sub_cta,0 pesos,"01/01/0001" fec
from   cuota
where  n_seguro not in (select nss from tmp_saldos_prc)
into temp tt_agregados

insert into tmp_saldos_prc
select * from tt_agregados

update statistics for table tmp_saldos_prc

display "Finaliza llenado de tmp_saldos_prc"


END FUNCTION
------------------------------------------------------------------------------

FUNCTION fn_arma_saldos()

PREPARE s_prioridad FROM "SET PDQPRIORITY HIGH"

EXECUTE s_prioridad

START REPORT rep_saldos TO v_listado

DECLARE c_saldos CURSOR FOR
SELECT s.nss        ,
       s.subcta_prc ,
       s.pesos      ,
       s.fecha
  FROM tmp_saldos_prc s
 ORDER BY 1,2

FOREACH c_saldos INTO r_saldos.*

   OUTPUT TO REPORT rep_saldos ( r_saldos.*)

END FOREACH

FINISH REPORT rep_saldos

END FUNCTION

REPORT rep_saldos ( r_saldos )

DEFINE r_saldos RECORD
       nss       CHAR(11),
       subcuenta SMALLINT,
       pesos     DECIMAL(15,2),
       fecha     DATE
       END RECORD

DEFINE rnss CHAR(11)

OUTPUT
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0
        PAGE LENGTH   3

ORDER EXTERNAL BY r_saldos.nss,
                  r_saldos.subcuenta


FORMAT

FIRST PAGE HEADER
      PRINT COLUMN  1,"01",
            COLUMN  3,"03",
            COLUMN  5,"70",
            COLUMN  7,"01",
            COLUMN  9,vcod_afore USING "&&&",
            COLUMN 12,"03",
            COLUMN 14,"001",
            COLUMN 17,today USING "YYYYMMDD",
            COLUMN 25,"001",
            COLUMN 28,58 SPACES

BEFORE GROUP OF r_saldos.nss
    SELECT COUNT(*)
      INTO v_total_saldos
      FROM tmp_saldos_prc
     WHERE nss = r_saldos.nss
   and pesos<>0   ---v2

   IF v_total_saldos < 1 then                  --v2
      LET v_total_saldos = v_total_saldos + 1   --v2
   END IF                                       --v2

    SELECT a.n_unico, a.tipo_solicitud
      INTO v_curp, v_tsol
      FROM safre_af:afi_mae_afiliado a
     WHERE a.n_seguro = r_saldos.nss

     IF v_tsol = 8 THEN
        LET rnss = ""
     ELSE
        LET rnss = r_saldos.nss
     END IF

      PRINT COLUMN 1, "02",
            COLUMN 3, rnss,
            COLUMN 14,v_curp,
            COLUMN 32,v_total_saldos USING "&&&",
            COLUMN 35,51 SPACES
ON EVERY ROW

      IF r_saldos.subcuenta = 16 THEN

         SELECT SUM(monto_en_pesos)
           INTO v_interes
           FROM safre_af:dis_cuenta
          WHERE nss = r_saldos.nss
            AND subcuenta = 14
            AND tipo_movimiento = 3
            AND fecha_valor = vfec_fovisss

         IF v_interes IS NULL THEN
            LET v_interes = 0
         END IF

         LET r_saldos.pesos = r_saldos.pesos + v_interes
      END IF

      IF r_saldos.subcuenta = 14 THEN

         SELECT SUM(monto_en_pesos)
           INTO v_interes
           FROM safre_af:dis_cuenta
          WHERE nss = r_saldos.nss
            AND subcuenta = 19
            AND tipo_movimiento = 3
            AND fecha_valor = vfec_fovisss

         IF v_interes IS NULL THEN
            LET v_interes = 0
         END IF

         LET r_saldos.pesos = r_saldos.pesos + v_interes
      END IF

      IF r_saldos.pesos is null THEN
         LET r_saldos.pesos=0
      END IF

      IF r_saldos.subcuenta > 20 THEN
         PRINT COLUMN  1, "03",
               COLUMN  3, rnss,
               COLUMN 14, v_curp,
               COLUMN 32, r_saldos.subcuenta USING "&&",
               COLUMN 34, r_saldos.pesos*100 USING "&&&&&&&&&&&&&&&",
               COLUMN 49, r_saldos.fecha USING "YYYYMMDD",
               COLUMN 57, 29 SPACES
     ELSE
         IF r_saldos.pesos > 0 THEN
            PRINT COLUMN  1, "03",
                  COLUMN  3, rnss,
                  COLUMN 14, v_curp,
                  COLUMN 32, r_saldos.subcuenta USING "&&",
                  COLUMN 34, r_saldos.pesos*100 USING "&&&&&&&&&&&&&&&",
                  COLUMN 49, r_saldos.fecha USING "YYYYMMDD",
                  COLUMN 57, 29 SPACES
         ELSE
            IF r_saldos.pesos = 0 THEN
               IF r_saldos.subcuenta = "01" THEN
                  PRINT COLUMN  1, "03",
                  COLUMN  3, rnss,
                  COLUMN 14, v_curp,
                  COLUMN 32, r_saldos.subcuenta USING "&&",
                  COLUMN 34, r_saldos.pesos USING "&&&&&&&&&&&&&&&",
                  COLUMN 49, r_saldos.fecha USING "YYYYMMDD",
                  COLUMN 57, 29 SPACES
               END IF
            ELSE
                PRINT COLUMN  1, "03",
                      COLUMN  3, rnss,
                      COLUMN 14, v_curp,
                      COLUMN 32, r_saldos.subcuenta USING "&&",
                      COLUMN 34, r_saldos.pesos*100 USING "-&&&&&&&&&&&&&&",
                      COLUMN 49, r_saldos.fecha USING "YYYYMMDD",
                      COLUMN 57, 29 SPACES
            END IF
         END IF
     END IF

AFTER GROUP OF r_saldos.nss
      LET v_total_nss = v_total_nss + 1

ON LAST ROW
      PRINT COLUMN 1,"09",
                     "03",
                     "70",
                     "01",
                     vcod_afore USING "&&&",
                     "03",
                     "001",
                     today USING "YYYYMMDD",
                     v_total_nss USING "&&&&&&&&&&&&&&&",
                     46 SPACES

END REPORT

FUNCTION limpia_nulos()
#ln--------------------

    DEFINE comm CHAR(500)

    LET v_listado = gparam_dis.ruta_envio CLIPPED,"/EST_SDO.",vfecha_corte USING "YYYYMM"

    LET comm = "cd ", gparam_dis.ruta_envio CLIPPED,
               "/ ; sed -e '/^$/d' ","EST_SDO.",vfecha_corte USING "YYYYMM" CLIPPED,
               " > sdo_op70"
    RUN comm

    LET comm = "cd " ,gparam_dis.ruta_envio CLIPPED,
               "/ ; mv sdo_op70  ","EST_SDO.",vfecha_corte USING "YYYYMM"
    RUN comm

END FUNCTION

