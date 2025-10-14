DATABASE safre_af
GLOBALS 
DEFINE l_acc decimal(16,6)
DEFINE reg_modulo RECORD LIKE seg_modulo.*
define ruta char(1000)

DEFINE reg_1 ARRAY[8,2] OF RECORD 
tot_cuentas integer ,
rcv         decimal(16,6),
sar92       decimal(16,6),
issste      decimal(16,6),
av          decimal(16,6),
acr         decimal(16,6)
END RECORD

DEFINE v_ind_edad SMALLINT

DEFINE fecha_ini date ,
       fecha_fin date


DEFINE v_existe  smallint ,
       v_criterio smallint

DEFINE reg_3  RECORD
       vfolio integer,
       vnss char(011),
       vfecha_traspaso date,
       vind_edad smallint
END RECORD

DEFINE i smallint
DEFINE j smallint
DEFINE txt char(1000)
END GLOBALS

MAIN


SELECT * 
INTO reg_modulo.*
from seg_modulo
where modulo_cod = 'est'


LET fecha_ini = ARG_VAL(1)
LET fecha_fin = ARG_VAL(2)

for i = 1 to 8 
  for j = 1 to 2
  LET reg_1[i,j].tot_cuentas = 0
  LET reg_1[i,j].rcv = 0
  LET reg_1[i,j].sar92 = 0
  LET reg_1[i,j].issste = 0
  LET reg_1[i,j].acr = 0
end for
end for

CREATE TEMP TABLE oficio_tes(folio integer , nss char(011),
fecha_traspaso date ,edad smallint)

LET txt = 'EXECUTE PROCEDURE fn_edad_sin_act(?,?)'
PREPARE qry_edad FROM txt
DECLARE cur_1 CURSOR FOR qry_edad

IF fecha_fin < fecha_ini THEN
   DISPLAY "FECHA FIN NO PUEDE SER MENOR A FECHA INI..."
   DISPLAY "ABORTANDO PROCESO..."
   EXIT PROGRAM
END IF

IF fecha_fin IS NULL THEN
   DISPLAY "FECHA FIN NO PUEDE SER NULA..."
   DISPLAY "ABORTANDO PROCESO..."
   EXIT PROGRAM
END IF

DECLARE cur_2 CURSOR FOR 

SELECT unique a.folio,a.nss,a.fecha_traspaso
from tes_solicitud a
where a.fecha_traspaso between fecha_ini and fecha_fin
and tipo_traspaso in (0,1)
and estado = 103


FOREACH  cur_2 INTO  reg_3.vfolio,reg_3.vnss,reg_3.vfecha_traspaso
  FOREACH cur_1 USING reg_3.vnss,reg_3.vfecha_traspaso
                   INTO  v_existe,reg_3.vind_edad,v_criterio
           insert into oficio_tes values(reg_3.*)
   end foreach
end foreach
  

select count(unique b.nss),sum(monto_en_acciones) 
into reg_1[2,1].tot_cuentas,
     reg_1[2,1].rcv
from oficio_tes a,
     dis_cuenta b
where  a.folio = b.folio 
and    a.nss   = b.nss
and    b.subcuenta in (1,2,5,6,9)
and b.tipo_movimiento = 1
and    a.edad = 0

select sum(a.monto_en_acciones)
into reg_1[2,1].sar92
from dis_cuenta  a ,
     oficio_tes  b
where  a.folio = b.folio 
and    a.nss   = b.nss
and    a.subcuenta  = 7
and a.tipo_movimiento = 1
and    b.edad = 0

select sum(a.monto_en_acciones)
into reg_1[2,1].issste
from dis_cuenta  a ,
     oficio_tes  b
where  a.folio = b.folio 
and    a.nss   = b.nss
and    a.subcuenta  = 13
and a.tipo_movimiento = 1
and    b.edad = 0

select count(unique b.nss),sum(monto_en_acciones) 
into reg_1[2,2].tot_cuentas,
     reg_1[2,2].rcv
from oficio_tes a,
     dis_cuenta b
where  a.folio = b.folio 
and    a.nss   = b.nss
and    b.subcuenta in (1,2,5,6,9)
and b.tipo_movimiento = 1
and    a.edad = 1

select sum(a.monto_en_acciones)
into reg_1[2,2].sar92
from dis_cuenta  a ,
     oficio_tes  b
where  a.folio = b.folio 
and    a.nss   = b.nss
and    a.subcuenta  = 7
and a.tipo_movimiento = 1
and    b.edad = 1

select sum(a.monto_en_acciones)
into reg_1[2,2].issste
from dis_cuenta  a ,
     oficio_tes  b
where  a.folio = b.folio 
and    a.nss   = b.nss
and    a.subcuenta  = 13
and a.tipo_movimiento = 1
and    b.edad = 1

select count(unique b.nss),sum(monto_en_acciones) 
into reg_1[4,1].tot_cuentas,
     reg_1[4,1].acr
from oficio_tes a,
     dis_cuenta b
where  a.folio = b.folio 
and    a.nss   = b.nss
and    b.subcuenta in (11,12)
and    b.tipo_movimiento = 1
and    a.edad = 0

select count(unique b.nss),sum(monto_en_acciones) 
into reg_1[4,2].tot_cuentas,
     reg_1[4,2].acr
from oficio_tes a,
     dis_cuenta b
where  a.folio = b.folio 
and    a.nss   = b.nss
and    b.subcuenta in (11,12)
and   b.tipo_movimiento = 1
and    a.edad = 1

let ruta = reg_modulo.ruta_listados CLIPPED ,"/anexob_",fecha_ini USING"YYYYMMDD","-",
                      fecha_fin USING"YYYYMMDD"
for i = 1 to 8
  for j = 1 to 2
  if reg_1[i,j].tot_cuentas IS  NULL  THEN
       let reg_1[i,j].tot_cuentas = 0
  end if
  if reg_1[i,j].rcv IS  NULL  THEN
      let reg_1[i,j].rcv = 0
  end if
  if reg_1[i,j].sar92 IS  NULL  THEN
      let reg_1[i,j].sar92 = 0
  end if
  if reg_1[i,j].issste IS  NULL  THEN
      let reg_1[i,j].issste = 0
  end if
  if reg_1[i,j].av IS  NULL  THEN
      let reg_1[i,j].av = 0
  end if
  if reg_1[i,j].acr IS  NULL  THEN
      let reg_1[i,j].acr = 0
  end if
end for
end for
 START REPORT  rpt_1 TO ruta

OUTPUT TO REPORT rpt_1()

FINISH REPORT rpt_1

UPDATE safre_af:taa_ctr_anexob
SET    estado = "FIN"

END MAIN


REPORT rpt_1()

 DEFINE campo      RECORD
           afore_cod  CHAR(03),
           raz_social CHAR(50)
                      END  RECORD
DEFINE pre_acc_0              DECIMAL(16,6)
    DEFINE pre_acc_1              DECIMAL(16,6)
    DEFINE pre_acc_2              DECIMAL(16,6)
    DEFINE pre_acc_11             DECIMAL(16,6)

 OUTPUT
        TOP MARGIN 1
        BOTTOM MARGIN 0
        LEFT MARGIN   0
        RIGHT MARGIN  0
        PAGE LENGTH   60

   FORMAT
        PAGE HEADER
           PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
           PRINT COLUMN  02,"ANEXO B",
                 COLUMN 165,"Pagina: ",PAGENO USING "<<<<"
           PRINT COLUMN 02,"__________________________________________________",
                           "__________________________________________________",
                           "__________________________________________________",
                           "______________________________"

        SELECT codigo_afore,razon_social
        INTO campo.afore_cod,campo.raz_social
        FROM tab_afore_local

           SELECT precio_del_dia
              INTO pre_acc_0
           FROM glo_valor_accion
           WHERE fecha_valuacion = today
             AND codigo_siefore = 0

           IF (pre_acc_0 IS NULL) THEN
              LET pre_acc_0  =  0
           END IF

           SELECT precio_del_dia
              INTO pre_acc_1
           FROM glo_valor_accion
           WHERE fecha_valuacion = today
             AND codigo_siefore = 1

           IF (pre_acc_1 IS NULL) THEN
              LET pre_acc_1  =  0
           END IF

           SELECT precio_del_dia
              INTO pre_acc_2
           FROM glo_valor_accion
           WHERE fecha_valuacion = today
             AND codigo_siefore = 2

           IF (pre_acc_2 IS NULL) THEN
              LET pre_acc_2  =  0
           END IF

           SELECT precio_del_dia
              INTO pre_acc_11
           FROM glo_valor_accion
           WHERE fecha_valuacion = today
             AND codigo_siefore = 11

          IF (pre_acc_11 IS NULL) THEN
             LET pre_acc_11  =  0
          END IF

        SKIP 1 LINE
        PRINT COLUMN 02,campo.afore_cod,"         ",campo.raz_social,
              COLUMN 146,"VALOR ACCION B1:  ",pre_acc_1 USING "###.######"

        PRINT COLUMN 02,"PERIODO :",fecha_ini USING"YYYYMMDD","-",fecha_fin
                          USING"YYYYMMDD",
              COLUMN 146,"VALOR ACCION B2:  ",pre_acc_2 USING "###.######"
        PRINT COLUMN 02,"FECHA :","     ",TODAY USING"DD-MM-YYYY",
              COLUMN 156,"PAVIS:  ",pre_acc_11 USING "###.######"
        PRINT COLUMN 02,"__________________________________________________",
                        "__________________________________________________",
                        "__________________________________________________",
                        "______________________________"
        SKIP 1 LINE
            PRINT "    TRANSFERENCIA   ",
                  "        EDAD        ",
                  "CUENTAS",
                  "            RCV",
                  "           SAR92",
                  "       SARISSSTE",
                  "              AV",
                  "             ACR"

        PRINT COLUMN 02,"__________________________________________________",
                        "__________________________________________________",
                        "__________________________________________________",
                        "______________________________"
        SKIP 1 LINE

        ON EVERY ROW
            PRINT "RCV,SAR92,SAR ISSSTE"
            PRINT "    sb1 a sb2       ",
                  "      < 56          ",
                  reg_1[1,1].tot_cuentas using "#####&",
                  reg_1[1,1].rcv USING"########&.&&&&&&",
                  reg_1[1,1].sar92 USING"########&.&&&&&&",
                  reg_1[1,1].issste USING"########&.&&&&&&"
        SKIP 1 LINE

            PRINT "RCV,SAR92,SAR ISSSTE"
            PRINT "    sb2 a sb1       ",
                  "      < 56          ",
                  reg_1[2,1].tot_cuentas using "#####&",
                  reg_1[2,1].rcv USING"########&.&&&&&&",
                  reg_1[2,1].sar92 USING"########&.&&&&&&",
                  reg_1[2,1].issste USING"########&.&&&&&&"
        SKIP 1 LINE

            PRINT "RCV,SAR92,SAR ISSSTE"
            PRINT "    sb2 a sb1       ",
                  "      >= 56         ",
                  reg_1[2,2].tot_cuentas using "#####&",
                  reg_1[2,2].rcv USING"########&.&&&&&&",
                  reg_1[2,2].sar92 USING"########&.&&&&&&",
                  reg_1[2,2].issste USING"########&.&&&&&&"

        SKIP 1 LINE
            PRINT "         AV         "
            PRINT "    sb1 a siav      ",
                  "       < 56         ",
                  reg_1[3,1].tot_cuentas using "#####&",
                  "                " ,
                  "                " ,
                  "                " ,
                  reg_1[3,1].av USING"########&.&&&&&&"
        SKIP 1 LINE
            PRINT "         AV         "
            PRINT "    sb1 a siav      ",
                  "       >=56         ",
                  reg_1[3,2].tot_cuentas using "#####&",
                  "                " ,
                  "                " ,
                  "                " ,
                  reg_1[3,2].av USING"########&.&&&&&&"
        SKIP 1 LINE
            PRINT "         AV         "
            PRINT "    siav a sb1      ",
                  "       < 56         ",
                  reg_1[4,1].tot_cuentas using "#####&",
                  "                " ,
                  "                " ,
                  "                " ,
                  reg_1[4,1].av USING"########&.&&&&&&"
        SKIP 1 LINE
            PRINT "         AV         "
            PRINT "    siav a sb1      ",
                  "       >=56         ",
                  reg_1[4,2].tot_cuentas using "#####&",
                  "                " ,
                  "                " ,
                  "                " ,
                  reg_1[4,2].av USING"########&.&&&&&&"
        SKIP 1 LINE
            PRINT "         AV         "
            PRINT "flujo futuro a siav ",
                  "       < 56         ",
                  reg_1[5,1].tot_cuentas using "#####&",
                  "                " ,
                  "                " ,
                  "                " ,
                  reg_1[5,1].av USING"########&.&&&&&&"
        SKIP 1 LINE
            PRINT "         AV         "
            PRINT "flujo futuro a siav ",
                  "       >=56         ",
                  reg_1[5,2].tot_cuentas using "#####&",
                  "                " ,
                  "                " ,
                  "                " ,
                  reg_1[5,2].av USING"########&.&&&&&&"
        SKIP 1 LINE
            PRINT "         AV         "
            PRINT "flujo futuro a sb1  ",
                  "       < 56         ",
                  reg_1[6,1].tot_cuentas using "#####&",
                  "                " ,
                  "                " ,
                  "                " ,
                  reg_1[6,1].av USING"########&.&&&&&&"
        SKIP 1 LINE
            PRINT "         AV         "
            PRINT "flujo futuro a sb1  ",
                  "       >=56         ",
                  reg_1[6,2].tot_cuentas using "#####&",
                  "                " ,
                  "                " ,
                  "                " ,
                  reg_1[6,2].av USING"########&.&&&&&&"
        SKIP 1 LINE
            PRINT "         AC         "
            PRINT "      sb1 a sb2     ",
                  "       < 56         ",
                  reg_1[7,1].tot_cuentas using "#####&",
                  "                " ,
                  "                " ,
                  "                " ,
                  "                " ,
                  reg_1[7,1].acr USING"########&.&&&&&&"
        SKIP 1 LINE

            PRINT "        AC          "
            PRINT "    sb2 a sb1       ",
                  "      <  56         ",
                  reg_1[8,1].tot_cuentas using "#####&",
                  "                " ,
                  "                " ,
                  "                " ,
                  "                " ,
                  reg_1[8,1].acr USING"########&.&&&&&&"

        SKIP 1 LINE
            PRINT "        AC          "
            PRINT "     sb2 a sb1      ",
                  "      >= 56         ",
                  reg_1[8,2].tot_cuentas using "#####&",
                  "                " ,
                  "                " ,
                  "                " ,
                  "                " ,
                  reg_1[8,2].acr USING"########&.&&&&&&"
        SKIP 1 LINE

           PRINT COLUMN 02,"__________________________________________________",
                           "__________________________________________________",
                           "__________________________________________________",
                           "______________________________"
END REPORT
