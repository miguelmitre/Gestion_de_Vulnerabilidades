###########################################################################
#Proyecto          => AFORE ( MEXICO )
#Propietario       => E.F.P.
#Programa TESL001  => GENERA REPORTE POR VARIOS FOLIOS 
#                     PARA EL MODULO DE TES  DE CUENTAS(ES DECIR UN 
#                     REPORTE GLOBAL DE LA TABLA => safre_af:tes_solicitud
#                     donde la fecha traspaso = today "Y" estado = 103
#                     liquidado).
#Autor             => MARCO ANTONIO GONZALEZ ROJAS
#Ultima Mod        => 30 de Abril del 2012  Se incorporo SIEFORE 10
#Fecha             => 16 MAY 2005
#Sistema           => TES
###########################################################################
DATABASE safre_af
GLOBALS 

DEFINE ghoy        DATE
DEFINE g_enter     CHAR(01)
DEFINE g_sql_01    ,
       g_sql_02    ,
       g_sql_03    CHAR(2000),
       g_tabname   CHAR(50)

###### variables globales para reporte ######
  #====                       =====
  DEFINE  g         RECORD
          fols      CHAR(10)
                    END RECORD

  DEFINE g_reg      RECORD
         folios  CHAR(300)
                   END RECORD

  DEFINE g_folios  CHAR(10)
  DEFINE gtip_tra  SMALLINT
  DEFINE gdesc     CHAR(50)
#=========
  DEFINE  i          SMALLINT
  DEFINE  r_arr      ARRAY[20]    OF RECORD
                     folio      CHAR(10),
                     desc       CHAR(50)
                                  END RECORD
#=========
  DEFINE g_nombre_programa    CHAR(15)

  DEFINE g_tipo_desc1          ,
         g_tipo_desc2          CHAR(45),
         g_usuario             CHAR(008)

  DEFINE g_fecha_accion       ,
         g_fecha_parti        DATE

  DEFINE g_total_cuentas      INTEGER

  DEFINE g_folio              INTEGER

  DEFINE g_seg_modulo           RECORD LIKE seg_modulo.*

  DEFINE hoy                  DATE

  DEFINE g_tip_rep            CHAR(04)

  DEFINE tmv  RECORD
         tipo_siefore      CHAR(11),
         tip_mov           LIKE safre_af:dis_cuenta.tipo_movimiento,
         subcuenta         LIKE safre_af:dis_cuenta.subcuenta,
         fecha_conversion  LIKE safre_af:dis_cuenta.fecha_conversion,
         acciones          DECIMAL(16,6),
         pesos             DECIMAL(16,6)
  END RECORD

  DEFINE sub  RECORD
         tipo_siefore          CHAR(11),
         subcuenta             SMALLINT,
         acciones              DECIMAL(16,6),
         pesos                 DECIMAL(16,6)
  END RECORD

############################################
END GLOBALS

MAIN

 CALL asigna_globales()

 CALL fol_procesados()
 CALL genera_reporte()
 
END MAIN

FUNCTION define_querys()
#dq--------------

LET g_sql_01 =
' SELECT CASE WHEN b.siefore = 0 THEN (SELECT x.razon_social     ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 0)  ',
          ' WHEN b.siefore = 1 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 1)  ',
          ' WHEN b.siefore = 2 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 2)  ',
          ' WHEN b.siefore = 3 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 3)  ',
          ' WHEN b.siefore = 4 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 4)  ',
          ' WHEN b.siefore = 5 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 5)  ',
          ' WHEN b.siefore = 6 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 6)  ',
          ' WHEN b.siefore = 11 THEN (SELECT x.razon_social      ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 11) ',
   ' END CASE ,',
   '       b.tipo_movimiento,b.subcuenta,b.fecha_conversion, ',
   '       SUM(b.monto_en_acciones),SUM(b.monto_en_pesos) ',
   '  FROM  ',g_tabname ,
   ' WHERE  b.folio IN ( SELECT a.folio FROM safre_af:tes_solicitud a ',
   '                     WHERE a.fecha_traspaso = TODAY ',
   '                       AND a.estado = 103) ',
   ' GROUP  BY  1,2,3,4 ',
   ' ORDER  BY  2,3,4 '

LET g_sql_02 = 
' SELECT CASE WHEN b.siefore = 0 THEN (SELECT x.razon_social     ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 0)  ',
          ' WHEN b.siefore = 1 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 1)  ',
          ' WHEN b.siefore = 2 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 2)  ',
          ' WHEN b.siefore = 3 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 3)  ',
          ' WHEN b.siefore = 4 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 4)  ',
          ' WHEN b.siefore = 5 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 5)  ',
          ' WHEN b.siefore = 6 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 6)  ',
          ' WHEN b.siefore = 11 THEN (SELECT x.razon_social      ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 11) ',
   ' END CASE, ',
   '       b.subcuenta,SUM(b.monto_en_acciones),SUM(b.monto_en_pesos) ',
   '  FROM  ',g_tabname ,
   ' WHERE  b.folio IN ( SELECT a.folio FROM safre_af:tes_solicitud a',
   '                     WHERE a.fecha_traspaso = TODAY ',
   '                       AND a.estado = 103) ',
   ' GROUP  BY  1,2 ',
   ' ORDER  BY  1,2 '

LET g_sql_03 = 
   'SELECT b.tipo_movimiento, ',
   '       SUM(b.monto_en_acciones),SUM(b.monto_en_pesos) ',
   '  FROM  ', g_tabname,
   ' WHERE  b.folio IN ( SELECT a.folio FROM safre_af:tes_solicitud a',
   '                     WHERE a.fecha_traspaso = TODAY ',
   '                       AND a.estado = 103) ',
   '   AND  b.siefore = ? ',
   ' GROUP  BY  1 ',
   ' ORDER  BY  1 '
END FUNCTION

FUNCTION asigna_globales()
#ag-----------------------

  LET     ghoy              =    TODAY                

  SELECT  user
  INTO  g_usuario
  FROM  tab_afore_local

  SELECT  *
  INTO  g_seg_modulo.*
  FROM  seg_modulo
  WHERE  modulo_cod = 'taa'
  

  LET     g_tabname         =    'dis_cuenta b '

  SELECT COUNT(unique nss)
  INTO   g_total_cuentas
  FROM   safre_af:tes_solicitud b
  WHERE  b.fecha_traspaso = TODAY
  AND  b.estado = 103

  SELECT UNIQUE e.fecha_liquidacion 
  INTO g_fecha_accion
  FROM safre_af:tes_ctr_folio e
  WHERE  e.folio IN ( SELECT a.folio FROM safre_af:tes_solicitud a
                      WHERE a.fecha_traspaso = TODAY 
                        AND a.estado = 103) 
 
  LET    g_fecha_parti      =     g_fecha_accion

  LET    g_nombre_programa  =     "TESL001"

  LET    g_tip_rep          =     'TTG' 


END FUNCTION

FUNCTION genera_reporte()
#gr---------------------

DEFINE l_hora  CHAR(005)
DEFINE l_enter CHAR(005)

DEFINE l_impre_tm     ,
   l_impre_sub    ,
   l_lista        CHAR(300)


  LET l_hora = TIME

  LET l_impre_tm = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
   ".LIS_",g_tip_rep CLIPPED,"_TIPMOV.",ghoy USING "DDMMYY", "_",l_hora[1,2] CLIPPED,l_hora[4,5] CLIPPED

  LET l_impre_sub = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
   ".LIS_",g_tip_rep CLIPPED,"_SUBCTA.",ghoy USING "DDMMYY", "_",l_hora[1,2] CLIPPED,l_hora[4,5] CLIPPED

  LET l_impre_tm  = l_impre_tm  CLIPPED
  LET l_impre_sub = l_impre_sub CLIPPED

  START REPORT rpt_tipmov TO l_impre_tm
  START REPORT rpt_subcta TO l_impre_sub       

  CALL cuentas_impri()
 
  LET l_lista = "chmod 777 ",l_impre_tm CLIPPED
  RUN l_lista

  LET l_lista = "chmod 777 ",l_impre_sub CLIPPED
  RUN l_lista

--LET l_lista = "lp ",l_impre_tm
--RUN l_lista

--LET l_lista = "lp ",l_impre_sub
--RUN l_lista

END FUNCTION

FUNCTION cuentas_impri()
#ci--------------------

 CALL define_querys()   
 PREPARE sql_01 FROM g_sql_01
 DECLARE cur_sql_01  CURSOR  FOR sql_01

 FOREACH cur_sql_01  INTO  tmv.*
    IF tmv.fecha_conversion IS NULL OR tmv.fecha_conversion="12/31/1899" THEN
       LET tmv.fecha_conversion = NULL
    END IF
    OUTPUT TO REPORT rpt_tipmov(tmv.*)
 END FOREACH

 PREPARE sql_02 FROM g_sql_02
 DECLARE cur_sql_02 CURSOR  FOR sql_02

 FOREACH cur_sql_02 INTO  sub.*
    OUTPUT TO REPORT rpt_subcta(sub.*)
 END FOREACH

 FINISH REPORT rpt_tipmov       

 FINISH REPORT rpt_subcta      
 DISPLAY " "
 DISPLAY  "TESL001:REPORTE GLOBAL DE TRANSFERENCIA GENERADO..."
 DISPLAY " "

END FUNCTION

REPORT rpt_tipmov(r_tmv)
#rtmov------------------

  DEFINE r_tmv  RECORD
         tipo_siefore      CHAR(11),
         tip_mov           LIKE safre_af:dis_cuenta.tipo_movimiento,
         subcuenta         LIKE safre_af:dis_cuenta.subcuenta,
         fecha_conversion  LIKE safre_af:dis_cuenta.fecha_conversion,
         acciones          DECIMAL(16,6),
         pesos             DECIMAL(16,6)
  END RECORD

  DEFINE campo      RECORD
         afore_cod  CHAR(03),
         raz_social CHAR(50)
                    END  RECORD
  DEFINE fech_conv              DATE
  DEFINE tot_siefores           SMALLINT
  DEFINE precios                ARRAY[11] OF RECORD 
         razon_social           CHAR(15),
         precio                 DECIMAL(16,6)
  END RECORD
  DEFINE sie_rep                SMALLINT 
  DEFINE tottip_mov             CHAR(03)
  DEFINE tip_mov_acc            DECIMAL(16,6)
  DEFINE tip_mov_pes            DECIMAL(16,6)
  DEFINE tip_tra                SMALLINT

     OUTPUT
        TOP MARGIN    0
        BOTTOM MARGIN 0
        LEFT MARGIN   0
        RIGHT MARGIN  0
        PAGE LENGTH   90

     ORDER BY r_tmv.tipo_siefore,r_tmv.tip_mov,r_tmv.subcuenta  

  FORMAT
     PAGE HEADER
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

       PRINT COLUMN 165,"Pagina: ",PAGENO USING "<<<<"
       PRINT COLUMN 02,"__________________________________________________",
                       "__________________________________________________",
                       "__________________________________________________",
                       "______________________________"
   
       SELECT codigo_afore,razon_social
       INTO campo.afore_cod,campo.raz_social       
       FROM tab_afore_local


       SELECT COUNT(*) 
       INTO   tot_siefores
       FROM   tab_siefore_local a
       WHERE  a.codigo_siefore not in (0,11,12,13,10)

       FOR i = 1 TO tot_siefores

        SELECT a.razon_social  ,
               b.precio_del_dia
          INTO precios[i].razon_social ,
               precios[i].precio
          FROM tab_siefore_local a     ,
               glo_valor_accion  b
         WHERE a.codigo_siefore  = b.codigo_siefore
           AND b.fecha_valuacion = today
           AND a.codigo_siefore  = i
       END FOR

    SKIP 1 LINE

    PRINT COLUMN  02,"REPORTE       : ","     ",g_nombre_programa CLIPPED,"  POR TIPO MOVIMIENTO",
          COLUMN 140,precios[1].razon_social,  COLUMN 155,precios[1].precio USING "###.######" 
    PRINT COLUMN 02,"ADMINISTRADORA: ","     ",campo.afore_cod USING "###","   ",campo.raz_social,
          COLUMN 140,precios[2].razon_social,  COLUMN 155,precios[2].precio USING "###.######" 
    PRINT COLUMN 02,"FECHA         :","      ",g_fecha_accion USING"DD-MM-YYYY",
          COLUMN 140,precios[3].razon_social,  COLUMN 155,precios[3].precio USING "###.######" 
    PRINT COLUMN 140,precios[4].razon_social,  COLUMN 155,precios[4].precio USING "###.######" 
    PRINT COLUMN 140,precios[5].razon_social,  COLUMN 155,precios[5].precio USING "###.######" 
    PRINT COLUMN 140,precios[6].razon_social,  COLUMN 155,precios[6].precio USING "###.######" 
     
    PRINT COLUMN 02,"FOLIO         :","      ",r_arr[1].folio," ",r_arr[1].desc,
          COLUMN 90,"FOLIO         :","      ",r_arr[6].folio," ",r_arr[6].desc
    PRINT COLUMN 02,"FOLIO         :","      ",r_arr[2].folio," ",r_arr[2].desc,
          COLUMN 90,"FOLIO         :","      ",r_arr[7].folio," ",r_arr[7].desc
    PRINT COLUMN 02,"FOLIO         :","      ",r_arr[3].folio," ",r_arr[3].desc,
          COLUMN 90,"FOLIO         :","      ",r_arr[8].folio," ",r_arr[8].desc
    PRINT COLUMN 02,"FOLIO         :","      ",r_arr[4].folio," ",r_arr[4].desc,
          COLUMN 90,"FOLIO         :","      ",r_arr[9].folio," ",r_arr[9].desc
    PRINT COLUMN 02,"FOLIO         :","      ",r_arr[5].folio," ",r_arr[5].desc,
          COLUMN 90,"FOLIO         :","      ",r_arr[10].folio," ",r_arr[10].desc
    PRINT COLUMN 02,"__________________________________________________",
                    "__________________________________________________",
                    "__________________________________________________",
                    "______________________________"

    SKIP 1 LINE
    PRINT COLUMN 02,"SIEFORE",
          COLUMN 33,"TIPO_MOV",
          COLUMN 55,"SUBCUENTA",
          COLUMN 89,"ACCIONES/PAVIS",
          COLUMN 130,"MONTO EN PESOS"

    PRINT COLUMN 02,"--------------------------------------------------",
                    "--------------------------------------------------",
                    "--------------------------------------------------",
                    "------------------------------"

 ON EVERY ROW
     PRINT COLUMN  2,r_tmv.tipo_siefore ,
           COLUMN 33,r_tmv.tip_mov,
           COLUMN 58,r_tmv.subcuenta          USING "###" ,
           COLUMN 84,r_tmv.acciones         USING "---,---,--&.######",
           COLUMN 125,r_tmv.pesos           USING "---,---,--&.######"

 
    AFTER GROUP  OF r_tmv.tipo_siefore

       SELECT a.codigo_siefore 
       INTO sie_rep
       FROM  tab_siefore_local a
       WHERE a.razon_social = r_tmv.tipo_siefore

    PRINT COLUMN 02,"--------------------------------------------------",
                    "--------------------------------------------------",
                    "--------------------------------------------------",
                    "------------------------------"
    SKIP 1 LINE

    PRINT COLUMN  2,"TOTAL POR TIPO DE MOVIMIENTO ==>",r_tmv.tipo_siefore CLIPPED

    PREPARE sql_03  FROM g_sql_03
    DECLARE cur_sql_03 CURSOR FOR sql_03

    FOREACH cur_sql_03 USING sie_rep
    INTO tottip_mov,tip_mov_acc,tip_mov_pes

     PRINT COLUMN 36,tottip_mov,
           COLUMN 85,tip_mov_acc USING "---,---,--&.######",
           COLUMN 125,tip_mov_pes USING "---,---,--&.######"

    END FOREACH

    SKIP 1 LINES

    PRINT COLUMN 02,"--------------------------------------------------",
                    "--------------------------------------------------",
                    "--------------------------------------------------",
                    "------------------------------"

    ON LAST ROW
     
     SKIP 1 LINE

     PRINT COLUMN  2,"TOTAL CUENTAS","==>",                                                          g_total_cuentas USING "######&"," ","REGS."


END REPORT
###############################################################################
REPORT rpt_subcta(s)
---bueno
  
DEFINE tot_siefores          SMALLINT
DEFINE s  RECORD
       tipo_siefore          CHAR(11),
       subcuenta             SMALLINT,
       acciones              DECIMAL(16,6),
       pesos                 DECIMAL(16,6)
          END RECORD
DEFINE precio                 ARRAY[11] OF RECORD 
       razon_social           CHAR(15),
       precio                 DECIMAL(16,6)
END RECORD
DEFINE fech_conv              DATE
DEFINE  campo         RECORD
        afore_cod             CHAR(03),
        raz_social            CHAR(50)
                      END RECORD
DEFINE tip_tra                SMALLINT

   OUTPUT
      TOP MARGIN     0
      BOTTOM MARGIN  0
      LEFT MARGIN    0
      RIGHT MARGIN   0
      PAGE LENGTH   90

  ORDER BY s.tipo_siefore,s.subcuenta

   FORMAT
      PAGE HEADER
         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

         PRINT COLUMN 165,"Pagina:  ",PAGENO USING "<<<<"
         PRINT COLUMN 02,"__________________________________________________",
                         "__________________________________________________",
                         "__________________________________________________",
                         "______________________________"

         SELECT codigo_afore,razon_social
            INTO campo.afore_cod,campo.raz_social
         FROM tab_afore_local

         SELECT COUNT(*) 
         INTO   tot_siefores
         FROM   tab_siefore_local 
         WHERE  codigo_siefore not in (0,11,12,13,10)
  
         FOR i = 1 TO tot_siefores

          SELECT a.razon_social,
                 b.precio_del_dia
            INTO precio[i].razon_social ,
                 precio[i].precio
            FROM tab_siefore_local a     ,
                 glo_valor_accion  b
           WHERE a.codigo_siefore  = b.codigo_siefore
             AND b.fecha_valuacion = today
             AND a.codigo_siefore = i
        END FOR

        SKIP 1 LINE
         
       PRINT COLUMN  02,"REPORTE       : ","     ",g_nombre_programa CLIPPED,"  POR SUBCUENTA",
             COLUMN 140,precio[1].razon_social,precio[1].precio USING "###.######" 
       PRINT COLUMN 02,"ADMINISTRADORA: ","     ",campo.afore_cod USING "###","   ",campo.raz_social,
             COLUMN 140,precio[2].razon_social,precio[2].precio USING "###.######" 
       PRINT COLUMN 02,"FECHA         :","      ",g_fecha_accion USING"DD-MM-YYYY",
             COLUMN 140,precio[3].razon_social,precio[3].precio USING "###.######" 
       PRINT COLUMN 140,precio[4].razon_social,precio[4].precio USING "###.######" 
       PRINT COLUMN 140,precio[5].razon_social,precio[5].precio USING "###.######" 
       PRINT COLUMN 140,precio[6].razon_social,precio[6].precio USING "###.######" 

       PRINT COLUMN 02,"FOLIO         :","      ",r_arr[1].folio," ",r_arr[1].desc,
             COLUMN 90,"FOLIO         :","      ",r_arr[6].folio," ",r_arr[6].desc
       PRINT COLUMN 02,"FOLIO         :","      ",r_arr[2].folio," ",r_arr[2].desc,
             COLUMN 90,"FOLIO         :","      ",r_arr[7].folio," ",r_arr[7].desc
       PRINT COLUMN 02,"FOLIO         :","      ",r_arr[3].folio," ",r_arr[3].desc,
             COLUMN 90,"FOLIO         :","      ",r_arr[8].folio," ",r_arr[8].desc
       PRINT COLUMN 02,"FOLIO         :","      ",r_arr[4].folio," ",r_arr[4].desc,
             COLUMN 90,"FOLIO         :","      ",r_arr[9].folio," ",r_arr[9].desc
       PRINT COLUMN 02,"FOLIO         :","      ",r_arr[5].folio," ",r_arr[5].desc,
             COLUMN 90,"FOLIO         :","      ",r_arr[10].folio," ",r_arr[10].desc


       PRINT COLUMN 02,"__________________________________________________",
                       "__________________________________________________",
                       "__________________________________________________",
                       "______________________________"

       SKIP 1 LINE

       PRINT COLUMN 02,"SIEFORE",
             COLUMN 55,"SUBCUENTA",
             COLUMN 89,"ACCIONES/PAVIS",
             COLUMN 130,"MONTO EN PESOS"

       PRINT COLUMN 02,"--------------------------------------------------",
                       "--------------------------------------------------",
                       "--------------------------------------------------",
                       "------------------------------"

       BEFORE GROUP OF s.tipo_siefore

          PRINT COLUMN  2,"TOTALES POR SUBCUENTA "

       ON EVERY ROW

          PRINT COLUMN  2,s.tipo_siefore ,
                COLUMN 58,s.subcuenta       USING "###" ,
                COLUMN 84,s.acciones        USING "---,---,--&.######",
                COLUMN 125,s.pesos          USING "---,---,--&.######"

       AFTER GROUP  OF s.tipo_siefore

       PRINT COLUMN 02,"--------------------------------------------------",
                       "--------------------------------------------------",
                       "--------------------------------------------------",
                       "------------------------------"

         PRINT COLUMN  2,"TOTAL SIEFORE  ",s.tipo_siefore CLIPPED,
               COLUMN  85,GROUP SUM(s.acciones) USING "---,---,--&.######",
               COLUMN 125,GROUP SUM(s.pesos)    USING "---,---,--&.######"
        SKIP 1 LINES

         

        PRINT COLUMN 02,"--------------------------------------------------",
                        "--------------------------------------------------",
                        "--------------------------------------------------",
                        "------------------------------"

        ON LAST ROW
          
           SKIP 1 LINE

           PRINT COLUMN  2,"TOTAL CUENTAS","==>",g_total_cuentas USING "######&"," ","REGS."


END REPORT
################################################################################
#fp--------------
FUNCTION fol_procesados()

   LET i= 0

   DECLARE  fols CURSOR FOR
      SELECT a.folio
      FROM safre_af:tes_solicitud a
      WHERE a.fecha_traspaso = TODAY
      AND a.estado = 103
      GROUP BY 1

   FOREACH fols INTO g.*

         LET   i = i + 1

         LET r_arr[i].folio = g.fols   CLIPPED

         SELECT b.tipo_traspaso,c.descripcion
         INTO gtip_tra,r_arr[i].desc
         FROM tes_ctr_folio b,
              tes_tipo_id_aportante c
         WHERE b.folio = g.fols  
         AND   c.tipo_traspaso = b.tipo_traspaso

   END FOREACH

   FOR i = 1 TO 10
     IF r_arr[i].folio IS NULL THEN 
        LET r_arr[i].desc = "N/A"
     END IF
   END FOR

END FUNCTION
