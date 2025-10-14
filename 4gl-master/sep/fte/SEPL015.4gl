###########################################################################
#Proyecto          => AFORE ( MEXICO )
#Propietario       => E.F.P.
#Programa SEPL015  => GENERA 2 REPORTES PARA EL MODULO DE SEPARACION DE 
#                     CUENTAS(ES DECIR UNO POR ==> TIPO_MOVIMIENTO
#                                      UNO POR ==> SUBCUENTA
#                     DE LA TABLA safre_af:dis_cuenta).
#                     ESTE PROGRAMA RECIBIRA COMO PARAMETRO UN ( FOLIO )
#Autor             => MARCO ANTONIO GONZALEZ ROJAS
#Fecha             => 30 MAY 2005
#Sistema           => SEP 
###########################################################################
DATABASE safre_af
GLOBALS 

DEFINE ghoy        DATE
DEFINE g_enter     CHAR(01)
DEFINE g_sql_01    ,
       g_sql_02    ,
       g_sql_03    ,           
       g_sql_04    CHAR(1000),
       g_tabname   CHAR(50)

###### variables globales para reporte ######
  #====                       =====
  DEFINE  g         RECORD
          fols      CHAR(10)
                    END RECORD

  DEFINE g_reg      RECORD
         folios  CHAR(300)
                   END RECORD

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
         nss               LIKE safre_af:dis_cuenta.nss,
         tipo_siefore      CHAR(11),
         tip_mov           LIKE safre_af:dis_cuenta.tipo_movimiento,
         subcuenta         LIKE safre_af:dis_cuenta.subcuenta,
         fecha_valor       LIKE safre_af:dis_cuenta.fecha_valor,
         fecha_conversion  LIKE safre_af:dis_cuenta.fecha_conversion,
         acciones          DECIMAL(16,6),
         pesos             DECIMAL(16,6),
         id_aportante      LIKE safre_af:dis_cuenta.id_aportante,
         precio_accion     LIKE safre_af:dis_cuenta.precio_accion
  END RECORD

  DEFINE sub  RECORD
         nss                   LIKE safre_af:dis_cuenta.nss,
         tipo_siefore          CHAR(11),
         subcuenta             SMALLINT,
         fecha_valor       LIKE safre_af:dis_cuenta.fecha_valor,
         fecha_conversion  LIKE safre_af:dis_cuenta.fecha_conversion,
         acciones              DECIMAL(16,6),
         pesos                 DECIMAL(16,6),
         id_aportante      LIKE safre_af:dis_cuenta.id_aportante,
         precio_accion     LIKE safre_af:dis_cuenta.precio_accion
  END RECORD

############################################
END GLOBALS

MAIN

 CALL asigna_globales()

 CALL genera_reporte()
 
END MAIN

FUNCTION define_querys()
#dq--------------

LET g_sql_01 =  #qry para reporte por tipo_movimiento
   'SELECT b.nss,CASE WHEN b.siefore = 0  THEN "PAVIS" ',
   '       WHEN b.siefore = 1  THEN "BASICA 1" ',
   '       WHEN b.siefore = 2  THEN "BASICA 2" ' ,
   '       WHEN b.siefore = 11 THEN "PAVIS" ' ,
   '   END CASE,' ,
   '       b.tipo_movimiento,b.subcuenta,b.fecha_valor,b.fecha_conversion, ',
   '       b.monto_en_acciones,b.monto_en_pesos,b.id_aportante, ',
   '       b.precio_accion ',
   '  FROM  ',g_tabname ,
   ' WHERE  b.folio = ?',
   ' ORDER  BY  2,3,4,5,6,7,8,9,10 '

LET g_sql_02 =  #qry para reporte de subcuenta
   'SELECT b.nss,CASE WHEN b.siefore = 0  THEN "PAVIS" ',
   '       WHEN b.siefore = 1  THEN "BASICA 1" ',
   '       WHEN b.siefore = 2  THEN "BASICA 2" ' ,
   '       WHEN b.siefore = 11 THEN "PAVIS" ' ,
   '   END CASE,' ,
   '       b.subcuenta,b.fecha_valor,b.fecha_conversion,b.monto_en_acciones, ',
   '       b.monto_en_pesos,b.id_aportante,b.precio_accion ',
   '  FROM  ',g_tabname ,
   ' WHERE  b.folio = ?',
   ' ORDER  BY  2,3,4,5,6,7,8,9 '

LET g_sql_03 = #qry para reporte por tipo_movimiento
   'SELECT b.tipo_movimiento, ',
   '       SUM(b.monto_en_acciones),SUM(b.monto_en_pesos) ',
   '  FROM  ', g_tabname,
   ' WHERE  b.folio = ? ',
   '   AND  b.siefore = ? ',
   '   AND  b.nss   = ? ',
   ' GROUP  BY  1 ',
   ' ORDER  BY  1 '
 
LET g_sql_04 = #qry para reporte por subcuenta       
   'SELECT b.subcuenta, ',
   '       SUM(b.monto_en_acciones),SUM(b.monto_en_pesos) ',
   '  FROM  ', g_tabname,
   ' WHERE  b.folio = ? ',
   '   AND  b.siefore = ? ',
   '   AND  b.nss   = ? ',
   ' GROUP  BY  1 ',
   ' ORDER  BY  1 '

END FUNCTION

FUNCTION asigna_globales()
#ag-----------------------

  LET     g_folio           =    ARG_VAL(1)
  LET     ghoy              =    TODAY                

  SELECT  user
  INTO  g_usuario
  FROM  tab_afore_local

  SELECT  *
  INTO  g_seg_modulo.*
  FROM  seg_modulo
  WHERE  modulo_cod = 'sep'
  

  LET     g_tabname         =    'dis_cuenta b '

  SELECT COUNT(unique nss)
  INTO   g_total_cuentas
  FROM   safre_af:dis_cuenta b
  WHERE  b.folio  =  g_folio
  
{  #CHECAR ESTO#####
  SELECT UNIQUE e.fecha_liquidacion 
  INTO g_fecha_accion
  FROM safre_af:tes_ctr_folio e
  WHERE  e.folio IN ( SELECT a.folio FROM safre_af:tes_solicitud a
                      WHERE a.fecha_traspaso = TODAY 
                        AND a.estado = 103) 
 
  LET    g_fecha_parti      =     g_fecha_accion
}

  LET    g_nombre_programa  =     "SEPL015"

  LET    g_tip_rep          =     'SS' 


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

 FOREACH cur_sql_01 USING g_folio 
 INTO  tmv.*

    IF tmv.fecha_conversion IS NULL OR tmv.fecha_conversion="12/31/1899" THEN
       LET tmv.fecha_conversion = NULL
    END IF

    IF tmv.fecha_valor IS NULL OR tmv.fecha_valor ="12/31/1899" THEN
       LET tmv.fecha_valor = NULL
    END IF

    OUTPUT TO REPORT rpt_tipmov(tmv.*)
 END FOREACH

 PREPARE sql_02 FROM g_sql_02
 DECLARE cur_sql_02 CURSOR  FOR sql_02

 FOREACH cur_sql_02 USING g_folio 
 INTO  sub.*
    OUTPUT TO REPORT rpt_subcta(sub.*)
 END FOREACH

 FINISH REPORT rpt_tipmov       

 FINISH REPORT rpt_subcta      
 DISPLAY " "
 DISPLAY  "SEPL015:REPORTE POR SEPARACION DE CUENTAS..."
 DISPLAY " "

END FUNCTION

REPORT rpt_tipmov(r_tmv)
#rtmov------------------

  DEFINE r_tmv  RECORD
         nss               LIKE safre_af:dis_cuenta.nss,
         tipo_siefore      CHAR(11),
         tip_mov           LIKE safre_af:dis_cuenta.tipo_movimiento,
         subcuenta         LIKE safre_af:dis_cuenta.subcuenta,
         fecha_valor       LIKE safre_af:dis_cuenta.fecha_valor,
         fecha_conversion  LIKE safre_af:dis_cuenta.fecha_conversion,
         acciones          DECIMAL(16,6),
         pesos             DECIMAL(16,6),
         id_aportante      LIKE safre_af:dis_cuenta.id_aportante,
         precio_accion     LIKE safre_af:dis_cuenta.precio_accion
  END RECORD

  DEFINE campo      RECORD
         afore_cod  CHAR(03),
         raz_social CHAR(50)
                    END  RECORD
  DEFINE fech_conv              DATE
  DEFINE pre_acc_0              DECIMAL(16,6)
  DEFINE pre_acc_1              DECIMAL(16,6)
  DEFINE pre_acc_2              DECIMAL(16,6)
  DEFINE pre_acc_11             DECIMAL(16,6)
  DEFINE sie_rep                SMALLINT 
  DEFINE tottip_mov             CHAR(03)
  DEFINE tip_mov_acc            DECIMAL(16,6)
  DEFINE mto                    DECIMAL(16,6)
  DEFINE tip_mov_pes            DECIMAL(16,6)
  DEFINE tip_tra                SMALLINT

     OUTPUT
        TOP MARGIN    0
        BOTTOM MARGIN 0
        LEFT MARGIN   0
        RIGHT MARGIN  0
        PAGE LENGTH   90

     ORDER BY r_tmv.nss,r_tmv.tipo_siefore,r_tmv.tip_mov,r_tmv.subcuenta  

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
    
    SKIP 1 LINE

    PRINT COLUMN  02,"REPORTE       : ","     ",g_nombre_programa CLIPPED,"  SEPARACION DE CUENTA POR TIPO MOVIMIENTO"
    PRINT COLUMN 02,"ADMINISTRADORA: ","     ",campo.afore_cod USING "###","   ",campo.raz_social
    PRINT COLUMN 02,"FECHA         :","      ",ghoy USING"DD-MM-YYYY"
    PRINT COLUMN 02,"FOLIO         :"," ",g_folio,"   ",g_tipo_desc1     


    PRINT COLUMN 02,"__________________________________________________",
                    "__________________________________________________",
                    "__________________________________________________",
                    "______________________________"

    SKIP 1 LINE

    PRINT COLUMN 02,"SIEFORE",
          COLUMN 23,"TIPO_MOV",
          COLUMN 35,"SUBCUENTA",
          COLUMN 50,"FECHA_VALOR",
          COLUMN 65,"FECHA_CONVER",
          COLUMN 85,"ACCIONES/PAVIS",
          COLUMN 119,"MONTO EN PESOS",
          COLUMN 140,"ID APORTANTE",
          COLUMN 161,"PRECIO_ACCION"

    PRINT COLUMN 02,"--------------------------------------------------",
                    "--------------------------------------------------",
                    "--------------------------------------------------",
                    "------------------------------"

    BEFORE GROUP OF r_tmv.nss
       PRINT COLUMN  2,"NSS: ",r_tmv.nss CLIPPED

 ON EVERY ROW
     PRINT COLUMN  2,r_tmv.tipo_siefore ,
           COLUMN 23,r_tmv.tip_mov,
           COLUMN 35,r_tmv.subcuenta         USING "###" ,
           COLUMN 50,r_tmv.fecha_valor,      
           COLUMN 65,r_tmv.fecha_conversion,
           COLUMN 80,r_tmv.acciones          USING "---,---,--&.######",
           COLUMN 115,r_tmv.pesos            USING "---,---,--&.######",
           COLUMN 142,r_tmv.id_aportante  CLIPPED, 
           COLUMN 155,r_tmv.precio_accion USING "---,---,--&.######"

    AFTER GROUP  OF r_tmv.tipo_siefore
       CASE r_tmv.tipo_siefore
          WHEN "SIN SIEFORE"
             LET sie_rep = 0
          WHEN "BASICA 1"
             LET sie_rep = 1
          WHEN "BASICA 2"
             LET sie_rep = 2
          WHEN "PAVIS"
             LET sie_rep = 11
          OTHERWISE
      END CASE  
 
    PRINT COLUMN 02,"--------------------------------------------------",
                    "--------------------------------------------------",
                    "--------------------------------------------------",
                    "------------------------------"
    SKIP 1 LINE

    PRINT COLUMN  2,"TOTAL POR TIPO DE MOVIMIENTO ==>",r_tmv.tipo_siefore CLIPPED

    LET mto  =    0

    PREPARE sql_03  FROM g_sql_03
    DECLARE cur_sql_03 CURSOR FOR sql_03
    
    FOREACH cur_sql_03 USING g_folio ,sie_rep ,r_tmv.nss
    INTO tottip_mov,tip_mov_acc,tip_mov_pes

       LET mto   = mto + tip_mov_acc

       PRINT COLUMN 26,tottip_mov,
             COLUMN 80,tip_mov_acc USING "---,---,--&.######",
             COLUMN 115,tip_mov_pes USING "---,---,--&.######"

    END FOREACH
       PRINT COLUMN 84,"_______________" 
       PRINT COLUMN 55,"TOTAL POR NSS Y SIEFORE: ",
             COLUMN 80,mto USING "---,---,--&.######"

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
 DEFINE  s    RECORD
         nss                   LIKE safre_af:dis_cuenta.nss,
         tipo_siefore          CHAR(11),
         subcuenta             SMALLINT,
         fecha_valor       LIKE safre_af:dis_cuenta.fecha_valor,
         fecha_conversion  LIKE safre_af:dis_cuenta.fecha_conversion,
         acciones              DECIMAL(16,6),
         pesos                 DECIMAL(16,6),
         id_aportante      LIKE safre_af:dis_cuenta.id_aportante,
         precio_accion     LIKE safre_af:dis_cuenta.precio_accion
  END RECORD

DEFINE fech_conv              DATE
DEFINE  campo         RECORD
        afore_cod             CHAR(03),
        raz_social            CHAR(50)
                      END RECORD
DEFINE tip_tra                SMALLINT
DEFINE sie_reps               SMALLINT
DEFINE tottip_sub             CHAR(03)
DEFINE tip_mov_accs           DECIMAL(16,6)
DEFINE tip_mov_pess           DECIMAL(16,6)
DEFINE mtos                   DECIMAL(16,6)


   OUTPUT
      TOP MARGIN     0
      BOTTOM MARGIN  0
      LEFT MARGIN    0
      RIGHT MARGIN   0
      PAGE LENGTH   90

  ORDER BY s.nss,s.tipo_siefore,s.subcuenta

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

         SKIP 1 LINE
         
       PRINT COLUMN  02,"REPORTE       : ","     ",g_nombre_programa CLIPPED," SEPARACION DE CUENTA POR SUBCUENTA"
       PRINT COLUMN 02,"ADMINISTRADORA: ","     ",campo.afore_cod USING "###","   ",campo.raz_social
       PRINT COLUMN 02,"FECHA         :","      ",ghoy USING"DD-MM-YYYY"
       PRINT COLUMN 02,"FOLIO         :","  ",g_folio,"   ",g_tipo_desc2


       PRINT COLUMN 02,"__________________________________________________",
                       "__________________________________________________",
                       "__________________________________________________",
                       "______________________________"

       SKIP 1 LINE

       PRINT COLUMN 02,"SIEFORE",
             COLUMN 23,"SUBCUENTA",
             COLUMN 35,"FECHA_VALOR",
             COLUMN 50,"FECHA_CONVER",
             COLUMN 70,"ACCIONES/PAVIS",
             COLUMN 90,"MONTO EN PESOS",
             COLUMN 119,"ID APORTANTE",
             COLUMN 146,"PRECIO_ACCION"
             

       PRINT COLUMN 02,"--------------------------------------------------",
                       "--------------------------------------------------",
                       "--------------------------------------------------",
                       "------------------------------"
       BEFORE GROUP OF s.nss
          PRINT COLUMN  2,"NSS: ",s.nss CLIPPED


       BEFORE GROUP OF s.tipo_siefore

          PRINT COLUMN  2,"DETALLE POR SUBCUENTA "

       ON EVERY ROW

          PRINT COLUMN 02,s.tipo_siefore ,
                COLUMN 23,s.subcuenta     USING "###" ,
                COLUMN 35,s.fecha_valor,
                COLUMN 50,s.fecha_conversion,
                COLUMN 65,s.acciones      USING "---,---,--&.######",
                COLUMN 85,s.pesos         USING "---,---,--&.######",
                COLUMN 119,s.id_aportante CLIPPED,
                COLUMN 139,s.precio_accion USING "---,---,--&.######"

       AFTER GROUP  OF s.tipo_siefore

          CASE s.tipo_siefore
             WHEN "SIN SIEFORE"
                LET sie_reps = 0
             WHEN "BASICA 1"
                LET sie_reps = 1
             WHEN "BASICA 2"
                LET sie_reps = 2
             WHEN "PAVIS"
                LET sie_reps = 11
             OTHERWISE
      END CASE
 

       PRINT COLUMN 02,"--------------------------------------------------",
                       "--------------------------------------------------",
                       "--------------------------------------------------",
                       "------------------------------"

        SKIP 1 LINES

        PRINT COLUMN  2,"TOTAL SIEFORE  ",s.tipo_siefore CLIPPED
    
        LET mtos =    0

        PREPARE sql_04  FROM g_sql_04
        DECLARE cur_sql_04 CURSOR FOR sql_04

        FOREACH cur_sql_04 USING g_folio ,sie_reps,s.nss
           INTO tottip_sub,tip_mov_accs,tip_mov_pess

           LET mtos   = mtos + tip_mov_accs

           PRINT COLUMN 26,tottip_sub,
                 COLUMN 65,tip_mov_accs USING "---,---,--&.######",
                 COLUMN 85,tip_mov_pess USING "---,---,--&.######"

       END FOREACH

       PRINT COLUMN 69,"_______________"
       PRINT COLUMN 35,"TOTAL POR NSS Y SIEFORE: ",
             COLUMN 65,mtos USING "---,---,--&.######"

    
        SKIP 1 LINES

         

        PRINT COLUMN 02,"--------------------------------------------------",
                        "--------------------------------------------------",
                        "--------------------------------------------------",
                        "------------------------------"

        ON LAST ROW
          
           SKIP 1 LINE

           PRINT COLUMN  2,"TOTAL CUENTAS","==>",g_total_cuentas USING "######&"," ","REGS."


END REPORT
