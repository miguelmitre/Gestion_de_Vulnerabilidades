DATABASE safre_af
GLOBALS

    DEFINE tmv  RECORD
       folio             LIKE safre_af:dis_cuenta.folio,
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

    DEFINE xnss  RECORD
       tipo_siefore          CHAR(11),
       nss                   CHAR(11),
       subcuenta             SMALLINT,
       acciones              DECIMAL(16,6),
       pesos                 DECIMAL(16,6)
    END RECORD
    
   

END GLOBALS

GLOBALS "GLOB_TRAS.4gl"

FUNCTION genera_reporte()
#gr---------------------
DEFINE l_hora  CHAR(005)
DEFINE l_enter CHAR(005)

DEFINE l_impre_tm     ,
       l_impre_sub    ,
       l_impre_xnss   ,
       l_lista        CHAR(300)
 

    LET l_hora = TIME

    LET l_impre_tm   = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
	  ".LIS_",g_tip_rep CLIPPED,"_TIPMOV.",HOY USING "DDMMYY", "_",l_hora[1,2],l_hora[4,5] CLIPPED

    LET l_impre_sub  = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
	  ".LIS_",g_tip_rep CLIPPED,"_SUBCTA.",HOY USING "DDMMYY", "_",l_hora[1,2],l_hora[4,5] CLIPPED

    LET l_impre_xnss = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
	  ".LIS_",g_tip_rep CLIPPED,"_XNSS.",HOY USING "DDMMYY", "_",l_hora[1,2],l_hora[4,5] CLIPPED

    LET l_impre_tm   =  l_impre_tm   CLIPPED
    LET l_impre_sub  =  l_impre_sub  CLIPPED
    LET l_impre_xnss =  l_impre_xnss CLIPPED

    START REPORT rpt_tipmov TO l_impre_tm
    START REPORT rpt_subcta TO l_impre_sub       
    START REPORT rpt_xnss TO l_impre_xnss       

    CALL cuentas_impri()
   
    LET l_lista = "chmod 777 ",l_impre_tm CLIPPED
    RUN l_lista

    LET l_lista = "chmod 777 ",l_impre_sub CLIPPED
    RUN l_lista

    LET l_lista = "chmod 777 ",l_impre_xnss CLIPPED
    RUN l_lista

    LET l_lista = "lp ",l_impre_tm
    RUN l_lista

    LET l_lista = "lp ",l_impre_sub
    RUN l_lista

    LET l_lista = "lp ",l_impre_xnss
    RUN l_lista

END FUNCTION

FUNCTION cuentas_impri()
#ci--------------------

   CALL define_querys()   

   PREPARE sql_01 FROM g_sql_01
   DECLARE cur_sql_01  CURSOR  FOR sql_01

   FOREACH cur_sql_01  USING g_folio
   INTO  tmv.*

      IF tmv.fecha_conversion IS NULL OR tmv.fecha_conversion="12/31/1899" THEN
         LET tmv.fecha_conversion = NULL
      END IF
         OUTPUT TO REPORT rpt_tipmov(tmv.*)
   END FOREACH

   PREPARE sql_02 FROM g_sql_02
   DECLARE cur_sql_02 CURSOR  FOR sql_02

   FOREACH cur_sql_02 USING g_folio
   INTO  sub.*
         OUTPUT TO REPORT rpt_subcta(sub.*)
   END FOREACH

   PREPARE sql_06 FROM g_sql_06
   DECLARE cur_sql_06 CURSOR  FOR sql_06

   FOREACH cur_sql_06 USING g_folio
   INTO  xnss.*
         OUTPUT TO REPORT rpt_xnss(xnss.*)
   END FOREACH


   FINISH REPORT rpt_tipmov       

   FINISH REPORT rpt_subcta      

   FINISH REPORT rpt_xnss        

   DISPLAY  "LISTADO IMPRESO..." 

END FUNCTION

REPORT rpt_tipmov(r_tmv)
#rtmov------------------
    DEFINE tot_siefores,i SMALLINT
    DEFINE r_tmv  RECORD
       folio             LIKE safre_af:dis_cuenta.folio,
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
    DEFINE precios               ARRAY[11] OF RECORD 
           razon_social          CHAR(015),
           precio                DECIMAL(16,6)
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
	   PRINT COLUMN  02,g_nombre_programa,
	         COLUMN 165,"Pagina: ",PAGENO USING "<<<<"
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
       WHERE  a.codigo_siefore not in (0)

       FOR i = 1 TO tot_siefores

        SELECT a.razon_social  ,
               b.precio_del_dia
          INTO precios[i].razon_social ,
               precios[i].precio
          FROM tab_siefore_local a     ,
               glo_valor_accion  b
         WHERE a.codigo_siefore  = b.codigo_siefore
           AND b.fecha_valuacion = g_fecha_accion
           AND a.codigo_siefore  = i
       END FOR

        SKIP 1 LINE
        PRINT COLUMN 02,"ADMINISTRADORA: ","     ",campo.afore_cod USING "###","   ",campo.raz_social,
              COLUMN 135,precios[1].razon_social,": ",precios[1].precio USING "###.######"
        PRINT COLUMN 02,"FOLIO         :",g_folio,"   ",g_tipo_desc1,
              COLUMN 135,precios[2].razon_social,": ",precios[2].precio USING "###.######"
        PRINT COLUMN 02,"FECHA         :","      ",g_fecha_accion USING"DD-MM-YYYY",
              COLUMN 135,precios[3].razon_social,": ",precios[3].precio USING "###.######"
        PRINT COLUMN 135,precios[4].razon_social,": ",precios[4].precio USING "###.######"
        PRINT COLUMN 135,precios[5].razon_social,": ",precios[5].precio USING "###.######"
        PRINT COLUMN 135,precios[6].razon_social,": ",precios[6].precio USING "###.######"
        PRINT COLUMN 135,precios[11].razon_social,": ",precios[11].precio USING "###.######"

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
           INTO   sie_rep
           FROM tab_siefore_local a
           WHERE a.razon_social = r_tmv.tipo_siefore
	
        PRINT COLUMN 02,"--------------------------------------------------",
                        "--------------------------------------------------",
                        "--------------------------------------------------",
                        "------------------------------"
        SKIP 1 LINE

        PRINT COLUMN  2,"TOTAL POR TIPO DE MOVIMIENTO ==>",r_tmv.tipo_siefore CLIPPED

        PREPARE sql_03  FROM g_sql_03
        DECLARE cur_sql_03 CURSOR FOR sql_03

        FOREACH cur_sql_03 USING g_folio ,sie_rep
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
DEFINE tot_siefores,i SMALLINT    
DEFINE s  RECORD
   tipo_siefore          CHAR(11),
   subcuenta             SMALLINT,
   acciones              DECIMAL(16,6),
   pesos                 DECIMAL(16,6)
END RECORD
DEFINE precios  ARRAY[11] OF RECORD 
       razon_social CHAR(15),
       precio       DECIMAL(16,6)
END RECORD
DEFINE fech_conv              DATE
DEFINE  campo         RECORD
        afore_cod             CHAR(03),
        raz_social            CHAR(50)
                      END RECORD
DEFINE tip_tra                SMALLINT
	
    OUTPUT
	TOP MARGIN    0
	BOTTOM MARGIN 0
	LEFT MARGIN   0
	RIGHT MARGIN  0
	PAGE LENGTH   90

   ORDER BY s.tipo_siefore,s.subcuenta

    FORMAT
       PAGE HEADER
          PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

          PRINT COLUMN 02,g_nombre_programa ,
                COLUMN 165,"Pagina:  ",PAGENO USING "<<<<"
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
       WHERE  a.codigo_siefore not in (0)

       FOR i = 1 TO tot_siefores

        SELECT a.razon_social  ,
               b.precio_del_dia
          INTO precios[i].razon_social ,
               precios[i].precio
          FROM tab_siefore_local a     ,
               glo_valor_accion  b
         WHERE a.codigo_siefore  = b.codigo_siefore
           AND b.fecha_valuacion = g_fecha_accion
           AND a.codigo_siefore  = i
       END FOR

          SKIP 1 LINE
        PRINT COLUMN 02,"ADMINISTRADORA: ","     ",campo.afore_cod USING "###","   ",campo.raz_social,
              COLUMN 135,precios[1].razon_social,": ",precios[1].precio USING "###.######"
        PRINT COLUMN 02,"FOLIO         :",g_folio,"   ",g_tipo_desc1,
              COLUMN 135,precios[2].razon_social,": ",precios[2].precio USING "###.######"
        PRINT COLUMN 02,"FECHA         :","      ",g_fecha_accion USING"DD-MM-YYYY",
              COLUMN 135,precios[3].razon_social,": ",precios[3].precio USING "###.######"
        PRINT COLUMN 135,precios[4].razon_social,": ",precios[4].precio USING "###.######"
        PRINT COLUMN 135,precios[5].razon_social,": ",precios[5].precio USING "###.######"
        PRINT COLUMN 135,precios[6].razon_social,": ",precios[6].precio USING "###.######"
        PRINT COLUMN 135,precios[11].razon_social,": ",precios[11].precio USING "###.######"

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
###############################################################################
REPORT rpt_xnss(n)
---bueno
DEFINE tot_siefores,i SMALLINT
DEFINE n  RECORD
       tipo_siefore          CHAR(11),
       nss                   CHAR(11),
       subcuenta             SMALLINT,
       acciones              DECIMAL(16,6),
       pesos                 DECIMAL(16,6)
END RECORD
    DEFINE precios                ARRAY[11] OF RECORD
           razon_social           CHAR(015),
           precio                 DECIMAL(16,6)
    END RECORD

DEFINE fech_conv              DATE

DEFINE campo         RECORD
       afore_cod             CHAR(03),
       raz_social            CHAR(50)
                      END RECORD
DEFINE tip_tra                SMALLINT
	
    OUTPUT
	TOP MARGIN    0
	BOTTOM MARGIN 0
	LEFT MARGIN   0
	RIGHT MARGIN  0
	PAGE LENGTH   90

   ORDER BY n.tipo_siefore,n.nss,n.subcuenta

    FORMAT
       PAGE HEADER
          PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

          PRINT COLUMN 02,g_nombre_programa ,
                COLUMN 165,"Pagina:  ",PAGENO USING "<<<<"
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
       WHERE  a.codigo_siefore not in (0)

       FOR i = 1 TO tot_siefores

        SELECT a.razon_social  ,
               b.precio_del_dia
          INTO precios[i].razon_social ,
               precios[i].precio
          FROM tab_siefore_local a     ,
               glo_valor_accion  b
         WHERE a.codigo_siefore  = b.codigo_siefore
           AND b.fecha_valuacion = g_fecha_accion
           AND a.codigo_siefore  = i
       END FOR

          SKIP 1 LINE
          
        PRINT COLUMN 02,"ADMINISTRADORA: ","     ",campo.afore_cod USING "###","   ",campo.raz_social,
              COLUMN 135,precios[1].razon_social,": ",precios[1].precio USING "###.######"
        PRINT COLUMN 02,"FOLIO         :",g_folio,"   ",g_tipo_desc1,
              COLUMN 135,precios[2].razon_social,": ",precios[2].precio USING "###.######"
        PRINT COLUMN 02,"FECHA         :","      ",g_fecha_accion USING"DD-MM-YYYY",
              COLUMN 135,precios[3].razon_social,": ",precios[3].precio USING "###.######"
        PRINT COLUMN 135,precios[4].razon_social,": ",precios[4].precio USING "###.######"

        PRINT COLUMN 02,"__________________________________________________",
                        "__________________________________________________",
                        "__________________________________________________",
                        "______________________________"

        SKIP 1 LINE

        PRINT COLUMN 02,"SIEFORE",
              COLUMN 34,"NSS",  
              COLUMN 62,"SUBCUENTA",
              COLUMN 93,"ACCIONES/PAVIS",
              COLUMN 134,"MONTO EN PESOS"

        PRINT COLUMN 02,"--------------------------------------------------",
                        "--------------------------------------------------",
                        "--------------------------------------------------",
                        "------------------------------"

        ON EVERY ROW

           PRINT COLUMN   2,n.tipo_siefore                       ,
                 COLUMN  30,n.nss       , ###USING "###########" ,
                 COLUMN  65,n.subcuenta      USING "###" ,
                 COLUMN  89,n.acciones       USING "---,---,--&.######",
                 COLUMN 130,n.pesos          USING "---,---,--&.######"

        AFTER GROUP  OF n.tipo_siefore

        PRINT COLUMN 02,"--------------------------------------------------",
                        "--------------------------------------------------",
                        "--------------------------------------------------",
                        "------------------------------"

         PRINT COLUMN  2,"TOTAL SIEFORE  ",n.tipo_siefore CLIPPED,
               COLUMN  89,GROUP SUM(n.acciones) USING "---,---,--&.######",
               COLUMN 130,GROUP SUM(n.pesos)    USING "---,---,--&.######"
         SKIP 1 LINES

          

         PRINT COLUMN 02,"--------------------------------------------------",
                        "--------------------------------------------------",
                        "--------------------------------------------------",
                        "------------------------------"

         ON LAST ROW
           
            SKIP 1 LINE

            PRINT COLUMN  2,"TOTAL CUENTAS","==>",g_total_cuentas USING "######&"," ","REGS.",
               COLUMN  89,SUM(n.acciones) USING "---,---,--&.######",
               COLUMN 130,SUM(n.pesos)    USING "---,---,--&.######"


END REPORT
