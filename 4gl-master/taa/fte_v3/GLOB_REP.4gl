###########################################################################
#Proyecto          => AFORE ( MEXICO )                                    #
#Propietario       => E.F.P.                                              #
#Programa GLOB_REP => GENERA LISTADO DE CIFRAS  PROVISION Y LIQUIDACION   #
#Sistema           => TCAA                                                #
#Fecha Creacion    => 23/06/2009                                          #
#Autor             => JOSE  FRANCISCO  LUGO  CORNEJO                      #
###########################################################################
DATABASE  safre_af
GLOBALS

    DEFINE   tmv        RECORD
       folio                         LIKE safre_af:dis_cuenta.folio,
       siefore                       SMALLINT,
       tip_mov                       LIKE safre_af:dis_cuenta.tipo_movimiento,
       subcuenta                     LIKE safre_af:dis_cuenta.subcuenta,
       fecha_conversion              LIKE safre_af:dis_cuenta.fecha_conversion,
       acciones                      DECIMAL(16,6),
       pesos                         DECIMAL(16,6)
                       END RECORD

    DEFINE sub         RECORD
       siefore                       SMALLINT,
       subcuenta                     SMALLINT,
       acciones                      DECIMAL(16,6),
       pesos                         DECIMAL(16,6)
                       END RECORD
   DEFINE   g_siefore                SMALLINT
   DEFINE   g_val_accion  ARRAY[50]  OF   RECORD  --siefore
            nom_siefore              CHAR(020),
            precio_accion            DEC(19,14)
                       END   RECORD
   DEFINE    g_enter                 CHAR(01)
END GLOBALS

GLOBALS "GLOB_REPS.4gl"

FUNCTION genera_reporte()
    DEFINE   l_hora                         ,
             l_enter                        CHAR(005)
    DEFINE   l_impre_tm                     ,
             l_impre_sub                    ,
             l_lista                        CHAR(300)
    CALL     STARTLOG("GLOB_REP.log")
    CALL     define_querys()   
    CALL     F_010_trae_precios_accion() 

    LET l_hora = TIME

    LET l_impre_tm = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
	  ".LIS_",g_tip_rep CLIPPED,"_TIPMOV.",HOY USING "DDMMYY", "_",l_hora[1,2] CLIPPED,l_hora[4,5] CLIPPED

    LET l_impre_sub = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
	  ".LIS_",g_tip_rep CLIPPED,"_SUBCTA.",HOY USING "DDMMYY", "_",l_hora[1,2] CLIPPED,l_hora[4,5] CLIPPED

    LET l_impre_tm  = l_impre_tm  CLIPPED
    LET l_impre_sub = l_impre_sub CLIPPED

    START REPORT rpt_tipmov TO l_impre_tm
    START REPORT rpt_subcta TO l_impre_sub       

    CALL cuentas_impri()
   
    LET l_lista = "chmod 777 ",l_impre_tm CLIPPED
    RUN l_lista

    LET l_lista = "chmod 777 ",l_impre_sub CLIPPED
    RUN l_lista

    LET l_lista = "lp ",l_impre_tm
    RUN l_lista

    LET l_lista = "lp ",l_impre_sub
    RUN l_lista

END FUNCTION

FUNCTION   F_010_trae_precios_accion() 
   DEFINE   l_precio                      DEC(19,14)
   DEFINE   l_nom_siefore                 CHAR(50)
   FOR      g_siefore   =  1     TO   50
            LET    g_val_accion  [g_siefore].precio_accion    =  0
   END FOR
   PREPARE  sql_06      FROM    g_sql_06
   DECLARE  cur_val_acc   CURSOR     FOR
   SELECT    codigo_siefore ,razon_social
     FROM    tab_siefore_local
    WHERE    codigo_siefore       >=  1
   FOREACH  cur_val_acc      INTO    g_siefore,l_nom_siefore
             EXECUTE   sql_06     USING   g_folio,g_siefore
                                   INTO   l_precio 
            LET   g_val_accion  [g_siefore].nom_siefore      =  l_nom_siefore 
            LET   g_val_accion  [g_siefore].precio_accion    =  l_precio 
                  
   END FOREACH
END FUNCTION

FUNCTION cuentas_impri()
   CALL     define_querys()   
   PREPARE  sql_01      FROM    g_sql_01
   DECLARE  cur_sql_01  CURSOR  FOR sql_01
   FOREACH  cur_sql_01  USING   g_folio      INTO  tmv.*
            IF       tmv.fecha_conversion      IS  NULL          OR
                     tmv.fecha_conversion       = "12/31/1899"   THEN
                     LET      tmv.fecha_conversion      =  NULL
            END IF
            OUTPUT   TO  REPORT rpt_tipmov(tmv.*)
   END FOREACH
   PREPARE  sql_02 FROM g_sql_02
   DECLARE  cur_sql_02      CURSOR  FOR sql_02
   FOREACH  cur_sql_02      USING   g_folio     INTO  sub.*
         OUTPUT             TO  REPORT  rpt_subcta(sub.*)
   END FOREACH
   FINISH   REPORT     rpt_tipmov       
   FINISH   REPORT     rpt_subcta      
   DISPLAY  "LISTADO IMPRESO..." 
END FUNCTION

REPORT rpt_tipmov(r_tmv)
    DEFINE r_tmv  RECORD
       folio                      LIKE safre_af:dis_cuenta.folio,
       siefore                    SMALLINT,
       tip_mov                    LIKE safre_af:dis_cuenta.tipo_movimiento,
       subcuenta                  LIKE safre_af:dis_cuenta.subcuenta,
       fecha_conversion           LIKE safre_af:dis_cuenta.fecha_conversion,
       acciones                   ,
       pesos                      DECIMAL(16,6)
    END RECORD

    DEFINE campo      RECORD
           afore_cod  CHAR(03),
           raz_social CHAR(50)
                      END  RECORD
    DEFINE fech_conv              DATE
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

   ORDER BY r_tmv.siefore,r_tmv.tip_mov,r_tmv.subcuenta	 

    FORMAT

	PAGE HEADER
           PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
	   PRINT COLUMN  02,g_nombre_programa,
	         COLUMN 80,"Pagina: ",PAGENO USING "<<<<"
	   PRINT COLUMN 02,"__________________________________________________",
                           "________________________________________"
       
        SELECT codigo_afore,razon_social
        INTO campo.afore_cod,campo.raz_social       
        FROM tab_afore_local
        
        SKIP 1 LINE
        PRINT COLUMN 02,"ADMINISTRADORA: ","     ",campo.afore_cod USING "###","   ",campo.raz_social

	PRINT COLUMN 02,"FOLIO         :",g_folio,"   ",g_tipo_desc1
        PRINT COLUMN 02,"__________________________________________________",
                        "________________________________________"

               

        SKIP 1 LINE
        PRINT COLUMN 02,"SIEFORE",
              COLUMN 11,"T_MOV",
              COLUMN 18,"SUBCTA",
              COLUMN 33,"ACCIONES/PAVIS",
              COLUMN 73,"MONTO EN PESOS"
        PRINT COLUMN 02,"--------------------------------------------------",
                        "---------------------------------------"

  ON EVERY ROW
       LET    g_siefore    =  r_tmv.siefore
       PRINT COLUMN  7,r_tmv.siefore           USING "##" ,
             COLUMN  13,r_tmv.tip_mov          USING "###" ,
             COLUMN  22,r_tmv.subcuenta        USING "##" ,
             COLUMN  25,r_tmv.acciones         USING "---,---,---,--&.######",
             COLUMN  65,r_tmv.pesos            USING "---,---,---,--&.######"

        AFTER GROUP  OF r_tmv.siefore
  	
        PRINT COLUMN 02,"--------------------------------------------------",
                        "---------------------------------------"
        SKIP 1 LINE

        PRINT COLUMN  2,"TOTAL POR TIPO DE MOVIMIENTO ==>",r_tmv.siefore CLIPPED
        PREPARE  sql_03         FROM  g_sql_03
        DECLARE  cur_sql_03   CURSOR  FOR sql_03
        FOREACH  cur_sql_03    USING  g_folio ,sie_rep
            INTO     tottip_mov,tip_mov_acc,tip_mov_pes
            LET      g_siefore           =  sie_rep
            PRINT    COLUMN  13,tottip_mov,
                     COLUMN  25,tip_mov_acc USING "---,---,---,--&.######",
                     COLUMN  65,tip_mov_pes USING "---,---,---,--&.######"
     END FOREACH
        SKIP 1 LINES

        PRINT COLUMN  08,"SIEFORE=> ",
              COLUMN  21, g_siefore      USING   "##","  ",
                         g_val_accion  [g_siefore].nom_siefore,"        ",
                          "VALOR ACCION: ",
                         g_val_accion  [g_siefore].precio_accion
                              USING  "###.######"
        PRINT COLUMN 02,"--------------------------------------------------",
                        "---------------------------------------"
        ON LAST ROW
           
           SKIP 1 LINE

           PRINT COLUMN  2,"TOTAL CUENTAS","==>",
                           g_total_cuentas   USING  "####,##&"," ","REGS."
        SKIP 1 LINE
        PRINT COLUMN 02,"__________________________________________________",
                        "______________________________________"
END REPORT
###############################################################################
REPORT rpt_subcta(s)
---bueno
    
DEFINE s  RECORD
   siefore          SMALLINT,
   subcuenta             SMALLINT,
   acciones              DECIMAL(16,6),
   pesos                 DECIMAL(16,6)
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

   ORDER BY s.siefore,s.subcuenta

    FORMAT
       PAGE HEADER
          PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

          PRINT COLUMN 02,g_nombre_programa ,
                COLUMN 80,"Pagina:  ",PAGENO USING "<<<<"
          PRINT COLUMN 02,"__________________________________________________",
                          "____________________________________"

          SELECT codigo_afore,razon_social
             INTO campo.afore_cod,campo.raz_social
          FROM tab_afore_local
          SKIP 1 LINE
          
        PRINT  COLUMN 02,"ADMINISTRADORA: ","     ",campo.afore_cod
                     USING    "###","   ",campo.raz_social

	PRINT COLUMN 2,"FOLIO         :",g_folio,"   ",g_tipo_desc2


        SKIP 1 LINE
        PRINT COLUMN 02,"__________________________________________________",
                        "______________________________________"
               
        SKIP 1 LINE

        PRINT COLUMN 02,"SIE",
              COLUMN 8,"SCTA",
              COLUMN 33,"ACCIONES/PAVIS",
              COLUMN 73,"MONTO EN PESOS"

        PRINT COLUMN 02,"--------------------------------------------------",
                        "--------------------------------------"

        BEFORE GROUP OF s.siefore

           PRINT COLUMN  2,"TOTALES POR SUBCUENTA "

    ON EVERY ROW
        LET    g_siefore    =  s.siefore 
        PRINT COLUMN  3,g_siefore          USING "##"  ,
              COLUMN 10,s.subcuenta        USING "##" ,
              COLUMN 25,s.acciones         USING "---,---,---,--&.######",
              COLUMN 65,s.pesos            USING "---,---,---,--&.######"

        AFTER GROUP  OF s.siefore

           LET    g_siefore    =  s.siefore 
        PRINT COLUMN 02,"--------------------------------------------------",
                        "--------------------------------------"

      PRINT COLUMN  2,"TOTAL SIEFORE  ",s.siefore CLIPPED,
            COLUMN  25,GROUP SUM(s.acciones) USING "---,---,---,--&.######",
            COLUMN  65,GROUP SUM(s.pesos)    USING "---,---,---,--&.######"
         SKIP 1 LINES
        PRINT COLUMN  08,"SIEFORE=> ",
              COLUMN  21, g_siefore      USING   "##","  ",
                         g_val_accion  [g_siefore].nom_siefore,"        ",
                          "VALOR ACCION: ",
                         g_val_accion  [g_siefore].precio_accion
                              USING  "###.######"

         PRINT COLUMN 02,"--------------------------------------------------",
                        "--------------------------------------"

         ON LAST ROW
           
            SKIP 1 LINE

            PRINT COLUMN  2,"TOTAL CUENTAS","==>",g_total_cuentas USING "####,##&"," ","REGS."


        PRINT COLUMN 02,"__________________________________________________",
                        "______________________________________"
            SKIP 1 LINE
END REPORT
