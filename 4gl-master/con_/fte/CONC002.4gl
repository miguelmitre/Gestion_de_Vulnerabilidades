####################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                #
#Owner             => E.F.P.                                       #
#Programa CONC002  => Consulta de Transacciones de Contabilidad    #
#Fecha             => 23/10/2001  .                                #
#Por               => Laura Eugenia Cortes Guzman                  #
#Sistema           => CON.                                         #
####################################################################
DATABASE safre_af
GLOBALS
    --DEFINE g_param_dis    RECORD LIKE glo_parametro.*
    DEFINE g_param_lst    RECORD LIKE seg_modulo.*   ---erm 23 Marzo 2006

    DEFINE enter               CHAR(1)

    DEFINE hoy                 DATE,
           sel_where           CHAR(1000),
           cla_where           CHAR(300),
           g_impre             CHAR(300),
           g_lista             CHAR(300),
           seg_usuario         CHAR(08),
           pos                 SMALLINT

    DEFINE g_reg, g_reg_1      RECORD
           folio               INTEGER,
           proceso_cod         CHAR(05),
           proceso_desc        CHAR(50),
           transaccion_cod     INTEGER,
           transac_desc        CHAR(50),
           siefore             SMALLINT,
           desc_siefore        CHAR(7),
           fecha_emision       DATE,
           fecha_valor         DATE,
           identificador       SMALLINT,
           identi_desc         CHAR(08),
           importe             DECIMAL(15,2),
           estado              SMALLINT,
           desc_estado         CHAR(25),
           subcuenta           CHAR(2),
           subcuenta1          CHAR(2),
           subcuenta2          CHAR(3),
           importe_pesos       DECIMAL(15,2),
           importe_acc         DECIMAL(15,2)
    END RECORD

    DEFINE l_record1  ARRAY[1000] OF RECORD
           folio               INTEGER,
           proceso_cod         CHAR(05),
           proceso_desc        CHAR(50),
           transaccion_cod     INTEGER,
           transac_desc        CHAR(50),
           siefore             SMALLINT,
           desc_siefore        CHAR(7),
           fecha_emision       DATE,
           fecha_valor         DATE,
           identificador       SMALLINT,
           identi_desc         CHAR(08),
           importe             DECIMAL(15,2),
           estado              SMALLINT,
           desc_estado         CHAR(25),
           subcuenta           CHAR(2),
           subcuenta1          CHAR(2),
           subcuenta2          CHAR(3)
    END RECORD

    DEFINE l_record2  ARRAY[1000] OF RECORD
           folio               INTEGER,
           proceso_cod         CHAR(05),
           proceso_desc        CHAR(50),
           transaccion_cod     INTEGER,
           transac_desc        CHAR(50),
           siefore             SMALLINT,
           desc_siefore        CHAR(7),
           fecha_emision       DATE,
           fecha_valor         DATE,
           identi_desc         CHAR(08),
           importe             DECIMAL(15,2),
           estado              SMALLINT,
           desc_estado         CHAR(25),
           subcuenta           CHAR(9)
    END RECORD
    DEFINE var_proceso         CHAR(5)
END GLOBALS
#####################################################################
MAIN
   OPTIONS PROMPT LINE LAST,
           INPUT WRAP,
           ACCEPT KEY CONTROL-O

   ##DEFER INTERRUPT

   CALL inicio()
   LET hoy = TODAY
   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 3,3 WITH FORM "CONC0021" ATTRIBUTE( BORDER)
      DISPLAY " CONC002                 REGISTRO HISTORICO CONTABLE                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY hoy USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

      DISPLAY " (Esc) Consulta              (Ctrl-p) Impresion             (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where   ON a.folio,
                               a.proceso_cod,
                               fecha_emision
                          FROM folio,
                               proceso_cod,
                               fecha_emision

      AFTER FIELD proceso_cod 
         LET  var_proceso = get_fldbuf(proceso_cod)

                ON KEY (esc)
                   ERROR "PROCESANDO INFORMACION..."
                   SLEEP 2
                   ERROR ""
                   LET int_flag = FALSE
                   EXIT CONSTRUCT

                ON KEY (CONTROL-C)
                   LET int_flag = TRUE
                   EXIT CONSTRUCT
      END CONSTRUCT
      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         --EXIT PROGRAM
      END IF

      IF (var_proceso = '00060' OR
         var_proceso = '00061' OR
         var_proceso = '00062') THEN
         LET sel_where = "SELECT  a.folio,           ",
                                " a.proceso_cod,     ",
                                " b.descripcion,     ",
                                " a.transaccion_cod, ",
                                " c.descripcion_1,   ",
                                " a.siefore,         ",
                                " d.razon_social,    ",
                                " a.fecha_emision,   ",
                                " a.fecha_valor,     ",
                                " a.identificador,   ",
                                " a.importe,         ",
                                " a.estado,          ",
                                " '',                ",
                                " c.subcuenta,       ",
                                " c.subcuenta1,      ",
                                " c.subcuenta2       ",
                         " FROM  con_transaccion a,  ",
                               " tab_proceso b,      ",
                               " tab_transaccion c,  ",
                               " tab_siefore_local d ",
                         " WHERE ",cla_where CLIPPED,
                         " AND   a.proceso_cod     = b.proceso_cod     ",
                         " AND   a.proceso_cod     = c.proceso_cod     ",
                         " AND   a.siefore         = d.codigo_siefore  ",
                         " AND   a.transaccion_cod = c.transaccion_cod ",
                         " AND   a.transaccion_cod  NOT IN (29400,29401,29402,29403,29404,29405,29406)",
                         " ORDER BY 1,2 "
      ELSE
         LET sel_where = "SELECT  a.folio,           ",
                                " a.proceso_cod,     ",
                                " b.descripcion,     ",
                                " a.transaccion_cod, ",
                                " c.descripcion_1,   ",
                                " a.siefore,         ",
                                " d.razon_social,    ",
                                " a.fecha_emision,   ",
                                " a.fecha_valor,     ",
                                " a.identificador,   ",
                                " a.importe,         ",
                                " a.estado,          ",
                                " '',                ",
                                " c.subcuenta,       ",
                                " c.subcuenta1,      ",
                                " c.subcuenta2       ",
                         " FROM  con_transaccion a,  ",
                               " tab_proceso b,      ",
                               " tab_transaccion c,  ",
                               " tab_siefore_local d ",
                         " WHERE ",cla_where CLIPPED,
                         " AND   a.proceso_cod     = b.proceso_cod     ",
                         " AND   a.proceso_cod     = c.proceso_cod     ",
                         " AND   a.siefore         = d.codigo_siefore  ",
                         " AND   a.transaccion_cod = c.transaccion_cod ",
                         " AND   a.transaccion_cod NOT IN (SELECT  transaccion_cod ",  
                                                          " FROM    tab_transaccion ",  
                                                          " WHERE   proceso_cod IN ",   
                                                          " ('00060','00061','00062') ",
                                                          " AND transaccion_cod IN ",   
                                                          " (29400,29401,29402,29403, ",
                                                          " 29404,29405,29406))",       

                         " ORDER BY 1,2 "
      END IF
      PREPARE query FROM sel_where
         
      DECLARE cursor_1 CURSOR FOR query
         
      LET pos = 1
      FOREACH cursor_1 INTO l_record1[pos].folio            ,
                            l_record1[pos].proceso_cod      ,
                            l_record1[pos].proceso_desc     ,
                            l_record1[pos].transaccion_cod  ,
                            l_record1[pos].transac_desc     ,
                            l_record1[pos].siefore          ,
                            l_record1[pos].desc_siefore     ,
                            l_record1[pos].fecha_emision    ,
                            l_record1[pos].fecha_valor      ,
                            l_record1[pos].identificador    ,
                            l_record1[pos].importe          ,
                            l_record1[pos].estado           ,
                            l_record1[pos].desc_estado      ,
                            l_record1[pos].subcuenta        ,
                            l_record1[pos].subcuenta1       ,
                            l_record1[pos].subcuenta2
         
         CASE l_record1[pos].identificador
              WHEN 1  LET l_record1[pos].identi_desc = "PESOS   "
              WHEN 2  LET l_record1[pos].identi_desc = "ACCIONES"
         END CASE

         SELECT descripcion
         INTO   l_record1[pos].desc_estado
         FROM   con_status
         WHERE  estado = l_record1[pos].estado

         LET l_record2[pos].folio           = l_record1[pos].folio
         LET l_record2[pos].proceso_cod     = l_record1[pos].proceso_cod
         LET l_record2[pos].proceso_desc    = l_record1[pos].proceso_desc
         LET l_record2[pos].transaccion_cod = l_record1[pos].transaccion_cod
         LET l_record2[pos].transac_desc    = l_record1[pos].transac_desc
         LET l_record2[pos].siefore         = l_record1[pos].siefore
         LET l_record2[pos].desc_siefore    = l_record1[pos].desc_siefore
         LET l_record2[pos].fecha_emision   = l_record1[pos].fecha_emision
         LET l_record2[pos].fecha_valor     = l_record1[pos].fecha_valor
         LET l_record2[pos].identi_desc     = l_record1[pos].identi_desc
         LET l_record2[pos].importe         = l_record1[pos].importe
         LET l_record2[pos].estado          = l_record1[pos].estado
         #LET l_record2[pos].desc_estado     = l_record1[pos].desc_estado
         LET l_record2[pos].subcuenta       = l_record1[pos].subcuenta,",",
                                              l_record1[pos].subcuenta1,",",
                                              l_record1[pos].subcuenta2
         CASE l_record2[pos].estado
              WHEN 10  LET l_record2[pos].desc_estado = "REGISTRADO"
              WHEN 20  LET l_record2[pos].desc_estado = "CONCILIADO"
              WHEN 40  LET l_record2[pos].desc_estado = "CONTABILIZADO"
         END CASE

         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record1[pos].*, l_record1[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record2 TO scr_1.*
            ON KEY (CONTROL-P)
               ERROR " PROCESANDO INFORMACION ..."
               CALL impresion(pos)

            ON KEY (CONTROL-C)
               EXIT DISPLAY

            ON KEY (INTERRUPT)
               EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE TRANSACCIONES.... VACIO"
         SLEEP 2
         ERROR ""
      END IF
   END IF
   CLEAR SCREEN
END MAIN

#####################################################################
FUNCTION inicio()
   INITIALIZE g_reg.*, g_reg_1.* TO NULL

{   SELECT ruta_spool,USER
          INTO   g_param_dis.ruta_spool, seg_usuario
          FROM   glo_parametro
}
          SELECT ruta_listados,USER
          INTO   g_param_lst.ruta_listados, seg_usuario
          FROM   seg_modulo
          WHERE modulo_cod = 'con'

END FUNCTION
################################################################################

FUNCTION impresion(pos)

   DEFINE i,pos        SMALLINT,
          total_pesos  DECIMAL(17,2),
          total_accion DECIMAL(17,2)

   LET total_pesos  = 0
   LET total_accion = 0

   LET g_impre = g_param_lst.ruta_listados CLIPPED,"/",seg_usuario CLIPPED,
                 ".CON_TRAN",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_conTransaccion TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.folio           = l_record1[i].folio
       LET g_reg.proceso_cod     = l_record1[i].proceso_cod
       LET g_reg.proceso_desc    = l_record1[i].proceso_desc
       LET g_reg.siefore         = l_record1[i].siefore
       LET g_reg.desc_siefore    = l_record1[i].desc_siefore
       LET g_reg.fecha_emision   = l_record1[i].fecha_emision
       LET g_reg.fecha_valor     = l_record1[i].fecha_valor
       LET g_reg.identificador   = l_record1[i].identificador
       LET g_reg.identi_desc     = l_record1[i].identi_desc
       LET g_reg.transaccion_cod = l_record1[i].transaccion_cod
       LET g_reg.importe         = l_record1[i].importe
       LET g_reg.transac_desc    = l_record1[i].transac_desc
       LET g_reg.estado          = l_record1[i].estado
       LET g_reg.transac_desc    = l_record1[i].desc_estado
       LET g_reg.subcuenta       = l_record1[i].subcuenta
       LET g_reg.subcuenta1      = l_record1[i].subcuenta1
       LET g_reg.subcuenta2      = l_record1[i].subcuenta2

       --LET total = total + g_reg.importe
       IF g_reg.identificador = 1 THEN
          LET total_pesos  = total_pesos + g_reg.importe
       ELSE 
          LET total_accion = total_accion + g_reg.importe
       END IF 

       IF g_reg.folio IS NULL THEN
          EXIT FOR
       END IF

       OUTPUT TO REPORT rpt_conTransaccion(g_reg.*,total_pesos,total_accion)
   END FOR

   FINISH REPORT rpt_conTransaccion

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre

   RUN g_lista

END FUNCTION
#####################################################################
REPORT rpt_conTransaccion(g_reg1,total_pesos,total_accion)

    DEFINE g_reg1             RECORD
           folio              INTEGER,
           proceso_cod        CHAR(05),
           proceso_desc       CHAR(40),
           transaccion_cod    INTEGER,
           transac_desc       CHAR(50),
           siefore            SMALLINT,
           desc_siefore       CHAR(7),
           fecha_emision      DATE,
           fecha_valor        DATE,
           identificador      SMALLINT,
           identi_desc        CHAR(08),
           importe            DECIMAL(15,2),
           estado             SMALLINT,
           desc_estado        CHAR(25),
           subcuenta          CHAR(2),
           subcuenta1          CHAR(2),
           subcuenta2          CHAR(3),
           total_pesos        DECIMAL(17,2),
           total_accion       DECIMAL(17,2)
    END RECORD,

    total_pesos               DECIMAL(17,2),
    total_accion              DECIMAL(17,2),
    L1                        CHAR(01),
    L2                        CHAR(02),
    L3                        CHAR(03),
    L4                        CHAR(04),
    L5                        CHAR(05),
    L6                        CHAR(06),
    L7                        CHAR(07),
    L8                        CHAR(08),
    L9                        CHAR(09),
    L10                       CHAR(10),
    identi_desc               CHAR(08)

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  90

   ORDER BY g_reg1.proceso_cod

   FORMAT
      PAGE HEADER

         LET L1  = "\304"
         LET L2  = "\304\304"
         LET L3  = "\304\304\304"
         LET L4  = "\304\304\304\304"
         LET L5  = "\304\304\304\304\304"
         LET L6  = "\304\304\304\304\304\304"
         LET L7  = "\304\304\304\304\304\304\304"
         LET L8  = "\304\304\304\304\304\304\304\304"
         LET L9  = "\304\304\304\304\304\304\304\304\304"
         LET L10 = "\304\304\304\304\304\304\304\304\304\304"

         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s9H'

         PRINT COLUMN 02,"CONC002 ",
               COLUMN 23,"         REGISTRO HISTORICO CONTABLE ",
               COLUMN 79,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT '\033e\033(s218T\033(s18H\033(s12B'

         PRINT

               COLUMN 01,"\332",L8,L2,
                         "\302",L10,L10,L10,L10,L6,
                         "\302",L10,L5,
                         "\302",L10,L6,
                         "\302",L10,L4,
                         "\302",L10,L3,
                         "\302",L10,L10,L10,L1,
                         "\302",L10,L3,L5,
                         "\277"


         PRINT COLUMN 01,"|",
               COLUMN 02,"FOLIO ",
               COLUMN 12,"|",
               COLUMN 13,"PROCESO ",
               COLUMN 59,"|",
               COLUMN 60,"SIEFORE ",
               COLUMN 75,"|",
               COLUMN 77,"TRANSACCION ",
               COLUMN 92,"|",
               COLUMN 93,"FECHA EMISION",
               COLUMN 107,"|",
               COLUMN 108,"FECHA VALOR",
               COLUMN 121,"|",
               COLUMN 122,"IDENTIDAD",
               COLUMN 132,"|",
               COLUMN 133,"SUBCUENTAS",
               COLUMN 153,"|",
               COLUMN 155,"I M P O R T E",
               COLUMN 172,"|"


         PRINT
               COLUMN 01,"\300",L8,L2,
                         "\301",L10,L10,L10,L10,L6,
                         "\301",L10,L5,
                         "\301",L10,L6,
                         "\301",L10,L4,
                         "\301",L10,L3,
                         "\301",L10,L10,L10,L1,
                         "\301",L10,L3,L5,
                         "\331"

      ON EVERY ROW
         SKIP 1 LINE

         CASE g_reg1.identificador
              WHEN 1 LET identi_desc = "PESOS   "
              WHEN 2 LET identi_desc = "ACCIONES"
         END CASE

         PRINT COLUMN  02,g_reg1.folio USING "######&&",
               COLUMN  13,g_reg1.proceso_cod,
               COLUMN  19,g_reg1.proceso_desc,
               COLUMN  60,g_reg1.siefore,
               COLUMN  69,g_reg1.desc_siefore,
               COLUMN  74,g_reg1.transaccion_cod,
               COLUMN  93,g_reg1.fecha_emision  USING "dd/mm/yyyy",
               COLUMN 108,g_reg1.fecha_valor    USING "dd/mm/yyyy",
               COLUMN 122,identi_desc,
               COLUMN 133,g_reg1.subcuenta CLIPPED,",",
                          g_reg1.subcuenta1 CLIPPED,",",
                          g_reg1.subcuenta2 CLIPPED,
               COLUMN 154,g_reg1.importe USING "###########&.&&"

      AFTER GROUP OF g_reg1.proceso_cod
        PRINT COLUMN 01,"-------------------------------------------------------",
                        "-------------------------------------------------------",
                        "-------------------------------------------------------"

        PRINT COLUMN 120,"TOTAL PESOS    :",
              COLUMN 154, GROUP SUM(g_reg1.importe) WHERE g_reg1.identificador = 1 USING "###########&.##"
        PRINT COLUMN 120,"TOTAL ACCIONES :",
              COLUMN 154, GROUP SUM(g_reg1.importe) WHERE g_reg1.identificador = 2 USING "###########&.##"

        SKIP 2 LINE

      PAGE TRAILER
         SKIP 1 LINE
         PRINT COLUMN 161," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
         PRINT
               COLUMN 01,"\332",L8,L3,
                                L10,L10,L10,L10,L6,
                                L10,L10,
                                L10,L3,
                                L10,L2,
                                L10,L10,L10,L10,L9,
                         "\302",L10,L7,
                         "\277"


         PRINT COLUMN 01,"|",
               COLUMN 02,"  Total de registros : ",COUNT(*) USING "<<<<",
               COLUMN 171,"|"
        PRINT COLUMN 01,"|",
               COLUMN 120,"  Total Pesos    : ",
               COLUMN 153,"|",
               total_pesos USING "###########&.&&",
               COLUMN 171,"|"
        PRINT COLUMN 01,"|",
               COLUMN 120,"  Total Acciones : ",
               COLUMN 153,"|",
               total_accion USING "###########&.&&",
               COLUMN 171,"|"

         PRINT
               COLUMN 01,"\300",L8,L3,
                                L10,L10,L10,L10,L6,
                                L10,L10,
                                L10,L3,
                                L10,L2,
                                L10,L10,L10,L10,L9,
                         "\301",L10,L7,
                         "\331"


END REPORT
