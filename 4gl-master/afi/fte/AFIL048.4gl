###############################################################################
-- Programa     => AFIL048                                                   --
-- Descripcion  => REPORTE CIFRAS TOTALES DE CAPTURA AL DIA                  --
-- Sistema      => AFI                                                       --
-- Por          => VERONICA LOPEZ                                            --
-- Fecha        => 06 Febrero 2004                                           --
-- Modificado   =>                                                           --
-- Fecha        =>                                                           --
###############################################################################
DATABASE safre_af

GLOBALS

    DEFINE g_reg RECORD
           Tipo_solicitud  SMALLINT,
           desc_solicitud  CHAR(25),
           status_interno  SMALLINT,
           estado_desc  CHAR(25),
           total  INTEGER
           END RECORD

    DEFINE g_param   RECORD LIKE seg_modulo.*

    DEFINE hoy       DATE,
           num       INTEGER,
           opc       CHAR(1),
           enter     CHAR(1),
           hora1     CHAR(4),
           usuario   CHAR(8),
           hora      CHAR(8),
           g_eco     CHAR(100),
           nom_arch  CHAR(100)
 
    DEFINE cla_where  CHAR(300),
           cla_sel    CHAR(900)
 
    DEFINE salida   CHAR(300),
           vimprime  CHAR(500)

    DEFINE arr1    ARRAY[1000] OF RECORD
           numero  INTEGER,
           tipo_solicitud  SMALLINT,
           desc_solicitud  CHAR(25),
           status_interno  SMALLINT,
           estado_desc     CHAR(25),
           total           INTEGER
           END RECORD

    DEFINE  i  INTEGER
   
    DEFINE  pantalla  SMALLINT

    DEFINE tot_total SMALLINT
        
    DEFINE vstatus_interno  INTEGER

END GLOBALS

#########################################################################
MAIN
    OPTIONS
       PROMPT LINE LAST,
       INPUT WRAP,
       ACCEPT KEY CONTROL-I,
       COMMENT LINE LAST
       DEFER INTERRUPT

    CALL STARTLOG("AFIL048.log")
    CALL Inicializa()
    CALL proceso() #P

END MAIN
#########################################################################

FUNCTION proceso()
#p----------------

    OPEN WINDOW ventana1 AT 2,2 WITH FORM "AFIL0481" ATTRIBUTE(BORDER)

    DISPLAY " AFIL048           REPORTE TOTAL AFILIADOS POR STATUS                          " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY hoy USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    MENU "REP CIFRAS TOTALES AFI"
      COMMAND "Consulta" "Consulta por pantalla cifras totales de afiliados"
          LET pantalla = 1
          CALL Genera_consulta() #Gc
          CLEAR SCREEN
          CLEAR SCREEN
      COMMAND "Reporte" "Reporte cifras totales de afiliados al dia"
          LET pantalla = 0
          CALL Genera_consulta() #Gc
          CLEAR SCREEN
      COMMAND "Salida" "Salir del Programa"
          EXIT MENU
    END MENU

END FUNCTION
############################################################################

FUNCTION Inicializa()

    SELECT *,
          USER
    INTO   g_param.*,
           usuario
    FROM   seg_modulo
    WHERE  modulo_cod = "afi"
   
    LET hoy = TODAY

    LET hora = TIME

    LET hora1 = hora[1,2],hora[4,5]

    LET num = 1

    LET salida = g_param.ruta_listados CLIPPED,"/",usuario CLIPPED,
                ".CIFRAS_TOT_AFI.", hoy USING "DDMMYY","_", hora1

    LET nom_arch = usuario CLIPPED,
                  ".CIFRAS_TOT_AFI.", hoy USING "DDMMYY","_", hora1

    LET i        = 1
   
    LET pantalla = 0

END FUNCTION
########################################################################

FUNCTION Genera_consulta()
    
    SELECT COUNT(*)
    INTO g_reg.total
    FROM afi_mae_afiliado
    WHERE tipo_solicitud = vstatus_interno

    LET INT_FLAG = FALSE


    CONSTRUCT cla_where ON a.tipo_solicitud
                        FROM tipo_solicitud 

       ON KEY (ESC)
          LET INT_FLAG = FALSE
          EXIT CONSTRUCT

       ON KEY (INTERRUPT)
          LET INT_FLAG = TRUE
         EXIT CONSTRUCT
  
    END CONSTRUCT

    IF INT_FLAG THEN
        LET INT_FLAG = FALSE
        MESSAGE "PROCESO CANCELADO..."
        CLEAR SCREEN
        #CLOSE WINDOW ventana1
        RETURN
    END IF

    MESSAGE "GENERANDO REPORTE..."

    LET cla_sel = " SELECT a.tipo_solicitud, ",
                         " b.desc_solicitud, ",
                         " a.status_interno, ",
                         " c.estado_desc,    ",
                         " count(*) ",
                  " FROM   afi_mae_afiliado a, tab_tipo_solic b, tab_status_afi c ",
                  " WHERE ",cla_where CLIPPED," ",
                  " AND    a.tipo_solicitud = b.tipo_solicitud ",
                  " AND    a.status_interno = c.estado_cod ",
                 # " AND    a.tipo_solicitud < 5 ",
                  " GROUP  BY 1,2,3,4 ",
                  " ORDER  BY a.tipo_solicitud, a.status_interno "


    IF pantalla = 0 THEN
        START REPORT rep_solicitud TO salida
    END IF

    PREPARE claexe FROM cla_sel
    DECLARE cur_solicitud CURSOR FOR claexe

    FOREACH cur_solicitud INTO g_reg.tipo_solicitud,
                               g_reg.desc_solicitud,
                               g_reg.status_interno,
                               g_reg.estado_desc,
                               g_reg.total
    
    LET tot_total = tot_total + g_reg.total
       
       IF pantalla = 0 THEN
           OUTPUT TO REPORT rep_solicitud(g_reg.*)
       END IF

       LET arr1[i].numero            = i
       LET arr1[i].tipo_solicitud    = g_reg.tipo_solicitud
       LET arr1[i].desc_solicitud    = g_reg.desc_solicitud
       LET arr1[i].status_interno    = g_reg.status_interno
       LET arr1[i].estado_desc       = g_reg.estado_desc
       LET arr1[i].total             = g_reg.total

       LET i                         = i + 1

    END FOREACH

    IF pantalla = 0 THEN
        FINISH REPORT rep_solicitud

        LET g_eco = "echo ", nom_arch CLIPPED, " > ",
                    g_param.ruta_listados CLIPPED,"/rescate.CIFRAS_TOT_AFI.",
                    hoy USING "DDMMYY","_",hora1

        RUN g_eco

        LET vimprime = "lp ",salida
  
        RUN vimprime
    ELSE
     
        OPEN WINDOW ventana2 AT 2,2 WITH FORM "AFIL0482" ATTRIBUTE(BORDER)

        DISPLAY "                            < Ctrl - P > Impresion                             " AT 3,1 ATTRIBUTE(BOLD)

        DISPLAY " AFIL048    REPORTE TOTAL AFILIADOS POR STATUS                              " AT 4,1 ATTRIBUTE(REVERSE)

        DISPLAY hoy USING "DD-MM-YYYY" AT 4,68 ATTRIBUTE(REVERSE)

    IF (i - 1) >= 1 THEN
         CALL SET_COUNT(i-1)
         DISPLAY ARRAY arr1 TO scr_1.*
   
         ON KEY (INTERRUPT)
            EXIT DISPLAY

         ON KEY (Control-p)
            CALL impresion()

         END DISPLAY
    ELSE

         MESSAGE "No existen registros. . ."

    END IF
   
    CLEAR SCREEN
    CLOSE WINDOW ventana2
    END IF
    
    MESSAGE "REPORTE FINALIZADO..."

END FUNCTION
##########################################################################

FUNCTION impresion()

    DEFINE i SMALLINT

    LET i = 1

    START REPORT rep_solicitud TO salida

    PREPARE claexr FROM cla_sel
    DECLARE cur_solic_rpt CURSOR FOR claexr

    FOREACH cur_solicitud INTO g_reg.tipo_solicitud,
                               g_reg.desc_solicitud,
                               g_reg.status_interno,
                               g_reg.estado_desc,
                               g_reg.total

    OUTPUT TO REPORT rep_solicitud(g_reg.*)

       LET arr1[i].numero = i
       LET arr1[i].tipo_solicitud = g_reg.tipo_solicitud
       LET arr1[i].desc_solicitud = g_reg.desc_solicitud
       LET arr1[i].status_interno = g_reg.status_interno
       LET arr1[i].estado_desc  = g_reg.estado_desc
       LET arr1[i].total  =  g_reg.total

       LET i = i + 1
    
    END FOREACH
        
    FINISH REPORT rep_solicitud

    LET g_eco = "echo ", nom_arch CLIPPED, " > ",
                g_param.ruta_listados CLIPPED,"/rescate.CIFRAS_TOT_AFI.",
                hoy USING "DDMMYY","_",hora1

    RUN g_eco
END FUNCTION
##########################################################################

REPORT rep_solicitud(g_reg)

    DEFINE g_reg RECORD
           tipo_solicitud  SMALLINT,
           desc_solicitud  CHAR(25),
           status_interno  SMALLINT,
           estado_desc     CHAR(25),
           total  INTEGER
           END RECORD

    DEFINE  L1   CHAR(01)

    DEFINE  L5   CHAR(05)

    DEFINE  L10  CHAR(10)

    DEFINE cod_afore   SMALLINT
   
    DEFINE descripcion CHAR(30)

    DEFINE codigo_estado SMALLINT
  
    DEFINE totales INTEGER

    DEFINE estado_total  INTEGER
    
    
    OUTPUT
       PAGE   LENGTH 90
       TOP    MARGIN 0
       BOTTOM MARGIN 0
       RIGHT  MARGIN 150
       LEFT   MARGIN 0
       ORDER BY g_reg.tipo_solicitud, g_reg.desc_solicitud, g_reg.status_interno, g_reg.estado_desc

    FORMAT
       PAGE HEADER
 
       PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'

       PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B'
    

    LET totales = 0

    LET L1  = "\304"
   
    LET L5  = "\304\304\304\304\304"
    
    LET L10 = "\304\304\304\304\304\304\304\304\304\304"


    SELECT a.codigo_afore,b.afore_desc
    INTO   cod_afore,descripcion
    FROM   tab_afore_local a,tab_afore b
    WHERE  a.codigo_afore = b.afore_cod

    PRINT
    PRINT COLUMN 1,'\033e\033(s218T\033(s11H\033(s7N'
    PRINT COLUMN 06,cod_afore,"     ",descripcion,
          COLUMN 92,"FECHA : ",TODAY USING"DD/MM/YYYY"

    SKIP 4 LINES

    PRINT COLUMN 12, "R E P O R T E     C I F R A S     T O T A L E S     D E     A F I L I A D O S    AL    DIA"

    SKIP 5 LINES

    PRINT
    PRINT COLUMN 9, "PROGRAMA : AFIL048"

    PRINT
    PRINT COLUMN 9, "PAGINA   : ",PAGENO USING"####"

    PRINT '\033e\033(s218T\033(s14H\033(s7B'

    SKIP 1 LINES

    PRINT COLUMN 10,"\332",L10,L1,L1,L1,
                    "\302",L10,L1,L1,L1,L1,
                    "\302",L10,L1,L1,L1,L1,
                    "\302",L10,L1,L1,L1,L1,
                    "\302",L10,L10,L10,L1,L1,
                    "\302",L10,L1,L1,L1,L1,
                    "\277"


    PRINT COLUMN 10,"|             |",
                    "              |",
                    "              |",
                    "              |",
                    "                                |",
                    "    NUMERO    |"

    PRINT COLUMN 10,"|             |",
                    " T. SOLICITUD |",
                    " DESC. SOLIC. |",
                    "    STATUS    |",
                    "    DESCRIPCION  STATUS         |",
                    "      DE      |"

    PRINT COLUMN 10,"|             |",
                    "              |",
                    "              |",
                    "              |",
                    "                                |",
                    "   REGISTROS  |"


    PRINT COLUMN 10,"\300",L10,L1,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\301",L10,L10,L10,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\331"

    ON EVERY ROW

    SKIP 1 LINES
    
    PRINT
    PRINT COLUMN 014, num USING "###",
          COLUMN 031, g_reg.tipo_solicitud    USING "#",
          COLUMN 042, g_reg.desc_solicitud    CLIPPED,
          COLUMN 059, g_reg.status_interno    USING "###",
          COLUMN 075, g_reg.estado_desc       CLIPPED,
          COLUMN 103, g_reg.total             USING "#######&"

    LET num = num +1

    PRINT COLUMN 10,"\304",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L5,L1,L1
          
    ON LAST ROW
 
    SKIP 3 LINES

    PRINT COLUMN 10,"\332",L10,L1,L1,L1,
                    "\302",L10,L1,L1,L1,L1,
                    "\302",L10,L1,L1,L1,L1,
                    "\302",L10,L1,L1,L1,L1,
                    "\302",L10,L10,L10,L1,L1,
                    "\302",L10,L1,L1,L1,L1,
                    "\277"


    PRINT COLUMN 10,"|             |",
                    "              |",
                    "              |",
                    "              |",
                    "       TOTALES:                 |",
           COLUMN 102,SUM(g_reg.total) USING "#########",
                    "     |"

    PRINT COLUMN 10,"\300",L10,L1,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\301",L10,L10,L10,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\331"

    SKIP 1 LINES
  

    DECLARE report CURSOR FOR

    SELECT status_interno,COUNT(*)
    FROM afi_mae_afiliado
    GROUP BY 1
    ORDER BY 1

    FOREACH report INTO codigo_estado,estado_total
    
         LET totales = totales + estado_total
    
         SELECT estado_desc
         INTO g_reg.estado_desc
         FROM tab_status_afi
         WHERE estado_cod = codigo_estado 
    
         PRINT
         PRINT COLUMN 059,codigo_estado,
               COLUMN 075,g_reg.estado_desc,
               COLUMN 104,estado_total
    SKIP 1 LINES
    
    END FOREACH

    SKIP 1 LINES

    PRINT COLUMN 10,"\332",L10,L1,L1,L1,
                    "\302",L10,L1,L1,L1,L1,
                    "\302",L10,L1,L1,L1,L1,
                    "\302",L10,L1,L1,L1,L1,
                    "\302",L10,L10,L10,L1,L1,
                    "\302",L10,L1,L1,L1,L1,
                    "\277"

    PRINT COLUMN 10,"|             |",
                    "              |",
                    "              |",
                    "              |",
                    "       TOTALES POR STATUS:      |",
           COLUMN 077,SUM(g_reg.total) USING "##########&",
                    "   |"

    PRINT COLUMN 10,"\300",L10,L1,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\301",L10,L10,L10,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\331"
END REPORT
#########################################################################
