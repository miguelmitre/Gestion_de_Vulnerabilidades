###############################################################################
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => AFIL046                                                   --
-- Descripcion  => REPORTE DE CIFRAS TOTALES DE CAPTURA AL DIA               --
-- Sistema      => AFILIACION                                                --
-- Autor        => VERONICA LOPEZ                                            --
-- Fecha        => 03 Febrero 2004.                                          --
###############################################################################
DATABASE safre_af

GLOBALS

    DEFINE g_reg RECORD
           frecafor  DATE,
           tipo_solicitud  SMALLINT,
           desc_solicitud  CHAR(25),
           status_interno  SMALLINT,
           estado_desc     CHAR(25),
           total           INTEGER
           END RECORD

    DEFINE g_param  RECORD LIKE seg_modulo.*

    DEFINE hoy      DATE,
           num      INTEGER,
           opc      CHAR(1),
           enter    CHAR(1),
           hora1    CHAR(4),
           usuario  CHAR(8),
           hora     CHAR(8),
           g_eco    CHAR(100),
           nom_arch CHAR(100)

    DEFINE cla_where  CHAR(300),
           cla_sel    CHAR(900)

    DEFINE salida    CHAR(300),
           vimprime  CHAR(500)

    DEFINE arr1	 ARRAY[1000] OF RECORD
           numero  INTEGER,
           frecafor  DATE,
           tipo_solicitud  SMALLINT,
           desc_solicitud  CHAR(25),
           status_interno  SMALLINT,
           estado_desc     CHAR(25),
           total  INTEGER
           END RECORD

    DEFINE i  INTEGER
   
    DEFINE pantalla  SMALLINT

END GLOBALS


#########################################################################
MAIN

    OPTIONS                 
       PROMPT LINE LAST,    
       INPUT WRAP,          
       ACCEPT KEY CONTROL-I,
       COMMENT LINE LAST    
       DEFER INTERRUPT         

    CALL STARTLOG("AFIL046.log")
    CALL Inicializa()
    CALL proceso() #P

    #CALL Genera_consulta()

END MAIN
#########################################################################


#########################################################################
FUNCTION proceso()
#p----------------

    OPEN WINDOW ventana1 AT 2,2 WITH FORM "AFIL0461" ATTRIBUTE(BORDER)

    DISPLAY " AFIL046          REPORTE CAPTURA SOLICITUDES POR FECHA                        " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY hoy USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    MENU "REP CIFRAS TOTALES"
      COMMAND "Consulta" "Consulta por pantalla cifras totales"
          LET pantalla = 1
          CALL Genera_consulta() #Gc
          CLEAR SCREEN
      COMMAND "Reporte" "Reporte cifras totales de captura al dia"
          LET pantalla = 0
          CALL Genera_consulta() #Gc
          CLEAR SCREEN
      COMMAND "Salida" "Salir del Programa"
          EXIT MENU
    END MENU

END FUNCTION
#########################################################################


#########################################################################
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
                 ".CIFRAS_TOT.", hoy USING "DDMMYY","_", hora1

    LET nom_arch = usuario CLIPPED,
                   ".CIFRAS_TOT.", hoy USING "DDMMYY","_", hora1

    LET i        = 1
    
    LET pantalla = 0

END FUNCTION
#######################################################################


#######################################################################
FUNCTION Genera_consulta()

    LET INT_FLAG = FALSE

    CONSTRUCT cla_where ON a.frecafor, a.tipo_solicitud 
                        FROM frecafor, tipo_solicitud
 
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

    LET cla_sel = " SELECT a.frecafor, ",
                         " a.tipo_solicitud, ",
                         " b.desc_solicitud, ",
                         " a.status_interno, ",
                         " c.estado_desc,    ",
                         " count(*)          ",
                  " FROM   afi_solicitud a, tab_tipo_solic b, tab_status_afi c ",
                  " WHERE ",cla_where CLIPPED," ",
                  " AND    a.tipo_solicitud = b.tipo_solicitud ",
                  " AND    a.status_interno = c.estado_cod ",
                  " GROUP  BY 1,2,3,4,5 ",
                  " ORDER  BY a.tipo_solicitud, a.status_interno, a.frecafor "
 
 
    IF pantalla = 0 THEN
         START REPORT rep_solicitud TO salida
    END IF

    PREPARE claexe FROM cla_sel
    DECLARE cur_solicitud CURSOR FOR claexe

    FOREACH cur_solicitud INTO g_reg.frecafor,
                               g_reg.tipo_solicitud,
                               g_reg.desc_solicitud,
                               g_reg.status_interno,
                               g_reg.estado_desc,
                               g_reg.total

          IF pantalla = 0 THEN
                OUTPUT TO REPORT rep_solicitud(g_reg.*)
          END IF
 
          LET arr1[i].numero  = i
          LET arr1[i].frecafor  = g_reg.frecafor
          LET arr1[i].tipo_solicitud  = g_reg.tipo_solicitud
          LET arr1[i].desc_solicitud  = g_reg.desc_solicitud
          LET arr1[i].status_interno  = g_reg.status_interno
          LET arr1[i].estado_desc  = g_reg.estado_desc
          LET arr1[i].total  = g_reg.total      

          LET i = i + 1

    END FOREACH

    IF pantalla = 0 THEN
         FINISH REPORT rep_solicitud

         LET g_eco = "echo ", nom_arch CLIPPED, " > ",
                      g_param.ruta_listados CLIPPED,"/rescate.CIFRAS_TOT.",
                      hoy USING "DDMMYY","_",hora1

         RUN g_eco

         LET vimprime = "lp ",salida
         RUN vimprime
    ELSE
         OPEN WINDOW ventana2 AT 2,2 WITH FORM "AFIL0462" ATTRIBUTE(BORDER)

         DISPLAY "                         < Ctrl - P > Imprimir                                 " AT 3,1 ATTRIBUTE(BOLD)

         DISPLAY " AFIL046       REPORTE DE CAPTURA SOLICITUDES POR FECHA                        " AT 4,1 ATTRIBUTE(REVERSE)

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
    CLOSE WINDOW ventana2
    END IF

    MESSAGE "REPORTE FINALIZADO..." 

END FUNCTION
#########################################################################


#########################################################################
FUNCTION impresion()

    DEFINE i SMALLINT

    LET i = 1

    START REPORT rep_solicitud TO salida

    PREPARE claexr FROM cla_sel
    DECLARE cur_solic_rpt CURSOR FOR claexr

    FOREACH cur_solic_rpt  INTO  g_reg.frecafor,
                                 g_reg.tipo_solicitud,
                                 g_reg.desc_solicitud,
                                 g_reg.status_interno,
                                 g_reg.estado_desc,
                                 g_reg.total

    OUTPUT TO REPORT rep_solicitud(g_reg.*)

      LET arr1[i].numero = i
      LET arr1[i].frecafor = g_reg.frecafor
      LET arr1[i].tipo_solicitud = g_reg.tipo_solicitud
      LET arr1[i].desc_solicitud = g_reg.desc_solicitud
      LET arr1[i].status_interno = g_reg.status_interno
      LET arr1[i].estado_desc = g_reg.estado_desc
      LET arr1[i].total = g_reg.total

      LET i = i + 1 

    END FOREACH
         
    FINISH REPORT rep_solicitud

    LET g_eco = "echo ", nom_arch CLIPPED, " > ",
                      g_param.ruta_listados CLIPPED,"/rescate.CIFRAS_TOT.",
                      hoy USING "DDMMYY","_",hora1

    RUN g_eco
END FUNCTION
############################################################################


############################################################################
REPORT rep_solicitud(g_reg)

    DEFINE g_reg RECORD
           frecafor  DATE,
           tipo_solicitud  SMALLINT,
           desc_solicitud  CHAR(25),
           status_interno  SMALLINT,
           estado_desc  CHAR(25),
           total  INTEGER
           END RECORD

    DEFINE  L1   CHAR(01)
   
    DEFINE  L5   CHAR(05)
   
    DEFINE  L10  CHAR(10)

    DEFINE cod_afore   SMALLINT
    
    DEFINE descripcion CHAR(30)


    OUTPUT
        PAGE   LENGTH 90
        TOP    MARGIN 0
        BOTTOM MARGIN 0
        RIGHT  MARGIN 150
        LEFT   MARGIN 0


    FORMAT
        PAGE HEADER

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'

        PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B'


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
    
    SKIP 3 LINES

    PRINT COLUMN 33, "R E P O R T E     C I F R A S     T O T A L E S    D E    C A P T U R A     A L     D I A"

    SKIP 2 LINES   

    PRINT COLUMN 40, "        A F I L I A C I O N      "

    SKIP 5 LINES

    PRINT 
    PRINT COLUMN 9, "PROGRAMA : AFIL046"

    PRINT
    PRINT COLUMN 9, "PAGINA   : ",PAGENO USING"####"

    #PRINT 
    #PRINT COLUMN 115, "FECHA    : ",TODAY USING"DD/MM/YYYY"

   PRINT '\033e\033(s218T\033(s14H\033(s7B'
 
   SKIP 1 LINES

   PRINT COLUMN 10,"\332",L10,L1,L1,L1,
                    "\302",L10,L1,L1,L1,L1,
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
                    "              |",
                    "                                |",
                    "    NUMERO    |"

    PRINT COLUMN 10,"|   NUMERO    |",
                    " F. RECEPCION |",
                    " T. SOLICITUD |",
                    " DESC. SOLIC. |",
                    "    STATUS    |",
                    "    DESCRIPCION  STATUS         |",
                    "      DE      |"

    PRINT COLUMN 10,"|             |",
                    "              |",
                    "              |",
                    "              |",
                    "              |",
                    "                                |",
                    "   REGISTROS  |"


    PRINT COLUMN 10,"\300",L10,L1,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\301",L10,L10,L10,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\331"

    ON EVERY ROW
         
       PRINT
       PRINT COLUMN 013, num USING "###",
             COLUMN 026, g_reg.frecafor,
             COLUMN 046, g_reg.tipo_solicitud    USING "#",
             COLUMN 058, g_reg.desc_solicitud    CLIPPED,
             COLUMN 075, g_reg.status_interno    USING "###",
             COLUMN 087, g_reg.estado_desc       CLIPPED,
             COLUMN 118, g_reg.total             USING "#######&"

             LET num = num +1

    ON LAST ROW

    SKIP 3 LINES


    PRINT COLUMN 10,"\332",L10,L1,L1,L1,
                    "\302",L10,L1,L1,L1,L1,
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
                    "              |",
                    "       TOTALES:                 |",
           COLUMN 123,SUM(g_reg.total) USING "####",
                    "     |"

    PRINT COLUMN 10,"\300",L10,L1,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\301",L10,L10,L10,L1,L1,
                    "\301",L10,L1,L1,L1,L1,
                    "\331"
END REPORT
#########################################################################
