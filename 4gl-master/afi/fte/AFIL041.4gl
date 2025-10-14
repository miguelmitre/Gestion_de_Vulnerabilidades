###############################################################################
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => AFIL041                                                   --
-- Descripcion  => REPORTE DE CIFRAS DE AFILIADOS                            --
-- Sistema      => AFI                                                       --
-- autor        => VERONICA LOPEZ                                            --
-- Fecha        => 03 Febrero 2004.                                          --
###############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_reg RECORD
           tipo_solicitud SMALLINT,
           desc_solicitud CHAR(25),
           total          INTEGER
           END RECORD

    DEFINE g_param RECORD LIKE seg_modulo.*

    DEFINE 
        hoy      DATE,
        num      INTEGER,
        opc      CHAR(1),
        enter    CHAR(1),
        hora1    CHAR(4),
        usuario  CHAR(8),
        hora     CHAR(8),
        g_eco    CHAR(100),
        nom_arch CHAR(100)

    DEFINE cla_where CHAR(300),
           cla_sel CHAR(900)

    DEFINE salida  CHAR(300),
          vimprime CHAR(500)

    DEFINE arr1 ARRAY[20] OF RECORD
           numero         INTEGER,
           tipo_solicitud SMALLINT,
           desc_solicitud CHAR(25),
           total          INTEGER
           END RECORD

    DEFINE i        INTEGER
    DEFINE pantalla SMALLINT
    DEFINE comm     CHAR(500)

END GLOBALS

MAIN

    DEFER INTERRUPT         
    OPTIONS                 
       PROMPT LINE LAST,    
       COMMENT LINE LAST    

    CALL STARTLOG("AFIL041.log")
    CALL inicio()
    CALL proceso() #P

END MAIN

FUNCTION proceso()
#p----------------

    OPEN WINDOW ventana1 AT 2,2 WITH FORM "AFIL0411" ATTRIBUTE(BORDER)

    DISPLAY " AFIL041                        TOTAL AFILIADOS                                " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY hoy USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    MENU "CIFRAS AFILIADOS"
       COMMAND "Consulta" "Consulta por pantalla cifras de afiliados"
          LET pantalla = 1
          CALL Genera_consulta() #Gc
          CLEAR SCREEN
       COMMAND "Reporte" "Reporte cifras de afiliados"
          LET pantalla = 0
          CALL Genera_consulta() #Gc
          CLEAR SCREEN
       COMMAND "Salida" "Salir del Programa"
          EXIT MENU
    END MENU

END FUNCTION

FUNCTION inicio()
#i---------------

    SELECT *,
           USER
    INTO   g_param.*,
           usuario
    FROM   seg_modulo
    WHERE  modulo_cod = "afi"

    LET hoy   = TODAY
    LET hora  = TIME
    LET hora1 = hora[1,2],hora[4,5]
    LET num   = 1

    LET salida = g_param.ruta_listados CLIPPED,"/",usuario CLIPPED,
                ".CIFRAS_GLOB.", hoy USING "DDMMYY","_", hora1

    LET nom_arch = usuario CLIPPED,
                  ".CIFRAS_GLOB.", hoy USING "DDMMYY","_", hora1

    LET i        = 1
    LET pantalla = 0

END FUNCTION

FUNCTION Genera_consulta()
#c------------------------

    DEFINE imprime CHAR(300)

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
         RETURN
    END IF

    MESSAGE "GENERANDO REPORTE..."

    CREATE TEMP TABLE nss_ced_glob
    (nss CHAR(11))

    INSERT INTO nss_ced_glob
    SELECT t.n_seguro
    FROM   taa_cd_det_cedido t
    WHERE  t.fecha_trasp <= TODAY
    AND    t.estado IN(103,12)

    INSERT INTO nss_ced_glob
    SELECT u.nss_cta1
    FROM   uni_unificado u
    WHERE  u.estado = 100
    AND    u.fnotifica <= TODAY

    LET cla_sel = " SELECT a.tipo_solicitud, ",
                         " b.desc_solicitud, ",
                         " count(*)          ",
                  " FROM   afi_mae_afiliado a, tab_tipo_solic b ",
                  " WHERE ",cla_where CLIPPED," ",
                  " AND    a.tipo_solicitud = b.tipo_solicitud ",
                  " AND    a.n_seguro NOT IN (SELECT c.nss ",
                  "                           FROM   nss_ced_glob c) ",
                  " GROUP  BY 1,2 ",
                  " ORDER  BY a.tipo_solicitud "

    IF pantalla = 0  THEN
         START REPORT rep_solicitud TO salida
    END IF

    PREPARE claexe FROM cla_sel
    DECLARE cur_solicitud CURSOR FOR claexe

    FOREACH cur_solicitud INTO g_reg.tipo_solicitud,
                               g_reg.desc_solicitud,
                               g_reg.total

         IF pantalla = 0 THEN
              OUTPUT TO REPORT rep_solicitud(g_reg.*)
         END IF
 
         LET arr1[i].numero         = i
         LET arr1[i].tipo_solicitud = g_reg.tipo_solicitud
         LET arr1[i].desc_solicitud = g_reg.desc_solicitud
         LET arr1[i].total          = g_reg.total      

         LET i = i + 1

    END FOREACH

    IF pantalla = 0 THEN
         FINISH REPORT rep_solicitud

         LET g_eco = "echo ", nom_arch CLIPPED, " > ",
                  g_param.ruta_listados CLIPPED,"/rescate.CIFRAS_GLOB.",
                  hoy USING "DDMMYY","_",hora1

         RUN g_eco

         --CALL limpia_nulos()

         --LET vimprime = "lp ",salida
         --RUN vimprime
    ELSE
         OPEN WINDOW ventana2 AT 2,2 WITH FORM "AFIL0412" ATTRIBUTE(BORDER)

         DISPLAY "                        < Ctrl - P > Imprimir                                  " AT 3,4 ATTRIBUTE(BOLD)

         DISPLAY " AFIL041                        TOTAL AFILIADOS                                " AT 4,1 ATTRIBUTE(REVERSE)

         DISPLAY hoy USING "DD-MM-YYYY" AT 4,68 ATTRIBUTE(REVERSE)
      
         IF (i - 1) >= 1 THEN
              CALL SET_COUNT(i-1)
              DISPLAY ARRAY arr1 TO scr_1.*
           
              ON KEY (INTERRUPT)
                 EXIT DISPLAY

              ON KEY (Control-p)
                 LET i= 1
                 LET pantalla = 0

                 START REPORT rep_solicitud TO salida

                 PREPARE claexr FROM cla_sel
                 DECLARE cur_solic_rpt CURSOR FOR claexr

                 FOREACH cur_solic_rpt INTO g_reg.tipo_solicitud,
                                            g_reg.desc_solicitud,
                                            g_reg.total
 
                  OUTPUT TO REPORT rep_solicitud(g_reg.*)

                  LET arr1[i].numero         = i
                  LET arr1[i].tipo_solicitud = g_reg.tipo_solicitud
                  LET arr1[i].desc_solicitud = g_reg.desc_solicitud
                  LET arr1[i].total          = g_reg.total

                  LET i = i + 1
                  END FOREACH

                  FINISH REPORT rep_solicitud

              LET g_eco = "echo ", nom_arch CLIPPED, " > ",
                         g_param.ruta_listados CLIPPED,"/rescate.CIFRAS_GLOB.",
                         hoy USING "DDMMYY","_",hora1

              RUN g_eco

              --CALL limpia_nulos()

              --LET vimprime = "lp ",salida
              --RUN vimprime
              END DISPLAY
    ELSE
         MESSAGE "No existen registros. . ."
         END IF
         CLOSE WINDOW ventana2
    END IF
    DROP TABLE nss_ced_glob
    MESSAGE "REPORTE FINALIZADO..." 

    CLEAR SCREEN

END FUNCTION

REPORT rep_solicitud(g_reg)

    DEFINE g_reg RECORD
           tipo_solicitud  SMALLINT,
           desc_solicitud  CHAR(25),
           total  INTEGER
           END RECORD

    DEFINE  L1   CHAR(01)
    DEFINE  L5   CHAR(05)
    DEFINE  L10  CHAR(10)

    DEFINE cod_afore   SMALLINT
    DEFINE descripcion CHAR(30)

    OUTPUT
       PAGE   LENGTH 60
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
          COLUMN 85,"FECHA : ",TODAY USING"DD/MM/YYYY"
    
    SKIP 3 LINES

    PRINT COLUMN 33, "R E P O R T E   C I F R A S  G L O B A L E S   D E   A F I L I A D O S"

    SKIP 2 LINES   

    PRINT COLUMN 48, "A F I L I A C I O N"

    SKIP 5 LINES

    PRINT 
    PRINT COLUMN 9, "PROGRAMA : AFIL041"

    PRINT
    PRINT COLUMN 9, "PAGINA   : ",PAGENO USING"####"

    PRINT '\033e\033(s218T\033(s14H\033(s7B'
 
    SKIP 1 LINES

    PRINT COLUMN 24,"\332",L10,L1,L1,L1,L1,
                    "\302",L10,L5,
                    "\302",L10,L10,L10,L1,
                    "\302",L10,L10,L10,L1,L1,L1,
                    "\277"

    PRINT COLUMN 24,"|              |",
                    "               |",
                    "                ",
                    "               |",
                    "             NUMERO              |"

    PRINT COLUMN 24,"|    ITEM      |",
                    " T. SOLICITUD  |",
                    " DESCRIPCION DE ",
                    " SOLICITUD     |",
                    "               DE                |"

    PRINT COLUMN 24,"|              |",
                    "               |",
                    "                ",
                    "               |",
                    "            REGISTROS            |"


    PRINT COLUMN 24,"\300",L10,L1,L1,L1,L1,
                    "\301",L10,L5,
                    "\301",L10,L10,L10,L1,
                    "\301",L10,L10,L10,L1,L1,L1,
                    "\331"

    ON EVERY ROW

    PRINT
    PRINT COLUMN 026, num USING "###",
          COLUMN 046, g_reg.tipo_solicitud  USING "#",
          COLUMN 058, g_reg.desc_solicitud  CLIPPED,
          COLUMN 100, g_reg.total           USING "#######&"

          LET num = num +1

    ON LAST ROW

    SKIP 3 LINES

    PRINT COLUMN 24,"\332",L10,L1,L1,L1,L1,
                    "\302",L10,L5,
                    "\302",L10,L10,L10,L1,
                    "\302",L10,L10,L10,L1,L1,L1,
                    "\277"


    PRINT COLUMN 24,"|              |",
                    "               |",
                    "    TOTAL   DE  ",
                    " REGISTROS     |",
          COLUMN 100,SUM(g_reg.total) USING "#######&",
                    "             |"

    PRINT COLUMN 24,"\300",L10,L1,L1,L1,L1,
                    "\301",L10,L5,
                    "\301",L10,L10,L10,L1,
                    "\301",L10,L10,L10,L1,L1,L1,
                    "\331"
END REPORT

FUNCTION limpia_nulos()
#ln--------------------

    LET comm = "cd " ,g_param.ruta_listados CLIPPED,
               "/ ; sed -e '/^$/d' ", nom_arch, " > rep_cif_glob"

    RUN comm

    LET comm = "cd " ,g_param.ruta_listados CLIPPED,
               "/ ; mv rep_cif_glob ", nom_arch

    RUN comm

END FUNCTION
