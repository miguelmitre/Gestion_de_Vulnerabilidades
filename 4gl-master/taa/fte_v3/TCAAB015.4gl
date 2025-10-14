####################################################################################
#Proyecto          => AFORE ( MEXICO )                                             #
#Propietario       => E.F.P.                                                       #
#Programa TCAAB015  =>TRASPASO O RECHAZO DE SOLICITUDES DE LA OP. 01               #
#Fecha             => 08 DE JUNIO DE 2011                                          #
#Autor             => JOSE FRANCISCO LUGO CORNEJO                                  #
#Sistema           => TCAA(CEDENTE)                                                #
####################################################################################
#Modificacion      => Se modifica forma y consulta, se eliminan las                #
#                  => columnas: siefore y subcuenta                                #
#                  => Se elimina rutina para marca y desmarca de cuentas           #
#                  => Alejandro Chagoya S.    06-Sep-2011 REQ_XXI-823              #
####################################################################################
#CPL-818           => Se modifica forma de hacer querys de UPDATE de estados       #
#                  => Se cambia SQL por PREPARE  - Alejandro Chagoya 16/mayo/2012  #
####################################################################################
#CPL-874           => Se validan estados 101 y 105 para rechazar/aceptar           #
#                  => solicitudes. -  Alejandro Chagoya 22/mayo/2012               #
####################################################################################

DATABASE safre_af
GLOBALS
    DEFINE  g_reg      ARRAY[20000]  OF  RECORD
            opcion        CHAR(01) ,
            folio         INTEGER  ,
            n_seguro      CHAR(011),
            pesos         DEC(12,2),
            estado        SMALLINT ,
            desc_estado   CHAR(030)
    END RECORD

    DEFINE
        g_fecha_trasp    ,
        g_today          DATE

    DEFINE
        g_nss        CHAR(011),
        g_enter      CHAR(0001),
        HORA         CHAR(0008),
        g_usuario    CHAR(0010),
        ejecuta      CHAR(0500),
        sel_where    CHAR(1000),
        cla_where    CHAR(1000),
        n_seguro     CHAR(0011)

    DEFINE
        i             SMALLINT 

    DEFINE
        g_lastkey     ,
        arr           ,
        src           , 
        pos           ,
        folio         ,
        g_folio       ,
        g_scr_line    ,
        tot_regs      INTEGER 
END GLOBALS

MAIN
    OPTIONS 
    PROMPT LINE LAST,
    INPUT WRAP
    CALL     f_001_inicio()
    CALL     f_100_proceso()
    CLOSE    WINDOW   ventana_1
END MAIN

#######################################################################################
FUNCTION  f_001_inicio()
   LET      g_today      =  TODAY
   LET      HORA         =  TIME
   OPEN WINDOW ventana_1 AT 3,2 WITH FORM "TCAAB015" ATTRIBUTE( BORDER)
   DISPLAY " <TCAAB015>             TRASPASOS  AFORE AFORE CEDENTE                                           " AT  1,1   ATTRIBUTE(REVERSE)
   DISPLAY "  TECLEE  CUENTA o ESTADO Y <Esc>  PARA  BUSCAR  REGISTROS...     " AT  2,10
   DISPLAY g_today USING "DD-MM-YYYY" AT 1,65 ATTRIBUTE(REVERSE)
   DISPLAY "             << TRASPASO O RECHAZO DE SOLICITUDES DE LA OPERACION 01 >>                               " AT  3,1   ATTRIBUTE(REVERSE)
--Se elimina rutina para marca y desmarca de cuentas   --ACS _sep2011

#selecciona fecha maxima
   SELECT   MAX(fecha_liquidacion)
     INTO   g_fecha_trasp
     FROM   taa_cd_ctr_folio
    WHERE   estado             =  101;

#CPL-818
     LET ejecuta =""
     LET ejecuta = "UPDATE safre_af:taa_cd_det_cedido SET estado = 101 ",
                   "WHERE folio = ?",
                   " AND n_seguro = ?"

     PREPARE p_acepta FROM ejecuta

    LET ejecuta =""
    LET ejecuta ="UPDATE safre_af:taa_cd_det_cedido SET estado=105 ",
                 "WHERE n_seguro = ?",
                 "AND folio = ?",
                 " AND estado = 101"
    PREPARE p_rechaza FROM ejecuta

#CPL-818 --> fin

END FUNCTION

#######################################################################################
FUNCTION   f_100_proceso()
   DEFINE   l_tot_reg           INTEGER
   LET      int_flag         =  FALSE
   CONSTRUCT   cla_where      ON  a.folio,
                                  a.n_seguro,
                                  a.estado
                            FROM  folio,
                                  n_seguro,
                                  estado
   ON KEY(ESC)
       LET     int_flag      =  FALSE
       EXIT CONSTRUCT
 
   ON KEY(INTERRUPT)
       LET      int_flag         =  FALSE
       ERROR   "BUSQUEDA CANCELADA..."
       SLEEP 2
       ERROR ""
       CLEAR FORM
       EXIT    PROGRAM
   END CONSTRUCT
   LET       pos        =  1
   LET     sel_where            = 
          'SELECT  a.folio,n_seguro,b.pesos, a.estado ',
          '  FROM  taa_cd_det_cedido  a, ',
          ' OUTER  safre_tmp:taa_cd_num_salarios  b  ',
          ' WHERE  ', cla_where  CLIPPED,
          '   AND  fecha_trasp          >=  ',"'",g_fecha_trasp,"'",
          '   AND  a.n_seguro            =  b.nss ',
          ' ORDER   BY  1,2  '
  LET      sel_where         =   sel_where CLIPPED
  PREPARE  qry_consul   FROM  sel_where
  DECLARE  cursor_c   CURSOR FOR  qry_consul
  FOREACH  cursor_c     INTO  g_reg[pos].folio     THRU  g_reg[pos].estado
           SELECT  trt.descripcion
             INTO  g_reg[pos].desc_estado
             FROM  taa_cd_edo_cedente trt
            WHERE  trt.estado            =  g_reg[pos].estado
           LET     pos                   =  pos      +  1
           IF pos >= 20000 THEN
              ERROR "SE EXCEDIO EL LIMITE DEL ARREGLO"
              EXIT FOREACH
           END IF
   END FOREACH
   IF     (pos-1)        =  0  THEN
            ERROR    "   NO  HAY  REGISTROS...                   "
            PROMPT   "   TECLEE  <ENTER>  PARA  SALIR...  "  FOR  g_enter
            EXIT  PROGRAM
   END IF
   LET     tot_regs          =  pos    -  1
   LET     l_tot_reg         =  tot_regs
   DISPLAY BY NAME       tot_regs
   DISPLAY "                                                                  " AT  2,10
   DISPLAY "  <Ctrl-B>  TRASPASA O RECHAZA SOLICITUDES.      <Ctrl-C>  SALIR...  " AT  2,5
   CALL   SET_COUNT(pos-1)
   INPUT    ARRAY    g_reg  WITHOUT DEFAULTS  FROM  scr_1.*
            BEFORE   ROW
                     LET      arr                   =  ARR_CURR()
                     LET      g_scr_line            =  SCR_LINE()
            AFTER    FIELD    opcion
                     IF       arr         >=  (tot_regs)  THEN
                              LET      g_lastkey  =  FGL_LASTKEY()
                              IF     ((g_lastkey  =  FGL_KEYVAL("down"))   OR
                                      (g_lastkey  =  FGL_KEYVAL("return")) OR
                                      (g_lastkey  =  FGL_KEYVAL("tab"))    OR
                                      (g_lastkey  =  FGL_KEYVAL("right"))) THEN
                                       ERROR    "  NO HAY MAS OPCIONES EN ESA",
                                                " DIRECCION ........           "
                                       NEXT     FIELD    opcion
                              END IF
                     END IF
            ON KEY   ( CONTROL-B )
                     LET      arr                   =  ARR_CURR()
                     LET      g_scr_line            =  SCR_LINE()
                     LET      g_nss                 =  g_reg[arr].n_seguro
                     LET      g_folio               =  g_reg[arr].folio
                     IF       g_reg[arr].estado     =  101    THEN
                              CALL     f_120_rechaza_solicitudes()
                     ELSE
                         IF g_reg[arr].estado = 105 THEN
                            CALL f_130_acepta_solicitudes()
                         ELSE
                            ERROR "NO SE PUEDE ACTUALIZAR CON ESTE ESTADO"
                            NEXT     FIELD    opcion
                         END IF
                     END IF
                              INITIALIZE    g_reg[arr].*     TO  NULL
                              DISPLAY  g_reg[arr].*   TO  scr_1[g_scr_line].*
                              LET      l_tot_reg       =  l_tot_reg    -  1
                              DISPLAY  l_tot_reg      TO  tot_regs

          ON KEY(INTERRUPT)
                ERROR "BUSQUEDA TERMINADA..."
                SLEEP 2
                ERROR ""
                CLEAR FORM
                EXIT INPUT
   END INPUT
   ERROR  "  PROCESANDO  INFORMACION...                "
END FUNCTION

#######################################################################################
FUNCTION    f_120_rechaza_solicitudes()
--Se elimina rutina para marca y desmarca de cuentas   --ACS _sep2011
EXECUTE p_rechaza USING g_nss, g_folio

END FUNCTION

#######################################################################################
FUNCTION  f_130_acepta_solicitudes()
--Se elimina rutina para marca y desmarca de cuentas   --ACS _sep2011
   EXECUTE p_acepta USING g_folio, g_nss

END FUNCTION

