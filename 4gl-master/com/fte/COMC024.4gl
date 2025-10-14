######################################################################
#Proyecto          => Sistema de AFORE.( MEXICO )                    #
#Propietario       => E.F.P.                                         #
#Programa          => COM                                            #
#Descripcion       => CONSULTA DE PROMOTORES SIN ESTRUCTURA COMERCIAL#
#Fecha             => 3 mayo 2002.                                   #
#Por               => ISABEL FONSECA FRIAS                           #
#Sistema           => COM                                            #
######################################################################
DATABASE safre_af
GLOBALS

         DEFINE g_param_dis    RECORD LIKE seg_modulo.*

         DEFINE aux_pausa       CHAR(1),
                sw_1            SMALLINT,
                g_usuario       CHAR(8),
                HOY             DATE,
                pos             INTEGER,
                g_archivo       CHAR(1000),
                ch              CHAR(10),
                sel_where       CHAR(1000),
                cla_where       CHAR(1000)

         DEFINE l_record   ARRAY[1000] OF RECORD
                cod_promotor    CHAR(10),
                agenc_cod       CHAR(10),
                n_seguro        CHAR(11),
                n_folio         DECIMAL(8,0),
                nivel           SMALLINT
         END RECORD

         DEFINE g_reg           RECORD
                cod_promotor    CHAR(10),
                agenc_cod       CHAR(10),
                n_seguro        CHAR(11),
                n_folio         DECIMAL(8,0),
                nivel           SMALLINT
         END RECORD

END GLOBALS

################################################################################
MAIN
        OPTIONS PROMPT LINE LAST,
                INPUT WRAP,
                ACCEPT KEY control-o
        DEFER INTERRUPT

        CALL inicio()
        CALL proceso_principal()
END MAIN
################################################################################
FUNCTION proceso_principal()

        LET HOY = TODAY

        OPEN WINDOW ventana_1 AT 3,2 WITH FORM "COMC0241" ATTRIBUTE( BORDER)
        DISPLAY " COMC024             CONSULTA PROMOTORES SIN GERENCIA                          " AT 3,1 ATTRIBUTE(REVERSE) 
        DISPLAY hoy USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

        MENU "PROMOTORES SIN GERENCIA"
        COMMAND "Consulta" "Consulta Promotores sin Gerencia"
           CALL Inicializa()
           CALL Consulta()
           CALL Inicializa()
        COMMAND "Salir" "Salir del Programa"
           EXIT MENU
        END MENU
          CLOSE WINDOW ventana_1
END FUNCTION


################################################################################
FUNCTION Inicializa()
        DEFINE ii                        SMALLINT
        CLEAR FORM
        FOR ii = 1 TO 9 
            INITIALIZE l_record[ii].* TO NULL
            DISPLAY l_record[ii].* TO scr_1.*
        END FOR
END FUNCTION
################################################################################

FUNCTION inicio()

   SELECT ruta_envio,USER
   INTO   g_param_dis.ruta_envio,g_usuario
   FROM   seg_modulo
   where modulo_cod = "com"
END FUNCTION

################################################################################

FUNCTION Consulta()

   DISPLAY "(Ctrl-C) Salir      (Ctrl-F) Genera Archivo                                    " AT 5,1 ATTRIBUTE(REVERSE)
   DISPLAY " CONSULTA " AT 5,65 ATTRIBUTE(REVERSE)

         DECLARE cursor_1 CURSOR FOR

         SELECT a.cod_Promotor,
         b.agenc_cod,
         a.n_seguro,
         a.n_folio,
         b.nivel
         FROM   afi_mae_afiliado a,
                pro_mae_promotor b
         WHERE  indicador_comision = 0
         AND    a.cod_promotor = b.cod_promotor
         AND    b.agenc_cod = 0

         ORDER  by 1

         LET pos = 1
         FOREACH cursor_1 INTO l_record[pos].*
                 LET pos = pos + 1
         END FOREACH
         IF (pos-1) >= 1 THEN
            CALL  SET_COUNT(pos-1)
            DISPLAY ARRAY l_record TO scr_1.*


            ON KEY (CONTROL-F)
               ERROR "PROCESANDO INFORMACION ..."
               CALL archivo(pos)
               EXIT DISPLAY

            ON KEY (CONTROL-M)
               EXIT DISPLAY
            END DISPLAY

         ELSE
            ERROR "NO HAY PROMOTORES SIN GERENCIA"
            SLEEP 2
         END IF

END FUNCTION

##################################################################
FUNCTION archivo(pos)
      DEFINE          pos, i             INTEGER

   LET g_archivo = g_param_dis.ruta_envio CLIPPED,"/",g_usuario CLIPPED,
                 ".PROMOTORES_S",hoy USING "YYMMDD" CLIPPED,".txt" CLIPPED

#   DISPLAY g_archivo prompt "" for aux_pausa
   START REPORT genera_archivo TO g_archivo

   FOR i=1 TO (pos+1)
       LET g_reg.cod_promotor        = l_record[i].cod_promotor
       LET g_reg.agenc_cod          = l_record[i].agenc_cod
       LET g_reg.n_seguro           = l_record[i].n_seguro
       LET g_reg.n_folio            = l_record[i].n_folio
       LET g_reg.nivel              = l_record[i].nivel

       IF g_reg.cod_promotor IS NULL THEN
          EXIT FOR
       END IF

       OUTPUT TO REPORT genera_archivo(g_reg.*)
   END FOR

   FINISH REPORT genera_archivo

   ERROR "ARCHIVO GENERADO..."
   SLEEP 2
   ERROR ""

        LET ch = "chmod 777 ",g_archivo
        run ch

END FUNCTION

#####################################################################
REPORT genera_archivo(g_reg)
         DEFINE g_reg           RECORD
                cod_promotor    CHAR(10),
                agenc_cod       CHAR(10),
                n_seguro        CHAR(11),
                n_folio         DECIMAL(8,0),
                nivel           SMALLINT
         END RECORD

    OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0


    FORMAT
    ON EVERY ROW
        PRINT
            COLUMN 001,g_reg.cod_promotor,                    #promotor
            COLUMN 011,",",                                   #separador
            COLUMN 012,g_reg.agenc_cod,                       #agente
            COLUMN 022,",",                                   #separador
            COLUMN 023,g_reg.n_seguro,                        #seguro
            COLUMN 034,",",                                   #separador
            COLUMN 035,g_reg.n_folio,                         #folio
            COLUMN 042,",",                                   #separador
            COLUMN 043,g_reg.nivel                            #nivel
END REPORT




