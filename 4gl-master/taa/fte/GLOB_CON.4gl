#############################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                         #
#Propietario       => E.F.P.                                                #
#Fecha             => 15 DE DICIEMBRE DEL 2004.                             #
#Por               => MARCO ANTONIO GONZALEZ ROJAS                          #
#Sistema           => TAA_V3                                                #
#Programa          => GLOB_CON                                              #
#Objetivo          => ESTE PROGRAMA LLAMARA A LAS FUNCIONES GLOB_REP y GLOB_#
#                     REPS,PARA PODER CONSULTAR Y GENERAR REPORTE DE MULTI- #
#                     SIEFORES.                                             #
#############################################################################
DATABASE safre_af
GLOBALS

DEFINE  g_tot_ctas                   INTEGER
DEFINE  g_tip_des1                   ,
        g_tip_des2                   CHAR(45)
DEFINE  g_nombre_progr               CHAR(15)
DEFINE  g_titulo_pant                CHAR(55)
DEFINE  g_repprovliq                 CHAR(50)


DEFINE
        g_b1                         ,
        g_b2                         ,
        g_pavis                      DECIMAL(16,6)
DEFINE  g_fec_b1_b2                  ,
        g_fec_par                    ,
        g_fec_pavis                  DATE
###VAR PARA LA PANTALLA
DEFINE
            i_arr_1                      ,
            g_cur                        ,
            i                            ,
            x                            ,
            pos                          ,
            g_src                        SMALLINT
DEFINE      l_record ARRAY[90] OF RECORD
            siefore               CHAR(14),
            subcuenta             SMALLINT,
            tipo_movimiento       SMALLINT,
            fecha_conversion      DATE,
            precio_accion         DECIMAL(16,6),
            monto_en_acciones     DECIMAL(16,6),
            monto_en_pesos        DECIMAL(16,6)
END RECORD
DEFINE      fecha_acc   DATE


END GLOBALS

GLOBALS "GLOB_REPS.4gl"

MAIN 
   CALL     STARTLOG("GLOB_CON.log")
   LET g_folio                         =               ARG_VAL(1) CLIPPED
   LET g_tot_ctas                      =               ARG_VAL(2) CLIPPED
   LET g_tip_des1                      =               ARG_VAL(3) CLIPPED
   LET g_tip_des2                      =               ARG_VAL(4) CLIPPED
   LET g_nombre_progr                  =               ARG_VAL(5) CLIPPED
   LET g_titulo_pant                   =               ARG_VAL(6) CLIPPED 
   LET g_tblprovliq                    =               ARG_VAL(7) CLIPPED 
   LET g_repprovliq                    =               ARG_VAL(8) CLIPPED

   CALL inicio()
   CALL consul_genrep()

END MAIN
################################################################################
#---c_g
FUNCTION consul_genrep()
    DEFINE     l_siefore         SMALLINT
    DEFINE     l_enter           CHAR(01)
    LET  HOY =  TODAY
    OPEN WINDOW ventana_2 AT 2,2 WITH FORM "GLOB_CON01" ATTRIBUTE( BORDER)
    DISPLAY "<<GLOB_CON>> (Ctrl-B) Ve Totales Siefore (Ctrl-C) Salir   (Ctrl-P) Imprimir      " AT 1,1 ATTRIBUTE(REVERSE,green)
    DISPLAY "FOLIO:",g_folio  USING "#######","      "  AT 2,1
             ATTRIBUTE(REVERSE,green)
    DISPLAY  g_titulo_pant,"   " AT 2,18 ATTRIBUTE(REVERSE,green)
    DISPLAY  HOY   USING " dd-mm-yyyy "  AT  2,67  ATTRIBUTE(REVERSE,green)

    ERROR "PROCESANDO INFORMACION"

    CALL define_querys()
    PREPARE q_arr FROM g_sql_04
    DECLARE cursor_1  CURSOR  FOR q_arr


    LET pos = 1

    FOREACH  cursor_1     USING   g_folio
                           INTO   l_record[pos].*  
          LET     l_siefore         =  l_record[pos].siefore
        SELECT  razon_social
          INTO  l_record[pos].siefore
          FROM  safre_af:tab_siefore_local
         WHERE  codigo_siefore      =  l_siefore;
          IF      pos         =  1           THEN
     	          LET     fecha_acc    =  l_record[pos].fecha_conversion
          END IF

          LET     pos         =  pos   +  1
    END FOREACH

    CALL busca_precc_acc()

    INITIALIZE l_record[pos].*     TO  NULL

    IF (pos-1) >= 1 THEN
	CALL  SET_COUNT(pos-1)

	ERROR ""

	DISPLAY ARRAY l_record TO scr_1.*
                ON KEY (Control-B)
                     LET     g_cur      =  ARR_CURR()
                     LET     g_src      =  SCR_LINE()
                     CALL    asigna_globales()
                     CALL    f_910_totales_por_folio(g_tblprovliq ,
                                                     g_folio,
                                       l_record[g_cur].fecha_conversion)
	        ON KEY (Control-p)
	             ERROR  "PROCESANDO IMPRESION..."
                     SLEEP   3
                     ERROR  " "
                     CALL    asigna_globales()
                     CALL    genera_reporte()

                     ERROR  "LISTADO IMPRESO..."
                     SLEEP   3
                     ERROR  " "
	        ON KEY (INTERRUPT)
	             EXIT DISPLAY
	END DISPLAY

	CLEAR WINDOW ventana_2
	CLOSE WINDOW ventana_2
    ELSE
	ERROR "ARCHIVO ... VACIO"
	SLEEP 2
	ERROR ""
    END IF 

END FUNCTION

###############################################################################
FUNCTION busca_precc_acc()
#bpa----------------------
    CALL define_querys()
    PREPARE q_arr_1 FROM g_sql_05
    DECLARE cursor_2  CURSOR  FOR q_arr_1

    FOREACH cursor_2  USING g_folio
    INTO g_fec_b1_b2     
    END FOREACH 


    LET    g_fec_par    = MDY(MONTH(g_fec_b1_b2),"01",YEAR(g_fec_b1_b2))
    LET    g_fec_pavis  = g_fec_par


           SELECT precio_del_dia
              INTO g_b1         
           FROM glo_valor_accion
           WHERE fecha_valuacion = g_fec_b1_b2   
             AND codigo_siefore = 1

           IF (g_b1 IS NULL) THEN
              LET g_b1  =  0
           END IF

           SELECT precio_del_dia
              INTO g_b2      
           FROM glo_valor_accion
           WHERE fecha_valuacion = g_fec_b1_b2   
             AND codigo_siefore = 2

           IF (g_b2 IS NULL) THEN
              LET g_b2 =  0
           END IF

           SELECT precio_del_dia
              INTO g_pavis    
           FROM glo_valor_accion
           WHERE fecha_valuacion = g_fec_pavis 
             AND codigo_siefore = 11

          IF (g_pavis IS NULL) THEN
             LET g_pavis  =  0
          END IF

END FUNCTION

FUNCTION asigna_globales()
#ag-----------------------
DEFINE   l_fecha_par                  DATE

    LET    g_tabname                =             g_tblprovliq   

    LET    g_total_cuentas          =             g_tot_ctas      #viene arg_val
   
    LET    g_fecha_accion           =             g_fec_b1_b2

    LET    g_fecha_parti            =             g_fec_pavis 

    LET    g_tipo_desc1             =             g_tip_des1     #viene arg_val

    LET    g_tipo_desc2             =             g_tip_des2     #viene arg_val 

    LET    g_nombre_programa        =             g_nombre_progr #viene arg_val

    LET    g_tip_rep                =             g_repprovliq   #viene arg_val

END FUNCTION
################################################################################
FUNCTION inicio()
LET HOY = TODAY

    SELECT  USER
    INTO    g_usuario
    FROM    tab_afore_local

    SELECT  *
    INTO    g_seg_modulo.*
    FROM    seg_modulo
    WHERE   modulo_cod = 'taa'

END FUNCTION
