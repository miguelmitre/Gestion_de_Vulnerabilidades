#############################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                         #
#Propietario       => E.F.P.                                                #
#Fecha             => 15 DE DICIEMBRE DEL 2007.                             #
#Por               => JOSE FRANCISCO LUGO CORNEJO                           #
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
    DEFINE      l_record ARRAY[300] OF RECORD
            folio                       INTEGER,
            siefore                     CHAR(02),
            subcuenta                   SMALLINT,
            tipo_movimiento             INTEGER,
            fecha_conversion            DATE,
            monto_en_acciones           DECIMAL(16,6),
            monto_en_pesos              DECIMAL(16,6)
                              END RECORD
    DEFINE  fecha_acc                   DATE
    DEFINE  g_enter                     CHAR(01)

END GLOBALS

GLOBALS "GLOB_REPS.4gl"

MAIN 
    LET       g_folio                =  ARG_VAL(1)  CLIPPED
    LET       g_tot_ctas             =  ARG_VAL(2)  CLIPPED
    LET       g_tip_des1             =  ARG_VAL(3)  CLIPPED
    LET       g_tip_des2             =  ARG_VAL(4)  CLIPPED
    LET       g_nombre_progr         =  ARG_VAL(5)  CLIPPED
    LET       g_titulo_pant          =  ARG_VAL(6)  CLIPPED 
    LET       g_tabname              =  ARG_VAL(7)  CLIPPED 
    LET       g_repprovliq           =  ARG_VAL(8)  CLIPPED
    CALL      inicio()
    CALL      consul_genrep()
END MAIN

FUNCTION  consul_genrep()
    CALL     STARTLOG("GLOB_CON.log")
    LET      HOY                 =  TODAY
    OPEN WINDOW ventana_2 AT 2,2 WITH FORM "GLOB_CON" ATTRIBUTE( BORDER)
    DISPLAY " (Ctrl-C) Salir              (Ctrl-P) Archivo   Imprimir              Consulta " AT 1,1 ATTRIBUTE(REVERSE,green)
    DISPLAY  g_nombre_progr AT 2,1 ATTRIBUTE(REVERSE,green)
    DISPLAY  g_titulo_pant,"   " AT 2,14 ATTRIBUTE(REVERSE,green)
    DISPLAY  HOY USING " dd-mm-yyyy " AT 2,67 ATTRIBUTE(REVERSE,green)
    ERROR   "PROCESANDO INFORMACION"
    CALL     define_querys()
    PREPARE  q_arr FROM g_sql_04
    DECLARE  cursor_1       CURSOR   FOR q_arr
    LET      pos        =  1
    FOREACH  cursor_1  USING g_folio
             INTO      l_record[pos].*  
             IF        pos          =  1  THEN
	               LET      fecha_acc  =  l_record[pos].fecha_conversion
             END IF
             LET       pos      =  pos  +  1
    END FOREACH
    CALL     busca_precc_acc()
    INITIALIZE  l_record[pos].*     TO  NULL
    IF      (pos-1)            >=  1   THEN
   	     CALL     SET_COUNT(pos-1)
             ERROR ""
	     DISPLAY  ARRAY      l_record  TO  scr_1.*
    	               ON KEY    (Control-p)
	                       ERROR "PROCESANDO IMPRESION..."
                               SLEEP 3
                               ERROR " "
                               CALL asigna_globales()
                               CALL genera_reporte()

                               ERROR "LISTADO IMPRESO..."
                               SLEEP 3
                               ERROR " "
	               ON KEY (INTERRUPT)
	                       EXIT DISPLAY
	    END DISPLAY
	    CLEAR    WINDOW    ventana_2
	    CLOSE    WINDOW    ventana_2
    ELSE
	    ERROR   "ARCHIVO ... VACIO"
	    SLEEP    2
	    ERROR ""
    END IF 
END FUNCTION

FUNCTION busca_precc_acc()
    CALL     define_querys()
    PREPARE  q_arr_1 FROM g_sql_05
    DECLARE  cursor_2         CURSOR    FOR  q_arr_1
    FOREACH  cursor_2          USING  g_folio
             INTO      g_fec_b1_b2     
    END FOREACH 
    LET      g_fec_par      =  MDY(MONTH(g_fec_b1_b2),"01",YEAR(g_fec_b1_b2))
    LET      g_fec_pavis    =  g_fec_par
END FUNCTION

FUNCTION  asigna_globales()
    DEFINE   l_fecha_par                 DATE
    LET      g_total_cuentas          =  g_tot_ctas      #viene arg_val
    LET      g_fecha_accion           =  g_fec_b1_b2
    LET      g_fecha_parti            =  g_fec_pavis 
    LET      g_tipo_desc1             =  g_tip_des1      #viene arg_val
    LET      g_tipo_desc2             =  g_tip_des2      #viene arg_val 
    LET      g_nombre_programa        =  g_nombre_progr  #viene arg_val
    LET      g_tip_rep                =  g_repprovliq    #viene arg_val
END  FUNCTION

FUNCTION inicio()
    LET      HOY                      =  TODAY
    SELECT   USER
    INTO     g_usuario
    FROM     tab_afore_local
    SELECT   *
    INTO     g_seg_modulo.*
    FROM     seg_modulo
    WHERE    modulo_cod               =  'taa'
END FUNCTION
