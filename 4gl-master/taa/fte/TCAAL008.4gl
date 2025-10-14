#############################################################################
#Proyecto        => Sistema de Afores. ( MEXICO )                           #
#Objetivo        => Genera reporte de Liquidacion trasp.(normal y comple)   #
#Propietario     => E.F.P.                                                  #
#Fecha           => 20 DE ENERO DEL 2005 .                                  #
#Por             => MARCO ANTONIO GONZALEZ ROJAS                            #
#Sistema         => TCAA                                                    #
#Programa        => TCAAL008(ESTE PROGAMA SE  INTEGRA CON EL OBJ GLOB_CFOLS)#
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_enter       CHAR(1)
#### VAR PARA ARREGLO
    DEFINE  g_reg_1                      RECORD LIKE safre_af:taa_cd_ctr_folio.*
    DEFINE  g_desc_estado                CHAR(17)

    DEFINE  reg_1        ARRAY[100]  OF  RECORD
            folio                        INTEGER        ,
            tipo_traspaso                CHAR(2)        ,
            estado                       SMALLINT       ,
            desc_estado                  CHAR(17)       ,
            f_presentacion               DATE           ,
            f_envio_saldos               DATE           ,
            f_liquidacion                DATE
                                       END  RECORD
    DEFINE  
            i_arr_1                      ,
            g_cur                        ,
            i                            ,
            x                            ,
            pos                          ,
            g_src                        SMALLINT

    DEFINE  lanza_proceso                CHAR(1500)
    DEFINE  g_folio                      INTEGER
####VAR QUE UTILIZA EL OBJ  GLOB_CFOLS
    DEFINE  g_sql_01_a                   CHAR(1000)
    DEFINE  g_hoy                        DATE
    DEFINE  g_tabla                      CHAR(50)
    DEFINE  g_estado                     SMALLINT
    DEFINE  g_construct                  CHAR(1000)
####VAR PARA ARGVAL
    DEFINE  g_tip_tra                    INTEGER
    DEFINE  g_tot_ctas                   INTEGER
    DEFINE  g_tip_des1                   ,
            g_tip_des2                   CHAR(45)
    DEFINE  g_nor_o_comple               CHAR(30)
    DEFINE  g_nombre_progr               CHAR(15)
    DEFINE  g_titulo_pant                CHAR(55)
    DEFINE  g_tblprovliq                 CHAR(50)
    DEFINE  g_repprovliq                 CHAR(04)


END GLOBALS
	

MAIN

    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
	INPUT WRAP,
	ACCEPT KEY control-o

    CALL asig_var_glob()
    CALL proceso_principal()

END MAIN


FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_21 AT 2,2 WITH FORM "TCAAL0081" ATTRIBUTE(BORDER)
    DISPLAY " <<TCAAL008>>          TRASPASOS  AFORE - AFORE  CEDENTE                               " AT  1,2 ATTRIBUTE(REVERSE)
    DISPLAY g_hoy USING "DD-MM-YYYY" AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY "              FOLIOS  LIQUIDADOS      ( CONSULTAS  Y  REPORTES )                               " AT  2,2 ATTRIBUTE(REVERSE)

    DISPLAY " <Enter> Ver Detalle, <Control-B> Ver Totales Siefore o <Control-c> Salir           "   AT 5,2 ATTRIBUTE(REVERSE)
    CALL    selecciona_folio()
END FUNCTION

FUNCTION selecciona_folio()
#sf----------------
    FOR     x              =  1   TO  10
            INITIALIZE     reg_1[x].*      TO  NULL
    END FOR   

    CONSTRUCT g_construct    ON  a.folio
                           FROM  folio

    ON KEY(INTERRUPT)
       EXIT PROGRAM

    ON KEY ( RETURN )
       LET int_flag = FALSE
       EXIT CONSTRUCT

    END CONSTRUCT

    CALL desp_arr_GLOB_CFOLS()

    PREPARE  qry_consul   FROM  g_sql_01_a
    DECLARE  cursor_c     CURSOR FOR   qry_consul

    LET      i_arr_1      =  0
    FOREACH  cursor_c     INTO  g_reg_1.*,g_desc_estado
        LET     i_arr_1                        =      i_arr_1   +  1
        LET reg_1[i_arr_1].folio               =      g_reg_1.folio
        LET reg_1[i_arr_1].tipo_traspaso       =      g_reg_1.tipo_traspaso 
        LET reg_1[i_arr_1].estado              =      g_reg_1.estado
        LET reg_1[i_arr_1].desc_estado         =      g_desc_estado
        LET reg_1[i_arr_1].f_presentacion      =      g_reg_1.fecha_presentacion
        LET reg_1[i_arr_1].f_envio_saldos      =      g_reg_1.fecha_envio_saldos
        LET reg_1[i_arr_1].f_liquidacion       =      g_reg_1.fecha_liquidacion
        
    END FOREACH
    IF     (i_arr_1)        >=  1    THEN
           CALL     SET_COUNT(i_arr_1)
           DISPLAY  ARRAY reg_1 TO scr_1.*
    ON KEY (Control-B)
       LET     g_cur      =  ARR_CURR()
       LET     g_src      =  SCR_LINE()
       CALL    prep_var_argval()
       CALL    f_910_totales_por_folio(g_tblprovliq ,
               reg_1[g_cur].folio,
               reg_1[g_cur].f_liquidacion)
    	        ON KEY ( RETURN )
                     LET     g_cur      =  ARR_CURR()
                     LET     g_src      =  SCR_LINE()
                     LET     g_folio    =  reg_1[g_cur].folio
                     CALL prep_var_argval()

                     LET lanza_proceso   = "fglgo GLOB_CON ",g_folio CLIPPED," ",g_tot_ctas CLIPPED," '",g_tip_des1 CLIPPED,"' '",g_tip_des2 CLIPPED,"' ",g_nombre_progr CLIPPED," '",g_titulo_pant CLIPPED,"' ","'",g_tblprovliq CLIPPED,"' "," ",g_repprovliq CLIPPED
                     RUN lanza_proceso

                ON KEY (INTERRUPT)

                     EXIT  PROGRAM 
          END DISPLAY
   ELSE 
          PROMPT " NO HAY FOLIOS LIQUIDADOS <Enter> para Salir " FOR g_enter
          EXIT  PROGRAM
   END IF 

END FUNCTION

###############################################################################
FUNCTION asig_var_glob()

   LET  g_tabla                  =         "taa_cd_ctr_folio"
   LET  g_estado                 =         "103"
   LET  g_hoy                    =          TODAY

END FUNCTION
################################################################################
FUNCTION prep_var_argval()


    SELECT tipo_traspaso
    INTO g_tip_tra
    FROM safre_af:taa_cd_ctr_folio
    WHERE folio = g_folio
    CASE  g_tip_tra
       WHEN  1
          LET g_nor_o_comple   = "NORMAL"
          SELECT COUNT(UNIQUE n_seguro)
          INTO g_tot_ctas
          FROM safre_af:taa_cd_det_cedido
          WHERE folio  = g_folio
            AND estado = g_estado
          IF	 (STATUS IS NULL) THEN
             LET g_tot_ctas    = 0
          END IF

       WHEN  2
          LET g_nor_o_comple   = "COMPLEMENTARIO"
          SELECT  COUNT(UNIQUE n_seguro)
          INTO g_tot_ctas
          FROM safre_af:taa_cd_det_comple
          WHERE folio  = g_folio
            AND estado = g_estado
          IF (STATUS IS NULL) THEN
             LET g_tot_ctas    = 0
          END IF

       WHEN  3
          LET g_nor_o_comple   = "INTERNET"
          SELECT COUNT(UNIQUE n_seguro)
          INTO g_tot_ctas
          FROM safre_af:taa_cd_det_cedido
          WHERE folio  = g_folio
            AND estado = g_estado
          IF	 (STATUS IS NULL) THEN
             LET g_tot_ctas    = 0
          END IF
       OTHERWISE
    END CASE

    LET    g_tip_des1                    =         "LIQUIDACION AF-AF POR TIPO MOV"," ",g_nor_o_comple CLIPPED

    LET    g_tip_des2                    =         "LIQUIDACION AF-AF POR SUBCUENTA"," ",g_nor_o_comple CLIPPED

    LET    g_nombre_progr                =         "TCAAL008"
    
     LET    g_titulo_pant                 =        "Consulta de Liquidacion Afore Cedente AF-AF"

    LET    g_tblprovliq                  =         "dis_cuenta b"

    LET    g_repprovliq                  =         "LIQ"

END FUNCTION
