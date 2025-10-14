##########################################################################
#Proyecto          => AFORE ( MEXICO )                                   #
#Propietario       => E.F.P.                                             #
#Programa TCAAB012  => CONSULTA DE FOLIOS DE TASPASOS                     #
#Fecha             => 25 DE JULIO DE 2003                                #
#Autor             => JOSE FRANCISCO LUGO CORNEJO                        #
#Sistema           => TCAA(CEDENTE)                                       #
##########################################################################
DATABASE safre_af

GLOBALS
    DEFINE  reg_1        ARRAY[20]  OF   RECORD
            folio                        INTEGER        ,
            tipo_traspaso                CHAR(2)        ,
            estado                       SMALLINT       ,
            desc_estado                  CHAR(17)       ,
            f_presentacion               DATE           ,
            f_envio_saldos               DATE           ,
            f_liquidacion                DATE
                                       END  RECORD
    DEFINE  reg_2        ARRAY[20]  OF   RECORD
            estado                       SMALLINT,
            desc_estado                  CHAR(30),
            total_cuentas                INTEGER 
                                       END  RECORD
    DEFINE
            g_hoy                        DATE

    DEFINE  
            i_arr_1                      ,
            i_arr_2                      ,
            g_cur                        ,
            x                            ,
            g_src                        ,
            g_procedente                 ,
            g_provisionada               ,
            g_vencida                    SMALLINT

    DEFINE
            g_folio                      INTEGER
    DEFINE
            g_enter                      CHAR

END GLOBALS

MAIN
    OPTIONS 
    PROMPT LINE LAST,
    INPUT WRAP
    DEFER INTERRUPT
    LET     g_hoy                 =  TODAY
    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "TCAAB012" ATTRIBUTE(BORDER)
    DISPLAY " <<TCAAB012>>         CONSULTA  FOLIOS  DE  TRASPASOS                 " AT  1,2 ATTRIBUTE(REVERSE)
    DISPLAY g_hoy USING "DD-MM-YYYY" AT 1,63 ATTRIBUTE(REVERSE)
    DISPLAY "   <<Enter>> Continuar                  <<Ctrl-c>> Salir                         " AT 3,2 ATTRIBUTE(REVERSE)
    MENU " TRASPASOS  CEDIDOS "
        COMMAND "Consulta" " Consulta Folios de Traspasos"
            CALL Consulta()
        COMMAND "Salir " " Salir de Programa"
            EXIT MENU
    END MENU
    CLOSE WINDOW ventana_1
END MAIN

FUNCTION Consulta()
#pp-------------------------
    SELECT  a.estado    INTO  g_procedente
      FROM  safre_af:taa_cd_edo_cedente a
     WHERE  a.descripcion     = "PROCEDENTE" 
       AND  a.tipo            =  2;
    SELECT  a.estado    INTO  g_provisionada
      FROM  safre_af:taa_cd_edo_cedente a
     WHERE  a.descripcion     = "PROVISIONADA" 
       AND  a.tipo            =  3;
    SELECT  a.estado    INTO  g_vencida
      FROM  safre_af:taa_cd_edo_cedente a
     WHERE  a.descripcion     = "VENCIDA" 
       AND  a.tipo            =  3;
{
    UPDATE  taa_cd_ctr_folio
       SET  estado          =  g_vencida
     WHERE  estado          =  g_procedente
       AND  fecha_envio_saldos    <  g_hoy;
    UPDATE  taa_cd_ctr_folio
       SET  estado          =  g_vencida
     WHERE  estado          =  g_provisionada
       AND  fecha_liquidacion     <  g_hoy;
}
    CALL  despliega_folios()
END FUNCTION
FUNCTION despliega_folios()
#C-----------------
    DEFINE  l_select           CHAR(1000)
    DEFINE  l_construct        CHAR(1000)
    FOR     x              =  1   TO  20
            INITIALIZE     reg_1[x].*      TO  NULL
    END FOR   

    LET int_flag = FALSE
    CONSTRUCT l_construct    ON  a.folio
                           FROM  folio

    ON KEY(INTERRUPT)
       EXIT PROGRAM

    ON KEY ( RETURN )
       LET int_flag = FALSE
       EXIT CONSTRUCT

    END CONSTRUCT

    LET   l_select       =
          " SELECT a.folio,  ",
          " a.tipo_traspaso, ",
          " a.estado, ",
          " b.descripcion, ",
          " a.fecha_presentacion, ",
          " a.fecha_envio_saldos, ",
          " a.fecha_liquidacion   ",
          " FROM safre_af:taa_cd_ctr_folio a, taa_cd_edo_cedente b",
          " WHERE b.tipo    IN(2,3)  ",
          "   AND  a.estado = b.estado ",
          "   AND ",l_construct   CLIPPED,
          " ORDER BY 1 DESC  "

    LET l_select = l_select CLIPPED

    PREPARE qry_consul FROM l_select 

    DECLARE cursor_c CURSOR FOR qry_consul

    LET i_arr_1 = 1
    FOREACH cursor_c INTO reg_1[i_arr_1].*
        LET     i_arr_1          =  i_arr_1   +  1
    END FOREACH

    IF   (i_arr_1-1) >= 1 THEN

        CALL SET_COUNT(i_arr_1-1)
        DISPLAY ARRAY reg_1 TO scr_1.*
	     ON KEY ( RETURN )
             LET g_cur = ARR_CURR()
             CALL   control_registros()
             LET g_cur = ARR_CURR()
             LET g_src = SCR_LINE()

        ON KEY(INTERRUPT)

            ERROR " CONSULTA  TERMINADA..."
            SLEEP 2
            ERROR ""
            CLEAR FORM
            EXIT DISPLAY 
        END DISPLAY
   ELSE 
          ERROR "  NO EXISTE  FOLIO  "
          SLEEP 2
          ERROR ""
   END IF 

END FUNCTION
FUNCTION  control_registros()
    DEFINE  l_qry         CHAR(1000)
    FOR     x              =  1   TO  20
            INITIALIZE     reg_2[x].*      TO  NULL
    END FOR   
    OPEN WINDOW ventana_2 AT 2,2 WITH FORM "TCAAB012_1" ATTRIBUTE( BORDER)
    DISPLAY " <<TCAAB012>>         CONSULTA  FOLIOS  DE  TRASPASOS                 " AT  1,2 ATTRIBUTE(REVERSE)
    DISPLAY g_hoy USING "DD-MM-YYYY" AT 1,63 ATTRIBUTE(REVERSE)
    DISPLAY "   <<Ctrl-c>> Salir                                                    " AT 3,2 ATTRIBUTE(REVERSE)
    DISPLAY  BY NAME reg_1[g_cur].folio
    IF       reg_1[g_cur].tipo_traspaso        =  1    THEN
         LET    l_qry    =
                'SELECT  a.estado,count(a.estado) ',
                'FROM  safre_af:taa_cd_det_cedido a ',
                'WHERE  a.folio       =  ',reg_1[g_cur].folio CLIPPED,
                'GROUP  BY 1 ',
                'ORDER  BY 1;'
             DISPLAY  "NORMAL"           TO  FORMONLY.tipo_traspaso
    ELSE
         LET    l_qry    =
                'SELECT  a.estado,count(a.estado) ',
                'FROM  safre_af:taa_cd_det_cedido a ',
                'WHERE  a.estado       =  108 ',
                'GROUP  BY 1 ',
                'ORDER  BY 1;'
             DISPLAY  "COMPLEMENTARIO"   TO  FORMONLY.tipo_traspaso
    END IF
    LET      l_qry      =  l_qry    CLIPPED
    LET      i_arr_2         =  1
    PREPARE  sql_edos      FROM  l_qry
    DECLARE  C_2    CURSOR  FOR   sql_edos
#   EXECUTE     sql_edos
    FOREACH C_2 INTO reg_2[i_arr_2].estado,reg_2[i_arr_2].total_cuentas
        SELECT  a.descripcion    INTO  reg_2[i_arr_2].desc_estado  
          FROM  safre_af:taa_cd_edo_cedente a
         WHERE  a.estado           =  reg_2[i_arr_2].estado
           AND  a.tipo             in (1,2,3)
        LET     i_arr_2            =  i_arr_2   +  1
    END FOREACH

        CALL SET_COUNT(i_arr_2 - 1)
     DISPLAY ARRAY reg_2 TO scr_2.*

     END DISPLAY
    CLOSE WINDOW ventana_2
END FUNCTION
