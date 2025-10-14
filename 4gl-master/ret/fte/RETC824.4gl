################################################################################
#Proyecto          => SISTEMA DE AFORES ( MEXICO )                             #
#Owner             => E.F.P.                                                   #
#Programa RETC824  => CONSULTA DE OPERACIONES                                  #
#Fecha creacion    => 05 DE FEBRERO 2004                                       #
#By                => JOSE LUIS SALDIVAR CARDOSO                               #
#Actualizacion     => JUAN CARLOS MENDOZA MORENO                               #
#Fecha actualiza   => 18 MARZO 2004                                            #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
   GLOBALS
      DEFINE arr_1 ARRAY[500] OF RECORD #glo #arr_1
         folio                 LIKE ret_ctr_envio_lote.folio           ,
         tipo_retiro           LIKE ret_ctr_envio_lote.tipo_retiro     ,
         estado                LIKE ret_ctr_envio_lote.estado          ,
         desc_estado           CHAR(40)                                ,
         fecha_genera          LIKE ret_ctr_envio_lote.fecha_genera    ,
         fecha_envio           LIKE ret_ctr_envio_lote.fecha_envio     ,
         tot_registros         LIKE ret_ctr_envio_lote.total_registros
      END RECORD

      DEFINE arr_2 ARRAY[1000] OF RECORD #glo #arr_2
         descripcion           CHAR(40)
      END RECORD

      DEFINE #glo #date
         HOY                   DATE

      DEFINE #glo #smallint    ,
         arr_c                 ,
         scr_l                 ,
         i                     SMALLINT

      DEFINE
         fecha_gen             DATE     ,
 	 fecha_env             DATE     ,
         sel_where             CHAR(500),
         cla_where             CHAR(500),
         opc                   CHAR(1)

END GLOBALS
################################################################################
MAIN

    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL init()
    OPEN WINDOW retc824 AT 2,2 WITH FORM "RETC8241" ATTRIBUTE(BORDER)
    DISPLAY " RETC824               CONSULTA DE RETIROS GENERADOS                           " AT 4,1 ATTRIBUTE(REVERSE)
    DISPLAY "  < ESC > CONSULTAR                                        < Ctrl-C > Salir    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 4,67 ATTRIBUTE(REVERSE)
    DISPLAY " DESCRIPCION RETIRO                                                            " AT 18,1 ATTRIBUTE(REVERSE)

    CALL consulta_operaciones() #co
    
END MAIN
################################################################################
FUNCTION init()
#i-------------

    LET HOY = TODAY
    INITIALIZE arr_1 TO NULL
    INITIALIZE arr_2 TO NULL
    LET fecha_gen = NULL
    LET fecha_env = NULL
     
END FUNCTION
################################################################################
FUNCTION consulta_operaciones()
#co----------------------------

    CALL init()
    LET int_flag = FALSE
    CONSTRUCT cla_where ON fecha_genera,
			   fecha_envio 
		      FROM fecha_gen, 
			   fecha_env 
       BEFORE FIELD fecha_gen
          ERROR "  PUEDE CONSULTAR LA FECHA EN UN INTERVALO POR EJEMPLO: 30/02/2001:15/03/2001   "

       BEFORE FIELD fecha_env
          ERROR "  PUEDE CONSULTAR LA FECHA EN UN INTERVALO POR EJEMPLO: 30/02/2001:15/03/2001   "

       ON KEY (ESC)
          LET int_flag = FALSE
          EXIT CONSTRUCT
    END CONSTRUCT

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR ""
        RETURN
    END IF

    LET sel_where = 
    "SELECT A.folio                  ,",
           "A.tipo_retiro            ,",
           "A.estado                 ,",
           "B.descripcion            ,",
           "A.fecha_genera           ,",
           "A.fecha_envio            ,",
           "A.total_registros         ",
    " FROM   ret_ctr_envio_lote A , ret_estado B ",
    " WHERE  ",cla_where CLIPPED,
    " AND   A.estado = B.estado_solicitud ",
    " ORDER BY 1,2,3,4 " CLIPPED

    PREPARE query_1 FROM  sel_where
    DECLARE cur_1 CURSOR FOR query_1

    LET i = 1
    FOREACH cur_1 INTO arr_1[i].*
        LET i = i + 1
        IF i = 501 THEN
            ERROR "  EL ARREGLO FUE LLENADO A SU MAXIMA CAPACIDAD  "
            EXIT FOREACH
        END IF
    END FOREACH

    CALL SET_COUNT(i-1)
    IF (i-1) < 1 THEN
        ERROR "  NO HAY OPERACIONES POR CONSULTAR...  "
        ATTRIBUTE(REVERSE)  
	SLEEP 1
       RETURN
--        CALL consulta_operaciones() #co
    END IF

    INPUT ARRAY arr_1 WITHOUT DEFAULTS FROM scr_1.*
        BEFORE ROW
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

            CALL descripcion_retiro(arr_1[arr_c].tipo_retiro)
                 RETURNING arr_2[1].descripcion

            FOR i = 1 TO 1
                DISPLAY arr_2[i].* TO scr_2[i].*
            END FOR

        ON KEY ( INTERRUPT )
            CLEAR FORM
            EXIT INPUT

        ON KEY ( CONTROL-C )
            CLEAR FORM
            EXIT INPUT

     END INPUT

END FUNCTION
################################################################################
FUNCTION descripcion_retiro(tipo_ret)
#dr------------------------------

    DEFINE tipo_ret    CHAR(01),
           desc_retiro CHAR(60)

    SELECT descripcion
    INTO   desc_retiro
    FROM   tab_retiro
    WHERE  tipo_retiro = tipo_ret

    RETURN desc_retiro
END FUNCTION

