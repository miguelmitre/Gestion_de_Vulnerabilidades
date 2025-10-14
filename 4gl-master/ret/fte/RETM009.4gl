################################################################################
#Proyecto          => SISTEMA DE AFORES ( SAFRE )                              #
#Owner             => E.F.P.                                                   #
#Programa RETM009  => CONSULTA DE SOLICITUDES PENDIENTES POR CONVIVENCIA       #
#Fecha creacion    => 8 DE ABRIL DEL 2003                                      #
#By                => STEFANIE DANIELA VERA PIÑA                               #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE arr_1 ARRAY[2000] OF RECORD #glo #arr_1       
         folio             LIKE ret_resol_retiro.folio         ,
         n_seguro          LIKE ret_resol_retiro.n_seguro      ,
         consecutivo       LIKE ret_resol_retiro.consecutivo   ,
         rechazo_cod       LIKE cta_his_marca.rechazo_cod      ,
         marca_causa       LIKE cta_his_marca.marca_causa      ,
         fecha_proceso     LIKE ret_resol_retiro.fecha_proceso ,
         fecha_fin         LIKE cta_his_marca.fecha_fin        ,
         fecha_envio       LIKE ret_resol_retiro.fecha_envio   
    END RECORD

    DEFINE arr_3 ARRAY[1000] OF RECORD #glo #arr_3       
         marca_causa2      LIKE tab_marca.marca_cod       ,
         desc_marca_causa  LIKE tab_marca.marca_desc
    END RECORD

    DEFINE reg_1 RECORD #glo #reg_1
        estado            INTEGER
    END RECORD
               
    DEFINE #glo #date
        HOY               DATE

    DEFINE #glo #char
        txt_1             CHAR(1200),
        enter             CHAR(0001),
        desc_marca_causa  CHAR(0050),
        desc_rechazo_cod  CHAR(0050)

    DEFINE #glo #smallint
        pos               ,
        arr_c             ,
        scr_l             ,   
        marca_causa2      ,
	i                 ,
        rechazo_cod2      , 
        sw_1              SMALLINT

    DEFINE #glo #integer  
        v_marca_desde     ,
        v_marca_hasta     ,
        v_retiro          INTEGER

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    LET HOY = TODAY
    OPEN WINDOW retm0091 AT 2,3 WITH FORM "RETM0091" ATTRIBUTE(BORDER)
    DISPLAY " RETM009           SOLICITUDES RECHAZADAS POR CONVIVENCIA                      " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    MENU "MENU"
    {
        COMMAND KEY(I) "(I)v-rt" "Consulta solicitudes de Retiro IV-RT"

             LET v_retiro = 1
             CALL primer_paso(v_retiro) #pp
}
        COMMAND KEY(N) "(N)egativa de Pension"
                       "Consulta solicitudes de Retiro Negativa de Pension"

             LET v_retiro = 2
             CALL primer_paso(v_retiro) #pp

        COMMAND KEY(A) "s(A)r" "Consulta solicitudes de Retiro SAR 92-97"

             LET v_retiro = 3
             CALL primer_paso(v_retiro) #pp

        COMMAND KEY(P) "(P)arcial" "Consulta solicitudes de Retiro Parcial"

             LET v_retiro = 4
             CALL primer_paso(v_retiro) #pp

        COMMAND KEY(L) "p(L)an privado "
                       "Consulta solicitudes de Retiro Plan Privado de Pension" 

             LET v_retiro = 5
             CALL primer_paso(v_retiro) #pp

        COMMAND KEY (R) "(R)etiro 97"
                "Consulta solicitudes del 2% Retiro 97"

             LET v_retiro = 6
             CALL primer_paso(v_retiro) #pp

        COMMAND KEY (T) "(T)odas"
                "Consulta todas las solicitudes de Retiro "

             LET v_retiro = 7
             CALL primer_paso(v_retiro) #pp

        COMMAND KEY(S) "(S)alida" "Vuelve al menu"
            EXIT PROGRAM
    END MENU
CLOSE WINDOW retm0091
END MAIN             

FUNCTION primer_paso(v_retiro)
#pp-------------------

    DEFINE #loc #char
        x_busca         CHAR(500)

    DEFINE #loc #smallint
        flag            SMALLINT

    DEFINE #loc #integer  
       v_retiro         INTEGER
                                           
    OPEN WINDOW retm0092 AT 2,3 WITH FORM "RETM0091" ATTRIBUTE (BORDER)
    DISPLAY " RETM009          SOLICITUDES RECHAZADAS POR CONVIVENCIA                       " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "                             <Ctrl-C> Salir                                    " AT 1,1
    ATTRIBUTE(REVERSE)

    CLEAR FORM                  
    INPUT BY NAME reg_1.* WITHOUT DEFAULTS
        AFTER FIELD estado         
            IF reg_1.estado IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD estado
            END IF                        

            ON KEY(ESC)
                IF reg_1.estado IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD estado
                END IF                        
                EXIT INPUT 
            ON KEY (INTERRUPT)
                PROMPT"PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
    END INPUT   
    
    LET flag = 0    
    CONSTRUCT BY NAME x_busca ON A.folio          ,
                                 A.n_seguro       ,
                                 A.consecutivo    ,
                                 B.rechazo_cod    ,
                                 B.marca_causa    ,
                                 A.fecha_proceso  ,
                                 A.fecha_fin      ,
                                 A.fecha_envio    

        ON KEY (CONTROL-C)
            IF flag = 0 THEN
                LET flag=1
                EXIT CONSTRUCT
            END IF

        ON KEY (INTERRUPT)
            IF flag = 0 THEN
                LET flag=1
                EXIT CONSTRUCT
            END IF

        ON KEY (Esc)
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT
                                           
    END CONSTRUCT

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR ""
        ERROR " BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLOSE WINDOW retm0092
        RETURN
    END IF

    CASE v_retiro
        WHEN 1
            LET  v_marca_desde = 401
            LET  v_marca_hasta = 412
        WHEN 2
            LET  v_marca_desde = 457
            LET  v_marca_hasta = 485
        WHEN 3
            LET  v_marca_desde = 415
            LET  v_marca_hasta = 455
        WHEN 4
            LET  v_marca_desde = 486
            LET  v_marca_hasta = 487
        WHEN 5
            LET  v_marca_desde = 495
            LET  v_marca_hasta = 499
        WHEN 6
            LET  v_marca_desde = 488
            LET  v_marca_hasta = 488
        WHEN 7
            LET  v_marca_desde = 400
            LET  v_marca_hasta = 499
    END CASE     
{
    IF v_retiro = 1 THEN
    CASE reg_1.estado 
        WHEN 1
            LET txt_1 = " SELECT A.folio         ,",
                        "        A.n_seguro      ,",
                        "        A.consecutivo   ,",
                        "        B.rechazo_cod   ,",
                        "        B.marca_causa   ,",
			"        ''              ,",  
                        "        B.fecha_fin     ,",
                        "        C.fecha_val_acc  ",
                        " FROM   res_det_pe03 A, cta_his_marca B,ret_his_det_pe05 C",
                        " WHERE ",x_busca CLIPPED,
                        " AND  A.referencia = 0 ",
                        " AND    A.n_seguro    = B.nss ",
                        " AND    A.consecutivo = B.correlativo ",
               " AND    B.marca_cod BETWEEN ",v_marca_desde," AND",v_marca_hasta
        WHEN 2
            LET txt_1 = " SELECT A.folio         ,",
                        "        A.n_seguro      ,",
                        "        A.consecutivo   ,",
                        "        B.rechazo_cod   ,",
                        "        B.marca_causa   ,",
                        "        A.fecha_proceso ,",
                        "        B.fecha_fin     ,",
                        "        A.fecha_envio    ",
                        " FROM   ret_resol_retiro A, cta_his_marca B",
                        " WHERE ",x_busca CLIPPED,
                        " AND  A.referencia > 0 ",
                        " AND    A.n_seguro    = B.nss ",
                        " AND    A.consecutivo = B.correlativo ",
               " AND    B.marca_cod BETWEEN ",v_marca_desde," AND",v_marca_hasta

        WHEN 3
            LET txt_1 = " SELECT A.folio         ,",
                        "        A.n_seguro      ,",
                        "        A.consecutivo   ,",
                        "        B.rechazo_cod   ,",
                        "        B.marca_causa   ,",
                        "        A.fecha_proceso ,",
                        "        B.fecha_fin     ,",
                        "        A.fecha_envio    ",
                        " FROM   ret_resol_retiro A, cta_his_marca B",
                        " WHERE ",x_busca CLIPPED,
                        " AND  A.referencia > 0 ",
                        " AND    A.n_seguro    = B.nss ",
                        " AND    A.consecutivo = B.correlativo ",
              " AND    B.marca_cod BETWEEN ",v_marca_desde," AND",v_marca_hasta,
                        " UNION ",
                        " SELECT A.folio         ,",
                        "        A.n_seguro      ,",
                        "        A.consecutivo   ,",
                        "        B.rechazo_cod   ,",
                        "        B.marca_causa   ,",
                        "        A.fecha_proceso ,",
                        "        B.fecha_fin     ,",
                        "        A.fecha_envio    ",
                        " FROM   ret_resol_retiro A, cta_his_marca B",
                        " WHERE ",x_busca CLIPPED,
                        " AND  A.referencia = 0 ",
                        " AND    A.n_seguro    = B.nss ",
                        " AND    A.consecutivo = B.correlativo ",
               " AND    B.marca_cod BETWEEN ",v_marca_desde," AND",v_marca_hasta
}
    CASE reg_1.estado 
        WHEN 1
            LET txt_1 = " SELECT A.folio         ,",
                        "        A.n_seguro      ,",
                        "        A.consecutivo   ,",
                        "        B.rechazo_cod   ,",
                        "        B.marca_causa   ,",
                        "        A.fecha_proceso ,",
                        "        B.fecha_ini     ,",
                        "        A.fecha_envio    ",
                        " FROM   ret_resol_retiro A, cta_his_marca B",
                        " WHERE ",x_busca CLIPPED,
                        " AND    A.referencia  =  0 ",
                        " AND    A.n_seguro    =  B.nss ",
                        " AND    A.consecutivo =  B.correlativo ",
			" AND    B.rechazo_cod <> 0 ",
               " AND    B.marca_cod BETWEEN ",v_marca_desde," AND",v_marca_hasta

        WHEN 2
            LET txt_1 = " SELECT A.folio         ,",
                        "        A.n_seguro      ,",
                        "        A.consecutivo   ,",
                        "        B.rechazo_cod   ,",
                        "        B.marca_causa   ,",
                        "        A.fecha_proceso ,",
                        "        ''              ,",
                        "        A.fecha_envio    ",
                        " FROM   ret_resol_retiro A, cta_his_marca B",
                        " WHERE ",x_busca CLIPPED,
                        " AND    A.referencia  > 0 ",
                        " AND    A.n_seguro    = B.nss ",
                        " AND    A.consecutivo = B.correlativo ",
               " AND    B.marca_cod BETWEEN ",v_marca_desde," AND",v_marca_hasta

        WHEN 3
            LET txt_1 = " SELECT A.folio         ,",
                        "        A.n_seguro      ,",
                        "        A.consecutivo   ,",
                        "        B.rechazo_cod   ,",
                        "        B.marca_causa   ,",
                        "        A.fecha_proceso ,",
			"        B.fecha_fin     ,",
                        "        A.fecha_envio    ",
                        " FROM   ret_resol_retiro A, cta_his_marca B",
                        " WHERE ",x_busca CLIPPED,
                        " AND    A.referencia  > 0 ",
                        " AND    A.n_seguro    = B.nss ",
                        " AND    A.consecutivo = B.correlativo ",
              " AND    B.marca_cod BETWEEN ",v_marca_desde," AND",v_marca_hasta,
                        " UNION ",
                        " SELECT A.folio         ,",
                        "        A.n_seguro      ,",
                        "        A.consecutivo   ,",
                        "        B.rechazo_cod   ,",
                        "        B.marca_causa   ,",
                        "        A.fecha_proceso ,",
                        "        B.fecha_ini     ,",
                        "        A.fecha_envio    ",
                        " FROM   ret_resol_retiro A, cta_his_marca B",
                        " WHERE ",x_busca CLIPPED,
                        " AND    A.referencia  = 0 ",
                        " AND    A.n_seguro    = B.nss ",
                        " AND    A.consecutivo = B.correlativo ",
			" AND    B.rechazo_cod <> 0 ",
               " AND    B.marca_cod BETWEEN ",v_marca_desde," AND",v_marca_hasta

    END CASE 

    PREPARE query FROM txt_1    

    DECLARE cur_1 CURSOR FOR query

    LET pos = 1
    FOREACH cur_1 INTO arr_1[pos].*

        IF reg_1.estado = 1 THEN
            SELECT fecha_ini
	    INTO   arr_1[pos].fecha_fin
	    FROM   cta_his_marca  
	    WHERE  nss         = arr_1[pos].n_seguro
	    AND    correlativo = arr_1[pos].consecutivo
	    AND    rechazo_cod = 0

	    LET arr_1[pos].fecha_envio = arr_1[pos].fecha_fin
        END IF

	IF reg_1.estado = 3 THEN
	   LET arr_1[pos].fecha_fin = ""  
        END IF

        LET pos = pos + 1

        IF pos >= 32000 THEN
            ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
            EXIT FOREACH
        END IF

    END FOREACH

    IF pos = 1 THEN
        INITIALIZE arr_1[pos] TO NULL
        CLEAR FORM
        ERROR "    NO EXISTE REGISTRO "
              ATTRIBUTE(NORMAL)
        CLOSE WINDOW retm0092
        RETURN
    END IF   

    CALL SET_COUNT(pos-1)

    LET arr_c = ARR_CURR()
    LET scr_l = SCR_LINE()

    SELECT marca_desc
    INTO   desc_marca_causa
    FROM   tab_marca
    WHERE  marca_cod = arr_1[arr_c].marca_causa
  
    LET  marca_causa2 = arr_1[arr_c].marca_causa 

    SELECT desc_larga   
    INTO   desc_rechazo_cod
    FROM   ret_rechazo_captura
    WHERE  codigo = arr_1[arr_c].rechazo_cod 

    LET rechazo_cod2 = arr_1[arr_c].rechazo_cod

    DISPLAY BY NAME marca_causa2,desc_marca_causa,
                    rechazo_cod2,desc_rechazo_cod

    DISPLAY ARRAY arr_1 TO scr_1.*


         ON KEY (CONTROL-C)
            EXIT DISPLAY
         ON KEY (INTERRUPT)              
            EXIT DISPLAY
      END DISPLAY
CLOSE WINDOW retm0092
END FUNCTION               

FUNCTION descrip_marca_causa(reg_4)
#do-------------------------------------------------

    DEFINE reg_4 RECORD #loc #reg_4
        marca_cod        LIKE tab_marca.marca_cod 
    END RECORD

    DEFINE reg_5 RECORD #loc #reg_5
        marca_desc      LIKE tab_marca.marca_desc
    END RECORD

    SELECT marca_desc
    INTO   reg_5.marca_desc
    FROM   tab_marca
    WHERE  marca_cod = reg_4.marca_cod         
    
    RETURN reg_5.marca_desc,
	   reg_4.marca_cod
END FUNCTION

