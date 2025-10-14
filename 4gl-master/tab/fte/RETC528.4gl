################################################################################
#Owner             => E.F.P.                                                   #
#Programa RETC528  => CONSULTA HISTORICO DE MARCAS                             #
#Fecha creacion    => 25 DE JULIO DEL 2002                                     #
#By                => MARCOS GODINEZ JIMENEZ                                   #
#Actualizacion     => STEFANIE DANIELA VERA PIÑA                               #
#Fecha actualiz.   => 05 DE DICIEMBRE DEL 2002                                 #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS

    DEFINE #glo #date
           HOY                   DATE

    DEFINE #glo #char
           enter                 CHAR(001),
           x_busca               CHAR(150)
  
    DEFINE #glo #int
           next_fld              ,
           total_pa              ,
           array_sz              ,
	   x                     ,
           v_rowid               INTEGER
            
    DEFINE #glo #smallint
           last_key              ,
           arr_c                 ,
           arr_c_1               ,
           arr_c_3               ,
           linea                 ,
           over_size             ,
	   cambio                ,
           tot_pag               SMALLINT

    DEFINE arr_0 ARRAY[32000] OF RECORD
           nss              LIKE cta_his_marca.nss,
           marca_cod        LIKE cta_his_marca.marca_cod,
           fecha_ini        LIKE cta_his_marca.fecha_ini,
           fecha_fin        LIKE cta_his_marca.fecha_fin,
           hora_ini         LIKE cta_his_marca.hora_ini,
           estado_marca           LIKE cta_his_marca.estado_marca,
           rechazo_cod      LIKE cta_his_marca.rechazo_cod,
           marca_causa      LIKE cta_his_marca.marca_causa,
           fecha_causa      LIKE cta_his_marca.fecha_causa,
           correlativo      LIKE cta_his_marca.correlativo, 
           usr_marca          LIKE cta_his_marca.usr_marca     
    END RECORD

    DEFINE arr_0_1  RECORD 
           nss              LIKE cta_his_marca.nss,
           marca_cod        LIKE cta_his_marca.marca_cod,
           fecha_ini        LIKE cta_his_marca.fecha_ini,
           fecha_fin        LIKE cta_his_marca.fecha_fin,
           hora_ini         LIKE cta_his_marca.hora_ini,
           estado_marca           LIKE cta_his_marca.estado_marca,
           rechazo_cod      LIKE cta_his_marca.rechazo_cod,
           marca_causa      LIKE cta_his_marca.marca_causa,
           fecha_causa      LIKE cta_his_marca.fecha_causa,
           correlativo      LIKE cta_his_marca.correlativo,
           usr_marca          LIKE cta_his_marca.usr_marca     
    END RECORD

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
       INPUT WRAP           ,
       PROMPT LINE LAST     ,
       ACCEPT KEY CONTROL-I
    LET array_sz = 1000
    CALL init() #i
    OPEN WINDOW retc5281 AT 2,4 WITH FORM "RETC5282" ATTRIBUTE(BORDER)
    DISPLAY " <Esc> Consulta                                        <Ctrl-C> Para Salir             " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC5281                  CONSULTA DE MARCAS                                  " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 2,65 ATTRIBUTE(REVERSE)
    WHILE TRUE
        LET INT_FLAG = FALSE
        LET cambio = 0
        CONSTRUCT BY NAME x_busca ON 
                     nss               ,
                     marca_cod         ,
                     fecha_ini         ,
                     fecha_fin         ,
                     hora_ini          ,
                     estado_marca            ,
                     rechazo_cod       ,
                     marca_causa       , 
                     fecha_causa       ,
                     correlativo       ,  
                     usr_marca 

            ON KEY (ESC)
                EXIT CONSTRUCT 
                
            ON KEY (CONTROL-C)
                EXIT PROGRAM

            ON KEY (INTERRUPT)
                EXIT PROGRAM
        END CONSTRUCT

        DISPLAY "Procesando Informacion ..." AT 22,2 ATTRIBUTE(REVERSE)   
        SLEEP 1 
        CALL primer_paso()#pp
        CLEAR FORM
    END WHILE

    CLOSE WINDOW retc5281
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY
END FUNCTION

FUNCTION primer_paso() 
#pp-------------------


    DEFINE #loc #char
        text_1                CHAR(1000)

    DEFINE #loc #smallint
        arr_c                 ,
        scr_l                 ,
        sw_1                  ,
        i                     SMALLINT,
        j                     SMALLINT


    LET text_1 = " SELECT A.nss             ,",
                 "        A.marca_cod       ,",
                 "        A.fecha_ini       ,",
                 "        A.fecha_fin       ,",
                 "        A.hora_ini        ,",
                 "        A.estado_marca          ,",
                 "        A.rechazo_cod     ,",
                 "        A.marca_causa     ,",
                 "        A.fecha_causa     ,",
                 "        A.correlativo     ,", 
                 "        A.usr_marca          ",
                 " FROM   cta_his_marca  A   ",
                 " WHERE  ",x_busca CLIPPED,
                 " ORDER BY 2 "

    PREPARE pre_1 FROM text_1
    DECLARE cur_1 CURSOR FOR pre_1

    LET over_size = FALSE
    LET i = 1
    FOREACH cur_1 INTO arr_0_1.*

       LET arr_0[i].nss               = arr_0_1.nss                
       LET arr_0[i].marca_cod         = arr_0_1.marca_cod  
       LET arr_0[i].fecha_ini         = arr_0_1.fecha_ini  
       LET arr_0[i].fecha_fin         = arr_0_1.fecha_fin  
       LET arr_0[i].hora_ini          = arr_0_1.hora_ini  
       LET arr_0[i].estado_marca            = arr_0_1.estado_marca  
       LET arr_0[i].rechazo_cod       = arr_0_1.rechazo_cod  
       LET arr_0[i].marca_causa       = arr_0_1.marca_causa  
       LET arr_0[i].fecha_causa       = arr_0_1.fecha_causa  
       LET arr_0[i].correlativo       = arr_0_1.correlativo 
       LET arr_0[i].usr_marca           =  arr_0_1.usr_marca 

       LET i = i + 1

       IF i >= 32000 THEN
          ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
          EXIT FOREACH
       END IF                

   END FOREACH
 
IF i = 1 THEN 
   PROMPT "No se Encontraron Registros ...  <ENTER> para Continuar " for char enter
   RETURN
END IF 
 
OPEN WINDOW retc5282  AT 2,4 WITH FORM "RETC5282" ATTRIBUTE(BORDER)
    DISPLAY " <Esc> Consulta                                        <Ctrl-C> Para Salir             " AT 1,1 ATTRIBUTE(REVERSE)
 
DISPLAY " RETC5282              CONSULTA HISTORICO DE MARCAS                            " AT 2,1 ATTRIBUTE(REVERSE)
DISPLAY HOY USING "DD-MM-YYYY" AT 2,65 ATTRIBUTE(REVERSE)

CALL SET_COUNT(i-1)

DISPLAY ARRAY arr_0 TO scr_1.*

   ON KEY(CONTROL-C)
      EXIT DISPLAY    

   ON KEY(INTERRUPT)
      EXIT DISPLAY    

   END DISPLAY

   CLOSE WINDOW retc5282

   DISPLAY "                                                                               " AT 1,1 
   DISPLAY " <Esc> Consulta                                        <Ctrl-C> Para Salir             " AT 1,1 ATTRIBUTE(REVERSE)
END FUNCTION
