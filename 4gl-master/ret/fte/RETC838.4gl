################################################################################
#Owner             => E.F.P.                                                   #
#Sistema           => RET                                                      #
#Programa RETC838  => RECEPCIONA ARCHIVO DE APORTACIONES ESPECIALES PARA       #
#                     GENERAR LA OPERACION 14                                  #
#Fecha creacion    => 11 DE NOVIEMBRE DEL 2004                                 #
#By                => JUAN CARLOS MENDOZA MORENO                               #
#Fecha actualiz.   =>                                                          #
#Actualizacion     => 24-Ago-2006 - IJR Impedir que carguen mediante archivo   #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE  #s_modulo
        s_modulo              RECORD LIKE seg_modulo.*

    DEFINE reg_2 RECORD  #glo #reg_2
        nom_archivo_esp       CHAR(020)
    END RECORD

    DEFINE #glo INTEGER
       ultimo_folio          ,
       cuantos               INTEGER

    DEFINE #glo CHAR
       enter                 CHAR(001) ,
       usuario               CHAR(008) ,
       carga_reg             CHAR(500) , 
       archivo_especial      CHAR(200)

    DEFINE #glo DATE
       HOY                   DATE
       
    DEFINE #glo smallint
        cont_nss             SMALLINT 
           
END GLOBALS

MAIN
    CALL STARTLOG("RETC838.log")
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        ACCEPT KEY CONTROL-I ,
        PROMPT LINE LAST            

    CALL init()#i

    OPEN WINDOW RETC8381 AT 2,2 WITH FORM "RETC8381" ATTRIBUTE(BORDER)
    DISPLAY " RETC838       RECEPCIONA ARCHIVO DE APORTACIONES ESPECIALES                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    --       MENU "MENU" 
    --           COMMAND KEY(C) "(C)argar archivo" "Cargar registros especiales desde un archivo"
    --               CALL cargar_archivo() #ca
    --           COMMAND KEY(A) "(A)gregar registros" "Agregar registros especiales identificados"
                   CALL agregar_reg() #a
    --           COMMAND KEY(S) "(S)alida" "Salir del Programa"
    --               EXIT PROGRAM
    --       END MENU
    CLOSE WINDOW RETC8381

END MAIN 

FUNCTION cargar_archivo()
#ca----------------------

    OPEN WINDOW RETC8382 AT 2,2 WITH FORM "RETC8382" ATTRIBUTE(BORDER)
    DISPLAY "                              < Ctrl-C > Salir                                     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC838       RECEPCIONA ARCHIVO DE APORTACIONES ESPECIALES                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME reg_2.nom_archivo_esp WITHOUT DEFAULTS
        BEFORE FIELD nom_archivo_esp
            LET reg_2.nom_archivo_esp = NULL
            CLEAR FORM

        AFTER FIELD nom_archivo_esp
            IF reg_2.nom_archivo_esp IS NULL THEN     
                ERROR "   CAMPO NO PUEDE SER NULO  "  
                NEXT FIELD nom_archivo_esp            
            END IF                                    
            
            SELECT "OK"                               
            FROM   ret_cza_especial                   
            WHERE  nom_archivo = reg_2.nom_archivo_esp
            
            IF STATUS <> NOTFOUND THEN
                ERROR "  EL ARCHIVO QUE INTENTA CARGAR YA EXISTE  "
                NEXT FIELD nom_archivo_esp  
            END IF
            
            
            WHENEVER ERROR CONTINUE
                SELECT *
                INTO   s_modulo.*
                FROM   seg_modulo
                WHERE  modulo_cod = "ret"
                
                LET archivo_especial = s_modulo.ruta_rescate CLIPPED,"/",
                                       reg_2.nom_archivo_esp CLIPPED
                
                LOAD FROM archivo_especial DELIMITER "+" 
                INSERT INTO ret_arch_esp
                
                LET cuantos = 0
                
                SELECT count(*)
                INTO   cuantos
                FROM   ret_arch_esp
                
                IF cuantos = 0 THEN
                    ERROR "  NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO  "
                    NEXT FIELD nom_archivo_esp
                ELSE
                    EXIT INPUT
                END IF
            WHENEVER ERROR STOP

        ON KEY (INTERRUPT)
            ERROR "  CARGA DE ARCHIVO CANCELADO ...  "
            SLEEP 2
            ERROR ""
            RETURN 
     END INPUT

     CALL primer_paso()  #pp --Verifica consistencia de datos
     CALL segundo_paso() #sp --Inserta registros 

     PROMPT " PROCESO FINALIZADO PRESIONE <ENTER> PARA SALIR "
     FOR CHAR enter

     CLOSE WINDOW RETC8382
END FUNCTION 


FUNCTION init()
#i------------
    LET HOY=FGL_GETENV("FECHA")
    
    IF HOY IS NULL OR (HOY="12/31/1899") THEN
       LET HOY=TODAY
    END IF

    LET cont_nss = 0

    SELECT USER
    INTO   usuario
    FROM   tab_afore_local

    SELECT MAX(folio) + 1
    INTO   ultimo_folio
    FROM   glo_folio

    IF ultimo_folio IS NULL THEN
        LET ultimo_folio = 1
    END IF

    INSERT INTO glo_folio VALUES (ultimo_folio)
    
    WHENEVER ERROR CONTINUE
        DROP TABLE ret_arch_esp
        
        CREATE TEMP TABLE ret_arch_esp
        (
         n_registros          CHAR(1000)
        )
    WHENEVER ERROR STOP

END FUNCTION

FUNCTION primer_paso()
#pp------------------
    DEFINE reg_temp_carga RECORD #loc #reg_temp_carga
        nss                   CHAR (011) ,
        sec_pension           CHAR (002)
    END RECORD

    DEFINE reg_tmp RECORD #loc #reg_tmp
        nss                   CHAR (011) ,
        sec_pension           CHAR (002)
    END RECORD

    DEFINE #loc #integer
        cont_tmp              INTEGER

    DEFINE #loc #char   
        nss_aux               CHAR(011)
    
    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_carga
        
        CREATE TEMP TABLE tmp_carga
        (
         nss                  CHAR(011),
         sec_pension          CHAR(002)
        )
    WHENEVER ERROR STOP

    DECLARE cur_aux CURSOR FOR
    SELECT *
    FROM   ret_arch_esp

    FOREACH cur_aux INTO carga_reg
        LET reg_temp_carga.nss         = carga_reg[01,11]
        LET reg_temp_carga.sec_pension = carga_reg[12,13]
        
        INSERT INTO tmp_carga
        VALUES( reg_temp_carga.nss        ,
                reg_temp_carga.sec_pension
               )
    END FOREACH
    CLOSE cur_aux

    DECLARE cur_esp CURSOR FOR
    SELECT *
    FROM   tmp_carga       

    FOREACH cur_esp INTO reg_tmp.*
    
        SELECT "OK"
        FROM   afi_mae_afiliado
        WHERE  n_seguro = reg_tmp.nss
        
        IF STATUS = NOTFOUND THEN
            PROMPT " NSS ",reg_tmp.nss," INEXISTENTE...<ENTER> PARA SALIR  " FOR CHAR enter
            EXIT PROGRAM
        ELSE
            SELECT count(*)
            INTO   cont_tmp
            FROM   tmp_carga
            WHERE  nss         = reg_tmp.nss
            AND    sec_pension = reg_tmp.sec_pension
            
            IF cont_tmp > 1 THEN
                PROMPT " NSS ",reg_tmp.nss," DUPLICADO EN EL ARCHIVO...<ENTER> PARA SALIR  " FOR CHAR enter
                EXIT PROGRAM
            END IF

             SELECT COUNT(A.nss)
             INTO   cont_nss
             FROM   ret_transf_rx A ,
                    ret_transf_tx B 
             WHERE  A.nss           = B.nss
             AND    A.consecutivo   = B.consecutivo
             AND    B.diag_registro IN(501,507)
             AND    A.nss           = reg_tmp.nss
             AND    A.sec_pension   = reg_tmp.sec_pension
             
             IF cont_nss = 0 THEN 
                 PROMPT " NSS ",reg_tmp.nss," SIN TRANSFERENCIA DE RECURSOS...<ENTER> PARA SALIR  " FOR CHAR enter
                 EXIT PROGRAM
             END IF 
           
{           
para ver    ificar que los nss de carga no tengan duplicados
            SELECT A.nss ,
                   B.fecha_envio
            FROM   ret_especial     A,
                   ret_cza_especial B
            WHERE  A.folio       = B.folio
            AND    A.nss         = reg_tmp.nss
            AND    A.sec_pension = reg_tmp.sec_pension
            AND    B.fecha_envio IS NULL
}           
            
            SELECT nss                                 -- Confirmar con la Afore si es valida
            INTO   nss_aux                             -- esta condición. Segun yo, si puede 
            FROM   ret_especial                        -- darse el caso de duplicidad
            WHERE  nss         = reg_tmp.nss
            AND    sec_pension = reg_tmp.sec_pension
            
            IF nss_aux = reg_tmp.nss THEN
                 PROMPT " NSS ",reg_tmp.nss," YA EXISTE...<ENTER> PARA SALIR  " FOR CHAR enter
                 EXIT PROGRAM
            END IF
            
            SELECT "OK"
            FROM   ret_extemporanea
            WHERE  nss = reg_tmp.nss
            
            IF STATUS <> NOTFOUND THEN 
                PROMPT " NSS ",reg_tmp.nss," IDENTIFICADO CON APORTACION EXTEMPORANEA...<ENTER> PARA SALIR  " FOR CHAR enter
                EXIT PROGRAM
            END IF 

        END IF 
    END FOREACH
    CLOSE cur_esp
END FUNCTION
    
FUNCTION segundo_paso()
#sp-------------------
    DEFINE reg_1 RECORD #loc #reg_1
        nss                   CHAR (011) , 
        sec_pension           CHAR (002)
    END RECORD 

    DEFINE #loc #integer   
        cont                  INTEGER
        
    LET cont = 0

    DECLARE cur_1 CURSOR FOR
    SELECT *
    FROM   tmp_carga
    
    FOREACH cur_1 INTO reg_1.*
    
        LET cont = cont + 1
        
        INSERT INTO ret_especial
            VALUES(reg_1.nss         , --nss
                   0                 , --consec_tramite
                   ultimo_folio      , --folio
                   reg_1.sec_pension   --sec_pension
                  )
            
        DISPLAY " TOTAL REGISTROS PROCESADOS        : ",cont     AT 10,18
    END FOREACH
    CLOSE cur_1

    INSERT INTO ret_cza_especial
        VALUES(ultimo_folio          ,#folio
               reg_2.nom_archivo_esp ,#nomb_archivo
               HOY                   ,#fecha_recepcion
               usuario               ,#usr_recepcion
               ""                    ,#fecha_envio
               cont                   #tot_registros
              )                  

END FUNCTION


FUNCTION agregar_reg()

    DEFINE la_reg1 ARRAY[500] OF RECORD #la_reg1
        nss                  CHAR(011) ,
        sec_pension          CHAR(002) 
    END RECORD
    
    DEFINE lr_esp RECORD  #loc #lr_esp
        consecutivo          INTEGER   ,
        sec_pension          CHAR(002) ,
        folio                INTEGER   
    END RECORD 
    
    DEFINE #loc #char
        nss_aux              CHAR(011)
        	
    DEFINE #loc #smallint
        tot_reg              ,
        cont2                ,
        cont_reg             ,
        cursor_arr           ,
        ciclo                ,
        arr_c                ,
        sa_reg1              SMALLINT
        
    INITIALIZE lr_esp.* TO NULL 
    LET cont_nss = 0
         
    OPEN WINDOW retc8383 AT 2,2 WITH FORM "RETC8383" ATTRIBUTE(BORDER)
    DISPLAY " <ESC> GRABAR REGISTROS                                       <CTRL-C> SALIR " AT 1,1 
    DISPLAY " RETC838  REGISTROS IDENTIFICADOS COMO APORTACIONES ESPECIALES               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)
    
    INPUT ARRAY la_reg1 WITHOUT DEFAULTS FROM sa_reg1.*
        BEFORE ROW
            LET arr_c   = ARR_CURR()
            LET sa_reg1 = SCR_LINE()

            DISPLAY "  USTED ESTA EN EL REGISTRO : ",arr_c USING "&&&#  " AT 20,1 ATTRIBUTE(REVERSE)

        AFTER FIELD nss
            IF la_reg1[arr_c].nss IS NULL OR la_reg1[arr_c].nss = "" THEN 
                ERROR "  EL NSS NO PUEDE SER NULO  "
                NEXT FIELD nss
            END IF 
        
             SELECT "OK"
             FROM   afi_mae_afiliado
             WHERE  n_seguro = la_reg1[arr_c].nss
             
             IF STATUS = NOTFOUND THEN
                 ERROR "  EL NSS NO ESTA REGISTRADO EN LA AFORE  "
                 NEXT FIELD nss
             END IF
             
             SELECT COUNT(A.nss)
             INTO   cont_nss
             FROM   ret_transf_rx A ,
                    ret_transf_tx B 
             WHERE  A.nss           = B.nss
             AND    A.consecutivo   = B.consecutivo
             AND    B.diag_registro IN(501,502,507)
             AND    A.nss           = la_reg1[arr_c].nss
             
             IF cont_nss = 0 THEN 
                 ERROR " EL NSS NO TIENE RETIROS POR TRANSFERENCIA REGISTRADOS "
                 NEXT FIELD nss
             ELSE 
                 LET nss_aux = 0
                 
                 --RECUPERA LA SECUENCIA DE PENSION DEL AFILIADO
                 DECLARE cur_his CURSOR FOR
                 SELECT A.consecutivo      ,
                        A.sec_pension      ,
                        A.folio            
                 FROM   ret_transf_rx A ,
                        ret_transf_tx B 
                 WHERE  A.nss           = B.nss
                 AND    A.consecutivo   = B.consecutivo
                 AND    B.diag_registro IN(501,507)
                 AND    A.nss           = la_reg1[arr_c].nss
                 ORDER BY 3 DESC
    
                 FOREACH cur_his INTO lr_esp.*
                     --RECUPERA LA PRIMER SECUENCIA DE PENSION
                     IF la_reg1[arr_c].nss <> nss_aux THEN
                         LET la_reg1[arr_c].sec_pension = lr_esp.sec_pension
                     END IF  
                     
                     LET nss_aux = la_reg1[arr_c].nss                 

                 END FOREACH
                 
                 CLOSE cur_his
    
                 DISPLAY la_reg1[arr_c].sec_pension TO sa_reg1[sa_reg1].sec_pension

                 --VALIDA QUE NO SE ENCUENTRE REGISTRADA LA APORTACION ESPECIAL
                 SELECT "OK"
                 FROM   ret_especial
                 WHERE  nss            = la_reg1[arr_c].nss
                 AND    sec_pension    = la_reg1[arr_c].sec_pension
#                 AND    consec_tramite = 0
                 
                 IF STATUS <> NOTFOUND THEN 
                     ERROR "  EL NSS YA EXISTE  "
                     NEXT FIELD nss
                 END IF 

                 SELECT "OK"
                 FROM   ret_extemporanea
                 WHERE  nss = la_reg1[arr_c].nss
                 
                 IF STATUS <> NOTFOUND THEN 
                     ERROR "  NSS IDENTIFICADO CON APORTACION EXTEMPORANEA  "
                     NEXT FIELD nss
                 END IF 
                                 
                 IF arr_c >= 2 THEN
                     FOR cont_reg = 1 TO arr_c
                         FOR cont2 = 1 TO arr_c - 1
                             IF (la_reg1[cont2].nss = la_reg1[arr_c].nss) AND 
                                (la_reg1[cont2].sec_pension = la_reg1[arr_c].sec_pension) THEN
                                 ERROR "  REGISTROS DUPLICADOS  "
                                 NEXT FIELD nss
                             END IF
                         END FOR 
                     END FOR 
                 END IF              
             END IF 

        ON KEY (ESC)
            DISPLAY "" AT 20,1

            WHILE TRUE
                PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        IF arr_c = 1 AND 
                          (la_reg1[arr_c].nss IS NULL OR
                           la_reg1[arr_c].nss = "" ) then
                            ERROR "  NO HAY REGISTROS PARA INGRESAR  "
                            SLEEP 1
                            ERROR ""
                            EXIT INPUT
                        ELSE 
                            LET cont_reg = 0
                            FOR ciclo = 1 TO arr_c - 1
                                    INSERT INTO ret_especial
                                        VALUES(la_reg1[ciclo].nss         , --nss
                                               0                          , --consec_tramite
                                               ultimo_folio               , --folio
                                               la_reg1[ciclo].sec_pension ,  --sec_pension
                                               0 ,
                                               0           
                                              )
                                    LET tot_reg = tot_reg + 1
                            END FOR
                            
                            IF tot_reg <> 0 THEN 
                                 INSERT INTO ret_cza_especial
                                     VALUES(ultimo_folio ,#folio
                                            ""           ,#nomb_archivo
                                            HOY          ,#fecha_recepcion
                                            usuario      ,#usr_recepcion
                                            ""           ,#fecha_envio
                                            tot_reg       #tot_registros
                                           )                  
                                 ERROR " AGREGANDO REGISTROS...  "
                                 SLEEP 1
                                 ERROR ""
                                 CLEAR FORM
                                 PROMPT "  PROCESO TERMINADO...<ENTER> PARA SALIR  " FOR CHAR enter                                 
                            END IF   
                        END IF 
                    ELSE
                        ERROR "  NO SE AGREGARON REGISTROS  "
                    END IF
                    SLEEP 1
                    MESSAGE ""
                    CALL init()
                    EXIT INPUT
                END IF
            END WHILE
            	
        ON KEY (INTERRUPT)
            EXIT INPUT 

    END INPUT

    CLOSE WINDOW retc8383

END FUNCTION 

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
