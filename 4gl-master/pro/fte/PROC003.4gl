#################################################################################
#Proyecto          => SISTEMA SAFRE ( MEXICO )                                  #
#Sistema           => PRO                                                       #
#Programa PROC003  => GENERACION DE ARCHIVO PARA LA CONSAR PROMOTORES DADOS DE  #
#                     BAJA                                                      #
#Fecha             => 24 DE ENERO DEL 2001                                      #
#ELABORADO POR     => FRANCO ESTEBAN ULLOA VIDELA                               #
#Fecha Actualizada => 29 DE MARZO DEL 2004                                      #
#ELABORADO POR     => LAURA EUGENIA CORTES GUZMAN                               #
#Modificado Por    => Isabel Fonseca Frias                                      #
#Fecha             => 17-09-2009                                                #
#Observacion       => Se actualiza de acuerdo a layout del 27/07/09             #
#                  => (v10)                                                     #
#                  => FSR SE REALIZA CAMBIO EN LA ACTUALIZACIÓN PRO_MAE_PROMOTOR#
#CPL-1820          => FSR ACTUALIZACION DE LAYOUT 26/03/2015                    #
#################################################################################
DATABASE safre_af
GLOBALS
    DEFINE #glo #paramgrales
        parametro      RECORD LIKE seg_modulo.*,

        HOY                DATE,
        borra_lineas       CHAR(200) ,
        enter              CHAR(01) ,
        G_LISTA_1          CHAR(300) ,
        G_LISTA_2          CHAR(300) ,
        G_LISTA_3          CHAR(300) , 
        g_usuario          CHAR(008) ,       
        tot_regis_env      ,
        tot_registros      ,#MLM-3507
        s_codigo_afore     SMALLINT

    DEFINE ls_status SMALLINT,#MLM-3507
           lc_report_curp CHAR(18) #MLM-3507      
        
    DEFINE reg_303 RECORD #loc #reg_303
        cod_promotor          CHAR(10) ,
        unico                 CHAR(18) , #CPL-1890
        motivo_suspende       CHAR(2)  ,
        fecha_baja             DATE
    END RECORD

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

   CALL STARTLOG(FGL_GETENV("USER")||".PROC003.log")
   CALL init() #i
   
   LET tot_registros = 0
   
   OPEN WINDOW proc0031 AT 4,4 WITH FORM "PROC0031" ATTRIBUTE(BORDER)
   DISPLAY "                           < Ctrl-C > Sa",
           "lir                                    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " PROC003     GENERACION DE ARCHIVO PROMOTORES DADOS ",
           "DE BAJA                    " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   WHILE TRUE
       PROMPT " ESTA SEGURO  S/N " FOR CHAR enter
       IF enter MATCHES "[SsnN]" THEN
           IF enter MATCHES "[Ss]" THEN
               
               SELECT COUNT(*) 
               INTO tot_registros
               FROM   pro_mae_promotor A
               WHERE  A.motivo_suspende MATCHES "2*"
               AND    A.motivo_suspende      <> "2F"
--               AND    A.status                =  2    --(v10)
               AND    A.status                in (2,3)    --(v10)
               AND    A.status_interno        = 0

               OR     A.motivo_suspende MATCHES "3*"
               AND    A.status                =  3
               AND    A.status_interno        = 0


               IF tot_registros = 0 THEN
                   PROMPT " NO EXISTEN REGISTROS ...<ENTER> PARA SALIR "
                   FOR CHAR enter
                   EXIT PROGRAM
               END IF
               EXIT WHILE
           ELSE
               PROMPT " PROCESO CANCELADO... <ENTER> PARA SALIR "
               FOR CHAR enter
               EXIT PROGRAM
           END IF
       END IF
   END WHILE

   DISPLAY " PROCESANDO INFORMACION... " AT 19,2 ATTRIBUTE(REVERSE)

    DECLARE cursor_2 CURSOR FOR
       SELECT A.cod_promotor    ,
              A.unico           ,
              A.motivo_suspende ,
              A.fecha_baja
       FROM   pro_mae_promotor A
       WHERE  A.motivo_suspende MATCHES "2*"
       AND    A.motivo_suspende      <> "2F"
       AND    A.motivo_suspende      <> "2C"
--       AND    A.status                = 2         --(v10)
       AND    A.status                in (2,3)      --(v10)
       AND    A.status_interno        = 0
       OR     A.motivo_suspende MATCHES "3*"
       AND    A.status                = 3
       AND    A.status_interno        = 0

       START REPORT listado_1 TO G_LISTA_1
       

    FOREACH cursor_2 INTO reg_303.*
       #VALIDAMOS QUE TENGAN BIOMETRICOS ACEPTADOS, EN CASO CONTRARIO NO DEBERÁN SER ENVIADO Y SOLO SE
       #DEBERAN REPORTAR.
        #RECHAZOS DE BIOMETRICOS MLM-3507
          
          --SELECT "OK"
          --FROM afi_ctr_det_op15
          --WHERE curp = reg_303.unico
          --AND status_interno = 30    	
          --
          --IF SQLCA.SQLCODE = 0 THEN 
             CALL primer_paso() #pp
          --ELSE 
          --  	#EN CASO NO CONTENGA EXPEDIENTE SE INGRESARÁ A UNA TABLA PARA GENERAR UN RERPORTE DE LOS RECHAZADOS
          --     LET ls_status = 0
          --                              
          --        SELECT status_interno                  
          --        INTO ls_status
          --        FROM afi_ctr_det_op15         
          --        WHERE curp = reg_303.unico
          --        GROUP BY 1      
          --                    	 
          --      INSERT INTO safre_tmp:pro_baj_rechazo VALUES (reg_303.unico,ls_status, TODAY)
          --      #RECHAZOS DE BIOMETRICOS MLM-3507
          --END IF 
    END FOREACH
    
    FINISH REPORT listado_1

    LET borra_lineas = "sed -e '/^$/d' ",
                       G_LISTA_1 CLIPPED ,
                       " > ",
                       G_LISTA_2 CLIPPED
    RUN borra_lineas

    LET G_LISTA_2 = "chmod 777 ",
                    parametro.ruta_envio CLIPPED,"/",
                    "BAJ"
    RUN G_LISTA_2

    IF tot_regis_env <> 0 THEN
       INSERT INTO pro_ctr_envio VALUES (HOY,"BAJ",2,"",tot_regis_env,"")
    END IF

   #MLM-3507
    SELECT "OK"
    FROM safre_tmp:pro_baj_rechazo
    GROUP BY 1 
    
   -- IF SQLCA.SQLCODE = 0 THEN 
    
      START REPORT listado_2 TO G_LISTA_3
       DECLARE cur_2 CURSOR FOR 
        
          SELECT curp
    			FROM safre_tmp:pro_baj_rechazo
    			GROUP BY 1 
       
       FOREACH cur_2 INTO lc_report_curp 
        
        OUTPUT TO REPORT listado_2 (lc_report_curp)
       END FOREACH
      
      FINISH REPORT listado_2  
    
   -- END IF  


   CLOSE WINDOW proc0031

   OPEN WINDOW proc0031 AT 4,4 WITH FORM "PROC0031" ATTRIBUTE(BORDER)
   DISPLAY "                           < Ctrl-C > Sa",
           "lir                                    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " PROC003     GENERACION DE ARCHIVO PROMOTORES DADOS ",
           "DE BAJA                    " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   DISPLAY " TOTAL DE BAJAS : ",tot_registros AT 09,18
  -- DISPLAY " CON BIOMETRICOS ACEPTADOS   : ",tot_regis_env AT 10,18

    SELECT "OK"
    FROM safre_tmp:pro_baj_rechazo
    GROUP BY 1 
    
    --IF SQLCA.SQLCODE = 0 THEN 
    --  DISPLAY "REPORTE DE RECHAZOS: " AT 14,05
    --   DISPLAY G_LISTA_3 AT 15,05
    --END IF 
          
   
   
   PROMPT " PROCESO FINALIZADO ...<ENTER> PARA SALIR " FOR CHAR enter

   CLOSE WINDOW proc0031
END MAIN

FUNCTION init()
#i-------------
    LET HOY  = TODAY

    SELECT codigo_afore INTO s_codigo_afore FROM tab_afore_local

    SELECT *, USER 
    INTO   parametro.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = "pro"

    LET G_LISTA_1 = parametro.ruta_envio CLIPPED,"/","baj"
    LET G_LISTA_2 = parametro.ruta_envio CLIPPED,"/","BAJ"
    LET G_LISTA_3 = parametro.ruta_envio CLIPPED,"/RECH_BAJ_",g_usuario CLIPPED, "_", TODAY USING "DDMMYYYY"    
    
    LET tot_regis_env = 0

    #CREAMOS TABLAS EN DONDE SE GUARDARON LOS RECHAZOS 
    
    WHENEVER ERROR CONTINUE
    DATABASE safre_tmp
    DROP TABLE pro_baj_rechazo 
     
     #RECHAZOS DE BIOMETRICOS MLM-3507
     CREATE TABLE pro_baj_rechazo
      ( curp CHAR(18)      , 
        status_bio SMALLINT, #ESTATUS EN LOS BIOMETRICOS
        fecha_intento DATE )  
     
    WHENEVER ERROR STOP
   DATABASE safre_af
       
       SELECT "OK"
       FROM safre_tmp:pro_baj_rechazo
       GROUP BY 1
       
       IF SQLCA.SQLCODE = 0 THEN 
       	PROMPT "EL USUARIO NO CUENTA CON LOS PERMISOS NECESARIOS [ENTER] P/SALIR" FOR CHAR enter
       		EXIT PROGRAM
      END IF   
    
           
END FUNCTION

FUNCTION primer_paso()
#pp-------------------

    DEFINE vfecha_baja         DATE,
           vfecha_baja2        DATE


      #LET tot_registros = tot_registros + 1

      LET vfecha_baja = reg_303.fecha_baja + 29 UNITS DAY

      LET tot_regis_env = tot_regis_env + 1

      #DISPLAY " TOTAL DE REGISTROS PROCESADOS : ",tot_registros AT 09,18
      #DISPLAY " TOTAL DE REGISTROS ENVIADOS   : ",tot_regis_env AT 11,18

      OUTPUT TO REPORT listado_1(reg_303.*) #l1

      INSERT INTO pro_envio_scb VALUES (HOY,reg_303.unico,reg_303.cod_promotor,reg_303.motivo_suspende,reg_303.fecha_baja,1,"")

      UPDATE pro_mae_promotor
      SET    status_interno = 1
      WHERE  cod_promotor  = reg_303.cod_promotor
      AND    status_interno        = 0

      {WHERE  pro_mae_promotor.status                = 2
      AND    pro_mae_promotor.status_interno        = 0
      AND    pro_mae_promotor.cod_promotor          = reg_303.cod_promotor
      OR     pro_mae_promotor.motivo_suspende MATCHES "3*"
      AND    pro_mae_promotor.status                = 3
      AND    pro_mae_promotor.cod_promotor          = reg_303.cod_promotor}

      INITIALIZE reg_303.* TO NULL

END FUNCTION

REPORT listado_1(reg_303)
#l1----------------------
    DEFINE reg_303 RECORD #loc #reg_303
               cod_promotor       CHAR(10) ,
               unico              CHAR(18) , #CPL-1890
               motivo_suspende    CHAR(2)  ,
               fecha_baja         DATE
           END RECORD,

           c8_fecha_proceso       CHAR(08),
           nro_lote               DECIMAL(9,0)

    OUTPUT
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT
            COLUMN 001,"303"                              ,#tipo_registro
            COLUMN 004,reg_303.cod_promotor               ,
            COLUMN 014,reg_303.unico                      , #CPL-1890 
            COLUMN 032,reg_303.motivo_suspende            ,
            COLUMN 034,reg_303.fecha_baja USING"YYYYMMDD" ,
            COLUMN 042,859 SPACES
END REPORT

#RECHAZOS DE BIOMETRICOS MLM-3507
REPORT listado_2(lc_curp)

 DEFINE lc_curp CHAR(18)

    OUTPUT
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0
       PAGE LENGTH   66

    FORMAT
    PAGE HEADER

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 01,"=================================================",
            COLUMN 50,"=============================="
        PRINT
            COLUMN 02,"LISTA DE PROMOTORES QUE NO CONTIENEN BIOMETRICOS"                       ,
            COLUMN 60,"FECHA     :",TODAY     USING "DD-MM-YYYY" 
        PRINT
            COLUMN 02,"NO ENVIADOS  A PROCERSAR EN 304"                               ,
            COLUMN 50,"Nro.PAGINA:",PAGENO    USING "##########"
        PRINT
            COLUMN 01,"=================================================",
            COLUMN 50,"=============================="
        PRINT
            COLUMN 01,"CURP"        ,
            COLUMN 30,"MOTIVO"      

        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"------------------------------"

    ON EVERY ROW
        PRINT
            COLUMN 01,lc_curp       ,
            COLUMN 30,"NO CUENTA NO ENROLAMIENTO BIOMETRICO ACEPTADO"        

END REPORT
