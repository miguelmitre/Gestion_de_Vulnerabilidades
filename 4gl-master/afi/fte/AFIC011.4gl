##############################################################################
#Project             => SAFRE (Mexico)                                       #
#Owner               => E.F.P.                                               #
#Programa AFIC011    => DESMARCAR CUENTAS TIPO 610                           #
#Fecha actualiz.     => 17 ENERO 2003                                        #
#Actualizacion       => FERNANDO HERRERA HERNANDEZ                           #
#Fecha actualiz.     =>                                                      #
#Sistema             => AFI                                                  #
##############################################################################
DATABASE safre_af
GLOBALS
    DEFINE
           HOY                     DATE,
           pos1			   INTEGER,
	   sel_where		   CHAR(1000),
           cla_where		   CHAR(1000),
	   z_reg2		   ARRAY[9] OF SMALLINT,
           z_reg		   RECORD  
				   orden_1     SMALLINT,           
				   orden_2     SMALLINT
    				   END RECORD,
           reg			   RECORD
 				   nss		CHAR(11),
				   fecha_ini	DATE,
                                   hora_ini     DATETIME HOUR TO SECOND,
				   marca_cod	SMALLINT,
                                   correlativo  INTEGER
				   END RECORD,
	   reg1			   ARRAY[5000] OF RECORD
				   nss		CHAR(11),
                                   nom_comp	CHAR(50),
				   fecha_ini	DATE,
				   marca_cod	SMALLINT,
                                   correlativo  INTEGER
				   END RECORD,
	   reg2			   ARRAY[5000] OF RECORD
                                   hora_ini     DATETIME HOUR TO SECOND
				   END RECORD,
           vn_seguro		   CHAR(11),
           enter                   CHAR(01),
           usuario                 CHAR(08),
           salir                   SMALLINT,
           g_paramgrales RECORD    LIKE seg_modulo.*,
           respuesta		   CHAR(1),
	   vhora		   DATETIME HOUR TO SECOND,
           vpaterno		   CHAR(40),
	   vmaterno		   CHAR(40),
 	   vnombres		   CHAR(40),
	   v_seguro		   CHAR(11),
	   vf_transf_lote	   DATE

    DEFINE
        opc             CHAR(1) ,
        vnss            CHAR(11) ,
        vmarca_entra    SMALLINT ,
        vmarca_estado   SMALLINT ,
        vcodigo_rechazo SMALLINT ,
        g_usuario       CHAR(8)  ,
        ejecuta         CHAR(300),
        xcodigo_marca   SMALLINT ,
        xcodigo_rechazo SMALLINT ,
        edo_proc        SMALLINT ,
        vnom_afore      CHAR(122) ,
        vnom_proce      CHAR(122) ,
        v_marca         CHAR(100) ,
        v_desmarca      CHAR(100) ,
        vcorrelativo    INTEGER        

END GLOBALS
############################################################################
MAIN
    DEFER INTERRUPT
    OPTIONS
    PROMPT LINE LAST,
           INPUT WRAP

    CALL STARTLOG('AFIC011.log')

    LET HOY = TODAY
    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIC011c" ATTRIBUTE(BORDER)
    DISPLAY " AFIC011                     A F I L I A C I O N                               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "                   D E S M A R C A    C U E N T A S    6 1 0                   " AT 6,1 ATTRIBUTE(REVERSE)

    MENU "DESMARCA"
         COMMAND "Consultar" "Consulta cuentas 610."           #c
                  CALL consulta()
         COMMAND "Salir" "Salir del Programa."
              EXIT PROGRAM
    END MENU
END MAIN

############################################################################
FUNCTION init()

    LET salir       = 0
    LET v_marca     = 610

    SELECT *, USER
      INTO g_paramgrales.* , g_usuario        
      FROM seg_modulo
     WHERE modulo_cod = 'afi'

    LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"

END FUNCTION

############################################################################

FUNCTION consulta()
#c-----------------

    OPTIONS
          ACCEPT KEY ESC,
          INPUT WRAP

    CALL init()
    OPEN WINDOW ventana_2 AT 3,5 WITH FORM "AFIC011a" ATTRIBUTE( BORDER)
    DISPLAY "                         OPCIONES DE CONSULTA                                  " AT 2,1 ATTRIBUTE(REVERSE) 
    DISPLAY "[ ESC ] Grabar   [ Ctrl-C ] Salir" AT 1,1 ATTRIBUTES(BOLD)
    LET pos1 = 2
    IF  (pos1 - 1) >= 1 THEN     
        CALL SET_COUNT(pos1 - 1) 
        LET int_flag = FALSE
        CONSTRUCT BY NAME cla_where ON nss, fecha_ini

        IF  int_flag = TRUE THEN          
            LET int_flag = FALSE          
            ERROR "BUSQUEDA CANCELADA..." 
            SLEEP 2                       
            ERROR ""                      
            CLOSE WINDOW ventana_2        
	    RETURN 
        END IF
        CALL ordena() #o
	CLEAR FORM

        LET z_reg2[1] = z_reg.orden_1
        LET z_reg2[2] = z_reg.orden_2
        
        LET sel_where = " SELECT nss, fecha_ini, hora_ini, marca_cod", 
                        " correlativo",
                        " FROM cta_act_marca",
			" WHERE ", cla_where CLIPPED ,
                        " AND marca_cod = 610",
                        " ORDER BY ", z_reg.orden_1,",",z_reg.orden_2

        LET sel_where = sel_where CLIPPED

        PREPARE qry_modif FROM sel_where 
        DECLARE cursor_m CURSOR FOR qry_modif
        LET pos1 = 1

        FOREACH cursor_m INTO reg.*
      
            LET reg1[pos1].nss          = reg.nss 
            LET vn_seguro               = reg.nss
            LET vcorrelativo            = reg.correlativo

            SELECT MAX(@f_transf_lote)
              INTO vf_transf_lote
              FROM afi_det_notifica
             WHERE @n_seguro            = reg.nss 

            SELECT unique @n_seguro, @paterno_proc, @materno_proc, @nombres_proc
              INTO v_seguro, vpaterno, vmaterno, vnombres
              FROM afi_det_notifica
             WHERE @n_seguro = reg.nss
              AND @f_transf_lote        = vf_transf_lote

            IF STATUS = NOTFOUND THEN
                LET reg1[pos1].nom_comp = NULL  
                LET vpaterno            = NULL
                LET vmaterno            = NULL
                LET vnombres            = NULL
                INITIALIZE reg.* TO NULL
            ELSE
                LET reg1[pos1].nom_comp = vnombres CLIPPED, " ", 
                                          vpaterno CLIPPED, " ",
                                          vmaterno CLIPPED
                LET vpaterno            = NULL
                LET vmaterno            = NULL
                LET vnombres            = NULL
            END IF 

            LET reg1[pos1].fecha_ini    = reg.fecha_ini
            LET reg2[pos1].hora_ini     = reg.hora_ini
            LET reg1[pos1].marca_cod    = reg.marca_cod
            LET reg1[pos1].correlativo  = reg.correlativo

            LET pos1                    = pos1 + 1

            IF pos1 < 1 THEN
               ERROR "No existe informacion."
               SLEEP 2
               CLOSE WINDOW ventana_2        
               RETURN
            END IF

            IF pos1 > 4999 THEN
               ERROR "Capacidad de presentacion de informacion insuficiente."
               SLEEP 2
               CLOSE WINDOW ventana_2        
               RETURN
            END IF    

            INITIALIZE reg.* TO NULL

        END FOREACH                          

        CLOSE WINDOW ventana_2              
        INITIALIZE reg1[pos1].* TO NULL

        IF  (pos1 - 1) >= 1 THEN                 
            CALL  SET_COUNT(pos1 - 1)            

            ERROR ""                     

            OPEN WINDOW ventana_10 AT 3,2 WITH FORM "AFIC011c"
               ATTRIBUTE( BORDER)
               DISPLAY "" AT 1,1
               DISPLAY "" AT 2,1
               DISPLAY "[ Ctrl-C ] Salir     [ ENTER ] Desmarcar" AT 2,1 
               ATTRIBUTE(BOLD)
               DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
               DISPLAY ARRAY reg1 TO scr_1.* 
                  ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")
                  ON KEY ( INTERRUPT )
                     ERROR "PROCESO CANCELADO. . ."
                     SLEEP 2
                     ERROR " "
                     EXIT DISPLAY

                  ON KEY ( CONTROL - M )
                     LET pos1 = ARR_CURR()
                     PROMPT "Esta seguro de desmarcar el registro [S/N]: " 
                        FOR respuesta 
                     IF respuesta MATCHES "[Ss]" THEN
                        LET vhora = TIME
                       
                        CALL desmarca_cuenta(vn_seguro,v_marca,g_usuario,
                                             vcorrelativo)

                        INSERT INTO cta_mod_desm VALUES (reg1[pos1].nss, 
                                                         reg1[pos1].marca_cod,
                                                         reg1[pos1].fecha_ini,
                                                         reg2[pos1].hora_ini, 
                                                         today, vhora, usuario)

                       ERROR "Cuenta -DESMARCADA-"
                       SLEEP 2
                       EXIT DISPLAY      
                       ERROR ""
                     ELSE
                       ERROR "Cuenta -NO- desmarcada."
                       SLEEP 2
                       ERROR ""
                     END IF  

               END DISPLAY 

          CLOSE WINDOW ventana_10

          IF vn_seguro IS NULL  OR
             vn_seguro = 0      THEN
             ERROR "  Informacion inexistente" ATTRIBUTE(NORMAL)
             LET salir = 3 
          ELSE
             LET salir = 1
          END IF

          CASE salir 
             WHEN 1
             WHEN 3
                PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR" 
                FOR CHAR enter
          END CASE
        ELSE                                   
            ERROR "NO EXISTE INFORMACION. . ."
            SLEEP 2                        
            ERROR ""                            
	    RETURN
        END IF                                 
    END IF

END FUNCTION

############################################################################
FUNCTION ordena()
#o--------------


    OPEN WINDOW ventana_3 AT 3,5 WITH FORM "AFIC011b" ATTRIBUTE(BORDER)
    DISPLAY " [ Esc ] Grabar      [ Ctrl-C ] Salir"
             AT 1,1 ATTRIBUTE(BOLD) 
    DISPLAY "                      OPCIONES DE ORDENAMIENTO                                        " AT 2,1 ATTRIBUTE(REVERSE,BOLD)

    LET z_reg.orden_1 = 1
    LET z_reg.orden_2 = 2
    DISPLAY BY NAME z_reg.*
    INPUT BY NAME z_reg.* WITHOUT DEFAULTS
        AFTER FIELD orden_1
            IF  z_reg.orden_1 IS NULL THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD orden_1
            ELSE
                IF  z_reg.orden_1 < 1 OR z_reg.orden_1 > 2 THEN
                    ERROR "La opcion de orden digitada no existe...Reingrese"
                    NEXT FIELD orden_1
                END IF
            END IF
        AFTER FIELD orden_2
            IF  z_reg.orden_2 IS NULL THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD orden_2
            ELSE
               IF  z_reg.orden_2 < 1 OR z_reg.orden_2 > 2  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_2
               END IF
               IF  z_reg.orden_2 = z_reg.orden_1 THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_2
               END IF
           END IF

       ON KEY ( ESC )
           IF  z_reg.orden_1 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_1
           ELSE
               IF  z_reg.orden_1 < 1 OR z_reg.orden_1 > 2  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_1
               END IF
           END IF
           IF  z_reg.orden_2 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_2
           ELSE
               IF  z_reg.orden_2 < 1 OR z_reg.orden_2 > 2  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_2
               END IF
               IF  z_reg.orden_2 = z_reg.orden_1 THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_2
               END IF
           END IF

       ON KEY ( INTERRUPT )
           EXIT INPUT
    END INPUT
    CLOSE WINDOW ventana_3
    RETURN
END FUNCTION
############################################################################
FUNCTION desmarca_cuenta(vnss,vmarca,vusuario,vcorrelativo)
#dc-------------------------------------------------------

   DEFINE
      vnss            CHAR(11),
      vmarca          SMALLINT,
      vusuario        CHAR(8),
      vcorrelativo    INTEGER,
      pestado_marca   SMALLINT,
      pmarca_causa    SMALLINT

   LET pestado_marca = 0
   LET pmarca_causa  = 0 

   PREPARE eje_desmarca FROM v_desmarca

   EXECUTE eje_desmarca
   USING vnss,
         vmarca,
         vcorrelativo,
         pestado_marca,
         pmarca_causa,
         vusuario

END FUNCTION 
