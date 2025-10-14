################################################################################
# Proyecto          => SISTEMA DE AFORES( MEXICO )                             #
# Sistema           => INF                                                     #
# Programa INFM001  => DEVOLUCION DE RECURSOS AL INFONAVIT POR APLICACIONES    #
#                      INDEVIDAS OPER.97-98                                    #
# Creado por        => DAVID HERNANDEZ OYARCE.                                 #
# Fecha creacion    => 02 DE DICIEMBRE DEL 2001                                #
# Actualizacion     => FRANCO ESTEBAN ULLOA VIDELA                             #
# Fecha actualiz.   => 25 DE ENERO DEL 2002                                    #
# Actualizacion     => JOSE LUIS SALDIVAR CARDOSO                              #
# fecha actualiz.   => 03 DE JUNIO DEL 2003                                    #
# Actualizacion     => ISAI JIMENEZ ROJAS                                      #
# fecha actualiz.   => 23 DE SEPTIEMBRE 04 (Integracion de participaciones)    #
#                   => Integracion de participaciones y mod. al layout   17:00 #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD #glo #reg_1
           nss                   char(11), --IJRLIKE inf_his_oper97.nss,
           nombre                CHAR(38)                              ,
	   opera_devolucion      CHAR(02)                              ,
	   origen_devolucion     CHAR(03)                              ,
	   desc_devolucion       CHAR(60)                              ,
           fecha_afectacion      LIKE inf_his_oper97.fecha_afectacion  ,
	   cargo_abono           CHAR(01)
           END RECORD		

    DEFINE arr_1 ARRAY[500] OF RECORD #glo #arr_1
           tipo_viv              LIKE inf_his_oper97.tipo_viv          ,
           fecha_valor_trasp     LIKE inf_his_oper97.fecha_valor_trasp ,
           fecha_valor_devol     LIKE inf_his_oper97.fecha_valor_devol ,
           mto_pesos_orig        LIKE inf_his_oper97.mto_pesos_orig    ,
           mto_parti_dev         LIKE inf_his_oper97.mto_parti_dev     ,
           mto_pesos_dev         LIKE inf_his_oper97.mto_pesos_dev     ,
           int_pesos_dev         LIKE inf_his_oper97.int_pesos_dev
           END RECORD

    DEFINE arr_3 ARRAY[50] OF RECORD #loc #arr_3
           nss                   LIKE inf_his_oper97.nss              ,
           nombre                CHAR(38)                             ,
	   opera_devolucion      CHAR(02)                             ,
	   origen_devolucion     CHAR(03)                             ,
           fecha_afectacion      LIKE inf_his_oper97.fecha_afectacion ,
	   cargo_abono           CHAR(01)
           END RECORD

    DEFINE reg_10 RECORD #glo #reg_10
        capturado             LIKE ret_status.status
    END RECORD

    DEFINE HOY                   DATE

    DEFINE #glo #char
	PARAMETRO             CHAR(008) ,
        c12_usuario           CHAR(012) ,
	g_nombres             CHAR(040) ,
        g_paterno             CHAR(040) ,
        g_materno             CHAR(040) ,
	g_n_unico             CHAR(018) ,
        enter	              CHAR(001) ,
	G_texto		      CHAR(200)

    DEFINE #glo #smallint
        s_codigo_afore        ,
        arr_c                 ,
        scr_l                 SMALLINT
    
    DEFINE g_ultimo_folio          LIKE glo_folio.folio
    
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT    WRAP         ,
        PROMPT   LINE LAST    ,
        MESSAGE  LINE LAST-1  ,
        PROMPT   LINE LAST    ,
        ACCEPT   KEY CONTROL-I,
        NEXT     KEY F3       ,
        PREVIOUS KEY F4
        


    CALL init_1() #i1
    OPEN WINDOW infm0011 AT 4,2 WITH FORM "INFM0011" ATTRIBUTE(BORDER)
    DISPLAY " INFM001          DEVOLUCION AL INFONAVIT OPER.97 O OPER.98                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

        MENU "MENU"
	    COMMAND "Agrega" "Agrega Registros"
                     CALL init_4()     #i4
	             CALL agrega()     #a
	             CALL inicializa() #i

	    COMMAND "Consulta" "Consulta Registros"
	             CALL inicializa() #i
		     LET  PARAMETRO = "CONSULTA"
                     CALL parametros_de_consulta() #pdcs
	             CALL inicializa() #i

	    COMMAND "Modifica" "Modifica Registros"
	             CALL inicializa() #i
		     LET  PARAMETRO = "MODIFICA"
                     CALL parametros_de_consulta() #pdc
	             CALL inicializa() #i

	    COMMAND "Elimina" "Elimina Registros"
	             CALL inicializa() #i
		     LET  PARAMETRO = "ELIMINA"
                     CALL parametros_de_consulta() #pdc
	             CALL inicializa() #i

	    COMMAND "Salir" "Salir del programa"
	             CALL inicializa() #i
	             EXIT MENU
	END MENU
END MAIN

FUNCTION init_1()
#i1--------------
    LET HOY = TODAY

    SELECT codigo_afore ,
           USER
    INTO   s_codigo_afore,
           c12_usuario
    FROM   tab_afore_local

    SELECT A.status
    INTO   reg_10.capturado
    FROM   ret_status A
    WHERE  descripcion = "CAPTURADO"
END FUNCTION

----------------------------------------------------------------------
-- Objetivo: Inicializar el arreglo de detalles
----------------------------------------------------------------------
FUNCTION init_3()
#i3--------------
    DEFINE #loc #smallint
        cont_2                SMALLINT

    INITIALIZE arr_1[1].* TO NULL

    FOR cont_2 = 1 TO 5
        DISPLAY arr_1[cont_2].* TO scr_2[cont_2].* 
    END FOR
END FUNCTION

FUNCTION init_4()
#i3--------------
    INITIALIZE reg_1 TO NULL
    DISPLAY BY NAME reg_1.*
END FUNCTION

FUNCTION inicializa()
#i-------------------
    DEFINE #loc #smallint
        ciclo                 SMALLINT

    INITIALIZE reg_1 TO NULL
    DISPLAY BY NAME reg_1.*

    INITIALIZE arr_1 TO NULL
    FOR ciclo = 1 TO 5
        DISPLAY arr_1[ciclo].* TO scr_1[ciclo].*
    END FOR

    LET reg_1.nombre = "                                                     "
    DISPLAY reg_1.nombre AT 5,35
END FUNCTION

FUNCTION pregunta_siono()
#es-------------------
    DEFINE #loc #char
        enter                     CHAR(1)

    WHILE TRUE
        PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[SsNn]" THEN
            IF enter MATCHES "[Ss]" THEN
               RETURN true
            ELSE
                RETURN FALSE
	    END IF
        END IF
    END WHILE
    RETURN TRUE
END FUNCTION

FUNCTION agrega()
#a---------------
    DEFINE msg CHAR(70)

    DISPLAY "" AT 1,1 
    DISPLAY "" AT 2,1 
    DISPLAY " AGREGA " AT 1,71 ATTRIBUTE(REVERSE)
    DISPLAY " [ Ctrl-c ] Salir sin agregar registros" AT 2,1 

    INPUT BY NAME reg_1.* WITHOUT DEFAULTS
        AFTER FIELD nss
            IF reg_1.nss IS NULL THEN
                ERROR "NUMERO DE SEGURIDAD SOCIAL NO PUEDE SER NULO"
                NEXT FIELD nss
            ELSE
                SELECT nombres   ,
                       paterno   ,
                       materno   , 
		       n_unico
                INTO   g_nombres ,
                       g_paterno ,
                       g_materno ,
		       g_n_unico   
                FROM   afi_mae_afiliado
		WHERE  n_seguro = reg_1.nss

		IF STATUS = NOTFOUND THEN
		    ERROR "NUMERO DE SEGURIDAD SOCIAL INEXISTENTE"
		    NEXT FIELD nss
		ELSE
                    LET reg_1.nombre = NULL
		    LET reg_1.nombre = g_nombres CLIPPED," ",
                                       g_paterno CLIPPED," ",
                                       g_materno CLIPPED
	            DISPLAY BY NAME reg_1.nombre
		END IF
            END IF

	AFTER FIELD opera_devolucion
	    IF reg_1.opera_devolucion IS NULL THEN
		ERROR "CAMPO NO PUEDE SER NULO"
		NEXT FIELD opera_devolucion
            ELSE
		IF reg_1.opera_devolucion <> '98' AND
		   reg_1.opera_devolucion <> '97' THEN
		    ERROR  "CAMPO UNICAMENTE PARA OP.98 O 97"
		    NEXT FIELD opera_devolucion
                END IF
            END IF

	AFTER FIELD origen_devolucion
	    IF reg_1.origen_devolucion IS NULL THEN
	        ERROR "CAMPO NO PUEDE SER NULO"
                CALL z_tab_tipo_devol() RETURNING reg_1.origen_devolucion,
		                                  reg_1.desc_devolucion

                DISPLAY BY NAME reg_1.origen_devolucion, reg_1.desc_devolucion
		NEXT FIELD origen_devolucion
            ELSE
                LET reg_1.origen_devolucion = reg_1.origen_devolucion 
					      USING "&&&"
		SELECT desc
		INTO   reg_1.desc_devolucion
		FROM   tab_tipo_devol
		WHERE  clave = reg_1.origen_devolucion 

                IF STATUS = NOTFOUND   THEN
		    ERROR  "EL TIPO DE DEVOLUCION NO EXISTE.."
		    NEXT FIELD origen_devolucion
                ELSE
	            DISPLAY BY NAME reg_1.desc_devolucion
                END IF
	    END IF

	AFTER FIELD fecha_afectacion
	    IF reg_1.fecha_afectacion IS NULL THEN
	        ERROR "FECHA AFECTA NO PUEDE SER NULA"
		NEXT FIELD fecha_afectacion
	    END IF

            IF reg_1.fecha_afectacion >= "08/01/2004" THEn
               DISPLAY "PARTICIPACIONES" AT 18,61 ATTRIBUTE(REVERSE)
            ELSE
               DISPLAY " *** PESOS *** " AT 18,61 ATTRIBUTE(REVERSE)
            END IF

	BEFORE FIELD cargo_abono
	    LET reg_1.cargo_abono = "N"

	AFTER FIELD cargo_abono
            IF reg_1.cargo_abono IS NULL THEN
               ERROR "EL CAMPO NO PUEDE SER NULO"
	       NEXT FIELD cargo_abono 
            END IF  

	    IF UPSHIFT(reg_1.cargo_abono) <> "N"  AND 
	       UPSHIFT(reg_1.cargo_abono) <> "S"  THEN
               ERROR "PARA ESTE CAMPO UNICAMENTE PUEDE SER (N/S)"
	       NEXT FIELD cargo_abono 
            END IF
         
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD fecha_afectacion
            END IF

	    CALL ingresa_detalle() #id
	    NEXT FIELD nss

        ON KEY (CONTROL-C) 
            CALL inicializa()
            EXIT INPUT

    END INPUT
END FUNCTION

FUNCTION ingresa_detalle()
#id-----------------------
    DEFINE ciclo            SMALLINT
    DEFINE vprecio_del_dia  LIKE glo_valor_accion.precio_del_dia
    DEFINE vint_parti_dev   DECIMAL(18,6)

    DISPLAY "[ Esc ] Grabar Registros  [ Ctrl-c ] Salir sin agregar registros"
            AT 2,1 

    INPUT ARRAY arr_1 WITHOUT DEFAULTS FROM scr_1.* ATTRIBUTE(REVERSE)
	BEFORE ROW
	    LET arr_c = ARR_CURR()
	    LET scr_l = SCR_LINE()

	    --DISPLAY arr_1[arr_c].* TO scr_1[scr_l].* 
	    DISPLAY "UD ESTA EN EL REGISTRO (",arr_c USING "&&#",")" AT 18,2 

	AFTER FIELD tipo_viv
	    IF arr_1[arr_c].tipo_viv IS NULL OR
              (arr_1[arr_c].tipo_viv != "92" AND 
               arr_1[arr_c].tipo_viv != "97")THEN 
	       ERROR "IMPORTE VIVIENDA SOLO PUEDE SER 92 o 97"
	       NEXT FIELD tipo_viv
	    END IF

	AFTER FIELD fecha_valor_trasp 
	    IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
	       FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
	       NEXT FIELD tipo_viv
	    END IF

	    IF arr_1[arr_c].fecha_valor_trasp IS NULL THEN
	        ERROR "CAMPO NO PUEDE SER NULO"
		NEXT FIELD fecha_valor_trasp
	    END IF
	    IF arr_1[arr_c].fecha_valor_trasp < "07/02/1997" THEN
	       ERROR "FECHA QUEBRANTO NO PUEDE SER MENOR QUE EL 02 JULIO ",
                     "DE 1997"
	       NEXT FIELD fecha_valor_trasp
	    END IF
            --IJR Validar si existe el precio de accion para este dia
            IF arr_1[arr_c].fecha_valor_trasp >= "08/01/2004" THEN
               IF existe_precio(arr_1[arr_c].fecha_valor_trasp,11)=FALSE THEN
                  ERROR "NO EXISTE PRECIO DE ACCION PARA ESTA FECHA"
	          NEXT FIELD fecha_valor_trasp
               END IF
            END IF

	AFTER FIELD fecha_valor_devol
	    IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
	       FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
	       NEXT FIELD fecha_valor_trasp
	    END IF
	      
	    IF arr_1[arr_c].fecha_valor_devol IS NULL THEN
	        ERROR "CAMPO NO PUEDE SER NULO"
		NEXT FIELD fecha_valor_devol
	    END IF
	    IF arr_1[arr_c].fecha_valor_devol < "07/02/1997" THEN
		    ERROR "FECHA DEVOLUCION NO PUEDE SER MENOR QUE EL 02 ",
                          "JULIO DE 1997"
		    NEXT FIELD fecha_valor_devol
            END IF
            IF arr_1[arr_c].fecha_valor_devol < primer_dia_mes(TODAY) THEN
	       ERROR "FECHA DEVOLUCION NO PUEDE SER MENOR AL ",
                     "PRIMER DIA DEL MES ACTUAL "
	       NEXT FIELD fecha_valor_devol
	    END IF
            --IJR Validar si existe el precio de accion para este dis
            IF existe_precio(arr_1[arr_c].fecha_valor_devol,11)=FALSE THEN
               ERROR "NO EXISTE PRECIO DE ACCION PARA ESTA FECHA"
	       NEXT FIELD fecha_valor_devol
            END IF

            IF arr_1[arr_c].fecha_valor_trasp >= "08/01/2004" THEN
               NEXT FIELD mto_parti_dev
            ELSE
               NEXT FIELD mto_pesos_orig
            END IF

         ---------------------------------------------------------------
         -- Se calcula equivalencia en Participaciones a partir de pesos 
         ---------------------------------------------------------------
         AFTER FIELD mto_pesos_orig
	    IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
	       FGL_LASTKEY() = FGL_KEYVAL("LEFTT") THEN
	       NEXT FIELD fecha_valor_devol
	    END IF

            CALL calcula_participaciones(arr_1[arr_c].fecha_valor_trasp,
                                         arr_1[arr_c].fecha_valor_devol,
                                         arr_1[arr_c].mto_pesos_orig)
                RETURNING arr_1[arr_c].mto_parti_dev,
                          arr_1[arr_c].mto_pesos_dev ,
                          arr_1[arr_c].int_pesos_dev
    
            NEXT FIELD mto_pesos_dev

         ---------------------------------------------------------------
         -- Se calcula equivalencia en Pesos a partir de Participaciones 
         ---------------------------------------------------------------
         AFTER FIELD mto_parti_dev

            CALL calcula_pesos(arr_1[arr_c].fecha_valor_trasp, #cp
                               arr_1[arr_c].fecha_valor_devol,
                               arr_1[arr_c].mto_parti_dev)
                RETURNING arr_1[arr_c].mto_pesos_orig,
                          arr_1[arr_c].mto_pesos_dev ,
                          arr_1[arr_c].int_pesos_dev
    
            NEXT FIELD mto_pesos_dev
           

	AFTER ROW
	    DISPLAY arr_1[arr_c].* TO scr_1[scr_l].* ATTRIBUTE(REVERSE)

        ON KEY (esc)
	    IF esta_seguro() THEN
	        MESSAGE " AGREGANDO REGISTROS" SLEEP 1
		FOR ciclo = 1 TO 500
		    IF arr_1[ciclo].tipo_viv           IS NOT NULL AND
	               arr_1[ciclo].fecha_valor_trasp  IS NOT NULL AND
	               arr_1[ciclo].fecha_valor_devol  IS NOT NULL AND
		       arr_1[ciclo].mto_pesos_orig     IS NOT NULL AND
		       arr_1[ciclo].mto_parti_dev      IS NOT NULL AND
	               arr_1[ciclo].mto_pesos_dev      IS NOT NULL AND
	               arr_1[ciclo].int_pesos_dev      IS NOT NULL THEN

                       CALL precio_del_dia(arr_1[ciclo].fecha_valor_devol,11)
                            RETURNING vprecio_del_dia
                            
		       LET vint_parti_dev = ( (arr_1[ciclo].mto_parti_dev * 
		                               vprecio_del_dia ) - 
		                               arr_1[ciclo].mto_pesos_orig) /
                                               vprecio_del_dia 
                       {
                       LET vint_parti_dev = arr_1[ciclo].int_pesos_dev /
                                            vprecio_del_dia 
                       }
                       -- Recupera y asigna el último folio
                       SELECT MAX(folio)+1
                         INTO g_ultimo_folio
                         FROM glo_folio
                       INSERT INTO glo_folio VALUES (g_ultimo_folio)
                       
                       -- Insert pot Agrega
		       INSERT INTO inf_his_oper97 
			   VALUES (g_ultimo_folio                   ,#folio
	                           reg_1.nss                      ,
                                   g_n_unico                      ,#n_unico
				   reg_1.opera_devolucion         ,
                                   reg_1.origen_devolucion        ,   
                                   ""                             ,#remanente
	                           arr_1[ciclo].tipo_viv          ,
		                   arr_1[ciclo].mto_pesos_orig    ,
		                   arr_1[ciclo].mto_pesos_dev     ,
		                   arr_1[ciclo].int_pesos_dev     ,
		                   arr_1[ciclo].mto_parti_dev     ,
		                   vint_parti_dev     ,
				   reg_1.cargo_abono              ,
	                           reg_1.fecha_afectacion         ,
	                           arr_1[ciclo].fecha_valor_trasp ,
	                           arr_1[ciclo].fecha_valor_devol ,
                                   HOY                            ,#fecha_ult_pr
                                   reg_10.capturado               ,#estado
                                   c12_usuario                     #usuario
                                  )
		    END IF
		END FOR
	    ELSE
		MESSAGE "NO SE AGREGARON REGISTROS" 
	    END IF
	    SLEEP 1
	    MESSAGE ""
	    CALL inicializa()
	    EXIT INPUT
    END INPUT

END FUNCTION

{===========================================================================}
{ Objetivo : Permite establecer los criterios de consulta para las opciones }
{            de Consultar, Modificar o Eliminar                             }
{             Cargando el arreglo con la informacion seleccionada           }
{===========================================================================}  

FUNCTION parametros_de_consulta()
#pdc-----------------------------
    DEFINE txt_1     CHAR(400) 
    DEFINE x_busca   CHAR(300)
    DEFINE sw_1      SMALLINT
    DEFINE cont_1    SMALLINT

    OPEN WINDOW infm0012 AT 4,2 WITH FORM "INFM0011" ATTRIBUTE(BORDER)
    DISPLAY " INFM001             DEVOLUCION AL INFONAVIT OPER.97 o OPER.98                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    DISPLAY "" AT 1,1 
    DISPLAY "" AT 2,1 
    DISPLAY " ",PARAMETRO," " AT 1,69 ATTRIBUTE(REVERSE)
    DISPLAY "[ ESC ] Consultar                            [ Ctrl-c ] Cancelar " AT 2,1 

    INITIALIZE arr_3 TO NULL
    
    LET sw_1 = 0
    WHILE TRUE
        MESSAGE  "PROPORCIONE SU CRITERIO DE BUSQUEDA"  ATTRIBUTE(REVERSE)
        CONSTRUCT BY NAME x_busca ON nss, opera_devolucion, origen_devolucion, 
				     fecha_afectacion,cargo_abono
            ON KEY (ESC)
                LET INT_FLAG = FALSE
                EXIT CONSTRUCT

            ON KEY (INTERRUPT)
                LET sw_1 = 1
                EXIT CONSTRUCT
        END CONSTRUCT
        MESSAGE  ""

        IF sw_1 = 1 THEN
            EXIT WHILE
        END IF

        LET txt_1 = " SELECT nss               ,",
                    "        ''                ,",
		    "        opera_devolucion  ,",
                    "        origen_devolucion ,",
                    "        fecha_afectacion  ,",
		    "        cargo_abono        ",
                    " FROM   inf_his_oper97     ",
                    " WHERE ",x_busca CLIPPED    ,
                    "   AND estado = '0' "       ,
                    " GROUP BY 1,2,3,4,5,6"

        PREPARE pre_1 FROM txt_1
        DECLARE cur_2 CURSOR FOR pre_1
  
        LET cont_1 = 1
        FOREACH cur_2 INTO arr_3[cont_1].* 
            LET cont_1 = cont_1 + 1
        END FOREACH

        LET cont_1 = cont_1 - 1

        IF cont_1 = 0 THEN
           MESSAGE "NO HAY INFORMACION CON ESE CRITERIO"
           SLEEP 2
           MESSAGE ""
           CONTINUE WHILE
        END IF

        CALL SET_COUNT(cont_1)
        
        DISPLAY "[Enter] Ver detalle                          [ Ctrl-c ] Cancelar " AT 2,1 

        INPUT ARRAY arr_3 WITHOUT DEFAULTS FROM scr.*
            BEFORE FIELD nss
                LET arr_c = ARR_CURR()

            ON KEY (CONTROL-M)
		CASE PARAMETRO
		    WHEN "CONSULTA"
                        CALL despliega_detalle(arr_3[arr_c].nss               ,
					       arr_3[arr_c].opera_devolucion  ,
					       arr_3[arr_c].origen_devolucion ,
                                               arr_3[arr_c].fecha_afectacion  ,
					       arr_3[arr_c].cargo_abono
                                              )#dd
                        IF INT_FLAG THEN
                           LET INT_FLAG = FALSE
                           EXIT INPUT
                        END IF

		    WHEN "MODIFICA"
                        CALL modifica_detalle(arr_3[arr_c].nss               ,
					      arr_3[arr_c].opera_devolucion  ,
					      arr_3[arr_c].origen_devolucion ,
                                              arr_3[arr_c].fecha_afectacion  ,
					      arr_3[arr_c].cargo_abono
                                             )#md
                        EXIT INPUT

		    WHEN "ELIMINA"
                        CALL elimina_detalle(arr_3[arr_c].nss               ,
					     arr_3[arr_c].opera_devolucion  ,
					     arr_3[arr_c].origen_devolucion ,
                                             arr_3[arr_c].fecha_afectacion  ,
					     arr_3[arr_c].cargo_abono
                                            )#ed
                        EXIT INPUT
		END CASE

            ON KEY (INTERRUPT)
                EXIT INPUT
		#ff
        END INPUT
    END WHILE
    CLOSE WINDOW infm0012
END FUNCTION

{=============================================================================}
{ Objetivo : Permitir seleccionar los detalles en base a los parametros       }
{            proporcionados                                                   }
{=============================================================================}
FUNCTION despliega_detalle(reg_4)
#dd------------------------------

    DEFINE reg_4 RECORD #loc #reg_4
           nss                   LIKE inf_his_oper97.nss              ,
	   opera_devolucion      CHAR(02)                             ,
	   origen_devolucion     CHAR(03)                             ,
           fecha_afectacion      LIKE inf_his_oper97.fecha_afectacion ,
	   cargo_abono           CHAR(01)
           END RECORD
            
    DEFINE arr_1 ARRAY[500] OF RECORD #glo #arr_1
           tipo_viv              LIKE inf_his_oper97.tipo_viv          ,
           fecha_valor_trasp     LIKE inf_his_oper97.fecha_valor_trasp ,
           fecha_valor_devol     LIKE inf_his_oper97.fecha_valor_devol ,
           mto_pesos_orig        LIKE inf_his_oper97.mto_pesos_orig    ,
           mto_parti_dev         LIKE inf_his_oper97.mto_parti_dev     ,
           mto_pesos_dev         LIKE inf_his_oper97.mto_pesos_dev     ,
           int_pesos_dev         LIKE inf_his_oper97.int_pesos_dev      
           END RECORD

    DEFINE cont_1                SMALLINT
    DEFINE ciclo                 SMALLINT

    -- Inicio de la Funcion 
    
    CALL init_3() #i3

    DECLARE cur_1 CURSOR FOR
    SELECT tipo_viv          ,
           fecha_valor_trasp ,
           fecha_valor_devol ,
           mto_pesos_orig    ,
           mto_parti_dev     ,
           mto_pesos_dev     ,
           int_pesos_dev  
    FROM   inf_his_oper97
    WHERE  nss               = reg_4.nss
    AND    opera_devolucion  = reg_4.opera_devolucion
    AND    origen_devolucion = reg_4.origen_devolucion
    AND    fecha_afectacion  = reg_4.fecha_afectacion

    LET ciclo = 1

    FOREACH cur_1 INTO arr_1[ciclo].*
        LET ciclo = ciclo + 1
    END FOREACH

    CALL SET_COUNT(ciclo-1)

    DISPLAY "[ Esc ] Salir                                [ Ctrl-c ] Cancelar " AT 2,1 

    DISPLAY ARRAY arr_1 TO scr_2.* ATTRIBUTE(REVERSE)
        ON KEY ( ESC )
           EXIT DISPLAY
        ON KEY ( INTERRUPT )
	   EXIT DISPLAY
    END DISPLAY

    --Limpia el arreglo de detalles en pantalla
    CALL init_3() #i3

    --Restaura el mensaje anterior
    DISPLAY "[Enter] Ver detalle                          [ Ctrl-c ] Cancelar " AT 2,1 
    
END FUNCTION


FUNCTION modifica_detalle(reg_4)
#md-----------------------------
    DEFINE reg_4 RECORD #loc #reg_4
           nss                   LIKE inf_his_oper97.nss               ,
	   opera_devolucion      LIKE inf_his_oper97.opera_devolucion  ,
	   origen_devolucion     LIKE inf_his_oper97.origen_devolucion ,
           fecha_afectacion      LIKE inf_his_oper97.fecha_afectacion  ,
	   cargo_abono           CHAR(01)
           END RECORD		

    DEFINE cont_1                SMALLINT
    DEFINE ciclo                 SMALLINT

    CALL init_3() #i3

    DECLARE cur_3 CURSOR FOR
    SELECT tipo_viv          ,
           fecha_valor_trasp ,
           fecha_valor_devol ,
           mto_pesos_orig    ,
           mto_parti_dev     ,
           mto_pesos_dev     ,
           int_pesos_dev
    FROM   inf_his_oper97
    WHERE  nss               = reg_4.nss
  --AND    opera_devolucion  = reg_4.opera_devolucion
  --AND    origen_devolucion = reg_4.origen_devolucion
    AND    fecha_afectacion  = reg_4.fecha_afectacion


    LET ciclo = 1
    FOREACH cur_3 INTO arr_1[ciclo].*
        LET ciclo = ciclo + 1
    END FOREACH

    LET ciclo = ciclo - 1
    CALL SET_COUNT(ciclo)

    CALL ingresa_detalle2(reg_4.*) #id2

    CALL init_3()
    
END FUNCTION

FUNCTION elimina_detalle(reg_4)
#ed----------------------------
    DEFINE reg_4                 RECORD
           nss                   LIKE inf_his_oper97.nss               ,
	   opera_devolucion      LIKE inf_his_oper97.opera_devolucion  ,
	   origen_devolucion     LIKE inf_his_oper97.origen_devolucion ,
           fecha_afectacion      LIKE inf_his_oper97.fecha_afectacion  , 
	   cargo_abono           CHAR(01)
           END RECORD		

    DEFINE sw_1                  SMALLINT
    DEFINE cont_1                SMALLINT
    DEFINE ciclo                 SMALLINT

    DISPLAY "[ Enter ] Eliminar Registro  [ Ctrl-c ] Salir sin eliminar registro"
            AT 2,1 

    WHILE TRUE  -- Para que sean eliminados tantos registros se requieran
       CALL init_3() #i3  -- Limpia el arreglo de detalles

       DECLARE cur_4 CURSOR FOR
       SELECT tipo_viv          ,
              fecha_valor_trasp ,
              fecha_valor_devol ,
              mto_pesos_orig    ,
              mto_parti_dev     ,
              mto_pesos_dev     ,
              int_pesos_dev  
       FROM   inf_his_oper97
       WHERE  nss               = reg_4.nss
       AND    opera_devolucion  = reg_4.opera_devolucion
       AND    origen_devolucion = reg_4.origen_devolucion
       AND    fecha_afectacion  = reg_4.fecha_afectacion

       LET ciclo = 1
       FOREACH cur_4 INTO arr_1[ciclo].*
           LET ciclo = ciclo + 1
       END FOREACH
       
       LET ciclo = ciclo-1
       
       CALL SET_COUNT(ciclo)
       
       IF ciclo = 0 THEN
          MESSAGE  "NO HAY MAS REGISTROS POR ELIMINAR"
          SLEEP 2
          MESSAGE  ""
          EXIT WHILE
       END IF
   
       LET sw_1 = 0
       
       DISPLAY ARRAY arr_1 TO scr_2.* ATTRIBUTE(REVERSE)
           ON KEY (CONTROL-M)
               LET arr_c = ARR_CURR()
   
               IF esta_seguro() = TRUE THEN	       

                  --Elimina el Registro Actual   
                  DELETE FROM inf_his_oper97
                   WHERE nss              = reg_4.nss
                     AND fecha_afectacion = reg_4.fecha_afectacion
                     AND mto_pesos_orig   = arr_1[arr_c].mto_pesos_orig
                     AND mto_parti_dev    = arr_1[arr_c].mto_parti_dev
                     
                  IF SQLCA.SQLCODE != 0 THEN
                     ERROR "NO PUDO ELIMINARSE EL REGISTRO, VERIFIQUE"
                  ELSE
	             MESSAGE "REGISTRO ELIMINADO ..." 
                     SLEEP 2
	             MESSAGE ""
	          END IF
                  INITIALIZE arr_3 TO NULL
                  EXIT DISPLAY
               END IF
           ON KEY (INTERRUPT)
               LET sw_1 = 1
               EXIT DISPLAY
       END DISPLAY
   
       INITIALIZE arr_1 TO NULL
   
       IF sw_1 = 1 THEN
           EXIT WHILE
       END IF
    END WHILE

    CALL init_3() #i3  -- Limpia el arreglo de detalles

END FUNCTION


FUNCTION ingresa_detalle2(reg_4)
#id2----------------------------
    DEFINE reg_4 RECORD #loc #reg_4
           nss                   LIKE inf_his_oper97.nss               ,
	   opera_devolucion      LIKE inf_his_oper97.opera_devolucion  ,
	   origen_devolucion     LIKE inf_his_oper97.origen_devolucion ,
           fecha_afectacion      LIKE inf_his_oper97.fecha_afectacion  ,
	   cargo_abono           CHAR(01)
           END RECORD		

    DEFINE j                     SMALLINT
    DEFINE ciclo                 SMALLINT
    DEFINE vprecio_del_dia       LIKE glo_valor_accion.precio_del_dia
    DEFINE vint_parti_dev        DECIMAL(18,6)

    DISPLAY "[ Esc ] Grabar Registros  [ Ctrl-c ] Salir sin agregar registros"
            AT 2,1 

    INPUT ARRAY arr_1 WITHOUT DEFAULTS FROM scr_2.* ATTRIBUTE(REVERSE)
	BEFORE ROW
	    LET arr_c = ARR_CURR()
	    LET scr_l = SCR_LINE()
	    --DISPLAY arr_1[arr_c].* TO scr_2[scr_l].* 

	    DISPLAY "UD ESTA EN EL REGISTRO (",arr_c USING "&&#",")" AT 18,2

	AFTER FIELD tipo_viv
            IF arr_1[arr_c].tipo_viv IS NULL OR
              (arr_1[arr_c].tipo_viv != "92" AND
               arr_1[arr_c].tipo_viv != "97")THEN
               ERROR "IMPORTE VIVIENDA SOLO PUEDE SER 92 o 97"
               NEXT FIELD tipo_viv
            END IF

	AFTER FIELD fecha_valor_trasp
	    IF arr_1[arr_c].fecha_valor_trasp IS NULL THEN
	        ERROR "CAMPO NO PUEDE SER NULO"
		NEXT FIELD fecha_valor_trasp
	    END IF
	    IF arr_1[arr_c].fecha_valor_trasp < "07/02/1997" THEN
	       ERROR "FECHA DEVOLUCION NO PUEDE SER MENOR QUE EL 02 ",
                     "JULIO DE 1997"
	       NEXT FIELD fecha_valor_trasp
	    END IF
            --IJR Validar si existe el precio de accion para este dia
            IF arr_1[arr_c].fecha_valor_trasp >= "08/01/2004" THEN
               IF existe_precio(arr_1[arr_c].fecha_valor_trasp,11)=FALSE THEN
                  ERROR "NO EXISTE PRECIO DE ACCION PARA ESTA FECHA"
                  NEXT FIELD fecha_valor_trasp
               END IF
            END IF
        AFTER FIELD fecha_valor_devol
            IF arr_1[arr_c].fecha_valor_devol IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD fecha_valor_devol
            END IF
            IF arr_1[arr_c].fecha_valor_devol < "07/02/1997" THEN
                    ERROR "FECHA DEVOLUCION NO PUEDE SER MENOR QUE EL 02 ",
                          "JULIO DE 1997"
                    NEXT FIELD fecha_valor_devol
            END IF
            IF arr_1[arr_c].fecha_valor_devol < primer_dia_mes(TODAY) THEN
               ERROR "FECHA DEVOLUCION NO PUEDE SER MENOR AL ",
                     "PRIMER DIA DEL MES ACTUAL "
               NEXT FIELD fecha_valor_devol
            END IF
            --IJR Validar si existe el precio de accion para este dis
            IF existe_precio(arr_1[arr_c].fecha_valor_devol,11)=FALSE THEN
               ERROR "NO EXISTE PRECIO DE ACCION PARA ESTA FECHA"
               NEXT FIELD fecha_valor_devol
            END IF

            IF arr_1[arr_c].fecha_valor_trasp >= "08/01/2004" THEN
               NEXT FIELD mto_parti_dev
            ELSE
               NEXT FIELD mto_pesos_orig
            END IF

         ---------------------------------------------------------------
         -- Se calcula equivalencia en Participaciones a partir de pesos
         ---------------------------------------------------------------
         AFTER FIELD mto_pesos_orig

            CALL calcula_participaciones(arr_1[arr_c].fecha_valor_trasp,
                                         arr_1[arr_c].fecha_valor_devol,
                                         arr_1[arr_c].mto_pesos_orig)
                RETURNING arr_1[arr_c].mto_parti_dev,
                          arr_1[arr_c].mto_pesos_dev ,
                          arr_1[arr_c].int_pesos_dev

            NEXT FIELD mto_pesos_dev

         ---------------------------------------------------------------
         -- Se calcula equivalencia en Pesos a partir de Participaciones
         ---------------------------------------------------------------
         AFTER FIELD mto_parti_dev

            CALL calcula_pesos(arr_1[arr_c].fecha_valor_trasp, #cp
                               arr_1[arr_c].fecha_valor_devol,
                               arr_1[arr_c].mto_parti_dev)
                RETURNING arr_1[arr_c].mto_pesos_orig,
                          arr_1[arr_c].mto_pesos_dev ,
                          arr_1[arr_c].int_pesos_dev

            NEXT FIELD mto_pesos_dev

	AFTER ROW
	    DISPLAY arr_1[arr_c].* TO scr_1[scr_l].* ATTRIBUTE(REVERSE)


        ON KEY (esc)
	    IF esta_seguro() THEN
	        MESSAGE " MODIFICANDO REGISTROS" SLEEP 1

		DELETE
		FROM  inf_his_oper97
		WHERE  nss               = reg_4.nss
		AND    opera_devolucion  = reg_4.opera_devolucion
		AND    origen_devolucion = reg_4.origen_devolucion
		AND    fecha_afectacion  = reg_4.fecha_afectacion

                MESSAGE "APLICANDO CAMBIOS"

		FOR ciclo = 1 TO 100
                    IF arr_1[ciclo].tipo_viv           IS NOT NULL AND
                       arr_1[ciclo].fecha_valor_trasp  IS NOT NULL AND
                       arr_1[ciclo].fecha_valor_devol  IS NOT NULL AND
                       arr_1[ciclo].mto_pesos_orig     IS NOT NULL AND
                       arr_1[ciclo].mto_parti_dev      IS NOT NULL AND
                       arr_1[ciclo].mto_pesos_dev      IS NOT NULL AND
                       arr_1[ciclo].int_pesos_dev      IS NOT NULL THEN

                       CALL precio_del_dia(arr_1[ciclo].fecha_valor_devol,11)
                            RETURNING vprecio_del_dia
                            
                       LET vint_parti_dev = arr_1[ciclo].int_pesos_dev /
                                            vprecio_del_dia

                       -- Recupera y asigna el último folio
                       SELECT MAX(folio)+1
                         INTO g_ultimo_folio
                         FROM glo_folio
                       INSERT INTO glo_folio VALUES (g_ultimo_folio)
                       
                       INSERT INTO inf_his_oper97
                           VALUES (g_ultimo_folio                   ,#folio
                                   reg_4.nss                      ,
                                   g_n_unico                      ,#n_unico
                                   reg_4.opera_devolucion         ,
                                   reg_4.origen_devolucion        ,
                                   ""                             ,#remanente
                                   arr_1[ciclo].tipo_viv          ,
                                   arr_1[ciclo].mto_pesos_orig    ,
                                   arr_1[ciclo].mto_pesos_dev     ,
                                   arr_1[ciclo].int_pesos_dev     ,
                                   arr_1[ciclo].mto_parti_dev    ,
                                   vint_parti_dev     ,
                                   reg_4.cargo_abono              ,
                                   reg_4.fecha_afectacion         ,
                                   arr_1[ciclo].fecha_valor_trasp ,
                                   arr_1[ciclo].fecha_valor_devol ,
                                   HOY                            ,#fecha_ult_pr
                                   reg_10.capturado               ,#estado
                                   c12_usuario                     #usuario
                                  )

		    END IF
		END FOR
                SLEEP 2
                MESSAGE ""
	    ELSE
		MESSAGE "NO SE AGREGARON REGISTROS" 
	    END IF
	    SLEEP 1
	    MESSAGE ""
	    CALL inicializa()
	    EXIT INPUT
    END INPUT

END FUNCTION


-- REGRESA FECHA CON EL PRIMER DIA DEL MES Y ANO RECIBIDO EN LA FECHA 
-- COMO ARGUMENTO

FUNCTION primer_dia_mes(vfecha )

   DEFINE vfecha DATE
   DEFINE vdia   SMALLINT
   DEFINE vmes   SMALLINT
   DEFINE vanio  SMALLINT

   LET vdia = 1
   LET vmes = MONTH(vfecha)
   LET vanio= YEAR(vfecha)

   LET vfecha = MDY(vmes,vdia,vanio)
   
   RETURN vfecha
END FUNCTION

{====================================================================}
{  CALCULA PARTICIPACIONES A PARTIR DE LOS PESOS                     }
{  Entra: pesos                                                      }
{  Sale : participaciones a devolver y pesos a devolver              }
{====================================================================}
FUNCTION calcula_participaciones(vfecha_valor_trasp,
                                 vfecha_valor_devol,
                                 vmto_pesos_orig)

   DEFINE vfecha_valor_trasp  LIKE inf_his_oper97.fecha_valor_trasp 
   DEFINE vfecha_valor_devol  LIKE inf_his_oper97.fecha_valor_devol
   DEFINE vmto_pesos_orig     LIKE inf_his_oper97.mto_pesos_orig 

   DEFINE vmto_parti_dev      LIKE inf_his_oper97.mto_parti_dev
   DEFINE vmto_pesos_dev      LIKE inf_his_oper97.mto_pesos_dev 
   DEFINE vint_pesos_dev      LIKE inf_his_oper97.int_pesos_dev 

   DEFINE vprecio_del_dia     LIKE glo_valor_accion.precio_del_dia
   DEFINE vinst               CHAR(200)
   DEFINE vfecha_aux          DATE
   
   -- Obtendion del precio de la accion a la fecha de devolucion

   SELECT precio_del_dia
     INTO vprecio_del_dia
     FROM glo_valor_accion
    WHERE fecha_valuacion = vfecha_valor_devol
      AND codigo_siefore  = 11 

   IF SQLCA.SQLCODE = NOTFOUND THEN
      ERROR "NO EXISTE PRECIO DE ACCION PARA ESTA FECHA"
      RETURN 0
   END IF

   --------------------------------------------------------------
   -- PASO 1
   -- Se calcula  correspondiente en participaciones
   -- Desde la fecha de traspaso hasta  el 01 de Agosto del 04
   --------------------------------------------------------------

   LET vfecha_aux = "08/01/2004"
   LET vinst = " EXECUTE PROCEDURE fn_cambia_aporte(?,?,?) "

   PREPARE exe_spl FROM vinst
   DECLARE cur_spl CURSOR FOR exe_spl
   OPEN cur_spl USING vmto_pesos_orig,
                      vfecha_valor_trasp, 
                      vfecha_aux
                   
   FETCH cur_spl INTO vmto_parti_dev
   CLOSE cur_spl

   -- Importes a Devolver

   LET vmto_pesos_dev = vmto_parti_dev * vprecio_del_dia
   LET vint_pesos_dev = vmto_pesos_dev - vmto_pesos_orig

   RETURN vmto_parti_dev ,
          vmto_pesos_dev  ,
          vint_pesos_dev

END FUNCTION

{====================================================================}
{  CALCULA PESOS A PARTIR DE LAS PARTICIPACIONES                     }
{ Entran: Participaciones                                            }
{ salen : Pesos                                                      }
{====================================================================}
#cp
FUNCTION calcula_pesos(vfecha_valor_trasp,
                       vfecha_valor_devol,
                       vmto_parti_dev)

   DEFINE vfecha_valor_trasp  LIKE inf_his_oper97.fecha_valor_trasp 
   DEFINE vfecha_valor_devol  LIKE inf_his_oper97.fecha_valor_devol
   DEFINE vmto_parti_dev      LIKE inf_his_oper97.mto_parti_dev

   DEFINE vmto_pesos_orig     LIKE inf_his_oper97.mto_pesos_orig 
   DEFINE vmto_pesos_dev      LIKE inf_his_oper97.mto_pesos_dev 
   DEFINE vint_pesos_dev      LIKE inf_his_oper97.int_pesos_dev 

   
   LET vmto_pesos_orig = vmto_parti_dev * precio_del_dia(vfecha_valor_trasp,11)
   LET vmto_pesos_dev  = vmto_parti_dev * precio_del_dia(vfecha_valor_devol,11)
   LET vint_pesos_dev  = vmto_pesos_dev - vmto_pesos_orig

   RETURN vmto_pesos_orig ,
          vmto_pesos_dev  ,
          vint_pesos_dev

END FUNCTION

--=======================================================================
-- VALIDA SI EXISTE PRECIO DE LA ACCION PARA EL DIA Y SIEFORE RECIBIDOS
--=======================================================================
FUNCTION existe_precio(vfecha,vcodigo_siefore)

   DEFINE vfecha          DATE
   DEFINE vcodigo_siefore LIKE glo_valor_accion.codigo_siefore

   SELECT "X"
     FROM glo_valor_accion
    WHERE fecha_valuacion = vfecha
      AND codigo_siefore  = vcodigo_siefore 

   IF SQLCA.SQLCODE = NOTFOUND THEN
      RETURN FALSE
   ELSE 
      RETURN TRUE
   END IF

END FUNCTION
FUNCTION esta_seguro()
#es-------------------
    DEFINE #loc #char
        enter                     CHAR(1)

    WHILE TRUE
        DISPLAY "                                 " AT 18,2
        PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[SsNn]" THEN
            IF enter MATCHES "[Ss]" THEN
                EXIT WHILE
            ELSE
                RETURN FALSE
            END IF
        END IF
    END WHILE
    RETURN TRUE
END FUNCTION

