###############################################################################
#Proyecto          => AFORE (MEXICO)                                          #
#Propietario       => E.F.P 		                                      #
#Programa CONM001  => CONSULTA DE REGISTROS CONTABLES                         #
#Sistema           => CON  		                                      #
#Autor             => Armando Rodriguez Castroparedes                         #
#Fecha             => 7 mayo 2002                                             #
#Fecha act         => 31 julio 2002                                           #
###############################################################################

DATABASE safre_af
GLOBALS

    DEFINE enter                 CHAR(1),
           g_usuario             CHAR(8),
           aux_pausa             CHAR(01),
           G_LISTA               CHAR(200),
           HORA                  CHAR(08),
           HOY                   DATE,
           vfolio                INTEGER,
           rechazo               SMALLINT,
           g_afore               RECORD LIKE tab_afore_local.*,
           g_paramgrales         RECORD LIKE seg_modulo.*
         
END GLOBALS

MAIN

    OPTIONS PROMPT LINE LAST,
    INPUT WRAP,
    COMMENT LINE LAST
    DEFER INTERRUPT

    CALL inicio()   #i
    CALL proceso_principal()   #pp

END MAIN

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CONM0011" ATTRIBUTE(BORDER)
    DISPLAY " CONM001            CONSULTA DE REGISTROS CONTABLES                               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "CONSULTA "
        COMMAND "Registros" "Consulta Registros contables "
	        CALL consulta()
		     CLEAR FORM
       
        COMMAND "Genera archivo" "Genera archivo contable del dia"
           CALL genera_reporte()
	        CLEAR FORM

        COMMAND KEY(V) "reVersa" "Reversa procesos por dia"
           CALL reversa()
	        CLEAR FORM

        COMMAND "Salir" "Salir de Programa"
           EXIT MENU
    END MENU

END FUNCTION

FUNCTION inicio()
#i---------------

    LET rechazo = 0
    LET HOY = TODAY

    LET HORA = TIME

    SELECT *, USER 
    INTO   g_afore.*, g_usuario 
    FROM   tab_afore_local
	
END FUNCTION
FUNCTION consulta()
#c-----------------
    DEFINE #loc #integer
        arr_c                 ,
        flag                  ,
        i                     INTEGER
    DEFINE x_busca        CHAR(100)
    DEFINE txt_2          CHAR(500)
    DEFINE arr_2  ARRAY[800] OF RECORD
        fecha_emision        DATE,
        proceso_cod          INTEGER,
        desc_proceso         CHAR(20),
        estado               SMALLINT,
        desc_estado          CHAR(10),
        identificador        SMALLINT,
        desc_identificador   CHAR(6),
        movimiento           SMALLINT,
        importe              DECIMAL(15,2)
    END RECORD

    INITIALIZE x_busca TO NULL
    CLEAR FORM

    DISPLAY "    Utilice las flechas para navegar     Control [V] para ver detalle    " AT 5,1
    DISPLAY "   FECHA       P R O C E S O          E S T A D O   T I P O    I M P O R T E    " AT 6,1 ATTRIBUTE(REVERSE)
    DISPLAY "  EMISION    TIPO   DESCRIPCION                                  PESOS / ACC    " AT 7,1 ATTRIBUTE(REVERSE)
    LET int_flag              = FALSE
    
    CONSTRUCT BY NAME x_busca ON fecha_emision,
                                 proceso_cod,
                                 estado

        ON KEY (INTERRUPT) 
            IF int_flag = TRUE THEN    
                LET int_flag=FALSE     
                EXIT CONSTRUCT 
                EXIT PROGRAM 
            END IF 

        ON KEY (Esc)
            LET int_flag = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    LET txt_2 =" SELECT a.fecha_emision, ",
               "a.proceso_cod, ",
               "' ', ",   #desc_proceso
               "a.estado, ",
               "' ', ",   #desc_estado
               "a.identificador, ",
               "' ', ",   #desc_identificador
               "0 ,",
               "0 ",
               " FROM   con_transaccion a ",
               " WHERE  ",x_busca CLIPPED,
               " GROUP BY 1,2,4,6 ",
	       " ORDER BY 2,6 "

    PREPARE pre_1 FROM txt_2
    DECLARE cur_1 CURSOR FOR pre_1

    LET i = 1
    FOREACH cur_1 INTO arr_2[i].*
        IF STATUS <> NOTFOUND THEN

	    SELECT a.descripcion,
                   a.movimiento
	    INTO   arr_2[i].desc_proceso,
                   arr_2[i].movimiento
	    FROM   tab_proceso a
	    WHERE  a.proceso_cod = arr_2[i].proceso_cod

	    SELECT a.descripcion
	    INTO   arr_2[i].desc_estado
	    FROM   con_status a
	    WHERE  a.estado = arr_2[i].estado
	    #WHERE  a.status = arr_2[i].estado

	    IF arr_2[i].identificador = 1 THEN
                LET arr_2[i].desc_identificador = "PESOS"
            ELSE
                LET arr_2[i].desc_identificador = "ACCION"
            END IF

	    IF arr_2[i].movimiento = -1 THEN

                SELECT SUM(importe)
                INTO   arr_2[i].importe
                FROM   con_transaccion
                WHERE  fecha_emision = arr_2[i].fecha_emision
                AND    identificador = arr_2[i].identificador
                AND    proceso_cod   = arr_2[i].proceso_cod
                AND    transaccion_cod NOT IN(
                                      SELECT transaccion_cod
                                      FROM   tab_transaccion
                                      WHERE  descripcion_1 MATCHES "*VIV*")
                                      #WHERE  (descripcion_1 MATCHES "COMIS*"
                                      #OR    descripcion_1 MATCHES "*VIV*"))
            ELSE
                SELECT SUM(importe)
                INTO   arr_2[i].importe
                FROM   con_transaccion
                WHERE  fecha_emision = arr_2[i].fecha_emision
                AND    identificador = arr_2[i].identificador
                AND    proceso_cod   = arr_2[i].proceso_cod
                AND    transaccion_cod NOT IN(
                                      SELECT transaccion_cod
                                      FROM   tab_transaccion
                                      WHERE  descripcion_1 MATCHES "*VIV*")
                                      #WHERE  (descripcion_1 MATCHES "COMIS*"
                                      #OR     descripcion_1 MATCHES "VIV*"))
            END IF

        END IF

        LET i = i + 1
    END FOREACH
    
    IF i = 1 THEN
        #INITIALIZE reg.* TO NULL
        CLEAR FORM
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        --CLOSE WINDOW unim0012
        RETURN
    END IF

    CALL SET_COUNT(i-1)
    DISPLAY ARRAY arr_2 TO scr_2.*
    #INPUT ARRAY arr_2 TO scr_2.*
        ON KEY ( control-v )
        #ON KEY ( control-m )
            LET i = ARR_CURR()
            CALL detalle_registro(arr_2[i].fecha_emision,
                                  arr_2[i].identificador,
                                  arr_2[i].proceso_cod)
        ON KEY ( control-m )
        #ON KEY ( INTERRUPT )
            --INITIALIZE arr_2 TO NULL
            FOR i = 1 TO 6
               DISPLAY arr_2[i].* TO scr_2[i].*
            END FOR
            --EXIT DISPLAY
    END DISPLAY
	  
		  {
        ON KEY ( INTERRUPT )
            #CALL inicializa()
	         EXIT DISPLAY
            END DISPLAY
            CLOSE WINDOW unim0011
				}
    --CLOSE WINDOW unim0012
END FUNCTION
FUNCTION detalle_registro(reg_2)
#c-----------------
    DEFINE #loc #integer
        arr_c                ,
        flag                 ,
        i                    INTEGER
    DEFINE x_busca           CHAR(100)
    DEFINE txt_2             CHAR(500)
    DEFINE aux_pausa         CHAR(01)
    DEFINE arc_2             SMALLINT
    DEFINE scr_2             SMALLINT
    DEFINE reg_2  RECORD
        fecha_emision        DATE,
        identificador        SMALLINT,
        proceso_cod          SMALLINT
    END RECORD
    DEFINE arr_3  ARRAY[200] OF RECORD
        transaccion_cod      INTEGER,
        desc_transaccion     CHAR(33),
        estado               SMALLINT,
        desc_estado          CHAR(10),
        importe              DECIMAL(15,2)
    END RECORD

    INITIALIZE x_busca TO NULL
    --CLEAR FORM

    OPEN WINDOW conm0012 AT 7,2 WITH FORM "CONM0012" ATTRIBUTE(BORDER)
    DISPLAY "   Utilice las flechas para navegar     [Esc] Para grabar modificaciones       " AT 1,1
    DISPLAY "               D E T A L L E   D E   M O V I M I E N T O S                     " AT  2,1 ATTRIBUTE(REVERSE)
    DISPLAY "     T R A N S A C C I O N                    E S T A D O     I M P O R T E    " AT  3,1 ATTRIBUTE(REVERSE)
    LET int_flag              = FALSE
    
    DECLARE cur_3 CURSOR FOR

     SELECT a.transaccion_cod,
            ' ',   #desc_proceso
            a.estado,
            ' ',   #desc_estado
            importe
     FROM  con_transaccion a
     WHERE a.fecha_emision = reg_2.fecha_emision 
     AND   a.identificador = reg_2.identificador
     AND   a.proceso_cod   = reg_2.proceso_cod
     GROUP BY 1,3,5
	  ORDER BY 1

    LET i = 1
    FOREACH cur_3 INTO arr_3[i].*
        IF STATUS <> NOTFOUND THEN

	         SELECT a.descripcion
	         INTO   arr_3[i].desc_estado
	         FROM   con_status a
	         WHERE  a.estado = arr_3[i].estado
	         #WHERE  a.status = arr_3[i].estado

	         SELECT a.descripcion_1
	         INTO   arr_3[i].desc_transaccion
	         FROM   tab_transaccion a
	         WHERE  a.transaccion_cod = arr_3[i].transaccion_cod
            AND    a.proceso_cod     = reg_2.proceso_cod

        END IF

        LET i = i + 1
    END FOREACH
    
    IF i = 1 THEN
        #INITIALIZE reg.* TO NULL
        CLEAR FORM
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW unim0012
        RETURN
    END IF

    CALL SET_COUNT(i-1)
    #DISPLAY ARRAY arr_3 TO scr_3.*
    INPUT ARRAY arr_3 WITHOUT DEFAULTS FROM scr_3.*
        BEFORE FIELD importe
        LET arc_2 = ARR_CURR()
        LET scr_2 = SCR_LINE()

        AFTER FIELD importe
            #IF (arr_3[arc_2].transaccion_cod = 51000 OR
                #arr_3[arc_2].transaccion_cod = 53000 )  THEN
            IF arr_3[arc_2].transaccion_cod = 99991 THEN
#arc
                WHILE TRUE
                    PROMPT "Desea Modificar el Registro S/N ? "
                    FOR CHAR aux_pausa
                    IF aux_pausa MATCHES "[SsNn]" THEN
                        EXIT WHILE
                    END IF
                END WHILE
                IF aux_pausa MATCHES "[Ss]" THEN
                    UPDATE con_transaccion
                    SET    importe          = arr_3[arc_2].importe
                    WHERE  fecha_emision    = reg_2.fecha_emision
                    AND    identificador    = reg_2.identificador
                    AND    proceso_cod      = reg_2.proceso_cod
                    AND    transaccion_cod  = arr_3[arc_2].transaccion_cod

                    ERROR "REGISTRO MODIFICADO"
                    SLEEP 2
                    ERROR ""
                    EXIT INPUT
                ELSE
                    ERROR "MODIFICACION CANCELADA"
                    SLEEP 2
                    ERROR ""
                END IF
            ELSE
                 ERROR "Este monto no es susceptible a modificacion"
                 SLEEP 1
                 --EXIT INPUT
            END IF
        ON KEY ( control-m )
            INITIALIZE arr_3 TO NULL
            FOR i = 1 TO 6
               DISPLAY arr_3[i].* TO scr_3[i].*
            END FOR
            EXIT INPUT
        ON KEY ( INTERRUPT )
            #CALL inicializa()
	         EXIT INPUT
            --END INPUT
    END INPUT
				
    CLOSE WINDOW conm0012
END FUNCTION

FUNCTION despliega_unificados(vfolio,nss_u,vestado)
#dt-------------------------

    DEFINE l_reg ARRAY[10] OF RECORD
           nss_cta1       CHAR(11),
           folio          INTEGER,
           folio_liquida  INTEGER,
           cve_ent_cta1   CHAR(3),
           nombre         CHAR(50),
           estado    	  SMALLINT
    END RECORD

    DEFINE vestado   INTEGER
    DEFINE vfolio    INTEGER
    DEFINE nss_u     CHAR(11)
    DEFINE vpaterno  CHAR(40)
    DEFINE vmaterno  CHAR(40)
    DEFINE vnombre   CHAR(40)
    DEFINE i   	   SMALLINT

    OPEN WINDOW unim0013 AT 9,2 WITH FORM "UNIM0013" ATTRIBUTE(BORDER)
    #DISPLAY "       NSS UNIFICADOS                  " AT 1,1 
    DISPLAY "      NSS          FOLIO       AFORE           NOMBRE                 ESTADO   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "  UNIFICADOS  NOTIFICA LIQUIDA                COMPLETO                         " AT 2,1 ATTRIBUTE(REVERSE)

   DECLARE cursor_1 CURSOR FOR
       SELECT nss_cta1,
              folio,
              folio_liquida,
              cve_ent_cta1,
              "",
              estado
       FROM   uni_unificado
       WHERE  nss_uni = nss_u
       AND    estado  = vestado
       AND    folio   = vfolio 

   LET i = 1
   FOREACH cursor_1 INTO l_reg[i].*
       SELECT paterno_cta1,
              materno_cta1,
              nombre_cta1
       INTO   vpaterno,
              vmaterno,
              vnombre
       FROM   uni_unificado
       WHERE  nss_cta1 = l_reg[i].nss_cta1
       AND    nss_uni = nss_u
       AND    estado  = vestado
       AND    folio   = vfolio 
      
       LET l_reg[i].nombre = vpaterno CLIPPED," ",vmaterno CLIPPED," ",
                             vnombre CLIPPED
       LET i = i + 1
   END FOREACH
   ERROR ""
   CALL SET_COUNT(i-1)
   DISPLAY ARRAY l_reg TO scr_3.*
      ON KEY ( control-m )
         INITIALIZE l_reg TO NULL
         FOR i = 1 TO 3
             DISPLAY l_reg[i].* TO scr_3[i].*
         END FOR
         EXIT DISPLAY
   END DISPLAY
   CLOSE WINDOW unim0013
END FUNCTION #despliega_tipo

FUNCTION genera_reporte()
#gr----------------------
    DEFINE  vfecha_reporte DATE
    DEFINE  vlote          SMALLINT
    DEFINE  vnumero        SMALLINT
    DEFINE  vfolio         INTEGER
    DEFINE  vcopia         CHAR(200)
    DEFINE  cont           SMALLINT
    DEFINE  cont1          SMALLINT
    DEFINE  xfolio         INTEGER 
    DEFINE reg_3  RECORD
        fecha_emision     DATE,
        fecha_valor       DATE,
        identificador     SMALLINT,
        transaccion_cod   INTEGER,
        importe           DECIMAL(15,2),
        proceso_cod       CHAR(05),
	folio             INTEGER
    END RECORD

    OPEN WINDOW ventana_2 AT 6,3 WITH FORM "CONM0013" ATTRIBUTE(BORDER)
    DISPLAY "          [ Esc ] Iniciar                            [ Ctrl-C ] Salir          " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "CONM001         GENERA ARCHIVO DE REGISTRO CONTABLE                            " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT vfecha_reporte FROM FORMONLY.vfecha_reporte

      AFTER FIELD vfecha_reporte
        IF vfecha_reporte IS NULL OR vfecha_reporte = " " THEN
           ERROR " Digite correctamente la Fecha "
           NEXT FIELD vfecha_reporte
        END IF

        SELECT "X"
        FROM   con_transaccion
        WHERE  fecha_emision  = vfecha_reporte
        AND    estado         = 20
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
             ERROR "No hay archivo por generar"
             SLEEP 3
             EXIT PROGRAM
        END IF
        SELECT MAX(lotes_correlativo) + 1
        INTO   vlote
        FROM   tab_lote
        WHERE  lotes_cod   = 15
        AND    lotes_fecha = HOY
    
        IF vlote IS NULL THEN
            LET vlote = 1
        END IF

        INSERT INTO tab_lote VALUES(HOY,15,"CONTABILIDAD",vlote,1)

        SELECT * 
        INTO   g_paramgrales.*
        FROM   seg_modulo
        WHERE  modulo_cod = "con"

        LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,"001"
                 CLIPPED, HOY using "DDMMYY" CLIPPED,"-"CLIPPED,vlote using"&&"
                      CLIPPED,".csv" CLIPPED
        DECLARE cur_4 CURSOR FOR
            SELECT fecha_emision,
                   fecha_valor,
                   identificador,
                   transaccion_cod,
                   sum(importe),
                   proceso_cod,
		   folio,
		   0
            FROM   con_transaccion
            WHERE  fecha_emision   = vfecha_reporte
            AND    estado          = 20
            GROUP BY 1,2,3,4,6,7
            ORDER BY 6,4,3

        START REPORT listado_3 TO G_LISTA
            LET cont = 0
	    LET vnumero = 0
            FOREACH cur_4 INTO reg_3.*,vnumero
	    #arc
		IF (reg_3.proceso_cod >= "00001" AND
		    reg_3.proceso_cod <= "00004") THEN
		     IF reg_3.folio <> xfolio THEN
                         LET cont    = cont + 1
                         LET xfolio  = reg_3.folio
                         LET vnumero = cont
                     ELSE
                         LET vnumero = cont
                     END IF
		     IF  reg_3.proceso_cod = "00002" THEN
			 LET reg_3.proceso_cod = "00001"
                     END IF
		     IF  reg_3.proceso_cod = "00004" THEN
			 LET reg_3.proceso_cod = "00003"
                     END IF
                END IF
                OUTPUT TO REPORT listado_3(reg_3.*,vnumero) #l3
            END FOREACH
        FINISH REPORT listado_3

        UPDATE con_transaccion
        SET    estado        = 40
        WHERE  fecha_emision = vfecha_reporte

#---copia para ING
        LET vcopia = "cp ",G_LISTA CLIPPED," ",
                   g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,"CONSAFRE.csv"
        RUN vcopia

        DISPLAY " ARCHIVO GENERADO EN : ", G_LISTA AT 16,1
        PROMPT " PROCESO FINALIZADO...<ENTER> PARA CONTINUAR "
        FOR enter
        EXIT INPUT

      ON KEY(CONTROL-C)
         ERROR " PROCESO CANCELADO "
         SLEEP 1
         EXIT INPUT

      ON KEY(INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 1
         EXIT INPUT

    END INPUT 
    CLOSE WINDOW ventana_2 

END FUNCTION
REPORT listado_3(reg_3)
#l3--------------------
    DEFINE reg_3 RECORD #glo #reg_3
        fecha_emision     DATE,
        fecha_valor       DATE,
        identificador     SMALLINT,
        transaccion_cod   INTEGER,
        importe           DECIMAL(15,2),
        proceso_cod       CHAR(05),
	folio             INTEGER,
        numero_proceso    SMALLINT
    END RECORD
   
    OUTPUT
        PAGE LENGTH 1
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT 
            COLUMN 001,"001",                                 #origen
            COLUMN 004,",",                                  #separador
            COLUMN 005,reg_3.fecha_emision USING"DDMMYYYY", #fecha_emision
            COLUMN 013,",",                                   #separador
            COLUMN 014,reg_3.fecha_valor   USING"DDMMYYYY", #fecha_emision
            COLUMN 022,",",                                   #separador
            COLUMN 023,reg_3.identificador USING"&",          #identificador
            COLUMN 024,",",                                   #separador
            COLUMN 025,reg_3.transaccion_cod USING"&&&&&",    #transaccion
            COLUMN 030,",",                                   #separador
            COLUMN 031,reg_3.importe*100 USING"&&&&&&&&&&&&&&&",  #importe
            COLUMN 046,",",                                   #separador
            COLUMN 047,reg_3.proceso_cod,                     #proceso
            COLUMN 051,",",                                   #separador
            #COLUMN 052,"0",
            COLUMN 052,reg_3.numero_proceso USING"&",
            COLUMN 053,",",                                   #separador
            COLUMN 054,"1"
END REPORT
FUNCTION reversa()
#c-----------------
    DEFINE vaccion_safre     DECIMAL(15,2)
    DEFINE vprecio_safre     DECIMAL(15,2)
    DEFINE vaccion_tesoreria DECIMAL(15,2)
    DEFINE vprecio_tesoreria DECIMAL(15,2)
    DEFINE vprecio           DECIMAL(10,6)
    DEFINE vdif_concilia     DECIMAL(5,2)
    DEFINE vdif_fraccion     DECIMAL(5,2)
    DEFINE vreverso1         CHAR(01)
    DEFINE vreverso2         CHAR(01)
    DEFINE aux_pausa         CHAR(01)
    DEFINE vproceso          CHAR(05)
    DEFINE vdescripcion      CHAR(40)
    DEFINE vdesc_estado      CHAR(40)
    DEFINE vmovimiento       SMALLINT
    DEFINE vfecha            DATE
    DEFINE vestado           SMALLINT
    DEFINE videntificador    SMALLINT
    DEFINE vtransaccion      SMALLINT
    DEFINE vacciones         SMALLINT
    DEFINE vpesos            SMALLINT
    DEFINE ventero           INTEGER

    LET vproceso       = " "
    LET vdescripcion   = " "
    LET vfolio         = " "
    LET vfecha         = " "
    LET vacciones      = 2
    LET vpesos         = 1

    OPEN WINDOW ventana_3 AT 5,3 WITH FORM "CONM0014" ATTRIBUTE(BORDER)
    DISPLAY "          [ Esc ] Iniciar                            [ Ctrl-C ] Salir          " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "CONM001         REVERSA PROCESOS REGISTRADOS Y CONCILIADOS                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME vproceso,
                  vdescripcion,
                  vfecha,
                  vestado,
                  vdesc_estado,
                  vreverso1,
                  vreverso2   WITHOUT DEFAULTS

        AFTER  FIELD vproceso

           IF vproceso IS NULL THEN
               CALL despliega_tipo()
               RETURNING vproceso,vdescripcion
           ELSE
               SELECT "X"
               FROM   tab_proceso
               WHERE  proceso_cod = vproceso

               IF STATUS <> NOTFOUND THEN
                   SELECT descripcion
                   INTO   vdescripcion
                   FROM   tab_proceso
                   WHERE  proceso_cod = vproceso
               ELSE
                   ERROR "No existe este tipo de proceso" 
                   SLEEP 3
                   NEXT FIELD vproceso
               END IF
           END IF
           DISPLAY "DESCRIPCION DEL PROCESO" AT  7,16 ATTRIBUTE(REVERSE)
           DISPLAY BY NAME vproceso,
                           vdescripcion

        AFTER  FIELD vfecha

           IF vfecha IS NULL THEN
               ERROR "La fecha no puede ser nula"
               SLEEP 2
               ERROR ""
               NEXT FIELD vfecha
           ELSE
               SELECT "X"
               FROM   con_transaccion
               WHERE  proceso_cod = vproceso
               AND    fecha_emision = vfecha
               GROUP BY 1
  
               IF STATUS = NOTFOUND THEN
                   ERROR "No existe proceso con esta fecha"
                   SLEEP 3
                   NEXT FIELD vfecha
               END IF

               SELECT estado
               INTO   vestado
               FROM   con_transaccion
               WHERE  proceso_cod   = vproceso
               AND    fecha_emision = vfecha
               GROUP BY 1

               IF vestado = 40 THEN
               ERROR "Este proceso ya esta Contabilizado y no se puede reversar"
                   SLEEP 3
                   EXIT PROGRAM
               END IF

               SELECT descripcion
               INTO   vdesc_estado
               FROM   con_status
               WHERE  status = vestado

               DISPLAY BY NAME vestado
               DISPLAY BY NAME vdesc_estado

           END IF

        AFTER  FIELD vreverso1

           IF vreverso1 = "N"   THEN
               ERROR "No Requiere Realizar Reverso Total de la Operacion"
               SLEEP 2
               ERROR ""
               NEXT FIELD vreverso2
           END IF
           IF vreverso1 = "S" THEN
               WHILE TRUE
                   PROMPT "Desea Realizar Reverso Total S/N ? "
                   FOR CHAR aux_pausa
                   IF aux_pausa MATCHES "[SsNn]" THEN
                        EXIT WHILE
                   END IF
               END WHILE
               IF aux_pausa MATCHES "[Ss]" THEN
                   ERROR "Procesando Informacion ... Espere un momento"
                       DELETE
                       FROM   con_transaccion
                       WHERE  proceso_cod   = vproceso
                       AND    fecha_emision = vfecha
                       AND    estado        <> 40
  
                   ERROR "EL REVERSO DEL PROCESO SE HA REALIZADO"
                   SLEEP 3
                   ERROR " "
                   EXIT INPUT
               END IF
           END IF
  
        AFTER  FIELD vreverso2

           IF vreverso2 = "N"   THEN
               ERROR "No Requiere Realizar ningun Reverso"
               SLEEP 2
               ERROR ""
               EXIT PROGRAM
           END IF
           IF vreverso2 = "S" THEN
               WHILE TRUE
                   PROMPT "Desea Realizar Reverso Parcial S/N ? "
                   FOR CHAR aux_pausa
                   IF aux_pausa MATCHES "[SsNn]" THEN
                        EXIT WHILE
                   END IF
               END WHILE
               IF aux_pausa MATCHES "[Ss]" THEN
                   CASE vestado
                       WHEN 10
                           DELETE 
                           FROM   con_transaccion
                           WHERE  proceso_cod   = vproceso
                           AND    fecha_emision = vfecha
                           AND    estado        = 10

                       WHEN 20
                           DELETE 
                           FROM   con_transaccion
                           WHERE  proceso_cod   = vproceso
                           AND    fecha_emision = vfecha
                           AND    estado        = 20
                           #AND    transaccion_cod IN (51000,53000)
                           AND    transaccion_cod = 99991
 
                           SELECT MIN(transaccion_cod)
                           INTO   vtransaccion
                           FROM   con_transaccion
                           WHERE  proceso_cod   = vproceso
                           AND    fecha_emision = vfecha
                           AND    estado        = 20

                           SELECT movimiento
                           INTO   vmovimiento
                           FROM   tab_proceso
                           WHERE  proceso_cod = vproceso
  
                           IF vmovimiento = 1 THEN
                               LET videntificador = 2
                               DELETE 
                               FROM   con_transaccion
                               WHERE  proceso_cod     = vproceso
                               AND    fecha_emision   = vfecha
                               AND    estado          = 20
                               AND    transaccion_cod = vtransaccion
                               AND    importe         < 1
                               AND    identificador   = videntificador
                           ELSE
                               LET videntificador = 1
                               DELETE 
                               FROM   con_transaccion
                               WHERE  proceso_cod     = vproceso
                               AND    fecha_emision   = vfecha
                               AND    estado          = 20
                               AND    transaccion_cod = vtransaccion
                               AND    importe         < -1
                               AND    identificador   = videntificador
                           END IF

                           UPDATE con_transaccion
                           SET    estado        = 10 
                           WHERE  proceso_cod   = vproceso
                           AND    fecha_emision = vfecha
                           AND    estado        = 20
  
                   END CASE
               ELSE
                   ERROR "PROCESO CANCELADO"
                   SLEEP 2
                   ERROR " "
                   EXIT INPUT
               END IF
           END IF
                    
     ERROR" REVERSO REALIZADO SATISFACTORIAMENTE"
     SLEEP 3
     ERROR ""
     EXIT INPUT


     ON KEY(CONTROL-C)
         ERROR " PROCESO CANCELADO "
         SLEEP 1
         EXIT INPUT

      ON KEY(INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 1
         EXIT INPUT

    END INPUT 
    CLOSE WINDOW ventana_3

END FUNCTION
FUNCTION  despliega_tipo()
    DEFINE pos        INTEGER
    DEFINE cla_where  CHAR(200)
    DEFINE sel_where  CHAR(200)
    DEFINE vproceso   CHAR(005)
    DEFINE vdescripcion   CHAR(080)
    DEFINE l_record  ARRAY[1000] OF RECORD
        codigo         CHAR(05),
        descripcion    CHAR(80)
    END RECORD

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,18 WITH FORM "CONC0012" ATTRIBUTE( BORDER)
      DISPLAY "    (Enter) Seleccionar                (Ctrl-C) Salir      " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "        Escoja con < ENTER > seleccionar el tipo           " AT 2,1
      DISPLAY "                                                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where   ON proceso_cod 
                          FROM proceso_cod

         ON KEY (CONTROL-M)
            LET int_flag = FALSE
            EXIT CONSTRUCT

         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT * FROM tab_proceso WHERE ",cla_where CLIPPED,
                      "ORDER BY 1,2 "
  
      PREPARE query1 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query1

      LET pos = 1
      FOREACH cursor_2 INTO l_record[pos].*
         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
          CALL SET_COUNT(pos-1) 
          DISPLAY ARRAY l_record TO scr_1.* 

             ON KEY (CONTROL-M)
                LET pos = ARR_CURR()
                LET vproceso     = l_record[pos].codigo
                LET vdescripcion = l_record[pos].descripcion
                EXIT DISPLAY

             ON KEY (CONTROL-C)
                ERROR "Debe Seleccionar un registro"
                LET pos = ARR_CURR()

             ON KEY (INTERRUPT)
                ERROR "Debe Seleccionar un registro"
                LET pos = ARR_CURR()

          END DISPLAY
          CLOSE WINDOW ventana_2
       ELSE
          ERROR "EL PROCESO NO EXISTE"
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
          RETURN
       END IF

    END IF

    RETURN l_record[pos].codigo, l_record[pos].descripcion
END FUNCTION
