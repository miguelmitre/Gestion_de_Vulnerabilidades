#########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                      #
#Propietario       => E.F.P.                                            #
#Programa AFIB004  => ACTUALIZA SALARIOS DE CANASE.                     #
#Sistema           => AFI. 					        #
#Por               => MAURO MUNIZ CABALLERO                             #
#Fecha             => 17 DE ENERO DE 2001                               #
#########################################################################
DATABASE safre_af
GLOBALS
    	DEFINE aux_pausa	   CHAR(1)
    	DEFINE aux_pausa1	   CHAR(1)
    	DEFINE HOY	   	   DATE
    	DEFINE opcion	   	   CHAR(2)
    	DEFINE HORA	   	   CHAR(8)
    	DEFINE ruta	   	   CHAR(180)
    	DEFINE txt	   	   CHAR(180)
    	DEFINE corre	   	   CHAR(180)
        DEFINE sub   RECORD
               n_seguro        CHAR(11),
               salario_actual  DECIMAL(10,2)
        END RECORD
        DEFINE seguro            CHAR(11)
        DEFINE g_paramgrales RECORD     LIKE glo_parametro.*,
           vcontafi INTEGER,
           vcontcana INTEGER
END GLOBALS

MAIN

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        CREATE TEMP TABLE safre_tmp:tabcanerr
             (n_seguro   CHAR(11))

        DATABASE safre_Af
    WHENEVER ERROR STOP
  
	OPTIONS INPUT WRAP,
	        PROMPT LINE LAST,
		ACCEPT KEY CONTROL-I
	LET HOY = TODAY
	LET HORA = TIME
	OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIB0041" ATTRIBUTE(BORDER)
        DISPLAY " AFIB004      ACTUALIZACION DE SUELDOS DE AFILIADOS                           " AT 3,1 ATTRIBUTE(REVERSE)

	DISPLAY "                                < Ctrl-C > Salir                               " AT 1,1 ATTRIBUTE(REVERSE)
	DISPLAY HOY USING"dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
          INPUT BY NAME opcion
            AFTER FIELD opcion
              CASE opcion
               WHEN "1"
                      SELECT "X" FROM afi_mae_afiliado
                      WHERE salario_base_comis = 0
                      GROUP BY 1
                      IF STATUS  = NOTFOUND THEN
       ERROR "No hay afiliado para actualizarle salario base para comision"
                          NEXT FIELD opcion
                      ELSE
                          CALL diario()
                      END IF
                WHEN "2"
                      CALL  bimes()
                WHEN "16"
                      EXIT PROGRAM
                OTHERWISE 
                   ERROR "No puede ser nulo"
                   NEXT FIELD opcion
              END CASE
         ON KEY ( INTERRUPT )
	 EXIT PROGRAM
	END INPUT		
END MAIN
FUNCTION diario()
#-----------------------
  DEFINE g_sube SMALLINT
  DEFINE cuenta SMALLINT
  DEFINE l_salario DECIMAL(12,2)
  DEFINE l_cont SMALLINT
  DEFINE l_cont1 SMALLINT
  DEFINE l_cont2 SMALLINT

    WHILE TRUE
        CALL pregunta()
        IF aux_pausa MATCHES "[SsNn]" THEN
           EXIT WHILE
       END IF
   END WHILE

LET vcontafi = 0
LET vcontcana = 0

ERROR "Preparando Informacion"
SELECT COUNT(*) INTO vcontafi 
FROM afi_mae_afiliado
SELECT COUNT(*) INTO vcontcana
FROM com_mae_actinact

IF vcontafi = vcontcana THEN

   LET l_cont = 0
   LET l_cont1 = 0
   LET l_cont2 = 0
   IF aux_pausa MATCHES "[Ss]" THEN
         LET g_sube = NULL
         ERROR "Procesando Informacion"
         DECLARE cur_2 CURSOR FOR 
             SELECT n_seguro,salario_base_comis FROM afi_mae_afiliado
             WHERE   indicador_comision = 99
         FOREACH  cur_2 into sub.*
             LET l_cont = l_cont + 1 
             DISPLAY "Regitros Procesados    ",l_cont USING "##,###" AT 17,1
             LET l_salario = 0
             SELECT sueldo INTO l_salario FROM com_mae_actinact
             WHERE n_seguro = sub.n_seguro
             IF STATUS <> NOTFOUND THEN
                LET l_cont1 = l_cont1 + 1 
                DISPLAY "Regitros Actualizados  ",l_cont1 USING "##,###" AT 18,1
                IF ( sub.salario_actual > 0 ) then
                    UPDATE afi_mae_afiliado  SET
                        salario_actual = sub.salario_actual,
                        #salario_base_comis = l_salario,
                        indicador_comision = 0,
                        fecha_actualiza_sa = HOY 
                    WHERE n_seguro = sub.n_seguro
                ELSE
                    UPDATE afi_mae_afiliado  SET
                        salario_actual = l_salario,
                        salario_base_comis = l_salario,
                        indicador_comision = 0,
                        fecha_actualiza_sa = HOY 
                    WHERE n_seguro = sub.n_seguro
                END IF
             ELSE
                IF ( sub.salario_actual > 0 ) then
                   UPDATE afi_mae_afiliado  SET
                       indicador_comision = 0,
                       fecha_actualiza_sa = HOY 
                   WHERE n_seguro = sub.n_seguro
                ELSE
                   UPDATE afi_mae_afiliado  SET
                       salario_actual = 0,
                       salario_base_comis = 0,
                       indicador_comision = 0,
                       fecha_actualiza_sa = HOY 
                   WHERE n_seguro = sub.n_seguro
                END IF
             LET l_cont2 = l_cont2 + 1 
             DISPLAY "Regitros NO Procesados ",l_cont2 USING "##,###" AT 19,1
                INSERT INTO safre_tmp:tabcanerr VALUES (sub.n_seguro)
             END IF
       END FOREACH 
                 IF l_cont <> 0 THEN
                     #DISPLAY "Existen ",l_cont ," Afiliados no encontrados en el archivo de CANASE " AT 20,1
                     SLEEP 3
                     WHILE TRUE
                        CALL pregunta1() 
                         IF aux_pausa1 MATCHES "[SsNn]" THEN
                           exit while
                         END IF
                     END WHILE
                       IF aux_pausa1 MATCHES "[Ss]" THEN
                         CALL reporte() 
                         ERROR "Proceso Terminado"
                       ELSE
                         EXIT PROGRAM
                       END IF
                 ELSE
                     ERROR "Proceso Terminado"
                     sleep 1
                     EXIT PROGRAM
                 END IF
         EXIT PROGRAM
   ELSE
         ERROR "Proceso cancelado ...."
         EXIT PROGRAM
   END IF
ELSE
   ERROR "No se puede calcular salarios hasta que Sistemas actualice el canase"
   SLEEP 3
   EXIT PROGRAM
END IF
END FUNCTION

FUNCTION bimes()
#-----------------------
  DEFINE g_sube SMALLINT
  DEFINE cuenta SMALLINT
  DEFINE l_cont SMALLINT
  DEFINE l_salario DECIMAL(12,2)
WHILE TRUE
   CALL pregunta()
   IF aux_pausa MATCHES "[SsNn]" THEN
      EXIT WHILE
   END IF
END WHILE
   LET l_cont = 0
   IF aux_pausa MATCHES "[Ss]" THEN
         LET g_sube = NULL
         ERROR "Procesando Informacion"
         DECLARE cur_3 CURSOR FOR 
             SELECT n_seguro FROM afi_mae_afiliado
             WHERE  salario_base_comis != 0
               AND  indicador_comision != 99
         FOREACH  cur_3 into sub.*
             LET l_salario = 0
             SELECT salario_actual INTO l_salario FROM afi_canase
             WHERE n_seguro = sub.n_seguro
             IF STATUS <> NOTFOUND THEN
                UPDATE afi_mae_afiliado 
                SET salario_actual = l_salario,
                    fecha_actualiza_sa = HOY 
                WHERE n_seguro = sub.n_seguro
            ELSE
                LET l_cont = l_cont + 1 
                INSERT INTO safre_tmp:tabcanerr VALUES (sub.n_seguro)
                #DISPLAY l_cont AT 20,1
            END IF
       END FOREACH 
                 IF l_cont <> 0 THEN
                     #DISPLAY "Existen ",l_cont ," Afiliados No encontrados en el archivo de CANASE " AT 20,1
                     SLEEP 3
                     WHILE TRUE
                        CALL pregunta1() 
                        IF aux_pausa1 MATCHES "[SsNn]" THEN
                           exit while
                        END IF
                     END WHILE
                       IF aux_pausa1 MATCHES "[Ss]" THEN
                        CALL reporte() 
                        ERROR "Proceso Terminado"
                     ELSE
                        EXIT PROGRAM
                     END IF
                 ELSE
                     ERROR "Proceso Terminado"
                     EXIT PROGRAM
                 END IF
         EXIT PROGRAM
   ELSE
      ERROR "Proceso cancelado ...."
      EXIT PROGRAM
   END IF
END FUNCTION

FUNCTION pregunta1()
PROMPT "Desea generar listado de numero de seguro que no se encontraron S/N ? " FOR aux_pausa1
END FUNCTION

FUNCTION pregunta()
    PROMPT "Desea ejecutar proceso S/N ? " FOR aux_pausa
END FUNCTION

FUNCTION reporte()
       DEFINE g_paramgrales   RECORD LIKE glo_parametro.*
	DEFINE w_aux  RECORD
            n_folio             LIKE afi_solicitud.n_folio,
            n_seguro            LIKE afi_solicitud.n_seguro,
            n_unico             LIKE afi_solicitud.n_unico ,
            n_rfc               LIKE afi_solicitud.n_rfc   ,
            paterno             LIKE afi_solicitud.paterno ,
            materno             LIKE afi_solicitud.materno ,
            nombres             LIKE afi_solicitud.nombres ,
            sexo                LIKE afi_solicitud.sexo    ,
            codven              LIKE afi_solicitud.codven  ,
            fecha_elaboracion   LIKE afi_solicitud.fecha_elaboracion,
            frecafor            LIKE afi_solicitud.frecafor  ,
            fentcons            LIKE afi_solicitud.fentcons
	END RECORD
        DEFINE hoy                 DATE
        DEFINE hora                 CHAR(8)
	DEFINE g_afore			RECORD LIKE tab_afore_local.*
	DEFINE g_usuario		CHAR(8)
	DEFINE G_LISTA  		CHAR(200)
        DEFINE txt1                     CHAR(600)
    LET hora = TIME
    LET hoy = TODAY
	    SELECT *        ,
                   USER 
            INTO   g_afore.*,
                   g_usuario 
            FROM   tab_afore_local

           SELECT *
           INTO g_paramgrales.*
           FROM glo_parametro    

	    LET G_LISTA = g_paramgrales.ruta_spool CLIPPED,"/",g_usuario CLIPPED,".LISTADO_DE_AFILIADOS_NO_ACTUALIZADOS_EN_SALARIO_",HOY USING "DD-MM-YY","_",hora CLIPPED
            START REPORT listado TO G_LISTA
         DECLARE cur_4 CURSOR for
               SELECT a.n_folio        ,  
                      a.n_seguro       ,  
                      a.n_unico        ,  
                      a.n_rfc          ,  
                      a.paterno          ,
                      a.materno          ,
                      a.nombres          ,
                      a.sexo             ,
                      a.codven           ,
                      a.fecha_elaboracion   ,
                      a.frecafor        ,
                      a.fentcons       
                 FROM   afi_solicitud a  , safre_tmp:tabcanerr b
                 WHERE b.n_seguro = a.n_seguro
                 ORDER BY n_seguro
                FOREACH cur_4 INTO w_aux.*
                    OUTPUT TO REPORT listado(w_aux.*)       
                END FOREACH 
            FINISH REPORT listado
                ERROR "LISTADO GENERADO" SLEEP 2
	    LET G_LISTA = "chmod 777 ", g_paramgrales.ruta_spool CLIPPED,"/",g_usuario CLIPPED,".LISTADO_DE_AFILIADOS_NO_ACTUALIZADOS_EN_SALARIO_",HOY USING "DD-MM-YY","_",hora CLIPPED
            RUN G_LISTA
    CLOSE cur_4
  EXIT PROGRAM
END FUNCTION
REPORT listado(w_aux)
#--------------------
    DEFINE w_aux  RECORD
        n_folio             LIKE afi_solicitud.n_folio ,
        n_seguro            LIKE afi_solicitud.n_seguro,
        n_unico             LIKE afi_solicitud.n_unico ,
        n_rfc               LIKE afi_solicitud.n_rfc   ,
        paterno             LIKE afi_solicitud.paterno ,
        materno             LIKE afi_solicitud.materno ,
        nombres             LIKE afi_solicitud.nombres ,
        sexo                LIKE afi_solicitud.sexo    ,
        codven              LIKE afi_solicitud.codven   ,
        fecha_elaboracion   LIKE afi_solicitud.fecha_elaboracion,
        frecafor            LIKE afi_solicitud.frecafor,
        fentcons            LIKE afi_solicitud.fentcons
    END RECORD
	DEFINE 
            aux_sexo			CHAR(10)   ,
            razon_social                CHAR(40)
        DEFINE w_col RECORD
             status_interno    LIKE afi_solicitud.status_interno,
            numero          CHAR(8)
        END RECORD
   DEFINE f_cons   RECORD
        fentcons            LIKE afi_mae_afiliado.fentcons
   END RECORD
    OUTPUT
      
        PAGE LENGTH 90
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
        SELECT A.razon_social
        INTO   razon_social
        FROM   tab_afore_local A

	PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 001,"========================================"           ,
            COLUMN 040,"========================================"           ,
            COLUMN 080,"========================================"           ,
            COLUMN 120,"========================================"           ,
            COLUMN 160,"========"
        PRINT
            COLUMN 001,razon_social                                         ,
            COLUMN 140,"FECHA   :",hoy USING "DD/MM/YY "                     ,
            COLUMN 149,hora [1,5]
        PRINT
            COLUMN 001,"AFIB004"                                            ,
            COLUMN 035,"       L I S T A D O   D E   A F I L I A D O S   N O  A C T U A L I Z A D O S   E N   S A L A R I O     ",
            COLUMN 140,"NUM.PAG.:",pageno USING "####,###"
            SKIP 1 LINE
        PRINT
            COLUMN 001,"----------------------------------------"           ,
            COLUMN 040,"----------------------------------------"           ,
            COLUMN 080,"----------------------------------------"           ,
            COLUMN 120,"----------------------------------------"           ,
            COLUMN 160,"--------"
        PRINT
            COLUMN 01,"Solicitud"                                           ,
            COLUMN 14,"N.S.S."                                            ,
            COLUMN 29,"C.U.R.P."                                          ,
            COLUMN 48,"R.F.C."                                             ,
            COLUMN 63,"Paterno"                                             ,
            COLUMN 80,"Materno"                                             ,
            COLUMN 97,"Nombres"                                             ,
            COLUMN 118,"Sx"                                               ,
            COLUMN 121,"Promotor"                                            ,
            COLUMN 133,"F. Firma"       ,
            COLUMN 145,"F. Captura"    ,
            COLUMN 157,"F. Aprobado"
        PRINT
            COLUMN 001,"========================================"           ,
            COLUMN 040,"========================================"           ,
            COLUMN 080,"========================================"           ,
            COLUMN 120,"========================================"           ,
            COLUMN 160,"========"
    ON EVERY ROW
	SELECT sexo_desc into aux_sexo FROM tab_sexo
	WHERE sexo_cod = w_aux.sexo
        PRINT
            COLUMN 01,w_aux.n_folio   USING "&&&&&&&&"                      ,
            COLUMN 14,w_aux.n_seguro                                        ,
            COLUMN 29,w_aux.n_unico                                         ,
            COLUMN 48,w_aux.n_rfc                                           ,
            COLUMN 63,w_aux.paterno [1,17],
            COLUMN 80,w_aux.materno [1,17],
            COLUMN 97,w_aux.nombres[1,21],
            COLUMN 118,aux_sexo    [1]                                         ,
            COLUMN 121,w_aux.codven USING "&&&&&&&&&&"                      ,
            COLUMN 133,w_aux.fecha_elaboracion USING "dd-mm-yyyy"       ,
            COLUMN 145,w_aux.frecafor USING "dd-mm-yyyy"  ,
            COLUMN 157,w_aux.fentcons USING "dd-mm-yyyy"
ON LAST ROW
            PRINT
            PRINT
                 COLUMN 01,"Se encontraron ",COUNT(*) USING "&&&&" , " Errores"
END REPORT
 
