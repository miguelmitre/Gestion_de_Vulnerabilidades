################################################################################
# Proyecto            => SISTEMA DE AFORES( MEXICO )                           #
# Owner               => E.F.P.                                                #
# Version             => 1.0                                                   #
# Sistema             => RET                                                   #
# Programa RETC845ESP => CONSULTA DE APORTACIONES EXTEMPORANEAS RECIBIDAS      #
# Fecha creacion      => 29 AGOSTO DE 2006                                     #
# Elaborado por       => ISAI JIMENEZ ROJAS                                    #
# Modificado por      =>                                                       #
################################################################################

DATABASE safre_af

MAIN

   DEFINE fecha_ini   DATE
   DEFINE fecha_fin   DATE
   DEFINE fecha_aux   DATE
   DEFINE HOY         DATE
   DEFINE i           INTEGER
   DEFINE vcontador   INTEGER
   DEFINE vfecha      DATE
   DEFINE tecla       CHAR(1)
   DEFINE vcomando    CHAR(100)
   
   
   OPTIONS INPUT WRAP,
           MESSAGE LINE LAST,
           PROMPT LINE LAST
   
   DEFER INTERRUPT
   
   ------------------
     
   LET HOY = TODAY
   LET fecha_ini = TODAY
   LET fecha_fin = TODAY 
      
   OPEN WINDOW retc831esp AT 2,2 WITH FORM "RETC831ESP" ATTRIBUTE (BORDER)
   DISPLAY " RETC831ESP ESPECIAL IDENTIFICACION DE APORTACIONES EXTEMPORANEAS " AT 3,1  ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
   
   INPUT BY NAME fecha_ini, fecha_fin WITHOUT DEFAULTS
         BEFORE INPUT
            MESSAGE " INGRESE EL PERIODO DEL CUAL REQUIERE REPOROCESAR LA INFORMACION "
         AFTER FIELD fecha_ini
               IF fecha_ini IS NULL OR fecha_ini = "" THEN
                  ERROR " LA FECHA INICIAL NO PUEDE SER NULA "
                  NEXT FIELD fecha_ini
               END IF
               
               IF (fecha_fin IS NOT NULL) AND (fecha_ini > fecha_fin) THEN 
                  ERROR " LA FECHA INICIAL NO PUEDE SER MAYOR QUE LA FINAL "
                  NEXT FIELD fecha_ini
               END IF
               
         AFTER FIELD fecha_fin
               IF fecha_fin IS NULL OR fecha_fin = "" THEN
                  ERROR " LA FECHA FINAL NO PUEDE SER NULA "
                  NEXT FIELD fecha_fin
               END IF
         AFTER INPUT 
               MESSAGE ""
    END INPUT
    
    IF INT_FLAG THEN
       ERROR " OPERACION CANCELADA POR EL USUARIO "
       SLEEP 2
    END IF

    -- BARRE CADA UNA DE LAS FECHAS DEL PERIODO ESPECIFICADO
    
    LET fecha_aux = fecha_ini
    
    WHILE fecha_aux <= fecha_fin
      
       DISPLAY " PROCESANDO ",fecha_aux AT 15,15

       LET vcomando = "export FECHA='",fecha_aux,"';fglgo RETC831"
       RUN vcomando
       
       LET fecha_aux = fecha_aux + 1
       
    END WHILE
    
    PROMPT " PROCESO FINALIZADO, PRESIONE <RETURN>: " FOR CHAR tecla
      
    CLOSE WINDOW  retc831esp
    
END MAIN
