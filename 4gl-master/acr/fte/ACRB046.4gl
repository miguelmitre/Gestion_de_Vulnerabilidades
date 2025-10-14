###############################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                             #
#Propietario       => E.F.P.                                                  #
#Programa ACRB046  => REVERSA PROVISION Y/O LIQUIDACION USO DE CREDITO        #
#Por               => MAURO MUNIZ CABALLERO                                   #
#Fecha creacion    => 28 DE JULIO DE 2000                                     #
#Actualizacion     => MODIFICACION DERL PROGRAMA ACRC003                      #
#Fecha actualiz.   => 04 DE FEBRERO DE 2003                                   #
#Sistema           => ACR                                                     #
###############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_acr_parametro RECORD LIKE seg_modulo.*
    	
    DEFINE s_codigo_afore LIKE safre_af:tab_afore_local.codigo_afore


    DEFINE
       enter             CHAR(1),
	     vproceso_cod      CHAR(5),
	     vnss              CHAR(11),
	     vrev_desmarca     CHAR(100)
                         
    DEFINE               
       gd_fecha_carga    ,
       HOY               ,
       vfecha            ,
       vfecha_ini        DATE
                         
    DEFINE               
       vcorrelativo      ,
       vfolio            INTEGER
                         
    DEFINE               
       opcion            ,
       v_marca_ent       SMALLINT
        
        

END GLOBALS


MAIN

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG("ACRB046.log")
    CALL inicio()            #i
    CALL proceso_principal() #pp

END MAIN


FUNCTION inicio()
#i----------------

    LET HOY = today

    SELECT codigo_afore
    INTO   s_codigo_afore
    FROM   safre_af:tab_afore_local

    SELECT *
    INTO   g_acr_parametro.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'acr' 

    LET vproceso_cod = '00030'

END FUNCTION


FUNCTION proceso_principal()
#pp-------------------------

   OPEN WINDOW v1 AT 4,4 WITH FORM "ACRB046" ATTRIBUTE(BORDER)
   DISPLAY "ACRB046          REVERSO OPERACIONES USO DE LA GARANTIA                        " AT 3,1 ATTRIBUTE(REVERSE)   
   DISPLAY "                           < Ctrl-C > Salir                                    "AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,61 ATTRIBUTE(REVERSE)

   INPUT BY NAME  opcion,gd_fecha_carga,vfolio

      
      AFTER FIELD opcion
      
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") 
         OR FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
            NEXT FIELD PREVIOUS
         END IF 
         
         IF opcion IS NULL THEN
            ERROR " OPCION INCORRECTA "
            NEXT FIELD opcion
         ELSE
            IF opcion > 4 THEN
               ERROR " OPCION INCORRECTA "
               NEXT FIELD opcion
            END IF            
         END IF

      AFTER FIELD gd_fecha_carga

         IF FGL_LASTKEY() = FGL_KEYVAL("UP")
         OR FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
            NEXT FIELD PREVIOUS
         END IF 
         
               
         IF opcion = 1  THEN
         	
            IF gd_fecha_carga IS NULL
            OR gd_fecha_carga = " " THEN
                ERROR " FECHA INCORRECTA "
                NEXT FIELD gd_fecha_carga
            END IF  
         END IF
               
      AFTER FIELD vfolio
      
         IF FGL_LASTKEY() = FGL_KEYVAL("UP")
         OR FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
            NEXT FIELD PREVIOUS
         END IF 
         
               
         IF opcion = 1 
         OR opcion = 2 THEN
            #-- NO SE REQUIERE EL FOLIO PARA REVERSO DE OP. 01,06  --#
         ELSE
            IF vfolio IS NULL
            OR vfolio = 0 THEN
                ERROR " FOLIO INCORRECTO "
                NEXT FIELD vfolio
            END IF                       
         END IF

      ON KEY (ESC)

         SELECT UNIQUE "g.X"
         FROM   cta_act_marca g
         WHERE  g.marca_cod = 236
         AND    g.fecha_ini = gd_fecha_carga

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NO EXISTE INFORMACION A REVERSAR "
            NEXT FIELD gd_fecha_carga         
         END IF

         IF opcion = 1 
         OR opcion = 2 THEN
         
            #-- NO SE REQUIERE EL FOLIO PARA REVERSO DE OP. 01,06  --#
         ELSE
            SELECT "X"
            FROM   con_transaccion
            WHERE  @folio     = vfolio
            AND    @proceso_cod = vproceso_cod
            AND    @estado      = 40
            GROUP BY 1
            
            IF STATUS <> NOTFOUND THEN
              ERROR " FOLIO YA PROCESADO EN EL AREA DE CONTABILIDAD "
              NEXT FIELD vfolio
            END IF
            
            IF opcion = 3 THEN
               SELECT "X"
               FROM   dis_provision
               WHERE  folio = vfolio
               GROUP BY 1

               IF SQLCA.SQLCODE <> 0 THEN                      
                  ERROR " NO EXISTE INFORMACION DEL FOLIO "
                  NEXT FIELD vfolio
               END IF
                           
               SELECT "X"
               FROM   dis_cuenta
               WHERE  folio = vfolio
               GROUP BY 1

               IF SQLCA.SQLCODE = 0 THEN                      
                  ERROR " FOLIO YA LIQUIDADO "
                  NEXT FIELD vfolio
               END IF
            END IF   
            
            IF opcion = 4 THEN
               SELECT "X"
               FROM   dis_cuenta
               WHERE  folio = vfolio
               GROUP BY 1

               IF SQLCA.SQLCODE <> 0 THEN                      
                  ERROR " NO EXISTE INFORMACION DEL FOLIO "
                  NEXT FIELD vfolio
               END IF                   
            END IF
         END IF

         DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
         SLEEP 3                                                      
 
         CASE opcion 
             WHEN 1 CALL reversa_op01()
             WHEN 2 CALL reversa_op06()
             WHEN 3 CALL reversa_op09()
             WHEN 4 CALL reversa_liquida()
         END CASE

         PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " 
         FOR enter

         EXIT INPUT

      ON KEY (INTERRUPT, CONTROL-C)
         PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " 
         FOR enter
         EXIT INPUT
   END INPUT

   CLOSE WINDOW v1

END FUNCTION


FUNCTION reversa_op01()
#-----------------------

    
   DELETE 
   FROM  cta_act_marca
   WHERE marca_cod in (236)
   AND   fecha_ini = gd_fecha_carga

   DELETE 
   FROM  cta_his_marca
   WHERE marca_cod in (236)
   AND   fecha_ini = gd_fecha_carga

   DELETE 
   FROM safre_tmp:cza_garantia
   
   DELETE 
   FROM safre_tmp:det_garantia
   
   DELETE 
   FROM safre_tmp:sum_garantia
   
   DELETE 
   FROM safre_tmp:tmp_pla_acr

END FUNCTION


FUNCTION reversa_op06()
#----------------------
   
   DELETE
   FROM safre_tmp:det_devol_uso    

END FUNCTION


FUNCTION reversa_op09()
#----------------------

   DELETE 
   FROM  dis_provision 
   WHERE folio = vfolio
   AND   tipo_movimiento IN (236)

   DELETE 
   FROM safre_tmp:sdo_acr_af

END FUNCTION


FUNCTION reversa_liquida()
#-------------------------

   DECLARE cur_1 CURSOR FOR
   SELECT nss, 
          correlativo
   FROM   cta_his_marca
   WHERE  nss IN (SELECT UNIQUE(nss) 
                  FROM   dis_cuenta
                  WHERE  folio = vfolio) 
   AND    marca_cod = 236
   AND    fecha_ini = gd_fecha_carga
                   
    FOREACH cur_1 INTO vnss,vcorrelativo
    
       LET v_marca_ent = 236
       LET vrev_desmarca = "EXECUTE PROCEDURE reversa_desmarca('",
                            vnss,"',",
                            v_marca_ent,",",
                            vcorrelativo,",",
                           "'",vfecha_ini,"'",
                           ")"
       PREPARE eje_revdesmarca2 FROM vrev_desmarca
       EXECUTE eje_revdesmarca2    
    
    END FOREACH
       
    DELETE 
    FROM   acr_cza_garantia
    WHERE  folio = vfolio
    
    DELETE 
    FROM   acr_det_garantia
    WHERE  folio = vfolio    

    DELETE 
    FROM   acr_sum_garantia
    WHERE  folio = vfolio
    
    DELETE 
    FROM   dis_cuenta
    WHERE  folio = vfolio
    AND    tipo_movimiento IN (236)

    UPDATE dis_provision
    SET    estado = 5
    WHERE  folio = vfolio
    AND    tipo_movimiento IN (236)
    AND    estado = 6
    
END FUNCTION
