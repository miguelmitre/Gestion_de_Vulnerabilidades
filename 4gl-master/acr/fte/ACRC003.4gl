###############################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                             #
#Propietario       => E.F.P.                                                  #
#Programa ACRC003  => REVERSA PROVISION Y/O LIQUIDACION TRANSF. ACREDITADOS   #
#Por               => MAURO MUNIZ CABALLERO                                   #
#Fecha creacion    => 28 DE JULIO DE 2000                                     #
#Actualizacion     =>                                                         #
#Fecha actualiz.   =>                                                         #
#Sistema           => ACR                                                     #
###############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_acr_parametro    RECORD LIKE acr_parametro.*
    DEFINE s_codigo_afore LIKE safre_af:tab_afore_local.codigo_afore

    DEFINE
        opcion            SMALLINT,
        vfolio            INTEGER,
        vfecha            DATE,
        HOY               DATE,
        enter             CHAR(1),
        vnombre_archivo   CHAR(20),
        vnss              CHAR(11),
        vrev_desmarca     CHAR(100),
        vcorrelativo      INTEGER,
        vfecha_ini        DATE,
        v_marca_ent       SMALLINT

    DEFINE
	vproceso_cod CHAR(5)

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL inicio()            #i
    CALL proceso_principal() #pp

END MAIN

FUNCTION inicio()
#i-------------

    LET HOY = today

    SELECT codigo_afore
    INTO   s_codigo_afore
    FROM   safre_af:tab_afore_local

    SELECT *
    INTO   g_acr_parametro.*
    FROM   acr_parametro

    LET vproceso_cod = '00016'
        
END FUNCTION


FUNCTION proceso_principal()
#pp-------------------------

   OPEN WINDOW ACRC0031 AT 4,4 WITH FORM "ACRC0031" ATTRIBUTE(BORDER)
   DISPLAY "ACRC003      REVERSO OPERACIONES TRANSFERENCIA ACREDITADOS                     " AT 3,1 ATTRIBUTE(REVERSE)   
   DISPLAY "                           < Ctrl-C > Salir                                    "AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,61 ATTRIBUTE(REVERSE)

   INPUT BY NAME  opcion,vnombre_archivo,vfolio

      
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

      AFTER FIELD vnombre_archivo

         IF FGL_LASTKEY() = FGL_KEYVAL("UP")
         OR FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
            NEXT FIELD PREVIOUS
         END IF 
         
               
         IF vnombre_archivo IS NULL
         OR vnombre_archivo = 0 THEN
             ERROR " NOMBRE DE ARCHIVO INCORRECTO "
             NEXT FIELD vnombre_archivo
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
         FROM   acr_ctr_arh g
         WHERE  g.nombre_archivo = vnombre_archivo

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NO EXISTE INFORMACION DEL NOMBRE DE ARCHIVO "
            NEXT FIELD vnombre_archivo         
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

   CLOSE WINDOW ACRC0031

END FUNCTION


FUNCTION reversa_op01()
#-----------------------

   SELECT fecha_proceso
   INTO   vfecha_ini 
   FROM   acr_ctr_arh
   WHERE  nombre_archivo = vnombre_archivo
    
   DELETE 
   FROM  cta_act_marca
   WHERE marca_cod in (230,232)
   AND   fecha_ini = vfecha_ini

   DELETE 
   FROM  cta_his_marca
   WHERE marca_cod in (230,232)
   AND   fecha_ini = vfecha_ini

   DELETE 
   FROM  acr_ctr_arh
   WHERE nombre_archivo = vnombre_archivo

   DELETE 
   FROM safre_tmp:cza_tra_acr
   
   DELETE 
   FROM safre_tmp:det_tra_acr
   
   DELETE 
   FROM safre_tmp:sum_tra_acr
   
   DELETE 
   FROM safre_tmp:tmp_pla_acr

END FUNCTION


FUNCTION reversa_op06()
#----------------------

   SELECT fecha_proceso
   INTO   vfecha_ini 
   FROM   acr_ctr_arh
   WHERE  nombre_archivo = vnombre_archivo

   DECLARE cur_1 CURSOR FOR
   SELECT nss, 
          correlativo
   FROM   cta_his_marca
   WHERE  nss IN (SELECT nss_afore
                  FROM   safre_tmp:det_devol_sol) 
   AND  marca_cod = 232
   AND  fecha_ini = vfecha_ini
                  
   FOREACH cur_1 INTO vnss,vcorrelativo
   
      LET v_marca_ent = 232
      LET vrev_desmarca = "EXECUTE PROCEDURE reversa_desmarca('",
                           vnss,"',",
                           v_marca_ent,",",
                           vcorrelativo,",",
                          "'",vfecha_ini,"'",
                          ")"
      PREPARE eje_revdesmarca FROM vrev_desmarca
      EXECUTE eje_revdesmarca    
   
      UPDATE safre_tmp:det_tra_acr
      SET    estado = 0
      WHERE  nss_afore = vnss
      
   END FOREACH
   
   DELETE
   FROM safre_tmp:det_devol_sol    

END FUNCTION


FUNCTION reversa_op09()
#----------------------

   DELETE 
   FROM  dis_provision 
   WHERE folio = vfolio
   AND   tipo_movimiento in(230,235)

   DELETE 
   FROM safre_tmp:sdo_acr_af

END FUNCTION


FUNCTION reversa_liquida()
#-------------------------

   SELECT fecha_proceso
   INTO   vfecha_ini 
   FROM   acr_ctr_arh
   WHERE  nombre_archivo = vnombre_archivo
          
   DECLARE cur_2 CURSOR FOR
   SELECT nss, 
          correlativo
   FROM   cta_his_marca
   WHERE  nss IN (SELECT UNIQUE(nss) 
                  FROM   dis_cuenta
                  WHERE  folio = vfolio) 
   AND    marca_cod = 232
   AND    fecha_ini = vfecha_ini
                   
    FOREACH cur_2 INTO vnss,vcorrelativo
    
       LET v_marca_ent = 232
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
    FROM   acr_cza_cedido
    WHERE  folio = vfolio
    DELETE 
    FROM   acr_det_cedido
    WHERE  folio = vfolio    

    DELETE 
    FROM   acr_sum_cedido
    WHERE  folio = vfolio
    
    DELETE 
    FROM   dis_cuenta
    WHERE  folio = vfolio
    AND    tipo_movimiento in(230,235)

    UPDATE dis_provision
    SET    estado = 5
    WHERE  folio = vfolio
    AND    tipo_movimiento in(230,235)

END FUNCTION
