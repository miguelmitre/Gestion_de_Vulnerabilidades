###############################################################################
#Proyecto          => AFORE ( MEXICO )                                        #
#Propietario       => E.F.P.                                                  #
#Programa CTAR004  => REVERSA PROCESO SALDO CERO                              #
#Autor             => FERNANDO HERRERA HERNANDEZ                              #
#Fecha             => 30 DE ABRIL DE 2009                                     #
#Sistema           => CTA                                                     #
###############################################################################

DATABASE safre_af

GLOBALS
  DEFINE 
    g_param_cta     RECORD LIKE seg_modulo.*
  
  DEFINE 
    s_codigo_afore  LIKE tab_afore_local.codigo_afore

  DEFINE
    opcion          SMALLINT,
    vfecha_corte    DATE,
    fecha_mes       DATE,
    vnombre_archivo CHAR(20),
    vfecha          DATE,
    hoy             DATE,
    enter           CHAR(1)

  DEFINE
    reg_cero        RECORD LIKE cta_saldo_cero.*

  DEFINE
    vopcion         SMALLINT
    
  DEFINE qry_cta_adm_rev CHAR(200)
  DEFINE rcta_admin      SMALLINT

END GLOBALS

MAIN
  OPTIONS INPUT WRAP,
  PROMPT LINE LAST,
  ACCEPT KEY CONTROL-I
  DEFER INTERRUPT

  CALL STARTLOG("CTAR004.log")
  CALL inicio()            #i
  CALL proceso_principal() #pp
END MAIN

FUNCTION inicio()
#i-------------
  LET hoy = today

  SELECT codigo_afore
  INTO   s_codigo_afore
  FROM   tab_afore_local

  SELECT *
  INTO   g_param_cta.*
  FROM   seg_modulo
  WHERE  modulo_cod = 'cta'

  --LET qry_cta_adm_rev = "EXECUTE FUNCTION fn_reverso_reg_cuenta ( ?, ?, ?, ?)"
  --PREPARE prp_rev_cta_admin FROM qry_cta_adm_rev

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------
  OPEN WINDOW CTAR0041 AT 4,4 WITH FORM "CTAR0041" ATTRIBUTE(BORDER)
    DISPLAY "CTAR004         REVERSO DE OPERACIONES SALDO CERO                              " AT 3,1 ATTRIBUTE(REVERSE)   
    DISPLAY " < CTRL-C > Salir " AT 1,26              
    DISPLAY hoy USING "DD-MM-YYYY" AT 3,61 ATTRIBUTE(REVERSE)

    INPUT BY NAME opcion, vfecha_corte, vnombre_archivo
    
      AFTER FIELD opcion
        IF opcion IS NULL THEN
           ERROR "Campo opcion reverso NO puede ser NULO"
           NEXT FIELD opcion
        ELSE
           IF opcion < 1 OR opcion > 5 THEN
              ERROR "REVERSAR: 1=> PREPARA, 2=> IDENTIFICACION, 4=> GEN. ARCHIVO, 5=>REC. ARCHIVO"
              NEXT FIELD opcion
           ELSE
              NEXT FIELD vfecha_corte
           END IF
        END IF
    
      AFTER FIELD vfecha_corte
        IF vfecha_corte IS NULL THEN
           ERROR "Campo fecha corte NO puede ser NULO"
           NEXT FIELD vfecha_corte
        ELSE 
        
           SELECT "X"
             FROM cta_ctr_saldo_cero
            WHERE @fecha_corte = vfecha_corte
            GROUP BY 1
           IF STATUS = NOTFOUND THEN
              PROMPT " FECHA DE CORTE NO PROCESADO,",
                     " [Enter] continuar "
              ATTRIBUTES (REVERSE) FOR enter
              NEXT FIELD vfecha_corte
           ELSE
              IF (opcion = 1  OR
                  opcion = 4) THEN
                 SELECT "X"
	           FROM cta_ctr_saldo_cero
	          WHERE @fecha_corte = vfecha_corte
	            AND @etapa       = opcion
	          GROUP BY 1
	         IF STATUS = NOTFOUND THEN
                    PROMPT " 1.LA OPCION NO HA SIDO PROCESADO,",
                           " [Enter] continuar "
                    ATTRIBUTES (REVERSE) FOR enter	                          
                    NEXT FIELD opcion
                 END IF
              ELSE
                 SELECT "X"
	           FROM cta_ctr_saldo_cero
	          WHERE @fecha_corte = vfecha_corte
	            AND @etapa      IN (2,3) 
	          GROUP BY 1
	         IF STATUS = NOTFOUND THEN
                    PROMPT " 2.LA OPCION NO HA SIDO PROCESADO,",
                           " [Enter] continuar "
                    ATTRIBUTES (REVERSE) FOR enter	                          
                    NEXT FIELD opcion
                 END IF              
              END IF

              IF opcion <> 5 THEN

                 CASE opcion
                   WHEN 1 LET vopcion = opcion + 1
                   WHEN 2 LET vopcion = opcion + 2
                   WHEN 4 LET vopcion = opcion
                 END CASE

                 SELECT "X"
                   FROM cta_ctr_saldo_cero
                  WHERE @fecha_corte = vfecha_corte
                    AND @etapa       > vopcion
                    AND @etapa      <= 4
                  GROUP BY 1
                 IF STATUS <> NOTFOUND THEN
                    PROMPT " 1.DEBE REVERSAR PRIMERO EL PROCESO POSTERIOR,",
                           " [Enter] continuar "
                    ATTRIBUTES (REVERSE) FOR enter
                    NEXT FIELD opcion
                 END IF
                 LET vopcion = 0
              END IF

              IF opcion = 4 THEN
                 LET fecha_mes = vfecha_corte + 30 UNITS DAY   
                 
                 SELECT "X"
                   FROM cta_ctr_archivo
                  WHERE fecha_recepcion >= vfecha_corte
                    AND fecha_recepcion <= fecha_mes
                  GROUP BY 1
                 IF STATUS <> NOTFOUND THEN
                    PROMPT " 2.DEBE REVERSAR PRIMERO EL PROCESO POSTERIOR,",
                           " [Enter] continuar "
                    ATTRIBUTES (REVERSE) FOR enter
                    NEXT FIELD opcion
                 END IF
              END IF
           END IF

           IF opcion = 5 THEN
              NEXT FIELD vnombre_archivo
           ELSE
              EXIT INPUT
           END IF
        END IF
        
      AFTER FIELD vnombre_archivo
        IF vnombre_archivo IS NULL THEN
           ERROR "Campo nombre archivo NO puede ser NULO"
           NEXT FIELD vnombre_archivo
        ELSE 
           SELECT "X"
             FROM cta_ctr_archivo
            WHERE @nombre       = vnombre_archivo
              AND @tipo_archivo = '99'
            GROUP BY 1
           IF STATUS = NOTFOUND THEN
              PROMPT " NOMBRE DE ARCHIVO NO PROCESADO,",
                     " [Enter] continuar "
              ATTRIBUTES (REVERSE) FOR enter
              NEXT FIELD vnombre_archivo
           ELSE
              EXIT INPUT
           END IF             
        END IF
          
      ON KEY ( INTERRUPT )
         EXIT PROGRAM

    END INPUT

   WHILE TRUE
     PROMPT "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? " FOR enter
       IF enter MATCHES "[sSnN]" THEN 
          IF enter MATCHES "[sS]" THEN
             EXIT WHILE
          ELSE
             ERROR "PROCESO CANCELADO" ATTRIBUTE(REVERSE) 
             SLEEP 2
             EXIT PROGRAM
          END IF
       END IF
   END WHILE

   DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

   CASE opcion 
     WHEN 1 CALL reversa_prepara(opcion)  #rp
     WHEN 2 CALL reversa_identificacion() #ri
     WHEN 4 CALL reversa_prepara(opcion)  #rp            
     WHEN 5 CALL reversa_recepcion()      #rr
   END CASE

   PROMPT  "PROCESO FINALIZADO, [Enter] para salir " ATTRIBUTE(REVERSE) 
   FOR enter

 CLOSE WINDOW CTAR0041

END FUNCTION

FUNCTION reversa_prepara(vopcion)
#rp-------------------------

  DEFINE vopcion SMALLINT

  DELETE
  FROM   cta_ctr_saldo_cero
  WHERE  fecha_corte = vfecha_corte
  AND    etapa       = vopcion;
  
  IF vopcion = 1 THEN
     WHENEVER ERROR CONTINUE 
       DATABASE safre_tmp
         DROP TABLE cuota_afore;
         DROP TABLE tmp_sdo_saldo_cero;
       DATABASE safre_af
     WHENEVER ERROR STOP
  END IF

END FUNCTION

FUNCTION reversa_identificacion()
#ri---------------------------

  DELETE 
  FROM   cta_cza_saldo_cero
  WHERE  fecha_corte = vfecha_corte;

  DELETE
  FROM   cta_ctr_saldo_cero
  WHERE  fecha_corte = vfecha_corte
  AND    etapa IN (2,3);

  DELETE 
  FROM   cta_rch_saldo_cero
  WHERE  fecha_corte = vfecha_corte;

  UPDATE cta_ctr_cuenta 
     SET ind_saldo_cero   = 0,
         fecha_saldo_cero = null
   WHERE nss IN (SELECT nss 
                   FROM cta_saldo_cero
                  WHERE nss         = nss
                    AND fecha_corte = vfecha_corte);

  DECLARE c1 CURSOR FOR
  SELECT a.*
  FROM   cta_saldo_cero a
  WHERE  a.fecha_corte = vfecha_corte
  FOREACH c1 INTO reg_cero.*

     DELETE 
     FROM  cta_act_marca
     WHERE nss       = reg_cero.nss
     AND   marca_cod = 151 
     AND   fecha_ini = reg_cero.fecha_proceso

     DELETE
     FROM  cta_his_marca
     WHERE nss = reg_cero.nss
     AND   marca_cod = 151
     AND   fecha_ini = reg_cero.fecha_proceso

  END FOREACH 

  DELETE 
  FROM   cta_saldo_cero
  WHERE  fecha_corte = vfecha_corte;

  WHENEVER ERROR CONTINUE
    DATABASE safre_tmp
      DROP TABLE nss_pendiente_liq;
    DATABASE safre_af
  WHENEVER ERROR STOP

END FUNCTION

FUNCTION reversa_recepcion()
#rr-----------------------

  DEFINE vdesmarca    CHAR(100)
  DEFINE vmarca_rev   SMALLINT
  DEFINE vcorrelativo SMALLINT
  DEFINE vn_folio     DECIMAL(10,0)
  DEFINE vtipo_solicitud SMALLINT
  
  LET vdesmarca = " EXECUTE PROCEDURE reversa_desmarca (?,?,?,?)"
  PREPARE desmarca FROM vdesmarca
  
  LET vmarca_rev   = 151
  LET vcorrelativo = 0

  DECLARE c2 CURSOR FOR
  SELECT a.*
  FROM   cta_saldo_cero a
  WHERE  a.fecha_corte = vfecha_corte
  FOREACH c2 INTO reg_cero.*
  
    UPDATE cta_ctr_cuenta
       SET ind_saldo_cero = 1
     WHERE nss            = reg_cero.nss
     
    UPDATE cta_saldo_cero
       SET diagnostico = NULL
     WHERE (nss        = reg_cero.nss
        OR  curp       = reg_cero.curp)
       AND fecha_corte = reg_cero.fecha_corte   
  
    -----  REVERSO DESMARCAJE SDO CERO MARCAJE DE CUENTA CANCELADA  -----

    EXECUTE desmarca USING reg_cero.nss,          # nss
                           vmarca_rev,            # marca_reverso
                           vcorrelativo,          # correlativo
                           reg_cero.fecha_proceso # fecha_ini
                           
    #### REVERSAR fn_cuenta_saliente #####
    --LET vn_folio        = NULL
    --LET vtipo_solicitud = NULL
    
    --SELECT n_folio, tipo_solicitud
    --INTO   vn_folio, vtipo_solicitud
    --FROM   afi_mae_afiliado
    --WHERE  n_seguro = reg_cero.nss
    
    --EXECUTE prp_rev_cta_admin USING '1',
    --                                reg_cero.nss,
    --                                vn_folio,
    --                                vtipo_solicitud
    --                           INTO rcta_admin
  END FOREACH

  DELETE 
    FROM cta_ctr_archivo
   WHERE nombre = vnombre_archivo

END FUNCTION

